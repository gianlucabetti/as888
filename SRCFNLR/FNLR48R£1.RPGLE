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
000500110906     FFIARg01L  IF   E           K DISK
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
000880030612     FFNARBD0T  O  A E             DISK    USROPN  RENAME(FNARBD00:FNARBDTT)
000890030612     FFIARBT0T  O  A E             DISK    USROPN  RENAME(FIARBT00:FIARBTTT)
000900030612     FFIARBU0T  O  A E             DISK    USROPN  RENAME(FIARBU00:FIARBUTT)
000910030612     FFNARBK0T  O  A E             DISK    USROPN  RENAME(FNARBK00:FNARBKTT)
000920030612     FFNARBM0T  O  A E             DISK    USROPN  RENAME(FNARBM00:FNARBMTT)
000930030612     FFNARBG0T  O  A E             DISK    USROPN  RENAME(FNARBG00:FNARBGTT)
000940030722
000950030722     FAZCLN01L  IF   E           K DISK
000960141020     FTIIDC02L  IF A E           K DISK
000970050601      * Bolle sede - Estensione
000980050601     fTita430c  if   e           k Disk    UsrOpn
000990151019     ffiar531c  uf a e           k Disk    UsrOpn infds(infdsar5)
001000151016     f                                     rename(fiar5000:fiar500S)
001010151016     f                                     rename(fiar5p00:fiar5p0S)
001020151019     fTitas30c  if   e           k disk    usropn infds(infdstitas)
001030151016     f                                     prefix(S_)
001040011120
001050150203     d normlod         s                   like(o95loc)
001060150203     d bollacpi        s                   like(arbcpi)
001070071025     d bollaFGS        s                   like(D48FGS)
001080070319     d sav_arbias      s                   like(vidias)
001090041027     d sav_arbvas      s                   like(vidvas)
001100041027     d sav_arbqft      s                   like(vidqft)
001110041027     d sav_arbvmd      s                   like(vidvmd)
001120041027     d sav_arbvad      s                   like(vidvad)
001130131016     d bol_vidias      s                   like(vidias)
001140131016     d bol_vidvas      s                   like(vidvas)
001150131016     d bol_vidvmd      s                   like(vidvmd)
001160131016     d bol_vidvad      s                   like(vidvad)
001170041027     d sav_arbctr      s                   like(vidctr)
001180020327     d sav_dsftd       s                   like(dsftd)
001190011205     d sav1_vidias     s                   like(vidias)
001200011205     d sav1_vidvmd     s                   like(vidvmd)
001210011205     d sav1_vidcas     s                   like(vidcas)
001220020305     d deseftw         s                   like(§15eft)
001230020305     d desefdw         s                   like(§15efd)
001240020305     d deseffw         s                   like(§15eff)
001250020305     d lnpntw          s                   like(§ogntw)
001260020318     d clintw          s                   like(§ogntw)
001270020305     d lnantw          s                   like(§ogntw)
001280020531     d wdisag          s                   like(ot0dos)
001290070906     d wdisagdest      s                   like(ot0dos)
001300020603     d w48f12          s                   like(d48f12)
001310060301     d w48mct          s                   like(d48mct)
001320020911     d Kfgs            s                   Like(FvvFgs)
001330030612     d kAr5Trd         s                   Like(Ar5Trd)
001340050601     d kta4Trc         s                   Like(ta4trc)
001350021121     d Wctr            s                   Like(VidCtr)
001360040122     d va2va2          s                   Like(Ar6Va1)
001370040122     d va2va3          s                   Like(Ar6Va1)
001380040122     d va2va4          s                   Like(Ar6Va1)
001390041130     d va2va5          s                   Like(Ar6Va1)
001400041130     d va2va6          s                   Like(Ar6Va1)
001410041007     d OLDFFD          s                   Like(Arbffd)
001420041008     d OLDVLC          s                   Like(Arbvlc)
001430050202     d SAVCTR          s                   Like(vidctr)
001440070308     d SAVDCR          s                   Like(comdcr)
001450070308     d SAVTCR          s                   Like(vidtcr)
001460070308     d SAVHCR          s                   Like(vidhcr)
001470070308     d bollaDCR        s                   Like(comdcr)
001480070308     d bollaTCR        s                   Like(vidtcr)
001490070308     d bollaHCR        s                   Like(vidhcr)
001500070308     d bollaFBC        s                   Like(arbfbc)
001510071025     d ar4not_P        s                   Like(ar4not)
001520071025     d sav11pid        s                   Like(v11pid)
001530030612      *
001540030612     d DueCtr          s              2  0
001550070320     d TipoEla         s              1
001560030122     d wI0             s              1
001570030123     d wfpf            s              1
001580070312     d xx              s              3  0
001590090526     d yy              s              3  0
001600030612     d $mbr            s              6
001610030612     d w014a           s             14
001620030612     d wdiffe          s             13  3
001630030612     d w0173           s             17  3
001640030612     d tot_importi     s             17  3
001650100120     d xforzait        s              1
001660100120     d WVariavol       s              1
001670120705     d WVariavolEXP    s              1
001680091106     d Aggiobol        s              1
001690071026     d forzacdf        s              1
001700071025     d forzapid        s              1
001710071025     d forzapid2       s              1
001720071025     d forzaias        s              1
001730080625     d forzaias2       s              1
001740030612     d forzavmd        s              1
001750030612     d forzacas        s              1
001760030709     d flgsf20         s              1
001770030711     d wdataiso        s               d   datfmt(*iso)
001780060201     d wdataLimIas     s               d   datfmt(*iso)
001790030711     d wdataoggi       s               d   datfmt(*iso)
001800030711     d miapsw          s             10
001810030711     d wokpsw          s              1
001820040123     d war72           s              1
001830040122     d contasv         s              2  0
001840040122     d totimv          s                   Like(Ar6Imv)
001850040122     d wesvar1         s              1
001860040122     d wvar3ai         s              1
001870040224     d wffd            s                   Like(ArbFfd)
001880040224     d waggar5         s              1
001890040224     d wCA             s              1
001900040324     d wgiac           s              1
001910040225     d warbdcr         s                   Like(§Ar5gDcr)
001920040316     d wgcfas          s                   Like(GcpFas)
001930040324     d wgcded          s                   Like(GcpDed)
001940040324     d wgcddm          s                   Like(GcpDdm)
001950040608     d wokscr          s              1    inz('0')
001960080416     d wcmitt          s              7
001970080416     d wsflagga_C      s              1
001980110629     d wemd            s              1
001990011205
002000020715     d tbl             s              2    dim(20)                              sk tbl con ass.
002010090526     d ADRs            s              3    dim(50)                              sk tbl con ass.
002020090526     d ADRc            s              3    dim(50)                              sk tbl con ass.
002030020805     D SOVR            S             48    DIM(1)                               DI COMODO SED
002040020805     D SALC            S             48    DIM(1)                               DI COMODO SED
002050020805     D SDLC            S             48    DIM(1)                               DI COMODO SED
002060020805     D PDLC            S             48    DIM(1)                               DI COMODO PAR
002070020805     D C20             S              1    DIM(48) CTDATA PERRCD(48)
002080020805     D C21             S              1    DIM(48) CTDATA PERRCD(48)
002090020805     D C22             S              1    DIM(48) CTDATA PERRCD(48)
002100930507     D GGG             S              1    DIM(8) CTDATA PERRCD(8)              GIORNI CHIUSUR
002110151016     d cmdt            s             60    dim(03) ctdata perrcd(1)
002120151221     D MSG             S             78    DIM(162) CTDATA PERRCD(1)
002130041027     d*cmd             s              1    dim(45) ctdata perrcd(45)            ovrdbf
002140030612
002150990930     D* VARIE DELLA DTASV
002160990930     D* VARIE DELLA DARBT
002170941213     D* VARIE DI COMODO
002180941213     D WSV             S              1    DIM(21)                              SIGLA  VARIE
002190990930     D WVA             S             11  3 DIM(21)                              VALORE VARIE
002200941215     D SKI             S              1    DIM(15)                              COMODO
002210011106     D SKII            S              1    DIM(18)                              COMODO
002220941215     D K78             S              1    DIM(78)                              COMODO
002230990930     D*
002240150119     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
002250070906     d
002260070906     d* Skiere di riesecuzione pgm
002270070906     D kdati           S            250    DIM(10)                              VARIE IN TOT.I
002280070906     D kcvb            S              2    DIM(10)                              VARIE IN TOT.I
002290070907     D ktrc            S              2    DIM(10)                              VARIE IN TOT.I
002300070911     D kffr            S              1    DIM(10)                              VARIE IN TOT.I
002310070911     D kf12            S              1    DIM(10)                              VARIE IN TOT.I
002320150414
002330150414      * filiali abilitate a cons.part "Z"=Expo
002340151119     d*fil_expo        s              3    dim(85)
002350030722
002360030722     D ktfp            S                   LIKE(CLNtfp)
002370030722     D ktfa            S                   LIKE(CLNtfa)
002380030722     D kann            S                   LIKE(CLNann)
002390030722     D kmes            S                   LIKE(CLNmes)
002400050601     d wlib            s             10
002410050601     d commt           s             80
002420050601     d lenght          s             15  5 inz(80)
002430120403     d erra            s              1
002440120403     d errp            s              1
002450120404     d wfatfil         s              2
002460120404     d war6agg         s              1
002470120411     d inx             s              2  0
002480120416     d savcpi_a        s                   like(arbcpi)
002490120418     d savcpi_p        s                   like(arbcpi)
002500120418     d savksc_a        s                   like(arbksc)
002510120418     d savctr_a        s                   like(arbctr)
002520120418     d savksc_p        s                   like(arbksc)
002530120418     d savctr_p        s                   like(arbctr)
002540030722
002550151019     d infdsar5        ds
002560151019     d   recname             261    270
002570151019     d infdstitas      ds
002580151019     d   recnamet            261    270
002590030722     D clnmat          DS
002600030722     D  mat                    1     31    dim(31)
002610030722     D clnpom          DS
002620030722     D  pom                    1     31    dim(31)
002630030722
002640030722     D                 DS
002650030722     D  ds_giorno              7      8  0
002660030722     D  ds_mese                5      6  0
002670030722     D  ds_anno                1      4  0
002680030722     D  ds_data                1      8  0
002690030722
002700941213     D*
002710941213     D* CODICI TARIFFA CON CUI TASSARE GLI ASSEGNATI 9999 - TAB 7W
002720990702     D* TARIFFE PER TASSAZIONE PREPAGATI
002730941222     D*-------------_____------------_______-------------_______-----
002740930505     D                 DS
002750941014     D  ARBAAS                 1      4  0
002760941014     D  ARBMGS                 5      8  0
002770941014     D  ARBDSP                 1      8  0
002780950114     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
002790931210     D PARAM3          DS
002800931210     D  PA3PDR                 1      7  0
002810931210     D  PA3TSR                 8      8
002820931210     D  PA3NDC                 9     15  0
002830931210     D  PA3FCT                16     16
002840931210     D  PA3FVD                17     17
002850931210     D  PA3FVT                18     18
002860950113     D  PA3DDC                19     26  0
002870041122     D  PA3sml               256    256
002880941014     D***
002890941014     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002900941014     D***
002910941014     D WLBDAT          DS                  INZ
002920941014     D  G02DAT                 1      8  0
002930941014     D  G02INV                 9     16  0
002940941014     D  G02ERR                17     17
002950941014     D  G02TGI                18     22  0
002960941019     D                 DS
002970941019     D  VIDISD                 1      2
002980941019     D  VIDCPD                 3     16
002990941019     D  VIDPID                 1     16
003000071022     D                 DS
003010071022     D  V11ISD                 1      2
003020071022     D  V11CPD                 3     16
003030071022     D  V11PID                 1     16
003040941223     D                 DS
003050941223     D  VIDISM                 1      2
003060941223     D  VIDCPM                 3     16
003070941223     D  VIDPIM                 1     16
003080930210     D                 DS
003090930210     D  EMME                   1      1
003100020805     d  $simfel                2      4  0
003110020805     D  PARMBR                 5      7  0
003120930210     D  MBRFIS                 1     10
003130140130
003140140130      *Schiera per filiali partite con gestione telefonate AUT
003150140130     d Fil816tel       ds           256
003160140130     d  skFil816tel            1    255    dim(85)
003170141029     d TRULVPODS     e ds
003180141029     D  skFilOk               16    765    dim(250)
003190141029
003200141029     D skFilOkdir      s                   like(skFilOk) dim(250) inz
003210140130
003220920211     D DSARBD        E DS
003230930510     D DSARBG        E DS
003240130930     D DSARBN        E DS
003250990930     D DARBT         E DS
003260030612     D  §SV                   44     63    DIM(20)                              SIGLA  VARIE
003270030612     D  §VA                   64    183P 3 DIM(20)                              VALORE VARIE
003280920128     D DSARBK        E DS
003290030612
003300930506     D DSCC          E DS
003310991001     D DS$2          E DS
003320941212     D DSCV          E DS
003330941020     D DSQT1         E DS
003340080416     D DS3K          E DS
003350990930     D DTAS          E DS
003360060925      ** DS Flag operativi DS DTAS
003370060925     d Dtas01        e ds
003380060925
003390990930     D DTASV         E DS
003400030612     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003410030612     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
003420030612     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
003430030612                                                                                ESENZ  VARIE
003440030612     D DSBL4W        E DS
003450070320     D DSBL4L        E DS
003460151126     D DSBL4m        E DS
003470000623     D DS3IPT        E DS
003480000510     D DS3IDP        E DS
003490000510     D DS1A          E DS
003500930506     D DS1P          E DS
003510060131     D DSSP          E DS
003520941209     D DS15          E DS
003530920117     D DS3A          E DS
003540020715     d dstb          e ds
003550000202     D OG143         E DS
003560070319     D OG146         E DS
003570070306     d Og148         e ds
003580070306     d Og150         e ds
003590000202     D DGEI          E DS
003600991006     D DGED          E DS
003610991028     D DSTA01        E DS
003620930210     D DS7T          E DS
003630021121     d ddfe          e ds
003640030710     d dvpo          e ds
003650071031     d dvpoDECO      e ds
003660050601     d dta4c         e ds
003670151019     D Dar5fat       E DS
003680090604     d ds5e          e ds
003690110906     d FNLVSTDS      e ds
003700130930     d dsemail       e ds
003710130927     d
003720130927     d darbn_ref     e ds
003730130927     d darbn_eml     e ds
003740130930     d darbn_sms     e ds
003750130930     d darbn_not     e ds
003760061107     d
003770151105     d xcfiva1ds     e ds
003780071025     d  ivaeu                  3      4
003790030612      * DS PER YEURCO - EURO CONVERTITORE
003800030612     D YeurcoDS      E DS
003810030612      * DS PER TRUL90R - reperimento stampanti e pgm di stampa
003820130322     D TRUL90DS      E DS
003830030612      * DS PER FNLV13R - CONTROLLO DI UN CAP
003840030612     D Fnlv13DS      E DS
003850030612      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
003860030612     D Fnlv14DS      E DS
003870030612      * DS PER TISI95R - CONTROLLO DI CAP
003880030612     D Tisi95DS      E DS
003890030612      * DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT E GEST PARTI
003900030612     D DS7pqrs       E DS
003910030612      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
003920030612     D Tibs02DS      E DS
003930030626      * DS PER STAMPA LDV
003940130321     D*FNLSB5DS      E DS
003950030612      * DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
003960030612     D FNLV04DS      E DS
003970050202      * DS PER FNLV59R - CARICAMENTO E CONTROLLO CODICI TARIFFA
003980050202     D Fnlv59DS      E DS
003990041008      * DS PER FNLV57R - PER aggiornamento files padroncini
004000041008     D FNLV57ds      E DS
004010030612      * DS PER FNLV16R - CONTROLLO TASSAZIONE
004020030612     D Fnlv16DS      E DS
004030040122     d dlv1601       e ds
004040030612      * DS PER FNLV18R - CALCOLO VOLUME AUTOMATICO
004050030612     D Fnlv18DS      E DS
004060060223      * DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
004070030612     D Fnlv19DS      E DS
004080030612      * DS PER FNLV20R - CALCOLA E SISTENA I VOLUMI SU BOLLA
004090030612     D Fnlv20DS      E DS
004100030612      * DS PER FNLV55R - REPERISCE TERMINAL DI PARTENZA E ARRIVO
004110030612     D Fnlv55DS      E DS
004120050511      * DS PER FNLRa2R - TASSAZIONE PORTI ASSEGNATI
004130050511     D Fnlra2DS      E DS
004140030612      * DS PER FNLR48R - DATI DI ENTRATA
004150030612     D Fnlr48DS      E DS
004160030612      * DS PER FNLR66R - CONTROLLO CAUSALE VARIAZIONE
004170030612     D Fnlr66DS      E DS
004180140130      * DS PER FNLRN6R - note LDV
004190140130     D FnlrN6DS      E DS
004200130321      * DS PER FIDG42R -
004210130321     D Fidg42ds      E DS
004220141029
004230141029     D fnlry09ds2    E DS
004240150603     d
004250150603     D tnsd99ds      E DS
004260070329     d
004270120413     D TIBS69DS      E DS
004280120413     D ds_cnaco      E DS                  extname(CNACO00F) prefix(BS_)
004290120413     D ds_cnind      E DS                  extname(CNIND00F) prefix(BS_)
004300120413     D ds_cnclp      E DS                  extname(CNCLP00F) prefix(BS_)
004310120413     D ds_fncls      E DS                  extname(FNCLS00F) prefix(BS_)
004320120413
004330070329      * Dati utente
004340060131     d Azuteds       e ds                  extname(Azute00f)
004350060131     d dDatiute      e ds
004360060131     d dute01        e ds
004370060131     D TIBS34DS      E DS
004380060213      * ds x richiamo pgm FNLv85R
004390060213     d Fnlv85ds      e ds
004400060705      * ds x controllo tipo incasso e divisa C/A
004410060705     d TRULTICDS     e ds
004420140515
004430140515     d TRULINTDS     e ds
004440980722      *
004450030612      * DS PER XEDIT - PGM EDITAZIONE
004460011009     D DSEDIT        E DS                  EXTNAME(XEDITDS)
004470011009     D  IEDNUM       E                     EXTFLD(IEDNUMERO)
004480011009     D  IEDNRD       E                     EXTFLD(IEDNRDEC)
004490011009     D  IEDEDT       E                     EXTFLD(IEDEDTCOD)
004500011009     D  IEDLEN       E                     EXTFLD(IEDLENFLD)
004510011009     D  OEDNUM       E                     EXTFLD(OEDNUMAN)
004520011009     D  OEDFLG       E                     EXTFLD(OEDFLGERR)
004530011009     D  OEDCOD       E                     EXTFLD(OEDCODERR)
004540941212      *
004550910429     D KPJBA         E DS
004560930210     D  LIB                   92    101
004570060131     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004580030612     D  DTU                  848    895P 0 DIM(12)    PACKEVEN                  DATE PARAM
004590011205
004600020327     D DSFTD         E DS                  EXTNAME(FIFTD00F)
004610011205     d trul21ds      e ds
004620011205     d trul22ds      e ds
004630011205     d trul23ds      e ds
004640020305     D* DS PER TRUL27R - Ricerca tarfiffa di cartello
004650020305     D trul27ds      E DS
004660020531     D tisit0ds      E DS
004670131227     D fnlva1ds      E DS
004680041007     d*Trul42Ds      e ds
004690140212     d Trtcp2ds      e ds
004700141021     D TRUL33DS      E DS
004710021129     d Dar5scr       e ds
004720030123     d dAr5Ban       e ds
004730030729     d dAr5Bnb       e ds
004740130924     d dAr5Emd       e ds                  prefix(emd_)
004750031120     d dAr5          e ds
004760031120     d dAr5Gen       e ds
004770040219     d  §Ar5gTcr     e                     Extfld(§Ar5Tcr)
004780040219     d  §Ar5gDcr     e                     Extfld(§Ar5Dcr)
004790040219     d  §Ar5gHcr     e                     Extfld(§Ar5Hcr)
004800040219     d  §Ar5gGc1     e                     Extfld(§Ar5Gc1)
004810040219     d  §Ar5gGc2     e                     Extfld(§Ar5Gc2)
004820040219     d  §Ar5gTc1     e                     Extfld(§Ar5Tc1)
004830040219     d  §Ar5gTc2     e                     Extfld(§Ar5Tc2)
004840070910     d dAr5GenPar    e ds                  extname(dar5gen) prefix(PAR:3)
004850070910     d  PAr5gTcr     e                     Extfld(§Ar5Tcr)
004860070910     d  PAr5gDcr     e                     Extfld(§Ar5Dcr)
004870070910     d  PAr5gHcr     e                     Extfld(§Ar5Hcr)
004880070910     d  PAr5gGc1     e                     Extfld(§Ar5Gc1)
004890070910     d  PAr5gGc2     e                     Extfld(§Ar5Gc2)
004900070910     d  PAr5gTc1     e                     Extfld(§Ar5Tc1)
004910070910     d  PAr5gTc2     e                     Extfld(§Ar5Tc2)
004920030123     d dTba          e ds
004930031028     d dsbl4a        e ds
004940040806
004950070309     d ds2Z          e ds
004960070309     d ds2g2         e ds
004970040806     d ds7q          e ds
004980140408     d ds7r          e ds
004990051109     d dgcpflr       e ds
005000060111     d ddstflo       e ds
005010040809
005020040809      * ds per il controllo della presenza di CA per la spedizione richiesta
005030080701     d****** FIDN12DS      e ds
005040080701     d***  skKey                 26    305    dim(20)
005050080701     d****  skAnn                306    325    dim(20)
005060080701     d****  skDca                326    485  0 dim(20)
005070080701     d****  skFca                486    545  0 dim(20)
005080080701     d****  skDch                546    705  0 dim(20)
005090080701     d****  skCch                706    745    dim(20)
005100040809     d*
005110040809     d dsKey           ds
005120040809     d  dsaac                         4  0
005130040809     d  dsfil                         3  0
005140040809     d  dsnca                         7  0
005150050531     D PARAMT          DS
005160050531     D  PARCA                256    256
005170011205
005180950104     D*
005190950104     D* DEFINIZIONE COSTANTI
005200950104     D*
005210950104     D CPDC            C                   CONST('su Piano dei Conti')
005220950114     D CNOTAR          C                   CONST('NO TARIFFE')
005230061107     D Cdest           C                   CONST('D E S T I N A T A R I O')
005240061107     D CMITT           C                   CONST('--- M I T T E N T E ---')
005250141022     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
005260141022     D RIMM            C                   CONST(X'21')
005270011205
005280011205     d                sds
005290011205     d  nompgm                 1     10
005300030620
005310030620     d sav_indsin      s                   Like(indsin)
005320041008     d waltrocpi       s                   Like(arbcpi)
005330060223     d wcpifile        s                   Like(arbcpi)
005340041008     d waltrofvf       s                   Like(arbfvf)
005350041008     d waltrovlf       s                   Like(arbvlf)
005360041022     d waltrotc1       s                   Like(arbtc1)
005370041022     d waltrotc2       s                   Like(arbtc2)
005380080422     d waltroft3       s                   Like(arbft3)
005390041027     d newcpi          s                   Like(arbcpi)
005400051114     d wtc1            s                   Like(arbtc1)
005410051114     d wtc2            s                   Like(arbtc2)
005420051220     d memntw          s                   Like(§ogntw)
005430060111     d d_o95lna        s                   like(o95lna)
005440060316     d s_arbrsd        s                   like(arbrsd)
005450060316     d s_arbrd2        s                   like(arbrd2)
005460061107     d wcliie          s              1
005470061107     d wprv            s                   like(arbprd)
005480151105     d wcap            s                   like(arbcad)
005490151105     d wloc            s                   like(arblod)
005500061107     d Esiste          s              1
005510091217     d Wtipoagg        s              3
005520110906     d Lnacons         s              3  0
005530061107     d wnar            s                   Like(arbnzd)
005540130411     d wpda_ndc        s                   Like(arbndc)
005550130411     d wpda_ddc        s                   Like(arbddc)
005560130411     d wpda_dcm        s                   Like(arbdcm)
005570131016     d winf14          s              1
005580131120
005590131120     d sav_ift         s                   Like(ar6ift)
005600140403     d comgf2          s                   Like(o95gf2)
005610140407     d wpk2            s                   Like(§AR5PK2)
005620141020     d wdir            s              1
005630141021     d $Finenum        s               n   inz(*off)
005640141021     d wfgslna         s                   like(d55tfa)
005650141021     D Esito           s              1a
005660141031     d w0090           s              9  0
005670141021     d kprg            s                   like(idcprg)
005680141021     d winf12          s              1
005690141022     d annocur         s              4s 0
005700141029     d w0140i          s             14s 0 inz
005710141203     d savzng          s                   like(vidzng)
005720141212     d wnoindir        s              1
005730150119     d w_noapp         s              1
005740151016     d PesVolpkcn      s                   Like(Taspkcn)
005750151016     d PesVolSpc       s                   Like(TasSpc)
005760151016     d PesVolIMV       s                   Like(Ar6IMV)
005770151019     d Tassatoimv      s                   Like(Ar6IMV)
005780160112     d wfile           s              1
005790141021
005800141021     D trul33R         pr                  extpgm('TRUL33R')
005810141021     D  kpjba                              likeds(kpjba)
005820070515     i*
005830070515     IFNBLP000
005840070515     I              BLPFST                      ARBFSTDDT
005850910430     C****************************************************************
005860910430     C*  RIEPILOGO INDICATORI
005870910430     C***************************************************************
005880920128     C* 01    - VARIAZIONE DESTINATARIO
005890941209     C* 02    - VARIAZIONE PESO / VOLUME
005900930510     C* 03    - PROGRAMMA RICHIAMATO  DSARBG
005910930505     C* 04    - SPEDIZIONE CON GIACENZA APERTA
005920941222     C* 05    - INDICA SE LA 1 BOLLA O LA 2 E' UN ASSEGNATO X P.IVA
005930941222     C* 06    - 1 BOLLA IN FRANCO
005940991006     C* 07    - PROTEZIONE DIVISA PER PREPAGATI E FATTURE IMMEDIATE
005950991012     C* 08    - PREPAGATO
005960941213     C* 09    - SONO STATE MESSE DELLE VARIE NELLA TASSAZIONE
005970941222     C* 10    - ON -BOLLE IN ARRIVO OFF-BOLLE IN PARTENZA
005980941102     C* 11/12 - PROTEGGO LE CONSEGNE PARTICOLARI SE IMMESSE IN PART E
005990941102     C*         NON UTILIZZABILI IN ARRIVO
006000941102     C* 13    - VARIAZIONE TASSAZIONE S.F.
006010911210     C* 14    - VARIAZIONE TASSAZIONE FATTURA
006020941102     C* 15    - RITASSAZIONE
006030950120     C* 16    - PER CAUSALE CP PROTEGGO CAMPI DELLA CONSEGNA RICH/GG CH
006040940614     C* 17 OFF- FILE TASSAZIONE PADRONCINI
006050940614     C* 17 ON - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
006060911211     C* 18    - NON ESISTONO TARIFFE PER IL CODICE DIGITATO
006070920128     C* 19    - PROGRAMMA RICHIAMATO  DSAR2T
006080920128     C* 20    - PROGRAMMA RICHIAMATO  GENERICO
006090990701     C* 21    - CAUSALE VARIAZIONE MODIFICA CONTRASSEGNO
006100990701     C* 22    - UATO PER PROTEZIONE MITTENTE
006110990701     C* 23    - DATI FATTURA NON MODIFICABILI PER PREPAGATO AUTOMAT
006120950113     C* 24    - ESEGUITA LA VARIAZIONE DEL MITTENTE
006130920128     C* 25    - PROGRAMMA RICHIAMATO  DSARBT
006140950607     C* 26    - ABILITA L'ENTER PER L'AGGIORNAMENTO
006150950607     C* 27    - ABILITA L'F12 PER RITORNO AL PGM CHIAMANTE
006160941212     C* 28    - EMETTO VIDMSG
006170950616     C* 29    - PROTEZIONE CAMPI DEL VIDEO
006180980513     C* 30-33 - COMODO
006190920211     C* 34    - PROGRAMMA RICHIAMATO  DSARBD
006200920129     C* 35    - PROGRAMMA RICHIAMATO  DSARBK
006210920129     C* 36    - VARIA TASSAZIONE FRANCHI: SALTO I CONTROLLI SUL COD.CLI
006220920204     C* 37    - VARIATO TOTALE IMPONIBILE: EFFETTUO RISTAMPA FATTURA
006230941014     C* 38    - CREO RECORD IN FNARBM0T
006240941014     C* 39    - CREO RECORD IN FNARBD0T-CAUS.K5 K8- PER AVERE DESTINAT
006250930507     C* 40-67 - ERRORE
006260030729      * 70    - Visualizzo Colli originali
006270991001     C* 71-72 - PROTEGGO CAMPI DESTINATARIO SE 9999 GIA' CONTABILIZZATO
006280990415     C* 73    - SPROTEGGO IL CAMPO DELL'IMPORTO D'ASSICURARE E DEL VALORE
006290990415     C*         MERCE DICHIARATO
006300060201     C* 74    - protezione campi tassazione
006310060201     C* 75    - protezione campi importo da assicurare
006320140213     C* 77    - condizionamento campo "fd da non contattare"
006330131001     C* 81    - della DSARBN  passato campo uni70 - note
006340990415     C* 82    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA IMP.ASSICURAZ.
006350131001     C* 83    - della DSARBN  passato campo cel
006360131001     C* 84    - della DSARBN  passato campo ref/tel
006370131001     C* 85    - SFPDSP
006380131001     C* 86    - della DSARBN  passato campo uni70 - email
006390071022     C* 87    - tasto funzionale"F17-Note ldv"attivo
006400070329     C* 88    - PROTEGGO PESO   SE NON VARIABILE
006410941216     C* 89    - PROTEGGO VOLUME SE NON VARIABILE
006420920220     C* 90    - ERRORE GENERICO
006430911213     C* 91    - ERRORE NON POSSIBILE ALLOCARE FILE DI INVIO
006440950407     C* 92    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA C/ASS.
006450941215     C* 93    - INDICATORE DI CHANGE SU VALORE MERCE DICH
006460941215     C* 94    - RI SU VALUTA VAL.MERCE SE PRESA IN AUTOMATICO DA NAZ
006470941102     C* 95    - INDICATORE DI CHANGE SU C/ASSEGNO
006480941215     C* 96    - RI SU VALUTA C/ASSEGNO SE PRESA IN AUTOMATICO DA NAZ
006490941215     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
006500941215     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
006510910430     C*****************************************************************
006520000000     C     *ENTRY        PLIST
006530000000     C                   PARM                    KPJBA
006540920211     C                   PARM                    DSARBD
006550920128     C                   PARM                    DSARBK
006560990930     C                   PARM                    DARBT
006570930510     C                   PARM                    DSARBG
006580030612     C                   PARM                    TRUL90DS
006590130930     C                   PARM                    DSARBn
006600050601     c*
006610050110     c*
006620050110     c                   setoff                                       kl
006630030612      *
006640030612      * Il numero dei parametri ricevuti è testato, in caso di modifica
006650030612      *  ricercare la stringa "%PARMS"
006660030612      *
006670030612     C                   MOVEL     KPJBU         Fnlr48DS
006680120426     C*
006690120426     C                   EXSR      OPN_TITA4
006700950104     C* ESEGUO LA VARIAZIONE
006710950104     C                   MOVEL     'E'           WESEG             1
006720020603     c                   movel     d48f12        w48f12
006730060301     c                   movel     d48mct        w48mct
006740950114     C* SE NON  RICHIAMATO IMPOSTO SEMPRE LA FERMATA NELLE VIDEATE
006750950114     C     D48TRC        IFEQ      *BLANKS
006760950609     C                   MOVEL     'V'           D48FFR
006770950114     C                   ENDIF
006780070907     c* skiere di riesecuzione automatica pgm: max 10 volte
006790070907     c                   clear                   kdati
006800070907     c                   clear                   kcvb
006810070907     c                   clear                   ktrc
006820070911     c                   clear                   kffr
006830070911     c                   clear                   kf12
006840070907     c                   clear                   kK                2 0
006850070907     c                   z-add     1             rr                2 0
006860941014     C*
006870910429     C*---------------------------------------------------------------*
006880041006     C* TIPO LANCIO, D48TLA "C" -           CHIUSO CON %R
006890941209     C* TIPO LANCIO, D48TLA "L" - ELABORO E CHIUDO CON LR
006900941209     C* TIPO LANCIO, D48TLA " " - ELABORO E CHIUDO IN RETRN
006910941209     C*
006920941209    1C     D48TLA        IFNE      'C'
006930001213     C*
006940001213    2C     D48FFR        IFNE      'E'
006950141017    2C     D48FFR        andne     'F'
006960001213    3C     OPN48         IFEQ      '0'
006970001213     C                   OPEN      FNLR48D
006980001213     C                   MOVE      '1'           OPN48
006990001213    3C                   ENDIF
007000001213   X2C                   ELSE
007010001213    3C     OPN48         IFEQ      '1'
007020001213     C                   CLOSE     FNLR48D
007030001213     C                   MOVE      '0'           OPN48
007040001213    3C                   ENDIF
007050001213    2C                   ENDIF
007060040224
007070040224     c                   Clear                   wCa
007080040324     c                   Clear                   wgiac
007090941209     C*
007100950104     C* FINTANTO CHE SI VUOLE SI PUO' RIMANRE DENTRO ALLE VARIAZIONI
007110950104   1AC     WESEG         DOWEQ     'E'
007120950104     C                   CLEAR                   WESEG
007130920123     C*
007140950104     C* CONTROLLI BOLLA CON CAUSALE VARIAZIONE
007150900509     C                   EXSR      CONTR
007160941014     C*
007170941014     C   90              GOTO      FINE
007180911209     C*
007190911210     C* CARICO BOLLA
007200120403     c                   if        flgcvr<>'S'
007210911210     C                   EXSR      CARICA
007220131002     c                   if        d48err='1'
007230131002     C                   GOTO      FINE
007240120403     c                   endif
007250131002     c                   endif
007260911209     C**
007270941019     C** VARIAZIONE: DESTINATARIO PESO VOLUME
007280911209     C**
007290941220     C                   SELECT
007300120330     c* Variazioni da manutenzione bolle di sede
007310120330     C     FLGCVR        WHENEQ    'S'
007320120427     c* Questa routine la posso eseguire solo se pgm richiamato
007330120427     c* Hanno fatto una manutenzione con CVB SD/SF da menù in quanto
007340120427     c* SD e SF non erano stati disabilitati per l'utilizzo da bolle par
007350120427     c* e bolle arrivi. In questo modo essendo azzerava peso,volume, mittente..
007360120427     c                   if        d48trc<>*blanks
007370120330     C                   EXSR      gesSD
007380120427     c                   endif
007390120330
007400941220     C     FLGCVR        WHENEQ    'I'
007410920120     C                   EXSR      EMFOR2
007420911210     C**
007430911211     C** VARIAZIONE TASSAZIONE 1° BOLLA
007440911210     C**
007450941220     C     FLGCVR        WHENEQ    'T'
007460920121     C                   EXSR      EMFOR3
007470911211     C**
007480911211     C** VARIAZIONE TASSAZIONE 2° BOLLA
007490911211     C**
007500941220     C     FLGCVR        WHENEQ    '2'
007510941102     C                   EXSR      EMFOR4
007520920116     C**
007530920116     C** VARIAZIONE: CODICE BOLLA CONTRASSEGNO
007540920116     C**
007550941220     C     FLGCVR        WHENEQ    'K'
007560941102     C                   EXSR      EMFOR5
007570920320     C**
007580920320     C** VARIAZIONE CONSEGNA RICHIESTA
007590920320     C**
007600941220     C     FLGCVR        WHENEQ    'C'
007610021129     c     D48Cvb        AndNe     'CS'
007620941102     C                   EXSR      EMFOR7
007630021129     C**
007640021129     C** VARIAZIONE CONSEGNA RICHIESTA SUPERMERCATI
007650021129     C**
007660021129     c     D48Cvb        WhenEq    'CS'
007670021129     c                   ExSr      Emfor71
007680941223     C**
007690941223     C** VARIAZIONE MITTENTE
007700941223     C**
007710941223     C     FLGCVR        WHENEQ    'M'
007720941223     C                   EXSR      EMFOR8
007730961211     C**
007740130927     C** VARIAZIONE DATI MITTENTE
007750961211     C**
007760961211     C     D48CVB        WHENEQ    'RM'
007770961211     C                   EXSR      EMFOR9
007780030123      **
007790030123      ** Variazione mancato ritiro BANCALI
007800030123      **
007810030123     c     D48Cvb        WhenEq    'BR'
007820030123     c                   ExSr      Emfor10
007830961211     C*
007840061107      **
007850061107      ** Variazione CODICE FISCALE
007860061107      **
007870061107     c     D48Cvb        WhenEq    'FI'
007880061107     c                   ExSr      Emfor11
007890130927      **
007900130927      ** Variazione dati destinatario SMS/mail/referente
007910130927      **
007920130927     C     FLGCVR        WHENEQ    'N'
007930130927     c                   ExSr      Emfor12
007940061107     C*
007950941220     C                   ENDSL
007960070907     c
007970070907     c* Verifico se ho delle riesecuzioni da fare
007980070907     c                   if        RR<=10 and kcvb(RR)<>*blanks
007990070907     c                   eval      weseg='E'
008000070907     c                   eval      d48cvb=kcvb(RR)
008010070907     c                   eval      d48trc=ktrc(RR)
008020070911     c                   eval      d48ffr=kffr(RR)
008030070911     c                   eval      w48f12=kf12(RR)
008040070907     c                   select
008050070907     c                   when      d48trc='D'
008060070907     c                   movel     kdati(RR)     dsarbd
008070070907     c                   when      d48trc='G'
008080070907     c                   movel     kdati(RR)     dsarbg
008090070907     c                   when      d48trc='K'
008100070907     c                   movel     kdati(RR)     dsarbK
008110070907     c                   when      d48trc='T'
008120070907     c                   movel     kdati(RR)     darbt
008130070907     c                   when      d48trc='2'
008140070907     c                   movel     kdati(RR)     darbt
008150070907     c                   ENDSL
008160070907     c
008170070907     c                   add       1             RR
008180070907     c                   endif
008190941209     C*
008200950104   1AC                   ENDDO
008210950104    1C                   ENDIF
008220920121     C*
008230000000     C     FINE          TAG
008240941209     C* CHIUSURA PGM
008250060302     C******             MOVEL     Fnlr48DS      KPJBU
008260950114     C*
008270950114     C* PULISCO IL CAMPO CHER AVEVO IMPOSTATO CON LA CONFERMA DEL MITT
008280950114     C*  CHE DEVE RIMANERE IMPOSTATO FINTANTO CHE SI E' DENTRO AL PGM
008290950114     C                   CLEAR                   WRICVL
008300950114     C                   CLEAR                   MITFVF
008310950114     C                   CLEAR                   MITVLF
008320950114     C                   CLEAR                   MITCTR
008330020722      * PULISCO IL CAMPO CHE AVEVO IMPOSTATO PER IL DESTINATARIO
008340020722      *  DISAGIATO
008350020722     C                   CLEAR                   Wdisag
008360060302     c*
008370060302     c                   Clear                   Fnlv85ds
008380060302     c                   Eval      Ilv85Tla = 'C'
008390060302     c                   movel     fnlv85ds      kpjbu
008400060302     c                   Call      'FNLV85R'
008410060302     c                   Parm                    kpjba
008420060302     c*
008430950114     C* CHIUSURA PGM
008440941216    1C     D48TLA        IFEQ      ' '
008450130417     C                   MOVEL     Fnlr48DS      KPJBU
008460941209     C                   RETURN
008470941209     C                   ELSE
008480941209     C*
008490930510     C* SE APERTI FILE CHIUDO
008500001213     C*
008510001213     C     OPN48         IFEQ      '1'
008520001213     C                   CLOSE     FNLR48D
008530001213     C                   ENDIF
008540001213     C*
008550030612     C                   CLEAR                   FNLV04DS
008560941209     C                   MOVEL     'C'           D04TLA
008570990930     C                   CALL      'FNLV04R'
008580030612     C                   PARM                    FNLV04DS
008590990930     C                   PARM                    DTASV
008600970808     C*
008610970808     C                   MOVEL     'C'           I13TLA
008620970808     C                   CALL      'FNLV13R'
008630970808     C                   PARM                    KPJBA
008640030612     C                   PARM                    Fnlv13DS
008650030612     C                   PARM                    Tisi95DS
008660030612     C                   CLEAR                   Tisi95DS
008670971105     C**
008680971105     C                   MOVEL     'C'           I95TLA
008690971105     C                   CALL      'TISI95R'
008700030612     C                   PARM                    Tisi95DS
008710970808     C*
008720970808     C                   MOVEL     'C'           I14TLA
008730970808     C                   CALL      'FNLV14R'
008740970808     C                   PARM                    KPJBA
008750030612     C                   PARM                    Fnlv14DS
008760971114     C*
008770030612     C                   CLEAR                   Fnlv55DS
008780971114     C                   MOVEL     'C'           D55TLA
008790971114     C                   CALL      'FNLV55R'
008800030612     C                   PARM                    Fnlv55DS
008810050202     c
008820050202     C                   CLEAR                   Fnlv59DS
008830050202     C                   MOVEL     'C'           ilv59tla
008840050202     C                   CALL      'FNLV59R'
008850050202     C                   PARM                    Fnlv59DS
008860020305     c*
008870020305     C                   CLEAR                   trul27ds
008880020305     C                   MOVEL     'C'           I27TLA
008890020305     C                   CALL      'TRUL27R'
008900020305     C                   PARM                    TRUL27DS
008910060705     c
008920060705     C                   CLEAR                   trulTICds
008930060705     C                   MOVEL     'C'           IticTLA
008940060705     C                   CALL      'TRULTICR'
008950060705     C                   PARM                    TRULTICDS
008960060705     C                   PARM                    DS1A
008970140515
008980140515     C                   CLEAR                   trulintds
008990140515     C                   MOVEL     'C'           IintTLA
009000140515     C                   CALL      'TRULINTR'
009010140515     C                   PARM                    TRULintds
009020110906     c
009030110906     C                   CLEAR                   fnlvstds
009040110906     C                   MOVEL     'C'           ILVSTTLA
009050110906     C                   CALL      'FNLVSTR'
009060110906     c                   parm                    kpjba
009070110906     C                   PARM                    fnlvstds
009080941220     C*
009090120426     C                   IF        %OPEN(TITA430C)
009100050601     c                   close     tita430c
009110120426     C                   ENDIF
009120151016     C*
009130151016     C                   IF        %OPEN(FIAR531C)
009140151016     c                   close     fiar531c
009150151016     C                   ENDIF
009160151016     C*
009170151016     C                   IF        %OPEN(TITAS30C)
009180151016     c                   close     titas30c
009190151016     C                   ENDIF
009200130321
009210130411     c                   clear                   fidg42ds
009220130411     C                   MOVEL     'C'           co42tla
009230130411     c                   movel     fidg42ds      kpjbu
009240130411     c                   call      'FIDG42R'
009250130411     c                   parm                    kpjba
009260131227
009270131227     c                   clear                   fnlva1ds
009280131227     C                   MOVEL     'C'           ia1tla
009290131227     c                   movel     fnlva1ds      kpjbu
009300131227     c                   call      'FNLVA1R'
009310131227     c                   parm                    kpjba
009320141029
009330141029     c                   eval      ilry09tla= 'C'
009340141029     c                   eval      kpjbu=fnlry09ds2
009350141031     c                   call      'FNLRY09C'
009360141029     c                   parm                    kpjba
009370130321
009380130417     C                   MOVEL     Fnlr48DS      KPJBU
009390041006     C                   SETON                                        LR
009400941209     C                   END
009410941220     C*
009420941209     C* IMPOSTAZIONI INIZIALI -------------------------------------*
009430941209     C     *INZSR        BEGSR
009440941212     C* MI SALVO LA KPJBU
009450941212     C                   MOVEL     KPJBU         W256A           256
009460060131      *
009470060131     c     *dtaara       define    §azute        azuteds
009480060131     c     *dtaara       define    §datiute      ddatiute
009490060131     c                   in(E)     *dtaara
009500060131    1c                   If        %error  or RSUT = *blanks
009510060131     c                   Clear                   Tibs34ds
009520060131     c                   Call      'TIBS34R'
009530060131     c                   Parm                    Tibs34ds
009540060131     c                   In        *dtaara
009550060131    1c                   EndIf
009560060131
009570060131      * se ho errori nei dati utente esco dal pgm
009580060131w   1c                   if        DutErr = 'E'
009590060131     c                   GoTo      Fine
009600060131x   1c                   else
009610060131     c                   Movel     UteFaf        Dute01
009620060131e   1c                   endif
009630941212     C*
009640941209     C                   Z-ADD     1             CODUT
009650060131     C                   CALL      'X§PARUT'
009660060131     C                   PARM                    UT§DSE
009670060131     C                   MOVEL     rsut          VIDDSA
009680000104     C**
009690000104     C                   MOVEL     DTU(10)       W0020             2 0
009700000104     C                   Z-ADD     DTU(10)       WDTU10
009710000104     C     W0020         IFGE      60
009720000104     C                   MOVEL     19            WDTU10
009730000104     C                   ELSE
009740000104     C                   MOVEL     20            WDTU10            8 0
009750000104     C                   ENDIF
009760941014     C*
009770020805
009780020805     c                   eval      $simfel = simfel
009790020805     c                   movel     simfel        $mbr
009800070329     c
009810070329     c* Per EDP note sempre attive, se lna ha geo attiva
009820070329     c                   if        %subst(knmus:1:3)='EDP'
009830070329     c                   eval      dutgeot='S'
009840070329     c                   else
009850070329     c     dutpou        chain     azorg01l
009860070329     c                   if        %found(azorg01l)
009870070329     c                   movel     orgde8        og148
009880070329     c                   else
009890070329     c                   clear                   og148
009900070329     c                   endif
009910070329     c                   movel     §oggeot       dutgeot           1
009920070329     c                   endif
009930941020     C***
009940941020     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
009950941020     C***
009960941020     C                   MOVEL     'QT'          COD
009970941020     C                   MOVEL     '1       '    KEY
009980941212     C                   EXSR      CTABEL
009990941020     C  N30              MOVEL     TBLUNI        DSQT1
010000941020     C   30              CLEAR                   DSQT1
010010941214     C***
010020941214     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"Z"
010030941214     C***
010040941214     C                   MOVEL     '7T'          COD
010050941214     C                   MOVEL(P)  'Z'           KEY
010060941220     C                   EXSR      CTABEL
010070941214     C   30              CLEAR                   DS7T
010080941214     C  N30              MOVEL     TBLUNI        DS7T
010090941214     C*
010100970901     C* FLAG DI SOSTITUZIONE
010110941214     C                   MOVEL     §7TAFA        ZAFA              1
010120941214     C                   MOVEL     §7TAAA        ZAAA              1
010130941214     C                   MOVEL     §7TAFN        ZAFN              1
010140941214     C                   MOVEL     §7TAAN        ZAAN              1
010150970418     C***
010160970418     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"T"
010170970418     C***
010180970418     C                   MOVEL     '7T'          COD
010190970418     C                   MOVEL(P)  'T'           KEY
010200970418     C                   EXSR      CTABEL
010210970418     C   30              CLEAR                   DS7T
010220970418     C  N30              MOVEL     TBLUNI        DS7T
010230970418     C*
010240970901     C* FLAG DI SOSTITUZIONE
010250970418     C                   MOVEL     §7TAFA        TAFA              1
010260970418     C                   MOVEL     §7TAAA        TAAA              1
010270970418     C                   MOVEL     §7TAFN        TAFN              1
010280970418     C                   MOVEL     §7TAAN        TAAN              1
010290991001     C*
010300991001     C* CARICO DA SIGLE VARIE
010310991001     C                   MOVEL     '$2'          COD
010320991001     C                   MOVEL(P)  '1'           KEY
010330991001     C                   EXSR      CTABEL
010340991001     C  N30              MOVEL     TBLUNI        DS$2
010350991001     C   30              CLEAR                   DS$2
010360990930     C**
010370990930     C* CARICO VARIE CHE FANNO PARTE DEL TOTALE IMPONIBILE
010380990930     C**
010390990930     C                   Z-ADD     0             II                3 0
010400990930     C                   MOVEL     'CC'          COD
010410990930     C     KTAB1         CHAIN     TABEL                              30
010420990930    1C     *IN30         DOWEQ     *OFF
010430990930    2C     TBLFLG        IFEQ      ' '
010440990930     C                   MOVEL     TBLKEY        W005A             5
010450990930    3C     W005A         IFEQ      'VARIE'
010460990930     C                   MOVEL     TBLUNI        DSCC
010470990930     C* SIGLA VARIA
010480990930    4C     §CCFL6        IFEQ      'S'
010490990930     C                   ADD       1             II
010500990930     C                   MOVE      TBLKEY        SVA(II)
010510990930    4C                   ENDIF
010520990930    3C                   ENDIF
010530990930    2C                   ENDIF
010540990930     C**
010550990930     C     KTAB1         READE     TABEL                                  30
010560990930    1C                   ENDDO
010570991006     c*
010580991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
010590030612     C                   CLEAR                   Tibs02DS
010600991117     C                   CLEAR                   DGED
010610991117     C                   MOVEL     'C'           T02MOD
010620991117     C                   MOVEL     KNSIF         T02SIF
010630991117     C                   MOVEL     'GED'         T02COD
010640991117     C                   MOVEL     '1'           T02KE1
010650991117     C                   CALL      'TIBS02R'
010660991117     C                   PARM                    KPJBA
010670030612     C                   PARM                    Tibs02DS
010680991117     C     T02ERR        IFEQ      *BLANKS
010690991117     C                   MOVEL     T02UNI        DGED
010700991117     C                   ENDIF
010710011009     C* REPERIMENTO importi DELLA MONETA DI CONTO
010720030612     C                   CLEAR                   Tibs02DS
010730011009     C                   CLEAR                   DGEI
010740011009     C                   MOVEL     'C'           T02MOD
010750011009     C                   MOVEL     KNSIF         T02SIF
010760011009     C                   MOVEL     'GEI'         T02COD
010770011009     C                   MOVEL     §GEDCN        T02KE1
010780011009     C                   CALL      'TIBS02R'
010790011009     C                   PARM                    KPJBA
010800030612     C                   PARM                    Tibs02DS
010810011009     C     T02ERR        IFEQ      *BLANKS
010820011009     C                   MOVEL     T02UNI        DGEI
010830011120     C                   Z-ADD     §GEDFC        DFCSAV
010840011220     C                   Z-ADD     §geimf        imfsav
010850011009     C                   ENDIF
010860011009      * Tabella CV della moneta di conto
010870011009     C                   CLEAR                   DSCV
010880011009     C                   MOVEL     'CV'          COD
010890011009     C                   MOVEL(P)  §GEDCN        KEY
010900011009     C                   EXSR      CTABEL
010910011009     C     *IN30         IFEQ      *OFF
010920011009     C                   MOVEL     TBLUNI        DSCV
010930011009     C                   MOVEL     §CVDEC        DECSAV
010940011009     C                   ENDIF
010950000510     C***
010960000510     C* CARIO TABELLA VARIABILI DPD E POSTE
010970000510     C***
010980000510     C                   MOVEL     '3I'          COD
010990000510     C                   MOVEL(P)  'POSTE'       KEY
011000000510     C                   EXSR      CTABEL
011010000510     C  N30              MOVEL     TBLUNI        DS3IPT
011020000510     C   30              CLEAR                   DS3IPT
011030000510     C*
011040000510     C                   MOVEL     '3I'          COD
011050000510     C                   MOVEL(P)  'DPD'         KEY
011060000510     C                   EXSR      CTABEL
011070000510     C  N30              MOVEL     TBLUNI        DS3IDP
011080000510     C   30              CLEAR                   DS3IDP
011090020715
011100020715      * carico sk con tipi bolla che prevedono l'assicurazione
011110020715     c                   eval      cod = 'TB'
011120020715     c                   clear                   key
011130020715     c                   clear                   xx
011140020715     c     ktab1         setll     tabel00f
011150020715     c                   do        *hival
011160020715     c     ktab1         reade     tabel00f
011170020715      * fine file
011180020715     c                   if        %eof(tabel00f)
011190020715     c                   leave
011200020715     c                   endif
011210020715      * record annullato
011220020715     c                   if        tblflg <> *blanks
011230020715     c                   iter
011240020715     c                   endif
011250020715     c                   movel     tbluni        dstb
011260020715     c                   if        §tbfas = '1'
011270020715     c                   add       1             xx
011280020715     c                   eval      tbl(xx) = tblkey
011290020715     c                   endif
011300020715     c                   enddo
011310070309
011320070309      * Carico sk con eventi che prevedono modifica della cons.rich
011330070309     c                   eval      cod = '2Z'
011340070309     c                   clear                   key
011350070309     c                   clear                   xx
011360090526     c                   clear                   yy
011370070309     c     ktab1         setll     tabel00f
011380070309     c                   do        *hival
011390070309     c     ktab1         reade     tabel00f
011400070309      * fine file
011410070309     c                   if        %eof(tabel00f)
011420070309     c                   leave
011430070309     c                   endif
011440070309      * record annullato
011450070309     c                   if        tblflg <> *blanks
011460070309     c                   iter
011470070309     c                   endif
011480070309     c                   movel     tbluni        ds2Z
011490090526     c* x pgm non richiamato--> controllo solo gli S-sempre
011500090526     c                   if        §2ZADR='S'
011510070309     c                   add       1             xx
011520090526     c                   eval      ADRs(xx) = tblkey
011530070309     c                   endif
011540090526     c*
011550090526     c* x pgm     richiamato--> controllo sia S-sempre  che
011560090526     c*                         C-richiamati
011570090526     c                   if        §2ZADR<>' '
011580090526     c                   add       1             yy
011590090526     c                   eval      ADRc(yy) = tblkey
011600090526     c                   endif
011610070309     c                   enddo
011620021121
011630021121      * Aggancio tabella default tariffe fedex
011640030612     c                   Clear                   Tibs02DS
011650021121     c                   Movel     'C'           T02Mod
011660021121     c                   Movel     KNSIF         T02Sif
011670021121     c                   Movel     'DFE'         T02Cod
011680021121     c                   Movel     'FED'         T02Ke1
011690021121     c                   Call      'TIBS02R'
011700021121     c                   Parm                    Kpjba
011710030612     c                   Parm                    Tibs02DS
011720021121     c                   If        T02Err = *Blanks
011730021121     c                   Movel     T02Uni        DDfe
011740021121     c                   Else
011750021121     c                   Clear                   DDfe
011760021121     c                   EndIf
011770030710
011780060131      * Aggancio tabella VPO recpord VPO
011790030710     c                   Clear                   Dvpo
011800030710     c                   Clear                   Tibs02ds
011810030710     c                   Eval      T02Mod = 'C'
011820030710     c                   Eval      T02Sif = knsif
011830030710     c                   Eval      T02Cod = 'VPO'
011840030710     c                   Movel(p)  'VPO'         T02Ke1
011850030710     c                   Call      'TIBS02R'
011860030710     c                   Parm                    Kpjba
011870030710     c                   Parm                    Tibs02ds
011880030710     c                   If        T02Err = *Blanks
011890030710     c                   Movel     T02Uni        Dvpo
011900030710     c                   EndIf
011910060131
011920060131      * Aggancio tabella VPO record DECO
011930071031     c                   Clear                   Dvpodeco
011940071031     c                   Clear                   Tibs02ds
011950071031     c                   Eval      T02Mod = 'C'
011960071031     c                   Eval      T02Sif = knsif
011970071031     c                   Eval      T02Cod = 'VPO'
011980071031     c                   Movel(p)  'DECO'        T02Ke1
011990071031     c                   Call      'TIBS02R'
012000071031     c                   Parm                    Kpjba
012010071031     c                   Parm                    Tibs02ds
012020071031     c                   If        T02Err = *Blanks
012030071031     c                   Movel     T02Uni        Dvpodeco
012040071031     c                   EndIf
012050140130
012060140130      * Aggancio tabella VPO record DECOFI816TEL
012070140130     c                   Clear                   Tibs02ds
012080140130     c                   Clear                   fil816tel
012090140130     c                   Eval      T02Mod = 'C'
012100140130     c                   Eval      T02Sif = knsif
012110140130     c                   Eval      T02Cod = 'VPO'
012120140130     c                   Movel(p)  'DECOFI816TEL'T02Ke1
012130140130     c                   Call      'TIBS02R'
012140140130     c                   Parm                    Kpjba
012150140130     c                   Parm                    Tibs02ds
012160140130     c                   If        T02Err = *Blanks
012170140130     c                   Movel     T02Uni        fil816tel
012180140130     c                   EndIf
012190150414
012200150414      * Aggancio tabella "VPO" "EXPO"
012210151119     C*                  CLEAR                   tibs02ds
012220151119     c*                  clear                   fil_expo
012230151119     C*                  MOVEL     'C'           T02MOD
012240151119     C*                  MOVEL     KNSIF         T02SIF
012250151119     C*                  MOVEL     'VPO'         T02COD
012260151119     c*                  movel(P)  'EXPO'        t02ke1
012270151119     C*                  CALL      'TIBS02R'
012280151119     C*                  PARM                    KPJBA
012290151119     C*                  PARM                    tibs02ds
012300151119    2C*                  IF        T02ERR = *BLANKS
012310151119     C*                  MOVEa     T02uni        fil_expo
012320151119    2C*                  ENDIF
012330141029
012340141029      * Aggancio tabella VPO record DECOFI816DIR
012350141029     c                   clear                   trulvpods
012360141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
012370141029     c                   call      'TRULVPOR'
012380141029     c                   parm                    trulvpods
012390141029     c                   eval      skfilokdir=skfilok
012400030710
012410030710      * Aggancio tabella MIA
012420030710     c                   Clear                   miapsw
012430030710     c                   Clear                   Tibs02ds
012440030710     c                   Eval      T02Mod = 'C'
012450030710     c                   Eval      T02Sif = knsif
012460030710     c                   Eval      T02Cod = 'MIA'
012470030710     c                   Movel(p)  'TUTTI'       T02Ke1
012480030710     c                   Call      'TIBS02R'
012490030710     c                   Parm                    Kpjba
012500030710     c                   Parm                    Tibs02ds
012510030710     c                   If        T02Err = *Blanks
012520030710     c                   Movel     T02Uni        miapsw
012530030710     c                   EndIf
012540031120
012550031120      * Aggancio tabella AR5 per record 'GEN'
012560031120     c                   Clear                   dAr5
012570031120     c                   Clear                   Tibs02ds
012580031120     c                   Eval      T02Mod = 'C'
012590031120     c                   Eval      T02Sif = knsif
012600031120     c                   Eval      T02Cod = 'AR5'
012610031120     c                   Movel(p)  'GEN'         T02Ke1
012620031120     c                   Call      'TIBS02R'
012630031120     c                   Parm                    Kpjba
012640031120     c                   Parm                    Tibs02ds
012650031120     c                   If        T02Err = *Blanks
012660031120     c                   Movel     T02Uni        dAr5
012670031120     c                   EndIf
012680040806      * Tabella 2G
012690040806     c                   Movel     '2G'          cod
012700040902     c                   Movel(p)  '2'           key
012710040806     c                   ExSr      ctabel
012720040902     c  n30              Movel     TblUni        ds2g2
012730040902     c   30              Clear                   ds2g2
012740050901      *
012750941222     C***
012760931119     C* DEFINIZIONE CAMPI
012770931119     C***
012780130411     C**** *LIKE         DEFINE    D66BDI        §7LFIN
012790991112     C     *LIKE         DEFINE    D66FFI        §7LFFI
012800991112     C     *LIKE         DEFINE    DATEU         WDATA
012810011010     C     *LIKE         DEFINE    §CVDEC        COMDEC
012820990702     C     *LIKE         DEFINE    AR6IMV        SAVIMV
012830151016     C     *LIKE         DEFINE    AR6IMV        SAVIMV_fat
012840990702     C     *LIKE         DEFINE    FVVNPG        KNPG
012850970423     C     *LIKE         DEFINE    SOVR(1)       SOARBT
012860941213     C     *LIKE         DEFINE    SOVR(1)       SAARBT
012870941213     C     *LIKE         DEFINE    SOVR(1)       SDARBT
012880941213     C     *LIKE         DEFINE    AR6SV1        WSV1
012890941212     C     *LIKE         DEFINE    AR6VA1        WVA1
012900941212     C     *LIKE         DEFINE    TAMPPA        WPPA
012910941212     C     *LIKE         DEFINE    TAMMPA        WMPA
012920050202     C     *LIKE         DEFINE    ilv59FIE      WFIE
012930941209     C     *LIKE         DEFINE    §15EFT        DESEFT
012940941209     C     *LIKE         DEFINE    TBLKUT        §KUT
012950911211     C     *LIKE         DEFINE    TBLCOD        §COD
012960911211     C     *LIKE         DEFINE    TBLKEY        §KEY
012970941212     C     *LIKE         DEFINE    ARBIAS        VARIAS
012980950426     C     *LIKE         DEFINE    ARBIAS        COMIAS
012990930507     C     *LIKE         DEFINE    ARBCTR        COMCTR
013000941220     C     *LIKE         DEFINE    ARBKSC        KSC2
013010950104     C     *LIKE         DEFINE    ARBFAP        COMFAP
013020950407     C     *LIKE         DEFINE    NEWCBO        SAVCBO
013030950419     C     *LIKE         DEFINE    VIDPID        WPIVA
013040011009      *
013050011120     C     *LIKE         DEFINE    §GEDFC        DFCSAV
013060011220     C     *LIKE         DEFINE    §GEimf        imfsav
013070011009     C     *LIKE         DEFINE    §CVDEC        DECSAV
013080011009      *
013090941220     C* PER I PADRONCINI
013100950113     C     *LIKE         DEFINE    ARBKSC        WKSC
013110950113     C     *LIKE         DEFINE    ARBRSM        WRSM
013120950113     C     *LIKE         DEFINE    ARBRSD        OLDRSD
013130150206     C     *LIKE         DEFINE    ARBlod        OLDlod
013140950113     C     *LIKE         DEFINE    ARBCAD        OLDCAD
013150950113     C     *LIKE         DEFINE    ARBFIN        OLDFIN
013160941209     C     *LIKE         DEFINE    ARBVLF        OLDVLF
013170950114     C     *LIKE         DEFINE    ARBFVF        OLDFVF
013180941209     C     *LIKE         DEFINE    ARBPKF        OLDPKF
013190950113     C     *LIKE         DEFINE    ARBRSM        OLDRSM
013200950113     C     *LIKE         DEFINE    ARBCAM        OLDCAM
013210950113     C     *LIKE         DEFINE    ARBKSC        OLDKSC
013220941220     C*
013230950113     C     *LIKE         DEFINE    ARBVLF        WVOL
013240950113     C     *LIKE         DEFINE    ARBVLF        WSOTV
013250941212     C     *LIKE         DEFINE    ARBVLF        WADDV
013260941215     C     *LIKE         DEFINE    ARBVLF        VARVLF
013270941215     C     *LIKE         DEFINE    ARBPKF        VARPKF
013280950113     C     *LIKE         DEFINE    ARBPKF        WPKG
013290941220     C* PER FAR RIDIGITARE L'IMPORTO
013300941215     C     *LIKE         DEFINE    ARBVAS        VARVAS
013310941215     C     *LIKE         DEFINE    ARBVAD        VARVAD
013320941213     C     *LIKE         DEFINE    AR9VCA        VARVCA
013330950918     C* PER CONTROLLARE SE E' MODIFICATO
013340950918     C     *LIKE         DEFINE    AR9VCA        SAVVCA
013350950918     C     *LIKE         DEFINE    AR9TIC        SAVTIC
013360950918     C     *LIKE         DEFINE    AR9CAS        SAVCAS
013370970808     C     *LIKE         DEFINE    VIDCAD        SAVCAD
013380970808     C     *LIKE         DEFINE    VIDCAD        WCAD
013390970808     C     *LIKE         DEFINE    VIDCAD        WCAD1
013400970808     C     *LIKE         DEFINE    VIDLOD        SAVLOD
013410970808     C     *LIKE         DEFINE    VIDLOD        WLOD
013420970808     C     *LIKE         DEFINE    VIDLOD        WLOD1
013430060111     c     *LIKE         DEFINE    VIDrsd        Wrsd1
013440060111     c     *LIKE         DEFINE    VIDrsd        savrsd
013450061116     c     *LIKE         DEFINE    VIDrsd        savrsdQ
013460060111     c     *LIKE         DEFINE    VIDind        wind1
013470060111     c     *LIKE         DEFINE    VIDind        savind
013480060111     c     *LIKE         DEFINE    VIDprd        wprd1
013490060111     c     *LIKE         DEFINE    VIDprd        savprd
013500060111     c     *LIKE         DEFINE    VIDnzd        wnzd1
013510060111     c     *LIKE         DEFINE    VIDnzd        savnzd
013520970808     C     *LIKE         DEFINE    E14CAP        VARCAD
013530970808     C     *LIKE         DEFINE    E14LOC        VARLOD
013540970808     C     *LIKE         DEFINE    E14CAP        VARCAM
013550970808     C     *LIKE         DEFINE    E14LOC        VARLOM
013560991006     C     *LIKE         DEFINE    AR6POR        VARPOR
013570991006     C     *LIKE         DEFINE    AR6VA1        VA2VA1
013580941220     C* PER IL CONTROLLO
013590941212     C     *LIKE         DEFINE    AR9VCA        WVLT
013600941212     C     *LIKE         DEFINE    AR4NOT        SAVRD2
013610941019     C     *LIKE         DEFINE    AR4TRC        WTRC
013620950424     C     *LIKE         DEFINE    VIDQFT        VARQFT
013630950424     C     *LIKE         DEFINE    VIDKLP        VARKLP
013640950424     C     *LIKE         DEFINE    VIDKSC        VARKSC
013650970418     C     *LIKE         DEFINE    §7TSOZ        WSOSC
013660970418     C     *LIKE         DEFINE    ARBFVF        WFVF
013670970813     C     *LIKE         DEFINE    VIDKSC        MEMKSC
013680991005     C     *LIKE         DEFINE    VIDDIV        DIVGEI
013690991006     C     *LIKE         DEFINE    VIDDIV        WDIV
013700991007     C     *LIKE         DEFINE    AR6DFT        DFT2BO
013710950113     C*
013720950113     C     *LIKE         DEFINE    FTDDDC        WDRT
013730950113     C     *LIKE         DEFINE    FTDPDR        WPDR
013740950113     C     *LIKE         DEFINE    FTDNDC        WNDC
013750950113     C     *LIKE         DEFINE    FTDDDC        WDDC
013760950113     C     *LIKE         DEFINE    FTDTSR        WTSR
013770020327     C     *LIKE         DEFINE    FTDSTP        wstp
013780950114     C* CAMPI COLLEGATI ALLA VARIAZIONE DEL MITTENTE DA STORICIZZARE
013790950114     C     *LIKE         DEFINE    ARBFVF        MITFVF
013800950114     C     *LIKE         DEFINE    ARBVLF        MITVLF
013810950114     C     *LIKE         DEFINE    ARBCTR        MITCTR
013820950601     C* CAMPI PER GESTIONE VARIAZIONI DI BOLLE ESTERE NON LOCALI
013830950601     C     *LIKE         DEFINE    ARBLNA        WEUV
013840950601     C     *LIKE         DEFINE    ARBCVB        PARCVB
013850950601     C     *LIKE         DEFINE    ARBDTV        PARDTV
013860950601     C     *LIKE         DEFINE    ARBORV        PARORV
013870950601     C     *LIKE         DEFINE    ARBAAS        PARAAS
013880950601     C     *LIKE         DEFINE    ARBLNP        PARLNP
013890950601     C     *LIKE         DEFINE    ARBNRS        PARNRS
013900950601     C     *LIKE         DEFINE    ARBNSP        PARNSP
013910950601     C     *LIKE         DEFINE    ARBCVB        PARCVC
013920950601     C     *LIKE         DEFINE    SIMFEL        PARFEV
013930130410     C***  *LIKE         DEFINE    ARITIP        KTIP
013940001204     C     *LIKE         DEFINE    ARBTC1        SAVTC1
013950001204     C     *LIKE         DEFINE    ARBTC1        SAVTC2
013960930507     C*
013970950113     C                   MOVEL     'R'           WTSR
013980911211     C                   MOVEL     'R'           RIS               1
013990930210     C                   MOVEL     'M'           EMME
014000990818     C                   MOVEL     'C '          FTC               2
014010130410     C***                MOVEL     'A'           KTIP
014020931119     C***
014030911211     C* GIRO UDATE
014040931119     C***
014050941014     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
014060941014     C                   TIME                    W0140            14 0
014070941014     C* UDATE IN GGMMAAAA
014080941014     C                   MOVE      W0140         WDTGIO            8 0
014090941014     C*
014100941014     C* UDATE IN AAAAMMGG
014110941014     C                   Z-ADD     WDTGIO        G02DAT
014120941014     C                   MOVEL     *BLANK        G02ERR
014130941014     C                   CALL      'XSRDA8'
014140941014     C                   PARM                    WLBDAT
014150941014     C                   MOVEL     G02INV        DATEU             8 0
014160030710     c     *iso          Move      dateu         wdataoggi
014170970423     C*
014180970423     C                   MOVEL     4             KNPG
014190941014     C**
014200941014     C*  KEY
014210941014     C**
014220941014     C     KARB          KLIST
014230941212     C                   KFLD                    D48AAS
014240941212     C                   KFLD                    D48LNP
014250941212     C                   KFLD                    D48NRS
014260941212     C                   KFLD                    D48NSP
014270941019     C     KARB2         KLIST
014280941212     C                   KFLD                    D48AAS
014290941212     C                   KFLD                    D48LNP
014300941212     C                   KFLD                    D48NRS
014310941212     C                   KFLD                    D48NSP
014320941019     C                   KFLD                    WTRC
014330130410     C***  KARI          KLIST
014340130410     C***                KFLD                    D48AAS
014350130410     C***                KFLD                    D48LNP
014360130410     C***                KFLD                    D48NRS
014370130410     C***                KFLD                    D48NSP
014380130410     C***                KFLD                    KTIP
014390941014     C     KFTT          KLIST
014400941215     C                   KFLD                    FTDPDR
014410941215     C                   KFLD                    FTDTSR
014420941215     C                   KFLD                    FTDNDC
014430950113     C                   KFLD                    FTDDDC
014440950113     C     KFTT2         KLIST
014450950113     C                   KFLD                    WPDR
014460950113     C                   KFLD                    WTSR
014470950113     C                   KFLD                    WNDC
014480950113     C                   KFLD                    WDRT
014490991005     C     KFTD          KLIST
014500950113     C                   KFLD                    WPDR
014510950113     C                   KFLD                    WTSR
014520950113     C                   KFLD                    WNDC
014530950113     C                   KFLD                    WDRT
014540950113     C                   KFLD                    WKSC
014550950113     C                   KFLD                    WRSM
014560020326     C     KFTD5         KLIST
014570020326     C                   KFLD                    WPDR
014580020326     C                   KFLD                    WTSR
014590020326     C                   KFLD                    WNDC
014600020326     C                   KFLD                    WDRT
014610020326     C                   KFLD                    D48AAS
014620020326     C                   KFLD                    D48LNP
014630020326     C                   KFLD                    D48NRS
014640020326     C                   KFLD                    D48NSP
014650941014     C* ACCESSO   TABEL                                    *
014660990930     C     KTAB1         KLIST                                                  *
014670990930     C                   KFLD                    CODUT                          *
014680990930     C                   KFLD                    COD               2            *
014690941014     C     KTAB2         KLIST                                                  *
014700941014     C                   KFLD                    CODUT                          *
014710941014     C                   KFLD                    COD               2            *
014720941014     C                   KFLD                    KEY               8            *
014730941014     C* ACCESSO   CNACO                                    *
014740941014     C     KACO          KLIST                                                  *
014750941014     C                   KFLD                    CODUT                          *
014760060131     C                   KFLD                    dutkci                         *
014770941014     C                   KFLD                    KSC               7 0          *
014780970423     C     KFVV          KLIST                                                  *
014790970423     C                   KFLD                    KNPG                           *
014800970423     C                   KFLD                    W0050                          *
014810020911     C                   KFLD                    Kfgs
014820941014     C* ACCESSO   TFTAM CON PROGRESSIVO                    *
014830941014     C*
014840941014     C     KTAM4         KLIST                                                  *
014850941014     C                   KFLD                    KSC                            *
014860941014     C                   KFLD                    COMCTR                         *
014870941014     C                   KFLD                    COMPRG            3 0          *
014880941014     C     KTAM6         KLIST                                                  *
014890941014     C                   KFLD                    KSC2                           *
014900941014     C                   KFLD                    COMCTR                         *
014910941014     C                   KFLD                    COMPRG                         *
014920941014     C     KTPT          KLIST                                                  *
014930941014     C                   KFLD                    KSC                            *
014940941014     C                   KFLD                    COMCTR                         *
014950941014     C                   KFLD                    COMPRG                         *
014960990818     C                   KFLD                    FTC               2            *
014970030124
014980030124     c     kAr5          Klist
014990030124     c                   Kfld                    D48Aas
015000030124     c                   Kfld                    D48Lnp
015010030124     c                   Kfld                    D48Nrs
015020030124     c                   Kfld                    D48Nsp
015030030124     c                   Kfld                    kAr5Trd
015040030722
015050030722     C     kazcln        klist
015060030722     C                   kfld                    ktfp
015070030722     C                   kfld                    ktfa
015080030722     C                   kfld                    kann
015090030722     C                   kfld                    kmes
015100040809
015110040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
015120080701     C***  KDCT1_C       KLIST
015130080701     C***                KFLD                    dsaac
015140080701     C***                KFLD                    dsfil
015150080701     C***                KFLD                    dsnca
015160041020     C* CHIAVE SU Fiqdt01L
015170041020     C     Kqdt          KLIST
015180041020     C                   KFLD                    kfgs
015190041020     C                   KFLD                    w0060
015200050601     c*
015210050601     C* CHIAVE SU Tita430C
015220050601     C     Kta4          KLIST
015230050601     C                   KFLD                    D48AAS
015240050601     C                   KFLD                    D48LNP
015250050601     C                   KFLD                    D48NRS
015260050601     C                   KFLD                    D48NSP
015270050601     c                   kfld                    kta4trc
015280141029
015290141029     C     Kidc1         KLIST
015300141029     C                   KFLD                    D48AAS
015310141029     C                   KFLD                    D48LNP
015320141029     C                   KFLD                    D48NRS
015330141029     C                   KFLD                    D48NSP
015340141029     C                   KFLD                    w0140I
015350030722
015360941212     C**
015370941212     C                   MOVEL     W256A         KPJBU
015380001213     C*
015390001213     C                   MOVE      '0'           OPN48             1
015400911211     C                   ENDSR
015410120426     C*
015420120426      * APERTURA TITA430C
015430120426     c* se provengo da variazione bolla di sede non ho bisogno del tita4
015440120426     C     OPN_TITA4     BEGSR
015450120426     c                   if        %subst(d48cvb:1:1)<>'S'
015460151016     C                   if        NOT %OPEN(TITA430C)
015470120426      * Se sono in ambiente buono - GAITRAGRU
015480141020     c*****              If        knsif = 'FILTRA201 '
015490141020     c                   If        %subst(knsif:7:1) <> 'P'
015500120426     c                   Eval      wlib = 'GAITRAGRU '
015510120426      *  Se sono in ambiente di prova - GAITRAGRPS
015520120426     c                   Else
015530120426     c                   Eval      wlib = 'GAITRAGRPS'
015540120426     c                   EndIf
015550120426     c*
015560120426     c                   Clear                   commt
015570151016     c                   Movel(p)  cmdt(1)       commt
015580120426     c                   Eval      %Subst(commt:30:10) = wlib
015590120426     c                   Eval      commt =
015600151016     c                             %trim(commt) + '/TITA430C)'
015610120426     c                   Call      'QCMDEXC'
015620120426     c                   Parm                    commt
015630120426     c                   Parm                    lenght
015640151016     c                   Open      tita430c
015650120426     c                   endif
015660151016     c* FIAR531C
015670151016     c                   if        not %open(fiar531c)
015680151016     c                   If        %subst(knsif:7:1) <> 'P'
015690151016     c                   Eval      wlib = 'GAITRAGRU '
015700151016      *  Se sono in ambiente di prova - GAITRAGRPS
015710151016     c                   Else
015720151016     c                   Eval      wlib = 'GAITRAGRPS'
015730151016     c                   EndIf
015740151016     c*
015750151016     c                   Clear                   commt
015760151016     c                   Movel(p)  cmdt(2)       commt
015770151016     c                   Eval      %Subst(commt:30:10) = wlib
015780151016     c                   Eval      commt =
015790151016     c                             %trim(commt) + '/FIAR531C)'
015800151016     c                   Call      'QCMDEXC'
015810151016     c                   Parm                    commt
015820151016     c                   Parm                    lenght
015830151016     c                   Open      fiar531c
015840151016     c                   endif
015850151016     c* TITAS30C
015860151016     C                   if        NOT %OPEN(TITAS30C)
015870151016      * Se sono in ambiente buono - GAITRAGRU
015880151016     c                   If        %subst(knsif:7:1) <> 'P'
015890151016     c                   Eval      wlib = 'GAITRAGRU '
015900151016      *  Se sono in ambiente di prova - GAITRAGRPS
015910151016     c                   Else
015920151016     c                   Eval      wlib = 'GAITRAGRPS'
015930151016     c                   EndIf
015940151016     c*
015950151016     c                   Clear                   commt
015960151016     c                   Movel(p)  cmdt(3)       commt
015970151016     c                   Eval      %Subst(commt:30:10) = wlib
015980151016     c                   Eval      commt =
015990151016     c                             %trim(commt) + '/TITAS30C)'
016000151016     c                   Call      'QCMDEXC'
016010151016     c                   Parm                    commt
016020151016     c                   Parm                    lenght
016030151016     c                   Open      titas30c
016040151016     c                   endif
016050151016     c                   endif
016060120426     C                   ENDSR
016070900509     C*
016080050907      *--- CONTROLLO LA PARTICOLARITA' GIACENZA NEL PERIODO DELLE TRAZIONI RIDOTTE ---*
016090050907     C     Contr7q       BEGSR
016100050901      *
016110050907      * Inizializzo un flag che indica se è consentita la variazione della data cons. richiesta
016120050907      * in periodo d trazioni ridotte ed in presenza d particolarità consegna genericamente "G8"
016130050907     c                   clear                   wDCRG8            1
016140050901      * Determino se variazione di bolla con part. giacenza durante
016150050901      *  periodo delle trazioni ridotte
016160050901     c                   If        dateu >= §2g2dri and
016170050901     c                             dateu <= §2g2drf
016180050901      * posso modificare se arbgga <> *Blanks e se in tabella 7Q ha 'R' come cod. disposizione
016190050901     c                   If        ArbGga <> *Blanks
016200050901     c                   Movel     '7Q'          cod
016210050901     c                   Movel(p)  ArbGga        key
016220050901     c                   ExSr      ctabel
016230050901     c  n30              Movel     TblUni        ds7q
016240050901     c   30              Clear                   ds7q
016250050901     c                   If        §7q4d1 = 'R' or §7q4d2 = 'R' or
016260050901     c                             §7q4d3 = 'R' or §7q4d4 = 'R' or
016270050901     c                             §7q4d5 = 'R'
016280050901     c                   Movel     'S'           wDCRG8
016290050901     c                   EndIF
016300050901     c                   EndIF
016310050901     c                   EndIF
016320050901
016330050901     c                   EndSR
016340080201     c
016350900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016360900509     C     CONTR         BEGSR
016370030122
016380030122     c     Riprova       Tag
016390030122
016400941014     C                   SETOFF                                       90
016410941014     C                   SETOFF                                       203519
016420941014     C                   SETOFF                                       253403
016430950609     C                   SETOFF                                       262729
016440990415     C                   SETOFF                                       73
016450151222     c                   clear                   wfile
016460941014     C***
016470941014     C* VEDO SE E' UN PROGRAMMA RICHIAMATO
016480941014     C***
016490941019     C     D48TRC        IFNE      *BLANKS
016500941019     C                   SETON                                        20
016510950114     C                   ELSE
016520941019     C                   ENDIF
016530950607     C***
016540950607     C* ABILITO L'ENTER COME TASTO DI AGGIORNAMENTO SE RICHIESTO
016550950607     C***
016560950607     C     D48FFR        IFNE      'V'
016570070309     C     D48FFR        andne     'F'
016580950607     C                   SETON                                        26
016590950607     C                   ENDIF
016600070309     c
016610070309     c* Se richiamo con F  --> mi imposto la E  (ma con 26 off non effettua
016620070309     c*    l'aggiornamento per ENTER)
016630070309     c                   if        d48ffr='F'
016640070309     c                   eval      d48ffr='E'
016650070309     c                   endif
016660070309     c
016670950607     C***
016680950607     C* ABILITO F12 SE RICHIESTO
016690950607     C***
016700020603     C     W48F12        IFEQ      'S'
016710950607     C                   SETON                                        27
016720950607     C                   ENDIF
016730950609     C***
016740950609     C* PROTEGGO TUTTI I CAMPI SE RICHIAMATO DA PGM PER ACQUISIZIONE
016750950609     C* AUTOMATICA VARIAZIONI DA PC ESTERO
016760950609     C***
016770140404     C***  D48EUV        IFEQ      'N'
016780140404     C***                SETON                                        29
016790140404     C***                END
016800941222     C**
016810941222     C* APRO I FILE A SECONDA DEL TIPO BOLLA
016820941222     C**
016830941222     C     D48TBO        IFEQ      'A'
016840941223     C                   SETON                                        10
016850941222     C                   ELSE
016860941223     C                   SETOFF                                       10
016870941222     C                   ENDIF
016880941222     C**
016890930505     C* SPEDIZIONE
016900941222     C**
016910151218     c***                if        d48cvb<>'SD' and d48cvb<>'SF'
016920151218     C***10KARB          CHAIN     FNARB01L                           3031
016930151218     C**N10KARB          CHAIN     FNBLP01L                           3031
016940930505     C*
016950930505     C* NON UTILIZZABILE
016960151218     C***  *IN31         IFEQ      *ON
016970151218     C***                MOVEL     MSG(1)        D48MSG
016980151218     C***                MOVEL     '1'           D48ERR
016990151218     C***                SETON                                        90
017000151218     C***                GOTO      ENDCTR
017010151218     C***                ENDIF
017020941223     C* NON TROVATA SPEDIZIONE
017030151218     C***  *IN30         IFEQ      *ON
017040151218     C***                MOVEL     MSG(61)       D48MSG
017050151218     C***                MOVEL     '1'           D48ERR
017060151218     C***                SETON                                        90
017070151218     C***                GOTO      ENDCTR
017080151218     C***                ENDIF
017090151218     c***                endif
017100080201     C*
017110080201     C* SE PGM RICHIAMATO CONTROLLO SOLO LA CORRETTEZZA DELLA CAUSALE
017120080201     C                   CLEAR                   Fnlr66DS
017130080201     C                   MOVEL     'C'           D66GES
017140080201     C                   MOVEL     D48CVB        D66CVB
017150080201     C  N20              MOVEL     D48AAS        D66AAS
017160080201     C  N20              MOVEL     D48LNP        D66LNP
017170080201     C  N20              MOVEL     D48NRS        D66NRS
017180080201     C  N20              MOVEL     D48NSP        D66NSP
017190080201     C  N20              MOVEL     D48TBO        D66TBO
017200160115     c                   eval      d66bnp='S'
017210080201     C                   MOVEL     Fnlr66DS      KPJBU
017220080201     C                   CALL      'FNLR66R'
017230080201     C                   PARM                    KPJBA
017240080201     C                   MOVEL     KPJBU         Fnlr66DS
017250080201     C*
017260080201     C* TROVATO ERRORE
017270080201     C     D66ERR        IFEQ      '1'
017280080201      * se prima ho variato il destinatario ed ora devo aggiornare le consegne particolari
017290080201      * in caso di errore provo a cambiare la causale di modifica da 'CP' a 'CR'
017300080201     c                   If        wI0 = 'S'
017310080201     c                   Movel     'CR'          D48Cvb
017320080201     c                   Clear                   wI0
017330080201     c                   Goto      Riprova
017340080201     c                   Else
017350080201     C                   MOVEL     D66MSG        D48MSG
017360080201     C                   MOVEL     D66ERR        D48ERR
017370080201     C                   SETON                                        90
017380151218     c** 10              unlock    fnarb01l
017390151218     c**n10              unlock    fnblp01l
017400080201     c
017410080201     C                   GOTO      ENDCTR
017420080201     c                   EndIf
017430080201     C                   ENDIF
017440141112     c* Le variazioni I0 non sono ammesse se presenti dispo di dirottamento
017450141112     c                   if        d48cvb='I0' or d48cvb='I7' or
017460141112     c                             d48cvb='I8' or d48cvb='I9'
017470141112     c                   clear                   fnlry09ds2
017480141112     c                   eval      ILRY09TCH='T'
017490141112     c                   eval      ILRY09TDIS='D'
017500141112     c                   eval      ILRY09AAS=d48aas
017510141112     c                   eval      ILRY09LNP=d48lnp
017520141112     c                   eval      ILRY09NRS=d48nrs
017530141112     c                   eval      ILRY09NSP=d48nsp
017540141112     c                   eval      kpjbu=fnlry09ds2
017550141112     c                   call      'FNLRY09C'
017560141112     c                   parm                    kpjba
017570141112     c                   eval      fnlry09ds2=kpjbu
017580141112     c                   if        OLRY09ESIT='1'
017590141112     C                   MOVEL     msg(156)      D48MSG
017600141112     C                   MOVEL     '1'           D48ERR
017610141112     C                   SETON                                        90
017620151218     c** 10              unlock    fnarb01l
017630151218     c**n10              unlock    fnblp01l
017640141112     c
017650141112     C                   GOTO      ENDCTR
017660141112     c                   endif
017670141112     c                   endif
017680151218     C**
017690151218     C* SPEDIZIONE
017700151218     C**
017710151218     c                   if        d48cvb<>'SD' and d48cvb<>'SF'
017720151221     c* Manutenzione bolle in arrivo
017730151221     c                   if        *in10
017740151221     c*
017750151221     c                   setoff                                       30
017760151221     c                   eval      wfile='A'
017770151221     c                   if        d66utp<>'P'
017780151221     C     KARB          CHAIN     FNARB01L                           3031
017790151221     c                   endif
017800151221     c                   if        (*in30 and d66utp='T') or d66utp='P'
017810151221     C     KARB          CHAIN     FNblp01L                           3031
017820160112     c                   if        not *in31 and not*in30  and ARBft1<>*blanks
017830151221     c                   unlock    fnblp01l
017840151221     C                   MOVEL     MSG(162)      D48MSG
017850151221     C                   MOVEL     '1'           D48ERR
017860151221     C                   SETON                                        90
017870151221     C                   GOTO      ENDCTR
017880151221     c                   endif
017890151221     c                   eval      wfile='P'
017900151221     c                   endif
017910151221     c                   else
017920151221     c* Manutenzione in partenza
017930151222     c                   eval      wfile='P'
017940151221     C     KARB          CHAIN     FNBLP01L                           3031
017950151221     c                   endif
017960151218     C*
017970151218     C* NON UTILIZZABILE
017980151218     C     *IN31         IFEQ      *ON
017990151218     C                   MOVEL     MSG(1)        D48MSG
018000151218     C                   MOVEL     '1'           D48ERR
018010151218     C                   SETON                                        90
018020151218     C                   GOTO      ENDCTR
018030151218     C                   ENDIF
018040151218     C* NON TROVATA SPEDIZIONE
018050151218     C     *IN30         IFEQ      *ON
018060151218     C                   MOVEL     MSG(61)       D48MSG
018070151218     C                   MOVEL     '1'           D48ERR
018080151218     C                   SETON                                        90
018090151218     C                   GOTO      ENDCTR
018100151218     C                   ENDIF
018110151218     c                   endif
018120080201     C* KEY SPEDIZIONE
018130080201     C                   MOVEL     D48CVB        VIDCVR
018140080201     C                   MOVEL     D48CVB        FLGCVR            1
018150080201     C*
018160080201     C* IMPOSTO IL CAMPO §7LFFI CHE MI SERVE PER ALLOCARE I FILE
018170080201     C                   MOVEL     D66FFI        §7LFFI
018180080201     C*
018190080201     C* IMPOSTO IL CAMPO §7LFIN CHE MI SERVE PER PROTEGGERE I CAMPI
018200080201     C* CHE NON DEVO VARIARE
018210130411     C***                MOVEL     D66BDI        §7LFIN
018220941014     C**
018230941014     C**
018240900509     C     ENDCTR        ENDSR
018250910429     C*
018260910429     C*--- CARICO DATI BOLLA -----------------------------------------*
018270910429     C     CARICA        BEGSR
018280920123     C                   SETOFF                                       040521
018290911211     C                   SETOFF                                       131415
018300990930     C                   SETOFF                                       0102
018310941220     C                   SETOFF                                       363789
018320941213     C                   SETOFF                                       111209
018330991001     C                   SETOFF                                       882307
018340941222     C                   SETOFF                                       525306
018350991012     C                   SETOFF                                       167308
018360061107     C                   SETOFF                                       747576
018370131001     C                   SETOFF                                       878681
018380140213     C                   SETOFF                                       838477
018390140408     c                   setoff                                       68
018400011018      *
018410011018     C                   CLEAR                   FLGVRN
018420110906     C                   CLEAR                   lnacons
018430941102     C*
018440950120     C                   CLEAR                   WIN95
018450950120     C                   CLEAR                   WIN97
018460950120     C                   CLEAR                   WIN93
018470131016     C                   CLEAR                   WINF14
018480950120     C                   CLEAR                   VARVCA
018490941215     C                   CLEAR                   VARVAS
018500941215     C                   CLEAR                   VARVAD
018510941215     C                   CLEAR                   VARVLF
018520941215     C                   CLEAR                   VARPKF
018530990702     C                   CLEAR                   SAVIMV
018540151016     C                   CLEAR                   SAVIMV_fat
018550991006     C                   CLEAR                   WDIV
018560991006     C                   CLEAR                   VARPOR
018570991006     C                   CLEAR                   VA2VA1
018580991012     C                   CLEAR                   SAVCBO
018590091124     C                   CLEAR                   WPT1              1
018600941209     C                   MOVEL     0             CIN               1 0
018610941209     C                   MOVEL     0             DIE               1 0
018620941212     C                   MOVEL     0             SEI               1 0
018630941214     C                   MOVEL     0             SET               1 0
018640941223     C                   MOVEL     0             OTT               1 0
018650970808     C                   MOVEL     0             WZV               1 0
018660970808     C                   MOVEL     0             WZ1               1 0
018670970808     C                   MOVEL     0             WZ2               1 0
018680070508     C                   MOVEL     0             WZ3               1 0
018690970808     C                   MOVEL     0             WZVM              1 0
018700970808     C                   MOVEL     0             WZ1M              1 0
018710970808     C                   MOVEL     0             WZ2M              1 0
018720141029     C                   MOVEL     0             Wforzdcr          1 0
018730141112     C                   MOVEL     0             Wforzgiac         1 0
018740990930     C                   MOVEL     ' '           WAR62             1
018750941019     C                   MOVEL     ' '           WAR6              1
018760941209     C                   MOVEL     ' '           WAR7              1
018770040123     c                   Clear                   War72
018780941019     C                   MOVEL     ' '           WAR9              1
018790941019     C                   MOVEL     ' '           WAR4D             1
018800150206     C                   MOVEL     ' '           WAR4X             1
018810061116     C                   MOVEL     ' '           WAR4Q             1
018820000623     C                   MOVEL     ' '           WBL4W             1
018830920319     C                   Z-ADD     0             COMKSC
018840930507     C                   Z-ADD     0             COMCTR
018850930507     C                   Z-ADD     0             COMPRG
018860911211     C                   Z-ADD     0             VI2DFT
018870911211     C                   Z-ADD     0             VIDDFT
018880941209     C                   Z-ADD     0             VARIAS
018890930507     C                   MOVEL     *BLANKS       VARCTR            3
018900941214     C                   CLEAR                   WSOST
018910030710     c                   Clear                   wokpsw
018920030729
018930030729     c                   Clear                   V1cBan
018940030729     c                   Clear                   V1cCor
018950030729     c                   Eval      *In70 = *Off
018960040123     c                   Clear                   ContaSv
018970040123     c                   Clear                   TotImv
018980060223     C                   CLEAR                   V1CCAS
018990060223     C                   CLEAR                   V1CTIC
019000060223     C                   CLEAR                   V1CVCA
019010060223     C                   CLEAR                   SAVCAS
019020060223     C                   CLEAR                   SAVTIC
019030060223     C                   CLEAR                   SAVVCA
019040090914     C                   CLEAR                   SAVrd2
019050150203     C                   CLEAR                   normlod
019060941215     C* PULIZIA DEI FORMATI VIDEO
019070950210     C                   CLEAR                   LR48D02
019080941215     C                   CLEAR                   LR48C03
019090941215     C                   CLEAR                   LR48D04
019100941215     C                   CLEAR                   LR48D05
019110941215     C                   CLEAR                   LR48D07
019120950104     C                   CLEAR                   LR48D08
019130961211     C                   CLEAR                   LR48D09
019140030124     c                   Clear                   Lr48d10
019150950213     C                   CLEAR                   V1CTIC
019160950213     C                   CLEAR                   V1CCAS
019170950213     C                   CLEAR                   V1CVCA
019180950213     C                   CLEAR                   V1CNT1
019190950213     C                   CLEAR                   V1CNT2
019200950213     C                   CLEAR                   V1CTC1
019210950213     C                   CLEAR                   V1DTC1
019220950213     C                   CLEAR                   V1CTC2
019230950213     C                   CLEAR                   V1DTC2
019240030124     c                   Clear                   VidNom
019250030124     c                   Clear                   VidTel
019260030124     c                   Clear                   VidMot
019270030122     c                   Clear                   wI0
019280031120     c                   Clear                   VidRef
019290070508     c                   Clear                   Vidtfd
019300031120     c                   Clear                   VidTeld
019310070306     c                   Clear                   AggioARP
019320070308     c                   Clear                   savdcr
019330070308     c                   Clear                   savtcr
019340070308     c                   Clear                   savhcr
019350070308     c                   Clear                   bollafbc
019360070309     c                   Clear                   Msgforza          3 0
019370150112     c                   Clear                   Msgforzatxt      78
019380070309     c                   clear                   eveADR            1
019390070910     c                   clear                   WesAR5GEN         1
019400130930     c                   clear                   WesAR5EMD         1
019410071025     c                   clear                   ForzaCDF
019420071025     c                   clear                   ForzaPID
019430071025     c                   clear                   ForzaPID2
019440071025     c                   clear                   sav11pid
019450071026     c                   clear                   AggioBol
019460111025     c                   clear                   Dcrindist         1
019470151019     c                   clear                   PesVolPkcn
019480151019     c                   clear                   PesVolSpc
019490151019     c                   clear                   PesVolImv
019500151019     c                   clear                   TassatoImv
019510061129     c  n10              movel     'T'           bollaft2
019520061129     c   10              movel     'R'           bollaft2
019530111207     c* ES - Per il momento lo imposto sempre = "S" in quanto l'ufficio qualità non è
019540111207     c*  sicuro di voler fare la modifica di bloccare la manut coonsegna richiesta
019550111207     c*  dalla manutenzione normale
019560111207     c                   eval      Dcrindist='S'
019570941223     C*
019580941223     C     D48TBO        IFEQ      'A'
019590941223     C                   SETON                                        10
019600941223     C                   ELSE
019610941223     C                   SETOFF                                       10
019620941223     C                   ENDIF
019630941223     C*
019640941223     C* CLEARO I FORMATI DELLE BOLLE
019650051118     C                   CLEAR                   FiAR9000
019660990930     C                   CLEAR                   FIAR7000
019670990930     C                   CLEAR                   FIAR6000
019680030124     c                   Clear                   Fiar5000
019690941220     C**
019700941220     C* PGM RICHIAMATO
019710941220     C**
019720941220     C                   SELECT
019730941220     C* PROGRAMMA RICHIAMATO    DSARBD
019740941220     C     D48TRC        WHENEQ    'D'
019750941220     C                   SETON                                        2034
019760941220     C* PROGRAMMA RICHIAMATO    DSARBK
019770941220     C     D48TRC        WHENEQ    'K'
019780950616     C                   SETON                                        203529
019790990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 1
019800941220     C     D48TRC        WHENEQ    'T'
019810941220     C                   SETON                                        2025
019820990416     C*
019830941220     C* PROGRAMMA RICHIAMATO    DSARBG
019840941220     C     D48TRC        WHENEQ    'G'
019850941220     C                   SETON                                        2003
019860990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 2
019870941220     C     D48TRC        WHENEQ    '2'
019880941220     C                   SETON                                        2019
019890130930     C* PROGRAMMA RICHIAMATO    DARBN TIPO RECORD N + parm
019900130930     C     D48TRC        WHENEQ    'N'
019910130930     c                   if        %parms=7
019920130930     C                   SETON                                        20
019930131008     c                   else
019940131008     C                   MOVEL     MSG(24)       D48MSG
019950131008     C                   MOVEL     '1'           D48ERR
019960131008     c                   leavesr
019970131008     c                   endif
019980131008     c
019990941220     C                   ENDSL
020000950607     C*
020010020603     C     W48F12        IFEQ      'S'
020020950607     C                   SETON                                        27
020030950607     C                   ENDIF
020040950609     C*
020050140404     C***  D48EUV        IFEQ      'N'
020060140404     C***                SETON                                        29
020070140404     C***                END
020080950114     C**
020090950114     C* 24 ON - VARIATO IL MITTENTE
020100950114     C**
020110950114     C     WRICVL        IFEQ      'S'
020120950114     C                   SETON                                        24
020130950114     C                   ENDIF
020140941019     C**
020150941019     C* CARICO I DATI DELLA VIDEATA GENERICA DELLA BOLLA
020160941019     C**
020170941019     C* CHAIN CON CODICE BOLLA
020180941019     C                   MOVEL     '3A'          COD
020190941102     C                   MOVEL(P)  ARBCBO        KEY
020200941212     C                   EXSR      CTABEL
020210941019     C  N30              MOVEL     TBLUNI        DS3A
020220941019     C   30              CLEAR                   DS3A
020230941019     C*
020240941019     C                   MOVEL     §3ATB1        FLGTB1            1
020250941019     C                   MOVEL     §3ADES        V1DCBO                         DESCRIZIONE
020260071116     c* Verifico se il 1°tipo bolla è tassabile o no
020270071116     C                   MOVEL     'TB'          COD
020280071116     C                   MOVEL(P)  §3atb1        KEY
020290071116     C                   EXSR      CTABEL
020300071116     C  N30              MOVEL     TBLUNI        DSTB
020310071116     C   30              CLEAR                   DSTB
020320071116     c                   movel     §tbfcb        oldtb1FCB         1
020330071116     c
020340070308     c
020350070308     c** Blocco consegna (mi serve per la consegna richiesta)
020360151221     c***10              eval      bollaFBC=arbfbc
020370151221     c                   if        *in10
020380151221     c                   if        wfile='A'
020390151221     c                   eval      bollaFBC=arbfbc
020400151221     c                   else
020410151221     c                   clear                   bollaFBC
020420151221     c                   endif
020430151221     c                   endif
020440070308     c  N10              clear                   bollaFBC
020450941019     C*
020460941019     C* SE FRANCO PROTEGGO P.IVA
020470011017     C     FLGTB1        COMP      'F'                                    05
020480941222     C   05              SETON                                            06
020490941019     C*
020500941019     C* SE ESISTE SECONDA BOLLA PERMETTO L'IMMISSIONE
020510061107     c                   clear                   ksc2bol
020520941212    1C     §3ATB2        IFNE      '  '
020530941019     C   05              SETOFF                                       05
020540941019     C* CHAIN SU 2 BOLLA
020550990930     C                   MOVEL     '2'           WTRC
020560070525     c                   if        flgcvr='2'
020570990930     C     KARB2         CHAIN     FIAR6000                           30
020580070525     c                   else
020590070525     C     KARB2         CHAIN(N)  FIAR6000                           30
020600070525     c                   endif
020610990930     C  N30              MOVEL     'E'           WAR62
020620990930     C  N30              MOVEL     AR6KSC        V1CKSC
020630991007     C  N30              Z-ADD     AR6DFT        DFT2BO
020640941019     C   30              Z-ADD     9999          V1CKSC
020650941019     C   30              MOVEL     ARBLNA        V1CLNA
020660991007     C   30              MOVE      *ZEROS        DFT2BO
020670061107     c                   movel     v1cksc        ksc2bol           7 0
020680040127      * Vedo se esiste anche il Fiar7
020690040127     c     Karb2         Setll     Fiar701l
020700040127     c                   If        %Equal(Fiar701l)
020710040127     c                   Eval      war72 = 'E'
020720040127     c                   EndIf
020730941019     C                   ELSE
020740941019     C                   MOVEL     ARBKSC        V1CKSC
020750941212    1C                   END
020760941019     C**
020770051118     C* SE PREVEDE IL C/A CHAI SU FiAR9
020780941019     C**
020790941212    1C     §3AFCA        IFNE      0
020800041022     c                   if        flgcvr='K'
020810051118     C     KARB          CHAIN     FiAR9000                           30
020820041022     c                   else
020830041022     c* non alloco record se non è variazione di C/A
020840051118     C     KARB          CHAIN(n)  Fiar9000                           30
020850041022     c                   endif
020860041022     c
020870941212    2C     *IN30         IFEQ      *OFF
020880941019     C                   MOVEL     'E'           WAR9              1
020890941019     C                   MOVEL     AR9CAS        V1CCAS
020900941019     C                   MOVEL     AR9TIC        V1CTIC
020910941102     C                   MOVEL     AR9VCA        V1CVCA
020920950918     C                   MOVEL     AR9CAS        SAVCAS
020930950918     C                   MOVEL     AR9TIC        SAVTIC
020940950918     C                   MOVEL     AR9VCA        SAVVCA
020950941212    2C                   ENDIF
020960941212    1C                   ENDIF
020970941019     C**
020980941019     C* VEDO SE CI SONO LE NOTE
020990941019     C**
021000941019     C                   MOVEL     '8'           WTRC
021010060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021020941212    1C     *IN30         IFEQ      *OFF
021030941019     C                   MOVEL     AR4NOT        V1CNT1
021040941019     C*
021050941019     C                   MOVEL     '9'           WTRC
021060060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021070941212    2C     *IN30         IFEQ      *OFF
021080941019     C                   MOVEL     AR4NOT        V1CNT2
021090941212    2C                   ENDIF
021100941212    1C                   ENDIF
021110970409     C* SI NO DDT
021120031028     c                   Clear                   dsbl4a
021130970409     C                   MOVEL     'A'           WTRC
021140151222     c                   if        wfile='A'
021150151222     C     KARB2         CHAIN(N)  FiAR4000                           30
021160151222    2C     *IN30         IFEQ      *OFF
021170031028     C**!!!              MOVEL     AR4NOT        V1CDDT
021180031028     c                   Movel     Ar4not        dsbl4a
021190031028     c                   Move      §b4abm        v1cddt
021200970409     C                   ELSE
021210970409     C                   MOVEL     'S'           V1CDDT
021220970409    2C                   ENDIF
021230151222     c                   else
021240151222     C                   MOVEL     ARBFSTddt     V1CDDT            1
021250151222     c                   endif
021260970409     C*
021270970409     C     V1CDDT        IFEQ      'S'
021280970409     C     V1CDDT        OREQ      'C'
021290031028     c     v1cddt        oreq      'P'
021300031028     c     v1cddt        oreq      'Q'
021310970409     C                   MOVEL     'DDT'         V1DDDT
021320970409     C                   ELSE
021330970409     C                   CLEAR                   V1DDDT
021340970409     C                   ENDIF
021350060222     C* CONTROLLO SE ESISTE RECORD W DI ar4 PER EVENTUALE
021360000623     C*   MODIFICA DI MITTENTE
021370000623     C     *IN10         IFEQ      *OFF
021380000623     C                   MOVEL     'W'           WTRC
021390060222     C     KARB2         CHAIN(N)  Fiar4000                           30
021400000623     C  N30              MOVEL     'S'           WBL4W             1
021410000623     C                   ENDIF
021420941019     C* DESCRIZIONE CAUSALE
021430941019     C                   MOVEL     D66DES        V1DCVR
021440941019     C                   MOVEL     ARBLNP        V1CLNP
021450941212     C                   MOVEL     ARBNRS        V1CNRS
021460941019     C                   MOVEL     ARBNSP        V1CNSP
021470941019     C                   MOVEL     ARBLNA        V1CLNA
021480941019     C                   MOVEL     ARBCBO        V1CCBO
021490941019     C                   MOVEL     ARBRSM        V1CRSM
021500941019     C                   MOVEL     ARBCAM        V1CCAM
021510941019     C                   MOVEL     ARBLOM        V1CLOM
021520941019     C                   MOVEL     ARBPRM        V1CPRM
021530941019     C                   MOVEL     ARBNZM        V1CNZM
021540970828     C                   MOVEL     ARBFAP        V1CFAP
021550941019     C                   MOVEL     ARBRSD        V1CRSD
021560941019     C                   MOVEL     ARBIND        V1CIND
021570941019     C                   MOVEL     ARBCAD        V1CCAD
021580941019     C                   MOVEL     ARBLOD        V1CLOD
021590941019     C                   MOVEL     ARBPRD        V1CPRD
021600941019     C                   MOVEL     ARBNZD        V1CNZD
021610970828     C                   MOVEL     ARBFIN        V1CFIN
021620941019     C                   MOVEL     ARBCTS        V1CCTS
021630941019     C                   MOVEL     ARBTC1        V1CTC1
021640941019     C                   MOVEL     ARBTC2        V1CTC2
021650941027     C                   MOVEL     ARBPKF        V1CPKF
021660941019     C                   MOVEL     ARBVLF        V1CVLF
021670941019     C                   MOVEL     ARBFVF        V1CFVF
021680030729     c                   Z-Add     ArbNcl        V1cNcl
021690141029     c                   clear                   v1dffd
021700141029     c                   if        arbffd='S'
021710141029     c                   eval      v1dffd='FermoDeposito'
021720141029     c                   endif
021730100121     c* Partita iva MITTENTE se bolla singola franco
021740100121     c                   select
021750100121     c                   when      flgtb1='F' and §3atb2=*blanks
021760100121     c                   eval      v1cpim=arbcpi
021770100121     c                   when      flgtb1='A' and §3atb2=*blanks
021780100121     c                   eval      v1cpid=arbcpi
021790100121     c* In arrivo   per bolla  doppia --> partita iva dell'assegnato
021800151222     c                   when      wfile='A'   and §3atb2<>*blanks
021810100121     c                   eval      v1cpid=arbcpi
021820100121     c* In partenza per bolla  doppia --> partita iva del mittente
021830151222     c                   when      wfile='P'   and §3atb2<>*blanks
021840100121     c                   eval      v1cpid=arbcpi
021850100121     c                   ENDSL
021860100121
021870950114     C* SALVO IL CODICE CLIENTE DELLA SPEDIZIONE
021880950114     C                   MOVEL     ARBKSC        SAVKSC            7 0
021890060511     C                   MOVEL     ARBKSC        kscbolla          7 0
021900941102     C* PRENDO LA VALUTA DAL DESTINATARIO E MITTENTE
021910941102     C                   MOVEL     '15'          COD
021920941102     C                   MOVEL(P)  ARBNZD        KEY
021930941212     C                   EXSR      CTABEL
021940941102     C  N30              MOVEL     TBLUNI        DS15
021950941102     C   30              CLEAR                   DS15
021960941102     C                   MOVEL     §15VLT        DESVLT            3
021970941212    1C     §15ITA        IFEQ      'I'
021980941102     C                   MOVEL     'I'           WDESIE            1
021990941102     C                   ELSE
022000941102     C                   MOVEL     'E'           WDESIE            1
022010941212    1C                   ENDIF
022020020305     c                   MOVEL     §15EFT        DESEFTW
022030020305     c                   MOVEL     §15EFD        DESEFDW
022040020305     c                   MOVEL     §15EFF        DESEFFW
022050971230     V* CHAIN SU AZORG CON LINEA PARTENZA
022060971230     C     ARBLNP        CHAIN     AZORG01L                           32
022070941209     C*
022080000223     C                   MOVEL     ORGDE3        OG143
022090020305     C                   MOVEL     §OGNTW        LNPNTW
022100941102     C*
022110941102     C                   MOVEL(P)  ARBNZM        KEY
022120941212     C                   EXSR      CTABEL
022130941102     C  N30              MOVEL     TBLUNI        DS15
022140941102     C   30              CLEAR                   DS15
022150941102     C                   MOVEL     §15VLT        MITVLT            3
022160941212    1C     §15ITA        IFEQ      'I'
022170941102     C                   MOVEL     'I'           WMITIE            1
022180941102     C                   ELSE
022190941102     C                   MOVEL     'E'           WMITIE            1
022200941212    1C                   ENDIF
022210001204     C**
022220051129     C* TERMINAL DI PARTENZA DELLA LNP: VEDO SE HO VDL
022230040123     c                   Clear                   og150
022240970901     C     ARBTFP        CHAIN     AZORG                              32
022250051129     C                   CLEAR                   WVDL
022260970901    1C     *IN32         IFEQ      *OFF
022270051129     C**!!!              TESTN                   DATVDL               31
022280040123     c                   Movel     Orgdf0        og150
022290120613     c                   testn                   §ogvol               31
022300970901     C*
022310040123    2C**!!!*IN31         IFEQ      '1'
022320051129     C**!!!DATVDL        ANDGT     *ZEROS
022330120613     c***!!!             If        §ogspv = 'S'
022340120613     c                   if        *in31 and §ogvol>*zeros
022350051129     C                   MOVEL     'S'           WVDL              1
022360970901    2C                   END
022370970901    1C                   END
022380941019     C**
022390941019     C* TIPO SERVIZIO
022400941019     C**
022410090604     c                   clear                   ds5e
022420941014     C                   MOVEL     '5E'          COD
022430941014     C                   MOVEL(P)  ARBTSP        KEY
022440941212     C                   EXSR      CTABEL
022450090604     C  N30              MOVEL     TBLUNI        ds5e
022460090604     C  N30              MOVEL     §5ed08        V1DTSP
022470941220     C   30              CLEAR                   V1DTSP
022480941019     C**
022490910429     C* DATA SPEDIZIONE
022500941019     C**
022510941014     C                   MOVEL     '3'           G02ERR
022520941014     C                   Z-ADD     ARBMGS        G02INV
022530941014     C                   MOVEL     ARBAAS        G02INV
022540941014     C                   CALL      'XSRDA8'
022550941014     C                   PARM                    WLBDAT
022560941014     C*
022570941212     C                   Z-ADD     G02DAT        V1CDSP
022580941019     C**
022590930505     C* LINEA DI ARRIVO
022600941019     C**
022610070306     C     ARBLNA        CHAIN     AZORG                              30
022620941019     C  N30              MOVEL     ORGDES        V1DLNA                          L.ARRIVO
022630941019     C   30              MOVEL     *BLANKS       V1DLNA
022640000202     C                   MOVEL     ORGDE3        OG143
022650020305     C                   MOVEL     §OGNTW        LNANTW
022660020305     c                   select
022670020305     c     lnantw        wheneq    'DPD'
022680020305     c                   movel     desefdw       deseft
022690020305     c     lnantw        wheneq    'FED'
022700020305     c                   movel     deseffw       deseft
022710020305     c                   other
022720020305     c                   movel     deseftw       deseft
022730020305     c                   endsl
022740070906     c*****
022750070906     c* Verifico se destinatario disagiato
022760070906     c*****
022770070906     c                   clear                   wdisagdest
022780070906     c                   clear                   tisit0ds
022790070906     c                   movel     'E'           it0tla
022800070906     c                   movel     arbnzd        it0naz
022810070906     c                   movel     arbprd        it0prv
022820070906     c                   movel     arbcad        it0cap
022830070906     c                   movel     arblod        it0loc
022840070906     c                   movel     arbind        it0ind
022850070906     c                   movel     arbrsd        it0rag
022860070906     c                   call      'TISIT5R'
022870070906     c                   parm                    tisit0ds
022880070906    1c     ot0err        ifeq      *blanks
022890070906    2c                   if        ot0dos = 'A' or ot0dos = 'D' or
022900070906     c                             ot0dos = 'S'
022910070906     c                   movel     ot0dos        wdisagdest
022920070907    3c     wdisagdest    ifeq      'D'
022930070906     c                   movel     'X'           wdisagdest
022940070906    3c                   endif
022950070906    2c                   endif
022960070906   x1c                   else
022970070906     c                   clear                   wdisagdest
022980070906    1c                   endif
022990070306     c
023000070910     c****
023010070910      * Reperisco record GEN nel fiar5
023020070910     c                   Clear                   dAr5Genpar
023030070910     c                   Eval      kAr5Trd = 'GEN'
023040070910     c     kAr5          Chain(n)  Fiar501l
023050070910     c                   If        %Found(Fiar501l)
023060070910     c                   Movel     Ar5Uni        dAr5Genpar
023070070910     c                   eval      wesar5gen='S'
023080070910     c                   endif
023090070319     c****
023100070319     c* FILIALE GESTIONE: VEDO SE GEO ATTIVA
023110070319     c****
023120070319     c                   clear                   FGSgeot
023130070319     c                   clear                   FGSddaSI
023140110906     c****               if        *in10
023150070319     c                   clear                   fnlv55ds
023160070319     C                   MOVEL     '6'           D55TPT
023170070319     C                   MOVEL     ARBLNA        D55LIN
023180070319     C                   MOVEL     DATEU         D55DRF
023190070319     C                   CALL      'FNLV55R'
023200070319     C                   PARM                    Fnlv55DS
023210110906     c                   eval      lnacons=d55tfa
023220070319     c
023230070319     c     d55tfa        CHAIN     AZORG                              30
023240070319     c                   if        not *in30
023250070319     C                   MOVEL     ORGDE8        og148                           L.ARRIVO
023260070319     C                   MOVEL     ORGDE6        og146                           L.ARRIVO
023270070319     C
023280070319     c                   movel     §oggeot       FGSgeot           1
023290070319     C
023300070319    1c                   if        §ogdda>*zeros
023310070319     c                   testn                   §ogdda               30
023320070319     c                   move      §ogdda        w001a             1
023330070319    2c                   if        *in30 and w001a>='0'
023340070319     c* Se la data = 20391231, come se non ci fosse
023350070319    3c                   if        §ogdda<>'20391231'
023360070319     c                   movel     §ogdda        w0080             8 0
023370070426     c* Se <=oggi, attiva procedura distinte automatiche
023380070426    4c                   if        w0080<=dateu
023390070319     c                   movel     'S'           FGSddaSI          1
023400070319    4c                   endif
023410070319    3c                   endif
023420070319    2c                   endif
023430070319    1c                   endif
023440070319     c
023450070319     c                   endif
023460110906     c****               endif
023470151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
023480151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
023490140213     c                   seton                                        77
023500140213     c                   endif
023510070329     c* Se p.o. utente o lna hanno GEO attiva, propongo F17 note
023520140130     c* non le propongo se bolla in consegna variazione da inviare a AUT e
023530140130     c* filiale partita con la nuova gestione telefonate AUT
023540151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
023550151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
023560140130     c                             and (
023570140130     c* non è in consegna
023580140130     c                             (arbndc=0 or arbdcm>0)
023590140130     c* filiale distinta non partita con nuova gestione telefonate AUT
023600140130     c                             or (%lookup(%subst
023610140130     c                                 (%editc(arbifp:'X'):7:3):skFil816tel)=0
023620140130     c                              and skfil816tel(1)<>'999')
023630140130     c* Causale variazione da non trasmettere a AUT
023640140130     c                             or d66pda=*blanks
023650140130     c                             )
023660070329     c                   seton                                        87
023670070329     c                   endif
023680941019     C**
023690941019     C* CONSEGNE PARTICOLARI
023700941019     C**
023710941019     C                   MOVEL     *BLANKS       V1DTC1
023720941019     C                   MOVEL     *BLANKS       V1DTC2
023730070914     C                   MOVEL     *BLANKS       V7DTC1
023740070914     C                   MOVEL     *BLANKS       V7DTC2
023750941212    1C     ARBTC1        IFNE      ' '
023760930505     C                   MOVEL     '1P'          COD
023770941019     C                   MOVEL(P)  ARBTC1        KEY
023780941212     C                   EXSR      CTABEL
023790941102     C  N30              MOVEL     TBLUNI        DS1P
023800941102     C   30              CLEAR                   DS1P
023810941102     C                   MOVEL     §1PDES        V1DTC1
023820941220     C                   MOVEL     §1PDES        V7DTC1
023830941102     C*
023840941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
023850941222    2C   10§1PUPR        IFEQ      'S'
023860941213     C     §1PUAR        ANDNE     'S'
023870941102     C                   SETON                                        11
023880941212    2C                   ENDIF
023890941222     C* O VICEVERSA
023900941222    2C  N10§1PUAR        IFEQ      'S'
023910941222     C     §1PUPR        ANDNE     'S'
023920941222     C                   SETON                                        11
023930941222    2C                   ENDIF
023940941102     C*
023950941212    1C                   ENDIF
023960941019     C*
023970941212    1C     ARBTC2        IFNE      ' '
023980941019     C                   MOVEL     '1P'          COD
023990941213     C                   MOVEL(P)  ARBTC2        KEY
024000941212     C                   EXSR      CTABEL
024010941102     C  N30              MOVEL     TBLUNI        DS1P
024020941102     C   30              CLEAR                   DS1P
024030941102     C                   MOVEL     §1PDES        V1DTC2
024040941220     C                   MOVEL     §1PDES        V7DTC2
024050941102     C*
024060941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024070941222    2C   10§1PUPR        IFEQ      'S'
024080941213     C     §1PUAR        ANDNE     'S'
024090941102     C                   SETON                                        12
024100941212    2C                   ENDIF
024110941222     C* O VICEVERSA
024120941222    2C  N10§1PUAR        IFEQ      'S'
024130941222     C     §1PUPR        ANDNE     'S'
024140941222     C                   SETON                                        12
024150941222    2C                   ENDIF
024160941222     C*
024170941212    1C                   ENDIF
024180941019     C**
024190941019     C* SPEDIZIONE GIACENTE
024200941019     C**
024210040316     c                   Clear                   wgcfas
024220040324     c                   Clear                   wgcded
024230040324     c                   Clear                   wgcddm
024240051109     c                   Clear                   dgcpflr
024250141113if  1C**   *IN10         IFEQ      *ON
024260141113     c**   d48cvb        oreq      'CS'
024270141113     c**   d48cvb        oreq      'CR'
024280050331     C     KARB          CHAIN     Tigcp21L                           30        GIACENZA
024290941212    1C     *IN30         IFEQ      *OFF
024300050331     C     GCPFAS        ANDLT     40
024310941019     C     GCPATB        ANDEQ     ' '
024320040316     c                   Eval      wgcfas = GcpFas
024330040324     c                   Eval      wgcded = GcpDed
024340040324     c                   Eval      wgcddm = GcpDdm
024350051109     c                   Movel     gcpflr        dgcpflr
024360941019     C                   SETON                                        04
024370941212    1C                   ENDIF
024380040219      * se non ho trovato il rcd di giacenza in arrivo provo in partenza
024390040219      * solo se sto facendo una variazione 'CS' o 'CR'
024400051109      * questo solo per accendere l'indicatore 04 di giacenza aperta, non serve per la
024410051109      * variazione della data consegna richiesta, questa è possibile solo se la giacenza
024420051109      * è aperta in arrivo!!!!
024430141113      * Ora lo 04 mi serve anche per il controllo dell'abilitazione dei dirottamenti sia
024440141113      * in partenza che in arrivo e quinidi lo faccio sempre
024450141113if  2c***                If        *In30 and
024460141113     c***                          (d48cvb = 'CS' or d48cvb = 'CR')
024470141113if  2c                   If        *In30
024480050331     c     kArb          Chain     Tigcp01l
024490050331if  3c                   If        %Found(Tigcp01l) and GcpFas < 50 and
024500040219     c                             GcpAtb = *Blanks
024510040219     c                   Seton                                        04
024520040316     c                   Clear                   wgcfas
024530040324     c                   Clear                   wgcded
024540040324     c                   Clear                   wgcddm
024550040219    3c                   EndIf
024560040219    2c                   EndIf
024570141113    1C*****              ENDIF
024580051129     C* DETERMINO SE IL VOLUME VDL (CHE EVENTUALMENTE SOSTITUIRA' IL
024590970418     C* VOLUME DA FATTURARE) E' PARZIALE O TOTALE
024600970418     C                   CLEAR                   WFVF
024610050111     C     arbncl        IFGT      ARBNCR
024620970418     C                   MOVEL     'Z'           WFVF                           VOLUME PARZ.
024630970418     C                   ELSE
024640970418     C                   MOVEL     'T'           WFVF                           VOLUME TOTAL
024650970418     C                   ENDIF
024660051129      * Determino se il peso VDL è parziale o totale
024670030122     c                   Clear                   VidTpk
024680030123     c                   Clear                   wfpf
024690030122     c                   If        ArbNcp > *Zeros
024700050111     c                   If        arbncl > ArbNcp
024710030122     c                   Eval      VidTpk = 'Par.'                              Peso Parziale
024720030123     c                   Eval      wfpf = 'Z'
024730030122     c                   Else
024740030122     c                   Eval      VidTpk = 'Tot.'                              Peso Totale
024750030123     c                   Eval      wfpf = 'T'
024760030122     c                   EndIf
024770030122     c                   EndIf
024780030729      * Recupero i Bancali
024790030729     c                   If        %Subst(ArbGva:1:1) = 'B'
024800030729     c                   Clear                   dAr5Ban
024810030729     c                   Eval      kAr5Trd = 'BAN'
024820030729     c     kAr5          Chain(N)  Fiar501l
024830030729     c                   If        %Found(Fiar501l)
024840030729     c                   Movel     Ar5Uni        dAr5Ban
024850030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
024860030729     c                   EndIf
024870030729     c                   EndIf
024880030729      * Recupero i Bancali e i colli originali
024890030729     c                   If        %Subst(ArbGva:1:1) = 'O'
024900030729     c                   Clear                   dAr5Bnb
024910030729     c                   Eval      kAr5Trd = 'BNB'
024920030729     c     kAr5          Chain(N)  Fiar501l
024930030729     c                   If        %Found(Fiar501l)
024940030729     c                   Movel     Ar5Uni        dAr5Bnb
024950030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
024960030729     c                   Z-Add     §Ar5bCor      V1cCor
024970030729     c                   Eval      *In70 = *On
024980030729     c                   EndIf
024990030729     c                   EndIf
025000130924      * Memorizzo se presente avviso al destinatario per passarlo alla tassazione
025010110629     c                   clear                   wemd
025020130924     c                   Clear                   dAr5emd
025030110629     c                   Eval      kAr5Trd = 'EMD'
025040130924     c     kAr5          chain(N)  Fiar501l
025050130924     c                   if        %found(fiar501l)
025060130924     c                   Movel     Ar5Uni        dAr5EMD
025070130930     c                   eval      wesar5EMD='S'
025080130924     c                   select
025090130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp=' '
025100110629     c                   eval      wemd='S'
025110130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp='S'
025120130924     c                   eval      wemd='E'
025130130924     c                   when      emd_§ar5smp='S'
025140130924     c                   eval      wemd='X'
025150130924     c                   endsl
025160110629     c                   endif
025170061107     C**
025180071022     C* F N A R B D 0 F  record del codice fiscale e partita IVA
025190061107     C**
025200061107    1C     VIDCVR        IFEQ      'FI'
025210071022     c*
025220061129     c* Memorizzo se bolla già trasmessa o meno
025230070528     c*  solo in partenza(in arrivo è per forza trasmessa
025240151222     c  n10              movel     arbft2        bollaft2          1
025250151222     c**                 if        wfile='P'
025260151222     c**                 movel     arbft2        bollaft2          1
025270151222     c***                endif
025280071022     c* Dati per partita IVA
025290071022     c
025300071025     c                   clear                   ar4not_P
025310071022     C                   MOVEL     'P'           WTRC
025320061107     C     KARB2         CHAIN(N)  FiAR4000                           30
025330071025     c  n30              movel     ar4not        ar4not_p
025340071022     C                   MOVEL     'Q'           WTRC
025350071022     C     KARB2         CHAIN(N)  FiAR4000                           30
025360061107     c
025370061107     c* Verifico se bolla singola .Se doppia guardo se sono in partenza
025380061107     c*  o arrivo
025390061107     c* carico il mittente se bolla franco in partenza sempre
025400061107     c*  o franco singola in arrivo
025410061107    2c                   if        (flgtb1='F'  and not *in10) or
025420061107     c                             (flgtb1='F'  and §3atb2=*blanks)
025430061107     c                   eval      v11mide='M'
025440061107     c* Mittente
025450061107     c                   eval      v11decmd=CMITT
025460061107     c                   eval      v11cod=arbksc
025470061107     c                   eval      v11rsc=arbrsm
025480061107     c                   eval      wnar=arbnzm
025490071025     c* codice fiscale
025500061107     c                   if        not *in30 and not *in20
025510061107     c                   movel     ar4not        v11cdfis
025520061107     c                   else
025530061107     c                   clear                   v11cdfis
025540061107     c                   endif
025550071025     c* Partita IVA
025560071107     c****               if        not *in20
025570071025     c                   if        %subst(ar4not_P:1:16)<>*blanks
025580071025     c                   movel     ar4not_P      v11pid
025590071025     c                   else
025600071025     c                   movel     arbcpi        v11pid
025610071025     c                   endif
025620071107     c****               endif
025630071025     c
025640061107     c* In arrivo di fatto non posso inserire cod.fiscale su un franco
025650061107     c*    singolo: proteggo il campo
025660061107    3c                   if        *in10 and §3atb2=*blanks
025670061107     c                   seton                                        76
025680061107    3c                   endif
025690061107    2c                   endif
025700061107     c
025710061107     c* Carico il destinatario in arrivo sempre, in partenza se singola
025720061107    2c                   if        (flgtb1='A') or
025730061107     c                             (flgtb1='F'  and §3atb2<>*blanks and
025740061107     c                             *in10)
025750061107     c                   eval      v11mide='D'
025760061107     c                   eval      v11decmd=Cdest
025770061107     c                   eval      v11rsc=arbrsd
025780061107     c                   eval      wnar=arbnzd
025790061107     c                   if        not *in30 and not *in20
025800061107     c                   move      ar4not        v11cdfis
025810061107     c                   else
025820061107     c                   clear                   v11cdfis
025830061107     c                   endif
025840071025     c* Partita IVA
025850071107     c*****              if        not *in20
025860071025     c                   if        %subst(ar4not_P:20:16)<>*blanks
025870071026     c                   move      ar4not_P      v11pid
025880071025     c                   else
025890071107     c                   movel     arbcpi        v11pid
025900071025     c                   endif
025910071107     c*****              endif
025920071025     c* In partenza proteggo sempre
025930061107     c  n10              seton                                        76
025940061107     c* codice destinatario: dal fiar6 prima o seconda bolla
025950061107    3c                   if        §3atb2<>*blanks
025960061107     c                   eval      v11cod=ksc2bol
025970061107   x3c                   else
025980061107     C                   MOVEL     '1'           WTRC
025990061107     C     KARB2         CHAIN(N)  FIAR6000                           30
026000061107     c   30              eval      v11cod=arbksc
026010061107     c  n30              eval      v11cod=ar6ksc
026020061107    3c                   endif
026030061107    2c                   endif
026040061107     C*
026050061107     C* PGM RICHIAMATO
026060061107     c                   if        *in20
026070071025     c* in CPI c'e' il codice fiscale
026080061107     c                   movel     §bdcpi        v11cdfis
026090071107     c* in RSD c'e' la partita IVA solo dalla data attivazione
026100071203     c***                if        dateu>=§vpopiv
026110071025     c                   movel     §bdrsd        v11PID
026120071203     c***                endif
026130071107     c                   endif
026140071025     c
026150071025     c* controllo Partita IVA se privato
026160071025    2c                   if        v11pid<>*blanks
026170071025     C     'PRIVATO'     SCAN      v11pid        posiz                    30
026180071025     c* solo numeri
026190071025     c* "PRIVATO" senza iso
026200071025    3c     *in30         ifeq      *off
026210071025     c     v11isd        andge     *zeros
026220071025     c     *in30         oreq      *on
026230071025     c     posiz         andlt     3
026240071025     c                   movel     v11pid        w016a            16
026250071025     c                   movel     w016a         v11cpd
026260071025     C                   CLEAR                   V11isd
026270071025    3c                   endif
026280071025    2c                   endif
026290061107     c*
026300061107    1c                   endif
026310941019     C**
026320941027     C* F N A R B D 0 F
026330941019     C**
026340941019    1C     FLGCVR        IFEQ      'I'
026350941019     C*
026360941019    2C     VIDCVR        IFEQ      'I0'
026370140407    2C     VIDCVR        oreQ      'I7'
026380140407    2C     VIDCVR        oreQ      'I8'
026390140407    2C     VIDCVR        oreQ      'I9'
026400941019     C                   SETON                                        01          DESTINATAR
026410941019     C                   ELSE
026420941019     C                   SETON                                        02          PESO VOL
026430941212    2C                   END
026440941025     C*
026450941025     C* 2 RAGIONE SOCIALE DESTINATARIO
026460941212     C                   MOVEL     'D'           WTRC
026470060222     C     KARB2         CHAIN(N)  Fiar4000                           30
026480941212     C     *IN30         IFEQ      *OFF
026490941212     C                   MOVEL     'E'           WAR4D             1
026500941212     C* LO SALVO PER LA STORICIZZAZIONE
026510970520     C                   MOVE      AR4NOT        SAVRD2
026520941212     C                   ENDIF
026530061116     C*
026540061116     C* codice fiscale destinatario
026550061116     C                   MOVEL     'Q'           WTRC
026560061116     C     KARB2         CHAIN(N)  Fiar4000                           30
026570061116     C     *IN30         IFEQ      *OFF
026580061116     C                   MOVEL     'E'           WAR4Q
026590061116     C* Se c'e' il cod fiscale del detinatario lo memorizzo
026600061116     C                   MOVE      AR4NOT        vidcdf
026610061116     c                   if        vidcdf<>*blanks
026620061116     C                   MOVEL     'D'           WAR4Q
026630061116     C                   ENDIF
026640061116     C                   ENDIF
026650150206
026660150206     C* LOCALITA' NORMALIZZATA
026670150206     C                   MOVEL     'X'           WTRC
026680150206     C     KARB2         CHAIN(N)  Fiar4000                           30
026690150206     C     *IN30         IFEQ      *OFF
026700150206     C                   MOVEL     'E'           WAR4X             1
026710150206     C                   ENDIF
026720061116     c
026730051129     C* VISUALIZZO ANCHE IL VOLUME VDL
026740941214     C                   MOVEL     ARBVLC        V1CVLC
026750941214     C                   MOVEL     ARBNCR        V1CNCR
026760031120
026770040123      * Reperisco il referente e il telefono e magazzino giacenza
026780070910     c                   If        wesar5gen='S'
026790070910     c                   Eval      VidRef =  PAr5Ref
026800070910     c                   Eval      VidTeld = PAr5Teld
026810070910     c                   Eval      VidZng =  PAr5Zng
026820031120     c                   EndIf
026830141203     c                   eval      savzng = vidzng
026840941019     C*
026850941019     C* PGM NON RICHIAMATO
026860941019    2C     *IN20         IFEQ      *OFF
026870970520     C                   CLEAR                   VIDRSD
026880941025     C                   MOVEL     ARBRSD        VIDRSD
026890941027     C     WAR4D         IFEQ      'E'
026900061116     C                   MOVE      savrd2        VIDRSD
026910941025     C                   ENDIF
026920941019     C                   MOVEL     ARBIND        VIDIND
026930941019     C                   MOVEL     ARBLOD        VIDLOD
026940941019     C                   MOVEL     ARBPRD        VIDPRD
026950941019     C                   MOVEL     ARBCAD        VIDCAD
026960061115     c
026970140428     c*05 off se --> bolla singola in assegnato
026980140428     c*              bolla doppia
026990071025    3C     *IN05         IFEQ      *OFF
027000140428     c* se bolla doppia in partenza la partita iva che ho caricato non è quella del
027010140428     c* destinatario e quindi la carico solo se sono in arrivo (10 on)
027020151222     c***  *in10         ifeq      *on
027030151222     c     wfile         ifeq      'A'
027040140428     c     §3atb2        oreq      *blanks
027050941220     C                   MOVEL     ARBCPI        VIDPID
027060140428     c                   endif
027070071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027080050922     c* solo numeri
027090050922     c* "PRIVATO" senza iso
027100071025    4c**   *in30         ifeq      *off
027110071025     c**   vidisd        andge     *zeros
027120071025     c**   *in30         oreq      *on
027130071025     c**   posiz         andlt     3
027140071025     C**                 CLEAR                   VIDPID
027150071025     c**                 movel     arbcpi        vidcpd
027160071025    4c**                 endif
027170071025    3C                   ENDIF
027180941220     C*
027190030122     c                   Z-Add     ArbPkc        VidPkc
027200941027     C                   MOVEL     ARBPKF        VIDPKF
027210941027     C                   MOVEL     ARBVLF        VIDVLF
027220941027     C                   MOVEL     ARBFVF        VIDFVF
027230941019     C                   MOVEL     ARBFIN        VIDFIN
027240941019     C                   MOVEL     ARBNZD        VIDNZD
027250941019     C* FERMO DEPOSITO
027260941019    3C     ARBFFD        IFEQ      'S'
027270941019     C                   MOVEL     'SI'          VIDFFD
027280941019     C                   ELSE
027290941019     C                   CLEAR                   VIDFFD
027300941019    3C                   ENDIF
027310941019     C**
027320941019   X2C                   ELSE
027330941019     C**
027340941019     C* PROGRAMMA     RICHIAMATO: PRENDO CAMPI DA DSARBD
027350941019     C                   MOVEL     §BDRSD        VIDRSD
027360970520     C                   MOVE      §BDRD2        VIDRSD
027370941019     C                   MOVEL     §BDIND        VIDIND
027380941019     C                   MOVEL     §BDLOD        VIDLOD
027390941019     C                   MOVEL     §BDPRD        VIDPRD
027400941019     C                   MOVEL     §BDCAD        VIDCAD
027410941019     C                   MOVEL     §BDNZD        VIDNZD
027420060308     c                   movel     §bdref        vidref
027430060308     c                   movel     §bdtel        vidteld
027440061115     c                   clear                   vidcdf
027450071025    3C     *IN05         IFEQ      *OFF
027460950102     C                   MOVEL     §BDCPI        VIDPID
027470071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027480050922     c* solo numeri
027490050922     c* "PRIVATO" senza iso
027500071025    4c**   *in30         ifeq      *off
027510071025     c**   vidisd        andge     *zeros
027520071025     c**   *in30         oreq      *on
027530071025     c**   posiz         andlt     3
027540071025     C**                 CLEAR                   VIDPID
027550071025     c**                 movel     §bdcpi        vidcpd
027560071025    4c**                 endif
027570071025    3C                   ENDIF
027580941019     C                   MOVEL     §BDFIN        VIDFIN
027590941027     C                   MOVEL     §BDPKF        VIDPKF
027600941027     C                   MOVEL     §BDVLF        VIDVLF
027610941209     C                   MOVEL     §BDFVF        VIDFVF
027620941019     C* FERMO DEPOSITO
027630141017     c* §BDFFD= 'S' => si fermo deposito; Si alert
027640141017     c* §BDFFD= 'Y' -->si fermo deposito; no alert
027650141017     c* Serve per le chiamate cieche in quanto il campo a video per la gestione
027660141017     c* del contatto (VIDTFD) non è presente nella dsarbd.
027670941019    3C     §BDFFD        IFEQ      'S'
027680141017    3C     §BDFFD        oreq      'Y'
027690941019     C                   MOVEL     'SI'          VIDFFD
027700941019     C                   ELSE
027710941019     C                   CLEAR                   VIDFFD
027720941019    3C                   ENDIF
027730941216    2C                   ENDIF
027740141017      *
027750141017     c                   if        §bdffd='Y'
027760141017     c                   eval      vidtfd='SI'
027770141017     c                   endif
027780071025     c
027790071025     c* controllo Partita IVA se privato
027800071025    2c                   if        vidpid<>*blanks
027810071025     C     'PRIVATO'     SCAN      vidpid        posiz                    30
027820071025     c* solo numeri
027830071025     c* "PRIVATO" senza iso
027840071025    3c     *in30         ifeq      *off
027850071025     c     vidisd        andge     *zeros
027860071025     c     *in30         oreq      *on
027870071025     c     posiz         andlt     3
027880071025     c                   movel     vidpid        w016a
027890071025     c                   movel     w016a         vidcpd
027900071025     C                   CLEAR                   VIDisd
027910071025    3c                   endif
027920071025    2c                   endif
027930991001     C*
027940991001    2C     *IN01         IFEQ      *ON
027950970808     C* MEMORIZZO CAP/PROVINCIA E LOCALITA' DEL DESTINATARIO
027960970808     C* (MI SERVIRA' NELLA ROUTINE DEI CONTROLLI
027970970808     C*
027980970808     C                   MOVE      VIDCAD        SAVCAD                         CAP
027990970808     C                   MOVE      VIDCAD        VARCAD                          "
028000970808     C                   MOVE      VIDCAD        WCAD                            "
028010970808     C                   MOVE      VIDCAD        WCAD1                           "
028020970808     C                   MOVE      VIDLOD        SAVLOD                         LOCALITA'
028030970808     C                   MOVE      VIDLOD        VARLOD                            "
028040970808     C                   MOVE      VIDLOD        WLOD                              "
028050970808     C                   MOVE      VIDLOD        WLOD1                             "
028060060111     c* memorizzo anche gli altri dati del destinatario
028070060111     c                   move      vidrsd        savrsd
028080061116     c                   move      vidrsd        savrsdQ
028090060111     c                   move      vidrsd        wrsd1
028100060111     c                   move      vidind        savind
028110060111     c                   move      vidind        wind1
028120060111     c                   move      vidprd        savprd
028130060111     c                   move      vidprd        wprd1
028140060111     c                   move      vidnzd        savnzd
028150060111     c                   move      vidnzd        wnzd1
028160970808     C*
028170950419     C* SE P.IVA DEL VIDEO = BLANKS LA PRENDO DALL'ESTENSIONE SE
028180950419     C* ESISTE
028190950419     C     VIDPID        IFEQ      *BLANKS
028200950419     C                   MOVEL     'P'           WTRC
028210060222     C     KARB2         CHAIN(N)  Fiar401L                           30
028220950419     C     *IN30         IFEQ      *OFF
028230950419     C                   MOVE      AR4NOT        VIDPID
028240050922     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028250050922     c* solo numeri
028260050922     c* "PRIVATO" senza iso
028270050922     c     *in30         ifeq      *off
028280050922     c     vidisd        andge     *zeros
028290050922     c     *in30         oreq      *on
028300050922     c     posiz         andlt     3
028310050922     C                   CLEAR                   VIDPID
028320050922     c                   move      ar4not        wpiva
028330050922     C                   MOVEL     WPIVA         VIDCPD
028340050922     c                   endif
028350050922     C**   VIDISD        IFGE      *ZEROS
028360050922     C**                 CLEAR                   VIDPID
028370050922     C**                 MOVE      AR4NOT        WPIVA
028380050922     C**                 MOVEL     WPIVA         VIDCPD
028390050922     C**                 ENDIF
028400950419     C                   END
028410950419     C                   END
028420950419     C*
028430160118     C                   SETOFF                                       717276
028440991001     C* SE ASSEGNATO 9999 GIA' CONTABILIZZATO PROTEGGO DATI
028450991001     C* DESTINATARIO (ANCHE LA P.IVA MA SOLO SE DIVERSA DA BLANKS)
028460991001     C                   MOVE      ARBKSC        CO1KSC            4 0
028470991001    3C     CO1KSC        IFEQ      9999
028480991001     C                   CLEAR                   AR6DFT
028490991001     C                   MOVEL     '1'           WTRC
028500991001     C     KARB2         CHAIN(N)  FIAR6000                           30
028510991001    4C     AR6DFT        IFNE      *ZEROS
028520000104     C     AR6DFT        ANDLE     WDTU10
028530991001     C                   SETON                                        71
028540000104    5C     VIDPID        IFNE      *BLANKS
028550000104     C                   SETON                                        72
028560991001    5C                   END
028570991001    4C                   END
028580991001    3C                   END
028590160118     c* Proteggo la partita iva/cod.fiscale anche se dagli arrivi sto manutenzionando BLP
028600160115     c                   if        *in10 and wfile='P'
028610160118     C                   SETON                                        7276
028620160115     c                   endif
028630991001    2C                   END
028640941019     C*
028650941222     C* PROTEGGO VOLUME SE NON VARIABILE
028660941216     C* SE SI TRATTA DI VARIAZIONE DI PESO, PROTEGGO SEMPRE IL VOLUME
028670941216    2C     D48CVB        IFEQ      'I2'
028680941216     C* PROTEGGO IL PESO
028690941216     C                   SETON                                        88
028700941216     C*
028710941019     C                   MOVEL     '7T'          COD
028720051214     C                   MOVEL(P)  VIDFVF        KEY
028730941212     C                   EXSR      CTABEL
028740941212     C   30              SETON                                        89
028750941222     C     *IN30         IFEQ      *OFF
028760941222     C                   MOVEL     TBLUNI        DS7T
028770091106     c
028780151222     c                   if        *in10
028790091106     c                   eval      wvariavol=§7tvra
028800120705     c                   eval      wvariavolEXP=§7tvraEXP
028810091106     c                   else
028820091106     c                   eval      wvariavol=§7tvrp
028830120705     c                   eval      wvariavolEXP=§7tvrPEXP
028840091106     c                   endif
028850091106     c
028860091106     c* Proteggo il volume in arrivo o in partenza se il flag è ' ' oppure
028870091124     c*  non corrisponde il tipo bolla
028880091106     c                   select
028890091106     c                   when      wvariavol=' '
028900091106     C                   SETON                                        89
028910091106     c                   when      Wvariavol='A' and flgtb1='F'
028920091106     C                   SETON                                        89
028930091106     c                   when      Wvariavol='F' and flgtb1='A'
028940091106     C                   SETON                                        89
028950091106     c                   endsl
028960091106     c
028970120705     C* Se  BOLLA EXPORT,  in aggiunta controllo il relativo flag
028980120705     c                   if        *in89 and (lnantw='FED' or
028990120705     c                                        lnantw='EEX' or lnantw='DPD')
029000120705     c                   select
029010120705     c                   when      WvariavolEXP='3'
029020120705     C                   SETOff                                       89
029030120705     c                   when      WvariavolEXP='A' and flgtb1='A'
029040120705     C                   SETOff                                       89
029050120705     c                   when      Wvariavolexp='F' and flgtb1='F'
029060120705     C                   SETOff                                       89
029070120705     c                   endsl
029080120705     c                   endif
029090091106     c
029100941222    3C                   ENDIF
029110941216     C*
029120941216   X2C                   ELSE
029130941216     C                   SETON                                        89
029140941216    2C                   ENDIF
029150030604
029160030604      * se linea arrivo FedEx e il volume è "T" proteggo sempre
029170030604      * (già da tabella "7T" il volume di tipo "T" non è sostituibile,
029180030604      *  faccio lo stesso la modifica nel caso di variazione della tabella)
029190030604     c                   If        *In02 and LnaNtw = 'FED' and vidfvf = 'T'
029200030604     c                   Eval      *In89 = *On
029210030604     c                   EndIf
029220941019    1C                   ENDIF
029230130930     c
029240130930     C**
029250130930     C* F N A R B N 0 F
029260130930     C**
029270130930    1C     FLGCVR        IFEQ      'N'
029280130930
029290130930      * Reperisco il referente e il telefono e magazzino giacenza
029300130930    2c                   if        not  *in20
029310130930
029320130930    3c                   if        d48cvb='NE'
029330131001     c                   seton                                        868483
029340130930     c                   If        wesar5gen='S'
029350130930     c                   Eval      VidRef =  PAr5Ref
029360130930     c                   Eval      VidTeld = PAr5Teld
029370130930     c                   EndIf
029380130930     c                   if        wesar5emd='S'
029390130930     c                   eval      videml = emd_§ar5eml
029400130930     c                   eval      vidtsms= emd_§ar5tel
029410130930     c                   endif
029420130930    3c                   endif
029430130930
029440130930    3c                   if        d48cvb='NT'
029450131001     c                   seton                                        81
029460130930     c                   eval      vidnt1=v1cnt1
029470130930     c                   eval      vidnt2=v1cnt2
029480130930    3c                   endif
029490130930     c
029500130930   x2c                   else
029510130930    3c                   select
029520130930     c
029530131002     c                   when      §bntrd='NOT'
029540131002     c                   if        §bnu_sn='S'
029550131001     c                   seton                                        81
029560130930     c                   movel     §bnuni70      vidnt1
029570130930     c                   move      §bnuni70      vidnt2
029580131002     c                   else
029590131002     C                   MOVEL     MSG(24)       D48MSG
029600131002     C                   MOVEL     '1'           D48ERR
029610131002     c                   leavesr
029620131002     c                   endif
029630131008     c* se la causale variazione non è NT --> errore
029640131008     c                   if        d48cvb<>'NT'
029650131008     C                   MOVEL     MSG(31)       D48MSG
029660131008     C                   MOVEL     '1'           D48ERR
029670131008     c                   leavesr
029680131008     c                   endif
029690131008
029700130930     c                   other
029710131008
029720131008     c* se la causale variazione non è NE --> errore
029730131008     c                   if        d48cvb<>'NE'
029740131008     C                   MOVEL     MSG(31)       D48MSG
029750131008     C                   MOVEL     '1'           D48ERR
029760131008     c                   leavesr
029770131008     c                   endif
029780130930     c
029790131001     c                   if        §bnu_sn='S'
029800130930     c                   eval      videml = §bnuni70
029810131001     c                   seton                                        86
029820131001     c                   endif
029830131001     c                   if        §bnr_sn='S'
029840131001     c                   eval      vidteld= §bntel
029850130930     c                   eval      vidref = §bnref
029860131001     c                   seton                                        84
029870131001     c                   endif
029880131001     c                   if        §bnc_sn='S'
029890131001     c                   eval      vidtsms= §bncel
029900131001     c                   seton                                        83
029910131001     c                   endif
029920131002     c                   if        *in86=*off and *in84=*off and *in83=*off
029930131002     C                   MOVEL     MSG(24)       D48MSG
029940131002     C                   MOVEL     '1'           D48ERR
029950131002     c                   leavesr
029960131002     c                   endif
029970131001     c
029980130930    3c                   endsl
029990130930     c
030000130930    2c                   endif
030010130930    1c                   endif
030020950104     C**
030030990930     C* F I A R B T 0 F
030040950104     C**
030050100120     c
030060941019    1C     FLGCVR        IFEQ      'T'
030070941019     C                   SELECT
030080941019     C*
030090941102     C     D66RTA        WHENEQ    'S'
030100950616     C                   SETON                                        1529        RITASSAZ
030110941102     C*
030120941102     C     D66BSF        WHENEQ    'S'
030130941019     C                   SETON                                        13          VARIA  SF
030140941019     C*
030150941102     C     D66BFI        WHENEQ    'S'
030160991006     C                   SETON                                        1407        VARIA  FAT
030170941019     C*
030180941019     C                   OTHER
030190941019     C                   SETON                                        36          TASS FRANC
030200941019     C                   ENDSL
030210941102     C* PULIZIA SFL
030220941102     C                   SETON                                        85
030230941215     C                   WRITE     LR48C03
030240941102     C                   SETOFF                                       85
030250941215     C*
030260941215     C                   MOVEL     *BLANKS       DESKSC
030270950104     C   10              MOVEL     ARBRSD        W008A             8
030280950104     C   10              MOVEL     W008A         DESKSC
030290950104     C  N10              MOVEL     ARBRSM        DESKSC
030300131120     c   10              clear                   sav_ift
030310941102     C*
030320941102     C*
030330990930     C* TASSAZIONE IN FIAR6
030340990930     C                   MOVEL     '1'           WTRC
030350990930     C     KARB2         CHAIN     FIAR6000                           30
030360990702    2C     *IN30         IFEQ      *OFF
030370990701     C                   MOVEL     'E'           WAR6
030380990701     C                   MOVEL     AR6NFT        VIDNFT
030390990701     C                   MOVEL     AR6FIV        VIDFIV
030400990702     C                   Z-ADD     AR6IMV        SAVIMV
030410151016     C                   Z-ADD     AR6IMV        SAVIMV_fat
030420131120     c   10              z-add     ar6ift        sav_ift
030430990702     C* PROTEZIONE DATI FATTURA PER PREPAGATI
030440990702    3C  N10VIDNFT        IFLT      §QTNFT
030450990719     C     VIDNFT        ANDGT     0
030460990701     C                   SETON                                        23
030470990702     C                   Z-ADD     AR6IFT        VIDIFT
030480990702    3C                   ENDIF
030490991001     C* PROTEZIONE DIVISA PER PREPAGATI
030500991001    3C  N10VIDNFT        IFGT      0
030510991012     C                   SETON                                        0708
030520991001    3C                   ENDIF
030530991108     C* SE PREPAGATO O P.A. FATTURA GIA'CONTAB. MODIFICO SOLO IMP.ASSIC
030540991008     C* E VALORE MERCE DICHIARATO
030550991108    3C     AR6DFT        IFGT      *ZEROS
030560000104     C     AR6DFT        ANDLE     WDTU10
030570991008     C                   SETON                                        2973
030580991008    3C                   ENDIF
030590991029    2C                   ENDIF
030600991029     C**
030610991029     C* SE ASSEGNATO GIA' INCASSATO, FACCIO MODIFICARE SOLO IMP.ASSIC EARIAZIONE
030620991029     C*  VALORE MERCE DICHIARATO
030630130409    1C*  10FLGCVR        IFEQ      'T'
030640130409    2C*    §7LFIN        IFEQ      'S'
030650130409     C*    ARBICA        ANDNE     ' '
030660130409     C*    ARBICA        ANDNE     'R'
030670991029     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
030680130409     C*                  SETON                                        2973
030690130409    2C*                  END
030700991108     C* SE ICA = 'R' E C'E' INCASSO PARZ<IALE IN ARI, PROTEGGO
030710991108     C*  LO STESS, FACCIO SOLO MODIFICARE IAS
030720130409    2C*    ARBICA        IFEQ      'R'
030730130409     C*    KARI          CHAIN     FIARI01L                           30
030740130409    3C* N30ARIATB        IFEQ      ' '
030750991108     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
030760130409     C*                  SETON                                        2973
030770130409    3C*                  ENDIF
030780130409    2C*                  ENDIF
030790130409    1C*                  ENDIF
030800130410     c* PROTEZIONE CAMPI PER POTER VARIARE SOLO IAS E VMD
030810130410     c* Se bolla già contabilizzata e variabile anche dopo contabilizzazione
030820130410     c* oppure se bolla già incassata e variabile anche dopo incasso (V.di fnlr66r)
030830130410     c                   if        d66bdi='S'
030840130409     C                   SETON                                        2973
030850130409     c                   endif
030860990701     C**
030870950807     C* VEDO SE ESISTE AMCHE L'AR7
030880990930     C     KARB2         SETLL     FIAR7000                               30
030890950807     C   30              MOVEL     'E'           WAR7
030900060201     c
030910060201      * Calcolo data limite mod. importo da assicurare
030920060201     c     *iso          Move      ArbDsp        wdataiso
030930060201     c                   adddur    §vpomia:*d    wdataiso
030940060201      *
030950060201     c                   do        *hival
030960060201      * verifico se è un giorno lavorativo
030970060201     c     *iso          move      wdataiso      ds_data
030980060201     C                   eval      kann = ds_anno
030990060201     C                   eval      kmes = ds_mese
031000060201     C     kazcln        chain     azcln01l
031010060201     C                   if        %found(azcln01l)
031020060201     C                   if        MAT(ds_giorno) =  'F'  or
031030060201     C                             POM(ds_giorno) =  'F'
031040060201      * se non va bene aggiungo 1 giorno
031050060201     c                   adddur    1:*d          wdataiso
031060060201     C                   iter
031070060201     c                   else
031080060201     c                   leave
031090060201     C                   endif
031100060201     c                   else
031110060201     c                   leave
031120060201     C                   endif
031130060201
031140060201     c                   enddo
031150060201
031160060201     c                   eval      wdatalimias=wdataiso
031170080625     c                   clear                   forzaias2
031180080625     c                   clear                   forzaias
031190060201     c
031200060201     c* IAS modificabile solo nei primi 2 gg  tassaz sempre
031210060201     c                   if        §utemtf=' '  and
031220060201     c                             wdataoggi>wdatalimIas
031230060201     c                   seton                                        75
031240060201     c                   endif
031250060201     c* solo IAs ma sempre
031260060201     c                   if        §utemtf='I'
031270060201     c                   seton                                        74
031280060201     c                   endif
031290060201     c* Nulla
031300060201     c                   if        §utemtf='N'
031310060201     c                   seton                                        7475
031320060201     c                   endif
031330991012     C*
031340060201     C*
031350060201     C                   CLEAR                   V3CEV1
031360060201     C                   CLEAR                   V3CSV1
031370060201     C                   CLEAR                   V3CVA1
031380060201     C                   CLEAR                   VARVA1
031390060201     C                   Z-ADD     1             NRR
031400060201     C                   DO        20            NRR
031410060201     C                   WRITE     LR48S03
031420060201     C                   ENDDO
031430920128     C* PROGRAMMA NON RICHIAMATO: DATI DA BOLLA
031440941019    2C     *IN20         IFEQ      *OFF
031450941019     C*
031460941019     C                   Z-ADD     ARBIAS        VIDIAS
031470950426     C                   Z-ADD     ARBIAS        COMIAS
031480941209     C                   MOVEL     ARBVAS        VIDVAS
031490941215     C                   MOVEL     ARBVAS        VARVAS
031500941019     C                   MOVEL     ARBKSC        VIDKLP
031510950424     C                   MOVEL     ARBKSC        VARKLP
031520941019     C                   MOVE      ARBKSC        VIDKSC
031530970822     C                   MOVE      ARBKSC        MEMKSC
031540950424     C                   MOVE      ARBKSC        VARKSC
031550941019     C                   MOVEL     ARBCTR        VIDCTR
031560050202     C                   MOVEL     ARBCTR        SAVCTR
031570060511     C                   MOVEL     ARBCTR        CTRBOLLA          3
031580941215     C                   MOVEL     ARBQFT        VIDQFT
031590950424     C                   MOVEL     ARBQFT        VARQFT
031600941215     C                   MOVEL     ARBVMD        VIDVMD
031610941215     C                   MOVEL     ARBVAD        VIDVAD
031620941215     C                   MOVEL     ARBVAD        VARVAD
031630990930     C* TASSAZIONE IN FIAR6
031640941102    3C     WAR6          IFEQ      'E'
031650990930     C                   MOVEL     AR6DIV        VIDDIV
031660991006     C                   MOVEL     AR6DIV        WDIV
031670941019     C                   MOVEL     AR6POR        VIDPOR
031680991006     C                   MOVEL     AR6POR        VARPOR
031690941019     C                   MOVEL     AR6IMV        VIDIMV
031700941019     C                   MOVEL     AR6CEI        VIDCEI
031710941102     C*
031720941102     C                   Z-ADD     0             NRR
031730941209     C                   MOVEL     AR6SV1        WSV1
031740941209     C                   Z-ADD     AR6VA1        WVA1
031750941102     C                   EXSR      CARVAR
031760941209     C                   MOVEL     AR6SV2        WSV1
031770941209     C                   Z-ADD     AR6VA2        WVA1
031780941102     C                   EXSR      CARVAR
031790941212    4C     AR6SV3        IFNE      ' '
031800941209     C                   MOVEL     AR6SV3        WSV1
031810941209     C                   Z-ADD     AR6VA3        WVA1
031820941102     C                   EXSR      CARVAR
031830941102     C*
031840941102     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
031850990930     C     KARB2         READE(N)  FIAR7000                               30
031860941102    5C     *IN30         DOWEQ     *OFF
031870941209     C                   MOVEL     AR7SVN        WSV1
031880941209     C                   MOVEL     AR7VAN        WVA1
031890941102     C                   EXSR      CARVAR
031900990930     C     KARB2         READE(N)  FIAR7000                               30
031910941102    5C                   ENDDO
031920941102    4C                   ENDIF
031930941102    3C                   ENDIF
031940941019     C*
031950941019   X2C                   ELSE
031960920128     C*
031970990930     C* PROGRAMMA     RICHIAMATO: DATI DA DARBT
031980941025     C                   Z-ADD     §BTIAS        VIDIAS
031990950426     C                   Z-ADD     §BTIAS        COMIAS
032000941019     C                   MOVEL     §BTVAS        VIDVAS
032010941220     C                   MOVEL     §BTVAS        VARVAS
032020941209     C                   MOVEL     §BTQFT        VIDQFT
032030950424     C                   MOVEL     §BTQFT        VARQFT
032040941209     C                   MOVEL     §BTVMD        VIDVMD
032050941209     C                   MOVEL     §BTVAD        VIDVAD
032060941220     C                   MOVEL     §BTVAD        VARVAD
032070920128     C                   MOVEL     §BTKSC        VIDKLP
032080950424     C                   MOVEL     §BTKSC        VARKLP
032090920128     C                   MOVE      §BTKSC        VIDKSC
032100970822     C                   MOVE      §BTKSC        MEMKSC
032110950424     C                   MOVE      §BTKSC        VARKSC
032120920128     C                   MOVEL     §BTKSC        SAVKSC            7 0
032130060511     c                   clear                   kscbolla
032140920128     C                   MOVEL     §BTCTR        VIDCTR
032150050202     C                   MOVEL     §BTCTR        SAVCTR
032160060511     c                   clear                   ctrbolla
032170990930     C                   MOVEL     §BTDIV        VIDDIV
032180991006     C                   MOVEL     §BTDIV        WDIV
032190990930     C                   Z-ADD     §BTPOR        VIDPOR
032200991006     C                   Z-ADD     §BTPOR        VARPOR
032210990930     C                   Z-ADD     §BTIMV        VIDIMV
032220920128     C                   MOVEL     §BTCEI        VIDCEI
032230941102     C* CARICO LE VARIE
032240941102     C                   Z-ADD     0             NRR               5 0
032250941209     C                   DO        20            C                 3 0
032260941209     C                   MOVEL     §SV(C)        WSV1
032270941209     C                   Z-ADD     §VA(C)        WVA1
032280941102     C                   EXSR      CARVAR
032290941209     C                   ENDDO
032300941019    2C                   ENDIF
032310131016
032320131016     c                   z-add     vidias        bol_vidias
032330131016     c                   movel     vidvas        bol_vidvas
032340131016     c                   z-add     vidvmd        bol_vidvmd
032350131016     c                   movel     vidvad        bol_vidvad
032360131016     c
032370941215     C*
032380941215     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
032390050202     C                   Z-ADD     SAVKSC        KSC
032400941215     C                   EXSR      CARCTR
032410941019     C*
032420920128     C* DECODIFICA ESENZIONE IVA
032430920128     C                   MOVEL     *BLANKS       DESCEI
032440941019    2C     VIDCEI        IFNE      ' '
032450920128     C                   MOVEL     'EI'          COD
032460920128     C                   MOVEL     *BLANKS       KEY
032470051214     C                   MOVEL(P)  VIDCEI        KEY
032480941212     C                   EXSR      CTABEL
032490920128     C  N30              MOVEL     TBLUNI        DESCEI
032500941019    2C                   END
032510920128     C* GIRO DATA FATTURA
032520941102    1C     WAR6          IFEQ      'E'
032530941102     C     AR6DFT        ANDGT     0
032540941019     C                   Z-ADD     AR6DFT        G02INV
032550941019     C                   MOVEL     '3'           G02ERR
032560941019     C                   CALL      'XSRDA8'
032570941019     C                   PARM                    WLBDAT
032580941019     C*
032590941019     C                   Z-ADD     G02DAT        VIDDFT
032600941019    2C                   ENDIF
032610941019    1C                   ENDIF
032620941019     C**
032630991001     C* F I A R B T 0 F   SECONDA BOLLA
032640941019     C**
032650911211     C     FLGCVR        IFEQ      '2'
032660941102     C                   SELECT
032670941102     C     D66RTA        WHENEQ    'S'
032680950616     C                   SETON                                        1529        RITASSAZ
032690941102     C*
032700941102     C     D66BSF        WHENEQ    'S'
032710941102     C                   SETON                                        13          VARIA  SF
032720941102     C*
032730941102     C     D66BFI        WHENEQ    'S'
032740991006     C                   SETON                                        1407        VARIA  FAT
032750941102     C*
032760941102     C                   ENDSL
032770911211     C*
032780911219     C                   MOVEL     *BLANKS       DESKSC
032790941019     C                   MOVEL     ARBRSD        W008A             8
032800941019     C                   MOVEL     W008A         DESKSC
032810131120     c                   clear                   sav_ift
032820941019     C*
032830941019     C* PGM NON RICHIAMATO
032840941019    2C     *IN20         IFEQ      *OFF
032850920128     C* NON ESISTE ESTENSIONE BOLLA
032860990930    3C     WAR62         IFEQ      ' '
032870911211     C                   MOVEL     ARBLNA        VIDKLP
032880911211     C                   MOVEL     9999          VIDKSC
032890911212     C                   MOVEL     *ZEROS        VIDCTR
032900050202     C                   MOVEL     *ZEROS        SAVCTR
032910060511     c                   clear                   ctrbolla
032920991013     C                   MOVE      *ZEROS        SAVKSC            7 0
032930060511     c                   clear                   kscbolla
032940941019     C*
032950941019   X3C                   ELSE
032960941019     C*
032970131120     c                   z-add     ar6ift        sav_ift
032980990930     C                   MOVEL     AR6DIV        VI2DIV
032990020305     C                   MOVEL     AR6DIV        WDIV
033000990930     C                   MOVEL     AR6SV1        VI2SV1
033010040122     C**!!!              Z-ADD     AR6VA1        VI2VA1
033020040122     C**!!!              Z-ADD     AR6VA1        VA2VA1
033030040123      * carico le varie
033040040122     c                   Movel     Ar6Sv1        wsv1
033050040122     c                   Z-add     Ar6Va1        wva1
033060040122     c                   ExSr      CarVar1
033070040122     c                   Movel     Ar6Sv2        wsv1
033080040122     c                   Z-add     Ar6Va2        wva1
033090040122     c                   ExSr      CarVar1
033100040122     c                   Movel     Ar6Sv3        wsv1
033110040122     c                   Z-add     Ar6Va3        wva1
033120040122     c                   ExSr      CarVar1
033130040122      * Verifico se ci sono altre varie
033140040126     c                   Movel     '2'           wtrc
033150040123     c     Karb2         Setll     Fiar701l
033160040122Do  4c                   Do        *Hival
033170040123     c     Karb2         Reade(n)  Fiar701l
033180040122     c                   If        %Eof(Fiar701l)
033190040122     c                   Leave
033200040122     c                   EndIf
033210040122     c                   Movel     Ar7Svn        wsv1
033220040122     c                   Z-add     Ar7Van        wva1
033230040122     c                   ExSr      CarVar1
033240040122    4c                   EndDo
033250990930     C                   MOVEL     AR6CEI        VI2CEI
033260990930     C                   MOVEL     AR6KSC        VIDKLP
033270990930     C                   MOVE      AR6KSC        VIDKSC
033280991013     C                   MOVEL     AR6KSC        SAVKSC
033290060511     C                   MOVEL     AR6KSC        kscbolla
033300990930     C                   MOVEL     AR6CTR        VIDCTR
033310050202     C                   MOVEL     AR6CTR        SAVCTR
033320060511     C                   MOVEL     AR6CTR        ctrbolla
033330941019    3C                   END
033340941019     C*
033350941019   X2C                   ELSE
033360941019     C*
033370990930     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DARBT 2BOLLA
033380990930     C                   MOVEL     §BTDIV        VI2DIV
033390991006     C                   MOVEL     §BTDIV        WDIV
033400040123     C**!!!              MOVEL     §BTSV1        VI2SV1
033410040123     C**!!!              Z-ADD     §BTVA1        VI2VA1
033420040123     C**!!!              Z-ADD     §BTVA1        VA2VA1
033430040123      * carico le varie
033440040123     c                   Do        20            c
033450040123     c                   Movel     §sv(c)        wsv1
033460040123     c                   Z-add     §va(c)        wva1
033470040123     c                   ExSr      CarVar1
033480041130     c                   If        ContaSv = 6
033490040123     c                   Leave
033500040123     c                   EndIf
033510040123     c                   EndDo
033520040123
033530990930     C                   MOVEL     §BTCEI        VI2CEI
033540990930     C                   MOVEL     §BTKSC        VIDKLP
033550990930     C                   MOVE      §BTKSC        VIDKSC
033560991013     C                   MOVE      §BTKSC        SAVKSC
033570060511     c                   clear                   kscbolla
033580990930     C                   MOVEL     §BTCTR        VIDCTR
033590050202     C                   MOVEL     §BTCTR        SAVCTR
033600060511     c                   clear                   ctrbolla
033610941019    2C                   END
033620050202     c
033630050202     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
033640050202     C                   Z-ADD     SAVKSC        KSC
033650050202     C                   EXSR      CARCTR
033660920128     C*
033670920128     C                   MOVEL     *BLANKS       DESCEI
033680941019    2C     VI2CEI        IFNE      ' '
033690911211     C                   MOVEL     'EI'          COD
033700911211     C                   MOVEL     *BLANKS       KEY
033710051214     C                   MOVEL(P)  VI2CEI        KEY
033720941212     C                   EXSR      CTABEL
033730911211     C  N30              MOVEL     TBLUNI        DESCEI
033740941019    2C                   END
033750941019     C* GIRO DATA FATTURA
033760990930    1C     WAR62         IFEQ      'E'
033770990930     C     AR6DFT        ANDGT     0
033780990930     C                   Z-ADD     AR6DFT        G02INV
033790941019     C                   MOVEL     '3'           G02ERR
033800941019     C                   CALL      'XSRDA8'
033810941019     C                   PARM                    WLBDAT
033820941019     C*
033830941019     C                   Z-ADD     G02DAT        VI2DFT
033840991007     C                   Z-ADD     AR6FIV        VI2FIV
033850991007     C                   Z-ADD     AR6NFT        VI2NFT
033860941019    2C                   ENDIF
033870911211     C                   END
033880941019     C**
033890050202     C* F N A R B K 0 F
033900941019     C**
033910941019    1C     FLGCVR        IFEQ      'K'
033920941019     C*
033930941019     C* CODICE BOLLA PRECEDENTE
033940941019     C                   MOVEL     ARBCBO        OLDCBO
033950941019     C* SALVO CAMPI DEL CODICE BOLLA CHE MI INTERESSANO
033960941019     C                   MOVEL     §3ATB1        SAVTB1            2
033970941102     C                   MOVEL     §3ARBL        SAVRBL            1
033980941019     C                   MOVEL     §3ATB2        SAVTB2            2
033990941019     C                   MOVEL     §3AFCA        SAVFCA            1 0
034000941019     C                   CLEAR                   DENCBO
034010050414     C*
034020050414     C* VERIFICO SE SI TRATTA DI UN PREPAGATO
034030050414     C                   MOVEL     '1'           WTRC
034040050414     C     KARB2         CHAIN(N)  FIAR6000                           30
034050050414     C  N30              MOVEL     'E'           WAR6
034060050414     C*
034070050414     C
034080920128     C*
034090920128     C* PROGRAMMA NON RICHIAMATO: PRENDO DATI DA BOLLA
034100941019    2C     *IN20         IFEQ      *OFF
034110941019     C*
034120941019    3C     WAR9          IFEQ      'E'
034130941019     C                   MOVEL     AR9TIC        VIDTIC
034140941019     C                   MOVEL     AR9CAS        VIDCAS
034150941019     C                   MOVEL     AR9VCA        VIDVCA
034160941215     C                   MOVEL     AR9VCA        VARVCA
034170941102     C                   MOVEL     AR9GCA        VIDGCA
034180941019    3C                   ENDIF
034190941019     C*
034200941019     C                   MOVEL     '?'           NEWCBO
034210920128     C*
034220941019   X2C                   ELSE
034230920128     C*
034240920128     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DSARBK
034250920128     C                   MOVEL     §BKTIC        VIDTIC
034260941019     C                   MOVEL     §BKCAS        VIDCAS
034270941102     C                   MOVEL     §BKGCA        VIDGCA
034280941019     C                   MOVEL     §BKCBN        NEWCBO
034290941220     C                   MOVEL     §BKVCA        VARVCA
034300941220     C                   MOVEL     §BKVCA        VIDVCA
034310950420     C* SE NON PASSATO IMPOSTO QUELLO DELLA BOLLA
034320950420     C     NEWCBO        IFEQ      *BLANKS
034330950420     C                   MOVEL     ARBCBO        NEWCBO
034340950420     C                   ENDIF
034350920128     C*
034360920128     C* DECODIFICA NUOVO CODICE BOLLA
034370920128     C                   MOVEL     '3A'          COD
034380950420     C                   MOVEL(P)  NEWCBO        KEY
034390941212     C                   EXSR      CTABEL
034400920128     C  N30              MOVEL     TBLUNI        DS3A
034410941019     C   30              CLEAR                   DS3A
034420941102     C                   MOVEL     §3ADES        DENCBO
034430941019    2C                   END
034440941019     C*
034450920325     C* VARIAZIONE IMPORTO CONTRASSEGNO
034460941019    2C     VIDCVR        IFEQ      'KA'                                          UFFICIO
034470931118     C     VIDCVR        OREQ      'KB'                                          DOPO CONS
034480931118     C     VIDCVR        OREQ      'KP'                                          PADRONCINO
034490920325     C                   SETON                                        21
034500941102     C                   MOVEL     ARBCBO        NEWCBO
034510941019    2C                   END
034520941019    1C                   END
034530941019     C**
034540070309     C* F N A R B G 0 F
034550941019     C**
034560950120     C     FLGCVR        IFEQ      'C'
034570950120     C* SE E' CAUSALE CP PROTEGGO I CAMPI DELLA CONSEGNA RICHIESTA E
034580950120     C*  GG CHIUSURA
034590950120     C     VIDCVR        IFEQ      'CP'
034600950120     C                   SETON                                        16
034610950120     C                   ENDIF
034620941019     C* PGM NON RICHIAMATO
034630941019     C     *IN20         IFEQ      *OFF
034640920320     C* DATA CONSEGNA RICHIESTA
034650941019     C                   Z-ADD     ARBDCR        G02INV
034660920320     C*
034670920320     C                   Z-ADD     ARBHCR        VIDHCR
034680920320     C*
034690920320     C                   MOVEL     ARBTCR        VIDTCR
034700930510     C                   MOVEL     ARBGC1        VIDGC1
034710930510     C                   MOVEL     ARBGC2        VIDGC2
034720941019     C                   MOVEL     ARBTC1        VIDTC1
034730941102     C                   MOVEL     ARBTC2        VIDTC2
034740111025     c* Se il pgm non è richiamato, verifico se nella DS nel campo tipo §BGTCR
034750111025     c*  c'e' una "S" --> significa che posso aggiornare la consegna richiesta
034760111025     c*  in FIARP se bolla in distinta (solo da gestione telefonate o da richiamo
034770111025     c*  con passaggio dati) In caso contrario non posso farlo e sid eve dare errore
034780111207     c* ES - 07/12/11 -  per il momento asterisco
034790111207     c****               eval      Dcrindist=§BGTCR
034800930510     C*
034810941019   X2C                   ELSE
034820930510     C*
034830941019     C                   Z-ADD     §BGDCR        G02INV
034840930510     C*
034850930510     C                   Z-ADD     §BGHCR        VIDHCR
034860930510     C*
034870930510     C                   MOVEL     §BGTCR        VIDTCR
034880930510     C                   MOVEL     §BGGC1        VIDGC1
034890930510     C                   MOVEL     §BGGC2        VIDGC2
034900941019     C                   MOVEL     §BGTC1        VIDTC1
034910941019     C                   MOVEL     §BGTC2        VIDTC2
034920111025     c                   eval      Dcrindist='S'
034930920320     C                   END
034940941019     C*
034950941019     C* GIRO DATA CONSEGNA RICHIESTA
034960941019     C                   MOVEL     '3'           G02ERR
034970941019     C                   CALL      'XSRDA8'
034980941019     C                   PARM                    WLBDAT
034990941019     C                   Z-ADD     G02DAT        VIDDCR
035000070309     c
035010070309     c* Se lna con GEO attiva, verifico per eventuale modifica consegna ric
035020070309     c*  se c'e' sulla bolla un evento per il quale è inseribile la
035030070319     c*  modifica della data consegna richiesta
035040070319     c                   if        FGSgeot='S'
035050070309     c     karb          setgt     fnevb05l
035060070309     c     karb          readpe    fnevb05l
035070070309     c                   dow       not %eof(fnevb05l) and eveADR=' '
035080090526     c* controllo a seconda che sia o meno richiamato il pgm
035090090526     c  n20evbcev        lookup    ADRs                                   30
035100090526     c   20evbcev        lookup    ADRC                                   30
035110070309     c                   if        *in30
035120070309     c                   eval      eveADR='S'
035130070309     c                   else
035140070309     c     karb          readpe    fnevb05l
035150070309     c                   endif
035160070309     c                   enddo
035170070309     c
035180070309     c                   endif
035190070309     c
035200930510     C                   END
035210020531     c* se modificato destinatario in disagiato o supermercato imposto il
035220020531     c* relativo flag nelle cosegne particolari
035230041015     c* Lo faccio senza testare se arbgva = "U" perchè se mi trovo in
035240041015     c* questa condizione è certo che arbgva è <> da "U"
035250070907     c**   wdisag        ifne      *blanks
035260070907     c**                 select
035270070907     c**   vidtc1        wheneq    *blanks
035280070907     c**                 movel     wdisag        vidtc1
035290070907     c**   vidtc2        wheneq    *blanks
035300070907     c**                 movel     wdisag        vidtc2
035310070907     c**                 endsl
035320070907     c**   vidtc1        ifeq      vidtc2
035330070907     c**                 clear                   vidtc2
035340070907     c**                 endif
035350070907     c**                 endif
035360950104     C**
035370070309     C* F N A R B M 0 F
035380950104     C**
035390950104     C     VIDCVR        IFEQ      'MI'
035400950104     C* PGM NON RICHIAMATO: PER ORA NON ESISTE IL RICHIAMO
035410950104     C* DECODIFICO TIPO TARIFFA
035420950104     C                   MOVEL     ARBCTR        VIDCTR
035430050202     C                   MOVEL     ARBCTR        SAVCTR
035440060511     C                   MOVEL     ARBCTR        ctrbolla
035450050202     C                   CLEAR                   Fnlv59DS
035460050202     C                   MOVEL     VIDCTR        ILV59CTR
035470050202     C                   MOVEL     'S'           ILV59CFO
035480050202     C                   CALL      'FNLV59R'
035490050202     C                   PARM                    Fnlv59DS
035500950104     C*
035510050202     C     OLV59ERR      IFEQ      ' '
035520050202     C                   MOVEL     OLV59DFS      DE2CTR
035530950104     C                   ENDIF
035540950104     C                   MOVEL     ARBKSC        VIDKLP
035550950104     C                   MOVE      ARBKSC        VIDKSC
035560970822     C                   MOVE      ARBKSC        MEMKSC
035570950104     C                   MOVEL     ARBRSM        W008A
035580950104     C                   MOVEL     W008A         DESKSC
035590051220     c                   clear                   memntw
035600051220     c     vidklp        chain     azorg01l
035610051220     c                   if        %found(azorg01l)
035620051220     C                   MOVEL     ORGDE3        OG143
035630051220     C                   MOVEL     §OGNTW        memntw
035640051220     c                   endif
035650970813     C* SE CODICE 8888 CARICO L'INDIRIZZO COMPLETO
035660950104     C     VIDKSC        IFEQ      8888
035670970813     C                   SETOFF                                       22
035680950104     C                   MOVEL     ARBRSM        VIDRSM
035690950104     C                   MOVEL     ARBINM        VIDINM
035700950104     C                   MOVEL     ARBCAM        VIDCAM
035710950104     C                   MOVEL     ARBLOM        VIDLOM
035720970808     C                   MOVEL     ARBCAM        VARCAM
035730970808     C                   MOVEL     ARBLOM        VARLOM
035740950104     C                   MOVEL     ARBPRM        VIDPRM
035750950104     C                   MOVEL     ARBNZM        VIDNZM
035760950104     C                   MOVEL     ARBFAP        VIDFAP
035770950104     C                   MOVEL     ARBCTR        VIDCTR
035780950104     C                   MOVEL     ARBCPI        VIDPIM
035790050922     C     'PRIVATO'     SCAN      vidpim        posiz             2 0    30
035800050922     c* solo numeri
035810050922     c* "PRIVATO senza iso
035820050922     c     *in30         ifeq      *off
035830050922     c     vidism        andge     *zeros
035840050922     c     *in30         oreq      *on
035850050922     c     posiz         andlt     3
035860050922     c                   clear                   vidpim
035870050922     c                   movel     arbcpi        vidcpm
035880050922     c                   endif
035890970813     C                   ELSE
035900970813     C                   SETON                                        22
035910950104     C                   ENDIF
035920950104     C                   ENDIF
035930961211     C**
035940970110     C* VARIAZIONE DEL RIFERIMENTO MITTENTE ALFABETICO E NATURA MERCE
035950961211     C**
035960961211     C     D48CVB        IFEQ      'RM'
035970970110     C                   MOVEL     ARBRMA        VIDRMA
035980970110     C                   MOVEL     ARBNAS        VIDNAS
035990961211     C                   END
036000950104     C*
036010011018      * Se l'importo d'assicurare è a 0 e la divisa non è impostata
036020011018     C     VIDIAS        IFEQ      *ZEROS
036030011018     C     VIDVAS        ANDEQ     *BLANKS
036040011018      * imposto la divisa della nazione del mittente
036050011018      * se estero
036060011018     C     MITVLT        IFNE      *BLANKS
036070011018     C     WMITIE        ANDEQ     'E'
036080011018     C                   MOVEL     MITVLT        VIDVAS
036090011018     C                   MOVEL     MITVLT        VARVAS
036100011018     C                   ELSE
036110011018      * altrimenti imposto la divisa della moneta di conto
036120011017     C                   MOVEL     §GEDCN        VIDVAS
036130011017     C                   MOVEL     §GEDCN        VARVAS
036140011017     C                   ENDIF
036150011018     C                   ENDIF
036160011018      *
036170011017      * Se la divisa e l'importo del valore merce non sono impostati
036180011018     C     VIDVAD        IFEQ      *BLANKS
036190011018     C     VIDVMD        ANDEQ     *ZEROS
036200011018      * imposto la divisa della nazione del mittente
036210011018      * se estero
036220011018     C     MITVLT        IFNE      *BLANKS
036230011018     C     WMITIE        ANDEQ     'E'
036240011018     C                   MOVEL     MITVLT        VIDVAD
036250011018     C                   MOVEL     MITVLT        VARVAD
036260011018     C                   ELSE
036270011018      * altrimenti imposto la divisa della moneta di conto
036280011017     C                   MOVEL     §GEDCN        VIDVAD
036290011017     C                   MOVEL     §GEDCN        VARVAD
036300011017     C                   ENDIF
036310011018     C                   ENDIF
036320011018      *
036330011017      * Se la divisa e l'importo del contrassegno non sono impostati
036340011018     C     VIDVCA        IFEQ      *BLANKS
036350011018     C     VIDCAS        ANDEQ     *ZEROS
036360011018      * imposto la divisa della nazione del destinatario
036370011018      * se estero
036380011018     C     DESVLT        IFNE      *BLANKS
036390011018     C     WDESIE        ANDEQ     'E'
036400011018     C                   MOVEL     DESVLT        VIDVCA
036410011018     C                   MOVEL     DESVLT        VARVCA
036420011018    2C                   ELSE
036430011018      * altrimenti imposto la divisa della moneta di conto
036440011017     C                   MOVEL     §GEDCN        VIDVCA
036450011017     C                   MOVEL     §GEDCN        VARVCA
036460011017     C                   ENDIF
036470011018     C                   ENDIF
036480030203      *------------
036490030124      * Bancali
036500030711      *  solo x causale 'BR'
036510030711If  1c                   If        D48Cvb = 'BR'
036520030124     c                   Clear                   dAr5Ban
036530030124     c                   Eval      kAr5Trd = 'BAN'
036540030725     c     kAr5          Chain     Fiar501l
036550030711     c                   If        %Found(Fiar501l)
036560030124     c                   Movel     Ar5Uni        dAr5Ban
036570030711     c                   EndIf
036580030124     c                   Eval      VidNba = §Ar5Nba
036590030124     c                   Eval      VidNb2 = §Ar5Nb2
036600030124     c                   Eval      VidRb1 = §Ar5Rb1
036610030124     c                   Eval      VidRb2 = §Ar5Rb2
036620030124     c                   Eval      VidNomb = §Ar5Nomb
036630030124     c                   Eval      VidMotb = §Ar5Motb
036640030124If  2c                   If        §Ar5Tba <> *Zeros
036650030612     c                   Clear                   Tibs02DS
036660030124     c                   Clear                   dTba
036670030124     c                   Movel     'C'           T02Mod
036680030124     c                   Movel     Knsif         T02Sif
036690030124     c                   Movel     'TBA'         T02Cod
036700030124     c                   Movel(p)  §Ar5Tba       T02Ke1
036710030124     c                   Call      'TIBS02R'
036720030124     c                   Parm                    Kpjba
036730030612     c                   Parm                    Tibs02DS
036740030124If  3c                   If        T02Err = *Blanks
036750030124     c                   Movel     T02Uni        dTba
036760030124    3c                   EndIf
036770030124     c                   Eval      VidTba = §TbaDesc
036780030124     c                   Eval      VidTbar = §TbaDesc
036790030124    2c                   EndIf
036800030124If  2c                   If        §Ar5Tb2 <> *Zeros
036810030612     c                   Clear                   Tibs02DS
036820030124     c                   Clear                   dTba
036830030124     c                   Movel     'C'           T02Mod
036840030124     c                   Movel     Knsif         T02Sif
036850030124     c                   Movel     'TBA'         T02Cod
036860030124     c                   Movel(p)  §Ar5Tb2       T02Ke1
036870030124     c                   Call      'TIBS02R'
036880030124     c                   Parm                    Kpjba
036890030612     c                   Parm                    Tibs02DS
036900030124If  3c                   If        T02Err = *Blanks
036910030124     c                   Movel     T02Uni        dTba
036920030124    3c                   EndIf
036930030124     c                   Eval      VidTb2 = §TbaDesc
036940030124     c                   Eval      VidTb2r = §TbaDesc
036950030124    2c                   EndIf
036960030711
036970080805     c* Se pgm richiamato, imposto dati dalla DSARBD
036980080805    2c                   if        *in20
036990080805     c                   Eval      VidRb1 = §bdpkf
037000080805     c                   Eval      VidRb2 = §bdvlf
037010080805    3c                   if        §bdrsd<>*blanks
037020080805     c                   Eval      VidNomb = §bdrsd
037030080805    3c                   endif
037040080805    3c                   if        §bdrd2<>*blanks
037050080805     c                   Eval      VidMotb = §bdrd2
037060080805     c                   else
037070080805     c                   Eval      VidMotb = 'PDA  '
037080080805    3c                   EndIf
037090080805
037100080805    2c                   EndIf
037110080805    1c                   EndIf
037120040608      *------------
037130040608      * Supermercati - telefono e referente
037140040608      *  solo x causale 'CS'
037150040608If  1c                   If        D48Cvb = 'CS'
037160040608     c                   Eval      wokscr = *Off
037170040608     c                   Clear                   dAr5Scr
037180040608     c                   Eval      kAr5Trd = 'SCR'
037190040608     c     kAr5          Setgt     Fiar501l
037200040608do  2c                   Do        *Hival
037210040608     c     kAr5          Readpe(n) Fiar501l
037220040608     c                   If        %Eof(Fiar501l)
037230040608     c                   Leave
037240040608     c                   EndIf
037250040608     c                   Movel     Ar5Uni        dAr5Scr
037260040608     c                   Eval      VidNom = §Ar5Nom
037270040608     c                   Eval      VidTel = §Ar5Tel
037280040608     c                   Eval      wokscr = *On
037290040608     c                   Leave
037300040608e   2c                   EndDo
037310040608      * se non ho trovato il rcd SCR cerco i dati sul rcd GEN
037320040608if  2c                   If        wokscr = *Off
037330070910     c***                Clear                   dAr5Gen
037340070910     c***                Eval      kAr5Trd = 'GEN'
037350070910     c***  kAr5          Chain(n)  Fiar501l
037360070910     c***                If        %Found(Fiar501l)
037370070910     c***                Movel     Ar5Uni        dAr5Gen
037380070910     c***                EndIf
037390070910     c***                Eval      VidNom = §Ar5Ref
037400070910     c***                Eval      VidTel = §Ar5Teld
037410070910     c                   Eval      VidNom = PAr5Ref
037420070910     c                   Eval      VidTel = PAr5Teld
037430040608e   2c                   EndIf
037440040608e   1c                   EndIf
037450911209     C*
037460910429     C                   ENDSR
037470941102     C*
037480941102     C* CARICO LA VARIA IN SFL ---------------------------------------*
037490941102     C     CARVAR        BEGSR
037500941102     C*
037510941102     C     WSV1          IFNE      ' '
037520011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
037530011018     C     ARBFBR        IFEQ      'S'
037540011018     C     WSV1          ANDEQ     'N'
037550011018     C     WVA1          ANDEQ     88888888
037560011018     C                   MOVEL     '1'           FLGVRN            1
037570011018     C                   ENDIF
037580990930     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
037590990930     C     WSV1          LOOKUP    SVA                                    33
037600990930     C     *IN33         IFEQ      *ON
037610941102     C                   ADD       1             NRR
037620941102     C     NRR           CHAIN     LR48S03                            30
037630941102     C                   MOVEL     WSV1          V3CSV1
037640941102     C                   Z-ADD     WVA1          V3CVA1
037650991006     C                   Z-ADD     WVA1          VARVA1
037660941102     C   30              WRITE     LR48S03
037670941102     C  N30              UPDATE    LR48S03
037680941102     C                   ENDIF
037690990930     C                   ENDIF
037700941102     C                   ENDSR
037710040122
037720040122      *---------------------------------------------------------------*
037730040122      * CARICO LA VARIA DELLA SECONDA BOLLA
037740040122      *---------------------------------------------------------------*
037750040122     c     CarVar1       BegSr
037760040122
037770040122if  1c                   If        wsv1 <> *Blanks
037780040122      * Carico la varia solo se fa parte del totale imponibile
037790040122     c     wsv1          Lookup    sva                                    31
037800040122if  2c                   If        *In31
037810040122     c                   add       1             ContaSv
037820040122     c                   add       wva1          TotImv
037830040122
037840040122s   3c                   Select
037850040122     c                   When      ContaSv = 1
037860040122     c                   Movel     wsv1          Vi2Sv1
037870040122     c                   Z-add     wva1          Vi2Va1
037880040122     c                   Z-add     wva1          Va2Va1
037890040122     c                   When      ContaSv = 2
037900040122     c                   Movel     wsv1          Vi2Sv2
037910040122     c                   Z-add     wva1          Vi2Va2
037920040122     c                   Z-add     wva1          Va2Va2
037930040122     c                   When      ContaSv = 3
037940040122     c                   Movel     wsv1          Vi2Sv3
037950040122     c                   Z-add     wva1          Vi2Va3
037960040122     c                   Z-add     wva1          Va2Va3
037970040122     c                   When      ContaSv = 4
037980040122     c                   Movel     wsv1          Vi2Sv4
037990040122     c                   Z-add     wva1          Vi2Va4
038000040122     c                   Z-add     wva1          Va2Va4
038010041130     c                   When      ContaSv = 5
038020041130     c                   Movel     wsv1          Vi2Sv5
038030041130     c                   Z-add     wva1          Vi2Va5
038040041130     c                   Z-add     wva1          Va2Va5
038050041130     c                   When      ContaSv = 6
038060041130     c                   Movel     wsv1          Vi2Sv6
038070041130     c                   Z-add     wva1          Vi2Va6
038080041130     c                   Z-add     wva1          Va2Va6
038090040122    3c                   EndSl
038100040122
038110040122    2c                   EndIf
038120040122
038130040122    1c                   EndIf
038140040122
038150040122     c                   EndSr
038160910429     C*
038170991001     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
038180991001     C     CARWAR        BEGSR
038190991001     C*
038200991001     C                   CLEAR                   WINL
038210991001     C*
038220991001     C                   CLEAR                   WSV
038230991001     C                   CLEAR                   WVA
038240991001     C                   CLEAR                   C
038250991001    1C                   DO        20            NRR
038260991001     C     NRR           CHAIN     LR48S03                            30
038270991001     C*
038280991001    2C     *IN30         IFEQ      *OFF
038290991001     C     V3CSV1        ANDNE     *BLANKS
038300991001     C                   ADD       1             C
038310991001     C                   MOVEL     V3CSV1        WSV(C)
038320991001     C                   Z-ADD     V3CVA1        WVA(C)
038330991001    2C                   ENDIF
038340991001    1C                   ENDDO
038350991001     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
038360991001     C     WCKINL        IFEQ      '1'
038370991001     C                   Z-ADD     1             II
038380991001     C     §$2FIN        LOOKUP    WSV(II)                                30
038390991001     C   30WVA(II)       IFGT      *ZEROS
038400991001     C                   MOVE      '1'           WINL              1
038410991001     C                   ENDIF
038420991001     C                   ENDIF
038430991001     C*
038440991001     C                   ENDSR
038450040123      *---------------------------------------------------------------*
038460040123      * Carico le varie della seconda bolla in una sk di comodo
038470040123      *---------------------------------------------------------------*
038480040123     c     Carwar1       Begsr
038490040123
038500040123     c                   Clear                   wsv
038510040123     c                   Clear                   wva
038520040123     c                   Clear                   c
038530040123
038540040123     c                   If        vi2sv1 <> *Blanks
038550050401     c                   add       1             c
038560050401     c                   Movel     vi2Sv1        wsv(c)
038570050401     c                   Z-add     vi2va1        wva(c)
038580040123     c                   EndIf
038590040123     c                   If        vi2sv2 <> *Blanks
038600050401     c                   add       1             c
038610050401     c                   Movel     vi2Sv2        wsv(c)
038620050401     c                   Z-add     vi2va2        wva(c)
038630040123     c                   EndIf
038640040123     c                   If        vi2sv3 <> *Blanks
038650050401     c                   add       1             c
038660050401     c                   Movel     vi2Sv3        wsv(c)
038670050401     c                   Z-add     vi2va3        wva(c)
038680040123     c                   EndIf
038690040123     c                   If        vi2sv4 <> *Blanks
038700050401     c                   add       1             c
038710050401     c                   Movel     vi2Sv4        wsv(c)
038720050401     c                   Z-add     vi2va4        wva(c)
038730040123     c                   EndIf
038740041130     c                   If        vi2sv5 <> *Blanks
038750050401     c                   add       1             c
038760050401     c                   Movel     vi2Sv5        wsv(c)
038770050401     c                   Z-add     vi2va5        wva(c)
038780041130     c                   EndIf
038790041130     c                   If        vi2sv6 <> *Blanks
038800050401     c                   add       1             c
038810050401     c                   Movel     vi2Sv6        wsv(c)
038820050401     c                   Z-add     vi2va6        wva(c)
038830041130     c                   EndIf
038840040123
038850040123     c                   EndSr
038860991012     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
038870991012     C     RDTASV        BEGSR
038880991012     C*
038890991012     C                   CLEAR                   C
038900030620     C                   CLEAR                   WVAR3A
038910040123     c                   Clear                   wvar3ai
038920991012    1C                   DO        20            NRR
038930991012     C     NRR           CHAIN     LR48S03                            30
038940991012    2C     *IN30         IFEQ      *OFF
038950991012     C                   ADD       1             C
038960020503     c* se la varia è uguale a quella prevista dal tipo bolla, non richiamo
038970020503     c*  piu' la tassazione
038980040123     c                   if        §3asva<>*blanks and v3csv1=§3asva
038990040123     c                   If        vidimv > *Zeros
039000020503     c                   eval      wtassa='N'
039010030620      * mi salvo l'imnporto della varia
039020030620     c                   eval      itassa = v3cva1
039030040123     c                   EndIf
039040030620      * mi salvo un campo di comodo
039050030620     c                   movel     'S'           Wvar3a            1
039060040123      * memorizzo anche se valorizzata
039070040123     c                   If        v3cva1 > 0
039080040123     c                   Eval      wvar3ai = 'S'
039090040123     c                   EndIf
039100030620      *
039110020503     c                   endif
039120030620      * verifico se esiste la varia "D" non la ricalcolo in caso di fattura immediata
039130030620     c                   if        v3csv1 = 'D'
039140030620     c                   eval      wtassad = 'N'
039150030620     c                   endif
039160030620      *
039170030624      * se esiste la varia "&" con tutti 8888888 non la passo al TNSF20R
039180030624      * ma passo un flag  di richiesta calcolo varia "&"
039190030709     C                   if        v3csv1 = '&' and v3cva1 = 88888888 and
039200030709     C                             flgsf20 = 'S'
039210030624     C                   eval      TASFCAA = 'S'
039220030624     C                   else
039230991012     C                   MOVEL     V3CSV1        VSV(C)
039240991012     C                   MOVEL     V3CVA1        VVA(C)
039250030624     c                   endif
039260030624      *
039270991012    2C                   ENDIF
039280991012    1C                   ENDDO
039290991012     C                   ENDSR
039300920120     C*--- EMISSIONE E GESTIONE FORMATO 2 ----------------------------*
039310920120     C     EMFOR2        BEGSR
039320941102     C*
039330920120     C     FOR02         TAG
039340941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
039350950519    1C     *IN90         IFEQ      *ON
039360950114     C     D48FFR        OREQ      'S'
039370950607     C     D48FFR        OREQ      'V'
039380941214     C                   WRITE     LR48T03
039390941014     C                   EXFMT     LR48D02
039400950519    1C                   END
039410941102     C*
039420941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
039430941102     C                   SETOFF                                       2890
039440941102     C                   SETOFF                                       404142
039450941102     C                   SETOFF                                       434445
039460941102     C                   SETOFF                                       464748
039470061115     C                   SETOFF                                       495051
039480140408     C                   SETOFF                                       5268
039490941220     C* F12 - RITORNO
039500941220    1C     *INKL         IFEQ      *OFF
039510070329     c
039520070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
039530070329     c                   if        *in87 and *inkr
039540070329     c                   exsr      CALLNOTELDV
039550070329     c                   goto      for02
039560070329     c                   endif
039570941216     C*
039580941216     C* SE NON POSSO VARIARE NE IL PESO, NE IL VOLUME, NON CONTROLLO
039590941216     C* E NON AGGIORNO
039600941220    2C     *IN88         IFEQ      *OFF
039610941216     C     *IN89         OREQ      *OFF
039620950114     C     *IN24         OREQ      *ON
039630941216     C*
039640941216     C* CONTROLLO FORMATO
039650920120     C                   EXSR      CONTR2
039660941215     C*
039670941220     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
039680941220    3C     *IN90         IFEQ      *ON
039690941215     C     D48FFR        ANDEQ     'E'
039700941215     C                   MOVEL     VIDMSG        D48MSG
039710941215     C                   MOVEL     '2'           D48ERR
039720941215     C                   GOTO      ENDEM2
039730941220    3C                   ENDIF
039740140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
039750140502     c*   (d48ffr='E' e 26 off)
039760140502     c  n90
039770140502     cann26              if        d48ffr='E'
039780140502     C                   GOTO      ENDEM2
039790140502     c                   endif
039800950607     C  NKF
039810950607     CANN26
039820920120     COR 90              GOTO      FOR02
039830060227     c* richiamo routine di richiesta/controllo motivazione
039840060301     c                   if        d66mtv = 'S' and w48mct = *blanks
039850060227     c                   exsr      ges_mtv
039860060301     c                   if        olv85f12 = '1'
039870060301     c                   if        ilv85wdw = ' '
039880060227     c                   goto      for02
039890060301     c                   else
039900060301     c                   eval      *inkl = '1'
039910060301     C                   GOTO      ENDEM2
039920060227     c                   endif
039930060301     c                   endif
039940060227     C     *IN90         IFEQ      *ON
039950060227     C     D48FFR        ANDEQ     'E'
039960060227     C                   MOVEL     VIDMSG        D48MSG
039970060227     C                   MOVEL     '2'           D48ERR
039980060227     C                   GOTO      ENDEM2
039990060227     C                   ENDIF
040000060227     c                   endif
040010920120     C*
040020141020     c* Se Richiesta modifica di indirizzo tale da variare la LNA emetto window
040030141020     c* per segnalare e richiedere una delle seguenti opzioni:
040040141020     c*  -Forzare la variazione senza variare la lna
040050141020     c*  -Richiedere il dirottamento
040060141111    3c                   if        d48ffr<>'E'  and *in01
040070141029     c                             and (%lookup('999':skFilOkdir)>0 or
040080141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)>0 and
040090141029     c                              %lookup(%editc(dutpou:'X'):skFilOkdir)>0) )
040100141020     c                   clear                   wdir
040110141020    4C     d_o95lna      ifne      ARBLNA
040120141022     c* le bolle export non le dirottiamo mai da qui
040130141022     c     lnantw        andne     'DPD'
040140141022     c     lnantw        andne     'EEX'
040150141022     c     lnantw        andne     'FED'
040160141219     c* no bolle di recupero
040170141219     c     §3arbl        andne     'R'
040180141204    5C     SAVCAD        IFNE      VIDCAD
040190141204     C     SAVLOD        ORNE      VIDLOD
040200141204     C     SAVrsd        ORNE      VIDrsd
040210141204     C     SAVind        ORNE      VIDind
040220141204     C     SAVprd        ORNE      VIDprd
040230141204     C     SAVnzd        ORNE      VIDnzd
040240141204     c     vidzng        oreq      savzng
040250141203     c     vidffd        andeq     *blanks
040260141021     c                   exsr      ges_wdw02
040270141021     c                   if        winf12='1'
040280141021     c                   goto      for02
040290141021     c                   endif
040300141020    5C                   END
040310141020    4C                   END
040320141020    4c                   if        wdir='1'
040330141212
040340141212     c* se hanno cambiato il Fermo deposito devo anche eseguire la REGIS2
040350141212     c                   if        arbffd<>%subst(vidffD:1:1)
040360141212     c                   eval      wnoindir='S'
040370141212     c                   exsr      regis2
040380141218     C   91              GOTO      FOR02
040390141212     c                   endif
040400141212
040410141020     c                   exsr      sr_tiidc
040420141212
040430141020   x4c                   else
040440141212     c                   eval      wnoindir=' '
040450141020     C                   EXSR      REGIS2
040460141020    4c                   endif
040470141020   x3c                   else
040480141212     c                   eval      wnoindir=' '
040490930517     C                   EXSR      REGIS2
040500141020    3c                   endif
040510991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040520991130     C     *IN91         IFEQ      *ON
040530991130     C     D48FFR        ANDEQ     'E'
040540991130     C                   MOVEL     VIDMSG        D48MSG
040550991130     C                   MOVEL     '2'           D48ERR
040560991130     C                   GOTO      ENDEM2
040570991130     C                   ENDIF
040580991130     C   91              GOTO      FOR02
040590941216     C*
040600941220   X2C                   ELSE
040610941216     C*
040620941216     C                   GOTO      FOR02
040630941220    2C                   ENDIF
040640941220     C*
040650950518    1C                   ENDIF
040660150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
040670150112     c*  se ce ne sono
040680150112     c                   if        msgforzatxt<>*blanks
040690150112     C                   MOVEL     msgforzatxt   D48MSG
040700150112     C                   MOVEL     'Z'           D48ERR
040710150112     c                   clear                   msgforzatxt
040720150112     C                   ENDIF
040730950518     C*
040740950518     C     ENDEM2        TAG
040750950518     C*
040760950518    1C     *INKL         IFEQ      *ON
040770950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040780950519    1C     D48FFR        OREQ      'E'
040790950518     C     *IN90         ANDEQ     *ON
040800000303    1C     D48FFR        OREQ      'E'
040810000303     C     *IN91         ANDEQ     *ON
040820941220     C* F12-RITORNO
040830160119    2C     *IN10         IFEQ      *ON
040840941220     C                   UNLOCK    FNARB01L
040850051118     C                   UNLOCK    FiAR901L
040860160119     c                   endif
040870160119     C****               ELSE
040880941222     C                   UNLOCK    FNBLP01L
040890160119    2C****               END
040900991001     C                   UNLOCK    FIAR601L
040910950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
040920950607     C* flag D48F12 = 1
040930950607     C   KL              MOVEL     '1'           D48F12
040940950519    1C                   END
040950941215     C*
040960950518     C                   ENDSR
040970920121     C*
040980920121     C*--- EMISSIONE E GESTIONE FORMATO 3 ----------------------------*
040990920121     C     EMFOR3        BEGSR
041000990415     C*
041010990415     C                   MOVEL     ' '           COMASS
041020941102     C*
041030920121     C     FOR03         TAG
041040941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
041050950114     C     *IN90         IFEQ      *ON
041060941212     C     D48FFR        OREQ      'S'
041070950607     C     D48FFR        OREQ      'V'
041080941215     C                   WRITE     LR48T03
041090941212     C                   WRITE     LR48Z03
041100941212     C                   EXFMT     LR48C03
041110941220    1C                   END
041120941102     C*
041130941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
041140941209     C                   SETOFF                                       289052
041150941102     C                   SETOFF                                       404142
041160991006     C                   SETOFF                                       434445
041170941102     C                   SETOFF                                       464748
041180941209     C                   SETOFF                                       495051
041190941215     C                   SETOFF                                       989453
041200961112     C                   SETOFF                                       545556
041210990415     C                   SETOFF                                       82
041220941220     C*
041230941220     C* PER F12-RITORNO O F21 RITASSAZIONE, DISALLOCO I RECORD
041240941220     C* F12-RITORNO
041250941220    1C     *INKL         IFEQ      *ON
041260941220     C     *INKV         OREQ      *ON
041270941222     C*
041280941222     C     *IN10         IFEQ      *ON
041290941220     C                   UNLOCK    FNARB01L
041300051118     C                   UNLOCK    FiAR901L
041310941222     C                   ELSE
041320941222     C                   UNLOCK    FNBLP01L
041330941220    1C                   ENDIF
041340991005     C                   UNLOCK    FIAR601L
041350950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041360950607     C* flag D48F12 = 1
041370950607     C   KL              MOVEL     '1'           D48F12
041380941222    1C                   ENDIF
041390941220     C* F12 - RITORNO
041400941220    1C  NKL              DO
041410941220     C*
041420941220     C* F21 - RITASSAZIONE BOLLA
041430941220    2C     *INKV         IFEQ      *ON
041440050511     C                   CLEAR                   Fnlra2DS
041450050511     C                   Z-ADD     ARBAAS        ilra2AAS
041460050511     C                   Z-ADD     ARBLNP        ilra2lNP
041470050511     C                   Z-ADD     ARBNRS        ilra2NRS
041480050511     C                   Z-ADD     ARBNSP        ilra2NSP
041490050511     C                   MOVEL     D48FGS        ilra2FGS
041500050511     C                   MOVEL     D48CVB        ilra2CVB
041510050511     C                   MOVEL     Fnlra2DS      KPJBU
041520050511     C                   CALL      'FNLRA2R'
041530941217     C                   PARM                    KPJBA
041540030718     C                   PARM                    TRUL90DS
041550050804     c* Se mi passa errore, lo passo dal lr48 al lr36
041560050804     c                   movel     kpjbu         fnlra2ds
041570050804     C                   MOVEL     olra2msg      D48MSG
041580050804     C                   MOVEL     olra2err      D48ERR
041590131119     c                   if        olra2nft>0
041600131119     C                   MOVEL     '1'           WTRC
041610131119     c     karb2         chain(n)  fiar6000
041620131119     c                   endif
041630131120     c* trasmetto variazione a PDA:
041640131120     c* se segue fattura --> solo se prima era una fattura immediata
041650131120     c* se fattura immediata --> solo se variato importo della fattura
041660131119     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
041670131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
041680131120     c                              or ar6div<>viddiv)))
041690130416     c                   eval      wpda_ndc=arbndc
041700130416     c                   eval      wpda_ddc=arbddc
041710130416     c                   eval      wpda_dcm=arbdcm
041720140120     c                   eval      arbdtv=olra2dtv
041730140120     c                   eval      arborv=olra2orv
041740140120     c                   eval      arbpru=olra2pru
041750140120     c                   eval      arbcvb=d48cvb
041760130416     c                   exsr      sr_pda
041770130416     c                   endif
041780920121     C*
041790941220   X2C                   ELSE
041800920121     C* CONTROLLO FORMATO
041810920121     C                   EXSR      CONTR3
041820941215     C*
041830941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041840941220    3C     *IN90         IFEQ      *ON
041850941215     C     D48FFR        ANDEQ     'E'
041860941215     C                   MOVEL     VIDMSG        D48MSG
041870941215     C                   MOVEL     '3'           D48ERR
041880941215     C                   GOTO      ENDEM3
041890941220    3C                   ENDIF
041900920320     C*
041910950607     C  NKF
041920950607     CANNKI
041930950607     CANN26
041940990415     COR 82
041950000105     COR KN
041960920320     COR 90              GOTO      FOR03
041970060302     c* richiamo routine di richiesta/controllo motivazione
041980060302     c                   if        d66mtv = 'S' and w48mct = *blanks
041990060302     c                   exsr      ges_mtv
042000060302     c                   if        olv85f12 = '1'
042010060302     c                   if        ilv85wdw = ' '
042020060302     c                   goto      for03
042030060302     c                   else
042040060302     c                   eval      *inkl = '1'
042050060302     C                   GOTO      ENDEM3
042060060302     c                   endif
042070060302     c                   endif
042080060302     C     *IN90         IFEQ      *ON
042090060302     C     D48FFR        ANDEQ     'E'
042100060302     C                   MOVEL     VIDMSG        D48MSG
042110070308     C                   MOVEL     '3'           D48ERR
042120060302     C                   GOTO      ENDEM3
042130060302     C                   ENDIF
042140060302     c                   endif
042150920121     C*
042160920204     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
042170920204     C                   EXSR      REGIS3
042180991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042190991130     C     *IN91         IFEQ      *ON
042200991130     C     D48FFR        ANDEQ     'E'
042210991130     C                   MOVEL     VIDMSG        D48MSG
042220991130     C                   MOVEL     '3'           D48ERR
042230991130     C                   GOTO      ENDEM3
042240991130     C                   ENDIF
042250920204     C   91              GOTO      FOR03
042260920204     C*
042270070309     c* ristampo fattura immediata se variata(37 on), se in arrivo
042280070309     c*  (10 on) e se non attiva GEO
042290130321    3C**   *IN37         IFEQ      *ON
042300130321    3C***  *IN10         ANDEQ     *ON
042310130321     c**   FGSddaSI      andeq     ' '
042320130321     C***                CLEAR                   FNLSB5DS
042330130321     C***                MOVEL     'V'           DB0RIS
042340130321     C***                MOVEL     D48TBO        DB0TBO
042350130321     C***                MOVEL     ARBAAS        DB0AAS
042360130321     C***                MOVEL     ARBLNP        DB0LNP
042370130321     C***                MOVEL     ARBNRS        DB0NRS
042380130321     C***                MOVEL     ARBNSP        DB0NSP
042390130321     C***                CALL      D90PSL
042400130321     C***                PARM                    FNLSB5DS
042410130321    3C***                END
042420941212     C*
042430941220    2C                   END
042440941220     C*
042450000303    1C                   END
042460000303     C     ENDEM3        TAG
042470000303     C* SE PGM RICHIAMATO E C'E' ERRORE DISALLOCO
042480000303    1C     D48FFR        IFEQ      'E'
042490000303     C     *IN90         ANDEQ     *ON
042500000303    1C     D48FFR        OREQ      'E'
042510000303     C     *IN91         ANDEQ     *ON
042520000303     C     *IN10         IFEQ      *ON
042530000303     C                   UNLOCK    FNARB01L
042540051118     C                   UNLOCK    FiAR901L
042550000303     C                   ELSE
042560000303     C                   UNLOCK    FNBLP01L
042570000303     C                   ENDIF
042580000303     C                   UNLOCK    FIAR601L
042590000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
042600000303     C* flag D48F12 = 1
042610000303     C   KL              MOVEL     '1'           D48F12
042620000303    1C                   ENDIF
042630920121     C*
042640000303     C                   ENDSR
042650941102     C*
042660941102     C* EMETTO FORMATO4 ----------------------------------------------*
042670941102     C     EMFOR4        BEGSR
042680941102     C*
042690941102     C     FOR04         TAG
042700941102     C*
042710950114     C     *IN90         IFEQ      *ON
042720941212     C     D48FFR        OREQ      'S'
042730950607     C     D48FFR        OREQ      'V'
042740941102     C                   WRITE     LR48T01
042750941102     C                   EXFMT     LR48D04
042760941102     C                   ENDIF
042770941209     C*
042780941209     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
042790941209     C                   SETOFF                                       289045
042800941209     C                   SETOFF                                       464748
042810991006     C                   SETOFF                                       495051
042820041130     c                   setoff                                       636465
042830041130     c                   setoff                                       6667
042840941220     C*
042850941220     C* PER F12 -RITORNO O F21-RITASSAZIONE SBLOCCO I RECORD
042860941220     C     *INKL         IFEQ      *ON
042870941220     C     *INKV         OREQ      *ON
042880941222     C*
042890941222     C     *IN10         IFEQ      *ON
042900941220     C                   UNLOCK    FNARB01L
042910051118     C                   UNLOCK    FiAR901L
042920941222     C                   ELSE
042930941222     C                   UNLOCK    FNBLP01L
042940941220     C                   ENDIF
042950991005     C                   UNLOCK    FIAR601L
042960950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
042970950607     C* flag D48F12 = 1
042980950607     C   KL              MOVEL     '1'           D48F12
042990941222     C                   ENDIF
043000941220     C* F12 - RITORNO
043010941102     C     *INKL         IFEQ      *OFF
043020050228     C* F21 - RITASSAZIONE BOLLA
043030941220     C     *INKV         IFEQ      *ON
043040050511     C                   CLEAR                   Fnlra2DS
043050050511     C                   Z-ADD     ARBAAS        ilra2AAS
043060050511     C                   Z-ADD     ARBLNP        ilra2LNP
043070050511     C                   Z-ADD     ARBNRS        ilra2NRS
043080050511     C                   Z-ADD     ARBNSP        ilra2NSP
043090050511     C                   MOVEL     D48FGS        ilra2FGS
043100050511     C                   MOVEL     D48CVB        ilra2CVB
043110050511     C                   MOVEL     Fnlra2DS      KPJBU
043120050511     C                   CALL      'FNLRA2R'
043130941102     C                   PARM                    KPJBA
043140030718     C                   PARM                    TRUL90DS
043150050804     c
043160050804     c* Se mi passa errore, lo passo dal lr48 al lr36
043170050804     c                   movel     kpjbu         fnlra2ds
043180050804     C                   MOVEL     olra2msg      D48MSG
043190050804     C                   MOVEL     olra2err      D48ERR
043200131119     c                   if        olra2nft>0
043210131119     C                   MOVEL     '2'           WTRC
043220131119     c     karb2         chain(n)  fiar6000
043230131119     c                   endif
043240131120     c* trasmetto variazione a PDA:
043250131120     c* se segue fattura --> solo se prima era una fattura immediata
043260131120     c* se fattura immediata --> solo se variato importo della fattura
043270131120     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
043280131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
043290131120     c                              or ar6div<>viddiv)))
043300130416     c                   eval      wpda_ndc=arbndc
043310130416     c                   eval      wpda_ddc=arbddc
043320130416     c                   eval      wpda_dcm=arbdcm
043330140120     c                   eval      arbdtv=olra2dtv
043340140120     c                   eval      arborv=olra2orv
043350140120     c                   eval      arbpru=olra2pru
043360140120     c                   eval      arbcvb=d48cvb
043370130416     c                   exsr      sr_pda
043380130416     c                   endif
043390941102     C*
043400941220     C                   ELSE
043410941102     C* CONTROLLO FORMATO
043420941102     C                   EXSR      CONTR4
043430941215     C*
043440941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
043450941215     C     *IN90         IFEQ      *ON
043460941215     C     D48FFR        ANDEQ     'E'
043470941215     C                   MOVEL     VIDMSG        D48MSG
043480941215     C                   MOVEL     '4'           D48ERR
043490941215     C                   GOTO      ENDEM4
043500941215     C                   ENDIF
043510941102     C*
043520140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
043530140502     c*   (d48ffr='E' e 26 off)
043540140502     c  n90
043550140502     cann26              if        d48ffr='E'
043560140502     C                   GOTO      ENDEM4
043570140502     c                   endif
043580950607     C  NKF
043590950607     CANNKI
043600950607     CANN26
043610941102     COR 90              GOTO      FOR04
043620060302     c* richiamo routine di richiesta/controllo motivazione
043630060302     c                   if        d66mtv = 'S' and w48mct = *blanks
043640060302     c                   exsr      ges_mtv
043650060302     c                   if        olv85f12 = '1'
043660060302     c                   if        ilv85wdw = ' '
043670060302     c                   goto      for04
043680060302     c                   else
043690060302     c                   eval      *inkl = '1'
043700060302     C                   GOTO      ENDEM4
043710060302     c                   endif
043720060302     c                   endif
043730060302     C     *IN90         IFEQ      *ON
043740060302     C     D48FFR        ANDEQ     'E'
043750060302     C                   MOVEL     VIDMSG        D48MSG
043760070308     C                   MOVEL     '4'           D48ERR
043770060302     C                   GOTO      ENDEM4
043780060302     C                   ENDIF
043790060302     c                   endif
043800941102     C*
043810941102     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
043820941102     C                   EXSR      REGIS4
043830991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
043840991130     C     *IN91         IFEQ      *ON
043850991130     C     D48FFR        ANDEQ     'E'
043860991130     C                   MOVEL     VIDMSG        D48MSG
043870991130     C                   MOVEL     '4'           D48ERR
043880991130     C                   GOTO      ENDEM4
043890991130     C                   ENDIF
043900941102     C   91              GOTO      FOR04
043910941102     C                   END
043920941102     C                   END
043930000303     C*
043940000303     C     ENDEM4        TAG
043950000303    1C     D48FFR        IFEQ      'E'
043960000303     C     *IN90         ANDEQ     *ON
043970000303    1C     D48FFR        OREQ      'E'
043980000303     C     *IN91         ANDEQ     *ON
043990000303     C     *IN10         IFEQ      *ON
044000000303     C                   UNLOCK    FNARB01L
044010051118     C                   UNLOCK    FiAR901L
044020000303     C                   ELSE
044030000303     C                   UNLOCK    FNBLP01L
044040000303     C                   ENDIF
044050000303     C                   UNLOCK    FIAR601L
044060000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
044070000303     C* flag D48F12 = 1
044080000303     C   KL              MOVEL     '1'           D48F12
044090000303    1C                   ENDIF
044100941102     C*
044110000303     C                   ENDSR
044120941102     C*
044130941102     C* EMETTO E GESTISCO FORMATO5 -----------------------------------*
044140941102     C     EMFOR5        BEGSR
044150950407     C*
044160941102     C     FOR05         TAG
044170941102     C*
044180950114     C     *IN90         IFEQ      *ON
044190941212     C     D48FFR        OREQ      'S'
044200950607     C     D48FFR        OREQ      'V'
044210941102     C                   WRITE     LR48T01
044220941102     C                   EXFMT     LR48D05
044230941102     C                   END
044240941102     C*
044250941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
044260941102     C                   SETOFF                                       289096
044270941102     C                   SETOFF                                       404142
044280991001     C                   SETOFF                                       434492
044290070329     C* F12 - RITORNO
044300941220    1C     *INKL         IFEQ      *OFF
044310070329     c
044320070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
044330070329     c                   if        *in87 and *inkr
044340070329     c                   exsr      CALLNOTELDV
044350070329     c                   goto      for05
044360070329     c                   endif
044370941102     C*
044380941102     C* CONTROLLO FORMATO
044390941213     C                   EXSR      CONTR5
044400941215     C*
044410941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044420941220    2C     *IN90         IFEQ      *ON
044430941215     C     D48FFR        ANDEQ     'E'
044440941215     C                   MOVEL     VIDMSG        D48MSG
044450941215     C                   MOVEL     '5'           D48ERR
044460941215     C                   GOTO      ENDEM5
044470941220    2C                   ENDIF
044480140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
044490140502     c*   (d48ffr='E' e 26 off)
044500140502     c  n90
044510140502     cann26              if        d48ffr='E'
044520140502     C                   GOTO      ENDEM5
044530140502     c                   endif
044540140502     c* Se errore forzabile ma pgm richiamato in modalità 'E' o 'F' cioè
044550140502     c* senza emissione di videata ignoro l'errore forzabile e permetto
044560140502     c* l'aggiornamento
044570140502     c   92              if        d48ffr='E'
044580140502     c                   setoff                                       92
044590140502     c                   endif
044600950607     C  NKF
044610950607     CANN26
044620950407     COR 90
044630950407     COR 92              GOTO      FOR05
044640060302     c* richiamo routine di richiesta/controllo motivazione
044650060302     c                   if        d66mtv = 'S' and w48mct = *blanks
044660060302     c                   exsr      ges_mtv
044670060302     c                   if        olv85f12 = '1'
044680060302     c                   if        ilv85wdw = ' '
044690060302     c                   goto      for05
044700060302     c                   else
044710060302     c                   eval      *inkl = '1'
044720060302     C                   GOTO      ENDEM5
044730060302     c                   endif
044740060302     c                   endif
044750060302     C     *IN90         IFEQ      *ON
044760060302     C     D48FFR        ANDEQ     'E'
044770060302     C                   MOVEL     VIDMSG        D48MSG
044780070308     C                   MOVEL     '5'           D48ERR
044790060302     C                   GOTO      ENDEM5
044800060302     C                   ENDIF
044810060302     c                   endif
044820941102     C*
044830941220     C                   EXSR      REGIS5
044840991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044850991130     C     *IN91         IFEQ      *ON
044860991130     C     D48FFR        ANDEQ     'E'
044870991130     C                   MOVEL     VIDMSG        D48MSG
044880991130     C                   MOVEL     '5'           D48ERR
044890991130     C                   GOTO      ENDEM5
044900991130     C                   ENDIF
044910941213     C   91              GOTO      FOR05
044920941220     C*
044930950518     C                   ENDIF
044940941222     C*
044950950518     C     ENDEM5        TAG
044960950518     C* F12-RITORNO  --> DISALLOCO RECROD
044970950518    1C     *INKL         IFEQ      *ON
044980950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044990950518     C     D48FFR        OREQ      'E'
045000950518     C     *IN90         ANDEQ     *ON
045010000303     C     D48FFR        OREQ      'E'
045020000303     C     *IN91         ANDEQ     *ON
045030950518     C*
045040950518    2C     *IN10         IFEQ      *ON
045050941220     C                   UNLOCK    FNARB01L
045060941222     C                   ELSE
045070941222     C                   UNLOCK    FNBLP01L
045080950518    2C                   END
045090051118     C                   UNLOCK    Fiar901L
045100991001     C                   UNLOCK    FIAR601L
045110950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
045120950607     C* flag D48F12 = 1
045130950607     C   KL              MOVEL     '1'           D48F12
045140941222    1C                   END
045150941102     C*
045160950518     C                   ENDSR
045170941102     C*
045180941102     C* EMISSIONE E GESTIONE FORMATO 7 ------------------------------*
045190941102     C     EMFOR7        BEGSR
045200941102     C*
045210941102     C     FOR07         TAG
045220941102     C*
045230950114     C     *IN90         IFEQ      *ON
045240941212     C     D48FFR        OREQ      'S'
045250950607     C     D48FFR        OREQ      'V'
045260941102     C                   WRITE     LR48T01
045270941102     C                   EXFMT     LR48D07
045280941102     C                   END
045290941102     C*
045300941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
045310941102     C                   SETOFF                                       2890
045320941102     C                   SETOFF                                       404142
045330941102     C                   SETOFF                                       434445
045340031001     c                   Setoff                                       62
045350070329     C* F12 - RITORNO
045360941102     C     *INKL         IFEQ      *OFF
045370070329     c
045380070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
045390070329     c                   if        *in87 and *inkr
045400070329     c                   exsr      CALLNOTELDV
045410070329     c                   goto      for07
045420070329     c                   endif
045430941102     C*
045440941102     C* CONTROLLO FORMATO
045450941102     C                   EXSR      CONTR7
045460040219      * questi controlli li devo saltare se il richiamo è stato fatto con causale 'CD'
045470040219      * cioè da chiusura distina
045480040224      * o causale 'CA' creata da LR48 in automatico
045490040224     c                   If        Not *In90 and D48Cvb <> 'CD' and
045500040224     c                             d48cvb <> 'CA'
045510040219     C                   ExSr      Contr72
045520040219     c                   EndIf
045530941215     C*
045540941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045550070309     c*          non se è forzabile solo allafine dopo la REGIS
045560941215     C     *IN90         IFEQ      *ON
045570941215     C     D48FFR        ANDEQ     'E'
045580070330     c**** Msgforza      andeq     0
045590941215     C                   MOVEL     VIDMSG        D48MSG
045600941215     C                   MOVEL     '7'           D48ERR
045610941215     C                   GOTO      ENDEM7
045620941215     C                   ENDIF
045630070330     c
045640070330     c* Se non ho avuto errori e richiamo solo per controlli, esco
045650070330     c*   (d48ffr='E' e 26 off)
045660070329     c  n90
045670070330     cann26              if        d48ffr='E'
045680070329     C                   GOTO      ENDEM7
045690070329     c                   endif
045700950607     C  NKF
045710950607     CANN26
045720941102     COR 90              GOTO      FOR07
045730060301     c* richiamo routine di richiesta/controllo motivazione
045740060301     c                   if        d66mtv = 'S' and w48mct = *blanks
045750060301     c                   exsr      ges_mtv
045760060301     c* premuto f12-Ritorno
045770060301     c                   if        olv85f12 = '1'
045780060301     c                   if        ilv85wdw = ' '
045790060301     c                   goto      for07
045800060301     c                   else
045810060301     c                   eval      *inkl = '1'
045820060301     C                   GOTO      ENDEM7
045830060301     c                   endif
045840060301     c                   endif
045850060301     c* Errore da ritornare a chiamante
045860060301     C     *IN90         IFEQ      *ON
045870060301     C     D48FFR        ANDEQ     'E'
045880060301     C                   MOVEL     VIDMSG        D48MSG
045890060301     C                   MOVEL     '7'           D48ERR
045900060301     C                   GOTO      ENDEM7
045910060301     C                   ENDIF
045920060301     c                   endif
045930941102     C*
045940070307     C* F6 - AGGIORNAMENTO
045950070307     c* Se devo scrivere file proposte, aggiorno solo FIARP
045960070307     c                   if        AggioARP='S'
045970070307     c                   EXSR      REGISARP
045980070307     c                   endif
045990070308     c
046000070308     c* Aggiorno comunque per gg chiusura e consegne particolari, se
046010070308     c*  modificate, in caso di scrittura di FIARP
046020070308     c                   if        AggioARP=' ' or
046030070308     c                             arbgc1<>vidgc1  or arbgc2<>vidgc2 or
046040151230     c                             arbtc1<>vidtc1 or arbtc2<>vidtc2
046050941102     C                   EXSR      REGIS7
046060070309     c                   else
046070160119     C   10              UNLOCK    FIAR601L
046080160119     C   10              UNLOCK    Fnarb01l
046090160119     C   10              UNLOCK    FiAR901L
046100160119     C                   UNLOCK    Fnblp01l
046110070308     c                   endif
046120070308     c
046130991130     C     *IN91         IFEQ      *ON
046140991130     C     D48FFR        ANDEQ     'E'
046150991130     C                   MOVEL     VIDMSG        D48MSG
046160991130     C                   MOVEL     '7'           D48ERR
046170991130     C                   GOTO      ENDEM7
046180991130     C                   ENDIF
046190991130     C   91              GOTO      FOR07
046200950519     C                   ENDIF
046210070309     c
046220070309     c* Se non ho passato msg vincolanti, passo msg di avvertimento
046230070309     c*  se ce ne sono
046240070309     c                   if        msgforza>0
046250070309     c                   z-aDD     MSGFORZA      XX
046260070309     C                   MOVEL     MSG(XX)       D48MSG
046270070309     C                   MOVEL     'Z'           D48ERR
046280070309     c                   clear                   msgforza
046290070309     C                   ENDIF
046300950519     C*
046310950519     C     ENDEM7        TAG
046320070309     C* F12-RITORNO  --> DISALLOCO REcord
046330941222     C*
046340950519    1C     *INKL         IFEQ      *ON
046350950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046360950519     C     D48FFR        OREQ      'E'
046370950519     C     *IN90         ANDEQ     *ON
046380000303     C     D48FFR        OREQ      'E'
046390000303     C     *IN91         ANDEQ     *ON
046400070329     C     D48FFR        OREQ      'E'
046410070329     c     *in26         andeq     *off
046420950519     C*
046430950519    2C     *IN10         IFEQ      *ON
046440941220     C                   UNLOCK    FNARB01L
046450991005     C                   UNLOCK    FIAR601L
046460051118     C                   UNLOCK    FiAR901L
046470160119     c                   endif
046480160119     C***                ELSE
046490941222     C                   UNLOCK    FNBLP01L
046500160119    2C***                END
046510950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
046520950607     C* flag D48F12 = 1
046530950607     C   KL              MOVEL     '1'           D48F12
046540950519    1C                   END
046550941102     C*
046560950519     C                   ENDSR
046570021129      **
046580021129      * EMISSIONE E GESTIONE FORMATO 7/1 Supermercati ---------------*
046590021129     c     Emfor71       BegSr
046600021129
046610021129     c     For071        Tag
046620021129
046630021129     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
046640021129     c                   Write     Lr48T01
046650021129     c                   Exfmt     Lr48D071
046660021129     c                   EndIf
046670021129
046680021129      * Spengo idnicatori di posizionamento cursore
046690021129     c                   Setoff                                       2890
046700021129     c                   Setoff                                       404142
046710021129     c                   Setoff                                       434445
046720021129     c                   Setoff                                       575859
046730031001     c                   Setoff                                       62
046740070329      * F12 - RITORNO
046750021129     c                   IF        Not *InKl
046760070329     c
046770070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
046780070329     c                   if        *in87 and *inkr
046790070329     c                   exsr      CALLNOTELDV
046800070329     c                   goto      for071
046810070329     c                   endif
046820021129
046830021129      * CONTROLLO FORMATO
046840021129     c                   ExSr      Contr7
046850021129
046860021129     c  n90              ExSr      Contr71
046870031028     C  n90              ExSr      Contr72
046880070306     c
046890070306     c* Nel caso dei supermercati devo sempre aggiornare la bolla
046900070306     c*  e mai il file di proposta FIARP per sicurezza lo pulisco
046910070306     c                   clear                   aggioarp
046920070308     c                   clear                   savdcr
046930070308     c                   clear                   savTcr
046940070308     c                   clear                   savHcr
046950070306     c
046960040330      * La data non può superare i gg limiti indicati in tab. VPO
046970040330     c                   If        Not *In90
046980040330     c                   Eval      wdataiso = wdataoggi
046990040330     c                   Adddur    §vpodcr:*d    wdataiso
047000040330     c     *iso          move      wdataiso      ds_data
047010040330     c                   Move      §vpodcr       w003a
047020040330     c                   Movea     w003a         ski(13)
047030040330     c                   Z-add     13            i
047040040330     c                   ExSr      abbl
047050040330     c                   Movea     ski(13)       w003a
047060040330     c                   If        ComDcr > ds_data
047070040330     c                   Eval      *In28 = *On
047080040330     c                   Eval      *In40 = *On
047090040330     c                   Eval      *In90 = *On
047100040330     c                   Eval      VidMsg = msg(129)
047110040330     c                   Eval      VidMsg = %replace(w003a:VidMsg:
047120040330     c                                      %scan('___':VidMsg))
047130040330     c                   EndIf
047140040330     c                   EndIf
047150021129
047160021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047170021129     c                   If        *In90 and D48Ffr = 'E'
047180021129     c                   Eval      D48Msg = VidMsg
047190021129     c                   Eval      D48Err = '7'
047200021129     c                   GoTo      EndEm71
047210021129     c                   EndIf
047220140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
047230140502     c*   (d48ffr='E' e 26 off)
047240140502     c  n90
047250140502     cann26              if        d48ffr='E'
047260140502     C                   GOTO      ENDEM71
047270140502     c                   endif
047280021129
047290021129     c  NKF
047300021129     cANN26
047310021129     cOR 90              GoTo      For071
047320060302     c* richiamo routine di richiesta/controllo motivazione
047330060302     c                   if        d66mtv = 'S' and w48mct = *blanks
047340060302     c                   exsr      ges_mtv
047350060302     c                   if        olv85f12 = '1'
047360060302     c                   if        ilv85wdw = ' '
047370060302     c                   goto      for071
047380060302     c                   else
047390060302     c                   eval      *inkl = '1'
047400060302     C                   GOTO      ENDEM71
047410060302     c                   endif
047420060302     c                   endif
047430060302     C     *IN90         IFEQ      *ON
047440060302     C     D48FFR        ANDEQ     'E'
047450060302     C                   MOVEL     VIDMSG        D48MSG
047460070308     C                   MOVEL     '7'           D48ERR
047470060302     C                   GOTO      ENDEM71
047480060302     C                   ENDIF
047490060302     c                   endif
047500021129
047510021129      * CMD6 - AGGIORNAMENTO
047520021129     c                   ExSr      Regis7
047530021129
047540021129     c  n91              ExSr      Regis71
047550021129
047560021129     c                   If        *In91 and D48Ffr = 'E'
047570021129     c                   Eval      D48Msg = VidMsg
047580021129     c                   Eval      D48Err = '7'
047590021129     c                   GoTo      EndEm71
047600021129     c                   EndIf
047610021129
047620021129     c   91              GoTo      For071
047630021129
047640021129     c                   EndIf
047650021129
047660021129     c     EndEm71       Tag
047670021129
047680021129      * F12-RITORNO  --> DISALLOCO RECROD
047690021129
047700021129    1c     *INKL         IFEQ      *ON
047710021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047720021129     c     D48FFR        OREQ      'E'
047730021129     c     *IN90         ANDEQ     *ON
047740021129     c     D48FFR        OREQ      'E'
047750021129     c     *IN91         ANDEQ     *ON
047760021129
047770021129    2c     *IN10         IFEQ      *ON
047780021129     c                   UNLOCK    FNARB01L
047790021129     c                   UNLOCK    FIAR601L
047800051118     c                   UNLOCK    FiAR901L
047810021129     c                   ELSE
047820021129     c                   UNLOCK    FNBLP01L
047830021129    2c                   END
047840021129      * Comunico al chiamante che è stato premuto F12 valorizzando il
047850021129      * flag D48F12 = 1
047860021129     c   KL              MOVEL     '1'           D48F12
047870021129    1c                   END
047880021129
047890021129     c                   EndSr
047900941223     C*
047910941223     C* EMISSIONE E GESTIONE FORMATO 8 ------------------------------*
047920941223     C     EMFOR8        BEGSR
047930941223     C*
047940941223     C     FOR08         TAG
047950941223     C*
047960950114     C     *IN90         IFEQ      *ON
047970941223     C     D48FFR        OREQ      'S'
047980950607     C     D48FFR        OREQ      'V'
047990941223     C                   WRITE     LR48T03
048000941223     C                   EXFMT     LR48D08
048010941223     C                   END
048020941223     C*
048030941223     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
048040941223     C                   SETOFF                                       2890
048050941223     C                   SETOFF                                       404142
048060941223     C                   SETOFF                                       434445
048070941223     C                   SETOFF                                       464748
048080941223     C                   SETOFF                                       4950
048090941223     C* CMD12 - RITORNO
048100941223     C     *INKL         IFEQ      *OFF
048110941223     C*
048120941223     C* CONTROLLO FORMATO
048130941223     C                   EXSR      CONTR8
048140941223     C*
048150941223     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048160941223     C     *IN90         IFEQ      *ON
048170941223     C     D48FFR        ANDEQ     'E'
048180941223     C                   MOVEL     VIDMSG        D48MSG
048190941223     C                   MOVEL     '8'           D48ERR
048200941223     C                   GOTO      ENDEM8
048210941223     C                   ENDIF
048220140502
048230140502     c* Non gestita uscita per d48ffr='F' ( n90 and n26 d48ffr='E')
048240140502     c* perchè bisognerebbe verificare bene cosa fare in questo caso
048250140502
048260970110     C  NKF
048270970110     CANNKJ
048280970110     CANN26
048290941223     COR 90              GOTO      FOR08
048300060302     c* richiamo routine di richiesta/controllo motivazione
048310060302     c                   if        d66mtv = 'S' and w48mct = *blanks
048320060302     c                   exsr      ges_mtv
048330060302     c                   if        olv85f12 = '1'
048340060302     c                   if        ilv85wdw = ' '
048350060302     c                   goto      for08
048360060302     c                   else
048370060302     c                   eval      *inkl = '1'
048380060302     C                   GOTO      ENDEM8
048390060302     c                   endif
048400060302     c                   endif
048410060302     C     *IN90         IFEQ      *ON
048420060302     C     D48FFR        ANDEQ     'E'
048430060302     C                   MOVEL     VIDMSG        D48MSG
048440070308     C                   MOVEL     '8'           D48ERR
048450060302     C                   GOTO      ENDEM8
048460060302     C                   ENDIF
048470060302     c                   endif
048480941223     C*
048490970110     C* F6 O F10 - AGGIORNAMENTO
048500941223     C                   EXSR      REGIS8
048510991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048520991130     C     *IN91         IFEQ      *ON
048530991130     C     D48FFR        ANDEQ     'E'
048540991130     C                   MOVEL     VIDMSG        D48MSG
048550991130     C                   MOVEL     '8'           D48ERR
048560991130     C                   GOTO      ENDEM8
048570991130     C                   ENDIF
048580941223     C   91              GOTO      FOR08
048590941223     C*
048600950519     C                   ENDIF
048610150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
048620150112     c*  se ce ne sono
048630150112     c                   if        msgforzatxt<>*blanks
048640150112     C                   MOVEL     msgforzatxt   D48MSG
048650150112     C                   MOVEL     'Z'           D48ERR
048660150112     c                   clear                   msgforzatxt
048670150112     C                   ENDIF
048680950519     C*
048690950519     C     ENDEM8        TAG
048700950104     C* F12-RITORNO  --> DISALLOCO RECORD
048710950519    1C     *INKL         IFEQ      *ON
048720950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048730950519     C     D48FFR        OREQ      'E'
048740950519     C     *IN90         ANDEQ     *ON
048750000303     C     D48FFR        OREQ      'E'
048760000303     C     *IN91         ANDEQ     *ON
048770941223     C*
048780941223     C     *IN10         IFEQ      *ON
048790941223     C                   UNLOCK    FNARB01L
048800991005     C                   UNLOCK    FIAR601L
048810051118     C                   UNLOCK    FiAR901L
048820941223     C                   ELSE
048830941223     C                   UNLOCK    FNBLP01L
048840941223     C                   END
048850950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
048860950607     C* flag D48F12 = 1
048870950607     C   KL              MOVEL     '1'           D48F12
048880941223     C                   END
048890941223     C*
048900950519     C                   ENDSR
048910961211     C* EMISSIONE E GESTIONE FORMATO 9 ------------------------------*
048920961211     C     EMFOR9        BEGSR
048930920120     C*
048940961218     C     FOR09         TAG
048950961218     C*
048960961218     C     *IN90         IFEQ      *ON
048970961218     C     D48FFR        OREQ      'S'
048980961218     C     D48FFR        OREQ      'V'
048990961219     C                   WRITE     LR48T01
049000961218     C                   EXFMT     LR48D09
049010961218     C                   END
049020961218     C*
049030961218     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
049040961218     C                   SETOFF                                       2890
049050970110     C                   SETOFF                                       4041
049060070329     C* F12 - RITORNO
049070961218     C     *INKL         IFEQ      *OFF
049080070329     c
049090070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
049100070329     c                   if        *in87 and *inkr
049110070329     c                   exsr      CALLNOTELDV
049120070329     c                   goto      for09
049130070329     c                   endif
049140070329     C*
049150961218     C*
049160961218     C* CONTROLLO FORMATO
049170961218     C                   EXSR      CONTR9
049180961218     C*
049190961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049200961218     C     *IN90         IFEQ      *ON
049210961218     C     D48FFR        ANDEQ     'E'
049220961218     C                   MOVEL     VIDMSG        D48MSG
049230961218     C                   MOVEL     '9'           D48ERR
049240961218     C                   GOTO      ENDEM9
049250961218     C                   ENDIF
049260140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
049270140502     c*   (d48ffr='E' e 26 off)
049280140502     c  n90
049290140502     cann26              if        d48ffr='E'
049300140502     C                   GOTO      ENDEM9
049310140502     c                   endif
049320970110     C  NKF
049330970110     CANN26
049340961218     COR 90              GOTO      FOR09
049350060302     c* richiamo routine di richiesta/controllo motivazione
049360060302     c                   if        d66mtv = 'S' and w48mct = *blanks
049370060302     c                   exsr      ges_mtv
049380060302     c                   if        olv85f12 = '1'
049390060302     c                   if        ilv85wdw = ' '
049400060302     c                   goto      for09
049410060302     c                   else
049420060302     c                   eval      *inkl = '1'
049430060302     C                   GOTO      ENDEM9
049440060302     c                   endif
049450060302     c                   endif
049460060302     C     *IN90         IFEQ      *ON
049470060302     C     D48FFR        ANDEQ     'E'
049480060302     C                   MOVEL     VIDMSG        D48MSG
049490060316     C                   MOVEL     '9'           D48ERR
049500060302     C                   GOTO      ENDEM9
049510060302     C                   ENDIF
049520060302     c                   endif
049530060316     c                   exsr      regis9
049540060316     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049550060316     C     *IN91         IFEQ      *ON
049560060316     C     D48FFR        ANDEQ     'E'
049570060316     C                   MOVEL     VIDMSG        D48MSG
049580060316     C                   MOVEL     '9'           D48ERR
049590060316     C                   GOTO      ENDEM9
049600060316     C                   ENDIF
049610060316     C   91              GOTO      FOR09
049620041028     c
049630961218     C*
049640961218     C                   ENDIF
049650961218     C*
049660961218     C     ENDEM9        TAG
049670961218     C* F12-RITORNO  --> DISALLOCO RECORD
049680961218    1C     *INKL         IFEQ      *ON
049690961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049700961218     C     D48FFR        OREQ      'E'
049710961218     C     *IN90         ANDEQ     *ON
049720961218     C*
049730961218     C     *IN10         IFEQ      *ON
049740961218     C                   UNLOCK    FNARB01L
049750991005     C                   UNLOCK    FIAR601L
049760051118     C                   UNLOCK    FiAR901L
049770961218     C                   ELSE
049780961218     C                   UNLOCK    FNBLP01L
049790961218     C                   END
049800961218     C* Comunico al chiamante che è stato premuto F12 valorizzando il
049810961218     C* flag D48F12 = 1
049820961218     C   KL              MOVEL     '1'           D48F12
049830961218     C                   END
049840961211     C*
049850961211     C                   ENDSR
049860030123      **
049870030123      * EMISSIONE E GESTIONE FORMATO 10 Mancato ritiro BANCALI ------*
049880030123     c     Emfor10       BegSr
049890030123
049900030123     c     For10         Tag
049910030123
049920030123     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
049930030123     c                   Write     Lr48T01
049940030123     c                   Exfmt     Lr48D10
049950030123     c                   EndIf
049960030123
049970030123      * Spengo indicatori di posizionamento cursore
049980030123     c                   Setoff                                       2890
049990030124     c                   Setoff                                       5759
050000030124     c                   Setoff                                       6061
050010070329      * F12 - RITORNO
050020030123     c                   IF        Not *InKl
050030070329     c
050040070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
050050070329     c                   if        *in87 and *inkr
050060070329     c                   exsr      CALLNOTELDV
050070070329     c                   goto      for10
050080070329     c                   endif
050090030123
050100030123      * CONTROLLO FORMATO
050110030123     c                   ExSr      Contr10
050120030123
050130030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050140030123     c                   If        *In90 and D48Ffr = 'E'
050150030123     c                   Eval      D48Msg = VidMsg
050160030123     c                   Eval      D48Err = 'B'
050170030123     c                   GoTo      EndEm10
050180030123     c                   EndIf
050190140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050200140502     c*   (d48ffr='E' e 26 off)
050210140502     c  n90
050220140502     cann26              if        d48ffr='E'
050230140502     C                   GOTO      ENDEM10
050240140502     c                   endif
050250030123
050260030123     c  NKF
050270030123     cANN26
050280030123     cOR 90              GoTo      For10
050290060302     c* richiamo routine di richiesta/controllo motivazione
050300060302     c                   if        d66mtv = 'S' and w48mct = *blanks
050310060302     c                   exsr      ges_mtv
050320060302     c                   if        olv85f12 = '1'
050330060302     c                   if        ilv85wdw = ' '
050340060302     c                   goto      for10
050350060302     c                   else
050360060302     c                   eval      *inkl = '1'
050370060302     C                   GOTO      ENDEM10
050380060302     c                   endif
050390060302     c                   endif
050400060302     C     *IN90         IFEQ      *ON
050410060302     C     D48FFR        ANDEQ     'E'
050420060302     C                   MOVEL     VIDMSG        D48MSG
050430070308     C                   MOVEL     'B'           D48ERR
050440060302     C                   GOTO      ENDEM10
050450060302     C                   ENDIF
050460060302     c                   endif
050470030123
050480030123      * CMD6 - AGGIORNAMENTO
050490030123     c                   ExSr      Regis10
050500030123
050510030123     c                   If        *In91 and D48Ffr = 'E'
050520030123     c                   Eval      D48Msg = VidMsg
050530030123     c                   Eval      D48Err = 'B'
050540030123     c                   GoTo      EndEm10
050550030123     c                   EndIf
050560030123
050570030123     c   91              GoTo      For10
050580030123
050590030123     c                   EndIf
050600030123
050610030123     c     EndEm10       Tag
050620030123
050630030123      * F12-RITORNO  --> DISALLOCO RECROD
050640030123
050650030123    1c     *INKL         IFEQ      *ON
050660030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050670030123     c     D48FFR        OREQ      'E'
050680030123     c     *IN90         ANDEQ     *ON
050690030123     c     D48FFR        OREQ      'E'
050700030123     c     *IN91         ANDEQ     *ON
050710030123
050720030123    2c     *IN10         IFEQ      *ON
050730030123     c                   UNLOCK    FNARB01L
050740030123     c                   UNLOCK    FIAR601L
050750051118     c                   UNLOCK    FiAR901L
050760030124     c                   UNLOCK    Fiar501L
050770030123     c                   ELSE
050780030123     c                   UNLOCK    FNBLP01L
050790030123    2c                   END
050800030123      * Comunico al chiamante che è stato premuto F12 valorizzando il
050810030123      * flag D48F12 = 1
050820030123     c   KL              MOVEL     '1'           D48F12
050830030123    1c                   END
050840030123
050850030123     c                   EndSr
050860061107     C
050870061107     C*--- EMISSIONE E GESTIONE FORMATO 11 ---------------------------*
050880061107     C     EMFOR11       BEGSR
050890061107     C*
050900061107     C     FOR11         TAG
050910061107     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
050920061107    1C     *IN90         IFEQ      *ON
050930061107     C     D48FFR        OREQ      'S'
050940061107     C     D48FFR        OREQ      'V'
050950061107     C                   WRITE     LR48T01
050960061107     C                   EXFMT     LR48D11
050970061107    1C                   END
050980061107     C*
050990061107     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051000061107     C                   SETOFF                                       2890
051010061107     C                   SETOFF                                       4041
051020061107     C* F12 - RITORNO
051030061107    1C     *INKL         IFEQ      *OFF
051040061107     C*
051050061107     C* CONTROLLO FORMATO
051060061107     C                   EXSR      CONTR11
051070061107     C*
051080061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051090061107    3C     *IN90         IFEQ      *ON
051100061107     C     D48FFR        ANDEQ     'E'
051110061107     C                   MOVEL     VIDMSG        D48MSG
051120061107     C                   MOVEL     'F'           D48ERR
051130061107     C                   GOTO      ENDEM11
051140061107    3C                   ENDIF
051150071025     c
051160071025     c* Se non ho avuto errori e richiamo solo per controlli, esco
051170071025     c*   (d48ffr='E' e 26 off)
051180071025     c  n90
051190071025     cann26              if        d48ffr='E'
051200071025     C                   GOTO      ENDEM11
051210071025     c                   endif
051220061107     C  NKF
051230061107     CANN26
051240061107     COR 90              GOTO      FOR11
051250061107     c* richiamo routine di richiesta/controllo motivazione
051260061107     c                   if        d66mtv = 'S' and w48mct = *blanks
051270061107     c                   exsr      ges_mtv
051280061107     c                   if        olv85f12 = '1'
051290061107     c                   if        ilv85wdw = ' '
051300061107     c                   goto      for11
051310061107     c                   else
051320061107     c                   eval      *inkl = '1'
051330061107     C                   GOTO      ENDEM11
051340061107     c                   endif
051350061107     c                   endif
051360061107     C     *IN90         IFEQ      *ON
051370061107     C     D48FFR        ANDEQ     'E'
051380061107     C                   MOVEL     VIDMSG        D48MSG
051390061107     C                   MOVEL     'F'           D48ERR
051400061107     C                   GOTO      ENDEM11
051410061107     C                   ENDIF
051420061107     c                   endif
051430061107     C*
051440071026     c                   clear                   Aggiobol
051450061107     C                   EXSR      REGIS11
051460061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051470061107     C     *IN91         IFEQ      *ON
051480061107     C     D48FFR        ANDEQ     'E'
051490061107     C                   MOVEL     VIDMSG        D48MSG
051500061107     C                   MOVEL     'F'           D48ERR
051510061107     C                   GOTO      ENDEM11
051520061107     C                   ENDIF
051530061107     C   91              GOTO      FOR11
051540061107     C*
051550061107    1C                   ENDIF
051560061107     C*
051570061107     C     ENDEM11       TAG
051580061107     C*
051590061107    1C     *INKL         IFEQ      *ON
051600061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051610061107    1C     D48FFR        OREQ      'E'
051620061107     C     *IN90         ANDEQ     *ON
051630061107    1C     D48FFR        OREQ      'E'
051640061107     C     *IN91         ANDEQ     *ON
051650061107     C* F12-RITORNO
051660061107    2C     *IN10         IFEQ      *ON
051670061107     C                   UNLOCK    FNARB01L
051680061107     C                   UNLOCK    FiAR901L
051690061107     C                   ELSE
051700061107     C                   UNLOCK    FNBLP01L
051710061107    2C                   END
051720061107     C* Comunico al chiamante che è stato premuto F12 valorizzando il
051730061107     C* flag D48F12 = 1
051740061107     C   KL              MOVEL     '1'           D48F12
051750061107    1C                   END
051760061107     c
051770061107     C                   UNLOCK    Fiar601l
051780061107     C*
051790061107     C                   ENDSR
051800130927     c
051810130927     C*--- EMISSIONE E GESTIONE FORMATO 12  variaz.'"Nx' -------------*
051820130927     C     EMFOR12       BEGSR
051830130927     C*
051840130927     C     FOR12         TAG
051850130927     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
051860130927    1C     *IN90         IFEQ      *ON
051870130927     C     D48FFR        OREQ      'S'
051880130927     C     D48FFR        OREQ      'V'
051890130927     C                   WRITE     LR48T01
051900130927     C                   EXFMT     LR48D12
051910130927    1C                   END
051920130927     C*
051930130927     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051940130927     C                   SETOFF                                       2890
051950130930     C                   SETOFF                                       41
051960130927     C* F12 - RITORNO
051970130927    1C     *INKL         IFEQ      *OFF
051980130927     c
051990130927     c* F17 - Note LDV   (solo se abilitato geo 87 on)
052000130927     c                   if        *in87 and *inkr
052010130927     c                   exsr      CALLNOTELDV
052020130927     c                   goto      for12
052030130927     c                   endif
052040130927     C*
052050130927     C* CONTROLLO FORMATO
052060130927     C                   EXSR      CONTR12
052070130927     C*
052080130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052090130927    3C     *IN90         IFEQ      *ON
052100130927     C     D48FFR        ANDEQ     'E'
052110130927     C                   MOVEL     VIDMSG        D48MSG
052120130927     C                   MOVEL     'F'           D48ERR
052130130927     C                   GOTO      ENDEM12
052140130927    3C                   ENDIF
052150130927     c
052160130927     c* Se non ho avuto errori e richiamo solo per controlli, esco
052170130927     c*   (d48ffr='E' e 26 off)
052180130927     c  n90
052190130927     cann26              if        d48ffr='E'
052200130927     C                   GOTO      ENDEM12
052210130927     c                   endif
052220130927     C  NKF
052230130927     CANN26
052240130930     COR 90              GOTO      FOR12
052250130927     c* richiamo routine di richiesta/controllo motivazione
052260130927     c                   if        d66mtv = 'S' and w48mct = *blanks
052270130927     c                   exsr      ges_mtv
052280130927     c                   if        olv85f12 = '1'
052290130927     c                   if        ilv85wdw = ' '
052300130927     c                   goto      for12
052310130927     c                   else
052320130927     c                   eval      *inkl = '1'
052330130927     C                   GOTO      ENDEM12
052340130927     c                   endif
052350130927     c                   endif
052360130927     C     *IN90         IFEQ      *ON
052370130927     C     D48FFR        ANDEQ     'E'
052380130927     C                   MOVEL     VIDMSG        D48MSG
052390130927     C                   MOVEL     'F'           D48ERR
052400130927     C                   GOTO      ENDEM12
052410130927     C                   ENDIF
052420130927     c                   endif
052430130927     C*
052440130927     c                   clear                   Aggiobol
052450130927     C                   EXSR      REGIS12
052460130927     c
052470130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052480130927     C     *IN91         IFEQ      *ON
052490130927     C     D48FFR        ANDEQ     'E'
052500130927     C                   MOVEL     VIDMSG        D48MSG
052510130927     C                   MOVEL     'F'           D48ERR
052520130927     C                   GOTO      ENDEM12
052530130927     C                   ENDIF
052540130927     C   91              GOTO      FOR12
052550130927     C*
052560130927    1C                   ENDIF
052570130927     C*
052580130927     C     ENDEM12       TAG
052590130927     C*
052600130927    1C     *INKL         IFEQ      *ON
052610130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052620130927    1C     D48FFR        OREQ      'E'
052630130927     C     *IN90         ANDEQ     *ON
052640130927    1C     D48FFR        OREQ      'E'
052650130927     C     *IN91         ANDEQ     *ON
052660130927     C* F12-RITORNO
052670130927    2C     *IN10         IFEQ      *ON
052680130927     C                   UNLOCK    FNARB01L
052690130927     C                   UNLOCK    FiAR901L
052700130927     C                   ELSE
052710130927     C                   UNLOCK    FNBLP01L
052720130927    2C                   END
052730130927     C* Comunico al chiamante che è stato premuto F12 valorizzando il
052740130927     C* flag D48F12 = 1
052750130927     C   KL              MOVEL     '1'           D48F12
052760130927    1C                   END
052770130927     c
052780130927     C                   UNLOCK    Fiar601l
052790130927     C*
052800130927     C                   ENDSR
052810070329     C
052820070329     C*--- richiama pgm per immettere note LDV -----------------------*
052830070329     C     CALLNoteLDV   BEGSR
052840140130     c                   clear                   fnlrn6ds
052850140130     c                   eval      in6mod='I'
052860140130     c                   eval      in6aas=arbaas
052870140130     c                   eval      in6lnp=arblnp
052880140130     c                   eval      in6nrs=arbnrs
052890140130     c                   eval      in6nsp=arbnsp
052900140130     c                   eval      in6pgm='FNLR48R'
052910140130     c                   eval      IN6FGS=arbifp
052920140130     c                   eval      IN6NFV=arbndc
052930140130     c                   eval      IN6DFV=arbddc
052940140130     c                   eval      IN6PDR=arbpdc
052950140130     c                   if        arbdcm=0  and arbndc>0
052960140130     c                   eval      IN6CONS='S'
052970140130     c                   endif
052980140130     c                   call      'FNLRN6R'
052990070329     c                   parm                    kpjba
053000140130     c                   parm                    fnlrn6ds
053010070329     c
053020070329     c                   ENDSR
053030061107     C
053040920120     C*--- CONTROLLI FORMATO2 ----------------------------------------*
053050920120     C     CONTR2        BEGSR
053060941025     C                   SETOFF                                       9028
053070020531     c                   clear                   wdisag
053080030122      * A T T E N Z I O N E!!!
053090030122      * Per la modifica destinatario
053100030122      * Se il nuovo destinatario necessita della consegna particolare
053110030122      * alla fine della REGIS2 viene impostata la causae 'CP' per
053120030122      * poter effettuare una nuova variazione bolla senza uscire dal pgm
053130941020     C***
053140941020     C*** DESTINATARIO  ***
053150941025    1C     *IN01         IFEQ      '1'
053160141112
053170141112     c* messaggio di avviso se pgm non richiamato e bolla con
053180141120     c* giacenza aperta  - Solo se variati dati del destinatario
053190141112     c                   if        *in20=*off and wforzgiac=0
053200141112     c                             and *in04
053210141120    5C     SAVCAD        IFNE      VIDCAD
053220141120     C     SAVLOD        ORNE      VIDLOD
053230141120     C     SAVind        ORNE      VIDind
053240141120     C     SAVprd        ORNE      VIDprd
053250141120     C     SAVnzd        ORNE      VIDnzd
053260141112     c                   eval      wforzgiac=1
053270141112     C                   SETON                                        2890
053280141112     C                   MOVEL     MSG(157)      VIDMSG
053290141112     C                   GOTO      ENDCT2
053300141112     c                   endif
053310141120     c                   endif
053320141112
053330970808     C                   MOVEL     VIDRSD        I14RSC
053340970808     C                   MOVE      VIDRSD        I14RS2
053350970808     C                   MOVEL     VIDIND        I14IND
053360970808     C                   MOVEL     VIDLOD        E14LOC
053370970808     C                   MOVEL     VIDPID        E14PIP
053380970808     C                   MOVEL     VIDPRD        E14PRV
053390970808     C                   MOVEL     VIDNZD        E14NAR
053400970808     C                   MOVEL     VIDCAD        E14CAP
053410970808     C                   MOVEL     VIDFIN        I14ISO
053420970808     C                   MOVEL     'D'           WCLI              1
053430970808     C                   EXSR      CTRIND
053440941020     C*
053450941020     C* CAMPI CHE POSSO AVER IMPOSTATO
053460970808     C                   MOVEL     E14LOC        VIDLOD
053470970808     C                   MOVEL     E14PIP        VIDPID
053480970808     C                   MOVEL     E14PRV        VIDPRD
053490970808     C                   MOVEL     E14NAR        VIDNZD
053500970808     C                   MOVEL     E14CAP        VIDCAD
053510970808     C* ERRORE
053520970808     C   90              GOTO      ENDCT2
053530060111     c* salvo lna desunta da cappario per non sporcarla se destinatario
053540060111     c* che richiede una particolare lna diversa da quella del cappario
053550060111     c                   move      o95lna        d_o95lna
053560020531     c* Verifico se destinatario disagiato
053570020604     c* Per bolle poste non faccio niente
053580060111     c                   clear                   ddstflo
053590020604     c     lnpntw        ifne      'PPT'
053600040702      * Referente e numero di telefono: se campi a video con '#'
053610040702      * sono quelli automatici (dal tisit5) e quindi li pulisco
053620040702     c                   If        %subst(VidRef:1:1) = '#'
053630040702     c                   Clear                   VidRef
053640040702     c                   EndIf
053650040705     c                   If        %subst(VidTeld:1:1) = '#'
053660040702     c                   Clear                   VidTeld
053670040702     c                   EndIf
053680020531     c                   clear                   tisit0ds
053690020531     c                   movel     'E'           it0tla
053700020531     c                   movel     vidnzd        it0naz
053710020531     c                   movel     vidprd        it0prv
053720020531     c                   movel     vidcad        it0cap
053730020531     c                   movel     vidlod        it0loc
053740020531     c                   movel     vidind        it0ind
053750020531     c                   movel     vidrsd        it0rag
053760020531     c                   call      'TISIT5R'
053770020531     c                   parm                    tisit0ds
053780020531     c     ot0err        ifeq      *blanks
053790060113     c                   movel     ot0flo        ddstflo
053800060111   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
053810060111     c                             ot0dos = 'S'
053820020531     c                   movel     ot0dos        wdisag
053830020531     c     wdisag        ifeq      'D'
053840020531     c                   movel     'X'           wdisag
053850020531     c                   endif
053860060111     c                   endif
053870020531     c                   else
053880020531     c                   clear                   wdisag
053890020531     c                   endif
053900060327     c***                If        wdisag <> *Blanks
053910060327     c***                          and %subst(arbgva:1:1) <> 'U' or
053920060327     c***                          wdisag='X' and %subst(arbgva:1:1)='U'
053930060327     c                   if        (wdisag = 'X') OR
053940060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
053950060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
053960060327     c                             (arbdcr=*zeros or arbtcr <> *blanks))
053970040702     c                   If        VidTeld = *Blanks
053980040702     c                   Eval      VidTeld = ot0tel
053990040702     c                   EndIf
054000040702     c                   If        VidRef = *Blanks
054010040702     c                   Eval      VidRef =  ot0ref
054020040702     c                   EndIf
054030040702     c                   EndIf
054040020604     c                   endif
054050060113     c*
054060060113     c                   if        §dstlna > *zeros
054070060111     c                   move      §dstlna       d_o95lna
054080060111     c                   endif
054090970808     C*
054100970808     C* MODIFICATI CAP E/O LOCALITA':
054110970808     C* 1) AGGIORNO A VIDEO IL FLAG INOLTRO E AVVISO CON MESSAGGIO
054120970808     C* 2) SE VARIA ANCHE LA LNA E CAP E LOCALITA SONO DIVERSI DALLA
054130970808     C*    BOLLA EMMETTO MESSAGGIO FORZABILE MANTENENDO COMUNQUE LA
054140970808     C*    LINEA DI ARRIVO E ZONA DELLA BOLLA
054150970808     C     WCAD          IFNE      VIDCAD
054160970808     C     WLOD          ORNE      VIDLOD
054170970808     C                   MOVE      VIDCAD        WCAD
054180970808     C                   MOVE      VIDLOD        WLOD
054190970808     C* FLAG INOLTRO
054200970808     C     O95ISO        IFNE      VIDFIN
054210970808     C                   MOVE      O95ISO        VIDFIN
054220140507     c* NON avviso se richiamo cieco
054230140507     c                   if        d48ffr<>'E'
054240970808     C                   SETON                                        289049
054250970808     C                   MOVEL     MSG(77)       VIDMSG
054260970808     C                   GOTO      ENDCT2
054270140507     c                   endif
054280970808     C                   END
054290970808     C                   END
054300970828     C* FLAG INOLTRO
054310970828     C     VIDFIN        IFEQ      ' '
054320970828     C                   MOVE      O95ISO        VIDFIN
054330140507     c* NON avviso se richiamo cieco
054340140507     c                   if        d48ffr<>'E'
054350970828     C                   SETON                                        289049
054360970828     C                   MOVEL     MSG(77)       VIDMSG
054370010412     C                   GOTO      ENDCT2
054380140507     c                   endif
054390970828     C                   END
054400010412     C  N29
054410010412     CANN71VIDFIN        IFEQ      'D'
054420010412     C     O95PID        ANDEQ     'N'
054430010412     C                   SETON                                        289049
054440010412     C                   MOVEL     MSG(98)       VIDMSG
054450010412     C                   GOTO      ENDCT2
054460010412     C                   ENDIF
054470970808     C*
054480140507     c* Non faccio il seguente controllo se richiamo "cieco"
054490141029     c                   if        %lookup('999':skFilOkdir)=0 and
054500141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)=0
054510141029     c                             or %lookup(%editc(dutpou:'X'):skFilOkdir)=0)
054520141029     c                   if        d48ffr<>'E'
054530141029     C     WCAD1         IFNE      VIDCAD
054540141029     C     WLOD1         ORNE      VIDLOD
054550141029     C     Wrsd1         ORNE      VIDrsd
054560141029     C     Wprd1         ORNE      VIDprd
054570141029     C     Wnzd1         ORNE      VIDnzd
054580141029     C     Wind1         ORNE      VIDind
054590141029     C                   MOVE      VIDCAD        WCAD1
054600141029     C                   MOVE      VIDLOD        WLOD1
054610141029     C                   MOVE      VIDrsd        Wrsd1
054620141029     C                   MOVE      VIDind        Wind1
054630141029     C                   MOVE      VIDprd        Wprd1
054640141029     C                   MOVE      VIDnzd        Wnzd1
054650141029     C     d_o95lna      ifne      ARBLNA
054660141029     C     SAVCAD        IFNE      VIDCAD
054670060111     C***  O95LNA        ANDNE     ARBLNA
054680141029     C     SAVLOD        ORNE      VIDLOD
054690060111     C***  O95LNA        ANDNE     ARBLNA
054700141029     C     SAVrsd        ORNE      VIDrsd
054710141029     C     SAVind        ORNE      VIDind
054720141029     C     SAVprd        ORNE      VIDprd
054730141029     C     SAVnzd        ORNE      VIDnzd
054740141029     C                   SETON                                        9028
054750141029     C                   MOVEL     MSG(76)       VIDMSG
054760141029     C                   GOTO      ENDCT2
054770141029     C                   END
054780141029     C                   END
054790141029     C                   END
054800141029     c                   endif
054810141029     c                   endif
054820941020     C**
054830950522     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
054840950522     C* obbligatoria
054850950412     C     *IN05         IFEQ      *OFF
054860950412     C     *IN72         ANDEQ     *OFF
054870950522     C     WDESIE        ANDEQ     'E'
054880950522     C     DESEFT        ANDNE     *BLANKS
054890950412     C     VIDPID        ANDEQ     *BLANKS
054900950412     C                   SETON                                        284590
054910950412     C                   MOVEL     MSG(67)       VIDMSG
054920950412     C                   GOTO      ENDCT2
054930950412     C                   END
054940061116     c
054950061116     c* Se modifica la ragione sociale del destinatario, controllare
054960061116     c*  la partita iva
054970140507     c* Non controllo se chiamata pgm richiamato "cieco"
054980140507     c                   if        d48ffr<>'E'
054990061116     c                   if        vidrsd<>savrsdQ and vidcdf<>*blanks
055000061116     C                   SETON                                        285190
055010061116     C                   MOVEL     MSG(134)      VIDMSG
055020061116     c                   movel     vidrsd        savrsdQ
055030061116     C                   GOTO      ENDCT2
055040061116     C                   END
055050140507     c                   endif
055060061116     c
055070061116     c* La memorizzo anche se cdf=*blanks
055080061116     c                   movel     vidrsd        savrsdQ
055090061115     c
055100061115      * Devo richiamare il nuovo driver fatto per controllare il codice
055110061115      * fiscale
055120061115     c                   if        vidcdf<>*blanks
055130151105     c                   clear                   xcfiva1ds
055140061115     c                   clear                   xcfivamod
055150061115     c                   eval      xcfivapar = vidcdf
055160061115     c                   eval      xcfivanar = vidnzd
055170061115     c                   eval      xcfivaprv = vidprd
055180151105     c                   eval      xcfivacap = vidcad
055190151105     c                   eval      xcfivaloc = vidlod
055200151105     c                   eval      xcfivapiva= vidpid
055210151105     c                   call      'XCFIVAR1'
055220151105     c                   parm                    xcfiva1ds
055230061115     c*
055240061115     c                   if        xcfivaflg < *zeros
055250061115     C                   SETON                                        289051
055260080505     C                   MOVEL     xcfivamsg     VIDMSG
055270061115     C                   GOTO      ENDCT2
055280061115     c                   endif
055290061115     c                   endif
055300120508     c* Parita iva $$ non ammessa per fattura di filiale
055310120508     c                   move      v1cksc        w0040
055320120508     c                   if        %scan('$$':vidpid)>0  and w0040=9999
055330120508     C                   SETON                                        284590
055340120508     C                   MOVEL     MSG(149)      VIDMSG
055350120508     C                   GOTO      ENDCT2
055360120508     c                   endif
055370070508     c
055380140408     c
055390070508     c                   movel     vidffd        wffd
055400141029     c* Il fermo deposito non si può inserire se presente dispo di dirottamento da
055410141029     c* eseguire
055420141029     c                   if        vidffd<>*blanks and arbffd<>wffd
055430141029     c                   clear                   fnlry09ds2
055440141029     c                   eval      ILRY09TCH='T'
055450141029     c                   eval      ILRY09TDIS='D'
055460141029     c                   eval      ILRY09AAS=d48aas
055470141029     c                   eval      ILRY09LNP=d48lnp
055480141029     c                   eval      ILRY09NRS=d48nrs
055490141029     c                   eval      ILRY09NSP=d48nsp
055500141029     c                   eval      kpjbu=fnlry09ds2
055510141031     c                   call      'FNLRY09C'
055520141029     c                   parm                    kpjba
055530141029     c                   eval      fnlry09ds2=kpjbu
055540141029     c                   if        OLRY09ESIT='1'
055550141029     C                   SETON                                        286890
055560141029     C                   MOVEL     MSG(155)      VIDMSG
055570141029     C                   GOTO      ENDCT2
055580141029     c                   endif
055590141029     c                   endif
055600140408     c* Il fermo deposito non si può inserire se presente particolarità consegna
055610140408     c* che prevede che il fermo deposito debba essere autorizzato dal mittente
055620140408     c                   if        vidffd<>*blanks and arbffd<>wffd and
055630140408     c                             arbgma<>*blanks
055640140408     c                   Movel     '7R'          cod
055650140408     c                   Movel(p)  ArbGMa        key
055660140408     c                   ExSr      ctabel
055670140408     c  n30              Movel     TblUni        ds7R
055680140408     c   30              Clear                   ds7R
055690140408     C                   IF        §7RFD='S' AND vidcvr<>'I7'
055700140408     C                   SETON                                        289068
055710140408     C                   MOVEL     MSG(153)      VIDMSG
055720140408     C                   GOTO      ENDCT2
055730140408     C                   ENDIF
055740140408     c                   endif
055750140408     c* Da contattare impostabile solo se immesso il fermo deposito
055760140408     c*  solo se GEO attiva
055770141223     c* NON faccio questo controllo se richiamo cieco
055780141223     c                   if        d48ffr<>'E'
055790140213     c   77              if        (arbffd=wffd or  vidffd='  ') and
055800070508     c                             vidtfd='SI'
055810070508     C                   SETON                                        289052
055820070508     C                   MOVEL     MSG(99)       VIDMSG
055830070508     C                   GOTO      ENDCT2
055840070508     c                   endif
055850141223     c                   endif
055860070508     C
055870070508     c* se immesso fermo deposito e c'e' la consegna richiesta avviso
055880140507     c* NON faccio questo controllo se richiamo cieco
055890140507     c                   if        d48ffr<>'E'
055900140213     c   77              if        vidffd='SI' and arbffd<>wffd and
055910070508     c                             vidtfd='  ' and arbdcr>0  AND WZ3=0
055920070508     C                   SETON                                        289052
055930070508     C                   MOVEL     MSG(143)      VIDMSG
055940070508     C                   EVAL      WZ3=1
055950070508     C                   GOTO      ENDCT2
055960140213     c                   endif
055970140507     c                   endif
055980061115     c
055990941025    1C                   ENDIF
056000941019     C*
056010930505     C***
056020941025    1C     *IN02         IFEQ      '1'
056030930505     C* PESO
056040030203      * Se non è protetto
056050030203   2ac                   If        Not *In88
056060930505     C* NON A 0 O MAGGIORE DEL LIMITE POSTO
056070980406     C     VIDPKF        IFEQ      0
056080941020     C                   SETON                                        479028
056090941020     C                   MOVEL     MSG(2)        VIDMSG
056100911209     C                   GOTO      ENDCT2
056110941025    2C                   END
056120000510     C* DPD NO MAGGIORE DEL LIMITE IMPOSTO
056130020318    2C     LNPNTW        IFEQ      'DPD'
056140020318     C     LNANTW        OREQ      'DPD'
056150000510    3C     VIDPKF        IFGT      §3IPKD
056160000510     C                   SETON                                        479028
056170000510     C                   MOVEA     MSG(52)       K78
056180000510     C                   MOVEL     §3IPKD        W006A             6
056190000510     C                   MOVEA     W006A         SKI(10)
056200000510     C                   Z-ADD     10            I                 3 0
056210000510     C                   EXSR      ABBL
056220000510     C                   MOVEA     SKI(10)       K78(53)
056230000510     C                   MOVE      §3IPKD        W001A
056240000510     C                   MOVEL     W001A         K78(60)
056250000510     C                   MOVEA     K78           VIDMSG
056260000510     C                   GOTO      ENDCT2
056270000510    3C                   END
056280000510    2C                   END
056290000510     C**
056300000510     C**
056310000510     C     VARPKF        IFNE      VIDPKF
056320000510     C                   CLEAR                   WPT1
056330000510     C                   ENDIF
056340000510     C*
056350000510     C     VARPKF        IFNE      VIDPKF
056360000510     C     ARBFVF        ANDEQ     'K'
056370000510     C                   CLEAR                   SET
056380000510     C                   ENDIF
056390000510     C                   MOVEL     VIDPKF        VARPKF
056400000510     C**
056410021121
056420021121     c                   Move      ArbCtr        DueCtr
056430021121
056440021121      * Bolla FedEx con tariffa documenti il peso non può essere superiore a
056450021121      * quanto indicato in tabella "DFE" --> msg forzabile
056460140507     c* Non controllo se progamma richiamato in modalità "cieca"
056470140507     c                   if        d48ffr<>'E'
056480021121If  2c                   If        LnaNtw = 'FED' and DueCtr >= §DfeDalD and
056490021121     c                             DueCtr <= §DfeAlD and VidPkf > §DfePkgD
056500021121     c                             and Wpt1 = *Blanks
056510021121     c                   Eval      *In47 = *On
056520021121     c                   Eval      *In28 = *On
056530021121     c                   Eval      *In90 = *On
056540021121      * Preparo il msg
056550021121     c                   Movea     Msg(116)      K78
056560021121     c                   Movel     §DfePkgD      W006a
056570021121     c                   Movea     W006a         Ski(10)
056580021121     c                   Z-Add     10            i
056590021121     c                   ExSr      Abbl
056600021121     c                   Movea     Ski(10)       K78(50)
056610021121     c                   Move      §DfePkgD      W001a
056620021121     c                   Movel     W001a         K78(57)
056630021121     c                   Movea     K78           VidMsg
056640021121     c                   Eval      Wpt1 = '1'
056650021121     c                   GoTo      EndCt2
056660021121    2c                   EndIf
056670140507     c                   endif
056680051129      * Nel caso di peso totalmente rilevato da VDL posso modificare solo se
056690051129      * inserisco il peso uguale a quello VDL
056700030123     c**!!!              If         ArbNcp = ArbNcl and VidPkc <> VidPkf
056710030123     c                   If         wfpf = 'T' and VidPkc <> VidPkf
056720030122     c                   Seton                                        479028
056730030122     c                   Eval      VidMsg = Msg(121)
056740030122     c                   GoTo      EndCt2
056750030122    2c                   EndIf
056760051129      * Nel caso di peso parzialmente rilevato da VDL posso immettere un peso uguale
056770051129      * o superiore a quello VDL
056780030123     c                   If        wfpf = 'Z' and VidPkf < VidPkc
056790030123     c                   Seton                                        479028
056800030123     c                   Eval      VidMsg = Msg(123)
056810030123     c                   GoTo      EndCt2
056820030123    2c                   EndIf
056830030203   2ac                   EndIf
056840021121
056850930505     C* VOLUME
056860091124      * Se non è protetto
056870091124   2ac                   If        Not *In89
056880091124     c
056890980406     C     VIDVLF        IFEQ      0
056900941215     C                   SETON                                        489028
056910941020     C                   MOVEL     MSG(3)        VIDMSG
056920911209     C                   GOTO      ENDCT2
056930941025    2C                   ENDIF
056940941214     C*
056950941027     C* SE IL TIPO VOLUME E' "Z" NON SI PUO' ABBASSARE
056960941027     C     VIDFVF        IFEQ      'Z'
056970941027     C     VIDVLF        ANDLT     ARBVLF
056980941215     C                   SETON                                        489028
056990941027     C                   MOVEL     MSG(5)        VIDMSG
057000941027     C                   GOTO      ENDCT2
057010941027    2C                   ENDIF
057020091124     c*
057030091124     c* Per volume totaole variabile solo come il totale
057040091124     c                   If         ArbNcr = ArbNcl and Vidvlf <> arbvlc
057050091124     c                   Seton                                        489028
057060091124     c                   Eval      VidMsg = Msg(147)
057070091124     c                   GoTo      EndCt2
057080091124    2c                   EndIf
057090091124     c
057100091124      * Nel caso di vol  parzialmente rilevato da VDL posso immettere un vol  uguale
057110091124      * o superiore a quello VDL
057120091124     c                   If         ArbNcr < ArbNcl and Vidvlf < arbvlc
057130091124     c                              and arbncr>0
057140091124     c                   Seton                                        489028
057150091124     c                   Eval      VidMsg = Msg(148)
057160091124     c                   GoTo      EndCt2
057170091124    2c                   EndIf
057180091124   2ac                   EndIf
057190941214     C*
057200051129     C* SE IL TIPO VOLUME NON E' "Z" MA ESISTE UN VOLUME VDL CHE DIVENT
057210941214     C*  SUPERIORE A QUELLO DA FATTURARE DOPO LA VARIAZIONE, AVVISO E
057220941215     C*  MODIFICO
057230941215     C     VARVLF        IFNE      VIDVLF
057240941215     C                   CLEAR                   SET
057250941215     C                   MOVEL     VIDVLF        VARVLF
057260941215     C                   ENDIF
057270941215     C*
057280051129     C*  POI IMPOSTERO NEL VOLUME DA FATTURARE IL VOLUME VDL
057290941214    2C     SET           IFEQ      0
057300941214     C     ARBVLC        ANDGT     0
057310941214     C     VIDFVF        ANDNE     'Z'
057320941214     C     VIDFVF        ANDNE     'T'
057330941214     C*
057340051129    3C     WVDL          IFEQ      'S'
057350970418    4C     WFVF          IFEQ      'Z'
057360941222     C   06              MOVEL     ZAFA          WSOST             1
057370941222     C  N06              MOVEL     ZAAA          WSOST
057380970418   X4C                   ELSE
057390970418     C   06              MOVEL     TAFA          WSOST             1
057400970418     C  N06              MOVEL     TAAA          WSOST
057410970418    4C                   END
057420970418   X3C                   ELSE
057430970418    4C     WFVF          IFEQ      'Z'
057440941222     C   06              MOVEL     ZAFN          WSOST             1
057450941222     C  N06              MOVEL     ZAAN          WSOST
057460970418     C                   ELSE
057470970418     C   06              MOVEL     TAFN          WSOST             1
057480970418     C  N06              MOVEL     TAAN          WSOST
057490970418    4C                   ENDIF
057500970418    3C                   ENDIF
057510941214     C*
057520941214     C* SE VOLUME SAREBBE DA SOSTITUIRE VEDO SE DIVENTA SUPERIORE
057530941214     C* DEL VOLUME DA FATTURARE
057540941214    3C     WSOST         IFEQ      'S'
057550970418     C                   CLEAR                   WSOSC
057560941214     C*
057570941214    4C     VIDVLF        IFLT      ARBVLF
057580941214     C     VIDVLF        ANDLT     ARBVLC
057590970418     C* PRENDO FLAG DI SOSTITUIBILITA' VOLUME REALE
057600970418     C                   MOVEL     '7T'          COD
057610970418     C                   MOVEL(P)  'R'           KEY
057620970418     C                   EXSR      CTABEL
057630970418     C   30              CLEAR                   DS7T
057640970418     C  N30              MOVEL     TBLUNI        DS7T
057650970418     C     WFVF          IFEQ      'Z'
057660970418     C                   MOVEL     §7TSOZ        WSOSC
057670970418     C                   ELSE
057680970418     C                   MOVEL     §7TSOT        WSOSC
057690970418     C                   END
057700970418     C* (SOLO SE IL VOLUME E' SOSTITUIBILE)
057710970418    5C     WSOSC         IFEQ      'S'
057720140507     C                   ADD       1             SET
057730140507     c* non segnalo se pgm richiamato in modalità "cieca"
057740140507     c                   if        d48ffr<>'E'
057750941214     C                   SETON                                        482890
057760941214     C                   MOVEL     MSG(58)       VIDMSG
057770941214     C                   GOTO      ENDCT2
057780140507     c                   endif
057790970418    5C                   ENDIF
057800941214    4C                   ENDIF
057810941214     C*
057820941214     C* SE VOLUME AUTOMATICO E MODIFICATO IL PESO CONTROLLO
057830970418   4AC     *IN88         IFEQ      *OFF
057840941214     C                   Z-ADD     VIDVLF        D20VLU
057850941214     C                   EXSR      VOLAUT
057860970418    4C     D20VLU        IFLT      ARBVLC
057870970418     C                   MOVEL     '7T'          COD
057880970418     C                   MOVEL(P)  ARBFVF        KEY
057890970418     C                   EXSR      CTABEL
057900970418     C   30              CLEAR                   DS7T
057910970418     C  N30              MOVEL     TBLUNI        DS7T
057920051129     C* FLAG DI SOSTITUIBILITA' DA VOL.VDL
057930970418     C     WFVF          IFEQ      'Z'
057940970418     C                   MOVEL     §7TSOZ        WSOSC
057950970418     C                   ELSE
057960970418     C                   MOVEL     §7TSOT        WSOSC
057970970418     C                   END
057980970418    5C     WSOSC         IFEQ      'S'
057990140507     C                   ADD       1             SET
058000140507     c* non segnalo se pgm richiamato in modalità "cieca"
058010140507     c                   if        d48ffr<>'E'
058020941215     C                   SETON                                        472890
058030941214     C                   MOVEL     MSG(58)       VIDMSG
058040941214     C                   GOTO      ENDCT2
058050140507     c                   endif
058060970418    5C                   ENDIF
058070941214    4C                   ENDIF
058080970418   4AC                   ENDIF
058090941214    3C                   ENDIF
058100941214    2C                   ENDIF
058110941214     C*
058120941027    1C                   ENDIF
058130910429     C*
058140910429     C     ENDCT2        ENDSR
058150911212     C*
058160911209     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE IX -----------------*
058170911209     C     REGIS2        BEGSR
058180130412     c                   clear                   wpda_ndc
058190130412     c                   clear                   wpda_ddc
058200130412     c                   clear                   wpda_dcm
058210950104     C*  GIRO UDATE
058220950104     C                   TIME                    W0140            14 0
058230950104     C* UDATE IN GGMMAAAA
058240950104     C                   MOVE      W0140         WDTGIO            8 0
058250950104     C*
058260950104     C* UDATE IN AAAAMMGG
058270950104     C                   Z-ADD     WDTGIO        G02DAT
058280950104     C                   MOVEL     *BLANK        G02ERR
058290950104     C                   CALL      'XSRDA8'
058300950104     C                   PARM                    WLBDAT
058310950104     C                   MOVEL     G02INV        DATEU             8 0
058320941213     C                   MOVEL     KNMUS         ARBPRU
058330941221     C* PRENDO I VALORI ASSOLUTI
058340941221     C                   MLLZO     1             VIDPKF
058350941221     C                   MLLZO     1             VIDVLF
058360041007     C* SALVO VOL/PKG/FTC/RSD/CAP/FIN VECCHIO IN FNARB
058370950114     C                   MOVEL     ARBVLF        OLDVLF
058380950114     C                   MOVEL     ARBFVF        OLDFVF
058390950114     C                   MOVEL     ARBPKF        OLDPKF
058400041007     C                   MOVEL     ARBvlc        OLDvlc
058410950114     C                   MOVEL     ARBRSD        OLDRSD
058420150206     C                   MOVEL     ARBLOD        OLDLOD
058430950114     C                   MOVEL     ARBCAD        OLDCAD
058440950114     C                   MOVEL     ARBFIN        OLDFIN
058450041007     C                   MOVEL     ARBFFD        OLDFFD
058460070320     c                   movel     VIDFFD        WFFD
058470151230     c* (Se variazione fatta da Arrivo su bolla ancora da partire non devo inviare)
058480151222     c                   if        not *in10 or wfile='A'
058490920120     C* INVIO PER SEDE
058500941025     C                   MOVEL     'FNARBD0T'    SFILE
058510920120     C* ALLOCO
058520911212     C                   EXSR      ALLOC
058530941027     C* ERRORI IN ALLOCAZIONE
058540941212    1C     *IN91         IFEQ      *ON
058550941025     C                   GOTO      ENDRG2
058560941212    1C                   ENDIF
058570070515    c
058580070515     c* Se modificato FERMO DEPOSITO, devo allocare record L di ar4
058590070515     c*  per blocco/sblocco telefonata
058600070515     c                   clear                   wesar4L           1
058610070515    1c                   if        Wffd<>oldFFD
058620070515     c                   eval      TipoEla='C'
058630070515     c                   exsr      GestAR4L
058640070515     c
058650070515    2c                   if        *in91
058660070515     c                   exsr      DLCSED
058670070525     C                   SETON                                        2891
058680070515     C                   GOTO      ENDRG2
058690070515    2c                   endif
058700070515    1c                   endif
058710070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
058720070515     c* iornare direttamente il file
058730070515     c                   clear                   waltrabolla       1
058740151222     C   10KARB          CHAIN     FNBLP01L                           3091
058750151222     C  n10KARB          CHAIN     FNARB01L                           3091
058760070515     c*
058770070515     c                   if        *in91
058780070515     C                   MOVEL     MSG(4)        VIDMSG
058790070515     c*
058800070515     c                   exsr      DLCSED
058810070515     c                   unlock    fiar401l
058820070515     c
058830070525     C                   SETON                                        2891
058840070515     C                   GOTO      ENDRG2
058850070515     c                   else
058860070515     c                   if        *in30
058870070515     c                   eval      waltrabolla='N'
058880070515     c                   else
058890070515     c* Salvo alcuni campi dell'altro file che potrebbero, per tempi
058900070515     c*  di aggiornamento, essere diversi.
058910070515     c                   eval      waltrofvf=arbfvf
058920070515     c                   eval      waltrovlf=arbvlf
058930070515     c                   eval      waltrocpi=arbcpi
058940130411     c* salvo campi di arb che mi servono per invio dati a pda
058950151222     c  n10              eval      wpda_ndc=arbndc
058960151222     c  n10              eval      wpda_ddc=arbddc
058970151222     c  n10              eval      wpda_dcm=arbdcm
058980151222     C   10KARB          CHAIN     FNARB01L                           30
058990151222     C  n10KARB          CHAIN     FNBLP01L                           30
059000070515     c* Alla fine per non sporcarmi i campi del record, richaino il
059010070515     c*  record originale che sto variando
059020151222     c   10              eval      wpda_ndc=arbndc
059030151222     c   10              eval      wpda_ddc=arbddc
059040151222     c   10              eval      wpda_dcm=arbdcm
059050070515     c                   endif
059060070515     c                   endif
059070151222     c                   endif
059080070207     C*
059090070207     C* SE SI TRATTA DI BOLLA FRANCO, SALVO IL CAMPO DELLA PARTITA IVA
059100151222     C*   10
059110151222     C*AN 05              MOVEL     ARBCPI        SAVCPI           16
059120151222     C*   10
059130151222     C*AN 05              MOVEL     *BLANKS       ARBCPI
059140070207     C*
059150151222     C*  N10
059160151222     C*AN 06              MOVEL     ARBCPI        SAVCPI           16
059170151222     C*  N10
059180151222     C*AN 06              MOVEL     *BLANKS       ARBCPI
059190151222     c                   if        (wfile='A' and *in05) or
059200151222     c                             (wfile='P' and *in06)
059210151222     C                   MOVEL     ARBCPI        SAVCPI           16
059220151222     C                   MOVEL     *BLANKS       ARBCPI
059230151222     c                   endif
059240070207     C*
059250061116     c* Se variazione di destinatario chaino anche fiar4 record Q se
059260061116     c*  mi serve
059270141215     c                   if        wnoindir=' '
059280061116     c                   if        *in01 and not *in05
059290061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
059300061116     c                             (vidcdf<>*blanks and war4Q<>' ')
059310061116     C                   MOVEL     'Q'           WTRC
059320061116     C     KARB2         CHAIN     FiAR4000                           3091
059330061116     c*
059340061116     c                   if        *in91
059350061116     C                   MOVEL     MSG(4)        VIDMSG
059360070515     c
059370151223     c                   if        not *in10 or wfile='A'
059380070515     c                   exsr      DLCSED
059390151223     c   10              unlock    fnblp01l
059400151223     c  N10              unlock    fnarb01l
059410151223     c                   endif
059420070525     C                   SETON                                        2891
059430061116     C                   GOTO      ENDRG2
059440061116     c                   endif
059450061116     c                   endif
059460061116     c                   endif
059470141215     c                   endif
059480061116     c
059490061116     c* Verifico se prima c'era
059500911212     C*
059510911209     C                   Z-ADD     DATEU         ARBDTV
059520911209     C                   TIME                    ARBORV
059530911209     C                   MOVEL     VIDCVR        ARBCVB
059540911209     C*
059550060130     C* SE DA STORICIZZARE
059560060130     C**   *IN10         IFEQ      *ON
059570060130    1C**   D66FSA        ANDEQ     'S'
059580060130     C**   *IN10         OREQ      *OFF
059590060130    1C**   D66FSP        ANDEQ     'S'
059600060130    1C     D66FSA        ifeq      'S'
059610941212     C                   MOVEL     SAVRD2        ARBRD2
059620950114     C* SE VENGO DA VARIAZIONE MITTENTE, IL VOLUME LO STORICIZZO CON I
059630950114     C* CAMPI CHE HO SALVATO
059640950114     C     *IN24         IFEQ      *ON
059650950114     C                   MOVEL     MITFVF        ARBFVF
059660950114     C                   MOVEL     MITVLF        ARBVLF
059670950114     C                   MOVEL     MITFVF        ARBFVB
059680950114     C                   MOVEL     MITVLF        ARBVLB
059690950114    1C                   ENDIF
059700011108      *
059710011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
059720011108      * è in partenza 'P' o in arrivo 'A'
059730011108     C     *IN10         IFEQ      *ON
059740160119     c                   if        wfile='P'
059750160119     C                   MOVE      'a'           ARBPRU
059760160119     c                   else
059770011108     C                   MOVE      'A'           ARBPRU
059780160119     c                   endif
059790011108     C                   ENDIF
059800011108     C     *IN10         IFEQ      *OFF
059810011108     C                   MOVE      'P'           ARBPRU
059820011108     C                   ENDIF
059830011108      *
059840941014     C                   WRITE     FNARBD00
059850130927     c
059860130927     c* Memorizzo anche referente e telefono se si tratta di varazione "I0"
059870131009     c                   if        *in01 and
059880131009     c                                  (vidref<>par5ref or vidteld<>par5teld)
059890131204     c                   clear                   darbn_ref
059900130927     c                   eval      §arbnref = par5ref
059910130927     c                   eval      §arbnteld= par5teld
059920131204     c                   eval      arbuni70 = darbn_ref
059930130930     c                   eval      arbtrd='REF'
059940130927     c                   write     FNARBN00
059950130927     c                   endif
059960950114     C*
059970950114     C     *IN24         IFEQ      *ON
059980950114     C                   MOVEL     OLDFVF        ARBFVF
059990950114     C                   MOVEL     OLDVLF        ARBVLF
060000950114     C                   MOVEL     OLDFVF        ARBFVB
060010950114     C                   MOVEL     OLDVLF        ARBVLB
060020950114    1C                   ENDIF
060030950114    1C                   ENDIF
060040060301     c* storicizzazione motivazione
060050060301     c     d66mtv        ifeq      'S'
060060060301     c                   exsr      sto_mtv
060070060301     c                   endif
060080911209     C*
060090941025    1C     *IN01         IFEQ      *ON
060100040224
060110040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
060120040224      * - Fermo deposito
060130040224      * devo memorizzare i dati della bolla prima della modifica
060140070320if  3c   10              If        wffd <> oldFfd
060150041022     c                   ExSr      RegAr5AR
060160040224    3c                   EndIf
060170941025     C*
060180941025     C* SE IMMESSA ANCHE LA 2 RAGIONE SOCIALE
060190141212     c                   if        wnoindir=' '
060200060223     c                   clear                   d19ftr
060210160115     c                   if        *in10 and wfile='P'
060220160115     c                   eval      d19ftr='T'
060230160115     c                   endif
060240941025     C                   MOVEL     ARBAAS        D19AAS
060250941025     C                   MOVEL     ARBLNP        D19LNP
060260941025     C                   MOVEL     ARBNRS        D19NRS
060270941025     C                   MOVEL     ARBNSP        D19NSP
060280941025     C                   MOVEL     'D'           D19TRC
060290970520     C                   MOVE      VIDRSD        D19NT1
060300941025     C                   CLEAR                   D19NT2
060310051114     c
060320941025     C                   CALL      'FNLV19R'
060330030612     C                   PARM                    Fnlv19DS
060340941025     C*
060350941025     C* ERRORE: RECORD NON VINCOLABILE
060360941212    2C     D19ERR        IFEQ      '2'
060370941025     C                   MOVEL     MSG(4)        VIDMSG
060380151223     c                   if        not *in10 or wfile='A'
060390070515     c                   exsr      dlcsed
060400070515     c   10              unlock    fnblp01l
060410070515     c  N10              unlock    fnarb01l
060420070515     c                   unlock    fiar401l
060430151230     c                   endif
060440151223     C                   SETON                                        2891
060450941025     C                   GOTO      ENDRG2
060460941212    2C                   ENDIF
060470141212     c                   endif
060480041006    c
060490950626     C*
060500950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
060510950626     C*     NELLA P.IVA
060520950626     C     VIDISD        IFEQ      '$$'
060530950626     C     VIDCPD        ANDEQ     *BLANKS
060540950626     C                   MOVEL     'PRIVATO'     VIDCPD
060550950626     C                   ENDIF
060560941025     C*
060570141212     c                   if        wnoindir=' '
060580941027     C                   MOVEL     VIDRSD        ARBRSD
060590970520     C                   MOVE      D19NT1        ARBRD2
060600911209     C                   MOVEL     VIDIND        ARBIND
060610911209     C                   MOVEL     VIDLOD        ARBLOD
060620911209     C                   MOVEL     VIDCAD        ARBCAD
060630060705     c* se spedizione export DPD diretta in Portogallo devo aggiungere al
060640060705     c* cap immesso tanti 0 a dx quanti ne servono per arrivare ad una
060650060705     c* lunghezza di cap di 7 byte
060660060705     c                   if        lnantw='DPD' and vidnzd = 'P  '
060670060705     c     ' '           checkr(e) arbcad:7      NN                1 0
060680060705     c                   if        %found
060690060705     c                   eval      %subst(arbcad:nn+1:7-nn)=*all'0'
060700060705     c                   endif
060710060705     c                   endif
060720060705     c*
060730911209     C                   MOVEL     VIDPRD        ARBPRD
060740941220     C                   MOVEL     VIDPID        ARBCPI
060750941220     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
060760941220     C*  IL CODICE DELLA P.IVA
060770941220     C     VIDCPD        IFNE      *BLANKS
060780941220     C     VIDISD        ANDEQ     *BLANKS
060790941220     C                   CLEAR                   ARBCPI
060800941220     C                   MOVEL     VIDCPD        ARBCPI
060810941220     C                   ENDIF
060820041027     c                   eval      newcpi=arbcpi
060830041027     c
060840060222     C* SE ESISTE RECORD P.IVA IN FIAR4 LO AGGIORNO
060850950413     C* SOLO SE P.IVA SPROTETTA
060860950413     C     *IN05         IFEQ      *OFF
060870950413     C     *IN72         ANDEQ     *OFF
060880950413     C                   MOVEL     'P'           WTRC
060890060222     C     KARB2         CHAIN     FIAR401L                           30
060900060222     C     *IN30         IFEQ      *OFF
060910060223     c                   move      ar4not        wcpifile
060920060223     c                   if        wcpifile <> arbcpi
060930060223     c                   movel     'T'           ar4ftr
060940060223     c                   z-add     dateu         ar4dtr
060950060223     c                   z-add     dateu         ar4duv
060960060222     C                   MOVE      ARBCPI        AR4NOT
060970060222     C                   UPDATE    FIAR4000
060980060223     c                   else
060990060223     c                   unlock    fiar401l
061000060223     c                   endif
061010060222     C                   END
061020950413     C                   END
061030141215     c                   endif
061040941212     C*
061050931207    2C     VIDFFD        IFEQ      'SI'
061060950222     C                   MOVEL     'C'           ARBFIN
061070930528     C                   MOVEL     'S'           ARBFFD
061080931207   X2C                   ELSE
061090930528     C                   MOVEL     ' '           ARBFFD
061100141212     c                   if        wnoindir=' '
061110930528     C                   MOVEL     VIDFIN        ARBFIN
061120141212     c                   endif
061130931207    2C                   END
061140110905     c
061150110905     c* se attiva procedura centro storico e c'e' il giro sulla bolla --> ricalcolo se
061160110905     c*  non è stato forzato a video
061170151230     c                   if        wnoindir=' '  and
061180151230     c                             (not *in10 or wfile='A')
061190110906     c                   if        vidffd=*blanks
061200110906     c                   if        (oldfin<>arbfin and arbfin=o95iso)
061210110906     c                             or oldffd<>vidffd
061220110906     c*****                        or oldfin=arbfin
061230110906     c
061240110905     c     karb          chain     fiarg01l
061250110905     c                   if        %found(fiarg01l) and argcgi<>*blanks
061260110905     c                             and olvstprop<>'N'
061270110905     c                   clear                   fnlvstds
061280110905     c                   eval      ilvstaas=arbaas
061290110905     c                   eval      ilvstlnp=arblnp
061300110905     c                   eval      ilvstnrs=arbnrs
061310110905     c                   eval      ilvstnsp=arbnsp
061320110905     c                   eval      ilvstmgs=arbmgs
061330110905     c                   eval      ilvstfin=arbfin
061340110905     c                   eval      ilvstcgi=argcgi
061350110906     c                   if        lnacons>0
061360110906     c                   eval      ilvstpoc=lnacons
061370110906     c                   else
061380110906     c                   eval      ilvstpoc=arblna
061390110906     c                   endif
061400110906     c                   if        arbddc>0
061410110906     c                   eval      ilvstdat=arbddc
061420110906     c                   else
061430110906     c                   eval      ilvstdat=dateu
061440110906     c                   endif
061450110905     c                   call      'FNLVSTR'
061460110905     c                   parm                    kpjba
061470110905     c                   parm                    fnlvstds
061480110906     c
061490110906     c* se aggiornato flag inoltro lo memorizzo nella variazione
061500110906     c                   if        OLVSTFINAG='S'
061510110906     c                   eval      arbfin=olvstfin
061520110905     c                   endif
061530110906     c
061540110906     c                   endif
061550110906     c                   endif
061560110906     c                   endif
061570141212     c                   endif
061580110905
061590970808     C* CODICE TASSAZIONE
061600141212     c                   if        wnoindir=' '
061610970808     C                   MOVEL     O95CTS        ARBCTS
061620141212     c                   endif
061630130606
061640031120
061650130930     c                   exsr      AggioREF
061660130930     c
061670931207    1C                   END
061680941025     C**
061690920319     C* RICALCOLO DEL VOLUME
061700941025     C**
061710941025    1C     *IN02         IFEQ      *ON
061720950114     C*
061730950114     C* SE VARIATO MITTENTE PER I PADRONCINI USO QUELLI SALVATI
061740941102     C* PULIZIA DELLA DS PER IMPOSTARE IL VOLUME DA FATTURARE VARIATO
061750030612     C                   CLEAR                   Fnlv20DS
061760941102     C*
061770941025     C* VOLUME REALE
061780041007    2C     VIDVLF        IFNE      oldVLF
061790941102     C                   MOVEL     'R'           D20FVL
061800941102     C                   Z-ADD     VIDVLF        D20VLU
061810930505     C*
061820931207   X2C                   ELSE
061830941025     C*
061840941025     C* VEDO SE DEVO MODIFICARE IL VOLUME DA FATTURARE
061850941214     C                   EXSR      VOLAUT
061860941102    2C                   ENDIF
061870941102     C**
061880941102     C* SE HO MODIFICATO IL VOLUME DA FATTURARE CALL A FNLV20R
061890941102     C*  PER DETERMINARE IL VOLUME BOLLETTATO
061900941102     C**
061910941102    2C     D20VLU        IFGT      0
061920941222     C                   MOVEL     D48TBO        D20TBO
061930941102     C                   MOVEL     'F'           D20TVL
061940041008     C                   MOVEL     arbFVB        D20FVB
061950041008     C                   MOVEL     arbVLB        D20VLB
061960041007     C                   MOVEL     oldFVF        D20FVF
061970041007     C                   MOVEL     oldVLF        D20VLF
061980030612     C                   MOVEL     Fnlv20DS      KPJBU
061990941102     C                   CALL      'FNLV20R'
062000941102     C                   PARM                    KPJBA
062010941102     C*
062020030612     C                   MOVEL     KPJBU         Fnlv20DS
062030941102     C*
062040941102     C                   MOVEL     D20FVB        ARBFVB
062050941102     C                   MOVEL     D20VLB        ARBVLB
062060941102     C                   MOVEL     D20FVF        ARBFVF
062070941102     C                   MOVEL     D20VLF        ARBVLF
062080941102    2C                   ENDIF
062090941214     C*
062100051129     C* SE DEVO SOSTITUIRE COL VOLUME VDL LO FACCIO
062110941214     C     WSOST         IFEQ      'S'
062120970418     C* WSOSC INDICA SE IL TIPO VOLUME DETERMINATO PER LA BOLLA E'
062130051129     C* SOSTITUIBILE DAL VOLUME VDL.
062140970418     C* IN CASO DI VARIAZIONE VOLUME WSOSC SARA' QUELLO DEL TIPO VOLUME
062150970418     C* "R"; IN CASO DI VARIAZIONE DI PESO WSOSC SARA' QUELLO DEL TIPO
062160970418     C* VOLUME DELLA BOLLA
062170041007     C     oldVLC        ANDGT     ARBVLF
062180970418     C     WSOSC         ANDEQ     'S'
062190041007     C                   Z-ADD     oldVLC        ARBVLF
062200970418     C                   MOVE      WFVF          ARBFVF
062210941214     C                   ENDIF
062220920319     C*
062230990628     C  N10              MOVEL     VIDPKF        ARBPKB
062240941102     C                   MOVEL     VIDPKF        ARBPKF
062250041007     c
062260041007    2C     *IN24         IFEQ      *ON
062270041007     C                   MOVEL     MITFVF        OLDFVF
062280041007     C                   MOVEL     MITVLF        OLDVLF
062290041007    2C                   ENDIF
062300941102    1C                   ENDIF
062310941212     C*
062320041007     C* Ora invio solo a sede
062330151223     c                   if        not *in10 or wfile='A'
062340070503     C     D66FFI        ifeq      'S'
062350950227     C*
062360950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
062370950413    2C     §3ABSD        IFEQ      ' '
062380941102     C                   OPEN      FNARBD0T
062390941014     C                   WRITE     FNARBDTT
062400941102     C                   CLOSE     FNARBD0T
062410950227    2C                   END
062420911212     C* DISALLOCO OGGETTO
062430941212     C                   EXSR      DLCSED
062440950208    1C                   END
062450911209     C*
062460950113     C* AGGIORNO FILE TASSAZIONE PADRONCINI
062470950113     C                   EXSR      AGGPDS
062480151223     c                   endif
062490931207     C*
062500941102     C* AGGIORNO
062510151223     C**   10
062520151223     C**AN 05              MOVEL     SAVCPI        ARBCPI
062530151223     C**  N10
062540151223     C**AN 06              MOVEL     SAVCPI        ARBCPI
062550151223     c                   if        (wfile='A' and *in05) or
062560151223     c                             (wfile='P' and *in06)
062570151223     C                   MOVEL     SAVCPI        ARBCPI
062580151223     c                   endif
062590941212     C*
062600151223     c                   if        wfile='A'
062610151223     C                   UPDATE    FNARB000
062620151223     c                   else
062630151223     C                   UPDATE    FNBLP000
062640151223     c                   endif
062650150206     c
062660150206     c* memorizzo la località normalizzata
062670150206     c                   if        vidlod<>normlod OR WAR4X='E'
062680150206     C
062690150211     c                   eval      d19ftr='T'
062700150206     c                   eval      d19agg='E'
062710150203     C                   MOVEL     ARBAAS        D19AAS
062720150203     C                   MOVEL     ARBLNP        D19LNP
062730150203     C                   MOVEL     ARBNRS        D19NRS
062740150203     C                   MOVEL     ARBNSP        D19NSP
062750150203     C                   MOVEL     'X'           D19TRC
062760150206     C                   IF        VIDLOD<>NORMLOD
062770150203     C                   MOVE      normlod       D19NT1
062780150206     C                   ELSE
062790150206     C                   CLEAR                   D19NT1
062800150206     C                   ENDIF
062810150206     C                   CLEAR                   D19NT2
062820150203     c
062830150203     C                   CALL      'FNLV19R'
062840150203     C                   PARM                    Fnlv19DS
062850150206     c                   endif
062860150203     c
062870151230     c* Salto le seguenti istruzioni se
062880151223     c* dall'arrivo sono in manutenzione di una bolla in partenza ancora da
062890151223     c* partire
062900151223     c                   if        not *in10 or wfile='A'
062910070320     c
062920070320     c* Se aggiunto   Fermo Deposito, senza consegna richiesta,
062930070320     c*  imposto il blocco. conconsegna richiesta lo tolgo
062940070320     c* Se tolgo      Fermo Deposito, tolgo il relativo blocco
062950070320     c
062960070320     c* Devo richainare e aggiornare (perchè nel frattempo ho letto
062970070320     c*  altri record fi FIAR4
062980151223
062990070320    1c                   if        Wffd<> oldffd
063000070320     c                   eval      tipoEla='C'
063010070320     c                   exsr      GestAR4L
063020070320     c
063030070320     c* Se allocato pazienza (non aggiorno ma vado avanti
063040070320     c*  fermarmi in questo punto non è bello,lascerebbe la
063050070320     c*  transazione a metà)
063060070320    2c                   if        not *in91
063070070320     c
063080070508    3c****               if        arbdcr=0 and wffd='S'
063090070508     c* imposto il flag telefonata in base a quanto indicato a video
063100070508     c                   if        vidtfd='SI'
063110070508     c                   clear                   §b4ltfd
063120070508     c                   else
063130070320     c                   eval      §b4ltfd='S'
063140070508     c                   endif
063150070508     c
063160070508     c****               else
063170070508     c****               clear                   §b4ltfd
063180070508    3c****               endif
063190070320     c                   eval      tipoEla='A'
063200070320     c                   exsr      GestAR4L
063210140212     c* richiamo pgm per eventuale scrittura/cancellazione file spia
063220150603     c* per aggiungere il file spia per la sped, la bolla non deve essere in stati
063230150603     c*  particolari
063240150603     c                   clear                   tnsd99ds
063250150603     c                   clear                   statofd           1
063260150603     c                   if        wffd='S'
063270150603     C                   MOVEL     'L'           D98tla
063280150603     C                   MOVEL     'A'           D98tbo
063290150603     C                   Z-ADD     arbaas        D98aas
063300150603     C                   Z-ADD     arblnp        D98lnp
063310150603     C                   Z-ADD     arbnrs        D98nrs
063320150603     C                   Z-ADD     arbnsp        D98nsp
063330150603     C*
063340150603     C                   call      'TNSD99R'
063350150603     C                   parm                    tnsd99ds
063360150603     c                   if        D98SPCDEE='FFD' or %subst(D98SPCDEE:1:2)='FD'
063370150603     c                   eval      statofd='S'
063380150603     c                   endif
063390150603     c                   endif
063400150603     c
063410150603     c*****              if        wffd=*blanks or vidtfd=*blanks
063420150603     c                   if        wffd=*blanks or (vidtfd=*blanks and
063430150603     c                             statofd='S')
063440140212     c                   clear                   trtcp2ds
063450140213     c                   if        wffd=*blanks
063460140212     c                   eval      t2chiamata='A'
063470140212     c                   endif
063480140212     c                   eval      t2chi= 'F'
063490140212     c                   eval      T2AAS=d48aas
063500140212     c                   eval      T2LNP=d48lnp
063510140212     c                   eval      T2NRS=d48nrs
063520140212     c                   eval      T2NSP=d48nsp
063530140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
063540140213     c                   eval      T2fgs=arblna
063550140212     c                   call      'TRTCP2R'
063560140212     c                   parm                    trtcp2ds
063570140213     c                   endif
063580070320     c
063590070320    2C                   END
063600070320    1C                   END
063610070320     c
063620041007     c* Per l'altro file sistemo eventuale volume da fatturare e P.I
063630041007     c*  e aggiorno
063640041007     c                   if        waltrabolla=' '
063650041007     c   02              if        waltrofvf='T'  or
063660041007     c                             (waltrofvf='Z' and arbfvf<>'T' aND
063670041007     C                              WALTROVLF> arbvlf)
063680041008     c                   movel     waltrofvf     arbfvf
063690041008     c                   movel     waltrovlf     arbvlf
063700041007     c                   endif
063710041007     c* Se 1°bolla franco, tengo la p.i. presente nel record
063720151223     c                   if        (*in10 and *in06) or
063730151223     c                             (not *in10 and *in05)
063740041007     c                   eval      arbcpi=waltrocpi
063750041027     c                   else
063760041027     c                   eval      arbcpi=NEWCPI
063770041007     c                   endif
063780041007     c
063790041007     c* aggiornamenti di campi mirati
063800041007     C   10              except    arbdblp
063810041007     C  N10              except    arbdarb
063820041007     c                   endif
063830151223
063840151223     c                   endif
063850950221     C*
063860950221     C* DISALLOCO  I FILES CHE NON USO
063870051118     C   10              UNLOCK    FiAR901L
063880991001     C                   UNLOCK    FIAR601L
063890061116     c
063900061116     c* Se si tratta di bolla in assegnato con possibilità di scrivere il
063910061116     c*  cod fiscale, richiamo la REGIS11 per la memorizzazione
063920151230     c                   if        wnoindir=' ' and
063930151230     c                             (not *in10 or wfile='A')
063940061116     c                   if        *in01 and not *in05
063950061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
063960061116     c                             vidcdf<>*blanks
063970061116     c                   eval      v11mide='D'
063980061116     c                   eval      v11cdfis=vidcdf
063990071102     c* Imposto anche il campo della partita iva di nuovo
064000071102     c                   eval      v11pid=vidpid
064010071102     c
064020061116     c                   movel     'FI'          vidcvr
064030071026     c* Non devo aggiornare la bolla perchè l'ho già fatto qui per la
064040071026     c*  partita IVA
064050130412     c                   if        v11pid<>*blanks or
064060130412     c                              %subst(v11cdfis:12:1)<> ' '
064070071026     c                   eval      Aggiobol='N'
064080130402     c                   endif
064090061116     c                   exsr      REGIS11
064100061116     c                   endif
064110061116     c                   endif
064120141215     c                   endif
064130041007     c*
064140141212     c                   if        wnoindir='S'
064150141212     c                   eval      wdisag=wdisagdest
064160141212     c                   endif
064170041007     c* Riaggancio la bolla in modifica, per controlli sul
064180041007     c*  destinatario disagiato e consegna richiesta rispetto a FFD
064190151223     c                   if        wfile='A'
064200151223     C     KARB          CHAIN(n)  FNARB01L                           30
064210151223     c                   else
064220151223     C     KARB          CHAIN(n)  FNBLP01L                           30
064230151223     c                   endif
064240070906     c* Se in destinatario non è più A D o S tolgo se non immesso
064250070906     c*  dalla partenza
064260070911     c                   if        wdisagdest<>wdisag and wdisagdest<>' ' and
064270070910    2c                             (arbtc1=wdisagdest or arbtc2=wdisagdest)
064280070906     c                   exsr      ContrTCP
064290070906     c                   endif
064300040224
064310950602     C*
064320020531     C* SE destinatario disagiato devo rifare il giro per le consegne
064330020531     c* particolari
064340041015     C***  wdisag        IFne      *blanks
064350041015     c***  arbtc1        andne     wdisag
064360041015     c***  arbtc2        andne     wdisag
064370060327     c***                If        wdisag <> *blanks and
064380060327     c***                          %subst(arbGva:1:1) <> 'U' or
064390060327     c***                          wdisag='X' and %subst(arbGva:1:1)='U'
064400060327     c* Per Disagiato ---->sempre
064410060327     c* per Supermercato ->solo se non è consegna ad uffici
064420060327     c* per Appuntamento ->solo se non è consegna ad uffici e non è
064430070917     c*                    prevista data consegna "Tassativa" da partenza
064440070917     c                   movel     arbdcr        w008a
064450070907    1c                   if        (wdisag = 'X') OR
064460060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
064470060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
064480070917     c                             (arbdcr=*zeros or arbtcr <> *blanks or
064490070917     c                              (par5mdp='S' and w008a<>par5gdcr)))
064500070907    2c     arbtc1        ifne      wdisag
064510041015     c     arbtc2        andne     wdisag
064520030122     c                   Eval      wI0 = 'S'
064530020531     C                   MOVEL     'CP'          D48CVB
064540070907     C                   MOVEL     'D'           D48TRC
064550070911     C***                MOVEL     'E'           WESEG
064560070911     c***                clear                   W48f12
064570070907     c***                if        *in20
064580050502     c                   eval      §bgtcr=arbtcr
064590050502     c                   eval      §bghcr=arbhcr
064600050502     c                   eval      §bgdcr=arbdcr
064610050502     c                   eval      §bggc1=arbgc1
064620050502     c                   eval      §bggc2=arbgc2
064630050502     c                   eval      §bgtc1=arbtc1
064640050502     c                   eval      §bgtc2=arbtc2
064650070907     c***                endif
064660070910     c
064670070910     c* Se da eliminare anche la consegna particolare imposto
064680070910     c*  insieme la variazione
064690070910     c                   if        eliminatcp='1'
064700070910     c                   clear                   §bgtc1
064710070911     c* ripristino la vecchia consegna partic.
064720070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
064730070911     c                   eval      §bgtc1=aggtcp
064740070911     c                   endif
064750070910     c                   endif
064760070910     c                   if        eliminatcp='2'
064770070910     c                   clear                   §bgtc2
064780070911     c* ripristino la vecchia consegna partic.
064790070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
064800070911     c                   if        §bgtc1=' '
064810070911     c                   eval      §bgtc1=aggtcp
064820070911     c                   else
064830070911     c                   eval      §bgtc1=aggtcp
064840070911     c                   endif
064850070911     c                   endif
064860070910     c                   endif
064870070911     c
064880070907     c* Se modificato destinatario in disagiato o supermercato imposto il
064890070907     c* relativo flag nelle cosegne particolari
064900070907    3c     wdisag        ifne      *blanks
064910070907     c                   select
064920070907     c     §bgtc1        wheneq    *blanks
064930070907     c                   movel     wdisag        §bgtc1
064940070907     c     §bgtc2        wheneq    *blanks
064950070907     c                   movel     wdisag        §bgtc2
064960070907     c                   endsl
064970070907    4c     §bgtc1        ifeq      §bgtc2
064980070907     c                   clear                   §bgtc2
064990070907    4c                   endif
065000070907    3c                   endif
065010070907     c* Memorizzo in skiera
065020070907    3c                   if        kk<=10
065030070910     c* se devo eliminare una consegna particolare, memorizzo nella
065040070910     c* stessa variazione
065050070910     c                   if        eliminatcp=' '
065060070907     c                   add       1             kk
065070070910     c                   endif
065080070911     c* Passo dati in DS
065090070907     c                   movel     dsarbg        kdati(KK)
065100070907     c                   movel     d48cvb        kcvb(KK)
065110070907     c                   movel     d48trc        ktrc(KK)
065120070911     c* videata si vede o meno in base al richiamo prevedente
065130070911     c                   movel     d48ffr        kffr(KK)
065140070911     c* Disabilito F12
065150070911     c                   movel     ' '           kF12(KK)
065160070907    3c                   endif
065170070907     c
065180070907    2c                   endif
065190070910     c
065200070910   x1c                   else
065210070910     c* Se non devo aggiungere delle consegne particolari,
065220070910      *  se c'è la data consegna richiesta controllo se è ancora valida
065230070910      *  solo se ho fatto una modifica in arrivo
065240070914     c* Solo se non devo aggiungere delle consegnee particolari
065250070914     c*  altrimenti ilk controllo lo faccio nella regis7
065260070914     c                   if        aggtcp=*blanks
065270071109     c
065280071109     c* Imposto il campo WGIAC in base alla giacenza
065290071109if  2c                   If        *In04 and wgcfas = 35
065300071109if  3c                   If        wgcded > *Zeros and arbdcr >= wgcded
065310071109     c                   Eval      wgiac = 'S'
065320071109    3c                   EndIf
065330071109if  3c                   If        wgcded = *Zeros and arbDcr >= wgcddm
065340071109     c                   Eval      wgiac = 'S'
065350071109    3c                   EndIf
065360071109    3c                   EndIf
065370071109     c
065380160114    2c                   If        ArbDcr > *Zeros and *In10  and wfile='A'
065390070910     c                             and wCa = *Blanks and wgiac = *Blanks
065400070910     c                   ExSr      ContrDcr
065410070910    2c                   EndIf
065420070914    2c                   EndIf
065430070907    1c                   endif
065440070907     c
065450070907     c                   clear                   eliminatcp
065460070911     c                   clear                   aggtcp
065470130411
065480130411     c* Richiamo routine per invio variazione a pda
065490151230     c                   if        wfile='A'
065500130411     c                   exsr      sr_pda
065510160112     c                   endif
065520911209     C*
065530911212     C     ENDRG2        ENDSR
065540130930     c
065550130930     c* Aggiorno referente -----------------------------------------------------*
065560130930     c     AggioREF      BEGSR
065570130930     c
065580130930      * Referente e Telefono e magazzino giacenza
065590130930     c                   Eval      kAr5Trd = 'GEN'
065600130930     c     kAr5          Chain     Fiar501l
065610130930      * Aggiorno Fiar5
065620130930if  2c                   If        %Found(Fiar501l)
065630130930     c                   movel     ar5uni        DAr5Gen
065640130930     c                   Eval      §Ar5Ref = VidRef
065650130930     c                   Eval      §Ar5Teld = VidTeld
065660130930     c
065670130930     c* Solo per causale I0 aggiorno anche mag giacenza e f.deposito
065680130930     c                   if        flgcvr = 'I'
065690130930     c                   Eval      §Ar5Zng = VidZng
065700130930     c
065710130930     c                   if        wffd<>oldffd
065720130930     c                   if        wffd='S'
065730130930     c                   eval      §ar5dfd=%editc(dateu:'X')
065740130930     c                   else
065750130930     c                   clear                   §ar5dfd
065760130930     c                   endif
065770130930     c                   endif
065780130930     c
065790130930     c                   endif
065800130930     c
065810130930     c                   Movel(p)  DAr5Gen       Ar5Uni
065820130930      * se devo trasmettere alla sede sfleggo per la trasmissione
065830130930     c                   If        §Ar5Trse = 'S'
065840130930     c                   Clear                   Ar5Ft2
065850130930     c                   EndIf
065860130930      * sfleggo sempre per la trasmissione al cliente
065870130930     c                   Clear                   Ar5Ft3
065880130930     c                   Update    Fiar5000
065890130930      * Scrivo Fiar5
065900130930   x2c                   Else
065910130930      * se immessi i dati
065920130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
065930130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
065940130930      * di manutenzione bolle
065950130930if  3c                   If        VidRef <> *Blanks or VidTeld <> *Blanks or
065960130930     c                             VidZng <> *Blanks
065970130930     c                   Clear                   Fiar5000
065980130930     c                   Eval      Ar5Aas = ArbAas
065990130930     c                   Eval      Ar5Lnp = ArbLnp
066000130930     c                   Eval      Ar5Nrs = ArbNrs
066010130930     c                   Eval      Ar5Nsp = ArbNsp
066020130930     c                   Eval      Ar5Trd = 'GEN'
066030130930     c                   Move      Dateu         Ar5Dac
066040130930     c                   Movel     W0140         Ar5Orc
066050130930     c                   Clear                   Dar5Gen
066060130930     c                   Eval      §Ar5Ref = VidRef
066070130930     c                   Eval      §Ar5Teld = VidTeld
066080130930     c                   Eval      §Ar5Zng = VidZng
066090130930     c                   Movel(p)  DAr5Gen       Ar5Uni
066100130930     c                   Write     Fiar5000
066110130930    3c                   EndIf
066120130930    2c                   EndIf
066130130930     c                   ENDSR
066140911210     C*
066150911210     C*--- CONTROLLI FORMATO3 ----------------------------------------*
066160911210     C     CONTR3        BEGSR
066170941209     C*
066180920319     C* CONTROLLI COMUNI CON CONTR4
066190920319     C                   EXSR      CTRCOM
066200941209     C   90              GOTO      ENDC03
066210920319     C*
066220920319     C* T A S S A Z I O N E
066230941222     C* CONTROLLO L'IMPORTO DA ASSICURARE SE SONO IN ARRIVO SOLO PER
066240941222     C*  ASSEGNATO, IN PARTENZA SEMPRE
066250941222    1C     *IN36         IFEQ      *OFF
066260941222     C     *IN10         OREQ      *OFF
066270911211     C*
066280011205     C*** I M P O R T O  D A  A S S I C U R A R E  ***
066290080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
066300990127     C*
066310990127     C* IMPOSSIBILE INSERIRE IMPORTO DA ASSICURARE SE BOLLA SI C.A.
066320990127     C*  IN LIQ.TRANSATTIVA
066330080701    2C***  VIDIAS        IFGT      0
066340990127     C* VEDO SE C'E' C.A.
066350040809     C**""*KARB          CHAIN     FNDCT02L                           30
066360040809    3C**""**IN30         DOWEQ     *OFF
066370040809    4C**""*DCTATB        IFEQ      ' '
066380990127     C*
066390040809    5C**""*DCTFPR        IFEQ      'T'
066400040809     C**""*              MOVEL     MSG(82)       VIDMSG
066410040809     C**""*              SETON                                        30
066420040809     C**""*              SETON                                        409028
066430040809    5C**""*              ENDIF
066440040809    4C**""*              ENDIF
066450990127     C**
066460040809     C**""*  N30KARB          READE     FNDCT02L                               30
066470040809    3C**""*              ENDDO
066480040809      *
066490040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
066500040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
066510040809      * legate alla spedizione che passo
066520080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
066530080701      ***    NON FACCIAMO PIUI' IL CONTROLLO ****
066540080701     c*                  clear                   fidn12ds
066550080701     c*                  eval      i12tla = 'L'
066560080701     c*                  eval      i12aas = D48AAS
066570080701     c*                  eval      i12lnp = D48LNP
066580080701     c*                  eval      i12nrs = D48NRS
066590080701     c*                  eval      i12nsp = D48NSP
066600080701     c*                  eval      i12fel = 'E'
066610080701     c*                  eval      i12fan = 'E'
066620040809     c*
066630080701     c*                  call      'FIDN12R'
066640080701     c*                  parm                    fidn12ds
066650040809     c*
066660080701     c*                  setoff                                       30
066670040809     c*
066680040809     c* se non ci sono errori
066690080701    3C*                  if        o12err <> ' '
066700080701     c*                  seton                                        30
066710080701     c*                  else
066720040809     c* se numero di CA trovate maggiore di zero
066730080701    4C*                  if        o12nca = 0
066740080701     c*                  seton                                        30
066750080701     c*                  else
066760080701     c*                  z-add     1             jj                2 0
066770080701    5c*                  dow       jj <= o12nca and *in30 = *off
066780080701     c*                  movea     skkey(jj)     dskey
066790040809     c*
066800040809     c* Aggancio il FNDCT01L x reperire se la CA è in "Liquidazione Transattiva"
066810080701     c*    KDCT1_C       chain     FNDCT01L
066820080701    6c*                  if        %found(FNDCT01L)
066830080701    7C*    DCTFPR        IFEQ      'T'
066840080701     C*                  MOVEL     MSG(82)       VIDMSG
066850080701     C*                  SETON                                        30
066860080701     C*                  SETON                                        409028
066870080701    7C*                  ENDIF
066880080701    6c*                  endif
066890040809     c*
066900080701     c*                  add       1             jj
066910080701    5c*                  enddo
066920040809     c*
066930040809     c                   seton                                        30
066940040809     c*
066950080701    4c*                  endif
066960080701    3c*                  endif
066970990127     C**
066980080701     C** 90              GOTO      ENDC03
066990080701    2C***                ENDIF
067000941215     C*
067010941215     C* SE IL CAMPO = '   ' PRENDO LA DIVISA DALLA NAZIONE DEL MITTENT
067020941215     C* LO EVIDENZIO A VIDEO
067030950120     C* SE VARIATO L'IMPORTO ME LO SALVO
067040950120     C     *IN97         IFEQ      *ON
067050950120     C                   MOVEL     '1'           WIN97             1
067060950120     C                   ENDIF
067070950120     C*
067080941215     C*
067090941215     C                   MOVEL     VIDVAS        WVLT
067100941215     C                   Z-ADD     VIDIAS        W0133            13 3
067110941215     C* CONTROLLO LA DIVISA
067120941215     C                   EXSR      CTRVLT
067130941215     C                   MOVEL     WVLT          VIDVAS
067140941215     C* ERRORE
067150941215     C   90              SETON                                        41
067160941215     C   90              GOTO      ENDC03
067170011009      *
067180011009      * Controllo se la divisa immessa è scaduta o no
067190011009     C     ARBDSP        IFGT      §CVDLI
067200011105     C     VIDIAS        ANDGT     0
067210011102     C                   MOVEL     MSG(102)      VIDMSG
067220011009     C                   SETON                                        419028
067230011009     C                   GOTO      ENDC03
067240011009     C                   ENDIF
067250941215     C*
067260941215     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
067270941222    2C     VIDVAS        IFNE      VARVAS
067280950120     C     WIN97         ANDEQ     *BLANKS
067290941222    3C     VIDVAS        IFNE      *BLANKS
067300941219     C     VIDIAS        ORNE      0
067310941215     C                   MOVEL     MSG(28)       VIDMSG
067320941215     C                   SETON                                        409028
067330941215     C                   GOTO      ENDC03
067340941222    3C                   ENDIF
067350941219    2C                   ENDIF
067360941215     C*
067370941215     C                   MOVEL     VIDVAS        VARVAS
067380950120     C                   CLEAR                   WIN97
067390941215     C*
067400011205
067410011205      * pulisco il flag x la forzatura importo da assicurare forzabile
067420011205     c                   if        vidias <> sav1_vidias
067430011205     c                   clear                   forzaias
067440080625     c                   clear                   forzaias2
067450011205     c                   endif
067460011205      *---------------------
067470011205  2b c                   if        vidias <> 0
067480011205     c                   eval      sav1_vidias = vidias
067490011205      * Controllo l'importo da assicurare tramite il trul22r
067500011205     c                   clear                   trul22ds
067510011205     c                   eval      i22tla = 'L'
067520011205     c                   eval      i22cbo = arbcbo
067530011205     c                   eval      i22tsp = arbtsp
067540011205     c                   eval      i22lnp = arblnp
067550011205     c                   eval      i22nzm = arbnzm
067560011205     c                   eval      i22lna = arblna
067570011205     c                   eval      i22nzd = arbnzd
067580011205     c                   movel     vidklp        i22ksc
067590011205     c                   move      vidksc        i22ksc
067600011205     c                   eval      i22ctr = vidctr
067610011205     c                   eval      i22imp = vidias
067620011205     c                   eval      i22div = vidvas
067630011205     c                   eval      i22ute = knmus
067640011205     c                   eval      i22pgm = nompgm
067650011205     c                   call      'TRUL22R'
067660011205     c                   parm                    trul22ds
067670011205      * se ho tutti i flag impostati non posso immettere l'importo
067680011205      * da assicurare
067690011205  3b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
067700011205     c                             and o22fx2 <> *blanks
067710011205     c                   eval      vidmsg = msg(79)
067720011205     c                   eval      *in40 = *on
067730011205     c                   eval      *in90 = *on
067740011205     c                   eval      *in28 = *on
067750011206     c                   goto      endc03
067760011205  3e c                   endif
067770011205      * controllo che non sia minore del limite previsto
067780011205  3b c                   if        o22fmn <> *blanks
067790011205     c                   eval      vidmsg = msg(87)
067800011205     c                   eval      *in40 = *on
067810011205     c                   eval      *in90 = *on
067820011205     c                   eval      *in28 = *on
067830011206     c                   goto      endc03
067840011205  3e c                   endif
067850011205      * controllo che non sia maggiore del limite previsto
067860011205      * non forzabile
067870011205  3b c                   if        o22fx2 <> *blanks
067880011205     c                   eval      vidmsg = msg(39)
067890011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
067900011205     c                   eval      *in40 = *on
067910011205     c                   eval      *in90 = *on
067920011205     c                   eval      *in28 = *on
067930011206     c                   goto      endc03
067940011205  3e c                   endif
067950011205      * controllo che non sia maggiore del limite previsto
067960011205      * forzabile
067970011205  3b c                   if        o22fx1 <> *blanks and forzaias = *blanks
067980011205     c                   eval      forzaias = '1'
067990011205     c                   eval      vidmsg = msg(80)
068000011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
068010011205     c                   eval      *in40 = *on
068020011205     c                   eval      *in90 = *on
068030011205     c                   eval      *in28 = *on
068040011206     c                   goto      endc03
068050011205  3e c                   endif
068060011205  2e c                   endif
068070011205
068080941209     C*
068090941209     C* SE HA TARIFFE, CONTROLLO
068100941209    2C     WTKS          IFEQ      'S'
068110941209     C     KTAM4         CHAIN     TNTAM000                           30
068120941209    3C     *IN30         IFEQ      *OFF
068130991028     C                   MOVEL     TAMFLO        DSTA01
068140991028     C                   MOVEL     'CV'          COD
068150991028     C                   MOVEL(P)  §TADIV        KEY
068160991028     C                   EXSR      CTABEL
068170991028     C                   MOVEL     TBLUNI        DSCV
068180011010     C                   MOVEL     §CVDEC        COMDEC
068190941209     C*
068200941215     C* PULISCO CAMPI D FORZATURA
068210011010    4C     W0133         IFNE      VARIAS
068220941209     C                   CLEAR                   DIE
068230941215     C                   CLEAR                   CIN
068240941215    4C                   END
068250020715
068260020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
068270020715     c     §3atb1        Lookup    tbl                                    30
068280020715
068290020715   4Ac                   If        *in30 = *On
068300941209     C*
068310941209     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
068320941209     C*   ED E' ESPRESSO IL VALORE MERCE NELLA BOLLA
068330990818     C     KTPT          CHAIN     TITPT000                           30
068340950810    4C     *IN30         IFEQ      *ON
068350950810     C     TPTATB        ORNE      ' '
068360941222     C*
068370950810     C     *IN10         OREQ      *ON
068380941209     C     TPTAPL        ANDNE     'P'
068390941222     C*
068400941222     C     *IN10         OREQ      *OFF
068410941222     C     TPTAPL        ANDNE     'A'
068420941215     C*
068430011010     C                   Z-ADD     W0133         VARIAS
068440941215     C*
068450941215     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
068460941215     C*                                         IMPORTO DA TARIFFA
068470941215    5C     TAMFIS        IFEQ      '1'
068480941215     C     CIN           ANDNE     1
068490011010     C     W0133         ANDEQ     0
068500941215     C                   MOVEL     MSG(40)       VIDMSG
068510941215     C                   SETON                                        409028
068520941215     C                   ADD       1             CIN
068530941215     C                   GOTO      ENDC03
068540941215    5C                   ENDIF
068550941215     C*
068560941215     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
068570941215    5C     TAMFIS        IFEQ      '3'
068580011010     C     W0133         ANDEQ     0
068590941215     C                   MOVEL     MSG(42)       VIDMSG
068600941215     C                   SETON                                        409028
068610941215     C                   GOTO      ENDC03
068620941215    5C                   ENDIF
068630941215     C*
068640941215     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
068650941215    5C     TAMFIS        IFEQ      '2'
068660011010     C     W0133         ANDNE     0
068670941215     C                   MOVEL     MSG(43)       VIDMSG
068680941215     C                   SETON                                        409028
068690941215     C                   GOTO      ENDC03
068700941215    5C                   ENDIF
068710941215     C*
068720941215     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
068730950810    5C  N30TPTATB        IFEQ      ' '
068740011010     C     W0133         ANDGT     0
068750941215     C     DIE           ANDNE     1
068760941215     C     TPTVLM        ANDGT     0
068770941215     C*
068780941209     C* VALORE A COLLO
068790941215    6C     TPTFVM        IFEQ      '1'
068800991028     C     ARBNCL        MULT(H)   TPTVLM        TPTVAL           13 3
068810941215   X6C                   ELSE
068820941215    7C     TPTFVM        IFEQ      '2'
068830991028     C                   Z-ADD     TPTVLM        TPTVAL
068840941215   X7C                   ELSE
068850941212     C     ARBPKF        ADD       0,9           W0060             6 0
068860991028     C     W0060         MULT(H)   TPTVLM        TPTVAL
068870941215    7C                   END
068880941215    6C                   END
068890991028     C*
068900011205     C                   z-add     vidias        vidval           13 3
068910991028    7C     §TADIV        IFNE      VIDVAS
068920030612     C                   CLEAR                   YeurcoDS
068930991028     C                   MOVEL     VIDVAS        YECDVI
068940991028     C                   MOVEL     §TADIV        YECDVO
068950991028     C                   Z-ADD     VIDIAS        YECIMI
068960011010     C                   MOVE      COMDEC        YECDCO
068970991028     C*
068980991028     C                   CALL      'YEURCO'
068990030612     C                   PARM                    YeurcoDS
069000991028     C*
069010991028    8C     YECESI        IFEQ      ' '
069020991028     C                   Z-ADD     YECIMO        VIDVAL
069030991028    8C                   ENDIF
069040991028    7C                   ENDIF
069050941209     C*
069060991028    6C     VIDVAL        IFGT      TPTVAL
069070941209     C                   ADD       1             DIE
069080011205     c                   eval      vidmsg = msg(44)
069090941209     C                   SETON                                        409028
069100941209     C                   GOTO      ENDC03
069110941215    6C                   ENDIF
069120941209     C*
069130941215    5C                   ENDIF
069140941222    4C                   ENDIF
069150020715   4ac                   EndIf
069160941209    3C                   ENDIF
069170941209    2C                   ENDIF
069180030710
069190030710      * calcolo data limite mod. importo da assicurare
069200060201     c**   *iso          Move      ArbDsp        wdataiso
069210060201     c**                 adddur    §vpomia:*d    wdataiso
069220030722      *
069230060201     c**                 do        *hival
069240030722      * verifico se è un giorno lavorativo
069250060201     c**   *iso          move      wdataiso      ds_data
069260060201     C**                 eval      kann = ds_anno
069270060201     C**                 eval      kmes = ds_mese
069280060201     C**   kazcln        chain     azcln01l
069290060201     C**                 if        %found(azcln01l)
069300060201     C**                 if        MAT(ds_giorno) =  'F'  or
069310060201     C**                           POM(ds_giorno) =  'F'
069320030722      * se non va bene aggiungo 1 giorno
069330060201     c**                 adddur    1:*d          wdataiso
069340060201     C**                 iter
069350060201     c**                 else
069360060201     c**                 leave
069370060201     C**                 endif
069380060201     c**                 else
069390060201     c**                 leave
069400060201     C**                 endif
069410030722
069420060201     c**                 enddo
069430030710      * se viene modificato l'importo da assicurare rispetto a quanto
069440030710      * scritto sul file (stessa cosa anche x la divisa)
069450030710      * devo controllare se i gg. limite x la manutenzione sono trascorsi o meno
069460030710      * se sono già trascorsi emetto una window x richiedere la password
069470060201     c**                 If        (VidIas <> ArbIas or
069480060201     c**                           (VidVas <> ArbVas and ArbVas <> *Blanks)) and
069490060201     c**                            wdataoggi > wdataiso
069500060201     c**                           and wokpsw = *Blanks
069510060201     c**                 Exsr      Sr_Gespsw
069520060201     c** 90              GoTo      EndC03
069530060201     c**                 EndIf
069540941209     C***
069550011206     C*** V A L O R E   M E R C E   D I C H I A R A T O ***
069560941209     C* OBBLIGATORIO SE DESTINATARIO ESTERO E C'E' IN TABELLA NAZIONE
069570941209     C* IL FLAG EFTA
069580950120     C* SE HO VARIATO L'IMPORTO LO SALVO
069590950120     C     *IN93         IFEQ      *ON
069600950120     C                   MOVEL     '1'           WIN93             1
069610950120     C                   ENDIF
069620941209     C*
069630941209     C                   MOVEL     VIDVAD        WVLT
069640941209     C                   Z-ADD     VIDVMD        W0133            13 3
069650941209     C* CONTROLLO LA DIVISA
069660941209     C                   EXSR      CTRVLT
069670941209     C                   MOVEL     WVLT          VIDVAD
069680941209     C* ERRORE
069690941209     C   90              SETON                                        44
069700941209     C   90              GOTO      ENDC03
069710011010      *
069720011010      * Controllo se la divisa immessa è scaduta o no
069730011010     C     ARBDSP        IFGT      §CVDLI
069740011105     C     VIDVMD        ANDGT     0
069750011102     C                   MOVEL     MSG(102)      VIDMSG
069760011010     C                   SETON                                        449028
069770011010     C                   GOTO      ENDC03
069780011010     C                   ENDIF
069790941215     C*
069800941215     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE L'IMPORTO
069810941215     C* INDICATORE DI CHANGE 93 ON
069820941215    2C     VARVAD        IFNE      VIDVAD
069830950120     C     WIN93         ANDEQ     *BLANKS
069840941219     C     VIDVAD        IFNE      *BLANKS
069850941219     C     VIDVMD        ORNE      0
069860941215     C                   MOVEL     MSG(28)       VIDMSG
069870941215     C                   SETON                                        439028
069880941215     C                   GOTO      ENDC03
069890941215    2C                   ENDIF
069900941219     C                   ENDIF
069910941215     C*
069920941215     C                   MOVEL     VIDVAD        VARVAD
069930950120     C                   CLEAR                   WIN93
069940011205
069950011205      * pulisco il flag x la forzatura importo da assicurare forzabile
069960011205     c                   if        vidvmd <> sav1_vidvmd
069970011205     c                   clear                   forzavmd
069980011205     c                   endif
069990011205      *---------------------
070000011205     c                   eval      sav1_vidvmd = vidvmd
070010011205      * Controllo il valore merce tramite il trul23r
070020011205     c                   clear                   trul23ds
070030011205     c                   eval      i23tla = 'L'
070040011205     c                   eval      i23cbo = arbcbo
070050011205     c                   eval      i23tsp = arbtsp
070060011205     c                   eval      i23lnp = arblnp
070070011205     c                   eval      i23nzm = arbnzm
070080011205     c                   eval      i23lna = arblna
070090011205     c                   eval      i23nzd = arbnzd
070100011205     c                   movel     vidklp        i23ksc
070110011205     c                   move      vidksc        i23ksc
070120011205     c                   eval      i23ctr = vidctr
070130011206     c                   eval      i23imp = vidvmd
070140011206     c                   eval      i23div = vidvad
070150011205     c                   eval      i23ute = knmus
070160011205     c                   eval      i23pgm = nompgm
070170011205     c                   call      'TRUL23R'
070180011205     c                   parm                    trul23ds
070190011205      * se ho il flag del limite minimo impostato e l'importo è a zero
070200011205      * è obbligatorio
070210011206  2b c                   if        o23fmn <> *blanks and vidvmd = 0
070220011205     c                   eval      vidmsg = msg(45)
070230011205     c                   eval      *in43 = *on
070240011205     c                   eval      *in90 = *on
070250011205     c                   eval      *in28 = *on
070260011205     c                   goto      endc03
070270011206  2e c                   endif
070280011205      * controllo che non sia minore del limite previsto
070290011206  2b c                   if        o23fmn <> *blanks
070300011205     c                   eval      vidmsg = msg(104)
070310011210     c                   eval      %subst (vidmsg:37:14) = (%editc(o23lmn: '4'))
070320011205     c                   eval      *in43 = *on
070330011205     c                   eval      *in90 = *on
070340011205     c                   eval      *in28 = *on
070350011205     c                   goto      endc03
070360011206  2e c                   endif
070370011205      * controllo che non sia maggiore del limite previsto
070380011205      * non forzabile
070390011206  2b c                   if        o23fx2 <> *blanks
070400011205     c                   eval      vidmsg = msg(105)
070410011210     c                   eval      %subst (vidmsg:53:14) = (%editc(o23lx2: '4'))
070420011205     c                   eval      *in43 = *on
070430011205     c                   eval      *in90 = *on
070440011205     c                   eval      *in28 = *on
070450011205     c                   goto      endc03
070460011206  2e c                   endif
070470011205      * controllo che non sia maggiore del limite previsto
070480011205      * forzabile
070490011206  2b c                   if        o23fx1 <> *blanks and forzavmd = *blanks
070500011205     c                   eval      forzavmd = '1'
070510011205     c                   eval      vidmsg = msg(106)
070520011210     c                   eval      %subst (vidmsg:30:14) = (%editc(o23lx1: '4'))
070530011205     c                   eval      *in43 = *on
070540011205     c                   eval      *in90 = *on
070550011205     c                   eval      *in28 = *on
070560011205     c                   goto      endc03
070570011206  2e c                   endif
070580930507     C*
070590930507    1C                   END
070600950424     C*
070610950426     C*** CONTROLLI PER CLIENTE CON TARIFFA A VALORE CON FLAG = '3' ***
070620950426    1C     WTKS          IFEQ      'S'
070630950424     C                   MOVEL     VIDCTR        W001A             1
070640950426    2C     W001A         IFEQ      '4'
070650950424     C     TAMFVD        ANDEQ     '3'
070660950426     C*
070670950426    3C     VIDKLP        IFNE      VARKLP
070680950426     C     VIDKSC        ORNE      VARKSC
070690950426     C     VIDIAS        ORNE      COMIAS
070700950426     C                   SELECT
070710950426     C* FATTURA IMMEDIATA
070720950426     C* Se modificato il cod.cliente o l'importo da assicurare
070730950426     C* è necessario che la q.tà da fatturare e importi tassazione
070740950426     C* vengano azzerati + F14
070750950426     C     *IN14         WHENEQ    *ON
070760991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
070770991001     C                   MOVEL     '1'           WCKINL            1
070780991001     C                   EXSR      CARWAR
070790950426    4C     VIDQFT        IFNE      *ZEROS
070800950426     C     VIDIMV        ORNE      *ZEROS
070810950426     C     VIDPOR        ORNE      *ZEROS
070820991001     C     WINL          OREQ      '1'
070830950426     C     *INKN         OREQ      *OFF
070840950426     C                   SETON                                        429028
070850950426     C                   MOVEL     MSG(71)       VIDMSG
070860950426     C                   GOTO      ENDC03
070870950426    4C                   END
070880950426     C* SEGUE FATTURA
070890950426     C* Se modificato il cod.cliente o l'importo da assicurare
070900950426     C* è necessario che la q.tà da fatturare e tot imponibile
070910950426     C* vengano azzerati
070920950426     C     *IN13         WHENEQ    *ON
070930950426    4C     VIDQFT        IFNE      *ZEROS
070940950426     C     VIDIMV        ORNE      *ZEROS
070950950426     C                   SETON                                        429028
070960950426     C                   MOVEL     MSG(70)       VIDMSG
070970950426     C                   GOTO      ENDC03
070980950426    4C                   END
070990950426     C                   ENDSL
071000950426     C                   MOVE      VIDKLP        VARKLP
071010950426     C                   MOVE      VIDKSC        VARKSC
071020950426     C                   MOVE      VIDIAS        COMIAS
071030950426     C                   MOVE      *ZEROS        VARQFT
071040950426    3C                   END
071050950426     C*
071060950426     C*** QUANTITA' DA FATTURARE ***
071070950426     C*
071080950426     C* Errore se modificata e diversa da 0
071090950426    3C     VIDQFT        IFNE      VARQFT
071100950426    4C     VIDQFT        IFNE      *ZEROS
071110950426     C                   MOVEL     MSG(68)       VIDMSG
071120950424     C                   SETON                                        429028
071130950424     C                   GOTO      ENDC03
071140950426    4C                   END
071150950426     C* Se azzerata q.tà da fatturare gli
071160950426     C* importi della tassazione devono essere = 0
071170950426     C                   SELECT
071180950426    4C     *IN14         WHENEQ    *ON
071190991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
071200991001     C                   MOVEL     '1'           WCKINL            1
071210991001     C                   EXSR      CARWAR
071220950426    5C     VIDIMV        IFNE      *ZEROS
071230950426     C     VIDPOR        ORNE      *ZEROS
071240991001     C     WINL          OREQ      '1'
071250950426     C     *INKN         OREQ      *OFF
071260950426     C                   SETON                                        499028
071270950426     C                   MOVEL     MSG(69)       VIDMSG
071280950426     C                   GOTO      ENDC03
071290950426    5C                   END
071300950426    4C     *IN13         WHENEQ    *ON
071310950426    5C     VIDIMV        IFNE      *ZEROS
071320950426     C                   SETON                                        499028
071330950426     C                   MOVEL     MSG(72)       VIDMSG
071340950426     C                   GOTO      ENDC03
071350950426    5C                   END
071360950426    4C                   ENDSL
071370950424     C                   MOVE      VIDQFT        VARQFT
071380950426    3C                   END
071390950424     C*
071400950426    2C                   END
071410950426    1C                   END
071420950424     C*
071430050601     c* Controlli su aggiunte/aumenti di importo da assicurare
071440080625    1c                   If        (VidIas <> ArbIas and vidias > 0 or
071450050804     c                             (VidVas <> ArbVas and vidVas <> *Blanks
071460050804     c                             and Vidias>0))
071470050601     c* verifico se cliente con mandato
071480050601     c                   exsr      tassar
071490050601     c* cliente senza mandato: si può aggiungere/aumentare l'importo solo
071500050601     c* se inviata e-mail
071510050601     c* idem per cliente con mandato se il valore è oltre mandato
071520080625    2c                   if        task88 = 'S'
071530050601     c* vedo se inviata e-mail
071540050609     c                   movel     'c'           kta4trc
071550050601     c                   clear                   dta4c
071560050601     c     kta4          chain     tita430c
071570080625    3c                   if        %found(tita430c)
071580050601     c                   movel     ta4not        dta4c
071590080625    3c                   endif
071600080625    3c                   if        §ta4mail <> 'S'
071610080625    4c                   if        §utemtf=' '
071620050601     c                   seton                                        409028
071630050601     c                   eval      vidmsg=msg(135)
071640050601     c                   goto      endc03
071650080625    4c                   endif
071660080625    4c                   if        (§utemtf='I' or §utemtf='E') and
071670080625     c                             forzaias2=' '
071680080625     c                   eval      forzaias2='1'
071690080625     c                   seton                                        409028
071700080625     c                   eval      vidmsg=msg(146)
071710080625     c                   goto      endc03
071720080625    4c                   endif
071730080625    3c                   endif
071740080625    2c                   endif
071750080625    1c                   endif
071760941221     C*
071770941221     C* F14 - TASSAZIONE
071780991011     C     *INKN         IFEQ      *ON
071790991029     C* Fattura immediata SE NON CONTABILIZZATA
071800991011     C     *IN14         OREQ      *ON
071810991029     C     *IN29         ANDEQ     *OFF
071820991029     c* Prepagati NON CONTABILIZZATA
071830991011     C     *IN10         OREQ      *OFF
071840991011     C     VIDNFT        ANDGT     *ZEROS
071850991029     C     *IN29         ANDEQ     *OFF
071860980513     C                   MOVEL     '1'           WBOL              1
071870011018      * Se è una bolla di reso con varia 'N' 88888888
071880011017      * tasso la varia 'N'
071890011018     C     FLGVRN        IFEQ      '1'
071900011017      * leggo il sbfl delle varie
071910011017     C                   DO        20            NRR
071920011017     C     NRR           CHAIN     LR48S03                            32
071930011017     C     V3CSV1        IFEQ      'N'
071940011017     C     V3CVA1        ANDEQ     88888888
071950011017     C                   EXSR      TASSAN
071960011017     C  N32              UPDATE    LR48S03
071970011017     C                   ENDIF
071980011017     C                   ENDDO
071990011017     C                   ENDIF
072000011017      *
072010151020     c                   if        vidimv<>PesVolImv
072020151016     c                   clear                   PesVolPkcn
072030151016     c                   clear                   PesVolSpc
072040151016     c                   clear                   PesVolImv
072050151020     c                   endif
072060151020     c***                clear                   Tassatoimv
072070941221     C                   EXSR      TASSAZ
072080151019     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
072090151020     c***                if        vidimv>0 and vidimv<>PesVolImv
072100151020     c***                eval      PesVolPkcn=taspkcn
072110151020     c***                eval      PesVolSpc=tasspc
072120151020     c***                eval      PesVolIMV=vidimv
072130151020     c***                endif
072140990702     C* SE SI TRATTA DI UN PREPAGATO DEVO ANCHE VISUALIZZARE IL TOT
072150990702     C*  IMPONIBILE ARROTONDATO
072160990702     C     *IN23         IFEQ      *ON
072170151020     c****               eval      TassatoImv=vidimv
072180990702     C                   Z-ADD     AR6DFT        ARBDFT
072190990702     C                   EXSR      TOTFAT
072200990702     C                   Z-ADD     D04IFT        VIDIFT
072210990702     C                   Z-ADD     D04IMV        VIDIMV
072220990702     C                   Z-ADD     D04POR        VIDPOR
072230991006     C                   Z-ADD     D04POR        VARPOR
072240990702     C                   Z-ADD     D04IMV        SAVIMV
072250990702     C                   ENDIF
072260151020     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
072270151020     c                   if        vidimv>0 and vidimv<>PesVolImv
072280151020     c                   eval      PesVolPkcn=taspkcn
072290151020     c                   eval      PesVolSpc=tasspc
072300151020     c                   eval      PesVolIMV=vidimv
072310151020     c                   endif
072320131016
072330131016     c                   if        *inkn
072340131016     c                   eval      winf14='1'
072350131016     c                   endif
072360991011     C   KN
072370991011     COR 90              GOTO      ENDC03
072380990702    1C                   END
072390991006     C* Se divisa sprotetta significa che manca tariffa: in questo caso
072400991006     C* controllo che la divisa inserita sia = alla divisa della moneta
072410991006     C*  di conto SE CLIENTE 8888 o 9999
072420991006     C*
072430991006     C* Fattura immediata
072440991006     C     *IN14         IFEQ      *ON
072450991006     C* Prepagati
072460991006     C     *IN10         OREQ      *OFF
072470991006     C     VIDNFT        ANDGT     *ZEROS
072480991006     C* Divisa sprotetta
072490991006     C     *IN07         IFEQ      *OFF
072500991006     C* Divisa diversa dalla divisa della moneta di conto
072510991006     C     VIDDIV        ANDNE     §GEDCN
072520991006     C* Cliente 8888 o 9999
072530991006     C     VIDKSC        IFEQ      8888
072540991006     C     VIDKSC        OREQ      9999
072550991006     C                   SETON                                        459028
072560991006     C                   MOVEA     MSG(90)       K78
072570991006     C                   MOVEA     §GEDCN        K78(71)
072580991006     C                   MOVEA     K78           VIDMSG
072590991006     C                   GOTO      ENDC03
072600991006     C                   ENDIF
072610991006     C                   ENDIF
072620991006     C                   ENDIF
072630941102     C**
072640991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
072650991124     C* TASSAZIONE
072660991124    1C     *IN07         IFEQ      *OFF
072670991124     C     VIDDIV        ANDNE     WDIV
072680991124     C     VIDDIV        ANDEQ     *BLANKS
072690991124    2C     VIDPOR        IFGT      *ZEROS
072700991125     C                   SETON                                        459028
072710991124     C                   MOVEL     MSG(94)       VIDMSG
072720991124     C                   GOTO      ENDC03
072730991124   X2C                   ELSE
072740991124    3C                   DO        20            NRR
072750991124     C     NRR           CHAIN     LR48S03                            30
072760991124    4C     V3CVA1        IFGT      *ZEROS
072770991125     C                   SETON                                        459028
072780991124     C                   MOVEL     MSG(94)       VIDMSG
072790991124     C                   GOTO      ENDC03
072800991124    4C                   ENDIF
072810991124    3C                   ENDDO
072820991124    2C                   ENDIF
072830991124    1C                   ENDIF
072840991005     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
072850991012     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
072860991012     C* CONTROLLO DELLA DIVISA
072870991013    0C     *IN07         IFEQ      *OFF
072880991013     C     VIDDIV        ANDNE     WDIV
072890991026     C     WDIV          ANDNE     *BLANKS
072900100120     c
072910030612     C                   CLEAR                   Fnlv16DS
072920991012     C                   CLEAR                   DTASV
072930100120     C                   CLEAR                   dlv1601
072940100120
072950991012     C                   MOVEL     'D'           D17FIM
072960991012     C                   MOVE      VIDDIV        D17DIV
072970991012     C     VIDDFT        IFGT      *ZEROS
072980991012     C                   CLEAR                   WLBDAT
072990991012     C                   Z-ADD     VIDDFT        G02DAT
073000991012     C                   MOVEL     *BLANK        G02ERR
073010991012     C                   CALL      'XSRDA8'
073020991012     C                   PARM                    WLBDAT
073030991012     C                   Z-ADD     G02INV        D17DSP
073040991012     C                   ELSE
073050991012     C                   Z-ADD     ARBDSP        D17DSP
073060991012     C                   END
073070991012     C                   CALL      'FNLV16R'
073080030612     C                   PARM                    Fnlv16DS
073090991012     C                   PARM                    DTASV
073100991014     C                   MOVEL     D17DIV        VIDDIV
073110991012   X0C                   ELSE
073120030612     C                   CLEAR                   Fnlv16DS
073130991005     C                   CLEAR                   DTASV
073140941222     C                   MOVEL     D48TBO        D17TBO
073150991006     C   23              MOVEL     'S'           D17PRP
073160941209     C                   MOVEL     KSC           D17KSC
073170991012     C     VIDDFT        IFGT      *ZEROS
073180991012     C                   CLEAR                   WLBDAT
073190991012     C                   Z-ADD     VIDDFT        G02DAT
073200991012     C                   MOVEL     *BLANK        G02ERR
073210991012     C                   CALL      'XSRDA8'
073220991012     C                   PARM                    WLBDAT
073230991012     C                   Z-ADD     G02INV        D17DSP
073240991012     C                   ELSE
073250991012     C                   Z-ADD     ARBDSP        D17DSP
073260991012     C                   END
073270941209     C                   MOVEL     ARBLNP        D17LNP
073280941209     C                   MOVEL     ARBLNA        D17LNA
073290941209     C                   MOVEL     §3ATB1        D17TBL
073300941209     C                   MOVEL     §3ARBL        D17RBL
073310020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
073320100120     C                   MOVEL     §3ASVA        §D17sva
073330100120
073340100120     c* Se fattura immediata o prepagato
073350100120     c*                        controllo evetuale esenzione iva
073360100121     c                   if        *in14   or (not *in10 and vidnft>0)
073370100120     c                   Eval      §d17ctriso='S'
073380100120     c                   if        arbcpi<>*blanks
073390100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
073400100120     c* solo numeri
073410100120     c                   select
073420100120     c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
073430100120     c                   eval      §d17iso='IT'
073440100120     c* "PRIVATO" senza iso
073450100120     c                   when      *in30 and posiz<=3
073460100120     c                   eval      §d17iso='$$'
073470100120     c                   other
073480100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
073490100120     c                   ENDSL
073500100120     c                   endif
073510100120     c                   endif
073520100120     c
073530100120     c                   Movel     Dlv1601       D17Flo
073540100120     c
073550941209     C                   Z-ADD     VIDIAS        D17IAS
073560941209     C                   Z-ADD     VIDQFT        D17QFT
073570941209     C                   Z-ADD     VIDPOR        D17POR
073580991005     C                   MOVEL     VIDDIV        D17DIV
073590991006     C                   MOVEL     WDIV          D17DVP
073600941209     C                   Z-ADD     VIDIMV        D17IMV
073610990702    1C     §3AFCA        IFEQ      1
073620941209     C                   MOVEL     'S'           D17FCA
073630990702    1C                   ENDIF
073640990702    1C* SE C'E' NUMERO E DATA FATTURA RIPASSO IL TOTALE IMPONIBILE
073650950114     C     *IN14         IFEQ      *ON
073660991005     C                   MOVEL     'E'           D17FIM
073670950114     C*
073680990702   X1C                   ELSE
073690950114     C*
073700950114     C                   MOVEL     'C'           D17FIM
073710990702    2C     VIDFIV        IFGT      0
073720950114     C     VIDNFT        ANDGT     0
073730950114     C     VIDDFT        ANDGT     0
073740991005     C                   MOVEL     'E'           D17FIM
073750990702    2C                   ENDIF
073760990702    1C                   ENDIF
073770941209     C                   MOVEL     VIDCEI        D17CEI
073780991029     C* SE PROTETTA LA TASSAZIONE, NON PASSO I DATI DELLA FATTURA
073790941209     C                   MOVEL     VIDFIV        D17FIV
073800941220     C                   MOVEL     'S'           D17FFI
073810941209     C                   MOVEL     VIDNFT        D17NFT
073820941209     C                   Z-ADD     VIDDFT        D17DFG
073830941209     C*
073840991012     C* RICHIAMO ROUTINE PER CARICAMENTO VARIE DELLA DTASV
073850991012     C                   EXSR      RDTASV
073860941209     C*
073870991005     C                   CALL      'FNLV16R'
073880030612     C                   PARM                    Fnlv16DS
073890991005     C                   PARM                    DTASV
073900941102     C*
073910991011     C                   MOVEL     D17DIV        VIDDIV
073920941209     C                   MOVEL     D17CEI        VIDCEI
073930991005     C                   MOVEL     O17DEI        DESCEI
073940991005     C                   MOVEL     O17DEI        V3DCEI
073950991006     C     D17FIM        IFEQ      'E'
073960991014     C     O17ERR        ANDEQ     *BLANKS
073970030620      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
073980030620      * SOLO SE LA VARIA ESISTE
073990040123     C**!!!§3ASVA        ORNE      *BLANKS
074000040123     C**!!!WVAR3A        ANDEQ     'S'
074010040123     C**!!!O17ERR        ANDEQ     *BLANKS
074020040123     C**!!!D17FIM        ANDEQ     'C'
074030991005     C                   MOVEL     O17IMV        VIDIMV
074040991006     C                   ENDIF
074050030620      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
074060030620     C     §3ASVA        IFNE      *BLANKS
074070030620     C     WVAR3A        ANDNE     'S'
074080030620     C     D17FIM        ANDEQ     'C'
074090030620     C                   Z-ADD     *ZEROS        VIDIMV
074100030620     C                   ENDIF
074110030620      ****
074120991029     C                   MOVEL     D17DFG        VIDDFT
074130941209     C* VARIE
074140990702    1C                   DO        20            NRR
074150941215     C     NRR           CHAIN     LR48S03                            30
074160941209     C                   MOVEL     VSV(NRR)      V3CSV1
074170941209     C                   MOVEL     VES(NRR)      V3CEV1
074180941215     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
074190991006    2C     O17ERV        IFEQ      NRR
074200941220     C                   SETON                                        5290
074210941215     C                   ELSE
074220941215     C                   SETOFF                                       52
074230990702    2C                   ENDIF
074240941215     C  N30              UPDATE    LR48S03
074250941215     C   30              WRITE     LR48S03
074260990702    1C                   ENDDO
074270991012    0C                   ENDIF
074280941215     C*
074290941215     C                   SETOFF                                       52
074300941102     C*  ERRORE
074310991006    1C     O17ERR        IFNE      *BLANKS
074320991006     C     O17FAS        OREQ      'S'
074330941102     C*
074340991006    2C     O17MSG        IFNE      *BLANKS
074350991006     C                   MOVEL     O17MSG        VIDMSG
074360941209     C                   SETON                                        28
074370941102    2C                   ENDIF
074380990415     C**
074390990415     C* VARIA R  MSG
074400991006    2C     O17ERR        IFEQ      '6'
074410991006     C     O17MSG        ANDEQ     MSG(85)
074420991006     C     O17FAS        OREQ      'S'
074430990416     C*
074440990415     C     COMASS        IFEQ      ' '
074450990415     C* SE MODIFICATO L'IMPORTO D'ASSICURARE E FATTURA GIA' INCASSATA O TASSATA
074460990415    3C     VIDIAS        IFNE      ARBIAS
074470990415     C     VIDVAS        ORNE      ARBVAS
074480990415     C*
074490990415     C* SE GIA' TASSATO FINO ALL'IMPONIBILE E LA DATA FATTURA > 0
074500990415    4C     AR6IMV        IFNE      *ZEROS
074510990415     C                   SETON                                        2882
074520990415     C                   SETOFF                                       90
074530990415     C* ERRORE NON BLOCCANTE:CORREGGERE  VARIA R
074540990419     C                   SELECT
074550990419    5C     ARBIAS        WHENEQ    0
074560990415     C     VIDIAS        ANDGT     0
074570990415     C                   MOVEL     MSG(83)       VIDMSG
074580990419    5C     ARBIAS        WHENGT    0
074590990419     C     VIDIAS        ANDEQ     0
074600990415     C                   MOVEL     MSG(84)       VIDMSG
074610990419     C                   OTHER
074620990419     C                   MOVEL     MSG(86)       VIDMSG
074630990419    5C                   ENDSL
074640990415    4C                   END
074650990415     C                   MOVEL     '1'           COMASS            1
074660990415    3C                   END
074670990415     C                   ELSE
074680990415     C                   SETOFF                                       9028
074690990415     C                   MOVEL     *BLANKS       VIDMSG
074700990415     C                   END
074710990415    2C                   END
074720990415     C* FATTURA
074730941102     C**
074740941102     C* ESENZIONE IVA
074750991006    2C     O17ERR        IFEQ      '1'
074760941209     C                   SETON                                        5190
074770941102    2C                   ENDIF
074780941102     C**
074790941102     C* IMPONIBILE
074800991006     C     O17ERR        IFEQ      '5'
074810941209     C                   SETON                                        5090
074820941102    2C                   ENDIF
074830941222     C* NUMERO FATTURA
074840991006     C     O17ERR        IFEQ      '2'
074850941222     C                   SETON                                        5590
074860941222    2C                   ENDIF
074870941222     C* FILIALE IVA FATTURA
074880991006     C     O17ERR        IFEQ      '3'
074890941222     C                   SETON                                        5490
074900941222    2C                   ENDIF
074910941222     C* DATA FATTURA
074920991006     C     O17ERR        IFEQ      '4'
074930941222     C                   SETON                                        5690
074940941222    2C                   ENDIF
074950991006     C* DIVISA
074960991006     C     O17ERR        IFEQ      '7'
074970991006     C                   SETON                                        4590
074980991006    2C                   ENDIF
074990991012     C* PORTO
075000991012     C     O17ERR        IFEQ      '8'
075010001116     C                   SETON                                        4990
075020991012    2C                   ENDIF
075030941102     C**
075040941220     C                   GOTO      ENDC03
075050941102    1C                   ENDIF
075060040123      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
075070040123      * non posso avere altre varia con importo > 0
075080040123if  1c                   If        §3asva <> *Blanks
075090040123do  2c                   Do        20            nrr
075100040123     c     nrr           Chain     Lr48s03                            30
075110040123     c                   Eval      *In52 = *Off
075120040123if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
075130040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
075140040123     c                   Eval      *In28 = *On
075150040123     c                   Eval      *In52 = *On
075160040123     c                   Eval      *In90 = *On
075170040123     c                   Eval      VidMsg = Msg(127)
075180040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
075190040123    3c                   EndIf
075200040123if  3c                   If        wvar3a = *Blanks  and
075210040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
075220040123     c                   Eval      *In28 = *On
075230040123     c                   Eval      *In52 = *On
075240040123     c                   Eval      *In90 = *On
075250040123     c                   Eval      VidMsg = Msg(127)
075260040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
075270040123    3c                   EndIf
075280040123     c  n30              Update    Lr48s03
075290040127     c   90              Goto      Endc03
075300040123    2c                   EndDo
075310040123    1c                   EndIf
075320991006     C*
075330991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
075340991006    1C  N07VIDDIV        IFNE      WDIV
075350991006     C* CONVERSIONE PORTO
075360991006    2C     VIDPOR        IFEQ      VARPOR
075370991006     C     VIDPOR        ANDGT     *ZEROS
075380991006     C     WDIV          ANDNE     *BLANKS
075390030612     C                   CLEAR                   YeurcoDS
075400991006     C                   MOVEL     WDIV          YECDVI
075410991006     C                   Z-ADD     VIDPOR        YECIMI
075420991006     C                   MOVEL     VIDDIV        YECDVO
075430991006     C     O17FDC        IFEQ      'S'
075440011114     C                   MOVE      '3'           YECDCO
075450991006     C                   ENDIF
075460991006     C                   CALL      'YEURCO'
075470030612     C                   PARM                    YeurcoDS
075480991006     C                   Z-ADD     YECIMO        VIDPOR
075490991012     C                   SETON                                        49
075500991006    2C                   END
075510991006     C* CONVERSIONE VARIE
075520991006    2C                   DO        20            NRR
075530991006     C     NRR           CHAIN     LR48S03                            30
075540991006    3C     V3CVA1        IFEQ      VARVA1
075550991006     C     V3CVA1        ANDGT     *ZEROS
075560991006     C     WDIV          ANDNE     *BLANKS
075570011019      *
075580011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
075590011019      * non devo convertire la varia 'N' 88888888
075600011019     C     VIDIMV        IFEQ      *ZEROS
075610011019     C     FLGVRN        ANDEQ     '1'
075620011019     C     V3CSV1        ANDEQ     'N'
075630011019     C     V3CVA1        ANDEQ     88888888
075640011019     C  N30              UPDATE    LR48S03
075650011019     C                   ITER
075660011019     C                   ENDIF
075670030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
075680030624      * non devo convertire la varia '&' 88888888
075690030624     C     VIDIMV        IFEQ      *ZEROS
075700030624     C     V3CSV1        ANDEQ     '&'
075710030624     C     V3CVA1        ANDEQ     88888888
075720030624     C  N30              UPDATE    LR48S03
075730030624     C                   ITER
075740030624     C                   ENDIF
075750011019      *
075760030612     C                   CLEAR                   YeurcoDS
075770991006     C                   MOVEL     WDIV          YECDVI
075780991006     C                   Z-ADD     V3CVA1        YECIMI
075790991006     C                   MOVEL     VIDDIV        YECDVO
075800991006     C     O17FDC        IFEQ      'S'
075810011114     C                   MOVE      '3'           YECDCO
075820991006     C                   ENDIF
075830991006     C                   CALL      'YEURCO'
075840030612     C                   PARM                    YeurcoDS
075850991006     C                   Z-ADD     YECIMO        V3CVA1
075860991012     C                   SETON                                        53
075870991006    3C                   ENDIF
075880991006     C                   Z-ADD     V3CVA1        VARVA1
075890991006     C  N30              UPDATE    LR48S03
075900991006    2C                   ENDDO
075910991006     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
075920991006     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
075930991006     C     VIDIMV        IFGT      *ZEROS
075940991006     C     WDIV          ANDNE     *BLANKS
075950991006     C                   MOVEL     'T'           D17FIM
075960991006     C                   MOVEL     *ZEROS        D17IMV
075970991006     C                   Z-ADD     VIDPOR        D17POR
075980991012     C                   CLEAR                   DTASV
075990991012     C                   EXSR      RDTASV
076000991006     C                   CALL      'FNLV16R'
076010030612     C                   PARM                    Fnlv16DS
076020991006     C                   PARM                    DTASV
076030991006     C                   Z-ADD     O17IMV        VIDIMV
076040991006     C                   ENDIF
076050991006     C                   Z-ADD     VIDPOR        VARPOR
076060991006     C                   MOVEL     VIDDIV        WDIV
076070991012     C* SE VARIATA DIVISA RIEMETTO IN OGNI CASO LA VIDEATA
076080991029     C* PERCHE' L'LV16 DOVRA' CONTROLLARE LA TASSAZIONE
076090991012     C                   SETON                                        90
076100991012     C                   GOTO      ENDC03
076110991006   X1C                   ELSE
076120991006     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
076130991006     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
076140991006     C                   Z-ADD     VIDPOR        VARPOR
076150991006    2C                   DO        20            NRR
076160991006     C     NRR           CHAIN     LR48S03                            30
076170991006     C                   Z-ADD     V3CVA1        VARVA1
076180991006     C  N30              UPDATE    LR48S03
076190991006    2C                   ENDDO
076200991006    1C                   ENDIF
076210941102     C*
076220941102     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
076230941209     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
076240991006    3C     O17FAS        IFEQ      'S'
076250941102     C*
076260941102     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
076270990702    2C     SEI           IFNE      1
076280941209     C                   MOVEL     MSG(46)       VIDMSG
076290941209     C                   SETON                                        5290
076300941102     C                   ADD       1             SEI
076310941102     C                   GOTO      ENDC03
076320990702    2C                   ENDIF
076330990702    1C                   ENDIF
076340941222     C*
076350941222     C* IN PARTENZA SE C'ERA PREPAGATO, DEVO LASCIARLO
076360990702    1C     *IN10         IFEQ      *OFF
076370990702    2C     AR6NFT        IFNE      0
076380941222     C     VIDNFT        ANDEQ     0
076390941222     C                   SETON                                        559028
076400941222     C                   MOVEL     MSG(60)       VIDMSG
076410941222     C                   GOTO      ENDC03
076420941222    2C                   ENDIF
076430990702     C*
076440990702     C* SE PREPAGATO INSERIRE TASSAZIONE O F14 PER TASSARE
076450990702     C  NKL
076460990702    2CANNKN*IN23         IFEQ      *ON
076470990702    3C     VIDIMV        IFEQ      0
076480990702     C                   SETON                                        529028
076490990702     C                   MOVEL     MSG(89)       VIDMSG
076500990702     C                   GOTO      ENDC03
076510990702   X3C                   ELSE
076520990702    4C     VIDIMV        IFNE      SAVIMV
076530990702     C                   Z-ADD     AR6DFT        ARBDFT
076540990702     C                   EXSR      TOTFAT
076550990702     C                   Z-ADD     D04IFT        VIDIFT
076560990702     C                   Z-ADD     D04IMV        VIDIMV
076570990702     C                   Z-ADD     D04POR        VIDPOR
076580991006     C                   Z-ADD     D04POR        VARPOR
076590990702     C                   Z-ADD     VIDIMV        SAVIMV
076600990702     C                   SETON                                        90
076610990702    4C                   ENDIF
076620990702    3C                   ENDIF
076630990702    2C                   ENDIF
076640990702    1C                   ENDIF
076650011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
076660011017     C                   SETOFF                                       52
076670011018    1C     FLGVRN        IFEQ      '1'
076680011017    2C                   DO        20            NRR
076690011017     C     NRR           CHAIN     LR48S03                            32
076700011017    3C     *IN32         IFEQ      *OFF
076710011018     C                   SETON                                        52
076720011018    4C     V3CSV1        IFEQ      'N'
076730011017     C                   SETOFF                                       52
076740011017     C                   LEAVE
076750011018    4C                   ENDIF
076760011017    3C                   ENDIF
076770011017    2C                   ENDDO
076780011018    2C     *IN52         IFEQ      '1'
076790011017     C     1             CHAIN     LR48S03                            32
076800011017     C  N32              UPDATE    LR48S03
076810011017     C                   MOVEL     MSG(101)      VIDMSG
076820011105     C                   SETON                                        9028
076830011017     C                   GOTO      ENDC03
076840011017    2C                   ENDIF
076850011017    1C                   ENDIF
076860011206      *
076870011206      * Controllo che la somma degli importi immessi non superi il valore
076880011206      * impostato in §GEIMF
076890011206      *      prima controllo il totale imponibile
076900011206  1b c                   if        vidimv <> 0
076910011206     c                   eval      w0173 = vidimv
076920011206      *      controllo se devo convertire il totale imponibile
076930011206  2b c                   if        viddiv <> §gedcn
076940030612     c                   clear                   YeurcoDS
076950011206     c                   eval      yecdvi = viddiv
076960011206     c                   eval      yecdvo = §gedcn
076970011206     c                   eval      yecimi = vidimv
076980011206     c                   move      decsav        yecdco
076990011206     c                   call      'YEURCO'
077000030612     c                   parm                    YeurcoDS
077010011206  3b c                   if        yecesi = ' '
077020011206     c                   eval      w0173 = yecimo
077030011206  3e c                   endif
077040011206  2e c                   endif
077050011206      *      controllo se l'imponibile supera il limite fatturabile
077060011220  2b c                   if        w0173 > imfsav
077070011206     c                   eval      vidmsg = msg(107)
077080011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
077090011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
077100011206     c                   eval      *in50 = *on
077110011206     c                   eval      *in90 = *on
077120011206     c                   eval      *in28 = *on
077130011206     c                   goto      endc03
077140011206  2e c                   endif
077150011206  1x c                   else
077160011206      *      altrimenti controllo tutti gli altri importi
077170011206     c                   eval      tot_importi = vidpor
077180011206      *      leggo le varie
077190011206  2b c                   do        20            nrr
077200011206     c     nrr           chain     lr48s03                            32
077210011206  3b c                   if        *in32 = *off and v3cva1 <> 0
077220011206  4b c                   if        flgvrn = '1' and v3cva1 = 88888888
077230011206     c                             and v3csv1 = 'N'
077240011206     c                   iter
077250011206  4e c                   endif
077260030624  4b c                   if        v3cva1 = 88888888
077270030624     c                             and v3csv1 = '&'
077280030624     c                   iter
077290030624  4e c                   endif
077300011206     c                   eval      tot_importi = tot_importi + v3cva1
077310011206  3e c                   endif
077320011206  2e c                   enddo
077330011206     c                   eval      w0173 = tot_importi
077340011206      *      controllo se devo convertire il totale importi
077350011206  2b c                   if        viddiv <> §gedcn
077360030612     c                   clear                   YeurcoDS
077370011206     c                   eval      yecdvi = viddiv
077380011206     c                   eval      yecdvo = §gedcn
077390011206     c                   eval      yecimi = tot_importi
077400011206     c                   move      decsav        yecdco
077410011206     c                   call      'YEURCO'
077420030612     c                   parm                    YeurcoDS
077430011206  3b c                   if        yecesi = ' '
077440011206     c                   eval      w0173 = yecimo
077450011206  3e c                   endif
077460011206  2e c                   endif
077470011206      *      controllo se l'imponibile supera il limite fatturabile
077480011220  2b c                   if        w0173 > imfsav
077490131016     c                   eval      vidmsg = msg(107)
077500011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
077510011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
077520011206     c                   eval      *in50 = *on
077530011206     c                   eval      *in90 = *on
077540011206     c                   eval      *in28 = *on
077550011206     c                   goto      endc03
077560011206  2e c                   endif
077570011206  1e c                   endif
077580131016     c
077590131016     c* CONTROLLI PER BOLLA CONTABILIZZATA E PGM NON RICHIAMATO (29 ON)
077600131016     c* se f6=conferma
077610131016     c                   if        *in29 and *inkf
077620131016     c* Se eseguito F14=Tassazione --> errore bolla contabilizzata
077630131016     c                   if        winf14='1'
077640131016     c                   eval      vidmsg = msg(151)
077650131016     c                   eval      *in90 = *on
077660131016     c                   eval      *in28 = *on
077670131016     c                   goto      endc03
077680131016     c                   endif
077690131016     c* Se non variato nè ias nè vmd -> errore premere F12
077700131016     c                   if        vidias=bol_vidias and
077710131016     c                             (vidvas=bol_vidvas or vidias=0) and
077720131016     c                             vidvmd=bol_vidvmd and
077730131016     c                             (vidvad=bol_vidvad or vidvmd=0)
077740131016     c                   eval      vidmsg = msg(152)
077750131016     c                   eval      *in90 = *on
077760131016     c                   eval      *in28 = *on
077770131016     c                   goto      endc03
077780131016     c                   endif
077790131016     c                   endif
077800911210     C*
077810941209     C     ENDC03        ENDSR
077820030710      *---------------------------------------------------------------*
077830030710      * Gestione videata password x importo da assicurare             *
077840030710      *---------------------------------------------------------------*
077850060201     c**   Sr_Gespsw     BegSr
077860060201     C**
077870060201     c**                 Clear                   w01psw
077880060201     c**                 Do        *Hival
077890060201     c**                 Exfmt     lr48w01
077900060201     c**                 Eval      *In28 = *Off
077910060201     c**                 If        *InKl
077920060201     c**                 Eval      *In90 = *On
077930060201     c**                 Leave
077940060201     c**                 EndIf
077950060201     c**                 If        w01psw = *Blanks
077960060201     c**                 Eval      w01msg = 'Immettere la password'
077970060201     c**                 Eval      *In28 = *On
077980060201     c**                 Iter
077990060201     c**                 EndIf
078000060201     c**                 If        w01psw <> miapsw
078010060201     c**                 Eval      w01msg = 'Password errata'
078020060201     c**                 Eval      *In28 = *On
078030060201     c**                 Iter
078040060201     c**                 EndIf
078050060201     c**                 Eval      wokpsw = '1'
078060060201     c**                 Leave
078070060201     c**                 EndDo
078080060201     C**
078090060201     c**                 Endsr
078100920319     C*
078110930505     C* CONTROLLO COMUNI  ALL CONTR3 3 CONTR 4 -----------------------*
078120920319     C     CTRCOM        BEGSR
078130920319     C                   SETOFF                                       90
078140030620     C                   CLEAR                   SAV_INDSIN
078150941103     C**
078160920319     C* CODICE CLIENTE
078170941103     C**
078180950104     C*               SALTO CONTROLLO SE SPEDIZIONE IN FRANCO  IN ARRIV
078190950104     C*               LO ESEGUO SE SONO IN PARTENZA
078200941103    1C     *IN36         IFEQ      *OFF
078210941222     C     *IN10         OREQ      *OFF
078220941103     C*
078230941103     C* F7=RICERCA ALFABETICA
078240941103    2C   KG              DO
078250941212     C                   MOVEL     VIDDSA        PARDUT           30
078260920319     C                   MOVEL     *BLANKS       DESCDS           48
078270920319     C                   MOVEL     DESKSC        DESCDS
078280920319     C* PARSTA=9 ESCLUDO ANNULLATI
078290920319     C                   Z-ADD     9             PARSTA
078300060131     C                   Z-ADD     dutkci        CCC               4 0
078310941103    3C     VIDKLP        IFEQ      0
078320000510     C   10              MOVEL     ARBLNA        PARKSM
078330941103     C                   ELSE
078340000510     C                   MOVEL     VIDKLP        PARKSM
078350941103    3C                   ENDIF
078360991112     C                   CLEAR                   PARDIT
078370000510     C                   Z-ADD     1             PARNUM
078380941103     C*
078390000510     C                   CALL      'XALFA3BR'
078400920319     C                   PARM                    PARDUT
078410920319     C                   PARM                    CODUT
078420920319     C                   PARM                    DESCDS
078430920319     C                   PARM                    CCC
078440920319     C                   PARM                    PARSTA            1 0
078450961016     C                   PARM                    PARFLR           90
078460991112     C                   PARM                    PARDIT            3
078470000510     C                   PARM                    PARNUM            2 0
078480000510     C                   PARM                    PARKCM           80
078490000510     C                   PARM                    PARKSM          140
078500000510     C                   PARM                    PARKDM           60
078510941103    3C     PARSTA        IFEQ      -1
078520920319     C*
078530941103    4C     FLGCVR        IFEQ      '2'
078540941212     C*
078550990930     C     WAR62         IFEQ      ' '
078560941212     C                   MOVEL     ARBLNA        VIDKLP
078570941212     C                   MOVEL     9999          VIDKSC
078580941212     C                   ELSE
078590991007     C                   MOVEL     AR6KSC        VIDKLP
078600991007     C                   MOVE      AR6KSC        VIDKSC
078610941212     C                   ENDIF
078620941212     C*
078630941103   X4C                   ELSE
078640920319     C                   MOVEL     ARBKSC        VIDKLP
078650920319     C                   MOVE      ARBKSC        VIDKSC
078660941103    4C                   END
078670920319     C*
078680941103   X3C                   ELSE
078690020305     C                   MOVEL     PARKSM        ACOKSC
078700920319     C                   MOVEL     ACOKSC        VIDKLP
078710920319     C                   MOVE      ACOKSC        VIDKSC
078720970814     C                   CLEAR                   VIDRSM
078730970814     C                   CLEAR                   VIDINM
078740970814     C                   CLEAR                   VIDCAM
078750970814     C                   CLEAR                   VIDLOM
078760970814     C                   CLEAR                   VIDPRM
078770970814     C                   CLEAR                   VIDNZM
078780970814     C                   CLEAR                   VIDPIM
078790941103    3C                   END
078800941103     C*
078810941103     C                   SETON                                        9047
078820941103     C                   GOTO      ENDCOM
078830941212    2C                   ENDDO
078840920319     C* CONTROLLO
078850941103    2C     VIDKLP        IFEQ      0
078860941103     C                   MOVEL     MSG(33)       VIDMSG
078870941103     C                   SETON                                        469028
078880941103     C                   GOTO      ENDCOM
078890941103    2C                   END
078900980722     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE DELLO STESSO S.I. IN
078910991112     C*  CUI SONO  PER LE FATTURE IMMEDIATE
078920040930     C     VIDKLP        CHAIN     AZORG01L                           30
078930040930     c*
078940020725     C     D66BFI        IFEQ      'S'
078950020725     C**N30ORGDIT        IFNE      O50PRA
078960020725     C**                 MOVEL     MSG(81)       VIDMSG
078970020725     C**                 SETON                                        469028
078980020725     C**                 GOTO      ENDCOM
078990020725     C**                 ENDIF
079000020508     c* sempre per le fatture immediate se cliente 9999 il p.o. cliente
079010020508     c* deve essere = alla linea di arrivo della bolla
079020020508     c     vidksc        ifeq      9999
079030020508     c     vidklp        andne     arblna
079040020508     C                   MOVEL     MSG(35)       VIDMSG
079050020508     C                   SETON                                        479028
079060020508     C                   GOTO      ENDCOM
079070020508     c                   endif
079080020306     C                   ENDIF
079090020306     c* se variazione di mittente e cliente 8888 il p.o. cliente deve
079100020306     c* essere = alla linea di partenza bolla
079110020306     c     flgcvr        ifeq      'M'
079120020306     c     vidksc        andeq     8888
079130020306     c     vidklp        andne     arblnp
079140020306     C                   MOVEL     MSG(35)       VIDMSG
079150020306     C                   SETON                                        479028
079160020306     C                   GOTO      ENDCOM
079170020306     c                   endif
079180020508     c* se variazione di bolla in partenza e cliente 9999 errore
079190020508     c     *in10         ifeq      *off
079200020508     c     vidksc        andeq     9999
079210020508     C                   MOVEL     MSG(111)      VIDMSG
079220020508     C                   SETON                                        479028
079230020508     C                   GOTO      ENDCOM
079240020508     c                   endif
079250930507     C*
079260920319     C                   MOVEL     VIDKLP        KSC
079270920319     C                   MOVE      VIDKSC        KSC
079280000510     C  N30              MOVEL     ORGDE3        OG143
079290020318     C  N30              MOVEL     §OGNTW        CLINTW
079300020318     C   30              CLEAR                   CLINTW
079310040930     c* non accetto clienti con ntw = "LOG" o "XXX'
079320040930     c                   if        clintw='LOG'  or clintw='XXX'
079330040930     C                   MOVEL     MSG(130)      VIDMSG
079340040930     C                   SETON                                        469028
079350040930     C                   GOTO      ENDCOM
079360040930     c                   endif
079370051220     c* non è possibile modificare cliente da dpd a non dpd e viceversa
079380051220     c     flgcvr        ifeq      'M'
079390051220     c                   if        (clintw = 'DPD' and clintw <> memntw) or
079400051220     c                             (memntw = 'DPD' and clintw <> memntw)
079410051220     C                   MOVEL     MSG(137)      VIDMSG
079420051220     C                   SETON                                        469028
079430051220     C                   GOTO      ENDCOM
079440051220     c                   endif
079450051220     c                   endif
079460941103     C**
079470920319     C*
079480941103     C     KACO          CHAIN     CNACO000                           30
079490941103    3C  N30ACOFLG        IFNE      ' '
079500920319     C                   SETON                                        30
079510941103    3C                   END
079520941103     C* ERRORE
079530941103    3C     *IN30         IFEQ      *ON
079540941103     C     VIDKSC        OREQ      8888
079550941222     C     *IN10         ANDEQ     *ON
079560941223     C     VIDKSC        OREQ      9999
079570941223     C     *IN36         ANDEQ     *ON
079580941103     C                   MOVEL     MSG(36)       VIDMSG
079590941103     C                   SETON                                        479028
079600941103     C                   GOTO      ENDCOM
079610941103    3C                   ENDIF
079620930507     C*
079630130315    3C**   ACOABL        IFEQ      '8'
079640130315    3C**   ACOABL        OREQ      '*'
079650130315     c
079660130315    3C     ACOABL        ifne      *blanks
079670130418     c     ksc           andne     kscbolla
079680040930     C                   MOVEL     mSG(37)       VIDMSG
079690941103     C                   SETON                                        479028
079700941103     C                   GOTO      ENDCOM
079710941103    3C                   END
079720930507     C*
079730920319     C* SE SEGUE FATTURA NON SI PUO' METTERE COD 9999
079740941103    3C   13VIDKSC        IFEQ      9999
079750941103     C                   MOVEL     MSG(38)       VIDMSG
079760941103     C                   SETON                                        479028
079770941103     C                   GOTO      ENDCOM
079780941103    3C                   END
079790930507     C*
079800970822     C     VIDKSC        IFNE      9999
079810970822     C     VIDKSC        ANDNE     8888
079820970822     C                   MOVEL     ACORAG        DESKSC
079830941103    2C                   END
079840941220     C*
079850941223     C* SALVO ULTIMO CODICE IMPOSTATO
079860941220    2C     COMKSC        IFNE      KSC
079870941220     C                   CLEAR                   CIN
079880941220     C                   CLEAR                   DIE
079890941220     C                   MOVEL     KSC           COMKSC            7 0
079900941220    2C                   END
079910941103    1C                   END
079920030620     C* CERCO IN CNIND IL DIRITTO DI FATTURAZIONE
079930030620     C*
079940030620     C     KACO          CHAIN     CNIND00F                           30
079950030620    3C                   IF        %FOUND(CNIND00F)
079960030620     C                   EVAL      SAV_INDSIN = INDSIN
079970030620     C                   ELSE
079980030620     C                   CLEAR                   SAV_INDSIN
079990030620     C                   ENDIF
080000941103     C**
080010941215     C* CARICO CODICI TARIFFA DEL CLIENTE ED ESEGUO CONTROLLO
080020941103     C**
080030930507     C                   EXSR      CARCTR
080040941209     C*
080050941209     C* SE HO RICERCATO LA TARIFFA, RIEMETTO LA VIDEATA
080060050202    1C     ilv59RIC      IFEQ      'S'
080070941223     C                   SETON                                        9048
080080941209     C                   GOTO      ENDCOM
080090941209    1C                   ENDIF
080100941209     C*
080110941215     C* ATTIVATO IL "?" SUL CODICE TARIFFA
080120050202    1C     olv59ERR      IFEQ      'R'
080130050202     C                   MOVEL     olv59CTR      VIDCTR
080140050202     C                   MOVEL     olv59DFS      DE2CTR
080150941223     C                   SETON                                        9048
080160941209     C                   GOTO      ENDCOM
080170941209    1C                   ENDIF
080180941209     C* ERRORE
080190050202    1C     olv59MSG      IFNE      *BLANKS
080200060606     c* Nella videata della variazione del cod mittente, non do errore
080210060606     c*  se premuto F10 per la modifica del codice tariffa
080220060606     c*  ma disabilito F12
080230060606     c                   if        *inkj and flgcvr='M'
080240060606     c                   clear                   w48f12
080250060606     c                   else
080260050202     C                   MOVEL     olv59MSG      VIDMSG
080270941223     C                   SETON                                        904828
080280941209     C                   GOTO      ENDCOM
080290941209    1C                   ENDIF
080300060606    1C                   ENDIF
080310930507     C*
080320941209     C                   MOVEL     VIDCTR        COMCTR
080330050202     C                   MOVEL     olv59PRG      COMPRG
080340021121
080350021121      * Per bolla FedEx se cliente 8888 devo richiamare il trul27
080360021121If  1c                   If        O27Ntw = 'FED'
080370021122     c                   Movel     O27Ctr        Wctr
080380021122      * --> se cliente 8888
080390021121      *     devo richiamare il trul27 per recuperare la tariffa di cartello FedEx
080400021121      *     quella immessa a video non può essere diversa da quella recuperata
080410021122If  2c                   If        VidKsc = 8888
080420021122      * ---> e O27Fie = 'M'
080430021122If  3c                   If        WFie =  'M'
080440021122      * la tariffa deve essere quella della cartello
080450021122If  4c                   If        VidCtr <> WCtr
080460021122     c                   Eval      *In48 = *On
080470021122     c                   Eval      *In28 = *On
080480021122     c                   Eval      *In90 = *On
080490021122     c                   Movel     Msg(114)      VidMsg
080500021122     c                   Goto      EndCom
080510021122    4c                   EndIf
080520021122      * ---> e O27Fie non è = 'M'
080530021122   x3c                   Else
080540021122      * richiamo il trul27r xchè la tariffa in bolla deve essere una delle
080550021122      * 2 cartello FedEx
080560021121     c                   Clear                   Trul27Ds
080570021121     c                   Eval      I27Ctb = VidCtr
080580021122     c                   Eval      I27Ntw = 'FED'
080590021121     c                   Call      'TRUL27R'
080600021121     c                   Parm                    Trul27Ds
080610021121     c                   Movel     O27Ctr        Wctr
080620021122If  4c                   If        O27Err = *Blanks
080630021121     c                             and Vidctr <> Wctr
080640021121     c                   Eval      *In48 = *On
080650021121     c                   Eval      *In90 = *On
080660021121     c                   Eval      *In28 = *On
080670021121     c                   Movel     Msg(114)      VidMsg
080680021121     c                   Goto      EndCom
080690021122    4c                   EndIf
080700021122    3c                   EndIf
080710021121      * --> se cliente codificato
080720021121   x2c                   Else
080730021121      * ---> e non sono state trovate tariffe
080740021121If  3c                   If        Wtks <> 'S'
080750021121      *      non posso accettare una tariffa documenti se il peso della
080760021121      *      bolla superare il limite previsto in tabella "DFE"
080770021121     c                   Move      Vidctr        DueCtr
080780021121If  4c                   If        ArbPkf > §DfePkgD and
080790021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
080800021121     c                   Eval      *In48 = *On
080810021121     c                   Eval      *In90 = *On
080820021121     c                   Eval      *In28 = *On
080830021121      * Preparo il msg
080840021121     c                   Movea     Msg(115)      K78
080850021121     c                   Movel     §DfePkgD      W006a
080860021121     c                   Movea     W006a         Ski(10)
080870021121     c                   Z-Add     10            i
080880021121     c                   ExSr      Abbl
080890021121     c                   Movea     Ski(10)       K78(59)
080900021121     c                   Move      §DfePkgD      W001a
080910021121     c                   Movel     W001a         K78(66)
080920021121     c                   Movea     K78           VidMsg
080930021121     c                   Goto      EndCom
080940021121    4c                   EndIf
080950021121    3c                   EndIf
080960021121    2c                   EndIf
080970021121    1c                   EndIf
080980920319     C*
080990941103     C     ENDCOM        ENDSR
081000011017     C*
081010011017      *--- TASSAZIONE VARIA 'N' --------------------------------------*
081020011017     C     TASSAN        BEGSR
081030011017      *
081040011017     C                   CLEAR                   DTAS
081050011017     C                   CLEAR                   DTASV
081060011017     C                   MOVEL     VIDKLP        TASKSC
081070011017     C                   MOVE      VIDKSC        TASKSC
081080011017     C                   MOVEL     VIDCTR        TASCTR
081090011017     C                   Z-ADD     ARBPKF        TASPKF
081100011017     C                   MOVEL     ARBLNP        TASLNP
081110011017     C                   MOVEL     ARBCTS        TASCTS
081120011017     C                   MOVEL     ARBDSP        TASDSP
081130011017     C                   MOVEL     ARBDSP        TASDCT
081140011017     C                   MOVEL     ARBVLF        TASVLF
081150011017     C                   MOVEL     ARBTSP        TASTSP
081160011017     C     WBOL          IFEQ      '1'
081170011017     C                   MOVEL     §3ATB1        TASTBL
081180011017     C                   ELSE
081190011017     C                   MOVEL     §3ATB2        TASTBL
081200011017     C                   ENDIF
081210011017     C                   MOVEL     ARBLNA        TASLNA
081220011017     C                   MOVEL     ARBNCL        TASNCL
081230011017     C                   MOVEL     ARBQFT        TASQFT
081240011017     C     WBOL          IFEQ      '1'
081250011017     C     §3AFCA        ANDEQ     1
081260011017     C     WBOL          OREQ      '2'
081270011017     C     §3AFCA        ANDEQ     2
081280011017     C                   MOVEL     AR9CAS        TASCAS
081290011017     C                   MOVEL     AR9VCA        TASVCA
081300011017     C                   MOVEL     AR9TIC        TASTIC
081310011017     C                   ENDIF
081320011017     C                   MOVEL     ARBIAS        TASIAS
081330011017     C                   MOVEL     ARBVAS        TASVAS
081340011017     C                   MOVEL     ARBCAD        TASCAD
081350011017     C                   MOVEL     ARBNZD        TASNZD
081360011017     C                   MOVEL     ARBFIN        TASFIN
081370011017     C                   MOVEL     ARBCAM        TASCAM
081380011017     C                   MOVEL     ARBNZM        TASNZM
081390011017     C                   MOVEL     ARBFAP        TASFAP
081400011017     C                   MOVEL     ARBTC1        TASTC1
081410011017     C                   MOVEL     ARBTC2        TASTC2
081420011017     C     V1CDDT        IFEQ      'C'                                          SI DDT
081430011017     C     V1CDDT        OREQ      'S'
081440031028     c     v1cddt        oreq      'P'
081450031028     c     v1cddt        oreq      'Q'
081460011017     C                   MOVEL     'S'           TASDDT
081470011017     C                   ENDIF
081480060925     c*****              movel     arbfbr        tasflo
081490060925     c                   clear                   dtas01
081500060925     c                   movel     arbfbr        §asfbr
081510060925     c                   movel     arbcca        §ascca
081520060925     c                   eval      tasflo = dtas01
081530011017      **
081540011017      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
081550011017      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
081560020305     C     LNANTW        ifeq      'COR'
081570020305     c     LNANTW        oreq      'MES'
081580020305     c     LNANTW        oreq      'PPT'
081590011017     C                   MOVEL     ARBFDN        TASFDN
081600011017     C                   ENDIF
081610011017      **
081620011017      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
081630030612     C                   CLEAR                   Tisi95DS
081640011017     C                   MOVEL     ARBLOM        I95LOC
081650011017     C                   MOVEL     ARBCAM        I95CAP
081660011017     C                   MOVEL     ARBNZM        I95NAR
081670011017     C                   MOVEL     '3'           I95TCN
081680011017     C                   MOVEL     ARBDSP        I95DAT
081690011017     C     §3AFCA        IFNE      0
081700011017     C                   MOVEL     'S'           I95FCA
081710011017     C                   ENDIF
081720011017     C                   MOVEL     §3ATB1        I95TPO
081730020305     c     lnpntw        ifeq      'FED'
081740020305     c                   movel     'S'           i95fi1
081750020305     c                   endif
081760011017     C                   CALL      'TISI95R'
081770030612     C                   PARM                    Tisi95DS
081780011017     C     O95ERR        IFEQ      *BLANKS
081790011017     C                   MOVEL     O95CTS        TASMCT
081800011017     C                   ENDIF
081810011017     C                   MOVEL     'S'           WCAL13            1
081820011017      * ELABORO E CHIUDO CON LR IL PGM
081830011017     C                   MOVEL     ' '           TASTLA
081840020930     c* imposto peso vdl e colli rilevati
081850030203     c**!!!              clear                   dtas01
081860030203     c**!!!              z-add     arbncp        §asncp
081870030203     c**!!!              z-add     arbpkc        §aspkc
081880030203     c**!!!              movel     dtas01        tasflo
081890030203     c                   z-add     arbncp        Tasncp
081900030203     c                   z-add     arbpkc        Taspkc
081910030204      * Bancali
081920030205     c                   If        %Subst(ArbGva:1:1) = 'B'
081930030729     c                   Eval      TasBan = V1cBan
081940030205     c                   EndIf
081950030729      * Colli Originali
081960030729     c                   If        %Subst(ArbGva:1:1) = 'O'
081970030729     c                   Eval      TasNcl = V1cCor
081980030729     c                   EndIf
081990050804     c
082000050804     c                   eval      parca  = ' '
082010050804     c                   movel     paramt        kpjbu
082020011017      *
082030011017     C                   CALL      'TNSF20R'
082040011017     C                   PARM                    KPJBA
082050011017     C                   PARM                    DTAS
082060011017     C                   PARM                    DTASV
082070011018      *
082080011018      * Tassazione andata buon fine
082090011018     C     TASERR        IFEQ      *BLANKS
082100011018      *
082110011018      * Controllo la divisa della tassazione con la divisa del video
082120011018      * se non corrispondono devo convertire la varia 'N' nella divisa
082130011018      * del video
082140011018     C     TASDIV        IFNE      VIDDIV
082150011018      * aggancio la tabella CV per sicurezza
082160011018     C                   CLEAR                   DSCV
082170011018     C                   MOVEL     'CV'          COD
082180011018     C                   MOVEL(P)  VIDDIV        KEY
082190011018     C                   EXSR      CTABEL
082200011018     C  N30              MOVEL     TBLUNI        DSCV
082210011018      *
082220030612     C                   CLEAR                   YeurcoDS
082230011018     C                   MOVEL     TASDIV        YECDVI
082240011018     C                   MOVEL     VIDDIV        YECDVO
082250011018     C                   Z-ADD     TASIMV        YECIMI
082260011018     C     §CVFDC        IFEQ      'S'
082270011018     C                   MOVE      '3'           YECDCO
082280011018     C                   ELSE
082290011018     C                   MOVE      '0'           YECDCO
082300011018     C                   ENDIF
082310011018     C                   CALL      'YEURCO'
082320030612     C                   PARM                    YeurcoDS
082330011018     C     YECESI        IFEQ      ' '
082340011018     C                   Z-ADD     YECIMO        TASIMV
082350011018     C                   ELSE
082360011018     C                   Z-ADD     88888888      TASIMV
082370011018     C                   ENDIF
082380011018     C                   ENDIF
082390011017      *
082400011017     C                   Z-ADD     TASIMV        V3CVA1
082410011017     C                   Z-ADD     TASIMV        VARVA1
082420011017     C                   ENDIF
082430011017      *
082440011017     C                   ENDSR
082450910429     C*
082460050531begsrC*--- TASSAZIONE BOLLA ------------------------------------------*
082470910429     C     TASSAZ        BEGSR
082480991005     C                   CLEAR                   DTAS
082490991005     C                   CLEAR                   DTASV
082500020503     C                   CLEAR                   wtassa            1
082510030620     C                   CLEAR                   wtassad           1
082520030620     C                   CLEAR                   itassa           11 3
082530050601     c*
082540050601     c* richiamo routine di impostazione campi "comuni" da passare a
082550050601     c* pgm tnsf20r
082560050601     c                   exsr      SR_rtassa
082570910429     C*
082580941221     C* CARICO LE VARIE
082590941221     C*
082600980513     C     WBOL          IFEQ      '1'
082610030709     c                   eval      flgsf20 = 'S'
082620991012     C                   EXSR      RDTASV
082630030709     c                   eval      flgsf20 = ' '
082640980513     C*
082650980513     C                   ELSE
082660040123      * carico varie seconda bolla
082670040123     C                   MOVEL     'G'           TASSVA
082680040123     c                   Eval      TascVag = 'S'
082690041202     C**!!!VI2VA1        IFGT      0
082700040123     C                   MOVEL     VI2SV1        VSV(1)
082710040123     C                   Z-ADD     VI2VA1        VVA(1)
082720040123     c                   Movel     Vi2Es1        ves(1)
082730041202     C**!!!              ENDIF
082740041202     c**!!!              If        Vi2Va2 > *Zeros
082750040123     c                   Movel     Vi2Sv2        Vsv(2)
082760040123     c                   Z-add     Vi2Va2        Vva(2)
082770040123     c                   Movel     Vi2Es2        ves(2)
082780041202     c**!!!              EndIf
082790041202     c**!!!              If        Vi2Va3 > *Zeros
082800040123     c                   Movel     Vi2Sv3        Vsv(3)
082810040123     c                   Z-add     Vi2Va3        Vva(3)
082820040123     c                   Movel     Vi2Es3        ves(3)
082830041202     c**!!!              EndIf
082840041202     c**!!!              If        Vi2Va4 > *Zeros
082850040123     c                   Movel     Vi2Sv4        Vsv(4)
082860040123     c                   Z-add     Vi2Va4        Vva(4)
082870040123     c                   Movel     Vi2Es4        ves(4)
082880041202     c**!!               EndIf
082890041202     c**!!!              If        Vi2Va5 > *Zeros
082900041130     c                   Movel     Vi2Sv5        Vsv(5)
082910041130     c                   Z-add     Vi2Va5        Vva(5)
082920041130     c                   Movel     Vi2Es5        ves(5)
082930041202     c**!!!              EndIf
082940041202     c**!!!              If        Vi2Va6 > *Zeros
082950041130     c                   Movel     Vi2Sv6        Vsv(6)
082960041130     c                   Z-add     Vi2Va6        Vva(6)
082970041130     c                   Movel     Vi2Es6        ves(6)
082980041202     c**!!!              EndIf
082990040126     c                   xfoot     vva           TotImv
083000040126     c                   Z-add     TotImv        TasImv
083010980608     C                   ENDIF
083020011211
083030011211      * Data fattura se c'è
083040030203     c**!!!              clear                   dtas01
083050020930     c**                 if        ar6dft <> 0
083060020930     c**                 movel     ar6dft        §asdat
083070020930     c**                 endif
083080020503     c**
083090020503     c                   if        wtassa=' '
083100020503     c* se devo tassare solo una varia, da tipo bolla, la imposto
083110020503     c                   if        §3asva<>*blanks
083120020503     c                   eval      tassva=§3asva
083130020503     c                   endif
083140030620     c                   endif
083150030925     c* se diritto di fatturazione uguale a zero non calcolo la varia D
083160030925     c                   if        sav_indsin = 0
083170030925     c                   eval      wtassad = 'N'
083180030925     c                   endif
083190030623      * se tassazione fattura immediata o ritassazione prepagato
083200030623      * passo al TNSF20R la varia "D" e l'importo del diritto
083210030620      * di fatturazione da sommare che prendo da cnind (INDSIN)
083220030623     c                   if        (*in14 and not *in29  and wtassad = ' ')  or
083230030623     c                             (not *in10 and vidnft > 0 and not *in29 and
083240030623     c                             wtassad = ' ')
083250030925      *
083260030620     c                   move      sav_indsin    tasivdf
083270030620     c                   eval      tassvdf = 'D'
083280030620      * se bolla già tassat devo sommare la sola varia "D"
083290030620      * imponibile > 0 oppure imponibile a zero ma singola varia vaolorizzata x bolle speciali
083300040123     c**!!!              if        tasimv > 0 or (tasimv = 0 and itassa > 0)
083310040123     c                   if        tasimv > 0
083320030620     c                   eval      tassva = 'D'
083330030620     c                   endif
083340030620      * se singola varia per bolla speciale non è da calcolare ed è uguale a zero non calcolo la
083350030620      * varia D
083360030620     c                   if        wtassa = 'N' and tasimv = 0 and itassa = 0
083370030620     c                   eval      wtassad = 'N'
083380030620     c                   endif
083390030620
083400030620     c                   endif
083410011211
083420030620      * se non è tassazione fattura immediata non devo calcolare la varia D
083430030623      * o ritassazione prepagato
083440030623     c*                  if        not *in14
083450030623     c                   if        not *in14  or  *in29 or
083460030623     c                             (not *in10 and vidnft = 0)
083470030620     c                   eval      wtassad = 'N'
083480030620     c                   endif
083490030620      * se non devo calcolare la singola varia e neppure la varia D salto la tassazione
083500030620     c                   if        wtassa = 'N' and wtassad = 'N'
083510030620     c                   goto      endtas
083520030620     c                   endif
083530151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
083540151127     c* è solo per tassare una specifica varia
083550151126     C     WBOL          IFEQ      '1'
083560151126     c* PACKING LIST
083570151126     c                   if        par5als='S' or par5alx='S'
083580151126     c                   eval      tasspl='S'
083590151126     c                   endif
083600151126     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
083610151126     C                   MOVEL     'M'           WTRC
083620151126     C     KARB2         CHAIN(N)  FiAR401l
083630151126     c                   if        %found(fiar401l)
083640151126     c                   movel     ar4not        dsbl4m
083650151126     c                   if        §b4ado='S'
083660151126     c                   eval      tasprt='S'
083670151126     c                   endif
083680151126     c                   endif
083690151126
083700151126     c                   endif
083710050804     c
083720050804     c                   eval      parca  = ' '
083730050804     c                   movel     paramt        kpjbu
083740910429     C*
083750941209     C                   CALL      'TNSF20R'
083760910429     C                   PARM                    KPJBA
083770991005     C                   PARM                    DTAS
083780991005     C                   PARM                    DTASV
083790910429     C*
083800941212    1C     TASERR        IFNE      *BLANKS
083810980608     C     TASMSG        IFNE      *BLANKS
083820980608     C                   MOVEL(P)  TASMSG        VIDMSG
083830980608     C                   ELSE
083840941212     C                   MOVEL     MSG(55)       VIDMSG
083850980608     C                   ENDIF
083860941209     C                   SETON                                        499028
083870991006     C* SE ERRORI IN TASSAZIONE SPROTEGGO LA DIVISA
083880991006     C                   SETOFF                                       07
083890941212   X1C                   ELSE
083900980513     C     WBOL          IFEQ      '1'
083910950421     C                   MOVEL     TASQFT        VIDQFT
083920950424     C                   MOVEL     TASQFT        VARQFT
083930910429     C                   MOVEL     TASPOR        VIDPOR
083940991006     C                   MOVEL     TASPOR        VARPOR
083950991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
083960991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
083970991025     C* DIVISA
083980991025     C     VIDDIV        IFNE      TASDIV
083990991025     C     *INKF         ANDEQ     *ON
084000991025     C                   SETON                                        90
084010991025     C                   ENDIF
084020991025     C*
084030991005     C                   MOVEL     TASDIV        VIDDIV
084040991006     C                   MOVEL     TASDIV        WDIV
084050991005     C                   Z-ADD     TASIMV        VIDIMV
084060941209     C* CARICO LE VARIE
084070941212    2C                   DO        20            NRR
084080941209     C     NRR           CHAIN     LR48S03                            30
084090941209     C                   MOVEL     VSV(NRR)      V3CSV1
084100941209     C                   Z-ADD     VVA(NRR)      V3CVA1
084110991006     C                   Z-ADD     VVA(NRR)      VARVA1
084120941209     C                   MOVEL     VES(NRR)      V3CEV1
084130941209     C   30              WRITE     LR48S03
084140941209     C  N30              UPDATE    LR48S03
084150941212    2C                   ENDDO
084160941209     C*
084170980513    XC                   ELSE
084180040123     c                   Clear                   ContaSv
084190040123      * carico le varie seconda bolla
084200040123Do  2c                   Do        20            c
084210040123If  3c                   If        Vsv(c) <> §$2Iva and
084220040123     c                             Vsv(c) <> §$2Bol
084230040123     c                   Add       1             ContaSv
084240040123S   4c                   Select
084250040123     c                   When      ContaSv = 1
084260040123     c                   Movel     Vsv(c)        Vi2Sv1
084270040123     c                   Z-add     Vva(c)        Vi2Va1
084280040123     c                   Movel     Ves(c)        Vi2Es1
084290040123     c                   When      ContaSv = 2
084300040123     c                   Movel     Vsv(c)        Vi2Sv2
084310040123     c                   Z-add     Vva(c)        Vi2Va2
084320040123     c                   Movel     Ves(c)        Vi2Es2
084330040123     c                   When      ContaSv = 3
084340040123     c                   Movel     Vsv(c)        Vi2Sv3
084350040123     c                   Z-add     Vva(c)        Vi2Va3
084360040123     c                   Movel     Ves(c)        Vi2Es3
084370040123     c                   When      ContaSv = 4
084380040123     c                   Movel     Vsv(c)        Vi2Sv4
084390040123     c                   Z-add     Vva(c)        Vi2Va4
084400040123     c                   Movel     Ves(c)        Vi2Es4
084410041130     c                   When      ContaSv = 5
084420041130     c                   Movel     Vsv(c)        Vi2Sv5
084430041130     c                   Z-add     Vva(c)        Vi2Va5
084440041130     c                   Movel     Ves(c)        Vi2Es5
084450041130     c                   When      ContaSv = 6
084460041130     c                   Movel     Vsv(c)        Vi2Sv6
084470041130     c                   Z-add     Vva(c)        Vi2Va6
084480041130     c                   Movel     Ves(c)        Vi2Es6
084490040123    4c                   EndSl
084500040123    3c                   EndIf
084510040123    2c                   EndDo
084520040123     C**!!!              Z-ADD     1             C
084530040123     C**!!!TASSVA        LOOKUP    VSV(C)                                 33
084540040123     C*!!33              Z-ADD     VVA(C)        VI2VA1
084550040123     C*!!33              Z-ADD     VVA(C)        VA2VA1
084560040123     C*!!33              MOVEL     VSV(C)        VI2SV1
084570991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
084580991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
084590991025     C* DIVISA
084600040123     C*!!33VI2DIV        IFNE      TASDIV
084610040123     C     VI2DIV        IFNE      TASDIV
084620991025     C     *INKF         ANDEQ     *ON
084630991025     C                   SETON                                        90
084640991025     C                   ENDIF
084650040123     C*!!33              MOVEL     TASDIV        VI2DIV
084660040123     C*!!33              MOVEL     TASDIV        WDIV
084670040123     C                   MOVEL     TASDIV        VI2DIV
084680040123     C                   MOVEL     TASDIV        WDIV
084690040123     C*!!33VES(C)        IFNE      ' '
084700040123     C**!!!VI2CEI        ANDEQ     ' '
084710040123     C**!!!              MOVEL     VES(C)        VI2CEI
084720040123     C**!!!              ENDIF
084730980513     C                   ENDIF
084740020503     C                   ENDIF
084750980513     C*
084760030620    1C*****              ENDIF
084770941209     C*
084780911211     C     ENDTAS        ENDSR
084790050601     C*
084800050601     C*--- Riempimento campi comuni di passaggio a tnsf20r -----------*
084810050601     C     sr_rtassa     BEGSR
084820050601     C* CODIFICATO
084830050601    1C     VIDKSC        IFNE      9999
084840050601    1C     VIDKSC        ANDNE     8888
084850050601     C                   MOVEL     VIDKLP        TASKSC
084860050601     C                   MOVE      VIDKSC        TASKSC
084870050601     C                   MOVEL     VIDCTR        TASCTR
084880050601   X1C                   ELSE
084890050601     C*
084900050601     C* E HO LA DATA FATTURA USO QUELLA, ALTRIMENTI DATEU
084910050601     C     AR6DFT        IFGT      0
084920050601     C                   Z-ADD     AR6DFT        WDATA
084930050601     C                   ELSE
084940050601     C                   Z-ADD     DATEU         WDATA
084950050601     C                   ENDIF
084960050601     C*
084970050601     C* DAL TIPO SERVIZIO PRENDO LA TARIFFA CON CUI TASSARE
084980050601     C*
084990050601     c                   eval      tasksc  = o27ksc
085000050601     c                   eval      tasctr  = o27ctr
085010050601     C**
085020050601    1C                   ENDIF
085030050601     C*
085040050601     C                   Z-ADD     ARBPKF        TASPKF
085050050601     C                   MOVEL     ARBLNP        TASLNP
085060050601     C                   MOVEL     ARBCTS        TASCTS
085070050601     C                   MOVEL     ARBDSP        TASDSP
085080050601     C                   MOVEL     ARBDSP        TASDCT
085090050601     C                   MOVEL     ARBVLF        TASVLF
085100050601     C                   MOVEL     ARBTSP        TASTSP
085110050601     C     WBOL          IFEQ      '1'
085120050601     C                   MOVEL     §3ATB1        TASTBL
085130050601     C                   MOVEL     VIDDIV        TASDIV
085140050601     C                   Z-ADD     VIDIMV        TASIMV
085150050601     C                   ELSE
085160050601     C                   MOVEL     §3ATB2        TASTBL
085170050601     C                   MOVEL     VI2DIV        TASDIV
085180050601     C**!!!              Z-ADD     VI2VA1        TASIMV
085190050601     C                   END
085200050601     C                   MOVEL     ARBLNA        TASLNA
085210050601     C                   MOVEL     ARBNCL        TASNCL
085220050601     C                   MOVEL     VIDPOR        TASPOR
085230050601     C                   MOVEL     VIDQFT        TASQFT
085240050601     C     WBOL          IFEQ      '1'
085250050601     C     §3AFCA        ANDEQ     1
085260050601     C     WBOL          OREQ      '2'
085270050601     C     §3AFCA        ANDEQ     2
085280050601     C                   MOVEL     AR9CAS        TASCAS
085290050601     C                   MOVEL     AR9VCA        TASVCA
085300050601     C                   MOVEL     AR9TIC        TASTIC
085310050601     C                   ENDIF
085320050601     C                   MOVEL     VIDIAS        TASIAS
085330050601     C                   MOVEL     VIDVAS        TASVAS
085340050601     C                   MOVEL     ARBCAD        TASCAD
085350050601     C                   MOVEL     ARBNZD        TASNZD
085360050601     C                   MOVEL     ARBCAM        TASCAM
085370050601     C                   MOVEL     ARBNZM        TASNZM
085380050601     C                   MOVEL     ARBFIN        TASFIN
085390050601     C                   MOVEL     ARBFAP        TASFAP
085400050601     C                   MOVEL     ARBTC1        TASTC1
085410050601     C                   MOVEL     ARBTC2        TASTC2
085420050601     C     V1CDDT        IFEQ      'S'
085430050601     C     V1CDDT        OREQ      'C'
085440050601     c     v1cddt        oreq      'P'
085450050601     c     v1cddt        oreq      'Q'
085460050601     C                   MOVEL     'S'           TASDDT
085470050601     C                   ENDIF
085480060925     c****               movel     arbfbr        tasflo
085490060925     c                   clear                   dtas01
085500060925     c                   movel     arbfbr        §asfbr
085510060925     c                   movel     arbcca        §ascca
085520110630     c                   movel     wemd          §asemd
085530150108     c* Pin Code
085540150108     c                   if        arbgma<>*blanks
085550150108     c                   Movel     '7R'          cod
085560150108     c                   Movel(p)  ArbGMa        key
085570150108     c                   ExSr      ctabel
085580150108     c  n30              Movel     TblUni        ds7R
085590150108     c   30              Clear                   ds7R
085600150108     c                   if        §7rpincode='S'
085610150108     c                   movel     'S'           §aspin
085620150108     c                   endif
085630150108     c                   endif
085640060925     c                   eval      tasflo = dtas01
085650050601     C* SE BOLLA ARRIVI IN ASSEGNATO EXPORT, NON PASSO LA CONSEGNA A
085660050601     C*  MAGAZZINO PER FAR CALCOLARE SEMPRE LA VARIA "U"
085670050601     C     *IN10         IFEQ      *OFF
085680050601     C     *IN06         OREQ      *ON
085690050601     C     lnantw        oreq      'COR'
085700050601     c     lnantw        oreq      'MES'
085710050601     c     lnantw        oreq      'PPT'
085720050601     C                   MOVEL     ARBFDN        TASFDN
085730050601     C                   ENDIF
085740050601     C**
085750050601     C* CHAI SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
085760050601     C                   CLEAR                   Tisi95DS
085770050601     C                   MOVEL     ARBCAM        I95CAP
085780050601     C                   MOVEL     ARBNZM        I95NAR
085790050601     C                   MOVEL     '3'           I95TCN
085800050601     C                   MOVEL     ARBDSP        I95DAT
085810050601     C                   MOVEL     FLGTB1        I95TPO
085820050601     C     §3AFCA        IFNE      0
085830050601     C                   MOVEL     'S'           I95FCA
085840050601     C                   ENDIF
085850050601     c     lnpntw        ifeq      'FED'
085860050601     c                   movel     'S'           i95fi1
085870050601     c                   endif
085880050601     C**
085890050601     C                   CALL      'TISI95R'
085900050601     C                   PARM                    Tisi95DS
085910050601     C     O95ERR        IFEQ      *BLANKS
085920050601     C                   MOVEL     O95CTS        TASMCT
085930050601     C                   ENDIF
085940050601
085950050601     c                   z-add     arbncp        Tasncp
085960050601     c                   z-add     arbpkc        Taspkc
085970050601      * Bancali
085980050601     c                   If        %Subst(ArbGva:1:1) = 'B'
085990050601     c                   Eval      TasBan = V1cBan
086000050601     c                   EndIf
086010050601      * Colli Originali
086020050601     c                   If        %Subst(ArbGva:1:1) = 'O'
086030050601     c                   Eval      TasNcl = V1cCor
086040050601     c                   EndIf
086050050601     c                   endsr
086060050531     C*--- TASSAZIONE BOLLA ------------------------------------------*
086070050531     C     TASSAR        BEGSR
086080050601     c                   clear                   dtas
086090050601     c                   clear                   dtasv
086100050601     c* richiamo routine di riempimento campi da passare a tnsf20r
086110050601     c                   eval      wbol = '1'
086120050601     c                   exsr      SR_rtassa
086130050531     c                   eval      tassva = 'R'
086140050804     c
086150050531     c                   eval      parca  = 'S'
086160050531     c                   movel     paramt        kpjbu
086170050531      *
086180050531     C                   CALL      'TNSF20R'
086190050531     C                   PARM                    KPJBA
086200050531     C                   PARM                    DTAS
086210050531     C                   PARM                    DTASV
086220050722      *
086230050804      *
086240050722     c                   eval      parca  = ' '
086250050804     c***                movel     paramt        kpjbu
086260050722      *
086270050531     c                   endsr
086280911211     C*
086290011017     C*
086300991006     C* CALCOLO IL TOTALE FATTURA-------------------------------------*
086310990702     C     TOTFAT        BEGSR
086320030612     C                   CLEAR                   FNLV04DS
086330990702     C* AROTONDO IN ARRIVO O IN PARTENZA SE PREPAGATI NUOVI
086340990702    2C     *IN10         IFEQ      *ON
086350990702     C     *IN23         OREQ      *ON
086360991005     C                   MOVEL     VIDDIV        DIVGEI
086370991005     C                   EXSR      RIEGEI
086380991005     C                   MOVEL     §GEAED        D04AED
086390991005     C                   Z-ADD     §GEAVL        D04VAR
086400990702    2C                   ENDIF
086410991005     C                   MOVEL     'T'           D04FIM
086420990702     C                   MOVEL     ARBDFT        D04DFT
086430990702     C                   MOVEL     VIDIMV        D04IMV
086440990702     C                   MOVEL     VIDCEI        D04CEI
086450990702     C                   MOVEL     AR9TIC        D04TIC
086460990702     C                   MOVEL     AR9VCA        D04VCA
086470990702     C                   Z-ADD     AR9CAS        D04CAS
086480990702     C                   MOVEL     '1'           D04SPE
086490990702     C   23              MOVEL     '8'           D04SPE
086500991005     C                   MOVEL     §3ATB1        D04TBL
086510991005     C                   MOVEL     VIDDIV        D04DIV
086520990702     C                   Z-ADD     VIDPOR        D04POR
086530990702     C                   MOVEL     '1'           UNO               1
086540990702     C*
086550991005     C                   CALL      'FNLV04R'
086560030612     C                   PARM                    FNLV04DS
086570991005     C                   PARM                    DTASV
086580990702     C                   ENDSR
086590990702     C*
086600991005     C**************************************************************************
086610991005     C*  REPERIMENTO TABELLA GEI
086620991005     C**************************************************************************
086630991005     C     RIEGEI        BEGSR
086640030612     C                   CLEAR                   Tibs02DS
086650991117     C                   CLEAR                   DGEI
086660991117     C                   MOVEL     'C'           T02MOD
086670991117     C                   MOVEL     KNSIF         T02SIF
086680991117     C                   MOVEL     'GEI'         T02COD
086690991117     C                   MOVEL     DIVGEI        T02KE1
086700991117     C                   CALL      'TIBS02R'
086710991117     C                   PARM                    KPJBA
086720030612     C                   PARM                    Tibs02DS
086730991117     C     T02ERR        IFEQ      *BLANKS
086740991117     C                   MOVEL     T02UNI        DGEI
086750991117     C                   ENDIF
086760991005     C                   ENDSR
086770911211     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE TX -----------------*
086780911211     C     REGIS3        BEGSR
086790041027     C                   SETOFF                                       3709
086800130412     c                   clear                   wpda_ndc
086810130412     c                   clear                   wpda_ddc
086820130412     c                   clear                   wpda_dcm
086830041027      * salvo i campi relativi al valore da assicurare, qtà da fatturare, valore merce
086840041027     c                   eval      sav_arbias = arbias
086850041027     c                   eval      sav_arbvas = arbvas
086860041027     c                   eval      sav_arbqft = arbqft
086870041027     c                   eval      sav_arbvmd = arbvmd
086880041027     c                   eval      sav_arbvad = arbvas
086890041027     c                   movel     arbctr        sav_arbctr
086900950104     C*  GIRO UDATE
086910950104     C                   TIME                    W0140            14 0
086920950104     C* UDATE IN GGMMAAAA
086930950104     C                   MOVE      W0140         WDTGIO            8 0
086940950104     C*
086950950104     C* UDATE IN AAAAMMGG
086960950104     C                   Z-ADD     WDTGIO        G02DAT
086970950104     C                   MOVEL     *BLANK        G02ERR
086980950104     C                   CALL      'XSRDA8'
086990950104     C                   PARM                    WLBDAT
087000950104     C                   MOVEL     G02INV        DATEU             8 0
087010941221     C* PRENDO I VALORI ASSOLUTI
087020941221     C                   MLLZO     1             VIDPOR
087030941221     C                   MLLZO     1             VIDIMV
087040941221     C                   MLLZO     1             VIDIAS
087050941221     C                   MLLZO     1             VIDVMD
087060941221     C                   MLLZO     1             VIDQFT
087070941221     C                   MLLZO     1             VIDKLP
087080941221     C                   MLLZO     1             VIDKSC
087090070515     c
087100070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
087110070515     c* iornare direttamente il file
087120070515     c*  solo se cambiati dati su testa bolla
087130070515     c                   clear                   wsiaggio          1
087140070515  2b c                   if        sav_arbias <> vidias or sav_arbvas<>vidvas
087150070515     c                             or
087160070515     c                             sav_arbvmd <> vidvmd or sav_arbvad<>vidvad
087170070515     c                             or
087180070515     c                             sav_arbqft <> vidqft  or
087190070515     c                             sav_arbctr<>vidctr
087200070515     c                   eval      wsiaggio='S'
087210070515     c                   clear                   waltrabolla       1
087220070515     C   10KARB          CHAIN     FNBLP01L                           3091
087230070515     C  n10KARB          CHAIN     FNARB01L                           3091
087240070515     c*
087250070515     c                   if        *in91
087260070515     C                   SETON                                        28
087270070515     C                   MOVEL     MSG(4)        VIDMSG
087280070515     C                   GOTO      ENDRG3
087290070515     c                   else
087300070515     c                   if        *in30
087310070515     c                   eval      waltrabolla='N'
087320070515     c                   else
087330130411     c  n10              eval      wpda_ndc=arbndc
087340130411     c  n10              eval      wpda_ddc=arbddc
087350130411     c  n10              eval      wpda_dcm=arbdcm
087360070515     C   10KARB          CHAIN     FNARB01L                           30
087370070515     C  N10KARB          CHAIN     FNBLP01L                           30
087380070515     c* Alla fine per non sporcarmi i campi del record, richaino il
087390070515     c*  record originale che sto variando
087400130411     c   10              eval      wpda_ndc=arbndc
087410130411     c   10              eval      wpda_ddc=arbddc
087420130411     c   10              eval      wpda_dcm=arbdcm
087430070515     c                   endif
087440070515     c                   endif
087450070515     c                   endif
087460131120    1c                   if        wsiaggio=*blanks and d66pda<>' '
087470130412     c                   clear                   waltrabolla       1
087480130412    2c                   if        not *in10
087490130412     C     KARB          CHAIN(n)  FNARB01L                           30
087500130412    3c                   if        *in30
087510130412     c                   eval      waltrabolla='N'
087520130412    3c                   endif
087530130412    2c                   endif
087540130412     c                   eval      wpda_ndc=arbndc
087550130412     c                   eval      wpda_ddc=arbddc
087560130412     c                   eval      wpda_dcm=arbdcm
087570130412     C  N10KARB          CHAIN     FNBLP01L                           30
087580130412     c                   endif
087590941213     C*
087600941213     C* INVIO PER SEDE
087610991006     C                   MOVEL     'FIARBT0T'    SFILE
087620941213     C* ALLOCO OGGETTI
087630941213     C                   EXSR      ALLOC
087640070515     c
087650070515     c                   if        *in91
087660070525     C   10              unlock    FNBLP01L
087670070525     C  N10              unlock    FNARB01L
087680070515     C                   GOTO      ENDRG3
087690070515     c                   endif
087700941213     C*
087710991001     C                   MOVEL     *BLANKS       WCKINL            1
087720991001     C                   EXSR      CARWAR
087730151020     c* Mmorizzo il totale imponibilie del video prima degli arrotondamenti
087740151020     c****               if        tassatoimv=0
087750151019     c                   movel     vidimv        Tassatoimv
087760151020     c****               endif
087770991006     C* SOLO SE C'E' NUMERO FATTURA CALCOLO
087780991006     C     VIDDFT        IFGT      *ZEROS
087790000105     C                   MOVEL     O17DFA        ARBDFT
087800991006     C                   EXSR      TOTFAT
087810991006     C     D04BOL        IFGT      *ZEROS
087820991006     C                   ADD       1             C
087830991006     C                   MOVEL     §$2BOL        WSV(C)
087840991006     C                   Z-ADD     D04BOL        WVA(C)
087850991006     C                   ENDIF
087860991006     C     D04IVA        IFGT      *ZEROS
087870991006     C                   ADD       1             C
087880991006     C                   MOVEL     §$2IVA        WSV(C)
087890991006     C                   Z-ADD     D04IVA        WVA(C)
087900991006     C                   ENDIF
087910991006     C                   ENDIF
087920941213     C*
087930941213     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
087940950616    1C     C             IFGT      0
087950941213     C                   SETON                                        09
087960941213     C*
087970941213     C* INVIO PER SEDE
087980941213     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
087990950616    2C     C             IFGT      3
088000991006     C* MI SALVO I CAMPI DELL'FIARBT
088010941213     C                   MOVEL     SOVR(1)       SOARBT
088020941213     C                   MOVEL     SALC(1)       SAARBT
088030941213     C                   MOVEL     SDLC(1)       SDARBT
088040941213     C*
088050991006     C                   MOVEL     'FIARBU0T'    SFILE
088060941213     C* ALLOCO OGGETTI
088070941213     C                   EXSR      ALLOC
088080070515     c
088090070515     c                   if        *in91
088100070525     C   10              unlock    FNBLP01L
088110070525     C  N10              unlock    FNARB01L
088120070515     C                   MOVEL     SDARBT        SDLC(1)
088130070515     c                   exsr      DLCSED
088140070525     C                   SETON                                        2891
088150070515     C                   GOTO      ENDRG3
088160070515     c                   endif
088170941213     C*
088180950616    2C                   ENDIF
088190950616    1C                   ENDIF
088200950114     C*
088210950114     C                   MOVEL     KNMUS         ARBPRU
088220950114     C                   Z-ADD     DATEU         ARBDTV
088230950114     C                   TIME                    ARBORV
088240950114     C                   MOVEL     VIDCVR        ARBCVB
088250991011     C                   MOVEL     '1'           ARBTRC
088260991011     C*
088270991011     C                   MOVEL     '1'           AR6TRC
088280930511     C*
088290060130     C* SE DA STORICIZZARE
088300060130    1C**   *IN10         IFEQ      *ON
088310060130     C**   D66FSA        ANDEQ     'S'
088320060130     C**   *IN10         OREQ      *OFF
088330060130     C**   D66FSP        ANDEQ     'S'
088340060130     C     D66FSA        ifeq      'S'
088350941213     C                   EXSR      STOARB
088360950616    1C                   ENDIF
088370060302     c* storicizzazione motivazione
088380060302     c     d66mtv        ifeq      'S'
088390060302     c                   exsr      sto_mtv
088400060302     c                   endif
088410941213     C**
088420991006     C* INVIO VARIAZIONE --> FIARBU0T
088430941213     C**
088440950616    1C     *IN09         IFEQ      *ON
088450941213     C     C             ANDGT     3
088460941213     C                   Z-ADD     4             C
088470941213     C*
088480941213    2C     D66FFI        IFEQ      'S'
088490950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
088500950616    3C     §3ABSD        IFEQ      ' '
088510941213     C                   Z-ADD     4             C
088520991006     C                   OPEN      FIARBU0T
088530941213     C*
088540941213     C* NE SCRIVO FIN TANTO CHE CE NE SONO
088550950616    4C     WSV(C)        DOWNE     ' '
088560941213     C                   MOVEL     WSV(C)        ARBSVN
088570941213     C                   Z-ADD     WVA(C)        ARBVAN
088580991006     C                   WRITE     FIARBUTT
088590941213     C                   ADD       1             C
088600950616    4C                   ENDDO
088610941213     C*
088620991006     C                   CLOSE     FIARBU0T
088630941213     C* DISALLOCO OGGETTO MEMBRO PER SEDE
088640950616    3C                   ENDIF
088650950208     C                   EXSR      DLCSED
088660950208    2C                   ENDIF
088670941213     C* REIMPOSTO I CAMPI
088680941213     C                   MOVEL     SOARBT        SOVR(1)
088690941213     C                   MOVEL     SAARBT        SALC(1)
088700941213     C                   MOVEL     SDARBT        SDLC(1)
088710941213    1C                   ENDIF
088720930506     C*
088730930506     C                   Z-ADD     VIDKSC        ARBKSC
088740930506     C                   MOVEL     VIDKLP        ARBKSC
088750941209     C                   MOVEL     VIDIAS        ARBIAS
088760941209     C                   MOVEL     VIDVAS        ARBVAS
088770011017      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
088780011017     C     VIDIAS        IFEQ      *ZEROS
088790011017     C                   CLEAR                   ARBVAS
088800011017     C                   ENDIF
088810011017      *
088820941209     C                   MOVEL     VIDVMD        ARBVMD
088830941215     C                   MOVEL     VIDVAD        ARBVAD
088840011017      * Se il valore merce è a 0 non devo scrivere la divisa
088850011017     C     VIDVMD        IFEQ      *ZEROS
088860011017     C                   CLEAR                   ARBVAD
088870011017     C                   ENDIF
088880011017      *
088890941209     C                   MOVEL     VIDQFT        ARBQFT
088900930506     C                   MOVEL     VIDCTR        ARBCTR
088910991006     C                   MOVEL     VIDDIV        ARBDIV
088920941213     C                   MOVEL     VIDPOR        ARBPOR
088930941213     C                   MOVEL     WSV(1)        ARBSV1
088940941213     C                   MOVEL     WVA(1)        ARBVA1
088950941213     C                   MOVEL     WSV(2)        ARBSV2
088960941213     C                   MOVEL     WVA(2)        ARBVA2
088970941213     C                   MOVEL     WSV(3)        ARBSV3
088980941213     C                   MOVEL     WVA(3)        ARBVA3
088990991006     C                   Z-ADD     VIDIMV        ARBIMV
089000991006     C                   MOVEL     O17DFA        ARBDFT
089010950104     C                   MOVEL     VIDNFT        ARBNFT
089020950104     C                   MOVEL     VIDFIV        ARBFIV
089030991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
089040991013     C* STATO MODIFICATO TOLGO LA 'P'
089050991013     C     AR6FIM        IFEQ      'P'
089060991013     C     ARBIMV        ANDNE     AR6IMV
089070991013     C     *IN10         ANDEQ     *ON
089080991013     C                   MOVE      *BLANKS       ARBFIM
089090991013     C                   ELSE
089100991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
089110991013     C                   MOVE      AR6FIM        ARBFIM
089120991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
089130991013     C     WAR6          IFNE      'E'
089140991013     C     *IN10         ANDEQ     *OFF
089150991013     C                   MOVEL     'P'           ARBFIM
089160991013     C                   END
089170991013     C                   ENDIF
089180941209     C*
089190941212     C                   MOVEL     VIDCEI        ARBCEI
089200930506     C*
089210950616    1C     ARBDFT        IFGT      0
089220930505     C*
089230930505     C* SE TOTALE FATTURA NON CORRISPONDE AL PRECEDENTE RISTAMPA FATT
089240941209     C     D04IFT        COMP      AR6IFT                             3737
089250930505     C*
089260941212     C                   MOVE      D04ALI        ARBALI
089270941212     C                   MOVE      D04IFT        ARBIFT
089280941212     C                   MOVE      D04IMV        ARBIMV
089290930514     C*
089300970613    2C                   SELECT
089310941221     C     ARBPOR        WHENGT    0
089320941212     C                   Z-ADD     D04POR        ARBPOR
089330941221     C     ARBVA1        WHENGT    0
089340991007     C     ARBSV1        ANDNE     §$2BOL
089350991007     C     ARBSV1        ANDNE     §$2IVA
089360941213     C                   ADD       D04POR        ARBVA1
089370941209     C     ARBVA2        WHENGT    0
089380991007     C     ARBSV2        ANDNE     §$2BOL
089390991007     C     ARBSV2        ANDNE     §$2IVA
089400941213     C                   ADD       D04POR        ARBVA2
089410941209     C     ARBVA3        WHENGT    0
089420941213     C                   ADD       D04POR        ARBVA3
089430970613    2C                   ENDSL
089440941209     C*
089450950616    1C                   ENDIF
089460930511     C*
089470930511     C                   MOVEL     VIDCVR        ARBCVB
089480911218     C*
089490950616    1C     D66FFI        IFEQ      'S'
089500950227     C*
089510950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
089520950616    2C     §3ABSD        IFEQ      ' '
089530991006     C                   OPEN      FIARBT0T
089540991006     C                   WRITE     FIARBTTT
089550991006     C                   CLOSE     FIARBT0T
089560950616    2C                   END
089570941212     C* DISALLOCO OGGETTO MEMBRO PER SEDE
089580941212     C                   EXSR      DLCSED                                       FNARBT0T
089590950616    1C                   END
089600941213     C**
089610941216     C* A G G I O R N O
089620941213     C**
089630051214     c* Semodificato il cod tariffa, verifico se volume automatico
089640051214     c                   if        not *in10 and vidctr<>sav_arbctr
089650051214     C*
089660051214     C                   MOVEL     '7T'          COD
089670051214     C                   MOVEL(P)  ARBFVF        KEY
089680051214     C                   EXSR      CTABEL
089690051214     C                   MOVEL     TBLUNI        DS7T
089700051214     C     §7TRVL        IFEQ      'A'
089710051214     C     §7TRVL        OREQ      'P'
089720051214     c                   movel     'S'           wricvl
089730051214     c                   seton                                        24
089740051214     c                   endif
089750051214     c                   endif
089760051214     c
089770991006     C                   Z-ADD     ARBKSC        AR6KSC
089780991006     C                   Z-ADD     ARBCTR        AR6CTR
089790941213     C                   MOVE      ARBALI        AR6ALI
089800941213     C                   MOVE      ARBIFT        AR6IFT
089810941213     C                   MOVE      ARBIMV        AR6IMV
089820991006     C                   MOVEL     ARBDIV        AR6DIV
089830941213     C                   MOVE      ARBPOR        AR6POR
089840941213     C                   MOVE      ARBSV1        AR6SV1
089850941213     C                   MOVE      ARBVA1        AR6VA1
089860941213     C                   MOVE      ARBSV2        AR6SV2
089870941213     C                   MOVE      ARBVA2        AR6VA2
089880941213     C                   MOVE      ARBSV3        AR6SV3
089890941213     C                   MOVE      ARBVA3        AR6VA3
089900941213     C                   MOVEL     ARBCEI        AR6CEI
089910950104     C                   MOVEL     ARBDFT        AR6DFT
089920950104     C                   MOVEL     ARBNFT        AR6NFT
089930950104     C                   MOVEL     ARBFIV        AR6FIV
089940991013     C                   MOVEL     ARBFIM        AR6FIM
089950941213     C*
089960991006     C* CANCELLO LE VARIE NELL'FIAR7
089970941221    1C     WAR7          IFEQ      'E'
089980991006     C                   MOVEL     '1'           WTRC
089990991006     C     KARB2         SETLL     FIAR7000
090000991006     C     KARB2         READE     FIAR7000                               30
090010941213     C*
090020941213    2C     *IN30         DOWEQ     *OFF
090030991006     C                   DELETE    FIAR7000
090040991006     C     KARB2         READE     FIAR7000                               30
090050941213    2C                   ENDDO
090060941213    1C                   ENDIF
090070941213     C*
090080941213     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
090090941213    1C     WSV(4)        IFNE      ' '
090100941213     C                   Z-ADD     4             C
090110991006     C                   CLEAR                   FIAR7000
090120941213     C                   MOVEL     ARBAAS        AR7AAS
090130941213     C                   MOVEL     ARBLNP        AR7LNP
090140941213     C                   MOVEL     ARBNRS        AR7NRS
090150941213     C                   MOVEL     ARBNSP        AR7NSP
090160991006     C                   MOVEL     '1'           AR7TRC
090170941213    2C     WSV(C)        DOWNE     ' '
090180941213     C                   MOVEL     WSV(C)        AR7SVN
090190941213     C                   MOVEL     WVA(C)        AR7VAN
090200991006     C                   WRITE     FIAR7000
090210941213     C                   ADD       1             C
090220941213    2C                   ENDDO
090230941213    1C                   ENDIF
090240941213     C**
090250941216     C* SE HO ELIMINATO TUTTA LA TASSAZIONE, PER S.F., DELETO RECORD
090260950616    1C     AR6DFT        IFEQ      0
090270941216     C     AR6SV1        ANDEQ     ' '
090280941216     C     AR6SV2        ANDEQ     ' '
090290941216     C     AR6SV3        ANDEQ     ' '
090300941216     C     AR6POR        ANDEQ     0
090310991116     C     AR6CEI        ANDEQ     ' '
090320941216     C*
090330950616    2C     WAR6          IFEQ      'E'
090340041027     C     *IN10         ifeq      *OFF
090350991006     C                   MOVEL     '1'           WTRC
090360991006     C     KARB2         DELETE    FIAR6000                           30
090370991116     C                   ELSE
090380041027     C* SE variaz, in ARRIVO DEVO UGUALMENTE tenere
090390991116     C* FIAR6000 (SERVE PER LA PROCEDURA DEI DANNI)
090400991116     C                   UPDATE    FIAR6000
090410991116     C                   ENDIF
090420991116   X2C                   ELSE
090430041027     C* SE NON ESISTE RECORD TASSAZIONE scrivo record se var.in arrivo
090440041027     C     *IN10         ifeq      *ON
090450991116     C                   MOVEL     ARBAAS        AR6AAS
090460991116     C                   MOVEL     ARBLNP        AR6LNP
090470991116     C                   MOVEL     ARBNRS        AR6NRS
090480991116     C                   MOVEL     ARBNSP        AR6NSP
090490991116     C                   WRITE     FIAR6000
090500041027    3C                   ENDIF
090510041027    2C                   ENDIF
090520941216     C*
090530941216     C                   ELSE
090540941216     C*
090550950616    2C     WAR6          IFEQ      'E'
090560991006     C                   UPDATE    FIAR6000
090570941209     C                   ELSE
090580941209     C                   MOVEL     ARBAAS        AR6AAS
090590941209     C                   MOVEL     ARBLNP        AR6LNP
090600941209     C                   MOVEL     ARBNRS        AR6NRS
090610941209     C                   MOVEL     ARBNSP        AR6NSP
090620991006     C                   WRITE     FIAR6000
090630950616    2C                   ENDIF
090640950616    1C                   ENDIF
090650941209     C*
090660151016     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
090670151016     c*  per la fatturazione se inserito o variato totale imponibile
090680151016     c                   if        (ar6imv>0 and ar6imv<>savimv_fat) or
090690151016     c                             ar6imv=0
090700151118     c                   exsr      regpesivol
090710151016     c                   endif
090720950113     C*
090730950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
090740950113     C*  SE VOLUME AUTOMATICO O PRESUNTO PER PRECEDENTE VARIAIZONE
090750950113     C*  MITTENTE
090760950616    1C     *IN24         IFEQ      *ON
090770051214     c* Mi salvo i campi che userò per la storicizzazione
090780051214     C                   MOVEL     sav_arbctr    MITCTR
090790051214     C                   MOVEL     ARBFVF        MITFVF
090800051214     C                   MOVEL     ARBVLF        MITVLF
090810051214     c
090820950113     C                   EXSR      CALVOL
090830070907     C**                 MOVEL     'I2'          D48CVB
090840070907     C**                 MOVEL     'E'           WESEG
090850070907     c                   add       1             kk
090860070907     c                   movel     'I2'          kcvb(kk)
090870070911     c                   movel     'V'           kffr(kk)
090880070911     c                   movel     ' '           kf12(kk)
090890070911     c****               clear                   w48f12
090900950114     C* AGGIORNO GIA' IL VOLUME TANTO PASSO DALLA VIDEATA DEL VOLUME
090910950114     C*  COMUNQUE PER LA TRASMISSIONE
090920950114     C                   MOVEL     D18FVF        ARBFVF
090930950114     C                   MOVEL     D18VLF        ARBVLF
090940070911     C*****              MOVEL     'V'           D48FFR
090950950616     C                   END
090960911211     C*
090970950114     C   10              UPDATE    FNARB000
090980950114     C  N10              UPDATE    FNBLP000
090990950114     C*
091000041027  1b c***                if        wstessogas = 'S' and (d66ffi = 'E' or
091010041027     c***                          d66ffi = 'P') and *in10 = *off
091020041027  2b c***                if        sav_vidias <> vidias or sav_vidvas <> vidvas
091030041027     c***                          or
091040041027     c***                          sav_vidvmd <> vidvmd or sav_vidvad <> vidvad
091050041027     c***                          or
091060041027     c***                          sav_vidqft <> vidqft
091070041027     c***                exsr      Agg_ARB
091080041027  2e c***                endif
091090041027  1e c***                endif
091100041027     c
091110041027     c* Aggiorno altra bolla se c'e' e variati i campi sopra
091120041027     c                   if        wsiaggio='S' and waltrabolla=' '
091130041027     c* non aggiono mai il ksc:
091140041027     c* in  partenza perchè il S.F. va in ar6
091150041027     c*  in arrivo perchè il cod mittente se cambiato viene impostato
091160041027     c*  in regis8
091170041027     C   10              except    arbTblp
091180041027     C  N10              except    arbTarb
091190041027     c                   endif
091200130411
091210130411     c* Richiamo routine per invio variazione a pda
091220130411     c                   exsr      sr_pda
091230911212     C     ENDRG3        ENDSR
091240911211     C*
091250970613     C* SE BOLLA SU DISTINTA CHIUSA NON FACCIO NULLA ALTRIMENTI ------*
091260970613     C*  CHIUDO LA BOLLA
091270970613     C     CHIUBO        BEGSR
091280970613     C                   MOVEL     'S'           WAGG              1
091290970613     C*
091300970613     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
091310970613     C*  UN NUMERO DISTINTA ED E' CHIUSA
091320080516     c                   clear                   fvvfcf
091330970613     C     ARBNDC        IFGT      0
091340020911     c                   Z-Add     ArbIfp        Kfgs
091350970613     C                   Z-ADD     ARBNDC        W0050             5 0
091360020911     C     KFVV          CHAIN     FNFVV01L                           30
091370970613     C     *IN30         IFEQ      *OFF
091380970613     C     FVVFCF        ANDEQ     'S'
091390970613     C                   CLEAR                   WAGG
091400970613     C                   ENDIF
091410970613     C                   ENDIF
091420970613     C*
091430970613    4C     WAGG          IFEQ      'S'
091440970613    5C     ARBNMI        IFEQ      0
091450970613     C                   Z-ADD     8888888       ARBNMI
091460970613     C                   Z-ADD     8888888       ARBNDC
091470970613     C                   Z-ADD     DATEU         ARBDDC
091480080516     c                   else
091490080516     c
091500080516     c                   if        arbndc>0 and fvvfcf<>'S'
091510080516     C                   Z-ADD     8888888       ARBNDC
091520080516     c                   endif
091530080516     c
091540970613    5C                   ENDIF
091550970613    4C                   ENDIF
091560970613     C                   ENDSR
091570970613     C*
091580911211     C*--- CONTROLLI FORMATO4 ----------------------------------------*
091590911211     C     CONTR4        BEGSR
091600040123     C**!!!              MOVEL     ' '           CEISV3
091610920319     C                   EXSR      CTRCOM
091620920319     C   90              GOTO      ENDCT4
091630920319     C*
091640911211     C* T A S S A Z I O N E
091650991011     C     *INKN         IFEQ      *ON
091660991011     c* fattura immediata
091670991011     C     *IN14         OREQ      *ON
091680991011     C                   MOVEL     '2'           WBOL              1
091690991011     C                   EXSR      TASSAZ
091700991011     C   KN
091710991011     COR 90              GOTO      ENDCT4
091720991011     C                   END
091730911211     C*
091740990930     C* OBBLIGATORIO IMPORTO DELLA VARIA
091750040123     C**!!!VI2VA1        IFEQ      0
091760040123     C**!!!              MOVEL     MSG(51)       VIDMSG
091770040123     C**!!!              SETON                                        519028
091780040123     C**!!!              GOTO      ENDCT4
091790040123     C**!!!              END
091800991007     C* Se divisa sprotetta significa che manca tariffa: in questo caso
091810991007     C* controllo che la divisa inserita sia = alla divisa della moneta
091820991007     C*  di conto SE CLIENTE  9999
091830991007     C     *IN14         IFEQ      *ON
091840991007     C     *IN07         ANDEQ     *OFF
091850991007     C     VI2DIV        ANDNE     §GEDCN
091860991007     C     VIDKSC        ANDEQ     9999
091870991007     C                   SETON                                        459028
091880991007     C                   MOVEA     MSG(90)       K78
091890991007     C                   MOVEA     §GEDCN        K78(71)
091900991007     C                   MOVEA     K78           VIDMSG
091910991007     C                   GOTO      ENDCT4
091920991007     C                   ENDIF
091930991006     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
091940100120     C                   CLEAR                   dlv1601
091950100120     C                   CLEAR                   Fnlv16DS
091960991006     C                   CLEAR                   DTASV
091970991006     C                   MOVEL     'A'           D17TBO
091980991006     C                   MOVEL     VIDKLP        D17KSC
091990991006     C                   MOVE      VIDKSC        D17KSC
092000991012     C     DFT2BO        IFGT      0
092010991012     C                   Z-ADD     DFT2BO        D17DSP
092020991012     C                   ELSE
092030991006     C                   Z-ADD     ARBDSP        D17DSP
092040991012     C                   ENDIF
092050991006     C                   MOVEL     ARBLNP        D17LNP
092060991006     C                   MOVEL     ARBLNA        D17LNA
092070100120     C                   MOVEL     §3ATB2        D17TBL
092080991006     C                   MOVEL     §3ARBL        D17RBL
092090020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
092100040123     C                   MOVEL     §3ASVA        §d17sva
092110040123      * passo che devo controllare la seconda bolla
092120040123     c                   Eval      §d17Tb2 = 'S'
092130100120
092140100120     c* Se si tratta di ffattura immediata , passo cod iso ddestintario
092150100120    1c                   if        *in14
092160100120     c                   Eval      §d17ctriso='S'
092170100120    2c                   if        arbcpi<>*blanks
092180100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
092190100120     c* solo numeri
092200100120     c                   select
092210100120    3c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
092220100120     c                   eval      §d17iso='IT'
092230100120     c* "PRIVATO" senza iso
092240100120     c                   when      *in30 and posiz<=3
092250100120     c                   eval      §d17iso='$$'
092260100120     c                   other
092270100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
092280100120    3c                   ENDSL
092290100120    2c                   endif
092300100120    1c                   endif
092310100120     c
092320040123     c                   Movel     dlv1601       D17Flo
092330100120     c
092340991006     C                   MOVEL     VI2DIV        D17DIV
092350991006     C                   MOVEL     WDIV          D17DVP
092360991006    1C     §3AFCA        IFEQ      2
092370991006     C                   MOVEL     'S'           D17FCA
092380991006    1C                   ENDIF
092390991006     C                   MOVEL     'E'           D17FIM
092400991006     C                   MOVEL     VI2CEI        D17CEI
092410991006     C                   MOVEL     VI2SV1        VSV(1)
092420040123     C**!!!              MOVEL     VI2VA1        VVA(1)
092430040123     c                   Z-add     Vi2Va1        vva(1)
092440040123     c                   Movel     Vi2Es1        ves(1)
092450040123     c                   Movel     Vi2Sv2        vsv(2)
092460040123     c                   Movel     Vi2Es2        ves(2)
092470040123     c                   Z-add     Vi2Va2        vva(2)
092480040123     c                   MOVEL     Vi2Sv3        vsv(3)
092490040123     c                   Z-add     Vi2Va3        vva(3)
092500040123     c                   Movel     Vi2Es3        ves(3)
092510040123     c                   MOVEL     Vi2Sv4        vsv(4)
092520040123     c                   Z-add     Vi2Va4        vva(4)
092530040123     c                   Movel     Vi2Es4        ves(4)
092540041130     c                   MOVEL     Vi2Sv5        vsv(5)
092550041130     c                   Z-add     Vi2Va5        vva(5)
092560041130     c                   Movel     Vi2Es5        ves(5)
092570041130     c                   MOVEL     Vi2Sv6        vsv(6)
092580041130     c                   Z-add     Vi2Va6        vva(6)
092590041130     c                   Movel     Vi2Es6        ves(6)
092600991006     C                   CALL      'FNLV16R'
092610030612     C                   PARM                    Fnlv16DS
092620991006     C                   PARM                    DTASV
092630991011     C                   MOVEL     D17DIV        VI2DIV
092640991006     C                   MOVEL     D17CEI        VI2CEI
092650991011     C                   MOVEL     O17DEI        DESCEI
092660040123     C**!!!              MOVEL     VSV(1)        VI2SV1
092670040123     C**!!!              MOVEL     VES(1)        CEISV3            1
092680040123      * Ricarico le varie
092690040123     c                   Movel     Vsv(1)        Vi2Sv1
092700040123     c                   Z-add     Vva(1)        Vi2Va1
092710040123     c                   Movel     Ves(1)        Vi2Es1
092720040123     c                   Movel     Vsv(2)        Vi2Sv2
092730040123     c                   Z-add     Vva(2)        Vi2Va2
092740040123     c                   Movel     Ves(2)        Vi2Es2
092750040123     c                   Movel     Vsv(3)        Vi2Sv3
092760040123     c                   Z-add     Vva(3)        Vi2Va3
092770040123     c                   Movel     Ves(3)        Vi2Es3
092780040123     c                   Movel     Vsv(4)        Vi2Sv4
092790040123     c                   Z-add     Vva(4)        Vi2Va4
092800040123     c                   Movel     Ves(4)        Vi2Es4
092810041130     c                   Movel     Vsv(5)        Vi2Sv5
092820041130     c                   Z-add     Vva(5)        Vi2Va5
092830041130     c                   Movel     Ves(5)        Vi2Es5
092840041130     c                   Movel     Vsv(6)        Vi2Sv6
092850041130     c                   Z-add     Vva(6)        Vi2Va6
092860041130     c                   Movel     Ves(6)        Vi2Es6
092870040123      * Controllo se caricata almeno una varia
092880041130do  1c                   Do        6             c
092890040123     c                   If        vsv(c) <> *Blanks
092900040123     c                   Eval      wesvar1 = 'S'
092910040123     c                   EndIf
092920040123    1c                   EndDo
092930040123
092940991006     C     O17ERR        IFNE      *BLANKS
092950991006     C                   SETON                                        90
092960991006     C     O17MSG        IFNE      *BLANKS
092970991006     C                   MOVEL     O17MSG        VIDMSG
092980991006     C                   SETON                                        28
092990991006     C                   ENDIF
093000991006     C     O17ERR        IFEQ      '6'
093010040123     c                   Select
093020040123     c                   When      O17Erv = 1
093030991006     C                   SETON                                        49
093040040123     c                   When      O17Erv = 2
093050040123     C                   SETON                                        63
093060040123     c                   When      O17Erv = 3
093070040123     C                   SETON                                        64
093080040123     c                   When      O17Erv = 4
093090040123     C                   SETON                                        65
093100041130     c                   When      O17Erv = 5
093110041130     C                   SETON                                        66
093120041130     c                   When      O17Erv = 6
093130041130     C                   SETON                                        67
093140040123     c                   EndSl
093150991006     C                   ENDIF
093160991006     C     O17ERR        IFEQ      '1'
093170991006     C                   SETON                                        50
093180991006     C                   ENDIF
093190991006     C     O17ERR        IFEQ      '7'
093200991006     C                   SETON                                        45
093210991006     C                   ENDIF
093220991006     C   90              GOTO      ENDCT4
093230991006     C                   ENDIF
093240991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
093250991006    1C  N07VI2DIV        IFNE      WDIV
093260991006     C* CONVERSIONE VARIA
093270991006    3C     VI2VA1        IFEQ      VA2VA1
093280991006     C     VI2VA1        ANDGT     *ZEROS
093290991006     C     WDIV          ANDNE     *BLANKS
093300030612     C                   CLEAR                   YeurcoDS
093310991006     C                   MOVEL     WDIV          YECDVI
093320991006     C                   Z-ADD     VI2VA1        YECIMI
093330991006     C                   MOVEL     VI2DIV        YECDVO
093340991006     C     O17FDC        IFEQ      'S'
093350011114     C                   MOVE      '3'           YECDCO
093360991006     C                   ELSE
093370991006     C                   CLEAR                   YECDCO
093380991006     C                   ENDIF
093390991006     C                   CALL      'YEURCO'
093400030612     C                   PARM                    YeurcoDS
093410991006     C                   Z-ADD     YECIMO        VI2VA1
093420991012     C                   SETON                                        51
093430991006    3C                   ENDIF
093440040123      * Conversione Varia 2
093450040123    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
093460040123     c                             wdiv <> *Blanks
093470040123     c                   Clear                   Yeurcods
093480040123     c                   Movel     wdiv          YECDVI
093490040123     c                   Z-add     Vi2Va2        YECIMI
093500040123     c                   Movel     Vi2Div        YECDVO
093510040123     c                   If        O17Fdc = 'S'
093520040123     c                   Movel     '3'           YECDCO
093530040123     c                   EndIf
093540040123     c                   Call      'YEURCO'
093550040123     c                   Parm                    Yeurcods
093560040123     c                   Z-add     YECIMO        Vi2Va2
093570040123     c                   Eval      *In63 = *On
093580040123    2c                   EndIf
093590040123      * Conversione Varia 3
093600040123    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
093610040123     c                             wdiv <> *Blanks
093620040123     c                   Clear                   yeurcods
093630040123     c                   Movel     wdiv          YECDVI
093640040123     c                   Z-add     Vi2Va3        YECIMI
093650040123     c                   Movel     Vi2Div        YECDVO
093660040123     c                   If        O17Fdc = 'S'
093670040123     c                   Movel     '3'           YECDCO
093680040123     c                   EndIf
093690040123     c                   Call      'YEURCO'
093700040123     c                   Parm                    yeurcods
093710040123     c                   Z-add     YECIMO        Vi2Va3
093720040123     c                   Eval      *In64 = *On
093730040123    2c                   EndIf
093740040123      * Conversione Varia 4
093750040123    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
093760040123     c                             wdiv <> *Blanks
093770040123     c                   Clear                   yeurcods
093780040123     c                   Movel     wdiv          YECDVI
093790040123     c                   Z-add     Vi2Va4        YECIMI
093800040123     c                   Movel     Vi2Div        YECDVO
093810040123     c                   If        O17Fdc = 'S'
093820040123     c                   Movel     '3'           YECDCO
093830040123     c                   EndIf
093840040123     c                   Call      'YEURCO'
093850040123     c                   Parm                    yeurcods
093860040123     c                   Z-add     YECIMO        Vi2Va4
093870040123     c                   Eval      *In65 = *On
093880040123    2c                   EndIf
093890041130      * Conversione Varia 5
093900041130    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
093910041130     c                             wdiv <> *Blanks
093920041130     c                   Clear                   yeurcods
093930041130     c                   Movel     wdiv          YECDVI
093940041130     c                   Z-add     Vi2Va5        YECIMI
093950041130     c                   Movel     Vi2Div        YECDVO
093960041130     c                   If        O17Fdc = 'S'
093970041130     c                   Movel     '3'           YECDCO
093980041130     c                   EndIf
093990041130     c                   Call      'YEURCO'
094000041130     c                   Parm                    yeurcods
094010041130     c                   Z-add     YECIMO        Vi2Va5
094020041130     c                   Eval      *In66 = *On
094030041130    2c                   EndIf
094040041130      * Conversione Varia 6
094050041130    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
094060041130     c                             wdiv <> *Blanks
094070041130     c                   Clear                   yeurcods
094080041130     c                   Movel     wdiv          YECDVI
094090041130     c                   Z-add     Vi2Va6        YECIMI
094100041130     c                   Movel     Vi2Div        YECDVO
094110041130     c                   If        O17Fdc = 'S'
094120041130     c                   Movel     '3'           YECDCO
094130041130     c                   EndIf
094140041130     c                   Call      'YEURCO'
094150041130     c                   Parm                    yeurcods
094160041130     c                   Z-add     YECIMO        Vi2Va6
094170041130     c                   Eval      *In67 = *On
094180041130    2c                   EndIf
094190040123     C**!!!              Z-ADD     VI2VA1        VA2VA1
094200040123     C**!!!              Z-ADD     VI2VA1        VVA(1)
094210040123     C**!!!              MOVEL     VI2DIV        WDIV
094220991012     C* RIEMETTO COMUNQUE LA VIDEATA SE HANNO VARIATO LA DIVISA
094230991012     C* (POTREI RIEMETTERLA SOLO SE HO CONVERTITO IMPORTI MA LA
094240991012     C* RIEMETTO COMUNQUE PER FARE COME PER LA PRIMA BOLLA)
094250991012     C                   SETON                                        90
094260040123
094270040123     c                   Z-add     Vi2Va1        Va2Va1
094280040123     c                   Z-add     Vi2Va2        Va2Va2
094290040123     c                   Z-add     Vi2Va3        Va2Va3
094300040123     c                   Z-add     Vi2Va4        Va2Va4
094310041130     c                   Z-add     Vi2Va5        Va2Va5
094320041130     c                   Z-add     Vi2Va6        Va2Va6
094330040123     c                   Z-add     Vi2Va1        Vva(1)
094340040123     c                   Z-add     Vi2Va2        Vva(2)
094350040123     c                   Z-add     Vi2Va3        Vva(3)
094360040123     c                   Z-add     Vi2Va4        Vva(4)
094370041130     c                   Z-add     Vi2Va5        Vva(5)
094380041130     c                   Z-add     Vi2Va6        Vva(6)
094390040123     c                   Movel     Vi2Div        Wdiv
094400040123
094410991012     C                   GOTO      ENDCT4
094420991006   X1C                   ELSE
094430991006     C                   Z-ADD     VI2VA1        VA2VA1
094440040123     c                   Z-add     Vi2Va2        Va2Va2
094450040123     c                   Z-add     Vi2Va3        Va2Va3
094460040123     c                   Z-add     Vi2Va4        Va2Va4
094470041130     c                   Z-add     Vi2Va5        Va2Va5
094480041130     c                   Z-add     Vi2Va6        Va2Va6
094490991006    1C                   ENDIF
094500011206      *
094510011206      * Controllo che la somma degli importi immessi non superi il valore
094520011206      * impostato in §GEIMF
094530040123     c**!!!              eval      w0173 = vi2va1
094540040123     c                   xfoot     vva           w0173
094550011206      *      controllo se devo convertire il totale imponibile
094560011206  1b c                   if        vi2div <> §gedcn
094570030612     c                   clear                   YeurcoDS
094580011206     c                   eval      yecdvi = vi2div
094590011206     c                   eval      yecdvo = §gedcn
094600040123     c**!!!              eval      yecimi = vi2va1
094610040123     c                   eval      yecimi = w0173
094620011206     c                   move      decsav        yecdco
094630011206     c                   call      'YEURCO'
094640030612     c                   parm                    YeurcoDS
094650011206  2b c                   if        yecesi = ' '
094660011206     c                   eval      w0173 = yecimo
094670011206  2e c                   endif
094680011206  1e c                   endif
094690011206      *      controllo se l'imponibile supera il limite fatturabile
094700011220  1b c                   if        w0173 > imfsav
094710011206     c                   eval      vidmsg = msg(110)
094720011220     c                   eval      %subst (vidmsg:45:14) = (%editc(imfsav: '4'))
094730011206     c                   eval      %subst (vidmsg:60:3) = §gedcn
094740040123     c**!!!              eval      *in51 = *on
094750011206     c                   eval      *in90 = *on
094760011206     c                   goto      endct4
094770011206  1e c                   endif
094780040123
094790040123     c                   Z-add     w0173         TotImv
094800920319     C*
094810911211     C     ENDCT4        ENDSR
094820911212     C*
094830911212     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE 2X -----------------*
094840911212     C     REGIS4        BEGSR
094850040130     c                   Setoff                                       3709
094860130412     c                   clear                   wpda_ndc
094870130412     c                   clear                   wpda_ddc
094880130412     c                   clear                   wpda_dcm
094890040130
094900950104     C*  GIRO UDATE
094910950104     C                   TIME                    W0140            14 0
094920950104     C* UDATE IN GGMMAAAA
094930950104     C                   MOVE      W0140         WDTGIO            8 0
094940950104     C*
094950950104     C* UDATE IN AAAAMMGG
094960950104     C                   Z-ADD     WDTGIO        G02DAT
094970950104     C                   MOVEL     *BLANK        G02ERR
094980950104     C                   CALL      'XSRDA8'
094990950104     C                   PARM                    WLBDAT
095000950104     C                   MOVEL     G02INV        DATEU             8 0
095010941221     C* PRENDO I VALORI ASSOLUTI
095020941221     C                   MLLZO     1             VIDKLP
095030941221     C                   MLLZO     1             VIDKSC
095040941221     C                   MLLZO     1             VI2VA1
095050040123     C                   MLLZO     1             VI2VA2
095060040123     C                   MLLZO     1             VI2VA3
095070040123     C                   MLLZO     1             VI2VA4
095080041130     C                   MLLZO     1             VI2VA5
095090041130     C                   MLLZO     1             VI2VA6
095100941221     C*
095110950203     C                   MOVEL     KNMUS         ARBPRU
095120920120     C* INVIO PER SEDE
095130991007     C                   MOVEL     'FIARBT0T'    SFILE             8
095140930210     C* ALLOCO OGGETTI
095150040123     C                   EXSR      ALLOC
095160930210     C   91              GOTO      ENDRG4
095170040123      * carico le varie in una sk di comodo
095180040123     c                   ExSr      CarWar1
095190040123      * se c'è n.fattura calcolo
095200040129If  1c                   If        Dft2bo > *Zeros
095210040129     c                   Movel     Dft2bo        ArbDft
095220040123     C                   MOVEL     VI2DIV        ARBDIV
095230040129     C                   MOVEL     VI2DIV        divgei
095240040123     C                   EXSR      RIEGEI
095250040123     C                   CLEAR                   Fnlv04ds
095260040123     C                   MOVEL     'T'           D04FIM
095270040123     C                   Z-ADD     ArbDft        D04DFT
095280040123     C                   MOVEL     §GEAED        D04AED
095290040123     C                   Z-ADD     §GEAVL        D04VAR
095300040123     c                   Z-add     TotImv        D04Imv
095310040123     C                   MOVEL     VI2CEI        D04CEI
095320040123     C                   MOVEL     AR9TIC        D04TIC
095330040123     C                   MOVEL     AR9VCA        D04VCA
095340040123     C                   Z-ADD     AR9CAS        D04CAS
095350040123     C                   MOVEL     '2'           D04SPE
095360040123     C                   MOVEL     §3ATB2        D04TBL
095370040123     C                   MOVEL     VI2DIV        D04DIV
095380040123     C                   MOVEL     '1'           UNO
095390040123     C*
095400040123     C                   CALL      'FNLV04R'
095410040123     C                   PARM                    Fnlv04ds
095420040123     C                   PARM                    DTASV
095430040123     C*
095440040123     C     D04BOL        IFGT      *ZEROS
095450040123     C                   ADD       1             C
095460040123     C                   MOVEL     §$2BOL        WSV(C)
095470040123     C                   Z-ADD     D04BOL        WVA(C)
095480040123     C                   ENDIF
095490040123     C     D04IVA        IFGT      *ZEROS
095500040123     C                   ADD       1             C
095510040123     C                   MOVEL     §$2IVA        WSV(C)
095520040123     C                   Z-ADD     D04IVA        WVA(C)
095530040123     C                   ENDIF
095540040123    1c                   EndIf
095550040123
095560040123     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
095570040123    1C     C             IFGT      0
095580040123     C                   SETON                                        09
095590040123     C*
095600040123     C* INVIO PER SEDE
095610040123     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
095620040123    2C     C             IFGT      3
095630040123     C* MI SALVO I CAMPI DELL'FIARBT
095640040123     C                   MOVEL     SOVR(1)       SOARBT
095650040123     C                   MOVEL     SALC(1)       SAARBT
095660040123     C                   MOVEL     SDLC(1)       SDARBT
095670040123     C*
095680040123     C                   MOVEL     'FIARBU0T'    SFILE
095690040123     C* ALLOCO OGGETTI
095700040123     C                   EXSR      ALLOC
095710070515     c                   if        *in91
095720070515     C                   MOVEL     SDARBT        SDLC(1)
095730070515     c                   exsr      DLCSED
095740070525     C                   SETON                                        2891
095750070515     C                   GOTO      ENDRG4
095760070515     c                   endif
095770070515     c
095780040123    2C                   ENDIF
095790040123    1C                   ENDIF
095800911212     C*
095810991007     C                   Z-ADD     DATEU         ARBDTV
095820991007     C                   TIME                    ARBORV
095830991007     C                   MOVEL     VIDCVR        ARBCVB
095840991011     C                   MOVEL     '2'           ARBTRC
095850991011     C                   MOVEL     '2'           AR6TRC
095860991007     C                   CLEAR                   ARBFST
095870991007     C                   CLEAR                   ARBDDS
095880991007     C                   CLEAR                   ARBQFT
095890991007     C                   CLEAR                   ARBVMD
095900991007     C                   CLEAR                   ARBVAD
095910991007     C                   CLEAR                   ARBIAS
095920991007     C                   CLEAR                   ARBVAS
095930911212     C*
095940941212     C* SE DA STORICIZZARE --> STORICIZZO
095950991007     C     WAR62         IFEQ      'E'
095960060130    1C     D66FSA        ANDEQ     'S'
095970060130     C**   *IN10         IFEQ      *ON
095980060130    1C**   D66FSA        ANDEQ     'S'
095990060130     C**   *IN10         OREQ      *OFF
096000060130    1C**   D66FSP        ANDEQ     'S'
096010991007     C                   EXSR      STOARB
096020941212     C                   END
096030060130     C**                 END
096040060302     c* storicizzazione motivazione
096050060302     c     d66mtv        ifeq      'S'
096060060302     c                   exsr      sto_mtv
096070060302     c                   endif
096080040123     C**
096090040123     C* INVIO VARIAZIONE --> FIARBU0T
096100040123     C**
096110040123    1C     *IN09         IFEQ      *ON
096120040123     C     C             ANDGT     3
096130040123     C                   Z-ADD     4             C
096140040123     C*
096150040123    3C     D66FFI        IFEQ      'S'
096160040123     C                   Z-ADD     4             C
096170040123     C                   OPEN      FIARBU0T
096180040123     C*
096190040123     C* NE SCRIVO FIN TANTO CHE CE NE SONO
096200040123    4C     WSV(C)        DOWNE     ' '
096210040123     C                   MOVEL     WSV(C)        ARBSVN
096220040123     C                   Z-ADD     WVA(C)        ARBVAN
096230040123     C                   WRITE     FIARBUTT
096240040123     C                   ADD       1             C
096250040123    4C                   ENDDO
096260040123     C*
096270040123     C                   CLOSE     FIARBU0T
096280040123     C* DISALLOCO OGGETTO MEMBRO PER SEDE
096290040123     C                   EXSR      DLCSED                                       FIARBU0T
096300040123    3C                   ENDIF
096310040123     C* REIMPOSTO I CAMPI
096320040123     C                   MOVEL     SOARBT        SOVR(1)
096330040123     C                   MOVEL     SAARBT        SALC(1)
096340040123     C                   MOVEL     SDARBT        SDLC(1)
096350040123    1C                   ENDIF
096360991011     C*
096370991011     C                   MOVEL     VIDKLP        ARBKSC
096380991007     C                   MOVE      VIDKSC        ARBKSC
096390991007     C                   CLEAR                   ARBPOR
096400991007     C                   MOVEL     VI2DIV        ARBDIV
096410040123     C**!!!              MOVEL     VI2SV1        ARBSV1
096420040123     C**!!!              MOVEL     VI2VA1        ARBVA1
096430040123     C                   MOVEL     WSV(1)        ARBSV1
096440040123     C                   Z-ADD     WVA(1)        ARBVA1
096450040123     C                   MOVEL     WSV(2)        ARBSV2
096460040123     C                   Z-ADD     WVA(2)        ARBVA2
096470040123     C                   MOVEL     WSV(3)        ARBSV3
096480040123     C                   Z-ADD     WVA(3)        ARBVA3
096490991007     C                   MOVEL     VI2CEI        ARBCEI
096500991007     C                   MOVEL     VIDCTR        ARBCTR
096510040123     C**!!!              Z-ADD     VI2VA1        ARBIMV
096520040123     C                   Z-ADD     TotImv        ARBIMV
096530040123     C**!!!              CLEAR                   ARBSV2
096540040123     C**!!!              CLEAR                   ARBVA2
096550040123     C**!!!              CLEAR                   ARBSV3
096560040123     C**!!!              CLEAR                   ARBVA3
096570040129     C**!!!              CLEAR                   ARBALI
096580040129     C**!!!              CLEAR                   ARBIFT
096590991007     C                   Z-ADD     VI2NFT        ARBNFT
096600991007     C                   Z-ADD     VI2FIV        ARBFIV
096610991007     C                   Z-ADD     AR6DFT        ARBDFT
096620991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
096630991013     C* STATO MODIFICATO TOLGO LA 'P'
096640991013     C     AR6FIM        IFEQ      'P'
096650991013     C     ARBIMV        ANDNE     AR6IMV
096660991013     C     *IN10         ANDEQ     *ON
096670991013     C                   MOVE      *BLANKS       ARBFIM
096680991013     C                   ELSE
096690991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
096700991013     C                   MOVE      AR6FIM        ARBFIM
096710991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
096720991013     C     WAR62         IFNE      'E'
096730991013     C     *IN10         ANDEQ     *OFF
096740991013     C                   MOVEL     'P'           ARBFIM
096750991013     C                   END
096760991013     C                   ENDIF
096770040129
096780040130if  1c                   If        Arbdft > 0
096790040129     C* SE CAMBIATO TOTALE FATTURA RISTAMPO IN AUTOMATICO LA BOLLA
096800040129     C     D04IFT        COMP      AR6IFT                             3737
096810040129
096820040129     C                   MOVE      D04ALI        ARBALI
096830040129     C                   Z-ADD     D04IFT        ARBIFT
096840040130     C                   z-add     D04IMV        ARBIMV
096850051111     c                   select
096860051111     C     ARBVA1        WHENGT    0
096870051111     C     ARBSV1        ANDNE     §$2BOL
096880051111     C     ARBSV1        ANDNE     §$2IVA
096890051111     C                   ADD       D04POR        ARBVA1
096900051111     C     ARBVA2        WHENGT    0
096910051111     C     ARBSV2        ANDNE     §$2BOL
096920051111     C     ARBSV2        ANDNE     §$2IVA
096930051111     C                   ADD       D04POR        ARBVA2
096940051111     C     ARBVA3        WHENGT    0
096950051111     C                   ADD       D04POR        ARBVA3
096960051111    3C                   ENDSL
096970040130     c                   EndIf
096980911212     C*
096990070503     C     D66FFI        IFEQ      'S'
097000991007     C                   OPEN      FIARBT0T
097010991007     C                   WRITE     FIARBTTT
097020991007     C                   CLOSE     FIARBT0T
097030911212     C* DISALLOCO OGGETTO
097040991007     C                   EXSR      DLCSED                                       FIARBT0T
097050911212     C                   END
097060911212     C*
097070911212     C* AGGIORNO
097080991013     C                   Z-ADD     ARBKSC        AR6KSC
097090991013     C                   Z-ADD     ARBCTR        AR6CTR
097100991013     C                   MOVEL     ARBDIV        AR6DIV
097110991013     C                   Z-ADD     ARBPOR        AR6POR
097120991013     C                   MOVEL     ARBSV1        AR6SV1
097130991013     C                   MOVEL     ARBSV2        AR6SV2
097140991013     C                   MOVEL     ARBSV3        AR6SV3
097150991013     C                   Z-ADD     ARBVA1        AR6VA1
097160991013     C                   Z-ADD     ARBVA2        AR6VA2
097170991013     C                   Z-ADD     ARBVA3        AR6VA3
097180991013     C                   Z-ADD     ARBIMV        AR6IMV
097190991013     C                   Z-ADD     ARBALI        AR6ALI
097200991013     C                   MOVEL     ARBCEI        AR6CEI
097210991013     C                   Z-ADD     ARBIFT        AR6IFT
097220991013     C                   Z-ADD     ARBFIV        AR6FIV
097230991013     C                   Z-ADD     ARBNFT        AR6NFT
097240991013     C                   Z-ADD     ARBDFT        AR6DFT
097250991013     C                   MOVEL     ARBFIM        AR6FIM
097260040123     C*
097270040123     C* CANCELLO LE VARIE NELL'FIAR7
097280040123    1C     WAR72         IFEQ      'E'
097290040123     C                   MOVEL     '2'           WTRC
097300040123     C     KARB2         SETLL     FIAR7000
097310040123     C     KARB2         READE     FIAR7000                               30
097320040123     C*
097330040123    2C     *IN30         DOWEQ     *OFF
097340040123     C                   DELETE    FIAR7000
097350040123     C     KARB2         READE     FIAR7000                               30
097360040123    2C                   ENDDO
097370040123    1C                   ENDIF
097380040123     C*
097390040123     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
097400040123    1C     WSV(4)        IFNE      ' '
097410040123     C                   Z-ADD     4             C
097420040123     C                   CLEAR                   FIAR7000
097430040123     C                   MOVEL     ARBAAS        AR7AAS
097440040123     C                   MOVEL     ARBLNP        AR7LNP
097450040123     C                   MOVEL     ARBNRS        AR7NRS
097460040123     C                   MOVEL     ARBNSP        AR7NSP
097470040123     C                   MOVEL     '2'           AR7TRC
097480040123    2C     WSV(C)        DOWNE     ' '
097490040123     C                   MOVEL     WSV(C)        AR7SVN
097500040123     C                   MOVEL     WVA(C)        AR7VAN
097510040123     C                   WRITE     FIAR7000
097520040123     C                   ADD       1             C
097530040123    2C                   ENDDO
097540040123    1C                   ENDIF
097550040123
097560990930     C     WAR62         IFEQ      'E'
097570991007     C                   UPDATE    FIAR6000
097580941212     C                   ELSE
097590991013     C                   CLEAR                   AR6ATB
097600991013     C                   Z-ADD     ARBAAS        AR6AAS
097610991013     C                   Z-ADD     ARBLNP        AR6LNP
097620991013     C                   Z-ADD     ARBNRS        AR6NRS
097630991013     C                   Z-ADD     ARBNSP        AR6NSP
097640991013     C                   MOVEL     '2'           AR6TRC
097650991007     C                   WRITE     FIAR6000
097660941212     C                   ENDIF
097670930211     C*
097680991013     C                   UNLOCK    FNARB01L
097690920204     C* RISTAMPO FATTURA
097700070309     c* ristampo fattura immediata se variata(37 on),
097710070309     c*   e se non attiva GEO
097720130321     C***  *IN37         IFEQ      *ON
097730130321     c***  FGSddaSI      andeq     ' '
097740130321     C***                CLEAR                   FNLSB5DS
097750130321     C***                MOVEL     'V'           DB0RIS
097760130321     C***                MOVEL     D48TBO        DB0TBO
097770130321     C***                MOVEL     ARBAAS        DB0AAS
097780130321     C***                MOVEL     ARBLNP        DB0LNP
097790130321     C***                MOVEL     ARBNRS        DB0NRS
097800130321     C***                MOVEL     ARBNSP        DB0NSP
097810130321     C***                CALL      D90PSL
097820130321     C***                PARM                    FNLSB5DS
097830130321     C***                END
097840130411
097850130411     c                   eval      wpda_ndc=arbndc
097860130411     c                   eval      wpda_ddc=arbddc
097870130411     c                   eval      wpda_dcm=arbdcm
097880130411     c* Richiamo routine per invio variazione a pda
097890130411     c                   exsr      sr_pda
097900911212     C*
097910911212     C     ENDRG4        ENDSR
097920920116     C*
097930920116     C*--- CONTROLLI FORMATO5 ----------------------------------------*
097940920117     C     CONTR5        BEGSR
097950920116     C* CODICE BOLLA NUOVO
097960941102    1C     *IN21         IFEQ      *OFF
097970941102     C*
097980941213     C     '?'           SCAN      NEWCBO                                 90
097990941213    2C     *IN90         IFEQ      *ON
098000941213     C                   MOVEL     CODUT         §KUT
098010920116     C                   MOVEL     '3A'          §COD
098020941102     C                   MOVE      *BLANKS       NEWCBO
098030991011     C                   CLEAR                   §KEY
098040920116     C                   CALL      'X§TABER'
098050920116     C                   PARM                    §KUT
098060920116     C                   PARM                    §COD
098070920116     C                   PARM                    §KEY
098080941209     C                   PARM                    §DES
098090941102     C                   MOVEL     §KEY          NEWCBO
098100941102     C                   MOVEL     §DES          DENCBO
098110920116     C                   SETON                                        90
098120920116     C                   GOTO      ENDCT5
098130941102    2C                   END
098140920116     C*
098150920116     C                   MOVEL     '3A'          COD
098160941102     C                   MOVEL(P)  NEWCBO        KEY
098170941212     C                   EXSR      CTABEL
098180920121     C*
098190941212    2C     *IN30         IFEQ      *ON
098200941102     C                   MOVEL     MSG(16)       VIDMSG
098210941102     C                   SETON                                        409028
098220941102     C                   GOTO      ENDCT5
098230941102    2C                   ENDIF
098240920117     C*
098250920116     C* CODICE NUOVO E VECCHIO NON POSSONO ESSERE UGUALI
098260940614     C* SE PGM RICHIAMATO NON CONTROLLO
098270941212    2C  N20OLDCBO        IFEQ      NEWCBO
098280941102     C                   MOVEL     MSG(17)       VIDMSG
098290941102     C                   SETON                                        409028
098300920116     C                   GOTO      ENDCT5
098310941102    2C                   END
098320920116     C*
098330920116     C                   MOVEL     TBLUNI        DS3A
098340920304     C*
098350941102     C                   MOVEL     §3ADES        DENCBO                          DESCRIZIONE
098360941102     C*
098370920116     C* NON POSSO TRASFORMARE IN BOLLA RECUPERO DA NON RECUPERO E VICEV
098380940322     C* SE RICHIAMATO PERMETTO TUTTO
098390941102    2C     *IN20         IFEQ      *OFF
098400940322     C*
098410941102    3C     §3ARBL        IFEQ      'R'
098420941102     C     SAVRBL        ANDNE     'R'
098430941102     C*
098440941102     C     §3ARBL        ORNE      'R'
098450941102     C     SAVRBL        ANDEQ     'R'
098460941102     C                   MOVEL     MSG(18)       VIDMSG
098470941102     C                   SETON                                        409028
098480920116     C                   GOTO      ENDCT5
098490941102    3C                   END
098500941102    2C                   END
098510920128     C*
098520941102     C* NON POSSO FARE VARIAZIONI DI PORTO
098530941212     C                   MOVEL     §3ATB1        NSCTB1            1
098540941212     C                   MOVEL     SAVTB1        OSCTB1            1
098550920116     C*
098560941102    2C     NSCTB1        IFNE      OSCTB1
098570941102     C                   MOVEL     MSG(19)       VIDMSG
098580941213     C                   SETON                                        409028
098590920128     C                   GOTO      ENDCT5
098600941102    2C                   ENDIF
098610150413     c* non ammesso contrasegno se bolle per EXPO
098620151119     c*                  if        §3afca=1 and (arbtc1='Z' or arbtc2='Z')
098630151119     C*                  MOVEL     MSG(159)      VIDMSG
098640151119     C*                  SETON                                        409028
098650151119     C*                  GOTO      ENDCT5
098660151119     c*                  endif
098670000510     C*
098680000510     C* BOLLE LNP POSTE: SOLO SPEDIZIONI FRANCIO
098690131008     C**   LNPNTW        IFEQ      'PPT'
098700131008     C**   NSCTB1        ANDNE     'F'
098710131008     C**                 MOVEL     MSG(30)       VIDMSG
098720131008     C**                 SETON                                        409028
098730131008     C**                 GOTO      ENDCT5
098740131008     C**                 ENDIF
098750000718     C* BOLLE LNP POSTE CON SERIE : NN POSSO TRASFRMARE IN C/S
098760131008     C**   LNPNTW        IFEQ      'PPT'
098770131008     C**   ARBNRS        ANDGT     0
098780131008     C**   §3ARBL        ANDEQ     'C'
098790131008     C**                 MOVEL     MSG(96)       VIDMSG
098800131008     C**                 SETON                                        409028
098810131008     C**                 GOTO      ENDCT5
098820131008     C**                 ENDIF
098830000925     C* BOLLE LNP POSTE: NON POSSO FARE BOLLE DOPPIE
098840131008     C**   LNPNTW        IFEQ      'PPT'
098850131008     C**   §3ATB2        ANDNE     '  '
098860131008     C**                 MOVEL     MSG(97)       VIDMSG
098870131008     C*+                 SETON                                        409028
098880131008     C**                 GOTO      ENDCT5
098890131008     C**                 ENDIF
098900050414     C* Se prepagato, non posso trasformare in C/S
098910050414     C     §3ARBL        ifeq      'C'
098920050414     c     war6          andeq     'E'
098930050414     c     ar6dft        andgt     0
098940060213     c     d48cvb        andne     'KM'
098950050414     C                   MOVEL     MSG(133)      VIDMSG
098960050414     C                   SETON                                        409028
098970050414     C                   GOTO      ENDCT5
098980050414     C                   ENDIF
098990950407     C*
099000950407     C* MEMORIZZO VARIAZIONE CODICE BOLLA
099010950407     C     NEWCBO        IFNE      SAVCBO
099020950407     C                   MOVE      *BLANKS       COM01A
099030950407     C                   MOVEL     NEWCBO        SAVCBO
099040950407     C                   END
099050941102    1C                   ENDIF
099060920120     C*
099070920121     C* CONTROLLO CONTRASSEGNO
099080941102    1C     §3AFCA        IFEQ      0
099090941213     C     VIDCAS        ANDNE     0
099100941213     C                   MOVEL     MSG(20)       VIDMSG
099110941213     C                   SETON                                        429028
099120941213     C                   GOTO      ENDCT5
099130941213    1C                   ENDIF
099140961204     C* NON POSSO INSERIRE IL C/ASSEGNO PER BOLLE DI RESO
099150961204    1C     §3AFCA        IFGT      0
099160980105     C     ARBFBR        IFEQ      'S'
099170961204     C                   MOVEL     MSG(73)       VIDMSG
099180961204     C                   SETON                                        429028
099190961204     C                   GOTO      ENDCT5
099200961204    1C                   ENDIF
099210980105     C* NON POSSO INSERIRE IL C/A SE BOLLE LEGATA CON LNP=LNA
099220980105     C  N21KARB          CHAIN     FNLBL01L                           30
099230980105    4C  N21*IN30         IFEQ      *OFF
099240980105     C     LBLLAP        ANDEQ     LBLLAN
099250980105     C                   MOVEL     MSG(78)       VIDMSG
099260980105     C                   SETON                                        429028
099270980105     C                   GOTO      ENDCT5
099280980105    1C                   ENDIF
099290980105     C                   ENDIF
099300941102     C*
099310941213     C* SE eliminato importo C/A, elimiare anche tutti i campi relativi
099320011018    1C     VIDCAS        IFEQ      0
099330011018    2C     VIDTIC        IFNE      *BLANKS
099340011018     C     VIDGCA        ORNE      '  '
099350011018     C                   MOVEL     MSG(56)       VIDMSG
099360011018     C                   SETON                                        419028
099370011018     C                   GOTO      ENDCT5
099380011018    2C                   END
099390011018    1C                   END
099400941102     C*
099410931118     C* IMPOSSIBILE ELIMINARE O VARIARE C/A SE GIA' INCASSATO
099420941220     C     SAVFCA        IFNE      0
099430941220     C     ARBICC        ANDNE     ' '
099440941220     C     ARBICC        ANDNE     'R'
099450941220     c*
099460941102    2C     VIDCAS        IFNE      AR9CAS
099470941102     C     VIDTIC        ORNE      AR9TIC
099480941102     C     VIDVCA        ORNE      AR9VCA
099490941102     C     VIDGCA        ORNE      AR9GCA
099500941102     C                   MOVEL     MSG(22)       VIDMSG
099510941102     C                   SETON                                        429028
099520931118     C                   GOTO      ENDCT5
099530941102    2C                   ENDIF
099540941102    1C                   ENDIF
099550920120     C*
099560920204     C* CAUSALE KB NON CONTROLLA ESISTENZA O MENO DEL C/A: OK TUTTO
099570941102    1C     VIDCVR        IFNE      'KB'
099580931227     C*
099590941102     C* SE NON RICHIAMATO NON POSSO USARE CERTI TIPI BOLLA
099600971229     C* NON CONTROLLO PER VARIAZIONE SOLO DI IMPORTO
099610971229     C  N21
099620971229    2CANN20§3AUCB        IFEQ      'N'
099630941102     C                   MOVEL     MSG(16)       VIDMSG
099640941102     C                   SETON                                        409028
099650931227     C                   GOTO      ENDCT5
099660941102    2C                   END
099670920204     C*
099680941102    2C     §3AFCA        IFNE      0
099690941212     C     VIDCAS        ANDEQ     0
099700941102     C                   MOVEL     MSG(21)       VIDMSG
099710941102     C                   SETON                                        429028
099720920120     C                   GOTO      ENDCT5
099730941102    2C                   END
099740941102    1C                   END
099750941213     C**
099760941213     C* SE IMMESSO IMPORTO C/A , CONTROLLO I CAMPI RELATIVI
099770941213     C**
099780941213    1C     VIDCAS        IFGT      0
099790950120     C* SE ACCESO INDICATORE DI VARIAZIONE C/ASS LO SALVO
099800950120     C     *IN95         IFEQ      *ON
099810950120     C                   MOVEL     '1'           WIN95             1
099820950120     C                   ENDIF
099830140515     c                   clear                   trulintds
099840140515     c                   eval      iinttip='B'
099850140515     c                   eval      iinttic=vidtic
099860140515     c                   eval      iintrscm=v1crsm
099870140515     c                   eval      iintrsco=arbrmo
099880140515     c                   eval      IINTAAS=d48aas
099890140515     c                   eval      IINTLNP=d48lnp
099900140515     c                   eval      IINTNRS=d48nrs
099910140515     c                   eval      IINTNSP=d48nsp
099920140515     c                   call      'TRULINTR'
099930140515     c                   parm                    trulintds
099940950120     C*
099950920204     C* CONTROLLO TIPO INCASSO C/A
099960060705     C* richiamo pgm TRULTICR
099970060705     c                   clear                   trulticds
099980060705     c                   clear                   ds1a
099990060705     c                   eval      iticTBO='A'
100000060705     c                   eval      iticlnp=arblnp
100010060705     c                   eval      iticlna=arblna
100020060705     c                   eval      iticlnantw=lnantw
100030060705     c                   eval      iticnzd=arbnzd
100040090707     c                   eval      iticprd=arbprd
100050090707     c                   eval      iticcad=arbcad
100060090707     c                   eval      iticlod=arblod
100070090707     c                   eval      iticind=arbind
100080090707     c                   eval      iticrsd=arbrsd
100090060705     c                   eval      iticdsp=arbdsp
100100130827    3C***  ARBCCM        IFGT      0
100110130827     C***                MOVEL     ARBCCM        iticccm
100120130827     C***                ELSE
100130130827     C***                MOVEL     ARBKSC        iticccm
100140130827    3C***                END
100150130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
100160130827     c                   if        flgtb1='F'
100170130827     c                   z-add     arbksc        iticccm
100180130827     c                   else
100190130827     c                   z-add     arbccm        iticccm
100200130827     c                   if        iticccm=0
100210130827     c                   z-add     8888          iticccm
100220130827     c                   movel     arblnp        iticccm
100230130827     c                   endif
100240130827     c                   endif
100250140515     c* se il beneficiario del c/a è l'ordinante allora passo questo in iticccm
100260140515     c                   if        ointtipi='R'
100270140515     c                   move      ointcdi       iticccm
100280140515     c                   endif
100290060705     c                   eval      itictic=vidtic
100300060705     c                   eval      iticticbol=ar9tic
100310060705     c                   eval      iticdiv=vidvca
100320060705     c                   eval      iticcas=vidcas
100330060705     c                   eval      iticpgm='FNLR48R'
100340060705     c                   call      'TRULTICR'
100350060705     c                   parm                    TRULTICDS
100360060705     c                   parm                    ds1a
100370060705     c
100380060705     c                   if        oticric='T'
100390060705     c                   movel     otictic       vidtic
100400060705     C                   SETON                                        4190
100410060705     C                   GOTO      ENDCT5
100420060705     c                   endif
100430060705     c                   if        oticric='D'
100440060705     c                   movel     oticdiv       vidvca
100450060705     C                   SETON                                        4390
100460060705     C                   GOTO      ENDCT5
100470060705     c                   endif
100480060705     c* Errore tipo incasso
100490091211     c                   if        oticerr='T' or oticerr='C'
100500060705     C                   MOVEL     oticmsg       VIDMSG
100510060705     C                   SETON                                        419028
100520060705     C                   GOTO      ENDCT5
100530060705    2C                   ENDIF
100540060705     c* Errore divisa
100550060705     c                   if        oticerr='D'
100560060705     C                   MOVEL     oticmsg       VIDMSG
100570060705     C                   SETON                                        439028
100580060705     C                   GOTO      ENDCT5
100590060705    2C                   ENDIF
100600011010      *
100610941102     C*
100620941102     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE IL C/A
100630941102     C* INDICATORE DI CHANGE 95 ON
100640941213    2C     VARVCA        IFNE      VIDVCA
100650950120     C     WIN95         ANDEQ     *BLANKS
100660950120     C*
100670941219     C     VIDVCA        IFNE      *BLANKS
100680941219     C     VIDCAS        ORNE      0
100690941102     C                   MOVEL     MSG(28)       VIDMSG
100700941215     C                   SETON                                        429028
100710941102     C                   GOTO      ENDCT5
100720941213    2C                   ENDIF
100730941219    2C                   ENDIF
100740941213     C*
100750011206
100760011206      * pulisco il flag x la forzatura c/assegno forzabile
100770011206     c                   if        vidcas <> sav1_vidcas
100780011206     c                   clear                   forzacas
100790011206     c                   endif
100800011206      *-------------------
100810011206  2b c                   if        vidcas <> 0
100820011206     c                   eval      sav1_vidcas = vidcas
100830011206      * Controllo il c/assegno tramite il trul21r
100840011206     c                   clear                   trul21ds
100850011206     c                   eval      i21tla = 'L'
100860011206     c                   if        *in21 = *off
100870011206     c                   eval      i21cbo = newcbo
100880011206     c                   else
100890011206     c                   eval      i21cbo = arbcbo
100900011206     c                   endif
100910011206     c                   eval      i21tsp = arbtsp
100920011206     c                   eval      i21lnp = arblnp
100930011206     c                   eval      i21nzm = arbnzm
100940011206     c                   eval      i21lna = arblna
100950011206     c                   eval      i21nzd = arbnzd
100960130827     c*****              eval      i21ksc = arbksc
100970011206     c                   movel     arbctr        i21ctr
100980011206     c                   eval      i21tic = vidtic
100990011206     c                   eval      i21imp = vidcas
101000011206     c                   eval      i21div = vidvca
101010011206     c                   eval      i21ute = knmus
101020011206     c                   eval      i21pgm = nompgm
101030130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
101040130827     c                   if        flgtb1='F'
101050130827     c                   z-add     arbksc        i21ksc
101060130827     c                   else
101070130827     c                   z-add     arbccm        i21ksc
101080130827     c                   if        i21ksc=0
101090130827     c                   z-add     8888          i21ksc
101100130827     c                   movel     arblnp        i21ksc
101110130827     c                   endif
101120130827     c                   endif
101130011206     c                   call      'TRUL21R'
101140011206     c                   parm                    trul21ds
101150011206      * controllo che non sia minore del limite previsto
101160011206  3b c                   if        o21fmn <> *blanks
101170011206     c                   eval      vidmsg = msg(108)
101180011210     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
101190011206     c                   eval      *in42 = *on
101200011206     c                   eval      *in90 = *on
101210011206     c                   eval      *in28 = *on
101220011206     c                   goto      endct5
101230011206  3e c                   endif
101240011206      * controllo che non sia maggiore del limite previsto
101250011206      * non forzabile
101260011206  3b c                   if        o21fx2 <> *blanks
101270011206     c                   eval      vidmsg = msg(91)
101280011210     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
101290011206     c                   eval      *in42 = *on
101300011206     c                   eval      *in90 = *on
101310011206     c                   eval      *in28 = *on
101320011206     c                   goto      endct5
101330011206  3e c                   endif
101340050301      * controllo che non sia maggiore del limite per bancanri bartolini no
101350050301      * abilitati da sede, non forzabile
101360050301  3b c                   if        o21fb <> *blanks
101370050301     c                   eval      vidmsg = msg(132)
101380050301     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
101390050301     c                   eval      *in42 = *on
101400050301     c                   eval      *in90 = *on
101410050301     c                   eval      *in28 = *on
101420050301     c                   goto      endct5
101430050301  3e c                   endif
101440011206      * controllo che non sia maggiore del limite previsto
101450011206      * forzabile
101460011206  3b c                   if        o21fx1 <> *blanks and forzacas = *blanks
101470011206     c                   eval      forzacas = '1'
101480011206     c                   eval      vidmsg = msg(109)
101490011210     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
101500011206     c                   eval      *in42 = *on
101510011206     c                   eval      *in90 = *on
101520011206     c                   eval      *in28 = *on
101530011206     c                   goto      endct5
101540011206  3e c                   endif
101550011206  2e c                   endif
101560011206
101570011120      *
101580011120      * Se bolla giacente e modifica 'KA' o 'KP' l'importo immesso (convertito) deve
101590011120      * essere uguale all'originario
101600011120  2b c                   if        *in04 = *ON  and (vidcvr = 'KA' or
101610011120     c                             vidcvr = 'KP')
101620011120     c                   z-add     vidcas        w0173
101630011120      * Cerco i decimali previsti per la divisa originaria
101640011120     c                   movel     'CV'          cod
101650011120     c                   movel(p)  ar9vca        key
101660011120     c                   exsr      ctabel
101670011120     c  n30              movel     tbluni        dscv
101680011120     c   30              clear                   dscv
101690011120      * Controlla la divisa
101700011120  3b c                   if        ar9vca <> vidvca
101710011120      * Converto il contrassegno inserito a video nella divisa originaria
101720030612     c                   clear                   YeurcoDS
101730011120     c                   movel     vidvca        yecdvi
101740011120     c                   movel     ar9vca        yecdvo
101750011120     c                   z-add     vidcas        yecimi
101760011120     c                   move      §cvdec        yecdco
101770011120     c                   call      'YEURCO'
101780030612     c                   parm                    YeurcoDS
101790011120  4b c                   if        yecesi = ' '
101800011120     c                   z-add     yecimo        w0173
101810011120  4e c                   endif
101820011120  3e c                   endif
101830011120      * Calcolo la differenza tra i 2 contrassegni
101840011120     c                   eval      wdiffe = (ar9cas - w0173)
101850011120     c                   mllzo     1             wdiffe
101860020103      * Converto la differenza nella divisa di conto
101870011120  3b c                   if        ar9vca <> §gedcn
101880030612     c                   clear                   YeurcoDS
101890020103     c                   movel     ar9vca        yecdvi
101900020103     c                   movel     §gedcn        yecdvo
101910011120     c                   z-add     wdiffe        yecimi
101920020103     c                   move      decsav        yecdco
101930011120     c                   call      'YEURCO'
101940030612     c                   parm                    YeurcoDS
101950011120  4b c                   if        yecesi = ' '
101960011120     c                   z-add     yecimo        wdiffe
101970011120  4e c                   endif
101980011120  3e c                   endif
101990020103      *
102000020103      * Se differenza maggiore del valore in tabella ERRORE
102010011120  3b c                   if        wdiffe > dfcsav
102020011120     c                   movel     msg(103)      vidmsg
102030011120     c                   seton                                        429028
102040011120     c                   goto      endct5
102050011120  3e c                   endif
102060011120  2e c                   endif
102070011120
102080991021     C**
102090991021     C                   MOVEL     VIDVCA        VARVCA
102100991021     C                   CLEAR                   WIN95
102110941102     c**
102120941102     C* PARTICOLARITA' C/ASSEGNO
102130941102     c**
102140941213    2C     VIDGCA        IFNE      *BLANKS
102150941102     C     VIDGCR        ORNE      *BLANKS
102160941102     C*
102170941102     C* non posso indicarla se NON IMMESSO C/A
102180941213    3C     VIDCAS        IFEQ      0
102190941102     C     VIDGCA        ANDNE     *BLANKS
102200941102     C                   SETON                                        449028
102210941102     C                   MOVEL     MSG(29)       VIDMSG
102220941102     C                   GOTO      ENDCT5
102230941213    3C                   ENDIF
102240941102     C*
102250941102     C* RICHIAMO IL PRG DI RICERCA
102260941213    3C     VIDGCR        IFEQ      '?'
102270030612     C                   CLEAR                   DS7pqrs
102280941102     C                   MOVEL     'S'           DS7RIC
102290941102     C                   MOVEL     'V'           DS7GES
102300941213    4C     ARBCCM        IFGT      0
102310941102     C                   MOVEL     ARBCCM        DS7KSC
102320941102     C                   ELSE
102330941102     C                   MOVEL     ARBKSC        DS7KSC
102340941213    4C                   ENDIF
102350941102     C                   MOVEL     VIDGCA        DS7COD
102360030612     C                   MOVEL     DS7pqrs       KPJBU
102370941102     C                   CALL      'TRTB72R'
102380941102     C                   PARM                    KPJBA
102390030612     C                   MOVEL     KPJBU         DS7pqrs
102400941102     C*
102410941102     C     VIDGCA        IFEQ      *BLANKS
102420941102     C                   MOVEL     DS7COD        VIDGCA
102430941213    3C                   ENDIF
102440941102     C*
102450941102     C                   CLEAR                   VIDGCR
102460941102     C*
102470941102     C                   SETON                                        90
102480941102     C                   GOTO      ENDCT5
102490941102    2C                   ENDIF
102500941102     C* CONTROLLO
102510030612     C                   CLEAR                   DS7pqrs
102520941102     C                   MOVEL     'S'           DS7RIC
102530941102     C                   MOVEL     'C'           DS7GES
102540941213    2C     ARBCCM        IFGT      0
102550941102     C                   MOVEL     ARBCCM        DS7KSC
102560941102     C                   ELSE
102570941102     C                   MOVEL     ARBKSC        DS7KSC
102580941102     C                   ENDIF
102590941102     C                   MOVEL     VIDGCA        DS7COD
102600030612     C                   MOVEL     DS7pqrs       KPJBU
102610941102     C                   CALL      'TRTB72R'
102620941102     C                   PARM                    KPJBA
102630030612     C                   MOVEL     KPJBU         DS7pqrs
102640941102     C* ERRORE
102650941213    3C     DS7ERR        IFEQ      '1'
102660941102     C                   MOVEL     DS7MSG        VIDMSG
102670941102     C                   SETON                                        449028
102680941102     C                   GOTO      ENDCT5
102690941213    3C                   ENDIF
102700941102     C*
102710941213    2C                   ENDIF
102720941213    1C                   ENDIF
102730011010      *-------------------
102740941220     C*
102750941220     c* SE MODIFICATO IK TIPO BOLLA ED ELIMINATA LA 2 MA GIA' INCASS
102760941220     C*  ERRORE
102770941220     C     §3ATB2        IFEQ      *BLANKS
102780941220     C*
102790941220     C     SAVTB2        ANDNE     *BLANKS
102800941220     C     ARBICA        ANDNE     ' '
102810941220     C     ARBICA        ANDNE     'R'
102820991007     C     DFT2BO        ANDGT     0
102830941220     C                   MOVEL     MSG(59)       VIDMSG
102840941220     C                   SETON                                        409028
102850941220     C                   GOTO      ENDCT5
102860941220     C                   END
102870950407     C* SE CAUSALE VARIAZIONE CODICE BOLLA
102880950407    1C     *IN21         IFEQ      *OFF
102890950407     C     COM01A        ANDEQ     *BLANKS
102900950407     C* E NON TROVATI ERRORI BLOCCANTI
102910950407     C     *IN90         ANDEQ     *OFF
102920950407     C* E ASSEGNATO
102930950407     C     FLGTB1        ANDEQ     'A'
102940950407     C* SE AGGIUNTO O TOLTO IL CONTRASSEGNO
102950950407    2C     SAVFCA        IFEQ      *ZEROS
102960950407     C     §3AFCA        ANDNE     *ZEROS
102970950407     C     SAVFCA        ORNE      *ZEROS
102980950407     C     §3AFCA        ANDEQ     *ZEROS
102990991007     C                   MOVEL     '1'           WTRC
103000991007     C     KARB2         CHAIN(N)  FIAR6000                           30
103010950407    3C     *IN30         IFEQ      *OFF
103020950407     C* SE GIA' TASSATO FINO ALL'IMPONIBILE
103030950407     C     AR6IMV        ANDNE     *ZEROS
103040950407     C                   SETON                                        2892
103050950407     C* ERRORE NON BLOCCANTE:MANCA VARIA G
103060950407    4C     §3AFCA        IFNE      *ZEROS
103070950407     C                   MOVEL     MSG(65)       VIDMSG
103080950407     C                   ELSE
103090950407     C* ERRORE NON BLOCCANTE:C'E' VARIA G
103100950407     C                   MOVEL     MSG(66)       VIDMSG
103110950407    4C                   END
103120950407    3C                   END
103130950407     C*
103140950407    2C                   END
103150950407     C                   MOVE      '1'           COM01A            1
103160950407    1C                   END
103170920116     C*
103180920116     C     ENDCT5        ENDSR
103190920116     C*
103200920116     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE KX -----------------*
103210920129     C     REGIS5        BEGSR
103220991001     C                   SETOFF                                       903839
103230130412     c                   clear                   wpda_ndc
103240130412     c                   clear                   wpda_ddc
103250130412     c                   clear                   wpda_dcm
103260071116     C
103270950104     C*  GIRO UDATE
103280950104     C                   TIME                    W0140            14 0
103290950104     C* UDATE IN GGMMAAAA
103300950104     C                   MOVE      W0140         WDTGIO            8 0
103310950104     C*
103320950104     C* UDATE IN AAAAMMGG
103330950104     C                   Z-ADD     WDTGIO        G02DAT
103340950104     C                   MOVEL     *BLANK        G02ERR
103350950104     C                   CALL      'XSRDA8'
103360950104     C                   PARM                    WLBDAT
103370950104     C                   MOVEL     G02INV        DATEU             8 0
103380941221     C* PRENDO I VALORI ASSOLUTI
103390941221     C                   MLLZO     1             VIDCAS
103400941221     C*
103410941213     C                   MOVEL     KNMUS         ARBPRU
103420941220     C                   MOVEL     ARBCPI        SAVCPI
103430070515     c
103440070515     c* Se modificato il C/A, devo allocarmi record "L" per il
103450070515     c*  blocco telefonata C/A
103460070515     c                   clear                   wesar4L           1
103470070515    1C     vidCAS        IFNE      SAVCAS
103480070515     C     vidTIC        ORNE      SAVTIC
103490070515     C     vidVCA        ORNE      SAVVCA
103500070807     C     vidcas        andgt     0
103510070515     c                   eval      Tipoela='C'
103520070515     c                   exsr      GestAR4L
103530070515    2c                   if        *in91
103540070515     C                   GOTO      ENDRG5
103550070515    2c                   endif
103560070515    1c                   endif
103570070515     c
103580070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
103590070515     c* iornare direttamente il file solo se cambiato tipo bolla
103600070515     c                   clear                   waltrabolla       1
103610130412     c                   setoff                                       3091
103620130412     c                   if        oldcbo<>newcbo
103630070515     C   10KARB          CHAIN     FNBLP01L                           3091
103640070515     C  n10KARB          CHAIN     FNARB01L                           3091
103650130412     c                   else
103660130412     c* questa chain mi serve per l'invio dati a PDA
103670131120     c                   if        d66pda<>' '
103680130412     C  n10KARB          CHAIN(n)  FNARB01L                           30
103690130412     c                   endif
103700130412     c                   endif
103710070515     c*
103720070515     c                   if        *in91
103730070515     C                   SETON                                        28
103740070515     C                   MOVEL     MSG(4)        VIDMSG
103750070515     c                   unlock    fiar401l
103760070515     C                   GOTO      ENDRG5
103770070515     c                   else
103780070515     c                   if        *in30
103790070515     c                   eval      waltrabolla='N'
103800070515     c                   else
103810130411     c  n10              eval      wpda_ndc=arbndc
103820130411     c  n10              eval      wpda_ddc=arbddc
103830130411     c  n10              eval      wpda_dcm=arbdcm
103840130412     c                   if        oldcbo<>newcbo
103850070515     C   10KARB          CHAIN     FNARB01L                           30
103860130412     c                   endif
103870131120     c                   if        oldcbo<>newcbo or d66pda<>' '
103880070515     C  N10KARB          CHAIN     FNBLP01L                           30
103890070515     c* Alla fine per non sporcarmi i campi del record, richaino il
103900070515     c*  record originale che sto variando
103910130411     c   10              eval      wpda_ndc=arbndc
103920130411     c   10              eval      wpda_ddc=arbddc
103930130411     c   10              eval      wpda_dcm=arbdcm
103940130412     c                   endif
103950070515     c                   endif
103960070515     c                   endif
103970130412     c*****              endif
103980920123     C*********
103990920116     C* INVIO PER SEDE
104000941014     C                   MOVEL     'FNARBK0T'    SFILE
104010920116     C* ALLOCO OGGETTI
104020920116     C                   EXSR      ALLOC
104030070515     c                   if        *in91
104040070515     c                   unlock    fiar401l
104050070515     c   10              unlock    fnblp01l
104060070515     c  n10              unlock    fnarb01l
104070070515     C                   GOTO      ENDRG5
104080070515     c                   endif
104090930511     C*
104100941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL MITTENTE PER C/A
104110941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL DESTINATARIO SE DOPPIA BOLLA
104120930511     C                   MOVEL     'S'           §7LFFI
104130930511     C*
104140930210     C********* PER  C O N T R A S S E G N O
104150941215    1C     AR9CAS        IFEQ      0
104160941102     C     VIDCAS        ANDNE     0
104170930210     C                   SETON                                        38
104180930210     C* INVIO PER SEDE
104190941014     C                   MOVEL     'FNARBM0T'    SFILE
104200930210     C* ALLOCO OGGETTI
104210930210     C                   EXSR      ALLOC
104220070515     c
104230070515     c                   if        *in91
104240070515     C                   MOVEA     'FNARBK0T'    C21(13)
104250070515     C                   MOVEA     VAR           C21(41)
104260070515     C                   MOVEL     *BLANKS       COMMAN           80
104270070515     C                   MOVEA     C21           COMMAN
104280070515     C                   CALL      'QCMDEXC'                            H1
104290070515     C                   PARM                    COMMAN
104300070515     C                   PARM                    LUNG
104310070515     c                   unlock    fiar401l
104320070515     c   10              unlock    fnblp01l
104330070515     c  n10              unlock    fnarb01l
104340070515     c
104350070515     C                   GOTO      ENDRG5
104360941212    1C                   END
104370070515    1C                   END
104380950324     C*
104390930210     C********* PER  D O P P I A    B O L L A
104400941212    1C     SAVTB2        IFEQ      '  '
104410930210     C     §3ATB2        ANDNE     '  '
104420930210     C                   SETON                                        39
104430930210     C* INVIO PER SEDE
104440941102     C                   MOVEL     'FNARBD0T'    SFILE
104450930210     C* ALLOCO OGGETTI
104460930210     C                   EXSR      ALLOC
104470070515     c                   if        *in91
104480070515     C                   MOVEA     'FNARBK0T'    C21(13)
104490070515     C                   MOVEA     VAR           C21(41)
104500070515     C                   MOVEL     *BLANKS       COMMAN           80
104510070515     C                   MOVEA     C21           COMMAN
104520070515     C                   CALL      'QCMDEXC'                            H1
104530070515     C                   PARM                    COMMAN
104540070515     C                   PARM                    LUNG
104550070515     c
104560070515     C                   MOVEA     'FNARBM0T'    C21(13)
104570070515     C                   MOVEL     *BLANKS       COMMAN           80
104580070515     C                   MOVEA     C21           COMMAN
104590070515     C                   CALL      'QCMDEXC'                            H1
104600070515     C                   PARM                    COMMAN
104610070515     C                   PARM                    LUNG
104620070515     c                   unlock    fiar401l
104630070515     c   10              unlock    fnblp01l
104640070515     c  n10              unlock    fnarb01l
104650070515     C                   GOTO      ENDRG5
104660950324     C                   ENDIF
104670070515     C                   ENDIF
104680041022     C*********
104690041022     C                   Z-ADD     DATEU         ARBDTV
104700060320     C                   TIME                    ARBORV
104710920116     C* STORICIZZAZIONE ALL'ARRIVO
104720060130    1C     D66FSA        ifeq      'S'
104730920116     C                   MOVEL     VIDCVR        ARBCVB
104740941213     C                   MOVEL     OLDCBO        ARBCBP
104750941213     C                   MOVEL     NEWCBO        ARBCBN
104760941213     C                   MOVEL     AR9TIC        ARBTIC
104770941213     C                   MOVEL     AR9CAS        ARBCAS
104780941213     C                   MOVEL     AR9VCA        ARBVCA
104790941213     C                   MOVEL     AR9GCA        ARBGCA
104800011108      *
104810011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
104820011108      * è in partenza 'P' o in arrivo 'A'
104830011108     C     *IN10         IFEQ      *ON
104840011108     C                   MOVE      'A'           ARBPRU
104850011108     C                   ENDIF
104860011108     C     *IN10         IFEQ      *OFF
104870011108     C                   MOVE      'P'           ARBPRU
104880011108     C                   ENDIF
104890011108      *
104900941014     C                   WRITE     FNARBK00
104910930210     C                   END
104920060213     c* storicizzazione motivazione
104930060213     c     d66mtv        ifeq      'S'
104940060301     c                   exsr      sto_mtv
104950060213     c                   endif
104960920116     C*
104970920123     C*********
104980920117     C* I N V I O
104990941102     C                   MOVEL     NEWCBO        ARBCBN
105000941213     C* PER IL FILE DI VARIAZIONE
105010941102     C                   MOVEL     VIDCAS        ARBCAS
105020920116     C                   MOVEL     VIDTIC        ARBTIC
105030941212     C                   MOVEL     VIDGCA        ARBGCA
105040941213     C*
105050941215     C*
105060941212     C                   MOVEL     VIDVCA        ARBVCA
105070011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
105080011017     C     VIDCAS        IFEQ      *ZEROS
105090011017     C                   CLEAR                   ARBVCA
105100011017     C                   ENDIF
105110920123     C*********
105120041022     C     D66FFI        ifeq      'S'
105130920117     C                   TIME                    ARBORV
105140920127     C                   MOVEL     VIDCVR        ARBCVB
105150941014     C                   OPEN      FNARBK0T
105160941014     C                   WRITE     FNARBKTT
105170941014     C                   CLOSE     FNARBK0T
105180920117     C* DISALLOCO
105190941014     C                   MOVEA     'FNARBK0T'    C21(13)
105200041022     C                   MOVEA     VAR           C21(41)
105210920117     C                   MOVEL     *BLANKS       COMMAN           80
105220920117     C                   MOVEA     C21           COMMAN
105230920117     C                   CALL      'QCMDEXC'                            H1
105240920117     C                   PARM                    COMMAN
105250920117     C                   PARM                    LUNG
105260920212     C*
105270920212     C   38              DO
105280941014     C                   OPEN      FNARBM0T
105290950102     C                   WRITE     FNARBMTT
105300941014     C                   CLOSE     FNARBM0T
105310920212     C* DISALLOCO
105320941014     C                   MOVEA     'FNARBM0T'    C21(13)
105330920212     C                   MOVEA     C21           COMMAN
105340920212     C                   CALL      'QCMDEXC'                            H1
105350920212     C                   PARM                    COMMAN
105360920212     C                   PARM                    LUNG
105370920212     C                   END
105380920213     C*
105390920213     C   39              DO
105400930210     C                   MOVEL     *BLANKS       ARBCPI
105410920213     C                   ADD       1             ARBORV
105420930210     C                   MOVEL     VIDCVR        ARBCVB
105430941212     C                   MOVEL     NEWCBO        ARBCBO
105440941102     C                   OPEN      FNARBD0T
105450941209     C                   WRITE     FNARBDTT
105460941102     C                   CLOSE     FNARBD0T
105470920213     C* DISALLOCO
105480941102     C                   MOVEA     'FNARBD0T'    C21(13)
105490920213     C                   MOVEA     C21           COMMAN
105500920213     C                   CALL      'QCMDEXC'                            H1
105510920213     C                   PARM                    COMMAN
105520920213     C                   PARM                    LUNG
105530920213     C                   END
105540930210     C                   END
105550920117     C*
105560920117     C* A G G I O R N O   F I L E
105570920117     C*
105580051118     c* se variato imposto la data ultima variazione
105590091217    1c                   if        vidvca<>ar9vca or
105600051118     c                             ar9cas<>vidcas or
105610051118     c                             ar9vca<>vidvca or
105620051118     c                             ar9gca<>vidgca
105630051118     c                   movel     dateu         ar9duv
105640091217    1c                   endif
105650091217     c
105660091217     c* Se variato il tipo incasso C/Ass, lo memorizzo
105670140516     c* non lo memorizzo in caso di annullamento altrimenti su internet viene
105680140516     c* visualizzata sia la variazione che l'annullamento
105690140516    1c                   if        savtic<>vidtic and vidcas>0
105700091217     c                   eval      Wtipoagg='TIC'
105710091217     c                   exsr      RegisAR5GEN
105720091217    1c                   EndIf
105730091217     c
105740091217     c
105750941212     C                   MOVEL     NEWCBO        ARBCBO
105760941212     C                   MOVEL     VIDCAS        AR9CAS
105770941212     C                   MOVEL     VIDTIC        AR9TIC
105780941212     C                   MOVEL     VIDGCA        AR9GCA
105790941212     C                   MOVEL     VIDVCA        AR9VCA
105800011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
105810011017     C     VIDCAS        IFEQ      *ZEROS
105820011017     C                   CLEAR                   AR9VCA
105830011017     C                   ENDIF
105840011019      *
105850941220     C                   MOVEL     ARBCPI        SAVCPI
105860920121     C*
105870941220     C* E L I M I N A T A   L A   2   B O L L A
105880941220    1C     SAVTB2        IFNE      '  '
105890920603     C     §3ATB2        ANDEQ     '  '
105900941220     C* PULISCO PARTITA IVA
105910930210     C                   MOVEL     *BLANKS       ARBCPI
105920991001     C* SE C'ERA AR6 SECONDA BOLLA LO DELETO
105930990930     C     WAR62         IFEQ      'E'
105940991008     C                   MOVEL     '2'           WTRC
105950991008     C     KARB2         DELETE    FIAR6000                           30
105960941220     C                   ENDIF
105970941220     C*
105980941220     C* SE GIA' CONSEGNATA DO' PER INCASSATO L'ASSEGNATO
105990000925    2C     ARBDCM        IFGT      0
106000000925    3C     ARBICA        IFEQ      'R'
106010000925     C     ARBICA        OREQ      ' '
106020941220     C                   MOVEL     'Y'           ARBICA
106030000925    4C     ARBICC        IFNE      'R'
106040970613     C                   EXSR      CHIUBO
106050000925    5C     ARBNMI        IFNE      8888888
106060971023     C                   MOVEL     'S'           ARBICA
106070000925    5C                   ENDIF
106080000925    4C                   ENDIF
106090000925    3C                   ENDIF
106100000925    2C                   ENDIF
106110941220     C* ABILITO L'ASSEGNATO
106120941220     C                   MOVEL     'S'           ARBACA
106130941220    1C                   ENDIF
106140941220     C*
106150941220     C* A G G I U N T A   2   B O L L A
106160941220     C     SAVTB2        IFEQ      '  '
106170941220     C     §3ATB2        ANDNE     '  '
106180941220     C* PULISCO PARTITA IVA
106190941220     C                   MOVEL     *BLANKS       ARBCPI
106200941220     C*
106210941220     C                   MOVEL     ' '           ARBACA
106220941220     C                   MOVEL     ' '           ARBICA
106230941220     C* SE GIA' CONSEGNATA SI TRATTA DI UN RITORNO ALL'INCASSO
106240941220     C     ARBDCM        IFGT      0
106250941220     C                   MOVEL     'R'           ARBICA
106260941220     C                   ENDIF
106270941220     C                   ENDIF
106280920120     C*
106290920117     C* ABILITAZIONE CONTRASSEGNO
106300950918     C* SE NON HO VARIATO IL C/A RISPETTO A PRIMA, LASCIO ABILITAZIONE
106310950918     C*  E INCASSO C/A COME ERANO
106320950918    1C     AR9CAS        IFNE      SAVCAS
106330950918     C     AR9TIC        ORNE      SAVTIC
106340950918     C     AR9VCA        ORNE      SAVVCA
106350920120     C                   MOVEL     ' '           ARBACC
106360070320     c                   eval      §b4ltca='S'
106370070320     c
106380941220     C*
106390920117     C* SE NON C'E' PIU' IL C/ASSEGNO ABILITO
106400950918    2C     §3AFCA        IFEQ      0
106410950104     C     AR9CAS        OREQ      0
106420920117     C                   MOVEL     'S'           ARBACC
106430070320     c                   eval      §b4ltca=' '
106440960704     C*
106450960909     C* SE IL CONTRASSEGNO E' UN RITORNO ALL'INCASSO, LO TOLGO
106460960704     C* SE CONSEGNATO, E RITORNO ALL'INCASSO LO DO PER INCASSATO
106470960704    3C     ARBDCM        IFGT      0
106480960704     C     ARBICC        ANDEQ     'R'
106490960704     C                   MOVEL     'Y'           ARBICC
106500970613     C* SE L'ASSEGNATO NON E' UN RITORNO ALL'INCASSO VEDO SE CHIUDERE
106510970613     C* LA BOLLA
106520970613     C     ARBICA        IFNE      'R'
106530970613     C                   EXSR      CHIUBO
106540971023     C     ARBNMI        IFNE      8888888
106550971023     C                   MOVEL     'S'           ARBICC
106560971023     C                   ENDIF
106570970613     C                   ENDIF
106580970613     C                   ENDIF
106590960704     C*
106600950918   X2C                   ELSE
106610941220     C*
106620941220     C* SE LA BOLLA E' GIA' CONSEGNA METTO "R" DI RITORNO ALL'INCASSO
106630950918    3C     ARBDCM        IFGT      0
106640941220     C                   MOVEL     'R'           ARBICC
106650950918   X3C                   ELSE
106660941220     C* ALTRIMENTI PULISCO IL FLAG DI INCASSO C/A
106670941220     C                   CLEAR                   ARBICC
106680950918    3C                   ENDIF
106690011031      *
106700070320    3c                   if        o21fca = *blanks
106710020103     c                   eval      arbacc = 'S'
106720070320     c                   clear                   §b4ltca
106730070320    3c                   endif
106740070320    2C                   END
106750070320     c
106760070320     c* Aggiorno record di FIar4
106770070320     c                   eval      TipoEla='A'
106780070320     c                   exsr      GestAR4L
106790070320    1C                   END
106800071116     c*
106810071116     C* M O D I F I C A T O   S O L O   1 °   T I P O   B O L L A
106820071116    1C     SAVTB2        IFEQ      '  '
106830071116     C     §3ATB2        ANDEQ     '  '
106840071116     C* PRENDI IL TIPO BOLLA DEL NUOVO CODICE
106850071116     C                   MOVEL     'TB'          COD
106860071116     C                   MOVEL(P)  §3atb1        KEY
106870071116     C                   EXSR      CTABEL
106880071116     C  N30              MOVEL     TBLUNI        DSTB
106890071116     C   30              CLEAR                   DSTB
106900071116     c                   movel     §tbfcb        NEWtb1FCB         1
106910071116     c
106920071116     C* Se il nuovo tipo bolla è da non contabilizzare, imposto
106930071116     c*  la "S" di abilitazione Assgnato
106940071116    2c                   if        ARBACA=' ' and NEWtb1FCB='0'
106950071116     c                   eval      arbaca='S'
106960071116    2c                   endif
106970071116     c
106980071116     c* Se il nuovo tipo bolla è da contabilizzare, ma non lo era il
106990071116     c*  vecchio, vedo se devo togliere la "S" per l'abilitazione del C/A
107000071116    2c                   if        NEWtb1FCB='1' and Newtb1FCB<>OLDtb1FCB
107010071116     c                             and arbaca='S'
107020071116     C* TASSAZIONE IN FIAR6
107030071116     C                   MOVEL     '1'           WTRC
107040071116     C     KARB2         CHAIN(N)  FIAR6000
107050071116    3c                   if        (%subst(%editc(arbksc: 'X'):4:4)='9999') and
107060071116     c                             (not %found(FIar601l) or ar6dft=0)
107070071116     c                   eval      arbaca=' '
107080071116    3c                   endif
107090071116    2c                   endif
107100071116     c
107110071116    1C                   ENDIF
107120920116     C*
107130941216     C     §3AFCA        IFNE      0
107140950104     C     AR9CAS        ANDGT     0
107150941212     C     WAR9          IFEQ      ' '
107160941212     C                   MOVEL     ARBAAS        AR9AAS
107170941212     C                   MOVEL     ARBLNP        AR9LNP
107180941212     C                   MOVEL     ARBNRS        AR9NRS
107190941212     C                   MOVEL     ARBNSP        AR9NSP
107200051118     c                   movel     dateu         ar9duv
107210051118     C                   WRITE     FiAR9000
107220941212     C                   ELSE
107230051118     C                   UPDATE    FiAR9000
107240941212     C                   ENDIF
107250941216     C*
107260941216     C                   ELSE
107270941216     C* SE MI HANNO CANCELLATO L'AR9 --> DELETO
107280941216     C     WAR9          IFEQ      'E'
107290051118     C     KARB          DELETE    FiAR9000                           30
107300941216     C                   ENDIF
107310941216     C                   ENDIF
107320941212     C*
107330060213     C   10              UPDATE    FNARB000
107340041022     c* Se cambiato codice bolla aggiorno fnblp
107350041022     c                   if        waltrabollA=' ' AND oldcbo<>newcbo
107360041022     c                   eval      arbcbo=newcbo
107370041022     c   10              except    arbkblp
107380041022     c  N10              except    arbkarb
107390041022     c                   endif
107400040224
107410040224      * Se c'è la data consegna richiesta controllo se è ancora valida
107420040224      * solo se ho fatto una modifica in arrivo
107430160114     c                   If        ArbDcr > *Zeros and *In10 and wfile='A'
107440040324     c                             and wCa = *Blanks and wgiac = *Blanks
107450040224     c                   ExSr      ContrDcr
107460040224     c                   EndIf
107470950602     C*
107480130411     c* Richiamo routine per invio variazione a pda
107490130411     c                   exsr      sr_pda
107500950104     C*
107510920117     C     ENDRG5        ENDSR
107520911211     C*
107530911211     C*--- CARICO CODICI TARIFFA -------------------------------------*
107540911211     C     CARCTR        BEGSR
107550020305     c**
107560020305     C* Richiamo trul27r per reperimento WFIE e per reperimento tariffa di
107570020305     C* cartello
107580020305     c**
107590020305     c                   clear                   trul27ds
107600021121     c                   Eval      I27Pkg = ArbPkf
107610020305     c                   eval      i27tsp = arbtsp
107620020305     c                   eval      i27lna = arblna
107630020305     c                   eval      i27lnp = arblnp
107640020305     C                   eval      i27cli = ksc
107650020305     c                   call      'TRUL27R'
107660020305     c                   parm                    trul27ds
107670020305     c                   eval      wfie = o27fie
107680020305     c*
107690941209     C                   MOVEL     *BLANKS       DE2CTR
107700050202     C                   CLEAR                   Fnlv59DS
107710941209     C*
107720050202     C                   MOVEL     D48TBO        ilv59tbo
107730040511      * SE BOLLA MONOVARIA NON PASSO IL TIPO BOLLA
107740040511     C                   IF        §3ASVA <> *BLANK
107750050202     C                   CLEAR                   ilv59tbo
107760050202     c                   eval      ilv59prp='P'
107770040511     C                   ENDIF
107780050202     C                   Z-ADD     ARBDSP        ilv59DSP
107790060131     C                   MOVEL     dutkci        ilv59KCI
107800050202     C                   Z-ADD     KSC           ilv59KSC
107810050202     C                   MOVEL     WFIE          ilv59FIE
107820050202     C                   MOVEL     ARBTSP        ilv59TSP
107830050202     C                   MOVEL     VIDCTR        ilv59CTR
107840941209     C*
107850941209     C* SE NON C'E' RICERCA SU COD.TARIFFA E CAMBIATO CODICE CLIENTE
107860941209     C*  --> RICERCO LA TARIFFA
107870941209     C                   SETOFF                                       32
107880941209     C     '?'           SCAN      VIDCTR                                 32
107890941215     C     *IN32         IFEQ      *OFF
107900941215     C     KSC           ANDNE     SAVKSC
107910050202     C     VIDCTR        ANDEQ     SAVCTR
107920050202     C                   CLEAR                   ilv59CTR
107930941223     C                   CLEAR                   OTT
107940050202     C                   MOVEL     'S'           ilv59RIC
107950941215     C                   ELSE
107960060516     c* Se cod cliente e cod tariffa invariati rispetto alla bolla,
107970060516     c*  accetto codici tariffa bloccati (ma quando immessi validi)
107980060516     c                   if        not *in32 and ksc=kscbolla
107990060511     c                             and ctrbolla=vidctr
108000060516     C                   MOVEL     'B'           ilv59ta2
108010941209     C                   ENDIF
108020060511     C                   ENDIF
108030060516     C                   MOVEL     'S'           ilv59CON
108040050202     C
108050050202     C                   MOVEL     KSC           SAVKSC
108060941209     C*
108070050202     C                   CALL      'FNLV59R'
108080050202     C                   PARM                    Fnlv59DS
108090050202     c
108100941209     C*
108110941209     C* NON ESISTONO TARIFFE
108120050202     C                   MOVEL     olv59TKS      WTKS              1
108130050202     C     olv59TKS      IFNE      'S'
108140941209     C                   SETON                                        18
108150911219     C                   SETOFF                                       32
108160950114     C                   MOVEL     CNOTAR        DE2CTR
108170941209     C                   ELSE
108180941209     C*
108190941215     C* SE TARIFFE TROVATE,  DEVO REIMPOSTARE LA TARIFFA A VIDEO
108200050202     C     ilv59RIC      IFEQ      'S'
108210050202     C                   MOVEL     olv59CTR      VIDCTR
108220941209     C                   ENDIF
108230941215     C* DECODIFICA
108240050202     C                   MOVEL     olv59DFS      DE2CTR
108250941209     C                   ENDIF
108260050202     c
108270050202     C                   EVAL      SAVCTR=VIDCTR
108280911211     C*
108290911211     C                   ENDSR
108300911211     C*
108310920320     C*--- CONTROLLI CAMPI CONSEGNA RICHIESTA ------------------------*
108320920320     C     CONTR7        BEGSR
108330920320     C                   SETOFF                                       90
108340941102     c**
108350920320     C* CONSEGNA RICHIESTA
108360941102     C**
108370920320     C                   Z-ADD     0             COMDCR
108380920320     C     VIDDCR        IFNE      0
108390941102     C                   Z-ADD     VIDDCR        G02DAT
108400920320     C                   MOVEL     *BLANK        G02ERR
108410941102     C                   CALL      'XSRDA8'
108420920320     C                   PARM                    WLBDAT
108430941102     C* DATA FORMALMENTE ERRATA
108440920320     C     G02ERR        IFEQ      '1'
108450941102     C                   MOVEL     MSG(6)        VIDMSG
108460941102     C                   SETON                                        409028
108470920320     C                   GOTO      ENDCT7
108480920320     C                   END
108490941102     C*
108500941102     C                   Z-ADD     G02DAT        VIDDCR
108510941102     C*
108520920320     C* DATA CONSEGNA NO < DATA SPEDIZIONE
108530930507     C     G02INV        IFLT      ARBDSP
108540941102     C                   MOVEL     MSG(7)        VIDMSG
108550941102     C                   SETON                                        409028
108560920320     C                   GOTO      ENDCT7
108570930507     C                   END
108580941102     C                   Z-ADD     G02INV        COMDCR            8 0
108590941102     C                   ENDIF
108600070308     c
108610070308      * La data non può superare i gg limiti indicati in tab. VPO
108620070308     c                   If        Not *In90
108630070308     c                   Eval      wdataiso = wdataoggi
108640070308     c                   Adddur    §vpodcr:*d    wdataiso
108650070308     c     *iso          move      wdataiso      ds_data
108660070308     c                   Move      §vpodcr       w003a
108670070308     c                   Movea     w003a         ski(13)
108680070308     c                   Z-add     13            i
108690070308     c                   ExSr      abbl
108700070308     c                   Movea     ski(13)       w003a
108710070308     c                   If        ComDcr > ds_data
108720070308     c                   Eval      *In28 = *On
108730070308     c                   Eval      *In40 = *On
108740070308     c                   Eval      *In90 = *On
108750070308     c                   Eval      VidMsg = msg(129)
108760070308     c                   Eval      VidMsg = %replace(w003a:VidMsg:
108770070308     c                                      %scan('___':VidMsg))
108780070308     C                   GOTO      ENDCT7
108790070308     c                   EndIf
108800070308     c                   EndIf
108810141029     c* Se dcr>0 errore forzabile se presente Fermo Deposito
108820141029     c                   if        d48ffr<>'E'
108830141029     c                   if        comdcr>0 and comdcr<>arbdcr and
108840141029     c                             arbffd<>*blanks and wforzdcr=0
108850141029     C                   MOVEL     MSG(154)      VIDMSG
108860141029     C                   SETON                                        409028
108870141029     C                   EVAL      Wforzdcr=1
108880141029     C                   GOTO      ENDCT7
108890141029     c                   endif
108900141029     c                   endif
108910941102     C**
108920941102     C** ORA CONSEGNA RICHIESTA
108930941102     C**
108940920320     C                   MOVEL     VIDHCR        HM                2 0
108950920320     C     HM            IFLT      0
108960920320     C     HM            ORGT      24
108970941102     C                   MOVEL     MSG(8)        VIDMSG
108980941102     C                   SETON                                        419028
108990920320     C                   GOTO      ENDCT7
109000920320     C                   END
109010941102     C**
109020941102     C** MINUTI CONSEGNA RICHISTA
109030941102     C**
109040920320     C                   MOVE      VIDHCR        HM                2 0
109050920320     C     HM            IFLT      0
109060920320     C     HM            ORGT      59
109070941102     C                   MOVEL     MSG(8)        VIDMSG
109080941102     C                   SETON                                        419028
109090920320     C                   GOTO      ENDCT7
109100920320     C                   END
109110930507     C*
109120941102     C* SE IMMESSO IL TIPO IMMETTERE ANCHE L'ORA O LA DATA
109130930507     C     VIDTCR        IFNE      ' '
109140941102     C     VIDDCR        IFEQ      0
109150941102     C     VIDHCR        ANDEQ     0
109160941102     C                   MOVEL     MSG(9)        VIDMSG
109170941102     C                   SETON                                        409028
109180930507     C                   GOTO      ENDCT7
109190930507     C                   END
109200930507     C                   END
109210070308     c
109220070308     c* Se lna  attivata con GEO, non posso inserire solo tipo e ora:
109230070308     c*  data sempre obbligatoria (se inserito in arrivo, accetto
109240070308     c*  dalla partenza)
109250100419     c* Escludo per le causali di ripristino
109260070319     c                   if        FGSgeot='S' and viddcr=0 and vidhcr>0
109270100419     c                             and d48cvb <> 'CA'
109280070308     c                   if        vidtcr<>arbtcr or vidhcr<>arbhcr or
109290070308     c                             comdcr<>arbdcr
109300070308     C                   MOVEL     MSG(140)      VIDMSG
109310070308     C                   SETON                                        409028
109320070308     C                   GOTO      ENDCT7
109330070308     C                   ENDif
109340070308     C                   ENDif
109350930507     C**
109360941102     C* GIORNI DI CHIUSURA
109370941102     C**
109380930507     C     VIDGC1        IFNE      *BLANKS
109390930507     C                   MOVEL     VIDGC1        ACOM              1
109400930507     C                   MOVE      VIDGC1        BCOM              1
109410941212     C                   Z-ADD     1             B                 2 0
109420930507     C     ACOM          LOOKUP    GGG(B)                                 32
109430941102     C     *IN32         IFEQ      *OFF
109440941102     C                   MOVEL     MSG(10)       VIDMSG
109450941102     C                   SETON                                        429028
109460941102     C                   GOTO      ENDCT7
109470941102     C                   ENDIF
109480930507     C*
109490930507     C     BCOM          IFNE      ' '
109500930507     C     BCOM          ANDNE     'P'
109510930507     C     BCOM          ANDNE     'M'
109520941102     C                   MOVEL     MSG(10)       VIDMSG
109530941102     C                   SETON                                        429028
109540930507     C                   GOTO      ENDCT7
109550930507     C                   END
109560041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
109570041130if  2c                   If         *In10 and VidGc1 <> ArbGc1 and
109580041130     c                             (VidGc1 = ' M' or VidGc1 = ' P')
109590041130     c                   Eval      *In28 = *On
109600041130     c                   Eval      *In42 = *On
109610041130     c                   Eval      *In90 = *On
109620041130     c                   Eval      VidMsg = msg(10)
109630041130e   2c                   EndIf
109640930507     C                   END
109650930507     C*
109660930507     C*** 2° GIORNO CHIUSURA ***
109670930507     C     VIDGC2        IFNE      *BLANKS
109680930507     C                   MOVEL     VIDGC2        ACOM              1
109690930507     C                   MOVE      VIDGC2        BCOM              1
109700930507     C                   Z-ADD     1             B
109710930507     C     ACOM          LOOKUP    GGG(B)                                 30
109720941102     C     *IN30         IFEQ      *OFF
109730941102     C                   MOVEL     MSG(10)       VIDMSG
109740941102     C                   SETON                                        439028
109750941102     C                   GOTO      ENDCT7
109760941102     C                   ENDIF
109770930507     C*
109780930507     C     BCOM          IFNE      ' '
109790930507     C     BCOM          ANDNE     'P'
109800930507     C     BCOM          ANDNE     'M'
109810941102     C                   MOVEL     MSG(11)       VIDMSG
109820941102     C                   SETON                                        439028
109830930507     C                   GOTO      ENDCT7
109840930507     C                   END
109850930507     C*
109860930507     C* I GIORNI DI CHIUSURA NON POSSO ESSERE UGUALI
109870930507     C     VIDGC1        IFEQ      VIDGC2
109880941102     C                   MOVEL     MSG(12)       VIDMSG
109890941102     C                   SETON                                        439028
109900930507     C                   GOTO      ENDCT7
109910930507     C                   END
109920930507     C* NON POSSONO ESSERE ' P' ' M'
109930930507     C     VIDGC1        IFEQ      ' M'
109940930507     C     VIDGC2        ANDEQ     ' P'
109950930507     C     VIDGC1        OREQ      ' P'
109960930507     C     VIDGC2        ANDEQ     ' M'
109970941102     C                   MOVEL     MSG(13)       VIDMSG
109980941102     C                   SETON                                        429028
109990930507     C                   GOTO      ENDCT7
110000930507     C                   END
110010041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
110020041130if  2c                   If         *In10 and VidGc2 <> ArbGc2 and
110030041130     c                             (VidGc2 = ' M' or VidGc2 = ' P')
110040041130     c                   Eval      *In28 = *On
110050041130     c                   Eval      *In43 = *On
110060041130     c                   Eval      *In90 = *On
110070041130     c                   Eval      VidMsg = msg(10)
110080041130e   2c                   EndIf
110090930507     C                   END
110100941102     C*
110110941102     C* CONSEGNE PARTICOLARi
110120020603     c* Verifico se destinatario disagiato
110130020604     c* per le bolle poste non lo faccio
110140020604     c                   clear                   wdisag
110150020604     c     lnpntw        ifne      'PPT'
110160020603     c                   clear                   tisit0ds
110170020603     c                   movel     'E'           it0tla
110180020603     c                   movel     arbnzd        it0naz
110190020603     c                   movel     arbprd        it0prv
110200020603     c                   movel     arbcad        it0cap
110210020603     c                   movel     arblod        it0loc
110220020603     c                   movel     arbind        it0ind
110230020603     c                   movel     arbrsd        it0rag
110240020603     c                   call      'TISIT5R'
110250020603     c                   parm                    tisit0ds
110260020603     c     ot0err        ifeq      *blanks
110270060203   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
110280060203     c                             ot0dos = 'S'
110290020603     c                   movel     ot0dos        wdisag
110300020603     c     wdisag        ifeq      'D'
110310020603     c                   movel     'X'           wdisag
110320020603     c                   endif
110330060203     c                   endif
110340020603     c                   else
110350020603     c                   clear                   wdisag
110360020603     c                   endif
110370020604     c                   endif
110380020603     c
110390941102     C*
110400941102     C* 11ON - NON POSO CAMBIARE LA 1 CONSEGNA PARTICOLARE
110410070910     c* Se ho modificato il destinatario è non è stata immessa in
110420070910     c*  partenza, devo per forza toglierla
110430070910     c                   if        *in11=*on and wdisag<>vidtc1 and
110440070910     c                             arbtc1=vidtc1 and par5mdp='S' and
110450070910     c                             par5gtc1<>vidtc1 and par5gtc2<>vidtc1
110460070910     C                   MOVEL     MSG(144)      VIDMSG
110470070910     c                   eval      %subst(vidmsg:27:1) = vidtc1
110480070910     c                   eval      %subst(vidmsg:29:8) = v7dtc1
110490070910     C                   SETON                                        449028
110500070910     C                   GOTO      ENDCT7
110510070910     c                   endif
110520070910     c
110530051116     c****               if        *in11 = *on and wdisag <> *blanks
110540070910     c* se consegna particolare non modificabile ma è stato immesso
110550070910     c* unacons.part.disagiato/supemercat... do' lo stesso la
110560070910     c* possibilita di cambiarla se la combinazione fra le due non è
110570070910     c* ammessa
110580051116     c                   clear                   wcomb
110590070910    0c                   if        *in11=*on and par5mdp='S' and
110600070910     c                             (arbtc1=par5gtc1 or arbtc1=par5gtc2)
110610070910     c
110620070910    1c                   if        vidtc2<>*blanks or wdisag<>*blanks
110630070910     c                             or vidtc1<>arbtc1
110640070910     c*
110650070910     c***                if        *in11 = *on and (vidtc2 <> *blanks or
110660070910     c***                          wdisag <> *blanks or vidtc1 <> arbtc1)
110670051114     c                   move      arbtc1        wtc1
110680051117     c
110690051116     c                   if        wdisag <> *blanks
110700051116     c                   move      wdisag        wtc2
110710051117     c                   exsr      sr_chkcomb
110720051117     c                   endif
110730051117     c                   if        wcomb = *blanks and vidtc1 <> arbtc1
110740051117     c                   move      vidtc1        wtc2
110750051117     c                   exsr      sr_chkcomb
110760051117     c                   endif
110770051117     c                   if        wcomb = *blanks and vidtc2 <> *blanks
110780051117     c                   move      vidtc2        wtc2
110790051117     c                   exsr      sr_chkcomb
110800051117     c                   endif
110810070910    1c                   endif
110820070910     c***
110830070910    0C***  *IN11         IFEQ      *ON
110840070910     c***  wdisag        andeq     *blanks
110850070910    0c***                if        *in11 = *on and wcomb = *blanks
110860070910     c
110870070910    1c                   if        wcomb=*blanks
110880941102     C*
110890070910    2C     VIDTC1        IFNE      ARBTC1
110900941102     C                   MOVEL     MSG(15)       VIDMSG
110910051117     c                   eval      %subst(vidmsg:36:1) = arbtc1
110920941102     C                   SETON                                        449028
110930941102     C                   GOTO      ENDCT7
110940070910    2c                   endif
110950070910    1c                   endif
110960070910    0c                   endif
110970070910     c
110980070910     c***                else
110990070910     C***                MOVEL     '1P'          COD
111000070910     C***                MOVEL(P)  VIDTC1        KEY
111010070910     C***                EXSR      CTABEL
111020070910     C***                MOVEL     TBLUNI        V7DTC1
111030070910    1C***                ENDIF
111040941102     C*
111050070910   X0C***                ELSE
111060941102     C*
111070941102     C                   CLEAR                   V7DTC1
111080941102     C*
111090941102    1C     VIDTC1        IFNE      ' '
111100941102     C* RICERCA
111110941102    2C     VIDTC1        IFEQ      '?'
111120941102     C                   MOVEL     CODUT         §KUT
111130941102     C                   MOVEL     '1P'          §COD
111140941102     C                   MOVE      *BLANKS       VIDTC1
111150991011     C                   CLEAR                   §KEY
111160941102     C                   CALL      'X§TABER'
111170941102     C                   PARM                    §KUT
111180941102     C                   PARM                    §COD
111190941102     C                   PARM                    §KEY
111200941209     C                   PARM                    §DES
111210941102     C                   MOVEL     §KEY          VIDTC1
111220941102     C                   MOVEL     §DES          V7DTC1
111230941102     C                   SETON                                        9044
111240941102     C                   GOTO      ENDCT7
111250941102    2C                   ENDIF
111260941102     C*
111270941102     C                   MOVEL     '1P'          COD
111280941102     C                   MOVEL(P)  VIDTC1        KEY
111290941212     C                   EXSR      CTABEL
111300941102     C*
111310941102    2C     *IN30         IFEQ      *ON
111320941102     C                   MOVEL     MSG(11)       VIDMSG
111330941102     C                   SETON                                        449028
111340941102     C                   GOTO      ENDCT7
111350941102    2C                   ENDIF
111360941102     C*
111370941102     C                   MOVEL     TBLUNI        DS1P
111380941102     C*
111390941102     C* NON UTILIZZABILE IN ARRIVO
111400950104    2C   10§1PUAR        IFNE      'S'
111410020318     C     LNPNTW        ANDNE     'PPT'
111420020318     C     LNPNTW        OREQ      'PPT'
111430000510     C     §1PFPC        ANDNE     'E'
111440000510     C     §1PFPC        ANDNE     'A'
111450040705      * e non è quella presa in automatico
111460051114     c                   If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
111470070914     c* Verifico se era stata messa in partenza ma tolta
111480070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
111490070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
111500941102     C                   MOVEL     MSG(11)       VIDMSG
111510941102     C                   SETON                                        449028
111520941102     C                   GOTO      ENDCT7
111530040705     c                   EndIf
111540070914     c                   EndIf
111550941102    2C                   END
111560060131     c
111570060131     c* SE devo considerare le abilitati, verifico se il profilo
111580060131     c*  può inserire la cons particolare, se non è presa in automatico
111590060131     c   10              If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
111600070914     c                             and  §utemfcp=' '
111610070914     c******                       and dateu>=§vpoacp and  §utemfcp=' '
111620070914     c*
111630070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
111640070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
111650060131     C*
111660060131     C                   MOVEL     'SP'          COD
111670060131     C                   MOVEL(P)  VIDTC1        KEY
111680060131     C                   EXSR      CTABEL
111690060131     C*
111700060131    2C     *IN30         IFEQ      *ON
111710060131     c                   clear                   dssp
111720060131     c                   else
111730060131     c                   movel     tbluni        DSSP
111740060131     c                   endif
111750060131     c
111760060131     c                   if        §spaba='S'
111770060131     C                   MOVEL     MSG(138)      VIDMSG
111780060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
111790060131     C                   SETON                                        449028
111800060131     C                   GOTO      ENDCT7
111810060131     c                   endif
111820060131     c                   endif
111830070914     c
111840070914     c                   endif
111850060131     c
111860950104     C* NON UTILIZZABILE IN PARTENZA
111870950104    2C  N10§1PUPR        IFNE      'S'
111880020318     C     LNPNTW        ANDNE     'PPT'
111890020318     C     LNPNTW        OREQ      'PPT'
111900000510     C     §1PFPC        ANDNE     'E'
111910000510     C     §1PFPC        ANDNE     'P'
111920950104     C                   MOVEL     MSG(11)       VIDMSG
111930950104     C                   SETON                                        449028
111940950104     C                   GOTO      ENDCT7
111950950104    2C                   END
111960000510     C* PER LE POSTE CONTROLLO IL SUO FLAG
111970941102     C*
111980941102     C                   MOVEL     TBLUNI        V7DTC1
111990150414     c* controlli per EXPO
112000151119     c*                  if        vidtc1='Z'
112010151119     c*                  exsr      sr_chkexpo
112020151119     c*                  if        *in90
112030151119     c*                  seton                                        44
112040151119     c*                  goto      endct7
112050151119     c*                  endif
112060151119     c*                  endif
112070941102     C*
112080941102     C* LA 1 E LA 2 NON POSSONO ESSERE UGUALI
112090941102    2C     VIDTC2        IFEQ      VIDTC1
112100941102     C                   MOVEL     MSG(14)       VIDMSG
112110941102     C                   SETON                                        459028
112120941102     C                   GOTO      ENDCT7
112130941102    2C                   END
112140020731     C*
112150941102     C*
112160941102    1C                   END
112170070910    0C******             END
112180941102     C*
112190941102     C* 12ON - NON POSO CAMBIARE LA 2 CONSEGNA PARTICOLARE
112200070910     c* Se ho modificato il destinatario è non è stata immessa in
112210070910     c*  partenza, devo per forza toglierla
112220070910    0c                   if        *in12=*on and wdisag<>vidtc2 and
112230070910     c                             arbtc2=vidtc2 and par5mdp='S' and
112240070910     c                             par5gtc1<>vidtc2 and par5gtc2<>vidtc2
112250070910     C                   MOVEL     MSG(144)      VIDMSG
112260070910     c                   eval      %subst(vidmsg:27:1) = vidtc2
112270070910     c                   eval      %subst(vidmsg:29:8) = v7dtc2
112280070910     C                   SETON                                        459028
112290070910     C                   GOTO      ENDCT7
112300070910    0c                   endif
112310070910     c
112320051114     c* se consegna particolare non modificabile ma è stato immesso
112330051114     c* un destinatrio disagiato/supemercat... do' lo stesso la
112340051114     c* possibilita di cambiarla se la combinazione fra le due non è
112350051114     c* ammessa
112360051116     c                   clear                   wcomb
112370070910    0c                   if        *in12=*on and par5mdp='S' and
112380070910     c                             (arbtc2=par5gtc1 or arbtc2=par5gtc2)
112390070910
112400070910    1c                   if        vidtc1 <> *blanks or
112410070910     c                             wdisag <> *blanks or vidtc2 <> arbtc2
112420070910     c
112430051116     c***                if        *in12 = *on and wdisag <> *blanks
112440070910     c***
112450070910     c***                if        *in12 = *on and (vidtc1 <> *blanks or
112460070910     c***                          wdisag <> *blanks or vidtc2 <> arbtc2)
112470051114     c                   move      arbtc2        wtc1
112480051116     c                   if        wdisag <> *blanks
112490051116     c                   move      wdisag        wtc2
112500051117     c                   exsr      sr_chkcomb
112510051117     c                   endif
112520051117     c                   if        wcomb = *blanks and vidtc2 <> arbtc2
112530051117     c                   move      vidtc2        wtc2
112540051117     c                   exsr      sr_chkcomb
112550051117     c                   endif
112560051117     c                   if        wcomb = *blanks and vidtc1 <> *blanks
112570051117     c                   move      vidtc1        wtc2
112580051117     c                   exsr      sr_chkcomb
112590051117     c                   endif
112600070910    1c                   endif
112610051117     c*
112620070910    0C***  *IN12         IFEQ      *ON
112630070910    0c***                if        *in12 = *on and wcomb = *blanks
112640070910    1c                   if        wcomb = *blanks
112650941102     C*
112660070910    2C     VIDTC2        IFNE      ARBTC2
112670941102     C                   MOVEL     MSG(15)       VIDMSG
112680051117     c                   eval      %subst(vidmsg:36:1) = arbtc2
112690941102     C                   SETON                                        459028
112700941102     C                   GOTO      ENDCT7
112710070910    2c                   endif
112720070910    1c                   endif
112730070910    0c                   endif
112740070910     c
112750070910     c****               else
112760070910     C****               MOVEL     '1P'          COD
112770070910     C****               MOVEL(P)  VIDTC2        KEY
112780070910     C****               EXSR      CTABEL
112790070910     C****               MOVEL     TBLUNI        V7DTC2
112800070910    1C****               ENDIF
112810941102     C*
112820070910   X0C****               ELSE
112830941102     C*
112840941102     C                   CLEAR                   V7DTC2
112850941102     C*
112860941102    1C     VIDTC2        IFNE      ' '
112870941102     C* RICERCA
112880941102    2C     VIDTC2        IFEQ      '?'
112890941102     C                   MOVEL     CODUT         §KUT
112900941102     C                   MOVEL     '1P'          §COD
112910941102     C                   MOVE      *BLANKS       VIDTC2
112920991011     C                   CLEAR                   §KEY
112930941102     C                   CALL      'X§TABER'
112940941102     C                   PARM                    §KUT
112950941102     C                   PARM                    §COD
112960941102     C                   PARM                    §KEY
112970941209     C                   PARM                    §DES
112980941102     C                   MOVEL     §KEY          VIDTC2
112990941102     C                   MOVEL     §DES          V7DTC2
113000941102     C                   SETON                                        9045
113010941102     C                   GOTO      ENDCT7
113020941102    2C                   ENDIF
113030941102     C*
113040941102     C                   MOVEL     '1P'          COD
113050941102     C                   MOVEL(P)  VIDTC2        KEY
113060941212     C                   EXSR      CTABEL
113070941102     C*
113080941102    2C     *IN30         IFEQ      *ON
113090941102     C                   MOVEL     MSG(11)       VIDMSG
113100941102     C                   SETON                                        459028
113110941102     C                   GOTO      ENDCT7
113120941102    2C                   ENDIF
113130941102     C*
113140941102     C                   MOVEL     TBLUNI        DS1P
113150941102     C*
113160941102     C* NON UTILIZZABILE IN ARRIVO
113170950104    2C   10§1PUAR        IFNE      'S'
113180020318     C     LNPNTW        ANDNE     'PPT'
113190020318     C     LNPNTW        OREQ      'PPT'
113200000518     C     §1PFPC        ANDNE     'E'
113210000518     C     §1PFPC        ANDNE     'A'
113220040705      * e non è quella presa in automatico
113230051114     c                   If        Vidtc2 <> wdisag  and vidtc2 <> arbtc2
113240070914     c* Verifico se era stata messa in partenza ma tolta
113250070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113260070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
113270941102     C                   MOVEL     MSG(11)       VIDMSG
113280941102     C                   SETON                                        459028
113290941102     C                   GOTO      ENDCT7
113300040705     c                   EndIf
113310070914     c                   EndIf
113320941102    2C                   END
113330060131     c
113340060131     c* SE devo considerare le abilitati, verifico se il profilo
113350060131     c*  può inserire la cons particolare, se non è presa in automatico
113360060131     c   10              If        Vidtc2 <> wdisag and vidtc2 <> arbtc2
113370070914     c                             and  §utemfcp=' '
113380070514     c****                         and dateu>=§vpoacp and  §utemfcp=' '
113390070914     c* Verifico se era stata messa in partenza ma tolta
113400070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113410070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
113420060131     C*
113430060131     C                   MOVEL     'SP'          COD
113440060131     C                   MOVEL(P)  VIDTC2        KEY
113450060131     C                   EXSR      CTABEL
113460060131     C*
113470060131    2C     *IN30         IFEQ      *ON
113480060131     c                   clear                   dssp
113490060131     c                   else
113500060131     c                   movel     tbluni        DSSP
113510060131     c                   endif
113520060131     c
113530060131     c                   if        §spaba='S'
113540060131     C                   MOVEL     MSG(138)      VIDMSG
113550060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
113560060131     C                   SETON                                        459028
113570060131     C                   GOTO      ENDCT7
113580060131     c                   endif
113590060131     c                   endif
113600070914     c                   endif
113610070914     c
113620950104     C* NON UTILIZZABILE IN PARTENZA
113630950104    2C  N10§1PUPR        IFNE      'S'
113640020318     C     LNPNTW        ANDNE     'PPT'
113650020318     C     LNPNTW        OREQ      'PPT'
113660000518     C     §1PFPC        ANDNE     'E'
113670000518     C     §1PFPC        ANDNE     'P'
113680950104     C                   MOVEL     MSG(11)       VIDMSG
113690950104     C                   SETON                                        459028
113700950104     C                   GOTO      ENDCT7
113710950104    2C                   END
113720941102     C*
113730941102     C                   MOVEL     TBLUNI        V7DTC2
113740150414     c* controlli per EXPO
113750151119     c*                  if        vidtc2='Z'
113760151119     c*                  exsr      sr_chkexpo
113770151119     c*                  if        *in90
113780151119     c*                  seton                                        45
113790151119     c*                  goto      endct7
113800151119     c*                  endif
113810151119     c*                  endif
113820941102     C*
113830941102    1C                   END
113840070910    0C****               END
113850060131     c
113860020531     c* Se destinatario disagiato o supermercato errore se non indicato
113870020531     c* in nessuno dei due campi di consegna particolare
113880041015     c*
113890041015     c* Se particolarità varia="U" --> consegna ad uffici devo invece
113900041015     c* dare errore se immesso supermercato o appuntamento
113910041015     c                   if        %subst(arbgva:1:1)='U'
113920041015     c                             and (vidtc1='S' or vidtc1='A' or vidtc2='S'
113930041015     c                             or vidtc2='A')
113940041015     C                   MOVEl     msg(131)      vidmsg
113950041015     C                   SETON                                        449028
113960041015     C                   GOTO      ENDCT7
113970041015     c                   endif
113980150210
113990150210     c* se sono in partenza e immessa la "A" e la data --> errore solo la data immettere
114000150210     c                   if        not *in10 and (vidtc1='A' or vidtc2='A') and
114010150210     c                             comdcr>0
114020150210     c                   if        (par5mdp='S' and par5gtc1<>'A' and par5gtc2
114030150210     c                                                    <> 'A')
114040150210     c                             or (par5mdp=' ' and arbtc1<>'A' and arbtc2
114050150210     c                                                    <> 'A')
114060150210     C                   MOVEl     msg(158)      vidmsg
114070150210     C                   SETON                                        449028
114080150210     C                   GOTO      ENDCT7
114090150210     c                   endif
114100150210     c                   endif
114110150119
114120150119     c* se destinatario particolare che prevede appuntamento controllo se presente
114130150119     c* DCR immessa dalla partenza perchè in questo caso non devo dare errore per la
114140150119     c* mancanza della "A" di appuntamento
114150150119     c                   eval      w_noapp=' '
114160150119     c                   if        wdisag='A'
114170150119     c                   if        (par5mdp='S' and par5gdcr>*zeros
114180150119     c                                          and par5gtcr=' ')
114190150119     c                             or (par5mdp=' ' and arbdcr>0 and arbtcr= ' ')
114200150119     c                   eval      w_noapp='1'
114210150119     c                   endif
114220150119     c                   endif
114230041015     c*
114240041015     C***  wdisag        ifne      *blanks
114250041015     c***  wdisag        andne     vidtc1
114260041015     c***  wdisag        andne     vidtc2
114270150119     c                   if        wdisag <> *blanks and w_noapp=' ' and
114280041015     c                             %subst(arbGva:1:1) <> 'U' or
114290041015     c                             wdisag='X' and %subst(arbGva:1:1)='U'
114300041015     c     wdisag        ifne      vidtc1
114310041015     c     wdisag        andne     vidtc2
114320020603     C                   MOVEa     msg(112)      k78
114330020531     c                   select
114340020531     c     wdisag        wheneq    'X'
114350020531     c                   eval      w014a = 'disagiato     '
114360020531     c     wdisag        wheneq    'S'
114370020531     c                   eval      w014a = '"supermercato"'
114380040705     c     wdisag        wheneq    'A'
114390040705     c                   eval      w014a = '"appuntamento"'
114400020531     c                   endsl
114410020531     C                   MOVEa     w014a         k78(14)
114420020531     C                   MOVEA     K78           VIDMSG
114430020603     C                   SETON                                        449028
114440020531     C                   GOTO      ENDCT7
114450020531     c                   endif
114460041015     c                   endif
114470070907     C* Non è possibile indicare contemporaneamente alcune consegne    i
114480070907     C*  particolari
114490051114     c                   move      vidtc1        wtc1
114500051114     c                   move      vidtc2        wtc2
114510051114     c                   exsr      sr_chkcomb
114520051114     c                   if        wcomb = 'S'
114530051114     C                   movel     MSG(113)      VIDmsg
114540051116     c                   eval      %subst(vidmsg:63:1) = vidtc1
114550051116     c                   eval      %subst(vidmsg:69:1) = vidtc2
114560051114     C                   seton                                        459028
114570051114     C                   goto      ENDCT7
114580051114    2C                   ENDIF
114590941102     C*
114600920320     C     ENDCT7        ENDSR
114610021129      **
114620021129      *--- CONTROLLI CAMPI CONSEGNA RICHIESTA SUPERMERCATI -----------*
114630021129     C     CONTR71       BEGSR
114640021129     C                   SETOFF                                       90
114650021129
114660030123      * Se la spedizione non ha già la consegna particolare 'S'
114670030123     c                   If        ArbTc1 <> 'S' and ArbTc2 <> 'S'
114680030123      * E' obbligatorio inserire la consegna
114690030123     c                   If        (VidTc1 = *Blanks and VidTc2 = *Blanks)
114700030123     c                   Eval      VidMsg = Msg(117)
114710030123     c                   Eval      *In44 = *On
114720030123     c                   Eval      *In28 = *On
114730030123     c                   Eval      *In90 = *On
114740030123     c                   GoTo      EndCt71
114750030123     c                   EndIf
114760030127      * Se ci sono già tutte e 2 almeno una deve essere 'S'
114770030127     c                   If        VidTc1 <> 'S' and VidTc2 <> 'S'
114780030127     c                   Eval      VidMsg = Msg(122)
114790030127     c                   Eval      *In44 = *On
114800030127     c                   Eval      *In28 = *On
114810030127     c                   Eval      *In90 = *On
114820030127     c                   GoTo      EndCt71
114830030127     c                   EndIf
114840030123      * Se la prima consegna c'è già e non è supermercato devo mettere 'S' nella seconda
114850030127     c                   If        ArbTc1 <> *Blanks and VidTc1 <> 'S' and
114860030127     c                             VidTc2 = *Blanks
114870030123     c                   Eval      VidMsg = Msg(117)
114880030123     c                   Eval      *In45 = *On
114890030123     c                   Eval      *In28 = *On
114900030123     c                   Eval      *In90 = *On
114910030123     c                   GoTo      EndCt71
114920030123     c                   EndIf
114930030123      * Se la seconda consegna c'è già e non è supermercato devo mettere 'S' nella prima
114940030127     c                   If        ArbTc2 <> *Blanks and VidTc2 <> 'S' and
114950030127     c                             VidTc1 = *Blanks
114960030123     c                   Eval      VidMsg = Msg(117)
114970030123     c                   Eval      *In45 = *On
114980030123     c                   Eval      *In28 = *On
114990030123     c                   Eval      *In90 = *On
115000030123     c                   GoTo      EndCt71
115010030123     c                   EndIf
115020021129     c                   EndIf
115030021129
115040021129      * Se è stata inserita la consegna particolare 'S' si devono
115050021129      * immettere anche la persona da contattare, il telefono e il motivo
115060021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
115070021129      * Nominativo
115080021129     c                   If        VidNom = *Blanks
115090021129     c                   Eval      VidMsg = Msg(118)
115100021129     c                   Eval      *In57 = *On
115110021129     c                   Eval      *In28 = *On
115120021129     c                   Eval      *In90 = *On
115130021129     c                   GoTo      EndCt71
115140021129     c                   EndIf
115150021129      * Telefono
115160021129     c                   If        VidTel = *Blanks
115170021129     c                   Eval      VidMsg = Msg(119)
115180021129     c                   Eval      *In58 = *On
115190021129     c                   Eval      *In28 = *On
115200021129     c                   Eval      *In90 = *On
115210021129     c                   GoTo      EndCt71
115220041007     c**                 Else
115230041007     c**                 Clear                   Trul42Ds
115240041007     c**                 Movel     VidTel        D42Fax
115250041007     c**                 Call      'TRUL42R'
115260041007     c***                Parm                    Trul42Ds
115270041007     c***                If        D42Err = '1'
115280041007     c****               Eval      *In58 = *On
115290041007     c**                 Eval      *In28 = *On
115300041007     c**                 Eval      *In90 = *On
115310041007     c**                 Eval      VidMsg = D42Msg
115320041007     c**                 GoTo      EndCt71
115330041007     c**                 EndIf
115340041007     c                   EndIf
115350021129      * Motivo
115360070406      * non più obbligatorio per evitare che scrivano "bagianate"
115370070406     c**                 If        VidMot = *Blanks
115380070406     c***                Eval      VidMsg = Msg(120)
115390070406     c**                 Eval      *In59 = *On
115400070406     c**                 Eval      *In28 = *On
115410070406     c**                 Eval      *In90 = *On
115420070406     c**                 GoTo      EndCt71
115430070406     c**                 EndIf
115440021129     c                   EndIf
115450021129
115460021129     c     EndCt71       EndSr
115470030930      **
115480030930      *--- CONTROLLO SE POSSIBILE LA MANUTENZIONE DELLA DATA CONSEGNA RICHIESTA ---*
115490030930     c     Contr72       BegSr
115500050907
115510030930     c                   Eval      *In90 = *Off
115520070306     c                   clear                   aggioarp          1
115530050907
115540030930      * Se tipo o data o ora consegna richiesta sono state variate
115550031001If  1c                   If        VidTcr <> ArbTcr or
115560031030     c                             ComDcr <> ArbDcr or
115570030930     c                             VidHcr <> ArbHcr
115580150205     c
115590150205     c* permetto sempre la variazione della cons richiesta se sono in partenza
115600160115     c                   if        not *in10 or wfile='P'
115610150205     c                   Goto      EndCt72
115620150205     c                   EndIf
115630150205     c
115640051109      * Se causale 'CS' e giacenza aperta in fase 30 con appuntamento (sulla giacenza)
115650051109      * posso variare la data
115660051109      * quindi bypasso tutti gli altri controlli
115670051109     c                   If        d48cvb = 'CS' and *In04 and wgcfas = 30 and
115680051109     c                             d§gcpapp = 'A'
115690051109     c                   Eval      wgiac = 'S'
115700051109     c                   Goto      EndCt72
115710051109     c                   EndIf
115720040224      * non posso modificare se la bolla ha giacenza aperta (IN04 ON)
115730050907      * e se la fase non è = a 35 o se la bolla è bloccata come giacenza
115740050907if  2c                   If        *In04 and (wgcfas <> 35 or arbfbc = 'G')
115750070312     c                   Eval      VidMsg = Msg(141)
115760040220     c                   Eval      *In28 = *On
115770040220     c                   Eval      *In90 = *On
115780040220     c                   If        VidTcr <> ArbTcr
115790040220     c                   Eval      *In62 = *On
115800040220     c                   GoTo      EndCt72
115810040220     c                   EndIf
115820040220     c                   If        ComDcr <> ArbDcr
115830040220     c                   Eval      *In40 = *On
115840040220     c                   GoTo      EndCt72
115850040220     c                   EndIf
115860040220     c                   If        VidHcr <> ArbHcr
115870040220     c                   Eval      *In41 = *On
115880040220     c                   GoTo      EndCt72
115890040220     c                   EndIf
115900040220    2c                   EndIf
115910040324      * posso modificare se la bolla ha giacenza aperta (IN04 ON) e la fase è = 35
115920040330      * ma solo se metto la data consegna richiesta maggiore/uguale alla data eseguibilità giacenza
115930040324if  2c                   If        *In04 and wgcfas = 35
115940040330if  3c                   If        wgcded > *Zeros and ComDcr >= wgcded
115950040324     c                   Eval      wgiac = 'S'
115960040324     c                   GoTo      EndCt72
115970040324    3c                   EndIf
115980040330if  3c                   If        wgcded = *Zeros and ComDcr >= wgcddm
115990040324     c                   Eval      wgiac = 'S'
116000040324     c                   GoTo      EndCt72
116010040324    3c                   EndIf
116020040324     c                   Eval      VidMsg = Msg(128)
116030040324     c                   Eval      *In28 = *On
116040040324     c                   Eval      *In90 = *On
116050040324     c                   If        VidTcr <> ArbTcr
116060040324     c                   Eval      *In62 = *On
116070040324     c                   GoTo      EndCt72
116080040324     c                   EndIf
116090040324     c                   If        ComDcr <> ArbDcr
116100040324     c                   Eval      *In40 = *On
116110040324     c                   GoTo      EndCt72
116120040324     c                   EndIf
116130040324     c                   If        VidHcr <> ArbHcr
116140040324     c                   Eval      *In41 = *On
116150040324     c                   GoTo      EndCt72
116160040324     c                   EndIf
116170040324    2c                   EndIf
116180050907      * richiamo il controllo della "7Q"
116190050907     c                   ExSr      Contr7q
116200030930      * posso modificare se
116210031001      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
116220031001If  2c                   If        V1cPkf >= §VpoPkg or
116230031001     c                             V1cVlf >= §VpoVlm or
116240031001      *                     Destinatario supermercato o Apputamento
116250031001     c                             VidTc1 = 'S' or VidTc1 = 'A' or
116260031001     c                             VidTc2 = 'S' or VidTc2 = 'A' or
116270150506     c                             VidTc1 = 'Z' or VidTc2 = 'Z' or
116280031001      *                     Lasciato avviso
116290031001     c                             ArbFbc = 'A' or
116300031001      *                     Fermo deposito
116310031114     c                             ArbFfd = 'S' or
116320031114      *                     Tipo bolla "FW" Recapito c/assegni
116330050907     c                             ArbCbo = 'FW' or
116340050907      *                     Periodo trazione ridotte e bolla con particolarità "G8"
116350050907     c                             wDCRG8 = 'S'
116360031001     c                   GoTo      EndCt72
116370031001    2c                   EndIf
116380070320     c
116390070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
116400070320     c*  manutenzionare la spedizione
116410070920     c***                Clear                   dAr5Gen
116420070920     c***                Eval      kAr5Trd = 'GEN'
116430070920     c***  kAr5          Chain(n)  Fiar501l
116440070920     c***                If        %Found(Fiar501l)
116450070920     c***                Movel     Ar5Uni        dAr5Gen
116460070920     c***                if        §ar5MDP='S'  and §ar5FFD='S'
116470070920     c                   if        Wesar5gen='S' and par5mdp='S' and
116480070920     c                             par5FFD='S'
116490070320     c                   GoTo      EndCt72
116500070320    2c                   EndIf
116510070920    2c***                EndIf
116520070312    c
116530070312     c* Se la bolla non è in distinta, non c'e' blocco giacenza e c'e'
116540070312     c*  un evento che prevede la modifica della cons, richiesta
116550070312     c*  allora posso inserirla ed aggiornare direttamente ARB+fiar5
116560070319     c                   if        arbndc=0 and FGSgeot='S' and
116570070312     c                             arbfbc<>'G' and eveADR='S'
116580070312     c                   GoTo      EndCt72
116590070312    3c                   EndIf
116600070312     c
116610031001      *                     C/assegno non abilitato
116620031001If  2c                   If        Ar9Cas > 0
116630031001     c                   Clear                   Trul21ds
116640031001     c                   Eval      I21Tla = 'L'
116650031001     c                   Eval      I21Cbo = ArbCbo
116660031001     c                   Eval      I21Tsp = ArbTsp
116670031001     c                   Eval      I21Lnp = ArbLnp
116680031001     c                   Eval      I21Nzm = ArbNzm
116690031001     c                   Eval      I21Lna = ArbLna
116700031001     c                   Eval      I21Nzd = ArbNzd
116710130827     c****               Eval      I21Ksc = ArbKsc
116720031001     c                   Movel     ArbCtr        I21Ctr
116730031001     c                   Eval      I21Tic = Ar9Tic
116740031001     c                   Eval      I21Imp = Ar9Cas
116750031001     c                   Eval      I21Div = Ar9Vca
116760031001     c                   Eval      I21Ute = Knmus
116770031001     c                   Eval      I21Pgm = nompgm
116780130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
116790130827     c                   if        flgtb1='F'
116800130827     c                   z-add     arbksc        i21ksc
116810130827     c                   else
116820130827     c                   z-add     arbccm        i21ksc
116830130827     c                   if        i21ksc=0
116840130827     c                   z-add     8888          i21ksc
116850130827     c                   movel     arblnp        i21ksc
116860130827     c                   endif
116870130827     c                   endif
116880031001     c                   Call      'TRUL21R'
116890031001     c                   Parm                    Trul21ds
116900031001If  3c                   If        O21Fca <> *Blanks
116910031001     c                   GoTo      EndCt72
116920031001    3c                   EndIf
116930031001    2c                   EndIf
116940070309     c
116950070309     c* Se la bolla è in distinta, non devo però aggiornare
116960070306     c*  fnarb ma FIARP
116970111025    2c                   if        arbndc>0 and FGSgeot='S'
116980111025     c
116990111025     c* Se posso, scrivo FIARP, altrimenti subiro errore
117000111025    3c                   if        DcrinDist<>'S'
117010111025      * erorre
117020111025     c                   Eval      VidMsg = Msg(126)
117030111025     c                   Eval      *In28 = *On
117040111025     c                   Eval      *In90 = *On
117050111025     c                   Eval      *In40 = *On
117060111025     c                   GoTo      EndCt72
117070111025     c
117080111025   x3c                   else
117090070308     c                   eval      Aggioarp='S'
117100111025    4c                   if        comdcr<>savdcr  or vidtcr<>savtcr or
117110070308     c                             vidhcr<>savhcr
117120070309     c* Se richiamo nascosto, imposto solo il campo msgforza,
117130070309     c*  altrimenti emetto il msg
117140070330     c*  non imposto il campo se devo fare solo i controlli (26 OFF)
117150111025    5c                   if        d48ffr<>'E'
117160070308     c                   seton                                        402890
117170070308     c                   Eval      VidMsg = Msg(139)
117180111025   x5c                   else
117190070330     c
117200070330     c                   if        *in26
117210070330     c                   eval      msgforza=142
117220070309     c                   endif
117230111025    5c                   endif
117240070309     c
117250070308     c                   eval      savdcr=comdcr
117260070308     c                   eval      savtcr=vidtcr
117270070308     c                   eval      savhcr=vidhcr
117280111025    4c                   endif
117290111025    3c                   endif
117300070308     c
117310070306     c                   GoTo      EndCt72
117320111025    2c                   endif
117330070306     c
117340040220      * se arrivo qua vuol dire che non posso modificare
117350140806      * Se variazione da internet permetto la modifica
117360140806     c                   if        d48cvb<>'CI'
117370031001     c                   Eval      VidMsg = Msg(126)
117380031001     c                   Eval      *In28 = *On
117390031001     c                   Eval      *In90 = *On
117400031030     c                   If        VidTcr <> ArbTcr
117410031001     c                   Eval      *In62 = *On
117420031001     c                   GoTo      EndCt72
117430031001     c                   EndIf
117440031030     c                   If        ComDcr <> ArbDcr
117450031001     c                   Eval      *In40 = *On
117460031001     c                   GoTo      EndCt72
117470031001     c                   EndIf
117480031001     c                   If        VidHcr <> ArbHcr
117490031001     c                   Eval      *In41 = *On
117500031001     c                   GoTo      EndCt72
117510031001     c                   EndIf
117520140806     c                   endif
117530031001    1c                   EndIf
117540030930
117550030930     c     EndCt72       EndSr
117560920320     C*
117570920320     C*--- AGGIORNAMENTO FILE ----------------------------------------*
117580920320     C     REGIS7        BEGSR
117590070308     c                   clear                   ModconsR          1
117600130412     c                   clear                   wpda_ndc
117610130412     c                   clear                   wpda_ddc
117620130412     c                   clear                   wpda_dcm
117630070308     C* PRENDO I VALORI ASSOLUTI
117640070308     C                   MLLZO     1             VIDHCR
117650070308     c
117660070308     c* Mi salvo i campi della consegna
117670070308     c*  richiesta della bolla per sucessivi aggiornamenti.
117680070308     c                   eval      bolladcr=arbdcr
117690070308     c                   eval      bollahcr=arbhcr
117700070308     c                   eval      bollatcr=arbtcr
117710070308     c
117720070308     c* Mi salvo anche se modificata, solo se c'e' la DATA consegna impostat
117730070308     c                   if        vidtcr<>arbtcr or comdcr<>arbdcr or
117740070308     c                             arbhcr<>vidhcr  and comdcr>0
117750070308     c                   eval      ModconsR='S'
117760070308     c                   endif
117770070308     c
117780950104     C*  GIRO UDATE
117790950104     C                   TIME                    W0140            14 0
117800950104     C* UDATE IN GGMMAAAA
117810950104     C                   MOVE      W0140         WDTGIO            8 0
117820950104     C* UDATE IN AAAAMMGG
117830950104     C                   Z-ADD     WDTGIO        G02DAT
117840950104     C                   MOVEL     *BLANK        G02ERR
117850950104     C                   CALL      'XSRDA8'
117860950104     C                   PARM                    WLBDAT
117870950104     C                   MOVEL     G02INV        DATEU             8 0
117880941213     C                   MOVEL     KNMUS         ARBPRU
117890930507     C*
117900151230     c                   if        not *in10 or wfile='A'
117910930507     C* INVIO PER SEDE
117920941014     C                   MOVEL     'FNARBG0T'    SFILE
117930930507     C* ALLOCO
117940930507     C                   EXSR      ALLOC
117950930507     C   91              GOTO      ENDRG7
117960070320     c
117970070320     c* Se impostata la "S" o la "A" devo impostare relativo blocco
117980070320     c*  telefonate in DSBL4L
117990070320     c                   clear                   wesar4L           1
118000070320     c                   clear                   BAS               1
118010070320     c                   clear                   TFD               1
118020070320     c                   if        ((vidtc1='S' or vidtc2='S') and
118030070320     c                              arbtc1<>'S' and arbtc2<>'S')  or
118040070320     c                             ((vidtc1='A' or vidtc2='A') and
118050070320     c                              arbtc1<>'A' and arbtc2<>'A')
118060070320     c                   eval      BAS='S'
118070070320     c                   endif
118080070320     c
118090070320     c* se tolto "A" o "S" devo eliminare il blocco
118100070320     c                   if        vidtc1<>'A' and vidtc1<>'S' and
118110070320     c                              vidtc2<>'A' and vidtc2<>'S' and
118120070320     c                             (arbtc1='S' or arbtc1='A' or
118130070320     c                              arbtc2='S' or arbtc2='A')
118140070320     c                   eval      BAS='N'
118150070320     c                   endif
118160070320     c* Tolgo blocco telefonata FFD se messa consegna richiesta e
118170070320     c*  presente FFD
118180070329     c****               if        arbffd='S' and viddcr>0
118190070329     c****               eval      TFD='N'
118200070329     c****               endif
118210070320     c                   if        bas<>' ' or TFD<>' '
118220070320     c                   eval      TipoEla='C'
118230070320     c                   exsr      Gestar4L
118240070320    1c                   if        *in91
118250070515     c                   exsr      DLCSED
118260070525     C                   SETON                                        2891
118270070320     C                   GOTO      ENDRG7
118280070320    1c                   endif
118290070320     c                   endif
118300041022     c
118310041022     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
118320041022     c* iornare direttamente il file
118330041022     c                   clear                   waltrabolla       1
118340041022     C   10KARB          CHAIN     FNBLP01L                           3091
118350041022     C  n10KARB          CHAIN     FNARB01L                           3091
118360041022     c*
118370070308    1c                   if        *in91
118380070515     c                   exsr      DLCSED
118390070515     c                   unlock    fiar401l
118400070525     C                   SETON                                        2891
118410041022     C                   MOVEL     MSG(4)        VIDMSG
118420041022     C                   GOTO      ENDRG7
118430070308    1c                   endif
118440070308     c
118450070308     c* Se lna attivata con GEO
118460070308     c* Se ho aggiornato FIARP, e devo aggiornare altri dati,
118470070308     c* nei campi della consegna richeista reimposto quelli della bolla
118480070319    1c                   if        FGSgeot='S'and AggioARP='S'
118490070308     c                   eval      comdcr=Bolladcr
118500070308     c                   eval      vidhcr=bollahcr
118510070308     c                   eval      vidtcr=Bollatcr
118520070308     c                   clear                   ModConsR
118530070308     c                   endif
118540070308     c
118550041022     c                   if        *in30
118560041022     c                   eval      waltrabolla='N'
118570041022     c                   else
118580041022     c* memorizzo campi prima della variazione
118590041022     c                   eval      waltrotc1=arbtc1
118600041022     c                   eval      waltrotc2=arbtc2
118610080422     c   10              eval      waltroft3=arbft3
118620130411     c  n10              eval      wpda_ndc=arbndc
118630130411     c  n10              eval      wpda_ddc=arbddc
118640130411     c  n10              eval      wpda_dcm=arbdcm
118650141223     c  n10              eval      bollaFBC=arbfbc
118660041022     C   10KARB          CHAIN     FNARB01L                           30
118670041022     C  N10KARB          CHAIN     FNBLP01L                           30
118680041022     c* Alla fine per non sporcarmi i campi del record, richaino il
118690041022     c*  record originale che sto variando
118700130411     c                   eval      wpda_ndc=arbndc
118710130411     c                   eval      wpda_ddc=arbddc
118720130411     c                   eval      wpda_dcm=arbdcm
118730041022     c                   endif
118740151230     c                   endif
118750930507     C*
118760930507     C                   Z-ADD     DATEU         ARBDTV
118770930507     C                   TIME                    ARBORV
118780930507     C                   MOVEL     VIDCVR        ARBCVB
118790001204     C*
118800001204     C                   MOVEL     ARBTC1        SAVTC1
118810001204     C                   MOVEL     ARBTC2        SAVTC2
118820080415     c*
118830930507     C*
118840060130     C* SE DA STORICIZZARE
118850060130     C**   *IN10         IFEQ      *ON
118860060130    1C**   D66FSA        ANDEQ     'S'
118870060130     C**   *IN10         OREQ      *OFF
118880060130    1C**   D66FSP        ANDEQ     'S'
118890070308     c
118900060130    1C     D66FSA        ifeq      'S'
118910011108      *
118920011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
118930011108      * è in partenza 'P' o in arrivo 'A'
118940011108     C     *IN10         IFEQ      *ON
118950160119     c                   if        wfile='P'
118960160119     C                   MOVE      'a'           ARBPRU
118970160119     c                   else
118980011108     C                   MOVE      'A'           ARBPRU
118990160119     c                   endif
119000011108     C                   ENDIF
119010011108     C     *IN10         IFEQ      *OFF
119020011108     C                   MOVE      'P'           ARBPRU
119030011108     C                   ENDIF
119040011108      *
119050941014     C                   WRITE     FNARBG00
119060930507     C                   END
119070060301     c* storicizzazione motivazione
119080060301     c     d66mtv        ifeq      'S'
119090060301     c                   exsr      sto_mtv
119100060301     c                   endif
119110040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
119120040224      * - Tipo/Data/Ora consegna richiesta
119130040224      * - Giorni di chiusura
119140040224      * - Consegna particolare
119150040224      * devo memorizzare i dati della bolla prima della modifica
119160150129     c******             If        *In10 and
119170150206     c* dal 06/02 momorizziamo anche se varia la partenza, come se lo facesse l'arrivo
119180150129     c                   if         (VidTcr <> ArbTcr or ComDcr <> ArbDcr or
119190040224     c                               VidHcr <> ArbHcr or VidGc1 <> ArbGc1 or
119200040224     c                               VidGc2 <> ArbGc1 or VidTc1 <> ArbTc1 or
119210040224     c                               VidTc2 <> ArbTc2)
119220041022     c                   ExSr      RegAr5AR
119230040223     c                   EndIf
119240080415     c*
119250080415     c* Se modifico data consegna richiesta verifico se devo sflaggare per
119260080415     c* clienti
119270080415     c                   eval      wsflagga_C=' '
119280080422     c                   if        comdcr<>arbdcr or
119290080415     c                             vidtcr<>arbtcr
119300080415     c                   clear                   wcmitt
119310080415     c                   if        flgtb1='A'
119320080415     c                   move      arbccm        wcmitt
119330080415     c                   else
119340080415     c                   move      arbksc        wcmitt
119350080415     c                   endif
119360080416     c                   if        wcmitt>'0000000'
119370080416     c                             and %subst(wcmitt:4:4)<>'8888'
119380080415     C                   MOVEL     '3K'          COD
119390080415     C                   MOVEL(P)  wcmitt        KEY
119400080415     C                   EXSR      CTABEL
119410080415     C  N30              MOVEL     TBLUNI        DS3K
119420080415     C   30              CLEAR                   DS3K
119430080415     c                   if        §3kcq8='S'
119440080415     c                   eval      wsflagga_C='S'
119450080415     c                   endif
119460080415     c                   endif
119470080415     c                   endif
119480041022     c
119490041022     c* Se variazione in partenza e modifico  le cons.partic
119500041022     c*  sflaggo per clienti
119510041025     c  n10              if        vidtc1<>arbtc1 or vidtc2<>arbtc2
119520080415     c                             or wsflagga_c='S'
119530041022     c                   clear                   arbft3
119540041022     c                   endif
119550070319     c
119560070319     c* Se impostata la "S" o la "A" devo impostare relativo blocco
119570070319     c*  telefonate in DSBL4L
119580151230     c                   if        not *in10 or wfile='A'
119590070320     c                   if        wesar4L<>' '
119600070320     c                   if        BAS='S'
119610070320     c                   eval      §b4lbas='S'
119620070320     c                   endif
119630070320     c                   if        BAS='N'
119640070320     c                   eval      §b4lbas=' '
119650070320     c                   endif
119660070320     c
119670070320     c                   if        TFD='N'
119680070320     c                   eval      §b4lTFD=' '
119690070320     c                   endif
119700070320     c
119710070320     c                   eval      TipoEla='A'
119720070320     c                   exsr      GestAR4L
119730070320     c                   endif
119740151230     c                   endif
119750070320     c
119760920320     C*
119770920320     C                   MOVEL     COMDCR        ARBDCR
119780920320     C                   MOVEL     VIDHCR        ARBHCR
119790920320     C                   MOVEL     VIDTCR        ARBTCR
119800930507     C                   MOVEL     VIDGC1        ARBGC1
119810930507     C                   MOVEL     VIDGC2        ARBGC2
119820941102     C                   MOVEL     VIDTC1        ARBTC1
119830941102     C                   MOVEL     VIDTC2        ARBTC2
119840070308     c
119850070308     c* Se modificata consegna richiesta in arrivo
119860070308     c*  devo anche pulire il flag blocco consegna se = 'A'
119870141223     c                   if        BollaFBC='A' and ModconsR='S'
119880070308     c                             and comdcr>0
119890070308     c                   clear                   arbfbc
119900070419     c                   clear                   arbcmc
119910070308     c                   endif
119920070920     c
119930070920     c* Se modificata la consegna richiesta e bolla già consegnata
119940070920     c* effettuo il ricalcolo della affidabilità
119950070920     c                   if        ModconsR='S' and arbdcr=0 and arbdcm>0
119960070920     c                   clear                   arbdti
119970070920     c                   clear                   arbhti
119980070920     c                   movel     'S'           arbfbs
119990070920     c                   endif
120000041025     c
120010070319     c* Verifico se devo aggiornare record già esistente in ar5
120020150206     c*  ma solo per le consegne particolari e fermo deposito
120030150206     c  N10              exsr      RegAr5PA
120040941212     C*
120050930507     C* PER SEDE
120060151230     c                   if        not *in10 or wfile='A'
120070070503     C     D66FFI        IFEQ      'S'
120080950227     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
120090950413    2C     §3ABSD        IFEQ      ' '
120100930507     C*
120110941014     C                   OPEN      FNARBG0T
120120941014     C                   WRITE     FNARBGTT
120130941014     C                   CLOSE     FNARBG0T
120140950227     C                   END
120150930507     C* DISALLOCO OGGETTO
120160941212     C                   EXSR      DLCSED                                       FNARBG0T
120170950227     C                   END
120180151230     c                   endif
120190041022     c
120200930507     C*
120210151230     c                   if        wfile='A'
120220151230     C                   UPDATE    FNARB000
120230151230     c                   else
120240151230     C                   UPDATE    FNBLP000
120250151230     c                   endif
120260941220     C*
120270941220     C* DISALLOCO I RECORD CHE NON USO
120280160119     C   10              UNLOCK    FIAR601L
120290041022     c* Aggiorno anche l'altra bolla
120300151230     c                   if        not *in10 or wfile='A'
120310041022     c                   if        waltrabolla=' '
120320080422     c   10              eval      arbft3=waltroft3
120330041025     c* SE aggiorno bopartenza sflaggo per clienti se variate le cons.partic
120340041025     c                   if        *in10 and (waltrotc1<>arbtc1 or
120350080415     c                             waltrotc2<>arbtc2 or
120360080415     c                             wsflagga_c='S')
120370041022     c                   clear                   arbft3
120380041022     c                   endif
120390041022     C   10              except    arbGblp
120400041022     C  N10              except    arbGarb
120410041022     c                   endif
120420151230     c                   endif
120430950602     C*
120440070308     c* Se non è previsto l'aggiornamento di FIARP, vedo se lo devo
120450070308     c*  cancellare (se variata la data consegna richiesta)
120460070319     c                   if        FGSgeot='S'  and  ModconsR='S'
120470070308     c     karb          delete    fiarp01l
120480070308     c                   endif
120490070308     c
120500040224      * Se c'è la data consegna richiesta controllo se è ancora valida
120510040224      * solo se ho fatto una modifica in arrivo
120520150129      * da adesso anche se modifica in partenza
120530150206     c                   If        ArbDcr > *Zeros
120540040324     c                             and wCa = *Blanks and wgiac = *Blanks
120550040224     c                   ExSr      ContrDcr
120560040224     c                   EndIf
120570070308     c
120580130411     C*
120590130411     c* Richiamo routine per invio variazione a pda
120600130411     c                   if        D48Cvb<>'CS'
120610151230     c                             and (not *in10 or wfile='A')
120620130411     c                   exsr      sr_pda
120630130411     c                   endif
120640950104     C*
120650930507     C     ENDRG7        ENDSR
120660021129      **
120670021129      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
120680021129     c     Regis71       BegSr
120690021129
120700021129      * Se è stata indicata la consegna particolare 'S' devo scrivere
120710021129      * il file FIAR5
120720021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
120730021129     c                   Clear                   Fiar5000
120740021129     c                   Eval      Ar5Aas = ArbAas
120750021129     c                   Eval      Ar5Lnp = ArbLnp
120760021129     c                   Eval      Ar5Nrs = ArbNrs
120770021129     c                   Eval      Ar5Nsp = ArbNsp
120780021129     c                   Eval      Ar5Trd = 'SCR'
120790021129     c                   Move      Dateu         Ar5Dac
120800021129     c                   Movel     W0140         Ar5Orc
120810021129     c                   Clear                   Dar5Scr
120820021129     c                   Eval      §ar5Cvb = D48Cvb
120830021129     c                   Eval      §ar5Pru = Knmus
120840060131     c                   Eval      §ar5Pou = dutPou
120850021129     c                   Eval      §ar5Tcr = VidTcr
120860021129     c                   Eval      §ar5Dcr = ComDcr
120870021129     c                   Eval      §ar5Hcr = VidHcr
120880021129     c                   Eval      §ar5Gc1 = VidGc1
120890021129     c                   Eval      §ar5Gc2 = VidGc2
120900021129     c                   Eval      §ar5Tc1 = VidTc1
120910021129     c                   Eval      §ar5Tc2 = VidTc2
120920021129     c                   Eval      §ar5Nom = VidNom
120930021129     c                   Eval      §ar5Tel = VidTel
120940021129     c                   Eval      §ar5Mot = VidMot
120950021129     c                   Movel     Dar5Scr       Ar5Uni
120960021129     c                   Write     Fiar5000
120970021129     c                   EndIf
120980040705
120990040705      * Aggiorno anche il record 'GEN' di Fiar5 con il nuovo Telefono e Referente
121000091217     c                   eval      Wtipoagg='REF'
121010091217     c                   exsr      RegisAR5GEN
121020091217     c***
121030091217     c***                Eval      kAr5Trd = 'GEN'
121040091217     c***                Clear                   dAr5Gen
121050091217     c***  kAr5          Chain     Fiar501l
121060040705      * Aggiorno Fiar5
121070091217if  1c***                If        %Found(Fiar501l)
121080091217     c***                Eval      dAr5Gen = Ar5Uni
121090091217     c***                Eval      §Ar5Ref = VidNom
121100091217     c***                Eval      §Ar5Teld = VidTel
121110091217     c***                Movel(p)  dAr5Gen       Ar5Uni
121120040705      * se la modifica è fatta in partenza e devo trasmettere all'arrivo
121130040705      * sfleggo per la trasmissione
121140070306     c***                If        Not *In10 and §Ar5Trar = 'S'
121150070306     c***                Clear                   Ar5Ft1
121160070306     c***                EndIf
121170040705      * se la modifica è fatta in arrivo e devo trasmettere alla partenza
121180040705      * sfleggo per la trasmissione
121190070306     c***                If        *In10 and §Ar5Trpa = 'S'
121200070306     c***                Clear                   Ar5Ft1
121210070306     c***                EndIf
121220040705      * se devo trasmettere alla sede sfleggo per la trasmissione
121230091217     c***                If        §Ar5Trse = 'S'
121240091217     c***                Clear                   Ar5Ft2
121250091217     c***                EndIf
121260040705      * sfleggo sempre per la trasmissione al cliente
121270091217     c***                Clear                   Ar5Ft3
121280091217     c***                Update    Fiar5000
121290040705      * Scrivo Fiar5
121300091217   x1c***                Else
121310040705      * se immessi i dati
121320091217     c***                Clear                   Fiar5000
121330091217     c***                Eval      Ar5Aas = ArbAas
121340091217     c***                Eval      Ar5Lnp = ArbLnp
121350091217     c***                Eval      Ar5Nrs = ArbNrs
121360091217     c***                Eval      Ar5Nsp = ArbNsp
121370091217     c***                Eval      Ar5Trd = 'GEN'
121380091217     c***                Move      Dateu         Ar5Dac
121390091217     c***                Movel     W0140         Ar5Orc
121400091217     c***                Clear                   Dar5Gen
121410091217     c***                Eval      §Ar5Ref = VidNom
121420091217     c***                Eval      §Ar5Teld = VidTel
121430091217     c***                Movel(p)  DAr5Gen       Ar5Uni
121440091217     c***                Write     Fiar5000
121450091217    1c***                EndIf
121460130411     c* Richiamo routine per invio variazione a pda
121470130411     c                   exsr      sr_pda
121480021129
121490021129     c                   EndSr
121500040223
121510041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in arrivo--*
121520041022     c     RegAr5AR      BegSr
121530070309     c* Se Attiva GEO e bolla non in distnta e non giacente con
121540070309     c*  un evento che prevede la modifica della cons.richista
121550070309     c*  memorizzo la cons richiesta nei campi di FIAR5
121560070309     c                   clear                   AggioAR5M         1
121570070309     c
121580070319     c                   if        arbndc=0 and FGSgeot='S' and
121590070309     c                             arbfbc<>'G' and eveADR='S'
121600070309     c                   eval      AggioAR5M='S'
121610070309     c                   endif
121620160114     c* Se manutenzione in Arrivo su bolla ancora da partire memorizzo
121630160114     c* di aggiornare i dati del ripristino con quelli del video
121640160114     c                   if        *in10 and wfile = 'P'
121650160114     c                   eval      AggioAR5M='S'
121660160114     c                   endif
121670040223
121680040224     c                   Clear                   waggar5
121690040223     c                   Clear                   dAr5Gen
121700040223     c                   Eval      kAr5Trd = 'GEN'
121710040223     c     kAr5          Chain     Fiar501l
121720040223     c                   If        %Found(Fiar501l)
121730040223     c                   Movel     Ar5Uni        dAr5Gen
121740040223     c                   EndIf
121750040223
121760040223      * Dati non memorizzati
121770040223if  1c                   If        §Ar5Mdp <> 'S'
121780040224     c                   Eval      §Ar5Mdp = 'S'
121790040223     c                   Eval      §Ar5gTcr = ArbTcr
121800040223     c                   Move      ArbDcr        §Ar5gDcr
121810040223     c                   Move      ArbHcr        §Ar5gHcr
121820040223     c                   Eval      §Ar5gGc1 = ArbGc1
121830040223     c                   Eval      §Ar5gGc2 = ArbGc2
121840040223     c                   Eval      §Ar5gTc1 = ArbTc1
121850040223     c                   Eval      §Ar5gTc2 = ArbTc2
121860040223     c                   Eval      §Ar5Ffd  = ArbFfd
121870040223      * Causale 'CD' da chiusura distinta
121880140806      *   oppure
121890140806      * Causale 'CI' da indicazioni da internet
121900040223      * imposto i campi a video
121910140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
121920040223     c                   Eval      §Ar5Tcrm = VidTcr
121930040223     c                   Move      ComDcr        §Ar5Dcrm
121940040223     c                   Move      VidHcr        §Ar5Hcrm
121950040223      * Altra causale
121960040223      * imposto i campi di arb prima della variazione
121970040223   x2c                   Else
121980040223     c                   Eval      §Ar5Tcrm = ArbTcr
121990040223     c                   Move      ArbDcr        §Ar5Dcrm
122000040223     c                   Move      ArbHcr        §Ar5Hcrm
122010040223    2c                   EndIf
122020040224     c                   Eval      waggar5 = 'S'
122030040223      * Dati memorizzati
122040040223   x1c                   Else
122050140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
122060040223     c                   Eval      §Ar5Tcrm = VidTcr
122070040223     c                   Move      ComDcr        §Ar5Dcrm
122080040223     c                   Move      VidHcr        §Ar5Hcrm
122090040224     c                   Eval      waggar5 = 'S'
122100040223    2c                   EndIf
122110040223    1c                   EndIf
122120150206     c* se si tratta di una modifica di cons richiesta in partenza lo memorizzo se
122130150206     c*  non l'ho già fatto
122140150206     c                   if        not *in10 and comdcr<>arbdcr and
122150150206     c                             §ar5dcrp<>'P'  and comdcr>0
122160150206     c                   eval      §ar5dcrp= 'P'
122170150206     c                   Eval      waggar5 = 'S'
122180150206    2c                   EndIf
122190150206     c                   if        not *in10 and comdcr<>arbdcr and
122200150206     c                             §ar5dcrp= 'P'  and comdcr=0
122210150206     c                   eval      §ar5dcrp= ' '
122220150206     c                   Eval      waggar5 = 'S'
122230150206    2c                   EndIf
122240150206     c
122250040223
122260040224if  1c                   If        waggar5 = 'S'
122270040224      * aggiorno Fiar5
122280040224if  2c                   If        %Found(Fiar501l)
122290040224     c                   Movel(p)  dAr5Gen       Ar5Uni
122300040224      * se devo trasmettere alla sede sfleggo per la trasmissione
122310040224     c                   If        §Ar5Trse = 'S'
122320040224     c                   Clear                   Ar5Ft2
122330040224     c                   EndIf
122340040224      * sfleggo sempre per la trasmissione al cliente
122350040224     c                   Clear                   Ar5Ft3
122360040223     c                   Update    Fiar5000
122370040224      * scrivo Fiar5
122380040224   x2c                   Else
122390040224     c                   Clear                   Fiar5000
122400040223     c                   Eval      Ar5Aas = ArbAas
122410040223     c                   Eval      Ar5Lnp = ArbLnp
122420040223     c                   Eval      Ar5Nrs = ArbNrs
122430040223     c                   Eval      Ar5Nsp = ArbNsp
122440040224     c                   Eval      Ar5Trd = 'GEN'
122450040223     c                   Move      Dateu         Ar5Dac
122460040223     c                   Movel     W0140         Ar5Orc
122470040224     c                   Movel(p)  dAr5Gen       Ar5Uni
122480041007     c* flaggo sempre il flag di filiale tanto non trasmettiamo più
122490040223     c                   Write     Fiar5000
122500040224    2c                   EndIf
122510040224   x1c                   Else
122520040224     c                   Unlock    Fiar501l
122530040224    1c                   EndIf
122540040223
122550040223     c                   EndSr
122560041022
122570041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in partenz-*
122580041022     c     RegAr5PA      BegSr
122590041022     c                   Clear                   dAr5Gen
122600041022     c                   Eval      kAr5Trd = 'GEN'
122610041022     c     kAr5          Chain     Fiar501l
122620041022if  2c                   If        %Found(Fiar501l)
122630041022     c                   Movel     Ar5Uni        dAr5Gen
122640041022      * Flag dati memorizzati
122650041022if  3c                   If        §Ar5Mdp = 'S'
122660041022      * Aggiorno i dati di ripristino solo se uguali a quelli salvati
122670150206if  4c***                If        §Ar5gTcr= §Ar5Tcrm and §Ar5gDcr = §Ar5Dcrm
122680150206     c***                          and §Ar5gHcr = §Ar5Hcrm
122690150206     c***                Eval      §Ar5Tcrm = ArbTcr
122700150206     c***                Move      ArbDcr        §Ar5Dcrm
122710150206     c***                Move      ArbHcr        §Ar5Hcrm
122720150206    4c***                EndIf
122730041022      * Memorizzo i dati della bolla
122740150206     c***                Eval      §Ar5gTcr = ArbTcr
122750150206     c***                Move      ArbDcr        §Ar5gdcr
122760150206     c***                Move      ArbHcr        §Ar5gHcr
122770150206     c***                Eval      §Ar5gGc1 = ArbGc1
122780150206     c***                Eval      §Ar5gGc2 = ArbGc2
122790150206     c* Da 02/15
122800150206     c* come partenza aggiorno soltanto i dati del fermo deposito e dei giorni di chiusura
122810041022     c                   Eval      §Ar5gTc1 = ArbTc1
122820041022     c                   Eval      §Ar5gTc2 = ArbTc2
122830041022     c                   Eval      §Ar5Ffd = arbFfd
122840041022    3c                   EndIf
122850041022     c                   Movel     Dar5Gen       Ar5Uni
122860041022     c                   Update    Fiar5000
122870041022    2c                   EndIf
122880041022     c                   EndSr
122890070906
122900070906      *--- CONTROLLO SE ANCORA VALIDA Le consegne particolari --------*
122910070906     c     ContrTCP      BegSr
122920070910      *
122930070906     c                   clear                   eliminatcp        1
122940070911     c                   clear                   wcomb
122950070911     c                   clear                   aggtcp            1
122960070906     c
122970070910    1c                   if        arbtc1=wdisagdest
122980070910if  2c                   If        (par5Mdp = 'S' and
122990070910     c                             par5gtc1<>arbtc1 and par5gtc2<>arbtc1)
123000070911     c*******                      or   par5Mdp=' '
123010070906     c                   eval      eliminatcp='1'
123020070911     c                   eval      wtc1=arbtc1
123030070907    2c                   endif
123040070910    1c                   endif
123050070910    1c                   if        arbtc2=wdisagdest
123060070910if  2c                   If        (par5Mdp = 'S' and
123070070910     c                             par5gtc1<>arbtc2 and par5gtc2<>arbtc2)
123080070911     c********                     or   par5Mdp=' '
123090070906     c                   eval      eliminatcp='2'
123100070911     c                   eval      wtc1=arbtc2
123110070907    2c                   endif
123120070910    1c                   endif
123130070906     c* Se da eliminare
123140070910    1c                   if        eliminatcp<>' '
123150070911     c* Verifico se per le combinazioni devo ripristinare la cons partic
123160070911     c*  della partenza
123170070911     c                   if        par5mdp='S' and par5gtc1<>' '
123180070911     c                   eval      wtc2=par5gtc1
123190070911     c                   exsr      sr_chkcomb
123200070911     c
123210070911     c                   if        wcomb<>' '
123220070911     c                   eval      aggtcp=par5gtc1
123230070911     c                   endif
123240070912     c                   endif
123250070911     c
123260070911     c                   if        wcomb=' ' and par5gtc2<>' '
123270070911     c                   eval      wtc2=par5gtc1
123280070911     c                   exsr      sr_chkcomb
123290070911     c                   if        wcomb<>' '
123300070911     c                   eval      aggtcp=par5gtc2
123310070911     c                   endif
123320070911     c                   endif
123330070911     c
123340070906     c                   Clear                   dsarbg
123350070906     c                   Eval      d48cvb = 'CA'
123360070906     c                   Eval      weseg = 'E'
123370070911     c*****              Clear                   d48ffr
123380070906     c                   Eval      d48Trc = 'G'
123390070906     c                   Eval      §bgTcr = arbtcr
123400070906     c                   Move      arbdcr        §bgDcr
123410070906     c                   Move      arbhcr        §bgHcr
123420070906     c                   Eval      §bgGc1 = Arbgc1
123430070906     c                   Eval      §bgGc2 = ArbGc2
123440070910    2c                   if        eliminatcp='1'
123450070906     c                   clear                   §bgtc1
123460070906     c                   clear                   arbtc1
123470070906     c                   Eval      §bgtc2 = Arbtc2
123480070911     c* ripristino la vecchia consegna partic.
123490070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
123500070911     c                   eval      §bgtc1=aggtcp
123510070911     c                   endif
123520070910   x2c                   else
123530070906     c                   clear                   §bgtc2
123540070906     c                   clear                   arbtc2
123550070906     c                   Eval      §bgtc1 = Arbtc1
123560070911     c* ripristino la vecchia consegna partic.
123570070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
123580070911     c                   if        §bgtc1=' '
123590070911     c                   eval      §bgtc1=aggtcp
123600070911     c                   else
123610070911     c                   eval      §bgtc1=aggtcp
123620070911     c                   endif
123630070911     c                   endif
123640070910    2c                   endif
123650070907     c
123660070907     c* Memorizzo in skiera rielaborazioni
123670070910    2c                   if        kk<=10
123680070907     c                   add       1             kk
123690070907     c                   movel     dsarbg        kdati(KK)
123700070907     c                   movel     d48cvb        kcvb(KK)
123710070907     c                   movel     d48trc        ktrc(KK)
123720070911     c                   movel     ' '           kffr(KK)
123730070911     c                   movel     ' '           kf12(KK)
123740070910    2c                   endif
123750070910    1c                   endif
123760070906     c
123770070906     c                   EndSr
123780040224
123790040224      *--- CONTROLLO SE ANCORA VALIDA LA DATA CONSEGNA RICHIESTA -----*
123800160114     c     ContrDcr      BegSr
123810040225
123820040225     c                   Move      ArbDcr        warbdcr
123830050907      * richiamo il controllo della "7Q"
123840050907     c                   ExSr      Contr7q
123850040225
123860040225      * Controllo se c'è il fiar5 rcd GEN
123870070906     c                   clear                   dar5gen
123880040225     c                   Eval      kAr5Trd = 'GEN'
123890040225     c     kAr5          Chain(n)  Fiar501l
123900040225     c                   If        %Found(Fiar501l)
123910040225     c                   Movel     Ar5Uni        dAr5Gen
123920040225     c                   EndIf
123930040225      * se ho memorizzato i dati
123940040225if  1c                   If        §Ar5Mdp = 'S' and
123950040225      * e la data è diversa
123960040225     c                             (wArbDcr <> §Ar5Dcrm or
123970040225     c                               ArbTcr <> §Ar5Tcrm)
123980040225      * controllo se la data è valida quando:
123990040224      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
124000040225if  2c                   If        ArbPkf >= §VpoPkg or
124010040224     c                             ArbVlf >= §VpoVlm or
124020150506      *                     Destinatario supermercato o Appuntamento
124030040224     c                             ArbTc1 = 'S' or ArbTc1 = 'A' or
124040040224     c                             ArbTc2 = 'S' or ArbTc2 = 'A' or
124050150506     c                             ArbTc1 = 'Z' or ArbTc2 = 'Z' or
124060040312      *                     Lasciato avviso
124070070308     c                             BollaFbc = 'A' or
124080040224      *                     Fermo deposito
124090040224     c                             ArbFfd = 'S' or
124100040224      *                     Tipo bolla "FW" Recapito c/assegni
124110040811     c                             ArbCbo = 'FW'or
124120040811      *                     Periodo trazione ridotte e bolla con particolarità "G8"
124130040811     c                             wDCRG8 = 'S'
124140040225     c                   Goto      EndContr
124150040225    2c                   EndIf
124160040224      *                     C/assegno non abilitato
124170040225if  2c                   If        Ar9Cas > 0
124180040224     c                   Clear                   Trul21ds
124190040224     c                   Eval      I21Tla = 'L'
124200040224     c                   Eval      I21Cbo = ArbCbo
124210040224     c                   Eval      I21Tsp = ArbTsp
124220040224     c                   Eval      I21Lnp = ArbLnp
124230040224     c                   Eval      I21Nzm = ArbNzm
124240040224     c                   Eval      I21Lna = ArbLna
124250040224     c                   Eval      I21Nzd = ArbNzd
124260130827     c*****              Eval      I21Ksc = ArbKsc
124270040224     c                   Movel     ArbCtr        I21Ctr
124280040224     c                   Eval      I21Tic = Ar9Tic
124290040224     c                   Eval      I21Imp = Ar9Cas
124300040224     c                   Eval      I21Div = Ar9Vca
124310040224     c                   Eval      I21Ute = Knmus
124320040224     c                   Eval      I21Pgm = nompgm
124330130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
124340130827     c                   if        flgtb1='F'
124350130827     c                   z-add     arbksc        i21ksc
124360130827     c                   else
124370130827     c                   z-add     arbccm        i21ksc
124380130827     c                   if        i21ksc=0
124390130827     c                   z-add     8888          i21ksc
124400130827     c                   movel     arblnp        i21ksc
124410130827     c                   endif
124420130827     c                   endif
124430040224     c                   Call      'TRUL21R'
124440040224     c                   Parm                    Trul21ds
124450040225if  3c                   If        O21Fca <> *Blanks
124460040225     c                   Goto      EndContr
124470040225    3c                   EndIf
124480040225    2c                   EndIf
124490070320     c
124500070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
124510070320     c*  manutenzionare la spedizione
124520160114    1c                   if        §ar5MDP='S'  and §ar5FFD='S'
124530070320     c                   GoTo      EndContr
124540070911    1c                   EndIf
124550150206     c* se la cons richiesta non è stata immessa dalla partenza e sono in partenza
124560150206     c*  ripristino
124570160114     c                   if        (not *in10 and  §ar5dcrp= 'P')
124580160114     c                             or (*in10 and wfile='P')
124590150206     c                   GoTo      EndContr
124600150206    1c                   EndIf
124610150206     c
124620040225      * in questo caso devo reimpostare la data consegna richiesta come indicato in Fiar5
124630040225      * facendo una variazione di tipo 'CA'
124640040225      * senza farla vedere a video
124650040225     c                   Clear                   dsarbg
124660040225     c                   Eval      d48cvb = 'CA'
124670040225     c                   Eval      weseg = 'E'
124680070911     c*****              Clear                   d48ffr
124690040225     c                   Eval      d48Trc = 'G'
124700040324     c                   Eval      §bgTcr = §Ar5Tcrm
124710040324     c                   Move      §Ar5Dcrm      §bgDcr
124720040324     c                   Move      §Ar5Hcrm      §bgHcr
124730040315     c                   Eval      §bgGc1 = Arbgc1
124740040315     c                   Eval      §bgGc2 = ArbGc2
124750040315     c                   Eval      §bgTc1 = ArbTc1
124760040315     c                   Eval      §bgTc2 = ArbTc2
124770040225     c                   Eval      wCA = 'S'
124780070906     c* Se da eliminare anche la consegna particolare imposto
124790070906     c*  insieme la variazione
124800070911    1c                   if        eliminatcp='1'
124810070906     c                   clear                   §bgtc1
124820070911     c* ripristino la vecchia consegna partic.
124830070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
124840070911     c                   eval      §bgtc1=aggtcp
124850070911    2c                   endif
124860070911    1c                   endif
124870070911    1c                   if        eliminatcp='2'
124880070906     c                   clear                   §bgtc2
124890070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
124900070911    3c                   if        §bgtc1=' '
124910070911     c                   eval      §bgtc1=aggtcp
124920070911     c                   else
124930070911     c                   eval      §bgtc2=aggtcp
124940070911    3c                   endif
124950070911    2c                   endif
124960070911    1c                   endif
124970070906     c
124980070906     c* Memorizzo in skiera rielaborazioni
124990070906     c                   if        kk<=10
125000070907     c* se devo eliminare una consegna particolare, memorizzo nella
125010070907     c* stessa variazione
125020070907     c                   if        eliminatcp=' '
125030070907     c                   add       1             kk
125040070907     c                   endif
125050070906     c                   movel     dsarbg        kdati(KK)
125060070906     c                   movel     d48cvb        kcvb(KK)
125070070907     c                   movel     d48trc        ktrc(KK)
125080070911     c                   movel     ' '           kffr(KK)
125090070911     c                   movel     ' '           kf12(KK)
125100070906     c                   endif
125110070906     c
125120040225    1c                   EndIf
125130040224
125140040225     c     EndContr      EndSr
125150040224
125160070307     C*--- AGGIORNAMENTO FILE proposte FIARP00F ----------------------*
125170070307     C     REGISARP      BEGSR
125180070307     C                   MLLZO     1             VIDHCR
125190070307     c     karb          chain     fiarp01l
125200070307     c                   clear                   fiarp000
125210070307     c                   eval      arpaas=arbaas
125220070307     c                   eval      arplnp=arblnp
125230070307     c                   eval      arpnrs=arbnrs
125240070307     c                   eval      arpnsp=arbnsp
125250070307     c                   eval      arptcr=vidtcr
125260070307     c                   eval      arpdcr=comdcr
125270070307     c                   eval      arphcr=vidhcr
125280070308     C*
125290070308     C                   TIME                    W0140            14 0
125300070308     C* UDATE IN AAAAMMGG
125310070308     C                   MOVE      W0140         WDTGIO            8 0
125320070308     C                   Z-ADD     WDTGIO        G02DAT
125330070307     C                   MOVEL     *BLANK        G02ERR
125340070307     C                   CALL      'XSRDA8'
125350070307     C                   PARM                    WLBDAT
125360070307     C                   MOVEL     G02INV        arpdim            8 0
125370070307     c                   time                    arpoim
125380070308     C                   MOVEL     KNMUS         ARPPRU
125390070307     c                   if        %found(fiarp01l)
125400070307     c                   update    fiarp000
125410070307     c                   else
125420070307     c                   write     fiarp000
125430070307     c                   endif
125440070307     c
125450070307     c                   ENDSR
125460941223     C*
125470941223     C*--- CONTROLLI CAMPI MITTENTE ----------------------------------*
125480941223     C     CONTR8        BEGSR
125490941223     C                   SETOFF                                       9028
125500950104     C                   CLEAR                   VIDFAP
125510941223     C*
125520941223     C* CONTROLLO IL CODICE DEL MITTENTE
125530970813     C                   EXSR      CTRCOM
125540970822     C*
125550970822     C* SE PASSO DA CODIFICATO AD 8888 O VICEVERSA SPROTEGGO O PROTEGGO
125560970822     C* CAMPI DELL'INDIRIZZO
125570970822     C     VIDKSC        IFEQ      8888
125580970822     C     MEMKSC        ANDNE     8888
125590970822     C                   MOVE      VIDKSC        MEMKSC
125600970822     C                   SETOFF                                       22
125610970822     C                   SETON                                        90
125620970822     C                   END
125630970822     C*
125640970822     C     VIDKSC        IFNE      8888
125650970822     C     MEMKSC        ANDEQ     8888
125660970822     C                   MOVE      VIDKSC        MEMKSC
125670970822     C                   SETON                                        2290
125680970822     C                   CLEAR                   VIDRSM
125690970822     C                   CLEAR                   VIDINM
125700970822     C                   CLEAR                   VIDCAM
125710970822     C                   CLEAR                   VIDLOM
125720970822     C                   CLEAR                   VIDPRM
125730970822     C                   CLEAR                   VIDNZM
125740970822     C                   CLEAR                   VIDPIM
125750970822     C                   END
125760950113     C**
125770950113     C     *IN90         IFEQ      *ON
125780950113     C  N48              GOTO      ENDCT8
125790950113     C*
125800950113     C* X IL CODICE TARIFFA DO ERRORE SOLO SE NON E' STATO PREMUTO F10
125810950114     C  NKJ
125820950114     CAN 28              GOTO      ENDCT8
125830950113     C                   ENDIF
125840941223     C*
125850020508     C* SE IMMESSO UN CODICE 8888 OBBLIGATORIO l'indirizzo completo
125860020508    1C     VIDKSC        IFEQ      8888
125870970808     C                   MOVEL     VIDRSM        I14RSC
125880970808     C                   CLEAR                   I14RS2
125890970808     C                   MOVEL     VIDINM        I14IND
125900970808     C                   MOVEL     VIDLOM        E14LOC
125910970808     C                   MOVEL     VIDPIM        E14PIP
125920970808     C                   MOVEL     VIDPRM        E14PRV
125930970808     C                   MOVEL     VIDNZM        E14NAR
125940970808     C                   MOVEL     VIDCAM        E14CAP
125950970808     C                   CLEAR                   I14ISO
125960970808     C                   MOVEL     'M'           WCLI              1
125970970808     C                   EXSR      CTRIND
125980941223     C*
125990941223     C* CAMPI CHE POSSO AVER IMPOSTATO
126000970808     C                   MOVEL     E14LOC        VIDLOM
126010970808     C                   MOVEL     E14PIP        VIDPIM
126020970808     C                   MOVEL     E14PRV        VIDPRM
126030970808     C                   MOVEL     E14NAR        VIDNZM
126040970808     C                   MOVEL     E14CAP        VIDCAM
126050950104     C* SALVO IL FLAG ANTEPORTO
126060970808     C                   MOVEL     O95ISO        COMFAP
126070970808     C                   MOVEL     O95ISO        VIDFAP
126080140403     c* salvo pickup 2
126090140403     C                   MOVEL     O95gf2        comgf2
126100941223     C**
126110970808     C* ERRORE
126120970808     C   90              GOTO      ENDCT8
126130950522     C* P.IVA obbligatoria se nazione estera e c'è flag efta in
126140950522     C* tabella nazione
126150950522     C     WDESIE        IFEQ      'E'
126160950522     C     DESEFT        ANDNE     *BLANKS
126170950412     C     VIDPIM        ANDEQ     *BLANKS
126180950412     C                   MOVEL     MSG(67)       VIDMSG
126190950412     C                   SETON                                        284990
126200950412     C                   GOTO      ENDCT8
126210950412     C                   END
126220941223     c*
126230941223   X1C                   ELSE
126240950104     c*
126250950104     C* VEDO SE ESISTE CNIND
126260950104     C     KACO          CHAIN     CNIND000                           32
126270950104     C*
126280950104     C     *IN32         IFEQ      *ON
126290950104     C                   MOVEL     MSG(63)       VIDMSG
126300950104     C                   SETON                                        479028
126310950104     C                   GOTO      ENDCT8
126320950104     C                   ENDIF
126330950104     C*
126340950104     C* CONTROLLO SUL CAP PROVINCIA NAZIONE
126350970423     C     INDSTA        IFEQ      *BLANKS
126360970423     C     INDPRV        ANDEQ     *BLANKS
126370950104     C                   MOVEL     MSG(64)       VIDMSG
126380950104     C                   SETON                                        479028
126390950104     C                   GOTO      ENDCT8
126400950104     C                   ENDIF
126410950104     C* CAP OBBLIGATORIO : CONTROLLO SE CONGRUENZA CON PROVINCIA
126420030612     C                   CLEAR                   Tisi95DS
126430030612     C                   CLEAR                   Fnlv13DS
126440970811     C                   MOVEL     INDSTA        I95NAR
126450970811     C                   MOVEL     INDCAE        I95CAP
126460970811     C                   MOVEL     INDPRV        I95PRV
126470970811     C                   MOVEL     INDCIT        I95LOC
126480970811     C                   MOVEL     '3'           I95TCN
126490970811     C                   MOVEL     ARBDSP        I95DAT
126500970811     C* ERRORE PER AFFIDABILITA' = 0
126510970811     C                   MOVEL     'S'           I13AF0
126520970811     C* CONTROLLO CONGRUENZA CAP/PROVINCIA
126530970811     C                   MOVEL     'S'           I13CNG
126540970811     C* CONTROLLO NON FORZABILE CITTA' CON VIARIO
126550970811     C                   MOVEL     'S'           I13CNV                         CAP CON VIAR
126560020305     c     lnpntw        ifeq      'FED'
126570020305     C                   movel     'S'           i95fi1
126580020305     c                   endif
126590970811     C                   CALL      'FNLV13R'
126600970811     C                   PARM                    KPJBA
126610030612     C                   PARM                    Fnlv13DS
126620030612     C                   PARM                    Tisi95DS
126630950104     C* ERRORE
126640970811     C     O13ERR        IFNE      *BLANKS
126650970811     C                   MOVEL     O13MSG        VIDMSG
126660950104     C                   MOVE      CPDC          VIDMSG
126670950104     C                   SETON                                        904728
126680950104     C                   GOTO      ENDCT8
126690950104    3C                   ENDIF
126700950104     C* SALVO IL FLAG ANTEPORTO
126710970811     C                   MOVEL     O95ISO        COMFAP
126720950104     C* SOLO IN VISUALIZZAZIONE
126730970811     C                   MOVEL     O95ISO        VIDFAP
126740140403     c* salvo pickup 2
126750140403     C                   MOVEL     O95gf2        comgf2
126760950104    2C                   ENDIF
126770941223     C*
126780941227     C     ENDCT8        ENDSR
126790961218     C*
126800941223     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE MT -----------------*
126810941227     C     REGIS8        BEGSR
126820130412     c                   clear                   wpda_ndc
126830130412     c                   clear                   wpda_ddc
126840130412     c                   clear                   wpda_dcm
126850950113     C                   MOVEL     ARBRSM        OLDRSM
126860950113     C                   MOVEL     ARBKSC        OLDKSC
126870950113     C                   MOVEL     ARBCAM        OLDCAM
126880041027     C                   MOVEL     ARBcpi        savCpi
126890950104     C*  GIRO UDATE
126900950104     C                   TIME                    W0140            14 0
126910950104     C* UDATE IN GGMMAAAA
126920950104     C                   MOVE      W0140         WDTGIO            8 0
126930950104     C*
126940950104     C* UDATE IN AAAAMMGG
126950950104     C                   Z-ADD     WDTGIO        G02DAT
126960950104     C                   MOVEL     *BLANK        G02ERR
126970950104     C                   CALL      'XSRDA8'
126980950104     C                   PARM                    WLBDAT
126990950104     C                   MOVEL     G02INV        DATEU             8 0
127000950104     C*
127010941223     C                   MOVEL     KNMUS         ARBPRU
127020950104     C*
127030950104     C* SALVO ARBCCM DELLA BOLLA PERCHE LO USO
127040950104     C                   MOVEL     ARBCCM        W0070             7 0
127050941223     C*
127060941223     C* INVIO PER SEDE
127070941223     C                   MOVEL     'FNARBM0T'    SFILE
127080941223     C* ALLOCO
127090941223     C                   EXSR      ALLOC
127100941223     C* ERRORI IN ALLOCAZIONE
127110941223    1C     *IN91         IFEQ      *ON
127120941223     C                   GOTO      ENDRG8
127130941223    1C                   ENDIF
127140041025     c
127150041025     c* alloco record bolla in altro file
127160041025     c                   clear                   waltrabolla       1
127170041025     C   10KARB          CHAIN     FNBLP01L                           3091
127180041025     C  n10KARB          CHAIN     FNARB01L                           3091
127190041025     c*
127200041025     c                   if        *in91
127210041025     C                   MOVEL     MSG(4)        VIDMSG
127220070515     c                   exsr      DLCSED
127230070525     C                   SETON                                        2891
127240041025     C                   GOTO      ENDRG8
127250041025     c                   else
127260041025     c                   if        *in30
127270041025     c                   eval      waltrabolla='N'
127280041025     c                   else
127290041025     c                   eval      waltrocpi=arbcpi
127300041025     c                   eval      waltrofvf=arbfvf
127310041025     c                   eval      waltrovlf=arbvlf
127320130411     c  n10              eval      wpda_ndc=arbndc
127330130411     c  n10              eval      wpda_ddc=arbddc
127340130411     c  n10              eval      wpda_dcm=arbdcm
127350041025     C   10KARB          CHAIN     FNARB01L                           30
127360041025     C  N10KARB          CHAIN     FNBLP01L                           30
127370041025     c* Alla fine per non sporcarmi i campi del record, richaino il
127380041025     c*  record originale che sto variando
127390130411     c   10              eval      wpda_ndc=arbndc
127400130411     c   10              eval      wpda_ddc=arbddc
127410130411     c   10              eval      wpda_dcm=arbdcm
127420041025     c                   endif
127430041025     c                   endif
127440941223     C*
127450941223     C                   Z-ADD     DATEU         ARBDTV
127460941223     C                   TIME                    ARBORV
127470941223     C                   MOVEL     VIDCVR        ARBCVB
127480941223     C*
127490060130     C* SE DA STORICIZZARE
127500060130     C**   *IN10         IFEQ      *ON
127510060130    1C**   D66FSA        ANDEQ     'S'
127520060130     C**   *IN10         OREQ      *OFF
127530060130    1C**   D66FSP        ANDEQ     'S'
127540060130    1C     D66FSA        ifeq      'S'
127550941223     C                   Z-ADD     ARBKSC        ARBCCM
127560011108      *
127570011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
127580011108      * è in partenza 'P' o in arrivo 'A'
127590011108     C     *IN10         IFEQ      *ON
127600011108     C                   MOVE      'A'           ARBPRU
127610011108     C                   ENDIF
127620011108     C     *IN10         IFEQ      *OFF
127630011108     C                   MOVE      'P'           ARBPRU
127640011108     C                   ENDIF
127650011108      *
127660941223     C                   WRITE     FNARBM00
127670941223    1C                   END
127680060302     c* storicizzazione motivazione
127690060302     c     d66mtv        ifeq      'S'
127700060302     c                   exsr      sto_mtv
127710060302     c                   endif
127720941223     C*
127730950104     C                   Z-ADD     VIDKSC        ARBKSC
127740950104     C                   MOVEL     VIDKLP        ARBKSC
127750950104     C                   MOVEL     ARBKSC        ARBCCM
127760970613     C     ARBFDN        IFEQ      ' '
127770970613     C                   MOVEL     COMFAP        ARBFAP
127780970613     C                   ELSE
127790970613     C                   MOVEL     'C'           ARBFAP
127800970613     C                   ENDIF
127810970613     C**
127820020508    1C     VIDKSC        IFEQ      8888
127830941223     C                   MOVEL     VIDRSM        ARBRSM
127840941223     C                   MOVEL     VIDINM        ARBINM
127850941223     C                   MOVEL     VIDLOM        ARBLOM
127860941223     C                   MOVEL     VIDCAM        ARBCAM
127870941223     C                   MOVEL     VIDPRM        ARBPRM
127880941223     C                   MOVEL     VIDNZM        ARBNZM
127890941223     C                   CLEAR                   ARBCPI
127900950626     C*
127910950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
127920950626     C*     NELLA P.IVA
127930950626     C     VIDISM        IFEQ      '$$'
127940950626     C     VIDCPM        ANDEQ     *BLANKS
127950950626     C                   MOVEL     'PRIVATO'     VIDCPM
127960950626     C                   ENDIF
127970950626     C*
127980950104    2C     VIDISM        IFEQ      *BLANKS
127990941223     C                   MOVEL     VIDCPM        ARBCPI
128000941223     C                   ELSE
128010941223     C                   MOVEL     VIDPIM        ARBCPI
128020950104    2C                   ENDIF
128030950104     C*
128040950104     C                   ELSE
128050950412     C* PRENDO I DATI DA CNACO E CNIND
128060950104     C                   MOVEL     ACORAG        ARBRSM
128070950104     C                   MOVEL     INDVIA        ARBINM
128080950104     C                   MOVEL     INDCIT        ARBLOM
128090950104     C                   MOVEL     INDCAE        ARBCAM
128100950104     C                   MOVEL     INDPRV        ARBPRM
128110950104     C                   MOVEL     INDSTA        ARBNZM
128120950104     C                   MOVEL     INDIVA        ARBCPI
128130941223     C                   ENDIF
128140041027     c* Mi salvo laa new P.iva
128150041027     c                   eval      newcpi=arbcpi
128160041027     c
128170041025     c
128180060223     C* SE ESISTE RECORD P.IVA IN FIAR4  LO AGGIORNO
128190120413     c                   exsr      up_fiar4P
128200941223     C*
128210941223     C* PER SEDE
128220070503     C     D66FFI        IFEQ      'S'
128230950227     C*
128240950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
128250950413    2C     §3ABSD        IFEQ      ' '
128260941227     C                   OPEN      FNARBM0T
128270941227     C                   WRITE     FNARBMTT
128280941227     C                   CLOSE     FNARBM0T
128290950227    2C                   END
128300941223     C* DISALLOCO OGGETTO
128310941223     C                   EXSR      DLCSED
128320950208    1C                   END
128330941223     C*
128340941223     C* AGGIORNO FILE TASSAZIONE PADRONCINI
128350950113     C                   EXSR      AGGPMT
128360950104     C*  REIMPOSTO IL CODICE CLIENTE MITTENTE
128370950104     C                   MOVEL     W0070         ARBCCM
128380950104     C*
128390950113     C* CONTROLLO SE SI TRATTA DI VOLUME AUTOMATICO
128400950113     C* SE HA VARIATO IL MITTENTE
128410950113     C     ARBKSC        IFNE      OLDKSC
128420950113     C*
128430950113     C                   MOVEL     '7T'          COD
128440051214     C                   MOVEL(P)  ARBFVF        KEY
128450950113     C                   EXSR      CTABEL
128460950113     C                   MOVEL     TBLUNI        DS7T
128470950113     C     §7TRVL        IFEQ      'A'
128480950113     C     §7TRVL        OREQ      'P'
128490950114     C                   MOVEL     'S'           WRICVL            1
128500950113     C                   ENDIF
128510950113     C                   ENDIF
128520950113     C*
128530950114     C* SALVO I CAMPI COLLEGATI AL MITTENTE CHE USERO PER STORICIZZARE
128540950114     C                   MOVEL     ARBCTR        MITCTR
128550950114     C                   MOVEL     ARBFVF        MITFVF
128560950114     C                   MOVEL     ARBVLF        MITVLF
128570140403     c*
128580140403     c* aggiorno fiar5 record gen per pickup2
128590140407     c                   if        comgf2<>'01' and arbnzm=*blanks and
128600140407     c                             arbfdn=*blanks
128610140407     c                   eval      wpk2='S'
128620140407     c                   else
128630140407     c                   clear                   wpk2
128640140407     c                   endif
128650140407     c                   if        (wpk2='S' and par5pk2<>'S') or
128660140407     c                              (wpk2<>'S' and par5pk2='S')
128670140403     c                   eval      Wtipoagg='PIK'
128680140403     c                   exsr      regisar5gen
128690140403     c                   endif
128700950114     C*
128710950113     C* SE PREMUTO F10 DEVO FARE IL GIRO CON IL CODICE TARIFFA
128720950104     C     *INKJ         IFEQ      *ON
128730070907     C***                MOVEL     'TP'          D48CVB
128740070907     C***                MOVEL     'E'           WESEG
128750070907     c                   add       1             kk
128760070907     c                   movel     'TP'          kcvb(kk)
128770070911     c                   movel     d48ffr        kffr(kk)
128780070911     c                   movel     w48f12        kf12(kk)
128790070907     c
128800950113     C                   ELSE
128810950114     C*
128820950114     C* SE HO RIPROPOSTO IL CODICE TARIFFA DEL CLIENTE DEVO PASSARE
128830950114     C*  DALLA VIDEATA DELLA TASSAZIONE
128840950114     C                   MOVEL     ARBCTR        W003A             3
128850950114     C     W003A         IFNE      VIDCTR
128860070907     C***                MOVEL     'TP'          D48CVB
128870070907     C***                MOVEL     'E'           WESEG
128880070911     C****               CLEAR                   D48FFR
128890070907     c                   add       1             kk
128900070907     c                   movel     'TP'          kcvb(kk)
128910070911     c                   movel     ' '           kffr(kk)
128920070911     c                   movel     ' '           kf12(kk)
128930950114     C                   ELSE
128940950114     C*
128950950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
128960950114     C     WRICVL        IFEQ      'S'
128970950113     C                   EXSR      CALVOL
128980950114     C                   MOVEL     D18FVF        ARBFVF
128990950114     C                   MOVEL     D18VLF        ARBVLF
129000070911     C***                MOVEL     'I2'          D48CVB
129010070911     C***                MOVEL     'E'           WESEG
129020070911     c***                clear                   w48f12
129030070907     c                   add       1             kk
129040070907     c                   movel     'I2'          kcvb(kk)
129050070911     c                   movel     d48ffr        Kffr(KK)
129060070911     c                   movel     ' '           Kf12(kk)
129070950104     C                   ENDIF
129080950113     C                   ENDIF
129090950114     C                   ENDIF
129100950114     C* MEMORIZZO IL COD TARIFFA SULLA BOLLA, TANTO SE E' CAMBIATO
129110950114     C*  PASSO PER FORZA DALLA VIDEATA DELLA TASSAZIONE PER POTERLO
129120950114     C*  TRASMETTERE
129130950114     C                   MOVEL     VIDCTR        ARBCTR
129140000623     C**
129150120416     c* Controllo e Gestione eventuale record W in fiar4
129160120416     c                   exsr      up_fiar4W
129170041027     c* In partenza memorizzo se 1°bolla franco anche se doppia
129180041027     c* In arrivo   memorizzo se 1°bolla franco e non    doppia
129190041027     c                   if        (*in10 and not *in05) or
129200041027     c                             (not *in10 and not *in06)
129210041027     c                   movel     savcpi        arbcpi
129220041027     c                   endif
129230950114     C*
129240950114     C   10              UPDATE    FNARB000
129250950114     C  N10              UPDATE    FNBLP000
129260041025     c*
129270041025     c* Aggiorno anche l'altra bolla
129280041025     c                   if        waltrabolla=' '
129290041025     c* Se  devo riaggiornare il volume benen altrimenti tengo quello d
129300041025     c*  della bolla
129310041025     c                   if        wricvl<>'S'
129320041025     c                   eval      arbfvf=waltrofvf
129330041027     c                   eval      arbvlf=waltrovlf
129340041025     c                   endif
129350041027     c
129360041027     c* P.IVA  idem come sopra
129370041027     c                   if        (*in10 and *in06) or
129380041027     c                             (not *in10 and *in05)
129390041027     c                   movel     newcpi        arbcpi
129400041027     c                   else
129410041027     c                   movel     waltrocpi     arbcpi
129420041027     c                   endif
129430041025     c
129440041025     C   10              except    arbMblp
129450041025     C  N10              except    arbMarb
129460041025     c                   endif
129470950114     C*
129480950114     C* DISALLOCO I RECORD CHE NON USO
129490991007     C   10              UNLOCK    FIAR601L
129500130411     C*
129510130411     c* Richiamo routine per invio variazione a pda
129520130411     c                   exsr      sr_pda
129530950602     C*
129540941227     C     ENDRG8        ENDSR
129550950113     C*
129560961219     C*--- CONTROLLI RIFERIMENTO MITTENTE ----------------------------*
129570961219     C     CONTR9        BEGSR
129580961219     C                   SETOFF                                       9028
129590061024     C*
129600061024     C* NATURA MERCE SEMPRE OBBLIGATORIA
129610061024     C     VIDNAS        IFEQ      *BLANKS
129620061024     C                   SETON                                        419028
129630061024     C                   MOVEL     MSG(75)       VIDMSG
129640061024     C                   END
129650961219     C* SE DESTINATARIO ESTERO IL RIFERIMENTO E' OBBLIGATORIO
129660061024     c* Solo se c'è attraversamento dogana (come fa FNLS01R)
129670961219     C     WDESIE        IFEQ      'E'
129680970110     C     VIDRMA        ANDEQ     *BLANKS
129690061024     C     deseft        ANDne     *BLANKS
129700961219     C                   SETON                                        409028
129710961219     C                   MOVEL     MSG(74)       VIDMSG
129720961219     C                   END
129730961219     C*
129740961219     C                   ENDSR
129750060316     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE RM -----------------*
129760060316     C     REGIS9        BEGSR
129770130412     c                   clear                   wpda_ndc
129780130412     c                   clear                   wpda_ddc
129790130412     c                   clear                   wpda_dcm
129800060316     C*  GIRO UDATE
129810060316     C                   TIME                    W0140            14 0
129820060316     C* UDATE IN GGMMAAAA
129830060316     C                   MOVE      W0140         WDTGIO            8 0
129840060316     C*
129850060316     C* UDATE IN AAAAMMGG
129860060316     C                   Z-ADD     WDTGIO        G02DAT
129870060316     C                   MOVEL     *BLANK        G02ERR
129880060316     C                   CALL      'XSRDA8'
129890060316     C                   PARM                    WLBDAT
129900060316     C                   MOVEL     G02INV        DATEU             8 0
129910060316     C*
129920060316     C                   MOVEL     KNMUS         ARBPRU
129930060316     C* INVIO PER SEDE
129940060316     C                   MOVEL     'FNARBD0T'    SFILE
129950060316     C* ALLOCO
129960060316     C                   EXSR      ALLOC
129970060316     C* ERRORI IN ALLOCAZIONE
129980060316    1C     *IN91         IFEQ      *ON
129990060316     C                   GOTO      ENDRG9
130000060316    1C                   ENDIF
130010060316     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
130020060316     c* iornare direttamente il file
130030060316     c                   clear                   waltrabolla       1
130040060316     C   10KARB          CHAIN     FNBLP01L                           3091
130050060316     C  n10KARB          CHAIN     FNARB01L                           3091
130060060316     c*
130070060316     c                   if        *in91
130080060316     C                   MOVEL     MSG(4)        VIDMSG
130090070515     c                   exsr      DLCSED
130100070525     C                   SETON                                        2891
130110060316     C                   GOTO      ENDRG9
130120060316     c                   else
130130060316     c                   if        *in30
130140060316     c                   eval      waltrabolla='N'
130150060316     c                   else
130160130411     c  n10              eval      wpda_ndc=arbndc
130170130411     c  n10              eval      wpda_ddc=arbddc
130180130411     c  n10              eval      wpda_dcm=arbdcm
130190060316     C   10KARB          CHAIN     FNARB01L                           30
130200060316     C  N10KARB          CHAIN     FNBLP01L                           30
130210060316     c* Alla fine per non sporcarmi i campi del record, richaino il
130220060316     c*  record originale che sto variando
130230130411     c   10              eval      wpda_ndc=arbndc
130240130411     c   10              eval      wpda_ddc=arbddc
130250130411     c   10              eval      wpda_dcm=arbdcm
130260060316     c                   endif
130270060316     c                   endif
130280060316     C                   Z-ADD     DATEU         ARBDTV
130290060320     C                   TIME                    ARBORV
130300060410     C                   MOVEL     VIDCVR        ARBCVB
130310060316     c* STORICIZZAZIONE
130320060316    1C     D66FSA        ifeq      'S'
130330060316      *
130340060316      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
130350060316      * è in partenza 'P' o in arrivo 'A'
130360060316     C     *IN10         IFEQ      *ON
130370060316     C                   MOVE      'A'           ARBPRU
130380060316     C                   ENDIF
130390060316     C     *IN10         IFEQ      *OFF
130400060316     C                   MOVE      'P'           ARBPRU
130410060316     C                   ENDIF
130420060316      *
130430060316     c* nella ragione sociale devo mettere rma e nas ma prima devo
130440060316     c* salvare i campi della ragione sociale
130450060316     c                   movel     arbrsd        s_arbrsd
130460060316     c                   movel     arbrd2        s_arbrd2
130470060316     c                   movel(P)  arbrma        arbrsd
130480060316     c                   movel(P)  arbnas        arbrd2
130490060316     c
130500060316     C                   WRITE     FNARBd00
130510060316     c                   movel     s_arbrsd      arbrsd
130520060316     c                   movel     s_arbrd2      arbrd2
130530060316     C                   END
130540060316     c* storicizzazione motivazione
130550060316     c     d66mtv        ifeq      'S'
130560060316     c                   exsr      sto_mtv
130570060316     c                   endif
130580060316     C* Ora invio solo a sede
130590060316    1C     D66FFI        IFEQ      'S'
130600060316     C*
130610060316     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
130620060316    2C     §3ABSD        IFEQ      ' '
130630060316     C                   OPEN      FNARBD0T
130640060316     c                   movel     arbrsd        s_arbrsd
130650060316     c                   movel     arbrd2        s_arbrd2
130660060316     c                   movel(p)  vidrma        arbrsd
130670060316     c                   movel(p)  vidnas        arbrd2
130680060316     C                   WRITE     FNARBDTT
130690060316     c                   movel     s_arbrsd      arbrsd
130700060316     c                   movel     s_arbrd2      s_arbrd2
130710060316     C                   CLOSE     FNARBD0T
130720060316    2C                   END
130730060316     C* DISALLOCO OGGETTO
130740060316     C                   EXSR      DLCSED
130750060316    1C                   END
130760060316     C*
130770060316     C* F6 - AGGIORNAMENTO
130780060316     C                   MOVE      VIDRMA        ARBRMA
130790060316     C                   MOVE      VIDNAS        ARBNAS
130800060316     C   10              UPDATE    FNARB000
130810060316     C  N10              UPDATE    FNBLP000
130820060316     c
130830060316     c                   if        waltrabolla=' '
130840060316     C                   MOVE      VIDRMA        ARBRMA
130850060316     C                   MOVE      VIDNAS        ARBNAS
130860060316     C   10              except    RMblp
130870060316     C  N10              except    RMarb
130880060316     c                   endif
130890130411     C*
130900130411     c* Richiamo routine per invio variazione a pda
130910130411     c                   exsr      sr_pda
130920060316     c     endrg9        endsr
130930030123      *
130940030123      *--- Controllo campi mancato ritiro BANCALI --------------------*
130950030123     c     Contr10       BegSr
130960030123     c                   Eval      *In90 = *Off
130970030127
130980030127      * Il numero dei bancali non ritirati è obbligatorio
130990030127     c                   If        VidRb1 = *Zeros and VidRb2 = *Zeros
131000030127     c                   Eval      VidMsg = Msg(125)
131010030127     c                   Eval      *In60 = *On
131020030127     c                   Eval      *In28 = *On
131030030127     c                   Eval      *In90 = *On
131040030127     c                   GoTo      EndCt10
131050030127     c                   EndIf
131060030124      * Controllo il n. dei bancali non ritirati
131070030124     c                   If        VidRb1 > VidNba
131080030124     c                   Eval      VidMsg = Msg(124)
131090030124     c                   Eval      *In60 = *On
131100030124     c                   Eval      *In28 = *On
131110030124     c                   Eval      *In90 = *On
131120030124     c                   GoTo      EndCt10
131130030124     c                   EndIf
131140030124     c                   If        VidRb2 > VidNb2
131150030124     c                   Eval      VidMsg = Msg(124)
131160030124     c                   Eval      *In61 = *On
131170030124     c                   Eval      *In28 = *On
131180030124     c                   Eval      *In90 = *On
131190030124     c                   GoTo      EndCt10
131200030124     c                   EndIf
131210030123      * Nominativo
131220030123     c                   If        VidNomb = *Blanks
131230030124     c                   Eval      VidMsg = Msg(118)
131240030124     c                   Eval      *In57 = *On
131250030123     c                   Eval      *In28 = *On
131260030123     c                   Eval      *In90 = *On
131270030123     c                   GoTo      EndCt10
131280030123     c                   EndIf
131290030123      * Motivo
131300030123     c                   If        VidMotb = *Blanks
131310030124     c                   Eval      VidMsg = Msg(120)
131320030124     c                   Eval      *In59 = *On
131330030123     c                   Eval      *In28 = *On
131340030123     c                   Eval      *In90 = *On
131350030123     c                   GoTo      EndCt10
131360030123     c                   EndIf
131370030123
131380030123     c     EndCt10       EndSr
131390030123      **
131400030123      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
131410030123     c     Regis10       BegSr
131420130412
131430130412     c                   clear                   wpda_ndc
131440130412     c                   clear                   wpda_ddc
131450130412     c                   clear                   wpda_dcm
131460030123
131470030124     c                   Eval      §Ar5Prub = Knmus
131480060131     c                   Eval      §Ar5Poub = dutPou
131490030124     c                   Move      Dateu         §Ar5Dav
131500030124     c                   Movel     W0140         §Ar5Orv
131510030124     c                   Eval      §Ar5Rb1 = VidRb1
131520030124     c                   Eval      §Ar5Rb2 = VidRb2
131530030124     c                   Eval      §Ar5Nomb = VidNomb
131540030124     c                   Eval      §Ar5Motb = VidMotb
131550030124     c                   Movel(p)  DAr5Ban       Ar5Uni
131560030124     c                   Clear                   Ar5Ft1
131570030124     c                   Clear                   Ar5Ft2
131580030124     c                   Clear                   Ar5Ft3
131590030124     c                   Update    Fiar5000
131600041020     c*
131610041020     c* aggiornamento file fiqdt00f per gestione rientri
131620041020     c                   z-add     arbifp        kfgs
131630041020     c                   z-add     arbndc        w0060             6 0
131640041020     c     kqdt          chain     fiqdt01l
131650041020     c                   if        %found(fiqdt01l)
131660041020     c                   add       vidrb1        qdtnbnbr
131670041020     c                   add       vidrb2        qdtnbnbr
131680041020     c                   except    aggqdt
131690041020     c                   endif
131700060302     c* storicizzazione motivazione
131710060302     c     d66mtv        ifeq      'S'
131720060320     c                   Move      Dateu         arbdtv
131730060320     c                   movel     w0140         arborv
131740060302     c                   exsr      sto_mtv
131750060302     c                   endif
131760130411     C*
131770130412     c                   clear                   waltrabolla       1
131780130412     c                   if        *in10
131790130412     c                   eval      wpda_ndc=arbndc
131800130412     c                   eval      wpda_ddc=arbddc
131810130412     c                   eval      wpda_dcm=arbdcm
131820130412     c                   unlock    fnarb01l
131830130412     c                   else
131840130412     c                   unlock    fnblp01l
131850131120     c                   if        d66pda<>' '
131860130412     C     KARB          CHAIN(n)  FNARB01L                           30
131870130412     c                   if        *in30
131880130412     c                   eval      waltrabolla='N'
131890130412     c                   else
131900130412     c                   eval      wpda_ndc=arbndc
131910130412     c                   eval      wpda_ddc=arbddc
131920130412     c                   eval      wpda_dcm=arbdcm
131930130412     c                   endif
131940130412     c                   endif
131950130412     c                   endif
131960130412     c*
131970130412     c* Richiamo routine per invio variazione a pda
131980130411     c                   exsr      sr_pda
131990030123
132000030123     c                   EndSr
132010061107     C
132020061107     C*--- CONTROLLI FORMATO 11 --------------------------------------*
132030071025     C     CONTR11       BEGSR
132040061107     C                   SETOFF                                       9028
132050151105     c                   clear                   xcfiva1ds
132060151105     c                   clear                   flgiva            1 0
132070151105     c                   move      v11cod        w0040
132080080505     c
132090061107     c* se non previsto inserimento, ERRORE
132100071025    1c                   if        *in76
132110061107     C                   SETON                                        289040
132120071025    2C                   IF        v11MIDE='M'
132130061107     C                   MOVEL     MSG(92)       VIDMSG
132140061107     C                   ELSE
132150061107     C                   MOVEL     MSG(95)       VIDMSG
132160071025    2c                   endif
132170061107     C                   GOTO      ENDCT11
132180071025    1c                   endif
132190061107     c* flag unico se cliente italia
132200071025    1c                   if        v11mide='M'
132210061107     c                   eval      wcliie=wmitie
132220061107     c                   eval      wprv=arbprm
132230151105     c                   eval      wcap=arbcam
132240151105     c                   eval      wloc=arblom
132250061107     c                   else
132260061107     c                   eval      wcliie=wdesie
132270061107     c                   eval      wprv=arbprd
132280151105     c                   eval      wcap=arbcad
132290151105     c                   eval      wloc=arblod
132300071025    1c                   endif
132310071025     c
132320080505     c* Controllo la Partita IVA
132330071025     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
132340071025     C* obbligatoria
132350071025     c
132360071025    1c                   if        v11pid=*blanks
132370071025     c
132380071025    2c                   if        wdesie='E' and deseft<>*blanks
132390071025     C                   SETON                                        284190
132400071025     C                   MOVEL     MSG(67)       VIDMSG
132410071025     C                   GOTO      ENDCT11
132420120507     c****               else
132430071031     c* solo se non richiamato nascosto
132440120507    3c*****              if        d48ffr='V' or d48ffr='S'
132450120507    4c****               if        wcliie='I' and   forzapid=' '
132460120507     c*****                        and dateu>=§vpopiv
132470120507     C******             SETON                                        289041
132480120507     C*****              MOVEL     MSG(57)       VIDMSG
132490120507     c******             movel     '1'           forzapid
132500120507     C********           GOTO      ENDCT11
132510120507    4c*********          endif
132520120507    3c********           endif
132530071031     c
132540071025    2c                   endif
132550071025    1C                   ENDif
132560071025     C*
132570151105    1c***                if        v11pid<>*blanks
132580071025     c* Se cambiata pulisco la forzatura
132590071025    2c                   if        v11pid<>sav11pid
132600071025     c                   clear                   forzapid2
132610071025     c                   movel     v11pid        sav11pid
132620071025    2c                   endif
132630071025     c
132640151105     c                   clear                   xcfiva1ds
132650071025     c                   clear                   xcfivamod
132660151105     c                   eval      xcfivafori= 'S'
132670100120     c                   eval      xcfivamod = 'P'
132680071025     c                   eval      xcfivapar = v11pid
132690071025     c                   eval      xcfivanar = wnar
132700071025     c                   eval      xcfivaprv = wprv
132710151105     c                   eval      xcfivacap = wcap
132720151105     c                   eval      xcfivaloc = wloc
132730151105     c                   if        w0040=8888 or w0040=9999
132740151105     c                   eval      xcfivap88='8'
132750151105     c                   endif
132760151105     c                   call      'XCFIVAR1'
132770151105     c                   parm                    xcfiva1ds
132780071025     C*
132790071025      * --> errata forzabile
132800100120    2c                   If        xcfivaflg = -9 and forzaPID2 = ' '
132810071025     c                   seton                                        289041
132820071025     c                   Eval      vidmsg = xcfivamsg
132830071025     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
132840100120     c                   eval      forzaPID2= '1'
132850071025     C                   GOTO      ENDCT11
132860071025    2c                   EndIf
132870071025      * --> errore
132880071025    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
132890071025     c                   seton                                        289041
132900071025     c                   eval      vidmsg = xcfivamsg
132910071025     C                   GOTO      ENDCT11
132920071025    2c                   EndIf
132930151105
132940151105     c                   eval      flgiva=xcfivaflg
132950071025     c
132960071025      * se non impostato devo riportare il codice iso della partita iva che
132970071025      * mi viene passato dal pgm di controllo
132980151105    2c                   if        v11pid<>*blanks and v11isd <> ivaeu
132990071025     c                   eval      v11isd = ivaeu
133000071025     c                   seton                                          9041
133010071025     C                   GOTO      ENDCT11
133020071025    2c                   endif
133030151105    1c****               endif
133040080505     c
133050080505      * Devo richiamare il nuovo driver fatto per controllare il codice
133060080505      * fiscale
133070151105     c                   clear                   xcfiva1ds
133080080505     c                   clear                   xcfivamod
133090080505     c                   eval      xcfivapar = v11cdfis
133100080505     c                   eval      xcfivanar = wnar
133110080505     c                   eval      xcfivaprv = wprv
133120151105     c                   eval      xcfivacap = wcap
133130151105     c                   eval      xcfivaloc = wloc
133140151105     c                   eval      xcfivapiva= v11pid
133150151105     c                   if        w0040=8888 or w0040=9999
133160151105     c                   eval      xcfivap88='8'
133170151105     c                   endif
133180151105     c                   call      'XCFIVAR1'
133190151105     c                   parm                    xcfiva1ds
133200080505     c*
133210080505    1c                   if        xcfivaflg < *zeros
133220080505     C                   SETON                                        289040
133230080505     C                   MOVEL     xcfivamsg     VIDMSG
133240080505     C                   GOTO      ENDCT11
133250080505    1c                   endif
133260080505     c
133270080505     c* Obbligatorio se cliente italia partita iva o codice fiscale
133280080505     c* solo se non richiamato nascosto
133290120507    1c***                if        d48ffr='V' or d48ffr='S'
133300120507    2c****               if        wcliie='I' and   v11cdfis=*blanks
133310120507     c****                         and forzacdf=' ' and xcfivaflg<>9
133320120507     C****               SETON                                        289040
133330120507     C*****              MOVEL     MSG(23)       VIDMSG
133340120507     c****               movel     '1'           forzacdf
133350120507     C*****              GOTO      ENDCT11
133360120507    2c*****              endif
133370120507    1c******             endif
133380080505     c
133390080505     c* Se entrambe vuote errore
133400080505    1c                   if        v11cdfis=*blanks and v11pid=*blanks
133410080505     C                   SETON                                        284090
133420080505     C                   MOVEL     MSG(145)      VIDMSG
133430080505     C                   GOTO      ENDCT11
133440080505    1c                   endif
133450120508     c* Se fattura immediata errore se partita iva '$$'
133460120508     c                   if        %scan('$$':v11pid)>0  and wcliie='I'
133470120508     c                   move      v11cod        w0040
133480120508     c                   if        w0040=8888 or w0040=9999
133490120507     C                   SETON                                        284190
133500120507     C                   MOVEL     MSG(149)      VIDMSG
133510120507     C                   GOTO      ENDCT11
133520120508     c                   endif
133530120507     c                   endif
133540120508     c* solo se non richiamato nascosto
133550130315    3c                   if        d48ffr='V' or d48ffr='S'
133560120507     c* Se codice fiscale lungo 11 si richiede la partita IVA  - Errore forzabile
133570120507     c                   if        v11cdfis<>*blanks and v11pid=*blanks and
133580120507     c                             %subst(v11cdfis:12:1)=' ' and wcliie='I'
133590120507     c                             and forzapid=' '
133600120507     C                   SETON                                        289041
133610120507     C                   MOVEL     MSG(57)       VIDMSG
133620120507     c                   movel     '1'           forzapid
133630120507     C                   GOTO      ENDCT11
133640120507     c                   endif
133650130315     c                   endif
133660061107     C*
133670061107     C     ENDCT11       ENDSR
133680130927     c
133690130927     C*--- CONTROLLI FORMATO 12 --------------------------------------*
133700130927     C     CONTR12       BEGSR
133710130927     C                   SETOFF                                       9028
133720130930     c* controllo indirizzo email
133730131001     c                   if        *in86  and videml<>*blanks
133740130930     c                   Clear                   dsemail
133750130930     c                   Eval      §emlindi = videml
133760130930     c                   Call      'XEMAIL'
133770130930     c                   Parm                    dsemail
133780130930if  3c                   If        §emlerro <> '0'
133790130930     C                   SETON                                        289041
133800130930     C                   MOVEL     MSG(150)      VIDMSG
133810130930     C                   GOTO      ENDCT12
133820130930     c                   endif
133830130930
133840130930     c                   endif
133850130930     c
133860130927     C     ENDCT12       ENDSR
133870061107     C*
133880061107     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE FI -----------------*
133890061107     C     REGIS11       BEGSR
133900130412
133910130412     c                   clear                   wpda_ndc
133920130412     c                   clear                   wpda_ddc
133930130412     c                   clear                   wpda_dcm
133940061107     C*  GIRO UDATE
133950061107     C                   TIME                    W0140            14 0
133960061107     C* UDATE IN GGMMAAAA
133970061107     C                   MOVE      W0140         WDTGIO            8 0
133980061107     C*
133990061107     C* UDATE IN AAAAMMGG
134000061107     C                   Z-ADD     WDTGIO        G02DAT
134010061107     C                   MOVEL     *BLANK        G02ERR
134020061107     C                   CALL      'XSRDA8'
134030061107     C                   PARM                    WLBDAT
134040061107     C                   MOVEL     G02INV        DATEU             8 0
134050061107     C                   MOVEL     KNMUS         ARBPRU
134060071025     c*
134070071025     C     V11ISD        IFEQ      '$$'
134080071025     C     V11CPD        ANDEQ     *BLANKS
134090071025     C                   MOVEL     'PRIVATO'     V11CPD
134100071025     C                   ENDIF
134110071025     c
134120071025     c* Salvo a parte la partita iva presente in bolla
134130071025     c                   movel     arbcpi        bollacpi
134140061107     C*
134150061129     c* Se la bolla non è trasmessa non devo allocare il file e non devo
134160061129     c*    creare la variazione
134170061129     c                   if        bollaft2<>' '
134180061107     C* INVIO PER SEDE
134190061107     C                   MOVEL     'FNARBD0T'    SFILE
134200061107     C* ALLOCO
134210061107     C                   EXSR      ALLOC
134220061107     C* ERRORI IN ALLOCAZIONE
134230061107    1C     *IN91         IFEQ      *ON
134240061107     C                   GOTO      ENDRG11
134250061107    1C                   ENDIF
134260061129    1C                   ENDIF
134270061107     c
134280061107     c* Alloco il record fiar4 del codice fiscale                      agg
134290061107     C                   MOVEL     'Q'           WTRC
134300061107     C     KARB2         CHAIN     FiAR4000                           3091
134310061107     c*
134320061107     c                   if        *in91
134330061107     C                   MOVEL     MSG(4)        VIDMSG
134340070515     c                   exsr      DLCSED
134350070525     C                   SETON                                        2891
134360061107     C                   GOTO      ENDRG11
134370061107     c                   endif
134380061107     c
134390061107     c                   if        *in30
134400061107     c                   eval      esiste='N'
134410061116     c                   clear                   ar4not
134420061107     c                   else
134430061107     c                   eval      esiste='S'
134440061107     c                   endif
134450071026     c*
134460071026     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
134470071026     c* iornare direttamente il file solo se non si tratta di bolla doppia
134480071026     c                   clear                   waltrabolla       1
134490130412     c                   setoff                                       3091
134500130412    1c                   if        §3atb2=*blanks and aggiobol<>'N'
134510071026     C   10KARB          CHAIN     FNBLP01L                           3091
134520071026     C  n10KARB          CHAIN     FNARB01L                           3091
134530130412     c                   else
134540131120     c                   if        d66pda<>' '
134550130412     c                   if        not *in10
134560130412     C     KARB          CHAIN(n)  FNARB01L                           30
134570130412     c                   endif
134580130412     c                   eval      wpda_ndc=arbndc
134590130412     c                   eval      wpda_ddc=arbddc
134600130412     c                   eval      wpda_dcm=arbdcm
134610130412     c                   endif
134620130412    1c                   endif
134630071026     c*
134640071026    2c                   if        *in91
134650071026     C                   MOVEL     MSG(4)        VIDMSG
134660071026     c*
134670071026     c                   exsr      DLCSED
134680071026     c                   unlock    fiar401l
134690071026     c
134700071026     C                   SETON                                        2891
134710071026     C                   GOTO      ENDRG11
134720071026   x2c                   else
134730071026    3c                   if        *in30
134740071026     c                   eval      waltrabolla='N'
134750071026   x3c                   else
134760130412    4c                   if        §3atb2=*blanks and aggiobol<>'N'
134770130411     c  n10              eval      wpda_ndc=arbndc
134780130411     c  n10              eval      wpda_ddc=arbddc
134790130411     c  n10              eval      wpda_dcm=arbdcm
134800071026     C   10KARB          CHAIN     FNARB01L                           30
134810071026     C  N10KARB          CHAIN     FNBLP01L                           30
134820071026     c* Alla fine per non sporcarmi i campi del record, richaino il
134830071026     c*  record originale che sto variando
134840130411     c   10              eval      wpda_ndc=arbndc
134850130411     c   10              eval      wpda_ddc=arbddc
134860130411     c   10              eval      wpda_dcm=arbdcm
134870130412    4c                   endif
134880071026    3c                   endif
134890071026    2c                   endif
134900130412    1c****               endif
134910061107     C*
134920061107     C                   Z-ADD     DATEU         ARBDTV
134930061107     C                   TIME                    ARBORV
134940061107     C                   MOVEL     VIDCVR        ARBCVB
134950061107     c* Memorizzo se si tratta del mittente o destinatario
134960061107     c                   movel     v11mide       arbrd2
134970061107     c
134980061107     c* clearo gli altri campi del file che non mi interessano
134990061107     c                   clear                   arbrsd
135000061107     c                   clear                   arbind
135010061107     c                   clear                   arbcad
135020061107     c                   clear                   arblod
135030061107     c                   clear                   arbprd
135040061107     c                   clear                   arbnzd
135050061107     c                   clear                   arbfin
135060061107     c                   clear                   arbpkb
135070061107     c                   clear                   arbpkf
135080061107     c                   clear                   arbvlb
135090061107     c                   clear                   arbvlf
135100061107     c                   clear                   arbfvb
135110061107     c                   clear                   arbfvf
135120061107     c                   clear                   arbffd
135130061107     c                   clear                   arbfst
135140061107     C*
135150061107     C* SE DA STORICIZZARE
135160061107    1C     D66FSA        ifeq      'S'
135170061107      *
135180061107      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
135190061107      * è in partenza 'P' o in arrivo 'A'
135200061107     C     *IN10         IFEQ      *ON
135210061107     C                   MOVE      'A'           ARBPRU
135220061107     C                   ENDIF
135230061107     C     *IN10         IFEQ      *OFF
135240061107     C                   MOVE      'P'           ARBPRU
135250061107     C                   ENDIF
135260061107     c                   if        v11mide='M'
135270061107     c                   movel     ar4not        arbcpi
135280061107     c                   else
135290061107     c                   move      ar4not        arbcpi
135300061107     c                   endif
135310071025     c* Partita iva
135320071025     c                   movel     bollacpi      arbrsd
135330061107      *
135340061107     C                   WRITE     FNARBD00
135350061107    1C                   ENDIF
135360061107    c
135370061107     c* storicizzazione motivazione
135380061107     c     d66mtv        ifeq      'S'
135390061107     c                   exsr      sto_mtv
135400061107     c                   endif
135410071025     c* Aggiorno campo codice fiscale
135420061107     c                   movel     v11cdfis      arbcpi
135430071025     c* Aggiorno campo per la partita iva
135440071025     C                   MOVEL     V11PID        ARBrsd
135450071025     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
135460071025     C*  IL CODICE DELLA P.IVA
135470071025     C     V11CPD        IFNE      *BLANKS
135480071025     C     V11ISD        ANDEQ     *BLANKS
135490071025     C                   CLEAR                   ARBrsd
135500071025     C                   MOVEL     V11CPD        ARBrsd
135510071025     C                   ENDIF
135520130402     c* Se Cd.Fisc. lungo 11 lo metto anche nella P.Iva se vuota
135530130402     c                   if        v11pid=*blanks and %subst(v11cdfis:12:1)=' '
135540130402     c                   movel     v11cdfis      arbrsd
135550130402     c                   endif
135560071025     c
135570071025     C                   MOVEL     'P'           WTRC
135580071025     C     KARB2         CHAIN     FIAR401L                           30
135590071025    1C     *IN30         IFEQ      *OFF
135600071025    2c                   if        v11mide='M'
135610071025     c                   movel     ar4not        wcpifile
135620071025   x2c                   else
135630071025     c                   move      ar4not        wcpifile
135640071025    2c                   endif
135650071025    2c                   if        wcpifile <> arbrsd
135660071025     c                   movel     'T'           ar4ftr
135670071025     c                   z-add     dateu         ar4dtr
135680071025     c                   z-add     dateu         ar4duv
135690071025    3c                   if        v11mide='M'
135700071025     c                   eval      %subst(ar4not:1:16)=%subst(arbrsd:1 :16)
135710071025     c                   else
135720071025     c                   eval      %subst(ar4not:20:16)=%subst(arbrsd:1:16)
135730071025    3c                   endif
135740071025     c
135750071025     C                   UPDATE    FIAR4000
135760071025   x2c                   else
135770071025     c                   unlock    fiar401l
135780071025    2c                   endif
135790071025    1c                   endif
135800061129     c
135810071025    0c                   if        bollaft2<>' '
135820061107     C* Ora invio solo a sede
135830061107     C     D66FFI        IFEQ      'S'
135840061107     C*
135850061107     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
135860061107    2C     §3ABSD        IFEQ      ' '
135870061107     C                   OPEN      FNARBD0T
135880061107     C                   WRITE     FNARBDTT
135890061107     C                   CLOSE     FNARBD0T
135900061107    2C                   END
135910061107     C* DISALLOCO OGGETTO
135920061107     C                   EXSR      DLCSED
135930061107    1C                   END
135940071025    0C                   END
135950061107     C*
135960061107     C* AGGIORNO  file: 30 on write del record
135970071025     C                   MOVEL     'Q'           WTRC
135980071025     C     KARB2         CHAIN     FiAR4000                           3091
135990061107     c                   if        Esiste='N'
136000061107     c                   movel     arbaas        ar4aas
136010061107     c                   movel     arblnp        ar4lnp
136020061107     c                   movel     arbnrs        ar4nrs
136030061107     c                   movel     arbnsp        ar4nsp
136040061107     c                   movel     'Q'           ar4trc
136050061107     c                   clear                   ar4not
136060061107     c                   clear                   ar4flo
136070061107     c                   endif
136080061107     c                   if        v11mide='M'
136090061107     c                   movel     v11cdfis      ar4not
136100061107     c                   else
136110061107     c                   move      v11cdfis      ar4not
136120061107     c                   endif
136130061107     c                   movel     'T'           ar4ftr
136140061107     c                   z-add     dateu         ar4dtr
136150061107     c                   z-add     dateu         ar4duv
136160061107     c
136170061116     c                   if        Esiste='N'  and ar4not<>*blanks
136180061107     c                   write     fiar4000
136190061116     c                   endif
136200061116     c                   if        Esiste='S'  and ar4not<>*blanks
136210061107     c                   update    fiar4000
136220061107     c                   endif
136230061116     c                   if        Esiste='S'  and ar4not=*blanks
136240061116     c                   delete    fiar4000
136250061116     c                   endif
136260071025     c
136270071026     c* Aggiorno la bolla per la partita iva solo se non richiamamto da I0
136280071026     c*  per cui ho già aggiornato la partita iova sulla bolla
136290071026     c                   if        Aggiobol<>'N'
136300071025     c                   movel     arbrsd        arbcpi
136310071025     c* aggiornamento mirato per non sporcare degli altri campi
136320071025     c  n10              except    SolocpiP
136330071025     c   10              except    SolocpiA
136340071026     c
136350071026     c* Se 'c'e' l'altra ed è bolla singola la aggiorno
136360071026     c                   if        §3atb2=*blanks and waltrabolla=' '
136370071026     c  n10              except    SolocpiA
136380071026     c   10              except    SolocpiP
136390071026     c                   endif
136400071026     c                   endif
136410061107     C*
136420061107     C* DISALLOCO  I FILES CHE NON USO
136430061107     C   10              UNLOCK    FiAR901L
136440071025     c** 10              unlock    fnarb01l
136450071025     c**N10              unlock    fnblp01l
136460130411     C*
136470130411     c* Richiamo routine per invio variazione a pda
136480130411     c                   exsr      sr_pda
136490061107     C*
136500061107     C     ENDRG11       ENDSR
136510130927     C*
136520130930     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE Nx -----------------*
136530130927     C     REGIS12       BEGSR
136540130927     c
136550130927     C*  GIRO UDATE
136560130927     C                   TIME                    W0140            14 0
136570130927     C* UDATE IN GGMMAAAA
136580130927     C                   MOVE      W0140         WDTGIO            8 0
136590130927     C*
136600130927     C* UDATE IN AAAAMMGG
136610130927     C                   Z-ADD     WDTGIO        G02DAT
136620130927     C                   MOVEL     *BLANK        G02ERR
136630130927     C                   CALL      'XSRDA8'
136640130927     C                   PARM                    WLBDAT
136650130927     C                   MOVEL     G02INV        DATEU             8 0
136660130927     C                   MOVEL     KNMUS         ARBPRU
136670130927     C                   Z-ADD     DATEU         ARBDTV
136680130927     C                   TIME                    ARBORV
136690130927     C                   MOVEL     VIDCVR        ARBCVB
136700131001      *
136710131001      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
136720131001      * è in partenza 'P' o in arrivo 'A'
136730131001     C     *IN10         IFEQ      *ON
136740131001     C                   MOVE      'A'           ARBPRU
136750131001     C                   ENDIF
136760131001     C     *IN10         IFEQ      *OFF
136770131001     C                   MOVE      'P'           ARBPRU
136780131001     C                   ENDIF
136790130927     C*
136800130927     C* SE DA STORICIZZARE
136810130927    1C     D66FSA        ifeq      'S'
136820130930
136830131001     c                   if        *in84 and
136840131001     c                                  (vidref<>par5ref or vidteld<>par5teld)
136850130930     c* Referente e consegna
136860130927     c                   clear                   darbn_ref
136870130927     c                   eval      §arbnref = par5ref
136880130927     c                   eval      §arbnteld= par5teld
136890130927     c                   eval      arbuni70 = darbn_ref
136900130927     c                   eval      arbtrd='REF'
136910130927     c                   write     FNARBN00
136920131001     c                   endif
136930131001     c
136940131001     c                   if        *in83 and emd_§ar5tel<>vidtsms
136950130930     c                   clear                   darbn_sms
136960130930     c                   eval      §arbncel = emd_§ar5tel
136970130930     c                   eval      arbuni70 = darbn_sms
136980130927     c                   eval      arbtrd='SMS'
136990130927     c                   write     FNARBN00
137000131001     c                   endif
137010131001     c
137020131001     c                   if        *in86 and emd_§ar5eml<>videml
137030130927     c                   clear                   darbn_eml
137040130930     c                   eval      §arbneml=emd_§ar5eml
137050130927     c                   eval      arbuni70 = darbn_eml
137060130927     c                   eval      arbtrd='EML'
137070130927     c                   write     FNARBN00
137080131001     c                   endif
137090131001
137100131001     c                   if        *in81 and (vidnt1<>v1cnt1 or vidnt2<>v1cnt2)
137110130930     c                   clear                   darbn_not
137120130930     c                   eval      §arbnnot1=v1cnt1
137130130930     c                   eval      §arbnnot2=v1cnt2
137140130930     c                   eval      arbuni70 = darbn_not
137150130930     c                   eval      arbtrd='NOT'
137160130930     c                   write     FNARBN00
137170130930     c                   endif
137180130930
137190130930     c                   endif
137200130930     c
137210131001     c                   if        *in84
137220130930     c                   exsr      AggioREF
137230131001     c                   endif
137240131001     c
137250130930      * EMAIL e cell sms
137260131001     c                   if        *in83 or *in86
137270130930     c                   Eval      kAr5Trd = 'EMD'
137280130930     c     kAr5          Chain     Fiar501l
137290130930      * Aggiorno Fiar5
137300130930if  2c                   If        %Found(Fiar501l)
137310130930     c                   movel     ar5uni        DAr5EMD
137320131001     c   86              eval      emd_§ar5eml=videml
137330131001     c   83              eval      emd_§ar5tel=vidtsms
137340130930     c                   Movel(p)  DAr5emd       Ar5Uni
137350130930     c                   Update    Fiar5000
137360130930      * Scrivo Fiar5
137370130930   x2c                   Else
137380130930      * se immessi i dati
137390130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
137400130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
137410130930      * di manutenzione bolle
137420130930if  3c                   If        Videml <> *Blanks or Vidtsms <> *Blanks
137430130930     c                   Clear                   Fiar5000
137440130930     c                   Eval      Ar5Aas = ArbAas
137450130930     c                   Eval      Ar5Lnp = ArbLnp
137460130930     c                   Eval      Ar5Nrs = ArbNrs
137470130930     c                   Eval      Ar5Nsp = ArbNsp
137480130930     c                   Eval      Ar5Trd = 'EMD'
137490130930     c                   Move      Dateu         Ar5Dac
137500130930     c                   Movel     W0140         Ar5Orc
137510130930     c                   Clear                   Dar5emd
137520131001     c   86              eval      emd_§ar5eml=videml
137530131001     c   83              eval      emd_§ar5tel=vidtsms
137540130930     c                   Movel(p)  DAr5emd       Ar5Uni
137550130930     c                   Write     Fiar5000
137560130930    3c                   EndIf
137570130930    2c                   EndIf
137580130930     c
137590131001   x1c                   endif
137600130930
137610131001     c                   if        *in81
137620130930     c* aggiorno le NOTE
137630130930     c* aggiorno/deleto trk 8 e 9 se variati
137640130930     c                   clear                   fnlv19ds
137650130930     C                   MOVEL     'T'           D19ftr
137660130930     C                   MOVEL     arbAAS        D19AAS
137670130930     C                   MOVEL     arbLNP        D19LNP
137680130930     C                   MOVEL     arbNRS        D19NRS
137690130930     C                   MOVEL     arbNSP        D19NSP
137700130930     C                   MOVEL     vidnt1        D19NT1
137710130930     C                   MOVEL     vidnt2        D19NT2
137720130930    2C     vidnt1        IFNE      *BLANKS
137730130930     C     vidnt2        ORNE      *BLANKS
137740130930     C                   MOVEL     '8'           D19TRC
137750130930     C                   CALL      'FNLV19R'
137760130930     C                   PARM                    fnlv19ds
137770130930    2C                   ENDIF
137780130930     c* Se il pgm mi restituisce la seconda riga vuota la deleto perchè
137790130930     c* prima poteva esistere
137800130930    2C     D19ERR        IFEQ      *BLANKS
137810130930     C     D19NT1        ANDNE     *BLANKS
137820130930     C     D19NT2        ANDEQ     *BLANKS
137830130930     C                   MOVEL     '9'           D19TRC
137840130930     C                   CLEAR                   D19NT1
137850130930     C                   CLEAR                   D19NT2
137860130930     C                   CALL      'FNLV19R'
137870130930     C                   PARM                    fnlv19ds
137880130930   x2C                   else
137890130930     C* Se tornato codice di err richiamo il pgm 2 volte:  una per nota
137900130930    3C     D19ERR        IFNE      *BLANKS
137910130930     C     vidnt1        oreq      *BLANKS
137920130930     C     vidnt2        ANDEQ     *BLANKS
137930130930     C* Nota 1
137940130930     C                   MOVEL     '8'           D19TRC
137950130930     C                   MOVEL     vidnt1        D19NT1
137960130930     C                   CLEAR                   D19NT2
137970130930     C                   CALL      'FNLV19R'
137980130930     C                   PARM                    fnlv19ds
137990130930     C* Nota 2
138000130930     C                   MOVEL     '9'           D19TRC
138010130930     C                   MOVEL     vidnt2        D19NT1
138020130930     C                   CLEAR                   D19NT2
138030130930     C                   CALL      'FNLV19R'
138040130930     C                   PARM                    fnlv19ds
138050130930    3C                   ENDIF
138060130930    2C                   ENDIF
138070130930
138080130930    1c                   EndIf
138090130927     c*
138100131008     c   10              unlock    fnarb01l
138110131008     c  n10              unlock    fnblp01l
138120130927     C     ENDRG12       ENDSR
138130961219     C*
138140950113     C*--- CALCOLO IL VOLUME AUTOMATICO ------------------------------*
138150950113     C     CALVOL        BEGSR
138160030612     C                   CLEAR                   Fnlv18DS
138170950113     C   10              MOVEL     'A'           D18TBO
138180950113     C  N10              MOVEL     'P'           D18TBO
138190950113     C                   Z-ADD     ARBKSC        D18KSC
138200950113     C                   Z-ADD     ARBCTR        D18CTR
138210950113     C                   MOVEL     §3ATB1        D18TBL
138220950113     C                   Z-ADD     ARBAAS        D18AAS
138230950113     C                   Z-ADD     ARBMGS        D18MGS
138240950113     C                   Z-ADD     ARBNCL        D18NCL
138250950113     C                   Z-ADD     ARBPKF        D18PKF
138260020311     c                   movel     arbtsp        d18tsp
138270020311     c                   z-add     arblna        d18lna
138280020311     c                   z-add     arblnp        d18lnp
138290950113     C                   CALL      'FNLV18R'
138300030612     C                   PARM                    Fnlv18DS
138310950113     C*
138320950113     C                   ENDSR
138330911212     C*
138340970808     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
138350970808     C     CTRIND        BEGSR
138360150203     c                   clear                   normlod
138370970808     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
138380030612     C                   CLEAR                   Fnlv13DS
138390030612     C                   CLEAR                   Tisi95DS
138400970808     C*
138410100120     c                   movel     'S'           i14forzait
138420970808     C                   MOVEL     ARBDSP        I14DRI
138430020305     c                   movel     'S'           i14cnz
138440970808     C                   CALL      'FNLV14R'
138450970808     C                   PARM                    KPJBA
138460030612     C                   PARM                    Fnlv14DS
138470970808     C                   MOVEL     O14ERR        WERR              1
138480970808     C                   MOVEL     O14MSG        VIDMSG
138490970808     C                   EXSR      CTRERR
138500970808     C*
138510970808     C     *IN90         IFEQ      *ON
138520970808     C                   GOTO      ENDIND
138530970808     C                   ENDIF
138540970808     C*************
138550970808     C* CONTROLLO CAP
138560970808     C                   MOVEL     '7'           I95TCN
138570970808     C                   MOVEL     E14CAP        I95CAP
138580970808     C                   MOVEL     E14LOC        I95LOC
138590970808     C                   MOVEL     E14PRV        I95PRV
138600970808     C                   MOVEL     E14NAR        I95NAR
138610000623     C                   MOVEL     ARBDSP        I95DAT
138620020318     C     LNPNTW        IFEQ      'PPT'
138630000623     C                   MOVEL     ARBLNP        I95TFP
138640000623     C                   ELSE
138650980602     C                   MOVEL     ARBTFP        I95TFP
138660000623     C                   ENDIF
138670971230     C                   Z-ADD     ARBPKF        I95LKG
138680971230     C                   Z-ADD     ARBVLF        I95LMC
138690971230     C                   MOVEL     ARBFFD        I95FFD
138700971201     C                   MOVEL     FLGTB1        I95TPO
138710971230     C                   MOVEL     ARBTSP        I95TSP
138720980602     C                   MOVEL     'S'           I95FRE
138730971201     C     §3AFCA        IFNE      0
138740971201     C                   MOVEL     'S'           I95FCA
138750971201     C                   ENDIF
138760970808     C                   MOVEL     'S'           I13AF0
138770140902     c                   if        d48ffr<>'E'
138780970808     C                   MOVEL     'S'           I13CNV
138790140807     c                   endif
138800970808     C                   MOVEL     'S'           I13SZV
138810141119     c* in caso di chiamata cieca non è da impostare: non si deve aprire la int località
138820141119     c                   if        d48ffr<>'E'
138830970808     C                   MOVEL     'S'           I13AF1
138840141119     c                   else
138850141119     C                   MOVEL     ' '           I13AF1
138860141119     c                   endif
138870970808     C                   MOVEL     'S'           I13SZ2
138880970808     C*
138890970808     C* SE CAMBIATO CAP PULISCO CAMPI PER FORZATURE
138900970808     C* ** DESTINATARIO **
138910970808     C     WCLI          IFEQ      'D'
138920970808    2C     E14CAP        IFNE      VARCAD
138930970808     C                   CLEAR                   WZV
138940970808     C                   CLEAR                   WZ1
138950970808     C                   CLEAR                   WZ2
138960970808     C                   MOVEL     E14CAP        VARCAD
138970970808    2C                   ENDIF
138980970808     C* MODIFICA LA LOCALITA'
138990970808    2C     E14LOC        IFNE      VARLOD
139000970808     C                   MOVEL     E14LOC        VARLOD
139010970808     C                   CLEAR                   WZ1
139020970808     C                   CLEAR                   WZ2
139030970808    2C                   ENDIF
139040970808     C                   MOVEL     WZV           E13FZV
139050970808     C                   MOVEL     WZ1           E13FZ1
139060970808     C                   MOVEL     WZ2           E13FZ2
139070020305     c     lnantw        ifeq      'FED'
139080020305     c                   movel     'S'           i95fi1
139090020305     c                   endif
139100970808     C                   ELSE
139110970808     C* ** MITTENTE **
139120970808    2C     E14CAP        IFNE      VARCAM
139130970808     C                   CLEAR                   WZVM
139140970808     C                   CLEAR                   WZ1M
139150970808     C                   CLEAR                   WZ2M
139160970808     C                   MOVEL     E14CAP        VARCAM
139170970808    2C                   ENDIF
139180970808     C* MODIFICA LA LOCALITA'
139190970808    2C     E14LOC        IFNE      VARLOM
139200970808     C                   MOVEL     E14LOC        VARLOM
139210970808     C                   CLEAR                   WZ1M
139220970808     C                   CLEAR                   WZ2M
139230970808    2C                   ENDIF
139240020305     c     lnpntw        ifeq      'FED'
139250020305     c                   movel     'S'           i95fi1
139260020305     c                   endif
139270970808     C                   MOVEL     WZVM          E13FZV
139280970808     C                   MOVEL     WZ1M          E13FZ1
139290970808     C                   MOVEL     WZ2M          E13FZ2
139300970808     C                   END
139310970808     C*
139320970808     C**
139330970808     C** CALL
139340970808     C                   CALL      'FNLV13R'
139350970808     C                   PARM                    KPJBA
139360030612     C                   PARM                    Fnlv13DS
139370030612     C                   PARM                    Tisi95DS
139380970808     C*
139390970808     C                   MOVEL     O13ERR        WERR              1
139400970808     C                   MOVEL     O13MSG        VIDMSG
139410970808     C                   EXSR      CTRERR
139420970808     C*
139430970808     C* REIMPOSTO LE FORZATURE
139440970808     C     WCLI          IFEQ      'D'
139450970808     C                   MOVEL     E13FZV        WZV
139460970808     C                   MOVEL     E13FZ1        WZ1
139470970808     C                   MOVEL     E13FZ2        WZ2
139480970808     C                   ELSE
139490970808     C                   MOVEL     E13FZV        WZVM
139500970808     C                   MOVEL     E13FZ1        WZ1M
139510970808     C                   MOVEL     E13FZ2        WZ2M
139520970808     C                   END
139530150203     c* memorizzo la località normalizzata per destinatario
139540150203     c                   if        wcli='D'
139550150203     c                   eval      normlod=o95loc
139560150203     c                   endif
139570970808     C     ENDIND        ENDSR
139580070320     C*
139590070320     C*--- Gestisce record"L"di FIAR4 --------------------------------*
139600070320     C     GestAR4L      BEGSR
139610070320     c* lo faccio solo se GEO attiva
139620070320    0c                   if        FGSGEOT='S'
139630070320     c* Se devo chainare
139640070320     C                   MOVEL     'L'           WTRC
139650070320     c
139660070321    1c                   if        TipoEla='C'
139670070320     c                   clear                   Wesar4L
139680070320     C     KARB2         CHAIN     FiAR4000                           3091
139690070320    2c                   if        *in91
139700070320     C                   SETON                                        28
139710070320     C                   MOVEL     MSG(4)        VIDMSG
139720070320     C                   GOTO      ENDar4L
139730070320    2c                   endif
139740070320     c
139750070320    2c                   if        not *in30
139760070320     c                   movel     'E'           WEsar4L           1
139770070320     c                   movel     ar4not        dsbl4L
139780070320     c                   else
139790070320     c                   movel     'N'           WEsar4L
139800070320     c                   clear                   dsbl4L
139810070320     c                   clear                   fiar4000
139820070320    2c                   endif
139830070320    1c                   endif
139840070320     c
139850070320    1c                   if        tipoela='A'
139860070320     c                   movel     dsbl4l        ar4not
139870070320     c                   movel     dateu         ar4duv
139880070320     c                   movel     dateu         ar4DTR
139890070320     c                   movel     'T'           ar4ftr
139900070320     c
139910070320    3c                   if        wesar4L='N'
139920070320     c                   eval      ar4aas=arbaas
139930070320     c                   eval      ar4lnp=arblnp
139940070320     c                   eval      ar4nrs=arbnrs
139950070320     c                   eval      ar4nsp=arbnsp
139960070320     c                   eval      ar4trc='L'
139970070320     c                   write     fiar4000
139980070320   x3c                   else
139990070320     c                   update    fiar4000
140000070320    3c                   endif
140010070320    1c                   endif
140020070320     c
140030070320    0c                   endif
140040070320     c
140050070320     C     ENDar4L       ENDSR
140060970808     C*
140070970808     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
140080970808     C     CTRERR        BEGSR
140090970808     C* IMPOSTO CAMPI PASSATI
140100970808    1C     O13RON        IFEQ      'S'
140110970808     C                   MOVEL     O95NAR        E14NAR
140120150112     c                   if        d48ffr='E'
140130150112     c                   clear                   werr
140140150112     c                   eval      msgforzatxt=o13msg
140150150112     c                   endif
140160970808    1C                   ENDIF
140170970808    1C     O13ROC        IFEQ      'S'
140180970808     C                   MOVEL     O95CAP        E14CAP
140190150112     c                   if        d48ffr='E'
140200150112     c                   clear                   werr
140210150112     c                   eval      msgforzatxt=o13msg
140220150112     c                   endif
140230970808    1C                   ENDIF
140240970808    1C     O13ROP        IFEQ      'S'
140250970808     C                   MOVEL     O95PRV        E14PRV
140260150112     c                   if        d48ffr='E'
140270150112     c                   clear                   werr
140280150112     c                   eval      msgforzatxt=o13msg
140290150112     c                   endif
140300970808    1C                   ENDIF
140310970808    1C     O13ROL        IFEQ      'S'
140320970808     C                   MOVEL     O95LOC        E14LOC
140330150112     c                   if        d48ffr='E'
140340150112     c                   clear                   werr
140350150112     c                   eval      msgforzatxt=o13msg
140360150112     c                   endif
140370970808    1C                   ENDIF
140380970808     C* ERRORE O RICERCA CAMPO
140390970808    1C     WERR          IFNE      ' '
140400970808     C*
140410970808    2C     VIDMSG        IFNE      *BLANKS
140420970808     C                   SETON                                        28
140430970808    2C                   ENDIF
140440970808     C* POSIZIONAMENTO CURSORE
140450970808    2C                   SELECT
140460970808     C     WERR          WHENEQ    '1'                                          R.SOCIALE
140470970808     C                   SETON                                        40
140480970808     C     WERR          WHENEQ    '2'                                          INDIRIZZO
140490970808     C                   SETON                                        41
140500970808     C     WERR          WHENEQ    '5'                                          CAP
140510970808     C     WCLI          IFEQ      'D'
140520970808     C                   SETON                                        43
140530970808     C                   ELSE
140540970808     C                   SETON                                        42
140550970808     C                   END
140560970808     C     WERR          WHENEQ    '3'                                          LOCALITA
140570970808     C                   SETON                                        43
140580970808     C     WERR          WHENEQ    '4'                                          PROVINCIA
140590970808     C     WCLI          IFEQ      'D'
140600970808     C                   SETON                                        50
140610970808     C                   ELSE
140620970808     C                   SETON                                        45
140630970808     C                   END
140640970808     C     WERR          WHENEQ    '6'                                          NAZIONE
140650970808     C                   SETON                                        44
140660970808     C     WERR          WHENEQ    '7'                                          PARTITA IVA
140670970808     C     WCLI          IFEQ      'D'
140680970808     C                   SETON                                        45
140690970808     C                   ELSE
140700970808     C                   SETON                                        49
140710970808     C                   END
140720970808     C     WERR          WHENEQ    '8'                                          COD.ISO
140730970808     C     WCLI          IFEQ      'D'
140740970808     C                   SETON                                        46
140750970808     C                   ELSE
140760970808     C                   SETON                                        50
140770970808     C                   END
140780970808     C     WERR          WHENEQ    '9'                                          INOLTRO
140790970808     C     WCLI          ANDEQ     'D'
140800970808     C                   SETON                                        49
140810020318     c     werr          wheneq    '0'
140820020318     C     WCLI          IFEQ      'D'
140830020318     C                   SETON                                        50
140840020318     C                   ELSE
140850020318     C                   SETON                                        45
140860020318     C                   END
140870020318     c
140880970808    2C                   ENDSL
140890970808     C*
140900970808     C                   SETON                                        90
140910970808    1C                   ENDIF
140920970808     C                   ENDSR
140930091217     c*
140940091217     c* Aggiornamento per record GEN ddel fiar5 --------------------------------*
140950091217     c     RegisAR5GEN   BEGSR
140960091217     c                   clear                   dar5gen
140970091217     c                   Eval      kAr5Trd = 'GEN'
140980091217     c     kAr5          Chain     Fiar501l
140990091217      * Aggiorno Fiar5
141000091217if  1c                   If        %Found(Fiar501l)
141010091217     c                   movel     ar5uni        DAr5Gen
141020091217     c*
141030091217     c* Aggiorno tipo incasso in arrivo
141040091217    2c                   select
141050091217     c                   when      Wtipoagg='TIC'
141060091217     c                   Eval      §Ar5ticmb=vidtic
141070100119     c                   eval      §ar5flgMB='S'
141080091217     c
141090091217     c                   when      Wtipoagg='REF'
141100091217     c                   Eval      §Ar5Ref = VidNom
141110091217     c                   Eval      §Ar5Teld = VidTel
141120140403     c                   when      Wtipoagg='PIK'
141130140407     c                   Eval      §Ar5pk2 = wpk2
141140091217    2c                   ENDSL
141150091217
141160091217     c                   Movel(p)  DAr5Gen       Ar5Uni
141170091217     c
141180091217      * se devo trasmettere alla sede sfleggo per la trasmissione
141190091217    2c                   If        §Ar5Trse = 'S'
141200091217     c                   Clear                   Ar5Ft2
141210091217    2c                   EndIf
141220091217     c                   Clear                   Ar5Ft3
141230091217     c                   Update    Fiar5000
141240091217      * Scrivo Fiar5
141250091217   x1c                   Else
141260091217      * se immessi i dati
141270091217     c                   Clear                   Fiar5000
141280091217     c                   Eval      Ar5Aas = ArbAas
141290091217     c                   Eval      Ar5Lnp = ArbLnp
141300091217     c                   Eval      Ar5Nrs = ArbNrs
141310091217     c                   Eval      Ar5Nsp = ArbNsp
141320091217     c                   Eval      Ar5Trd = 'GEN'
141330091217     c                   Move      Dateu         Ar5Dac
141340091217     c                   Movel     W0140         Ar5Orc
141350091217     c                   Clear                   Dar5Gen
141360091217     c
141370091217    2c                   select
141380091217     c                   when      Wtipoagg='TIC'
141390091217     c                   Eval      §Ar5ticmb=vidtic
141400100119     c                   eval      §ar5flgMB='S'
141410091217     c
141420091217     c                   when      Wtipoagg='REF'
141430091217     c                   Eval      §Ar5Ref = VidNom
141440091217     c                   Eval      §Ar5Teld = VidTel
141450140403     c                   when      Wtipoagg='PIK'
141460140407     c                   Eval      §Ar5pk2 = wpk2
141470091217    2c                   ENDSL
141480091217
141490091217     c                   Movel(p)  DAr5Gen       Ar5Uni
141500091217    2c                   if        ar5uni<>*blanks
141510091217     c                   Write     Fiar5000
141520091217    2c                   EndIf
141530091217    1c                   EndIf
141540091217     c
141550091217     c                   ENDSR
141560091217     c
141570911212     C*--- ALLOCO OGGETTI --------------------------------------------*
141580911212     C     ALLOC         BEGSR
141590020805     C                   MOVE      ')'           VAR               7
141600020805     C                   MOVE      '))'          VAR2              8
141610020805     C                   Z-ADD     48            LUNG             15 5
141620920120     C*
141630920120     C                   MOVEA     SFILE         C20(13)
141640920120     C                   MOVEA     SFILE         C21(13)
141650920120     C                   MOVEA     SFILE         C22(13)
141660920120     C                   MOVEA     C22           SOVR(1)
141670920120     C                   MOVEA     C20           SALC(1)
141680920120     C                   MOVEA     C21           SDLC(1)
141690920120     C*
141700041027     C* solo PER SEDE
141710941212     C     §7LFFI        IFEQ      'S'
141720911212     C* CHECK OBJ E ADDPFM
141730971114     C                   Z-ADD     046           PARMBR            3 0
141740020805     c                   move      046           $mbr
141750930210     C                   MOVEL     SFILE         FISICO
141760930210     C                   CALL      'TRUL50C'
141770930210     C                   PARM                    FISICO           10
141780930210     C                   PARM                    MBRFIS           10
141790930210     C                   PARM                    LIB              10
141800930210     C                   PARM                    LOGICO           10
141810930210     C                   PARM                    MBRLOG           10
141820930210     C                   PARM                    ULFLG             1
141830911212     C** OVRDBF
141840020805     c                   movel     $mbr          var
141850920211     C                   MOVE      VAR           SOVR
141860911212     C                   MOVEL     *BLANKS       COMMAN           80
141870911218     C                   MOVEA     SOVR(1)       COMMAN
141880911212     C                   CALL      'QCMDEXC'
141890911212     C                   PARM                    COMMAN
141900911212     C                   PARM                    LUNG
141910911212     C* ALLOCO OGGETTO
141920020805     c                   movel     $mbr          var2
141930920211     C                   MOVE      VAR2          SALC
141940911212     C                   MOVEL     *BLANKS       COMMAN           80
141950911218     C                   MOVEA     SALC(1)       COMMAN
141960911213     C                   CALL      'QCMDEXC'                            91
141970911212     C                   PARM                    COMMAN
141980911212     C                   PARM                    LUNG
141990911212     C*
142000911212     C* 90 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
142010911213     C   91              GOTO      ENDALC
142020911212     C                   END
142030941212     C*
142040941212     C     ENDALC        TAG
142050941212     C* 91 ON - ERRORE
142060941212     C*
142070941212     C     *IN91         IFEQ      *ON
142080941212     C                   SETON                                        28
142090051129     C                   MOVEa     MSG(136)      k78
142100141110     C                   MOVEa     SFILE         k78(68)
142110051129     C                   MOVEa     k78           vidmsg
142120941212     C                   ENDIF
142130941212     C*
142140941212     C                   ENDSR
142150920831     C*
142160940321     C*--- AGGIORNO FILE TASSAZIONE PADRONCINI -----------------------*
142170950113     C     AGGPDS        BEGSR
142180041008     C                   MOVEL     ' '           UNO               1
142190041008     c* richiamo pgm fnlv57ds
142200041008     c                   clear                   fnlv57ds
142210041008     C* 01 ON  - VARIAZIONE DESTINATARIO
142220041008    1c     *in01         ifeq      *on
142230041008    2C     ARBRSD        IFNE      OLDRSD
142240041008     C     ARBCAD        ORNE      OLDCAD
142250041008     C     ARBFIN        ORNE      OLDFIN
142260041008     c                   eval      uno='1'
142270041008    2c                   endif
142280041008    1c                   endif
142290041008     c
142300041008    1c                   if        *in02
142310041008    2c     ARBVLF        IFNE      OLDVLF
142320041008     C     ARBPKF        orne      OLDPKF
142330041008     c                   eval      uno='1'
142340041008    2c                   endif
142350041008    1c                   endif
142360041008     c*
142370041008    1c                   if        uno='1'
142380041008     c                   eval      i57aas=arbaas
142390041008     c                   eval      i57lnp=arblnp
142400041008     c                   eval      i57nrs=arbnrs
142410041008     c                   eval      i57nsp=arbnsp
142420041008     c
142430041008     c                   select
142440041008     c* Se presenti entrambe le bolle
142450041008     c                   when      waltrabolla=' '
142460070320     c                   eval      i57drt=arbdrt
142470070320     c                   eval      i57nrt=arbnrt
142480070320     c                   eval      i57fpp=arbfpp
142490070320     c                   eval      i57prt=arbpdr
142500070320     c                   eval      i57pco=arbpdc
142510041008     c* modifica in arrivo non presente fnblp
142520041008     c                   when      *in10 and waltrabolla='N'
142530041008     c                   eval      i57pdr=arbpdc
142540041008     c                   eval      i57tbo='A'
142550041008     c* modifica in partenza non presente fnarb
142560041008     c                   when      not *in10 and waltrabolla='N'
142570070320     c                   eval      i57pdr=arbpdr
142580041008     c                   eval      i57tbo='P'
142590070320     c                   eval      i57drt=arbdrt
142600070320     c                   eval      i57nrt=arbnrt
142610070320     c                   eval      i57fpp=arbfpp
142620041008     c                   endsl
142630041008     c
142640041008     c                   eval      i57ncl=arbncl
142650041011     c   02              eval      i57tvl='F'
142660041008     c* volume vecchio
142670041008     c                   eval      i57vlc=arbvlc
142680041008     c                   eval      i57ncr=arbncr
142690041008     c                   eval      i57vlf=oldvlf
142700041008     c* volume nuovo
142710041008     c                   eval      i57vcn=arbvlc
142720041008     c                   eval      i57ncn=arbncp
142730041008     c                   eval      i57vfn=arbvlf
142740041008     c* peso vecchio
142750041008     c                   eval      i57pkc=arbpkc
142760041008     c                   eval      i57ncp=arbncp
142770041008     c                   eval      i57pkf=oldpkf
142780041008     c* peso nuovo
142790041008     c                   eval      i57pcn=arbpkc
142800041008     c                   eval      i57npn=arbncp
142810041008     c                   eval      i57pfn=arbpkf
142820041008     c* destinatario vecchio
142830041008     c                   eval      i57rsd=oldrsd
142840041008     c                   eval      i57cad=oldcad
142850041008     c                   eval      i57fin=oldfin
142860041008     c* destinatario nuovo
142870041008     c                   eval      i57rdn=arbrsd
142880041008     c                   eval      i57cdn=arbcad
142890041008     c                   eval      i57fdn=arbfin
142900041008     c
142910041008     c                   call      'FNLV57R'
142920041008     c                   parm                    kpjba
142930041008     c                   parm                    fnlv57ds
142940041008     c                   endif
142950940321     C*
142960940321     C                   ENDSR
142970950113     C*
142980950113     C* AGGIORNA PADRONCINI PER IL MITTENTE --------------------------*
142990950113     C     AGGPMT        BEGSR
143000950113     C*
143010950113    1C     ARBKSC        IFNE      OLDKSC
143020950113     C     ARBRSM        ORNE      OLDRSM
143030950113     C     ARBCAM        ORNE      OLDCAM
143040950113     C*
143050950113     C* CONTROLLO CON QUALE VOLUME HO AGGIORNATO LA BOLLA
143060950113     C                   CLEAR                   WVOL
143070950113     C*
143080950113     C* VOLUME PARZIALE "Z"
143090950113     C*
143100050111     C     ARBNCR        ifne      ARBNCL
143110950113     C*
143120950113    3C     ARBVLC        IFGE      ARBVLF
143130950113     C                   Z-ADD     ARBVLC        WVOL
143140950113   X3C                   ELSE
143150950113     C                   Z-ADD     ARBVLF        WVOL
143160950113    3C                   ENDIF
143170950113     C* VOLUME TOTALE "T"
143180950113   X2C                   ELSE
143190950113     C                   Z-ADD     ARBVLC        WVOL
143200950113    2C                   ENDIF
143210950113     C****
143220950113     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
143230011126     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
143240011126     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
143250950113     C****
143260950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
143270950113     C                   SETOFF                                       17
143280950113     C*
143290070320     C                   MOVE      arbPDR        WPDR
143300070320     C                   Z-ADD     arbDRT        WDRT
143310950113     C* NUMERO DISTINTA
143320070320    2C     arbFPP        IFEQ      'M'
143330950113     C                   Z-ADD     1000          WNDC
143340950113     C                   ELSE
143350950113     C                   Z-ADD     5000          WNDC
143360950113    2C                   ENDIF
143370070320     C                   ADD       arbNRT        WNDC
143380950113     C*
143390950113     C*
143400950113    2C                   DO        2
143410011126     C   17KFTT2         CHAIN     fifst000                           30
143420011126     C  N17KFTT2         CHAIN     fiftt000                           30
143430950113     C*
143440950113    3C     *IN30         IFEQ      *OFF
143450950113     C*
143460950113     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
143470950113    4C     FTTFVL        IFNE      'C'
143480020326     c*
143490020326     c  n17kftd5         chain     fiftd005                           30
143500020326     c   17kftd5         chain     fifsd005                           30
143510020326     c     *in30         ifeq      *off
143520020326     c                   z-add     arbpkf        ftdpkl
143530020326     c                   z-add     wvol          ftdvlu
143540020326     c                   movel     arbrsm        ftdrsc
143550020326     c* dò per scontato che la bolla sia un porto franco
143560020326     c                   z-add     arbksc        ftdksc
143570020326     c                   movel     arbcam        ftdcap
143580020326     c                   movel     arbfap        ftdfin
143590020326     c* se è stato modificato codice cliente o ragione sociale devo
143600020326     c* aggiornare anche il numero di stop
143610020327    1C     ARBKSC        IFNE      OLDKSC
143620020327     C     ARBRSM        ORNE      OLDRSM
143630020327     c                   exsr      aggstp
143640020327     c                   endif
143650020327     c  n17              update    fiftd005
143660020327     c   17              update    fifsd005
143670020326     c                   endif
143680950113     C*
143690950113     C* SE VALORIZZATA SVALORIZZO
143700950113     C                   EXSR      SVAFTT
143710950113     C*
143720950113    4C                   ENDIF
143730950113    3C                   ENDIF
143740950113     C*
143750950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
143760950113     C                   SETON                                        17
143770950113    2C                   ENDDO
143780950113     C*
143790950113    1C                   ENDIF
143800950113     C*
143810950113     C                   ENDSR
143820020327     c*
143830020327     c* AGGSTP - Aggiornamento numero stop a cambio di mittente ------*
143840020327     C     AGGSTP        BEGSR
143850020327     c                   clear                   wstp
143860020327     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
143870020327     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
143880020327     C                   Z-ADD     ftdksc        w0040             4 0
143890020327    5C     w0040         IFNE      *ALL'8'
143900020327     C     w0040         ANDNE     *ALL'9'
143910020327     C     ftdksc        CHAIN     FNCLS000                           31
143920020327     C  N31              z-add     CLSSTP        wstp
143930020327    5C                   ENDIF
143940020327     c     wstp          ifeq      *zeros
143950020327     c* prima di fare la chain mi devo salvare i dati del record di fiftd005
143960020327     c                   movel     dsftd         sav_dsftd
143970020327     C                   MOVEL     ARBKSC        WKSC
143980020327     C                   MOVEL     ARBRSM        WRSM
143990020327     C  N17KFTD          CHAIN(N)  fiftd004                           31
144000020327     C   17KFTD          CHAIN(N)  fifsd004                           31
144010020327     c  n31              z-add     ftdstp        wstp
144020020327     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
144030020327     c* nella distinta il primo numero disponibile
144040020327     c     wstp          ifeq      *zeros
144050020327     C  N17KFTT2         setgt     fiftd001
144060020327     C   17KFTT2         setgt     fifsd001
144070020327     C  N17KFTT2         readpe    fiftd001
144080020327     C   17KFTT2         readpe    fifsd001
144090020327     c                   if        not %eof
144100020327     c                   eval      wstp = (ftdstp+1)
144110020417     c     wstp          iflt      1000
144120020417     c                   z-add     1000          wstp
144130020417     c                   endif
144140020417     c                   else
144150020417     c* qui non dovrebbe mai entrare ma lo gestisco comunque anche
144160020417     c* per unifomrità con fnlsa2r
144170020417     c                   z-add     1000          wstp
144180020327     c                   endif
144190020327     c                   endif
144200020327     c*
144210020327     c* ripristino i dati del record salvato
144220020327     c                   movel     sav_dsftd     dsftd
144230020327     c                   endif
144240020327     c                   z-add     wstp          ftdstp
144250020327     C                   ENDSR
144260020326     c*
144270950113     C* SVALORIZZA UNA DISTINTA --------------------------------------*
144280950113     C     SVAFTT        BEGSR
144290950113     C* SOLO SE VALORIZZATA
144300950113    6C     FTTFVL        IFEQ      'V'
144310950113     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
144320950113     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
144330950113     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
144340950113     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
144350950113     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
144360950113     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
144370950113     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
144380041122     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
144390041122     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
144400950113     C                   MOVEL     PARAM3        KPJBU
144410011129     C  N17              CALL      'FICNE2R'
144420950113     C                   PARM                    KPJBA
144430011129     C   17              CALL      'FICNE9C'
144440950113     C                   PARM                    KPJBA
144450950113    6C                   ENDIF
144460950113     C                   ENDSR
144470941102     C*
144480941102     C* CONTROLLO DIVISA ---------------------------------------------*
144490941102     C     CTRVLT        BEGSR
144500011011      *
144510011011      * La divisa è obbligatoria se è inserito un importo
144520011011     C     W0133         IFGT      *ZEROS
144530011011     C     WVLT          ANDEQ     *BLANKS
144540011011     C                   MOVEL     MSG(100)      VIDMSG
144550011011     C                   SETON                                        9028
144560011011     C                   GOTO      ENDVLT
144570011011     C                   ENDIF
144580941102     C*
144590941102     C                   MOVEL     'CV'          COD
144600941102     C* RICERCA
144610941102     C     '?'           SCAN      WVLT                                   90
144620941102     C     *IN90         IFEQ      *ON
144630991011     C                   CLEAR                   §KEY
144640941102     C                   CALL      'X§TABER'
144650941102     C                   PARM                    CODUT
144660941102     C                   PARM                    COD
144670941102     C                   PARM                    §KEY
144680991011     C                   PARM                    §DES             25
144690941102     C                   MOVEL     §KEY          WVLT
144700941102     C                   GOTO      ENDVLT
144710941102     C                   ENDIF
144720941102     C*
144730941102     C* CONTROLLO DIVISA
144740941102     C                   MOVEL(P)  WVLT          KEY
144750941213     C                   EXSR      CTABEL
144760941102     C* ERRORE
144770941102    1C     *IN30         IFEQ      *ON
144780941102     C                   MOVEL     MSG(26)       VIDMSG
144790941102     C                   SETON                                        9028
144800941102     C                   GOTO      ENDVLT
144810941102    1C                   ENDIF
144820941102     C                   MOVEL     TBLUNI        DSCV
144830941102     C*
144840941102     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
144850941102    1C     §CVFDC        IFNE      'S'
144860941102     C                   MOVE      W0133         W0033             3 3
144870941102    2C     W0033         IFNE      0
144880941102     C                   MOVEL     MSG(27)       VIDMSG
144890941102     C                   SETON                                        9028
144900941102     C                   GOTO      ENDVLT
144910941102    2C                   ENDIF
144920941102    1C                   ENDIF
144930011010      *
144940011010      * Controllo il numero di decimali immessi
144950011011    1C     §CVFDC        IFEQ      'S'
144960011011     C     W0133         ANDNE     *ZEROS
144970011011      *
144980011011    2C                   SELECT
144990011011     C     §CVDEC        WHENEQ    3
145000011011     C     §CVDEC        WHENEQ    2
145010011011     C                   MOVE      W0133         W0010             1 0
145020011011    3C     W0010         IFNE      *ZEROS
145030011011     C                   MOVEA     MSG(93)       K78
145040011011     C                   MOVE      §CVDEC        WCVDEC            2
145050011011     C                   MOVEA     WCVDEC        K78(29)
145060011011     C                   MOVEA     K78           VIDMSG
145070011011     C                   SETON                                        9028
145080011011    3C                   ENDIF
145090011011     C     §CVDEC        WHENEQ    1
145100011011     C                   MOVE      W0133         W0020             2 0
145110011011    3C     W0020         IFNE      *ZEROS
145120011011     C                   MOVEA     MSG(93)       K78
145130011011     C                   MOVE      §CVDEC        WCVDEC            2
145140011011     C                   MOVEA     WCVDEC        K78(29)
145150011011     C                   MOVEA     K78           VIDMSG
145160011011     C                   SETON                                        9028
145170011011    3C                   ENDIF
145180011011     C     §CVDEC        WHENEQ    0
145190011011    2C                   ENDSL
145200011011      *
145210011011    1C                   ENDIF
145220941102     C*
145230941102     C     ENDVLT        ENDSR
145240941212     c*
145250941212     C*--- CHAIN TABEL00F --------------------------------------------*
145260941212     C     CTABEL        BEGSR
145270941212     C     KTAB2         CHAIN     TABEL                              30
145280941212     C  N30TBLFLG        IFNE      ' '
145290941212     C                   SETON                                        30
145300941212     C                   END
145310941212     C                   ENDSR
145320941212     C*---------------------------------------------------------------
145330941212     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
145340941212     C*---------------------------------------------------------------
145350941212     C     DLCSED        BEGSR
145360020805     c                   move      046           $mbr
145370020805     c                   movel     $mbr          var2
145380941220     C                   MOVE      VAR2          SDLC(1)
145390941212     C                   MOVEL     *BLANKS       COMMAN           80
145400941212     C                   MOVEA     SDLC(1)       COMMAN
145410941212     C                   CALL      'QCMDEXC'                            91
145420941212     C                   PARM                    COMMAN
145430941212     C                   PARM                    LUNG
145440941212     C                   ENDSR
145450941213     C*---------------------------------------------------------------
145460941213     C* STORICIZZO LA TASSAZIONE
145470941213     C*---------------------------------------------------------------
145480941213     C     STOARB        BEGSR
145490991006     C* PER LA PRIMA BOLLA KSC E CTR LI PRENDO SEMPRE DALLA BOLLA
145500991006     C* PRINCIPALE PERCHE' SONO SICURA DI AVERLA
145510991006     C* PER LA SECONDA BOLLA LI DEVO PRENDERE DA AR6 PERCHE'
145520991006     C* QUELLI DELLA BOLLA PRINCIPALE SI RIFERISCONO ALLA PRIMA BOLLA
145530991006     C     AR6TRC        IFEQ      '2'
145540991006     C                   Z-ADD     AR6KSC        ARBKSC
145550991006     C                   Z-ADD     AR6CTR        ARBCTR
145560991006     C                   ENDIF
145570991006     C                   MOVEL     AR6DIV        ARBDIV
145580941213     C                   MOVEL     AR6POR        ARBPOR
145590941213     C                   MOVEL     AR6SV1        ARBSV1
145600941213     C                   MOVEL     AR6VA1        ARBVA1
145610941213     C                   MOVEL     AR6SV2        ARBSV2
145620941213     C                   MOVEL     AR6VA2        ARBVA2
145630941213     C                   MOVEL     AR6SV3        ARBSV3
145640941213     C                   MOVEL     AR6VA3        ARBVA3
145650941213     C                   MOVEL     AR6IMV        ARBIMV
145660941213     C                   MOVEL     AR6ALI        ARBALI
145670941213     C                   MOVEL     AR6CEI        ARBCEI
145680941213     C                   MOVEL     AR6IFT        ARBIFT
145690941213     C                   MOVEL     AR6DFT        ARBDFT
145700941213     C                   MOVEL     AR6NFT        ARBNFT
145710941213     C                   MOVEL     AR6FIV        ARBFIV
145720950114     C* SE VENGO DA VARIAIZONE MITTENTE COME CODICE TARIFFA CI METTO
145730950114     C*  QUELLO SALVATO
145740950114     C   24              MOVEL     MITCTR        ARBCTR
145750991013     C                   MOVEL     AR6FIM        ARBFIM
145760011108      *
145770011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
145780011108      * è in partenza 'P' o in arrivo 'A'
145790011108     C     *IN10         IFEQ      *ON
145800011108     C                   MOVE      'A'           ARBPRU
145810011108     C                   ENDIF
145820011108     C     *IN10         IFEQ      *OFF
145830011108     C                   MOVE      'P'           ARBPRU
145840011108     C                   ENDIF
145850011108      *
145860950114     C*
145870991006     C                   WRITE     FIARBT00
145880950114     C*
145890991006     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
145900941221    1C     WAR7          IFEQ      'E'
145910991006    1C     ARBTRC        ANDEQ     '1'
145920991006     C                   MOVEL     '1'           WTRC
145930991006     C     KARB2         SETLL     FIAR7000
145940991006     C     KARB2         READE(N)  FIAR7000                               30
145950941213     C*
145960941213    2C     *IN30         DOWEQ     *OFF
145970941213     C                   MOVEL     AR7SVN        ARBSVN
145980941213     C                   MOVEL     AR7VAN        ARBVAN
145990991006     C                   WRITE     FIARBU00
146000941213     C*
146010991006     C     KARB2         READE(N)  FIAR7000                               30
146020941213    2C                   ENDDO
146030941213     C                   ENDIF
146040041130      *-------
146050041130      * Seconda bolla
146060041130     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
146070041130    1C     WAR72         IFEQ      'E'
146080041130    1C     ARBTRC        ANDEQ     '2'
146090041130     C                   MOVEL     '2'           WTRC
146100041130     C     KARB2         SETLL     FIAR7000
146110041130     C     KARB2         READE(N)  FIAR7000                               30
146120041130     C*
146130041130    2C     *IN30         DOWEQ     *OFF
146140041130     C                   MOVEL     AR7SVN        ARBSVN
146150041130     C                   MOVEL     AR7VAN        ARBVAN
146160041130     C                   WRITE     FIARBU00
146170041130     C*
146180041130     C     KARB2         READE(N)  FIAR7000                               30
146190041130    2C                   ENDDO
146200041130     C                   ENDIF
146210041130      *-------
146220941213     C*
146230941213     C                   ENDSR
146240941214     C*----------------------------------------------------------------
146250941214     C* RICALCOLO IL VOLUME AUTOMATICO
146260941214     C*----------------------------------------------------------------
146270941214     C     VOLAUT        BEGSR
146280941214     C*
146290941214     C* VOLUME AUTOMATICO A PESO: PESO MODIFICATO
146300941214    3C     VIDPKF        IFNE      ARBPKF
146310941214     C     ARBFVF        ANDEQ     'K'
146320941214     C*
146330120702     C     ARBPKF        DIV(H)    ARBVLF        CLIKPM            9 1
146340941214     C     VIDPKF        DIV(H)    CLIKPM        D20VLU
146350941214     C*
146360941214     C     ARBVLB        IFEQ      0
146370941214     C                   Z-ADD     0,001         D20VLU
146380941214     C                   END
146390941214     C                   MOVEL     ARBFVF        D20FVL
146400941214     C*
146410941214    3C                   END
146420941214     C                   ENDSR
146430941215     C**************************************************************************
146440941215     C*  ABBLANCA ZERO NON SIGNIFICATIVI
146450941215     C**************************************************************************
146460941215     C     ABBL          BEGSR
146470941215     C*
146480941215     C                   SETOFF                                       99
146490941215     C     SKI(I)        DOWEQ     '0'
146500941215     C                   MOVEL     ' '           SKI(I)
146510941215     C                   ADD       1             I
146520941215     C     I             COMP      15                                     99
146530941215     C  N99              END
146540941215     C*
146550941215     C                   ENDSR
146560051114     C**************************************************************************
146570051114     C*  ABBLANCA ZERO NON SIGNIFICATIVI
146580051114     C**************************************************************************
146590051114     C     SR_chkcomb    BEGSR
146600051114     c                   clear                   wcomb             1
146610051114     C* Non è possibile indicare contemporaneamente consegne particolari
146620051117     C*  'S' ed 'X' / 'S' ed 'A' / "S" e "P" / "X" e "P"
146630051114    2C                   IF           (wTC1 = 'X'  and  wTC2 = 'S')
146640051114     C                             or (wTC1 = 'S'  and  wTC2 = 'X')
146650051114     C                             or (wTC1 = 'A'  and  wTC2 = 'S')
146660051114     C                             or (wTC1 = 'S'  and  wTC2 = 'A')
146670051114     C                             or (wTC1 = 'S'  and  wTC2 = 'P')
146680051114     C                             or (wTC1 = 'P'  and  wTC2 = 'S')
146690051117     C***                          or (wTC1 = 'X'  and  wTC2 = 'A')
146700051117     C***                          or (wTC1 = 'A'  and  wTC2 = 'X')
146710051114     C                             or (wTC1 = 'X'  and  wTC2 = 'P')
146720051114     C                             or (wTC1 = 'P'  and  wTC2 = 'X')
146730051114     c                   eval      wcomb = 'S'
146740051114     c                   endif
146750051114     C                   ENDSR
146760060227     C**************************************************************************
146770060301     C*  Richiamo pgm di richiesta/controllo motivazione variazione
146780060227     C**************************************************************************
146790060227     C     ges_mtv       BEGSR
146800060227     c                   clear                   fnlv85ds
146810060227     c                   movel     'V'           ilv85tel
146820060227     c                   if        d48ffr = 'S' or d48ffr = 'V'
146830060227     c                   clear                   ilv85wdw
146840060227     c                   else
146850060301     c                   if        d48ffr = *blanks
146860060301     c                   movel     'E'           ilv85wdw
146870060301     c                   else
146880060227     c                   movel     'N'           ilv85wdw
146890060301     c                   endif
146900060227     c                   endif
146910060227     c                   movel     d48cvb        ilv85cvb
146920060227     c                   if        d48mtv <> *blanks
146930060227     c                   movel     d48mtv        ilv85mtv
146940060227     c                   movel     d48not        ilv85nt1
146950060227     c                   move      d48not        ilv85nt2
146960060227     c                   endif
146970060227     c                   movel     fnlv85ds      kpjbu
146980060302     c                   call      'FNLV85R'
146990060227     c                   parm                    kpjba
147000060227     c                   movel     kpjbu         fnlv85ds
147010060227     c                   if        olv85err = '1'
147020060227     c                   seton                                        90
147030060227     C                   eval      vidmsg='Motivazione variazione mancante o er-
147040060227     c                             rata'
147050060227     c                   endif
147060060301     c                   if        olv85f12 = ' ' and olv85err = ' '
147070060301     c                   movel     'N'           w48mct
147080060301     c                   endif
147090060227     c                   endsr
147100060301     C**************************************************************************
147110060301     C*  Richiamo pgm per storicizzazione motivazione variazione
147120060301     C**************************************************************************
147130060301     C     sto_mtv       BEGSR
147140060303     c                   clear                   fnlv85ds
147150060301     c                   movel     ' '           ilv85tla
147160060301     c                   movel     'R'           ilv85tel
147170060301     c                   movel     d48cvb        ilv85cvb
147180060301     c                   z-add     arbdtv        ilv85dac
147190060301     c                   z-add     arborv        ilv85orc
147200060301     c                   z-add     arbaas        ilv85aas
147210060301     c                   z-add     arblnp        ilv85lnp
147220060301     c                   z-add     arbnrs        ilv85nrs
147230060301     c                   z-add     arbnsp        ilv85nsp
147240060303     c                   if        d48mtv <> *blanks and
147250060303     c                             (d48ffr = 'E' or d48mct = 'N')
147260060303     c                   movel     d48mtv        ilv85mtv
147270060303     c                   movel     d48not        ilv85nt1
147280060303     c                   move      d48not        ilv85nt2
147290060303     c                   endif
147300060301     c                   movel     fnlv85ds      kpjbu
147310060301     c                   call      'FNLV85R'
147320060301     c                   parm                    kpjba
147330060301     c                   endsr
147340120330     C**************************************************************************
147350120330     C*  AGGIORNAMENTO DA MANUTENZIONE BOLLE DI SEDE (CAUS.VAR.BOLLA "SD")
147360120330     C**************************************************************************
147370120330     C     GESSD         BEGSR
147380120330     c                   clear                   errp
147390120330     c                   clear                   erra
147400120419     c                   clear                   savcpi_p
147410120419     c                   clear                   savksc_p
147420120419     c                   clear                   savctr_p
147430120419     c                   clear                   savcpi_a
147440120419     c                   clear                   savksc_a
147450120419     c                   clear                   savctr_a
147460120330     c* chaino le bolle in partenza e in arrivo
147470120330     C     KARB          CHAIN(e)  FNBLP01L
147480120402    1c                   if        %error
147490120330     c                   eval      errp='A'
147500120402   x1c                   else
147510120402    2c                   if        not %found(fnblp01l)
147520120330     c                   eval      errp='N'
147530120418     c                   else
147540120418     c                   eval      savcpi_p=arbcpi
147550120418     c                   eval      savksc_p=arbksc
147560120418     c                   eval      savctr_p=arbctr
147570120402    2c                   endif
147580120402    1c                   endif
147590120330     C     KARB          CHAIN(e)  FNARB01L
147600120402    1c                   if        %error
147610120330     c                   eval      erra='A'
147620120402   x1c                   else
147630120402    2c                   if        not %found(fnarb01l)
147640120330     c                   eval      erra='N'
147650120413     c                   else
147660120413     c                   eval      savcpi_a=arbcpi
147670120418     c                   eval      savksc_a=arbksc
147680120418     c                   eval      savctr_a=arbctr
147690120402    2c                   endif
147700120402    1c                   endif
147710120402     c* Una delle due bolle allocate --> restituisco messaggio di avviso
147720120402    1c                   if        errp='A' or erra='A'
147730120402     c                   eval      d48err='A'
147740120402     c                   eval      d48msg='Bolla di filiale allocata da altro l+
147750120418     c                             avoro:NON AGGIORNATA in'
147760120418    2c                   if        errp='A'
147770120418     c                   eval      d48msg=%trim(d48msg)+' Partenza'
147780120418    3c                   if        erra='A'
147790120418     c                   eval      d48msg=%trim(d48msg)+' e'
147800120418    3c                   endif
147810120418    2c                   endif
147820120418    2c                   if        erra='A'
147830120418     c                   eval      d48msg=%trim(d48msg)+' Arrivo'
147840120418    2c                   endif
147850120402    1c                   endif
147860120330     c* se entrambe le bolle allocate/inesistenti vado a fine
147870120402    1c                   if        errp<>*blanks and erra<>*blanks
147880120330     c                   leavesr
147890120402    1c                   endif
147900120404     c*Variazione di PESO
147910120404    1c                   if        §bdpkf<>arbpkf
147920120402     c                   z-add     §bdpkf        arbpkf
147930120404    1c                   endif
147940120404     c*Variazione di VOLUME: il volume, in sede, può essere variato solo da
147950120404     c* profilo di sede. Siccome in questi casi la variazione avviene solitamente
147960120404     c* quando in filiale la bolla non c'è più sostituisco sempre
147970120404    1c                   if        §bdvlf<>arbvlf or §bdfvf<>arbfvf
147980120404     c                   z-add     §bdvlf        arbvlf
147990120411     c                   movel     §bdfvf        arbfvf
148000120404    1c                   endif
148010120529     c* IMOPORTO DA ASSICURARE
148020120529     c                   if        §btias<>arbias or §btvas<>arbvas
148030120529     c                   z-add     §btias        arbias
148040120529     c                   movel     §btvas        arbvas
148050120529     c                   endif
148060120404     c* Variazione TASSAZIONE: vengono caricate solo se c'è corrispodenza
148070120404     c* fra sede e filiale al livello di tipo di fattura: Segue Fattura o
148080120404     c* Fattura Immediata
148090120411     c* Chaino fiar6 per vedere se S.F. o  F.I.:
148100120404     c                   clear                   wfatfil
148110120404     c                   clear                   ar6ksc
148120120411     c                   clear                   war6
148130120416     c                   clear                   fiar6000
148140120416     C                   MOVEL     §bttrc        WTRC
148150120404     c     karb2         chain     fiar601l
148160120411    1c                   if        %found(fiar601l) and ar6nft= 0 or
148170120411     c                             not %found(fiar601l)
148180120404     c                   eval      wfatfil='SF'
148190120411     c                   if        not %found(fiar601l)
148200120411     c                   eval      war6='N'
148210120411     c                   endif
148220120404     c                   else
148230120404     c                   eval      wfatfil='SD'
148240120404    1c                   endif
148250120404    1c                   if        d48cvb=wfatfil
148260120404     c
148270120404     c* . . . . . . . QUANTITA' DA FATTURARE
148280120411    2c                   if        §btqft<>arbqft
148290120411     c                   z-add     §btqft        arbqft
148300120404    2c                   endif
148310120404     c* . . . . . . . CLIENTE DI FATTURAZIONE e COD.TARIFFA
148320120419     C*
148330120404     C                   MOVEL     '3A'          COD
148340120404     C                   MOVEL(P)  ARBCBO        KEY
148350120404     C                   EXSR      CTABEL
148360120404     C  N30              MOVEL     TBLUNI        DS3A
148370120404     C   30              CLEAR                   DS3A
148380120413     c* assegnato
148390120404    2c                   if        §bttrc='2' or %subst(§3atb1:1:1)='A'
148400120404    3c                   if        §btksc<>ar6ksc
148410120404     c                   eval      ar6ksc=§btksc
148420120404    3c                   endif
148430120404    3c                   if        §btctr<>ar6ctr
148440120404     c                   eval      ar6ctr=§btctr
148450120404    3c                   endif
148460120418     c* Se assegnato no seconda bolla devo aggiornare su arb anche il ksc/ctr
148470120418    2c                   if        %subst(§3atb1:1:1)='A'
148480120418    3c                   if        §btksc<>savksc_a
148490120418     c                   eval      savksc_a=§btksc
148500120418    3c                   endif
148510120418    3c                   if        §btctr<>savctr_a
148520120418     c                   eval      savctr_a=§btctr
148530120418    3c                   endif
148540120418     c                   endif
148550120404   x2c                   else
148560120413     c* franco
148570120413     c*  se non codificato non faccio niente
148580120413     c                   z-add     §btksc        w0040
148590120418    3c                   if        w0040<>8888 and §btksc<>arbksc
148600120413     c                   exsr      gessd_ksc
148610120411    3c                   endif
148620120411    3c                   if        §btctr<>arbctr
148630120411     c                   eval      arbctr=§btctr
148640120411    3c                   endif
148650120404    2c                   endif
148660120404     c* . . . . . . . CAMPI DI TASSAZIONE
148670120404     c* . . . . Divisa
148680120404     c                   if        §btdiv<>ar6div
148690120404     c                   eval      ar6div=§btdiv
148700120404     c                   endif
148710120404     c* . . . . Porto
148720120404     c                   if        §btpor<>ar6por
148730120404     c                   eval      ar6por=§btpor
148740120404     c                   endif
148750120411     c* . . . . Imponibile IVA
148760120411     c                   if        §btimv<>ar6imv
148770120411     c                   eval      ar6imv=§btimv
148780120411     c                   endif
148790120411     c* . . . . Causale esenzione
148800120411     c                   if        §btcei<>ar6cei
148810120411     c                   eval      ar6cei=§btcei
148820120411     c                   endif
148830120404     c* . . . . Varie - In §SV e §VA ci sono le varie ricevute dal chiamante
148840120404     c*                 IN WSV e WVA carico le varie presenti sul fiar6/7
148850120411     C                   MOVEL     §sv(1)        ar6sv1
148860120411     C                   Z-ADD     §va(1)        ar6va1
148870120411     C                   MOVEL     §sv(2)        ar6sv2
148880120411     C                   Z-ADD     §va(2)        ar6va2
148890120411     C                   MOVEL     §sv(3)        ar6sv3
148900120411     C                   Z-ADD     §va(3)        ar6va3
148910120411     c* cancello varie su fiar7 se presenti
148920120419     C                   MOVEL     §bttrc        WTRC
148930120411     c     karb2         setll     fiar7000
148940120411     c     karb2         reade     fiar7000
148950120411     c                   dow       not %eof(fiar701l)
148960120411     C                   DELETE    FIAR7000
148970120411     c     karb2         reade     fiar7000
148980120411     c                   enddo
148990120411     c* se presenti più di 3 verie le memorizzo nell'fiar7
149000120411    2c                   if        §sv(4)<>' '
149010120411     c                   z-add     4             inx
149020120411     c                   clear                   AR7ATB
149030120411     c                   z-add     d48aas        AR7AAS
149040120411     c                   z-add     d48lnp        AR7LNP
149050120411     c                   z-add     d48nrs        AR7NRS
149060120411     c                   z-add     d48nsp        AR7NSP
149070120419     c                   movel     §bttrc        AR7TRC
149080120411     c                   dow       §sv(inx)<>*blanks or inx>20
149090120411     C                   MOVEL     §sv(inx)      ar7svn
149100120411     C                   z-add     §va(inx)      ar7van
149110120411     C                   write     fIAR7000
149120120411     c                   add       1             inx
149130120411     c                   enddo
149140120411    2C                   endif
149150120416     c* Aggiornamento FIAR6
149160120416     c* se record non esiste lo creo solo se è pieno almeno uno dei campi
149170120416    2c                   if        war6='N'
149180120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
149190120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
149200120416     c                             or ar6cei<>*blanks
149210120411     c                   eval      AR6AAS=d48aas
149220120411     c                   eval      AR6LNP=d48lnp
149230120411     c                   eval      AR6NRS=d48nrs
149240120411     c                   eval      AR6NSP=d48nsp
149250120411     c                   eval      AR6TRC=§bttrc
149260120411
149270120411     c                   write     fiar6000
149280120416    3c                   endif
149290120417     c                   else
149300120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
149310120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
149320120416     c                             or ar6cei<>*blanks
149330120404     c                   except    ar6sd
149340120416     c                   else
149350120416     c                   delete    fiar6000
149360120416     c                   endif
149370120416    2c                   endif
149380120404   x1c                   else
149390120404     c                   unlock    fiar601l
149400120404    1c                   endif
149410120404     c* aggiornamento file
149420120418     c* BLP
149430120404    1c                   if        errp=*blanks
149440120418     c                   eval      arbcpi=savcpi_p
149450120418     c* solo se assegnato rimetto il ksc/ctr che c'era
149460120418     c                   if        %subst(§3atb1:1:1)='A'
149470120418     c                   eval      arbksc=savksc_p
149480120418     c                   eval      arbctr=savctr_p
149490120418     c                   endif
149500120404     c                   except    arbsdblp
149510120404    1c                   endif
149520120418     c* ARB
149530120404    1c                   if        erra=*blanks
149540120416     c* P.iva DEL MITTENTE SU ARB LA AGGIORNO SOLO SE FRANCO NO DOPPIA BOLLA
149550120419     c* in caso contrario tengo la P.IVA che c'era
149560120416     c                   eval      arbcpi=savcpi_a
149570120418     c*
149580120418     c                   if        %subst(§3atb1:1:1)='A'
149590120418     c                   eval      arbksc=savksc_a
149600120418     c                   eval      arbctr=savctr_a
149610120418     c                   endif
149620120404     c                   except    arbsdarb
149630120404    1c                   endif
149640120330     c
149650120330     C                   ENDSR
149660120413     C**************************************************************************
149670120413     C*  AGGIORNAMENTi derivanti da variazioni in sede del ksc di bolla FRANCO
149680120413     C**************************************************************************
149690120413     C     GESSD_ksc     BEGSR
149700120416     C* Controllo se esiste record W di fiar4 per eventuale modifica di mittente
149710120416     C                   MOVEL     'W'           WTRC
149720120416     c                   clear                   wbl4w
149730120416     C     KARB2         CHAIN(N)  Fiar401l
149740120416     c                   if        %found(fiar401l)
149750120416     C                   MOVEL     'S'           WBL4W             1
149760120416     c                   endif
149770120416     c                   z-add     arbksc        oldksc
149780120416     c*
149790120413     c                   eval      arbksc=§btksc
149800120413     c* recupero dati del cliente da anagrafica
149810120413     C                   clear                   TIBS69DS
149820120413     C                   z-add     §btksc        I69kac
149830120413     C                   z-add     §btksc        I69kin
149840120413     C                   call      'TIBS69R'
149850120413     C                   parm                    tibs69DS
149860120413     C                   parm                    ds_cnaco
149870120413     C                   parm                    ds_cnind
149880120413     C                   parm                    ds_cnclp
149890120413     C                   parm                    ds_fncls
149900120413     c                   eval      arbrsm=bs_acorag
149910120413     c                   eval      arbinm=bs_indvia
149920120413     c                   eval      arbLOM=bs_indcit
149930120413     c                   eval      arbCAM=bs_indcae
149940120413     c                   eval      arbPRM=bs_indprv
149950120413     c                   eval      arbNZM=bs_indsta
149960120418     c* P.IVA: non aggiorno arbcpi perchè comune per blp e arb e non va bene:
149970120418     c* se bolla doppia su blp c'è la p.iva del mittente e su arb la p.iva del dest
149980120413     c* dell'aggiornamento di FNARB
149990120419     c                   if        %subst(§3atb1:1:1)='F' and §3atb2=*blanks
150000120419     c                   eval      savcpi_a=bs_indiva
150010120419     c                   endif
150020120419     c                   eval      savcpi_p=bs_indiva
150030120416     c                   exsr      up_fiar4P
150040120416     c                   exsr      up_fiar4w
150050120416     c*
150060120413     c                   endsr
150070120413     C**************************************************************************
150080120413     C* AGGIORNAMENTO RECORD P.IVA IN FIAR4  se esiste
150090120413     C**************************************************************************
150100120413     C     up_fiar4P     BEGSR
150110120413     C                   MOVEL     'P'           WTRC
150120120413     C     KARB2         CHAIN     FiAR401L                           30
150130120413     C     *IN30         IFEQ      *OFF
150140120413     c                   movel     ar4not        wcpifile
150150120413     c                   if        wcpifile <> arbcpi
150160120413     c                   movel     'T'           ar4ftr
150170120413     c                   z-add     dateu         ar4duv
150180120413     c                   z-add     dateu         ar4dtr
150190120413     C                   MOVEl     ARBCPI        AR4NOT
150200120413     C                   UPDATE    FiAR4000
150210120413     c                   else
150220120413     c                   unlock    fiar401l
150230120413     c                   endif
150240120413     C                   END
150250120413     c                   endsr
150260120416     C**************************************************************************
150270120416     C* Gestione record fiar4 record W per DATA TRASMISSIONE VAB
150280120416     C**************************************************************************
150290120416     C     up_fiar4W     BEGSR
150300120416     C* CONTROLLO SE C'E' NUOVO CODICE O VARIATO PER AGGIORNARE O ELIM
150310120416     C*  O SCRIVERE RECORD W IN ar4
150320120416     C                   MOVEL     '3Q'          COD
150330120416     C                   MOVEL(P)  ARBKSC        KEY
150340120416     C     KTAB2         CHAIN     TABEL00F                           30
150350120416     C  N30TBLFLG        IFNE      ' '
150360120416     C                   SETON                                        30
150370120416     C                   ENDIF
150380120416     C*
150390120416     C* C'E' 3Q DEVO CREARE O VARIARE
150400120416    1C  N10*IN30         IFEQ      *OFF
150410120416    2C     WBL4W         IFEQ      ' '
150420120416    3C     ARBKSC        orNE      OLDKSC
150430120416     C                   CLEAR                   DSBL4W
150440120416     C                   CLEAR                   fnlv19ds
150450120416     C                   MOVEL     DSBL4W        d19nt1
150460120416     C                   MOVEL     ARBAAS        d19AAS
150470120416     C                   MOVEL     ARBLNP        d19LNP
150480120416     C                   MOVEL     ARBNRS        d19NRS
150490120416     C                   MOVEL     ARBNSP        d19NSP
150500120416     C                   MOVEL     'W'           d19TRC
150510120416     C                   MOVEL     'T'           d19ftr
150520120416     C                   call      'FNLV19R'
150530120416     c                   parm                    fnlv19ds
150540120416    2C                   ENDIF
150550120416     C*
150560120416   X1C                   ELSE
150570120416     C* SE NO CI DEVE ESERE E PRIMA C'ERA LO DELETO
150580120416    2C     WBL4W         IFEQ      'S'
150590120416     c                   clear                   fnlv19ds
150600120416     C                   clear                   d19nt1
150610120416     c                   clear                   d19nt2
150620120416     C                   MOVEL     arbAAS        d19AAS
150630120416     C                   MOVEL     arbLNP        d19LNP
150640120416     C                   MOVEL     arbNRS        d19NRS
150650120416     C                   MOVEL     arbNSP        d19NSP
150660120416     C                   MOVEL     'W'           d19TRC
150670120416     c                   movel     'T'           d19ftr
150680120416     C                   CALL      'FNLV19R'
150690120416     C                   PARM                    fnlv19ds
150700120416    2C                   ENDIF
150710120416    1C                   ENDIF
150720120416     c                   endsr
150730130321     C**************************************************************************
150740130411     C* Invio Variazione bolla a PDA
150750130321     C**************************************************************************
150760130411     C     sr_pda        BEGSR
150770140908     c* Solo se la chiamata non è cieca:
150780140908     c                   if        d48ffr<>'E'
150790141020     c****               if        d66pda<>' ' and (*in10 or waltrabolla=' ')
150800141020     c                   if        d66pda<>' ' and *in10
150810131227     c                             and wpda_ndc>0 and wpda_dcm=0
150820131227     c* richiamo pgm per verificare che ci sia stata un'effettiva variazione
150830131227     c                   clear                   fnlva1ds
150840131227     c                   eval      IA1AAS=arbaas
150850131227     c                   eval      IA1LNP=arblnp
150860131227     c                   eval      IA1NRS=arbnrs
150870131227     c                   eval      IA1NSP=arbnsp
150880131227     c                   eval      IA1DTV=arbdtv
150890131227     c                   eval      IA1ORV=arborv
150900131227     c                   eval      IA1CVB=arbcvb
150910131227     c                   eval      IA1PRU=arbpru
150920131227     c                   movel     fnlva1ds      kpjbu
150930131227     c                   call      'FNLVA1R'
150940131227     c                   parm                    kpjba
150950131227     c                   movel     kpjbu         fnlva1ds
150960131227     c*
150970131227     c                   if        OA1ESITO='V'
150980140130     c* se filiale partita con nuova gestione telefonate richiamo pgm gestione
150990140130     c* note che si preoccuperà anche del richiamo a fidg42r
151000140130     c                   if        *in87=*off
151010140130     c                   clear                   fnlrn6ds
151020140130     c                   eval      in6mod='I'
151030140130     c                   eval      in6aas=arbaas
151040140130     c                   eval      in6lnp=arblnp
151050140130     c                   eval      in6nrs=arbnrs
151060140130     c                   eval      in6nsp=arbnsp
151070140130     c                   eval      in6pgm='FNLR48R'
151080140130     c                   eval      IN6FGS=arbifp
151090140130     c                   eval      IN6NFV=wpda_ndc
151100140130     c                   eval      IN6DFV=wpda_ddc
151110140130     c                   eval      IN6PDR=arbpdc
151120140130     c                   eval      IN6VAR=d66pda
151130140130     c                   eval      IN6CONS='S'
151140140130     c                   call      'FNLRN6R'
151150140130     C                   parm                    kpjba
151160140130     C                   parm                    fnlrn6ds
151170140130     c                   else
151180140130     c* altrimenti richiamo direttemente fidg42r
151190131227     c
151200130411     c                   z-add     arbifp        w0030             3 0
151210130411     c     w0030         chain     azorg01l
151220130411     c                   if        %found(azorg01l)
151230130411     c                   eval      og148=orgde8
151240131227     c                   if        §ogpdacon<>' '
151250130411     c                   clear                   fidg42ds
151260130411     c                   eval      CO42FGS=arbifp
151270130411     c                   eval      CO42NDC=wpda_ndc
151280130411     c                   eval      CO42DDC=wpda_ddc
151290130411     c                   eval      CO42AAS=arbaas
151300130411     c                   eval      CO42LNP=arblnp
151310130411     c                   eval      CO42NRS=arbnrs
151320130411     c                   eval      CO42NSP=arbnsp
151330130411     c                   eval      CO42CMT='N'
151340130411     c                   movel     fidg42ds      kpjbu
151350130411     c                   call      'FIDG42R'
151360130411     c                   parm                    kpjba
151370130411     c                   endif
151380130411     c                   endif
151390140130     c                   endif
151400131227     c                   endif
151410130411     c                   endif
151420140908     c                   endif
151430130411     c                   endsr
151440141021     C**************************************************************************
151450141021     C* Gestione finestra per richiesta dirottamento
151460141021     C**************************************************************************
151470141021     C     ges_wdw02     BEGSR
151480141021     c                   clear                   winf12
151490141022     c                   eval      atrf21=rimm
151500141125     c                   setoff                                       782879
151510141125     c* iN partenza non si può mai variare senza il dirottamento --> disabilito l'F8
151520141125     c                   if        not *in10
151530141125     c                   seton                                        79
151540141125     c                   endif
151550141022     c                   eval      w02txt='La Linea di Arrivo desunta è diversa-
151560141022     c                              dalla Linea di Arrivo della bolla'
151570141029     c                   exsr      sr_NoAbiDir
151580141021    6c                   do        *hival
151590141021     c                   exfmt     lr48w02
151600141113     c  n78              setoff                                       28
151610141113     c  n78              clear                   w02msg
151620141021     c                   select
151630141021     c* F21=Richiesta dirottamento
151640141021     c                   when      *inkv
151650141021     c                   eval      wdir='1'
151660141021     c* F12=Ritorno
151670141021     c                   when      *inkl
151680141021     c                   eval      winf12='1'
151690141021     c                   endsl
151700141106     c                   if        *inkv or *inkh or *inkl
151710141021     c                   leave
151720141021     c                   endif
151730141021    6c                   enddo
151740141113     c                   setoff                                       7828
151750141021     c                   endsr
151760141029     C**************************************************************************
151770141029     C* Verifica condizioni che disabilitano F21=Dirottamento
151780141029     C**************************************************************************
151790141029     C     sr_NoAbiDir   BEGSR
151800141029
151810141029     c*******************************************
151820141029     c* Casi di disabilitazione del Dirottamento:
151830141029     c*******************************************
151840141029     c* - BOLLA LEGATA
151850141029     c* Se la bolla è legata il dirottamento può essere richiesto solo sull'ultima figlia
151860141029     c     karb          chain     fnlbl02l
151870141029     c                   if        %found(fnlbl02l)
151880141029     c* disattivo l'f21 ed emetto messaggio per avvisare che il dirottamento deve essere
151890141029     c* fatto sulla figlia
151900141029     c                   seton                                        7828
151910141029     c                   eval      w02msg ='Bolla legata:il dirottamento può -
151920141029     c                             essere richiesto solo sulla figlia'
151930141029     c                   clear                   atrf21
151940141029     c                   leavesr
151950141029     c                   endif
151960141029
151970141029     c* - PRESENZA DISPO DA ESAMINARE
151980141029     c**   kidc1         setll     tiidc01l
151990141029     c***                if        %equal(tiidc01l)
152000141029     c                   clear                   fnlry09ds2
152010141029     c                   eval      ILRY09TCH='T'
152020141029     c                   eval      ILRY09AAS=d48aas
152030141029     c                   eval      ILRY09LNP=d48lnp
152040141029     c                   eval      ILRY09NRS=d48nrs
152050141029     c                   eval      ILRY09NSP=d48nsp
152060141029     c                   eval      kpjbu=fnlry09ds2
152070141031     c                   call      'FNLRY09C'
152080141029     c                   parm                    kpjba
152090141029     c                   eval      fnlry09ds2=kpjbu
152100141029     c                   if        OLRY09ESIT='1'
152110141029     c                   seton                                        7828
152120141029     c                   eval      w02msg ='Presenti Disposizioni di Consegna-
152130141029     c                              da elaborare: non è possibile dirottare'
152140141029     c                   clear                   atrf21
152150141029     c                   leavesr
152160141029     c                   endif
152170141029     c
152180141029     c* - PRESENZA FERMO DEPOSITO
152190141029     c                   if        vidffd<>*blanks
152200141029     c                   seton                                        7828
152210141125     c                   if        not *in79
152220141125     c                   eval      w02msg='Presente Fermo Deposito: F8 per non -
152230141029     c                             dirottare, F12 per correggere'
152240141125     c                   else
152250141125     c                   eval      w02msg='Presente Fermo Deposito: F12 per cor-
152260141125     c                             reggere'
152270141125     c                   endif
152280141029     c                   clear                   atrf21
152290141029     c                   endif
152300141112     c* - PRESENZA giacenza aperta
152310141112     c                   if        *in04
152320141112     c                   seton                                        7828
152330141112     c                   eval      w02msg='Presente Giacenza aperta: non è poss-
152340141112     c                             ibile dirottare'
152350141112     c                   clear                   atrf21
152360141112     c                   endif
152370141029     c                   endsr
152380150414     C**************************************************************************
152390150414     C* controlli per cons. particolare "Z"=EXPO
152400150414     C**************************************************************************
152410151119     C*    sr_chkexpo    BEGSR
152420150414     c* Se Expo errore se bolla con contrassegno
152430151119     c*                  if        §3afca=1
152440151119     C*                  MOVEL     MSG(159)      VIDMSG
152450151119     C*                  SETON                                        9028
152460151119     C*                  leavesr
152470151119     c*                  endif
152480150414     c* se expo errore se non è franco
152490151119     c*                  if        %subst(§3atb1:1:1)<>'F'
152500151119     C*                  MOVEL     MSG(160)      VIDMSG
152510151119     C*                  SETON                                        9028
152520151119     C*                  leavesr
152530151119     c*                  endif
152540150414     c* errore se lna non è in tabella
152550151119     c*                  move      v1clna        w003a
152560151119     c*                  if        %lookup(w003a:fil_expo)=0
152570151119     C*                  MOVEL     MSG(161)      VIDMSG
152580151119     c*                  eval      vidmsg=%trim(vidmsg) + ' ' + w003a
152590151119     C*                  SETON                                        9028
152600151119     C*                  leavesr
152610151119     c*                  endif
152620151119     c*                  endsr
152630151016     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
152640151016     C     REGPesivol    BEGSR
152650151016     c                   clear                   dar5fat
152660151016     c                   Eval      kar5trd='FAT'
152670151016     c     kar5          Chain     Fiar531c
152680151016     c                   If        %Found(Fiar531c)
152690151016     c                   Movel     ar5uni        dar5fat
152700151016     c                   endif
152710151016     c* Se tassata
152720151016    1c                   if        ar6imv>0
152730151016     c* Volume
152740151016     c                   movel     arbvlf        §ar5vltas
152750151016     c                   movel     arbfvf        §ar5fvtas
152760151016     c* Peso
152770151016     c                   movel     arbpkf        §ar5pktas
152780151016     c                   movel     'R'           §ar5fptas
152790151016     c
152800151016     c* Se il totale imponibile aggiornato senza arrotondamento
152810151016     c*  è = a quello memorizzato al momento del richiamo al
152820151016     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
152830151016    2c                   if        PesVolIMV=Tassatoimv  and
152840151016     c                             PesVolspc='S'
152850151016     c                   movel     PesVolPkcn    §ar5pktas
152860151016    3c                   if        arbncp=arbncl
152870151016     c                   movel     'T'           §ar5fptas
152880151016     c                   else
152890151016     c                   movel     'Z'           §ar5fptas
152900151016    3c                   endif
152910151016     c
152920151016    2c                   endif
152930151016     c                   eval      §AR5DATAS=%dec(%date())
152940151016     c                   eval      §AR5HMTAS=%dec(%time())
152950151016     c                   eval      §AR5PRTAS=knmus
152960151016     c                   eval      §AR5NJTAS=knmtd
152970151016     c*
152980151016    1c                   endif
152990151016     c
153000151016      * aggiorno Fiar5 record "FAT"
153010151016if  1c                   If        %Found(Fiar531c)
153020151016     c* Se ho eliminato la tassazione  deleto
153030151016     c                   if        ar6imv=0
153040151016     c                   if        recname='FIAR5000'
153050151016     c                   delete    fiar500s
153060151016     c                   else
153070151016     c                   delete    fiar5p0s
153080151016     c                   endif
153090151016     c                   else
153100151016     c                   Movel(p)  dar5fat       Ar5uni
153110151016     c                   eval      ar5dac=%dec(%date())
153120151016     c                   eval      ar5orc=%dec(%time())
153130151016     c                   if        recname='FIAR5000'
153140151016     c                   Update    Fiar500s
153150151016     c                   else
153160151016     c                   Update    Fiar5p0s
153170151016     c                   endif
153180151016     c                   endif
153190151016      * scrivo   Fiar5 record "FAT"
153200151016   x1c                   Else
153210151016    2c                   if        ar6imv>0
153220151016     c                   Clear                   Fiar500s
153230151016     c                   Clear                   Fiar5p0s
153240151016     c                   Eval      Ar5Aas = ArbAas
153250151016     c                   Eval      Ar5Lnp = ArbLnp
153260151016     c                   Eval      Ar5Nrs = ArbNrs
153270151016     c                   Eval      Ar5Nsp = ArbNsp
153280151016     c                   Eval      Ar5Trd = 'FAT'
153290151016     c                   Movel(p)  dar5fat       Ar5uni
153300151016     c                   eval      ar5dac=%dec(%date())
153310151016     c                   eval      ar5orc=%dec(%time())
153320151016     c                   Eval      AR5FT1='T'
153330151016     c                   Eval      AR5DT1=99999999
153340151016     c                   Eval      AR5FT2='T'
153350151016     c                   Eval      AR5DT2=99999999
153360151016     c                   Eval      AR5FT3='T'
153370151016     c                   Eval      AR5DT3=99999999
153380151016     c* Verifico dove si trova TITAS per la write di fiar5
153390151016     c     karb          chain     titaS30c
153400160104     c                   if        %found(titas30c)
153410151019     c                   if        recnamet='TITAS000'
153420151016     c                   Write     Fiar500s
153430151016     c                   else
153440151016     c                   Write     Fiar5p0s
153450151016     c                   endif
153460160104     c                   else
153470160104     c                   Write     Fiar500s
153480160104     c                   endif
153490151016    2c                   EndIf
153500151016    1c                   EndIf
153510151016     c
153520151016     c                   ENDSR
153530141020     C**************************************************************************
153540141020     C* Scrittura TIIDC per Caricamento Disposizione di dirottamento
153550141020     C**************************************************************************
153560141020     C     sr_tiidc      BEGSR
153570141020      /free
153580141020       clear tiidc000;
153590141020       exsr repnum;
153600141020       if esito = *blanks;
153610141020          IDCPRG=kprg                         ;
153620151201       // Se progressivo non reperito lo lascio a 0:
153630151201       // per le dispo di reso e dirottamento non abbiamo la necessità di
153640151201       // avere il progressivo dal momento che per una stessa spedizione
153650151201       // non è ammessa la presenza di + dispo se una di queste è dirottamento o reso
153660151201       // Abbiamo quindi preferito scrivere a 0 il progressivo piuttosto che perdere la
153670151201       // dispo. Questo discorso non val per le dispo che riceviamo da web
153680151201       //else;
153690151201       //   leavesr;
153700141020       endif;
153710141020       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
153720141020                                    : 1 : 14 ) );
153730141020       //IDCtimestn   = ;
153740141020       IDCMODACC='B'                       ;
153750141020       //IDCIP = ;
153760141020       //IDCIDCLI = ;
153770141023       if *in10;
153780141023          IDCTIPOCLI= 'D' ;
153790141023       else;
153800141023          IDCTIPOCLI= 'M' ;
153810141023       endif;
153820141020       IDCWAAS   = arbaas;
153830141020       IDCWLNP   = arblnp;
153840141020       IDCWNRS   = arbnrs;
153850141020       IDCWNSP   = arbnsp;
153860141020       IDCAAS    = arbaas;
153870141020       IDCLNP    = arblnp;
153880141020       IDCNRS    = arbnrs;
153890141020       IDCNSP    = arbnsp;
153900141021       IDCFGS    = lnacons;
153910141020       IDCTIPODIS = '3';
153920141020       //IDCMAILAVV     ;
153930141020       //IDCTELAVV  ;
153940141020       //IDCALERT
153950141021          IDCRSD   = vidrsd;
153960141021          IDCIND   = vidind;
153970141021          IDCCAD   = vidcad;
153980141021          IDCLOD   = vidlod;
153990141021          IDCPRD   = vidprd;
154000141021          IDCNAZ   = vidnzd;
154010141020       // bolla da dirottare
154020141021          IDCNEWLNA=d_o95lna;
154030141021          IDCFLA   ='S';
154040141020       IDCELAFLG='S';
154050141022       IDCELAUTE=knmus;
154060141023       idcref=Vidref;
154070141023       idcteld=vidteld;
154080141020
154090141020         write tiidc000;
154100141020       endsr;
154110141020        //-----------------------------------------------------------------------*
154120141020        // Reperimento progressivo richiesta da numeratore 650
154130141020        //-----------------------------------------------------------------------*
154140141020        begsr RepNum;
154150141020
154160141022          annocur  = %subdt(%date():*years);
154170141020          clear esito;
154180141020          $finenum=*off;
154190141020          clear trul33ds;
154200141020
154210141020          I33TLa = 'L';
154220141020          I33Ope = *ZEROS;
154230141030          I33CNu = 651;
154240141020          I33Num = 1;
154250141020
154260141020          KPJBU = TRUL33DS;
154270141020
154280141020          dow $finenum=*off   ;
154290141020             Trul33R(kpjba);
154300141020
154310141020             TRUL33DS = KPJBU;
154320141020
154330141020             if O33Err <> *zeros;
154340141020               Esito = '2';
154350141020               $finenum=*on;
154360141020             else;
154370141020               w0090=o33nri;
154380141020               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
154390141020               setll kprg tiidc02l;
154400141020        // Se numero già presente nel file TIIDC ne cerco un altro
154410141020               if not %equal;
154420141020                  $finenum=*on;
154430141020               endif;
154440141020             endif;
154450141020          enddo;
154460141020
154470141020          endsr;
154480141020      /end-free
154490940321     C*---------------------------------------------------------------*
154500041007     Ofnblp000  E            ARBDblp
154510041007     O                       arbrsd
154520041007     O                       arbind
154530041007     O                       arbcad
154540041007     O                       arblod
154550041007     O                       arbprd
154560041007     O                       arbnzd
154570041007     O                       arbfin
154580041007     O                       arbcpi
154590041007     O                       arbpkb
154600041007     O                       arbpkf
154610041007     O                       arbvlb
154620041007     O                       arbfvb
154630041007     O                       arbvlf
154640041007     O                       arbfvf
154650041007     O                       arbffd
154660041007     O                       arbcts
154670041022     Ofnblp000  E            ARBKblp
154680041022     O                       arbcbo
154690041022     Ofnblp000  E            ARBGblp
154700041022     O                       arbdcr
154710041022     O                       arbhcr
154720041022     O                       arbtcr
154730041022     O                       arbgc1
154740041022     O                       arbgc2
154750041022     O                       arbtc1
154760041022     O                       arbtc2
154770041022     O                       arbft3
154780041025     Ofnblp000  E            ARBMblp
154790041025     O                       arbksc
154800041025     O                       arbrsm
154810041025     O                       arbinm
154820041025     O                       arbcam
154830041025     O                       arblom
154840041025     O                       arbprm
154850041025     O                       arbnzm
154860041025     O                       arbfap
154870041025     O                       arbcpi
154880041025     O                       arbCTR
154890041025     O                       arbfvf
154900041025     O                       arbvlf
154910041027     Ofnblp000  E            ARBTblp
154920041027     O                       arbvas
154930041027     O                       arbias
154940041027     O                       arbvmd
154950041027     O                       arbvad
154960041027     O                       arbqft
154970041027     O                       arbctr
154980041028     Ofnblp000  E            RMblp
154990041028     O                       arbrma
155000041028     O                       arbnas
155010071025     Ofnblp000  E            SoloCPIP
155020071025     O                       arbcpi
155030120404     Ofnblp000  E            ARBsdblp
155040120404     O                       arbpkf
155050120404     O                       arbvlf
155060120404     O                       arbfvf
155070120416     O                       arbksc
155080120416     O                       arbctr
155090120416     O                       arbrsm
155100120416     O                       arbinm
155110120416     O                       arbLOM
155120120416     O                       arbCAM
155130120416     O                       arbPRM
155140120416     O                       arbNZM
155150120416     O                       arbcpi
155160120531     o                       arbias
155170120531     o                       arbvas
155180041007     Ofnarb000  E            ARBDarb
155190041025     O                       arbrsd
155200041007     O                       arbind
155210041007     O                       arbcad
155220041007     O                       arblod
155230041007     O                       arbprd
155240041007     O                       arbnzd
155250041007     O                       arbfin
155260041007     O                       arbcpi
155270041007     O                       arbpkb
155280041007     O                       arbpkf
155290041007     O                       arbvlb
155300041007     O                       arbfvb
155310041007     O                       arbvlf
155320041007     O                       arbfvf
155330041007     O                       arbffd
155340041007     O                       arbcts
155350041022     Ofnarb000  E            ARBKarb
155360041022     O                       arbcbo
155370041025     Ofnarb000  E            ARBGarb
155380041022     O                       arbdcr
155390041022     O                       arbhcr
155400041022     O                       arbtcr
155410041022     O                       arbgc1
155420041022     O                       arbgc2
155430041022     O                       arbtc1
155440041022     O                       arbtc2
155450141223     O                       arbfbc
155460141223     O                       arbcmc
155470041025     Ofnarb000  E            ARBMarb
155480041025     O                       arbksc
155490041025     O                       arbrsm
155500041025     O                       arbinm
155510041025     O                       arbcam
155520041025     O                       arblom
155530041025     O                       arbprm
155540041025     O                       arbnzm
155550041025     O                       arbfap
155560041025     O                       arbcpi
155570041025     O                       arbCTR
155580041025     O                       arbfvf
155590041025     O                       arbvlf
155600041027     Ofnarb000  E            ARBTarb
155610041027     O                       arbvas
155620041027     O                       arbias
155630041027     O                       arbvmd
155640041027     O                       arbvad
155650041027     O                       arbqft
155660041027     O                       arbctr
155670041028     Ofnarb000  E            RMarb
155680041028     O                       arbrma
155690041028     O                       arbnas
155700071025     Ofnarb000  E            SoloCPIA
155710071025     O                       arbcpi
155720120404     Ofnarb000  E            ARBsdarb
155730120404     O                       arbpkf
155740120404     O                       arbvlf
155750120404     O                       arbfvf
155760120416     O                       arbksc
155770120416     O                       arbctr
155780120416     O                       arbrsm
155790120416     O                       arbinm
155800120416     O                       arbLOM
155810120416     O                       arbCAM
155820120416     O                       arbPRM
155830120416     O                       arbNZM
155840120416     O                       arbcpi
155850120531     o                       arbias
155860120531     o                       arbvas
155870120411     ofiar6000  e            AR6SD
155880120411     o                       AR6KSC
155890120411     o                       AR6CTR
155900120411     o                       AR6DIV
155910120411     o                       AR6POR
155920120411     o                       AR6SV1
155930120411     o                       AR6VA1
155940120411     o                       AR6SV2
155950120411     o                       AR6VA2
155960120411     o                       AR6SV3
155970120411     o                       AR6VA3
155980120411     o                       ar6imv
155990120411     o                       ar6cei
156000041020     Ofiqdt000  E            AGGqdt
156010041020     O                       qdtnbnbr
156020011120**         C20
156030020805ALCOBJ OBJ((FLARXXXX   *FILE   *EXCL   M000000))
156040920120**         C21
156050020805DLCOBJ OBJ((FLARXXXX   *FILE   *EXCL   M000000))
156060920120**         C22
156070020805OVRDBF FILE(FLARXXXX)               MBR(M000000)
156080930507**         GGG
1560909803091234567
156100050601** cmdt
156110050601OVRDBF FILE(TITA430C) TOFILE(
156120151016OVRDBF FILE(FIAR531C) TOFILE(
156130151016OVRDBF FILE(TITAS30C) TOFILE(
156140941014**
156150941102Spedizione al momento non disponibile: gia' utilizzata da un altro utente     01
156160980406Peso mancante                                                                 02
156170980406Volume mancante                                                               03
156180051129Variazione non eseguibile: bolla allocata da altro utente. Attendi e riprova
156190051129Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL05
156200941102Data formalmente errata                                                       06
156210941102Data consegna richiesta non puo' essere inferiore della data spedizione       07
156220941102Ora richiesta formalmente errata                                              08
156230941102Se immesso tipo immettere anche ora o data consegna richiesta                 09
156240941102Giorno di chiusura errato                                                     10
156250000510Consegna particolare errata o non utilizzabile                                11
156260941102I giorni di chiusura non possono essere uguali
156270941102Il destinatario non puo' essere sempre chiuso!!!?!!
156280941102Le consegne particolari non possono essere uguali
156290051117Impossibile modificare Cons.Part. " " :immessa da partenza no gestib.in arriv 15
156300941102Codice Bolla errato o non utilizzabile
156310941102Codice bolla esistente e nuovo non possono essere uguali
156320941102Impossibile trasformare una bolla da non recupero in recupero o viceversa
156330941213Impossibile effettuare cambi di porto: utilizzare l'apposito programma
156340941213Il codice bolla non ammette il C/Assegno                                      20
156350941102Il codice bolla vuole il C/Assegno: inserire almeno l'importo
156360941102Impossibile variare i campi relativi al C/Assegno perche' gia' incassato
156370071025Codice Fiscale obbligatorio per cliente ITALIA.  Enter per forzare            23
156380131002Mancano i dati passati per il tipo di aggiornamento richiesto                 24
156390941102Se immessa la valuta immettere anche l'importo o eliminare entrambi           25
156400941102Valuta errata
156410000510Non ammessi importi con decimali per la valuta indicata                       27
156420000510Se modificata la divisa, ridigitare il relativo importo                       28
156430941102Impossibile immettere le particolarita' C/Assegno per bolla senza C/A
156440131008$$Sono ammesse solo spedizioni in  FRANCO  per le  POSTE                        30
156450131008Causale variazione errata per il tipo di dati passati                         31
156460000510Spedizione POSTE : peso superiore a xxxxxx,x kg.  Enter per forzare!          32
156470941103Filiale Cliente inesistente
156480941103Per FATTURA IMMEDIATA impossibile specificare codice di altra filiale
156490941103Codice non ammesso se cliente di altra filiale                                35
156500941103Codice cliente errato o inesistente
156510941103Codice cliente non utilizzabile: bloccato in Piano dei Conti
156520941103Se Segue Fattura impossibile indicare il codice 9999
156530011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 39
156540941209Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  40
156550941103Impossibile eseguire la tassazione: cliente non dell'area
156560941209Importo da Assicurare obbligatorio, come indicato nella tariffa del cliente   42
156570011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  43
156580011205Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 44
156590011205Valore merce dichiarato obbligatorio                                          45
156600941209Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 46
156610941209Codice cliente di altra filiale: F9 per forzare l'immissione
156620941209Sigla varia inesistente                                                       48
156630941209Varia "G" non ammessa                                                         49
156640941209Varia "G" obbligatoria                                                        50
156650941209Importo varia obbligatorio                                                    51
156660000510Spedizione D.P.D. : non ammesso un peso superiore a xxxxxx,x kg.              52
156670990702Non e' stata trovata la tariffa per la tassazione                             53
156680941212Provvigione a zero: non indicata la percentuale in tariffa                    54
156690941212Errore in tassazione                                                          55
156700011018Se eliminato importo C/A eliminare anche il Tipo Incasso e Particolar.        56
156710071025Partita IVA  obbligatoria per cliente ITALIA.  Enter per forzare              57
156720051129Nuovo volume da FATT. inferiore al volume parziale VDL: sara' sostituito      58
156730950420Non e' possibile modificare il tipo bolla: assegnato gia' incassato           59
156740941222Non e' possibile eliminare i dati della fattura se era un PREPAGATO           60
156750941223Non trovata spedizione                                                        61
156760941223Immesso codice cliente: l'indirizzo sara' ignorato                            62
156770950406Anagrafica mittente non completa in Piano dei Conti                           63
156780950406Manca la provincia in anagrafica mittente: inserirla!!!                       64
156790950407Aggiunto contrassegno: aggiungere anche varia G. Enter per forzatura          65
156800950407Eliminato contrassegno: eliminare anche varia G. Enter per forzatura          66
156810950522Partita IVA obbligatoria se bolla diretta all'estero con attraversam. dogana  67
156820950424Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic68
156830991012Azzerata q.tà da fattur.:azzerare anche gli importi di tassazione             69
156840950426Variato cliente o importo da assic.:azzerare q.tà da fattur. e tot.imponibile.70
156850991012Variato cliente o importo da assic.:azzerare q.tà da fattur. e tassazione     71
156860950426Azzerata q.tà da fattur.e tariffa a valore:azzerare anche il tot. imponibile  72
156870961204Impossibile inserire il C/Assegno in bolle di RESO
156880061024Rif. Mittente obbligatorio se destinatario estero con attraversamento dogana  74
156890970110Natura merce obbligatoria                                                     75
156900970826ATTENZIONE!LArrivo desunta, diversa da bolla ma non verra'variata.ENTER forza
156910970808ATTENZIONE! Inoltro modificato. Enter per continuare                          77
156920980309Impossibile inserire il C/Assegno in questa bolla legata                      78
156930011205Importo da Assicurare non ammesso                                             79
156940011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  80
156950991112Impossibile fare fattura immediata a cliente di altro network                 81
156960990127Impossibile inserire importo da assicurare:bolla su C.A.in Liquidaz.Transatt. 82
156970990419Aggiunto Imp.Assicur.: aggiungere anche varia R. ENTER per forzare            83
156980990419Eliminato Imp.Assicur.: eliminare anche varia R. ENTER per forzare            84
156990120525Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
157000990419Modificato Imp.Assicur: controllare varia R . ENTER per forzare               86
157010060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 87
157020990701Per i Prepagati, Numero fattura deve essere maggiore di XXXXXX                88
157030990702Prepagato: premere F14 per tassare o inserire tassazione fino all'imponibile  89
157040991006Per codice cliente 8888 o 9999 senza tariffa è obbligatoria la divisa xxx     90
157050011210Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              91
157060061107Aggiornamento effettuabile soltanto in PARTENZA su spedizioni in Prepagato
157070011010Ammessi importi con massimo __ decimali per la divisa indicata                93
157080991124Divisa obbligatoria se immesso almeno un importo di tassazione                94
157090061107Aggiornamento effettuabile soltanto in ARRIVO su spedizioni Fattura Immediata 95
157100131008$$Bolla cliente POSTE: non si puo' trasformare in C/Servizio                    96
157110131008$$Bolla cliente POSTE: spese C/A sempre a carico del mittente                   97
157120010412Tipo inoltro "D"=Disagiato non ammesso per questo cap/località                98
157130070508Impostare tipo CONTATTO solo se immesso il Fermo Deposito                     99
157140011011Divisa obbligatoria                                                           00
157150011017Manca la varia 'N'                                                            01
157160011102Divisa scaduta                                                                02
157170011120Spedizione giacente: importo C/A non valido                                   03
157180011210Valore merce dichiarato INFERIORE a xxxxxxxxxx,xxx EUR                        04
157190011210Valore merce dichiarato SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR        05
157200011210Valore merce dichiarato > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE  06
157210011206Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       07
157220011210Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              08
157230011210Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        09
157240011206Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                10
157250020508Discordanza tra codice bolla e codice cliente mittente
157260020531Destinatario               : immettere la relativa consegna particolare       12
157270051116Non è possibile indicare contemporaneamente cons.particolari 'S' e 'X'        13
157280021121Immettere codice tariffa della relativa cartello FedEx                        14
157290021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            15
157300021121ATTENZIONE! Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE    16
157310021129Immettere consegna particolare "S"                                            17
157320030124Immettere il nominativo                                                       18
157330021129Immettere il n. di telefono della persona contattata                          19
157340030124Immettere il motivo                                                           20
157350051129Peso rilevato totalmente da VDL.  Si può inserire solo il peso = a peso VDL   21
157360030123Almeno una delle consegne particolari deve essere "S"                         22
157370051129Impossibile inserire un peso inferiore a quello parzialmente rilevato da VDL  23
157380030124Il n. dei bancali non ritirati non può superare il n. dei bancali bollettati  24
157390030124Immettere il n. dei bancali non ritirati                                      25
157400031001Non è possibile modificare la consegna richiesta                              26
157410040123Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "  27
157420040330La data consegna richiesta deve essere inferiore alla eseguibilità disp.giac. 28
157430040330Data consegna richiesta superiore a ___ giorni                                29
157440131008Codice cliente o linea cliente non utilizzabile                               30
157450041015Prevista consegna ad Uffici:consegna per Appuntam. o a Supermerc. non ammessa 31
157460110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       32
157470050414Impossibile trasformare prepagato in C/S                                      33
157480061116Modificata ragione sociale DESTINATARIO: controllare correttezza Cod.FIScale  34
157490050603Non ammessi inserimenti di importo da assicurare: bolla non coperta da mandato35
157500141110Variazione non effettuabile:salvataggio in corso. Riprovare (File "        ") 36
157510051220Non è possibile cambiare il mittente da DPD a non DPD e viceversa             37
157520060131Utente non abilitato ad inserire la consegna particolare " "                  38
157530070312Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta:ENTERavanza
157540070308Data consegna richiesta OBBLIGATORIA insieme al Tipo e/o all'Ora
157550070312Consegna Richiesta non modificabile: giacenza aperta o sped.con blocco "G"    41
157560070330Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta            42
157570070508Impostato Fermo Dep.DA CONTATTARE ma presente ConsegnaRich.in bolla:ENTR forza 43
157580070910Togliere Cons.Particolare X-xxxxxxxx:non immessa in partenza e non da Destinat44
157590071025Immettere codice fiscale o partita IVA o uscire con F12                       45
157600091124Inserimento/aumento ImpAssic x bolla non coperta da mandato:inviare MAIL.Forza46
157610091124Volume rilevato totalmente da VDL. Si può inserire solo il volume = al VDL    47
157620091124Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL48
157630120508P.Iva "$$" non ammessa per fatture emesse in filiale                          49
157640130930Indirizzo e-mail formalmente errato                                           50
157650131016Eseguita tassazione su bolla contabilizzata:non ammesso F6. F12 per uscire    51
157660131016Non effettuate variazioni: non ammesso F6=Conferma. Premere F12 per uscire    52
157670140408Il Fermo Deposito deve essere autorizzato dal Mittente                        53
157680141029Immessa Data Cons. Richiesta ma presente Fermo Deposito in bolla: ENTER forza 54
157690160204Presenti Dispo di Dirott./Reso ancora da elaborare: Fermo Deposito non ammesso55
157700160204Presente Dispo di Dirott./Reso ancora da elaborare: variazione non ammessa    56
157710141112Presente Giacenza aperta: Enter per Forzare                                   57
157720150210Consegna part. per Appuntamento non valida: immessa data consegna "Tassativa" 58
157730150413Spedizione per EXPO: non ammesso il Contrassegno                              59
157740150414Spedizione per EXPO possibile solo per bolle Franco                           60
157750150414Spedizione per EXPO non ammessa per la linea di arrivo                        61
157760151221Spedizione non manutenzionabile per questa Causale Variazione                 62
