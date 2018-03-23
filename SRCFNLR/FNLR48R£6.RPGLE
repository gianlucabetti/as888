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
001050160225     d FedD_ksc        s                   like(oc7kscc)
001060160225     d FedD_ctr        s              3
001070160225     d FedM_ksc        s                   like(oc7kscc)
001080160225     d FedM_ctr        s              3
001090160225     d
001100160225     d normlod         s                   like(o95loc)
001110150203     d bollacpi        s                   like(arbcpi)
001120071025     d bollaFGS        s                   like(D48FGS)
001130070319     d sav_arbias      s                   like(vidias)
001140041027     d sav_arbvas      s                   like(vidvas)
001150041027     d sav_arbqft      s                   like(vidqft)
001160041027     d sav_arbvmd      s                   like(vidvmd)
001170041027     d sav_arbvad      s                   like(vidvad)
001180131016     d bol_vidias      s                   like(vidias)
001190131016     d bol_vidvas      s                   like(vidvas)
001200131016     d bol_vidvmd      s                   like(vidvmd)
001210131016     d bol_vidvad      s                   like(vidvad)
001220041027     d sav_arbctr      s                   like(vidctr)
001230020327     d sav_dsftd       s                   like(dsftd)
001240011205     d sav1_vidias     s                   like(vidias)
001250011205     d sav1_vidvmd     s                   like(vidvmd)
001260011205     d sav1_vidcas     s                   like(vidcas)
001270020305     d deseftw         s                   like(§15eft)
001280020305     d desefdw         s                   like(§15efd)
001290020305     d deseffw         s                   like(§15eff)
001300020305     d lnpntw          s                   like(§ogntw)
001310020318     d clintw          s                   like(§ogntw)
001320020305     d lnantw          s                   like(§ogntw)
001330020531     d wdisag          s                   like(ot0dos)
001340070906     d wdisagdest      s                   like(ot0dos)
001350020603     d w48f12          s                   like(d48f12)
001360060301     d w48mct          s                   like(d48mct)
001370020911     d Kfgs            s                   Like(FvvFgs)
001380030612     d kAr5Trd         s                   Like(Ar5Trd)
001390050601     d kta4Trc         s                   Like(ta4trc)
001400021121     d Wctr            s                   Like(VidCtr)
001410040122     d va2va2          s                   Like(Ar6Va1)
001420040122     d va2va3          s                   Like(Ar6Va1)
001430040122     d va2va4          s                   Like(Ar6Va1)
001440041130     d va2va5          s                   Like(Ar6Va1)
001450041130     d va2va6          s                   Like(Ar6Va1)
001460041007     d OLDFFD          s                   Like(Arbffd)
001470041008     d OLDVLC          s                   Like(Arbvlc)
001480050202     d SAVCTR          s                   Like(vidctr)
001490070308     d SAVDCR          s                   Like(comdcr)
001500070308     d SAVTCR          s                   Like(vidtcr)
001510070308     d SAVHCR          s                   Like(vidhcr)
001520070308     d bollaDCR        s                   Like(comdcr)
001530070308     d bollaTCR        s                   Like(vidtcr)
001540070308     d bollaHCR        s                   Like(vidhcr)
001550070308     d bollaFBC        s                   Like(arbfbc)
001560071025     d ar4not_P        s                   Like(ar4not)
001570071025     d sav11pid        s                   Like(v11pid)
001580030612      *
001590030612     d DueCtr          s              2  0
001600070320     d TipoEla         s              1
001610030122     d wI0             s              1
001620030123     d wfpf            s              1
001630070312     d xx              s              3  0
001640090526     d yy              s              3  0
001650030612     d $mbr            s              6
001660030612     d w014a           s             14
001670030612     d wdiffe          s             13  3
001680030612     d w0173           s             17  3
001690030612     d tot_importi     s             17  3
001700100120     d xforzait        s              1
001710100120     d WVariavol       s              1
001720120705     d WVariavolEXP    s              1
001730091106     d Aggiobol        s              1
001740071026     d forzacdf        s              1
001750071025     d forzapid        s              1
001760071025     d forzapid2       s              1
001770071025     d forzaias        s              1
001780080625     d forzaias2       s              1
001790030612     d forzavmd        s              1
001800030612     d forzacas        s              1
001810030709     d flgsf20         s              1
001820030711     d wdataiso        s               d   datfmt(*iso)
001830060201     d wdataLimIas     s               d   datfmt(*iso)
001840030711     d wdataoggi       s               d   datfmt(*iso)
001850030711     d miapsw          s             10
001860030711     d wokpsw          s              1
001870040123     d war72           s              1
001880040122     d contasv         s              2  0
001890040122     d totimv          s                   Like(Ar6Imv)
001900040122     d wesvar1         s              1
001910040122     d wvar3ai         s              1
001920040224     d wffd            s                   Like(ArbFfd)
001930040224     d waggar5         s              1
001940040224     d wCA             s              1
001950040324     d wgiac           s              1
001960040225     d warbdcr         s                   Like(§Ar5gDcr)
001970040316     d wgcfas          s                   Like(GcpFas)
001980040324     d wgcded          s                   Like(GcpDed)
001990040324     d wgcddm          s                   Like(GcpDdm)
002000040608     d wokscr          s              1    inz('0')
002010080416     d wcmitt          s              7
002020080416     d wsflagga_C      s              1
002030110629     d wemd            s              1
002040011205
002050020715     d tbl             s              2    dim(20)                              sk tbl con ass.
002060090526     d ADRs            s              3    dim(50)                              sk tbl con ass.
002070090526     d ADRc            s              3    dim(50)                              sk tbl con ass.
002080020805     D SOVR            S             48    DIM(1)                               DI COMODO SED
002090020805     D SALC            S             48    DIM(1)                               DI COMODO SED
002100020805     D SDLC            S             48    DIM(1)                               DI COMODO SED
002110020805     D PDLC            S             48    DIM(1)                               DI COMODO PAR
002120020805     D C20             S              1    DIM(48) CTDATA PERRCD(48)
002130020805     D C21             S              1    DIM(48) CTDATA PERRCD(48)
002140020805     D C22             S              1    DIM(48) CTDATA PERRCD(48)
002150930507     D GGG             S              1    DIM(8) CTDATA PERRCD(8)              GIORNI CHIUSUR
002160151016     d cmdt            s             60    dim(03) ctdata perrcd(1)
002170151221     D MSG             S             78    DIM(162) CTDATA PERRCD(1)
002180041027     d*cmd             s              1    dim(45) ctdata perrcd(45)            ovrdbf
002190030612
002200990930     D* VARIE DELLA DTASV
002210990930     D* VARIE DELLA DARBT
002220941213     D* VARIE DI COMODO
002230941213     D WSV             S              1    DIM(21)                              SIGLA  VARIE
002240990930     D WVA             S             11  3 DIM(21)                              VALORE VARIE
002250941215     D SKI             S              1    DIM(15)                              COMODO
002260011106     D SKII            S              1    DIM(18)                              COMODO
002270941215     D K78             S              1    DIM(78)                              COMODO
002280990930     D*
002290150119     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
002300070906     d
002310070906     d* Skiere di riesecuzione pgm
002320070906     D kdati           S            250    DIM(10)                              VARIE IN TOT.I
002330070906     D kcvb            S              2    DIM(10)                              VARIE IN TOT.I
002340070907     D ktrc            S              2    DIM(10)                              VARIE IN TOT.I
002350070911     D kffr            S              1    DIM(10)                              VARIE IN TOT.I
002360070911     D kf12            S              1    DIM(10)                              VARIE IN TOT.I
002370150414
002380150414      * filiali abilitate a cons.part "Z"=Expo
002390151119     d*fil_expo        s              3    dim(85)
002400030722
002410030722     D ktfp            S                   LIKE(CLNtfp)
002420030722     D ktfa            S                   LIKE(CLNtfa)
002430030722     D kann            S                   LIKE(CLNann)
002440030722     D kmes            S                   LIKE(CLNmes)
002450050601     d wlib            s             10
002460050601     d commt           s             80
002470050601     d lenght          s             15  5 inz(80)
002480120403     d erra            s              1
002490120403     d errp            s              1
002500120404     d wfatfil         s              2
002510120404     d war6agg         s              1
002520120411     d inx             s              2  0
002530120416     d savcpi_a        s                   like(arbcpi)
002540120418     d savcpi_p        s                   like(arbcpi)
002550120418     d savksc_a        s                   like(arbksc)
002560120418     d savctr_a        s                   like(arbctr)
002570120418     d savksc_p        s                   like(arbksc)
002580120418     d savctr_p        s                   like(arbctr)
002590030722
002600151019     d infdsar5        ds
002610151019     d   recname             261    270
002620151019     d infdstitas      ds
002630151019     d   recnamet            261    270
002640030722     D clnmat          DS
002650030722     D  mat                    1     31    dim(31)
002660030722     D clnpom          DS
002670030722     D  pom                    1     31    dim(31)
002680030722
002690030722     D                 DS
002700030722     D  ds_giorno              7      8  0
002710030722     D  ds_mese                5      6  0
002720030722     D  ds_anno                1      4  0
002730030722     D  ds_data                1      8  0
002740030722
002750941213     D*
002760941213     D* CODICI TARIFFA CON CUI TASSARE GLI ASSEGNATI 9999 - TAB 7W
002770990702     D* TARIFFE PER TASSAZIONE PREPAGATI
002780941222     D*-------------_____------------_______-------------_______-----
002790930505     D                 DS
002800941014     D  ARBAAS                 1      4  0
002810941014     D  ARBMGS                 5      8  0
002820941014     D  ARBDSP                 1      8  0
002830950114     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
002840931210     D PARAM3          DS
002850931210     D  PA3PDR                 1      7  0
002860931210     D  PA3TSR                 8      8
002870931210     D  PA3NDC                 9     15  0
002880931210     D  PA3FCT                16     16
002890931210     D  PA3FVD                17     17
002900931210     D  PA3FVT                18     18
002910950113     D  PA3DDC                19     26  0
002920041122     D  PA3sml               256    256
002930941014     D***
002940941014     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002950941014     D***
002960941014     D WLBDAT          DS                  INZ
002970941014     D  G02DAT                 1      8  0
002980941014     D  G02INV                 9     16  0
002990941014     D  G02ERR                17     17
003000941014     D  G02TGI                18     22  0
003010941019     D                 DS
003020941019     D  VIDISD                 1      2
003030941019     D  VIDCPD                 3     16
003040941019     D  VIDPID                 1     16
003050071022     D                 DS
003060071022     D  V11ISD                 1      2
003070071022     D  V11CPD                 3     16
003080071022     D  V11PID                 1     16
003090941223     D                 DS
003100941223     D  VIDISM                 1      2
003110941223     D  VIDCPM                 3     16
003120941223     D  VIDPIM                 1     16
003130930210     D                 DS
003140930210     D  EMME                   1      1
003150020805     d  $simfel                2      4  0
003160020805     D  PARMBR                 5      7  0
003170930210     D  MBRFIS                 1     10
003180140130
003190140130      *Schiera per filiali partite con gestione telefonate AUT
003200140130     d Fil816tel       ds           256
003210140130     d  skFil816tel            1    255    dim(85)
003220141029     d TRULVPODS     e ds
003230141029     D  skFilOk               16    765    dim(250)
003240141029
003250141029     D skFilOkdir      s                   like(skFilOk) dim(250) inz
003260140130
003270920211     D DSARBD        E DS
003280930510     D DSARBG        E DS
003290130930     D DSARBN        E DS
003300990930     D DARBT         E DS
003310030612     D  §SV                   44     63    DIM(20)                              SIGLA  VARIE
003320030612     D  §VA                   64    183P 3 DIM(20)                              VALORE VARIE
003330920128     D DSARBK        E DS
003340030612
003350930506     D DSCC          E DS
003360991001     D DS$2          E DS
003370941212     D DSCV          E DS
003380941020     D DSQT1         E DS
003390080416     D DS3K          E DS
003400160915     D DTAS          E DS                  inz
003410060925      ** DS Flag operativi DS DTAS
003420060925     d Dtas01        e ds
003430060925
003440990930     D DTASV         E DS
003450030612     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
003460030612     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
003470030612     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
003480030612                                                                                ESENZ  VARIE
003490030612     D DSBL4W        E DS
003500070320     D DSBL4L        E DS
003510151126     D DSBL4m        E DS
003520160318     D DSBL4i        E DS
003530000623     D DS3IPT        E DS
003540000510     D DS3IDP        E DS
003550000510     D DS1A          E DS
003560930506     D DS1P          E DS
003570060131     D DSSP          E DS
003580941209     D DS15          E DS
003590920117     D DS3A          E DS
003600020715     d dstb          e ds
003610000202     D OG143         E DS
003620070319     D OG146         E DS
003630070306     d Og148         e ds
003640070306     d Og150         e ds
003650000202     D DGEI          E DS
003660991006     D DGED          E DS
003670991028     D DSTA01        E DS
003680930210     D DS7T          E DS
003690021121     d ddfe          e ds
003700030710     d dvpo          e ds
003710071031     d dvpoDECO      e ds
003720160318     d dnsd          e ds
003730050601     d dta4c         e ds
003740151019     D Dar5fat       E DS
003750090604     d ds5e          e ds
003760110906     d FNLVSTDS      e ds
003770130930     d dsemail       e ds
003780130927     d
003790130927     d darbn_ref     e ds
003800130927     d darbn_eml     e ds
003810130930     d darbn_sms     e ds
003820130930     d darbn_not     e ds
003830061107     d
003840151105     d xcfiva1ds     e ds
003850071025     d  ivaeu                  3      4
003860030612      * DS PER YEURCO - EURO CONVERTITORE
003870030612     D YeurcoDS      E DS
003880030612      * DS PER TRUL90R - reperimento stampanti e pgm di stampa
003890130322     D TRUL90DS      E DS
003900030612      * DS PER FNLV13R - CONTROLLO DI UN CAP
003910030612     D Fnlv13DS      E DS
003920030612      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
003930030612     D Fnlv14DS      E DS
003940030612      * DS PER TISI95R - CONTROLLO DI CAP
003950030612     D Tisi95DS      E DS
003960030612      * DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT E GEST PARTI
003970030612     D DS7pqrs       E DS
003980030612      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
003990030612     D Tibs02DS      E DS
004000030626      * DS PER STAMPA LDV
004010130321     D*FNLSB5DS      E DS
004020030612      * DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
004030030612     D FNLV04DS      E DS
004040050202      * DS PER FNLV59R - CARICAMENTO E CONTROLLO CODICI TARIFFA
004050050202     D Fnlv59DS      E DS
004060041008      * DS PER FNLV57R - PER aggiornamento files padroncini
004070041008     D FNLV57ds      E DS
004080030612      * DS PER FNLV16R - CONTROLLO TASSAZIONE
004090030612     D Fnlv16DS      E DS
004100040122     d dlv1601       e ds
004110030612      * DS PER FNLV18R - CALCOLO VOLUME AUTOMATICO
004120030612     D Fnlv18DS      E DS
004130060223      * DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
004140030612     D Fnlv19DS      E DS
004150030612      * DS PER FNLV20R - CALCOLA E SISTENA I VOLUMI SU BOLLA
004160030612     D Fnlv20DS      E DS
004170030612      * DS PER FNLV55R - REPERISCE TERMINAL DI PARTENZA E ARRIVO
004180030612     D Fnlv55DS      E DS
004190050511      * DS PER FNLRa2R - TASSAZIONE PORTI ASSEGNATI
004200050511     D Fnlra2DS      E DS
004210030612      * DS PER FNLR48R - DATI DI ENTRATA
004220030612     D Fnlr48DS      E DS
004230030612      * DS PER FNLR66R - CONTROLLO CAUSALE VARIAZIONE
004240030612     D Fnlr66DS      E DS
004250140130      * DS PER FNLRN6R - note LDV
004260140130     D FnlrN6DS      E DS
004270130321      * DS PER FIDG42R -
004280130321     D Fidg42ds      E DS
004290160318      * DS PER FNLR98R -
004300160318     D Fnlr98ds      E DS
004310141029
004320141029     D fnlry09ds2    E DS
004330150603     d
004340150603     D tnsd99ds      E DS
004350070329     d
004360120413     D TIBS69DS      E DS
004370120413     D ds_cnaco      E DS                  extname(CNACO00F) prefix(BS_)
004380120413     D ds_cnind      E DS                  extname(CNIND00F) prefix(BS_)
004390120413     D ds_cnclp      E DS                  extname(CNCLP00F) prefix(BS_)
004400120413     D ds_fncls      E DS                  extname(FNCLS00F) prefix(BS_)
004410120413
004420070329      * Dati utente
004430060131     d Azuteds       e ds                  extname(Azute00f)
004440060131     d dDatiute      e ds
004450060131     d dute01        e ds
004460060131     D TIBS34DS      E DS
004470060213      * ds x richiamo pgm FNLv85R
004480060213     d Fnlv85ds      e ds
004490060705      * ds x controllo tipo incasso e divisa C/A
004500060705     d TRULTICDS     e ds
004510140515
004520140515     d TRULINTDS     e ds
004530980722      *
004540030612      * DS PER XEDIT - PGM EDITAZIONE
004550011009     D DSEDIT        E DS                  EXTNAME(XEDITDS)
004560011009     D  IEDNUM       E                     EXTFLD(IEDNUMERO)
004570011009     D  IEDNRD       E                     EXTFLD(IEDNRDEC)
004580011009     D  IEDEDT       E                     EXTFLD(IEDEDTCOD)
004590011009     D  IEDLEN       E                     EXTFLD(IEDLENFLD)
004600011009     D  OEDNUM       E                     EXTFLD(OEDNUMAN)
004610011009     D  OEDFLG       E                     EXTFLD(OEDFLGERR)
004620011009     D  OEDCOD       E                     EXTFLD(OEDCODERR)
004630941212      *
004640910429     D KPJBA         E DS
004650930210     D  LIB                   92    101
004660060131     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004670030612     D  DTU                  848    895P 0 DIM(12)    PACKEVEN                  DATE PARAM
004680011205
004690020327     D DSFTD         E DS                  EXTNAME(FIFTD00F)
004700011205     d trul21ds      e ds
004710011205     d trul22ds      e ds
004720011205     d trul23ds      e ds
004730160225     D* DS PER TRUL27R1 - network bolla
004740160225     D trul27ds1     E DS
004750160225     D
004760160225     D trulC7ds      E DS
004770020531     D tisit0ds      E DS
004780131227     D fnlva1ds      E DS
004790041007     d*Trul42Ds      e ds
004800140212     d Trtcp2ds      e ds
004810141021     D TRUL33DS      E DS
004820021129     d Dar5scr       e ds
004830030123     d dAr5Ban       e ds
004840030729     d dAr5Bnb       e ds
004850130924     d dAr5Emd       e ds                  prefix(emd_)
004860031120     d dAr5          e ds
004870031120     d dAr5Gen       e ds
004880040219     d  §Ar5gTcr     e                     Extfld(§Ar5Tcr)
004890040219     d  §Ar5gDcr     e                     Extfld(§Ar5Dcr)
004900040219     d  §Ar5gHcr     e                     Extfld(§Ar5Hcr)
004910040219     d  §Ar5gGc1     e                     Extfld(§Ar5Gc1)
004920040219     d  §Ar5gGc2     e                     Extfld(§Ar5Gc2)
004930040219     d  §Ar5gTc1     e                     Extfld(§Ar5Tc1)
004940040219     d  §Ar5gTc2     e                     Extfld(§Ar5Tc2)
004950070910     d dAr5GenPar    e ds                  extname(dar5gen) prefix(PAR:3)
004960070910     d  PAr5gTcr     e                     Extfld(§Ar5Tcr)
004970070910     d  PAr5gDcr     e                     Extfld(§Ar5Dcr)
004980070910     d  PAr5gHcr     e                     Extfld(§Ar5Hcr)
004990070910     d  PAr5gGc1     e                     Extfld(§Ar5Gc1)
005000070910     d  PAr5gGc2     e                     Extfld(§Ar5Gc2)
005010070910     d  PAr5gTc1     e                     Extfld(§Ar5Tc1)
005020070910     d  PAr5gTc2     e                     Extfld(§Ar5Tc2)
005030030123     d dTba          e ds
005040031028     d dsbl4a        e ds
005050040806
005060070309     d ds2Z          e ds
005070070309     d ds2g2         e ds
005080040806     d ds7q          e ds
005090140408     d ds7r          e ds
005100051109     d dgcpflr       e ds
005110060111     d ddstflo       e ds
005120040809
005130040809      * ds per il controllo della presenza di CA per la spedizione richiesta
005140080701     d****** FIDN12DS      e ds
005150080701     d***  skKey                 26    305    dim(20)
005160080701     d****  skAnn                306    325    dim(20)
005170080701     d****  skDca                326    485  0 dim(20)
005180080701     d****  skFca                486    545  0 dim(20)
005190080701     d****  skDch                546    705  0 dim(20)
005200080701     d****  skCch                706    745    dim(20)
005210040809     d*
005220040809     d dsKey           ds
005230040809     d  dsaac                         4  0
005240040809     d  dsfil                         3  0
005250040809     d  dsnca                         7  0
005260050531     D PARAMT          DS
005270050531     D  PARCA                256    256
005280011205
005290950104     D*
005300950104     D* DEFINIZIONE COSTANTI
005310950104     D*
005320950104     D CPDC            C                   CONST('su Piano dei Conti')
005330950114     D CNOTAR          C                   CONST('NO TARIFFE')
005340061107     D Cdest           C                   CONST('D E S T I N A T A R I O')
005350061107     D CMITT           C                   CONST('--- M I T T E N T E ---')
005360141022     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
005370141022     D RIMM            C                   CONST(X'21')
005380011205
005390011205     d                sds
005400011205     d  nompgm                 1     10
005410030620
005420030620     d sav_indsin      s                   Like(indsin)
005430041008     d waltrocpi       s                   Like(arbcpi)
005440060223     d wcpifile        s                   Like(arbcpi)
005450041008     d waltrofvf       s                   Like(arbfvf)
005460041008     d waltrovlf       s                   Like(arbvlf)
005470041022     d waltrotc1       s                   Like(arbtc1)
005480041022     d waltrotc2       s                   Like(arbtc2)
005490080422     d waltroft3       s                   Like(arbft3)
005500041027     d newcpi          s                   Like(arbcpi)
005510051114     d wtc1            s                   Like(arbtc1)
005520051114     d wtc2            s                   Like(arbtc2)
005530051220     d memntw          s                   Like(§ogntw)
005540060111     d d_o95lna        s                   like(o95lna)
005550060316     d s_arbrsd        s                   like(arbrsd)
005560060316     d s_arbrd2        s                   like(arbrd2)
005570061107     d wcliie          s              1
005580061107     d wprv            s                   like(arbprd)
005590151105     d wcap            s                   like(arbcad)
005600151105     d wloc            s                   like(arblod)
005610061107     d Esiste          s              1
005620091217     d Wtipoagg        s              3
005630110906     d Lnacons         s              3  0
005640061107     d wnar            s                   Like(arbnzd)
005650130411     d wpda_ndc        s                   Like(arbndc)
005660130411     d wpda_ddc        s                   Like(arbddc)
005670130411     d wpda_dcm        s                   Like(arbdcm)
005680131016     d winf14          s              1
005690131120
005700131120     d sav_ift         s                   Like(ar6ift)
005710140403     d comgf2          s                   Like(o95gf2)
005720140407     d wpk2            s                   Like(§AR5PK2)
005730141020     d wdir            s              1
005740141021     d $Finenum        s               n   inz(*off)
005750141021     d wfgslna         s                   like(d55tfa)
005760141021     D Esito           s              1a
005770141031     d w0090           s              9  0
005780141021     d kprg            s                   like(idcprg)
005790141021     d winf12          s              1
005800141022     d annocur         s              4s 0
005810141029     d w0140i          s             14s 0 inz
005820141203     d savzng          s                   like(vidzng)
005830141212     d wnoindir        s              1
005840150119     d w_noapp         s              1
005850151016     d PesVolpkcn      s                   Like(Taspkcn)
005860151016     d PesVolSpc       s                   Like(TasSpc)
005870151016     d PesVolIMV       s                   Like(Ar6IMV)
005880151019     d Tassatoimv      s                   Like(Ar6IMV)
005890160112     d wfile           s              1
005900160318     d wtogli          s              1
005910141021
005920141021     D trul33R         pr                  extpgm('TRUL33R')
005930141021     D  kpjba                              likeds(kpjba)
005940160318     D fnlv19r         pr                  extpgm('FNLV19R')
005950160318     D  kpjba                              likeds(fnlv19ds)
005960160330     D fnlr98c         pr                  extpgm('FNLR98C')
005970160318     D  kpjba                              likeds(kpjba)
005980070515     i*
005990070515     IFNBLP000
006000070515     I              BLPFST                      ARBFSTDDT
006010910430     C****************************************************************
006020910430     C*  RIEPILOGO INDICATORI
006030910430     C***************************************************************
006040920128     C* 01    - VARIAZIONE DESTINATARIO
006050941209     C* 02    - VARIAZIONE PESO / VOLUME
006060930510     C* 03    - PROGRAMMA RICHIAMATO  DSARBG
006070930505     C* 04    - SPEDIZIONE CON GIACENZA APERTA
006080941222     C* 05    - INDICA SE LA 1 BOLLA O LA 2 E' UN ASSEGNATO X P.IVA
006090941222     C* 06    - 1 BOLLA IN FRANCO
006100991006     C* 07    - PROTEZIONE DIVISA PER PREPAGATI E FATTURE IMMEDIATE
006110991012     C* 08    - PREPAGATO
006120941213     C* 09    - SONO STATE MESSE DELLE VARIE NELLA TASSAZIONE
006130941222     C* 10    - ON -BOLLE IN ARRIVO OFF-BOLLE IN PARTENZA
006140941102     C* 11/12 - PROTEGGO LE CONSEGNE PARTICOLARI SE IMMESSE IN PART E
006150941102     C*         NON UTILIZZABILI IN ARRIVO
006160941102     C* 13    - VARIAZIONE TASSAZIONE S.F.
006170911210     C* 14    - VARIAZIONE TASSAZIONE FATTURA
006180941102     C* 15    - RITASSAZIONE
006190950120     C* 16    - PER CAUSALE CP PROTEGGO CAMPI DELLA CONSEGNA RICH/GG CH
006200940614     C* 17 OFF- FILE TASSAZIONE PADRONCINI
006210940614     C* 17 ON - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
006220911211     C* 18    - NON ESISTONO TARIFFE PER IL CODICE DIGITATO
006230920128     C* 19    - PROGRAMMA RICHIAMATO  DSAR2T
006240920128     C* 20    - PROGRAMMA RICHIAMATO  GENERICO
006250990701     C* 21    - CAUSALE VARIAZIONE MODIFICA CONTRASSEGNO
006260990701     C* 22    - UATO PER PROTEZIONE MITTENTE
006270990701     C* 23    - DATI FATTURA NON MODIFICABILI PER PREPAGATO AUTOMAT
006280950113     C* 24    - ESEGUITA LA VARIAZIONE DEL MITTENTE
006290920128     C* 25    - PROGRAMMA RICHIAMATO  DSARBT
006300950607     C* 26    - ABILITA L'ENTER PER L'AGGIORNAMENTO
006310950607     C* 27    - ABILITA L'F12 PER RITORNO AL PGM CHIAMANTE
006320941212     C* 28    - EMETTO VIDMSG
006330950616     C* 29    - PROTEZIONE CAMPI DEL VIDEO
006340980513     C* 30-33 - COMODO
006350920211     C* 34    - PROGRAMMA RICHIAMATO  DSARBD
006360920129     C* 35    - PROGRAMMA RICHIAMATO  DSARBK
006370920129     C* 36    - VARIA TASSAZIONE FRANCHI: SALTO I CONTROLLI SUL COD.CLI
006380920204     C* 37    - VARIATO TOTALE IMPONIBILE: EFFETTUO RISTAMPA FATTURA
006390941014     C* 38    - CREO RECORD IN FNARBM0T
006400941014     C* 39    - CREO RECORD IN FNARBD0T-CAUS.K5 K8- PER AVERE DESTINAT
006410930507     C* 40-67 - ERRORE
006420030729      * 70    - Visualizzo Colli originali
006430991001     C* 71-72 - PROTEGGO CAMPI DESTINATARIO SE 9999 GIA' CONTABILIZZATO
006440990415     C* 73    - SPROTEGGO IL CAMPO DELL'IMPORTO D'ASSICURARE E DEL VALORE
006450990415     C*         MERCE DICHIARATO
006460060201     C* 74    - protezione campi tassazione
006470060201     C* 75    - protezione campi importo da assicurare
006480140213     C* 77    - condizionamento campo "fd da non contattare"
006490131001     C* 81    - della DSARBN  passato campo uni70 - note
006500990415     C* 82    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA IMP.ASSICURAZ.
006510131001     C* 83    - della DSARBN  passato campo cel
006520131001     C* 84    - della DSARBN  passato campo ref/tel
006530131001     C* 85    - SFPDSP
006540131001     C* 86    - della DSARBN  passato campo uni70 - email
006550071022     C* 87    - tasto funzionale"F17-Note ldv"attivo
006560070329     C* 88    - PROTEGGO PESO   SE NON VARIABILE
006570941216     C* 89    - PROTEGGO VOLUME SE NON VARIABILE
006580920220     C* 90    - ERRORE GENERICO
006590911213     C* 91    - ERRORE NON POSSIBILE ALLOCARE FILE DI INVIO
006600950407     C* 92    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA C/ASS.
006610941215     C* 93    - INDICATORE DI CHANGE SU VALORE MERCE DICH
006620941215     C* 94    - RI SU VALUTA VAL.MERCE SE PRESA IN AUTOMATICO DA NAZ
006630941102     C* 95    - INDICATORE DI CHANGE SU C/ASSEGNO
006640941215     C* 96    - RI SU VALUTA C/ASSEGNO SE PRESA IN AUTOMATICO DA NAZ
006650941215     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
006660941215     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
006670910430     C*****************************************************************
006680000000     C     *ENTRY        PLIST
006690000000     C                   PARM                    KPJBA
006700920211     C                   PARM                    DSARBD
006710920128     C                   PARM                    DSARBK
006720990930     C                   PARM                    DARBT
006730930510     C                   PARM                    DSARBG
006740030612     C                   PARM                    TRUL90DS
006750130930     C                   PARM                    DSARBn
006760050601     c*
006770050110     c*
006780050110     c                   setoff                                       kl
006790030612      *
006800030612      * Il numero dei parametri ricevuti è testato, in caso di modifica
006810030612      *  ricercare la stringa "%PARMS"
006820030612      *
006830030612     C                   MOVEL     KPJBU         Fnlr48DS
006840120426     C*
006850120426     C                   EXSR      OPN_TITA4
006860950104     C* ESEGUO LA VARIAZIONE
006870950104     C                   MOVEL     'E'           WESEG             1
006880020603     c                   movel     d48f12        w48f12
006890060301     c                   movel     d48mct        w48mct
006900950114     C* SE NON  RICHIAMATO IMPOSTO SEMPRE LA FERMATA NELLE VIDEATE
006910950114     C     D48TRC        IFEQ      *BLANKS
006920950609     C                   MOVEL     'V'           D48FFR
006930950114     C                   ENDIF
006940070907     c* skiere di riesecuzione automatica pgm: max 10 volte
006950070907     c                   clear                   kdati
006960070907     c                   clear                   kcvb
006970070907     c                   clear                   ktrc
006980070911     c                   clear                   kffr
006990070911     c                   clear                   kf12
007000070907     c                   clear                   kK                2 0
007010070907     c                   z-add     1             rr                2 0
007020941014     C*
007030910429     C*---------------------------------------------------------------*
007040041006     C* TIPO LANCIO, D48TLA "C" -           CHIUSO CON %R
007050941209     C* TIPO LANCIO, D48TLA "L" - ELABORO E CHIUDO CON LR
007060941209     C* TIPO LANCIO, D48TLA " " - ELABORO E CHIUDO IN RETRN
007070941209     C*
007080941209    1C     D48TLA        IFNE      'C'
007090001213     C*
007100001213    2C     D48FFR        IFNE      'E'
007110141017    2C     D48FFR        andne     'F'
007120001213    3C     OPN48         IFEQ      '0'
007130001213     C                   OPEN      FNLR48D
007140001213     C                   MOVE      '1'           OPN48
007150001213    3C                   ENDIF
007160001213   X2C                   ELSE
007170001213    3C     OPN48         IFEQ      '1'
007180001213     C                   CLOSE     FNLR48D
007190001213     C                   MOVE      '0'           OPN48
007200001213    3C                   ENDIF
007210001213    2C                   ENDIF
007220040224
007230040224     c                   Clear                   wCa
007240040324     c                   Clear                   wgiac
007250941209     C*
007260950104     C* FINTANTO CHE SI VUOLE SI PUO' RIMANRE DENTRO ALLE VARIAZIONI
007270950104   1AC     WESEG         DOWEQ     'E'
007280950104     C                   CLEAR                   WESEG
007290920123     C*
007300950104     C* CONTROLLI BOLLA CON CAUSALE VARIAZIONE
007310900509     C                   EXSR      CONTR
007320941014     C*
007330941014     C   90              GOTO      FINE
007340911209     C*
007350911210     C* CARICO BOLLA
007360120403     c                   if        flgcvr<>'S'
007370911210     C                   EXSR      CARICA
007380131002     c                   if        d48err='1'
007390131002     C                   GOTO      FINE
007400120403     c                   endif
007410131002     c                   endif
007420911209     C**
007430941019     C** VARIAZIONE: DESTINATARIO PESO VOLUME
007440911209     C**
007450941220     C                   SELECT
007460120330     c* Variazioni da manutenzione bolle di sede
007470120330     C     FLGCVR        WHENEQ    'S'
007480120427     c* Questa routine la posso eseguire solo se pgm richiamato
007490120427     c* Hanno fatto una manutenzione con CVB SD/SF da menù in quanto
007500120427     c* SD e SF non erano stati disabilitati per l'utilizzo da bolle par
007510120427     c* e bolle arrivi. In questo modo essendo azzerava peso,volume, mittente..
007520120427     c                   if        d48trc<>*blanks
007530120330     C                   EXSR      gesSD
007540120427     c                   endif
007550120330
007560941220     C     FLGCVR        WHENEQ    'I'
007570920120     C                   EXSR      EMFOR2
007580911210     C**
007590911211     C** VARIAZIONE TASSAZIONE 1° BOLLA
007600911210     C**
007610941220     C     FLGCVR        WHENEQ    'T'
007620920121     C                   EXSR      EMFOR3
007630911211     C**
007640911211     C** VARIAZIONE TASSAZIONE 2° BOLLA
007650911211     C**
007660941220     C     FLGCVR        WHENEQ    '2'
007670941102     C                   EXSR      EMFOR4
007680920116     C**
007690920116     C** VARIAZIONE: CODICE BOLLA CONTRASSEGNO
007700920116     C**
007710941220     C     FLGCVR        WHENEQ    'K'
007720941102     C                   EXSR      EMFOR5
007730920320     C**
007740920320     C** VARIAZIONE CONSEGNA RICHIESTA
007750920320     C**
007760941220     C     FLGCVR        WHENEQ    'C'
007770021129     c     D48Cvb        AndNe     'CS'
007780941102     C                   EXSR      EMFOR7
007790021129     C**
007800021129     C** VARIAZIONE CONSEGNA RICHIESTA SUPERMERCATI
007810021129     C**
007820021129     c     D48Cvb        WhenEq    'CS'
007830021129     c                   ExSr      Emfor71
007840941223     C**
007850941223     C** VARIAZIONE MITTENTE
007860941223     C**
007870941223     C     FLGCVR        WHENEQ    'M'
007880941223     C                   EXSR      EMFOR8
007890961211     C**
007900130927     C** VARIAZIONE DATI MITTENTE
007910961211     C**
007920961211     C     D48CVB        WHENEQ    'RM'
007930961211     C                   EXSR      EMFOR9
007940030123      **
007950030123      ** Variazione mancato ritiro BANCALI
007960030123      **
007970030123     c     D48Cvb        WhenEq    'BR'
007980030123     c                   ExSr      Emfor10
007990961211     C*
008000061107      **
008010061107      ** Variazione CODICE FISCALE
008020061107      **
008030061107     c     D48Cvb        WhenEq    'FI'
008040061107     c                   ExSr      Emfor11
008050130927      **
008060130927      ** Variazione dati destinatario SMS/mail/referente
008070130927      **
008080130927     C     FLGCVR        WHENEQ    'N'
008090130927     c                   ExSr      Emfor12
008100061107     C*
008110941220     C                   ENDSL
008120070907     c
008130070907     c* Verifico se ho delle riesecuzioni da fare
008140070907     c                   if        RR<=10 and kcvb(RR)<>*blanks
008150070907     c                   eval      weseg='E'
008160070907     c                   eval      d48cvb=kcvb(RR)
008170070907     c                   eval      d48trc=ktrc(RR)
008180070911     c                   eval      d48ffr=kffr(RR)
008190070911     c                   eval      w48f12=kf12(RR)
008200070907     c                   select
008210070907     c                   when      d48trc='D'
008220070907     c                   movel     kdati(RR)     dsarbd
008230070907     c                   when      d48trc='G'
008240070907     c                   movel     kdati(RR)     dsarbg
008250070907     c                   when      d48trc='K'
008260070907     c                   movel     kdati(RR)     dsarbK
008270070907     c                   when      d48trc='T'
008280070907     c                   movel     kdati(RR)     darbt
008290070907     c                   when      d48trc='2'
008300070907     c                   movel     kdati(RR)     darbt
008310070907     c                   ENDSL
008320070907     c
008330070907     c                   add       1             RR
008340070907     c                   endif
008350941209     C*
008360950104   1AC                   ENDDO
008370950104    1C                   ENDIF
008380920121     C*
008390000000     C     FINE          TAG
008400941209     C* CHIUSURA PGM
008410060302     C******             MOVEL     Fnlr48DS      KPJBU
008420950114     C*
008430950114     C* PULISCO IL CAMPO CHER AVEVO IMPOSTATO CON LA CONFERMA DEL MITT
008440950114     C*  CHE DEVE RIMANERE IMPOSTATO FINTANTO CHE SI E' DENTRO AL PGM
008450950114     C                   CLEAR                   WRICVL
008460950114     C                   CLEAR                   MITFVF
008470950114     C                   CLEAR                   MITVLF
008480950114     C                   CLEAR                   MITCTR
008490020722      * PULISCO IL CAMPO CHE AVEVO IMPOSTATO PER IL DESTINATARIO
008500020722      *  DISAGIATO
008510020722     C                   CLEAR                   Wdisag
008520060302     c*
008530060302     c                   Clear                   Fnlv85ds
008540060302     c                   Eval      Ilv85Tla = 'C'
008550060302     c                   movel     fnlv85ds      kpjbu
008560060302     c                   Call      'FNLV85R'
008570060302     c                   Parm                    kpjba
008580060302     c*
008590950114     C* CHIUSURA PGM
008600941216    1C     D48TLA        IFEQ      ' '
008610130417     C                   MOVEL     Fnlr48DS      KPJBU
008620941209     C                   RETURN
008630941209     C                   ELSE
008640941209     C*
008650930510     C* SE APERTI FILE CHIUDO
008660001213     C*
008670001213     C     OPN48         IFEQ      '1'
008680001213     C                   CLOSE     FNLR48D
008690001213     C                   ENDIF
008700001213     C*
008710030612     C                   CLEAR                   FNLV04DS
008720941209     C                   MOVEL     'C'           D04TLA
008730990930     C                   CALL      'FNLV04R'
008740030612     C                   PARM                    FNLV04DS
008750990930     C                   PARM                    DTASV
008760970808     C*
008770970808     C                   MOVEL     'C'           I13TLA
008780970808     C                   CALL      'FNLV13R'
008790970808     C                   PARM                    KPJBA
008800030612     C                   PARM                    Fnlv13DS
008810030612     C                   PARM                    Tisi95DS
008820030612     C                   CLEAR                   Tisi95DS
008830971105     C**
008840971105     C                   MOVEL     'C'           I95TLA
008850971105     C                   CALL      'TISI95R'
008860030612     C                   PARM                    Tisi95DS
008870970808     C*
008880970808     C                   MOVEL     'C'           I14TLA
008890970808     C                   CALL      'FNLV14R'
008900970808     C                   PARM                    KPJBA
008910030612     C                   PARM                    Fnlv14DS
008920971114     C*
008930030612     C                   CLEAR                   Fnlv55DS
008940971114     C                   MOVEL     'C'           D55TLA
008950971114     C                   CALL      'FNLV55R'
008960030612     C                   PARM                    Fnlv55DS
008970050202     c
008980050202     C                   CLEAR                   Fnlv59DS
008990050202     C                   MOVEL     'C'           ilv59tla
009000050202     C                   CALL      'FNLV59R'
009010050202     C                   PARM                    Fnlv59DS
009020020305     c*
009030160225     C                   CLEAR                   trul27ds1
009040020305     C                   MOVEL     'C'           I27TLA
009050160225     C                   CALL      'TRUL27R1'
009060160225     C                   PARM                    TRUL27DS1
009070160225     c
009080060705     C                   CLEAR                   trulTICds
009090060705     C                   MOVEL     'C'           IticTLA
009100060705     C                   CALL      'TRULTICR'
009110060705     C                   PARM                    TRULTICDS
009120060705     C                   PARM                    DS1A
009130140515
009140140515     C                   CLEAR                   trulintds
009150140515     C                   MOVEL     'C'           IintTLA
009160140515     C                   CALL      'TRULINTR'
009170140515     C                   PARM                    TRULintds
009180110906     c
009190110906     C                   CLEAR                   fnlvstds
009200110906     C                   MOVEL     'C'           ILVSTTLA
009210110906     C                   CALL      'FNLVSTR'
009220110906     c                   parm                    kpjba
009230110906     C                   PARM                    fnlvstds
009240941220     C*
009250120426     C                   IF        %OPEN(TITA430C)
009260050601     c                   close     tita430c
009270120426     C                   ENDIF
009280151016     C*
009290151016     C                   IF        %OPEN(FIAR531C)
009300151016     c                   close     fiar531c
009310151016     C                   ENDIF
009320151016     C*
009330151016     C                   IF        %OPEN(TITAS30C)
009340151016     c                   close     titas30c
009350151016     C                   ENDIF
009360130321
009370130411     c                   clear                   fidg42ds
009380130411     C                   MOVEL     'C'           co42tla
009390130411     c                   movel     fidg42ds      kpjbu
009400130411     c                   call      'FIDG42R'
009410130411     c                   parm                    kpjba
009420131227
009430131227     c                   clear                   fnlva1ds
009440131227     C                   MOVEL     'C'           ia1tla
009450131227     c                   movel     fnlva1ds      kpjbu
009460131227     c                   call      'FNLVA1R'
009470131227     c                   parm                    kpjba
009480141029
009490141029     c                   eval      ilry09tla= 'C'
009500141029     c                   eval      kpjbu=fnlry09ds2
009510141031     c                   call      'FNLRY09C'
009520141029     c                   parm                    kpjba
009530160318
009540160318     c                   eval      d19tla  =  'C'
009550160318     c                   call      'FNLV19R'
009560160318     c                   parm                    fnlv19ds
009570130321
009580130417     C                   MOVEL     Fnlr48DS      KPJBU
009590041006     C                   SETON                                        LR
009600941209     C                   END
009610941220     C*
009620941209     C* IMPOSTAZIONI INIZIALI -------------------------------------*
009630941209     C     *INZSR        BEGSR
009640941212     C* MI SALVO LA KPJBU
009650941212     C                   MOVEL     KPJBU         W256A           256
009660060131      *
009670060131     c     *dtaara       define    §azute        azuteds
009680060131     c     *dtaara       define    §datiute      ddatiute
009690060131     c                   in(E)     *dtaara
009700060131    1c                   If        %error  or RSUT = *blanks
009710060131     c                   Clear                   Tibs34ds
009720060131     c                   Call      'TIBS34R'
009730060131     c                   Parm                    Tibs34ds
009740060131     c                   In        *dtaara
009750060131    1c                   EndIf
009760060131
009770060131      * se ho errori nei dati utente esco dal pgm
009780060131w   1c                   if        DutErr = 'E'
009790060131     c                   GoTo      Fine
009800060131x   1c                   else
009810060131     c                   Movel     UteFaf        Dute01
009820060131e   1c                   endif
009830941212     C*
009840941209     C                   Z-ADD     1             CODUT
009850060131     C                   CALL      'X§PARUT'
009860060131     C                   PARM                    UT§DSE
009870060131     C                   MOVEL     rsut          VIDDSA
009880000104     C**
009890000104     C                   MOVEL     DTU(10)       W0020             2 0
009900000104     C                   Z-ADD     DTU(10)       WDTU10
009910000104     C     W0020         IFGE      60
009920000104     C                   MOVEL     19            WDTU10
009930000104     C                   ELSE
009940000104     C                   MOVEL     20            WDTU10            8 0
009950000104     C                   ENDIF
009960941014     C*
009970020805
009980020805     c                   eval      $simfel = simfel
009990020805     c                   movel     simfel        $mbr
010000070329     c
010010070329     c* Per EDP note sempre attive, se lna ha geo attiva
010020070329     c                   if        %subst(knmus:1:3)='EDP'
010030070329     c                   eval      dutgeot='S'
010040070329     c                   else
010050070329     c     dutpou        chain     azorg01l
010060070329     c                   if        %found(azorg01l)
010070070329     c                   movel     orgde8        og148
010080070329     c                   else
010090070329     c                   clear                   og148
010100070329     c                   endif
010110070329     c                   movel     §oggeot       dutgeot           1
010120070329     c                   endif
010130941020     C***
010140941020     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
010150941020     C***
010160941020     C                   MOVEL     'QT'          COD
010170941020     C                   MOVEL     '1       '    KEY
010180941212     C                   EXSR      CTABEL
010190941020     C  N30              MOVEL     TBLUNI        DSQT1
010200941020     C   30              CLEAR                   DSQT1
010210941214     C***
010220941214     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"Z"
010230941214     C***
010240941214     C                   MOVEL     '7T'          COD
010250941214     C                   MOVEL(P)  'Z'           KEY
010260941220     C                   EXSR      CTABEL
010270941214     C   30              CLEAR                   DS7T
010280941214     C  N30              MOVEL     TBLUNI        DS7T
010290941214     C*
010300970901     C* FLAG DI SOSTITUZIONE
010310941214     C                   MOVEL     §7TAFA        ZAFA              1
010320941214     C                   MOVEL     §7TAAA        ZAAA              1
010330941214     C                   MOVEL     §7TAFN        ZAFN              1
010340941214     C                   MOVEL     §7TAAN        ZAAN              1
010350970418     C***
010360970418     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"T"
010370970418     C***
010380970418     C                   MOVEL     '7T'          COD
010390970418     C                   MOVEL(P)  'T'           KEY
010400970418     C                   EXSR      CTABEL
010410970418     C   30              CLEAR                   DS7T
010420970418     C  N30              MOVEL     TBLUNI        DS7T
010430970418     C*
010440970901     C* FLAG DI SOSTITUZIONE
010450970418     C                   MOVEL     §7TAFA        TAFA              1
010460970418     C                   MOVEL     §7TAAA        TAAA              1
010470970418     C                   MOVEL     §7TAFN        TAFN              1
010480970418     C                   MOVEL     §7TAAN        TAAN              1
010490991001     C*
010500991001     C* CARICO DA SIGLE VARIE
010510991001     C                   MOVEL     '$2'          COD
010520991001     C                   MOVEL(P)  '1'           KEY
010530991001     C                   EXSR      CTABEL
010540991001     C  N30              MOVEL     TBLUNI        DS$2
010550991001     C   30              CLEAR                   DS$2
010560990930     C**
010570990930     C* CARICO VARIE CHE FANNO PARTE DEL TOTALE IMPONIBILE
010580990930     C**
010590990930     C                   Z-ADD     0             II                3 0
010600990930     C                   MOVEL     'CC'          COD
010610990930     C     KTAB1         CHAIN     TABEL                              30
010620990930    1C     *IN30         DOWEQ     *OFF
010630990930    2C     TBLFLG        IFEQ      ' '
010640990930     C                   MOVEL     TBLKEY        W005A             5
010650990930    3C     W005A         IFEQ      'VARIE'
010660990930     C                   MOVEL     TBLUNI        DSCC
010670990930     C* SIGLA VARIA
010680990930    4C     §CCFL6        IFEQ      'S'
010690990930     C                   ADD       1             II
010700990930     C                   MOVE      TBLKEY        SVA(II)
010710990930    4C                   ENDIF
010720990930    3C                   ENDIF
010730990930    2C                   ENDIF
010740990930     C**
010750990930     C     KTAB1         READE     TABEL                                  30
010760990930    1C                   ENDDO
010770991006     c*
010780991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
010790030612     C                   CLEAR                   Tibs02DS
010800991117     C                   CLEAR                   DGED
010810991117     C                   MOVEL     'C'           T02MOD
010820991117     C                   MOVEL     KNSIF         T02SIF
010830991117     C                   MOVEL     'GED'         T02COD
010840991117     C                   MOVEL     '1'           T02KE1
010850991117     C                   CALL      'TIBS02R'
010860991117     C                   PARM                    KPJBA
010870030612     C                   PARM                    Tibs02DS
010880991117     C     T02ERR        IFEQ      *BLANKS
010890991117     C                   MOVEL     T02UNI        DGED
010900991117     C                   ENDIF
010910011009     C* REPERIMENTO importi DELLA MONETA DI CONTO
010920030612     C                   CLEAR                   Tibs02DS
010930011009     C                   CLEAR                   DGEI
010940011009     C                   MOVEL     'C'           T02MOD
010950011009     C                   MOVEL     KNSIF         T02SIF
010960011009     C                   MOVEL     'GEI'         T02COD
010970011009     C                   MOVEL     §GEDCN        T02KE1
010980011009     C                   CALL      'TIBS02R'
010990011009     C                   PARM                    KPJBA
011000030612     C                   PARM                    Tibs02DS
011010011009     C     T02ERR        IFEQ      *BLANKS
011020011009     C                   MOVEL     T02UNI        DGEI
011030011120     C                   Z-ADD     §GEDFC        DFCSAV
011040011220     C                   Z-ADD     §geimf        imfsav
011050011009     C                   ENDIF
011060011009      * Tabella CV della moneta di conto
011070011009     C                   CLEAR                   DSCV
011080011009     C                   MOVEL     'CV'          COD
011090011009     C                   MOVEL(P)  §GEDCN        KEY
011100011009     C                   EXSR      CTABEL
011110011009     C     *IN30         IFEQ      *OFF
011120011009     C                   MOVEL     TBLUNI        DSCV
011130011009     C                   MOVEL     §CVDEC        DECSAV
011140011009     C                   ENDIF
011150000510     C***
011160000510     C* CARIO TABELLA VARIABILI DPD E POSTE
011170000510     C***
011180000510     C                   MOVEL     '3I'          COD
011190000510     C                   MOVEL(P)  'POSTE'       KEY
011200000510     C                   EXSR      CTABEL
011210000510     C  N30              MOVEL     TBLUNI        DS3IPT
011220000510     C   30              CLEAR                   DS3IPT
011230000510     C*
011240000510     C                   MOVEL     '3I'          COD
011250000510     C                   MOVEL(P)  'DPD'         KEY
011260000510     C                   EXSR      CTABEL
011270000510     C  N30              MOVEL     TBLUNI        DS3IDP
011280000510     C   30              CLEAR                   DS3IDP
011290020715
011300020715      * carico sk con tipi bolla che prevedono l'assicurazione
011310020715     c                   eval      cod = 'TB'
011320020715     c                   clear                   key
011330020715     c                   clear                   xx
011340020715     c     ktab1         setll     tabel00f
011350020715     c                   do        *hival
011360020715     c     ktab1         reade     tabel00f
011370020715      * fine file
011380020715     c                   if        %eof(tabel00f)
011390020715     c                   leave
011400020715     c                   endif
011410020715      * record annullato
011420020715     c                   if        tblflg <> *blanks
011430020715     c                   iter
011440020715     c                   endif
011450020715     c                   movel     tbluni        dstb
011460020715     c                   if        §tbfas = '1'
011470020715     c                   add       1             xx
011480020715     c                   eval      tbl(xx) = tblkey
011490020715     c                   endif
011500020715     c                   enddo
011510070309
011520070309      * Carico sk con eventi che prevedono modifica della cons.rich
011530070309     c                   eval      cod = '2Z'
011540070309     c                   clear                   key
011550070309     c                   clear                   xx
011560090526     c                   clear                   yy
011570070309     c     ktab1         setll     tabel00f
011580070309     c                   do        *hival
011590070309     c     ktab1         reade     tabel00f
011600070309      * fine file
011610070309     c                   if        %eof(tabel00f)
011620070309     c                   leave
011630070309     c                   endif
011640070309      * record annullato
011650070309     c                   if        tblflg <> *blanks
011660070309     c                   iter
011670070309     c                   endif
011680070309     c                   movel     tbluni        ds2Z
011690090526     c* x pgm non richiamato--> controllo solo gli S-sempre
011700090526     c                   if        §2ZADR='S'
011710070309     c                   add       1             xx
011720090526     c                   eval      ADRs(xx) = tblkey
011730070309     c                   endif
011740090526     c*
011750090526     c* x pgm     richiamato--> controllo sia S-sempre  che
011760090526     c*                         C-richiamati
011770090526     c                   if        §2ZADR<>' '
011780090526     c                   add       1             yy
011790090526     c                   eval      ADRc(yy) = tblkey
011800090526     c                   endif
011810070309     c                   enddo
011820021121
011830021121      * Aggancio tabella default tariffe fedex
011840030612     c                   Clear                   Tibs02DS
011850021121     c                   Movel     'C'           T02Mod
011860021121     c                   Movel     KNSIF         T02Sif
011870021121     c                   Movel     'DFE'         T02Cod
011880021121     c                   Movel     'FED'         T02Ke1
011890021121     c                   Call      'TIBS02R'
011900021121     c                   Parm                    Kpjba
011910030612     c                   Parm                    Tibs02DS
011920021121     c                   If        T02Err = *Blanks
011930021121     c                   Movel     T02Uni        DDfe
011940021121     c                   Else
011950021121     c                   Clear                   DDfe
011960021121     c                   EndIf
011970030710
011980060131      * Aggancio tabella VPO recpord VPO
011990030710     c                   Clear                   Dvpo
012000030710     c                   Clear                   Tibs02ds
012010030710     c                   Eval      T02Mod = 'C'
012020030710     c                   Eval      T02Sif = knsif
012030030710     c                   Eval      T02Cod = 'VPO'
012040030710     c                   Movel(p)  'VPO'         T02Ke1
012050030710     c                   Call      'TIBS02R'
012060030710     c                   Parm                    Kpjba
012070030710     c                   Parm                    Tibs02ds
012080030710     c                   If        T02Err = *Blanks
012090030710     c                   Movel     T02Uni        Dvpo
012100030710     c                   EndIf
012110060131
012120060131      * Aggancio tabella VPO record DECO
012130071031     c                   Clear                   Dvpodeco
012140071031     c                   Clear                   Tibs02ds
012150071031     c                   Eval      T02Mod = 'C'
012160071031     c                   Eval      T02Sif = knsif
012170071031     c                   Eval      T02Cod = 'VPO'
012180071031     c                   Movel(p)  'DECO'        T02Ke1
012190071031     c                   Call      'TIBS02R'
012200071031     c                   Parm                    Kpjba
012210071031     c                   Parm                    Tibs02ds
012220071031     c                   If        T02Err = *Blanks
012230071031     c                   Movel     T02Uni        Dvpodeco
012240071031     c                   EndIf
012250140130
012260140130      * Aggancio tabella VPO record DECOFI816TEL
012270140130     c                   Clear                   Tibs02ds
012280140130     c                   Clear                   fil816tel
012290140130     c                   Eval      T02Mod = 'C'
012300140130     c                   Eval      T02Sif = knsif
012310140130     c                   Eval      T02Cod = 'VPO'
012320140130     c                   Movel(p)  'DECOFI816TEL'T02Ke1
012330140130     c                   Call      'TIBS02R'
012340140130     c                   Parm                    Kpjba
012350140130     c                   Parm                    Tibs02ds
012360140130     c                   If        T02Err = *Blanks
012370140130     c                   Movel     T02Uni        fil816tel
012380140130     c                   EndIf
012390150414
012400150414      * Aggancio tabella "VPO" "EXPO"
012410151119     C*                  CLEAR                   tibs02ds
012420151119     c*                  clear                   fil_expo
012430151119     C*                  MOVEL     'C'           T02MOD
012440151119     C*                  MOVEL     KNSIF         T02SIF
012450151119     C*                  MOVEL     'VPO'         T02COD
012460151119     c*                  movel(P)  'EXPO'        t02ke1
012470151119     C*                  CALL      'TIBS02R'
012480151119     C*                  PARM                    KPJBA
012490151119     C*                  PARM                    tibs02ds
012500151119    2C*                  IF        T02ERR = *BLANKS
012510151119     C*                  MOVEa     T02uni        fil_expo
012520151119    2C*                  ENDIF
012530141029
012540141029      * Aggancio tabella VPO record DECOFI816DIR
012550141029     c                   clear                   trulvpods
012560141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
012570141029     c                   call      'TRULVPOR'
012580141029     c                   parm                    trulvpods
012590141029     c                   eval      skfilokdir=skfilok
012600030710
012610030710      * Aggancio tabella MIA
012620030710     c                   Clear                   miapsw
012630030710     c                   Clear                   Tibs02ds
012640030710     c                   Eval      T02Mod = 'C'
012650030710     c                   Eval      T02Sif = knsif
012660030710     c                   Eval      T02Cod = 'MIA'
012670030710     c                   Movel(p)  'TUTTI'       T02Ke1
012680030710     c                   Call      'TIBS02R'
012690030710     c                   Parm                    Kpjba
012700030710     c                   Parm                    Tibs02ds
012710030710     c                   If        T02Err = *Blanks
012720030710     c                   Movel     T02Uni        miapsw
012730030710     c                   EndIf
012740160318      * Aggancio tabella NSD (per Import DPD "VEDI PACCO")
012750160318     c                   clear                   dnsd
012760160318     c                   Clear                   Tibs02ds
012770160318     c                   Eval      T02Mod = 'C'
012780160318     c                   Eval      T02Sif = knsif
012790160318     c                   Eval      T02Cod = 'NSD'
012800160318     c                   Movel(p)  '1'           T02Ke1
012810160318     c                   Call      'TIBS02R'
012820160318     c                   Parm                    Kpjba
012830160318     c                   Parm                    Tibs02ds
012840160318     c                   If        T02Err = *Blanks
012850160318     c                   Movel     T02Uni        dnsd
012860160318     c                   EndIf
012870031120
012880031120      * Aggancio tabella AR5 per record 'GEN'
012890031120     c                   Clear                   dAr5
012900031120     c                   Clear                   Tibs02ds
012910031120     c                   Eval      T02Mod = 'C'
012920031120     c                   Eval      T02Sif = knsif
012930031120     c                   Eval      T02Cod = 'AR5'
012940031120     c                   Movel(p)  'GEN'         T02Ke1
012950031120     c                   Call      'TIBS02R'
012960031120     c                   Parm                    Kpjba
012970031120     c                   Parm                    Tibs02ds
012980031120     c                   If        T02Err = *Blanks
012990031120     c                   Movel     T02Uni        dAr5
013000031120     c                   EndIf
013010040806      * Tabella 2G
013020040806     c                   Movel     '2G'          cod
013030040902     c                   Movel(p)  '2'           key
013040040806     c                   ExSr      ctabel
013050040902     c  n30              Movel     TblUni        ds2g2
013060040902     c   30              Clear                   ds2g2
013070050901      *
013080941222     C***
013090931119     C* DEFINIZIONE CAMPI
013100931119     C***
013110130411     C**** *LIKE         DEFINE    D66BDI        §7LFIN
013120991112     C     *LIKE         DEFINE    D66FFI        §7LFFI
013130991112     C     *LIKE         DEFINE    DATEU         WDATA
013140011010     C     *LIKE         DEFINE    §CVDEC        COMDEC
013150990702     C     *LIKE         DEFINE    AR6IMV        SAVIMV
013160151016     C     *LIKE         DEFINE    AR6IMV        SAVIMV_fat
013170990702     C     *LIKE         DEFINE    FVVNPG        KNPG
013180970423     C     *LIKE         DEFINE    SOVR(1)       SOARBT
013190941213     C     *LIKE         DEFINE    SOVR(1)       SAARBT
013200941213     C     *LIKE         DEFINE    SOVR(1)       SDARBT
013210941213     C     *LIKE         DEFINE    AR6SV1        WSV1
013220941212     C     *LIKE         DEFINE    AR6VA1        WVA1
013230941212     C     *LIKE         DEFINE    TAMPPA        WPPA
013240941212     C     *LIKE         DEFINE    TAMMPA        WMPA
013250050202     C     *LIKE         DEFINE    ilv59FIE      WFIE
013260941209     C     *LIKE         DEFINE    §15EFT        DESEFT
013270941209     C     *LIKE         DEFINE    TBLKUT        §KUT
013280911211     C     *LIKE         DEFINE    TBLCOD        §COD
013290911211     C     *LIKE         DEFINE    TBLKEY        §KEY
013300941212     C     *LIKE         DEFINE    ARBIAS        VARIAS
013310950426     C     *LIKE         DEFINE    ARBIAS        COMIAS
013320930507     C     *LIKE         DEFINE    ARBCTR        COMCTR
013330941220     C     *LIKE         DEFINE    ARBKSC        KSC2
013340950104     C     *LIKE         DEFINE    ARBFAP        COMFAP
013350950407     C     *LIKE         DEFINE    NEWCBO        SAVCBO
013360950419     C     *LIKE         DEFINE    VIDPID        WPIVA
013370011009      *
013380011120     C     *LIKE         DEFINE    §GEDFC        DFCSAV
013390011220     C     *LIKE         DEFINE    §GEimf        imfsav
013400011009     C     *LIKE         DEFINE    §CVDEC        DECSAV
013410011009      *
013420941220     C* PER I PADRONCINI
013430950113     C     *LIKE         DEFINE    ARBKSC        WKSC
013440950113     C     *LIKE         DEFINE    ARBRSM        WRSM
013450950113     C     *LIKE         DEFINE    ARBRSD        OLDRSD
013460150206     C     *LIKE         DEFINE    ARBlod        OLDlod
013470950113     C     *LIKE         DEFINE    ARBCAD        OLDCAD
013480950113     C     *LIKE         DEFINE    ARBFIN        OLDFIN
013490941209     C     *LIKE         DEFINE    ARBVLF        OLDVLF
013500950114     C     *LIKE         DEFINE    ARBFVF        OLDFVF
013510941209     C     *LIKE         DEFINE    ARBPKF        OLDPKF
013520950113     C     *LIKE         DEFINE    ARBRSM        OLDRSM
013530950113     C     *LIKE         DEFINE    ARBCAM        OLDCAM
013540950113     C     *LIKE         DEFINE    ARBKSC        OLDKSC
013550941220     C*
013560950113     C     *LIKE         DEFINE    ARBVLF        WVOL
013570950113     C     *LIKE         DEFINE    ARBVLF        WSOTV
013580941212     C     *LIKE         DEFINE    ARBVLF        WADDV
013590941215     C     *LIKE         DEFINE    ARBVLF        VARVLF
013600941215     C     *LIKE         DEFINE    ARBPKF        VARPKF
013610950113     C     *LIKE         DEFINE    ARBPKF        WPKG
013620941220     C* PER FAR RIDIGITARE L'IMPORTO
013630941215     C     *LIKE         DEFINE    ARBVAS        VARVAS
013640941215     C     *LIKE         DEFINE    ARBVAD        VARVAD
013650941213     C     *LIKE         DEFINE    AR9VCA        VARVCA
013660950918     C* PER CONTROLLARE SE E' MODIFICATO
013670950918     C     *LIKE         DEFINE    AR9VCA        SAVVCA
013680950918     C     *LIKE         DEFINE    AR9TIC        SAVTIC
013690950918     C     *LIKE         DEFINE    AR9CAS        SAVCAS
013700970808     C     *LIKE         DEFINE    VIDCAD        SAVCAD
013710970808     C     *LIKE         DEFINE    VIDCAD        WCAD
013720970808     C     *LIKE         DEFINE    VIDCAD        WCAD1
013730970808     C     *LIKE         DEFINE    VIDLOD        SAVLOD
013740970808     C     *LIKE         DEFINE    VIDLOD        WLOD
013750970808     C     *LIKE         DEFINE    VIDLOD        WLOD1
013760060111     c     *LIKE         DEFINE    VIDrsd        Wrsd1
013770060111     c     *LIKE         DEFINE    VIDrsd        savrsd
013780061116     c     *LIKE         DEFINE    VIDrsd        savrsdQ
013790060111     c     *LIKE         DEFINE    VIDind        wind1
013800060111     c     *LIKE         DEFINE    VIDind        savind
013810060111     c     *LIKE         DEFINE    VIDprd        wprd1
013820060111     c     *LIKE         DEFINE    VIDprd        savprd
013830060111     c     *LIKE         DEFINE    VIDnzd        wnzd1
013840060111     c     *LIKE         DEFINE    VIDnzd        savnzd
013850970808     C     *LIKE         DEFINE    E14CAP        VARCAD
013860970808     C     *LIKE         DEFINE    E14LOC        VARLOD
013870970808     C     *LIKE         DEFINE    E14CAP        VARCAM
013880970808     C     *LIKE         DEFINE    E14LOC        VARLOM
013890991006     C     *LIKE         DEFINE    AR6POR        VARPOR
013900991006     C     *LIKE         DEFINE    AR6VA1        VA2VA1
013910941220     C* PER IL CONTROLLO
013920941212     C     *LIKE         DEFINE    AR9VCA        WVLT
013930941212     C     *LIKE         DEFINE    AR4NOT        SAVRD2
013940941019     C     *LIKE         DEFINE    AR4TRC        WTRC
013950950424     C     *LIKE         DEFINE    VIDQFT        VARQFT
013960950424     C     *LIKE         DEFINE    VIDKLP        VARKLP
013970950424     C     *LIKE         DEFINE    VIDKSC        VARKSC
013980970418     C     *LIKE         DEFINE    §7TSOZ        WSOSC
013990970418     C     *LIKE         DEFINE    ARBFVF        WFVF
014000970813     C     *LIKE         DEFINE    VIDKSC        MEMKSC
014010991005     C     *LIKE         DEFINE    VIDDIV        DIVGEI
014020991006     C     *LIKE         DEFINE    VIDDIV        WDIV
014030991007     C     *LIKE         DEFINE    AR6DFT        DFT2BO
014040950113     C*
014050950113     C     *LIKE         DEFINE    FTDDDC        WDRT
014060950113     C     *LIKE         DEFINE    FTDPDR        WPDR
014070950113     C     *LIKE         DEFINE    FTDNDC        WNDC
014080950113     C     *LIKE         DEFINE    FTDDDC        WDDC
014090950113     C     *LIKE         DEFINE    FTDTSR        WTSR
014100020327     C     *LIKE         DEFINE    FTDSTP        wstp
014110950114     C* CAMPI COLLEGATI ALLA VARIAZIONE DEL MITTENTE DA STORICIZZARE
014120950114     C     *LIKE         DEFINE    ARBFVF        MITFVF
014130950114     C     *LIKE         DEFINE    ARBVLF        MITVLF
014140950114     C     *LIKE         DEFINE    ARBCTR        MITCTR
014150950601     C* CAMPI PER GESTIONE VARIAZIONI DI BOLLE ESTERE NON LOCALI
014160950601     C     *LIKE         DEFINE    ARBLNA        WEUV
014170950601     C     *LIKE         DEFINE    ARBCVB        PARCVB
014180950601     C     *LIKE         DEFINE    ARBDTV        PARDTV
014190950601     C     *LIKE         DEFINE    ARBORV        PARORV
014200950601     C     *LIKE         DEFINE    ARBAAS        PARAAS
014210950601     C     *LIKE         DEFINE    ARBLNP        PARLNP
014220950601     C     *LIKE         DEFINE    ARBNRS        PARNRS
014230950601     C     *LIKE         DEFINE    ARBNSP        PARNSP
014240950601     C     *LIKE         DEFINE    ARBCVB        PARCVC
014250950601     C     *LIKE         DEFINE    SIMFEL        PARFEV
014260130410     C***  *LIKE         DEFINE    ARITIP        KTIP
014270001204     C     *LIKE         DEFINE    ARBTC1        SAVTC1
014280001204     C     *LIKE         DEFINE    ARBTC1        SAVTC2
014290930507     C*
014300950113     C                   MOVEL     'R'           WTSR
014310911211     C                   MOVEL     'R'           RIS               1
014320930210     C                   MOVEL     'M'           EMME
014330990818     C                   MOVEL     'C '          FTC               2
014340130410     C***                MOVEL     'A'           KTIP
014350931119     C***
014360911211     C* GIRO UDATE
014370931119     C***
014380941014     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
014390941014     C                   TIME                    W0140            14 0
014400941014     C* UDATE IN GGMMAAAA
014410941014     C                   MOVE      W0140         WDTGIO            8 0
014420941014     C*
014430941014     C* UDATE IN AAAAMMGG
014440941014     C                   Z-ADD     WDTGIO        G02DAT
014450941014     C                   MOVEL     *BLANK        G02ERR
014460941014     C                   CALL      'XSRDA8'
014470941014     C                   PARM                    WLBDAT
014480941014     C                   MOVEL     G02INV        DATEU             8 0
014490030710     c     *iso          Move      dateu         wdataoggi
014500970423     C*
014510970423     C                   MOVEL     4             KNPG
014520941014     C**
014530941014     C*  KEY
014540941014     C**
014550941014     C     KARB          KLIST
014560941212     C                   KFLD                    D48AAS
014570941212     C                   KFLD                    D48LNP
014580941212     C                   KFLD                    D48NRS
014590941212     C                   KFLD                    D48NSP
014600941019     C     KARB2         KLIST
014610941212     C                   KFLD                    D48AAS
014620941212     C                   KFLD                    D48LNP
014630941212     C                   KFLD                    D48NRS
014640941212     C                   KFLD                    D48NSP
014650941019     C                   KFLD                    WTRC
014660130410     C***  KARI          KLIST
014670130410     C***                KFLD                    D48AAS
014680130410     C***                KFLD                    D48LNP
014690130410     C***                KFLD                    D48NRS
014700130410     C***                KFLD                    D48NSP
014710130410     C***                KFLD                    KTIP
014720941014     C     KFTT          KLIST
014730941215     C                   KFLD                    FTDPDR
014740941215     C                   KFLD                    FTDTSR
014750941215     C                   KFLD                    FTDNDC
014760950113     C                   KFLD                    FTDDDC
014770950113     C     KFTT2         KLIST
014780950113     C                   KFLD                    WPDR
014790950113     C                   KFLD                    WTSR
014800950113     C                   KFLD                    WNDC
014810950113     C                   KFLD                    WDRT
014820991005     C     KFTD          KLIST
014830950113     C                   KFLD                    WPDR
014840950113     C                   KFLD                    WTSR
014850950113     C                   KFLD                    WNDC
014860950113     C                   KFLD                    WDRT
014870950113     C                   KFLD                    WKSC
014880950113     C                   KFLD                    WRSM
014890020326     C     KFTD5         KLIST
014900020326     C                   KFLD                    WPDR
014910020326     C                   KFLD                    WTSR
014920020326     C                   KFLD                    WNDC
014930020326     C                   KFLD                    WDRT
014940020326     C                   KFLD                    D48AAS
014950020326     C                   KFLD                    D48LNP
014960020326     C                   KFLD                    D48NRS
014970020326     C                   KFLD                    D48NSP
014980941014     C* ACCESSO   TABEL                                    *
014990990930     C     KTAB1         KLIST                                                  *
015000990930     C                   KFLD                    CODUT                          *
015010990930     C                   KFLD                    COD               2            *
015020941014     C     KTAB2         KLIST                                                  *
015030941014     C                   KFLD                    CODUT                          *
015040941014     C                   KFLD                    COD               2            *
015050941014     C                   KFLD                    KEY               8            *
015060941014     C* ACCESSO   CNACO                                    *
015070941014     C     KACO          KLIST                                                  *
015080941014     C                   KFLD                    CODUT                          *
015090060131     C                   KFLD                    dutkci                         *
015100941014     C                   KFLD                    KSC               7 0          *
015110970423     C     KFVV          KLIST                                                  *
015120970423     C                   KFLD                    KNPG                           *
015130970423     C                   KFLD                    W0050                          *
015140020911     C                   KFLD                    Kfgs
015150941014     C* ACCESSO   TFTAM CON PROGRESSIVO                    *
015160941014     C*
015170941014     C     KTAM4         KLIST                                                  *
015180941014     C                   KFLD                    KSC                            *
015190941014     C                   KFLD                    COMCTR                         *
015200941014     C                   KFLD                    COMPRG            3 0          *
015210941014     C     KTAM6         KLIST                                                  *
015220941014     C                   KFLD                    KSC2                           *
015230941014     C                   KFLD                    COMCTR                         *
015240941014     C                   KFLD                    COMPRG                         *
015250941014     C     KTPT          KLIST                                                  *
015260941014     C                   KFLD                    KSC                            *
015270941014     C                   KFLD                    COMCTR                         *
015280941014     C                   KFLD                    COMPRG                         *
015290990818     C                   KFLD                    FTC               2            *
015300030124
015310030124     c     kAr5          Klist
015320030124     c                   Kfld                    D48Aas
015330030124     c                   Kfld                    D48Lnp
015340030124     c                   Kfld                    D48Nrs
015350030124     c                   Kfld                    D48Nsp
015360030124     c                   Kfld                    kAr5Trd
015370030722
015380030722     C     kazcln        klist
015390030722     C                   kfld                    ktfp
015400030722     C                   kfld                    ktfa
015410030722     C                   kfld                    kann
015420030722     C                   kfld                    kmes
015430040809
015440040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
015450080701     C***  KDCT1_C       KLIST
015460080701     C***                KFLD                    dsaac
015470080701     C***                KFLD                    dsfil
015480080701     C***                KFLD                    dsnca
015490041020     C* CHIAVE SU Fiqdt01L
015500041020     C     Kqdt          KLIST
015510041020     C                   KFLD                    kfgs
015520041020     C                   KFLD                    w0060
015530050601     c*
015540050601     C* CHIAVE SU Tita430C
015550050601     C     Kta4          KLIST
015560050601     C                   KFLD                    D48AAS
015570050601     C                   KFLD                    D48LNP
015580050601     C                   KFLD                    D48NRS
015590050601     C                   KFLD                    D48NSP
015600050601     c                   kfld                    kta4trc
015610141029
015620141029     C     Kidc1         KLIST
015630141029     C                   KFLD                    D48AAS
015640141029     C                   KFLD                    D48LNP
015650141029     C                   KFLD                    D48NRS
015660141029     C                   KFLD                    D48NSP
015670141029     C                   KFLD                    w0140I
015680030722
015690941212     C**
015700941212     C                   MOVEL     W256A         KPJBU
015710001213     C*
015720001213     C                   MOVE      '0'           OPN48             1
015730911211     C                   ENDSR
015740120426     C*
015750120426      * APERTURA TITA430C
015760120426     c* se provengo da variazione bolla di sede non ho bisogno del tita4
015770120426     C     OPN_TITA4     BEGSR
015780120426     c                   if        %subst(d48cvb:1:1)<>'S'
015790151016     C                   if        NOT %OPEN(TITA430C)
015800120426      * Se sono in ambiente buono - GAITRAGRU
015810141020     c*****              If        knsif = 'FILTRA201 '
015820141020     c                   If        %subst(knsif:7:1) <> 'P'
015830120426     c                   Eval      wlib = 'GAITRAGRU '
015840120426      *  Se sono in ambiente di prova - GAITRAGRPS
015850120426     c                   Else
015860120426     c                   Eval      wlib = 'GAITRAGRPS'
015870120426     c                   EndIf
015880120426     c*
015890120426     c                   Clear                   commt
015900151016     c                   Movel(p)  cmdt(1)       commt
015910120426     c                   Eval      %Subst(commt:30:10) = wlib
015920120426     c                   Eval      commt =
015930151016     c                             %trim(commt) + '/TITA430C)'
015940120426     c                   Call      'QCMDEXC'
015950120426     c                   Parm                    commt
015960120426     c                   Parm                    lenght
015970151016     c                   Open      tita430c
015980120426     c                   endif
015990151016     c* FIAR531C
016000151016     c                   if        not %open(fiar531c)
016010151016     c                   If        %subst(knsif:7:1) <> 'P'
016020151016     c                   Eval      wlib = 'GAITRAGRU '
016030151016      *  Se sono in ambiente di prova - GAITRAGRPS
016040151016     c                   Else
016050151016     c                   Eval      wlib = 'GAITRAGRPS'
016060151016     c                   EndIf
016070151016     c*
016080151016     c                   Clear                   commt
016090151016     c                   Movel(p)  cmdt(2)       commt
016100151016     c                   Eval      %Subst(commt:30:10) = wlib
016110151016     c                   Eval      commt =
016120151016     c                             %trim(commt) + '/FIAR531C)'
016130151016     c                   Call      'QCMDEXC'
016140151016     c                   Parm                    commt
016150151016     c                   Parm                    lenght
016160151016     c                   Open      fiar531c
016170151016     c                   endif
016180151016     c* TITAS30C
016190151016     C                   if        NOT %OPEN(TITAS30C)
016200151016      * Se sono in ambiente buono - GAITRAGRU
016210151016     c                   If        %subst(knsif:7:1) <> 'P'
016220151016     c                   Eval      wlib = 'GAITRAGRU '
016230151016      *  Se sono in ambiente di prova - GAITRAGRPS
016240151016     c                   Else
016250151016     c                   Eval      wlib = 'GAITRAGRPS'
016260151016     c                   EndIf
016270151016     c*
016280151016     c                   Clear                   commt
016290151016     c                   Movel(p)  cmdt(3)       commt
016300151016     c                   Eval      %Subst(commt:30:10) = wlib
016310151016     c                   Eval      commt =
016320151016     c                             %trim(commt) + '/TITAS30C)'
016330151016     c                   Call      'QCMDEXC'
016340151016     c                   Parm                    commt
016350151016     c                   Parm                    lenght
016360151016     c                   Open      titas30c
016370151016     c                   endif
016380151016     c                   endif
016390120426     C                   ENDSR
016400900509     C*
016410050907      *--- CONTROLLO LA PARTICOLARITA' GIACENZA NEL PERIODO DELLE TRAZIONI RIDOTTE ---*
016420050907     C     Contr7q       BEGSR
016430050901      *
016440050907      * Inizializzo un flag che indica se è consentita la variazione della data cons. richiesta
016450050907      * in periodo d trazioni ridotte ed in presenza d particolarità consegna genericamente "G8"
016460050907     c                   clear                   wDCRG8            1
016470050901      * Determino se variazione di bolla con part. giacenza durante
016480050901      *  periodo delle trazioni ridotte
016490050901     c                   If        dateu >= §2g2dri and
016500050901     c                             dateu <= §2g2drf
016510050901      * posso modificare se arbgga <> *Blanks e se in tabella 7Q ha 'R' come cod. disposizione
016520050901     c                   If        ArbGga <> *Blanks
016530050901     c                   Movel     '7Q'          cod
016540050901     c                   Movel(p)  ArbGga        key
016550050901     c                   ExSr      ctabel
016560050901     c  n30              Movel     TblUni        ds7q
016570050901     c   30              Clear                   ds7q
016580050901     c                   If        §7q4d1 = 'R' or §7q4d2 = 'R' or
016590050901     c                             §7q4d3 = 'R' or §7q4d4 = 'R' or
016600050901     c                             §7q4d5 = 'R'
016610050901     c                   Movel     'S'           wDCRG8
016620050901     c                   EndIF
016630050901     c                   EndIF
016640050901     c                   EndIF
016650050901
016660050901     c                   EndSR
016670080201     c
016680900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016690900509     C     CONTR         BEGSR
016700030122
016710030122     c     Riprova       Tag
016720030122
016730941014     C                   SETOFF                                       90
016740941014     C                   SETOFF                                       203519
016750941014     C                   SETOFF                                       253403
016760950609     C                   SETOFF                                       262729
016770990415     C                   SETOFF                                       73
016780151222     c                   clear                   wfile
016790941014     C***
016800941014     C* VEDO SE E' UN PROGRAMMA RICHIAMATO
016810941014     C***
016820941019     C     D48TRC        IFNE      *BLANKS
016830941019     C                   SETON                                        20
016840950114     C                   ELSE
016850941019     C                   ENDIF
016860950607     C***
016870950607     C* ABILITO L'ENTER COME TASTO DI AGGIORNAMENTO SE RICHIESTO
016880950607     C***
016890950607     C     D48FFR        IFNE      'V'
016900070309     C     D48FFR        andne     'F'
016910950607     C                   SETON                                        26
016920950607     C                   ENDIF
016930070309     c
016940070309     c* Se richiamo con F  --> mi imposto la E  (ma con 26 off non effettua
016950070309     c*    l'aggiornamento per ENTER)
016960070309     c                   if        d48ffr='F'
016970070309     c                   eval      d48ffr='E'
016980070309     c                   endif
016990070309     c
017000950607     C***
017010950607     C* ABILITO F12 SE RICHIESTO
017020950607     C***
017030020603     C     W48F12        IFEQ      'S'
017040950607     C                   SETON                                        27
017050950607     C                   ENDIF
017060950609     C***
017070950609     C* PROTEGGO TUTTI I CAMPI SE RICHIAMATO DA PGM PER ACQUISIZIONE
017080950609     C* AUTOMATICA VARIAZIONI DA PC ESTERO
017090950609     C***
017100140404     C***  D48EUV        IFEQ      'N'
017110140404     C***                SETON                                        29
017120140404     C***                END
017130941222     C**
017140941222     C* APRO I FILE A SECONDA DEL TIPO BOLLA
017150941222     C**
017160941222     C     D48TBO        IFEQ      'A'
017170941223     C                   SETON                                        10
017180941222     C                   ELSE
017190941223     C                   SETOFF                                       10
017200941222     C                   ENDIF
017210941222     C**
017220930505     C* SPEDIZIONE
017230941222     C**
017240151218     c***                if        d48cvb<>'SD' and d48cvb<>'SF'
017250151218     C***10KARB          CHAIN     FNARB01L                           3031
017260151218     C**N10KARB          CHAIN     FNBLP01L                           3031
017270930505     C*
017280930505     C* NON UTILIZZABILE
017290151218     C***  *IN31         IFEQ      *ON
017300151218     C***                MOVEL     MSG(1)        D48MSG
017310151218     C***                MOVEL     '1'           D48ERR
017320151218     C***                SETON                                        90
017330151218     C***                GOTO      ENDCTR
017340151218     C***                ENDIF
017350941223     C* NON TROVATA SPEDIZIONE
017360151218     C***  *IN30         IFEQ      *ON
017370151218     C***                MOVEL     MSG(61)       D48MSG
017380151218     C***                MOVEL     '1'           D48ERR
017390151218     C***                SETON                                        90
017400151218     C***                GOTO      ENDCTR
017410151218     C***                ENDIF
017420151218     c***                endif
017430080201     C*
017440080201     C* SE PGM RICHIAMATO CONTROLLO SOLO LA CORRETTEZZA DELLA CAUSALE
017450080201     C                   CLEAR                   Fnlr66DS
017460080201     C                   MOVEL     'C'           D66GES
017470080201     C                   MOVEL     D48CVB        D66CVB
017480080201     C  N20              MOVEL     D48AAS        D66AAS
017490080201     C  N20              MOVEL     D48LNP        D66LNP
017500080201     C  N20              MOVEL     D48NRS        D66NRS
017510080201     C  N20              MOVEL     D48NSP        D66NSP
017520080201     C  N20              MOVEL     D48TBO        D66TBO
017530160115     c                   eval      d66bnp='S'
017540080201     C                   MOVEL     Fnlr66DS      KPJBU
017550080201     C                   CALL      'FNLR66R'
017560080201     C                   PARM                    KPJBA
017570080201     C                   MOVEL     KPJBU         Fnlr66DS
017580080201     C*
017590080201     C* TROVATO ERRORE
017600080201     C     D66ERR        IFEQ      '1'
017610080201      * se prima ho variato il destinatario ed ora devo aggiornare le consegne particolari
017620080201      * in caso di errore provo a cambiare la causale di modifica da 'CP' a 'CR'
017630080201     c                   If        wI0 = 'S'
017640080201     c                   Movel     'CR'          D48Cvb
017650080201     c                   Clear                   wI0
017660080201     c                   Goto      Riprova
017670080201     c                   Else
017680080201     C                   MOVEL     D66MSG        D48MSG
017690080201     C                   MOVEL     D66ERR        D48ERR
017700080201     C                   SETON                                        90
017710151218     c** 10              unlock    fnarb01l
017720151218     c**n10              unlock    fnblp01l
017730080201     c
017740080201     C                   GOTO      ENDCTR
017750080201     c                   EndIf
017760080201     C                   ENDIF
017770141112     c* Le variazioni I0 non sono ammesse se presenti dispo di dirottamento
017780141112     c                   if        d48cvb='I0' or d48cvb='I7' or
017790141112     c                             d48cvb='I8' or d48cvb='I9'
017800141112     c                   clear                   fnlry09ds2
017810141112     c                   eval      ILRY09TCH='T'
017820141112     c                   eval      ILRY09TDIS='D'
017830141112     c                   eval      ILRY09AAS=d48aas
017840141112     c                   eval      ILRY09LNP=d48lnp
017850141112     c                   eval      ILRY09NRS=d48nrs
017860141112     c                   eval      ILRY09NSP=d48nsp
017870141112     c                   eval      kpjbu=fnlry09ds2
017880141112     c                   call      'FNLRY09C'
017890141112     c                   parm                    kpjba
017900141112     c                   eval      fnlry09ds2=kpjbu
017910141112     c                   if        OLRY09ESIT='1'
017920141112     C                   MOVEL     msg(156)      D48MSG
017930141112     C                   MOVEL     '1'           D48ERR
017940141112     C                   SETON                                        90
017950151218     c** 10              unlock    fnarb01l
017960151218     c**n10              unlock    fnblp01l
017970141112     c
017980141112     C                   GOTO      ENDCTR
017990141112     c                   endif
018000141112     c                   endif
018010151218     C**
018020151218     C* SPEDIZIONE
018030151218     C**
018040151218     c                   if        d48cvb<>'SD' and d48cvb<>'SF'
018050151221     c* Manutenzione bolle in arrivo
018060151221     c                   if        *in10
018070151221     c*
018080151221     c                   setoff                                       30
018090151221     c                   eval      wfile='A'
018100151221     c                   if        d66utp<>'P'
018110151221     C     KARB          CHAIN     FNARB01L                           3031
018120151221     c                   endif
018130151221     c                   if        (*in30 and d66utp='T') or d66utp='P'
018140151221     C     KARB          CHAIN     FNblp01L                           3031
018150160112     c                   if        not *in31 and not*in30  and ARBft1<>*blanks
018160151221     c                   unlock    fnblp01l
018170151221     C                   MOVEL     MSG(162)      D48MSG
018180151221     C                   MOVEL     '1'           D48ERR
018190151221     C                   SETON                                        90
018200151221     C                   GOTO      ENDCTR
018210151221     c                   endif
018220151221     c                   eval      wfile='P'
018230151221     c                   endif
018240151221     c                   else
018250151221     c* Manutenzione in partenza
018260151222     c                   eval      wfile='P'
018270151221     C     KARB          CHAIN     FNBLP01L                           3031
018280151221     c                   endif
018290151218     C*
018300151218     C* NON UTILIZZABILE
018310151218     C     *IN31         IFEQ      *ON
018320151218     C                   MOVEL     MSG(1)        D48MSG
018330151218     C                   MOVEL     '1'           D48ERR
018340151218     C                   SETON                                        90
018350151218     C                   GOTO      ENDCTR
018360151218     C                   ENDIF
018370151218     C* NON TROVATA SPEDIZIONE
018380151218     C     *IN30         IFEQ      *ON
018390151218     C                   MOVEL     MSG(61)       D48MSG
018400151218     C                   MOVEL     '1'           D48ERR
018410151218     C                   SETON                                        90
018420151218     C                   GOTO      ENDCTR
018430151218     C                   ENDIF
018440151218     c                   endif
018450080201     C* KEY SPEDIZIONE
018460080201     C                   MOVEL     D48CVB        VIDCVR
018470080201     C                   MOVEL     D48CVB        FLGCVR            1
018480080201     C*
018490080201     C* IMPOSTO IL CAMPO §7LFFI CHE MI SERVE PER ALLOCARE I FILE
018500080201     C                   MOVEL     D66FFI        §7LFFI
018510080201     C*
018520080201     C* IMPOSTO IL CAMPO §7LFIN CHE MI SERVE PER PROTEGGERE I CAMPI
018530080201     C* CHE NON DEVO VARIARE
018540130411     C***                MOVEL     D66BDI        §7LFIN
018550941014     C**
018560941014     C**
018570900509     C     ENDCTR        ENDSR
018580910429     C*
018590910429     C*--- CARICO DATI BOLLA -----------------------------------------*
018600910429     C     CARICA        BEGSR
018610920123     C                   SETOFF                                       040521
018620911211     C                   SETOFF                                       131415
018630990930     C                   SETOFF                                       0102
018640941220     C                   SETOFF                                       363789
018650941213     C                   SETOFF                                       111209
018660991001     C                   SETOFF                                       882307
018670941222     C                   SETOFF                                       525306
018680991012     C                   SETOFF                                       167308
018690061107     C                   SETOFF                                       747576
018700131001     C                   SETOFF                                       878681
018710140213     C                   SETOFF                                       838477
018720140408     c                   setoff                                       68
018730011018      *
018740011018     C                   CLEAR                   FLGVRN
018750110906     C                   CLEAR                   lnacons
018760941102     C*
018770950120     C                   CLEAR                   WIN95
018780950120     C                   CLEAR                   WIN97
018790950120     C                   CLEAR                   WIN93
018800131016     C                   CLEAR                   WINF14
018810950120     C                   CLEAR                   VARVCA
018820941215     C                   CLEAR                   VARVAS
018830941215     C                   CLEAR                   VARVAD
018840941215     C                   CLEAR                   VARVLF
018850941215     C                   CLEAR                   VARPKF
018860990702     C                   CLEAR                   SAVIMV
018870151016     C                   CLEAR                   SAVIMV_fat
018880991006     C                   CLEAR                   WDIV
018890991006     C                   CLEAR                   VARPOR
018900991006     C                   CLEAR                   VA2VA1
018910991012     C                   CLEAR                   SAVCBO
018920091124     C                   CLEAR                   WPT1              1
018930941209     C                   MOVEL     0             CIN               1 0
018940941209     C                   MOVEL     0             DIE               1 0
018950941212     C                   MOVEL     0             SEI               1 0
018960941214     C                   MOVEL     0             SET               1 0
018970941223     C                   MOVEL     0             OTT               1 0
018980970808     C                   MOVEL     0             WZV               1 0
018990970808     C                   MOVEL     0             WZ1               1 0
019000970808     C                   MOVEL     0             WZ2               1 0
019010070508     C                   MOVEL     0             WZ3               1 0
019020970808     C                   MOVEL     0             WZVM              1 0
019030970808     C                   MOVEL     0             WZ1M              1 0
019040970808     C                   MOVEL     0             WZ2M              1 0
019050141029     C                   MOVEL     0             Wforzdcr          1 0
019060141112     C                   MOVEL     0             Wforzgiac         1 0
019070990930     C                   MOVEL     ' '           WAR62             1
019080941019     C                   MOVEL     ' '           WAR6              1
019090941209     C                   MOVEL     ' '           WAR7              1
019100040123     c                   Clear                   War72
019110941019     C                   MOVEL     ' '           WAR9              1
019120941019     C                   MOVEL     ' '           WAR4D             1
019130150206     C                   MOVEL     ' '           WAR4X             1
019140061116     C                   MOVEL     ' '           WAR4Q             1
019150000623     C                   MOVEL     ' '           WBL4W             1
019160920319     C                   Z-ADD     0             COMKSC
019170930507     C                   Z-ADD     0             COMCTR
019180930507     C                   Z-ADD     0             COMPRG
019190911211     C                   Z-ADD     0             VI2DFT
019200911211     C                   Z-ADD     0             VIDDFT
019210941209     C                   Z-ADD     0             VARIAS
019220930507     C                   MOVEL     *BLANKS       VARCTR            3
019230941214     C                   CLEAR                   WSOST
019240030710     c                   Clear                   wokpsw
019250030729
019260030729     c                   Clear                   V1cBan
019270030729     c                   Clear                   V1cCor
019280030729     c                   Eval      *In70 = *Off
019290040123     c                   Clear                   ContaSv
019300040123     c                   Clear                   TotImv
019310060223     C                   CLEAR                   V1CCAS
019320060223     C                   CLEAR                   V1CTIC
019330060223     C                   CLEAR                   V1CVCA
019340060223     C                   CLEAR                   SAVCAS
019350060223     C                   CLEAR                   SAVTIC
019360060223     C                   CLEAR                   SAVVCA
019370090914     C                   CLEAR                   SAVrd2
019380150203     C                   CLEAR                   normlod
019390941215     C* PULIZIA DEI FORMATI VIDEO
019400950210     C                   CLEAR                   LR48D02
019410941215     C                   CLEAR                   LR48C03
019420941215     C                   CLEAR                   LR48D04
019430941215     C                   CLEAR                   LR48D05
019440941215     C                   CLEAR                   LR48D07
019450950104     C                   CLEAR                   LR48D08
019460961211     C                   CLEAR                   LR48D09
019470030124     c                   Clear                   Lr48d10
019480950213     C                   CLEAR                   V1CTIC
019490950213     C                   CLEAR                   V1CCAS
019500950213     C                   CLEAR                   V1CVCA
019510950213     C                   CLEAR                   V1CNT1
019520950213     C                   CLEAR                   V1CNT2
019530950213     C                   CLEAR                   V1CTC1
019540950213     C                   CLEAR                   V1DTC1
019550950213     C                   CLEAR                   V1CTC2
019560950213     C                   CLEAR                   V1DTC2
019570030124     c                   Clear                   VidNom
019580030124     c                   Clear                   VidTel
019590030124     c                   Clear                   VidMot
019600030122     c                   Clear                   wI0
019610031120     c                   Clear                   VidRef
019620070508     c                   Clear                   Vidtfd
019630031120     c                   Clear                   VidTeld
019640070306     c                   Clear                   AggioARP
019650070308     c                   Clear                   savdcr
019660070308     c                   Clear                   savtcr
019670070308     c                   Clear                   savhcr
019680070308     c                   Clear                   bollafbc
019690070309     c                   Clear                   Msgforza          3 0
019700150112     c                   Clear                   Msgforzatxt      78
019710070309     c                   clear                   eveADR            1
019720070910     c                   clear                   WesAR5GEN         1
019730130930     c                   clear                   WesAR5EMD         1
019740071025     c                   clear                   ForzaCDF
019750071025     c                   clear                   ForzaPID
019760071025     c                   clear                   ForzaPID2
019770071025     c                   clear                   sav11pid
019780071026     c                   clear                   AggioBol
019790111025     c                   clear                   Dcrindist         1
019800151019     c                   clear                   PesVolPkcn
019810151019     c                   clear                   PesVolSpc
019820151019     c                   clear                   PesVolImv
019830151019     c                   clear                   TassatoImv
019840061129     c  n10              movel     'T'           bollaft2
019850061129     c   10              movel     'R'           bollaft2
019860160330     c                   clear                   wtogli
019870111207     c* ES - Per il momento lo imposto sempre = "S" in quanto l'ufficio qualità non è
019880111207     c*  sicuro di voler fare la modifica di bloccare la manut coonsegna richiesta
019890111207     c*  dalla manutenzione normale
019900111207     c                   eval      Dcrindist='S'
019910941223     C*
019920941223     C     D48TBO        IFEQ      'A'
019930941223     C                   SETON                                        10
019940941223     C                   ELSE
019950941223     C                   SETOFF                                       10
019960941223     C                   ENDIF
019970941223     C*
019980941223     C* CLEARO I FORMATI DELLE BOLLE
019990051118     C                   CLEAR                   FiAR9000
020000990930     C                   CLEAR                   FIAR7000
020010990930     C                   CLEAR                   FIAR6000
020020030124     c                   Clear                   Fiar5000
020030941220     C**
020040941220     C* PGM RICHIAMATO
020050941220     C**
020060941220     C                   SELECT
020070941220     C* PROGRAMMA RICHIAMATO    DSARBD
020080941220     C     D48TRC        WHENEQ    'D'
020090941220     C                   SETON                                        2034
020100941220     C* PROGRAMMA RICHIAMATO    DSARBK
020110941220     C     D48TRC        WHENEQ    'K'
020120950616     C                   SETON                                        203529
020130990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 1
020140941220     C     D48TRC        WHENEQ    'T'
020150941220     C                   SETON                                        2025
020160990416     C*
020170941220     C* PROGRAMMA RICHIAMATO    DSARBG
020180941220     C     D48TRC        WHENEQ    'G'
020190941220     C                   SETON                                        2003
020200990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 2
020210941220     C     D48TRC        WHENEQ    '2'
020220941220     C                   SETON                                        2019
020230130930     C* PROGRAMMA RICHIAMATO    DARBN TIPO RECORD N + parm
020240130930     C     D48TRC        WHENEQ    'N'
020250130930     c                   if        %parms=7
020260130930     C                   SETON                                        20
020270131008     c                   else
020280131008     C                   MOVEL     MSG(24)       D48MSG
020290131008     C                   MOVEL     '1'           D48ERR
020300131008     c                   leavesr
020310131008     c                   endif
020320131008     c
020330941220     C                   ENDSL
020340950607     C*
020350020603     C     W48F12        IFEQ      'S'
020360950607     C                   SETON                                        27
020370950607     C                   ENDIF
020380950609     C*
020390140404     C***  D48EUV        IFEQ      'N'
020400140404     C***                SETON                                        29
020410140404     C***                END
020420950114     C**
020430950114     C* 24 ON - VARIATO IL MITTENTE
020440950114     C**
020450950114     C     WRICVL        IFEQ      'S'
020460950114     C                   SETON                                        24
020470950114     C                   ENDIF
020480941019     C**
020490941019     C* CARICO I DATI DELLA VIDEATA GENERICA DELLA BOLLA
020500941019     C**
020510941019     C* CHAIN CON CODICE BOLLA
020520941019     C                   MOVEL     '3A'          COD
020530941102     C                   MOVEL(P)  ARBCBO        KEY
020540941212     C                   EXSR      CTABEL
020550941019     C  N30              MOVEL     TBLUNI        DS3A
020560941019     C   30              CLEAR                   DS3A
020570941019     C*
020580941019     C                   MOVEL     §3ATB1        FLGTB1            1
020590941019     C                   MOVEL     §3ADES        V1DCBO                         DESCRIZIONE
020600071116     c* Verifico se il 1°tipo bolla è tassabile o no
020610071116     C                   MOVEL     'TB'          COD
020620071116     C                   MOVEL(P)  §3atb1        KEY
020630071116     C                   EXSR      CTABEL
020640071116     C  N30              MOVEL     TBLUNI        DSTB
020650071116     C   30              CLEAR                   DSTB
020660071116     c                   movel     §tbfcb        oldtb1FCB         1
020670071116     c
020680070308     c
020690070308     c** Blocco consegna (mi serve per la consegna richiesta)
020700151221     c***10              eval      bollaFBC=arbfbc
020710151221     c                   if        *in10
020720151221     c                   if        wfile='A'
020730151221     c                   eval      bollaFBC=arbfbc
020740151221     c                   else
020750151221     c                   clear                   bollaFBC
020760151221     c                   endif
020770151221     c                   endif
020780070308     c  N10              clear                   bollaFBC
020790941019     C*
020800941019     C* SE FRANCO PROTEGGO P.IVA
020810011017     C     FLGTB1        COMP      'F'                                    05
020820941222     C   05              SETON                                            06
020830941019     C*
020840941019     C* SE ESISTE SECONDA BOLLA PERMETTO L'IMMISSIONE
020850061107     c                   clear                   ksc2bol
020860941212    1C     §3ATB2        IFNE      '  '
020870941019     C   05              SETOFF                                       05
020880941019     C* CHAIN SU 2 BOLLA
020890990930     C                   MOVEL     '2'           WTRC
020900070525     c                   if        flgcvr='2'
020910990930     C     KARB2         CHAIN     FIAR6000                           30
020920070525     c                   else
020930070525     C     KARB2         CHAIN(N)  FIAR6000                           30
020940070525     c                   endif
020950990930     C  N30              MOVEL     'E'           WAR62
020960990930     C  N30              MOVEL     AR6KSC        V1CKSC
020970991007     C  N30              Z-ADD     AR6DFT        DFT2BO
020980941019     C   30              Z-ADD     9999          V1CKSC
020990941019     C   30              MOVEL     ARBLNA        V1CLNA
021000991007     C   30              MOVE      *ZEROS        DFT2BO
021010061107     c                   movel     v1cksc        ksc2bol           7 0
021020040127      * Vedo se esiste anche il Fiar7
021030040127     c     Karb2         Setll     Fiar701l
021040040127     c                   If        %Equal(Fiar701l)
021050040127     c                   Eval      war72 = 'E'
021060040127     c                   EndIf
021070941019     C                   ELSE
021080941019     C                   MOVEL     ARBKSC        V1CKSC
021090941212    1C                   END
021100941019     C**
021110051118     C* SE PREVEDE IL C/A CHAI SU FiAR9
021120941019     C**
021130941212    1C     §3AFCA        IFNE      0
021140041022     c                   if        flgcvr='K'
021150051118     C     KARB          CHAIN     FiAR9000                           30
021160041022     c                   else
021170041022     c* non alloco record se non è variazione di C/A
021180051118     C     KARB          CHAIN(n)  Fiar9000                           30
021190041022     c                   endif
021200041022     c
021210941212    2C     *IN30         IFEQ      *OFF
021220941019     C                   MOVEL     'E'           WAR9              1
021230941019     C                   MOVEL     AR9CAS        V1CCAS
021240941019     C                   MOVEL     AR9TIC        V1CTIC
021250941102     C                   MOVEL     AR9VCA        V1CVCA
021260950918     C                   MOVEL     AR9CAS        SAVCAS
021270950918     C                   MOVEL     AR9TIC        SAVTIC
021280950918     C                   MOVEL     AR9VCA        SAVVCA
021290941212    2C                   ENDIF
021300941212    1C                   ENDIF
021310941019     C**
021320941019     C* VEDO SE CI SONO LE NOTE
021330941019     C**
021340941019     C                   MOVEL     '8'           WTRC
021350060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021360941212    1C     *IN30         IFEQ      *OFF
021370941019     C                   MOVEL     AR4NOT        V1CNT1
021380941019     C*
021390941019     C                   MOVEL     '9'           WTRC
021400060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021410941212    2C     *IN30         IFEQ      *OFF
021420941019     C                   MOVEL     AR4NOT        V1CNT2
021430941212    2C                   ENDIF
021440941212    1C                   ENDIF
021450970409     C* SI NO DDT
021460031028     c                   Clear                   dsbl4a
021470970409     C                   MOVEL     'A'           WTRC
021480151222     c                   if        wfile='A'
021490151222     C     KARB2         CHAIN(N)  FiAR4000                           30
021500151222    2C     *IN30         IFEQ      *OFF
021510031028     C**!!!              MOVEL     AR4NOT        V1CDDT
021520031028     c                   Movel     Ar4not        dsbl4a
021530031028     c                   Move      §b4abm        v1cddt
021540970409     C                   ELSE
021550970409     C                   MOVEL     'S'           V1CDDT
021560970409    2C                   ENDIF
021570151222     c                   else
021580151222     C                   MOVEL     ARBFSTddt     V1CDDT            1
021590151222     c                   endif
021600970409     C*
021610970409     C     V1CDDT        IFEQ      'S'
021620970409     C     V1CDDT        OREQ      'C'
021630031028     c     v1cddt        oreq      'P'
021640031028     c     v1cddt        oreq      'Q'
021650970409     C                   MOVEL     'DDT'         V1DDDT
021660970409     C                   ELSE
021670970409     C                   CLEAR                   V1DDDT
021680970409     C                   ENDIF
021690060222     C* CONTROLLO SE ESISTE RECORD W DI ar4 PER EVENTUALE
021700000623     C*   MODIFICA DI MITTENTE
021710000623     C     *IN10         IFEQ      *OFF
021720000623     C                   MOVEL     'W'           WTRC
021730060222     C     KARB2         CHAIN(N)  Fiar4000                           30
021740000623     C  N30              MOVEL     'S'           WBL4W             1
021750000623     C                   ENDIF
021760941019     C* DESCRIZIONE CAUSALE
021770941019     C                   MOVEL     D66DES        V1DCVR
021780941019     C                   MOVEL     ARBLNP        V1CLNP
021790941212     C                   MOVEL     ARBNRS        V1CNRS
021800941019     C                   MOVEL     ARBNSP        V1CNSP
021810941019     C                   MOVEL     ARBLNA        V1CLNA
021820941019     C                   MOVEL     ARBCBO        V1CCBO
021830941019     C                   MOVEL     ARBRSM        V1CRSM
021840941019     C                   MOVEL     ARBCAM        V1CCAM
021850941019     C                   MOVEL     ARBLOM        V1CLOM
021860941019     C                   MOVEL     ARBPRM        V1CPRM
021870941019     C                   MOVEL     ARBNZM        V1CNZM
021880970828     C                   MOVEL     ARBFAP        V1CFAP
021890941019     C                   MOVEL     ARBRSD        V1CRSD
021900941019     C                   MOVEL     ARBIND        V1CIND
021910941019     C                   MOVEL     ARBCAD        V1CCAD
021920941019     C                   MOVEL     ARBLOD        V1CLOD
021930941019     C                   MOVEL     ARBPRD        V1CPRD
021940941019     C                   MOVEL     ARBNZD        V1CNZD
021950970828     C                   MOVEL     ARBFIN        V1CFIN
021960941019     C                   MOVEL     ARBCTS        V1CCTS
021970941019     C                   MOVEL     ARBTC1        V1CTC1
021980941019     C                   MOVEL     ARBTC2        V1CTC2
021990941027     C                   MOVEL     ARBPKF        V1CPKF
022000941019     C                   MOVEL     ARBVLF        V1CVLF
022010941019     C                   MOVEL     ARBFVF        V1CFVF
022020030729     c                   Z-Add     ArbNcl        V1cNcl
022030141029     c                   clear                   v1dffd
022040141029     c                   if        arbffd='S'
022050141029     c                   eval      v1dffd='FermoDeposito'
022060141029     c                   endif
022070100121     c* Partita iva MITTENTE se bolla singola franco
022080100121     c                   select
022090100121     c                   when      flgtb1='F' and §3atb2=*blanks
022100100121     c                   eval      v1cpim=arbcpi
022110100121     c                   when      flgtb1='A' and §3atb2=*blanks
022120100121     c                   eval      v1cpid=arbcpi
022130100121     c* In arrivo   per bolla  doppia --> partita iva dell'assegnato
022140151222     c                   when      wfile='A'   and §3atb2<>*blanks
022150100121     c                   eval      v1cpid=arbcpi
022160100121     c* In partenza per bolla  doppia --> partita iva del mittente
022170151222     c                   when      wfile='P'   and §3atb2<>*blanks
022180100121     c                   eval      v1cpid=arbcpi
022190100121     c                   ENDSL
022200100121
022210950114     C* SALVO IL CODICE CLIENTE DELLA SPEDIZIONE
022220950114     C                   MOVEL     ARBKSC        SAVKSC            7 0
022230060511     C                   MOVEL     ARBKSC        kscbolla          7 0
022240941102     C* PRENDO LA VALUTA DAL DESTINATARIO E MITTENTE
022250941102     C                   MOVEL     '15'          COD
022260941102     C                   MOVEL(P)  ARBNZD        KEY
022270941212     C                   EXSR      CTABEL
022280941102     C  N30              MOVEL     TBLUNI        DS15
022290941102     C   30              CLEAR                   DS15
022300941102     C                   MOVEL     §15VLT        DESVLT            3
022310941212    1C     §15ITA        IFEQ      'I'
022320941102     C                   MOVEL     'I'           WDESIE            1
022330941102     C                   ELSE
022340941102     C                   MOVEL     'E'           WDESIE            1
022350941212    1C                   ENDIF
022360020305     c                   MOVEL     §15EFT        DESEFTW
022370020305     c                   MOVEL     §15EFD        DESEFDW
022380020305     c                   MOVEL     §15EFF        DESEFFW
022390971230     V* CHAIN SU AZORG CON LINEA PARTENZA
022400971230     C     ARBLNP        CHAIN     AZORG01L                           32
022410941209     C*
022420000223     C                   MOVEL     ORGDE3        OG143
022430020305     C                   MOVEL     §OGNTW        LNPNTW
022440941102     C*
022450941102     C                   MOVEL(P)  ARBNZM        KEY
022460941212     C                   EXSR      CTABEL
022470941102     C  N30              MOVEL     TBLUNI        DS15
022480941102     C   30              CLEAR                   DS15
022490941102     C                   MOVEL     §15VLT        MITVLT            3
022500941212    1C     §15ITA        IFEQ      'I'
022510941102     C                   MOVEL     'I'           WMITIE            1
022520941102     C                   ELSE
022530941102     C                   MOVEL     'E'           WMITIE            1
022540941212    1C                   ENDIF
022550001204     C**
022560051129     C* TERMINAL DI PARTENZA DELLA LNP: VEDO SE HO VDL
022570040123     c                   Clear                   og150
022580970901     C     ARBTFP        CHAIN     AZORG                              32
022590051129     C                   CLEAR                   WVDL
022600970901    1C     *IN32         IFEQ      *OFF
022610051129     C**!!!              TESTN                   DATVDL               31
022620040123     c                   Movel     Orgdf0        og150
022630120613     c                   testn                   §ogvol               31
022640970901     C*
022650040123    2C**!!!*IN31         IFEQ      '1'
022660051129     C**!!!DATVDL        ANDGT     *ZEROS
022670120613     c***!!!             If        §ogspv = 'S'
022680120613     c                   if        *in31 and §ogvol>*zeros
022690051129     C                   MOVEL     'S'           WVDL              1
022700970901    2C                   END
022710970901    1C                   END
022720941019     C**
022730941019     C* TIPO SERVIZIO
022740941019     C**
022750090604     c                   clear                   ds5e
022760941014     C                   MOVEL     '5E'          COD
022770941014     C                   MOVEL(P)  ARBTSP        KEY
022780941212     C                   EXSR      CTABEL
022790090604     C  N30              MOVEL     TBLUNI        ds5e
022800090604     C  N30              MOVEL     §5ed08        V1DTSP
022810941220     C   30              CLEAR                   V1DTSP
022820941019     C**
022830910429     C* DATA SPEDIZIONE
022840941019     C**
022850941014     C                   MOVEL     '3'           G02ERR
022860941014     C                   Z-ADD     ARBMGS        G02INV
022870941014     C                   MOVEL     ARBAAS        G02INV
022880941014     C                   CALL      'XSRDA8'
022890941014     C                   PARM                    WLBDAT
022900941014     C*
022910941212     C                   Z-ADD     G02DAT        V1CDSP
022920941019     C**
022930930505     C* LINEA DI ARRIVO
022940941019     C**
022950070306     C     ARBLNA        CHAIN     AZORG                              30
022960941019     C  N30              MOVEL     ORGDES        V1DLNA                          L.ARRIVO
022970941019     C   30              MOVEL     *BLANKS       V1DLNA
022980000202     C                   MOVEL     ORGDE3        OG143
022990020305     C                   MOVEL     §OGNTW        LNANTW
023000020305     c                   select
023010020305     c     lnantw        wheneq    'DPD'
023020020305     c                   movel     desefdw       deseft
023030020305     c     lnantw        wheneq    'FED'
023040020305     c                   movel     deseffw       deseft
023050020305     c                   other
023060020305     c                   movel     deseftw       deseft
023070020305     c                   endsl
023080160225
023090160225     c                   if        lnantw='FED'
023100160225     c                   clear                   trulc7ds
023110160225     c                   eval      ic7tntw='FED'
023120160225     c                   eval      ic7tip='FM'
023130160225     c                   call      'TRULC7R'
023140160225     c                   parm                    trulc7ds
023150160225     c                   eval      fedm_ksc=oc7kscc
023160160225     c                   movel     oc7ctrc       fedm_ctr
023170160225     c
023180160225     c                   clear                   trulc7ds
023190160225     c                   eval      ic7tla='L'
023200160225     c                   eval      ic7tntw='FED'
023210160225     c                   eval      ic7tip='FD'
023220160225     c                   call      'TRULC7R'
023230160225     c                   parm                    trulc7ds
023240160225     c
023250160225     c                   eval      fedd_ksc=oc7kscc
023260160225     c                   movel     oc7ctrc       fedd_ctr
023270160225     c                   endif
023280070906     c*****
023290070906     c* Verifico se destinatario disagiato
023300070906     c*****
023310070906     c                   clear                   wdisagdest
023320070906     c                   clear                   tisit0ds
023330070906     c                   movel     'E'           it0tla
023340070906     c                   movel     arbnzd        it0naz
023350070906     c                   movel     arbprd        it0prv
023360070906     c                   movel     arbcad        it0cap
023370070906     c                   movel     arblod        it0loc
023380070906     c                   movel     arbind        it0ind
023390070906     c                   movel     arbrsd        it0rag
023400070906     c                   call      'TISIT5R'
023410070906     c                   parm                    tisit0ds
023420070906    1c     ot0err        ifeq      *blanks
023430070906    2c                   if        ot0dos = 'A' or ot0dos = 'D' or
023440070906     c                             ot0dos = 'S'
023450070906     c                   movel     ot0dos        wdisagdest
023460070907    3c     wdisagdest    ifeq      'D'
023470070906     c                   movel     'X'           wdisagdest
023480070906    3c                   endif
023490070906    2c                   endif
023500070906   x1c                   else
023510070906     c                   clear                   wdisagdest
023520070906    1c                   endif
023530070306     c
023540070910     c****
023550070910      * Reperisco record GEN nel fiar5
023560070910     c                   Clear                   dAr5Genpar
023570070910     c                   Eval      kAr5Trd = 'GEN'
023580070910     c     kAr5          Chain(n)  Fiar501l
023590070910     c                   If        %Found(Fiar501l)
023600070910     c                   Movel     Ar5Uni        dAr5Genpar
023610070910     c                   eval      wesar5gen='S'
023620070910     c                   endif
023630070319     c****
023640070319     c* FILIALE GESTIONE: VEDO SE GEO ATTIVA
023650070319     c****
023660070319     c                   clear                   FGSgeot
023670070319     c                   clear                   FGSddaSI
023680110906     c****               if        *in10
023690070319     c                   clear                   fnlv55ds
023700070319     C                   MOVEL     '6'           D55TPT
023710070319     C                   MOVEL     ARBLNA        D55LIN
023720070319     C                   MOVEL     DATEU         D55DRF
023730070319     C                   CALL      'FNLV55R'
023740070319     C                   PARM                    Fnlv55DS
023750110906     c                   eval      lnacons=d55tfa
023760070319     c
023770070319     c     d55tfa        CHAIN     AZORG                              30
023780070319     c                   if        not *in30
023790070319     C                   MOVEL     ORGDE8        og148                           L.ARRIVO
023800070319     C                   MOVEL     ORGDE6        og146                           L.ARRIVO
023810070319     C
023820070319     c                   movel     §oggeot       FGSgeot           1
023830070319     C
023840070319    1c                   if        §ogdda>*zeros
023850070319     c                   testn                   §ogdda               30
023860070319     c                   move      §ogdda        w001a             1
023870070319    2c                   if        *in30 and w001a>='0'
023880070319     c* Se la data = 20391231, come se non ci fosse
023890070319    3c                   if        §ogdda<>'20391231'
023900070319     c                   movel     §ogdda        w0080             8 0
023910070426     c* Se <=oggi, attiva procedura distinte automatiche
023920070426    4c                   if        w0080<=dateu
023930070319     c                   movel     'S'           FGSddaSI          1
023940070319    4c                   endif
023950070319    3c                   endif
023960070319    2c                   endif
023970070319    1c                   endif
023980070319     c
023990070319     c                   endif
024000110906     c****               endif
024010151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024020151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024030140213     c                   seton                                        77
024040140213     c                   endif
024050070329     c* Se p.o. utente o lna hanno GEO attiva, propongo F17 note
024060140130     c* non le propongo se bolla in consegna variazione da inviare a AUT e
024070140130     c* filiale partita con la nuova gestione telefonate AUT
024080151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
024090151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
024100140130     c                             and (
024110140130     c* non è in consegna
024120140130     c                             (arbndc=0 or arbdcm>0)
024130140130     c* filiale distinta non partita con nuova gestione telefonate AUT
024140140130     c                             or (%lookup(%subst
024150140130     c                                 (%editc(arbifp:'X'):7:3):skFil816tel)=0
024160140130     c                              and skfil816tel(1)<>'999')
024170140130     c* Causale variazione da non trasmettere a AUT
024180140130     c                             or d66pda=*blanks
024190140130     c                             )
024200070329     c                   seton                                        87
024210070329     c                   endif
024220941019     C**
024230941019     C* CONSEGNE PARTICOLARI
024240941019     C**
024250941019     C                   MOVEL     *BLANKS       V1DTC1
024260941019     C                   MOVEL     *BLANKS       V1DTC2
024270070914     C                   MOVEL     *BLANKS       V7DTC1
024280070914     C                   MOVEL     *BLANKS       V7DTC2
024290941212    1C     ARBTC1        IFNE      ' '
024300930505     C                   MOVEL     '1P'          COD
024310941019     C                   MOVEL(P)  ARBTC1        KEY
024320941212     C                   EXSR      CTABEL
024330941102     C  N30              MOVEL     TBLUNI        DS1P
024340941102     C   30              CLEAR                   DS1P
024350941102     C                   MOVEL     §1PDES        V1DTC1
024360941220     C                   MOVEL     §1PDES        V7DTC1
024370941102     C*
024380941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024390941222    2C   10§1PUPR        IFEQ      'S'
024400941213     C     §1PUAR        ANDNE     'S'
024410941102     C                   SETON                                        11
024420941212    2C                   ENDIF
024430941222     C* O VICEVERSA
024440941222    2C  N10§1PUAR        IFEQ      'S'
024450941222     C     §1PUPR        ANDNE     'S'
024460941222     C                   SETON                                        11
024470941222    2C                   ENDIF
024480941102     C*
024490941212    1C                   ENDIF
024500941019     C*
024510941212    1C     ARBTC2        IFNE      ' '
024520941019     C                   MOVEL     '1P'          COD
024530941213     C                   MOVEL(P)  ARBTC2        KEY
024540941212     C                   EXSR      CTABEL
024550941102     C  N30              MOVEL     TBLUNI        DS1P
024560941102     C   30              CLEAR                   DS1P
024570941102     C                   MOVEL     §1PDES        V1DTC2
024580941220     C                   MOVEL     §1PDES        V7DTC2
024590941102     C*
024600941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024610941222    2C   10§1PUPR        IFEQ      'S'
024620941213     C     §1PUAR        ANDNE     'S'
024630941102     C                   SETON                                        12
024640941212    2C                   ENDIF
024650941222     C* O VICEVERSA
024660941222    2C  N10§1PUAR        IFEQ      'S'
024670941222     C     §1PUPR        ANDNE     'S'
024680941222     C                   SETON                                        12
024690941222    2C                   ENDIF
024700941222     C*
024710941212    1C                   ENDIF
024720941019     C**
024730941019     C* SPEDIZIONE GIACENTE
024740941019     C**
024750040316     c                   Clear                   wgcfas
024760040324     c                   Clear                   wgcded
024770040324     c                   Clear                   wgcddm
024780051109     c                   Clear                   dgcpflr
024790141113if  1C**   *IN10         IFEQ      *ON
024800141113     c**   d48cvb        oreq      'CS'
024810141113     c**   d48cvb        oreq      'CR'
024820050331     C     KARB          CHAIN     Tigcp21L                           30        GIACENZA
024830941212    1C     *IN30         IFEQ      *OFF
024840050331     C     GCPFAS        ANDLT     40
024850941019     C     GCPATB        ANDEQ     ' '
024860040316     c                   Eval      wgcfas = GcpFas
024870040324     c                   Eval      wgcded = GcpDed
024880040324     c                   Eval      wgcddm = GcpDdm
024890051109     c                   Movel     gcpflr        dgcpflr
024900941019     C                   SETON                                        04
024910941212    1C                   ENDIF
024920040219      * se non ho trovato il rcd di giacenza in arrivo provo in partenza
024930040219      * solo se sto facendo una variazione 'CS' o 'CR'
024940051109      * questo solo per accendere l'indicatore 04 di giacenza aperta, non serve per la
024950051109      * variazione della data consegna richiesta, questa è possibile solo se la giacenza
024960051109      * è aperta in arrivo!!!!
024970141113      * Ora lo 04 mi serve anche per il controllo dell'abilitazione dei dirottamenti sia
024980141113      * in partenza che in arrivo e quinidi lo faccio sempre
024990141113if  2c***                If        *In30 and
025000141113     c***                          (d48cvb = 'CS' or d48cvb = 'CR')
025010141113if  2c                   If        *In30
025020050331     c     kArb          Chain     Tigcp01l
025030050331if  3c                   If        %Found(Tigcp01l) and GcpFas < 50 and
025040040219     c                             GcpAtb = *Blanks
025050040219     c                   Seton                                        04
025060040316     c                   Clear                   wgcfas
025070040324     c                   Clear                   wgcded
025080040324     c                   Clear                   wgcddm
025090040219    3c                   EndIf
025100040219    2c                   EndIf
025110141113    1C*****              ENDIF
025120051129     C* DETERMINO SE IL VOLUME VDL (CHE EVENTUALMENTE SOSTITUIRA' IL
025130970418     C* VOLUME DA FATTURARE) E' PARZIALE O TOTALE
025140970418     C                   CLEAR                   WFVF
025150050111     C     arbncl        IFGT      ARBNCR
025160970418     C                   MOVEL     'Z'           WFVF                           VOLUME PARZ.
025170970418     C                   ELSE
025180970418     C                   MOVEL     'T'           WFVF                           VOLUME TOTAL
025190970418     C                   ENDIF
025200051129      * Determino se il peso VDL è parziale o totale
025210030122     c                   Clear                   VidTpk
025220030123     c                   Clear                   wfpf
025230030122     c                   If        ArbNcp > *Zeros
025240050111     c                   If        arbncl > ArbNcp
025250030122     c                   Eval      VidTpk = 'Par.'                              Peso Parziale
025260030123     c                   Eval      wfpf = 'Z'
025270030122     c                   Else
025280030122     c                   Eval      VidTpk = 'Tot.'                              Peso Totale
025290030123     c                   Eval      wfpf = 'T'
025300030122     c                   EndIf
025310030122     c                   EndIf
025320030729      * Recupero i Bancali
025330030729     c                   If        %Subst(ArbGva:1:1) = 'B'
025340030729     c                   Clear                   dAr5Ban
025350030729     c                   Eval      kAr5Trd = 'BAN'
025360030729     c     kAr5          Chain(N)  Fiar501l
025370030729     c                   If        %Found(Fiar501l)
025380030729     c                   Movel     Ar5Uni        dAr5Ban
025390030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
025400030729     c                   EndIf
025410030729     c                   EndIf
025420030729      * Recupero i Bancali e i colli originali
025430030729     c                   If        %Subst(ArbGva:1:1) = 'O'
025440030729     c                   Clear                   dAr5Bnb
025450030729     c                   Eval      kAr5Trd = 'BNB'
025460030729     c     kAr5          Chain(N)  Fiar501l
025470030729     c                   If        %Found(Fiar501l)
025480030729     c                   Movel     Ar5Uni        dAr5Bnb
025490030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
025500030729     c                   Z-Add     §Ar5bCor      V1cCor
025510030729     c                   Eval      *In70 = *On
025520030729     c                   EndIf
025530030729     c                   EndIf
025540130924      * Memorizzo se presente avviso al destinatario per passarlo alla tassazione
025550110629     c                   clear                   wemd
025560130924     c                   Clear                   dAr5emd
025570110629     c                   Eval      kAr5Trd = 'EMD'
025580130924     c     kAr5          chain(N)  Fiar501l
025590130924     c                   if        %found(fiar501l)
025600130924     c                   Movel     Ar5Uni        dAr5EMD
025610130930     c                   eval      wesar5EMD='S'
025620130924     c                   select
025630130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp=' '
025640110629     c                   eval      wemd='S'
025650130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp='S'
025660130924     c                   eval      wemd='E'
025670130924     c                   when      emd_§ar5smp='S'
025680130924     c                   eval      wemd='X'
025690130924     c                   endsl
025700110629     c                   endif
025710061107     C**
025720071022     C* F N A R B D 0 F  record del codice fiscale e partita IVA
025730061107     C**
025740061107    1C     VIDCVR        IFEQ      'FI'
025750071022     c*
025760061129     c* Memorizzo se bolla già trasmessa o meno
025770070528     c*  solo in partenza(in arrivo è per forza trasmessa
025780151222     c  n10              movel     arbft2        bollaft2          1
025790151222     c**                 if        wfile='P'
025800151222     c**                 movel     arbft2        bollaft2          1
025810151222     c***                endif
025820071022     c* Dati per partita IVA
025830071022     c
025840071025     c                   clear                   ar4not_P
025850071022     C                   MOVEL     'P'           WTRC
025860061107     C     KARB2         CHAIN(N)  FiAR4000                           30
025870071025     c  n30              movel     ar4not        ar4not_p
025880071022     C                   MOVEL     'Q'           WTRC
025890071022     C     KARB2         CHAIN(N)  FiAR4000                           30
025900061107     c
025910061107     c* Verifico se bolla singola .Se doppia guardo se sono in partenza
025920061107     c*  o arrivo
025930061107     c* carico il mittente se bolla franco in partenza sempre
025940061107     c*  o franco singola in arrivo
025950061107    2c                   if        (flgtb1='F'  and not *in10) or
025960061107     c                             (flgtb1='F'  and §3atb2=*blanks)
025970061107     c                   eval      v11mide='M'
025980061107     c* Mittente
025990061107     c                   eval      v11decmd=CMITT
026000061107     c                   eval      v11cod=arbksc
026010061107     c                   eval      v11rsc=arbrsm
026020061107     c                   eval      wnar=arbnzm
026030071025     c* codice fiscale
026040061107     c                   if        not *in30 and not *in20
026050061107     c                   movel     ar4not        v11cdfis
026060061107     c                   else
026070061107     c                   clear                   v11cdfis
026080061107     c                   endif
026090071025     c* Partita IVA
026100071107     c****               if        not *in20
026110071025     c                   if        %subst(ar4not_P:1:16)<>*blanks
026120071025     c                   movel     ar4not_P      v11pid
026130071025     c                   else
026140071025     c                   movel     arbcpi        v11pid
026150071025     c                   endif
026160071107     c****               endif
026170071025     c
026180061107     c* In arrivo di fatto non posso inserire cod.fiscale su un franco
026190061107     c*    singolo: proteggo il campo
026200061107    3c                   if        *in10 and §3atb2=*blanks
026210061107     c                   seton                                        76
026220061107    3c                   endif
026230061107    2c                   endif
026240061107     c
026250061107     c* Carico il destinatario in arrivo sempre, in partenza se singola
026260061107    2c                   if        (flgtb1='A') or
026270061107     c                             (flgtb1='F'  and §3atb2<>*blanks and
026280061107     c                             *in10)
026290061107     c                   eval      v11mide='D'
026300061107     c                   eval      v11decmd=Cdest
026310061107     c                   eval      v11rsc=arbrsd
026320061107     c                   eval      wnar=arbnzd
026330061107     c                   if        not *in30 and not *in20
026340061107     c                   move      ar4not        v11cdfis
026350061107     c                   else
026360061107     c                   clear                   v11cdfis
026370061107     c                   endif
026380071025     c* Partita IVA
026390071107     c*****              if        not *in20
026400071025     c                   if        %subst(ar4not_P:20:16)<>*blanks
026410071026     c                   move      ar4not_P      v11pid
026420071025     c                   else
026430071107     c                   movel     arbcpi        v11pid
026440071025     c                   endif
026450071107     c*****              endif
026460071025     c* In partenza proteggo sempre
026470061107     c  n10              seton                                        76
026480061107     c* codice destinatario: dal fiar6 prima o seconda bolla
026490061107    3c                   if        §3atb2<>*blanks
026500061107     c                   eval      v11cod=ksc2bol
026510061107   x3c                   else
026520061107     C                   MOVEL     '1'           WTRC
026530061107     C     KARB2         CHAIN(N)  FIAR6000                           30
026540061107     c   30              eval      v11cod=arbksc
026550061107     c  n30              eval      v11cod=ar6ksc
026560061107    3c                   endif
026570061107    2c                   endif
026580061107     C*
026590061107     C* PGM RICHIAMATO
026600061107     c                   if        *in20
026610071025     c* in CPI c'e' il codice fiscale
026620061107     c                   movel     §bdcpi        v11cdfis
026630071107     c* in RSD c'e' la partita IVA solo dalla data attivazione
026640071203     c***                if        dateu>=§vpopiv
026650071025     c                   movel     §bdrsd        v11PID
026660071203     c***                endif
026670071107     c                   endif
026680071025     c
026690071025     c* controllo Partita IVA se privato
026700071025    2c                   if        v11pid<>*blanks
026710071025     C     'PRIVATO'     SCAN      v11pid        posiz                    30
026720071025     c* solo numeri
026730071025     c* "PRIVATO" senza iso
026740071025    3c     *in30         ifeq      *off
026750071025     c     v11isd        andge     *zeros
026760071025     c     *in30         oreq      *on
026770071025     c     posiz         andlt     3
026780071025     c                   movel     v11pid        w016a            16
026790071025     c                   movel     w016a         v11cpd
026800071025     C                   CLEAR                   V11isd
026810071025    3c                   endif
026820071025    2c                   endif
026830061107     c*
026840061107    1c                   endif
026850941019     C**
026860941027     C* F N A R B D 0 F
026870941019     C**
026880941019    1C     FLGCVR        IFEQ      'I'
026890941019     C*
026900941019    2C     VIDCVR        IFEQ      'I0'
026910140407    2C     VIDCVR        oreQ      'I7'
026920140407    2C     VIDCVR        oreQ      'I8'
026930140407    2C     VIDCVR        oreQ      'I9'
026940941019     C                   SETON                                        01          DESTINATAR
026950941019     C                   ELSE
026960941019     C                   SETON                                        02          PESO VOL
026970941212    2C                   END
026980941025     C*
026990941025     C* 2 RAGIONE SOCIALE DESTINATARIO
027000941212     C                   MOVEL     'D'           WTRC
027010060222     C     KARB2         CHAIN(N)  Fiar4000                           30
027020941212     C     *IN30         IFEQ      *OFF
027030941212     C                   MOVEL     'E'           WAR4D             1
027040941212     C* LO SALVO PER LA STORICIZZAZIONE
027050970520     C                   MOVE      AR4NOT        SAVRD2
027060941212     C                   ENDIF
027070061116     C*
027080061116     C* codice fiscale destinatario
027090061116     C                   MOVEL     'Q'           WTRC
027100061116     C     KARB2         CHAIN(N)  Fiar4000                           30
027110061116     C     *IN30         IFEQ      *OFF
027120061116     C                   MOVEL     'E'           WAR4Q
027130061116     C* Se c'e' il cod fiscale del detinatario lo memorizzo
027140061116     C                   MOVE      AR4NOT        vidcdf
027150061116     c                   if        vidcdf<>*blanks
027160061116     C                   MOVEL     'D'           WAR4Q
027170061116     C                   ENDIF
027180061116     C                   ENDIF
027190150206
027200150206     C* LOCALITA' NORMALIZZATA
027210150206     C                   MOVEL     'X'           WTRC
027220150206     C     KARB2         CHAIN(N)  Fiar4000                           30
027230150206     C     *IN30         IFEQ      *OFF
027240150206     C                   MOVEL     'E'           WAR4X             1
027250150206     C                   ENDIF
027260061116     c
027270051129     C* VISUALIZZO ANCHE IL VOLUME VDL
027280941214     C                   MOVEL     ARBVLC        V1CVLC
027290941214     C                   MOVEL     ARBNCR        V1CNCR
027300031120
027310040123      * Reperisco il referente e il telefono e magazzino giacenza
027320070910     c                   If        wesar5gen='S'
027330070910     c                   Eval      VidRef =  PAr5Ref
027340070910     c                   Eval      VidTeld = PAr5Teld
027350070910     c                   Eval      VidZng =  PAr5Zng
027360031120     c                   EndIf
027370141203     c                   eval      savzng = vidzng
027380941019     C*
027390941019     C* PGM NON RICHIAMATO
027400941019    2C     *IN20         IFEQ      *OFF
027410970520     C                   CLEAR                   VIDRSD
027420941025     C                   MOVEL     ARBRSD        VIDRSD
027430941027     C     WAR4D         IFEQ      'E'
027440061116     C                   MOVE      savrd2        VIDRSD
027450941025     C                   ENDIF
027460941019     C                   MOVEL     ARBIND        VIDIND
027470941019     C                   MOVEL     ARBLOD        VIDLOD
027480941019     C                   MOVEL     ARBPRD        VIDPRD
027490941019     C                   MOVEL     ARBCAD        VIDCAD
027500061115     c
027510140428     c*05 off se --> bolla singola in assegnato
027520140428     c*              bolla doppia
027530071025    3C     *IN05         IFEQ      *OFF
027540140428     c* se bolla doppia in partenza la partita iva che ho caricato non è quella del
027550140428     c* destinatario e quindi la carico solo se sono in arrivo (10 on)
027560151222     c***  *in10         ifeq      *on
027570151222     c     wfile         ifeq      'A'
027580140428     c     §3atb2        oreq      *blanks
027590941220     C                   MOVEL     ARBCPI        VIDPID
027600140428     c                   endif
027610071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027620050922     c* solo numeri
027630050922     c* "PRIVATO" senza iso
027640071025    4c**   *in30         ifeq      *off
027650071025     c**   vidisd        andge     *zeros
027660071025     c**   *in30         oreq      *on
027670071025     c**   posiz         andlt     3
027680071025     C**                 CLEAR                   VIDPID
027690071025     c**                 movel     arbcpi        vidcpd
027700071025    4c**                 endif
027710071025    3C                   ENDIF
027720941220     C*
027730030122     c                   Z-Add     ArbPkc        VidPkc
027740941027     C                   MOVEL     ARBPKF        VIDPKF
027750941027     C                   MOVEL     ARBVLF        VIDVLF
027760941027     C                   MOVEL     ARBFVF        VIDFVF
027770941019     C                   MOVEL     ARBFIN        VIDFIN
027780941019     C                   MOVEL     ARBNZD        VIDNZD
027790941019     C* FERMO DEPOSITO
027800941019    3C     ARBFFD        IFEQ      'S'
027810941019     C                   MOVEL     'SI'          VIDFFD
027820941019     C                   ELSE
027830941019     C                   CLEAR                   VIDFFD
027840941019    3C                   ENDIF
027850941019     C**
027860941019   X2C                   ELSE
027870941019     C**
027880941019     C* PROGRAMMA     RICHIAMATO: PRENDO CAMPI DA DSARBD
027890941019     C                   MOVEL     §BDRSD        VIDRSD
027900970520     C                   MOVE      §BDRD2        VIDRSD
027910941019     C                   MOVEL     §BDIND        VIDIND
027920941019     C                   MOVEL     §BDLOD        VIDLOD
027930941019     C                   MOVEL     §BDPRD        VIDPRD
027940941019     C                   MOVEL     §BDCAD        VIDCAD
027950941019     C                   MOVEL     §BDNZD        VIDNZD
027960060308     c                   movel     §bdref        vidref
027970060308     c                   movel     §bdtel        vidteld
027980061115     c                   clear                   vidcdf
027990071025    3C     *IN05         IFEQ      *OFF
028000950102     C                   MOVEL     §BDCPI        VIDPID
028010071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
028020050922     c* solo numeri
028030050922     c* "PRIVATO" senza iso
028040071025    4c**   *in30         ifeq      *off
028050071025     c**   vidisd        andge     *zeros
028060071025     c**   *in30         oreq      *on
028070071025     c**   posiz         andlt     3
028080071025     C**                 CLEAR                   VIDPID
028090071025     c**                 movel     §bdcpi        vidcpd
028100071025    4c**                 endif
028110071025    3C                   ENDIF
028120941019     C                   MOVEL     §BDFIN        VIDFIN
028130941027     C                   MOVEL     §BDPKF        VIDPKF
028140941027     C                   MOVEL     §BDVLF        VIDVLF
028150941209     C                   MOVEL     §BDFVF        VIDFVF
028160941019     C* FERMO DEPOSITO
028170141017     c* §BDFFD= 'S' => si fermo deposito; Si alert
028180141017     c* §BDFFD= 'Y' -->si fermo deposito; no alert
028190141017     c* Serve per le chiamate cieche in quanto il campo a video per la gestione
028200141017     c* del contatto (VIDTFD) non è presente nella dsarbd.
028210941019    3C     §BDFFD        IFEQ      'S'
028220141017    3C     §BDFFD        oreq      'Y'
028230941019     C                   MOVEL     'SI'          VIDFFD
028240941019     C                   ELSE
028250941019     C                   CLEAR                   VIDFFD
028260941019    3C                   ENDIF
028270941216    2C                   ENDIF
028280141017      *
028290141017     c                   if        §bdffd='Y'
028300141017     c                   eval      vidtfd='SI'
028310141017     c                   endif
028320071025     c
028330071025     c* controllo Partita IVA se privato
028340071025    2c                   if        vidpid<>*blanks
028350071025     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028360071025     c* solo numeri
028370071025     c* "PRIVATO" senza iso
028380071025    3c     *in30         ifeq      *off
028390071025     c     vidisd        andge     *zeros
028400071025     c     *in30         oreq      *on
028410071025     c     posiz         andlt     3
028420071025     c                   movel     vidpid        w016a
028430071025     c                   movel     w016a         vidcpd
028440071025     C                   CLEAR                   VIDisd
028450071025    3c                   endif
028460071025    2c                   endif
028470991001     C*
028480991001    2C     *IN01         IFEQ      *ON
028490970808     C* MEMORIZZO CAP/PROVINCIA E LOCALITA' DEL DESTINATARIO
028500970808     C* (MI SERVIRA' NELLA ROUTINE DEI CONTROLLI
028510970808     C*
028520970808     C                   MOVE      VIDCAD        SAVCAD                         CAP
028530970808     C                   MOVE      VIDCAD        VARCAD                          "
028540970808     C                   MOVE      VIDCAD        WCAD                            "
028550970808     C                   MOVE      VIDCAD        WCAD1                           "
028560970808     C                   MOVE      VIDLOD        SAVLOD                         LOCALITA'
028570970808     C                   MOVE      VIDLOD        VARLOD                            "
028580970808     C                   MOVE      VIDLOD        WLOD                              "
028590970808     C                   MOVE      VIDLOD        WLOD1                             "
028600060111     c* memorizzo anche gli altri dati del destinatario
028610060111     c                   move      vidrsd        savrsd
028620061116     c                   move      vidrsd        savrsdQ
028630060111     c                   move      vidrsd        wrsd1
028640060111     c                   move      vidind        savind
028650060111     c                   move      vidind        wind1
028660060111     c                   move      vidprd        savprd
028670060111     c                   move      vidprd        wprd1
028680060111     c                   move      vidnzd        savnzd
028690060111     c                   move      vidnzd        wnzd1
028700970808     C*
028710950419     C* SE P.IVA DEL VIDEO = BLANKS LA PRENDO DALL'ESTENSIONE SE
028720950419     C* ESISTE
028730950419     C     VIDPID        IFEQ      *BLANKS
028740950419     C                   MOVEL     'P'           WTRC
028750060222     C     KARB2         CHAIN(N)  Fiar401L                           30
028760950419     C     *IN30         IFEQ      *OFF
028770950419     C                   MOVE      AR4NOT        VIDPID
028780050922     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028790050922     c* solo numeri
028800050922     c* "PRIVATO" senza iso
028810050922     c     *in30         ifeq      *off
028820050922     c     vidisd        andge     *zeros
028830050922     c     *in30         oreq      *on
028840050922     c     posiz         andlt     3
028850050922     C                   CLEAR                   VIDPID
028860050922     c                   move      ar4not        wpiva
028870050922     C                   MOVEL     WPIVA         VIDCPD
028880050922     c                   endif
028890050922     C**   VIDISD        IFGE      *ZEROS
028900050922     C**                 CLEAR                   VIDPID
028910050922     C**                 MOVE      AR4NOT        WPIVA
028920050922     C**                 MOVEL     WPIVA         VIDCPD
028930050922     C**                 ENDIF
028940950419     C                   END
028950950419     C                   END
028960950419     C*
028970160118     C                   SETOFF                                       717276
028980991001     C* SE ASSEGNATO 9999 GIA' CONTABILIZZATO PROTEGGO DATI
028990991001     C* DESTINATARIO (ANCHE LA P.IVA MA SOLO SE DIVERSA DA BLANKS)
029000991001     C                   MOVE      ARBKSC        CO1KSC            4 0
029010991001    3C     CO1KSC        IFEQ      9999
029020991001     C                   CLEAR                   AR6DFT
029030991001     C                   MOVEL     '1'           WTRC
029040991001     C     KARB2         CHAIN(N)  FIAR6000                           30
029050991001    4C     AR6DFT        IFNE      *ZEROS
029060000104     C     AR6DFT        ANDLE     WDTU10
029070991001     C                   SETON                                        71
029080000104    5C     VIDPID        IFNE      *BLANKS
029090000104     C                   SETON                                        72
029100991001    5C                   END
029110991001    4C                   END
029120991001    3C                   END
029130160118     c* Proteggo la partita iva/cod.fiscale anche se dagli arrivi sto manutenzionando BLP
029140160115     c                   if        *in10 and wfile='P'
029150160118     C                   SETON                                        7276
029160160115     c                   endif
029170991001    2C                   END
029180941019     C*
029190941222     C* PROTEGGO VOLUME SE NON VARIABILE
029200941216     C* SE SI TRATTA DI VARIAZIONE DI PESO, PROTEGGO SEMPRE IL VOLUME
029210941216    2C     D48CVB        IFEQ      'I2'
029220941216     C* PROTEGGO IL PESO
029230941216     C                   SETON                                        88
029240941216     C*
029250941019     C                   MOVEL     '7T'          COD
029260051214     C                   MOVEL(P)  VIDFVF        KEY
029270941212     C                   EXSR      CTABEL
029280941212     C   30              SETON                                        89
029290941222     C     *IN30         IFEQ      *OFF
029300941222     C                   MOVEL     TBLUNI        DS7T
029310091106     c
029320151222     c                   if        *in10
029330091106     c                   eval      wvariavol=§7tvra
029340120705     c                   eval      wvariavolEXP=§7tvraEXP
029350091106     c                   else
029360091106     c                   eval      wvariavol=§7tvrp
029370120705     c                   eval      wvariavolEXP=§7tvrPEXP
029380091106     c                   endif
029390091106     c
029400091106     c* Proteggo il volume in arrivo o in partenza se il flag è ' ' oppure
029410091124     c*  non corrisponde il tipo bolla
029420091106     c                   select
029430091106     c                   when      wvariavol=' '
029440091106     C                   SETON                                        89
029450091106     c                   when      Wvariavol='A' and flgtb1='F'
029460091106     C                   SETON                                        89
029470091106     c                   when      Wvariavol='F' and flgtb1='A'
029480091106     C                   SETON                                        89
029490091106     c                   endsl
029500091106     c
029510120705     C* Se  BOLLA EXPORT,  in aggiunta controllo il relativo flag
029520120705     c                   if        *in89 and (lnantw='FED' or
029530120705     c                                        lnantw='EEX' or lnantw='DPD')
029540120705     c                   select
029550120705     c                   when      WvariavolEXP='3'
029560120705     C                   SETOff                                       89
029570120705     c                   when      WvariavolEXP='A' and flgtb1='A'
029580120705     C                   SETOff                                       89
029590120705     c                   when      Wvariavolexp='F' and flgtb1='F'
029600120705     C                   SETOff                                       89
029610120705     c                   endsl
029620120705     c                   endif
029630091106     c
029640941222    3C                   ENDIF
029650941216     C*
029660941216   X2C                   ELSE
029670941216     C                   SETON                                        89
029680941216    2C                   ENDIF
029690030604
029700030604      * se linea arrivo FedEx e il volume è "T" proteggo sempre
029710030604      * (già da tabella "7T" il volume di tipo "T" non è sostituibile,
029720030604      *  faccio lo stesso la modifica nel caso di variazione della tabella)
029730030604     c                   If        *In02 and LnaNtw = 'FED' and vidfvf = 'T'
029740030604     c                   Eval      *In89 = *On
029750030604     c                   EndIf
029760941019    1C                   ENDIF
029770130930     c
029780130930     C**
029790130930     C* F N A R B N 0 F
029800130930     C**
029810130930    1C     FLGCVR        IFEQ      'N'
029820130930
029830130930      * Reperisco il referente e il telefono e magazzino giacenza
029840130930    2c                   if        not  *in20
029850130930
029860130930    3c                   if        d48cvb='NE'
029870131001     c                   seton                                        868483
029880130930     c                   If        wesar5gen='S'
029890130930     c                   Eval      VidRef =  PAr5Ref
029900130930     c                   Eval      VidTeld = PAr5Teld
029910130930     c                   EndIf
029920130930     c                   if        wesar5emd='S'
029930130930     c                   eval      videml = emd_§ar5eml
029940130930     c                   eval      vidtsms= emd_§ar5tel
029950130930     c                   endif
029960130930    3c                   endif
029970130930
029980130930    3c                   if        d48cvb='NT'
029990131001     c                   seton                                        81
030000130930     c                   eval      vidnt1=v1cnt1
030010130930     c                   eval      vidnt2=v1cnt2
030020130930    3c                   endif
030030130930     c
030040130930   x2c                   else
030050130930    3c                   select
030060130930     c
030070131002     c                   when      §bntrd='NOT'
030080131002     c                   if        §bnu_sn='S'
030090131001     c                   seton                                        81
030100130930     c                   movel     §bnuni70      vidnt1
030110130930     c                   move      §bnuni70      vidnt2
030120131002     c                   else
030130131002     C                   MOVEL     MSG(24)       D48MSG
030140131002     C                   MOVEL     '1'           D48ERR
030150131002     c                   leavesr
030160131002     c                   endif
030170131008     c* se la causale variazione non è NT --> errore
030180131008     c                   if        d48cvb<>'NT'
030190131008     C                   MOVEL     MSG(31)       D48MSG
030200131008     C                   MOVEL     '1'           D48ERR
030210131008     c                   leavesr
030220131008     c                   endif
030230131008
030240130930     c                   other
030250131008
030260131008     c* se la causale variazione non è NE --> errore
030270131008     c                   if        d48cvb<>'NE'
030280131008     C                   MOVEL     MSG(31)       D48MSG
030290131008     C                   MOVEL     '1'           D48ERR
030300131008     c                   leavesr
030310131008     c                   endif
030320130930     c
030330131001     c                   if        §bnu_sn='S'
030340130930     c                   eval      videml = §bnuni70
030350131001     c                   seton                                        86
030360131001     c                   endif
030370131001     c                   if        §bnr_sn='S'
030380131001     c                   eval      vidteld= §bntel
030390130930     c                   eval      vidref = §bnref
030400131001     c                   seton                                        84
030410131001     c                   endif
030420131001     c                   if        §bnc_sn='S'
030430131001     c                   eval      vidtsms= §bncel
030440131001     c                   seton                                        83
030450131001     c                   endif
030460131002     c                   if        *in86=*off and *in84=*off and *in83=*off
030470131002     C                   MOVEL     MSG(24)       D48MSG
030480131002     C                   MOVEL     '1'           D48ERR
030490131002     c                   leavesr
030500131002     c                   endif
030510131001     c
030520130930    3c                   endsl
030530130930     c
030540130930    2c                   endif
030550130930    1c                   endif
030560950104     C**
030570990930     C* F I A R B T 0 F
030580950104     C**
030590100120     c
030600941019    1C     FLGCVR        IFEQ      'T'
030610941019     C                   SELECT
030620941019     C*
030630941102     C     D66RTA        WHENEQ    'S'
030640950616     C                   SETON                                        1529        RITASSAZ
030650941102     C*
030660941102     C     D66BSF        WHENEQ    'S'
030670941019     C                   SETON                                        13          VARIA  SF
030680941019     C*
030690941102     C     D66BFI        WHENEQ    'S'
030700991006     C                   SETON                                        1407        VARIA  FAT
030710941019     C*
030720941019     C                   OTHER
030730941019     C                   SETON                                        36          TASS FRANC
030740941019     C                   ENDSL
030750941102     C* PULIZIA SFL
030760941102     C                   SETON                                        85
030770941215     C                   WRITE     LR48C03
030780941102     C                   SETOFF                                       85
030790941215     C*
030800941215     C                   MOVEL     *BLANKS       DESKSC
030810950104     C   10              MOVEL     ARBRSD        W008A             8
030820950104     C   10              MOVEL     W008A         DESKSC
030830950104     C  N10              MOVEL     ARBRSM        DESKSC
030840131120     c   10              clear                   sav_ift
030850941102     C*
030860941102     C*
030870990930     C* TASSAZIONE IN FIAR6
030880990930     C                   MOVEL     '1'           WTRC
030890990930     C     KARB2         CHAIN     FIAR6000                           30
030900990702    2C     *IN30         IFEQ      *OFF
030910990701     C                   MOVEL     'E'           WAR6
030920990701     C                   MOVEL     AR6NFT        VIDNFT
030930990701     C                   MOVEL     AR6FIV        VIDFIV
030940990702     C                   Z-ADD     AR6IMV        SAVIMV
030950151016     C                   Z-ADD     AR6IMV        SAVIMV_fat
030960131120     c   10              z-add     ar6ift        sav_ift
030970990702     C* PROTEZIONE DATI FATTURA PER PREPAGATI
030980990702    3C  N10VIDNFT        IFLT      §QTNFT
030990990719     C     VIDNFT        ANDGT     0
031000990701     C                   SETON                                        23
031010990702     C                   Z-ADD     AR6IFT        VIDIFT
031020990702    3C                   ENDIF
031030991001     C* PROTEZIONE DIVISA PER PREPAGATI
031040991001    3C  N10VIDNFT        IFGT      0
031050991012     C                   SETON                                        0708
031060991001    3C                   ENDIF
031070991108     C* SE PREPAGATO O P.A. FATTURA GIA'CONTAB. MODIFICO SOLO IMP.ASSIC
031080991008     C* E VALORE MERCE DICHIARATO
031090991108    3C     AR6DFT        IFGT      *ZEROS
031100000104     C     AR6DFT        ANDLE     WDTU10
031110991008     C                   SETON                                        2973
031120991008    3C                   ENDIF
031130991029    2C                   ENDIF
031140991029     C**
031150991029     C* SE ASSEGNATO GIA' INCASSATO, FACCIO MODIFICARE SOLO IMP.ASSIC EARIAZIONE
031160991029     C*  VALORE MERCE DICHIARATO
031170130409    1C*  10FLGCVR        IFEQ      'T'
031180130409    2C*    §7LFIN        IFEQ      'S'
031190130409     C*    ARBICA        ANDNE     ' '
031200130409     C*    ARBICA        ANDNE     'R'
031210991029     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031220130409     C*                  SETON                                        2973
031230130409    2C*                  END
031240991108     C* SE ICA = 'R' E C'E' INCASSO PARZ<IALE IN ARI, PROTEGGO
031250991108     C*  LO STESS, FACCIO SOLO MODIFICARE IAS
031260130409    2C*    ARBICA        IFEQ      'R'
031270130409     C*    KARI          CHAIN     FIARI01L                           30
031280130409    3C* N30ARIATB        IFEQ      ' '
031290991108     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031300130409     C*                  SETON                                        2973
031310130409    3C*                  ENDIF
031320130409    2C*                  ENDIF
031330130409    1C*                  ENDIF
031340130410     c* PROTEZIONE CAMPI PER POTER VARIARE SOLO IAS E VMD
031350130410     c* Se bolla già contabilizzata e variabile anche dopo contabilizzazione
031360130410     c* oppure se bolla già incassata e variabile anche dopo incasso (V.di fnlr66r)
031370130410     c                   if        d66bdi='S'
031380130409     C                   SETON                                        2973
031390130409     c                   endif
031400990701     C**
031410950807     C* VEDO SE ESISTE AMCHE L'AR7
031420990930     C     KARB2         SETLL     FIAR7000                               30
031430950807     C   30              MOVEL     'E'           WAR7
031440060201     c
031450060201      * Calcolo data limite mod. importo da assicurare
031460060201     c     *iso          Move      ArbDsp        wdataiso
031470060201     c                   adddur    §vpomia:*d    wdataiso
031480060201      *
031490060201     c                   do        *hival
031500060201      * verifico se è un giorno lavorativo
031510060201     c     *iso          move      wdataiso      ds_data
031520060201     C                   eval      kann = ds_anno
031530060201     C                   eval      kmes = ds_mese
031540060201     C     kazcln        chain     azcln01l
031550060201     C                   if        %found(azcln01l)
031560060201     C                   if        MAT(ds_giorno) =  'F'  or
031570060201     C                             POM(ds_giorno) =  'F'
031580060201      * se non va bene aggiungo 1 giorno
031590060201     c                   adddur    1:*d          wdataiso
031600060201     C                   iter
031610060201     c                   else
031620060201     c                   leave
031630060201     C                   endif
031640060201     c                   else
031650060201     c                   leave
031660060201     C                   endif
031670060201
031680060201     c                   enddo
031690060201
031700060201     c                   eval      wdatalimias=wdataiso
031710080625     c                   clear                   forzaias2
031720080625     c                   clear                   forzaias
031730060201     c
031740060201     c* IAS modificabile solo nei primi 2 gg  tassaz sempre
031750060201     c                   if        §utemtf=' '  and
031760060201     c                             wdataoggi>wdatalimIas
031770060201     c                   seton                                        75
031780060201     c                   endif
031790060201     c* solo IAs ma sempre
031800060201     c                   if        §utemtf='I'
031810060201     c                   seton                                        74
031820060201     c                   endif
031830060201     c* Nulla
031840060201     c                   if        §utemtf='N'
031850060201     c                   seton                                        7475
031860060201     c                   endif
031870991012     C*
031880060201     C*
031890060201     C                   CLEAR                   V3CEV1
031900060201     C                   CLEAR                   V3CSV1
031910060201     C                   CLEAR                   V3CVA1
031920060201     C                   CLEAR                   VARVA1
031930060201     C                   Z-ADD     1             NRR
031940060201     C                   DO        20            NRR
031950060201     C                   WRITE     LR48S03
031960060201     C                   ENDDO
031970920128     C* PROGRAMMA NON RICHIAMATO: DATI DA BOLLA
031980941019    2C     *IN20         IFEQ      *OFF
031990941019     C*
032000941019     C                   Z-ADD     ARBIAS        VIDIAS
032010950426     C                   Z-ADD     ARBIAS        COMIAS
032020941209     C                   MOVEL     ARBVAS        VIDVAS
032030941215     C                   MOVEL     ARBVAS        VARVAS
032040941019     C                   MOVEL     ARBKSC        VIDKLP
032050950424     C                   MOVEL     ARBKSC        VARKLP
032060941019     C                   MOVE      ARBKSC        VIDKSC
032070970822     C                   MOVE      ARBKSC        MEMKSC
032080950424     C                   MOVE      ARBKSC        VARKSC
032090941019     C                   MOVEL     ARBCTR        VIDCTR
032100050202     C                   MOVEL     ARBCTR        SAVCTR
032110060511     C                   MOVEL     ARBCTR        CTRBOLLA          3
032120941215     C                   MOVEL     ARBQFT        VIDQFT
032130950424     C                   MOVEL     ARBQFT        VARQFT
032140941215     C                   MOVEL     ARBVMD        VIDVMD
032150941215     C                   MOVEL     ARBVAD        VIDVAD
032160941215     C                   MOVEL     ARBVAD        VARVAD
032170990930     C* TASSAZIONE IN FIAR6
032180941102    3C     WAR6          IFEQ      'E'
032190990930     C                   MOVEL     AR6DIV        VIDDIV
032200991006     C                   MOVEL     AR6DIV        WDIV
032210941019     C                   MOVEL     AR6POR        VIDPOR
032220991006     C                   MOVEL     AR6POR        VARPOR
032230941019     C                   MOVEL     AR6IMV        VIDIMV
032240941019     C                   MOVEL     AR6CEI        VIDCEI
032250941102     C*
032260941102     C                   Z-ADD     0             NRR
032270941209     C                   MOVEL     AR6SV1        WSV1
032280941209     C                   Z-ADD     AR6VA1        WVA1
032290941102     C                   EXSR      CARVAR
032300941209     C                   MOVEL     AR6SV2        WSV1
032310941209     C                   Z-ADD     AR6VA2        WVA1
032320941102     C                   EXSR      CARVAR
032330941212    4C     AR6SV3        IFNE      ' '
032340941209     C                   MOVEL     AR6SV3        WSV1
032350941209     C                   Z-ADD     AR6VA3        WVA1
032360941102     C                   EXSR      CARVAR
032370941102     C*
032380941102     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
032390990930     C     KARB2         READE(N)  FIAR7000                               30
032400941102    5C     *IN30         DOWEQ     *OFF
032410941209     C                   MOVEL     AR7SVN        WSV1
032420941209     C                   MOVEL     AR7VAN        WVA1
032430941102     C                   EXSR      CARVAR
032440990930     C     KARB2         READE(N)  FIAR7000                               30
032450941102    5C                   ENDDO
032460941102    4C                   ENDIF
032470941102    3C                   ENDIF
032480941019     C*
032490941019   X2C                   ELSE
032500920128     C*
032510990930     C* PROGRAMMA     RICHIAMATO: DATI DA DARBT
032520941025     C                   Z-ADD     §BTIAS        VIDIAS
032530950426     C                   Z-ADD     §BTIAS        COMIAS
032540941019     C                   MOVEL     §BTVAS        VIDVAS
032550941220     C                   MOVEL     §BTVAS        VARVAS
032560941209     C                   MOVEL     §BTQFT        VIDQFT
032570950424     C                   MOVEL     §BTQFT        VARQFT
032580941209     C                   MOVEL     §BTVMD        VIDVMD
032590941209     C                   MOVEL     §BTVAD        VIDVAD
032600941220     C                   MOVEL     §BTVAD        VARVAD
032610920128     C                   MOVEL     §BTKSC        VIDKLP
032620950424     C                   MOVEL     §BTKSC        VARKLP
032630920128     C                   MOVE      §BTKSC        VIDKSC
032640970822     C                   MOVE      §BTKSC        MEMKSC
032650950424     C                   MOVE      §BTKSC        VARKSC
032660920128     C                   MOVEL     §BTKSC        SAVKSC            7 0
032670060511     c                   clear                   kscbolla
032680920128     C                   MOVEL     §BTCTR        VIDCTR
032690050202     C                   MOVEL     §BTCTR        SAVCTR
032700060511     c                   clear                   ctrbolla
032710990930     C                   MOVEL     §BTDIV        VIDDIV
032720991006     C                   MOVEL     §BTDIV        WDIV
032730990930     C                   Z-ADD     §BTPOR        VIDPOR
032740991006     C                   Z-ADD     §BTPOR        VARPOR
032750990930     C                   Z-ADD     §BTIMV        VIDIMV
032760920128     C                   MOVEL     §BTCEI        VIDCEI
032770941102     C* CARICO LE VARIE
032780941102     C                   Z-ADD     0             NRR               5 0
032790941209     C                   DO        20            C                 3 0
032800941209     C                   MOVEL     §SV(C)        WSV1
032810941209     C                   Z-ADD     §VA(C)        WVA1
032820941102     C                   EXSR      CARVAR
032830941209     C                   ENDDO
032840941019    2C                   ENDIF
032850131016
032860131016     c                   z-add     vidias        bol_vidias
032870131016     c                   movel     vidvas        bol_vidvas
032880131016     c                   z-add     vidvmd        bol_vidvmd
032890131016     c                   movel     vidvad        bol_vidvad
032900131016     c
032910941215     C*
032920941215     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
032930050202     C                   Z-ADD     SAVKSC        KSC
032940941215     C                   EXSR      CARCTR
032950941019     C*
032960920128     C* DECODIFICA ESENZIONE IVA
032970920128     C                   MOVEL     *BLANKS       DESCEI
032980941019    2C     VIDCEI        IFNE      ' '
032990920128     C                   MOVEL     'EI'          COD
033000920128     C                   MOVEL     *BLANKS       KEY
033010051214     C                   MOVEL(P)  VIDCEI        KEY
033020941212     C                   EXSR      CTABEL
033030920128     C  N30              MOVEL     TBLUNI        DESCEI
033040941019    2C                   END
033050920128     C* GIRO DATA FATTURA
033060941102    1C     WAR6          IFEQ      'E'
033070941102     C     AR6DFT        ANDGT     0
033080941019     C                   Z-ADD     AR6DFT        G02INV
033090941019     C                   MOVEL     '3'           G02ERR
033100941019     C                   CALL      'XSRDA8'
033110941019     C                   PARM                    WLBDAT
033120941019     C*
033130941019     C                   Z-ADD     G02DAT        VIDDFT
033140941019    2C                   ENDIF
033150941019    1C                   ENDIF
033160941019     C**
033170991001     C* F I A R B T 0 F   SECONDA BOLLA
033180941019     C**
033190911211     C     FLGCVR        IFEQ      '2'
033200941102     C                   SELECT
033210941102     C     D66RTA        WHENEQ    'S'
033220950616     C                   SETON                                        1529        RITASSAZ
033230941102     C*
033240941102     C     D66BSF        WHENEQ    'S'
033250941102     C                   SETON                                        13          VARIA  SF
033260941102     C*
033270941102     C     D66BFI        WHENEQ    'S'
033280991006     C                   SETON                                        1407        VARIA  FAT
033290941102     C*
033300941102     C                   ENDSL
033310911211     C*
033320911219     C                   MOVEL     *BLANKS       DESKSC
033330941019     C                   MOVEL     ARBRSD        W008A             8
033340941019     C                   MOVEL     W008A         DESKSC
033350131120     c                   clear                   sav_ift
033360941019     C*
033370941019     C* PGM NON RICHIAMATO
033380941019    2C     *IN20         IFEQ      *OFF
033390920128     C* NON ESISTE ESTENSIONE BOLLA
033400990930    3C     WAR62         IFEQ      ' '
033410911211     C                   MOVEL     ARBLNA        VIDKLP
033420911211     C                   MOVEL     9999          VIDKSC
033430911212     C                   MOVEL     *ZEROS        VIDCTR
033440050202     C                   MOVEL     *ZEROS        SAVCTR
033450060511     c                   clear                   ctrbolla
033460991013     C                   MOVE      *ZEROS        SAVKSC            7 0
033470060511     c                   clear                   kscbolla
033480941019     C*
033490941019   X3C                   ELSE
033500941019     C*
033510131120     c                   z-add     ar6ift        sav_ift
033520990930     C                   MOVEL     AR6DIV        VI2DIV
033530020305     C                   MOVEL     AR6DIV        WDIV
033540990930     C                   MOVEL     AR6SV1        VI2SV1
033550040122     C**!!!              Z-ADD     AR6VA1        VI2VA1
033560040122     C**!!!              Z-ADD     AR6VA1        VA2VA1
033570040123      * carico le varie
033580040122     c                   Movel     Ar6Sv1        wsv1
033590040122     c                   Z-add     Ar6Va1        wva1
033600040122     c                   ExSr      CarVar1
033610040122     c                   Movel     Ar6Sv2        wsv1
033620040122     c                   Z-add     Ar6Va2        wva1
033630040122     c                   ExSr      CarVar1
033640040122     c                   Movel     Ar6Sv3        wsv1
033650040122     c                   Z-add     Ar6Va3        wva1
033660040122     c                   ExSr      CarVar1
033670040122      * Verifico se ci sono altre varie
033680040126     c                   Movel     '2'           wtrc
033690040123     c     Karb2         Setll     Fiar701l
033700040122Do  4c                   Do        *Hival
033710040123     c     Karb2         Reade(n)  Fiar701l
033720040122     c                   If        %Eof(Fiar701l)
033730040122     c                   Leave
033740040122     c                   EndIf
033750040122     c                   Movel     Ar7Svn        wsv1
033760040122     c                   Z-add     Ar7Van        wva1
033770040122     c                   ExSr      CarVar1
033780040122    4c                   EndDo
033790990930     C                   MOVEL     AR6CEI        VI2CEI
033800990930     C                   MOVEL     AR6KSC        VIDKLP
033810990930     C                   MOVE      AR6KSC        VIDKSC
033820991013     C                   MOVEL     AR6KSC        SAVKSC
033830060511     C                   MOVEL     AR6KSC        kscbolla
033840990930     C                   MOVEL     AR6CTR        VIDCTR
033850050202     C                   MOVEL     AR6CTR        SAVCTR
033860060511     C                   MOVEL     AR6CTR        ctrbolla
033870941019    3C                   END
033880941019     C*
033890941019   X2C                   ELSE
033900941019     C*
033910990930     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DARBT 2BOLLA
033920990930     C                   MOVEL     §BTDIV        VI2DIV
033930991006     C                   MOVEL     §BTDIV        WDIV
033940040123     C**!!!              MOVEL     §BTSV1        VI2SV1
033950040123     C**!!!              Z-ADD     §BTVA1        VI2VA1
033960040123     C**!!!              Z-ADD     §BTVA1        VA2VA1
033970040123      * carico le varie
033980040123     c                   Do        20            c
033990040123     c                   Movel     §sv(c)        wsv1
034000040123     c                   Z-add     §va(c)        wva1
034010040123     c                   ExSr      CarVar1
034020041130     c                   If        ContaSv = 6
034030040123     c                   Leave
034040040123     c                   EndIf
034050040123     c                   EndDo
034060040123
034070990930     C                   MOVEL     §BTCEI        VI2CEI
034080990930     C                   MOVEL     §BTKSC        VIDKLP
034090990930     C                   MOVE      §BTKSC        VIDKSC
034100991013     C                   MOVE      §BTKSC        SAVKSC
034110060511     c                   clear                   kscbolla
034120990930     C                   MOVEL     §BTCTR        VIDCTR
034130050202     C                   MOVEL     §BTCTR        SAVCTR
034140060511     c                   clear                   ctrbolla
034150941019    2C                   END
034160050202     c
034170050202     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
034180050202     C                   Z-ADD     SAVKSC        KSC
034190050202     C                   EXSR      CARCTR
034200920128     C*
034210920128     C                   MOVEL     *BLANKS       DESCEI
034220941019    2C     VI2CEI        IFNE      ' '
034230911211     C                   MOVEL     'EI'          COD
034240911211     C                   MOVEL     *BLANKS       KEY
034250051214     C                   MOVEL(P)  VI2CEI        KEY
034260941212     C                   EXSR      CTABEL
034270911211     C  N30              MOVEL     TBLUNI        DESCEI
034280941019    2C                   END
034290941019     C* GIRO DATA FATTURA
034300990930    1C     WAR62         IFEQ      'E'
034310990930     C     AR6DFT        ANDGT     0
034320990930     C                   Z-ADD     AR6DFT        G02INV
034330941019     C                   MOVEL     '3'           G02ERR
034340941019     C                   CALL      'XSRDA8'
034350941019     C                   PARM                    WLBDAT
034360941019     C*
034370941019     C                   Z-ADD     G02DAT        VI2DFT
034380991007     C                   Z-ADD     AR6FIV        VI2FIV
034390991007     C                   Z-ADD     AR6NFT        VI2NFT
034400941019    2C                   ENDIF
034410911211     C                   END
034420941019     C**
034430050202     C* F N A R B K 0 F
034440941019     C**
034450941019    1C     FLGCVR        IFEQ      'K'
034460941019     C*
034470941019     C* CODICE BOLLA PRECEDENTE
034480941019     C                   MOVEL     ARBCBO        OLDCBO
034490941019     C* SALVO CAMPI DEL CODICE BOLLA CHE MI INTERESSANO
034500941019     C                   MOVEL     §3ATB1        SAVTB1            2
034510941102     C                   MOVEL     §3ARBL        SAVRBL            1
034520941019     C                   MOVEL     §3ATB2        SAVTB2            2
034530941019     C                   MOVEL     §3AFCA        SAVFCA            1 0
034540941019     C                   CLEAR                   DENCBO
034550050414     C*
034560050414     C* VERIFICO SE SI TRATTA DI UN PREPAGATO
034570050414     C                   MOVEL     '1'           WTRC
034580050414     C     KARB2         CHAIN(N)  FIAR6000                           30
034590050414     C  N30              MOVEL     'E'           WAR6
034600050414     C*
034610050414     C
034620920128     C*
034630920128     C* PROGRAMMA NON RICHIAMATO: PRENDO DATI DA BOLLA
034640941019    2C     *IN20         IFEQ      *OFF
034650941019     C*
034660941019    3C     WAR9          IFEQ      'E'
034670941019     C                   MOVEL     AR9TIC        VIDTIC
034680941019     C                   MOVEL     AR9CAS        VIDCAS
034690941019     C                   MOVEL     AR9VCA        VIDVCA
034700941215     C                   MOVEL     AR9VCA        VARVCA
034710941102     C                   MOVEL     AR9GCA        VIDGCA
034720941019    3C                   ENDIF
034730941019     C*
034740941019     C                   MOVEL     '?'           NEWCBO
034750920128     C*
034760941019   X2C                   ELSE
034770920128     C*
034780920128     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DSARBK
034790920128     C                   MOVEL     §BKTIC        VIDTIC
034800941019     C                   MOVEL     §BKCAS        VIDCAS
034810941102     C                   MOVEL     §BKGCA        VIDGCA
034820941019     C                   MOVEL     §BKCBN        NEWCBO
034830941220     C                   MOVEL     §BKVCA        VARVCA
034840941220     C                   MOVEL     §BKVCA        VIDVCA
034850950420     C* SE NON PASSATO IMPOSTO QUELLO DELLA BOLLA
034860950420     C     NEWCBO        IFEQ      *BLANKS
034870950420     C                   MOVEL     ARBCBO        NEWCBO
034880950420     C                   ENDIF
034890920128     C*
034900920128     C* DECODIFICA NUOVO CODICE BOLLA
034910920128     C                   MOVEL     '3A'          COD
034920950420     C                   MOVEL(P)  NEWCBO        KEY
034930941212     C                   EXSR      CTABEL
034940920128     C  N30              MOVEL     TBLUNI        DS3A
034950941019     C   30              CLEAR                   DS3A
034960941102     C                   MOVEL     §3ADES        DENCBO
034970941019    2C                   END
034980941019     C*
034990920325     C* VARIAZIONE IMPORTO CONTRASSEGNO
035000941019    2C     VIDCVR        IFEQ      'KA'                                          UFFICIO
035010931118     C     VIDCVR        OREQ      'KB'                                          DOPO CONS
035020931118     C     VIDCVR        OREQ      'KP'                                          PADRONCINO
035030920325     C                   SETON                                        21
035040941102     C                   MOVEL     ARBCBO        NEWCBO
035050941019    2C                   END
035060941019    1C                   END
035070941019     C**
035080070309     C* F N A R B G 0 F
035090941019     C**
035100950120     C     FLGCVR        IFEQ      'C'
035110950120     C* SE E' CAUSALE CP PROTEGGO I CAMPI DELLA CONSEGNA RICHIESTA E
035120950120     C*  GG CHIUSURA
035130950120     C     VIDCVR        IFEQ      'CP'
035140950120     C                   SETON                                        16
035150950120     C                   ENDIF
035160941019     C* PGM NON RICHIAMATO
035170941019     C     *IN20         IFEQ      *OFF
035180920320     C* DATA CONSEGNA RICHIESTA
035190941019     C                   Z-ADD     ARBDCR        G02INV
035200920320     C*
035210920320     C                   Z-ADD     ARBHCR        VIDHCR
035220920320     C*
035230920320     C                   MOVEL     ARBTCR        VIDTCR
035240930510     C                   MOVEL     ARBGC1        VIDGC1
035250930510     C                   MOVEL     ARBGC2        VIDGC2
035260941019     C                   MOVEL     ARBTC1        VIDTC1
035270941102     C                   MOVEL     ARBTC2        VIDTC2
035280111025     c* Se il pgm non è richiamato, verifico se nella DS nel campo tipo §BGTCR
035290111025     c*  c'e' una "S" --> significa che posso aggiornare la consegna richiesta
035300111025     c*  in FIARP se bolla in distinta (solo da gestione telefonate o da richiamo
035310111025     c*  con passaggio dati) In caso contrario non posso farlo e sid eve dare errore
035320111207     c* ES - 07/12/11 -  per il momento asterisco
035330111207     c****               eval      Dcrindist=§BGTCR
035340930510     C*
035350941019   X2C                   ELSE
035360930510     C*
035370941019     C                   Z-ADD     §BGDCR        G02INV
035380930510     C*
035390930510     C                   Z-ADD     §BGHCR        VIDHCR
035400930510     C*
035410930510     C                   MOVEL     §BGTCR        VIDTCR
035420930510     C                   MOVEL     §BGGC1        VIDGC1
035430930510     C                   MOVEL     §BGGC2        VIDGC2
035440941019     C                   MOVEL     §BGTC1        VIDTC1
035450941019     C                   MOVEL     §BGTC2        VIDTC2
035460111025     c                   eval      Dcrindist='S'
035470920320     C                   END
035480941019     C*
035490941019     C* GIRO DATA CONSEGNA RICHIESTA
035500941019     C                   MOVEL     '3'           G02ERR
035510941019     C                   CALL      'XSRDA8'
035520941019     C                   PARM                    WLBDAT
035530941019     C                   Z-ADD     G02DAT        VIDDCR
035540070309     c
035550070309     c* Se lna con GEO attiva, verifico per eventuale modifica consegna ric
035560070309     c*  se c'e' sulla bolla un evento per il quale è inseribile la
035570070319     c*  modifica della data consegna richiesta
035580070319     c                   if        FGSgeot='S'
035590070309     c     karb          setgt     fnevb05l
035600070309     c     karb          readpe    fnevb05l
035610070309     c                   dow       not %eof(fnevb05l) and eveADR=' '
035620090526     c* controllo a seconda che sia o meno richiamato il pgm
035630090526     c  n20evbcev        lookup    ADRs                                   30
035640090526     c   20evbcev        lookup    ADRC                                   30
035650070309     c                   if        *in30
035660070309     c                   eval      eveADR='S'
035670070309     c                   else
035680070309     c     karb          readpe    fnevb05l
035690070309     c                   endif
035700070309     c                   enddo
035710070309     c
035720070309     c                   endif
035730070309     c
035740930510     C                   END
035750020531     c* se modificato destinatario in disagiato o supermercato imposto il
035760020531     c* relativo flag nelle cosegne particolari
035770041015     c* Lo faccio senza testare se arbgva = "U" perchè se mi trovo in
035780041015     c* questa condizione è certo che arbgva è <> da "U"
035790070907     c**   wdisag        ifne      *blanks
035800070907     c**                 select
035810070907     c**   vidtc1        wheneq    *blanks
035820070907     c**                 movel     wdisag        vidtc1
035830070907     c**   vidtc2        wheneq    *blanks
035840070907     c**                 movel     wdisag        vidtc2
035850070907     c**                 endsl
035860070907     c**   vidtc1        ifeq      vidtc2
035870070907     c**                 clear                   vidtc2
035880070907     c**                 endif
035890070907     c**                 endif
035900950104     C**
035910070309     C* F N A R B M 0 F
035920950104     C**
035930950104     C     VIDCVR        IFEQ      'MI'
035940950104     C* PGM NON RICHIAMATO: PER ORA NON ESISTE IL RICHIAMO
035950950104     C* DECODIFICO TIPO TARIFFA
035960950104     C                   MOVEL     ARBCTR        VIDCTR
035970050202     C                   MOVEL     ARBCTR        SAVCTR
035980060511     C                   MOVEL     ARBCTR        ctrbolla
035990050202     C                   CLEAR                   Fnlv59DS
036000050202     C                   MOVEL     VIDCTR        ILV59CTR
036010050202     C                   MOVEL     'S'           ILV59CFO
036020050202     C                   CALL      'FNLV59R'
036030050202     C                   PARM                    Fnlv59DS
036040950104     C*
036050050202     C     OLV59ERR      IFEQ      ' '
036060050202     C                   MOVEL     OLV59DFS      DE2CTR
036070950104     C                   ENDIF
036080950104     C                   MOVEL     ARBKSC        VIDKLP
036090950104     C                   MOVE      ARBKSC        VIDKSC
036100970822     C                   MOVE      ARBKSC        MEMKSC
036110950104     C                   MOVEL     ARBRSM        W008A
036120950104     C                   MOVEL     W008A         DESKSC
036130051220     c                   clear                   memntw
036140051220     c     vidklp        chain     azorg01l
036150051220     c                   if        %found(azorg01l)
036160051220     C                   MOVEL     ORGDE3        OG143
036170051220     C                   MOVEL     §OGNTW        memntw
036180051220     c                   endif
036190160225     c* Se lna fedex --> carico le tariffe di cartello FED
036200160225     c
036210970813     C* SE CODICE 8888 CARICO L'INDIRIZZO COMPLETO
036220950104     C     VIDKSC        IFEQ      8888
036230970813     C                   SETOFF                                       22
036240950104     C                   MOVEL     ARBRSM        VIDRSM
036250950104     C                   MOVEL     ARBINM        VIDINM
036260950104     C                   MOVEL     ARBCAM        VIDCAM
036270950104     C                   MOVEL     ARBLOM        VIDLOM
036280970808     C                   MOVEL     ARBCAM        VARCAM
036290970808     C                   MOVEL     ARBLOM        VARLOM
036300950104     C                   MOVEL     ARBPRM        VIDPRM
036310950104     C                   MOVEL     ARBNZM        VIDNZM
036320950104     C                   MOVEL     ARBFAP        VIDFAP
036330950104     C                   MOVEL     ARBCTR        VIDCTR
036340950104     C                   MOVEL     ARBCPI        VIDPIM
036350050922     C     'PRIVATO'     SCAN      vidpim        posiz             2 0    30
036360050922     c* solo numeri
036370050922     c* "PRIVATO senza iso
036380050922     c     *in30         ifeq      *off
036390050922     c     vidism        andge     *zeros
036400050922     c     *in30         oreq      *on
036410050922     c     posiz         andlt     3
036420050922     c                   clear                   vidpim
036430050922     c                   movel     arbcpi        vidcpm
036440050922     c                   endif
036450970813     C                   ELSE
036460970813     C                   SETON                                        22
036470950104     C                   ENDIF
036480950104     C                   ENDIF
036490961211     C**
036500970110     C* VARIAZIONE DEL RIFERIMENTO MITTENTE ALFABETICO E NATURA MERCE
036510961211     C**
036520961211     C     D48CVB        IFEQ      'RM'
036530970110     C                   MOVEL     ARBRMA        VIDRMA
036540970110     C                   MOVEL     ARBNAS        VIDNAS
036550961211     C                   END
036560950104     C*
036570011018      * Se l'importo d'assicurare è a 0 e la divisa non è impostata
036580011018     C     VIDIAS        IFEQ      *ZEROS
036590011018     C     VIDVAS        ANDEQ     *BLANKS
036600011018      * imposto la divisa della nazione del mittente
036610011018      * se estero
036620011018     C     MITVLT        IFNE      *BLANKS
036630011018     C     WMITIE        ANDEQ     'E'
036640011018     C                   MOVEL     MITVLT        VIDVAS
036650011018     C                   MOVEL     MITVLT        VARVAS
036660011018     C                   ELSE
036670011018      * altrimenti imposto la divisa della moneta di conto
036680011017     C                   MOVEL     §GEDCN        VIDVAS
036690011017     C                   MOVEL     §GEDCN        VARVAS
036700011017     C                   ENDIF
036710011018     C                   ENDIF
036720011018      *
036730011017      * Se la divisa e l'importo del valore merce non sono impostati
036740011018     C     VIDVAD        IFEQ      *BLANKS
036750011018     C     VIDVMD        ANDEQ     *ZEROS
036760011018      * imposto la divisa della nazione del mittente
036770011018      * se estero
036780011018     C     MITVLT        IFNE      *BLANKS
036790011018     C     WMITIE        ANDEQ     'E'
036800011018     C                   MOVEL     MITVLT        VIDVAD
036810011018     C                   MOVEL     MITVLT        VARVAD
036820011018     C                   ELSE
036830011018      * altrimenti imposto la divisa della moneta di conto
036840011017     C                   MOVEL     §GEDCN        VIDVAD
036850011017     C                   MOVEL     §GEDCN        VARVAD
036860011017     C                   ENDIF
036870011018     C                   ENDIF
036880011018      *
036890011017      * Se la divisa e l'importo del contrassegno non sono impostati
036900011018     C     VIDVCA        IFEQ      *BLANKS
036910011018     C     VIDCAS        ANDEQ     *ZEROS
036920011018      * imposto la divisa della nazione del destinatario
036930011018      * se estero
036940011018     C     DESVLT        IFNE      *BLANKS
036950011018     C     WDESIE        ANDEQ     'E'
036960011018     C                   MOVEL     DESVLT        VIDVCA
036970011018     C                   MOVEL     DESVLT        VARVCA
036980011018    2C                   ELSE
036990011018      * altrimenti imposto la divisa della moneta di conto
037000011017     C                   MOVEL     §GEDCN        VIDVCA
037010011017     C                   MOVEL     §GEDCN        VARVCA
037020011017     C                   ENDIF
037030011018     C                   ENDIF
037040030203      *------------
037050030124      * Bancali
037060030711      *  solo x causale 'BR'
037070030711If  1c                   If        D48Cvb = 'BR'
037080030124     c                   Clear                   dAr5Ban
037090030124     c                   Eval      kAr5Trd = 'BAN'
037100030725     c     kAr5          Chain     Fiar501l
037110030711     c                   If        %Found(Fiar501l)
037120030124     c                   Movel     Ar5Uni        dAr5Ban
037130030711     c                   EndIf
037140030124     c                   Eval      VidNba = §Ar5Nba
037150030124     c                   Eval      VidNb2 = §Ar5Nb2
037160030124     c                   Eval      VidRb1 = §Ar5Rb1
037170030124     c                   Eval      VidRb2 = §Ar5Rb2
037180030124     c                   Eval      VidNomb = §Ar5Nomb
037190030124     c                   Eval      VidMotb = §Ar5Motb
037200030124If  2c                   If        §Ar5Tba <> *Zeros
037210030612     c                   Clear                   Tibs02DS
037220030124     c                   Clear                   dTba
037230030124     c                   Movel     'C'           T02Mod
037240030124     c                   Movel     Knsif         T02Sif
037250030124     c                   Movel     'TBA'         T02Cod
037260030124     c                   Movel(p)  §Ar5Tba       T02Ke1
037270030124     c                   Call      'TIBS02R'
037280030124     c                   Parm                    Kpjba
037290030612     c                   Parm                    Tibs02DS
037300030124If  3c                   If        T02Err = *Blanks
037310030124     c                   Movel     T02Uni        dTba
037320030124    3c                   EndIf
037330030124     c                   Eval      VidTba = §TbaDesc
037340030124     c                   Eval      VidTbar = §TbaDesc
037350030124    2c                   EndIf
037360030124If  2c                   If        §Ar5Tb2 <> *Zeros
037370030612     c                   Clear                   Tibs02DS
037380030124     c                   Clear                   dTba
037390030124     c                   Movel     'C'           T02Mod
037400030124     c                   Movel     Knsif         T02Sif
037410030124     c                   Movel     'TBA'         T02Cod
037420030124     c                   Movel(p)  §Ar5Tb2       T02Ke1
037430030124     c                   Call      'TIBS02R'
037440030124     c                   Parm                    Kpjba
037450030612     c                   Parm                    Tibs02DS
037460030124If  3c                   If        T02Err = *Blanks
037470030124     c                   Movel     T02Uni        dTba
037480030124    3c                   EndIf
037490030124     c                   Eval      VidTb2 = §TbaDesc
037500030124     c                   Eval      VidTb2r = §TbaDesc
037510030124    2c                   EndIf
037520030711
037530080805     c* Se pgm richiamato, imposto dati dalla DSARBD
037540080805    2c                   if        *in20
037550080805     c                   Eval      VidRb1 = §bdpkf
037560080805     c                   Eval      VidRb2 = §bdvlf
037570080805    3c                   if        §bdrsd<>*blanks
037580080805     c                   Eval      VidNomb = §bdrsd
037590080805    3c                   endif
037600080805    3c                   if        §bdrd2<>*blanks
037610080805     c                   Eval      VidMotb = §bdrd2
037620080805     c                   else
037630080805     c                   Eval      VidMotb = 'PDA  '
037640080805    3c                   EndIf
037650080805
037660080805    2c                   EndIf
037670080805    1c                   EndIf
037680040608      *------------
037690040608      * Supermercati - telefono e referente
037700040608      *  solo x causale 'CS'
037710040608If  1c                   If        D48Cvb = 'CS'
037720040608     c                   Eval      wokscr = *Off
037730040608     c                   Clear                   dAr5Scr
037740040608     c                   Eval      kAr5Trd = 'SCR'
037750040608     c     kAr5          Setgt     Fiar501l
037760040608do  2c                   Do        *Hival
037770040608     c     kAr5          Readpe(n) Fiar501l
037780040608     c                   If        %Eof(Fiar501l)
037790040608     c                   Leave
037800040608     c                   EndIf
037810040608     c                   Movel     Ar5Uni        dAr5Scr
037820040608     c                   Eval      VidNom = §Ar5Nom
037830040608     c                   Eval      VidTel = §Ar5Tel
037840040608     c                   Eval      wokscr = *On
037850040608     c                   Leave
037860040608e   2c                   EndDo
037870040608      * se non ho trovato il rcd SCR cerco i dati sul rcd GEN
037880040608if  2c                   If        wokscr = *Off
037890070910     c***                Clear                   dAr5Gen
037900070910     c***                Eval      kAr5Trd = 'GEN'
037910070910     c***  kAr5          Chain(n)  Fiar501l
037920070910     c***                If        %Found(Fiar501l)
037930070910     c***                Movel     Ar5Uni        dAr5Gen
037940070910     c***                EndIf
037950070910     c***                Eval      VidNom = §Ar5Ref
037960070910     c***                Eval      VidTel = §Ar5Teld
037970070910     c                   Eval      VidNom = PAr5Ref
037980070910     c                   Eval      VidTel = PAr5Teld
037990040608e   2c                   EndIf
038000040608e   1c                   EndIf
038010911209     C*
038020910429     C                   ENDSR
038030941102     C*
038040941102     C* CARICO LA VARIA IN SFL ---------------------------------------*
038050941102     C     CARVAR        BEGSR
038060941102     C*
038070941102     C     WSV1          IFNE      ' '
038080011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
038090011018     C     ARBFBR        IFEQ      'S'
038100011018     C     WSV1          ANDEQ     'N'
038110011018     C     WVA1          ANDEQ     88888888
038120011018     C                   MOVEL     '1'           FLGVRN            1
038130011018     C                   ENDIF
038140990930     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
038150990930     C     WSV1          LOOKUP    SVA                                    33
038160990930     C     *IN33         IFEQ      *ON
038170941102     C                   ADD       1             NRR
038180941102     C     NRR           CHAIN     LR48S03                            30
038190941102     C                   MOVEL     WSV1          V3CSV1
038200941102     C                   Z-ADD     WVA1          V3CVA1
038210991006     C                   Z-ADD     WVA1          VARVA1
038220941102     C   30              WRITE     LR48S03
038230941102     C  N30              UPDATE    LR48S03
038240941102     C                   ENDIF
038250990930     C                   ENDIF
038260941102     C                   ENDSR
038270040122
038280040122      *---------------------------------------------------------------*
038290040122      * CARICO LA VARIA DELLA SECONDA BOLLA
038300040122      *---------------------------------------------------------------*
038310040122     c     CarVar1       BegSr
038320040122
038330040122if  1c                   If        wsv1 <> *Blanks
038340040122      * Carico la varia solo se fa parte del totale imponibile
038350040122     c     wsv1          Lookup    sva                                    31
038360040122if  2c                   If        *In31
038370040122     c                   add       1             ContaSv
038380040122     c                   add       wva1          TotImv
038390040122
038400040122s   3c                   Select
038410040122     c                   When      ContaSv = 1
038420040122     c                   Movel     wsv1          Vi2Sv1
038430040122     c                   Z-add     wva1          Vi2Va1
038440040122     c                   Z-add     wva1          Va2Va1
038450040122     c                   When      ContaSv = 2
038460040122     c                   Movel     wsv1          Vi2Sv2
038470040122     c                   Z-add     wva1          Vi2Va2
038480040122     c                   Z-add     wva1          Va2Va2
038490040122     c                   When      ContaSv = 3
038500040122     c                   Movel     wsv1          Vi2Sv3
038510040122     c                   Z-add     wva1          Vi2Va3
038520040122     c                   Z-add     wva1          Va2Va3
038530040122     c                   When      ContaSv = 4
038540040122     c                   Movel     wsv1          Vi2Sv4
038550040122     c                   Z-add     wva1          Vi2Va4
038560040122     c                   Z-add     wva1          Va2Va4
038570041130     c                   When      ContaSv = 5
038580041130     c                   Movel     wsv1          Vi2Sv5
038590041130     c                   Z-add     wva1          Vi2Va5
038600041130     c                   Z-add     wva1          Va2Va5
038610041130     c                   When      ContaSv = 6
038620041130     c                   Movel     wsv1          Vi2Sv6
038630041130     c                   Z-add     wva1          Vi2Va6
038640041130     c                   Z-add     wva1          Va2Va6
038650040122    3c                   EndSl
038660040122
038670040122    2c                   EndIf
038680040122
038690040122    1c                   EndIf
038700040122
038710040122     c                   EndSr
038720910429     C*
038730991001     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
038740991001     C     CARWAR        BEGSR
038750991001     C*
038760991001     C                   CLEAR                   WINL
038770991001     C*
038780991001     C                   CLEAR                   WSV
038790991001     C                   CLEAR                   WVA
038800991001     C                   CLEAR                   C
038810991001    1C                   DO        20            NRR
038820991001     C     NRR           CHAIN     LR48S03                            30
038830991001     C*
038840991001    2C     *IN30         IFEQ      *OFF
038850991001     C     V3CSV1        ANDNE     *BLANKS
038860991001     C                   ADD       1             C
038870991001     C                   MOVEL     V3CSV1        WSV(C)
038880991001     C                   Z-ADD     V3CVA1        WVA(C)
038890991001    2C                   ENDIF
038900991001    1C                   ENDDO
038910991001     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
038920991001     C     WCKINL        IFEQ      '1'
038930991001     C                   Z-ADD     1             II
038940991001     C     §$2FIN        LOOKUP    WSV(II)                                30
038950991001     C   30WVA(II)       IFGT      *ZEROS
038960991001     C                   MOVE      '1'           WINL              1
038970991001     C                   ENDIF
038980991001     C                   ENDIF
038990991001     C*
039000991001     C                   ENDSR
039010040123      *---------------------------------------------------------------*
039020040123      * Carico le varie della seconda bolla in una sk di comodo
039030040123      *---------------------------------------------------------------*
039040040123     c     Carwar1       Begsr
039050040123
039060040123     c                   Clear                   wsv
039070040123     c                   Clear                   wva
039080040123     c                   Clear                   c
039090040123
039100040123     c                   If        vi2sv1 <> *Blanks
039110050401     c                   add       1             c
039120050401     c                   Movel     vi2Sv1        wsv(c)
039130050401     c                   Z-add     vi2va1        wva(c)
039140040123     c                   EndIf
039150040123     c                   If        vi2sv2 <> *Blanks
039160050401     c                   add       1             c
039170050401     c                   Movel     vi2Sv2        wsv(c)
039180050401     c                   Z-add     vi2va2        wva(c)
039190040123     c                   EndIf
039200040123     c                   If        vi2sv3 <> *Blanks
039210050401     c                   add       1             c
039220050401     c                   Movel     vi2Sv3        wsv(c)
039230050401     c                   Z-add     vi2va3        wva(c)
039240040123     c                   EndIf
039250040123     c                   If        vi2sv4 <> *Blanks
039260050401     c                   add       1             c
039270050401     c                   Movel     vi2Sv4        wsv(c)
039280050401     c                   Z-add     vi2va4        wva(c)
039290040123     c                   EndIf
039300041130     c                   If        vi2sv5 <> *Blanks
039310050401     c                   add       1             c
039320050401     c                   Movel     vi2Sv5        wsv(c)
039330050401     c                   Z-add     vi2va5        wva(c)
039340041130     c                   EndIf
039350041130     c                   If        vi2sv6 <> *Blanks
039360050401     c                   add       1             c
039370050401     c                   Movel     vi2Sv6        wsv(c)
039380050401     c                   Z-add     vi2va6        wva(c)
039390041130     c                   EndIf
039400040123
039410040123     c                   EndSr
039420991012     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039430991012     C     RDTASV        BEGSR
039440991012     C*
039450991012     C                   CLEAR                   C
039460030620     C                   CLEAR                   WVAR3A
039470040123     c                   Clear                   wvar3ai
039480991012    1C                   DO        20            NRR
039490991012     C     NRR           CHAIN     LR48S03                            30
039500991012    2C     *IN30         IFEQ      *OFF
039510991012     C                   ADD       1             C
039520020503     c* se la varia è uguale a quella prevista dal tipo bolla, non richiamo
039530020503     c*  piu' la tassazione
039540040123     c                   if        §3asva<>*blanks and v3csv1=§3asva
039550040123     c                   If        vidimv > *Zeros
039560020503     c                   eval      wtassa='N'
039570030620      * mi salvo l'imnporto della varia
039580030620     c                   eval      itassa = v3cva1
039590040123     c                   EndIf
039600030620      * mi salvo un campo di comodo
039610030620     c                   movel     'S'           Wvar3a            1
039620040123      * memorizzo anche se valorizzata
039630040123     c                   If        v3cva1 > 0
039640040123     c                   Eval      wvar3ai = 'S'
039650040123     c                   EndIf
039660030620      *
039670020503     c                   endif
039680030620      * verifico se esiste la varia "D" non la ricalcolo in caso di fattura immediata
039690030620     c                   if        v3csv1 = 'D'
039700030620     c                   eval      wtassad = 'N'
039710030620     c                   endif
039720030620      *
039730030624      * se esiste la varia "&" con tutti 8888888 non la passo al TNSF20R
039740030624      * ma passo un flag  di richiesta calcolo varia "&"
039750030709     C                   if        v3csv1 = '&' and v3cva1 = 88888888 and
039760030709     C                             flgsf20 = 'S'
039770030624     C                   eval      TASFCAA = 'S'
039780030624     C                   else
039790991012     C                   MOVEL     V3CSV1        VSV(C)
039800991012     C                   MOVEL     V3CVA1        VVA(C)
039810030624     c                   endif
039820030624      *
039830991012    2C                   ENDIF
039840991012    1C                   ENDDO
039850991012     C                   ENDSR
039860920120     C*--- EMISSIONE E GESTIONE FORMATO 2 ----------------------------*
039870920120     C     EMFOR2        BEGSR
039880941102     C*
039890920120     C     FOR02         TAG
039900941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
039910950519    1C     *IN90         IFEQ      *ON
039920950114     C     D48FFR        OREQ      'S'
039930950607     C     D48FFR        OREQ      'V'
039940941214     C                   WRITE     LR48T03
039950941014     C                   EXFMT     LR48D02
039960950519    1C                   END
039970941102     C*
039980941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
039990941102     C                   SETOFF                                       2890
040000941102     C                   SETOFF                                       404142
040010941102     C                   SETOFF                                       434445
040020941102     C                   SETOFF                                       464748
040030061115     C                   SETOFF                                       495051
040040140408     C                   SETOFF                                       5268
040050941220     C* F12 - RITORNO
040060941220    1C     *INKL         IFEQ      *OFF
040070070329     c
040080070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
040090070329     c                   if        *in87 and *inkr
040100070329     c                   exsr      CALLNOTELDV
040110070329     c                   goto      for02
040120070329     c                   endif
040130941216     C*
040140941216     C* SE NON POSSO VARIARE NE IL PESO, NE IL VOLUME, NON CONTROLLO
040150941216     C* E NON AGGIORNO
040160941220    2C     *IN88         IFEQ      *OFF
040170941216     C     *IN89         OREQ      *OFF
040180950114     C     *IN24         OREQ      *ON
040190941216     C*
040200941216     C* CONTROLLO FORMATO
040210920120     C                   EXSR      CONTR2
040220941215     C*
040230941220     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040240941220    3C     *IN90         IFEQ      *ON
040250941215     C     D48FFR        ANDEQ     'E'
040260941215     C                   MOVEL     VIDMSG        D48MSG
040270941215     C                   MOVEL     '2'           D48ERR
040280941215     C                   GOTO      ENDEM2
040290941220    3C                   ENDIF
040300140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
040310140502     c*   (d48ffr='E' e 26 off)
040320140502     c  n90
040330140502     cann26              if        d48ffr='E'
040340140502     C                   GOTO      ENDEM2
040350140502     c                   endif
040360950607     C  NKF
040370950607     CANN26
040380920120     COR 90              GOTO      FOR02
040390060227     c* richiamo routine di richiesta/controllo motivazione
040400060301     c                   if        d66mtv = 'S' and w48mct = *blanks
040410060227     c                   exsr      ges_mtv
040420060301     c                   if        olv85f12 = '1'
040430060301     c                   if        ilv85wdw = ' '
040440060227     c                   goto      for02
040450060301     c                   else
040460060301     c                   eval      *inkl = '1'
040470060301     C                   GOTO      ENDEM2
040480060227     c                   endif
040490060301     c                   endif
040500060227     C     *IN90         IFEQ      *ON
040510060227     C     D48FFR        ANDEQ     'E'
040520060227     C                   MOVEL     VIDMSG        D48MSG
040530060227     C                   MOVEL     '2'           D48ERR
040540060227     C                   GOTO      ENDEM2
040550060227     C                   ENDIF
040560060227     c                   endif
040570920120     C*
040580141020     c* Se Richiesta modifica di indirizzo tale da variare la LNA emetto window
040590141020     c* per segnalare e richiedere una delle seguenti opzioni:
040600141020     c*  -Forzare la variazione senza variare la lna
040610141020     c*  -Richiedere il dirottamento
040620141111    3c                   if        d48ffr<>'E'  and *in01
040630141029     c                             and (%lookup('999':skFilOkdir)>0 or
040640141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)>0 and
040650141029     c                              %lookup(%editc(dutpou:'X'):skFilOkdir)>0) )
040660141020     c                   clear                   wdir
040670141020    4C     d_o95lna      ifne      ARBLNA
040680141022     c* le bolle export non le dirottiamo mai da qui
040690141022     c     lnantw        andne     'DPD'
040700141022     c     lnantw        andne     'EEX'
040710141022     c     lnantw        andne     'FED'
040720141219     c* no bolle di recupero
040730141219     c     §3arbl        andne     'R'
040740141204    5C     SAVCAD        IFNE      VIDCAD
040750141204     C     SAVLOD        ORNE      VIDLOD
040760141204     C     SAVrsd        ORNE      VIDrsd
040770141204     C     SAVind        ORNE      VIDind
040780141204     C     SAVprd        ORNE      VIDprd
040790141204     C     SAVnzd        ORNE      VIDnzd
040800141204     c     vidzng        oreq      savzng
040810141203     c     vidffd        andeq     *blanks
040820141021     c                   exsr      ges_wdw02
040830141021     c                   if        winf12='1'
040840141021     c                   goto      for02
040850141021     c                   endif
040860141020    5C                   END
040870141020    4C                   END
040880141020    4c                   if        wdir='1'
040890141212
040900141212     c* se hanno cambiato il Fermo deposito devo anche eseguire la REGIS2
040910141212     c                   if        arbffd<>%subst(vidffD:1:1)
040920141212     c                   eval      wnoindir='S'
040930141212     c                   exsr      regis2
040940141218     C   91              GOTO      FOR02
040950141212     c                   endif
040960141212
040970141020     c                   exsr      sr_tiidc
040980141212
040990141020   x4c                   else
041000141212     c                   eval      wnoindir=' '
041010141020     C                   EXSR      REGIS2
041020141020    4c                   endif
041030141020   x3c                   else
041040141212     c                   eval      wnoindir=' '
041050930517     C                   EXSR      REGIS2
041060141020    3c                   endif
041070991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041080991130     C     *IN91         IFEQ      *ON
041090991130     C     D48FFR        ANDEQ     'E'
041100991130     C                   MOVEL     VIDMSG        D48MSG
041110991130     C                   MOVEL     '2'           D48ERR
041120991130     C                   GOTO      ENDEM2
041130991130     C                   ENDIF
041140991130     C   91              GOTO      FOR02
041150941216     C*
041160941220   X2C                   ELSE
041170941216     C*
041180941216     C                   GOTO      FOR02
041190941220    2C                   ENDIF
041200941220     C*
041210950518    1C                   ENDIF
041220150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
041230150112     c*  se ce ne sono
041240150112     c                   if        msgforzatxt<>*blanks
041250150112     C                   MOVEL     msgforzatxt   D48MSG
041260150112     C                   MOVEL     'Z'           D48ERR
041270150112     c                   clear                   msgforzatxt
041280150112     C                   ENDIF
041290950518     C*
041300950518     C     ENDEM2        TAG
041310950518     C*
041320950518    1C     *INKL         IFEQ      *ON
041330950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041340950519    1C     D48FFR        OREQ      'E'
041350950518     C     *IN90         ANDEQ     *ON
041360000303    1C     D48FFR        OREQ      'E'
041370000303     C     *IN91         ANDEQ     *ON
041380941220     C* F12-RITORNO
041390160119    2C     *IN10         IFEQ      *ON
041400941220     C                   UNLOCK    FNARB01L
041410051118     C                   UNLOCK    FiAR901L
041420160119     c                   endif
041430160119     C****               ELSE
041440941222     C                   UNLOCK    FNBLP01L
041450160119    2C****               END
041460991001     C                   UNLOCK    FIAR601L
041470950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041480950607     C* flag D48F12 = 1
041490950607     C   KL              MOVEL     '1'           D48F12
041500950519    1C                   END
041510941215     C*
041520950518     C                   ENDSR
041530920121     C*
041540920121     C*--- EMISSIONE E GESTIONE FORMATO 3 ----------------------------*
041550920121     C     EMFOR3        BEGSR
041560990415     C*
041570990415     C                   MOVEL     ' '           COMASS
041580941102     C*
041590920121     C     FOR03         TAG
041600941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
041610950114     C     *IN90         IFEQ      *ON
041620941212     C     D48FFR        OREQ      'S'
041630950607     C     D48FFR        OREQ      'V'
041640941215     C                   WRITE     LR48T03
041650941212     C                   WRITE     LR48Z03
041660941212     C                   EXFMT     LR48C03
041670941220    1C                   END
041680941102     C*
041690941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
041700941209     C                   SETOFF                                       289052
041710941102     C                   SETOFF                                       404142
041720991006     C                   SETOFF                                       434445
041730941102     C                   SETOFF                                       464748
041740941209     C                   SETOFF                                       495051
041750941215     C                   SETOFF                                       989453
041760961112     C                   SETOFF                                       545556
041770990415     C                   SETOFF                                       82
041780941220     C*
041790941220     C* PER F12-RITORNO O F21 RITASSAZIONE, DISALLOCO I RECORD
041800941220     C* F12-RITORNO
041810941220    1C     *INKL         IFEQ      *ON
041820941220     C     *INKV         OREQ      *ON
041830941222     C*
041840941222     C     *IN10         IFEQ      *ON
041850941220     C                   UNLOCK    FNARB01L
041860051118     C                   UNLOCK    FiAR901L
041870941222     C                   ELSE
041880941222     C                   UNLOCK    FNBLP01L
041890941220    1C                   ENDIF
041900991005     C                   UNLOCK    FIAR601L
041910950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041920950607     C* flag D48F12 = 1
041930950607     C   KL              MOVEL     '1'           D48F12
041940941222    1C                   ENDIF
041950941220     C* F12 - RITORNO
041960941220    1C  NKL              DO
041970941220     C*
041980941220     C* F21 - RITASSAZIONE BOLLA
041990941220    2C     *INKV         IFEQ      *ON
042000050511     C                   CLEAR                   Fnlra2DS
042010050511     C                   Z-ADD     ARBAAS        ilra2AAS
042020050511     C                   Z-ADD     ARBLNP        ilra2lNP
042030050511     C                   Z-ADD     ARBNRS        ilra2NRS
042040050511     C                   Z-ADD     ARBNSP        ilra2NSP
042050050511     C                   MOVEL     D48FGS        ilra2FGS
042060050511     C                   MOVEL     D48CVB        ilra2CVB
042070050511     C                   MOVEL     Fnlra2DS      KPJBU
042080050511     C                   CALL      'FNLRA2R'
042090941217     C                   PARM                    KPJBA
042100030718     C                   PARM                    TRUL90DS
042110050804     c* Se mi passa errore, lo passo dal lr48 al lr36
042120050804     c                   movel     kpjbu         fnlra2ds
042130050804     C                   MOVEL     olra2msg      D48MSG
042140050804     C                   MOVEL     olra2err      D48ERR
042150131119     c                   if        olra2nft>0
042160131119     C                   MOVEL     '1'           WTRC
042170131119     c     karb2         chain(n)  fiar6000
042180131119     c                   endif
042190131120     c* trasmetto variazione a PDA:
042200131120     c* se segue fattura --> solo se prima era una fattura immediata
042210131120     c* se fattura immediata --> solo se variato importo della fattura
042220131119     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
042230131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
042240131120     c                              or ar6div<>viddiv)))
042250130416     c                   eval      wpda_ndc=arbndc
042260130416     c                   eval      wpda_ddc=arbddc
042270130416     c                   eval      wpda_dcm=arbdcm
042280140120     c                   eval      arbdtv=olra2dtv
042290140120     c                   eval      arborv=olra2orv
042300140120     c                   eval      arbpru=olra2pru
042310140120     c                   eval      arbcvb=d48cvb
042320130416     c                   exsr      sr_pda
042330130416     c                   endif
042340920121     C*
042350941220   X2C                   ELSE
042360920121     C* CONTROLLO FORMATO
042370920121     C                   EXSR      CONTR3
042380941215     C*
042390941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042400941220    3C     *IN90         IFEQ      *ON
042410941215     C     D48FFR        ANDEQ     'E'
042420941215     C                   MOVEL     VIDMSG        D48MSG
042430941215     C                   MOVEL     '3'           D48ERR
042440941215     C                   GOTO      ENDEM3
042450941220    3C                   ENDIF
042460920320     C*
042470950607     C  NKF
042480950607     CANNKI
042490950607     CANN26
042500990415     COR 82
042510000105     COR KN
042520920320     COR 90              GOTO      FOR03
042530060302     c* richiamo routine di richiesta/controllo motivazione
042540060302     c                   if        d66mtv = 'S' and w48mct = *blanks
042550060302     c                   exsr      ges_mtv
042560060302     c                   if        olv85f12 = '1'
042570060302     c                   if        ilv85wdw = ' '
042580060302     c                   goto      for03
042590060302     c                   else
042600060302     c                   eval      *inkl = '1'
042610060302     C                   GOTO      ENDEM3
042620060302     c                   endif
042630060302     c                   endif
042640060302     C     *IN90         IFEQ      *ON
042650060302     C     D48FFR        ANDEQ     'E'
042660060302     C                   MOVEL     VIDMSG        D48MSG
042670070308     C                   MOVEL     '3'           D48ERR
042680060302     C                   GOTO      ENDEM3
042690060302     C                   ENDIF
042700060302     c                   endif
042710920121     C*
042720920204     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
042730920204     C                   EXSR      REGIS3
042740991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042750991130     C     *IN91         IFEQ      *ON
042760991130     C     D48FFR        ANDEQ     'E'
042770991130     C                   MOVEL     VIDMSG        D48MSG
042780991130     C                   MOVEL     '3'           D48ERR
042790991130     C                   GOTO      ENDEM3
042800991130     C                   ENDIF
042810920204     C   91              GOTO      FOR03
042820920204     C*
042830070309     c* ristampo fattura immediata se variata(37 on), se in arrivo
042840070309     c*  (10 on) e se non attiva GEO
042850130321    3C**   *IN37         IFEQ      *ON
042860130321    3C***  *IN10         ANDEQ     *ON
042870130321     c**   FGSddaSI      andeq     ' '
042880130321     C***                CLEAR                   FNLSB5DS
042890130321     C***                MOVEL     'V'           DB0RIS
042900130321     C***                MOVEL     D48TBO        DB0TBO
042910130321     C***                MOVEL     ARBAAS        DB0AAS
042920130321     C***                MOVEL     ARBLNP        DB0LNP
042930130321     C***                MOVEL     ARBNRS        DB0NRS
042940130321     C***                MOVEL     ARBNSP        DB0NSP
042950130321     C***                CALL      D90PSL
042960130321     C***                PARM                    FNLSB5DS
042970130321    3C***                END
042980941212     C*
042990941220    2C                   END
043000941220     C*
043010000303    1C                   END
043020000303     C     ENDEM3        TAG
043030000303     C* SE PGM RICHIAMATO E C'E' ERRORE DISALLOCO
043040000303    1C     D48FFR        IFEQ      'E'
043050000303     C     *IN90         ANDEQ     *ON
043060000303    1C     D48FFR        OREQ      'E'
043070000303     C     *IN91         ANDEQ     *ON
043080000303     C     *IN10         IFEQ      *ON
043090000303     C                   UNLOCK    FNARB01L
043100051118     C                   UNLOCK    FiAR901L
043110000303     C                   ELSE
043120000303     C                   UNLOCK    FNBLP01L
043130000303     C                   ENDIF
043140000303     C                   UNLOCK    FIAR601L
043150000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043160000303     C* flag D48F12 = 1
043170000303     C   KL              MOVEL     '1'           D48F12
043180000303    1C                   ENDIF
043190920121     C*
043200000303     C                   ENDSR
043210941102     C*
043220941102     C* EMETTO FORMATO4 ----------------------------------------------*
043230941102     C     EMFOR4        BEGSR
043240941102     C*
043250941102     C     FOR04         TAG
043260941102     C*
043270950114     C     *IN90         IFEQ      *ON
043280941212     C     D48FFR        OREQ      'S'
043290950607     C     D48FFR        OREQ      'V'
043300941102     C                   WRITE     LR48T01
043310941102     C                   EXFMT     LR48D04
043320941102     C                   ENDIF
043330941209     C*
043340941209     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
043350941209     C                   SETOFF                                       289045
043360941209     C                   SETOFF                                       464748
043370991006     C                   SETOFF                                       495051
043380041130     c                   setoff                                       636465
043390041130     c                   setoff                                       6667
043400941220     C*
043410941220     C* PER F12 -RITORNO O F21-RITASSAZIONE SBLOCCO I RECORD
043420941220     C     *INKL         IFEQ      *ON
043430941220     C     *INKV         OREQ      *ON
043440941222     C*
043450941222     C     *IN10         IFEQ      *ON
043460941220     C                   UNLOCK    FNARB01L
043470051118     C                   UNLOCK    FiAR901L
043480941222     C                   ELSE
043490941222     C                   UNLOCK    FNBLP01L
043500941220     C                   ENDIF
043510991005     C                   UNLOCK    FIAR601L
043520950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043530950607     C* flag D48F12 = 1
043540950607     C   KL              MOVEL     '1'           D48F12
043550941222     C                   ENDIF
043560941220     C* F12 - RITORNO
043570941102     C     *INKL         IFEQ      *OFF
043580050228     C* F21 - RITASSAZIONE BOLLA
043590941220     C     *INKV         IFEQ      *ON
043600050511     C                   CLEAR                   Fnlra2DS
043610050511     C                   Z-ADD     ARBAAS        ilra2AAS
043620050511     C                   Z-ADD     ARBLNP        ilra2LNP
043630050511     C                   Z-ADD     ARBNRS        ilra2NRS
043640050511     C                   Z-ADD     ARBNSP        ilra2NSP
043650050511     C                   MOVEL     D48FGS        ilra2FGS
043660050511     C                   MOVEL     D48CVB        ilra2CVB
043670050511     C                   MOVEL     Fnlra2DS      KPJBU
043680050511     C                   CALL      'FNLRA2R'
043690941102     C                   PARM                    KPJBA
043700030718     C                   PARM                    TRUL90DS
043710050804     c
043720050804     c* Se mi passa errore, lo passo dal lr48 al lr36
043730050804     c                   movel     kpjbu         fnlra2ds
043740050804     C                   MOVEL     olra2msg      D48MSG
043750050804     C                   MOVEL     olra2err      D48ERR
043760131119     c                   if        olra2nft>0
043770131119     C                   MOVEL     '2'           WTRC
043780131119     c     karb2         chain(n)  fiar6000
043790131119     c                   endif
043800131120     c* trasmetto variazione a PDA:
043810131120     c* se segue fattura --> solo se prima era una fattura immediata
043820131120     c* se fattura immediata --> solo se variato importo della fattura
043830131120     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
043840131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
043850131120     c                              or ar6div<>viddiv)))
043860130416     c                   eval      wpda_ndc=arbndc
043870130416     c                   eval      wpda_ddc=arbddc
043880130416     c                   eval      wpda_dcm=arbdcm
043890140120     c                   eval      arbdtv=olra2dtv
043900140120     c                   eval      arborv=olra2orv
043910140120     c                   eval      arbpru=olra2pru
043920140120     c                   eval      arbcvb=d48cvb
043930130416     c                   exsr      sr_pda
043940130416     c                   endif
043950941102     C*
043960941220     C                   ELSE
043970941102     C* CONTROLLO FORMATO
043980941102     C                   EXSR      CONTR4
043990941215     C*
044000941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044010941215     C     *IN90         IFEQ      *ON
044020941215     C     D48FFR        ANDEQ     'E'
044030941215     C                   MOVEL     VIDMSG        D48MSG
044040941215     C                   MOVEL     '4'           D48ERR
044050941215     C                   GOTO      ENDEM4
044060941215     C                   ENDIF
044070941102     C*
044080140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
044090140502     c*   (d48ffr='E' e 26 off)
044100140502     c  n90
044110140502     cann26              if        d48ffr='E'
044120140502     C                   GOTO      ENDEM4
044130140502     c                   endif
044140950607     C  NKF
044150950607     CANNKI
044160950607     CANN26
044170941102     COR 90              GOTO      FOR04
044180060302     c* richiamo routine di richiesta/controllo motivazione
044190060302     c                   if        d66mtv = 'S' and w48mct = *blanks
044200060302     c                   exsr      ges_mtv
044210060302     c                   if        olv85f12 = '1'
044220060302     c                   if        ilv85wdw = ' '
044230060302     c                   goto      for04
044240060302     c                   else
044250060302     c                   eval      *inkl = '1'
044260060302     C                   GOTO      ENDEM4
044270060302     c                   endif
044280060302     c                   endif
044290060302     C     *IN90         IFEQ      *ON
044300060302     C     D48FFR        ANDEQ     'E'
044310060302     C                   MOVEL     VIDMSG        D48MSG
044320070308     C                   MOVEL     '4'           D48ERR
044330060302     C                   GOTO      ENDEM4
044340060302     C                   ENDIF
044350060302     c                   endif
044360941102     C*
044370941102     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
044380941102     C                   EXSR      REGIS4
044390991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044400991130     C     *IN91         IFEQ      *ON
044410991130     C     D48FFR        ANDEQ     'E'
044420991130     C                   MOVEL     VIDMSG        D48MSG
044430991130     C                   MOVEL     '4'           D48ERR
044440991130     C                   GOTO      ENDEM4
044450991130     C                   ENDIF
044460941102     C   91              GOTO      FOR04
044470941102     C                   END
044480941102     C                   END
044490000303     C*
044500000303     C     ENDEM4        TAG
044510000303    1C     D48FFR        IFEQ      'E'
044520000303     C     *IN90         ANDEQ     *ON
044530000303    1C     D48FFR        OREQ      'E'
044540000303     C     *IN91         ANDEQ     *ON
044550000303     C     *IN10         IFEQ      *ON
044560000303     C                   UNLOCK    FNARB01L
044570051118     C                   UNLOCK    FiAR901L
044580000303     C                   ELSE
044590000303     C                   UNLOCK    FNBLP01L
044600000303     C                   ENDIF
044610000303     C                   UNLOCK    FIAR601L
044620000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
044630000303     C* flag D48F12 = 1
044640000303     C   KL              MOVEL     '1'           D48F12
044650000303    1C                   ENDIF
044660941102     C*
044670000303     C                   ENDSR
044680941102     C*
044690941102     C* EMETTO E GESTISCO FORMATO5 -----------------------------------*
044700941102     C     EMFOR5        BEGSR
044710950407     C*
044720941102     C     FOR05         TAG
044730941102     C*
044740950114     C     *IN90         IFEQ      *ON
044750941212     C     D48FFR        OREQ      'S'
044760950607     C     D48FFR        OREQ      'V'
044770941102     C                   WRITE     LR48T01
044780941102     C                   EXFMT     LR48D05
044790941102     C                   END
044800941102     C*
044810941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
044820941102     C                   SETOFF                                       289096
044830941102     C                   SETOFF                                       404142
044840991001     C                   SETOFF                                       434492
044850070329     C* F12 - RITORNO
044860941220    1C     *INKL         IFEQ      *OFF
044870070329     c
044880070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
044890070329     c                   if        *in87 and *inkr
044900070329     c                   exsr      CALLNOTELDV
044910070329     c                   goto      for05
044920070329     c                   endif
044930941102     C*
044940941102     C* CONTROLLO FORMATO
044950941213     C                   EXSR      CONTR5
044960941215     C*
044970941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044980941220    2C     *IN90         IFEQ      *ON
044990941215     C     D48FFR        ANDEQ     'E'
045000941215     C                   MOVEL     VIDMSG        D48MSG
045010941215     C                   MOVEL     '5'           D48ERR
045020941215     C                   GOTO      ENDEM5
045030941220    2C                   ENDIF
045040140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
045050140502     c*   (d48ffr='E' e 26 off)
045060140502     c  n90
045070140502     cann26              if        d48ffr='E'
045080140502     C                   GOTO      ENDEM5
045090140502     c                   endif
045100140502     c* Se errore forzabile ma pgm richiamato in modalità 'E' o 'F' cioè
045110140502     c* senza emissione di videata ignoro l'errore forzabile e permetto
045120140502     c* l'aggiornamento
045130140502     c   92              if        d48ffr='E'
045140140502     c                   setoff                                       92
045150140502     c                   endif
045160950607     C  NKF
045170950607     CANN26
045180950407     COR 90
045190950407     COR 92              GOTO      FOR05
045200060302     c* richiamo routine di richiesta/controllo motivazione
045210060302     c                   if        d66mtv = 'S' and w48mct = *blanks
045220060302     c                   exsr      ges_mtv
045230060302     c                   if        olv85f12 = '1'
045240060302     c                   if        ilv85wdw = ' '
045250060302     c                   goto      for05
045260060302     c                   else
045270060302     c                   eval      *inkl = '1'
045280060302     C                   GOTO      ENDEM5
045290060302     c                   endif
045300060302     c                   endif
045310060302     C     *IN90         IFEQ      *ON
045320060302     C     D48FFR        ANDEQ     'E'
045330060302     C                   MOVEL     VIDMSG        D48MSG
045340070308     C                   MOVEL     '5'           D48ERR
045350060302     C                   GOTO      ENDEM5
045360060302     C                   ENDIF
045370060302     c                   endif
045380941102     C*
045390941220     C                   EXSR      REGIS5
045400991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045410991130     C     *IN91         IFEQ      *ON
045420991130     C     D48FFR        ANDEQ     'E'
045430991130     C                   MOVEL     VIDMSG        D48MSG
045440991130     C                   MOVEL     '5'           D48ERR
045450991130     C                   GOTO      ENDEM5
045460991130     C                   ENDIF
045470941213     C   91              GOTO      FOR05
045480941220     C*
045490950518     C                   ENDIF
045500941222     C*
045510950518     C     ENDEM5        TAG
045520950518     C* F12-RITORNO  --> DISALLOCO RECROD
045530950518    1C     *INKL         IFEQ      *ON
045540950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045550950518     C     D48FFR        OREQ      'E'
045560950518     C     *IN90         ANDEQ     *ON
045570000303     C     D48FFR        OREQ      'E'
045580000303     C     *IN91         ANDEQ     *ON
045590950518     C*
045600950518    2C     *IN10         IFEQ      *ON
045610941220     C                   UNLOCK    FNARB01L
045620941222     C                   ELSE
045630941222     C                   UNLOCK    FNBLP01L
045640950518    2C                   END
045650051118     C                   UNLOCK    Fiar901L
045660991001     C                   UNLOCK    FIAR601L
045670950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
045680950607     C* flag D48F12 = 1
045690950607     C   KL              MOVEL     '1'           D48F12
045700941222    1C                   END
045710941102     C*
045720950518     C                   ENDSR
045730941102     C*
045740941102     C* EMISSIONE E GESTIONE FORMATO 7 ------------------------------*
045750941102     C     EMFOR7        BEGSR
045760941102     C*
045770941102     C     FOR07         TAG
045780941102     C*
045790950114     C     *IN90         IFEQ      *ON
045800941212     C     D48FFR        OREQ      'S'
045810950607     C     D48FFR        OREQ      'V'
045820941102     C                   WRITE     LR48T01
045830941102     C                   EXFMT     LR48D07
045840941102     C                   END
045850941102     C*
045860941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
045870941102     C                   SETOFF                                       2890
045880941102     C                   SETOFF                                       404142
045890941102     C                   SETOFF                                       434445
045900031001     c                   Setoff                                       62
045910070329     C* F12 - RITORNO
045920941102     C     *INKL         IFEQ      *OFF
045930070329     c
045940070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
045950070329     c                   if        *in87 and *inkr
045960070329     c                   exsr      CALLNOTELDV
045970070329     c                   goto      for07
045980070329     c                   endif
045990941102     C*
046000941102     C* CONTROLLO FORMATO
046010941102     C                   EXSR      CONTR7
046020040219      * questi controlli li devo saltare se il richiamo è stato fatto con causale 'CD'
046030040219      * cioè da chiusura distina
046040040224      * o causale 'CA' creata da LR48 in automatico
046050040224     c                   If        Not *In90 and D48Cvb <> 'CD' and
046060040224     c                             d48cvb <> 'CA'
046070040219     C                   ExSr      Contr72
046080040219     c                   EndIf
046090941215     C*
046100941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046110070309     c*          non se è forzabile solo allafine dopo la REGIS
046120941215     C     *IN90         IFEQ      *ON
046130941215     C     D48FFR        ANDEQ     'E'
046140070330     c**** Msgforza      andeq     0
046150941215     C                   MOVEL     VIDMSG        D48MSG
046160941215     C                   MOVEL     '7'           D48ERR
046170941215     C                   GOTO      ENDEM7
046180941215     C                   ENDIF
046190070330     c
046200070330     c* Se non ho avuto errori e richiamo solo per controlli, esco
046210070330     c*   (d48ffr='E' e 26 off)
046220070329     c  n90
046230070330     cann26              if        d48ffr='E'
046240070329     C                   GOTO      ENDEM7
046250070329     c                   endif
046260950607     C  NKF
046270950607     CANN26
046280941102     COR 90              GOTO      FOR07
046290060301     c* richiamo routine di richiesta/controllo motivazione
046300060301     c                   if        d66mtv = 'S' and w48mct = *blanks
046310060301     c                   exsr      ges_mtv
046320060301     c* premuto f12-Ritorno
046330060301     c                   if        olv85f12 = '1'
046340060301     c                   if        ilv85wdw = ' '
046350060301     c                   goto      for07
046360060301     c                   else
046370060301     c                   eval      *inkl = '1'
046380060301     C                   GOTO      ENDEM7
046390060301     c                   endif
046400060301     c                   endif
046410060301     c* Errore da ritornare a chiamante
046420060301     C     *IN90         IFEQ      *ON
046430060301     C     D48FFR        ANDEQ     'E'
046440060301     C                   MOVEL     VIDMSG        D48MSG
046450060301     C                   MOVEL     '7'           D48ERR
046460060301     C                   GOTO      ENDEM7
046470060301     C                   ENDIF
046480060301     c                   endif
046490941102     C*
046500070307     C* F6 - AGGIORNAMENTO
046510070307     c* Se devo scrivere file proposte, aggiorno solo FIARP
046520070307     c                   if        AggioARP='S'
046530070307     c                   EXSR      REGISARP
046540070307     c                   endif
046550070308     c
046560070308     c* Aggiorno comunque per gg chiusura e consegne particolari, se
046570070308     c*  modificate, in caso di scrittura di FIARP
046580070308     c                   if        AggioARP=' ' or
046590070308     c                             arbgc1<>vidgc1  or arbgc2<>vidgc2 or
046600151230     c                             arbtc1<>vidtc1 or arbtc2<>vidtc2
046610941102     C                   EXSR      REGIS7
046620070309     c                   else
046630160119     C   10              UNLOCK    FIAR601L
046640160119     C   10              UNLOCK    Fnarb01l
046650160119     C   10              UNLOCK    FiAR901L
046660160119     C                   UNLOCK    Fnblp01l
046670070308     c                   endif
046680070308     c
046690991130     C     *IN91         IFEQ      *ON
046700991130     C     D48FFR        ANDEQ     'E'
046710991130     C                   MOVEL     VIDMSG        D48MSG
046720991130     C                   MOVEL     '7'           D48ERR
046730991130     C                   GOTO      ENDEM7
046740991130     C                   ENDIF
046750991130     C   91              GOTO      FOR07
046760950519     C                   ENDIF
046770070309     c
046780070309     c* Se non ho passato msg vincolanti, passo msg di avvertimento
046790070309     c*  se ce ne sono
046800070309     c                   if        msgforza>0
046810070309     c                   z-aDD     MSGFORZA      XX
046820070309     C                   MOVEL     MSG(XX)       D48MSG
046830070309     C                   MOVEL     'Z'           D48ERR
046840070309     c                   clear                   msgforza
046850070309     C                   ENDIF
046860950519     C*
046870950519     C     ENDEM7        TAG
046880070309     C* F12-RITORNO  --> DISALLOCO REcord
046890941222     C*
046900950519    1C     *INKL         IFEQ      *ON
046910950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046920950519     C     D48FFR        OREQ      'E'
046930950519     C     *IN90         ANDEQ     *ON
046940000303     C     D48FFR        OREQ      'E'
046950000303     C     *IN91         ANDEQ     *ON
046960070329     C     D48FFR        OREQ      'E'
046970070329     c     *in26         andeq     *off
046980950519     C*
046990950519    2C     *IN10         IFEQ      *ON
047000941220     C                   UNLOCK    FNARB01L
047010991005     C                   UNLOCK    FIAR601L
047020051118     C                   UNLOCK    FiAR901L
047030160119     c                   endif
047040160119     C***                ELSE
047050941222     C                   UNLOCK    FNBLP01L
047060160119    2C***                END
047070950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
047080950607     C* flag D48F12 = 1
047090950607     C   KL              MOVEL     '1'           D48F12
047100950519    1C                   END
047110941102     C*
047120950519     C                   ENDSR
047130021129      **
047140021129      * EMISSIONE E GESTIONE FORMATO 7/1 Supermercati ---------------*
047150021129     c     Emfor71       BegSr
047160021129
047170021129     c     For071        Tag
047180021129
047190021129     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
047200021129     c                   Write     Lr48T01
047210021129     c                   Exfmt     Lr48D071
047220021129     c                   EndIf
047230021129
047240021129      * Spengo idnicatori di posizionamento cursore
047250021129     c                   Setoff                                       2890
047260021129     c                   Setoff                                       404142
047270021129     c                   Setoff                                       434445
047280021129     c                   Setoff                                       575859
047290031001     c                   Setoff                                       62
047300070329      * F12 - RITORNO
047310021129     c                   IF        Not *InKl
047320070329     c
047330070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
047340070329     c                   if        *in87 and *inkr
047350070329     c                   exsr      CALLNOTELDV
047360070329     c                   goto      for071
047370070329     c                   endif
047380021129
047390021129      * CONTROLLO FORMATO
047400021129     c                   ExSr      Contr7
047410021129
047420021129     c  n90              ExSr      Contr71
047430031028     C  n90              ExSr      Contr72
047440070306     c
047450070306     c* Nel caso dei supermercati devo sempre aggiornare la bolla
047460070306     c*  e mai il file di proposta FIARP per sicurezza lo pulisco
047470070306     c                   clear                   aggioarp
047480070308     c                   clear                   savdcr
047490070308     c                   clear                   savTcr
047500070308     c                   clear                   savHcr
047510070306     c
047520040330      * La data non può superare i gg limiti indicati in tab. VPO
047530040330     c                   If        Not *In90
047540040330     c                   Eval      wdataiso = wdataoggi
047550040330     c                   Adddur    §vpodcr:*d    wdataiso
047560040330     c     *iso          move      wdataiso      ds_data
047570040330     c                   Move      §vpodcr       w003a
047580040330     c                   Movea     w003a         ski(13)
047590040330     c                   Z-add     13            i
047600040330     c                   ExSr      abbl
047610040330     c                   Movea     ski(13)       w003a
047620040330     c                   If        ComDcr > ds_data
047630040330     c                   Eval      *In28 = *On
047640040330     c                   Eval      *In40 = *On
047650040330     c                   Eval      *In90 = *On
047660040330     c                   Eval      VidMsg = msg(129)
047670040330     c                   Eval      VidMsg = %replace(w003a:VidMsg:
047680040330     c                                      %scan('___':VidMsg))
047690040330     c                   EndIf
047700040330     c                   EndIf
047710021129
047720021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047730021129     c                   If        *In90 and D48Ffr = 'E'
047740021129     c                   Eval      D48Msg = VidMsg
047750021129     c                   Eval      D48Err = '7'
047760021129     c                   GoTo      EndEm71
047770021129     c                   EndIf
047780140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
047790140502     c*   (d48ffr='E' e 26 off)
047800140502     c  n90
047810140502     cann26              if        d48ffr='E'
047820140502     C                   GOTO      ENDEM71
047830140502     c                   endif
047840021129
047850021129     c  NKF
047860021129     cANN26
047870021129     cOR 90              GoTo      For071
047880060302     c* richiamo routine di richiesta/controllo motivazione
047890060302     c                   if        d66mtv = 'S' and w48mct = *blanks
047900060302     c                   exsr      ges_mtv
047910060302     c                   if        olv85f12 = '1'
047920060302     c                   if        ilv85wdw = ' '
047930060302     c                   goto      for071
047940060302     c                   else
047950060302     c                   eval      *inkl = '1'
047960060302     C                   GOTO      ENDEM71
047970060302     c                   endif
047980060302     c                   endif
047990060302     C     *IN90         IFEQ      *ON
048000060302     C     D48FFR        ANDEQ     'E'
048010060302     C                   MOVEL     VIDMSG        D48MSG
048020070308     C                   MOVEL     '7'           D48ERR
048030060302     C                   GOTO      ENDEM71
048040060302     C                   ENDIF
048050060302     c                   endif
048060021129
048070021129      * CMD6 - AGGIORNAMENTO
048080021129     c                   ExSr      Regis7
048090021129
048100021129     c  n91              ExSr      Regis71
048110021129
048120021129     c                   If        *In91 and D48Ffr = 'E'
048130021129     c                   Eval      D48Msg = VidMsg
048140021129     c                   Eval      D48Err = '7'
048150021129     c                   GoTo      EndEm71
048160021129     c                   EndIf
048170021129
048180021129     c   91              GoTo      For071
048190021129
048200021129     c                   EndIf
048210021129
048220021129     c     EndEm71       Tag
048230021129
048240021129      * F12-RITORNO  --> DISALLOCO RECROD
048250021129
048260021129    1c     *INKL         IFEQ      *ON
048270021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048280021129     c     D48FFR        OREQ      'E'
048290021129     c     *IN90         ANDEQ     *ON
048300021129     c     D48FFR        OREQ      'E'
048310021129     c     *IN91         ANDEQ     *ON
048320021129
048330021129    2c     *IN10         IFEQ      *ON
048340021129     c                   UNLOCK    FNARB01L
048350021129     c                   UNLOCK    FIAR601L
048360051118     c                   UNLOCK    FiAR901L
048370021129     c                   ELSE
048380021129     c                   UNLOCK    FNBLP01L
048390021129    2c                   END
048400021129      * Comunico al chiamante che è stato premuto F12 valorizzando il
048410021129      * flag D48F12 = 1
048420021129     c   KL              MOVEL     '1'           D48F12
048430021129    1c                   END
048440021129
048450021129     c                   EndSr
048460941223     C*
048470941223     C* EMISSIONE E GESTIONE FORMATO 8 ------------------------------*
048480941223     C     EMFOR8        BEGSR
048490941223     C*
048500941223     C     FOR08         TAG
048510941223     C*
048520950114     C     *IN90         IFEQ      *ON
048530941223     C     D48FFR        OREQ      'S'
048540950607     C     D48FFR        OREQ      'V'
048550941223     C                   WRITE     LR48T03
048560941223     C                   EXFMT     LR48D08
048570941223     C                   END
048580941223     C*
048590941223     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
048600941223     C                   SETOFF                                       2890
048610941223     C                   SETOFF                                       404142
048620941223     C                   SETOFF                                       434445
048630941223     C                   SETOFF                                       464748
048640941223     C                   SETOFF                                       4950
048650941223     C* CMD12 - RITORNO
048660941223     C     *INKL         IFEQ      *OFF
048670941223     C*
048680941223     C* CONTROLLO FORMATO
048690941223     C                   EXSR      CONTR8
048700941223     C*
048710941223     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048720941223     C     *IN90         IFEQ      *ON
048730941223     C     D48FFR        ANDEQ     'E'
048740941223     C                   MOVEL     VIDMSG        D48MSG
048750941223     C                   MOVEL     '8'           D48ERR
048760941223     C                   GOTO      ENDEM8
048770941223     C                   ENDIF
048780140502
048790140502     c* Non gestita uscita per d48ffr='F' ( n90 and n26 d48ffr='E')
048800140502     c* perchè bisognerebbe verificare bene cosa fare in questo caso
048810140502
048820970110     C  NKF
048830970110     CANNKJ
048840970110     CANN26
048850941223     COR 90              GOTO      FOR08
048860060302     c* richiamo routine di richiesta/controllo motivazione
048870060302     c                   if        d66mtv = 'S' and w48mct = *blanks
048880060302     c                   exsr      ges_mtv
048890060302     c                   if        olv85f12 = '1'
048900060302     c                   if        ilv85wdw = ' '
048910060302     c                   goto      for08
048920060302     c                   else
048930060302     c                   eval      *inkl = '1'
048940060302     C                   GOTO      ENDEM8
048950060302     c                   endif
048960060302     c                   endif
048970060302     C     *IN90         IFEQ      *ON
048980060302     C     D48FFR        ANDEQ     'E'
048990060302     C                   MOVEL     VIDMSG        D48MSG
049000070308     C                   MOVEL     '8'           D48ERR
049010060302     C                   GOTO      ENDEM8
049020060302     C                   ENDIF
049030060302     c                   endif
049040941223     C*
049050970110     C* F6 O F10 - AGGIORNAMENTO
049060941223     C                   EXSR      REGIS8
049070991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049080991130     C     *IN91         IFEQ      *ON
049090991130     C     D48FFR        ANDEQ     'E'
049100991130     C                   MOVEL     VIDMSG        D48MSG
049110991130     C                   MOVEL     '8'           D48ERR
049120991130     C                   GOTO      ENDEM8
049130991130     C                   ENDIF
049140941223     C   91              GOTO      FOR08
049150941223     C*
049160950519     C                   ENDIF
049170150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
049180150112     c*  se ce ne sono
049190150112     c                   if        msgforzatxt<>*blanks
049200150112     C                   MOVEL     msgforzatxt   D48MSG
049210150112     C                   MOVEL     'Z'           D48ERR
049220150112     c                   clear                   msgforzatxt
049230150112     C                   ENDIF
049240950519     C*
049250950519     C     ENDEM8        TAG
049260950104     C* F12-RITORNO  --> DISALLOCO RECORD
049270950519    1C     *INKL         IFEQ      *ON
049280950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049290950519     C     D48FFR        OREQ      'E'
049300950519     C     *IN90         ANDEQ     *ON
049310000303     C     D48FFR        OREQ      'E'
049320000303     C     *IN91         ANDEQ     *ON
049330941223     C*
049340941223     C     *IN10         IFEQ      *ON
049350941223     C                   UNLOCK    FNARB01L
049360991005     C                   UNLOCK    FIAR601L
049370051118     C                   UNLOCK    FiAR901L
049380941223     C                   ELSE
049390941223     C                   UNLOCK    FNBLP01L
049400941223     C                   END
049410950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
049420950607     C* flag D48F12 = 1
049430950607     C   KL              MOVEL     '1'           D48F12
049440941223     C                   END
049450941223     C*
049460950519     C                   ENDSR
049470961211     C* EMISSIONE E GESTIONE FORMATO 9 ------------------------------*
049480961211     C     EMFOR9        BEGSR
049490920120     C*
049500961218     C     FOR09         TAG
049510961218     C*
049520961218     C     *IN90         IFEQ      *ON
049530961218     C     D48FFR        OREQ      'S'
049540961218     C     D48FFR        OREQ      'V'
049550961219     C                   WRITE     LR48T01
049560961218     C                   EXFMT     LR48D09
049570961218     C                   END
049580961218     C*
049590961218     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
049600961218     C                   SETOFF                                       2890
049610970110     C                   SETOFF                                       4041
049620070329     C* F12 - RITORNO
049630961218     C     *INKL         IFEQ      *OFF
049640070329     c
049650070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
049660070329     c                   if        *in87 and *inkr
049670070329     c                   exsr      CALLNOTELDV
049680070329     c                   goto      for09
049690070329     c                   endif
049700070329     C*
049710961218     C*
049720961218     C* CONTROLLO FORMATO
049730961218     C                   EXSR      CONTR9
049740961218     C*
049750961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049760961218     C     *IN90         IFEQ      *ON
049770961218     C     D48FFR        ANDEQ     'E'
049780961218     C                   MOVEL     VIDMSG        D48MSG
049790961218     C                   MOVEL     '9'           D48ERR
049800961218     C                   GOTO      ENDEM9
049810961218     C                   ENDIF
049820140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
049830140502     c*   (d48ffr='E' e 26 off)
049840140502     c  n90
049850140502     cann26              if        d48ffr='E'
049860140502     C                   GOTO      ENDEM9
049870140502     c                   endif
049880970110     C  NKF
049890970110     CANN26
049900961218     COR 90              GOTO      FOR09
049910060302     c* richiamo routine di richiesta/controllo motivazione
049920060302     c                   if        d66mtv = 'S' and w48mct = *blanks
049930060302     c                   exsr      ges_mtv
049940060302     c                   if        olv85f12 = '1'
049950060302     c                   if        ilv85wdw = ' '
049960060302     c                   goto      for09
049970060302     c                   else
049980060302     c                   eval      *inkl = '1'
049990060302     C                   GOTO      ENDEM9
050000060302     c                   endif
050010060302     c                   endif
050020060302     C     *IN90         IFEQ      *ON
050030060302     C     D48FFR        ANDEQ     'E'
050040060302     C                   MOVEL     VIDMSG        D48MSG
050050060316     C                   MOVEL     '9'           D48ERR
050060060302     C                   GOTO      ENDEM9
050070060302     C                   ENDIF
050080060302     c                   endif
050090060316     c                   exsr      regis9
050100060316     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050110060316     C     *IN91         IFEQ      *ON
050120060316     C     D48FFR        ANDEQ     'E'
050130060316     C                   MOVEL     VIDMSG        D48MSG
050140060316     C                   MOVEL     '9'           D48ERR
050150060316     C                   GOTO      ENDEM9
050160060316     C                   ENDIF
050170060316     C   91              GOTO      FOR09
050180041028     c
050190961218     C*
050200961218     C                   ENDIF
050210961218     C*
050220961218     C     ENDEM9        TAG
050230961218     C* F12-RITORNO  --> DISALLOCO RECORD
050240961218    1C     *INKL         IFEQ      *ON
050250961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050260961218     C     D48FFR        OREQ      'E'
050270961218     C     *IN90         ANDEQ     *ON
050280961218     C*
050290961218     C     *IN10         IFEQ      *ON
050300961218     C                   UNLOCK    FNARB01L
050310991005     C                   UNLOCK    FIAR601L
050320051118     C                   UNLOCK    FiAR901L
050330961218     C                   ELSE
050340961218     C                   UNLOCK    FNBLP01L
050350961218     C                   END
050360961218     C* Comunico al chiamante che è stato premuto F12 valorizzando il
050370961218     C* flag D48F12 = 1
050380961218     C   KL              MOVEL     '1'           D48F12
050390961218     C                   END
050400961211     C*
050410961211     C                   ENDSR
050420030123      **
050430030123      * EMISSIONE E GESTIONE FORMATO 10 Mancato ritiro BANCALI ------*
050440030123     c     Emfor10       BegSr
050450030123
050460030123     c     For10         Tag
050470030123
050480030123     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
050490030123     c                   Write     Lr48T01
050500030123     c                   Exfmt     Lr48D10
050510030123     c                   EndIf
050520030123
050530030123      * Spengo indicatori di posizionamento cursore
050540030123     c                   Setoff                                       2890
050550030124     c                   Setoff                                       5759
050560030124     c                   Setoff                                       6061
050570070329      * F12 - RITORNO
050580030123     c                   IF        Not *InKl
050590070329     c
050600070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
050610070329     c                   if        *in87 and *inkr
050620070329     c                   exsr      CALLNOTELDV
050630070329     c                   goto      for10
050640070329     c                   endif
050650030123
050660030123      * CONTROLLO FORMATO
050670030123     c                   ExSr      Contr10
050680030123
050690030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050700030123     c                   If        *In90 and D48Ffr = 'E'
050710030123     c                   Eval      D48Msg = VidMsg
050720030123     c                   Eval      D48Err = 'B'
050730030123     c                   GoTo      EndEm10
050740030123     c                   EndIf
050750140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050760140502     c*   (d48ffr='E' e 26 off)
050770140502     c  n90
050780140502     cann26              if        d48ffr='E'
050790140502     C                   GOTO      ENDEM10
050800140502     c                   endif
050810030123
050820030123     c  NKF
050830030123     cANN26
050840030123     cOR 90              GoTo      For10
050850060302     c* richiamo routine di richiesta/controllo motivazione
050860060302     c                   if        d66mtv = 'S' and w48mct = *blanks
050870060302     c                   exsr      ges_mtv
050880060302     c                   if        olv85f12 = '1'
050890060302     c                   if        ilv85wdw = ' '
050900060302     c                   goto      for10
050910060302     c                   else
050920060302     c                   eval      *inkl = '1'
050930060302     C                   GOTO      ENDEM10
050940060302     c                   endif
050950060302     c                   endif
050960060302     C     *IN90         IFEQ      *ON
050970060302     C     D48FFR        ANDEQ     'E'
050980060302     C                   MOVEL     VIDMSG        D48MSG
050990070308     C                   MOVEL     'B'           D48ERR
051000060302     C                   GOTO      ENDEM10
051010060302     C                   ENDIF
051020060302     c                   endif
051030030123
051040030123      * CMD6 - AGGIORNAMENTO
051050030123     c                   ExSr      Regis10
051060030123
051070030123     c                   If        *In91 and D48Ffr = 'E'
051080030123     c                   Eval      D48Msg = VidMsg
051090030123     c                   Eval      D48Err = 'B'
051100030123     c                   GoTo      EndEm10
051110030123     c                   EndIf
051120030123
051130030123     c   91              GoTo      For10
051140030123
051150030123     c                   EndIf
051160030123
051170030123     c     EndEm10       Tag
051180030123
051190030123      * F12-RITORNO  --> DISALLOCO RECROD
051200030123
051210030123    1c     *INKL         IFEQ      *ON
051220030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051230030123     c     D48FFR        OREQ      'E'
051240030123     c     *IN90         ANDEQ     *ON
051250030123     c     D48FFR        OREQ      'E'
051260030123     c     *IN91         ANDEQ     *ON
051270030123
051280030123    2c     *IN10         IFEQ      *ON
051290030123     c                   UNLOCK    FNARB01L
051300030123     c                   UNLOCK    FIAR601L
051310051118     c                   UNLOCK    FiAR901L
051320030124     c                   UNLOCK    Fiar501L
051330030123     c                   ELSE
051340030123     c                   UNLOCK    FNBLP01L
051350030123    2c                   END
051360030123      * Comunico al chiamante che è stato premuto F12 valorizzando il
051370030123      * flag D48F12 = 1
051380030123     c   KL              MOVEL     '1'           D48F12
051390030123    1c                   END
051400030123
051410030123     c                   EndSr
051420061107     C
051430061107     C*--- EMISSIONE E GESTIONE FORMATO 11 ---------------------------*
051440061107     C     EMFOR11       BEGSR
051450061107     C*
051460061107     C     FOR11         TAG
051470061107     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
051480061107    1C     *IN90         IFEQ      *ON
051490061107     C     D48FFR        OREQ      'S'
051500061107     C     D48FFR        OREQ      'V'
051510061107     C                   WRITE     LR48T01
051520061107     C                   EXFMT     LR48D11
051530061107    1C                   END
051540061107     C*
051550061107     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051560061107     C                   SETOFF                                       2890
051570061107     C                   SETOFF                                       4041
051580061107     C* F12 - RITORNO
051590061107    1C     *INKL         IFEQ      *OFF
051600061107     C*
051610061107     C* CONTROLLO FORMATO
051620061107     C                   EXSR      CONTR11
051630061107     C*
051640061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051650061107    3C     *IN90         IFEQ      *ON
051660061107     C     D48FFR        ANDEQ     'E'
051670061107     C                   MOVEL     VIDMSG        D48MSG
051680061107     C                   MOVEL     'F'           D48ERR
051690061107     C                   GOTO      ENDEM11
051700061107    3C                   ENDIF
051710071025     c
051720071025     c* Se non ho avuto errori e richiamo solo per controlli, esco
051730071025     c*   (d48ffr='E' e 26 off)
051740071025     c  n90
051750071025     cann26              if        d48ffr='E'
051760071025     C                   GOTO      ENDEM11
051770071025     c                   endif
051780061107     C  NKF
051790061107     CANN26
051800061107     COR 90              GOTO      FOR11
051810061107     c* richiamo routine di richiesta/controllo motivazione
051820061107     c                   if        d66mtv = 'S' and w48mct = *blanks
051830061107     c                   exsr      ges_mtv
051840061107     c                   if        olv85f12 = '1'
051850061107     c                   if        ilv85wdw = ' '
051860061107     c                   goto      for11
051870061107     c                   else
051880061107     c                   eval      *inkl = '1'
051890061107     C                   GOTO      ENDEM11
051900061107     c                   endif
051910061107     c                   endif
051920061107     C     *IN90         IFEQ      *ON
051930061107     C     D48FFR        ANDEQ     'E'
051940061107     C                   MOVEL     VIDMSG        D48MSG
051950061107     C                   MOVEL     'F'           D48ERR
051960061107     C                   GOTO      ENDEM11
051970061107     C                   ENDIF
051980061107     c                   endif
051990061107     C*
052000071026     c                   clear                   Aggiobol
052010061107     C                   EXSR      REGIS11
052020061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052030061107     C     *IN91         IFEQ      *ON
052040061107     C     D48FFR        ANDEQ     'E'
052050061107     C                   MOVEL     VIDMSG        D48MSG
052060061107     C                   MOVEL     'F'           D48ERR
052070061107     C                   GOTO      ENDEM11
052080061107     C                   ENDIF
052090061107     C   91              GOTO      FOR11
052100061107     C*
052110061107    1C                   ENDIF
052120061107     C*
052130061107     C     ENDEM11       TAG
052140061107     C*
052150061107    1C     *INKL         IFEQ      *ON
052160061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052170061107    1C     D48FFR        OREQ      'E'
052180061107     C     *IN90         ANDEQ     *ON
052190061107    1C     D48FFR        OREQ      'E'
052200061107     C     *IN91         ANDEQ     *ON
052210061107     C* F12-RITORNO
052220061107    2C     *IN10         IFEQ      *ON
052230061107     C                   UNLOCK    FNARB01L
052240061107     C                   UNLOCK    FiAR901L
052250061107     C                   ELSE
052260061107     C                   UNLOCK    FNBLP01L
052270061107    2C                   END
052280061107     C* Comunico al chiamante che è stato premuto F12 valorizzando il
052290061107     C* flag D48F12 = 1
052300061107     C   KL              MOVEL     '1'           D48F12
052310061107    1C                   END
052320061107     c
052330061107     C                   UNLOCK    Fiar601l
052340061107     C*
052350061107     C                   ENDSR
052360130927     c
052370130927     C*--- EMISSIONE E GESTIONE FORMATO 12  variaz.'"Nx' -------------*
052380130927     C     EMFOR12       BEGSR
052390130927     C*
052400130927     C     FOR12         TAG
052410130927     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
052420130927    1C     *IN90         IFEQ      *ON
052430130927     C     D48FFR        OREQ      'S'
052440130927     C     D48FFR        OREQ      'V'
052450130927     C                   WRITE     LR48T01
052460130927     C                   EXFMT     LR48D12
052470130927    1C                   END
052480130927     C*
052490130927     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
052500130927     C                   SETOFF                                       2890
052510130930     C                   SETOFF                                       41
052520130927     C* F12 - RITORNO
052530130927    1C     *INKL         IFEQ      *OFF
052540130927     c
052550130927     c* F17 - Note LDV   (solo se abilitato geo 87 on)
052560130927     c                   if        *in87 and *inkr
052570130927     c                   exsr      CALLNOTELDV
052580130927     c                   goto      for12
052590130927     c                   endif
052600130927     C*
052610130927     C* CONTROLLO FORMATO
052620130927     C                   EXSR      CONTR12
052630130927     C*
052640130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052650130927    3C     *IN90         IFEQ      *ON
052660130927     C     D48FFR        ANDEQ     'E'
052670130927     C                   MOVEL     VIDMSG        D48MSG
052680130927     C                   MOVEL     'F'           D48ERR
052690130927     C                   GOTO      ENDEM12
052700130927    3C                   ENDIF
052710130927     c
052720130927     c* Se non ho avuto errori e richiamo solo per controlli, esco
052730130927     c*   (d48ffr='E' e 26 off)
052740130927     c  n90
052750130927     cann26              if        d48ffr='E'
052760130927     C                   GOTO      ENDEM12
052770130927     c                   endif
052780130927     C  NKF
052790130927     CANN26
052800130930     COR 90              GOTO      FOR12
052810130927     c* richiamo routine di richiesta/controllo motivazione
052820130927     c                   if        d66mtv = 'S' and w48mct = *blanks
052830130927     c                   exsr      ges_mtv
052840130927     c                   if        olv85f12 = '1'
052850130927     c                   if        ilv85wdw = ' '
052860130927     c                   goto      for12
052870130927     c                   else
052880130927     c                   eval      *inkl = '1'
052890130927     C                   GOTO      ENDEM12
052900130927     c                   endif
052910130927     c                   endif
052920130927     C     *IN90         IFEQ      *ON
052930130927     C     D48FFR        ANDEQ     'E'
052940130927     C                   MOVEL     VIDMSG        D48MSG
052950130927     C                   MOVEL     'F'           D48ERR
052960130927     C                   GOTO      ENDEM12
052970130927     C                   ENDIF
052980130927     c                   endif
052990130927     C*
053000130927     c                   clear                   Aggiobol
053010130927     C                   EXSR      REGIS12
053020130927     c
053030130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053040130927     C     *IN91         IFEQ      *ON
053050130927     C     D48FFR        ANDEQ     'E'
053060130927     C                   MOVEL     VIDMSG        D48MSG
053070130927     C                   MOVEL     'F'           D48ERR
053080130927     C                   GOTO      ENDEM12
053090130927     C                   ENDIF
053100130927     C   91              GOTO      FOR12
053110130927     C*
053120130927    1C                   ENDIF
053130130927     C*
053140130927     C     ENDEM12       TAG
053150130927     C*
053160130927    1C     *INKL         IFEQ      *ON
053170130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
053180130927    1C     D48FFR        OREQ      'E'
053190130927     C     *IN90         ANDEQ     *ON
053200130927    1C     D48FFR        OREQ      'E'
053210130927     C     *IN91         ANDEQ     *ON
053220130927     C* F12-RITORNO
053230130927    2C     *IN10         IFEQ      *ON
053240130927     C                   UNLOCK    FNARB01L
053250130927     C                   UNLOCK    FiAR901L
053260130927     C                   ELSE
053270130927     C                   UNLOCK    FNBLP01L
053280130927    2C                   END
053290130927     C* Comunico al chiamante che è stato premuto F12 valorizzando il
053300130927     C* flag D48F12 = 1
053310130927     C   KL              MOVEL     '1'           D48F12
053320130927    1C                   END
053330130927     c
053340130927     C                   UNLOCK    Fiar601l
053350130927     C*
053360130927     C                   ENDSR
053370070329     C
053380070329     C*--- richiama pgm per immettere note LDV -----------------------*
053390070329     C     CALLNoteLDV   BEGSR
053400140130     c                   clear                   fnlrn6ds
053410140130     c                   eval      in6mod='I'
053420140130     c                   eval      in6aas=arbaas
053430140130     c                   eval      in6lnp=arblnp
053440140130     c                   eval      in6nrs=arbnrs
053450140130     c                   eval      in6nsp=arbnsp
053460140130     c                   eval      in6pgm='FNLR48R'
053470140130     c                   eval      IN6FGS=arbifp
053480140130     c                   eval      IN6NFV=arbndc
053490140130     c                   eval      IN6DFV=arbddc
053500140130     c                   eval      IN6PDR=arbpdc
053510140130     c                   if        arbdcm=0  and arbndc>0
053520140130     c                   eval      IN6CONS='S'
053530140130     c                   endif
053540140130     c                   call      'FNLRN6R'
053550070329     c                   parm                    kpjba
053560140130     c                   parm                    fnlrn6ds
053570070329     c
053580070329     c                   ENDSR
053590061107     C
053600920120     C*--- CONTROLLI FORMATO2 ----------------------------------------*
053610920120     C     CONTR2        BEGSR
053620941025     C                   SETOFF                                       9028
053630020531     c                   clear                   wdisag
053640030122      * A T T E N Z I O N E!!!
053650030122      * Per la modifica destinatario
053660030122      * Se il nuovo destinatario necessita della consegna particolare
053670030122      * alla fine della REGIS2 viene impostata la causae 'CP' per
053680030122      * poter effettuare una nuova variazione bolla senza uscire dal pgm
053690941020     C***
053700941020     C*** DESTINATARIO  ***
053710941025    1C     *IN01         IFEQ      '1'
053720141112
053730141112     c* messaggio di avviso se pgm non richiamato e bolla con
053740141120     c* giacenza aperta  - Solo se variati dati del destinatario
053750141112     c                   if        *in20=*off and wforzgiac=0
053760141112     c                             and *in04
053770141120    5C     SAVCAD        IFNE      VIDCAD
053780141120     C     SAVLOD        ORNE      VIDLOD
053790141120     C     SAVind        ORNE      VIDind
053800141120     C     SAVprd        ORNE      VIDprd
053810141120     C     SAVnzd        ORNE      VIDnzd
053820141112     c                   eval      wforzgiac=1
053830141112     C                   SETON                                        2890
053840141112     C                   MOVEL     MSG(157)      VIDMSG
053850141112     C                   GOTO      ENDCT2
053860141112     c                   endif
053870141120     c                   endif
053880141112
053890970808     C                   MOVEL     VIDRSD        I14RSC
053900970808     C                   MOVE      VIDRSD        I14RS2
053910970808     C                   MOVEL     VIDIND        I14IND
053920970808     C                   MOVEL     VIDLOD        E14LOC
053930970808     C                   MOVEL     VIDPID        E14PIP
053940970808     C                   MOVEL     VIDPRD        E14PRV
053950970808     C                   MOVEL     VIDNZD        E14NAR
053960970808     C                   MOVEL     VIDCAD        E14CAP
053970970808     C                   MOVEL     VIDFIN        I14ISO
053980970808     C                   MOVEL     'D'           WCLI              1
053990970808     C                   EXSR      CTRIND
054000941020     C*
054010941020     C* CAMPI CHE POSSO AVER IMPOSTATO
054020970808     C                   MOVEL     E14LOC        VIDLOD
054030970808     C                   MOVEL     E14PIP        VIDPID
054040970808     C                   MOVEL     E14PRV        VIDPRD
054050970808     C                   MOVEL     E14NAR        VIDNZD
054060970808     C                   MOVEL     E14CAP        VIDCAD
054070970808     C* ERRORE
054080970808     C   90              GOTO      ENDCT2
054090060111     c* salvo lna desunta da cappario per non sporcarla se destinatario
054100060111     c* che richiede una particolare lna diversa da quella del cappario
054110060111     c                   move      o95lna        d_o95lna
054120020531     c* Verifico se destinatario disagiato
054130020604     c* Per bolle poste non faccio niente
054140060111     c                   clear                   ddstflo
054150020604     c     lnpntw        ifne      'PPT'
054160040702      * Referente e numero di telefono: se campi a video con '#'
054170040702      * sono quelli automatici (dal tisit5) e quindi li pulisco
054180040702     c                   If        %subst(VidRef:1:1) = '#'
054190040702     c                   Clear                   VidRef
054200040702     c                   EndIf
054210040705     c                   If        %subst(VidTeld:1:1) = '#'
054220040702     c                   Clear                   VidTeld
054230040702     c                   EndIf
054240020531     c                   clear                   tisit0ds
054250020531     c                   movel     'E'           it0tla
054260020531     c                   movel     vidnzd        it0naz
054270020531     c                   movel     vidprd        it0prv
054280020531     c                   movel     vidcad        it0cap
054290020531     c                   movel     vidlod        it0loc
054300020531     c                   movel     vidind        it0ind
054310020531     c                   movel     vidrsd        it0rag
054320020531     c                   call      'TISIT5R'
054330020531     c                   parm                    tisit0ds
054340020531     c     ot0err        ifeq      *blanks
054350060113     c                   movel     ot0flo        ddstflo
054360060111   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
054370060111     c                             ot0dos = 'S'
054380020531     c                   movel     ot0dos        wdisag
054390020531     c     wdisag        ifeq      'D'
054400020531     c                   movel     'X'           wdisag
054410020531     c                   endif
054420060111     c                   endif
054430020531     c                   else
054440020531     c                   clear                   wdisag
054450020531     c                   endif
054460060327     c***                If        wdisag <> *Blanks
054470060327     c***                          and %subst(arbgva:1:1) <> 'U' or
054480060327     c***                          wdisag='X' and %subst(arbgva:1:1)='U'
054490060327     c                   if        (wdisag = 'X') OR
054500060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
054510060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
054520060327     c                             (arbdcr=*zeros or arbtcr <> *blanks))
054530040702     c                   If        VidTeld = *Blanks
054540040702     c                   Eval      VidTeld = ot0tel
054550040702     c                   EndIf
054560040702     c                   If        VidRef = *Blanks
054570040702     c                   Eval      VidRef =  ot0ref
054580040702     c                   EndIf
054590040702     c                   EndIf
054600020604     c                   endif
054610060113     c*
054620060113     c                   if        §dstlna > *zeros
054630060111     c                   move      §dstlna       d_o95lna
054640060111     c                   endif
054650970808     C*
054660970808     C* MODIFICATI CAP E/O LOCALITA':
054670970808     C* 1) AGGIORNO A VIDEO IL FLAG INOLTRO E AVVISO CON MESSAGGIO
054680970808     C* 2) SE VARIA ANCHE LA LNA E CAP E LOCALITA SONO DIVERSI DALLA
054690970808     C*    BOLLA EMMETTO MESSAGGIO FORZABILE MANTENENDO COMUNQUE LA
054700970808     C*    LINEA DI ARRIVO E ZONA DELLA BOLLA
054710970808     C     WCAD          IFNE      VIDCAD
054720970808     C     WLOD          ORNE      VIDLOD
054730970808     C                   MOVE      VIDCAD        WCAD
054740970808     C                   MOVE      VIDLOD        WLOD
054750970808     C* FLAG INOLTRO
054760970808     C     O95ISO        IFNE      VIDFIN
054770970808     C                   MOVE      O95ISO        VIDFIN
054780140507     c* NON avviso se richiamo cieco
054790140507     c                   if        d48ffr<>'E'
054800970808     C                   SETON                                        289049
054810970808     C                   MOVEL     MSG(77)       VIDMSG
054820970808     C                   GOTO      ENDCT2
054830140507     c                   endif
054840970808     C                   END
054850970808     C                   END
054860970828     C* FLAG INOLTRO
054870970828     C     VIDFIN        IFEQ      ' '
054880970828     C                   MOVE      O95ISO        VIDFIN
054890140507     c* NON avviso se richiamo cieco
054900140507     c                   if        d48ffr<>'E'
054910970828     C                   SETON                                        289049
054920970828     C                   MOVEL     MSG(77)       VIDMSG
054930010412     C                   GOTO      ENDCT2
054940140507     c                   endif
054950970828     C                   END
054960010412     C  N29
054970010412     CANN71VIDFIN        IFEQ      'D'
054980010412     C     O95PID        ANDEQ     'N'
054990010412     C                   SETON                                        289049
055000010412     C                   MOVEL     MSG(98)       VIDMSG
055010010412     C                   GOTO      ENDCT2
055020010412     C                   ENDIF
055030970808     C*
055040140507     c* Non faccio il seguente controllo se richiamo "cieco"
055050141029     c                   if        %lookup('999':skFilOkdir)=0 and
055060141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)=0
055070141029     c                             or %lookup(%editc(dutpou:'X'):skFilOkdir)=0)
055080141029     c                   if        d48ffr<>'E'
055090141029     C     WCAD1         IFNE      VIDCAD
055100141029     C     WLOD1         ORNE      VIDLOD
055110141029     C     Wrsd1         ORNE      VIDrsd
055120141029     C     Wprd1         ORNE      VIDprd
055130141029     C     Wnzd1         ORNE      VIDnzd
055140141029     C     Wind1         ORNE      VIDind
055150141029     C                   MOVE      VIDCAD        WCAD1
055160141029     C                   MOVE      VIDLOD        WLOD1
055170141029     C                   MOVE      VIDrsd        Wrsd1
055180141029     C                   MOVE      VIDind        Wind1
055190141029     C                   MOVE      VIDprd        Wprd1
055200141029     C                   MOVE      VIDnzd        Wnzd1
055210141029     C     d_o95lna      ifne      ARBLNA
055220141029     C     SAVCAD        IFNE      VIDCAD
055230060111     C***  O95LNA        ANDNE     ARBLNA
055240141029     C     SAVLOD        ORNE      VIDLOD
055250060111     C***  O95LNA        ANDNE     ARBLNA
055260141029     C     SAVrsd        ORNE      VIDrsd
055270141029     C     SAVind        ORNE      VIDind
055280141029     C     SAVprd        ORNE      VIDprd
055290141029     C     SAVnzd        ORNE      VIDnzd
055300141029     C                   SETON                                        9028
055310141029     C                   MOVEL     MSG(76)       VIDMSG
055320141029     C                   GOTO      ENDCT2
055330141029     C                   END
055340141029     C                   END
055350141029     C                   END
055360141029     c                   endif
055370141029     c                   endif
055380941020     C**
055390950522     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
055400950522     C* obbligatoria
055410950412     C     *IN05         IFEQ      *OFF
055420950412     C     *IN72         ANDEQ     *OFF
055430950522     C     WDESIE        ANDEQ     'E'
055440950522     C     DESEFT        ANDNE     *BLANKS
055450950412     C     VIDPID        ANDEQ     *BLANKS
055460950412     C                   SETON                                        284590
055470950412     C                   MOVEL     MSG(67)       VIDMSG
055480950412     C                   GOTO      ENDCT2
055490950412     C                   END
055500061116     c
055510061116     c* Se modifica la ragione sociale del destinatario, controllare
055520061116     c*  la partita iva
055530140507     c* Non controllo se chiamata pgm richiamato "cieco"
055540140507     c                   if        d48ffr<>'E'
055550061116     c                   if        vidrsd<>savrsdQ and vidcdf<>*blanks
055560061116     C                   SETON                                        285190
055570061116     C                   MOVEL     MSG(134)      VIDMSG
055580061116     c                   movel     vidrsd        savrsdQ
055590061116     C                   GOTO      ENDCT2
055600061116     C                   END
055610140507     c                   endif
055620061116     c
055630061116     c* La memorizzo anche se cdf=*blanks
055640061116     c                   movel     vidrsd        savrsdQ
055650061115     c
055660061115      * Devo richiamare il nuovo driver fatto per controllare il codice
055670061115      * fiscale
055680061115     c                   if        vidcdf<>*blanks
055690151105     c                   clear                   xcfiva1ds
055700061115     c                   clear                   xcfivamod
055710061115     c                   eval      xcfivapar = vidcdf
055720061115     c                   eval      xcfivanar = vidnzd
055730061115     c                   eval      xcfivaprv = vidprd
055740151105     c                   eval      xcfivacap = vidcad
055750151105     c                   eval      xcfivaloc = vidlod
055760151105     c                   eval      xcfivapiva= vidpid
055770151105     c                   call      'XCFIVAR1'
055780151105     c                   parm                    xcfiva1ds
055790061115     c*
055800061115     c                   if        xcfivaflg < *zeros
055810061115     C                   SETON                                        289051
055820080505     C                   MOVEL     xcfivamsg     VIDMSG
055830061115     C                   GOTO      ENDCT2
055840061115     c                   endif
055850061115     c                   endif
055860120508     c* Parita iva $$ non ammessa per fattura di filiale
055870120508     c                   move      v1cksc        w0040
055880120508     c                   if        %scan('$$':vidpid)>0  and w0040=9999
055890120508     C                   SETON                                        284590
055900120508     C                   MOVEL     MSG(149)      VIDMSG
055910120508     C                   GOTO      ENDCT2
055920120508     c                   endif
055930070508     c
055940140408     c
055950070508     c                   movel     vidffd        wffd
055960141029     c* Il fermo deposito non si può inserire se presente dispo di dirottamento da
055970141029     c* eseguire
055980141029     c                   if        vidffd<>*blanks and arbffd<>wffd
055990141029     c                   clear                   fnlry09ds2
056000141029     c                   eval      ILRY09TCH='T'
056010141029     c                   eval      ILRY09TDIS='D'
056020141029     c                   eval      ILRY09AAS=d48aas
056030141029     c                   eval      ILRY09LNP=d48lnp
056040141029     c                   eval      ILRY09NRS=d48nrs
056050141029     c                   eval      ILRY09NSP=d48nsp
056060141029     c                   eval      kpjbu=fnlry09ds2
056070141031     c                   call      'FNLRY09C'
056080141029     c                   parm                    kpjba
056090141029     c                   eval      fnlry09ds2=kpjbu
056100141029     c                   if        OLRY09ESIT='1'
056110141029     C                   SETON                                        286890
056120141029     C                   MOVEL     MSG(155)      VIDMSG
056130141029     C                   GOTO      ENDCT2
056140141029     c                   endif
056150141029     c                   endif
056160140408     c* Il fermo deposito non si può inserire se presente particolarità consegna
056170140408     c* che prevede che il fermo deposito debba essere autorizzato dal mittente
056180140408     c                   if        vidffd<>*blanks and arbffd<>wffd and
056190140408     c                             arbgma<>*blanks
056200140408     c                   Movel     '7R'          cod
056210140408     c                   Movel(p)  ArbGMa        key
056220140408     c                   ExSr      ctabel
056230140408     c  n30              Movel     TblUni        ds7R
056240140408     c   30              Clear                   ds7R
056250140408     C                   IF        §7RFD='S' AND vidcvr<>'I7'
056260140408     C                   SETON                                        289068
056270140408     C                   MOVEL     MSG(153)      VIDMSG
056280140408     C                   GOTO      ENDCT2
056290140408     C                   ENDIF
056300140408     c                   endif
056310140408     c* Da contattare impostabile solo se immesso il fermo deposito
056320140408     c*  solo se GEO attiva
056330141223     c* NON faccio questo controllo se richiamo cieco
056340141223     c                   if        d48ffr<>'E'
056350140213     c   77              if        (arbffd=wffd or  vidffd='  ') and
056360070508     c                             vidtfd='SI'
056370070508     C                   SETON                                        289052
056380070508     C                   MOVEL     MSG(99)       VIDMSG
056390070508     C                   GOTO      ENDCT2
056400070508     c                   endif
056410141223     c                   endif
056420070508     C
056430070508     c* se immesso fermo deposito e c'e' la consegna richiesta avviso
056440140507     c* NON faccio questo controllo se richiamo cieco
056450140507     c                   if        d48ffr<>'E'
056460140213     c   77              if        vidffd='SI' and arbffd<>wffd and
056470070508     c                             vidtfd='  ' and arbdcr>0  AND WZ3=0
056480070508     C                   SETON                                        289052
056490070508     C                   MOVEL     MSG(143)      VIDMSG
056500070508     C                   EVAL      WZ3=1
056510070508     C                   GOTO      ENDCT2
056520140213     c                   endif
056530140507     c                   endif
056540061115     c
056550941025    1C                   ENDIF
056560941019     C*
056570930505     C***
056580941025    1C     *IN02         IFEQ      '1'
056590930505     C* PESO
056600030203      * Se non è protetto
056610030203   2ac                   If        Not *In88
056620930505     C* NON A 0 O MAGGIORE DEL LIMITE POSTO
056630980406     C     VIDPKF        IFEQ      0
056640941020     C                   SETON                                        479028
056650941020     C                   MOVEL     MSG(2)        VIDMSG
056660911209     C                   GOTO      ENDCT2
056670941025    2C                   END
056680000510     C* DPD NO MAGGIORE DEL LIMITE IMPOSTO
056690020318    2C     LNPNTW        IFEQ      'DPD'
056700020318     C     LNANTW        OREQ      'DPD'
056710000510    3C     VIDPKF        IFGT      §3IPKD
056720000510     C                   SETON                                        479028
056730000510     C                   MOVEA     MSG(52)       K78
056740000510     C                   MOVEL     §3IPKD        W006A             6
056750000510     C                   MOVEA     W006A         SKI(10)
056760000510     C                   Z-ADD     10            I                 3 0
056770000510     C                   EXSR      ABBL
056780000510     C                   MOVEA     SKI(10)       K78(53)
056790000510     C                   MOVE      §3IPKD        W001A
056800000510     C                   MOVEL     W001A         K78(60)
056810000510     C                   MOVEA     K78           VIDMSG
056820000510     C                   GOTO      ENDCT2
056830000510    3C                   END
056840000510    2C                   END
056850000510     C**
056860000510     C**
056870000510     C     VARPKF        IFNE      VIDPKF
056880000510     C                   CLEAR                   WPT1
056890000510     C                   ENDIF
056900000510     C*
056910000510     C     VARPKF        IFNE      VIDPKF
056920000510     C     ARBFVF        ANDEQ     'K'
056930000510     C                   CLEAR                   SET
056940000510     C                   ENDIF
056950000510     C                   MOVEL     VIDPKF        VARPKF
056960000510     C**
056970021121
056980021121     c                   Move      ArbCtr        DueCtr
056990021121
057000021121      * Bolla FedEx con tariffa documenti il peso non può essere superiore a
057010021121      * quanto indicato in tabella "DFE" --> msg forzabile
057020140507     c* Non controllo se progamma richiamato in modalità "cieca"
057030140507     c                   if        d48ffr<>'E'
057040021121If  2c                   If        LnaNtw = 'FED' and DueCtr >= §DfeDalD and
057050021121     c                             DueCtr <= §DfeAlD and VidPkf > §DfePkgD
057060021121     c                             and Wpt1 = *Blanks
057070021121     c                   Eval      *In47 = *On
057080021121     c                   Eval      *In28 = *On
057090021121     c                   Eval      *In90 = *On
057100021121      * Preparo il msg
057110021121     c                   Movea     Msg(116)      K78
057120021121     c                   Movel     §DfePkgD      W006a
057130021121     c                   Movea     W006a         Ski(10)
057140021121     c                   Z-Add     10            i
057150021121     c                   ExSr      Abbl
057160021121     c                   Movea     Ski(10)       K78(50)
057170021121     c                   Move      §DfePkgD      W001a
057180021121     c                   Movel     W001a         K78(57)
057190021121     c                   Movea     K78           VidMsg
057200021121     c                   Eval      Wpt1 = '1'
057210021121     c                   GoTo      EndCt2
057220021121    2c                   EndIf
057230140507     c                   endif
057240051129      * Nel caso di peso totalmente rilevato da VDL posso modificare solo se
057250051129      * inserisco il peso uguale a quello VDL
057260030123     c**!!!              If         ArbNcp = ArbNcl and VidPkc <> VidPkf
057270030123     c                   If         wfpf = 'T' and VidPkc <> VidPkf
057280030122     c                   Seton                                        479028
057290030122     c                   Eval      VidMsg = Msg(121)
057300030122     c                   GoTo      EndCt2
057310030122    2c                   EndIf
057320051129      * Nel caso di peso parzialmente rilevato da VDL posso immettere un peso uguale
057330051129      * o superiore a quello VDL
057340030123     c                   If        wfpf = 'Z' and VidPkf < VidPkc
057350030123     c                   Seton                                        479028
057360030123     c                   Eval      VidMsg = Msg(123)
057370030123     c                   GoTo      EndCt2
057380030123    2c                   EndIf
057390030203   2ac                   EndIf
057400021121
057410930505     C* VOLUME
057420091124      * Se non è protetto
057430091124   2ac                   If        Not *In89
057440091124     c
057450980406     C     VIDVLF        IFEQ      0
057460941215     C                   SETON                                        489028
057470941020     C                   MOVEL     MSG(3)        VIDMSG
057480911209     C                   GOTO      ENDCT2
057490941025    2C                   ENDIF
057500941214     C*
057510941027     C* SE IL TIPO VOLUME E' "Z" NON SI PUO' ABBASSARE
057520941027     C     VIDFVF        IFEQ      'Z'
057530941027     C     VIDVLF        ANDLT     ARBVLF
057540941215     C                   SETON                                        489028
057550941027     C                   MOVEL     MSG(5)        VIDMSG
057560941027     C                   GOTO      ENDCT2
057570941027    2C                   ENDIF
057580091124     c*
057590091124     c* Per volume totaole variabile solo come il totale
057600091124     c                   If         ArbNcr = ArbNcl and Vidvlf <> arbvlc
057610091124     c                   Seton                                        489028
057620091124     c                   Eval      VidMsg = Msg(147)
057630091124     c                   GoTo      EndCt2
057640091124    2c                   EndIf
057650091124     c
057660091124      * Nel caso di vol  parzialmente rilevato da VDL posso immettere un vol  uguale
057670091124      * o superiore a quello VDL
057680091124     c                   If         ArbNcr < ArbNcl and Vidvlf < arbvlc
057690091124     c                              and arbncr>0
057700091124     c                   Seton                                        489028
057710091124     c                   Eval      VidMsg = Msg(148)
057720091124     c                   GoTo      EndCt2
057730091124    2c                   EndIf
057740091124   2ac                   EndIf
057750941214     C*
057760051129     C* SE IL TIPO VOLUME NON E' "Z" MA ESISTE UN VOLUME VDL CHE DIVENT
057770941214     C*  SUPERIORE A QUELLO DA FATTURARE DOPO LA VARIAZIONE, AVVISO E
057780941215     C*  MODIFICO
057790941215     C     VARVLF        IFNE      VIDVLF
057800941215     C                   CLEAR                   SET
057810941215     C                   MOVEL     VIDVLF        VARVLF
057820941215     C                   ENDIF
057830941215     C*
057840051129     C*  POI IMPOSTERO NEL VOLUME DA FATTURARE IL VOLUME VDL
057850941214    2C     SET           IFEQ      0
057860941214     C     ARBVLC        ANDGT     0
057870941214     C     VIDFVF        ANDNE     'Z'
057880941214     C     VIDFVF        ANDNE     'T'
057890941214     C*
057900051129    3C     WVDL          IFEQ      'S'
057910970418    4C     WFVF          IFEQ      'Z'
057920941222     C   06              MOVEL     ZAFA          WSOST             1
057930941222     C  N06              MOVEL     ZAAA          WSOST
057940970418   X4C                   ELSE
057950970418     C   06              MOVEL     TAFA          WSOST             1
057960970418     C  N06              MOVEL     TAAA          WSOST
057970970418    4C                   END
057980970418   X3C                   ELSE
057990970418    4C     WFVF          IFEQ      'Z'
058000941222     C   06              MOVEL     ZAFN          WSOST             1
058010941222     C  N06              MOVEL     ZAAN          WSOST
058020970418     C                   ELSE
058030970418     C   06              MOVEL     TAFN          WSOST             1
058040970418     C  N06              MOVEL     TAAN          WSOST
058050970418    4C                   ENDIF
058060970418    3C                   ENDIF
058070941214     C*
058080941214     C* SE VOLUME SAREBBE DA SOSTITUIRE VEDO SE DIVENTA SUPERIORE
058090941214     C* DEL VOLUME DA FATTURARE
058100941214    3C     WSOST         IFEQ      'S'
058110970418     C                   CLEAR                   WSOSC
058120941214     C*
058130941214    4C     VIDVLF        IFLT      ARBVLF
058140941214     C     VIDVLF        ANDLT     ARBVLC
058150970418     C* PRENDO FLAG DI SOSTITUIBILITA' VOLUME REALE
058160970418     C                   MOVEL     '7T'          COD
058170970418     C                   MOVEL(P)  'R'           KEY
058180970418     C                   EXSR      CTABEL
058190970418     C   30              CLEAR                   DS7T
058200970418     C  N30              MOVEL     TBLUNI        DS7T
058210970418     C     WFVF          IFEQ      'Z'
058220970418     C                   MOVEL     §7TSOZ        WSOSC
058230970418     C                   ELSE
058240970418     C                   MOVEL     §7TSOT        WSOSC
058250970418     C                   END
058260970418     C* (SOLO SE IL VOLUME E' SOSTITUIBILE)
058270970418    5C     WSOSC         IFEQ      'S'
058280140507     C                   ADD       1             SET
058290140507     c* non segnalo se pgm richiamato in modalità "cieca"
058300140507     c                   if        d48ffr<>'E'
058310941214     C                   SETON                                        482890
058320941214     C                   MOVEL     MSG(58)       VIDMSG
058330941214     C                   GOTO      ENDCT2
058340140507     c                   endif
058350970418    5C                   ENDIF
058360941214    4C                   ENDIF
058370941214     C*
058380941214     C* SE VOLUME AUTOMATICO E MODIFICATO IL PESO CONTROLLO
058390970418   4AC     *IN88         IFEQ      *OFF
058400941214     C                   Z-ADD     VIDVLF        D20VLU
058410941214     C                   EXSR      VOLAUT
058420970418    4C     D20VLU        IFLT      ARBVLC
058430970418     C                   MOVEL     '7T'          COD
058440970418     C                   MOVEL(P)  ARBFVF        KEY
058450970418     C                   EXSR      CTABEL
058460970418     C   30              CLEAR                   DS7T
058470970418     C  N30              MOVEL     TBLUNI        DS7T
058480051129     C* FLAG DI SOSTITUIBILITA' DA VOL.VDL
058490970418     C     WFVF          IFEQ      'Z'
058500970418     C                   MOVEL     §7TSOZ        WSOSC
058510970418     C                   ELSE
058520970418     C                   MOVEL     §7TSOT        WSOSC
058530970418     C                   END
058540970418    5C     WSOSC         IFEQ      'S'
058550140507     C                   ADD       1             SET
058560140507     c* non segnalo se pgm richiamato in modalità "cieca"
058570140507     c                   if        d48ffr<>'E'
058580941215     C                   SETON                                        472890
058590941214     C                   MOVEL     MSG(58)       VIDMSG
058600941214     C                   GOTO      ENDCT2
058610140507     c                   endif
058620970418    5C                   ENDIF
058630941214    4C                   ENDIF
058640970418   4AC                   ENDIF
058650941214    3C                   ENDIF
058660941214    2C                   ENDIF
058670941214     C*
058680941027    1C                   ENDIF
058690910429     C*
058700910429     C     ENDCT2        ENDSR
058710911212     C*
058720911209     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE IX -----------------*
058730911209     C     REGIS2        BEGSR
058740130412     c                   clear                   wpda_ndc
058750130412     c                   clear                   wpda_ddc
058760130412     c                   clear                   wpda_dcm
058770160318     c* flag per togliere da distinta spedizioni import dpd "VEDI PACCO"
058780160318     c                   clear                   wtogli
058790950104     C*  GIRO UDATE
058800950104     C                   TIME                    W0140            14 0
058810950104     C* UDATE IN GGMMAAAA
058820950104     C                   MOVE      W0140         WDTGIO            8 0
058830950104     C*
058840950104     C* UDATE IN AAAAMMGG
058850950104     C                   Z-ADD     WDTGIO        G02DAT
058860950104     C                   MOVEL     *BLANK        G02ERR
058870950104     C                   CALL      'XSRDA8'
058880950104     C                   PARM                    WLBDAT
058890950104     C                   MOVEL     G02INV        DATEU             8 0
058900941213     C                   MOVEL     KNMUS         ARBPRU
058910941221     C* PRENDO I VALORI ASSOLUTI
058920941221     C                   MLLZO     1             VIDPKF
058930941221     C                   MLLZO     1             VIDVLF
058940041007     C* SALVO VOL/PKG/FTC/RSD/CAP/FIN VECCHIO IN FNARB
058950950114     C                   MOVEL     ARBVLF        OLDVLF
058960950114     C                   MOVEL     ARBFVF        OLDFVF
058970950114     C                   MOVEL     ARBPKF        OLDPKF
058980041007     C                   MOVEL     ARBvlc        OLDvlc
058990950114     C                   MOVEL     ARBRSD        OLDRSD
059000150206     C                   MOVEL     ARBLOD        OLDLOD
059010950114     C                   MOVEL     ARBCAD        OLDCAD
059020950114     C                   MOVEL     ARBFIN        OLDFIN
059030041007     C                   MOVEL     ARBFFD        OLDFFD
059040070320     c                   movel     VIDFFD        WFFD
059050151230     c* (Se variazione fatta da Arrivo su bolla ancora da partire non devo inviare)
059060151222     c                   if        not *in10 or wfile='A'
059070920120     C* INVIO PER SEDE
059080941025     C                   MOVEL     'FNARBD0T'    SFILE
059090920120     C* ALLOCO
059100911212     C                   EXSR      ALLOC
059110941027     C* ERRORI IN ALLOCAZIONE
059120941212    1C     *IN91         IFEQ      *ON
059130941025     C                   GOTO      ENDRG2
059140941212    1C                   ENDIF
059150070515    c
059160070515     c* Se modificato FERMO DEPOSITO, devo allocare record L di ar4
059170070515     c*  per blocco/sblocco telefonata
059180070515     c                   clear                   wesar4L           1
059190070515    1c                   if        Wffd<>oldFFD
059200070515     c                   eval      TipoEla='C'
059210070515     c                   exsr      GestAR4L
059220070515     c
059230070515    2c                   if        *in91
059240070515     c                   exsr      DLCSED
059250070525     C                   SETON                                        2891
059260070515     C                   GOTO      ENDRG2
059270070515    2c                   endif
059280070515    1c                   endif
059290070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
059300070515     c* iornare direttamente il file
059310070515     c                   clear                   waltrabolla       1
059320151222     C   10KARB          CHAIN     FNBLP01L                           3091
059330151222     C  n10KARB          CHAIN     FNARB01L                           3091
059340070515     c*
059350070515     c                   if        *in91
059360070515     C                   MOVEL     MSG(4)        VIDMSG
059370070515     c*
059380070515     c                   exsr      DLCSED
059390070515     c                   unlock    fiar401l
059400070515     c
059410070525     C                   SETON                                        2891
059420070515     C                   GOTO      ENDRG2
059430070515     c                   else
059440070515     c                   if        *in30
059450070515     c                   eval      waltrabolla='N'
059460070515     c                   else
059470070515     c* Salvo alcuni campi dell'altro file che potrebbero, per tempi
059480070515     c*  di aggiornamento, essere diversi.
059490070515     c                   eval      waltrofvf=arbfvf
059500070515     c                   eval      waltrovlf=arbvlf
059510070515     c                   eval      waltrocpi=arbcpi
059520130411     c* salvo campi di arb che mi servono per invio dati a pda
059530151222     c  n10              eval      wpda_ndc=arbndc
059540151222     c  n10              eval      wpda_ddc=arbddc
059550151222     c  n10              eval      wpda_dcm=arbdcm
059560151222     C   10KARB          CHAIN     FNARB01L                           30
059570151222     C  n10KARB          CHAIN     FNBLP01L                           30
059580070515     c* Alla fine per non sporcarmi i campi del record, richaino il
059590070515     c*  record originale che sto variando
059600151222     c   10              eval      wpda_ndc=arbndc
059610151222     c   10              eval      wpda_ddc=arbddc
059620151222     c   10              eval      wpda_dcm=arbdcm
059630070515     c                   endif
059640070515     c                   endif
059650151222     c                   endif
059660070207     C*
059670070207     C* SE SI TRATTA DI BOLLA FRANCO, SALVO IL CAMPO DELLA PARTITA IVA
059680151222     C*   10
059690151222     C*AN 05              MOVEL     ARBCPI        SAVCPI           16
059700151222     C*   10
059710151222     C*AN 05              MOVEL     *BLANKS       ARBCPI
059720070207     C*
059730151222     C*  N10
059740151222     C*AN 06              MOVEL     ARBCPI        SAVCPI           16
059750151222     C*  N10
059760151222     C*AN 06              MOVEL     *BLANKS       ARBCPI
059770151222     c                   if        (wfile='A' and *in05) or
059780151222     c                             (wfile='P' and *in06)
059790151222     C                   MOVEL     ARBCPI        SAVCPI           16
059800151222     C                   MOVEL     *BLANKS       ARBCPI
059810151222     c                   endif
059820070207     C*
059830061116     c* Se variazione di destinatario chaino anche fiar4 record Q se
059840061116     c*  mi serve
059850141215     c                   if        wnoindir=' '
059860061116     c                   if        *in01 and not *in05
059870061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
059880061116     c                             (vidcdf<>*blanks and war4Q<>' ')
059890061116     C                   MOVEL     'Q'           WTRC
059900061116     C     KARB2         CHAIN     FiAR4000                           3091
059910061116     c*
059920061116     c                   if        *in91
059930061116     C                   MOVEL     MSG(4)        VIDMSG
059940070515     c
059950151223     c                   if        not *in10 or wfile='A'
059960070515     c                   exsr      DLCSED
059970151223     c   10              unlock    fnblp01l
059980151223     c  N10              unlock    fnarb01l
059990151223     c                   endif
060000070525     C                   SETON                                        2891
060010061116     C                   GOTO      ENDRG2
060020061116     c                   endif
060030061116     c                   endif
060040061116     c                   endif
060050141215     c                   endif
060060061116     c
060070061116     c* Verifico se prima c'era
060080911212     C*
060090911209     C                   Z-ADD     DATEU         ARBDTV
060100911209     C                   TIME                    ARBORV
060110911209     C                   MOVEL     VIDCVR        ARBCVB
060120911209     C*
060130060130     C* SE DA STORICIZZARE
060140060130     C**   *IN10         IFEQ      *ON
060150060130    1C**   D66FSA        ANDEQ     'S'
060160060130     C**   *IN10         OREQ      *OFF
060170060130    1C**   D66FSP        ANDEQ     'S'
060180060130    1C     D66FSA        ifeq      'S'
060190941212     C                   MOVEL     SAVRD2        ARBRD2
060200950114     C* SE VENGO DA VARIAZIONE MITTENTE, IL VOLUME LO STORICIZZO CON I
060210950114     C* CAMPI CHE HO SALVATO
060220950114     C     *IN24         IFEQ      *ON
060230950114     C                   MOVEL     MITFVF        ARBFVF
060240950114     C                   MOVEL     MITVLF        ARBVLF
060250950114     C                   MOVEL     MITFVF        ARBFVB
060260950114     C                   MOVEL     MITVLF        ARBVLB
060270950114    1C                   ENDIF
060280011108      *
060290011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
060300011108      * è in partenza 'P' o in arrivo 'A'
060310011108     C     *IN10         IFEQ      *ON
060320160119     c                   if        wfile='P'
060330160119     C                   MOVE      'a'           ARBPRU
060340160119     c                   else
060350011108     C                   MOVE      'A'           ARBPRU
060360160119     c                   endif
060370011108     C                   ENDIF
060380011108     C     *IN10         IFEQ      *OFF
060390011108     C                   MOVE      'P'           ARBPRU
060400011108     C                   ENDIF
060410011108      *
060420941014     C                   WRITE     FNARBD00
060430130927     c
060440130927     c* Memorizzo anche referente e telefono se si tratta di varazione "I0"
060450131009     c                   if        *in01 and
060460131009     c                                  (vidref<>par5ref or vidteld<>par5teld)
060470131204     c                   clear                   darbn_ref
060480130927     c                   eval      §arbnref = par5ref
060490130927     c                   eval      §arbnteld= par5teld
060500131204     c                   eval      arbuni70 = darbn_ref
060510130930     c                   eval      arbtrd='REF'
060520130927     c                   write     FNARBN00
060530130927     c                   endif
060540950114     C*
060550950114     C     *IN24         IFEQ      *ON
060560950114     C                   MOVEL     OLDFVF        ARBFVF
060570950114     C                   MOVEL     OLDVLF        ARBVLF
060580950114     C                   MOVEL     OLDFVF        ARBFVB
060590950114     C                   MOVEL     OLDVLF        ARBVLB
060600950114    1C                   ENDIF
060610950114    1C                   ENDIF
060620060301     c* storicizzazione motivazione
060630060301     c     d66mtv        ifeq      'S'
060640060301     c                   exsr      sto_mtv
060650060301     c                   endif
060660911209     C*
060670941025    1C     *IN01         IFEQ      *ON
060680040224
060690040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
060700040224      * - Fermo deposito
060710040224      * devo memorizzare i dati della bolla prima della modifica
060720070320if  3c   10              If        wffd <> oldFfd
060730041022     c                   ExSr      RegAr5AR
060740040224    3c                   EndIf
060750941025     C*
060760941025     C* SE IMMESSA ANCHE LA 2 RAGIONE SOCIALE
060770141212     c                   if        wnoindir=' '
060780060223     c                   clear                   d19ftr
060790160115     c                   if        *in10 and wfile='P'
060800160115     c                   eval      d19ftr='T'
060810160115     c                   endif
060820941025     C                   MOVEL     ARBAAS        D19AAS
060830941025     C                   MOVEL     ARBLNP        D19LNP
060840941025     C                   MOVEL     ARBNRS        D19NRS
060850941025     C                   MOVEL     ARBNSP        D19NSP
060860941025     C                   MOVEL     'D'           D19TRC
060870970520     C                   MOVE      VIDRSD        D19NT1
060880941025     C                   CLEAR                   D19NT2
060890051114     c
060900941025     C                   CALL      'FNLV19R'
060910030612     C                   PARM                    Fnlv19DS
060920941025     C*
060930941025     C* ERRORE: RECORD NON VINCOLABILE
060940941212    2C     D19ERR        IFEQ      '2'
060950941025     C                   MOVEL     MSG(4)        VIDMSG
060960151223     c                   if        not *in10 or wfile='A'
060970070515     c                   exsr      dlcsed
060980070515     c   10              unlock    fnblp01l
060990070515     c  N10              unlock    fnarb01l
061000070515     c                   unlock    fiar401l
061010151230     c                   endif
061020151223     C                   SETON                                        2891
061030941025     C                   GOTO      ENDRG2
061040941212    2C                   ENDIF
061050141212     c                   endif
061060041006    c
061070950626     C*
061080950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
061090950626     C*     NELLA P.IVA
061100950626     C     VIDISD        IFEQ      '$$'
061110950626     C     VIDCPD        ANDEQ     *BLANKS
061120950626     C                   MOVEL     'PRIVATO'     VIDCPD
061130950626     C                   ENDIF
061140941025     C*
061150141212     c                   if        wnoindir=' '
061160941027     C                   MOVEL     VIDRSD        ARBRSD
061170970520     C                   MOVE      D19NT1        ARBRD2
061180911209     C                   MOVEL     VIDIND        ARBIND
061190911209     C                   MOVEL     VIDLOD        ARBLOD
061200911209     C                   MOVEL     VIDCAD        ARBCAD
061210060705     c* se spedizione export DPD diretta in Portogallo devo aggiungere al
061220060705     c* cap immesso tanti 0 a dx quanti ne servono per arrivare ad una
061230060705     c* lunghezza di cap di 7 byte
061240060705     c                   if        lnantw='DPD' and vidnzd = 'P  '
061250060705     c     ' '           checkr(e) arbcad:7      NN                1 0
061260060705     c                   if        %found
061270060705     c                   eval      %subst(arbcad:nn+1:7-nn)=*all'0'
061280060705     c                   endif
061290060705     c                   endif
061300060705     c*
061310911209     C                   MOVEL     VIDPRD        ARBPRD
061320941220     C                   MOVEL     VIDPID        ARBCPI
061330941220     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
061340941220     C*  IL CODICE DELLA P.IVA
061350941220     C     VIDCPD        IFNE      *BLANKS
061360941220     C     VIDISD        ANDEQ     *BLANKS
061370941220     C                   CLEAR                   ARBCPI
061380941220     C                   MOVEL     VIDCPD        ARBCPI
061390941220     C                   ENDIF
061400041027     c                   eval      newcpi=arbcpi
061410041027     c
061420060222     C* SE ESISTE RECORD P.IVA IN FIAR4 LO AGGIORNO
061430950413     C* SOLO SE P.IVA SPROTETTA
061440950413     C     *IN05         IFEQ      *OFF
061450950413     C     *IN72         ANDEQ     *OFF
061460950413     C                   MOVEL     'P'           WTRC
061470060222     C     KARB2         CHAIN     FIAR401L                           30
061480060222     C     *IN30         IFEQ      *OFF
061490060223     c                   move      ar4not        wcpifile
061500060223     c                   if        wcpifile <> arbcpi
061510060223     c                   movel     'T'           ar4ftr
061520060223     c                   z-add     dateu         ar4dtr
061530060223     c                   z-add     dateu         ar4duv
061540060222     C                   MOVE      ARBCPI        AR4NOT
061550060222     C                   UPDATE    FIAR4000
061560060223     c                   else
061570060223     c                   unlock    fiar401l
061580060223     c                   endif
061590060222     C                   END
061600950413     C                   END
061610141215     c                   endif
061620941212     C*
061630931207    2C     VIDFFD        IFEQ      'SI'
061640950222     C                   MOVEL     'C'           ARBFIN
061650930528     C                   MOVEL     'S'           ARBFFD
061660931207   X2C                   ELSE
061670930528     C                   MOVEL     ' '           ARBFFD
061680141212     c                   if        wnoindir=' '
061690930528     C                   MOVEL     VIDFIN        ARBFIN
061700141212     c                   endif
061710931207    2C                   END
061720160318     c* Se spedizione Import dpd richiamo routine per aggiornamenti per
061730160318     c* spedizione "VEDI PACCO"
061740160318     c* (Aggiornamento del flag traduttore su fiar4 rek "I", pulizia del
061750160318     c* codice giro + memorizzaizone di togliere sped da distinta)
061760160318     c                   if        lnpntw='DPD'
061770160318     c                   exsr      sr_VediPDpd
061780160318     c                   endif
061790110905     c* se attiva procedura centro storico e c'e' il giro sulla bolla --> ricalcolo se
061800110905     c*  non è stato forzato a video
061810151230     c                   if        wnoindir=' '  and
061820151230     c                             (not *in10 or wfile='A')
061830110906     c                   if        vidffd=*blanks
061840110906     c                   if        (oldfin<>arbfin and arbfin=o95iso)
061850110906     c                             or oldffd<>vidffd
061860110906     c*****                        or oldfin=arbfin
061870110906     c
061880160318     c     karb          chain(n)  fiarg01l
061890160318     c                   if        %found(fiarg01l) and argcgi<>*blanks
061900160318     c****                         and olvstprop<>'N'
061910110905     c                   clear                   fnlvstds
061920110905     c                   eval      ilvstaas=arbaas
061930110905     c                   eval      ilvstlnp=arblnp
061940110905     c                   eval      ilvstnrs=arbnrs
061950110905     c                   eval      ilvstnsp=arbnsp
061960110905     c                   eval      ilvstmgs=arbmgs
061970110905     c                   eval      ilvstfin=arbfin
061980110905     c                   eval      ilvstcgi=argcgi
061990110906     c                   if        lnacons>0
062000110906     c                   eval      ilvstpoc=lnacons
062010110906     c                   else
062020110906     c                   eval      ilvstpoc=arblna
062030110906     c                   endif
062040110906     c                   if        arbddc>0
062050110906     c                   eval      ilvstdat=arbddc
062060110906     c                   else
062070110906     c                   eval      ilvstdat=dateu
062080110906     c                   endif
062090110905     c                   call      'FNLVSTR'
062100110905     c                   parm                    kpjba
062110110905     c                   parm                    fnlvstds
062120110906     c
062130110906     c* se aggiornato flag inoltro lo memorizzo nella variazione
062140110906     c                   if        OLVSTFINAG='S'
062150110906     c                   eval      arbfin=olvstfin
062160110905     c                   endif
062170110906     c
062180110906     c                   endif
062190110906     c                   endif
062200110906     c                   endif
062210141212     c                   endif
062220110905
062230970808     C* CODICE TASSAZIONE
062240141212     c                   if        wnoindir=' '
062250970808     C                   MOVEL     O95CTS        ARBCTS
062260141212     c                   endif
062270130606
062280031120
062290130930     c                   exsr      AggioREF
062300130930     c
062310931207    1C                   END
062320941025     C**
062330920319     C* RICALCOLO DEL VOLUME
062340941025     C**
062350941025    1C     *IN02         IFEQ      *ON
062360950114     C*
062370950114     C* SE VARIATO MITTENTE PER I PADRONCINI USO QUELLI SALVATI
062380941102     C* PULIZIA DELLA DS PER IMPOSTARE IL VOLUME DA FATTURARE VARIATO
062390030612     C                   CLEAR                   Fnlv20DS
062400941102     C*
062410941025     C* VOLUME REALE
062420041007    2C     VIDVLF        IFNE      oldVLF
062430941102     C                   MOVEL     'R'           D20FVL
062440941102     C                   Z-ADD     VIDVLF        D20VLU
062450930505     C*
062460931207   X2C                   ELSE
062470941025     C*
062480941025     C* VEDO SE DEVO MODIFICARE IL VOLUME DA FATTURARE
062490941214     C                   EXSR      VOLAUT
062500941102    2C                   ENDIF
062510941102     C**
062520941102     C* SE HO MODIFICATO IL VOLUME DA FATTURARE CALL A FNLV20R
062530941102     C*  PER DETERMINARE IL VOLUME BOLLETTATO
062540941102     C**
062550941102    2C     D20VLU        IFGT      0
062560941222     C                   MOVEL     D48TBO        D20TBO
062570941102     C                   MOVEL     'F'           D20TVL
062580041008     C                   MOVEL     arbFVB        D20FVB
062590041008     C                   MOVEL     arbVLB        D20VLB
062600041007     C                   MOVEL     oldFVF        D20FVF
062610041007     C                   MOVEL     oldVLF        D20VLF
062620030612     C                   MOVEL     Fnlv20DS      KPJBU
062630941102     C                   CALL      'FNLV20R'
062640941102     C                   PARM                    KPJBA
062650941102     C*
062660030612     C                   MOVEL     KPJBU         Fnlv20DS
062670941102     C*
062680941102     C                   MOVEL     D20FVB        ARBFVB
062690941102     C                   MOVEL     D20VLB        ARBVLB
062700941102     C                   MOVEL     D20FVF        ARBFVF
062710941102     C                   MOVEL     D20VLF        ARBVLF
062720941102    2C                   ENDIF
062730941214     C*
062740051129     C* SE DEVO SOSTITUIRE COL VOLUME VDL LO FACCIO
062750941214     C     WSOST         IFEQ      'S'
062760970418     C* WSOSC INDICA SE IL TIPO VOLUME DETERMINATO PER LA BOLLA E'
062770051129     C* SOSTITUIBILE DAL VOLUME VDL.
062780970418     C* IN CASO DI VARIAZIONE VOLUME WSOSC SARA' QUELLO DEL TIPO VOLUME
062790970418     C* "R"; IN CASO DI VARIAZIONE DI PESO WSOSC SARA' QUELLO DEL TIPO
062800970418     C* VOLUME DELLA BOLLA
062810041007     C     oldVLC        ANDGT     ARBVLF
062820970418     C     WSOSC         ANDEQ     'S'
062830041007     C                   Z-ADD     oldVLC        ARBVLF
062840970418     C                   MOVE      WFVF          ARBFVF
062850941214     C                   ENDIF
062860920319     C*
062870990628     C  N10              MOVEL     VIDPKF        ARBPKB
062880941102     C                   MOVEL     VIDPKF        ARBPKF
062890041007     c
062900041007    2C     *IN24         IFEQ      *ON
062910041007     C                   MOVEL     MITFVF        OLDFVF
062920041007     C                   MOVEL     MITVLF        OLDVLF
062930041007    2C                   ENDIF
062940941102    1C                   ENDIF
062950941212     C*
062960041007     C* Ora invio solo a sede
062970151223     c                   if        not *in10 or wfile='A'
062980070503     C     D66FFI        ifeq      'S'
062990950227     C*
063000950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
063010950413    2C     §3ABSD        IFEQ      ' '
063020941102     C                   OPEN      FNARBD0T
063030941014     C                   WRITE     FNARBDTT
063040941102     C                   CLOSE     FNARBD0T
063050950227    2C                   END
063060911212     C* DISALLOCO OGGETTO
063070941212     C                   EXSR      DLCSED
063080950208    1C                   END
063090911209     C*
063100950113     C* AGGIORNO FILE TASSAZIONE PADRONCINI
063110950113     C                   EXSR      AGGPDS
063120151223     c                   endif
063130931207     C*
063140941102     C* AGGIORNO
063150151223     C**   10
063160151223     C**AN 05              MOVEL     SAVCPI        ARBCPI
063170151223     C**  N10
063180151223     C**AN 06              MOVEL     SAVCPI        ARBCPI
063190151223     c                   if        (wfile='A' and *in05) or
063200151223     c                             (wfile='P' and *in06)
063210151223     C                   MOVEL     SAVCPI        ARBCPI
063220151223     c                   endif
063230941212     C*
063240151223     c                   if        wfile='A'
063250151223     C                   UPDATE    FNARB000
063260151223     c                   else
063270151223     C                   UPDATE    FNBLP000
063280151223     c                   endif
063290150206     c
063300160318     c* Tolgo  spedizione "VEDI PACCO DPD" da distinta
063310160318     c                   if        wtogli='1'
063320160318     c                   exsr      sr_TogliVDpd
063330160318     c                   endif
063340150206     c* memorizzo la località normalizzata
063350150206     c                   if        vidlod<>normlod OR WAR4X='E'
063360150206     C
063370150211     c                   eval      d19ftr='T'
063380150206     c                   eval      d19agg='E'
063390150203     C                   MOVEL     ARBAAS        D19AAS
063400150203     C                   MOVEL     ARBLNP        D19LNP
063410150203     C                   MOVEL     ARBNRS        D19NRS
063420150203     C                   MOVEL     ARBNSP        D19NSP
063430150203     C                   MOVEL     'X'           D19TRC
063440150206     C                   IF        VIDLOD<>NORMLOD
063450150203     C                   MOVE      normlod       D19NT1
063460150206     C                   ELSE
063470150206     C                   CLEAR                   D19NT1
063480150206     C                   ENDIF
063490150206     C                   CLEAR                   D19NT2
063500150203     c
063510150203     C                   CALL      'FNLV19R'
063520150203     C                   PARM                    Fnlv19DS
063530150206     c                   endif
063540150203     c
063550151230     c* Salto le seguenti istruzioni se
063560151223     c* dall'arrivo sono in manutenzione di una bolla in partenza ancora da
063570151223     c* partire
063580151223     c                   if        not *in10 or wfile='A'
063590070320     c
063600070320     c* Se aggiunto   Fermo Deposito, senza consegna richiesta,
063610070320     c*  imposto il blocco. conconsegna richiesta lo tolgo
063620070320     c* Se tolgo      Fermo Deposito, tolgo il relativo blocco
063630070320     c
063640070320     c* Devo richainare e aggiornare (perchè nel frattempo ho letto
063650070320     c*  altri record fi FIAR4
063660151223
063670070320    1c                   if        Wffd<> oldffd
063680070320     c                   eval      tipoEla='C'
063690070320     c                   exsr      GestAR4L
063700070320     c
063710070320     c* Se allocato pazienza (non aggiorno ma vado avanti
063720070320     c*  fermarmi in questo punto non è bello,lascerebbe la
063730070320     c*  transazione a metà)
063740070320    2c                   if        not *in91
063750070320     c
063760070508    3c****               if        arbdcr=0 and wffd='S'
063770070508     c* imposto il flag telefonata in base a quanto indicato a video
063780070508     c                   if        vidtfd='SI'
063790070508     c                   clear                   §b4ltfd
063800070508     c                   else
063810070320     c                   eval      §b4ltfd='S'
063820070508     c                   endif
063830070508     c
063840070508     c****               else
063850070508     c****               clear                   §b4ltfd
063860070508    3c****               endif
063870070320     c                   eval      tipoEla='A'
063880070320     c                   exsr      GestAR4L
063890140212     c* richiamo pgm per eventuale scrittura/cancellazione file spia
063900150603     c* per aggiungere il file spia per la sped, la bolla non deve essere in stati
063910150603     c*  particolari
063920150603     c                   clear                   tnsd99ds
063930150603     c                   clear                   statofd           1
063940150603     c                   if        wffd='S'
063950150603     C                   MOVEL     'L'           D98tla
063960150603     C                   MOVEL     'A'           D98tbo
063970150603     C                   Z-ADD     arbaas        D98aas
063980150603     C                   Z-ADD     arblnp        D98lnp
063990150603     C                   Z-ADD     arbnrs        D98nrs
064000150603     C                   Z-ADD     arbnsp        D98nsp
064010150603     C*
064020150603     C                   call      'TNSD99R'
064030150603     C                   parm                    tnsd99ds
064040150603     c                   if        D98SPCDEE='FFD' or %subst(D98SPCDEE:1:2)='FD'
064050150603     c                   eval      statofd='S'
064060150603     c                   endif
064070150603     c                   endif
064080150603     c
064090150603     c*****              if        wffd=*blanks or vidtfd=*blanks
064100150603     c                   if        wffd=*blanks or (vidtfd=*blanks and
064110150603     c                             statofd='S')
064120140212     c                   clear                   trtcp2ds
064130140213     c                   if        wffd=*blanks
064140140212     c                   eval      t2chiamata='A'
064150140212     c                   endif
064160140212     c                   eval      t2chi= 'F'
064170140212     c                   eval      T2AAS=d48aas
064180140212     c                   eval      T2LNP=d48lnp
064190140212     c                   eval      T2NRS=d48nrs
064200140212     c                   eval      T2NSP=d48nsp
064210140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
064220140213     c                   eval      T2fgs=arblna
064230140212     c                   call      'TRTCP2R'
064240140212     c                   parm                    trtcp2ds
064250140213     c                   endif
064260070320     c
064270070320    2C                   END
064280070320    1C                   END
064290070320     c
064300041007     c* Per l'altro file sistemo eventuale volume da fatturare e P.I
064310041007     c*  e aggiorno
064320041007     c                   if        waltrabolla=' '
064330041007     c   02              if        waltrofvf='T'  or
064340041007     c                             (waltrofvf='Z' and arbfvf<>'T' aND
064350041007     C                              WALTROVLF> arbvlf)
064360041008     c                   movel     waltrofvf     arbfvf
064370041008     c                   movel     waltrovlf     arbvlf
064380041007     c                   endif
064390041007     c* Se 1°bolla franco, tengo la p.i. presente nel record
064400151223     c                   if        (*in10 and *in06) or
064410151223     c                             (not *in10 and *in05)
064420041007     c                   eval      arbcpi=waltrocpi
064430041027     c                   else
064440041027     c                   eval      arbcpi=NEWCPI
064450041007     c                   endif
064460041007     c
064470041007     c* aggiornamenti di campi mirati
064480041007     C   10              except    arbdblp
064490041007     C  N10              except    arbdarb
064500041007     c                   endif
064510151223
064520151223     c                   endif
064530950221     C*
064540950221     C* DISALLOCO  I FILES CHE NON USO
064550051118     C   10              UNLOCK    FiAR901L
064560991001     C                   UNLOCK    FIAR601L
064570061116     c
064580061116     c* Se si tratta di bolla in assegnato con possibilità di scrivere il
064590061116     c*  cod fiscale, richiamo la REGIS11 per la memorizzazione
064600151230     c                   if        wnoindir=' ' and
064610151230     c                             (not *in10 or wfile='A')
064620061116     c                   if        *in01 and not *in05
064630061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
064640061116     c                             vidcdf<>*blanks
064650061116     c                   eval      v11mide='D'
064660061116     c                   eval      v11cdfis=vidcdf
064670071102     c* Imposto anche il campo della partita iva di nuovo
064680071102     c                   eval      v11pid=vidpid
064690071102     c
064700061116     c                   movel     'FI'          vidcvr
064710071026     c* Non devo aggiornare la bolla perchè l'ho già fatto qui per la
064720071026     c*  partita IVA
064730130412     c                   if        v11pid<>*blanks or
064740130412     c                              %subst(v11cdfis:12:1)<> ' '
064750071026     c                   eval      Aggiobol='N'
064760130402     c                   endif
064770061116     c                   exsr      REGIS11
064780061116     c                   endif
064790061116     c                   endif
064800141215     c                   endif
064810041007     c*
064820141212     c                   if        wnoindir='S'
064830141212     c                   eval      wdisag=wdisagdest
064840141212     c                   endif
064850041007     c* Riaggancio la bolla in modifica, per controlli sul
064860041007     c*  destinatario disagiato e consegna richiesta rispetto a FFD
064870151223     c                   if        wfile='A'
064880151223     C     KARB          CHAIN(n)  FNARB01L                           30
064890151223     c                   else
064900151223     C     KARB          CHAIN(n)  FNBLP01L                           30
064910151223     c                   endif
064920070906     c* Se in destinatario non è più A D o S tolgo se non immesso
064930070906     c*  dalla partenza
064940070911     c                   if        wdisagdest<>wdisag and wdisagdest<>' ' and
064950070910    2c                             (arbtc1=wdisagdest or arbtc2=wdisagdest)
064960070906     c                   exsr      ContrTCP
064970070906     c                   endif
064980040224
064990950602     C*
065000020531     C* SE destinatario disagiato devo rifare il giro per le consegne
065010020531     c* particolari
065020041015     C***  wdisag        IFne      *blanks
065030041015     c***  arbtc1        andne     wdisag
065040041015     c***  arbtc2        andne     wdisag
065050060327     c***                If        wdisag <> *blanks and
065060060327     c***                          %subst(arbGva:1:1) <> 'U' or
065070060327     c***                          wdisag='X' and %subst(arbGva:1:1)='U'
065080060327     c* Per Disagiato ---->sempre
065090060327     c* per Supermercato ->solo se non è consegna ad uffici
065100060327     c* per Appuntamento ->solo se non è consegna ad uffici e non è
065110070917     c*                    prevista data consegna "Tassativa" da partenza
065120070917     c                   movel     arbdcr        w008a
065130070907    1c                   if        (wdisag = 'X') OR
065140060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
065150060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
065160070917     c                             (arbdcr=*zeros or arbtcr <> *blanks or
065170070917     c                              (par5mdp='S' and w008a<>par5gdcr)))
065180070907    2c     arbtc1        ifne      wdisag
065190041015     c     arbtc2        andne     wdisag
065200030122     c                   Eval      wI0 = 'S'
065210020531     C                   MOVEL     'CP'          D48CVB
065220070907     C                   MOVEL     'D'           D48TRC
065230070911     C***                MOVEL     'E'           WESEG
065240070911     c***                clear                   W48f12
065250070907     c***                if        *in20
065260050502     c                   eval      §bgtcr=arbtcr
065270050502     c                   eval      §bghcr=arbhcr
065280050502     c                   eval      §bgdcr=arbdcr
065290050502     c                   eval      §bggc1=arbgc1
065300050502     c                   eval      §bggc2=arbgc2
065310050502     c                   eval      §bgtc1=arbtc1
065320050502     c                   eval      §bgtc2=arbtc2
065330070907     c***                endif
065340070910     c
065350070910     c* Se da eliminare anche la consegna particolare imposto
065360070910     c*  insieme la variazione
065370070910     c                   if        eliminatcp='1'
065380070910     c                   clear                   §bgtc1
065390070911     c* ripristino la vecchia consegna partic.
065400070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
065410070911     c                   eval      §bgtc1=aggtcp
065420070911     c                   endif
065430070910     c                   endif
065440070910     c                   if        eliminatcp='2'
065450070910     c                   clear                   §bgtc2
065460070911     c* ripristino la vecchia consegna partic.
065470070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
065480070911     c                   if        §bgtc1=' '
065490070911     c                   eval      §bgtc1=aggtcp
065500070911     c                   else
065510070911     c                   eval      §bgtc1=aggtcp
065520070911     c                   endif
065530070911     c                   endif
065540070910     c                   endif
065550070911     c
065560070907     c* Se modificato destinatario in disagiato o supermercato imposto il
065570070907     c* relativo flag nelle cosegne particolari
065580070907    3c     wdisag        ifne      *blanks
065590070907     c                   select
065600070907     c     §bgtc1        wheneq    *blanks
065610070907     c                   movel     wdisag        §bgtc1
065620070907     c     §bgtc2        wheneq    *blanks
065630070907     c                   movel     wdisag        §bgtc2
065640070907     c                   endsl
065650070907    4c     §bgtc1        ifeq      §bgtc2
065660070907     c                   clear                   §bgtc2
065670070907    4c                   endif
065680070907    3c                   endif
065690070907     c* Memorizzo in skiera
065700070907    3c                   if        kk<=10
065710070910     c* se devo eliminare una consegna particolare, memorizzo nella
065720070910     c* stessa variazione
065730070910     c                   if        eliminatcp=' '
065740070907     c                   add       1             kk
065750070910     c                   endif
065760070911     c* Passo dati in DS
065770070907     c                   movel     dsarbg        kdati(KK)
065780070907     c                   movel     d48cvb        kcvb(KK)
065790070907     c                   movel     d48trc        ktrc(KK)
065800070911     c* videata si vede o meno in base al richiamo prevedente
065810070911     c                   movel     d48ffr        kffr(KK)
065820070911     c* Disabilito F12
065830070911     c                   movel     ' '           kF12(KK)
065840070907    3c                   endif
065850070907     c
065860070907    2c                   endif
065870070910     c
065880070910   x1c                   else
065890070910     c* Se non devo aggiungere delle consegne particolari,
065900070910      *  se c'è la data consegna richiesta controllo se è ancora valida
065910070910      *  solo se ho fatto una modifica in arrivo
065920070914     c* Solo se non devo aggiungere delle consegnee particolari
065930070914     c*  altrimenti ilk controllo lo faccio nella regis7
065940070914     c                   if        aggtcp=*blanks
065950071109     c
065960071109     c* Imposto il campo WGIAC in base alla giacenza
065970071109if  2c                   If        *In04 and wgcfas = 35
065980071109if  3c                   If        wgcded > *Zeros and arbdcr >= wgcded
065990071109     c                   Eval      wgiac = 'S'
066000071109    3c                   EndIf
066010071109if  3c                   If        wgcded = *Zeros and arbDcr >= wgcddm
066020071109     c                   Eval      wgiac = 'S'
066030071109    3c                   EndIf
066040071109    3c                   EndIf
066050071109     c
066060160114    2c                   If        ArbDcr > *Zeros and *In10  and wfile='A'
066070070910     c                             and wCa = *Blanks and wgiac = *Blanks
066080070910     c                   ExSr      ContrDcr
066090070910    2c                   EndIf
066100070914    2c                   EndIf
066110070907    1c                   endif
066120070907     c
066130070907     c                   clear                   eliminatcp
066140070911     c                   clear                   aggtcp
066150130411
066160130411     c* Richiamo routine per invio variazione a pda
066170160330     c                   if        wfile='A' and wtogli=*blanks
066180130411     c                   exsr      sr_pda
066190160112     c                   endif
066200911209     C*
066210911212     C     ENDRG2        ENDSR
066220130930     c
066230130930     c* Aggiorno referente -----------------------------------------------------*
066240130930     c     AggioREF      BEGSR
066250130930     c
066260130930      * Referente e Telefono e magazzino giacenza
066270130930     c                   Eval      kAr5Trd = 'GEN'
066280130930     c     kAr5          Chain     Fiar501l
066290130930      * Aggiorno Fiar5
066300130930if  2c                   If        %Found(Fiar501l)
066310130930     c                   movel     ar5uni        DAr5Gen
066320130930     c                   Eval      §Ar5Ref = VidRef
066330130930     c                   Eval      §Ar5Teld = VidTeld
066340130930     c
066350130930     c* Solo per causale I0 aggiorno anche mag giacenza e f.deposito
066360130930     c                   if        flgcvr = 'I'
066370130930     c                   Eval      §Ar5Zng = VidZng
066380130930     c
066390130930     c                   if        wffd<>oldffd
066400130930     c                   if        wffd='S'
066410130930     c                   eval      §ar5dfd=%editc(dateu:'X')
066420130930     c                   else
066430130930     c                   clear                   §ar5dfd
066440130930     c                   endif
066450130930     c                   endif
066460130930     c
066470130930     c                   endif
066480130930     c
066490130930     c                   Movel(p)  DAr5Gen       Ar5Uni
066500130930      * se devo trasmettere alla sede sfleggo per la trasmissione
066510130930     c                   If        §Ar5Trse = 'S'
066520130930     c                   Clear                   Ar5Ft2
066530130930     c                   EndIf
066540130930      * sfleggo sempre per la trasmissione al cliente
066550130930     c                   Clear                   Ar5Ft3
066560130930     c                   Update    Fiar5000
066570130930      * Scrivo Fiar5
066580130930   x2c                   Else
066590130930      * se immessi i dati
066600130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
066610130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
066620130930      * di manutenzione bolle
066630130930if  3c                   If        VidRef <> *Blanks or VidTeld <> *Blanks or
066640130930     c                             VidZng <> *Blanks
066650130930     c                   Clear                   Fiar5000
066660130930     c                   Eval      Ar5Aas = ArbAas
066670130930     c                   Eval      Ar5Lnp = ArbLnp
066680130930     c                   Eval      Ar5Nrs = ArbNrs
066690130930     c                   Eval      Ar5Nsp = ArbNsp
066700130930     c                   Eval      Ar5Trd = 'GEN'
066710130930     c                   Move      Dateu         Ar5Dac
066720130930     c                   Movel     W0140         Ar5Orc
066730130930     c                   Clear                   Dar5Gen
066740130930     c                   Eval      §Ar5Ref = VidRef
066750130930     c                   Eval      §Ar5Teld = VidTeld
066760130930     c                   Eval      §Ar5Zng = VidZng
066770130930     c                   Movel(p)  DAr5Gen       Ar5Uni
066780130930     c                   Write     Fiar5000
066790130930    3c                   EndIf
066800130930    2c                   EndIf
066810130930     c                   ENDSR
066820911210     C*
066830911210     C*--- CONTROLLI FORMATO3 ----------------------------------------*
066840911210     C     CONTR3        BEGSR
066850941209     C*
066860920319     C* CONTROLLI COMUNI CON CONTR4
066870920319     C                   EXSR      CTRCOM
066880941209     C   90              GOTO      ENDC03
066890920319     C*
066900920319     C* T A S S A Z I O N E
066910941222     C* CONTROLLO L'IMPORTO DA ASSICURARE SE SONO IN ARRIVO SOLO PER
066920941222     C*  ASSEGNATO, IN PARTENZA SEMPRE
066930941222    1C     *IN36         IFEQ      *OFF
066940941222     C     *IN10         OREQ      *OFF
066950911211     C*
066960011205     C*** I M P O R T O  D A  A S S I C U R A R E  ***
066970080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
066980990127     C*
066990990127     C* IMPOSSIBILE INSERIRE IMPORTO DA ASSICURARE SE BOLLA SI C.A.
067000990127     C*  IN LIQ.TRANSATTIVA
067010080701    2C***  VIDIAS        IFGT      0
067020990127     C* VEDO SE C'E' C.A.
067030040809     C**""*KARB          CHAIN     FNDCT02L                           30
067040040809    3C**""**IN30         DOWEQ     *OFF
067050040809    4C**""*DCTATB        IFEQ      ' '
067060990127     C*
067070040809    5C**""*DCTFPR        IFEQ      'T'
067080040809     C**""*              MOVEL     MSG(82)       VIDMSG
067090040809     C**""*              SETON                                        30
067100040809     C**""*              SETON                                        409028
067110040809    5C**""*              ENDIF
067120040809    4C**""*              ENDIF
067130990127     C**
067140040809     C**""*  N30KARB          READE     FNDCT02L                               30
067150040809    3C**""*              ENDDO
067160040809      *
067170040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
067180040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
067190040809      * legate alla spedizione che passo
067200080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
067210080701      ***    NON FACCIAMO PIUI' IL CONTROLLO ****
067220080701     c*                  clear                   fidn12ds
067230080701     c*                  eval      i12tla = 'L'
067240080701     c*                  eval      i12aas = D48AAS
067250080701     c*                  eval      i12lnp = D48LNP
067260080701     c*                  eval      i12nrs = D48NRS
067270080701     c*                  eval      i12nsp = D48NSP
067280080701     c*                  eval      i12fel = 'E'
067290080701     c*                  eval      i12fan = 'E'
067300040809     c*
067310080701     c*                  call      'FIDN12R'
067320080701     c*                  parm                    fidn12ds
067330040809     c*
067340080701     c*                  setoff                                       30
067350040809     c*
067360040809     c* se non ci sono errori
067370080701    3C*                  if        o12err <> ' '
067380080701     c*                  seton                                        30
067390080701     c*                  else
067400040809     c* se numero di CA trovate maggiore di zero
067410080701    4C*                  if        o12nca = 0
067420080701     c*                  seton                                        30
067430080701     c*                  else
067440080701     c*                  z-add     1             jj                2 0
067450080701    5c*                  dow       jj <= o12nca and *in30 = *off
067460080701     c*                  movea     skkey(jj)     dskey
067470040809     c*
067480040809     c* Aggancio il FNDCT01L x reperire se la CA è in "Liquidazione Transattiva"
067490080701     c*    KDCT1_C       chain     FNDCT01L
067500080701    6c*                  if        %found(FNDCT01L)
067510080701    7C*    DCTFPR        IFEQ      'T'
067520080701     C*                  MOVEL     MSG(82)       VIDMSG
067530080701     C*                  SETON                                        30
067540080701     C*                  SETON                                        409028
067550080701    7C*                  ENDIF
067560080701    6c*                  endif
067570040809     c*
067580080701     c*                  add       1             jj
067590080701    5c*                  enddo
067600040809     c*
067610040809     c                   seton                                        30
067620040809     c*
067630080701    4c*                  endif
067640080701    3c*                  endif
067650990127     C**
067660080701     C** 90              GOTO      ENDC03
067670080701    2C***                ENDIF
067680941215     C*
067690941215     C* SE IL CAMPO = '   ' PRENDO LA DIVISA DALLA NAZIONE DEL MITTENT
067700941215     C* LO EVIDENZIO A VIDEO
067710950120     C* SE VARIATO L'IMPORTO ME LO SALVO
067720950120     C     *IN97         IFEQ      *ON
067730950120     C                   MOVEL     '1'           WIN97             1
067740950120     C                   ENDIF
067750950120     C*
067760941215     C*
067770941215     C                   MOVEL     VIDVAS        WVLT
067780941215     C                   Z-ADD     VIDIAS        W0133            13 3
067790941215     C* CONTROLLO LA DIVISA
067800941215     C                   EXSR      CTRVLT
067810941215     C                   MOVEL     WVLT          VIDVAS
067820941215     C* ERRORE
067830941215     C   90              SETON                                        41
067840941215     C   90              GOTO      ENDC03
067850011009      *
067860011009      * Controllo se la divisa immessa è scaduta o no
067870011009     C     ARBDSP        IFGT      §CVDLI
067880011105     C     VIDIAS        ANDGT     0
067890011102     C                   MOVEL     MSG(102)      VIDMSG
067900011009     C                   SETON                                        419028
067910011009     C                   GOTO      ENDC03
067920011009     C                   ENDIF
067930941215     C*
067940941215     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
067950941222    2C     VIDVAS        IFNE      VARVAS
067960950120     C     WIN97         ANDEQ     *BLANKS
067970941222    3C     VIDVAS        IFNE      *BLANKS
067980941219     C     VIDIAS        ORNE      0
067990941215     C                   MOVEL     MSG(28)       VIDMSG
068000941215     C                   SETON                                        409028
068010941215     C                   GOTO      ENDC03
068020941222    3C                   ENDIF
068030941219    2C                   ENDIF
068040941215     C*
068050941215     C                   MOVEL     VIDVAS        VARVAS
068060950120     C                   CLEAR                   WIN97
068070941215     C*
068080011205
068090011205      * pulisco il flag x la forzatura importo da assicurare forzabile
068100011205     c                   if        vidias <> sav1_vidias
068110011205     c                   clear                   forzaias
068120080625     c                   clear                   forzaias2
068130011205     c                   endif
068140011205      *---------------------
068150011205  2b c                   if        vidias <> 0
068160011205     c                   eval      sav1_vidias = vidias
068170011205      * Controllo l'importo da assicurare tramite il trul22r
068180011205     c                   clear                   trul22ds
068190011205     c                   eval      i22tla = 'L'
068200011205     c                   eval      i22cbo = arbcbo
068210011205     c                   eval      i22tsp = arbtsp
068220011205     c                   eval      i22lnp = arblnp
068230011205     c                   eval      i22nzm = arbnzm
068240011205     c                   eval      i22lna = arblna
068250011205     c                   eval      i22nzd = arbnzd
068260011205     c                   movel     vidklp        i22ksc
068270011205     c                   move      vidksc        i22ksc
068280011205     c                   eval      i22ctr = vidctr
068290011205     c                   eval      i22imp = vidias
068300011205     c                   eval      i22div = vidvas
068310011205     c                   eval      i22ute = knmus
068320011205     c                   eval      i22pgm = nompgm
068330011205     c                   call      'TRUL22R'
068340011205     c                   parm                    trul22ds
068350011205      * se ho tutti i flag impostati non posso immettere l'importo
068360011205      * da assicurare
068370011205  3b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
068380011205     c                             and o22fx2 <> *blanks
068390011205     c                   eval      vidmsg = msg(79)
068400011205     c                   eval      *in40 = *on
068410011205     c                   eval      *in90 = *on
068420011205     c                   eval      *in28 = *on
068430011206     c                   goto      endc03
068440011205  3e c                   endif
068450011205      * controllo che non sia minore del limite previsto
068460011205  3b c                   if        o22fmn <> *blanks
068470011205     c                   eval      vidmsg = msg(87)
068480011205     c                   eval      *in40 = *on
068490011205     c                   eval      *in90 = *on
068500011205     c                   eval      *in28 = *on
068510011206     c                   goto      endc03
068520011205  3e c                   endif
068530011205      * controllo che non sia maggiore del limite previsto
068540011205      * non forzabile
068550011205  3b c                   if        o22fx2 <> *blanks
068560011205     c                   eval      vidmsg = msg(39)
068570011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
068580011205     c                   eval      *in40 = *on
068590011205     c                   eval      *in90 = *on
068600011205     c                   eval      *in28 = *on
068610011206     c                   goto      endc03
068620011205  3e c                   endif
068630011205      * controllo che non sia maggiore del limite previsto
068640011205      * forzabile
068650011205  3b c                   if        o22fx1 <> *blanks and forzaias = *blanks
068660011205     c                   eval      forzaias = '1'
068670011205     c                   eval      vidmsg = msg(80)
068680011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
068690011205     c                   eval      *in40 = *on
068700011205     c                   eval      *in90 = *on
068710011205     c                   eval      *in28 = *on
068720011206     c                   goto      endc03
068730011205  3e c                   endif
068740011205  2e c                   endif
068750011205
068760941209     C*
068770941209     C* SE HA TARIFFE, CONTROLLO
068780941209    2C     WTKS          IFEQ      'S'
068790941209     C     KTAM4         CHAIN     TNTAM000                           30
068800941209    3C     *IN30         IFEQ      *OFF
068810991028     C                   MOVEL     TAMFLO        DSTA01
068820991028     C                   MOVEL     'CV'          COD
068830991028     C                   MOVEL(P)  §TADIV        KEY
068840991028     C                   EXSR      CTABEL
068850991028     C                   MOVEL     TBLUNI        DSCV
068860011010     C                   MOVEL     §CVDEC        COMDEC
068870941209     C*
068880941215     C* PULISCO CAMPI D FORZATURA
068890011010    4C     W0133         IFNE      VARIAS
068900941209     C                   CLEAR                   DIE
068910941215     C                   CLEAR                   CIN
068920941215    4C                   END
068930020715
068940020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
068950020715     c     §3atb1        Lookup    tbl                                    30
068960020715
068970020715   4Ac                   If        *in30 = *On
068980941209     C*
068990941209     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
069000941209     C*   ED E' ESPRESSO IL VALORE MERCE NELLA BOLLA
069010990818     C     KTPT          CHAIN     TITPT000                           30
069020950810    4C     *IN30         IFEQ      *ON
069030950810     C     TPTATB        ORNE      ' '
069040941222     C*
069050950810     C     *IN10         OREQ      *ON
069060941209     C     TPTAPL        ANDNE     'P'
069070941222     C*
069080941222     C     *IN10         OREQ      *OFF
069090941222     C     TPTAPL        ANDNE     'A'
069100941215     C*
069110011010     C                   Z-ADD     W0133         VARIAS
069120941215     C*
069130941215     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
069140941215     C*                                         IMPORTO DA TARIFFA
069150941215    5C     TAMFIS        IFEQ      '1'
069160941215     C     CIN           ANDNE     1
069170011010     C     W0133         ANDEQ     0
069180941215     C                   MOVEL     MSG(40)       VIDMSG
069190941215     C                   SETON                                        409028
069200941215     C                   ADD       1             CIN
069210941215     C                   GOTO      ENDC03
069220941215    5C                   ENDIF
069230941215     C*
069240941215     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
069250941215    5C     TAMFIS        IFEQ      '3'
069260011010     C     W0133         ANDEQ     0
069270941215     C                   MOVEL     MSG(42)       VIDMSG
069280941215     C                   SETON                                        409028
069290941215     C                   GOTO      ENDC03
069300941215    5C                   ENDIF
069310941215     C*
069320941215     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
069330941215    5C     TAMFIS        IFEQ      '2'
069340011010     C     W0133         ANDNE     0
069350941215     C                   MOVEL     MSG(43)       VIDMSG
069360941215     C                   SETON                                        409028
069370941215     C                   GOTO      ENDC03
069380941215    5C                   ENDIF
069390941215     C*
069400941215     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
069410950810    5C  N30TPTATB        IFEQ      ' '
069420011010     C     W0133         ANDGT     0
069430941215     C     DIE           ANDNE     1
069440941215     C     TPTVLM        ANDGT     0
069450941215     C*
069460941209     C* VALORE A COLLO
069470941215    6C     TPTFVM        IFEQ      '1'
069480991028     C     ARBNCL        MULT(H)   TPTVLM        TPTVAL           13 3
069490941215   X6C                   ELSE
069500941215    7C     TPTFVM        IFEQ      '2'
069510991028     C                   Z-ADD     TPTVLM        TPTVAL
069520941215   X7C                   ELSE
069530941212     C     ARBPKF        ADD       0,9           W0060             6 0
069540991028     C     W0060         MULT(H)   TPTVLM        TPTVAL
069550941215    7C                   END
069560941215    6C                   END
069570991028     C*
069580011205     C                   z-add     vidias        vidval           13 3
069590991028    7C     §TADIV        IFNE      VIDVAS
069600030612     C                   CLEAR                   YeurcoDS
069610991028     C                   MOVEL     VIDVAS        YECDVI
069620991028     C                   MOVEL     §TADIV        YECDVO
069630991028     C                   Z-ADD     VIDIAS        YECIMI
069640011010     C                   MOVE      COMDEC        YECDCO
069650991028     C*
069660991028     C                   CALL      'YEURCO'
069670030612     C                   PARM                    YeurcoDS
069680991028     C*
069690991028    8C     YECESI        IFEQ      ' '
069700991028     C                   Z-ADD     YECIMO        VIDVAL
069710991028    8C                   ENDIF
069720991028    7C                   ENDIF
069730941209     C*
069740991028    6C     VIDVAL        IFGT      TPTVAL
069750941209     C                   ADD       1             DIE
069760011205     c                   eval      vidmsg = msg(44)
069770941209     C                   SETON                                        409028
069780941209     C                   GOTO      ENDC03
069790941215    6C                   ENDIF
069800941209     C*
069810941215    5C                   ENDIF
069820941222    4C                   ENDIF
069830020715   4ac                   EndIf
069840941209    3C                   ENDIF
069850941209    2C                   ENDIF
069860030710
069870030710      * calcolo data limite mod. importo da assicurare
069880060201     c**   *iso          Move      ArbDsp        wdataiso
069890060201     c**                 adddur    §vpomia:*d    wdataiso
069900030722      *
069910060201     c**                 do        *hival
069920030722      * verifico se è un giorno lavorativo
069930060201     c**   *iso          move      wdataiso      ds_data
069940060201     C**                 eval      kann = ds_anno
069950060201     C**                 eval      kmes = ds_mese
069960060201     C**   kazcln        chain     azcln01l
069970060201     C**                 if        %found(azcln01l)
069980060201     C**                 if        MAT(ds_giorno) =  'F'  or
069990060201     C**                           POM(ds_giorno) =  'F'
070000030722      * se non va bene aggiungo 1 giorno
070010060201     c**                 adddur    1:*d          wdataiso
070020060201     C**                 iter
070030060201     c**                 else
070040060201     c**                 leave
070050060201     C**                 endif
070060060201     c**                 else
070070060201     c**                 leave
070080060201     C**                 endif
070090030722
070100060201     c**                 enddo
070110030710      * se viene modificato l'importo da assicurare rispetto a quanto
070120030710      * scritto sul file (stessa cosa anche x la divisa)
070130030710      * devo controllare se i gg. limite x la manutenzione sono trascorsi o meno
070140030710      * se sono già trascorsi emetto una window x richiedere la password
070150060201     c**                 If        (VidIas <> ArbIas or
070160060201     c**                           (VidVas <> ArbVas and ArbVas <> *Blanks)) and
070170060201     c**                            wdataoggi > wdataiso
070180060201     c**                           and wokpsw = *Blanks
070190060201     c**                 Exsr      Sr_Gespsw
070200060201     c** 90              GoTo      EndC03
070210060201     c**                 EndIf
070220941209     C***
070230011206     C*** V A L O R E   M E R C E   D I C H I A R A T O ***
070240941209     C* OBBLIGATORIO SE DESTINATARIO ESTERO E C'E' IN TABELLA NAZIONE
070250941209     C* IL FLAG EFTA
070260950120     C* SE HO VARIATO L'IMPORTO LO SALVO
070270950120     C     *IN93         IFEQ      *ON
070280950120     C                   MOVEL     '1'           WIN93             1
070290950120     C                   ENDIF
070300941209     C*
070310941209     C                   MOVEL     VIDVAD        WVLT
070320941209     C                   Z-ADD     VIDVMD        W0133            13 3
070330941209     C* CONTROLLO LA DIVISA
070340941209     C                   EXSR      CTRVLT
070350941209     C                   MOVEL     WVLT          VIDVAD
070360941209     C* ERRORE
070370941209     C   90              SETON                                        44
070380941209     C   90              GOTO      ENDC03
070390011010      *
070400011010      * Controllo se la divisa immessa è scaduta o no
070410011010     C     ARBDSP        IFGT      §CVDLI
070420011105     C     VIDVMD        ANDGT     0
070430011102     C                   MOVEL     MSG(102)      VIDMSG
070440011010     C                   SETON                                        449028
070450011010     C                   GOTO      ENDC03
070460011010     C                   ENDIF
070470941215     C*
070480941215     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE L'IMPORTO
070490941215     C* INDICATORE DI CHANGE 93 ON
070500941215    2C     VARVAD        IFNE      VIDVAD
070510950120     C     WIN93         ANDEQ     *BLANKS
070520941219     C     VIDVAD        IFNE      *BLANKS
070530941219     C     VIDVMD        ORNE      0
070540941215     C                   MOVEL     MSG(28)       VIDMSG
070550941215     C                   SETON                                        439028
070560941215     C                   GOTO      ENDC03
070570941215    2C                   ENDIF
070580941219     C                   ENDIF
070590941215     C*
070600941215     C                   MOVEL     VIDVAD        VARVAD
070610950120     C                   CLEAR                   WIN93
070620011205
070630011205      * pulisco il flag x la forzatura importo da assicurare forzabile
070640011205     c                   if        vidvmd <> sav1_vidvmd
070650011205     c                   clear                   forzavmd
070660011205     c                   endif
070670011205      *---------------------
070680011205     c                   eval      sav1_vidvmd = vidvmd
070690011205      * Controllo il valore merce tramite il trul23r
070700011205     c                   clear                   trul23ds
070710011205     c                   eval      i23tla = 'L'
070720011205     c                   eval      i23cbo = arbcbo
070730011205     c                   eval      i23tsp = arbtsp
070740011205     c                   eval      i23lnp = arblnp
070750011205     c                   eval      i23nzm = arbnzm
070760011205     c                   eval      i23lna = arblna
070770011205     c                   eval      i23nzd = arbnzd
070780011205     c                   movel     vidklp        i23ksc
070790011205     c                   move      vidksc        i23ksc
070800011205     c                   eval      i23ctr = vidctr
070810011206     c                   eval      i23imp = vidvmd
070820011206     c                   eval      i23div = vidvad
070830011205     c                   eval      i23ute = knmus
070840011205     c                   eval      i23pgm = nompgm
070850011205     c                   call      'TRUL23R'
070860011205     c                   parm                    trul23ds
070870011205      * se ho il flag del limite minimo impostato e l'importo è a zero
070880011205      * è obbligatorio
070890011206  2b c                   if        o23fmn <> *blanks and vidvmd = 0
070900011205     c                   eval      vidmsg = msg(45)
070910011205     c                   eval      *in43 = *on
070920011205     c                   eval      *in90 = *on
070930011205     c                   eval      *in28 = *on
070940011205     c                   goto      endc03
070950011206  2e c                   endif
070960011205      * controllo che non sia minore del limite previsto
070970011206  2b c                   if        o23fmn <> *blanks
070980011205     c                   eval      vidmsg = msg(104)
070990011210     c                   eval      %subst (vidmsg:37:14) = (%editc(o23lmn: '4'))
071000011205     c                   eval      *in43 = *on
071010011205     c                   eval      *in90 = *on
071020011205     c                   eval      *in28 = *on
071030011205     c                   goto      endc03
071040011206  2e c                   endif
071050011205      * controllo che non sia maggiore del limite previsto
071060011205      * non forzabile
071070011206  2b c                   if        o23fx2 <> *blanks
071080011205     c                   eval      vidmsg = msg(105)
071090011210     c                   eval      %subst (vidmsg:53:14) = (%editc(o23lx2: '4'))
071100011205     c                   eval      *in43 = *on
071110011205     c                   eval      *in90 = *on
071120011205     c                   eval      *in28 = *on
071130011205     c                   goto      endc03
071140011206  2e c                   endif
071150011205      * controllo che non sia maggiore del limite previsto
071160011205      * forzabile
071170011206  2b c                   if        o23fx1 <> *blanks and forzavmd = *blanks
071180011205     c                   eval      forzavmd = '1'
071190011205     c                   eval      vidmsg = msg(106)
071200011210     c                   eval      %subst (vidmsg:30:14) = (%editc(o23lx1: '4'))
071210011205     c                   eval      *in43 = *on
071220011205     c                   eval      *in90 = *on
071230011205     c                   eval      *in28 = *on
071240011205     c                   goto      endc03
071250011206  2e c                   endif
071260930507     C*
071270930507    1C                   END
071280950424     C*
071290950426     C*** CONTROLLI PER CLIENTE CON TARIFFA A VALORE CON FLAG = '3' ***
071300950426    1C     WTKS          IFEQ      'S'
071310950424     C                   MOVEL     VIDCTR        W001A             1
071320950426    2C     W001A         IFEQ      '4'
071330950424     C     TAMFVD        ANDEQ     '3'
071340950426     C*
071350950426    3C     VIDKLP        IFNE      VARKLP
071360950426     C     VIDKSC        ORNE      VARKSC
071370950426     C     VIDIAS        ORNE      COMIAS
071380950426     C                   SELECT
071390950426     C* FATTURA IMMEDIATA
071400950426     C* Se modificato il cod.cliente o l'importo da assicurare
071410950426     C* è necessario che la q.tà da fatturare e importi tassazione
071420950426     C* vengano azzerati + F14
071430950426     C     *IN14         WHENEQ    *ON
071440991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
071450991001     C                   MOVEL     '1'           WCKINL            1
071460991001     C                   EXSR      CARWAR
071470950426    4C     VIDQFT        IFNE      *ZEROS
071480950426     C     VIDIMV        ORNE      *ZEROS
071490950426     C     VIDPOR        ORNE      *ZEROS
071500991001     C     WINL          OREQ      '1'
071510950426     C     *INKN         OREQ      *OFF
071520950426     C                   SETON                                        429028
071530950426     C                   MOVEL     MSG(71)       VIDMSG
071540950426     C                   GOTO      ENDC03
071550950426    4C                   END
071560950426     C* SEGUE FATTURA
071570950426     C* Se modificato il cod.cliente o l'importo da assicurare
071580950426     C* è necessario che la q.tà da fatturare e tot imponibile
071590950426     C* vengano azzerati
071600950426     C     *IN13         WHENEQ    *ON
071610950426    4C     VIDQFT        IFNE      *ZEROS
071620950426     C     VIDIMV        ORNE      *ZEROS
071630950426     C                   SETON                                        429028
071640950426     C                   MOVEL     MSG(70)       VIDMSG
071650950426     C                   GOTO      ENDC03
071660950426    4C                   END
071670950426     C                   ENDSL
071680950426     C                   MOVE      VIDKLP        VARKLP
071690950426     C                   MOVE      VIDKSC        VARKSC
071700950426     C                   MOVE      VIDIAS        COMIAS
071710950426     C                   MOVE      *ZEROS        VARQFT
071720950426    3C                   END
071730950426     C*
071740950426     C*** QUANTITA' DA FATTURARE ***
071750950426     C*
071760950426     C* Errore se modificata e diversa da 0
071770950426    3C     VIDQFT        IFNE      VARQFT
071780950426    4C     VIDQFT        IFNE      *ZEROS
071790950426     C                   MOVEL     MSG(68)       VIDMSG
071800950424     C                   SETON                                        429028
071810950424     C                   GOTO      ENDC03
071820950426    4C                   END
071830950426     C* Se azzerata q.tà da fatturare gli
071840950426     C* importi della tassazione devono essere = 0
071850950426     C                   SELECT
071860950426    4C     *IN14         WHENEQ    *ON
071870991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
071880991001     C                   MOVEL     '1'           WCKINL            1
071890991001     C                   EXSR      CARWAR
071900950426    5C     VIDIMV        IFNE      *ZEROS
071910950426     C     VIDPOR        ORNE      *ZEROS
071920991001     C     WINL          OREQ      '1'
071930950426     C     *INKN         OREQ      *OFF
071940950426     C                   SETON                                        499028
071950950426     C                   MOVEL     MSG(69)       VIDMSG
071960950426     C                   GOTO      ENDC03
071970950426    5C                   END
071980950426    4C     *IN13         WHENEQ    *ON
071990950426    5C     VIDIMV        IFNE      *ZEROS
072000950426     C                   SETON                                        499028
072010950426     C                   MOVEL     MSG(72)       VIDMSG
072020950426     C                   GOTO      ENDC03
072030950426    5C                   END
072040950426    4C                   ENDSL
072050950424     C                   MOVE      VIDQFT        VARQFT
072060950426    3C                   END
072070950424     C*
072080950426    2C                   END
072090950426    1C                   END
072100950424     C*
072110050601     c* Controlli su aggiunte/aumenti di importo da assicurare
072120080625    1c                   If        (VidIas <> ArbIas and vidias > 0 or
072130050804     c                             (VidVas <> ArbVas and vidVas <> *Blanks
072140050804     c                             and Vidias>0))
072150050601     c* verifico se cliente con mandato
072160050601     c                   exsr      tassar
072170050601     c* cliente senza mandato: si può aggiungere/aumentare l'importo solo
072180050601     c* se inviata e-mail
072190050601     c* idem per cliente con mandato se il valore è oltre mandato
072200080625    2c                   if        task88 = 'S'
072210050601     c* vedo se inviata e-mail
072220050609     c                   movel     'c'           kta4trc
072230050601     c                   clear                   dta4c
072240050601     c     kta4          chain     tita430c
072250080625    3c                   if        %found(tita430c)
072260050601     c                   movel     ta4not        dta4c
072270080625    3c                   endif
072280080625    3c                   if        §ta4mail <> 'S'
072290080625    4c                   if        §utemtf=' '
072300050601     c                   seton                                        409028
072310050601     c                   eval      vidmsg=msg(135)
072320050601     c                   goto      endc03
072330080625    4c                   endif
072340080625    4c                   if        (§utemtf='I' or §utemtf='E') and
072350080625     c                             forzaias2=' '
072360080625     c                   eval      forzaias2='1'
072370080625     c                   seton                                        409028
072380080625     c                   eval      vidmsg=msg(146)
072390080625     c                   goto      endc03
072400080625    4c                   endif
072410080625    3c                   endif
072420080625    2c                   endif
072430080625    1c                   endif
072440941221     C*
072450941221     C* F14 - TASSAZIONE
072460991011     C     *INKN         IFEQ      *ON
072470991029     C* Fattura immediata SE NON CONTABILIZZATA
072480991011     C     *IN14         OREQ      *ON
072490991029     C     *IN29         ANDEQ     *OFF
072500991029     c* Prepagati NON CONTABILIZZATA
072510991011     C     *IN10         OREQ      *OFF
072520991011     C     VIDNFT        ANDGT     *ZEROS
072530991029     C     *IN29         ANDEQ     *OFF
072540980513     C                   MOVEL     '1'           WBOL              1
072550011018      * Se è una bolla di reso con varia 'N' 88888888
072560011017      * tasso la varia 'N'
072570011018     C     FLGVRN        IFEQ      '1'
072580011017      * leggo il sbfl delle varie
072590011017     C                   DO        20            NRR
072600011017     C     NRR           CHAIN     LR48S03                            32
072610011017     C     V3CSV1        IFEQ      'N'
072620011017     C     V3CVA1        ANDEQ     88888888
072630011017     C                   EXSR      TASSAN
072640011017     C  N32              UPDATE    LR48S03
072650011017     C                   ENDIF
072660011017     C                   ENDDO
072670011017     C                   ENDIF
072680011017      *
072690151020     c                   if        vidimv<>PesVolImv
072700151016     c                   clear                   PesVolPkcn
072710151016     c                   clear                   PesVolSpc
072720151016     c                   clear                   PesVolImv
072730151020     c                   endif
072740151020     c***                clear                   Tassatoimv
072750941221     C                   EXSR      TASSAZ
072760151019     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
072770151020     c***                if        vidimv>0 and vidimv<>PesVolImv
072780151020     c***                eval      PesVolPkcn=taspkcn
072790151020     c***                eval      PesVolSpc=tasspc
072800151020     c***                eval      PesVolIMV=vidimv
072810151020     c***                endif
072820990702     C* SE SI TRATTA DI UN PREPAGATO DEVO ANCHE VISUALIZZARE IL TOT
072830990702     C*  IMPONIBILE ARROTONDATO
072840990702     C     *IN23         IFEQ      *ON
072850151020     c****               eval      TassatoImv=vidimv
072860990702     C                   Z-ADD     AR6DFT        ARBDFT
072870990702     C                   EXSR      TOTFAT
072880990702     C                   Z-ADD     D04IFT        VIDIFT
072890990702     C                   Z-ADD     D04IMV        VIDIMV
072900990702     C                   Z-ADD     D04POR        VIDPOR
072910991006     C                   Z-ADD     D04POR        VARPOR
072920990702     C                   Z-ADD     D04IMV        SAVIMV
072930990702     C                   ENDIF
072940151020     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
072950151020     c                   if        vidimv>0 and vidimv<>PesVolImv
072960151020     c                   eval      PesVolPkcn=taspkcn
072970151020     c                   eval      PesVolSpc=tasspc
072980151020     c                   eval      PesVolIMV=vidimv
072990151020     c                   endif
073000131016
073010131016     c                   if        *inkn
073020131016     c                   eval      winf14='1'
073030131016     c                   endif
073040991011     C   KN
073050991011     COR 90              GOTO      ENDC03
073060990702    1C                   END
073070991006     C* Se divisa sprotetta significa che manca tariffa: in questo caso
073080991006     C* controllo che la divisa inserita sia = alla divisa della moneta
073090991006     C*  di conto SE CLIENTE 8888 o 9999
073100991006     C*
073110991006     C* Fattura immediata
073120991006     C     *IN14         IFEQ      *ON
073130991006     C* Prepagati
073140991006     C     *IN10         OREQ      *OFF
073150991006     C     VIDNFT        ANDGT     *ZEROS
073160991006     C* Divisa sprotetta
073170991006     C     *IN07         IFEQ      *OFF
073180991006     C* Divisa diversa dalla divisa della moneta di conto
073190991006     C     VIDDIV        ANDNE     §GEDCN
073200991006     C* Cliente 8888 o 9999
073210991006     C     VIDKSC        IFEQ      8888
073220991006     C     VIDKSC        OREQ      9999
073230991006     C                   SETON                                        459028
073240991006     C                   MOVEA     MSG(90)       K78
073250991006     C                   MOVEA     §GEDCN        K78(71)
073260991006     C                   MOVEA     K78           VIDMSG
073270991006     C                   GOTO      ENDC03
073280991006     C                   ENDIF
073290991006     C                   ENDIF
073300991006     C                   ENDIF
073310941102     C**
073320991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
073330991124     C* TASSAZIONE
073340991124    1C     *IN07         IFEQ      *OFF
073350991124     C     VIDDIV        ANDNE     WDIV
073360991124     C     VIDDIV        ANDEQ     *BLANKS
073370991124    2C     VIDPOR        IFGT      *ZEROS
073380991125     C                   SETON                                        459028
073390991124     C                   MOVEL     MSG(94)       VIDMSG
073400991124     C                   GOTO      ENDC03
073410991124   X2C                   ELSE
073420991124    3C                   DO        20            NRR
073430991124     C     NRR           CHAIN     LR48S03                            30
073440991124    4C     V3CVA1        IFGT      *ZEROS
073450991125     C                   SETON                                        459028
073460991124     C                   MOVEL     MSG(94)       VIDMSG
073470991124     C                   GOTO      ENDC03
073480991124    4C                   ENDIF
073490991124    3C                   ENDDO
073500991124    2C                   ENDIF
073510991124    1C                   ENDIF
073520991005     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
073530991012     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
073540991012     C* CONTROLLO DELLA DIVISA
073550991013    0C     *IN07         IFEQ      *OFF
073560991013     C     VIDDIV        ANDNE     WDIV
073570991026     C     WDIV          ANDNE     *BLANKS
073580100120     c
073590030612     C                   CLEAR                   Fnlv16DS
073600991012     C                   CLEAR                   DTASV
073610100120     C                   CLEAR                   dlv1601
073620100120
073630991012     C                   MOVEL     'D'           D17FIM
073640991012     C                   MOVE      VIDDIV        D17DIV
073650991012     C     VIDDFT        IFGT      *ZEROS
073660991012     C                   CLEAR                   WLBDAT
073670991012     C                   Z-ADD     VIDDFT        G02DAT
073680991012     C                   MOVEL     *BLANK        G02ERR
073690991012     C                   CALL      'XSRDA8'
073700991012     C                   PARM                    WLBDAT
073710991012     C                   Z-ADD     G02INV        D17DSP
073720991012     C                   ELSE
073730991012     C                   Z-ADD     ARBDSP        D17DSP
073740991012     C                   END
073750991012     C                   CALL      'FNLV16R'
073760030612     C                   PARM                    Fnlv16DS
073770991012     C                   PARM                    DTASV
073780991014     C                   MOVEL     D17DIV        VIDDIV
073790991012   X0C                   ELSE
073800030612     C                   CLEAR                   Fnlv16DS
073810991005     C                   CLEAR                   DTASV
073820941222     C                   MOVEL     D48TBO        D17TBO
073830991006     C   23              MOVEL     'S'           D17PRP
073840941209     C                   MOVEL     KSC           D17KSC
073850991012     C     VIDDFT        IFGT      *ZEROS
073860991012     C                   CLEAR                   WLBDAT
073870991012     C                   Z-ADD     VIDDFT        G02DAT
073880991012     C                   MOVEL     *BLANK        G02ERR
073890991012     C                   CALL      'XSRDA8'
073900991012     C                   PARM                    WLBDAT
073910991012     C                   Z-ADD     G02INV        D17DSP
073920991012     C                   ELSE
073930991012     C                   Z-ADD     ARBDSP        D17DSP
073940991012     C                   END
073950941209     C                   MOVEL     ARBLNP        D17LNP
073960941209     C                   MOVEL     ARBLNA        D17LNA
073970941209     C                   MOVEL     §3ATB1        D17TBL
073980941209     C                   MOVEL     §3ARBL        D17RBL
073990020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
074000100120     C                   MOVEL     §3ASVA        §D17sva
074010100120
074020100120     c* Se fattura immediata o prepagato
074030100120     c*                        controllo evetuale esenzione iva
074040100121     c                   if        *in14   or (not *in10 and vidnft>0)
074050100120     c                   Eval      §d17ctriso='S'
074060100120     c                   if        arbcpi<>*blanks
074070100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
074080100120     c* solo numeri
074090100120     c                   select
074100100120     c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
074110100120     c                   eval      §d17iso='IT'
074120100120     c* "PRIVATO" senza iso
074130100120     c                   when      *in30 and posiz<=3
074140100120     c                   eval      §d17iso='$$'
074150100120     c                   other
074160100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
074170100120     c                   ENDSL
074180100120     c                   endif
074190100120     c                   endif
074200100120     c
074210100120     c                   Movel     Dlv1601       D17Flo
074220100120     c
074230941209     C                   Z-ADD     VIDIAS        D17IAS
074240941209     C                   Z-ADD     VIDQFT        D17QFT
074250941209     C                   Z-ADD     VIDPOR        D17POR
074260991005     C                   MOVEL     VIDDIV        D17DIV
074270991006     C                   MOVEL     WDIV          D17DVP
074280941209     C                   Z-ADD     VIDIMV        D17IMV
074290990702    1C     §3AFCA        IFEQ      1
074300941209     C                   MOVEL     'S'           D17FCA
074310990702    1C                   ENDIF
074320990702    1C* SE C'E' NUMERO E DATA FATTURA RIPASSO IL TOTALE IMPONIBILE
074330950114     C     *IN14         IFEQ      *ON
074340991005     C                   MOVEL     'E'           D17FIM
074350950114     C*
074360990702   X1C                   ELSE
074370950114     C*
074380950114     C                   MOVEL     'C'           D17FIM
074390990702    2C     VIDFIV        IFGT      0
074400950114     C     VIDNFT        ANDGT     0
074410950114     C     VIDDFT        ANDGT     0
074420991005     C                   MOVEL     'E'           D17FIM
074430990702    2C                   ENDIF
074440990702    1C                   ENDIF
074450941209     C                   MOVEL     VIDCEI        D17CEI
074460991029     C* SE PROTETTA LA TASSAZIONE, NON PASSO I DATI DELLA FATTURA
074470941209     C                   MOVEL     VIDFIV        D17FIV
074480941220     C                   MOVEL     'S'           D17FFI
074490941209     C                   MOVEL     VIDNFT        D17NFT
074500941209     C                   Z-ADD     VIDDFT        D17DFG
074510941209     C*
074520991012     C* RICHIAMO ROUTINE PER CARICAMENTO VARIE DELLA DTASV
074530991012     C                   EXSR      RDTASV
074540941209     C*
074550991005     C                   CALL      'FNLV16R'
074560030612     C                   PARM                    Fnlv16DS
074570991005     C                   PARM                    DTASV
074580941102     C*
074590991011     C                   MOVEL     D17DIV        VIDDIV
074600941209     C                   MOVEL     D17CEI        VIDCEI
074610991005     C                   MOVEL     O17DEI        DESCEI
074620991005     C                   MOVEL     O17DEI        V3DCEI
074630991006     C     D17FIM        IFEQ      'E'
074640991014     C     O17ERR        ANDEQ     *BLANKS
074650030620      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
074660030620      * SOLO SE LA VARIA ESISTE
074670040123     C**!!!§3ASVA        ORNE      *BLANKS
074680040123     C**!!!WVAR3A        ANDEQ     'S'
074690040123     C**!!!O17ERR        ANDEQ     *BLANKS
074700040123     C**!!!D17FIM        ANDEQ     'C'
074710991005     C                   MOVEL     O17IMV        VIDIMV
074720991006     C                   ENDIF
074730030620      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
074740030620     C     §3ASVA        IFNE      *BLANKS
074750030620     C     WVAR3A        ANDNE     'S'
074760030620     C     D17FIM        ANDEQ     'C'
074770030620     C                   Z-ADD     *ZEROS        VIDIMV
074780030620     C                   ENDIF
074790030620      ****
074800991029     C                   MOVEL     D17DFG        VIDDFT
074810941209     C* VARIE
074820990702    1C                   DO        20            NRR
074830941215     C     NRR           CHAIN     LR48S03                            30
074840941209     C                   MOVEL     VSV(NRR)      V3CSV1
074850941209     C                   MOVEL     VES(NRR)      V3CEV1
074860941215     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
074870991006    2C     O17ERV        IFEQ      NRR
074880941220     C                   SETON                                        5290
074890941215     C                   ELSE
074900941215     C                   SETOFF                                       52
074910990702    2C                   ENDIF
074920941215     C  N30              UPDATE    LR48S03
074930941215     C   30              WRITE     LR48S03
074940990702    1C                   ENDDO
074950991012    0C                   ENDIF
074960941215     C*
074970941215     C                   SETOFF                                       52
074980941102     C*  ERRORE
074990991006    1C     O17ERR        IFNE      *BLANKS
075000991006     C     O17FAS        OREQ      'S'
075010941102     C*
075020991006    2C     O17MSG        IFNE      *BLANKS
075030991006     C                   MOVEL     O17MSG        VIDMSG
075040941209     C                   SETON                                        28
075050941102    2C                   ENDIF
075060990415     C**
075070990415     C* VARIA R  MSG
075080991006    2C     O17ERR        IFEQ      '6'
075090991006     C     O17MSG        ANDEQ     MSG(85)
075100991006     C     O17FAS        OREQ      'S'
075110990416     C*
075120990415     C     COMASS        IFEQ      ' '
075130990415     C* SE MODIFICATO L'IMPORTO D'ASSICURARE E FATTURA GIA' INCASSATA O TASSATA
075140990415    3C     VIDIAS        IFNE      ARBIAS
075150990415     C     VIDVAS        ORNE      ARBVAS
075160990415     C*
075170990415     C* SE GIA' TASSATO FINO ALL'IMPONIBILE E LA DATA FATTURA > 0
075180990415    4C     AR6IMV        IFNE      *ZEROS
075190990415     C                   SETON                                        2882
075200990415     C                   SETOFF                                       90
075210990415     C* ERRORE NON BLOCCANTE:CORREGGERE  VARIA R
075220990419     C                   SELECT
075230990419    5C     ARBIAS        WHENEQ    0
075240990415     C     VIDIAS        ANDGT     0
075250990415     C                   MOVEL     MSG(83)       VIDMSG
075260990419    5C     ARBIAS        WHENGT    0
075270990419     C     VIDIAS        ANDEQ     0
075280990415     C                   MOVEL     MSG(84)       VIDMSG
075290990419     C                   OTHER
075300990419     C                   MOVEL     MSG(86)       VIDMSG
075310990419    5C                   ENDSL
075320990415    4C                   END
075330990415     C                   MOVEL     '1'           COMASS            1
075340990415    3C                   END
075350990415     C                   ELSE
075360990415     C                   SETOFF                                       9028
075370990415     C                   MOVEL     *BLANKS       VIDMSG
075380990415     C                   END
075390990415    2C                   END
075400990415     C* FATTURA
075410941102     C**
075420941102     C* ESENZIONE IVA
075430991006    2C     O17ERR        IFEQ      '1'
075440941209     C                   SETON                                        5190
075450941102    2C                   ENDIF
075460941102     C**
075470941102     C* IMPONIBILE
075480991006     C     O17ERR        IFEQ      '5'
075490941209     C                   SETON                                        5090
075500941102    2C                   ENDIF
075510941222     C* NUMERO FATTURA
075520991006     C     O17ERR        IFEQ      '2'
075530941222     C                   SETON                                        5590
075540941222    2C                   ENDIF
075550941222     C* FILIALE IVA FATTURA
075560991006     C     O17ERR        IFEQ      '3'
075570941222     C                   SETON                                        5490
075580941222    2C                   ENDIF
075590941222     C* DATA FATTURA
075600991006     C     O17ERR        IFEQ      '4'
075610941222     C                   SETON                                        5690
075620941222    2C                   ENDIF
075630991006     C* DIVISA
075640991006     C     O17ERR        IFEQ      '7'
075650991006     C                   SETON                                        4590
075660991006    2C                   ENDIF
075670991012     C* PORTO
075680991012     C     O17ERR        IFEQ      '8'
075690001116     C                   SETON                                        4990
075700991012    2C                   ENDIF
075710941102     C**
075720941220     C                   GOTO      ENDC03
075730941102    1C                   ENDIF
075740040123      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
075750040123      * non posso avere altre varia con importo > 0
075760040123if  1c                   If        §3asva <> *Blanks
075770040123do  2c                   Do        20            nrr
075780040123     c     nrr           Chain     Lr48s03                            30
075790040123     c                   Eval      *In52 = *Off
075800040123if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
075810040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
075820040123     c                   Eval      *In28 = *On
075830040123     c                   Eval      *In52 = *On
075840040123     c                   Eval      *In90 = *On
075850040123     c                   Eval      VidMsg = Msg(127)
075860040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
075870040123    3c                   EndIf
075880040123if  3c                   If        wvar3a = *Blanks  and
075890040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
075900040123     c                   Eval      *In28 = *On
075910040123     c                   Eval      *In52 = *On
075920040123     c                   Eval      *In90 = *On
075930040123     c                   Eval      VidMsg = Msg(127)
075940040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
075950040123    3c                   EndIf
075960040123     c  n30              Update    Lr48s03
075970040127     c   90              Goto      Endc03
075980040123    2c                   EndDo
075990040123    1c                   EndIf
076000991006     C*
076010991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
076020991006    1C  N07VIDDIV        IFNE      WDIV
076030991006     C* CONVERSIONE PORTO
076040991006    2C     VIDPOR        IFEQ      VARPOR
076050991006     C     VIDPOR        ANDGT     *ZEROS
076060991006     C     WDIV          ANDNE     *BLANKS
076070030612     C                   CLEAR                   YeurcoDS
076080991006     C                   MOVEL     WDIV          YECDVI
076090991006     C                   Z-ADD     VIDPOR        YECIMI
076100991006     C                   MOVEL     VIDDIV        YECDVO
076110991006     C     O17FDC        IFEQ      'S'
076120011114     C                   MOVE      '3'           YECDCO
076130991006     C                   ENDIF
076140991006     C                   CALL      'YEURCO'
076150030612     C                   PARM                    YeurcoDS
076160991006     C                   Z-ADD     YECIMO        VIDPOR
076170991012     C                   SETON                                        49
076180991006    2C                   END
076190991006     C* CONVERSIONE VARIE
076200991006    2C                   DO        20            NRR
076210991006     C     NRR           CHAIN     LR48S03                            30
076220991006    3C     V3CVA1        IFEQ      VARVA1
076230991006     C     V3CVA1        ANDGT     *ZEROS
076240991006     C     WDIV          ANDNE     *BLANKS
076250011019      *
076260011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
076270011019      * non devo convertire la varia 'N' 88888888
076280011019     C     VIDIMV        IFEQ      *ZEROS
076290011019     C     FLGVRN        ANDEQ     '1'
076300011019     C     V3CSV1        ANDEQ     'N'
076310011019     C     V3CVA1        ANDEQ     88888888
076320011019     C  N30              UPDATE    LR48S03
076330011019     C                   ITER
076340011019     C                   ENDIF
076350030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
076360030624      * non devo convertire la varia '&' 88888888
076370030624     C     VIDIMV        IFEQ      *ZEROS
076380030624     C     V3CSV1        ANDEQ     '&'
076390030624     C     V3CVA1        ANDEQ     88888888
076400030624     C  N30              UPDATE    LR48S03
076410030624     C                   ITER
076420030624     C                   ENDIF
076430011019      *
076440030612     C                   CLEAR                   YeurcoDS
076450991006     C                   MOVEL     WDIV          YECDVI
076460991006     C                   Z-ADD     V3CVA1        YECIMI
076470991006     C                   MOVEL     VIDDIV        YECDVO
076480991006     C     O17FDC        IFEQ      'S'
076490011114     C                   MOVE      '3'           YECDCO
076500991006     C                   ENDIF
076510991006     C                   CALL      'YEURCO'
076520030612     C                   PARM                    YeurcoDS
076530991006     C                   Z-ADD     YECIMO        V3CVA1
076540991012     C                   SETON                                        53
076550991006    3C                   ENDIF
076560991006     C                   Z-ADD     V3CVA1        VARVA1
076570991006     C  N30              UPDATE    LR48S03
076580991006    2C                   ENDDO
076590991006     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
076600991006     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
076610991006     C     VIDIMV        IFGT      *ZEROS
076620991006     C     WDIV          ANDNE     *BLANKS
076630991006     C                   MOVEL     'T'           D17FIM
076640991006     C                   MOVEL     *ZEROS        D17IMV
076650991006     C                   Z-ADD     VIDPOR        D17POR
076660991012     C                   CLEAR                   DTASV
076670991012     C                   EXSR      RDTASV
076680991006     C                   CALL      'FNLV16R'
076690030612     C                   PARM                    Fnlv16DS
076700991006     C                   PARM                    DTASV
076710991006     C                   Z-ADD     O17IMV        VIDIMV
076720991006     C                   ENDIF
076730991006     C                   Z-ADD     VIDPOR        VARPOR
076740991006     C                   MOVEL     VIDDIV        WDIV
076750991012     C* SE VARIATA DIVISA RIEMETTO IN OGNI CASO LA VIDEATA
076760991029     C* PERCHE' L'LV16 DOVRA' CONTROLLARE LA TASSAZIONE
076770991012     C                   SETON                                        90
076780991012     C                   GOTO      ENDC03
076790991006   X1C                   ELSE
076800991006     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
076810991006     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
076820991006     C                   Z-ADD     VIDPOR        VARPOR
076830991006    2C                   DO        20            NRR
076840991006     C     NRR           CHAIN     LR48S03                            30
076850991006     C                   Z-ADD     V3CVA1        VARVA1
076860991006     C  N30              UPDATE    LR48S03
076870991006    2C                   ENDDO
076880991006    1C                   ENDIF
076890941102     C*
076900941102     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
076910941209     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
076920991006    3C     O17FAS        IFEQ      'S'
076930941102     C*
076940941102     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
076950990702    2C     SEI           IFNE      1
076960941209     C                   MOVEL     MSG(46)       VIDMSG
076970941209     C                   SETON                                        5290
076980941102     C                   ADD       1             SEI
076990941102     C                   GOTO      ENDC03
077000990702    2C                   ENDIF
077010990702    1C                   ENDIF
077020941222     C*
077030941222     C* IN PARTENZA SE C'ERA PREPAGATO, DEVO LASCIARLO
077040990702    1C     *IN10         IFEQ      *OFF
077050990702    2C     AR6NFT        IFNE      0
077060941222     C     VIDNFT        ANDEQ     0
077070941222     C                   SETON                                        559028
077080941222     C                   MOVEL     MSG(60)       VIDMSG
077090941222     C                   GOTO      ENDC03
077100941222    2C                   ENDIF
077110990702     C*
077120990702     C* SE PREPAGATO INSERIRE TASSAZIONE O F14 PER TASSARE
077130990702     C  NKL
077140990702    2CANNKN*IN23         IFEQ      *ON
077150990702    3C     VIDIMV        IFEQ      0
077160990702     C                   SETON                                        529028
077170990702     C                   MOVEL     MSG(89)       VIDMSG
077180990702     C                   GOTO      ENDC03
077190990702   X3C                   ELSE
077200990702    4C     VIDIMV        IFNE      SAVIMV
077210990702     C                   Z-ADD     AR6DFT        ARBDFT
077220990702     C                   EXSR      TOTFAT
077230990702     C                   Z-ADD     D04IFT        VIDIFT
077240990702     C                   Z-ADD     D04IMV        VIDIMV
077250990702     C                   Z-ADD     D04POR        VIDPOR
077260991006     C                   Z-ADD     D04POR        VARPOR
077270990702     C                   Z-ADD     VIDIMV        SAVIMV
077280990702     C                   SETON                                        90
077290990702    4C                   ENDIF
077300990702    3C                   ENDIF
077310990702    2C                   ENDIF
077320990702    1C                   ENDIF
077330011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
077340011017     C                   SETOFF                                       52
077350011018    1C     FLGVRN        IFEQ      '1'
077360011017    2C                   DO        20            NRR
077370011017     C     NRR           CHAIN     LR48S03                            32
077380011017    3C     *IN32         IFEQ      *OFF
077390011018     C                   SETON                                        52
077400011018    4C     V3CSV1        IFEQ      'N'
077410011017     C                   SETOFF                                       52
077420011017     C                   LEAVE
077430011018    4C                   ENDIF
077440011017    3C                   ENDIF
077450011017    2C                   ENDDO
077460011018    2C     *IN52         IFEQ      '1'
077470011017     C     1             CHAIN     LR48S03                            32
077480011017     C  N32              UPDATE    LR48S03
077490011017     C                   MOVEL     MSG(101)      VIDMSG
077500011105     C                   SETON                                        9028
077510011017     C                   GOTO      ENDC03
077520011017    2C                   ENDIF
077530011017    1C                   ENDIF
077540011206      *
077550011206      * Controllo che la somma degli importi immessi non superi il valore
077560011206      * impostato in §GEIMF
077570011206      *      prima controllo il totale imponibile
077580011206  1b c                   if        vidimv <> 0
077590011206     c                   eval      w0173 = vidimv
077600011206      *      controllo se devo convertire il totale imponibile
077610011206  2b c                   if        viddiv <> §gedcn
077620030612     c                   clear                   YeurcoDS
077630011206     c                   eval      yecdvi = viddiv
077640011206     c                   eval      yecdvo = §gedcn
077650011206     c                   eval      yecimi = vidimv
077660011206     c                   move      decsav        yecdco
077670011206     c                   call      'YEURCO'
077680030612     c                   parm                    YeurcoDS
077690011206  3b c                   if        yecesi = ' '
077700011206     c                   eval      w0173 = yecimo
077710011206  3e c                   endif
077720011206  2e c                   endif
077730011206      *      controllo se l'imponibile supera il limite fatturabile
077740011220  2b c                   if        w0173 > imfsav
077750011206     c                   eval      vidmsg = msg(107)
077760011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
077770011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
077780011206     c                   eval      *in50 = *on
077790011206     c                   eval      *in90 = *on
077800011206     c                   eval      *in28 = *on
077810011206     c                   goto      endc03
077820011206  2e c                   endif
077830011206  1x c                   else
077840011206      *      altrimenti controllo tutti gli altri importi
077850011206     c                   eval      tot_importi = vidpor
077860011206      *      leggo le varie
077870011206  2b c                   do        20            nrr
077880011206     c     nrr           chain     lr48s03                            32
077890011206  3b c                   if        *in32 = *off and v3cva1 <> 0
077900011206  4b c                   if        flgvrn = '1' and v3cva1 = 88888888
077910011206     c                             and v3csv1 = 'N'
077920011206     c                   iter
077930011206  4e c                   endif
077940030624  4b c                   if        v3cva1 = 88888888
077950030624     c                             and v3csv1 = '&'
077960030624     c                   iter
077970030624  4e c                   endif
077980011206     c                   eval      tot_importi = tot_importi + v3cva1
077990011206  3e c                   endif
078000011206  2e c                   enddo
078010011206     c                   eval      w0173 = tot_importi
078020011206      *      controllo se devo convertire il totale importi
078030011206  2b c                   if        viddiv <> §gedcn
078040030612     c                   clear                   YeurcoDS
078050011206     c                   eval      yecdvi = viddiv
078060011206     c                   eval      yecdvo = §gedcn
078070011206     c                   eval      yecimi = tot_importi
078080011206     c                   move      decsav        yecdco
078090011206     c                   call      'YEURCO'
078100030612     c                   parm                    YeurcoDS
078110011206  3b c                   if        yecesi = ' '
078120011206     c                   eval      w0173 = yecimo
078130011206  3e c                   endif
078140011206  2e c                   endif
078150011206      *      controllo se l'imponibile supera il limite fatturabile
078160011220  2b c                   if        w0173 > imfsav
078170131016     c                   eval      vidmsg = msg(107)
078180011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
078190011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
078200011206     c                   eval      *in50 = *on
078210011206     c                   eval      *in90 = *on
078220011206     c                   eval      *in28 = *on
078230011206     c                   goto      endc03
078240011206  2e c                   endif
078250011206  1e c                   endif
078260131016     c
078270131016     c* CONTROLLI PER BOLLA CONTABILIZZATA E PGM NON RICHIAMATO (29 ON)
078280131016     c* se f6=conferma
078290131016     c                   if        *in29 and *inkf
078300131016     c* Se eseguito F14=Tassazione --> errore bolla contabilizzata
078310131016     c                   if        winf14='1'
078320131016     c                   eval      vidmsg = msg(151)
078330131016     c                   eval      *in90 = *on
078340131016     c                   eval      *in28 = *on
078350131016     c                   goto      endc03
078360131016     c                   endif
078370131016     c* Se non variato nè ias nè vmd -> errore premere F12
078380131016     c                   if        vidias=bol_vidias and
078390131016     c                             (vidvas=bol_vidvas or vidias=0) and
078400131016     c                             vidvmd=bol_vidvmd and
078410131016     c                             (vidvad=bol_vidvad or vidvmd=0)
078420131016     c                   eval      vidmsg = msg(152)
078430131016     c                   eval      *in90 = *on
078440131016     c                   eval      *in28 = *on
078450131016     c                   goto      endc03
078460131016     c                   endif
078470131016     c                   endif
078480911210     C*
078490941209     C     ENDC03        ENDSR
078500030710      *---------------------------------------------------------------*
078510030710      * Gestione videata password x importo da assicurare             *
078520030710      *---------------------------------------------------------------*
078530060201     c**   Sr_Gespsw     BegSr
078540060201     C**
078550060201     c**                 Clear                   w01psw
078560060201     c**                 Do        *Hival
078570060201     c**                 Exfmt     lr48w01
078580060201     c**                 Eval      *In28 = *Off
078590060201     c**                 If        *InKl
078600060201     c**                 Eval      *In90 = *On
078610060201     c**                 Leave
078620060201     c**                 EndIf
078630060201     c**                 If        w01psw = *Blanks
078640060201     c**                 Eval      w01msg = 'Immettere la password'
078650060201     c**                 Eval      *In28 = *On
078660060201     c**                 Iter
078670060201     c**                 EndIf
078680060201     c**                 If        w01psw <> miapsw
078690060201     c**                 Eval      w01msg = 'Password errata'
078700060201     c**                 Eval      *In28 = *On
078710060201     c**                 Iter
078720060201     c**                 EndIf
078730060201     c**                 Eval      wokpsw = '1'
078740060201     c**                 Leave
078750060201     c**                 EndDo
078760060201     C**
078770060201     c**                 Endsr
078780920319     C*
078790930505     C* CONTROLLO COMUNI  ALL CONTR3 3 CONTR 4 -----------------------*
078800920319     C     CTRCOM        BEGSR
078810920319     C                   SETOFF                                       90
078820030620     C                   CLEAR                   SAV_INDSIN
078830941103     C**
078840920319     C* CODICE CLIENTE
078850941103     C**
078860950104     C*               SALTO CONTROLLO SE SPEDIZIONE IN FRANCO  IN ARRIV
078870950104     C*               LO ESEGUO SE SONO IN PARTENZA
078880941103    1C     *IN36         IFEQ      *OFF
078890941222     C     *IN10         OREQ      *OFF
078900941103     C*
078910941103     C* F7=RICERCA ALFABETICA
078920941103    2C   KG              DO
078930941212     C                   MOVEL     VIDDSA        PARDUT           30
078940920319     C                   MOVEL     *BLANKS       DESCDS           48
078950920319     C                   MOVEL     DESKSC        DESCDS
078960920319     C* PARSTA=9 ESCLUDO ANNULLATI
078970920319     C                   Z-ADD     9             PARSTA
078980060131     C                   Z-ADD     dutkci        CCC               4 0
078990941103    3C     VIDKLP        IFEQ      0
079000000510     C   10              MOVEL     ARBLNA        PARKSM
079010941103     C                   ELSE
079020000510     C                   MOVEL     VIDKLP        PARKSM
079030941103    3C                   ENDIF
079040991112     C                   CLEAR                   PARDIT
079050000510     C                   Z-ADD     1             PARNUM
079060941103     C*
079070000510     C                   CALL      'XALFA3BR'
079080920319     C                   PARM                    PARDUT
079090920319     C                   PARM                    CODUT
079100920319     C                   PARM                    DESCDS
079110920319     C                   PARM                    CCC
079120920319     C                   PARM                    PARSTA            1 0
079130961016     C                   PARM                    PARFLR           90
079140991112     C                   PARM                    PARDIT            3
079150000510     C                   PARM                    PARNUM            2 0
079160000510     C                   PARM                    PARKCM           80
079170000510     C                   PARM                    PARKSM          140
079180000510     C                   PARM                    PARKDM           60
079190941103    3C     PARSTA        IFEQ      -1
079200920319     C*
079210941103    4C     FLGCVR        IFEQ      '2'
079220941212     C*
079230990930     C     WAR62         IFEQ      ' '
079240941212     C                   MOVEL     ARBLNA        VIDKLP
079250941212     C                   MOVEL     9999          VIDKSC
079260941212     C                   ELSE
079270991007     C                   MOVEL     AR6KSC        VIDKLP
079280991007     C                   MOVE      AR6KSC        VIDKSC
079290941212     C                   ENDIF
079300941212     C*
079310941103   X4C                   ELSE
079320920319     C                   MOVEL     ARBKSC        VIDKLP
079330920319     C                   MOVE      ARBKSC        VIDKSC
079340941103    4C                   END
079350920319     C*
079360941103   X3C                   ELSE
079370020305     C                   MOVEL     PARKSM        ACOKSC
079380920319     C                   MOVEL     ACOKSC        VIDKLP
079390920319     C                   MOVE      ACOKSC        VIDKSC
079400970814     C                   CLEAR                   VIDRSM
079410970814     C                   CLEAR                   VIDINM
079420970814     C                   CLEAR                   VIDCAM
079430970814     C                   CLEAR                   VIDLOM
079440970814     C                   CLEAR                   VIDPRM
079450970814     C                   CLEAR                   VIDNZM
079460970814     C                   CLEAR                   VIDPIM
079470941103    3C                   END
079480941103     C*
079490941103     C                   SETON                                        9047
079500941103     C                   GOTO      ENDCOM
079510941212    2C                   ENDDO
079520920319     C* CONTROLLO
079530941103    2C     VIDKLP        IFEQ      0
079540941103     C                   MOVEL     MSG(33)       VIDMSG
079550941103     C                   SETON                                        469028
079560941103     C                   GOTO      ENDCOM
079570941103    2C                   END
079580980722     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE DELLO STESSO S.I. IN
079590991112     C*  CUI SONO  PER LE FATTURE IMMEDIATE
079600040930     C     VIDKLP        CHAIN     AZORG01L                           30
079610040930     c*
079620020725     C     D66BFI        IFEQ      'S'
079630020725     C**N30ORGDIT        IFNE      O50PRA
079640020725     C**                 MOVEL     MSG(81)       VIDMSG
079650020725     C**                 SETON                                        469028
079660020725     C**                 GOTO      ENDCOM
079670020725     C**                 ENDIF
079680020508     c* sempre per le fatture immediate se cliente 9999 il p.o. cliente
079690020508     c* deve essere = alla linea di arrivo della bolla
079700020508     c     vidksc        ifeq      9999
079710020508     c     vidklp        andne     arblna
079720020508     C                   MOVEL     MSG(35)       VIDMSG
079730020508     C                   SETON                                        479028
079740020508     C                   GOTO      ENDCOM
079750020508     c                   endif
079760020306     C                   ENDIF
079770020306     c* se variazione di mittente e cliente 8888 il p.o. cliente deve
079780020306     c* essere = alla linea di partenza bolla
079790020306     c     flgcvr        ifeq      'M'
079800020306     c     vidksc        andeq     8888
079810020306     c     vidklp        andne     arblnp
079820020306     C                   MOVEL     MSG(35)       VIDMSG
079830020306     C                   SETON                                        479028
079840020306     C                   GOTO      ENDCOM
079850020306     c                   endif
079860020508     c* se variazione di bolla in partenza e cliente 9999 errore
079870020508     c     *in10         ifeq      *off
079880020508     c     vidksc        andeq     9999
079890020508     C                   MOVEL     MSG(111)      VIDMSG
079900020508     C                   SETON                                        479028
079910020508     C                   GOTO      ENDCOM
079920020508     c                   endif
079930930507     C*
079940920319     C                   MOVEL     VIDKLP        KSC
079950920319     C                   MOVE      VIDKSC        KSC
079960000510     C  N30              MOVEL     ORGDE3        OG143
079970020318     C  N30              MOVEL     §OGNTW        CLINTW
079980020318     C   30              CLEAR                   CLINTW
079990040930     c* non accetto clienti con ntw = "LOG" o "XXX'
080000040930     c                   if        clintw='LOG'  or clintw='XXX'
080010040930     C                   MOVEL     MSG(130)      VIDMSG
080020040930     C                   SETON                                        469028
080030040930     C                   GOTO      ENDCOM
080040040930     c                   endif
080050051220     c* non è possibile modificare cliente da dpd a non dpd e viceversa
080060051220     c     flgcvr        ifeq      'M'
080070051220     c                   if        (clintw = 'DPD' and clintw <> memntw) or
080080051220     c                             (memntw = 'DPD' and clintw <> memntw)
080090051220     C                   MOVEL     MSG(137)      VIDMSG
080100051220     C                   SETON                                        469028
080110051220     C                   GOTO      ENDCOM
080120051220     c                   endif
080130051220     c                   endif
080140941103     C**
080150920319     C*
080160941103     C     KACO          CHAIN     CNACO000                           30
080170941103    3C  N30ACOFLG        IFNE      ' '
080180920319     C                   SETON                                        30
080190941103    3C                   END
080200941103     C* ERRORE
080210941103    3C     *IN30         IFEQ      *ON
080220941103     C     VIDKSC        OREQ      8888
080230941222     C     *IN10         ANDEQ     *ON
080240941223     C     VIDKSC        OREQ      9999
080250941223     C     *IN36         ANDEQ     *ON
080260941103     C                   MOVEL     MSG(36)       VIDMSG
080270941103     C                   SETON                                        479028
080280941103     C                   GOTO      ENDCOM
080290941103    3C                   ENDIF
080300930507     C*
080310130315    3C**   ACOABL        IFEQ      '8'
080320130315    3C**   ACOABL        OREQ      '*'
080330130315     c
080340130315    3C     ACOABL        ifne      *blanks
080350130418     c     ksc           andne     kscbolla
080360040930     C                   MOVEL     mSG(37)       VIDMSG
080370941103     C                   SETON                                        479028
080380941103     C                   GOTO      ENDCOM
080390941103    3C                   END
080400930507     C*
080410920319     C* SE SEGUE FATTURA NON SI PUO' METTERE COD 9999
080420941103    3C   13VIDKSC        IFEQ      9999
080430941103     C                   MOVEL     MSG(38)       VIDMSG
080440941103     C                   SETON                                        479028
080450941103     C                   GOTO      ENDCOM
080460941103    3C                   END
080470930507     C*
080480970822     C     VIDKSC        IFNE      9999
080490970822     C     VIDKSC        ANDNE     8888
080500970822     C                   MOVEL     ACORAG        DESKSC
080510941103    2C                   END
080520941220     C*
080530941223     C* SALVO ULTIMO CODICE IMPOSTATO
080540941220    2C     COMKSC        IFNE      KSC
080550941220     C                   CLEAR                   CIN
080560941220     C                   CLEAR                   DIE
080570941220     C                   MOVEL     KSC           COMKSC            7 0
080580941220    2C                   END
080590941103    1C                   END
080600030620     C* CERCO IN CNIND IL DIRITTO DI FATTURAZIONE
080610030620     C*
080620030620     C     KACO          CHAIN     CNIND00F                           30
080630030620    3C                   IF        %FOUND(CNIND00F)
080640030620     C                   EVAL      SAV_INDSIN = INDSIN
080650030620     C                   ELSE
080660030620     C                   CLEAR                   SAV_INDSIN
080670030620     C                   ENDIF
080680941103     C**
080690941215     C* CARICO CODICI TARIFFA DEL CLIENTE ED ESEGUO CONTROLLO
080700941103     C**
080710930507     C                   EXSR      CARCTR
080720941209     C*
080730941209     C* SE HO RICERCATO LA TARIFFA, RIEMETTO LA VIDEATA
080740050202    1C     ilv59RIC      IFEQ      'S'
080750941223     C                   SETON                                        9048
080760941209     C                   GOTO      ENDCOM
080770941209    1C                   ENDIF
080780941209     C*
080790941215     C* ATTIVATO IL "?" SUL CODICE TARIFFA
080800050202    1C     olv59ERR      IFEQ      'R'
080810050202     C                   MOVEL     olv59CTR      VIDCTR
080820050202     C                   MOVEL     olv59DFS      DE2CTR
080830941223     C                   SETON                                        9048
080840941209     C                   GOTO      ENDCOM
080850941209    1C                   ENDIF
080860941209     C* ERRORE
080870050202    1C     olv59MSG      IFNE      *BLANKS
080880060606     c* Nella videata della variazione del cod mittente, non do errore
080890060606     c*  se premuto F10 per la modifica del codice tariffa
080900060606     c*  ma disabilito F12
080910060606     c                   if        *inkj and flgcvr='M'
080920060606     c                   clear                   w48f12
080930060606     c                   else
080940050202     C                   MOVEL     olv59MSG      VIDMSG
080950941223     C                   SETON                                        904828
080960941209     C                   GOTO      ENDCOM
080970941209    1C                   ENDIF
080980060606    1C                   ENDIF
080990930507     C*
081000941209     C                   MOVEL     VIDCTR        COMCTR
081010050202     C                   MOVEL     olv59PRG      COMPRG
081020021121
081030160225      * Per bolla FedEx
081040160225If  1c                   If        lnantW = 'FED'
081050160225     c
081060021122      * --> se cliente 8888
081070160225      *     il CTR a video deve essere uno delle tariffe fedex
081080021122If  2c                   If        VidKsc = 8888
081090021122      * ---> e O27Fie = 'M'
081100021122If  3c                   If        WFie =  'M'
081110021122      * la tariffa deve essere quella della cartello
081120160225If  4c                   If        VidCtr <> FedM_ctr
081130021122     c                   Eval      *In48 = *On
081140021122     c                   Eval      *In28 = *On
081150021122     c                   Eval      *In90 = *On
081160021122     c                   Movel     Msg(114)      VidMsg
081170021122     c                   Goto      EndCom
081180021122    4c                   EndIf
081190021122      * ---> e O27Fie non è = 'M'
081200021122   x3c                   Else
081210160225    4c                   if        Vidctr <> FedD_ctr
081220021121     c                   Eval      *In48 = *On
081230021121     c                   Eval      *In90 = *On
081240021121     c                   Eval      *In28 = *On
081250021121     c                   Movel     Msg(114)      VidMsg
081260021121     c                   Goto      EndCom
081270021122    4c                   EndIf
081280021122    3c                   EndIf
081290160225     c
081300021121      * --> se cliente codificato
081310021121   x2c                   Else
081320021121      * ---> e non sono state trovate tariffe
081330021121If  3c                   If        Wtks <> 'S'
081340021121      *      non posso accettare una tariffa documenti se il peso della
081350021121      *      bolla superare il limite previsto in tabella "DFE"
081360021121     c                   Move      Vidctr        DueCtr
081370021121If  4c                   If        ArbPkf > §DfePkgD and
081380021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
081390021121     c                   Eval      *In48 = *On
081400021121     c                   Eval      *In90 = *On
081410021121     c                   Eval      *In28 = *On
081420021121      * Preparo il msg
081430021121     c                   Movea     Msg(115)      K78
081440021121     c                   Movel     §DfePkgD      W006a
081450021121     c                   Movea     W006a         Ski(10)
081460021121     c                   Z-Add     10            i
081470021121     c                   ExSr      Abbl
081480021121     c                   Movea     Ski(10)       K78(59)
081490021121     c                   Move      §DfePkgD      W001a
081500021121     c                   Movel     W001a         K78(66)
081510021121     c                   Movea     K78           VidMsg
081520021121     c                   Goto      EndCom
081530021121    4c                   EndIf
081540021121    3c                   EndIf
081550021121    2c                   EndIf
081560021121    1c                   EndIf
081570920319     C*
081580941103     C     ENDCOM        ENDSR
081590011017     C*
081600011017      *--- TASSAZIONE VARIA 'N' --------------------------------------*
081610011017     C     TASSAN        BEGSR
081620011017      *
081630011017     C                   CLEAR                   DTAS
081640011017     C                   CLEAR                   DTASV
081650011017     C                   MOVEL     VIDKLP        TASKSC
081660011017     C                   MOVE      VIDKSC        TASKSC
081670011017     C                   MOVEL     VIDCTR        TASCTR
081680011017     C                   Z-ADD     ARBPKF        TASPKF
081690160530     C                   Z-ADD     ARBPKb        TASPKb
081700011017     C                   MOVEL     ARBLNP        TASLNP
081710011017     C                   MOVEL     ARBCTS        TASCTS
081720011017     C                   MOVEL     ARBDSP        TASDSP
081730011017     C                   MOVEL     ARBDSP        TASDCT
081740011017     C                   MOVEL     ARBVLF        TASVLF
081750160530     C                   MOVEL     ARBVLc        TASVLc
081760160530     C                   MOVEL     ARBncr        TASncr
081770160530     C                   MOVEL     ARBVLF        TASVLF
081780160530     C                   MOVEL     ARBfvf        TASfvF
081790011017     C                   MOVEL     ARBTSP        TASTSP
081800011017     C     WBOL          IFEQ      '1'
081810011017     C                   MOVEL     §3ATB1        TASTBL
081820011017     C                   ELSE
081830011017     C                   MOVEL     §3ATB2        TASTBL
081840011017     C                   ENDIF
081850011017     C                   MOVEL     ARBLNA        TASLNA
081860011017     C                   MOVEL     ARBNCL        TASNCL
081870160530     C                   MOVEL     ARBNCL        TASNCLb
081880011017     C                   MOVEL     ARBQFT        TASQFT
081890011017     C     WBOL          IFEQ      '1'
081900011017     C     §3AFCA        ANDEQ     1
081910011017     C     WBOL          OREQ      '2'
081920011017     C     §3AFCA        ANDEQ     2
081930011017     C                   MOVEL     AR9CAS        TASCAS
081940011017     C                   MOVEL     AR9VCA        TASVCA
081950011017     C                   MOVEL     AR9TIC        TASTIC
081960011017     C                   ENDIF
081970011017     C                   MOVEL     ARBIAS        TASIAS
081980011017     C                   MOVEL     ARBVAS        TASVAS
081990011017     C                   MOVEL     ARBCAD        TASCAD
082000011017     C                   MOVEL     ARBNZD        TASNZD
082010011017     C                   MOVEL     ARBFIN        TASFIN
082020011017     C                   MOVEL     ARBCAM        TASCAM
082030011017     C                   MOVEL     ARBNZM        TASNZM
082040011017     C                   MOVEL     ARBFAP        TASFAP
082050011017     C                   MOVEL     ARBTC1        TASTC1
082060011017     C                   MOVEL     ARBTC2        TASTC2
082070011017     C     V1CDDT        IFEQ      'C'                                          SI DDT
082080011017     C     V1CDDT        OREQ      'S'
082090031028     c     v1cddt        oreq      'P'
082100031028     c     v1cddt        oreq      'Q'
082110011017     C                   MOVEL     'S'           TASDDT
082120011017     C                   ENDIF
082130060925     c*****              movel     arbfbr        tasflo
082140060925     c                   clear                   dtas01
082150060925     c                   movel     arbfbr        §asfbr
082160060925     c                   movel     arbcca        §ascca
082170060925     c                   eval      tasflo = dtas01
082180011017      **
082190011017      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
082200011017      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
082210020305     C     LNANTW        ifeq      'COR'
082220020305     c     LNANTW        oreq      'MES'
082230020305     c     LNANTW        oreq      'PPT'
082240011017     C                   MOVEL     ARBFDN        TASFDN
082250011017     C                   ENDIF
082260011017      **
082270011017      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
082280030612     C                   CLEAR                   Tisi95DS
082290011017     C                   MOVEL     ARBLOM        I95LOC
082300011017     C                   MOVEL     ARBCAM        I95CAP
082310011017     C                   MOVEL     ARBNZM        I95NAR
082320011017     C                   MOVEL     '3'           I95TCN
082330011017     C                   MOVEL     ARBDSP        I95DAT
082340011017     C     §3AFCA        IFNE      0
082350011017     C                   MOVEL     'S'           I95FCA
082360011017     C                   ENDIF
082370011017     C                   MOVEL     §3ATB1        I95TPO
082380020305     c     lnpntw        ifeq      'FED'
082390020305     c                   movel     'S'           i95fi1
082400020305     c                   endif
082410011017     C                   CALL      'TISI95R'
082420030612     C                   PARM                    Tisi95DS
082430011017     C     O95ERR        IFEQ      *BLANKS
082440011017     C                   MOVEL     O95CTS        TASMCT
082450011017     C                   ENDIF
082460011017     C                   MOVEL     'S'           WCAL13            1
082470011017      * ELABORO E CHIUDO CON LR IL PGM
082480011017     C                   MOVEL     ' '           TASTLA
082490020930     c* imposto peso vdl e colli rilevati
082500030203     c**!!!              clear                   dtas01
082510030203     c**!!!              z-add     arbncp        §asncp
082520030203     c**!!!              z-add     arbpkc        §aspkc
082530030203     c**!!!              movel     dtas01        tasflo
082540030203     c                   z-add     arbncp        Tasncp
082550030203     c                   z-add     arbpkc        Taspkc
082560030204      * Bancali
082570030205     c                   If        %Subst(ArbGva:1:1) = 'B'
082580030729     c                   Eval      TasBan = V1cBan
082590030205     c                   EndIf
082600030729      * Colli Originali
082610030729     c                   If        %Subst(ArbGva:1:1) = 'O'
082620030729     c                   Eval      TasNcl = V1cCor
082630030729     c                   EndIf
082640050804     c
082650050804     c                   eval      parca  = ' '
082660050804     c                   movel     paramt        kpjbu
082670011017      *
082680011017     C                   CALL      'TNSF20R'
082690011017     C                   PARM                    KPJBA
082700011017     C                   PARM                    DTAS
082710011017     C                   PARM                    DTASV
082720011018      *
082730011018      * Tassazione andata buon fine
082740011018     C     TASERR        IFEQ      *BLANKS
082750011018      *
082760011018      * Controllo la divisa della tassazione con la divisa del video
082770011018      * se non corrispondono devo convertire la varia 'N' nella divisa
082780011018      * del video
082790011018     C     TASDIV        IFNE      VIDDIV
082800011018      * aggancio la tabella CV per sicurezza
082810011018     C                   CLEAR                   DSCV
082820011018     C                   MOVEL     'CV'          COD
082830011018     C                   MOVEL(P)  VIDDIV        KEY
082840011018     C                   EXSR      CTABEL
082850011018     C  N30              MOVEL     TBLUNI        DSCV
082860011018      *
082870030612     C                   CLEAR                   YeurcoDS
082880011018     C                   MOVEL     TASDIV        YECDVI
082890011018     C                   MOVEL     VIDDIV        YECDVO
082900011018     C                   Z-ADD     TASIMV        YECIMI
082910011018     C     §CVFDC        IFEQ      'S'
082920011018     C                   MOVE      '3'           YECDCO
082930011018     C                   ELSE
082940011018     C                   MOVE      '0'           YECDCO
082950011018     C                   ENDIF
082960011018     C                   CALL      'YEURCO'
082970030612     C                   PARM                    YeurcoDS
082980011018     C     YECESI        IFEQ      ' '
082990011018     C                   Z-ADD     YECIMO        TASIMV
083000011018     C                   ELSE
083010011018     C                   Z-ADD     88888888      TASIMV
083020011018     C                   ENDIF
083030011018     C                   ENDIF
083040011017      *
083050011017     C                   Z-ADD     TASIMV        V3CVA1
083060011017     C                   Z-ADD     TASIMV        VARVA1
083070011017     C                   ENDIF
083080011017      *
083090011017     C                   ENDSR
083100910429     C*
083110050531begsrC*--- TASSAZIONE BOLLA ------------------------------------------*
083120910429     C     TASSAZ        BEGSR
083130991005     C                   CLEAR                   DTAS
083140991005     C                   CLEAR                   DTASV
083150020503     C                   CLEAR                   wtassa            1
083160030620     C                   CLEAR                   wtassad           1
083170030620     C                   CLEAR                   itassa           11 3
083180050601     c*
083190050601     c* richiamo routine di impostazione campi "comuni" da passare a
083200050601     c* pgm tnsf20r
083210050601     c                   exsr      SR_rtassa
083220910429     C*
083230941221     C* CARICO LE VARIE
083240941221     C*
083250980513     C     WBOL          IFEQ      '1'
083260030709     c                   eval      flgsf20 = 'S'
083270991012     C                   EXSR      RDTASV
083280030709     c                   eval      flgsf20 = ' '
083290980513     C*
083300980513     C                   ELSE
083310040123      * carico varie seconda bolla
083320040123     C                   MOVEL     'G'           TASSVA
083330040123     c                   Eval      TascVag = 'S'
083340041202     C**!!!VI2VA1        IFGT      0
083350040123     C                   MOVEL     VI2SV1        VSV(1)
083360040123     C                   Z-ADD     VI2VA1        VVA(1)
083370040123     c                   Movel     Vi2Es1        ves(1)
083380041202     C**!!!              ENDIF
083390041202     c**!!!              If        Vi2Va2 > *Zeros
083400040123     c                   Movel     Vi2Sv2        Vsv(2)
083410040123     c                   Z-add     Vi2Va2        Vva(2)
083420040123     c                   Movel     Vi2Es2        ves(2)
083430041202     c**!!!              EndIf
083440041202     c**!!!              If        Vi2Va3 > *Zeros
083450040123     c                   Movel     Vi2Sv3        Vsv(3)
083460040123     c                   Z-add     Vi2Va3        Vva(3)
083470040123     c                   Movel     Vi2Es3        ves(3)
083480041202     c**!!!              EndIf
083490041202     c**!!!              If        Vi2Va4 > *Zeros
083500040123     c                   Movel     Vi2Sv4        Vsv(4)
083510040123     c                   Z-add     Vi2Va4        Vva(4)
083520040123     c                   Movel     Vi2Es4        ves(4)
083530041202     c**!!               EndIf
083540041202     c**!!!              If        Vi2Va5 > *Zeros
083550041130     c                   Movel     Vi2Sv5        Vsv(5)
083560041130     c                   Z-add     Vi2Va5        Vva(5)
083570041130     c                   Movel     Vi2Es5        ves(5)
083580041202     c**!!!              EndIf
083590041202     c**!!!              If        Vi2Va6 > *Zeros
083600041130     c                   Movel     Vi2Sv6        Vsv(6)
083610041130     c                   Z-add     Vi2Va6        Vva(6)
083620041130     c                   Movel     Vi2Es6        ves(6)
083630041202     c**!!!              EndIf
083640040126     c                   xfoot     vva           TotImv
083650040126     c                   Z-add     TotImv        TasImv
083660980608     C                   ENDIF
083670011211
083680011211      * Data fattura se c'è
083690030203     c**!!!              clear                   dtas01
083700020930     c**                 if        ar6dft <> 0
083710020930     c**                 movel     ar6dft        §asdat
083720020930     c**                 endif
083730020503     c**
083740020503     c                   if        wtassa=' '
083750020503     c* se devo tassare solo una varia, da tipo bolla, la imposto
083760020503     c                   if        §3asva<>*blanks
083770020503     c                   eval      tassva=§3asva
083780020503     c                   endif
083790030620     c                   endif
083800030925     c* se diritto di fatturazione uguale a zero non calcolo la varia D
083810030925     c                   if        sav_indsin = 0
083820030925     c                   eval      wtassad = 'N'
083830030925     c                   endif
083840030623      * se tassazione fattura immediata o ritassazione prepagato
083850030623      * passo al TNSF20R la varia "D" e l'importo del diritto
083860030620      * di fatturazione da sommare che prendo da cnind (INDSIN)
083870030623     c                   if        (*in14 and not *in29  and wtassad = ' ')  or
083880030623     c                             (not *in10 and vidnft > 0 and not *in29 and
083890030623     c                             wtassad = ' ')
083900030925      *
083910030620     c                   move      sav_indsin    tasivdf
083920030620     c                   eval      tassvdf = 'D'
083930030620      * se bolla già tassat devo sommare la sola varia "D"
083940030620      * imponibile > 0 oppure imponibile a zero ma singola varia vaolorizzata x bolle speciali
083950040123     c**!!!              if        tasimv > 0 or (tasimv = 0 and itassa > 0)
083960040123     c                   if        tasimv > 0
083970030620     c                   eval      tassva = 'D'
083980030620     c                   endif
083990030620      * se singola varia per bolla speciale non è da calcolare ed è uguale a zero non calcolo la
084000030620      * varia D
084010030620     c                   if        wtassa = 'N' and tasimv = 0 and itassa = 0
084020030620     c                   eval      wtassad = 'N'
084030030620     c                   endif
084040030620
084050030620     c                   endif
084060011211
084070030620      * se non è tassazione fattura immediata non devo calcolare la varia D
084080030623      * o ritassazione prepagato
084090030623     c*                  if        not *in14
084100030623     c                   if        not *in14  or  *in29 or
084110030623     c                             (not *in10 and vidnft = 0)
084120030620     c                   eval      wtassad = 'N'
084130030620     c                   endif
084140030620      * se non devo calcolare la singola varia e neppure la varia D salto la tassazione
084150030620     c                   if        wtassa = 'N' and wtassad = 'N'
084160030620     c                   goto      endtas
084170030620     c                   endif
084180151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
084190151127     c* è solo per tassare una specifica varia
084200151126     C     WBOL          IFEQ      '1'
084210151126     c* PACKING LIST
084220151126     c                   if        par5als='S' or par5alx='S'
084230151126     c                   eval      tasspl='S'
084240151126     c                   endif
084250151126     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
084260151126     C                   MOVEL     'M'           WTRC
084270151126     C     KARB2         CHAIN(N)  FiAR401l
084280151126     c                   if        %found(fiar401l)
084290151126     c                   movel     ar4not        dsbl4m
084300151126     c                   if        §b4ado='S'
084310151126     c                   eval      tasprt='S'
084320151126     c                   endif
084330151126     c                   endif
084340151126
084350151126     c                   endif
084360050804     c
084370050804     c                   eval      parca  = ' '
084380050804     c                   movel     paramt        kpjbu
084390910429     C*
084400941209     C                   CALL      'TNSF20R'
084410910429     C                   PARM                    KPJBA
084420991005     C                   PARM                    DTAS
084430991005     C                   PARM                    DTASV
084440910429     C*
084450941212    1C     TASERR        IFNE      *BLANKS
084460980608     C     TASMSG        IFNE      *BLANKS
084470980608     C                   MOVEL(P)  TASMSG        VIDMSG
084480980608     C                   ELSE
084490941212     C                   MOVEL     MSG(55)       VIDMSG
084500980608     C                   ENDIF
084510941209     C                   SETON                                        499028
084520991006     C* SE ERRORI IN TASSAZIONE SPROTEGGO LA DIVISA
084530991006     C                   SETOFF                                       07
084540941212   X1C                   ELSE
084550980513     C     WBOL          IFEQ      '1'
084560950421     C                   MOVEL     TASQFT        VIDQFT
084570950424     C                   MOVEL     TASQFT        VARQFT
084580910429     C                   MOVEL     TASPOR        VIDPOR
084590991006     C                   MOVEL     TASPOR        VARPOR
084600991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
084610991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
084620991025     C* DIVISA
084630991025     C     VIDDIV        IFNE      TASDIV
084640991025     C     *INKF         ANDEQ     *ON
084650991025     C                   SETON                                        90
084660991025     C                   ENDIF
084670991025     C*
084680991005     C                   MOVEL     TASDIV        VIDDIV
084690991006     C                   MOVEL     TASDIV        WDIV
084700991005     C                   Z-ADD     TASIMV        VIDIMV
084710941209     C* CARICO LE VARIE
084720941212    2C                   DO        20            NRR
084730941209     C     NRR           CHAIN     LR48S03                            30
084740941209     C                   MOVEL     VSV(NRR)      V3CSV1
084750941209     C                   Z-ADD     VVA(NRR)      V3CVA1
084760991006     C                   Z-ADD     VVA(NRR)      VARVA1
084770941209     C                   MOVEL     VES(NRR)      V3CEV1
084780941209     C   30              WRITE     LR48S03
084790941209     C  N30              UPDATE    LR48S03
084800941212    2C                   ENDDO
084810941209     C*
084820980513    XC                   ELSE
084830040123     c                   Clear                   ContaSv
084840040123      * carico le varie seconda bolla
084850040123Do  2c                   Do        20            c
084860040123If  3c                   If        Vsv(c) <> §$2Iva and
084870040123     c                             Vsv(c) <> §$2Bol
084880040123     c                   Add       1             ContaSv
084890040123S   4c                   Select
084900040123     c                   When      ContaSv = 1
084910040123     c                   Movel     Vsv(c)        Vi2Sv1
084920040123     c                   Z-add     Vva(c)        Vi2Va1
084930040123     c                   Movel     Ves(c)        Vi2Es1
084940040123     c                   When      ContaSv = 2
084950040123     c                   Movel     Vsv(c)        Vi2Sv2
084960040123     c                   Z-add     Vva(c)        Vi2Va2
084970040123     c                   Movel     Ves(c)        Vi2Es2
084980040123     c                   When      ContaSv = 3
084990040123     c                   Movel     Vsv(c)        Vi2Sv3
085000040123     c                   Z-add     Vva(c)        Vi2Va3
085010040123     c                   Movel     Ves(c)        Vi2Es3
085020040123     c                   When      ContaSv = 4
085030040123     c                   Movel     Vsv(c)        Vi2Sv4
085040040123     c                   Z-add     Vva(c)        Vi2Va4
085050040123     c                   Movel     Ves(c)        Vi2Es4
085060041130     c                   When      ContaSv = 5
085070041130     c                   Movel     Vsv(c)        Vi2Sv5
085080041130     c                   Z-add     Vva(c)        Vi2Va5
085090041130     c                   Movel     Ves(c)        Vi2Es5
085100041130     c                   When      ContaSv = 6
085110041130     c                   Movel     Vsv(c)        Vi2Sv6
085120041130     c                   Z-add     Vva(c)        Vi2Va6
085130041130     c                   Movel     Ves(c)        Vi2Es6
085140040123    4c                   EndSl
085150040123    3c                   EndIf
085160040123    2c                   EndDo
085170040123     C**!!!              Z-ADD     1             C
085180040123     C**!!!TASSVA        LOOKUP    VSV(C)                                 33
085190040123     C*!!33              Z-ADD     VVA(C)        VI2VA1
085200040123     C*!!33              Z-ADD     VVA(C)        VA2VA1
085210040123     C*!!33              MOVEL     VSV(C)        VI2SV1
085220991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
085230991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
085240991025     C* DIVISA
085250040123     C*!!33VI2DIV        IFNE      TASDIV
085260040123     C     VI2DIV        IFNE      TASDIV
085270991025     C     *INKF         ANDEQ     *ON
085280991025     C                   SETON                                        90
085290991025     C                   ENDIF
085300040123     C*!!33              MOVEL     TASDIV        VI2DIV
085310040123     C*!!33              MOVEL     TASDIV        WDIV
085320040123     C                   MOVEL     TASDIV        VI2DIV
085330040123     C                   MOVEL     TASDIV        WDIV
085340040123     C*!!33VES(C)        IFNE      ' '
085350040123     C**!!!VI2CEI        ANDEQ     ' '
085360040123     C**!!!              MOVEL     VES(C)        VI2CEI
085370040123     C**!!!              ENDIF
085380980513     C                   ENDIF
085390020503     C                   ENDIF
085400980513     C*
085410030620    1C*****              ENDIF
085420941209     C*
085430911211     C     ENDTAS        ENDSR
085440050601     C*
085450050601     C*--- Riempimento campi comuni di passaggio a tnsf20r -----------*
085460050601     C     sr_rtassa     BEGSR
085470050601     C* CODIFICATO
085480050601    1C     VIDKSC        IFNE      9999
085490050601    1C     VIDKSC        ANDNE     8888
085500050601     C                   MOVEL     VIDKLP        TASKSC
085510050601     C                   MOVE      VIDKSC        TASKSC
085520050601     C                   MOVEL     VIDCTR        TASCTR
085530050601   X1C                   ELSE
085540050601     C*
085550050601     C* E HO LA DATA FATTURA USO QUELLA, ALTRIMENTI DATEU
085560050601     C     AR6DFT        IFGT      0
085570050601     C                   Z-ADD     AR6DFT        WDATA
085580050601     C                   ELSE
085590050601     C                   Z-ADD     DATEU         WDATA
085600050601     C                   ENDIF
085610050601     C*
085620050601     C* DAL TIPO SERVIZIO PRENDO LA TARIFFA CON CUI TASSARE
085630050601     C*
085640160225     c**                 eval      tasksc  = o27ksc
085650160225     c**                 eval      tasctr  = o27ctr
085660160225     C                   MOVEL     VIDKLP        TASKSC
085670160225     C                   MOVE      VIDKSC        TASKSC
085680160225     C                   MOVEL     VIDCTR        TASCTR
085690050601     C**
085700050601    1C                   ENDIF
085710050601     C*
085720050601     C                   Z-ADD     ARBPKF        TASPKF
085730160530     C                   Z-ADD     ARBPKb        TASPKb
085740050601     C                   MOVEL     ARBLNP        TASLNP
085750050601     C                   MOVEL     ARBCTS        TASCTS
085760050601     C                   MOVEL     ARBDSP        TASDSP
085770050601     C                   MOVEL     ARBDSP        TASDCT
085780050601     C                   MOVEL     ARBVLF        TASVLF
085790160530     C                   MOVEL     ARBfvF        TASfvf
085800160530     c                   z-add     arbvlc        tasvlc
085810160530     c                   z-add     arbncr        tasncr
085820050601     C                   MOVEL     ARBTSP        TASTSP
085830050601     C     WBOL          IFEQ      '1'
085840050601     C                   MOVEL     §3ATB1        TASTBL
085850050601     C                   MOVEL     VIDDIV        TASDIV
085860050601     C                   Z-ADD     VIDIMV        TASIMV
085870050601     C                   ELSE
085880050601     C                   MOVEL     §3ATB2        TASTBL
085890050601     C                   MOVEL     VI2DIV        TASDIV
085900050601     C**!!!              Z-ADD     VI2VA1        TASIMV
085910050601     C                   END
085920050601     C                   MOVEL     ARBLNA        TASLNA
085930050601     C                   MOVEL     ARBNCL        TASNCL
085940160530     C                   MOVEL     ARBNCL        TASNCLB
085950050601     C                   MOVEL     VIDPOR        TASPOR
085960050601     C                   MOVEL     VIDQFT        TASQFT
085970050601     C     WBOL          IFEQ      '1'
085980050601     C     §3AFCA        ANDEQ     1
085990050601     C     WBOL          OREQ      '2'
086000050601     C     §3AFCA        ANDEQ     2
086010050601     C                   MOVEL     AR9CAS        TASCAS
086020050601     C                   MOVEL     AR9VCA        TASVCA
086030050601     C                   MOVEL     AR9TIC        TASTIC
086040050601     C                   ENDIF
086050050601     C                   MOVEL     VIDIAS        TASIAS
086060050601     C                   MOVEL     VIDVAS        TASVAS
086070050601     C                   MOVEL     ARBCAD        TASCAD
086080050601     C                   MOVEL     ARBNZD        TASNZD
086090050601     C                   MOVEL     ARBCAM        TASCAM
086100050601     C                   MOVEL     ARBNZM        TASNZM
086110050601     C                   MOVEL     ARBFIN        TASFIN
086120050601     C                   MOVEL     ARBFAP        TASFAP
086130050601     C                   MOVEL     ARBTC1        TASTC1
086140050601     C                   MOVEL     ARBTC2        TASTC2
086150050601     C     V1CDDT        IFEQ      'S'
086160050601     C     V1CDDT        OREQ      'C'
086170050601     c     v1cddt        oreq      'P'
086180050601     c     v1cddt        oreq      'Q'
086190050601     C                   MOVEL     'S'           TASDDT
086200050601     C                   ENDIF
086210060925     c****               movel     arbfbr        tasflo
086220060925     c                   clear                   dtas01
086230060925     c                   movel     arbfbr        §asfbr
086240060925     c                   movel     arbcca        §ascca
086250110630     c                   movel     wemd          §asemd
086260150108     c* Pin Code
086270150108     c                   if        arbgma<>*blanks
086280150108     c                   Movel     '7R'          cod
086290150108     c                   Movel(p)  ArbGMa        key
086300150108     c                   ExSr      ctabel
086310150108     c  n30              Movel     TblUni        ds7R
086320150108     c   30              Clear                   ds7R
086330150108     c                   if        §7rpincode='S'
086340150108     c                   movel     'S'           §aspin
086350150108     c                   endif
086360150108     c                   endif
086370060925     c                   eval      tasflo = dtas01
086380050601     C* SE BOLLA ARRIVI IN ASSEGNATO EXPORT, NON PASSO LA CONSEGNA A
086390050601     C*  MAGAZZINO PER FAR CALCOLARE SEMPRE LA VARIA "U"
086400050601     C     *IN10         IFEQ      *OFF
086410050601     C     *IN06         OREQ      *ON
086420050601     C     lnantw        oreq      'COR'
086430050601     c     lnantw        oreq      'MES'
086440050601     c     lnantw        oreq      'PPT'
086450050601     C                   MOVEL     ARBFDN        TASFDN
086460050601     C                   ENDIF
086470050601     C**
086480050601     C* CHAI SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
086490050601     C                   CLEAR                   Tisi95DS
086500050601     C                   MOVEL     ARBCAM        I95CAP
086510050601     C                   MOVEL     ARBNZM        I95NAR
086520050601     C                   MOVEL     '3'           I95TCN
086530050601     C                   MOVEL     ARBDSP        I95DAT
086540050601     C                   MOVEL     FLGTB1        I95TPO
086550050601     C     §3AFCA        IFNE      0
086560050601     C                   MOVEL     'S'           I95FCA
086570050601     C                   ENDIF
086580050601     c     lnpntw        ifeq      'FED'
086590050601     c                   movel     'S'           i95fi1
086600050601     c                   endif
086610050601     C**
086620050601     C                   CALL      'TISI95R'
086630050601     C                   PARM                    Tisi95DS
086640050601     C     O95ERR        IFEQ      *BLANKS
086650050601     C                   MOVEL     O95CTS        TASMCT
086660050601     C                   ENDIF
086670050601
086680050601     c                   z-add     arbncp        Tasncp
086690050601     c                   z-add     arbpkc        Taspkc
086700050601      * Bancali
086710050601     c                   If        %Subst(ArbGva:1:1) = 'B'
086720050601     c                   Eval      TasBan = V1cBan
086730050601     c                   EndIf
086740050601      * Colli Originali
086750050601     c                   If        %Subst(ArbGva:1:1) = 'O'
086760050601     c                   Eval      TasNcl = V1cCor
086770050601     c                   EndIf
086780050601     c                   endsr
086790050531     C*--- TASSAZIONE BOLLA ------------------------------------------*
086800050531     C     TASSAR        BEGSR
086810050601     c                   clear                   dtas
086820050601     c                   clear                   dtasv
086830050601     c* richiamo routine di riempimento campi da passare a tnsf20r
086840050601     c                   eval      wbol = '1'
086850050601     c                   exsr      SR_rtassa
086860050531     c                   eval      tassva = 'R'
086870050804     c
086880050531     c                   eval      parca  = 'S'
086890050531     c                   movel     paramt        kpjbu
086900050531      *
086910050531     C                   CALL      'TNSF20R'
086920050531     C                   PARM                    KPJBA
086930050531     C                   PARM                    DTAS
086940050531     C                   PARM                    DTASV
086950050722      *
086960050804      *
086970050722     c                   eval      parca  = ' '
086980050804     c***                movel     paramt        kpjbu
086990050722      *
087000050531     c                   endsr
087010911211     C*
087020011017     C*
087030991006     C* CALCOLO IL TOTALE FATTURA-------------------------------------*
087040990702     C     TOTFAT        BEGSR
087050030612     C                   CLEAR                   FNLV04DS
087060990702     C* AROTONDO IN ARRIVO O IN PARTENZA SE PREPAGATI NUOVI
087070990702    2C     *IN10         IFEQ      *ON
087080990702     C     *IN23         OREQ      *ON
087090991005     C                   MOVEL     VIDDIV        DIVGEI
087100991005     C                   EXSR      RIEGEI
087110991005     C                   MOVEL     §GEAED        D04AED
087120991005     C                   Z-ADD     §GEAVL        D04VAR
087130990702    2C                   ENDIF
087140991005     C                   MOVEL     'T'           D04FIM
087150990702     C                   MOVEL     ARBDFT        D04DFT
087160990702     C                   MOVEL     VIDIMV        D04IMV
087170990702     C                   MOVEL     VIDCEI        D04CEI
087180990702     C                   MOVEL     AR9TIC        D04TIC
087190990702     C                   MOVEL     AR9VCA        D04VCA
087200990702     C                   Z-ADD     AR9CAS        D04CAS
087210990702     C                   MOVEL     '1'           D04SPE
087220990702     C   23              MOVEL     '8'           D04SPE
087230991005     C                   MOVEL     §3ATB1        D04TBL
087240991005     C                   MOVEL     VIDDIV        D04DIV
087250990702     C                   Z-ADD     VIDPOR        D04POR
087260990702     C                   MOVEL     '1'           UNO               1
087270990702     C*
087280991005     C                   CALL      'FNLV04R'
087290030612     C                   PARM                    FNLV04DS
087300991005     C                   PARM                    DTASV
087310990702     C                   ENDSR
087320990702     C*
087330991005     C**************************************************************************
087340991005     C*  REPERIMENTO TABELLA GEI
087350991005     C**************************************************************************
087360991005     C     RIEGEI        BEGSR
087370030612     C                   CLEAR                   Tibs02DS
087380991117     C                   CLEAR                   DGEI
087390991117     C                   MOVEL     'C'           T02MOD
087400991117     C                   MOVEL     KNSIF         T02SIF
087410991117     C                   MOVEL     'GEI'         T02COD
087420991117     C                   MOVEL     DIVGEI        T02KE1
087430991117     C                   CALL      'TIBS02R'
087440991117     C                   PARM                    KPJBA
087450030612     C                   PARM                    Tibs02DS
087460991117     C     T02ERR        IFEQ      *BLANKS
087470991117     C                   MOVEL     T02UNI        DGEI
087480991117     C                   ENDIF
087490991005     C                   ENDSR
087500911211     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE TX -----------------*
087510911211     C     REGIS3        BEGSR
087520041027     C                   SETOFF                                       3709
087530130412     c                   clear                   wpda_ndc
087540130412     c                   clear                   wpda_ddc
087550130412     c                   clear                   wpda_dcm
087560041027      * salvo i campi relativi al valore da assicurare, qtà da fatturare, valore merce
087570041027     c                   eval      sav_arbias = arbias
087580041027     c                   eval      sav_arbvas = arbvas
087590041027     c                   eval      sav_arbqft = arbqft
087600041027     c                   eval      sav_arbvmd = arbvmd
087610041027     c                   eval      sav_arbvad = arbvas
087620041027     c                   movel     arbctr        sav_arbctr
087630950104     C*  GIRO UDATE
087640950104     C                   TIME                    W0140            14 0
087650950104     C* UDATE IN GGMMAAAA
087660950104     C                   MOVE      W0140         WDTGIO            8 0
087670950104     C*
087680950104     C* UDATE IN AAAAMMGG
087690950104     C                   Z-ADD     WDTGIO        G02DAT
087700950104     C                   MOVEL     *BLANK        G02ERR
087710950104     C                   CALL      'XSRDA8'
087720950104     C                   PARM                    WLBDAT
087730950104     C                   MOVEL     G02INV        DATEU             8 0
087740941221     C* PRENDO I VALORI ASSOLUTI
087750941221     C                   MLLZO     1             VIDPOR
087760941221     C                   MLLZO     1             VIDIMV
087770941221     C                   MLLZO     1             VIDIAS
087780941221     C                   MLLZO     1             VIDVMD
087790941221     C                   MLLZO     1             VIDQFT
087800941221     C                   MLLZO     1             VIDKLP
087810941221     C                   MLLZO     1             VIDKSC
087820070515     c
087830070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
087840070515     c* iornare direttamente il file
087850070515     c*  solo se cambiati dati su testa bolla
087860070515     c                   clear                   wsiaggio          1
087870070515  2b c                   if        sav_arbias <> vidias or sav_arbvas<>vidvas
087880070515     c                             or
087890070515     c                             sav_arbvmd <> vidvmd or sav_arbvad<>vidvad
087900070515     c                             or
087910070515     c                             sav_arbqft <> vidqft  or
087920070515     c                             sav_arbctr<>vidctr
087930070515     c                   eval      wsiaggio='S'
087940070515     c                   clear                   waltrabolla       1
087950070515     C   10KARB          CHAIN     FNBLP01L                           3091
087960070515     C  n10KARB          CHAIN     FNARB01L                           3091
087970070515     c*
087980070515     c                   if        *in91
087990070515     C                   SETON                                        28
088000070515     C                   MOVEL     MSG(4)        VIDMSG
088010070515     C                   GOTO      ENDRG3
088020070515     c                   else
088030070515     c                   if        *in30
088040070515     c                   eval      waltrabolla='N'
088050070515     c                   else
088060130411     c  n10              eval      wpda_ndc=arbndc
088070130411     c  n10              eval      wpda_ddc=arbddc
088080130411     c  n10              eval      wpda_dcm=arbdcm
088090070515     C   10KARB          CHAIN     FNARB01L                           30
088100070515     C  N10KARB          CHAIN     FNBLP01L                           30
088110070515     c* Alla fine per non sporcarmi i campi del record, richaino il
088120070515     c*  record originale che sto variando
088130130411     c   10              eval      wpda_ndc=arbndc
088140130411     c   10              eval      wpda_ddc=arbddc
088150130411     c   10              eval      wpda_dcm=arbdcm
088160070515     c                   endif
088170070515     c                   endif
088180070515     c                   endif
088190131120    1c                   if        wsiaggio=*blanks and d66pda<>' '
088200130412     c                   clear                   waltrabolla       1
088210130412    2c                   if        not *in10
088220130412     C     KARB          CHAIN(n)  FNARB01L                           30
088230130412    3c                   if        *in30
088240130412     c                   eval      waltrabolla='N'
088250130412    3c                   endif
088260130412    2c                   endif
088270130412     c                   eval      wpda_ndc=arbndc
088280130412     c                   eval      wpda_ddc=arbddc
088290130412     c                   eval      wpda_dcm=arbdcm
088300130412     C  N10KARB          CHAIN     FNBLP01L                           30
088310130412     c                   endif
088320941213     C*
088330941213     C* INVIO PER SEDE
088340991006     C                   MOVEL     'FIARBT0T'    SFILE
088350941213     C* ALLOCO OGGETTI
088360941213     C                   EXSR      ALLOC
088370070515     c
088380070515     c                   if        *in91
088390070525     C   10              unlock    FNBLP01L
088400070525     C  N10              unlock    FNARB01L
088410070515     C                   GOTO      ENDRG3
088420070515     c                   endif
088430941213     C*
088440991001     C                   MOVEL     *BLANKS       WCKINL            1
088450991001     C                   EXSR      CARWAR
088460151020     c* Mmorizzo il totale imponibilie del video prima degli arrotondamenti
088470151020     c****               if        tassatoimv=0
088480151019     c                   movel     vidimv        Tassatoimv
088490151020     c****               endif
088500991006     C* SOLO SE C'E' NUMERO FATTURA CALCOLO
088510991006     C     VIDDFT        IFGT      *ZEROS
088520000105     C                   MOVEL     O17DFA        ARBDFT
088530991006     C                   EXSR      TOTFAT
088540991006     C     D04BOL        IFGT      *ZEROS
088550991006     C                   ADD       1             C
088560991006     C                   MOVEL     §$2BOL        WSV(C)
088570991006     C                   Z-ADD     D04BOL        WVA(C)
088580991006     C                   ENDIF
088590991006     C     D04IVA        IFGT      *ZEROS
088600991006     C                   ADD       1             C
088610991006     C                   MOVEL     §$2IVA        WSV(C)
088620991006     C                   Z-ADD     D04IVA        WVA(C)
088630991006     C                   ENDIF
088640991006     C                   ENDIF
088650941213     C*
088660941213     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
088670950616    1C     C             IFGT      0
088680941213     C                   SETON                                        09
088690941213     C*
088700941213     C* INVIO PER SEDE
088710941213     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
088720950616    2C     C             IFGT      3
088730991006     C* MI SALVO I CAMPI DELL'FIARBT
088740941213     C                   MOVEL     SOVR(1)       SOARBT
088750941213     C                   MOVEL     SALC(1)       SAARBT
088760941213     C                   MOVEL     SDLC(1)       SDARBT
088770941213     C*
088780991006     C                   MOVEL     'FIARBU0T'    SFILE
088790941213     C* ALLOCO OGGETTI
088800941213     C                   EXSR      ALLOC
088810070515     c
088820070515     c                   if        *in91
088830070525     C   10              unlock    FNBLP01L
088840070525     C  N10              unlock    FNARB01L
088850070515     C                   MOVEL     SDARBT        SDLC(1)
088860070515     c                   exsr      DLCSED
088870070525     C                   SETON                                        2891
088880070515     C                   GOTO      ENDRG3
088890070515     c                   endif
088900941213     C*
088910950616    2C                   ENDIF
088920950616    1C                   ENDIF
088930950114     C*
088940950114     C                   MOVEL     KNMUS         ARBPRU
088950950114     C                   Z-ADD     DATEU         ARBDTV
088960950114     C                   TIME                    ARBORV
088970950114     C                   MOVEL     VIDCVR        ARBCVB
088980991011     C                   MOVEL     '1'           ARBTRC
088990991011     C*
089000991011     C                   MOVEL     '1'           AR6TRC
089010930511     C*
089020060130     C* SE DA STORICIZZARE
089030060130    1C**   *IN10         IFEQ      *ON
089040060130     C**   D66FSA        ANDEQ     'S'
089050060130     C**   *IN10         OREQ      *OFF
089060060130     C**   D66FSP        ANDEQ     'S'
089070060130     C     D66FSA        ifeq      'S'
089080941213     C                   EXSR      STOARB
089090950616    1C                   ENDIF
089100060302     c* storicizzazione motivazione
089110060302     c     d66mtv        ifeq      'S'
089120060302     c                   exsr      sto_mtv
089130060302     c                   endif
089140941213     C**
089150991006     C* INVIO VARIAZIONE --> FIARBU0T
089160941213     C**
089170950616    1C     *IN09         IFEQ      *ON
089180941213     C     C             ANDGT     3
089190941213     C                   Z-ADD     4             C
089200941213     C*
089210941213    2C     D66FFI        IFEQ      'S'
089220950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
089230950616    3C     §3ABSD        IFEQ      ' '
089240941213     C                   Z-ADD     4             C
089250991006     C                   OPEN      FIARBU0T
089260941213     C*
089270941213     C* NE SCRIVO FIN TANTO CHE CE NE SONO
089280950616    4C     WSV(C)        DOWNE     ' '
089290941213     C                   MOVEL     WSV(C)        ARBSVN
089300941213     C                   Z-ADD     WVA(C)        ARBVAN
089310991006     C                   WRITE     FIARBUTT
089320941213     C                   ADD       1             C
089330950616    4C                   ENDDO
089340941213     C*
089350991006     C                   CLOSE     FIARBU0T
089360941213     C* DISALLOCO OGGETTO MEMBRO PER SEDE
089370950616    3C                   ENDIF
089380950208     C                   EXSR      DLCSED
089390950208    2C                   ENDIF
089400941213     C* REIMPOSTO I CAMPI
089410941213     C                   MOVEL     SOARBT        SOVR(1)
089420941213     C                   MOVEL     SAARBT        SALC(1)
089430941213     C                   MOVEL     SDARBT        SDLC(1)
089440941213    1C                   ENDIF
089450930506     C*
089460930506     C                   Z-ADD     VIDKSC        ARBKSC
089470930506     C                   MOVEL     VIDKLP        ARBKSC
089480941209     C                   MOVEL     VIDIAS        ARBIAS
089490941209     C                   MOVEL     VIDVAS        ARBVAS
089500011017      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
089510011017     C     VIDIAS        IFEQ      *ZEROS
089520011017     C                   CLEAR                   ARBVAS
089530011017     C                   ENDIF
089540011017      *
089550941209     C                   MOVEL     VIDVMD        ARBVMD
089560941215     C                   MOVEL     VIDVAD        ARBVAD
089570011017      * Se il valore merce è a 0 non devo scrivere la divisa
089580011017     C     VIDVMD        IFEQ      *ZEROS
089590011017     C                   CLEAR                   ARBVAD
089600011017     C                   ENDIF
089610011017      *
089620941209     C                   MOVEL     VIDQFT        ARBQFT
089630930506     C                   MOVEL     VIDCTR        ARBCTR
089640991006     C                   MOVEL     VIDDIV        ARBDIV
089650941213     C                   MOVEL     VIDPOR        ARBPOR
089660941213     C                   MOVEL     WSV(1)        ARBSV1
089670941213     C                   MOVEL     WVA(1)        ARBVA1
089680941213     C                   MOVEL     WSV(2)        ARBSV2
089690941213     C                   MOVEL     WVA(2)        ARBVA2
089700941213     C                   MOVEL     WSV(3)        ARBSV3
089710941213     C                   MOVEL     WVA(3)        ARBVA3
089720991006     C                   Z-ADD     VIDIMV        ARBIMV
089730991006     C                   MOVEL     O17DFA        ARBDFT
089740950104     C                   MOVEL     VIDNFT        ARBNFT
089750950104     C                   MOVEL     VIDFIV        ARBFIV
089760991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
089770991013     C* STATO MODIFICATO TOLGO LA 'P'
089780991013     C     AR6FIM        IFEQ      'P'
089790991013     C     ARBIMV        ANDNE     AR6IMV
089800991013     C     *IN10         ANDEQ     *ON
089810991013     C                   MOVE      *BLANKS       ARBFIM
089820991013     C                   ELSE
089830991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
089840991013     C                   MOVE      AR6FIM        ARBFIM
089850991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
089860991013     C     WAR6          IFNE      'E'
089870991013     C     *IN10         ANDEQ     *OFF
089880991013     C                   MOVEL     'P'           ARBFIM
089890991013     C                   END
089900991013     C                   ENDIF
089910941209     C*
089920941212     C                   MOVEL     VIDCEI        ARBCEI
089930930506     C*
089940950616    1C     ARBDFT        IFGT      0
089950930505     C*
089960930505     C* SE TOTALE FATTURA NON CORRISPONDE AL PRECEDENTE RISTAMPA FATT
089970941209     C     D04IFT        COMP      AR6IFT                             3737
089980930505     C*
089990941212     C                   MOVE      D04ALI        ARBALI
090000941212     C                   MOVE      D04IFT        ARBIFT
090010941212     C                   MOVE      D04IMV        ARBIMV
090020930514     C*
090030970613    2C                   SELECT
090040941221     C     ARBPOR        WHENGT    0
090050941212     C                   Z-ADD     D04POR        ARBPOR
090060941221     C     ARBVA1        WHENGT    0
090070991007     C     ARBSV1        ANDNE     §$2BOL
090080991007     C     ARBSV1        ANDNE     §$2IVA
090090941213     C                   ADD       D04POR        ARBVA1
090100941209     C     ARBVA2        WHENGT    0
090110991007     C     ARBSV2        ANDNE     §$2BOL
090120991007     C     ARBSV2        ANDNE     §$2IVA
090130941213     C                   ADD       D04POR        ARBVA2
090140941209     C     ARBVA3        WHENGT    0
090150941213     C                   ADD       D04POR        ARBVA3
090160970613    2C                   ENDSL
090170941209     C*
090180950616    1C                   ENDIF
090190930511     C*
090200930511     C                   MOVEL     VIDCVR        ARBCVB
090210911218     C*
090220950616    1C     D66FFI        IFEQ      'S'
090230950227     C*
090240950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
090250950616    2C     §3ABSD        IFEQ      ' '
090260991006     C                   OPEN      FIARBT0T
090270991006     C                   WRITE     FIARBTTT
090280991006     C                   CLOSE     FIARBT0T
090290950616    2C                   END
090300941212     C* DISALLOCO OGGETTO MEMBRO PER SEDE
090310941212     C                   EXSR      DLCSED                                       FNARBT0T
090320950616    1C                   END
090330941213     C**
090340941216     C* A G G I O R N O
090350941213     C**
090360051214     c* Semodificato il cod tariffa, verifico se volume automatico
090370051214     c                   if        not *in10 and vidctr<>sav_arbctr
090380051214     C*
090390051214     C                   MOVEL     '7T'          COD
090400051214     C                   MOVEL(P)  ARBFVF        KEY
090410051214     C                   EXSR      CTABEL
090420051214     C                   MOVEL     TBLUNI        DS7T
090430051214     C     §7TRVL        IFEQ      'A'
090440051214     C     §7TRVL        OREQ      'P'
090450051214     c                   movel     'S'           wricvl
090460051214     c                   seton                                        24
090470051214     c                   endif
090480051214     c                   endif
090490051214     c
090500991006     C                   Z-ADD     ARBKSC        AR6KSC
090510991006     C                   Z-ADD     ARBCTR        AR6CTR
090520941213     C                   MOVE      ARBALI        AR6ALI
090530941213     C                   MOVE      ARBIFT        AR6IFT
090540941213     C                   MOVE      ARBIMV        AR6IMV
090550991006     C                   MOVEL     ARBDIV        AR6DIV
090560941213     C                   MOVE      ARBPOR        AR6POR
090570941213     C                   MOVE      ARBSV1        AR6SV1
090580941213     C                   MOVE      ARBVA1        AR6VA1
090590941213     C                   MOVE      ARBSV2        AR6SV2
090600941213     C                   MOVE      ARBVA2        AR6VA2
090610941213     C                   MOVE      ARBSV3        AR6SV3
090620941213     C                   MOVE      ARBVA3        AR6VA3
090630941213     C                   MOVEL     ARBCEI        AR6CEI
090640950104     C                   MOVEL     ARBDFT        AR6DFT
090650950104     C                   MOVEL     ARBNFT        AR6NFT
090660950104     C                   MOVEL     ARBFIV        AR6FIV
090670991013     C                   MOVEL     ARBFIM        AR6FIM
090680941213     C*
090690991006     C* CANCELLO LE VARIE NELL'FIAR7
090700941221    1C     WAR7          IFEQ      'E'
090710991006     C                   MOVEL     '1'           WTRC
090720991006     C     KARB2         SETLL     FIAR7000
090730991006     C     KARB2         READE     FIAR7000                               30
090740941213     C*
090750941213    2C     *IN30         DOWEQ     *OFF
090760991006     C                   DELETE    FIAR7000
090770991006     C     KARB2         READE     FIAR7000                               30
090780941213    2C                   ENDDO
090790941213    1C                   ENDIF
090800941213     C*
090810941213     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
090820941213    1C     WSV(4)        IFNE      ' '
090830941213     C                   Z-ADD     4             C
090840991006     C                   CLEAR                   FIAR7000
090850941213     C                   MOVEL     ARBAAS        AR7AAS
090860941213     C                   MOVEL     ARBLNP        AR7LNP
090870941213     C                   MOVEL     ARBNRS        AR7NRS
090880941213     C                   MOVEL     ARBNSP        AR7NSP
090890991006     C                   MOVEL     '1'           AR7TRC
090900941213    2C     WSV(C)        DOWNE     ' '
090910941213     C                   MOVEL     WSV(C)        AR7SVN
090920941213     C                   MOVEL     WVA(C)        AR7VAN
090930991006     C                   WRITE     FIAR7000
090940941213     C                   ADD       1             C
090950941213    2C                   ENDDO
090960941213    1C                   ENDIF
090970941213     C**
090980941216     C* SE HO ELIMINATO TUTTA LA TASSAZIONE, PER S.F., DELETO RECORD
090990950616    1C     AR6DFT        IFEQ      0
091000941216     C     AR6SV1        ANDEQ     ' '
091010941216     C     AR6SV2        ANDEQ     ' '
091020941216     C     AR6SV3        ANDEQ     ' '
091030941216     C     AR6POR        ANDEQ     0
091040991116     C     AR6CEI        ANDEQ     ' '
091050941216     C*
091060950616    2C     WAR6          IFEQ      'E'
091070041027     C     *IN10         ifeq      *OFF
091080991006     C                   MOVEL     '1'           WTRC
091090991006     C     KARB2         DELETE    FIAR6000                           30
091100991116     C                   ELSE
091110041027     C* SE variaz, in ARRIVO DEVO UGUALMENTE tenere
091120991116     C* FIAR6000 (SERVE PER LA PROCEDURA DEI DANNI)
091130991116     C                   UPDATE    FIAR6000
091140991116     C                   ENDIF
091150991116   X2C                   ELSE
091160041027     C* SE NON ESISTE RECORD TASSAZIONE scrivo record se var.in arrivo
091170041027     C     *IN10         ifeq      *ON
091180991116     C                   MOVEL     ARBAAS        AR6AAS
091190991116     C                   MOVEL     ARBLNP        AR6LNP
091200991116     C                   MOVEL     ARBNRS        AR6NRS
091210991116     C                   MOVEL     ARBNSP        AR6NSP
091220991116     C                   WRITE     FIAR6000
091230041027    3C                   ENDIF
091240041027    2C                   ENDIF
091250941216     C*
091260941216     C                   ELSE
091270941216     C*
091280950616    2C     WAR6          IFEQ      'E'
091290991006     C                   UPDATE    FIAR6000
091300941209     C                   ELSE
091310941209     C                   MOVEL     ARBAAS        AR6AAS
091320941209     C                   MOVEL     ARBLNP        AR6LNP
091330941209     C                   MOVEL     ARBNRS        AR6NRS
091340941209     C                   MOVEL     ARBNSP        AR6NSP
091350991006     C                   WRITE     FIAR6000
091360950616    2C                   ENDIF
091370950616    1C                   ENDIF
091380941209     C*
091390151016     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
091400151016     c*  per la fatturazione se inserito o variato totale imponibile
091410151016     c                   if        (ar6imv>0 and ar6imv<>savimv_fat) or
091420151016     c                             ar6imv=0
091430151118     c                   exsr      regpesivol
091440151016     c                   endif
091450950113     C*
091460950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
091470950113     C*  SE VOLUME AUTOMATICO O PRESUNTO PER PRECEDENTE VARIAIZONE
091480950113     C*  MITTENTE
091490950616    1C     *IN24         IFEQ      *ON
091500051214     c* Mi salvo i campi che userò per la storicizzazione
091510051214     C                   MOVEL     sav_arbctr    MITCTR
091520051214     C                   MOVEL     ARBFVF        MITFVF
091530051214     C                   MOVEL     ARBVLF        MITVLF
091540051214     c
091550950113     C                   EXSR      CALVOL
091560070907     C**                 MOVEL     'I2'          D48CVB
091570070907     C**                 MOVEL     'E'           WESEG
091580070907     c                   add       1             kk
091590070907     c                   movel     'I2'          kcvb(kk)
091600070911     c                   movel     'V'           kffr(kk)
091610070911     c                   movel     ' '           kf12(kk)
091620070911     c****               clear                   w48f12
091630950114     C* AGGIORNO GIA' IL VOLUME TANTO PASSO DALLA VIDEATA DEL VOLUME
091640950114     C*  COMUNQUE PER LA TRASMISSIONE
091650950114     C                   MOVEL     D18FVF        ARBFVF
091660950114     C                   MOVEL     D18VLF        ARBVLF
091670070911     C*****              MOVEL     'V'           D48FFR
091680950616     C                   END
091690911211     C*
091700950114     C   10              UPDATE    FNARB000
091710950114     C  N10              UPDATE    FNBLP000
091720950114     C*
091730041027  1b c***                if        wstessogas = 'S' and (d66ffi = 'E' or
091740041027     c***                          d66ffi = 'P') and *in10 = *off
091750041027  2b c***                if        sav_vidias <> vidias or sav_vidvas <> vidvas
091760041027     c***                          or
091770041027     c***                          sav_vidvmd <> vidvmd or sav_vidvad <> vidvad
091780041027     c***                          or
091790041027     c***                          sav_vidqft <> vidqft
091800041027     c***                exsr      Agg_ARB
091810041027  2e c***                endif
091820041027  1e c***                endif
091830041027     c
091840041027     c* Aggiorno altra bolla se c'e' e variati i campi sopra
091850041027     c                   if        wsiaggio='S' and waltrabolla=' '
091860041027     c* non aggiono mai il ksc:
091870041027     c* in  partenza perchè il S.F. va in ar6
091880041027     c*  in arrivo perchè il cod mittente se cambiato viene impostato
091890041027     c*  in regis8
091900041027     C   10              except    arbTblp
091910041027     C  N10              except    arbTarb
091920041027     c                   endif
091930130411
091940130411     c* Richiamo routine per invio variazione a pda
091950130411     c                   exsr      sr_pda
091960911212     C     ENDRG3        ENDSR
091970911211     C*
091980970613     C* SE BOLLA SU DISTINTA CHIUSA NON FACCIO NULLA ALTRIMENTI ------*
091990970613     C*  CHIUDO LA BOLLA
092000970613     C     CHIUBO        BEGSR
092010970613     C                   MOVEL     'S'           WAGG              1
092020970613     C*
092030970613     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
092040970613     C*  UN NUMERO DISTINTA ED E' CHIUSA
092050080516     c                   clear                   fvvfcf
092060970613     C     ARBNDC        IFGT      0
092070020911     c                   Z-Add     ArbIfp        Kfgs
092080970613     C                   Z-ADD     ARBNDC        W0050             5 0
092090020911     C     KFVV          CHAIN     FNFVV01L                           30
092100970613     C     *IN30         IFEQ      *OFF
092110970613     C     FVVFCF        ANDEQ     'S'
092120970613     C                   CLEAR                   WAGG
092130970613     C                   ENDIF
092140970613     C                   ENDIF
092150970613     C*
092160970613    4C     WAGG          IFEQ      'S'
092170970613    5C     ARBNMI        IFEQ      0
092180970613     C                   Z-ADD     8888888       ARBNMI
092190970613     C                   Z-ADD     8888888       ARBNDC
092200970613     C                   Z-ADD     DATEU         ARBDDC
092210080516     c                   else
092220080516     c
092230080516     c                   if        arbndc>0 and fvvfcf<>'S'
092240080516     C                   Z-ADD     8888888       ARBNDC
092250080516     c                   endif
092260080516     c
092270970613    5C                   ENDIF
092280970613    4C                   ENDIF
092290970613     C                   ENDSR
092300970613     C*
092310911211     C*--- CONTROLLI FORMATO4 ----------------------------------------*
092320911211     C     CONTR4        BEGSR
092330040123     C**!!!              MOVEL     ' '           CEISV3
092340920319     C                   EXSR      CTRCOM
092350920319     C   90              GOTO      ENDCT4
092360920319     C*
092370911211     C* T A S S A Z I O N E
092380991011     C     *INKN         IFEQ      *ON
092390991011     c* fattura immediata
092400991011     C     *IN14         OREQ      *ON
092410991011     C                   MOVEL     '2'           WBOL              1
092420991011     C                   EXSR      TASSAZ
092430991011     C   KN
092440991011     COR 90              GOTO      ENDCT4
092450991011     C                   END
092460911211     C*
092470990930     C* OBBLIGATORIO IMPORTO DELLA VARIA
092480040123     C**!!!VI2VA1        IFEQ      0
092490040123     C**!!!              MOVEL     MSG(51)       VIDMSG
092500040123     C**!!!              SETON                                        519028
092510040123     C**!!!              GOTO      ENDCT4
092520040123     C**!!!              END
092530991007     C* Se divisa sprotetta significa che manca tariffa: in questo caso
092540991007     C* controllo che la divisa inserita sia = alla divisa della moneta
092550991007     C*  di conto SE CLIENTE  9999
092560991007     C     *IN14         IFEQ      *ON
092570991007     C     *IN07         ANDEQ     *OFF
092580991007     C     VI2DIV        ANDNE     §GEDCN
092590991007     C     VIDKSC        ANDEQ     9999
092600991007     C                   SETON                                        459028
092610991007     C                   MOVEA     MSG(90)       K78
092620991007     C                   MOVEA     §GEDCN        K78(71)
092630991007     C                   MOVEA     K78           VIDMSG
092640991007     C                   GOTO      ENDCT4
092650991007     C                   ENDIF
092660991006     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
092670100120     C                   CLEAR                   dlv1601
092680100120     C                   CLEAR                   Fnlv16DS
092690991006     C                   CLEAR                   DTASV
092700991006     C                   MOVEL     'A'           D17TBO
092710991006     C                   MOVEL     VIDKLP        D17KSC
092720991006     C                   MOVE      VIDKSC        D17KSC
092730991012     C     DFT2BO        IFGT      0
092740991012     C                   Z-ADD     DFT2BO        D17DSP
092750991012     C                   ELSE
092760991006     C                   Z-ADD     ARBDSP        D17DSP
092770991012     C                   ENDIF
092780991006     C                   MOVEL     ARBLNP        D17LNP
092790991006     C                   MOVEL     ARBLNA        D17LNA
092800100120     C                   MOVEL     §3ATB2        D17TBL
092810991006     C                   MOVEL     §3ARBL        D17RBL
092820020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
092830040123     C                   MOVEL     §3ASVA        §d17sva
092840040123      * passo che devo controllare la seconda bolla
092850040123     c                   Eval      §d17Tb2 = 'S'
092860100120
092870100120     c* Se si tratta di ffattura immediata , passo cod iso ddestintario
092880100120    1c                   if        *in14
092890100120     c                   Eval      §d17ctriso='S'
092900100120    2c                   if        arbcpi<>*blanks
092910100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
092920100120     c* solo numeri
092930100120     c                   select
092940100120    3c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
092950100120     c                   eval      §d17iso='IT'
092960100120     c* "PRIVATO" senza iso
092970100120     c                   when      *in30 and posiz<=3
092980100120     c                   eval      §d17iso='$$'
092990100120     c                   other
093000100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
093010100120    3c                   ENDSL
093020100120    2c                   endif
093030100120    1c                   endif
093040100120     c
093050040123     c                   Movel     dlv1601       D17Flo
093060100120     c
093070991006     C                   MOVEL     VI2DIV        D17DIV
093080991006     C                   MOVEL     WDIV          D17DVP
093090991006    1C     §3AFCA        IFEQ      2
093100991006     C                   MOVEL     'S'           D17FCA
093110991006    1C                   ENDIF
093120991006     C                   MOVEL     'E'           D17FIM
093130991006     C                   MOVEL     VI2CEI        D17CEI
093140991006     C                   MOVEL     VI2SV1        VSV(1)
093150040123     C**!!!              MOVEL     VI2VA1        VVA(1)
093160040123     c                   Z-add     Vi2Va1        vva(1)
093170040123     c                   Movel     Vi2Es1        ves(1)
093180040123     c                   Movel     Vi2Sv2        vsv(2)
093190040123     c                   Movel     Vi2Es2        ves(2)
093200040123     c                   Z-add     Vi2Va2        vva(2)
093210040123     c                   MOVEL     Vi2Sv3        vsv(3)
093220040123     c                   Z-add     Vi2Va3        vva(3)
093230040123     c                   Movel     Vi2Es3        ves(3)
093240040123     c                   MOVEL     Vi2Sv4        vsv(4)
093250040123     c                   Z-add     Vi2Va4        vva(4)
093260040123     c                   Movel     Vi2Es4        ves(4)
093270041130     c                   MOVEL     Vi2Sv5        vsv(5)
093280041130     c                   Z-add     Vi2Va5        vva(5)
093290041130     c                   Movel     Vi2Es5        ves(5)
093300041130     c                   MOVEL     Vi2Sv6        vsv(6)
093310041130     c                   Z-add     Vi2Va6        vva(6)
093320041130     c                   Movel     Vi2Es6        ves(6)
093330991006     C                   CALL      'FNLV16R'
093340030612     C                   PARM                    Fnlv16DS
093350991006     C                   PARM                    DTASV
093360991011     C                   MOVEL     D17DIV        VI2DIV
093370991006     C                   MOVEL     D17CEI        VI2CEI
093380991011     C                   MOVEL     O17DEI        DESCEI
093390040123     C**!!!              MOVEL     VSV(1)        VI2SV1
093400040123     C**!!!              MOVEL     VES(1)        CEISV3            1
093410040123      * Ricarico le varie
093420040123     c                   Movel     Vsv(1)        Vi2Sv1
093430040123     c                   Z-add     Vva(1)        Vi2Va1
093440040123     c                   Movel     Ves(1)        Vi2Es1
093450040123     c                   Movel     Vsv(2)        Vi2Sv2
093460040123     c                   Z-add     Vva(2)        Vi2Va2
093470040123     c                   Movel     Ves(2)        Vi2Es2
093480040123     c                   Movel     Vsv(3)        Vi2Sv3
093490040123     c                   Z-add     Vva(3)        Vi2Va3
093500040123     c                   Movel     Ves(3)        Vi2Es3
093510040123     c                   Movel     Vsv(4)        Vi2Sv4
093520040123     c                   Z-add     Vva(4)        Vi2Va4
093530040123     c                   Movel     Ves(4)        Vi2Es4
093540041130     c                   Movel     Vsv(5)        Vi2Sv5
093550041130     c                   Z-add     Vva(5)        Vi2Va5
093560041130     c                   Movel     Ves(5)        Vi2Es5
093570041130     c                   Movel     Vsv(6)        Vi2Sv6
093580041130     c                   Z-add     Vva(6)        Vi2Va6
093590041130     c                   Movel     Ves(6)        Vi2Es6
093600040123      * Controllo se caricata almeno una varia
093610041130do  1c                   Do        6             c
093620040123     c                   If        vsv(c) <> *Blanks
093630040123     c                   Eval      wesvar1 = 'S'
093640040123     c                   EndIf
093650040123    1c                   EndDo
093660040123
093670991006     C     O17ERR        IFNE      *BLANKS
093680991006     C                   SETON                                        90
093690991006     C     O17MSG        IFNE      *BLANKS
093700991006     C                   MOVEL     O17MSG        VIDMSG
093710991006     C                   SETON                                        28
093720991006     C                   ENDIF
093730991006     C     O17ERR        IFEQ      '6'
093740040123     c                   Select
093750040123     c                   When      O17Erv = 1
093760991006     C                   SETON                                        49
093770040123     c                   When      O17Erv = 2
093780040123     C                   SETON                                        63
093790040123     c                   When      O17Erv = 3
093800040123     C                   SETON                                        64
093810040123     c                   When      O17Erv = 4
093820040123     C                   SETON                                        65
093830041130     c                   When      O17Erv = 5
093840041130     C                   SETON                                        66
093850041130     c                   When      O17Erv = 6
093860041130     C                   SETON                                        67
093870040123     c                   EndSl
093880991006     C                   ENDIF
093890991006     C     O17ERR        IFEQ      '1'
093900991006     C                   SETON                                        50
093910991006     C                   ENDIF
093920991006     C     O17ERR        IFEQ      '7'
093930991006     C                   SETON                                        45
093940991006     C                   ENDIF
093950991006     C   90              GOTO      ENDCT4
093960991006     C                   ENDIF
093970991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
093980991006    1C  N07VI2DIV        IFNE      WDIV
093990991006     C* CONVERSIONE VARIA
094000991006    3C     VI2VA1        IFEQ      VA2VA1
094010991006     C     VI2VA1        ANDGT     *ZEROS
094020991006     C     WDIV          ANDNE     *BLANKS
094030030612     C                   CLEAR                   YeurcoDS
094040991006     C                   MOVEL     WDIV          YECDVI
094050991006     C                   Z-ADD     VI2VA1        YECIMI
094060991006     C                   MOVEL     VI2DIV        YECDVO
094070991006     C     O17FDC        IFEQ      'S'
094080011114     C                   MOVE      '3'           YECDCO
094090991006     C                   ELSE
094100991006     C                   CLEAR                   YECDCO
094110991006     C                   ENDIF
094120991006     C                   CALL      'YEURCO'
094130030612     C                   PARM                    YeurcoDS
094140991006     C                   Z-ADD     YECIMO        VI2VA1
094150991012     C                   SETON                                        51
094160991006    3C                   ENDIF
094170040123      * Conversione Varia 2
094180040123    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
094190040123     c                             wdiv <> *Blanks
094200040123     c                   Clear                   Yeurcods
094210040123     c                   Movel     wdiv          YECDVI
094220040123     c                   Z-add     Vi2Va2        YECIMI
094230040123     c                   Movel     Vi2Div        YECDVO
094240040123     c                   If        O17Fdc = 'S'
094250040123     c                   Movel     '3'           YECDCO
094260040123     c                   EndIf
094270040123     c                   Call      'YEURCO'
094280040123     c                   Parm                    Yeurcods
094290040123     c                   Z-add     YECIMO        Vi2Va2
094300040123     c                   Eval      *In63 = *On
094310040123    2c                   EndIf
094320040123      * Conversione Varia 3
094330040123    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
094340040123     c                             wdiv <> *Blanks
094350040123     c                   Clear                   yeurcods
094360040123     c                   Movel     wdiv          YECDVI
094370040123     c                   Z-add     Vi2Va3        YECIMI
094380040123     c                   Movel     Vi2Div        YECDVO
094390040123     c                   If        O17Fdc = 'S'
094400040123     c                   Movel     '3'           YECDCO
094410040123     c                   EndIf
094420040123     c                   Call      'YEURCO'
094430040123     c                   Parm                    yeurcods
094440040123     c                   Z-add     YECIMO        Vi2Va3
094450040123     c                   Eval      *In64 = *On
094460040123    2c                   EndIf
094470040123      * Conversione Varia 4
094480040123    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
094490040123     c                             wdiv <> *Blanks
094500040123     c                   Clear                   yeurcods
094510040123     c                   Movel     wdiv          YECDVI
094520040123     c                   Z-add     Vi2Va4        YECIMI
094530040123     c                   Movel     Vi2Div        YECDVO
094540040123     c                   If        O17Fdc = 'S'
094550040123     c                   Movel     '3'           YECDCO
094560040123     c                   EndIf
094570040123     c                   Call      'YEURCO'
094580040123     c                   Parm                    yeurcods
094590040123     c                   Z-add     YECIMO        Vi2Va4
094600040123     c                   Eval      *In65 = *On
094610040123    2c                   EndIf
094620041130      * Conversione Varia 5
094630041130    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
094640041130     c                             wdiv <> *Blanks
094650041130     c                   Clear                   yeurcods
094660041130     c                   Movel     wdiv          YECDVI
094670041130     c                   Z-add     Vi2Va5        YECIMI
094680041130     c                   Movel     Vi2Div        YECDVO
094690041130     c                   If        O17Fdc = 'S'
094700041130     c                   Movel     '3'           YECDCO
094710041130     c                   EndIf
094720041130     c                   Call      'YEURCO'
094730041130     c                   Parm                    yeurcods
094740041130     c                   Z-add     YECIMO        Vi2Va5
094750041130     c                   Eval      *In66 = *On
094760041130    2c                   EndIf
094770041130      * Conversione Varia 6
094780041130    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
094790041130     c                             wdiv <> *Blanks
094800041130     c                   Clear                   yeurcods
094810041130     c                   Movel     wdiv          YECDVI
094820041130     c                   Z-add     Vi2Va6        YECIMI
094830041130     c                   Movel     Vi2Div        YECDVO
094840041130     c                   If        O17Fdc = 'S'
094850041130     c                   Movel     '3'           YECDCO
094860041130     c                   EndIf
094870041130     c                   Call      'YEURCO'
094880041130     c                   Parm                    yeurcods
094890041130     c                   Z-add     YECIMO        Vi2Va6
094900041130     c                   Eval      *In67 = *On
094910041130    2c                   EndIf
094920040123     C**!!!              Z-ADD     VI2VA1        VA2VA1
094930040123     C**!!!              Z-ADD     VI2VA1        VVA(1)
094940040123     C**!!!              MOVEL     VI2DIV        WDIV
094950991012     C* RIEMETTO COMUNQUE LA VIDEATA SE HANNO VARIATO LA DIVISA
094960991012     C* (POTREI RIEMETTERLA SOLO SE HO CONVERTITO IMPORTI MA LA
094970991012     C* RIEMETTO COMUNQUE PER FARE COME PER LA PRIMA BOLLA)
094980991012     C                   SETON                                        90
094990040123
095000040123     c                   Z-add     Vi2Va1        Va2Va1
095010040123     c                   Z-add     Vi2Va2        Va2Va2
095020040123     c                   Z-add     Vi2Va3        Va2Va3
095030040123     c                   Z-add     Vi2Va4        Va2Va4
095040041130     c                   Z-add     Vi2Va5        Va2Va5
095050041130     c                   Z-add     Vi2Va6        Va2Va6
095060040123     c                   Z-add     Vi2Va1        Vva(1)
095070040123     c                   Z-add     Vi2Va2        Vva(2)
095080040123     c                   Z-add     Vi2Va3        Vva(3)
095090040123     c                   Z-add     Vi2Va4        Vva(4)
095100041130     c                   Z-add     Vi2Va5        Vva(5)
095110041130     c                   Z-add     Vi2Va6        Vva(6)
095120040123     c                   Movel     Vi2Div        Wdiv
095130040123
095140991012     C                   GOTO      ENDCT4
095150991006   X1C                   ELSE
095160991006     C                   Z-ADD     VI2VA1        VA2VA1
095170040123     c                   Z-add     Vi2Va2        Va2Va2
095180040123     c                   Z-add     Vi2Va3        Va2Va3
095190040123     c                   Z-add     Vi2Va4        Va2Va4
095200041130     c                   Z-add     Vi2Va5        Va2Va5
095210041130     c                   Z-add     Vi2Va6        Va2Va6
095220991006    1C                   ENDIF
095230011206      *
095240011206      * Controllo che la somma degli importi immessi non superi il valore
095250011206      * impostato in §GEIMF
095260040123     c**!!!              eval      w0173 = vi2va1
095270040123     c                   xfoot     vva           w0173
095280011206      *      controllo se devo convertire il totale imponibile
095290011206  1b c                   if        vi2div <> §gedcn
095300030612     c                   clear                   YeurcoDS
095310011206     c                   eval      yecdvi = vi2div
095320011206     c                   eval      yecdvo = §gedcn
095330040123     c**!!!              eval      yecimi = vi2va1
095340040123     c                   eval      yecimi = w0173
095350011206     c                   move      decsav        yecdco
095360011206     c                   call      'YEURCO'
095370030612     c                   parm                    YeurcoDS
095380011206  2b c                   if        yecesi = ' '
095390011206     c                   eval      w0173 = yecimo
095400011206  2e c                   endif
095410011206  1e c                   endif
095420011206      *      controllo se l'imponibile supera il limite fatturabile
095430011220  1b c                   if        w0173 > imfsav
095440011206     c                   eval      vidmsg = msg(110)
095450011220     c                   eval      %subst (vidmsg:45:14) = (%editc(imfsav: '4'))
095460011206     c                   eval      %subst (vidmsg:60:3) = §gedcn
095470040123     c**!!!              eval      *in51 = *on
095480011206     c                   eval      *in90 = *on
095490011206     c                   goto      endct4
095500011206  1e c                   endif
095510040123
095520040123     c                   Z-add     w0173         TotImv
095530920319     C*
095540911211     C     ENDCT4        ENDSR
095550911212     C*
095560911212     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE 2X -----------------*
095570911212     C     REGIS4        BEGSR
095580040130     c                   Setoff                                       3709
095590130412     c                   clear                   wpda_ndc
095600130412     c                   clear                   wpda_ddc
095610130412     c                   clear                   wpda_dcm
095620040130
095630950104     C*  GIRO UDATE
095640950104     C                   TIME                    W0140            14 0
095650950104     C* UDATE IN GGMMAAAA
095660950104     C                   MOVE      W0140         WDTGIO            8 0
095670950104     C*
095680950104     C* UDATE IN AAAAMMGG
095690950104     C                   Z-ADD     WDTGIO        G02DAT
095700950104     C                   MOVEL     *BLANK        G02ERR
095710950104     C                   CALL      'XSRDA8'
095720950104     C                   PARM                    WLBDAT
095730950104     C                   MOVEL     G02INV        DATEU             8 0
095740941221     C* PRENDO I VALORI ASSOLUTI
095750941221     C                   MLLZO     1             VIDKLP
095760941221     C                   MLLZO     1             VIDKSC
095770941221     C                   MLLZO     1             VI2VA1
095780040123     C                   MLLZO     1             VI2VA2
095790040123     C                   MLLZO     1             VI2VA3
095800040123     C                   MLLZO     1             VI2VA4
095810041130     C                   MLLZO     1             VI2VA5
095820041130     C                   MLLZO     1             VI2VA6
095830941221     C*
095840950203     C                   MOVEL     KNMUS         ARBPRU
095850920120     C* INVIO PER SEDE
095860991007     C                   MOVEL     'FIARBT0T'    SFILE             8
095870930210     C* ALLOCO OGGETTI
095880040123     C                   EXSR      ALLOC
095890930210     C   91              GOTO      ENDRG4
095900040123      * carico le varie in una sk di comodo
095910040123     c                   ExSr      CarWar1
095920040123      * se c'è n.fattura calcolo
095930040129If  1c                   If        Dft2bo > *Zeros
095940040129     c                   Movel     Dft2bo        ArbDft
095950040123     C                   MOVEL     VI2DIV        ARBDIV
095960040129     C                   MOVEL     VI2DIV        divgei
095970040123     C                   EXSR      RIEGEI
095980040123     C                   CLEAR                   Fnlv04ds
095990040123     C                   MOVEL     'T'           D04FIM
096000040123     C                   Z-ADD     ArbDft        D04DFT
096010040123     C                   MOVEL     §GEAED        D04AED
096020040123     C                   Z-ADD     §GEAVL        D04VAR
096030040123     c                   Z-add     TotImv        D04Imv
096040040123     C                   MOVEL     VI2CEI        D04CEI
096050040123     C                   MOVEL     AR9TIC        D04TIC
096060040123     C                   MOVEL     AR9VCA        D04VCA
096070040123     C                   Z-ADD     AR9CAS        D04CAS
096080040123     C                   MOVEL     '2'           D04SPE
096090040123     C                   MOVEL     §3ATB2        D04TBL
096100040123     C                   MOVEL     VI2DIV        D04DIV
096110040123     C                   MOVEL     '1'           UNO
096120040123     C*
096130040123     C                   CALL      'FNLV04R'
096140040123     C                   PARM                    Fnlv04ds
096150040123     C                   PARM                    DTASV
096160040123     C*
096170040123     C     D04BOL        IFGT      *ZEROS
096180040123     C                   ADD       1             C
096190040123     C                   MOVEL     §$2BOL        WSV(C)
096200040123     C                   Z-ADD     D04BOL        WVA(C)
096210040123     C                   ENDIF
096220040123     C     D04IVA        IFGT      *ZEROS
096230040123     C                   ADD       1             C
096240040123     C                   MOVEL     §$2IVA        WSV(C)
096250040123     C                   Z-ADD     D04IVA        WVA(C)
096260040123     C                   ENDIF
096270040123    1c                   EndIf
096280040123
096290040123     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
096300040123    1C     C             IFGT      0
096310040123     C                   SETON                                        09
096320040123     C*
096330040123     C* INVIO PER SEDE
096340040123     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
096350040123    2C     C             IFGT      3
096360040123     C* MI SALVO I CAMPI DELL'FIARBT
096370040123     C                   MOVEL     SOVR(1)       SOARBT
096380040123     C                   MOVEL     SALC(1)       SAARBT
096390040123     C                   MOVEL     SDLC(1)       SDARBT
096400040123     C*
096410040123     C                   MOVEL     'FIARBU0T'    SFILE
096420040123     C* ALLOCO OGGETTI
096430040123     C                   EXSR      ALLOC
096440070515     c                   if        *in91
096450070515     C                   MOVEL     SDARBT        SDLC(1)
096460070515     c                   exsr      DLCSED
096470070525     C                   SETON                                        2891
096480070515     C                   GOTO      ENDRG4
096490070515     c                   endif
096500070515     c
096510040123    2C                   ENDIF
096520040123    1C                   ENDIF
096530911212     C*
096540991007     C                   Z-ADD     DATEU         ARBDTV
096550991007     C                   TIME                    ARBORV
096560991007     C                   MOVEL     VIDCVR        ARBCVB
096570991011     C                   MOVEL     '2'           ARBTRC
096580991011     C                   MOVEL     '2'           AR6TRC
096590991007     C                   CLEAR                   ARBFST
096600991007     C                   CLEAR                   ARBDDS
096610991007     C                   CLEAR                   ARBQFT
096620991007     C                   CLEAR                   ARBVMD
096630991007     C                   CLEAR                   ARBVAD
096640991007     C                   CLEAR                   ARBIAS
096650991007     C                   CLEAR                   ARBVAS
096660911212     C*
096670941212     C* SE DA STORICIZZARE --> STORICIZZO
096680991007     C     WAR62         IFEQ      'E'
096690060130    1C     D66FSA        ANDEQ     'S'
096700060130     C**   *IN10         IFEQ      *ON
096710060130    1C**   D66FSA        ANDEQ     'S'
096720060130     C**   *IN10         OREQ      *OFF
096730060130    1C**   D66FSP        ANDEQ     'S'
096740991007     C                   EXSR      STOARB
096750941212     C                   END
096760060130     C**                 END
096770060302     c* storicizzazione motivazione
096780060302     c     d66mtv        ifeq      'S'
096790060302     c                   exsr      sto_mtv
096800060302     c                   endif
096810040123     C**
096820040123     C* INVIO VARIAZIONE --> FIARBU0T
096830040123     C**
096840040123    1C     *IN09         IFEQ      *ON
096850040123     C     C             ANDGT     3
096860040123     C                   Z-ADD     4             C
096870040123     C*
096880040123    3C     D66FFI        IFEQ      'S'
096890040123     C                   Z-ADD     4             C
096900040123     C                   OPEN      FIARBU0T
096910040123     C*
096920040123     C* NE SCRIVO FIN TANTO CHE CE NE SONO
096930040123    4C     WSV(C)        DOWNE     ' '
096940040123     C                   MOVEL     WSV(C)        ARBSVN
096950040123     C                   Z-ADD     WVA(C)        ARBVAN
096960040123     C                   WRITE     FIARBUTT
096970040123     C                   ADD       1             C
096980040123    4C                   ENDDO
096990040123     C*
097000040123     C                   CLOSE     FIARBU0T
097010040123     C* DISALLOCO OGGETTO MEMBRO PER SEDE
097020040123     C                   EXSR      DLCSED                                       FIARBU0T
097030040123    3C                   ENDIF
097040040123     C* REIMPOSTO I CAMPI
097050040123     C                   MOVEL     SOARBT        SOVR(1)
097060040123     C                   MOVEL     SAARBT        SALC(1)
097070040123     C                   MOVEL     SDARBT        SDLC(1)
097080040123    1C                   ENDIF
097090991011     C*
097100991011     C                   MOVEL     VIDKLP        ARBKSC
097110991007     C                   MOVE      VIDKSC        ARBKSC
097120991007     C                   CLEAR                   ARBPOR
097130991007     C                   MOVEL     VI2DIV        ARBDIV
097140040123     C**!!!              MOVEL     VI2SV1        ARBSV1
097150040123     C**!!!              MOVEL     VI2VA1        ARBVA1
097160040123     C                   MOVEL     WSV(1)        ARBSV1
097170040123     C                   Z-ADD     WVA(1)        ARBVA1
097180040123     C                   MOVEL     WSV(2)        ARBSV2
097190040123     C                   Z-ADD     WVA(2)        ARBVA2
097200040123     C                   MOVEL     WSV(3)        ARBSV3
097210040123     C                   Z-ADD     WVA(3)        ARBVA3
097220991007     C                   MOVEL     VI2CEI        ARBCEI
097230991007     C                   MOVEL     VIDCTR        ARBCTR
097240040123     C**!!!              Z-ADD     VI2VA1        ARBIMV
097250040123     C                   Z-ADD     TotImv        ARBIMV
097260040123     C**!!!              CLEAR                   ARBSV2
097270040123     C**!!!              CLEAR                   ARBVA2
097280040123     C**!!!              CLEAR                   ARBSV3
097290040123     C**!!!              CLEAR                   ARBVA3
097300040129     C**!!!              CLEAR                   ARBALI
097310040129     C**!!!              CLEAR                   ARBIFT
097320991007     C                   Z-ADD     VI2NFT        ARBNFT
097330991007     C                   Z-ADD     VI2FIV        ARBFIV
097340991007     C                   Z-ADD     AR6DFT        ARBDFT
097350991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
097360991013     C* STATO MODIFICATO TOLGO LA 'P'
097370991013     C     AR6FIM        IFEQ      'P'
097380991013     C     ARBIMV        ANDNE     AR6IMV
097390991013     C     *IN10         ANDEQ     *ON
097400991013     C                   MOVE      *BLANKS       ARBFIM
097410991013     C                   ELSE
097420991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
097430991013     C                   MOVE      AR6FIM        ARBFIM
097440991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
097450991013     C     WAR62         IFNE      'E'
097460991013     C     *IN10         ANDEQ     *OFF
097470991013     C                   MOVEL     'P'           ARBFIM
097480991013     C                   END
097490991013     C                   ENDIF
097500040129
097510040130if  1c                   If        Arbdft > 0
097520040129     C* SE CAMBIATO TOTALE FATTURA RISTAMPO IN AUTOMATICO LA BOLLA
097530040129     C     D04IFT        COMP      AR6IFT                             3737
097540040129
097550040129     C                   MOVE      D04ALI        ARBALI
097560040129     C                   Z-ADD     D04IFT        ARBIFT
097570040130     C                   z-add     D04IMV        ARBIMV
097580051111     c                   select
097590051111     C     ARBVA1        WHENGT    0
097600051111     C     ARBSV1        ANDNE     §$2BOL
097610051111     C     ARBSV1        ANDNE     §$2IVA
097620051111     C                   ADD       D04POR        ARBVA1
097630051111     C     ARBVA2        WHENGT    0
097640051111     C     ARBSV2        ANDNE     §$2BOL
097650051111     C     ARBSV2        ANDNE     §$2IVA
097660051111     C                   ADD       D04POR        ARBVA2
097670051111     C     ARBVA3        WHENGT    0
097680051111     C                   ADD       D04POR        ARBVA3
097690051111    3C                   ENDSL
097700040130     c                   EndIf
097710911212     C*
097720070503     C     D66FFI        IFEQ      'S'
097730991007     C                   OPEN      FIARBT0T
097740991007     C                   WRITE     FIARBTTT
097750991007     C                   CLOSE     FIARBT0T
097760911212     C* DISALLOCO OGGETTO
097770991007     C                   EXSR      DLCSED                                       FIARBT0T
097780911212     C                   END
097790911212     C*
097800911212     C* AGGIORNO
097810991013     C                   Z-ADD     ARBKSC        AR6KSC
097820991013     C                   Z-ADD     ARBCTR        AR6CTR
097830991013     C                   MOVEL     ARBDIV        AR6DIV
097840991013     C                   Z-ADD     ARBPOR        AR6POR
097850991013     C                   MOVEL     ARBSV1        AR6SV1
097860991013     C                   MOVEL     ARBSV2        AR6SV2
097870991013     C                   MOVEL     ARBSV3        AR6SV3
097880991013     C                   Z-ADD     ARBVA1        AR6VA1
097890991013     C                   Z-ADD     ARBVA2        AR6VA2
097900991013     C                   Z-ADD     ARBVA3        AR6VA3
097910991013     C                   Z-ADD     ARBIMV        AR6IMV
097920991013     C                   Z-ADD     ARBALI        AR6ALI
097930991013     C                   MOVEL     ARBCEI        AR6CEI
097940991013     C                   Z-ADD     ARBIFT        AR6IFT
097950991013     C                   Z-ADD     ARBFIV        AR6FIV
097960991013     C                   Z-ADD     ARBNFT        AR6NFT
097970991013     C                   Z-ADD     ARBDFT        AR6DFT
097980991013     C                   MOVEL     ARBFIM        AR6FIM
097990040123     C*
098000040123     C* CANCELLO LE VARIE NELL'FIAR7
098010040123    1C     WAR72         IFEQ      'E'
098020040123     C                   MOVEL     '2'           WTRC
098030040123     C     KARB2         SETLL     FIAR7000
098040040123     C     KARB2         READE     FIAR7000                               30
098050040123     C*
098060040123    2C     *IN30         DOWEQ     *OFF
098070040123     C                   DELETE    FIAR7000
098080040123     C     KARB2         READE     FIAR7000                               30
098090040123    2C                   ENDDO
098100040123    1C                   ENDIF
098110040123     C*
098120040123     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
098130040123    1C     WSV(4)        IFNE      ' '
098140040123     C                   Z-ADD     4             C
098150040123     C                   CLEAR                   FIAR7000
098160040123     C                   MOVEL     ARBAAS        AR7AAS
098170040123     C                   MOVEL     ARBLNP        AR7LNP
098180040123     C                   MOVEL     ARBNRS        AR7NRS
098190040123     C                   MOVEL     ARBNSP        AR7NSP
098200040123     C                   MOVEL     '2'           AR7TRC
098210040123    2C     WSV(C)        DOWNE     ' '
098220040123     C                   MOVEL     WSV(C)        AR7SVN
098230040123     C                   MOVEL     WVA(C)        AR7VAN
098240040123     C                   WRITE     FIAR7000
098250040123     C                   ADD       1             C
098260040123    2C                   ENDDO
098270040123    1C                   ENDIF
098280040123
098290990930     C     WAR62         IFEQ      'E'
098300991007     C                   UPDATE    FIAR6000
098310941212     C                   ELSE
098320991013     C                   CLEAR                   AR6ATB
098330991013     C                   Z-ADD     ARBAAS        AR6AAS
098340991013     C                   Z-ADD     ARBLNP        AR6LNP
098350991013     C                   Z-ADD     ARBNRS        AR6NRS
098360991013     C                   Z-ADD     ARBNSP        AR6NSP
098370991013     C                   MOVEL     '2'           AR6TRC
098380991007     C                   WRITE     FIAR6000
098390941212     C                   ENDIF
098400930211     C*
098410991013     C                   UNLOCK    FNARB01L
098420920204     C* RISTAMPO FATTURA
098430070309     c* ristampo fattura immediata se variata(37 on),
098440070309     c*   e se non attiva GEO
098450130321     C***  *IN37         IFEQ      *ON
098460130321     c***  FGSddaSI      andeq     ' '
098470130321     C***                CLEAR                   FNLSB5DS
098480130321     C***                MOVEL     'V'           DB0RIS
098490130321     C***                MOVEL     D48TBO        DB0TBO
098500130321     C***                MOVEL     ARBAAS        DB0AAS
098510130321     C***                MOVEL     ARBLNP        DB0LNP
098520130321     C***                MOVEL     ARBNRS        DB0NRS
098530130321     C***                MOVEL     ARBNSP        DB0NSP
098540130321     C***                CALL      D90PSL
098550130321     C***                PARM                    FNLSB5DS
098560130321     C***                END
098570130411
098580130411     c                   eval      wpda_ndc=arbndc
098590130411     c                   eval      wpda_ddc=arbddc
098600130411     c                   eval      wpda_dcm=arbdcm
098610130411     c* Richiamo routine per invio variazione a pda
098620130411     c                   exsr      sr_pda
098630911212     C*
098640911212     C     ENDRG4        ENDSR
098650920116     C*
098660920116     C*--- CONTROLLI FORMATO5 ----------------------------------------*
098670920117     C     CONTR5        BEGSR
098680920116     C* CODICE BOLLA NUOVO
098690941102    1C     *IN21         IFEQ      *OFF
098700941102     C*
098710941213     C     '?'           SCAN      NEWCBO                                 90
098720941213    2C     *IN90         IFEQ      *ON
098730941213     C                   MOVEL     CODUT         §KUT
098740920116     C                   MOVEL     '3A'          §COD
098750941102     C                   MOVE      *BLANKS       NEWCBO
098760991011     C                   CLEAR                   §KEY
098770920116     C                   CALL      'X§TABER'
098780920116     C                   PARM                    §KUT
098790920116     C                   PARM                    §COD
098800920116     C                   PARM                    §KEY
098810941209     C                   PARM                    §DES
098820941102     C                   MOVEL     §KEY          NEWCBO
098830941102     C                   MOVEL     §DES          DENCBO
098840920116     C                   SETON                                        90
098850920116     C                   GOTO      ENDCT5
098860941102    2C                   END
098870920116     C*
098880920116     C                   MOVEL     '3A'          COD
098890941102     C                   MOVEL(P)  NEWCBO        KEY
098900941212     C                   EXSR      CTABEL
098910920121     C*
098920941212    2C     *IN30         IFEQ      *ON
098930941102     C                   MOVEL     MSG(16)       VIDMSG
098940941102     C                   SETON                                        409028
098950941102     C                   GOTO      ENDCT5
098960941102    2C                   ENDIF
098970920117     C*
098980920116     C* CODICE NUOVO E VECCHIO NON POSSONO ESSERE UGUALI
098990940614     C* SE PGM RICHIAMATO NON CONTROLLO
099000941212    2C  N20OLDCBO        IFEQ      NEWCBO
099010941102     C                   MOVEL     MSG(17)       VIDMSG
099020941102     C                   SETON                                        409028
099030920116     C                   GOTO      ENDCT5
099040941102    2C                   END
099050920116     C*
099060920116     C                   MOVEL     TBLUNI        DS3A
099070920304     C*
099080941102     C                   MOVEL     §3ADES        DENCBO                          DESCRIZIONE
099090941102     C*
099100920116     C* NON POSSO TRASFORMARE IN BOLLA RECUPERO DA NON RECUPERO E VICEV
099110940322     C* SE RICHIAMATO PERMETTO TUTTO
099120941102    2C     *IN20         IFEQ      *OFF
099130940322     C*
099140941102    3C     §3ARBL        IFEQ      'R'
099150941102     C     SAVRBL        ANDNE     'R'
099160941102     C*
099170941102     C     §3ARBL        ORNE      'R'
099180941102     C     SAVRBL        ANDEQ     'R'
099190941102     C                   MOVEL     MSG(18)       VIDMSG
099200941102     C                   SETON                                        409028
099210920116     C                   GOTO      ENDCT5
099220941102    3C                   END
099230941102    2C                   END
099240920128     C*
099250941102     C* NON POSSO FARE VARIAZIONI DI PORTO
099260941212     C                   MOVEL     §3ATB1        NSCTB1            1
099270941212     C                   MOVEL     SAVTB1        OSCTB1            1
099280920116     C*
099290941102    2C     NSCTB1        IFNE      OSCTB1
099300941102     C                   MOVEL     MSG(19)       VIDMSG
099310941213     C                   SETON                                        409028
099320920128     C                   GOTO      ENDCT5
099330941102    2C                   ENDIF
099340150413     c* non ammesso contrasegno se bolle per EXPO
099350151119     c*                  if        §3afca=1 and (arbtc1='Z' or arbtc2='Z')
099360151119     C*                  MOVEL     MSG(159)      VIDMSG
099370151119     C*                  SETON                                        409028
099380151119     C*                  GOTO      ENDCT5
099390151119     c*                  endif
099400000510     C*
099410000510     C* BOLLE LNP POSTE: SOLO SPEDIZIONI FRANCIO
099420131008     C**   LNPNTW        IFEQ      'PPT'
099430131008     C**   NSCTB1        ANDNE     'F'
099440131008     C**                 MOVEL     MSG(30)       VIDMSG
099450131008     C**                 SETON                                        409028
099460131008     C**                 GOTO      ENDCT5
099470131008     C**                 ENDIF
099480000718     C* BOLLE LNP POSTE CON SERIE : NN POSSO TRASFRMARE IN C/S
099490131008     C**   LNPNTW        IFEQ      'PPT'
099500131008     C**   ARBNRS        ANDGT     0
099510131008     C**   §3ARBL        ANDEQ     'C'
099520131008     C**                 MOVEL     MSG(96)       VIDMSG
099530131008     C**                 SETON                                        409028
099540131008     C**                 GOTO      ENDCT5
099550131008     C**                 ENDIF
099560000925     C* BOLLE LNP POSTE: NON POSSO FARE BOLLE DOPPIE
099570131008     C**   LNPNTW        IFEQ      'PPT'
099580131008     C**   §3ATB2        ANDNE     '  '
099590131008     C**                 MOVEL     MSG(97)       VIDMSG
099600131008     C*+                 SETON                                        409028
099610131008     C**                 GOTO      ENDCT5
099620131008     C**                 ENDIF
099630050414     C* Se prepagato, non posso trasformare in C/S
099640050414     C     §3ARBL        ifeq      'C'
099650050414     c     war6          andeq     'E'
099660050414     c     ar6dft        andgt     0
099670060213     c     d48cvb        andne     'KM'
099680050414     C                   MOVEL     MSG(133)      VIDMSG
099690050414     C                   SETON                                        409028
099700050414     C                   GOTO      ENDCT5
099710050414     C                   ENDIF
099720950407     C*
099730950407     C* MEMORIZZO VARIAZIONE CODICE BOLLA
099740950407     C     NEWCBO        IFNE      SAVCBO
099750950407     C                   MOVE      *BLANKS       COM01A
099760950407     C                   MOVEL     NEWCBO        SAVCBO
099770950407     C                   END
099780941102    1C                   ENDIF
099790920120     C*
099800920121     C* CONTROLLO CONTRASSEGNO
099810941102    1C     §3AFCA        IFEQ      0
099820941213     C     VIDCAS        ANDNE     0
099830941213     C                   MOVEL     MSG(20)       VIDMSG
099840941213     C                   SETON                                        429028
099850941213     C                   GOTO      ENDCT5
099860941213    1C                   ENDIF
099870961204     C* NON POSSO INSERIRE IL C/ASSEGNO PER BOLLE DI RESO
099880961204    1C     §3AFCA        IFGT      0
099890980105     C     ARBFBR        IFEQ      'S'
099900961204     C                   MOVEL     MSG(73)       VIDMSG
099910961204     C                   SETON                                        429028
099920961204     C                   GOTO      ENDCT5
099930961204    1C                   ENDIF
099940980105     C* NON POSSO INSERIRE IL C/A SE BOLLE LEGATA CON LNP=LNA
099950980105     C  N21KARB          CHAIN     FNLBL01L                           30
099960980105    4C  N21*IN30         IFEQ      *OFF
099970980105     C     LBLLAP        ANDEQ     LBLLAN
099980980105     C                   MOVEL     MSG(78)       VIDMSG
099990980105     C                   SETON                                        429028
100000980105     C                   GOTO      ENDCT5
100010980105    1C                   ENDIF
100020980105     C                   ENDIF
100030941102     C*
100040941213     C* SE eliminato importo C/A, elimiare anche tutti i campi relativi
100050011018    1C     VIDCAS        IFEQ      0
100060011018    2C     VIDTIC        IFNE      *BLANKS
100070011018     C     VIDGCA        ORNE      '  '
100080011018     C                   MOVEL     MSG(56)       VIDMSG
100090011018     C                   SETON                                        419028
100100011018     C                   GOTO      ENDCT5
100110011018    2C                   END
100120011018    1C                   END
100130941102     C*
100140931118     C* IMPOSSIBILE ELIMINARE O VARIARE C/A SE GIA' INCASSATO
100150941220     C     SAVFCA        IFNE      0
100160941220     C     ARBICC        ANDNE     ' '
100170941220     C     ARBICC        ANDNE     'R'
100180941220     c*
100190941102    2C     VIDCAS        IFNE      AR9CAS
100200941102     C     VIDTIC        ORNE      AR9TIC
100210941102     C     VIDVCA        ORNE      AR9VCA
100220941102     C     VIDGCA        ORNE      AR9GCA
100230941102     C                   MOVEL     MSG(22)       VIDMSG
100240941102     C                   SETON                                        429028
100250931118     C                   GOTO      ENDCT5
100260941102    2C                   ENDIF
100270941102    1C                   ENDIF
100280920120     C*
100290920204     C* CAUSALE KB NON CONTROLLA ESISTENZA O MENO DEL C/A: OK TUTTO
100300941102    1C     VIDCVR        IFNE      'KB'
100310931227     C*
100320941102     C* SE NON RICHIAMATO NON POSSO USARE CERTI TIPI BOLLA
100330971229     C* NON CONTROLLO PER VARIAZIONE SOLO DI IMPORTO
100340971229     C  N21
100350971229    2CANN20§3AUCB        IFEQ      'N'
100360941102     C                   MOVEL     MSG(16)       VIDMSG
100370941102     C                   SETON                                        409028
100380931227     C                   GOTO      ENDCT5
100390941102    2C                   END
100400920204     C*
100410941102    2C     §3AFCA        IFNE      0
100420941212     C     VIDCAS        ANDEQ     0
100430941102     C                   MOVEL     MSG(21)       VIDMSG
100440941102     C                   SETON                                        429028
100450920120     C                   GOTO      ENDCT5
100460941102    2C                   END
100470941102    1C                   END
100480941213     C**
100490941213     C* SE IMMESSO IMPORTO C/A , CONTROLLO I CAMPI RELATIVI
100500941213     C**
100510941213    1C     VIDCAS        IFGT      0
100520950120     C* SE ACCESO INDICATORE DI VARIAZIONE C/ASS LO SALVO
100530950120     C     *IN95         IFEQ      *ON
100540950120     C                   MOVEL     '1'           WIN95             1
100550950120     C                   ENDIF
100560140515     c                   clear                   trulintds
100570140515     c                   eval      iinttip='B'
100580140515     c                   eval      iinttic=vidtic
100590140515     c                   eval      iintrscm=v1crsm
100600140515     c                   eval      iintrsco=arbrmo
100610140515     c                   eval      IINTAAS=d48aas
100620140515     c                   eval      IINTLNP=d48lnp
100630140515     c                   eval      IINTNRS=d48nrs
100640140515     c                   eval      IINTNSP=d48nsp
100650140515     c                   call      'TRULINTR'
100660140515     c                   parm                    trulintds
100670950120     C*
100680920204     C* CONTROLLO TIPO INCASSO C/A
100690060705     C* richiamo pgm TRULTICR
100700060705     c                   clear                   trulticds
100710060705     c                   clear                   ds1a
100720060705     c                   eval      iticTBO='A'
100730060705     c                   eval      iticlnp=arblnp
100740060705     c                   eval      iticlna=arblna
100750060705     c                   eval      iticlnantw=lnantw
100760060705     c                   eval      iticnzd=arbnzd
100770090707     c                   eval      iticprd=arbprd
100780090707     c                   eval      iticcad=arbcad
100790090707     c                   eval      iticlod=arblod
100800090707     c                   eval      iticind=arbind
100810090707     c                   eval      iticrsd=arbrsd
100820060705     c                   eval      iticdsp=arbdsp
100830130827    3C***  ARBCCM        IFGT      0
100840130827     C***                MOVEL     ARBCCM        iticccm
100850130827     C***                ELSE
100860130827     C***                MOVEL     ARBKSC        iticccm
100870130827    3C***                END
100880130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
100890130827     c                   if        flgtb1='F'
100900130827     c                   z-add     arbksc        iticccm
100910130827     c                   else
100920130827     c                   z-add     arbccm        iticccm
100930130827     c                   if        iticccm=0
100940130827     c                   z-add     8888          iticccm
100950130827     c                   movel     arblnp        iticccm
100960130827     c                   endif
100970130827     c                   endif
100980140515     c* se il beneficiario del c/a è l'ordinante allora passo questo in iticccm
100990140515     c                   if        ointtipi='R'
101000140515     c                   move      ointcdi       iticccm
101010140515     c                   endif
101020060705     c                   eval      itictic=vidtic
101030060705     c                   eval      iticticbol=ar9tic
101040060705     c                   eval      iticdiv=vidvca
101050060705     c                   eval      iticcas=vidcas
101060060705     c                   eval      iticpgm='FNLR48R'
101070060705     c                   call      'TRULTICR'
101080060705     c                   parm                    TRULTICDS
101090060705     c                   parm                    ds1a
101100060705     c
101110060705     c                   if        oticric='T'
101120060705     c                   movel     otictic       vidtic
101130060705     C                   SETON                                        4190
101140060705     C                   GOTO      ENDCT5
101150060705     c                   endif
101160060705     c                   if        oticric='D'
101170060705     c                   movel     oticdiv       vidvca
101180060705     C                   SETON                                        4390
101190060705     C                   GOTO      ENDCT5
101200060705     c                   endif
101210060705     c* Errore tipo incasso
101220091211     c                   if        oticerr='T' or oticerr='C'
101230060705     C                   MOVEL     oticmsg       VIDMSG
101240060705     C                   SETON                                        419028
101250060705     C                   GOTO      ENDCT5
101260060705    2C                   ENDIF
101270060705     c* Errore divisa
101280060705     c                   if        oticerr='D'
101290060705     C                   MOVEL     oticmsg       VIDMSG
101300060705     C                   SETON                                        439028
101310060705     C                   GOTO      ENDCT5
101320060705    2C                   ENDIF
101330011010      *
101340941102     C*
101350941102     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE IL C/A
101360941102     C* INDICATORE DI CHANGE 95 ON
101370941213    2C     VARVCA        IFNE      VIDVCA
101380950120     C     WIN95         ANDEQ     *BLANKS
101390950120     C*
101400941219     C     VIDVCA        IFNE      *BLANKS
101410941219     C     VIDCAS        ORNE      0
101420941102     C                   MOVEL     MSG(28)       VIDMSG
101430941215     C                   SETON                                        429028
101440941102     C                   GOTO      ENDCT5
101450941213    2C                   ENDIF
101460941219    2C                   ENDIF
101470941213     C*
101480011206
101490011206      * pulisco il flag x la forzatura c/assegno forzabile
101500011206     c                   if        vidcas <> sav1_vidcas
101510011206     c                   clear                   forzacas
101520011206     c                   endif
101530011206      *-------------------
101540011206  2b c                   if        vidcas <> 0
101550011206     c                   eval      sav1_vidcas = vidcas
101560011206      * Controllo il c/assegno tramite il trul21r
101570011206     c                   clear                   trul21ds
101580011206     c                   eval      i21tla = 'L'
101590011206     c                   if        *in21 = *off
101600011206     c                   eval      i21cbo = newcbo
101610011206     c                   else
101620011206     c                   eval      i21cbo = arbcbo
101630011206     c                   endif
101640011206     c                   eval      i21tsp = arbtsp
101650011206     c                   eval      i21lnp = arblnp
101660011206     c                   eval      i21nzm = arbnzm
101670011206     c                   eval      i21lna = arblna
101680011206     c                   eval      i21nzd = arbnzd
101690130827     c*****              eval      i21ksc = arbksc
101700011206     c                   movel     arbctr        i21ctr
101710011206     c                   eval      i21tic = vidtic
101720011206     c                   eval      i21imp = vidcas
101730011206     c                   eval      i21div = vidvca
101740011206     c                   eval      i21ute = knmus
101750011206     c                   eval      i21pgm = nompgm
101760130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
101770130827     c                   if        flgtb1='F'
101780130827     c                   z-add     arbksc        i21ksc
101790130827     c                   else
101800130827     c                   z-add     arbccm        i21ksc
101810130827     c                   if        i21ksc=0
101820130827     c                   z-add     8888          i21ksc
101830130827     c                   movel     arblnp        i21ksc
101840130827     c                   endif
101850130827     c                   endif
101860011206     c                   call      'TRUL21R'
101870011206     c                   parm                    trul21ds
101880011206      * controllo che non sia minore del limite previsto
101890011206  3b c                   if        o21fmn <> *blanks
101900011206     c                   eval      vidmsg = msg(108)
101910011210     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
101920011206     c                   eval      *in42 = *on
101930011206     c                   eval      *in90 = *on
101940011206     c                   eval      *in28 = *on
101950011206     c                   goto      endct5
101960011206  3e c                   endif
101970011206      * controllo che non sia maggiore del limite previsto
101980011206      * non forzabile
101990011206  3b c                   if        o21fx2 <> *blanks
102000011206     c                   eval      vidmsg = msg(91)
102010011210     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
102020011206     c                   eval      *in42 = *on
102030011206     c                   eval      *in90 = *on
102040011206     c                   eval      *in28 = *on
102050011206     c                   goto      endct5
102060011206  3e c                   endif
102070050301      * controllo che non sia maggiore del limite per bancanri bartolini no
102080050301      * abilitati da sede, non forzabile
102090050301  3b c                   if        o21fb <> *blanks
102100050301     c                   eval      vidmsg = msg(132)
102110050301     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
102120050301     c                   eval      *in42 = *on
102130050301     c                   eval      *in90 = *on
102140050301     c                   eval      *in28 = *on
102150050301     c                   goto      endct5
102160050301  3e c                   endif
102170011206      * controllo che non sia maggiore del limite previsto
102180011206      * forzabile
102190011206  3b c                   if        o21fx1 <> *blanks and forzacas = *blanks
102200011206     c                   eval      forzacas = '1'
102210011206     c                   eval      vidmsg = msg(109)
102220011210     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
102230011206     c                   eval      *in42 = *on
102240011206     c                   eval      *in90 = *on
102250011206     c                   eval      *in28 = *on
102260011206     c                   goto      endct5
102270011206  3e c                   endif
102280011206  2e c                   endif
102290011206
102300011120      *
102310011120      * Se bolla giacente e modifica 'KA' o 'KP' l'importo immesso (convertito) deve
102320011120      * essere uguale all'originario
102330011120  2b c                   if        *in04 = *ON  and (vidcvr = 'KA' or
102340011120     c                             vidcvr = 'KP')
102350011120     c                   z-add     vidcas        w0173
102360011120      * Cerco i decimali previsti per la divisa originaria
102370011120     c                   movel     'CV'          cod
102380011120     c                   movel(p)  ar9vca        key
102390011120     c                   exsr      ctabel
102400011120     c  n30              movel     tbluni        dscv
102410011120     c   30              clear                   dscv
102420011120      * Controlla la divisa
102430011120  3b c                   if        ar9vca <> vidvca
102440011120      * Converto il contrassegno inserito a video nella divisa originaria
102450030612     c                   clear                   YeurcoDS
102460011120     c                   movel     vidvca        yecdvi
102470011120     c                   movel     ar9vca        yecdvo
102480011120     c                   z-add     vidcas        yecimi
102490011120     c                   move      §cvdec        yecdco
102500011120     c                   call      'YEURCO'
102510030612     c                   parm                    YeurcoDS
102520011120  4b c                   if        yecesi = ' '
102530011120     c                   z-add     yecimo        w0173
102540011120  4e c                   endif
102550011120  3e c                   endif
102560011120      * Calcolo la differenza tra i 2 contrassegni
102570011120     c                   eval      wdiffe = (ar9cas - w0173)
102580011120     c                   mllzo     1             wdiffe
102590020103      * Converto la differenza nella divisa di conto
102600011120  3b c                   if        ar9vca <> §gedcn
102610030612     c                   clear                   YeurcoDS
102620020103     c                   movel     ar9vca        yecdvi
102630020103     c                   movel     §gedcn        yecdvo
102640011120     c                   z-add     wdiffe        yecimi
102650020103     c                   move      decsav        yecdco
102660011120     c                   call      'YEURCO'
102670030612     c                   parm                    YeurcoDS
102680011120  4b c                   if        yecesi = ' '
102690011120     c                   z-add     yecimo        wdiffe
102700011120  4e c                   endif
102710011120  3e c                   endif
102720020103      *
102730020103      * Se differenza maggiore del valore in tabella ERRORE
102740011120  3b c                   if        wdiffe > dfcsav
102750011120     c                   movel     msg(103)      vidmsg
102760011120     c                   seton                                        429028
102770011120     c                   goto      endct5
102780011120  3e c                   endif
102790011120  2e c                   endif
102800011120
102810991021     C**
102820991021     C                   MOVEL     VIDVCA        VARVCA
102830991021     C                   CLEAR                   WIN95
102840941102     c**
102850941102     C* PARTICOLARITA' C/ASSEGNO
102860941102     c**
102870941213    2C     VIDGCA        IFNE      *BLANKS
102880941102     C     VIDGCR        ORNE      *BLANKS
102890941102     C*
102900941102     C* non posso indicarla se NON IMMESSO C/A
102910941213    3C     VIDCAS        IFEQ      0
102920941102     C     VIDGCA        ANDNE     *BLANKS
102930941102     C                   SETON                                        449028
102940941102     C                   MOVEL     MSG(29)       VIDMSG
102950941102     C                   GOTO      ENDCT5
102960941213    3C                   ENDIF
102970941102     C*
102980941102     C* RICHIAMO IL PRG DI RICERCA
102990941213    3C     VIDGCR        IFEQ      '?'
103000030612     C                   CLEAR                   DS7pqrs
103010941102     C                   MOVEL     'S'           DS7RIC
103020941102     C                   MOVEL     'V'           DS7GES
103030941213    4C     ARBCCM        IFGT      0
103040941102     C                   MOVEL     ARBCCM        DS7KSC
103050941102     C                   ELSE
103060941102     C                   MOVEL     ARBKSC        DS7KSC
103070941213    4C                   ENDIF
103080941102     C                   MOVEL     VIDGCA        DS7COD
103090030612     C                   MOVEL     DS7pqrs       KPJBU
103100941102     C                   CALL      'TRTB72R'
103110941102     C                   PARM                    KPJBA
103120030612     C                   MOVEL     KPJBU         DS7pqrs
103130941102     C*
103140941102     C     VIDGCA        IFEQ      *BLANKS
103150941102     C                   MOVEL     DS7COD        VIDGCA
103160941213    3C                   ENDIF
103170941102     C*
103180941102     C                   CLEAR                   VIDGCR
103190941102     C*
103200941102     C                   SETON                                        90
103210941102     C                   GOTO      ENDCT5
103220941102    2C                   ENDIF
103230941102     C* CONTROLLO
103240030612     C                   CLEAR                   DS7pqrs
103250941102     C                   MOVEL     'S'           DS7RIC
103260941102     C                   MOVEL     'C'           DS7GES
103270941213    2C     ARBCCM        IFGT      0
103280941102     C                   MOVEL     ARBCCM        DS7KSC
103290941102     C                   ELSE
103300941102     C                   MOVEL     ARBKSC        DS7KSC
103310941102     C                   ENDIF
103320941102     C                   MOVEL     VIDGCA        DS7COD
103330030612     C                   MOVEL     DS7pqrs       KPJBU
103340941102     C                   CALL      'TRTB72R'
103350941102     C                   PARM                    KPJBA
103360030612     C                   MOVEL     KPJBU         DS7pqrs
103370941102     C* ERRORE
103380941213    3C     DS7ERR        IFEQ      '1'
103390941102     C                   MOVEL     DS7MSG        VIDMSG
103400941102     C                   SETON                                        449028
103410941102     C                   GOTO      ENDCT5
103420941213    3C                   ENDIF
103430941102     C*
103440941213    2C                   ENDIF
103450941213    1C                   ENDIF
103460011010      *-------------------
103470941220     C*
103480941220     c* SE MODIFICATO IK TIPO BOLLA ED ELIMINATA LA 2 MA GIA' INCASS
103490941220     C*  ERRORE
103500941220     C     §3ATB2        IFEQ      *BLANKS
103510941220     C*
103520941220     C     SAVTB2        ANDNE     *BLANKS
103530941220     C     ARBICA        ANDNE     ' '
103540941220     C     ARBICA        ANDNE     'R'
103550991007     C     DFT2BO        ANDGT     0
103560941220     C                   MOVEL     MSG(59)       VIDMSG
103570941220     C                   SETON                                        409028
103580941220     C                   GOTO      ENDCT5
103590941220     C                   END
103600950407     C* SE CAUSALE VARIAZIONE CODICE BOLLA
103610950407    1C     *IN21         IFEQ      *OFF
103620950407     C     COM01A        ANDEQ     *BLANKS
103630950407     C* E NON TROVATI ERRORI BLOCCANTI
103640950407     C     *IN90         ANDEQ     *OFF
103650950407     C* E ASSEGNATO
103660950407     C     FLGTB1        ANDEQ     'A'
103670950407     C* SE AGGIUNTO O TOLTO IL CONTRASSEGNO
103680950407    2C     SAVFCA        IFEQ      *ZEROS
103690950407     C     §3AFCA        ANDNE     *ZEROS
103700950407     C     SAVFCA        ORNE      *ZEROS
103710950407     C     §3AFCA        ANDEQ     *ZEROS
103720991007     C                   MOVEL     '1'           WTRC
103730991007     C     KARB2         CHAIN(N)  FIAR6000                           30
103740950407    3C     *IN30         IFEQ      *OFF
103750950407     C* SE GIA' TASSATO FINO ALL'IMPONIBILE
103760950407     C     AR6IMV        ANDNE     *ZEROS
103770950407     C                   SETON                                        2892
103780950407     C* ERRORE NON BLOCCANTE:MANCA VARIA G
103790950407    4C     §3AFCA        IFNE      *ZEROS
103800950407     C                   MOVEL     MSG(65)       VIDMSG
103810950407     C                   ELSE
103820950407     C* ERRORE NON BLOCCANTE:C'E' VARIA G
103830950407     C                   MOVEL     MSG(66)       VIDMSG
103840950407    4C                   END
103850950407    3C                   END
103860950407     C*
103870950407    2C                   END
103880950407     C                   MOVE      '1'           COM01A            1
103890950407    1C                   END
103900920116     C*
103910920116     C     ENDCT5        ENDSR
103920920116     C*
103930920116     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE KX -----------------*
103940920129     C     REGIS5        BEGSR
103950991001     C                   SETOFF                                       903839
103960130412     c                   clear                   wpda_ndc
103970130412     c                   clear                   wpda_ddc
103980130412     c                   clear                   wpda_dcm
103990071116     C
104000950104     C*  GIRO UDATE
104010950104     C                   TIME                    W0140            14 0
104020950104     C* UDATE IN GGMMAAAA
104030950104     C                   MOVE      W0140         WDTGIO            8 0
104040950104     C*
104050950104     C* UDATE IN AAAAMMGG
104060950104     C                   Z-ADD     WDTGIO        G02DAT
104070950104     C                   MOVEL     *BLANK        G02ERR
104080950104     C                   CALL      'XSRDA8'
104090950104     C                   PARM                    WLBDAT
104100950104     C                   MOVEL     G02INV        DATEU             8 0
104110941221     C* PRENDO I VALORI ASSOLUTI
104120941221     C                   MLLZO     1             VIDCAS
104130941221     C*
104140941213     C                   MOVEL     KNMUS         ARBPRU
104150941220     C                   MOVEL     ARBCPI        SAVCPI
104160070515     c
104170070515     c* Se modificato il C/A, devo allocarmi record "L" per il
104180070515     c*  blocco telefonata C/A
104190070515     c                   clear                   wesar4L           1
104200070515    1C     vidCAS        IFNE      SAVCAS
104210070515     C     vidTIC        ORNE      SAVTIC
104220070515     C     vidVCA        ORNE      SAVVCA
104230070807     C     vidcas        andgt     0
104240070515     c                   eval      Tipoela='C'
104250070515     c                   exsr      GestAR4L
104260070515    2c                   if        *in91
104270070515     C                   GOTO      ENDRG5
104280070515    2c                   endif
104290070515    1c                   endif
104300070515     c
104310070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
104320070515     c* iornare direttamente il file solo se cambiato tipo bolla
104330070515     c                   clear                   waltrabolla       1
104340130412     c                   setoff                                       3091
104350130412     c                   if        oldcbo<>newcbo
104360070515     C   10KARB          CHAIN     FNBLP01L                           3091
104370070515     C  n10KARB          CHAIN     FNARB01L                           3091
104380130412     c                   else
104390130412     c* questa chain mi serve per l'invio dati a PDA
104400131120     c                   if        d66pda<>' '
104410130412     C  n10KARB          CHAIN(n)  FNARB01L                           30
104420130412     c                   endif
104430130412     c                   endif
104440070515     c*
104450070515     c                   if        *in91
104460070515     C                   SETON                                        28
104470070515     C                   MOVEL     MSG(4)        VIDMSG
104480070515     c                   unlock    fiar401l
104490070515     C                   GOTO      ENDRG5
104500070515     c                   else
104510070515     c                   if        *in30
104520070515     c                   eval      waltrabolla='N'
104530070515     c                   else
104540130411     c  n10              eval      wpda_ndc=arbndc
104550130411     c  n10              eval      wpda_ddc=arbddc
104560130411     c  n10              eval      wpda_dcm=arbdcm
104570130412     c                   if        oldcbo<>newcbo
104580070515     C   10KARB          CHAIN     FNARB01L                           30
104590130412     c                   endif
104600131120     c                   if        oldcbo<>newcbo or d66pda<>' '
104610070515     C  N10KARB          CHAIN     FNBLP01L                           30
104620070515     c* Alla fine per non sporcarmi i campi del record, richaino il
104630070515     c*  record originale che sto variando
104640130411     c   10              eval      wpda_ndc=arbndc
104650130411     c   10              eval      wpda_ddc=arbddc
104660130411     c   10              eval      wpda_dcm=arbdcm
104670130412     c                   endif
104680070515     c                   endif
104690070515     c                   endif
104700130412     c*****              endif
104710920123     C*********
104720920116     C* INVIO PER SEDE
104730941014     C                   MOVEL     'FNARBK0T'    SFILE
104740920116     C* ALLOCO OGGETTI
104750920116     C                   EXSR      ALLOC
104760070515     c                   if        *in91
104770070515     c                   unlock    fiar401l
104780070515     c   10              unlock    fnblp01l
104790070515     c  n10              unlock    fnarb01l
104800070515     C                   GOTO      ENDRG5
104810070515     c                   endif
104820930511     C*
104830941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL MITTENTE PER C/A
104840941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL DESTINATARIO SE DOPPIA BOLLA
104850930511     C                   MOVEL     'S'           §7LFFI
104860930511     C*
104870930210     C********* PER  C O N T R A S S E G N O
104880941215    1C     AR9CAS        IFEQ      0
104890941102     C     VIDCAS        ANDNE     0
104900930210     C                   SETON                                        38
104910930210     C* INVIO PER SEDE
104920941014     C                   MOVEL     'FNARBM0T'    SFILE
104930930210     C* ALLOCO OGGETTI
104940930210     C                   EXSR      ALLOC
104950070515     c
104960070515     c                   if        *in91
104970070515     C                   MOVEA     'FNARBK0T'    C21(13)
104980070515     C                   MOVEA     VAR           C21(41)
104990070515     C                   MOVEL     *BLANKS       COMMAN           80
105000070515     C                   MOVEA     C21           COMMAN
105010070515     C                   CALL      'QCMDEXC'                            H1
105020070515     C                   PARM                    COMMAN
105030070515     C                   PARM                    LUNG
105040070515     c                   unlock    fiar401l
105050070515     c   10              unlock    fnblp01l
105060070515     c  n10              unlock    fnarb01l
105070070515     c
105080070515     C                   GOTO      ENDRG5
105090941212    1C                   END
105100070515    1C                   END
105110950324     C*
105120930210     C********* PER  D O P P I A    B O L L A
105130941212    1C     SAVTB2        IFEQ      '  '
105140930210     C     §3ATB2        ANDNE     '  '
105150930210     C                   SETON                                        39
105160930210     C* INVIO PER SEDE
105170941102     C                   MOVEL     'FNARBD0T'    SFILE
105180930210     C* ALLOCO OGGETTI
105190930210     C                   EXSR      ALLOC
105200070515     c                   if        *in91
105210070515     C                   MOVEA     'FNARBK0T'    C21(13)
105220070515     C                   MOVEA     VAR           C21(41)
105230070515     C                   MOVEL     *BLANKS       COMMAN           80
105240070515     C                   MOVEA     C21           COMMAN
105250070515     C                   CALL      'QCMDEXC'                            H1
105260070515     C                   PARM                    COMMAN
105270070515     C                   PARM                    LUNG
105280070515     c
105290070515     C                   MOVEA     'FNARBM0T'    C21(13)
105300070515     C                   MOVEL     *BLANKS       COMMAN           80
105310070515     C                   MOVEA     C21           COMMAN
105320070515     C                   CALL      'QCMDEXC'                            H1
105330070515     C                   PARM                    COMMAN
105340070515     C                   PARM                    LUNG
105350070515     c                   unlock    fiar401l
105360070515     c   10              unlock    fnblp01l
105370070515     c  n10              unlock    fnarb01l
105380070515     C                   GOTO      ENDRG5
105390950324     C                   ENDIF
105400070515     C                   ENDIF
105410041022     C*********
105420041022     C                   Z-ADD     DATEU         ARBDTV
105430060320     C                   TIME                    ARBORV
105440920116     C* STORICIZZAZIONE ALL'ARRIVO
105450060130    1C     D66FSA        ifeq      'S'
105460920116     C                   MOVEL     VIDCVR        ARBCVB
105470941213     C                   MOVEL     OLDCBO        ARBCBP
105480941213     C                   MOVEL     NEWCBO        ARBCBN
105490941213     C                   MOVEL     AR9TIC        ARBTIC
105500941213     C                   MOVEL     AR9CAS        ARBCAS
105510941213     C                   MOVEL     AR9VCA        ARBVCA
105520941213     C                   MOVEL     AR9GCA        ARBGCA
105530011108      *
105540011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
105550011108      * è in partenza 'P' o in arrivo 'A'
105560011108     C     *IN10         IFEQ      *ON
105570011108     C                   MOVE      'A'           ARBPRU
105580011108     C                   ENDIF
105590011108     C     *IN10         IFEQ      *OFF
105600011108     C                   MOVE      'P'           ARBPRU
105610011108     C                   ENDIF
105620011108      *
105630941014     C                   WRITE     FNARBK00
105640930210     C                   END
105650060213     c* storicizzazione motivazione
105660060213     c     d66mtv        ifeq      'S'
105670060301     c                   exsr      sto_mtv
105680060213     c                   endif
105690920116     C*
105700920123     C*********
105710920117     C* I N V I O
105720941102     C                   MOVEL     NEWCBO        ARBCBN
105730941213     C* PER IL FILE DI VARIAZIONE
105740941102     C                   MOVEL     VIDCAS        ARBCAS
105750920116     C                   MOVEL     VIDTIC        ARBTIC
105760941212     C                   MOVEL     VIDGCA        ARBGCA
105770941213     C*
105780941215     C*
105790941212     C                   MOVEL     VIDVCA        ARBVCA
105800011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
105810011017     C     VIDCAS        IFEQ      *ZEROS
105820011017     C                   CLEAR                   ARBVCA
105830011017     C                   ENDIF
105840920123     C*********
105850041022     C     D66FFI        ifeq      'S'
105860920117     C                   TIME                    ARBORV
105870920127     C                   MOVEL     VIDCVR        ARBCVB
105880941014     C                   OPEN      FNARBK0T
105890941014     C                   WRITE     FNARBKTT
105900941014     C                   CLOSE     FNARBK0T
105910920117     C* DISALLOCO
105920941014     C                   MOVEA     'FNARBK0T'    C21(13)
105930041022     C                   MOVEA     VAR           C21(41)
105940920117     C                   MOVEL     *BLANKS       COMMAN           80
105950920117     C                   MOVEA     C21           COMMAN
105960920117     C                   CALL      'QCMDEXC'                            H1
105970920117     C                   PARM                    COMMAN
105980920117     C                   PARM                    LUNG
105990920212     C*
106000920212     C   38              DO
106010941014     C                   OPEN      FNARBM0T
106020950102     C                   WRITE     FNARBMTT
106030941014     C                   CLOSE     FNARBM0T
106040920212     C* DISALLOCO
106050941014     C                   MOVEA     'FNARBM0T'    C21(13)
106060920212     C                   MOVEA     C21           COMMAN
106070920212     C                   CALL      'QCMDEXC'                            H1
106080920212     C                   PARM                    COMMAN
106090920212     C                   PARM                    LUNG
106100920212     C                   END
106110920213     C*
106120920213     C   39              DO
106130930210     C                   MOVEL     *BLANKS       ARBCPI
106140920213     C                   ADD       1             ARBORV
106150930210     C                   MOVEL     VIDCVR        ARBCVB
106160941212     C                   MOVEL     NEWCBO        ARBCBO
106170941102     C                   OPEN      FNARBD0T
106180941209     C                   WRITE     FNARBDTT
106190941102     C                   CLOSE     FNARBD0T
106200920213     C* DISALLOCO
106210941102     C                   MOVEA     'FNARBD0T'    C21(13)
106220920213     C                   MOVEA     C21           COMMAN
106230920213     C                   CALL      'QCMDEXC'                            H1
106240920213     C                   PARM                    COMMAN
106250920213     C                   PARM                    LUNG
106260920213     C                   END
106270930210     C                   END
106280920117     C*
106290920117     C* A G G I O R N O   F I L E
106300920117     C*
106310051118     c* se variato imposto la data ultima variazione
106320091217    1c                   if        vidvca<>ar9vca or
106330051118     c                             ar9cas<>vidcas or
106340051118     c                             ar9vca<>vidvca or
106350051118     c                             ar9gca<>vidgca
106360051118     c                   movel     dateu         ar9duv
106370091217    1c                   endif
106380091217     c
106390091217     c* Se variato il tipo incasso C/Ass, lo memorizzo
106400140516     c* non lo memorizzo in caso di annullamento altrimenti su internet viene
106410140516     c* visualizzata sia la variazione che l'annullamento
106420140516    1c                   if        savtic<>vidtic and vidcas>0
106430091217     c                   eval      Wtipoagg='TIC'
106440091217     c                   exsr      RegisAR5GEN
106450091217    1c                   EndIf
106460091217     c
106470091217     c
106480941212     C                   MOVEL     NEWCBO        ARBCBO
106490941212     C                   MOVEL     VIDCAS        AR9CAS
106500941212     C                   MOVEL     VIDTIC        AR9TIC
106510941212     C                   MOVEL     VIDGCA        AR9GCA
106520941212     C                   MOVEL     VIDVCA        AR9VCA
106530011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
106540011017     C     VIDCAS        IFEQ      *ZEROS
106550011017     C                   CLEAR                   AR9VCA
106560011017     C                   ENDIF
106570011019      *
106580941220     C                   MOVEL     ARBCPI        SAVCPI
106590920121     C*
106600941220     C* E L I M I N A T A   L A   2   B O L L A
106610941220    1C     SAVTB2        IFNE      '  '
106620920603     C     §3ATB2        ANDEQ     '  '
106630941220     C* PULISCO PARTITA IVA
106640930210     C                   MOVEL     *BLANKS       ARBCPI
106650991001     C* SE C'ERA AR6 SECONDA BOLLA LO DELETO
106660990930     C     WAR62         IFEQ      'E'
106670991008     C                   MOVEL     '2'           WTRC
106680991008     C     KARB2         DELETE    FIAR6000                           30
106690941220     C                   ENDIF
106700941220     C*
106710941220     C* SE GIA' CONSEGNATA DO' PER INCASSATO L'ASSEGNATO
106720000925    2C     ARBDCM        IFGT      0
106730000925    3C     ARBICA        IFEQ      'R'
106740000925     C     ARBICA        OREQ      ' '
106750941220     C                   MOVEL     'Y'           ARBICA
106760000925    4C     ARBICC        IFNE      'R'
106770970613     C                   EXSR      CHIUBO
106780000925    5C     ARBNMI        IFNE      8888888
106790971023     C                   MOVEL     'S'           ARBICA
106800000925    5C                   ENDIF
106810000925    4C                   ENDIF
106820000925    3C                   ENDIF
106830000925    2C                   ENDIF
106840941220     C* ABILITO L'ASSEGNATO
106850941220     C                   MOVEL     'S'           ARBACA
106860941220    1C                   ENDIF
106870941220     C*
106880941220     C* A G G I U N T A   2   B O L L A
106890941220     C     SAVTB2        IFEQ      '  '
106900941220     C     §3ATB2        ANDNE     '  '
106910941220     C* PULISCO PARTITA IVA
106920941220     C                   MOVEL     *BLANKS       ARBCPI
106930941220     C*
106940941220     C                   MOVEL     ' '           ARBACA
106950941220     C                   MOVEL     ' '           ARBICA
106960941220     C* SE GIA' CONSEGNATA SI TRATTA DI UN RITORNO ALL'INCASSO
106970941220     C     ARBDCM        IFGT      0
106980941220     C                   MOVEL     'R'           ARBICA
106990941220     C                   ENDIF
107000941220     C                   ENDIF
107010920120     C*
107020920117     C* ABILITAZIONE CONTRASSEGNO
107030950918     C* SE NON HO VARIATO IL C/A RISPETTO A PRIMA, LASCIO ABILITAZIONE
107040950918     C*  E INCASSO C/A COME ERANO
107050950918    1C     AR9CAS        IFNE      SAVCAS
107060950918     C     AR9TIC        ORNE      SAVTIC
107070950918     C     AR9VCA        ORNE      SAVVCA
107080920120     C                   MOVEL     ' '           ARBACC
107090070320     c                   eval      §b4ltca='S'
107100070320     c
107110941220     C*
107120920117     C* SE NON C'E' PIU' IL C/ASSEGNO ABILITO
107130950918    2C     §3AFCA        IFEQ      0
107140950104     C     AR9CAS        OREQ      0
107150920117     C                   MOVEL     'S'           ARBACC
107160070320     c                   eval      §b4ltca=' '
107170960704     C*
107180960909     C* SE IL CONTRASSEGNO E' UN RITORNO ALL'INCASSO, LO TOLGO
107190960704     C* SE CONSEGNATO, E RITORNO ALL'INCASSO LO DO PER INCASSATO
107200960704    3C     ARBDCM        IFGT      0
107210960704     C     ARBICC        ANDEQ     'R'
107220960704     C                   MOVEL     'Y'           ARBICC
107230970613     C* SE L'ASSEGNATO NON E' UN RITORNO ALL'INCASSO VEDO SE CHIUDERE
107240970613     C* LA BOLLA
107250970613     C     ARBICA        IFNE      'R'
107260970613     C                   EXSR      CHIUBO
107270971023     C     ARBNMI        IFNE      8888888
107280971023     C                   MOVEL     'S'           ARBICC
107290971023     C                   ENDIF
107300970613     C                   ENDIF
107310970613     C                   ENDIF
107320960704     C*
107330950918   X2C                   ELSE
107340941220     C*
107350941220     C* SE LA BOLLA E' GIA' CONSEGNA METTO "R" DI RITORNO ALL'INCASSO
107360950918    3C     ARBDCM        IFGT      0
107370941220     C                   MOVEL     'R'           ARBICC
107380950918   X3C                   ELSE
107390941220     C* ALTRIMENTI PULISCO IL FLAG DI INCASSO C/A
107400941220     C                   CLEAR                   ARBICC
107410950918    3C                   ENDIF
107420011031      *
107430070320    3c                   if        o21fca = *blanks
107440020103     c                   eval      arbacc = 'S'
107450070320     c                   clear                   §b4ltca
107460070320    3c                   endif
107470070320    2C                   END
107480070320     c
107490070320     c* Aggiorno record di FIar4
107500070320     c                   eval      TipoEla='A'
107510070320     c                   exsr      GestAR4L
107520070320    1C                   END
107530071116     c*
107540071116     C* M O D I F I C A T O   S O L O   1 °   T I P O   B O L L A
107550071116    1C     SAVTB2        IFEQ      '  '
107560071116     C     §3ATB2        ANDEQ     '  '
107570071116     C* PRENDI IL TIPO BOLLA DEL NUOVO CODICE
107580071116     C                   MOVEL     'TB'          COD
107590071116     C                   MOVEL(P)  §3atb1        KEY
107600071116     C                   EXSR      CTABEL
107610071116     C  N30              MOVEL     TBLUNI        DSTB
107620071116     C   30              CLEAR                   DSTB
107630071116     c                   movel     §tbfcb        NEWtb1FCB         1
107640071116     c
107650071116     C* Se il nuovo tipo bolla è da non contabilizzare, imposto
107660071116     c*  la "S" di abilitazione Assgnato
107670071116    2c                   if        ARBACA=' ' and NEWtb1FCB='0'
107680071116     c                   eval      arbaca='S'
107690071116    2c                   endif
107700071116     c
107710071116     c* Se il nuovo tipo bolla è da contabilizzare, ma non lo era il
107720071116     c*  vecchio, vedo se devo togliere la "S" per l'abilitazione del C/A
107730071116    2c                   if        NEWtb1FCB='1' and Newtb1FCB<>OLDtb1FCB
107740071116     c                             and arbaca='S'
107750071116     C* TASSAZIONE IN FIAR6
107760071116     C                   MOVEL     '1'           WTRC
107770071116     C     KARB2         CHAIN(N)  FIAR6000
107780071116    3c                   if        (%subst(%editc(arbksc: 'X'):4:4)='9999') and
107790071116     c                             (not %found(FIar601l) or ar6dft=0)
107800071116     c                   eval      arbaca=' '
107810071116    3c                   endif
107820071116    2c                   endif
107830071116     c
107840071116    1C                   ENDIF
107850920116     C*
107860941216     C     §3AFCA        IFNE      0
107870950104     C     AR9CAS        ANDGT     0
107880941212     C     WAR9          IFEQ      ' '
107890941212     C                   MOVEL     ARBAAS        AR9AAS
107900941212     C                   MOVEL     ARBLNP        AR9LNP
107910941212     C                   MOVEL     ARBNRS        AR9NRS
107920941212     C                   MOVEL     ARBNSP        AR9NSP
107930051118     c                   movel     dateu         ar9duv
107940051118     C                   WRITE     FiAR9000
107950941212     C                   ELSE
107960051118     C                   UPDATE    FiAR9000
107970941212     C                   ENDIF
107980941216     C*
107990941216     C                   ELSE
108000941216     C* SE MI HANNO CANCELLATO L'AR9 --> DELETO
108010941216     C     WAR9          IFEQ      'E'
108020051118     C     KARB          DELETE    FiAR9000                           30
108030941216     C                   ENDIF
108040941216     C                   ENDIF
108050941212     C*
108060060213     C   10              UPDATE    FNARB000
108070041022     c* Se cambiato codice bolla aggiorno fnblp
108080041022     c                   if        waltrabollA=' ' AND oldcbo<>newcbo
108090041022     c                   eval      arbcbo=newcbo
108100041022     c   10              except    arbkblp
108110041022     c  N10              except    arbkarb
108120041022     c                   endif
108130040224
108140040224      * Se c'è la data consegna richiesta controllo se è ancora valida
108150040224      * solo se ho fatto una modifica in arrivo
108160160114     c                   If        ArbDcr > *Zeros and *In10 and wfile='A'
108170040324     c                             and wCa = *Blanks and wgiac = *Blanks
108180040224     c                   ExSr      ContrDcr
108190040224     c                   EndIf
108200950602     C*
108210130411     c* Richiamo routine per invio variazione a pda
108220130411     c                   exsr      sr_pda
108230950104     C*
108240920117     C     ENDRG5        ENDSR
108250911211     C*
108260911211     C*--- CARICO CODICI TARIFFA -------------------------------------*
108270911211     C     CARCTR        BEGSR
108280020305     c**
108290020305     C* Richiamo trul27r per reperimento WFIE e per reperimento tariffa di
108300020305     C* cartello
108310020305     c**
108320160225     c                   clear                   trul27ds1
108330021121     c                   Eval      I27Pkg = ArbPkf
108340020305     c                   eval      i27tsp = arbtsp
108350020305     c                   eval      i27lna = arblna
108360020305     c                   eval      i27lnp = arblnp
108370020305     C                   eval      i27cli = ksc
108380160225     c                   call      'TRUL27R1'
108390160225     c                   parm                    trul27ds1
108400020305     c                   eval      wfie = o27fie
108410020305     c*
108420941209     C                   MOVEL     *BLANKS       DE2CTR
108430050202     C                   CLEAR                   Fnlv59DS
108440941209     C*
108450050202     C                   MOVEL     D48TBO        ilv59tbo
108460040511      * SE BOLLA MONOVARIA NON PASSO IL TIPO BOLLA
108470040511     C                   IF        §3ASVA <> *BLANK
108480050202     C                   CLEAR                   ilv59tbo
108490050202     c                   eval      ilv59prp='P'
108500040511     C                   ENDIF
108510050202     C                   Z-ADD     ARBDSP        ilv59DSP
108520060131     C                   MOVEL     dutkci        ilv59KCI
108530050202     C                   Z-ADD     KSC           ilv59KSC
108540050202     C                   MOVEL     WFIE          ilv59FIE
108550160225     C                   MOVEL     WFIE          ilv59ta3
108560050202     C                   MOVEL     ARBTSP        ilv59TSP
108570050202     C                   MOVEL     VIDCTR        ilv59CTR
108580941209     C*
108590941209     C* SE NON C'E' RICERCA SU COD.TARIFFA E CAMBIATO CODICE CLIENTE
108600941209     C*  --> RICERCO LA TARIFFA
108610941209     C                   SETOFF                                       32
108620941209     C     '?'           SCAN      VIDCTR                                 32
108630941215     C     *IN32         IFEQ      *OFF
108640941215     C     KSC           ANDNE     SAVKSC
108650050202     C     VIDCTR        ANDEQ     SAVCTR
108660160225     C***                CLEAR                   ilv59CTR
108670941223     C                   CLEAR                   OTT
108680160225     C***                MOVEL     'S'           ilv59RIC
108690941215     C                   ELSE
108700060516     c* Se cod cliente e cod tariffa invariati rispetto alla bolla,
108710060516     c*  accetto codici tariffa bloccati (ma quando immessi validi)
108720060516     c                   if        not *in32 and ksc=kscbolla
108730060511     c                             and ctrbolla=vidctr
108740060516     C                   MOVEL     'B'           ilv59ta2
108750941209     C                   ENDIF
108760060511     C                   ENDIF
108770060516     C                   MOVEL     'S'           ilv59CON
108780050202     C
108790050202     C                   MOVEL     KSC           SAVKSC
108800160303     c* se ho abblancato il codice tariffa, lo faccio reimpostare dal pgm
108810160303     c                   if        ilv59ctr=*blanks
108820160303     C                   MOVEL     'S'           ilv59RIC
108830160303     c                   endif
108840941209     C*
108850050202     C                   CALL      'FNLV59R'
108860050202     C                   PARM                    Fnlv59DS
108870050202     c
108880941209     C*
108890941209     C* NON ESISTONO TARIFFE
108900050202     C                   MOVEL     olv59TKS      WTKS              1
108910050202     C     olv59TKS      IFNE      'S'
108920941209     C                   SETON                                        18
108930911219     C                   SETOFF                                       32
108940950114     C                   MOVEL     CNOTAR        DE2CTR
108950941209     C                   ELSE
108960941209     C*
108970941215     C* SE TARIFFE TROVATE,  DEVO REIMPOSTARE LA TARIFFA A VIDEO
108980050202     C     ilv59RIC      IFEQ      'S'
108990050202     C                   MOVEL     olv59CTR      VIDCTR
109000941209     C                   ENDIF
109010941215     C* DECODIFICA
109020050202     C                   MOVEL     olv59DFS      DE2CTR
109030941209     C                   ENDIF
109040050202     c
109050050202     C                   EVAL      SAVCTR=VIDCTR
109060911211     C*
109070911211     C                   ENDSR
109080911211     C*
109090920320     C*--- CONTROLLI CAMPI CONSEGNA RICHIESTA ------------------------*
109100920320     C     CONTR7        BEGSR
109110920320     C                   SETOFF                                       90
109120941102     c**
109130920320     C* CONSEGNA RICHIESTA
109140941102     C**
109150920320     C                   Z-ADD     0             COMDCR
109160920320     C     VIDDCR        IFNE      0
109170941102     C                   Z-ADD     VIDDCR        G02DAT
109180920320     C                   MOVEL     *BLANK        G02ERR
109190941102     C                   CALL      'XSRDA8'
109200920320     C                   PARM                    WLBDAT
109210941102     C* DATA FORMALMENTE ERRATA
109220920320     C     G02ERR        IFEQ      '1'
109230941102     C                   MOVEL     MSG(6)        VIDMSG
109240941102     C                   SETON                                        409028
109250920320     C                   GOTO      ENDCT7
109260920320     C                   END
109270941102     C*
109280941102     C                   Z-ADD     G02DAT        VIDDCR
109290941102     C*
109300920320     C* DATA CONSEGNA NO < DATA SPEDIZIONE
109310930507     C     G02INV        IFLT      ARBDSP
109320941102     C                   MOVEL     MSG(7)        VIDMSG
109330941102     C                   SETON                                        409028
109340920320     C                   GOTO      ENDCT7
109350930507     C                   END
109360941102     C                   Z-ADD     G02INV        COMDCR            8 0
109370941102     C                   ENDIF
109380070308     c
109390070308      * La data non può superare i gg limiti indicati in tab. VPO
109400070308     c                   If        Not *In90
109410070308     c                   Eval      wdataiso = wdataoggi
109420070308     c                   Adddur    §vpodcr:*d    wdataiso
109430070308     c     *iso          move      wdataiso      ds_data
109440070308     c                   Move      §vpodcr       w003a
109450070308     c                   Movea     w003a         ski(13)
109460070308     c                   Z-add     13            i
109470070308     c                   ExSr      abbl
109480070308     c                   Movea     ski(13)       w003a
109490070308     c                   If        ComDcr > ds_data
109500070308     c                   Eval      *In28 = *On
109510070308     c                   Eval      *In40 = *On
109520070308     c                   Eval      *In90 = *On
109530070308     c                   Eval      VidMsg = msg(129)
109540070308     c                   Eval      VidMsg = %replace(w003a:VidMsg:
109550070308     c                                      %scan('___':VidMsg))
109560070308     C                   GOTO      ENDCT7
109570070308     c                   EndIf
109580070308     c                   EndIf
109590141029     c* Se dcr>0 errore forzabile se presente Fermo Deposito
109600141029     c                   if        d48ffr<>'E'
109610141029     c                   if        comdcr>0 and comdcr<>arbdcr and
109620141029     c                             arbffd<>*blanks and wforzdcr=0
109630141029     C                   MOVEL     MSG(154)      VIDMSG
109640141029     C                   SETON                                        409028
109650141029     C                   EVAL      Wforzdcr=1
109660141029     C                   GOTO      ENDCT7
109670141029     c                   endif
109680141029     c                   endif
109690941102     C**
109700941102     C** ORA CONSEGNA RICHIESTA
109710941102     C**
109720920320     C                   MOVEL     VIDHCR        HM                2 0
109730920320     C     HM            IFLT      0
109740920320     C     HM            ORGT      24
109750941102     C                   MOVEL     MSG(8)        VIDMSG
109760941102     C                   SETON                                        419028
109770920320     C                   GOTO      ENDCT7
109780920320     C                   END
109790941102     C**
109800941102     C** MINUTI CONSEGNA RICHISTA
109810941102     C**
109820920320     C                   MOVE      VIDHCR        HM                2 0
109830920320     C     HM            IFLT      0
109840920320     C     HM            ORGT      59
109850941102     C                   MOVEL     MSG(8)        VIDMSG
109860941102     C                   SETON                                        419028
109870920320     C                   GOTO      ENDCT7
109880920320     C                   END
109890930507     C*
109900941102     C* SE IMMESSO IL TIPO IMMETTERE ANCHE L'ORA O LA DATA
109910930507     C     VIDTCR        IFNE      ' '
109920941102     C     VIDDCR        IFEQ      0
109930941102     C     VIDHCR        ANDEQ     0
109940941102     C                   MOVEL     MSG(9)        VIDMSG
109950941102     C                   SETON                                        409028
109960930507     C                   GOTO      ENDCT7
109970930507     C                   END
109980930507     C                   END
109990070308     c
110000070308     c* Se lna  attivata con GEO, non posso inserire solo tipo e ora:
110010070308     c*  data sempre obbligatoria (se inserito in arrivo, accetto
110020070308     c*  dalla partenza)
110030100419     c* Escludo per le causali di ripristino
110040070319     c                   if        FGSgeot='S' and viddcr=0 and vidhcr>0
110050100419     c                             and d48cvb <> 'CA'
110060070308     c                   if        vidtcr<>arbtcr or vidhcr<>arbhcr or
110070070308     c                             comdcr<>arbdcr
110080070308     C                   MOVEL     MSG(140)      VIDMSG
110090070308     C                   SETON                                        409028
110100070308     C                   GOTO      ENDCT7
110110070308     C                   ENDif
110120070308     C                   ENDif
110130930507     C**
110140941102     C* GIORNI DI CHIUSURA
110150941102     C**
110160930507     C     VIDGC1        IFNE      *BLANKS
110170930507     C                   MOVEL     VIDGC1        ACOM              1
110180930507     C                   MOVE      VIDGC1        BCOM              1
110190941212     C                   Z-ADD     1             B                 2 0
110200930507     C     ACOM          LOOKUP    GGG(B)                                 32
110210941102     C     *IN32         IFEQ      *OFF
110220941102     C                   MOVEL     MSG(10)       VIDMSG
110230941102     C                   SETON                                        429028
110240941102     C                   GOTO      ENDCT7
110250941102     C                   ENDIF
110260930507     C*
110270930507     C     BCOM          IFNE      ' '
110280930507     C     BCOM          ANDNE     'P'
110290930507     C     BCOM          ANDNE     'M'
110300941102     C                   MOVEL     MSG(10)       VIDMSG
110310941102     C                   SETON                                        429028
110320930507     C                   GOTO      ENDCT7
110330930507     C                   END
110340041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
110350041130if  2c                   If         *In10 and VidGc1 <> ArbGc1 and
110360041130     c                             (VidGc1 = ' M' or VidGc1 = ' P')
110370041130     c                   Eval      *In28 = *On
110380041130     c                   Eval      *In42 = *On
110390041130     c                   Eval      *In90 = *On
110400041130     c                   Eval      VidMsg = msg(10)
110410041130e   2c                   EndIf
110420930507     C                   END
110430930507     C*
110440930507     C*** 2° GIORNO CHIUSURA ***
110450930507     C     VIDGC2        IFNE      *BLANKS
110460930507     C                   MOVEL     VIDGC2        ACOM              1
110470930507     C                   MOVE      VIDGC2        BCOM              1
110480930507     C                   Z-ADD     1             B
110490930507     C     ACOM          LOOKUP    GGG(B)                                 30
110500941102     C     *IN30         IFEQ      *OFF
110510941102     C                   MOVEL     MSG(10)       VIDMSG
110520941102     C                   SETON                                        439028
110530941102     C                   GOTO      ENDCT7
110540941102     C                   ENDIF
110550930507     C*
110560930507     C     BCOM          IFNE      ' '
110570930507     C     BCOM          ANDNE     'P'
110580930507     C     BCOM          ANDNE     'M'
110590941102     C                   MOVEL     MSG(11)       VIDMSG
110600941102     C                   SETON                                        439028
110610930507     C                   GOTO      ENDCT7
110620930507     C                   END
110630930507     C*
110640930507     C* I GIORNI DI CHIUSURA NON POSSO ESSERE UGUALI
110650930507     C     VIDGC1        IFEQ      VIDGC2
110660941102     C                   MOVEL     MSG(12)       VIDMSG
110670941102     C                   SETON                                        439028
110680930507     C                   GOTO      ENDCT7
110690930507     C                   END
110700930507     C* NON POSSONO ESSERE ' P' ' M'
110710930507     C     VIDGC1        IFEQ      ' M'
110720930507     C     VIDGC2        ANDEQ     ' P'
110730930507     C     VIDGC1        OREQ      ' P'
110740930507     C     VIDGC2        ANDEQ     ' M'
110750941102     C                   MOVEL     MSG(13)       VIDMSG
110760941102     C                   SETON                                        429028
110770930507     C                   GOTO      ENDCT7
110780930507     C                   END
110790041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
110800041130if  2c                   If         *In10 and VidGc2 <> ArbGc2 and
110810041130     c                             (VidGc2 = ' M' or VidGc2 = ' P')
110820041130     c                   Eval      *In28 = *On
110830041130     c                   Eval      *In43 = *On
110840041130     c                   Eval      *In90 = *On
110850041130     c                   Eval      VidMsg = msg(10)
110860041130e   2c                   EndIf
110870930507     C                   END
110880941102     C*
110890941102     C* CONSEGNE PARTICOLARi
110900020603     c* Verifico se destinatario disagiato
110910020604     c* per le bolle poste non lo faccio
110920020604     c                   clear                   wdisag
110930020604     c     lnpntw        ifne      'PPT'
110940020603     c                   clear                   tisit0ds
110950020603     c                   movel     'E'           it0tla
110960020603     c                   movel     arbnzd        it0naz
110970020603     c                   movel     arbprd        it0prv
110980020603     c                   movel     arbcad        it0cap
110990020603     c                   movel     arblod        it0loc
111000020603     c                   movel     arbind        it0ind
111010020603     c                   movel     arbrsd        it0rag
111020020603     c                   call      'TISIT5R'
111030020603     c                   parm                    tisit0ds
111040020603     c     ot0err        ifeq      *blanks
111050060203   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
111060060203     c                             ot0dos = 'S'
111070020603     c                   movel     ot0dos        wdisag
111080020603     c     wdisag        ifeq      'D'
111090020603     c                   movel     'X'           wdisag
111100020603     c                   endif
111110060203     c                   endif
111120020603     c                   else
111130020603     c                   clear                   wdisag
111140020603     c                   endif
111150020604     c                   endif
111160020603     c
111170941102     C*
111180941102     C* 11ON - NON POSO CAMBIARE LA 1 CONSEGNA PARTICOLARE
111190070910     c* Se ho modificato il destinatario è non è stata immessa in
111200070910     c*  partenza, devo per forza toglierla
111210070910     c                   if        *in11=*on and wdisag<>vidtc1 and
111220070910     c                             arbtc1=vidtc1 and par5mdp='S' and
111230070910     c                             par5gtc1<>vidtc1 and par5gtc2<>vidtc1
111240070910     C                   MOVEL     MSG(144)      VIDMSG
111250070910     c                   eval      %subst(vidmsg:27:1) = vidtc1
111260070910     c                   eval      %subst(vidmsg:29:8) = v7dtc1
111270070910     C                   SETON                                        449028
111280070910     C                   GOTO      ENDCT7
111290070910     c                   endif
111300070910     c
111310051116     c****               if        *in11 = *on and wdisag <> *blanks
111320070910     c* se consegna particolare non modificabile ma è stato immesso
111330070910     c* unacons.part.disagiato/supemercat... do' lo stesso la
111340070910     c* possibilita di cambiarla se la combinazione fra le due non è
111350070910     c* ammessa
111360051116     c                   clear                   wcomb
111370070910    0c                   if        *in11=*on and par5mdp='S' and
111380070910     c                             (arbtc1=par5gtc1 or arbtc1=par5gtc2)
111390070910     c
111400070910    1c                   if        vidtc2<>*blanks or wdisag<>*blanks
111410070910     c                             or vidtc1<>arbtc1
111420070910     c*
111430070910     c***                if        *in11 = *on and (vidtc2 <> *blanks or
111440070910     c***                          wdisag <> *blanks or vidtc1 <> arbtc1)
111450051114     c                   move      arbtc1        wtc1
111460051117     c
111470051116     c                   if        wdisag <> *blanks
111480051116     c                   move      wdisag        wtc2
111490051117     c                   exsr      sr_chkcomb
111500051117     c                   endif
111510051117     c                   if        wcomb = *blanks and vidtc1 <> arbtc1
111520051117     c                   move      vidtc1        wtc2
111530051117     c                   exsr      sr_chkcomb
111540051117     c                   endif
111550051117     c                   if        wcomb = *blanks and vidtc2 <> *blanks
111560051117     c                   move      vidtc2        wtc2
111570051117     c                   exsr      sr_chkcomb
111580051117     c                   endif
111590070910    1c                   endif
111600070910     c***
111610070910    0C***  *IN11         IFEQ      *ON
111620070910     c***  wdisag        andeq     *blanks
111630070910    0c***                if        *in11 = *on and wcomb = *blanks
111640070910     c
111650070910    1c                   if        wcomb=*blanks
111660941102     C*
111670070910    2C     VIDTC1        IFNE      ARBTC1
111680941102     C                   MOVEL     MSG(15)       VIDMSG
111690051117     c                   eval      %subst(vidmsg:36:1) = arbtc1
111700941102     C                   SETON                                        449028
111710941102     C                   GOTO      ENDCT7
111720070910    2c                   endif
111730070910    1c                   endif
111740070910    0c                   endif
111750070910     c
111760070910     c***                else
111770070910     C***                MOVEL     '1P'          COD
111780070910     C***                MOVEL(P)  VIDTC1        KEY
111790070910     C***                EXSR      CTABEL
111800070910     C***                MOVEL     TBLUNI        V7DTC1
111810070910    1C***                ENDIF
111820941102     C*
111830070910   X0C***                ELSE
111840941102     C*
111850941102     C                   CLEAR                   V7DTC1
111860941102     C*
111870941102    1C     VIDTC1        IFNE      ' '
111880941102     C* RICERCA
111890941102    2C     VIDTC1        IFEQ      '?'
111900941102     C                   MOVEL     CODUT         §KUT
111910941102     C                   MOVEL     '1P'          §COD
111920941102     C                   MOVE      *BLANKS       VIDTC1
111930991011     C                   CLEAR                   §KEY
111940941102     C                   CALL      'X§TABER'
111950941102     C                   PARM                    §KUT
111960941102     C                   PARM                    §COD
111970941102     C                   PARM                    §KEY
111980941209     C                   PARM                    §DES
111990941102     C                   MOVEL     §KEY          VIDTC1
112000941102     C                   MOVEL     §DES          V7DTC1
112010941102     C                   SETON                                        9044
112020941102     C                   GOTO      ENDCT7
112030941102    2C                   ENDIF
112040941102     C*
112050941102     C                   MOVEL     '1P'          COD
112060941102     C                   MOVEL(P)  VIDTC1        KEY
112070941212     C                   EXSR      CTABEL
112080941102     C*
112090941102    2C     *IN30         IFEQ      *ON
112100941102     C                   MOVEL     MSG(11)       VIDMSG
112110941102     C                   SETON                                        449028
112120941102     C                   GOTO      ENDCT7
112130941102    2C                   ENDIF
112140941102     C*
112150941102     C                   MOVEL     TBLUNI        DS1P
112160941102     C*
112170941102     C* NON UTILIZZABILE IN ARRIVO
112180950104    2C   10§1PUAR        IFNE      'S'
112190020318     C     LNPNTW        ANDNE     'PPT'
112200020318     C     LNPNTW        OREQ      'PPT'
112210000510     C     §1PFPC        ANDNE     'E'
112220000510     C     §1PFPC        ANDNE     'A'
112230040705      * e non è quella presa in automatico
112240051114     c                   If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
112250070914     c* Verifico se era stata messa in partenza ma tolta
112260070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
112270070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
112280941102     C                   MOVEL     MSG(11)       VIDMSG
112290941102     C                   SETON                                        449028
112300941102     C                   GOTO      ENDCT7
112310040705     c                   EndIf
112320070914     c                   EndIf
112330941102    2C                   END
112340060131     c
112350060131     c* SE devo considerare le abilitati, verifico se il profilo
112360060131     c*  può inserire la cons particolare, se non è presa in automatico
112370060131     c   10              If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
112380070914     c                             and  §utemfcp=' '
112390070914     c******                       and dateu>=§vpoacp and  §utemfcp=' '
112400070914     c*
112410070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
112420070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
112430060131     C*
112440060131     C                   MOVEL     'SP'          COD
112450060131     C                   MOVEL(P)  VIDTC1        KEY
112460060131     C                   EXSR      CTABEL
112470060131     C*
112480060131    2C     *IN30         IFEQ      *ON
112490060131     c                   clear                   dssp
112500060131     c                   else
112510060131     c                   movel     tbluni        DSSP
112520060131     c                   endif
112530060131     c
112540060131     c                   if        §spaba='S'
112550060131     C                   MOVEL     MSG(138)      VIDMSG
112560060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
112570060131     C                   SETON                                        449028
112580060131     C                   GOTO      ENDCT7
112590060131     c                   endif
112600060131     c                   endif
112610070914     c
112620070914     c                   endif
112630060131     c
112640950104     C* NON UTILIZZABILE IN PARTENZA
112650950104    2C  N10§1PUPR        IFNE      'S'
112660020318     C     LNPNTW        ANDNE     'PPT'
112670020318     C     LNPNTW        OREQ      'PPT'
112680000510     C     §1PFPC        ANDNE     'E'
112690000510     C     §1PFPC        ANDNE     'P'
112700950104     C                   MOVEL     MSG(11)       VIDMSG
112710950104     C                   SETON                                        449028
112720950104     C                   GOTO      ENDCT7
112730950104    2C                   END
112740000510     C* PER LE POSTE CONTROLLO IL SUO FLAG
112750941102     C*
112760941102     C                   MOVEL     TBLUNI        V7DTC1
112770150414     c* controlli per EXPO
112780151119     c*                  if        vidtc1='Z'
112790151119     c*                  exsr      sr_chkexpo
112800151119     c*                  if        *in90
112810151119     c*                  seton                                        44
112820151119     c*                  goto      endct7
112830151119     c*                  endif
112840151119     c*                  endif
112850941102     C*
112860941102     C* LA 1 E LA 2 NON POSSONO ESSERE UGUALI
112870941102    2C     VIDTC2        IFEQ      VIDTC1
112880941102     C                   MOVEL     MSG(14)       VIDMSG
112890941102     C                   SETON                                        459028
112900941102     C                   GOTO      ENDCT7
112910941102    2C                   END
112920020731     C*
112930941102     C*
112940941102    1C                   END
112950070910    0C******             END
112960941102     C*
112970941102     C* 12ON - NON POSO CAMBIARE LA 2 CONSEGNA PARTICOLARE
112980070910     c* Se ho modificato il destinatario è non è stata immessa in
112990070910     c*  partenza, devo per forza toglierla
113000070910    0c                   if        *in12=*on and wdisag<>vidtc2 and
113010070910     c                             arbtc2=vidtc2 and par5mdp='S' and
113020070910     c                             par5gtc1<>vidtc2 and par5gtc2<>vidtc2
113030070910     C                   MOVEL     MSG(144)      VIDMSG
113040070910     c                   eval      %subst(vidmsg:27:1) = vidtc2
113050070910     c                   eval      %subst(vidmsg:29:8) = v7dtc2
113060070910     C                   SETON                                        459028
113070070910     C                   GOTO      ENDCT7
113080070910    0c                   endif
113090070910     c
113100051114     c* se consegna particolare non modificabile ma è stato immesso
113110051114     c* un destinatrio disagiato/supemercat... do' lo stesso la
113120051114     c* possibilita di cambiarla se la combinazione fra le due non è
113130051114     c* ammessa
113140051116     c                   clear                   wcomb
113150070910    0c                   if        *in12=*on and par5mdp='S' and
113160070910     c                             (arbtc2=par5gtc1 or arbtc2=par5gtc2)
113170070910
113180070910    1c                   if        vidtc1 <> *blanks or
113190070910     c                             wdisag <> *blanks or vidtc2 <> arbtc2
113200070910     c
113210051116     c***                if        *in12 = *on and wdisag <> *blanks
113220070910     c***
113230070910     c***                if        *in12 = *on and (vidtc1 <> *blanks or
113240070910     c***                          wdisag <> *blanks or vidtc2 <> arbtc2)
113250051114     c                   move      arbtc2        wtc1
113260051116     c                   if        wdisag <> *blanks
113270051116     c                   move      wdisag        wtc2
113280051117     c                   exsr      sr_chkcomb
113290051117     c                   endif
113300051117     c                   if        wcomb = *blanks and vidtc2 <> arbtc2
113310051117     c                   move      vidtc2        wtc2
113320051117     c                   exsr      sr_chkcomb
113330051117     c                   endif
113340051117     c                   if        wcomb = *blanks and vidtc1 <> *blanks
113350051117     c                   move      vidtc1        wtc2
113360051117     c                   exsr      sr_chkcomb
113370051117     c                   endif
113380070910    1c                   endif
113390051117     c*
113400070910    0C***  *IN12         IFEQ      *ON
113410070910    0c***                if        *in12 = *on and wcomb = *blanks
113420070910    1c                   if        wcomb = *blanks
113430941102     C*
113440070910    2C     VIDTC2        IFNE      ARBTC2
113450941102     C                   MOVEL     MSG(15)       VIDMSG
113460051117     c                   eval      %subst(vidmsg:36:1) = arbtc2
113470941102     C                   SETON                                        459028
113480941102     C                   GOTO      ENDCT7
113490070910    2c                   endif
113500070910    1c                   endif
113510070910    0c                   endif
113520070910     c
113530070910     c****               else
113540070910     C****               MOVEL     '1P'          COD
113550070910     C****               MOVEL(P)  VIDTC2        KEY
113560070910     C****               EXSR      CTABEL
113570070910     C****               MOVEL     TBLUNI        V7DTC2
113580070910    1C****               ENDIF
113590941102     C*
113600070910   X0C****               ELSE
113610941102     C*
113620941102     C                   CLEAR                   V7DTC2
113630941102     C*
113640941102    1C     VIDTC2        IFNE      ' '
113650941102     C* RICERCA
113660941102    2C     VIDTC2        IFEQ      '?'
113670941102     C                   MOVEL     CODUT         §KUT
113680941102     C                   MOVEL     '1P'          §COD
113690941102     C                   MOVE      *BLANKS       VIDTC2
113700991011     C                   CLEAR                   §KEY
113710941102     C                   CALL      'X§TABER'
113720941102     C                   PARM                    §KUT
113730941102     C                   PARM                    §COD
113740941102     C                   PARM                    §KEY
113750941209     C                   PARM                    §DES
113760941102     C                   MOVEL     §KEY          VIDTC2
113770941102     C                   MOVEL     §DES          V7DTC2
113780941102     C                   SETON                                        9045
113790941102     C                   GOTO      ENDCT7
113800941102    2C                   ENDIF
113810941102     C*
113820941102     C                   MOVEL     '1P'          COD
113830941102     C                   MOVEL(P)  VIDTC2        KEY
113840941212     C                   EXSR      CTABEL
113850941102     C*
113860941102    2C     *IN30         IFEQ      *ON
113870941102     C                   MOVEL     MSG(11)       VIDMSG
113880941102     C                   SETON                                        459028
113890941102     C                   GOTO      ENDCT7
113900941102    2C                   ENDIF
113910941102     C*
113920941102     C                   MOVEL     TBLUNI        DS1P
113930941102     C*
113940941102     C* NON UTILIZZABILE IN ARRIVO
113950950104    2C   10§1PUAR        IFNE      'S'
113960020318     C     LNPNTW        ANDNE     'PPT'
113970020318     C     LNPNTW        OREQ      'PPT'
113980000518     C     §1PFPC        ANDNE     'E'
113990000518     C     §1PFPC        ANDNE     'A'
114000040705      * e non è quella presa in automatico
114010051114     c                   If        Vidtc2 <> wdisag  and vidtc2 <> arbtc2
114020070914     c* Verifico se era stata messa in partenza ma tolta
114030070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
114040070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
114050941102     C                   MOVEL     MSG(11)       VIDMSG
114060941102     C                   SETON                                        459028
114070941102     C                   GOTO      ENDCT7
114080040705     c                   EndIf
114090070914     c                   EndIf
114100941102    2C                   END
114110060131     c
114120060131     c* SE devo considerare le abilitati, verifico se il profilo
114130060131     c*  può inserire la cons particolare, se non è presa in automatico
114140060131     c   10              If        Vidtc2 <> wdisag and vidtc2 <> arbtc2
114150070914     c                             and  §utemfcp=' '
114160070514     c****                         and dateu>=§vpoacp and  §utemfcp=' '
114170070914     c* Verifico se era stata messa in partenza ma tolta
114180070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
114190070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
114200060131     C*
114210060131     C                   MOVEL     'SP'          COD
114220060131     C                   MOVEL(P)  VIDTC2        KEY
114230060131     C                   EXSR      CTABEL
114240060131     C*
114250060131    2C     *IN30         IFEQ      *ON
114260060131     c                   clear                   dssp
114270060131     c                   else
114280060131     c                   movel     tbluni        DSSP
114290060131     c                   endif
114300060131     c
114310060131     c                   if        §spaba='S'
114320060131     C                   MOVEL     MSG(138)      VIDMSG
114330060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
114340060131     C                   SETON                                        459028
114350060131     C                   GOTO      ENDCT7
114360060131     c                   endif
114370060131     c                   endif
114380070914     c                   endif
114390070914     c
114400950104     C* NON UTILIZZABILE IN PARTENZA
114410950104    2C  N10§1PUPR        IFNE      'S'
114420020318     C     LNPNTW        ANDNE     'PPT'
114430020318     C     LNPNTW        OREQ      'PPT'
114440000518     C     §1PFPC        ANDNE     'E'
114450000518     C     §1PFPC        ANDNE     'P'
114460950104     C                   MOVEL     MSG(11)       VIDMSG
114470950104     C                   SETON                                        459028
114480950104     C                   GOTO      ENDCT7
114490950104    2C                   END
114500941102     C*
114510941102     C                   MOVEL     TBLUNI        V7DTC2
114520150414     c* controlli per EXPO
114530151119     c*                  if        vidtc2='Z'
114540151119     c*                  exsr      sr_chkexpo
114550151119     c*                  if        *in90
114560151119     c*                  seton                                        45
114570151119     c*                  goto      endct7
114580151119     c*                  endif
114590151119     c*                  endif
114600941102     C*
114610941102    1C                   END
114620070910    0C****               END
114630060131     c
114640020531     c* Se destinatario disagiato o supermercato errore se non indicato
114650020531     c* in nessuno dei due campi di consegna particolare
114660041015     c*
114670041015     c* Se particolarità varia="U" --> consegna ad uffici devo invece
114680041015     c* dare errore se immesso supermercato o appuntamento
114690041015     c                   if        %subst(arbgva:1:1)='U'
114700041015     c                             and (vidtc1='S' or vidtc1='A' or vidtc2='S'
114710041015     c                             or vidtc2='A')
114720041015     C                   MOVEl     msg(131)      vidmsg
114730041015     C                   SETON                                        449028
114740041015     C                   GOTO      ENDCT7
114750041015     c                   endif
114760150210
114770150210     c* se sono in partenza e immessa la "A" e la data --> errore solo la data immettere
114780150210     c                   if        not *in10 and (vidtc1='A' or vidtc2='A') and
114790150210     c                             comdcr>0
114800150210     c                   if        (par5mdp='S' and par5gtc1<>'A' and par5gtc2
114810150210     c                                                    <> 'A')
114820150210     c                             or (par5mdp=' ' and arbtc1<>'A' and arbtc2
114830150210     c                                                    <> 'A')
114840150210     C                   MOVEl     msg(158)      vidmsg
114850150210     C                   SETON                                        449028
114860150210     C                   GOTO      ENDCT7
114870150210     c                   endif
114880150210     c                   endif
114890150119
114900150119     c* se destinatario particolare che prevede appuntamento controllo se presente
114910150119     c* DCR immessa dalla partenza perchè in questo caso non devo dare errore per la
114920150119     c* mancanza della "A" di appuntamento
114930150119     c                   eval      w_noapp=' '
114940150119     c                   if        wdisag='A'
114950150119     c                   if        (par5mdp='S' and par5gdcr>*zeros
114960150119     c                                          and par5gtcr=' ')
114970150119     c                             or (par5mdp=' ' and arbdcr>0 and arbtcr= ' ')
114980150119     c                   eval      w_noapp='1'
114990150119     c                   endif
115000150119     c                   endif
115010041015     c*
115020041015     C***  wdisag        ifne      *blanks
115030041015     c***  wdisag        andne     vidtc1
115040041015     c***  wdisag        andne     vidtc2
115050150119     c                   if        wdisag <> *blanks and w_noapp=' ' and
115060041015     c                             %subst(arbGva:1:1) <> 'U' or
115070041015     c                             wdisag='X' and %subst(arbGva:1:1)='U'
115080041015     c     wdisag        ifne      vidtc1
115090041015     c     wdisag        andne     vidtc2
115100020603     C                   MOVEa     msg(112)      k78
115110020531     c                   select
115120020531     c     wdisag        wheneq    'X'
115130020531     c                   eval      w014a = 'disagiato     '
115140020531     c     wdisag        wheneq    'S'
115150020531     c                   eval      w014a = '"supermercato"'
115160040705     c     wdisag        wheneq    'A'
115170040705     c                   eval      w014a = '"appuntamento"'
115180020531     c                   endsl
115190020531     C                   MOVEa     w014a         k78(14)
115200020531     C                   MOVEA     K78           VIDMSG
115210020603     C                   SETON                                        449028
115220020531     C                   GOTO      ENDCT7
115230020531     c                   endif
115240041015     c                   endif
115250070907     C* Non è possibile indicare contemporaneamente alcune consegne    i
115260070907     C*  particolari
115270051114     c                   move      vidtc1        wtc1
115280051114     c                   move      vidtc2        wtc2
115290051114     c                   exsr      sr_chkcomb
115300051114     c                   if        wcomb = 'S'
115310051114     C                   movel     MSG(113)      VIDmsg
115320051116     c                   eval      %subst(vidmsg:63:1) = vidtc1
115330051116     c                   eval      %subst(vidmsg:69:1) = vidtc2
115340051114     C                   seton                                        459028
115350051114     C                   goto      ENDCT7
115360051114    2C                   ENDIF
115370941102     C*
115380920320     C     ENDCT7        ENDSR
115390021129      **
115400021129      *--- CONTROLLI CAMPI CONSEGNA RICHIESTA SUPERMERCATI -----------*
115410021129     C     CONTR71       BEGSR
115420021129     C                   SETOFF                                       90
115430021129
115440030123      * Se la spedizione non ha già la consegna particolare 'S'
115450030123     c                   If        ArbTc1 <> 'S' and ArbTc2 <> 'S'
115460030123      * E' obbligatorio inserire la consegna
115470030123     c                   If        (VidTc1 = *Blanks and VidTc2 = *Blanks)
115480030123     c                   Eval      VidMsg = Msg(117)
115490030123     c                   Eval      *In44 = *On
115500030123     c                   Eval      *In28 = *On
115510030123     c                   Eval      *In90 = *On
115520030123     c                   GoTo      EndCt71
115530030123     c                   EndIf
115540030127      * Se ci sono già tutte e 2 almeno una deve essere 'S'
115550030127     c                   If        VidTc1 <> 'S' and VidTc2 <> 'S'
115560030127     c                   Eval      VidMsg = Msg(122)
115570030127     c                   Eval      *In44 = *On
115580030127     c                   Eval      *In28 = *On
115590030127     c                   Eval      *In90 = *On
115600030127     c                   GoTo      EndCt71
115610030127     c                   EndIf
115620030123      * Se la prima consegna c'è già e non è supermercato devo mettere 'S' nella seconda
115630030127     c                   If        ArbTc1 <> *Blanks and VidTc1 <> 'S' and
115640030127     c                             VidTc2 = *Blanks
115650030123     c                   Eval      VidMsg = Msg(117)
115660030123     c                   Eval      *In45 = *On
115670030123     c                   Eval      *In28 = *On
115680030123     c                   Eval      *In90 = *On
115690030123     c                   GoTo      EndCt71
115700030123     c                   EndIf
115710030123      * Se la seconda consegna c'è già e non è supermercato devo mettere 'S' nella prima
115720030127     c                   If        ArbTc2 <> *Blanks and VidTc2 <> 'S' and
115730030127     c                             VidTc1 = *Blanks
115740030123     c                   Eval      VidMsg = Msg(117)
115750030123     c                   Eval      *In45 = *On
115760030123     c                   Eval      *In28 = *On
115770030123     c                   Eval      *In90 = *On
115780030123     c                   GoTo      EndCt71
115790030123     c                   EndIf
115800021129     c                   EndIf
115810021129
115820021129      * Se è stata inserita la consegna particolare 'S' si devono
115830021129      * immettere anche la persona da contattare, il telefono e il motivo
115840021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
115850021129      * Nominativo
115860021129     c                   If        VidNom = *Blanks
115870021129     c                   Eval      VidMsg = Msg(118)
115880021129     c                   Eval      *In57 = *On
115890021129     c                   Eval      *In28 = *On
115900021129     c                   Eval      *In90 = *On
115910021129     c                   GoTo      EndCt71
115920021129     c                   EndIf
115930021129      * Telefono
115940021129     c                   If        VidTel = *Blanks
115950021129     c                   Eval      VidMsg = Msg(119)
115960021129     c                   Eval      *In58 = *On
115970021129     c                   Eval      *In28 = *On
115980021129     c                   Eval      *In90 = *On
115990021129     c                   GoTo      EndCt71
116000041007     c**                 Else
116010041007     c**                 Clear                   Trul42Ds
116020041007     c**                 Movel     VidTel        D42Fax
116030041007     c**                 Call      'TRUL42R'
116040041007     c***                Parm                    Trul42Ds
116050041007     c***                If        D42Err = '1'
116060041007     c****               Eval      *In58 = *On
116070041007     c**                 Eval      *In28 = *On
116080041007     c**                 Eval      *In90 = *On
116090041007     c**                 Eval      VidMsg = D42Msg
116100041007     c**                 GoTo      EndCt71
116110041007     c**                 EndIf
116120041007     c                   EndIf
116130021129      * Motivo
116140070406      * non più obbligatorio per evitare che scrivano "bagianate"
116150070406     c**                 If        VidMot = *Blanks
116160070406     c***                Eval      VidMsg = Msg(120)
116170070406     c**                 Eval      *In59 = *On
116180070406     c**                 Eval      *In28 = *On
116190070406     c**                 Eval      *In90 = *On
116200070406     c**                 GoTo      EndCt71
116210070406     c**                 EndIf
116220021129     c                   EndIf
116230021129
116240021129     c     EndCt71       EndSr
116250030930      **
116260030930      *--- CONTROLLO SE POSSIBILE LA MANUTENZIONE DELLA DATA CONSEGNA RICHIESTA ---*
116270030930     c     Contr72       BegSr
116280050907
116290030930     c                   Eval      *In90 = *Off
116300070306     c                   clear                   aggioarp          1
116310050907
116320030930      * Se tipo o data o ora consegna richiesta sono state variate
116330031001If  1c                   If        VidTcr <> ArbTcr or
116340031030     c                             ComDcr <> ArbDcr or
116350030930     c                             VidHcr <> ArbHcr
116360150205     c
116370150205     c* permetto sempre la variazione della cons richiesta se sono in partenza
116380160115     c                   if        not *in10 or wfile='P'
116390150205     c                   Goto      EndCt72
116400150205     c                   EndIf
116410150205     c
116420051109      * Se causale 'CS' e giacenza aperta in fase 30 con appuntamento (sulla giacenza)
116430051109      * posso variare la data
116440051109      * quindi bypasso tutti gli altri controlli
116450051109     c                   If        d48cvb = 'CS' and *In04 and wgcfas = 30 and
116460051109     c                             d§gcpapp = 'A'
116470051109     c                   Eval      wgiac = 'S'
116480051109     c                   Goto      EndCt72
116490051109     c                   EndIf
116500040224      * non posso modificare se la bolla ha giacenza aperta (IN04 ON)
116510050907      * e se la fase non è = a 35 o se la bolla è bloccata come giacenza
116520050907if  2c                   If        *In04 and (wgcfas <> 35 or arbfbc = 'G')
116530070312     c                   Eval      VidMsg = Msg(141)
116540040220     c                   Eval      *In28 = *On
116550040220     c                   Eval      *In90 = *On
116560040220     c                   If        VidTcr <> ArbTcr
116570040220     c                   Eval      *In62 = *On
116580040220     c                   GoTo      EndCt72
116590040220     c                   EndIf
116600040220     c                   If        ComDcr <> ArbDcr
116610040220     c                   Eval      *In40 = *On
116620040220     c                   GoTo      EndCt72
116630040220     c                   EndIf
116640040220     c                   If        VidHcr <> ArbHcr
116650040220     c                   Eval      *In41 = *On
116660040220     c                   GoTo      EndCt72
116670040220     c                   EndIf
116680040220    2c                   EndIf
116690040324      * posso modificare se la bolla ha giacenza aperta (IN04 ON) e la fase è = 35
116700040330      * ma solo se metto la data consegna richiesta maggiore/uguale alla data eseguibilità giacenza
116710040324if  2c                   If        *In04 and wgcfas = 35
116720040330if  3c                   If        wgcded > *Zeros and ComDcr >= wgcded
116730040324     c                   Eval      wgiac = 'S'
116740040324     c                   GoTo      EndCt72
116750040324    3c                   EndIf
116760040330if  3c                   If        wgcded = *Zeros and ComDcr >= wgcddm
116770040324     c                   Eval      wgiac = 'S'
116780040324     c                   GoTo      EndCt72
116790040324    3c                   EndIf
116800040324     c                   Eval      VidMsg = Msg(128)
116810040324     c                   Eval      *In28 = *On
116820040324     c                   Eval      *In90 = *On
116830040324     c                   If        VidTcr <> ArbTcr
116840040324     c                   Eval      *In62 = *On
116850040324     c                   GoTo      EndCt72
116860040324     c                   EndIf
116870040324     c                   If        ComDcr <> ArbDcr
116880040324     c                   Eval      *In40 = *On
116890040324     c                   GoTo      EndCt72
116900040324     c                   EndIf
116910040324     c                   If        VidHcr <> ArbHcr
116920040324     c                   Eval      *In41 = *On
116930040324     c                   GoTo      EndCt72
116940040324     c                   EndIf
116950040324    2c                   EndIf
116960050907      * richiamo il controllo della "7Q"
116970050907     c                   ExSr      Contr7q
116980030930      * posso modificare se
116990031001      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
117000031001If  2c                   If        V1cPkf >= §VpoPkg or
117010031001     c                             V1cVlf >= §VpoVlm or
117020031001      *                     Destinatario supermercato o Apputamento
117030031001     c                             VidTc1 = 'S' or VidTc1 = 'A' or
117040031001     c                             VidTc2 = 'S' or VidTc2 = 'A' or
117050150506     c                             VidTc1 = 'Z' or VidTc2 = 'Z' or
117060031001      *                     Lasciato avviso
117070031001     c                             ArbFbc = 'A' or
117080031001      *                     Fermo deposito
117090031114     c                             ArbFfd = 'S' or
117100031114      *                     Tipo bolla "FW" Recapito c/assegni
117110050907     c                             ArbCbo = 'FW' or
117120050907      *                     Periodo trazione ridotte e bolla con particolarità "G8"
117130050907     c                             wDCRG8 = 'S'
117140031001     c                   GoTo      EndCt72
117150031001    2c                   EndIf
117160070320     c
117170070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
117180070320     c*  manutenzionare la spedizione
117190070920     c***                Clear                   dAr5Gen
117200070920     c***                Eval      kAr5Trd = 'GEN'
117210070920     c***  kAr5          Chain(n)  Fiar501l
117220070920     c***                If        %Found(Fiar501l)
117230070920     c***                Movel     Ar5Uni        dAr5Gen
117240070920     c***                if        §ar5MDP='S'  and §ar5FFD='S'
117250070920     c                   if        Wesar5gen='S' and par5mdp='S' and
117260070920     c                             par5FFD='S'
117270070320     c                   GoTo      EndCt72
117280070320    2c                   EndIf
117290070920    2c***                EndIf
117300070312    c
117310070312     c* Se la bolla non è in distinta, non c'e' blocco giacenza e c'e'
117320070312     c*  un evento che prevede la modifica della cons, richiesta
117330070312     c*  allora posso inserirla ed aggiornare direttamente ARB+fiar5
117340070319     c                   if        arbndc=0 and FGSgeot='S' and
117350070312     c                             arbfbc<>'G' and eveADR='S'
117360070312     c                   GoTo      EndCt72
117370070312    3c                   EndIf
117380070312     c
117390031001      *                     C/assegno non abilitato
117400031001If  2c                   If        Ar9Cas > 0
117410031001     c                   Clear                   Trul21ds
117420031001     c                   Eval      I21Tla = 'L'
117430031001     c                   Eval      I21Cbo = ArbCbo
117440031001     c                   Eval      I21Tsp = ArbTsp
117450031001     c                   Eval      I21Lnp = ArbLnp
117460031001     c                   Eval      I21Nzm = ArbNzm
117470031001     c                   Eval      I21Lna = ArbLna
117480031001     c                   Eval      I21Nzd = ArbNzd
117490130827     c****               Eval      I21Ksc = ArbKsc
117500031001     c                   Movel     ArbCtr        I21Ctr
117510031001     c                   Eval      I21Tic = Ar9Tic
117520031001     c                   Eval      I21Imp = Ar9Cas
117530031001     c                   Eval      I21Div = Ar9Vca
117540031001     c                   Eval      I21Ute = Knmus
117550031001     c                   Eval      I21Pgm = nompgm
117560130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
117570130827     c                   if        flgtb1='F'
117580130827     c                   z-add     arbksc        i21ksc
117590130827     c                   else
117600130827     c                   z-add     arbccm        i21ksc
117610130827     c                   if        i21ksc=0
117620130827     c                   z-add     8888          i21ksc
117630130827     c                   movel     arblnp        i21ksc
117640130827     c                   endif
117650130827     c                   endif
117660031001     c                   Call      'TRUL21R'
117670031001     c                   Parm                    Trul21ds
117680031001If  3c                   If        O21Fca <> *Blanks
117690031001     c                   GoTo      EndCt72
117700031001    3c                   EndIf
117710031001    2c                   EndIf
117720070309     c
117730070309     c* Se la bolla è in distinta, non devo però aggiornare
117740070306     c*  fnarb ma FIARP
117750111025    2c                   if        arbndc>0 and FGSgeot='S'
117760111025     c
117770111025     c* Se posso, scrivo FIARP, altrimenti subiro errore
117780111025    3c                   if        DcrinDist<>'S'
117790111025      * erorre
117800111025     c                   Eval      VidMsg = Msg(126)
117810111025     c                   Eval      *In28 = *On
117820111025     c                   Eval      *In90 = *On
117830111025     c                   Eval      *In40 = *On
117840111025     c                   GoTo      EndCt72
117850111025     c
117860111025   x3c                   else
117870070308     c                   eval      Aggioarp='S'
117880111025    4c                   if        comdcr<>savdcr  or vidtcr<>savtcr or
117890070308     c                             vidhcr<>savhcr
117900070309     c* Se richiamo nascosto, imposto solo il campo msgforza,
117910070309     c*  altrimenti emetto il msg
117920070330     c*  non imposto il campo se devo fare solo i controlli (26 OFF)
117930111025    5c                   if        d48ffr<>'E'
117940070308     c                   seton                                        402890
117950070308     c                   Eval      VidMsg = Msg(139)
117960111025   x5c                   else
117970070330     c
117980070330     c                   if        *in26
117990070330     c                   eval      msgforza=142
118000070309     c                   endif
118010111025    5c                   endif
118020070309     c
118030070308     c                   eval      savdcr=comdcr
118040070308     c                   eval      savtcr=vidtcr
118050070308     c                   eval      savhcr=vidhcr
118060111025    4c                   endif
118070111025    3c                   endif
118080070308     c
118090070306     c                   GoTo      EndCt72
118100111025    2c                   endif
118110070306     c
118120040220      * se arrivo qua vuol dire che non posso modificare
118130140806      * Se variazione da internet permetto la modifica
118140140806     c                   if        d48cvb<>'CI'
118150031001     c                   Eval      VidMsg = Msg(126)
118160031001     c                   Eval      *In28 = *On
118170031001     c                   Eval      *In90 = *On
118180031030     c                   If        VidTcr <> ArbTcr
118190031001     c                   Eval      *In62 = *On
118200031001     c                   GoTo      EndCt72
118210031001     c                   EndIf
118220031030     c                   If        ComDcr <> ArbDcr
118230031001     c                   Eval      *In40 = *On
118240031001     c                   GoTo      EndCt72
118250031001     c                   EndIf
118260031001     c                   If        VidHcr <> ArbHcr
118270031001     c                   Eval      *In41 = *On
118280031001     c                   GoTo      EndCt72
118290031001     c                   EndIf
118300140806     c                   endif
118310031001    1c                   EndIf
118320030930
118330030930     c     EndCt72       EndSr
118340920320     C*
118350920320     C*--- AGGIORNAMENTO FILE ----------------------------------------*
118360920320     C     REGIS7        BEGSR
118370070308     c                   clear                   ModconsR          1
118380130412     c                   clear                   wpda_ndc
118390130412     c                   clear                   wpda_ddc
118400130412     c                   clear                   wpda_dcm
118410070308     C* PRENDO I VALORI ASSOLUTI
118420070308     C                   MLLZO     1             VIDHCR
118430070308     c
118440070308     c* Mi salvo i campi della consegna
118450070308     c*  richiesta della bolla per sucessivi aggiornamenti.
118460070308     c                   eval      bolladcr=arbdcr
118470070308     c                   eval      bollahcr=arbhcr
118480070308     c                   eval      bollatcr=arbtcr
118490070308     c
118500070308     c* Mi salvo anche se modificata, solo se c'e' la DATA consegna impostat
118510070308     c                   if        vidtcr<>arbtcr or comdcr<>arbdcr or
118520070308     c                             arbhcr<>vidhcr  and comdcr>0
118530070308     c                   eval      ModconsR='S'
118540070308     c                   endif
118550070308     c
118560950104     C*  GIRO UDATE
118570950104     C                   TIME                    W0140            14 0
118580950104     C* UDATE IN GGMMAAAA
118590950104     C                   MOVE      W0140         WDTGIO            8 0
118600950104     C* UDATE IN AAAAMMGG
118610950104     C                   Z-ADD     WDTGIO        G02DAT
118620950104     C                   MOVEL     *BLANK        G02ERR
118630950104     C                   CALL      'XSRDA8'
118640950104     C                   PARM                    WLBDAT
118650950104     C                   MOVEL     G02INV        DATEU             8 0
118660941213     C                   MOVEL     KNMUS         ARBPRU
118670930507     C*
118680151230     c                   if        not *in10 or wfile='A'
118690930507     C* INVIO PER SEDE
118700941014     C                   MOVEL     'FNARBG0T'    SFILE
118710930507     C* ALLOCO
118720930507     C                   EXSR      ALLOC
118730930507     C   91              GOTO      ENDRG7
118740070320     c
118750070320     c* Se impostata la "S" o la "A" devo impostare relativo blocco
118760070320     c*  telefonate in DSBL4L
118770070320     c                   clear                   wesar4L           1
118780070320     c                   clear                   BAS               1
118790070320     c                   clear                   TFD               1
118800070320     c                   if        ((vidtc1='S' or vidtc2='S') and
118810070320     c                              arbtc1<>'S' and arbtc2<>'S')  or
118820070320     c                             ((vidtc1='A' or vidtc2='A') and
118830070320     c                              arbtc1<>'A' and arbtc2<>'A')
118840070320     c                   eval      BAS='S'
118850070320     c                   endif
118860070320     c
118870070320     c* se tolto "A" o "S" devo eliminare il blocco
118880070320     c                   if        vidtc1<>'A' and vidtc1<>'S' and
118890070320     c                              vidtc2<>'A' and vidtc2<>'S' and
118900070320     c                             (arbtc1='S' or arbtc1='A' or
118910070320     c                              arbtc2='S' or arbtc2='A')
118920070320     c                   eval      BAS='N'
118930070320     c                   endif
118940070320     c* Tolgo blocco telefonata FFD se messa consegna richiesta e
118950070320     c*  presente FFD
118960070329     c****               if        arbffd='S' and viddcr>0
118970070329     c****               eval      TFD='N'
118980070329     c****               endif
118990070320     c                   if        bas<>' ' or TFD<>' '
119000070320     c                   eval      TipoEla='C'
119010070320     c                   exsr      Gestar4L
119020070320    1c                   if        *in91
119030070515     c                   exsr      DLCSED
119040070525     C                   SETON                                        2891
119050070320     C                   GOTO      ENDRG7
119060070320    1c                   endif
119070070320     c                   endif
119080041022     c
119090041022     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
119100041022     c* iornare direttamente il file
119110041022     c                   clear                   waltrabolla       1
119120041022     C   10KARB          CHAIN     FNBLP01L                           3091
119130041022     C  n10KARB          CHAIN     FNARB01L                           3091
119140041022     c*
119150070308    1c                   if        *in91
119160070515     c                   exsr      DLCSED
119170070515     c                   unlock    fiar401l
119180070525     C                   SETON                                        2891
119190041022     C                   MOVEL     MSG(4)        VIDMSG
119200041022     C                   GOTO      ENDRG7
119210070308    1c                   endif
119220070308     c
119230070308     c* Se lna attivata con GEO
119240070308     c* Se ho aggiornato FIARP, e devo aggiornare altri dati,
119250070308     c* nei campi della consegna richeista reimposto quelli della bolla
119260070319    1c                   if        FGSgeot='S'and AggioARP='S'
119270070308     c                   eval      comdcr=Bolladcr
119280070308     c                   eval      vidhcr=bollahcr
119290070308     c                   eval      vidtcr=Bollatcr
119300070308     c                   clear                   ModConsR
119310070308     c                   endif
119320070308     c
119330041022     c                   if        *in30
119340041022     c                   eval      waltrabolla='N'
119350041022     c                   else
119360041022     c* memorizzo campi prima della variazione
119370041022     c                   eval      waltrotc1=arbtc1
119380041022     c                   eval      waltrotc2=arbtc2
119390080422     c   10              eval      waltroft3=arbft3
119400130411     c  n10              eval      wpda_ndc=arbndc
119410130411     c  n10              eval      wpda_ddc=arbddc
119420130411     c  n10              eval      wpda_dcm=arbdcm
119430141223     c  n10              eval      bollaFBC=arbfbc
119440041022     C   10KARB          CHAIN     FNARB01L                           30
119450041022     C  N10KARB          CHAIN     FNBLP01L                           30
119460041022     c* Alla fine per non sporcarmi i campi del record, richaino il
119470041022     c*  record originale che sto variando
119480130411     c                   eval      wpda_ndc=arbndc
119490130411     c                   eval      wpda_ddc=arbddc
119500130411     c                   eval      wpda_dcm=arbdcm
119510041022     c                   endif
119520151230     c                   endif
119530930507     C*
119540930507     C                   Z-ADD     DATEU         ARBDTV
119550930507     C                   TIME                    ARBORV
119560930507     C                   MOVEL     VIDCVR        ARBCVB
119570001204     C*
119580001204     C                   MOVEL     ARBTC1        SAVTC1
119590001204     C                   MOVEL     ARBTC2        SAVTC2
119600080415     c*
119610930507     C*
119620060130     C* SE DA STORICIZZARE
119630060130     C**   *IN10         IFEQ      *ON
119640060130    1C**   D66FSA        ANDEQ     'S'
119650060130     C**   *IN10         OREQ      *OFF
119660060130    1C**   D66FSP        ANDEQ     'S'
119670070308     c
119680060130    1C     D66FSA        ifeq      'S'
119690011108      *
119700011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
119710011108      * è in partenza 'P' o in arrivo 'A'
119720011108     C     *IN10         IFEQ      *ON
119730160119     c                   if        wfile='P'
119740160119     C                   MOVE      'a'           ARBPRU
119750160119     c                   else
119760011108     C                   MOVE      'A'           ARBPRU
119770160119     c                   endif
119780011108     C                   ENDIF
119790011108     C     *IN10         IFEQ      *OFF
119800011108     C                   MOVE      'P'           ARBPRU
119810011108     C                   ENDIF
119820011108      *
119830941014     C                   WRITE     FNARBG00
119840930507     C                   END
119850060301     c* storicizzazione motivazione
119860060301     c     d66mtv        ifeq      'S'
119870060301     c                   exsr      sto_mtv
119880060301     c                   endif
119890040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
119900040224      * - Tipo/Data/Ora consegna richiesta
119910040224      * - Giorni di chiusura
119920040224      * - Consegna particolare
119930040224      * devo memorizzare i dati della bolla prima della modifica
119940150129     c******             If        *In10 and
119950150206     c* dal 06/02 momorizziamo anche se varia la partenza, come se lo facesse l'arrivo
119960150129     c                   if         (VidTcr <> ArbTcr or ComDcr <> ArbDcr or
119970040224     c                               VidHcr <> ArbHcr or VidGc1 <> ArbGc1 or
119980040224     c                               VidGc2 <> ArbGc1 or VidTc1 <> ArbTc1 or
119990040224     c                               VidTc2 <> ArbTc2)
120000041022     c                   ExSr      RegAr5AR
120010040223     c                   EndIf
120020080415     c*
120030080415     c* Se modifico data consegna richiesta verifico se devo sflaggare per
120040080415     c* clienti
120050080415     c                   eval      wsflagga_C=' '
120060080422     c                   if        comdcr<>arbdcr or
120070080415     c                             vidtcr<>arbtcr
120080080415     c                   clear                   wcmitt
120090080415     c                   if        flgtb1='A'
120100080415     c                   move      arbccm        wcmitt
120110080415     c                   else
120120080415     c                   move      arbksc        wcmitt
120130080415     c                   endif
120140080416     c                   if        wcmitt>'0000000'
120150080416     c                             and %subst(wcmitt:4:4)<>'8888'
120160080415     C                   MOVEL     '3K'          COD
120170080415     C                   MOVEL(P)  wcmitt        KEY
120180080415     C                   EXSR      CTABEL
120190080415     C  N30              MOVEL     TBLUNI        DS3K
120200080415     C   30              CLEAR                   DS3K
120210080415     c                   if        §3kcq8='S'
120220080415     c                   eval      wsflagga_C='S'
120230080415     c                   endif
120240080415     c                   endif
120250080415     c                   endif
120260041022     c
120270041022     c* Se variazione in partenza e modifico  le cons.partic
120280041022     c*  sflaggo per clienti
120290041025     c  n10              if        vidtc1<>arbtc1 or vidtc2<>arbtc2
120300080415     c                             or wsflagga_c='S'
120310041022     c                   clear                   arbft3
120320041022     c                   endif
120330070319     c
120340070319     c* Se impostata la "S" o la "A" devo impostare relativo blocco
120350070319     c*  telefonate in DSBL4L
120360151230     c                   if        not *in10 or wfile='A'
120370070320     c                   if        wesar4L<>' '
120380070320     c                   if        BAS='S'
120390070320     c                   eval      §b4lbas='S'
120400070320     c                   endif
120410070320     c                   if        BAS='N'
120420070320     c                   eval      §b4lbas=' '
120430070320     c                   endif
120440070320     c
120450070320     c                   if        TFD='N'
120460070320     c                   eval      §b4lTFD=' '
120470070320     c                   endif
120480070320     c
120490070320     c                   eval      TipoEla='A'
120500070320     c                   exsr      GestAR4L
120510070320     c                   endif
120520151230     c                   endif
120530070320     c
120540920320     C*
120550920320     C                   MOVEL     COMDCR        ARBDCR
120560920320     C                   MOVEL     VIDHCR        ARBHCR
120570920320     C                   MOVEL     VIDTCR        ARBTCR
120580930507     C                   MOVEL     VIDGC1        ARBGC1
120590930507     C                   MOVEL     VIDGC2        ARBGC2
120600941102     C                   MOVEL     VIDTC1        ARBTC1
120610941102     C                   MOVEL     VIDTC2        ARBTC2
120620070308     c
120630070308     c* Se modificata consegna richiesta in arrivo
120640070308     c*  devo anche pulire il flag blocco consegna se = 'A'
120650141223     c                   if        BollaFBC='A' and ModconsR='S'
120660070308     c                             and comdcr>0
120670070308     c                   clear                   arbfbc
120680070419     c                   clear                   arbcmc
120690070308     c                   endif
120700070920     c
120710070920     c* Se modificata la consegna richiesta e bolla già consegnata
120720070920     c* effettuo il ricalcolo della affidabilità
120730070920     c                   if        ModconsR='S' and arbdcr=0 and arbdcm>0
120740070920     c                   clear                   arbdti
120750070920     c                   clear                   arbhti
120760070920     c                   movel     'S'           arbfbs
120770070920     c                   endif
120780041025     c
120790070319     c* Verifico se devo aggiornare record già esistente in ar5
120800150206     c*  ma solo per le consegne particolari e fermo deposito
120810150206     c  N10              exsr      RegAr5PA
120820941212     C*
120830930507     C* PER SEDE
120840151230     c                   if        not *in10 or wfile='A'
120850070503     C     D66FFI        IFEQ      'S'
120860950227     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
120870950413    2C     §3ABSD        IFEQ      ' '
120880930507     C*
120890941014     C                   OPEN      FNARBG0T
120900941014     C                   WRITE     FNARBGTT
120910941014     C                   CLOSE     FNARBG0T
120920950227     C                   END
120930930507     C* DISALLOCO OGGETTO
120940941212     C                   EXSR      DLCSED                                       FNARBG0T
120950950227     C                   END
120960151230     c                   endif
120970041022     c
120980930507     C*
120990151230     c                   if        wfile='A'
121000151230     C                   UPDATE    FNARB000
121010151230     c                   else
121020151230     C                   UPDATE    FNBLP000
121030151230     c                   endif
121040941220     C*
121050941220     C* DISALLOCO I RECORD CHE NON USO
121060160119     C   10              UNLOCK    FIAR601L
121070041022     c* Aggiorno anche l'altra bolla
121080151230     c                   if        not *in10 or wfile='A'
121090041022     c                   if        waltrabolla=' '
121100080422     c   10              eval      arbft3=waltroft3
121110041025     c* SE aggiorno bopartenza sflaggo per clienti se variate le cons.partic
121120041025     c                   if        *in10 and (waltrotc1<>arbtc1 or
121130080415     c                             waltrotc2<>arbtc2 or
121140080415     c                             wsflagga_c='S')
121150041022     c                   clear                   arbft3
121160041022     c                   endif
121170041022     C   10              except    arbGblp
121180041022     C  N10              except    arbGarb
121190041022     c                   endif
121200151230     c                   endif
121210950602     C*
121220070308     c* Se non è previsto l'aggiornamento di FIARP, vedo se lo devo
121230070308     c*  cancellare (se variata la data consegna richiesta)
121240070319     c                   if        FGSgeot='S'  and  ModconsR='S'
121250070308     c     karb          delete    fiarp01l
121260070308     c                   endif
121270070308     c
121280040224      * Se c'è la data consegna richiesta controllo se è ancora valida
121290040224      * solo se ho fatto una modifica in arrivo
121300150129      * da adesso anche se modifica in partenza
121310150206     c                   If        ArbDcr > *Zeros
121320040324     c                             and wCa = *Blanks and wgiac = *Blanks
121330040224     c                   ExSr      ContrDcr
121340040224     c                   EndIf
121350070308     c
121360130411     C*
121370130411     c* Richiamo routine per invio variazione a pda
121380130411     c                   if        D48Cvb<>'CS'
121390151230     c                             and (not *in10 or wfile='A')
121400130411     c                   exsr      sr_pda
121410130411     c                   endif
121420950104     C*
121430930507     C     ENDRG7        ENDSR
121440021129      **
121450021129      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
121460021129     c     Regis71       BegSr
121470021129
121480021129      * Se è stata indicata la consegna particolare 'S' devo scrivere
121490021129      * il file FIAR5
121500021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
121510021129     c                   Clear                   Fiar5000
121520021129     c                   Eval      Ar5Aas = ArbAas
121530021129     c                   Eval      Ar5Lnp = ArbLnp
121540021129     c                   Eval      Ar5Nrs = ArbNrs
121550021129     c                   Eval      Ar5Nsp = ArbNsp
121560021129     c                   Eval      Ar5Trd = 'SCR'
121570021129     c                   Move      Dateu         Ar5Dac
121580021129     c                   Movel     W0140         Ar5Orc
121590021129     c                   Clear                   Dar5Scr
121600021129     c                   Eval      §ar5Cvb = D48Cvb
121610021129     c                   Eval      §ar5Pru = Knmus
121620060131     c                   Eval      §ar5Pou = dutPou
121630021129     c                   Eval      §ar5Tcr = VidTcr
121640021129     c                   Eval      §ar5Dcr = ComDcr
121650021129     c                   Eval      §ar5Hcr = VidHcr
121660021129     c                   Eval      §ar5Gc1 = VidGc1
121670021129     c                   Eval      §ar5Gc2 = VidGc2
121680021129     c                   Eval      §ar5Tc1 = VidTc1
121690021129     c                   Eval      §ar5Tc2 = VidTc2
121700021129     c                   Eval      §ar5Nom = VidNom
121710021129     c                   Eval      §ar5Tel = VidTel
121720021129     c                   Eval      §ar5Mot = VidMot
121730021129     c                   Movel     Dar5Scr       Ar5Uni
121740021129     c                   Write     Fiar5000
121750021129     c                   EndIf
121760040705
121770040705      * Aggiorno anche il record 'GEN' di Fiar5 con il nuovo Telefono e Referente
121780091217     c                   eval      Wtipoagg='REF'
121790091217     c                   exsr      RegisAR5GEN
121800091217     c***
121810091217     c***                Eval      kAr5Trd = 'GEN'
121820091217     c***                Clear                   dAr5Gen
121830091217     c***  kAr5          Chain     Fiar501l
121840040705      * Aggiorno Fiar5
121850091217if  1c***                If        %Found(Fiar501l)
121860091217     c***                Eval      dAr5Gen = Ar5Uni
121870091217     c***                Eval      §Ar5Ref = VidNom
121880091217     c***                Eval      §Ar5Teld = VidTel
121890091217     c***                Movel(p)  dAr5Gen       Ar5Uni
121900040705      * se la modifica è fatta in partenza e devo trasmettere all'arrivo
121910040705      * sfleggo per la trasmissione
121920070306     c***                If        Not *In10 and §Ar5Trar = 'S'
121930070306     c***                Clear                   Ar5Ft1
121940070306     c***                EndIf
121950040705      * se la modifica è fatta in arrivo e devo trasmettere alla partenza
121960040705      * sfleggo per la trasmissione
121970070306     c***                If        *In10 and §Ar5Trpa = 'S'
121980070306     c***                Clear                   Ar5Ft1
121990070306     c***                EndIf
122000040705      * se devo trasmettere alla sede sfleggo per la trasmissione
122010091217     c***                If        §Ar5Trse = 'S'
122020091217     c***                Clear                   Ar5Ft2
122030091217     c***                EndIf
122040040705      * sfleggo sempre per la trasmissione al cliente
122050091217     c***                Clear                   Ar5Ft3
122060091217     c***                Update    Fiar5000
122070040705      * Scrivo Fiar5
122080091217   x1c***                Else
122090040705      * se immessi i dati
122100091217     c***                Clear                   Fiar5000
122110091217     c***                Eval      Ar5Aas = ArbAas
122120091217     c***                Eval      Ar5Lnp = ArbLnp
122130091217     c***                Eval      Ar5Nrs = ArbNrs
122140091217     c***                Eval      Ar5Nsp = ArbNsp
122150091217     c***                Eval      Ar5Trd = 'GEN'
122160091217     c***                Move      Dateu         Ar5Dac
122170091217     c***                Movel     W0140         Ar5Orc
122180091217     c***                Clear                   Dar5Gen
122190091217     c***                Eval      §Ar5Ref = VidNom
122200091217     c***                Eval      §Ar5Teld = VidTel
122210091217     c***                Movel(p)  DAr5Gen       Ar5Uni
122220091217     c***                Write     Fiar5000
122230091217    1c***                EndIf
122240130411     c* Richiamo routine per invio variazione a pda
122250130411     c                   exsr      sr_pda
122260021129
122270021129     c                   EndSr
122280040223
122290041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in arrivo--*
122300041022     c     RegAr5AR      BegSr
122310070309     c* Se Attiva GEO e bolla non in distnta e non giacente con
122320070309     c*  un evento che prevede la modifica della cons.richista
122330070309     c*  memorizzo la cons richiesta nei campi di FIAR5
122340070309     c                   clear                   AggioAR5M         1
122350070309     c
122360070319     c                   if        arbndc=0 and FGSgeot='S' and
122370070309     c                             arbfbc<>'G' and eveADR='S'
122380070309     c                   eval      AggioAR5M='S'
122390070309     c                   endif
122400160114     c* Se manutenzione in Arrivo su bolla ancora da partire memorizzo
122410160114     c* di aggiornare i dati del ripristino con quelli del video
122420160114     c                   if        *in10 and wfile = 'P'
122430160114     c                   eval      AggioAR5M='S'
122440160114     c                   endif
122450040223
122460040224     c                   Clear                   waggar5
122470040223     c                   Clear                   dAr5Gen
122480040223     c                   Eval      kAr5Trd = 'GEN'
122490040223     c     kAr5          Chain     Fiar501l
122500040223     c                   If        %Found(Fiar501l)
122510040223     c                   Movel     Ar5Uni        dAr5Gen
122520040223     c                   EndIf
122530040223
122540040223      * Dati non memorizzati
122550040223if  1c                   If        §Ar5Mdp <> 'S'
122560040224     c                   Eval      §Ar5Mdp = 'S'
122570040223     c                   Eval      §Ar5gTcr = ArbTcr
122580040223     c                   Move      ArbDcr        §Ar5gDcr
122590040223     c                   Move      ArbHcr        §Ar5gHcr
122600040223     c                   Eval      §Ar5gGc1 = ArbGc1
122610040223     c                   Eval      §Ar5gGc2 = ArbGc2
122620040223     c                   Eval      §Ar5gTc1 = ArbTc1
122630040223     c                   Eval      §Ar5gTc2 = ArbTc2
122640040223     c                   Eval      §Ar5Ffd  = ArbFfd
122650040223      * Causale 'CD' da chiusura distinta
122660140806      *   oppure
122670140806      * Causale 'CI' da indicazioni da internet
122680040223      * imposto i campi a video
122690140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
122700040223     c                   Eval      §Ar5Tcrm = VidTcr
122710040223     c                   Move      ComDcr        §Ar5Dcrm
122720040223     c                   Move      VidHcr        §Ar5Hcrm
122730040223      * Altra causale
122740040223      * imposto i campi di arb prima della variazione
122750040223   x2c                   Else
122760040223     c                   Eval      §Ar5Tcrm = ArbTcr
122770040223     c                   Move      ArbDcr        §Ar5Dcrm
122780040223     c                   Move      ArbHcr        §Ar5Hcrm
122790040223    2c                   EndIf
122800040224     c                   Eval      waggar5 = 'S'
122810040223      * Dati memorizzati
122820040223   x1c                   Else
122830140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
122840040223     c                   Eval      §Ar5Tcrm = VidTcr
122850040223     c                   Move      ComDcr        §Ar5Dcrm
122860040223     c                   Move      VidHcr        §Ar5Hcrm
122870040224     c                   Eval      waggar5 = 'S'
122880040223    2c                   EndIf
122890040223    1c                   EndIf
122900150206     c* se si tratta di una modifica di cons richiesta in partenza lo memorizzo se
122910150206     c*  non l'ho già fatto
122920150206     c                   if        not *in10 and comdcr<>arbdcr and
122930150206     c                             §ar5dcrp<>'P'  and comdcr>0
122940150206     c                   eval      §ar5dcrp= 'P'
122950150206     c                   Eval      waggar5 = 'S'
122960150206    2c                   EndIf
122970150206     c                   if        not *in10 and comdcr<>arbdcr and
122980150206     c                             §ar5dcrp= 'P'  and comdcr=0
122990150206     c                   eval      §ar5dcrp= ' '
123000150206     c                   Eval      waggar5 = 'S'
123010150206    2c                   EndIf
123020150206     c
123030040223
123040040224if  1c                   If        waggar5 = 'S'
123050040224      * aggiorno Fiar5
123060040224if  2c                   If        %Found(Fiar501l)
123070040224     c                   Movel(p)  dAr5Gen       Ar5Uni
123080040224      * se devo trasmettere alla sede sfleggo per la trasmissione
123090040224     c                   If        §Ar5Trse = 'S'
123100040224     c                   Clear                   Ar5Ft2
123110040224     c                   EndIf
123120040224      * sfleggo sempre per la trasmissione al cliente
123130040224     c                   Clear                   Ar5Ft3
123140040223     c                   Update    Fiar5000
123150040224      * scrivo Fiar5
123160040224   x2c                   Else
123170040224     c                   Clear                   Fiar5000
123180040223     c                   Eval      Ar5Aas = ArbAas
123190040223     c                   Eval      Ar5Lnp = ArbLnp
123200040223     c                   Eval      Ar5Nrs = ArbNrs
123210040223     c                   Eval      Ar5Nsp = ArbNsp
123220040224     c                   Eval      Ar5Trd = 'GEN'
123230040223     c                   Move      Dateu         Ar5Dac
123240040223     c                   Movel     W0140         Ar5Orc
123250040224     c                   Movel(p)  dAr5Gen       Ar5Uni
123260041007     c* flaggo sempre il flag di filiale tanto non trasmettiamo più
123270040223     c                   Write     Fiar5000
123280040224    2c                   EndIf
123290040224   x1c                   Else
123300040224     c                   Unlock    Fiar501l
123310040224    1c                   EndIf
123320040223
123330040223     c                   EndSr
123340041022
123350041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in partenz-*
123360041022     c     RegAr5PA      BegSr
123370041022     c                   Clear                   dAr5Gen
123380041022     c                   Eval      kAr5Trd = 'GEN'
123390041022     c     kAr5          Chain     Fiar501l
123400041022if  2c                   If        %Found(Fiar501l)
123410041022     c                   Movel     Ar5Uni        dAr5Gen
123420041022      * Flag dati memorizzati
123430041022if  3c                   If        §Ar5Mdp = 'S'
123440041022      * Aggiorno i dati di ripristino solo se uguali a quelli salvati
123450150206if  4c***                If        §Ar5gTcr= §Ar5Tcrm and §Ar5gDcr = §Ar5Dcrm
123460150206     c***                          and §Ar5gHcr = §Ar5Hcrm
123470150206     c***                Eval      §Ar5Tcrm = ArbTcr
123480150206     c***                Move      ArbDcr        §Ar5Dcrm
123490150206     c***                Move      ArbHcr        §Ar5Hcrm
123500150206    4c***                EndIf
123510041022      * Memorizzo i dati della bolla
123520150206     c***                Eval      §Ar5gTcr = ArbTcr
123530150206     c***                Move      ArbDcr        §Ar5gdcr
123540150206     c***                Move      ArbHcr        §Ar5gHcr
123550150206     c***                Eval      §Ar5gGc1 = ArbGc1
123560150206     c***                Eval      §Ar5gGc2 = ArbGc2
123570150206     c* Da 02/15
123580150206     c* come partenza aggiorno soltanto i dati del fermo deposito e dei giorni di chiusura
123590041022     c                   Eval      §Ar5gTc1 = ArbTc1
123600041022     c                   Eval      §Ar5gTc2 = ArbTc2
123610041022     c                   Eval      §Ar5Ffd = arbFfd
123620041022    3c                   EndIf
123630041022     c                   Movel     Dar5Gen       Ar5Uni
123640041022     c                   Update    Fiar5000
123650041022    2c                   EndIf
123660041022     c                   EndSr
123670070906
123680070906      *--- CONTROLLO SE ANCORA VALIDA Le consegne particolari --------*
123690070906     c     ContrTCP      BegSr
123700070910      *
123710070906     c                   clear                   eliminatcp        1
123720070911     c                   clear                   wcomb
123730070911     c                   clear                   aggtcp            1
123740070906     c
123750070910    1c                   if        arbtc1=wdisagdest
123760070910if  2c                   If        (par5Mdp = 'S' and
123770070910     c                             par5gtc1<>arbtc1 and par5gtc2<>arbtc1)
123780070911     c*******                      or   par5Mdp=' '
123790070906     c                   eval      eliminatcp='1'
123800070911     c                   eval      wtc1=arbtc1
123810070907    2c                   endif
123820070910    1c                   endif
123830070910    1c                   if        arbtc2=wdisagdest
123840070910if  2c                   If        (par5Mdp = 'S' and
123850070910     c                             par5gtc1<>arbtc2 and par5gtc2<>arbtc2)
123860070911     c********                     or   par5Mdp=' '
123870070906     c                   eval      eliminatcp='2'
123880070911     c                   eval      wtc1=arbtc2
123890070907    2c                   endif
123900070910    1c                   endif
123910070906     c* Se da eliminare
123920070910    1c                   if        eliminatcp<>' '
123930070911     c* Verifico se per le combinazioni devo ripristinare la cons partic
123940070911     c*  della partenza
123950070911     c                   if        par5mdp='S' and par5gtc1<>' '
123960070911     c                   eval      wtc2=par5gtc1
123970070911     c                   exsr      sr_chkcomb
123980070911     c
123990070911     c                   if        wcomb<>' '
124000070911     c                   eval      aggtcp=par5gtc1
124010070911     c                   endif
124020070912     c                   endif
124030070911     c
124040070911     c                   if        wcomb=' ' and par5gtc2<>' '
124050070911     c                   eval      wtc2=par5gtc1
124060070911     c                   exsr      sr_chkcomb
124070070911     c                   if        wcomb<>' '
124080070911     c                   eval      aggtcp=par5gtc2
124090070911     c                   endif
124100070911     c                   endif
124110070911     c
124120070906     c                   Clear                   dsarbg
124130070906     c                   Eval      d48cvb = 'CA'
124140070906     c                   Eval      weseg = 'E'
124150070911     c*****              Clear                   d48ffr
124160070906     c                   Eval      d48Trc = 'G'
124170070906     c                   Eval      §bgTcr = arbtcr
124180070906     c                   Move      arbdcr        §bgDcr
124190070906     c                   Move      arbhcr        §bgHcr
124200070906     c                   Eval      §bgGc1 = Arbgc1
124210070906     c                   Eval      §bgGc2 = ArbGc2
124220070910    2c                   if        eliminatcp='1'
124230070906     c                   clear                   §bgtc1
124240070906     c                   clear                   arbtc1
124250070906     c                   Eval      §bgtc2 = Arbtc2
124260070911     c* ripristino la vecchia consegna partic.
124270070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
124280070911     c                   eval      §bgtc1=aggtcp
124290070911     c                   endif
124300070910   x2c                   else
124310070906     c                   clear                   §bgtc2
124320070906     c                   clear                   arbtc2
124330070906     c                   Eval      §bgtc1 = Arbtc1
124340070911     c* ripristino la vecchia consegna partic.
124350070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
124360070911     c                   if        §bgtc1=' '
124370070911     c                   eval      §bgtc1=aggtcp
124380070911     c                   else
124390070911     c                   eval      §bgtc1=aggtcp
124400070911     c                   endif
124410070911     c                   endif
124420070910    2c                   endif
124430070907     c
124440070907     c* Memorizzo in skiera rielaborazioni
124450070910    2c                   if        kk<=10
124460070907     c                   add       1             kk
124470070907     c                   movel     dsarbg        kdati(KK)
124480070907     c                   movel     d48cvb        kcvb(KK)
124490070907     c                   movel     d48trc        ktrc(KK)
124500070911     c                   movel     ' '           kffr(KK)
124510070911     c                   movel     ' '           kf12(KK)
124520070910    2c                   endif
124530070910    1c                   endif
124540070906     c
124550070906     c                   EndSr
124560040224
124570040224      *--- CONTROLLO SE ANCORA VALIDA LA DATA CONSEGNA RICHIESTA -----*
124580160114     c     ContrDcr      BegSr
124590040225
124600040225     c                   Move      ArbDcr        warbdcr
124610050907      * richiamo il controllo della "7Q"
124620050907     c                   ExSr      Contr7q
124630040225
124640040225      * Controllo se c'è il fiar5 rcd GEN
124650070906     c                   clear                   dar5gen
124660040225     c                   Eval      kAr5Trd = 'GEN'
124670040225     c     kAr5          Chain(n)  Fiar501l
124680040225     c                   If        %Found(Fiar501l)
124690040225     c                   Movel     Ar5Uni        dAr5Gen
124700040225     c                   EndIf
124710040225      * se ho memorizzato i dati
124720040225if  1c                   If        §Ar5Mdp = 'S' and
124730040225      * e la data è diversa
124740040225     c                             (wArbDcr <> §Ar5Dcrm or
124750040225     c                               ArbTcr <> §Ar5Tcrm)
124760040225      * controllo se la data è valida quando:
124770040224      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
124780040225if  2c                   If        ArbPkf >= §VpoPkg or
124790040224     c                             ArbVlf >= §VpoVlm or
124800150506      *                     Destinatario supermercato o Appuntamento
124810040224     c                             ArbTc1 = 'S' or ArbTc1 = 'A' or
124820040224     c                             ArbTc2 = 'S' or ArbTc2 = 'A' or
124830150506     c                             ArbTc1 = 'Z' or ArbTc2 = 'Z' or
124840040312      *                     Lasciato avviso
124850070308     c                             BollaFbc = 'A' or
124860040224      *                     Fermo deposito
124870040224     c                             ArbFfd = 'S' or
124880040224      *                     Tipo bolla "FW" Recapito c/assegni
124890040811     c                             ArbCbo = 'FW'or
124900040811      *                     Periodo trazione ridotte e bolla con particolarità "G8"
124910040811     c                             wDCRG8 = 'S'
124920040225     c                   Goto      EndContr
124930040225    2c                   EndIf
124940040224      *                     C/assegno non abilitato
124950040225if  2c                   If        Ar9Cas > 0
124960040224     c                   Clear                   Trul21ds
124970040224     c                   Eval      I21Tla = 'L'
124980040224     c                   Eval      I21Cbo = ArbCbo
124990040224     c                   Eval      I21Tsp = ArbTsp
125000040224     c                   Eval      I21Lnp = ArbLnp
125010040224     c                   Eval      I21Nzm = ArbNzm
125020040224     c                   Eval      I21Lna = ArbLna
125030040224     c                   Eval      I21Nzd = ArbNzd
125040130827     c*****              Eval      I21Ksc = ArbKsc
125050040224     c                   Movel     ArbCtr        I21Ctr
125060040224     c                   Eval      I21Tic = Ar9Tic
125070040224     c                   Eval      I21Imp = Ar9Cas
125080040224     c                   Eval      I21Div = Ar9Vca
125090040224     c                   Eval      I21Ute = Knmus
125100040224     c                   Eval      I21Pgm = nompgm
125110130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
125120130827     c                   if        flgtb1='F'
125130130827     c                   z-add     arbksc        i21ksc
125140130827     c                   else
125150130827     c                   z-add     arbccm        i21ksc
125160130827     c                   if        i21ksc=0
125170130827     c                   z-add     8888          i21ksc
125180130827     c                   movel     arblnp        i21ksc
125190130827     c                   endif
125200130827     c                   endif
125210040224     c                   Call      'TRUL21R'
125220040224     c                   Parm                    Trul21ds
125230040225if  3c                   If        O21Fca <> *Blanks
125240040225     c                   Goto      EndContr
125250040225    3c                   EndIf
125260040225    2c                   EndIf
125270070320     c
125280070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
125290070320     c*  manutenzionare la spedizione
125300160114    1c                   if        §ar5MDP='S'  and §ar5FFD='S'
125310070320     c                   GoTo      EndContr
125320070911    1c                   EndIf
125330150206     c* se la cons richiesta non è stata immessa dalla partenza e sono in partenza
125340150206     c*  ripristino
125350160114     c                   if        (not *in10 and  §ar5dcrp= 'P')
125360160114     c                             or (*in10 and wfile='P')
125370150206     c                   GoTo      EndContr
125380150206    1c                   EndIf
125390150206     c
125400040225      * in questo caso devo reimpostare la data consegna richiesta come indicato in Fiar5
125410040225      * facendo una variazione di tipo 'CA'
125420040225      * senza farla vedere a video
125430040225     c                   Clear                   dsarbg
125440040225     c                   Eval      d48cvb = 'CA'
125450040225     c                   Eval      weseg = 'E'
125460070911     c*****              Clear                   d48ffr
125470040225     c                   Eval      d48Trc = 'G'
125480040324     c                   Eval      §bgTcr = §Ar5Tcrm
125490040324     c                   Move      §Ar5Dcrm      §bgDcr
125500040324     c                   Move      §Ar5Hcrm      §bgHcr
125510040315     c                   Eval      §bgGc1 = Arbgc1
125520040315     c                   Eval      §bgGc2 = ArbGc2
125530040315     c                   Eval      §bgTc1 = ArbTc1
125540040315     c                   Eval      §bgTc2 = ArbTc2
125550040225     c                   Eval      wCA = 'S'
125560070906     c* Se da eliminare anche la consegna particolare imposto
125570070906     c*  insieme la variazione
125580070911    1c                   if        eliminatcp='1'
125590070906     c                   clear                   §bgtc1
125600070911     c* ripristino la vecchia consegna partic.
125610070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
125620070911     c                   eval      §bgtc1=aggtcp
125630070911    2c                   endif
125640070911    1c                   endif
125650070911    1c                   if        eliminatcp='2'
125660070906     c                   clear                   §bgtc2
125670070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
125680070911    3c                   if        §bgtc1=' '
125690070911     c                   eval      §bgtc1=aggtcp
125700070911     c                   else
125710070911     c                   eval      §bgtc2=aggtcp
125720070911    3c                   endif
125730070911    2c                   endif
125740070911    1c                   endif
125750070906     c
125760070906     c* Memorizzo in skiera rielaborazioni
125770070906     c                   if        kk<=10
125780070907     c* se devo eliminare una consegna particolare, memorizzo nella
125790070907     c* stessa variazione
125800070907     c                   if        eliminatcp=' '
125810070907     c                   add       1             kk
125820070907     c                   endif
125830070906     c                   movel     dsarbg        kdati(KK)
125840070906     c                   movel     d48cvb        kcvb(KK)
125850070907     c                   movel     d48trc        ktrc(KK)
125860070911     c                   movel     ' '           kffr(KK)
125870070911     c                   movel     ' '           kf12(KK)
125880070906     c                   endif
125890070906     c
125900040225    1c                   EndIf
125910040224
125920040225     c     EndContr      EndSr
125930040224
125940070307     C*--- AGGIORNAMENTO FILE proposte FIARP00F ----------------------*
125950070307     C     REGISARP      BEGSR
125960070307     C                   MLLZO     1             VIDHCR
125970070307     c     karb          chain     fiarp01l
125980070307     c                   clear                   fiarp000
125990070307     c                   eval      arpaas=arbaas
126000070307     c                   eval      arplnp=arblnp
126010070307     c                   eval      arpnrs=arbnrs
126020070307     c                   eval      arpnsp=arbnsp
126030070307     c                   eval      arptcr=vidtcr
126040070307     c                   eval      arpdcr=comdcr
126050070307     c                   eval      arphcr=vidhcr
126060070308     C*
126070070308     C                   TIME                    W0140            14 0
126080070308     C* UDATE IN AAAAMMGG
126090070308     C                   MOVE      W0140         WDTGIO            8 0
126100070308     C                   Z-ADD     WDTGIO        G02DAT
126110070307     C                   MOVEL     *BLANK        G02ERR
126120070307     C                   CALL      'XSRDA8'
126130070307     C                   PARM                    WLBDAT
126140070307     C                   MOVEL     G02INV        arpdim            8 0
126150070307     c                   time                    arpoim
126160070308     C                   MOVEL     KNMUS         ARPPRU
126170070307     c                   if        %found(fiarp01l)
126180070307     c                   update    fiarp000
126190070307     c                   else
126200070307     c                   write     fiarp000
126210070307     c                   endif
126220070307     c
126230070307     c                   ENDSR
126240941223     C*
126250941223     C*--- CONTROLLI CAMPI MITTENTE ----------------------------------*
126260941223     C     CONTR8        BEGSR
126270941223     C                   SETOFF                                       9028
126280950104     C                   CLEAR                   VIDFAP
126290941223     C*
126300941223     C* CONTROLLO IL CODICE DEL MITTENTE
126310970813     C                   EXSR      CTRCOM
126320970822     C*
126330970822     C* SE PASSO DA CODIFICATO AD 8888 O VICEVERSA SPROTEGGO O PROTEGGO
126340970822     C* CAMPI DELL'INDIRIZZO
126350970822     C     VIDKSC        IFEQ      8888
126360970822     C     MEMKSC        ANDNE     8888
126370970822     C                   MOVE      VIDKSC        MEMKSC
126380970822     C                   SETOFF                                       22
126390970822     C                   SETON                                        90
126400970822     C                   END
126410970822     C*
126420970822     C     VIDKSC        IFNE      8888
126430970822     C     MEMKSC        ANDEQ     8888
126440970822     C                   MOVE      VIDKSC        MEMKSC
126450970822     C                   SETON                                        2290
126460970822     C                   CLEAR                   VIDRSM
126470970822     C                   CLEAR                   VIDINM
126480970822     C                   CLEAR                   VIDCAM
126490970822     C                   CLEAR                   VIDLOM
126500970822     C                   CLEAR                   VIDPRM
126510970822     C                   CLEAR                   VIDNZM
126520970822     C                   CLEAR                   VIDPIM
126530970822     C                   END
126540950113     C**
126550950113     C     *IN90         IFEQ      *ON
126560950113     C  N48              GOTO      ENDCT8
126570950113     C*
126580950113     C* X IL CODICE TARIFFA DO ERRORE SOLO SE NON E' STATO PREMUTO F10
126590950114     C  NKJ
126600950114     CAN 28              GOTO      ENDCT8
126610950113     C                   ENDIF
126620941223     C*
126630020508     C* SE IMMESSO UN CODICE 8888 OBBLIGATORIO l'indirizzo completo
126640020508    1C     VIDKSC        IFEQ      8888
126650970808     C                   MOVEL     VIDRSM        I14RSC
126660970808     C                   CLEAR                   I14RS2
126670970808     C                   MOVEL     VIDINM        I14IND
126680970808     C                   MOVEL     VIDLOM        E14LOC
126690970808     C                   MOVEL     VIDPIM        E14PIP
126700970808     C                   MOVEL     VIDPRM        E14PRV
126710970808     C                   MOVEL     VIDNZM        E14NAR
126720970808     C                   MOVEL     VIDCAM        E14CAP
126730970808     C                   CLEAR                   I14ISO
126740970808     C                   MOVEL     'M'           WCLI              1
126750970808     C                   EXSR      CTRIND
126760941223     C*
126770941223     C* CAMPI CHE POSSO AVER IMPOSTATO
126780970808     C                   MOVEL     E14LOC        VIDLOM
126790970808     C                   MOVEL     E14PIP        VIDPIM
126800970808     C                   MOVEL     E14PRV        VIDPRM
126810970808     C                   MOVEL     E14NAR        VIDNZM
126820970808     C                   MOVEL     E14CAP        VIDCAM
126830950104     C* SALVO IL FLAG ANTEPORTO
126840970808     C                   MOVEL     O95ISO        COMFAP
126850970808     C                   MOVEL     O95ISO        VIDFAP
126860140403     c* salvo pickup 2
126870140403     C                   MOVEL     O95gf2        comgf2
126880941223     C**
126890970808     C* ERRORE
126900970808     C   90              GOTO      ENDCT8
126910950522     C* P.IVA obbligatoria se nazione estera e c'è flag efta in
126920950522     C* tabella nazione
126930950522     C     WDESIE        IFEQ      'E'
126940950522     C     DESEFT        ANDNE     *BLANKS
126950950412     C     VIDPIM        ANDEQ     *BLANKS
126960950412     C                   MOVEL     MSG(67)       VIDMSG
126970950412     C                   SETON                                        284990
126980950412     C                   GOTO      ENDCT8
126990950412     C                   END
127000941223     c*
127010941223   X1C                   ELSE
127020950104     c*
127030950104     C* VEDO SE ESISTE CNIND
127040950104     C     KACO          CHAIN     CNIND000                           32
127050950104     C*
127060950104     C     *IN32         IFEQ      *ON
127070950104     C                   MOVEL     MSG(63)       VIDMSG
127080950104     C                   SETON                                        479028
127090950104     C                   GOTO      ENDCT8
127100950104     C                   ENDIF
127110950104     C*
127120950104     C* CONTROLLO SUL CAP PROVINCIA NAZIONE
127130970423     C     INDSTA        IFEQ      *BLANKS
127140970423     C     INDPRV        ANDEQ     *BLANKS
127150950104     C                   MOVEL     MSG(64)       VIDMSG
127160950104     C                   SETON                                        479028
127170950104     C                   GOTO      ENDCT8
127180950104     C                   ENDIF
127190950104     C* CAP OBBLIGATORIO : CONTROLLO SE CONGRUENZA CON PROVINCIA
127200030612     C                   CLEAR                   Tisi95DS
127210030612     C                   CLEAR                   Fnlv13DS
127220970811     C                   MOVEL     INDSTA        I95NAR
127230970811     C                   MOVEL     INDCAE        I95CAP
127240970811     C                   MOVEL     INDPRV        I95PRV
127250970811     C                   MOVEL     INDCIT        I95LOC
127260970811     C                   MOVEL     '3'           I95TCN
127270970811     C                   MOVEL     ARBDSP        I95DAT
127280970811     C* ERRORE PER AFFIDABILITA' = 0
127290970811     C                   MOVEL     'S'           I13AF0
127300970811     C* CONTROLLO CONGRUENZA CAP/PROVINCIA
127310970811     C                   MOVEL     'S'           I13CNG
127320970811     C* CONTROLLO NON FORZABILE CITTA' CON VIARIO
127330970811     C                   MOVEL     'S'           I13CNV                         CAP CON VIAR
127340020305     c     lnpntw        ifeq      'FED'
127350020305     C                   movel     'S'           i95fi1
127360020305     c                   endif
127370970811     C                   CALL      'FNLV13R'
127380970811     C                   PARM                    KPJBA
127390030612     C                   PARM                    Fnlv13DS
127400030612     C                   PARM                    Tisi95DS
127410950104     C* ERRORE
127420970811     C     O13ERR        IFNE      *BLANKS
127430970811     C                   MOVEL     O13MSG        VIDMSG
127440950104     C                   MOVE      CPDC          VIDMSG
127450950104     C                   SETON                                        904728
127460950104     C                   GOTO      ENDCT8
127470950104    3C                   ENDIF
127480950104     C* SALVO IL FLAG ANTEPORTO
127490970811     C                   MOVEL     O95ISO        COMFAP
127500950104     C* SOLO IN VISUALIZZAZIONE
127510970811     C                   MOVEL     O95ISO        VIDFAP
127520140403     c* salvo pickup 2
127530140403     C                   MOVEL     O95gf2        comgf2
127540950104    2C                   ENDIF
127550941223     C*
127560941227     C     ENDCT8        ENDSR
127570961218     C*
127580941223     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE MT -----------------*
127590941227     C     REGIS8        BEGSR
127600130412     c                   clear                   wpda_ndc
127610130412     c                   clear                   wpda_ddc
127620130412     c                   clear                   wpda_dcm
127630950113     C                   MOVEL     ARBRSM        OLDRSM
127640950113     C                   MOVEL     ARBKSC        OLDKSC
127650950113     C                   MOVEL     ARBCAM        OLDCAM
127660041027     C                   MOVEL     ARBcpi        savCpi
127670950104     C*  GIRO UDATE
127680950104     C                   TIME                    W0140            14 0
127690950104     C* UDATE IN GGMMAAAA
127700950104     C                   MOVE      W0140         WDTGIO            8 0
127710950104     C*
127720950104     C* UDATE IN AAAAMMGG
127730950104     C                   Z-ADD     WDTGIO        G02DAT
127740950104     C                   MOVEL     *BLANK        G02ERR
127750950104     C                   CALL      'XSRDA8'
127760950104     C                   PARM                    WLBDAT
127770950104     C                   MOVEL     G02INV        DATEU             8 0
127780950104     C*
127790941223     C                   MOVEL     KNMUS         ARBPRU
127800950104     C*
127810950104     C* SALVO ARBCCM DELLA BOLLA PERCHE LO USO
127820950104     C                   MOVEL     ARBCCM        W0070             7 0
127830941223     C*
127840941223     C* INVIO PER SEDE
127850941223     C                   MOVEL     'FNARBM0T'    SFILE
127860941223     C* ALLOCO
127870941223     C                   EXSR      ALLOC
127880941223     C* ERRORI IN ALLOCAZIONE
127890941223    1C     *IN91         IFEQ      *ON
127900941223     C                   GOTO      ENDRG8
127910941223    1C                   ENDIF
127920041025     c
127930041025     c* alloco record bolla in altro file
127940041025     c                   clear                   waltrabolla       1
127950041025     C   10KARB          CHAIN     FNBLP01L                           3091
127960041025     C  n10KARB          CHAIN     FNARB01L                           3091
127970041025     c*
127980041025     c                   if        *in91
127990041025     C                   MOVEL     MSG(4)        VIDMSG
128000070515     c                   exsr      DLCSED
128010070525     C                   SETON                                        2891
128020041025     C                   GOTO      ENDRG8
128030041025     c                   else
128040041025     c                   if        *in30
128050041025     c                   eval      waltrabolla='N'
128060041025     c                   else
128070041025     c                   eval      waltrocpi=arbcpi
128080041025     c                   eval      waltrofvf=arbfvf
128090041025     c                   eval      waltrovlf=arbvlf
128100130411     c  n10              eval      wpda_ndc=arbndc
128110130411     c  n10              eval      wpda_ddc=arbddc
128120130411     c  n10              eval      wpda_dcm=arbdcm
128130041025     C   10KARB          CHAIN     FNARB01L                           30
128140041025     C  N10KARB          CHAIN     FNBLP01L                           30
128150041025     c* Alla fine per non sporcarmi i campi del record, richaino il
128160041025     c*  record originale che sto variando
128170130411     c   10              eval      wpda_ndc=arbndc
128180130411     c   10              eval      wpda_ddc=arbddc
128190130411     c   10              eval      wpda_dcm=arbdcm
128200041025     c                   endif
128210041025     c                   endif
128220941223     C*
128230941223     C                   Z-ADD     DATEU         ARBDTV
128240941223     C                   TIME                    ARBORV
128250941223     C                   MOVEL     VIDCVR        ARBCVB
128260941223     C*
128270060130     C* SE DA STORICIZZARE
128280060130     C**   *IN10         IFEQ      *ON
128290060130    1C**   D66FSA        ANDEQ     'S'
128300060130     C**   *IN10         OREQ      *OFF
128310060130    1C**   D66FSP        ANDEQ     'S'
128320060130    1C     D66FSA        ifeq      'S'
128330941223     C                   Z-ADD     ARBKSC        ARBCCM
128340011108      *
128350011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
128360011108      * è in partenza 'P' o in arrivo 'A'
128370011108     C     *IN10         IFEQ      *ON
128380011108     C                   MOVE      'A'           ARBPRU
128390011108     C                   ENDIF
128400011108     C     *IN10         IFEQ      *OFF
128410011108     C                   MOVE      'P'           ARBPRU
128420011108     C                   ENDIF
128430011108      *
128440941223     C                   WRITE     FNARBM00
128450941223    1C                   END
128460060302     c* storicizzazione motivazione
128470060302     c     d66mtv        ifeq      'S'
128480060302     c                   exsr      sto_mtv
128490060302     c                   endif
128500941223     C*
128510950104     C                   Z-ADD     VIDKSC        ARBKSC
128520950104     C                   MOVEL     VIDKLP        ARBKSC
128530950104     C                   MOVEL     ARBKSC        ARBCCM
128540970613     C     ARBFDN        IFEQ      ' '
128550970613     C                   MOVEL     COMFAP        ARBFAP
128560970613     C                   ELSE
128570970613     C                   MOVEL     'C'           ARBFAP
128580970613     C                   ENDIF
128590970613     C**
128600020508    1C     VIDKSC        IFEQ      8888
128610941223     C                   MOVEL     VIDRSM        ARBRSM
128620941223     C                   MOVEL     VIDINM        ARBINM
128630941223     C                   MOVEL     VIDLOM        ARBLOM
128640941223     C                   MOVEL     VIDCAM        ARBCAM
128650941223     C                   MOVEL     VIDPRM        ARBPRM
128660941223     C                   MOVEL     VIDNZM        ARBNZM
128670941223     C                   CLEAR                   ARBCPI
128680950626     C*
128690950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
128700950626     C*     NELLA P.IVA
128710950626     C     VIDISM        IFEQ      '$$'
128720950626     C     VIDCPM        ANDEQ     *BLANKS
128730950626     C                   MOVEL     'PRIVATO'     VIDCPM
128740950626     C                   ENDIF
128750950626     C*
128760950104    2C     VIDISM        IFEQ      *BLANKS
128770941223     C                   MOVEL     VIDCPM        ARBCPI
128780941223     C                   ELSE
128790941223     C                   MOVEL     VIDPIM        ARBCPI
128800950104    2C                   ENDIF
128810950104     C*
128820950104     C                   ELSE
128830950412     C* PRENDO I DATI DA CNACO E CNIND
128840950104     C                   MOVEL     ACORAG        ARBRSM
128850950104     C                   MOVEL     INDVIA        ARBINM
128860950104     C                   MOVEL     INDCIT        ARBLOM
128870950104     C                   MOVEL     INDCAE        ARBCAM
128880950104     C                   MOVEL     INDPRV        ARBPRM
128890950104     C                   MOVEL     INDSTA        ARBNZM
128900950104     C                   MOVEL     INDIVA        ARBCPI
128910941223     C                   ENDIF
128920041027     c* Mi salvo laa new P.iva
128930041027     c                   eval      newcpi=arbcpi
128940041027     c
128950041025     c
128960060223     C* SE ESISTE RECORD P.IVA IN FIAR4  LO AGGIORNO
128970120413     c                   exsr      up_fiar4P
128980941223     C*
128990941223     C* PER SEDE
129000070503     C     D66FFI        IFEQ      'S'
129010950227     C*
129020950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
129030950413    2C     §3ABSD        IFEQ      ' '
129040941227     C                   OPEN      FNARBM0T
129050941227     C                   WRITE     FNARBMTT
129060941227     C                   CLOSE     FNARBM0T
129070950227    2C                   END
129080941223     C* DISALLOCO OGGETTO
129090941223     C                   EXSR      DLCSED
129100950208    1C                   END
129110941223     C*
129120941223     C* AGGIORNO FILE TASSAZIONE PADRONCINI
129130950113     C                   EXSR      AGGPMT
129140950104     C*  REIMPOSTO IL CODICE CLIENTE MITTENTE
129150950104     C                   MOVEL     W0070         ARBCCM
129160950104     C*
129170950113     C* CONTROLLO SE SI TRATTA DI VOLUME AUTOMATICO
129180950113     C* SE HA VARIATO IL MITTENTE
129190950113     C     ARBKSC        IFNE      OLDKSC
129200950113     C*
129210950113     C                   MOVEL     '7T'          COD
129220051214     C                   MOVEL(P)  ARBFVF        KEY
129230950113     C                   EXSR      CTABEL
129240950113     C                   MOVEL     TBLUNI        DS7T
129250950113     C     §7TRVL        IFEQ      'A'
129260950113     C     §7TRVL        OREQ      'P'
129270950114     C                   MOVEL     'S'           WRICVL            1
129280950113     C                   ENDIF
129290950113     C                   ENDIF
129300950113     C*
129310950114     C* SALVO I CAMPI COLLEGATI AL MITTENTE CHE USERO PER STORICIZZARE
129320950114     C                   MOVEL     ARBCTR        MITCTR
129330950114     C                   MOVEL     ARBFVF        MITFVF
129340950114     C                   MOVEL     ARBVLF        MITVLF
129350140403     c*
129360140403     c* aggiorno fiar5 record gen per pickup2
129370140407     c                   if        comgf2<>'01' and arbnzm=*blanks and
129380140407     c                             arbfdn=*blanks
129390140407     c                   eval      wpk2='S'
129400140407     c                   else
129410140407     c                   clear                   wpk2
129420140407     c                   endif
129430140407     c                   if        (wpk2='S' and par5pk2<>'S') or
129440140407     c                              (wpk2<>'S' and par5pk2='S')
129450140403     c                   eval      Wtipoagg='PIK'
129460140403     c                   exsr      regisar5gen
129470140403     c                   endif
129480950114     C*
129490950113     C* SE PREMUTO F10 DEVO FARE IL GIRO CON IL CODICE TARIFFA
129500950104     C     *INKJ         IFEQ      *ON
129510070907     C***                MOVEL     'TP'          D48CVB
129520070907     C***                MOVEL     'E'           WESEG
129530070907     c                   add       1             kk
129540070907     c                   movel     'TP'          kcvb(kk)
129550070911     c                   movel     d48ffr        kffr(kk)
129560070911     c                   movel     w48f12        kf12(kk)
129570070907     c
129580950113     C                   ELSE
129590950114     C*
129600950114     C* SE HO RIPROPOSTO IL CODICE TARIFFA DEL CLIENTE DEVO PASSARE
129610950114     C*  DALLA VIDEATA DELLA TASSAZIONE
129620950114     C                   MOVEL     ARBCTR        W003A             3
129630950114     C     W003A         IFNE      VIDCTR
129640070907     C***                MOVEL     'TP'          D48CVB
129650070907     C***                MOVEL     'E'           WESEG
129660070911     C****               CLEAR                   D48FFR
129670070907     c                   add       1             kk
129680070907     c                   movel     'TP'          kcvb(kk)
129690070911     c                   movel     ' '           kffr(kk)
129700070911     c                   movel     ' '           kf12(kk)
129710950114     C                   ELSE
129720950114     C*
129730950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
129740950114     C     WRICVL        IFEQ      'S'
129750950113     C                   EXSR      CALVOL
129760950114     C                   MOVEL     D18FVF        ARBFVF
129770950114     C                   MOVEL     D18VLF        ARBVLF
129780070911     C***                MOVEL     'I2'          D48CVB
129790070911     C***                MOVEL     'E'           WESEG
129800070911     c***                clear                   w48f12
129810070907     c                   add       1             kk
129820070907     c                   movel     'I2'          kcvb(kk)
129830070911     c                   movel     d48ffr        Kffr(KK)
129840070911     c                   movel     ' '           Kf12(kk)
129850950104     C                   ENDIF
129860950113     C                   ENDIF
129870950114     C                   ENDIF
129880950114     C* MEMORIZZO IL COD TARIFFA SULLA BOLLA, TANTO SE E' CAMBIATO
129890950114     C*  PASSO PER FORZA DALLA VIDEATA DELLA TASSAZIONE PER POTERLO
129900950114     C*  TRASMETTERE
129910950114     C                   MOVEL     VIDCTR        ARBCTR
129920000623     C**
129930120416     c* Controllo e Gestione eventuale record W in fiar4
129940120416     c                   exsr      up_fiar4W
129950041027     c* In partenza memorizzo se 1°bolla franco anche se doppia
129960041027     c* In arrivo   memorizzo se 1°bolla franco e non    doppia
129970041027     c                   if        (*in10 and not *in05) or
129980041027     c                             (not *in10 and not *in06)
129990041027     c                   movel     savcpi        arbcpi
130000041027     c                   endif
130010950114     C*
130020950114     C   10              UPDATE    FNARB000
130030950114     C  N10              UPDATE    FNBLP000
130040041025     c*
130050041025     c* Aggiorno anche l'altra bolla
130060041025     c                   if        waltrabolla=' '
130070041025     c* Se  devo riaggiornare il volume benen altrimenti tengo quello d
130080041025     c*  della bolla
130090041025     c                   if        wricvl<>'S'
130100041025     c                   eval      arbfvf=waltrofvf
130110041027     c                   eval      arbvlf=waltrovlf
130120041025     c                   endif
130130041027     c
130140041027     c* P.IVA  idem come sopra
130150041027     c                   if        (*in10 and *in06) or
130160041027     c                             (not *in10 and *in05)
130170041027     c                   movel     newcpi        arbcpi
130180041027     c                   else
130190041027     c                   movel     waltrocpi     arbcpi
130200041027     c                   endif
130210041025     c
130220041025     C   10              except    arbMblp
130230041025     C  N10              except    arbMarb
130240041025     c                   endif
130250950114     C*
130260950114     C* DISALLOCO I RECORD CHE NON USO
130270991007     C   10              UNLOCK    FIAR601L
130280130411     C*
130290130411     c* Richiamo routine per invio variazione a pda
130300130411     c                   exsr      sr_pda
130310950602     C*
130320941227     C     ENDRG8        ENDSR
130330950113     C*
130340961219     C*--- CONTROLLI RIFERIMENTO MITTENTE ----------------------------*
130350961219     C     CONTR9        BEGSR
130360961219     C                   SETOFF                                       9028
130370061024     C*
130380061024     C* NATURA MERCE SEMPRE OBBLIGATORIA
130390061024     C     VIDNAS        IFEQ      *BLANKS
130400061024     C                   SETON                                        419028
130410061024     C                   MOVEL     MSG(75)       VIDMSG
130420061024     C                   END
130430961219     C* SE DESTINATARIO ESTERO IL RIFERIMENTO E' OBBLIGATORIO
130440061024     c* Solo se c'è attraversamento dogana (come fa FNLS01R)
130450961219     C     WDESIE        IFEQ      'E'
130460970110     C     VIDRMA        ANDEQ     *BLANKS
130470061024     C     deseft        ANDne     *BLANKS
130480961219     C                   SETON                                        409028
130490961219     C                   MOVEL     MSG(74)       VIDMSG
130500961219     C                   END
130510961219     C*
130520961219     C                   ENDSR
130530060316     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE RM -----------------*
130540060316     C     REGIS9        BEGSR
130550130412     c                   clear                   wpda_ndc
130560130412     c                   clear                   wpda_ddc
130570130412     c                   clear                   wpda_dcm
130580060316     C*  GIRO UDATE
130590060316     C                   TIME                    W0140            14 0
130600060316     C* UDATE IN GGMMAAAA
130610060316     C                   MOVE      W0140         WDTGIO            8 0
130620060316     C*
130630060316     C* UDATE IN AAAAMMGG
130640060316     C                   Z-ADD     WDTGIO        G02DAT
130650060316     C                   MOVEL     *BLANK        G02ERR
130660060316     C                   CALL      'XSRDA8'
130670060316     C                   PARM                    WLBDAT
130680060316     C                   MOVEL     G02INV        DATEU             8 0
130690060316     C*
130700060316     C                   MOVEL     KNMUS         ARBPRU
130710060316     C* INVIO PER SEDE
130720060316     C                   MOVEL     'FNARBD0T'    SFILE
130730060316     C* ALLOCO
130740060316     C                   EXSR      ALLOC
130750060316     C* ERRORI IN ALLOCAZIONE
130760060316    1C     *IN91         IFEQ      *ON
130770060316     C                   GOTO      ENDRG9
130780060316    1C                   ENDIF
130790060316     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
130800060316     c* iornare direttamente il file
130810060316     c                   clear                   waltrabolla       1
130820060316     C   10KARB          CHAIN     FNBLP01L                           3091
130830060316     C  n10KARB          CHAIN     FNARB01L                           3091
130840060316     c*
130850060316     c                   if        *in91
130860060316     C                   MOVEL     MSG(4)        VIDMSG
130870070515     c                   exsr      DLCSED
130880070525     C                   SETON                                        2891
130890060316     C                   GOTO      ENDRG9
130900060316     c                   else
130910060316     c                   if        *in30
130920060316     c                   eval      waltrabolla='N'
130930060316     c                   else
130940130411     c  n10              eval      wpda_ndc=arbndc
130950130411     c  n10              eval      wpda_ddc=arbddc
130960130411     c  n10              eval      wpda_dcm=arbdcm
130970060316     C   10KARB          CHAIN     FNARB01L                           30
130980060316     C  N10KARB          CHAIN     FNBLP01L                           30
130990060316     c* Alla fine per non sporcarmi i campi del record, richaino il
131000060316     c*  record originale che sto variando
131010130411     c   10              eval      wpda_ndc=arbndc
131020130411     c   10              eval      wpda_ddc=arbddc
131030130411     c   10              eval      wpda_dcm=arbdcm
131040060316     c                   endif
131050060316     c                   endif
131060060316     C                   Z-ADD     DATEU         ARBDTV
131070060320     C                   TIME                    ARBORV
131080060410     C                   MOVEL     VIDCVR        ARBCVB
131090060316     c* STORICIZZAZIONE
131100060316    1C     D66FSA        ifeq      'S'
131110060316      *
131120060316      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
131130060316      * è in partenza 'P' o in arrivo 'A'
131140060316     C     *IN10         IFEQ      *ON
131150060316     C                   MOVE      'A'           ARBPRU
131160060316     C                   ENDIF
131170060316     C     *IN10         IFEQ      *OFF
131180060316     C                   MOVE      'P'           ARBPRU
131190060316     C                   ENDIF
131200060316      *
131210060316     c* nella ragione sociale devo mettere rma e nas ma prima devo
131220060316     c* salvare i campi della ragione sociale
131230060316     c                   movel     arbrsd        s_arbrsd
131240060316     c                   movel     arbrd2        s_arbrd2
131250060316     c                   movel(P)  arbrma        arbrsd
131260060316     c                   movel(P)  arbnas        arbrd2
131270060316     c
131280060316     C                   WRITE     FNARBd00
131290060316     c                   movel     s_arbrsd      arbrsd
131300060316     c                   movel     s_arbrd2      arbrd2
131310060316     C                   END
131320060316     c* storicizzazione motivazione
131330060316     c     d66mtv        ifeq      'S'
131340060316     c                   exsr      sto_mtv
131350060316     c                   endif
131360060316     C* Ora invio solo a sede
131370060316    1C     D66FFI        IFEQ      'S'
131380060316     C*
131390060316     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
131400060316    2C     §3ABSD        IFEQ      ' '
131410060316     C                   OPEN      FNARBD0T
131420060316     c                   movel     arbrsd        s_arbrsd
131430060316     c                   movel     arbrd2        s_arbrd2
131440060316     c                   movel(p)  vidrma        arbrsd
131450060316     c                   movel(p)  vidnas        arbrd2
131460060316     C                   WRITE     FNARBDTT
131470060316     c                   movel     s_arbrsd      arbrsd
131480060316     c                   movel     s_arbrd2      s_arbrd2
131490060316     C                   CLOSE     FNARBD0T
131500060316    2C                   END
131510060316     C* DISALLOCO OGGETTO
131520060316     C                   EXSR      DLCSED
131530060316    1C                   END
131540060316     C*
131550060316     C* F6 - AGGIORNAMENTO
131560060316     C                   MOVE      VIDRMA        ARBRMA
131570060316     C                   MOVE      VIDNAS        ARBNAS
131580060316     C   10              UPDATE    FNARB000
131590060316     C  N10              UPDATE    FNBLP000
131600060316     c
131610060316     c                   if        waltrabolla=' '
131620060316     C                   MOVE      VIDRMA        ARBRMA
131630060316     C                   MOVE      VIDNAS        ARBNAS
131640060316     C   10              except    RMblp
131650060316     C  N10              except    RMarb
131660060316     c                   endif
131670130411     C*
131680130411     c* Richiamo routine per invio variazione a pda
131690130411     c                   exsr      sr_pda
131700060316     c     endrg9        endsr
131710030123      *
131720030123      *--- Controllo campi mancato ritiro BANCALI --------------------*
131730030123     c     Contr10       BegSr
131740030123     c                   Eval      *In90 = *Off
131750030127
131760030127      * Il numero dei bancali non ritirati è obbligatorio
131770030127     c                   If        VidRb1 = *Zeros and VidRb2 = *Zeros
131780030127     c                   Eval      VidMsg = Msg(125)
131790030127     c                   Eval      *In60 = *On
131800030127     c                   Eval      *In28 = *On
131810030127     c                   Eval      *In90 = *On
131820030127     c                   GoTo      EndCt10
131830030127     c                   EndIf
131840030124      * Controllo il n. dei bancali non ritirati
131850030124     c                   If        VidRb1 > VidNba
131860030124     c                   Eval      VidMsg = Msg(124)
131870030124     c                   Eval      *In60 = *On
131880030124     c                   Eval      *In28 = *On
131890030124     c                   Eval      *In90 = *On
131900030124     c                   GoTo      EndCt10
131910030124     c                   EndIf
131920030124     c                   If        VidRb2 > VidNb2
131930030124     c                   Eval      VidMsg = Msg(124)
131940030124     c                   Eval      *In61 = *On
131950030124     c                   Eval      *In28 = *On
131960030124     c                   Eval      *In90 = *On
131970030124     c                   GoTo      EndCt10
131980030124     c                   EndIf
131990030123      * Nominativo
132000030123     c                   If        VidNomb = *Blanks
132010030124     c                   Eval      VidMsg = Msg(118)
132020030124     c                   Eval      *In57 = *On
132030030123     c                   Eval      *In28 = *On
132040030123     c                   Eval      *In90 = *On
132050030123     c                   GoTo      EndCt10
132060030123     c                   EndIf
132070030123      * Motivo
132080030123     c                   If        VidMotb = *Blanks
132090030124     c                   Eval      VidMsg = Msg(120)
132100030124     c                   Eval      *In59 = *On
132110030123     c                   Eval      *In28 = *On
132120030123     c                   Eval      *In90 = *On
132130030123     c                   GoTo      EndCt10
132140030123     c                   EndIf
132150030123
132160030123     c     EndCt10       EndSr
132170030123      **
132180030123      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
132190030123     c     Regis10       BegSr
132200130412
132210130412     c                   clear                   wpda_ndc
132220130412     c                   clear                   wpda_ddc
132230130412     c                   clear                   wpda_dcm
132240030123
132250030124     c                   Eval      §Ar5Prub = Knmus
132260060131     c                   Eval      §Ar5Poub = dutPou
132270030124     c                   Move      Dateu         §Ar5Dav
132280030124     c                   Movel     W0140         §Ar5Orv
132290030124     c                   Eval      §Ar5Rb1 = VidRb1
132300030124     c                   Eval      §Ar5Rb2 = VidRb2
132310030124     c                   Eval      §Ar5Nomb = VidNomb
132320030124     c                   Eval      §Ar5Motb = VidMotb
132330030124     c                   Movel(p)  DAr5Ban       Ar5Uni
132340030124     c                   Clear                   Ar5Ft1
132350030124     c                   Clear                   Ar5Ft2
132360030124     c                   Clear                   Ar5Ft3
132370030124     c                   Update    Fiar5000
132380041020     c*
132390041020     c* aggiornamento file fiqdt00f per gestione rientri
132400041020     c                   z-add     arbifp        kfgs
132410041020     c                   z-add     arbndc        w0060             6 0
132420041020     c     kqdt          chain     fiqdt01l
132430041020     c                   if        %found(fiqdt01l)
132440041020     c                   add       vidrb1        qdtnbnbr
132450041020     c                   add       vidrb2        qdtnbnbr
132460041020     c                   except    aggqdt
132470041020     c                   endif
132480060302     c* storicizzazione motivazione
132490060302     c     d66mtv        ifeq      'S'
132500060320     c                   Move      Dateu         arbdtv
132510060320     c                   movel     w0140         arborv
132520060302     c                   exsr      sto_mtv
132530060302     c                   endif
132540130411     C*
132550130412     c                   clear                   waltrabolla       1
132560130412     c                   if        *in10
132570130412     c                   eval      wpda_ndc=arbndc
132580130412     c                   eval      wpda_ddc=arbddc
132590130412     c                   eval      wpda_dcm=arbdcm
132600130412     c                   unlock    fnarb01l
132610130412     c                   else
132620130412     c                   unlock    fnblp01l
132630131120     c                   if        d66pda<>' '
132640130412     C     KARB          CHAIN(n)  FNARB01L                           30
132650130412     c                   if        *in30
132660130412     c                   eval      waltrabolla='N'
132670130412     c                   else
132680130412     c                   eval      wpda_ndc=arbndc
132690130412     c                   eval      wpda_ddc=arbddc
132700130412     c                   eval      wpda_dcm=arbdcm
132710130412     c                   endif
132720130412     c                   endif
132730130412     c                   endif
132740130412     c*
132750130412     c* Richiamo routine per invio variazione a pda
132760130411     c                   exsr      sr_pda
132770030123
132780030123     c                   EndSr
132790061107     C
132800061107     C*--- CONTROLLI FORMATO 11 --------------------------------------*
132810071025     C     CONTR11       BEGSR
132820061107     C                   SETOFF                                       9028
132830151105     c                   clear                   xcfiva1ds
132840151105     c                   clear                   flgiva            1 0
132850151105     c                   move      v11cod        w0040
132860080505     c
132870061107     c* se non previsto inserimento, ERRORE
132880071025    1c                   if        *in76
132890061107     C                   SETON                                        289040
132900071025    2C                   IF        v11MIDE='M'
132910061107     C                   MOVEL     MSG(92)       VIDMSG
132920061107     C                   ELSE
132930061107     C                   MOVEL     MSG(95)       VIDMSG
132940071025    2c                   endif
132950061107     C                   GOTO      ENDCT11
132960071025    1c                   endif
132970061107     c* flag unico se cliente italia
132980071025    1c                   if        v11mide='M'
132990061107     c                   eval      wcliie=wmitie
133000061107     c                   eval      wprv=arbprm
133010151105     c                   eval      wcap=arbcam
133020151105     c                   eval      wloc=arblom
133030061107     c                   else
133040061107     c                   eval      wcliie=wdesie
133050061107     c                   eval      wprv=arbprd
133060151105     c                   eval      wcap=arbcad
133070151105     c                   eval      wloc=arblod
133080071025    1c                   endif
133090071025     c
133100080505     c* Controllo la Partita IVA
133110071025     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
133120071025     C* obbligatoria
133130071025     c
133140071025    1c                   if        v11pid=*blanks
133150071025     c
133160071025    2c                   if        wdesie='E' and deseft<>*blanks
133170071025     C                   SETON                                        284190
133180071025     C                   MOVEL     MSG(67)       VIDMSG
133190071025     C                   GOTO      ENDCT11
133200120507     c****               else
133210071031     c* solo se non richiamato nascosto
133220120507    3c*****              if        d48ffr='V' or d48ffr='S'
133230120507    4c****               if        wcliie='I' and   forzapid=' '
133240120507     c*****                        and dateu>=§vpopiv
133250120507     C******             SETON                                        289041
133260120507     C*****              MOVEL     MSG(57)       VIDMSG
133270120507     c******             movel     '1'           forzapid
133280120507     C********           GOTO      ENDCT11
133290120507    4c*********          endif
133300120507    3c********           endif
133310071031     c
133320071025    2c                   endif
133330071025    1C                   ENDif
133340071025     C*
133350151105    1c***                if        v11pid<>*blanks
133360071025     c* Se cambiata pulisco la forzatura
133370071025    2c                   if        v11pid<>sav11pid
133380071025     c                   clear                   forzapid2
133390071025     c                   movel     v11pid        sav11pid
133400071025    2c                   endif
133410071025     c
133420151105     c                   clear                   xcfiva1ds
133430071025     c                   clear                   xcfivamod
133440151105     c                   eval      xcfivafori= 'S'
133450100120     c                   eval      xcfivamod = 'P'
133460071025     c                   eval      xcfivapar = v11pid
133470071025     c                   eval      xcfivanar = wnar
133480071025     c                   eval      xcfivaprv = wprv
133490151105     c                   eval      xcfivacap = wcap
133500151105     c                   eval      xcfivaloc = wloc
133510151105     c                   if        w0040=8888 or w0040=9999
133520151105     c                   eval      xcfivap88='8'
133530151105     c                   endif
133540151105     c                   call      'XCFIVAR1'
133550151105     c                   parm                    xcfiva1ds
133560071025     C*
133570071025      * --> errata forzabile
133580100120    2c                   If        xcfivaflg = -9 and forzaPID2 = ' '
133590071025     c                   seton                                        289041
133600071025     c                   Eval      vidmsg = xcfivamsg
133610071025     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
133620100120     c                   eval      forzaPID2= '1'
133630071025     C                   GOTO      ENDCT11
133640071025    2c                   EndIf
133650071025      * --> errore
133660071025    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
133670071025     c                   seton                                        289041
133680071025     c                   eval      vidmsg = xcfivamsg
133690071025     C                   GOTO      ENDCT11
133700071025    2c                   EndIf
133710151105
133720151105     c                   eval      flgiva=xcfivaflg
133730071025     c
133740071025      * se non impostato devo riportare il codice iso della partita iva che
133750071025      * mi viene passato dal pgm di controllo
133760151105    2c                   if        v11pid<>*blanks and v11isd <> ivaeu
133770071025     c                   eval      v11isd = ivaeu
133780071025     c                   seton                                          9041
133790071025     C                   GOTO      ENDCT11
133800071025    2c                   endif
133810151105    1c****               endif
133820080505     c
133830080505      * Devo richiamare il nuovo driver fatto per controllare il codice
133840080505      * fiscale
133850151105     c                   clear                   xcfiva1ds
133860080505     c                   clear                   xcfivamod
133870080505     c                   eval      xcfivapar = v11cdfis
133880080505     c                   eval      xcfivanar = wnar
133890080505     c                   eval      xcfivaprv = wprv
133900151105     c                   eval      xcfivacap = wcap
133910151105     c                   eval      xcfivaloc = wloc
133920151105     c                   eval      xcfivapiva= v11pid
133930151105     c                   if        w0040=8888 or w0040=9999
133940151105     c                   eval      xcfivap88='8'
133950151105     c                   endif
133960151105     c                   call      'XCFIVAR1'
133970151105     c                   parm                    xcfiva1ds
133980080505     c*
133990080505    1c                   if        xcfivaflg < *zeros
134000080505     C                   SETON                                        289040
134010080505     C                   MOVEL     xcfivamsg     VIDMSG
134020080505     C                   GOTO      ENDCT11
134030080505    1c                   endif
134040080505     c
134050080505     c* Obbligatorio se cliente italia partita iva o codice fiscale
134060080505     c* solo se non richiamato nascosto
134070120507    1c***                if        d48ffr='V' or d48ffr='S'
134080120507    2c****               if        wcliie='I' and   v11cdfis=*blanks
134090120507     c****                         and forzacdf=' ' and xcfivaflg<>9
134100120507     C****               SETON                                        289040
134110120507     C*****              MOVEL     MSG(23)       VIDMSG
134120120507     c****               movel     '1'           forzacdf
134130120507     C*****              GOTO      ENDCT11
134140120507    2c*****              endif
134150120507    1c******             endif
134160080505     c
134170080505     c* Se entrambe vuote errore
134180080505    1c                   if        v11cdfis=*blanks and v11pid=*blanks
134190080505     C                   SETON                                        284090
134200080505     C                   MOVEL     MSG(145)      VIDMSG
134210080505     C                   GOTO      ENDCT11
134220080505    1c                   endif
134230120508     c* Se fattura immediata errore se partita iva '$$'
134240120508     c                   if        %scan('$$':v11pid)>0  and wcliie='I'
134250120508     c                   move      v11cod        w0040
134260120508     c                   if        w0040=8888 or w0040=9999
134270120507     C                   SETON                                        284190
134280120507     C                   MOVEL     MSG(149)      VIDMSG
134290120507     C                   GOTO      ENDCT11
134300120508     c                   endif
134310120507     c                   endif
134320120508     c* solo se non richiamato nascosto
134330130315    3c                   if        d48ffr='V' or d48ffr='S'
134340120507     c* Se codice fiscale lungo 11 si richiede la partita IVA  - Errore forzabile
134350120507     c                   if        v11cdfis<>*blanks and v11pid=*blanks and
134360120507     c                             %subst(v11cdfis:12:1)=' ' and wcliie='I'
134370120507     c                             and forzapid=' '
134380120507     C                   SETON                                        289041
134390120507     C                   MOVEL     MSG(57)       VIDMSG
134400120507     c                   movel     '1'           forzapid
134410120507     C                   GOTO      ENDCT11
134420120507     c                   endif
134430130315     c                   endif
134440061107     C*
134450061107     C     ENDCT11       ENDSR
134460130927     c
134470130927     C*--- CONTROLLI FORMATO 12 --------------------------------------*
134480130927     C     CONTR12       BEGSR
134490130927     C                   SETOFF                                       9028
134500130930     c* controllo indirizzo email
134510131001     c                   if        *in86  and videml<>*blanks
134520130930     c                   Clear                   dsemail
134530130930     c                   Eval      §emlindi = videml
134540130930     c                   Call      'XEMAIL'
134550130930     c                   Parm                    dsemail
134560130930if  3c                   If        §emlerro <> '0'
134570130930     C                   SETON                                        289041
134580130930     C                   MOVEL     MSG(150)      VIDMSG
134590130930     C                   GOTO      ENDCT12
134600130930     c                   endif
134610130930
134620130930     c                   endif
134630130930     c
134640130927     C     ENDCT12       ENDSR
134650061107     C*
134660061107     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE FI -----------------*
134670061107     C     REGIS11       BEGSR
134680130412
134690130412     c                   clear                   wpda_ndc
134700130412     c                   clear                   wpda_ddc
134710130412     c                   clear                   wpda_dcm
134720061107     C*  GIRO UDATE
134730061107     C                   TIME                    W0140            14 0
134740061107     C* UDATE IN GGMMAAAA
134750061107     C                   MOVE      W0140         WDTGIO            8 0
134760061107     C*
134770061107     C* UDATE IN AAAAMMGG
134780061107     C                   Z-ADD     WDTGIO        G02DAT
134790061107     C                   MOVEL     *BLANK        G02ERR
134800061107     C                   CALL      'XSRDA8'
134810061107     C                   PARM                    WLBDAT
134820061107     C                   MOVEL     G02INV        DATEU             8 0
134830061107     C                   MOVEL     KNMUS         ARBPRU
134840071025     c*
134850071025     C     V11ISD        IFEQ      '$$'
134860071025     C     V11CPD        ANDEQ     *BLANKS
134870071025     C                   MOVEL     'PRIVATO'     V11CPD
134880071025     C                   ENDIF
134890071025     c
134900071025     c* Salvo a parte la partita iva presente in bolla
134910071025     c                   movel     arbcpi        bollacpi
134920061107     C*
134930061129     c* Se la bolla non è trasmessa non devo allocare il file e non devo
134940061129     c*    creare la variazione
134950061129     c                   if        bollaft2<>' '
134960061107     C* INVIO PER SEDE
134970061107     C                   MOVEL     'FNARBD0T'    SFILE
134980061107     C* ALLOCO
134990061107     C                   EXSR      ALLOC
135000061107     C* ERRORI IN ALLOCAZIONE
135010061107    1C     *IN91         IFEQ      *ON
135020061107     C                   GOTO      ENDRG11
135030061107    1C                   ENDIF
135040061129    1C                   ENDIF
135050061107     c
135060061107     c* Alloco il record fiar4 del codice fiscale                      agg
135070061107     C                   MOVEL     'Q'           WTRC
135080061107     C     KARB2         CHAIN     FiAR4000                           3091
135090061107     c*
135100061107     c                   if        *in91
135110061107     C                   MOVEL     MSG(4)        VIDMSG
135120070515     c                   exsr      DLCSED
135130070525     C                   SETON                                        2891
135140061107     C                   GOTO      ENDRG11
135150061107     c                   endif
135160061107     c
135170061107     c                   if        *in30
135180061107     c                   eval      esiste='N'
135190061116     c                   clear                   ar4not
135200061107     c                   else
135210061107     c                   eval      esiste='S'
135220061107     c                   endif
135230071026     c*
135240071026     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
135250071026     c* iornare direttamente il file solo se non si tratta di bolla doppia
135260071026     c                   clear                   waltrabolla       1
135270130412     c                   setoff                                       3091
135280130412    1c                   if        §3atb2=*blanks and aggiobol<>'N'
135290071026     C   10KARB          CHAIN     FNBLP01L                           3091
135300071026     C  n10KARB          CHAIN     FNARB01L                           3091
135310130412     c                   else
135320131120     c                   if        d66pda<>' '
135330130412     c                   if        not *in10
135340130412     C     KARB          CHAIN(n)  FNARB01L                           30
135350130412     c                   endif
135360130412     c                   eval      wpda_ndc=arbndc
135370130412     c                   eval      wpda_ddc=arbddc
135380130412     c                   eval      wpda_dcm=arbdcm
135390130412     c                   endif
135400130412    1c                   endif
135410071026     c*
135420071026    2c                   if        *in91
135430071026     C                   MOVEL     MSG(4)        VIDMSG
135440071026     c*
135450071026     c                   exsr      DLCSED
135460071026     c                   unlock    fiar401l
135470071026     c
135480071026     C                   SETON                                        2891
135490071026     C                   GOTO      ENDRG11
135500071026   x2c                   else
135510071026    3c                   if        *in30
135520071026     c                   eval      waltrabolla='N'
135530071026   x3c                   else
135540130412    4c                   if        §3atb2=*blanks and aggiobol<>'N'
135550130411     c  n10              eval      wpda_ndc=arbndc
135560130411     c  n10              eval      wpda_ddc=arbddc
135570130411     c  n10              eval      wpda_dcm=arbdcm
135580071026     C   10KARB          CHAIN     FNARB01L                           30
135590071026     C  N10KARB          CHAIN     FNBLP01L                           30
135600071026     c* Alla fine per non sporcarmi i campi del record, richaino il
135610071026     c*  record originale che sto variando
135620130411     c   10              eval      wpda_ndc=arbndc
135630130411     c   10              eval      wpda_ddc=arbddc
135640130411     c   10              eval      wpda_dcm=arbdcm
135650130412    4c                   endif
135660071026    3c                   endif
135670071026    2c                   endif
135680130412    1c****               endif
135690061107     C*
135700061107     C                   Z-ADD     DATEU         ARBDTV
135710061107     C                   TIME                    ARBORV
135720061107     C                   MOVEL     VIDCVR        ARBCVB
135730061107     c* Memorizzo se si tratta del mittente o destinatario
135740061107     c                   movel     v11mide       arbrd2
135750061107     c
135760061107     c* clearo gli altri campi del file che non mi interessano
135770061107     c                   clear                   arbrsd
135780061107     c                   clear                   arbind
135790061107     c                   clear                   arbcad
135800061107     c                   clear                   arblod
135810061107     c                   clear                   arbprd
135820061107     c                   clear                   arbnzd
135830061107     c                   clear                   arbfin
135840061107     c                   clear                   arbpkb
135850061107     c                   clear                   arbpkf
135860061107     c                   clear                   arbvlb
135870061107     c                   clear                   arbvlf
135880061107     c                   clear                   arbfvb
135890061107     c                   clear                   arbfvf
135900061107     c                   clear                   arbffd
135910061107     c                   clear                   arbfst
135920061107     C*
135930061107     C* SE DA STORICIZZARE
135940061107    1C     D66FSA        ifeq      'S'
135950061107      *
135960061107      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
135970061107      * è in partenza 'P' o in arrivo 'A'
135980061107     C     *IN10         IFEQ      *ON
135990061107     C                   MOVE      'A'           ARBPRU
136000061107     C                   ENDIF
136010061107     C     *IN10         IFEQ      *OFF
136020061107     C                   MOVE      'P'           ARBPRU
136030061107     C                   ENDIF
136040061107     c                   if        v11mide='M'
136050061107     c                   movel     ar4not        arbcpi
136060061107     c                   else
136070061107     c                   move      ar4not        arbcpi
136080061107     c                   endif
136090071025     c* Partita iva
136100071025     c                   movel     bollacpi      arbrsd
136110061107      *
136120061107     C                   WRITE     FNARBD00
136130061107    1C                   ENDIF
136140061107    c
136150061107     c* storicizzazione motivazione
136160061107     c     d66mtv        ifeq      'S'
136170061107     c                   exsr      sto_mtv
136180061107     c                   endif
136190071025     c* Aggiorno campo codice fiscale
136200061107     c                   movel     v11cdfis      arbcpi
136210071025     c* Aggiorno campo per la partita iva
136220071025     C                   MOVEL     V11PID        ARBrsd
136230071025     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
136240071025     C*  IL CODICE DELLA P.IVA
136250071025     C     V11CPD        IFNE      *BLANKS
136260071025     C     V11ISD        ANDEQ     *BLANKS
136270071025     C                   CLEAR                   ARBrsd
136280071025     C                   MOVEL     V11CPD        ARBrsd
136290071025     C                   ENDIF
136300130402     c* Se Cd.Fisc. lungo 11 lo metto anche nella P.Iva se vuota
136310130402     c                   if        v11pid=*blanks and %subst(v11cdfis:12:1)=' '
136320130402     c                   movel     v11cdfis      arbrsd
136330130402     c                   endif
136340071025     c
136350071025     C                   MOVEL     'P'           WTRC
136360071025     C     KARB2         CHAIN     FIAR401L                           30
136370071025    1C     *IN30         IFEQ      *OFF
136380071025    2c                   if        v11mide='M'
136390071025     c                   movel     ar4not        wcpifile
136400071025   x2c                   else
136410071025     c                   move      ar4not        wcpifile
136420071025    2c                   endif
136430071025    2c                   if        wcpifile <> arbrsd
136440071025     c                   movel     'T'           ar4ftr
136450071025     c                   z-add     dateu         ar4dtr
136460071025     c                   z-add     dateu         ar4duv
136470071025    3c                   if        v11mide='M'
136480071025     c                   eval      %subst(ar4not:1:16)=%subst(arbrsd:1 :16)
136490071025     c                   else
136500071025     c                   eval      %subst(ar4not:20:16)=%subst(arbrsd:1:16)
136510071025    3c                   endif
136520071025     c
136530071025     C                   UPDATE    FIAR4000
136540071025   x2c                   else
136550071025     c                   unlock    fiar401l
136560071025    2c                   endif
136570071025    1c                   endif
136580061129     c
136590071025    0c                   if        bollaft2<>' '
136600061107     C* Ora invio solo a sede
136610061107     C     D66FFI        IFEQ      'S'
136620061107     C*
136630061107     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
136640061107    2C     §3ABSD        IFEQ      ' '
136650061107     C                   OPEN      FNARBD0T
136660061107     C                   WRITE     FNARBDTT
136670061107     C                   CLOSE     FNARBD0T
136680061107    2C                   END
136690061107     C* DISALLOCO OGGETTO
136700061107     C                   EXSR      DLCSED
136710061107    1C                   END
136720071025    0C                   END
136730061107     C*
136740061107     C* AGGIORNO  file: 30 on write del record
136750071025     C                   MOVEL     'Q'           WTRC
136760071025     C     KARB2         CHAIN     FiAR4000                           3091
136770061107     c                   if        Esiste='N'
136780061107     c                   movel     arbaas        ar4aas
136790061107     c                   movel     arblnp        ar4lnp
136800061107     c                   movel     arbnrs        ar4nrs
136810061107     c                   movel     arbnsp        ar4nsp
136820061107     c                   movel     'Q'           ar4trc
136830061107     c                   clear                   ar4not
136840061107     c                   clear                   ar4flo
136850061107     c                   endif
136860061107     c                   if        v11mide='M'
136870061107     c                   movel     v11cdfis      ar4not
136880061107     c                   else
136890061107     c                   move      v11cdfis      ar4not
136900061107     c                   endif
136910061107     c                   movel     'T'           ar4ftr
136920061107     c                   z-add     dateu         ar4dtr
136930061107     c                   z-add     dateu         ar4duv
136940061107     c
136950061116     c                   if        Esiste='N'  and ar4not<>*blanks
136960061107     c                   write     fiar4000
136970061116     c                   endif
136980061116     c                   if        Esiste='S'  and ar4not<>*blanks
136990061107     c                   update    fiar4000
137000061107     c                   endif
137010061116     c                   if        Esiste='S'  and ar4not=*blanks
137020061116     c                   delete    fiar4000
137030061116     c                   endif
137040071025     c
137050071026     c* Aggiorno la bolla per la partita iva solo se non richiamamto da I0
137060071026     c*  per cui ho già aggiornato la partita iova sulla bolla
137070071026     c                   if        Aggiobol<>'N'
137080071025     c                   movel     arbrsd        arbcpi
137090071025     c* aggiornamento mirato per non sporcare degli altri campi
137100071025     c  n10              except    SolocpiP
137110071025     c   10              except    SolocpiA
137120071026     c
137130071026     c* Se 'c'e' l'altra ed è bolla singola la aggiorno
137140071026     c                   if        §3atb2=*blanks and waltrabolla=' '
137150071026     c  n10              except    SolocpiA
137160071026     c   10              except    SolocpiP
137170071026     c                   endif
137180071026     c                   endif
137190061107     C*
137200061107     C* DISALLOCO  I FILES CHE NON USO
137210061107     C   10              UNLOCK    FiAR901L
137220071025     c** 10              unlock    fnarb01l
137230071025     c**N10              unlock    fnblp01l
137240130411     C*
137250130411     c* Richiamo routine per invio variazione a pda
137260130411     c                   exsr      sr_pda
137270061107     C*
137280061107     C     ENDRG11       ENDSR
137290130927     C*
137300130930     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE Nx -----------------*
137310130927     C     REGIS12       BEGSR
137320130927     c
137330130927     C*  GIRO UDATE
137340130927     C                   TIME                    W0140            14 0
137350130927     C* UDATE IN GGMMAAAA
137360130927     C                   MOVE      W0140         WDTGIO            8 0
137370130927     C*
137380130927     C* UDATE IN AAAAMMGG
137390130927     C                   Z-ADD     WDTGIO        G02DAT
137400130927     C                   MOVEL     *BLANK        G02ERR
137410130927     C                   CALL      'XSRDA8'
137420130927     C                   PARM                    WLBDAT
137430130927     C                   MOVEL     G02INV        DATEU             8 0
137440130927     C                   MOVEL     KNMUS         ARBPRU
137450130927     C                   Z-ADD     DATEU         ARBDTV
137460130927     C                   TIME                    ARBORV
137470130927     C                   MOVEL     VIDCVR        ARBCVB
137480131001      *
137490131001      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
137500131001      * è in partenza 'P' o in arrivo 'A'
137510131001     C     *IN10         IFEQ      *ON
137520131001     C                   MOVE      'A'           ARBPRU
137530131001     C                   ENDIF
137540131001     C     *IN10         IFEQ      *OFF
137550131001     C                   MOVE      'P'           ARBPRU
137560131001     C                   ENDIF
137570130927     C*
137580130927     C* SE DA STORICIZZARE
137590130927    1C     D66FSA        ifeq      'S'
137600130930
137610131001     c                   if        *in84 and
137620131001     c                                  (vidref<>par5ref or vidteld<>par5teld)
137630130930     c* Referente e consegna
137640130927     c                   clear                   darbn_ref
137650130927     c                   eval      §arbnref = par5ref
137660130927     c                   eval      §arbnteld= par5teld
137670130927     c                   eval      arbuni70 = darbn_ref
137680130927     c                   eval      arbtrd='REF'
137690130927     c                   write     FNARBN00
137700131001     c                   endif
137710131001     c
137720131001     c                   if        *in83 and emd_§ar5tel<>vidtsms
137730130930     c                   clear                   darbn_sms
137740130930     c                   eval      §arbncel = emd_§ar5tel
137750130930     c                   eval      arbuni70 = darbn_sms
137760130927     c                   eval      arbtrd='SMS'
137770130927     c                   write     FNARBN00
137780131001     c                   endif
137790131001     c
137800131001     c                   if        *in86 and emd_§ar5eml<>videml
137810130927     c                   clear                   darbn_eml
137820130930     c                   eval      §arbneml=emd_§ar5eml
137830130927     c                   eval      arbuni70 = darbn_eml
137840130927     c                   eval      arbtrd='EML'
137850130927     c                   write     FNARBN00
137860131001     c                   endif
137870131001
137880131001     c                   if        *in81 and (vidnt1<>v1cnt1 or vidnt2<>v1cnt2)
137890130930     c                   clear                   darbn_not
137900130930     c                   eval      §arbnnot1=v1cnt1
137910130930     c                   eval      §arbnnot2=v1cnt2
137920130930     c                   eval      arbuni70 = darbn_not
137930130930     c                   eval      arbtrd='NOT'
137940130930     c                   write     FNARBN00
137950130930     c                   endif
137960130930
137970130930     c                   endif
137980130930     c
137990131001     c                   if        *in84
138000130930     c                   exsr      AggioREF
138010131001     c                   endif
138020131001     c
138030130930      * EMAIL e cell sms
138040131001     c                   if        *in83 or *in86
138050130930     c                   Eval      kAr5Trd = 'EMD'
138060130930     c     kAr5          Chain     Fiar501l
138070130930      * Aggiorno Fiar5
138080130930if  2c                   If        %Found(Fiar501l)
138090130930     c                   movel     ar5uni        DAr5EMD
138100131001     c   86              eval      emd_§ar5eml=videml
138110131001     c   83              eval      emd_§ar5tel=vidtsms
138120130930     c                   Movel(p)  DAr5emd       Ar5Uni
138130130930     c                   Update    Fiar5000
138140130930      * Scrivo Fiar5
138150130930   x2c                   Else
138160130930      * se immessi i dati
138170130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
138180130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
138190130930      * di manutenzione bolle
138200130930if  3c                   If        Videml <> *Blanks or Vidtsms <> *Blanks
138210130930     c                   Clear                   Fiar5000
138220130930     c                   Eval      Ar5Aas = ArbAas
138230130930     c                   Eval      Ar5Lnp = ArbLnp
138240130930     c                   Eval      Ar5Nrs = ArbNrs
138250130930     c                   Eval      Ar5Nsp = ArbNsp
138260130930     c                   Eval      Ar5Trd = 'EMD'
138270130930     c                   Move      Dateu         Ar5Dac
138280130930     c                   Movel     W0140         Ar5Orc
138290130930     c                   Clear                   Dar5emd
138300131001     c   86              eval      emd_§ar5eml=videml
138310131001     c   83              eval      emd_§ar5tel=vidtsms
138320130930     c                   Movel(p)  DAr5emd       Ar5Uni
138330160426     c                   Eval      Ar5Ft1 = 'T'
138340160426     c                   Eval      Ar5Dt1 = *All'9'
138350160426     c                   Eval      Ar5Ft2 = 'T'
138360160426     c                   Eval      Ar5Dt2 = *All'9'
138370130930     c                   Write     Fiar5000
138380130930    3c                   EndIf
138390130930    2c                   EndIf
138400130930     c
138410131001   x1c                   endif
138420130930
138430131001     c                   if        *in81
138440130930     c* aggiorno le NOTE
138450130930     c* aggiorno/deleto trk 8 e 9 se variati
138460130930     c                   clear                   fnlv19ds
138470130930     C                   MOVEL     'T'           D19ftr
138480130930     C                   MOVEL     arbAAS        D19AAS
138490130930     C                   MOVEL     arbLNP        D19LNP
138500130930     C                   MOVEL     arbNRS        D19NRS
138510130930     C                   MOVEL     arbNSP        D19NSP
138520130930     C                   MOVEL     vidnt1        D19NT1
138530130930     C                   MOVEL     vidnt2        D19NT2
138540130930    2C     vidnt1        IFNE      *BLANKS
138550130930     C     vidnt2        ORNE      *BLANKS
138560130930     C                   MOVEL     '8'           D19TRC
138570130930     C                   CALL      'FNLV19R'
138580130930     C                   PARM                    fnlv19ds
138590130930    2C                   ENDIF
138600130930     c* Se il pgm mi restituisce la seconda riga vuota la deleto perchè
138610130930     c* prima poteva esistere
138620130930    2C     D19ERR        IFEQ      *BLANKS
138630130930     C     D19NT1        ANDNE     *BLANKS
138640130930     C     D19NT2        ANDEQ     *BLANKS
138650130930     C                   MOVEL     '9'           D19TRC
138660130930     C                   CLEAR                   D19NT1
138670130930     C                   CLEAR                   D19NT2
138680130930     C                   CALL      'FNLV19R'
138690130930     C                   PARM                    fnlv19ds
138700130930   x2C                   else
138710130930     C* Se tornato codice di err richiamo il pgm 2 volte:  una per nota
138720130930    3C     D19ERR        IFNE      *BLANKS
138730130930     C     vidnt1        oreq      *BLANKS
138740130930     C     vidnt2        ANDEQ     *BLANKS
138750130930     C* Nota 1
138760130930     C                   MOVEL     '8'           D19TRC
138770130930     C                   MOVEL     vidnt1        D19NT1
138780130930     C                   CLEAR                   D19NT2
138790130930     C                   CALL      'FNLV19R'
138800130930     C                   PARM                    fnlv19ds
138810130930     C* Nota 2
138820130930     C                   MOVEL     '9'           D19TRC
138830130930     C                   MOVEL     vidnt2        D19NT1
138840130930     C                   CLEAR                   D19NT2
138850130930     C                   CALL      'FNLV19R'
138860130930     C                   PARM                    fnlv19ds
138870130930    3C                   ENDIF
138880130930    2C                   ENDIF
138890130930
138900130930    1c                   EndIf
138910130927     c*
138920131008     c   10              unlock    fnarb01l
138930131008     c  n10              unlock    fnblp01l
138940130927     C     ENDRG12       ENDSR
138950961219     C*
138960950113     C*--- CALCOLO IL VOLUME AUTOMATICO ------------------------------*
138970950113     C     CALVOL        BEGSR
138980030612     C                   CLEAR                   Fnlv18DS
138990950113     C   10              MOVEL     'A'           D18TBO
139000950113     C  N10              MOVEL     'P'           D18TBO
139010950113     C                   Z-ADD     ARBKSC        D18KSC
139020950113     C                   Z-ADD     ARBCTR        D18CTR
139030950113     C                   MOVEL     §3ATB1        D18TBL
139040950113     C                   Z-ADD     ARBAAS        D18AAS
139050950113     C                   Z-ADD     ARBMGS        D18MGS
139060950113     C                   Z-ADD     ARBNCL        D18NCL
139070950113     C                   Z-ADD     ARBPKF        D18PKF
139080020311     c                   movel     arbtsp        d18tsp
139090020311     c                   z-add     arblna        d18lna
139100020311     c                   z-add     arblnp        d18lnp
139110950113     C                   CALL      'FNLV18R'
139120030612     C                   PARM                    Fnlv18DS
139130950113     C*
139140950113     C                   ENDSR
139150911212     C*
139160970808     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
139170970808     C     CTRIND        BEGSR
139180150203     c                   clear                   normlod
139190970808     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
139200030612     C                   CLEAR                   Fnlv13DS
139210030612     C                   CLEAR                   Tisi95DS
139220970808     C*
139230100120     c                   movel     'S'           i14forzait
139240970808     C                   MOVEL     ARBDSP        I14DRI
139250020305     c                   movel     'S'           i14cnz
139260970808     C                   CALL      'FNLV14R'
139270970808     C                   PARM                    KPJBA
139280030612     C                   PARM                    Fnlv14DS
139290970808     C                   MOVEL     O14ERR        WERR              1
139300970808     C                   MOVEL     O14MSG        VIDMSG
139310970808     C                   EXSR      CTRERR
139320970808     C*
139330970808     C     *IN90         IFEQ      *ON
139340970808     C                   GOTO      ENDIND
139350970808     C                   ENDIF
139360970808     C*************
139370970808     C* CONTROLLO CAP
139380970808     C                   MOVEL     '7'           I95TCN
139390970808     C                   MOVEL     E14CAP        I95CAP
139400970808     C                   MOVEL     E14LOC        I95LOC
139410970808     C                   MOVEL     E14PRV        I95PRV
139420970808     C                   MOVEL     E14NAR        I95NAR
139430000623     C                   MOVEL     ARBDSP        I95DAT
139440020318     C     LNPNTW        IFEQ      'PPT'
139450000623     C                   MOVEL     ARBLNP        I95TFP
139460000623     C                   ELSE
139470980602     C                   MOVEL     ARBTFP        I95TFP
139480000623     C                   ENDIF
139490971230     C                   Z-ADD     ARBPKF        I95LKG
139500971230     C                   Z-ADD     ARBVLF        I95LMC
139510971230     C                   MOVEL     ARBFFD        I95FFD
139520971201     C                   MOVEL     FLGTB1        I95TPO
139530971230     C                   MOVEL     ARBTSP        I95TSP
139540980602     C                   MOVEL     'S'           I95FRE
139550971201     C     §3AFCA        IFNE      0
139560971201     C                   MOVEL     'S'           I95FCA
139570971201     C                   ENDIF
139580970808     C                   MOVEL     'S'           I13AF0
139590140902     c                   if        d48ffr<>'E'
139600970808     C                   MOVEL     'S'           I13CNV
139610140807     c                   endif
139620970808     C                   MOVEL     'S'           I13SZV
139630141119     c* in caso di chiamata cieca non è da impostare: non si deve aprire la int località
139640141119     c                   if        d48ffr<>'E'
139650970808     C                   MOVEL     'S'           I13AF1
139660141119     c                   else
139670141119     C                   MOVEL     ' '           I13AF1
139680141119     c                   endif
139690970808     C                   MOVEL     'S'           I13SZ2
139700970808     C*
139710970808     C* SE CAMBIATO CAP PULISCO CAMPI PER FORZATURE
139720970808     C* ** DESTINATARIO **
139730970808     C     WCLI          IFEQ      'D'
139740970808    2C     E14CAP        IFNE      VARCAD
139750970808     C                   CLEAR                   WZV
139760970808     C                   CLEAR                   WZ1
139770970808     C                   CLEAR                   WZ2
139780970808     C                   MOVEL     E14CAP        VARCAD
139790970808    2C                   ENDIF
139800970808     C* MODIFICA LA LOCALITA'
139810970808    2C     E14LOC        IFNE      VARLOD
139820970808     C                   MOVEL     E14LOC        VARLOD
139830970808     C                   CLEAR                   WZ1
139840970808     C                   CLEAR                   WZ2
139850970808    2C                   ENDIF
139860970808     C                   MOVEL     WZV           E13FZV
139870970808     C                   MOVEL     WZ1           E13FZ1
139880970808     C                   MOVEL     WZ2           E13FZ2
139890020305     c     lnantw        ifeq      'FED'
139900020305     c                   movel     'S'           i95fi1
139910020305     c                   endif
139920970808     C                   ELSE
139930970808     C* ** MITTENTE **
139940970808    2C     E14CAP        IFNE      VARCAM
139950970808     C                   CLEAR                   WZVM
139960970808     C                   CLEAR                   WZ1M
139970970808     C                   CLEAR                   WZ2M
139980970808     C                   MOVEL     E14CAP        VARCAM
139990970808    2C                   ENDIF
140000970808     C* MODIFICA LA LOCALITA'
140010970808    2C     E14LOC        IFNE      VARLOM
140020970808     C                   MOVEL     E14LOC        VARLOM
140030970808     C                   CLEAR                   WZ1M
140040970808     C                   CLEAR                   WZ2M
140050970808    2C                   ENDIF
140060020305     c     lnpntw        ifeq      'FED'
140070020305     c                   movel     'S'           i95fi1
140080020305     c                   endif
140090970808     C                   MOVEL     WZVM          E13FZV
140100970808     C                   MOVEL     WZ1M          E13FZ1
140110970808     C                   MOVEL     WZ2M          E13FZ2
140120970808     C                   END
140130970808     C*
140140970808     C**
140150970808     C** CALL
140160970808     C                   CALL      'FNLV13R'
140170970808     C                   PARM                    KPJBA
140180030612     C                   PARM                    Fnlv13DS
140190030612     C                   PARM                    Tisi95DS
140200970808     C*
140210970808     C                   MOVEL     O13ERR        WERR              1
140220970808     C                   MOVEL     O13MSG        VIDMSG
140230970808     C                   EXSR      CTRERR
140240970808     C*
140250970808     C* REIMPOSTO LE FORZATURE
140260970808     C     WCLI          IFEQ      'D'
140270970808     C                   MOVEL     E13FZV        WZV
140280970808     C                   MOVEL     E13FZ1        WZ1
140290970808     C                   MOVEL     E13FZ2        WZ2
140300970808     C                   ELSE
140310970808     C                   MOVEL     E13FZV        WZVM
140320970808     C                   MOVEL     E13FZ1        WZ1M
140330970808     C                   MOVEL     E13FZ2        WZ2M
140340970808     C                   END
140350150203     c* memorizzo la località normalizzata per destinatario
140360150203     c                   if        wcli='D'
140370150203     c                   eval      normlod=o95loc
140380150203     c                   endif
140390970808     C     ENDIND        ENDSR
140400070320     C*
140410070320     C*--- Gestisce record"L"di FIAR4 --------------------------------*
140420070320     C     GestAR4L      BEGSR
140430070320     c* lo faccio solo se GEO attiva
140440070320    0c                   if        FGSGEOT='S'
140450070320     c* Se devo chainare
140460070320     C                   MOVEL     'L'           WTRC
140470070320     c
140480070321    1c                   if        TipoEla='C'
140490070320     c                   clear                   Wesar4L
140500070320     C     KARB2         CHAIN     FiAR4000                           3091
140510070320    2c                   if        *in91
140520070320     C                   SETON                                        28
140530070320     C                   MOVEL     MSG(4)        VIDMSG
140540070320     C                   GOTO      ENDar4L
140550070320    2c                   endif
140560070320     c
140570070320    2c                   if        not *in30
140580070320     c                   movel     'E'           WEsar4L           1
140590070320     c                   movel     ar4not        dsbl4L
140600070320     c                   else
140610070320     c                   movel     'N'           WEsar4L
140620070320     c                   clear                   dsbl4L
140630070320     c                   clear                   fiar4000
140640070320    2c                   endif
140650070320    1c                   endif
140660070320     c
140670070320    1c                   if        tipoela='A'
140680070320     c                   movel     dsbl4l        ar4not
140690070320     c                   movel     dateu         ar4duv
140700070320     c                   movel     dateu         ar4DTR
140710070320     c                   movel     'T'           ar4ftr
140720070320     c
140730070320    3c                   if        wesar4L='N'
140740070320     c                   eval      ar4aas=arbaas
140750070320     c                   eval      ar4lnp=arblnp
140760070320     c                   eval      ar4nrs=arbnrs
140770070320     c                   eval      ar4nsp=arbnsp
140780070320     c                   eval      ar4trc='L'
140790070320     c                   write     fiar4000
140800070320   x3c                   else
140810070320     c                   update    fiar4000
140820070320    3c                   endif
140830070320    1c                   endif
140840070320     c
140850070320    0c                   endif
140860070320     c
140870070320     C     ENDar4L       ENDSR
140880970808     C*
140890970808     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
140900970808     C     CTRERR        BEGSR
140910970808     C* IMPOSTO CAMPI PASSATI
140920970808    1C     O13RON        IFEQ      'S'
140930970808     C                   MOVEL     O95NAR        E14NAR
140940150112     c                   if        d48ffr='E'
140950150112     c                   clear                   werr
140960150112     c                   eval      msgforzatxt=o13msg
140970150112     c                   endif
140980970808    1C                   ENDIF
140990970808    1C     O13ROC        IFEQ      'S'
141000970808     C                   MOVEL     O95CAP        E14CAP
141010150112     c                   if        d48ffr='E'
141020150112     c                   clear                   werr
141030150112     c                   eval      msgforzatxt=o13msg
141040150112     c                   endif
141050970808    1C                   ENDIF
141060970808    1C     O13ROP        IFEQ      'S'
141070970808     C                   MOVEL     O95PRV        E14PRV
141080150112     c                   if        d48ffr='E'
141090150112     c                   clear                   werr
141100150112     c                   eval      msgforzatxt=o13msg
141110150112     c                   endif
141120970808    1C                   ENDIF
141130970808    1C     O13ROL        IFEQ      'S'
141140970808     C                   MOVEL     O95LOC        E14LOC
141150150112     c                   if        d48ffr='E'
141160150112     c                   clear                   werr
141170150112     c                   eval      msgforzatxt=o13msg
141180150112     c                   endif
141190970808    1C                   ENDIF
141200970808     C* ERRORE O RICERCA CAMPO
141210970808    1C     WERR          IFNE      ' '
141220970808     C*
141230970808    2C     VIDMSG        IFNE      *BLANKS
141240970808     C                   SETON                                        28
141250970808    2C                   ENDIF
141260970808     C* POSIZIONAMENTO CURSORE
141270970808    2C                   SELECT
141280970808     C     WERR          WHENEQ    '1'                                          R.SOCIALE
141290970808     C                   SETON                                        40
141300970808     C     WERR          WHENEQ    '2'                                          INDIRIZZO
141310970808     C                   SETON                                        41
141320970808     C     WERR          WHENEQ    '5'                                          CAP
141330970808     C     WCLI          IFEQ      'D'
141340970808     C                   SETON                                        43
141350970808     C                   ELSE
141360970808     C                   SETON                                        42
141370970808     C                   END
141380970808     C     WERR          WHENEQ    '3'                                          LOCALITA
141390970808     C                   SETON                                        43
141400970808     C     WERR          WHENEQ    '4'                                          PROVINCIA
141410970808     C     WCLI          IFEQ      'D'
141420970808     C                   SETON                                        50
141430970808     C                   ELSE
141440970808     C                   SETON                                        45
141450970808     C                   END
141460970808     C     WERR          WHENEQ    '6'                                          NAZIONE
141470970808     C                   SETON                                        44
141480970808     C     WERR          WHENEQ    '7'                                          PARTITA IVA
141490970808     C     WCLI          IFEQ      'D'
141500970808     C                   SETON                                        45
141510970808     C                   ELSE
141520970808     C                   SETON                                        49
141530970808     C                   END
141540970808     C     WERR          WHENEQ    '8'                                          COD.ISO
141550970808     C     WCLI          IFEQ      'D'
141560970808     C                   SETON                                        46
141570970808     C                   ELSE
141580970808     C                   SETON                                        50
141590970808     C                   END
141600970808     C     WERR          WHENEQ    '9'                                          INOLTRO
141610970808     C     WCLI          ANDEQ     'D'
141620970808     C                   SETON                                        49
141630020318     c     werr          wheneq    '0'
141640020318     C     WCLI          IFEQ      'D'
141650020318     C                   SETON                                        50
141660020318     C                   ELSE
141670020318     C                   SETON                                        45
141680020318     C                   END
141690020318     c
141700970808    2C                   ENDSL
141710970808     C*
141720970808     C                   SETON                                        90
141730970808    1C                   ENDIF
141740970808     C                   ENDSR
141750091217     c*
141760091217     c* Aggiornamento per record GEN ddel fiar5 --------------------------------*
141770091217     c     RegisAR5GEN   BEGSR
141780091217     c                   clear                   dar5gen
141790091217     c                   Eval      kAr5Trd = 'GEN'
141800091217     c     kAr5          Chain     Fiar501l
141810091217      * Aggiorno Fiar5
141820091217if  1c                   If        %Found(Fiar501l)
141830091217     c                   movel     ar5uni        DAr5Gen
141840091217     c*
141850091217     c* Aggiorno tipo incasso in arrivo
141860091217    2c                   select
141870091217     c                   when      Wtipoagg='TIC'
141880091217     c                   Eval      §Ar5ticmb=vidtic
141890100119     c                   eval      §ar5flgMB='S'
141900091217     c
141910091217     c                   when      Wtipoagg='REF'
141920091217     c                   Eval      §Ar5Ref = VidNom
141930091217     c                   Eval      §Ar5Teld = VidTel
141940140403     c                   when      Wtipoagg='PIK'
141950140407     c                   Eval      §Ar5pk2 = wpk2
141960091217    2c                   ENDSL
141970091217
141980091217     c                   Movel(p)  DAr5Gen       Ar5Uni
141990091217     c
142000091217      * se devo trasmettere alla sede sfleggo per la trasmissione
142010091217    2c                   If        §Ar5Trse = 'S'
142020091217     c                   Clear                   Ar5Ft2
142030091217    2c                   EndIf
142040091217     c                   Clear                   Ar5Ft3
142050091217     c                   Update    Fiar5000
142060091217      * Scrivo Fiar5
142070091217   x1c                   Else
142080091217      * se immessi i dati
142090091217     c                   Clear                   Fiar5000
142100091217     c                   Eval      Ar5Aas = ArbAas
142110091217     c                   Eval      Ar5Lnp = ArbLnp
142120091217     c                   Eval      Ar5Nrs = ArbNrs
142130091217     c                   Eval      Ar5Nsp = ArbNsp
142140091217     c                   Eval      Ar5Trd = 'GEN'
142150091217     c                   Move      Dateu         Ar5Dac
142160091217     c                   Movel     W0140         Ar5Orc
142170091217     c                   Clear                   Dar5Gen
142180091217     c
142190091217    2c                   select
142200091217     c                   when      Wtipoagg='TIC'
142210091217     c                   Eval      §Ar5ticmb=vidtic
142220100119     c                   eval      §ar5flgMB='S'
142230091217     c
142240091217     c                   when      Wtipoagg='REF'
142250091217     c                   Eval      §Ar5Ref = VidNom
142260091217     c                   Eval      §Ar5Teld = VidTel
142270140403     c                   when      Wtipoagg='PIK'
142280140407     c                   Eval      §Ar5pk2 = wpk2
142290091217    2c                   ENDSL
142300091217
142310091217     c                   Movel(p)  DAr5Gen       Ar5Uni
142320091217    2c                   if        ar5uni<>*blanks
142330091217     c                   Write     Fiar5000
142340091217    2c                   EndIf
142350091217    1c                   EndIf
142360091217     c
142370091217     c                   ENDSR
142380091217     c
142390911212     C*--- ALLOCO OGGETTI --------------------------------------------*
142400911212     C     ALLOC         BEGSR
142410020805     C                   MOVE      ')'           VAR               7
142420020805     C                   MOVE      '))'          VAR2              8
142430020805     C                   Z-ADD     48            LUNG             15 5
142440920120     C*
142450920120     C                   MOVEA     SFILE         C20(13)
142460920120     C                   MOVEA     SFILE         C21(13)
142470920120     C                   MOVEA     SFILE         C22(13)
142480920120     C                   MOVEA     C22           SOVR(1)
142490920120     C                   MOVEA     C20           SALC(1)
142500920120     C                   MOVEA     C21           SDLC(1)
142510920120     C*
142520041027     C* solo PER SEDE
142530941212     C     §7LFFI        IFEQ      'S'
142540911212     C* CHECK OBJ E ADDPFM
142550971114     C                   Z-ADD     046           PARMBR            3 0
142560020805     c                   move      046           $mbr
142570930210     C                   MOVEL     SFILE         FISICO
142580930210     C                   CALL      'TRUL50C'
142590930210     C                   PARM                    FISICO           10
142600930210     C                   PARM                    MBRFIS           10
142610930210     C                   PARM                    LIB              10
142620930210     C                   PARM                    LOGICO           10
142630930210     C                   PARM                    MBRLOG           10
142640930210     C                   PARM                    ULFLG             1
142650911212     C** OVRDBF
142660020805     c                   movel     $mbr          var
142670920211     C                   MOVE      VAR           SOVR
142680911212     C                   MOVEL     *BLANKS       COMMAN           80
142690911218     C                   MOVEA     SOVR(1)       COMMAN
142700911212     C                   CALL      'QCMDEXC'
142710911212     C                   PARM                    COMMAN
142720911212     C                   PARM                    LUNG
142730911212     C* ALLOCO OGGETTO
142740020805     c                   movel     $mbr          var2
142750920211     C                   MOVE      VAR2          SALC
142760911212     C                   MOVEL     *BLANKS       COMMAN           80
142770911218     C                   MOVEA     SALC(1)       COMMAN
142780911213     C                   CALL      'QCMDEXC'                            91
142790911212     C                   PARM                    COMMAN
142800911212     C                   PARM                    LUNG
142810911212     C*
142820911212     C* 90 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
142830911213     C   91              GOTO      ENDALC
142840911212     C                   END
142850941212     C*
142860941212     C     ENDALC        TAG
142870941212     C* 91 ON - ERRORE
142880941212     C*
142890941212     C     *IN91         IFEQ      *ON
142900941212     C                   SETON                                        28
142910051129     C                   MOVEa     MSG(136)      k78
142920141110     C                   MOVEa     SFILE         k78(68)
142930051129     C                   MOVEa     k78           vidmsg
142940941212     C                   ENDIF
142950941212     C*
142960941212     C                   ENDSR
142970920831     C*
142980940321     C*--- AGGIORNO FILE TASSAZIONE PADRONCINI -----------------------*
142990950113     C     AGGPDS        BEGSR
143000041008     C                   MOVEL     ' '           UNO               1
143010041008     c* richiamo pgm fnlv57ds
143020041008     c                   clear                   fnlv57ds
143030041008     C* 01 ON  - VARIAZIONE DESTINATARIO
143040041008    1c     *in01         ifeq      *on
143050041008    2C     ARBRSD        IFNE      OLDRSD
143060041008     C     ARBCAD        ORNE      OLDCAD
143070041008     C     ARBFIN        ORNE      OLDFIN
143080041008     c                   eval      uno='1'
143090041008    2c                   endif
143100041008    1c                   endif
143110041008     c
143120041008    1c                   if        *in02
143130041008    2c     ARBVLF        IFNE      OLDVLF
143140041008     C     ARBPKF        orne      OLDPKF
143150041008     c                   eval      uno='1'
143160041008    2c                   endif
143170041008    1c                   endif
143180041008     c*
143190041008    1c                   if        uno='1'
143200041008     c                   eval      i57aas=arbaas
143210041008     c                   eval      i57lnp=arblnp
143220041008     c                   eval      i57nrs=arbnrs
143230041008     c                   eval      i57nsp=arbnsp
143240041008     c
143250041008     c                   select
143260041008     c* Se presenti entrambe le bolle
143270041008     c                   when      waltrabolla=' '
143280070320     c                   eval      i57drt=arbdrt
143290070320     c                   eval      i57nrt=arbnrt
143300070320     c                   eval      i57fpp=arbfpp
143310070320     c                   eval      i57prt=arbpdr
143320070320     c                   eval      i57pco=arbpdc
143330041008     c* modifica in arrivo non presente fnblp
143340041008     c                   when      *in10 and waltrabolla='N'
143350041008     c                   eval      i57pdr=arbpdc
143360041008     c                   eval      i57tbo='A'
143370041008     c* modifica in partenza non presente fnarb
143380041008     c                   when      not *in10 and waltrabolla='N'
143390070320     c                   eval      i57pdr=arbpdr
143400041008     c                   eval      i57tbo='P'
143410070320     c                   eval      i57drt=arbdrt
143420070320     c                   eval      i57nrt=arbnrt
143430070320     c                   eval      i57fpp=arbfpp
143440041008     c                   endsl
143450041008     c
143460041008     c                   eval      i57ncl=arbncl
143470041011     c   02              eval      i57tvl='F'
143480041008     c* volume vecchio
143490041008     c                   eval      i57vlc=arbvlc
143500041008     c                   eval      i57ncr=arbncr
143510041008     c                   eval      i57vlf=oldvlf
143520041008     c* volume nuovo
143530041008     c                   eval      i57vcn=arbvlc
143540041008     c                   eval      i57ncn=arbncp
143550041008     c                   eval      i57vfn=arbvlf
143560041008     c* peso vecchio
143570041008     c                   eval      i57pkc=arbpkc
143580041008     c                   eval      i57ncp=arbncp
143590041008     c                   eval      i57pkf=oldpkf
143600041008     c* peso nuovo
143610041008     c                   eval      i57pcn=arbpkc
143620041008     c                   eval      i57npn=arbncp
143630041008     c                   eval      i57pfn=arbpkf
143640041008     c* destinatario vecchio
143650041008     c                   eval      i57rsd=oldrsd
143660041008     c                   eval      i57cad=oldcad
143670041008     c                   eval      i57fin=oldfin
143680041008     c* destinatario nuovo
143690041008     c                   eval      i57rdn=arbrsd
143700041008     c                   eval      i57cdn=arbcad
143710041008     c                   eval      i57fdn=arbfin
143720041008     c
143730041008     c                   call      'FNLV57R'
143740041008     c                   parm                    kpjba
143750041008     c                   parm                    fnlv57ds
143760041008     c                   endif
143770940321     C*
143780940321     C                   ENDSR
143790950113     C*
143800950113     C* AGGIORNA PADRONCINI PER IL MITTENTE --------------------------*
143810950113     C     AGGPMT        BEGSR
143820950113     C*
143830950113    1C     ARBKSC        IFNE      OLDKSC
143840950113     C     ARBRSM        ORNE      OLDRSM
143850950113     C     ARBCAM        ORNE      OLDCAM
143860950113     C*
143870950113     C* CONTROLLO CON QUALE VOLUME HO AGGIORNATO LA BOLLA
143880950113     C                   CLEAR                   WVOL
143890950113     C*
143900950113     C* VOLUME PARZIALE "Z"
143910950113     C*
143920050111     C     ARBNCR        ifne      ARBNCL
143930950113     C*
143940950113    3C     ARBVLC        IFGE      ARBVLF
143950950113     C                   Z-ADD     ARBVLC        WVOL
143960950113   X3C                   ELSE
143970950113     C                   Z-ADD     ARBVLF        WVOL
143980950113    3C                   ENDIF
143990950113     C* VOLUME TOTALE "T"
144000950113   X2C                   ELSE
144010950113     C                   Z-ADD     ARBVLC        WVOL
144020950113    2C                   ENDIF
144030950113     C****
144040950113     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
144050011126     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
144060011126     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
144070950113     C****
144080950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
144090950113     C                   SETOFF                                       17
144100950113     C*
144110070320     C                   MOVE      arbPDR        WPDR
144120070320     C                   Z-ADD     arbDRT        WDRT
144130950113     C* NUMERO DISTINTA
144140070320    2C     arbFPP        IFEQ      'M'
144150950113     C                   Z-ADD     1000          WNDC
144160950113     C                   ELSE
144170950113     C                   Z-ADD     5000          WNDC
144180950113    2C                   ENDIF
144190070320     C                   ADD       arbNRT        WNDC
144200950113     C*
144210950113     C*
144220950113    2C                   DO        2
144230011126     C   17KFTT2         CHAIN     fifst000                           30
144240011126     C  N17KFTT2         CHAIN     fiftt000                           30
144250950113     C*
144260950113    3C     *IN30         IFEQ      *OFF
144270950113     C*
144280950113     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
144290950113    4C     FTTFVL        IFNE      'C'
144300020326     c*
144310020326     c  n17kftd5         chain     fiftd005                           30
144320020326     c   17kftd5         chain     fifsd005                           30
144330020326     c     *in30         ifeq      *off
144340020326     c                   z-add     arbpkf        ftdpkl
144350020326     c                   z-add     wvol          ftdvlu
144360020326     c                   movel     arbrsm        ftdrsc
144370020326     c* dò per scontato che la bolla sia un porto franco
144380020326     c                   z-add     arbksc        ftdksc
144390020326     c                   movel     arbcam        ftdcap
144400020326     c                   movel     arbfap        ftdfin
144410020326     c* se è stato modificato codice cliente o ragione sociale devo
144420020326     c* aggiornare anche il numero di stop
144430020327    1C     ARBKSC        IFNE      OLDKSC
144440020327     C     ARBRSM        ORNE      OLDRSM
144450020327     c                   exsr      aggstp
144460020327     c                   endif
144470020327     c  n17              update    fiftd005
144480020327     c   17              update    fifsd005
144490020326     c                   endif
144500950113     C*
144510950113     C* SE VALORIZZATA SVALORIZZO
144520950113     C                   EXSR      SVAFTT
144530950113     C*
144540950113    4C                   ENDIF
144550950113    3C                   ENDIF
144560950113     C*
144570950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
144580950113     C                   SETON                                        17
144590950113    2C                   ENDDO
144600950113     C*
144610950113    1C                   ENDIF
144620950113     C*
144630950113     C                   ENDSR
144640020327     c*
144650020327     c* AGGSTP - Aggiornamento numero stop a cambio di mittente ------*
144660020327     C     AGGSTP        BEGSR
144670020327     c                   clear                   wstp
144680020327     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
144690020327     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
144700020327     C                   Z-ADD     ftdksc        w0040             4 0
144710020327    5C     w0040         IFNE      *ALL'8'
144720020327     C     w0040         ANDNE     *ALL'9'
144730020327     C     ftdksc        CHAIN     FNCLS000                           31
144740020327     C  N31              z-add     CLSSTP        wstp
144750020327    5C                   ENDIF
144760020327     c     wstp          ifeq      *zeros
144770020327     c* prima di fare la chain mi devo salvare i dati del record di fiftd005
144780020327     c                   movel     dsftd         sav_dsftd
144790020327     C                   MOVEL     ARBKSC        WKSC
144800020327     C                   MOVEL     ARBRSM        WRSM
144810020327     C  N17KFTD          CHAIN(N)  fiftd004                           31
144820020327     C   17KFTD          CHAIN(N)  fifsd004                           31
144830020327     c  n31              z-add     ftdstp        wstp
144840020327     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
144850020327     c* nella distinta il primo numero disponibile
144860020327     c     wstp          ifeq      *zeros
144870020327     C  N17KFTT2         setgt     fiftd001
144880020327     C   17KFTT2         setgt     fifsd001
144890020327     C  N17KFTT2         readpe    fiftd001
144900020327     C   17KFTT2         readpe    fifsd001
144910020327     c                   if        not %eof
144920020327     c                   eval      wstp = (ftdstp+1)
144930020417     c     wstp          iflt      1000
144940020417     c                   z-add     1000          wstp
144950020417     c                   endif
144960020417     c                   else
144970020417     c* qui non dovrebbe mai entrare ma lo gestisco comunque anche
144980020417     c* per unifomrità con fnlsa2r
144990020417     c                   z-add     1000          wstp
145000020327     c                   endif
145010020327     c                   endif
145020020327     c*
145030020327     c* ripristino i dati del record salvato
145040020327     c                   movel     sav_dsftd     dsftd
145050020327     c                   endif
145060020327     c                   z-add     wstp          ftdstp
145070020327     C                   ENDSR
145080020326     c*
145090950113     C* SVALORIZZA UNA DISTINTA --------------------------------------*
145100950113     C     SVAFTT        BEGSR
145110950113     C* SOLO SE VALORIZZATA
145120950113    6C     FTTFVL        IFEQ      'V'
145130950113     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
145140950113     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
145150950113     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
145160950113     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
145170950113     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
145180950113     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
145190950113     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
145200041122     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
145210041122     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
145220950113     C                   MOVEL     PARAM3        KPJBU
145230011129     C  N17              CALL      'FICNE2R'
145240950113     C                   PARM                    KPJBA
145250011129     C   17              CALL      'FICNE9C'
145260950113     C                   PARM                    KPJBA
145270950113    6C                   ENDIF
145280950113     C                   ENDSR
145290941102     C*
145300941102     C* CONTROLLO DIVISA ---------------------------------------------*
145310941102     C     CTRVLT        BEGSR
145320011011      *
145330011011      * La divisa è obbligatoria se è inserito un importo
145340011011     C     W0133         IFGT      *ZEROS
145350011011     C     WVLT          ANDEQ     *BLANKS
145360011011     C                   MOVEL     MSG(100)      VIDMSG
145370011011     C                   SETON                                        9028
145380011011     C                   GOTO      ENDVLT
145390011011     C                   ENDIF
145400941102     C*
145410941102     C                   MOVEL     'CV'          COD
145420941102     C* RICERCA
145430941102     C     '?'           SCAN      WVLT                                   90
145440941102     C     *IN90         IFEQ      *ON
145450991011     C                   CLEAR                   §KEY
145460941102     C                   CALL      'X§TABER'
145470941102     C                   PARM                    CODUT
145480941102     C                   PARM                    COD
145490941102     C                   PARM                    §KEY
145500991011     C                   PARM                    §DES             25
145510941102     C                   MOVEL     §KEY          WVLT
145520941102     C                   GOTO      ENDVLT
145530941102     C                   ENDIF
145540941102     C*
145550941102     C* CONTROLLO DIVISA
145560941102     C                   MOVEL(P)  WVLT          KEY
145570941213     C                   EXSR      CTABEL
145580941102     C* ERRORE
145590941102    1C     *IN30         IFEQ      *ON
145600941102     C                   MOVEL     MSG(26)       VIDMSG
145610941102     C                   SETON                                        9028
145620941102     C                   GOTO      ENDVLT
145630941102    1C                   ENDIF
145640941102     C                   MOVEL     TBLUNI        DSCV
145650941102     C*
145660941102     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
145670941102    1C     §CVFDC        IFNE      'S'
145680941102     C                   MOVE      W0133         W0033             3 3
145690941102    2C     W0033         IFNE      0
145700941102     C                   MOVEL     MSG(27)       VIDMSG
145710941102     C                   SETON                                        9028
145720941102     C                   GOTO      ENDVLT
145730941102    2C                   ENDIF
145740941102    1C                   ENDIF
145750011010      *
145760011010      * Controllo il numero di decimali immessi
145770011011    1C     §CVFDC        IFEQ      'S'
145780011011     C     W0133         ANDNE     *ZEROS
145790011011      *
145800011011    2C                   SELECT
145810011011     C     §CVDEC        WHENEQ    3
145820011011     C     §CVDEC        WHENEQ    2
145830011011     C                   MOVE      W0133         W0010             1 0
145840011011    3C     W0010         IFNE      *ZEROS
145850011011     C                   MOVEA     MSG(93)       K78
145860011011     C                   MOVE      §CVDEC        WCVDEC            2
145870011011     C                   MOVEA     WCVDEC        K78(29)
145880011011     C                   MOVEA     K78           VIDMSG
145890011011     C                   SETON                                        9028
145900011011    3C                   ENDIF
145910011011     C     §CVDEC        WHENEQ    1
145920011011     C                   MOVE      W0133         W0020             2 0
145930011011    3C     W0020         IFNE      *ZEROS
145940011011     C                   MOVEA     MSG(93)       K78
145950011011     C                   MOVE      §CVDEC        WCVDEC            2
145960011011     C                   MOVEA     WCVDEC        K78(29)
145970011011     C                   MOVEA     K78           VIDMSG
145980011011     C                   SETON                                        9028
145990011011    3C                   ENDIF
146000011011     C     §CVDEC        WHENEQ    0
146010011011    2C                   ENDSL
146020011011      *
146030011011    1C                   ENDIF
146040941102     C*
146050941102     C     ENDVLT        ENDSR
146060941212     c*
146070941212     C*--- CHAIN TABEL00F --------------------------------------------*
146080941212     C     CTABEL        BEGSR
146090941212     C     KTAB2         CHAIN     TABEL                              30
146100941212     C  N30TBLFLG        IFNE      ' '
146110941212     C                   SETON                                        30
146120941212     C                   END
146130941212     C                   ENDSR
146140941212     C*---------------------------------------------------------------
146150941212     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
146160941212     C*---------------------------------------------------------------
146170941212     C     DLCSED        BEGSR
146180020805     c                   move      046           $mbr
146190020805     c                   movel     $mbr          var2
146200941220     C                   MOVE      VAR2          SDLC(1)
146210941212     C                   MOVEL     *BLANKS       COMMAN           80
146220941212     C                   MOVEA     SDLC(1)       COMMAN
146230941212     C                   CALL      'QCMDEXC'                            91
146240941212     C                   PARM                    COMMAN
146250941212     C                   PARM                    LUNG
146260941212     C                   ENDSR
146270941213     C*---------------------------------------------------------------
146280941213     C* STORICIZZO LA TASSAZIONE
146290941213     C*---------------------------------------------------------------
146300941213     C     STOARB        BEGSR
146310991006     C* PER LA PRIMA BOLLA KSC E CTR LI PRENDO SEMPRE DALLA BOLLA
146320991006     C* PRINCIPALE PERCHE' SONO SICURA DI AVERLA
146330991006     C* PER LA SECONDA BOLLA LI DEVO PRENDERE DA AR6 PERCHE'
146340991006     C* QUELLI DELLA BOLLA PRINCIPALE SI RIFERISCONO ALLA PRIMA BOLLA
146350991006     C     AR6TRC        IFEQ      '2'
146360991006     C                   Z-ADD     AR6KSC        ARBKSC
146370991006     C                   Z-ADD     AR6CTR        ARBCTR
146380991006     C                   ENDIF
146390991006     C                   MOVEL     AR6DIV        ARBDIV
146400941213     C                   MOVEL     AR6POR        ARBPOR
146410941213     C                   MOVEL     AR6SV1        ARBSV1
146420941213     C                   MOVEL     AR6VA1        ARBVA1
146430941213     C                   MOVEL     AR6SV2        ARBSV2
146440941213     C                   MOVEL     AR6VA2        ARBVA2
146450941213     C                   MOVEL     AR6SV3        ARBSV3
146460941213     C                   MOVEL     AR6VA3        ARBVA3
146470941213     C                   MOVEL     AR6IMV        ARBIMV
146480941213     C                   MOVEL     AR6ALI        ARBALI
146490941213     C                   MOVEL     AR6CEI        ARBCEI
146500941213     C                   MOVEL     AR6IFT        ARBIFT
146510941213     C                   MOVEL     AR6DFT        ARBDFT
146520941213     C                   MOVEL     AR6NFT        ARBNFT
146530941213     C                   MOVEL     AR6FIV        ARBFIV
146540950114     C* SE VENGO DA VARIAIZONE MITTENTE COME CODICE TARIFFA CI METTO
146550950114     C*  QUELLO SALVATO
146560950114     C   24              MOVEL     MITCTR        ARBCTR
146570991013     C                   MOVEL     AR6FIM        ARBFIM
146580011108      *
146590011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
146600011108      * è in partenza 'P' o in arrivo 'A'
146610011108     C     *IN10         IFEQ      *ON
146620011108     C                   MOVE      'A'           ARBPRU
146630011108     C                   ENDIF
146640011108     C     *IN10         IFEQ      *OFF
146650011108     C                   MOVE      'P'           ARBPRU
146660011108     C                   ENDIF
146670011108      *
146680950114     C*
146690991006     C                   WRITE     FIARBT00
146700950114     C*
146710991006     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
146720941221    1C     WAR7          IFEQ      'E'
146730991006    1C     ARBTRC        ANDEQ     '1'
146740991006     C                   MOVEL     '1'           WTRC
146750991006     C     KARB2         SETLL     FIAR7000
146760991006     C     KARB2         READE(N)  FIAR7000                               30
146770941213     C*
146780941213    2C     *IN30         DOWEQ     *OFF
146790941213     C                   MOVEL     AR7SVN        ARBSVN
146800941213     C                   MOVEL     AR7VAN        ARBVAN
146810991006     C                   WRITE     FIARBU00
146820941213     C*
146830991006     C     KARB2         READE(N)  FIAR7000                               30
146840941213    2C                   ENDDO
146850941213     C                   ENDIF
146860041130      *-------
146870041130      * Seconda bolla
146880041130     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
146890041130    1C     WAR72         IFEQ      'E'
146900041130    1C     ARBTRC        ANDEQ     '2'
146910041130     C                   MOVEL     '2'           WTRC
146920041130     C     KARB2         SETLL     FIAR7000
146930041130     C     KARB2         READE(N)  FIAR7000                               30
146940041130     C*
146950041130    2C     *IN30         DOWEQ     *OFF
146960041130     C                   MOVEL     AR7SVN        ARBSVN
146970041130     C                   MOVEL     AR7VAN        ARBVAN
146980041130     C                   WRITE     FIARBU00
146990041130     C*
147000041130     C     KARB2         READE(N)  FIAR7000                               30
147010041130    2C                   ENDDO
147020041130     C                   ENDIF
147030041130      *-------
147040941213     C*
147050941213     C                   ENDSR
147060941214     C*----------------------------------------------------------------
147070941214     C* RICALCOLO IL VOLUME AUTOMATICO
147080941214     C*----------------------------------------------------------------
147090941214     C     VOLAUT        BEGSR
147100941214     C*
147110941214     C* VOLUME AUTOMATICO A PESO: PESO MODIFICATO
147120941214    3C     VIDPKF        IFNE      ARBPKF
147130941214     C     ARBFVF        ANDEQ     'K'
147140941214     C*
147150120702     C     ARBPKF        DIV(H)    ARBVLF        CLIKPM            9 1
147160941214     C     VIDPKF        DIV(H)    CLIKPM        D20VLU
147170941214     C*
147180941214     C     ARBVLB        IFEQ      0
147190941214     C                   Z-ADD     0,001         D20VLU
147200941214     C                   END
147210941214     C                   MOVEL     ARBFVF        D20FVL
147220941214     C*
147230941214    3C                   END
147240941214     C                   ENDSR
147250941215     C**************************************************************************
147260941215     C*  ABBLANCA ZERO NON SIGNIFICATIVI
147270941215     C**************************************************************************
147280941215     C     ABBL          BEGSR
147290941215     C*
147300941215     C                   SETOFF                                       99
147310941215     C     SKI(I)        DOWEQ     '0'
147320941215     C                   MOVEL     ' '           SKI(I)
147330941215     C                   ADD       1             I
147340941215     C     I             COMP      15                                     99
147350941215     C  N99              END
147360941215     C*
147370941215     C                   ENDSR
147380051114     C**************************************************************************
147390051114     C*  ABBLANCA ZERO NON SIGNIFICATIVI
147400051114     C**************************************************************************
147410051114     C     SR_chkcomb    BEGSR
147420051114     c                   clear                   wcomb             1
147430051114     C* Non è possibile indicare contemporaneamente consegne particolari
147440051117     C*  'S' ed 'X' / 'S' ed 'A' / "S" e "P" / "X" e "P"
147450051114    2C                   IF           (wTC1 = 'X'  and  wTC2 = 'S')
147460051114     C                             or (wTC1 = 'S'  and  wTC2 = 'X')
147470051114     C                             or (wTC1 = 'A'  and  wTC2 = 'S')
147480051114     C                             or (wTC1 = 'S'  and  wTC2 = 'A')
147490051114     C                             or (wTC1 = 'S'  and  wTC2 = 'P')
147500051114     C                             or (wTC1 = 'P'  and  wTC2 = 'S')
147510051117     C***                          or (wTC1 = 'X'  and  wTC2 = 'A')
147520051117     C***                          or (wTC1 = 'A'  and  wTC2 = 'X')
147530051114     C                             or (wTC1 = 'X'  and  wTC2 = 'P')
147540051114     C                             or (wTC1 = 'P'  and  wTC2 = 'X')
147550051114     c                   eval      wcomb = 'S'
147560051114     c                   endif
147570051114     C                   ENDSR
147580060227     C**************************************************************************
147590060301     C*  Richiamo pgm di richiesta/controllo motivazione variazione
147600060227     C**************************************************************************
147610060227     C     ges_mtv       BEGSR
147620060227     c                   clear                   fnlv85ds
147630060227     c                   movel     'V'           ilv85tel
147640060227     c                   if        d48ffr = 'S' or d48ffr = 'V'
147650060227     c                   clear                   ilv85wdw
147660060227     c                   else
147670060301     c                   if        d48ffr = *blanks
147680060301     c                   movel     'E'           ilv85wdw
147690060301     c                   else
147700060227     c                   movel     'N'           ilv85wdw
147710060301     c                   endif
147720060227     c                   endif
147730060227     c                   movel     d48cvb        ilv85cvb
147740060227     c                   if        d48mtv <> *blanks
147750060227     c                   movel     d48mtv        ilv85mtv
147760060227     c                   movel     d48not        ilv85nt1
147770060227     c                   move      d48not        ilv85nt2
147780060227     c                   endif
147790060227     c                   movel     fnlv85ds      kpjbu
147800060302     c                   call      'FNLV85R'
147810060227     c                   parm                    kpjba
147820060227     c                   movel     kpjbu         fnlv85ds
147830060227     c                   if        olv85err = '1'
147840060227     c                   seton                                        90
147850060227     C                   eval      vidmsg='Motivazione variazione mancante o er-
147860060227     c                             rata'
147870060227     c                   endif
147880060301     c                   if        olv85f12 = ' ' and olv85err = ' '
147890060301     c                   movel     'N'           w48mct
147900060301     c                   endif
147910060227     c                   endsr
147920060301     C**************************************************************************
147930060301     C*  Richiamo pgm per storicizzazione motivazione variazione
147940060301     C**************************************************************************
147950060301     C     sto_mtv       BEGSR
147960060303     c                   clear                   fnlv85ds
147970060301     c                   movel     ' '           ilv85tla
147980060301     c                   movel     'R'           ilv85tel
147990060301     c                   movel     d48cvb        ilv85cvb
148000060301     c                   z-add     arbdtv        ilv85dac
148010060301     c                   z-add     arborv        ilv85orc
148020060301     c                   z-add     arbaas        ilv85aas
148030060301     c                   z-add     arblnp        ilv85lnp
148040060301     c                   z-add     arbnrs        ilv85nrs
148050060301     c                   z-add     arbnsp        ilv85nsp
148060060303     c                   if        d48mtv <> *blanks and
148070060303     c                             (d48ffr = 'E' or d48mct = 'N')
148080060303     c                   movel     d48mtv        ilv85mtv
148090060303     c                   movel     d48not        ilv85nt1
148100060303     c                   move      d48not        ilv85nt2
148110060303     c                   endif
148120060301     c                   movel     fnlv85ds      kpjbu
148130060301     c                   call      'FNLV85R'
148140060301     c                   parm                    kpjba
148150060301     c                   endsr
148160120330     C**************************************************************************
148170120330     C*  AGGIORNAMENTO DA MANUTENZIONE BOLLE DI SEDE (CAUS.VAR.BOLLA "SD")
148180120330     C**************************************************************************
148190120330     C     GESSD         BEGSR
148200120330     c                   clear                   errp
148210120330     c                   clear                   erra
148220120419     c                   clear                   savcpi_p
148230120419     c                   clear                   savksc_p
148240120419     c                   clear                   savctr_p
148250120419     c                   clear                   savcpi_a
148260120419     c                   clear                   savksc_a
148270120419     c                   clear                   savctr_a
148280120330     c* chaino le bolle in partenza e in arrivo
148290120330     C     KARB          CHAIN(e)  FNBLP01L
148300120402    1c                   if        %error
148310120330     c                   eval      errp='A'
148320120402   x1c                   else
148330120402    2c                   if        not %found(fnblp01l)
148340120330     c                   eval      errp='N'
148350120418     c                   else
148360120418     c                   eval      savcpi_p=arbcpi
148370120418     c                   eval      savksc_p=arbksc
148380120418     c                   eval      savctr_p=arbctr
148390120402    2c                   endif
148400120402    1c                   endif
148410120330     C     KARB          CHAIN(e)  FNARB01L
148420120402    1c                   if        %error
148430120330     c                   eval      erra='A'
148440120402   x1c                   else
148450120402    2c                   if        not %found(fnarb01l)
148460120330     c                   eval      erra='N'
148470120413     c                   else
148480120413     c                   eval      savcpi_a=arbcpi
148490120418     c                   eval      savksc_a=arbksc
148500120418     c                   eval      savctr_a=arbctr
148510120402    2c                   endif
148520120402    1c                   endif
148530120402     c* Una delle due bolle allocate --> restituisco messaggio di avviso
148540120402    1c                   if        errp='A' or erra='A'
148550120402     c                   eval      d48err='A'
148560120402     c                   eval      d48msg='Bolla di filiale allocata da altro l+
148570120418     c                             avoro:NON AGGIORNATA in'
148580120418    2c                   if        errp='A'
148590120418     c                   eval      d48msg=%trim(d48msg)+' Partenza'
148600120418    3c                   if        erra='A'
148610120418     c                   eval      d48msg=%trim(d48msg)+' e'
148620120418    3c                   endif
148630120418    2c                   endif
148640120418    2c                   if        erra='A'
148650120418     c                   eval      d48msg=%trim(d48msg)+' Arrivo'
148660120418    2c                   endif
148670120402    1c                   endif
148680120330     c* se entrambe le bolle allocate/inesistenti vado a fine
148690120402    1c                   if        errp<>*blanks and erra<>*blanks
148700120330     c                   leavesr
148710120402    1c                   endif
148720120404     c*Variazione di PESO
148730120404    1c                   if        §bdpkf<>arbpkf
148740120402     c                   z-add     §bdpkf        arbpkf
148750120404    1c                   endif
148760120404     c*Variazione di VOLUME: il volume, in sede, può essere variato solo da
148770120404     c* profilo di sede. Siccome in questi casi la variazione avviene solitamente
148780120404     c* quando in filiale la bolla non c'è più sostituisco sempre
148790120404    1c                   if        §bdvlf<>arbvlf or §bdfvf<>arbfvf
148800120404     c                   z-add     §bdvlf        arbvlf
148810120411     c                   movel     §bdfvf        arbfvf
148820120404    1c                   endif
148830120529     c* IMOPORTO DA ASSICURARE
148840120529     c                   if        §btias<>arbias or §btvas<>arbvas
148850120529     c                   z-add     §btias        arbias
148860120529     c                   movel     §btvas        arbvas
148870120529     c                   endif
148880120404     c* Variazione TASSAZIONE: vengono caricate solo se c'è corrispodenza
148890120404     c* fra sede e filiale al livello di tipo di fattura: Segue Fattura o
148900120404     c* Fattura Immediata
148910120411     c* Chaino fiar6 per vedere se S.F. o  F.I.:
148920120404     c                   clear                   wfatfil
148930120404     c                   clear                   ar6ksc
148940120411     c                   clear                   war6
148950120416     c                   clear                   fiar6000
148960120416     C                   MOVEL     §bttrc        WTRC
148970120404     c     karb2         chain     fiar601l
148980120411    1c                   if        %found(fiar601l) and ar6nft= 0 or
148990120411     c                             not %found(fiar601l)
149000120404     c                   eval      wfatfil='SF'
149010120411     c                   if        not %found(fiar601l)
149020120411     c                   eval      war6='N'
149030120411     c                   endif
149040120404     c                   else
149050120404     c                   eval      wfatfil='SD'
149060120404    1c                   endif
149070120404    1c                   if        d48cvb=wfatfil
149080120404     c
149090120404     c* . . . . . . . QUANTITA' DA FATTURARE
149100120411    2c                   if        §btqft<>arbqft
149110120411     c                   z-add     §btqft        arbqft
149120120404    2c                   endif
149130120404     c* . . . . . . . CLIENTE DI FATTURAZIONE e COD.TARIFFA
149140120419     C*
149150120404     C                   MOVEL     '3A'          COD
149160120404     C                   MOVEL(P)  ARBCBO        KEY
149170120404     C                   EXSR      CTABEL
149180120404     C  N30              MOVEL     TBLUNI        DS3A
149190120404     C   30              CLEAR                   DS3A
149200120413     c* assegnato
149210120404    2c                   if        §bttrc='2' or %subst(§3atb1:1:1)='A'
149220120404    3c                   if        §btksc<>ar6ksc
149230120404     c                   eval      ar6ksc=§btksc
149240120404    3c                   endif
149250120404    3c                   if        §btctr<>ar6ctr
149260120404     c                   eval      ar6ctr=§btctr
149270120404    3c                   endif
149280120418     c* Se assegnato no seconda bolla devo aggiornare su arb anche il ksc/ctr
149290120418    2c                   if        %subst(§3atb1:1:1)='A'
149300120418    3c                   if        §btksc<>savksc_a
149310120418     c                   eval      savksc_a=§btksc
149320120418    3c                   endif
149330120418    3c                   if        §btctr<>savctr_a
149340120418     c                   eval      savctr_a=§btctr
149350120418    3c                   endif
149360120418     c                   endif
149370120404   x2c                   else
149380120413     c* franco
149390120413     c*  se non codificato non faccio niente
149400120413     c                   z-add     §btksc        w0040
149410120418    3c                   if        w0040<>8888 and §btksc<>arbksc
149420120413     c                   exsr      gessd_ksc
149430120411    3c                   endif
149440120411    3c                   if        §btctr<>arbctr
149450120411     c                   eval      arbctr=§btctr
149460120411    3c                   endif
149470120404    2c                   endif
149480120404     c* . . . . . . . CAMPI DI TASSAZIONE
149490120404     c* . . . . Divisa
149500120404     c                   if        §btdiv<>ar6div
149510120404     c                   eval      ar6div=§btdiv
149520120404     c                   endif
149530120404     c* . . . . Porto
149540120404     c                   if        §btpor<>ar6por
149550120404     c                   eval      ar6por=§btpor
149560120404     c                   endif
149570120411     c* . . . . Imponibile IVA
149580120411     c                   if        §btimv<>ar6imv
149590120411     c                   eval      ar6imv=§btimv
149600120411     c                   endif
149610120411     c* . . . . Causale esenzione
149620120411     c                   if        §btcei<>ar6cei
149630120411     c                   eval      ar6cei=§btcei
149640120411     c                   endif
149650120404     c* . . . . Varie - In §SV e §VA ci sono le varie ricevute dal chiamante
149660120404     c*                 IN WSV e WVA carico le varie presenti sul fiar6/7
149670120411     C                   MOVEL     §sv(1)        ar6sv1
149680120411     C                   Z-ADD     §va(1)        ar6va1
149690120411     C                   MOVEL     §sv(2)        ar6sv2
149700120411     C                   Z-ADD     §va(2)        ar6va2
149710120411     C                   MOVEL     §sv(3)        ar6sv3
149720120411     C                   Z-ADD     §va(3)        ar6va3
149730120411     c* cancello varie su fiar7 se presenti
149740120419     C                   MOVEL     §bttrc        WTRC
149750120411     c     karb2         setll     fiar7000
149760120411     c     karb2         reade     fiar7000
149770120411     c                   dow       not %eof(fiar701l)
149780120411     C                   DELETE    FIAR7000
149790120411     c     karb2         reade     fiar7000
149800120411     c                   enddo
149810120411     c* se presenti più di 3 verie le memorizzo nell'fiar7
149820120411    2c                   if        §sv(4)<>' '
149830120411     c                   z-add     4             inx
149840120411     c                   clear                   AR7ATB
149850120411     c                   z-add     d48aas        AR7AAS
149860120411     c                   z-add     d48lnp        AR7LNP
149870120411     c                   z-add     d48nrs        AR7NRS
149880120411     c                   z-add     d48nsp        AR7NSP
149890120419     c                   movel     §bttrc        AR7TRC
149900120411     c                   dow       §sv(inx)<>*blanks or inx>20
149910120411     C                   MOVEL     §sv(inx)      ar7svn
149920120411     C                   z-add     §va(inx)      ar7van
149930120411     C                   write     fIAR7000
149940120411     c                   add       1             inx
149950120411     c                   enddo
149960120411    2C                   endif
149970120416     c* Aggiornamento FIAR6
149980120416     c* se record non esiste lo creo solo se è pieno almeno uno dei campi
149990120416    2c                   if        war6='N'
150000120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
150010120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
150020120416     c                             or ar6cei<>*blanks
150030120411     c                   eval      AR6AAS=d48aas
150040120411     c                   eval      AR6LNP=d48lnp
150050120411     c                   eval      AR6NRS=d48nrs
150060120411     c                   eval      AR6NSP=d48nsp
150070120411     c                   eval      AR6TRC=§bttrc
150080120411
150090120411     c                   write     fiar6000
150100120416    3c                   endif
150110120417     c                   else
150120120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
150130120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
150140120416     c                             or ar6cei<>*blanks
150150120404     c                   except    ar6sd
150160120416     c                   else
150170120416     c                   delete    fiar6000
150180120416     c                   endif
150190120416    2c                   endif
150200120404   x1c                   else
150210120404     c                   unlock    fiar601l
150220120404    1c                   endif
150230120404     c* aggiornamento file
150240120418     c* BLP
150250120404    1c                   if        errp=*blanks
150260120418     c                   eval      arbcpi=savcpi_p
150270120418     c* solo se assegnato rimetto il ksc/ctr che c'era
150280120418     c                   if        %subst(§3atb1:1:1)='A'
150290120418     c                   eval      arbksc=savksc_p
150300120418     c                   eval      arbctr=savctr_p
150310120418     c                   endif
150320120404     c                   except    arbsdblp
150330120404    1c                   endif
150340120418     c* ARB
150350120404    1c                   if        erra=*blanks
150360120416     c* P.iva DEL MITTENTE SU ARB LA AGGIORNO SOLO SE FRANCO NO DOPPIA BOLLA
150370120419     c* in caso contrario tengo la P.IVA che c'era
150380120416     c                   eval      arbcpi=savcpi_a
150390120418     c*
150400120418     c                   if        %subst(§3atb1:1:1)='A'
150410120418     c                   eval      arbksc=savksc_a
150420120418     c                   eval      arbctr=savctr_a
150430120418     c                   endif
150440120404     c                   except    arbsdarb
150450120404    1c                   endif
150460120330     c
150470120330     C                   ENDSR
150480120413     C**************************************************************************
150490120413     C*  AGGIORNAMENTi derivanti da variazioni in sede del ksc di bolla FRANCO
150500120413     C**************************************************************************
150510120413     C     GESSD_ksc     BEGSR
150520120416     C* Controllo se esiste record W di fiar4 per eventuale modifica di mittente
150530120416     C                   MOVEL     'W'           WTRC
150540120416     c                   clear                   wbl4w
150550120416     C     KARB2         CHAIN(N)  Fiar401l
150560120416     c                   if        %found(fiar401l)
150570120416     C                   MOVEL     'S'           WBL4W             1
150580120416     c                   endif
150590120416     c                   z-add     arbksc        oldksc
150600120416     c*
150610120413     c                   eval      arbksc=§btksc
150620120413     c* recupero dati del cliente da anagrafica
150630120413     C                   clear                   TIBS69DS
150640120413     C                   z-add     §btksc        I69kac
150650120413     C                   z-add     §btksc        I69kin
150660120413     C                   call      'TIBS69R'
150670120413     C                   parm                    tibs69DS
150680120413     C                   parm                    ds_cnaco
150690120413     C                   parm                    ds_cnind
150700120413     C                   parm                    ds_cnclp
150710120413     C                   parm                    ds_fncls
150720120413     c                   eval      arbrsm=bs_acorag
150730120413     c                   eval      arbinm=bs_indvia
150740120413     c                   eval      arbLOM=bs_indcit
150750120413     c                   eval      arbCAM=bs_indcae
150760120413     c                   eval      arbPRM=bs_indprv
150770120413     c                   eval      arbNZM=bs_indsta
150780120418     c* P.IVA: non aggiorno arbcpi perchè comune per blp e arb e non va bene:
150790120418     c* se bolla doppia su blp c'è la p.iva del mittente e su arb la p.iva del dest
150800120413     c* dell'aggiornamento di FNARB
150810120419     c                   if        %subst(§3atb1:1:1)='F' and §3atb2=*blanks
150820120419     c                   eval      savcpi_a=bs_indiva
150830120419     c                   endif
150840120419     c                   eval      savcpi_p=bs_indiva
150850120416     c                   exsr      up_fiar4P
150860120416     c                   exsr      up_fiar4w
150870120416     c*
150880120413     c                   endsr
150890120413     C**************************************************************************
150900120413     C* AGGIORNAMENTO RECORD P.IVA IN FIAR4  se esiste
150910120413     C**************************************************************************
150920120413     C     up_fiar4P     BEGSR
150930120413     C                   MOVEL     'P'           WTRC
150940120413     C     KARB2         CHAIN     FiAR401L                           30
150950120413     C     *IN30         IFEQ      *OFF
150960120413     c                   movel     ar4not        wcpifile
150970120413     c                   if        wcpifile <> arbcpi
150980120413     c                   movel     'T'           ar4ftr
150990120413     c                   z-add     dateu         ar4duv
151000120413     c                   z-add     dateu         ar4dtr
151010120413     C                   MOVEl     ARBCPI        AR4NOT
151020120413     C                   UPDATE    FiAR4000
151030120413     c                   else
151040120413     c                   unlock    fiar401l
151050120413     c                   endif
151060120413     C                   END
151070120413     c                   endsr
151080120416     C**************************************************************************
151090120416     C* Gestione record fiar4 record W per DATA TRASMISSIONE VAB
151100120416     C**************************************************************************
151110120416     C     up_fiar4W     BEGSR
151120120416     C* CONTROLLO SE C'E' NUOVO CODICE O VARIATO PER AGGIORNARE O ELIM
151130120416     C*  O SCRIVERE RECORD W IN ar4
151140120416     C                   MOVEL     '3Q'          COD
151150120416     C                   MOVEL(P)  ARBKSC        KEY
151160120416     C     KTAB2         CHAIN     TABEL00F                           30
151170120416     C  N30TBLFLG        IFNE      ' '
151180120416     C                   SETON                                        30
151190120416     C                   ENDIF
151200120416     C*
151210120416     C* C'E' 3Q DEVO CREARE O VARIARE
151220120416    1C  N10*IN30         IFEQ      *OFF
151230120416    2C     WBL4W         IFEQ      ' '
151240120416    3C     ARBKSC        orNE      OLDKSC
151250120416     C                   CLEAR                   DSBL4W
151260120416     C                   CLEAR                   fnlv19ds
151270120416     C                   MOVEL     DSBL4W        d19nt1
151280120416     C                   MOVEL     ARBAAS        d19AAS
151290120416     C                   MOVEL     ARBLNP        d19LNP
151300120416     C                   MOVEL     ARBNRS        d19NRS
151310120416     C                   MOVEL     ARBNSP        d19NSP
151320120416     C                   MOVEL     'W'           d19TRC
151330120416     C                   MOVEL     'T'           d19ftr
151340120416     C                   call      'FNLV19R'
151350120416     c                   parm                    fnlv19ds
151360120416    2C                   ENDIF
151370120416     C*
151380120416   X1C                   ELSE
151390120416     C* SE NO CI DEVE ESERE E PRIMA C'ERA LO DELETO
151400120416    2C     WBL4W         IFEQ      'S'
151410120416     c                   clear                   fnlv19ds
151420120416     C                   clear                   d19nt1
151430120416     c                   clear                   d19nt2
151440120416     C                   MOVEL     arbAAS        d19AAS
151450120416     C                   MOVEL     arbLNP        d19LNP
151460120416     C                   MOVEL     arbNRS        d19NRS
151470120416     C                   MOVEL     arbNSP        d19NSP
151480120416     C                   MOVEL     'W'           d19TRC
151490120416     c                   movel     'T'           d19ftr
151500120416     C                   CALL      'FNLV19R'
151510120416     C                   PARM                    fnlv19ds
151520120416    2C                   ENDIF
151530120416    1C                   ENDIF
151540120416     c                   endsr
151550130321     C**************************************************************************
151560130411     C* Invio Variazione bolla a PDA
151570130321     C**************************************************************************
151580130411     C     sr_pda        BEGSR
151590140908     c* Solo se la chiamata non è cieca:
151600140908     c                   if        d48ffr<>'E'
151610141020     c****               if        d66pda<>' ' and (*in10 or waltrabolla=' ')
151620141020     c                   if        d66pda<>' ' and *in10
151630131227     c                             and wpda_ndc>0 and wpda_dcm=0
151640131227     c* richiamo pgm per verificare che ci sia stata un'effettiva variazione
151650131227     c                   clear                   fnlva1ds
151660131227     c                   eval      IA1AAS=arbaas
151670131227     c                   eval      IA1LNP=arblnp
151680131227     c                   eval      IA1NRS=arbnrs
151690131227     c                   eval      IA1NSP=arbnsp
151700131227     c                   eval      IA1DTV=arbdtv
151710131227     c                   eval      IA1ORV=arborv
151720131227     c                   eval      IA1CVB=arbcvb
151730131227     c                   eval      IA1PRU=arbpru
151740131227     c                   movel     fnlva1ds      kpjbu
151750131227     c                   call      'FNLVA1R'
151760131227     c                   parm                    kpjba
151770131227     c                   movel     kpjbu         fnlva1ds
151780131227     c*
151790131227     c                   if        OA1ESITO='V'
151800140130     c* se filiale partita con nuova gestione telefonate richiamo pgm gestione
151810140130     c* note che si preoccuperà anche del richiamo a fidg42r
151820140130     c                   if        *in87=*off
151830140130     c                   clear                   fnlrn6ds
151840140130     c                   eval      in6mod='I'
151850140130     c                   eval      in6aas=arbaas
151860140130     c                   eval      in6lnp=arblnp
151870140130     c                   eval      in6nrs=arbnrs
151880140130     c                   eval      in6nsp=arbnsp
151890140130     c                   eval      in6pgm='FNLR48R'
151900140130     c                   eval      IN6FGS=arbifp
151910140130     c                   eval      IN6NFV=wpda_ndc
151920140130     c                   eval      IN6DFV=wpda_ddc
151930140130     c                   eval      IN6PDR=arbpdc
151940140130     c                   eval      IN6VAR=d66pda
151950140130     c                   eval      IN6CONS='S'
151960140130     c                   call      'FNLRN6R'
151970140130     C                   parm                    kpjba
151980140130     C                   parm                    fnlrn6ds
151990140130     c                   else
152000140130     c* altrimenti richiamo direttemente fidg42r
152010131227     c
152020130411     c                   z-add     arbifp        w0030             3 0
152030130411     c     w0030         chain     azorg01l
152040130411     c                   if        %found(azorg01l)
152050130411     c                   eval      og148=orgde8
152060131227     c                   if        §ogpdacon<>' '
152070130411     c                   clear                   fidg42ds
152080130411     c                   eval      CO42FGS=arbifp
152090130411     c                   eval      CO42NDC=wpda_ndc
152100130411     c                   eval      CO42DDC=wpda_ddc
152110130411     c                   eval      CO42AAS=arbaas
152120130411     c                   eval      CO42LNP=arblnp
152130130411     c                   eval      CO42NRS=arbnrs
152140130411     c                   eval      CO42NSP=arbnsp
152150130411     c                   eval      CO42CMT='N'
152160130411     c                   movel     fidg42ds      kpjbu
152170130411     c                   call      'FIDG42R'
152180130411     c                   parm                    kpjba
152190130411     c                   endif
152200130411     c                   endif
152210140130     c                   endif
152220131227     c                   endif
152230130411     c                   endif
152240140908     c                   endif
152250130411     c                   endsr
152260141021     C**************************************************************************
152270141021     C* Gestione finestra per richiesta dirottamento
152280141021     C**************************************************************************
152290141021     C     ges_wdw02     BEGSR
152300141021     c                   clear                   winf12
152310141022     c                   eval      atrf21=rimm
152320141125     c                   setoff                                       782879
152330141125     c* iN partenza non si può mai variare senza il dirottamento --> disabilito l'F8
152340141125     c                   if        not *in10
152350141125     c                   seton                                        79
152360141125     c                   endif
152370141022     c                   eval      w02txt='La Linea di Arrivo desunta è diversa-
152380141022     c                              dalla Linea di Arrivo della bolla'
152390141029     c                   exsr      sr_NoAbiDir
152400141021    6c                   do        *hival
152410141021     c                   exfmt     lr48w02
152420141113     c  n78              setoff                                       28
152430141113     c  n78              clear                   w02msg
152440141021     c                   select
152450141021     c* F21=Richiesta dirottamento
152460141021     c                   when      *inkv
152470141021     c                   eval      wdir='1'
152480141021     c* F12=Ritorno
152490141021     c                   when      *inkl
152500141021     c                   eval      winf12='1'
152510141021     c                   endsl
152520141106     c                   if        *inkv or *inkh or *inkl
152530141021     c                   leave
152540141021     c                   endif
152550141021    6c                   enddo
152560141113     c                   setoff                                       7828
152570141021     c                   endsr
152580141029     C**************************************************************************
152590141029     C* Verifica condizioni che disabilitano F21=Dirottamento
152600141029     C**************************************************************************
152610141029     C     sr_NoAbiDir   BEGSR
152620141029
152630141029     c*******************************************
152640141029     c* Casi di disabilitazione del Dirottamento:
152650141029     c*******************************************
152660141029     c* - BOLLA LEGATA
152670141029     c* Se la bolla è legata il dirottamento può essere richiesto solo sull'ultima figlia
152680141029     c     karb          chain     fnlbl02l
152690141029     c                   if        %found(fnlbl02l)
152700141029     c* disattivo l'f21 ed emetto messaggio per avvisare che il dirottamento deve essere
152710141029     c* fatto sulla figlia
152720141029     c                   seton                                        7828
152730141029     c                   eval      w02msg ='Bolla legata:il dirottamento può -
152740141029     c                             essere richiesto solo sulla figlia'
152750141029     c                   clear                   atrf21
152760141029     c                   leavesr
152770141029     c                   endif
152780141029
152790141029     c* - PRESENZA DISPO DA ESAMINARE
152800141029     c**   kidc1         setll     tiidc01l
152810141029     c***                if        %equal(tiidc01l)
152820141029     c                   clear                   fnlry09ds2
152830141029     c                   eval      ILRY09TCH='T'
152840141029     c                   eval      ILRY09AAS=d48aas
152850141029     c                   eval      ILRY09LNP=d48lnp
152860141029     c                   eval      ILRY09NRS=d48nrs
152870141029     c                   eval      ILRY09NSP=d48nsp
152880141029     c                   eval      kpjbu=fnlry09ds2
152890141031     c                   call      'FNLRY09C'
152900141029     c                   parm                    kpjba
152910141029     c                   eval      fnlry09ds2=kpjbu
152920141029     c                   if        OLRY09ESIT='1'
152930141029     c                   seton                                        7828
152940141029     c                   eval      w02msg ='Presenti Disposizioni di Consegna-
152950141029     c                              da elaborare: non è possibile dirottare'
152960141029     c                   clear                   atrf21
152970141029     c                   leavesr
152980141029     c                   endif
152990141029     c
153000141029     c* - PRESENZA FERMO DEPOSITO
153010141029     c                   if        vidffd<>*blanks
153020141029     c                   seton                                        7828
153030141125     c                   if        not *in79
153040141125     c                   eval      w02msg='Presente Fermo Deposito: F8 per non -
153050141029     c                             dirottare, F12 per correggere'
153060141125     c                   else
153070141125     c                   eval      w02msg='Presente Fermo Deposito: F12 per cor-
153080141125     c                             reggere'
153090141125     c                   endif
153100141029     c                   clear                   atrf21
153110141029     c                   endif
153120141112     c* - PRESENZA giacenza aperta
153130141112     c                   if        *in04
153140141112     c                   seton                                        7828
153150141112     c                   eval      w02msg='Presente Giacenza aperta: non è poss-
153160141112     c                             ibile dirottare'
153170141112     c                   clear                   atrf21
153180141112     c                   endif
153190141029     c                   endsr
153200150414     C**************************************************************************
153210150414     C* controlli per cons. particolare "Z"=EXPO
153220150414     C**************************************************************************
153230151119     C*    sr_chkexpo    BEGSR
153240150414     c* Se Expo errore se bolla con contrassegno
153250151119     c*                  if        §3afca=1
153260151119     C*                  MOVEL     MSG(159)      VIDMSG
153270151119     C*                  SETON                                        9028
153280151119     C*                  leavesr
153290151119     c*                  endif
153300150414     c* se expo errore se non è franco
153310151119     c*                  if        %subst(§3atb1:1:1)<>'F'
153320151119     C*                  MOVEL     MSG(160)      VIDMSG
153330151119     C*                  SETON                                        9028
153340151119     C*                  leavesr
153350151119     c*                  endif
153360150414     c* errore se lna non è in tabella
153370151119     c*                  move      v1clna        w003a
153380151119     c*                  if        %lookup(w003a:fil_expo)=0
153390151119     C*                  MOVEL     MSG(161)      VIDMSG
153400151119     c*                  eval      vidmsg=%trim(vidmsg) + ' ' + w003a
153410151119     C*                  SETON                                        9028
153420151119     C*                  leavesr
153430151119     c*                  endif
153440151119     c*                  endsr
153450151016     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
153460151016     C     REGPesivol    BEGSR
153470151016     c                   clear                   dar5fat
153480151016     c                   Eval      kar5trd='FAT'
153490151016     c     kar5          Chain     Fiar531c
153500151016     c                   If        %Found(Fiar531c)
153510151016     c                   Movel     ar5uni        dar5fat
153520151016     c                   endif
153530151016     c* Se tassata
153540151016    1c                   if        ar6imv>0
153550151016     c* Volume
153560160530     c                   if        tasvlt>0
153570160530     c                   z-add     tasvlt        §ar5vltas
153580160530     c                   movel     tasfvt        §ar5fvtas
153590160530     c                   else
153600151016     c                   movel     arbvlf        §ar5vltas
153610151016     c                   movel     arbfvf        §ar5fvtas
153620160530     c                   endif
153630151016     c* Peso
153640151016     c                   movel     arbpkf        §ar5pktas
153650151016     c                   movel     'R'           §ar5fptas
153660151016     c
153670151016     c* Se il totale imponibile aggiornato senza arrotondamento
153680151016     c*  è = a quello memorizzato al momento del richiamo al
153690151016     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
153700151016    2c                   if        PesVolIMV=Tassatoimv  and
153710151016     c                             PesVolspc='S'
153720151016     c                   movel     PesVolPkcn    §ar5pktas
153730151016    3c                   if        arbncp=arbncl
153740151016     c                   movel     'T'           §ar5fptas
153750151016     c                   else
153760151016     c                   movel     'Z'           §ar5fptas
153770151016    3c                   endif
153780151016     c
153790151016    2c                   endif
153800151016     c                   eval      §AR5DATAS=%dec(%date())
153810151016     c                   eval      §AR5HMTAS=%dec(%time())
153820151016     c                   eval      §AR5PRTAS=knmus
153830151016     c                   eval      §AR5NJTAS=knmtd
153840151016     c*
153850151016    1c                   endif
153860151016     c
153870151016      * aggiorno Fiar5 record "FAT"
153880151016if  1c                   If        %Found(Fiar531c)
153890151016     c* Se ho eliminato la tassazione  deleto
153900151016     c                   if        ar6imv=0
153910151016     c                   if        recname='FIAR5000'
153920151016     c                   delete    fiar500s
153930151016     c                   else
153940151016     c                   delete    fiar5p0s
153950151016     c                   endif
153960151016     c                   else
153970151016     c                   Movel(p)  dar5fat       Ar5uni
153980151016     c                   eval      ar5dac=%dec(%date())
153990151016     c                   eval      ar5orc=%dec(%time())
154000151016     c                   if        recname='FIAR5000'
154010151016     c                   Update    Fiar500s
154020151016     c                   else
154030151016     c                   Update    Fiar5p0s
154040151016     c                   endif
154050151016     c                   endif
154060151016      * scrivo   Fiar5 record "FAT"
154070151016   x1c                   Else
154080151016    2c                   if        ar6imv>0
154090151016     c                   Clear                   Fiar500s
154100151016     c                   Clear                   Fiar5p0s
154110151016     c                   Eval      Ar5Aas = ArbAas
154120151016     c                   Eval      Ar5Lnp = ArbLnp
154130151016     c                   Eval      Ar5Nrs = ArbNrs
154140151016     c                   Eval      Ar5Nsp = ArbNsp
154150151016     c                   Eval      Ar5Trd = 'FAT'
154160151016     c                   Movel(p)  dar5fat       Ar5uni
154170151016     c                   eval      ar5dac=%dec(%date())
154180151016     c                   eval      ar5orc=%dec(%time())
154190151016     c                   Eval      AR5FT1='T'
154200151016     c                   Eval      AR5DT1=99999999
154210151016     c                   Eval      AR5FT2='T'
154220151016     c                   Eval      AR5DT2=99999999
154230151016     c                   Eval      AR5FT3='T'
154240151016     c                   Eval      AR5DT3=99999999
154250151016     c* Verifico dove si trova TITAS per la write di fiar5
154260151016     c     karb          chain     titaS30c
154270160104     c                   if        %found(titas30c)
154280151019     c                   if        recnamet='TITAS000'
154290151016     c                   Write     Fiar500s
154300151016     c                   else
154310151016     c                   Write     Fiar5p0s
154320151016     c                   endif
154330160104     c                   else
154340160104     c                   Write     Fiar500s
154350160104     c                   endif
154360151016    2c                   EndIf
154370151016    1c                   EndIf
154380151016     c
154390151016     c                   ENDSR
154400141020     C**************************************************************************
154410141020     C* Scrittura TIIDC per Caricamento Disposizione di dirottamento
154420141020     C**************************************************************************
154430141020     C     sr_tiidc      BEGSR
154440141020      /free
154450141020       clear tiidc000;
154460141020       exsr repnum;
154470141020       if esito = *blanks;
154480141020          IDCPRG=kprg                         ;
154490151201       // Se progressivo non reperito lo lascio a 0:
154500151201       // per le dispo di reso e dirottamento non abbiamo la necessità di
154510151201       // avere il progressivo dal momento che per una stessa spedizione
154520151201       // non è ammessa la presenza di + dispo se una di queste è dirottamento o reso
154530151201       // Abbiamo quindi preferito scrivere a 0 il progressivo piuttosto che perdere la
154540151201       // dispo. Questo discorso non val per le dispo che riceviamo da web
154550151201       //else;
154560151201       //   leavesr;
154570141020       endif;
154580141020       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
154590141020                                    : 1 : 14 ) );
154600141020       //IDCtimestn   = ;
154610141020       IDCMODACC='B'                       ;
154620141020       //IDCIP = ;
154630141020       //IDCIDCLI = ;
154640141023       if *in10;
154650141023          IDCTIPOCLI= 'D' ;
154660141023       else;
154670141023          IDCTIPOCLI= 'M' ;
154680141023       endif;
154690141020       IDCWAAS   = arbaas;
154700141020       IDCWLNP   = arblnp;
154710141020       IDCWNRS   = arbnrs;
154720141020       IDCWNSP   = arbnsp;
154730141020       IDCAAS    = arbaas;
154740141020       IDCLNP    = arblnp;
154750141020       IDCNRS    = arbnrs;
154760141020       IDCNSP    = arbnsp;
154770141021       IDCFGS    = lnacons;
154780141020       IDCTIPODIS = '3';
154790141020       //IDCMAILAVV     ;
154800141020       //IDCTELAVV  ;
154810141020       //IDCALERT
154820141021          IDCRSD   = vidrsd;
154830141021          IDCIND   = vidind;
154840141021          IDCCAD   = vidcad;
154850141021          IDCLOD   = vidlod;
154860141021          IDCPRD   = vidprd;
154870141021          IDCNAZ   = vidnzd;
154880141020       // bolla da dirottare
154890141021          IDCNEWLNA=d_o95lna;
154900141021          IDCFLA   ='S';
154910141020       IDCELAFLG='S';
154920141022       IDCELAUTE=knmus;
154930141023       idcref=Vidref;
154940141023       idcteld=vidteld;
154950141020
154960141020         write tiidc000;
154970141020       endsr;
154980141020        //-----------------------------------------------------------------------*
154990141020        // Reperimento progressivo richiesta da numeratore 650
155000141020        //-----------------------------------------------------------------------*
155010141020        begsr RepNum;
155020141020
155030141022          annocur  = %subdt(%date():*years);
155040141020          clear esito;
155050141020          $finenum=*off;
155060141020          clear trul33ds;
155070141020
155080141020          I33TLa = 'L';
155090141020          I33Ope = *ZEROS;
155100141030          I33CNu = 651;
155110141020          I33Num = 1;
155120141020
155130141020          KPJBU = TRUL33DS;
155140141020
155150141020          dow $finenum=*off   ;
155160141020             Trul33R(kpjba);
155170141020
155180141020             TRUL33DS = KPJBU;
155190141020
155200141020             if O33Err <> *zeros;
155210141020               Esito = '2';
155220141020               $finenum=*on;
155230141020             else;
155240141020               w0090=o33nri;
155250141020               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
155260141020               setll kprg tiidc02l;
155270141020        // Se numero già presente nel file TIIDC ne cerco un altro
155280141020               if not %equal;
155290141020                  $finenum=*on;
155300141020               endif;
155310141020             endif;
155320141020          enddo;
155330141020
155340141020          endsr;
155350160318        //-----------------------------------------------------------------------*
155360160318        // Aggiornamenti specifici per "VEDI PACCO DPD"
155370160318        //-----------------------------------------------------------------------*
155380160318        begsr sr_VediPDpd ;
155390160318        // Per prima cosa chaino fiar4 record "I" per verificare se si tratta di
155400160318        // "VEDI PACCO"
155410160318             chain(n) (arbaas:arblnp:arbnrs:arbnsp:'I') fiar401l ;
155420160318             if %found(fiar401l);
155430160318                dsbl4i=ar4not;
155440160318              if §b4trd='1';
155450160318        // Se "VEDI PACCO" con giro forzato come da tabella NSD:
155460160318                 chain (arbaas:arblnp:arbnrs:arbnsp) fiarg01l;
155470160318                 if %found(fiarg01l) and argcgi=§nsdcgi;
155480160318        // Memorizzo che, appena aggiornata la bolla, dovrò richiamare il pgm per togliere
155490160318        // la sped. dalla distinta
155500160318        // (Solo se manutenzione fatta in arrivo)
155510160318                  if *in10 and arbndc>0 ;
155520160318                    wtogli='1'      ;
155530160318                  endif;
155540160318        // Cancello il giro forzato
155550160318                    clear argcgi;
155560160318                    clear argtgi;
155570160318                    update fiarg000;
155580160318                 else ;
155590160318                    unlock fiarg01l ;
155600160318                 endif;
155610160318        // Aggiorno il flag Traduttore da 1 a 2
155620160318                 clear  fnlv19ds ;
155630160318                 D19AGG='E';
155640160318                 D19FTR='T';
155650160318                 D19AAS=arbaas;
155660160318                 D19LNP=arblnp;
155670160318                 D19NRS=arbnrs;
155680160318                 D19NSP=arbnsp;
155690160318                 D19TRC='I';
155700160318                 if *in10;
155710160318                    §b4trd='2';
155720160318                 else;
155730160318                    §b4trd='3';
155740160318                 endif;
155750160318                 D19NT1=dsbl4i;
155760160318                 fnlv19r(fnlv19ds);
155770160318              endif;
155780160318             endif;
155790160318        endsr;
155800160318        //-----------------------------------------------------------------------*
155810160318        // Toglie da distinta spedizione "VEDI PACCO DPD"
155820160318        //-----------------------------------------------------------------------*
155830160318        begsr sr_TogliVDpd ;
155840160318                    clear  fnlr98ds;
155850160318                    I98AAS    = arbaas;
155860160318                    I98LNP    = arblnp;
155870160318                    I98NRS    = arbnrs;
155880160318                    I98NSP    = arbnsp;
155890160318                    I98VIDEO  = 'N';
155900160318                    I98npg    = 4  ;
155910160318                    I98fgs    = arbifp ;
155920160318                    I98ndc    = arbndc ;
155930160331                    I98ALCDST = ' ' ;
155940160331                    I98CTLDST = ' ' ;
155950160330                    I98STRCMT = '  ';
155960160318                    kpjbu = fnlr98ds ;
155970160330                    fnlr98c(kpjba);
155980160401        //          if o98esito='1';
155990160401        //              msgforzatxt=O98MSGERR ;
156000160401        //          endif;
156010160318        endsr;
156020141020      /end-free
156030940321     C*---------------------------------------------------------------*
156040041007     Ofnblp000  E            ARBDblp
156050041007     O                       arbrsd
156060041007     O                       arbind
156070041007     O                       arbcad
156080041007     O                       arblod
156090041007     O                       arbprd
156100041007     O                       arbnzd
156110041007     O                       arbfin
156120041007     O                       arbcpi
156130041007     O                       arbpkb
156140041007     O                       arbpkf
156150041007     O                       arbvlb
156160041007     O                       arbfvb
156170041007     O                       arbvlf
156180041007     O                       arbfvf
156190041007     O                       arbffd
156200041007     O                       arbcts
156210041022     Ofnblp000  E            ARBKblp
156220041022     O                       arbcbo
156230041022     Ofnblp000  E            ARBGblp
156240041022     O                       arbdcr
156250041022     O                       arbhcr
156260041022     O                       arbtcr
156270041022     O                       arbgc1
156280041022     O                       arbgc2
156290041022     O                       arbtc1
156300041022     O                       arbtc2
156310041022     O                       arbft3
156320041025     Ofnblp000  E            ARBMblp
156330041025     O                       arbksc
156340041025     O                       arbrsm
156350041025     O                       arbinm
156360041025     O                       arbcam
156370041025     O                       arblom
156380041025     O                       arbprm
156390041025     O                       arbnzm
156400041025     O                       arbfap
156410041025     O                       arbcpi
156420041025     O                       arbCTR
156430041025     O                       arbfvf
156440041025     O                       arbvlf
156450041027     Ofnblp000  E            ARBTblp
156460041027     O                       arbvas
156470041027     O                       arbias
156480041027     O                       arbvmd
156490041027     O                       arbvad
156500041027     O                       arbqft
156510041027     O                       arbctr
156520041028     Ofnblp000  E            RMblp
156530041028     O                       arbrma
156540041028     O                       arbnas
156550071025     Ofnblp000  E            SoloCPIP
156560071025     O                       arbcpi
156570120404     Ofnblp000  E            ARBsdblp
156580120404     O                       arbpkf
156590120404     O                       arbvlf
156600120404     O                       arbfvf
156610120416     O                       arbksc
156620120416     O                       arbctr
156630120416     O                       arbrsm
156640120416     O                       arbinm
156650120416     O                       arbLOM
156660120416     O                       arbCAM
156670120416     O                       arbPRM
156680120416     O                       arbNZM
156690120416     O                       arbcpi
156700120531     o                       arbias
156710120531     o                       arbvas
156720041007     Ofnarb000  E            ARBDarb
156730041025     O                       arbrsd
156740041007     O                       arbind
156750041007     O                       arbcad
156760041007     O                       arblod
156770041007     O                       arbprd
156780041007     O                       arbnzd
156790041007     O                       arbfin
156800041007     O                       arbcpi
156810041007     O                       arbpkb
156820041007     O                       arbpkf
156830041007     O                       arbvlb
156840041007     O                       arbfvb
156850041007     O                       arbvlf
156860041007     O                       arbfvf
156870041007     O                       arbffd
156880041007     O                       arbcts
156890041022     Ofnarb000  E            ARBKarb
156900041022     O                       arbcbo
156910041025     Ofnarb000  E            ARBGarb
156920041022     O                       arbdcr
156930041022     O                       arbhcr
156940041022     O                       arbtcr
156950041022     O                       arbgc1
156960041022     O                       arbgc2
156970041022     O                       arbtc1
156980041022     O                       arbtc2
156990141223     O                       arbfbc
157000141223     O                       arbcmc
157010041025     Ofnarb000  E            ARBMarb
157020041025     O                       arbksc
157030041025     O                       arbrsm
157040041025     O                       arbinm
157050041025     O                       arbcam
157060041025     O                       arblom
157070041025     O                       arbprm
157080041025     O                       arbnzm
157090041025     O                       arbfap
157100041025     O                       arbcpi
157110041025     O                       arbCTR
157120041025     O                       arbfvf
157130041025     O                       arbvlf
157140041027     Ofnarb000  E            ARBTarb
157150041027     O                       arbvas
157160041027     O                       arbias
157170041027     O                       arbvmd
157180041027     O                       arbvad
157190041027     O                       arbqft
157200041027     O                       arbctr
157210041028     Ofnarb000  E            RMarb
157220041028     O                       arbrma
157230041028     O                       arbnas
157240071025     Ofnarb000  E            SoloCPIA
157250071025     O                       arbcpi
157260120404     Ofnarb000  E            ARBsdarb
157270120404     O                       arbpkf
157280120404     O                       arbvlf
157290120404     O                       arbfvf
157300120416     O                       arbksc
157310120416     O                       arbctr
157320120416     O                       arbrsm
157330120416     O                       arbinm
157340120416     O                       arbLOM
157350120416     O                       arbCAM
157360120416     O                       arbPRM
157370120416     O                       arbNZM
157380120416     O                       arbcpi
157390120531     o                       arbias
157400120531     o                       arbvas
157410120411     ofiar6000  e            AR6SD
157420120411     o                       AR6KSC
157430120411     o                       AR6CTR
157440120411     o                       AR6DIV
157450120411     o                       AR6POR
157460120411     o                       AR6SV1
157470120411     o                       AR6VA1
157480120411     o                       AR6SV2
157490120411     o                       AR6VA2
157500120411     o                       AR6SV3
157510120411     o                       AR6VA3
157520120411     o                       ar6imv
157530120411     o                       ar6cei
157540041020     Ofiqdt000  E            AGGqdt
157550041020     O                       qdtnbnbr
157560011120**         C20
157570020805ALCOBJ OBJ((FLARXXXX   *FILE   *EXCL   M000000))
157580920120**         C21
157590020805DLCOBJ OBJ((FLARXXXX   *FILE   *EXCL   M000000))
157600920120**         C22
157610020805OVRDBF FILE(FLARXXXX)               MBR(M000000)
157620930507**         GGG
1576309803091234567
157640050601** cmdt
157650050601OVRDBF FILE(TITA430C) TOFILE(
157660151016OVRDBF FILE(FIAR531C) TOFILE(
157670151016OVRDBF FILE(TITAS30C) TOFILE(
157680941014**
157690941102Spedizione al momento non disponibile: gia' utilizzata da un altro utente     01
157700980406Peso mancante                                                                 02
157710980406Volume mancante                                                               03
157720051129Variazione non eseguibile: bolla allocata da altro utente. Attendi e riprova
157730051129Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL05
157740941102Data formalmente errata                                                       06
157750941102Data consegna richiesta non puo' essere inferiore della data spedizione       07
157760941102Ora richiesta formalmente errata                                              08
157770941102Se immesso tipo immettere anche ora o data consegna richiesta                 09
157780941102Giorno di chiusura errato                                                     10
157790000510Consegna particolare errata o non utilizzabile                                11
157800941102I giorni di chiusura non possono essere uguali
157810941102Il destinatario non puo' essere sempre chiuso!!!?!!
157820941102Le consegne particolari non possono essere uguali
157830051117Impossibile modificare Cons.Part. " " :immessa da partenza no gestib.in arriv 15
157840941102Codice Bolla errato o non utilizzabile
157850941102Codice bolla esistente e nuovo non possono essere uguali
157860941102Impossibile trasformare una bolla da non recupero in recupero o viceversa
157870941213Impossibile effettuare cambi di porto: utilizzare l'apposito programma
157880941213Il codice bolla non ammette il C/Assegno                                      20
157890941102Il codice bolla vuole il C/Assegno: inserire almeno l'importo
157900941102Impossibile variare i campi relativi al C/Assegno perche' gia' incassato
157910071025Codice Fiscale obbligatorio per cliente ITALIA.  Enter per forzare            23
157920131002Mancano i dati passati per il tipo di aggiornamento richiesto                 24
157930941102Se immessa la valuta immettere anche l'importo o eliminare entrambi           25
157940941102Valuta errata
157950000510Non ammessi importi con decimali per la valuta indicata                       27
157960000510Se modificata la divisa, ridigitare il relativo importo                       28
157970941102Impossibile immettere le particolarita' C/Assegno per bolla senza C/A
157980131008$$Sono ammesse solo spedizioni in  FRANCO  per le  POSTE                        30
157990131008Causale variazione errata per il tipo di dati passati                         31
158000000510Spedizione POSTE : peso superiore a xxxxxx,x kg.  Enter per forzare!          32
158010941103Filiale Cliente inesistente
158020941103Per FATTURA IMMEDIATA impossibile specificare codice di altra filiale
158030941103Codice non ammesso se cliente di altra filiale                                35
158040941103Codice cliente errato o inesistente
158050941103Codice cliente non utilizzabile: bloccato in Piano dei Conti
158060941103Se Segue Fattura impossibile indicare il codice 9999
158070011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 39
158080941209Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  40
158090941103Impossibile eseguire la tassazione: cliente non dell'area
158100941209Importo da Assicurare obbligatorio, come indicato nella tariffa del cliente   42
158110011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  43
158120011205Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 44
158130011205Valore merce dichiarato obbligatorio                                          45
158140941209Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 46
158150941209Codice cliente di altra filiale: F9 per forzare l'immissione
158160941209Sigla varia inesistente                                                       48
158170941209Varia "G" non ammessa                                                         49
158180941209Varia "G" obbligatoria                                                        50
158190941209Importo varia obbligatorio                                                    51
158200000510Spedizione D.P.D. : non ammesso un peso superiore a xxxxxx,x kg.              52
158210990702Non e' stata trovata la tariffa per la tassazione                             53
158220941212Provvigione a zero: non indicata la percentuale in tariffa                    54
158230941212Errore in tassazione                                                          55
158240011018Se eliminato importo C/A eliminare anche il Tipo Incasso e Particolar.        56
158250071025Partita IVA  obbligatoria per cliente ITALIA.  Enter per forzare              57
158260051129Nuovo volume da FATT. inferiore al volume parziale VDL: sara' sostituito      58
158270950420Non e' possibile modificare il tipo bolla: assegnato gia' incassato           59
158280941222Non e' possibile eliminare i dati della fattura se era un PREPAGATO           60
158290941223Non trovata spedizione                                                        61
158300941223Immesso codice cliente: l'indirizzo sara' ignorato                            62
158310950406Anagrafica mittente non completa in Piano dei Conti                           63
158320950406Manca la provincia in anagrafica mittente: inserirla!!!                       64
158330950407Aggiunto contrassegno: aggiungere anche varia G. Enter per forzatura          65
158340950407Eliminato contrassegno: eliminare anche varia G. Enter per forzatura          66
158350950522Partita IVA obbligatoria se bolla diretta all'estero con attraversam. dogana  67
158360950424Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic68
158370991012Azzerata q.tà da fattur.:azzerare anche gli importi di tassazione             69
158380950426Variato cliente o importo da assic.:azzerare q.tà da fattur. e tot.imponibile.70
158390991012Variato cliente o importo da assic.:azzerare q.tà da fattur. e tassazione     71
158400950426Azzerata q.tà da fattur.e tariffa a valore:azzerare anche il tot. imponibile  72
158410961204Impossibile inserire il C/Assegno in bolle di RESO
158420061024Rif. Mittente obbligatorio se destinatario estero con attraversamento dogana  74
158430970110Natura merce obbligatoria                                                     75
158440970826ATTENZIONE!LArrivo desunta, diversa da bolla ma non verra'variata.ENTER forza
158450970808ATTENZIONE! Inoltro modificato. Enter per continuare                          77
158460980309Impossibile inserire il C/Assegno in questa bolla legata                      78
158470011205Importo da Assicurare non ammesso                                             79
158480011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  80
158490991112Impossibile fare fattura immediata a cliente di altro network                 81
158500990127Impossibile inserire importo da assicurare:bolla su C.A.in Liquidaz.Transatt. 82
158510990419Aggiunto Imp.Assicur.: aggiungere anche varia R. ENTER per forzare            83
158520990419Eliminato Imp.Assicur.: eliminare anche varia R. ENTER per forzare            84
158530120525Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
158540990419Modificato Imp.Assicur: controllare varia R . ENTER per forzare               86
158550060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 87
158560990701Per i Prepagati, Numero fattura deve essere maggiore di XXXXXX                88
158570990702Prepagato: premere F14 per tassare o inserire tassazione fino all'imponibile  89
158580991006Per codice cliente 8888 o 9999 senza tariffa è obbligatoria la divisa xxx     90
158590011210Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              91
158600061107Aggiornamento effettuabile soltanto in PARTENZA su spedizioni in Prepagato
158610011010Ammessi importi con massimo __ decimali per la divisa indicata                93
158620991124Divisa obbligatoria se immesso almeno un importo di tassazione                94
158630061107Aggiornamento effettuabile soltanto in ARRIVO su spedizioni Fattura Immediata 95
158640131008$$Bolla cliente POSTE: non si puo' trasformare in C/Servizio                    96
158650131008$$Bolla cliente POSTE: spese C/A sempre a carico del mittente                   97
158660010412Tipo inoltro "D"=Disagiato non ammesso per questo cap/località                98
158670070508Impostare tipo CONTATTO solo se immesso il Fermo Deposito                     99
158680011011Divisa obbligatoria                                                           00
158690011017Manca la varia 'N'                                                            01
158700011102Divisa scaduta                                                                02
158710011120Spedizione giacente: importo C/A non valido                                   03
158720011210Valore merce dichiarato INFERIORE a xxxxxxxxxx,xxx EUR                        04
158730011210Valore merce dichiarato SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR        05
158740011210Valore merce dichiarato > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE  06
158750011206Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       07
158760011210Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              08
158770011210Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        09
158780011206Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                10
158790020508Discordanza tra codice bolla e codice cliente mittente
158800020531Destinatario               : immettere la relativa consegna particolare       12
158810051116Non è possibile indicare contemporaneamente cons.particolari 'S' e 'X'        13
158820021121Immettere codice tariffa della relativa cartello FedEx                        14
158830021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            15
158840021121ATTENZIONE! Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE    16
158850021129Immettere consegna particolare "S"                                            17
158860030124Immettere il nominativo                                                       18
158870021129Immettere il n. di telefono della persona contattata                          19
158880030124Immettere il motivo                                                           20
158890051129Peso rilevato totalmente da VDL.  Si può inserire solo il peso = a peso VDL   21
158900030123Almeno una delle consegne particolari deve essere "S"                         22
158910051129Impossibile inserire un peso inferiore a quello parzialmente rilevato da VDL  23
158920030124Il n. dei bancali non ritirati non può superare il n. dei bancali bollettati  24
158930030124Immettere il n. dei bancali non ritirati                                      25
158940031001Non è possibile modificare la consegna richiesta                              26
158950040123Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "  27
158960040330La data consegna richiesta deve essere inferiore alla eseguibilità disp.giac. 28
158970040330Data consegna richiesta superiore a ___ giorni                                29
158980131008Codice cliente o linea cliente non utilizzabile                               30
158990041015Prevista consegna ad Uffici:consegna per Appuntam. o a Supermerc. non ammessa 31
159000110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       32
159010050414Impossibile trasformare prepagato in C/S                                      33
159020061116Modificata ragione sociale DESTINATARIO: controllare correttezza Cod.FIScale  34
159030050603Non ammessi inserimenti di importo da assicurare: bolla non coperta da mandato35
159040141110Variazione non effettuabile:salvataggio in corso. Riprovare (File "        ") 36
159050051220Non è possibile cambiare il mittente da DPD a non DPD e viceversa             37
159060060131Utente non abilitato ad inserire la consegna particolare " "                  38
159070070312Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta:ENTERavanza
159080070308Data consegna richiesta OBBLIGATORIA insieme al Tipo e/o all'Ora
159090070312Consegna Richiesta non modificabile: giacenza aperta o sped.con blocco "G"    41
159100070330Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta            42
159110070508Impostato Fermo Dep.DA CONTATTARE ma presente ConsegnaRich.in bolla:ENTR forza 43
159120070910Togliere Cons.Particolare X-xxxxxxxx:non immessa in partenza e non da Destinat44
159130071025Immettere codice fiscale o partita IVA o uscire con F12                       45
159140091124Inserimento/aumento ImpAssic x bolla non coperta da mandato:inviare MAIL.Forza46
159150091124Volume rilevato totalmente da VDL. Si può inserire solo il volume = al VDL    47
159160091124Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL48
159170120508P.Iva "$$" non ammessa per fatture emesse in filiale                          49
159180130930Indirizzo e-mail formalmente errato                                           50
159190131016Eseguita tassazione su bolla contabilizzata:non ammesso F6. F12 per uscire    51
159200131016Non effettuate variazioni: non ammesso F6=Conferma. Premere F12 per uscire    52
159210140408Il Fermo Deposito deve essere autorizzato dal Mittente                        53
159220141029Immessa Data Cons. Richiesta ma presente Fermo Deposito in bolla: ENTER forza 54
159230160204Presenti Dispo di Dirott./Reso ancora da elaborare: Fermo Deposito non ammesso55
159240160204Presente Dispo di Dirott./Reso ancora da elaborare: variazione non ammessa    56
159250141112Presente Giacenza aperta: Enter per Forzare                                   57
159260150210Consegna part. per Appuntamento non valida: immessa data consegna "Tassativa" 58
159270150413Spedizione per EXPO: non ammesso il Contrassegno                              59
159280150414Spedizione per EXPO possibile solo per bolle Franco                           60
159290150414Spedizione per EXPO non ammessa per la linea di arrivo                        61
159300151221Spedizione non manutenzionabile per questa Causale Variazione                 62
