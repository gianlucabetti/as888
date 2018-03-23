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
003400990930     D DTAS          E DS
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
003520000623     D DS3IPT        E DS
003530000510     D DS3IDP        E DS
003540000510     D DS1A          E DS
003550930506     D DS1P          E DS
003560060131     D DSSP          E DS
003570941209     D DS15          E DS
003580920117     D DS3A          E DS
003590020715     d dstb          e ds
003600000202     D OG143         E DS
003610070319     D OG146         E DS
003620070306     d Og148         e ds
003630070306     d Og150         e ds
003640000202     D DGEI          E DS
003650991006     D DGED          E DS
003660991028     D DSTA01        E DS
003670930210     D DS7T          E DS
003680021121     d ddfe          e ds
003690030710     d dvpo          e ds
003700071031     d dvpoDECO      e ds
003710050601     d dta4c         e ds
003720151019     D Dar5fat       E DS
003730090604     d ds5e          e ds
003740110906     d FNLVSTDS      e ds
003750130930     d dsemail       e ds
003760130927     d
003770130927     d darbn_ref     e ds
003780130927     d darbn_eml     e ds
003790130930     d darbn_sms     e ds
003800130930     d darbn_not     e ds
003810061107     d
003820151105     d xcfiva1ds     e ds
003830071025     d  ivaeu                  3      4
003840030612      * DS PER YEURCO - EURO CONVERTITORE
003850030612     D YeurcoDS      E DS
003860030612      * DS PER TRUL90R - reperimento stampanti e pgm di stampa
003870130322     D TRUL90DS      E DS
003880030612      * DS PER FNLV13R - CONTROLLO DI UN CAP
003890030612     D Fnlv13DS      E DS
003900030612      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
003910030612     D Fnlv14DS      E DS
003920030612      * DS PER TISI95R - CONTROLLO DI CAP
003930030612     D Tisi95DS      E DS
003940030612      * DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT E GEST PARTI
003950030612     D DS7pqrs       E DS
003960030612      * DS PER TIBS02R - RICERCA TABELLA SU TNTBE
003970030612     D Tibs02DS      E DS
003980030626      * DS PER STAMPA LDV
003990130321     D*FNLSB5DS      E DS
004000030612      * DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
004010030612     D FNLV04DS      E DS
004020050202      * DS PER FNLV59R - CARICAMENTO E CONTROLLO CODICI TARIFFA
004030050202     D Fnlv59DS      E DS
004040041008      * DS PER FNLV57R - PER aggiornamento files padroncini
004050041008     D FNLV57ds      E DS
004060030612      * DS PER FNLV16R - CONTROLLO TASSAZIONE
004070030612     D Fnlv16DS      E DS
004080040122     d dlv1601       e ds
004090030612      * DS PER FNLV18R - CALCOLO VOLUME AUTOMATICO
004100030612     D Fnlv18DS      E DS
004110060223      * DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
004120030612     D Fnlv19DS      E DS
004130030612      * DS PER FNLV20R - CALCOLA E SISTENA I VOLUMI SU BOLLA
004140030612     D Fnlv20DS      E DS
004150030612      * DS PER FNLV55R - REPERISCE TERMINAL DI PARTENZA E ARRIVO
004160030612     D Fnlv55DS      E DS
004170050511      * DS PER FNLRa2R - TASSAZIONE PORTI ASSEGNATI
004180050511     D Fnlra2DS      E DS
004190030612      * DS PER FNLR48R - DATI DI ENTRATA
004200030612     D Fnlr48DS      E DS
004210030612      * DS PER FNLR66R - CONTROLLO CAUSALE VARIAZIONE
004220030612     D Fnlr66DS      E DS
004230140130      * DS PER FNLRN6R - note LDV
004240140130     D FnlrN6DS      E DS
004250130321      * DS PER FIDG42R -
004260130321     D Fidg42ds      E DS
004270141029
004280141029     D fnlry09ds2    E DS
004290150603     d
004300150603     D tnsd99ds      E DS
004310070329     d
004320120413     D TIBS69DS      E DS
004330120413     D ds_cnaco      E DS                  extname(CNACO00F) prefix(BS_)
004340120413     D ds_cnind      E DS                  extname(CNIND00F) prefix(BS_)
004350120413     D ds_cnclp      E DS                  extname(CNCLP00F) prefix(BS_)
004360120413     D ds_fncls      E DS                  extname(FNCLS00F) prefix(BS_)
004370120413
004380070329      * Dati utente
004390060131     d Azuteds       e ds                  extname(Azute00f)
004400060131     d dDatiute      e ds
004410060131     d dute01        e ds
004420060131     D TIBS34DS      E DS
004430060213      * ds x richiamo pgm FNLv85R
004440060213     d Fnlv85ds      e ds
004450060705      * ds x controllo tipo incasso e divisa C/A
004460060705     d TRULTICDS     e ds
004470140515
004480140515     d TRULINTDS     e ds
004490980722      *
004500030612      * DS PER XEDIT - PGM EDITAZIONE
004510011009     D DSEDIT        E DS                  EXTNAME(XEDITDS)
004520011009     D  IEDNUM       E                     EXTFLD(IEDNUMERO)
004530011009     D  IEDNRD       E                     EXTFLD(IEDNRDEC)
004540011009     D  IEDEDT       E                     EXTFLD(IEDEDTCOD)
004550011009     D  IEDLEN       E                     EXTFLD(IEDLENFLD)
004560011009     D  OEDNUM       E                     EXTFLD(OEDNUMAN)
004570011009     D  OEDFLG       E                     EXTFLD(OEDFLGERR)
004580011009     D  OEDCOD       E                     EXTFLD(OEDCODERR)
004590941212      *
004600910429     D KPJBA         E DS
004610930210     D  LIB                   92    101
004620060131     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004630030612     D  DTU                  848    895P 0 DIM(12)    PACKEVEN                  DATE PARAM
004640011205
004650020327     D DSFTD         E DS                  EXTNAME(FIFTD00F)
004660011205     d trul21ds      e ds
004670011205     d trul22ds      e ds
004680011205     d trul23ds      e ds
004690160225     D* DS PER TRUL27R1 - network bolla
004700160225     D trul27ds1     E DS
004710160225     D
004720160225     D trulC7ds      E DS
004730020531     D tisit0ds      E DS
004740131227     D fnlva1ds      E DS
004750041007     d*Trul42Ds      e ds
004760140212     d Trtcp2ds      e ds
004770141021     D TRUL33DS      E DS
004780021129     d Dar5scr       e ds
004790030123     d dAr5Ban       e ds
004800030729     d dAr5Bnb       e ds
004810130924     d dAr5Emd       e ds                  prefix(emd_)
004820031120     d dAr5          e ds
004830031120     d dAr5Gen       e ds
004840040219     d  §Ar5gTcr     e                     Extfld(§Ar5Tcr)
004850040219     d  §Ar5gDcr     e                     Extfld(§Ar5Dcr)
004860040219     d  §Ar5gHcr     e                     Extfld(§Ar5Hcr)
004870040219     d  §Ar5gGc1     e                     Extfld(§Ar5Gc1)
004880040219     d  §Ar5gGc2     e                     Extfld(§Ar5Gc2)
004890040219     d  §Ar5gTc1     e                     Extfld(§Ar5Tc1)
004900040219     d  §Ar5gTc2     e                     Extfld(§Ar5Tc2)
004910070910     d dAr5GenPar    e ds                  extname(dar5gen) prefix(PAR:3)
004920070910     d  PAr5gTcr     e                     Extfld(§Ar5Tcr)
004930070910     d  PAr5gDcr     e                     Extfld(§Ar5Dcr)
004940070910     d  PAr5gHcr     e                     Extfld(§Ar5Hcr)
004950070910     d  PAr5gGc1     e                     Extfld(§Ar5Gc1)
004960070910     d  PAr5gGc2     e                     Extfld(§Ar5Gc2)
004970070910     d  PAr5gTc1     e                     Extfld(§Ar5Tc1)
004980070910     d  PAr5gTc2     e                     Extfld(§Ar5Tc2)
004990030123     d dTba          e ds
005000031028     d dsbl4a        e ds
005010040806
005020070309     d ds2Z          e ds
005030070309     d ds2g2         e ds
005040040806     d ds7q          e ds
005050140408     d ds7r          e ds
005060051109     d dgcpflr       e ds
005070060111     d ddstflo       e ds
005080040809
005090040809      * ds per il controllo della presenza di CA per la spedizione richiesta
005100080701     d****** FIDN12DS      e ds
005110080701     d***  skKey                 26    305    dim(20)
005120080701     d****  skAnn                306    325    dim(20)
005130080701     d****  skDca                326    485  0 dim(20)
005140080701     d****  skFca                486    545  0 dim(20)
005150080701     d****  skDch                546    705  0 dim(20)
005160080701     d****  skCch                706    745    dim(20)
005170040809     d*
005180040809     d dsKey           ds
005190040809     d  dsaac                         4  0
005200040809     d  dsfil                         3  0
005210040809     d  dsnca                         7  0
005220050531     D PARAMT          DS
005230050531     D  PARCA                256    256
005240011205
005250950104     D*
005260950104     D* DEFINIZIONE COSTANTI
005270950104     D*
005280950104     D CPDC            C                   CONST('su Piano dei Conti')
005290950114     D CNOTAR          C                   CONST('NO TARIFFE')
005300061107     D Cdest           C                   CONST('D E S T I N A T A R I O')
005310061107     D CMITT           C                   CONST('--- M I T T E N T E ---')
005320141022     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
005330141022     D RIMM            C                   CONST(X'21')
005340011205
005350011205     d                sds
005360011205     d  nompgm                 1     10
005370030620
005380030620     d sav_indsin      s                   Like(indsin)
005390041008     d waltrocpi       s                   Like(arbcpi)
005400060223     d wcpifile        s                   Like(arbcpi)
005410041008     d waltrofvf       s                   Like(arbfvf)
005420041008     d waltrovlf       s                   Like(arbvlf)
005430041022     d waltrotc1       s                   Like(arbtc1)
005440041022     d waltrotc2       s                   Like(arbtc2)
005450080422     d waltroft3       s                   Like(arbft3)
005460041027     d newcpi          s                   Like(arbcpi)
005470051114     d wtc1            s                   Like(arbtc1)
005480051114     d wtc2            s                   Like(arbtc2)
005490051220     d memntw          s                   Like(§ogntw)
005500060111     d d_o95lna        s                   like(o95lna)
005510060316     d s_arbrsd        s                   like(arbrsd)
005520060316     d s_arbrd2        s                   like(arbrd2)
005530061107     d wcliie          s              1
005540061107     d wprv            s                   like(arbprd)
005550151105     d wcap            s                   like(arbcad)
005560151105     d wloc            s                   like(arblod)
005570061107     d Esiste          s              1
005580091217     d Wtipoagg        s              3
005590110906     d Lnacons         s              3  0
005600061107     d wnar            s                   Like(arbnzd)
005610130411     d wpda_ndc        s                   Like(arbndc)
005620130411     d wpda_ddc        s                   Like(arbddc)
005630130411     d wpda_dcm        s                   Like(arbdcm)
005640131016     d winf14          s              1
005650131120
005660131120     d sav_ift         s                   Like(ar6ift)
005670140403     d comgf2          s                   Like(o95gf2)
005680140407     d wpk2            s                   Like(§AR5PK2)
005690141020     d wdir            s              1
005700141021     d $Finenum        s               n   inz(*off)
005710141021     d wfgslna         s                   like(d55tfa)
005720141021     D Esito           s              1a
005730141031     d w0090           s              9  0
005740141021     d kprg            s                   like(idcprg)
005750141021     d winf12          s              1
005760141022     d annocur         s              4s 0
005770141029     d w0140i          s             14s 0 inz
005780141203     d savzng          s                   like(vidzng)
005790141212     d wnoindir        s              1
005800150119     d w_noapp         s              1
005810151016     d PesVolpkcn      s                   Like(Taspkcn)
005820151016     d PesVolSpc       s                   Like(TasSpc)
005830151016     d PesVolIMV       s                   Like(Ar6IMV)
005840151019     d Tassatoimv      s                   Like(Ar6IMV)
005850160112     d wfile           s              1
005860141021
005870141021     D trul33R         pr                  extpgm('TRUL33R')
005880141021     D  kpjba                              likeds(kpjba)
005890070515     i*
005900070515     IFNBLP000
005910070515     I              BLPFST                      ARBFSTDDT
005920910430     C****************************************************************
005930910430     C*  RIEPILOGO INDICATORI
005940910430     C***************************************************************
005950920128     C* 01    - VARIAZIONE DESTINATARIO
005960941209     C* 02    - VARIAZIONE PESO / VOLUME
005970930510     C* 03    - PROGRAMMA RICHIAMATO  DSARBG
005980930505     C* 04    - SPEDIZIONE CON GIACENZA APERTA
005990941222     C* 05    - INDICA SE LA 1 BOLLA O LA 2 E' UN ASSEGNATO X P.IVA
006000941222     C* 06    - 1 BOLLA IN FRANCO
006010991006     C* 07    - PROTEZIONE DIVISA PER PREPAGATI E FATTURE IMMEDIATE
006020991012     C* 08    - PREPAGATO
006030941213     C* 09    - SONO STATE MESSE DELLE VARIE NELLA TASSAZIONE
006040941222     C* 10    - ON -BOLLE IN ARRIVO OFF-BOLLE IN PARTENZA
006050941102     C* 11/12 - PROTEGGO LE CONSEGNE PARTICOLARI SE IMMESSE IN PART E
006060941102     C*         NON UTILIZZABILI IN ARRIVO
006070941102     C* 13    - VARIAZIONE TASSAZIONE S.F.
006080911210     C* 14    - VARIAZIONE TASSAZIONE FATTURA
006090941102     C* 15    - RITASSAZIONE
006100950120     C* 16    - PER CAUSALE CP PROTEGGO CAMPI DELLA CONSEGNA RICH/GG CH
006110940614     C* 17 OFF- FILE TASSAZIONE PADRONCINI
006120940614     C* 17 ON - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
006130911211     C* 18    - NON ESISTONO TARIFFE PER IL CODICE DIGITATO
006140920128     C* 19    - PROGRAMMA RICHIAMATO  DSAR2T
006150920128     C* 20    - PROGRAMMA RICHIAMATO  GENERICO
006160990701     C* 21    - CAUSALE VARIAZIONE MODIFICA CONTRASSEGNO
006170990701     C* 22    - UATO PER PROTEZIONE MITTENTE
006180990701     C* 23    - DATI FATTURA NON MODIFICABILI PER PREPAGATO AUTOMAT
006190950113     C* 24    - ESEGUITA LA VARIAZIONE DEL MITTENTE
006200920128     C* 25    - PROGRAMMA RICHIAMATO  DSARBT
006210950607     C* 26    - ABILITA L'ENTER PER L'AGGIORNAMENTO
006220950607     C* 27    - ABILITA L'F12 PER RITORNO AL PGM CHIAMANTE
006230941212     C* 28    - EMETTO VIDMSG
006240950616     C* 29    - PROTEZIONE CAMPI DEL VIDEO
006250980513     C* 30-33 - COMODO
006260920211     C* 34    - PROGRAMMA RICHIAMATO  DSARBD
006270920129     C* 35    - PROGRAMMA RICHIAMATO  DSARBK
006280920129     C* 36    - VARIA TASSAZIONE FRANCHI: SALTO I CONTROLLI SUL COD.CLI
006290920204     C* 37    - VARIATO TOTALE IMPONIBILE: EFFETTUO RISTAMPA FATTURA
006300941014     C* 38    - CREO RECORD IN FNARBM0T
006310941014     C* 39    - CREO RECORD IN FNARBD0T-CAUS.K5 K8- PER AVERE DESTINAT
006320930507     C* 40-67 - ERRORE
006330030729      * 70    - Visualizzo Colli originali
006340991001     C* 71-72 - PROTEGGO CAMPI DESTINATARIO SE 9999 GIA' CONTABILIZZATO
006350990415     C* 73    - SPROTEGGO IL CAMPO DELL'IMPORTO D'ASSICURARE E DEL VALORE
006360990415     C*         MERCE DICHIARATO
006370060201     C* 74    - protezione campi tassazione
006380060201     C* 75    - protezione campi importo da assicurare
006390140213     C* 77    - condizionamento campo "fd da non contattare"
006400131001     C* 81    - della DSARBN  passato campo uni70 - note
006410990415     C* 82    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA IMP.ASSICURAZ.
006420131001     C* 83    - della DSARBN  passato campo cel
006430131001     C* 84    - della DSARBN  passato campo ref/tel
006440131001     C* 85    - SFPDSP
006450131001     C* 86    - della DSARBN  passato campo uni70 - email
006460071022     C* 87    - tasto funzionale"F17-Note ldv"attivo
006470070329     C* 88    - PROTEGGO PESO   SE NON VARIABILE
006480941216     C* 89    - PROTEGGO VOLUME SE NON VARIABILE
006490920220     C* 90    - ERRORE GENERICO
006500911213     C* 91    - ERRORE NON POSSIBILE ALLOCARE FILE DI INVIO
006510950407     C* 92    - ERRORE NON BLOCCANTE PER ELIMINAZIONE O AGGIUNTA C/ASS.
006520941215     C* 93    - INDICATORE DI CHANGE SU VALORE MERCE DICH
006530941215     C* 94    - RI SU VALUTA VAL.MERCE SE PRESA IN AUTOMATICO DA NAZ
006540941102     C* 95    - INDICATORE DI CHANGE SU C/ASSEGNO
006550941215     C* 96    - RI SU VALUTA C/ASSEGNO SE PRESA IN AUTOMATICO DA NAZ
006560941215     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
006570941215     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
006580910430     C*****************************************************************
006590000000     C     *ENTRY        PLIST
006600000000     C                   PARM                    KPJBA
006610920211     C                   PARM                    DSARBD
006620920128     C                   PARM                    DSARBK
006630990930     C                   PARM                    DARBT
006640930510     C                   PARM                    DSARBG
006650030612     C                   PARM                    TRUL90DS
006660130930     C                   PARM                    DSARBn
006670050601     c*
006680050110     c*
006690050110     c                   setoff                                       kl
006700030612      *
006710030612      * Il numero dei parametri ricevuti è testato, in caso di modifica
006720030612      *  ricercare la stringa "%PARMS"
006730030612      *
006740030612     C                   MOVEL     KPJBU         Fnlr48DS
006750120426     C*
006760120426     C                   EXSR      OPN_TITA4
006770950104     C* ESEGUO LA VARIAZIONE
006780950104     C                   MOVEL     'E'           WESEG             1
006790020603     c                   movel     d48f12        w48f12
006800060301     c                   movel     d48mct        w48mct
006810950114     C* SE NON  RICHIAMATO IMPOSTO SEMPRE LA FERMATA NELLE VIDEATE
006820950114     C     D48TRC        IFEQ      *BLANKS
006830950609     C                   MOVEL     'V'           D48FFR
006840950114     C                   ENDIF
006850070907     c* skiere di riesecuzione automatica pgm: max 10 volte
006860070907     c                   clear                   kdati
006870070907     c                   clear                   kcvb
006880070907     c                   clear                   ktrc
006890070911     c                   clear                   kffr
006900070911     c                   clear                   kf12
006910070907     c                   clear                   kK                2 0
006920070907     c                   z-add     1             rr                2 0
006930941014     C*
006940910429     C*---------------------------------------------------------------*
006950041006     C* TIPO LANCIO, D48TLA "C" -           CHIUSO CON %R
006960941209     C* TIPO LANCIO, D48TLA "L" - ELABORO E CHIUDO CON LR
006970941209     C* TIPO LANCIO, D48TLA " " - ELABORO E CHIUDO IN RETRN
006980941209     C*
006990941209    1C     D48TLA        IFNE      'C'
007000001213     C*
007010001213    2C     D48FFR        IFNE      'E'
007020141017    2C     D48FFR        andne     'F'
007030001213    3C     OPN48         IFEQ      '0'
007040001213     C                   OPEN      FNLR48D
007050001213     C                   MOVE      '1'           OPN48
007060001213    3C                   ENDIF
007070001213   X2C                   ELSE
007080001213    3C     OPN48         IFEQ      '1'
007090001213     C                   CLOSE     FNLR48D
007100001213     C                   MOVE      '0'           OPN48
007110001213    3C                   ENDIF
007120001213    2C                   ENDIF
007130040224
007140040224     c                   Clear                   wCa
007150040324     c                   Clear                   wgiac
007160941209     C*
007170950104     C* FINTANTO CHE SI VUOLE SI PUO' RIMANRE DENTRO ALLE VARIAZIONI
007180950104   1AC     WESEG         DOWEQ     'E'
007190950104     C                   CLEAR                   WESEG
007200920123     C*
007210950104     C* CONTROLLI BOLLA CON CAUSALE VARIAZIONE
007220900509     C                   EXSR      CONTR
007230941014     C*
007240941014     C   90              GOTO      FINE
007250911209     C*
007260911210     C* CARICO BOLLA
007270120403     c                   if        flgcvr<>'S'
007280911210     C                   EXSR      CARICA
007290131002     c                   if        d48err='1'
007300131002     C                   GOTO      FINE
007310120403     c                   endif
007320131002     c                   endif
007330911209     C**
007340941019     C** VARIAZIONE: DESTINATARIO PESO VOLUME
007350911209     C**
007360941220     C                   SELECT
007370120330     c* Variazioni da manutenzione bolle di sede
007380120330     C     FLGCVR        WHENEQ    'S'
007390120427     c* Questa routine la posso eseguire solo se pgm richiamato
007400120427     c* Hanno fatto una manutenzione con CVB SD/SF da menù in quanto
007410120427     c* SD e SF non erano stati disabilitati per l'utilizzo da bolle par
007420120427     c* e bolle arrivi. In questo modo essendo azzerava peso,volume, mittente..
007430120427     c                   if        d48trc<>*blanks
007440120330     C                   EXSR      gesSD
007450120427     c                   endif
007460120330
007470941220     C     FLGCVR        WHENEQ    'I'
007480920120     C                   EXSR      EMFOR2
007490911210     C**
007500911211     C** VARIAZIONE TASSAZIONE 1° BOLLA
007510911210     C**
007520941220     C     FLGCVR        WHENEQ    'T'
007530920121     C                   EXSR      EMFOR3
007540911211     C**
007550911211     C** VARIAZIONE TASSAZIONE 2° BOLLA
007560911211     C**
007570941220     C     FLGCVR        WHENEQ    '2'
007580941102     C                   EXSR      EMFOR4
007590920116     C**
007600920116     C** VARIAZIONE: CODICE BOLLA CONTRASSEGNO
007610920116     C**
007620941220     C     FLGCVR        WHENEQ    'K'
007630941102     C                   EXSR      EMFOR5
007640920320     C**
007650920320     C** VARIAZIONE CONSEGNA RICHIESTA
007660920320     C**
007670941220     C     FLGCVR        WHENEQ    'C'
007680021129     c     D48Cvb        AndNe     'CS'
007690941102     C                   EXSR      EMFOR7
007700021129     C**
007710021129     C** VARIAZIONE CONSEGNA RICHIESTA SUPERMERCATI
007720021129     C**
007730021129     c     D48Cvb        WhenEq    'CS'
007740021129     c                   ExSr      Emfor71
007750941223     C**
007760941223     C** VARIAZIONE MITTENTE
007770941223     C**
007780941223     C     FLGCVR        WHENEQ    'M'
007790941223     C                   EXSR      EMFOR8
007800961211     C**
007810130927     C** VARIAZIONE DATI MITTENTE
007820961211     C**
007830961211     C     D48CVB        WHENEQ    'RM'
007840961211     C                   EXSR      EMFOR9
007850030123      **
007860030123      ** Variazione mancato ritiro BANCALI
007870030123      **
007880030123     c     D48Cvb        WhenEq    'BR'
007890030123     c                   ExSr      Emfor10
007900961211     C*
007910061107      **
007920061107      ** Variazione CODICE FISCALE
007930061107      **
007940061107     c     D48Cvb        WhenEq    'FI'
007950061107     c                   ExSr      Emfor11
007960130927      **
007970130927      ** Variazione dati destinatario SMS/mail/referente
007980130927      **
007990130927     C     FLGCVR        WHENEQ    'N'
008000130927     c                   ExSr      Emfor12
008010061107     C*
008020941220     C                   ENDSL
008030070907     c
008040070907     c* Verifico se ho delle riesecuzioni da fare
008050070907     c                   if        RR<=10 and kcvb(RR)<>*blanks
008060070907     c                   eval      weseg='E'
008070070907     c                   eval      d48cvb=kcvb(RR)
008080070907     c                   eval      d48trc=ktrc(RR)
008090070911     c                   eval      d48ffr=kffr(RR)
008100070911     c                   eval      w48f12=kf12(RR)
008110070907     c                   select
008120070907     c                   when      d48trc='D'
008130070907     c                   movel     kdati(RR)     dsarbd
008140070907     c                   when      d48trc='G'
008150070907     c                   movel     kdati(RR)     dsarbg
008160070907     c                   when      d48trc='K'
008170070907     c                   movel     kdati(RR)     dsarbK
008180070907     c                   when      d48trc='T'
008190070907     c                   movel     kdati(RR)     darbt
008200070907     c                   when      d48trc='2'
008210070907     c                   movel     kdati(RR)     darbt
008220070907     c                   ENDSL
008230070907     c
008240070907     c                   add       1             RR
008250070907     c                   endif
008260941209     C*
008270950104   1AC                   ENDDO
008280950104    1C                   ENDIF
008290920121     C*
008300000000     C     FINE          TAG
008310941209     C* CHIUSURA PGM
008320060302     C******             MOVEL     Fnlr48DS      KPJBU
008330950114     C*
008340950114     C* PULISCO IL CAMPO CHER AVEVO IMPOSTATO CON LA CONFERMA DEL MITT
008350950114     C*  CHE DEVE RIMANERE IMPOSTATO FINTANTO CHE SI E' DENTRO AL PGM
008360950114     C                   CLEAR                   WRICVL
008370950114     C                   CLEAR                   MITFVF
008380950114     C                   CLEAR                   MITVLF
008390950114     C                   CLEAR                   MITCTR
008400020722      * PULISCO IL CAMPO CHE AVEVO IMPOSTATO PER IL DESTINATARIO
008410020722      *  DISAGIATO
008420020722     C                   CLEAR                   Wdisag
008430060302     c*
008440060302     c                   Clear                   Fnlv85ds
008450060302     c                   Eval      Ilv85Tla = 'C'
008460060302     c                   movel     fnlv85ds      kpjbu
008470060302     c                   Call      'FNLV85R'
008480060302     c                   Parm                    kpjba
008490060302     c*
008500950114     C* CHIUSURA PGM
008510941216    1C     D48TLA        IFEQ      ' '
008520130417     C                   MOVEL     Fnlr48DS      KPJBU
008530941209     C                   RETURN
008540941209     C                   ELSE
008550941209     C*
008560930510     C* SE APERTI FILE CHIUDO
008570001213     C*
008580001213     C     OPN48         IFEQ      '1'
008590001213     C                   CLOSE     FNLR48D
008600001213     C                   ENDIF
008610001213     C*
008620030612     C                   CLEAR                   FNLV04DS
008630941209     C                   MOVEL     'C'           D04TLA
008640990930     C                   CALL      'FNLV04R'
008650030612     C                   PARM                    FNLV04DS
008660990930     C                   PARM                    DTASV
008670970808     C*
008680970808     C                   MOVEL     'C'           I13TLA
008690970808     C                   CALL      'FNLV13R'
008700970808     C                   PARM                    KPJBA
008710030612     C                   PARM                    Fnlv13DS
008720030612     C                   PARM                    Tisi95DS
008730030612     C                   CLEAR                   Tisi95DS
008740971105     C**
008750971105     C                   MOVEL     'C'           I95TLA
008760971105     C                   CALL      'TISI95R'
008770030612     C                   PARM                    Tisi95DS
008780970808     C*
008790970808     C                   MOVEL     'C'           I14TLA
008800970808     C                   CALL      'FNLV14R'
008810970808     C                   PARM                    KPJBA
008820030612     C                   PARM                    Fnlv14DS
008830971114     C*
008840030612     C                   CLEAR                   Fnlv55DS
008850971114     C                   MOVEL     'C'           D55TLA
008860971114     C                   CALL      'FNLV55R'
008870030612     C                   PARM                    Fnlv55DS
008880050202     c
008890050202     C                   CLEAR                   Fnlv59DS
008900050202     C                   MOVEL     'C'           ilv59tla
008910050202     C                   CALL      'FNLV59R'
008920050202     C                   PARM                    Fnlv59DS
008930020305     c*
008940160225     C                   CLEAR                   trul27ds1
008950020305     C                   MOVEL     'C'           I27TLA
008960160225     C                   CALL      'TRUL27R1'
008970160225     C                   PARM                    TRUL27DS1
008980160225     c
008990060705     C                   CLEAR                   trulTICds
009000060705     C                   MOVEL     'C'           IticTLA
009010060705     C                   CALL      'TRULTICR'
009020060705     C                   PARM                    TRULTICDS
009030060705     C                   PARM                    DS1A
009040140515
009050140515     C                   CLEAR                   trulintds
009060140515     C                   MOVEL     'C'           IintTLA
009070140515     C                   CALL      'TRULINTR'
009080140515     C                   PARM                    TRULintds
009090110906     c
009100110906     C                   CLEAR                   fnlvstds
009110110906     C                   MOVEL     'C'           ILVSTTLA
009120110906     C                   CALL      'FNLVSTR'
009130110906     c                   parm                    kpjba
009140110906     C                   PARM                    fnlvstds
009150941220     C*
009160120426     C                   IF        %OPEN(TITA430C)
009170050601     c                   close     tita430c
009180120426     C                   ENDIF
009190151016     C*
009200151016     C                   IF        %OPEN(FIAR531C)
009210151016     c                   close     fiar531c
009220151016     C                   ENDIF
009230151016     C*
009240151016     C                   IF        %OPEN(TITAS30C)
009250151016     c                   close     titas30c
009260151016     C                   ENDIF
009270130321
009280130411     c                   clear                   fidg42ds
009290130411     C                   MOVEL     'C'           co42tla
009300130411     c                   movel     fidg42ds      kpjbu
009310130411     c                   call      'FIDG42R'
009320130411     c                   parm                    kpjba
009330131227
009340131227     c                   clear                   fnlva1ds
009350131227     C                   MOVEL     'C'           ia1tla
009360131227     c                   movel     fnlva1ds      kpjbu
009370131227     c                   call      'FNLVA1R'
009380131227     c                   parm                    kpjba
009390141029
009400141029     c                   eval      ilry09tla= 'C'
009410141029     c                   eval      kpjbu=fnlry09ds2
009420141031     c                   call      'FNLRY09C'
009430141029     c                   parm                    kpjba
009440130321
009450130417     C                   MOVEL     Fnlr48DS      KPJBU
009460041006     C                   SETON                                        LR
009470941209     C                   END
009480941220     C*
009490941209     C* IMPOSTAZIONI INIZIALI -------------------------------------*
009500941209     C     *INZSR        BEGSR
009510941212     C* MI SALVO LA KPJBU
009520941212     C                   MOVEL     KPJBU         W256A           256
009530060131      *
009540060131     c     *dtaara       define    §azute        azuteds
009550060131     c     *dtaara       define    §datiute      ddatiute
009560060131     c                   in(E)     *dtaara
009570060131    1c                   If        %error  or RSUT = *blanks
009580060131     c                   Clear                   Tibs34ds
009590060131     c                   Call      'TIBS34R'
009600060131     c                   Parm                    Tibs34ds
009610060131     c                   In        *dtaara
009620060131    1c                   EndIf
009630060131
009640060131      * se ho errori nei dati utente esco dal pgm
009650060131w   1c                   if        DutErr = 'E'
009660060131     c                   GoTo      Fine
009670060131x   1c                   else
009680060131     c                   Movel     UteFaf        Dute01
009690060131e   1c                   endif
009700941212     C*
009710941209     C                   Z-ADD     1             CODUT
009720060131     C                   CALL      'X§PARUT'
009730060131     C                   PARM                    UT§DSE
009740060131     C                   MOVEL     rsut          VIDDSA
009750000104     C**
009760000104     C                   MOVEL     DTU(10)       W0020             2 0
009770000104     C                   Z-ADD     DTU(10)       WDTU10
009780000104     C     W0020         IFGE      60
009790000104     C                   MOVEL     19            WDTU10
009800000104     C                   ELSE
009810000104     C                   MOVEL     20            WDTU10            8 0
009820000104     C                   ENDIF
009830941014     C*
009840020805
009850020805     c                   eval      $simfel = simfel
009860020805     c                   movel     simfel        $mbr
009870070329     c
009880070329     c* Per EDP note sempre attive, se lna ha geo attiva
009890070329     c                   if        %subst(knmus:1:3)='EDP'
009900070329     c                   eval      dutgeot='S'
009910070329     c                   else
009920070329     c     dutpou        chain     azorg01l
009930070329     c                   if        %found(azorg01l)
009940070329     c                   movel     orgde8        og148
009950070329     c                   else
009960070329     c                   clear                   og148
009970070329     c                   endif
009980070329     c                   movel     §oggeot       dutgeot           1
009990070329     c                   endif
010000941020     C***
010010941020     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
010020941020     C***
010030941020     C                   MOVEL     'QT'          COD
010040941020     C                   MOVEL     '1       '    KEY
010050941212     C                   EXSR      CTABEL
010060941020     C  N30              MOVEL     TBLUNI        DSQT1
010070941020     C   30              CLEAR                   DSQT1
010080941214     C***
010090941214     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"Z"
010100941214     C***
010110941214     C                   MOVEL     '7T'          COD
010120941214     C                   MOVEL(P)  'Z'           KEY
010130941220     C                   EXSR      CTABEL
010140941214     C   30              CLEAR                   DS7T
010150941214     C  N30              MOVEL     TBLUNI        DS7T
010160941214     C*
010170970901     C* FLAG DI SOSTITUZIONE
010180941214     C                   MOVEL     §7TAFA        ZAFA              1
010190941214     C                   MOVEL     §7TAAA        ZAAA              1
010200941214     C                   MOVEL     §7TAFN        ZAFN              1
010210941214     C                   MOVEL     §7TAAN        ZAAN              1
010220970418     C***
010230970418     C* PRENDO DALLA TABELLA DEI VOLUMI IL FLAG DI SOSTITUZIONE PER"T"
010240970418     C***
010250970418     C                   MOVEL     '7T'          COD
010260970418     C                   MOVEL(P)  'T'           KEY
010270970418     C                   EXSR      CTABEL
010280970418     C   30              CLEAR                   DS7T
010290970418     C  N30              MOVEL     TBLUNI        DS7T
010300970418     C*
010310970901     C* FLAG DI SOSTITUZIONE
010320970418     C                   MOVEL     §7TAFA        TAFA              1
010330970418     C                   MOVEL     §7TAAA        TAAA              1
010340970418     C                   MOVEL     §7TAFN        TAFN              1
010350970418     C                   MOVEL     §7TAAN        TAAN              1
010360991001     C*
010370991001     C* CARICO DA SIGLE VARIE
010380991001     C                   MOVEL     '$2'          COD
010390991001     C                   MOVEL(P)  '1'           KEY
010400991001     C                   EXSR      CTABEL
010410991001     C  N30              MOVEL     TBLUNI        DS$2
010420991001     C   30              CLEAR                   DS$2
010430990930     C**
010440990930     C* CARICO VARIE CHE FANNO PARTE DEL TOTALE IMPONIBILE
010450990930     C**
010460990930     C                   Z-ADD     0             II                3 0
010470990930     C                   MOVEL     'CC'          COD
010480990930     C     KTAB1         CHAIN     TABEL                              30
010490990930    1C     *IN30         DOWEQ     *OFF
010500990930    2C     TBLFLG        IFEQ      ' '
010510990930     C                   MOVEL     TBLKEY        W005A             5
010520990930    3C     W005A         IFEQ      'VARIE'
010530990930     C                   MOVEL     TBLUNI        DSCC
010540990930     C* SIGLA VARIA
010550990930    4C     §CCFL6        IFEQ      'S'
010560990930     C                   ADD       1             II
010570990930     C                   MOVE      TBLKEY        SVA(II)
010580990930    4C                   ENDIF
010590990930    3C                   ENDIF
010600990930    2C                   ENDIF
010610990930     C**
010620990930     C     KTAB1         READE     TABEL                                  30
010630990930    1C                   ENDDO
010640991006     c*
010650991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
010660030612     C                   CLEAR                   Tibs02DS
010670991117     C                   CLEAR                   DGED
010680991117     C                   MOVEL     'C'           T02MOD
010690991117     C                   MOVEL     KNSIF         T02SIF
010700991117     C                   MOVEL     'GED'         T02COD
010710991117     C                   MOVEL     '1'           T02KE1
010720991117     C                   CALL      'TIBS02R'
010730991117     C                   PARM                    KPJBA
010740030612     C                   PARM                    Tibs02DS
010750991117     C     T02ERR        IFEQ      *BLANKS
010760991117     C                   MOVEL     T02UNI        DGED
010770991117     C                   ENDIF
010780011009     C* REPERIMENTO importi DELLA MONETA DI CONTO
010790030612     C                   CLEAR                   Tibs02DS
010800011009     C                   CLEAR                   DGEI
010810011009     C                   MOVEL     'C'           T02MOD
010820011009     C                   MOVEL     KNSIF         T02SIF
010830011009     C                   MOVEL     'GEI'         T02COD
010840011009     C                   MOVEL     §GEDCN        T02KE1
010850011009     C                   CALL      'TIBS02R'
010860011009     C                   PARM                    KPJBA
010870030612     C                   PARM                    Tibs02DS
010880011009     C     T02ERR        IFEQ      *BLANKS
010890011009     C                   MOVEL     T02UNI        DGEI
010900011120     C                   Z-ADD     §GEDFC        DFCSAV
010910011220     C                   Z-ADD     §geimf        imfsav
010920011009     C                   ENDIF
010930011009      * Tabella CV della moneta di conto
010940011009     C                   CLEAR                   DSCV
010950011009     C                   MOVEL     'CV'          COD
010960011009     C                   MOVEL(P)  §GEDCN        KEY
010970011009     C                   EXSR      CTABEL
010980011009     C     *IN30         IFEQ      *OFF
010990011009     C                   MOVEL     TBLUNI        DSCV
011000011009     C                   MOVEL     §CVDEC        DECSAV
011010011009     C                   ENDIF
011020000510     C***
011030000510     C* CARIO TABELLA VARIABILI DPD E POSTE
011040000510     C***
011050000510     C                   MOVEL     '3I'          COD
011060000510     C                   MOVEL(P)  'POSTE'       KEY
011070000510     C                   EXSR      CTABEL
011080000510     C  N30              MOVEL     TBLUNI        DS3IPT
011090000510     C   30              CLEAR                   DS3IPT
011100000510     C*
011110000510     C                   MOVEL     '3I'          COD
011120000510     C                   MOVEL(P)  'DPD'         KEY
011130000510     C                   EXSR      CTABEL
011140000510     C  N30              MOVEL     TBLUNI        DS3IDP
011150000510     C   30              CLEAR                   DS3IDP
011160020715
011170020715      * carico sk con tipi bolla che prevedono l'assicurazione
011180020715     c                   eval      cod = 'TB'
011190020715     c                   clear                   key
011200020715     c                   clear                   xx
011210020715     c     ktab1         setll     tabel00f
011220020715     c                   do        *hival
011230020715     c     ktab1         reade     tabel00f
011240020715      * fine file
011250020715     c                   if        %eof(tabel00f)
011260020715     c                   leave
011270020715     c                   endif
011280020715      * record annullato
011290020715     c                   if        tblflg <> *blanks
011300020715     c                   iter
011310020715     c                   endif
011320020715     c                   movel     tbluni        dstb
011330020715     c                   if        §tbfas = '1'
011340020715     c                   add       1             xx
011350020715     c                   eval      tbl(xx) = tblkey
011360020715     c                   endif
011370020715     c                   enddo
011380070309
011390070309      * Carico sk con eventi che prevedono modifica della cons.rich
011400070309     c                   eval      cod = '2Z'
011410070309     c                   clear                   key
011420070309     c                   clear                   xx
011430090526     c                   clear                   yy
011440070309     c     ktab1         setll     tabel00f
011450070309     c                   do        *hival
011460070309     c     ktab1         reade     tabel00f
011470070309      * fine file
011480070309     c                   if        %eof(tabel00f)
011490070309     c                   leave
011500070309     c                   endif
011510070309      * record annullato
011520070309     c                   if        tblflg <> *blanks
011530070309     c                   iter
011540070309     c                   endif
011550070309     c                   movel     tbluni        ds2Z
011560090526     c* x pgm non richiamato--> controllo solo gli S-sempre
011570090526     c                   if        §2ZADR='S'
011580070309     c                   add       1             xx
011590090526     c                   eval      ADRs(xx) = tblkey
011600070309     c                   endif
011610090526     c*
011620090526     c* x pgm     richiamato--> controllo sia S-sempre  che
011630090526     c*                         C-richiamati
011640090526     c                   if        §2ZADR<>' '
011650090526     c                   add       1             yy
011660090526     c                   eval      ADRc(yy) = tblkey
011670090526     c                   endif
011680070309     c                   enddo
011690021121
011700021121      * Aggancio tabella default tariffe fedex
011710030612     c                   Clear                   Tibs02DS
011720021121     c                   Movel     'C'           T02Mod
011730021121     c                   Movel     KNSIF         T02Sif
011740021121     c                   Movel     'DFE'         T02Cod
011750021121     c                   Movel     'FED'         T02Ke1
011760021121     c                   Call      'TIBS02R'
011770021121     c                   Parm                    Kpjba
011780030612     c                   Parm                    Tibs02DS
011790021121     c                   If        T02Err = *Blanks
011800021121     c                   Movel     T02Uni        DDfe
011810021121     c                   Else
011820021121     c                   Clear                   DDfe
011830021121     c                   EndIf
011840030710
011850060131      * Aggancio tabella VPO recpord VPO
011860030710     c                   Clear                   Dvpo
011870030710     c                   Clear                   Tibs02ds
011880030710     c                   Eval      T02Mod = 'C'
011890030710     c                   Eval      T02Sif = knsif
011900030710     c                   Eval      T02Cod = 'VPO'
011910030710     c                   Movel(p)  'VPO'         T02Ke1
011920030710     c                   Call      'TIBS02R'
011930030710     c                   Parm                    Kpjba
011940030710     c                   Parm                    Tibs02ds
011950030710     c                   If        T02Err = *Blanks
011960030710     c                   Movel     T02Uni        Dvpo
011970030710     c                   EndIf
011980060131
011990060131      * Aggancio tabella VPO record DECO
012000071031     c                   Clear                   Dvpodeco
012010071031     c                   Clear                   Tibs02ds
012020071031     c                   Eval      T02Mod = 'C'
012030071031     c                   Eval      T02Sif = knsif
012040071031     c                   Eval      T02Cod = 'VPO'
012050071031     c                   Movel(p)  'DECO'        T02Ke1
012060071031     c                   Call      'TIBS02R'
012070071031     c                   Parm                    Kpjba
012080071031     c                   Parm                    Tibs02ds
012090071031     c                   If        T02Err = *Blanks
012100071031     c                   Movel     T02Uni        Dvpodeco
012110071031     c                   EndIf
012120140130
012130140130      * Aggancio tabella VPO record DECOFI816TEL
012140140130     c                   Clear                   Tibs02ds
012150140130     c                   Clear                   fil816tel
012160140130     c                   Eval      T02Mod = 'C'
012170140130     c                   Eval      T02Sif = knsif
012180140130     c                   Eval      T02Cod = 'VPO'
012190140130     c                   Movel(p)  'DECOFI816TEL'T02Ke1
012200140130     c                   Call      'TIBS02R'
012210140130     c                   Parm                    Kpjba
012220140130     c                   Parm                    Tibs02ds
012230140130     c                   If        T02Err = *Blanks
012240140130     c                   Movel     T02Uni        fil816tel
012250140130     c                   EndIf
012260150414
012270150414      * Aggancio tabella "VPO" "EXPO"
012280151119     C*                  CLEAR                   tibs02ds
012290151119     c*                  clear                   fil_expo
012300151119     C*                  MOVEL     'C'           T02MOD
012310151119     C*                  MOVEL     KNSIF         T02SIF
012320151119     C*                  MOVEL     'VPO'         T02COD
012330151119     c*                  movel(P)  'EXPO'        t02ke1
012340151119     C*                  CALL      'TIBS02R'
012350151119     C*                  PARM                    KPJBA
012360151119     C*                  PARM                    tibs02ds
012370151119    2C*                  IF        T02ERR = *BLANKS
012380151119     C*                  MOVEa     T02uni        fil_expo
012390151119    2C*                  ENDIF
012400141029
012410141029      * Aggancio tabella VPO record DECOFI816DIR
012420141029     c                   clear                   trulvpods
012430141029     c                   eval      ivpoke1 = 'DECOFI816DIR'
012440141029     c                   call      'TRULVPOR'
012450141029     c                   parm                    trulvpods
012460141029     c                   eval      skfilokdir=skfilok
012470030710
012480030710      * Aggancio tabella MIA
012490030710     c                   Clear                   miapsw
012500030710     c                   Clear                   Tibs02ds
012510030710     c                   Eval      T02Mod = 'C'
012520030710     c                   Eval      T02Sif = knsif
012530030710     c                   Eval      T02Cod = 'MIA'
012540030710     c                   Movel(p)  'TUTTI'       T02Ke1
012550030710     c                   Call      'TIBS02R'
012560030710     c                   Parm                    Kpjba
012570030710     c                   Parm                    Tibs02ds
012580030710     c                   If        T02Err = *Blanks
012590030710     c                   Movel     T02Uni        miapsw
012600030710     c                   EndIf
012610031120
012620031120      * Aggancio tabella AR5 per record 'GEN'
012630031120     c                   Clear                   dAr5
012640031120     c                   Clear                   Tibs02ds
012650031120     c                   Eval      T02Mod = 'C'
012660031120     c                   Eval      T02Sif = knsif
012670031120     c                   Eval      T02Cod = 'AR5'
012680031120     c                   Movel(p)  'GEN'         T02Ke1
012690031120     c                   Call      'TIBS02R'
012700031120     c                   Parm                    Kpjba
012710031120     c                   Parm                    Tibs02ds
012720031120     c                   If        T02Err = *Blanks
012730031120     c                   Movel     T02Uni        dAr5
012740031120     c                   EndIf
012750040806      * Tabella 2G
012760040806     c                   Movel     '2G'          cod
012770040902     c                   Movel(p)  '2'           key
012780040806     c                   ExSr      ctabel
012790040902     c  n30              Movel     TblUni        ds2g2
012800040902     c   30              Clear                   ds2g2
012810050901      *
012820941222     C***
012830931119     C* DEFINIZIONE CAMPI
012840931119     C***
012850130411     C**** *LIKE         DEFINE    D66BDI        §7LFIN
012860991112     C     *LIKE         DEFINE    D66FFI        §7LFFI
012870991112     C     *LIKE         DEFINE    DATEU         WDATA
012880011010     C     *LIKE         DEFINE    §CVDEC        COMDEC
012890990702     C     *LIKE         DEFINE    AR6IMV        SAVIMV
012900151016     C     *LIKE         DEFINE    AR6IMV        SAVIMV_fat
012910990702     C     *LIKE         DEFINE    FVVNPG        KNPG
012920970423     C     *LIKE         DEFINE    SOVR(1)       SOARBT
012930941213     C     *LIKE         DEFINE    SOVR(1)       SAARBT
012940941213     C     *LIKE         DEFINE    SOVR(1)       SDARBT
012950941213     C     *LIKE         DEFINE    AR6SV1        WSV1
012960941212     C     *LIKE         DEFINE    AR6VA1        WVA1
012970941212     C     *LIKE         DEFINE    TAMPPA        WPPA
012980941212     C     *LIKE         DEFINE    TAMMPA        WMPA
012990050202     C     *LIKE         DEFINE    ilv59FIE      WFIE
013000941209     C     *LIKE         DEFINE    §15EFT        DESEFT
013010941209     C     *LIKE         DEFINE    TBLKUT        §KUT
013020911211     C     *LIKE         DEFINE    TBLCOD        §COD
013030911211     C     *LIKE         DEFINE    TBLKEY        §KEY
013040941212     C     *LIKE         DEFINE    ARBIAS        VARIAS
013050950426     C     *LIKE         DEFINE    ARBIAS        COMIAS
013060930507     C     *LIKE         DEFINE    ARBCTR        COMCTR
013070941220     C     *LIKE         DEFINE    ARBKSC        KSC2
013080950104     C     *LIKE         DEFINE    ARBFAP        COMFAP
013090950407     C     *LIKE         DEFINE    NEWCBO        SAVCBO
013100950419     C     *LIKE         DEFINE    VIDPID        WPIVA
013110011009      *
013120011120     C     *LIKE         DEFINE    §GEDFC        DFCSAV
013130011220     C     *LIKE         DEFINE    §GEimf        imfsav
013140011009     C     *LIKE         DEFINE    §CVDEC        DECSAV
013150011009      *
013160941220     C* PER I PADRONCINI
013170950113     C     *LIKE         DEFINE    ARBKSC        WKSC
013180950113     C     *LIKE         DEFINE    ARBRSM        WRSM
013190950113     C     *LIKE         DEFINE    ARBRSD        OLDRSD
013200150206     C     *LIKE         DEFINE    ARBlod        OLDlod
013210950113     C     *LIKE         DEFINE    ARBCAD        OLDCAD
013220950113     C     *LIKE         DEFINE    ARBFIN        OLDFIN
013230941209     C     *LIKE         DEFINE    ARBVLF        OLDVLF
013240950114     C     *LIKE         DEFINE    ARBFVF        OLDFVF
013250941209     C     *LIKE         DEFINE    ARBPKF        OLDPKF
013260950113     C     *LIKE         DEFINE    ARBRSM        OLDRSM
013270950113     C     *LIKE         DEFINE    ARBCAM        OLDCAM
013280950113     C     *LIKE         DEFINE    ARBKSC        OLDKSC
013290941220     C*
013300950113     C     *LIKE         DEFINE    ARBVLF        WVOL
013310950113     C     *LIKE         DEFINE    ARBVLF        WSOTV
013320941212     C     *LIKE         DEFINE    ARBVLF        WADDV
013330941215     C     *LIKE         DEFINE    ARBVLF        VARVLF
013340941215     C     *LIKE         DEFINE    ARBPKF        VARPKF
013350950113     C     *LIKE         DEFINE    ARBPKF        WPKG
013360941220     C* PER FAR RIDIGITARE L'IMPORTO
013370941215     C     *LIKE         DEFINE    ARBVAS        VARVAS
013380941215     C     *LIKE         DEFINE    ARBVAD        VARVAD
013390941213     C     *LIKE         DEFINE    AR9VCA        VARVCA
013400950918     C* PER CONTROLLARE SE E' MODIFICATO
013410950918     C     *LIKE         DEFINE    AR9VCA        SAVVCA
013420950918     C     *LIKE         DEFINE    AR9TIC        SAVTIC
013430950918     C     *LIKE         DEFINE    AR9CAS        SAVCAS
013440970808     C     *LIKE         DEFINE    VIDCAD        SAVCAD
013450970808     C     *LIKE         DEFINE    VIDCAD        WCAD
013460970808     C     *LIKE         DEFINE    VIDCAD        WCAD1
013470970808     C     *LIKE         DEFINE    VIDLOD        SAVLOD
013480970808     C     *LIKE         DEFINE    VIDLOD        WLOD
013490970808     C     *LIKE         DEFINE    VIDLOD        WLOD1
013500060111     c     *LIKE         DEFINE    VIDrsd        Wrsd1
013510060111     c     *LIKE         DEFINE    VIDrsd        savrsd
013520061116     c     *LIKE         DEFINE    VIDrsd        savrsdQ
013530060111     c     *LIKE         DEFINE    VIDind        wind1
013540060111     c     *LIKE         DEFINE    VIDind        savind
013550060111     c     *LIKE         DEFINE    VIDprd        wprd1
013560060111     c     *LIKE         DEFINE    VIDprd        savprd
013570060111     c     *LIKE         DEFINE    VIDnzd        wnzd1
013580060111     c     *LIKE         DEFINE    VIDnzd        savnzd
013590970808     C     *LIKE         DEFINE    E14CAP        VARCAD
013600970808     C     *LIKE         DEFINE    E14LOC        VARLOD
013610970808     C     *LIKE         DEFINE    E14CAP        VARCAM
013620970808     C     *LIKE         DEFINE    E14LOC        VARLOM
013630991006     C     *LIKE         DEFINE    AR6POR        VARPOR
013640991006     C     *LIKE         DEFINE    AR6VA1        VA2VA1
013650941220     C* PER IL CONTROLLO
013660941212     C     *LIKE         DEFINE    AR9VCA        WVLT
013670941212     C     *LIKE         DEFINE    AR4NOT        SAVRD2
013680941019     C     *LIKE         DEFINE    AR4TRC        WTRC
013690950424     C     *LIKE         DEFINE    VIDQFT        VARQFT
013700950424     C     *LIKE         DEFINE    VIDKLP        VARKLP
013710950424     C     *LIKE         DEFINE    VIDKSC        VARKSC
013720970418     C     *LIKE         DEFINE    §7TSOZ        WSOSC
013730970418     C     *LIKE         DEFINE    ARBFVF        WFVF
013740970813     C     *LIKE         DEFINE    VIDKSC        MEMKSC
013750991005     C     *LIKE         DEFINE    VIDDIV        DIVGEI
013760991006     C     *LIKE         DEFINE    VIDDIV        WDIV
013770991007     C     *LIKE         DEFINE    AR6DFT        DFT2BO
013780950113     C*
013790950113     C     *LIKE         DEFINE    FTDDDC        WDRT
013800950113     C     *LIKE         DEFINE    FTDPDR        WPDR
013810950113     C     *LIKE         DEFINE    FTDNDC        WNDC
013820950113     C     *LIKE         DEFINE    FTDDDC        WDDC
013830950113     C     *LIKE         DEFINE    FTDTSR        WTSR
013840020327     C     *LIKE         DEFINE    FTDSTP        wstp
013850950114     C* CAMPI COLLEGATI ALLA VARIAZIONE DEL MITTENTE DA STORICIZZARE
013860950114     C     *LIKE         DEFINE    ARBFVF        MITFVF
013870950114     C     *LIKE         DEFINE    ARBVLF        MITVLF
013880950114     C     *LIKE         DEFINE    ARBCTR        MITCTR
013890950601     C* CAMPI PER GESTIONE VARIAZIONI DI BOLLE ESTERE NON LOCALI
013900950601     C     *LIKE         DEFINE    ARBLNA        WEUV
013910950601     C     *LIKE         DEFINE    ARBCVB        PARCVB
013920950601     C     *LIKE         DEFINE    ARBDTV        PARDTV
013930950601     C     *LIKE         DEFINE    ARBORV        PARORV
013940950601     C     *LIKE         DEFINE    ARBAAS        PARAAS
013950950601     C     *LIKE         DEFINE    ARBLNP        PARLNP
013960950601     C     *LIKE         DEFINE    ARBNRS        PARNRS
013970950601     C     *LIKE         DEFINE    ARBNSP        PARNSP
013980950601     C     *LIKE         DEFINE    ARBCVB        PARCVC
013990950601     C     *LIKE         DEFINE    SIMFEL        PARFEV
014000130410     C***  *LIKE         DEFINE    ARITIP        KTIP
014010001204     C     *LIKE         DEFINE    ARBTC1        SAVTC1
014020001204     C     *LIKE         DEFINE    ARBTC1        SAVTC2
014030930507     C*
014040950113     C                   MOVEL     'R'           WTSR
014050911211     C                   MOVEL     'R'           RIS               1
014060930210     C                   MOVEL     'M'           EMME
014070990818     C                   MOVEL     'C '          FTC               2
014080130410     C***                MOVEL     'A'           KTIP
014090931119     C***
014100911211     C* GIRO UDATE
014110931119     C***
014120941014     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 3
014130941014     C                   TIME                    W0140            14 0
014140941014     C* UDATE IN GGMMAAAA
014150941014     C                   MOVE      W0140         WDTGIO            8 0
014160941014     C*
014170941014     C* UDATE IN AAAAMMGG
014180941014     C                   Z-ADD     WDTGIO        G02DAT
014190941014     C                   MOVEL     *BLANK        G02ERR
014200941014     C                   CALL      'XSRDA8'
014210941014     C                   PARM                    WLBDAT
014220941014     C                   MOVEL     G02INV        DATEU             8 0
014230030710     c     *iso          Move      dateu         wdataoggi
014240970423     C*
014250970423     C                   MOVEL     4             KNPG
014260941014     C**
014270941014     C*  KEY
014280941014     C**
014290941014     C     KARB          KLIST
014300941212     C                   KFLD                    D48AAS
014310941212     C                   KFLD                    D48LNP
014320941212     C                   KFLD                    D48NRS
014330941212     C                   KFLD                    D48NSP
014340941019     C     KARB2         KLIST
014350941212     C                   KFLD                    D48AAS
014360941212     C                   KFLD                    D48LNP
014370941212     C                   KFLD                    D48NRS
014380941212     C                   KFLD                    D48NSP
014390941019     C                   KFLD                    WTRC
014400130410     C***  KARI          KLIST
014410130410     C***                KFLD                    D48AAS
014420130410     C***                KFLD                    D48LNP
014430130410     C***                KFLD                    D48NRS
014440130410     C***                KFLD                    D48NSP
014450130410     C***                KFLD                    KTIP
014460941014     C     KFTT          KLIST
014470941215     C                   KFLD                    FTDPDR
014480941215     C                   KFLD                    FTDTSR
014490941215     C                   KFLD                    FTDNDC
014500950113     C                   KFLD                    FTDDDC
014510950113     C     KFTT2         KLIST
014520950113     C                   KFLD                    WPDR
014530950113     C                   KFLD                    WTSR
014540950113     C                   KFLD                    WNDC
014550950113     C                   KFLD                    WDRT
014560991005     C     KFTD          KLIST
014570950113     C                   KFLD                    WPDR
014580950113     C                   KFLD                    WTSR
014590950113     C                   KFLD                    WNDC
014600950113     C                   KFLD                    WDRT
014610950113     C                   KFLD                    WKSC
014620950113     C                   KFLD                    WRSM
014630020326     C     KFTD5         KLIST
014640020326     C                   KFLD                    WPDR
014650020326     C                   KFLD                    WTSR
014660020326     C                   KFLD                    WNDC
014670020326     C                   KFLD                    WDRT
014680020326     C                   KFLD                    D48AAS
014690020326     C                   KFLD                    D48LNP
014700020326     C                   KFLD                    D48NRS
014710020326     C                   KFLD                    D48NSP
014720941014     C* ACCESSO   TABEL                                    *
014730990930     C     KTAB1         KLIST                                                  *
014740990930     C                   KFLD                    CODUT                          *
014750990930     C                   KFLD                    COD               2            *
014760941014     C     KTAB2         KLIST                                                  *
014770941014     C                   KFLD                    CODUT                          *
014780941014     C                   KFLD                    COD               2            *
014790941014     C                   KFLD                    KEY               8            *
014800941014     C* ACCESSO   CNACO                                    *
014810941014     C     KACO          KLIST                                                  *
014820941014     C                   KFLD                    CODUT                          *
014830060131     C                   KFLD                    dutkci                         *
014840941014     C                   KFLD                    KSC               7 0          *
014850970423     C     KFVV          KLIST                                                  *
014860970423     C                   KFLD                    KNPG                           *
014870970423     C                   KFLD                    W0050                          *
014880020911     C                   KFLD                    Kfgs
014890941014     C* ACCESSO   TFTAM CON PROGRESSIVO                    *
014900941014     C*
014910941014     C     KTAM4         KLIST                                                  *
014920941014     C                   KFLD                    KSC                            *
014930941014     C                   KFLD                    COMCTR                         *
014940941014     C                   KFLD                    COMPRG            3 0          *
014950941014     C     KTAM6         KLIST                                                  *
014960941014     C                   KFLD                    KSC2                           *
014970941014     C                   KFLD                    COMCTR                         *
014980941014     C                   KFLD                    COMPRG                         *
014990941014     C     KTPT          KLIST                                                  *
015000941014     C                   KFLD                    KSC                            *
015010941014     C                   KFLD                    COMCTR                         *
015020941014     C                   KFLD                    COMPRG                         *
015030990818     C                   KFLD                    FTC               2            *
015040030124
015050030124     c     kAr5          Klist
015060030124     c                   Kfld                    D48Aas
015070030124     c                   Kfld                    D48Lnp
015080030124     c                   Kfld                    D48Nrs
015090030124     c                   Kfld                    D48Nsp
015100030124     c                   Kfld                    kAr5Trd
015110030722
015120030722     C     kazcln        klist
015130030722     C                   kfld                    ktfp
015140030722     C                   kfld                    ktfa
015150030722     C                   kfld                    kann
015160030722     C                   kfld                    kmes
015170040809
015180040809     C* CHIAVE SU FNDCT01L da *PGM utilità FIDN12R - Completa
015190080701     C***  KDCT1_C       KLIST
015200080701     C***                KFLD                    dsaac
015210080701     C***                KFLD                    dsfil
015220080701     C***                KFLD                    dsnca
015230041020     C* CHIAVE SU Fiqdt01L
015240041020     C     Kqdt          KLIST
015250041020     C                   KFLD                    kfgs
015260041020     C                   KFLD                    w0060
015270050601     c*
015280050601     C* CHIAVE SU Tita430C
015290050601     C     Kta4          KLIST
015300050601     C                   KFLD                    D48AAS
015310050601     C                   KFLD                    D48LNP
015320050601     C                   KFLD                    D48NRS
015330050601     C                   KFLD                    D48NSP
015340050601     c                   kfld                    kta4trc
015350141029
015360141029     C     Kidc1         KLIST
015370141029     C                   KFLD                    D48AAS
015380141029     C                   KFLD                    D48LNP
015390141029     C                   KFLD                    D48NRS
015400141029     C                   KFLD                    D48NSP
015410141029     C                   KFLD                    w0140I
015420030722
015430941212     C**
015440941212     C                   MOVEL     W256A         KPJBU
015450001213     C*
015460001213     C                   MOVE      '0'           OPN48             1
015470911211     C                   ENDSR
015480120426     C*
015490120426      * APERTURA TITA430C
015500120426     c* se provengo da variazione bolla di sede non ho bisogno del tita4
015510120426     C     OPN_TITA4     BEGSR
015520120426     c                   if        %subst(d48cvb:1:1)<>'S'
015530151016     C                   if        NOT %OPEN(TITA430C)
015540120426      * Se sono in ambiente buono - GAITRAGRU
015550141020     c*****              If        knsif = 'FILTRA201 '
015560141020     c                   If        %subst(knsif:7:1) <> 'P'
015570120426     c                   Eval      wlib = 'GAITRAGRU '
015580120426      *  Se sono in ambiente di prova - GAITRAGRPS
015590120426     c                   Else
015600120426     c                   Eval      wlib = 'GAITRAGRPS'
015610120426     c                   EndIf
015620120426     c*
015630120426     c                   Clear                   commt
015640151016     c                   Movel(p)  cmdt(1)       commt
015650120426     c                   Eval      %Subst(commt:30:10) = wlib
015660120426     c                   Eval      commt =
015670151016     c                             %trim(commt) + '/TITA430C)'
015680120426     c                   Call      'QCMDEXC'
015690120426     c                   Parm                    commt
015700120426     c                   Parm                    lenght
015710151016     c                   Open      tita430c
015720120426     c                   endif
015730151016     c* FIAR531C
015740151016     c                   if        not %open(fiar531c)
015750151016     c                   If        %subst(knsif:7:1) <> 'P'
015760151016     c                   Eval      wlib = 'GAITRAGRU '
015770151016      *  Se sono in ambiente di prova - GAITRAGRPS
015780151016     c                   Else
015790151016     c                   Eval      wlib = 'GAITRAGRPS'
015800151016     c                   EndIf
015810151016     c*
015820151016     c                   Clear                   commt
015830151016     c                   Movel(p)  cmdt(2)       commt
015840151016     c                   Eval      %Subst(commt:30:10) = wlib
015850151016     c                   Eval      commt =
015860151016     c                             %trim(commt) + '/FIAR531C)'
015870151016     c                   Call      'QCMDEXC'
015880151016     c                   Parm                    commt
015890151016     c                   Parm                    lenght
015900151016     c                   Open      fiar531c
015910151016     c                   endif
015920151016     c* TITAS30C
015930151016     C                   if        NOT %OPEN(TITAS30C)
015940151016      * Se sono in ambiente buono - GAITRAGRU
015950151016     c                   If        %subst(knsif:7:1) <> 'P'
015960151016     c                   Eval      wlib = 'GAITRAGRU '
015970151016      *  Se sono in ambiente di prova - GAITRAGRPS
015980151016     c                   Else
015990151016     c                   Eval      wlib = 'GAITRAGRPS'
016000151016     c                   EndIf
016010151016     c*
016020151016     c                   Clear                   commt
016030151016     c                   Movel(p)  cmdt(3)       commt
016040151016     c                   Eval      %Subst(commt:30:10) = wlib
016050151016     c                   Eval      commt =
016060151016     c                             %trim(commt) + '/TITAS30C)'
016070151016     c                   Call      'QCMDEXC'
016080151016     c                   Parm                    commt
016090151016     c                   Parm                    lenght
016100151016     c                   Open      titas30c
016110151016     c                   endif
016120151016     c                   endif
016130120426     C                   ENDSR
016140900509     C*
016150050907      *--- CONTROLLO LA PARTICOLARITA' GIACENZA NEL PERIODO DELLE TRAZIONI RIDOTTE ---*
016160050907     C     Contr7q       BEGSR
016170050901      *
016180050907      * Inizializzo un flag che indica se è consentita la variazione della data cons. richiesta
016190050907      * in periodo d trazioni ridotte ed in presenza d particolarità consegna genericamente "G8"
016200050907     c                   clear                   wDCRG8            1
016210050901      * Determino se variazione di bolla con part. giacenza durante
016220050901      *  periodo delle trazioni ridotte
016230050901     c                   If        dateu >= §2g2dri and
016240050901     c                             dateu <= §2g2drf
016250050901      * posso modificare se arbgga <> *Blanks e se in tabella 7Q ha 'R' come cod. disposizione
016260050901     c                   If        ArbGga <> *Blanks
016270050901     c                   Movel     '7Q'          cod
016280050901     c                   Movel(p)  ArbGga        key
016290050901     c                   ExSr      ctabel
016300050901     c  n30              Movel     TblUni        ds7q
016310050901     c   30              Clear                   ds7q
016320050901     c                   If        §7q4d1 = 'R' or §7q4d2 = 'R' or
016330050901     c                             §7q4d3 = 'R' or §7q4d4 = 'R' or
016340050901     c                             §7q4d5 = 'R'
016350050901     c                   Movel     'S'           wDCRG8
016360050901     c                   EndIF
016370050901     c                   EndIF
016380050901     c                   EndIF
016390050901
016400050901     c                   EndSR
016410080201     c
016420900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
016430900509     C     CONTR         BEGSR
016440030122
016450030122     c     Riprova       Tag
016460030122
016470941014     C                   SETOFF                                       90
016480941014     C                   SETOFF                                       203519
016490941014     C                   SETOFF                                       253403
016500950609     C                   SETOFF                                       262729
016510990415     C                   SETOFF                                       73
016520151222     c                   clear                   wfile
016530941014     C***
016540941014     C* VEDO SE E' UN PROGRAMMA RICHIAMATO
016550941014     C***
016560941019     C     D48TRC        IFNE      *BLANKS
016570941019     C                   SETON                                        20
016580950114     C                   ELSE
016590941019     C                   ENDIF
016600950607     C***
016610950607     C* ABILITO L'ENTER COME TASTO DI AGGIORNAMENTO SE RICHIESTO
016620950607     C***
016630950607     C     D48FFR        IFNE      'V'
016640070309     C     D48FFR        andne     'F'
016650950607     C                   SETON                                        26
016660950607     C                   ENDIF
016670070309     c
016680070309     c* Se richiamo con F  --> mi imposto la E  (ma con 26 off non effettua
016690070309     c*    l'aggiornamento per ENTER)
016700070309     c                   if        d48ffr='F'
016710070309     c                   eval      d48ffr='E'
016720070309     c                   endif
016730070309     c
016740950607     C***
016750950607     C* ABILITO F12 SE RICHIESTO
016760950607     C***
016770020603     C     W48F12        IFEQ      'S'
016780950607     C                   SETON                                        27
016790950607     C                   ENDIF
016800950609     C***
016810950609     C* PROTEGGO TUTTI I CAMPI SE RICHIAMATO DA PGM PER ACQUISIZIONE
016820950609     C* AUTOMATICA VARIAZIONI DA PC ESTERO
016830950609     C***
016840140404     C***  D48EUV        IFEQ      'N'
016850140404     C***                SETON                                        29
016860140404     C***                END
016870941222     C**
016880941222     C* APRO I FILE A SECONDA DEL TIPO BOLLA
016890941222     C**
016900941222     C     D48TBO        IFEQ      'A'
016910941223     C                   SETON                                        10
016920941222     C                   ELSE
016930941223     C                   SETOFF                                       10
016940941222     C                   ENDIF
016950941222     C**
016960930505     C* SPEDIZIONE
016970941222     C**
016980151218     c***                if        d48cvb<>'SD' and d48cvb<>'SF'
016990151218     C***10KARB          CHAIN     FNARB01L                           3031
017000151218     C**N10KARB          CHAIN     FNBLP01L                           3031
017010930505     C*
017020930505     C* NON UTILIZZABILE
017030151218     C***  *IN31         IFEQ      *ON
017040151218     C***                MOVEL     MSG(1)        D48MSG
017050151218     C***                MOVEL     '1'           D48ERR
017060151218     C***                SETON                                        90
017070151218     C***                GOTO      ENDCTR
017080151218     C***                ENDIF
017090941223     C* NON TROVATA SPEDIZIONE
017100151218     C***  *IN30         IFEQ      *ON
017110151218     C***                MOVEL     MSG(61)       D48MSG
017120151218     C***                MOVEL     '1'           D48ERR
017130151218     C***                SETON                                        90
017140151218     C***                GOTO      ENDCTR
017150151218     C***                ENDIF
017160151218     c***                endif
017170080201     C*
017180080201     C* SE PGM RICHIAMATO CONTROLLO SOLO LA CORRETTEZZA DELLA CAUSALE
017190080201     C                   CLEAR                   Fnlr66DS
017200080201     C                   MOVEL     'C'           D66GES
017210080201     C                   MOVEL     D48CVB        D66CVB
017220080201     C  N20              MOVEL     D48AAS        D66AAS
017230080201     C  N20              MOVEL     D48LNP        D66LNP
017240080201     C  N20              MOVEL     D48NRS        D66NRS
017250080201     C  N20              MOVEL     D48NSP        D66NSP
017260080201     C  N20              MOVEL     D48TBO        D66TBO
017270160115     c                   eval      d66bnp='S'
017280080201     C                   MOVEL     Fnlr66DS      KPJBU
017290080201     C                   CALL      'FNLR66R'
017300080201     C                   PARM                    KPJBA
017310080201     C                   MOVEL     KPJBU         Fnlr66DS
017320080201     C*
017330080201     C* TROVATO ERRORE
017340080201     C     D66ERR        IFEQ      '1'
017350080201      * se prima ho variato il destinatario ed ora devo aggiornare le consegne particolari
017360080201      * in caso di errore provo a cambiare la causale di modifica da 'CP' a 'CR'
017370080201     c                   If        wI0 = 'S'
017380080201     c                   Movel     'CR'          D48Cvb
017390080201     c                   Clear                   wI0
017400080201     c                   Goto      Riprova
017410080201     c                   Else
017420080201     C                   MOVEL     D66MSG        D48MSG
017430080201     C                   MOVEL     D66ERR        D48ERR
017440080201     C                   SETON                                        90
017450151218     c** 10              unlock    fnarb01l
017460151218     c**n10              unlock    fnblp01l
017470080201     c
017480080201     C                   GOTO      ENDCTR
017490080201     c                   EndIf
017500080201     C                   ENDIF
017510141112     c* Le variazioni I0 non sono ammesse se presenti dispo di dirottamento
017520141112     c                   if        d48cvb='I0' or d48cvb='I7' or
017530141112     c                             d48cvb='I8' or d48cvb='I9'
017540141112     c                   clear                   fnlry09ds2
017550141112     c                   eval      ILRY09TCH='T'
017560141112     c                   eval      ILRY09TDIS='D'
017570141112     c                   eval      ILRY09AAS=d48aas
017580141112     c                   eval      ILRY09LNP=d48lnp
017590141112     c                   eval      ILRY09NRS=d48nrs
017600141112     c                   eval      ILRY09NSP=d48nsp
017610141112     c                   eval      kpjbu=fnlry09ds2
017620141112     c                   call      'FNLRY09C'
017630141112     c                   parm                    kpjba
017640141112     c                   eval      fnlry09ds2=kpjbu
017650141112     c                   if        OLRY09ESIT='1'
017660141112     C                   MOVEL     msg(156)      D48MSG
017670141112     C                   MOVEL     '1'           D48ERR
017680141112     C                   SETON                                        90
017690151218     c** 10              unlock    fnarb01l
017700151218     c**n10              unlock    fnblp01l
017710141112     c
017720141112     C                   GOTO      ENDCTR
017730141112     c                   endif
017740141112     c                   endif
017750151218     C**
017760151218     C* SPEDIZIONE
017770151218     C**
017780151218     c                   if        d48cvb<>'SD' and d48cvb<>'SF'
017790151221     c* Manutenzione bolle in arrivo
017800151221     c                   if        *in10
017810151221     c*
017820151221     c                   setoff                                       30
017830151221     c                   eval      wfile='A'
017840151221     c                   if        d66utp<>'P'
017850151221     C     KARB          CHAIN     FNARB01L                           3031
017860151221     c                   endif
017870151221     c                   if        (*in30 and d66utp='T') or d66utp='P'
017880151221     C     KARB          CHAIN     FNblp01L                           3031
017890160112     c                   if        not *in31 and not*in30  and ARBft1<>*blanks
017900151221     c                   unlock    fnblp01l
017910151221     C                   MOVEL     MSG(162)      D48MSG
017920151221     C                   MOVEL     '1'           D48ERR
017930151221     C                   SETON                                        90
017940151221     C                   GOTO      ENDCTR
017950151221     c                   endif
017960151221     c                   eval      wfile='P'
017970151221     c                   endif
017980151221     c                   else
017990151221     c* Manutenzione in partenza
018000151222     c                   eval      wfile='P'
018010151221     C     KARB          CHAIN     FNBLP01L                           3031
018020151221     c                   endif
018030151218     C*
018040151218     C* NON UTILIZZABILE
018050151218     C     *IN31         IFEQ      *ON
018060151218     C                   MOVEL     MSG(1)        D48MSG
018070151218     C                   MOVEL     '1'           D48ERR
018080151218     C                   SETON                                        90
018090151218     C                   GOTO      ENDCTR
018100151218     C                   ENDIF
018110151218     C* NON TROVATA SPEDIZIONE
018120151218     C     *IN30         IFEQ      *ON
018130151218     C                   MOVEL     MSG(61)       D48MSG
018140151218     C                   MOVEL     '1'           D48ERR
018150151218     C                   SETON                                        90
018160151218     C                   GOTO      ENDCTR
018170151218     C                   ENDIF
018180151218     c                   endif
018190080201     C* KEY SPEDIZIONE
018200080201     C                   MOVEL     D48CVB        VIDCVR
018210080201     C                   MOVEL     D48CVB        FLGCVR            1
018220080201     C*
018230080201     C* IMPOSTO IL CAMPO §7LFFI CHE MI SERVE PER ALLOCARE I FILE
018240080201     C                   MOVEL     D66FFI        §7LFFI
018250080201     C*
018260080201     C* IMPOSTO IL CAMPO §7LFIN CHE MI SERVE PER PROTEGGERE I CAMPI
018270080201     C* CHE NON DEVO VARIARE
018280130411     C***                MOVEL     D66BDI        §7LFIN
018290941014     C**
018300941014     C**
018310900509     C     ENDCTR        ENDSR
018320910429     C*
018330910429     C*--- CARICO DATI BOLLA -----------------------------------------*
018340910429     C     CARICA        BEGSR
018350920123     C                   SETOFF                                       040521
018360911211     C                   SETOFF                                       131415
018370990930     C                   SETOFF                                       0102
018380941220     C                   SETOFF                                       363789
018390941213     C                   SETOFF                                       111209
018400991001     C                   SETOFF                                       882307
018410941222     C                   SETOFF                                       525306
018420991012     C                   SETOFF                                       167308
018430061107     C                   SETOFF                                       747576
018440131001     C                   SETOFF                                       878681
018450140213     C                   SETOFF                                       838477
018460140408     c                   setoff                                       68
018470011018      *
018480011018     C                   CLEAR                   FLGVRN
018490110906     C                   CLEAR                   lnacons
018500941102     C*
018510950120     C                   CLEAR                   WIN95
018520950120     C                   CLEAR                   WIN97
018530950120     C                   CLEAR                   WIN93
018540131016     C                   CLEAR                   WINF14
018550950120     C                   CLEAR                   VARVCA
018560941215     C                   CLEAR                   VARVAS
018570941215     C                   CLEAR                   VARVAD
018580941215     C                   CLEAR                   VARVLF
018590941215     C                   CLEAR                   VARPKF
018600990702     C                   CLEAR                   SAVIMV
018610151016     C                   CLEAR                   SAVIMV_fat
018620991006     C                   CLEAR                   WDIV
018630991006     C                   CLEAR                   VARPOR
018640991006     C                   CLEAR                   VA2VA1
018650991012     C                   CLEAR                   SAVCBO
018660091124     C                   CLEAR                   WPT1              1
018670941209     C                   MOVEL     0             CIN               1 0
018680941209     C                   MOVEL     0             DIE               1 0
018690941212     C                   MOVEL     0             SEI               1 0
018700941214     C                   MOVEL     0             SET               1 0
018710941223     C                   MOVEL     0             OTT               1 0
018720970808     C                   MOVEL     0             WZV               1 0
018730970808     C                   MOVEL     0             WZ1               1 0
018740970808     C                   MOVEL     0             WZ2               1 0
018750070508     C                   MOVEL     0             WZ3               1 0
018760970808     C                   MOVEL     0             WZVM              1 0
018770970808     C                   MOVEL     0             WZ1M              1 0
018780970808     C                   MOVEL     0             WZ2M              1 0
018790141029     C                   MOVEL     0             Wforzdcr          1 0
018800141112     C                   MOVEL     0             Wforzgiac         1 0
018810990930     C                   MOVEL     ' '           WAR62             1
018820941019     C                   MOVEL     ' '           WAR6              1
018830941209     C                   MOVEL     ' '           WAR7              1
018840040123     c                   Clear                   War72
018850941019     C                   MOVEL     ' '           WAR9              1
018860941019     C                   MOVEL     ' '           WAR4D             1
018870150206     C                   MOVEL     ' '           WAR4X             1
018880061116     C                   MOVEL     ' '           WAR4Q             1
018890000623     C                   MOVEL     ' '           WBL4W             1
018900920319     C                   Z-ADD     0             COMKSC
018910930507     C                   Z-ADD     0             COMCTR
018920930507     C                   Z-ADD     0             COMPRG
018930911211     C                   Z-ADD     0             VI2DFT
018940911211     C                   Z-ADD     0             VIDDFT
018950941209     C                   Z-ADD     0             VARIAS
018960930507     C                   MOVEL     *BLANKS       VARCTR            3
018970941214     C                   CLEAR                   WSOST
018980030710     c                   Clear                   wokpsw
018990030729
019000030729     c                   Clear                   V1cBan
019010030729     c                   Clear                   V1cCor
019020030729     c                   Eval      *In70 = *Off
019030040123     c                   Clear                   ContaSv
019040040123     c                   Clear                   TotImv
019050060223     C                   CLEAR                   V1CCAS
019060060223     C                   CLEAR                   V1CTIC
019070060223     C                   CLEAR                   V1CVCA
019080060223     C                   CLEAR                   SAVCAS
019090060223     C                   CLEAR                   SAVTIC
019100060223     C                   CLEAR                   SAVVCA
019110090914     C                   CLEAR                   SAVrd2
019120150203     C                   CLEAR                   normlod
019130941215     C* PULIZIA DEI FORMATI VIDEO
019140950210     C                   CLEAR                   LR48D02
019150941215     C                   CLEAR                   LR48C03
019160941215     C                   CLEAR                   LR48D04
019170941215     C                   CLEAR                   LR48D05
019180941215     C                   CLEAR                   LR48D07
019190950104     C                   CLEAR                   LR48D08
019200961211     C                   CLEAR                   LR48D09
019210030124     c                   Clear                   Lr48d10
019220950213     C                   CLEAR                   V1CTIC
019230950213     C                   CLEAR                   V1CCAS
019240950213     C                   CLEAR                   V1CVCA
019250950213     C                   CLEAR                   V1CNT1
019260950213     C                   CLEAR                   V1CNT2
019270950213     C                   CLEAR                   V1CTC1
019280950213     C                   CLEAR                   V1DTC1
019290950213     C                   CLEAR                   V1CTC2
019300950213     C                   CLEAR                   V1DTC2
019310030124     c                   Clear                   VidNom
019320030124     c                   Clear                   VidTel
019330030124     c                   Clear                   VidMot
019340030122     c                   Clear                   wI0
019350031120     c                   Clear                   VidRef
019360070508     c                   Clear                   Vidtfd
019370031120     c                   Clear                   VidTeld
019380070306     c                   Clear                   AggioARP
019390070308     c                   Clear                   savdcr
019400070308     c                   Clear                   savtcr
019410070308     c                   Clear                   savhcr
019420070308     c                   Clear                   bollafbc
019430070309     c                   Clear                   Msgforza          3 0
019440150112     c                   Clear                   Msgforzatxt      78
019450070309     c                   clear                   eveADR            1
019460070910     c                   clear                   WesAR5GEN         1
019470130930     c                   clear                   WesAR5EMD         1
019480071025     c                   clear                   ForzaCDF
019490071025     c                   clear                   ForzaPID
019500071025     c                   clear                   ForzaPID2
019510071025     c                   clear                   sav11pid
019520071026     c                   clear                   AggioBol
019530111025     c                   clear                   Dcrindist         1
019540151019     c                   clear                   PesVolPkcn
019550151019     c                   clear                   PesVolSpc
019560151019     c                   clear                   PesVolImv
019570151019     c                   clear                   TassatoImv
019580061129     c  n10              movel     'T'           bollaft2
019590061129     c   10              movel     'R'           bollaft2
019600111207     c* ES - Per il momento lo imposto sempre = "S" in quanto l'ufficio qualità non è
019610111207     c*  sicuro di voler fare la modifica di bloccare la manut coonsegna richiesta
019620111207     c*  dalla manutenzione normale
019630111207     c                   eval      Dcrindist='S'
019640941223     C*
019650941223     C     D48TBO        IFEQ      'A'
019660941223     C                   SETON                                        10
019670941223     C                   ELSE
019680941223     C                   SETOFF                                       10
019690941223     C                   ENDIF
019700941223     C*
019710941223     C* CLEARO I FORMATI DELLE BOLLE
019720051118     C                   CLEAR                   FiAR9000
019730990930     C                   CLEAR                   FIAR7000
019740990930     C                   CLEAR                   FIAR6000
019750030124     c                   Clear                   Fiar5000
019760941220     C**
019770941220     C* PGM RICHIAMATO
019780941220     C**
019790941220     C                   SELECT
019800941220     C* PROGRAMMA RICHIAMATO    DSARBD
019810941220     C     D48TRC        WHENEQ    'D'
019820941220     C                   SETON                                        2034
019830941220     C* PROGRAMMA RICHIAMATO    DSARBK
019840941220     C     D48TRC        WHENEQ    'K'
019850950616     C                   SETON                                        203529
019860990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 1
019870941220     C     D48TRC        WHENEQ    'T'
019880941220     C                   SETON                                        2025
019890990416     C*
019900941220     C* PROGRAMMA RICHIAMATO    DSARBG
019910941220     C     D48TRC        WHENEQ    'G'
019920941220     C                   SETON                                        2003
019930990930     C* PROGRAMMA RICHIAMATO    DARBT TIPO RECORD 2
019940941220     C     D48TRC        WHENEQ    '2'
019950941220     C                   SETON                                        2019
019960130930     C* PROGRAMMA RICHIAMATO    DARBN TIPO RECORD N + parm
019970130930     C     D48TRC        WHENEQ    'N'
019980130930     c                   if        %parms=7
019990130930     C                   SETON                                        20
020000131008     c                   else
020010131008     C                   MOVEL     MSG(24)       D48MSG
020020131008     C                   MOVEL     '1'           D48ERR
020030131008     c                   leavesr
020040131008     c                   endif
020050131008     c
020060941220     C                   ENDSL
020070950607     C*
020080020603     C     W48F12        IFEQ      'S'
020090950607     C                   SETON                                        27
020100950607     C                   ENDIF
020110950609     C*
020120140404     C***  D48EUV        IFEQ      'N'
020130140404     C***                SETON                                        29
020140140404     C***                END
020150950114     C**
020160950114     C* 24 ON - VARIATO IL MITTENTE
020170950114     C**
020180950114     C     WRICVL        IFEQ      'S'
020190950114     C                   SETON                                        24
020200950114     C                   ENDIF
020210941019     C**
020220941019     C* CARICO I DATI DELLA VIDEATA GENERICA DELLA BOLLA
020230941019     C**
020240941019     C* CHAIN CON CODICE BOLLA
020250941019     C                   MOVEL     '3A'          COD
020260941102     C                   MOVEL(P)  ARBCBO        KEY
020270941212     C                   EXSR      CTABEL
020280941019     C  N30              MOVEL     TBLUNI        DS3A
020290941019     C   30              CLEAR                   DS3A
020300941019     C*
020310941019     C                   MOVEL     §3ATB1        FLGTB1            1
020320941019     C                   MOVEL     §3ADES        V1DCBO                         DESCRIZIONE
020330071116     c* Verifico se il 1°tipo bolla è tassabile o no
020340071116     C                   MOVEL     'TB'          COD
020350071116     C                   MOVEL(P)  §3atb1        KEY
020360071116     C                   EXSR      CTABEL
020370071116     C  N30              MOVEL     TBLUNI        DSTB
020380071116     C   30              CLEAR                   DSTB
020390071116     c                   movel     §tbfcb        oldtb1FCB         1
020400071116     c
020410070308     c
020420070308     c** Blocco consegna (mi serve per la consegna richiesta)
020430151221     c***10              eval      bollaFBC=arbfbc
020440151221     c                   if        *in10
020450151221     c                   if        wfile='A'
020460151221     c                   eval      bollaFBC=arbfbc
020470151221     c                   else
020480151221     c                   clear                   bollaFBC
020490151221     c                   endif
020500151221     c                   endif
020510070308     c  N10              clear                   bollaFBC
020520941019     C*
020530941019     C* SE FRANCO PROTEGGO P.IVA
020540011017     C     FLGTB1        COMP      'F'                                    05
020550941222     C   05              SETON                                            06
020560941019     C*
020570941019     C* SE ESISTE SECONDA BOLLA PERMETTO L'IMMISSIONE
020580061107     c                   clear                   ksc2bol
020590941212    1C     §3ATB2        IFNE      '  '
020600941019     C   05              SETOFF                                       05
020610941019     C* CHAIN SU 2 BOLLA
020620990930     C                   MOVEL     '2'           WTRC
020630070525     c                   if        flgcvr='2'
020640990930     C     KARB2         CHAIN     FIAR6000                           30
020650070525     c                   else
020660070525     C     KARB2         CHAIN(N)  FIAR6000                           30
020670070525     c                   endif
020680990930     C  N30              MOVEL     'E'           WAR62
020690990930     C  N30              MOVEL     AR6KSC        V1CKSC
020700991007     C  N30              Z-ADD     AR6DFT        DFT2BO
020710941019     C   30              Z-ADD     9999          V1CKSC
020720941019     C   30              MOVEL     ARBLNA        V1CLNA
020730991007     C   30              MOVE      *ZEROS        DFT2BO
020740061107     c                   movel     v1cksc        ksc2bol           7 0
020750040127      * Vedo se esiste anche il Fiar7
020760040127     c     Karb2         Setll     Fiar701l
020770040127     c                   If        %Equal(Fiar701l)
020780040127     c                   Eval      war72 = 'E'
020790040127     c                   EndIf
020800941019     C                   ELSE
020810941019     C                   MOVEL     ARBKSC        V1CKSC
020820941212    1C                   END
020830941019     C**
020840051118     C* SE PREVEDE IL C/A CHAI SU FiAR9
020850941019     C**
020860941212    1C     §3AFCA        IFNE      0
020870041022     c                   if        flgcvr='K'
020880051118     C     KARB          CHAIN     FiAR9000                           30
020890041022     c                   else
020900041022     c* non alloco record se non è variazione di C/A
020910051118     C     KARB          CHAIN(n)  Fiar9000                           30
020920041022     c                   endif
020930041022     c
020940941212    2C     *IN30         IFEQ      *OFF
020950941019     C                   MOVEL     'E'           WAR9              1
020960941019     C                   MOVEL     AR9CAS        V1CCAS
020970941019     C                   MOVEL     AR9TIC        V1CTIC
020980941102     C                   MOVEL     AR9VCA        V1CVCA
020990950918     C                   MOVEL     AR9CAS        SAVCAS
021000950918     C                   MOVEL     AR9TIC        SAVTIC
021010950918     C                   MOVEL     AR9VCA        SAVVCA
021020941212    2C                   ENDIF
021030941212    1C                   ENDIF
021040941019     C**
021050941019     C* VEDO SE CI SONO LE NOTE
021060941019     C**
021070941019     C                   MOVEL     '8'           WTRC
021080060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021090941212    1C     *IN30         IFEQ      *OFF
021100941019     C                   MOVEL     AR4NOT        V1CNT1
021110941019     C*
021120941019     C                   MOVEL     '9'           WTRC
021130060222     C     KARB2         CHAIN(N)  FiAR4000                           30
021140941212    2C     *IN30         IFEQ      *OFF
021150941019     C                   MOVEL     AR4NOT        V1CNT2
021160941212    2C                   ENDIF
021170941212    1C                   ENDIF
021180970409     C* SI NO DDT
021190031028     c                   Clear                   dsbl4a
021200970409     C                   MOVEL     'A'           WTRC
021210151222     c                   if        wfile='A'
021220151222     C     KARB2         CHAIN(N)  FiAR4000                           30
021230151222    2C     *IN30         IFEQ      *OFF
021240031028     C**!!!              MOVEL     AR4NOT        V1CDDT
021250031028     c                   Movel     Ar4not        dsbl4a
021260031028     c                   Move      §b4abm        v1cddt
021270970409     C                   ELSE
021280970409     C                   MOVEL     'S'           V1CDDT
021290970409    2C                   ENDIF
021300151222     c                   else
021310151222     C                   MOVEL     ARBFSTddt     V1CDDT            1
021320151222     c                   endif
021330970409     C*
021340970409     C     V1CDDT        IFEQ      'S'
021350970409     C     V1CDDT        OREQ      'C'
021360031028     c     v1cddt        oreq      'P'
021370031028     c     v1cddt        oreq      'Q'
021380970409     C                   MOVEL     'DDT'         V1DDDT
021390970409     C                   ELSE
021400970409     C                   CLEAR                   V1DDDT
021410970409     C                   ENDIF
021420060222     C* CONTROLLO SE ESISTE RECORD W DI ar4 PER EVENTUALE
021430000623     C*   MODIFICA DI MITTENTE
021440000623     C     *IN10         IFEQ      *OFF
021450000623     C                   MOVEL     'W'           WTRC
021460060222     C     KARB2         CHAIN(N)  Fiar4000                           30
021470000623     C  N30              MOVEL     'S'           WBL4W             1
021480000623     C                   ENDIF
021490941019     C* DESCRIZIONE CAUSALE
021500941019     C                   MOVEL     D66DES        V1DCVR
021510941019     C                   MOVEL     ARBLNP        V1CLNP
021520941212     C                   MOVEL     ARBNRS        V1CNRS
021530941019     C                   MOVEL     ARBNSP        V1CNSP
021540941019     C                   MOVEL     ARBLNA        V1CLNA
021550941019     C                   MOVEL     ARBCBO        V1CCBO
021560941019     C                   MOVEL     ARBRSM        V1CRSM
021570941019     C                   MOVEL     ARBCAM        V1CCAM
021580941019     C                   MOVEL     ARBLOM        V1CLOM
021590941019     C                   MOVEL     ARBPRM        V1CPRM
021600941019     C                   MOVEL     ARBNZM        V1CNZM
021610970828     C                   MOVEL     ARBFAP        V1CFAP
021620941019     C                   MOVEL     ARBRSD        V1CRSD
021630941019     C                   MOVEL     ARBIND        V1CIND
021640941019     C                   MOVEL     ARBCAD        V1CCAD
021650941019     C                   MOVEL     ARBLOD        V1CLOD
021660941019     C                   MOVEL     ARBPRD        V1CPRD
021670941019     C                   MOVEL     ARBNZD        V1CNZD
021680970828     C                   MOVEL     ARBFIN        V1CFIN
021690941019     C                   MOVEL     ARBCTS        V1CCTS
021700941019     C                   MOVEL     ARBTC1        V1CTC1
021710941019     C                   MOVEL     ARBTC2        V1CTC2
021720941027     C                   MOVEL     ARBPKF        V1CPKF
021730941019     C                   MOVEL     ARBVLF        V1CVLF
021740941019     C                   MOVEL     ARBFVF        V1CFVF
021750030729     c                   Z-Add     ArbNcl        V1cNcl
021760141029     c                   clear                   v1dffd
021770141029     c                   if        arbffd='S'
021780141029     c                   eval      v1dffd='FermoDeposito'
021790141029     c                   endif
021800100121     c* Partita iva MITTENTE se bolla singola franco
021810100121     c                   select
021820100121     c                   when      flgtb1='F' and §3atb2=*blanks
021830100121     c                   eval      v1cpim=arbcpi
021840100121     c                   when      flgtb1='A' and §3atb2=*blanks
021850100121     c                   eval      v1cpid=arbcpi
021860100121     c* In arrivo   per bolla  doppia --> partita iva dell'assegnato
021870151222     c                   when      wfile='A'   and §3atb2<>*blanks
021880100121     c                   eval      v1cpid=arbcpi
021890100121     c* In partenza per bolla  doppia --> partita iva del mittente
021900151222     c                   when      wfile='P'   and §3atb2<>*blanks
021910100121     c                   eval      v1cpid=arbcpi
021920100121     c                   ENDSL
021930100121
021940950114     C* SALVO IL CODICE CLIENTE DELLA SPEDIZIONE
021950950114     C                   MOVEL     ARBKSC        SAVKSC            7 0
021960060511     C                   MOVEL     ARBKSC        kscbolla          7 0
021970941102     C* PRENDO LA VALUTA DAL DESTINATARIO E MITTENTE
021980941102     C                   MOVEL     '15'          COD
021990941102     C                   MOVEL(P)  ARBNZD        KEY
022000941212     C                   EXSR      CTABEL
022010941102     C  N30              MOVEL     TBLUNI        DS15
022020941102     C   30              CLEAR                   DS15
022030941102     C                   MOVEL     §15VLT        DESVLT            3
022040941212    1C     §15ITA        IFEQ      'I'
022050941102     C                   MOVEL     'I'           WDESIE            1
022060941102     C                   ELSE
022070941102     C                   MOVEL     'E'           WDESIE            1
022080941212    1C                   ENDIF
022090020305     c                   MOVEL     §15EFT        DESEFTW
022100020305     c                   MOVEL     §15EFD        DESEFDW
022110020305     c                   MOVEL     §15EFF        DESEFFW
022120971230     V* CHAIN SU AZORG CON LINEA PARTENZA
022130971230     C     ARBLNP        CHAIN     AZORG01L                           32
022140941209     C*
022150000223     C                   MOVEL     ORGDE3        OG143
022160020305     C                   MOVEL     §OGNTW        LNPNTW
022170941102     C*
022180941102     C                   MOVEL(P)  ARBNZM        KEY
022190941212     C                   EXSR      CTABEL
022200941102     C  N30              MOVEL     TBLUNI        DS15
022210941102     C   30              CLEAR                   DS15
022220941102     C                   MOVEL     §15VLT        MITVLT            3
022230941212    1C     §15ITA        IFEQ      'I'
022240941102     C                   MOVEL     'I'           WMITIE            1
022250941102     C                   ELSE
022260941102     C                   MOVEL     'E'           WMITIE            1
022270941212    1C                   ENDIF
022280001204     C**
022290051129     C* TERMINAL DI PARTENZA DELLA LNP: VEDO SE HO VDL
022300040123     c                   Clear                   og150
022310970901     C     ARBTFP        CHAIN     AZORG                              32
022320051129     C                   CLEAR                   WVDL
022330970901    1C     *IN32         IFEQ      *OFF
022340051129     C**!!!              TESTN                   DATVDL               31
022350040123     c                   Movel     Orgdf0        og150
022360120613     c                   testn                   §ogvol               31
022370970901     C*
022380040123    2C**!!!*IN31         IFEQ      '1'
022390051129     C**!!!DATVDL        ANDGT     *ZEROS
022400120613     c***!!!             If        §ogspv = 'S'
022410120613     c                   if        *in31 and §ogvol>*zeros
022420051129     C                   MOVEL     'S'           WVDL              1
022430970901    2C                   END
022440970901    1C                   END
022450941019     C**
022460941019     C* TIPO SERVIZIO
022470941019     C**
022480090604     c                   clear                   ds5e
022490941014     C                   MOVEL     '5E'          COD
022500941014     C                   MOVEL(P)  ARBTSP        KEY
022510941212     C                   EXSR      CTABEL
022520090604     C  N30              MOVEL     TBLUNI        ds5e
022530090604     C  N30              MOVEL     §5ed08        V1DTSP
022540941220     C   30              CLEAR                   V1DTSP
022550941019     C**
022560910429     C* DATA SPEDIZIONE
022570941019     C**
022580941014     C                   MOVEL     '3'           G02ERR
022590941014     C                   Z-ADD     ARBMGS        G02INV
022600941014     C                   MOVEL     ARBAAS        G02INV
022610941014     C                   CALL      'XSRDA8'
022620941014     C                   PARM                    WLBDAT
022630941014     C*
022640941212     C                   Z-ADD     G02DAT        V1CDSP
022650941019     C**
022660930505     C* LINEA DI ARRIVO
022670941019     C**
022680070306     C     ARBLNA        CHAIN     AZORG                              30
022690941019     C  N30              MOVEL     ORGDES        V1DLNA                          L.ARRIVO
022700941019     C   30              MOVEL     *BLANKS       V1DLNA
022710000202     C                   MOVEL     ORGDE3        OG143
022720020305     C                   MOVEL     §OGNTW        LNANTW
022730020305     c                   select
022740020305     c     lnantw        wheneq    'DPD'
022750020305     c                   movel     desefdw       deseft
022760020305     c     lnantw        wheneq    'FED'
022770020305     c                   movel     deseffw       deseft
022780020305     c                   other
022790020305     c                   movel     deseftw       deseft
022800020305     c                   endsl
022810160225
022820160225     c                   if        lnantw='FED'
022830160225     c                   clear                   trulc7ds
022840160225     c                   eval      ic7tntw='FED'
022850160225     c                   eval      ic7tip='FM'
022860160225     c                   call      'TRULC7R'
022870160225     c                   parm                    trulc7ds
022880160225     c                   eval      fedm_ksc=oc7kscc
022890160225     c                   movel     oc7ctrc       fedm_ctr
022900160225     c
022910160225     c                   clear                   trulc7ds
022920160225     c                   eval      ic7tla='L'
022930160225     c                   eval      ic7tntw='FED'
022940160225     c                   eval      ic7tip='FD'
022950160225     c                   call      'TRULC7R'
022960160225     c                   parm                    trulc7ds
022970160225     c
022980160225     c                   eval      fedd_ksc=oc7kscc
022990160225     c                   movel     oc7ctrc       fedd_ctr
023000160225     c                   endif
023010070906     c*****
023020070906     c* Verifico se destinatario disagiato
023030070906     c*****
023040070906     c                   clear                   wdisagdest
023050070906     c                   clear                   tisit0ds
023060070906     c                   movel     'E'           it0tla
023070070906     c                   movel     arbnzd        it0naz
023080070906     c                   movel     arbprd        it0prv
023090070906     c                   movel     arbcad        it0cap
023100070906     c                   movel     arblod        it0loc
023110070906     c                   movel     arbind        it0ind
023120070906     c                   movel     arbrsd        it0rag
023130070906     c                   call      'TISIT5R'
023140070906     c                   parm                    tisit0ds
023150070906    1c     ot0err        ifeq      *blanks
023160070906    2c                   if        ot0dos = 'A' or ot0dos = 'D' or
023170070906     c                             ot0dos = 'S'
023180070906     c                   movel     ot0dos        wdisagdest
023190070907    3c     wdisagdest    ifeq      'D'
023200070906     c                   movel     'X'           wdisagdest
023210070906    3c                   endif
023220070906    2c                   endif
023230070906   x1c                   else
023240070906     c                   clear                   wdisagdest
023250070906    1c                   endif
023260070306     c
023270070910     c****
023280070910      * Reperisco record GEN nel fiar5
023290070910     c                   Clear                   dAr5Genpar
023300070910     c                   Eval      kAr5Trd = 'GEN'
023310070910     c     kAr5          Chain(n)  Fiar501l
023320070910     c                   If        %Found(Fiar501l)
023330070910     c                   Movel     Ar5Uni        dAr5Genpar
023340070910     c                   eval      wesar5gen='S'
023350070910     c                   endif
023360070319     c****
023370070319     c* FILIALE GESTIONE: VEDO SE GEO ATTIVA
023380070319     c****
023390070319     c                   clear                   FGSgeot
023400070319     c                   clear                   FGSddaSI
023410110906     c****               if        *in10
023420070319     c                   clear                   fnlv55ds
023430070319     C                   MOVEL     '6'           D55TPT
023440070319     C                   MOVEL     ARBLNA        D55LIN
023450070319     C                   MOVEL     DATEU         D55DRF
023460070319     C                   CALL      'FNLV55R'
023470070319     C                   PARM                    Fnlv55DS
023480110906     c                   eval      lnacons=d55tfa
023490070319     c
023500070319     c     d55tfa        CHAIN     AZORG                              30
023510070319     c                   if        not *in30
023520070319     C                   MOVEL     ORGDE8        og148                           L.ARRIVO
023530070319     C                   MOVEL     ORGDE6        og146                           L.ARRIVO
023540070319     C
023550070319     c                   movel     §oggeot       FGSgeot           1
023560070319     C
023570070319    1c                   if        §ogdda>*zeros
023580070319     c                   testn                   §ogdda               30
023590070319     c                   move      §ogdda        w001a             1
023600070319    2c                   if        *in30 and w001a>='0'
023610070319     c* Se la data = 20391231, come se non ci fosse
023620070319    3c                   if        §ogdda<>'20391231'
023630070319     c                   movel     §ogdda        w0080             8 0
023640070426     c* Se <=oggi, attiva procedura distinte automatiche
023650070426    4c                   if        w0080<=dateu
023660070319     c                   movel     'S'           FGSddaSI          1
023670070319    4c                   endif
023680070319    3c                   endif
023690070319    2c                   endif
023700070319    1c                   endif
023710070319     c
023720070319     c                   endif
023730110906     c****               endif
023740151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
023750151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
023760140213     c                   seton                                        77
023770140213     c                   endif
023780070329     c* Se p.o. utente o lna hanno GEO attiva, propongo F17 note
023790140130     c* non le propongo se bolla in consegna variazione da inviare a AUT e
023800140130     c* filiale partita con la nuova gestione telefonate AUT
023810151222     c** 10              if        fgsgeot='S'  and dutgeot='S'
023820151222     c                   if        wfile='A' and fgsgeot='S'  and dutgeot='S'
023830140130     c                             and (
023840140130     c* non è in consegna
023850140130     c                             (arbndc=0 or arbdcm>0)
023860140130     c* filiale distinta non partita con nuova gestione telefonate AUT
023870140130     c                             or (%lookup(%subst
023880140130     c                                 (%editc(arbifp:'X'):7:3):skFil816tel)=0
023890140130     c                              and skfil816tel(1)<>'999')
023900140130     c* Causale variazione da non trasmettere a AUT
023910140130     c                             or d66pda=*blanks
023920140130     c                             )
023930070329     c                   seton                                        87
023940070329     c                   endif
023950941019     C**
023960941019     C* CONSEGNE PARTICOLARI
023970941019     C**
023980941019     C                   MOVEL     *BLANKS       V1DTC1
023990941019     C                   MOVEL     *BLANKS       V1DTC2
024000070914     C                   MOVEL     *BLANKS       V7DTC1
024010070914     C                   MOVEL     *BLANKS       V7DTC2
024020941212    1C     ARBTC1        IFNE      ' '
024030930505     C                   MOVEL     '1P'          COD
024040941019     C                   MOVEL(P)  ARBTC1        KEY
024050941212     C                   EXSR      CTABEL
024060941102     C  N30              MOVEL     TBLUNI        DS1P
024070941102     C   30              CLEAR                   DS1P
024080941102     C                   MOVEL     §1PDES        V1DTC1
024090941220     C                   MOVEL     §1PDES        V7DTC1
024100941102     C*
024110941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024120941222    2C   10§1PUPR        IFEQ      'S'
024130941213     C     §1PUAR        ANDNE     'S'
024140941102     C                   SETON                                        11
024150941212    2C                   ENDIF
024160941222     C* O VICEVERSA
024170941222    2C  N10§1PUAR        IFEQ      'S'
024180941222     C     §1PUPR        ANDNE     'S'
024190941222     C                   SETON                                        11
024200941222    2C                   ENDIF
024210941102     C*
024220941212    1C                   ENDIF
024230941019     C*
024240941212    1C     ARBTC2        IFNE      ' '
024250941019     C                   MOVEL     '1P'          COD
024260941213     C                   MOVEL(P)  ARBTC2        KEY
024270941212     C                   EXSR      CTABEL
024280941102     C  N30              MOVEL     TBLUNI        DS1P
024290941102     C   30              CLEAR                   DS1P
024300941102     C                   MOVEL     §1PDES        V1DTC2
024310941220     C                   MOVEL     §1PDES        V7DTC2
024320941102     C*
024330941102     C* SE IMMESSA DALLA PARTENZA E NON UTILIZZABILE IN ARRIVO:PROTEGGO
024340941222    2C   10§1PUPR        IFEQ      'S'
024350941213     C     §1PUAR        ANDNE     'S'
024360941102     C                   SETON                                        12
024370941212    2C                   ENDIF
024380941222     C* O VICEVERSA
024390941222    2C  N10§1PUAR        IFEQ      'S'
024400941222     C     §1PUPR        ANDNE     'S'
024410941222     C                   SETON                                        12
024420941222    2C                   ENDIF
024430941222     C*
024440941212    1C                   ENDIF
024450941019     C**
024460941019     C* SPEDIZIONE GIACENTE
024470941019     C**
024480040316     c                   Clear                   wgcfas
024490040324     c                   Clear                   wgcded
024500040324     c                   Clear                   wgcddm
024510051109     c                   Clear                   dgcpflr
024520141113if  1C**   *IN10         IFEQ      *ON
024530141113     c**   d48cvb        oreq      'CS'
024540141113     c**   d48cvb        oreq      'CR'
024550050331     C     KARB          CHAIN     Tigcp21L                           30        GIACENZA
024560941212    1C     *IN30         IFEQ      *OFF
024570050331     C     GCPFAS        ANDLT     40
024580941019     C     GCPATB        ANDEQ     ' '
024590040316     c                   Eval      wgcfas = GcpFas
024600040324     c                   Eval      wgcded = GcpDed
024610040324     c                   Eval      wgcddm = GcpDdm
024620051109     c                   Movel     gcpflr        dgcpflr
024630941019     C                   SETON                                        04
024640941212    1C                   ENDIF
024650040219      * se non ho trovato il rcd di giacenza in arrivo provo in partenza
024660040219      * solo se sto facendo una variazione 'CS' o 'CR'
024670051109      * questo solo per accendere l'indicatore 04 di giacenza aperta, non serve per la
024680051109      * variazione della data consegna richiesta, questa è possibile solo se la giacenza
024690051109      * è aperta in arrivo!!!!
024700141113      * Ora lo 04 mi serve anche per il controllo dell'abilitazione dei dirottamenti sia
024710141113      * in partenza che in arrivo e quinidi lo faccio sempre
024720141113if  2c***                If        *In30 and
024730141113     c***                          (d48cvb = 'CS' or d48cvb = 'CR')
024740141113if  2c                   If        *In30
024750050331     c     kArb          Chain     Tigcp01l
024760050331if  3c                   If        %Found(Tigcp01l) and GcpFas < 50 and
024770040219     c                             GcpAtb = *Blanks
024780040219     c                   Seton                                        04
024790040316     c                   Clear                   wgcfas
024800040324     c                   Clear                   wgcded
024810040324     c                   Clear                   wgcddm
024820040219    3c                   EndIf
024830040219    2c                   EndIf
024840141113    1C*****              ENDIF
024850051129     C* DETERMINO SE IL VOLUME VDL (CHE EVENTUALMENTE SOSTITUIRA' IL
024860970418     C* VOLUME DA FATTURARE) E' PARZIALE O TOTALE
024870970418     C                   CLEAR                   WFVF
024880050111     C     arbncl        IFGT      ARBNCR
024890970418     C                   MOVEL     'Z'           WFVF                           VOLUME PARZ.
024900970418     C                   ELSE
024910970418     C                   MOVEL     'T'           WFVF                           VOLUME TOTAL
024920970418     C                   ENDIF
024930051129      * Determino se il peso VDL è parziale o totale
024940030122     c                   Clear                   VidTpk
024950030123     c                   Clear                   wfpf
024960030122     c                   If        ArbNcp > *Zeros
024970050111     c                   If        arbncl > ArbNcp
024980030122     c                   Eval      VidTpk = 'Par.'                              Peso Parziale
024990030123     c                   Eval      wfpf = 'Z'
025000030122     c                   Else
025010030122     c                   Eval      VidTpk = 'Tot.'                              Peso Totale
025020030123     c                   Eval      wfpf = 'T'
025030030122     c                   EndIf
025040030122     c                   EndIf
025050030729      * Recupero i Bancali
025060030729     c                   If        %Subst(ArbGva:1:1) = 'B'
025070030729     c                   Clear                   dAr5Ban
025080030729     c                   Eval      kAr5Trd = 'BAN'
025090030729     c     kAr5          Chain(N)  Fiar501l
025100030729     c                   If        %Found(Fiar501l)
025110030729     c                   Movel     Ar5Uni        dAr5Ban
025120030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
025130030729     c                   EndIf
025140030729     c                   EndIf
025150030729      * Recupero i Bancali e i colli originali
025160030729     c                   If        %Subst(ArbGva:1:1) = 'O'
025170030729     c                   Clear                   dAr5Bnb
025180030729     c                   Eval      kAr5Trd = 'BNB'
025190030729     c     kAr5          Chain(N)  Fiar501l
025200030729     c                   If        %Found(Fiar501l)
025210030729     c                   Movel     Ar5Uni        dAr5Bnb
025220030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
025230030729     c                   Z-Add     §Ar5bCor      V1cCor
025240030729     c                   Eval      *In70 = *On
025250030729     c                   EndIf
025260030729     c                   EndIf
025270130924      * Memorizzo se presente avviso al destinatario per passarlo alla tassazione
025280110629     c                   clear                   wemd
025290130924     c                   Clear                   dAr5emd
025300110629     c                   Eval      kAr5Trd = 'EMD'
025310130924     c     kAr5          chain(N)  Fiar501l
025320130924     c                   if        %found(fiar501l)
025330130924     c                   Movel     Ar5Uni        dAr5EMD
025340130930     c                   eval      wesar5EMD='S'
025350130924     c                   select
025360130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp=' '
025370110629     c                   eval      wemd='S'
025380130924     c                   when      emd_§ar5mmp='S' and emd_§ar5smp='S'
025390130924     c                   eval      wemd='E'
025400130924     c                   when      emd_§ar5smp='S'
025410130924     c                   eval      wemd='X'
025420130924     c                   endsl
025430110629     c                   endif
025440061107     C**
025450071022     C* F N A R B D 0 F  record del codice fiscale e partita IVA
025460061107     C**
025470061107    1C     VIDCVR        IFEQ      'FI'
025480071022     c*
025490061129     c* Memorizzo se bolla già trasmessa o meno
025500070528     c*  solo in partenza(in arrivo è per forza trasmessa
025510151222     c  n10              movel     arbft2        bollaft2          1
025520151222     c**                 if        wfile='P'
025530151222     c**                 movel     arbft2        bollaft2          1
025540151222     c***                endif
025550071022     c* Dati per partita IVA
025560071022     c
025570071025     c                   clear                   ar4not_P
025580071022     C                   MOVEL     'P'           WTRC
025590061107     C     KARB2         CHAIN(N)  FiAR4000                           30
025600071025     c  n30              movel     ar4not        ar4not_p
025610071022     C                   MOVEL     'Q'           WTRC
025620071022     C     KARB2         CHAIN(N)  FiAR4000                           30
025630061107     c
025640061107     c* Verifico se bolla singola .Se doppia guardo se sono in partenza
025650061107     c*  o arrivo
025660061107     c* carico il mittente se bolla franco in partenza sempre
025670061107     c*  o franco singola in arrivo
025680061107    2c                   if        (flgtb1='F'  and not *in10) or
025690061107     c                             (flgtb1='F'  and §3atb2=*blanks)
025700061107     c                   eval      v11mide='M'
025710061107     c* Mittente
025720061107     c                   eval      v11decmd=CMITT
025730061107     c                   eval      v11cod=arbksc
025740061107     c                   eval      v11rsc=arbrsm
025750061107     c                   eval      wnar=arbnzm
025760071025     c* codice fiscale
025770061107     c                   if        not *in30 and not *in20
025780061107     c                   movel     ar4not        v11cdfis
025790061107     c                   else
025800061107     c                   clear                   v11cdfis
025810061107     c                   endif
025820071025     c* Partita IVA
025830071107     c****               if        not *in20
025840071025     c                   if        %subst(ar4not_P:1:16)<>*blanks
025850071025     c                   movel     ar4not_P      v11pid
025860071025     c                   else
025870071025     c                   movel     arbcpi        v11pid
025880071025     c                   endif
025890071107     c****               endif
025900071025     c
025910061107     c* In arrivo di fatto non posso inserire cod.fiscale su un franco
025920061107     c*    singolo: proteggo il campo
025930061107    3c                   if        *in10 and §3atb2=*blanks
025940061107     c                   seton                                        76
025950061107    3c                   endif
025960061107    2c                   endif
025970061107     c
025980061107     c* Carico il destinatario in arrivo sempre, in partenza se singola
025990061107    2c                   if        (flgtb1='A') or
026000061107     c                             (flgtb1='F'  and §3atb2<>*blanks and
026010061107     c                             *in10)
026020061107     c                   eval      v11mide='D'
026030061107     c                   eval      v11decmd=Cdest
026040061107     c                   eval      v11rsc=arbrsd
026050061107     c                   eval      wnar=arbnzd
026060061107     c                   if        not *in30 and not *in20
026070061107     c                   move      ar4not        v11cdfis
026080061107     c                   else
026090061107     c                   clear                   v11cdfis
026100061107     c                   endif
026110071025     c* Partita IVA
026120071107     c*****              if        not *in20
026130071025     c                   if        %subst(ar4not_P:20:16)<>*blanks
026140071026     c                   move      ar4not_P      v11pid
026150071025     c                   else
026160071107     c                   movel     arbcpi        v11pid
026170071025     c                   endif
026180071107     c*****              endif
026190071025     c* In partenza proteggo sempre
026200061107     c  n10              seton                                        76
026210061107     c* codice destinatario: dal fiar6 prima o seconda bolla
026220061107    3c                   if        §3atb2<>*blanks
026230061107     c                   eval      v11cod=ksc2bol
026240061107   x3c                   else
026250061107     C                   MOVEL     '1'           WTRC
026260061107     C     KARB2         CHAIN(N)  FIAR6000                           30
026270061107     c   30              eval      v11cod=arbksc
026280061107     c  n30              eval      v11cod=ar6ksc
026290061107    3c                   endif
026300061107    2c                   endif
026310061107     C*
026320061107     C* PGM RICHIAMATO
026330061107     c                   if        *in20
026340071025     c* in CPI c'e' il codice fiscale
026350061107     c                   movel     §bdcpi        v11cdfis
026360071107     c* in RSD c'e' la partita IVA solo dalla data attivazione
026370071203     c***                if        dateu>=§vpopiv
026380071025     c                   movel     §bdrsd        v11PID
026390071203     c***                endif
026400071107     c                   endif
026410071025     c
026420071025     c* controllo Partita IVA se privato
026430071025    2c                   if        v11pid<>*blanks
026440071025     C     'PRIVATO'     SCAN      v11pid        posiz                    30
026450071025     c* solo numeri
026460071025     c* "PRIVATO" senza iso
026470071025    3c     *in30         ifeq      *off
026480071025     c     v11isd        andge     *zeros
026490071025     c     *in30         oreq      *on
026500071025     c     posiz         andlt     3
026510071025     c                   movel     v11pid        w016a            16
026520071025     c                   movel     w016a         v11cpd
026530071025     C                   CLEAR                   V11isd
026540071025    3c                   endif
026550071025    2c                   endif
026560061107     c*
026570061107    1c                   endif
026580941019     C**
026590941027     C* F N A R B D 0 F
026600941019     C**
026610941019    1C     FLGCVR        IFEQ      'I'
026620941019     C*
026630941019    2C     VIDCVR        IFEQ      'I0'
026640140407    2C     VIDCVR        oreQ      'I7'
026650140407    2C     VIDCVR        oreQ      'I8'
026660140407    2C     VIDCVR        oreQ      'I9'
026670941019     C                   SETON                                        01          DESTINATAR
026680941019     C                   ELSE
026690941019     C                   SETON                                        02          PESO VOL
026700941212    2C                   END
026710941025     C*
026720941025     C* 2 RAGIONE SOCIALE DESTINATARIO
026730941212     C                   MOVEL     'D'           WTRC
026740060222     C     KARB2         CHAIN(N)  Fiar4000                           30
026750941212     C     *IN30         IFEQ      *OFF
026760941212     C                   MOVEL     'E'           WAR4D             1
026770941212     C* LO SALVO PER LA STORICIZZAZIONE
026780970520     C                   MOVE      AR4NOT        SAVRD2
026790941212     C                   ENDIF
026800061116     C*
026810061116     C* codice fiscale destinatario
026820061116     C                   MOVEL     'Q'           WTRC
026830061116     C     KARB2         CHAIN(N)  Fiar4000                           30
026840061116     C     *IN30         IFEQ      *OFF
026850061116     C                   MOVEL     'E'           WAR4Q
026860061116     C* Se c'e' il cod fiscale del detinatario lo memorizzo
026870061116     C                   MOVE      AR4NOT        vidcdf
026880061116     c                   if        vidcdf<>*blanks
026890061116     C                   MOVEL     'D'           WAR4Q
026900061116     C                   ENDIF
026910061116     C                   ENDIF
026920150206
026930150206     C* LOCALITA' NORMALIZZATA
026940150206     C                   MOVEL     'X'           WTRC
026950150206     C     KARB2         CHAIN(N)  Fiar4000                           30
026960150206     C     *IN30         IFEQ      *OFF
026970150206     C                   MOVEL     'E'           WAR4X             1
026980150206     C                   ENDIF
026990061116     c
027000051129     C* VISUALIZZO ANCHE IL VOLUME VDL
027010941214     C                   MOVEL     ARBVLC        V1CVLC
027020941214     C                   MOVEL     ARBNCR        V1CNCR
027030031120
027040040123      * Reperisco il referente e il telefono e magazzino giacenza
027050070910     c                   If        wesar5gen='S'
027060070910     c                   Eval      VidRef =  PAr5Ref
027070070910     c                   Eval      VidTeld = PAr5Teld
027080070910     c                   Eval      VidZng =  PAr5Zng
027090031120     c                   EndIf
027100141203     c                   eval      savzng = vidzng
027110941019     C*
027120941019     C* PGM NON RICHIAMATO
027130941019    2C     *IN20         IFEQ      *OFF
027140970520     C                   CLEAR                   VIDRSD
027150941025     C                   MOVEL     ARBRSD        VIDRSD
027160941027     C     WAR4D         IFEQ      'E'
027170061116     C                   MOVE      savrd2        VIDRSD
027180941025     C                   ENDIF
027190941019     C                   MOVEL     ARBIND        VIDIND
027200941019     C                   MOVEL     ARBLOD        VIDLOD
027210941019     C                   MOVEL     ARBPRD        VIDPRD
027220941019     C                   MOVEL     ARBCAD        VIDCAD
027230061115     c
027240140428     c*05 off se --> bolla singola in assegnato
027250140428     c*              bolla doppia
027260071025    3C     *IN05         IFEQ      *OFF
027270140428     c* se bolla doppia in partenza la partita iva che ho caricato non è quella del
027280140428     c* destinatario e quindi la carico solo se sono in arrivo (10 on)
027290151222     c***  *in10         ifeq      *on
027300151222     c     wfile         ifeq      'A'
027310140428     c     §3atb2        oreq      *blanks
027320941220     C                   MOVEL     ARBCPI        VIDPID
027330140428     c                   endif
027340071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027350050922     c* solo numeri
027360050922     c* "PRIVATO" senza iso
027370071025    4c**   *in30         ifeq      *off
027380071025     c**   vidisd        andge     *zeros
027390071025     c**   *in30         oreq      *on
027400071025     c**   posiz         andlt     3
027410071025     C**                 CLEAR                   VIDPID
027420071025     c**                 movel     arbcpi        vidcpd
027430071025    4c**                 endif
027440071025    3C                   ENDIF
027450941220     C*
027460030122     c                   Z-Add     ArbPkc        VidPkc
027470941027     C                   MOVEL     ARBPKF        VIDPKF
027480941027     C                   MOVEL     ARBVLF        VIDVLF
027490941027     C                   MOVEL     ARBFVF        VIDFVF
027500941019     C                   MOVEL     ARBFIN        VIDFIN
027510941019     C                   MOVEL     ARBNZD        VIDNZD
027520941019     C* FERMO DEPOSITO
027530941019    3C     ARBFFD        IFEQ      'S'
027540941019     C                   MOVEL     'SI'          VIDFFD
027550941019     C                   ELSE
027560941019     C                   CLEAR                   VIDFFD
027570941019    3C                   ENDIF
027580941019     C**
027590941019   X2C                   ELSE
027600941019     C**
027610941019     C* PROGRAMMA     RICHIAMATO: PRENDO CAMPI DA DSARBD
027620941019     C                   MOVEL     §BDRSD        VIDRSD
027630970520     C                   MOVE      §BDRD2        VIDRSD
027640941019     C                   MOVEL     §BDIND        VIDIND
027650941019     C                   MOVEL     §BDLOD        VIDLOD
027660941019     C                   MOVEL     §BDPRD        VIDPRD
027670941019     C                   MOVEL     §BDCAD        VIDCAD
027680941019     C                   MOVEL     §BDNZD        VIDNZD
027690060308     c                   movel     §bdref        vidref
027700060308     c                   movel     §bdtel        vidteld
027710061115     c                   clear                   vidcdf
027720071025    3C     *IN05         IFEQ      *OFF
027730950102     C                   MOVEL     §BDCPI        VIDPID
027740071025     C**   'PRIVATO'     SCAN      vidpid        posiz                    30
027750050922     c* solo numeri
027760050922     c* "PRIVATO" senza iso
027770071025    4c**   *in30         ifeq      *off
027780071025     c**   vidisd        andge     *zeros
027790071025     c**   *in30         oreq      *on
027800071025     c**   posiz         andlt     3
027810071025     C**                 CLEAR                   VIDPID
027820071025     c**                 movel     §bdcpi        vidcpd
027830071025    4c**                 endif
027840071025    3C                   ENDIF
027850941019     C                   MOVEL     §BDFIN        VIDFIN
027860941027     C                   MOVEL     §BDPKF        VIDPKF
027870941027     C                   MOVEL     §BDVLF        VIDVLF
027880941209     C                   MOVEL     §BDFVF        VIDFVF
027890941019     C* FERMO DEPOSITO
027900141017     c* §BDFFD= 'S' => si fermo deposito; Si alert
027910141017     c* §BDFFD= 'Y' -->si fermo deposito; no alert
027920141017     c* Serve per le chiamate cieche in quanto il campo a video per la gestione
027930141017     c* del contatto (VIDTFD) non è presente nella dsarbd.
027940941019    3C     §BDFFD        IFEQ      'S'
027950141017    3C     §BDFFD        oreq      'Y'
027960941019     C                   MOVEL     'SI'          VIDFFD
027970941019     C                   ELSE
027980941019     C                   CLEAR                   VIDFFD
027990941019    3C                   ENDIF
028000941216    2C                   ENDIF
028010141017      *
028020141017     c                   if        §bdffd='Y'
028030141017     c                   eval      vidtfd='SI'
028040141017     c                   endif
028050071025     c
028060071025     c* controllo Partita IVA se privato
028070071025    2c                   if        vidpid<>*blanks
028080071025     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028090071025     c* solo numeri
028100071025     c* "PRIVATO" senza iso
028110071025    3c     *in30         ifeq      *off
028120071025     c     vidisd        andge     *zeros
028130071025     c     *in30         oreq      *on
028140071025     c     posiz         andlt     3
028150071025     c                   movel     vidpid        w016a
028160071025     c                   movel     w016a         vidcpd
028170071025     C                   CLEAR                   VIDisd
028180071025    3c                   endif
028190071025    2c                   endif
028200991001     C*
028210991001    2C     *IN01         IFEQ      *ON
028220970808     C* MEMORIZZO CAP/PROVINCIA E LOCALITA' DEL DESTINATARIO
028230970808     C* (MI SERVIRA' NELLA ROUTINE DEI CONTROLLI
028240970808     C*
028250970808     C                   MOVE      VIDCAD        SAVCAD                         CAP
028260970808     C                   MOVE      VIDCAD        VARCAD                          "
028270970808     C                   MOVE      VIDCAD        WCAD                            "
028280970808     C                   MOVE      VIDCAD        WCAD1                           "
028290970808     C                   MOVE      VIDLOD        SAVLOD                         LOCALITA'
028300970808     C                   MOVE      VIDLOD        VARLOD                            "
028310970808     C                   MOVE      VIDLOD        WLOD                              "
028320970808     C                   MOVE      VIDLOD        WLOD1                             "
028330060111     c* memorizzo anche gli altri dati del destinatario
028340060111     c                   move      vidrsd        savrsd
028350061116     c                   move      vidrsd        savrsdQ
028360060111     c                   move      vidrsd        wrsd1
028370060111     c                   move      vidind        savind
028380060111     c                   move      vidind        wind1
028390060111     c                   move      vidprd        savprd
028400060111     c                   move      vidprd        wprd1
028410060111     c                   move      vidnzd        savnzd
028420060111     c                   move      vidnzd        wnzd1
028430970808     C*
028440950419     C* SE P.IVA DEL VIDEO = BLANKS LA PRENDO DALL'ESTENSIONE SE
028450950419     C* ESISTE
028460950419     C     VIDPID        IFEQ      *BLANKS
028470950419     C                   MOVEL     'P'           WTRC
028480060222     C     KARB2         CHAIN(N)  Fiar401L                           30
028490950419     C     *IN30         IFEQ      *OFF
028500950419     C                   MOVE      AR4NOT        VIDPID
028510050922     C     'PRIVATO'     SCAN      vidpid        posiz                    30
028520050922     c* solo numeri
028530050922     c* "PRIVATO" senza iso
028540050922     c     *in30         ifeq      *off
028550050922     c     vidisd        andge     *zeros
028560050922     c     *in30         oreq      *on
028570050922     c     posiz         andlt     3
028580050922     C                   CLEAR                   VIDPID
028590050922     c                   move      ar4not        wpiva
028600050922     C                   MOVEL     WPIVA         VIDCPD
028610050922     c                   endif
028620050922     C**   VIDISD        IFGE      *ZEROS
028630050922     C**                 CLEAR                   VIDPID
028640050922     C**                 MOVE      AR4NOT        WPIVA
028650050922     C**                 MOVEL     WPIVA         VIDCPD
028660050922     C**                 ENDIF
028670950419     C                   END
028680950419     C                   END
028690950419     C*
028700160118     C                   SETOFF                                       717276
028710991001     C* SE ASSEGNATO 9999 GIA' CONTABILIZZATO PROTEGGO DATI
028720991001     C* DESTINATARIO (ANCHE LA P.IVA MA SOLO SE DIVERSA DA BLANKS)
028730991001     C                   MOVE      ARBKSC        CO1KSC            4 0
028740991001    3C     CO1KSC        IFEQ      9999
028750991001     C                   CLEAR                   AR6DFT
028760991001     C                   MOVEL     '1'           WTRC
028770991001     C     KARB2         CHAIN(N)  FIAR6000                           30
028780991001    4C     AR6DFT        IFNE      *ZEROS
028790000104     C     AR6DFT        ANDLE     WDTU10
028800991001     C                   SETON                                        71
028810000104    5C     VIDPID        IFNE      *BLANKS
028820000104     C                   SETON                                        72
028830991001    5C                   END
028840991001    4C                   END
028850991001    3C                   END
028860160118     c* Proteggo la partita iva/cod.fiscale anche se dagli arrivi sto manutenzionando BLP
028870160115     c                   if        *in10 and wfile='P'
028880160118     C                   SETON                                        7276
028890160115     c                   endif
028900991001    2C                   END
028910941019     C*
028920941222     C* PROTEGGO VOLUME SE NON VARIABILE
028930941216     C* SE SI TRATTA DI VARIAZIONE DI PESO, PROTEGGO SEMPRE IL VOLUME
028940941216    2C     D48CVB        IFEQ      'I2'
028950941216     C* PROTEGGO IL PESO
028960941216     C                   SETON                                        88
028970941216     C*
028980941019     C                   MOVEL     '7T'          COD
028990051214     C                   MOVEL(P)  VIDFVF        KEY
029000941212     C                   EXSR      CTABEL
029010941212     C   30              SETON                                        89
029020941222     C     *IN30         IFEQ      *OFF
029030941222     C                   MOVEL     TBLUNI        DS7T
029040091106     c
029050151222     c                   if        *in10
029060091106     c                   eval      wvariavol=§7tvra
029070120705     c                   eval      wvariavolEXP=§7tvraEXP
029080091106     c                   else
029090091106     c                   eval      wvariavol=§7tvrp
029100120705     c                   eval      wvariavolEXP=§7tvrPEXP
029110091106     c                   endif
029120091106     c
029130091106     c* Proteggo il volume in arrivo o in partenza se il flag è ' ' oppure
029140091124     c*  non corrisponde il tipo bolla
029150091106     c                   select
029160091106     c                   when      wvariavol=' '
029170091106     C                   SETON                                        89
029180091106     c                   when      Wvariavol='A' and flgtb1='F'
029190091106     C                   SETON                                        89
029200091106     c                   when      Wvariavol='F' and flgtb1='A'
029210091106     C                   SETON                                        89
029220091106     c                   endsl
029230091106     c
029240120705     C* Se  BOLLA EXPORT,  in aggiunta controllo il relativo flag
029250120705     c                   if        *in89 and (lnantw='FED' or
029260120705     c                                        lnantw='EEX' or lnantw='DPD')
029270120705     c                   select
029280120705     c                   when      WvariavolEXP='3'
029290120705     C                   SETOff                                       89
029300120705     c                   when      WvariavolEXP='A' and flgtb1='A'
029310120705     C                   SETOff                                       89
029320120705     c                   when      Wvariavolexp='F' and flgtb1='F'
029330120705     C                   SETOff                                       89
029340120705     c                   endsl
029350120705     c                   endif
029360091106     c
029370941222    3C                   ENDIF
029380941216     C*
029390941216   X2C                   ELSE
029400941216     C                   SETON                                        89
029410941216    2C                   ENDIF
029420030604
029430030604      * se linea arrivo FedEx e il volume è "T" proteggo sempre
029440030604      * (già da tabella "7T" il volume di tipo "T" non è sostituibile,
029450030604      *  faccio lo stesso la modifica nel caso di variazione della tabella)
029460030604     c                   If        *In02 and LnaNtw = 'FED' and vidfvf = 'T'
029470030604     c                   Eval      *In89 = *On
029480030604     c                   EndIf
029490941019    1C                   ENDIF
029500130930     c
029510130930     C**
029520130930     C* F N A R B N 0 F
029530130930     C**
029540130930    1C     FLGCVR        IFEQ      'N'
029550130930
029560130930      * Reperisco il referente e il telefono e magazzino giacenza
029570130930    2c                   if        not  *in20
029580130930
029590130930    3c                   if        d48cvb='NE'
029600131001     c                   seton                                        868483
029610130930     c                   If        wesar5gen='S'
029620130930     c                   Eval      VidRef =  PAr5Ref
029630130930     c                   Eval      VidTeld = PAr5Teld
029640130930     c                   EndIf
029650130930     c                   if        wesar5emd='S'
029660130930     c                   eval      videml = emd_§ar5eml
029670130930     c                   eval      vidtsms= emd_§ar5tel
029680130930     c                   endif
029690130930    3c                   endif
029700130930
029710130930    3c                   if        d48cvb='NT'
029720131001     c                   seton                                        81
029730130930     c                   eval      vidnt1=v1cnt1
029740130930     c                   eval      vidnt2=v1cnt2
029750130930    3c                   endif
029760130930     c
029770130930   x2c                   else
029780130930    3c                   select
029790130930     c
029800131002     c                   when      §bntrd='NOT'
029810131002     c                   if        §bnu_sn='S'
029820131001     c                   seton                                        81
029830130930     c                   movel     §bnuni70      vidnt1
029840130930     c                   move      §bnuni70      vidnt2
029850131002     c                   else
029860131002     C                   MOVEL     MSG(24)       D48MSG
029870131002     C                   MOVEL     '1'           D48ERR
029880131002     c                   leavesr
029890131002     c                   endif
029900131008     c* se la causale variazione non è NT --> errore
029910131008     c                   if        d48cvb<>'NT'
029920131008     C                   MOVEL     MSG(31)       D48MSG
029930131008     C                   MOVEL     '1'           D48ERR
029940131008     c                   leavesr
029950131008     c                   endif
029960131008
029970130930     c                   other
029980131008
029990131008     c* se la causale variazione non è NE --> errore
030000131008     c                   if        d48cvb<>'NE'
030010131008     C                   MOVEL     MSG(31)       D48MSG
030020131008     C                   MOVEL     '1'           D48ERR
030030131008     c                   leavesr
030040131008     c                   endif
030050130930     c
030060131001     c                   if        §bnu_sn='S'
030070130930     c                   eval      videml = §bnuni70
030080131001     c                   seton                                        86
030090131001     c                   endif
030100131001     c                   if        §bnr_sn='S'
030110131001     c                   eval      vidteld= §bntel
030120130930     c                   eval      vidref = §bnref
030130131001     c                   seton                                        84
030140131001     c                   endif
030150131001     c                   if        §bnc_sn='S'
030160131001     c                   eval      vidtsms= §bncel
030170131001     c                   seton                                        83
030180131001     c                   endif
030190131002     c                   if        *in86=*off and *in84=*off and *in83=*off
030200131002     C                   MOVEL     MSG(24)       D48MSG
030210131002     C                   MOVEL     '1'           D48ERR
030220131002     c                   leavesr
030230131002     c                   endif
030240131001     c
030250130930    3c                   endsl
030260130930     c
030270130930    2c                   endif
030280130930    1c                   endif
030290950104     C**
030300990930     C* F I A R B T 0 F
030310950104     C**
030320100120     c
030330941019    1C     FLGCVR        IFEQ      'T'
030340941019     C                   SELECT
030350941019     C*
030360941102     C     D66RTA        WHENEQ    'S'
030370950616     C                   SETON                                        1529        RITASSAZ
030380941102     C*
030390941102     C     D66BSF        WHENEQ    'S'
030400941019     C                   SETON                                        13          VARIA  SF
030410941019     C*
030420941102     C     D66BFI        WHENEQ    'S'
030430991006     C                   SETON                                        1407        VARIA  FAT
030440941019     C*
030450941019     C                   OTHER
030460941019     C                   SETON                                        36          TASS FRANC
030470941019     C                   ENDSL
030480941102     C* PULIZIA SFL
030490941102     C                   SETON                                        85
030500941215     C                   WRITE     LR48C03
030510941102     C                   SETOFF                                       85
030520941215     C*
030530941215     C                   MOVEL     *BLANKS       DESKSC
030540950104     C   10              MOVEL     ARBRSD        W008A             8
030550950104     C   10              MOVEL     W008A         DESKSC
030560950104     C  N10              MOVEL     ARBRSM        DESKSC
030570131120     c   10              clear                   sav_ift
030580941102     C*
030590941102     C*
030600990930     C* TASSAZIONE IN FIAR6
030610990930     C                   MOVEL     '1'           WTRC
030620990930     C     KARB2         CHAIN     FIAR6000                           30
030630990702    2C     *IN30         IFEQ      *OFF
030640990701     C                   MOVEL     'E'           WAR6
030650990701     C                   MOVEL     AR6NFT        VIDNFT
030660990701     C                   MOVEL     AR6FIV        VIDFIV
030670990702     C                   Z-ADD     AR6IMV        SAVIMV
030680151016     C                   Z-ADD     AR6IMV        SAVIMV_fat
030690131120     c   10              z-add     ar6ift        sav_ift
030700990702     C* PROTEZIONE DATI FATTURA PER PREPAGATI
030710990702    3C  N10VIDNFT        IFLT      §QTNFT
030720990719     C     VIDNFT        ANDGT     0
030730990701     C                   SETON                                        23
030740990702     C                   Z-ADD     AR6IFT        VIDIFT
030750990702    3C                   ENDIF
030760991001     C* PROTEZIONE DIVISA PER PREPAGATI
030770991001    3C  N10VIDNFT        IFGT      0
030780991012     C                   SETON                                        0708
030790991001    3C                   ENDIF
030800991108     C* SE PREPAGATO O P.A. FATTURA GIA'CONTAB. MODIFICO SOLO IMP.ASSIC
030810991008     C* E VALORE MERCE DICHIARATO
030820991108    3C     AR6DFT        IFGT      *ZEROS
030830000104     C     AR6DFT        ANDLE     WDTU10
030840991008     C                   SETON                                        2973
030850991008    3C                   ENDIF
030860991029    2C                   ENDIF
030870991029     C**
030880991029     C* SE ASSEGNATO GIA' INCASSATO, FACCIO MODIFICARE SOLO IMP.ASSIC EARIAZIONE
030890991029     C*  VALORE MERCE DICHIARATO
030900130409    1C*  10FLGCVR        IFEQ      'T'
030910130409    2C*    §7LFIN        IFEQ      'S'
030920130409     C*    ARBICA        ANDNE     ' '
030930130409     C*    ARBICA        ANDNE     'R'
030940991029     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
030950130409     C*                  SETON                                        2973
030960130409    2C*                  END
030970991108     C* SE ICA = 'R' E C'E' INCASSO PARZ<IALE IN ARI, PROTEGGO
030980991108     C*  LO STESS, FACCIO SOLO MODIFICARE IAS
030990130409    2C*    ARBICA        IFEQ      'R'
031000130409     C*    KARI          CHAIN     FIARI01L                           30
031010130409    3C* N30ARIATB        IFEQ      ' '
031020991108     C* PROTEGGO I CAMPI CHE NON DEVONO ESSERE MANUTENZIONATI
031030130409     C*                  SETON                                        2973
031040130409    3C*                  ENDIF
031050130409    2C*                  ENDIF
031060130409    1C*                  ENDIF
031070130410     c* PROTEZIONE CAMPI PER POTER VARIARE SOLO IAS E VMD
031080130410     c* Se bolla già contabilizzata e variabile anche dopo contabilizzazione
031090130410     c* oppure se bolla già incassata e variabile anche dopo incasso (V.di fnlr66r)
031100130410     c                   if        d66bdi='S'
031110130409     C                   SETON                                        2973
031120130409     c                   endif
031130990701     C**
031140950807     C* VEDO SE ESISTE AMCHE L'AR7
031150990930     C     KARB2         SETLL     FIAR7000                               30
031160950807     C   30              MOVEL     'E'           WAR7
031170060201     c
031180060201      * Calcolo data limite mod. importo da assicurare
031190060201     c     *iso          Move      ArbDsp        wdataiso
031200060201     c                   adddur    §vpomia:*d    wdataiso
031210060201      *
031220060201     c                   do        *hival
031230060201      * verifico se è un giorno lavorativo
031240060201     c     *iso          move      wdataiso      ds_data
031250060201     C                   eval      kann = ds_anno
031260060201     C                   eval      kmes = ds_mese
031270060201     C     kazcln        chain     azcln01l
031280060201     C                   if        %found(azcln01l)
031290060201     C                   if        MAT(ds_giorno) =  'F'  or
031300060201     C                             POM(ds_giorno) =  'F'
031310060201      * se non va bene aggiungo 1 giorno
031320060201     c                   adddur    1:*d          wdataiso
031330060201     C                   iter
031340060201     c                   else
031350060201     c                   leave
031360060201     C                   endif
031370060201     c                   else
031380060201     c                   leave
031390060201     C                   endif
031400060201
031410060201     c                   enddo
031420060201
031430060201     c                   eval      wdatalimias=wdataiso
031440080625     c                   clear                   forzaias2
031450080625     c                   clear                   forzaias
031460060201     c
031470060201     c* IAS modificabile solo nei primi 2 gg  tassaz sempre
031480060201     c                   if        §utemtf=' '  and
031490060201     c                             wdataoggi>wdatalimIas
031500060201     c                   seton                                        75
031510060201     c                   endif
031520060201     c* solo IAs ma sempre
031530060201     c                   if        §utemtf='I'
031540060201     c                   seton                                        74
031550060201     c                   endif
031560060201     c* Nulla
031570060201     c                   if        §utemtf='N'
031580060201     c                   seton                                        7475
031590060201     c                   endif
031600991012     C*
031610060201     C*
031620060201     C                   CLEAR                   V3CEV1
031630060201     C                   CLEAR                   V3CSV1
031640060201     C                   CLEAR                   V3CVA1
031650060201     C                   CLEAR                   VARVA1
031660060201     C                   Z-ADD     1             NRR
031670060201     C                   DO        20            NRR
031680060201     C                   WRITE     LR48S03
031690060201     C                   ENDDO
031700920128     C* PROGRAMMA NON RICHIAMATO: DATI DA BOLLA
031710941019    2C     *IN20         IFEQ      *OFF
031720941019     C*
031730941019     C                   Z-ADD     ARBIAS        VIDIAS
031740950426     C                   Z-ADD     ARBIAS        COMIAS
031750941209     C                   MOVEL     ARBVAS        VIDVAS
031760941215     C                   MOVEL     ARBVAS        VARVAS
031770941019     C                   MOVEL     ARBKSC        VIDKLP
031780950424     C                   MOVEL     ARBKSC        VARKLP
031790941019     C                   MOVE      ARBKSC        VIDKSC
031800970822     C                   MOVE      ARBKSC        MEMKSC
031810950424     C                   MOVE      ARBKSC        VARKSC
031820941019     C                   MOVEL     ARBCTR        VIDCTR
031830050202     C                   MOVEL     ARBCTR        SAVCTR
031840060511     C                   MOVEL     ARBCTR        CTRBOLLA          3
031850941215     C                   MOVEL     ARBQFT        VIDQFT
031860950424     C                   MOVEL     ARBQFT        VARQFT
031870941215     C                   MOVEL     ARBVMD        VIDVMD
031880941215     C                   MOVEL     ARBVAD        VIDVAD
031890941215     C                   MOVEL     ARBVAD        VARVAD
031900990930     C* TASSAZIONE IN FIAR6
031910941102    3C     WAR6          IFEQ      'E'
031920990930     C                   MOVEL     AR6DIV        VIDDIV
031930991006     C                   MOVEL     AR6DIV        WDIV
031940941019     C                   MOVEL     AR6POR        VIDPOR
031950991006     C                   MOVEL     AR6POR        VARPOR
031960941019     C                   MOVEL     AR6IMV        VIDIMV
031970941019     C                   MOVEL     AR6CEI        VIDCEI
031980941102     C*
031990941102     C                   Z-ADD     0             NRR
032000941209     C                   MOVEL     AR6SV1        WSV1
032010941209     C                   Z-ADD     AR6VA1        WVA1
032020941102     C                   EXSR      CARVAR
032030941209     C                   MOVEL     AR6SV2        WSV1
032040941209     C                   Z-ADD     AR6VA2        WVA1
032050941102     C                   EXSR      CARVAR
032060941212    4C     AR6SV3        IFNE      ' '
032070941209     C                   MOVEL     AR6SV3        WSV1
032080941209     C                   Z-ADD     AR6VA3        WVA1
032090941102     C                   EXSR      CARVAR
032100941102     C*
032110941102     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
032120990930     C     KARB2         READE(N)  FIAR7000                               30
032130941102    5C     *IN30         DOWEQ     *OFF
032140941209     C                   MOVEL     AR7SVN        WSV1
032150941209     C                   MOVEL     AR7VAN        WVA1
032160941102     C                   EXSR      CARVAR
032170990930     C     KARB2         READE(N)  FIAR7000                               30
032180941102    5C                   ENDDO
032190941102    4C                   ENDIF
032200941102    3C                   ENDIF
032210941019     C*
032220941019   X2C                   ELSE
032230920128     C*
032240990930     C* PROGRAMMA     RICHIAMATO: DATI DA DARBT
032250941025     C                   Z-ADD     §BTIAS        VIDIAS
032260950426     C                   Z-ADD     §BTIAS        COMIAS
032270941019     C                   MOVEL     §BTVAS        VIDVAS
032280941220     C                   MOVEL     §BTVAS        VARVAS
032290941209     C                   MOVEL     §BTQFT        VIDQFT
032300950424     C                   MOVEL     §BTQFT        VARQFT
032310941209     C                   MOVEL     §BTVMD        VIDVMD
032320941209     C                   MOVEL     §BTVAD        VIDVAD
032330941220     C                   MOVEL     §BTVAD        VARVAD
032340920128     C                   MOVEL     §BTKSC        VIDKLP
032350950424     C                   MOVEL     §BTKSC        VARKLP
032360920128     C                   MOVE      §BTKSC        VIDKSC
032370970822     C                   MOVE      §BTKSC        MEMKSC
032380950424     C                   MOVE      §BTKSC        VARKSC
032390920128     C                   MOVEL     §BTKSC        SAVKSC            7 0
032400060511     c                   clear                   kscbolla
032410920128     C                   MOVEL     §BTCTR        VIDCTR
032420050202     C                   MOVEL     §BTCTR        SAVCTR
032430060511     c                   clear                   ctrbolla
032440990930     C                   MOVEL     §BTDIV        VIDDIV
032450991006     C                   MOVEL     §BTDIV        WDIV
032460990930     C                   Z-ADD     §BTPOR        VIDPOR
032470991006     C                   Z-ADD     §BTPOR        VARPOR
032480990930     C                   Z-ADD     §BTIMV        VIDIMV
032490920128     C                   MOVEL     §BTCEI        VIDCEI
032500941102     C* CARICO LE VARIE
032510941102     C                   Z-ADD     0             NRR               5 0
032520941209     C                   DO        20            C                 3 0
032530941209     C                   MOVEL     §SV(C)        WSV1
032540941209     C                   Z-ADD     §VA(C)        WVA1
032550941102     C                   EXSR      CARVAR
032560941209     C                   ENDDO
032570941019    2C                   ENDIF
032580131016
032590131016     c                   z-add     vidias        bol_vidias
032600131016     c                   movel     vidvas        bol_vidvas
032610131016     c                   z-add     vidvmd        bol_vidvmd
032620131016     c                   movel     vidvad        bol_vidvad
032630131016     c
032640941215     C*
032650941215     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
032660050202     C                   Z-ADD     SAVKSC        KSC
032670941215     C                   EXSR      CARCTR
032680941019     C*
032690920128     C* DECODIFICA ESENZIONE IVA
032700920128     C                   MOVEL     *BLANKS       DESCEI
032710941019    2C     VIDCEI        IFNE      ' '
032720920128     C                   MOVEL     'EI'          COD
032730920128     C                   MOVEL     *BLANKS       KEY
032740051214     C                   MOVEL(P)  VIDCEI        KEY
032750941212     C                   EXSR      CTABEL
032760920128     C  N30              MOVEL     TBLUNI        DESCEI
032770941019    2C                   END
032780920128     C* GIRO DATA FATTURA
032790941102    1C     WAR6          IFEQ      'E'
032800941102     C     AR6DFT        ANDGT     0
032810941019     C                   Z-ADD     AR6DFT        G02INV
032820941019     C                   MOVEL     '3'           G02ERR
032830941019     C                   CALL      'XSRDA8'
032840941019     C                   PARM                    WLBDAT
032850941019     C*
032860941019     C                   Z-ADD     G02DAT        VIDDFT
032870941019    2C                   ENDIF
032880941019    1C                   ENDIF
032890941019     C**
032900991001     C* F I A R B T 0 F   SECONDA BOLLA
032910941019     C**
032920911211     C     FLGCVR        IFEQ      '2'
032930941102     C                   SELECT
032940941102     C     D66RTA        WHENEQ    'S'
032950950616     C                   SETON                                        1529        RITASSAZ
032960941102     C*
032970941102     C     D66BSF        WHENEQ    'S'
032980941102     C                   SETON                                        13          VARIA  SF
032990941102     C*
033000941102     C     D66BFI        WHENEQ    'S'
033010991006     C                   SETON                                        1407        VARIA  FAT
033020941102     C*
033030941102     C                   ENDSL
033040911211     C*
033050911219     C                   MOVEL     *BLANKS       DESKSC
033060941019     C                   MOVEL     ARBRSD        W008A             8
033070941019     C                   MOVEL     W008A         DESKSC
033080131120     c                   clear                   sav_ift
033090941019     C*
033100941019     C* PGM NON RICHIAMATO
033110941019    2C     *IN20         IFEQ      *OFF
033120920128     C* NON ESISTE ESTENSIONE BOLLA
033130990930    3C     WAR62         IFEQ      ' '
033140911211     C                   MOVEL     ARBLNA        VIDKLP
033150911211     C                   MOVEL     9999          VIDKSC
033160911212     C                   MOVEL     *ZEROS        VIDCTR
033170050202     C                   MOVEL     *ZEROS        SAVCTR
033180060511     c                   clear                   ctrbolla
033190991013     C                   MOVE      *ZEROS        SAVKSC            7 0
033200060511     c                   clear                   kscbolla
033210941019     C*
033220941019   X3C                   ELSE
033230941019     C*
033240131120     c                   z-add     ar6ift        sav_ift
033250990930     C                   MOVEL     AR6DIV        VI2DIV
033260020305     C                   MOVEL     AR6DIV        WDIV
033270990930     C                   MOVEL     AR6SV1        VI2SV1
033280040122     C**!!!              Z-ADD     AR6VA1        VI2VA1
033290040122     C**!!!              Z-ADD     AR6VA1        VA2VA1
033300040123      * carico le varie
033310040122     c                   Movel     Ar6Sv1        wsv1
033320040122     c                   Z-add     Ar6Va1        wva1
033330040122     c                   ExSr      CarVar1
033340040122     c                   Movel     Ar6Sv2        wsv1
033350040122     c                   Z-add     Ar6Va2        wva1
033360040122     c                   ExSr      CarVar1
033370040122     c                   Movel     Ar6Sv3        wsv1
033380040122     c                   Z-add     Ar6Va3        wva1
033390040122     c                   ExSr      CarVar1
033400040122      * Verifico se ci sono altre varie
033410040126     c                   Movel     '2'           wtrc
033420040123     c     Karb2         Setll     Fiar701l
033430040122Do  4c                   Do        *Hival
033440040123     c     Karb2         Reade(n)  Fiar701l
033450040122     c                   If        %Eof(Fiar701l)
033460040122     c                   Leave
033470040122     c                   EndIf
033480040122     c                   Movel     Ar7Svn        wsv1
033490040122     c                   Z-add     Ar7Van        wva1
033500040122     c                   ExSr      CarVar1
033510040122    4c                   EndDo
033520990930     C                   MOVEL     AR6CEI        VI2CEI
033530990930     C                   MOVEL     AR6KSC        VIDKLP
033540990930     C                   MOVE      AR6KSC        VIDKSC
033550991013     C                   MOVEL     AR6KSC        SAVKSC
033560060511     C                   MOVEL     AR6KSC        kscbolla
033570990930     C                   MOVEL     AR6CTR        VIDCTR
033580050202     C                   MOVEL     AR6CTR        SAVCTR
033590060511     C                   MOVEL     AR6CTR        ctrbolla
033600941019    3C                   END
033610941019     C*
033620941019   X2C                   ELSE
033630941019     C*
033640990930     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DARBT 2BOLLA
033650990930     C                   MOVEL     §BTDIV        VI2DIV
033660991006     C                   MOVEL     §BTDIV        WDIV
033670040123     C**!!!              MOVEL     §BTSV1        VI2SV1
033680040123     C**!!!              Z-ADD     §BTVA1        VI2VA1
033690040123     C**!!!              Z-ADD     §BTVA1        VA2VA1
033700040123      * carico le varie
033710040123     c                   Do        20            c
033720040123     c                   Movel     §sv(c)        wsv1
033730040123     c                   Z-add     §va(c)        wva1
033740040123     c                   ExSr      CarVar1
033750041130     c                   If        ContaSv = 6
033760040123     c                   Leave
033770040123     c                   EndIf
033780040123     c                   EndDo
033790040123
033800990930     C                   MOVEL     §BTCEI        VI2CEI
033810990930     C                   MOVEL     §BTKSC        VIDKLP
033820990930     C                   MOVE      §BTKSC        VIDKSC
033830991013     C                   MOVE      §BTKSC        SAVKSC
033840060511     c                   clear                   kscbolla
033850990930     C                   MOVEL     §BTCTR        VIDCTR
033860050202     C                   MOVEL     §BTCTR        SAVCTR
033870060511     c                   clear                   ctrbolla
033880941019    2C                   END
033890050202     c
033900050202     C* CARICO LE TARIFFE E DEOCIFICO IL CTR CHE C'E'I
033910050202     C                   Z-ADD     SAVKSC        KSC
033920050202     C                   EXSR      CARCTR
033930920128     C*
033940920128     C                   MOVEL     *BLANKS       DESCEI
033950941019    2C     VI2CEI        IFNE      ' '
033960911211     C                   MOVEL     'EI'          COD
033970911211     C                   MOVEL     *BLANKS       KEY
033980051214     C                   MOVEL(P)  VI2CEI        KEY
033990941212     C                   EXSR      CTABEL
034000911211     C  N30              MOVEL     TBLUNI        DESCEI
034010941019    2C                   END
034020941019     C* GIRO DATA FATTURA
034030990930    1C     WAR62         IFEQ      'E'
034040990930     C     AR6DFT        ANDGT     0
034050990930     C                   Z-ADD     AR6DFT        G02INV
034060941019     C                   MOVEL     '3'           G02ERR
034070941019     C                   CALL      'XSRDA8'
034080941019     C                   PARM                    WLBDAT
034090941019     C*
034100941019     C                   Z-ADD     G02DAT        VI2DFT
034110991007     C                   Z-ADD     AR6FIV        VI2FIV
034120991007     C                   Z-ADD     AR6NFT        VI2NFT
034130941019    2C                   ENDIF
034140911211     C                   END
034150941019     C**
034160050202     C* F N A R B K 0 F
034170941019     C**
034180941019    1C     FLGCVR        IFEQ      'K'
034190941019     C*
034200941019     C* CODICE BOLLA PRECEDENTE
034210941019     C                   MOVEL     ARBCBO        OLDCBO
034220941019     C* SALVO CAMPI DEL CODICE BOLLA CHE MI INTERESSANO
034230941019     C                   MOVEL     §3ATB1        SAVTB1            2
034240941102     C                   MOVEL     §3ARBL        SAVRBL            1
034250941019     C                   MOVEL     §3ATB2        SAVTB2            2
034260941019     C                   MOVEL     §3AFCA        SAVFCA            1 0
034270941019     C                   CLEAR                   DENCBO
034280050414     C*
034290050414     C* VERIFICO SE SI TRATTA DI UN PREPAGATO
034300050414     C                   MOVEL     '1'           WTRC
034310050414     C     KARB2         CHAIN(N)  FIAR6000                           30
034320050414     C  N30              MOVEL     'E'           WAR6
034330050414     C*
034340050414     C
034350920128     C*
034360920128     C* PROGRAMMA NON RICHIAMATO: PRENDO DATI DA BOLLA
034370941019    2C     *IN20         IFEQ      *OFF
034380941019     C*
034390941019    3C     WAR9          IFEQ      'E'
034400941019     C                   MOVEL     AR9TIC        VIDTIC
034410941019     C                   MOVEL     AR9CAS        VIDCAS
034420941019     C                   MOVEL     AR9VCA        VIDVCA
034430941215     C                   MOVEL     AR9VCA        VARVCA
034440941102     C                   MOVEL     AR9GCA        VIDGCA
034450941019    3C                   ENDIF
034460941019     C*
034470941019     C                   MOVEL     '?'           NEWCBO
034480920128     C*
034490941019   X2C                   ELSE
034500920128     C*
034510920128     C* PROGRAMMA     RICHIAMATO: PRENDO DATI DA DSARBK
034520920128     C                   MOVEL     §BKTIC        VIDTIC
034530941019     C                   MOVEL     §BKCAS        VIDCAS
034540941102     C                   MOVEL     §BKGCA        VIDGCA
034550941019     C                   MOVEL     §BKCBN        NEWCBO
034560941220     C                   MOVEL     §BKVCA        VARVCA
034570941220     C                   MOVEL     §BKVCA        VIDVCA
034580950420     C* SE NON PASSATO IMPOSTO QUELLO DELLA BOLLA
034590950420     C     NEWCBO        IFEQ      *BLANKS
034600950420     C                   MOVEL     ARBCBO        NEWCBO
034610950420     C                   ENDIF
034620920128     C*
034630920128     C* DECODIFICA NUOVO CODICE BOLLA
034640920128     C                   MOVEL     '3A'          COD
034650950420     C                   MOVEL(P)  NEWCBO        KEY
034660941212     C                   EXSR      CTABEL
034670920128     C  N30              MOVEL     TBLUNI        DS3A
034680941019     C   30              CLEAR                   DS3A
034690941102     C                   MOVEL     §3ADES        DENCBO
034700941019    2C                   END
034710941019     C*
034720920325     C* VARIAZIONE IMPORTO CONTRASSEGNO
034730941019    2C     VIDCVR        IFEQ      'KA'                                          UFFICIO
034740931118     C     VIDCVR        OREQ      'KB'                                          DOPO CONS
034750931118     C     VIDCVR        OREQ      'KP'                                          PADRONCINO
034760920325     C                   SETON                                        21
034770941102     C                   MOVEL     ARBCBO        NEWCBO
034780941019    2C                   END
034790941019    1C                   END
034800941019     C**
034810070309     C* F N A R B G 0 F
034820941019     C**
034830950120     C     FLGCVR        IFEQ      'C'
034840950120     C* SE E' CAUSALE CP PROTEGGO I CAMPI DELLA CONSEGNA RICHIESTA E
034850950120     C*  GG CHIUSURA
034860950120     C     VIDCVR        IFEQ      'CP'
034870950120     C                   SETON                                        16
034880950120     C                   ENDIF
034890941019     C* PGM NON RICHIAMATO
034900941019     C     *IN20         IFEQ      *OFF
034910920320     C* DATA CONSEGNA RICHIESTA
034920941019     C                   Z-ADD     ARBDCR        G02INV
034930920320     C*
034940920320     C                   Z-ADD     ARBHCR        VIDHCR
034950920320     C*
034960920320     C                   MOVEL     ARBTCR        VIDTCR
034970930510     C                   MOVEL     ARBGC1        VIDGC1
034980930510     C                   MOVEL     ARBGC2        VIDGC2
034990941019     C                   MOVEL     ARBTC1        VIDTC1
035000941102     C                   MOVEL     ARBTC2        VIDTC2
035010111025     c* Se il pgm non è richiamato, verifico se nella DS nel campo tipo §BGTCR
035020111025     c*  c'e' una "S" --> significa che posso aggiornare la consegna richiesta
035030111025     c*  in FIARP se bolla in distinta (solo da gestione telefonate o da richiamo
035040111025     c*  con passaggio dati) In caso contrario non posso farlo e sid eve dare errore
035050111207     c* ES - 07/12/11 -  per il momento asterisco
035060111207     c****               eval      Dcrindist=§BGTCR
035070930510     C*
035080941019   X2C                   ELSE
035090930510     C*
035100941019     C                   Z-ADD     §BGDCR        G02INV
035110930510     C*
035120930510     C                   Z-ADD     §BGHCR        VIDHCR
035130930510     C*
035140930510     C                   MOVEL     §BGTCR        VIDTCR
035150930510     C                   MOVEL     §BGGC1        VIDGC1
035160930510     C                   MOVEL     §BGGC2        VIDGC2
035170941019     C                   MOVEL     §BGTC1        VIDTC1
035180941019     C                   MOVEL     §BGTC2        VIDTC2
035190111025     c                   eval      Dcrindist='S'
035200920320     C                   END
035210941019     C*
035220941019     C* GIRO DATA CONSEGNA RICHIESTA
035230941019     C                   MOVEL     '3'           G02ERR
035240941019     C                   CALL      'XSRDA8'
035250941019     C                   PARM                    WLBDAT
035260941019     C                   Z-ADD     G02DAT        VIDDCR
035270070309     c
035280070309     c* Se lna con GEO attiva, verifico per eventuale modifica consegna ric
035290070309     c*  se c'e' sulla bolla un evento per il quale è inseribile la
035300070319     c*  modifica della data consegna richiesta
035310070319     c                   if        FGSgeot='S'
035320070309     c     karb          setgt     fnevb05l
035330070309     c     karb          readpe    fnevb05l
035340070309     c                   dow       not %eof(fnevb05l) and eveADR=' '
035350090526     c* controllo a seconda che sia o meno richiamato il pgm
035360090526     c  n20evbcev        lookup    ADRs                                   30
035370090526     c   20evbcev        lookup    ADRC                                   30
035380070309     c                   if        *in30
035390070309     c                   eval      eveADR='S'
035400070309     c                   else
035410070309     c     karb          readpe    fnevb05l
035420070309     c                   endif
035430070309     c                   enddo
035440070309     c
035450070309     c                   endif
035460070309     c
035470930510     C                   END
035480020531     c* se modificato destinatario in disagiato o supermercato imposto il
035490020531     c* relativo flag nelle cosegne particolari
035500041015     c* Lo faccio senza testare se arbgva = "U" perchè se mi trovo in
035510041015     c* questa condizione è certo che arbgva è <> da "U"
035520070907     c**   wdisag        ifne      *blanks
035530070907     c**                 select
035540070907     c**   vidtc1        wheneq    *blanks
035550070907     c**                 movel     wdisag        vidtc1
035560070907     c**   vidtc2        wheneq    *blanks
035570070907     c**                 movel     wdisag        vidtc2
035580070907     c**                 endsl
035590070907     c**   vidtc1        ifeq      vidtc2
035600070907     c**                 clear                   vidtc2
035610070907     c**                 endif
035620070907     c**                 endif
035630950104     C**
035640070309     C* F N A R B M 0 F
035650950104     C**
035660950104     C     VIDCVR        IFEQ      'MI'
035670950104     C* PGM NON RICHIAMATO: PER ORA NON ESISTE IL RICHIAMO
035680950104     C* DECODIFICO TIPO TARIFFA
035690950104     C                   MOVEL     ARBCTR        VIDCTR
035700050202     C                   MOVEL     ARBCTR        SAVCTR
035710060511     C                   MOVEL     ARBCTR        ctrbolla
035720050202     C                   CLEAR                   Fnlv59DS
035730050202     C                   MOVEL     VIDCTR        ILV59CTR
035740050202     C                   MOVEL     'S'           ILV59CFO
035750050202     C                   CALL      'FNLV59R'
035760050202     C                   PARM                    Fnlv59DS
035770950104     C*
035780050202     C     OLV59ERR      IFEQ      ' '
035790050202     C                   MOVEL     OLV59DFS      DE2CTR
035800950104     C                   ENDIF
035810950104     C                   MOVEL     ARBKSC        VIDKLP
035820950104     C                   MOVE      ARBKSC        VIDKSC
035830970822     C                   MOVE      ARBKSC        MEMKSC
035840950104     C                   MOVEL     ARBRSM        W008A
035850950104     C                   MOVEL     W008A         DESKSC
035860051220     c                   clear                   memntw
035870051220     c     vidklp        chain     azorg01l
035880051220     c                   if        %found(azorg01l)
035890051220     C                   MOVEL     ORGDE3        OG143
035900051220     C                   MOVEL     §OGNTW        memntw
035910051220     c                   endif
035920160225     c* Se lna fedex --> carico le tariffe di cartello FED
035930160225     c
035940970813     C* SE CODICE 8888 CARICO L'INDIRIZZO COMPLETO
035950950104     C     VIDKSC        IFEQ      8888
035960970813     C                   SETOFF                                       22
035970950104     C                   MOVEL     ARBRSM        VIDRSM
035980950104     C                   MOVEL     ARBINM        VIDINM
035990950104     C                   MOVEL     ARBCAM        VIDCAM
036000950104     C                   MOVEL     ARBLOM        VIDLOM
036010970808     C                   MOVEL     ARBCAM        VARCAM
036020970808     C                   MOVEL     ARBLOM        VARLOM
036030950104     C                   MOVEL     ARBPRM        VIDPRM
036040950104     C                   MOVEL     ARBNZM        VIDNZM
036050950104     C                   MOVEL     ARBFAP        VIDFAP
036060950104     C                   MOVEL     ARBCTR        VIDCTR
036070950104     C                   MOVEL     ARBCPI        VIDPIM
036080050922     C     'PRIVATO'     SCAN      vidpim        posiz             2 0    30
036090050922     c* solo numeri
036100050922     c* "PRIVATO senza iso
036110050922     c     *in30         ifeq      *off
036120050922     c     vidism        andge     *zeros
036130050922     c     *in30         oreq      *on
036140050922     c     posiz         andlt     3
036150050922     c                   clear                   vidpim
036160050922     c                   movel     arbcpi        vidcpm
036170050922     c                   endif
036180970813     C                   ELSE
036190970813     C                   SETON                                        22
036200950104     C                   ENDIF
036210950104     C                   ENDIF
036220961211     C**
036230970110     C* VARIAZIONE DEL RIFERIMENTO MITTENTE ALFABETICO E NATURA MERCE
036240961211     C**
036250961211     C     D48CVB        IFEQ      'RM'
036260970110     C                   MOVEL     ARBRMA        VIDRMA
036270970110     C                   MOVEL     ARBNAS        VIDNAS
036280961211     C                   END
036290950104     C*
036300011018      * Se l'importo d'assicurare è a 0 e la divisa non è impostata
036310011018     C     VIDIAS        IFEQ      *ZEROS
036320011018     C     VIDVAS        ANDEQ     *BLANKS
036330011018      * imposto la divisa della nazione del mittente
036340011018      * se estero
036350011018     C     MITVLT        IFNE      *BLANKS
036360011018     C     WMITIE        ANDEQ     'E'
036370011018     C                   MOVEL     MITVLT        VIDVAS
036380011018     C                   MOVEL     MITVLT        VARVAS
036390011018     C                   ELSE
036400011018      * altrimenti imposto la divisa della moneta di conto
036410011017     C                   MOVEL     §GEDCN        VIDVAS
036420011017     C                   MOVEL     §GEDCN        VARVAS
036430011017     C                   ENDIF
036440011018     C                   ENDIF
036450011018      *
036460011017      * Se la divisa e l'importo del valore merce non sono impostati
036470011018     C     VIDVAD        IFEQ      *BLANKS
036480011018     C     VIDVMD        ANDEQ     *ZEROS
036490011018      * imposto la divisa della nazione del mittente
036500011018      * se estero
036510011018     C     MITVLT        IFNE      *BLANKS
036520011018     C     WMITIE        ANDEQ     'E'
036530011018     C                   MOVEL     MITVLT        VIDVAD
036540011018     C                   MOVEL     MITVLT        VARVAD
036550011018     C                   ELSE
036560011018      * altrimenti imposto la divisa della moneta di conto
036570011017     C                   MOVEL     §GEDCN        VIDVAD
036580011017     C                   MOVEL     §GEDCN        VARVAD
036590011017     C                   ENDIF
036600011018     C                   ENDIF
036610011018      *
036620011017      * Se la divisa e l'importo del contrassegno non sono impostati
036630011018     C     VIDVCA        IFEQ      *BLANKS
036640011018     C     VIDCAS        ANDEQ     *ZEROS
036650011018      * imposto la divisa della nazione del destinatario
036660011018      * se estero
036670011018     C     DESVLT        IFNE      *BLANKS
036680011018     C     WDESIE        ANDEQ     'E'
036690011018     C                   MOVEL     DESVLT        VIDVCA
036700011018     C                   MOVEL     DESVLT        VARVCA
036710011018    2C                   ELSE
036720011018      * altrimenti imposto la divisa della moneta di conto
036730011017     C                   MOVEL     §GEDCN        VIDVCA
036740011017     C                   MOVEL     §GEDCN        VARVCA
036750011017     C                   ENDIF
036760011018     C                   ENDIF
036770030203      *------------
036780030124      * Bancali
036790030711      *  solo x causale 'BR'
036800030711If  1c                   If        D48Cvb = 'BR'
036810030124     c                   Clear                   dAr5Ban
036820030124     c                   Eval      kAr5Trd = 'BAN'
036830030725     c     kAr5          Chain     Fiar501l
036840030711     c                   If        %Found(Fiar501l)
036850030124     c                   Movel     Ar5Uni        dAr5Ban
036860030711     c                   EndIf
036870030124     c                   Eval      VidNba = §Ar5Nba
036880030124     c                   Eval      VidNb2 = §Ar5Nb2
036890030124     c                   Eval      VidRb1 = §Ar5Rb1
036900030124     c                   Eval      VidRb2 = §Ar5Rb2
036910030124     c                   Eval      VidNomb = §Ar5Nomb
036920030124     c                   Eval      VidMotb = §Ar5Motb
036930030124If  2c                   If        §Ar5Tba <> *Zeros
036940030612     c                   Clear                   Tibs02DS
036950030124     c                   Clear                   dTba
036960030124     c                   Movel     'C'           T02Mod
036970030124     c                   Movel     Knsif         T02Sif
036980030124     c                   Movel     'TBA'         T02Cod
036990030124     c                   Movel(p)  §Ar5Tba       T02Ke1
037000030124     c                   Call      'TIBS02R'
037010030124     c                   Parm                    Kpjba
037020030612     c                   Parm                    Tibs02DS
037030030124If  3c                   If        T02Err = *Blanks
037040030124     c                   Movel     T02Uni        dTba
037050030124    3c                   EndIf
037060030124     c                   Eval      VidTba = §TbaDesc
037070030124     c                   Eval      VidTbar = §TbaDesc
037080030124    2c                   EndIf
037090030124If  2c                   If        §Ar5Tb2 <> *Zeros
037100030612     c                   Clear                   Tibs02DS
037110030124     c                   Clear                   dTba
037120030124     c                   Movel     'C'           T02Mod
037130030124     c                   Movel     Knsif         T02Sif
037140030124     c                   Movel     'TBA'         T02Cod
037150030124     c                   Movel(p)  §Ar5Tb2       T02Ke1
037160030124     c                   Call      'TIBS02R'
037170030124     c                   Parm                    Kpjba
037180030612     c                   Parm                    Tibs02DS
037190030124If  3c                   If        T02Err = *Blanks
037200030124     c                   Movel     T02Uni        dTba
037210030124    3c                   EndIf
037220030124     c                   Eval      VidTb2 = §TbaDesc
037230030124     c                   Eval      VidTb2r = §TbaDesc
037240030124    2c                   EndIf
037250030711
037260080805     c* Se pgm richiamato, imposto dati dalla DSARBD
037270080805    2c                   if        *in20
037280080805     c                   Eval      VidRb1 = §bdpkf
037290080805     c                   Eval      VidRb2 = §bdvlf
037300080805    3c                   if        §bdrsd<>*blanks
037310080805     c                   Eval      VidNomb = §bdrsd
037320080805    3c                   endif
037330080805    3c                   if        §bdrd2<>*blanks
037340080805     c                   Eval      VidMotb = §bdrd2
037350080805     c                   else
037360080805     c                   Eval      VidMotb = 'PDA  '
037370080805    3c                   EndIf
037380080805
037390080805    2c                   EndIf
037400080805    1c                   EndIf
037410040608      *------------
037420040608      * Supermercati - telefono e referente
037430040608      *  solo x causale 'CS'
037440040608If  1c                   If        D48Cvb = 'CS'
037450040608     c                   Eval      wokscr = *Off
037460040608     c                   Clear                   dAr5Scr
037470040608     c                   Eval      kAr5Trd = 'SCR'
037480040608     c     kAr5          Setgt     Fiar501l
037490040608do  2c                   Do        *Hival
037500040608     c     kAr5          Readpe(n) Fiar501l
037510040608     c                   If        %Eof(Fiar501l)
037520040608     c                   Leave
037530040608     c                   EndIf
037540040608     c                   Movel     Ar5Uni        dAr5Scr
037550040608     c                   Eval      VidNom = §Ar5Nom
037560040608     c                   Eval      VidTel = §Ar5Tel
037570040608     c                   Eval      wokscr = *On
037580040608     c                   Leave
037590040608e   2c                   EndDo
037600040608      * se non ho trovato il rcd SCR cerco i dati sul rcd GEN
037610040608if  2c                   If        wokscr = *Off
037620070910     c***                Clear                   dAr5Gen
037630070910     c***                Eval      kAr5Trd = 'GEN'
037640070910     c***  kAr5          Chain(n)  Fiar501l
037650070910     c***                If        %Found(Fiar501l)
037660070910     c***                Movel     Ar5Uni        dAr5Gen
037670070910     c***                EndIf
037680070910     c***                Eval      VidNom = §Ar5Ref
037690070910     c***                Eval      VidTel = §Ar5Teld
037700070910     c                   Eval      VidNom = PAr5Ref
037710070910     c                   Eval      VidTel = PAr5Teld
037720040608e   2c                   EndIf
037730040608e   1c                   EndIf
037740911209     C*
037750910429     C                   ENDSR
037760941102     C*
037770941102     C* CARICO LA VARIA IN SFL ---------------------------------------*
037780941102     C     CARVAR        BEGSR
037790941102     C*
037800941102     C     WSV1          IFNE      ' '
037810011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
037820011018     C     ARBFBR        IFEQ      'S'
037830011018     C     WSV1          ANDEQ     'N'
037840011018     C     WVA1          ANDEQ     88888888
037850011018     C                   MOVEL     '1'           FLGVRN            1
037860011018     C                   ENDIF
037870990930     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
037880990930     C     WSV1          LOOKUP    SVA                                    33
037890990930     C     *IN33         IFEQ      *ON
037900941102     C                   ADD       1             NRR
037910941102     C     NRR           CHAIN     LR48S03                            30
037920941102     C                   MOVEL     WSV1          V3CSV1
037930941102     C                   Z-ADD     WVA1          V3CVA1
037940991006     C                   Z-ADD     WVA1          VARVA1
037950941102     C   30              WRITE     LR48S03
037960941102     C  N30              UPDATE    LR48S03
037970941102     C                   ENDIF
037980990930     C                   ENDIF
037990941102     C                   ENDSR
038000040122
038010040122      *---------------------------------------------------------------*
038020040122      * CARICO LA VARIA DELLA SECONDA BOLLA
038030040122      *---------------------------------------------------------------*
038040040122     c     CarVar1       BegSr
038050040122
038060040122if  1c                   If        wsv1 <> *Blanks
038070040122      * Carico la varia solo se fa parte del totale imponibile
038080040122     c     wsv1          Lookup    sva                                    31
038090040122if  2c                   If        *In31
038100040122     c                   add       1             ContaSv
038110040122     c                   add       wva1          TotImv
038120040122
038130040122s   3c                   Select
038140040122     c                   When      ContaSv = 1
038150040122     c                   Movel     wsv1          Vi2Sv1
038160040122     c                   Z-add     wva1          Vi2Va1
038170040122     c                   Z-add     wva1          Va2Va1
038180040122     c                   When      ContaSv = 2
038190040122     c                   Movel     wsv1          Vi2Sv2
038200040122     c                   Z-add     wva1          Vi2Va2
038210040122     c                   Z-add     wva1          Va2Va2
038220040122     c                   When      ContaSv = 3
038230040122     c                   Movel     wsv1          Vi2Sv3
038240040122     c                   Z-add     wva1          Vi2Va3
038250040122     c                   Z-add     wva1          Va2Va3
038260040122     c                   When      ContaSv = 4
038270040122     c                   Movel     wsv1          Vi2Sv4
038280040122     c                   Z-add     wva1          Vi2Va4
038290040122     c                   Z-add     wva1          Va2Va4
038300041130     c                   When      ContaSv = 5
038310041130     c                   Movel     wsv1          Vi2Sv5
038320041130     c                   Z-add     wva1          Vi2Va5
038330041130     c                   Z-add     wva1          Va2Va5
038340041130     c                   When      ContaSv = 6
038350041130     c                   Movel     wsv1          Vi2Sv6
038360041130     c                   Z-add     wva1          Vi2Va6
038370041130     c                   Z-add     wva1          Va2Va6
038380040122    3c                   EndSl
038390040122
038400040122    2c                   EndIf
038410040122
038420040122    1c                   EndIf
038430040122
038440040122     c                   EndSr
038450910429     C*
038460991001     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
038470991001     C     CARWAR        BEGSR
038480991001     C*
038490991001     C                   CLEAR                   WINL
038500991001     C*
038510991001     C                   CLEAR                   WSV
038520991001     C                   CLEAR                   WVA
038530991001     C                   CLEAR                   C
038540991001    1C                   DO        20            NRR
038550991001     C     NRR           CHAIN     LR48S03                            30
038560991001     C*
038570991001    2C     *IN30         IFEQ      *OFF
038580991001     C     V3CSV1        ANDNE     *BLANKS
038590991001     C                   ADD       1             C
038600991001     C                   MOVEL     V3CSV1        WSV(C)
038610991001     C                   Z-ADD     V3CVA1        WVA(C)
038620991001    2C                   ENDIF
038630991001    1C                   ENDDO
038640991001     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
038650991001     C     WCKINL        IFEQ      '1'
038660991001     C                   Z-ADD     1             II
038670991001     C     §$2FIN        LOOKUP    WSV(II)                                30
038680991001     C   30WVA(II)       IFGT      *ZEROS
038690991001     C                   MOVE      '1'           WINL              1
038700991001     C                   ENDIF
038710991001     C                   ENDIF
038720991001     C*
038730991001     C                   ENDSR
038740040123      *---------------------------------------------------------------*
038750040123      * Carico le varie della seconda bolla in una sk di comodo
038760040123      *---------------------------------------------------------------*
038770040123     c     Carwar1       Begsr
038780040123
038790040123     c                   Clear                   wsv
038800040123     c                   Clear                   wva
038810040123     c                   Clear                   c
038820040123
038830040123     c                   If        vi2sv1 <> *Blanks
038840050401     c                   add       1             c
038850050401     c                   Movel     vi2Sv1        wsv(c)
038860050401     c                   Z-add     vi2va1        wva(c)
038870040123     c                   EndIf
038880040123     c                   If        vi2sv2 <> *Blanks
038890050401     c                   add       1             c
038900050401     c                   Movel     vi2Sv2        wsv(c)
038910050401     c                   Z-add     vi2va2        wva(c)
038920040123     c                   EndIf
038930040123     c                   If        vi2sv3 <> *Blanks
038940050401     c                   add       1             c
038950050401     c                   Movel     vi2Sv3        wsv(c)
038960050401     c                   Z-add     vi2va3        wva(c)
038970040123     c                   EndIf
038980040123     c                   If        vi2sv4 <> *Blanks
038990050401     c                   add       1             c
039000050401     c                   Movel     vi2Sv4        wsv(c)
039010050401     c                   Z-add     vi2va4        wva(c)
039020040123     c                   EndIf
039030041130     c                   If        vi2sv5 <> *Blanks
039040050401     c                   add       1             c
039050050401     c                   Movel     vi2Sv5        wsv(c)
039060050401     c                   Z-add     vi2va5        wva(c)
039070041130     c                   EndIf
039080041130     c                   If        vi2sv6 <> *Blanks
039090050401     c                   add       1             c
039100050401     c                   Movel     vi2Sv6        wsv(c)
039110050401     c                   Z-add     vi2va6        wva(c)
039120041130     c                   EndIf
039130040123
039140040123     c                   EndSr
039150991012     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
039160991012     C     RDTASV        BEGSR
039170991012     C*
039180991012     C                   CLEAR                   C
039190030620     C                   CLEAR                   WVAR3A
039200040123     c                   Clear                   wvar3ai
039210991012    1C                   DO        20            NRR
039220991012     C     NRR           CHAIN     LR48S03                            30
039230991012    2C     *IN30         IFEQ      *OFF
039240991012     C                   ADD       1             C
039250020503     c* se la varia è uguale a quella prevista dal tipo bolla, non richiamo
039260020503     c*  piu' la tassazione
039270040123     c                   if        §3asva<>*blanks and v3csv1=§3asva
039280040123     c                   If        vidimv > *Zeros
039290020503     c                   eval      wtassa='N'
039300030620      * mi salvo l'imnporto della varia
039310030620     c                   eval      itassa = v3cva1
039320040123     c                   EndIf
039330030620      * mi salvo un campo di comodo
039340030620     c                   movel     'S'           Wvar3a            1
039350040123      * memorizzo anche se valorizzata
039360040123     c                   If        v3cva1 > 0
039370040123     c                   Eval      wvar3ai = 'S'
039380040123     c                   EndIf
039390030620      *
039400020503     c                   endif
039410030620      * verifico se esiste la varia "D" non la ricalcolo in caso di fattura immediata
039420030620     c                   if        v3csv1 = 'D'
039430030620     c                   eval      wtassad = 'N'
039440030620     c                   endif
039450030620      *
039460030624      * se esiste la varia "&" con tutti 8888888 non la passo al TNSF20R
039470030624      * ma passo un flag  di richiesta calcolo varia "&"
039480030709     C                   if        v3csv1 = '&' and v3cva1 = 88888888 and
039490030709     C                             flgsf20 = 'S'
039500030624     C                   eval      TASFCAA = 'S'
039510030624     C                   else
039520991012     C                   MOVEL     V3CSV1        VSV(C)
039530991012     C                   MOVEL     V3CVA1        VVA(C)
039540030624     c                   endif
039550030624      *
039560991012    2C                   ENDIF
039570991012    1C                   ENDDO
039580991012     C                   ENDSR
039590920120     C*--- EMISSIONE E GESTIONE FORMATO 2 ----------------------------*
039600920120     C     EMFOR2        BEGSR
039610941102     C*
039620920120     C     FOR02         TAG
039630941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
039640950519    1C     *IN90         IFEQ      *ON
039650950114     C     D48FFR        OREQ      'S'
039660950607     C     D48FFR        OREQ      'V'
039670941214     C                   WRITE     LR48T03
039680941014     C                   EXFMT     LR48D02
039690950519    1C                   END
039700941102     C*
039710941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
039720941102     C                   SETOFF                                       2890
039730941102     C                   SETOFF                                       404142
039740941102     C                   SETOFF                                       434445
039750941102     C                   SETOFF                                       464748
039760061115     C                   SETOFF                                       495051
039770140408     C                   SETOFF                                       5268
039780941220     C* F12 - RITORNO
039790941220    1C     *INKL         IFEQ      *OFF
039800070329     c
039810070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
039820070329     c                   if        *in87 and *inkr
039830070329     c                   exsr      CALLNOTELDV
039840070329     c                   goto      for02
039850070329     c                   endif
039860941216     C*
039870941216     C* SE NON POSSO VARIARE NE IL PESO, NE IL VOLUME, NON CONTROLLO
039880941216     C* E NON AGGIORNO
039890941220    2C     *IN88         IFEQ      *OFF
039900941216     C     *IN89         OREQ      *OFF
039910950114     C     *IN24         OREQ      *ON
039920941216     C*
039930941216     C* CONTROLLO FORMATO
039940920120     C                   EXSR      CONTR2
039950941215     C*
039960941220     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
039970941220    3C     *IN90         IFEQ      *ON
039980941215     C     D48FFR        ANDEQ     'E'
039990941215     C                   MOVEL     VIDMSG        D48MSG
040000941215     C                   MOVEL     '2'           D48ERR
040010941215     C                   GOTO      ENDEM2
040020941220    3C                   ENDIF
040030140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
040040140502     c*   (d48ffr='E' e 26 off)
040050140502     c  n90
040060140502     cann26              if        d48ffr='E'
040070140502     C                   GOTO      ENDEM2
040080140502     c                   endif
040090950607     C  NKF
040100950607     CANN26
040110920120     COR 90              GOTO      FOR02
040120060227     c* richiamo routine di richiesta/controllo motivazione
040130060301     c                   if        d66mtv = 'S' and w48mct = *blanks
040140060227     c                   exsr      ges_mtv
040150060301     c                   if        olv85f12 = '1'
040160060301     c                   if        ilv85wdw = ' '
040170060227     c                   goto      for02
040180060301     c                   else
040190060301     c                   eval      *inkl = '1'
040200060301     C                   GOTO      ENDEM2
040210060227     c                   endif
040220060301     c                   endif
040230060227     C     *IN90         IFEQ      *ON
040240060227     C     D48FFR        ANDEQ     'E'
040250060227     C                   MOVEL     VIDMSG        D48MSG
040260060227     C                   MOVEL     '2'           D48ERR
040270060227     C                   GOTO      ENDEM2
040280060227     C                   ENDIF
040290060227     c                   endif
040300920120     C*
040310141020     c* Se Richiesta modifica di indirizzo tale da variare la LNA emetto window
040320141020     c* per segnalare e richiedere una delle seguenti opzioni:
040330141020     c*  -Forzare la variazione senza variare la lna
040340141020     c*  -Richiedere il dirottamento
040350141111    3c                   if        d48ffr<>'E'  and *in01
040360141029     c                             and (%lookup('999':skFilOkdir)>0 or
040370141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)>0 and
040380141029     c                              %lookup(%editc(dutpou:'X'):skFilOkdir)>0) )
040390141020     c                   clear                   wdir
040400141020    4C     d_o95lna      ifne      ARBLNA
040410141022     c* le bolle export non le dirottiamo mai da qui
040420141022     c     lnantw        andne     'DPD'
040430141022     c     lnantw        andne     'EEX'
040440141022     c     lnantw        andne     'FED'
040450141219     c* no bolle di recupero
040460141219     c     §3arbl        andne     'R'
040470141204    5C     SAVCAD        IFNE      VIDCAD
040480141204     C     SAVLOD        ORNE      VIDLOD
040490141204     C     SAVrsd        ORNE      VIDrsd
040500141204     C     SAVind        ORNE      VIDind
040510141204     C     SAVprd        ORNE      VIDprd
040520141204     C     SAVnzd        ORNE      VIDnzd
040530141204     c     vidzng        oreq      savzng
040540141203     c     vidffd        andeq     *blanks
040550141021     c                   exsr      ges_wdw02
040560141021     c                   if        winf12='1'
040570141021     c                   goto      for02
040580141021     c                   endif
040590141020    5C                   END
040600141020    4C                   END
040610141020    4c                   if        wdir='1'
040620141212
040630141212     c* se hanno cambiato il Fermo deposito devo anche eseguire la REGIS2
040640141212     c                   if        arbffd<>%subst(vidffD:1:1)
040650141212     c                   eval      wnoindir='S'
040660141212     c                   exsr      regis2
040670141218     C   91              GOTO      FOR02
040680141212     c                   endif
040690141212
040700141020     c                   exsr      sr_tiidc
040710141212
040720141020   x4c                   else
040730141212     c                   eval      wnoindir=' '
040740141020     C                   EXSR      REGIS2
040750141020    4c                   endif
040760141020   x3c                   else
040770141212     c                   eval      wnoindir=' '
040780930517     C                   EXSR      REGIS2
040790141020    3c                   endif
040800991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
040810991130     C     *IN91         IFEQ      *ON
040820991130     C     D48FFR        ANDEQ     'E'
040830991130     C                   MOVEL     VIDMSG        D48MSG
040840991130     C                   MOVEL     '2'           D48ERR
040850991130     C                   GOTO      ENDEM2
040860991130     C                   ENDIF
040870991130     C   91              GOTO      FOR02
040880941216     C*
040890941220   X2C                   ELSE
040900941216     C*
040910941216     C                   GOTO      FOR02
040920941220    2C                   ENDIF
040930941220     C*
040940950518    1C                   ENDIF
040950150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
040960150112     c*  se ce ne sono
040970150112     c                   if        msgforzatxt<>*blanks
040980150112     C                   MOVEL     msgforzatxt   D48MSG
040990150112     C                   MOVEL     'Z'           D48ERR
041000150112     c                   clear                   msgforzatxt
041010150112     C                   ENDIF
041020950518     C*
041030950518     C     ENDEM2        TAG
041040950518     C*
041050950518    1C     *INKL         IFEQ      *ON
041060950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
041070950519    1C     D48FFR        OREQ      'E'
041080950518     C     *IN90         ANDEQ     *ON
041090000303    1C     D48FFR        OREQ      'E'
041100000303     C     *IN91         ANDEQ     *ON
041110941220     C* F12-RITORNO
041120160119    2C     *IN10         IFEQ      *ON
041130941220     C                   UNLOCK    FNARB01L
041140051118     C                   UNLOCK    FiAR901L
041150160119     c                   endif
041160160119     C****               ELSE
041170941222     C                   UNLOCK    FNBLP01L
041180160119    2C****               END
041190991001     C                   UNLOCK    FIAR601L
041200950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041210950607     C* flag D48F12 = 1
041220950607     C   KL              MOVEL     '1'           D48F12
041230950519    1C                   END
041240941215     C*
041250950518     C                   ENDSR
041260920121     C*
041270920121     C*--- EMISSIONE E GESTIONE FORMATO 3 ----------------------------*
041280920121     C     EMFOR3        BEGSR
041290990415     C*
041300990415     C                   MOVEL     ' '           COMASS
041310941102     C*
041320920121     C     FOR03         TAG
041330941102     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
041340950114     C     *IN90         IFEQ      *ON
041350941212     C     D48FFR        OREQ      'S'
041360950607     C     D48FFR        OREQ      'V'
041370941215     C                   WRITE     LR48T03
041380941212     C                   WRITE     LR48Z03
041390941212     C                   EXFMT     LR48C03
041400941220    1C                   END
041410941102     C*
041420941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
041430941209     C                   SETOFF                                       289052
041440941102     C                   SETOFF                                       404142
041450991006     C                   SETOFF                                       434445
041460941102     C                   SETOFF                                       464748
041470941209     C                   SETOFF                                       495051
041480941215     C                   SETOFF                                       989453
041490961112     C                   SETOFF                                       545556
041500990415     C                   SETOFF                                       82
041510941220     C*
041520941220     C* PER F12-RITORNO O F21 RITASSAZIONE, DISALLOCO I RECORD
041530941220     C* F12-RITORNO
041540941220    1C     *INKL         IFEQ      *ON
041550941220     C     *INKV         OREQ      *ON
041560941222     C*
041570941222     C     *IN10         IFEQ      *ON
041580941220     C                   UNLOCK    FNARB01L
041590051118     C                   UNLOCK    FiAR901L
041600941222     C                   ELSE
041610941222     C                   UNLOCK    FNBLP01L
041620941220    1C                   ENDIF
041630991005     C                   UNLOCK    FIAR601L
041640950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
041650950607     C* flag D48F12 = 1
041660950607     C   KL              MOVEL     '1'           D48F12
041670941222    1C                   ENDIF
041680941220     C* F12 - RITORNO
041690941220    1C  NKL              DO
041700941220     C*
041710941220     C* F21 - RITASSAZIONE BOLLA
041720941220    2C     *INKV         IFEQ      *ON
041730050511     C                   CLEAR                   Fnlra2DS
041740050511     C                   Z-ADD     ARBAAS        ilra2AAS
041750050511     C                   Z-ADD     ARBLNP        ilra2lNP
041760050511     C                   Z-ADD     ARBNRS        ilra2NRS
041770050511     C                   Z-ADD     ARBNSP        ilra2NSP
041780050511     C                   MOVEL     D48FGS        ilra2FGS
041790050511     C                   MOVEL     D48CVB        ilra2CVB
041800050511     C                   MOVEL     Fnlra2DS      KPJBU
041810050511     C                   CALL      'FNLRA2R'
041820941217     C                   PARM                    KPJBA
041830030718     C                   PARM                    TRUL90DS
041840050804     c* Se mi passa errore, lo passo dal lr48 al lr36
041850050804     c                   movel     kpjbu         fnlra2ds
041860050804     C                   MOVEL     olra2msg      D48MSG
041870050804     C                   MOVEL     olra2err      D48ERR
041880131119     c                   if        olra2nft>0
041890131119     C                   MOVEL     '1'           WTRC
041900131119     c     karb2         chain(n)  fiar6000
041910131119     c                   endif
041920131120     c* trasmetto variazione a PDA:
041930131120     c* se segue fattura --> solo se prima era una fattura immediata
041940131120     c* se fattura immediata --> solo se variato importo della fattura
041950131119     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
041960131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
041970131120     c                              or ar6div<>viddiv)))
041980130416     c                   eval      wpda_ndc=arbndc
041990130416     c                   eval      wpda_ddc=arbddc
042000130416     c                   eval      wpda_dcm=arbdcm
042010140120     c                   eval      arbdtv=olra2dtv
042020140120     c                   eval      arborv=olra2orv
042030140120     c                   eval      arbpru=olra2pru
042040140120     c                   eval      arbcvb=d48cvb
042050130416     c                   exsr      sr_pda
042060130416     c                   endif
042070920121     C*
042080941220   X2C                   ELSE
042090920121     C* CONTROLLO FORMATO
042100920121     C                   EXSR      CONTR3
042110941215     C*
042120941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042130941220    3C     *IN90         IFEQ      *ON
042140941215     C     D48FFR        ANDEQ     'E'
042150941215     C                   MOVEL     VIDMSG        D48MSG
042160941215     C                   MOVEL     '3'           D48ERR
042170941215     C                   GOTO      ENDEM3
042180941220    3C                   ENDIF
042190920320     C*
042200950607     C  NKF
042210950607     CANNKI
042220950607     CANN26
042230990415     COR 82
042240000105     COR KN
042250920320     COR 90              GOTO      FOR03
042260060302     c* richiamo routine di richiesta/controllo motivazione
042270060302     c                   if        d66mtv = 'S' and w48mct = *blanks
042280060302     c                   exsr      ges_mtv
042290060302     c                   if        olv85f12 = '1'
042300060302     c                   if        ilv85wdw = ' '
042310060302     c                   goto      for03
042320060302     c                   else
042330060302     c                   eval      *inkl = '1'
042340060302     C                   GOTO      ENDEM3
042350060302     c                   endif
042360060302     c                   endif
042370060302     C     *IN90         IFEQ      *ON
042380060302     C     D48FFR        ANDEQ     'E'
042390060302     C                   MOVEL     VIDMSG        D48MSG
042400070308     C                   MOVEL     '3'           D48ERR
042410060302     C                   GOTO      ENDEM3
042420060302     C                   ENDIF
042430060302     c                   endif
042440920121     C*
042450920204     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
042460920204     C                   EXSR      REGIS3
042470991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
042480991130     C     *IN91         IFEQ      *ON
042490991130     C     D48FFR        ANDEQ     'E'
042500991130     C                   MOVEL     VIDMSG        D48MSG
042510991130     C                   MOVEL     '3'           D48ERR
042520991130     C                   GOTO      ENDEM3
042530991130     C                   ENDIF
042540920204     C   91              GOTO      FOR03
042550920204     C*
042560070309     c* ristampo fattura immediata se variata(37 on), se in arrivo
042570070309     c*  (10 on) e se non attiva GEO
042580130321    3C**   *IN37         IFEQ      *ON
042590130321    3C***  *IN10         ANDEQ     *ON
042600130321     c**   FGSddaSI      andeq     ' '
042610130321     C***                CLEAR                   FNLSB5DS
042620130321     C***                MOVEL     'V'           DB0RIS
042630130321     C***                MOVEL     D48TBO        DB0TBO
042640130321     C***                MOVEL     ARBAAS        DB0AAS
042650130321     C***                MOVEL     ARBLNP        DB0LNP
042660130321     C***                MOVEL     ARBNRS        DB0NRS
042670130321     C***                MOVEL     ARBNSP        DB0NSP
042680130321     C***                CALL      D90PSL
042690130321     C***                PARM                    FNLSB5DS
042700130321    3C***                END
042710941212     C*
042720941220    2C                   END
042730941220     C*
042740000303    1C                   END
042750000303     C     ENDEM3        TAG
042760000303     C* SE PGM RICHIAMATO E C'E' ERRORE DISALLOCO
042770000303    1C     D48FFR        IFEQ      'E'
042780000303     C     *IN90         ANDEQ     *ON
042790000303    1C     D48FFR        OREQ      'E'
042800000303     C     *IN91         ANDEQ     *ON
042810000303     C     *IN10         IFEQ      *ON
042820000303     C                   UNLOCK    FNARB01L
042830051118     C                   UNLOCK    FiAR901L
042840000303     C                   ELSE
042850000303     C                   UNLOCK    FNBLP01L
042860000303     C                   ENDIF
042870000303     C                   UNLOCK    FIAR601L
042880000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
042890000303     C* flag D48F12 = 1
042900000303     C   KL              MOVEL     '1'           D48F12
042910000303    1C                   ENDIF
042920920121     C*
042930000303     C                   ENDSR
042940941102     C*
042950941102     C* EMETTO FORMATO4 ----------------------------------------------*
042960941102     C     EMFOR4        BEGSR
042970941102     C*
042980941102     C     FOR04         TAG
042990941102     C*
043000950114     C     *IN90         IFEQ      *ON
043010941212     C     D48FFR        OREQ      'S'
043020950607     C     D48FFR        OREQ      'V'
043030941102     C                   WRITE     LR48T01
043040941102     C                   EXFMT     LR48D04
043050941102     C                   ENDIF
043060941209     C*
043070941209     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
043080941209     C                   SETOFF                                       289045
043090941209     C                   SETOFF                                       464748
043100991006     C                   SETOFF                                       495051
043110041130     c                   setoff                                       636465
043120041130     c                   setoff                                       6667
043130941220     C*
043140941220     C* PER F12 -RITORNO O F21-RITASSAZIONE SBLOCCO I RECORD
043150941220     C     *INKL         IFEQ      *ON
043160941220     C     *INKV         OREQ      *ON
043170941222     C*
043180941222     C     *IN10         IFEQ      *ON
043190941220     C                   UNLOCK    FNARB01L
043200051118     C                   UNLOCK    FiAR901L
043210941222     C                   ELSE
043220941222     C                   UNLOCK    FNBLP01L
043230941220     C                   ENDIF
043240991005     C                   UNLOCK    FIAR601L
043250950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
043260950607     C* flag D48F12 = 1
043270950607     C   KL              MOVEL     '1'           D48F12
043280941222     C                   ENDIF
043290941220     C* F12 - RITORNO
043300941102     C     *INKL         IFEQ      *OFF
043310050228     C* F21 - RITASSAZIONE BOLLA
043320941220     C     *INKV         IFEQ      *ON
043330050511     C                   CLEAR                   Fnlra2DS
043340050511     C                   Z-ADD     ARBAAS        ilra2AAS
043350050511     C                   Z-ADD     ARBLNP        ilra2LNP
043360050511     C                   Z-ADD     ARBNRS        ilra2NRS
043370050511     C                   Z-ADD     ARBNSP        ilra2NSP
043380050511     C                   MOVEL     D48FGS        ilra2FGS
043390050511     C                   MOVEL     D48CVB        ilra2CVB
043400050511     C                   MOVEL     Fnlra2DS      KPJBU
043410050511     C                   CALL      'FNLRA2R'
043420941102     C                   PARM                    KPJBA
043430030718     C                   PARM                    TRUL90DS
043440050804     c
043450050804     c* Se mi passa errore, lo passo dal lr48 al lr36
043460050804     c                   movel     kpjbu         fnlra2ds
043470050804     C                   MOVEL     olra2msg      D48MSG
043480050804     C                   MOVEL     olra2err      D48ERR
043490131119     c                   if        olra2nft>0
043500131119     C                   MOVEL     '2'           WTRC
043510131119     c     karb2         chain(n)  fiar6000
043520131119     c                   endif
043530131120     c* trasmetto variazione a PDA:
043540131120     c* se segue fattura --> solo se prima era una fattura immediata
043550131120     c* se fattura immediata --> solo se variato importo della fattura
043560131120     c                   if        *in10 and ((olra2nft=0  and ar6nft>0)
043570131120     c                              or (olra2nft>0 and (ar6ift<>sav_ift
043580131120     c                              or ar6div<>viddiv)))
043590130416     c                   eval      wpda_ndc=arbndc
043600130416     c                   eval      wpda_ddc=arbddc
043610130416     c                   eval      wpda_dcm=arbdcm
043620140120     c                   eval      arbdtv=olra2dtv
043630140120     c                   eval      arborv=olra2orv
043640140120     c                   eval      arbpru=olra2pru
043650140120     c                   eval      arbcvb=d48cvb
043660130416     c                   exsr      sr_pda
043670130416     c                   endif
043680941102     C*
043690941220     C                   ELSE
043700941102     C* CONTROLLO FORMATO
043710941102     C                   EXSR      CONTR4
043720941215     C*
043730941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
043740941215     C     *IN90         IFEQ      *ON
043750941215     C     D48FFR        ANDEQ     'E'
043760941215     C                   MOVEL     VIDMSG        D48MSG
043770941215     C                   MOVEL     '4'           D48ERR
043780941215     C                   GOTO      ENDEM4
043790941215     C                   ENDIF
043800941102     C*
043810140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
043820140502     c*   (d48ffr='E' e 26 off)
043830140502     c  n90
043840140502     cann26              if        d48ffr='E'
043850140502     C                   GOTO      ENDEM4
043860140502     c                   endif
043870950607     C  NKF
043880950607     CANNKI
043890950607     CANN26
043900941102     COR 90              GOTO      FOR04
043910060302     c* richiamo routine di richiesta/controllo motivazione
043920060302     c                   if        d66mtv = 'S' and w48mct = *blanks
043930060302     c                   exsr      ges_mtv
043940060302     c                   if        olv85f12 = '1'
043950060302     c                   if        ilv85wdw = ' '
043960060302     c                   goto      for04
043970060302     c                   else
043980060302     c                   eval      *inkl = '1'
043990060302     C                   GOTO      ENDEM4
044000060302     c                   endif
044010060302     c                   endif
044020060302     C     *IN90         IFEQ      *ON
044030060302     C     D48FFR        ANDEQ     'E'
044040060302     C                   MOVEL     VIDMSG        D48MSG
044050070308     C                   MOVEL     '4'           D48ERR
044060060302     C                   GOTO      ENDEM4
044070060302     C                   ENDIF
044080060302     c                   endif
044090941102     C*
044100941102     C* CMD6 - AGGIORNAMENTO PIU' EVENTUALE STAMPA
044110941102     C                   EXSR      REGIS4
044120991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044130991130     C     *IN91         IFEQ      *ON
044140991130     C     D48FFR        ANDEQ     'E'
044150991130     C                   MOVEL     VIDMSG        D48MSG
044160991130     C                   MOVEL     '4'           D48ERR
044170991130     C                   GOTO      ENDEM4
044180991130     C                   ENDIF
044190941102     C   91              GOTO      FOR04
044200941102     C                   END
044210941102     C                   END
044220000303     C*
044230000303     C     ENDEM4        TAG
044240000303    1C     D48FFR        IFEQ      'E'
044250000303     C     *IN90         ANDEQ     *ON
044260000303    1C     D48FFR        OREQ      'E'
044270000303     C     *IN91         ANDEQ     *ON
044280000303     C     *IN10         IFEQ      *ON
044290000303     C                   UNLOCK    FNARB01L
044300051118     C                   UNLOCK    FiAR901L
044310000303     C                   ELSE
044320000303     C                   UNLOCK    FNBLP01L
044330000303     C                   ENDIF
044340000303     C                   UNLOCK    FIAR601L
044350000303     C* Comunico al chiamante che è stato premuto F12 valorizzando il
044360000303     C* flag D48F12 = 1
044370000303     C   KL              MOVEL     '1'           D48F12
044380000303    1C                   ENDIF
044390941102     C*
044400000303     C                   ENDSR
044410941102     C*
044420941102     C* EMETTO E GESTISCO FORMATO5 -----------------------------------*
044430941102     C     EMFOR5        BEGSR
044440950407     C*
044450941102     C     FOR05         TAG
044460941102     C*
044470950114     C     *IN90         IFEQ      *ON
044480941212     C     D48FFR        OREQ      'S'
044490950607     C     D48FFR        OREQ      'V'
044500941102     C                   WRITE     LR48T01
044510941102     C                   EXFMT     LR48D05
044520941102     C                   END
044530941102     C*
044540941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
044550941102     C                   SETOFF                                       289096
044560941102     C                   SETOFF                                       404142
044570991001     C                   SETOFF                                       434492
044580070329     C* F12 - RITORNO
044590941220    1C     *INKL         IFEQ      *OFF
044600070329     c
044610070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
044620070329     c                   if        *in87 and *inkr
044630070329     c                   exsr      CALLNOTELDV
044640070329     c                   goto      for05
044650070329     c                   endif
044660941102     C*
044670941102     C* CONTROLLO FORMATO
044680941213     C                   EXSR      CONTR5
044690941215     C*
044700941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
044710941220    2C     *IN90         IFEQ      *ON
044720941215     C     D48FFR        ANDEQ     'E'
044730941215     C                   MOVEL     VIDMSG        D48MSG
044740941215     C                   MOVEL     '5'           D48ERR
044750941215     C                   GOTO      ENDEM5
044760941220    2C                   ENDIF
044770140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
044780140502     c*   (d48ffr='E' e 26 off)
044790140502     c  n90
044800140502     cann26              if        d48ffr='E'
044810140502     C                   GOTO      ENDEM5
044820140502     c                   endif
044830140502     c* Se errore forzabile ma pgm richiamato in modalità 'E' o 'F' cioè
044840140502     c* senza emissione di videata ignoro l'errore forzabile e permetto
044850140502     c* l'aggiornamento
044860140502     c   92              if        d48ffr='E'
044870140502     c                   setoff                                       92
044880140502     c                   endif
044890950607     C  NKF
044900950607     CANN26
044910950407     COR 90
044920950407     COR 92              GOTO      FOR05
044930060302     c* richiamo routine di richiesta/controllo motivazione
044940060302     c                   if        d66mtv = 'S' and w48mct = *blanks
044950060302     c                   exsr      ges_mtv
044960060302     c                   if        olv85f12 = '1'
044970060302     c                   if        ilv85wdw = ' '
044980060302     c                   goto      for05
044990060302     c                   else
045000060302     c                   eval      *inkl = '1'
045010060302     C                   GOTO      ENDEM5
045020060302     c                   endif
045030060302     c                   endif
045040060302     C     *IN90         IFEQ      *ON
045050060302     C     D48FFR        ANDEQ     'E'
045060060302     C                   MOVEL     VIDMSG        D48MSG
045070070308     C                   MOVEL     '5'           D48ERR
045080060302     C                   GOTO      ENDEM5
045090060302     C                   ENDIF
045100060302     c                   endif
045110941102     C*
045120941220     C                   EXSR      REGIS5
045130991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045140991130     C     *IN91         IFEQ      *ON
045150991130     C     D48FFR        ANDEQ     'E'
045160991130     C                   MOVEL     VIDMSG        D48MSG
045170991130     C                   MOVEL     '5'           D48ERR
045180991130     C                   GOTO      ENDEM5
045190991130     C                   ENDIF
045200941213     C   91              GOTO      FOR05
045210941220     C*
045220950518     C                   ENDIF
045230941222     C*
045240950518     C     ENDEM5        TAG
045250950518     C* F12-RITORNO  --> DISALLOCO RECROD
045260950518    1C     *INKL         IFEQ      *ON
045270950518     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045280950518     C     D48FFR        OREQ      'E'
045290950518     C     *IN90         ANDEQ     *ON
045300000303     C     D48FFR        OREQ      'E'
045310000303     C     *IN91         ANDEQ     *ON
045320950518     C*
045330950518    2C     *IN10         IFEQ      *ON
045340941220     C                   UNLOCK    FNARB01L
045350941222     C                   ELSE
045360941222     C                   UNLOCK    FNBLP01L
045370950518    2C                   END
045380051118     C                   UNLOCK    Fiar901L
045390991001     C                   UNLOCK    FIAR601L
045400950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
045410950607     C* flag D48F12 = 1
045420950607     C   KL              MOVEL     '1'           D48F12
045430941222    1C                   END
045440941102     C*
045450950518     C                   ENDSR
045460941102     C*
045470941102     C* EMISSIONE E GESTIONE FORMATO 7 ------------------------------*
045480941102     C     EMFOR7        BEGSR
045490941102     C*
045500941102     C     FOR07         TAG
045510941102     C*
045520950114     C     *IN90         IFEQ      *ON
045530941212     C     D48FFR        OREQ      'S'
045540950607     C     D48FFR        OREQ      'V'
045550941102     C                   WRITE     LR48T01
045560941102     C                   EXFMT     LR48D07
045570941102     C                   END
045580941102     C*
045590941102     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
045600941102     C                   SETOFF                                       2890
045610941102     C                   SETOFF                                       404142
045620941102     C                   SETOFF                                       434445
045630031001     c                   Setoff                                       62
045640070329     C* F12 - RITORNO
045650941102     C     *INKL         IFEQ      *OFF
045660070329     c
045670070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
045680070329     c                   if        *in87 and *inkr
045690070329     c                   exsr      CALLNOTELDV
045700070329     c                   goto      for07
045710070329     c                   endif
045720941102     C*
045730941102     C* CONTROLLO FORMATO
045740941102     C                   EXSR      CONTR7
045750040219      * questi controlli li devo saltare se il richiamo è stato fatto con causale 'CD'
045760040219      * cioè da chiusura distina
045770040224      * o causale 'CA' creata da LR48 in automatico
045780040224     c                   If        Not *In90 and D48Cvb <> 'CD' and
045790040224     c                             d48cvb <> 'CA'
045800040219     C                   ExSr      Contr72
045810040219     c                   EndIf
045820941215     C*
045830941215     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
045840070309     c*          non se è forzabile solo allafine dopo la REGIS
045850941215     C     *IN90         IFEQ      *ON
045860941215     C     D48FFR        ANDEQ     'E'
045870070330     c**** Msgforza      andeq     0
045880941215     C                   MOVEL     VIDMSG        D48MSG
045890941215     C                   MOVEL     '7'           D48ERR
045900941215     C                   GOTO      ENDEM7
045910941215     C                   ENDIF
045920070330     c
045930070330     c* Se non ho avuto errori e richiamo solo per controlli, esco
045940070330     c*   (d48ffr='E' e 26 off)
045950070329     c  n90
045960070330     cann26              if        d48ffr='E'
045970070329     C                   GOTO      ENDEM7
045980070329     c                   endif
045990950607     C  NKF
046000950607     CANN26
046010941102     COR 90              GOTO      FOR07
046020060301     c* richiamo routine di richiesta/controllo motivazione
046030060301     c                   if        d66mtv = 'S' and w48mct = *blanks
046040060301     c                   exsr      ges_mtv
046050060301     c* premuto f12-Ritorno
046060060301     c                   if        olv85f12 = '1'
046070060301     c                   if        ilv85wdw = ' '
046080060301     c                   goto      for07
046090060301     c                   else
046100060301     c                   eval      *inkl = '1'
046110060301     C                   GOTO      ENDEM7
046120060301     c                   endif
046130060301     c                   endif
046140060301     c* Errore da ritornare a chiamante
046150060301     C     *IN90         IFEQ      *ON
046160060301     C     D48FFR        ANDEQ     'E'
046170060301     C                   MOVEL     VIDMSG        D48MSG
046180060301     C                   MOVEL     '7'           D48ERR
046190060301     C                   GOTO      ENDEM7
046200060301     C                   ENDIF
046210060301     c                   endif
046220941102     C*
046230070307     C* F6 - AGGIORNAMENTO
046240070307     c* Se devo scrivere file proposte, aggiorno solo FIARP
046250070307     c                   if        AggioARP='S'
046260070307     c                   EXSR      REGISARP
046270070307     c                   endif
046280070308     c
046290070308     c* Aggiorno comunque per gg chiusura e consegne particolari, se
046300070308     c*  modificate, in caso di scrittura di FIARP
046310070308     c                   if        AggioARP=' ' or
046320070308     c                             arbgc1<>vidgc1  or arbgc2<>vidgc2 or
046330151230     c                             arbtc1<>vidtc1 or arbtc2<>vidtc2
046340941102     C                   EXSR      REGIS7
046350070309     c                   else
046360160119     C   10              UNLOCK    FIAR601L
046370160119     C   10              UNLOCK    Fnarb01l
046380160119     C   10              UNLOCK    FiAR901L
046390160119     C                   UNLOCK    Fnblp01l
046400070308     c                   endif
046410070308     c
046420991130     C     *IN91         IFEQ      *ON
046430991130     C     D48FFR        ANDEQ     'E'
046440991130     C                   MOVEL     VIDMSG        D48MSG
046450991130     C                   MOVEL     '7'           D48ERR
046460991130     C                   GOTO      ENDEM7
046470991130     C                   ENDIF
046480991130     C   91              GOTO      FOR07
046490950519     C                   ENDIF
046500070309     c
046510070309     c* Se non ho passato msg vincolanti, passo msg di avvertimento
046520070309     c*  se ce ne sono
046530070309     c                   if        msgforza>0
046540070309     c                   z-aDD     MSGFORZA      XX
046550070309     C                   MOVEL     MSG(XX)       D48MSG
046560070309     C                   MOVEL     'Z'           D48ERR
046570070309     c                   clear                   msgforza
046580070309     C                   ENDIF
046590950519     C*
046600950519     C     ENDEM7        TAG
046610070309     C* F12-RITORNO  --> DISALLOCO REcord
046620941222     C*
046630950519    1C     *INKL         IFEQ      *ON
046640950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
046650950519     C     D48FFR        OREQ      'E'
046660950519     C     *IN90         ANDEQ     *ON
046670000303     C     D48FFR        OREQ      'E'
046680000303     C     *IN91         ANDEQ     *ON
046690070329     C     D48FFR        OREQ      'E'
046700070329     c     *in26         andeq     *off
046710950519     C*
046720950519    2C     *IN10         IFEQ      *ON
046730941220     C                   UNLOCK    FNARB01L
046740991005     C                   UNLOCK    FIAR601L
046750051118     C                   UNLOCK    FiAR901L
046760160119     c                   endif
046770160119     C***                ELSE
046780941222     C                   UNLOCK    FNBLP01L
046790160119    2C***                END
046800950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
046810950607     C* flag D48F12 = 1
046820950607     C   KL              MOVEL     '1'           D48F12
046830950519    1C                   END
046840941102     C*
046850950519     C                   ENDSR
046860021129      **
046870021129      * EMISSIONE E GESTIONE FORMATO 7/1 Supermercati ---------------*
046880021129     c     Emfor71       BegSr
046890021129
046900021129     c     For071        Tag
046910021129
046920021129     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
046930021129     c                   Write     Lr48T01
046940021129     c                   Exfmt     Lr48D071
046950021129     c                   EndIf
046960021129
046970021129      * Spengo idnicatori di posizionamento cursore
046980021129     c                   Setoff                                       2890
046990021129     c                   Setoff                                       404142
047000021129     c                   Setoff                                       434445
047010021129     c                   Setoff                                       575859
047020031001     c                   Setoff                                       62
047030070329      * F12 - RITORNO
047040021129     c                   IF        Not *InKl
047050070329     c
047060070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
047070070329     c                   if        *in87 and *inkr
047080070329     c                   exsr      CALLNOTELDV
047090070329     c                   goto      for071
047100070329     c                   endif
047110021129
047120021129      * CONTROLLO FORMATO
047130021129     c                   ExSr      Contr7
047140021129
047150021129     c  n90              ExSr      Contr71
047160031028     C  n90              ExSr      Contr72
047170070306     c
047180070306     c* Nel caso dei supermercati devo sempre aggiornare la bolla
047190070306     c*  e mai il file di proposta FIARP per sicurezza lo pulisco
047200070306     c                   clear                   aggioarp
047210070308     c                   clear                   savdcr
047220070308     c                   clear                   savTcr
047230070308     c                   clear                   savHcr
047240070306     c
047250040330      * La data non può superare i gg limiti indicati in tab. VPO
047260040330     c                   If        Not *In90
047270040330     c                   Eval      wdataiso = wdataoggi
047280040330     c                   Adddur    §vpodcr:*d    wdataiso
047290040330     c     *iso          move      wdataiso      ds_data
047300040330     c                   Move      §vpodcr       w003a
047310040330     c                   Movea     w003a         ski(13)
047320040330     c                   Z-add     13            i
047330040330     c                   ExSr      abbl
047340040330     c                   Movea     ski(13)       w003a
047350040330     c                   If        ComDcr > ds_data
047360040330     c                   Eval      *In28 = *On
047370040330     c                   Eval      *In40 = *On
047380040330     c                   Eval      *In90 = *On
047390040330     c                   Eval      VidMsg = msg(129)
047400040330     c                   Eval      VidMsg = %replace(w003a:VidMsg:
047410040330     c                                      %scan('___':VidMsg))
047420040330     c                   EndIf
047430040330     c                   EndIf
047440021129
047450021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
047460021129     c                   If        *In90 and D48Ffr = 'E'
047470021129     c                   Eval      D48Msg = VidMsg
047480021129     c                   Eval      D48Err = '7'
047490021129     c                   GoTo      EndEm71
047500021129     c                   EndIf
047510140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
047520140502     c*   (d48ffr='E' e 26 off)
047530140502     c  n90
047540140502     cann26              if        d48ffr='E'
047550140502     C                   GOTO      ENDEM71
047560140502     c                   endif
047570021129
047580021129     c  NKF
047590021129     cANN26
047600021129     cOR 90              GoTo      For071
047610060302     c* richiamo routine di richiesta/controllo motivazione
047620060302     c                   if        d66mtv = 'S' and w48mct = *blanks
047630060302     c                   exsr      ges_mtv
047640060302     c                   if        olv85f12 = '1'
047650060302     c                   if        ilv85wdw = ' '
047660060302     c                   goto      for071
047670060302     c                   else
047680060302     c                   eval      *inkl = '1'
047690060302     C                   GOTO      ENDEM71
047700060302     c                   endif
047710060302     c                   endif
047720060302     C     *IN90         IFEQ      *ON
047730060302     C     D48FFR        ANDEQ     'E'
047740060302     C                   MOVEL     VIDMSG        D48MSG
047750070308     C                   MOVEL     '7'           D48ERR
047760060302     C                   GOTO      ENDEM71
047770060302     C                   ENDIF
047780060302     c                   endif
047790021129
047800021129      * CMD6 - AGGIORNAMENTO
047810021129     c                   ExSr      Regis7
047820021129
047830021129     c  n91              ExSr      Regis71
047840021129
047850021129     c                   If        *In91 and D48Ffr = 'E'
047860021129     c                   Eval      D48Msg = VidMsg
047870021129     c                   Eval      D48Err = '7'
047880021129     c                   GoTo      EndEm71
047890021129     c                   EndIf
047900021129
047910021129     c   91              GoTo      For071
047920021129
047930021129     c                   EndIf
047940021129
047950021129     c     EndEm71       Tag
047960021129
047970021129      * F12-RITORNO  --> DISALLOCO RECROD
047980021129
047990021129    1c     *INKL         IFEQ      *ON
048000021129      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048010021129     c     D48FFR        OREQ      'E'
048020021129     c     *IN90         ANDEQ     *ON
048030021129     c     D48FFR        OREQ      'E'
048040021129     c     *IN91         ANDEQ     *ON
048050021129
048060021129    2c     *IN10         IFEQ      *ON
048070021129     c                   UNLOCK    FNARB01L
048080021129     c                   UNLOCK    FIAR601L
048090051118     c                   UNLOCK    FiAR901L
048100021129     c                   ELSE
048110021129     c                   UNLOCK    FNBLP01L
048120021129    2c                   END
048130021129      * Comunico al chiamante che è stato premuto F12 valorizzando il
048140021129      * flag D48F12 = 1
048150021129     c   KL              MOVEL     '1'           D48F12
048160021129    1c                   END
048170021129
048180021129     c                   EndSr
048190941223     C*
048200941223     C* EMISSIONE E GESTIONE FORMATO 8 ------------------------------*
048210941223     C     EMFOR8        BEGSR
048220941223     C*
048230941223     C     FOR08         TAG
048240941223     C*
048250950114     C     *IN90         IFEQ      *ON
048260941223     C     D48FFR        OREQ      'S'
048270950607     C     D48FFR        OREQ      'V'
048280941223     C                   WRITE     LR48T03
048290941223     C                   EXFMT     LR48D08
048300941223     C                   END
048310941223     C*
048320941223     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
048330941223     C                   SETOFF                                       2890
048340941223     C                   SETOFF                                       404142
048350941223     C                   SETOFF                                       434445
048360941223     C                   SETOFF                                       464748
048370941223     C                   SETOFF                                       4950
048380941223     C* CMD12 - RITORNO
048390941223     C     *INKL         IFEQ      *OFF
048400941223     C*
048410941223     C* CONTROLLO FORMATO
048420941223     C                   EXSR      CONTR8
048430941223     C*
048440941223     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048450941223     C     *IN90         IFEQ      *ON
048460941223     C     D48FFR        ANDEQ     'E'
048470941223     C                   MOVEL     VIDMSG        D48MSG
048480941223     C                   MOVEL     '8'           D48ERR
048490941223     C                   GOTO      ENDEM8
048500941223     C                   ENDIF
048510140502
048520140502     c* Non gestita uscita per d48ffr='F' ( n90 and n26 d48ffr='E')
048530140502     c* perchè bisognerebbe verificare bene cosa fare in questo caso
048540140502
048550970110     C  NKF
048560970110     CANNKJ
048570970110     CANN26
048580941223     COR 90              GOTO      FOR08
048590060302     c* richiamo routine di richiesta/controllo motivazione
048600060302     c                   if        d66mtv = 'S' and w48mct = *blanks
048610060302     c                   exsr      ges_mtv
048620060302     c                   if        olv85f12 = '1'
048630060302     c                   if        ilv85wdw = ' '
048640060302     c                   goto      for08
048650060302     c                   else
048660060302     c                   eval      *inkl = '1'
048670060302     C                   GOTO      ENDEM8
048680060302     c                   endif
048690060302     c                   endif
048700060302     C     *IN90         IFEQ      *ON
048710060302     C     D48FFR        ANDEQ     'E'
048720060302     C                   MOVEL     VIDMSG        D48MSG
048730070308     C                   MOVEL     '8'           D48ERR
048740060302     C                   GOTO      ENDEM8
048750060302     C                   ENDIF
048760060302     c                   endif
048770941223     C*
048780970110     C* F6 O F10 - AGGIORNAMENTO
048790941223     C                   EXSR      REGIS8
048800991130     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
048810991130     C     *IN91         IFEQ      *ON
048820991130     C     D48FFR        ANDEQ     'E'
048830991130     C                   MOVEL     VIDMSG        D48MSG
048840991130     C                   MOVEL     '8'           D48ERR
048850991130     C                   GOTO      ENDEM8
048860991130     C                   ENDIF
048870941223     C   91              GOTO      FOR08
048880941223     C*
048890950519     C                   ENDIF
048900150112     c* Se non ho passato msg vincolanti, passo msg di avvertimento
048910150112     c*  se ce ne sono
048920150112     c                   if        msgforzatxt<>*blanks
048930150112     C                   MOVEL     msgforzatxt   D48MSG
048940150112     C                   MOVEL     'Z'           D48ERR
048950150112     c                   clear                   msgforzatxt
048960150112     C                   ENDIF
048970950519     C*
048980950519     C     ENDEM8        TAG
048990950104     C* F12-RITORNO  --> DISALLOCO RECORD
049000950519    1C     *INKL         IFEQ      *ON
049010950519     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049020950519     C     D48FFR        OREQ      'E'
049030950519     C     *IN90         ANDEQ     *ON
049040000303     C     D48FFR        OREQ      'E'
049050000303     C     *IN91         ANDEQ     *ON
049060941223     C*
049070941223     C     *IN10         IFEQ      *ON
049080941223     C                   UNLOCK    FNARB01L
049090991005     C                   UNLOCK    FIAR601L
049100051118     C                   UNLOCK    FiAR901L
049110941223     C                   ELSE
049120941223     C                   UNLOCK    FNBLP01L
049130941223     C                   END
049140950607     C* Comunico al chiamante che è stato premuto F12 valorizzando il
049150950607     C* flag D48F12 = 1
049160950607     C   KL              MOVEL     '1'           D48F12
049170941223     C                   END
049180941223     C*
049190950519     C                   ENDSR
049200961211     C* EMISSIONE E GESTIONE FORMATO 9 ------------------------------*
049210961211     C     EMFOR9        BEGSR
049220920120     C*
049230961218     C     FOR09         TAG
049240961218     C*
049250961218     C     *IN90         IFEQ      *ON
049260961218     C     D48FFR        OREQ      'S'
049270961218     C     D48FFR        OREQ      'V'
049280961219     C                   WRITE     LR48T01
049290961218     C                   EXFMT     LR48D09
049300961218     C                   END
049310961218     C*
049320961218     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
049330961218     C                   SETOFF                                       2890
049340970110     C                   SETOFF                                       4041
049350070329     C* F12 - RITORNO
049360961218     C     *INKL         IFEQ      *OFF
049370070329     c
049380070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
049390070329     c                   if        *in87 and *inkr
049400070329     c                   exsr      CALLNOTELDV
049410070329     c                   goto      for09
049420070329     c                   endif
049430070329     C*
049440961218     C*
049450961218     C* CONTROLLO FORMATO
049460961218     C                   EXSR      CONTR9
049470961218     C*
049480961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049490961218     C     *IN90         IFEQ      *ON
049500961218     C     D48FFR        ANDEQ     'E'
049510961218     C                   MOVEL     VIDMSG        D48MSG
049520961218     C                   MOVEL     '9'           D48ERR
049530961218     C                   GOTO      ENDEM9
049540961218     C                   ENDIF
049550140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
049560140502     c*   (d48ffr='E' e 26 off)
049570140502     c  n90
049580140502     cann26              if        d48ffr='E'
049590140502     C                   GOTO      ENDEM9
049600140502     c                   endif
049610970110     C  NKF
049620970110     CANN26
049630961218     COR 90              GOTO      FOR09
049640060302     c* richiamo routine di richiesta/controllo motivazione
049650060302     c                   if        d66mtv = 'S' and w48mct = *blanks
049660060302     c                   exsr      ges_mtv
049670060302     c                   if        olv85f12 = '1'
049680060302     c                   if        ilv85wdw = ' '
049690060302     c                   goto      for09
049700060302     c                   else
049710060302     c                   eval      *inkl = '1'
049720060302     C                   GOTO      ENDEM9
049730060302     c                   endif
049740060302     c                   endif
049750060302     C     *IN90         IFEQ      *ON
049760060302     C     D48FFR        ANDEQ     'E'
049770060302     C                   MOVEL     VIDMSG        D48MSG
049780060316     C                   MOVEL     '9'           D48ERR
049790060302     C                   GOTO      ENDEM9
049800060302     C                   ENDIF
049810060302     c                   endif
049820060316     c                   exsr      regis9
049830060316     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049840060316     C     *IN91         IFEQ      *ON
049850060316     C     D48FFR        ANDEQ     'E'
049860060316     C                   MOVEL     VIDMSG        D48MSG
049870060316     C                   MOVEL     '9'           D48ERR
049880060316     C                   GOTO      ENDEM9
049890060316     C                   ENDIF
049900060316     C   91              GOTO      FOR09
049910041028     c
049920961218     C*
049930961218     C                   ENDIF
049940961218     C*
049950961218     C     ENDEM9        TAG
049960961218     C* F12-RITORNO  --> DISALLOCO RECORD
049970961218    1C     *INKL         IFEQ      *ON
049980961218     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
049990961218     C     D48FFR        OREQ      'E'
050000961218     C     *IN90         ANDEQ     *ON
050010961218     C*
050020961218     C     *IN10         IFEQ      *ON
050030961218     C                   UNLOCK    FNARB01L
050040991005     C                   UNLOCK    FIAR601L
050050051118     C                   UNLOCK    FiAR901L
050060961218     C                   ELSE
050070961218     C                   UNLOCK    FNBLP01L
050080961218     C                   END
050090961218     C* Comunico al chiamante che è stato premuto F12 valorizzando il
050100961218     C* flag D48F12 = 1
050110961218     C   KL              MOVEL     '1'           D48F12
050120961218     C                   END
050130961211     C*
050140961211     C                   ENDSR
050150030123      **
050160030123      * EMISSIONE E GESTIONE FORMATO 10 Mancato ritiro BANCALI ------*
050170030123     c     Emfor10       BegSr
050180030123
050190030123     c     For10         Tag
050200030123
050210030123     c                   If        *In90 or D48Ffr = 'S' or D48Ffr = 'V'
050220030123     c                   Write     Lr48T01
050230030123     c                   Exfmt     Lr48D10
050240030123     c                   EndIf
050250030123
050260030123      * Spengo indicatori di posizionamento cursore
050270030123     c                   Setoff                                       2890
050280030124     c                   Setoff                                       5759
050290030124     c                   Setoff                                       6061
050300070329      * F12 - RITORNO
050310030123     c                   IF        Not *InKl
050320070329     c
050330070329     c* F17 - Note LDV   (solo se abilitato geo 87 on)
050340070329     c                   if        *in87 and *inkr
050350070329     c                   exsr      CALLNOTELDV
050360070329     c                   goto      for10
050370070329     c                   endif
050380030123
050390030123      * CONTROLLO FORMATO
050400030123     c                   ExSr      Contr10
050410030123
050420030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050430030123     c                   If        *In90 and D48Ffr = 'E'
050440030123     c                   Eval      D48Msg = VidMsg
050450030123     c                   Eval      D48Err = 'B'
050460030123     c                   GoTo      EndEm10
050470030123     c                   EndIf
050480140502     c* Se non ho avuto errori e richiamo solo per controlli, esco
050490140502     c*   (d48ffr='E' e 26 off)
050500140502     c  n90
050510140502     cann26              if        d48ffr='E'
050520140502     C                   GOTO      ENDEM10
050530140502     c                   endif
050540030123
050550030123     c  NKF
050560030123     cANN26
050570030123     cOR 90              GoTo      For10
050580060302     c* richiamo routine di richiesta/controllo motivazione
050590060302     c                   if        d66mtv = 'S' and w48mct = *blanks
050600060302     c                   exsr      ges_mtv
050610060302     c                   if        olv85f12 = '1'
050620060302     c                   if        ilv85wdw = ' '
050630060302     c                   goto      for10
050640060302     c                   else
050650060302     c                   eval      *inkl = '1'
050660060302     C                   GOTO      ENDEM10
050670060302     c                   endif
050680060302     c                   endif
050690060302     C     *IN90         IFEQ      *ON
050700060302     C     D48FFR        ANDEQ     'E'
050710060302     C                   MOVEL     VIDMSG        D48MSG
050720070308     C                   MOVEL     'B'           D48ERR
050730060302     C                   GOTO      ENDEM10
050740060302     C                   ENDIF
050750060302     c                   endif
050760030123
050770030123      * CMD6 - AGGIORNAMENTO
050780030123     c                   ExSr      Regis10
050790030123
050800030123     c                   If        *In91 and D48Ffr = 'E'
050810030123     c                   Eval      D48Msg = VidMsg
050820030123     c                   Eval      D48Err = 'B'
050830030123     c                   GoTo      EndEm10
050840030123     c                   EndIf
050850030123
050860030123     c   91              GoTo      For10
050870030123
050880030123     c                   EndIf
050890030123
050900030123     c     EndEm10       Tag
050910030123
050920030123      * F12-RITORNO  --> DISALLOCO RECROD
050930030123
050940030123    1c     *INKL         IFEQ      *ON
050950030123      * D48FFR = E PASSO L'ERRORE AL CHIAMANTE
050960030123     c     D48FFR        OREQ      'E'
050970030123     c     *IN90         ANDEQ     *ON
050980030123     c     D48FFR        OREQ      'E'
050990030123     c     *IN91         ANDEQ     *ON
051000030123
051010030123    2c     *IN10         IFEQ      *ON
051020030123     c                   UNLOCK    FNARB01L
051030030123     c                   UNLOCK    FIAR601L
051040051118     c                   UNLOCK    FiAR901L
051050030124     c                   UNLOCK    Fiar501L
051060030123     c                   ELSE
051070030123     c                   UNLOCK    FNBLP01L
051080030123    2c                   END
051090030123      * Comunico al chiamante che è stato premuto F12 valorizzando il
051100030123      * flag D48F12 = 1
051110030123     c   KL              MOVEL     '1'           D48F12
051120030123    1c                   END
051130030123
051140030123     c                   EndSr
051150061107     C
051160061107     C*--- EMISSIONE E GESTIONE FORMATO 11 ---------------------------*
051170061107     C     EMFOR11       BEGSR
051180061107     C*
051190061107     C     FOR11         TAG
051200061107     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
051210061107    1C     *IN90         IFEQ      *ON
051220061107     C     D48FFR        OREQ      'S'
051230061107     C     D48FFR        OREQ      'V'
051240061107     C                   WRITE     LR48T01
051250061107     C                   EXFMT     LR48D11
051260061107    1C                   END
051270061107     C*
051280061107     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
051290061107     C                   SETOFF                                       2890
051300061107     C                   SETOFF                                       4041
051310061107     C* F12 - RITORNO
051320061107    1C     *INKL         IFEQ      *OFF
051330061107     C*
051340061107     C* CONTROLLO FORMATO
051350061107     C                   EXSR      CONTR11
051360061107     C*
051370061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051380061107    3C     *IN90         IFEQ      *ON
051390061107     C     D48FFR        ANDEQ     'E'
051400061107     C                   MOVEL     VIDMSG        D48MSG
051410061107     C                   MOVEL     'F'           D48ERR
051420061107     C                   GOTO      ENDEM11
051430061107    3C                   ENDIF
051440071025     c
051450071025     c* Se non ho avuto errori e richiamo solo per controlli, esco
051460071025     c*   (d48ffr='E' e 26 off)
051470071025     c  n90
051480071025     cann26              if        d48ffr='E'
051490071025     C                   GOTO      ENDEM11
051500071025     c                   endif
051510061107     C  NKF
051520061107     CANN26
051530061107     COR 90              GOTO      FOR11
051540061107     c* richiamo routine di richiesta/controllo motivazione
051550061107     c                   if        d66mtv = 'S' and w48mct = *blanks
051560061107     c                   exsr      ges_mtv
051570061107     c                   if        olv85f12 = '1'
051580061107     c                   if        ilv85wdw = ' '
051590061107     c                   goto      for11
051600061107     c                   else
051610061107     c                   eval      *inkl = '1'
051620061107     C                   GOTO      ENDEM11
051630061107     c                   endif
051640061107     c                   endif
051650061107     C     *IN90         IFEQ      *ON
051660061107     C     D48FFR        ANDEQ     'E'
051670061107     C                   MOVEL     VIDMSG        D48MSG
051680061107     C                   MOVEL     'F'           D48ERR
051690061107     C                   GOTO      ENDEM11
051700061107     C                   ENDIF
051710061107     c                   endif
051720061107     C*
051730071026     c                   clear                   Aggiobol
051740061107     C                   EXSR      REGIS11
051750061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051760061107     C     *IN91         IFEQ      *ON
051770061107     C     D48FFR        ANDEQ     'E'
051780061107     C                   MOVEL     VIDMSG        D48MSG
051790061107     C                   MOVEL     'F'           D48ERR
051800061107     C                   GOTO      ENDEM11
051810061107     C                   ENDIF
051820061107     C   91              GOTO      FOR11
051830061107     C*
051840061107    1C                   ENDIF
051850061107     C*
051860061107     C     ENDEM11       TAG
051870061107     C*
051880061107    1C     *INKL         IFEQ      *ON
051890061107     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
051900061107    1C     D48FFR        OREQ      'E'
051910061107     C     *IN90         ANDEQ     *ON
051920061107    1C     D48FFR        OREQ      'E'
051930061107     C     *IN91         ANDEQ     *ON
051940061107     C* F12-RITORNO
051950061107    2C     *IN10         IFEQ      *ON
051960061107     C                   UNLOCK    FNARB01L
051970061107     C                   UNLOCK    FiAR901L
051980061107     C                   ELSE
051990061107     C                   UNLOCK    FNBLP01L
052000061107    2C                   END
052010061107     C* Comunico al chiamante che è stato premuto F12 valorizzando il
052020061107     C* flag D48F12 = 1
052030061107     C   KL              MOVEL     '1'           D48F12
052040061107    1C                   END
052050061107     c
052060061107     C                   UNLOCK    Fiar601l
052070061107     C*
052080061107     C                   ENDSR
052090130927     c
052100130927     C*--- EMISSIONE E GESTIONE FORMATO 12  variaz.'"Nx' -------------*
052110130927     C     EMFOR12       BEGSR
052120130927     C*
052130130927     C     FOR12         TAG
052140130927     C* SE PGM RICHIAMATO SOLO SE RICHIESTO E SE NON CI SONO ERRORI
052150130927    1C     *IN90         IFEQ      *ON
052160130927     C     D48FFR        OREQ      'S'
052170130927     C     D48FFR        OREQ      'V'
052180130927     C                   WRITE     LR48T01
052190130927     C                   EXFMT     LR48D12
052200130927    1C                   END
052210130927     C*
052220130927     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
052230130927     C                   SETOFF                                       2890
052240130930     C                   SETOFF                                       41
052250130927     C* F12 - RITORNO
052260130927    1C     *INKL         IFEQ      *OFF
052270130927     c
052280130927     c* F17 - Note LDV   (solo se abilitato geo 87 on)
052290130927     c                   if        *in87 and *inkr
052300130927     c                   exsr      CALLNOTELDV
052310130927     c                   goto      for12
052320130927     c                   endif
052330130927     C*
052340130927     C* CONTROLLO FORMATO
052350130927     C                   EXSR      CONTR12
052360130927     C*
052370130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052380130927    3C     *IN90         IFEQ      *ON
052390130927     C     D48FFR        ANDEQ     'E'
052400130927     C                   MOVEL     VIDMSG        D48MSG
052410130927     C                   MOVEL     'F'           D48ERR
052420130927     C                   GOTO      ENDEM12
052430130927    3C                   ENDIF
052440130927     c
052450130927     c* Se non ho avuto errori e richiamo solo per controlli, esco
052460130927     c*   (d48ffr='E' e 26 off)
052470130927     c  n90
052480130927     cann26              if        d48ffr='E'
052490130927     C                   GOTO      ENDEM12
052500130927     c                   endif
052510130927     C  NKF
052520130927     CANN26
052530130930     COR 90              GOTO      FOR12
052540130927     c* richiamo routine di richiesta/controllo motivazione
052550130927     c                   if        d66mtv = 'S' and w48mct = *blanks
052560130927     c                   exsr      ges_mtv
052570130927     c                   if        olv85f12 = '1'
052580130927     c                   if        ilv85wdw = ' '
052590130927     c                   goto      for12
052600130927     c                   else
052610130927     c                   eval      *inkl = '1'
052620130927     C                   GOTO      ENDEM12
052630130927     c                   endif
052640130927     c                   endif
052650130927     C     *IN90         IFEQ      *ON
052660130927     C     D48FFR        ANDEQ     'E'
052670130927     C                   MOVEL     VIDMSG        D48MSG
052680130927     C                   MOVEL     'F'           D48ERR
052690130927     C                   GOTO      ENDEM12
052700130927     C                   ENDIF
052710130927     c                   endif
052720130927     C*
052730130927     c                   clear                   Aggiobol
052740130927     C                   EXSR      REGIS12
052750130927     c
052760130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052770130927     C     *IN91         IFEQ      *ON
052780130927     C     D48FFR        ANDEQ     'E'
052790130927     C                   MOVEL     VIDMSG        D48MSG
052800130927     C                   MOVEL     'F'           D48ERR
052810130927     C                   GOTO      ENDEM12
052820130927     C                   ENDIF
052830130927     C   91              GOTO      FOR12
052840130927     C*
052850130927    1C                   ENDIF
052860130927     C*
052870130927     C     ENDEM12       TAG
052880130927     C*
052890130927    1C     *INKL         IFEQ      *ON
052900130927     C* D48FFR = E PASSO L'ERRORE AL CHIAMANTE
052910130927    1C     D48FFR        OREQ      'E'
052920130927     C     *IN90         ANDEQ     *ON
052930130927    1C     D48FFR        OREQ      'E'
052940130927     C     *IN91         ANDEQ     *ON
052950130927     C* F12-RITORNO
052960130927    2C     *IN10         IFEQ      *ON
052970130927     C                   UNLOCK    FNARB01L
052980130927     C                   UNLOCK    FiAR901L
052990130927     C                   ELSE
053000130927     C                   UNLOCK    FNBLP01L
053010130927    2C                   END
053020130927     C* Comunico al chiamante che è stato premuto F12 valorizzando il
053030130927     C* flag D48F12 = 1
053040130927     C   KL              MOVEL     '1'           D48F12
053050130927    1C                   END
053060130927     c
053070130927     C                   UNLOCK    Fiar601l
053080130927     C*
053090130927     C                   ENDSR
053100070329     C
053110070329     C*--- richiama pgm per immettere note LDV -----------------------*
053120070329     C     CALLNoteLDV   BEGSR
053130140130     c                   clear                   fnlrn6ds
053140140130     c                   eval      in6mod='I'
053150140130     c                   eval      in6aas=arbaas
053160140130     c                   eval      in6lnp=arblnp
053170140130     c                   eval      in6nrs=arbnrs
053180140130     c                   eval      in6nsp=arbnsp
053190140130     c                   eval      in6pgm='FNLR48R'
053200140130     c                   eval      IN6FGS=arbifp
053210140130     c                   eval      IN6NFV=arbndc
053220140130     c                   eval      IN6DFV=arbddc
053230140130     c                   eval      IN6PDR=arbpdc
053240140130     c                   if        arbdcm=0  and arbndc>0
053250140130     c                   eval      IN6CONS='S'
053260140130     c                   endif
053270140130     c                   call      'FNLRN6R'
053280070329     c                   parm                    kpjba
053290140130     c                   parm                    fnlrn6ds
053300070329     c
053310070329     c                   ENDSR
053320061107     C
053330920120     C*--- CONTROLLI FORMATO2 ----------------------------------------*
053340920120     C     CONTR2        BEGSR
053350941025     C                   SETOFF                                       9028
053360020531     c                   clear                   wdisag
053370030122      * A T T E N Z I O N E!!!
053380030122      * Per la modifica destinatario
053390030122      * Se il nuovo destinatario necessita della consegna particolare
053400030122      * alla fine della REGIS2 viene impostata la causae 'CP' per
053410030122      * poter effettuare una nuova variazione bolla senza uscire dal pgm
053420941020     C***
053430941020     C*** DESTINATARIO  ***
053440941025    1C     *IN01         IFEQ      '1'
053450141112
053460141112     c* messaggio di avviso se pgm non richiamato e bolla con
053470141120     c* giacenza aperta  - Solo se variati dati del destinatario
053480141112     c                   if        *in20=*off and wforzgiac=0
053490141112     c                             and *in04
053500141120    5C     SAVCAD        IFNE      VIDCAD
053510141120     C     SAVLOD        ORNE      VIDLOD
053520141120     C     SAVind        ORNE      VIDind
053530141120     C     SAVprd        ORNE      VIDprd
053540141120     C     SAVnzd        ORNE      VIDnzd
053550141112     c                   eval      wforzgiac=1
053560141112     C                   SETON                                        2890
053570141112     C                   MOVEL     MSG(157)      VIDMSG
053580141112     C                   GOTO      ENDCT2
053590141112     c                   endif
053600141120     c                   endif
053610141112
053620970808     C                   MOVEL     VIDRSD        I14RSC
053630970808     C                   MOVE      VIDRSD        I14RS2
053640970808     C                   MOVEL     VIDIND        I14IND
053650970808     C                   MOVEL     VIDLOD        E14LOC
053660970808     C                   MOVEL     VIDPID        E14PIP
053670970808     C                   MOVEL     VIDPRD        E14PRV
053680970808     C                   MOVEL     VIDNZD        E14NAR
053690970808     C                   MOVEL     VIDCAD        E14CAP
053700970808     C                   MOVEL     VIDFIN        I14ISO
053710970808     C                   MOVEL     'D'           WCLI              1
053720970808     C                   EXSR      CTRIND
053730941020     C*
053740941020     C* CAMPI CHE POSSO AVER IMPOSTATO
053750970808     C                   MOVEL     E14LOC        VIDLOD
053760970808     C                   MOVEL     E14PIP        VIDPID
053770970808     C                   MOVEL     E14PRV        VIDPRD
053780970808     C                   MOVEL     E14NAR        VIDNZD
053790970808     C                   MOVEL     E14CAP        VIDCAD
053800970808     C* ERRORE
053810970808     C   90              GOTO      ENDCT2
053820060111     c* salvo lna desunta da cappario per non sporcarla se destinatario
053830060111     c* che richiede una particolare lna diversa da quella del cappario
053840060111     c                   move      o95lna        d_o95lna
053850020531     c* Verifico se destinatario disagiato
053860020604     c* Per bolle poste non faccio niente
053870060111     c                   clear                   ddstflo
053880020604     c     lnpntw        ifne      'PPT'
053890040702      * Referente e numero di telefono: se campi a video con '#'
053900040702      * sono quelli automatici (dal tisit5) e quindi li pulisco
053910040702     c                   If        %subst(VidRef:1:1) = '#'
053920040702     c                   Clear                   VidRef
053930040702     c                   EndIf
053940040705     c                   If        %subst(VidTeld:1:1) = '#'
053950040702     c                   Clear                   VidTeld
053960040702     c                   EndIf
053970020531     c                   clear                   tisit0ds
053980020531     c                   movel     'E'           it0tla
053990020531     c                   movel     vidnzd        it0naz
054000020531     c                   movel     vidprd        it0prv
054010020531     c                   movel     vidcad        it0cap
054020020531     c                   movel     vidlod        it0loc
054030020531     c                   movel     vidind        it0ind
054040020531     c                   movel     vidrsd        it0rag
054050020531     c                   call      'TISIT5R'
054060020531     c                   parm                    tisit0ds
054070020531     c     ot0err        ifeq      *blanks
054080060113     c                   movel     ot0flo        ddstflo
054090060111   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
054100060111     c                             ot0dos = 'S'
054110020531     c                   movel     ot0dos        wdisag
054120020531     c     wdisag        ifeq      'D'
054130020531     c                   movel     'X'           wdisag
054140020531     c                   endif
054150060111     c                   endif
054160020531     c                   else
054170020531     c                   clear                   wdisag
054180020531     c                   endif
054190060327     c***                If        wdisag <> *Blanks
054200060327     c***                          and %subst(arbgva:1:1) <> 'U' or
054210060327     c***                          wdisag='X' and %subst(arbgva:1:1)='U'
054220060327     c                   if        (wdisag = 'X') OR
054230060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
054240060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
054250060327     c                             (arbdcr=*zeros or arbtcr <> *blanks))
054260040702     c                   If        VidTeld = *Blanks
054270040702     c                   Eval      VidTeld = ot0tel
054280040702     c                   EndIf
054290040702     c                   If        VidRef = *Blanks
054300040702     c                   Eval      VidRef =  ot0ref
054310040702     c                   EndIf
054320040702     c                   EndIf
054330020604     c                   endif
054340060113     c*
054350060113     c                   if        §dstlna > *zeros
054360060111     c                   move      §dstlna       d_o95lna
054370060111     c                   endif
054380970808     C*
054390970808     C* MODIFICATI CAP E/O LOCALITA':
054400970808     C* 1) AGGIORNO A VIDEO IL FLAG INOLTRO E AVVISO CON MESSAGGIO
054410970808     C* 2) SE VARIA ANCHE LA LNA E CAP E LOCALITA SONO DIVERSI DALLA
054420970808     C*    BOLLA EMMETTO MESSAGGIO FORZABILE MANTENENDO COMUNQUE LA
054430970808     C*    LINEA DI ARRIVO E ZONA DELLA BOLLA
054440970808     C     WCAD          IFNE      VIDCAD
054450970808     C     WLOD          ORNE      VIDLOD
054460970808     C                   MOVE      VIDCAD        WCAD
054470970808     C                   MOVE      VIDLOD        WLOD
054480970808     C* FLAG INOLTRO
054490970808     C     O95ISO        IFNE      VIDFIN
054500970808     C                   MOVE      O95ISO        VIDFIN
054510140507     c* NON avviso se richiamo cieco
054520140507     c                   if        d48ffr<>'E'
054530970808     C                   SETON                                        289049
054540970808     C                   MOVEL     MSG(77)       VIDMSG
054550970808     C                   GOTO      ENDCT2
054560140507     c                   endif
054570970808     C                   END
054580970808     C                   END
054590970828     C* FLAG INOLTRO
054600970828     C     VIDFIN        IFEQ      ' '
054610970828     C                   MOVE      O95ISO        VIDFIN
054620140507     c* NON avviso se richiamo cieco
054630140507     c                   if        d48ffr<>'E'
054640970828     C                   SETON                                        289049
054650970828     C                   MOVEL     MSG(77)       VIDMSG
054660010412     C                   GOTO      ENDCT2
054670140507     c                   endif
054680970828     C                   END
054690010412     C  N29
054700010412     CANN71VIDFIN        IFEQ      'D'
054710010412     C     O95PID        ANDEQ     'N'
054720010412     C                   SETON                                        289049
054730010412     C                   MOVEL     MSG(98)       VIDMSG
054740010412     C                   GOTO      ENDCT2
054750010412     C                   ENDIF
054760970808     C*
054770140507     c* Non faccio il seguente controllo se richiamo "cieco"
054780141029     c                   if        %lookup('999':skFilOkdir)=0 and
054790141029     c                             (%lookup(%editc(arblna:'X'):skFilOkdir)=0
054800141029     c                             or %lookup(%editc(dutpou:'X'):skFilOkdir)=0)
054810141029     c                   if        d48ffr<>'E'
054820141029     C     WCAD1         IFNE      VIDCAD
054830141029     C     WLOD1         ORNE      VIDLOD
054840141029     C     Wrsd1         ORNE      VIDrsd
054850141029     C     Wprd1         ORNE      VIDprd
054860141029     C     Wnzd1         ORNE      VIDnzd
054870141029     C     Wind1         ORNE      VIDind
054880141029     C                   MOVE      VIDCAD        WCAD1
054890141029     C                   MOVE      VIDLOD        WLOD1
054900141029     C                   MOVE      VIDrsd        Wrsd1
054910141029     C                   MOVE      VIDind        Wind1
054920141029     C                   MOVE      VIDprd        Wprd1
054930141029     C                   MOVE      VIDnzd        Wnzd1
054940141029     C     d_o95lna      ifne      ARBLNA
054950141029     C     SAVCAD        IFNE      VIDCAD
054960060111     C***  O95LNA        ANDNE     ARBLNA
054970141029     C     SAVLOD        ORNE      VIDLOD
054980060111     C***  O95LNA        ANDNE     ARBLNA
054990141029     C     SAVrsd        ORNE      VIDrsd
055000141029     C     SAVind        ORNE      VIDind
055010141029     C     SAVprd        ORNE      VIDprd
055020141029     C     SAVnzd        ORNE      VIDnzd
055030141029     C                   SETON                                        9028
055040141029     C                   MOVEL     MSG(76)       VIDMSG
055050141029     C                   GOTO      ENDCT2
055060141029     C                   END
055070141029     C                   END
055080141029     C                   END
055090141029     c                   endif
055100141029     c                   endif
055110941020     C**
055120950522     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
055130950522     C* obbligatoria
055140950412     C     *IN05         IFEQ      *OFF
055150950412     C     *IN72         ANDEQ     *OFF
055160950522     C     WDESIE        ANDEQ     'E'
055170950522     C     DESEFT        ANDNE     *BLANKS
055180950412     C     VIDPID        ANDEQ     *BLANKS
055190950412     C                   SETON                                        284590
055200950412     C                   MOVEL     MSG(67)       VIDMSG
055210950412     C                   GOTO      ENDCT2
055220950412     C                   END
055230061116     c
055240061116     c* Se modifica la ragione sociale del destinatario, controllare
055250061116     c*  la partita iva
055260140507     c* Non controllo se chiamata pgm richiamato "cieco"
055270140507     c                   if        d48ffr<>'E'
055280061116     c                   if        vidrsd<>savrsdQ and vidcdf<>*blanks
055290061116     C                   SETON                                        285190
055300061116     C                   MOVEL     MSG(134)      VIDMSG
055310061116     c                   movel     vidrsd        savrsdQ
055320061116     C                   GOTO      ENDCT2
055330061116     C                   END
055340140507     c                   endif
055350061116     c
055360061116     c* La memorizzo anche se cdf=*blanks
055370061116     c                   movel     vidrsd        savrsdQ
055380061115     c
055390061115      * Devo richiamare il nuovo driver fatto per controllare il codice
055400061115      * fiscale
055410061115     c                   if        vidcdf<>*blanks
055420151105     c                   clear                   xcfiva1ds
055430061115     c                   clear                   xcfivamod
055440061115     c                   eval      xcfivapar = vidcdf
055450061115     c                   eval      xcfivanar = vidnzd
055460061115     c                   eval      xcfivaprv = vidprd
055470151105     c                   eval      xcfivacap = vidcad
055480151105     c                   eval      xcfivaloc = vidlod
055490151105     c                   eval      xcfivapiva= vidpid
055500151105     c                   call      'XCFIVAR1'
055510151105     c                   parm                    xcfiva1ds
055520061115     c*
055530061115     c                   if        xcfivaflg < *zeros
055540061115     C                   SETON                                        289051
055550080505     C                   MOVEL     xcfivamsg     VIDMSG
055560061115     C                   GOTO      ENDCT2
055570061115     c                   endif
055580061115     c                   endif
055590120508     c* Parita iva $$ non ammessa per fattura di filiale
055600120508     c                   move      v1cksc        w0040
055610120508     c                   if        %scan('$$':vidpid)>0  and w0040=9999
055620120508     C                   SETON                                        284590
055630120508     C                   MOVEL     MSG(149)      VIDMSG
055640120508     C                   GOTO      ENDCT2
055650120508     c                   endif
055660070508     c
055670140408     c
055680070508     c                   movel     vidffd        wffd
055690141029     c* Il fermo deposito non si può inserire se presente dispo di dirottamento da
055700141029     c* eseguire
055710141029     c                   if        vidffd<>*blanks and arbffd<>wffd
055720141029     c                   clear                   fnlry09ds2
055730141029     c                   eval      ILRY09TCH='T'
055740141029     c                   eval      ILRY09TDIS='D'
055750141029     c                   eval      ILRY09AAS=d48aas
055760141029     c                   eval      ILRY09LNP=d48lnp
055770141029     c                   eval      ILRY09NRS=d48nrs
055780141029     c                   eval      ILRY09NSP=d48nsp
055790141029     c                   eval      kpjbu=fnlry09ds2
055800141031     c                   call      'FNLRY09C'
055810141029     c                   parm                    kpjba
055820141029     c                   eval      fnlry09ds2=kpjbu
055830141029     c                   if        OLRY09ESIT='1'
055840141029     C                   SETON                                        286890
055850141029     C                   MOVEL     MSG(155)      VIDMSG
055860141029     C                   GOTO      ENDCT2
055870141029     c                   endif
055880141029     c                   endif
055890140408     c* Il fermo deposito non si può inserire se presente particolarità consegna
055900140408     c* che prevede che il fermo deposito debba essere autorizzato dal mittente
055910140408     c                   if        vidffd<>*blanks and arbffd<>wffd and
055920140408     c                             arbgma<>*blanks
055930140408     c                   Movel     '7R'          cod
055940140408     c                   Movel(p)  ArbGMa        key
055950140408     c                   ExSr      ctabel
055960140408     c  n30              Movel     TblUni        ds7R
055970140408     c   30              Clear                   ds7R
055980140408     C                   IF        §7RFD='S' AND vidcvr<>'I7'
055990140408     C                   SETON                                        289068
056000140408     C                   MOVEL     MSG(153)      VIDMSG
056010140408     C                   GOTO      ENDCT2
056020140408     C                   ENDIF
056030140408     c                   endif
056040140408     c* Da contattare impostabile solo se immesso il fermo deposito
056050140408     c*  solo se GEO attiva
056060141223     c* NON faccio questo controllo se richiamo cieco
056070141223     c                   if        d48ffr<>'E'
056080140213     c   77              if        (arbffd=wffd or  vidffd='  ') and
056090070508     c                             vidtfd='SI'
056100070508     C                   SETON                                        289052
056110070508     C                   MOVEL     MSG(99)       VIDMSG
056120070508     C                   GOTO      ENDCT2
056130070508     c                   endif
056140141223     c                   endif
056150070508     C
056160070508     c* se immesso fermo deposito e c'e' la consegna richiesta avviso
056170140507     c* NON faccio questo controllo se richiamo cieco
056180140507     c                   if        d48ffr<>'E'
056190140213     c   77              if        vidffd='SI' and arbffd<>wffd and
056200070508     c                             vidtfd='  ' and arbdcr>0  AND WZ3=0
056210070508     C                   SETON                                        289052
056220070508     C                   MOVEL     MSG(143)      VIDMSG
056230070508     C                   EVAL      WZ3=1
056240070508     C                   GOTO      ENDCT2
056250140213     c                   endif
056260140507     c                   endif
056270061115     c
056280941025    1C                   ENDIF
056290941019     C*
056300930505     C***
056310941025    1C     *IN02         IFEQ      '1'
056320930505     C* PESO
056330030203      * Se non è protetto
056340030203   2ac                   If        Not *In88
056350930505     C* NON A 0 O MAGGIORE DEL LIMITE POSTO
056360980406     C     VIDPKF        IFEQ      0
056370941020     C                   SETON                                        479028
056380941020     C                   MOVEL     MSG(2)        VIDMSG
056390911209     C                   GOTO      ENDCT2
056400941025    2C                   END
056410000510     C* DPD NO MAGGIORE DEL LIMITE IMPOSTO
056420020318    2C     LNPNTW        IFEQ      'DPD'
056430020318     C     LNANTW        OREQ      'DPD'
056440000510    3C     VIDPKF        IFGT      §3IPKD
056450000510     C                   SETON                                        479028
056460000510     C                   MOVEA     MSG(52)       K78
056470000510     C                   MOVEL     §3IPKD        W006A             6
056480000510     C                   MOVEA     W006A         SKI(10)
056490000510     C                   Z-ADD     10            I                 3 0
056500000510     C                   EXSR      ABBL
056510000510     C                   MOVEA     SKI(10)       K78(53)
056520000510     C                   MOVE      §3IPKD        W001A
056530000510     C                   MOVEL     W001A         K78(60)
056540000510     C                   MOVEA     K78           VIDMSG
056550000510     C                   GOTO      ENDCT2
056560000510    3C                   END
056570000510    2C                   END
056580000510     C**
056590000510     C**
056600000510     C     VARPKF        IFNE      VIDPKF
056610000510     C                   CLEAR                   WPT1
056620000510     C                   ENDIF
056630000510     C*
056640000510     C     VARPKF        IFNE      VIDPKF
056650000510     C     ARBFVF        ANDEQ     'K'
056660000510     C                   CLEAR                   SET
056670000510     C                   ENDIF
056680000510     C                   MOVEL     VIDPKF        VARPKF
056690000510     C**
056700021121
056710021121     c                   Move      ArbCtr        DueCtr
056720021121
056730021121      * Bolla FedEx con tariffa documenti il peso non può essere superiore a
056740021121      * quanto indicato in tabella "DFE" --> msg forzabile
056750140507     c* Non controllo se progamma richiamato in modalità "cieca"
056760140507     c                   if        d48ffr<>'E'
056770021121If  2c                   If        LnaNtw = 'FED' and DueCtr >= §DfeDalD and
056780021121     c                             DueCtr <= §DfeAlD and VidPkf > §DfePkgD
056790021121     c                             and Wpt1 = *Blanks
056800021121     c                   Eval      *In47 = *On
056810021121     c                   Eval      *In28 = *On
056820021121     c                   Eval      *In90 = *On
056830021121      * Preparo il msg
056840021121     c                   Movea     Msg(116)      K78
056850021121     c                   Movel     §DfePkgD      W006a
056860021121     c                   Movea     W006a         Ski(10)
056870021121     c                   Z-Add     10            i
056880021121     c                   ExSr      Abbl
056890021121     c                   Movea     Ski(10)       K78(50)
056900021121     c                   Move      §DfePkgD      W001a
056910021121     c                   Movel     W001a         K78(57)
056920021121     c                   Movea     K78           VidMsg
056930021121     c                   Eval      Wpt1 = '1'
056940021121     c                   GoTo      EndCt2
056950021121    2c                   EndIf
056960140507     c                   endif
056970051129      * Nel caso di peso totalmente rilevato da VDL posso modificare solo se
056980051129      * inserisco il peso uguale a quello VDL
056990030123     c**!!!              If         ArbNcp = ArbNcl and VidPkc <> VidPkf
057000030123     c                   If         wfpf = 'T' and VidPkc <> VidPkf
057010030122     c                   Seton                                        479028
057020030122     c                   Eval      VidMsg = Msg(121)
057030030122     c                   GoTo      EndCt2
057040030122    2c                   EndIf
057050051129      * Nel caso di peso parzialmente rilevato da VDL posso immettere un peso uguale
057060051129      * o superiore a quello VDL
057070030123     c                   If        wfpf = 'Z' and VidPkf < VidPkc
057080030123     c                   Seton                                        479028
057090030123     c                   Eval      VidMsg = Msg(123)
057100030123     c                   GoTo      EndCt2
057110030123    2c                   EndIf
057120030203   2ac                   EndIf
057130021121
057140930505     C* VOLUME
057150091124      * Se non è protetto
057160091124   2ac                   If        Not *In89
057170091124     c
057180980406     C     VIDVLF        IFEQ      0
057190941215     C                   SETON                                        489028
057200941020     C                   MOVEL     MSG(3)        VIDMSG
057210911209     C                   GOTO      ENDCT2
057220941025    2C                   ENDIF
057230941214     C*
057240941027     C* SE IL TIPO VOLUME E' "Z" NON SI PUO' ABBASSARE
057250941027     C     VIDFVF        IFEQ      'Z'
057260941027     C     VIDVLF        ANDLT     ARBVLF
057270941215     C                   SETON                                        489028
057280941027     C                   MOVEL     MSG(5)        VIDMSG
057290941027     C                   GOTO      ENDCT2
057300941027    2C                   ENDIF
057310091124     c*
057320091124     c* Per volume totaole variabile solo come il totale
057330091124     c                   If         ArbNcr = ArbNcl and Vidvlf <> arbvlc
057340091124     c                   Seton                                        489028
057350091124     c                   Eval      VidMsg = Msg(147)
057360091124     c                   GoTo      EndCt2
057370091124    2c                   EndIf
057380091124     c
057390091124      * Nel caso di vol  parzialmente rilevato da VDL posso immettere un vol  uguale
057400091124      * o superiore a quello VDL
057410091124     c                   If         ArbNcr < ArbNcl and Vidvlf < arbvlc
057420091124     c                              and arbncr>0
057430091124     c                   Seton                                        489028
057440091124     c                   Eval      VidMsg = Msg(148)
057450091124     c                   GoTo      EndCt2
057460091124    2c                   EndIf
057470091124   2ac                   EndIf
057480941214     C*
057490051129     C* SE IL TIPO VOLUME NON E' "Z" MA ESISTE UN VOLUME VDL CHE DIVENT
057500941214     C*  SUPERIORE A QUELLO DA FATTURARE DOPO LA VARIAZIONE, AVVISO E
057510941215     C*  MODIFICO
057520941215     C     VARVLF        IFNE      VIDVLF
057530941215     C                   CLEAR                   SET
057540941215     C                   MOVEL     VIDVLF        VARVLF
057550941215     C                   ENDIF
057560941215     C*
057570051129     C*  POI IMPOSTERO NEL VOLUME DA FATTURARE IL VOLUME VDL
057580941214    2C     SET           IFEQ      0
057590941214     C     ARBVLC        ANDGT     0
057600941214     C     VIDFVF        ANDNE     'Z'
057610941214     C     VIDFVF        ANDNE     'T'
057620941214     C*
057630051129    3C     WVDL          IFEQ      'S'
057640970418    4C     WFVF          IFEQ      'Z'
057650941222     C   06              MOVEL     ZAFA          WSOST             1
057660941222     C  N06              MOVEL     ZAAA          WSOST
057670970418   X4C                   ELSE
057680970418     C   06              MOVEL     TAFA          WSOST             1
057690970418     C  N06              MOVEL     TAAA          WSOST
057700970418    4C                   END
057710970418   X3C                   ELSE
057720970418    4C     WFVF          IFEQ      'Z'
057730941222     C   06              MOVEL     ZAFN          WSOST             1
057740941222     C  N06              MOVEL     ZAAN          WSOST
057750970418     C                   ELSE
057760970418     C   06              MOVEL     TAFN          WSOST             1
057770970418     C  N06              MOVEL     TAAN          WSOST
057780970418    4C                   ENDIF
057790970418    3C                   ENDIF
057800941214     C*
057810941214     C* SE VOLUME SAREBBE DA SOSTITUIRE VEDO SE DIVENTA SUPERIORE
057820941214     C* DEL VOLUME DA FATTURARE
057830941214    3C     WSOST         IFEQ      'S'
057840970418     C                   CLEAR                   WSOSC
057850941214     C*
057860941214    4C     VIDVLF        IFLT      ARBVLF
057870941214     C     VIDVLF        ANDLT     ARBVLC
057880970418     C* PRENDO FLAG DI SOSTITUIBILITA' VOLUME REALE
057890970418     C                   MOVEL     '7T'          COD
057900970418     C                   MOVEL(P)  'R'           KEY
057910970418     C                   EXSR      CTABEL
057920970418     C   30              CLEAR                   DS7T
057930970418     C  N30              MOVEL     TBLUNI        DS7T
057940970418     C     WFVF          IFEQ      'Z'
057950970418     C                   MOVEL     §7TSOZ        WSOSC
057960970418     C                   ELSE
057970970418     C                   MOVEL     §7TSOT        WSOSC
057980970418     C                   END
057990970418     C* (SOLO SE IL VOLUME E' SOSTITUIBILE)
058000970418    5C     WSOSC         IFEQ      'S'
058010140507     C                   ADD       1             SET
058020140507     c* non segnalo se pgm richiamato in modalità "cieca"
058030140507     c                   if        d48ffr<>'E'
058040941214     C                   SETON                                        482890
058050941214     C                   MOVEL     MSG(58)       VIDMSG
058060941214     C                   GOTO      ENDCT2
058070140507     c                   endif
058080970418    5C                   ENDIF
058090941214    4C                   ENDIF
058100941214     C*
058110941214     C* SE VOLUME AUTOMATICO E MODIFICATO IL PESO CONTROLLO
058120970418   4AC     *IN88         IFEQ      *OFF
058130941214     C                   Z-ADD     VIDVLF        D20VLU
058140941214     C                   EXSR      VOLAUT
058150970418    4C     D20VLU        IFLT      ARBVLC
058160970418     C                   MOVEL     '7T'          COD
058170970418     C                   MOVEL(P)  ARBFVF        KEY
058180970418     C                   EXSR      CTABEL
058190970418     C   30              CLEAR                   DS7T
058200970418     C  N30              MOVEL     TBLUNI        DS7T
058210051129     C* FLAG DI SOSTITUIBILITA' DA VOL.VDL
058220970418     C     WFVF          IFEQ      'Z'
058230970418     C                   MOVEL     §7TSOZ        WSOSC
058240970418     C                   ELSE
058250970418     C                   MOVEL     §7TSOT        WSOSC
058260970418     C                   END
058270970418    5C     WSOSC         IFEQ      'S'
058280140507     C                   ADD       1             SET
058290140507     c* non segnalo se pgm richiamato in modalità "cieca"
058300140507     c                   if        d48ffr<>'E'
058310941215     C                   SETON                                        472890
058320941214     C                   MOVEL     MSG(58)       VIDMSG
058330941214     C                   GOTO      ENDCT2
058340140507     c                   endif
058350970418    5C                   ENDIF
058360941214    4C                   ENDIF
058370970418   4AC                   ENDIF
058380941214    3C                   ENDIF
058390941214    2C                   ENDIF
058400941214     C*
058410941027    1C                   ENDIF
058420910429     C*
058430910429     C     ENDCT2        ENDSR
058440911212     C*
058450911209     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE IX -----------------*
058460911209     C     REGIS2        BEGSR
058470130412     c                   clear                   wpda_ndc
058480130412     c                   clear                   wpda_ddc
058490130412     c                   clear                   wpda_dcm
058500950104     C*  GIRO UDATE
058510950104     C                   TIME                    W0140            14 0
058520950104     C* UDATE IN GGMMAAAA
058530950104     C                   MOVE      W0140         WDTGIO            8 0
058540950104     C*
058550950104     C* UDATE IN AAAAMMGG
058560950104     C                   Z-ADD     WDTGIO        G02DAT
058570950104     C                   MOVEL     *BLANK        G02ERR
058580950104     C                   CALL      'XSRDA8'
058590950104     C                   PARM                    WLBDAT
058600950104     C                   MOVEL     G02INV        DATEU             8 0
058610941213     C                   MOVEL     KNMUS         ARBPRU
058620941221     C* PRENDO I VALORI ASSOLUTI
058630941221     C                   MLLZO     1             VIDPKF
058640941221     C                   MLLZO     1             VIDVLF
058650041007     C* SALVO VOL/PKG/FTC/RSD/CAP/FIN VECCHIO IN FNARB
058660950114     C                   MOVEL     ARBVLF        OLDVLF
058670950114     C                   MOVEL     ARBFVF        OLDFVF
058680950114     C                   MOVEL     ARBPKF        OLDPKF
058690041007     C                   MOVEL     ARBvlc        OLDvlc
058700950114     C                   MOVEL     ARBRSD        OLDRSD
058710150206     C                   MOVEL     ARBLOD        OLDLOD
058720950114     C                   MOVEL     ARBCAD        OLDCAD
058730950114     C                   MOVEL     ARBFIN        OLDFIN
058740041007     C                   MOVEL     ARBFFD        OLDFFD
058750070320     c                   movel     VIDFFD        WFFD
058760151230     c* (Se variazione fatta da Arrivo su bolla ancora da partire non devo inviare)
058770151222     c                   if        not *in10 or wfile='A'
058780920120     C* INVIO PER SEDE
058790941025     C                   MOVEL     'FNARBD0T'    SFILE
058800920120     C* ALLOCO
058810911212     C                   EXSR      ALLOC
058820941027     C* ERRORI IN ALLOCAZIONE
058830941212    1C     *IN91         IFEQ      *ON
058840941025     C                   GOTO      ENDRG2
058850941212    1C                   ENDIF
058860070515    c
058870070515     c* Se modificato FERMO DEPOSITO, devo allocare record L di ar4
058880070515     c*  per blocco/sblocco telefonata
058890070515     c                   clear                   wesar4L           1
058900070515    1c                   if        Wffd<>oldFFD
058910070515     c                   eval      TipoEla='C'
058920070515     c                   exsr      GestAR4L
058930070515     c
058940070515    2c                   if        *in91
058950070515     c                   exsr      DLCSED
058960070525     C                   SETON                                        2891
058970070515     C                   GOTO      ENDRG2
058980070515    2c                   endif
058990070515    1c                   endif
059000070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
059010070515     c* iornare direttamente il file
059020070515     c                   clear                   waltrabolla       1
059030151222     C   10KARB          CHAIN     FNBLP01L                           3091
059040151222     C  n10KARB          CHAIN     FNARB01L                           3091
059050070515     c*
059060070515     c                   if        *in91
059070070515     C                   MOVEL     MSG(4)        VIDMSG
059080070515     c*
059090070515     c                   exsr      DLCSED
059100070515     c                   unlock    fiar401l
059110070515     c
059120070525     C                   SETON                                        2891
059130070515     C                   GOTO      ENDRG2
059140070515     c                   else
059150070515     c                   if        *in30
059160070515     c                   eval      waltrabolla='N'
059170070515     c                   else
059180070515     c* Salvo alcuni campi dell'altro file che potrebbero, per tempi
059190070515     c*  di aggiornamento, essere diversi.
059200070515     c                   eval      waltrofvf=arbfvf
059210070515     c                   eval      waltrovlf=arbvlf
059220070515     c                   eval      waltrocpi=arbcpi
059230130411     c* salvo campi di arb che mi servono per invio dati a pda
059240151222     c  n10              eval      wpda_ndc=arbndc
059250151222     c  n10              eval      wpda_ddc=arbddc
059260151222     c  n10              eval      wpda_dcm=arbdcm
059270151222     C   10KARB          CHAIN     FNARB01L                           30
059280151222     C  n10KARB          CHAIN     FNBLP01L                           30
059290070515     c* Alla fine per non sporcarmi i campi del record, richaino il
059300070515     c*  record originale che sto variando
059310151222     c   10              eval      wpda_ndc=arbndc
059320151222     c   10              eval      wpda_ddc=arbddc
059330151222     c   10              eval      wpda_dcm=arbdcm
059340070515     c                   endif
059350070515     c                   endif
059360151222     c                   endif
059370070207     C*
059380070207     C* SE SI TRATTA DI BOLLA FRANCO, SALVO IL CAMPO DELLA PARTITA IVA
059390151222     C*   10
059400151222     C*AN 05              MOVEL     ARBCPI        SAVCPI           16
059410151222     C*   10
059420151222     C*AN 05              MOVEL     *BLANKS       ARBCPI
059430070207     C*
059440151222     C*  N10
059450151222     C*AN 06              MOVEL     ARBCPI        SAVCPI           16
059460151222     C*  N10
059470151222     C*AN 06              MOVEL     *BLANKS       ARBCPI
059480151222     c                   if        (wfile='A' and *in05) or
059490151222     c                             (wfile='P' and *in06)
059500151222     C                   MOVEL     ARBCPI        SAVCPI           16
059510151222     C                   MOVEL     *BLANKS       ARBCPI
059520151222     c                   endif
059530070207     C*
059540061116     c* Se variazione di destinatario chaino anche fiar4 record Q se
059550061116     c*  mi serve
059560141215     c                   if        wnoindir=' '
059570061116     c                   if        *in01 and not *in05
059580061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
059590061116     c                             (vidcdf<>*blanks and war4Q<>' ')
059600061116     C                   MOVEL     'Q'           WTRC
059610061116     C     KARB2         CHAIN     FiAR4000                           3091
059620061116     c*
059630061116     c                   if        *in91
059640061116     C                   MOVEL     MSG(4)        VIDMSG
059650070515     c
059660151223     c                   if        not *in10 or wfile='A'
059670070515     c                   exsr      DLCSED
059680151223     c   10              unlock    fnblp01l
059690151223     c  N10              unlock    fnarb01l
059700151223     c                   endif
059710070525     C                   SETON                                        2891
059720061116     C                   GOTO      ENDRG2
059730061116     c                   endif
059740061116     c                   endif
059750061116     c                   endif
059760141215     c                   endif
059770061116     c
059780061116     c* Verifico se prima c'era
059790911212     C*
059800911209     C                   Z-ADD     DATEU         ARBDTV
059810911209     C                   TIME                    ARBORV
059820911209     C                   MOVEL     VIDCVR        ARBCVB
059830911209     C*
059840060130     C* SE DA STORICIZZARE
059850060130     C**   *IN10         IFEQ      *ON
059860060130    1C**   D66FSA        ANDEQ     'S'
059870060130     C**   *IN10         OREQ      *OFF
059880060130    1C**   D66FSP        ANDEQ     'S'
059890060130    1C     D66FSA        ifeq      'S'
059900941212     C                   MOVEL     SAVRD2        ARBRD2
059910950114     C* SE VENGO DA VARIAZIONE MITTENTE, IL VOLUME LO STORICIZZO CON I
059920950114     C* CAMPI CHE HO SALVATO
059930950114     C     *IN24         IFEQ      *ON
059940950114     C                   MOVEL     MITFVF        ARBFVF
059950950114     C                   MOVEL     MITVLF        ARBVLF
059960950114     C                   MOVEL     MITFVF        ARBFVB
059970950114     C                   MOVEL     MITVLF        ARBVLB
059980950114    1C                   ENDIF
059990011108      *
060000011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
060010011108      * è in partenza 'P' o in arrivo 'A'
060020011108     C     *IN10         IFEQ      *ON
060030160119     c                   if        wfile='P'
060040160119     C                   MOVE      'a'           ARBPRU
060050160119     c                   else
060060011108     C                   MOVE      'A'           ARBPRU
060070160119     c                   endif
060080011108     C                   ENDIF
060090011108     C     *IN10         IFEQ      *OFF
060100011108     C                   MOVE      'P'           ARBPRU
060110011108     C                   ENDIF
060120011108      *
060130941014     C                   WRITE     FNARBD00
060140130927     c
060150130927     c* Memorizzo anche referente e telefono se si tratta di varazione "I0"
060160131009     c                   if        *in01 and
060170131009     c                                  (vidref<>par5ref or vidteld<>par5teld)
060180131204     c                   clear                   darbn_ref
060190130927     c                   eval      §arbnref = par5ref
060200130927     c                   eval      §arbnteld= par5teld
060210131204     c                   eval      arbuni70 = darbn_ref
060220130930     c                   eval      arbtrd='REF'
060230130927     c                   write     FNARBN00
060240130927     c                   endif
060250950114     C*
060260950114     C     *IN24         IFEQ      *ON
060270950114     C                   MOVEL     OLDFVF        ARBFVF
060280950114     C                   MOVEL     OLDVLF        ARBVLF
060290950114     C                   MOVEL     OLDFVF        ARBFVB
060300950114     C                   MOVEL     OLDVLF        ARBVLB
060310950114    1C                   ENDIF
060320950114    1C                   ENDIF
060330060301     c* storicizzazione motivazione
060340060301     c     d66mtv        ifeq      'S'
060350060301     c                   exsr      sto_mtv
060360060301     c                   endif
060370911209     C*
060380941025    1C     *IN01         IFEQ      *ON
060390040224
060400040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
060410040224      * - Fermo deposito
060420040224      * devo memorizzare i dati della bolla prima della modifica
060430070320if  3c   10              If        wffd <> oldFfd
060440041022     c                   ExSr      RegAr5AR
060450040224    3c                   EndIf
060460941025     C*
060470941025     C* SE IMMESSA ANCHE LA 2 RAGIONE SOCIALE
060480141212     c                   if        wnoindir=' '
060490060223     c                   clear                   d19ftr
060500160115     c                   if        *in10 and wfile='P'
060510160115     c                   eval      d19ftr='T'
060520160115     c                   endif
060530941025     C                   MOVEL     ARBAAS        D19AAS
060540941025     C                   MOVEL     ARBLNP        D19LNP
060550941025     C                   MOVEL     ARBNRS        D19NRS
060560941025     C                   MOVEL     ARBNSP        D19NSP
060570941025     C                   MOVEL     'D'           D19TRC
060580970520     C                   MOVE      VIDRSD        D19NT1
060590941025     C                   CLEAR                   D19NT2
060600051114     c
060610941025     C                   CALL      'FNLV19R'
060620030612     C                   PARM                    Fnlv19DS
060630941025     C*
060640941025     C* ERRORE: RECORD NON VINCOLABILE
060650941212    2C     D19ERR        IFEQ      '2'
060660941025     C                   MOVEL     MSG(4)        VIDMSG
060670151223     c                   if        not *in10 or wfile='A'
060680070515     c                   exsr      dlcsed
060690070515     c   10              unlock    fnblp01l
060700070515     c  N10              unlock    fnarb01l
060710070515     c                   unlock    fiar401l
060720151230     c                   endif
060730151223     C                   SETON                                        2891
060740941025     C                   GOTO      ENDRG2
060750941212    2C                   ENDIF
060760141212     c                   endif
060770041006    c
060780950626     C*
060790950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
060800950626     C*     NELLA P.IVA
060810950626     C     VIDISD        IFEQ      '$$'
060820950626     C     VIDCPD        ANDEQ     *BLANKS
060830950626     C                   MOVEL     'PRIVATO'     VIDCPD
060840950626     C                   ENDIF
060850941025     C*
060860141212     c                   if        wnoindir=' '
060870941027     C                   MOVEL     VIDRSD        ARBRSD
060880970520     C                   MOVE      D19NT1        ARBRD2
060890911209     C                   MOVEL     VIDIND        ARBIND
060900911209     C                   MOVEL     VIDLOD        ARBLOD
060910911209     C                   MOVEL     VIDCAD        ARBCAD
060920060705     c* se spedizione export DPD diretta in Portogallo devo aggiungere al
060930060705     c* cap immesso tanti 0 a dx quanti ne servono per arrivare ad una
060940060705     c* lunghezza di cap di 7 byte
060950060705     c                   if        lnantw='DPD' and vidnzd = 'P  '
060960060705     c     ' '           checkr(e) arbcad:7      NN                1 0
060970060705     c                   if        %found
060980060705     c                   eval      %subst(arbcad:nn+1:7-nn)=*all'0'
060990060705     c                   endif
061000060705     c                   endif
061010060705     c*
061020911209     C                   MOVEL     VIDPRD        ARBPRD
061030941220     C                   MOVEL     VIDPID        ARBCPI
061040941220     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
061050941220     C*  IL CODICE DELLA P.IVA
061060941220     C     VIDCPD        IFNE      *BLANKS
061070941220     C     VIDISD        ANDEQ     *BLANKS
061080941220     C                   CLEAR                   ARBCPI
061090941220     C                   MOVEL     VIDCPD        ARBCPI
061100941220     C                   ENDIF
061110041027     c                   eval      newcpi=arbcpi
061120041027     c
061130060222     C* SE ESISTE RECORD P.IVA IN FIAR4 LO AGGIORNO
061140950413     C* SOLO SE P.IVA SPROTETTA
061150950413     C     *IN05         IFEQ      *OFF
061160950413     C     *IN72         ANDEQ     *OFF
061170950413     C                   MOVEL     'P'           WTRC
061180060222     C     KARB2         CHAIN     FIAR401L                           30
061190060222     C     *IN30         IFEQ      *OFF
061200060223     c                   move      ar4not        wcpifile
061210060223     c                   if        wcpifile <> arbcpi
061220060223     c                   movel     'T'           ar4ftr
061230060223     c                   z-add     dateu         ar4dtr
061240060223     c                   z-add     dateu         ar4duv
061250060222     C                   MOVE      ARBCPI        AR4NOT
061260060222     C                   UPDATE    FIAR4000
061270060223     c                   else
061280060223     c                   unlock    fiar401l
061290060223     c                   endif
061300060222     C                   END
061310950413     C                   END
061320141215     c                   endif
061330941212     C*
061340931207    2C     VIDFFD        IFEQ      'SI'
061350950222     C                   MOVEL     'C'           ARBFIN
061360930528     C                   MOVEL     'S'           ARBFFD
061370931207   X2C                   ELSE
061380930528     C                   MOVEL     ' '           ARBFFD
061390141212     c                   if        wnoindir=' '
061400930528     C                   MOVEL     VIDFIN        ARBFIN
061410141212     c                   endif
061420931207    2C                   END
061430110905     c
061440110905     c* se attiva procedura centro storico e c'e' il giro sulla bolla --> ricalcolo se
061450110905     c*  non è stato forzato a video
061460151230     c                   if        wnoindir=' '  and
061470151230     c                             (not *in10 or wfile='A')
061480110906     c                   if        vidffd=*blanks
061490110906     c                   if        (oldfin<>arbfin and arbfin=o95iso)
061500110906     c                             or oldffd<>vidffd
061510110906     c*****                        or oldfin=arbfin
061520110906     c
061530110905     c     karb          chain     fiarg01l
061540110905     c                   if        %found(fiarg01l) and argcgi<>*blanks
061550110905     c                             and olvstprop<>'N'
061560110905     c                   clear                   fnlvstds
061570110905     c                   eval      ilvstaas=arbaas
061580110905     c                   eval      ilvstlnp=arblnp
061590110905     c                   eval      ilvstnrs=arbnrs
061600110905     c                   eval      ilvstnsp=arbnsp
061610110905     c                   eval      ilvstmgs=arbmgs
061620110905     c                   eval      ilvstfin=arbfin
061630110905     c                   eval      ilvstcgi=argcgi
061640110906     c                   if        lnacons>0
061650110906     c                   eval      ilvstpoc=lnacons
061660110906     c                   else
061670110906     c                   eval      ilvstpoc=arblna
061680110906     c                   endif
061690110906     c                   if        arbddc>0
061700110906     c                   eval      ilvstdat=arbddc
061710110906     c                   else
061720110906     c                   eval      ilvstdat=dateu
061730110906     c                   endif
061740110905     c                   call      'FNLVSTR'
061750110905     c                   parm                    kpjba
061760110905     c                   parm                    fnlvstds
061770110906     c
061780110906     c* se aggiornato flag inoltro lo memorizzo nella variazione
061790110906     c                   if        OLVSTFINAG='S'
061800110906     c                   eval      arbfin=olvstfin
061810110905     c                   endif
061820110906     c
061830110906     c                   endif
061840110906     c                   endif
061850110906     c                   endif
061860141212     c                   endif
061870110905
061880970808     C* CODICE TASSAZIONE
061890141212     c                   if        wnoindir=' '
061900970808     C                   MOVEL     O95CTS        ARBCTS
061910141212     c                   endif
061920130606
061930031120
061940130930     c                   exsr      AggioREF
061950130930     c
061960931207    1C                   END
061970941025     C**
061980920319     C* RICALCOLO DEL VOLUME
061990941025     C**
062000941025    1C     *IN02         IFEQ      *ON
062010950114     C*
062020950114     C* SE VARIATO MITTENTE PER I PADRONCINI USO QUELLI SALVATI
062030941102     C* PULIZIA DELLA DS PER IMPOSTARE IL VOLUME DA FATTURARE VARIATO
062040030612     C                   CLEAR                   Fnlv20DS
062050941102     C*
062060941025     C* VOLUME REALE
062070041007    2C     VIDVLF        IFNE      oldVLF
062080941102     C                   MOVEL     'R'           D20FVL
062090941102     C                   Z-ADD     VIDVLF        D20VLU
062100930505     C*
062110931207   X2C                   ELSE
062120941025     C*
062130941025     C* VEDO SE DEVO MODIFICARE IL VOLUME DA FATTURARE
062140941214     C                   EXSR      VOLAUT
062150941102    2C                   ENDIF
062160941102     C**
062170941102     C* SE HO MODIFICATO IL VOLUME DA FATTURARE CALL A FNLV20R
062180941102     C*  PER DETERMINARE IL VOLUME BOLLETTATO
062190941102     C**
062200941102    2C     D20VLU        IFGT      0
062210941222     C                   MOVEL     D48TBO        D20TBO
062220941102     C                   MOVEL     'F'           D20TVL
062230041008     C                   MOVEL     arbFVB        D20FVB
062240041008     C                   MOVEL     arbVLB        D20VLB
062250041007     C                   MOVEL     oldFVF        D20FVF
062260041007     C                   MOVEL     oldVLF        D20VLF
062270030612     C                   MOVEL     Fnlv20DS      KPJBU
062280941102     C                   CALL      'FNLV20R'
062290941102     C                   PARM                    KPJBA
062300941102     C*
062310030612     C                   MOVEL     KPJBU         Fnlv20DS
062320941102     C*
062330941102     C                   MOVEL     D20FVB        ARBFVB
062340941102     C                   MOVEL     D20VLB        ARBVLB
062350941102     C                   MOVEL     D20FVF        ARBFVF
062360941102     C                   MOVEL     D20VLF        ARBVLF
062370941102    2C                   ENDIF
062380941214     C*
062390051129     C* SE DEVO SOSTITUIRE COL VOLUME VDL LO FACCIO
062400941214     C     WSOST         IFEQ      'S'
062410970418     C* WSOSC INDICA SE IL TIPO VOLUME DETERMINATO PER LA BOLLA E'
062420051129     C* SOSTITUIBILE DAL VOLUME VDL.
062430970418     C* IN CASO DI VARIAZIONE VOLUME WSOSC SARA' QUELLO DEL TIPO VOLUME
062440970418     C* "R"; IN CASO DI VARIAZIONE DI PESO WSOSC SARA' QUELLO DEL TIPO
062450970418     C* VOLUME DELLA BOLLA
062460041007     C     oldVLC        ANDGT     ARBVLF
062470970418     C     WSOSC         ANDEQ     'S'
062480041007     C                   Z-ADD     oldVLC        ARBVLF
062490970418     C                   MOVE      WFVF          ARBFVF
062500941214     C                   ENDIF
062510920319     C*
062520990628     C  N10              MOVEL     VIDPKF        ARBPKB
062530941102     C                   MOVEL     VIDPKF        ARBPKF
062540041007     c
062550041007    2C     *IN24         IFEQ      *ON
062560041007     C                   MOVEL     MITFVF        OLDFVF
062570041007     C                   MOVEL     MITVLF        OLDVLF
062580041007    2C                   ENDIF
062590941102    1C                   ENDIF
062600941212     C*
062610041007     C* Ora invio solo a sede
062620151223     c                   if        not *in10 or wfile='A'
062630070503     C     D66FFI        ifeq      'S'
062640950227     C*
062650950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
062660950413    2C     §3ABSD        IFEQ      ' '
062670941102     C                   OPEN      FNARBD0T
062680941014     C                   WRITE     FNARBDTT
062690941102     C                   CLOSE     FNARBD0T
062700950227    2C                   END
062710911212     C* DISALLOCO OGGETTO
062720941212     C                   EXSR      DLCSED
062730950208    1C                   END
062740911209     C*
062750950113     C* AGGIORNO FILE TASSAZIONE PADRONCINI
062760950113     C                   EXSR      AGGPDS
062770151223     c                   endif
062780931207     C*
062790941102     C* AGGIORNO
062800151223     C**   10
062810151223     C**AN 05              MOVEL     SAVCPI        ARBCPI
062820151223     C**  N10
062830151223     C**AN 06              MOVEL     SAVCPI        ARBCPI
062840151223     c                   if        (wfile='A' and *in05) or
062850151223     c                             (wfile='P' and *in06)
062860151223     C                   MOVEL     SAVCPI        ARBCPI
062870151223     c                   endif
062880941212     C*
062890151223     c                   if        wfile='A'
062900151223     C                   UPDATE    FNARB000
062910151223     c                   else
062920151223     C                   UPDATE    FNBLP000
062930151223     c                   endif
062940150206     c
062950150206     c* memorizzo la località normalizzata
062960150206     c                   if        vidlod<>normlod OR WAR4X='E'
062970150206     C
062980150211     c                   eval      d19ftr='T'
062990150206     c                   eval      d19agg='E'
063000150203     C                   MOVEL     ARBAAS        D19AAS
063010150203     C                   MOVEL     ARBLNP        D19LNP
063020150203     C                   MOVEL     ARBNRS        D19NRS
063030150203     C                   MOVEL     ARBNSP        D19NSP
063040150203     C                   MOVEL     'X'           D19TRC
063050150206     C                   IF        VIDLOD<>NORMLOD
063060150203     C                   MOVE      normlod       D19NT1
063070150206     C                   ELSE
063080150206     C                   CLEAR                   D19NT1
063090150206     C                   ENDIF
063100150206     C                   CLEAR                   D19NT2
063110150203     c
063120150203     C                   CALL      'FNLV19R'
063130150203     C                   PARM                    Fnlv19DS
063140150206     c                   endif
063150150203     c
063160151230     c* Salto le seguenti istruzioni se
063170151223     c* dall'arrivo sono in manutenzione di una bolla in partenza ancora da
063180151223     c* partire
063190151223     c                   if        not *in10 or wfile='A'
063200070320     c
063210070320     c* Se aggiunto   Fermo Deposito, senza consegna richiesta,
063220070320     c*  imposto il blocco. conconsegna richiesta lo tolgo
063230070320     c* Se tolgo      Fermo Deposito, tolgo il relativo blocco
063240070320     c
063250070320     c* Devo richainare e aggiornare (perchè nel frattempo ho letto
063260070320     c*  altri record fi FIAR4
063270151223
063280070320    1c                   if        Wffd<> oldffd
063290070320     c                   eval      tipoEla='C'
063300070320     c                   exsr      GestAR4L
063310070320     c
063320070320     c* Se allocato pazienza (non aggiorno ma vado avanti
063330070320     c*  fermarmi in questo punto non è bello,lascerebbe la
063340070320     c*  transazione a metà)
063350070320    2c                   if        not *in91
063360070320     c
063370070508    3c****               if        arbdcr=0 and wffd='S'
063380070508     c* imposto il flag telefonata in base a quanto indicato a video
063390070508     c                   if        vidtfd='SI'
063400070508     c                   clear                   §b4ltfd
063410070508     c                   else
063420070320     c                   eval      §b4ltfd='S'
063430070508     c                   endif
063440070508     c
063450070508     c****               else
063460070508     c****               clear                   §b4ltfd
063470070508    3c****               endif
063480070320     c                   eval      tipoEla='A'
063490070320     c                   exsr      GestAR4L
063500140212     c* richiamo pgm per eventuale scrittura/cancellazione file spia
063510150603     c* per aggiungere il file spia per la sped, la bolla non deve essere in stati
063520150603     c*  particolari
063530150603     c                   clear                   tnsd99ds
063540150603     c                   clear                   statofd           1
063550150603     c                   if        wffd='S'
063560150603     C                   MOVEL     'L'           D98tla
063570150603     C                   MOVEL     'A'           D98tbo
063580150603     C                   Z-ADD     arbaas        D98aas
063590150603     C                   Z-ADD     arblnp        D98lnp
063600150603     C                   Z-ADD     arbnrs        D98nrs
063610150603     C                   Z-ADD     arbnsp        D98nsp
063620150603     C*
063630150603     C                   call      'TNSD99R'
063640150603     C                   parm                    tnsd99ds
063650150603     c                   if        D98SPCDEE='FFD' or %subst(D98SPCDEE:1:2)='FD'
063660150603     c                   eval      statofd='S'
063670150603     c                   endif
063680150603     c                   endif
063690150603     c
063700150603     c*****              if        wffd=*blanks or vidtfd=*blanks
063710150603     c                   if        wffd=*blanks or (vidtfd=*blanks and
063720150603     c                             statofd='S')
063730140212     c                   clear                   trtcp2ds
063740140213     c                   if        wffd=*blanks
063750140212     c                   eval      t2chiamata='A'
063760140212     c                   endif
063770140212     c                   eval      t2chi= 'F'
063780140212     c                   eval      T2AAS=d48aas
063790140212     c                   eval      T2LNP=d48lnp
063800140212     c                   eval      T2NRS=d48nrs
063810140212     c                   eval      T2NSP=d48nsp
063820140213     c* passo in t2fgs la lna (serve per far funzionare il chiodo nel trtcp2r)
063830140213     c                   eval      T2fgs=arblna
063840140212     c                   call      'TRTCP2R'
063850140212     c                   parm                    trtcp2ds
063860140213     c                   endif
063870070320     c
063880070320    2C                   END
063890070320    1C                   END
063900070320     c
063910041007     c* Per l'altro file sistemo eventuale volume da fatturare e P.I
063920041007     c*  e aggiorno
063930041007     c                   if        waltrabolla=' '
063940041007     c   02              if        waltrofvf='T'  or
063950041007     c                             (waltrofvf='Z' and arbfvf<>'T' aND
063960041007     C                              WALTROVLF> arbvlf)
063970041008     c                   movel     waltrofvf     arbfvf
063980041008     c                   movel     waltrovlf     arbvlf
063990041007     c                   endif
064000041007     c* Se 1°bolla franco, tengo la p.i. presente nel record
064010151223     c                   if        (*in10 and *in06) or
064020151223     c                             (not *in10 and *in05)
064030041007     c                   eval      arbcpi=waltrocpi
064040041027     c                   else
064050041027     c                   eval      arbcpi=NEWCPI
064060041007     c                   endif
064070041007     c
064080041007     c* aggiornamenti di campi mirati
064090041007     C   10              except    arbdblp
064100041007     C  N10              except    arbdarb
064110041007     c                   endif
064120151223
064130151223     c                   endif
064140950221     C*
064150950221     C* DISALLOCO  I FILES CHE NON USO
064160051118     C   10              UNLOCK    FiAR901L
064170991001     C                   UNLOCK    FIAR601L
064180061116     c
064190061116     c* Se si tratta di bolla in assegnato con possibilità di scrivere il
064200061116     c*  cod fiscale, richiamo la REGIS11 per la memorizzazione
064210151230     c                   if        wnoindir=' ' and
064220151230     c                             (not *in10 or wfile='A')
064230061116     c                   if        *in01 and not *in05
064240061116     c                   if        (vidcdf=*blanks  and war4Q='D') or
064250061116     c                             vidcdf<>*blanks
064260061116     c                   eval      v11mide='D'
064270061116     c                   eval      v11cdfis=vidcdf
064280071102     c* Imposto anche il campo della partita iva di nuovo
064290071102     c                   eval      v11pid=vidpid
064300071102     c
064310061116     c                   movel     'FI'          vidcvr
064320071026     c* Non devo aggiornare la bolla perchè l'ho già fatto qui per la
064330071026     c*  partita IVA
064340130412     c                   if        v11pid<>*blanks or
064350130412     c                              %subst(v11cdfis:12:1)<> ' '
064360071026     c                   eval      Aggiobol='N'
064370130402     c                   endif
064380061116     c                   exsr      REGIS11
064390061116     c                   endif
064400061116     c                   endif
064410141215     c                   endif
064420041007     c*
064430141212     c                   if        wnoindir='S'
064440141212     c                   eval      wdisag=wdisagdest
064450141212     c                   endif
064460041007     c* Riaggancio la bolla in modifica, per controlli sul
064470041007     c*  destinatario disagiato e consegna richiesta rispetto a FFD
064480151223     c                   if        wfile='A'
064490151223     C     KARB          CHAIN(n)  FNARB01L                           30
064500151223     c                   else
064510151223     C     KARB          CHAIN(n)  FNBLP01L                           30
064520151223     c                   endif
064530070906     c* Se in destinatario non è più A D o S tolgo se non immesso
064540070906     c*  dalla partenza
064550070911     c                   if        wdisagdest<>wdisag and wdisagdest<>' ' and
064560070910    2c                             (arbtc1=wdisagdest or arbtc2=wdisagdest)
064570070906     c                   exsr      ContrTCP
064580070906     c                   endif
064590040224
064600950602     C*
064610020531     C* SE destinatario disagiato devo rifare il giro per le consegne
064620020531     c* particolari
064630041015     C***  wdisag        IFne      *blanks
064640041015     c***  arbtc1        andne     wdisag
064650041015     c***  arbtc2        andne     wdisag
064660060327     c***                If        wdisag <> *blanks and
064670060327     c***                          %subst(arbGva:1:1) <> 'U' or
064680060327     c***                          wdisag='X' and %subst(arbGva:1:1)='U'
064690060327     c* Per Disagiato ---->sempre
064700060327     c* per Supermercato ->solo se non è consegna ad uffici
064710060327     c* per Appuntamento ->solo se non è consegna ad uffici e non è
064720070917     c*                    prevista data consegna "Tassativa" da partenza
064730070917     c                   movel     arbdcr        w008a
064740070907    1c                   if        (wdisag = 'X') OR
064750060327     c                             (wdisag='S' and %subst(arbGva:1:1)<>'U') OR
064760060327     c                             (Wdisag='A' and %subst(arbGva:1:1)<>'U' and
064770070917     c                             (arbdcr=*zeros or arbtcr <> *blanks or
064780070917     c                              (par5mdp='S' and w008a<>par5gdcr)))
064790070907    2c     arbtc1        ifne      wdisag
064800041015     c     arbtc2        andne     wdisag
064810030122     c                   Eval      wI0 = 'S'
064820020531     C                   MOVEL     'CP'          D48CVB
064830070907     C                   MOVEL     'D'           D48TRC
064840070911     C***                MOVEL     'E'           WESEG
064850070911     c***                clear                   W48f12
064860070907     c***                if        *in20
064870050502     c                   eval      §bgtcr=arbtcr
064880050502     c                   eval      §bghcr=arbhcr
064890050502     c                   eval      §bgdcr=arbdcr
064900050502     c                   eval      §bggc1=arbgc1
064910050502     c                   eval      §bggc2=arbgc2
064920050502     c                   eval      §bgtc1=arbtc1
064930050502     c                   eval      §bgtc2=arbtc2
064940070907     c***                endif
064950070910     c
064960070910     c* Se da eliminare anche la consegna particolare imposto
064970070910     c*  insieme la variazione
064980070910     c                   if        eliminatcp='1'
064990070910     c                   clear                   §bgtc1
065000070911     c* ripristino la vecchia consegna partic.
065010070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
065020070911     c                   eval      §bgtc1=aggtcp
065030070911     c                   endif
065040070910     c                   endif
065050070910     c                   if        eliminatcp='2'
065060070910     c                   clear                   §bgtc2
065070070911     c* ripristino la vecchia consegna partic.
065080070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
065090070911     c                   if        §bgtc1=' '
065100070911     c                   eval      §bgtc1=aggtcp
065110070911     c                   else
065120070911     c                   eval      §bgtc1=aggtcp
065130070911     c                   endif
065140070911     c                   endif
065150070910     c                   endif
065160070911     c
065170070907     c* Se modificato destinatario in disagiato o supermercato imposto il
065180070907     c* relativo flag nelle cosegne particolari
065190070907    3c     wdisag        ifne      *blanks
065200070907     c                   select
065210070907     c     §bgtc1        wheneq    *blanks
065220070907     c                   movel     wdisag        §bgtc1
065230070907     c     §bgtc2        wheneq    *blanks
065240070907     c                   movel     wdisag        §bgtc2
065250070907     c                   endsl
065260070907    4c     §bgtc1        ifeq      §bgtc2
065270070907     c                   clear                   §bgtc2
065280070907    4c                   endif
065290070907    3c                   endif
065300070907     c* Memorizzo in skiera
065310070907    3c                   if        kk<=10
065320070910     c* se devo eliminare una consegna particolare, memorizzo nella
065330070910     c* stessa variazione
065340070910     c                   if        eliminatcp=' '
065350070907     c                   add       1             kk
065360070910     c                   endif
065370070911     c* Passo dati in DS
065380070907     c                   movel     dsarbg        kdati(KK)
065390070907     c                   movel     d48cvb        kcvb(KK)
065400070907     c                   movel     d48trc        ktrc(KK)
065410070911     c* videata si vede o meno in base al richiamo prevedente
065420070911     c                   movel     d48ffr        kffr(KK)
065430070911     c* Disabilito F12
065440070911     c                   movel     ' '           kF12(KK)
065450070907    3c                   endif
065460070907     c
065470070907    2c                   endif
065480070910     c
065490070910   x1c                   else
065500070910     c* Se non devo aggiungere delle consegne particolari,
065510070910      *  se c'è la data consegna richiesta controllo se è ancora valida
065520070910      *  solo se ho fatto una modifica in arrivo
065530070914     c* Solo se non devo aggiungere delle consegnee particolari
065540070914     c*  altrimenti ilk controllo lo faccio nella regis7
065550070914     c                   if        aggtcp=*blanks
065560071109     c
065570071109     c* Imposto il campo WGIAC in base alla giacenza
065580071109if  2c                   If        *In04 and wgcfas = 35
065590071109if  3c                   If        wgcded > *Zeros and arbdcr >= wgcded
065600071109     c                   Eval      wgiac = 'S'
065610071109    3c                   EndIf
065620071109if  3c                   If        wgcded = *Zeros and arbDcr >= wgcddm
065630071109     c                   Eval      wgiac = 'S'
065640071109    3c                   EndIf
065650071109    3c                   EndIf
065660071109     c
065670160114    2c                   If        ArbDcr > *Zeros and *In10  and wfile='A'
065680070910     c                             and wCa = *Blanks and wgiac = *Blanks
065690070910     c                   ExSr      ContrDcr
065700070910    2c                   EndIf
065710070914    2c                   EndIf
065720070907    1c                   endif
065730070907     c
065740070907     c                   clear                   eliminatcp
065750070911     c                   clear                   aggtcp
065760130411
065770130411     c* Richiamo routine per invio variazione a pda
065780151230     c                   if        wfile='A'
065790130411     c                   exsr      sr_pda
065800160112     c                   endif
065810911209     C*
065820911212     C     ENDRG2        ENDSR
065830130930     c
065840130930     c* Aggiorno referente -----------------------------------------------------*
065850130930     c     AggioREF      BEGSR
065860130930     c
065870130930      * Referente e Telefono e magazzino giacenza
065880130930     c                   Eval      kAr5Trd = 'GEN'
065890130930     c     kAr5          Chain     Fiar501l
065900130930      * Aggiorno Fiar5
065910130930if  2c                   If        %Found(Fiar501l)
065920130930     c                   movel     ar5uni        DAr5Gen
065930130930     c                   Eval      §Ar5Ref = VidRef
065940130930     c                   Eval      §Ar5Teld = VidTeld
065950130930     c
065960130930     c* Solo per causale I0 aggiorno anche mag giacenza e f.deposito
065970130930     c                   if        flgcvr = 'I'
065980130930     c                   Eval      §Ar5Zng = VidZng
065990130930     c
066000130930     c                   if        wffd<>oldffd
066010130930     c                   if        wffd='S'
066020130930     c                   eval      §ar5dfd=%editc(dateu:'X')
066030130930     c                   else
066040130930     c                   clear                   §ar5dfd
066050130930     c                   endif
066060130930     c                   endif
066070130930     c
066080130930     c                   endif
066090130930     c
066100130930     c                   Movel(p)  DAr5Gen       Ar5Uni
066110130930      * se devo trasmettere alla sede sfleggo per la trasmissione
066120130930     c                   If        §Ar5Trse = 'S'
066130130930     c                   Clear                   Ar5Ft2
066140130930     c                   EndIf
066150130930      * sfleggo sempre per la trasmissione al cliente
066160130930     c                   Clear                   Ar5Ft3
066170130930     c                   Update    Fiar5000
066180130930      * Scrivo Fiar5
066190130930   x2c                   Else
066200130930      * se immessi i dati
066210130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
066220130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
066230130930      * di manutenzione bolle
066240130930if  3c                   If        VidRef <> *Blanks or VidTeld <> *Blanks or
066250130930     c                             VidZng <> *Blanks
066260130930     c                   Clear                   Fiar5000
066270130930     c                   Eval      Ar5Aas = ArbAas
066280130930     c                   Eval      Ar5Lnp = ArbLnp
066290130930     c                   Eval      Ar5Nrs = ArbNrs
066300130930     c                   Eval      Ar5Nsp = ArbNsp
066310130930     c                   Eval      Ar5Trd = 'GEN'
066320130930     c                   Move      Dateu         Ar5Dac
066330130930     c                   Movel     W0140         Ar5Orc
066340130930     c                   Clear                   Dar5Gen
066350130930     c                   Eval      §Ar5Ref = VidRef
066360130930     c                   Eval      §Ar5Teld = VidTeld
066370130930     c                   Eval      §Ar5Zng = VidZng
066380130930     c                   Movel(p)  DAr5Gen       Ar5Uni
066390130930     c                   Write     Fiar5000
066400130930    3c                   EndIf
066410130930    2c                   EndIf
066420130930     c                   ENDSR
066430911210     C*
066440911210     C*--- CONTROLLI FORMATO3 ----------------------------------------*
066450911210     C     CONTR3        BEGSR
066460941209     C*
066470920319     C* CONTROLLI COMUNI CON CONTR4
066480920319     C                   EXSR      CTRCOM
066490941209     C   90              GOTO      ENDC03
066500920319     C*
066510920319     C* T A S S A Z I O N E
066520941222     C* CONTROLLO L'IMPORTO DA ASSICURARE SE SONO IN ARRIVO SOLO PER
066530941222     C*  ASSEGNATO, IN PARTENZA SEMPRE
066540941222    1C     *IN36         IFEQ      *OFF
066550941222     C     *IN10         OREQ      *OFF
066560911211     C*
066570011205     C*** I M P O R T O  D A  A S S I C U R A R E  ***
066580080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
066590990127     C*
066600990127     C* IMPOSSIBILE INSERIRE IMPORTO DA ASSICURARE SE BOLLA SI C.A.
066610990127     C*  IN LIQ.TRANSATTIVA
066620080701    2C***  VIDIAS        IFGT      0
066630990127     C* VEDO SE C'E' C.A.
066640040809     C**""*KARB          CHAIN     FNDCT02L                           30
066650040809    3C**""**IN30         DOWEQ     *OFF
066660040809    4C**""*DCTATB        IFEQ      ' '
066670990127     C*
066680040809    5C**""*DCTFPR        IFEQ      'T'
066690040809     C**""*              MOVEL     MSG(82)       VIDMSG
066700040809     C**""*              SETON                                        30
066710040809     C**""*              SETON                                        409028
066720040809    5C**""*              ENDIF
066730040809    4C**""*              ENDIF
066740990127     C**
066750040809     C**""*  N30KARB          READE     FNDCT02L                               30
066760040809    3C**""*              ENDDO
066770040809      *
066780040809      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
066790040809      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annullate
066800040809      * legate alla spedizione che passo
066810080701      **** OBSOLETO DAL 1 GIUGNO 2008 CHE È STATO INSTALLATO LIQUIDAZIONE TANSATTIVA AUTOMATICA ***
066820080701      ***    NON FACCIAMO PIUI' IL CONTROLLO ****
066830080701     c*                  clear                   fidn12ds
066840080701     c*                  eval      i12tla = 'L'
066850080701     c*                  eval      i12aas = D48AAS
066860080701     c*                  eval      i12lnp = D48LNP
066870080701     c*                  eval      i12nrs = D48NRS
066880080701     c*                  eval      i12nsp = D48NSP
066890080701     c*                  eval      i12fel = 'E'
066900080701     c*                  eval      i12fan = 'E'
066910040809     c*
066920080701     c*                  call      'FIDN12R'
066930080701     c*                  parm                    fidn12ds
066940040809     c*
066950080701     c*                  setoff                                       30
066960040809     c*
066970040809     c* se non ci sono errori
066980080701    3C*                  if        o12err <> ' '
066990080701     c*                  seton                                        30
067000080701     c*                  else
067010040809     c* se numero di CA trovate maggiore di zero
067020080701    4C*                  if        o12nca = 0
067030080701     c*                  seton                                        30
067040080701     c*                  else
067050080701     c*                  z-add     1             jj                2 0
067060080701    5c*                  dow       jj <= o12nca and *in30 = *off
067070080701     c*                  movea     skkey(jj)     dskey
067080040809     c*
067090040809     c* Aggancio il FNDCT01L x reperire se la CA è in "Liquidazione Transattiva"
067100080701     c*    KDCT1_C       chain     FNDCT01L
067110080701    6c*                  if        %found(FNDCT01L)
067120080701    7C*    DCTFPR        IFEQ      'T'
067130080701     C*                  MOVEL     MSG(82)       VIDMSG
067140080701     C*                  SETON                                        30
067150080701     C*                  SETON                                        409028
067160080701    7C*                  ENDIF
067170080701    6c*                  endif
067180040809     c*
067190080701     c*                  add       1             jj
067200080701    5c*                  enddo
067210040809     c*
067220040809     c                   seton                                        30
067230040809     c*
067240080701    4c*                  endif
067250080701    3c*                  endif
067260990127     C**
067270080701     C** 90              GOTO      ENDC03
067280080701    2C***                ENDIF
067290941215     C*
067300941215     C* SE IL CAMPO = '   ' PRENDO LA DIVISA DALLA NAZIONE DEL MITTENT
067310941215     C* LO EVIDENZIO A VIDEO
067320950120     C* SE VARIATO L'IMPORTO ME LO SALVO
067330950120     C     *IN97         IFEQ      *ON
067340950120     C                   MOVEL     '1'           WIN97             1
067350950120     C                   ENDIF
067360950120     C*
067370941215     C*
067380941215     C                   MOVEL     VIDVAS        WVLT
067390941215     C                   Z-ADD     VIDIAS        W0133            13 3
067400941215     C* CONTROLLO LA DIVISA
067410941215     C                   EXSR      CTRVLT
067420941215     C                   MOVEL     WVLT          VIDVAS
067430941215     C* ERRORE
067440941215     C   90              SETON                                        41
067450941215     C   90              GOTO      ENDC03
067460011009      *
067470011009      * Controllo se la divisa immessa è scaduta o no
067480011009     C     ARBDSP        IFGT      §CVDLI
067490011105     C     VIDIAS        ANDGT     0
067500011102     C                   MOVEL     MSG(102)      VIDMSG
067510011009     C                   SETON                                        419028
067520011009     C                   GOTO      ENDC03
067530011009     C                   ENDIF
067540941215     C*
067550941215     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
067560941222    2C     VIDVAS        IFNE      VARVAS
067570950120     C     WIN97         ANDEQ     *BLANKS
067580941222    3C     VIDVAS        IFNE      *BLANKS
067590941219     C     VIDIAS        ORNE      0
067600941215     C                   MOVEL     MSG(28)       VIDMSG
067610941215     C                   SETON                                        409028
067620941215     C                   GOTO      ENDC03
067630941222    3C                   ENDIF
067640941219    2C                   ENDIF
067650941215     C*
067660941215     C                   MOVEL     VIDVAS        VARVAS
067670950120     C                   CLEAR                   WIN97
067680941215     C*
067690011205
067700011205      * pulisco il flag x la forzatura importo da assicurare forzabile
067710011205     c                   if        vidias <> sav1_vidias
067720011205     c                   clear                   forzaias
067730080625     c                   clear                   forzaias2
067740011205     c                   endif
067750011205      *---------------------
067760011205  2b c                   if        vidias <> 0
067770011205     c                   eval      sav1_vidias = vidias
067780011205      * Controllo l'importo da assicurare tramite il trul22r
067790011205     c                   clear                   trul22ds
067800011205     c                   eval      i22tla = 'L'
067810011205     c                   eval      i22cbo = arbcbo
067820011205     c                   eval      i22tsp = arbtsp
067830011205     c                   eval      i22lnp = arblnp
067840011205     c                   eval      i22nzm = arbnzm
067850011205     c                   eval      i22lna = arblna
067860011205     c                   eval      i22nzd = arbnzd
067870011205     c                   movel     vidklp        i22ksc
067880011205     c                   move      vidksc        i22ksc
067890011205     c                   eval      i22ctr = vidctr
067900011205     c                   eval      i22imp = vidias
067910011205     c                   eval      i22div = vidvas
067920011205     c                   eval      i22ute = knmus
067930011205     c                   eval      i22pgm = nompgm
067940011205     c                   call      'TRUL22R'
067950011205     c                   parm                    trul22ds
067960011205      * se ho tutti i flag impostati non posso immettere l'importo
067970011205      * da assicurare
067980011205  3b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
067990011205     c                             and o22fx2 <> *blanks
068000011205     c                   eval      vidmsg = msg(79)
068010011205     c                   eval      *in40 = *on
068020011205     c                   eval      *in90 = *on
068030011205     c                   eval      *in28 = *on
068040011206     c                   goto      endc03
068050011205  3e c                   endif
068060011205      * controllo che non sia minore del limite previsto
068070011205  3b c                   if        o22fmn <> *blanks
068080011205     c                   eval      vidmsg = msg(87)
068090011205     c                   eval      *in40 = *on
068100011205     c                   eval      *in90 = *on
068110011205     c                   eval      *in28 = *on
068120011206     c                   goto      endc03
068130011205  3e c                   endif
068140011205      * controllo che non sia maggiore del limite previsto
068150011205      * non forzabile
068160011205  3b c                   if        o22fx2 <> *blanks
068170011205     c                   eval      vidmsg = msg(39)
068180011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
068190011205     c                   eval      *in40 = *on
068200011205     c                   eval      *in90 = *on
068210011205     c                   eval      *in28 = *on
068220011206     c                   goto      endc03
068230011205  3e c                   endif
068240011205      * controllo che non sia maggiore del limite previsto
068250011205      * forzabile
068260011205  3b c                   if        o22fx1 <> *blanks and forzaias = *blanks
068270011205     c                   eval      forzaias = '1'
068280011205     c                   eval      vidmsg = msg(80)
068290011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
068300011205     c                   eval      *in40 = *on
068310011205     c                   eval      *in90 = *on
068320011205     c                   eval      *in28 = *on
068330011206     c                   goto      endc03
068340011205  3e c                   endif
068350011205  2e c                   endif
068360011205
068370941209     C*
068380941209     C* SE HA TARIFFE, CONTROLLO
068390941209    2C     WTKS          IFEQ      'S'
068400941209     C     KTAM4         CHAIN     TNTAM000                           30
068410941209    3C     *IN30         IFEQ      *OFF
068420991028     C                   MOVEL     TAMFLO        DSTA01
068430991028     C                   MOVEL     'CV'          COD
068440991028     C                   MOVEL(P)  §TADIV        KEY
068450991028     C                   EXSR      CTABEL
068460991028     C                   MOVEL     TBLUNI        DSCV
068470011010     C                   MOVEL     §CVDEC        COMDEC
068480941209     C*
068490941215     C* PULISCO CAMPI D FORZATURA
068500011010    4C     W0133         IFNE      VARIAS
068510941209     C                   CLEAR                   DIE
068520941215     C                   CLEAR                   CIN
068530941215    4C                   END
068540020715
068550020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
068560020715     c     §3atb1        Lookup    tbl                                    30
068570020715
068580020715   4Ac                   If        *in30 = *On
068590941209     C*
068600941209     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
068610941209     C*   ED E' ESPRESSO IL VALORE MERCE NELLA BOLLA
068620990818     C     KTPT          CHAIN     TITPT000                           30
068630950810    4C     *IN30         IFEQ      *ON
068640950810     C     TPTATB        ORNE      ' '
068650941222     C*
068660950810     C     *IN10         OREQ      *ON
068670941209     C     TPTAPL        ANDNE     'P'
068680941222     C*
068690941222     C     *IN10         OREQ      *OFF
068700941222     C     TPTAPL        ANDNE     'A'
068710941215     C*
068720011010     C                   Z-ADD     W0133         VARIAS
068730941215     C*
068740941215     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
068750941215     C*                                         IMPORTO DA TARIFFA
068760941215    5C     TAMFIS        IFEQ      '1'
068770941215     C     CIN           ANDNE     1
068780011010     C     W0133         ANDEQ     0
068790941215     C                   MOVEL     MSG(40)       VIDMSG
068800941215     C                   SETON                                        409028
068810941215     C                   ADD       1             CIN
068820941215     C                   GOTO      ENDC03
068830941215    5C                   ENDIF
068840941215     C*
068850941215     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
068860941215    5C     TAMFIS        IFEQ      '3'
068870011010     C     W0133         ANDEQ     0
068880941215     C                   MOVEL     MSG(42)       VIDMSG
068890941215     C                   SETON                                        409028
068900941215     C                   GOTO      ENDC03
068910941215    5C                   ENDIF
068920941215     C*
068930941215     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
068940941215    5C     TAMFIS        IFEQ      '2'
068950011010     C     W0133         ANDNE     0
068960941215     C                   MOVEL     MSG(43)       VIDMSG
068970941215     C                   SETON                                        409028
068980941215     C                   GOTO      ENDC03
068990941215    5C                   ENDIF
069000941215     C*
069010941215     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
069020950810    5C  N30TPTATB        IFEQ      ' '
069030011010     C     W0133         ANDGT     0
069040941215     C     DIE           ANDNE     1
069050941215     C     TPTVLM        ANDGT     0
069060941215     C*
069070941209     C* VALORE A COLLO
069080941215    6C     TPTFVM        IFEQ      '1'
069090991028     C     ARBNCL        MULT(H)   TPTVLM        TPTVAL           13 3
069100941215   X6C                   ELSE
069110941215    7C     TPTFVM        IFEQ      '2'
069120991028     C                   Z-ADD     TPTVLM        TPTVAL
069130941215   X7C                   ELSE
069140941212     C     ARBPKF        ADD       0,9           W0060             6 0
069150991028     C     W0060         MULT(H)   TPTVLM        TPTVAL
069160941215    7C                   END
069170941215    6C                   END
069180991028     C*
069190011205     C                   z-add     vidias        vidval           13 3
069200991028    7C     §TADIV        IFNE      VIDVAS
069210030612     C                   CLEAR                   YeurcoDS
069220991028     C                   MOVEL     VIDVAS        YECDVI
069230991028     C                   MOVEL     §TADIV        YECDVO
069240991028     C                   Z-ADD     VIDIAS        YECIMI
069250011010     C                   MOVE      COMDEC        YECDCO
069260991028     C*
069270991028     C                   CALL      'YEURCO'
069280030612     C                   PARM                    YeurcoDS
069290991028     C*
069300991028    8C     YECESI        IFEQ      ' '
069310991028     C                   Z-ADD     YECIMO        VIDVAL
069320991028    8C                   ENDIF
069330991028    7C                   ENDIF
069340941209     C*
069350991028    6C     VIDVAL        IFGT      TPTVAL
069360941209     C                   ADD       1             DIE
069370011205     c                   eval      vidmsg = msg(44)
069380941209     C                   SETON                                        409028
069390941209     C                   GOTO      ENDC03
069400941215    6C                   ENDIF
069410941209     C*
069420941215    5C                   ENDIF
069430941222    4C                   ENDIF
069440020715   4ac                   EndIf
069450941209    3C                   ENDIF
069460941209    2C                   ENDIF
069470030710
069480030710      * calcolo data limite mod. importo da assicurare
069490060201     c**   *iso          Move      ArbDsp        wdataiso
069500060201     c**                 adddur    §vpomia:*d    wdataiso
069510030722      *
069520060201     c**                 do        *hival
069530030722      * verifico se è un giorno lavorativo
069540060201     c**   *iso          move      wdataiso      ds_data
069550060201     C**                 eval      kann = ds_anno
069560060201     C**                 eval      kmes = ds_mese
069570060201     C**   kazcln        chain     azcln01l
069580060201     C**                 if        %found(azcln01l)
069590060201     C**                 if        MAT(ds_giorno) =  'F'  or
069600060201     C**                           POM(ds_giorno) =  'F'
069610030722      * se non va bene aggiungo 1 giorno
069620060201     c**                 adddur    1:*d          wdataiso
069630060201     C**                 iter
069640060201     c**                 else
069650060201     c**                 leave
069660060201     C**                 endif
069670060201     c**                 else
069680060201     c**                 leave
069690060201     C**                 endif
069700030722
069710060201     c**                 enddo
069720030710      * se viene modificato l'importo da assicurare rispetto a quanto
069730030710      * scritto sul file (stessa cosa anche x la divisa)
069740030710      * devo controllare se i gg. limite x la manutenzione sono trascorsi o meno
069750030710      * se sono già trascorsi emetto una window x richiedere la password
069760060201     c**                 If        (VidIas <> ArbIas or
069770060201     c**                           (VidVas <> ArbVas and ArbVas <> *Blanks)) and
069780060201     c**                            wdataoggi > wdataiso
069790060201     c**                           and wokpsw = *Blanks
069800060201     c**                 Exsr      Sr_Gespsw
069810060201     c** 90              GoTo      EndC03
069820060201     c**                 EndIf
069830941209     C***
069840011206     C*** V A L O R E   M E R C E   D I C H I A R A T O ***
069850941209     C* OBBLIGATORIO SE DESTINATARIO ESTERO E C'E' IN TABELLA NAZIONE
069860941209     C* IL FLAG EFTA
069870950120     C* SE HO VARIATO L'IMPORTO LO SALVO
069880950120     C     *IN93         IFEQ      *ON
069890950120     C                   MOVEL     '1'           WIN93             1
069900950120     C                   ENDIF
069910941209     C*
069920941209     C                   MOVEL     VIDVAD        WVLT
069930941209     C                   Z-ADD     VIDVMD        W0133            13 3
069940941209     C* CONTROLLO LA DIVISA
069950941209     C                   EXSR      CTRVLT
069960941209     C                   MOVEL     WVLT          VIDVAD
069970941209     C* ERRORE
069980941209     C   90              SETON                                        44
069990941209     C   90              GOTO      ENDC03
070000011010      *
070010011010      * Controllo se la divisa immessa è scaduta o no
070020011010     C     ARBDSP        IFGT      §CVDLI
070030011105     C     VIDVMD        ANDGT     0
070040011102     C                   MOVEL     MSG(102)      VIDMSG
070050011010     C                   SETON                                        449028
070060011010     C                   GOTO      ENDC03
070070011010     C                   ENDIF
070080941215     C*
070090941215     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE L'IMPORTO
070100941215     C* INDICATORE DI CHANGE 93 ON
070110941215    2C     VARVAD        IFNE      VIDVAD
070120950120     C     WIN93         ANDEQ     *BLANKS
070130941219     C     VIDVAD        IFNE      *BLANKS
070140941219     C     VIDVMD        ORNE      0
070150941215     C                   MOVEL     MSG(28)       VIDMSG
070160941215     C                   SETON                                        439028
070170941215     C                   GOTO      ENDC03
070180941215    2C                   ENDIF
070190941219     C                   ENDIF
070200941215     C*
070210941215     C                   MOVEL     VIDVAD        VARVAD
070220950120     C                   CLEAR                   WIN93
070230011205
070240011205      * pulisco il flag x la forzatura importo da assicurare forzabile
070250011205     c                   if        vidvmd <> sav1_vidvmd
070260011205     c                   clear                   forzavmd
070270011205     c                   endif
070280011205      *---------------------
070290011205     c                   eval      sav1_vidvmd = vidvmd
070300011205      * Controllo il valore merce tramite il trul23r
070310011205     c                   clear                   trul23ds
070320011205     c                   eval      i23tla = 'L'
070330011205     c                   eval      i23cbo = arbcbo
070340011205     c                   eval      i23tsp = arbtsp
070350011205     c                   eval      i23lnp = arblnp
070360011205     c                   eval      i23nzm = arbnzm
070370011205     c                   eval      i23lna = arblna
070380011205     c                   eval      i23nzd = arbnzd
070390011205     c                   movel     vidklp        i23ksc
070400011205     c                   move      vidksc        i23ksc
070410011205     c                   eval      i23ctr = vidctr
070420011206     c                   eval      i23imp = vidvmd
070430011206     c                   eval      i23div = vidvad
070440011205     c                   eval      i23ute = knmus
070450011205     c                   eval      i23pgm = nompgm
070460011205     c                   call      'TRUL23R'
070470011205     c                   parm                    trul23ds
070480011205      * se ho il flag del limite minimo impostato e l'importo è a zero
070490011205      * è obbligatorio
070500011206  2b c                   if        o23fmn <> *blanks and vidvmd = 0
070510011205     c                   eval      vidmsg = msg(45)
070520011205     c                   eval      *in43 = *on
070530011205     c                   eval      *in90 = *on
070540011205     c                   eval      *in28 = *on
070550011205     c                   goto      endc03
070560011206  2e c                   endif
070570011205      * controllo che non sia minore del limite previsto
070580011206  2b c                   if        o23fmn <> *blanks
070590011205     c                   eval      vidmsg = msg(104)
070600011210     c                   eval      %subst (vidmsg:37:14) = (%editc(o23lmn: '4'))
070610011205     c                   eval      *in43 = *on
070620011205     c                   eval      *in90 = *on
070630011205     c                   eval      *in28 = *on
070640011205     c                   goto      endc03
070650011206  2e c                   endif
070660011205      * controllo che non sia maggiore del limite previsto
070670011205      * non forzabile
070680011206  2b c                   if        o23fx2 <> *blanks
070690011205     c                   eval      vidmsg = msg(105)
070700011210     c                   eval      %subst (vidmsg:53:14) = (%editc(o23lx2: '4'))
070710011205     c                   eval      *in43 = *on
070720011205     c                   eval      *in90 = *on
070730011205     c                   eval      *in28 = *on
070740011205     c                   goto      endc03
070750011206  2e c                   endif
070760011205      * controllo che non sia maggiore del limite previsto
070770011205      * forzabile
070780011206  2b c                   if        o23fx1 <> *blanks and forzavmd = *blanks
070790011205     c                   eval      forzavmd = '1'
070800011205     c                   eval      vidmsg = msg(106)
070810011210     c                   eval      %subst (vidmsg:30:14) = (%editc(o23lx1: '4'))
070820011205     c                   eval      *in43 = *on
070830011205     c                   eval      *in90 = *on
070840011205     c                   eval      *in28 = *on
070850011205     c                   goto      endc03
070860011206  2e c                   endif
070870930507     C*
070880930507    1C                   END
070890950424     C*
070900950426     C*** CONTROLLI PER CLIENTE CON TARIFFA A VALORE CON FLAG = '3' ***
070910950426    1C     WTKS          IFEQ      'S'
070920950424     C                   MOVEL     VIDCTR        W001A             1
070930950426    2C     W001A         IFEQ      '4'
070940950424     C     TAMFVD        ANDEQ     '3'
070950950426     C*
070960950426    3C     VIDKLP        IFNE      VARKLP
070970950426     C     VIDKSC        ORNE      VARKSC
070980950426     C     VIDIAS        ORNE      COMIAS
070990950426     C                   SELECT
071000950426     C* FATTURA IMMEDIATA
071010950426     C* Se modificato il cod.cliente o l'importo da assicurare
071020950426     C* è necessario che la q.tà da fatturare e importi tassazione
071030950426     C* vengano azzerati + F14
071040950426     C     *IN14         WHENEQ    *ON
071050991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
071060991001     C                   MOVEL     '1'           WCKINL            1
071070991001     C                   EXSR      CARWAR
071080950426    4C     VIDQFT        IFNE      *ZEROS
071090950426     C     VIDIMV        ORNE      *ZEROS
071100950426     C     VIDPOR        ORNE      *ZEROS
071110991001     C     WINL          OREQ      '1'
071120950426     C     *INKN         OREQ      *OFF
071130950426     C                   SETON                                        429028
071140950426     C                   MOVEL     MSG(71)       VIDMSG
071150950426     C                   GOTO      ENDC03
071160950426    4C                   END
071170950426     C* SEGUE FATTURA
071180950426     C* Se modificato il cod.cliente o l'importo da assicurare
071190950426     C* è necessario che la q.tà da fatturare e tot imponibile
071200950426     C* vengano azzerati
071210950426     C     *IN13         WHENEQ    *ON
071220950426    4C     VIDQFT        IFNE      *ZEROS
071230950426     C     VIDIMV        ORNE      *ZEROS
071240950426     C                   SETON                                        429028
071250950426     C                   MOVEL     MSG(70)       VIDMSG
071260950426     C                   GOTO      ENDC03
071270950426    4C                   END
071280950426     C                   ENDSL
071290950426     C                   MOVE      VIDKLP        VARKLP
071300950426     C                   MOVE      VIDKSC        VARKSC
071310950426     C                   MOVE      VIDIAS        COMIAS
071320950426     C                   MOVE      *ZEROS        VARQFT
071330950426    3C                   END
071340950426     C*
071350950426     C*** QUANTITA' DA FATTURARE ***
071360950426     C*
071370950426     C* Errore se modificata e diversa da 0
071380950426    3C     VIDQFT        IFNE      VARQFT
071390950426    4C     VIDQFT        IFNE      *ZEROS
071400950426     C                   MOVEL     MSG(68)       VIDMSG
071410950424     C                   SETON                                        429028
071420950424     C                   GOTO      ENDC03
071430950426    4C                   END
071440950426     C* Se azzerata q.tà da fatturare gli
071450950426     C* importi della tassazione devono essere = 0
071460950426     C                   SELECT
071470950426    4C     *IN14         WHENEQ    *ON
071480991001     C* Carico varie del video in schiera per vedere se c'è l'inoltro
071490991001     C                   MOVEL     '1'           WCKINL            1
071500991001     C                   EXSR      CARWAR
071510950426    5C     VIDIMV        IFNE      *ZEROS
071520950426     C     VIDPOR        ORNE      *ZEROS
071530991001     C     WINL          OREQ      '1'
071540950426     C     *INKN         OREQ      *OFF
071550950426     C                   SETON                                        499028
071560950426     C                   MOVEL     MSG(69)       VIDMSG
071570950426     C                   GOTO      ENDC03
071580950426    5C                   END
071590950426    4C     *IN13         WHENEQ    *ON
071600950426    5C     VIDIMV        IFNE      *ZEROS
071610950426     C                   SETON                                        499028
071620950426     C                   MOVEL     MSG(72)       VIDMSG
071630950426     C                   GOTO      ENDC03
071640950426    5C                   END
071650950426    4C                   ENDSL
071660950424     C                   MOVE      VIDQFT        VARQFT
071670950426    3C                   END
071680950424     C*
071690950426    2C                   END
071700950426    1C                   END
071710950424     C*
071720050601     c* Controlli su aggiunte/aumenti di importo da assicurare
071730080625    1c                   If        (VidIas <> ArbIas and vidias > 0 or
071740050804     c                             (VidVas <> ArbVas and vidVas <> *Blanks
071750050804     c                             and Vidias>0))
071760050601     c* verifico se cliente con mandato
071770050601     c                   exsr      tassar
071780050601     c* cliente senza mandato: si può aggiungere/aumentare l'importo solo
071790050601     c* se inviata e-mail
071800050601     c* idem per cliente con mandato se il valore è oltre mandato
071810080625    2c                   if        task88 = 'S'
071820050601     c* vedo se inviata e-mail
071830050609     c                   movel     'c'           kta4trc
071840050601     c                   clear                   dta4c
071850050601     c     kta4          chain     tita430c
071860080625    3c                   if        %found(tita430c)
071870050601     c                   movel     ta4not        dta4c
071880080625    3c                   endif
071890080625    3c                   if        §ta4mail <> 'S'
071900080625    4c                   if        §utemtf=' '
071910050601     c                   seton                                        409028
071920050601     c                   eval      vidmsg=msg(135)
071930050601     c                   goto      endc03
071940080625    4c                   endif
071950080625    4c                   if        (§utemtf='I' or §utemtf='E') and
071960080625     c                             forzaias2=' '
071970080625     c                   eval      forzaias2='1'
071980080625     c                   seton                                        409028
071990080625     c                   eval      vidmsg=msg(146)
072000080625     c                   goto      endc03
072010080625    4c                   endif
072020080625    3c                   endif
072030080625    2c                   endif
072040080625    1c                   endif
072050941221     C*
072060941221     C* F14 - TASSAZIONE
072070991011     C     *INKN         IFEQ      *ON
072080991029     C* Fattura immediata SE NON CONTABILIZZATA
072090991011     C     *IN14         OREQ      *ON
072100991029     C     *IN29         ANDEQ     *OFF
072110991029     c* Prepagati NON CONTABILIZZATA
072120991011     C     *IN10         OREQ      *OFF
072130991011     C     VIDNFT        ANDGT     *ZEROS
072140991029     C     *IN29         ANDEQ     *OFF
072150980513     C                   MOVEL     '1'           WBOL              1
072160011018      * Se è una bolla di reso con varia 'N' 88888888
072170011017      * tasso la varia 'N'
072180011018     C     FLGVRN        IFEQ      '1'
072190011017      * leggo il sbfl delle varie
072200011017     C                   DO        20            NRR
072210011017     C     NRR           CHAIN     LR48S03                            32
072220011017     C     V3CSV1        IFEQ      'N'
072230011017     C     V3CVA1        ANDEQ     88888888
072240011017     C                   EXSR      TASSAN
072250011017     C  N32              UPDATE    LR48S03
072260011017     C                   ENDIF
072270011017     C                   ENDDO
072280011017     C                   ENDIF
072290011017      *
072300151020     c                   if        vidimv<>PesVolImv
072310151016     c                   clear                   PesVolPkcn
072320151016     c                   clear                   PesVolSpc
072330151016     c                   clear                   PesVolImv
072340151020     c                   endif
072350151020     c***                clear                   Tassatoimv
072360941221     C                   EXSR      TASSAZ
072370151019     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
072380151020     c***                if        vidimv>0 and vidimv<>PesVolImv
072390151020     c***                eval      PesVolPkcn=taspkcn
072400151020     c***                eval      PesVolSpc=tasspc
072410151020     c***                eval      PesVolIMV=vidimv
072420151020     c***                endif
072430990702     C* SE SI TRATTA DI UN PREPAGATO DEVO ANCHE VISUALIZZARE IL TOT
072440990702     C*  IMPONIBILE ARROTONDATO
072450990702     C     *IN23         IFEQ      *ON
072460151020     c****               eval      TassatoImv=vidimv
072470990702     C                   Z-ADD     AR6DFT        ARBDFT
072480990702     C                   EXSR      TOTFAT
072490990702     C                   Z-ADD     D04IFT        VIDIFT
072500990702     C                   Z-ADD     D04IMV        VIDIMV
072510990702     C                   Z-ADD     D04POR        VIDPOR
072520991006     C                   Z-ADD     D04POR        VARPOR
072530990702     C                   Z-ADD     D04IMV        SAVIMV
072540990702     C                   ENDIF
072550151020     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
072560151020     c                   if        vidimv>0 and vidimv<>PesVolImv
072570151020     c                   eval      PesVolPkcn=taspkcn
072580151020     c                   eval      PesVolSpc=tasspc
072590151020     c                   eval      PesVolIMV=vidimv
072600151020     c                   endif
072610131016
072620131016     c                   if        *inkn
072630131016     c                   eval      winf14='1'
072640131016     c                   endif
072650991011     C   KN
072660991011     COR 90              GOTO      ENDC03
072670990702    1C                   END
072680991006     C* Se divisa sprotetta significa che manca tariffa: in questo caso
072690991006     C* controllo che la divisa inserita sia = alla divisa della moneta
072700991006     C*  di conto SE CLIENTE 8888 o 9999
072710991006     C*
072720991006     C* Fattura immediata
072730991006     C     *IN14         IFEQ      *ON
072740991006     C* Prepagati
072750991006     C     *IN10         OREQ      *OFF
072760991006     C     VIDNFT        ANDGT     *ZEROS
072770991006     C* Divisa sprotetta
072780991006     C     *IN07         IFEQ      *OFF
072790991006     C* Divisa diversa dalla divisa della moneta di conto
072800991006     C     VIDDIV        ANDNE     §GEDCN
072810991006     C* Cliente 8888 o 9999
072820991006     C     VIDKSC        IFEQ      8888
072830991006     C     VIDKSC        OREQ      9999
072840991006     C                   SETON                                        459028
072850991006     C                   MOVEA     MSG(90)       K78
072860991006     C                   MOVEA     §GEDCN        K78(71)
072870991006     C                   MOVEA     K78           VIDMSG
072880991006     C                   GOTO      ENDC03
072890991006     C                   ENDIF
072900991006     C                   ENDIF
072910991006     C                   ENDIF
072920941102     C**
072930991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
072940991124     C* TASSAZIONE
072950991124    1C     *IN07         IFEQ      *OFF
072960991124     C     VIDDIV        ANDNE     WDIV
072970991124     C     VIDDIV        ANDEQ     *BLANKS
072980991124    2C     VIDPOR        IFGT      *ZEROS
072990991125     C                   SETON                                        459028
073000991124     C                   MOVEL     MSG(94)       VIDMSG
073010991124     C                   GOTO      ENDC03
073020991124   X2C                   ELSE
073030991124    3C                   DO        20            NRR
073040991124     C     NRR           CHAIN     LR48S03                            30
073050991124    4C     V3CVA1        IFGT      *ZEROS
073060991125     C                   SETON                                        459028
073070991124     C                   MOVEL     MSG(94)       VIDMSG
073080991124     C                   GOTO      ENDC03
073090991124    4C                   ENDIF
073100991124    3C                   ENDDO
073110991124    2C                   ENDIF
073120991124    1C                   ENDIF
073130991005     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
073140991012     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
073150991012     C* CONTROLLO DELLA DIVISA
073160991013    0C     *IN07         IFEQ      *OFF
073170991013     C     VIDDIV        ANDNE     WDIV
073180991026     C     WDIV          ANDNE     *BLANKS
073190100120     c
073200030612     C                   CLEAR                   Fnlv16DS
073210991012     C                   CLEAR                   DTASV
073220100120     C                   CLEAR                   dlv1601
073230100120
073240991012     C                   MOVEL     'D'           D17FIM
073250991012     C                   MOVE      VIDDIV        D17DIV
073260991012     C     VIDDFT        IFGT      *ZEROS
073270991012     C                   CLEAR                   WLBDAT
073280991012     C                   Z-ADD     VIDDFT        G02DAT
073290991012     C                   MOVEL     *BLANK        G02ERR
073300991012     C                   CALL      'XSRDA8'
073310991012     C                   PARM                    WLBDAT
073320991012     C                   Z-ADD     G02INV        D17DSP
073330991012     C                   ELSE
073340991012     C                   Z-ADD     ARBDSP        D17DSP
073350991012     C                   END
073360991012     C                   CALL      'FNLV16R'
073370030612     C                   PARM                    Fnlv16DS
073380991012     C                   PARM                    DTASV
073390991014     C                   MOVEL     D17DIV        VIDDIV
073400991012   X0C                   ELSE
073410030612     C                   CLEAR                   Fnlv16DS
073420991005     C                   CLEAR                   DTASV
073430941222     C                   MOVEL     D48TBO        D17TBO
073440991006     C   23              MOVEL     'S'           D17PRP
073450941209     C                   MOVEL     KSC           D17KSC
073460991012     C     VIDDFT        IFGT      *ZEROS
073470991012     C                   CLEAR                   WLBDAT
073480991012     C                   Z-ADD     VIDDFT        G02DAT
073490991012     C                   MOVEL     *BLANK        G02ERR
073500991012     C                   CALL      'XSRDA8'
073510991012     C                   PARM                    WLBDAT
073520991012     C                   Z-ADD     G02INV        D17DSP
073530991012     C                   ELSE
073540991012     C                   Z-ADD     ARBDSP        D17DSP
073550991012     C                   END
073560941209     C                   MOVEL     ARBLNP        D17LNP
073570941209     C                   MOVEL     ARBLNA        D17LNA
073580941209     C                   MOVEL     §3ATB1        D17TBL
073590941209     C                   MOVEL     §3ARBL        D17RBL
073600020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
073610100120     C                   MOVEL     §3ASVA        §D17sva
073620100120
073630100120     c* Se fattura immediata o prepagato
073640100120     c*                        controllo evetuale esenzione iva
073650100121     c                   if        *in14   or (not *in10 and vidnft>0)
073660100120     c                   Eval      §d17ctriso='S'
073670100120     c                   if        arbcpi<>*blanks
073680100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
073690100120     c* solo numeri
073700100120     c                   select
073710100120     c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
073720100120     c                   eval      §d17iso='IT'
073730100120     c* "PRIVATO" senza iso
073740100120     c                   when      *in30 and posiz<=3
073750100120     c                   eval      §d17iso='$$'
073760100120     c                   other
073770100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
073780100120     c                   ENDSL
073790100120     c                   endif
073800100120     c                   endif
073810100120     c
073820100120     c                   Movel     Dlv1601       D17Flo
073830100120     c
073840941209     C                   Z-ADD     VIDIAS        D17IAS
073850941209     C                   Z-ADD     VIDQFT        D17QFT
073860941209     C                   Z-ADD     VIDPOR        D17POR
073870991005     C                   MOVEL     VIDDIV        D17DIV
073880991006     C                   MOVEL     WDIV          D17DVP
073890941209     C                   Z-ADD     VIDIMV        D17IMV
073900990702    1C     §3AFCA        IFEQ      1
073910941209     C                   MOVEL     'S'           D17FCA
073920990702    1C                   ENDIF
073930990702    1C* SE C'E' NUMERO E DATA FATTURA RIPASSO IL TOTALE IMPONIBILE
073940950114     C     *IN14         IFEQ      *ON
073950991005     C                   MOVEL     'E'           D17FIM
073960950114     C*
073970990702   X1C                   ELSE
073980950114     C*
073990950114     C                   MOVEL     'C'           D17FIM
074000990702    2C     VIDFIV        IFGT      0
074010950114     C     VIDNFT        ANDGT     0
074020950114     C     VIDDFT        ANDGT     0
074030991005     C                   MOVEL     'E'           D17FIM
074040990702    2C                   ENDIF
074050990702    1C                   ENDIF
074060941209     C                   MOVEL     VIDCEI        D17CEI
074070991029     C* SE PROTETTA LA TASSAZIONE, NON PASSO I DATI DELLA FATTURA
074080941209     C                   MOVEL     VIDFIV        D17FIV
074090941220     C                   MOVEL     'S'           D17FFI
074100941209     C                   MOVEL     VIDNFT        D17NFT
074110941209     C                   Z-ADD     VIDDFT        D17DFG
074120941209     C*
074130991012     C* RICHIAMO ROUTINE PER CARICAMENTO VARIE DELLA DTASV
074140991012     C                   EXSR      RDTASV
074150941209     C*
074160991005     C                   CALL      'FNLV16R'
074170030612     C                   PARM                    Fnlv16DS
074180991005     C                   PARM                    DTASV
074190941102     C*
074200991011     C                   MOVEL     D17DIV        VIDDIV
074210941209     C                   MOVEL     D17CEI        VIDCEI
074220991005     C                   MOVEL     O17DEI        DESCEI
074230991005     C                   MOVEL     O17DEI        V3DCEI
074240991006     C     D17FIM        IFEQ      'E'
074250991014     C     O17ERR        ANDEQ     *BLANKS
074260030620      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
074270030620      * SOLO SE LA VARIA ESISTE
074280040123     C**!!!§3ASVA        ORNE      *BLANKS
074290040123     C**!!!WVAR3A        ANDEQ     'S'
074300040123     C**!!!O17ERR        ANDEQ     *BLANKS
074310040123     C**!!!D17FIM        ANDEQ     'C'
074320991005     C                   MOVEL     O17IMV        VIDIMV
074330991006     C                   ENDIF
074340030620      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
074350030620     C     §3ASVA        IFNE      *BLANKS
074360030620     C     WVAR3A        ANDNE     'S'
074370030620     C     D17FIM        ANDEQ     'C'
074380030620     C                   Z-ADD     *ZEROS        VIDIMV
074390030620     C                   ENDIF
074400030620      ****
074410991029     C                   MOVEL     D17DFG        VIDDFT
074420941209     C* VARIE
074430990702    1C                   DO        20            NRR
074440941215     C     NRR           CHAIN     LR48S03                            30
074450941209     C                   MOVEL     VSV(NRR)      V3CSV1
074460941209     C                   MOVEL     VES(NRR)      V3CEV1
074470941215     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
074480991006    2C     O17ERV        IFEQ      NRR
074490941220     C                   SETON                                        5290
074500941215     C                   ELSE
074510941215     C                   SETOFF                                       52
074520990702    2C                   ENDIF
074530941215     C  N30              UPDATE    LR48S03
074540941215     C   30              WRITE     LR48S03
074550990702    1C                   ENDDO
074560991012    0C                   ENDIF
074570941215     C*
074580941215     C                   SETOFF                                       52
074590941102     C*  ERRORE
074600991006    1C     O17ERR        IFNE      *BLANKS
074610991006     C     O17FAS        OREQ      'S'
074620941102     C*
074630991006    2C     O17MSG        IFNE      *BLANKS
074640991006     C                   MOVEL     O17MSG        VIDMSG
074650941209     C                   SETON                                        28
074660941102    2C                   ENDIF
074670990415     C**
074680990415     C* VARIA R  MSG
074690991006    2C     O17ERR        IFEQ      '6'
074700991006     C     O17MSG        ANDEQ     MSG(85)
074710991006     C     O17FAS        OREQ      'S'
074720990416     C*
074730990415     C     COMASS        IFEQ      ' '
074740990415     C* SE MODIFICATO L'IMPORTO D'ASSICURARE E FATTURA GIA' INCASSATA O TASSATA
074750990415    3C     VIDIAS        IFNE      ARBIAS
074760990415     C     VIDVAS        ORNE      ARBVAS
074770990415     C*
074780990415     C* SE GIA' TASSATO FINO ALL'IMPONIBILE E LA DATA FATTURA > 0
074790990415    4C     AR6IMV        IFNE      *ZEROS
074800990415     C                   SETON                                        2882
074810990415     C                   SETOFF                                       90
074820990415     C* ERRORE NON BLOCCANTE:CORREGGERE  VARIA R
074830990419     C                   SELECT
074840990419    5C     ARBIAS        WHENEQ    0
074850990415     C     VIDIAS        ANDGT     0
074860990415     C                   MOVEL     MSG(83)       VIDMSG
074870990419    5C     ARBIAS        WHENGT    0
074880990419     C     VIDIAS        ANDEQ     0
074890990415     C                   MOVEL     MSG(84)       VIDMSG
074900990419     C                   OTHER
074910990419     C                   MOVEL     MSG(86)       VIDMSG
074920990419    5C                   ENDSL
074930990415    4C                   END
074940990415     C                   MOVEL     '1'           COMASS            1
074950990415    3C                   END
074960990415     C                   ELSE
074970990415     C                   SETOFF                                       9028
074980990415     C                   MOVEL     *BLANKS       VIDMSG
074990990415     C                   END
075000990415    2C                   END
075010990415     C* FATTURA
075020941102     C**
075030941102     C* ESENZIONE IVA
075040991006    2C     O17ERR        IFEQ      '1'
075050941209     C                   SETON                                        5190
075060941102    2C                   ENDIF
075070941102     C**
075080941102     C* IMPONIBILE
075090991006     C     O17ERR        IFEQ      '5'
075100941209     C                   SETON                                        5090
075110941102    2C                   ENDIF
075120941222     C* NUMERO FATTURA
075130991006     C     O17ERR        IFEQ      '2'
075140941222     C                   SETON                                        5590
075150941222    2C                   ENDIF
075160941222     C* FILIALE IVA FATTURA
075170991006     C     O17ERR        IFEQ      '3'
075180941222     C                   SETON                                        5490
075190941222    2C                   ENDIF
075200941222     C* DATA FATTURA
075210991006     C     O17ERR        IFEQ      '4'
075220941222     C                   SETON                                        5690
075230941222    2C                   ENDIF
075240991006     C* DIVISA
075250991006     C     O17ERR        IFEQ      '7'
075260991006     C                   SETON                                        4590
075270991006    2C                   ENDIF
075280991012     C* PORTO
075290991012     C     O17ERR        IFEQ      '8'
075300001116     C                   SETON                                        4990
075310991012    2C                   ENDIF
075320941102     C**
075330941220     C                   GOTO      ENDC03
075340941102    1C                   ENDIF
075350040123      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
075360040123      * non posso avere altre varia con importo > 0
075370040123if  1c                   If        §3asva <> *Blanks
075380040123do  2c                   Do        20            nrr
075390040123     c     nrr           Chain     Lr48s03                            30
075400040123     c                   Eval      *In52 = *Off
075410040123if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
075420040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
075430040123     c                   Eval      *In28 = *On
075440040123     c                   Eval      *In52 = *On
075450040123     c                   Eval      *In90 = *On
075460040123     c                   Eval      VidMsg = Msg(127)
075470040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
075480040123    3c                   EndIf
075490040123if  3c                   If        wvar3a = *Blanks  and
075500040123     c                             V3cSv1 <> *Blanks and V3cVa1 > *Zeros
075510040123     c                   Eval      *In28 = *On
075520040123     c                   Eval      *In52 = *On
075530040123     c                   Eval      *In90 = *On
075540040123     c                   Eval      VidMsg = Msg(127)
075550040123     c                   Eval      %subst(VidMsg:75:1) = §3asva
075560040123    3c                   EndIf
075570040123     c  n30              Update    Lr48s03
075580040127     c   90              Goto      Endc03
075590040123    2c                   EndDo
075600040123    1c                   EndIf
075610991006     C*
075620991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
075630991006    1C  N07VIDDIV        IFNE      WDIV
075640991006     C* CONVERSIONE PORTO
075650991006    2C     VIDPOR        IFEQ      VARPOR
075660991006     C     VIDPOR        ANDGT     *ZEROS
075670991006     C     WDIV          ANDNE     *BLANKS
075680030612     C                   CLEAR                   YeurcoDS
075690991006     C                   MOVEL     WDIV          YECDVI
075700991006     C                   Z-ADD     VIDPOR        YECIMI
075710991006     C                   MOVEL     VIDDIV        YECDVO
075720991006     C     O17FDC        IFEQ      'S'
075730011114     C                   MOVE      '3'           YECDCO
075740991006     C                   ENDIF
075750991006     C                   CALL      'YEURCO'
075760030612     C                   PARM                    YeurcoDS
075770991006     C                   Z-ADD     YECIMO        VIDPOR
075780991012     C                   SETON                                        49
075790991006    2C                   END
075800991006     C* CONVERSIONE VARIE
075810991006    2C                   DO        20            NRR
075820991006     C     NRR           CHAIN     LR48S03                            30
075830991006    3C     V3CVA1        IFEQ      VARVA1
075840991006     C     V3CVA1        ANDGT     *ZEROS
075850991006     C     WDIV          ANDNE     *BLANKS
075860011019      *
075870011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
075880011019      * non devo convertire la varia 'N' 88888888
075890011019     C     VIDIMV        IFEQ      *ZEROS
075900011019     C     FLGVRN        ANDEQ     '1'
075910011019     C     V3CSV1        ANDEQ     'N'
075920011019     C     V3CVA1        ANDEQ     88888888
075930011019     C  N30              UPDATE    LR48S03
075940011019     C                   ITER
075950011019     C                   ENDIF
075960030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
075970030624      * non devo convertire la varia '&' 88888888
075980030624     C     VIDIMV        IFEQ      *ZEROS
075990030624     C     V3CSV1        ANDEQ     '&'
076000030624     C     V3CVA1        ANDEQ     88888888
076010030624     C  N30              UPDATE    LR48S03
076020030624     C                   ITER
076030030624     C                   ENDIF
076040011019      *
076050030612     C                   CLEAR                   YeurcoDS
076060991006     C                   MOVEL     WDIV          YECDVI
076070991006     C                   Z-ADD     V3CVA1        YECIMI
076080991006     C                   MOVEL     VIDDIV        YECDVO
076090991006     C     O17FDC        IFEQ      'S'
076100011114     C                   MOVE      '3'           YECDCO
076110991006     C                   ENDIF
076120991006     C                   CALL      'YEURCO'
076130030612     C                   PARM                    YeurcoDS
076140991006     C                   Z-ADD     YECIMO        V3CVA1
076150991012     C                   SETON                                        53
076160991006    3C                   ENDIF
076170991006     C                   Z-ADD     V3CVA1        VARVA1
076180991006     C  N30              UPDATE    LR48S03
076190991006    2C                   ENDDO
076200991006     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
076210991006     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
076220991006     C     VIDIMV        IFGT      *ZEROS
076230991006     C     WDIV          ANDNE     *BLANKS
076240991006     C                   MOVEL     'T'           D17FIM
076250991006     C                   MOVEL     *ZEROS        D17IMV
076260991006     C                   Z-ADD     VIDPOR        D17POR
076270991012     C                   CLEAR                   DTASV
076280991012     C                   EXSR      RDTASV
076290991006     C                   CALL      'FNLV16R'
076300030612     C                   PARM                    Fnlv16DS
076310991006     C                   PARM                    DTASV
076320991006     C                   Z-ADD     O17IMV        VIDIMV
076330991006     C                   ENDIF
076340991006     C                   Z-ADD     VIDPOR        VARPOR
076350991006     C                   MOVEL     VIDDIV        WDIV
076360991012     C* SE VARIATA DIVISA RIEMETTO IN OGNI CASO LA VIDEATA
076370991029     C* PERCHE' L'LV16 DOVRA' CONTROLLARE LA TASSAZIONE
076380991012     C                   SETON                                        90
076390991012     C                   GOTO      ENDC03
076400991006   X1C                   ELSE
076410991006     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
076420991006     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
076430991006     C                   Z-ADD     VIDPOR        VARPOR
076440991006    2C                   DO        20            NRR
076450991006     C     NRR           CHAIN     LR48S03                            30
076460991006     C                   Z-ADD     V3CVA1        VARVA1
076470991006     C  N30              UPDATE    LR48S03
076480991006    2C                   ENDDO
076490991006    1C                   ENDIF
076500941102     C*
076510941102     C* PER COD 8888 O 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
076520941209     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
076530991006    3C     O17FAS        IFEQ      'S'
076540941102     C*
076550941102     C* SE = 0 E FORZO CON ENTER PRENDO VALORE DA TARIFFA
076560990702    2C     SEI           IFNE      1
076570941209     C                   MOVEL     MSG(46)       VIDMSG
076580941209     C                   SETON                                        5290
076590941102     C                   ADD       1             SEI
076600941102     C                   GOTO      ENDC03
076610990702    2C                   ENDIF
076620990702    1C                   ENDIF
076630941222     C*
076640941222     C* IN PARTENZA SE C'ERA PREPAGATO, DEVO LASCIARLO
076650990702    1C     *IN10         IFEQ      *OFF
076660990702    2C     AR6NFT        IFNE      0
076670941222     C     VIDNFT        ANDEQ     0
076680941222     C                   SETON                                        559028
076690941222     C                   MOVEL     MSG(60)       VIDMSG
076700941222     C                   GOTO      ENDC03
076710941222    2C                   ENDIF
076720990702     C*
076730990702     C* SE PREPAGATO INSERIRE TASSAZIONE O F14 PER TASSARE
076740990702     C  NKL
076750990702    2CANNKN*IN23         IFEQ      *ON
076760990702    3C     VIDIMV        IFEQ      0
076770990702     C                   SETON                                        529028
076780990702     C                   MOVEL     MSG(89)       VIDMSG
076790990702     C                   GOTO      ENDC03
076800990702   X3C                   ELSE
076810990702    4C     VIDIMV        IFNE      SAVIMV
076820990702     C                   Z-ADD     AR6DFT        ARBDFT
076830990702     C                   EXSR      TOTFAT
076840990702     C                   Z-ADD     D04IFT        VIDIFT
076850990702     C                   Z-ADD     D04IMV        VIDIMV
076860990702     C                   Z-ADD     D04POR        VIDPOR
076870991006     C                   Z-ADD     D04POR        VARPOR
076880990702     C                   Z-ADD     VIDIMV        SAVIMV
076890990702     C                   SETON                                        90
076900990702    4C                   ENDIF
076910990702    3C                   ENDIF
076920990702    2C                   ENDIF
076930990702    1C                   ENDIF
076940011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
076950011017     C                   SETOFF                                       52
076960011018    1C     FLGVRN        IFEQ      '1'
076970011017    2C                   DO        20            NRR
076980011017     C     NRR           CHAIN     LR48S03                            32
076990011017    3C     *IN32         IFEQ      *OFF
077000011018     C                   SETON                                        52
077010011018    4C     V3CSV1        IFEQ      'N'
077020011017     C                   SETOFF                                       52
077030011017     C                   LEAVE
077040011018    4C                   ENDIF
077050011017    3C                   ENDIF
077060011017    2C                   ENDDO
077070011018    2C     *IN52         IFEQ      '1'
077080011017     C     1             CHAIN     LR48S03                            32
077090011017     C  N32              UPDATE    LR48S03
077100011017     C                   MOVEL     MSG(101)      VIDMSG
077110011105     C                   SETON                                        9028
077120011017     C                   GOTO      ENDC03
077130011017    2C                   ENDIF
077140011017    1C                   ENDIF
077150011206      *
077160011206      * Controllo che la somma degli importi immessi non superi il valore
077170011206      * impostato in §GEIMF
077180011206      *      prima controllo il totale imponibile
077190011206  1b c                   if        vidimv <> 0
077200011206     c                   eval      w0173 = vidimv
077210011206      *      controllo se devo convertire il totale imponibile
077220011206  2b c                   if        viddiv <> §gedcn
077230030612     c                   clear                   YeurcoDS
077240011206     c                   eval      yecdvi = viddiv
077250011206     c                   eval      yecdvo = §gedcn
077260011206     c                   eval      yecimi = vidimv
077270011206     c                   move      decsav        yecdco
077280011206     c                   call      'YEURCO'
077290030612     c                   parm                    YeurcoDS
077300011206  3b c                   if        yecesi = ' '
077310011206     c                   eval      w0173 = yecimo
077320011206  3e c                   endif
077330011206  2e c                   endif
077340011206      *      controllo se l'imponibile supera il limite fatturabile
077350011220  2b c                   if        w0173 > imfsav
077360011206     c                   eval      vidmsg = msg(107)
077370011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
077380011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
077390011206     c                   eval      *in50 = *on
077400011206     c                   eval      *in90 = *on
077410011206     c                   eval      *in28 = *on
077420011206     c                   goto      endc03
077430011206  2e c                   endif
077440011206  1x c                   else
077450011206      *      altrimenti controllo tutti gli altri importi
077460011206     c                   eval      tot_importi = vidpor
077470011206      *      leggo le varie
077480011206  2b c                   do        20            nrr
077490011206     c     nrr           chain     lr48s03                            32
077500011206  3b c                   if        *in32 = *off and v3cva1 <> 0
077510011206  4b c                   if        flgvrn = '1' and v3cva1 = 88888888
077520011206     c                             and v3csv1 = 'N'
077530011206     c                   iter
077540011206  4e c                   endif
077550030624  4b c                   if        v3cva1 = 88888888
077560030624     c                             and v3csv1 = '&'
077570030624     c                   iter
077580030624  4e c                   endif
077590011206     c                   eval      tot_importi = tot_importi + v3cva1
077600011206  3e c                   endif
077610011206  2e c                   enddo
077620011206     c                   eval      w0173 = tot_importi
077630011206      *      controllo se devo convertire il totale importi
077640011206  2b c                   if        viddiv <> §gedcn
077650030612     c                   clear                   YeurcoDS
077660011206     c                   eval      yecdvi = viddiv
077670011206     c                   eval      yecdvo = §gedcn
077680011206     c                   eval      yecimi = tot_importi
077690011206     c                   move      decsav        yecdco
077700011206     c                   call      'YEURCO'
077710030612     c                   parm                    YeurcoDS
077720011206  3b c                   if        yecesi = ' '
077730011206     c                   eval      w0173 = yecimo
077740011206  3e c                   endif
077750011206  2e c                   endif
077760011206      *      controllo se l'imponibile supera il limite fatturabile
077770011220  2b c                   if        w0173 > imfsav
077780131016     c                   eval      vidmsg = msg(107)
077790011220     c                   eval      %subst (vidmsg:54:14) = (%editc(imfsav: '4'))
077800011206     c                   eval      %subst (vidmsg:69:3) = §gedcn
077810011206     c                   eval      *in50 = *on
077820011206     c                   eval      *in90 = *on
077830011206     c                   eval      *in28 = *on
077840011206     c                   goto      endc03
077850011206  2e c                   endif
077860011206  1e c                   endif
077870131016     c
077880131016     c* CONTROLLI PER BOLLA CONTABILIZZATA E PGM NON RICHIAMATO (29 ON)
077890131016     c* se f6=conferma
077900131016     c                   if        *in29 and *inkf
077910131016     c* Se eseguito F14=Tassazione --> errore bolla contabilizzata
077920131016     c                   if        winf14='1'
077930131016     c                   eval      vidmsg = msg(151)
077940131016     c                   eval      *in90 = *on
077950131016     c                   eval      *in28 = *on
077960131016     c                   goto      endc03
077970131016     c                   endif
077980131016     c* Se non variato nè ias nè vmd -> errore premere F12
077990131016     c                   if        vidias=bol_vidias and
078000131016     c                             (vidvas=bol_vidvas or vidias=0) and
078010131016     c                             vidvmd=bol_vidvmd and
078020131016     c                             (vidvad=bol_vidvad or vidvmd=0)
078030131016     c                   eval      vidmsg = msg(152)
078040131016     c                   eval      *in90 = *on
078050131016     c                   eval      *in28 = *on
078060131016     c                   goto      endc03
078070131016     c                   endif
078080131016     c                   endif
078090911210     C*
078100941209     C     ENDC03        ENDSR
078110030710      *---------------------------------------------------------------*
078120030710      * Gestione videata password x importo da assicurare             *
078130030710      *---------------------------------------------------------------*
078140060201     c**   Sr_Gespsw     BegSr
078150060201     C**
078160060201     c**                 Clear                   w01psw
078170060201     c**                 Do        *Hival
078180060201     c**                 Exfmt     lr48w01
078190060201     c**                 Eval      *In28 = *Off
078200060201     c**                 If        *InKl
078210060201     c**                 Eval      *In90 = *On
078220060201     c**                 Leave
078230060201     c**                 EndIf
078240060201     c**                 If        w01psw = *Blanks
078250060201     c**                 Eval      w01msg = 'Immettere la password'
078260060201     c**                 Eval      *In28 = *On
078270060201     c**                 Iter
078280060201     c**                 EndIf
078290060201     c**                 If        w01psw <> miapsw
078300060201     c**                 Eval      w01msg = 'Password errata'
078310060201     c**                 Eval      *In28 = *On
078320060201     c**                 Iter
078330060201     c**                 EndIf
078340060201     c**                 Eval      wokpsw = '1'
078350060201     c**                 Leave
078360060201     c**                 EndDo
078370060201     C**
078380060201     c**                 Endsr
078390920319     C*
078400930505     C* CONTROLLO COMUNI  ALL CONTR3 3 CONTR 4 -----------------------*
078410920319     C     CTRCOM        BEGSR
078420920319     C                   SETOFF                                       90
078430030620     C                   CLEAR                   SAV_INDSIN
078440941103     C**
078450920319     C* CODICE CLIENTE
078460941103     C**
078470950104     C*               SALTO CONTROLLO SE SPEDIZIONE IN FRANCO  IN ARRIV
078480950104     C*               LO ESEGUO SE SONO IN PARTENZA
078490941103    1C     *IN36         IFEQ      *OFF
078500941222     C     *IN10         OREQ      *OFF
078510941103     C*
078520941103     C* F7=RICERCA ALFABETICA
078530941103    2C   KG              DO
078540941212     C                   MOVEL     VIDDSA        PARDUT           30
078550920319     C                   MOVEL     *BLANKS       DESCDS           48
078560920319     C                   MOVEL     DESKSC        DESCDS
078570920319     C* PARSTA=9 ESCLUDO ANNULLATI
078580920319     C                   Z-ADD     9             PARSTA
078590060131     C                   Z-ADD     dutkci        CCC               4 0
078600941103    3C     VIDKLP        IFEQ      0
078610000510     C   10              MOVEL     ARBLNA        PARKSM
078620941103     C                   ELSE
078630000510     C                   MOVEL     VIDKLP        PARKSM
078640941103    3C                   ENDIF
078650991112     C                   CLEAR                   PARDIT
078660000510     C                   Z-ADD     1             PARNUM
078670941103     C*
078680000510     C                   CALL      'XALFA3BR'
078690920319     C                   PARM                    PARDUT
078700920319     C                   PARM                    CODUT
078710920319     C                   PARM                    DESCDS
078720920319     C                   PARM                    CCC
078730920319     C                   PARM                    PARSTA            1 0
078740961016     C                   PARM                    PARFLR           90
078750991112     C                   PARM                    PARDIT            3
078760000510     C                   PARM                    PARNUM            2 0
078770000510     C                   PARM                    PARKCM           80
078780000510     C                   PARM                    PARKSM          140
078790000510     C                   PARM                    PARKDM           60
078800941103    3C     PARSTA        IFEQ      -1
078810920319     C*
078820941103    4C     FLGCVR        IFEQ      '2'
078830941212     C*
078840990930     C     WAR62         IFEQ      ' '
078850941212     C                   MOVEL     ARBLNA        VIDKLP
078860941212     C                   MOVEL     9999          VIDKSC
078870941212     C                   ELSE
078880991007     C                   MOVEL     AR6KSC        VIDKLP
078890991007     C                   MOVE      AR6KSC        VIDKSC
078900941212     C                   ENDIF
078910941212     C*
078920941103   X4C                   ELSE
078930920319     C                   MOVEL     ARBKSC        VIDKLP
078940920319     C                   MOVE      ARBKSC        VIDKSC
078950941103    4C                   END
078960920319     C*
078970941103   X3C                   ELSE
078980020305     C                   MOVEL     PARKSM        ACOKSC
078990920319     C                   MOVEL     ACOKSC        VIDKLP
079000920319     C                   MOVE      ACOKSC        VIDKSC
079010970814     C                   CLEAR                   VIDRSM
079020970814     C                   CLEAR                   VIDINM
079030970814     C                   CLEAR                   VIDCAM
079040970814     C                   CLEAR                   VIDLOM
079050970814     C                   CLEAR                   VIDPRM
079060970814     C                   CLEAR                   VIDNZM
079070970814     C                   CLEAR                   VIDPIM
079080941103    3C                   END
079090941103     C*
079100941103     C                   SETON                                        9047
079110941103     C                   GOTO      ENDCOM
079120941212    2C                   ENDDO
079130920319     C* CONTROLLO
079140941103    2C     VIDKLP        IFEQ      0
079150941103     C                   MOVEL     MSG(33)       VIDMSG
079160941103     C                   SETON                                        469028
079170941103     C                   GOTO      ENDCOM
079180941103    2C                   END
079190980722     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE DELLO STESSO S.I. IN
079200991112     C*  CUI SONO  PER LE FATTURE IMMEDIATE
079210040930     C     VIDKLP        CHAIN     AZORG01L                           30
079220040930     c*
079230020725     C     D66BFI        IFEQ      'S'
079240020725     C**N30ORGDIT        IFNE      O50PRA
079250020725     C**                 MOVEL     MSG(81)       VIDMSG
079260020725     C**                 SETON                                        469028
079270020725     C**                 GOTO      ENDCOM
079280020725     C**                 ENDIF
079290020508     c* sempre per le fatture immediate se cliente 9999 il p.o. cliente
079300020508     c* deve essere = alla linea di arrivo della bolla
079310020508     c     vidksc        ifeq      9999
079320020508     c     vidklp        andne     arblna
079330020508     C                   MOVEL     MSG(35)       VIDMSG
079340020508     C                   SETON                                        479028
079350020508     C                   GOTO      ENDCOM
079360020508     c                   endif
079370020306     C                   ENDIF
079380020306     c* se variazione di mittente e cliente 8888 il p.o. cliente deve
079390020306     c* essere = alla linea di partenza bolla
079400020306     c     flgcvr        ifeq      'M'
079410020306     c     vidksc        andeq     8888
079420020306     c     vidklp        andne     arblnp
079430020306     C                   MOVEL     MSG(35)       VIDMSG
079440020306     C                   SETON                                        479028
079450020306     C                   GOTO      ENDCOM
079460020306     c                   endif
079470020508     c* se variazione di bolla in partenza e cliente 9999 errore
079480020508     c     *in10         ifeq      *off
079490020508     c     vidksc        andeq     9999
079500020508     C                   MOVEL     MSG(111)      VIDMSG
079510020508     C                   SETON                                        479028
079520020508     C                   GOTO      ENDCOM
079530020508     c                   endif
079540930507     C*
079550920319     C                   MOVEL     VIDKLP        KSC
079560920319     C                   MOVE      VIDKSC        KSC
079570000510     C  N30              MOVEL     ORGDE3        OG143
079580020318     C  N30              MOVEL     §OGNTW        CLINTW
079590020318     C   30              CLEAR                   CLINTW
079600040930     c* non accetto clienti con ntw = "LOG" o "XXX'
079610040930     c                   if        clintw='LOG'  or clintw='XXX'
079620040930     C                   MOVEL     MSG(130)      VIDMSG
079630040930     C                   SETON                                        469028
079640040930     C                   GOTO      ENDCOM
079650040930     c                   endif
079660051220     c* non è possibile modificare cliente da dpd a non dpd e viceversa
079670051220     c     flgcvr        ifeq      'M'
079680051220     c                   if        (clintw = 'DPD' and clintw <> memntw) or
079690051220     c                             (memntw = 'DPD' and clintw <> memntw)
079700051220     C                   MOVEL     MSG(137)      VIDMSG
079710051220     C                   SETON                                        469028
079720051220     C                   GOTO      ENDCOM
079730051220     c                   endif
079740051220     c                   endif
079750941103     C**
079760920319     C*
079770941103     C     KACO          CHAIN     CNACO000                           30
079780941103    3C  N30ACOFLG        IFNE      ' '
079790920319     C                   SETON                                        30
079800941103    3C                   END
079810941103     C* ERRORE
079820941103    3C     *IN30         IFEQ      *ON
079830941103     C     VIDKSC        OREQ      8888
079840941222     C     *IN10         ANDEQ     *ON
079850941223     C     VIDKSC        OREQ      9999
079860941223     C     *IN36         ANDEQ     *ON
079870941103     C                   MOVEL     MSG(36)       VIDMSG
079880941103     C                   SETON                                        479028
079890941103     C                   GOTO      ENDCOM
079900941103    3C                   ENDIF
079910930507     C*
079920130315    3C**   ACOABL        IFEQ      '8'
079930130315    3C**   ACOABL        OREQ      '*'
079940130315     c
079950130315    3C     ACOABL        ifne      *blanks
079960130418     c     ksc           andne     kscbolla
079970040930     C                   MOVEL     mSG(37)       VIDMSG
079980941103     C                   SETON                                        479028
079990941103     C                   GOTO      ENDCOM
080000941103    3C                   END
080010930507     C*
080020920319     C* SE SEGUE FATTURA NON SI PUO' METTERE COD 9999
080030941103    3C   13VIDKSC        IFEQ      9999
080040941103     C                   MOVEL     MSG(38)       VIDMSG
080050941103     C                   SETON                                        479028
080060941103     C                   GOTO      ENDCOM
080070941103    3C                   END
080080930507     C*
080090970822     C     VIDKSC        IFNE      9999
080100970822     C     VIDKSC        ANDNE     8888
080110970822     C                   MOVEL     ACORAG        DESKSC
080120941103    2C                   END
080130941220     C*
080140941223     C* SALVO ULTIMO CODICE IMPOSTATO
080150941220    2C     COMKSC        IFNE      KSC
080160941220     C                   CLEAR                   CIN
080170941220     C                   CLEAR                   DIE
080180941220     C                   MOVEL     KSC           COMKSC            7 0
080190941220    2C                   END
080200941103    1C                   END
080210030620     C* CERCO IN CNIND IL DIRITTO DI FATTURAZIONE
080220030620     C*
080230030620     C     KACO          CHAIN     CNIND00F                           30
080240030620    3C                   IF        %FOUND(CNIND00F)
080250030620     C                   EVAL      SAV_INDSIN = INDSIN
080260030620     C                   ELSE
080270030620     C                   CLEAR                   SAV_INDSIN
080280030620     C                   ENDIF
080290941103     C**
080300941215     C* CARICO CODICI TARIFFA DEL CLIENTE ED ESEGUO CONTROLLO
080310941103     C**
080320930507     C                   EXSR      CARCTR
080330941209     C*
080340941209     C* SE HO RICERCATO LA TARIFFA, RIEMETTO LA VIDEATA
080350050202    1C     ilv59RIC      IFEQ      'S'
080360941223     C                   SETON                                        9048
080370941209     C                   GOTO      ENDCOM
080380941209    1C                   ENDIF
080390941209     C*
080400941215     C* ATTIVATO IL "?" SUL CODICE TARIFFA
080410050202    1C     olv59ERR      IFEQ      'R'
080420050202     C                   MOVEL     olv59CTR      VIDCTR
080430050202     C                   MOVEL     olv59DFS      DE2CTR
080440941223     C                   SETON                                        9048
080450941209     C                   GOTO      ENDCOM
080460941209    1C                   ENDIF
080470941209     C* ERRORE
080480050202    1C     olv59MSG      IFNE      *BLANKS
080490060606     c* Nella videata della variazione del cod mittente, non do errore
080500060606     c*  se premuto F10 per la modifica del codice tariffa
080510060606     c*  ma disabilito F12
080520060606     c                   if        *inkj and flgcvr='M'
080530060606     c                   clear                   w48f12
080540060606     c                   else
080550050202     C                   MOVEL     olv59MSG      VIDMSG
080560941223     C                   SETON                                        904828
080570941209     C                   GOTO      ENDCOM
080580941209    1C                   ENDIF
080590060606    1C                   ENDIF
080600930507     C*
080610941209     C                   MOVEL     VIDCTR        COMCTR
080620050202     C                   MOVEL     olv59PRG      COMPRG
080630021121
080640160225      * Per bolla FedEx
080650160225If  1c                   If        lnantW = 'FED'
080660160225     c
080670021122      * --> se cliente 8888
080680160225      *     il CTR a video deve essere uno delle tariffe fedex
080690021122If  2c                   If        VidKsc = 8888
080700021122      * ---> e O27Fie = 'M'
080710021122If  3c                   If        WFie =  'M'
080720021122      * la tariffa deve essere quella della cartello
080730160225If  4c                   If        VidCtr <> FedM_ctr
080740021122     c                   Eval      *In48 = *On
080750021122     c                   Eval      *In28 = *On
080760021122     c                   Eval      *In90 = *On
080770021122     c                   Movel     Msg(114)      VidMsg
080780021122     c                   Goto      EndCom
080790021122    4c                   EndIf
080800021122      * ---> e O27Fie non è = 'M'
080810021122   x3c                   Else
080820160225    4c                   if        Vidctr <> FedD_ctr
080830021121     c                   Eval      *In48 = *On
080840021121     c                   Eval      *In90 = *On
080850021121     c                   Eval      *In28 = *On
080860021121     c                   Movel     Msg(114)      VidMsg
080870021121     c                   Goto      EndCom
080880021122    4c                   EndIf
080890021122    3c                   EndIf
080900160225     c
080910021121      * --> se cliente codificato
080920021121   x2c                   Else
080930021121      * ---> e non sono state trovate tariffe
080940021121If  3c                   If        Wtks <> 'S'
080950021121      *      non posso accettare una tariffa documenti se il peso della
080960021121      *      bolla superare il limite previsto in tabella "DFE"
080970021121     c                   Move      Vidctr        DueCtr
080980021121If  4c                   If        ArbPkf > §DfePkgD and
080990021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
081000021121     c                   Eval      *In48 = *On
081010021121     c                   Eval      *In90 = *On
081020021121     c                   Eval      *In28 = *On
081030021121      * Preparo il msg
081040021121     c                   Movea     Msg(115)      K78
081050021121     c                   Movel     §DfePkgD      W006a
081060021121     c                   Movea     W006a         Ski(10)
081070021121     c                   Z-Add     10            i
081080021121     c                   ExSr      Abbl
081090021121     c                   Movea     Ski(10)       K78(59)
081100021121     c                   Move      §DfePkgD      W001a
081110021121     c                   Movel     W001a         K78(66)
081120021121     c                   Movea     K78           VidMsg
081130021121     c                   Goto      EndCom
081140021121    4c                   EndIf
081150021121    3c                   EndIf
081160021121    2c                   EndIf
081170021121    1c                   EndIf
081180920319     C*
081190941103     C     ENDCOM        ENDSR
081200011017     C*
081210011017      *--- TASSAZIONE VARIA 'N' --------------------------------------*
081220011017     C     TASSAN        BEGSR
081230011017      *
081240011017     C                   CLEAR                   DTAS
081250011017     C                   CLEAR                   DTASV
081260011017     C                   MOVEL     VIDKLP        TASKSC
081270011017     C                   MOVE      VIDKSC        TASKSC
081280011017     C                   MOVEL     VIDCTR        TASCTR
081290011017     C                   Z-ADD     ARBPKF        TASPKF
081300011017     C                   MOVEL     ARBLNP        TASLNP
081310011017     C                   MOVEL     ARBCTS        TASCTS
081320011017     C                   MOVEL     ARBDSP        TASDSP
081330011017     C                   MOVEL     ARBDSP        TASDCT
081340011017     C                   MOVEL     ARBVLF        TASVLF
081350011017     C                   MOVEL     ARBTSP        TASTSP
081360011017     C     WBOL          IFEQ      '1'
081370011017     C                   MOVEL     §3ATB1        TASTBL
081380011017     C                   ELSE
081390011017     C                   MOVEL     §3ATB2        TASTBL
081400011017     C                   ENDIF
081410011017     C                   MOVEL     ARBLNA        TASLNA
081420011017     C                   MOVEL     ARBNCL        TASNCL
081430011017     C                   MOVEL     ARBQFT        TASQFT
081440011017     C     WBOL          IFEQ      '1'
081450011017     C     §3AFCA        ANDEQ     1
081460011017     C     WBOL          OREQ      '2'
081470011017     C     §3AFCA        ANDEQ     2
081480011017     C                   MOVEL     AR9CAS        TASCAS
081490011017     C                   MOVEL     AR9VCA        TASVCA
081500011017     C                   MOVEL     AR9TIC        TASTIC
081510011017     C                   ENDIF
081520011017     C                   MOVEL     ARBIAS        TASIAS
081530011017     C                   MOVEL     ARBVAS        TASVAS
081540011017     C                   MOVEL     ARBCAD        TASCAD
081550011017     C                   MOVEL     ARBNZD        TASNZD
081560011017     C                   MOVEL     ARBFIN        TASFIN
081570011017     C                   MOVEL     ARBCAM        TASCAM
081580011017     C                   MOVEL     ARBNZM        TASNZM
081590011017     C                   MOVEL     ARBFAP        TASFAP
081600011017     C                   MOVEL     ARBTC1        TASTC1
081610011017     C                   MOVEL     ARBTC2        TASTC2
081620011017     C     V1CDDT        IFEQ      'C'                                          SI DDT
081630011017     C     V1CDDT        OREQ      'S'
081640031028     c     v1cddt        oreq      'P'
081650031028     c     v1cddt        oreq      'Q'
081660011017     C                   MOVEL     'S'           TASDDT
081670011017     C                   ENDIF
081680060925     c*****              movel     arbfbr        tasflo
081690060925     c                   clear                   dtas01
081700060925     c                   movel     arbfbr        §asfbr
081710060925     c                   movel     arbcca        §ascca
081720060925     c                   eval      tasflo = dtas01
081730011017      **
081740011017      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
081750011017      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
081760020305     C     LNANTW        ifeq      'COR'
081770020305     c     LNANTW        oreq      'MES'
081780020305     c     LNANTW        oreq      'PPT'
081790011017     C                   MOVEL     ARBFDN        TASFDN
081800011017     C                   ENDIF
081810011017      **
081820011017      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
081830030612     C                   CLEAR                   Tisi95DS
081840011017     C                   MOVEL     ARBLOM        I95LOC
081850011017     C                   MOVEL     ARBCAM        I95CAP
081860011017     C                   MOVEL     ARBNZM        I95NAR
081870011017     C                   MOVEL     '3'           I95TCN
081880011017     C                   MOVEL     ARBDSP        I95DAT
081890011017     C     §3AFCA        IFNE      0
081900011017     C                   MOVEL     'S'           I95FCA
081910011017     C                   ENDIF
081920011017     C                   MOVEL     §3ATB1        I95TPO
081930020305     c     lnpntw        ifeq      'FED'
081940020305     c                   movel     'S'           i95fi1
081950020305     c                   endif
081960011017     C                   CALL      'TISI95R'
081970030612     C                   PARM                    Tisi95DS
081980011017     C     O95ERR        IFEQ      *BLANKS
081990011017     C                   MOVEL     O95CTS        TASMCT
082000011017     C                   ENDIF
082010011017     C                   MOVEL     'S'           WCAL13            1
082020011017      * ELABORO E CHIUDO CON LR IL PGM
082030011017     C                   MOVEL     ' '           TASTLA
082040020930     c* imposto peso vdl e colli rilevati
082050030203     c**!!!              clear                   dtas01
082060030203     c**!!!              z-add     arbncp        §asncp
082070030203     c**!!!              z-add     arbpkc        §aspkc
082080030203     c**!!!              movel     dtas01        tasflo
082090030203     c                   z-add     arbncp        Tasncp
082100030203     c                   z-add     arbpkc        Taspkc
082110030204      * Bancali
082120030205     c                   If        %Subst(ArbGva:1:1) = 'B'
082130030729     c                   Eval      TasBan = V1cBan
082140030205     c                   EndIf
082150030729      * Colli Originali
082160030729     c                   If        %Subst(ArbGva:1:1) = 'O'
082170030729     c                   Eval      TasNcl = V1cCor
082180030729     c                   EndIf
082190050804     c
082200050804     c                   eval      parca  = ' '
082210050804     c                   movel     paramt        kpjbu
082220011017      *
082230011017     C                   CALL      'TNSF20R'
082240011017     C                   PARM                    KPJBA
082250011017     C                   PARM                    DTAS
082260011017     C                   PARM                    DTASV
082270011018      *
082280011018      * Tassazione andata buon fine
082290011018     C     TASERR        IFEQ      *BLANKS
082300011018      *
082310011018      * Controllo la divisa della tassazione con la divisa del video
082320011018      * se non corrispondono devo convertire la varia 'N' nella divisa
082330011018      * del video
082340011018     C     TASDIV        IFNE      VIDDIV
082350011018      * aggancio la tabella CV per sicurezza
082360011018     C                   CLEAR                   DSCV
082370011018     C                   MOVEL     'CV'          COD
082380011018     C                   MOVEL(P)  VIDDIV        KEY
082390011018     C                   EXSR      CTABEL
082400011018     C  N30              MOVEL     TBLUNI        DSCV
082410011018      *
082420030612     C                   CLEAR                   YeurcoDS
082430011018     C                   MOVEL     TASDIV        YECDVI
082440011018     C                   MOVEL     VIDDIV        YECDVO
082450011018     C                   Z-ADD     TASIMV        YECIMI
082460011018     C     §CVFDC        IFEQ      'S'
082470011018     C                   MOVE      '3'           YECDCO
082480011018     C                   ELSE
082490011018     C                   MOVE      '0'           YECDCO
082500011018     C                   ENDIF
082510011018     C                   CALL      'YEURCO'
082520030612     C                   PARM                    YeurcoDS
082530011018     C     YECESI        IFEQ      ' '
082540011018     C                   Z-ADD     YECIMO        TASIMV
082550011018     C                   ELSE
082560011018     C                   Z-ADD     88888888      TASIMV
082570011018     C                   ENDIF
082580011018     C                   ENDIF
082590011017      *
082600011017     C                   Z-ADD     TASIMV        V3CVA1
082610011017     C                   Z-ADD     TASIMV        VARVA1
082620011017     C                   ENDIF
082630011017      *
082640011017     C                   ENDSR
082650910429     C*
082660050531begsrC*--- TASSAZIONE BOLLA ------------------------------------------*
082670910429     C     TASSAZ        BEGSR
082680991005     C                   CLEAR                   DTAS
082690991005     C                   CLEAR                   DTASV
082700020503     C                   CLEAR                   wtassa            1
082710030620     C                   CLEAR                   wtassad           1
082720030620     C                   CLEAR                   itassa           11 3
082730050601     c*
082740050601     c* richiamo routine di impostazione campi "comuni" da passare a
082750050601     c* pgm tnsf20r
082760050601     c                   exsr      SR_rtassa
082770910429     C*
082780941221     C* CARICO LE VARIE
082790941221     C*
082800980513     C     WBOL          IFEQ      '1'
082810030709     c                   eval      flgsf20 = 'S'
082820991012     C                   EXSR      RDTASV
082830030709     c                   eval      flgsf20 = ' '
082840980513     C*
082850980513     C                   ELSE
082860040123      * carico varie seconda bolla
082870040123     C                   MOVEL     'G'           TASSVA
082880040123     c                   Eval      TascVag = 'S'
082890041202     C**!!!VI2VA1        IFGT      0
082900040123     C                   MOVEL     VI2SV1        VSV(1)
082910040123     C                   Z-ADD     VI2VA1        VVA(1)
082920040123     c                   Movel     Vi2Es1        ves(1)
082930041202     C**!!!              ENDIF
082940041202     c**!!!              If        Vi2Va2 > *Zeros
082950040123     c                   Movel     Vi2Sv2        Vsv(2)
082960040123     c                   Z-add     Vi2Va2        Vva(2)
082970040123     c                   Movel     Vi2Es2        ves(2)
082980041202     c**!!!              EndIf
082990041202     c**!!!              If        Vi2Va3 > *Zeros
083000040123     c                   Movel     Vi2Sv3        Vsv(3)
083010040123     c                   Z-add     Vi2Va3        Vva(3)
083020040123     c                   Movel     Vi2Es3        ves(3)
083030041202     c**!!!              EndIf
083040041202     c**!!!              If        Vi2Va4 > *Zeros
083050040123     c                   Movel     Vi2Sv4        Vsv(4)
083060040123     c                   Z-add     Vi2Va4        Vva(4)
083070040123     c                   Movel     Vi2Es4        ves(4)
083080041202     c**!!               EndIf
083090041202     c**!!!              If        Vi2Va5 > *Zeros
083100041130     c                   Movel     Vi2Sv5        Vsv(5)
083110041130     c                   Z-add     Vi2Va5        Vva(5)
083120041130     c                   Movel     Vi2Es5        ves(5)
083130041202     c**!!!              EndIf
083140041202     c**!!!              If        Vi2Va6 > *Zeros
083150041130     c                   Movel     Vi2Sv6        Vsv(6)
083160041130     c                   Z-add     Vi2Va6        Vva(6)
083170041130     c                   Movel     Vi2Es6        ves(6)
083180041202     c**!!!              EndIf
083190040126     c                   xfoot     vva           TotImv
083200040126     c                   Z-add     TotImv        TasImv
083210980608     C                   ENDIF
083220011211
083230011211      * Data fattura se c'è
083240030203     c**!!!              clear                   dtas01
083250020930     c**                 if        ar6dft <> 0
083260020930     c**                 movel     ar6dft        §asdat
083270020930     c**                 endif
083280020503     c**
083290020503     c                   if        wtassa=' '
083300020503     c* se devo tassare solo una varia, da tipo bolla, la imposto
083310020503     c                   if        §3asva<>*blanks
083320020503     c                   eval      tassva=§3asva
083330020503     c                   endif
083340030620     c                   endif
083350030925     c* se diritto di fatturazione uguale a zero non calcolo la varia D
083360030925     c                   if        sav_indsin = 0
083370030925     c                   eval      wtassad = 'N'
083380030925     c                   endif
083390030623      * se tassazione fattura immediata o ritassazione prepagato
083400030623      * passo al TNSF20R la varia "D" e l'importo del diritto
083410030620      * di fatturazione da sommare che prendo da cnind (INDSIN)
083420030623     c                   if        (*in14 and not *in29  and wtassad = ' ')  or
083430030623     c                             (not *in10 and vidnft > 0 and not *in29 and
083440030623     c                             wtassad = ' ')
083450030925      *
083460030620     c                   move      sav_indsin    tasivdf
083470030620     c                   eval      tassvdf = 'D'
083480030620      * se bolla già tassat devo sommare la sola varia "D"
083490030620      * imponibile > 0 oppure imponibile a zero ma singola varia vaolorizzata x bolle speciali
083500040123     c**!!!              if        tasimv > 0 or (tasimv = 0 and itassa > 0)
083510040123     c                   if        tasimv > 0
083520030620     c                   eval      tassva = 'D'
083530030620     c                   endif
083540030620      * se singola varia per bolla speciale non è da calcolare ed è uguale a zero non calcolo la
083550030620      * varia D
083560030620     c                   if        wtassa = 'N' and tasimv = 0 and itassa = 0
083570030620     c                   eval      wtassad = 'N'
083580030620     c                   endif
083590030620
083600030620     c                   endif
083610011211
083620030620      * se non è tassazione fattura immediata non devo calcolare la varia D
083630030623      * o ritassazione prepagato
083640030623     c*                  if        not *in14
083650030623     c                   if        not *in14  or  *in29 or
083660030623     c                             (not *in10 and vidnft = 0)
083670030620     c                   eval      wtassad = 'N'
083680030620     c                   endif
083690030620      * se non devo calcolare la singola varia e neppure la varia D salto la tassazione
083700030620     c                   if        wtassa = 'N' and wtassad = 'N'
083710030620     c                   goto      endtas
083720030620     c                   endif
083730151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
083740151127     c* è solo per tassare una specifica varia
083750151126     C     WBOL          IFEQ      '1'
083760151126     c* PACKING LIST
083770151126     c                   if        par5als='S' or par5alx='S'
083780151126     c                   eval      tasspl='S'
083790151126     c                   endif
083800151126     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
083810151126     C                   MOVEL     'M'           WTRC
083820151126     C     KARB2         CHAIN(N)  FiAR401l
083830151126     c                   if        %found(fiar401l)
083840151126     c                   movel     ar4not        dsbl4m
083850151126     c                   if        §b4ado='S'
083860151126     c                   eval      tasprt='S'
083870151126     c                   endif
083880151126     c                   endif
083890151126
083900151126     c                   endif
083910050804     c
083920050804     c                   eval      parca  = ' '
083930050804     c                   movel     paramt        kpjbu
083940910429     C*
083950941209     C                   CALL      'TNSF20R'
083960910429     C                   PARM                    KPJBA
083970991005     C                   PARM                    DTAS
083980991005     C                   PARM                    DTASV
083990910429     C*
084000941212    1C     TASERR        IFNE      *BLANKS
084010980608     C     TASMSG        IFNE      *BLANKS
084020980608     C                   MOVEL(P)  TASMSG        VIDMSG
084030980608     C                   ELSE
084040941212     C                   MOVEL     MSG(55)       VIDMSG
084050980608     C                   ENDIF
084060941209     C                   SETON                                        499028
084070991006     C* SE ERRORI IN TASSAZIONE SPROTEGGO LA DIVISA
084080991006     C                   SETOFF                                       07
084090941212   X1C                   ELSE
084100980513     C     WBOL          IFEQ      '1'
084110950421     C                   MOVEL     TASQFT        VIDQFT
084120950424     C                   MOVEL     TASQFT        VARQFT
084130910429     C                   MOVEL     TASPOR        VIDPOR
084140991006     C                   MOVEL     TASPOR        VARPOR
084150991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
084160991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
084170991025     C* DIVISA
084180991025     C     VIDDIV        IFNE      TASDIV
084190991025     C     *INKF         ANDEQ     *ON
084200991025     C                   SETON                                        90
084210991025     C                   ENDIF
084220991025     C*
084230991005     C                   MOVEL     TASDIV        VIDDIV
084240991006     C                   MOVEL     TASDIV        WDIV
084250991005     C                   Z-ADD     TASIMV        VIDIMV
084260941209     C* CARICO LE VARIE
084270941212    2C                   DO        20            NRR
084280941209     C     NRR           CHAIN     LR48S03                            30
084290941209     C                   MOVEL     VSV(NRR)      V3CSV1
084300941209     C                   Z-ADD     VVA(NRR)      V3CVA1
084310991006     C                   Z-ADD     VVA(NRR)      VARVA1
084320941209     C                   MOVEL     VES(NRR)      V3CEV1
084330941209     C   30              WRITE     LR48S03
084340941209     C  N30              UPDATE    LR48S03
084350941212    2C                   ENDDO
084360941209     C*
084370980513    XC                   ELSE
084380040123     c                   Clear                   ContaSv
084390040123      * carico le varie seconda bolla
084400040123Do  2c                   Do        20            c
084410040123If  3c                   If        Vsv(c) <> §$2Iva and
084420040123     c                             Vsv(c) <> §$2Bol
084430040123     c                   Add       1             ContaSv
084440040123S   4c                   Select
084450040123     c                   When      ContaSv = 1
084460040123     c                   Movel     Vsv(c)        Vi2Sv1
084470040123     c                   Z-add     Vva(c)        Vi2Va1
084480040123     c                   Movel     Ves(c)        Vi2Es1
084490040123     c                   When      ContaSv = 2
084500040123     c                   Movel     Vsv(c)        Vi2Sv2
084510040123     c                   Z-add     Vva(c)        Vi2Va2
084520040123     c                   Movel     Ves(c)        Vi2Es2
084530040123     c                   When      ContaSv = 3
084540040123     c                   Movel     Vsv(c)        Vi2Sv3
084550040123     c                   Z-add     Vva(c)        Vi2Va3
084560040123     c                   Movel     Ves(c)        Vi2Es3
084570040123     c                   When      ContaSv = 4
084580040123     c                   Movel     Vsv(c)        Vi2Sv4
084590040123     c                   Z-add     Vva(c)        Vi2Va4
084600040123     c                   Movel     Ves(c)        Vi2Es4
084610041130     c                   When      ContaSv = 5
084620041130     c                   Movel     Vsv(c)        Vi2Sv5
084630041130     c                   Z-add     Vva(c)        Vi2Va5
084640041130     c                   Movel     Ves(c)        Vi2Es5
084650041130     c                   When      ContaSv = 6
084660041130     c                   Movel     Vsv(c)        Vi2Sv6
084670041130     c                   Z-add     Vva(c)        Vi2Va6
084680041130     c                   Movel     Ves(c)        Vi2Es6
084690040123    4c                   EndSl
084700040123    3c                   EndIf
084710040123    2c                   EndDo
084720040123     C**!!!              Z-ADD     1             C
084730040123     C**!!!TASSVA        LOOKUP    VSV(C)                                 33
084740040123     C*!!33              Z-ADD     VVA(C)        VI2VA1
084750040123     C*!!33              Z-ADD     VVA(C)        VA2VA1
084760040123     C*!!33              MOVEL     VSV(C)        VI2SV1
084770991025     C* SE DIVISA DIVERSA DA PRECEDENTE E PREMUTO F6 ACCENDO IL 90
084780991025     C* PER RIEMETTERE LA VIDEATA IN MODO DA VISUALIZZARE LA NUOVA
084790991025     C* DIVISA
084800040123     C*!!33VI2DIV        IFNE      TASDIV
084810040123     C     VI2DIV        IFNE      TASDIV
084820991025     C     *INKF         ANDEQ     *ON
084830991025     C                   SETON                                        90
084840991025     C                   ENDIF
084850040123     C*!!33              MOVEL     TASDIV        VI2DIV
084860040123     C*!!33              MOVEL     TASDIV        WDIV
084870040123     C                   MOVEL     TASDIV        VI2DIV
084880040123     C                   MOVEL     TASDIV        WDIV
084890040123     C*!!33VES(C)        IFNE      ' '
084900040123     C**!!!VI2CEI        ANDEQ     ' '
084910040123     C**!!!              MOVEL     VES(C)        VI2CEI
084920040123     C**!!!              ENDIF
084930980513     C                   ENDIF
084940020503     C                   ENDIF
084950980513     C*
084960030620    1C*****              ENDIF
084970941209     C*
084980911211     C     ENDTAS        ENDSR
084990050601     C*
085000050601     C*--- Riempimento campi comuni di passaggio a tnsf20r -----------*
085010050601     C     sr_rtassa     BEGSR
085020050601     C* CODIFICATO
085030050601    1C     VIDKSC        IFNE      9999
085040050601    1C     VIDKSC        ANDNE     8888
085050050601     C                   MOVEL     VIDKLP        TASKSC
085060050601     C                   MOVE      VIDKSC        TASKSC
085070050601     C                   MOVEL     VIDCTR        TASCTR
085080050601   X1C                   ELSE
085090050601     C*
085100050601     C* E HO LA DATA FATTURA USO QUELLA, ALTRIMENTI DATEU
085110050601     C     AR6DFT        IFGT      0
085120050601     C                   Z-ADD     AR6DFT        WDATA
085130050601     C                   ELSE
085140050601     C                   Z-ADD     DATEU         WDATA
085150050601     C                   ENDIF
085160050601     C*
085170050601     C* DAL TIPO SERVIZIO PRENDO LA TARIFFA CON CUI TASSARE
085180050601     C*
085190160225     c**                 eval      tasksc  = o27ksc
085200160225     c**                 eval      tasctr  = o27ctr
085210160225     C                   MOVEL     VIDKLP        TASKSC
085220160225     C                   MOVE      VIDKSC        TASKSC
085230160225     C                   MOVEL     VIDCTR        TASCTR
085240050601     C**
085250050601    1C                   ENDIF
085260050601     C*
085270050601     C                   Z-ADD     ARBPKF        TASPKF
085280050601     C                   MOVEL     ARBLNP        TASLNP
085290050601     C                   MOVEL     ARBCTS        TASCTS
085300050601     C                   MOVEL     ARBDSP        TASDSP
085310050601     C                   MOVEL     ARBDSP        TASDCT
085320050601     C                   MOVEL     ARBVLF        TASVLF
085330050601     C                   MOVEL     ARBTSP        TASTSP
085340050601     C     WBOL          IFEQ      '1'
085350050601     C                   MOVEL     §3ATB1        TASTBL
085360050601     C                   MOVEL     VIDDIV        TASDIV
085370050601     C                   Z-ADD     VIDIMV        TASIMV
085380050601     C                   ELSE
085390050601     C                   MOVEL     §3ATB2        TASTBL
085400050601     C                   MOVEL     VI2DIV        TASDIV
085410050601     C**!!!              Z-ADD     VI2VA1        TASIMV
085420050601     C                   END
085430050601     C                   MOVEL     ARBLNA        TASLNA
085440050601     C                   MOVEL     ARBNCL        TASNCL
085450050601     C                   MOVEL     VIDPOR        TASPOR
085460050601     C                   MOVEL     VIDQFT        TASQFT
085470050601     C     WBOL          IFEQ      '1'
085480050601     C     §3AFCA        ANDEQ     1
085490050601     C     WBOL          OREQ      '2'
085500050601     C     §3AFCA        ANDEQ     2
085510050601     C                   MOVEL     AR9CAS        TASCAS
085520050601     C                   MOVEL     AR9VCA        TASVCA
085530050601     C                   MOVEL     AR9TIC        TASTIC
085540050601     C                   ENDIF
085550050601     C                   MOVEL     VIDIAS        TASIAS
085560050601     C                   MOVEL     VIDVAS        TASVAS
085570050601     C                   MOVEL     ARBCAD        TASCAD
085580050601     C                   MOVEL     ARBNZD        TASNZD
085590050601     C                   MOVEL     ARBCAM        TASCAM
085600050601     C                   MOVEL     ARBNZM        TASNZM
085610050601     C                   MOVEL     ARBFIN        TASFIN
085620050601     C                   MOVEL     ARBFAP        TASFAP
085630050601     C                   MOVEL     ARBTC1        TASTC1
085640050601     C                   MOVEL     ARBTC2        TASTC2
085650050601     C     V1CDDT        IFEQ      'S'
085660050601     C     V1CDDT        OREQ      'C'
085670050601     c     v1cddt        oreq      'P'
085680050601     c     v1cddt        oreq      'Q'
085690050601     C                   MOVEL     'S'           TASDDT
085700050601     C                   ENDIF
085710060925     c****               movel     arbfbr        tasflo
085720060925     c                   clear                   dtas01
085730060925     c                   movel     arbfbr        §asfbr
085740060925     c                   movel     arbcca        §ascca
085750110630     c                   movel     wemd          §asemd
085760150108     c* Pin Code
085770150108     c                   if        arbgma<>*blanks
085780150108     c                   Movel     '7R'          cod
085790150108     c                   Movel(p)  ArbGMa        key
085800150108     c                   ExSr      ctabel
085810150108     c  n30              Movel     TblUni        ds7R
085820150108     c   30              Clear                   ds7R
085830150108     c                   if        §7rpincode='S'
085840150108     c                   movel     'S'           §aspin
085850150108     c                   endif
085860150108     c                   endif
085870060925     c                   eval      tasflo = dtas01
085880050601     C* SE BOLLA ARRIVI IN ASSEGNATO EXPORT, NON PASSO LA CONSEGNA A
085890050601     C*  MAGAZZINO PER FAR CALCOLARE SEMPRE LA VARIA "U"
085900050601     C     *IN10         IFEQ      *OFF
085910050601     C     *IN06         OREQ      *ON
085920050601     C     lnantw        oreq      'COR'
085930050601     c     lnantw        oreq      'MES'
085940050601     c     lnantw        oreq      'PPT'
085950050601     C                   MOVEL     ARBFDN        TASFDN
085960050601     C                   ENDIF
085970050601     C**
085980050601     C* CHAI SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
085990050601     C                   CLEAR                   Tisi95DS
086000050601     C                   MOVEL     ARBCAM        I95CAP
086010050601     C                   MOVEL     ARBNZM        I95NAR
086020050601     C                   MOVEL     '3'           I95TCN
086030050601     C                   MOVEL     ARBDSP        I95DAT
086040050601     C                   MOVEL     FLGTB1        I95TPO
086050050601     C     §3AFCA        IFNE      0
086060050601     C                   MOVEL     'S'           I95FCA
086070050601     C                   ENDIF
086080050601     c     lnpntw        ifeq      'FED'
086090050601     c                   movel     'S'           i95fi1
086100050601     c                   endif
086110050601     C**
086120050601     C                   CALL      'TISI95R'
086130050601     C                   PARM                    Tisi95DS
086140050601     C     O95ERR        IFEQ      *BLANKS
086150050601     C                   MOVEL     O95CTS        TASMCT
086160050601     C                   ENDIF
086170050601
086180050601     c                   z-add     arbncp        Tasncp
086190050601     c                   z-add     arbpkc        Taspkc
086200050601      * Bancali
086210050601     c                   If        %Subst(ArbGva:1:1) = 'B'
086220050601     c                   Eval      TasBan = V1cBan
086230050601     c                   EndIf
086240050601      * Colli Originali
086250050601     c                   If        %Subst(ArbGva:1:1) = 'O'
086260050601     c                   Eval      TasNcl = V1cCor
086270050601     c                   EndIf
086280050601     c                   endsr
086290050531     C*--- TASSAZIONE BOLLA ------------------------------------------*
086300050531     C     TASSAR        BEGSR
086310050601     c                   clear                   dtas
086320050601     c                   clear                   dtasv
086330050601     c* richiamo routine di riempimento campi da passare a tnsf20r
086340050601     c                   eval      wbol = '1'
086350050601     c                   exsr      SR_rtassa
086360050531     c                   eval      tassva = 'R'
086370050804     c
086380050531     c                   eval      parca  = 'S'
086390050531     c                   movel     paramt        kpjbu
086400050531      *
086410050531     C                   CALL      'TNSF20R'
086420050531     C                   PARM                    KPJBA
086430050531     C                   PARM                    DTAS
086440050531     C                   PARM                    DTASV
086450050722      *
086460050804      *
086470050722     c                   eval      parca  = ' '
086480050804     c***                movel     paramt        kpjbu
086490050722      *
086500050531     c                   endsr
086510911211     C*
086520011017     C*
086530991006     C* CALCOLO IL TOTALE FATTURA-------------------------------------*
086540990702     C     TOTFAT        BEGSR
086550030612     C                   CLEAR                   FNLV04DS
086560990702     C* AROTONDO IN ARRIVO O IN PARTENZA SE PREPAGATI NUOVI
086570990702    2C     *IN10         IFEQ      *ON
086580990702     C     *IN23         OREQ      *ON
086590991005     C                   MOVEL     VIDDIV        DIVGEI
086600991005     C                   EXSR      RIEGEI
086610991005     C                   MOVEL     §GEAED        D04AED
086620991005     C                   Z-ADD     §GEAVL        D04VAR
086630990702    2C                   ENDIF
086640991005     C                   MOVEL     'T'           D04FIM
086650990702     C                   MOVEL     ARBDFT        D04DFT
086660990702     C                   MOVEL     VIDIMV        D04IMV
086670990702     C                   MOVEL     VIDCEI        D04CEI
086680990702     C                   MOVEL     AR9TIC        D04TIC
086690990702     C                   MOVEL     AR9VCA        D04VCA
086700990702     C                   Z-ADD     AR9CAS        D04CAS
086710990702     C                   MOVEL     '1'           D04SPE
086720990702     C   23              MOVEL     '8'           D04SPE
086730991005     C                   MOVEL     §3ATB1        D04TBL
086740991005     C                   MOVEL     VIDDIV        D04DIV
086750990702     C                   Z-ADD     VIDPOR        D04POR
086760990702     C                   MOVEL     '1'           UNO               1
086770990702     C*
086780991005     C                   CALL      'FNLV04R'
086790030612     C                   PARM                    FNLV04DS
086800991005     C                   PARM                    DTASV
086810990702     C                   ENDSR
086820990702     C*
086830991005     C**************************************************************************
086840991005     C*  REPERIMENTO TABELLA GEI
086850991005     C**************************************************************************
086860991005     C     RIEGEI        BEGSR
086870030612     C                   CLEAR                   Tibs02DS
086880991117     C                   CLEAR                   DGEI
086890991117     C                   MOVEL     'C'           T02MOD
086900991117     C                   MOVEL     KNSIF         T02SIF
086910991117     C                   MOVEL     'GEI'         T02COD
086920991117     C                   MOVEL     DIVGEI        T02KE1
086930991117     C                   CALL      'TIBS02R'
086940991117     C                   PARM                    KPJBA
086950030612     C                   PARM                    Tibs02DS
086960991117     C     T02ERR        IFEQ      *BLANKS
086970991117     C                   MOVEL     T02UNI        DGEI
086980991117     C                   ENDIF
086990991005     C                   ENDSR
087000911211     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE TX -----------------*
087010911211     C     REGIS3        BEGSR
087020041027     C                   SETOFF                                       3709
087030130412     c                   clear                   wpda_ndc
087040130412     c                   clear                   wpda_ddc
087050130412     c                   clear                   wpda_dcm
087060041027      * salvo i campi relativi al valore da assicurare, qtà da fatturare, valore merce
087070041027     c                   eval      sav_arbias = arbias
087080041027     c                   eval      sav_arbvas = arbvas
087090041027     c                   eval      sav_arbqft = arbqft
087100041027     c                   eval      sav_arbvmd = arbvmd
087110041027     c                   eval      sav_arbvad = arbvas
087120041027     c                   movel     arbctr        sav_arbctr
087130950104     C*  GIRO UDATE
087140950104     C                   TIME                    W0140            14 0
087150950104     C* UDATE IN GGMMAAAA
087160950104     C                   MOVE      W0140         WDTGIO            8 0
087170950104     C*
087180950104     C* UDATE IN AAAAMMGG
087190950104     C                   Z-ADD     WDTGIO        G02DAT
087200950104     C                   MOVEL     *BLANK        G02ERR
087210950104     C                   CALL      'XSRDA8'
087220950104     C                   PARM                    WLBDAT
087230950104     C                   MOVEL     G02INV        DATEU             8 0
087240941221     C* PRENDO I VALORI ASSOLUTI
087250941221     C                   MLLZO     1             VIDPOR
087260941221     C                   MLLZO     1             VIDIMV
087270941221     C                   MLLZO     1             VIDIAS
087280941221     C                   MLLZO     1             VIDVMD
087290941221     C                   MLLZO     1             VIDQFT
087300941221     C                   MLLZO     1             VIDKLP
087310941221     C                   MLLZO     1             VIDKSC
087320070515     c
087330070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
087340070515     c* iornare direttamente il file
087350070515     c*  solo se cambiati dati su testa bolla
087360070515     c                   clear                   wsiaggio          1
087370070515  2b c                   if        sav_arbias <> vidias or sav_arbvas<>vidvas
087380070515     c                             or
087390070515     c                             sav_arbvmd <> vidvmd or sav_arbvad<>vidvad
087400070515     c                             or
087410070515     c                             sav_arbqft <> vidqft  or
087420070515     c                             sav_arbctr<>vidctr
087430070515     c                   eval      wsiaggio='S'
087440070515     c                   clear                   waltrabolla       1
087450070515     C   10KARB          CHAIN     FNBLP01L                           3091
087460070515     C  n10KARB          CHAIN     FNARB01L                           3091
087470070515     c*
087480070515     c                   if        *in91
087490070515     C                   SETON                                        28
087500070515     C                   MOVEL     MSG(4)        VIDMSG
087510070515     C                   GOTO      ENDRG3
087520070515     c                   else
087530070515     c                   if        *in30
087540070515     c                   eval      waltrabolla='N'
087550070515     c                   else
087560130411     c  n10              eval      wpda_ndc=arbndc
087570130411     c  n10              eval      wpda_ddc=arbddc
087580130411     c  n10              eval      wpda_dcm=arbdcm
087590070515     C   10KARB          CHAIN     FNARB01L                           30
087600070515     C  N10KARB          CHAIN     FNBLP01L                           30
087610070515     c* Alla fine per non sporcarmi i campi del record, richaino il
087620070515     c*  record originale che sto variando
087630130411     c   10              eval      wpda_ndc=arbndc
087640130411     c   10              eval      wpda_ddc=arbddc
087650130411     c   10              eval      wpda_dcm=arbdcm
087660070515     c                   endif
087670070515     c                   endif
087680070515     c                   endif
087690131120    1c                   if        wsiaggio=*blanks and d66pda<>' '
087700130412     c                   clear                   waltrabolla       1
087710130412    2c                   if        not *in10
087720130412     C     KARB          CHAIN(n)  FNARB01L                           30
087730130412    3c                   if        *in30
087740130412     c                   eval      waltrabolla='N'
087750130412    3c                   endif
087760130412    2c                   endif
087770130412     c                   eval      wpda_ndc=arbndc
087780130412     c                   eval      wpda_ddc=arbddc
087790130412     c                   eval      wpda_dcm=arbdcm
087800130412     C  N10KARB          CHAIN     FNBLP01L                           30
087810130412     c                   endif
087820941213     C*
087830941213     C* INVIO PER SEDE
087840991006     C                   MOVEL     'FIARBT0T'    SFILE
087850941213     C* ALLOCO OGGETTI
087860941213     C                   EXSR      ALLOC
087870070515     c
087880070515     c                   if        *in91
087890070525     C   10              unlock    FNBLP01L
087900070525     C  N10              unlock    FNARB01L
087910070515     C                   GOTO      ENDRG3
087920070515     c                   endif
087930941213     C*
087940991001     C                   MOVEL     *BLANKS       WCKINL            1
087950991001     C                   EXSR      CARWAR
087960151020     c* Mmorizzo il totale imponibilie del video prima degli arrotondamenti
087970151020     c****               if        tassatoimv=0
087980151019     c                   movel     vidimv        Tassatoimv
087990151020     c****               endif
088000991006     C* SOLO SE C'E' NUMERO FATTURA CALCOLO
088010991006     C     VIDDFT        IFGT      *ZEROS
088020000105     C                   MOVEL     O17DFA        ARBDFT
088030991006     C                   EXSR      TOTFAT
088040991006     C     D04BOL        IFGT      *ZEROS
088050991006     C                   ADD       1             C
088060991006     C                   MOVEL     §$2BOL        WSV(C)
088070991006     C                   Z-ADD     D04BOL        WVA(C)
088080991006     C                   ENDIF
088090991006     C     D04IVA        IFGT      *ZEROS
088100991006     C                   ADD       1             C
088110991006     C                   MOVEL     §$2IVA        WSV(C)
088120991006     C                   Z-ADD     D04IVA        WVA(C)
088130991006     C                   ENDIF
088140991006     C                   ENDIF
088150941213     C*
088160941213     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
088170950616    1C     C             IFGT      0
088180941213     C                   SETON                                        09
088190941213     C*
088200941213     C* INVIO PER SEDE
088210941213     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
088220950616    2C     C             IFGT      3
088230991006     C* MI SALVO I CAMPI DELL'FIARBT
088240941213     C                   MOVEL     SOVR(1)       SOARBT
088250941213     C                   MOVEL     SALC(1)       SAARBT
088260941213     C                   MOVEL     SDLC(1)       SDARBT
088270941213     C*
088280991006     C                   MOVEL     'FIARBU0T'    SFILE
088290941213     C* ALLOCO OGGETTI
088300941213     C                   EXSR      ALLOC
088310070515     c
088320070515     c                   if        *in91
088330070525     C   10              unlock    FNBLP01L
088340070525     C  N10              unlock    FNARB01L
088350070515     C                   MOVEL     SDARBT        SDLC(1)
088360070515     c                   exsr      DLCSED
088370070525     C                   SETON                                        2891
088380070515     C                   GOTO      ENDRG3
088390070515     c                   endif
088400941213     C*
088410950616    2C                   ENDIF
088420950616    1C                   ENDIF
088430950114     C*
088440950114     C                   MOVEL     KNMUS         ARBPRU
088450950114     C                   Z-ADD     DATEU         ARBDTV
088460950114     C                   TIME                    ARBORV
088470950114     C                   MOVEL     VIDCVR        ARBCVB
088480991011     C                   MOVEL     '1'           ARBTRC
088490991011     C*
088500991011     C                   MOVEL     '1'           AR6TRC
088510930511     C*
088520060130     C* SE DA STORICIZZARE
088530060130    1C**   *IN10         IFEQ      *ON
088540060130     C**   D66FSA        ANDEQ     'S'
088550060130     C**   *IN10         OREQ      *OFF
088560060130     C**   D66FSP        ANDEQ     'S'
088570060130     C     D66FSA        ifeq      'S'
088580941213     C                   EXSR      STOARB
088590950616    1C                   ENDIF
088600060302     c* storicizzazione motivazione
088610060302     c     d66mtv        ifeq      'S'
088620060302     c                   exsr      sto_mtv
088630060302     c                   endif
088640941213     C**
088650991006     C* INVIO VARIAZIONE --> FIARBU0T
088660941213     C**
088670950616    1C     *IN09         IFEQ      *ON
088680941213     C     C             ANDGT     3
088690941213     C                   Z-ADD     4             C
088700941213     C*
088710941213    2C     D66FFI        IFEQ      'S'
088720950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
088730950616    3C     §3ABSD        IFEQ      ' '
088740941213     C                   Z-ADD     4             C
088750991006     C                   OPEN      FIARBU0T
088760941213     C*
088770941213     C* NE SCRIVO FIN TANTO CHE CE NE SONO
088780950616    4C     WSV(C)        DOWNE     ' '
088790941213     C                   MOVEL     WSV(C)        ARBSVN
088800941213     C                   Z-ADD     WVA(C)        ARBVAN
088810991006     C                   WRITE     FIARBUTT
088820941213     C                   ADD       1             C
088830950616    4C                   ENDDO
088840941213     C*
088850991006     C                   CLOSE     FIARBU0T
088860941213     C* DISALLOCO OGGETTO MEMBRO PER SEDE
088870950616    3C                   ENDIF
088880950208     C                   EXSR      DLCSED
088890950208    2C                   ENDIF
088900941213     C* REIMPOSTO I CAMPI
088910941213     C                   MOVEL     SOARBT        SOVR(1)
088920941213     C                   MOVEL     SAARBT        SALC(1)
088930941213     C                   MOVEL     SDARBT        SDLC(1)
088940941213    1C                   ENDIF
088950930506     C*
088960930506     C                   Z-ADD     VIDKSC        ARBKSC
088970930506     C                   MOVEL     VIDKLP        ARBKSC
088980941209     C                   MOVEL     VIDIAS        ARBIAS
088990941209     C                   MOVEL     VIDVAS        ARBVAS
089000011017      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
089010011017     C     VIDIAS        IFEQ      *ZEROS
089020011017     C                   CLEAR                   ARBVAS
089030011017     C                   ENDIF
089040011017      *
089050941209     C                   MOVEL     VIDVMD        ARBVMD
089060941215     C                   MOVEL     VIDVAD        ARBVAD
089070011017      * Se il valore merce è a 0 non devo scrivere la divisa
089080011017     C     VIDVMD        IFEQ      *ZEROS
089090011017     C                   CLEAR                   ARBVAD
089100011017     C                   ENDIF
089110011017      *
089120941209     C                   MOVEL     VIDQFT        ARBQFT
089130930506     C                   MOVEL     VIDCTR        ARBCTR
089140991006     C                   MOVEL     VIDDIV        ARBDIV
089150941213     C                   MOVEL     VIDPOR        ARBPOR
089160941213     C                   MOVEL     WSV(1)        ARBSV1
089170941213     C                   MOVEL     WVA(1)        ARBVA1
089180941213     C                   MOVEL     WSV(2)        ARBSV2
089190941213     C                   MOVEL     WVA(2)        ARBVA2
089200941213     C                   MOVEL     WSV(3)        ARBSV3
089210941213     C                   MOVEL     WVA(3)        ARBVA3
089220991006     C                   Z-ADD     VIDIMV        ARBIMV
089230991006     C                   MOVEL     O17DFA        ARBDFT
089240950104     C                   MOVEL     VIDNFT        ARBNFT
089250950104     C                   MOVEL     VIDFIV        ARBFIV
089260991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
089270991013     C* STATO MODIFICATO TOLGO LA 'P'
089280991013     C     AR6FIM        IFEQ      'P'
089290991013     C     ARBIMV        ANDNE     AR6IMV
089300991013     C     *IN10         ANDEQ     *ON
089310991013     C                   MOVE      *BLANKS       ARBFIM
089320991013     C                   ELSE
089330991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
089340991013     C                   MOVE      AR6FIM        ARBFIM
089350991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
089360991013     C     WAR6          IFNE      'E'
089370991013     C     *IN10         ANDEQ     *OFF
089380991013     C                   MOVEL     'P'           ARBFIM
089390991013     C                   END
089400991013     C                   ENDIF
089410941209     C*
089420941212     C                   MOVEL     VIDCEI        ARBCEI
089430930506     C*
089440950616    1C     ARBDFT        IFGT      0
089450930505     C*
089460930505     C* SE TOTALE FATTURA NON CORRISPONDE AL PRECEDENTE RISTAMPA FATT
089470941209     C     D04IFT        COMP      AR6IFT                             3737
089480930505     C*
089490941212     C                   MOVE      D04ALI        ARBALI
089500941212     C                   MOVE      D04IFT        ARBIFT
089510941212     C                   MOVE      D04IMV        ARBIMV
089520930514     C*
089530970613    2C                   SELECT
089540941221     C     ARBPOR        WHENGT    0
089550941212     C                   Z-ADD     D04POR        ARBPOR
089560941221     C     ARBVA1        WHENGT    0
089570991007     C     ARBSV1        ANDNE     §$2BOL
089580991007     C     ARBSV1        ANDNE     §$2IVA
089590941213     C                   ADD       D04POR        ARBVA1
089600941209     C     ARBVA2        WHENGT    0
089610991007     C     ARBSV2        ANDNE     §$2BOL
089620991007     C     ARBSV2        ANDNE     §$2IVA
089630941213     C                   ADD       D04POR        ARBVA2
089640941209     C     ARBVA3        WHENGT    0
089650941213     C                   ADD       D04POR        ARBVA3
089660970613    2C                   ENDSL
089670941209     C*
089680950616    1C                   ENDIF
089690930511     C*
089700930511     C                   MOVEL     VIDCVR        ARBCVB
089710911218     C*
089720950616    1C     D66FFI        IFEQ      'S'
089730950227     C*
089740950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
089750950616    2C     §3ABSD        IFEQ      ' '
089760991006     C                   OPEN      FIARBT0T
089770991006     C                   WRITE     FIARBTTT
089780991006     C                   CLOSE     FIARBT0T
089790950616    2C                   END
089800941212     C* DISALLOCO OGGETTO MEMBRO PER SEDE
089810941212     C                   EXSR      DLCSED                                       FNARBT0T
089820950616    1C                   END
089830941213     C**
089840941216     C* A G G I O R N O
089850941213     C**
089860051214     c* Semodificato il cod tariffa, verifico se volume automatico
089870051214     c                   if        not *in10 and vidctr<>sav_arbctr
089880051214     C*
089890051214     C                   MOVEL     '7T'          COD
089900051214     C                   MOVEL(P)  ARBFVF        KEY
089910051214     C                   EXSR      CTABEL
089920051214     C                   MOVEL     TBLUNI        DS7T
089930051214     C     §7TRVL        IFEQ      'A'
089940051214     C     §7TRVL        OREQ      'P'
089950051214     c                   movel     'S'           wricvl
089960051214     c                   seton                                        24
089970051214     c                   endif
089980051214     c                   endif
089990051214     c
090000991006     C                   Z-ADD     ARBKSC        AR6KSC
090010991006     C                   Z-ADD     ARBCTR        AR6CTR
090020941213     C                   MOVE      ARBALI        AR6ALI
090030941213     C                   MOVE      ARBIFT        AR6IFT
090040941213     C                   MOVE      ARBIMV        AR6IMV
090050991006     C                   MOVEL     ARBDIV        AR6DIV
090060941213     C                   MOVE      ARBPOR        AR6POR
090070941213     C                   MOVE      ARBSV1        AR6SV1
090080941213     C                   MOVE      ARBVA1        AR6VA1
090090941213     C                   MOVE      ARBSV2        AR6SV2
090100941213     C                   MOVE      ARBVA2        AR6VA2
090110941213     C                   MOVE      ARBSV3        AR6SV3
090120941213     C                   MOVE      ARBVA3        AR6VA3
090130941213     C                   MOVEL     ARBCEI        AR6CEI
090140950104     C                   MOVEL     ARBDFT        AR6DFT
090150950104     C                   MOVEL     ARBNFT        AR6NFT
090160950104     C                   MOVEL     ARBFIV        AR6FIV
090170991013     C                   MOVEL     ARBFIM        AR6FIM
090180941213     C*
090190991006     C* CANCELLO LE VARIE NELL'FIAR7
090200941221    1C     WAR7          IFEQ      'E'
090210991006     C                   MOVEL     '1'           WTRC
090220991006     C     KARB2         SETLL     FIAR7000
090230991006     C     KARB2         READE     FIAR7000                               30
090240941213     C*
090250941213    2C     *IN30         DOWEQ     *OFF
090260991006     C                   DELETE    FIAR7000
090270991006     C     KARB2         READE     FIAR7000                               30
090280941213    2C                   ENDDO
090290941213    1C                   ENDIF
090300941213     C*
090310941213     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
090320941213    1C     WSV(4)        IFNE      ' '
090330941213     C                   Z-ADD     4             C
090340991006     C                   CLEAR                   FIAR7000
090350941213     C                   MOVEL     ARBAAS        AR7AAS
090360941213     C                   MOVEL     ARBLNP        AR7LNP
090370941213     C                   MOVEL     ARBNRS        AR7NRS
090380941213     C                   MOVEL     ARBNSP        AR7NSP
090390991006     C                   MOVEL     '1'           AR7TRC
090400941213    2C     WSV(C)        DOWNE     ' '
090410941213     C                   MOVEL     WSV(C)        AR7SVN
090420941213     C                   MOVEL     WVA(C)        AR7VAN
090430991006     C                   WRITE     FIAR7000
090440941213     C                   ADD       1             C
090450941213    2C                   ENDDO
090460941213    1C                   ENDIF
090470941213     C**
090480941216     C* SE HO ELIMINATO TUTTA LA TASSAZIONE, PER S.F., DELETO RECORD
090490950616    1C     AR6DFT        IFEQ      0
090500941216     C     AR6SV1        ANDEQ     ' '
090510941216     C     AR6SV2        ANDEQ     ' '
090520941216     C     AR6SV3        ANDEQ     ' '
090530941216     C     AR6POR        ANDEQ     0
090540991116     C     AR6CEI        ANDEQ     ' '
090550941216     C*
090560950616    2C     WAR6          IFEQ      'E'
090570041027     C     *IN10         ifeq      *OFF
090580991006     C                   MOVEL     '1'           WTRC
090590991006     C     KARB2         DELETE    FIAR6000                           30
090600991116     C                   ELSE
090610041027     C* SE variaz, in ARRIVO DEVO UGUALMENTE tenere
090620991116     C* FIAR6000 (SERVE PER LA PROCEDURA DEI DANNI)
090630991116     C                   UPDATE    FIAR6000
090640991116     C                   ENDIF
090650991116   X2C                   ELSE
090660041027     C* SE NON ESISTE RECORD TASSAZIONE scrivo record se var.in arrivo
090670041027     C     *IN10         ifeq      *ON
090680991116     C                   MOVEL     ARBAAS        AR6AAS
090690991116     C                   MOVEL     ARBLNP        AR6LNP
090700991116     C                   MOVEL     ARBNRS        AR6NRS
090710991116     C                   MOVEL     ARBNSP        AR6NSP
090720991116     C                   WRITE     FIAR6000
090730041027    3C                   ENDIF
090740041027    2C                   ENDIF
090750941216     C*
090760941216     C                   ELSE
090770941216     C*
090780950616    2C     WAR6          IFEQ      'E'
090790991006     C                   UPDATE    FIAR6000
090800941209     C                   ELSE
090810941209     C                   MOVEL     ARBAAS        AR6AAS
090820941209     C                   MOVEL     ARBLNP        AR6LNP
090830941209     C                   MOVEL     ARBNRS        AR6NRS
090840941209     C                   MOVEL     ARBNSP        AR6NSP
090850991006     C                   WRITE     FIAR6000
090860950616    2C                   ENDIF
090870950616    1C                   ENDIF
090880941209     C*
090890151016     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
090900151016     c*  per la fatturazione se inserito o variato totale imponibile
090910151016     c                   if        (ar6imv>0 and ar6imv<>savimv_fat) or
090920151016     c                             ar6imv=0
090930151118     c                   exsr      regpesivol
090940151016     c                   endif
090950950113     C*
090960950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
090970950113     C*  SE VOLUME AUTOMATICO O PRESUNTO PER PRECEDENTE VARIAIZONE
090980950113     C*  MITTENTE
090990950616    1C     *IN24         IFEQ      *ON
091000051214     c* Mi salvo i campi che userò per la storicizzazione
091010051214     C                   MOVEL     sav_arbctr    MITCTR
091020051214     C                   MOVEL     ARBFVF        MITFVF
091030051214     C                   MOVEL     ARBVLF        MITVLF
091040051214     c
091050950113     C                   EXSR      CALVOL
091060070907     C**                 MOVEL     'I2'          D48CVB
091070070907     C**                 MOVEL     'E'           WESEG
091080070907     c                   add       1             kk
091090070907     c                   movel     'I2'          kcvb(kk)
091100070911     c                   movel     'V'           kffr(kk)
091110070911     c                   movel     ' '           kf12(kk)
091120070911     c****               clear                   w48f12
091130950114     C* AGGIORNO GIA' IL VOLUME TANTO PASSO DALLA VIDEATA DEL VOLUME
091140950114     C*  COMUNQUE PER LA TRASMISSIONE
091150950114     C                   MOVEL     D18FVF        ARBFVF
091160950114     C                   MOVEL     D18VLF        ARBVLF
091170070911     C*****              MOVEL     'V'           D48FFR
091180950616     C                   END
091190911211     C*
091200950114     C   10              UPDATE    FNARB000
091210950114     C  N10              UPDATE    FNBLP000
091220950114     C*
091230041027  1b c***                if        wstessogas = 'S' and (d66ffi = 'E' or
091240041027     c***                          d66ffi = 'P') and *in10 = *off
091250041027  2b c***                if        sav_vidias <> vidias or sav_vidvas <> vidvas
091260041027     c***                          or
091270041027     c***                          sav_vidvmd <> vidvmd or sav_vidvad <> vidvad
091280041027     c***                          or
091290041027     c***                          sav_vidqft <> vidqft
091300041027     c***                exsr      Agg_ARB
091310041027  2e c***                endif
091320041027  1e c***                endif
091330041027     c
091340041027     c* Aggiorno altra bolla se c'e' e variati i campi sopra
091350041027     c                   if        wsiaggio='S' and waltrabolla=' '
091360041027     c* non aggiono mai il ksc:
091370041027     c* in  partenza perchè il S.F. va in ar6
091380041027     c*  in arrivo perchè il cod mittente se cambiato viene impostato
091390041027     c*  in regis8
091400041027     C   10              except    arbTblp
091410041027     C  N10              except    arbTarb
091420041027     c                   endif
091430130411
091440130411     c* Richiamo routine per invio variazione a pda
091450130411     c                   exsr      sr_pda
091460911212     C     ENDRG3        ENDSR
091470911211     C*
091480970613     C* SE BOLLA SU DISTINTA CHIUSA NON FACCIO NULLA ALTRIMENTI ------*
091490970613     C*  CHIUDO LA BOLLA
091500970613     C     CHIUBO        BEGSR
091510970613     C                   MOVEL     'S'           WAGG              1
091520970613     C*
091530970613     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
091540970613     C*  UN NUMERO DISTINTA ED E' CHIUSA
091550080516     c                   clear                   fvvfcf
091560970613     C     ARBNDC        IFGT      0
091570020911     c                   Z-Add     ArbIfp        Kfgs
091580970613     C                   Z-ADD     ARBNDC        W0050             5 0
091590020911     C     KFVV          CHAIN     FNFVV01L                           30
091600970613     C     *IN30         IFEQ      *OFF
091610970613     C     FVVFCF        ANDEQ     'S'
091620970613     C                   CLEAR                   WAGG
091630970613     C                   ENDIF
091640970613     C                   ENDIF
091650970613     C*
091660970613    4C     WAGG          IFEQ      'S'
091670970613    5C     ARBNMI        IFEQ      0
091680970613     C                   Z-ADD     8888888       ARBNMI
091690970613     C                   Z-ADD     8888888       ARBNDC
091700970613     C                   Z-ADD     DATEU         ARBDDC
091710080516     c                   else
091720080516     c
091730080516     c                   if        arbndc>0 and fvvfcf<>'S'
091740080516     C                   Z-ADD     8888888       ARBNDC
091750080516     c                   endif
091760080516     c
091770970613    5C                   ENDIF
091780970613    4C                   ENDIF
091790970613     C                   ENDSR
091800970613     C*
091810911211     C*--- CONTROLLI FORMATO4 ----------------------------------------*
091820911211     C     CONTR4        BEGSR
091830040123     C**!!!              MOVEL     ' '           CEISV3
091840920319     C                   EXSR      CTRCOM
091850920319     C   90              GOTO      ENDCT4
091860920319     C*
091870911211     C* T A S S A Z I O N E
091880991011     C     *INKN         IFEQ      *ON
091890991011     c* fattura immediata
091900991011     C     *IN14         OREQ      *ON
091910991011     C                   MOVEL     '2'           WBOL              1
091920991011     C                   EXSR      TASSAZ
091930991011     C   KN
091940991011     COR 90              GOTO      ENDCT4
091950991011     C                   END
091960911211     C*
091970990930     C* OBBLIGATORIO IMPORTO DELLA VARIA
091980040123     C**!!!VI2VA1        IFEQ      0
091990040123     C**!!!              MOVEL     MSG(51)       VIDMSG
092000040123     C**!!!              SETON                                        519028
092010040123     C**!!!              GOTO      ENDCT4
092020040123     C**!!!              END
092030991007     C* Se divisa sprotetta significa che manca tariffa: in questo caso
092040991007     C* controllo che la divisa inserita sia = alla divisa della moneta
092050991007     C*  di conto SE CLIENTE  9999
092060991007     C     *IN14         IFEQ      *ON
092070991007     C     *IN07         ANDEQ     *OFF
092080991007     C     VI2DIV        ANDNE     §GEDCN
092090991007     C     VIDKSC        ANDEQ     9999
092100991007     C                   SETON                                        459028
092110991007     C                   MOVEA     MSG(90)       K78
092120991007     C                   MOVEA     §GEDCN        K78(71)
092130991007     C                   MOVEA     K78           VIDMSG
092140991007     C                   GOTO      ENDCT4
092150991007     C                   ENDIF
092160991006     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
092170100120     C                   CLEAR                   dlv1601
092180100120     C                   CLEAR                   Fnlv16DS
092190991006     C                   CLEAR                   DTASV
092200991006     C                   MOVEL     'A'           D17TBO
092210991006     C                   MOVEL     VIDKLP        D17KSC
092220991006     C                   MOVE      VIDKSC        D17KSC
092230991012     C     DFT2BO        IFGT      0
092240991012     C                   Z-ADD     DFT2BO        D17DSP
092250991012     C                   ELSE
092260991006     C                   Z-ADD     ARBDSP        D17DSP
092270991012     C                   ENDIF
092280991006     C                   MOVEL     ARBLNP        D17LNP
092290991006     C                   MOVEL     ARBLNA        D17LNA
092300100120     C                   MOVEL     §3ATB2        D17TBL
092310991006     C                   MOVEL     §3ARBL        D17RBL
092320020503     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
092330040123     C                   MOVEL     §3ASVA        §d17sva
092340040123      * passo che devo controllare la seconda bolla
092350040123     c                   Eval      §d17Tb2 = 'S'
092360100120
092370100120     c* Se si tratta di ffattura immediata , passo cod iso ddestintario
092380100120    1c                   if        *in14
092390100120     c                   Eval      §d17ctriso='S'
092400100120    2c                   if        arbcpi<>*blanks
092410100120     C     'PRIVATO'     SCAN      arbcpi        posiz                    30
092420100120     c* solo numeri
092430100120     c                   select
092440100120    3c                   when      not *in30 and %subst(arbcpi:1:2)>=*zeros
092450100120     c                   eval      §d17iso='IT'
092460100120     c* "PRIVATO" senza iso
092470100120     c                   when      *in30 and posiz<=3
092480100120     c                   eval      §d17iso='$$'
092490100120     c                   other
092500100120     c                   eval      §d17iso= %subst(arbcpi:1:2)
092510100120    3c                   ENDSL
092520100120    2c                   endif
092530100120    1c                   endif
092540100120     c
092550040123     c                   Movel     dlv1601       D17Flo
092560100120     c
092570991006     C                   MOVEL     VI2DIV        D17DIV
092580991006     C                   MOVEL     WDIV          D17DVP
092590991006    1C     §3AFCA        IFEQ      2
092600991006     C                   MOVEL     'S'           D17FCA
092610991006    1C                   ENDIF
092620991006     C                   MOVEL     'E'           D17FIM
092630991006     C                   MOVEL     VI2CEI        D17CEI
092640991006     C                   MOVEL     VI2SV1        VSV(1)
092650040123     C**!!!              MOVEL     VI2VA1        VVA(1)
092660040123     c                   Z-add     Vi2Va1        vva(1)
092670040123     c                   Movel     Vi2Es1        ves(1)
092680040123     c                   Movel     Vi2Sv2        vsv(2)
092690040123     c                   Movel     Vi2Es2        ves(2)
092700040123     c                   Z-add     Vi2Va2        vva(2)
092710040123     c                   MOVEL     Vi2Sv3        vsv(3)
092720040123     c                   Z-add     Vi2Va3        vva(3)
092730040123     c                   Movel     Vi2Es3        ves(3)
092740040123     c                   MOVEL     Vi2Sv4        vsv(4)
092750040123     c                   Z-add     Vi2Va4        vva(4)
092760040123     c                   Movel     Vi2Es4        ves(4)
092770041130     c                   MOVEL     Vi2Sv5        vsv(5)
092780041130     c                   Z-add     Vi2Va5        vva(5)
092790041130     c                   Movel     Vi2Es5        ves(5)
092800041130     c                   MOVEL     Vi2Sv6        vsv(6)
092810041130     c                   Z-add     Vi2Va6        vva(6)
092820041130     c                   Movel     Vi2Es6        ves(6)
092830991006     C                   CALL      'FNLV16R'
092840030612     C                   PARM                    Fnlv16DS
092850991006     C                   PARM                    DTASV
092860991011     C                   MOVEL     D17DIV        VI2DIV
092870991006     C                   MOVEL     D17CEI        VI2CEI
092880991011     C                   MOVEL     O17DEI        DESCEI
092890040123     C**!!!              MOVEL     VSV(1)        VI2SV1
092900040123     C**!!!              MOVEL     VES(1)        CEISV3            1
092910040123      * Ricarico le varie
092920040123     c                   Movel     Vsv(1)        Vi2Sv1
092930040123     c                   Z-add     Vva(1)        Vi2Va1
092940040123     c                   Movel     Ves(1)        Vi2Es1
092950040123     c                   Movel     Vsv(2)        Vi2Sv2
092960040123     c                   Z-add     Vva(2)        Vi2Va2
092970040123     c                   Movel     Ves(2)        Vi2Es2
092980040123     c                   Movel     Vsv(3)        Vi2Sv3
092990040123     c                   Z-add     Vva(3)        Vi2Va3
093000040123     c                   Movel     Ves(3)        Vi2Es3
093010040123     c                   Movel     Vsv(4)        Vi2Sv4
093020040123     c                   Z-add     Vva(4)        Vi2Va4
093030040123     c                   Movel     Ves(4)        Vi2Es4
093040041130     c                   Movel     Vsv(5)        Vi2Sv5
093050041130     c                   Z-add     Vva(5)        Vi2Va5
093060041130     c                   Movel     Ves(5)        Vi2Es5
093070041130     c                   Movel     Vsv(6)        Vi2Sv6
093080041130     c                   Z-add     Vva(6)        Vi2Va6
093090041130     c                   Movel     Ves(6)        Vi2Es6
093100040123      * Controllo se caricata almeno una varia
093110041130do  1c                   Do        6             c
093120040123     c                   If        vsv(c) <> *Blanks
093130040123     c                   Eval      wesvar1 = 'S'
093140040123     c                   EndIf
093150040123    1c                   EndDo
093160040123
093170991006     C     O17ERR        IFNE      *BLANKS
093180991006     C                   SETON                                        90
093190991006     C     O17MSG        IFNE      *BLANKS
093200991006     C                   MOVEL     O17MSG        VIDMSG
093210991006     C                   SETON                                        28
093220991006     C                   ENDIF
093230991006     C     O17ERR        IFEQ      '6'
093240040123     c                   Select
093250040123     c                   When      O17Erv = 1
093260991006     C                   SETON                                        49
093270040123     c                   When      O17Erv = 2
093280040123     C                   SETON                                        63
093290040123     c                   When      O17Erv = 3
093300040123     C                   SETON                                        64
093310040123     c                   When      O17Erv = 4
093320040123     C                   SETON                                        65
093330041130     c                   When      O17Erv = 5
093340041130     C                   SETON                                        66
093350041130     c                   When      O17Erv = 6
093360041130     C                   SETON                                        67
093370040123     c                   EndSl
093380991006     C                   ENDIF
093390991006     C     O17ERR        IFEQ      '1'
093400991006     C                   SETON                                        50
093410991006     C                   ENDIF
093420991006     C     O17ERR        IFEQ      '7'
093430991006     C                   SETON                                        45
093440991006     C                   ENDIF
093450991006     C   90              GOTO      ENDCT4
093460991006     C                   ENDIF
093470991006     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
093480991006    1C  N07VI2DIV        IFNE      WDIV
093490991006     C* CONVERSIONE VARIA
093500991006    3C     VI2VA1        IFEQ      VA2VA1
093510991006     C     VI2VA1        ANDGT     *ZEROS
093520991006     C     WDIV          ANDNE     *BLANKS
093530030612     C                   CLEAR                   YeurcoDS
093540991006     C                   MOVEL     WDIV          YECDVI
093550991006     C                   Z-ADD     VI2VA1        YECIMI
093560991006     C                   MOVEL     VI2DIV        YECDVO
093570991006     C     O17FDC        IFEQ      'S'
093580011114     C                   MOVE      '3'           YECDCO
093590991006     C                   ELSE
093600991006     C                   CLEAR                   YECDCO
093610991006     C                   ENDIF
093620991006     C                   CALL      'YEURCO'
093630030612     C                   PARM                    YeurcoDS
093640991006     C                   Z-ADD     YECIMO        VI2VA1
093650991012     C                   SETON                                        51
093660991006    3C                   ENDIF
093670040123      * Conversione Varia 2
093680040123    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
093690040123     c                             wdiv <> *Blanks
093700040123     c                   Clear                   Yeurcods
093710040123     c                   Movel     wdiv          YECDVI
093720040123     c                   Z-add     Vi2Va2        YECIMI
093730040123     c                   Movel     Vi2Div        YECDVO
093740040123     c                   If        O17Fdc = 'S'
093750040123     c                   Movel     '3'           YECDCO
093760040123     c                   EndIf
093770040123     c                   Call      'YEURCO'
093780040123     c                   Parm                    Yeurcods
093790040123     c                   Z-add     YECIMO        Vi2Va2
093800040123     c                   Eval      *In63 = *On
093810040123    2c                   EndIf
093820040123      * Conversione Varia 3
093830040123    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
093840040123     c                             wdiv <> *Blanks
093850040123     c                   Clear                   yeurcods
093860040123     c                   Movel     wdiv          YECDVI
093870040123     c                   Z-add     Vi2Va3        YECIMI
093880040123     c                   Movel     Vi2Div        YECDVO
093890040123     c                   If        O17Fdc = 'S'
093900040123     c                   Movel     '3'           YECDCO
093910040123     c                   EndIf
093920040123     c                   Call      'YEURCO'
093930040123     c                   Parm                    yeurcods
093940040123     c                   Z-add     YECIMO        Vi2Va3
093950040123     c                   Eval      *In64 = *On
093960040123    2c                   EndIf
093970040123      * Conversione Varia 4
093980040123    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
093990040123     c                             wdiv <> *Blanks
094000040123     c                   Clear                   yeurcods
094010040123     c                   Movel     wdiv          YECDVI
094020040123     c                   Z-add     Vi2Va4        YECIMI
094030040123     c                   Movel     Vi2Div        YECDVO
094040040123     c                   If        O17Fdc = 'S'
094050040123     c                   Movel     '3'           YECDCO
094060040123     c                   EndIf
094070040123     c                   Call      'YEURCO'
094080040123     c                   Parm                    yeurcods
094090040123     c                   Z-add     YECIMO        Vi2Va4
094100040123     c                   Eval      *In65 = *On
094110040123    2c                   EndIf
094120041130      * Conversione Varia 5
094130041130    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
094140041130     c                             wdiv <> *Blanks
094150041130     c                   Clear                   yeurcods
094160041130     c                   Movel     wdiv          YECDVI
094170041130     c                   Z-add     Vi2Va5        YECIMI
094180041130     c                   Movel     Vi2Div        YECDVO
094190041130     c                   If        O17Fdc = 'S'
094200041130     c                   Movel     '3'           YECDCO
094210041130     c                   EndIf
094220041130     c                   Call      'YEURCO'
094230041130     c                   Parm                    yeurcods
094240041130     c                   Z-add     YECIMO        Vi2Va5
094250041130     c                   Eval      *In66 = *On
094260041130    2c                   EndIf
094270041130      * Conversione Varia 6
094280041130    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
094290041130     c                             wdiv <> *Blanks
094300041130     c                   Clear                   yeurcods
094310041130     c                   Movel     wdiv          YECDVI
094320041130     c                   Z-add     Vi2Va6        YECIMI
094330041130     c                   Movel     Vi2Div        YECDVO
094340041130     c                   If        O17Fdc = 'S'
094350041130     c                   Movel     '3'           YECDCO
094360041130     c                   EndIf
094370041130     c                   Call      'YEURCO'
094380041130     c                   Parm                    yeurcods
094390041130     c                   Z-add     YECIMO        Vi2Va6
094400041130     c                   Eval      *In67 = *On
094410041130    2c                   EndIf
094420040123     C**!!!              Z-ADD     VI2VA1        VA2VA1
094430040123     C**!!!              Z-ADD     VI2VA1        VVA(1)
094440040123     C**!!!              MOVEL     VI2DIV        WDIV
094450991012     C* RIEMETTO COMUNQUE LA VIDEATA SE HANNO VARIATO LA DIVISA
094460991012     C* (POTREI RIEMETTERLA SOLO SE HO CONVERTITO IMPORTI MA LA
094470991012     C* RIEMETTO COMUNQUE PER FARE COME PER LA PRIMA BOLLA)
094480991012     C                   SETON                                        90
094490040123
094500040123     c                   Z-add     Vi2Va1        Va2Va1
094510040123     c                   Z-add     Vi2Va2        Va2Va2
094520040123     c                   Z-add     Vi2Va3        Va2Va3
094530040123     c                   Z-add     Vi2Va4        Va2Va4
094540041130     c                   Z-add     Vi2Va5        Va2Va5
094550041130     c                   Z-add     Vi2Va6        Va2Va6
094560040123     c                   Z-add     Vi2Va1        Vva(1)
094570040123     c                   Z-add     Vi2Va2        Vva(2)
094580040123     c                   Z-add     Vi2Va3        Vva(3)
094590040123     c                   Z-add     Vi2Va4        Vva(4)
094600041130     c                   Z-add     Vi2Va5        Vva(5)
094610041130     c                   Z-add     Vi2Va6        Vva(6)
094620040123     c                   Movel     Vi2Div        Wdiv
094630040123
094640991012     C                   GOTO      ENDCT4
094650991006   X1C                   ELSE
094660991006     C                   Z-ADD     VI2VA1        VA2VA1
094670040123     c                   Z-add     Vi2Va2        Va2Va2
094680040123     c                   Z-add     Vi2Va3        Va2Va3
094690040123     c                   Z-add     Vi2Va4        Va2Va4
094700041130     c                   Z-add     Vi2Va5        Va2Va5
094710041130     c                   Z-add     Vi2Va6        Va2Va6
094720991006    1C                   ENDIF
094730011206      *
094740011206      * Controllo che la somma degli importi immessi non superi il valore
094750011206      * impostato in §GEIMF
094760040123     c**!!!              eval      w0173 = vi2va1
094770040123     c                   xfoot     vva           w0173
094780011206      *      controllo se devo convertire il totale imponibile
094790011206  1b c                   if        vi2div <> §gedcn
094800030612     c                   clear                   YeurcoDS
094810011206     c                   eval      yecdvi = vi2div
094820011206     c                   eval      yecdvo = §gedcn
094830040123     c**!!!              eval      yecimi = vi2va1
094840040123     c                   eval      yecimi = w0173
094850011206     c                   move      decsav        yecdco
094860011206     c                   call      'YEURCO'
094870030612     c                   parm                    YeurcoDS
094880011206  2b c                   if        yecesi = ' '
094890011206     c                   eval      w0173 = yecimo
094900011206  2e c                   endif
094910011206  1e c                   endif
094920011206      *      controllo se l'imponibile supera il limite fatturabile
094930011220  1b c                   if        w0173 > imfsav
094940011206     c                   eval      vidmsg = msg(110)
094950011220     c                   eval      %subst (vidmsg:45:14) = (%editc(imfsav: '4'))
094960011206     c                   eval      %subst (vidmsg:60:3) = §gedcn
094970040123     c**!!!              eval      *in51 = *on
094980011206     c                   eval      *in90 = *on
094990011206     c                   goto      endct4
095000011206  1e c                   endif
095010040123
095020040123     c                   Z-add     w0173         TotImv
095030920319     C*
095040911211     C     ENDCT4        ENDSR
095050911212     C*
095060911212     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE 2X -----------------*
095070911212     C     REGIS4        BEGSR
095080040130     c                   Setoff                                       3709
095090130412     c                   clear                   wpda_ndc
095100130412     c                   clear                   wpda_ddc
095110130412     c                   clear                   wpda_dcm
095120040130
095130950104     C*  GIRO UDATE
095140950104     C                   TIME                    W0140            14 0
095150950104     C* UDATE IN GGMMAAAA
095160950104     C                   MOVE      W0140         WDTGIO            8 0
095170950104     C*
095180950104     C* UDATE IN AAAAMMGG
095190950104     C                   Z-ADD     WDTGIO        G02DAT
095200950104     C                   MOVEL     *BLANK        G02ERR
095210950104     C                   CALL      'XSRDA8'
095220950104     C                   PARM                    WLBDAT
095230950104     C                   MOVEL     G02INV        DATEU             8 0
095240941221     C* PRENDO I VALORI ASSOLUTI
095250941221     C                   MLLZO     1             VIDKLP
095260941221     C                   MLLZO     1             VIDKSC
095270941221     C                   MLLZO     1             VI2VA1
095280040123     C                   MLLZO     1             VI2VA2
095290040123     C                   MLLZO     1             VI2VA3
095300040123     C                   MLLZO     1             VI2VA4
095310041130     C                   MLLZO     1             VI2VA5
095320041130     C                   MLLZO     1             VI2VA6
095330941221     C*
095340950203     C                   MOVEL     KNMUS         ARBPRU
095350920120     C* INVIO PER SEDE
095360991007     C                   MOVEL     'FIARBT0T'    SFILE             8
095370930210     C* ALLOCO OGGETTI
095380040123     C                   EXSR      ALLOC
095390930210     C   91              GOTO      ENDRG4
095400040123      * carico le varie in una sk di comodo
095410040123     c                   ExSr      CarWar1
095420040123      * se c'è n.fattura calcolo
095430040129If  1c                   If        Dft2bo > *Zeros
095440040129     c                   Movel     Dft2bo        ArbDft
095450040123     C                   MOVEL     VI2DIV        ARBDIV
095460040129     C                   MOVEL     VI2DIV        divgei
095470040123     C                   EXSR      RIEGEI
095480040123     C                   CLEAR                   Fnlv04ds
095490040123     C                   MOVEL     'T'           D04FIM
095500040123     C                   Z-ADD     ArbDft        D04DFT
095510040123     C                   MOVEL     §GEAED        D04AED
095520040123     C                   Z-ADD     §GEAVL        D04VAR
095530040123     c                   Z-add     TotImv        D04Imv
095540040123     C                   MOVEL     VI2CEI        D04CEI
095550040123     C                   MOVEL     AR9TIC        D04TIC
095560040123     C                   MOVEL     AR9VCA        D04VCA
095570040123     C                   Z-ADD     AR9CAS        D04CAS
095580040123     C                   MOVEL     '2'           D04SPE
095590040123     C                   MOVEL     §3ATB2        D04TBL
095600040123     C                   MOVEL     VI2DIV        D04DIV
095610040123     C                   MOVEL     '1'           UNO
095620040123     C*
095630040123     C                   CALL      'FNLV04R'
095640040123     C                   PARM                    Fnlv04ds
095650040123     C                   PARM                    DTASV
095660040123     C*
095670040123     C     D04BOL        IFGT      *ZEROS
095680040123     C                   ADD       1             C
095690040123     C                   MOVEL     §$2BOL        WSV(C)
095700040123     C                   Z-ADD     D04BOL        WVA(C)
095710040123     C                   ENDIF
095720040123     C     D04IVA        IFGT      *ZEROS
095730040123     C                   ADD       1             C
095740040123     C                   MOVEL     §$2IVA        WSV(C)
095750040123     C                   Z-ADD     D04IVA        WVA(C)
095760040123     C                   ENDIF
095770040123    1c                   EndIf
095780040123
095790040123     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
095800040123    1C     C             IFGT      0
095810040123     C                   SETON                                        09
095820040123     C*
095830040123     C* INVIO PER SEDE
095840040123     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
095850040123    2C     C             IFGT      3
095860040123     C* MI SALVO I CAMPI DELL'FIARBT
095870040123     C                   MOVEL     SOVR(1)       SOARBT
095880040123     C                   MOVEL     SALC(1)       SAARBT
095890040123     C                   MOVEL     SDLC(1)       SDARBT
095900040123     C*
095910040123     C                   MOVEL     'FIARBU0T'    SFILE
095920040123     C* ALLOCO OGGETTI
095930040123     C                   EXSR      ALLOC
095940070515     c                   if        *in91
095950070515     C                   MOVEL     SDARBT        SDLC(1)
095960070515     c                   exsr      DLCSED
095970070525     C                   SETON                                        2891
095980070515     C                   GOTO      ENDRG4
095990070515     c                   endif
096000070515     c
096010040123    2C                   ENDIF
096020040123    1C                   ENDIF
096030911212     C*
096040991007     C                   Z-ADD     DATEU         ARBDTV
096050991007     C                   TIME                    ARBORV
096060991007     C                   MOVEL     VIDCVR        ARBCVB
096070991011     C                   MOVEL     '2'           ARBTRC
096080991011     C                   MOVEL     '2'           AR6TRC
096090991007     C                   CLEAR                   ARBFST
096100991007     C                   CLEAR                   ARBDDS
096110991007     C                   CLEAR                   ARBQFT
096120991007     C                   CLEAR                   ARBVMD
096130991007     C                   CLEAR                   ARBVAD
096140991007     C                   CLEAR                   ARBIAS
096150991007     C                   CLEAR                   ARBVAS
096160911212     C*
096170941212     C* SE DA STORICIZZARE --> STORICIZZO
096180991007     C     WAR62         IFEQ      'E'
096190060130    1C     D66FSA        ANDEQ     'S'
096200060130     C**   *IN10         IFEQ      *ON
096210060130    1C**   D66FSA        ANDEQ     'S'
096220060130     C**   *IN10         OREQ      *OFF
096230060130    1C**   D66FSP        ANDEQ     'S'
096240991007     C                   EXSR      STOARB
096250941212     C                   END
096260060130     C**                 END
096270060302     c* storicizzazione motivazione
096280060302     c     d66mtv        ifeq      'S'
096290060302     c                   exsr      sto_mtv
096300060302     c                   endif
096310040123     C**
096320040123     C* INVIO VARIAZIONE --> FIARBU0T
096330040123     C**
096340040123    1C     *IN09         IFEQ      *ON
096350040123     C     C             ANDGT     3
096360040123     C                   Z-ADD     4             C
096370040123     C*
096380040123    3C     D66FFI        IFEQ      'S'
096390040123     C                   Z-ADD     4             C
096400040123     C                   OPEN      FIARBU0T
096410040123     C*
096420040123     C* NE SCRIVO FIN TANTO CHE CE NE SONO
096430040123    4C     WSV(C)        DOWNE     ' '
096440040123     C                   MOVEL     WSV(C)        ARBSVN
096450040123     C                   Z-ADD     WVA(C)        ARBVAN
096460040123     C                   WRITE     FIARBUTT
096470040123     C                   ADD       1             C
096480040123    4C                   ENDDO
096490040123     C*
096500040123     C                   CLOSE     FIARBU0T
096510040123     C* DISALLOCO OGGETTO MEMBRO PER SEDE
096520040123     C                   EXSR      DLCSED                                       FIARBU0T
096530040123    3C                   ENDIF
096540040123     C* REIMPOSTO I CAMPI
096550040123     C                   MOVEL     SOARBT        SOVR(1)
096560040123     C                   MOVEL     SAARBT        SALC(1)
096570040123     C                   MOVEL     SDARBT        SDLC(1)
096580040123    1C                   ENDIF
096590991011     C*
096600991011     C                   MOVEL     VIDKLP        ARBKSC
096610991007     C                   MOVE      VIDKSC        ARBKSC
096620991007     C                   CLEAR                   ARBPOR
096630991007     C                   MOVEL     VI2DIV        ARBDIV
096640040123     C**!!!              MOVEL     VI2SV1        ARBSV1
096650040123     C**!!!              MOVEL     VI2VA1        ARBVA1
096660040123     C                   MOVEL     WSV(1)        ARBSV1
096670040123     C                   Z-ADD     WVA(1)        ARBVA1
096680040123     C                   MOVEL     WSV(2)        ARBSV2
096690040123     C                   Z-ADD     WVA(2)        ARBVA2
096700040123     C                   MOVEL     WSV(3)        ARBSV3
096710040123     C                   Z-ADD     WVA(3)        ARBVA3
096720991007     C                   MOVEL     VI2CEI        ARBCEI
096730991007     C                   MOVEL     VIDCTR        ARBCTR
096740040123     C**!!!              Z-ADD     VI2VA1        ARBIMV
096750040123     C                   Z-ADD     TotImv        ARBIMV
096760040123     C**!!!              CLEAR                   ARBSV2
096770040123     C**!!!              CLEAR                   ARBVA2
096780040123     C**!!!              CLEAR                   ARBSV3
096790040123     C**!!!              CLEAR                   ARBVA3
096800040129     C**!!!              CLEAR                   ARBALI
096810040129     C**!!!              CLEAR                   ARBIFT
096820991007     C                   Z-ADD     VI2NFT        ARBNFT
096830991007     C                   Z-ADD     VI2FIV        ARBFIV
096840991007     C                   Z-ADD     AR6DFT        ARBDFT
096850991013     C* SE TOTALE IMPONIBILE ERA IMMESSO DALLA PARTENZA ED E'
096860991013     C* STATO MODIFICATO TOLGO LA 'P'
096870991013     C     AR6FIM        IFEQ      'P'
096880991013     C     ARBIMV        ANDNE     AR6IMV
096890991013     C     *IN10         ANDEQ     *ON
096900991013     C                   MOVE      *BLANKS       ARBFIM
096910991013     C                   ELSE
096920991013     C* ALTRIMENTI CI METTO QUELLO CHE C'ERA
096930991013     C                   MOVE      AR6FIM        ARBFIM
096940991013     C* SE NON ESISTE FIAR6 E SONO IN PARTENZA IMPOSTO 'P'
096950991013     C     WAR62         IFNE      'E'
096960991013     C     *IN10         ANDEQ     *OFF
096970991013     C                   MOVEL     'P'           ARBFIM
096980991013     C                   END
096990991013     C                   ENDIF
097000040129
097010040130if  1c                   If        Arbdft > 0
097020040129     C* SE CAMBIATO TOTALE FATTURA RISTAMPO IN AUTOMATICO LA BOLLA
097030040129     C     D04IFT        COMP      AR6IFT                             3737
097040040129
097050040129     C                   MOVE      D04ALI        ARBALI
097060040129     C                   Z-ADD     D04IFT        ARBIFT
097070040130     C                   z-add     D04IMV        ARBIMV
097080051111     c                   select
097090051111     C     ARBVA1        WHENGT    0
097100051111     C     ARBSV1        ANDNE     §$2BOL
097110051111     C     ARBSV1        ANDNE     §$2IVA
097120051111     C                   ADD       D04POR        ARBVA1
097130051111     C     ARBVA2        WHENGT    0
097140051111     C     ARBSV2        ANDNE     §$2BOL
097150051111     C     ARBSV2        ANDNE     §$2IVA
097160051111     C                   ADD       D04POR        ARBVA2
097170051111     C     ARBVA3        WHENGT    0
097180051111     C                   ADD       D04POR        ARBVA3
097190051111    3C                   ENDSL
097200040130     c                   EndIf
097210911212     C*
097220070503     C     D66FFI        IFEQ      'S'
097230991007     C                   OPEN      FIARBT0T
097240991007     C                   WRITE     FIARBTTT
097250991007     C                   CLOSE     FIARBT0T
097260911212     C* DISALLOCO OGGETTO
097270991007     C                   EXSR      DLCSED                                       FIARBT0T
097280911212     C                   END
097290911212     C*
097300911212     C* AGGIORNO
097310991013     C                   Z-ADD     ARBKSC        AR6KSC
097320991013     C                   Z-ADD     ARBCTR        AR6CTR
097330991013     C                   MOVEL     ARBDIV        AR6DIV
097340991013     C                   Z-ADD     ARBPOR        AR6POR
097350991013     C                   MOVEL     ARBSV1        AR6SV1
097360991013     C                   MOVEL     ARBSV2        AR6SV2
097370991013     C                   MOVEL     ARBSV3        AR6SV3
097380991013     C                   Z-ADD     ARBVA1        AR6VA1
097390991013     C                   Z-ADD     ARBVA2        AR6VA2
097400991013     C                   Z-ADD     ARBVA3        AR6VA3
097410991013     C                   Z-ADD     ARBIMV        AR6IMV
097420991013     C                   Z-ADD     ARBALI        AR6ALI
097430991013     C                   MOVEL     ARBCEI        AR6CEI
097440991013     C                   Z-ADD     ARBIFT        AR6IFT
097450991013     C                   Z-ADD     ARBFIV        AR6FIV
097460991013     C                   Z-ADD     ARBNFT        AR6NFT
097470991013     C                   Z-ADD     ARBDFT        AR6DFT
097480991013     C                   MOVEL     ARBFIM        AR6FIM
097490040123     C*
097500040123     C* CANCELLO LE VARIE NELL'FIAR7
097510040123    1C     WAR72         IFEQ      'E'
097520040123     C                   MOVEL     '2'           WTRC
097530040123     C     KARB2         SETLL     FIAR7000
097540040123     C     KARB2         READE     FIAR7000                               30
097550040123     C*
097560040123    2C     *IN30         DOWEQ     *OFF
097570040123     C                   DELETE    FIAR7000
097580040123     C     KARB2         READE     FIAR7000                               30
097590040123    2C                   ENDDO
097600040123    1C                   ENDIF
097610040123     C*
097620040123     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
097630040123    1C     WSV(4)        IFNE      ' '
097640040123     C                   Z-ADD     4             C
097650040123     C                   CLEAR                   FIAR7000
097660040123     C                   MOVEL     ARBAAS        AR7AAS
097670040123     C                   MOVEL     ARBLNP        AR7LNP
097680040123     C                   MOVEL     ARBNRS        AR7NRS
097690040123     C                   MOVEL     ARBNSP        AR7NSP
097700040123     C                   MOVEL     '2'           AR7TRC
097710040123    2C     WSV(C)        DOWNE     ' '
097720040123     C                   MOVEL     WSV(C)        AR7SVN
097730040123     C                   MOVEL     WVA(C)        AR7VAN
097740040123     C                   WRITE     FIAR7000
097750040123     C                   ADD       1             C
097760040123    2C                   ENDDO
097770040123    1C                   ENDIF
097780040123
097790990930     C     WAR62         IFEQ      'E'
097800991007     C                   UPDATE    FIAR6000
097810941212     C                   ELSE
097820991013     C                   CLEAR                   AR6ATB
097830991013     C                   Z-ADD     ARBAAS        AR6AAS
097840991013     C                   Z-ADD     ARBLNP        AR6LNP
097850991013     C                   Z-ADD     ARBNRS        AR6NRS
097860991013     C                   Z-ADD     ARBNSP        AR6NSP
097870991013     C                   MOVEL     '2'           AR6TRC
097880991007     C                   WRITE     FIAR6000
097890941212     C                   ENDIF
097900930211     C*
097910991013     C                   UNLOCK    FNARB01L
097920920204     C* RISTAMPO FATTURA
097930070309     c* ristampo fattura immediata se variata(37 on),
097940070309     c*   e se non attiva GEO
097950130321     C***  *IN37         IFEQ      *ON
097960130321     c***  FGSddaSI      andeq     ' '
097970130321     C***                CLEAR                   FNLSB5DS
097980130321     C***                MOVEL     'V'           DB0RIS
097990130321     C***                MOVEL     D48TBO        DB0TBO
098000130321     C***                MOVEL     ARBAAS        DB0AAS
098010130321     C***                MOVEL     ARBLNP        DB0LNP
098020130321     C***                MOVEL     ARBNRS        DB0NRS
098030130321     C***                MOVEL     ARBNSP        DB0NSP
098040130321     C***                CALL      D90PSL
098050130321     C***                PARM                    FNLSB5DS
098060130321     C***                END
098070130411
098080130411     c                   eval      wpda_ndc=arbndc
098090130411     c                   eval      wpda_ddc=arbddc
098100130411     c                   eval      wpda_dcm=arbdcm
098110130411     c* Richiamo routine per invio variazione a pda
098120130411     c                   exsr      sr_pda
098130911212     C*
098140911212     C     ENDRG4        ENDSR
098150920116     C*
098160920116     C*--- CONTROLLI FORMATO5 ----------------------------------------*
098170920117     C     CONTR5        BEGSR
098180920116     C* CODICE BOLLA NUOVO
098190941102    1C     *IN21         IFEQ      *OFF
098200941102     C*
098210941213     C     '?'           SCAN      NEWCBO                                 90
098220941213    2C     *IN90         IFEQ      *ON
098230941213     C                   MOVEL     CODUT         §KUT
098240920116     C                   MOVEL     '3A'          §COD
098250941102     C                   MOVE      *BLANKS       NEWCBO
098260991011     C                   CLEAR                   §KEY
098270920116     C                   CALL      'X§TABER'
098280920116     C                   PARM                    §KUT
098290920116     C                   PARM                    §COD
098300920116     C                   PARM                    §KEY
098310941209     C                   PARM                    §DES
098320941102     C                   MOVEL     §KEY          NEWCBO
098330941102     C                   MOVEL     §DES          DENCBO
098340920116     C                   SETON                                        90
098350920116     C                   GOTO      ENDCT5
098360941102    2C                   END
098370920116     C*
098380920116     C                   MOVEL     '3A'          COD
098390941102     C                   MOVEL(P)  NEWCBO        KEY
098400941212     C                   EXSR      CTABEL
098410920121     C*
098420941212    2C     *IN30         IFEQ      *ON
098430941102     C                   MOVEL     MSG(16)       VIDMSG
098440941102     C                   SETON                                        409028
098450941102     C                   GOTO      ENDCT5
098460941102    2C                   ENDIF
098470920117     C*
098480920116     C* CODICE NUOVO E VECCHIO NON POSSONO ESSERE UGUALI
098490940614     C* SE PGM RICHIAMATO NON CONTROLLO
098500941212    2C  N20OLDCBO        IFEQ      NEWCBO
098510941102     C                   MOVEL     MSG(17)       VIDMSG
098520941102     C                   SETON                                        409028
098530920116     C                   GOTO      ENDCT5
098540941102    2C                   END
098550920116     C*
098560920116     C                   MOVEL     TBLUNI        DS3A
098570920304     C*
098580941102     C                   MOVEL     §3ADES        DENCBO                          DESCRIZIONE
098590941102     C*
098600920116     C* NON POSSO TRASFORMARE IN BOLLA RECUPERO DA NON RECUPERO E VICEV
098610940322     C* SE RICHIAMATO PERMETTO TUTTO
098620941102    2C     *IN20         IFEQ      *OFF
098630940322     C*
098640941102    3C     §3ARBL        IFEQ      'R'
098650941102     C     SAVRBL        ANDNE     'R'
098660941102     C*
098670941102     C     §3ARBL        ORNE      'R'
098680941102     C     SAVRBL        ANDEQ     'R'
098690941102     C                   MOVEL     MSG(18)       VIDMSG
098700941102     C                   SETON                                        409028
098710920116     C                   GOTO      ENDCT5
098720941102    3C                   END
098730941102    2C                   END
098740920128     C*
098750941102     C* NON POSSO FARE VARIAZIONI DI PORTO
098760941212     C                   MOVEL     §3ATB1        NSCTB1            1
098770941212     C                   MOVEL     SAVTB1        OSCTB1            1
098780920116     C*
098790941102    2C     NSCTB1        IFNE      OSCTB1
098800941102     C                   MOVEL     MSG(19)       VIDMSG
098810941213     C                   SETON                                        409028
098820920128     C                   GOTO      ENDCT5
098830941102    2C                   ENDIF
098840150413     c* non ammesso contrasegno se bolle per EXPO
098850151119     c*                  if        §3afca=1 and (arbtc1='Z' or arbtc2='Z')
098860151119     C*                  MOVEL     MSG(159)      VIDMSG
098870151119     C*                  SETON                                        409028
098880151119     C*                  GOTO      ENDCT5
098890151119     c*                  endif
098900000510     C*
098910000510     C* BOLLE LNP POSTE: SOLO SPEDIZIONI FRANCIO
098920131008     C**   LNPNTW        IFEQ      'PPT'
098930131008     C**   NSCTB1        ANDNE     'F'
098940131008     C**                 MOVEL     MSG(30)       VIDMSG
098950131008     C**                 SETON                                        409028
098960131008     C**                 GOTO      ENDCT5
098970131008     C**                 ENDIF
098980000718     C* BOLLE LNP POSTE CON SERIE : NN POSSO TRASFRMARE IN C/S
098990131008     C**   LNPNTW        IFEQ      'PPT'
099000131008     C**   ARBNRS        ANDGT     0
099010131008     C**   §3ARBL        ANDEQ     'C'
099020131008     C**                 MOVEL     MSG(96)       VIDMSG
099030131008     C**                 SETON                                        409028
099040131008     C**                 GOTO      ENDCT5
099050131008     C**                 ENDIF
099060000925     C* BOLLE LNP POSTE: NON POSSO FARE BOLLE DOPPIE
099070131008     C**   LNPNTW        IFEQ      'PPT'
099080131008     C**   §3ATB2        ANDNE     '  '
099090131008     C**                 MOVEL     MSG(97)       VIDMSG
099100131008     C*+                 SETON                                        409028
099110131008     C**                 GOTO      ENDCT5
099120131008     C**                 ENDIF
099130050414     C* Se prepagato, non posso trasformare in C/S
099140050414     C     §3ARBL        ifeq      'C'
099150050414     c     war6          andeq     'E'
099160050414     c     ar6dft        andgt     0
099170060213     c     d48cvb        andne     'KM'
099180050414     C                   MOVEL     MSG(133)      VIDMSG
099190050414     C                   SETON                                        409028
099200050414     C                   GOTO      ENDCT5
099210050414     C                   ENDIF
099220950407     C*
099230950407     C* MEMORIZZO VARIAZIONE CODICE BOLLA
099240950407     C     NEWCBO        IFNE      SAVCBO
099250950407     C                   MOVE      *BLANKS       COM01A
099260950407     C                   MOVEL     NEWCBO        SAVCBO
099270950407     C                   END
099280941102    1C                   ENDIF
099290920120     C*
099300920121     C* CONTROLLO CONTRASSEGNO
099310941102    1C     §3AFCA        IFEQ      0
099320941213     C     VIDCAS        ANDNE     0
099330941213     C                   MOVEL     MSG(20)       VIDMSG
099340941213     C                   SETON                                        429028
099350941213     C                   GOTO      ENDCT5
099360941213    1C                   ENDIF
099370961204     C* NON POSSO INSERIRE IL C/ASSEGNO PER BOLLE DI RESO
099380961204    1C     §3AFCA        IFGT      0
099390980105     C     ARBFBR        IFEQ      'S'
099400961204     C                   MOVEL     MSG(73)       VIDMSG
099410961204     C                   SETON                                        429028
099420961204     C                   GOTO      ENDCT5
099430961204    1C                   ENDIF
099440980105     C* NON POSSO INSERIRE IL C/A SE BOLLE LEGATA CON LNP=LNA
099450980105     C  N21KARB          CHAIN     FNLBL01L                           30
099460980105    4C  N21*IN30         IFEQ      *OFF
099470980105     C     LBLLAP        ANDEQ     LBLLAN
099480980105     C                   MOVEL     MSG(78)       VIDMSG
099490980105     C                   SETON                                        429028
099500980105     C                   GOTO      ENDCT5
099510980105    1C                   ENDIF
099520980105     C                   ENDIF
099530941102     C*
099540941213     C* SE eliminato importo C/A, elimiare anche tutti i campi relativi
099550011018    1C     VIDCAS        IFEQ      0
099560011018    2C     VIDTIC        IFNE      *BLANKS
099570011018     C     VIDGCA        ORNE      '  '
099580011018     C                   MOVEL     MSG(56)       VIDMSG
099590011018     C                   SETON                                        419028
099600011018     C                   GOTO      ENDCT5
099610011018    2C                   END
099620011018    1C                   END
099630941102     C*
099640931118     C* IMPOSSIBILE ELIMINARE O VARIARE C/A SE GIA' INCASSATO
099650941220     C     SAVFCA        IFNE      0
099660941220     C     ARBICC        ANDNE     ' '
099670941220     C     ARBICC        ANDNE     'R'
099680941220     c*
099690941102    2C     VIDCAS        IFNE      AR9CAS
099700941102     C     VIDTIC        ORNE      AR9TIC
099710941102     C     VIDVCA        ORNE      AR9VCA
099720941102     C     VIDGCA        ORNE      AR9GCA
099730941102     C                   MOVEL     MSG(22)       VIDMSG
099740941102     C                   SETON                                        429028
099750931118     C                   GOTO      ENDCT5
099760941102    2C                   ENDIF
099770941102    1C                   ENDIF
099780920120     C*
099790920204     C* CAUSALE KB NON CONTROLLA ESISTENZA O MENO DEL C/A: OK TUTTO
099800941102    1C     VIDCVR        IFNE      'KB'
099810931227     C*
099820941102     C* SE NON RICHIAMATO NON POSSO USARE CERTI TIPI BOLLA
099830971229     C* NON CONTROLLO PER VARIAZIONE SOLO DI IMPORTO
099840971229     C  N21
099850971229    2CANN20§3AUCB        IFEQ      'N'
099860941102     C                   MOVEL     MSG(16)       VIDMSG
099870941102     C                   SETON                                        409028
099880931227     C                   GOTO      ENDCT5
099890941102    2C                   END
099900920204     C*
099910941102    2C     §3AFCA        IFNE      0
099920941212     C     VIDCAS        ANDEQ     0
099930941102     C                   MOVEL     MSG(21)       VIDMSG
099940941102     C                   SETON                                        429028
099950920120     C                   GOTO      ENDCT5
099960941102    2C                   END
099970941102    1C                   END
099980941213     C**
099990941213     C* SE IMMESSO IMPORTO C/A , CONTROLLO I CAMPI RELATIVI
100000941213     C**
100010941213    1C     VIDCAS        IFGT      0
100020950120     C* SE ACCESO INDICATORE DI VARIAZIONE C/ASS LO SALVO
100030950120     C     *IN95         IFEQ      *ON
100040950120     C                   MOVEL     '1'           WIN95             1
100050950120     C                   ENDIF
100060140515     c                   clear                   trulintds
100070140515     c                   eval      iinttip='B'
100080140515     c                   eval      iinttic=vidtic
100090140515     c                   eval      iintrscm=v1crsm
100100140515     c                   eval      iintrsco=arbrmo
100110140515     c                   eval      IINTAAS=d48aas
100120140515     c                   eval      IINTLNP=d48lnp
100130140515     c                   eval      IINTNRS=d48nrs
100140140515     c                   eval      IINTNSP=d48nsp
100150140515     c                   call      'TRULINTR'
100160140515     c                   parm                    trulintds
100170950120     C*
100180920204     C* CONTROLLO TIPO INCASSO C/A
100190060705     C* richiamo pgm TRULTICR
100200060705     c                   clear                   trulticds
100210060705     c                   clear                   ds1a
100220060705     c                   eval      iticTBO='A'
100230060705     c                   eval      iticlnp=arblnp
100240060705     c                   eval      iticlna=arblna
100250060705     c                   eval      iticlnantw=lnantw
100260060705     c                   eval      iticnzd=arbnzd
100270090707     c                   eval      iticprd=arbprd
100280090707     c                   eval      iticcad=arbcad
100290090707     c                   eval      iticlod=arblod
100300090707     c                   eval      iticind=arbind
100310090707     c                   eval      iticrsd=arbrsd
100320060705     c                   eval      iticdsp=arbdsp
100330130827    3C***  ARBCCM        IFGT      0
100340130827     C***                MOVEL     ARBCCM        iticccm
100350130827     C***                ELSE
100360130827     C***                MOVEL     ARBKSC        iticccm
100370130827    3C***                END
100380130827     c* in iticccm passo il MITTENTE: CCM se assegnato, KSC se franco
100390130827     c                   if        flgtb1='F'
100400130827     c                   z-add     arbksc        iticccm
100410130827     c                   else
100420130827     c                   z-add     arbccm        iticccm
100430130827     c                   if        iticccm=0
100440130827     c                   z-add     8888          iticccm
100450130827     c                   movel     arblnp        iticccm
100460130827     c                   endif
100470130827     c                   endif
100480140515     c* se il beneficiario del c/a è l'ordinante allora passo questo in iticccm
100490140515     c                   if        ointtipi='R'
100500140515     c                   move      ointcdi       iticccm
100510140515     c                   endif
100520060705     c                   eval      itictic=vidtic
100530060705     c                   eval      iticticbol=ar9tic
100540060705     c                   eval      iticdiv=vidvca
100550060705     c                   eval      iticcas=vidcas
100560060705     c                   eval      iticpgm='FNLR48R'
100570060705     c                   call      'TRULTICR'
100580060705     c                   parm                    TRULTICDS
100590060705     c                   parm                    ds1a
100600060705     c
100610060705     c                   if        oticric='T'
100620060705     c                   movel     otictic       vidtic
100630060705     C                   SETON                                        4190
100640060705     C                   GOTO      ENDCT5
100650060705     c                   endif
100660060705     c                   if        oticric='D'
100670060705     c                   movel     oticdiv       vidvca
100680060705     C                   SETON                                        4390
100690060705     C                   GOTO      ENDCT5
100700060705     c                   endif
100710060705     c* Errore tipo incasso
100720091211     c                   if        oticerr='T' or oticerr='C'
100730060705     C                   MOVEL     oticmsg       VIDMSG
100740060705     C                   SETON                                        419028
100750060705     C                   GOTO      ENDCT5
100760060705    2C                   ENDIF
100770060705     c* Errore divisa
100780060705     c                   if        oticerr='D'
100790060705     C                   MOVEL     oticmsg       VIDMSG
100800060705     C                   SETON                                        439028
100810060705     C                   GOTO      ENDCT5
100820060705    2C                   ENDIF
100830011010      *
100840941102     C*
100850941102     C* SE HO MODIFICATO LA VALUTA DEVO RIDIGITARE IL C/A
100860941102     C* INDICATORE DI CHANGE 95 ON
100870941213    2C     VARVCA        IFNE      VIDVCA
100880950120     C     WIN95         ANDEQ     *BLANKS
100890950120     C*
100900941219     C     VIDVCA        IFNE      *BLANKS
100910941219     C     VIDCAS        ORNE      0
100920941102     C                   MOVEL     MSG(28)       VIDMSG
100930941215     C                   SETON                                        429028
100940941102     C                   GOTO      ENDCT5
100950941213    2C                   ENDIF
100960941219    2C                   ENDIF
100970941213     C*
100980011206
100990011206      * pulisco il flag x la forzatura c/assegno forzabile
101000011206     c                   if        vidcas <> sav1_vidcas
101010011206     c                   clear                   forzacas
101020011206     c                   endif
101030011206      *-------------------
101040011206  2b c                   if        vidcas <> 0
101050011206     c                   eval      sav1_vidcas = vidcas
101060011206      * Controllo il c/assegno tramite il trul21r
101070011206     c                   clear                   trul21ds
101080011206     c                   eval      i21tla = 'L'
101090011206     c                   if        *in21 = *off
101100011206     c                   eval      i21cbo = newcbo
101110011206     c                   else
101120011206     c                   eval      i21cbo = arbcbo
101130011206     c                   endif
101140011206     c                   eval      i21tsp = arbtsp
101150011206     c                   eval      i21lnp = arblnp
101160011206     c                   eval      i21nzm = arbnzm
101170011206     c                   eval      i21lna = arblna
101180011206     c                   eval      i21nzd = arbnzd
101190130827     c*****              eval      i21ksc = arbksc
101200011206     c                   movel     arbctr        i21ctr
101210011206     c                   eval      i21tic = vidtic
101220011206     c                   eval      i21imp = vidcas
101230011206     c                   eval      i21div = vidvca
101240011206     c                   eval      i21ute = knmus
101250011206     c                   eval      i21pgm = nompgm
101260130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
101270130827     c                   if        flgtb1='F'
101280130827     c                   z-add     arbksc        i21ksc
101290130827     c                   else
101300130827     c                   z-add     arbccm        i21ksc
101310130827     c                   if        i21ksc=0
101320130827     c                   z-add     8888          i21ksc
101330130827     c                   movel     arblnp        i21ksc
101340130827     c                   endif
101350130827     c                   endif
101360011206     c                   call      'TRUL21R'
101370011206     c                   parm                    trul21ds
101380011206      * controllo che non sia minore del limite previsto
101390011206  3b c                   if        o21fmn <> *blanks
101400011206     c                   eval      vidmsg = msg(108)
101410011210     c                   eval      %subst (vidmsg:31:14) = (%editc(o21lmn: '4'))
101420011206     c                   eval      *in42 = *on
101430011206     c                   eval      *in90 = *on
101440011206     c                   eval      *in28 = *on
101450011206     c                   goto      endct5
101460011206  3e c                   endif
101470011206      * controllo che non sia maggiore del limite previsto
101480011206      * non forzabile
101490011206  3b c                   if        o21fx2 <> *blanks
101500011206     c                   eval      vidmsg = msg(91)
101510011210     c                   eval      %subst (vidmsg:47:14) = (%editc(o21lx2: '4'))
101520011206     c                   eval      *in42 = *on
101530011206     c                   eval      *in90 = *on
101540011206     c                   eval      *in28 = *on
101550011206     c                   goto      endct5
101560011206  3e c                   endif
101570050301      * controllo che non sia maggiore del limite per bancanri bartolini no
101580050301      * abilitati da sede, non forzabile
101590050301  3b c                   if        o21fb <> *blanks
101600050301     c                   eval      vidmsg = msg(132)
101610050301     c                   eval      %subst (vidmsg:26:14) = (%editc(o21lb: '4'))
101620050301     c                   eval      *in42 = *on
101630050301     c                   eval      *in90 = *on
101640050301     c                   eval      *in28 = *on
101650050301     c                   goto      endct5
101660050301  3e c                   endif
101670011206      * controllo che non sia maggiore del limite previsto
101680011206      * forzabile
101690011206  3b c                   if        o21fx1 <> *blanks and forzacas = *blanks
101700011206     c                   eval      forzacas = '1'
101710011206     c                   eval      vidmsg = msg(109)
101720011210     c                   eval      %subst (vidmsg:24:14) = (%editc(o21lx1: '4'))
101730011206     c                   eval      *in42 = *on
101740011206     c                   eval      *in90 = *on
101750011206     c                   eval      *in28 = *on
101760011206     c                   goto      endct5
101770011206  3e c                   endif
101780011206  2e c                   endif
101790011206
101800011120      *
101810011120      * Se bolla giacente e modifica 'KA' o 'KP' l'importo immesso (convertito) deve
101820011120      * essere uguale all'originario
101830011120  2b c                   if        *in04 = *ON  and (vidcvr = 'KA' or
101840011120     c                             vidcvr = 'KP')
101850011120     c                   z-add     vidcas        w0173
101860011120      * Cerco i decimali previsti per la divisa originaria
101870011120     c                   movel     'CV'          cod
101880011120     c                   movel(p)  ar9vca        key
101890011120     c                   exsr      ctabel
101900011120     c  n30              movel     tbluni        dscv
101910011120     c   30              clear                   dscv
101920011120      * Controlla la divisa
101930011120  3b c                   if        ar9vca <> vidvca
101940011120      * Converto il contrassegno inserito a video nella divisa originaria
101950030612     c                   clear                   YeurcoDS
101960011120     c                   movel     vidvca        yecdvi
101970011120     c                   movel     ar9vca        yecdvo
101980011120     c                   z-add     vidcas        yecimi
101990011120     c                   move      §cvdec        yecdco
102000011120     c                   call      'YEURCO'
102010030612     c                   parm                    YeurcoDS
102020011120  4b c                   if        yecesi = ' '
102030011120     c                   z-add     yecimo        w0173
102040011120  4e c                   endif
102050011120  3e c                   endif
102060011120      * Calcolo la differenza tra i 2 contrassegni
102070011120     c                   eval      wdiffe = (ar9cas - w0173)
102080011120     c                   mllzo     1             wdiffe
102090020103      * Converto la differenza nella divisa di conto
102100011120  3b c                   if        ar9vca <> §gedcn
102110030612     c                   clear                   YeurcoDS
102120020103     c                   movel     ar9vca        yecdvi
102130020103     c                   movel     §gedcn        yecdvo
102140011120     c                   z-add     wdiffe        yecimi
102150020103     c                   move      decsav        yecdco
102160011120     c                   call      'YEURCO'
102170030612     c                   parm                    YeurcoDS
102180011120  4b c                   if        yecesi = ' '
102190011120     c                   z-add     yecimo        wdiffe
102200011120  4e c                   endif
102210011120  3e c                   endif
102220020103      *
102230020103      * Se differenza maggiore del valore in tabella ERRORE
102240011120  3b c                   if        wdiffe > dfcsav
102250011120     c                   movel     msg(103)      vidmsg
102260011120     c                   seton                                        429028
102270011120     c                   goto      endct5
102280011120  3e c                   endif
102290011120  2e c                   endif
102300011120
102310991021     C**
102320991021     C                   MOVEL     VIDVCA        VARVCA
102330991021     C                   CLEAR                   WIN95
102340941102     c**
102350941102     C* PARTICOLARITA' C/ASSEGNO
102360941102     c**
102370941213    2C     VIDGCA        IFNE      *BLANKS
102380941102     C     VIDGCR        ORNE      *BLANKS
102390941102     C*
102400941102     C* non posso indicarla se NON IMMESSO C/A
102410941213    3C     VIDCAS        IFEQ      0
102420941102     C     VIDGCA        ANDNE     *BLANKS
102430941102     C                   SETON                                        449028
102440941102     C                   MOVEL     MSG(29)       VIDMSG
102450941102     C                   GOTO      ENDCT5
102460941213    3C                   ENDIF
102470941102     C*
102480941102     C* RICHIAMO IL PRG DI RICERCA
102490941213    3C     VIDGCR        IFEQ      '?'
102500030612     C                   CLEAR                   DS7pqrs
102510941102     C                   MOVEL     'S'           DS7RIC
102520941102     C                   MOVEL     'V'           DS7GES
102530941213    4C     ARBCCM        IFGT      0
102540941102     C                   MOVEL     ARBCCM        DS7KSC
102550941102     C                   ELSE
102560941102     C                   MOVEL     ARBKSC        DS7KSC
102570941213    4C                   ENDIF
102580941102     C                   MOVEL     VIDGCA        DS7COD
102590030612     C                   MOVEL     DS7pqrs       KPJBU
102600941102     C                   CALL      'TRTB72R'
102610941102     C                   PARM                    KPJBA
102620030612     C                   MOVEL     KPJBU         DS7pqrs
102630941102     C*
102640941102     C     VIDGCA        IFEQ      *BLANKS
102650941102     C                   MOVEL     DS7COD        VIDGCA
102660941213    3C                   ENDIF
102670941102     C*
102680941102     C                   CLEAR                   VIDGCR
102690941102     C*
102700941102     C                   SETON                                        90
102710941102     C                   GOTO      ENDCT5
102720941102    2C                   ENDIF
102730941102     C* CONTROLLO
102740030612     C                   CLEAR                   DS7pqrs
102750941102     C                   MOVEL     'S'           DS7RIC
102760941102     C                   MOVEL     'C'           DS7GES
102770941213    2C     ARBCCM        IFGT      0
102780941102     C                   MOVEL     ARBCCM        DS7KSC
102790941102     C                   ELSE
102800941102     C                   MOVEL     ARBKSC        DS7KSC
102810941102     C                   ENDIF
102820941102     C                   MOVEL     VIDGCA        DS7COD
102830030612     C                   MOVEL     DS7pqrs       KPJBU
102840941102     C                   CALL      'TRTB72R'
102850941102     C                   PARM                    KPJBA
102860030612     C                   MOVEL     KPJBU         DS7pqrs
102870941102     C* ERRORE
102880941213    3C     DS7ERR        IFEQ      '1'
102890941102     C                   MOVEL     DS7MSG        VIDMSG
102900941102     C                   SETON                                        449028
102910941102     C                   GOTO      ENDCT5
102920941213    3C                   ENDIF
102930941102     C*
102940941213    2C                   ENDIF
102950941213    1C                   ENDIF
102960011010      *-------------------
102970941220     C*
102980941220     c* SE MODIFICATO IK TIPO BOLLA ED ELIMINATA LA 2 MA GIA' INCASS
102990941220     C*  ERRORE
103000941220     C     §3ATB2        IFEQ      *BLANKS
103010941220     C*
103020941220     C     SAVTB2        ANDNE     *BLANKS
103030941220     C     ARBICA        ANDNE     ' '
103040941220     C     ARBICA        ANDNE     'R'
103050991007     C     DFT2BO        ANDGT     0
103060941220     C                   MOVEL     MSG(59)       VIDMSG
103070941220     C                   SETON                                        409028
103080941220     C                   GOTO      ENDCT5
103090941220     C                   END
103100950407     C* SE CAUSALE VARIAZIONE CODICE BOLLA
103110950407    1C     *IN21         IFEQ      *OFF
103120950407     C     COM01A        ANDEQ     *BLANKS
103130950407     C* E NON TROVATI ERRORI BLOCCANTI
103140950407     C     *IN90         ANDEQ     *OFF
103150950407     C* E ASSEGNATO
103160950407     C     FLGTB1        ANDEQ     'A'
103170950407     C* SE AGGIUNTO O TOLTO IL CONTRASSEGNO
103180950407    2C     SAVFCA        IFEQ      *ZEROS
103190950407     C     §3AFCA        ANDNE     *ZEROS
103200950407     C     SAVFCA        ORNE      *ZEROS
103210950407     C     §3AFCA        ANDEQ     *ZEROS
103220991007     C                   MOVEL     '1'           WTRC
103230991007     C     KARB2         CHAIN(N)  FIAR6000                           30
103240950407    3C     *IN30         IFEQ      *OFF
103250950407     C* SE GIA' TASSATO FINO ALL'IMPONIBILE
103260950407     C     AR6IMV        ANDNE     *ZEROS
103270950407     C                   SETON                                        2892
103280950407     C* ERRORE NON BLOCCANTE:MANCA VARIA G
103290950407    4C     §3AFCA        IFNE      *ZEROS
103300950407     C                   MOVEL     MSG(65)       VIDMSG
103310950407     C                   ELSE
103320950407     C* ERRORE NON BLOCCANTE:C'E' VARIA G
103330950407     C                   MOVEL     MSG(66)       VIDMSG
103340950407    4C                   END
103350950407    3C                   END
103360950407     C*
103370950407    2C                   END
103380950407     C                   MOVE      '1'           COM01A            1
103390950407    1C                   END
103400920116     C*
103410920116     C     ENDCT5        ENDSR
103420920116     C*
103430920116     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE KX -----------------*
103440920129     C     REGIS5        BEGSR
103450991001     C                   SETOFF                                       903839
103460130412     c                   clear                   wpda_ndc
103470130412     c                   clear                   wpda_ddc
103480130412     c                   clear                   wpda_dcm
103490071116     C
103500950104     C*  GIRO UDATE
103510950104     C                   TIME                    W0140            14 0
103520950104     C* UDATE IN GGMMAAAA
103530950104     C                   MOVE      W0140         WDTGIO            8 0
103540950104     C*
103550950104     C* UDATE IN AAAAMMGG
103560950104     C                   Z-ADD     WDTGIO        G02DAT
103570950104     C                   MOVEL     *BLANK        G02ERR
103580950104     C                   CALL      'XSRDA8'
103590950104     C                   PARM                    WLBDAT
103600950104     C                   MOVEL     G02INV        DATEU             8 0
103610941221     C* PRENDO I VALORI ASSOLUTI
103620941221     C                   MLLZO     1             VIDCAS
103630941221     C*
103640941213     C                   MOVEL     KNMUS         ARBPRU
103650941220     C                   MOVEL     ARBCPI        SAVCPI
103660070515     c
103670070515     c* Se modificato il C/A, devo allocarmi record "L" per il
103680070515     c*  blocco telefonata C/A
103690070515     c                   clear                   wesar4L           1
103700070515    1C     vidCAS        IFNE      SAVCAS
103710070515     C     vidTIC        ORNE      SAVTIC
103720070515     C     vidVCA        ORNE      SAVVCA
103730070807     C     vidcas        andgt     0
103740070515     c                   eval      Tipoela='C'
103750070515     c                   exsr      GestAR4L
103760070515    2c                   if        *in91
103770070515     C                   GOTO      ENDRG5
103780070515    2c                   endif
103790070515    1c                   endif
103800070515     c
103810070515     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
103820070515     c* iornare direttamente il file solo se cambiato tipo bolla
103830070515     c                   clear                   waltrabolla       1
103840130412     c                   setoff                                       3091
103850130412     c                   if        oldcbo<>newcbo
103860070515     C   10KARB          CHAIN     FNBLP01L                           3091
103870070515     C  n10KARB          CHAIN     FNARB01L                           3091
103880130412     c                   else
103890130412     c* questa chain mi serve per l'invio dati a PDA
103900131120     c                   if        d66pda<>' '
103910130412     C  n10KARB          CHAIN(n)  FNARB01L                           30
103920130412     c                   endif
103930130412     c                   endif
103940070515     c*
103950070515     c                   if        *in91
103960070515     C                   SETON                                        28
103970070515     C                   MOVEL     MSG(4)        VIDMSG
103980070515     c                   unlock    fiar401l
103990070515     C                   GOTO      ENDRG5
104000070515     c                   else
104010070515     c                   if        *in30
104020070515     c                   eval      waltrabolla='N'
104030070515     c                   else
104040130411     c  n10              eval      wpda_ndc=arbndc
104050130411     c  n10              eval      wpda_ddc=arbddc
104060130411     c  n10              eval      wpda_dcm=arbdcm
104070130412     c                   if        oldcbo<>newcbo
104080070515     C   10KARB          CHAIN     FNARB01L                           30
104090130412     c                   endif
104100131120     c                   if        oldcbo<>newcbo or d66pda<>' '
104110070515     C  N10KARB          CHAIN     FNBLP01L                           30
104120070515     c* Alla fine per non sporcarmi i campi del record, richaino il
104130070515     c*  record originale che sto variando
104140130411     c   10              eval      wpda_ndc=arbndc
104150130411     c   10              eval      wpda_ddc=arbddc
104160130411     c   10              eval      wpda_dcm=arbdcm
104170130412     c                   endif
104180070515     c                   endif
104190070515     c                   endif
104200130412     c*****              endif
104210920123     C*********
104220920116     C* INVIO PER SEDE
104230941014     C                   MOVEL     'FNARBK0T'    SFILE
104240920116     C* ALLOCO OGGETTI
104250920116     C                   EXSR      ALLOC
104260070515     c                   if        *in91
104270070515     c                   unlock    fiar401l
104280070515     c   10              unlock    fnblp01l
104290070515     c  n10              unlock    fnarb01l
104300070515     C                   GOTO      ENDRG5
104310070515     c                   endif
104320930511     C*
104330941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL MITTENTE PER C/A
104340941212     C* INVIO SOLO IN SEDE L'INDIRIZZO DEL DESTINATARIO SE DOPPIA BOLLA
104350930511     C                   MOVEL     'S'           §7LFFI
104360930511     C*
104370930210     C********* PER  C O N T R A S S E G N O
104380941215    1C     AR9CAS        IFEQ      0
104390941102     C     VIDCAS        ANDNE     0
104400930210     C                   SETON                                        38
104410930210     C* INVIO PER SEDE
104420941014     C                   MOVEL     'FNARBM0T'    SFILE
104430930210     C* ALLOCO OGGETTI
104440930210     C                   EXSR      ALLOC
104450070515     c
104460070515     c                   if        *in91
104470070515     C                   MOVEA     'FNARBK0T'    C21(13)
104480070515     C                   MOVEA     VAR           C21(41)
104490070515     C                   MOVEL     *BLANKS       COMMAN           80
104500070515     C                   MOVEA     C21           COMMAN
104510070515     C                   CALL      'QCMDEXC'                            H1
104520070515     C                   PARM                    COMMAN
104530070515     C                   PARM                    LUNG
104540070515     c                   unlock    fiar401l
104550070515     c   10              unlock    fnblp01l
104560070515     c  n10              unlock    fnarb01l
104570070515     c
104580070515     C                   GOTO      ENDRG5
104590941212    1C                   END
104600070515    1C                   END
104610950324     C*
104620930210     C********* PER  D O P P I A    B O L L A
104630941212    1C     SAVTB2        IFEQ      '  '
104640930210     C     §3ATB2        ANDNE     '  '
104650930210     C                   SETON                                        39
104660930210     C* INVIO PER SEDE
104670941102     C                   MOVEL     'FNARBD0T'    SFILE
104680930210     C* ALLOCO OGGETTI
104690930210     C                   EXSR      ALLOC
104700070515     c                   if        *in91
104710070515     C                   MOVEA     'FNARBK0T'    C21(13)
104720070515     C                   MOVEA     VAR           C21(41)
104730070515     C                   MOVEL     *BLANKS       COMMAN           80
104740070515     C                   MOVEA     C21           COMMAN
104750070515     C                   CALL      'QCMDEXC'                            H1
104760070515     C                   PARM                    COMMAN
104770070515     C                   PARM                    LUNG
104780070515     c
104790070515     C                   MOVEA     'FNARBM0T'    C21(13)
104800070515     C                   MOVEL     *BLANKS       COMMAN           80
104810070515     C                   MOVEA     C21           COMMAN
104820070515     C                   CALL      'QCMDEXC'                            H1
104830070515     C                   PARM                    COMMAN
104840070515     C                   PARM                    LUNG
104850070515     c                   unlock    fiar401l
104860070515     c   10              unlock    fnblp01l
104870070515     c  n10              unlock    fnarb01l
104880070515     C                   GOTO      ENDRG5
104890950324     C                   ENDIF
104900070515     C                   ENDIF
104910041022     C*********
104920041022     C                   Z-ADD     DATEU         ARBDTV
104930060320     C                   TIME                    ARBORV
104940920116     C* STORICIZZAZIONE ALL'ARRIVO
104950060130    1C     D66FSA        ifeq      'S'
104960920116     C                   MOVEL     VIDCVR        ARBCVB
104970941213     C                   MOVEL     OLDCBO        ARBCBP
104980941213     C                   MOVEL     NEWCBO        ARBCBN
104990941213     C                   MOVEL     AR9TIC        ARBTIC
105000941213     C                   MOVEL     AR9CAS        ARBCAS
105010941213     C                   MOVEL     AR9VCA        ARBVCA
105020941213     C                   MOVEL     AR9GCA        ARBGCA
105030011108      *
105040011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
105050011108      * è in partenza 'P' o in arrivo 'A'
105060011108     C     *IN10         IFEQ      *ON
105070011108     C                   MOVE      'A'           ARBPRU
105080011108     C                   ENDIF
105090011108     C     *IN10         IFEQ      *OFF
105100011108     C                   MOVE      'P'           ARBPRU
105110011108     C                   ENDIF
105120011108      *
105130941014     C                   WRITE     FNARBK00
105140930210     C                   END
105150060213     c* storicizzazione motivazione
105160060213     c     d66mtv        ifeq      'S'
105170060301     c                   exsr      sto_mtv
105180060213     c                   endif
105190920116     C*
105200920123     C*********
105210920117     C* I N V I O
105220941102     C                   MOVEL     NEWCBO        ARBCBN
105230941213     C* PER IL FILE DI VARIAZIONE
105240941102     C                   MOVEL     VIDCAS        ARBCAS
105250920116     C                   MOVEL     VIDTIC        ARBTIC
105260941212     C                   MOVEL     VIDGCA        ARBGCA
105270941213     C*
105280941215     C*
105290941212     C                   MOVEL     VIDVCA        ARBVCA
105300011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
105310011017     C     VIDCAS        IFEQ      *ZEROS
105320011017     C                   CLEAR                   ARBVCA
105330011017     C                   ENDIF
105340920123     C*********
105350041022     C     D66FFI        ifeq      'S'
105360920117     C                   TIME                    ARBORV
105370920127     C                   MOVEL     VIDCVR        ARBCVB
105380941014     C                   OPEN      FNARBK0T
105390941014     C                   WRITE     FNARBKTT
105400941014     C                   CLOSE     FNARBK0T
105410920117     C* DISALLOCO
105420941014     C                   MOVEA     'FNARBK0T'    C21(13)
105430041022     C                   MOVEA     VAR           C21(41)
105440920117     C                   MOVEL     *BLANKS       COMMAN           80
105450920117     C                   MOVEA     C21           COMMAN
105460920117     C                   CALL      'QCMDEXC'                            H1
105470920117     C                   PARM                    COMMAN
105480920117     C                   PARM                    LUNG
105490920212     C*
105500920212     C   38              DO
105510941014     C                   OPEN      FNARBM0T
105520950102     C                   WRITE     FNARBMTT
105530941014     C                   CLOSE     FNARBM0T
105540920212     C* DISALLOCO
105550941014     C                   MOVEA     'FNARBM0T'    C21(13)
105560920212     C                   MOVEA     C21           COMMAN
105570920212     C                   CALL      'QCMDEXC'                            H1
105580920212     C                   PARM                    COMMAN
105590920212     C                   PARM                    LUNG
105600920212     C                   END
105610920213     C*
105620920213     C   39              DO
105630930210     C                   MOVEL     *BLANKS       ARBCPI
105640920213     C                   ADD       1             ARBORV
105650930210     C                   MOVEL     VIDCVR        ARBCVB
105660941212     C                   MOVEL     NEWCBO        ARBCBO
105670941102     C                   OPEN      FNARBD0T
105680941209     C                   WRITE     FNARBDTT
105690941102     C                   CLOSE     FNARBD0T
105700920213     C* DISALLOCO
105710941102     C                   MOVEA     'FNARBD0T'    C21(13)
105720920213     C                   MOVEA     C21           COMMAN
105730920213     C                   CALL      'QCMDEXC'                            H1
105740920213     C                   PARM                    COMMAN
105750920213     C                   PARM                    LUNG
105760920213     C                   END
105770930210     C                   END
105780920117     C*
105790920117     C* A G G I O R N O   F I L E
105800920117     C*
105810051118     c* se variato imposto la data ultima variazione
105820091217    1c                   if        vidvca<>ar9vca or
105830051118     c                             ar9cas<>vidcas or
105840051118     c                             ar9vca<>vidvca or
105850051118     c                             ar9gca<>vidgca
105860051118     c                   movel     dateu         ar9duv
105870091217    1c                   endif
105880091217     c
105890091217     c* Se variato il tipo incasso C/Ass, lo memorizzo
105900140516     c* non lo memorizzo in caso di annullamento altrimenti su internet viene
105910140516     c* visualizzata sia la variazione che l'annullamento
105920140516    1c                   if        savtic<>vidtic and vidcas>0
105930091217     c                   eval      Wtipoagg='TIC'
105940091217     c                   exsr      RegisAR5GEN
105950091217    1c                   EndIf
105960091217     c
105970091217     c
105980941212     C                   MOVEL     NEWCBO        ARBCBO
105990941212     C                   MOVEL     VIDCAS        AR9CAS
106000941212     C                   MOVEL     VIDTIC        AR9TIC
106010941212     C                   MOVEL     VIDGCA        AR9GCA
106020941212     C                   MOVEL     VIDVCA        AR9VCA
106030011017      * Se l'importo del contrassegno è 0 non devo scrivere la divisa
106040011017     C     VIDCAS        IFEQ      *ZEROS
106050011017     C                   CLEAR                   AR9VCA
106060011017     C                   ENDIF
106070011019      *
106080941220     C                   MOVEL     ARBCPI        SAVCPI
106090920121     C*
106100941220     C* E L I M I N A T A   L A   2   B O L L A
106110941220    1C     SAVTB2        IFNE      '  '
106120920603     C     §3ATB2        ANDEQ     '  '
106130941220     C* PULISCO PARTITA IVA
106140930210     C                   MOVEL     *BLANKS       ARBCPI
106150991001     C* SE C'ERA AR6 SECONDA BOLLA LO DELETO
106160990930     C     WAR62         IFEQ      'E'
106170991008     C                   MOVEL     '2'           WTRC
106180991008     C     KARB2         DELETE    FIAR6000                           30
106190941220     C                   ENDIF
106200941220     C*
106210941220     C* SE GIA' CONSEGNATA DO' PER INCASSATO L'ASSEGNATO
106220000925    2C     ARBDCM        IFGT      0
106230000925    3C     ARBICA        IFEQ      'R'
106240000925     C     ARBICA        OREQ      ' '
106250941220     C                   MOVEL     'Y'           ARBICA
106260000925    4C     ARBICC        IFNE      'R'
106270970613     C                   EXSR      CHIUBO
106280000925    5C     ARBNMI        IFNE      8888888
106290971023     C                   MOVEL     'S'           ARBICA
106300000925    5C                   ENDIF
106310000925    4C                   ENDIF
106320000925    3C                   ENDIF
106330000925    2C                   ENDIF
106340941220     C* ABILITO L'ASSEGNATO
106350941220     C                   MOVEL     'S'           ARBACA
106360941220    1C                   ENDIF
106370941220     C*
106380941220     C* A G G I U N T A   2   B O L L A
106390941220     C     SAVTB2        IFEQ      '  '
106400941220     C     §3ATB2        ANDNE     '  '
106410941220     C* PULISCO PARTITA IVA
106420941220     C                   MOVEL     *BLANKS       ARBCPI
106430941220     C*
106440941220     C                   MOVEL     ' '           ARBACA
106450941220     C                   MOVEL     ' '           ARBICA
106460941220     C* SE GIA' CONSEGNATA SI TRATTA DI UN RITORNO ALL'INCASSO
106470941220     C     ARBDCM        IFGT      0
106480941220     C                   MOVEL     'R'           ARBICA
106490941220     C                   ENDIF
106500941220     C                   ENDIF
106510920120     C*
106520920117     C* ABILITAZIONE CONTRASSEGNO
106530950918     C* SE NON HO VARIATO IL C/A RISPETTO A PRIMA, LASCIO ABILITAZIONE
106540950918     C*  E INCASSO C/A COME ERANO
106550950918    1C     AR9CAS        IFNE      SAVCAS
106560950918     C     AR9TIC        ORNE      SAVTIC
106570950918     C     AR9VCA        ORNE      SAVVCA
106580920120     C                   MOVEL     ' '           ARBACC
106590070320     c                   eval      §b4ltca='S'
106600070320     c
106610941220     C*
106620920117     C* SE NON C'E' PIU' IL C/ASSEGNO ABILITO
106630950918    2C     §3AFCA        IFEQ      0
106640950104     C     AR9CAS        OREQ      0
106650920117     C                   MOVEL     'S'           ARBACC
106660070320     c                   eval      §b4ltca=' '
106670960704     C*
106680960909     C* SE IL CONTRASSEGNO E' UN RITORNO ALL'INCASSO, LO TOLGO
106690960704     C* SE CONSEGNATO, E RITORNO ALL'INCASSO LO DO PER INCASSATO
106700960704    3C     ARBDCM        IFGT      0
106710960704     C     ARBICC        ANDEQ     'R'
106720960704     C                   MOVEL     'Y'           ARBICC
106730970613     C* SE L'ASSEGNATO NON E' UN RITORNO ALL'INCASSO VEDO SE CHIUDERE
106740970613     C* LA BOLLA
106750970613     C     ARBICA        IFNE      'R'
106760970613     C                   EXSR      CHIUBO
106770971023     C     ARBNMI        IFNE      8888888
106780971023     C                   MOVEL     'S'           ARBICC
106790971023     C                   ENDIF
106800970613     C                   ENDIF
106810970613     C                   ENDIF
106820960704     C*
106830950918   X2C                   ELSE
106840941220     C*
106850941220     C* SE LA BOLLA E' GIA' CONSEGNA METTO "R" DI RITORNO ALL'INCASSO
106860950918    3C     ARBDCM        IFGT      0
106870941220     C                   MOVEL     'R'           ARBICC
106880950918   X3C                   ELSE
106890941220     C* ALTRIMENTI PULISCO IL FLAG DI INCASSO C/A
106900941220     C                   CLEAR                   ARBICC
106910950918    3C                   ENDIF
106920011031      *
106930070320    3c                   if        o21fca = *blanks
106940020103     c                   eval      arbacc = 'S'
106950070320     c                   clear                   §b4ltca
106960070320    3c                   endif
106970070320    2C                   END
106980070320     c
106990070320     c* Aggiorno record di FIar4
107000070320     c                   eval      TipoEla='A'
107010070320     c                   exsr      GestAR4L
107020070320    1C                   END
107030071116     c*
107040071116     C* M O D I F I C A T O   S O L O   1 °   T I P O   B O L L A
107050071116    1C     SAVTB2        IFEQ      '  '
107060071116     C     §3ATB2        ANDEQ     '  '
107070071116     C* PRENDI IL TIPO BOLLA DEL NUOVO CODICE
107080071116     C                   MOVEL     'TB'          COD
107090071116     C                   MOVEL(P)  §3atb1        KEY
107100071116     C                   EXSR      CTABEL
107110071116     C  N30              MOVEL     TBLUNI        DSTB
107120071116     C   30              CLEAR                   DSTB
107130071116     c                   movel     §tbfcb        NEWtb1FCB         1
107140071116     c
107150071116     C* Se il nuovo tipo bolla è da non contabilizzare, imposto
107160071116     c*  la "S" di abilitazione Assgnato
107170071116    2c                   if        ARBACA=' ' and NEWtb1FCB='0'
107180071116     c                   eval      arbaca='S'
107190071116    2c                   endif
107200071116     c
107210071116     c* Se il nuovo tipo bolla è da contabilizzare, ma non lo era il
107220071116     c*  vecchio, vedo se devo togliere la "S" per l'abilitazione del C/A
107230071116    2c                   if        NEWtb1FCB='1' and Newtb1FCB<>OLDtb1FCB
107240071116     c                             and arbaca='S'
107250071116     C* TASSAZIONE IN FIAR6
107260071116     C                   MOVEL     '1'           WTRC
107270071116     C     KARB2         CHAIN(N)  FIAR6000
107280071116    3c                   if        (%subst(%editc(arbksc: 'X'):4:4)='9999') and
107290071116     c                             (not %found(FIar601l) or ar6dft=0)
107300071116     c                   eval      arbaca=' '
107310071116    3c                   endif
107320071116    2c                   endif
107330071116     c
107340071116    1C                   ENDIF
107350920116     C*
107360941216     C     §3AFCA        IFNE      0
107370950104     C     AR9CAS        ANDGT     0
107380941212     C     WAR9          IFEQ      ' '
107390941212     C                   MOVEL     ARBAAS        AR9AAS
107400941212     C                   MOVEL     ARBLNP        AR9LNP
107410941212     C                   MOVEL     ARBNRS        AR9NRS
107420941212     C                   MOVEL     ARBNSP        AR9NSP
107430051118     c                   movel     dateu         ar9duv
107440051118     C                   WRITE     FiAR9000
107450941212     C                   ELSE
107460051118     C                   UPDATE    FiAR9000
107470941212     C                   ENDIF
107480941216     C*
107490941216     C                   ELSE
107500941216     C* SE MI HANNO CANCELLATO L'AR9 --> DELETO
107510941216     C     WAR9          IFEQ      'E'
107520051118     C     KARB          DELETE    FiAR9000                           30
107530941216     C                   ENDIF
107540941216     C                   ENDIF
107550941212     C*
107560060213     C   10              UPDATE    FNARB000
107570041022     c* Se cambiato codice bolla aggiorno fnblp
107580041022     c                   if        waltrabollA=' ' AND oldcbo<>newcbo
107590041022     c                   eval      arbcbo=newcbo
107600041022     c   10              except    arbkblp
107610041022     c  N10              except    arbkarb
107620041022     c                   endif
107630040224
107640040224      * Se c'è la data consegna richiesta controllo se è ancora valida
107650040224      * solo se ho fatto una modifica in arrivo
107660160114     c                   If        ArbDcr > *Zeros and *In10 and wfile='A'
107670040324     c                             and wCa = *Blanks and wgiac = *Blanks
107680040224     c                   ExSr      ContrDcr
107690040224     c                   EndIf
107700950602     C*
107710130411     c* Richiamo routine per invio variazione a pda
107720130411     c                   exsr      sr_pda
107730950104     C*
107740920117     C     ENDRG5        ENDSR
107750911211     C*
107760911211     C*--- CARICO CODICI TARIFFA -------------------------------------*
107770911211     C     CARCTR        BEGSR
107780020305     c**
107790020305     C* Richiamo trul27r per reperimento WFIE e per reperimento tariffa di
107800020305     C* cartello
107810020305     c**
107820160225     c                   clear                   trul27ds1
107830021121     c                   Eval      I27Pkg = ArbPkf
107840020305     c                   eval      i27tsp = arbtsp
107850020305     c                   eval      i27lna = arblna
107860020305     c                   eval      i27lnp = arblnp
107870020305     C                   eval      i27cli = ksc
107880160225     c                   call      'TRUL27R1'
107890160225     c                   parm                    trul27ds1
107900020305     c                   eval      wfie = o27fie
107910020305     c*
107920941209     C                   MOVEL     *BLANKS       DE2CTR
107930050202     C                   CLEAR                   Fnlv59DS
107940941209     C*
107950050202     C                   MOVEL     D48TBO        ilv59tbo
107960040511      * SE BOLLA MONOVARIA NON PASSO IL TIPO BOLLA
107970040511     C                   IF        §3ASVA <> *BLANK
107980050202     C                   CLEAR                   ilv59tbo
107990050202     c                   eval      ilv59prp='P'
108000040511     C                   ENDIF
108010050202     C                   Z-ADD     ARBDSP        ilv59DSP
108020060131     C                   MOVEL     dutkci        ilv59KCI
108030050202     C                   Z-ADD     KSC           ilv59KSC
108040050202     C                   MOVEL     WFIE          ilv59FIE
108050160225     C                   MOVEL     WFIE          ilv59ta3
108060050202     C                   MOVEL     ARBTSP        ilv59TSP
108070050202     C                   MOVEL     VIDCTR        ilv59CTR
108080941209     C*
108090941209     C* SE NON C'E' RICERCA SU COD.TARIFFA E CAMBIATO CODICE CLIENTE
108100941209     C*  --> RICERCO LA TARIFFA
108110941209     C                   SETOFF                                       32
108120941209     C     '?'           SCAN      VIDCTR                                 32
108130941215     C     *IN32         IFEQ      *OFF
108140941215     C     KSC           ANDNE     SAVKSC
108150050202     C     VIDCTR        ANDEQ     SAVCTR
108160160225     C***                CLEAR                   ilv59CTR
108170941223     C                   CLEAR                   OTT
108180160225     C***                MOVEL     'S'           ilv59RIC
108190941215     C                   ELSE
108200060516     c* Se cod cliente e cod tariffa invariati rispetto alla bolla,
108210060516     c*  accetto codici tariffa bloccati (ma quando immessi validi)
108220060516     c                   if        not *in32 and ksc=kscbolla
108230060511     c                             and ctrbolla=vidctr
108240060516     C                   MOVEL     'B'           ilv59ta2
108250941209     C                   ENDIF
108260060511     C                   ENDIF
108270060516     C                   MOVEL     'S'           ilv59CON
108280050202     C
108290050202     C                   MOVEL     KSC           SAVKSC
108300160303     c* se ho abblancato il codice tariffa, lo faccio reimpostare dal pgm
108310160303     c                   if        ilv59ctr=*blanks
108320160303     C                   MOVEL     'S'           ilv59RIC
108330160303     c                   endif
108340941209     C*
108350050202     C                   CALL      'FNLV59R'
108360050202     C                   PARM                    Fnlv59DS
108370050202     c
108380941209     C*
108390941209     C* NON ESISTONO TARIFFE
108400050202     C                   MOVEL     olv59TKS      WTKS              1
108410050202     C     olv59TKS      IFNE      'S'
108420941209     C                   SETON                                        18
108430911219     C                   SETOFF                                       32
108440950114     C                   MOVEL     CNOTAR        DE2CTR
108450941209     C                   ELSE
108460941209     C*
108470941215     C* SE TARIFFE TROVATE,  DEVO REIMPOSTARE LA TARIFFA A VIDEO
108480050202     C     ilv59RIC      IFEQ      'S'
108490050202     C                   MOVEL     olv59CTR      VIDCTR
108500941209     C                   ENDIF
108510941215     C* DECODIFICA
108520050202     C                   MOVEL     olv59DFS      DE2CTR
108530941209     C                   ENDIF
108540050202     c
108550050202     C                   EVAL      SAVCTR=VIDCTR
108560911211     C*
108570911211     C                   ENDSR
108580911211     C*
108590920320     C*--- CONTROLLI CAMPI CONSEGNA RICHIESTA ------------------------*
108600920320     C     CONTR7        BEGSR
108610920320     C                   SETOFF                                       90
108620941102     c**
108630920320     C* CONSEGNA RICHIESTA
108640941102     C**
108650920320     C                   Z-ADD     0             COMDCR
108660920320     C     VIDDCR        IFNE      0
108670941102     C                   Z-ADD     VIDDCR        G02DAT
108680920320     C                   MOVEL     *BLANK        G02ERR
108690941102     C                   CALL      'XSRDA8'
108700920320     C                   PARM                    WLBDAT
108710941102     C* DATA FORMALMENTE ERRATA
108720920320     C     G02ERR        IFEQ      '1'
108730941102     C                   MOVEL     MSG(6)        VIDMSG
108740941102     C                   SETON                                        409028
108750920320     C                   GOTO      ENDCT7
108760920320     C                   END
108770941102     C*
108780941102     C                   Z-ADD     G02DAT        VIDDCR
108790941102     C*
108800920320     C* DATA CONSEGNA NO < DATA SPEDIZIONE
108810930507     C     G02INV        IFLT      ARBDSP
108820941102     C                   MOVEL     MSG(7)        VIDMSG
108830941102     C                   SETON                                        409028
108840920320     C                   GOTO      ENDCT7
108850930507     C                   END
108860941102     C                   Z-ADD     G02INV        COMDCR            8 0
108870941102     C                   ENDIF
108880070308     c
108890070308      * La data non può superare i gg limiti indicati in tab. VPO
108900070308     c                   If        Not *In90
108910070308     c                   Eval      wdataiso = wdataoggi
108920070308     c                   Adddur    §vpodcr:*d    wdataiso
108930070308     c     *iso          move      wdataiso      ds_data
108940070308     c                   Move      §vpodcr       w003a
108950070308     c                   Movea     w003a         ski(13)
108960070308     c                   Z-add     13            i
108970070308     c                   ExSr      abbl
108980070308     c                   Movea     ski(13)       w003a
108990070308     c                   If        ComDcr > ds_data
109000070308     c                   Eval      *In28 = *On
109010070308     c                   Eval      *In40 = *On
109020070308     c                   Eval      *In90 = *On
109030070308     c                   Eval      VidMsg = msg(129)
109040070308     c                   Eval      VidMsg = %replace(w003a:VidMsg:
109050070308     c                                      %scan('___':VidMsg))
109060070308     C                   GOTO      ENDCT7
109070070308     c                   EndIf
109080070308     c                   EndIf
109090141029     c* Se dcr>0 errore forzabile se presente Fermo Deposito
109100141029     c                   if        d48ffr<>'E'
109110141029     c                   if        comdcr>0 and comdcr<>arbdcr and
109120141029     c                             arbffd<>*blanks and wforzdcr=0
109130141029     C                   MOVEL     MSG(154)      VIDMSG
109140141029     C                   SETON                                        409028
109150141029     C                   EVAL      Wforzdcr=1
109160141029     C                   GOTO      ENDCT7
109170141029     c                   endif
109180141029     c                   endif
109190941102     C**
109200941102     C** ORA CONSEGNA RICHIESTA
109210941102     C**
109220920320     C                   MOVEL     VIDHCR        HM                2 0
109230920320     C     HM            IFLT      0
109240920320     C     HM            ORGT      24
109250941102     C                   MOVEL     MSG(8)        VIDMSG
109260941102     C                   SETON                                        419028
109270920320     C                   GOTO      ENDCT7
109280920320     C                   END
109290941102     C**
109300941102     C** MINUTI CONSEGNA RICHISTA
109310941102     C**
109320920320     C                   MOVE      VIDHCR        HM                2 0
109330920320     C     HM            IFLT      0
109340920320     C     HM            ORGT      59
109350941102     C                   MOVEL     MSG(8)        VIDMSG
109360941102     C                   SETON                                        419028
109370920320     C                   GOTO      ENDCT7
109380920320     C                   END
109390930507     C*
109400941102     C* SE IMMESSO IL TIPO IMMETTERE ANCHE L'ORA O LA DATA
109410930507     C     VIDTCR        IFNE      ' '
109420941102     C     VIDDCR        IFEQ      0
109430941102     C     VIDHCR        ANDEQ     0
109440941102     C                   MOVEL     MSG(9)        VIDMSG
109450941102     C                   SETON                                        409028
109460930507     C                   GOTO      ENDCT7
109470930507     C                   END
109480930507     C                   END
109490070308     c
109500070308     c* Se lna  attivata con GEO, non posso inserire solo tipo e ora:
109510070308     c*  data sempre obbligatoria (se inserito in arrivo, accetto
109520070308     c*  dalla partenza)
109530100419     c* Escludo per le causali di ripristino
109540070319     c                   if        FGSgeot='S' and viddcr=0 and vidhcr>0
109550100419     c                             and d48cvb <> 'CA'
109560070308     c                   if        vidtcr<>arbtcr or vidhcr<>arbhcr or
109570070308     c                             comdcr<>arbdcr
109580070308     C                   MOVEL     MSG(140)      VIDMSG
109590070308     C                   SETON                                        409028
109600070308     C                   GOTO      ENDCT7
109610070308     C                   ENDif
109620070308     C                   ENDif
109630930507     C**
109640941102     C* GIORNI DI CHIUSURA
109650941102     C**
109660930507     C     VIDGC1        IFNE      *BLANKS
109670930507     C                   MOVEL     VIDGC1        ACOM              1
109680930507     C                   MOVE      VIDGC1        BCOM              1
109690941212     C                   Z-ADD     1             B                 2 0
109700930507     C     ACOM          LOOKUP    GGG(B)                                 32
109710941102     C     *IN32         IFEQ      *OFF
109720941102     C                   MOVEL     MSG(10)       VIDMSG
109730941102     C                   SETON                                        429028
109740941102     C                   GOTO      ENDCT7
109750941102     C                   ENDIF
109760930507     C*
109770930507     C     BCOM          IFNE      ' '
109780930507     C     BCOM          ANDNE     'P'
109790930507     C     BCOM          ANDNE     'M'
109800941102     C                   MOVEL     MSG(10)       VIDMSG
109810941102     C                   SETON                                        429028
109820930507     C                   GOTO      ENDCT7
109830930507     C                   END
109840041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
109850041130if  2c                   If         *In10 and VidGc1 <> ArbGc1 and
109860041130     c                             (VidGc1 = ' M' or VidGc1 = ' P')
109870041130     c                   Eval      *In28 = *On
109880041130     c                   Eval      *In42 = *On
109890041130     c                   Eval      *In90 = *On
109900041130     c                   Eval      VidMsg = msg(10)
109910041130e   2c                   EndIf
109920930507     C                   END
109930930507     C*
109940930507     C*** 2° GIORNO CHIUSURA ***
109950930507     C     VIDGC2        IFNE      *BLANKS
109960930507     C                   MOVEL     VIDGC2        ACOM              1
109970930507     C                   MOVE      VIDGC2        BCOM              1
109980930507     C                   Z-ADD     1             B
109990930507     C     ACOM          LOOKUP    GGG(B)                                 30
110000941102     C     *IN30         IFEQ      *OFF
110010941102     C                   MOVEL     MSG(10)       VIDMSG
110020941102     C                   SETON                                        439028
110030941102     C                   GOTO      ENDCT7
110040941102     C                   ENDIF
110050930507     C*
110060930507     C     BCOM          IFNE      ' '
110070930507     C     BCOM          ANDNE     'P'
110080930507     C     BCOM          ANDNE     'M'
110090941102     C                   MOVEL     MSG(11)       VIDMSG
110100941102     C                   SETON                                        439028
110110930507     C                   GOTO      ENDCT7
110120930507     C                   END
110130930507     C*
110140930507     C* I GIORNI DI CHIUSURA NON POSSO ESSERE UGUALI
110150930507     C     VIDGC1        IFEQ      VIDGC2
110160941102     C                   MOVEL     MSG(12)       VIDMSG
110170941102     C                   SETON                                        439028
110180930507     C                   GOTO      ENDCT7
110190930507     C                   END
110200930507     C* NON POSSONO ESSERE ' P' ' M'
110210930507     C     VIDGC1        IFEQ      ' M'
110220930507     C     VIDGC2        ANDEQ     ' P'
110230930507     C     VIDGC1        OREQ      ' P'
110240930507     C     VIDGC2        ANDEQ     ' M'
110250941102     C                   MOVEL     MSG(13)       VIDMSG
110260941102     C                   SETON                                        429028
110270930507     C                   GOTO      ENDCT7
110280930507     C                   END
110290041130      * se modifica fatta in arrivo non posso mettere ' M' o ' P'
110300041130if  2c                   If         *In10 and VidGc2 <> ArbGc2 and
110310041130     c                             (VidGc2 = ' M' or VidGc2 = ' P')
110320041130     c                   Eval      *In28 = *On
110330041130     c                   Eval      *In43 = *On
110340041130     c                   Eval      *In90 = *On
110350041130     c                   Eval      VidMsg = msg(10)
110360041130e   2c                   EndIf
110370930507     C                   END
110380941102     C*
110390941102     C* CONSEGNE PARTICOLARi
110400020603     c* Verifico se destinatario disagiato
110410020604     c* per le bolle poste non lo faccio
110420020604     c                   clear                   wdisag
110430020604     c     lnpntw        ifne      'PPT'
110440020603     c                   clear                   tisit0ds
110450020603     c                   movel     'E'           it0tla
110460020603     c                   movel     arbnzd        it0naz
110470020603     c                   movel     arbprd        it0prv
110480020603     c                   movel     arbcad        it0cap
110490020603     c                   movel     arblod        it0loc
110500020603     c                   movel     arbind        it0ind
110510020603     c                   movel     arbrsd        it0rag
110520020603     c                   call      'TISIT5R'
110530020603     c                   parm                    tisit0ds
110540020603     c     ot0err        ifeq      *blanks
110550060203   2ac                   if        ot0dos = 'A' or ot0dos = 'D' or
110560060203     c                             ot0dos = 'S'
110570020603     c                   movel     ot0dos        wdisag
110580020603     c     wdisag        ifeq      'D'
110590020603     c                   movel     'X'           wdisag
110600020603     c                   endif
110610060203     c                   endif
110620020603     c                   else
110630020603     c                   clear                   wdisag
110640020603     c                   endif
110650020604     c                   endif
110660020603     c
110670941102     C*
110680941102     C* 11ON - NON POSO CAMBIARE LA 1 CONSEGNA PARTICOLARE
110690070910     c* Se ho modificato il destinatario è non è stata immessa in
110700070910     c*  partenza, devo per forza toglierla
110710070910     c                   if        *in11=*on and wdisag<>vidtc1 and
110720070910     c                             arbtc1=vidtc1 and par5mdp='S' and
110730070910     c                             par5gtc1<>vidtc1 and par5gtc2<>vidtc1
110740070910     C                   MOVEL     MSG(144)      VIDMSG
110750070910     c                   eval      %subst(vidmsg:27:1) = vidtc1
110760070910     c                   eval      %subst(vidmsg:29:8) = v7dtc1
110770070910     C                   SETON                                        449028
110780070910     C                   GOTO      ENDCT7
110790070910     c                   endif
110800070910     c
110810051116     c****               if        *in11 = *on and wdisag <> *blanks
110820070910     c* se consegna particolare non modificabile ma è stato immesso
110830070910     c* unacons.part.disagiato/supemercat... do' lo stesso la
110840070910     c* possibilita di cambiarla se la combinazione fra le due non è
110850070910     c* ammessa
110860051116     c                   clear                   wcomb
110870070910    0c                   if        *in11=*on and par5mdp='S' and
110880070910     c                             (arbtc1=par5gtc1 or arbtc1=par5gtc2)
110890070910     c
110900070910    1c                   if        vidtc2<>*blanks or wdisag<>*blanks
110910070910     c                             or vidtc1<>arbtc1
110920070910     c*
110930070910     c***                if        *in11 = *on and (vidtc2 <> *blanks or
110940070910     c***                          wdisag <> *blanks or vidtc1 <> arbtc1)
110950051114     c                   move      arbtc1        wtc1
110960051117     c
110970051116     c                   if        wdisag <> *blanks
110980051116     c                   move      wdisag        wtc2
110990051117     c                   exsr      sr_chkcomb
111000051117     c                   endif
111010051117     c                   if        wcomb = *blanks and vidtc1 <> arbtc1
111020051117     c                   move      vidtc1        wtc2
111030051117     c                   exsr      sr_chkcomb
111040051117     c                   endif
111050051117     c                   if        wcomb = *blanks and vidtc2 <> *blanks
111060051117     c                   move      vidtc2        wtc2
111070051117     c                   exsr      sr_chkcomb
111080051117     c                   endif
111090070910    1c                   endif
111100070910     c***
111110070910    0C***  *IN11         IFEQ      *ON
111120070910     c***  wdisag        andeq     *blanks
111130070910    0c***                if        *in11 = *on and wcomb = *blanks
111140070910     c
111150070910    1c                   if        wcomb=*blanks
111160941102     C*
111170070910    2C     VIDTC1        IFNE      ARBTC1
111180941102     C                   MOVEL     MSG(15)       VIDMSG
111190051117     c                   eval      %subst(vidmsg:36:1) = arbtc1
111200941102     C                   SETON                                        449028
111210941102     C                   GOTO      ENDCT7
111220070910    2c                   endif
111230070910    1c                   endif
111240070910    0c                   endif
111250070910     c
111260070910     c***                else
111270070910     C***                MOVEL     '1P'          COD
111280070910     C***                MOVEL(P)  VIDTC1        KEY
111290070910     C***                EXSR      CTABEL
111300070910     C***                MOVEL     TBLUNI        V7DTC1
111310070910    1C***                ENDIF
111320941102     C*
111330070910   X0C***                ELSE
111340941102     C*
111350941102     C                   CLEAR                   V7DTC1
111360941102     C*
111370941102    1C     VIDTC1        IFNE      ' '
111380941102     C* RICERCA
111390941102    2C     VIDTC1        IFEQ      '?'
111400941102     C                   MOVEL     CODUT         §KUT
111410941102     C                   MOVEL     '1P'          §COD
111420941102     C                   MOVE      *BLANKS       VIDTC1
111430991011     C                   CLEAR                   §KEY
111440941102     C                   CALL      'X§TABER'
111450941102     C                   PARM                    §KUT
111460941102     C                   PARM                    §COD
111470941102     C                   PARM                    §KEY
111480941209     C                   PARM                    §DES
111490941102     C                   MOVEL     §KEY          VIDTC1
111500941102     C                   MOVEL     §DES          V7DTC1
111510941102     C                   SETON                                        9044
111520941102     C                   GOTO      ENDCT7
111530941102    2C                   ENDIF
111540941102     C*
111550941102     C                   MOVEL     '1P'          COD
111560941102     C                   MOVEL(P)  VIDTC1        KEY
111570941212     C                   EXSR      CTABEL
111580941102     C*
111590941102    2C     *IN30         IFEQ      *ON
111600941102     C                   MOVEL     MSG(11)       VIDMSG
111610941102     C                   SETON                                        449028
111620941102     C                   GOTO      ENDCT7
111630941102    2C                   ENDIF
111640941102     C*
111650941102     C                   MOVEL     TBLUNI        DS1P
111660941102     C*
111670941102     C* NON UTILIZZABILE IN ARRIVO
111680950104    2C   10§1PUAR        IFNE      'S'
111690020318     C     LNPNTW        ANDNE     'PPT'
111700020318     C     LNPNTW        OREQ      'PPT'
111710000510     C     §1PFPC        ANDNE     'E'
111720000510     C     §1PFPC        ANDNE     'A'
111730040705      * e non è quella presa in automatico
111740051114     c                   If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
111750070914     c* Verifico se era stata messa in partenza ma tolta
111760070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
111770070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
111780941102     C                   MOVEL     MSG(11)       VIDMSG
111790941102     C                   SETON                                        449028
111800941102     C                   GOTO      ENDCT7
111810040705     c                   EndIf
111820070914     c                   EndIf
111830941102    2C                   END
111840060131     c
111850060131     c* SE devo considerare le abilitati, verifico se il profilo
111860060131     c*  può inserire la cons particolare, se non è presa in automatico
111870060131     c   10              If        Vidtc1 <> wdisag and vidtc1 <> arbtc1
111880070914     c                             and  §utemfcp=' '
111890070914     c******                       and dateu>=§vpoacp and  §utemfcp=' '
111900070914     c*
111910070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
111920070914     c                             <>vidtc1 and par5gtc2<>vidtc1)
111930060131     C*
111940060131     C                   MOVEL     'SP'          COD
111950060131     C                   MOVEL(P)  VIDTC1        KEY
111960060131     C                   EXSR      CTABEL
111970060131     C*
111980060131    2C     *IN30         IFEQ      *ON
111990060131     c                   clear                   dssp
112000060131     c                   else
112010060131     c                   movel     tbluni        DSSP
112020060131     c                   endif
112030060131     c
112040060131     c                   if        §spaba='S'
112050060131     C                   MOVEL     MSG(138)      VIDMSG
112060060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
112070060131     C                   SETON                                        449028
112080060131     C                   GOTO      ENDCT7
112090060131     c                   endif
112100060131     c                   endif
112110070914     c
112120070914     c                   endif
112130060131     c
112140950104     C* NON UTILIZZABILE IN PARTENZA
112150950104    2C  N10§1PUPR        IFNE      'S'
112160020318     C     LNPNTW        ANDNE     'PPT'
112170020318     C     LNPNTW        OREQ      'PPT'
112180000510     C     §1PFPC        ANDNE     'E'
112190000510     C     §1PFPC        ANDNE     'P'
112200950104     C                   MOVEL     MSG(11)       VIDMSG
112210950104     C                   SETON                                        449028
112220950104     C                   GOTO      ENDCT7
112230950104    2C                   END
112240000510     C* PER LE POSTE CONTROLLO IL SUO FLAG
112250941102     C*
112260941102     C                   MOVEL     TBLUNI        V7DTC1
112270150414     c* controlli per EXPO
112280151119     c*                  if        vidtc1='Z'
112290151119     c*                  exsr      sr_chkexpo
112300151119     c*                  if        *in90
112310151119     c*                  seton                                        44
112320151119     c*                  goto      endct7
112330151119     c*                  endif
112340151119     c*                  endif
112350941102     C*
112360941102     C* LA 1 E LA 2 NON POSSONO ESSERE UGUALI
112370941102    2C     VIDTC2        IFEQ      VIDTC1
112380941102     C                   MOVEL     MSG(14)       VIDMSG
112390941102     C                   SETON                                        459028
112400941102     C                   GOTO      ENDCT7
112410941102    2C                   END
112420020731     C*
112430941102     C*
112440941102    1C                   END
112450070910    0C******             END
112460941102     C*
112470941102     C* 12ON - NON POSO CAMBIARE LA 2 CONSEGNA PARTICOLARE
112480070910     c* Se ho modificato il destinatario è non è stata immessa in
112490070910     c*  partenza, devo per forza toglierla
112500070910    0c                   if        *in12=*on and wdisag<>vidtc2 and
112510070910     c                             arbtc2=vidtc2 and par5mdp='S' and
112520070910     c                             par5gtc1<>vidtc2 and par5gtc2<>vidtc2
112530070910     C                   MOVEL     MSG(144)      VIDMSG
112540070910     c                   eval      %subst(vidmsg:27:1) = vidtc2
112550070910     c                   eval      %subst(vidmsg:29:8) = v7dtc2
112560070910     C                   SETON                                        459028
112570070910     C                   GOTO      ENDCT7
112580070910    0c                   endif
112590070910     c
112600051114     c* se consegna particolare non modificabile ma è stato immesso
112610051114     c* un destinatrio disagiato/supemercat... do' lo stesso la
112620051114     c* possibilita di cambiarla se la combinazione fra le due non è
112630051114     c* ammessa
112640051116     c                   clear                   wcomb
112650070910    0c                   if        *in12=*on and par5mdp='S' and
112660070910     c                             (arbtc2=par5gtc1 or arbtc2=par5gtc2)
112670070910
112680070910    1c                   if        vidtc1 <> *blanks or
112690070910     c                             wdisag <> *blanks or vidtc2 <> arbtc2
112700070910     c
112710051116     c***                if        *in12 = *on and wdisag <> *blanks
112720070910     c***
112730070910     c***                if        *in12 = *on and (vidtc1 <> *blanks or
112740070910     c***                          wdisag <> *blanks or vidtc2 <> arbtc2)
112750051114     c                   move      arbtc2        wtc1
112760051116     c                   if        wdisag <> *blanks
112770051116     c                   move      wdisag        wtc2
112780051117     c                   exsr      sr_chkcomb
112790051117     c                   endif
112800051117     c                   if        wcomb = *blanks and vidtc2 <> arbtc2
112810051117     c                   move      vidtc2        wtc2
112820051117     c                   exsr      sr_chkcomb
112830051117     c                   endif
112840051117     c                   if        wcomb = *blanks and vidtc1 <> *blanks
112850051117     c                   move      vidtc1        wtc2
112860051117     c                   exsr      sr_chkcomb
112870051117     c                   endif
112880070910    1c                   endif
112890051117     c*
112900070910    0C***  *IN12         IFEQ      *ON
112910070910    0c***                if        *in12 = *on and wcomb = *blanks
112920070910    1c                   if        wcomb = *blanks
112930941102     C*
112940070910    2C     VIDTC2        IFNE      ARBTC2
112950941102     C                   MOVEL     MSG(15)       VIDMSG
112960051117     c                   eval      %subst(vidmsg:36:1) = arbtc2
112970941102     C                   SETON                                        459028
112980941102     C                   GOTO      ENDCT7
112990070910    2c                   endif
113000070910    1c                   endif
113010070910    0c                   endif
113020070910     c
113030070910     c****               else
113040070910     C****               MOVEL     '1P'          COD
113050070910     C****               MOVEL(P)  VIDTC2        KEY
113060070910     C****               EXSR      CTABEL
113070070910     C****               MOVEL     TBLUNI        V7DTC2
113080070910    1C****               ENDIF
113090941102     C*
113100070910   X0C****               ELSE
113110941102     C*
113120941102     C                   CLEAR                   V7DTC2
113130941102     C*
113140941102    1C     VIDTC2        IFNE      ' '
113150941102     C* RICERCA
113160941102    2C     VIDTC2        IFEQ      '?'
113170941102     C                   MOVEL     CODUT         §KUT
113180941102     C                   MOVEL     '1P'          §COD
113190941102     C                   MOVE      *BLANKS       VIDTC2
113200991011     C                   CLEAR                   §KEY
113210941102     C                   CALL      'X§TABER'
113220941102     C                   PARM                    §KUT
113230941102     C                   PARM                    §COD
113240941102     C                   PARM                    §KEY
113250941209     C                   PARM                    §DES
113260941102     C                   MOVEL     §KEY          VIDTC2
113270941102     C                   MOVEL     §DES          V7DTC2
113280941102     C                   SETON                                        9045
113290941102     C                   GOTO      ENDCT7
113300941102    2C                   ENDIF
113310941102     C*
113320941102     C                   MOVEL     '1P'          COD
113330941102     C                   MOVEL(P)  VIDTC2        KEY
113340941212     C                   EXSR      CTABEL
113350941102     C*
113360941102    2C     *IN30         IFEQ      *ON
113370941102     C                   MOVEL     MSG(11)       VIDMSG
113380941102     C                   SETON                                        459028
113390941102     C                   GOTO      ENDCT7
113400941102    2C                   ENDIF
113410941102     C*
113420941102     C                   MOVEL     TBLUNI        DS1P
113430941102     C*
113440941102     C* NON UTILIZZABILE IN ARRIVO
113450950104    2C   10§1PUAR        IFNE      'S'
113460020318     C     LNPNTW        ANDNE     'PPT'
113470020318     C     LNPNTW        OREQ      'PPT'
113480000518     C     §1PFPC        ANDNE     'E'
113490000518     C     §1PFPC        ANDNE     'A'
113500040705      * e non è quella presa in automatico
113510051114     c                   If        Vidtc2 <> wdisag  and vidtc2 <> arbtc2
113520070914     c* Verifico se era stata messa in partenza ma tolta
113530070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113540070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
113550941102     C                   MOVEL     MSG(11)       VIDMSG
113560941102     C                   SETON                                        459028
113570941102     C                   GOTO      ENDCT7
113580040705     c                   EndIf
113590070914     c                   EndIf
113600941102    2C                   END
113610060131     c
113620060131     c* SE devo considerare le abilitati, verifico se il profilo
113630060131     c*  può inserire la cons particolare, se non è presa in automatico
113640060131     c   10              If        Vidtc2 <> wdisag and vidtc2 <> arbtc2
113650070914     c                             and  §utemfcp=' '
113660070514     c****                         and dateu>=§vpoacp and  §utemfcp=' '
113670070914     c* Verifico se era stata messa in partenza ma tolta
113680070914     c                   if        par5mdp=' ' or (par5mdp='S' and par5gtc1
113690070914     c                             <>vidtc2 and par5gtc2<>vidtc2)
113700060131     C*
113710060131     C                   MOVEL     'SP'          COD
113720060131     C                   MOVEL(P)  VIDTC2        KEY
113730060131     C                   EXSR      CTABEL
113740060131     C*
113750060131    2C     *IN30         IFEQ      *ON
113760060131     c                   clear                   dssp
113770060131     c                   else
113780060131     c                   movel     tbluni        DSSP
113790060131     c                   endif
113800060131     c
113810060131     c                   if        §spaba='S'
113820060131     C                   MOVEL     MSG(138)      VIDMSG
113830060131     c                   eval      %subst(vidmsg:59:1) = vidtc1
113840060131     C                   SETON                                        459028
113850060131     C                   GOTO      ENDCT7
113860060131     c                   endif
113870060131     c                   endif
113880070914     c                   endif
113890070914     c
113900950104     C* NON UTILIZZABILE IN PARTENZA
113910950104    2C  N10§1PUPR        IFNE      'S'
113920020318     C     LNPNTW        ANDNE     'PPT'
113930020318     C     LNPNTW        OREQ      'PPT'
113940000518     C     §1PFPC        ANDNE     'E'
113950000518     C     §1PFPC        ANDNE     'P'
113960950104     C                   MOVEL     MSG(11)       VIDMSG
113970950104     C                   SETON                                        459028
113980950104     C                   GOTO      ENDCT7
113990950104    2C                   END
114000941102     C*
114010941102     C                   MOVEL     TBLUNI        V7DTC2
114020150414     c* controlli per EXPO
114030151119     c*                  if        vidtc2='Z'
114040151119     c*                  exsr      sr_chkexpo
114050151119     c*                  if        *in90
114060151119     c*                  seton                                        45
114070151119     c*                  goto      endct7
114080151119     c*                  endif
114090151119     c*                  endif
114100941102     C*
114110941102    1C                   END
114120070910    0C****               END
114130060131     c
114140020531     c* Se destinatario disagiato o supermercato errore se non indicato
114150020531     c* in nessuno dei due campi di consegna particolare
114160041015     c*
114170041015     c* Se particolarità varia="U" --> consegna ad uffici devo invece
114180041015     c* dare errore se immesso supermercato o appuntamento
114190041015     c                   if        %subst(arbgva:1:1)='U'
114200041015     c                             and (vidtc1='S' or vidtc1='A' or vidtc2='S'
114210041015     c                             or vidtc2='A')
114220041015     C                   MOVEl     msg(131)      vidmsg
114230041015     C                   SETON                                        449028
114240041015     C                   GOTO      ENDCT7
114250041015     c                   endif
114260150210
114270150210     c* se sono in partenza e immessa la "A" e la data --> errore solo la data immettere
114280150210     c                   if        not *in10 and (vidtc1='A' or vidtc2='A') and
114290150210     c                             comdcr>0
114300150210     c                   if        (par5mdp='S' and par5gtc1<>'A' and par5gtc2
114310150210     c                                                    <> 'A')
114320150210     c                             or (par5mdp=' ' and arbtc1<>'A' and arbtc2
114330150210     c                                                    <> 'A')
114340150210     C                   MOVEl     msg(158)      vidmsg
114350150210     C                   SETON                                        449028
114360150210     C                   GOTO      ENDCT7
114370150210     c                   endif
114380150210     c                   endif
114390150119
114400150119     c* se destinatario particolare che prevede appuntamento controllo se presente
114410150119     c* DCR immessa dalla partenza perchè in questo caso non devo dare errore per la
114420150119     c* mancanza della "A" di appuntamento
114430150119     c                   eval      w_noapp=' '
114440150119     c                   if        wdisag='A'
114450150119     c                   if        (par5mdp='S' and par5gdcr>*zeros
114460150119     c                                          and par5gtcr=' ')
114470150119     c                             or (par5mdp=' ' and arbdcr>0 and arbtcr= ' ')
114480150119     c                   eval      w_noapp='1'
114490150119     c                   endif
114500150119     c                   endif
114510041015     c*
114520041015     C***  wdisag        ifne      *blanks
114530041015     c***  wdisag        andne     vidtc1
114540041015     c***  wdisag        andne     vidtc2
114550150119     c                   if        wdisag <> *blanks and w_noapp=' ' and
114560041015     c                             %subst(arbGva:1:1) <> 'U' or
114570041015     c                             wdisag='X' and %subst(arbGva:1:1)='U'
114580041015     c     wdisag        ifne      vidtc1
114590041015     c     wdisag        andne     vidtc2
114600020603     C                   MOVEa     msg(112)      k78
114610020531     c                   select
114620020531     c     wdisag        wheneq    'X'
114630020531     c                   eval      w014a = 'disagiato     '
114640020531     c     wdisag        wheneq    'S'
114650020531     c                   eval      w014a = '"supermercato"'
114660040705     c     wdisag        wheneq    'A'
114670040705     c                   eval      w014a = '"appuntamento"'
114680020531     c                   endsl
114690020531     C                   MOVEa     w014a         k78(14)
114700020531     C                   MOVEA     K78           VIDMSG
114710020603     C                   SETON                                        449028
114720020531     C                   GOTO      ENDCT7
114730020531     c                   endif
114740041015     c                   endif
114750070907     C* Non è possibile indicare contemporaneamente alcune consegne    i
114760070907     C*  particolari
114770051114     c                   move      vidtc1        wtc1
114780051114     c                   move      vidtc2        wtc2
114790051114     c                   exsr      sr_chkcomb
114800051114     c                   if        wcomb = 'S'
114810051114     C                   movel     MSG(113)      VIDmsg
114820051116     c                   eval      %subst(vidmsg:63:1) = vidtc1
114830051116     c                   eval      %subst(vidmsg:69:1) = vidtc2
114840051114     C                   seton                                        459028
114850051114     C                   goto      ENDCT7
114860051114    2C                   ENDIF
114870941102     C*
114880920320     C     ENDCT7        ENDSR
114890021129      **
114900021129      *--- CONTROLLI CAMPI CONSEGNA RICHIESTA SUPERMERCATI -----------*
114910021129     C     CONTR71       BEGSR
114920021129     C                   SETOFF                                       90
114930021129
114940030123      * Se la spedizione non ha già la consegna particolare 'S'
114950030123     c                   If        ArbTc1 <> 'S' and ArbTc2 <> 'S'
114960030123      * E' obbligatorio inserire la consegna
114970030123     c                   If        (VidTc1 = *Blanks and VidTc2 = *Blanks)
114980030123     c                   Eval      VidMsg = Msg(117)
114990030123     c                   Eval      *In44 = *On
115000030123     c                   Eval      *In28 = *On
115010030123     c                   Eval      *In90 = *On
115020030123     c                   GoTo      EndCt71
115030030123     c                   EndIf
115040030127      * Se ci sono già tutte e 2 almeno una deve essere 'S'
115050030127     c                   If        VidTc1 <> 'S' and VidTc2 <> 'S'
115060030127     c                   Eval      VidMsg = Msg(122)
115070030127     c                   Eval      *In44 = *On
115080030127     c                   Eval      *In28 = *On
115090030127     c                   Eval      *In90 = *On
115100030127     c                   GoTo      EndCt71
115110030127     c                   EndIf
115120030123      * Se la prima consegna c'è già e non è supermercato devo mettere 'S' nella seconda
115130030127     c                   If        ArbTc1 <> *Blanks and VidTc1 <> 'S' and
115140030127     c                             VidTc2 = *Blanks
115150030123     c                   Eval      VidMsg = Msg(117)
115160030123     c                   Eval      *In45 = *On
115170030123     c                   Eval      *In28 = *On
115180030123     c                   Eval      *In90 = *On
115190030123     c                   GoTo      EndCt71
115200030123     c                   EndIf
115210030123      * Se la seconda consegna c'è già e non è supermercato devo mettere 'S' nella prima
115220030127     c                   If        ArbTc2 <> *Blanks and VidTc2 <> 'S' and
115230030127     c                             VidTc1 = *Blanks
115240030123     c                   Eval      VidMsg = Msg(117)
115250030123     c                   Eval      *In45 = *On
115260030123     c                   Eval      *In28 = *On
115270030123     c                   Eval      *In90 = *On
115280030123     c                   GoTo      EndCt71
115290030123     c                   EndIf
115300021129     c                   EndIf
115310021129
115320021129      * Se è stata inserita la consegna particolare 'S' si devono
115330021129      * immettere anche la persona da contattare, il telefono e il motivo
115340021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
115350021129      * Nominativo
115360021129     c                   If        VidNom = *Blanks
115370021129     c                   Eval      VidMsg = Msg(118)
115380021129     c                   Eval      *In57 = *On
115390021129     c                   Eval      *In28 = *On
115400021129     c                   Eval      *In90 = *On
115410021129     c                   GoTo      EndCt71
115420021129     c                   EndIf
115430021129      * Telefono
115440021129     c                   If        VidTel = *Blanks
115450021129     c                   Eval      VidMsg = Msg(119)
115460021129     c                   Eval      *In58 = *On
115470021129     c                   Eval      *In28 = *On
115480021129     c                   Eval      *In90 = *On
115490021129     c                   GoTo      EndCt71
115500041007     c**                 Else
115510041007     c**                 Clear                   Trul42Ds
115520041007     c**                 Movel     VidTel        D42Fax
115530041007     c**                 Call      'TRUL42R'
115540041007     c***                Parm                    Trul42Ds
115550041007     c***                If        D42Err = '1'
115560041007     c****               Eval      *In58 = *On
115570041007     c**                 Eval      *In28 = *On
115580041007     c**                 Eval      *In90 = *On
115590041007     c**                 Eval      VidMsg = D42Msg
115600041007     c**                 GoTo      EndCt71
115610041007     c**                 EndIf
115620041007     c                   EndIf
115630021129      * Motivo
115640070406      * non più obbligatorio per evitare che scrivano "bagianate"
115650070406     c**                 If        VidMot = *Blanks
115660070406     c***                Eval      VidMsg = Msg(120)
115670070406     c**                 Eval      *In59 = *On
115680070406     c**                 Eval      *In28 = *On
115690070406     c**                 Eval      *In90 = *On
115700070406     c**                 GoTo      EndCt71
115710070406     c**                 EndIf
115720021129     c                   EndIf
115730021129
115740021129     c     EndCt71       EndSr
115750030930      **
115760030930      *--- CONTROLLO SE POSSIBILE LA MANUTENZIONE DELLA DATA CONSEGNA RICHIESTA ---*
115770030930     c     Contr72       BegSr
115780050907
115790030930     c                   Eval      *In90 = *Off
115800070306     c                   clear                   aggioarp          1
115810050907
115820030930      * Se tipo o data o ora consegna richiesta sono state variate
115830031001If  1c                   If        VidTcr <> ArbTcr or
115840031030     c                             ComDcr <> ArbDcr or
115850030930     c                             VidHcr <> ArbHcr
115860150205     c
115870150205     c* permetto sempre la variazione della cons richiesta se sono in partenza
115880160115     c                   if        not *in10 or wfile='P'
115890150205     c                   Goto      EndCt72
115900150205     c                   EndIf
115910150205     c
115920051109      * Se causale 'CS' e giacenza aperta in fase 30 con appuntamento (sulla giacenza)
115930051109      * posso variare la data
115940051109      * quindi bypasso tutti gli altri controlli
115950051109     c                   If        d48cvb = 'CS' and *In04 and wgcfas = 30 and
115960051109     c                             d§gcpapp = 'A'
115970051109     c                   Eval      wgiac = 'S'
115980051109     c                   Goto      EndCt72
115990051109     c                   EndIf
116000040224      * non posso modificare se la bolla ha giacenza aperta (IN04 ON)
116010050907      * e se la fase non è = a 35 o se la bolla è bloccata come giacenza
116020050907if  2c                   If        *In04 and (wgcfas <> 35 or arbfbc = 'G')
116030070312     c                   Eval      VidMsg = Msg(141)
116040040220     c                   Eval      *In28 = *On
116050040220     c                   Eval      *In90 = *On
116060040220     c                   If        VidTcr <> ArbTcr
116070040220     c                   Eval      *In62 = *On
116080040220     c                   GoTo      EndCt72
116090040220     c                   EndIf
116100040220     c                   If        ComDcr <> ArbDcr
116110040220     c                   Eval      *In40 = *On
116120040220     c                   GoTo      EndCt72
116130040220     c                   EndIf
116140040220     c                   If        VidHcr <> ArbHcr
116150040220     c                   Eval      *In41 = *On
116160040220     c                   GoTo      EndCt72
116170040220     c                   EndIf
116180040220    2c                   EndIf
116190040324      * posso modificare se la bolla ha giacenza aperta (IN04 ON) e la fase è = 35
116200040330      * ma solo se metto la data consegna richiesta maggiore/uguale alla data eseguibilità giacenza
116210040324if  2c                   If        *In04 and wgcfas = 35
116220040330if  3c                   If        wgcded > *Zeros and ComDcr >= wgcded
116230040324     c                   Eval      wgiac = 'S'
116240040324     c                   GoTo      EndCt72
116250040324    3c                   EndIf
116260040330if  3c                   If        wgcded = *Zeros and ComDcr >= wgcddm
116270040324     c                   Eval      wgiac = 'S'
116280040324     c                   GoTo      EndCt72
116290040324    3c                   EndIf
116300040324     c                   Eval      VidMsg = Msg(128)
116310040324     c                   Eval      *In28 = *On
116320040324     c                   Eval      *In90 = *On
116330040324     c                   If        VidTcr <> ArbTcr
116340040324     c                   Eval      *In62 = *On
116350040324     c                   GoTo      EndCt72
116360040324     c                   EndIf
116370040324     c                   If        ComDcr <> ArbDcr
116380040324     c                   Eval      *In40 = *On
116390040324     c                   GoTo      EndCt72
116400040324     c                   EndIf
116410040324     c                   If        VidHcr <> ArbHcr
116420040324     c                   Eval      *In41 = *On
116430040324     c                   GoTo      EndCt72
116440040324     c                   EndIf
116450040324    2c                   EndIf
116460050907      * richiamo il controllo della "7Q"
116470050907     c                   ExSr      Contr7q
116480030930      * posso modificare se
116490031001      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
116500031001If  2c                   If        V1cPkf >= §VpoPkg or
116510031001     c                             V1cVlf >= §VpoVlm or
116520031001      *                     Destinatario supermercato o Apputamento
116530031001     c                             VidTc1 = 'S' or VidTc1 = 'A' or
116540031001     c                             VidTc2 = 'S' or VidTc2 = 'A' or
116550150506     c                             VidTc1 = 'Z' or VidTc2 = 'Z' or
116560031001      *                     Lasciato avviso
116570031001     c                             ArbFbc = 'A' or
116580031001      *                     Fermo deposito
116590031114     c                             ArbFfd = 'S' or
116600031114      *                     Tipo bolla "FW" Recapito c/assegni
116610050907     c                             ArbCbo = 'FW' or
116620050907      *                     Periodo trazione ridotte e bolla con particolarità "G8"
116630050907     c                             wDCRG8 = 'S'
116640031001     c                   GoTo      EndCt72
116650031001    2c                   EndIf
116660070320     c
116670070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
116680070320     c*  manutenzionare la spedizione
116690070920     c***                Clear                   dAr5Gen
116700070920     c***                Eval      kAr5Trd = 'GEN'
116710070920     c***  kAr5          Chain(n)  Fiar501l
116720070920     c***                If        %Found(Fiar501l)
116730070920     c***                Movel     Ar5Uni        dAr5Gen
116740070920     c***                if        §ar5MDP='S'  and §ar5FFD='S'
116750070920     c                   if        Wesar5gen='S' and par5mdp='S' and
116760070920     c                             par5FFD='S'
116770070320     c                   GoTo      EndCt72
116780070320    2c                   EndIf
116790070920    2c***                EndIf
116800070312    c
116810070312     c* Se la bolla non è in distinta, non c'e' blocco giacenza e c'e'
116820070312     c*  un evento che prevede la modifica della cons, richiesta
116830070312     c*  allora posso inserirla ed aggiornare direttamente ARB+fiar5
116840070319     c                   if        arbndc=0 and FGSgeot='S' and
116850070312     c                             arbfbc<>'G' and eveADR='S'
116860070312     c                   GoTo      EndCt72
116870070312    3c                   EndIf
116880070312     c
116890031001      *                     C/assegno non abilitato
116900031001If  2c                   If        Ar9Cas > 0
116910031001     c                   Clear                   Trul21ds
116920031001     c                   Eval      I21Tla = 'L'
116930031001     c                   Eval      I21Cbo = ArbCbo
116940031001     c                   Eval      I21Tsp = ArbTsp
116950031001     c                   Eval      I21Lnp = ArbLnp
116960031001     c                   Eval      I21Nzm = ArbNzm
116970031001     c                   Eval      I21Lna = ArbLna
116980031001     c                   Eval      I21Nzd = ArbNzd
116990130827     c****               Eval      I21Ksc = ArbKsc
117000031001     c                   Movel     ArbCtr        I21Ctr
117010031001     c                   Eval      I21Tic = Ar9Tic
117020031001     c                   Eval      I21Imp = Ar9Cas
117030031001     c                   Eval      I21Div = Ar9Vca
117040031001     c                   Eval      I21Ute = Knmus
117050031001     c                   Eval      I21Pgm = nompgm
117060130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
117070130827     c                   if        flgtb1='F'
117080130827     c                   z-add     arbksc        i21ksc
117090130827     c                   else
117100130827     c                   z-add     arbccm        i21ksc
117110130827     c                   if        i21ksc=0
117120130827     c                   z-add     8888          i21ksc
117130130827     c                   movel     arblnp        i21ksc
117140130827     c                   endif
117150130827     c                   endif
117160031001     c                   Call      'TRUL21R'
117170031001     c                   Parm                    Trul21ds
117180031001If  3c                   If        O21Fca <> *Blanks
117190031001     c                   GoTo      EndCt72
117200031001    3c                   EndIf
117210031001    2c                   EndIf
117220070309     c
117230070309     c* Se la bolla è in distinta, non devo però aggiornare
117240070306     c*  fnarb ma FIARP
117250111025    2c                   if        arbndc>0 and FGSgeot='S'
117260111025     c
117270111025     c* Se posso, scrivo FIARP, altrimenti subiro errore
117280111025    3c                   if        DcrinDist<>'S'
117290111025      * erorre
117300111025     c                   Eval      VidMsg = Msg(126)
117310111025     c                   Eval      *In28 = *On
117320111025     c                   Eval      *In90 = *On
117330111025     c                   Eval      *In40 = *On
117340111025     c                   GoTo      EndCt72
117350111025     c
117360111025   x3c                   else
117370070308     c                   eval      Aggioarp='S'
117380111025    4c                   if        comdcr<>savdcr  or vidtcr<>savtcr or
117390070308     c                             vidhcr<>savhcr
117400070309     c* Se richiamo nascosto, imposto solo il campo msgforza,
117410070309     c*  altrimenti emetto il msg
117420070330     c*  non imposto il campo se devo fare solo i controlli (26 OFF)
117430111025    5c                   if        d48ffr<>'E'
117440070308     c                   seton                                        402890
117450070308     c                   Eval      VidMsg = Msg(139)
117460111025   x5c                   else
117470070330     c
117480070330     c                   if        *in26
117490070330     c                   eval      msgforza=142
117500070309     c                   endif
117510111025    5c                   endif
117520070309     c
117530070308     c                   eval      savdcr=comdcr
117540070308     c                   eval      savtcr=vidtcr
117550070308     c                   eval      savhcr=vidhcr
117560111025    4c                   endif
117570111025    3c                   endif
117580070308     c
117590070306     c                   GoTo      EndCt72
117600111025    2c                   endif
117610070306     c
117620040220      * se arrivo qua vuol dire che non posso modificare
117630140806      * Se variazione da internet permetto la modifica
117640140806     c                   if        d48cvb<>'CI'
117650031001     c                   Eval      VidMsg = Msg(126)
117660031001     c                   Eval      *In28 = *On
117670031001     c                   Eval      *In90 = *On
117680031030     c                   If        VidTcr <> ArbTcr
117690031001     c                   Eval      *In62 = *On
117700031001     c                   GoTo      EndCt72
117710031001     c                   EndIf
117720031030     c                   If        ComDcr <> ArbDcr
117730031001     c                   Eval      *In40 = *On
117740031001     c                   GoTo      EndCt72
117750031001     c                   EndIf
117760031001     c                   If        VidHcr <> ArbHcr
117770031001     c                   Eval      *In41 = *On
117780031001     c                   GoTo      EndCt72
117790031001     c                   EndIf
117800140806     c                   endif
117810031001    1c                   EndIf
117820030930
117830030930     c     EndCt72       EndSr
117840920320     C*
117850920320     C*--- AGGIORNAMENTO FILE ----------------------------------------*
117860920320     C     REGIS7        BEGSR
117870070308     c                   clear                   ModconsR          1
117880130412     c                   clear                   wpda_ndc
117890130412     c                   clear                   wpda_ddc
117900130412     c                   clear                   wpda_dcm
117910070308     C* PRENDO I VALORI ASSOLUTI
117920070308     C                   MLLZO     1             VIDHCR
117930070308     c
117940070308     c* Mi salvo i campi della consegna
117950070308     c*  richiesta della bolla per sucessivi aggiornamenti.
117960070308     c                   eval      bolladcr=arbdcr
117970070308     c                   eval      bollahcr=arbhcr
117980070308     c                   eval      bollatcr=arbtcr
117990070308     c
118000070308     c* Mi salvo anche se modificata, solo se c'e' la DATA consegna impostat
118010070308     c                   if        vidtcr<>arbtcr or comdcr<>arbdcr or
118020070308     c                             arbhcr<>vidhcr  and comdcr>0
118030070308     c                   eval      ModconsR='S'
118040070308     c                   endif
118050070308     c
118060950104     C*  GIRO UDATE
118070950104     C                   TIME                    W0140            14 0
118080950104     C* UDATE IN GGMMAAAA
118090950104     C                   MOVE      W0140         WDTGIO            8 0
118100950104     C* UDATE IN AAAAMMGG
118110950104     C                   Z-ADD     WDTGIO        G02DAT
118120950104     C                   MOVEL     *BLANK        G02ERR
118130950104     C                   CALL      'XSRDA8'
118140950104     C                   PARM                    WLBDAT
118150950104     C                   MOVEL     G02INV        DATEU             8 0
118160941213     C                   MOVEL     KNMUS         ARBPRU
118170930507     C*
118180151230     c                   if        not *in10 or wfile='A'
118190930507     C* INVIO PER SEDE
118200941014     C                   MOVEL     'FNARBG0T'    SFILE
118210930507     C* ALLOCO
118220930507     C                   EXSR      ALLOC
118230930507     C   91              GOTO      ENDRG7
118240070320     c
118250070320     c* Se impostata la "S" o la "A" devo impostare relativo blocco
118260070320     c*  telefonate in DSBL4L
118270070320     c                   clear                   wesar4L           1
118280070320     c                   clear                   BAS               1
118290070320     c                   clear                   TFD               1
118300070320     c                   if        ((vidtc1='S' or vidtc2='S') and
118310070320     c                              arbtc1<>'S' and arbtc2<>'S')  or
118320070320     c                             ((vidtc1='A' or vidtc2='A') and
118330070320     c                              arbtc1<>'A' and arbtc2<>'A')
118340070320     c                   eval      BAS='S'
118350070320     c                   endif
118360070320     c
118370070320     c* se tolto "A" o "S" devo eliminare il blocco
118380070320     c                   if        vidtc1<>'A' and vidtc1<>'S' and
118390070320     c                              vidtc2<>'A' and vidtc2<>'S' and
118400070320     c                             (arbtc1='S' or arbtc1='A' or
118410070320     c                              arbtc2='S' or arbtc2='A')
118420070320     c                   eval      BAS='N'
118430070320     c                   endif
118440070320     c* Tolgo blocco telefonata FFD se messa consegna richiesta e
118450070320     c*  presente FFD
118460070329     c****               if        arbffd='S' and viddcr>0
118470070329     c****               eval      TFD='N'
118480070329     c****               endif
118490070320     c                   if        bas<>' ' or TFD<>' '
118500070320     c                   eval      TipoEla='C'
118510070320     c                   exsr      Gestar4L
118520070320    1c                   if        *in91
118530070515     c                   exsr      DLCSED
118540070525     C                   SETON                                        2891
118550070320     C                   GOTO      ENDRG7
118560070320    1c                   endif
118570070320     c                   endif
118580041022     c
118590041022     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
118600041022     c* iornare direttamente il file
118610041022     c                   clear                   waltrabolla       1
118620041022     C   10KARB          CHAIN     FNBLP01L                           3091
118630041022     C  n10KARB          CHAIN     FNARB01L                           3091
118640041022     c*
118650070308    1c                   if        *in91
118660070515     c                   exsr      DLCSED
118670070515     c                   unlock    fiar401l
118680070525     C                   SETON                                        2891
118690041022     C                   MOVEL     MSG(4)        VIDMSG
118700041022     C                   GOTO      ENDRG7
118710070308    1c                   endif
118720070308     c
118730070308     c* Se lna attivata con GEO
118740070308     c* Se ho aggiornato FIARP, e devo aggiornare altri dati,
118750070308     c* nei campi della consegna richeista reimposto quelli della bolla
118760070319    1c                   if        FGSgeot='S'and AggioARP='S'
118770070308     c                   eval      comdcr=Bolladcr
118780070308     c                   eval      vidhcr=bollahcr
118790070308     c                   eval      vidtcr=Bollatcr
118800070308     c                   clear                   ModConsR
118810070308     c                   endif
118820070308     c
118830041022     c                   if        *in30
118840041022     c                   eval      waltrabolla='N'
118850041022     c                   else
118860041022     c* memorizzo campi prima della variazione
118870041022     c                   eval      waltrotc1=arbtc1
118880041022     c                   eval      waltrotc2=arbtc2
118890080422     c   10              eval      waltroft3=arbft3
118900130411     c  n10              eval      wpda_ndc=arbndc
118910130411     c  n10              eval      wpda_ddc=arbddc
118920130411     c  n10              eval      wpda_dcm=arbdcm
118930141223     c  n10              eval      bollaFBC=arbfbc
118940041022     C   10KARB          CHAIN     FNARB01L                           30
118950041022     C  N10KARB          CHAIN     FNBLP01L                           30
118960041022     c* Alla fine per non sporcarmi i campi del record, richaino il
118970041022     c*  record originale che sto variando
118980130411     c                   eval      wpda_ndc=arbndc
118990130411     c                   eval      wpda_ddc=arbddc
119000130411     c                   eval      wpda_dcm=arbdcm
119010041022     c                   endif
119020151230     c                   endif
119030930507     C*
119040930507     C                   Z-ADD     DATEU         ARBDTV
119050930507     C                   TIME                    ARBORV
119060930507     C                   MOVEL     VIDCVR        ARBCVB
119070001204     C*
119080001204     C                   MOVEL     ARBTC1        SAVTC1
119090001204     C                   MOVEL     ARBTC2        SAVTC2
119100080415     c*
119110930507     C*
119120060130     C* SE DA STORICIZZARE
119130060130     C**   *IN10         IFEQ      *ON
119140060130    1C**   D66FSA        ANDEQ     'S'
119150060130     C**   *IN10         OREQ      *OFF
119160060130    1C**   D66FSP        ANDEQ     'S'
119170070308     c
119180060130    1C     D66FSA        ifeq      'S'
119190011108      *
119200011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
119210011108      * è in partenza 'P' o in arrivo 'A'
119220011108     C     *IN10         IFEQ      *ON
119230160119     c                   if        wfile='P'
119240160119     C                   MOVE      'a'           ARBPRU
119250160119     c                   else
119260011108     C                   MOVE      'A'           ARBPRU
119270160119     c                   endif
119280011108     C                   ENDIF
119290011108     C     *IN10         IFEQ      *OFF
119300011108     C                   MOVE      'P'           ARBPRU
119310011108     C                   ENDIF
119320011108      *
119330941014     C                   WRITE     FNARBG00
119340930507     C                   END
119350060301     c* storicizzazione motivazione
119360060301     c     d66mtv        ifeq      'S'
119370060301     c                   exsr      sto_mtv
119380060301     c                   endif
119390040224      * se in arrivo ho modificato uno dei dati del Fiar5 rcd GEN:
119400040224      * - Tipo/Data/Ora consegna richiesta
119410040224      * - Giorni di chiusura
119420040224      * - Consegna particolare
119430040224      * devo memorizzare i dati della bolla prima della modifica
119440150129     c******             If        *In10 and
119450150206     c* dal 06/02 momorizziamo anche se varia la partenza, come se lo facesse l'arrivo
119460150129     c                   if         (VidTcr <> ArbTcr or ComDcr <> ArbDcr or
119470040224     c                               VidHcr <> ArbHcr or VidGc1 <> ArbGc1 or
119480040224     c                               VidGc2 <> ArbGc1 or VidTc1 <> ArbTc1 or
119490040224     c                               VidTc2 <> ArbTc2)
119500041022     c                   ExSr      RegAr5AR
119510040223     c                   EndIf
119520080415     c*
119530080415     c* Se modifico data consegna richiesta verifico se devo sflaggare per
119540080415     c* clienti
119550080415     c                   eval      wsflagga_C=' '
119560080422     c                   if        comdcr<>arbdcr or
119570080415     c                             vidtcr<>arbtcr
119580080415     c                   clear                   wcmitt
119590080415     c                   if        flgtb1='A'
119600080415     c                   move      arbccm        wcmitt
119610080415     c                   else
119620080415     c                   move      arbksc        wcmitt
119630080415     c                   endif
119640080416     c                   if        wcmitt>'0000000'
119650080416     c                             and %subst(wcmitt:4:4)<>'8888'
119660080415     C                   MOVEL     '3K'          COD
119670080415     C                   MOVEL(P)  wcmitt        KEY
119680080415     C                   EXSR      CTABEL
119690080415     C  N30              MOVEL     TBLUNI        DS3K
119700080415     C   30              CLEAR                   DS3K
119710080415     c                   if        §3kcq8='S'
119720080415     c                   eval      wsflagga_C='S'
119730080415     c                   endif
119740080415     c                   endif
119750080415     c                   endif
119760041022     c
119770041022     c* Se variazione in partenza e modifico  le cons.partic
119780041022     c*  sflaggo per clienti
119790041025     c  n10              if        vidtc1<>arbtc1 or vidtc2<>arbtc2
119800080415     c                             or wsflagga_c='S'
119810041022     c                   clear                   arbft3
119820041022     c                   endif
119830070319     c
119840070319     c* Se impostata la "S" o la "A" devo impostare relativo blocco
119850070319     c*  telefonate in DSBL4L
119860151230     c                   if        not *in10 or wfile='A'
119870070320     c                   if        wesar4L<>' '
119880070320     c                   if        BAS='S'
119890070320     c                   eval      §b4lbas='S'
119900070320     c                   endif
119910070320     c                   if        BAS='N'
119920070320     c                   eval      §b4lbas=' '
119930070320     c                   endif
119940070320     c
119950070320     c                   if        TFD='N'
119960070320     c                   eval      §b4lTFD=' '
119970070320     c                   endif
119980070320     c
119990070320     c                   eval      TipoEla='A'
120000070320     c                   exsr      GestAR4L
120010070320     c                   endif
120020151230     c                   endif
120030070320     c
120040920320     C*
120050920320     C                   MOVEL     COMDCR        ARBDCR
120060920320     C                   MOVEL     VIDHCR        ARBHCR
120070920320     C                   MOVEL     VIDTCR        ARBTCR
120080930507     C                   MOVEL     VIDGC1        ARBGC1
120090930507     C                   MOVEL     VIDGC2        ARBGC2
120100941102     C                   MOVEL     VIDTC1        ARBTC1
120110941102     C                   MOVEL     VIDTC2        ARBTC2
120120070308     c
120130070308     c* Se modificata consegna richiesta in arrivo
120140070308     c*  devo anche pulire il flag blocco consegna se = 'A'
120150141223     c                   if        BollaFBC='A' and ModconsR='S'
120160070308     c                             and comdcr>0
120170070308     c                   clear                   arbfbc
120180070419     c                   clear                   arbcmc
120190070308     c                   endif
120200070920     c
120210070920     c* Se modificata la consegna richiesta e bolla già consegnata
120220070920     c* effettuo il ricalcolo della affidabilità
120230070920     c                   if        ModconsR='S' and arbdcr=0 and arbdcm>0
120240070920     c                   clear                   arbdti
120250070920     c                   clear                   arbhti
120260070920     c                   movel     'S'           arbfbs
120270070920     c                   endif
120280041025     c
120290070319     c* Verifico se devo aggiornare record già esistente in ar5
120300150206     c*  ma solo per le consegne particolari e fermo deposito
120310150206     c  N10              exsr      RegAr5PA
120320941212     C*
120330930507     C* PER SEDE
120340151230     c                   if        not *in10 or wfile='A'
120350070503     C     D66FFI        IFEQ      'S'
120360950227     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
120370950413    2C     §3ABSD        IFEQ      ' '
120380930507     C*
120390941014     C                   OPEN      FNARBG0T
120400941014     C                   WRITE     FNARBGTT
120410941014     C                   CLOSE     FNARBG0T
120420950227     C                   END
120430930507     C* DISALLOCO OGGETTO
120440941212     C                   EXSR      DLCSED                                       FNARBG0T
120450950227     C                   END
120460151230     c                   endif
120470041022     c
120480930507     C*
120490151230     c                   if        wfile='A'
120500151230     C                   UPDATE    FNARB000
120510151230     c                   else
120520151230     C                   UPDATE    FNBLP000
120530151230     c                   endif
120540941220     C*
120550941220     C* DISALLOCO I RECORD CHE NON USO
120560160119     C   10              UNLOCK    FIAR601L
120570041022     c* Aggiorno anche l'altra bolla
120580151230     c                   if        not *in10 or wfile='A'
120590041022     c                   if        waltrabolla=' '
120600080422     c   10              eval      arbft3=waltroft3
120610041025     c* SE aggiorno bopartenza sflaggo per clienti se variate le cons.partic
120620041025     c                   if        *in10 and (waltrotc1<>arbtc1 or
120630080415     c                             waltrotc2<>arbtc2 or
120640080415     c                             wsflagga_c='S')
120650041022     c                   clear                   arbft3
120660041022     c                   endif
120670041022     C   10              except    arbGblp
120680041022     C  N10              except    arbGarb
120690041022     c                   endif
120700151230     c                   endif
120710950602     C*
120720070308     c* Se non è previsto l'aggiornamento di FIARP, vedo se lo devo
120730070308     c*  cancellare (se variata la data consegna richiesta)
120740070319     c                   if        FGSgeot='S'  and  ModconsR='S'
120750070308     c     karb          delete    fiarp01l
120760070308     c                   endif
120770070308     c
120780040224      * Se c'è la data consegna richiesta controllo se è ancora valida
120790040224      * solo se ho fatto una modifica in arrivo
120800150129      * da adesso anche se modifica in partenza
120810150206     c                   If        ArbDcr > *Zeros
120820040324     c                             and wCa = *Blanks and wgiac = *Blanks
120830040224     c                   ExSr      ContrDcr
120840040224     c                   EndIf
120850070308     c
120860130411     C*
120870130411     c* Richiamo routine per invio variazione a pda
120880130411     c                   if        D48Cvb<>'CS'
120890151230     c                             and (not *in10 or wfile='A')
120900130411     c                   exsr      sr_pda
120910130411     c                   endif
120920950104     C*
120930930507     C     ENDRG7        ENDSR
120940021129      **
120950021129      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
120960021129     c     Regis71       BegSr
120970021129
120980021129      * Se è stata indicata la consegna particolare 'S' devo scrivere
120990021129      * il file FIAR5
121000021129     c                   If        VidTc1 = 'S' or VidTc2 = 'S'
121010021129     c                   Clear                   Fiar5000
121020021129     c                   Eval      Ar5Aas = ArbAas
121030021129     c                   Eval      Ar5Lnp = ArbLnp
121040021129     c                   Eval      Ar5Nrs = ArbNrs
121050021129     c                   Eval      Ar5Nsp = ArbNsp
121060021129     c                   Eval      Ar5Trd = 'SCR'
121070021129     c                   Move      Dateu         Ar5Dac
121080021129     c                   Movel     W0140         Ar5Orc
121090021129     c                   Clear                   Dar5Scr
121100021129     c                   Eval      §ar5Cvb = D48Cvb
121110021129     c                   Eval      §ar5Pru = Knmus
121120060131     c                   Eval      §ar5Pou = dutPou
121130021129     c                   Eval      §ar5Tcr = VidTcr
121140021129     c                   Eval      §ar5Dcr = ComDcr
121150021129     c                   Eval      §ar5Hcr = VidHcr
121160021129     c                   Eval      §ar5Gc1 = VidGc1
121170021129     c                   Eval      §ar5Gc2 = VidGc2
121180021129     c                   Eval      §ar5Tc1 = VidTc1
121190021129     c                   Eval      §ar5Tc2 = VidTc2
121200021129     c                   Eval      §ar5Nom = VidNom
121210021129     c                   Eval      §ar5Tel = VidTel
121220021129     c                   Eval      §ar5Mot = VidMot
121230021129     c                   Movel     Dar5Scr       Ar5Uni
121240021129     c                   Write     Fiar5000
121250021129     c                   EndIf
121260040705
121270040705      * Aggiorno anche il record 'GEN' di Fiar5 con il nuovo Telefono e Referente
121280091217     c                   eval      Wtipoagg='REF'
121290091217     c                   exsr      RegisAR5GEN
121300091217     c***
121310091217     c***                Eval      kAr5Trd = 'GEN'
121320091217     c***                Clear                   dAr5Gen
121330091217     c***  kAr5          Chain     Fiar501l
121340040705      * Aggiorno Fiar5
121350091217if  1c***                If        %Found(Fiar501l)
121360091217     c***                Eval      dAr5Gen = Ar5Uni
121370091217     c***                Eval      §Ar5Ref = VidNom
121380091217     c***                Eval      §Ar5Teld = VidTel
121390091217     c***                Movel(p)  dAr5Gen       Ar5Uni
121400040705      * se la modifica è fatta in partenza e devo trasmettere all'arrivo
121410040705      * sfleggo per la trasmissione
121420070306     c***                If        Not *In10 and §Ar5Trar = 'S'
121430070306     c***                Clear                   Ar5Ft1
121440070306     c***                EndIf
121450040705      * se la modifica è fatta in arrivo e devo trasmettere alla partenza
121460040705      * sfleggo per la trasmissione
121470070306     c***                If        *In10 and §Ar5Trpa = 'S'
121480070306     c***                Clear                   Ar5Ft1
121490070306     c***                EndIf
121500040705      * se devo trasmettere alla sede sfleggo per la trasmissione
121510091217     c***                If        §Ar5Trse = 'S'
121520091217     c***                Clear                   Ar5Ft2
121530091217     c***                EndIf
121540040705      * sfleggo sempre per la trasmissione al cliente
121550091217     c***                Clear                   Ar5Ft3
121560091217     c***                Update    Fiar5000
121570040705      * Scrivo Fiar5
121580091217   x1c***                Else
121590040705      * se immessi i dati
121600091217     c***                Clear                   Fiar5000
121610091217     c***                Eval      Ar5Aas = ArbAas
121620091217     c***                Eval      Ar5Lnp = ArbLnp
121630091217     c***                Eval      Ar5Nrs = ArbNrs
121640091217     c***                Eval      Ar5Nsp = ArbNsp
121650091217     c***                Eval      Ar5Trd = 'GEN'
121660091217     c***                Move      Dateu         Ar5Dac
121670091217     c***                Movel     W0140         Ar5Orc
121680091217     c***                Clear                   Dar5Gen
121690091217     c***                Eval      §Ar5Ref = VidNom
121700091217     c***                Eval      §Ar5Teld = VidTel
121710091217     c***                Movel(p)  DAr5Gen       Ar5Uni
121720091217     c***                Write     Fiar5000
121730091217    1c***                EndIf
121740130411     c* Richiamo routine per invio variazione a pda
121750130411     c                   exsr      sr_pda
121760021129
121770021129     c                   EndSr
121780040223
121790041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in arrivo--*
121800041022     c     RegAr5AR      BegSr
121810070309     c* Se Attiva GEO e bolla non in distnta e non giacente con
121820070309     c*  un evento che prevede la modifica della cons.richista
121830070309     c*  memorizzo la cons richiesta nei campi di FIAR5
121840070309     c                   clear                   AggioAR5M         1
121850070309     c
121860070319     c                   if        arbndc=0 and FGSgeot='S' and
121870070309     c                             arbfbc<>'G' and eveADR='S'
121880070309     c                   eval      AggioAR5M='S'
121890070309     c                   endif
121900160114     c* Se manutenzione in Arrivo su bolla ancora da partire memorizzo
121910160114     c* di aggiornare i dati del ripristino con quelli del video
121920160114     c                   if        *in10 and wfile = 'P'
121930160114     c                   eval      AggioAR5M='S'
121940160114     c                   endif
121950040223
121960040224     c                   Clear                   waggar5
121970040223     c                   Clear                   dAr5Gen
121980040223     c                   Eval      kAr5Trd = 'GEN'
121990040223     c     kAr5          Chain     Fiar501l
122000040223     c                   If        %Found(Fiar501l)
122010040223     c                   Movel     Ar5Uni        dAr5Gen
122020040223     c                   EndIf
122030040223
122040040223      * Dati non memorizzati
122050040223if  1c                   If        §Ar5Mdp <> 'S'
122060040224     c                   Eval      §Ar5Mdp = 'S'
122070040223     c                   Eval      §Ar5gTcr = ArbTcr
122080040223     c                   Move      ArbDcr        §Ar5gDcr
122090040223     c                   Move      ArbHcr        §Ar5gHcr
122100040223     c                   Eval      §Ar5gGc1 = ArbGc1
122110040223     c                   Eval      §Ar5gGc2 = ArbGc2
122120040223     c                   Eval      §Ar5gTc1 = ArbTc1
122130040223     c                   Eval      §Ar5gTc2 = ArbTc2
122140040223     c                   Eval      §Ar5Ffd  = ArbFfd
122150040223      * Causale 'CD' da chiusura distinta
122160140806      *   oppure
122170140806      * Causale 'CI' da indicazioni da internet
122180040223      * imposto i campi a video
122190140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
122200040223     c                   Eval      §Ar5Tcrm = VidTcr
122210040223     c                   Move      ComDcr        §Ar5Dcrm
122220040223     c                   Move      VidHcr        §Ar5Hcrm
122230040223      * Altra causale
122240040223      * imposto i campi di arb prima della variazione
122250040223   x2c                   Else
122260040223     c                   Eval      §Ar5Tcrm = ArbTcr
122270040223     c                   Move      ArbDcr        §Ar5Dcrm
122280040223     c                   Move      ArbHcr        §Ar5Hcrm
122290040223    2c                   EndIf
122300040224     c                   Eval      waggar5 = 'S'
122310040223      * Dati memorizzati
122320040223   x1c                   Else
122330140806if  2c                   If        d48cvb = 'CD' or d48cvb='CI' or AggioAR5M='S'
122340040223     c                   Eval      §Ar5Tcrm = VidTcr
122350040223     c                   Move      ComDcr        §Ar5Dcrm
122360040223     c                   Move      VidHcr        §Ar5Hcrm
122370040224     c                   Eval      waggar5 = 'S'
122380040223    2c                   EndIf
122390040223    1c                   EndIf
122400150206     c* se si tratta di una modifica di cons richiesta in partenza lo memorizzo se
122410150206     c*  non l'ho già fatto
122420150206     c                   if        not *in10 and comdcr<>arbdcr and
122430150206     c                             §ar5dcrp<>'P'  and comdcr>0
122440150206     c                   eval      §ar5dcrp= 'P'
122450150206     c                   Eval      waggar5 = 'S'
122460150206    2c                   EndIf
122470150206     c                   if        not *in10 and comdcr<>arbdcr and
122480150206     c                             §ar5dcrp= 'P'  and comdcr=0
122490150206     c                   eval      §ar5dcrp= ' '
122500150206     c                   Eval      waggar5 = 'S'
122510150206    2c                   EndIf
122520150206     c
122530040223
122540040224if  1c                   If        waggar5 = 'S'
122550040224      * aggiorno Fiar5
122560040224if  2c                   If        %Found(Fiar501l)
122570040224     c                   Movel(p)  dAr5Gen       Ar5Uni
122580040224      * se devo trasmettere alla sede sfleggo per la trasmissione
122590040224     c                   If        §Ar5Trse = 'S'
122600040224     c                   Clear                   Ar5Ft2
122610040224     c                   EndIf
122620040224      * sfleggo sempre per la trasmissione al cliente
122630040224     c                   Clear                   Ar5Ft3
122640040223     c                   Update    Fiar5000
122650040224      * scrivo Fiar5
122660040224   x2c                   Else
122670040224     c                   Clear                   Fiar5000
122680040223     c                   Eval      Ar5Aas = ArbAas
122690040223     c                   Eval      Ar5Lnp = ArbLnp
122700040223     c                   Eval      Ar5Nrs = ArbNrs
122710040223     c                   Eval      Ar5Nsp = ArbNsp
122720040224     c                   Eval      Ar5Trd = 'GEN'
122730040223     c                   Move      Dateu         Ar5Dac
122740040223     c                   Movel     W0140         Ar5Orc
122750040224     c                   Movel(p)  dAr5Gen       Ar5Uni
122760041007     c* flaggo sempre il flag di filiale tanto non trasmettiamo più
122770040223     c                   Write     Fiar5000
122780040224    2c                   EndIf
122790040224   x1c                   Else
122800040224     c                   Unlock    Fiar501l
122810040224    1c                   EndIf
122820040223
122830040223     c                   EndSr
122840041022
122850041022      *--- CONTROLLO E AGGIORNAMENTO FIAR5 RCD GEN se sono in partenz-*
122860041022     c     RegAr5PA      BegSr
122870041022     c                   Clear                   dAr5Gen
122880041022     c                   Eval      kAr5Trd = 'GEN'
122890041022     c     kAr5          Chain     Fiar501l
122900041022if  2c                   If        %Found(Fiar501l)
122910041022     c                   Movel     Ar5Uni        dAr5Gen
122920041022      * Flag dati memorizzati
122930041022if  3c                   If        §Ar5Mdp = 'S'
122940041022      * Aggiorno i dati di ripristino solo se uguali a quelli salvati
122950150206if  4c***                If        §Ar5gTcr= §Ar5Tcrm and §Ar5gDcr = §Ar5Dcrm
122960150206     c***                          and §Ar5gHcr = §Ar5Hcrm
122970150206     c***                Eval      §Ar5Tcrm = ArbTcr
122980150206     c***                Move      ArbDcr        §Ar5Dcrm
122990150206     c***                Move      ArbHcr        §Ar5Hcrm
123000150206    4c***                EndIf
123010041022      * Memorizzo i dati della bolla
123020150206     c***                Eval      §Ar5gTcr = ArbTcr
123030150206     c***                Move      ArbDcr        §Ar5gdcr
123040150206     c***                Move      ArbHcr        §Ar5gHcr
123050150206     c***                Eval      §Ar5gGc1 = ArbGc1
123060150206     c***                Eval      §Ar5gGc2 = ArbGc2
123070150206     c* Da 02/15
123080150206     c* come partenza aggiorno soltanto i dati del fermo deposito e dei giorni di chiusura
123090041022     c                   Eval      §Ar5gTc1 = ArbTc1
123100041022     c                   Eval      §Ar5gTc2 = ArbTc2
123110041022     c                   Eval      §Ar5Ffd = arbFfd
123120041022    3c                   EndIf
123130041022     c                   Movel     Dar5Gen       Ar5Uni
123140041022     c                   Update    Fiar5000
123150041022    2c                   EndIf
123160041022     c                   EndSr
123170070906
123180070906      *--- CONTROLLO SE ANCORA VALIDA Le consegne particolari --------*
123190070906     c     ContrTCP      BegSr
123200070910      *
123210070906     c                   clear                   eliminatcp        1
123220070911     c                   clear                   wcomb
123230070911     c                   clear                   aggtcp            1
123240070906     c
123250070910    1c                   if        arbtc1=wdisagdest
123260070910if  2c                   If        (par5Mdp = 'S' and
123270070910     c                             par5gtc1<>arbtc1 and par5gtc2<>arbtc1)
123280070911     c*******                      or   par5Mdp=' '
123290070906     c                   eval      eliminatcp='1'
123300070911     c                   eval      wtc1=arbtc1
123310070907    2c                   endif
123320070910    1c                   endif
123330070910    1c                   if        arbtc2=wdisagdest
123340070910if  2c                   If        (par5Mdp = 'S' and
123350070910     c                             par5gtc1<>arbtc2 and par5gtc2<>arbtc2)
123360070911     c********                     or   par5Mdp=' '
123370070906     c                   eval      eliminatcp='2'
123380070911     c                   eval      wtc1=arbtc2
123390070907    2c                   endif
123400070910    1c                   endif
123410070906     c* Se da eliminare
123420070910    1c                   if        eliminatcp<>' '
123430070911     c* Verifico se per le combinazioni devo ripristinare la cons partic
123440070911     c*  della partenza
123450070911     c                   if        par5mdp='S' and par5gtc1<>' '
123460070911     c                   eval      wtc2=par5gtc1
123470070911     c                   exsr      sr_chkcomb
123480070911     c
123490070911     c                   if        wcomb<>' '
123500070911     c                   eval      aggtcp=par5gtc1
123510070911     c                   endif
123520070912     c                   endif
123530070911     c
123540070911     c                   if        wcomb=' ' and par5gtc2<>' '
123550070911     c                   eval      wtc2=par5gtc1
123560070911     c                   exsr      sr_chkcomb
123570070911     c                   if        wcomb<>' '
123580070911     c                   eval      aggtcp=par5gtc2
123590070911     c                   endif
123600070911     c                   endif
123610070911     c
123620070906     c                   Clear                   dsarbg
123630070906     c                   Eval      d48cvb = 'CA'
123640070906     c                   Eval      weseg = 'E'
123650070911     c*****              Clear                   d48ffr
123660070906     c                   Eval      d48Trc = 'G'
123670070906     c                   Eval      §bgTcr = arbtcr
123680070906     c                   Move      arbdcr        §bgDcr
123690070906     c                   Move      arbhcr        §bgHcr
123700070906     c                   Eval      §bgGc1 = Arbgc1
123710070906     c                   Eval      §bgGc2 = ArbGc2
123720070910    2c                   if        eliminatcp='1'
123730070906     c                   clear                   §bgtc1
123740070906     c                   clear                   arbtc1
123750070906     c                   Eval      §bgtc2 = Arbtc2
123760070911     c* ripristino la vecchia consegna partic.
123770070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
123780070911     c                   eval      §bgtc1=aggtcp
123790070911     c                   endif
123800070910   x2c                   else
123810070906     c                   clear                   §bgtc2
123820070906     c                   clear                   arbtc2
123830070906     c                   Eval      §bgtc1 = Arbtc1
123840070911     c* ripristino la vecchia consegna partic.
123850070911     c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
123860070911     c                   if        §bgtc1=' '
123870070911     c                   eval      §bgtc1=aggtcp
123880070911     c                   else
123890070911     c                   eval      §bgtc1=aggtcp
123900070911     c                   endif
123910070911     c                   endif
123920070910    2c                   endif
123930070907     c
123940070907     c* Memorizzo in skiera rielaborazioni
123950070910    2c                   if        kk<=10
123960070907     c                   add       1             kk
123970070907     c                   movel     dsarbg        kdati(KK)
123980070907     c                   movel     d48cvb        kcvb(KK)
123990070907     c                   movel     d48trc        ktrc(KK)
124000070911     c                   movel     ' '           kffr(KK)
124010070911     c                   movel     ' '           kf12(KK)
124020070910    2c                   endif
124030070910    1c                   endif
124040070906     c
124050070906     c                   EndSr
124060040224
124070040224      *--- CONTROLLO SE ANCORA VALIDA LA DATA CONSEGNA RICHIESTA -----*
124080160114     c     ContrDcr      BegSr
124090040225
124100040225     c                   Move      ArbDcr        warbdcr
124110050907      * richiamo il controllo della "7Q"
124120050907     c                   ExSr      Contr7q
124130040225
124140040225      * Controllo se c'è il fiar5 rcd GEN
124150070906     c                   clear                   dar5gen
124160040225     c                   Eval      kAr5Trd = 'GEN'
124170040225     c     kAr5          Chain(n)  Fiar501l
124180040225     c                   If        %Found(Fiar501l)
124190040225     c                   Movel     Ar5Uni        dAr5Gen
124200040225     c                   EndIf
124210040225      * se ho memorizzato i dati
124220040225if  1c                   If        §Ar5Mdp = 'S' and
124230040225      * e la data è diversa
124240040225     c                             (wArbDcr <> §Ar5Dcrm or
124250040225     c                               ArbTcr <> §Ar5Tcrm)
124260040225      * controllo se la data è valida quando:
124270040224      *                     Peso o volume da fatturare sono >= a quanto tabellato in VPO
124280040225if  2c                   If        ArbPkf >= §VpoPkg or
124290040224     c                             ArbVlf >= §VpoVlm or
124300150506      *                     Destinatario supermercato o Appuntamento
124310040224     c                             ArbTc1 = 'S' or ArbTc1 = 'A' or
124320040224     c                             ArbTc2 = 'S' or ArbTc2 = 'A' or
124330150506     c                             ArbTc1 = 'Z' or ArbTc2 = 'Z' or
124340040312      *                     Lasciato avviso
124350070308     c                             BollaFbc = 'A' or
124360040224      *                     Fermo deposito
124370040224     c                             ArbFfd = 'S' or
124380040224      *                     Tipo bolla "FW" Recapito c/assegni
124390040811     c                             ArbCbo = 'FW'or
124400040811      *                     Periodo trazione ridotte e bolla con particolarità "G8"
124410040811     c                             wDCRG8 = 'S'
124420040225     c                   Goto      EndContr
124430040225    2c                   EndIf
124440040224      *                     C/assegno non abilitato
124450040225if  2c                   If        Ar9Cas > 0
124460040224     c                   Clear                   Trul21ds
124470040224     c                   Eval      I21Tla = 'L'
124480040224     c                   Eval      I21Cbo = ArbCbo
124490040224     c                   Eval      I21Tsp = ArbTsp
124500040224     c                   Eval      I21Lnp = ArbLnp
124510040224     c                   Eval      I21Nzm = ArbNzm
124520040224     c                   Eval      I21Lna = ArbLna
124530040224     c                   Eval      I21Nzd = ArbNzd
124540130827     c*****              Eval      I21Ksc = ArbKsc
124550040224     c                   Movel     ArbCtr        I21Ctr
124560040224     c                   Eval      I21Tic = Ar9Tic
124570040224     c                   Eval      I21Imp = Ar9Cas
124580040224     c                   Eval      I21Div = Ar9Vca
124590040224     c                   Eval      I21Ute = Knmus
124600040224     c                   Eval      I21Pgm = nompgm
124610130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
124620130827     c                   if        flgtb1='F'
124630130827     c                   z-add     arbksc        i21ksc
124640130827     c                   else
124650130827     c                   z-add     arbccm        i21ksc
124660130827     c                   if        i21ksc=0
124670130827     c                   z-add     8888          i21ksc
124680130827     c                   movel     arblnp        i21ksc
124690130827     c                   endif
124700130827     c                   endif
124710040224     c                   Call      'TRUL21R'
124720040224     c                   Parm                    Trul21ds
124730040225if  3c                   If        O21Fca <> *Blanks
124740040225     c                   Goto      EndContr
124750040225    3c                   EndIf
124760040225    2c                   EndIf
124770070320     c
124780070320     c* Se FFD immesso dalla partenza, anche se già tolto, posso
124790070320     c*  manutenzionare la spedizione
124800160114    1c                   if        §ar5MDP='S'  and §ar5FFD='S'
124810070320     c                   GoTo      EndContr
124820070911    1c                   EndIf
124830150206     c* se la cons richiesta non è stata immessa dalla partenza e sono in partenza
124840150206     c*  ripristino
124850160114     c                   if        (not *in10 and  §ar5dcrp= 'P')
124860160114     c                             or (*in10 and wfile='P')
124870150206     c                   GoTo      EndContr
124880150206    1c                   EndIf
124890150206     c
124900040225      * in questo caso devo reimpostare la data consegna richiesta come indicato in Fiar5
124910040225      * facendo una variazione di tipo 'CA'
124920040225      * senza farla vedere a video
124930040225     c                   Clear                   dsarbg
124940040225     c                   Eval      d48cvb = 'CA'
124950040225     c                   Eval      weseg = 'E'
124960070911     c*****              Clear                   d48ffr
124970040225     c                   Eval      d48Trc = 'G'
124980040324     c                   Eval      §bgTcr = §Ar5Tcrm
124990040324     c                   Move      §Ar5Dcrm      §bgDcr
125000040324     c                   Move      §Ar5Hcrm      §bgHcr
125010040315     c                   Eval      §bgGc1 = Arbgc1
125020040315     c                   Eval      §bgGc2 = ArbGc2
125030040315     c                   Eval      §bgTc1 = ArbTc1
125040040315     c                   Eval      §bgTc2 = ArbTc2
125050040225     c                   Eval      wCA = 'S'
125060070906     c* Se da eliminare anche la consegna particolare imposto
125070070906     c*  insieme la variazione
125080070911    1c                   if        eliminatcp='1'
125090070906     c                   clear                   §bgtc1
125100070911     c* ripristino la vecchia consegna partic.
125110070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc2
125120070911     c                   eval      §bgtc1=aggtcp
125130070911    2c                   endif
125140070911    1c                   endif
125150070911    1c                   if        eliminatcp='2'
125160070906     c                   clear                   §bgtc2
125170070911    2c                   if        aggtcp<>' '  and aggtcp<>§bgtc1
125180070911    3c                   if        §bgtc1=' '
125190070911     c                   eval      §bgtc1=aggtcp
125200070911     c                   else
125210070911     c                   eval      §bgtc2=aggtcp
125220070911    3c                   endif
125230070911    2c                   endif
125240070911    1c                   endif
125250070906     c
125260070906     c* Memorizzo in skiera rielaborazioni
125270070906     c                   if        kk<=10
125280070907     c* se devo eliminare una consegna particolare, memorizzo nella
125290070907     c* stessa variazione
125300070907     c                   if        eliminatcp=' '
125310070907     c                   add       1             kk
125320070907     c                   endif
125330070906     c                   movel     dsarbg        kdati(KK)
125340070906     c                   movel     d48cvb        kcvb(KK)
125350070907     c                   movel     d48trc        ktrc(KK)
125360070911     c                   movel     ' '           kffr(KK)
125370070911     c                   movel     ' '           kf12(KK)
125380070906     c                   endif
125390070906     c
125400040225    1c                   EndIf
125410040224
125420040225     c     EndContr      EndSr
125430040224
125440070307     C*--- AGGIORNAMENTO FILE proposte FIARP00F ----------------------*
125450070307     C     REGISARP      BEGSR
125460070307     C                   MLLZO     1             VIDHCR
125470070307     c     karb          chain     fiarp01l
125480070307     c                   clear                   fiarp000
125490070307     c                   eval      arpaas=arbaas
125500070307     c                   eval      arplnp=arblnp
125510070307     c                   eval      arpnrs=arbnrs
125520070307     c                   eval      arpnsp=arbnsp
125530070307     c                   eval      arptcr=vidtcr
125540070307     c                   eval      arpdcr=comdcr
125550070307     c                   eval      arphcr=vidhcr
125560070308     C*
125570070308     C                   TIME                    W0140            14 0
125580070308     C* UDATE IN AAAAMMGG
125590070308     C                   MOVE      W0140         WDTGIO            8 0
125600070308     C                   Z-ADD     WDTGIO        G02DAT
125610070307     C                   MOVEL     *BLANK        G02ERR
125620070307     C                   CALL      'XSRDA8'
125630070307     C                   PARM                    WLBDAT
125640070307     C                   MOVEL     G02INV        arpdim            8 0
125650070307     c                   time                    arpoim
125660070308     C                   MOVEL     KNMUS         ARPPRU
125670070307     c                   if        %found(fiarp01l)
125680070307     c                   update    fiarp000
125690070307     c                   else
125700070307     c                   write     fiarp000
125710070307     c                   endif
125720070307     c
125730070307     c                   ENDSR
125740941223     C*
125750941223     C*--- CONTROLLI CAMPI MITTENTE ----------------------------------*
125760941223     C     CONTR8        BEGSR
125770941223     C                   SETOFF                                       9028
125780950104     C                   CLEAR                   VIDFAP
125790941223     C*
125800941223     C* CONTROLLO IL CODICE DEL MITTENTE
125810970813     C                   EXSR      CTRCOM
125820970822     C*
125830970822     C* SE PASSO DA CODIFICATO AD 8888 O VICEVERSA SPROTEGGO O PROTEGGO
125840970822     C* CAMPI DELL'INDIRIZZO
125850970822     C     VIDKSC        IFEQ      8888
125860970822     C     MEMKSC        ANDNE     8888
125870970822     C                   MOVE      VIDKSC        MEMKSC
125880970822     C                   SETOFF                                       22
125890970822     C                   SETON                                        90
125900970822     C                   END
125910970822     C*
125920970822     C     VIDKSC        IFNE      8888
125930970822     C     MEMKSC        ANDEQ     8888
125940970822     C                   MOVE      VIDKSC        MEMKSC
125950970822     C                   SETON                                        2290
125960970822     C                   CLEAR                   VIDRSM
125970970822     C                   CLEAR                   VIDINM
125980970822     C                   CLEAR                   VIDCAM
125990970822     C                   CLEAR                   VIDLOM
126000970822     C                   CLEAR                   VIDPRM
126010970822     C                   CLEAR                   VIDNZM
126020970822     C                   CLEAR                   VIDPIM
126030970822     C                   END
126040950113     C**
126050950113     C     *IN90         IFEQ      *ON
126060950113     C  N48              GOTO      ENDCT8
126070950113     C*
126080950113     C* X IL CODICE TARIFFA DO ERRORE SOLO SE NON E' STATO PREMUTO F10
126090950114     C  NKJ
126100950114     CAN 28              GOTO      ENDCT8
126110950113     C                   ENDIF
126120941223     C*
126130020508     C* SE IMMESSO UN CODICE 8888 OBBLIGATORIO l'indirizzo completo
126140020508    1C     VIDKSC        IFEQ      8888
126150970808     C                   MOVEL     VIDRSM        I14RSC
126160970808     C                   CLEAR                   I14RS2
126170970808     C                   MOVEL     VIDINM        I14IND
126180970808     C                   MOVEL     VIDLOM        E14LOC
126190970808     C                   MOVEL     VIDPIM        E14PIP
126200970808     C                   MOVEL     VIDPRM        E14PRV
126210970808     C                   MOVEL     VIDNZM        E14NAR
126220970808     C                   MOVEL     VIDCAM        E14CAP
126230970808     C                   CLEAR                   I14ISO
126240970808     C                   MOVEL     'M'           WCLI              1
126250970808     C                   EXSR      CTRIND
126260941223     C*
126270941223     C* CAMPI CHE POSSO AVER IMPOSTATO
126280970808     C                   MOVEL     E14LOC        VIDLOM
126290970808     C                   MOVEL     E14PIP        VIDPIM
126300970808     C                   MOVEL     E14PRV        VIDPRM
126310970808     C                   MOVEL     E14NAR        VIDNZM
126320970808     C                   MOVEL     E14CAP        VIDCAM
126330950104     C* SALVO IL FLAG ANTEPORTO
126340970808     C                   MOVEL     O95ISO        COMFAP
126350970808     C                   MOVEL     O95ISO        VIDFAP
126360140403     c* salvo pickup 2
126370140403     C                   MOVEL     O95gf2        comgf2
126380941223     C**
126390970808     C* ERRORE
126400970808     C   90              GOTO      ENDCT8
126410950522     C* P.IVA obbligatoria se nazione estera e c'è flag efta in
126420950522     C* tabella nazione
126430950522     C     WDESIE        IFEQ      'E'
126440950522     C     DESEFT        ANDNE     *BLANKS
126450950412     C     VIDPIM        ANDEQ     *BLANKS
126460950412     C                   MOVEL     MSG(67)       VIDMSG
126470950412     C                   SETON                                        284990
126480950412     C                   GOTO      ENDCT8
126490950412     C                   END
126500941223     c*
126510941223   X1C                   ELSE
126520950104     c*
126530950104     C* VEDO SE ESISTE CNIND
126540950104     C     KACO          CHAIN     CNIND000                           32
126550950104     C*
126560950104     C     *IN32         IFEQ      *ON
126570950104     C                   MOVEL     MSG(63)       VIDMSG
126580950104     C                   SETON                                        479028
126590950104     C                   GOTO      ENDCT8
126600950104     C                   ENDIF
126610950104     C*
126620950104     C* CONTROLLO SUL CAP PROVINCIA NAZIONE
126630970423     C     INDSTA        IFEQ      *BLANKS
126640970423     C     INDPRV        ANDEQ     *BLANKS
126650950104     C                   MOVEL     MSG(64)       VIDMSG
126660950104     C                   SETON                                        479028
126670950104     C                   GOTO      ENDCT8
126680950104     C                   ENDIF
126690950104     C* CAP OBBLIGATORIO : CONTROLLO SE CONGRUENZA CON PROVINCIA
126700030612     C                   CLEAR                   Tisi95DS
126710030612     C                   CLEAR                   Fnlv13DS
126720970811     C                   MOVEL     INDSTA        I95NAR
126730970811     C                   MOVEL     INDCAE        I95CAP
126740970811     C                   MOVEL     INDPRV        I95PRV
126750970811     C                   MOVEL     INDCIT        I95LOC
126760970811     C                   MOVEL     '3'           I95TCN
126770970811     C                   MOVEL     ARBDSP        I95DAT
126780970811     C* ERRORE PER AFFIDABILITA' = 0
126790970811     C                   MOVEL     'S'           I13AF0
126800970811     C* CONTROLLO CONGRUENZA CAP/PROVINCIA
126810970811     C                   MOVEL     'S'           I13CNG
126820970811     C* CONTROLLO NON FORZABILE CITTA' CON VIARIO
126830970811     C                   MOVEL     'S'           I13CNV                         CAP CON VIAR
126840020305     c     lnpntw        ifeq      'FED'
126850020305     C                   movel     'S'           i95fi1
126860020305     c                   endif
126870970811     C                   CALL      'FNLV13R'
126880970811     C                   PARM                    KPJBA
126890030612     C                   PARM                    Fnlv13DS
126900030612     C                   PARM                    Tisi95DS
126910950104     C* ERRORE
126920970811     C     O13ERR        IFNE      *BLANKS
126930970811     C                   MOVEL     O13MSG        VIDMSG
126940950104     C                   MOVE      CPDC          VIDMSG
126950950104     C                   SETON                                        904728
126960950104     C                   GOTO      ENDCT8
126970950104    3C                   ENDIF
126980950104     C* SALVO IL FLAG ANTEPORTO
126990970811     C                   MOVEL     O95ISO        COMFAP
127000950104     C* SOLO IN VISUALIZZAZIONE
127010970811     C                   MOVEL     O95ISO        VIDFAP
127020140403     c* salvo pickup 2
127030140403     C                   MOVEL     O95gf2        comgf2
127040950104    2C                   ENDIF
127050941223     C*
127060941227     C     ENDCT8        ENDSR
127070961218     C*
127080941223     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE MT -----------------*
127090941227     C     REGIS8        BEGSR
127100130412     c                   clear                   wpda_ndc
127110130412     c                   clear                   wpda_ddc
127120130412     c                   clear                   wpda_dcm
127130950113     C                   MOVEL     ARBRSM        OLDRSM
127140950113     C                   MOVEL     ARBKSC        OLDKSC
127150950113     C                   MOVEL     ARBCAM        OLDCAM
127160041027     C                   MOVEL     ARBcpi        savCpi
127170950104     C*  GIRO UDATE
127180950104     C                   TIME                    W0140            14 0
127190950104     C* UDATE IN GGMMAAAA
127200950104     C                   MOVE      W0140         WDTGIO            8 0
127210950104     C*
127220950104     C* UDATE IN AAAAMMGG
127230950104     C                   Z-ADD     WDTGIO        G02DAT
127240950104     C                   MOVEL     *BLANK        G02ERR
127250950104     C                   CALL      'XSRDA8'
127260950104     C                   PARM                    WLBDAT
127270950104     C                   MOVEL     G02INV        DATEU             8 0
127280950104     C*
127290941223     C                   MOVEL     KNMUS         ARBPRU
127300950104     C*
127310950104     C* SALVO ARBCCM DELLA BOLLA PERCHE LO USO
127320950104     C                   MOVEL     ARBCCM        W0070             7 0
127330941223     C*
127340941223     C* INVIO PER SEDE
127350941223     C                   MOVEL     'FNARBM0T'    SFILE
127360941223     C* ALLOCO
127370941223     C                   EXSR      ALLOC
127380941223     C* ERRORI IN ALLOCAZIONE
127390941223    1C     *IN91         IFEQ      *ON
127400941223     C                   GOTO      ENDRG8
127410941223    1C                   ENDIF
127420041025     c
127430041025     c* alloco record bolla in altro file
127440041025     c                   clear                   waltrabolla       1
127450041025     C   10KARB          CHAIN     FNBLP01L                           3091
127460041025     C  n10KARB          CHAIN     FNARB01L                           3091
127470041025     c*
127480041025     c                   if        *in91
127490041025     C                   MOVEL     MSG(4)        VIDMSG
127500070515     c                   exsr      DLCSED
127510070525     C                   SETON                                        2891
127520041025     C                   GOTO      ENDRG8
127530041025     c                   else
127540041025     c                   if        *in30
127550041025     c                   eval      waltrabolla='N'
127560041025     c                   else
127570041025     c                   eval      waltrocpi=arbcpi
127580041025     c                   eval      waltrofvf=arbfvf
127590041025     c                   eval      waltrovlf=arbvlf
127600130411     c  n10              eval      wpda_ndc=arbndc
127610130411     c  n10              eval      wpda_ddc=arbddc
127620130411     c  n10              eval      wpda_dcm=arbdcm
127630041025     C   10KARB          CHAIN     FNARB01L                           30
127640041025     C  N10KARB          CHAIN     FNBLP01L                           30
127650041025     c* Alla fine per non sporcarmi i campi del record, richaino il
127660041025     c*  record originale che sto variando
127670130411     c   10              eval      wpda_ndc=arbndc
127680130411     c   10              eval      wpda_ddc=arbddc
127690130411     c   10              eval      wpda_dcm=arbdcm
127700041025     c                   endif
127710041025     c                   endif
127720941223     C*
127730941223     C                   Z-ADD     DATEU         ARBDTV
127740941223     C                   TIME                    ARBORV
127750941223     C                   MOVEL     VIDCVR        ARBCVB
127760941223     C*
127770060130     C* SE DA STORICIZZARE
127780060130     C**   *IN10         IFEQ      *ON
127790060130    1C**   D66FSA        ANDEQ     'S'
127800060130     C**   *IN10         OREQ      *OFF
127810060130    1C**   D66FSP        ANDEQ     'S'
127820060130    1C     D66FSA        ifeq      'S'
127830941223     C                   Z-ADD     ARBKSC        ARBCCM
127840011108      *
127850011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
127860011108      * è in partenza 'P' o in arrivo 'A'
127870011108     C     *IN10         IFEQ      *ON
127880011108     C                   MOVE      'A'           ARBPRU
127890011108     C                   ENDIF
127900011108     C     *IN10         IFEQ      *OFF
127910011108     C                   MOVE      'P'           ARBPRU
127920011108     C                   ENDIF
127930011108      *
127940941223     C                   WRITE     FNARBM00
127950941223    1C                   END
127960060302     c* storicizzazione motivazione
127970060302     c     d66mtv        ifeq      'S'
127980060302     c                   exsr      sto_mtv
127990060302     c                   endif
128000941223     C*
128010950104     C                   Z-ADD     VIDKSC        ARBKSC
128020950104     C                   MOVEL     VIDKLP        ARBKSC
128030950104     C                   MOVEL     ARBKSC        ARBCCM
128040970613     C     ARBFDN        IFEQ      ' '
128050970613     C                   MOVEL     COMFAP        ARBFAP
128060970613     C                   ELSE
128070970613     C                   MOVEL     'C'           ARBFAP
128080970613     C                   ENDIF
128090970613     C**
128100020508    1C     VIDKSC        IFEQ      8888
128110941223     C                   MOVEL     VIDRSM        ARBRSM
128120941223     C                   MOVEL     VIDINM        ARBINM
128130941223     C                   MOVEL     VIDLOM        ARBLOM
128140941223     C                   MOVEL     VIDCAM        ARBCAM
128150941223     C                   MOVEL     VIDPRM        ARBPRM
128160941223     C                   MOVEL     VIDNZM        ARBNZM
128170941223     C                   CLEAR                   ARBCPI
128180950626     C*
128190950626     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
128200950626     C*     NELLA P.IVA
128210950626     C     VIDISM        IFEQ      '$$'
128220950626     C     VIDCPM        ANDEQ     *BLANKS
128230950626     C                   MOVEL     'PRIVATO'     VIDCPM
128240950626     C                   ENDIF
128250950626     C*
128260950104    2C     VIDISM        IFEQ      *BLANKS
128270941223     C                   MOVEL     VIDCPM        ARBCPI
128280941223     C                   ELSE
128290941223     C                   MOVEL     VIDPIM        ARBCPI
128300950104    2C                   ENDIF
128310950104     C*
128320950104     C                   ELSE
128330950412     C* PRENDO I DATI DA CNACO E CNIND
128340950104     C                   MOVEL     ACORAG        ARBRSM
128350950104     C                   MOVEL     INDVIA        ARBINM
128360950104     C                   MOVEL     INDCIT        ARBLOM
128370950104     C                   MOVEL     INDCAE        ARBCAM
128380950104     C                   MOVEL     INDPRV        ARBPRM
128390950104     C                   MOVEL     INDSTA        ARBNZM
128400950104     C                   MOVEL     INDIVA        ARBCPI
128410941223     C                   ENDIF
128420041027     c* Mi salvo laa new P.iva
128430041027     c                   eval      newcpi=arbcpi
128440041027     c
128450041025     c
128460060223     C* SE ESISTE RECORD P.IVA IN FIAR4  LO AGGIORNO
128470120413     c                   exsr      up_fiar4P
128480941223     C*
128490941223     C* PER SEDE
128500070503     C     D66FFI        IFEQ      'S'
128510950227     C*
128520950208     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
128530950413    2C     §3ABSD        IFEQ      ' '
128540941227     C                   OPEN      FNARBM0T
128550941227     C                   WRITE     FNARBMTT
128560941227     C                   CLOSE     FNARBM0T
128570950227    2C                   END
128580941223     C* DISALLOCO OGGETTO
128590941223     C                   EXSR      DLCSED
128600950208    1C                   END
128610941223     C*
128620941223     C* AGGIORNO FILE TASSAZIONE PADRONCINI
128630950113     C                   EXSR      AGGPMT
128640950104     C*  REIMPOSTO IL CODICE CLIENTE MITTENTE
128650950104     C                   MOVEL     W0070         ARBCCM
128660950104     C*
128670950113     C* CONTROLLO SE SI TRATTA DI VOLUME AUTOMATICO
128680950113     C* SE HA VARIATO IL MITTENTE
128690950113     C     ARBKSC        IFNE      OLDKSC
128700950113     C*
128710950113     C                   MOVEL     '7T'          COD
128720051214     C                   MOVEL(P)  ARBFVF        KEY
128730950113     C                   EXSR      CTABEL
128740950113     C                   MOVEL     TBLUNI        DS7T
128750950113     C     §7TRVL        IFEQ      'A'
128760950113     C     §7TRVL        OREQ      'P'
128770950114     C                   MOVEL     'S'           WRICVL            1
128780950113     C                   ENDIF
128790950113     C                   ENDIF
128800950113     C*
128810950114     C* SALVO I CAMPI COLLEGATI AL MITTENTE CHE USERO PER STORICIZZARE
128820950114     C                   MOVEL     ARBCTR        MITCTR
128830950114     C                   MOVEL     ARBFVF        MITFVF
128840950114     C                   MOVEL     ARBVLF        MITVLF
128850140403     c*
128860140403     c* aggiorno fiar5 record gen per pickup2
128870140407     c                   if        comgf2<>'01' and arbnzm=*blanks and
128880140407     c                             arbfdn=*blanks
128890140407     c                   eval      wpk2='S'
128900140407     c                   else
128910140407     c                   clear                   wpk2
128920140407     c                   endif
128930140407     c                   if        (wpk2='S' and par5pk2<>'S') or
128940140407     c                              (wpk2<>'S' and par5pk2='S')
128950140403     c                   eval      Wtipoagg='PIK'
128960140403     c                   exsr      regisar5gen
128970140403     c                   endif
128980950114     C*
128990950113     C* SE PREMUTO F10 DEVO FARE IL GIRO CON IL CODICE TARIFFA
129000950104     C     *INKJ         IFEQ      *ON
129010070907     C***                MOVEL     'TP'          D48CVB
129020070907     C***                MOVEL     'E'           WESEG
129030070907     c                   add       1             kk
129040070907     c                   movel     'TP'          kcvb(kk)
129050070911     c                   movel     d48ffr        kffr(kk)
129060070911     c                   movel     w48f12        kf12(kk)
129070070907     c
129080950113     C                   ELSE
129090950114     C*
129100950114     C* SE HO RIPROPOSTO IL CODICE TARIFFA DEL CLIENTE DEVO PASSARE
129110950114     C*  DALLA VIDEATA DELLA TASSAZIONE
129120950114     C                   MOVEL     ARBCTR        W003A             3
129130950114     C     W003A         IFNE      VIDCTR
129140070907     C***                MOVEL     'TP'          D48CVB
129150070907     C***                MOVEL     'E'           WESEG
129160070911     C****               CLEAR                   D48FFR
129170070907     c                   add       1             kk
129180070907     c                   movel     'TP'          kcvb(kk)
129190070911     c                   movel     ' '           kffr(kk)
129200070911     c                   movel     ' '           kf12(kk)
129210950114     C                   ELSE
129220950114     C*
129230950113     C* RICALCOLO VOLUME AUTOMATICO E POI VADO IN VIDEATA DEL VOLUME
129240950114     C     WRICVL        IFEQ      'S'
129250950113     C                   EXSR      CALVOL
129260950114     C                   MOVEL     D18FVF        ARBFVF
129270950114     C                   MOVEL     D18VLF        ARBVLF
129280070911     C***                MOVEL     'I2'          D48CVB
129290070911     C***                MOVEL     'E'           WESEG
129300070911     c***                clear                   w48f12
129310070907     c                   add       1             kk
129320070907     c                   movel     'I2'          kcvb(kk)
129330070911     c                   movel     d48ffr        Kffr(KK)
129340070911     c                   movel     ' '           Kf12(kk)
129350950104     C                   ENDIF
129360950113     C                   ENDIF
129370950114     C                   ENDIF
129380950114     C* MEMORIZZO IL COD TARIFFA SULLA BOLLA, TANTO SE E' CAMBIATO
129390950114     C*  PASSO PER FORZA DALLA VIDEATA DELLA TASSAZIONE PER POTERLO
129400950114     C*  TRASMETTERE
129410950114     C                   MOVEL     VIDCTR        ARBCTR
129420000623     C**
129430120416     c* Controllo e Gestione eventuale record W in fiar4
129440120416     c                   exsr      up_fiar4W
129450041027     c* In partenza memorizzo se 1°bolla franco anche se doppia
129460041027     c* In arrivo   memorizzo se 1°bolla franco e non    doppia
129470041027     c                   if        (*in10 and not *in05) or
129480041027     c                             (not *in10 and not *in06)
129490041027     c                   movel     savcpi        arbcpi
129500041027     c                   endif
129510950114     C*
129520950114     C   10              UPDATE    FNARB000
129530950114     C  N10              UPDATE    FNBLP000
129540041025     c*
129550041025     c* Aggiorno anche l'altra bolla
129560041025     c                   if        waltrabolla=' '
129570041025     c* Se  devo riaggiornare il volume benen altrimenti tengo quello d
129580041025     c*  della bolla
129590041025     c                   if        wricvl<>'S'
129600041025     c                   eval      arbfvf=waltrofvf
129610041027     c                   eval      arbvlf=waltrovlf
129620041025     c                   endif
129630041027     c
129640041027     c* P.IVA  idem come sopra
129650041027     c                   if        (*in10 and *in06) or
129660041027     c                             (not *in10 and *in05)
129670041027     c                   movel     newcpi        arbcpi
129680041027     c                   else
129690041027     c                   movel     waltrocpi     arbcpi
129700041027     c                   endif
129710041025     c
129720041025     C   10              except    arbMblp
129730041025     C  N10              except    arbMarb
129740041025     c                   endif
129750950114     C*
129760950114     C* DISALLOCO I RECORD CHE NON USO
129770991007     C   10              UNLOCK    FIAR601L
129780130411     C*
129790130411     c* Richiamo routine per invio variazione a pda
129800130411     c                   exsr      sr_pda
129810950602     C*
129820941227     C     ENDRG8        ENDSR
129830950113     C*
129840961219     C*--- CONTROLLI RIFERIMENTO MITTENTE ----------------------------*
129850961219     C     CONTR9        BEGSR
129860961219     C                   SETOFF                                       9028
129870061024     C*
129880061024     C* NATURA MERCE SEMPRE OBBLIGATORIA
129890061024     C     VIDNAS        IFEQ      *BLANKS
129900061024     C                   SETON                                        419028
129910061024     C                   MOVEL     MSG(75)       VIDMSG
129920061024     C                   END
129930961219     C* SE DESTINATARIO ESTERO IL RIFERIMENTO E' OBBLIGATORIO
129940061024     c* Solo se c'è attraversamento dogana (come fa FNLS01R)
129950961219     C     WDESIE        IFEQ      'E'
129960970110     C     VIDRMA        ANDEQ     *BLANKS
129970061024     C     deseft        ANDne     *BLANKS
129980961219     C                   SETON                                        409028
129990961219     C                   MOVEL     MSG(74)       VIDMSG
130000961219     C                   END
130010961219     C*
130020961219     C                   ENDSR
130030060316     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE RM -----------------*
130040060316     C     REGIS9        BEGSR
130050130412     c                   clear                   wpda_ndc
130060130412     c                   clear                   wpda_ddc
130070130412     c                   clear                   wpda_dcm
130080060316     C*  GIRO UDATE
130090060316     C                   TIME                    W0140            14 0
130100060316     C* UDATE IN GGMMAAAA
130110060316     C                   MOVE      W0140         WDTGIO            8 0
130120060316     C*
130130060316     C* UDATE IN AAAAMMGG
130140060316     C                   Z-ADD     WDTGIO        G02DAT
130150060316     C                   MOVEL     *BLANK        G02ERR
130160060316     C                   CALL      'XSRDA8'
130170060316     C                   PARM                    WLBDAT
130180060316     C                   MOVEL     G02INV        DATEU             8 0
130190060316     C*
130200060316     C                   MOVEL     KNMUS         ARBPRU
130210060316     C* INVIO PER SEDE
130220060316     C                   MOVEL     'FNARBD0T'    SFILE
130230060316     C* ALLOCO
130240060316     C                   EXSR      ALLOC
130250060316     C* ERRORI IN ALLOCAZIONE
130260060316    1C     *IN91         IFEQ      *ON
130270060316     C                   GOTO      ENDRG9
130280060316    1C                   ENDIF
130290060316     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
130300060316     c* iornare direttamente il file
130310060316     c                   clear                   waltrabolla       1
130320060316     C   10KARB          CHAIN     FNBLP01L                           3091
130330060316     C  n10KARB          CHAIN     FNARB01L                           3091
130340060316     c*
130350060316     c                   if        *in91
130360060316     C                   MOVEL     MSG(4)        VIDMSG
130370070515     c                   exsr      DLCSED
130380070525     C                   SETON                                        2891
130390060316     C                   GOTO      ENDRG9
130400060316     c                   else
130410060316     c                   if        *in30
130420060316     c                   eval      waltrabolla='N'
130430060316     c                   else
130440130411     c  n10              eval      wpda_ndc=arbndc
130450130411     c  n10              eval      wpda_ddc=arbddc
130460130411     c  n10              eval      wpda_dcm=arbdcm
130470060316     C   10KARB          CHAIN     FNARB01L                           30
130480060316     C  N10KARB          CHAIN     FNBLP01L                           30
130490060316     c* Alla fine per non sporcarmi i campi del record, richaino il
130500060316     c*  record originale che sto variando
130510130411     c   10              eval      wpda_ndc=arbndc
130520130411     c   10              eval      wpda_ddc=arbddc
130530130411     c   10              eval      wpda_dcm=arbdcm
130540060316     c                   endif
130550060316     c                   endif
130560060316     C                   Z-ADD     DATEU         ARBDTV
130570060320     C                   TIME                    ARBORV
130580060410     C                   MOVEL     VIDCVR        ARBCVB
130590060316     c* STORICIZZAZIONE
130600060316    1C     D66FSA        ifeq      'S'
130610060316      *
130620060316      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
130630060316      * è in partenza 'P' o in arrivo 'A'
130640060316     C     *IN10         IFEQ      *ON
130650060316     C                   MOVE      'A'           ARBPRU
130660060316     C                   ENDIF
130670060316     C     *IN10         IFEQ      *OFF
130680060316     C                   MOVE      'P'           ARBPRU
130690060316     C                   ENDIF
130700060316      *
130710060316     c* nella ragione sociale devo mettere rma e nas ma prima devo
130720060316     c* salvare i campi della ragione sociale
130730060316     c                   movel     arbrsd        s_arbrsd
130740060316     c                   movel     arbrd2        s_arbrd2
130750060316     c                   movel(P)  arbrma        arbrsd
130760060316     c                   movel(P)  arbnas        arbrd2
130770060316     c
130780060316     C                   WRITE     FNARBd00
130790060316     c                   movel     s_arbrsd      arbrsd
130800060316     c                   movel     s_arbrd2      arbrd2
130810060316     C                   END
130820060316     c* storicizzazione motivazione
130830060316     c     d66mtv        ifeq      'S'
130840060316     c                   exsr      sto_mtv
130850060316     c                   endif
130860060316     C* Ora invio solo a sede
130870060316    1C     D66FFI        IFEQ      'S'
130880060316     C*
130890060316     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
130900060316    2C     §3ABSD        IFEQ      ' '
130910060316     C                   OPEN      FNARBD0T
130920060316     c                   movel     arbrsd        s_arbrsd
130930060316     c                   movel     arbrd2        s_arbrd2
130940060316     c                   movel(p)  vidrma        arbrsd
130950060316     c                   movel(p)  vidnas        arbrd2
130960060316     C                   WRITE     FNARBDTT
130970060316     c                   movel     s_arbrsd      arbrsd
130980060316     c                   movel     s_arbrd2      s_arbrd2
130990060316     C                   CLOSE     FNARBD0T
131000060316    2C                   END
131010060316     C* DISALLOCO OGGETTO
131020060316     C                   EXSR      DLCSED
131030060316    1C                   END
131040060316     C*
131050060316     C* F6 - AGGIORNAMENTO
131060060316     C                   MOVE      VIDRMA        ARBRMA
131070060316     C                   MOVE      VIDNAS        ARBNAS
131080060316     C   10              UPDATE    FNARB000
131090060316     C  N10              UPDATE    FNBLP000
131100060316     c
131110060316     c                   if        waltrabolla=' '
131120060316     C                   MOVE      VIDRMA        ARBRMA
131130060316     C                   MOVE      VIDNAS        ARBNAS
131140060316     C   10              except    RMblp
131150060316     C  N10              except    RMarb
131160060316     c                   endif
131170130411     C*
131180130411     c* Richiamo routine per invio variazione a pda
131190130411     c                   exsr      sr_pda
131200060316     c     endrg9        endsr
131210030123      *
131220030123      *--- Controllo campi mancato ritiro BANCALI --------------------*
131230030123     c     Contr10       BegSr
131240030123     c                   Eval      *In90 = *Off
131250030127
131260030127      * Il numero dei bancali non ritirati è obbligatorio
131270030127     c                   If        VidRb1 = *Zeros and VidRb2 = *Zeros
131280030127     c                   Eval      VidMsg = Msg(125)
131290030127     c                   Eval      *In60 = *On
131300030127     c                   Eval      *In28 = *On
131310030127     c                   Eval      *In90 = *On
131320030127     c                   GoTo      EndCt10
131330030127     c                   EndIf
131340030124      * Controllo il n. dei bancali non ritirati
131350030124     c                   If        VidRb1 > VidNba
131360030124     c                   Eval      VidMsg = Msg(124)
131370030124     c                   Eval      *In60 = *On
131380030124     c                   Eval      *In28 = *On
131390030124     c                   Eval      *In90 = *On
131400030124     c                   GoTo      EndCt10
131410030124     c                   EndIf
131420030124     c                   If        VidRb2 > VidNb2
131430030124     c                   Eval      VidMsg = Msg(124)
131440030124     c                   Eval      *In61 = *On
131450030124     c                   Eval      *In28 = *On
131460030124     c                   Eval      *In90 = *On
131470030124     c                   GoTo      EndCt10
131480030124     c                   EndIf
131490030123      * Nominativo
131500030123     c                   If        VidNomb = *Blanks
131510030124     c                   Eval      VidMsg = Msg(118)
131520030124     c                   Eval      *In57 = *On
131530030123     c                   Eval      *In28 = *On
131540030123     c                   Eval      *In90 = *On
131550030123     c                   GoTo      EndCt10
131560030123     c                   EndIf
131570030123      * Motivo
131580030123     c                   If        VidMotb = *Blanks
131590030124     c                   Eval      VidMsg = Msg(120)
131600030124     c                   Eval      *In59 = *On
131610030123     c                   Eval      *In28 = *On
131620030123     c                   Eval      *In90 = *On
131630030123     c                   GoTo      EndCt10
131640030123     c                   EndIf
131650030123
131660030123     c     EndCt10       EndSr
131670030123      **
131680030123      *--- AGGIORNAMENTO FILE FIAR5 ----------------------------------*
131690030123     c     Regis10       BegSr
131700130412
131710130412     c                   clear                   wpda_ndc
131720130412     c                   clear                   wpda_ddc
131730130412     c                   clear                   wpda_dcm
131740030123
131750030124     c                   Eval      §Ar5Prub = Knmus
131760060131     c                   Eval      §Ar5Poub = dutPou
131770030124     c                   Move      Dateu         §Ar5Dav
131780030124     c                   Movel     W0140         §Ar5Orv
131790030124     c                   Eval      §Ar5Rb1 = VidRb1
131800030124     c                   Eval      §Ar5Rb2 = VidRb2
131810030124     c                   Eval      §Ar5Nomb = VidNomb
131820030124     c                   Eval      §Ar5Motb = VidMotb
131830030124     c                   Movel(p)  DAr5Ban       Ar5Uni
131840030124     c                   Clear                   Ar5Ft1
131850030124     c                   Clear                   Ar5Ft2
131860030124     c                   Clear                   Ar5Ft3
131870030124     c                   Update    Fiar5000
131880041020     c*
131890041020     c* aggiornamento file fiqdt00f per gestione rientri
131900041020     c                   z-add     arbifp        kfgs
131910041020     c                   z-add     arbndc        w0060             6 0
131920041020     c     kqdt          chain     fiqdt01l
131930041020     c                   if        %found(fiqdt01l)
131940041020     c                   add       vidrb1        qdtnbnbr
131950041020     c                   add       vidrb2        qdtnbnbr
131960041020     c                   except    aggqdt
131970041020     c                   endif
131980060302     c* storicizzazione motivazione
131990060302     c     d66mtv        ifeq      'S'
132000060320     c                   Move      Dateu         arbdtv
132010060320     c                   movel     w0140         arborv
132020060302     c                   exsr      sto_mtv
132030060302     c                   endif
132040130411     C*
132050130412     c                   clear                   waltrabolla       1
132060130412     c                   if        *in10
132070130412     c                   eval      wpda_ndc=arbndc
132080130412     c                   eval      wpda_ddc=arbddc
132090130412     c                   eval      wpda_dcm=arbdcm
132100130412     c                   unlock    fnarb01l
132110130412     c                   else
132120130412     c                   unlock    fnblp01l
132130131120     c                   if        d66pda<>' '
132140130412     C     KARB          CHAIN(n)  FNARB01L                           30
132150130412     c                   if        *in30
132160130412     c                   eval      waltrabolla='N'
132170130412     c                   else
132180130412     c                   eval      wpda_ndc=arbndc
132190130412     c                   eval      wpda_ddc=arbddc
132200130412     c                   eval      wpda_dcm=arbdcm
132210130412     c                   endif
132220130412     c                   endif
132230130412     c                   endif
132240130412     c*
132250130412     c* Richiamo routine per invio variazione a pda
132260130411     c                   exsr      sr_pda
132270030123
132280030123     c                   EndSr
132290061107     C
132300061107     C*--- CONTROLLI FORMATO 11 --------------------------------------*
132310071025     C     CONTR11       BEGSR
132320061107     C                   SETOFF                                       9028
132330151105     c                   clear                   xcfiva1ds
132340151105     c                   clear                   flgiva            1 0
132350151105     c                   move      v11cod        w0040
132360080505     c
132370061107     c* se non previsto inserimento, ERRORE
132380071025    1c                   if        *in76
132390061107     C                   SETON                                        289040
132400071025    2C                   IF        v11MIDE='M'
132410061107     C                   MOVEL     MSG(92)       VIDMSG
132420061107     C                   ELSE
132430061107     C                   MOVEL     MSG(95)       VIDMSG
132440071025    2c                   endif
132450061107     C                   GOTO      ENDCT11
132460071025    1c                   endif
132470061107     c* flag unico se cliente italia
132480071025    1c                   if        v11mide='M'
132490061107     c                   eval      wcliie=wmitie
132500061107     c                   eval      wprv=arbprm
132510151105     c                   eval      wcap=arbcam
132520151105     c                   eval      wloc=arblom
132530061107     c                   else
132540061107     c                   eval      wcliie=wdesie
132550061107     c                   eval      wprv=arbprd
132560151105     c                   eval      wcap=arbcad
132570151105     c                   eval      wloc=arblod
132580071025    1c                   endif
132590071025     c
132600080505     c* Controllo la Partita IVA
132610071025     C* Se P.IVA sprotetta e nazione estera con flag efta la P.IVA è
132620071025     C* obbligatoria
132630071025     c
132640071025    1c                   if        v11pid=*blanks
132650071025     c
132660071025    2c                   if        wdesie='E' and deseft<>*blanks
132670071025     C                   SETON                                        284190
132680071025     C                   MOVEL     MSG(67)       VIDMSG
132690071025     C                   GOTO      ENDCT11
132700120507     c****               else
132710071031     c* solo se non richiamato nascosto
132720120507    3c*****              if        d48ffr='V' or d48ffr='S'
132730120507    4c****               if        wcliie='I' and   forzapid=' '
132740120507     c*****                        and dateu>=§vpopiv
132750120507     C******             SETON                                        289041
132760120507     C*****              MOVEL     MSG(57)       VIDMSG
132770120507     c******             movel     '1'           forzapid
132780120507     C********           GOTO      ENDCT11
132790120507    4c*********          endif
132800120507    3c********           endif
132810071031     c
132820071025    2c                   endif
132830071025    1C                   ENDif
132840071025     C*
132850151105    1c***                if        v11pid<>*blanks
132860071025     c* Se cambiata pulisco la forzatura
132870071025    2c                   if        v11pid<>sav11pid
132880071025     c                   clear                   forzapid2
132890071025     c                   movel     v11pid        sav11pid
132900071025    2c                   endif
132910071025     c
132920151105     c                   clear                   xcfiva1ds
132930071025     c                   clear                   xcfivamod
132940151105     c                   eval      xcfivafori= 'S'
132950100120     c                   eval      xcfivamod = 'P'
132960071025     c                   eval      xcfivapar = v11pid
132970071025     c                   eval      xcfivanar = wnar
132980071025     c                   eval      xcfivaprv = wprv
132990151105     c                   eval      xcfivacap = wcap
133000151105     c                   eval      xcfivaloc = wloc
133010151105     c                   if        w0040=8888 or w0040=9999
133020151105     c                   eval      xcfivap88='8'
133030151105     c                   endif
133040151105     c                   call      'XCFIVAR1'
133050151105     c                   parm                    xcfiva1ds
133060071025     C*
133070071025      * --> errata forzabile
133080100120    2c                   If        xcfivaflg = -9 and forzaPID2 = ' '
133090071025     c                   seton                                        289041
133100071025     c                   Eval      vidmsg = xcfivamsg
133110071025     c                   Eval      vidmsg = %trim(vidmsg) + ' Enter x forzare'
133120100120     c                   eval      forzaPID2= '1'
133130071025     C                   GOTO      ENDCT11
133140071025    2c                   EndIf
133150071025      * --> errore
133160071025    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
133170071025     c                   seton                                        289041
133180071025     c                   eval      vidmsg = xcfivamsg
133190071025     C                   GOTO      ENDCT11
133200071025    2c                   EndIf
133210151105
133220151105     c                   eval      flgiva=xcfivaflg
133230071025     c
133240071025      * se non impostato devo riportare il codice iso della partita iva che
133250071025      * mi viene passato dal pgm di controllo
133260151105    2c                   if        v11pid<>*blanks and v11isd <> ivaeu
133270071025     c                   eval      v11isd = ivaeu
133280071025     c                   seton                                          9041
133290071025     C                   GOTO      ENDCT11
133300071025    2c                   endif
133310151105    1c****               endif
133320080505     c
133330080505      * Devo richiamare il nuovo driver fatto per controllare il codice
133340080505      * fiscale
133350151105     c                   clear                   xcfiva1ds
133360080505     c                   clear                   xcfivamod
133370080505     c                   eval      xcfivapar = v11cdfis
133380080505     c                   eval      xcfivanar = wnar
133390080505     c                   eval      xcfivaprv = wprv
133400151105     c                   eval      xcfivacap = wcap
133410151105     c                   eval      xcfivaloc = wloc
133420151105     c                   eval      xcfivapiva= v11pid
133430151105     c                   if        w0040=8888 or w0040=9999
133440151105     c                   eval      xcfivap88='8'
133450151105     c                   endif
133460151105     c                   call      'XCFIVAR1'
133470151105     c                   parm                    xcfiva1ds
133480080505     c*
133490080505    1c                   if        xcfivaflg < *zeros
133500080505     C                   SETON                                        289040
133510080505     C                   MOVEL     xcfivamsg     VIDMSG
133520080505     C                   GOTO      ENDCT11
133530080505    1c                   endif
133540080505     c
133550080505     c* Obbligatorio se cliente italia partita iva o codice fiscale
133560080505     c* solo se non richiamato nascosto
133570120507    1c***                if        d48ffr='V' or d48ffr='S'
133580120507    2c****               if        wcliie='I' and   v11cdfis=*blanks
133590120507     c****                         and forzacdf=' ' and xcfivaflg<>9
133600120507     C****               SETON                                        289040
133610120507     C*****              MOVEL     MSG(23)       VIDMSG
133620120507     c****               movel     '1'           forzacdf
133630120507     C*****              GOTO      ENDCT11
133640120507    2c*****              endif
133650120507    1c******             endif
133660080505     c
133670080505     c* Se entrambe vuote errore
133680080505    1c                   if        v11cdfis=*blanks and v11pid=*blanks
133690080505     C                   SETON                                        284090
133700080505     C                   MOVEL     MSG(145)      VIDMSG
133710080505     C                   GOTO      ENDCT11
133720080505    1c                   endif
133730120508     c* Se fattura immediata errore se partita iva '$$'
133740120508     c                   if        %scan('$$':v11pid)>0  and wcliie='I'
133750120508     c                   move      v11cod        w0040
133760120508     c                   if        w0040=8888 or w0040=9999
133770120507     C                   SETON                                        284190
133780120507     C                   MOVEL     MSG(149)      VIDMSG
133790120507     C                   GOTO      ENDCT11
133800120508     c                   endif
133810120507     c                   endif
133820120508     c* solo se non richiamato nascosto
133830130315    3c                   if        d48ffr='V' or d48ffr='S'
133840120507     c* Se codice fiscale lungo 11 si richiede la partita IVA  - Errore forzabile
133850120507     c                   if        v11cdfis<>*blanks and v11pid=*blanks and
133860120507     c                             %subst(v11cdfis:12:1)=' ' and wcliie='I'
133870120507     c                             and forzapid=' '
133880120507     C                   SETON                                        289041
133890120507     C                   MOVEL     MSG(57)       VIDMSG
133900120507     c                   movel     '1'           forzapid
133910120507     C                   GOTO      ENDCT11
133920120507     c                   endif
133930130315     c                   endif
133940061107     C*
133950061107     C     ENDCT11       ENDSR
133960130927     c
133970130927     C*--- CONTROLLI FORMATO 12 --------------------------------------*
133980130927     C     CONTR12       BEGSR
133990130927     C                   SETOFF                                       9028
134000130930     c* controllo indirizzo email
134010131001     c                   if        *in86  and videml<>*blanks
134020130930     c                   Clear                   dsemail
134030130930     c                   Eval      §emlindi = videml
134040130930     c                   Call      'XEMAIL'
134050130930     c                   Parm                    dsemail
134060130930if  3c                   If        §emlerro <> '0'
134070130930     C                   SETON                                        289041
134080130930     C                   MOVEL     MSG(150)      VIDMSG
134090130930     C                   GOTO      ENDCT12
134100130930     c                   endif
134110130930
134120130930     c                   endif
134130130930     c
134140130927     C     ENDCT12       ENDSR
134150061107     C*
134160061107     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE FI -----------------*
134170061107     C     REGIS11       BEGSR
134180130412
134190130412     c                   clear                   wpda_ndc
134200130412     c                   clear                   wpda_ddc
134210130412     c                   clear                   wpda_dcm
134220061107     C*  GIRO UDATE
134230061107     C                   TIME                    W0140            14 0
134240061107     C* UDATE IN GGMMAAAA
134250061107     C                   MOVE      W0140         WDTGIO            8 0
134260061107     C*
134270061107     C* UDATE IN AAAAMMGG
134280061107     C                   Z-ADD     WDTGIO        G02DAT
134290061107     C                   MOVEL     *BLANK        G02ERR
134300061107     C                   CALL      'XSRDA8'
134310061107     C                   PARM                    WLBDAT
134320061107     C                   MOVEL     G02INV        DATEU             8 0
134330061107     C                   MOVEL     KNMUS         ARBPRU
134340071025     c*
134350071025     C     V11ISD        IFEQ      '$$'
134360071025     C     V11CPD        ANDEQ     *BLANKS
134370071025     C                   MOVEL     'PRIVATO'     V11CPD
134380071025     C                   ENDIF
134390071025     c
134400071025     c* Salvo a parte la partita iva presente in bolla
134410071025     c                   movel     arbcpi        bollacpi
134420061107     C*
134430061129     c* Se la bolla non è trasmessa non devo allocare il file e non devo
134440061129     c*    creare la variazione
134450061129     c                   if        bollaft2<>' '
134460061107     C* INVIO PER SEDE
134470061107     C                   MOVEL     'FNARBD0T'    SFILE
134480061107     C* ALLOCO
134490061107     C                   EXSR      ALLOC
134500061107     C* ERRORI IN ALLOCAZIONE
134510061107    1C     *IN91         IFEQ      *ON
134520061107     C                   GOTO      ENDRG11
134530061107    1C                   ENDIF
134540061129    1C                   ENDIF
134550061107     c
134560061107     c* Alloco il record fiar4 del codice fiscale                      agg
134570061107     C                   MOVEL     'Q'           WTRC
134580061107     C     KARB2         CHAIN     FiAR4000                           3091
134590061107     c*
134600061107     c                   if        *in91
134610061107     C                   MOVEL     MSG(4)        VIDMSG
134620070515     c                   exsr      DLCSED
134630070525     C                   SETON                                        2891
134640061107     C                   GOTO      ENDRG11
134650061107     c                   endif
134660061107     c
134670061107     c                   if        *in30
134680061107     c                   eval      esiste='N'
134690061116     c                   clear                   ar4not
134700061107     c                   else
134710061107     c                   eval      esiste='S'
134720061107     c                   endif
134730071026     c*
134740071026     c* Alloco il record in partenza se sono in arrivo e viceversa per agg
134750071026     c* iornare direttamente il file solo se non si tratta di bolla doppia
134760071026     c                   clear                   waltrabolla       1
134770130412     c                   setoff                                       3091
134780130412    1c                   if        §3atb2=*blanks and aggiobol<>'N'
134790071026     C   10KARB          CHAIN     FNBLP01L                           3091
134800071026     C  n10KARB          CHAIN     FNARB01L                           3091
134810130412     c                   else
134820131120     c                   if        d66pda<>' '
134830130412     c                   if        not *in10
134840130412     C     KARB          CHAIN(n)  FNARB01L                           30
134850130412     c                   endif
134860130412     c                   eval      wpda_ndc=arbndc
134870130412     c                   eval      wpda_ddc=arbddc
134880130412     c                   eval      wpda_dcm=arbdcm
134890130412     c                   endif
134900130412    1c                   endif
134910071026     c*
134920071026    2c                   if        *in91
134930071026     C                   MOVEL     MSG(4)        VIDMSG
134940071026     c*
134950071026     c                   exsr      DLCSED
134960071026     c                   unlock    fiar401l
134970071026     c
134980071026     C                   SETON                                        2891
134990071026     C                   GOTO      ENDRG11
135000071026   x2c                   else
135010071026    3c                   if        *in30
135020071026     c                   eval      waltrabolla='N'
135030071026   x3c                   else
135040130412    4c                   if        §3atb2=*blanks and aggiobol<>'N'
135050130411     c  n10              eval      wpda_ndc=arbndc
135060130411     c  n10              eval      wpda_ddc=arbddc
135070130411     c  n10              eval      wpda_dcm=arbdcm
135080071026     C   10KARB          CHAIN     FNARB01L                           30
135090071026     C  N10KARB          CHAIN     FNBLP01L                           30
135100071026     c* Alla fine per non sporcarmi i campi del record, richaino il
135110071026     c*  record originale che sto variando
135120130411     c   10              eval      wpda_ndc=arbndc
135130130411     c   10              eval      wpda_ddc=arbddc
135140130411     c   10              eval      wpda_dcm=arbdcm
135150130412    4c                   endif
135160071026    3c                   endif
135170071026    2c                   endif
135180130412    1c****               endif
135190061107     C*
135200061107     C                   Z-ADD     DATEU         ARBDTV
135210061107     C                   TIME                    ARBORV
135220061107     C                   MOVEL     VIDCVR        ARBCVB
135230061107     c* Memorizzo se si tratta del mittente o destinatario
135240061107     c                   movel     v11mide       arbrd2
135250061107     c
135260061107     c* clearo gli altri campi del file che non mi interessano
135270061107     c                   clear                   arbrsd
135280061107     c                   clear                   arbind
135290061107     c                   clear                   arbcad
135300061107     c                   clear                   arblod
135310061107     c                   clear                   arbprd
135320061107     c                   clear                   arbnzd
135330061107     c                   clear                   arbfin
135340061107     c                   clear                   arbpkb
135350061107     c                   clear                   arbpkf
135360061107     c                   clear                   arbvlb
135370061107     c                   clear                   arbvlf
135380061107     c                   clear                   arbfvb
135390061107     c                   clear                   arbfvf
135400061107     c                   clear                   arbffd
135410061107     c                   clear                   arbfst
135420061107     C*
135430061107     C* SE DA STORICIZZARE
135440061107    1C     D66FSA        ifeq      'S'
135450061107      *
135460061107      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
135470061107      * è in partenza 'P' o in arrivo 'A'
135480061107     C     *IN10         IFEQ      *ON
135490061107     C                   MOVE      'A'           ARBPRU
135500061107     C                   ENDIF
135510061107     C     *IN10         IFEQ      *OFF
135520061107     C                   MOVE      'P'           ARBPRU
135530061107     C                   ENDIF
135540061107     c                   if        v11mide='M'
135550061107     c                   movel     ar4not        arbcpi
135560061107     c                   else
135570061107     c                   move      ar4not        arbcpi
135580061107     c                   endif
135590071025     c* Partita iva
135600071025     c                   movel     bollacpi      arbrsd
135610061107      *
135620061107     C                   WRITE     FNARBD00
135630061107    1C                   ENDIF
135640061107    c
135650061107     c* storicizzazione motivazione
135660061107     c     d66mtv        ifeq      'S'
135670061107     c                   exsr      sto_mtv
135680061107     c                   endif
135690071025     c* Aggiorno campo codice fiscale
135700061107     c                   movel     v11cdfis      arbcpi
135710071025     c* Aggiorno campo per la partita iva
135720071025     C                   MOVEL     V11PID        ARBrsd
135730071025     C* LA P.IVA SE HA CODICE ISO VIENE MESSO A SIN ALTRIMENTI SUBITO
135740071025     C*  IL CODICE DELLA P.IVA
135750071025     C     V11CPD        IFNE      *BLANKS
135760071025     C     V11ISD        ANDEQ     *BLANKS
135770071025     C                   CLEAR                   ARBrsd
135780071025     C                   MOVEL     V11CPD        ARBrsd
135790071025     C                   ENDIF
135800130402     c* Se Cd.Fisc. lungo 11 lo metto anche nella P.Iva se vuota
135810130402     c                   if        v11pid=*blanks and %subst(v11cdfis:12:1)=' '
135820130402     c                   movel     v11cdfis      arbrsd
135830130402     c                   endif
135840071025     c
135850071025     C                   MOVEL     'P'           WTRC
135860071025     C     KARB2         CHAIN     FIAR401L                           30
135870071025    1C     *IN30         IFEQ      *OFF
135880071025    2c                   if        v11mide='M'
135890071025     c                   movel     ar4not        wcpifile
135900071025   x2c                   else
135910071025     c                   move      ar4not        wcpifile
135920071025    2c                   endif
135930071025    2c                   if        wcpifile <> arbrsd
135940071025     c                   movel     'T'           ar4ftr
135950071025     c                   z-add     dateu         ar4dtr
135960071025     c                   z-add     dateu         ar4duv
135970071025    3c                   if        v11mide='M'
135980071025     c                   eval      %subst(ar4not:1:16)=%subst(arbrsd:1 :16)
135990071025     c                   else
136000071025     c                   eval      %subst(ar4not:20:16)=%subst(arbrsd:1:16)
136010071025    3c                   endif
136020071025     c
136030071025     C                   UPDATE    FIAR4000
136040071025   x2c                   else
136050071025     c                   unlock    fiar401l
136060071025    2c                   endif
136070071025    1c                   endif
136080061129     c
136090071025    0c                   if        bollaft2<>' '
136100061107     C* Ora invio solo a sede
136110061107     C     D66FFI        IFEQ      'S'
136120061107     C*
136130061107     C* SOLO SE LA BOLLA E' STATA TRASMESSA IN SEDE
136140061107    2C     §3ABSD        IFEQ      ' '
136150061107     C                   OPEN      FNARBD0T
136160061107     C                   WRITE     FNARBDTT
136170061107     C                   CLOSE     FNARBD0T
136180061107    2C                   END
136190061107     C* DISALLOCO OGGETTO
136200061107     C                   EXSR      DLCSED
136210061107    1C                   END
136220071025    0C                   END
136230061107     C*
136240061107     C* AGGIORNO  file: 30 on write del record
136250071025     C                   MOVEL     'Q'           WTRC
136260071025     C     KARB2         CHAIN     FiAR4000                           3091
136270061107     c                   if        Esiste='N'
136280061107     c                   movel     arbaas        ar4aas
136290061107     c                   movel     arblnp        ar4lnp
136300061107     c                   movel     arbnrs        ar4nrs
136310061107     c                   movel     arbnsp        ar4nsp
136320061107     c                   movel     'Q'           ar4trc
136330061107     c                   clear                   ar4not
136340061107     c                   clear                   ar4flo
136350061107     c                   endif
136360061107     c                   if        v11mide='M'
136370061107     c                   movel     v11cdfis      ar4not
136380061107     c                   else
136390061107     c                   move      v11cdfis      ar4not
136400061107     c                   endif
136410061107     c                   movel     'T'           ar4ftr
136420061107     c                   z-add     dateu         ar4dtr
136430061107     c                   z-add     dateu         ar4duv
136440061107     c
136450061116     c                   if        Esiste='N'  and ar4not<>*blanks
136460061107     c                   write     fiar4000
136470061116     c                   endif
136480061116     c                   if        Esiste='S'  and ar4not<>*blanks
136490061107     c                   update    fiar4000
136500061107     c                   endif
136510061116     c                   if        Esiste='S'  and ar4not=*blanks
136520061116     c                   delete    fiar4000
136530061116     c                   endif
136540071025     c
136550071026     c* Aggiorno la bolla per la partita iva solo se non richiamamto da I0
136560071026     c*  per cui ho già aggiornato la partita iova sulla bolla
136570071026     c                   if        Aggiobol<>'N'
136580071025     c                   movel     arbrsd        arbcpi
136590071025     c* aggiornamento mirato per non sporcare degli altri campi
136600071025     c  n10              except    SolocpiP
136610071025     c   10              except    SolocpiA
136620071026     c
136630071026     c* Se 'c'e' l'altra ed è bolla singola la aggiorno
136640071026     c                   if        §3atb2=*blanks and waltrabolla=' '
136650071026     c  n10              except    SolocpiA
136660071026     c   10              except    SolocpiP
136670071026     c                   endif
136680071026     c                   endif
136690061107     C*
136700061107     C* DISALLOCO  I FILES CHE NON USO
136710061107     C   10              UNLOCK    FiAR901L
136720071025     c** 10              unlock    fnarb01l
136730071025     c**N10              unlock    fnblp01l
136740130411     C*
136750130411     c* Richiamo routine per invio variazione a pda
136760130411     c                   exsr      sr_pda
136770061107     C*
136780061107     C     ENDRG11       ENDSR
136790130927     C*
136800130930     C*--- REGISTRAZIONE BOLLA CAUSALE VARIAZIONE Nx -----------------*
136810130927     C     REGIS12       BEGSR
136820130927     c
136830130927     C*  GIRO UDATE
136840130927     C                   TIME                    W0140            14 0
136850130927     C* UDATE IN GGMMAAAA
136860130927     C                   MOVE      W0140         WDTGIO            8 0
136870130927     C*
136880130927     C* UDATE IN AAAAMMGG
136890130927     C                   Z-ADD     WDTGIO        G02DAT
136900130927     C                   MOVEL     *BLANK        G02ERR
136910130927     C                   CALL      'XSRDA8'
136920130927     C                   PARM                    WLBDAT
136930130927     C                   MOVEL     G02INV        DATEU             8 0
136940130927     C                   MOVEL     KNMUS         ARBPRU
136950130927     C                   Z-ADD     DATEU         ARBDTV
136960130927     C                   TIME                    ARBORV
136970130927     C                   MOVEL     VIDCVR        ARBCVB
136980131001      *
136990131001      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
137000131001      * è in partenza 'P' o in arrivo 'A'
137010131001     C     *IN10         IFEQ      *ON
137020131001     C                   MOVE      'A'           ARBPRU
137030131001     C                   ENDIF
137040131001     C     *IN10         IFEQ      *OFF
137050131001     C                   MOVE      'P'           ARBPRU
137060131001     C                   ENDIF
137070130927     C*
137080130927     C* SE DA STORICIZZARE
137090130927    1C     D66FSA        ifeq      'S'
137100130930
137110131001     c                   if        *in84 and
137120131001     c                                  (vidref<>par5ref or vidteld<>par5teld)
137130130930     c* Referente e consegna
137140130927     c                   clear                   darbn_ref
137150130927     c                   eval      §arbnref = par5ref
137160130927     c                   eval      §arbnteld= par5teld
137170130927     c                   eval      arbuni70 = darbn_ref
137180130927     c                   eval      arbtrd='REF'
137190130927     c                   write     FNARBN00
137200131001     c                   endif
137210131001     c
137220131001     c                   if        *in83 and emd_§ar5tel<>vidtsms
137230130930     c                   clear                   darbn_sms
137240130930     c                   eval      §arbncel = emd_§ar5tel
137250130930     c                   eval      arbuni70 = darbn_sms
137260130927     c                   eval      arbtrd='SMS'
137270130927     c                   write     FNARBN00
137280131001     c                   endif
137290131001     c
137300131001     c                   if        *in86 and emd_§ar5eml<>videml
137310130927     c                   clear                   darbn_eml
137320130930     c                   eval      §arbneml=emd_§ar5eml
137330130927     c                   eval      arbuni70 = darbn_eml
137340130927     c                   eval      arbtrd='EML'
137350130927     c                   write     FNARBN00
137360131001     c                   endif
137370131001
137380131001     c                   if        *in81 and (vidnt1<>v1cnt1 or vidnt2<>v1cnt2)
137390130930     c                   clear                   darbn_not
137400130930     c                   eval      §arbnnot1=v1cnt1
137410130930     c                   eval      §arbnnot2=v1cnt2
137420130930     c                   eval      arbuni70 = darbn_not
137430130930     c                   eval      arbtrd='NOT'
137440130930     c                   write     FNARBN00
137450130930     c                   endif
137460130930
137470130930     c                   endif
137480130930     c
137490131001     c                   if        *in84
137500130930     c                   exsr      AggioREF
137510131001     c                   endif
137520131001     c
137530130930      * EMAIL e cell sms
137540131001     c                   if        *in83 or *in86
137550130930     c                   Eval      kAr5Trd = 'EMD'
137560130930     c     kAr5          Chain     Fiar501l
137570130930      * Aggiorno Fiar5
137580130930if  2c                   If        %Found(Fiar501l)
137590130930     c                   movel     ar5uni        DAr5EMD
137600131001     c   86              eval      emd_§ar5eml=videml
137610131001     c   83              eval      emd_§ar5tel=vidtsms
137620130930     c                   Movel(p)  DAr5emd       Ar5Uni
137630130930     c                   Update    Fiar5000
137640130930      * Scrivo Fiar5
137650130930   x2c                   Else
137660130930      * se immessi i dati
137670130930      * non mi preoccupo per il fermo deposito: il fiar5 record GEN viene sempre creato
137680130930      * in immissione bolle. Non esiste più il caso di fiar5 record GEN inesistente in fase
137690130930      * di manutenzione bolle
137700130930if  3c                   If        Videml <> *Blanks or Vidtsms <> *Blanks
137710130930     c                   Clear                   Fiar5000
137720130930     c                   Eval      Ar5Aas = ArbAas
137730130930     c                   Eval      Ar5Lnp = ArbLnp
137740130930     c                   Eval      Ar5Nrs = ArbNrs
137750130930     c                   Eval      Ar5Nsp = ArbNsp
137760130930     c                   Eval      Ar5Trd = 'EMD'
137770130930     c                   Move      Dateu         Ar5Dac
137780130930     c                   Movel     W0140         Ar5Orc
137790130930     c                   Clear                   Dar5emd
137800131001     c   86              eval      emd_§ar5eml=videml
137810131001     c   83              eval      emd_§ar5tel=vidtsms
137820130930     c                   Movel(p)  DAr5emd       Ar5Uni
137830130930     c                   Write     Fiar5000
137840130930    3c                   EndIf
137850130930    2c                   EndIf
137860130930     c
137870131001   x1c                   endif
137880130930
137890131001     c                   if        *in81
137900130930     c* aggiorno le NOTE
137910130930     c* aggiorno/deleto trk 8 e 9 se variati
137920130930     c                   clear                   fnlv19ds
137930130930     C                   MOVEL     'T'           D19ftr
137940130930     C                   MOVEL     arbAAS        D19AAS
137950130930     C                   MOVEL     arbLNP        D19LNP
137960130930     C                   MOVEL     arbNRS        D19NRS
137970130930     C                   MOVEL     arbNSP        D19NSP
137980130930     C                   MOVEL     vidnt1        D19NT1
137990130930     C                   MOVEL     vidnt2        D19NT2
138000130930    2C     vidnt1        IFNE      *BLANKS
138010130930     C     vidnt2        ORNE      *BLANKS
138020130930     C                   MOVEL     '8'           D19TRC
138030130930     C                   CALL      'FNLV19R'
138040130930     C                   PARM                    fnlv19ds
138050130930    2C                   ENDIF
138060130930     c* Se il pgm mi restituisce la seconda riga vuota la deleto perchè
138070130930     c* prima poteva esistere
138080130930    2C     D19ERR        IFEQ      *BLANKS
138090130930     C     D19NT1        ANDNE     *BLANKS
138100130930     C     D19NT2        ANDEQ     *BLANKS
138110130930     C                   MOVEL     '9'           D19TRC
138120130930     C                   CLEAR                   D19NT1
138130130930     C                   CLEAR                   D19NT2
138140130930     C                   CALL      'FNLV19R'
138150130930     C                   PARM                    fnlv19ds
138160130930   x2C                   else
138170130930     C* Se tornato codice di err richiamo il pgm 2 volte:  una per nota
138180130930    3C     D19ERR        IFNE      *BLANKS
138190130930     C     vidnt1        oreq      *BLANKS
138200130930     C     vidnt2        ANDEQ     *BLANKS
138210130930     C* Nota 1
138220130930     C                   MOVEL     '8'           D19TRC
138230130930     C                   MOVEL     vidnt1        D19NT1
138240130930     C                   CLEAR                   D19NT2
138250130930     C                   CALL      'FNLV19R'
138260130930     C                   PARM                    fnlv19ds
138270130930     C* Nota 2
138280130930     C                   MOVEL     '9'           D19TRC
138290130930     C                   MOVEL     vidnt2        D19NT1
138300130930     C                   CLEAR                   D19NT2
138310130930     C                   CALL      'FNLV19R'
138320130930     C                   PARM                    fnlv19ds
138330130930    3C                   ENDIF
138340130930    2C                   ENDIF
138350130930
138360130930    1c                   EndIf
138370130927     c*
138380131008     c   10              unlock    fnarb01l
138390131008     c  n10              unlock    fnblp01l
138400130927     C     ENDRG12       ENDSR
138410961219     C*
138420950113     C*--- CALCOLO IL VOLUME AUTOMATICO ------------------------------*
138430950113     C     CALVOL        BEGSR
138440030612     C                   CLEAR                   Fnlv18DS
138450950113     C   10              MOVEL     'A'           D18TBO
138460950113     C  N10              MOVEL     'P'           D18TBO
138470950113     C                   Z-ADD     ARBKSC        D18KSC
138480950113     C                   Z-ADD     ARBCTR        D18CTR
138490950113     C                   MOVEL     §3ATB1        D18TBL
138500950113     C                   Z-ADD     ARBAAS        D18AAS
138510950113     C                   Z-ADD     ARBMGS        D18MGS
138520950113     C                   Z-ADD     ARBNCL        D18NCL
138530950113     C                   Z-ADD     ARBPKF        D18PKF
138540020311     c                   movel     arbtsp        d18tsp
138550020311     c                   z-add     arblna        d18lna
138560020311     c                   z-add     arblnp        d18lnp
138570950113     C                   CALL      'FNLV18R'
138580030612     C                   PARM                    Fnlv18DS
138590950113     C*
138600950113     C                   ENDSR
138610911212     C*
138620970808     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
138630970808     C     CTRIND        BEGSR
138640150203     c                   clear                   normlod
138650970808     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
138660030612     C                   CLEAR                   Fnlv13DS
138670030612     C                   CLEAR                   Tisi95DS
138680970808     C*
138690100120     c                   movel     'S'           i14forzait
138700970808     C                   MOVEL     ARBDSP        I14DRI
138710020305     c                   movel     'S'           i14cnz
138720970808     C                   CALL      'FNLV14R'
138730970808     C                   PARM                    KPJBA
138740030612     C                   PARM                    Fnlv14DS
138750970808     C                   MOVEL     O14ERR        WERR              1
138760970808     C                   MOVEL     O14MSG        VIDMSG
138770970808     C                   EXSR      CTRERR
138780970808     C*
138790970808     C     *IN90         IFEQ      *ON
138800970808     C                   GOTO      ENDIND
138810970808     C                   ENDIF
138820970808     C*************
138830970808     C* CONTROLLO CAP
138840970808     C                   MOVEL     '7'           I95TCN
138850970808     C                   MOVEL     E14CAP        I95CAP
138860970808     C                   MOVEL     E14LOC        I95LOC
138870970808     C                   MOVEL     E14PRV        I95PRV
138880970808     C                   MOVEL     E14NAR        I95NAR
138890000623     C                   MOVEL     ARBDSP        I95DAT
138900020318     C     LNPNTW        IFEQ      'PPT'
138910000623     C                   MOVEL     ARBLNP        I95TFP
138920000623     C                   ELSE
138930980602     C                   MOVEL     ARBTFP        I95TFP
138940000623     C                   ENDIF
138950971230     C                   Z-ADD     ARBPKF        I95LKG
138960971230     C                   Z-ADD     ARBVLF        I95LMC
138970971230     C                   MOVEL     ARBFFD        I95FFD
138980971201     C                   MOVEL     FLGTB1        I95TPO
138990971230     C                   MOVEL     ARBTSP        I95TSP
139000980602     C                   MOVEL     'S'           I95FRE
139010971201     C     §3AFCA        IFNE      0
139020971201     C                   MOVEL     'S'           I95FCA
139030971201     C                   ENDIF
139040970808     C                   MOVEL     'S'           I13AF0
139050140902     c                   if        d48ffr<>'E'
139060970808     C                   MOVEL     'S'           I13CNV
139070140807     c                   endif
139080970808     C                   MOVEL     'S'           I13SZV
139090141119     c* in caso di chiamata cieca non è da impostare: non si deve aprire la int località
139100141119     c                   if        d48ffr<>'E'
139110970808     C                   MOVEL     'S'           I13AF1
139120141119     c                   else
139130141119     C                   MOVEL     ' '           I13AF1
139140141119     c                   endif
139150970808     C                   MOVEL     'S'           I13SZ2
139160970808     C*
139170970808     C* SE CAMBIATO CAP PULISCO CAMPI PER FORZATURE
139180970808     C* ** DESTINATARIO **
139190970808     C     WCLI          IFEQ      'D'
139200970808    2C     E14CAP        IFNE      VARCAD
139210970808     C                   CLEAR                   WZV
139220970808     C                   CLEAR                   WZ1
139230970808     C                   CLEAR                   WZ2
139240970808     C                   MOVEL     E14CAP        VARCAD
139250970808    2C                   ENDIF
139260970808     C* MODIFICA LA LOCALITA'
139270970808    2C     E14LOC        IFNE      VARLOD
139280970808     C                   MOVEL     E14LOC        VARLOD
139290970808     C                   CLEAR                   WZ1
139300970808     C                   CLEAR                   WZ2
139310970808    2C                   ENDIF
139320970808     C                   MOVEL     WZV           E13FZV
139330970808     C                   MOVEL     WZ1           E13FZ1
139340970808     C                   MOVEL     WZ2           E13FZ2
139350020305     c     lnantw        ifeq      'FED'
139360020305     c                   movel     'S'           i95fi1
139370020305     c                   endif
139380970808     C                   ELSE
139390970808     C* ** MITTENTE **
139400970808    2C     E14CAP        IFNE      VARCAM
139410970808     C                   CLEAR                   WZVM
139420970808     C                   CLEAR                   WZ1M
139430970808     C                   CLEAR                   WZ2M
139440970808     C                   MOVEL     E14CAP        VARCAM
139450970808    2C                   ENDIF
139460970808     C* MODIFICA LA LOCALITA'
139470970808    2C     E14LOC        IFNE      VARLOM
139480970808     C                   MOVEL     E14LOC        VARLOM
139490970808     C                   CLEAR                   WZ1M
139500970808     C                   CLEAR                   WZ2M
139510970808    2C                   ENDIF
139520020305     c     lnpntw        ifeq      'FED'
139530020305     c                   movel     'S'           i95fi1
139540020305     c                   endif
139550970808     C                   MOVEL     WZVM          E13FZV
139560970808     C                   MOVEL     WZ1M          E13FZ1
139570970808     C                   MOVEL     WZ2M          E13FZ2
139580970808     C                   END
139590970808     C*
139600970808     C**
139610970808     C** CALL
139620970808     C                   CALL      'FNLV13R'
139630970808     C                   PARM                    KPJBA
139640030612     C                   PARM                    Fnlv13DS
139650030612     C                   PARM                    Tisi95DS
139660970808     C*
139670970808     C                   MOVEL     O13ERR        WERR              1
139680970808     C                   MOVEL     O13MSG        VIDMSG
139690970808     C                   EXSR      CTRERR
139700970808     C*
139710970808     C* REIMPOSTO LE FORZATURE
139720970808     C     WCLI          IFEQ      'D'
139730970808     C                   MOVEL     E13FZV        WZV
139740970808     C                   MOVEL     E13FZ1        WZ1
139750970808     C                   MOVEL     E13FZ2        WZ2
139760970808     C                   ELSE
139770970808     C                   MOVEL     E13FZV        WZVM
139780970808     C                   MOVEL     E13FZ1        WZ1M
139790970808     C                   MOVEL     E13FZ2        WZ2M
139800970808     C                   END
139810150203     c* memorizzo la località normalizzata per destinatario
139820150203     c                   if        wcli='D'
139830150203     c                   eval      normlod=o95loc
139840150203     c                   endif
139850970808     C     ENDIND        ENDSR
139860070320     C*
139870070320     C*--- Gestisce record"L"di FIAR4 --------------------------------*
139880070320     C     GestAR4L      BEGSR
139890070320     c* lo faccio solo se GEO attiva
139900070320    0c                   if        FGSGEOT='S'
139910070320     c* Se devo chainare
139920070320     C                   MOVEL     'L'           WTRC
139930070320     c
139940070321    1c                   if        TipoEla='C'
139950070320     c                   clear                   Wesar4L
139960070320     C     KARB2         CHAIN     FiAR4000                           3091
139970070320    2c                   if        *in91
139980070320     C                   SETON                                        28
139990070320     C                   MOVEL     MSG(4)        VIDMSG
140000070320     C                   GOTO      ENDar4L
140010070320    2c                   endif
140020070320     c
140030070320    2c                   if        not *in30
140040070320     c                   movel     'E'           WEsar4L           1
140050070320     c                   movel     ar4not        dsbl4L
140060070320     c                   else
140070070320     c                   movel     'N'           WEsar4L
140080070320     c                   clear                   dsbl4L
140090070320     c                   clear                   fiar4000
140100070320    2c                   endif
140110070320    1c                   endif
140120070320     c
140130070320    1c                   if        tipoela='A'
140140070320     c                   movel     dsbl4l        ar4not
140150070320     c                   movel     dateu         ar4duv
140160070320     c                   movel     dateu         ar4DTR
140170070320     c                   movel     'T'           ar4ftr
140180070320     c
140190070320    3c                   if        wesar4L='N'
140200070320     c                   eval      ar4aas=arbaas
140210070320     c                   eval      ar4lnp=arblnp
140220070320     c                   eval      ar4nrs=arbnrs
140230070320     c                   eval      ar4nsp=arbnsp
140240070320     c                   eval      ar4trc='L'
140250070320     c                   write     fiar4000
140260070320   x3c                   else
140270070320     c                   update    fiar4000
140280070320    3c                   endif
140290070320    1c                   endif
140300070320     c
140310070320    0c                   endif
140320070320     c
140330070320     C     ENDar4L       ENDSR
140340970808     C*
140350970808     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
140360970808     C     CTRERR        BEGSR
140370970808     C* IMPOSTO CAMPI PASSATI
140380970808    1C     O13RON        IFEQ      'S'
140390970808     C                   MOVEL     O95NAR        E14NAR
140400150112     c                   if        d48ffr='E'
140410150112     c                   clear                   werr
140420150112     c                   eval      msgforzatxt=o13msg
140430150112     c                   endif
140440970808    1C                   ENDIF
140450970808    1C     O13ROC        IFEQ      'S'
140460970808     C                   MOVEL     O95CAP        E14CAP
140470150112     c                   if        d48ffr='E'
140480150112     c                   clear                   werr
140490150112     c                   eval      msgforzatxt=o13msg
140500150112     c                   endif
140510970808    1C                   ENDIF
140520970808    1C     O13ROP        IFEQ      'S'
140530970808     C                   MOVEL     O95PRV        E14PRV
140540150112     c                   if        d48ffr='E'
140550150112     c                   clear                   werr
140560150112     c                   eval      msgforzatxt=o13msg
140570150112     c                   endif
140580970808    1C                   ENDIF
140590970808    1C     O13ROL        IFEQ      'S'
140600970808     C                   MOVEL     O95LOC        E14LOC
140610150112     c                   if        d48ffr='E'
140620150112     c                   clear                   werr
140630150112     c                   eval      msgforzatxt=o13msg
140640150112     c                   endif
140650970808    1C                   ENDIF
140660970808     C* ERRORE O RICERCA CAMPO
140670970808    1C     WERR          IFNE      ' '
140680970808     C*
140690970808    2C     VIDMSG        IFNE      *BLANKS
140700970808     C                   SETON                                        28
140710970808    2C                   ENDIF
140720970808     C* POSIZIONAMENTO CURSORE
140730970808    2C                   SELECT
140740970808     C     WERR          WHENEQ    '1'                                          R.SOCIALE
140750970808     C                   SETON                                        40
140760970808     C     WERR          WHENEQ    '2'                                          INDIRIZZO
140770970808     C                   SETON                                        41
140780970808     C     WERR          WHENEQ    '5'                                          CAP
140790970808     C     WCLI          IFEQ      'D'
140800970808     C                   SETON                                        43
140810970808     C                   ELSE
140820970808     C                   SETON                                        42
140830970808     C                   END
140840970808     C     WERR          WHENEQ    '3'                                          LOCALITA
140850970808     C                   SETON                                        43
140860970808     C     WERR          WHENEQ    '4'                                          PROVINCIA
140870970808     C     WCLI          IFEQ      'D'
140880970808     C                   SETON                                        50
140890970808     C                   ELSE
140900970808     C                   SETON                                        45
140910970808     C                   END
140920970808     C     WERR          WHENEQ    '6'                                          NAZIONE
140930970808     C                   SETON                                        44
140940970808     C     WERR          WHENEQ    '7'                                          PARTITA IVA
140950970808     C     WCLI          IFEQ      'D'
140960970808     C                   SETON                                        45
140970970808     C                   ELSE
140980970808     C                   SETON                                        49
140990970808     C                   END
141000970808     C     WERR          WHENEQ    '8'                                          COD.ISO
141010970808     C     WCLI          IFEQ      'D'
141020970808     C                   SETON                                        46
141030970808     C                   ELSE
141040970808     C                   SETON                                        50
141050970808     C                   END
141060970808     C     WERR          WHENEQ    '9'                                          INOLTRO
141070970808     C     WCLI          ANDEQ     'D'
141080970808     C                   SETON                                        49
141090020318     c     werr          wheneq    '0'
141100020318     C     WCLI          IFEQ      'D'
141110020318     C                   SETON                                        50
141120020318     C                   ELSE
141130020318     C                   SETON                                        45
141140020318     C                   END
141150020318     c
141160970808    2C                   ENDSL
141170970808     C*
141180970808     C                   SETON                                        90
141190970808    1C                   ENDIF
141200970808     C                   ENDSR
141210091217     c*
141220091217     c* Aggiornamento per record GEN ddel fiar5 --------------------------------*
141230091217     c     RegisAR5GEN   BEGSR
141240091217     c                   clear                   dar5gen
141250091217     c                   Eval      kAr5Trd = 'GEN'
141260091217     c     kAr5          Chain     Fiar501l
141270091217      * Aggiorno Fiar5
141280091217if  1c                   If        %Found(Fiar501l)
141290091217     c                   movel     ar5uni        DAr5Gen
141300091217     c*
141310091217     c* Aggiorno tipo incasso in arrivo
141320091217    2c                   select
141330091217     c                   when      Wtipoagg='TIC'
141340091217     c                   Eval      §Ar5ticmb=vidtic
141350100119     c                   eval      §ar5flgMB='S'
141360091217     c
141370091217     c                   when      Wtipoagg='REF'
141380091217     c                   Eval      §Ar5Ref = VidNom
141390091217     c                   Eval      §Ar5Teld = VidTel
141400140403     c                   when      Wtipoagg='PIK'
141410140407     c                   Eval      §Ar5pk2 = wpk2
141420091217    2c                   ENDSL
141430091217
141440091217     c                   Movel(p)  DAr5Gen       Ar5Uni
141450091217     c
141460091217      * se devo trasmettere alla sede sfleggo per la trasmissione
141470091217    2c                   If        §Ar5Trse = 'S'
141480091217     c                   Clear                   Ar5Ft2
141490091217    2c                   EndIf
141500091217     c                   Clear                   Ar5Ft3
141510091217     c                   Update    Fiar5000
141520091217      * Scrivo Fiar5
141530091217   x1c                   Else
141540091217      * se immessi i dati
141550091217     c                   Clear                   Fiar5000
141560091217     c                   Eval      Ar5Aas = ArbAas
141570091217     c                   Eval      Ar5Lnp = ArbLnp
141580091217     c                   Eval      Ar5Nrs = ArbNrs
141590091217     c                   Eval      Ar5Nsp = ArbNsp
141600091217     c                   Eval      Ar5Trd = 'GEN'
141610091217     c                   Move      Dateu         Ar5Dac
141620091217     c                   Movel     W0140         Ar5Orc
141630091217     c                   Clear                   Dar5Gen
141640091217     c
141650091217    2c                   select
141660091217     c                   when      Wtipoagg='TIC'
141670091217     c                   Eval      §Ar5ticmb=vidtic
141680100119     c                   eval      §ar5flgMB='S'
141690091217     c
141700091217     c                   when      Wtipoagg='REF'
141710091217     c                   Eval      §Ar5Ref = VidNom
141720091217     c                   Eval      §Ar5Teld = VidTel
141730140403     c                   when      Wtipoagg='PIK'
141740140407     c                   Eval      §Ar5pk2 = wpk2
141750091217    2c                   ENDSL
141760091217
141770091217     c                   Movel(p)  DAr5Gen       Ar5Uni
141780091217    2c                   if        ar5uni<>*blanks
141790091217     c                   Write     Fiar5000
141800091217    2c                   EndIf
141810091217    1c                   EndIf
141820091217     c
141830091217     c                   ENDSR
141840091217     c
141850911212     C*--- ALLOCO OGGETTI --------------------------------------------*
141860911212     C     ALLOC         BEGSR
141870020805     C                   MOVE      ')'           VAR               7
141880020805     C                   MOVE      '))'          VAR2              8
141890020805     C                   Z-ADD     48            LUNG             15 5
141900920120     C*
141910920120     C                   MOVEA     SFILE         C20(13)
141920920120     C                   MOVEA     SFILE         C21(13)
141930920120     C                   MOVEA     SFILE         C22(13)
141940920120     C                   MOVEA     C22           SOVR(1)
141950920120     C                   MOVEA     C20           SALC(1)
141960920120     C                   MOVEA     C21           SDLC(1)
141970920120     C*
141980041027     C* solo PER SEDE
141990941212     C     §7LFFI        IFEQ      'S'
142000911212     C* CHECK OBJ E ADDPFM
142010971114     C                   Z-ADD     046           PARMBR            3 0
142020020805     c                   move      046           $mbr
142030930210     C                   MOVEL     SFILE         FISICO
142040930210     C                   CALL      'TRUL50C'
142050930210     C                   PARM                    FISICO           10
142060930210     C                   PARM                    MBRFIS           10
142070930210     C                   PARM                    LIB              10
142080930210     C                   PARM                    LOGICO           10
142090930210     C                   PARM                    MBRLOG           10
142100930210     C                   PARM                    ULFLG             1
142110911212     C** OVRDBF
142120020805     c                   movel     $mbr          var
142130920211     C                   MOVE      VAR           SOVR
142140911212     C                   MOVEL     *BLANKS       COMMAN           80
142150911218     C                   MOVEA     SOVR(1)       COMMAN
142160911212     C                   CALL      'QCMDEXC'
142170911212     C                   PARM                    COMMAN
142180911212     C                   PARM                    LUNG
142190911212     C* ALLOCO OGGETTO
142200020805     c                   movel     $mbr          var2
142210920211     C                   MOVE      VAR2          SALC
142220911212     C                   MOVEL     *BLANKS       COMMAN           80
142230911218     C                   MOVEA     SALC(1)       COMMAN
142240911213     C                   CALL      'QCMDEXC'                            91
142250911212     C                   PARM                    COMMAN
142260911212     C                   PARM                    LUNG
142270911212     C*
142280911212     C* 90 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
142290911213     C   91              GOTO      ENDALC
142300911212     C                   END
142310941212     C*
142320941212     C     ENDALC        TAG
142330941212     C* 91 ON - ERRORE
142340941212     C*
142350941212     C     *IN91         IFEQ      *ON
142360941212     C                   SETON                                        28
142370051129     C                   MOVEa     MSG(136)      k78
142380141110     C                   MOVEa     SFILE         k78(68)
142390051129     C                   MOVEa     k78           vidmsg
142400941212     C                   ENDIF
142410941212     C*
142420941212     C                   ENDSR
142430920831     C*
142440940321     C*--- AGGIORNO FILE TASSAZIONE PADRONCINI -----------------------*
142450950113     C     AGGPDS        BEGSR
142460041008     C                   MOVEL     ' '           UNO               1
142470041008     c* richiamo pgm fnlv57ds
142480041008     c                   clear                   fnlv57ds
142490041008     C* 01 ON  - VARIAZIONE DESTINATARIO
142500041008    1c     *in01         ifeq      *on
142510041008    2C     ARBRSD        IFNE      OLDRSD
142520041008     C     ARBCAD        ORNE      OLDCAD
142530041008     C     ARBFIN        ORNE      OLDFIN
142540041008     c                   eval      uno='1'
142550041008    2c                   endif
142560041008    1c                   endif
142570041008     c
142580041008    1c                   if        *in02
142590041008    2c     ARBVLF        IFNE      OLDVLF
142600041008     C     ARBPKF        orne      OLDPKF
142610041008     c                   eval      uno='1'
142620041008    2c                   endif
142630041008    1c                   endif
142640041008     c*
142650041008    1c                   if        uno='1'
142660041008     c                   eval      i57aas=arbaas
142670041008     c                   eval      i57lnp=arblnp
142680041008     c                   eval      i57nrs=arbnrs
142690041008     c                   eval      i57nsp=arbnsp
142700041008     c
142710041008     c                   select
142720041008     c* Se presenti entrambe le bolle
142730041008     c                   when      waltrabolla=' '
142740070320     c                   eval      i57drt=arbdrt
142750070320     c                   eval      i57nrt=arbnrt
142760070320     c                   eval      i57fpp=arbfpp
142770070320     c                   eval      i57prt=arbpdr
142780070320     c                   eval      i57pco=arbpdc
142790041008     c* modifica in arrivo non presente fnblp
142800041008     c                   when      *in10 and waltrabolla='N'
142810041008     c                   eval      i57pdr=arbpdc
142820041008     c                   eval      i57tbo='A'
142830041008     c* modifica in partenza non presente fnarb
142840041008     c                   when      not *in10 and waltrabolla='N'
142850070320     c                   eval      i57pdr=arbpdr
142860041008     c                   eval      i57tbo='P'
142870070320     c                   eval      i57drt=arbdrt
142880070320     c                   eval      i57nrt=arbnrt
142890070320     c                   eval      i57fpp=arbfpp
142900041008     c                   endsl
142910041008     c
142920041008     c                   eval      i57ncl=arbncl
142930041011     c   02              eval      i57tvl='F'
142940041008     c* volume vecchio
142950041008     c                   eval      i57vlc=arbvlc
142960041008     c                   eval      i57ncr=arbncr
142970041008     c                   eval      i57vlf=oldvlf
142980041008     c* volume nuovo
142990041008     c                   eval      i57vcn=arbvlc
143000041008     c                   eval      i57ncn=arbncp
143010041008     c                   eval      i57vfn=arbvlf
143020041008     c* peso vecchio
143030041008     c                   eval      i57pkc=arbpkc
143040041008     c                   eval      i57ncp=arbncp
143050041008     c                   eval      i57pkf=oldpkf
143060041008     c* peso nuovo
143070041008     c                   eval      i57pcn=arbpkc
143080041008     c                   eval      i57npn=arbncp
143090041008     c                   eval      i57pfn=arbpkf
143100041008     c* destinatario vecchio
143110041008     c                   eval      i57rsd=oldrsd
143120041008     c                   eval      i57cad=oldcad
143130041008     c                   eval      i57fin=oldfin
143140041008     c* destinatario nuovo
143150041008     c                   eval      i57rdn=arbrsd
143160041008     c                   eval      i57cdn=arbcad
143170041008     c                   eval      i57fdn=arbfin
143180041008     c
143190041008     c                   call      'FNLV57R'
143200041008     c                   parm                    kpjba
143210041008     c                   parm                    fnlv57ds
143220041008     c                   endif
143230940321     C*
143240940321     C                   ENDSR
143250950113     C*
143260950113     C* AGGIORNA PADRONCINI PER IL MITTENTE --------------------------*
143270950113     C     AGGPMT        BEGSR
143280950113     C*
143290950113    1C     ARBKSC        IFNE      OLDKSC
143300950113     C     ARBRSM        ORNE      OLDRSM
143310950113     C     ARBCAM        ORNE      OLDCAM
143320950113     C*
143330950113     C* CONTROLLO CON QUALE VOLUME HO AGGIORNATO LA BOLLA
143340950113     C                   CLEAR                   WVOL
143350950113     C*
143360950113     C* VOLUME PARZIALE "Z"
143370950113     C*
143380050111     C     ARBNCR        ifne      ARBNCL
143390950113     C*
143400950113    3C     ARBVLC        IFGE      ARBVLF
143410950113     C                   Z-ADD     ARBVLC        WVOL
143420950113   X3C                   ELSE
143430950113     C                   Z-ADD     ARBVLF        WVOL
143440950113    3C                   ENDIF
143450950113     C* VOLUME TOTALE "T"
143460950113   X2C                   ELSE
143470950113     C                   Z-ADD     ARBVLC        WVOL
143480950113    2C                   ENDIF
143490950113     C****
143500950113     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
143510011126     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
143520011126     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
143530950113     C****
143540950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
143550950113     C                   SETOFF                                       17
143560950113     C*
143570070320     C                   MOVE      arbPDR        WPDR
143580070320     C                   Z-ADD     arbDRT        WDRT
143590950113     C* NUMERO DISTINTA
143600070320    2C     arbFPP        IFEQ      'M'
143610950113     C                   Z-ADD     1000          WNDC
143620950113     C                   ELSE
143630950113     C                   Z-ADD     5000          WNDC
143640950113    2C                   ENDIF
143650070320     C                   ADD       arbNRT        WNDC
143660950113     C*
143670950113     C*
143680950113    2C                   DO        2
143690011126     C   17KFTT2         CHAIN     fifst000                           30
143700011126     C  N17KFTT2         CHAIN     fiftt000                           30
143710950113     C*
143720950113    3C     *IN30         IFEQ      *OFF
143730950113     C*
143740950113     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
143750950113    4C     FTTFVL        IFNE      'C'
143760020326     c*
143770020326     c  n17kftd5         chain     fiftd005                           30
143780020326     c   17kftd5         chain     fifsd005                           30
143790020326     c     *in30         ifeq      *off
143800020326     c                   z-add     arbpkf        ftdpkl
143810020326     c                   z-add     wvol          ftdvlu
143820020326     c                   movel     arbrsm        ftdrsc
143830020326     c* dò per scontato che la bolla sia un porto franco
143840020326     c                   z-add     arbksc        ftdksc
143850020326     c                   movel     arbcam        ftdcap
143860020326     c                   movel     arbfap        ftdfin
143870020326     c* se è stato modificato codice cliente o ragione sociale devo
143880020326     c* aggiornare anche il numero di stop
143890020327    1C     ARBKSC        IFNE      OLDKSC
143900020327     C     ARBRSM        ORNE      OLDRSM
143910020327     c                   exsr      aggstp
143920020327     c                   endif
143930020327     c  n17              update    fiftd005
143940020327     c   17              update    fifsd005
143950020326     c                   endif
143960950113     C*
143970950113     C* SE VALORIZZATA SVALORIZZO
143980950113     C                   EXSR      SVAFTT
143990950113     C*
144000950113    4C                   ENDIF
144010950113    3C                   ENDIF
144020950113     C*
144030950113     C* 17 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
144040950113     C                   SETON                                        17
144050950113    2C                   ENDDO
144060950113     C*
144070950113    1C                   ENDIF
144080950113     C*
144090950113     C                   ENDSR
144100020327     c*
144110020327     c* AGGSTP - Aggiornamento numero stop a cambio di mittente ------*
144120020327     C     AGGSTP        BEGSR
144130020327     c                   clear                   wstp
144140020327     C* ESCLUDENDO FRANCHI E ASSEGNATI VARI,
144150020327     C*   CHAINO FNCLS00F PER PRENDERE IL NUMERO DEGLI STOP
144160020327     C                   Z-ADD     ftdksc        w0040             4 0
144170020327    5C     w0040         IFNE      *ALL'8'
144180020327     C     w0040         ANDNE     *ALL'9'
144190020327     C     ftdksc        CHAIN     FNCLS000                           31
144200020327     C  N31              z-add     CLSSTP        wstp
144210020327    5C                   ENDIF
144220020327     c     wstp          ifeq      *zeros
144230020327     c* prima di fare la chain mi devo salvare i dati del record di fiftd005
144240020327     c                   movel     dsftd         sav_dsftd
144250020327     C                   MOVEL     ARBKSC        WKSC
144260020327     C                   MOVEL     ARBRSM        WRSM
144270020327     C  N17KFTD          CHAIN(N)  fiftd004                           31
144280020327     C   17KFTD          CHAIN(N)  fifsd004                           31
144290020327     c  n31              z-add     ftdstp        wstp
144300020327     c* Se il numero stop da attribuire è ancora = a 0 devo cercare
144310020327     c* nella distinta il primo numero disponibile
144320020327     c     wstp          ifeq      *zeros
144330020327     C  N17KFTT2         setgt     fiftd001
144340020327     C   17KFTT2         setgt     fifsd001
144350020327     C  N17KFTT2         readpe    fiftd001
144360020327     C   17KFTT2         readpe    fifsd001
144370020327     c                   if        not %eof
144380020327     c                   eval      wstp = (ftdstp+1)
144390020417     c     wstp          iflt      1000
144400020417     c                   z-add     1000          wstp
144410020417     c                   endif
144420020417     c                   else
144430020417     c* qui non dovrebbe mai entrare ma lo gestisco comunque anche
144440020417     c* per unifomrità con fnlsa2r
144450020417     c                   z-add     1000          wstp
144460020327     c                   endif
144470020327     c                   endif
144480020327     c*
144490020327     c* ripristino i dati del record salvato
144500020327     c                   movel     sav_dsftd     dsftd
144510020327     c                   endif
144520020327     c                   z-add     wstp          ftdstp
144530020327     C                   ENDSR
144540020326     c*
144550950113     C* SVALORIZZA UNA DISTINTA --------------------------------------*
144560950113     C     SVAFTT        BEGSR
144570950113     C* SOLO SE VALORIZZATA
144580950113    6C     FTTFVL        IFEQ      'V'
144590950113     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
144600950113     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
144610950113     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
144620950113     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
144630950113     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
144640950113     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
144650950113     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
144660041122     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
144670041122     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
144680950113     C                   MOVEL     PARAM3        KPJBU
144690011129     C  N17              CALL      'FICNE2R'
144700950113     C                   PARM                    KPJBA
144710011129     C   17              CALL      'FICNE9C'
144720950113     C                   PARM                    KPJBA
144730950113    6C                   ENDIF
144740950113     C                   ENDSR
144750941102     C*
144760941102     C* CONTROLLO DIVISA ---------------------------------------------*
144770941102     C     CTRVLT        BEGSR
144780011011      *
144790011011      * La divisa è obbligatoria se è inserito un importo
144800011011     C     W0133         IFGT      *ZEROS
144810011011     C     WVLT          ANDEQ     *BLANKS
144820011011     C                   MOVEL     MSG(100)      VIDMSG
144830011011     C                   SETON                                        9028
144840011011     C                   GOTO      ENDVLT
144850011011     C                   ENDIF
144860941102     C*
144870941102     C                   MOVEL     'CV'          COD
144880941102     C* RICERCA
144890941102     C     '?'           SCAN      WVLT                                   90
144900941102     C     *IN90         IFEQ      *ON
144910991011     C                   CLEAR                   §KEY
144920941102     C                   CALL      'X§TABER'
144930941102     C                   PARM                    CODUT
144940941102     C                   PARM                    COD
144950941102     C                   PARM                    §KEY
144960991011     C                   PARM                    §DES             25
144970941102     C                   MOVEL     §KEY          WVLT
144980941102     C                   GOTO      ENDVLT
144990941102     C                   ENDIF
145000941102     C*
145010941102     C* CONTROLLO DIVISA
145020941102     C                   MOVEL(P)  WVLT          KEY
145030941213     C                   EXSR      CTABEL
145040941102     C* ERRORE
145050941102    1C     *IN30         IFEQ      *ON
145060941102     C                   MOVEL     MSG(26)       VIDMSG
145070941102     C                   SETON                                        9028
145080941102     C                   GOTO      ENDVLT
145090941102    1C                   ENDIF
145100941102     C                   MOVEL     TBLUNI        DSCV
145110941102     C*
145120941102     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
145130941102    1C     §CVFDC        IFNE      'S'
145140941102     C                   MOVE      W0133         W0033             3 3
145150941102    2C     W0033         IFNE      0
145160941102     C                   MOVEL     MSG(27)       VIDMSG
145170941102     C                   SETON                                        9028
145180941102     C                   GOTO      ENDVLT
145190941102    2C                   ENDIF
145200941102    1C                   ENDIF
145210011010      *
145220011010      * Controllo il numero di decimali immessi
145230011011    1C     §CVFDC        IFEQ      'S'
145240011011     C     W0133         ANDNE     *ZEROS
145250011011      *
145260011011    2C                   SELECT
145270011011     C     §CVDEC        WHENEQ    3
145280011011     C     §CVDEC        WHENEQ    2
145290011011     C                   MOVE      W0133         W0010             1 0
145300011011    3C     W0010         IFNE      *ZEROS
145310011011     C                   MOVEA     MSG(93)       K78
145320011011     C                   MOVE      §CVDEC        WCVDEC            2
145330011011     C                   MOVEA     WCVDEC        K78(29)
145340011011     C                   MOVEA     K78           VIDMSG
145350011011     C                   SETON                                        9028
145360011011    3C                   ENDIF
145370011011     C     §CVDEC        WHENEQ    1
145380011011     C                   MOVE      W0133         W0020             2 0
145390011011    3C     W0020         IFNE      *ZEROS
145400011011     C                   MOVEA     MSG(93)       K78
145410011011     C                   MOVE      §CVDEC        WCVDEC            2
145420011011     C                   MOVEA     WCVDEC        K78(29)
145430011011     C                   MOVEA     K78           VIDMSG
145440011011     C                   SETON                                        9028
145450011011    3C                   ENDIF
145460011011     C     §CVDEC        WHENEQ    0
145470011011    2C                   ENDSL
145480011011      *
145490011011    1C                   ENDIF
145500941102     C*
145510941102     C     ENDVLT        ENDSR
145520941212     c*
145530941212     C*--- CHAIN TABEL00F --------------------------------------------*
145540941212     C     CTABEL        BEGSR
145550941212     C     KTAB2         CHAIN     TABEL                              30
145560941212     C  N30TBLFLG        IFNE      ' '
145570941212     C                   SETON                                        30
145580941212     C                   END
145590941212     C                   ENDSR
145600941212     C*---------------------------------------------------------------
145610941212     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
145620941212     C*---------------------------------------------------------------
145630941212     C     DLCSED        BEGSR
145640020805     c                   move      046           $mbr
145650020805     c                   movel     $mbr          var2
145660941220     C                   MOVE      VAR2          SDLC(1)
145670941212     C                   MOVEL     *BLANKS       COMMAN           80
145680941212     C                   MOVEA     SDLC(1)       COMMAN
145690941212     C                   CALL      'QCMDEXC'                            91
145700941212     C                   PARM                    COMMAN
145710941212     C                   PARM                    LUNG
145720941212     C                   ENDSR
145730941213     C*---------------------------------------------------------------
145740941213     C* STORICIZZO LA TASSAZIONE
145750941213     C*---------------------------------------------------------------
145760941213     C     STOARB        BEGSR
145770991006     C* PER LA PRIMA BOLLA KSC E CTR LI PRENDO SEMPRE DALLA BOLLA
145780991006     C* PRINCIPALE PERCHE' SONO SICURA DI AVERLA
145790991006     C* PER LA SECONDA BOLLA LI DEVO PRENDERE DA AR6 PERCHE'
145800991006     C* QUELLI DELLA BOLLA PRINCIPALE SI RIFERISCONO ALLA PRIMA BOLLA
145810991006     C     AR6TRC        IFEQ      '2'
145820991006     C                   Z-ADD     AR6KSC        ARBKSC
145830991006     C                   Z-ADD     AR6CTR        ARBCTR
145840991006     C                   ENDIF
145850991006     C                   MOVEL     AR6DIV        ARBDIV
145860941213     C                   MOVEL     AR6POR        ARBPOR
145870941213     C                   MOVEL     AR6SV1        ARBSV1
145880941213     C                   MOVEL     AR6VA1        ARBVA1
145890941213     C                   MOVEL     AR6SV2        ARBSV2
145900941213     C                   MOVEL     AR6VA2        ARBVA2
145910941213     C                   MOVEL     AR6SV3        ARBSV3
145920941213     C                   MOVEL     AR6VA3        ARBVA3
145930941213     C                   MOVEL     AR6IMV        ARBIMV
145940941213     C                   MOVEL     AR6ALI        ARBALI
145950941213     C                   MOVEL     AR6CEI        ARBCEI
145960941213     C                   MOVEL     AR6IFT        ARBIFT
145970941213     C                   MOVEL     AR6DFT        ARBDFT
145980941213     C                   MOVEL     AR6NFT        ARBNFT
145990941213     C                   MOVEL     AR6FIV        ARBFIV
146000950114     C* SE VENGO DA VARIAIZONE MITTENTE COME CODICE TARIFFA CI METTO
146010950114     C*  QUELLO SALVATO
146020950114     C   24              MOVEL     MITCTR        ARBCTR
146030991013     C                   MOVEL     AR6FIM        ARBFIM
146040011108      *
146050011112      * Metto nell'ultimo byte del campo arbpru se la bolla aggiornata
146060011108      * è in partenza 'P' o in arrivo 'A'
146070011108     C     *IN10         IFEQ      *ON
146080011108     C                   MOVE      'A'           ARBPRU
146090011108     C                   ENDIF
146100011108     C     *IN10         IFEQ      *OFF
146110011108     C                   MOVE      'P'           ARBPRU
146120011108     C                   ENDIF
146130011108      *
146140950114     C*
146150991006     C                   WRITE     FIARBT00
146160950114     C*
146170991006     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
146180941221    1C     WAR7          IFEQ      'E'
146190991006    1C     ARBTRC        ANDEQ     '1'
146200991006     C                   MOVEL     '1'           WTRC
146210991006     C     KARB2         SETLL     FIAR7000
146220991006     C     KARB2         READE(N)  FIAR7000                               30
146230941213     C*
146240941213    2C     *IN30         DOWEQ     *OFF
146250941213     C                   MOVEL     AR7SVN        ARBSVN
146260941213     C                   MOVEL     AR7VAN        ARBVAN
146270991006     C                   WRITE     FIARBU00
146280941213     C*
146290991006     C     KARB2         READE(N)  FIAR7000                               30
146300941213    2C                   ENDDO
146310941213     C                   ENDIF
146320041130      *-------
146330041130      * Seconda bolla
146340041130     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
146350041130    1C     WAR72         IFEQ      'E'
146360041130    1C     ARBTRC        ANDEQ     '2'
146370041130     C                   MOVEL     '2'           WTRC
146380041130     C     KARB2         SETLL     FIAR7000
146390041130     C     KARB2         READE(N)  FIAR7000                               30
146400041130     C*
146410041130    2C     *IN30         DOWEQ     *OFF
146420041130     C                   MOVEL     AR7SVN        ARBSVN
146430041130     C                   MOVEL     AR7VAN        ARBVAN
146440041130     C                   WRITE     FIARBU00
146450041130     C*
146460041130     C     KARB2         READE(N)  FIAR7000                               30
146470041130    2C                   ENDDO
146480041130     C                   ENDIF
146490041130      *-------
146500941213     C*
146510941213     C                   ENDSR
146520941214     C*----------------------------------------------------------------
146530941214     C* RICALCOLO IL VOLUME AUTOMATICO
146540941214     C*----------------------------------------------------------------
146550941214     C     VOLAUT        BEGSR
146560941214     C*
146570941214     C* VOLUME AUTOMATICO A PESO: PESO MODIFICATO
146580941214    3C     VIDPKF        IFNE      ARBPKF
146590941214     C     ARBFVF        ANDEQ     'K'
146600941214     C*
146610120702     C     ARBPKF        DIV(H)    ARBVLF        CLIKPM            9 1
146620941214     C     VIDPKF        DIV(H)    CLIKPM        D20VLU
146630941214     C*
146640941214     C     ARBVLB        IFEQ      0
146650941214     C                   Z-ADD     0,001         D20VLU
146660941214     C                   END
146670941214     C                   MOVEL     ARBFVF        D20FVL
146680941214     C*
146690941214    3C                   END
146700941214     C                   ENDSR
146710941215     C**************************************************************************
146720941215     C*  ABBLANCA ZERO NON SIGNIFICATIVI
146730941215     C**************************************************************************
146740941215     C     ABBL          BEGSR
146750941215     C*
146760941215     C                   SETOFF                                       99
146770941215     C     SKI(I)        DOWEQ     '0'
146780941215     C                   MOVEL     ' '           SKI(I)
146790941215     C                   ADD       1             I
146800941215     C     I             COMP      15                                     99
146810941215     C  N99              END
146820941215     C*
146830941215     C                   ENDSR
146840051114     C**************************************************************************
146850051114     C*  ABBLANCA ZERO NON SIGNIFICATIVI
146860051114     C**************************************************************************
146870051114     C     SR_chkcomb    BEGSR
146880051114     c                   clear                   wcomb             1
146890051114     C* Non è possibile indicare contemporaneamente consegne particolari
146900051117     C*  'S' ed 'X' / 'S' ed 'A' / "S" e "P" / "X" e "P"
146910051114    2C                   IF           (wTC1 = 'X'  and  wTC2 = 'S')
146920051114     C                             or (wTC1 = 'S'  and  wTC2 = 'X')
146930051114     C                             or (wTC1 = 'A'  and  wTC2 = 'S')
146940051114     C                             or (wTC1 = 'S'  and  wTC2 = 'A')
146950051114     C                             or (wTC1 = 'S'  and  wTC2 = 'P')
146960051114     C                             or (wTC1 = 'P'  and  wTC2 = 'S')
146970051117     C***                          or (wTC1 = 'X'  and  wTC2 = 'A')
146980051117     C***                          or (wTC1 = 'A'  and  wTC2 = 'X')
146990051114     C                             or (wTC1 = 'X'  and  wTC2 = 'P')
147000051114     C                             or (wTC1 = 'P'  and  wTC2 = 'X')
147010051114     c                   eval      wcomb = 'S'
147020051114     c                   endif
147030051114     C                   ENDSR
147040060227     C**************************************************************************
147050060301     C*  Richiamo pgm di richiesta/controllo motivazione variazione
147060060227     C**************************************************************************
147070060227     C     ges_mtv       BEGSR
147080060227     c                   clear                   fnlv85ds
147090060227     c                   movel     'V'           ilv85tel
147100060227     c                   if        d48ffr = 'S' or d48ffr = 'V'
147110060227     c                   clear                   ilv85wdw
147120060227     c                   else
147130060301     c                   if        d48ffr = *blanks
147140060301     c                   movel     'E'           ilv85wdw
147150060301     c                   else
147160060227     c                   movel     'N'           ilv85wdw
147170060301     c                   endif
147180060227     c                   endif
147190060227     c                   movel     d48cvb        ilv85cvb
147200060227     c                   if        d48mtv <> *blanks
147210060227     c                   movel     d48mtv        ilv85mtv
147220060227     c                   movel     d48not        ilv85nt1
147230060227     c                   move      d48not        ilv85nt2
147240060227     c                   endif
147250060227     c                   movel     fnlv85ds      kpjbu
147260060302     c                   call      'FNLV85R'
147270060227     c                   parm                    kpjba
147280060227     c                   movel     kpjbu         fnlv85ds
147290060227     c                   if        olv85err = '1'
147300060227     c                   seton                                        90
147310060227     C                   eval      vidmsg='Motivazione variazione mancante o er-
147320060227     c                             rata'
147330060227     c                   endif
147340060301     c                   if        olv85f12 = ' ' and olv85err = ' '
147350060301     c                   movel     'N'           w48mct
147360060301     c                   endif
147370060227     c                   endsr
147380060301     C**************************************************************************
147390060301     C*  Richiamo pgm per storicizzazione motivazione variazione
147400060301     C**************************************************************************
147410060301     C     sto_mtv       BEGSR
147420060303     c                   clear                   fnlv85ds
147430060301     c                   movel     ' '           ilv85tla
147440060301     c                   movel     'R'           ilv85tel
147450060301     c                   movel     d48cvb        ilv85cvb
147460060301     c                   z-add     arbdtv        ilv85dac
147470060301     c                   z-add     arborv        ilv85orc
147480060301     c                   z-add     arbaas        ilv85aas
147490060301     c                   z-add     arblnp        ilv85lnp
147500060301     c                   z-add     arbnrs        ilv85nrs
147510060301     c                   z-add     arbnsp        ilv85nsp
147520060303     c                   if        d48mtv <> *blanks and
147530060303     c                             (d48ffr = 'E' or d48mct = 'N')
147540060303     c                   movel     d48mtv        ilv85mtv
147550060303     c                   movel     d48not        ilv85nt1
147560060303     c                   move      d48not        ilv85nt2
147570060303     c                   endif
147580060301     c                   movel     fnlv85ds      kpjbu
147590060301     c                   call      'FNLV85R'
147600060301     c                   parm                    kpjba
147610060301     c                   endsr
147620120330     C**************************************************************************
147630120330     C*  AGGIORNAMENTO DA MANUTENZIONE BOLLE DI SEDE (CAUS.VAR.BOLLA "SD")
147640120330     C**************************************************************************
147650120330     C     GESSD         BEGSR
147660120330     c                   clear                   errp
147670120330     c                   clear                   erra
147680120419     c                   clear                   savcpi_p
147690120419     c                   clear                   savksc_p
147700120419     c                   clear                   savctr_p
147710120419     c                   clear                   savcpi_a
147720120419     c                   clear                   savksc_a
147730120419     c                   clear                   savctr_a
147740120330     c* chaino le bolle in partenza e in arrivo
147750120330     C     KARB          CHAIN(e)  FNBLP01L
147760120402    1c                   if        %error
147770120330     c                   eval      errp='A'
147780120402   x1c                   else
147790120402    2c                   if        not %found(fnblp01l)
147800120330     c                   eval      errp='N'
147810120418     c                   else
147820120418     c                   eval      savcpi_p=arbcpi
147830120418     c                   eval      savksc_p=arbksc
147840120418     c                   eval      savctr_p=arbctr
147850120402    2c                   endif
147860120402    1c                   endif
147870120330     C     KARB          CHAIN(e)  FNARB01L
147880120402    1c                   if        %error
147890120330     c                   eval      erra='A'
147900120402   x1c                   else
147910120402    2c                   if        not %found(fnarb01l)
147920120330     c                   eval      erra='N'
147930120413     c                   else
147940120413     c                   eval      savcpi_a=arbcpi
147950120418     c                   eval      savksc_a=arbksc
147960120418     c                   eval      savctr_a=arbctr
147970120402    2c                   endif
147980120402    1c                   endif
147990120402     c* Una delle due bolle allocate --> restituisco messaggio di avviso
148000120402    1c                   if        errp='A' or erra='A'
148010120402     c                   eval      d48err='A'
148020120402     c                   eval      d48msg='Bolla di filiale allocata da altro l+
148030120418     c                             avoro:NON AGGIORNATA in'
148040120418    2c                   if        errp='A'
148050120418     c                   eval      d48msg=%trim(d48msg)+' Partenza'
148060120418    3c                   if        erra='A'
148070120418     c                   eval      d48msg=%trim(d48msg)+' e'
148080120418    3c                   endif
148090120418    2c                   endif
148100120418    2c                   if        erra='A'
148110120418     c                   eval      d48msg=%trim(d48msg)+' Arrivo'
148120120418    2c                   endif
148130120402    1c                   endif
148140120330     c* se entrambe le bolle allocate/inesistenti vado a fine
148150120402    1c                   if        errp<>*blanks and erra<>*blanks
148160120330     c                   leavesr
148170120402    1c                   endif
148180120404     c*Variazione di PESO
148190120404    1c                   if        §bdpkf<>arbpkf
148200120402     c                   z-add     §bdpkf        arbpkf
148210120404    1c                   endif
148220120404     c*Variazione di VOLUME: il volume, in sede, può essere variato solo da
148230120404     c* profilo di sede. Siccome in questi casi la variazione avviene solitamente
148240120404     c* quando in filiale la bolla non c'è più sostituisco sempre
148250120404    1c                   if        §bdvlf<>arbvlf or §bdfvf<>arbfvf
148260120404     c                   z-add     §bdvlf        arbvlf
148270120411     c                   movel     §bdfvf        arbfvf
148280120404    1c                   endif
148290120529     c* IMOPORTO DA ASSICURARE
148300120529     c                   if        §btias<>arbias or §btvas<>arbvas
148310120529     c                   z-add     §btias        arbias
148320120529     c                   movel     §btvas        arbvas
148330120529     c                   endif
148340120404     c* Variazione TASSAZIONE: vengono caricate solo se c'è corrispodenza
148350120404     c* fra sede e filiale al livello di tipo di fattura: Segue Fattura o
148360120404     c* Fattura Immediata
148370120411     c* Chaino fiar6 per vedere se S.F. o  F.I.:
148380120404     c                   clear                   wfatfil
148390120404     c                   clear                   ar6ksc
148400120411     c                   clear                   war6
148410120416     c                   clear                   fiar6000
148420120416     C                   MOVEL     §bttrc        WTRC
148430120404     c     karb2         chain     fiar601l
148440120411    1c                   if        %found(fiar601l) and ar6nft= 0 or
148450120411     c                             not %found(fiar601l)
148460120404     c                   eval      wfatfil='SF'
148470120411     c                   if        not %found(fiar601l)
148480120411     c                   eval      war6='N'
148490120411     c                   endif
148500120404     c                   else
148510120404     c                   eval      wfatfil='SD'
148520120404    1c                   endif
148530120404    1c                   if        d48cvb=wfatfil
148540120404     c
148550120404     c* . . . . . . . QUANTITA' DA FATTURARE
148560120411    2c                   if        §btqft<>arbqft
148570120411     c                   z-add     §btqft        arbqft
148580120404    2c                   endif
148590120404     c* . . . . . . . CLIENTE DI FATTURAZIONE e COD.TARIFFA
148600120419     C*
148610120404     C                   MOVEL     '3A'          COD
148620120404     C                   MOVEL(P)  ARBCBO        KEY
148630120404     C                   EXSR      CTABEL
148640120404     C  N30              MOVEL     TBLUNI        DS3A
148650120404     C   30              CLEAR                   DS3A
148660120413     c* assegnato
148670120404    2c                   if        §bttrc='2' or %subst(§3atb1:1:1)='A'
148680120404    3c                   if        §btksc<>ar6ksc
148690120404     c                   eval      ar6ksc=§btksc
148700120404    3c                   endif
148710120404    3c                   if        §btctr<>ar6ctr
148720120404     c                   eval      ar6ctr=§btctr
148730120404    3c                   endif
148740120418     c* Se assegnato no seconda bolla devo aggiornare su arb anche il ksc/ctr
148750120418    2c                   if        %subst(§3atb1:1:1)='A'
148760120418    3c                   if        §btksc<>savksc_a
148770120418     c                   eval      savksc_a=§btksc
148780120418    3c                   endif
148790120418    3c                   if        §btctr<>savctr_a
148800120418     c                   eval      savctr_a=§btctr
148810120418    3c                   endif
148820120418     c                   endif
148830120404   x2c                   else
148840120413     c* franco
148850120413     c*  se non codificato non faccio niente
148860120413     c                   z-add     §btksc        w0040
148870120418    3c                   if        w0040<>8888 and §btksc<>arbksc
148880120413     c                   exsr      gessd_ksc
148890120411    3c                   endif
148900120411    3c                   if        §btctr<>arbctr
148910120411     c                   eval      arbctr=§btctr
148920120411    3c                   endif
148930120404    2c                   endif
148940120404     c* . . . . . . . CAMPI DI TASSAZIONE
148950120404     c* . . . . Divisa
148960120404     c                   if        §btdiv<>ar6div
148970120404     c                   eval      ar6div=§btdiv
148980120404     c                   endif
148990120404     c* . . . . Porto
149000120404     c                   if        §btpor<>ar6por
149010120404     c                   eval      ar6por=§btpor
149020120404     c                   endif
149030120411     c* . . . . Imponibile IVA
149040120411     c                   if        §btimv<>ar6imv
149050120411     c                   eval      ar6imv=§btimv
149060120411     c                   endif
149070120411     c* . . . . Causale esenzione
149080120411     c                   if        §btcei<>ar6cei
149090120411     c                   eval      ar6cei=§btcei
149100120411     c                   endif
149110120404     c* . . . . Varie - In §SV e §VA ci sono le varie ricevute dal chiamante
149120120404     c*                 IN WSV e WVA carico le varie presenti sul fiar6/7
149130120411     C                   MOVEL     §sv(1)        ar6sv1
149140120411     C                   Z-ADD     §va(1)        ar6va1
149150120411     C                   MOVEL     §sv(2)        ar6sv2
149160120411     C                   Z-ADD     §va(2)        ar6va2
149170120411     C                   MOVEL     §sv(3)        ar6sv3
149180120411     C                   Z-ADD     §va(3)        ar6va3
149190120411     c* cancello varie su fiar7 se presenti
149200120419     C                   MOVEL     §bttrc        WTRC
149210120411     c     karb2         setll     fiar7000
149220120411     c     karb2         reade     fiar7000
149230120411     c                   dow       not %eof(fiar701l)
149240120411     C                   DELETE    FIAR7000
149250120411     c     karb2         reade     fiar7000
149260120411     c                   enddo
149270120411     c* se presenti più di 3 verie le memorizzo nell'fiar7
149280120411    2c                   if        §sv(4)<>' '
149290120411     c                   z-add     4             inx
149300120411     c                   clear                   AR7ATB
149310120411     c                   z-add     d48aas        AR7AAS
149320120411     c                   z-add     d48lnp        AR7LNP
149330120411     c                   z-add     d48nrs        AR7NRS
149340120411     c                   z-add     d48nsp        AR7NSP
149350120419     c                   movel     §bttrc        AR7TRC
149360120411     c                   dow       §sv(inx)<>*blanks or inx>20
149370120411     C                   MOVEL     §sv(inx)      ar7svn
149380120411     C                   z-add     §va(inx)      ar7van
149390120411     C                   write     fIAR7000
149400120411     c                   add       1             inx
149410120411     c                   enddo
149420120411    2C                   endif
149430120416     c* Aggiornamento FIAR6
149440120416     c* se record non esiste lo creo solo se è pieno almeno uno dei campi
149450120416    2c                   if        war6='N'
149460120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
149470120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
149480120416     c                             or ar6cei<>*blanks
149490120411     c                   eval      AR6AAS=d48aas
149500120411     c                   eval      AR6LNP=d48lnp
149510120411     c                   eval      AR6NRS=d48nrs
149520120411     c                   eval      AR6NSP=d48nsp
149530120411     c                   eval      AR6TRC=§bttrc
149540120411
149550120411     c                   write     fiar6000
149560120416    3c                   endif
149570120417     c                   else
149580120416    3c                   if        ar6ksc>0 or ar6por>0 or ar6sv1<>*blanks
149590120416     c                             or ar6sv2<>*blanks or ar6sv3<>*blanks
149600120416     c                             or ar6cei<>*blanks
149610120404     c                   except    ar6sd
149620120416     c                   else
149630120416     c                   delete    fiar6000
149640120416     c                   endif
149650120416    2c                   endif
149660120404   x1c                   else
149670120404     c                   unlock    fiar601l
149680120404    1c                   endif
149690120404     c* aggiornamento file
149700120418     c* BLP
149710120404    1c                   if        errp=*blanks
149720120418     c                   eval      arbcpi=savcpi_p
149730120418     c* solo se assegnato rimetto il ksc/ctr che c'era
149740120418     c                   if        %subst(§3atb1:1:1)='A'
149750120418     c                   eval      arbksc=savksc_p
149760120418     c                   eval      arbctr=savctr_p
149770120418     c                   endif
149780120404     c                   except    arbsdblp
149790120404    1c                   endif
149800120418     c* ARB
149810120404    1c                   if        erra=*blanks
149820120416     c* P.iva DEL MITTENTE SU ARB LA AGGIORNO SOLO SE FRANCO NO DOPPIA BOLLA
149830120419     c* in caso contrario tengo la P.IVA che c'era
149840120416     c                   eval      arbcpi=savcpi_a
149850120418     c*
149860120418     c                   if        %subst(§3atb1:1:1)='A'
149870120418     c                   eval      arbksc=savksc_a
149880120418     c                   eval      arbctr=savctr_a
149890120418     c                   endif
149900120404     c                   except    arbsdarb
149910120404    1c                   endif
149920120330     c
149930120330     C                   ENDSR
149940120413     C**************************************************************************
149950120413     C*  AGGIORNAMENTi derivanti da variazioni in sede del ksc di bolla FRANCO
149960120413     C**************************************************************************
149970120413     C     GESSD_ksc     BEGSR
149980120416     C* Controllo se esiste record W di fiar4 per eventuale modifica di mittente
149990120416     C                   MOVEL     'W'           WTRC
150000120416     c                   clear                   wbl4w
150010120416     C     KARB2         CHAIN(N)  Fiar401l
150020120416     c                   if        %found(fiar401l)
150030120416     C                   MOVEL     'S'           WBL4W             1
150040120416     c                   endif
150050120416     c                   z-add     arbksc        oldksc
150060120416     c*
150070120413     c                   eval      arbksc=§btksc
150080120413     c* recupero dati del cliente da anagrafica
150090120413     C                   clear                   TIBS69DS
150100120413     C                   z-add     §btksc        I69kac
150110120413     C                   z-add     §btksc        I69kin
150120120413     C                   call      'TIBS69R'
150130120413     C                   parm                    tibs69DS
150140120413     C                   parm                    ds_cnaco
150150120413     C                   parm                    ds_cnind
150160120413     C                   parm                    ds_cnclp
150170120413     C                   parm                    ds_fncls
150180120413     c                   eval      arbrsm=bs_acorag
150190120413     c                   eval      arbinm=bs_indvia
150200120413     c                   eval      arbLOM=bs_indcit
150210120413     c                   eval      arbCAM=bs_indcae
150220120413     c                   eval      arbPRM=bs_indprv
150230120413     c                   eval      arbNZM=bs_indsta
150240120418     c* P.IVA: non aggiorno arbcpi perchè comune per blp e arb e non va bene:
150250120418     c* se bolla doppia su blp c'è la p.iva del mittente e su arb la p.iva del dest
150260120413     c* dell'aggiornamento di FNARB
150270120419     c                   if        %subst(§3atb1:1:1)='F' and §3atb2=*blanks
150280120419     c                   eval      savcpi_a=bs_indiva
150290120419     c                   endif
150300120419     c                   eval      savcpi_p=bs_indiva
150310120416     c                   exsr      up_fiar4P
150320120416     c                   exsr      up_fiar4w
150330120416     c*
150340120413     c                   endsr
150350120413     C**************************************************************************
150360120413     C* AGGIORNAMENTO RECORD P.IVA IN FIAR4  se esiste
150370120413     C**************************************************************************
150380120413     C     up_fiar4P     BEGSR
150390120413     C                   MOVEL     'P'           WTRC
150400120413     C     KARB2         CHAIN     FiAR401L                           30
150410120413     C     *IN30         IFEQ      *OFF
150420120413     c                   movel     ar4not        wcpifile
150430120413     c                   if        wcpifile <> arbcpi
150440120413     c                   movel     'T'           ar4ftr
150450120413     c                   z-add     dateu         ar4duv
150460120413     c                   z-add     dateu         ar4dtr
150470120413     C                   MOVEl     ARBCPI        AR4NOT
150480120413     C                   UPDATE    FiAR4000
150490120413     c                   else
150500120413     c                   unlock    fiar401l
150510120413     c                   endif
150520120413     C                   END
150530120413     c                   endsr
150540120416     C**************************************************************************
150550120416     C* Gestione record fiar4 record W per DATA TRASMISSIONE VAB
150560120416     C**************************************************************************
150570120416     C     up_fiar4W     BEGSR
150580120416     C* CONTROLLO SE C'E' NUOVO CODICE O VARIATO PER AGGIORNARE O ELIM
150590120416     C*  O SCRIVERE RECORD W IN ar4
150600120416     C                   MOVEL     '3Q'          COD
150610120416     C                   MOVEL(P)  ARBKSC        KEY
150620120416     C     KTAB2         CHAIN     TABEL00F                           30
150630120416     C  N30TBLFLG        IFNE      ' '
150640120416     C                   SETON                                        30
150650120416     C                   ENDIF
150660120416     C*
150670120416     C* C'E' 3Q DEVO CREARE O VARIARE
150680120416    1C  N10*IN30         IFEQ      *OFF
150690120416    2C     WBL4W         IFEQ      ' '
150700120416    3C     ARBKSC        orNE      OLDKSC
150710120416     C                   CLEAR                   DSBL4W
150720120416     C                   CLEAR                   fnlv19ds
150730120416     C                   MOVEL     DSBL4W        d19nt1
150740120416     C                   MOVEL     ARBAAS        d19AAS
150750120416     C                   MOVEL     ARBLNP        d19LNP
150760120416     C                   MOVEL     ARBNRS        d19NRS
150770120416     C                   MOVEL     ARBNSP        d19NSP
150780120416     C                   MOVEL     'W'           d19TRC
150790120416     C                   MOVEL     'T'           d19ftr
150800120416     C                   call      'FNLV19R'
150810120416     c                   parm                    fnlv19ds
150820120416    2C                   ENDIF
150830120416     C*
150840120416   X1C                   ELSE
150850120416     C* SE NO CI DEVE ESERE E PRIMA C'ERA LO DELETO
150860120416    2C     WBL4W         IFEQ      'S'
150870120416     c                   clear                   fnlv19ds
150880120416     C                   clear                   d19nt1
150890120416     c                   clear                   d19nt2
150900120416     C                   MOVEL     arbAAS        d19AAS
150910120416     C                   MOVEL     arbLNP        d19LNP
150920120416     C                   MOVEL     arbNRS        d19NRS
150930120416     C                   MOVEL     arbNSP        d19NSP
150940120416     C                   MOVEL     'W'           d19TRC
150950120416     c                   movel     'T'           d19ftr
150960120416     C                   CALL      'FNLV19R'
150970120416     C                   PARM                    fnlv19ds
150980120416    2C                   ENDIF
150990120416    1C                   ENDIF
151000120416     c                   endsr
151010130321     C**************************************************************************
151020130411     C* Invio Variazione bolla a PDA
151030130321     C**************************************************************************
151040130411     C     sr_pda        BEGSR
151050140908     c* Solo se la chiamata non è cieca:
151060140908     c                   if        d48ffr<>'E'
151070141020     c****               if        d66pda<>' ' and (*in10 or waltrabolla=' ')
151080141020     c                   if        d66pda<>' ' and *in10
151090131227     c                             and wpda_ndc>0 and wpda_dcm=0
151100131227     c* richiamo pgm per verificare che ci sia stata un'effettiva variazione
151110131227     c                   clear                   fnlva1ds
151120131227     c                   eval      IA1AAS=arbaas
151130131227     c                   eval      IA1LNP=arblnp
151140131227     c                   eval      IA1NRS=arbnrs
151150131227     c                   eval      IA1NSP=arbnsp
151160131227     c                   eval      IA1DTV=arbdtv
151170131227     c                   eval      IA1ORV=arborv
151180131227     c                   eval      IA1CVB=arbcvb
151190131227     c                   eval      IA1PRU=arbpru
151200131227     c                   movel     fnlva1ds      kpjbu
151210131227     c                   call      'FNLVA1R'
151220131227     c                   parm                    kpjba
151230131227     c                   movel     kpjbu         fnlva1ds
151240131227     c*
151250131227     c                   if        OA1ESITO='V'
151260140130     c* se filiale partita con nuova gestione telefonate richiamo pgm gestione
151270140130     c* note che si preoccuperà anche del richiamo a fidg42r
151280140130     c                   if        *in87=*off
151290140130     c                   clear                   fnlrn6ds
151300140130     c                   eval      in6mod='I'
151310140130     c                   eval      in6aas=arbaas
151320140130     c                   eval      in6lnp=arblnp
151330140130     c                   eval      in6nrs=arbnrs
151340140130     c                   eval      in6nsp=arbnsp
151350140130     c                   eval      in6pgm='FNLR48R'
151360140130     c                   eval      IN6FGS=arbifp
151370140130     c                   eval      IN6NFV=wpda_ndc
151380140130     c                   eval      IN6DFV=wpda_ddc
151390140130     c                   eval      IN6PDR=arbpdc
151400140130     c                   eval      IN6VAR=d66pda
151410140130     c                   eval      IN6CONS='S'
151420140130     c                   call      'FNLRN6R'
151430140130     C                   parm                    kpjba
151440140130     C                   parm                    fnlrn6ds
151450140130     c                   else
151460140130     c* altrimenti richiamo direttemente fidg42r
151470131227     c
151480130411     c                   z-add     arbifp        w0030             3 0
151490130411     c     w0030         chain     azorg01l
151500130411     c                   if        %found(azorg01l)
151510130411     c                   eval      og148=orgde8
151520131227     c                   if        §ogpdacon<>' '
151530130411     c                   clear                   fidg42ds
151540130411     c                   eval      CO42FGS=arbifp
151550130411     c                   eval      CO42NDC=wpda_ndc
151560130411     c                   eval      CO42DDC=wpda_ddc
151570130411     c                   eval      CO42AAS=arbaas
151580130411     c                   eval      CO42LNP=arblnp
151590130411     c                   eval      CO42NRS=arbnrs
151600130411     c                   eval      CO42NSP=arbnsp
151610130411     c                   eval      CO42CMT='N'
151620130411     c                   movel     fidg42ds      kpjbu
151630130411     c                   call      'FIDG42R'
151640130411     c                   parm                    kpjba
151650130411     c                   endif
151660130411     c                   endif
151670140130     c                   endif
151680131227     c                   endif
151690130411     c                   endif
151700140908     c                   endif
151710130411     c                   endsr
151720141021     C**************************************************************************
151730141021     C* Gestione finestra per richiesta dirottamento
151740141021     C**************************************************************************
151750141021     C     ges_wdw02     BEGSR
151760141021     c                   clear                   winf12
151770141022     c                   eval      atrf21=rimm
151780141125     c                   setoff                                       782879
151790141125     c* iN partenza non si può mai variare senza il dirottamento --> disabilito l'F8
151800141125     c                   if        not *in10
151810141125     c                   seton                                        79
151820141125     c                   endif
151830141022     c                   eval      w02txt='La Linea di Arrivo desunta è diversa-
151840141022     c                              dalla Linea di Arrivo della bolla'
151850141029     c                   exsr      sr_NoAbiDir
151860141021    6c                   do        *hival
151870141021     c                   exfmt     lr48w02
151880141113     c  n78              setoff                                       28
151890141113     c  n78              clear                   w02msg
151900141021     c                   select
151910141021     c* F21=Richiesta dirottamento
151920141021     c                   when      *inkv
151930141021     c                   eval      wdir='1'
151940141021     c* F12=Ritorno
151950141021     c                   when      *inkl
151960141021     c                   eval      winf12='1'
151970141021     c                   endsl
151980141106     c                   if        *inkv or *inkh or *inkl
151990141021     c                   leave
152000141021     c                   endif
152010141021    6c                   enddo
152020141113     c                   setoff                                       7828
152030141021     c                   endsr
152040141029     C**************************************************************************
152050141029     C* Verifica condizioni che disabilitano F21=Dirottamento
152060141029     C**************************************************************************
152070141029     C     sr_NoAbiDir   BEGSR
152080141029
152090141029     c*******************************************
152100141029     c* Casi di disabilitazione del Dirottamento:
152110141029     c*******************************************
152120141029     c* - BOLLA LEGATA
152130141029     c* Se la bolla è legata il dirottamento può essere richiesto solo sull'ultima figlia
152140141029     c     karb          chain     fnlbl02l
152150141029     c                   if        %found(fnlbl02l)
152160141029     c* disattivo l'f21 ed emetto messaggio per avvisare che il dirottamento deve essere
152170141029     c* fatto sulla figlia
152180141029     c                   seton                                        7828
152190141029     c                   eval      w02msg ='Bolla legata:il dirottamento può -
152200141029     c                             essere richiesto solo sulla figlia'
152210141029     c                   clear                   atrf21
152220141029     c                   leavesr
152230141029     c                   endif
152240141029
152250141029     c* - PRESENZA DISPO DA ESAMINARE
152260141029     c**   kidc1         setll     tiidc01l
152270141029     c***                if        %equal(tiidc01l)
152280141029     c                   clear                   fnlry09ds2
152290141029     c                   eval      ILRY09TCH='T'
152300141029     c                   eval      ILRY09AAS=d48aas
152310141029     c                   eval      ILRY09LNP=d48lnp
152320141029     c                   eval      ILRY09NRS=d48nrs
152330141029     c                   eval      ILRY09NSP=d48nsp
152340141029     c                   eval      kpjbu=fnlry09ds2
152350141031     c                   call      'FNLRY09C'
152360141029     c                   parm                    kpjba
152370141029     c                   eval      fnlry09ds2=kpjbu
152380141029     c                   if        OLRY09ESIT='1'
152390141029     c                   seton                                        7828
152400141029     c                   eval      w02msg ='Presenti Disposizioni di Consegna-
152410141029     c                              da elaborare: non è possibile dirottare'
152420141029     c                   clear                   atrf21
152430141029     c                   leavesr
152440141029     c                   endif
152450141029     c
152460141029     c* - PRESENZA FERMO DEPOSITO
152470141029     c                   if        vidffd<>*blanks
152480141029     c                   seton                                        7828
152490141125     c                   if        not *in79
152500141125     c                   eval      w02msg='Presente Fermo Deposito: F8 per non -
152510141029     c                             dirottare, F12 per correggere'
152520141125     c                   else
152530141125     c                   eval      w02msg='Presente Fermo Deposito: F12 per cor-
152540141125     c                             reggere'
152550141125     c                   endif
152560141029     c                   clear                   atrf21
152570141029     c                   endif
152580141112     c* - PRESENZA giacenza aperta
152590141112     c                   if        *in04
152600141112     c                   seton                                        7828
152610141112     c                   eval      w02msg='Presente Giacenza aperta: non è poss-
152620141112     c                             ibile dirottare'
152630141112     c                   clear                   atrf21
152640141112     c                   endif
152650141029     c                   endsr
152660150414     C**************************************************************************
152670150414     C* controlli per cons. particolare "Z"=EXPO
152680150414     C**************************************************************************
152690151119     C*    sr_chkexpo    BEGSR
152700150414     c* Se Expo errore se bolla con contrassegno
152710151119     c*                  if        §3afca=1
152720151119     C*                  MOVEL     MSG(159)      VIDMSG
152730151119     C*                  SETON                                        9028
152740151119     C*                  leavesr
152750151119     c*                  endif
152760150414     c* se expo errore se non è franco
152770151119     c*                  if        %subst(§3atb1:1:1)<>'F'
152780151119     C*                  MOVEL     MSG(160)      VIDMSG
152790151119     C*                  SETON                                        9028
152800151119     C*                  leavesr
152810151119     c*                  endif
152820150414     c* errore se lna non è in tabella
152830151119     c*                  move      v1clna        w003a
152840151119     c*                  if        %lookup(w003a:fil_expo)=0
152850151119     C*                  MOVEL     MSG(161)      VIDMSG
152860151119     c*                  eval      vidmsg=%trim(vidmsg) + ' ' + w003a
152870151119     C*                  SETON                                        9028
152880151119     C*                  leavesr
152890151119     c*                  endif
152900151119     c*                  endsr
152910151016     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
152920151016     C     REGPesivol    BEGSR
152930151016     c                   clear                   dar5fat
152940151016     c                   Eval      kar5trd='FAT'
152950151016     c     kar5          Chain     Fiar531c
152960151016     c                   If        %Found(Fiar531c)
152970151016     c                   Movel     ar5uni        dar5fat
152980151016     c                   endif
152990151016     c* Se tassata
153000151016    1c                   if        ar6imv>0
153010151016     c* Volume
153020151016     c                   movel     arbvlf        §ar5vltas
153030151016     c                   movel     arbfvf        §ar5fvtas
153040151016     c* Peso
153050151016     c                   movel     arbpkf        §ar5pktas
153060151016     c                   movel     'R'           §ar5fptas
153070151016     c
153080151016     c* Se il totale imponibile aggiornato senza arrotondamento
153090151016     c*  è = a quello memorizzato al momento del richiamo al
153100151016     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
153110151016    2c                   if        PesVolIMV=Tassatoimv  and
153120151016     c                             PesVolspc='S'
153130151016     c                   movel     PesVolPkcn    §ar5pktas
153140151016    3c                   if        arbncp=arbncl
153150151016     c                   movel     'T'           §ar5fptas
153160151016     c                   else
153170151016     c                   movel     'Z'           §ar5fptas
153180151016    3c                   endif
153190151016     c
153200151016    2c                   endif
153210151016     c                   eval      §AR5DATAS=%dec(%date())
153220151016     c                   eval      §AR5HMTAS=%dec(%time())
153230151016     c                   eval      §AR5PRTAS=knmus
153240151016     c                   eval      §AR5NJTAS=knmtd
153250151016     c*
153260151016    1c                   endif
153270151016     c
153280151016      * aggiorno Fiar5 record "FAT"
153290151016if  1c                   If        %Found(Fiar531c)
153300151016     c* Se ho eliminato la tassazione  deleto
153310151016     c                   if        ar6imv=0
153320151016     c                   if        recname='FIAR5000'
153330151016     c                   delete    fiar500s
153340151016     c                   else
153350151016     c                   delete    fiar5p0s
153360151016     c                   endif
153370151016     c                   else
153380151016     c                   Movel(p)  dar5fat       Ar5uni
153390151016     c                   eval      ar5dac=%dec(%date())
153400151016     c                   eval      ar5orc=%dec(%time())
153410151016     c                   if        recname='FIAR5000'
153420151016     c                   Update    Fiar500s
153430151016     c                   else
153440151016     c                   Update    Fiar5p0s
153450151016     c                   endif
153460151016     c                   endif
153470151016      * scrivo   Fiar5 record "FAT"
153480151016   x1c                   Else
153490151016    2c                   if        ar6imv>0
153500151016     c                   Clear                   Fiar500s
153510151016     c                   Clear                   Fiar5p0s
153520151016     c                   Eval      Ar5Aas = ArbAas
153530151016     c                   Eval      Ar5Lnp = ArbLnp
153540151016     c                   Eval      Ar5Nrs = ArbNrs
153550151016     c                   Eval      Ar5Nsp = ArbNsp
153560151016     c                   Eval      Ar5Trd = 'FAT'
153570151016     c                   Movel(p)  dar5fat       Ar5uni
153580151016     c                   eval      ar5dac=%dec(%date())
153590151016     c                   eval      ar5orc=%dec(%time())
153600151016     c                   Eval      AR5FT1='T'
153610151016     c                   Eval      AR5DT1=99999999
153620151016     c                   Eval      AR5FT2='T'
153630151016     c                   Eval      AR5DT2=99999999
153640151016     c                   Eval      AR5FT3='T'
153650151016     c                   Eval      AR5DT3=99999999
153660151016     c* Verifico dove si trova TITAS per la write di fiar5
153670151016     c     karb          chain     titaS30c
153680160104     c                   if        %found(titas30c)
153690151019     c                   if        recnamet='TITAS000'
153700151016     c                   Write     Fiar500s
153710151016     c                   else
153720151016     c                   Write     Fiar5p0s
153730151016     c                   endif
153740160104     c                   else
153750160104     c                   Write     Fiar500s
153760160104     c                   endif
153770151016    2c                   EndIf
153780151016    1c                   EndIf
153790151016     c
153800151016     c                   ENDSR
153810141020     C**************************************************************************
153820141020     C* Scrittura TIIDC per Caricamento Disposizione di dirottamento
153830141020     C**************************************************************************
153840141020     C     sr_tiidc      BEGSR
153850141020      /free
153860141020       clear tiidc000;
153870141020       exsr repnum;
153880141020       if esito = *blanks;
153890141020          IDCPRG=kprg                         ;
153900151201       // Se progressivo non reperito lo lascio a 0:
153910151201       // per le dispo di reso e dirottamento non abbiamo la necessità di
153920151201       // avere il progressivo dal momento che per una stessa spedizione
153930151201       // non è ammessa la presenza di + dispo se una di queste è dirottamento o reso
153940151201       // Abbiamo quindi preferito scrivere a 0 il progressivo piuttosto che perdere la
153950151201       // dispo. Questo discorso non val per le dispo che riceviamo da web
153960151201       //else;
153970151201       //   leavesr;
153980141020       endif;
153990141020       IDCINSDATA   = %int( %subst( %char( %dec( %timestamp() ) )
154000141020                                    : 1 : 14 ) );
154010141020       //IDCtimestn   = ;
154020141020       IDCMODACC='B'                       ;
154030141020       //IDCIP = ;
154040141020       //IDCIDCLI = ;
154050141023       if *in10;
154060141023          IDCTIPOCLI= 'D' ;
154070141023       else;
154080141023          IDCTIPOCLI= 'M' ;
154090141023       endif;
154100141020       IDCWAAS   = arbaas;
154110141020       IDCWLNP   = arblnp;
154120141020       IDCWNRS   = arbnrs;
154130141020       IDCWNSP   = arbnsp;
154140141020       IDCAAS    = arbaas;
154150141020       IDCLNP    = arblnp;
154160141020       IDCNRS    = arbnrs;
154170141020       IDCNSP    = arbnsp;
154180141021       IDCFGS    = lnacons;
154190141020       IDCTIPODIS = '3';
154200141020       //IDCMAILAVV     ;
154210141020       //IDCTELAVV  ;
154220141020       //IDCALERT
154230141021          IDCRSD   = vidrsd;
154240141021          IDCIND   = vidind;
154250141021          IDCCAD   = vidcad;
154260141021          IDCLOD   = vidlod;
154270141021          IDCPRD   = vidprd;
154280141021          IDCNAZ   = vidnzd;
154290141020       // bolla da dirottare
154300141021          IDCNEWLNA=d_o95lna;
154310141021          IDCFLA   ='S';
154320141020       IDCELAFLG='S';
154330141022       IDCELAUTE=knmus;
154340141023       idcref=Vidref;
154350141023       idcteld=vidteld;
154360141020
154370141020         write tiidc000;
154380141020       endsr;
154390141020        //-----------------------------------------------------------------------*
154400141020        // Reperimento progressivo richiesta da numeratore 650
154410141020        //-----------------------------------------------------------------------*
154420141020        begsr RepNum;
154430141020
154440141022          annocur  = %subdt(%date():*years);
154450141020          clear esito;
154460141020          $finenum=*off;
154470141020          clear trul33ds;
154480141020
154490141020          I33TLa = 'L';
154500141020          I33Ope = *ZEROS;
154510141030          I33CNu = 651;
154520141020          I33Num = 1;
154530141020
154540141020          KPJBU = TRUL33DS;
154550141020
154560141020          dow $finenum=*off   ;
154570141020             Trul33R(kpjba);
154580141020
154590141020             TRUL33DS = KPJBU;
154600141020
154610141020             if O33Err <> *zeros;
154620141020               Esito = '2';
154630141020               $finenum=*on;
154640141020             else;
154650141020               w0090=o33nri;
154660141020               kprg=%dec(%subst(%editc(annocur:'X'):3:2):2:0)*1000000000+w0090;
154670141020               setll kprg tiidc02l;
154680141020        // Se numero già presente nel file TIIDC ne cerco un altro
154690141020               if not %equal;
154700141020                  $finenum=*on;
154710141020               endif;
154720141020             endif;
154730141020          enddo;
154740141020
154750141020          endsr;
154760141020      /end-free
154770940321     C*---------------------------------------------------------------*
154780041007     Ofnblp000  E            ARBDblp
154790041007     O                       arbrsd
154800041007     O                       arbind
154810041007     O                       arbcad
154820041007     O                       arblod
154830041007     O                       arbprd
154840041007     O                       arbnzd
154850041007     O                       arbfin
154860041007     O                       arbcpi
154870041007     O                       arbpkb
154880041007     O                       arbpkf
154890041007     O                       arbvlb
154900041007     O                       arbfvb
154910041007     O                       arbvlf
154920041007     O                       arbfvf
154930041007     O                       arbffd
154940041007     O                       arbcts
154950041022     Ofnblp000  E            ARBKblp
154960041022     O                       arbcbo
154970041022     Ofnblp000  E            ARBGblp
154980041022     O                       arbdcr
154990041022     O                       arbhcr
155000041022     O                       arbtcr
155010041022     O                       arbgc1
155020041022     O                       arbgc2
155030041022     O                       arbtc1
155040041022     O                       arbtc2
155050041022     O                       arbft3
155060041025     Ofnblp000  E            ARBMblp
155070041025     O                       arbksc
155080041025     O                       arbrsm
155090041025     O                       arbinm
155100041025     O                       arbcam
155110041025     O                       arblom
155120041025     O                       arbprm
155130041025     O                       arbnzm
155140041025     O                       arbfap
155150041025     O                       arbcpi
155160041025     O                       arbCTR
155170041025     O                       arbfvf
155180041025     O                       arbvlf
155190041027     Ofnblp000  E            ARBTblp
155200041027     O                       arbvas
155210041027     O                       arbias
155220041027     O                       arbvmd
155230041027     O                       arbvad
155240041027     O                       arbqft
155250041027     O                       arbctr
155260041028     Ofnblp000  E            RMblp
155270041028     O                       arbrma
155280041028     O                       arbnas
155290071025     Ofnblp000  E            SoloCPIP
155300071025     O                       arbcpi
155310120404     Ofnblp000  E            ARBsdblp
155320120404     O                       arbpkf
155330120404     O                       arbvlf
155340120404     O                       arbfvf
155350120416     O                       arbksc
155360120416     O                       arbctr
155370120416     O                       arbrsm
155380120416     O                       arbinm
155390120416     O                       arbLOM
155400120416     O                       arbCAM
155410120416     O                       arbPRM
155420120416     O                       arbNZM
155430120416     O                       arbcpi
155440120531     o                       arbias
155450120531     o                       arbvas
155460041007     Ofnarb000  E            ARBDarb
155470041025     O                       arbrsd
155480041007     O                       arbind
155490041007     O                       arbcad
155500041007     O                       arblod
155510041007     O                       arbprd
155520041007     O                       arbnzd
155530041007     O                       arbfin
155540041007     O                       arbcpi
155550041007     O                       arbpkb
155560041007     O                       arbpkf
155570041007     O                       arbvlb
155580041007     O                       arbfvb
155590041007     O                       arbvlf
155600041007     O                       arbfvf
155610041007     O                       arbffd
155620041007     O                       arbcts
155630041022     Ofnarb000  E            ARBKarb
155640041022     O                       arbcbo
155650041025     Ofnarb000  E            ARBGarb
155660041022     O                       arbdcr
155670041022     O                       arbhcr
155680041022     O                       arbtcr
155690041022     O                       arbgc1
155700041022     O                       arbgc2
155710041022     O                       arbtc1
155720041022     O                       arbtc2
155730141223     O                       arbfbc
155740141223     O                       arbcmc
155750041025     Ofnarb000  E            ARBMarb
155760041025     O                       arbksc
155770041025     O                       arbrsm
155780041025     O                       arbinm
155790041025     O                       arbcam
155800041025     O                       arblom
155810041025     O                       arbprm
155820041025     O                       arbnzm
155830041025     O                       arbfap
155840041025     O                       arbcpi
155850041025     O                       arbCTR
155860041025     O                       arbfvf
155870041025     O                       arbvlf
155880041027     Ofnarb000  E            ARBTarb
155890041027     O                       arbvas
155900041027     O                       arbias
155910041027     O                       arbvmd
155920041027     O                       arbvad
155930041027     O                       arbqft
155940041027     O                       arbctr
155950041028     Ofnarb000  E            RMarb
155960041028     O                       arbrma
155970041028     O                       arbnas
155980071025     Ofnarb000  E            SoloCPIA
155990071025     O                       arbcpi
156000120404     Ofnarb000  E            ARBsdarb
156010120404     O                       arbpkf
156020120404     O                       arbvlf
156030120404     O                       arbfvf
156040120416     O                       arbksc
156050120416     O                       arbctr
156060120416     O                       arbrsm
156070120416     O                       arbinm
156080120416     O                       arbLOM
156090120416     O                       arbCAM
156100120416     O                       arbPRM
156110120416     O                       arbNZM
156120120416     O                       arbcpi
156130120531     o                       arbias
156140120531     o                       arbvas
156150120411     ofiar6000  e            AR6SD
156160120411     o                       AR6KSC
156170120411     o                       AR6CTR
156180120411     o                       AR6DIV
156190120411     o                       AR6POR
156200120411     o                       AR6SV1
156210120411     o                       AR6VA1
156220120411     o                       AR6SV2
156230120411     o                       AR6VA2
156240120411     o                       AR6SV3
156250120411     o                       AR6VA3
156260120411     o                       ar6imv
156270120411     o                       ar6cei
156280041020     Ofiqdt000  E            AGGqdt
156290041020     O                       qdtnbnbr
156300011120**         C20
156310020805ALCOBJ OBJ((FLARXXXX   *FILE   *EXCL   M000000))
156320920120**         C21
156330020805DLCOBJ OBJ((FLARXXXX   *FILE   *EXCL   M000000))
156340920120**         C22
156350020805OVRDBF FILE(FLARXXXX)               MBR(M000000)
156360930507**         GGG
1563709803091234567
156380050601** cmdt
156390050601OVRDBF FILE(TITA430C) TOFILE(
156400151016OVRDBF FILE(FIAR531C) TOFILE(
156410151016OVRDBF FILE(TITAS30C) TOFILE(
156420941014**
156430941102Spedizione al momento non disponibile: gia' utilizzata da un altro utente     01
156440980406Peso mancante                                                                 02
156450980406Volume mancante                                                               03
156460051129Variazione non eseguibile: bolla allocata da altro utente. Attendi e riprova
156470051129Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL05
156480941102Data formalmente errata                                                       06
156490941102Data consegna richiesta non puo' essere inferiore della data spedizione       07
156500941102Ora richiesta formalmente errata                                              08
156510941102Se immesso tipo immettere anche ora o data consegna richiesta                 09
156520941102Giorno di chiusura errato                                                     10
156530000510Consegna particolare errata o non utilizzabile                                11
156540941102I giorni di chiusura non possono essere uguali
156550941102Il destinatario non puo' essere sempre chiuso!!!?!!
156560941102Le consegne particolari non possono essere uguali
156570051117Impossibile modificare Cons.Part. " " :immessa da partenza no gestib.in arriv 15
156580941102Codice Bolla errato o non utilizzabile
156590941102Codice bolla esistente e nuovo non possono essere uguali
156600941102Impossibile trasformare una bolla da non recupero in recupero o viceversa
156610941213Impossibile effettuare cambi di porto: utilizzare l'apposito programma
156620941213Il codice bolla non ammette il C/Assegno                                      20
156630941102Il codice bolla vuole il C/Assegno: inserire almeno l'importo
156640941102Impossibile variare i campi relativi al C/Assegno perche' gia' incassato
156650071025Codice Fiscale obbligatorio per cliente ITALIA.  Enter per forzare            23
156660131002Mancano i dati passati per il tipo di aggiornamento richiesto                 24
156670941102Se immessa la valuta immettere anche l'importo o eliminare entrambi           25
156680941102Valuta errata
156690000510Non ammessi importi con decimali per la valuta indicata                       27
156700000510Se modificata la divisa, ridigitare il relativo importo                       28
156710941102Impossibile immettere le particolarita' C/Assegno per bolla senza C/A
156720131008$$Sono ammesse solo spedizioni in  FRANCO  per le  POSTE                        30
156730131008Causale variazione errata per il tipo di dati passati                         31
156740000510Spedizione POSTE : peso superiore a xxxxxx,x kg.  Enter per forzare!          32
156750941103Filiale Cliente inesistente
156760941103Per FATTURA IMMEDIATA impossibile specificare codice di altra filiale
156770941103Codice non ammesso se cliente di altra filiale                                35
156780941103Codice cliente errato o inesistente
156790941103Codice cliente non utilizzabile: bloccato in Piano dei Conti
156800941103Se Segue Fattura impossibile indicare il codice 9999
156810011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR. TEL.IN SEDE X AUTOR. 39
156820941209Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  40
156830941103Impossibile eseguire la tassazione: cliente non dell'area
156840941209Importo da Assicurare obbligatorio, come indicato nella tariffa del cliente   42
156850011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  43
156860011205Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 44
156870011205Valore merce dichiarato obbligatorio                                          45
156880941209Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 46
156890941209Codice cliente di altra filiale: F9 per forzare l'immissione
156900941209Sigla varia inesistente                                                       48
156910941209Varia "G" non ammessa                                                         49
156920941209Varia "G" obbligatoria                                                        50
156930941209Importo varia obbligatorio                                                    51
156940000510Spedizione D.P.D. : non ammesso un peso superiore a xxxxxx,x kg.              52
156950990702Non e' stata trovata la tariffa per la tassazione                             53
156960941212Provvigione a zero: non indicata la percentuale in tariffa                    54
156970941212Errore in tassazione                                                          55
156980011018Se eliminato importo C/A eliminare anche il Tipo Incasso e Particolar.        56
156990071025Partita IVA  obbligatoria per cliente ITALIA.  Enter per forzare              57
157000051129Nuovo volume da FATT. inferiore al volume parziale VDL: sara' sostituito      58
157010950420Non e' possibile modificare il tipo bolla: assegnato gia' incassato           59
157020941222Non e' possibile eliminare i dati della fattura se era un PREPAGATO           60
157030941223Non trovata spedizione                                                        61
157040941223Immesso codice cliente: l'indirizzo sara' ignorato                            62
157050950406Anagrafica mittente non completa in Piano dei Conti                           63
157060950406Manca la provincia in anagrafica mittente: inserirla!!!                       64
157070950407Aggiunto contrassegno: aggiungere anche varia G. Enter per forzatura          65
157080950407Eliminato contrassegno: eliminare anche varia G. Enter per forzatura          66
157090950522Partita IVA obbligatoria se bolla diretta all'estero con attraversam. dogana  67
157100950424Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic68
157110991012Azzerata q.tà da fattur.:azzerare anche gli importi di tassazione             69
157120950426Variato cliente o importo da assic.:azzerare q.tà da fattur. e tot.imponibile.70
157130991012Variato cliente o importo da assic.:azzerare q.tà da fattur. e tassazione     71
157140950426Azzerata q.tà da fattur.e tariffa a valore:azzerare anche il tot. imponibile  72
157150961204Impossibile inserire il C/Assegno in bolle di RESO
157160061024Rif. Mittente obbligatorio se destinatario estero con attraversamento dogana  74
157170970110Natura merce obbligatoria                                                     75
157180970826ATTENZIONE!LArrivo desunta, diversa da bolla ma non verra'variata.ENTER forza
157190970808ATTENZIONE! Inoltro modificato. Enter per continuare                          77
157200980309Impossibile inserire il C/Assegno in questa bolla legata                      78
157210011205Importo da Assicurare non ammesso                                             79
157220011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  80
157230991112Impossibile fare fattura immediata a cliente di altro network                 81
157240990127Impossibile inserire importo da assicurare:bolla su C.A.in Liquidaz.Transatt. 82
157250990419Aggiunto Imp.Assicur.: aggiungere anche varia R. ENTER per forzare            83
157260990419Eliminato Imp.Assicur.: eliminare anche varia R. ENTER per forzare            84
157270120525Cli 8888/9999:se ImportoAssicurare e TotImponibile InserireVaria"R"
157280990419Modificato Imp.Assicur: controllare varia R . ENTER per forzare               86
157290060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 87
157300990701Per i Prepagati, Numero fattura deve essere maggiore di XXXXXX                88
157310990702Prepagato: premere F14 per tassare o inserire tassazione fino all'imponibile  89
157320991006Per codice cliente 8888 o 9999 senza tariffa è obbligatoria la divisa xxx     90
157330011210Importo C/Assegno SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR              91
157340061107Aggiornamento effettuabile soltanto in PARTENZA su spedizioni in Prepagato
157350011010Ammessi importi con massimo __ decimali per la divisa indicata                93
157360991124Divisa obbligatoria se immesso almeno un importo di tassazione                94
157370061107Aggiornamento effettuabile soltanto in ARRIVO su spedizioni Fattura Immediata 95
157380131008$$Bolla cliente POSTE: non si puo' trasformare in C/Servizio                    96
157390131008$$Bolla cliente POSTE: spese C/A sempre a carico del mittente                   97
157400010412Tipo inoltro "D"=Disagiato non ammesso per questo cap/località                98
157410070508Impostare tipo CONTATTO solo se immesso il Fermo Deposito                     99
157420011011Divisa obbligatoria                                                           00
157430011017Manca la varia 'N'                                                            01
157440011102Divisa scaduta                                                                02
157450011120Spedizione giacente: importo C/A non valido                                   03
157460011210Valore merce dichiarato INFERIORE a xxxxxxxxxx,xxx EUR                        04
157470011210Valore merce dichiarato SUPERIORE al limite imposto xxxxxxxxxx,xxx EUR        05
157480011210Valore merce dichiarato > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE  06
157490011206Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       07
157500011210Importo C/Assegno INFERIORE a xxxxxxxxxx,xxx EUR                              08
157510011210Importo C/Assegno > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER X FORZARE        09
157520011206Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                10
157530020508Discordanza tra codice bolla e codice cliente mittente
157540020531Destinatario               : immettere la relativa consegna particolare       12
157550051116Non è possibile indicare contemporaneamente cons.particolari 'S' e 'X'        13
157560021121Immettere codice tariffa della relativa cartello FedEx                        14
157570021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            15
157580021121ATTENZIONE! Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE    16
157590021129Immettere consegna particolare "S"                                            17
157600030124Immettere il nominativo                                                       18
157610021129Immettere il n. di telefono della persona contattata                          19
157620030124Immettere il motivo                                                           20
157630051129Peso rilevato totalmente da VDL.  Si può inserire solo il peso = a peso VDL   21
157640030123Almeno una delle consegne particolari deve essere "S"                         22
157650051129Impossibile inserire un peso inferiore a quello parzialmente rilevato da VDL  23
157660030124Il n. dei bancali non ritirati non può superare il n. dei bancali bollettati  24
157670030124Immettere il n. dei bancali non ritirati                                      25
157680031001Non è possibile modificare la consegna richiesta                              26
157690040123Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "  27
157700040330La data consegna richiesta deve essere inferiore alla eseguibilità disp.giac. 28
157710040330Data consegna richiesta superiore a ___ giorni                                29
157720131008Codice cliente o linea cliente non utilizzabile                               30
157730041015Prevista consegna ad Uffici:consegna per Appuntam. o a Supermerc. non ammessa 31
157740110512C/A  SUPERIORE al limite xxxxxxxxxx,xxx EUR non ammesso per bancari BRT       32
157750050414Impossibile trasformare prepagato in C/S                                      33
157760061116Modificata ragione sociale DESTINATARIO: controllare correttezza Cod.FIScale  34
157770050603Non ammessi inserimenti di importo da assicurare: bolla non coperta da mandato35
157780141110Variazione non effettuabile:salvataggio in corso. Riprovare (File "        ") 36
157790051220Non è possibile cambiare il mittente da DPD a non DPD e viceversa             37
157800060131Utente non abilitato ad inserire la consegna particolare " "                  38
157810070312Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta:ENTERavanza
157820070308Data consegna richiesta OBBLIGATORIA insieme al Tipo e/o all'Ora
157830070312Consegna Richiesta non modificabile: giacenza aperta o sped.con blocco "G"    41
157840070330Modifica di DataConsegnaRichiesta POSTICIPATA in chiusura distinta            42
157850070508Impostato Fermo Dep.DA CONTATTARE ma presente ConsegnaRich.in bolla:ENTR forza 43
157860070910Togliere Cons.Particolare X-xxxxxxxx:non immessa in partenza e non da Destinat44
157870071025Immettere codice fiscale o partita IVA o uscire con F12                       45
157880091124Inserimento/aumento ImpAssic x bolla non coperta da mandato:inviare MAIL.Forza46
157890091124Volume rilevato totalmente da VDL. Si può inserire solo il volume = al VDL    47
157900091124Impossibile inserire un volume inferiore a quello parzialmente rilevato da VDL48
157910120508P.Iva "$$" non ammessa per fatture emesse in filiale                          49
157920130930Indirizzo e-mail formalmente errato                                           50
157930131016Eseguita tassazione su bolla contabilizzata:non ammesso F6. F12 per uscire    51
157940131016Non effettuate variazioni: non ammesso F6=Conferma. Premere F12 per uscire    52
157950140408Il Fermo Deposito deve essere autorizzato dal Mittente                        53
157960141029Immessa Data Cons. Richiesta ma presente Fermo Deposito in bolla: ENTER forza 54
157970160204Presenti Dispo di Dirott./Reso ancora da elaborare: Fermo Deposito non ammesso55
157980160204Presente Dispo di Dirott./Reso ancora da elaborare: variazione non ammessa    56
157990141112Presente Giacenza aperta: Enter per Forzare                                   57
158000150210Consegna part. per Appuntamento non valida: immessa data consegna "Tassativa" 58
158010150413Spedizione per EXPO: non ammesso il Contrassegno                              59
158020150414Spedizione per EXPO possibile solo per bolle Franco                           60
158030150414Spedizione per EXPO non ammessa per la linea di arrivo                        61
158040151221Spedizione non manutenzionabile per questa Causale Variazione                 62
