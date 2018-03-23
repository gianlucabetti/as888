000100040107      *---------------------------------------------------------------*
000200040610      * GESTIONE RICHIESTE ASSISTENZA CLIENTI                         *
000300040107      *---------------------------------------------------------------*
000400070430      *                                                               *
000500070430      *   ATTENZIONE PER COMPILARE BISOGNA AVERE ANCHE LA GAITRAGRPS  *
000600070430      *   NELLA LISTA DELLE LIBRERIE                                  *
000700070430      *                                                               *
000800070430      *---------------------------------------------------------------*
000900151201
001000151201       //--------------------------------------------------------------
001100151201       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001200151201       //--------------------------------------------------------------
001300151201
001400151201     /*PRM  dbgview(*source)
001500151201     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001600151201     /*CMD         ovrscope(*calllvl)
001700151201     /*END  dltovr file(TITAS30C) lvl(*)
001800151201     /*END
001900151201
002000151201       //--------------------------------------------------------------
002100151201       //?Specifiche di controllo.                                     ?
002200151201       //--------------------------------------------------------------
002300040107
002400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500131119     H DFTACTGRP(*NO) ACTGRP(*NEW)
002600141210     h bnddir('TRUL')
002700040107
002800151201       //--------------------------------------------------------------
002900151201       //?Dichiarazione file.                                          ?
003000151201       //--------------------------------------------------------------
003100040107
003200040126     fFITGD01L  uf a e           k disk
003300040126     fFITGR01L  uf a e           k disk
003400150325     fFITGD32L  if   e           k disk    rename(fitgd000:fitgd32)
003500150603     f                                     prefix(K_)
003600070402     fFITGN01L  if   e           k disk
003700150319     fFITGM01L  if a e           k disk
003800160314     fTIGCP01l  if   e           k disk
003900160314     fTIGCP21L  if   e           k disk    rename(TIGCP000:TIGCP21)
004000170518     fFNLBL02L  if   e           k disk
004001170519     fFNLBL01l  if   e           k disk    rename(fnlbl000:fnlbl1)
004100040211     fFNORM01L  if   e           K disk
004200040212     fAZORG01L  if   e           K disk
004300040426     fAZCLN01L  if   e           K disk
004400040323     fAZUTE01L  if   e           K disk
004500041005      * filiale
004600041005     fFNARB01L  if   e           K disk    usropn
004700041005     fFNBLP01L  if   e           K disk    usropn
004800140429     fFIAR401L  if   e           k disk    usropn
004900140429     fFIAR501L  if   e           k disk    usropn
005000041005      * sede
005100041005     fTITAS30C  if   e           K DISK    usropn
005200151201      *
005300151201     fTIIDC02L  if A e           k disk    usropn
005400040112      *
005500161027     fFIDNA1D   cf   e             workstn sfile(FIA1S01:S01nrr)
005600160104     f                                     ignore( FIA1W03 )
005700040107
005800040107      *---------------------------------------------------------------*
005900070327      *  MOTIVI DI APERTURA FISSATI A PROGRAMMA
006000070327      *---------------------------------------------------------------*
006100070327      *   2 = LASCIATO AVVISO                               W_LAV
006200070327      *  10 = CONTRASSEGNO                                  W_CON
006201170524      *  12 = GIACENZA                                      W_GIA
006300070327      *  14 = SUPERMERCATO                                  W_SUP
006400070327      *  15 = APPUNTAMENTO                                  W_APP
006500070327      *  16 = FERMO DEPOSITO                                W_FDE
006600140429      *  17 = FERMO DEPOSITO AUTORIZZA MITTENTE             C_FDEAM
006700151130      *  18 = RESO                                          C_MOTAP_RESO
006800141210      *  60 = VERIFICA PIN-CODE TELEFONICA                  C_VPINC
006900150720      *  70 = RECLAMO DA SEDE                               C_RECLSEDE
007000070327      *---------------------------------------------------------------*
007100040107
007200040107      *
007300040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
007400040107      *
007500040205
007600170526     d MSG             S             78    DIM(064) CTDATA PERRCD(1)            MSG VIDEO
007700040531     D CCR             S              1    DIM(20)                              Chi chiama
007800070323      *
007900070323     d L6              s              3  0 dim(30)                              po £6
008000130610     D wpog            s              3    dim(250)
008100070327
008200070327     D NOB             S             35    DIM(12)                              Note
008300070327     D NORA            S             35    DIM(12)                              Note da scrivere
008400070327     D NOV             S             35    DIM(04)                              Note a video
008500070327
008600070323      *
008700040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
008800040107      *
008900040107     d KPJBA         e ds
009000040213      * - Parametri per gestione reclami già esistenti
009100040213     d FIDNA0DS      e ds
009200040213      * - Parametri ritorno da interrigazione oggetto
009300040213     d FIDNA1DS      e ds
009400040203      * - Parametri note
009500040107     d FIDNB0DS      e ds
009600040217      * - Parametri cancellazione reclamo
009700150928     d FIDNCAds      e ds                  inz
009800070330      * - Parametri per richiamo pgm note per Aut / ldv
009900131206     d FNLRN6DS      e ds
010000070329      * - Parametri per richiamo pgm blocchi
010100070329     d FNLRG2DS      e ds
010200070430      * - Parametri per richiamo pgm recupero blocchi
010300070430     d FNLRG6DS      E DS
010400070430     d DLRG6ABI      E DS
010500070329      * - Parametri per richiamo pgm FNLR48R
010600070329     D fnlr48ds      E DS
010700070608      * - Parametri per richiamo pgm FNLR66R controllo causali variazioni bolle
010800070608     D fnlr66ds      E DS
010900150310      * - Parametri per richiamo pgm FIOR05R immissione ORM
011000150310     d FIOR05DS      e ds
011001170220      * - Parametri per richiamo pgm FNLG07R1 restituisce se c'è giacenza
011002170220     d FNLG071DS     e ds
011003170221      * - Parametri per richiamo pgm FNLG10R
011004170221     D fnlg82ds      E DS
011100070329      *
011200070329     D DSARBD        E DS
011300070329     D DSARBG        E DS
011400070329     D DARBT         E DS
011500070329     D DSARBK        E DS
011600070329     D trul90ds      E DS
011700140924     D DSARBN        E DS
011800040108      *
011900040108      * - Parametri x Controllo profilo utenti
012000040108     d TIBS34DS      e ds                  inz
012100040108      * - Ds di riferimento al file esterno AZUTE00F
012200040108     d AZUTEDS       e ds                  extname(AZUTE00F)
012300040108      * - Ds per dati organigramma
012400040108     d DDatiUte      e ds
012500070323      * abilitazioni
012600070323     D OG148         E DS
012700070323      * DS per TRUL06R - Caricamento £X
012800070323     d TRUL06DS      E DS
012900070323     d  LIN                    1     90  0
013000070323     d                                     DIM(30)
013100040202      * - Ds per Anagrafiche Clienti
013200040202     d TIBS69DS      E DS
013300040202     d DS_cnaco      E DS                  extname(CNACO00F)
013400040202     d DS_cnind      E DS                  extname(CNIND00F)
013500040202     d DS_cnclp      E DS                  extname(CNCLP00F)
013600040202     d DS_fncls      E DS                  extname(FNCLS00F)
013700040107      *
013800040129      * - Parametri x Numeratore Reclami
013900040212     d TRUL33ds      e ds
014000040109      * - Tabelle
014100040129     d TIBS02ds      e ds                  inz
014200131119     d tntbMARds1    e ds                  inz
014300130610
014400130610     D TRUL31DS      E DS
014500130610     d dLat          e ds
014600130610     d dute01        e ds
014700070327      *
014800040212     d DMAR          e ds
014900040212     d DCHR          e ds
015000040212     d DCMU          e ds
015100040217     d DCOR          e ds
015200130514     d DOGR          e ds
015300130514     d dOGR_S        e ds                  extname(dOGR)  qualified
015400130514     d                                     inz
015500040220     d DCCR          e ds
015600040323     d DPRR          e ds
015700040323     d DUGR          e ds
015800040323     d DREC          e ds
015900141210      *
016000141210     d ds7R          e ds                  inz  qualified
016100140429      *
016200140429      * -?Estensione TESTATA BOLLA: tipo record "GEN"?
016300140429     d dAR5gen       e ds                  inz
016400141210      *
016500151211      * -?Gestione Telefonate - Flag Avanzamento Fase R.A.?
016600151210     d dTGDfar       e ds                  inz  qualified
016700151211
016800151211      * -?Gestione Telefonate - Flag operativi di FITGD00F.TGDFLO?
016900151211     d dTGD01        e ds                  inz  qualified
017000151210
017100141210      * -?Gestione Telefonate - Riferimenti Chiamante: Riferimento "PINcode"?
017200141210     d dTGR01        e ds                  inz  qualified
017300151210
017400151210      * -?Gestione Telefonate - Riferimenti Chiamante: Attesa Risposta?
017500151210     d dTGR01a       e ds                  inz  qualified
017600151210
017700151210      * -?DS Modifiche Avanzamento Fase FITGM
017800151210     d dTGMa         e ds                  inz  qualified
017900150318
018000150318      * -?DS Modifiche Causale Chiusura FITGM
018100150318     d dTGMc         E DS
018200150318
018300150318      * -?DS Modifiche Cod. Chiamante FITGM
018400150318     d dTGMK         E DS
018500150318
018600151210      * -?DS Modifiche Fil. Responsabile FITGM
018700150318     d dTGMR         E DS
018800150326
018900150326      * -?DS Modifiche Stato Responsabile FITGM
019000150326     d dTGMS         E DS
019100150316
019200150316      * -?DS Modifiche Utente FITGM
019300150316     d dTGMu         E DS
019400131119      *
019500131119      * - Parametri x controllo indirizzo e-mail
019600040217     d DSEMAIL       E DS
019700160708      *
019800160708      * -?Aggiunge/Toglie gg/mm dalla data?
019900160708     d XGIOLAVds     e ds                  inz
020000160701      *
020100160706      * -?Parametri da passare al pgm interrogaz. Bolle di Filiale?
020200160701     d ParamU6ds       ds                  inz   qualified
020300160706      ** ?CAMPI DI OUTPUT:?
020400160706      * ?. Key spedizione?
020500160706     d   V2Caas               14     17  0 inz
020600160706     d   COMlnp               18     20  0 inz
020700160706     d   V2Cnrs               21     22  0 inz
020800160706     d   V2Cnsp               23     29  0 inz
020900160706      * ?. Flag '1'=Premuto F3=Fine?
021000160706     d   PA1f03               30     30
021100160706      ** ?CAMPI DI INPUT:?
021200160706      * ?. Modalità di richiamo?
021300160706     d   PA1flg               31     31
021400160706      * ?. Flag '1'= RICH DA PGM GIAC.?
021500160706      *             ?(Serve per disabilitare tasto funzionale F14=Giac.?
021600160706      *              ?in interrogazione della bolla ed evitare così?
021700160706      *              ?errore di chiamata ricorsiva?
021800160706     d   PA1gia              144    144
021900160706      * ?. Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:?
022000160706     d   PA1rma              149    163
022100160706     d   PA1xco              164    164
022200160706      * ?. non utilizzato?
022300160706     d   PA1flo              165    256
022400160706      *
022500160706      * -?Parametri da passare al pgm interrogaz. singola Bolla di Filiale?
022600160706     d ParamU6ds1      ds                  inz   qualified
022700160706      ** ?CAMPI DI INPUT:?
022800160706     d  V2Caas                14     17  0 inz
022900160706     d  COMlnp                18     20  0 inz
023000160706     d  V2Cnrs                21     22  0 inz
023100160706     d  V2Cnsp                23     29  0 inz
023200160706      ** ?CAMPI DI OUTPUT:?
023300160706      * ?. Flag '1'=Premuto F3=Fine?
023400160706     D  PARf03                30     30
023500160706      ** ?CAMPI DI INPUT:?
023600160706      * ?. Modalità di richiamo?
023700160706     D  PARflg                31     31
023800160706      * ?. Flag '1'= RICH DA PGM GIAC.?
023900160706     D  PARgia               144    144
024000160706      ** ?CAMPI DI OUTPUT:?
024100160706     d  PARrma               149    163                                         x F22=PerfectOrder
024200160706     d  PARxco               164    164                                         x F22=PerfectOrder
024300160706      ** ?CAMPI DI INPUT:?
024400160706      * ?. Tipo lancio: "C" -           CHIUSO CON LR?
024500160706      *                ?"L" - ELABORO E CHIUDO CON LR?
024600160706      *                ?" " - ELABORO E CHIUDO IN RETRN?
024700160706     d  PARtla               165    165
024800170119      ** ?CAMPI DI OUTPUT:?
024900170119      * ?. Codice di errore: "1" = Sped. non trovata?
025000170119     d  PARerr               166    166
025100170119     d  PARflo               167    256
025200160701      *
025300160706      * -?Altri Parametri da passare al pgm interrogaz. Bolle di Filiale?
025400160701     d fnlrU6ds      e ds                  inz
025500040109      *
025600040211      * - Parametri da passare al pgm di visualizzazione ORM
025700040211     D Parm01          DS
025800040211     D  ppoe                          3  0
025900040211     D  pnor                          7  0
026000040211     D  pnsr                          2  0
026100040211     D  pnrv                          2  0
026200040211     D  psce                          1
026300040211     D  pfgs                          3  0
026400040211     D  ppor                          3  0
026500040211     D  pdtr                          8  0
026600040211     D  pmdb                         10
026700040211     D  pprb                         10
026800040211     D  pdts                          8  0
026900040211     D  prmp                          1
027000040211     D  pbrc                          1
027100040211     D  pref                          2
027200040211     D  pmio                          1
027300041005      * - Parametri da passare al pgm di visualizzazione Bolle di sede
027400041005     D TNSB50DS      E DS
027500040211      *
027600040205      * - Esplosione oggetto spedizione
027700040205     D DS_spediz       DS                  INZ
027800040219     D  DSspe_lnp              1      3  0
027900040219     D  DSspe_nrs              4      5  0
028000040219     D  DSspe_nsp              6     12  0
028100040219     D  DSspe_aas             13     16  0
028200040205      * - Esplosione oggetto ORM
028300040205     D DS_orm          DS                  INZ
028400040205     D  DSorm_poe              1      3  0
028500040205     D  DSorm_nsr              4      5  0
028600040205     D  DSorm_nor              6     12  0
028700141212     D  DSorm_nrv             13     14  0
028800040205      *
028900040205      * - Parametri da passare al pgm interrogazione ORM
029000161107     D FIOR010DS     E DS                  inz
029100161107     d   IOR010ric   e                     inz('1')
029200161107     d   IOR010rsrX  e                     inz('X')
029300161107     d   IOR010inrX  e                     inz('X')
029400161107     d   IOR010lorX  e                     inz('X')
029500161107     d   IOR010rsoX  e                     inz('X')
029600161107     d   IOR010rscX  e                     inz('X')
029700070327      *
029800040109     d Status         sds
029900040109     d  SDSpgm           *proc
030000040213     d  PARMS            *parms
030100040107      *
030200170113     d*// dspINFOds       ds           512
030300170113     d*//  di_TASTO             369    369
030400170113     d*//  di_NRG               370    370
030500170113     d*//  di_SFLnrr            378    379B 0
030600040109      *
030700040109     d WLBdat          ds                  inz
030800040109     d  G02dat                 1      8  0 inz
030900040109     d  G02inv                 9     16  0 inz
031000040109     d  G02err                17     17    inz('3')
031100040109     d  G02tgi                18     22  0 inz
031200040211      * - DS di comodo    ORM
031300040211     D                 DS
031400040211     D  W_poe                         3  0
031500040211     D  W_nsr                         2  0
031600040211     D  W_nor                         7  0
031700040211     D  W_nrv                         2  0
031800040303     D  Wormvid                1     14  0 inz
031900040211      *
032000040211     D                 DS
032100040211     D  v1poeo                        3  0
032200040211     D  v1nsor                        2  0
032300040211     D  v1norm                        7  0
032400040211     D  v1nrvo                        2  0
032500040303     D  ormvid                 1     14  0 inz
032600040216      *
032700040216     D                 DS
032800040216     D  WRKpoe                        3
032900040216     D  WRKnso                        2
033000040216     D  WRKnor                        7
033100040216     D  WRKnrv                        2
033200040216     D  WRKORM                 1     14
033300040211      * - DS di comodo    spedizione
033400040211     D                 DS
033500040211     D  W_lnp                         3  0
033600040211     D  W_nrs                         2  0
033700040211     D  W_nsp                         7  0
033800040211     D  W_aas                         4  0
033900040303     D  Wspevid                1     16  0 inz
034000040211      *
034100040211     D                 DS
034200040211     D  v1lnps                        3  0
034300040211     D  v1nrss                        2  0
034400040211     D  v1nsps                        7  0
034500040217     D  v1anns                        4  0
034600040303     D  spevid                 1     16  0 inz
034700040216     D                 DS
034800040219     D  WRKlnp                        3
034900040219     D  WRKnrs                        2
035000040219     D  WRKnsp                        7
035100040219     D  WRKann                        4
035200040219     D  WRKSPE                 1     16
035300040216      * - DS per girare le date
035400040216     D                 DS
035500040216     D  anno4                  1      4  0
035600040216     D  anno2                  3      4  0
035700040216     D  mese                   5      6  0
035800040216     D  giorno                 7      8  0
035900040216     D  data8                  1      8  0
036000040216
036100040212      * - DS Parametri per ricerca Punto Operativo
036200040212     D Dstnsd          DS
036300040212     D  §cod                          3
036400040212     D  §tip                          1
036500040212     D  §des                         25
036600040212     D  §pra                          3
036700040420
036800040420      * - DS Parametri per ricerca Programmi chiamanti
036900040420     D DSStack         ds
037000040420     D  £Stack                       10    Dim(100)
037100040212
037200040426      * - DS Controllo giorni festivi
037300040426     d clnmat          ds
037400040426     d  mat                    1     31    dim(31)
037500040426     d clnpom          ds
037600040426     d  pom                    1     31    dim(31)
037700150317
037800150317     D wFIL_ds         ds
037900150317     d   sk_wFIL               1    750    dim(250)
038000151130
038100151130     d wFILest_ds      ds
038200151130     d   sk_FILest             1    750    dim(250)
038300150114
038400040107      *
038500040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
038600040107      *
038700040109      * - Campi di comodo
038800040108     d w0140           s             14  0
038900040108     d wData           s              8  0
039000040120     d wOra            s              6  0
039100040108     d DateU           s              8  0
039200040213     d ora             s              2  0
039300040213     d minuti          s              2  0
039400040426     d wgiorno         s              2  0
039500151201     d w0090           s              9  0
039600041206     d w0070           s              7  0
039700041206     d w0040           s              4  0
039800130611     d w003a           s              3
039900040212     d wtibs69r        s              1
040000040212     d w1cdar          s              8  0
040100040217     d w1corr          s              6  0
040200040213     d wmod            s              1
040300040217     d rec_wrt         s              3  0
040400040217     d s01nrr          s              4  0
040500040217     d wdar            s              8  0
040600070329     d wdcr            s              8  0
040700040426     d giorni          s              3  0
040800041206     d wforz           s              1
040900040218     d wrk_recsbf      s              1
041000040218     d Wfidnb0r        s              1
041100070327     d W_cau           s              1
041200040219     d W_confe         s              1
041300040323     d W_ute           s              3
041400040617     d W_pot           s              3  0
041500070330     d Not_dcr         s              1
041600070330     d Not_ldv         s              1
041700141210     d Not_PINcode     s              1    inz
041800070402     d w_dtcr          s             11
041900040531     d forz_rec        s              1
042000041213     d mod_testa       s              1
042100050119     d Sv_inkd         s              1
042101170503     d UteGia          s              1                                         Valore P o A
042200070330     d Note_dcr        s             78
042201170505     d Note_gia        s             70
042300130610     d wabi            s                   like(UteAut)
042400150313     d w0030           s              3  0
042500150313     d sav_in48        s              1a
042600150316     d wtmo            s                   like(TGMtmo)
042700150422     d $Video          s              2    inz
042800150422     d $GesW04         s               n   inz
042900160128     d $GesW04N        s               n   inz
043000150422     d $Reparto        s               n   inz
043100160104     d $SaveF6         s               n   inz
043200150421     d wwMail          s                   like(V1mail)  inz
043300150421     d wwcMAD          s                   like(V1Cmad)  inz
043400150422     d wwdMAD          s                   like(V1Dmad)  inz
043500150421     d wwcPOG          s                   like(V1Cpog)  inz
043600150422     d wwdPOG          s                   like(W4Dpog)  inz
043700150423     d wwcRPR          s                   like(W4Crpr)  inz
043800150423     d wwdRPR          s                   like(W4Drpr)  inz
043900160201     d WrepR           s                   like(V1CrepR) inz
044000160201     d WfilR           s                   like(V1CfilR) inz
044001170519     d wAAS            s                   like(ARBaas) inz
044002170519     d wLNP            s                   like(ARBlnp) inz
044003170519     d wNRS            s                   like(ARBnrs) inz
044004170519     d wNSP            s                   like(ARBnsp) inz
044005170519     d LNP_abil        s                   like(ARBlnp) inz
044006170519     d LNA_abil        s                   like(ARBlna) inz
044007170519
044008170519
044009170221
044010170221     d Qcmd1           s             80    inz
044011170221     d Qlen1           s             15  5 inz(%len(Qcmd1))
044100070329
044200141212     d xx              s              3  0 inz
044300070329     d x               s              2  0
044400070329     d y               s              2  0
044500070329      *
044600040426
044700040426     d ktfp            s                   like(clntfp)
044800040426     d ktfa            s                   like(clntfa)
044900040426     d kann            s                   like(clnann)
045000040426     d kmes            s                   like(clnmes)
045100040324
045200070327     d W_LAV           s                   like(tgdmad) inz('  2')
045300070327     d W_CON           s                   like(tgdmad) inz(' 10')
045301170524     d W_GIA           s                   like(tgdmad) inz(' 12')
045400070327     d W_SUP           s                   like(tgdmad) inz(' 14')
045500070327     d W_APP           s                   like(tgdmad) inz(' 15')
045600070327     d W_FDE           s                   like(tgdmad) inz(' 16')
045700140429     d c_FDEAM         c                   const(' 17')
045800151130     d c_MotAp_Reso    c                   const(' 18')
045900141210     d c_VPINc         c                   const(' 60')
046000150717     d c_ReclSede      c                   const(' 70')
046100070327
046200040324     D Wdataeur        S               D   DATFMT(*eur)
046300040324     D Wdataoggi       S               D   DATFMT(*eur)
046400040216
046500040421     d Keyant          s                   like(tgdant)
046600040218     d Keynut          s                   like(tgdnut)
046700040218     d Keypdt          s                   like(tgdpdt)
046800040212     d Keytri          s                   like(tgrtri)
046900070402     d Keytin          s                   like(tgntin) inz('GEN')
047000040217     d W_v1cchi        s                   like(v1cchi)
047100041207     d Wrk_cchiA       s                   like(v1cchi)
047200041207     d Wrk_cchiB       s                   like(v1cchi)
047300041207     d Wrk_cchiC       s                   like(v1cchi)
047400041207     d Wrk_cchi        s                   like(v1cchi)
047500041206     d SW_v1cchi       s                   like(v1cchi)
047600070503     d SW_cau          s                   like(W_cau)
047700070508     d note_blocco     s                   like(tgnnot)
047800040218     d W_por           s                   like(orgfil)
047900070412     d W_Cmad          s                   like(v1cmad)
048000150122     d wSaveMAD        s                   like(V1Cmad) inz
048100040531     d W_v1cogg        s                   like(tgdogg)
048200040604     d SW_v1cogg       s                   like(tgdogg)
048300041210     d Sv_pdt          s                   like(v1cpdt)
048400141210     d wTGRpdt         s                   like(TGRpdt) inz
048500150326     d W_prt           s                   like(TGDprt)
048600150625     d wSavePRT        s                   like(TGDprt) inz
048700151217     d wSavePGN        s                   like(TGNpgn) inz
048800040217
048900070322      * - Flags booleani
049000131119     d $ric_fnlrg2r    s               n   inz(*off)                            richiamo FNLRG2R
049100131119     d $abilitato      s               n   inz(*off)                            abilitato a geo
049200131119     d $riemetti       s               n   inz(*off)                            riemissione
049300131119     d $bloccof3       s               n   inz(*off)                            blocco f3
049400160512     d $Note_scritte   s               n   inz(*off)                            indica la scrit.note
049500131206     d $cons           s               n   inz(*off)                            spediz.in consegna
049600131119     d $fnarb          s               n   inz(*off)                            trovato fnarb
049700160314     d $Giacenza       s               n   inz
049800140506     d $RA_MA_17       s               n   inz                                  RA con mot.ap. 17
049900151203     d $RA_MA_18       s               n   inz                                  RA con mot.ap. 18
050000141210     d $RA_MA_60       s               n   inz                                  RA con mot.ap. 60
050001170221     d $StrCmtCtl      s              1    inz(*off)
050100151201     d $Ok_IDCprg      s               n   inz
050200151202     d $WrtTIIDC       s               n   inz
050300160120     d $AttesaRisposta...
050400160120     d                 s               n   inz
050500040217      *
050600040217      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
050700040217      *
050800040217
050900040610     D TIT1            C                   CONST('*** IMMISSIONE RICHIESTE ***')
051000040610     D TIT2            C                   CONST('** MANUTENZIONE RICHIESTE **')
051100041213
051200140429     d c_CauVarB_I7    c                   const('I7')
051300150421
051400150421     d c_Sede          c                   const('046')
051500150311
051600150311     d Digits          c                   const('0123456789')
051601170221
051602170221     d C_StrCmtCtl     c                   const('StrCmtCtl +
051603170221     d                                            lcklvl(*CHG) +
051604170221     d                                            cmtscope(*JOB)')
051605170221     d C_EndCmtCtl     c                   const('EndCmtCtl')
051606170505     d C_ImmDisp       c                   const('Immessa Disposizione Giacenz+
051607170509     d                                            a      N.')
051700160105
051800160105       // -?Attributi di Visualizzazione?
051900160105     d c_DspAtr_ND     c                   const(x'27')
052000160105     d c_DspAtrNorm    c                   const(x'20')
052100160105     d c_DspAtr_RI     c                   const(x'21')
052200170221     d c_DspAtr_HI     c                   const(x'22')
052300160105     d c_DspAtr_BL     c                   const(x'28')
052400160105     d c_DspAtrWht     c                   const(x'22')
052500160105     d c_DspAtrWht_RI  c                   const(x'23')
052600160105     d c_DspAtrWht_UL  c                   const(x'26')
052700160105     d c_DspAtrRed     c                   const(x'28')
052800160105     d c_DspAtrRed_RI  c                   const(x'29')
052900160105     d c_DspAtrRed_BL  c                   const(x'2A')
053000160105     d c_DspAtrRed_UL  c                   const(x'2C')
053100140123
053200140123       //--------------------------------------------------------------
053300140123       //?Definizione prototipi e procedure.
053400140123       //--------------------------------------------------------------
053500140123
053600141210       // -?Reperimento dati tabelle?
053700141210      /copy gaitrasrc/srcProtoPI,TRULTAB
053800141210      /copy gaitrasrc/srcProtoPR,TRULTAB
053900151201
054000151201       // -?Reperimento/Aggiornamento Numeratori in GRU?
054100151201      /copy gaitrasrc/srcProtoPR,TRUL33R
054200141210
054300141210       // -?Gestione ricerca / Controllo tabelle?
054400150911      /copy gaitrasrc/srcProtoPR,TIBS02R
054500160708
054600160708      *// // -?Aggiunge/Toglie gg/mm dalla data?
054700160708     d*// XGIOLAV         pr                  extpgm('XGIOLAV')
054800160708     d*//   xGioLAVds                         likeds(XGIOLAVds)
054900140429
055000140924       // -?Variazione bolle in arrivo?
055100140924      /copy gaitrasrc/srcProtoPR,FNLR48R
055200140429
055300140429       // -?Controllo Causali Variazione bolle?
055400140429      /copy gaitrasrc/srcProtoPR,FNLR66R
055500151130
055600151130       // -?Reperimento Disposizioni di Consegna da WEB?
055700151130     d FNLRY09ds2    e ds                  inz
055800151130     d FNLRY09R        pr                  extpgm('FNLRY09R')
055900151130     d   kpjba                             likeds(KPJBA)
056000141210
056100141210       // -?Richiesta e Controllo del PINcode?
056200141210     d trulPINds     e ds                  inz
056300141210      /copy gaitrasrc/srcProtoPR,TRULPINR
056400150310
056402170518       // -?Recupero bolla mamma e ultima bolla figlia
056403170518     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
056404170518     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
056405170518
056500150310       // -?Immissione ORM
056600150310     d FIOR05R         pr                  extpgm('FIOR05R')
056700150911     d   kpjba                             likeds(kpjba)
056800150911     d   fior05ds                          likeds(FIOR05DS)
056900150318
057000160105       // -?Storico Variazioni
057100150318     d FIDNA3R         pr                  extpgm('FIDNA3R')
057200150911     d   kpjba                             likeds(kpjba)
057300150911     d   fidna0ds                          likeds(FIDNA0DS)
057400150317
057500150317       // -?Caricamento filiali abilitate
057600150911     d TRUL31R         pr                  extpgm('TRUL31R')
057700150911     d   kpjba                             likeds(KPJBA)
057800150911     d   trul31ds                          likeds(TRUL31DS)
057900150319
058000150508       // -?Selezione tab. "RGR" = Reparto Gestione R.A.?
058100150508     d tntbRGRds     e ds                  inz
058200150508     d tntbRGRr        pr                  extpgm('TNTBRGRR')
058300150508     d   kpjba                             likeds(kpjba)
058400150508
058500150319       // -?Selezione tab. "UGR" = Utenti
058600150319     d tntbUGRds     e ds                  inz
058700150319     d tntbUGRr        pr                  extpgm('TNTBUGRR')
058800150319     d   kpjba                             likeds(kpjba)
058900150915
059000150915      *// // -?Interrogazione CAP?
059100150915     d*// TISI00ds      e ds                  inz
059200150915     d*// tisi02r         pr                  extpgm('TISI02R')
059300150915     d*//   kpjba                             likeds(kpjba)
059400150915     d*//   tisi00ds                          likeds(TISI00ds)
059500150915
059600150915       // -?Interrogazione Località?
059700150915     d TISI00ds      e ds                  inz
059800150915     d TISI30ds      e ds                  inz
059900150915     d tisi30r         pr                  extpgm('TISI30R')
060000150915     d   kpjba                             likeds(kpjba)
060100150915     d   tisi00ds                          likeds(TISI00ds)
060200150915     d   tisi30ds                          likeds(TISI30ds)
060300150911
060400150911       // -?Controllo CAP ("Pretrullo TISI95R")?
060500150911     d TISI95ds      e ds                  inz
060600150911     d tisi95r         pr                  extpgm('TISI95R')
060700150911     d   tisi95ds                          likeds(TISI95ds)
060800150915
060900151130       // -?Reperimento Terminal di partenza/arrivo?
061000150915     d FNLV55ds      e ds                  inz
061100150915     d   D55tla      e                     inz('L')
061200150915     d   D55tpt      e                     inz('P')
061300150915     d fnlv55r         pr                  extpgm('FNLV55R')
061400150915     d   fnlv55ds                          likeds(FNLV55ds)
061500170113
061600170113       // -?Parametri API QCAPCMD (Process Commands)?
061700170113     d Qcmd            s           2048    inz  varying
061800170113      /copy qSysInc/qRpgleSrc,QCAPCMD
061900170113       // -?API QCAPCMD (Process Commands)?
062000170113      /copy gaitrasrc/srcProtoPR,QCAPCMD
062100170113
062200170113       // -?Parametri gestione errori API.?
062300170113      /copy qsysinc/qrpglesrc,QUSEC
062400140429
062500040323      *
062600040108      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
062700040108      *
062800151201      * -?File TIIDC02L?
062900151201     d keyTIIDC02    e ds                  extname( TIIDC02L : *key )
063000151201     d                                     prefix( k_ )  inz
063100040120      * - file FITGD01L
063200040421     c     ktgdan        klist
063300040421     c                   kfld                    v1cant
063400040421     c                   kfld                    v1cnut
063500040218      *
063600040218     c     ktgdrec       klist
063700040421     c                   kfld                    Keyant
063800040218     c                   kfld                    Keynut
063900040218     c                   kfld                    Keypdt
064000040212      * - file FITGR01L
064100040421     c     ktgran        klist
064200040421     c                   kfld                    v1cant
064300040421     c                   kfld                    v1cnut
064400141210     c                   kfld                    wTGRpdt
064500040421      *
064600040421     c     ktgran2       klist
064700040421     c                   kfld                    tgdant
064800040421     c                   kfld                    tgdnut
064900141210     c                   kfld                    wTGRpdt
065000151215      *
065100151215     c     keyFITGR01    klist
065200151215     c                   kfld                    TGDant
065300151215     c                   kfld                    TGDnut
065400151215     c                   kfld                    TGDpdt
065500151215     c                   kfld                    TGRtri
065600040421      *
065700150325      * - file FITGD32L
065800150325     c     ktgd32        klist
065900040531     c                   kfld                    v1ctor
066000040531     c                   kfld                    W_v1cogg
066100070402      * - file FITGN01L
066200070402     c     ktgn          klist
066300070402     c                   kfld                    v1cant
066400070402     c                   kfld                    v1cnut
066500070402     c                   kfld                    v1cpdt
066600070402     c                   kfld                    keytin
066700040107
066800070502      * - file FNARB01L - FNBLP01L -
066900040210     c     ksped         klist
067000040210     c                   kfld                    v1anns
067100040210     c                   kfld                    v1lnps
067200040210     c                   kfld                    v1nrss
067300040210     c                   kfld                    v1nsps
067400041209
067500041209     c     kspedw        klist
067600041209     c                   kfld                    W_aas
067700041209     c                   kfld                    W_lnp
067800041209     c                   kfld                    W_nrs
067900041209     c                   kfld                    W_nsp
068000040211      * - file FNORM01L
068100040211     c     korm          klist
068200040211     c                   kfld                    v1poeo
068300040211     c                   kfld                    v1nsor
068400040211     c                   kfld                    v1norm
068500040211     c                   kfld                    v1nrvo
068600041209
068700041209     c     kormw         klist
068800041209     c                   kfld                    W_poe
068900041209     c                   kfld                    W_nsr
069000041209     c                   kfld                    W_nor
069100041209     c                   kfld                    W_nrv
069200040426      * - file AZCLN01L
069300040426     c     kazcln        klist
069400040426     c                   kfld                    ktfp
069500040426     c                   kfld                    ktfa
069600040426     c                   kfld                    kann
069700040426     c                   kfld                    kmes
069800040210
069900040107      *===============================================================*
070000040107      *  RIEPILOGO INDICATORI                                         *
070100040107      *---------------------------------------------------------------*
070200040126      *  01    - Visualizza numero spedizione                         *
070300040126      *  02    - Visualizza numero Orm                                *
070400040126      *  03    - Visualizza chiave generica                           *
070401170220      *  04    - Abilita tasto gestione giacenza                      *
070500041005      *  05    - Sono in Sede                                         *
070600041012      *  07    - Utente abilitato a modificare tutto                  *
070700070608      *  08    - Filiale abilitata a GEO                              *
070800070625      *  09    - Bolla in partenza non borderizzata                   *
070900040324      *  10    - Posizionamento cursore                               *
071000050118      *  11    - Posizionamento cursore                               *
071100040421      *  15    - Indicatore  da video numero reclamo                  *
071200131206      *  16    - Spedizione in consegna non visualizza F17 Note Aut   *
071300150319      *  17    - Visualizzo F11=Variazioni                            *
071400040126      *  20    - SFLDSPCTL in C01                                     *
071500040126      *  21    - SFLDSP    in C01                                     *
071600151211      *  21    - SFLEND    in C01                                     *
071700040202      *  22    - SFLCLR    in C01                                     *
071800151211      *  27    - Comodo x lookup in chiera WPOG                       *
071900040109      *  28    - Errore generico dspf                                 *
072000040212      *  29    - Visualizzo dicitura "Altre note"                     *
072100120919      *  30/48 - Errore                                               *
072200150420      *  49    - Immissione: non gestisce Filiale/Reparto/Utente gest.*
072300040303      *  50    - Manutenzione usato per proteggere/non visualizzare   *
072400040303      *          campi e comandi video                                *
072500040324      *  51/54 - Protezione note                                      *
072600150421      *  55    - Errore - n° Telefono in W04                          *
072700150911      *  56    - Errore - C.A.P. errato in W04                        *
072800151223      *  57    - Posiz. cursore in W04/C01 su Reparto x Attesa Risp.  *
072900151223      *  58    - Posiz. cursore in W04/C01 su Filiale x Attesa Risp.  *
073000070328      *  60    - Protezione note dopo richiamo gestione blocchi       *
073100070329      *  61    - emissione data consegna richiesta                    *
073200151211      *  62    - Tipo/Data/Ora consegna richiesta errato/a            *
073300070403      *  63    - Non si può più dare f3                               *
073400151211      *  64    - ERRORE ora richiesta consegna                        *
073500040324      *  70    - Protezione ... non si può modificare nulla           *
073600041012      *  72    - Manutenzione e non è stato selezionato nulla         *
073700150624      *  73    - Protezione ... non si può modificare Tipo Comunicaz. *
073800070326      *  74    - Protezione ... non si può modificare motivo apertura *
073900150624      *  75    - Comodo in lookup                                     *
074000040220      *  79    - Errore campo non numerico                            *
074100040707      *  80    - Oggetto inesistente                                  *
074200040305      *  81    - Posizionamento cursore Opzione subfile               *
074300040422      *  82    - Reverse immage cursore record allocato               *
074400141215      *  83    - Campo opzione (nel sfl) protetto & non visualizzato  *
074500150317      *  84    - RA non manutenzionabile dall'utente                  *
074600040109      *  90    - Errore                                               *
074700040107      *===============================================================*
074800040107
074900040129      *
075000150319     c                   eval      *in17 = *off
075100040129      *
075200040129     c                   do        *hival
075300040213      * se sono in modalità immissione
075400040213     c                   if        wmod = 'I'
075500040213      * Pulizia del video
075600040213     C                   exsr      SR_PUL01
075700040213      *
075800040213     c                   else
075900040213      * carico il reclamo da gestire
076000040303     c                   exsr      SR_RECupero
076100040213      *
076200040213     c                   endif
076300040213
076400040213      * gestione videata di immissione/manutenzione reclamo
076500040213
076600040129     c                   exsr      SR_RECLAMO
076700041214      * verifico se modificata la testa aggiorno i file nella parte della testata
076800041214     c                   if        Mod_testa = 'S'
076900041214     c                   exsr      SR_chiudo
077000041214     c                   endif
077100040213
077200040129      * fine programma F3
077300040213
077400040304     c                   if        *inkc  OR (*INKL and *IN50)
077500041213      *
077600040315     c                   if        *inkc and *in50
077700041213      *
077800041213      * se sono stata richiamata valorizzo la ds FIDNA0DS
077900040305     c                   eval      oa0f03 = 'S'
078000040305     c                   endif
078100040305
078200040129     c                   leave
078300040129     c                   endif
078400040217      *
078500040129     c                   enddo
078600040202
078700040202     C                   IF        Wtibs69r  <>  *BLANKS
078800040202     C                   eval      I69TLA  = 'C'
078900040202     C                   call      'TIBS69R'
079000040202     C                   parm                    TIBS69DS
079100040202     C                   ENDIF
079200040617      *
079300040617     c                   clear                   tibs02ds
079400040617     c                   movel     'C'           t02tla
079500040617     c                   call      'TIBS02R'
079600040617     c                   parm                    kpjba
079700040617     c                   parm                    tibs02ds
079800040617      *
079900040617     c                   clear                   FIDNB0DS
080000040617     c                   movel     'C'           ib0tla
080100040617     c                   movel     FIDNB0DS      kpjbu
080200040617     c                   call      'FIDNB0R'
080300040617     c                   parm                    kpjba
080400040617     c                   parm                    FIDNB0DS
080500150928      *
080600150928     c                   clear                   fidnCAds
080700150928     c                   eval      IC0tla = 'C'
080800150928     c                   call      'FIDNCAR'
080900150928     c                   parm                    fidnCAds
081000040617      *
081100141210     c                   callb     'CLOTABELLA'
081200141210     c                   parm      c_Tabel       wFilTab           1
081300170113      *
081400170113     c                   clear                   TRUL33ds
081500170113     c                   eval      I33tla = 'C'
081600170113     c                   movel(p)  TRUL33ds      KPJBU
081700170113     c                   call      'TRUL33R'
081800170113     c                   parm                    KPJBA
081900170113      *
082000170113      * -?Cancellazione override sul *file AZNUM01L?
082100170113      *  ?(usato dal *pgm chiamato TRUL33R)?
082200170113      *  ?per limitare il "Tempo massimo attesa record"?
082300170113     c                   exsr      sr_DltOvrAZNUM
082400040129      *
082500040129     c                   seton                                        lr
082600160607      *
082700040129      *---------------------------------------------------------------*
082800040129      * Stacco il numero dal numeratore                               *
082900040129      *---------------------------------------------------------------*
083000040129     c     SR_NUMERO     BegSR
083100040129      *
083200040421     c                   time                    w0140
083300040421     C                   MOVE      W0140         V1CANT
083400160302      *
083500040129     C                   clear                   Trul33ds
083600170113     c*//                eval      i33tla = 'L'
083700040129     C                   z-add     0             I33ope
083800141020     C                   z-add     039           I33cnu
083900040129     C                   z-add     1             I33num
084000040421     C                   z-add     v1cant        I33aaa
084100160302      *
084200160302     c                   DoU       v1cnut > *zeros
084300160302      *
084400040129     C                   movel(p)  trul33ds      kpjbu
084500041007      * se sono in sede richiamo il CL
084600040129     C                   CALL      'TRUL33R'
084700040129     C                   PARM                    KPJBA
084800040129     C                   eval      trul33ds = kpjbu
084900160302      * SE num. "39" allocato: si prova con il "38"
085000160302     c                   IF        o33err = 02
085100160302     c                   z-add     038           I33cnu
085200160302     c                   movel(p)  trul33ds      kpjbu
085300160302     c                   call      'TRUL33R'
085400160302     c                   parm                    KPJBA
085500160302     c                   eval      trul33ds = kpjbu
085600160302     c                   ENDIF
085700040129      *
085800160302     c                   if        o33err = 02
085900160302     c                   eval      *in28 = *on
086000160302     c                   eval      *in41 = *on
086100160302     c                   movel     Msg(60)       V1Cmsg
086200160302     c                   leavesr
086300160302     c                   endif
086400160302      *
086500040129     C                   if        o33err = *zeros
086600040202     C                   z-add     o33nri        v1cnut
086700141020     c                   endif
086800160302      *
086900141020     c                   enddo
087000160302      *
087100040421      * spengo indicatore del video
087200040421     c                   setoff                                       15
087300040416      *
087400040421      * se data chiamata uguale a zero la imposto
087500040421      * imposto ora e data inserimento chiamata
087600040421     c                   if        v1cdar = *zeros
087700040416     c                   move      w0140         v1cdar
087800040421     c                   movel     w0140         v1corr
087900040421     c                   endif
088000040421
088100040416     c                   movel     w0140         W1corr
088200040416     c                   clear                   WLBdat
088300040422     c                   move      w0140         g02dat
088400040416     c                   call      'XSRDA8'
088500040416     c                   parm                    WLBdat
088600040416     c                   z-add     G02inv        w1cdar
088700040129      *
088800040129     c                   endsr
088900040202      *---------------------------------------------------------------*
089000040213      * Routine gestione SUBFILE - CONTROL   RECLAMO                  *
089100040202      *---------------------------------------------------------------*
089200040213     c     SR_RECLAMO    BegSR
089300040202
089400040202     c                   seton                                        20
089500150421     c                   eval      *in49 = ( wMod = 'I')
089600040202      *
089700040202     c                   Do        *Hival
089800070330     c                   write     fia1z01
089900040202      * emetto la videata
090000040202     c                   exfmt     fia1c01
090100040203     c                   setoff                                       281090
090200040212     c                   setoff                                       303132
090300040212     c                   setoff                                       333435
090400040212     c                   setoff                                       363738
090500040217     c                   setoff                                       394041
090600040217     c                   setoff                                       424344
090700070330     c                   setoff                                       461162
090800141029     c                   setoff                                       644748
090900170515     c                   setoff                                       5758
091000040202     c                   clear                   V1Cmsg
091100050120     c                   clear                   SV_inkd
091200111025     c                   clear                   $riemetti
091300040304
091400040304      * F12 = Ritorno al programma chiamante  (attivo solo per 50 acceso)
091500040304
091600040304     c                   If        *inkl
091700040304     c                   leave
091800040304     c                   endif
091900040216
092000040216      * F3 = Fine  se non sono in inserimento vado a fine pgm  altrimenti
092100040216      *            prima verifco che non sia stato inserito nulla altrimenti emetto
092200040216      *            un messaggio forzabile e pulisco tutto quello che è stato inserito fino adesso
092300040416      * ATTENZIONE TUTTO QUELLO CHE E' STATO INSERITO ANDRA' PERSO !!!! FORZARE F6
092400070328      *            ma solo se non è stato modificata una spedizione per lasciato avviso o per altri
092500070326      *            blocchi F3 è disabilitato
092600040223
092700040223     c                   If        *inkc
092800040223
092900040223     c                   If        Wmod <> 'I'  or
093000040223     c                             (wmod = 'I' and rec_wrt = 0 and
093100040308     c                             v1ctor = *blanks )
093200040305      *
093300040202     c                   leave
093400040202     c                   endif
093500070326      *
093600070326     c                   If        (wmod = 'I' and rec_wrt > 0) or
093700070326     c                             (wmod = 'I' and rec_wrt = 0 and
093800070326     c                             v1ctor <> *blanks )
093900070326     c                   exsr      SR_GesWin
094000040216      *
094100040216     c                   if        *inkf
094200040223     c                   eval      *inkf  = *off
094300040223     c                   eval      *inkc  = *on
094400040216     c                   leave
094500040216     c                   endif
094600040203
094700040216     c                   iter
094800040216     c                   endif
094900040223
095000040223     c                   endif
095100170116
095200170116      * F10=Immissione ORM Telefonici
095300170116      *?(Spostato PRIMA del reperimento numero R.A.)?
095400170116     c                   If        *inKJ
095500170116     c                   exsr      SR_F10
095600170116      * accendo posizionamento cursore su chiamante
095700170116     c                   seton                                        44
095800170116     c                   iter
095900170116     c                   endif
096000151211
096100040416      * se numero reclamo uguale a zero lo imposto
096200040416     c                   if        v1cnut = *zeros
096300040416     c                   exsr      SR_numero
096400160302     c                   if        *in28
096500160302     c                   iter
096600160302     c                   endif
096700040416     c                   endif
096800150609
096900150609      * F1=Interrogazione piano dei conti
097000150609
097100150609     c                   If        *inKA
097200150609     c                   exsr      SR_ricalf
097300150609      * accendo posizionamento cursore su chiamante
097400150609     c                   seton                                        44
097500150609     c                   iter
097600150609     c                   EndIf
097700040216
097800170116      *//* F10=Immissione ORM Telefonici
097900170116     c*//                If        *inKJ
098000170116     c*//                exsr      SR_F10
098100170116      *//* accendo posizionamento cursore su chiamante
098200170116     c*//                seton                                        44
098300170116     c*//                iter
098400170116     c*//                endif
098500150318
098600150318      /free
098700150318       //?F11=Visualizza Variazioni
098800150318         IF  *inkk;
098900150318           exsr sr_F11;
099000150318           *in44 = *on;   // posizione cursore su chiamante
099100150318           iter;
099200150318         ENDIF;
099300150318      /end-free
099400040213
099500040203      * Controllo il codice e recupero categoria del cliente
099600040203
099700040202     c                   if        v1cchi > 0 and W_v1cchi <> v1cchi
099800040203
099900040202      * aggancio anagrafico
100000040203
100100040202     C                   clear                   TIBS69DS
100200040202     c                   z-add     v1cchi        I69kac
100300040202     c                   z-add     v1cchi        I69kcp
100400040202     C                   call      'TIBS69R'
100500040202     C                   parm                    tibs69DS
100600040202     C                   parm                    DS_cnaco
100700040202     C                   parm                    DS_cnind
100800040202     C                   parm                    DS_cnclp
100900040202     C                   parm                    DS_fncls
101000040202     C                   eval      wtibs69r = '1'
101100040203     c                   If         O69ERR =  '1'
101200040203     C                   eval      *IN28 = *on
101300040203     c                   movel     msg(1)        V1Cmsg
101400040203     C                   eval      *IN30 = *on
101500040213     C                   iter
101600040202     c                   Else
101700050118     c                   movel     ACOrag        v1dchi
101800041206
101900130411     c                   eval      W_v1cchi = v1cchi
102000040202     c                   movel     clpclv        v1cclv
102100040202     c                   Endif
102200040203
102300040203      * se cambiato codice cliente oppure decodificato per la prima volta riemetto la videata
102400040203     c                   iter
102500040203
102600040202     c                   endif
102700151211
102800050119      * se KD prima faccio KF poi faccio kd
102900050119     c                   If        *inkd and v1ctor <> ' '
103000050119     c                   eval      SV_inkd = *inkd
103100050119     c                   eval      *inkd  = *off
103200050119     c                   eval      *inkf  = *on
103300050119     c                   endif
103400050119
103500040203      * richiamo la routine di controllo campi video
103600040203     c                   exsr      SR_CONTR01
103700040212      * se all'uscita della routine di controllo indicatore di errore generico
103800040212      * e di riemissione della videata è acceso riemetto la videata
103900040216     c                   if        *in90 or *in28
104000040212     c                   iter
104100040212     c                   endif
104200040212
104300040615      * F6 =Conferma riga  oggetto  non eseguo se 72 acceso
104400040212
104500040615     c                   If        *inKF and not *in72
104600040212     c                   exsr      SR_CONFE
104700040213      *
104800040213      * pulisco la parte del control relativa all'oggetto
104900040213      *
105000040213     c                   exsr      SR_pulctl
105100040305      * posizionamento cursore su tipo oggetto  se sono in modalità immissione
105200040305     c                   If        Wmod = 'I'
105300040213     c                   seton                                        31
105400040305      *
105500040305      * posizionamento cursore su opzione del subfile
105600040305     c                   else
105700141215     c                   eval      *in72 = *on
105800040305     c                   endif
105900040213      * carico subfile
106000040216     c                   exsr      sr_carsbf
106100040212     c                   endif
106200050119      *
106300050119     c                   if        sv_inkd = '1'
106400050119     c                   eval      *inkf = *off
106500050119     c                   eval      *inkd = *on
106600050119     c                   clear                   sv_inkd
106700050119     c                   exsr      SR_contr01
106800050119      * se all'uscita della routine di controllo indicatore di errore generico
106900050119      * e di riemissione della videata è acceso riemetto la videata
107000050119     c                   if        *in90 or *in28
107100050119     c                   iter
107200050119     c                   endif
107300050119     c                   endif
107400050119      *
107500040213
107600040213      * F4 = FINE CHIAMATA
107700040213
107800040216     c                   If        *inKD
107900040219      *
108000040219      * se devo scrivere un dettaglio prima di chiudere la chiamata il campo W_confe = ' '
108100040219      *
108200040219     c                   if        W_confe <> 'N'
108300040216     c                   exsr      SR_CONFE
108400040219     c                   eval      W_confe = ' '
108500040219     c                   endif
108600040219
108700040219     c                   exsr      SR_chiudo
108800040216
108900040216     c                   leave
109000040216
109100040216     c                   endif
109200040202      *
109300040203     c                   enddo
109400040202      *
109500040203     c                   endsr
109600040202      *
109700040203      *---------------------------------------------------------------*
109800040203      * Routine controllo campi video                                 *
109900040203      *---------------------------------------------------------------*
110000040203     c     SR_CONTR01    BegSR
110100040203
110200040203      *
110300040211    1c                   Do        *Hival
110400040219      *
110500040219      * se dato F4  FINE CHIAMATA
110600040219      *
110700041206    2c                   if        *inkd
110800070430      * controlli della parte alta della videata
110900070430     c                   exsr      sr_contrf4
111000070430     c                   if        *in28
111100070430     c                   leave
111200070430     c                   endif
111300070430      *
111400041206    2c                   endif
111500041206      *
111600040213
111700040213      * Controllo tipo comunicazione
111800040213
111900150625     c                   clear                   dCOR
112000040213    2c                   If        v1ctco <> *blank
112100040213      * Ricerca
112200040213     c     '?'           scan      v1ctco
112300040213    3c                   if        %found
112400040217     c                   clear                   tibs02ds
112500040213     C                   MOVEL     'R'           t02mod
112600040213     C                   MOVEL     knsif         t02sif
112700040213     C                   MOVEL     'COR'         t02cod
112800040213      *
112900040213     C                   CALL      'TIBS02R'
113000040213     C                   PARM                    KPJBA
113100040213     C                   PARM                    TIBS02DS
113200040213      *
113300040213    4C                   IF        T02err = *BLANKS
113400040217     C                   movel     T02uni        DCOR
113500040213     C                   movel     T02ke1        v1ctco
113600040213     C                   movel     §CORdesc      v1dtco
113700040223      *  errore (non ritorna nulla dalla selezione pulisco il campo)
113800040213     C                   ELSE
113900040223     c                   clear                   v1ctco
114000040223     c                   clear                   v1dtco
114100040213    4C                   ENDIF
114200040213      *
114300040213     c                   eval      *in90 = *on
114400040219     C                   eval      *IN41 = *on
114500040213     c                   leave
114600040213      *
114700040213   x3C                   ELSE
114800040213      *
114900040213      * Controllo
115000040217     c                   clear                   tibs02ds
115100040213     C                   MOVEL     'C'           t02mod
115200040213     C                   MOVEL     knsif         t02sif
115300040213     C                   MOVEL     'COR'         t02cod
115400040213     C                   MOVEL     v1ctco        t02ke1
115500040213      *
115600040213     C                   CALL      'TIBS02R'
115700040213     C                   PARM                    KPJBA
115800040213     C                   PARM                    TIBS02DS
115900040213      *
116000040213    4C                   IF        t02err = *BLANKS
116100040213     C                   movel     T02uni        DCOR
116200140923     c                   if        §CORTCUT =  'N'
116300140923     c                   eval      *in28 = *on
116400140923     c                   movel     MSG(43)       V1Cmsg
116500140923     C                   eval      *IN41 = *on
116600140923     c                   clear                   v1dtco
116700140923     c                   leave
116800140923     c                   endif
116900040213     C                   movel     T02ke1        v1ctco
117000040213     C                   movel     §CORdesc      v1dtco
117100040213      *
117200040213     C                   ELSE
117300040213     c                   eval      *in28 = *on
117400040223     c                   movel     MSG(17)       V1Cmsg
117500040213     C                   eval      *IN41 = *on
117600040213     c                   leave
117700040213    4C                   ENDIF
117800040213      *
117900040213    3c                   endif
118000040213      *
118100040213   x2c                   else
118200040213      *
118300040213      *  Campo Obbligatorio
118400040213     c                   eval      *in28 = *on
118500040213     c                   movel     MSG(10)       V1Cmsg
118600040223     C                   eval      *IN41 = *on
118700040213     c                   leave
118800040213    2C                   endif
118900040213
119000040213      * Controllo data e ora comunicazione
119100040213
119200040428    2C                   if        v1cdar =  *zeros
119300040428     c                   eval      *in28 = *on
119400040428     c                   movel     MSG(11)       V1Cmsg
119500040428     C                   eval      *IN42 = *on
119600040428     C                   leave
119700040428    2C                   else
119800040213     C                   clear                   wlbdat
119900040213     C                   z-add     v1cdar        G02dat
120000040213     C                   call      'XSRDA8'
120100040213     C                   parm                    wlbdat
120200040213    3C                   if        G02err = '1'
120300040213     c                   eval      *in28 = *on
120400040213     c                   movel     MSG(11)       V1Cmsg
120500040213     C                   eval      *IN42 = *on
120600040213     C                   leave
120700040213     C                   else
120800040213     C                   z-add     G02dat        v1cdar
120900040213     C                   z-add     G02inv        Wdar
121000040213    3C                   endif
121100040223      * verifico che la data non sia maggiore della data odierna
121200040223    3c                   if        wdar > dateu
121300040223     c                   eval      *in28 = *on
121400040223     c                   movel     MSG(21)       V1Cmsg
121500040223     C                   eval      *IN42 = *on
121600040223     C                   leave
121700040223    3c                   endif
121800040223
121900040213    2C                   endif
122000040213      * Ora comunicazione
122100040213    2C                   if        v1corr > *zeros
122200040213     C                   movel     v1corr        ora
122300040213     C                   move      v1corr        minuti
122400040213
122500040213    3C                   if        ora > 24 or minuti > 59
122600040213     c                   eval      *in28 = *on
122700040213     c                   eval      *in43 = *on
122800040217     C                   movel     msg(12)       v1cmsg
122900040213     c                   leave
123000040213    3C                   endif
123100040213
123200040213     C                   else
123300040213     c                   eval      *in28 = *on
123400040213     c                   eval      *in43 = *on
123500040217     C                   movel     msg(12)       v1cmsg
123600040213     c                   leave
123700040213    2C                   endif
123800040219
123900050119      * controllo del chiamante se uguale a blanks e sono in manutenzione
124000050119
124100050119    2c                   If        v1crsc = *blanks and *in50
124200050119     c                   eval      *in28 = *on
124300050119     c                   movel     MSG(15)       V1Cmsg
124400050119     C                   eval      *IN44 = *on
124500050119     c                   leave
124600050119    2c                   endif
124700040213
124800040213      * verifico se inserito indirizzo mail corretto
124900040213
125000040213    2c                   If        v1mail <> *blanks
125100150421     c                   eval      wwMail = V1mail
125200150421     c                   exsr      sr_CtrlMail
125300150421     c                   eval      V1mail = wwMail
125400150421     c                   if        *in28
125500150421     c                   leave
125600150421     c                   endif
125700040213    2c                   endif
125800151211
125900040203      *
126000040203      * gestione OGGETTO reclamo
126100040203      *
126200040203
126300160706      * F7 Bolle   Interrogazione e ricerca bolla
126400040203      *
126500040211    2C                   IF        *INKG
126600040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
126700040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
126800040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
126900040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
127000040223     c                   exsr      SR_GESWIN2
127100040223      * ritorno alla videata senza interrogare
127200040223     c                   If        *inKL
127300040223     c                   eval      *in90 = *on
127400150324     c                   if        wMod = 'I'
127500150114     c                   eval      *in48 = *on
127600150114     c                   else
127700040223     c                   eval      *in31 = *on
127800150114     c                   endif
127900040223     c                   leave
128000040223     c                   endif
128100040205      *
128200040223     c                   endif
128300040223      *
128400160706      * se non sono in sede interrogo le bolle
128500041005      *
128600041005   a2c                   if        not *in05
128700160701     c                   clear                   KPJBU
128800160701     c                   clear                   ParamU6ds
128900160701     c                   eval      ParamU6ds.PA1flg = '2'
129000160701     c                   eval      KPJBU = ParamU6ds
129100040205      * preparo l'altro parametro da passare
129200160701     c                   clear                   fnlrU6ds
129300160701     c                   eval      iLRU6sel = 'S'
129400160701     c                   eval      iLRU6ra  = 'S'
129500160701     c                   eval      iLRU6nzd = V1naz
129600160701     c                   eval      iLRU6prd = V1prov
129700160701     c                   eval      iLRU6rsd = V1Crsc
129800161027     c                   eval      iLRU6dtd = %dec( %date() - %days(7) )
129900161027     c                   eval      iLRU6dta = %dec( %date() )
130000160701      *
130100160701     c                   call      'FNLRU6R'
130200160701     c                   parm                    KPJBA
130300160701     c                   parm                    fnlrU6ds
130400160701     c                   movel     kpjbu         ParamU6ds
130500160701     c                   if        oLRU6err = 'E'
130600160719     c                   eval      *in28 = *on
130700160719     c                   eval      *in44 = *on
130800160719     c                   movel     oLRU6msg      V1Cmsg
130900160719     c                   leave
131000160701     c                   endif
131100040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
131200160701    3C                   IF        ParamU6ds.PA1f03 = *blanks  and
131300160701     C                             oLRU6tor         = 'S'      and
131400160701     C                             oLRU6err         = *blanks
131500040217     c                   eval      v1ctor = 'S'
131600160701     c                   movel     oLRU6ogg      DS_Spediz
131700040205     c                   seton                                        01
131800040205     c                   setoff                                       0203
131900040205     C                   z-add     DSspe_aas     V1anns
132000040205     C                   z-add     DSspe_lnp     V1lnps
132100040205     C                   z-add     DSspe_nrs     V1nrss
132200040205     C                   z-add     DSspe_nsp     V1nsps
132300140204      * verifico se spedizione in consegna
132400140204     c                   exsr      Ctr_specons
132500131206
132600040211    3C                   endif
132700041007      *
132800041007      * sono in sede
132900041007      *
133000041007     c                   else
133100041007     c                   clear                   fidna1ds
133200041007     c                   eval      IA1ant = v1cant
133300041007     c                   eval      IA1nut = v1cnut
133400041007     c                   eval      IA1pdt = v1cpdt
133500041007      *
133600041008     c                   eval      %SUBST(KPJBU:2:2) = '01'
133700041007      *
133800041007     c                   CALL      'TNSB50R'
133900041007     C                   PARM                    KPJBA
134000041007     C                   PARM                    FIDNA1DS
134100041007      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
134200041007    3c                   IF        IA1INS = 'S'
134300041007     c                   eval      v1ctor = 'S'
134400041007     c                   movel     IA1ogg        DS_Spediz
134500041007     c                   seton                                        01
134600041007     c                   setoff                                       0203
134700041007     C                   z-add     DSspe_aas     V1anns
134800041007     C                   z-add     DSspe_lnp     V1lnps
134900041007     C                   z-add     DSspe_nrs     V1nrss
135000041007     C                   z-add     DSspe_nsp     V1nsps
135100041007     c                   endif
135200041007      *
135300041005   a2c                   endif
135400150112      *
135500040212     c                   eval      *in90 = *on
135600150324if  3c                   if        wMod = 'I'
135700150114     c                   exsr      sr_ImpAut_Ins
135800150114x   3c                   else
135900150112     c                   eval      *in35 = *on
136000150114e   3c                   endif
136100040205     C                   LEAVE
136200040211    2C                   endif
136300040205      *
136400040205      * F09 ORM  Interrogazione e ricerca
136500040205      *
136600040211    2C                   IF        *INKI
136700040205      *
136800040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
136900040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
137000040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
137100040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
137200040223     c                   exsr      SR_GESWIN2
137300040223      * ritorno alla videata senza interrogare
137400040223     c                   If        *inKL
137500040223     c                   eval      *in90 = *on
137600150324     c                   if        wMod = 'I'
137700150114     c                   eval      *in48 = *on
137800150114     c                   else
137900040223     c                   eval      *in31 = *on
138000150114     c                   endif
138100040223     c                   leave
138200040223     c                   endif
138300040223      *
138400040223     c                   endif
138500040223      *
138600161107     c*//                clear                   FIOR010DS
138700161107     c*//                eval      IOR010ric  = '1'
138800161107     c                   reset                   FIOR010ds
138900160708      * -?Date di ritiro:?
139000160708      *  ?· DA = oggi -1?
139100160708     c                   clear                   XGIOLAVds
139200160708     c                   eval      iXGLdata = %int( %subst( %char( %dec(
139300160708     c                                              %timestamp() ) )
139400160708     c                                              : 1 : 8 ) )
139500160708     c                   eval      iXGLsub  = 'S'
139600160708     c                   eval      iXGLgg   = 1
139700160708     c                   eval      iXGLlav  = 'S'
139800160708     c                   eval      iXGLpa   = 'P'
139900160708     c                   eval      iXGLfil  = DUTpou
140000160708     c                   call      'XGIOLAV'
140100160708     c                   parm                    XGIOLAVds
140200160708     c                   if        oXGLerr  = *blanks
140300160708     c                   eval      iOR010darD = %dec( %date( oXGLdata : *iso ) )
140400160708     c                   else
140500160708     c                   eval      iOR010darD = iXGLdata
140600160708     c                   endif
140700160708      *  ?· A  = oggi +1?
140800160708     c                   clear                   XGIOLAVds
140900160708     c                   eval      iXGLdata = %int( %subst( %char( %dec(
141000160708     c                                              %timestamp() ) )
141100160708     c                                              : 1 : 8 ) )
141200160708     c                   eval      iXGLadd  = 'S'
141300160708     c                   eval      iXGLgg   = 1
141400160708     c                   eval      iXGLlav  = 'S'
141500160708     c                   eval      iXGLpa   = 'P'
141600160708     c                   eval      iXGLfil  = DUTpou
141700160708     c                   call      'XGIOLAV'
141800160708     c                   parm                    XGIOLAVds
141900160708     c                   if        oXGLerr  = *blanks
142000160708     c                   eval      iOR010darA = %dec( %date( oXGLdata : *iso ) )
142100160708     c                   else
142200160708     c                   eval      iOR010darA = iXGLdata
142300160708     c                   endif
142400160708      * -?Ragione sociale cliente Ritiro?
142500160708     c                   eval      iOR010rsr  = V1Crsc
142600160708      * -?Provincia cliente Ritiro?
142700160708     c                   eval      iOR010prr  = V1prov
142800160708      * -?Nazione cliente Ritiro?
142900160708     c                   eval      iOR010nar  = V1naz
143000160708      * -?Non Fatti?
143100161115     c                   eval      iOR010nonF = 'NO'
143200040205      * preparo l'altro parametro da passare
143300040205     c                   clear                   fidna1ds
143400040421     c                   eval      IA1ant = v1cant
143500040205     c                   eval      IA1nut = v1cnut
143600040205     c                   eval      IA1pdt = v1cpdt
143700040205      *
143800150701     c                   CALL      'FIOR010R'
143900040205     C                   PARM                    KPJBA
144000150701     C                   PARM                    FIOR010DS
144100040205     C                   PARM                    FIDNA1DS
144200040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
144300040211    3c                   IF        IA1INS = 'S'
144400040217     c                   eval      v1ctor = 'O'
144500040205     c                   movel     IA1ogg        DS_ORM
144600040205     c                   seton                                        02
144700040205     c                   setoff                                       0103
144800040205     C                   z-add     DSorm_poe     V1poeo
144900040217     C                   z-add     DSorm_nsr     V1nsor
145000040205     C                   z-add     DSorm_nor     V1norm
145100040205     C                   z-add     DSorm_nrv     V1nrvo
145200040211    3C                   endif
145300040212     c                   eval      *in90 = *on
145400150324     c                   if        wMod = 'I'
145500150114     c                   exsr      sr_ImpAut_Ins
145600150112     c                   else
145700040219     c                   eval      *in35 = *on
145800150112     c                   endif
145900040205     C                   LEAVE
146000040211    2C                   endif
146100040211      *
146200040211      * controllo la correttezza dei campi inseriti
146300040211      *
146400040218
146500040218      * se dato enter  e tutti i campi del  control (solo la parte relativa all'oggetto della
146600040218      * chiamato) non sono valorizzati e nel subfile c'è almeno un record controllo se è stata
146700040218      * richiesta la modifica di una riga già esistente
146800040218
146900040220    2c                   if        s01nrr > 0  and v1ctor = *blanks and
147000040218     c                             v1cogg = *blanks and ormvid = *zeros and
147100040218     c                             spevid = *zeros and v1ctch = *blanks and
147200040218     c                             v1cmad = *blanks and v1ccdt = *blanks and
147300040218     c                             v1not1 = *blanks and v1not2 = *blanks and
147400040218     c                             v1not3 = *blanks and v1not4 = *blanks and
147500040218     c                             v1crsp = *blanks and v1cpor = *blanks
147600040218
147700070326      * se dato F4 vado a fine altrimenti controllo se è stata fatta richiesta di gestione
147800040219      * riga di subfile
147900040220    3c                   if        *inkd
148000040219      * imposto un campo che mi dice di non eseguire la routine di conferma riga
148100040219     c                   eval      W_confe = 'N'
148200040219
148300040219     c                   leave
148400040219      *
148500040219     c                   else
148600040218     c                   exsr      SR_gessbf
148700040218
148800040218      * se scelto un record da manutenzionare esco dal ciclo per riemettere la videata con
148900040218      * i dati da manutenzionare altrimenti proseguo nei controlli
149000040220    4c                   If        Wrk_recsbf = 'Y'
149100040218     c                   eval      *in90 = *on
149200040422     C                   eval      *IN37 = *on
149300040422     C                   eval      *IN72 = *off
149400040218     c                   leave
149500040220    4c                   endif
149600040218
149700040220    3c                   endif
149800040219
149900040220    2c                   endif
150000151223      *
150100151223      * -?Non si eseguono i controlli se sono stati rilevati errori nella?
150200151223      *  ?gestione del SBF (vedi subr. sr_GesSBF)?
150300151223      *
150400151223    2c                   if        *in28
150500151223     c                   leave
150600151223    2c                   endif
150700040422      *
150800040615      * Non eseguo i controlli se sono in manutenzione e non ho selezionato nulla
150900040422      *
151000040422    2c                   If        *in72
151100040422     c                   leave
151200040422    2c                   endif
151300150112      *
151400040217      * verifico se inserito tipo oggetto
151500040217      *
151600040217    2c                   If        v1cTOR = *blank
151700040217     C                   eval      *IN28 = *on
151800040217     c                   movel     msg(13)       V1Cmsg
151900040217     C                   eval      *IN31 = *on
152000040217     c                   leave
152100040220      *
152200040220     c                   else
152300040220      * Ricerca
152400040220     c     '?'           scan      v1ctor
152500040220    3c                   if        %found
152600040220     c                   clear                   tibs02ds
152700040220     C                   MOVEL     'R'           t02mod
152800040220     C                   MOVEL     knsif         t02sif
152900040220     C                   MOVEL     'OGR'         t02cod
153000040220      *
153100040220     C                   CALL      'TIBS02R'
153200040220     C                   PARM                    KPJBA
153300040220     C                   PARM                    TIBS02DS
153400040220      *
153500040220    4C                   IF        T02err = *BLANKS
153600040220     C                   movel     T02uni        DOGR
153700040220     C                   movel     T02ke1        v1ctor
153800040220      *  errore
153900040220     C                   ELSE
154000040220     c                   clear                   v1ctor
154100040223     C                   eval      *IN31 = *on
154200040223     c                   eval      *in90 = *on
154300040223     c                   leave
154400040220    4C                   ENDIF
154500040220      *
154600040220      *
154700040220   x3C                   ELSE
154800040220      *
154900040220      * Controllo
155000040220     c                   clear                   tibs02ds
155100040220     C                   MOVEL     'C'           t02mod
155200040220     C                   MOVEL     knsif         t02sif
155300040220     C                   MOVEL     'OGR'         t02cod
155400040220     C                   MOVEL     v1ctor        t02ke1
155500040220      *
155600040220     C                   CALL      'TIBS02R'
155700040220     C                   PARM                    KPJBA
155800040220     C                   PARM                    TIBS02DS
155900040220      *
156000040220    4C                   IF        t02err = *BLANKS
156100040223     C                   movel     T02uni        DOGR
156200040220     C                   movel     T02ke1        v1ctor
156300040220      *
156400040220     C                   ELSE
156500040220     C                   eval      *IN28 = *on
156600040220     c                   movel     msg(13)       V1Cmsg
156700040220     C                   eval      *IN31 = *on
156800040220     c                   leave
156900070327    4C                   ENDIF
157000040220      *
157100040220    3c                   endif
157200040220      *
157300040223     c                   movea     §ogrccha      ccr
157400040217    2c                   endif
157500150112      *
157600040211      * tipo oggetto
157700151211      *
157800040211      *               generico e indicatore 03 spento riemetto la videata
157900040211      *
158000040217    2c                   If        v1ctor = 'G' and not *in03
158100040211     c                   setoff                                       0102
158200040217     c                   seton                                        0332
158300040223     c                   clear                   spevid
158400040223     c                   clear                   ormvid
158401170515     c                   eval      *IN04        = *off
158500040212     c                   eval      *in90 = *on
158600040211     c                   leave
158700040211    2c                   endif
158800151211      *
158900040211      *               spedizione e indicatore 01 spento riemetto la videata
159000151211      *
159100040217    2c                   If        v1ctor = 'S' and not *in01
159200040211     c                   setoff                                       0203
159300040217     c                   seton                                        0133
159400040223     c                   clear                   v1cogg
159500040223     c                   clear                   ormvid
159600040212     c                   eval      *in90 = *on
159601170515     c                   eval      *IN04        = *off
159700040211     c                   leave
159800040211    2c                   endif
159900151211      *
160000040211      *               spedizione e indicatore 02 spento riemetto la videata
160100151211      *
160200040217    2c                   If        v1ctor = 'O' and not *in02
160300040211     c                   setoff                                       0103
160400040217     c                   seton                                        0234
160500040223     c                   clear                   v1cogg
160600040223     c                   clear                   spevid
160700070412      * pulisco anche i campi per la gestione della data consegna
160800141029     c                   setoff                                       616264
160900070412     c                   eval      $ric_fnlrg2r = *off
161000070416     c                   eval      $fnarb       = *off
161001170220     c                   eval      *IN04        = *off
161100070412
161200040212     c                   eval      *in90 = *on
161300040211     c                   leave
161400040211    2c                   endif
161500040211      *
161600040707      * verifico se inserito oggetto
161700040211      *
161800040211      * generico
161900040211    2c                   If        *in03 and v1cogg = *blank
162000040211     C                   eval      *IN28 = *on
162100040211     c                   movel     msg(2)        V1Cmsg
162200040217     C                   eval      *IN32 = *on
162201170516     c                   eval      *IN04        = *off
162300040211     c                   leave
162400040211    2c                   endif
162500040211      * spedizione
162600040211    2c                   If        *in01 and (v1lnps = *zeros or v1nsps = *zeros
162700040211     c                             or v1anns = *zeros)
162800040211     C                   eval      *IN28 = *on
162900040211     c                   movel     msg(3)        V1Cmsg
163000040211     C                   eval      *IN33 = *on
163100040211     c                   leave
163200040211    2c                   endif
163300040217      *
163400040217      * verifico se inserito anno di 2
163500040217      *
163600040217    2c                   If        *in01 and  v1anns > 60 and v1anns < 100
163700040217     c                   add       1900          V1anns
163800040217    2c                   endif
163900040217
164000040217    2c                   If        *in01 and  v1anns <=  60 and v1anns < 100
164100040217     c                   add       2000          V1anns
164200040217    2c                   endif
164300040217
164400040211      * ORM
164500040211    2c                   If        *in02 and (v1poeo=*zeros or v1norm=*zeros)
164600040211     C                   eval      *IN28 = *on
164700040211     c                   movel     msg(5)        V1Cmsg
164800040211     C                   eval      *IN34 = *on
164900040211     c                   leave
165000040211    2c                   endif
165100040211      *
165200040211      * verifico se oggetto inserito esiste
165300040211      *
165400040211      * spedizione
165500151211      *
165600040211    2c                   If        *in01 and Wspevid <> spevid
165700070625     c                   eval      *in09 = *off
165800041005      * se non sono in sede
165900041005   a2c                   if        not *in05
166000040211      * verifico se esiste nelle bolle arrivi
166100040211      *
166200160104     c     KSPED         chain     Fnarb000
166300040211    3c                   If        not %found(Fnarb01l)
166400070416     c                   eval      $fnarb = *off
166500040211      * verifico se esiste nelle bolle partenze
166600040211      *
166700160104     c     KSPED         chain     Fnblp000
166800040211    4c                   If        not %found(Fnblp01l)
166900040707      * errore solo se non sono in manutenzione
167000040707    5c                   If        not *in50
167100040211     C                   eval      *IN28 = *on
167200040211     c                   movel     msg(4)        V1Cmsg
167300040211     C                   eval      *IN33 = *on
167400040211     c                   leave
167500040707      * accendo indicatore di non visualizzazine oggetto con F2
167600040707     c                   else
167700040707     c                   seton                                        80
167800040707    5c                   endif
167900070625      * se trovata in bolla partenza
168000070625     c                   else
168100070625      * verifico se si tratta di bolla ancora da borderizzare
168200070625     c                   eval      *in09 = (blpft2 = ' ' )
168300041209
168400041206    4c                   endif
168500070416      *
168600070416     c                   else
168700070416      * se trovato in arb
168800070416     c                   eval      $fnarb = *on
168900140204      * controllo e ripristino i campi della spedizione in consegna
169000140204     c                   exsr      CTR_specons
169100041206    3c                   endif
169200041005      *
169300041005      * sono in SEDE
169400041005      *
169500041005  xa2c                   else
169600041005     c     KSPED         chain     Titas30c
169700041005    3c                   If        not %found(Titas30c)
169800041005      * errore solo se non sono in manutenzione
169900041005    4c                   If        not *in50
170000041005     C                   eval      *IN28 = *on
170100041005     c                   movel     msg(4)        V1Cmsg
170200041005     C                   eval      *IN33 = *on
170300041005     c                   leave
170400130513      * accendo indicatore di non visualizzazione oggetto con F2
170500041005     c                   else
170600041005     c                   seton                                        80
170700041005    4c                   endif
170800041005
170900041005    3c                   endif
171000041005      *
171100041005   a2c                   endif
171200040211      * mi salvo il numero spedizione
171300040211     c                   z-add     spevid        Wspevid
171400150114      * ...e pulisco quello dell'ORM
171500150114     c                   clear                   WormVid
171600070412      * pulisco anche i campi per la gestione della data consegna
171700141029     c                   setoff                                       616264
171800070412     c                   eval      $ric_fnlrg2r = *off
171900150114      *
172000150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
172100150324     c                   if        wMod = 'I'
172200150114     c                   exsr      sr_ImpAut_Ins
172300150114     c                   endif
172400070412
172500040211    2c                   endif
172600040211      *
172700040211      * ORM
172800151211      *
172900040211    2c                   If        *in02 and Wormvid <> ormvid
173000040211      * verifico se esiste in archivio
173100040211      *
173200160104     c     KORM          chain     Fnorm000
173300040211    3c                   If        not %found(Fnorm01l)
173400040211     c
173500040707      * errore solo se non sono in manutenzione
173600040707    4c                   If        not *in50
173700040211     C                   eval      *IN28 = *on
173800040211     c                   movel     msg(6)        V1Cmsg
173900040211     C                   eval      *IN34 = *on
174000040211     c                   leave
174100040707     c                   else
174200040707      * accendo indicatore di non visualizzazine oggetto con F2
174300040707     c                   seton                                        80
174400040707    4c                   endif
174500041209
174600040211    3c                   endif
174700040211      * mi salvo il numero ORM
174800040211     c                   z-add     ormvid        Wormvid
174900150114      * ...e pulisco quello della spedizione
175000150114     c                   clear                   WspeVid
175100150114      *
175200150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
175300150324     c                   if        wMod = 'I'
175400150114     c                   exsr      sr_ImpAut_Ins
175500150114     c                   endif
175600150114      *
175700040211    2c                   endif
175701170516      * se non è ne ORM ne spedizione pulisco i campi di salvataggio spedizione e orm
175702170516     c                   if        *in03
175703170516     c                   clear                   wspevid
175704170516     c                   clear                   wormvid
175705170516     c                   endif
175706170605
175707170605      * verifico se bolla legata a giacenza ma solo se bolla presente in ARB e se motivo
175708170605      * apertura è 12 =  Giacenza
175709170605     c                   If        $fnarb = *on and *in01 and
175710170605     c                             v1cmad = W_gia
175711170605     c                   exsr      Sr_CtrGiacenza
175712170605     c                   else
175713170605     c                   setoff                                       04
175714170605     c                   endif
175715170605     c
175800151211      *
175900040317      * F2   Interrogazione oggetto
176000040217      *
176100040707    2c                   IF        *inKB
176200040707      * se c'è 80 acceso significa che manca l'oggetto e quindi emetto l'errore
176300040707     c                   if        *in80
176400040707     C                   eval      *IN28 = *on
176500040707     c                   movel     msg(31)       V1Cmsg
176600040707     C                   eval      *IN31 = *on
176700040707     c                   leave
176800040707     c                   endif
176900040217      * verifico se tipo oggetto inserito
177000040217    3c                   if        v1ctor <> 'S' and v1ctor <> 'O'
177100040217     C                   eval      *IN28 = *on
177200040315     c                   movel     msg(22)       V1Cmsg
177300040217     C                   eval      *IN31 = *on
177400040217     c                   leave
177500040217    3c                   endif
177600070430      * richiamo la routine di interrogazione oggetto
177700070430     c                   exsr      SR_gesf2
177800070430      * se oggetto trovato riemetto la  videata
177900070430     c                   if        *in90
178000070430     c                   leave
178100070430     c                   endif
178200040217    2c                   endif
178300040531      *
178400040531      * se sono in immissione verifico se oggetto presente in altri reclami
178500041007      * se non è un oggetto generico
178600040604      * se modificato oggetto
178700041007    2c                   If        not *in03
178800041007      *
178900041007    3c                   If        forz_rec = 'Y'
179000040604
179100041007    4c                   if        *in01
179200041209     c                   movel(p)  spevid        SW_v1cogg
179300041007    4c                   endif
179400040604
179500041007    4c                   if        *in02
179600041209     c                   movel(p)  ormvid        SW_v1cogg
179700041007    4c                   endif
179800040604
179900041007    4c                   If        SW_v1cogg <> W_v1cogg
180000040604     c                   clear                   forz_rec
180100041007    4c                   endif
180200040604
180300041007    3c                   endif
180400040604
180500041007    3c                   If        not *in50 and WRK_recsbf <> 'Y' and
180600040531     c                             forz_rec = ' '
180700040531
180800041007    4c                   if        *in01
180900041209     c                   movel(p)  spevid        W_v1cogg
181000041007    4c                   endif
181100040531
181200041007    4c                   if        *in02
181300041209     c                   movel(p)  ormvid        W_v1cogg
181400041007    4c                   endif
181500040531
181600150325      * verfiico in FITGD32L
181700040531
181800160104     c     Ktgd32        chain     fitgd32
181900150325    4c                   if        %found(fitgd32l)
182000040531     c                   eval      *in28 = *on
182100040531     c                   movel     msg(30)       V1Cmsg
182200040531     C                   eval      *IN31 = *on
182300040531     C                   eval      forz_rec = 'Y'
182400040531     c                   leave
182500041007    4c                   endif
182600040531
182700041007    3c                   endif
182800041007
182900041007    2c                   endif
183000040211      *
183100040211      * verifico l'inserimento del CHI CHIAMA
183200040220      *
183300040220     c                   clear                   v1dtch
183400040220      *
183500040220    2c                   If        v1cTch = *blank
183600040220     C                   eval      *IN28 = *on
183700040220     c                   movel     msg(07)       V1Cmsg
183800040220     C                   eval      *IN35 = *on
183900040220     c                   leave
184000040220      *
184100040220     c                   else
184200040220      * Ricerca
184300040220     c     '?'           scan      v1ctch
184400040220    3c                   if        %found
184500040220     c                   clear                   tibs02ds
184600040220     C                   MOVEL     'R'           t02mod
184700040220     C                   MOVEL     knsif         t02sif
184800040220     C                   MOVEL     'CCR'         t02cod
184900040220      *
185000040220     C                   CALL      'TIBS02R'
185100040220     C                   PARM                    KPJBA
185200040220     C                   PARM                    TIBS02DS
185300040220      *
185400040220    4C                   IF        T02err = *BLANKS
185500040220     C                   movel     T02uni        DCCR
185600040220     C                   movel     T02ke1        v1ctch
185700040220     c                   movel     §ccrdesc      v1dtch
185800040220      *  errore
185900040220     C                   ELSE
186000040220     c                   clear                   v1ctch
186100040220     c                   clear                   v1dtch
186200040220    4C                   ENDIF
186300040220      *
186400040220     C                   eval      *IN35 = *on
186500040220     c                   eval      *in90 = *on
186600040220     c                   leave
186700040220      *
186800040220   x3C                   ELSE
186900040220      *
187000040220      * Controllo
187100150112     c                   exsr      sr_Ctrl_CCR
187200150112     c                   if        *in28
187300150112     c                   leave
187400150112     c                   endif
187500040220      *
187600040220    3c                   endif
187700040220      *
187800040220    2c                   endif
187900151211
188000040220      * verifico se chi chiama è ammesso per quel tipo oggetto
188100040220
188200150421    2c                   if        %lookup( V1Ctch : CCR ) = *zero
188300040220      * errore chi chiama non ammesso
188400040211     C                   eval      *IN28 = *on
188500040220     c                   movel     msg(16)       V1Cmsg
188600040211     C                   eval      *IN35 = *on
188700040211     c                   leave
188800040211    2c                   endif
188900040211      *
189000040211      * verifico l'inserimento del MOTIVO del reclamo
189100070412
189200070412      * se modificato rispetto al precedente pulisco alcuni campi
189300070412     c                   If        v1cmad <> W_cmad
189400070412     c                   eval      $ric_fnlrg2r = *off
189500141029     c                   setoff                                       616264
189600070412     c                   eval      W_cmad = v1cmad
189700141030     c                   clear                   v1ctcr
189800141030     c                   clear                   v1cdcr
189900141030     c                   clear                   v1cocr
190000070412     c                   endif
190100070412
190200150421      * - Ricerca / Controllo
190300150421     c                   eval      wwcMAD = V1Cmad
190400150421     c                   exsr      sr_CtrlMAD
190500150421     c                   eval      V1Cmad = wwcMAD
190600150422     c                   eval      V1Dmad = wwdMAD
190700150421     c                   if        *in28  or  *in90
190800150421     c                   leave
190900150421     c                   endif
191000140212      *
191100140212      * F17=Note LDV
191200140212      *
191300140217     c                   clear                   Not_LdV
191400140217     c                   if        Wmod = 'I'  and  V1Cmad = W_Lav
191500140217     c                                         and  $Cons  = *on
191600140217     c                   eval      Not_LdV = 'S'
191700140217     c                   endif
191800140217      *
191900140212if  2c                   If        *inKR
192000140217      * - SE "Immissione" & "Obbligo Immissione Note LdV"   oppure
192100140217      *   SE "Immissione" & "Motivo" = "  2" & Spedizione in Consegna
192200140217      *   => Note all'F6
192300140217     c                   if        (Wmod = 'I'  and  §MARnotLDV = 'S')  or
192400140217     c                             (Wmod = 'I'  and  V1Cmad = W_Lav
192500140217     c                                          and  $Cons  = *on)
192600140212     C                   eval      *in28 = *on
192700140212     c                   movel     Msg(40)       V1Cmsg
192800140212     c                   leave
192900140212     c                   endif
193000140212     c                   exsr      SR_F17
193100140212     c                   leave
193200140212e   2c                   endif
193300131119      *
193400070322      *  verifico se la causale mi invita all'inserimento della data consegna richiesta
193500070326      *  se non sono in modifica , non sono in sede e sono abilitato a GEO
193600070416      *  e se non ho già richiamato il pgm dei blocchi e se sono un oggetto spedizione  presente
193700070416      *  in arrivo
193800070330    2c                   if        §marrdco = 'S'  and not *in61
193900070402     c                             and not *in74  and dutlpo <> 'S'
194000070403     c                             and $abilitato = *on and $ric_fnlrg2r = *off
194100070416     c                             and v1ctor = 'S' and $fnarb = *on
194200070322      * richiamo la routine gestione data consegna
194300070322     c                   exsr      SR_GES_DTA
194400070329    3c                   if        $riemetti = *on
194500070327     c                   seton                                        90
194600070327     c                   leave
194700070329    3c                   endif
194800070329    2c                   endif
194900070329      * Controllo tipo a data consegna richiesta
195000070403    2c                   if        v1ctcr<> *blanks and *in61 and
195100141030     c                             v1cdcr = 0 and v1cocr = 0
195200070403     c                   eval      *IN28 = *on
195300070403     c                   movel     msg(11)       V1Cmsg
195400070403     c                   eval      *IN62 = *on
195500070403     c                   leave
195600070403    3c                   endif
195700141030      * Controllo ora consegna richiesta presente senza data
195800141030    2c                   if        *in61 and  v1cdcr = 0
195900141030     c                             and v1cocr > 0
196000141030     c                   eval      *IN28 = *on
196100141030     c                   movel     msg(11)       V1Cmsg
196200141030     c                   eval      *IN62 = *on
196300141030     c                   leave
196400141030    3c                   endif
196500070329    2c                   if        v1cdcr>0 and *in61
196600070329     c* controllo formale data
196700070329     c                   clear                   wlbdat
196800070329     c                   z-add     v1cdcr        g02dat
196900070329     c                   clear                   g02err
197000070329     c                   call      'XSRDA8'
197100070329     c                   parm                    wlbdat
197200070329      * data formalmente errata
197300070329    3c                   if        g02err = '1'
197400070329     c                   eval      *IN28 = *on
197500070329     c                   movel     msg(11)       V1Cmsg
197600070329     c                   eval      *IN62 = *on
197700070329     c                   leave
197800070329    3c                   endif
197900141029      * ora formalmente errata
198000141029     C                   if        v1cocr <> *zeros
198100141030     C                   movel     v1cocr        ora
198200141029     C                   move      v1cocr        minuti
198300141029
198400141030     C                   if        ora > 24 or minuti > 59
198500141030     C                             or ora < 0
198600141029     c                   eval      *IN28 = *on
198700141029     c                   movel     msg(44)       V1Cmsg
198800141029     c                   eval      *IN64 = *on
198900141029     c                   leave
199000141029     C                   endif
199100141030
199200141030     C                   endif
199300070329      * richiamo il controllo della data da FNLR48R
199400070329     c                   z-add     g02inv        Wdcr
199500070329     c                   z-add     g02dat        v1cdcr
199600070329     c                   clear                   fnlr48ds
199700070329     c                   eval      d48cvb='CR'
199800070329     c                   eval      d48ffr='F'
199900070329     C                   z-add     v1anns        D48AAS
200000070329     C                   z-add     v1lnps        D48LNP
200100070329     C                   z-add     v1nrss        D48NRS
200200070329     c                   z-add     v1nsps        D48NSP
200300070329     c                   movel     ARBCBO        D48CBO
200400070329     c                   movel     'A'           D48TBO
200500070329     c                   movel     'G'           D48trc
200600070329     c                   movel     fnlr48ds      KPJBU
200700140429     c                   clear                   dsARBd
200800070329     c                   movel     v1ctcr        §BGTCR
200900141029     c                   z-add     v1cocr        §BGHCR
201000070329     c                   z-add     wdcr          §BGDCR
201100070329     c                   movel     arbgc1        §BGGC1
201200070329     c                   movel     arbgc2        §BGGC2
201300070329     c                   movel     arbtc1        §BGTC1
201400070329     c                   movel     arbtc2        §BGTC2
201500070329     c                   call      'FNLR48R'
201600070329     c                   parm                    KPJBA
201700070329     c                   parm                    DSARBD
201800070329     c                   parm                    DSARBK
201900070329     c                   parm                    DARBT
202000070329     c                   parm                    DSARBG
202100070329     c                   parm                    trul90ds
202200070329     c                   movel     Kpjbu         fnlr48ds
202300070329      * ritorno errore da controllo fnlr48r
202400070402    3c                   if        d48err<>*blanks
202500070329     c                   movel     d48msg        v1cmsg
202600070329     c                   eval      *IN28 = *on
202700070329     c                   eval      *IN62 = *on
202800070329     c                   leave
202900070329    3c                   endif
203000070329    2c                   endif
203100120919
203200120919      * se il motivo di apertura RA prevede l'immissione delle note le richiedo
203300120919      * obbligatorie
203400120919     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
203500131017     c                             V1not3 = *blanks and V1not4 = *blanks and
203600120919     c                             §MARnote = 'S'
203700120919     c                   eval      *in28 = *on
203800120919     c                   eval      *in48 = *on
203900120919     c                   eval      V1Cmsg = msg(38)
204000120919     c                   leave
204100120919     c                   ENDIF
204200070329
204300120913      * se il motivo di apertura RA prevede una chiusura automatica
204400120913      * imposto il motivo di chiusura se non già impostato
204500120913     c                   IF        V1Ccdt = *blanks and §MARmotch <> *blanks
204600120913     c                   eval      V1Ccdt = §MARmotch
204700120913     c                   ENDIF
204800040212      *
204900040212      * verifico l'inserimento del CODICE di CHIUSURA del reclamo
205000040220      *
205100040220     c                   clear                   v1dcdt
205200040420     c                   clear                   v1crsp
205300040212    2c                   If        v1ccdt <> *blanks
205400120913
205500120913      * se il motivo di chiusura messo dell'utente è diverso da quello
205600150720      * previsto in tabella MAR errore  se l'utente non è abilitato
205700130705      * alla modifica
205800130705     c                   If        not *in07
205900120913     c                   IF        V1Ccdt <> §MARmotch and §MARmotch <> *blanks
206000120913     c                   eval      *in28 = *on
206100120913     c                   eval      *in37 = *on
206200120913     c                   eval      V1Cmsg = MSG(37)
206300131119     c                   eval      V1Cmsg = %trim(V1Cmsg) + ' "' +
206400120913     c                             %trim(§MARmotch) + '"'
206500120913     c                   leavesr
206600120913     c                   ENDIF
206700130705     c                   Endif
206800150720
206900150720      * -?Se R.A. con Motivo Apertura " 70": la Sede NON può chiuderla?
207000150720     c                   If        *in05  and  wwcMAD = c_ReclSede
207100150720     c                   eval      *in28 = *on
207200150720     c                   eval      *in37 = *on
207300150720     c                   eval      V1Cmsg = Msg(52)
207400150720     c                   leavesr
207500150720     c                   EndIf
207600120913
207700040212     c                   clear                   tibs02ds
207800040212      *
207900040212      * Ricerca
208000040212     c     '?'           scan      v1ccdt
208100040212    3c                   if        %found
208200040217     c                   clear                   tibs02ds
208300040212     C                   MOVEL     'R'           t02mod
208400040212     C                   MOVEL     knsif         t02sif
208500040212     C                   MOVEL     'CHR'         t02cod
208600040212      *
208700040212     C                   CALL      'TIBS02R'
208800040212     C                   PARM                    KPJBA
208900040212     C                   PARM                    TIBS02DS
209000040212      *
209100040212    4C                   IF        T02err = *BLANKS
209200040212     C                   movel     T02uni        DCHR
209300040212     C                   movel     T02ke1        v1ccdt
209400040212     C                   movel     §chrdesc      v1dcdt
209500040420     C                   movel     §chrresp      v1crsp
209600040223      *  errore (non è stato selezionato nulla pulisco  il campo e mi posiziono)
209700040212     C                   ELSE
209800040223     C                   clear                   v1ccdt
209900040223     C                   clear                   v1dcdt
210000040420     C                   clear                   v1crsp
210100040212    4C                   ENDIF
210200040212      *
210300040212     c                   eval      *in90 = *on
210400040219     C                   eval      *IN37 = *on
210500040212     c                   leave
210600040212      *
210700040212   x3C                   ELSE
210800040212      *
210900040212      * Controllo
211000040217     c                   clear                   tibs02ds
211100040212     C                   MOVEL     'C'           t02mod
211200040212     C                   MOVEL     knsif         t02sif
211300040212     C                   MOVEL     'CHR'         t02cod
211400040212     C                   MOVEL     v1ccdt        t02ke1
211500040212      *
211600040212     C                   CALL      'TIBS02R'
211700040212     C                   PARM                    KPJBA
211800040212     C                   PARM                    TIBS02DS
211900040212      *
212000040212    4C                   IF        t02err = *BLANKS
212100040212     C                   movel     T02uni        DCHR
212200040212     C                   movel     §CHRdesc      v1dcdt
212300040420     C                   movel     §chrresp      v1crsp
212400040212      *
212500040212     C                   ELSE
212600040212     c                   eval      *in28 = *on
212700040223     c                   movel     MSG(19)       V1Cmsg
212800040212     C                   eval      *IN37 = *on
212900040212     c                   leave
213000040212    4C                   ENDIF
213100040212      *
213200040212    3c                   endif
213300040212      *
213400040212    2C                   endif
213500151218      *
213600151218      *
213700150624      *?Impostazione STATO (ex Priorità):?
213800150624      * Se c'è la causale chiusura metto priorità '9' (in inserimento)
213900150624      *   o '8' (in modifica - se modificata la causale Chiusura)
214000160108if  2c                   IF        V1Ccdt <> *blanks
214100150624      *
214200160108if  3c                   IF        wmod = 'I'
214300150624      * -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
214400150624      *  ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
214500150625      *  ?(già reperita nella subr. SR_CONTR01)?
214600150624     c                   if        §CORstato <> *blanks
214700150624     c                   eval      V1Cprt = §CORstato
214800150624     c                   else
214900150313     c                   eval      V1Cprt = '9'
215000150624     c                   endif
215100160108x   3c                   ELSE
215200160108sel 4c                   Select
215300160108      * -?SE modalità Modifica & R.A. Chiusa & R.A. in attesa di?
215400160108      *  ?risposta (questa) => si imposta lo Stato "0"?
215500160108      *  ?(la R.A. rimane aperta, girata in Gestione alla fil. che?
215600160108      *  ?Attende Risposta)?
215700160108w   4c                   When      V1CfilR  > *zero         and
215800160201     c                             V1CrepR <> *blank        and
215900160108     c                             (V1Ccdt <> *blank        and
216000160108     c                              dTGDfar.§TGDris = *blank   or
216100160108     c                              V1Ccdt  = *blank        and
216200160108     c                              dTGDfar.§TGDris = 'S')
216300160108     c                   eval      V1Cprt = '0'
216400160108      * -?SE modificato il codice Chiusura => si imposta lo Stato "8"?
216500160108w   4c                   When      V1ccdt <> tgdcdt
216600150324     c                   eval      V1Cprt = '8'
216700160108e   4c                   EndSl
216800160108e   3c                   ENDIF
216900150624      *
217000150624      * Altrimenti (se NON c'è la causale di chiusura) metto
217100150624      *   priorita '0' (in inserimento) o '1' (in modifica)
217200160108x   2c                   ELSE
217300150624      *
217400150421     c                   If        wmod = 'I'
217500150421     c                   eval      V1Cprt = '0'
217600150421     c                   Else
217700150313     c                   eval      V1Cprt = '1'
217800150421     c                   EndIf
217900150624      *
218000160108e   2c                   ENDIF
218100150320      *
218200150320      * Decodifico (potrebbe essere cambiato)
218300150320     c                   clear                   tibs02ds
218400150320     C                   MOVEL     'C'           t02mod
218500150320     C                   MOVEL     knsif         t02sif
218600150320     C                   MOVEL     'PRR'         t02cod
218700150320     C                   MOVEL     v1cprt        t02ke1
218800150320      *
218900150320     C                   CALL      'TIBS02R'
219000150320     C                   PARM                    KPJBA
219100150320     C                   PARM                    TIBS02DS
219200150320      *
219300150320    4C                   IF        t02err = *BLANKS
219400150320     C                   movel     T02uni        DPRR
219500150320     C                   movel     §PRRdesc      v1dprt
219600150320     c                   endif
219700151217      *
219800150324      * se RA aperta richiedo note obbligatorie
219900150324     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
220000150324     c                             V1not3 = *blanks and V1not4 = *blanks and
220100151211     c                             V1Ccdt = *blanks
220200151211     c                             and not *inks
220300150324     c                   eval      *in28 = *on
220400150324     c                   eval      *in48 = *on
220500150324     c                   eval      V1Cmsg = msg(38)
220600150324     c                   leave
220700150324     c                   ENDIF
220800160107      *
220900160204      * -?Note obbligatorie per R.A. "chiusa" (poi ri-aperta automaticam.)?
221000160204      *  ?con Attesa Risposta = "S" & Ricevuta Risposta = " "?
221100160108      *  ?(con salvataggio del tasto funzionale F6)?
221200160108     c                   eval      $SaveF6  = *inKF
221300160204if  2c                   If        Not  *inKS             and
221400160204     c                             V1Ccdt <> *blank       and
221500160204     c                             dTGDfar.§TGDatt = 'S'  and
221600160204     c                             dTGDfar.§TGDris = *blank
221700160108      *
221800160204      * -?Reperisco ULTIMO Progressivo Note inserito?
221900160107     c     kTGN          setGT     FITGN000
222000160107     c     kTGN          readPE    FITGN000
222100160108if  3c                   if        %eof(FITGN01L)
222200160107     c                   clear                   TGNpgn
222300160108e   3c                   endif
222400160108      *
222500160108if  3c                   if        TGNpgn  <= wSavePGN
222600160107     c                   eval      *inKS    = *on
222700160108e   2c                   endif
222800160108      *
222900160108e   2c                   EndIf
223000040212      *
223100040212      * F18 - GESTIONE NOTE
223200040212      *
223300040212    2C                   IF        *INKS
223400040212      *
223500040212     c                   exsr      Sr_f18
223600040212      *
223700040219     c                   seton                                        39
223800040212     c                   eval      *in90 = *on
223900040212     c                   leave
224000040212      *
224100040212    2c                   ENDIF
224200150313
224300150313      * mi salvo l'indicatore 48 per posizionamento su campo note
224400150313     c                   eval      sav_in48 = *in48
224500150313      * spengo il 48
224600150313     c                   setoff                                       48
224700160104      *
224800160104      * -?Ripristino tasto funzionale F6?
224900160104     c                   eval      *inKF = $SaveF6
225000150311
225100150420      * Controllo la Filiale Gestione (SE gestita)
225200150428     c                   IF        wMod <> 'I'
225300150420      *
225400150421      * - Ricerca / Controllo
225500150421     c                   eval      wwcPOG = V1Cpog
225600150717     c*//?già impostata? eval      wwcMAD = V1Cmad
225700150421     c                   exsr      sr_CtrlPOG
225800150421     c                   eval      V1Cpog = wwcPOG
225900150421     c                   if        *in28  or  *in90
226000150421     c                   leave
226100150421     c                   endif
226200150420      *
226300150420     c                   ENDIF
226400040212      *
226500040323      * verifico l'inserimento dell'UTENTE che ha in GESTIONE il reclamo
226600150421      * (SE gestito)
226700150622     c                   clear                   $Reparto
226800150622      *
226900150428     c                   IF        wMod <> 'I'
227000150423      *
227100150422    2c                   If        V1Crpg <> *blanks
227200150421      *
227300150428      /free
227400150429
227500150429             // -?Ricerca/Controllo Reparto Gestione?
227600150617             //  ?(NON della Sede: per Sede si possono inserire solo Utenti)?
227700150617             If  V1Cpog <> c_Sede;
227800150617
227900150617               wwcRPR = V1Crpg;
228000150617               exsr  sr_CtrlRPR;
228100150617               V1Crpg = wwcRPR;
228200150617               //V1Drpg = wwdRPR;
228300150617
228400150617               if  *in90;
228500150617                 leave;
228600150617               endif;
228700150617
228800150617               $Reparto = ( Not *in28 );
228900150617
229000150617               *in28 = *off;
229100150617               *in39 = *off;
229200150617               *in45 = *off;
229300150617               clear  V1Cmsg;
229400150617
229500150617             EndIf;
229600150429
229700150429      /end-free
229800150421      *
229900150429if  3c                   IF        Not $Reparto
230000150421      *
230100150421      * 2° controllo come Utente Gestione
230200150421      *
230300040420      * verifico se esiste utente nell'archivio profili
230400040420     c     v1crpg        chain     azute01l
230500040420      *
230600150428    5c                   if        %found(azute01l) and UTEdsc > DateU
230700150311      * se lo trovo verifico se è della filiale di gestione
230800150723    6c                   if        %editc(UTEfil:'X') <> V1Cpog
230900040420     c                   eval      *in28 = *on
231000150313     c                   eval      *in46 = *on
231100040420     c                   movel     MSG(23)       V1Cmsg
231200040420     c                   leave
231300040420    6c                   endif
231400160607      *
231500150423    5c                   else
231600160607      *
231700040420     c                   eval      *in28 = *on
231800040420     c                   eval      *in39 = *on
231900150617     c                   if        V1Cpog <> c_Sede
232000150429     c                   movel     Msg(20)       V1Cmsg
232100150617     c                   else
232200150617     c                   eval      V1Cmsg = %trimR( Msg(20) )
232300150617     c                                    + ': ammessi solo utenti di sede'
232400150617     c                   endif
232500040420     c                   leave
232600040420    5c                   endif
232700150421      *
232800150421e   3c                   ENDIF
232900160607      *
233000150422   x2c                   Else
233100150421      * se Reparto/Utente a blanks do errore
233200040422     c                   eval      *in28 = *on
233300040422     c                   eval      *in39 = *on
233400040422     c                   movel     MSG(29)       V1Cmsg
233500040422     c                   leave
233600150422    2C                   EndIf
233700150422      *
233800150422 e_1bc                   ENDIF
233900160207      *//
234000160207      *//* se arrivo qua vuol dire che non ho avuto errori sui campi
234100160207      *//* filiale e utente gestione
234200160207      *//* quindi ripristino l'indicatore 48
234300160207      *//* => meglio farlo alla fine della subroutine...
234400160207     c*//                eval      *in48 = sav_in48
234500160203      *
234600160203      /free
234700160203
234800160203         // -?Confronto tra Reparto/Filiale in Attesa Risposta?
234900160203         //            ?con Reparto/Filiale di Gestione?
235000160203         If  dTGDfar.§TGDatt = 'S'  and
235100160203             V1Cpog = V1CfilR       and  V1Crpg = V1CrepR;
235200160203           *in28  = *on;
235300160203           *in46  = *on;
235400160203           V1Cmsg = Msg(59);
235500160203           leave;
235600160203         EndIf;
235700160203
235800160203      /end-free
235900040527      *
236000040527      * se responsabilità vettore  e P.O. responsabile non valorizzato metto me stesso
236100040527      * se responsabilità no vettore  e P.O. responsabile deve essere =  a blank
236200040527      *
236300040527    2c                   if        v1crsp = 'V'  and (v1cpor = *blanks
236400040527     c                             or v1cpor = '000')
236500130513     c                   select
236600130513     c                   when      §MARffr = 'A'
236700130513     c                   select
236800130513     c                   when      *in05
236900130513     c                   move      TASlna        V1Cpor
237000130513     c                   when      $fnARB = *on
237100130513     c                   move      ARBlna        V1Cpor
237200130513     c                   other
237300130513     c                   move      BLPlna        V1Cpor
237400130513     c                   endsl
237500130513     c                   when      §MARffr = 'P'
237600130513     c                   select
237700130513     c                   when      *in05
237800130513     c                   move      TASlnp        V1Cpor
237900130513     c                   when      $fnARB = *on
238000130513     c                   move      ARBlnp        V1Cpor
238100130513     c                   other
238200130513     c                   move      BLPlnp        V1Cpor
238300130513     c                   endsl
238400130513     c                   other
238500040527     c                   move      v1cpot        v1cpor
238600130513     c                   endsl
238700040527    2c                   endif
238800040527
238900040527    2c                   if        v1crsp <> 'V'
239000040527     c                   clear                   v1cpor
239100040527     c                   clear                   v1dpor
239200040527    2c                   endif
239300040211      *
239400040212      * verifico l'inserimento del P.O. responsabile
239500040220      *
239600040220     C                   clear                   v1dpor
239700040212    2c                   If        v1cpor <> *blanks
239800040212      *
239900040212      * Ricerca
240000040212     c     '?'           scan      v1cpor
240100040212    3c                   if        %found
240200040212     C                   clear                   dstnsd
240300040212     C                   call      'TNSD24R'
240400040212     C                   parm                    §cod
240500040212     C                   parm                    §tip
240600040212     C                   parm                    §des
240700040212     C                   parm                    §pra
240800040212     C                   movel     §cod          v1cpor
240900040212     C                   movel     §des          v1dpor
241000040212     c                   eval      *in90 = *on
241100040219     C                   eval      *IN40 = *on
241200040212     c                   leave
241300040212    3C                   endif
241400040217    2C                   endif
241500040217      *
241600040217     C     v1cpor        ifeq      *blanks
241700040217     C                   move      *all'0'       v1cpor
241800040217     C                   endif
241900040212      *
242000040212     C                   testn                   v1cpor               79
242100040212     C                   move      v1cpor        w001a             1
242200040217      *
242300040217     c                   move      v1cpor        W_por
242400040212      *
242500040217    2C     *in79         ifeq      *off
242600040212     C     w001a         orlt      '0'
242700040212     c                   eval      *in28 = *on
242800040212     c                   movel     MSG(9)        V1Cmsg
242900040212     C                   eval      *IN40 = *on
243000040212     c                   leave
243100040217    2C                   endif
243200040212      * verifico se P.O. esistente
243300040217    2c                   if        W_por > 0
243400160104     C     W_por         chain     azorg
243500040212    3C                   if        %found(azorg01l)
243600040212     C                   movel     orgdes        v1dpor
243700040420      * verifico se P.O. valido
243800040617     c                   if        orgfva<>' ' or (orgfag<>'F' and orgfag<>'A'
243900040617     c                             and orgfil <> 46)
244000040420     c                   eval      *in28 = *on
244100040420     c                   movel     MSG(9)        V1Cmsg
244200040420     C                   eval      *IN40 = *on
244300040420     c                   leave
244400040420     c                   endif
244500040212      *  errore
244600040212     C                   ELSE
244700040212     c                   eval      *in28 = *on
244800040213     c                   movel     MSG(9)        V1Cmsg
244900040212     C                   eval      *IN40 = *on
245000040212     c                   leave
245100040212    3C                   ENDIF
245200040212      *
245300040217    2C                   ENDIF
245400070608
245500070626      * controllo causale variazione per bolle se esiste  e se bolla valida per la variazione
245600070608
245700070608     c                   clear                   fnlr66ds
245800070608    2c                   if        V1ccva  <> *blanks
245900070626      * per 09 acceso emetto un msg di errore
246000160208     c* permetto SOLO in arrivo la modofica delle bolle non ancora borderizzate
246100160208    3c**                 If        *in09
246200160208     c**                 eval      *in28 = *on
246300160208     c**                 movel     MSG(34)       V1Cmsg
246400160208     C**                 eval      *IN47 = *on
246500160208     c**                 leave
246600160208    3c**                 endif
246700160208     c**
246800070608     c                   movel     V1anns        d66aas
246900070608     c                   movel     V1lnps        d66lnp
247000070608     c                   movel     V1nrss        d66nrs
247100070608     c                   movel     V1nsps        d66nsp
247200071018      * determino se sono il p.o. di partenza o il p.o. di arrivo
247300071018
247400160208      * se non trovata bolla in arb errore se la bolla in FNBLP è già partita
247500160208    3c******             If        $fnarb = *off
247600160208    3c                   If        $fnarb = *off and not *in09
247700071018     c                   eval      *in28 = *on
247800071018     c                   movel     MSG(35)       V1Cmsg
247900071018     C                   eval      *IN47 = *on
248000071018     c                   leave
248100071018    3c                   endif
248200071018
248300160208     c* Se la bolla deve ancora partire uso BLPLNA
248400160208     c                   if        *in09
248500160208     c                   move      blplna        w003a
248600160208     c                   else
248700130611     c                   move      arblna        w003a
248800160208     c                   endif
248900160208     c*
249000130611     c     w003a         lookup    wPOG                                   27
249100071018    3c                   if        %found
249200071018     c                   movel     'A'           d66tbo
249300071018   x3c                   else
249400071018      * verifico se sono la linea di partenza
249500160208     c                   if        *in09
249600160208     c                   move      blplnp        w003a
249700160208     c                   else
249800130611     c                   move      arblnp        w003a
249900160208     c                   endif
250000160208     c*
250100130611     c     w003a         lookup    wPOG                                   27
250200170512    4c                   if        %found
250300071018     c                   movel     'P'           d66tbo
250400071018    4c                   endif
250500071018
250600071018    3c                   endif
250700160208
250800160208    3c                   If        *in09  and d66tbo='P'
250900160208     c                   eval      *in28 = *on
251000160208     c                   movel     MSG(34)       V1Cmsg
251100160208     C                   eval      *IN47 = *on
251200160208     c                   leave
251300160208    3c                   endif
251400071018
251500071018      * se bolla non in gestione al Po Errore
251600071018    3c                   if        d66tbo = ' '
251700071018     c                   eval      *in28 = *on
251800071018     c                   movel     MSG(36)       V1Cmsg
251900071018     C                   eval      *IN47 = *on
252000071018     c                   leave
252100071018    3c                   endif
252200070608      *
252300070608     c     '?'           scan      v1ccva                                 90
252400070608      * Ricerca causale
252500070608    3c                   if        *in90
252600070608     c                   movel     'V'           d66ges
252700070608      *
252800070608     c                   movel     fnlr66ds      kpjbu
252900070608     c                   call      'FNLR66R'
253000070608     c                   parm                    kpjba
253100070608     c                   movel     kpjbu         fnlr66ds
253200070608     c                   movel     d66cvb        V1ccva
253300070608     c                   movel     d66des        v1dcva
253400070608   X3c                   else
253500070608      * Controllo causale inserita
253600070608     c                   movel     'C'           d66ges
253700160208     c* Posso variare anche le bolle in partenza per alcune causali
253800160208     c                   movel     'S'           d66BNP
253900070608     c                   movel     V1ccva        d66cvb
254000070608      *
254100070608     c                   movel     fnlr66ds      kpjbu
254200070608     c                   call      'FNLR66R'
254300070608     c                   parm                    kpjba
254400070608     c                   movel     kpjbu         fnlr66ds
254500070608     c                   movel     d66cvb        v1ccva
254600070608     c                   MOVEL     d66des        v1dcva
254700070608      *
254800070608      * Errore
254900070608    4c                   if        d66msg <> *blank
255000070608     c                   eval      *in28 = *on
255100070608     c                   movel     MSG(34)       V1Cmsg
255200070608     C                   eval      *IN47 = *on
255300070608     c                   leave
255400070608    4c                   endif
255500070608
255600070608    3c                   endif
255700070608      *
255800070608      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
255900070608      *
256000070608     c                   clear                   fnlr48ds
256100070608     c                   movel     V1anns        d48aas
256200070608     c                   movel     V1lnps        d48lnp
256300141029     c                   movel     V1nrss        d48nrs
256400070608     c                   movel     V1nsps        d48nsp
256500070608     c                   movel     Dutpou        d48fgs
256600070608     c                   If        $fnarb = *on
256700070608     c                   movel     arbcbo        d48cbo
256800070608     c                   else
256900070608     c                   movel     blpcbo        d48cbo
257000070608     c                   endif
257100071018     c                   eval      d48tbo = d66tbo
257200070608     c                   movel     v1ccva        d48cvb
257300070608     c                   movel     'S'           d48f12
257400140429     c                   movel     'V'           d48ffr
257500140429      *
257600140429     c                   clear                   dsARBd
257700111025     c
257800070608     c                   movel     fnlr48ds      kpjbu
257900070608     c                   call      'FNLR48R'
258000070608     c                   parm                    KPJBA
258100070608     c                   parm                    DSARBD
258200070608     c                   parm                    DSARBK
258300070608     c                   parm                    DARBT
258400070608     c                   parm                    DSARBG
258500070608     c                   parm                    TRUL90DS
258600070608      *
258700070608     c                   movel     kpjbu         fnlr48ds
258800070608     C* errore
258900070608     C                   if        d48err = '1'
259000070608     c                   eval      *in28 = *on
259100070608     c                   movel     MSG(33)       V1Cmsg
259200070608     C                   eval      *IN47 = *on
259300070608     c                   leave
259400070608     c                   else
259500070608     c                   clear                   v1ccva
259600070608     c                   clear                   v1dcva
259700070608     c                   endif
259800160209      *
259900160209     c* Se no errore e no F12 --> non permetto l'uscita con F3
260000160209     c                   if        d48err = ' ' and d48f12 <> '1'
260100160209     c                   eval      $BloccoF3 = *on
260200160209     c                   eval      *in63     = $BloccoF3
260300160209     c                   endif
260400070608     C*
260500070608    2C                   endif
260501170221      *
260502170221      * F14 Gestione disopsizini di giacenza
260503170221      *
260504170512    2c                   If        *inkn = *on
260506170221     c                   exsr      sr_Ctrgiacenza
260507170221      * controllo se la giacenza è ancora gestibile altrimenti errore
260508170221     c                   If        *in04 = *off
260509170221     c                   eval      *in28 = *on
260510170526      * emetto il messaggio di errore differente se dato f14 con motivo di apertura
260511170526      * diverso da " 12" o se la giacenza non è più nelle condizioni di essere
260512170526      * gestita (esempio stato <> 20)
260513170526     c                   if        v1cmad <> W_gia
260514170526     c                   movel     Msg(64)       V1cmsg
260515170526     C                   eval      *IN36 = *on
260516170526     c                   else
260517170222     c                   if        o071esito <> ' ' and o071msg <> ' '
260518170222     c                   eval      V1cmsg = o071msg
260520170222     c                   else
260528170221     c                   movel     MSG(28)       V1Cmsg
260530170526     c                   endif
260531170526     C                   eval      *IN33 = *on
260532170222     c                   endif
260533170221     c                   leave
260534170221     c                   endif
260535170414      * verifico che non sia stato già chiamato
260536170414     c                   setoff                                       75
260537170414     c     'FNLG10R   '  lookup    £Stack                                 75
260538170512     c                   if        %found
260539170512     c                   movel     MSG(63)       V1Cmsg
260540170512     C                   eval      *IN28 = *on
260541170512     C                   eval      *IN33 = *on
260542170512     c                   leave
260543170512     c                   endif
260544170505
260545170505     c                   clear                   note_gia
260546170505
260547170221      * se non c'è errore richiamo il FNLG10R
260548170509     c***                call(e)   'QCMDEXC'
260549170509     c***                parm      C_StrCmtCtl   Qcmd1
260550170509     c***                parm                    Qlen1
260551170509     c***                if        NOT %error
260552170509     c***                eval      $StrCmtCtl =  *on
260553170509     c****               endif
260554170221      * prima richiamo il trul90 per il recupero delle stampanti
260555170221     C                   clear                   trul90ds
260556170221      * non faccio vedere il video della scelta stampanti
260557170221     C                   movel     'X'           D90rse
260558170221     C                   movel     'X'           D90rsb
260559170314     c                   movel     i071lna       par_key           3
260560170221     C                   call      'TRUL90R'
260561170221     C                   parm                    kpjba
260562170221     C                   parm                    trul90ds
260563170314     C                   parm                    par_key
260564170221      * preparo la ds per il richiamo delle disposizioni di giacenza
260565170413     c                   clear                   fnlg82ds
260566170503     c                   eval      d82ges = UteGia
260567170221     c                   eval      d82gia = 'S'
260568170221     C                   eval      d82MOB  =   d90MdB
260569170221     C                   eval      d82PRB  =   d90PRB
260570170221     C                   eval      d82MOE  =   d90MdE
260571170221     C                   eval      d82PRE  =   d90PRE
260572170221     C                   eval      d82PSS  =   d90PSS
260573170221     C                   eval      d82MDB4 =   d90MDB4
260574170221     C                   eval      d82MDB5 =   d90MDB5
260575170221     C                   eval      d82BSL  =   d90BSL
260576170221     C                   eval      d82PSL  =   d90PSL
260577170221     C                   eval      d82PRB4 =   d90PRB4
260578170221     C                   eval      d82PRB5 =   d90PRB5
260579170221      * giacenza
260580170221     c                   movel     O071fgc       d82fgc
260581170221     c                   movel     O071dap       d82agc
260582170221     c                   movel     O071ngc       d82ngc
260583170221
260584170221     c                   eval      d82comm =   'S'
260585170221     c                   movel     dutpou        d82fgs
260586170221
260587170221     c                   movel(p)  fnlg82ds      kpjbu
260588170509     c******             call      'FNLG10R'
260589170516     c                   call      'FNLG10C'
260590170221     c                   parm                    Kpjba
260591170406     c                   movel(p)  kpjbu         fnlg82ds
260592170406
260593170221
260594170221      * - Chiudo il commit control se è stato avviato da questo pgm.
260595170509     c****               if        $StrCmtCtl =  *on
260596170509     c****               call      'QCMDEXC'
260597170509     c****               parm      C_EndCmtCtl   Qcmd1
260598170509     c****               parm                    Qlen1
260599170509     c****               eval      $StrCmtCtl =  *off
260600170509     c****               endif
260601170222
260602170222      * - se il campo D82F12 è blank vuol dire che ho immesso una disposizione ???
260603170222      *   quindi spengo l'indicatore che condiziona F14 Dispo Giacenza e blocco F3
260604170222     c                   If        D82f12 <>'1'
260605170222     c                   setoff                                       04
260606170222     c                   eval      $BloccoF3 = *on
260607170222     c                   eval      *in63     = $BloccoF3
260608170508     c                   eval      note_gia = C_ImmDisp + ' ' + d82fgc + '/'  +
260609170508     c                                        d82ngc +   ' del ' +
260610170505     c                                         d82agc
260611170222     c                   endif
260612170222      *
260613170414      *
260614170414     c                   Endif
260615160207      *
260700160207      * se arrivo qua vuol dire che non ho avuto errori su
260800160207      * nessun campo, quindi ripristino l'indicatore 48
260900160207      * per posizionare (forse) il cursore sulle note.
261000160207     c                   eval      *in48 = sav_in48
261100160205      *
261200160205      /free
261300160205
261400160205         // -?Immissione R.A. non chiusa: emissione W04?
261500160205         IF  wMod = 'I'  and  V1Ccdt = *blank  and  Not $GesW04;
261600160205
261700160205           exsr  sr_GesW04;
261800160205
261900160205           // -?F12 = Ritorno?
262000160205           if  *inKL;
262100160205             *in90 = *on;
262200160205           endif;
262300160205           if  *in90;
262400160205             leave;
262500160205           endif;
262600160205
262700160205         ENDIF;
262800160205
262900160205      /end-free
263000160205      *
263100070330      * se è andato tutto ok ed è stato dato F6  confermo richiamo
263200070330    2c                   If        *inkf and *in61 and v1cdcr > 0
263300070330     c                   clear                   fnlr48ds
263400070330    3c                   if        arbtc1 = 'S' or arbtc2 = 'S'
263500070330     c                   eval      d48cvb='CS'
263600070330     c                   eval      d48ffr='V'
263700070330     c                   eval      d48f12='S'
263800070330     c                   else
263900070330     c                   eval      d48cvb='CR'
264000070330     c                   eval      d48ffr='E'
264100070330     c                   eval      d48err='Z'
264200070330    3c                   endif
264300070330     c                   z-add     v1anns        D48AAS
264400070330     c                   z-add     v1lnps        D48LNP
264500070330     c                   z-add     v1nrss        D48NRS
264600070330     c                   z-add     v1nsps        D48NSP
264700070330     c                   MOVEL     ARBCBO        D48CBO
264800070330     c                   MOVEL     'A'           D48TBO
264900070330     c                   MOVEL     'S'           D48F12
265000070330     c                   MOVEL     'G'           D48trc
265100070330     c                   MOVEL     fnlr48ds      KPJBU
265200140429     c                   clear                   dsARBd
265300070330     c                   movel     v1ctcr        §BGTCR
265400141029     c                   z-add     v1cocr        §BGHCR
265500070330     c                   z-add     wdcr          §BGDCR
265600070330     c                   movel     arbgc1        §BGGC1
265700070330     c                   movel     arbgc2        §BGGC2
265800070330     c                   movel     arbtc1        §BGTC1
265900070330     c                   movel     arbtc2        §BGTC2
266000070330     c                   call      'FNLR48R'
266100070330     c                   PARM                    KPJBA
266200070330     c                   PARM                    DSARBD
266300070330     c                   PARM                    DSARBK
266400070330     c                   PARM                    DARBT
266500070330     c                   PARM                    DSARBG
266600070330     c                   PARM                    trul90ds
266700070330     c                   MOVEL     KPJBU         fnlr48ds
266800070330      * Ritorno con errore o premuto f12
266900070330    3c                   if        d48err<>*blanks and d48err <> 'Z'
267000160207     c                   eval      *in48 = *off
267100070330     c                   eval      *IN28 = *on
267200070330     c                   eval      *IN62 = *on
267300070330     c                   movel     d48msg        v1cmsg
267400070330     c                   leave
267500070330    3c                   endif
267600070330      * se premuto f12 nulla
267700070330    3c                   if        d48f12='1'
267800070330     c                   eval      *IN90 = *on
267900070330     c                   leave
268000070330    3c                   endif
268100070330
268200070330      * preparo il campo fisso per le modifiche effettuate
268300070330     c                   If        d48err = 'Z' and d48msg <> *blanks
268400070330     c                   eval      not_dcr = 'S'
268500070330     c                   movel     d48msg        note_dcr
268600070330      * preparo un campo note
268700070330     c                   endif
268800160209      * Se no errore e no F12 --> non permetto l'uscita con F3
268900160209     c                   if        D48err = ' ' and D48f12 <> '1'
269000160209     c                   eval      $BloccoF3 = *on
269100160209     c                   eval      *in63     = $BloccoF3
269200160209     c                   endif
269300070330      * valorizzo il flag che mi guida all'inserimento delle note ldv
269400070330     c                   eval      not_ldv = 'S'
269500070330     c                   seton                                        60
269600070330    2c                   endif
269700140429      *
269800140429      /free
269900150421
270000160204         // -?Conferma modifica R.A. non chiusa con altra Filiale Gestione:?
270100160204         //  ?· emissione W04N?
270200160204         //  ?· note obbligatorie SE richiesta Risposta?
270300160204         clear  $GesW04N;
270400160204         clear  $Video;
270500160204
270600160205         IF  *inKF   =  *on    and
270700160205             wMod    <> 'I'    and  V1Ccdt =  *blank  and
270800160205             ( dTGDfar.§TGDatt = *blank   or  dTGDfar.§TGDris = 'S' )
270900160205                               and  (
271000160205             ( w_POt >  *zero  and  V1Cpog <> %editc( w_POt  : 'X' ) )  or
271100160205             ( w_POt <= *zero  and  V1Cpog <> %editc( DUTpou : 'X' ) )  or
271200160205             V1Crpg  <> KNMUS       );
271300160205
271400160205           DoW  Not  $GesW04N;
271500160205
271600160204             exsr  sr_GesW04N;
271700160205
271800160204             // -?F12 = Ritorno?
271900160204             if  *inKL;
272000160204               *in90 = *on;
272100160204             endif;
272200160204             if  *in90;
272300160204               leave;
272400160204             endif;
272500160205
272600160204             *inKF = *on;
272700160205
272800160205           EndDo;
272900160205
273000160205         ENDIF;
273100140429
273200140429         // -?Conferma R.A. con Motivo Apertura " 17"?
273300140429         //  ?(Fermo Deposito autorizza mittente)?
273400140430         //  ?- SE non si è in Sede ma si è in Arrivo -?
273500140506         //  ?- SE non si è in modifica (*in50/70/72/74) -?
273600140430         //  ?=> Variazione Bolle (solo Arrivo)?
273700140506         IF  *inKF  and  $FNARB  and  V1Cmad = c_FDEAM
273800140516                    and  Not *in50
273900140516                    and  Not *in70  and  Not *in72  and  Not *in74;
274000140506
274100140506           $RA_MA_17 = *on;
274200140429
274300140429           clear  dsARBd;
274400140429           clear  dsARBk;
274500140429           clear  dARBt;
274600140429           clear  dsARBg;
274700140429           clear  FNLR48ds;
274800140429
274900140429           D48aas = V1annS;
275000140429           D48lnp = V1lnpS;
275100140429           D48nrs = V1nrsS;
275200140429           D48nsp = V1nspS;
275300140430           D48cbo = ARBcbo;
275400140429           D48cvb = c_CauVarB_I7;
275500140429           D48trc = 'D';
275600140429           D48ffr = 'E';
275700140430           D48tbo = 'A';
275800140429
275900140430           §BDrsd = ARBrsd;
276000140430           §BDind = ARBind;
276100140430           §BDcad = ARBcad;
276200140430           §BDlod = ARBlod;
276300140430           §BDprd = ARBprd;
276400140430           §BDnzd = ARBnzd;
276500140430           §BDfin = ARBfin;
276600140506           §BDffd = 'S';
276700140430           §BDcpi = ARBcpi;
276800140506
276900140506           // -?Reperimento ragione sociale estesa del destinatario?
277000140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'D' )  FIAR4000;
277100140506           if  %found(FIAR401L);
277200140506             §BDrd2 = AR4not;
277300140506           endif;
277400140506
277500140506           // -?Reperimento referente e telefono del destinatario?
277600140506           clear  dAR5gen;
277700140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'GEN' )  FIAR5000;
277800140506           if  %found(FIAR501L)  and  AR5atb = *blank;
277900140506             dAR5gen = AR5uni;
278000140506             §BDref = §AR5ref;
278100140506             §BDtel = §AR5teld;
278200140506           endif;
278300140429
278400140429           kpjbu = FNLR48ds;
278500140429           fnlr48r ( kpjba :
278600140429                     dsARBd :
278700140429                     dsARBk :
278800140429                     dARBt :
278900140429                     dsARBg :
279000140429                     TRUL90ds );
279100140429           FNLR48ds = kpjbu;
279200140429
279300140429           select;
279400140429             //?- se rilevato errore => nulla?
279500140429             when  D48err <> *blank;
279600140429               *in28  = *on;
279700140429               *in62  = *on;
279800140429               V1Cmsg = D48msg;
279900160207               leave;
280000140429             //?- se premuto F12 => nulla?
280100160209             //?  (F12 non abilitato)?
280200160209             //when  D48f12 = *on;
280300160209             //  *in90  = *on;
280400160209             //  leave;
280500160209             //?- se NON premuto F12 => disabilitazione F3?
280600160209             when  D48err = *blank  and  D48f12 <> *on;
280700160209               $BloccoF3  = *on;
280800160209               *in63      = $BloccoF3;
280900140429           endsl;
281000140429
281100140429         ENDIF;
281200141210
281300141210         // -?Conferma R.A. con Motivo Apertura " 60"?
281400141210         //  ?(Verifica telefonica "PINcode")?
281500141210         //  ?- SE non si è in Sede ma si è in Arrivo -?
281600141210         //  ?- SE non si è in modifica (*in50/70/72/74) -?
281700141210         //  ?=> Richiesta BRTcode + PINcode?
281800141210         //  ?=> DOPO: Registrazione Esito nelle Note?
281900141210         IF  *inKF  and  $FNARB  and  V1Cmad = c_VPINC
282000141210                    and  Not *in50
282100141210                    and  Not *in70  and  Not *in72  and  Not *in74;
282200141210
282300141210           // -?Richiesta / Controllo "PINcode"?
282400141210           clear  trulPINds;
282500141210           iPINaas = V1annS;
282600141210           iPINlnp = V1lnpS;
282700141210           iPINnrs = V1nrsS;
282800141210           iPINnsp = V1nspS;
282900141210
283000141210           trulPINr ( trulPINds );
283100141212
283200141216           if  oPINfxx = '03'  and  oPINnre = *zero;
283300141216             *inKF = *off;
283400141216             *in90 = *on;
283500141216             exsr  sr_PulCtl2;
283600141216           else;
283700141216             $RA_MA_60 = *on;
283800141216             // -?N.B. - L'esito verrà registrato DOPO nelle NOTE?
283900141216           endif;
284000141210
284100141210         ENDIF;
284200150114
284300150114
284400150114         //?Posizionamento curosre sulle Note - SE Inserim. & NO errori?
284500150114         if  wMod = 'I'        and
284600150114             Not *in28         and  Not *in90                    and
284700150114             ( ( V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero )    or
284800150324               ( V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero ) );
284900150114           *in48 = *on;
285000150114         endif;
285100140429
285200140429       /end-free
285300040422      *
285400040217     c                   leave
285500040212      *
285600040211    1C                   endDO
285700040211      *
285800040211     c                   ENDSR
285900150112      *
286000150112      *---------------------------------------------------------------*
286100150114      *?Routine di Impostazione automatica di alcuni campi a video   ?*
286200150114      *?  - SE INSERIMENTO -                                         ?*
286300150112      *---------------------------------------------------------------*
286400150114     c     sr_ImpAut_Ins BEGSR
286500150112      *
286600150113sel 1c                   Select
286700150113      *
286800150112      * -?Impostazione campi (se vuoti) per Spedizione?
286900150113w   1c                   When      V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero
287000150112      * - -?Chi Chiama => Destinatario?
287100150112if  4c                   if        V1Ctch = *blank
287200150112     c                   eval      V1Ctch = 'D'
287300150112     c                   exsr      sr_Ctrl_CCR
287400150112e   4c                   endif
287500150202      * - -?Motivo Apertura => Consegna +?
287600150112if  4c                   if        V1Cmad = *blank
287700150112     c                   eval      V1Cmad = '  1'
287800150202     c                   clear                   wSaveMAD
287900150112e   4c                   endif
288000150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
288100150202if  4c                   if        V1Cmad <> wSaveMAD
288200150202     c                   eval      wSaveMAD = V1Cmad
288300150202     c                   exsr      sr_Ctrl_MAR
288400150202     c                   eval      V1Ccdt   = §MARcauChP
288500150202e   4c                   endif
288600150114      * - -?Posizionamento cursore sulle Note?
288700150114     c                   eval      *in48 = *on
288800150112      *
288900150112      * -?Impostazione campi (se vuoti) per O.R.M.?
289000150113w   1c                   When      V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero
289100150112      * - -?Chi Chiama => Mittente?
289200150112if  4c                   if        V1Ctch = *blank
289300150113     c                   eval      V1Ctch = 'M'
289400150112     c                   exsr      sr_Ctrl_CCR
289500150112e   4c                   endif
289600150112      * - -?Motivo Apertura => Ritiro?
289700150112if  4c                   if        V1Cmad = *blank
289800150112     c                   eval      V1Cmad = '  3'
289900150202     c                   clear                   wSaveMAD
290000150112e   4c                   endif
290100150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
290200150202if  4c                   if        V1Cmad <> wSaveMAD
290300150202     c                   eval      wSaveMAD = V1Cmad
290400150202     c                   exsr      sr_Ctrl_MAR
290500150202     c                   eval      V1Ccdt   = §MARcauChP
290600150202e   4c                   endif
290700150114      * - -?Posizionamento cursore sulle Note?
290800150114     c                   eval      *in48 = *on
290900150407      *
291000150407      * -?Impostazione campi (se vuoti) per Generico?
291100150407w   1c                   When      V1Ctor = 'G'
291200150407      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
291300150407if  4c                   if        V1Cmad <> wSaveMAD
291400150407     c                   eval      wSaveMAD = V1Cmad
291500150407     c                   exsr      sr_Ctrl_MAR
291600150407     c                   eval      V1Ccdt   = §MARcauChP
291700150407e   4c                   endif
291800150112      *
291900150113e   1c                   EndSl
292000150313
292100150625      * - -?STATO (ex Priorità):?
292200150625      *    ?Se viene impostata la causale chiusura metto priorità "9"?
292300150625      *    ?(in inserimento) o "8" (in modifica - SE modificata la?
292400150625      *    ?Causale Chiusura)?
292500150313     c                   IF        V1Ccdt <> *blanks
292600150402     c                   IF        wmod = 'I'
292700150625      *   -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
292800150625      *    ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
292900150625      *    ?(già reperita nella subr. SR_CONTR01)?
293000150625     c                   if        §CORstato <> *blanks
293100150625     c                   eval      V1Cprt = §CORstato
293200150625     c                   else
293300150313     c                   eval      V1Cprt = '9'
293400150625     c                   endif
293500150324     c                   ELSE
293600150324     c                   eval      V1Cprt = '8'
293700150324     c                   ENDIF
293800150313     c                   ENDIF
293900150320      *
294000150320      * Decodifico (potrebbe essere cambiato)
294100150320     c                   clear                   tibs02ds
294200150320     C                   MOVEL     'C'           t02mod
294300150320     C                   MOVEL     knsif         t02sif
294400150320     C                   MOVEL     'PRR'         t02cod
294500150320     C                   MOVEL     v1cprt        t02ke1
294600150320      *
294700150320     C                   CALL      'TIBS02R'
294800150320     C                   PARM                    KPJBA
294900150320     C                   PARM                    TIBS02DS
295000150320      *
295100150320    4C                   IF        t02err = *BLANKS
295200150320     C                   movel     T02uni        DPRR
295300150320     C                   movel     §PRRdesc      v1dprt
295400150320     c                   endif
295500150112      *
295600150112     c                   ENDSR
295700150112      *
295800150112      *---------------------------------------------------------------*
295900150112      * Routine controllo tabella "MAR" = Motivo Apertura             *
296000150112      *---------------------------------------------------------------*
296100150112     c     sr_Ctrl_MAR   BEGSR
296200150323
296300150323      * Ricerca
296400150323    3c                   if        %scan('?':V1Cmad) > 0
296500150422     c                             and       $Video <> 'W4'
296600150323     c                   clear                   tntbMARds1
296700150323      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
296800150323      *      dall'utente in APERTURA;
296900150323      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
297000150323      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
297100150323      *      in GESTIONE.
297200150323     c                   if        wMod <> 'I'
297300150323     c                   eval      iMARutUt = 'I'
297400150323     c                   endif
297500150323     c                   eval      iMARutAp = 'I'
297600150323     c                   movel(p)  tntbMARds1    KPJBU
297700150323      *
297800150323     c                   call      'TNTBMARR1'
297900150323     c                   parm                    KPJBA
298000150323     c                   movel     KPJBU         tntbMARds1
298100150323    4c                   if        oMARfxx = *blank  and  oMARerr = *off
298200150323     c                   eval      V1Cmad  = oMARcod
298300150323     C                   eval      V1Dmad  = oMARdes
298400150323      *  errore (non trovato nulla pulisco il '?' )
298500150323     C                   ELSE
298600150323     c                   clear                   V1Cmad
298700150323     c                   clear                   V1Dmad
298800150323    4C                   ENDIF
298900150422    3C                   ENDIF
299000150112      *
299100150112     c                   clear                   tibs02ds
299200150112     C                   MOVEL     'C'           t02mod
299300150112     C                   MOVEL     knsif         t02sif
299400150112     C                   MOVEL     'MAR'         t02cod
299500150422     c                   if        $Video = 'W4'
299600150422     c                   eval      T02ke1 = wwcMAD
299700150422     c                   else
299800150112     C                   MOVEL     v1cmad        t02ke1
299900150422     c                   endif
300000150112      *
300100150112     C                   CALL      'TIBS02R'
300200150112     C                   PARM                    KPJBA
300300150112     C                   PARM                    TIBS02DS
300400150112      *
300500150112    4C                   IF        t02err = *BLANKS
300600150112     C                   movel     T02uni        DMAR
300700150422     c                   if        $Video = 'W4'
300800150422     c                   eval      wwdMAD = §MARdesc
300900150422     c                   else
301000150112     C                   movel     §MARdesc      v1dmad
301100150423     c                   eval      wwdMAD = §MARdesc
301200150422     c                   endif
301300150112     c                   if        (*in74 = *off  and  §MARutAp = 'N')  or
301400150112     c                             (wMod <> 'I'   and  §MARutUt = 'N')
301500150112     c                   eval      *in28 = *on
301600150112     c                   eval      *in36 = *on
301700150112     c                   movel     msg(18)       V1Cmsg
301800150112     c                   leavesr
301900150112     c                   endif
302000150112      *
302100150112     C                   ELSE
302200150112     c                   eval      *in28 = *on
302300150112     c                   movel     msg(18)       V1Cmsg
302400150112     C                   eval      *IN36 = *on
302500150112     c                   leavesr
302600150112    4C                   ENDIF
302700150112      *
302800150112     c                   ENDSR
302900150112      *
303000150112      *---------------------------------------------------------------*
303100150112      * Routine controllo tabella "CCR" = Chi Chiama                  *
303200150112      *---------------------------------------------------------------*
303300150112     c     sr_Ctrl_CCR   BEGSR
303400150112      *
303500150112     c                   clear                   tibs02ds
303600150112     C                   MOVEL     'C'           t02mod
303700150112     C                   MOVEL     knsif         t02sif
303800150112     C                   MOVEL     'CCR'         t02cod
303900150112     C                   MOVEL     v1ctch        t02ke1
304000150112      *
304100150112     C                   CALL      'TIBS02R'
304200150112     C                   PARM                    KPJBA
304300150112     C                   PARM                    TIBS02DS
304400150112      *
304500150112    4C                   IF        t02err = *BLANKS
304600150112     C                   movel     T02uni        DCCR
304700150112     C                   movel     T02ke1        v1ctch
304800150112     c                   movel     §ccrdesc      v1dtch
304900150112      *
305000150112     C                   ELSE
305100150112     C                   eval      *IN28 = *on
305200150112     c                   movel     msg(07)       V1Cmsg
305300150112     C                   eval      *IN35 = *on
305400150112     c                   leavesr
305500150112    4C                   ENDIF
305600150112      *
305700150112     c                   ENDSR
305701170220      *---------------------------------------------------------------*
305702170220      *?Routine di Controllo spedizione legata a Giacenza            ?*
305703170220      *?  - SE INSERIMENTO -                                         ?*
305704170220      *---------------------------------------------------------------*
305705170220     c     sr_CtrGiacenzaBEGSR
305706170524      *
305707170524      * Verifico se motivo di apertura uguale a giacenza altrimenti spengo lo 04
305708170524      *
305709170524     c                   if        v1cmad <> W_gia
305710170524     c                   eval      *in04 = *off
305711170524     c                   else
305712170220      *
305713170515      * Verifico se utente autorizzato alla giacenza o come linea di partenza o come
305714170503      * linea di arrivo.
305715170515      * Se autorizzato verifico lo stato della giacenza
305716170503      * Memorizzo anche se posso operare sulla giacenza come Partenza (priorità) o Arrivo
305717170518
305718170519      * la bolla non legata  controllo abilitazione alla spedizione in base
305719170519      * alla sua lnp e lna
305720170519     c                   eval      lnp_abil = arblnp
305721170519     c                   eval      lna_abil = arblna
305722170519      *
305723170518      * aggancio il file legami bolla per determinare se ci sono dei legami e
305724170519      * in tal caso impostare la lnp (mamma) e lna (ultima figlia)
305725170518     C                   movel     *blanks       wEsito1           1
305726170518     C                   movel     *blanks       wEsito2           1
305727170519     C*
305728170519     C                   eval      wAAS = V1annS
305729170519     C                   eval      wLNP = V1lnps
305730170519     C                   eval      wNRS = V1nrss
305731170519     C                   eval      wNSP = V1nsps
305732170519     C*
305733170518     C* Chiamata metodo GetLblTyp
305734170518     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
305735170519     C                                                wAAS
305736170519     C                                               :wLNP
305737170519     C                                               :wNRS
305738170519     C                                               :wNSP
305739170518     C                                               :pOutLblTyp
305740170518     C                                               :pOutAnnoBO
305741170518     C                                               :pOutLineaParBO
305742170518     C                                               :pOutSerieBO
305743170518     C                                               :pOutNumSpedBO
305744170518     C                                               :pOutDcmBO
305745170518     C                                               :pOutCcaBO
305746170518     C                                               :pOutRblBO))
305747170519      * se bolla legata verifico se sono la bolla originale
305748170519     c                   If        wesito1 = '0'
305749170519
305750170518     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
305751170518     C                   if        pOutLblTyp <> 'O'
305752170518     C                   eval      wAAS = pOutAnnoBO
305753170518     C                   eval      wLNP = pOutLineaParBO
305754170518     C                   eval      wNRS = pOutSerieBO
305755170518     C                   eval      wNSP = pOutNumSpedBO
305756170518     C                   endif
305757170518     C*
305758170518     C* Chiamata metodo GetLastChild
305759170518     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
305760170518     C                                                wAAS
305761170518     C                                               :wLNP
305762170518     C                                               :wNRS
305763170518     C                                               :wNSP
305764170518     C                                               :pOutAnnoFI
305765170518     C                                               :pOutLineaParFI
305766170518     C                                               :pOutSerieFI
305767170518     C                                               :pOutNumSpedFI
305768170518     C                                               :pOutDcmFI
305769170518     C                                               :pOutCcaFI))
305770170519      * se bolla figlia trovata recupero le linee di arrivo dell'ultima figlia e partenza dalla
305771170519      * spedizione originale
305772170519      /free
305773170519         If  Wesito2 = '0' ;
305774170519             chain (poutannofi:poutlineaparfi:poutseriefi:poutNumspedfi) FNLBL1;
305775170519             if  %found( FNLBL01L );
305776170519                 lnp_abil = lbllpo ;
305777170519                 lna_abil = lbllan ;
305778170519             Endif ;
305779170519         Endif ;
305780170519      /end-free
305781170519
305782170519     c                   Endif
305783170519
305784170503     c                   clear                   utegia
305785170515     c                   eval      *in04 = *off
305786170515
305787170503      *
305788170519     c                   move      lnp_abil      w003a
305789170505     c     w003a         lookup    wpog                                   27
305790170503     c                   if        %found
305791170503     c                   eval      Utegia= 'P'
305792170503     c                   else
305793170519     c                   move      lna_abil      w003a
305794170505     c     w003a         lookup    wpog                                   27
305795170503     c                   if        %found
305796170503     c                   eval      Utegia= 'A'
305797170503     c                   endif
305798170503     c                   endif
305799170503      * se non è possibile gestire la spedizione non ricerco la giacenza
305800170515    2c                   If        Utegia <> ' '
305801170503
305802170220     c                   clear                   FNLG071DS
305803170220     c                   eval      I071CHIA = 'C'
305804170220     c                   eval      I071AAS = v1anns
305805170220     c                   eval      I071LNP = v1lnps
305806170220     c                   eval      I071NRS = v1nrss
305807170220     c                   eval      I071NSP = v1nsps
305808170606     c                   eval      I071LNA = lna_abil
305809170503     c                   eval      I071CHI = UteGia
305810170220     c                   call      'FNLG07R1'
305811170220     c                   parm                    kpjba
305812170220     c                   parm                    FNLG071DS
305813170220      * verifico l'Esito alla chiamata del FNLG071
305814170220     c                   if        O071esito <> ' '
305815170220     c                             or O071ngc = 0
305816170220     c                   setoff                                       04
305817170220     c                   endif
305818170220     c                   if        O071esito = ' '
305819170220     c                             and O071ngc <> 0
305820170220     c                   seton                                        04
305821170503     c                   endif
305822170503
305823170515    2c                   endif
305824170524
305825170524    2c                   endif
305826170524
305827170220     c                   ENDSR
305828070323      *
305900140429      /free
306000150421
306100150421       //--------------------------------------------------------------
306200150421       //?Gestione window W4 per immissione R.A. non chiusa            ?
306300150421       //--------------------------------------------------------------
306400150421       BEGSR  sr_GesW04;
306500150421
306600150423         $Video   = 'W4';
306700150421
306800150421         //clear  FIA1W04;  // -?NO: serve sapere se sono già stati?
306900150421                            //  ?    inseriti dati?
307000160120
307100160204         // -?Impostazione flag "Attesa Risposta"?
307200160120         $AttesaRisposta = *off;
307300150616
307400150915         // -?Impostazione gestione Reparto?
307500150915         $Reparto = ( V1Cpog <> c_Sede  or  V1Cmad = c_ReclSede );
307600150421
307700150421         W4nTel = V1nTel;
307800150421         W4mail = V1mail;
307900150421         W4cMad = V1cMad;
308000150915         clear  W4cRpr;
308100150915         clear  W4cPog;
308200151211         clear  W4cCap;
308300160607         // -?Reparto/Utente gestione?
308400150720         select;
308500150915           // -?Inserimento Reclamo da Sede (in Sede, motivo " 70"):?
308600150915           //  ?proposto reparto?
308700150720           when  *in05  and  W4cMad = c_ReclSede;
308800150915             W4cRpr = 'RAC';
308900150720           // -?Proposto Utente, non Reparto (in Sede)?
309000150720           when  Not $Reparto;
309100150720             W4cRpr = KNMUS;
309200150720           // -?Proposto Reparto (in Filiale)?
309300150720           other;
309400150720             W4cRpr = 'RAC';
309500150720         endsl;
309600160607         // -?Filiale gestione?
309700150720         if  Not *in05  or  W4cMad <> c_ReclSede;
309800160607           select;
309900160607             // -?LNA della Spedizione?
310000160607             when  V1Ctor = 'S';
310100160607               if  $FNARB;
310200160607                 W4cPog = %editc( ARBlna : 'X' );
310300160607               else;
310400160607                 W4cPog = %editc( BLPlna : 'X' );
310500160607               endif;
310600160607             // -?Filiale Ritiro dell'O.R.M.?
310700160607             when  V1Ctor = 'O';
310800160607               W4cPog = %editc( ORMpor : 'X' );
310900160607             // -?Filiale Utente altrimenti?
311000160607             other;
311100160607               W4cPog = V1Cpog;
311200160607           endsl;
311300150720         endif;
311400160128         // -?Attesa Risposta?
311500151223         Select;
311600151223           When  *in05;
311700151223             W4tgdATT = 'N';
311800151223           When  V1CfilR > *zero  and  V1CrepR <> *blank;
311900151223             W4tgdATT = 'S';
312000151223             W4repR = V1CrepR;
312100151223             W4filR = V1CfilR;
312200151223           Other;
312300151223             W4tgdATT = 'N';
312400151223             W4repR = 'RAC';
312500160129             if  w_POt = *zero;
312600160129               W4filR = %editc( DUTpou : 'X' );
312700160129             else;
312800160129               W4filR = %editc( w_POt : 'X' );
312900160129             endif;
313000151223         EndSl;
313100150428
313200150428         // -?Decodifiche dati immessi o proposti?
313300150428         wwcMad = W4cMad;
313400150428         exsr  sr_CtrlMAD;
313500150428         W4cMad = wwcMad;
313600150428         W4dMad = wwdMad;
313700150428         wwcRPR = W4cRpr;
313800150616         if  Not $Reparto;
313900150616           wwcPog = W4cPog;
314000150616           exsr  sr_CtrlRPG;
314100150616         else;
314200150616           exsr  sr_CtrlRPR;
314300150616         endif;
314400150428         W4cRpr = wwcRPR;
314500150428         W4dRpr = wwdRPR;
314600150717         //wwcMad = W4cMad;   ?(già impostata prima)?
314700150428         wwcPog = W4cPog;
314800150428         exsr  sr_CtrlPOG;
314900150428         W4cPog = wwcPog;
315000150428         W4dPog = wwdPog;
315100151211         wwcRPR  = W4repR;
315200151211         exsr  sr_CtrlRPR;
315300151211         W4repR  = wwcRPR;
315400151211         W4repRd = wwdRPR;
315500151211         wwcPog  = W4filR;
315600151211         exsr  sr_CtrlPOG;
315700151211         W4filR  = wwcPog;
315800151211         W4filRd = wwdPog;
315900150428
316000150428         *in28 = *off;
316100150428         *in90 = *off;
316200150428         *in36 = *on;     // -?Posiz. cursore su Motivo?
316300150428         *in38 = *off;    // -?Posiz. cursore su Mail?
316400150428         *in45 = *off;    // -?Posiz. cursore su Reparto Gestione?
316500150428         *in46 = *off;    // -?Posiz. cursore su Filiale Gestione?
316600150428         *in55 = *off;    // -?Posiz. cursore su Telefono?
316700150911         *in56 = *off;    // -?Posiz. cursore su C.A.P. cliente?
316800151211         *in57 = *off;    // -?Posiz. cursore su Reparto Risposta?
316900151211         *in58 = *off;    // -?Posiz. cursore su Filiale Risposta?
317000150422
317100150423         DoU  *in28 = *off  and  *in90 = *off  and  *inKF;
317200150423
317300150423           exfmt  FIA1W04;
317400150423
317500150914           clear  V1Cmsg;
317600150423           *in28 = *off;
317700150423           *in90 = *off;
317800150423           *in36 = *off;       // -?Motivo errato?
317900150423           *in38 = *off;       // -?Mail   errata?
318000150423           *in45 = *off;       // -?Reparto Gestione errato?
318100150423           *in46 = *off;       // -?Filiale Gestione errata?
318200150423           *in55 = *off;       // -?Telefono obbligatorio?
318300150911           *in56 = *off;       // -?C.A.P. cliente   errato?
318400151211           *in57 = *off;       // -?Reparto Risposta errato?
318500151211           *in58 = *off;       // -?Filiale Risposta errata?
318600150423
318700150423           // -?F12 = Ritorno al fmt D01?
318800150423           if  *inKL;
318900150423             clear  $Video;
319000150423             leavesr;
319100150423           endif;
319200150423
319300150423           // -?Num. Telefono o Mail obbligatori?
319400150423           if  W4nTel = *blank  and  W4mail = *blank;
319500150723             *in28  = *on;
319600150423             *in55  = *on;
319700150423             V1Cmsg = Msg(49);
319800150423             iter;
319900150423           endif;
320000150423
320100150423           // -?Mail errata?
320200150423           if  W4mail <> *blank;
320300150423             wwMail = W4mail;
320400150423             exsr  sr_CtrlMail;
320500150423             W4mail = wwMail;
320600150723             if  *in28  or  *in90;
320700150423               iter;
320800150423             endif;
320900150423           endif;
321000150423
321100150423           // -?Controllo Motivo Apertura Reclamo?
321200150423           wwcMad = W4cMad;
321300150423           exsr  sr_CtrlMAD;
321400150423           W4cMad = wwcMad;
321500150423           W4dMad = wwdMad;
321600150723           if  *in28  or  *in90;
321700150423             iter;
321800150423           endif;
321900150723
322000150723           // -?SE Motivo = " 70" (quindi devo essere in Sede):?
322100150723           //  ?Controllo la Filiale Gestione PRIMA dell'Utente Gestione?
322200150723           If  W4cMad = c_ReclSede;
322300150911             // ·?La Filiale Gestione potrebbe voler essere ricavata?
322400150911             //  ?dal C.A.P. del cliente?
322500150911             if  W4cCap <> *blank;
322600150911               exsr  sr_CtrlCAP;
322700150914               if  *in28;
322800150914                 iter;
322900150914               endif;
323000150911             endif;
323100150723             wwcPog = W4cPog;
323200150723             //wwcMad = W4cMad; ?(già impostata prima)?
323300150723             exsr  sr_CtrlPOG;
323400150723             W4cPog = wwcPog;
323500150723             W4dPog = wwdPog;
323600150915             if  *in28;
323700150723               iter;
323800150723             endif;
323900150723             if  W4cPog = c_Sede;
324000150723               *in28 = *on;
324100150723               *in46 = *on;
324200150723               if  Not *in05;
324300150723                 V1Cmsg = Msg(48);
324400150723               else;
324500150723                 V1Cmsg = Msg(53);
324600150723               endif;
324700150723               iter;
324800150723             endif;
324900150723           EndIf;
325000151202
325100151202           // -?SE modificato Motivo in " 18" (Reso):?
325200151202           //  ?ritorno alla videata pecedente (la R.A. va chiusa)?
325300151202           If  W4cMad = c_MotAp_Reso;
325400151202             *in90 = *on;
325500151202             leave;
325600151202           EndIf;
325700150915
325800150915           // -?Re-impostazione gestione Reparto?
325900150915           $Reparto = ( W4cPog <> c_Sede  or  W4cMad = c_ReclSede );
326000150423
326100150617           // -?Controllo Reparto Gestione (in filiale)?
326200150617           //  ?Controllo Utente  Gestione (in sede)?
326300150429           wwcRpr = W4cRpr;
326400150616           if  Not $Reparto;
326500150616             wwcPog = W4cPog;
326600150616             exsr  sr_CtrlRPG;
326700150616           else;
326800150616             exsr  sr_CtrlRPR;
326900150616           endif;
327000150429           W4cRpr = wwcRpr;
327100150429           W4dRpr = wwdRpr;
327200150915           if  *in28;
327300150429             iter;
327400150429           endif;
327500150423
327600150723           // -?SE Motivo <> " 70":?
327700150723           //  ?Controllo Filiale Gestione   DOPO?
327800150723           //  ?il Reparto Gestione (in filiale)?
327900150723           //  ?o l'Utente  Gestione (in sede)?
328000150723           If  W4cMad <> c_ReclSede;
328100150723             wwcPog = W4cPog;
328200160120             //wwcMad = W4cMad;    ?(già impostata prima)?
328300150723             exsr  sr_CtrlPOG;
328400150723             W4cPog = wwcPog;
328500150723             W4dPog = wwdPog;
328600150915             if  *in28;
328700150723               iter;
328800150723             endif;
328900150723             if  Not *in05  and  W4cPog =  c_Sede;
329000150723               *in28 = *on;
329100150723               *in46 = *on;
329200150723               V1Cmsg = Msg(48);
329300150723               iter;
329400150723             endif;
329500150723           EndIf;
329600151211
329700160120           // -?Controllo Reparto che Attende Risposta?
329800151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
329900160121           //  ?- in Sede NON viene richiesto Reparto/Filiale... -?
330000160121           If  Not *in05  and  W4tgdATT = 'S';
330100151214             if  W4repR = *blank;
330200151214               W4repR   = 'RAC';
330300151214             endif;
330400151223             wwcRpr  = W4repR;
330500151214             exsr  sr_CtrlRPR;
330600151214             W4repR  = wwcRPR;
330700151214             W4repRd = wwdRPR;
330800151223             if  *in28  or  *in90;
330900151223               *in45 = *off;
331000151223               *in57 = *on;
331100151223               if  *in28;
331200151223                 V1Cmsg = Msg(57);
331300151223               endif;
331400151214               iter;
331500151214             endif;
331600151223           Else;
331700160121             W4tgdATT = 'N';
331800151223             clear  W4repR;
331900151223             clear  W4repRd;
332000151214           EndIf;
332100151211
332200160120           // -?Controllo Filiale che Attende Risposta?
332300151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
332400160121           //  ?- in Sede NON viene richiesto Reparto/Filiale... -?
332500151223           If  Not *in05  and  W4tgdATT = 'S';
332600151214             if  W4filR = *blank;
332700160129               if  w_POt = *zero;
332800160129                 W4filR  = %editc( DUTpou : 'X' );
332900160129               else;
333000160129                 W4filR  = %editc( w_POt : 'X' );
333100160129               endif;
333200151214             endif;
333300160121             $AttesaRisposta = *on;
333400160120             //wwcRpr  = W4repR;   ?(già impostata prima)?
333500151214             wwcPog  = W4filR;
333600151214             exsr  sr_CtrlPOG;
333700151214             W4filR  = wwcPog;
333800151214             W4filRd = wwdPog;
333900160121             $AttesaRisposta = *off;
334000151222             if  *in28  or  *in90;
334100151222               *in46 = *off;
334200151222               *in58 = *on;
334300151223               if  *in28;
334400151223                 V1Cmsg = Msg(58);
334500151223               endif;
334600151214               iter;
334700151214             endif;
334800160121             if  W4filR = c_Sede;
334900160121               *in28 = *on;
335000160121               *in58 = *on;
335100160121               V1Cmsg = Msg(48);
335200160121               iter;
335300160121             endif;
335400151223           Else;
335500160121             W4tgdATT = 'N';
335600151223             clear  W4filR;
335700151223             clear  W4filRd;
335800151214           EndIf;
335900151222
336000160128           // -?Confronto tra Reparto/Filiale in Attesa Risposta?
336100151222           //            ?con Reparto/Filiale di Gestione?
336200151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
336300151223           If  Not *in05          and
336400151223               W4tgdATT = 'S'     and
336500151223               W4repR   = W4Crpr  and
336600151223               W4filR   = W4Cpog;
336700151222             *in28 = *on;
336800151222             *in57 = *on;
336900151222             V1Cmsg = Msg(59);
337000151222             iter;
337100151222           endif;
337200150423
337300150423           *in36 = *on;     // -?Posiz. cursore su Motivo?
337400150422
337500150423         EndDo;
337600150421
337700150421         V1nTel = W4nTel;
337800150421         V1mail = W4mail;
337900150421         V1Cmad = W4cMad;
338000151202
338100151202         If  W4cMad <> c_MotAp_Reso;
338200151202
338300151202           V1Crpg = W4cRpr;
338400151202           V1Cpog = W4cPog;
338500151202
338600151202           $GesW04 = *on;
338700151202
338800151202         EndIf;
338900151223
339000151223         V1CfilR = W4filR;
339100151223         V1CrepR = W4repR;
339200151202
339300151202         clear  $Video;
339400150421
339500150421       ENDSR;
339600160128
339700160128       //--------------------------------------------------------------
339800160128       //?Gestione window W4M per nuova Attesa Risposta                ?
339900160128       //--------------------------------------------------------------
340000160128       BEGSR  sr_GesW04N;
340100160128
340200160204         IF  $Video <> 'M4';
340300160204
340400160204           $Video = 'M4';
340500160204           clear  FIA1W04N;
340600160204
340700160204           // -?Attesa Risposta di default?
340800160204           If    dTGDfar.§TGDatt = *blank  or
340900160204               ( dTGDfar.§TGDatt = 'S'    and
341000160204                 dTGDfar.§TGDris = 'S' );
341100160204             W4tgdATT = 'N';
341200160204             W4repR   = 'RAC';
341300160204             if  w_POt = *zero;
341400160204               W4filR   = %editc( DUTpou : 'X' );
341500160204             else;
341600160204               W4filR   = %editc( w_POt : 'X' );
341700160204             endif;
341800160204           Else;
341900160204             W4tgdATT = dTGDfar.§TGDatt;
342000160204             W4repR   = dTGR01A.§TGRrepR;
342100160204             W4filR   = %editc( dTGD01.§TGDfilR : 'X' );
342200160204           EndIf;
342300160204
342400160204           // -?Decodifiche dati immessi o proposti?
342500160204           wwcRPR  = W4repR;
342600160204           exsr  sr_CtrlRPR;
342700160204           W4repR  = wwcRPR;
342800160204           W4repRd = wwdRPR;
342900160204           $AttesaRisposta = *on;
343000160204           wwcPog  = W4filR;
343100160204           exsr  sr_CtrlPOG;
343200160204           W4filR  = wwcPog;
343300160204           W4filRd = wwdPog;
343400160204           $AttesaRisposta = *off;
343500160204
343600160204         ENDIF;
343700160128
343800160128         *in28 = *off;
343900160128         *in90 = *off;
344000160128         *in57 = *off;    // -?Posiz. cursore su Reparto Risposta?
344100160128         *in58 = *off;    // -?Posiz. cursore su Filiale Risposta?
344200160128
344300160128         DoU  *in28 = *off  and  *in90 = *off;
344400160128
344500160128           exfmt  FIA1W04N;
344600160128
344700160128           clear  V1Cmsg;
344800160128           *in28 = *off;
344900160128           *in90 = *off;
345000160128           *in57 = *off;       // -?Reparto Risposta errato?
345100160128           *in58 = *off;       // -?Filiale Risposta errata?
345200160128
345300160128           // -?F12 = Ritorno al fmt D01?
345400160128           if  *inKL;
345500160128             clear  $Video;
345600160207             W4tgdATT = 'N';
345700160128             leavesr;
345800160128           endif;
345900160128
346000160128           // -?NON Richiesta Risposta: uscita?
346100160128           //  ?(con aperta ed in gestione ad altro Rep./Fil.)?
346200160129           If  W4tgdATT = 'N';
346300160204             $GesW04N = *on;
346400160129             clear  W4repRd;
346500160128             clear  W4repR;
346600160129             clear  V1CrepR;
346700160129             clear  W4filRd;
346800160129             W4filR  = *zero;
346900160129             V1CfilR = *zero;
347000160128             clear  $Video;
347100160128             leavesr;
347200160128           EndIf;
347300160128
347400160128           // -?Controllo Reparto che Attende Risposta?
347500160128           if  W4repR = *blank;
347600160128             W4repR   = 'RAC';
347700160128           endif;
347800160128           wwcRpr  = W4repR;
347900160128           exsr  sr_CtrlRPR;
348000160128           W4repR  = wwcRPR;
348100160128           W4repRd = wwdRPR;
348200160128           if  *in28  or  *in90;
348300160128             *in45 = *off;
348400160128             *in57 = *on;
348500160128             if  *in28;
348600160128               V1Cmsg = Msg(57);
348700160128             endif;
348800160128             iter;
348900160128           endif;
349000160128
349100160128           // -?Controllo Filiale che Attende Risposta?
349200160129           if  W4filR  = *blank;
349300160129             if  w_POt = *zero;
349400160129               W4filR  = %editc( DUTpou : 'X' );
349500160129             else;
349600160129               W4filR  = %editc( w_POt : 'X' );
349700160129             endif;
349800160128           endif;
349900160128           $AttesaRisposta = *on;
350000160128           //wwcRpr  = W4repR;   ?(già impostata prima)?
350100160128           wwcPog  = W4filR;
350200160128           exsr  sr_CtrlPOG;
350300160128           W4filR  = wwcPog;
350400160128           W4filRd = wwdPog;
350500160128           $AttesaRisposta = *off;
350600160128           if  *in28  or  *in90;
350700160128             *in46 = *off;
350800160128             *in58 = *on;
350900160128             if  *in28;
351000160128               V1Cmsg = Msg(58);
351100160128             endif;
351200160128             iter;
351300160128           endif;
351400160128           if  W4filR = c_Sede;
351500160128             *in28 = *on;
351600160128             *in58 = *on;
351700160128             V1Cmsg = Msg(48);
351800160128             iter;
351900160128           endif;
352000160128
352100160128           // -?Confronto tra Reparto/Filiale in Attesa Risposta?
352200160128           //            ?con Reparto/Filiale di Gestione?
352300160128           If  W4repR   = V1Crpg  and
352400160128               W4filR   = V1Cpog;
352500160128             *in28 = *on;
352600160128             *in57 = *on;
352700160128             V1Cmsg = Msg(59);
352800160128             iter;
352900160128           EndIf;
353000160204
353100160204           // -?Reperimento ULTIMO Progressivo Note inserito?
353200160204           setGT   ( V1Cant : V1Cnut : V1Cpdt : KEYtin )  FITGN000;
353300160204           readPE  ( V1Cant : V1Cnut : V1Cpdt : KEYtin )  FITGN000;
353400160204           if  %eof(FITGN01L);
353500160204             clear  TGNpgn;
353600160204           endif;
353700160204           // -?Note obbligatorie?
353800160204           If  TGNpgn <= wSavePGN;
353900160204             exsr  sr_F18;
354000160204             *in90 = ( oB0fxx <> *blank );
354100160204           EndIf;
354200160128
354300160128         EndDo;
354400160129
354500160204         V1CfilR  = W4filR;
354600160204         V1CrepR  = W4repR;
354700160204         $GesW04N = *on;
354800160128
354900160128       ENDSR;
355000140429
355100140429       //--------------------------------------------------------------
355200140429       //?Controlli specifici per Motivo Apertura R.A. " 17"=F.Deposito?
355300140429       //--------------------------------------------------------------
355400140429       BEGSR  sr_CtrMotAp_17;
355500140430
355600140430         Select;
355700140430
355800140430           // -?Errore SE si è in Sede?
355900140430           When  *in05;
356000140430             *in28  = *on;
356100140430             *in36  = *on;
356200140430             V1Cmsg = Msg(42);
356300140430             leavesr;
356400140430
356500140430           // -?Errore SE la spedizione NON è in Arrivo?
356600140430           When  Not $FNARB;
356700140430             *in28  = *on;
356800140430             *in36  = *on;
356900140430             V1Cmsg = Msg(42);
357000140430             leavesr;
357100140430
357200140430           // -?Errore SE la linea di partenza NON è tra le filiali in?
357300140430           //  ?gestione all'utente?
357400140430           When  %lookup( %editc( ARBlnp : 'X' ) : wPOG ) = *zero;
357500140430             *in28 = *on;
357600140430             *in36 = *on;
357700140430             V1Cmsg = Msg(41);
357800140430             leavesr;
357900140430
358000140430         EndSl;
358100140430
358200140430
358300140430         // -?Verifica SE la causale variazione bolle "I7" può essere?
358400140430         //  ?utilizzata con la spedizione in oggetto?
358500140429         clear  FNLR66ds;
358600140429
358700140507         D66ges = 'K';
358800140429         D66cvb = c_CauVarB_I7;
358900140429         D66aas = V1annS;
359000140429         D66lnp = V1lnpS;
359100140429         D66nrs = V1nrsS;
359200140429         D66nsp = V1nspS;
359300140430         D66tbo = 'A';
359400140429
359500140505         kpjbu = FNLR66ds;
359600140505         fnlr66r ( kpjba );
359700140505         FNLR66ds = kpjbu;
359800140505
359900140505         if  D66msg <> *blank;
360000140505           *in28  = *on;
360100140505           *in36  = *on;
360200140505           V1Cmsg = Msg(26);
360300140505           leavesr;
360400140505         endif;
360500140429
360600140429
360700140429         // -?Verifica SE ci sono errori nella manutenzione della?
360800140429         //  ?spedizione in oggetto?
360900140429         clear  dsARBd;
361000140429         clear  dsARBk;
361100140429         clear  dARBt;
361200140429         clear  dsARBg;
361300140429         clear  FNLR48ds;
361400140429
361500140429         D48aas = V1annS;
361600140429         D48lnp = V1lnpS;
361700140429         D48nrs = V1nrsS;
361800140429         D48nsp = V1nspS;
361900140430         D48cbo = ARBcbo;
362000140429         D48cvb = c_CauVarB_I7;
362100140429         D48trc = 'D';
362200140429         D48ffr = 'F';
362300140429         D48tbo = D66tbo;
362400140429
362500140430         §BDrsd = ARBrsd;
362600140430         §BDind = ARBind;
362700140430         §BDcad = ARBcad;
362800140430         §BDlod = ARBlod;
362900140430         §BDprd = ARBprd;
363000140430         §BDnzd = ARBnzd;
363100140430         §BDfin = ARBfin;
363200140506         §BDffd = 'S';
363300140430         §BDcpi = ARBcpi;
363400140429
363500140429         // -?Reperimento ragione sociale estesa del destinatario?
363600140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'D' )  FIAR4000;
363700140429         if  %found(FIAR401L);
363800140429           §BDrd2 = AR4not;
363900140429         endif;
364000140429
364100140429         // -?Reperimento referente e telefono del destinatario?
364200140429         clear  dAR5gen;
364300140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'GEN' )  FIAR5000;
364400140506         if  %found(FIAR501L)  and  AR5atb = *blank;
364500140429           dAR5gen = AR5uni;
364600140429           §BDref = §AR5ref;
364700140429           §BDtel = §AR5teld;
364800140429         endif;
364900140429
365000140429         kpjbu = FNLR48ds;
365100140429         fnlr48r ( kpjba :
365200140429                   dsARBd :
365300140429                   dsARBk :
365400140429                   dARBt :
365500140429                   dsARBg :
365600140429                   TRUL90ds );
365700140429         FNLR48ds = kpjbu;
365800140429
365900140429         if  D48err <> *blank;
366000140429           *in28  = *on;
366100140430           *in36  = *on;
366200140506           V1Cmsg = D48msg;
366300140429           leavesr;
366400140429         endif;
366500140429
366600140429       ENDSR;
366700151130
366800151130       //--------------------------------------------------------------
366900151203       //?Controlli specifici per Motivo Apertura R.A. " 18"=Reso      ?
367000151130       //--------------------------------------------------------------
367100151130       BEGSR  sr_CtrMotAp_18;
367200151130
367300151201         Select;
367400151201           // -?Errore SE inserimento da Sede?
367500151201           When  *in05;
367600151201             *in28  = *on;
367700151201             *in36  = *on;
367800151201             V1Cmsg = Msg(53);
367900151201             leavesr;
368000151201           // -?Errore SE R.A. non per Spedizione?
368100151201           When  V1Ctor <> 'S';
368200151201             *in28  = *on;
368300151201             *in36  = *on;
368400151201             V1Cmsg = %subst( Msg(45) : 1 : 48 );
368500151201             leavesr;
368600151201         EndSl;
368700151130
368800151130         // -?Errore SE bolla EXPORT (LNA estera)?
368900151130         clear  xx;
369000151201         if  $FNARB = *on;
369100151201           xx = %lookup( %editc( ARBlna : 'X' ) : sk_FILest );
369200151201         else;
369300151201           xx = %lookup( %editc( BLPlna : 'X' ) : sk_FILest );
369400151201         endif;
369500151201         if  xx > *zero;
369600151130           *in28  = *on;
369700151130           *in36  = *on;
369800151130           V1Cmsg = Msg(55);
369900151130           leavesr;
370000151130         endif;
370100151130
370200151130         // -?Verifica se esiste già una disposiz. di Reso/Dirottamento?
370300151130         //  ?per la stessa spedizione?
370400151130         clear  FNLRY09ds2;
370500151204         iLRY09tch  = 'T';
370600151130         iLRY09aas  = V1annS;
370700151130         iLRY09lnp  = V1lnpS;
370800151130         iLRY09nrs  = V1nrsS;
370900151130         iLRY09nsp  = V1nspS;
371000151130         kpjbu = FNLRY09ds2;
371100151130         fnlrY09r ( kpjba );
371200151130         FNLRY09ds2 = kpjbu;
371300151130         If  oLRY09esdi = *on;
371400151130           *in28  = *on;
371500151130           *in36  = *on;
371600151130           V1Cmsg = Msg(56);
371700151204           if  oLRY09reso = *on;
371800151204             xx = %scan( 'Dirottamento' : V1Cmsg );
371900151204             V1Cmsg = %replace( 'Reso        ' : V1Cmsg : xx );
372000151204             V1Cmsg = %subst( V1Cmsg : 1 : xx + 4 ) +
372100151204                      %trimL( %subst( V1Cmsg : xx + 4 ) );
372200151130           endif;
372300151130           leavesr;
372400151130         EndIf;
372500151130
372600160314         // -?Verifica se presente Giacenza aperta?
372700160314         chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  TIGCP21;
372800160314         $Giacenza = ( %found( TIGCP21L )  and
372900160314                       GCPatb = *blank     and
373000160314                       GCPfas < 40 );
373100160314         // -?SE non ho trovato il rcd di giacenza in arrivo provo in partenza?
373200160314         If  Not %found( TIGCP21L );
373300160314           chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  TIGCP000;
373400160314           $Giacenza = ( %found( TIGCP01L )  and
373500160314                         GCPatb = *blank     and
373600160314                         GCPfas < 50 );
373700160314         EndIf;
373800160314         if  $Giacenza;
373900160314           *in28  = *on;
374000160314           *in36  = *on;
374100160315           V1Cmsg = Msg(61);
374200160314           leavesr;
374300160314         EndIf;
374400160314
374500160314         // -?Verifica se bolla legata: dirottamento ammesso solo sulla?
374600160314         //  ?ultima figlia?
374700160314         chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  FNLBL000;
374800160314         if  %found( FNLBL02L );
374900160314           *in28  = *on;
375000160314           *in36  = *on;
375100160314           V1Cmsg = Msg(62);
375200160314           leavesr;
375300160314         endif;
375400160314
375500151130       ENDSR;
375600141210
375700141210       //--------------------------------------------------------------
375800141210       //?Ctrl specifici per Motivo Apertura R.A. " 60"=Verif. PINcode ?
375900141210       //--------------------------------------------------------------
376000141210       BEGSR  sr_CtrMotAp_60;
376100141210
376200141210         Select;
376300141210
376400141210           // -?Errore SE si è in Sede?
376500141210           When  *in05;
376600141210             *in28  = *on;
376700141210             *in36  = *on;
376800141210             V1Cmsg = Msg(42);
376900141210             leavesr;
377000141210
377100141212           // -?Errore SE utente NON abilitato alla modifica bolle fil.?
377200141212           When  §UTEmbf = *blank;
377300141210             *in28  = *on;
377400141210             *in36  = *on;
377500141210             V1Cmsg = Msg(27);
377600141210             leavesr;
377700141210
377800141210           // -?Errore SE la spedizione NON è in Arrivo?
377900141210           When  Not $FNARB;
378000141210             *in28  = *on;
378100141210             *in36  = *on;
378200141210             V1Cmsg = Msg(45);
378300141210             leavesr;
378400141210
378500141210         EndSl;
378600141210
378700141210
378800141210         // -?Errore SE la spedizione NON è in consegna oggi?
378900141212         exsr  ctr_SpeCons;
379000141212         if  $Cons = *off;
379100141210           *in28  = *on;
379200141210           *in36  = *on;
379300141210           V1Cmsg = Msg(45);
379400141210           leavesr;
379500141210         endif;
379600141210
379700141210         // -?Verifica SE la Particolarità Consegna prevede il PINcode?
379800141210         clear  ds7R;
379900141210         ds_TNTBE.TBEke1 = ARBgma;
380000141210         if  getTabella ( c_Tabel : '7R'  : ds_TNTBE.TBEke1 :
380100141210                          *omit : *omit : *omit :
380200141210                          *omit : *omit :
380300141210                          *omit : *omit : *omit : *omit :
380400141210                          *omit : *omit : *omit : *omit :
380500141210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
380600141210                        = *zero      AND
380700141210             ds_TNTBE.TBEatb = *blank;
380800141210           ds7R   = ds_TNTBE.TBEuni;
380900141210         endif;
381000141210         if  ds7R.§7RpinCode <> 'S';
381100141210           *in28  = *on;
381200141210           *in36  = *on;
381300141210           V1Cmsg = Msg(46);
381400141210           leavesr;
381500141210         endif;
381600141218
381700141218         // -?Verifica esistenza trk "Y" in FIAR4 per la spedizione?
381800141218         setll  ( V1annS : V1lnpS : V1nrsS : V1nspS : 'Y' )  FIAR4000;
381900141218         if  NOT %equal(FIAR401L);
382000141218           *in28  = *on;
382100141218           *in36  = *on;
382200141218           V1Cmsg = Msg(47);
382300141218           leavesr;
382400141218         endif;
382500141210
382600141210       ENDSR;
382700150717
382800150717       //--------------------------------------------------------------
382900150717       //?Ctrl specifici per Motivo Apertura R.A. " 70"=Reclamo da Sede?
383000150717       //--------------------------------------------------------------
383100150717       BEGSR  sr_CtrMotAp_70;
383200150717
383300150717         Select;
383400150717
383500150717           // -?Errore SE NON si è in Sede?
383600150717           When  Not *in05;
383700150717             *in28  = *on;
383800150717             *in36  = *on;
383900150717             V1Cmsg = Msg(51);
384000150717             leavesr;
384100150717
384200150717         EndSl;
384300150717
384400150717       ENDSR;
384500140429
384600140429      /end-free
384700150421
384800150421       //--------------------------------------------------------------
384900150421       //?Controllo Mail del cliente.                                  ?
385000150421       //--------------------------------------------------------------
385100150421     c     sr_CtrlMail   BEGSR
385200150421      *
385300150421     c                   clear                   DSEMAIL
385400150421     c                   movel     wwMail        §emlindi
385500150421     c                   call      'XEMAIL'
385600150421     c                   parm                    DSEMAIL
385700150421      * errore
385800150421    3c                   If        §emlerro = '1'
385900150421     C                   eval      *IN28 = *on
386000150421     C                   eval      *IN38 = *on
386100150421     c                   movel     §emlmsgo      V1Cmsg
386200150421     c                   leavesr
386300150421      *
386400150421     c                   else
386500150421     c                   movel     §emlindo      wwMail
386600150421    3c                   endif
386700150421      *
386800150421     c                   ENDSR
386900150421
387000150421       //--------------------------------------------------------------
387100150421       //?Controllo Motivo Apertura Reclamo.                           ?
387200150421       //--------------------------------------------------------------
387300150421     c     sr_CtrlMAD    BEGSR
387400150421      *
387500150422     c                   clear                   wwdMAD
387600150422      *
387700150421    2c                   If        wwcMAD <> *blanks
387800150421      *
387900150421     c                   clear                   tibs02ds
388000150421      *
388100150421      * Ricerca
388200150421     c     '?'           scan      wwcMAD
388300150421    3c                   if        %found
388400150421      *
388500150421     c                   clear                   tntbMARds1
388600150421      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
388700150421      *      dall'utente in APERTURA;
388800150421      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
388900150421      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
389000150421      *      in GESTIONE.
389100150421     c                   if        wMod <> 'I'
389200150421     c                   eval      iMARutUt = 'I'
389300150421     c                   endif
389400150421     c                   eval      iMARutAp = 'I'
389500150421     c                   movel(p)  tntbMARds1    KPJBU
389600150421      *
389700150421     c                   call      'TNTBMARR1'
389800150421     c                   parm                    KPJBA
389900150421      *
390000150421     c                   movel     KPJBU         tntbMARds1
390100150421    4c                   if        oMARfxx = *blank  and  oMARerr = *off
390200150421     c                   eval      wwcMAD  = oMARcod
390300150422     c                   eval      wwdMAD  = oMARdes
390400150421      *  ?Impostazione campi (se variati) ?
390500150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
390600150421      *    ?(SE variato il Motivo Apertura)?
390700150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
390800150421if  5c                   if        wMod    =  'I'       and
390900150421     c                             wwcMAD  <> wSaveMAD  and
391000150421     c                             $Video  <> 'W4'
391100150429     c                   eval      V1Cmad  =  wwcMAD
391200150429     c                   eval      wSaveMAD = V1Cmad
391300150421     c                   exsr      sr_Ctrl_MAR
391400150421     c                   eval      V1Ccdt   = §MARcauChP
391500150421e   5c                   endif
391600150421      *  errore (non trovato nulla pulisco il '?' )
391700150421     C                   ELSE
391800150421     c                   clear                   wwcMAD
391900150422     c                   clear                   wwdMAD
392000150421    4C                   ENDIF
392100150421      *
392200150421     c                   eval      *in90 = *on
392300150421     C                   eval      *IN36 = *on
392400150421     c                   leavesr
392500150421      *
392600150421   x3C                   ELSE
392700150421      *
392800150421      * Controllo
392900150421     c                   exsr      sr_Ctrl_MAR
393000150723     c                   if        *in28  or  *in90
393100150421     c                   leavesr
393200150421     c                   endif
393300150421      *  ?Impostazione campi (se variati) ?
393400150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
393500150421      *    ?(SE variato il Motivo Apertura)?
393600150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
393700150421if  4c                   if        wMod    =  'I'       and
393800150421     c                             wwcMAD  <> wSaveMAD  and
393900150421     c                             $Video  <> 'W4'
394000150421     c                   eval      wSaveMAD = wwcMAD
394100150421     c                   exsr      sr_Ctrl_MAR
394200150421     c                   eval      V1Ccdt   = §MARcauChP
394300150421e   4c                   endif
394400150421      *
394500150421    3c                   endif
394600150421      *
394700150421      * Confronto del Motivo associato al Tipo Oggetto Reclamo
394800150421      *           con quello inserito a video
394900150422      *?- SE si viene da subfile-control "C01" può essere stato variato?
395000150422      *?  (NON se si viene dalla window "W04")?
395100150421if  3c                   if        §MARtor <> *blank  and  V1Ctor <> §MARtor
395200150421     c                   clear                   dOGR_S
395300150421     c                   eval      T02mod = 'C'
395400150421     c                   eval      T02sif = knsif
395500150421     c                   eval      T02cod = 'OGR'
395600150421     c                   eval      T02ke1 = §MARtor
395700150421     c                   call      'TIBS02R'
395800150421     c                   parm                    KPJBA
395900150421     c                   parm                    TIBS02ds
396000150421if  4c                   if        T02err = *blanks
396100150421     c                   movel     T02uni        dOGR_S
396200150421e   4c                   endif
396300150421      *
396400150421     c                   eval      *in28 = *on
396500150421     c                   eval      *in31 = *on
396600150421     c                   eval      V1Cmsg = %trim(Msg(39)) +
396700150421     c                                      ' "' + §MARtor + '" = ' +
396800150421     c                                      %trim(dOGR_S.§OGRdesc)
396900150421     c                   leavesr
397000150421e   3c                   endif
397100150421      *
397200150421   x2c                   Else
397300150421      *
397400150421      *  Campo Obbligatorio
397500150421     c                   eval      *in28 = *on
397600150723     C                   eval      *IN36 = *on
397700150421     c                   movel     MSG(8)        V1Cmsg
397800150421     c                   leavesr
397900150421      *
398000150421    2C                   EndIf
398100150421      *
398200150421      *?Controlli specifici per Motivo Apertura R.A. " 17"?
398300150421      *?(Fermo Deposito autorizza mittente)?
398400150421      *?SE non protetto?
398500150421     c                   If        wwcMAD = c_FDEAM
398600150421     c                             and  Not *in50
398700150421     c                             and  Not *in70  and  Not *in72
398800150421     c                             and  Not *in74
398900150421     c                   exsr      sr_CtrMotAp_17
399000150421     c                   if        *in28  or  *in90
399100150421     c                   leavesr
399200150421     c                   endif
399300150421     c                   EndIf
399400151130      *
399500151130      *?Controlli specifici per Motivo Apertura R.A. " 18"?
399600151130      *?(Reso)?
399700151130      *?SE  INSERIMENTO  e  non protetto?
399800151130     c                   If        wwcMAD = c_MotAp_Reso  and
399900151130     c                             Wmod   = 'I'      and
400000151130     c                             Not *in50         and
400100151130     c                             Not *in70         and  Not *in72
400200151130     c                                               and  Not *in74
400300151130     c                   exsr      sr_CtrMotAp_18
400400151130     c                   if        *in28  or  *in90
400500151130     c                   leavesr
400600151130     c                   endif
400700151130     c                   EndIf
400800150421      *
400900150421      *?Controlli specifici per Motivo Apertura R.A. " 60"?
401000150421      *?(Verifica telefonica del "PINcode")?
401100150421      *?SE  INSERIMENTO  e  non protetto?
401200150421     c                   If        wwcMAD = c_VPINC  and
401300150421     c                             Wmod   = 'I'      and
401400150421     c                             Not *in50         and
401500150421     c                             Not *in70         and  Not *in72
401600150421     c                                               and  Not *in74
401700150421     c                   exsr      sr_CtrMotAp_60
401800150421     c                   if        *in28  or  *in90
401900150421     c                   leavesr
402000150421     c                   endif
402100150421     c                   EndIf
402200150717      *
402300150717      *?Controlli specifici per Motivo Apertura R.A. " 70"?
402400150717      *?(Reclamo da Sede)?
402500150717      *?SE non protetto?
402600150717     c                   If        wwcMAD = c_ReclSede
402700150717     c                             and  Not *in70  and  Not *in72
402800150717     c                                             and  Not *in74
402900150717     c                   exsr      sr_CtrMotAp_70
403000150717     c                   if        *in28  or  *in90
403100150717     c                   leavesr
403200150717     c                   endif
403300150717     c                   EndIf
403301170524      *
403302170524      *?Controlli specifici per Motivo Apertura R.A. " 12"?
403303170524      *?(Giacenza)?
403304170524      *?SE non protetto?
403305170524     c                   If        wwcMAD = W_Gia
403306170524     c                             and  Not *in70  and  Not *in72
403307170524     c                             and  Not *in50  and  Not *in74
403308170524     c                   exsr      sr_CtrGiacenza
403309170524     c                   if        *in28  or  *in90
403310170524     c                   leavesr
403311170524     c                   endif
403312170524     c                   EndIf
403400150421      *
403500150421     c                   ENDSR
403600150429
403700150616      /free
403800150616
403900150429       //--------------------------------------------------------------
404000150429       //?Controllo Reparto Gestione.                                  ?
404100150429       //--------------------------------------------------------------
404200150429       BEGSR  sr_CtrlRPR;
404300150429
404400150429         clear  wwdRPR;
404500150429
404600150429         Select;
404700150429
404800150429           When  %scan( '?' : wwcRPR ) > *zero;
404900150429             clear  wwcRPR;
405000150508             clear  tntbRGRds;
405100150508             iRGRopz = 1;
405200150508             kpjbu   = tntbRGRds;
405300150508             tntbRGRr ( KPJBA );
405400150508             tntbRGRds = kpjbu;
405500150508             If  oRGRerr = *off  and  oRGRfxx = *blank;
405600150508               wwcRPR = oRGRcod;
405700150508               wwdRPR = oRGRdes;
405800150508             endif;
405900150429             *in90 = *on;
406000150429             if  $Video = 'W4';
406100150429               *in45 = *on;
406200150429             else;
406300150429               *in39 = *on;
406400150429             endif;
406500150429             leavesr;
406600150429
406700150429           When  wwcRPR = *blank;
406800150429             *in28 = *on;
406900150429             if  $Video = 'W4';
407000150429               *in45 = *on;
407100150429             else;
407200150429               *in39 = *on;
407300150429             endif;
407400150616             if  *in05;
407500150616               V1Cmsg = Msg(20);
407600150616             else;
407700150616               V1Cmsg = Msg(50);
407800150616             endif;
407900150429             leavesr;
408000150429
408100150429           Other;
408200150429             clear  TIBS02ds;
408300150429             T02mod = 'C';
408400150429             T02sif = knsif;
408500150429             T02cod = 'RGR';
408600150429             T02ke1 = wwcRPR;
408700150429             TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
408800150429             if  T02err = *blank;
408900150429               wwdRPR = T02uni;
409000150429             else;
409100150429               *in28 = *on;
409200150429               if  $Video = 'W4';
409300150429                 *in45 = *on;
409400150429               else;
409500150429                 *in39 = *on;
409600150429               endif;
409700150616               if  *in05;
409800150616                 V1Cmsg = Msg(20);
409900150616               else;
410000150616                 V1Cmsg = Msg(50);
410100150616               endif;
410200150429               leavesr;
410300150429             endif;
410400150429
410500150429         EndSl;
410600150429
410700150429       ENDSR;
410800150616
410900150616       //--------------------------------------------------------------
411000150616       //?Controllo Utente Gestione.                                   ?
411100150616       //--------------------------------------------------------------
411200150616       BEGSR  sr_CtrlRPG;
411300150616
411400150616         clear  wwdRPR;
411500150616
411600150616         // -?Verifica se esiste utente nell'archivio profili?
411700150616         chain  ( wwcRPR )  AZUTE000;
411800150616
411900150616         If  %found(AZUTE01L)  and  UTEdsc > DateU;
412000150616
412100150616           // -?SE lo trova: verifica se è della filiale di gestione?
412200150723           if  %editc(UTEfil:'X') <> wwcPOG;
412300150616             *in28  = *on;
412400150616             *in46  = *on;
412500150616             V1Cmsg = Msg(23);
412600150616             leavesr;
412700150616           endif;
412800150616
412900150616         Else;
413000150616
413100150616           *in28 = *on;
413200150616           *in45 = *on;
413300150616           V1Cmsg = Msg(20);
413400150616           leavesr;
413500150616
413600150616         EndIf;
413700150616
413800150616       ENDSR;
413900150911
414000150911       //--------------------------------------------------------------
414100150911       //?Controllo C.A.P. cliente per reperimento Filiale Gestione    ?
414200150911       //?(per R.A. inerita in SEDE)                                   ?
414300150911       //--------------------------------------------------------------
414400150911       BEGSR  sr_CtrlCAP;
414500150911
414600150911         // -?Se CAP vuoto: uscita?
414700150914         if  W4cCap <> *blank  and  %trim( W4cCap ) = *zero;
414800150911           clear  W4cCap;
414900150911         endif;
415000150911
415100150911         if  W4cCap = *blank;
415200150911           leavesr;
415300150911         endif;
415400150915
415500150915         // -?Selezione CAP?
415600150915         if  %scan( '?' : W4cCap ) > *zero;
415700150915
415800150915           clear  W4cCap;
415900150915           clear  TISI00ds;
416000150915           D00op0 = 'L01';
416100150915           D00dri = DateU;
416200150915           clear  TISI30ds;
416300150915           I30flg = 'S';
416400150915           TISI30R ( kpjba : TISI00ds : TISI30ds );
416500150915           if  D00cap <> *blank;
416600150915             W4cCap = D00cap;
416700150915           endif;
416800150915
416900150915         endif;
417000150911
417100150911         // -?Reperimento filiale (LNA) dal CAP?
417200150911         clear  TISI95ds;
417300150911         I95tcn = '3';
417400150911         I95cap = W4cCap;
417500150911         I95dat = DateU;
417600150911
417700150911         TISI95R ( TISI95ds );
417800150911
417900150914         If  O95err = *blank;
418000150914
418100150914           if  W4cPog <> %editc( O95lna : 'X' );
418200150914             *in90 = *on;
418300150914           endif;
418400150914           W4cPog = %editc( O95lna : 'X' );
418500150914           clear  W4cCap;
418600150915
418700150915           reset  FNLV55ds;
418800150915           //D55tla = 'L';
418900150915           //D55tpt = 'P';
419000150915           D55lin = O95lna;
419100150915           D55drf = DateU;
419200150915           fnlv55r ( FNLV55ds );
419300150915           if  D55err = *blank;
419400150915             W4cPog = %editc( D55tfp : 'X' );
419500150915           endif;
419600150914
419700150914         Else;
419800150914
419900150914           *in28 = *on;
420000150914           *in56 = *on;
420100150914           V1Cmsg = Msg(54);
420200150914           leavesr;
420300150914
420400150914         EndIf;
420500150911
420600150911       ENDSR;
420700150616
420800150616      /end-free
420900150421
421000150421       //--------------------------------------------------------------
421100150421       //?Controllo Filiale Gestione.                                  ?
421200150421       //--------------------------------------------------------------
421300150421     c     sr_CtrlPOG    BEGSR
421400150421      *
421500150422     c                   clear                   wwdPOG
421600150422      *
421700150421     c                   IF        %scan('?':wwcPOG) > 0
421800150421     c                   clear                   dstnsd
421900150421     c                   call      'TNSD24R'
422000150421     c                   parm                    §cod
422100150421     c                   parm                    §tip
422200150421     c                   parm                    §des
422300150421     c                   parm                    §pra
422400150421     c                   movel     §cod          wwcPOG
422500150422     c                   movel     §des          wwdPOG
422600150421     c                   eval      *in90 = *on
422700150421     c                   eval      *in46 = *on
422800150421     c                   leavesr
422900150421     c                   ENDIF
423000150421
423100150421     c                   IF        wwcPOG = *blanks
423200150421     c                   eval      *in28  = *on
423300150421     c                   eval      *in46  = *on
423400150421     c                   eval      V1Cmsg = Msg(48)
423500150421     c                   leavesr
423600150421     c                   ENDIF
423700150421
423800150421     c                   eval      xx = 1
423900150421     c                   FOR       xx by 1 to %len(wwcPOG)
424000150421     c                   IF        %subst(wwcPOG:xx:1) <> *blanks and
424100150421     c                             %check(Digits:%subst(wwcPOG:xx:1)) > *zeros
424200150421     c                   eval      *in28 = *on
424300150421     c                   eval      *in46 = *on
424400150421     c                   eval      V1Cmsg = Msg(48)
424500150421     c                   leavesr
424600150421     c                   ENDIF
424700150421     c                   ENDFOR
424800150421
424900150421     c                   move      wwcPOG        w0030
425000160104     c     w0030         chain     AZORG
425100150717     c                   IF        not %found(AZORG01L)  OR
425200150717     c                              ORGfva <> *blanks    OR
425300150428     c                             (wwcPOG <> c_Sede  and
425400150717     c                              ORGfag <> 'F'     and
425500150723     c                              ORGfag <> 'A')
425600150421     c                   eval      *in28 = *on
425700150421     c                   eval      *in46 = *on
425800150421     c                   eval      V1Cmsg = Msg(48)
425900150421     c                   leavesr
426000150421     c                   ENDIF
426100150422     c                   movel     ORGdes        wwdPOG
426200160120      *
426300150428      *?Filiale Gestione 046 inseribile (in modifica) SOLO da utente di Sede?
426400151211     c                   If        wMod   <> 'I'    and
426500150428     c                             wwcPOG = c_Sede  and  SIMfel <> *zero
426600160121     c                                              and  NOT $AttesaRisposta
426700150429      * - prima verifica che non sia stata richiesta l'interrogazione
426800160121     c                   if        %scan( '?' : V1Crpg ) > *zero
426900150429     c                   eval      wwcRPR = V1Crpg
427000150429     c                   exsr      sr_CtrlRPR
427100160121     c                   eval      V1Crpg = wwcRPR
427200150429     c                   eval      *in28 = *off
427300150429     c                   eval      *in45 = *off
427400150429     c                   eval      *in39 = *off
427500150429     c                   clear                   V1Cmsg
427600160120     c                   endif
427700160121     c                   eval      w_Ute  = V1Crpg
427800150428     c                   exsr      sr_TabUGR
427900150428     c                   if        §UGRpofo <> %int(c_Sede)
428000150428     c                   eval      *in28 = *on
428100150428     c                   eval      *in46 = *on
428200150428     c                   eval      V1Cmsg = Msg(48)
428300150428     c                   leavesr
428400150428     c                   endif
428500150428     c                   EndIf
428600150421      *
428700150421     c                   ENDSR
428800150428
428900150428      /free
429000150428
429100150428       //--------------------------------------------------------------
429200150428       //?Reperimento dati Tab. "UGR" (Utente Gestione Reclami)        ?
429300150428       //--------------------------------------------------------------
429400150428       BEGSR  sr_TabUGR;
429500150428
429600150428         clear  dUGR;
429700150428         clear  TIBS02ds;
429800150428         T02mod = 'C';
429900150428         T02sif = KNSIF;
430000150428         T02cod = 'UGR';
430100150428         T02ke1 = w_Ute;
430200150428
430300150428         TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
430400150428
430500150428         if  T02err = *blank;
430600150428           dUGR = T02uni;
430700150428         endif;
430800150428
430900150428       ENDSR;
431000150428
431100150428      /end-free
431200140429
431300070430      *---------------------------------------------------------------*
431400070430      * Controlli parte alta del ctl per f4 fine richiesta            *
431500070430      *---------------------------------------------------------------*
431600070430     c     SR_CONTRF4    BegSR
431700070430
431800070430    1c                   do        *hival
431900070430      * se dato F4 e non è stato inserito nessun oggetto e non lo sto inserendo adesso ERRORE
432000070430    2c                   if        s01nrr = 0  and v1ctor = *blanks and
432100070430     c                             v1cogg = *blanks and ormvid = *zeros and
432200070430     c                             spevid = *zeros and v1ctch = *blanks and
432300070430     c                             v1cmad = *blanks and v1ccdt = *blanks and
432400070430     c                             v1not1 = *blanks and v1not2 = *blanks and
432500070430     c                             v1not3 = *blanks and v1not4 = *blanks and
432600070430     c                             v1crsp = *blanks and v1cpor = *blanks
432700070430     c                   eval      *in28 = *on
432800070430     c                   movel     MSG(14)       V1Cmsg
432900070430     C                   eval      *IN44 = *on
433000070430     c                   leave
433100070430    2c                   endif
433200070430      * verifco se inserito  il codice chiamante
433300070430    2c                   If        v1cchi = *zeros and wforz = ' '
433400070430      * richamo la routine di ricerca codice nel dettaglio oggetti
433500070430     c                   exsr      SR_RECCHI
433600070430      * controllo se ne ho uno da proporre
433700070430     c                   select
433800070430     c                   when      wrk_cchiA > 0
433900070430     c                   eval      wrk_cchi = wrk_cchiA
434000070430     c                   when      wrk_cchiB > 0
434100070430     c                   eval      wrk_cchi = wrk_cchiB
434200070430     c                   other
434300070430     c                   eval      wrk_cchi = wrk_cchiC
434400070430     c                   endsl
434500070430      *
434600070430    3c                   if        wrk_cchi > *zeros
434700070430     c                   eval      v1cchi = wrk_cchi
434800070430     c                   eval      SW_v1cchi = wrk_cchi
434900070430     c                   clear                   wrk_cchi
435000070430      * se ragione sociale a blank la decodifico
435100070430     C                   clear                   TIBS69DS
435200070430     c                   z-add     v1cchi        I69kac
435300070430     c                   z-add     v1cchi        I69kcp
435400070430     C                   call      'TIBS69R'
435500070430     C                   parm                    tibs69DS
435600070430     C                   parm                    DS_cnaco
435700070430     C                   parm                    DS_cnind
435800070430     C                   parm                    DS_cnclp
435900070430     C                   parm                    DS_fncls
436000070430     C                   eval      wtibs69r = '1'
436100070430    4c                   If         O69ERR =  ' '
436200070430     c                   movel     ACOrag        v1dchi
436300070430     c                   movel     clpclv        v1cclv
436400070430    4c                   Endif
436500070430      *
436600070430   x3c                   else
436700070430      *
436800070430     c                   eval      wforz = '1'
436900070430     c                   eval      *in28 = *on
437000070430     c                   movel     MSG(32)       V1Cmsg
437100070430     C                   eval      *IN10 = *on
437200070430     c                   leave
437300070430    3c                   endif
437400070430    2c                   endif
437500070430
437600070430      * controllo del chiamante se uguale a blanks ma codice cliente valorizzato non faccio nulla
437700070430
437800070430    2c                   If        v1crsc = *blanks and v1dchi = *blanks
437900070430     c                   eval      *in28 = *on
438000070430     c                   movel     MSG(15)       V1Cmsg
438100070430     C                   eval      *IN44 = *on
438200070430     c                   leave
438300070430      * Se anche il chiamante è a blank imposto la ragione sociale del cliente
438400070430     c                   else
438500070430    3c                   if        v1dchi <> *blanks AND v1crsc = *blanks
438600070430     c                   eval      v1crsc = v1dchi
438700070430    3c                   endif
438800070430    2c                   endif
438900070430      *
439000070503     c                   leave
439100070430     c                   enddo
439200070430      *
439300070430     c                   ENDSR
439400070430      *
439500070430      *---------------------------------------------------------------*
439600070430      * Gestione interrogazione oggetto                               *
439700070430      *---------------------------------------------------------------*
439800070430     c     SR_GESF2      BegSR
439900070430
440000070430    1c                   do        *hival
440100070430      * interrogazione Spedizione
440200070430      *
440300070430    2c                   if        v1ctor = 'S'
440400131119      * se non sono in sede
440500070430    3c                   if        not *in05
440600070430      *
440700160701      * verifico se programma richiamato dal PGM interrogazione  Bolle
440800070430    5c                   If        *in50
440900070430     c                   setoff                                       75
441000070430     c     'FNLR36R   '  lookup    £Stack                                 75
441100150615     c  n75'FNLS05R   '  lookup    £Stack                                 75
441200160706     c  n75'FNLRU6R1  '  lookup    £Stack                                 75
441300070430
441400070430      * se trovato vado fuori dal programma
441500070430    6c                   if        %found
441600070430     c                   eval      *in90 = *on
441700070430     c                   eval      *in31 = *on
441702170512     c                   movel     MSG(63)       V1Cmsg
441703170512     C                   eval      *IN28 = *on
441800070430     c                   leave
441900070430    6c                   endif
442000070430      *
442100070430    5c                   endif
442200070430      *
442300070430      * se richiamato
442400070430     c                   clear                   fidna1ds
442500160706      *
442600150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
442700150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene ..
442800150601    5c                   if        *in50
442900150601      *
443000150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
443100150601      * oggetto ho più di una RA
443200150601     c                   exsr      Sr_countRA
443300150601      *
443400150601     C                   ELSE
443500150601     c                   eval      IA1INS = 'I'
443600150601    5c                   endif
443700160706      *
443800160706     c                   clear                   KPJBU
443900160706     c                   clear                   ParamU6ds1
444000160706     c                   eval      ParamU6ds1.V2Caas = V1anns
444100160706     c                   eval      ParamU6ds1.COMlnp = V1lnps
444200160706     c                   eval      ParamU6ds1.V2Cnrs = V1nrss
444300160706     c                   eval      ParamU6ds1.V2Cnsp = V1nsps
444400160706     c                   eval      KPJBU = ParamU6ds1
444500160706      * preparo l'altro parametro da passare
444600160706     c                   clear                   fnlrU6ds
444700160706     c*//                eval      iLRU6sel = 'S'
444800160706     c                   eval      iLRU6ra  = 'S'
444900160706     c                   eval      eLRU6ins = iA1ins
445000160706      *
445100160706     c                   call      'FNLRU6R1'
445200160706     c                   parm                    KPJBA
445300160706     c                   parm                    fnlrU6ds
445400160706     c                   movel     kpjbu         ParamU6ds1
445500160706     c                   if        oLRU6err = 'E'
445600160706     C                   eval      *in28 = *on
445700160706     C                   eval      *in44 = *on
445800160706     c                   movel     oLRU6msg      V1Cmsg
445900160706     c                   leave
446000160706     c                   endif
446100070430      *
446200070430     c                   eval      *in90 = *on
446300070430     C                   eval      *IN31 = *on
446400070430     c                   leave
446500070430      *
446600070430      * sono in sede
446700070430      *
446800070430   x3c                   else
446900070430      *
447000070430     c     KSPED         chain     Titas30c
447100070430      * spedizione in sede
447200070430    4c                   If        %found(Titas30c)
447300070430      * verifico se programm
447400070430
447500070430    5c                   If        *in50
447600070430     c                   setoff                                       75
447700070430     c     'TNSB50R   '  lookup    £Stack                                 75
447800070430
447900070430      * se trovato vado fuori dal programma
448000070430    6c                   if        %found
448100070430     c                   eval      *in90 = *on
448200070430     c                   eval      *in31 = *on
448300070430     c                   leave
448400070430    6c                   endif
448500120330      * se non trovo il TNSB50R faccio un tentativo anche con il TNSB51R
448600120330     c                   setoff                                       75
448700120330     c     'TNSB51R   '  lookup    £Stack                                 75
448800120330
448900120330      * se trovato vado fuori dal programma
449000120330    6c                   if        %found
449100120330     c                   eval      *in90 = *on
449200120330     c                   eval      *in31 = *on
449300120330     c                   leave
449400120330    6c                   endif
449500070430      *
449600070430    5c                   endif
449700070430      *
449800070430     c                   clear                   TNSB50DS
449900070430     C                   movel     'R05'         I50OP0
450000070430     C                   z-add     V1anns        D50aas
450100070430     C                   z-add     V1lnps        D50lnp
450200070430     C                   z-add     V1nrss        D50nrs
450300070430     C                   z-add     V1nsps        D50nsp
450400070430     C                   movel(P)  TNSB50DS      KPJBU
450500070430      *
450600070430      * se richiamato
450700070430     c                   clear                   fidna1ds
450800150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
450900150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
451000150601    5c                   if        *in50
451100150601      *
451200150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
451300150601      * oggetto ho più di una RA
451400150601     c                   exsr      Sr_countRA
451500150601      *
451600150601     C                   ELSE
451700150601     c                   eval      IA1INS = 'I'
451800150601    5c                   endif
451900070430      *
452000070430     C                   call      'TNSB50R'
452100070430     C                   parm                    KPJBA
452200070430     C                   parm                    FIDNA1DS
452300070430      *
452400070430
452500070430     c                   eval      *in90 = *on
452600070430     C                   eval      *IN31 = *on
452700070430     c                   leave
452800070430
452900070430    4C                   endif
453000070430
453100070430    3c                   endif
453200070430
453300070430    2c                   endif
453400070430      *
453500070430      * interrogazione ORM
453600070430      * interrogo direttamente con il richiano di FIOR07R
453700070430      *
453800070430    2c                   if        v1ctor = 'O'
453900070430      * verifico se programma richiamato dal PGM interrogazione  ORM
454000070430
454100070430    3c                   If        *in50
454200070430     c                   setoff                                       75
454300070430     c     'FIOR07R   '  lookup    £Stack                                 75
454400070430      * se trovato vado fuori dal programma
454500070430    4c                   if        %found
454600070430     c                   eval      *in90 = *on
454700070430     c                   eval      *in31 = *on
454800070430     c                   leave
454900070430    4c                   endif
455000070430      *
455100070430    3c                   endif
455200070430      *
455300070430     C                   clear                   PARM01
455400070430     C                   move      v1poeo        ppoe
455500070430     C                   move      v1nsor        pnsr
455600070430     C                   move      v1norm        pnor
455700070430     C                   move      v1nrvo        pnrv
455800070430     C                   move      '5'           psce
455900070430      * attenzione non muovo nulla nel campo del p.o. gestione .... vediamo se funziona !!!!!!
456000070430     C                   movel(p)  PARM01        KPJBU
456100070430      * se richiamato
456200070430     c                   clear                   fidna1ds
456300150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
456400150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
456500150601    3c                   if        *in50
456600150601      *
456700150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
456800150601      * oggetto ho più di una RA
456900150601     c                   exsr      Sr_countRA
457000150601      *
457100150601     c                   else
457200150601     c                   eval      IA1INS = 'I'
457300150601    3c                   endif
457400070430      *
457500070430     C                   call      'FIOR07R'
457600070430     C                   parm                    KPJBA
457700070430     C                   parm                    FIDNA1DS
457800070430
457900070430      *
458000070430     c                   eval      *in90 = *on
458100070430     C                   eval      *IN31 = *on
458200070430     c                   leave
458300070430
458400070430    2c                   endif
458500070430
458600070430    1c                   enddo
458700070430
458800070430     c                   endsr
458900070430      *
459000070323      *---------------------------------------------------------------*
459100070323      * Routine gestione richiesta data consegna richiesta            *
459200070323      *---------------------------------------------------------------*
459300070323     c     SR_GES_DTA    BegSR
459400070323      *
459500070416      * se si tratta di una spedizione e non è stata consegnata
459600070327     c                   if        v1ctor = 'S' and arbdcm = *zeros
459700070323      * verifico se sono la filiale di arrivo della bolla
459800130611     c                   move      arblna        w003a
459900130610     c     w003a         lookup    wPOG                                   27
460000070326     c                   If        %found
460100070326      * verifico se si tratta di :
460200070326      *                            lasciato avviso
460300070326      *                            contrassegno
460400070326      *                            supermercato
460500070326      *                            appuntamento
460600070326      *                            fermo deposito
460700070327      *
460800070430      * richiamo il pgm dei blocchi FNLRG6R
460900070430     c                   clear                   FNLRG6DS
461000070430     c                   eval      ilrg6tla = 'L'
461100070430     c                   eval      ilrg6aas = v1anns
461200070430     c                   eval      ilrg6lnp = v1lnps
461300070430     c                   eval      ilrg6nrs = v1nrss
461400070430     c                   eval      ilrg6nsp = v1nsps
461500070430     c                   call      'FNLRG6R'
461600070430     c                   parm                    fnlrg6ds
461700070430     c                   movel     olrg6abi      dlrg6abi
461800070430     c
461900070327     c                   select
462000070326      * Lasciato Avviso
462100070430     c                   when      v1cmad = W_lav
462200070327      * verifico se si tratta di spedizione bloccata
462300070430     c                   If        d§lrg6tla <> ' '
462400070327     c                   eval      W_cau = 'L'
462500070327     c                   exsr      Sr_ges_bloc
462600070327     c                   else
462700070329      * verifico se la spedizione non ha blocchi di altro tipo
462800070507     c                   if        d§lrg6tca = ' ' and d§lrg6tsu = ' '
462900070507     c                             and d§lrg6tap = ' ' and d§lrg6tfd = ' '
463000070329      * se la spedizione non è bloccata per nessun altro motivo
463100070330      * accendo indicatore di emissione della data consegna richiesta
463200070412     c                   seton                                        616290
463300070329      * gestione della videata con immissione data consegna richiesta + richiamo fnlr48r
463400070329     c                   eval      $riemetti = *on
463500070411      * valorizzo il flag per non essere richiamato di nuovo
463600070411     c                   eval      $ric_fnlrg2r = *on
463700070329     c                   endif
463800070327
463900070327     c                   endif
464000070507     c                   leavesr
464100070327
464200070502      * Contrassegno   e spedizione bloccata per c/assegno
464300070502     c                   when      v1cmad = W_con  and d§lrg6tca <> ' '
464400070327     c                   eval      W_cau = 'C'
464500070327     c                   exsr      Sr_ges_bloc
464600070507     c                   leavesr
464700070327
464800070502      * Supermercato   e spediizone bloccata per supermercato
464900070507     c                   when      v1cmad = W_sup  and d§lrg6tsu <> ' '
465000070327     c                   eval      W_cau = 'S'
465100070327     c                   exsr      Sr_ges_bloc
465200070507     c                   leavesr
465300070327
465400070502      * Appuntamento   e la spedizione è bloccata per appuntamento
465500070507     c                   when      v1cmad = W_app  and d§lrg6tap <> ' '
465600070327     c                   eval      W_cau = 'A'
465700070327     c                   exsr      Sr_ges_bloc
465800070507     c                   leavesr
465900070502
466000070502      * Fermo deposito e la spedizione è bloccata per fermo deposito
466100070502     c                   when      v1cmad = W_fde  and d§lrg6tfd <> ' '
466200070327     c                   eval      W_cau = 'F'
466300070327     c                   exsr      Sr_ges_bloc
466400070327
466500070327     c                   endsl
466600070327
466700070327     c                   endif
466800070327
466900070327     c                   endif
467000070327
467100070323     c                   Endsr
467200070327      *---------------------------------------------------------------*
467300070327      * Routine gestione sblocchi spedizioni                          *
467400070327      *---------------------------------------------------------------*
467500070327     c     SR_GES_BLOC   BegSR
467600070411      *
467700070411      * valorizzo il flag per non essere richiamato di nuovo
467800070411     c                   eval      $ric_fnlrg2r = *on
467900070327      *
468000070327     c                   clear                   Fnlrg2ds
468100070327     c                   eval      ilrg2tla = 'L'
468200070327     c                   eval      ilrg2tch = 'R'
468300070327     c                   eval      ilrg2aas = v1anns
468400070327     c                   eval      ilrg2lnp = v1lnps
468500070327     c                   eval      ilrg2nrs = v1nrss
468600070327     c                   eval      ilrg2nsp = v1nsps
468700070327     c                   eval      ilrg2cau = w_cau
468800070327      * se le note a video sono valorizzate
468900070327     c                   clear                   nov
469000070327     c                   eval      nov(1) = v1not1
469100070327     c                   eval      nov(2) = v1not2
469200070327     c                   eval      nov(3) = v1not3
469300070327     c                   eval      nov(4) = v1not4
469400070327      *
469500070327     c                   movea     nov           ilrg2no1
469600070327
469700070327     c                   call      'FNLRG2R'
469800070327     c                   Parm                    kpjba
469900070327     c                   Parm                    Fnlrg2ds
470000070327
470100070327      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
470200070327     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
470300070327      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
470400070403     c                   eval      $bloccof3    = *on
470500070327      * scrittura delle note
470600070327     c                   exsr      Sr_wrt_note
470700160512     c                   eval      $Note_scritte = *on
470800070503      * verifico se ci sono altri blocchi per questa spedizione gestibili da R.A.
470900070503     c                   exsr      Sr_piublocchi
471000070507      * recupero le note
471100070507      *
471200070507     C                   clear                   FIDNB0DS
471300070507     c                   z-add     v1cant        iB0ant
471400070507     c                   z-add     v1cnut        iB0nut
471500070507     c                   z-add     v1cpdt        iB0pdt
471600070507     c                   movel     'GEN'         iB0tIN
471700070507     c                   movel     'R'           iB0flm
471800070507      *
471900070507     C                   CALL      'FIDNB0R'
472000070507     C                   PARM                    kpjba
472100070507     C                   PARM                    FIDNB0DS
472200070507      *
472300070507      * valorizzo le prime 4 note a video
472400070507      *
472500070507     c                   movel     DB0NO1        v1not1
472600070507     c                   movel     DB0NO2        v1not2
472700070507     c                   movel     DB0NO3        v1not3
472800070507     c                   movel     DB0NO4        v1not4
472900070508      * se ob0piu è valorizzato ci sono + di 4 righe
473000070508     c                   eval      *in29 = (OB0PIU = '+')
473100070327      * proteggo i campi e riemetto la videata
473200070418     c                   seton                                        6074
473300070403     c                   eval      $riemetti = *on
473400070327     c                   endif
473500070418
473600070418     c                   eval      *in37 = *on
473700070327
473800070327     c                   endsr
473900070327      *---------------------------------------------------------------*
474000070327      * Routine gestione note che vengono dalla gestione blocchi      *
474100070327      *---------------------------------------------------------------*
474200070327     c     SR_wrt_note   BegSR
474300070412      *
474400070412     c                   clear                   nora
474500070412     c                   clear                   nob
474600070413     c                   clear                   x
474700070413     c                   clear                   y
474800070327      *
474900070327     c                   movea     olrg2no1      nob(1)
475000070327     c                   movea     olrg2no2      nob(9)
475100070327      * tolgo le note a blank
475200070327     c                   do        12            x
475300070327     c                   if        nob(x) <> *blanks
475400070327     c                   add       1             y
475500070327     c                   move      nob(x)        nora(Y)
475600070327     c                   endif
475700070327     c                   enddo
475800070328      * accendo indicatore che mi segnale che ci sono altre note se ne ho + di 4
475900070328     c                   eval      *in29 = y > 4
476000070328      * registro le note sul file note
476100070328    1c                   if        y > *zeros
476200070328     c                   clear                   FIDNB0DS
476300070328     c                   eval      db0no1 = nora(1)
476400070328     c                   eval      db0no2 = nora(2)
476500070328     c                   eval      db0no3 = nora(3)
476600070328     c                   eval      db0no4 = nora(4)
476700070508
476800070508     c                   exsr      SR_add_note
476900070508
477000070328      * verifico se ho altre note da scrivere
477100070328    2c                   if        nora(5) <> *blanks
477200070328     c                   clear                   fidnb0ds
477300070328     c                   eval      db0no1 = nora(5)
477400070328     c                   eval      db0no2 = nora(6)
477500070328     c                   eval      db0no3 = nora(7)
477600070328     c                   eval      db0no4 = nora(8)
477700070328
477800070508     c                   exsr      SR_add_note
477900070328
478000070328    2c                   endif
478100070328      * verifico se ho altre note da scrivere
478200070328    2c                   if        nora(9) <> *blanks
478300070328     c                   clear                   fidnb0ds
478400070328     c                   eval      db0no1 = nora(9)
478500070328     c                   eval      db0no2 = nora(10)
478600070328     c                   eval      db0no3 = nora(11)
478700070328     c                   eval      db0no4 = nora(12)
478800070328
478900070508     c                   exsr      SR_add_note
479000070508
479100070328    2c                   endif
479200070328
479300070328    2c                   endif
479400070328
479500070328
479600070327     c                   endsr
479700070328      *
479800070508      *---------------------------------------------------------------*
479900070508      * Routine che aggiunge note al reclamo esistente                *
480000070508      *---------------------------------------------------------------*
480100070508     c     SR_add_note   BegSR
480200070508
480300070508     c                   z-add     v1cant        iB0ant
480400070508     c                   z-add     v1cnut        iB0nut
480500070508     c                   z-add     v1cpdt        iB0pdt
480600070508     c                   z-add     999           iB0pgn
480700070508     c                   movel     'GEN'         iB0tIN
480800070508     c                   eval      ib0pot =v1cpot
480900070508     c                   eval      db0pon =Dutpou
481000070508     c                   eval      db0utn =knmus
481100070508      * imposto 'W'
481200070508     c                   movel     'W'           iB0flm
481300070508      *
481400070508     c                   call      'FIDNB0R'
481500070508     c                   parm                    kpjba
481600070508     c                   parm                    fidnb0ds
481700070508
481800070508      *
481900070508     c                   endsr
482000070503      *---------------------------------------------------------------*
482100070503      * Routine gestione eventuali blocchi in spediizione             *
482200070503      *---------------------------------------------------------------*
482300070503     c     SR_piublocchi BegSR
482400070503      * salvo il precedente blocco
482500070503     c                   eval      sw_cau = W_cau
482600070508     c                   clear                   note_blocco
482700070503      * richiamo il pgm dei blocchi FNLRG6R
482800070503     c                   clear                   FNLRG6DS
482900070503     c                   eval      ilrg6tla = 'L'
483000070503     c                   eval      ilrg6aas = v1anns
483100070503     c                   eval      ilrg6lnp = v1lnps
483200070503     c                   eval      ilrg6nrs = v1nrss
483300070503     c                   eval      ilrg6nsp = v1nsps
483400070503     c                   call      'FNLRG6R'
483500070503     c                   parm                    fnlrg6ds
483600070503     c                   movel     olrg6abi      dlrg6abi
483700070503      * verifico se ci sono ulteriori blocchi a quello gestito in precedenza
483800070503     c                   select
483900070507      * C/assegno
484000070503     c                   when      d§lrg6tca <> ' ' and sw_cau <> 'C'
484100070503     c                   eval      w_cau = 'C'
484200070508     c                   eval      note_blocco = 'Abilitazione : C/assegno'
484300070503     c                   exsr      sr_ges_bloc2
484400070507      * supermercato
484500070507     c                   when      d§lrg6tsu <> ' ' and sw_cau <> 'S'
484600070507     c                   eval      w_cau = 'S'
484700070508     c                   eval      note_blocco = 'Abilitazione : Supermercato'
484800070507     c                   exsr      sr_ges_bloc2
484900070507      * appuntamento
485000070507     c                   when      d§lrg6tap <> ' ' and sw_cau <> 'A'
485100070507     c                   eval      w_cau = 'A'
485200070508     c                   eval      note_blocco = 'Abilitazione : Appuntamento'
485300070507     c                   exsr      sr_ges_bloc2
485400070507      * fermo deposito
485500070507     c                   when      d§lrg6tfd <> ' ' and sw_cau <> 'F'
485600070507     c                   eval      w_cau = 'F'
485700070508     c                   eval      note_blocco = 'Abilitazione : Fermo Deposito'
485800070507     c                   exsr      sr_ges_bloc2
485900070507      * lasciato avviso
486000070507     c                   when      d§lrg6tla <> ' ' and sw_cau <> 'L'
486100070507     c                   eval      w_cau = 'L'
486200070508     c                   eval      note_blocco= 'Abilitazione : Lasciato Avviso'
486300070507     c                   exsr      sr_ges_bloc2
486400070507
486500070507     c                   endsl
486600070507
486700070503     c                   endsr
486800070503      *
486900070503      *---------------------------------------------------------------*
487000070503      * Routine gestione blocco come seconda o terza chiamata         *
487100070503      *---------------------------------------------------------------*
487200070503     c     SR_ges_bloc2  BegSR
487300070503      *
487400070503     c                   clear                   Fnlrg2ds
487500070503     c                   eval      ilrg2tla = 'L'
487600070503     c                   eval      ilrg2tch = 'R'
487700070503     c                   eval      ilrg2aas = v1anns
487800070503     c                   eval      ilrg2lnp = v1lnps
487900070503     c                   eval      ilrg2nrs = v1nrss
488000070503     c                   eval      ilrg2nsp = v1nsps
488100070503     c                   eval      ilrg2cau = w_cau
488200070503
488300070503     c                   call      'FNLRG2R'
488400070503     c                   Parm                    kpjba
488500070503     c                   Parm                    Fnlrg2ds
488600070503
488700070503      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
488800070503     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
488900070503      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
489000070503     c                   eval      $bloccof3    = *on
489100070508      * prima di scrivere le note  scrivo il motivo del 2° blocco
489200070508     c                   clear                   FIDNB0DS
489300070508     c                   eval      db0no1 = note_blocco
489400070508     c                   exsr      sr_add_note
489500070503      * scrittura delle note
489600070503     c                   exsr      Sr_wrt_note
489700160512     c                   eval      $Note_scritte= *on
489800070503     c                   endif
489900070503
490000070503     c                   endsr
490100131206      *
490200131206      *---------------------------------------------------------------*
490300131206      * Controllo se la spedizione è in consegna                      *
490400131206      *---------------------------------------------------------------*
490500131206     c     CTR_specons   BegSR
490600131206      *
490700140204     c                   setoff                                       16
490800131206     c                   eval      $cons = *off
490900131206      * Aggancio bolle arrivi
491000160104     c     KSPED         chain     Fnarb000
491100131206     c                   If        %found(Fnarb01l)
491200131206      * se si tratta di una spedizione e non è stata consegnata
491300131206     c                   If        ARBfbc = *blanks
491400131206     c                             and  ARBddc > *zeros
491500131206     c                             and  ARBndc > *zeros
491600160317     c                             and  ARBndc <> *all'8'
491700160317     c                             and  ARBndc <> *all'9'
491800131206     c                             and (ARBdcm = *zero    or
491900131206     c                                  ARBica = *blanks  or  ARBica = 'R'
492000131206     c                              or  ARBicc = *blanks  or  ARBicc = 'R')
492100131206     c                   eval      $cons = *on
492200131206     c                   Endif
492300070328      *
492400131206     c                   Endif
492500140123      *
492600131206     c                   endsr
492700131206      *
492800040212      *---------------------------------------------------------------*
492900040212      * Routine conferma riga di dettaglio reclamo                    *
493000040212      *---------------------------------------------------------------*
493100040218     c     SR_CONFE      BegSR
493200150423      *
493300150423      * Se sono in immissione o in gestione di una riga appena inserita
493400150423      *   re-imposto la richiesta della window W04.
493500150423     c                   If        Wmod = 'I'
493600150423     c                   clear                   $GesW04
493700160128     c                   else
493800160128     c                   clear                   $GesW04N
493900150423     c                   EndIf
494000040407      *
494100040218      * se sono in immissione e non in gestione di una riga già inserita
494200040218      * pulisco il record
494300040218     c                   If        Wmod = 'I' and WRK_recsbf = ' '
494400040218     c                   clear                   fitgd000
494500040218     c                   endif
494600150603
494700150603     c                   eval      tgdprt = v1cprt                              priorità dettaglio
494800150625      *
494900041213      * se non sono in immissione verifico se è cambiato qualcosa dalla testa \del reclamo
495000041213      *
495100160217     c                   movel     TGDorr        w0040
495200041213     c                   If        Wmod <> 'I' and (tgdtco <> v1ctco or
495300160217     c                             tgddar <> wdar   or  w0040 <> v1corr or
495400041213     c                             tgdksc <> v1cchi or tgdrsc <> v1crsc or
495500041213     c                             tgdclv <> v1cclv)
495600041213     c                   eval      mod_testa = 'S'
495700041213     c                   endif
495800151202      *
495900151202      * -?Memorizzazione  immissione R.A. con Motivo Apertura " 18"?
496000151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
496100151202      *         ?& ho modificato il motivo in " 18" (Reso)?
496200151203      *  ?=> Non sarà più possibile premere F3?
496300151202     c                   clear                   $WrtTIIDC
496400151202if  1c                   If        Wmod    = 'I'           and
496500151202     c                             V1Cmad  = c_MotAp_Reso  and
496600151202     c                             ( wrk_RecSbf = *blank   or
496700151202     c                             ( wrk_RecSbf = 'Y'     and
496800151202     c                               V1Cmad    <> TGDmad ) )
496900151202     c                   eval      $WrtTIIDC = *on
497000151203     c                   eval      $RA_MA_18 = *on
497100151202e   1c                   EndIf
497200040218      *
497300040218      * oggetto
497400040218     c                   eval      tgdtor = v1ctor                              tipo oggetto reclamo
497500040218     c                   if        v1ctor = 'S'
497600040218     c                   movel     spevid        tgdogg                         spedizione
497700040218     c                   endif
497800040218     c                   if        v1ctor = 'O'
497900040218     c                   movel     ormvid        tgdogg                         ORM
498000040218     c                   endif
498100040218     c                   if        v1ctor = 'G'
498200040218     c                   movel     v1cogg        tgdogg                         generico
498300040218     c                   endif
498400040218      * motivo apertura
498500040218     c                   eval      tgdmad = v1cmad
498600141216      *
498700151216      /free
498800151216
498900151216         // -?Richiesta Risposta (Avanzamento Fase)?
499000151216         Select;
499100160207
499200160121           // -?Utente di Sede su R.A. con Attesa di Risposta?
499300160121           //  ?- CASO IMPOSSIBILE !!! -?
499400160121           When  *in05  and  dTGDfar.§TGDatt = 'S';
499500160121             // ·?Storicizzazione Variazioni Richiesta Risposta?
499600160121             wTMO = 'A';
499700160121             clear dTGMa;
499800160121             dTGMa.§MODattO = dTGDfar.§TGDatt;
499900160121             dTGMa.§MODrisO = dTGDfar.§TGDris;
500000160121             dTGMa.§MODfilO = dTGD01.§TGDfilR;
500100160121             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
500200160121             exsr  WRT_Modifiche;
500300160121             // ·?Valorizzazione campi di FITGD?
500400160121             clear  dTGDfar.§TGDatt;
500500160121             clear  dTGDfar.§TGDris;
500600160121             clear  V1CrepR;
500700160207             V1CfilR = *zero;
500800160207
500900160207           // -?R.A. "chiusa" con Attesa Risposta = "S"?
501000160207           //                ?e Ricevuta Risposta = " "?
501100160129           When  wMod           <> 'I'     and
501200160129                 V1Ccdt         <> *blank  and
501300160129                 dTGDfar.§TGDatt = 'S'     and
501400160129                 dTGDfar.§TGDris = *blank  and
501500160201                 V1CfilR         > *zero   and
501600160201                 V1CrepR        <> *blank;
501700151216             // ·?Storicizzazione Variazioni Richiesta Risposta?
501800151216             wTMO = 'A';
501900151216             clear dTGMa;
502000151216             dTGMa.§MODattO = dTGDfar.§TGDatt;
502100151216             dTGMa.§MODrisO = dTGDfar.§TGDris;
502200151216             dTGMa.§MODfilO = dTGD01.§TGDfilR;
502300151216             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
502400151223             dTGMa.§MODattN = 'S';
502500151216             dTGMa.§MODrisN = 'S';
502600160129             dTGMa.§MODfilN = %int( V1CfilR );
502700160129             dTGMa.§MODrepN = V1CrepR;
502800151216             exsr  WRT_Modifiche;
502900151216             // ·?Valorizzazione campi di FITGD?
503000151223             dTGDfar.§TGDatt = 'S';
503100151216             dTGDfar.§TGDris = 'S';
503200151216             clear  V1Ccdt;
503300151216             if  wMod = 'I';
503400160121               //V1Cpog = W4filR;  ?(già così)?
503500160121               //V1Crpg = W4repR;  ?(già così)?
503600151216             else;
503700151223               V1Cpog = V1CfilR;
503800151223               V1Crpg = V1CrepR;
503900151216             endif;
504000160207
504100160121           // -?R.A. NON chiusa con Attesa Risposta = "S"?
504200160121           //                  ?e Ricevuta Risposta = " "?
504300160121           //  ?e variati Filiale e/o Reparto che Attende Risposta?
504400160121           When  wMod           <> 'I'     and
504500160121                 V1Ccdt          = *blank  and
504600160121                 dTGDfar.§TGDatt = 'S'     and
504700160121                 dTGDfar.§TGDris = *blank  and
504800160121                 ( V1CfilR      <> %editc( dTGD01.§TGDfilR : 'X' )  or
504900160121                   V1CrepR      <> dTGR01A.§TGRrepR );
505000160121             // ·?Storicizzazione Variazioni Richiesta Risposta?
505100160121             wTMO = 'A';
505200160121             clear dTGMa;
505300160121             dTGMa.§MODattO = dTGDfar.§TGDatt;
505400160121             dTGMa.§MODrisO = dTGDfar.§TGDris;
505500160121             dTGMa.§MODfilO = dTGD01.§TGDfilR;
505600160121             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
505700160129             if  V1CfilR > *zero  and  V1CrepR <> *blank;
505800160129               dTGMa.§MODattN = 'S';
505900160129             endif;
506000160121             dTGMa.§MODfilN = %int( V1CfilR );
506100160121             dTGMa.§MODrepN = V1CrepR;
506200160121             exsr  WRT_Modifiche;
506300160121             // ·?Valorizzazione campi di FITGD?
506400160129             if  V1CfilR > *zero  and  V1CrepR <> *blank;
506500160129               dTGDfar.§TGDatt = 'S';
506600160129             else;
506700160129               clear  dTGDfar.§TGDatt;
506800160129             endif;
506900160129             clear  dTGDfar.§TGDris;
507000160129             clear  V1Ccdt;
507100160121             //V1Cpog = V1CfilR;   ?(da lasciare com'è)?
507200160121             //V1Crpg = V1CrepR;   ?(da lasciare com'è)?
507300160207
507400160105           // -?R.A. NON chiusa con Attesa Risposta = "S"?
507500160105           //                    ?e Ricevuta Risposta = "S"?
507600160105           When  wMod           <> 'I'     and
507700160105                 V1Ccdt          = *blank  and
507800160129                 dTGDfar.§TGDatt = 'S'     and
507900160129                 dTGDfar.§TGDris = 'S'     and
508000160201                 V1CfilR         > *zero   and
508100160201                 V1CrepR        <> *blank;
508200160105             // ·?Storicizzazione Variazioni Richiesta Risposta?
508300160105             wTMO = 'A';
508400160105             clear dTGMa;
508500160105             dTGMa.§MODattO = dTGDfar.§TGDatt;
508600160105             dTGMa.§MODrisO = dTGDfar.§TGDris;
508700160105             dTGMa.§MODfilO = dTGD01.§TGDfilR;
508800160105             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
508900160105             dTGMa.§MODattN = 'S';
509000160105             dTGMa.§MODfilN = %int( V1CfilR );
509100160105             dTGMa.§MODrepN = V1CrepR;
509200160105             exsr  WRT_Modifiche;
509300160105             // ·?Valorizzazione campi di FITGD?
509400160105             dTGDfar.§TGDatt = 'S';
509500160105             clear  dTGDfar.§TGDris;
509600160105             clear  V1Ccdt;
509700160107             //V1Cpog = V1CfilR;   ?(NON in NON chiusura)?
509800160107             //V1Crpg = V1CrepR;   ?(NON in NON chiusura)?
509900160207
510000160105           // -?R.A. con Attesa Risposta variata?
510100151223           When  ( V1CfilR         > *zero   and
510200160201                   V1CrepR        <> *blank  and
510300160105                   dTGDfar.§TGDatt = *blank ) OR
510400151223                 ( V1CfilR        <= *zero   and
510500160201                   V1CrepR         = *blank  and
510600160105                   dTGDfar.§TGDatt = 'S' );
510700160104             // ·?Storicizzazione Variazioni Ricevuta Risposta?
510800151222             wTMO = 'A';
510900151222             clear dTGMa;
511000151222             dTGMa.§MODattO = dTGDfar.§TGDatt;
511100151222             dTGMa.§MODrisO = dTGDfar.§TGDris;
511200151222             dTGMa.§MODfilO = dTGD01.§TGDfilR;
511300151222             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
511400160201             if  V1CfilR > *zero  and  V1CrepR <> *blank;
511500151223               dTGMa.§MODattN = 'S';
511600151222               if  wMod = 'I';
511700151222                 dTGMa.§MODfilN = %int( W4filR );
511800151222                 dTGMa.§MODrepN = W4repR;
511900151222               else;
512000151223                 dTGMa.§MODfilN = %int( V1CfilR );
512100151223                 dTGMa.§MODrepN = V1CrepR;
512200151222               endif;
512300151222             endif;
512400151222             exsr  WRT_Modifiche;
512500151222             // ·?Valorizzazione campi di FITGD?
512600160201             if  V1CfilR > *zero  and  V1CrepR <> *blank;
512700151223               dTGDfar.§TGDatt = 'S';
512800160105             else;
512900160105               clear  dTGDfar.§TGDatt;
513000151223             endif;
513100151222             clear  dTGDfar.§TGDris;
513200160207
513300151223           // -?R.A. inserita (aperta) & cliente in attesa di Risposta?
513400160216           When  wMod     = 'I'    and  V1Ccdt  = *blank  and
513500160216                 W4tgdATT = 'S'    and
513600160216                 W4filR   > *zero  and  W4repR <> *blank;
513700151223             // ·?Storicizzazione inserimento di R.A. (aperta) con?
513800151223             //  ?Richiesta Risposta?
513900160216             wTMO = 'A';
514000160216             clear  dTGMa;
514100160216             // ·?Meglio inserire anche i dati già esistenti:?
514200160216             //  ?potrei aver ora modificato la filiale e/o il?
514300160216             //  ?reparto della R.A. appena inserita (F6) ma NON?
514400160216             //  ?ancora confermata (F4)?
514500160216             dTGMa.§MODattO = dTGDfar.§TGDatt;
514600160216             dTGMa.§MODrisO = dTGDfar.§TGDris;
514700160216             dTGMa.§MODfilO = dTGD01.§TGDfilR;
514800160216             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
514900160216             dTGMa.§MODattN = W4tgdATT;
515000160216             dTGMa.§MODfilN = %int( W4filR );
515100160216             dTGMa.§MODrepN = W4repR;
515200160216             exsr  WRT_Modifiche;
515300151223             // ·?Valorizzazione campi di FITGD?
515400151216             dTGDfar.§TGDatt = W4tgdATT;
515500160108             clear  dTGDfar.§TGDris;
515600160207
515700160207           // -?R.A. chiusa dalla filiale in Attesa Risposta?
515800160207           //  ?(con Risposta Ricevuta)?
515900160207           When  wMod           <> 'I'     and
516000160207                 V1Ccdt         <> *blank  and
516100160207                 dTGDfar.§TGDatt = 'S'     and
516200160207                 dTGDfar.§TGDris = 'S';
516300160207             clear  dTGDfar.§TGDatt;
516400160207             clear  dTGDfar.§TGDris;
516500160207             V1CfilR = *zero;
516600160207             clear  V1CrepR;
516700160207
516800151223           // -?Cliente NON in attesa di Risposta?
516900150108           When  dTGDfar.§TGDris = *blank  and
517000160201                 V1CfilR        <= *zero   and
517100160201                 V1CrepR         = *blank;
517200151216             clear  dTGDfar.§TGDatt;
517300160108             clear  dTGDfar.§TGDris;
517400160207             V1CfilR = *zero;
517500160207             clear  V1CrepR;
517600160207
517700151216         EndSl;
517800151216
517900151216         TGDfar = dTGDfar;
518000151216
518100151216      /end-free
518200151216      *
518300040218      * chiusura
518400040406      * se sono in manutenzione e modifico la causale di chiusura scrivo delle note
518500150622      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
518600150622      ***?già impostato.?
518700040406     c                   if        tgdcdt <> *blank and v1ccdt <> tgdcdt
518800040406     c                             and Wmod <> 'I'
518900150318     c                   eval      wtmo = 'C'
519000150318     c                   clear                   dTGMc
519100150318     c                   eval      §MODcdto = TGDcdt
519200150318     c                   eval      §MODcdtn = V1Ccdt
519300150318     c                   exsr      WRT_modifiche
519400040406     c                   endif
519500040406      *
519600040218      * se è la prima volta che chiudo imposto anche data,ora,p.o. e profilo chiusura
519700040406      *
519800151214if  1c                   if        tgdcdt = *blank and v1ccdt <> *blank
519900151214if  2c                   if        Wmod = 'I'
520000040304      * imposto ora e data inserimento chiamata  se sono in immissione
520100040304     c                   eval      tgddcd = w1cdar                              data chiusura
520200150324     c                   eval      tgdocd = %dec(%time())                       ora chiusura
520300040304
520400040304     c                   else
520500150603      * se sono in manutenzione e modifico la causale di chiusura
520600040304      * recupero ora e data chiusura se sono in manutenzione
520700150625      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
520800150625      ***?già impostato.?
520900040304     c                   time                    w0140
521000040304
521100040304     c                   clear                   WLBdat
521200040304     c                   move      w0140         g02dat
521300040304     c                   call      'XSRDA8'
521400040304     c                   parm                    WLBdat
521500040304     c                   eval      tgddcd = g02inv                              data chiusura
521600040304     c                   movel     w0140         tgdocd                         ora chiusura
521700151214e   2c                   endif
521800040304
521900040617     c                   eval      tgdpct = v1cpot                              p.o. chiusura
522000040218     c                   eval      tgdutc = KNMUS                               utente chiusura
522100040304
522200151214e   1c                   endif
522300151216      *
522400040223      * se codice chiusura è blanks e prima non lo era pulisco i campi del file
522500040223     c                   if        tgdcdt <> *blank and v1ccdt = *blank
522600040223     c                   clear                   tgddcd                         data chiusura
522700040223     c                   clear                   tgdocd                         ora chiusura
522800040223     c                   clear                   tgdpct                         p.o. chiusura
522900040223     c                   clear                   tgdutc                         utente chiusura
523000040223     c                   endif
523100040223      *
523200040223     c                   eval      tgdcdt = v1ccdt                              codice chiusura
523300040218
523400150316      * se utente a video non è l'utente del job
523500150316      * priorità a '1'
523600150316      * imposto priorità a '0'
523700150313     c                   IF        TGDprt = '1' and
523800150316     c                             V1Crpg <> KNMUS
523900150313     c                   eval      TGDprt = '0'
524000150313     c                   ENDIF
524100150316      * se sono in manutenzione e variato utente scrivo file variazioni
524200160111      *    (o anche se variata filiale gestione)
524300160111     c*//                if        TGDrpg <> V1Crpg and wmod <> 'I'
524400160111     c                   if          wMOD   <> 'I'     AND
524500160111     c                             ( TGDrpg <> V1Crpg  or
524600160111     c                               TGDpog <> %int(V1Cpog) )
524700150316     c                   eval      wtmo = 'U'
524800150316     c                   clear                   dTGMu
524900150316     c                   eval      §MODuteo = TGDrpg
525000150316     c                   eval      §MODpogo = TGDpog
525100150316     c                   eval      §MODuten = V1Crpg
525200160121     c                   eval      §MODpogn = %dec(V1Cpog:3:0)
525300150316     c                   exsr      WRT_modifiche
525400040618     c                   endif
525500040618      *
525600150423     c                   eval      tgdpog = %dec(v1cpog:3:0)                    filiale gestione
525700150423     c                   eval      tgdrpg = v1crpg                              utente  gestione
525800160119     c                   if        V1Ccdt =  *blanks  and
525900160119     c                             V1Cpog <> c_Sede   and
526000160119     c                             ( $Reparto         or
526100160201     c                             ( V1CfilR  > *zero and
526200160201     c                               V1CrepR <> *blank ) )
526300150422     c                   eval      tgdRPR = V1Crpg                              reparto gestione
526400150422     c                   endif
526500040618     c                   eval      tgdrsp = v1crsp                              responsabilità
526600040618      *
526700040407      * P.O. responsabile
526800040407      * se sono in manutenzione e modifico il p.o.responsabile scrivo delle note
526900040407      *
527000040407     c                   if        tgdpor <> *zeros and W_por  <> tgdpor
527100040407     c                             and Wmod <> 'I'
527200150318     c                   eval      wtmo = 'R'
527300150318     c                   clear                   dTGMr
527400150318     c                   eval      §MODporo = TGDpor
527500150318     c                   eval      §MODporn = W_por
527600150318     c                   exsr      WRT_modifiche
527700040407     c                   endif
527800040407      *
527900040218     c                   eval      tgdpor = W_por                               P.O. responsabile
528000040213      *
528100151214if  1c                   If        wmod = 'I'
528200040213     c                   eval      tgddar = wdar                                dta ricez. comunicaz
528300040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
528400040217     c                   eval      tgdtco = v1ctco                              tipo comunicazione
528500040213      *
528600040213     c                   eval      tgdtch = v1ctch                              tipo chiamamte
528700040213     c                   eval      tgdclv = v1cclv                              categoria cliente
528800040213     c                   eval      tgdrsc = v1crsc                              ragione sociale
528900041209     c                   eval      tgdksc = v1cchi                              codice cliente
529000120919      * se il motivo di apertura lo richiede NON memorizzo il KSC
529100120919     c                   IF        §MARksc = 'N'
529200120919     c                   clear                   TGDksc
529300120919     c                   ENDIF
529400040218     c                   eval      tgdpot = v1cpot                              p.o. immette recla
529500040421     c                   eval      tgdant = v1cant                              Anno   reclamo
529600040218     c                   eval      tgdnut = v1cnut                              Numero reclamo
529700040218     c                   eval      tgdpdt = v1cpdt                              progressivo dettagli
529800151214e   1c                   endif
529900041213      * solo in immissione valorizzo i seguenti campi
530000040407     c                   if        wmod = 'I' and wrk_recsbf = ' '
530100040218     c                   eval      tgddac = w1cdar                              data inserimento
530200150324     c                   eval      tgdorc = %dec(%time())                       ora inserimento
530300040218     c                   eval      tgdpru = KNMUS                               utente inserimento
530400040407     c                   endif
530500150326      /free
530600151215         // -?Flag Operativi?
530700160107         clear  dTGD01.§TGDfilR;
530800160207         if  TGDcdt = *blank  and  dTGDfar.§TGDatt = 'S'
530900160207                              and  V1CfilR > *zero;
531000160129           dTGD01.§TGDfilR = %int( V1CfilR );
531100151215         endif;
531200160107         TGDflo = dTGD01;
531300151215
531400150326       //?Storicizzo Variazioni Stato
531500150326         IF  W_prt <> TGDprt;
531600150326           wtmo = 'S';
531700150326           clear dTGMs;
531800150326           §MODprto = W_prt;
531900150326           §MODprtn = TGDprt;
532000150326           exsr WRT_modifiche;
532100150326         ENDIF;
532200150326      /end-free
532300040213      *
532400040309
532500040309      * se sono in immissione e non in gestione di una riga già inserita
532600040309      * valorizzo i seguenti campi
532700040309
532800040309     c                   If        Wmod = 'I' and WRK_recsbf = ' '
532900040218     c                   write     fitgd000
533000040218     c                   FEOD      fitgd01l
533100040218     c                   add       1             rec_wrt
533200041210      * aggiungo 1 al numeratore
533300041210     c                   add       1             sv_pdt
533400040218      *
533500040218     c                   else
533600040218
533700040218     c                   update    fitgd000
533800040218      *
533900040218     c                   endif
534000151201      *
534100151201      * -?Immissione R.A. con Motivo Apertura " 18":?
534200151201      *  ?=> Generazione automatica della "Disposizione di Reso"?
534300151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
534400151202      *         ?& ho modificato il motivo in " 18" (Reso)?
534500151202     c                   If        $WrtTIIDC = *on
534600151201     c                   exsr      sr_Wrt_TIIDC
534700151202e   1c                   EndIf
534800141216      *
534900141216      * -?Immissione R.A. con Motivo Apertura " 60":?
535000141216      *  ?SE NON sono in modifica di una spedizione nella stessa telefonata?
535100141216if  1c                   If        Wmod       = 'I'      and
535200141216     c                             V1Cmad     = c_VPINC  and
535300141216     c                             WRK_recsbf = *blank
535400141216      *
535500141216      * ·?=> Registrazione automatica dell'esito nelle Note?
535600141216     c                   eval      not_PINcode = 'S'
535700141216     c                   exsr      WRT_note
535800141216      * ·?=> Registrazione automatica dell'esito nei Riferimenti PINcode?
535900141216      *?· Impostazione dati della spedizione nella ds per TGRRIF?
536000141216     c                   clear                   dTGR01
536100141216     c                   if        oPINfxx = *blank  and  oPINerr = *blank
536200141216     c                   eval      dTGR01.§TGResito = 'P'
536300141216     c                   else
536400141216     c                   eval      dTGR01.§TGResito = 'N'
536500141216     c                   endif
536600141216     c                   eval      dTGR01.§TGRfgs   = ARBifp
536700141216     c                   eval      dTGR01.§TGRnfv   = ARBndc
536800141216     c                   eval      dTGR01.§TGRdfv   = ARBddc
536900141216     c                   eval      dTGR01.§TGRpdr   = ARBpdc
537000141216      *?· Impostazione dati nel file FITGR00F?
537100141216     c                   clear                   FITGR000
537200141216     c                   eval      TGRpot = V1Cpot                              Fil. immette tel.
537300141216     c                   eval      TGRant = V1Cant                              Anno   telefonata
537400141216     c                   eval      TGRnut = V1Cnut                              Numero telefonata
537500141216     c                   eval      TGRpdt = V1Cpdt                              Progr. Dettaglio
537600141216     c                   eval      TGRtri = 'D'                                 Tipo Riferimento
537700141216     c                   eval      TGRrif = dTGR01
537800141216     c                   write     FITGR000
537900141216     c                   feod      FITGR01L
538000141216      *
538100141216e   1c                   EndIf
538200040213
538300040213      * verifico se esistono per questa riga che confermo dei record di riferimenti
538400040213      * ed evantualmente li deleto e poi riscrivo
538500040213
538600040421     c     ktgran        setll     fitgr01l
538700040213      *
538800040213     c                   do        *hival
538900040421     c     ktgran        reade     fitgr01l
539000040213      *
539100040219     c                   if        not %eof(fitgr01l)
539200040213     c                   delete    fitgr01l
539300040213     c                   else
539400040213     c                   leave
539500040213     c                   endif
539600040213      *
539700040213     c                   enddo
539800151214      *
539900040213      * se esiste referente
540000040213     c                   if        v1refe <> *blank
540100040213     c                   clear                   fitgr000
540200040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
540300040421     c                   eval      tgrant = v1cant                              Numero reclamo
540400040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
540500040213     c                   eval      tgrtri = 'R'                                 tipo  riferimento
540600040213     c                   movel     v1refe        tgrrif
540700040213     c                   write     fitgr000
540800040219     c                   feod      fitgr01l
540900040213     c                   endif
541000040213      * se esiste indirizzo e-mail
541100040213     c                   if        v1mail <> *blank
541200040213     c                   clear                   fitgr000
541300040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
541400040421     c                   eval      tgrant = v1cant                              Numero reclamo
541500040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
541600040213     c                   eval      tgrtri = 'M'                                 tipo  riferimento
541700040213     c                   movel     v1mail        tgrrif
541800040213     c                   write     fitgr000
541900040219     c                   feod      fitgr01l
542000040213     c                   endif
542100040213      * se esiste numero telefono e/o numero fax
542200040213     c                   if        v1ntel <> *blank or v1nfax <> *blank
542300040213     c                   clear                   fitgr000
542400040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
542500040421     c                   eval      tgrant = v1cant                              Numero reclamo
542600040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
542700040213     c                   eval      tgrtri = 'N'                                 tipo  riferimento
542800040213     c                   movel     v1ntel        tgrrif
542900040213     c                   move      v1nfax        tgrrif
543000040213     c                   write     fitgr000
543100040219     c                   feod      fitgr01l
543200040213     c                   endif
543300151215      * -?se Richiesta Risposta?
543400151215     c                   eval      TGRtri = 'A'
543500151215     c     keyFITGR01    setll     FITGR000
543600151215     c                   if        %equal(FITGR01L)
543700151215     c     keyFITGR01    delete    FITGR000
543800151215     c                   endif
543900151215     c                   clear                   wTGRpdt
544000160204     c                   If        V1CfilR  > *zero  and
544100160204     c                             V1CrepR <> *blank
544200160129     c*//?NON serve:?    clear                   dTGR01A
544300160129     c                   eval      dTGR01A.§TGRrepR = V1CrepR
544400151214     c                   clear                   FITGR000
544500151214     c                   eval      TGRpot = V1Cpot                              Fil. immiss. recl.
544600151214     c                   eval      TGRant = V1Cant                              Anno   reclamo
544700151214     c                   eval      TGRnut = V1Cnut                              Numero reclamo
544800151215     c                   eval      TGRpdt = TGDpdt                              Progr. reclamo
544900151214     c                   eval      TGRtri = 'A'                                 Tipo  riferimento
545000151214     c                   eval      TGRrif = dTGR01A
545100151214     c                   write     FITGR000
545200151214     c                   feod      FITGR01L
545300151214     c                   EndIf
545400040217
545500070413      * confermo le note se sono in immissione e non ho scritto nulla precedentemente
545600040217
545700160512     c****               If        Wmod = 'I' and $bloccof3 = *off
545800160512     c                   If        Wmod = 'I' and $Note_scritte= *off
545900070413     c                   clear                   FIDNB0DS
546000040421     c                   z-add     v1cant        iB0ant
546100040217     c                   z-add     v1cnut        iB0nut
546200040217     c                   z-add     v1cpdt        iB0pdt
546300040217     c                   movel     'GEN'         iB0tIN
546400141217     c                   if        WRK_RecSbf = *blank
546500141216     c                   z-add     999           iB0pgn
546600141217     c                   endif
546700040217     c                   movel     v1not1        db0no1
546800040217     c                   movel     v1not2        db0no2
546900040217     c                   movel     v1not3        db0no3
547000040217     c                   movel     v1not4        db0no4
547100040617     c                   eval      ib0pot =v1cpot
547200041013     c                   eval      db0pon =Dutpou
547300040617     c                   eval      db0utn =knmus
547400040218      * se non è mai stato chiamato imposto 'W'
547500040218     c                   if        wfidnb0r = 'Y'
547600040217     c                   movel     'C'           iB0flm
547700040218     c                   else
547800040218     c                   movel     'W'           iB0flm
547900040220
548000040218     c                   endif
548100040217      *
548200040217     C                   CALL      'FIDNB0R'
548300040217     C                   PARM                    kpjba
548400040217     C                   PARM                    FIDNB0DS
548500040217      *
548600040305     c                   endif
548700040305
548800070413      * modalità manutenzione
548900040305
549000070424    1c                   If         Wmod = 'G' or
549100070424     c                             (Wmod = 'I' and $bloccof3 = *on)
549200040305      * nel caso in cui sia stato richiamato il pgm fidnb0r per gestire le note
549300040305      * non lo richiamo + per confermarle
549400070413    2c                   if        wfidnb0r <> 'Y'
549500040305     C                   clear                   FIDNB0DS
549600040421     c                   z-add     v1cant        iB0ant
549700040305     c                   z-add     v1cnut        iB0nut
549800040305     c                   z-add     v1cpdt        iB0pdt
549900040305     c                   movel     'GEN'         iB0tIN
550000040305     c                   movel     'W'           iB0flm
550100040305     c                   eval      ib0pot =v1cpot
550200041013     c                   eval      db0pon =Dutpou
550300040305     c                   eval      db0utn =knmus
550400040305      * valorizzo il progressivo riga  scrivo la prima riga
550500040305    3c                   if        *in51 = *off and V1not1 <> *blanks
550600040305     c                   z-add     1             Ib0pgn
550700040305     c                   movel     v1not1        db0no1
550800040305      *
550900040305     C                   CALL      'FIDNB0R'
551000040305     C                   PARM                    kpjba
551100040305     C                   PARM                    FIDNB0DS
551200040305    3c                   endif
551300040305
551400040305      * valorizzo il progressivo riga  scrivo la seconda riga
551500040305    3c                   if        *in52 = *off and V1not2 <> *blanks
551600040305     c                   z-add     2             Ib0pgn
551700040305     c                   movel     v1not2        db0no1
551800040305      *
551900040305     C                   CALL      'FIDNB0R'
552000040305     C                   PARM                    kpjba
552100040305     C                   PARM                    FIDNB0DS
552200040305    3c                   endif
552300040305      * valorizzo il progressivo riga  scrivo la terza   riga
552400040305    3c                   if        *in53 = *off and V1not3 <> *blanks
552500040305     c                   z-add     3             Ib0pgn
552600131017     c                   movel     v1not3        db0no1
552700040305      *
552800040305     C                   CALL      'FIDNB0R'
552900040305     C                   PARM                    kpjba
553000040305     C                   PARM                    FIDNB0DS
553100040305    3c                   endif
553200040305      * valorizzo il progressivo riga  scrivo la quarta  riga
553300040305    3c                   if        *in54 = *off and V1not4 <> *blanks
553400040305     c                   z-add     4             Ib0pgn
553500040305     c                   movel     v1not4        db0no1
553600040305      *
553700040305     C                   CALL      'FIDNB0R'
553800040305     C                   PARM                    kpjba
553900040305     C                   PARM                    FIDNB0DS
554000040305    3c                   endif
554100040305
554200040305    2c                   endif
554300040305
554400040305    1c                   endif
554500070402
554600140205      * se impostata data consegna   o   se previste note LDV obbligatorie
554700131119      * => devo scrivere alcune note R.A. e note ldv
554800070402
554900140205    1c                   if        not_ldv = 'S'  or  §MARnotLdV = 'S'
555000070403      * se devo inserire delle note indica che ho fatto delle modifiche
555100070403      * alla data consegna e quindi impedisco l'uso del f3
555200070403     c                   eval      $bloccof3  =  *on
555300070404      * recupero le note da passare al pgm note ldv
555400070404     c                   clear                   nob
555500070404     c                   clear                   x
555600070402      * recupero le note R.A. dal file e le passo al PGM note LDV
555700070402     c     ktgn          setll     fitgn01l
555800070402     c                   do        *hival
555900070402     c     ktgn          reade     fitgn01l
556000070402
556100070402     c                   if        %eof(fitgn01l)
556200070402     c                   leave
556300070402     c                   endif
556400070402      * passo solo le prime 8(qui non ci sono ancora le modifiche registrate)
556500070402     c                   if        x < 8
556600070402     c                   add       1             x
556700070402     c                   movea     tgnnot        nob(x)
556800070402     c                   else
556900070402     c                   leave
557000070402     c                   endif
557100070402
557200070402     c                   enddo
557300070402
557400131119      * richiamo il programma note LdV in  immissione
557500131206     C                   clear                   Fnlrn6DS
557600131206     c                   eval      in6mod  = 'I'
557700131206     c                   eval      in6aas  = v1anns
557800131206     c                   eval      in6lnp  = v1lnps
557900131206     c                   eval      in6nrs  = v1nrss
558000131206     c                   eval      in6nsp  = v1nsps
558100070402      * note
558200131206     c                   movel     nob(1)        in6not1
558300131206     c                   move      nob(2)        in6not1
558400131206     c                   movel     nob(3)        in6not2
558500131206     c                   move      nob(4)        in6not2
558600131206     c                   movel     nob(5)        in6not3
558700131206     c                   move      nob(6)        in6not3
558800131206     c                   movel     nob(7)        in6not4
558900131206     c                   move      nob(8)        in6not4
559000070402     c
559100131206     c                   eval      in6pgm  = 'FIDNA1R'
559200131206      * se sono una spedizione in consegna passo i dati della distinta del file ARB
559300131206     c                   If        $cons  = *on
559400160104     c     KSPED         chain     Fnarb000
559500131206     c                   If        %found(Fnarb01l)
559600131206     c                   eval      in6fgs = arbifp
559700131206     c                   eval      in6nfv = arbndc
559800131206     c                   eval      in6dfv = arbddc
559900131206     c                   eval      in6pdr = arbpdc
560000131206     c                   eval      in6cons = 'S'
560100131206     c                   Endif
560200131206     c                   Endif
560300070402
560400131206     c                   call      'FNLRN6R'
560500070402     c                   parm                    kpjba
560600131206     c                   parm                    FNLRN6DS
560700070402
560800070402     c                   clear                   not_ldv
560900070412    1c                   endif
561000070402
561100070402      * se devo registrare delle note di modifica data consegna
561200070402      * richiesta scrivo delle note mie nelle note R.A.
561300070402      * Le Faccio uguali a quelle della Rita ?????
561400070402
561500070402      * Decodifico tipo consegna richiesta
561600070402
561700070412    1c                   If        v1cdcr > 0
561800070402     c                   select
561900070402
562000070402     c                   when      v1ctcr = 'P'
562100070404     c                   eval      W_dtcr   ='PRIMA DEL'
562200070402     c                   when      v1ctcr = 'D'
562300070404     c                   eval      W_dtcr   ='DOPO  IL '
562400070402     c                   other
562500070404     c                   eval      W_dtcr   ='      IL '
562600070402
562700070402     c                   endsl
562800070402
562900070402      * creo le note R.A.
563000070402     c                   clear                   nob
563100070402     c                   clear                   x
563200070402     c                   eval      nob(1)='Cons.Rich:'
563300070402     c                   eval      nob(1)=%trim(nob(1))+%trim(w_dtcr)
563400070402     c                   eval      nob(1)=%trim(nob(1))+' '+
563500070402     c                             %editw(v1cdcr:'  /  /    ')
563600141031    2c                   if        v1cocr>0
563700141031     c                   eval      nob(1)=%trim(nob(1))+' ore'
563800141031    3c                   if        v1ctcr=*blanks
563900141031     c                   eval      nob(1)=%trim(nob(1))+' '+
564000141031     c                             %editw(v1cocr:'  :  ')
564100141031     c                   else
564200141031     c                   eval      nob(2)=
564300141031     c                             %editw(v1cocr:'  :  ')
564400141031    3c                   endif
564500141031    2c                   endif
564600070402
564700070402     c                   clear                   not_dcr
564800070402
564900070402      * se valorizzate anche le note DCR con il msg del 48 scrivo
565000070402      * il msg in note R.A.
565100070412    2c                   if        note_dcr <> *blanks
565200141031     c                   if        v1ctcr = *blanks
565300070402     c                   movea     note_dcr      nob(2)
565400141031     c                   else
565500141031     c                   eval      %subst(nob(2):7:29)= %subst(note_dcr:1:29)
565600141031     c                   eval      nob(3) = %subst(note_dcr:30:35)
565700141031     c                   eval      nob(4) = %subst(note_dcr:65:14)
565800141031     c                   endif
565900070402      * visto che il messaggio è lungo 78 dovrebbe riempire 3 note
566000070402     c                   clear                   note_dcr
566100070412    2c                   endif
566200070402      * richiamo in scrittura il pgm delle note
566300070402     c                   clear                   FIDNB0DS
566400070402     c                   eval      db0no1 = nob(1)
566500070402     c                   eval      db0no2 = nob(2)
566600070402     c                   eval      db0no3 = nob(3)
566700070402     c                   eval      db0no4 = nob(4)
566800070402
566900070402     c                   z-add     v1cant        iB0ant
567000070402     c                   z-add     v1cnut        iB0nut
567100070402     c                   z-add     v1cpdt        iB0pdt
567200070404     c                   z-add     999           ib0pgn
567300070402     c                   movel     'GEN'         iB0tIN
567400070402     c                   eval      ib0pot =v1cpot
567500070402     c                   eval      db0pon =Dutpou
567600070402     c                   eval      db0utn =knmus
567700070402      * imposto 'W'
567800070402     c                   movel     'W'           iB0flm
567900070402      *
568000070402     c                   call      'FIDNB0R'
568100070402     c                   parm                    kpjba
568200070402     c                   parm                    fidnb0ds
568300070402
568400070412    1c                   endif
568500140506      *
568600070403      * se c'è stata almeno una riga che ha fatto modificare una data consegna richiesta
568700070403      * impedisco l'uso del tasto funzionale F3
568800070403     c                   if        $bloccof3  =  *on
568900070403     c                   seton                                        63
569000070403     c                   endif
569100140506      *
569200140506      * se è stata inserita almeno una R.A. con Motivo Apertura " 17"
569300151203      *                                   o con Motivo Apertura " 18"
569400141210      *                                   o con Motivo Apertura " 60"
569500140506      * impedisco l'uso del tasto funzionale F3
569600141210     c                   if        $RA_MA_17  =  *on  or
569700151203     c                             $RA_MA_18  =  *on  or
569800141210     c                             $RA_MA_60  =  *on
569900140506     c                   seton                                        63
570000140506     c                   endif
570001170505      * richiamo in scrittura il pgm delle note in caso di immissione disposizioni
570002170505      * di giacenza
570003170505     c                   if        note_gia <> *blank
570004170508     c                   clear                   nob
570005170505     c                   movea     note_gia      nob(1)
570006170505     c                   clear                   FIDNB0DS
570007170505     c                   eval      db0no1 = nob(1)
570008170505     c                   eval      db0no2 = nob(2)
570009170505     c                   eval      db0no3 = nob(3)
570010170505     c                   eval      db0no4 = nob(4)
570011170505
570012170505     c                   z-add     v1cant        iB0ant
570013170505     c                   z-add     v1cnut        iB0nut
570014170505     c                   z-add     v1cpdt        iB0pdt
570015170505     c                   z-add     999           ib0pgn
570016170505     c                   movel     'GEN'         iB0tIN
570017170505     c                   eval      ib0pot =v1cpot
570018170505     c                   eval      db0pon =Dutpou
570019170505     c                   eval      db0utn =knmus
570020170505      * imposto 'W'
570021170505     c                   movel     'W'           iB0flm
570022170505      *
570023170505     c                   call      'FIDNB0R'
570024170505     c                   parm                    kpjba
570025170505     c                   parm                    fidnb0ds
570026170505     c                   clear                   note_gia
570027170505
570028170505     c                   endif
570029170505
570100040213      *
570200070402
570300040213     c                   ENDSR
570400151201      /free
570500151201
570600151201       //--------------------------------------------------------------
570700151201       //?Scrittura record in TIIDC00F per Reso/Dirottamento.          ?
570800151201       //--------------------------------------------------------------
570900151201       BEGSR  sr_Wrt_TIIDC;
571000151201
571100151201           if  Not %open(TIIDC02L);
571200151201             open  TIIDC02L;
571300151201           endif;
571400151201
571500151201           // -?Reperimento eventuale capofila £6 della FGS?
571600151201           clear  FNLV55ds;
571700151201           d55tpt = '6';
571800151201           if  $FNARB = *on;
571900151201             d55lin = ARBlna;
572000151201           else;
572100151201             d55lin = BLPlna;
572200151201           endif;
572300151201           d55drf = %dec( %date() );
572400151201           FNLV55R ( FNLV55ds );
572500151202
572600151202           // -?Reperimento progressivo richiesta per la "Disposizione di Reso"?
572700151202           exsr  sr_Rep_IDCprg;
572800151201
572900151201           // -?Impostazione campi di TIIDC00F?
573000151201           clear  TIIDC000;
573100151201
573200151201if  2      If  $Ok_IDCprg;
573300151201             IDCprg = k_IDCprg;
573400151201           endif;
573500151201           IDCinsData = %int( %subst( %char( %dec( %timestamp() ) )
573600151201                                      : 1 : 14 ) );
573700151201           IDCmodAcc  = 'B';
573800151201           if  V1Ctch = 'D';
573900151201             IDCtipoCli = V1Ctch;
574000151201           else;
574100151201             IDCtipoCli = 'M';
574200151201           endif;
574300151201           if  $FNARB = *on;
574400151201             IDCwaas  = ARBaas;
574500151201             IDCwlnp  = ARBlnp;
574600151201             IDCwnrs  = ARBnrs;
574700151201             IDCwnsp  = ARBnsp;
574800151201             IDCaas   = ARBaas;
574900151201             IDClnp   = ARBlnp;
575000151201             IDCnrs   = ARBnrs;
575100151201             IDCnsp   = ARBnsp;
575200151201             //IDCfgs   = ARBlna;     ?(vedi capofile £6)?
575300151201           else;
575400151201             IDCwaas  = BLPaas;
575500151201             IDCwlnp  = BLPlnp;
575600151201             IDCwnrs  = BLPnrs;
575700151201             IDCwnsp  = BLPnsp;
575800151201             IDCaas   = BLPaas;
575900151201             IDClnp   = BLPlnp;
576000151201             IDCnrs   = BLPnrs;
576100151201             IDCnsp   = BLPnsp;
576200151201             //IDCfgs   = BLPlna;     ?(vedi capofile £6)?
576300151201           endif;
576400151201           IDCfgs     = d55tfa;
576500151201           IDCtipoDis = '5';
576600151201           IDCnewLna  = *zero;
576700151201           IDCfla     = 'R';
576800151201           IDCelaFlg  = 'S';
576900151201           IDCelaUte  = kNmUs;
577000151201
577100151201           // -?Scrittura record in TIIDC00F?
577200151201           //______________
577300151201           write  TIIDC000;
577400151201           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
577500151201
577600151201e   1  ENDSR;
577700151201
577800151201       //--------------------------------------------------------------
577900151201       //?Reperimento progressivo per disposizione di Reso/Dirottamento?
578000151201       //--------------------------------------------------------------
578100151201       BEGSR  sr_Rep_IDCprg;
578200151201
578300151201         clear  $Ok_IDCprg;
578400151201         clear  keyTIIDC02;
578500151201
578600151201         clear  TRUL33ds;
578700170113         //*·I33tla = 'L';
578800151201         clear  I33ope;
578900151217         I33cnu = 651;
579000151201         I33num = 1;
579100151201
579200151201         DoW  Not  $Ok_IDCprg;
579300151201
579400151201           kpjbu = TRUL33ds;
579500151201           TRUL33R ( kpjba );
579600151201           TRUL33ds = kpjbu;
579700151201
579800151201           if  O33err <> *zeros;
579900151201             leavesr;
580000151201           endif;
580100151201
580200151201           w0090  = O33nri;
580300151217           // -?Così (???) NON viene estratto l'anno di 2 cifre !!!?
580400151217           //k_IDCprg = %int(
580500151217           //             %subst( %editc( %subdt( %date() : *years ) : 'X' )
580600151217           //               : 3 : 2 ) + %editc( w0090 : 'X' ) );
580700151217           w0040  = %subdt( %date() : *years );
580800151217           k_IDCprg = ( %int( %subst( %editc( w0040 : 'X' ) : 3 : 2 ) )
580900151217                        * 1000000000 ) + w0090;
581000151201           setll  %kds( keyTIIDC02 )  TIIDC000;
581100151201
581200151201           // -?Se numero già presente nel file TIIDC ne cerco un altro?
581300151201           if  Not %equal(TIIDC02L);
581400151201             $Ok_IDCprg = *on;
581500151201           endif;
581600151201
581700151201         EndDo;
581800151201
581900151201       ENDSR;
582000151201
582100151201      /end-free
582200040213      *
582300040213      *---------------------------------------------------------------*
582400040213      * Routine caricamento record nel subfile                        *
582500040213      *---------------------------------------------------------------*
582600040213     c     SR_CARSBF     BegSR
582700040213
582800040216      * caricamento dettaglio reclami
582900040217     c                   z-add     *zeros        s01nrr
583000040216
583100040421     c     ktgdan        setll     Fitgd01l
583200040216      *
583300040216     c                   Do        *hival
583400040422     c     ktgdan        reade(N)  fitgd01l
583500040216
583600040216     c                   if        %eof(fitgd01l)
583700040216     c                   leave
583800040216     c                   endif
583900040216
584000040216     c                   add       1             s01nrr
584100040218
584200040218     C
584300040218      * numero riga
584400040217     c                   eval      vs1pdt = tgdpdt
584500040216
584600040216      * - OGGETTO
584700040216
584800040220     c                   clear                   vs1ogg
584900040217
585000040216      * ---------- SPEDIZIONE -----------
585100040216
585200040217     c                   if        tgdtor = 'S'
585300040218      *
585400040218     c                   movel     tgdogg        WRKspe
585500040219     c                   eval      vs1ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
585600040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
585700040219     c                             ' ' + WRKann
585800040218      *
585900040216     c                   endif
586000040216
586100040216      * ----------    ORM     -----------
586200040216
586300040217     c                   if        tgdtor = 'O'
586400040218     c                   movel     tgdogg        WRKorm
586500040218      *
586600040218     c                   eval      vs1ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
586700040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
586800040218     c                             '/' + %trim(WRKnrv)
586900040218      *
587000040216     c                   endif
587100040216
587200040216      * ----------  GENERICO  -----------
587300040216
587400040217     c                   if        tgdtor = 'G'
587500040218     c                   EVAL      VS1ogg = TGDogg
587600040216     c                   endif
587700040218
587800040218      * MOTIVO RECLAMO
587900040218
588000040218     c                   eval      vs1mad = tgdmad
588100040218      * Controllo
588200040218     c                   clear                   tibs02ds
588300040218     C                   MOVEL     'C'           t02mod
588400040218     C                   MOVEL     knsif         t02sif
588500040218     C                   MOVEL     'MAR'         t02cod
588600040218     C                   MOVEL     vs1mad        t02ke1
588700040218      *
588800040218     C                   CALL      'TIBS02R'
588900040218     C                   PARM                    KPJBA
589000040218     C                   PARM                    TIBS02DS
589100040218      *
589200040218    4C                   IF        t02err = *BLANKS
589300040218     C                   movel     T02uni        DMAR
589400040218     C                   movel     §MARdesc      vs1dmr
589500040218     c                   endif
589600141215      *
589700141215      * -?Se il motivo di apertura NON può essere usato: proteggo campo opz.?
589800141215     c                   eval      *in83 =((*in74 = *off  and  §MARutAp = 'N')
589900141215     c                                      or
590000141215     c                                     (wMod <> 'I'   and  §MARutUt = 'N'))
590100040218
590200040218      * CHIUSURA RECLAMO
590300040218
590400040218     c                   eval      vs1cdt = tgdcdt
590500040218
590600040218      * DATA CHIUSURA
590700040218
590800040220     c                   clear                   vs1dcr
590900040220
591000040218     C                   If        tgddcd > *zeros
591100040218     c                   z-add     tgddcd        data8
591200040218     c                   move      anno2         vs1dcr
591300040218     c                   move      mese          mmgg              4 0
591400040218     c                   movel     mmgg          vs1dcr
591500040218     c                   movel     giorno        vs1dcr
591600040218     c                   endif
591700040218
591800040218     c                   write     fia1s01
591900141215     c                   setoff                                       81  83
592000040218
592100040218     c                   enddo
592200040218      *
592300040218      * verifico se inserite righe ed accendo indicatori di emissione
592400040218      *
592500040218     c                   if        s01nrr > 0
592600040218     c                   seton                                        21
592700040218     c                   endif
592800040218
592900040218     c                   seton                                        20
593000040218
593100040213     c                   ENDSR
593200041209      *
593300041209      *---------------------------------------------------------------*
593400041209      * Routine recupero codice chiamante                             *
593500041209      *---------------------------------------------------------------*
593600041209     c     SR_RECCHI     BegSR
593700041209      *
593800041209     c                   clear                   Wrk_cchi
593900041209     c                   clear                   Wrk_cchiA
594000041209     c                   clear                   Wrk_cchiB
594100041209     c                   clear                   Wrk_cchiC
594200041209      *
594300041209      * rileggo il dettaglio
594400041209      *
594500041209
594600041209     c     ktgdan        setll     Fitgd01l
594700041209      *
594800041209     c                   Do        *hival
594900041209     c     ktgdan        reade(N)  fitgd01l
595000041209
595100041209     c                   if        %eof(fitgd01l)
595200041209     c                   leave
595300041209     c                   endif
595400041209
595500041209      * ---------- SPEDIZIONE -----------
595600041209
595700041209    2c                   if        tgdtor = 'S'
595800041209      *
595900041209     c                   movel     tgdogg        Wspevid
596000041230      * se non sono in sede
596100041230     c                   If        not *in05
596200041209      * Aggancio bolle arrivi
596300160104     c     KSPEDw        chain     Fnarb000
596400041209    3c                   If        %found(Fnarb01l)
596500041209      * mittente se valorizzato
596600041209     c                   move      arbccm        w0040
596700041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
596800041210     c                             w0040 <> 9999
596900041209     c                   z-add     arbccm        Wrk_cchiA
597000041209      * se trovato codice mittente prendo quello e scappo
597100041209     c                   leave
597200041209    4c                   endif
597300041209      *
597400041209     c                   z-add     arbksc        W0040
597500041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
597600041209     c                             w0040 <> 9999
597700041209      * in caso di CCM uguale a zeros valorizzo con KSC
597800041209     c                   z-add     arbksc        Wrk_cchiA
597900041209     c                   leave
598000041209    4c                   endif
598100041209      *
598200041209    3c                   endif
598300041209      * Aggancio bolle partenze se non ho trovato ancora nulla
598400160104     c     KSPEDw        chain     Fnblp000
598500041209    3c                   If        %found(Fnblp01l)
598600041209      * mittente se valorizzato
598700041210     c                   move      blpccm        w0040
598800041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
598900041210     c                             w0040 <> 9999
599000041209     c                   z-add     blpccm        Wrk_cchiA
599100041209      * se trovato codice mittente prendo quello e scappo
599200041209     c                   leave
599300041209    4c                   endif
599400041209      *
599500041209     c                   z-add     blpksc        W0040
599600041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
599700041209     c                             w0040 <> 9999
599800041209      * in caso di CCM uguale a zeros valorizzo con KSC
599900041209     c                   z-add     blpksc        Wrk_cchiA
600000041209     c                   leave
600100041209    4c                   endif
600200041209      *
600300041209    3c                   endif
600400041230      * sono in sede
600500041230     c                   else
600600041209      * Aggancio bolle di sede  se non ho trovato ancora nulla
600700041209     c     KSPEDw        chain     titas30c
600800041209    3c                   If        %found(Titas30c)
600900041209      * mittente se valorizzato
601000041209     c                   move      tasccm        w0040
601100041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
601200041210     c                             w0040 <> 9999
601300041209     c                   z-add     tasccm        Wrk_cchiA
601400041209      * se trovato codice mittente prendo quello e scappo
601500041209     c                   leave
601600041209    4c                   endif
601700041209      *
601800041209     c                   z-add     tasksc        W0040
601900041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
602000041209     c                             w0040 <> 9999
602100041209      * in caso di CCM uguale a zeros valorizzo con KSC
602200041209     c                   z-add     tasksc        Wrk_cchiA
602300041209     c                   leave
602400041209    4c                   endif
602500041209      *
602600041209    3c                   endif
602700041209      *
602800041230    3c                   endif
602900041230      *
603000041209    2c                   endif
603100041209
603200041209      * ----------    ORM     -----------
603300041209
603400041209    2c                   if        tgdtor = 'O'
603500041209     c                   movel     tgdogg        Wormvid
603600041209      *
603700160104     c     KORMw         chain     Fnorm000
603800041209    3c                   If        %found(Fnorm01l)
603900041209     c
604000041209      * ordinanante
604100041209     c                   movel     ormcor        w0070
604200041209     c                   move      w0070         w0040
604300041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
604400041209     c                             w0040 <> 9999  and Wrk_cchiA = 0
604500041209     c                   z-add     w0070         Wrk_cchiA
604600041209     c                   endif
604700041209
604800041209      * ritiro
604900041209     c                   movel     ormcra        w0070
605000041209     c                   move      w0070         w0040
605100041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
605200041209     c                             w0040 <> 9999 and Wrk_cchiB = 0
605300041209     c                   z-add     w0070         Wrk_cchiB
605400041209     c                   endif
605500041209
605600041209      * destinatario
605700041209     c                   movel     ormcrc        w0070
605800041209     c                   move      w0070         w0040
605900041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
606000041209     c                             w0040 <> 9999 and Wrk_cchiC = 0
606100041209     c                   z-add     w0070         Wrk_cchiC
606200041209     c                   endif
606300041209    3c                   endif
606400041209    2c                   endif
606500041209
606600041209      * ----------  GENERICO  -----------
606700041209      * leggo il successivo
606800041209    2c                   if        tgdtor = 'G'
606900041209     c                   iter
607000041209    2c                   endif
607100041209
607200041209     c                   enddo
607300041209
607400041209     c                   endsr
607500040203      *
607600040129      *---------------------------------------------------------------*
607700040212      * Routine pulizia  videata Immissione Reclamo  x nuovo reclamo  *
607800040129      *---------------------------------------------------------------*
607900040129     c     SR_PUL01      BegSR
608000040129      *
608100040421     C                   clear                   v1cant
608200040416     C                   clear                   v1cnut
608300040421      * accendo indicatore che non mette in inversione fondo e sottolineato il numero reclamo
608400040421     c                   seton                                        15
608500040421      *
608600040202     C                   clear                   v1ctco
608700040202     C                   clear                   v1dtco
608800040202     C                   clear                   v1cdar
608900040202     C                   clear                   v1corr
609000040202     C                   clear                   v1cchi
609100050118     C                   clear                   v1dchi
609200040202     C                   clear                   v1crsc
609300160701     C                   clear                   V1naz
609400160701     C                   clear                   V1prov
609500040129     C                   clear                   v1cclv
609600040129     C                   clear                   v1refe
609700040129     C                   clear                   v1ntel
609800040129     C                   clear                   v1mail
609900040129     C                   clear                   v1nfax
610000040218     C                   clear                   W_v1cchi
610100040219     C                   clear                   rec_wrt
610200041207     c                   clear                   Wrk_cchi
610300041207     c                   clear                   Wrk_cchiA
610400041207     c                   clear                   Wrk_cchiB
610500041207     c                   clear                   Wrk_cchiC
610600041207     c                   clear                   SW_v1cchi
610700070413     c                   eval      $bloccof3 = *off
610800160512     c                   eval      $Note_scritte = *off
610900140506     c                   eval      $RA_MA_17 = *off
611000151203     c                   eval      $RA_MA_18 = *off
611100141210     c                   eval      $RA_MA_60 = *off
611200151201     c                   clear                   $Ok_IDCprg
611300151202     c                   clear                   $WrtTIIDC
611400070403     c                   setoff                                       63
611500041210      * Imposto il numeratore
611600041210     c                   z-add     1             SV_pdt
611700040213      * pulizia video relativo oggetto
611800040213     c                   exsr      sr_pulctl
611900040213      *
612000040202      * pulizia subfile
612100040202     c                   clear                   s01nrr
612200150310      * accendo indicatore di posizionamento cursore su CHIAMANTE
612300150310     c                   seton                                        44
612400040213      * imposto il tipo comunicazione
612500040213     c                   eval      v1ctco = 'T'
612600040213      * decodifico
612700150625     c                   clear                   dCOR
612800040217     c                   clear                   tibs02ds
612900040213     C                   MOVEL     'C'           t02mod
613000040213     C                   MOVEL     knsif         t02sif
613100040213     C                   MOVEL     'COR'         t02cod
613200040213     C                   MOVEL     v1ctco        t02ke1
613300040213      *
613400040213     C                   CALL      'TIBS02R'
613500040213     C                   PARM                    KPJBA
613600040213     C                   PARM                    TIBS02DS
613700040213      *
613800040213    4C                   IF        t02err = *BLANKS
613900040213     C                   movel     T02uni        Dcor
614000040216     C                   movel     §CORdesc      v1dtco
614100040213      *
614200040213     C                   ELSE
614300040216     C                   clear                   v1dtco
614400040213    4C                   ENDIF
614500040203      *
614600040129     c                   endsr
614700040213      *
614800040213      *---------------------------------------------------------------*
614900040213      * Routine pulizia  videata Immissione Reclamo  x nuova riga     *
615000040213      *---------------------------------------------------------------*
615100040213     c     SR_PULctl     BegSR
615200040213      *
615300141215     c                   exsr      sr_PULctl2
615400040216      *
615500040216     c                   setoff                                       2120
615600040216     c                   seton                                        22
615700040216     c                   write     fia1c01
615800040216     c                   setoff                                       22
615900040213      *
616000040213     c                   ENDSR
616100141215      *
616200141215      *---------------------------------------------------------------*
616300141215      *
616400141215     c     SR_PULctl2    BegSR
616500141215      *
616600141215     C                   clear                   forz_rec
616700141215     C                   clear                   v1ctor
616800141215     C                   clear                   v1poeo
616900141215     C                   clear                   v1nsor
617000141215     C                   clear                   v1norm
617100141215     C                   clear                   v1nrvo
617200141215     C                   clear                   v1cogg
617300141215     C                   clear                   v1lnps
617400141215     C                   clear                   v1nrss
617500141215     C                   clear                   v1nsps
617600141215     C                   clear                   v1anns
617700141215     C                   clear                   v1ctch
617800141215     C                   clear                   v1dtch
617900141215     C                   clear                   W_cmad
618000150122     C                   clear                   wSaveMAD
618100141215     C                   clear                   v1cmad
618200141215     C                   clear                   v1dmad
618300141215     C                   clear                   v1ccdt
618400141215     C                   clear                   v1dcdt
618500141215     C                   clear                   v1not1
618600141215     C                   clear                   v1not2
618700141215     C                   clear                   v1not3
618800141215     C                   clear                   v1not4
618900150313     C                   clear                   v1crpg
619000150316     C                   clear                   v1cpog
619100150313     C                   clear                   v1cprt
619200150313     C                   clear                   v1dprt
619300141215     C                   clear                   v1crsp
619400141215     C                   clear                   v1cpor
619500141215     C                   clear                   v1cdcr
619600141215     C                   clear                   v1cocr
619700141215     C                   clear                   v1ctcr
619800141215     c                   clear                   v1ccva
619900141215     C                   clear                   v1dcva
620000151223     c                   clear                   V1CfilR
620100151223     c                   clear                   V1CrepR
620200160129     c                   clear                   V1Cafg
620300160129     c                   clear                   V1Carg
620400141215     c                   clear                   W_por
620500141215     C                   clear                   v1dpor
620600141215     C                   clear                   Wormvid
620700141215     C                   clear                   Wspevid
620800141215     C                   clear                   WRK_recsbf
620900141215     C                   clear                   WFIDNB0R
621000141215     c                   clear                   not_dcr
621100141215     c                   clear                   not_ldv
621200141215     c                   clear                   note_dcr
621300151217     c                   clear                   wSavePGN
621400141215      * pulisco i campi di lavoro
621500141215     c                   clear                   wforz
621600160601     c                   eval      $Note_scritte= *off
621700160601     c                   eval      $ric_fnlrg2r = *off
621800141215     c                   eval      $riemetti    = *off
621900141215     c                   eval      $fnarb       = *off
621901170220     c                   eval      *IN04        = *off
622000141217     c                   eval      $RA_MA_60    = *off
622100150326     c                   clear                   W_prt
622200160607      *
622300160215     c                   clear                   dTGD01
622400160215     c                   clear                   dTGDfar
622500160215     c                   clear                   dTGR01A
622600160607      *
622700141215      * accendo indicatore di emissione dell'oggetto generico
622800141215     c                   seton                                        03
622900141215     c                   setoff                                       0102
623000141215     c                   setoff                                       80
623100141215      * data consegna riochiesta
623200141215     c                   setoff                                       616074
623300141215      * pulizia indicatori di protezione
623400141215     c                   setoff                                       515253
623500141215     c                   setoff                                       54
623600141215      * spengo indicatore di note aggiuntive
623700141215     c                   setoff                                       29
623701170515      * spengo indicatore di gestione giacenza
623702170515     c                   setoff                                       04
623800150624      *
623900150624      * -?Spegnimento indicatore di protezione "Tipo Comunicazione"?
624000150624     c                   eval      *in73 = *off
624100151210      *
624200151210      * -?Spegnimento indicatori di gestione "Richiesta Risposta"?
624300151211     c                   eval      *in18 = *off
624400141215      *
624500141215      * imposto il numero progressivo riga solo se sono in immissione
624600141215     c                   if        wmod  = 'I'
624700141215      *
624800141215     c                   z-add     sv_pdt        v1cpdt
624900150313      * imposto la priorità a '1' quando sono in immissione
625000150313     c                   eval      V1Cprt = '1'
625100141215      * Decodifico
625200141215     c                   clear                   tibs02ds
625300141215     C                   MOVEL     'C'           t02mod
625400141215     C                   MOVEL     knsif         t02sif
625500141215     C                   MOVEL     'PRR'         t02cod
625600141215     C                   MOVEL     v1cprt        t02ke1
625700141215      *
625800141215     C                   CALL      'TIBS02R'
625900141215     C                   PARM                    KPJBA
626000141215     C                   PARM                    TIBS02DS
626100141215      *
626200141215     C                   if        t02err = *BLANKS
626300141215     C                   movel     T02uni        DPRR
626400141215     C                   movel     §PRRdesc      v1dprt
626500141215     c                   endif
626600160607      *
626700150313      * Imposto utente e filiale gestione
626800150313     c                   IF        w_pot > 0
626900150313     c                   eval      V1Cpog = %editc(w_pot:'X')
627000150313     c                   ELSE
627100150313     c                   eval      V1Cpog = %editc(DUTpou:'X')
627200150313     c                   ENDIF
627300150313     c                   eval      V1Crpg = KNMUS
627400141215      *
627500141215     c                   endif
627600141215      *
627700141215     c                   EndSR
627800141215      *
627900040218      *****************************************************************
628000040218      *  Gestione del subfile Manutenzione riga reclamo
628100040218      *****************************************************************
628200040218     c     SR_GESSBF     begsr
628300160607      *
628400040218     c                   do        *hival
628500160607      *
628600040218     c                   readc     fia1s01
628700150616     c                   eval      *in84 = *off
628800160607      *
628900161027     c                   If        %eof(fidnA1d)
629000040218     c                   leave
629100040218     c                   endif
629200160607      *
629300040218     c                   if        vs1opz = '2'
629400160607      *
629500150317      /free
629600150317       //?Controllo se l'utente è abiliato alla filiale gestione della RA da manutenzionare
629700150616       //?(SE NON SONO IN SEDE)
629800150317         *in84 = *off;
629900150317         chain(n) (V1Cant:V1Cnut:VS1pdt) FITGD01L;
630000150317         IF  %found(FITGD01L) and
630100150616             NOT *in05        and
630200150318             %lookup(%editc(TGDpog:'X'):sk_wFIL) = 0;
630300150317           *in84 = *on;
630400151223           *in28 = *on;
630500160225           //V1Cmsg = 'Richiesta NON gestibile dall''utente';
630600160225           V1Cmsg = Msg(24);
630700151223           clear  VS1opz;
630800150317           update FIA1S01;
630900150317           leave;
631000150317         ENDIF;
631100150724
631200150724         // -?Controllo anche se R.A. con Motivo Apertura = " 70"?
631300150724         //  ?(Reclamo da Sede) - in gestione ad una filiale?
631400150724         *in84 = *off;
631500150724         If  TGDmad = c_ReclSede  and  TGDpog <> %int(c_Sede)  and  *in05;
631600150724           *in84 = *on;
631700151223           *in28 = *on;
631800160225           //V1Cmsg = 'Richiesta NON gestibile dall''utente';
631900160225           V1Cmsg = Msg(24);
632000151223           clear  VS1opz;
632100150724           update  FIA1S01;
632200150724           leave;
632300150724         EndIf;
632400150317      /end-free
632500160607      *
632600040218      * valorizzo un campo di comodo per differenziare un recupero da subfile
632700040218      * da un recupero da richiamo da altro PGM
632800040218     c                   eval      WRK_recsbf = 'Y'
632900160607      *
633000040421     c                   eval      keyant = v1cant
633100040218     c                   eval      keynut = v1cnut
633200040218     c                   eval      keypdt = vs1pdt
633300040218     c                   exsr      SR_recogg
633400141215     c  n82              clear                   vs1opz
633500040218     c                   update    fia1s01
633600040218      * esco dal ciclo per la gestione del dettaglio scelto
633700160607      *
633800040218     c                   leave
633900040218     c                   endif
634000160607      *
634100040218     c                   enddo
634200040218      *
634300040218     c                   ENDSR
634400040218      *****************************************************************
634500040218      *  Recupero Dettaglio Reclamo in manutenzione
634600040218      *****************************************************************
634700040218     c     SR_RECOGG     begsr
634800040303
634900040303     c                   setoff                                       515253
635000040422     c                   setoff                                       547082
635100151223     c                   setoff                                       748384
635200150625      * -?Proteggo il Tipo Comunicazione SE sono in MODIFICA?
635300150625     c                   eval      *in73 = (wMod <> 'I')
635400150326
635500150326     c                   clear                   W_prt
635600151211     c                   clear                   dTGD01
635700151211     c                   clear                   dTGDfar
635800160126     c                   clear                   dTGR01A
635900040218      *
636000040422     c     Ktgdrec       chain(e)  fitgd01l
636100040218
636200040422    1c                   if        %error
636300040422      * emetto l'errore
636400040422     c                   seton                                        82
636500151223     c                   eval      *in28 = *on
636600160225     c*//                eval      V1Cmsg = 'Richiesta NON gestibile +
636700160225     c*//                                    dall''utente perchè R.A. +
636800160225     c*//                                    impegnata da altro utente'
636900160225     c                   eval      V1Cmsg = Msg(25)
637000040422     c                   else
637100040422      *
637200151210   1Bc                   if        %found(fitgd01l)
637201170517     c                   setoff                                       04
637300040218
637400040218      * numero riga
637500040218     c                   eval      v1cpdt = tgdpdt
637600040218
637700040218      * - OGGETTO
637800040218
637900040218     c                   eval      v1ctor = tgdtor
638000040218
638100040218      * ---------- SPEDIZIONE -----------
638200040218
638300040218    2c                   if        tgdtor = 'S'
638400040218     c                   movel     tgdogg        spevid
638500040218     c                   seton                                        01
638600040218     c                   setoff                                       0203
638700140204     c  n05              exsr      CTR_specons
638701170517     c                   exsr      SR_ctrgiacenza
638800040218      *
638900040218    2c                   endif
639000040218
639100040218      * ----------    ORM     -----------
639200040218
639300040218    2c                   if        tgdtor = 'O'
639400040218     c                   movel     tgdogg        ormvid
639500040218     c                   seton                                        02
639600040218     c                   setoff                                       0103
639700040218    2c                   endif
639800040218
639900040218      * ----------  GENERICO  -----------
640000040218
640100040218    2c                   if        tgdtor = 'G'
640200040218     c                   movel     tgdogg        v1cogg
640300040218     c                   seton                                        03
640400040218     c                   setoff                                       0102
640500040218    2c                   endif
640600040218      * chi chiama
640700040218     c                   eval      v1ctch = tgdtch
640800040304     c                   clear                   tibs02ds
640900040304     C                   MOVEL     'C'           t02mod
641000040304     C                   MOVEL     knsif         t02sif
641100040304     C                   MOVEL     'CCR'         t02cod
641200040304     C                   MOVEL     v1ctch        t02ke1
641300040304      *
641400040304     C                   CALL      'TIBS02R'
641500040304     C                   PARM                    KPJBA
641600040304     C                   PARM                    TIBS02DS
641700040304      *
641800040304     C                   movel     T02uni        DCCR
641900040304     c                   movel     §ccrdesc      v1dtch
642000040218      * motivo
642100040218     c                   eval      v1cmad = tgdmad
642200150410     c                   eval      wSaveMAD = V1Cmad
642300040218      * Decodifica
642400040218     c                   clear                   tibs02ds
642500040218     C                   MOVEL     'C'           t02mod
642600040218     C                   MOVEL     knsif         t02sif
642700040218     C                   MOVEL     'MAR'         t02cod
642800040218     C                   MOVEL     v1cmad        t02ke1
642900040218      *
643000040218     C                   CALL      'TIBS02R'
643100040218     C                   PARM                    KPJBA
643200040218     C                   PARM                    TIBS02DS
643300040218      *
643400040218    2C                   IF        t02err = *BLANKS
643500040218     C                   movel     T02uni        DMAR
643600040218     C                   movel     §MARdesc      v1dmad
643700070323      * se è un motivo di apertura che deve essere protetto in manutenzione accendo il 74
643800070323     c                   if        §marprot = 'S'
643900070323     c                   eval      *in74 = *on
644000070323     c                   endif
644100070323
644200040218    2c                   endif
644300040218      * chiusura reclamo
644400040218     c                   eval      v1ccdt = tgdcdt
644500040218      * Decodifica
644600040218     c                   clear                   tibs02ds
644700040218     C                   MOVEL     'C'           t02mod
644800040218     C                   MOVEL     knsif         t02sif
644900040218     C                   MOVEL     'CHR'         t02cod
645000040218     C                   MOVEL     v1ccdt        t02ke1
645100040218      *
645200040218     C                   CALL      'TIBS02R'
645300040218     C                   PARM                    KPJBA
645400040218     C                   PARM                    TIBS02DS
645500040218      *
645600040218    2C                   IF        t02err = *BLANKS
645700040218     C                   movel     T02uni        DCHR
645800040218     C                   movel     §CHRdesc      v1dcdt
645900040218    2c                   endif
646000150624      *
646100040218      * recupero le note
646200040218      *
646300040218     C                   clear                   FIDNB0DS
646400040421     c                   z-add     v1cant        iB0ant
646500040218     c                   z-add     v1cnut        iB0nut
646600040218     c                   z-add     v1cpdt        iB0pdt
646700040218     c                   movel     'GEN'         iB0tIN
646800040218     c                   movel     'R'           iB0flm
646900040218      *
647000040218     C                   CALL      'FIDNB0R'
647100040218     C                   PARM                    kpjba
647200040218     C                   PARM                    FIDNB0DS
647300040218      *
647400040218      * valorizzo le prime 4 note a video
647500040218      *
647600040218     c                   movel     DB0NO1        v1not1
647700040218     c                   movel     DB0NO2        v1not2
647800040218     c                   movel     DB0NO3        v1not3
647900040218     c                   movel     DB0NO4        v1not4
648000040309      * in caso di manutenzione non do la possibilità di modifcare niente
648100040309     c                   if        Wmod = 'G'
648200040309     c                   seton                                        515253
648300040309     c                   seton                                        54
648400040309     c                   ENDIF
648500070413      * se è un motivo di apertura che deve essere protetto in manutenzione proteggo le note
648600070413     c                   if        §marprot = 'S' and wmod = 'I'
648700070413     c                   eval      *in51 = db0no1 <> *blanks
648800070413     c                   eval      *in52 = db0no2 <> *blanks
648900070413     c                   eval      *in53 = db0no3 <> *blanks
649000070413     c                   eval      *in54 = db0no4 <> *blanks
649100070413     c                   endif
649200040218      *
649300040218      * se ob0piu è valorizzato ci sono + di 4 righe
649400040218     c                   eval      *in29 = (OB0PIU = '+')
649500151217      *
649600151217      * -?Memorizzo ULTIMO Progressivo Note inserito per la R.A.?
649700151217     c                   clear                   wSavePGN
649800151217     c     kTGN          setGT     FITGN000
649900151217     c     kTGN          readPE    FITGN000
650000151217     c                   if        NOT %eof(FITGN01L)
650100151217     c                   eval      wSavePGN = TGNpgn
650200151217     c                   endif
650300151217      *
650400040218      * reparto gestione
650500150312     c                   eval      v1cpog = %editc(tgdpog:'X')
650600040218     c                   eval      v1crpg = tgdrpg
650700040218      * priorità
650800040218     c                   eval      v1cprt = tgdprt
650900150326     c                   eval      W_prt  = tgdprt
651000150313      * se non è chiuso imposto utente e filiale da azute
651100150313      * e priorità a '1' se è a '0'
651200160129if  2c                   IF        V1Ccdt = *blanks
651300150313     c                   eval      V1Crpg = KNMUS
651400160129     c                   if        w_POt  = *zero
651500160129     c                   eval      V1Cpog = %editc( DUTpou : 'X' )
651600160129     c                   else
651700160129     c                   eval      V1Cpog = %editc( w_POt : 'X' )
651800160129     c                   endif
651900150313     c                   IF        V1Cprt = '0'
652000150313     c                   eval      V1Cprt = '1'
652100150313     c                   ENDIF
652200160129e   2c                   ENDIF
652300040323      *
652400040323      * Decodifico
652500040323     c                   clear                   tibs02ds
652600040323     C                   MOVEL     'C'           t02mod
652700040323     C                   MOVEL     knsif         t02sif
652800040323     C                   MOVEL     'PRR'         t02cod
652900040323     C                   MOVEL     v1cprt        t02ke1
653000040323      *
653100040323     C                   CALL      'TIBS02R'
653200040323     C                   PARM                    KPJBA
653300040323     C                   PARM                    TIBS02DS
653400040323      *
653500040323    4C                   IF        t02err = *BLANKS
653600040323     C                   movel     T02uni        DPRR
653700040323     C                   movel     §PRRdesc      v1dprt
653800040323     c                   endif
653900150625      *
654000040218      * responsabilità
654100040218     c                   eval      v1crsp = tgdrsp
654200040218      *
654300040218      * P.O. responsabilità
654400040218     c                   movel     tgdpor        v1cpor
654500040218      * decodifica
654600040218    2c                   if        tgdpor > 0
654700160104     C     tgdpor        chain     azorg
654800040218    3C                   if        %found(azorg01l)
654900040218     C                   movel     orgdes        v1dpor
655000040218    3c                   endif
655100040218    2c                   endif
655200151211      *
655300160104      * -?Richiesta/Ricevuta Risposta?
655400151214     c                   if        TGDflo <> *blank
655500160218if  3c                   if        %subst( TGDflo : 2 : 3) > *zero
655600151211     c                   eval      dTGD01  = TGDflo
655700160218x   3c                   else
655800160218     c                   eval      dTGD01.§TGDaRap =
655900160218     c                             %subst( TGDflo : 1 : 1)
656000160218e   3c                   endif
656100151214     c                   endif
656200151211     c                   eval      dTGDfar = TGDfar
656300040218
656400151210   1Bc                   endif
656500160108
656600160108      /free
656700160108
656800160128         // -?Filiale che Attende Risposta (o che l'ha già Ricevuta)?
656900160108         clear  V1CfilR;
657000160128         if  dTGD01.§TGDfilR > *zero;
657100160128           V1CfilR = %editc( dTGD01.§TGDfilR : 'X' );
657200160128         endif;
657300160108
657400160128         // -?Reparto che Attende Risposta (o che l'ha già Ricevuta)?
657500160128         clear  V1CrepR;
657600160128         TGRtri = 'A';
657700160128         chain(N)  ( TGDant : TGDnut : TGDpdt : TGRtri )  FITGR000;
657800160128         if  %found(FITGR01L);
657900160128           dTGR01A = TGRrif;
658000160128         endif;
658100160128         V1CrepR = dTGR01A.§TGRrepR;
658200160108
658300160128         // -?Visualizzazione "Attesa o Ricevuta Risposta"?
658400160128         clear  V1AttRis;
658500160128         Select;
658600160203           When  dTGDfar.§TGDatt = 'S'  and  dTGDfar.§TGDris = 'S';
658700160201             exsr  sr_ReperimentoRispostaRicevuta;
658800160201             if  WrepR  = *blank;
658900160203               V1AttRis = 'Risposta di Ritorno   RICEVUTA   dal Reparto '
659000160203                        + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
659100160203                        + %editc( dTGD01.§TGDfilR : 'X' );
659200160201             else;
659300160203               V1AttRis = 'Risposta di Ritorno   RICEVUTA   dall''Utente '
659400160201                        + %trimR( WrepR ) + ' della Filiale '
659500160201                        + WfilR;
659600160201             endif;
659700160128           When  dTGDfar.§TGDatt = 'S';
659800160203             V1AttRis = 'Risposta di Ritorno   ATTESA   dal Reparto '
659900160203                      + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
660000160203                      + %editc( dTGD01.§TGDfilR : 'X' );
660100160128         EndSl;
660200160129
660300160129         // -?Visualizzazione Reparto/Filiale che ha attualmente in?
660400160129         //  ?gestione la R.A.?
660500160129         //If  dTGDfar.§TGDatt = 'S';
660600160129           if  V1Crpg <> TGDrpg;
660700160129              V1Carg = '(' + %trimR( TGDrpg ) + ')';
660800160129           endif;
660900160129           if  V1Cpog <> %editc( TGDpog : 'X' );
661000160129              V1Cafg = '(' + %editc( TGDpog : 'X' ) + ')';
661100160129           endif;
661200160129         //EndIf;
661300160108
661400160108      /end-free
661500151211
661600040324      * posizionamento cursore su causale chiusura
661700040324     c                   seton                                        37
661800040324      * Se reclamo chiuso controllo la data chiusura se sono passati più dei
661900040324      * giorni massimi per variare un reclamo chiuso (definiti nella tabella
662000040324      * dati standard REC) proteggo tutti i campi ed emetto un msg di avvertimento
662100041012      * Se utente abilitato a modificare tutto (esempio uff. qualità) non faccio il
662200041012      * controllo (*in07 acceso)
662300041012     c                   if        tgddcd > *zeros and *in50 and not *in07
662400040426
662500040324     c     *iso          movel     tgddcd        Wdataeur
662600040426     c                   z-add     0             giorni
662700040426      *
662800040426     c                   dow       giorni < §recnmgm
662900040426     c                   adddur    1:*d          Wdataeur
663000040426     c                   extrct    wdataeur:*y   kann
663100040426     c                   extrct    wdataeur:*m   kmes
663200040426     c                   extrct    wdataeur:*d   wgiorno
663300040426     c                   clear                   ktfa
663400040426     c                   eval      ktfp = tgdpot
663500160104     c     kazcln        chain     azcln000
663600040426     c                   if        not%found(azcln01l)
663700040426     c                   leave
663800040426     c                   endif
663900040426     c                   if        MAT(wgiorno) =  'F'
664000040426     c                             or  POM(wgiorno) =  'F'
664100040426     c                   iter
664200040426     c                   else
664300040426     c                   add       1             giorni
664400040426     c                   endif
664500040426     c                   enddo
664600040426      *
664700040324     c                   if        wdataeur < wdataoggi
664800040324      * se non sono in immissione proteggo tutti i campi
664900040324     c                   eval      *in70 = *on
665000151223     c                   eval      *in28 = *on
665100151223     c                   eval      V1Cmsg = 'Richiesta chiusa non è più +
665200151223     c                                       modificabile'
665300040324      * posizionamento cursore su opzione subfile
665400141215      * ·?SE non protetta?
665500141215     c                   eval      *in81 = ((*in74 = *on or  §MARutAp <> 'N')
665600141215     c                                       and
665700141215     c                                      (wMod = 'I'  or  §MARutUt <> 'N'))
665800040324     c                   eval      *in37 = *off
665900040324      *
666000040324     c                   endif
666100040324      *
666200040324     c                   endif
666300040422      *
666400151210    1c                   endif
666500150319
666600150319      /free
666700150319       //?Controllo se attivare o no il tasto F11=Variazioni
666800150319         *in17 = *off;
666900150319         chain (V1Cant:V1Cnut:VS1pdt) FITGM01L;
667000150319         IF  %found(FITGM01L);
667100150319           *in17 = *on;
667200150319         ENDIF;
667300150319      /end-free
667400150319
667500040218      *
667600040218     c                   ENDSR
667700160201
667800160201      /free
667900160201
668000160201       BEGSR  sr_ReperimentoRispostaRicevuta;
668100160201
668200160201         clear  WrepR;
668300160201         clear  WfilR;
668400160201
668500160204         setGT  ( V1Cant : V1Cnut : V1Cpdt : 'A' )  FITGM000;
668600160204         readPE ( V1Cant : V1Cnut : V1Cpdt : 'A' )  FITGM000;
668700160201
668800160204         if  %eof( FITGM01L );
668900160201           leavesr;
669000160201         endif;
669100160201
669200160201         WrepR = TGMutn;
669300160201         WfilR = %editc( TGMpon : 'X' );
669400160201
669500160201       ENDSR;
669600160201
669700160201      /end-free
669800160128
669900040219      *---------------------------------------------------------------*
670000040219      * Routine chiusura chiamata                                     *
670100040219      *---------------------------------------------------------------*
670200040219     c     SR_CHIUDO     BegSR
670300040219
670400040219      * per ogni record di dettaglio reclamo aggiorno i dati della parte alta
670500040219      * del video
670600040219
670700040421     c     Ktgdan        setll     Fitgd01l
670800040219      *
670900040219     c                   Do        *hival
671000160216     c     Ktgdan        reade(E)  fitgd01l
671100040219
671200160216      * -?Record allocato?
671300160216     c                   if        %error
671400160216     c     kTGDan        reade(N)  FITGD000
671500160216     c                   iter
671600160216     c                   endif
671700160216      * -?End Of File?
671800040219     c                   if        %eof(fitgd01l)
671900040219     c                   leave
672000040219     c                   endif
672100040219
672200040219     c                   eval      tgddar = wdar                                dta ricez. comunicaz
672300040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
672400040219     c                   eval      tgdtco = v1ctco                              tipo comunicazione
672500040219     c                   eval      tgdrsc = v1crsc                              rag.sociale chiamant
672600041213      * se codice cliente diverso da prima scrivo una nota
672700041214     c                   if        tgdksc <> v1cchi and mod_testa = 'S'
672800150318     c                   eval      wtmo = 'K'
672900150318     c                   clear                   dTGMk
673000150318     c                   eval      §MODksco = TGDksc
673100150318     c                   eval      §MODkscn = V1Cchi
673200150318     c                   exsr      WRT_modifiche
673300041213     c                   endif
673400041213
673500041231     c                   eval      tgdksc = v1cchi                              codice cliente
673600040219     c                   eval      tgdclv = v1cclv                              categoria chiamante
673700121008
673800121008      * se il motivo di apertura lo richiede NON memorizzo il KSC
673900121008     c                   clear                   tibs02ds
674000121008     c                   eval      T02mod = 'C'
674100121008     c                   eval      T02sif = knsif
674200121008     c                   eval      T02cod = 'MAR'
674300121008     c                   eval      T02ke1 = TGDmad
674400121008     c                   call      'TIBS02R'
674500121008     c                   parm                    KPJBA
674600121008     c                   parm                    TIBS02DS
674700121008     c                   eval      dMAR = T02uni
674800121008     c                   IF        §MARksc = 'N'
674900121008     c                   clear                   TGDksc
675000121008     c                   ENDIF
675100150625      *
675200150625      * -?SE premuto F4=Fine chiamata (abilitato SOLO in immissione)?
675300150625      *  ? & R.A. chiusa?
675400150625      *  ? & Tipo Comunicazione con Stato (ex Priorità) in tab. "COR"?
675500150625      *  ?=> lo imposto in TGDPRT?
675600150625     c                   clear                   wSavePRT
675700150625     c                   if             *inKD
675800150625     c                             and  TGDcdt    <> *blanks
675900150625     c                   eval      wSavePRT = TGDprt
676000150625     c                   if             §CORstato <> *blanks
676100150625     c                   eval      TGDprt = §CORstato
676200150625     c                   else
676300150625     c                   eval      TGDprt = '9'
676400150625     c                   endif
676500150625      /free
676600150625         // -?Storicizzo Variazioni Stato?
676700150625         IF  wSavePRT <> TGDprt;
676800150625           wtmo = 'S';
676900150625           clear dTGMs;
677000150625           §MODprto = wSavePRT;
677100150625           §MODprtn = TGDprt;
677200150625           exsr WRT_modifiche;
677300150625         ENDIF;
677400150625      /end-free
677500150625     c                   endif
677600040219
677700040219     c                   update    fitgd000
677800040219
677900040219     c                   enddo
678000040219
678100040219      * verifico se esistono per questa riga che confermo dei record di riferimenti
678200040219      * ed evantualmente li deleto e poi riscrivo
678300040219
678400040421     c     ktgran        setll     fitgr01l
678500040219      *
678600040219     c                   do        *hival
678700040421     c     ktgran        reade     fitgr01l
678800040219      *
678900040219     c                   if        not %eof(fitgr01l)
679000040219     c                   delete    fitgr01l
679100040219     c                   else
679200040219     c                   leave
679300040219     c                   endif
679400040219      *
679500040219     c                   enddo
679600040219      * se esiste referente
679700040219     c                   if        v1refe <> *blank
679800040219     c                   clear                   fitgr000
679900040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
680000040421     c                   eval      tgrant = v1cant                              Numero reclamo
680100040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
680200040219     c                   eval      tgrtri = 'R'                                 tipo  riferimento
680300040219     c                   movel     v1refe        tgrrif
680400040219     c                   write     fitgr000
680500040219     c                   endif
680600040219      * se esiste indirizzo e-mail
680700040219     c                   if        v1mail <> *blank
680800040219     c                   clear                   fitgr000
680900040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
681000040421     c                   eval      tgrant = v1cant                              Numero reclamo
681100040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
681200040219     c                   eval      tgrtri = 'M'                                 tipo  riferimento
681300040219     c                   movel     v1mail        tgrrif
681400040219     c                   write     fitgr000
681500040219     c                   endif
681600040219      * se esiste numero telefono e/o numero fax
681700040219     c                   if        v1ntel <> *blank or v1nfax <> *blank
681800040219     c                   clear                   fitgr000
681900040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
682000040421     c                   eval      tgrant = v1cant                              Numero reclamo
682100040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
682200040219     c                   eval      tgrtri = 'N'                                 tipo  riferimento
682300040219     c                   movel     v1ntel        tgrrif
682400040219     c                   move      v1nfax        tgrrif
682500040219     c                   write     fitgr000
682600040219     c                   endif
682700040219      *
682800040219     c                   ENDSR
682900040219      *
683000040216      *****************************************************************
683100040216      *  Recupero Reclamo in manutenzione
683200040216      *****************************************************************
683300040218     c     SR_RECUPERO   begsr
683400041213      *
683500041213     c                   clear                   mod_testa
683600151202     c                   clear                   $WrtTIIDC
683700150625      *
683800150625      * -?In MODIFICA: proteggo SEMPRE il Tipo Comunicazione?
683900150625     c                   eval      *in73  = *on
684000040302
684100040302      * preparo la chiave con i dati passati
684200040302
684300040421     c                   eval      keyant = ia0ant
684400040302     c                   eval      keynut = ia0nut
684500040302     c                   eval      keypdt = ia0pdt
684600040304     c                   eval      v1cpdt = ia0pdt
684700040304     c                   eval      v1cnut = ia0nut
684800040421     c                   eval      v1cant = ia0ant
684900040304      * carico il subfile
685000040304     c                   exsr      SR_CARSBF
685100040304
685200040422      * dettaglio
685300040422     c                   exsr      SR_recogg
685400040422
685500040302      * valorizzo anche la parte alta della videata
685600040302
685700040303     c                   eval      v1cpot = tgdpot
685800040303     c                   eval      v1ctco = tgdtco
685900040303      * decodifico
686000150625     c                   clear                   dCOR
686100040303     c                   clear                   tibs02ds
686200040303     C                   MOVEL     'C'           t02mod
686300040303     C                   MOVEL     knsif         t02sif
686400040303     C                   MOVEL     'COR'         t02cod
686500040303     C                   MOVEL     v1ctco        t02ke1
686600040303      *
686700040303     C                   CALL      'TIBS02R'
686800040303     C                   PARM                    KPJBA
686900040303     C                   PARM                    TIBS02DS
687000040303      *
687100040303     C                   movel     T02uni        DCOR
687200040303     C                   movel     §CORdesc      v1dtco
687300040303
687400040303     c                   eval      G02inv = tgddar
687500040303     c                   eval      G02err = '3'
687600040303     c                   call      'XSRDA8'
687700040303     c                   parm                    WLBdat
687800040303     c                   z-add     G02dat        v1cdar                         data ricez.comunicaz
687900040303      *
688000040304     c                   MOVEL     tgdorr        v1corr                         ora  ricez.comunicaz
688100040303
688200040303     c                   clear                   v1cchi
688300040303
688400040303     c                   eval      v1crsc = tgdrsc
688500041209     c                   eval      v1cchi = tgdksc
688600070403     c                   eval      W_v1cchi = tgdksc
688700050118      * decodifica
688800050118
688900050118     C                   clear                   TIBS69DS
689000050118     c                   z-add     v1cchi        I69kac
689100050118     C                   call      'TIBS69R'
689200050118     C                   parm                    tibs69DS
689300050118     C                   parm                    DS_cnaco
689400050118     C                   parm                    DS_cnind
689500050118     C                   parm                    DS_cnclp
689600050118     C                   parm                    DS_fncls
689700050118     C                   eval      wtibs69r = '1'
689800050118     c                   If        o69err = ' '
689900050118     c                   movel     ACOrag        v1dchi
690000050118     c                   endif
690100050118
690200040303     c                   eval      v1cclv = tgdclv
690300040303
690400040303      * recupero i riferimenti reclamo
690500040303
690600040303     c                   clear                   v1refe
690700040303     c                   clear                   v1ntel
690800040303     c                   clear                   v1mail
690900040303     c                   clear                   v1nfax
691000040303      *
691100040421     c     Ktgran2       setll     fitgr01l
691200040303     c                   do        *hival
691300040421     c     Ktgran2       reade     fitgr01l
691400040303
691500040303     c                   if        %eof(fitgr01l)
691600040303     c                   leave
691700040303     c                   endif
691800040303
691900040303      * referente
692000040303     c                   if        tgrtri = 'R'
692100040303     c                   eval      v1refe = tgrrif
692200040303     c                   endif
692300040303      * mail
692400040303     c                   if        tgrtri = 'M'
692500040303     c                   eval      v1mail = tgrrif
692600040303     c                   endif
692700040303      * numeri telefonoici
692800040303     c                   if        tgrtri = 'N'
692900040303     c                   movel     tgrrif        v1ntel
693000040303     c                   move      tgrrif        v1nfax
693100040303     c                   endif
693200040303
693300040303     c                   enddo
693400151216      *
693500040216     c                   ENDSR
693600150609
693700150609      *****************************************************************
693800150609      *  Ricerca alfabetica
693900150609      *****************************************************************
694000150609     c     SR_RICALF     begsr
694100150609      *
694200150609     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
694300150609     C                   Z-ADD     3             PAXkut                         tipo estrazione
694400150609     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
694500150609     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
694600150609     c                   movel(P)  V1dchi        PAXrag
694700150609     C                   z-add     1             PAXnum
694800150609      *
694900150609     C                   CALL      'XALFA3BR'
695000150609     C                   PARM                    PAXDUT           30
695100150609     C                   PARM                    PAXkut            1 0
695200150609     C                   PARM                    PAXrag           48
695300150609     C                   PARM                    PAXkcc            4 0
695400150609     C                   PARM                    PAXsta            1 0
695500150609     C                   PARM      *blanks       PAXflr           90
695600150609     C                   PARM      *blanks       PAXdit            3
695700150609     C                   PARM                    PAXnum            2 0
695800150609     C                   PARM                    PAXkcm           80
695900150609     C                   PARM                    PAXksm          140
696000150609     C                   PARM                    PAXkdm           60
696100150609      *
696200150609      * NON TROVATO O PREMUTO CMD12
696300150609    2C                   If        paxsta > -1
696400150609     C                   movel     PAXksm        V1Cchi
696500150609     C                   movel     PAXrag        V1dchi
696600150609     c                   endif
696700150609      *
696800150609     c                   endsr
696900150310
697000150310      /free
697100150310       //--------------------------------------------------------------
697200150318       //?F10 = Immissione ORM.
697300150310       //--------------------------------------------------------------
697400150310       BEGSR  SR_F10;
697500150310
697600150417         clear parm01;
697700150417         clear FIOR05DS;
697800150417         pfgs = DUTpou;
697900150417         ppoe = DUTpou;
698000150417         psce = '1';
698100150417         kpjbu = parm01;
698200150417         fior05r (kpjba:fior05ds);
698300150310
698400150310       ENDSR;
698500150318
698600150318       //--------------------------------------------------------------
698700150318       //?F11 = Visualizza Variazioni.
698800150318       //--------------------------------------------------------------
698900150318       BEGSR  SR_F11;
699000150318
699100150318         fidna3r (kpjba:fidna0ds);
699200150318
699300150318       ENDSR;
699400150310      /end-free
699500150310
699600070330      ****************************************************************
699700070330      * F17 = GESTIONE NOTE LDV
699800070330      ****************************************************************
699900070330     C     Sr_f17        BEGSR
700000070330      *
700100131206     c                   clear                   Fnlrn6DS
700200070423
700300070423      * se al momento del richiamo delle note LDV ho delle note RA propongo il
700400070423      * pgm delle note in immissione
700500070423      * se la linea di arrivo della bolla  è abilitata a GEO e l'ho trovata in fnarb
700600070423
700700131206     c                   eval      in6mod  = 'V'
700800140205      * note valorizzate   o   prevista obbligatorietà (in tab. "MAR")
700900070423     c                   If        (v1not1 <> *blanks or v1not2 <> *blanks  or
701000140205     c                              v1not3 <> *blanks or v1not4 <> *blanks  or
701100140205     c                              §MARnotLdV = 'S')    and
701200070423      * po abilitato a geo e bolla in arb  e non sono la sede
701300070423     c                             $abilitato = *on and $fnarb = *on
701400070423      * verifico se sono la filiale di arrivo della bolla
701500130611     c                   move      arblna        w003a
701600130610     c     w003a         lookup    wPOG                                   27
701700070423     c                   If        %found
701800131206     c                   eval      in6mod  = 'I'
701900131206     c                   movel     v1not1        in6not1
702000131206     c                   move      v1not2        in6not1
702100131206     c                   movel     v1not3        in6not2
702200131206     c                   move      v1not4        in6not2
702300070423     c                   endif
702400070423     c                   endif
702500131206     c                   eval      in6aas  = v1anns
702600131206     c                   eval      in6lnp  = v1lnps
702700131206     c                   eval      in6nrs  = v1nrss
702800131206     c                   eval      in6nsp  = v1nsps
702900131206     c                   eval      in6pgm  = 'FIDNA1R'
703000140205      *
703100140205      * se sono una spedizione in consegna passo i dati della distinta del file ARB
703200140205     c                   If        $cons  = *on
703300160104     c     KSPED         chain     Fnarb000
703400140205     c                   If        %found(Fnarb01l)
703500140205     c                   eval      in6fgs = arbifp
703600140205     c                   eval      in6nfv = arbndc
703700140205     c                   eval      in6dfv = arbddc
703800140205     c                   eval      in6pdr = arbpdc
703900140205     c                   eval      in6cons = 'S'
704000140205     c                   Endif
704100140205     c                   Endif
704200070330
704300131206     c                   call      'FNLRN6R'
704400070330     c                   parm                    kpjba
704500131206     c                   parm                    FNLRN6DS
704600070330
704700070330     c                   endsr
704800040212      ****************************************************************
704900040212      * F18 = GESTIONE NOTE
705000040212      ****************************************************************
705100040212     C     Sr_f18        BEGSR
705200040212      *
705300040212     C                   clear                   FIDNB0DS
705400040218     c                   z-add     v1cpot        iB0pot
705500040421     c                   z-add     v1cant        iB0ant
705600040212     c                   z-add     v1cnut        iB0nut
705700040212     c                   z-add     v1cpdt        iB0pdt
705800040218
705900040219     c                   z-add     Wdar          ib0dar
706000040421     c                   movel(p)  v1corr        ib0orr
706100040218
706200040218     c                   move      v1cmad        ib0mad
706300040218     c                   movel     v1dmad        ib0dma
706400040218
706500040212     c                   movel     'GEN'         iB0tIN
706600040218
706700040218      * - OGGETTO
706800040218
706900040218      * ---------- SPEDIZIONE -----------
707000040218
707100040218     c                   if        v1ctor = 'S'
707200040218
707300040218     c                   movel     spevid        WRKspe
707400040218
707500040219     c                   eval      ib0ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
707600040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
707700040219     c                             ' del ' + WRKann
707800040218      *
707900040218     c                   endif
708000040218
708100040218      * ----------    ORM     -----------
708200040218
708300040218     c                   if        v1ctor = 'O'
708400040218
708500040218     c                   movel     ormvid        WRKorm
708600040218
708700040218     c                   eval      ib0ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
708800040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
708900040218     c                             '/' + %trim(WRKnrv)
709000040218      *
709100040218     c                   endif
709200040218
709300040218      * ----------  GENERICO  -----------
709400040218
709500040218     c                   if        v1ctor = 'G'
709600040218     c                   movel     v1cogg        ib0ogg
709700040218     c                   endif
709800040218
709900040212     c                   movel     v1not1        db0no1
710000040212     c                   movel     v1not2        db0no2
710100040212     c                   movel     v1not3        db0no3
710200040212     c                   movel     v1not4        db0no4
710300040219      * se sono in modalità immissione e richiamo una riga da subfile richiamo il pgm note con
710400040219      * FLM = ' ' altrimenti con "M"
710500040305     c                   if        Wmod = 'I'
710600040305     c                   if        wrk_recsbf = 'Y'
710700040219     c                   movel     ' '           iB0flm
710800040219     c                   else
710900040212     c                   movel     'M'           iB0flm
711000040219     c                   endif
711100040305      * modalità manutenzione
711200040305     c                   else
711300040305     c                   movel     'V'           iB0flm
711400040305     c                   endif
711500070413      * se ho già scritto delle note anche se sono in immissione perchè provengo
711600070413      * da gestione dei blocchi consegne vado nel pgm gestione note ijn manutenzione
711700160601     c****               If        Wmod = 'I' and $bloccof3 = *on
711800160601     c                   If        Wmod = 'I' and $Note_scritte= *oN
711900070413     c                   movel     'V'           iB0flm
712000070413     c                   endif
712100040617      * aggiunto valorizzazione DB0PON
712200040617      * se sono utente particolare che ha un P.O. impostato da tabella passo il P.O. in db0pon
712300040617     c                   eval      db0pon = W_pot
712400040212      *
712500040212     C                   CALL      'FIDNB0R'
712600040212     C                   PARM                    kpjba
712700040217     C                   PARM                    FIDNB0DS
712800040212      *
712900040212      * valorizzo le prime 4 note a video
713000040212      *
713100040212     c                   movel     DB0NO1        v1not1
713200040212     c                   movel     DB0NO2        v1not2
713300040212     c                   movel     DB0NO3        v1not3
713400040212     c                   movel     DB0NO4        v1not4
713500040213      *
713600040213      * se ob0piu è valorizzato ci sono + di 4 righe
713700040213     c                   eval      *in29 = (OB0PIU = '+')
713800040218      *
713900040218      * VALORIZZO FLAG RICHIAMO FIDNB0R
714000040218      *
714100040218     C                   eval      wfidnb0r = 'Y'
714200040212      *
714300040212     c                   ENDSR
714400040216      *
714500040216      *---------------------------------------------------------------*
714600040216      * Gestione WINDOW                                               *
714700040216      *---------------------------------------------------------------*
714800040216     c     SR_GESWIN     BegSR
714900040216
715000040216     c                   do        *hival
715100040216      *
715200040216     c                   exfmt     fia1w01
715300040216      * KF = conferma fine lavoro perdita dei dati
715400040216     c                   if        *inkf
715500040220      * disalloco il file dettagli
715600040220     c                   unlock    fitgd01l
715700040220      *
715800150928     c                   clear                   fidnCAds
715900040421     c                   z-add     v1cant        ic0ant
716000040216     c                   z-add     v1cnut        ic0nut
716100040216     c                   z-add     999           ic0pdt
716200040216      *
716300150928     c                   call      'FIDNCAR'
716400150928     c                   parm                    fidnCAds
716500040216      *
716600040216     c                   leave
716700040216     c                   endif
716800040216      * KL = ritorna alla videata precedente senza deletare nulla
716900040216     c                   if        *inkl
717000040216     c                   leave
717100040216     c                   endif
717200040216
717300040216     c                   enddo
717400040216
717500040216     c                   endsr
717600040223      *---------------------------------------------------------------*
717700040223      * Gestione WINDOW     2                                         *
717800040223      *---------------------------------------------------------------*
717900040223     c     SR_GESWIN2    BegSR
718000040223
718100040223      *
718200040223     c                   exfmt     fia1w02
718300040223      * ENTER = conferma interrogazione bolle arrivi / partenze / ORM
718400040223      * KL = ritorna alla videata
718500040223
718600040223     c                   endsr
718700040406      *---------------------------------------------------------------*
718800040406      * Routine scrittura note aggiuntive automatiche                 *
718900040406      *---------------------------------------------------------------*
719000040406     c     WRT_NOTE      BegSR
719100040406
719200040406     C                   clear                   FIDNB0DS
719300040407      * chiusura
719400141210      *?Verifica telefonica "PINcode"?
719500141210if  1c                   If        Not_PINcode = 'S'
719600141211if  2c                   if        oPINfxx = *blank  and  oPINerr = *blank
719700141210     c                   eval      dB0no1 = 'ESITO: POSITIVO'
719800141210x   2c                   else
719900141210     c                   eval      dB0no1 = 'ESITO: NEGATIVO'
720000141210e   2c                   endif
720100141215     c                   clear                   WLBdat
720200141210     c                   eval      G02inv = ARBddc
720300141215     c                   eval      G02err = '3'
720400141210     c                   call      'XSRDA8'
720500141210     c                   parm                    WLBdat
720600141216     c                   eval      dB0no2 = 'DIST: ' +
720700141210     c                                      %subst( %editc( ARBifp : 'X' ) :
720800141210     c                                              %len( ARBifp ) - 2 : 3 ) +
720900141216     c                                      ' ' +
721000141210     c                                      %trim( %editc( ARBndc : '3' ) ) +
721100141210     c                                      ' DEL ' +
721200141211     c                                      %trim( %editc( G02dat : 'Y' ) )
721300141211     c                   eval      dB0no3 = 'AUT: ' +
721400141210     c                                      %trim( %editc( ARBpdc : 'X' ) )
721500141210e   1c                   EndIf
721600040407
721700040407
721800041214     c                   z-add     tgdant        iB0ant
721900041214     c                   z-add     tgdnut        iB0nut
722000041214     c                   z-add     tgdpdt        iB0pdt
722100040406     c                   movel     'GEN'         iB0tIN
722200141216     c                   z-add     999           iB0pgn
722300040406     c                   movel     'W'           iB0flm
722400050111     c                   eval      db0sts ='N'
722500041214     c                   eval      ib0pot =tgdpot
722600041013     c                   eval      db0pon =Dutpou
722700040406     c                   eval      db0utn =knmus
722800040406
722900040406     C                   CALL      'FIDNB0R'
723000040406     C                   PARM                    kpjba
723100040406     C                   PARM                    FIDNB0DS
723200040407
723300141210     c                   clear                   Not_PINcode
723400040406
723500040406     c                   endSR
723600150316
723700150316      /free
723800150316       //---------------------------------------------------------------*
723900150316       // Routine scrittura file modifiche                              *
724000150316       //---------------------------------------------------------------*
724100150316       BEGSR WRT_Modifiche;
724200150316
724300150316         clear FITGM000;
724400160105         if  wTMO = 'A';
724500160105           TGMpot = V1Cpot;
724600160105           TGMant = V1Cant;
724700160105           TGMnut = V1Cnut;
724800160105           TGMpdt = V1Cpdt;
724900160105         else;
725000160105           TGMpot = TGDpot;
725100160105           TGMant = TGDant;
725200160105           TGMnut = TGDnut;
725300151214           TGMpdt = TGDpdt;
725400151214         endif;
725500150316         TGMtmo = wtmo;
725600150316         SELECT;
725700151214         WHEN  TGMtmo = 'A';
725800151214           TGMmod = dTGMa;
725900150318         WHEN  TGMtmo = 'C';
726000150318           TGMmod = dTGMc;
726100150318         WHEN  TGMtmo = 'K';
726200150318           TGMmod = dTGMk;
726300150318         WHEN  TGMtmo = 'R';
726400150318           TGMmod = dTGMr;
726500150326         WHEN  TGMtmo = 'S';
726600150326           TGMmod = dTGMs;
726700150316         WHEN  TGMtmo = 'U';
726800150316           TGMmod = dTGMu;
726900150316         ENDSL;
727000150316         TGMdmo = %dec(%date());
727100150316         TGMomo = %dec(%time());
727200150316         TGMutn = knmus;
727300150316         TGMpon = DUTpou;
727400150318         TGMnoj = knmeb;
727500150316         write FITGM000;
727600150319         feod FITGM01L;
727700150316
727800150316       ENDSR;
727900150316      /end-free
728000150316
728100150316      *
728200040126      *---------------------------------------------------------------*
728300040126      * Routine Iniziale                                              *
728400040126      *---------------------------------------------------------------*
728500040126     c     *InzSR        BegSR
728600040126      *
728700040129     c     *Entry        plist
728800040129     c                   parm                    KPJBA
728900040213     c                   parm                    FIDNA0DS
729000040126      * Reperisco data corrente
729100040126     c                   time                    w0140
729200040126     c                   move      w0140         wData
729300040126     c                   movel     w0140         wOra
729400040126     c                   clear                   WLBdat
729500040126     c                   eval      G02dat = wData
729600040126     c                   call      'XSRDA8'
729700040126     c                   parm                    WLBdat
729800040126     c                   z-add     G02inv        DateU
729900040324     C     *iso          movel     dateu         Wdataoggi
730000040126      *
730100040126      * Reperisco dati job
730200040126     c                   exsr      DatiJob
730300040126     c                   movel     SDSpgm        VTCpgm
730400130806
730500130806      * Imposto le abilitazioni dell'utente
730600130806     c                   eval      dute01 = UTEfaf
730700041005
730800041005      * Se sono in sede accendo 05
730900041005     c                   eval      *in05=(simfel = 0)
731000041005
731100040213      * Se ricevuto parametro per manutenzione  non valorizzo il p.o. emittente con quello
731200040213      * in linea
731300040213     c                   if        parms   > 1
731400040213      * modalità GESTIONE
731500040213     c                   eval      wmod   = 'G'
731600040217     c                   eval      v1tmod = tit2
731700040303     c                   eval      *in50 = *on
731800040420      * reperisco i programmi che hanno richiamato questo pgm
731900040420     c                   Call      'XRTVSTKR'
732000040420     C                   Parm                    PStack         1000
732100040420     C                   Parm                    PStartPoint       3 0
732200040420     C                   Parm                    PEntries          3 0
732300040420     c                   Movel     Pstack        DSstack
732400040213     c                   else
732500040213     c                   eval      v1cpot = dutpou
732600040616      *
732700040213      * modalità IMMISSIONE
732800040213     c                   eval      wmod   = 'I'
732900040217     c                   eval      v1tmod = tit1
733000040213     c                   endif
733100040617      * verifico se sto inserendo o modificando un reclamo da parte di un utente
733200040617      * che deve avere come p.o. apertura e gestione quello della tabella UGR
733300040617      * tabella UGR
733400040617     c                   movel     KNMUS         w_ute
733500040617     c                   clear                   tibs02ds
733600040617     C                   MOVEL     'C'           t02mod
733700040617     C                   MOVEL     knsif         t02sif
733800040617     C                   MOVEL     'UGR'         t02cod
733900040617     C                   MOVEL     W_ute         t02ke1
734000040617      *
734100040617     C                   CALL      'TIBS02R'
734200040617     C                   PARM                    KPJBA
734300040617     C                   PARM                    TIBS02DS
734400040617      *
734500040617     C                   IF        t02err = *BLANKS
734600040617      *
734700040617     c                   movel     t02uni        DUGR
734800040617      * se P.O. da forzare diverso da zero imposto in automatico
734900040617     c                   if        §UGRpofo > 0
735000040617     c                   eval      v1cpot = §UGRpofo
735100040617     c                   eval      W_pot  = §UGRpofo
735200040617     c                   endif
735300040617      *
735400040617     c                   endif
735500040324      * reperisco i dati standard RECLAMI
735600040324      * Controllo
735700040324     c                   clear                   tibs02ds
735800040324     C                   MOVEL     'C'           t02mod
735900040324     C                   MOVEL     knsif         t02sif
736000040324     C                   MOVEL     'REC'         t02cod
736100040324     C                   MOVEL     '1'           t02ke1
736200040324      *
736300040324     C                   CALL      'TIBS02R'
736400040324     C                   PARM                    KPJBA
736500040324     C                   PARM                    TIBS02DS
736600040324      *
736700040324     C                   IF        t02err = *BLANKS
736800040324     C                   movel     T02uni        DREC
736900040324     c                   endif
737000041012      * verifico se sono un utente che può modficare tutto senza controlli
737100130806     c                   if        §UTEvrac = 'S'
737200041012     c                   eval      *in07 = *on
737300041012     c                   endif
737400041005      * se sono in sede apro il file TITAS
737500041005     c                   If        *in05
737600041005     c                   open      titas30c
737700041005     c                   else
737800041005     c                   open      fnblp01l
737900041005     c                   open      fnarb01l
738000140429     c                   open      FIAR401L
738100140429     c                   open      FIAR501L
738200041005     c                   endif
738300151130      *
738400151130      /free
738500170113
738600170113         // -?Esecuzione override al *file AZNUM01L?
738700170113         //  ?(usato dal *pgm chiamato TRUL33R)?
738800170113         //  ?per limitare il "Tempo massimo attesa record"?
738900170113         exsr  sr_OvrAZNUM;
739000151130
739100151130         // -?Caricamento filiali x Estero?
739200151130         clear  TRUL31ds;
739300151130         I31abi = 'EE';
739400151130         trul31r ( KPJBA : TRUL31ds );
739500151130         wFILest_ds = O31pog;
739600151130
739700151130      /end-free
739800151130      *
739900070323      * se non sono in sede verifico se il PO è abilitato a Geo telefonate e mi carico la £6
740000070323     c                   if        not *in05
740100070323     c                   exsr      sr_carl6
740200070323     c                   endif
740300040126      *
740400040126     c                   EndSr
740500040126
740600070323      *---------------------------------------------------------------*
740700070323      * Recupero i P.O.  della £6                                     *
740800070323      *---------------------------------------------------------------*
740900070323     c     Sr_CARL6      begsr
741000070323
741100070323      * Recupero i PO della £6
741200070323      *
741300070323     c                   clear                   trul06ds
741400070323     c                   move      '£6'          d06cod
741500070323     c                   movel     Dutpou        d06key
741600070323     c                   movel(p)  trul06ds      KPJBU
741700070323
741800070323     c                   call      'TRUL06R'
741900070323     c                   parm                    kpjba
742000070323     c                   movel     kpjbu         trul06ds
742100070323     c                   movea     lin           L6
742200070323
742300070323      * verico se PO abilitato alla gestione telefonate GEO
742400070323
742500070323     c                   clear                   og148
742600160104     c     l6(1)         chain     azorg
742700070323     c                   if        %found(azorg01l)
742800070323     c                   movel     orgde8        og148
742900070323     c                   endif
743000070323
743100070323     c                   If        §oggeot = 'S'
743200070323     c                   eval      $abilitato = *on
743300070608     c                   eval      *in08 = *on
743400070323     c                   endif
743500130610
743600130611      /free
743700130611       //?Imposto abilitazione utente
743800130611         clear wpog;
743900130611         SELECT;
744000130611       //?Imposto abilitazione manutenzione bolle di filiale
744100130611           WHEN  §UTEmbf <> *blanks;
744200130611             wabi = §UTEmbf;
744300130611           WHEN  UTEaut <> *blanks;
744400130611             wabi = UTEaut;
744500130611           WHEN  DUTlpo = '1';
744600130611             wabi = 'TP';
744700130611           WHEN  DUTlpo = '2';
744800130611             wabi = 'PO';
744900130611         ENDSL;
745000140123
745100130611      /end-free
745200130611
745300130610      * controllo se ok l'abilitazione dell'utente
745400130611     c                   clear                   TIBS02DS
745500130611     c                   clear                   dLAT
745600130611     c                   eval      T02mod = 'C'
745700130611     c                   eval      T02sif = knsif
745800130611     c                   eval      T02cod = 'LAT'
745900130611     c                   movel(p)  wabi          T02ke1
746000130611     c                   call      'TIBS02R'
746100130611     c                   parm                    kpjba
746200130611     c                   parm                    TIBS02DS
746300130611if  1c                   IF        T02err = *blanks
746400130611     c                   eval      dLAT = T02uni
746500130611e   1c                   ENDIF
746600130611
746700130611      * Se abilitazione trovata e OK
746800130611if  1c                   IF        T02err = *blanks and §LATabi = *blanks
746900130610      * Reperimento dei P.O. gestibili dall'utente
747000130610     c                   clear                   TRUL31DS
747100130610     c                   eval      I31abi = wabi
747200130610     c                   eval      I31cdi = DUTdis
747300130610     c                   eval      I31car = DUTare
747400130610     c                   eval      I31cpo = DUTpou
747500130610     c                   call      'TRUL31R'
747600130611     c                   parm                    kpjba
747700130610     c                   parm                    TRUL31DS
747800130611if  2c                   IF        O31pog > *zeros
747900130610     c                   movea     O31pog        wpog
748000130611x   2c                   ENDIF
748100130611    1c                   ENDIF
748200130611
748300130611      * se sk delle filiali abilitate all'utente vuota impsoto il primo elemento della £6
748400130611     c                   IF        wpog(1) = *blanks
748500130611     c                   eval      wpog(1) = %editc(l6(1):'X')
748600130611     c                   ENDIF
748700150317
748800150317      /free
748900150317       //?Imposto abilitazione utente x manutenzione RA
749000150317         clear wFIL_ds;
749100150317         SELECT;
749200150317         WHEN  §UTErac <> *blanks;
749300150317           wabi = §UTErac;
749400150317         WHEN  UTEaut <> *blanks;
749500150317           wabi = UTEaut;
749600150317         WHEN  DUTlpo = '1';
749700150317           wabi = 'TP';
749800150317         WHEN  DUTlpo = '2';
749900150317           wabi = 'PO';
750000150317         ENDSL;
750100150317       //?Reperimento delle filiali gestibili dall'utente per manutenzione RA
750200150317         clear TRUL31DS;
750300150317         I31abi = wabi;
750400150317         I31cdi = DUTdis;
750500150317         I31car = DUTare;
750600150317         I31cpo = DUTpou;
750700150317         trul31r (kpjba : TRUL31DS);
750800150317         wFIL_ds = O31pog;
750900150317         IF  wFIL_ds = *blanks;
751000150317           sk_wFIL(01) = %editc(DUTpou:'X');
751100150317         ENDIF;
751200150317      /end-free
751300070323
751400070323     c                   endsr
751500070323
751600150601      *---------------------------------------------------------------*
751700150601      * Conto quante RA ci sono a fronte dello stesso oggetto         *
751800150601      *---------------------------------------------------------------*
751900150601     c     Sr_countRA    BegSr
752000150601      *
752100150601      *
752200150603     c                   if        V1ctor = 'O'
752300150603     c                   movel(p)  ormvid        W_V1cogg
752400150603     c                   endif
752500150603     c                   if        V1ctor = 'S'
752600150603     c                   movel(p)  spevid        W_V1cogg
752700150603     c                   endif
752800150603      *
752900160104     c     Ktgd32        setll     fitgd32
753000160104     c     Ktgd32        reade     fitgd32
753100150603     c                   if        %eof(Fitgd32l)
753200150603     c                   eval      IA1INS = 'N'
753300150603     c                   else
753400160104     c     Ktgd32        reade     fitgd32
753500150603      * se c'è una sola RA non abilito F16 nei programmi di interrogazione bolle o ORM
753600150603      * se più di una lo abilito
753700150603     c                   if        not %eof(Fitgd32l)
753800150603     c                   eval      IA1INS = 'I'
753900150603     c                   else
754000150603     c                   eval      IA1INS = 'N'
754100150603     c                   Endif
754200150603
754300150603     c                   Endif
754400150603
754500150601     c                   endsr
754600150601
754700040126      *---------------------------------------------------------------*
754800040126      * Reperimento Dati del job (Utente/Operativi)                   *
754900040126      *---------------------------------------------------------------*
755000040126     c     DatiJob       BegSr
755100040126      *
755200040126     c     *dtaara       define    §azute        azuteds
755300040126     c     *dtaara       define    §datiute      ddatiute
755400040126      *
755500040126     c                   in(E)     *dtaara
755600040126     c                   IF        %ERROR or RSUT = *blanks
755700040126     c                   clear                   Tibs34Ds
755800040126     c                   call      'TIBS34R'
755900040126     c                   parm                    Tibs34Ds
756000040126     c                   in        *dtaara
756100040126     c                   ENDIF
756200040126      *
756300040126     c                   EndSr
756400170113      *
756500170113      /free
756600170113
756700170113       //--------------------------------------------------------------
756800170113       //?Esecuzione OVERRIDE al *file del numeratore R.A. per limitare?
756900170113       //?il "Tempo massimo attesa record" ad un secondo.              ?
757000170113       //--------------------------------------------------------------
757100170113       BEGSR  sr_OvrAZNUM;
757200170113
757300170113         Qcmd = 'ovrdbf file(AZNUM01L) WAITRCD(1) ovrscope(*callLvl)';
757400170113
757500170113         exsr  sr_ExecCmd;
757600170113
757700170113       ENDSR;
757800170113
757900170113       //--------------------------------------------------------------
758000170113       //?Cancellazione OVERRIDE dal *file del numeratore R.A. per     ?
758100170113       //?limitare il "Tempo massimo attesa record" ad un secondo.     ?
758200170113       //--------------------------------------------------------------
758300170113       BEGSR  sr_DltOvrAZNUM;
758400170113
758500170113         Qcmd = 'dltovr file(AZNUM01L) lvl(*)';
758600170113
758700170113         exsr  sr_ExecCmd;
758800170113
758900170113       ENDSR;
759000170113
759100170113       //--------------------------------------------------------------
759200170113       //?Esecuzione del comando (già impostato).                      ?
759300170113       //--------------------------------------------------------------
759400170113       BEGSR  sr_ExecCmd;
759500170113
759600170113         clear Qcap0100;
759700170113         Qcabcsdh = *off;
759800170113         Qcapa    = *off;
759900170113         Qcacmdss = *off;
760000170113         Qcaerved = *allX'00';
760100170113
760200170113         clear Qusec;
760300170113         Qusbprv  = %size(Qusec);
760400170113
760500170113         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
760600170113                           %size(Qcap0100) : 'CPOP0100' : *omit :
760700170113                           0 : 0 : Qusec);
760800170113
760900170113         //if  Qusei <> *blank;
761000170113         //  ...;
761100170113         //endif;
761200170113
761300170113       ENDSR;
761400170113
761500170113      /end-free
761600040126
761700130513** -?Msg:?Messaggi di Errore?------------------------------------------------*
761800040205Codice cliente errato                                                                    1
761900040610Inserire l'oggetto della richiesta                                                       2
762000040211Inserire correttamente numero Spedizione                                                 3
762100040211Spedizione inesistente                                                                   4
762200040211Inserire correttamente numero ORM                                                        5
762300040211Numero ORM inesistente                                                                   6
762400040211Inserire correttamente il "CHI CHIAMA"                                                   7
762500040610Inserire motivo apertura richiesta                                                       8
762600150318Filiale responsabile errata                                                              9
762700040213Inserire Tipo Comunicazione                                                             10
762800040217Data errata                                                                             11
762900040217Ora errata                                                                              12
763000040217Inserire il tipo oggetto                                                                13
763100040610Prima di dare F4 inserire almeno una richiesta                                          14
763200040219Inserire la Ragione Sociale del chiamante                                               15
763300040220Per questo oggetto 'Chi Chiama' non ammesso                                             16
763400040223Tipo comunicazione errata                                                               17
763500040610Motivo apertura richiesta errato                                                        18
763600040610Codice chiusura richiesta errato                                                        19
763700150429Reparto/Utente in gestione errato                                                       20*#
763800040610Data richiesta non deve essere maggiore della data odierna                              21
763900040315Non si può interrogare un oggetto generico                                              22
764000150318Filiale di appartenenza dell'utente diversa dalla filiale gestione                      23
764100160225Richiesta NON gestibile dall'utente                                                     24 +
764200160225Richiesta NON gestibile dall'utente perchè R.A. impegnata da altro utente               25 +
764300160225Se responsabilità Vettore è obbligatorio inserire la Fil. responsabile                  26
764400040610Utente non abilitato alla gestione richieste                                            27
764500170222Impossibile gestire le disposizioni in Giacenza                                         28
764600150429Inserire reparto/utente in gestione                                                     29 #
764700040610Oggetto presente in altre richieste Enter per forzare                                   30
764800040707Oggetto non presente in archivi                                                         31
764900041210Codice Chiamante mancante F4 per forzare la chiusura                                    32
765000070608Variazione non effettuata                                                               33
765100070608Causale variazione errata o non ammessa                                                 34
765200071019Variazione non consentita                                                               35
765300071018Variazione non consentita bolla non in gestione alla Filiale                            36
765400120914Questa Richiesta Assistenza va chiusa con Codice Chiusura                               37
765500120919Immettere le note                                                                       38
765600131119Per il motivo inserito l'oggetto dev'essere                                             39
765700140213Non utilizzabile. Proposto in automatico confermando la R.A.                            40
765800140430Variazione possibile solo su bolle in PARTENZA dalla propria filiale                    41 <
765900140430Motivo Apertura " 17" utilizzabile solo su bolle in ARRIVO                              42 <
766000140923Tipo comunicazione non utilizzabile in gestione R.A.                                    43
766100141029Ora richiesta consegna errata                                                           44
766200141210Motivo Apertura utilizzabile solo per spedizioni in consegna oggi                       45 «
766300141210Motivo Apertura utilizzabile solo per spedizioni che prevedono un "PINcode"             46 «
766400141218Manca il "PINcode" per questa spedizione                                                47 «
766500150429Filiale gestione errata                                                                 48
766600150422Inserire il telefono e/o la mail                                                        49 #
766700150422Reparto gestione errato                                                                 50 #
766800150717Motivo Apertura impostabile solo da utenti di sede                                      51
766900150723Chiusura non consentita da SEDE                                                         52
767000150723Richiesta Assistenza non gestibile da SEDE                                              53
767100150911C.A.P. errato                                                                           54
767200151130Motivo Apertura " 18" NON utilizzabile per bolle EXPORT                                 55
767300151130Esiste già almeno una disposizione di Dirottamento per questa spedizione                56
767400160120Reparto che Attende Risposta errato                                                     57 ¥
767500160120Filiale che Attende Risposta errata                                                     58 ¥
767600160120Reparto/Filiale in Attesa Risposta NON può coincidere con quello di Gestione            59
767700160302ATTENZIONE: numeratori per le R.A. bloccati da altri lavori.  AVVISARE IL CED           60
767800160315Presente Giacenza aperta: Motivo apertura " 18" NON utilizzabile                        61
767900160314Bolla legata: il reso può essere richiesto solo sulla figlia                            62
768000170516Funzione non eseguibile Programma richiamato in precedenza                              63
768100170526Impossibile gestire le disposizioni in Giacenza con Motivo Apertura inserito            64
