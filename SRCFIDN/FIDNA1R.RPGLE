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
004100170519     fFNLBL01l  if   e           k disk    rename(fnlbl000:fnlbl1)
004200040211     fFNORM01L  if   e           K disk
004300040212     fAZORG01L  if   e           K disk
004400040426     fAZCLN01L  if   e           K disk
004500040323     fAZUTE01L  if   e           K disk
004600041005      * filiale
004700041005     fFNARB01L  if   e           K disk    usropn
004800041005     fFNBLP01L  if   e           K disk    usropn
004900140429     fFIAR401L  if   e           k disk    usropn
005000140429     fFIAR501L  if   e           k disk    usropn
005100041005      * sede
005200041005     fTITAS30C  if   e           K DISK    usropn
005300151201      *
005400151201     fTIIDC02L  if A e           k disk    usropn
005500040112      *
005600161027     fFIDNA1D   cf   e             workstn sfile(FIA1S01:S01nrr)
005700160104     f                                     ignore( FIA1W03 )
005800040107
005900040107      *---------------------------------------------------------------*
006000070327      *  MOTIVI DI APERTURA FISSATI A PROGRAMMA
006100070327      *---------------------------------------------------------------*
006200070327      *   2 = LASCIATO AVVISO                               W_LAV
006300070327      *  10 = CONTRASSEGNO                                  W_CON
006400170524      *  12 = GIACENZA                                      W_GIA
006500070327      *  14 = SUPERMERCATO                                  W_SUP
006600070327      *  15 = APPUNTAMENTO                                  W_APP
006700070327      *  16 = FERMO DEPOSITO                                W_FDE
006800140429      *  17 = FERMO DEPOSITO AUTORIZZA MITTENTE             C_FDEAM
006900151130      *  18 = RESO                                          C_MOTAP_RESO
007000141210      *  60 = VERIFICA PIN-CODE TELEFONICA                  C_VPINC
007100150720      *  70 = RECLAMO DA SEDE                               C_RECLSEDE
007200070327      *---------------------------------------------------------------*
007300040107
007400040107      *
007500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
007600040107      *
007700040205
007800171122     d MSG             S             78    DIM(065) CTDATA PERRCD(1)            MSG VIDEO
007900040531     D CCR             S              1    DIM(20)                              Chi chiama
008000070323      *
008100070323     d L6              s              3  0 dim(30)                              po £6
008200130610     D wpog            s              3    dim(250)
008300070327
008400070327     D NOB             S             35    DIM(12)                              Note
008500070327     D NORA            S             35    DIM(12)                              Note da scrivere
008600070327     D NOV             S             35    DIM(04)                              Note a video
008700070327
008800070323      *
008900040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
009000040107      *
009100040107     d KPJBA         e ds
009200040213      * - Parametri per gestione reclami già esistenti
009300040213     d FIDNA0DS      e ds
009400040213      * - Parametri ritorno da interrigazione oggetto
009500040213     d FIDNA1DS      e ds
009600040203      * - Parametri note
009700040107     d FIDNB0DS      e ds
009800040217      * - Parametri cancellazione reclamo
009900150928     d FIDNCAds      e ds                  inz
010000070330      * - Parametri per richiamo pgm note per Aut / ldv
010100131206     d FNLRN6DS      e ds
010200070329      * - Parametri per richiamo pgm blocchi
010300070329     d FNLRG2DS      e ds
010400070430      * - Parametri per richiamo pgm recupero blocchi
010500070430     d FNLRG6DS      E DS
010600070430     d DLRG6ABI      E DS
010700070329      * - Parametri per richiamo pgm FNLR48R
010800070329     D fnlr48ds      E DS
010900070608      * - Parametri per richiamo pgm FNLR66R controllo causali variazioni bolle
011000070608     D fnlr66ds      E DS
011100150310      * - Parametri per richiamo pgm FIOR05R immissione ORM
011200150310     d FIOR05DS      e ds
011300170220      * - Parametri per richiamo pgm FNLG07R1 restituisce se c'è giacenza
011400170220     d FNLG071DS     e ds
011500170221      * - Parametri per richiamo pgm FNLG10R
011600170221     D fnlg82ds      E DS
011700070329      *
011800070329     D DSARBD        E DS
011900070329     D DSARBG        E DS
012000070329     D DARBT         E DS
012100070329     D DSARBK        E DS
012200070329     D trul90ds      E DS
012300140924     D DSARBN        E DS
012400040108      *
012500040108      * - Parametri x Controllo profilo utenti
012600040108     d TIBS34DS      e ds                  inz
012700040108      * - Ds di riferimento al file esterno AZUTE00F
012800040108     d AZUTEDS       e ds                  extname(AZUTE00F)
012900040108      * - Ds per dati organigramma
013000040108     d DDatiUte      e ds
013100070323      * abilitazioni
013200070323     D OG148         E DS
013300070323      * DS per TRUL06R - Caricamento £X
013400070323     d TRUL06DS      E DS
013500070323     d  LIN                    1     90  0
013600070323     d                                     DIM(30)
013700040202      * - Ds per Anagrafiche Clienti
013800040202     d TIBS69DS      E DS
013900040202     d DS_cnaco      E DS                  extname(CNACO00F)
014000040202     d DS_cnind      E DS                  extname(CNIND00F)
014100040202     d DS_cnclp      E DS                  extname(CNCLP00F)
014200040202     d DS_fncls      E DS                  extname(FNCLS00F)
014300040107      *
014400040129      * - Parametri x Numeratore Reclami
014500040212     d TRUL33ds      e ds
014600040109      * - Tabelle
014700040129     d TIBS02ds      e ds                  inz
014800131119     d tntbMARds1    e ds                  inz
014900130610
015000130610     D TRUL31DS      E DS
015100130610     d dLat          e ds
015200130610     d dute01        e ds
015300070327      *
015400040212     d DMAR          e ds
015500040212     d DCHR          e ds
015600040212     d DCMU          e ds
015700040217     d DCOR          e ds
015800130514     d DOGR          e ds
015900130514     d dOGR_S        e ds                  extname(dOGR)  qualified
016000130514     d                                     inz
016100040220     d DCCR          e ds
016200040323     d DPRR          e ds
016300040323     d DUGR          e ds
016400040323     d DREC          e ds
016500141210      *
016600141210     d ds7R          e ds                  inz  qualified
016700140429      *
016800140429      * -?Estensione TESTATA BOLLA: tipo record "GEN"?
016900140429     d dAR5gen       e ds                  inz
017000141210      *
017100151211      * -?Gestione Telefonate - Flag Avanzamento Fase R.A.?
017200151210     d dTGDfar       e ds                  inz  qualified
017300151211
017400151211      * -?Gestione Telefonate - Flag operativi di FITGD00F.TGDFLO?
017500151211     d dTGD01        e ds                  inz  qualified
017600151210
017700141210      * -?Gestione Telefonate - Riferimenti Chiamante: Riferimento "PINcode"?
017800141210     d dTGR01        e ds                  inz  qualified
017900151210
018000151210      * -?Gestione Telefonate - Riferimenti Chiamante: Attesa Risposta?
018100151210     d dTGR01a       e ds                  inz  qualified
018200151210
018300151210      * -?DS Modifiche Avanzamento Fase FITGM
018400151210     d dTGMa         e ds                  inz  qualified
018500150318
018600150318      * -?DS Modifiche Causale Chiusura FITGM
018700150318     d dTGMc         E DS
018800150318
018900150318      * -?DS Modifiche Cod. Chiamante FITGM
019000150318     d dTGMK         E DS
019100150318
019200151210      * -?DS Modifiche Fil. Responsabile FITGM
019300150318     d dTGMR         E DS
019400150326
019500150326      * -?DS Modifiche Stato Responsabile FITGM
019600150326     d dTGMS         E DS
019700150316
019800150316      * -?DS Modifiche Utente FITGM
019900150316     d dTGMu         E DS
020000131119      *
020100131119      * - Parametri x controllo indirizzo e-mail
020200040217     d DSEMAIL       E DS
020300160708      *
020400160708      * -?Aggiunge/Toglie gg/mm dalla data?
020500160708     d XGIOLAVds     e ds                  inz
020600160701      *
020700160706      * -?Parametri da passare al pgm interrogaz. Bolle di Filiale?
020800160701     d ParamU6ds       ds                  inz   qualified
020900160706      ** ?CAMPI DI OUTPUT:?
021000160706      * ?. Key spedizione?
021100160706     d   V2Caas               14     17  0 inz
021200160706     d   COMlnp               18     20  0 inz
021300160706     d   V2Cnrs               21     22  0 inz
021400160706     d   V2Cnsp               23     29  0 inz
021500160706      * ?. Flag '1'=Premuto F3=Fine?
021600160706     d   PA1f03               30     30
021700160706      ** ?CAMPI DI INPUT:?
021800160706      * ?. Modalità di richiamo?
021900160706     d   PA1flg               31     31
022000160706      * ?. Flag '1'= RICH DA PGM GIAC.?
022100160706      *             ?(Serve per disabilitare tasto funzionale F14=Giac.?
022200160706      *              ?in interrogazione della bolla ed evitare così?
022300160706      *              ?errore di chiamata ricorsiva?
022400160706     d   PA1gia              144    144
022500160706      * ?. Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:?
022600160706     d   PA1rma              149    163
022700160706     d   PA1xco              164    164
022800160706      * ?. non utilizzato?
022900160706     d   PA1flo              165    256
023000160706      *
023100160706      * -?Parametri da passare al pgm interrogaz. singola Bolla di Filiale?
023200160706     d ParamU6ds1      ds                  inz   qualified
023300160706      ** ?CAMPI DI INPUT:?
023400160706     d  V2Caas                14     17  0 inz
023500160706     d  COMlnp                18     20  0 inz
023600160706     d  V2Cnrs                21     22  0 inz
023700160706     d  V2Cnsp                23     29  0 inz
023800160706      ** ?CAMPI DI OUTPUT:?
023900160706      * ?. Flag '1'=Premuto F3=Fine?
024000160706     D  PARf03                30     30
024100160706      ** ?CAMPI DI INPUT:?
024200160706      * ?. Modalità di richiamo?
024300160706     D  PARflg                31     31
024400160706      * ?. Flag '1'= RICH DA PGM GIAC.?
024500160706     D  PARgia               144    144
024600160706      ** ?CAMPI DI OUTPUT:?
024700160706     d  PARrma               149    163                                         x F22=PerfectOrder
024800160706     d  PARxco               164    164                                         x F22=PerfectOrder
024900160706      ** ?CAMPI DI INPUT:?
025000160706      * ?. Tipo lancio: "C" -           CHIUSO CON LR?
025100160706      *                ?"L" - ELABORO E CHIUDO CON LR?
025200160706      *                ?" " - ELABORO E CHIUDO IN RETRN?
025300160706     d  PARtla               165    165
025400170119      ** ?CAMPI DI OUTPUT:?
025500170119      * ?. Codice di errore: "1" = Sped. non trovata?
025600170119     d  PARerr               166    166
025700170119     d  PARflo               167    256
025800160701      *
025900160706      * -?Altri Parametri da passare al pgm interrogaz. Bolle di Filiale?
026000160701     d fnlrU6ds      e ds                  inz
026100040109      *
026200040211      * - Parametri da passare al pgm di visualizzazione ORM
026300040211     D Parm01          DS
026400040211     D  ppoe                          3  0
026500040211     D  pnor                          7  0
026600040211     D  pnsr                          2  0
026700040211     D  pnrv                          2  0
026800040211     D  psce                          1
026900040211     D  pfgs                          3  0
027000040211     D  ppor                          3  0
027100040211     D  pdtr                          8  0
027200040211     D  pmdb                         10
027300040211     D  pprb                         10
027400040211     D  pdts                          8  0
027500040211     D  prmp                          1
027600040211     D  pbrc                          1
027700040211     D  pref                          2
027800040211     D  pmio                          1
027900041005      * - Parametri da passare al pgm di visualizzazione Bolle di sede
028000041005     D TNSB50DS      E DS
028100040211      *
028200040205      * - Esplosione oggetto spedizione
028300040205     D DS_spediz       DS                  INZ
028400040219     D  DSspe_lnp              1      3  0
028500040219     D  DSspe_nrs              4      5  0
028600040219     D  DSspe_nsp              6     12  0
028700040219     D  DSspe_aas             13     16  0
028800040205      * - Esplosione oggetto ORM
028900040205     D DS_orm          DS                  INZ
029000040205     D  DSorm_poe              1      3  0
029100040205     D  DSorm_nsr              4      5  0
029200040205     D  DSorm_nor              6     12  0
029300141212     D  DSorm_nrv             13     14  0
029400040205      *
029500040205      * - Parametri da passare al pgm interrogazione ORM
029600161107     D FIOR010DS     E DS                  inz
029700161107     d   IOR010ric   e                     inz('1')
029800161107     d   IOR010rsrX  e                     inz('X')
029900161107     d   IOR010inrX  e                     inz('X')
030000161107     d   IOR010lorX  e                     inz('X')
030100161107     d   IOR010rsoX  e                     inz('X')
030200161107     d   IOR010rscX  e                     inz('X')
030300070327      *
030400040109     d Status         sds
030500040109     d  SDSpgm           *proc
030600040213     d  PARMS            *parms
030700040107      *
030800170113     d*// dspINFOds       ds           512
030900170113     d*//  di_TASTO             369    369
031000170113     d*//  di_NRG               370    370
031100170113     d*//  di_SFLnrr            378    379B 0
031200040109      *
031300040109     d WLBdat          ds                  inz
031400040109     d  G02dat                 1      8  0 inz
031500040109     d  G02inv                 9     16  0 inz
031600040109     d  G02err                17     17    inz('3')
031700040109     d  G02tgi                18     22  0 inz
031800040211      * - DS di comodo    ORM
031900040211     D                 DS
032000040211     D  W_poe                         3  0
032100040211     D  W_nsr                         2  0
032200040211     D  W_nor                         7  0
032300040211     D  W_nrv                         2  0
032400040303     D  Wormvid                1     14  0 inz
032500040211      *
032600040211     D                 DS
032700040211     D  v1poeo                        3  0
032800040211     D  v1nsor                        2  0
032900040211     D  v1norm                        7  0
033000040211     D  v1nrvo                        2  0
033100040303     D  ormvid                 1     14  0 inz
033200040216      *
033300040216     D                 DS
033400040216     D  WRKpoe                        3
033500040216     D  WRKnso                        2
033600040216     D  WRKnor                        7
033700040216     D  WRKnrv                        2
033800040216     D  WRKORM                 1     14
033900040211      * - DS di comodo    spedizione
034000040211     D                 DS
034100040211     D  W_lnp                         3  0
034200040211     D  W_nrs                         2  0
034300040211     D  W_nsp                         7  0
034400040211     D  W_aas                         4  0
034500040303     D  Wspevid                1     16  0 inz
034600040211      *
034700040211     D                 DS
034800040211     D  v1lnps                        3  0
034900040211     D  v1nrss                        2  0
035000040211     D  v1nsps                        7  0
035100040217     D  v1anns                        4  0
035200040303     D  spevid                 1     16  0 inz
035300040216     D                 DS
035400040219     D  WRKlnp                        3
035500040219     D  WRKnrs                        2
035600040219     D  WRKnsp                        7
035700040219     D  WRKann                        4
035800040219     D  WRKSPE                 1     16
035900040216      * - DS per girare le date
036000040216     D                 DS
036100040216     D  anno4                  1      4  0
036200040216     D  anno2                  3      4  0
036300040216     D  mese                   5      6  0
036400040216     D  giorno                 7      8  0
036500040216     D  data8                  1      8  0
036600040216
036700040212      * - DS Parametri per ricerca Punto Operativo
036800040212     D Dstnsd          DS
036900040212     D  §cod                          3
037000040212     D  §tip                          1
037100040212     D  §des                         25
037200040212     D  §pra                          3
037300040420
037400040420      * - DS Parametri per ricerca Programmi chiamanti
037500040420     D DSStack         ds
037600040420     D  £Stack                       10    Dim(100)
037700040212
037800040426      * - DS Controllo giorni festivi
037900040426     d clnmat          ds
038000040426     d  mat                    1     31    dim(31)
038100040426     d clnpom          ds
038200040426     d  pom                    1     31    dim(31)
038300150317
038400150317     D wFIL_ds         ds
038500150317     d   sk_wFIL               1    750    dim(250)
038600151130
038700151130     d wFILest_ds      ds
038800151130     d   sk_FILest             1    750    dim(250)
038900150114
039000040107      *
039100040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
039200040107      *
039300040109      * - Campi di comodo
039400040108     d w0140           s             14  0
039500040108     d wData           s              8  0
039600040120     d wOra            s              6  0
039700040108     d DateU           s              8  0
039800040213     d ora             s              2  0
039900040213     d minuti          s              2  0
040000040426     d wgiorno         s              2  0
040100151201     d w0090           s              9  0
040200041206     d w0070           s              7  0
040300041206     d w0040           s              4  0
040400130611     d w003a           s              3
040500040212     d wtibs69r        s              1
040600040212     d w1cdar          s              8  0
040700040217     d w1corr          s              6  0
040800040213     d wmod            s              1
040900040217     d rec_wrt         s              3  0
041000040217     d s01nrr          s              4  0
041100040217     d wdar            s              8  0
041200070329     d wdcr            s              8  0
041300040426     d giorni          s              3  0
041400041206     d wforz           s              1
041500040218     d wrk_recsbf      s              1
041600040218     d Wfidnb0r        s              1
041700070327     d W_cau           s              1
041800040219     d W_confe         s              1
041900040323     d W_ute           s              3
042000040617     d W_pot           s              3  0
042100070330     d Not_dcr         s              1
042200070330     d Not_ldv         s              1
042300141210     d Not_PINcode     s              1    inz
042400070402     d w_dtcr          s             11
042500040531     d forz_rec        s              1
042600041213     d mod_testa       s              1
042700050119     d Sv_inkd         s              1
042800170503     d UteGia          s              1                                         Valore P o A
042900070330     d Note_dcr        s             78
043000170505     d Note_gia        s             70
043100130610     d wabi            s                   like(UteAut)
043200150313     d w0030           s              3  0
043300150313     d sav_in48        s              1a
043400150316     d wtmo            s                   like(TGMtmo)
043500150422     d $Video          s              2    inz
043600150422     d $GesW04         s               n   inz
043700160128     d $GesW04N        s               n   inz
043800150422     d $Reparto        s               n   inz
043900160104     d $SaveF6         s               n   inz
044000150421     d wwMail          s                   like(V1mail)  inz
044100150421     d wwcMAD          s                   like(V1Cmad)  inz
044200150422     d wwdMAD          s                   like(V1Dmad)  inz
044300150421     d wwcPOG          s                   like(V1Cpog)  inz
044400150422     d wwdPOG          s                   like(W4Dpog)  inz
044500150423     d wwcRPR          s                   like(W4Crpr)  inz
044600150423     d wwdRPR          s                   like(W4Drpr)  inz
044700160201     d WrepR           s                   like(V1CrepR) inz
044800160201     d WfilR           s                   like(V1CfilR) inz
044900170519     d wAAS            s                   like(ARBaas) inz
045000170519     d wLNP            s                   like(ARBlnp) inz
045100170519     d wNRS            s                   like(ARBnrs) inz
045200170519     d wNSP            s                   like(ARBnsp) inz
045300170519     d LNP_abil        s                   like(ARBlnp) inz
045400170519     d LNA_abil        s                   like(ARBlna) inz
045500170519
045600170519
045700170221
045800170221     d Qcmd1           s             80    inz
045900170221     d Qlen1           s             15  5 inz(%len(Qcmd1))
046000070329
046100141212     d xx              s              3  0 inz
046200070329     d x               s              2  0
046300070329     d y               s              2  0
046400070329      *
046500040426
046600040426     d ktfp            s                   like(clntfp)
046700040426     d ktfa            s                   like(clntfa)
046800040426     d kann            s                   like(clnann)
046900040426     d kmes            s                   like(clnmes)
047000040324
047100070327     d W_LAV           s                   like(tgdmad) inz('  2')
047200070327     d W_CON           s                   like(tgdmad) inz(' 10')
047300170524     d W_GIA           s                   like(tgdmad) inz(' 12')
047400070327     d W_SUP           s                   like(tgdmad) inz(' 14')
047500070327     d W_APP           s                   like(tgdmad) inz(' 15')
047600070327     d W_FDE           s                   like(tgdmad) inz(' 16')
047700140429     d c_FDEAM         c                   const(' 17')
047800151130     d c_MotAp_Reso    c                   const(' 18')
047900141210     d c_VPINc         c                   const(' 60')
048000150717     d c_ReclSede      c                   const(' 70')
048100070327
048200040324     D Wdataeur        S               D   DATFMT(*eur)
048300040324     D Wdataoggi       S               D   DATFMT(*eur)
048400040216
048500040421     d Keyant          s                   like(tgdant)
048600040218     d Keynut          s                   like(tgdnut)
048700040218     d Keypdt          s                   like(tgdpdt)
048800040212     d Keytri          s                   like(tgrtri)
048900070402     d Keytin          s                   like(tgntin) inz('GEN')
049000040217     d W_v1cchi        s                   like(v1cchi)
049100041207     d Wrk_cchiA       s                   like(v1cchi)
049200041207     d Wrk_cchiB       s                   like(v1cchi)
049300041207     d Wrk_cchiC       s                   like(v1cchi)
049400041207     d Wrk_cchi        s                   like(v1cchi)
049500041206     d SW_v1cchi       s                   like(v1cchi)
049600070503     d SW_cau          s                   like(W_cau)
049700070508     d note_blocco     s                   like(tgnnot)
049800040218     d W_por           s                   like(orgfil)
049900070412     d W_Cmad          s                   like(v1cmad)
050000150122     d wSaveMAD        s                   like(V1Cmad) inz
050100040531     d W_v1cogg        s                   like(tgdogg)
050200040604     d SW_v1cogg       s                   like(tgdogg)
050300041210     d Sv_pdt          s                   like(v1cpdt)
050400141210     d wTGRpdt         s                   like(TGRpdt) inz
050500150326     d W_prt           s                   like(TGDprt)
050600150625     d wSavePRT        s                   like(TGDprt) inz
050700151217     d wSavePGN        s                   like(TGNpgn) inz
050800040217
050900070322      * - Flags booleani
051000131119     d $ric_fnlrg2r    s               n   inz(*off)                            richiamo FNLRG2R
051100131119     d $abilitato      s               n   inz(*off)                            abilitato a geo
051200131119     d $riemetti       s               n   inz(*off)                            riemissione
051300131119     d $bloccof3       s               n   inz(*off)                            blocco f3
051400160512     d $Note_scritte   s               n   inz(*off)                            indica la scrit.note
051500131206     d $cons           s               n   inz(*off)                            spediz.in consegna
051600131119     d $fnarb          s               n   inz(*off)                            trovato fnarb
051700160314     d $Giacenza       s               n   inz
051800140506     d $RA_MA_17       s               n   inz                                  RA con mot.ap. 17
051900151203     d $RA_MA_18       s               n   inz                                  RA con mot.ap. 18
052000141210     d $RA_MA_60       s               n   inz                                  RA con mot.ap. 60
052100170221     d $StrCmtCtl      s              1    inz(*off)
052200151201     d $Ok_IDCprg      s               n   inz
052300151202     d $WrtTIIDC       s               n   inz
052400160120     d $AttesaRisposta...
052500160120     d                 s               n   inz
052600040217      *
052700040217      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
052800040217      *
052900040217
053000040610     D TIT1            C                   CONST('*** IMMISSIONE RICHIESTE ***')
053100040610     D TIT2            C                   CONST('** MANUTENZIONE RICHIESTE **')
053200041213
053300140429     d c_CauVarB_I7    c                   const('I7')
053400150421
053500150421     d c_Sede          c                   const('046')
053600150311
053700150311     d Digits          c                   const('0123456789')
053800170221
053900170221     d C_StrCmtCtl     c                   const('StrCmtCtl +
054000170221     d                                            lcklvl(*CHG) +
054100170221     d                                            cmtscope(*JOB)')
054200170221     d C_EndCmtCtl     c                   const('EndCmtCtl')
054300170505     d C_ImmDisp       c                   const('Immessa Disposizione Giacenz+
054400170509     d                                            a      N.')
054500160105
054600160105       // -?Attributi di Visualizzazione?
054700160105     d c_DspAtr_ND     c                   const(x'27')
054800160105     d c_DspAtrNorm    c                   const(x'20')
054900160105     d c_DspAtr_RI     c                   const(x'21')
055000170221     d c_DspAtr_HI     c                   const(x'22')
055100160105     d c_DspAtr_BL     c                   const(x'28')
055200160105     d c_DspAtrWht     c                   const(x'22')
055300160105     d c_DspAtrWht_RI  c                   const(x'23')
055400160105     d c_DspAtrWht_UL  c                   const(x'26')
055500160105     d c_DspAtrRed     c                   const(x'28')
055600160105     d c_DspAtrRed_RI  c                   const(x'29')
055700160105     d c_DspAtrRed_BL  c                   const(x'2A')
055800160105     d c_DspAtrRed_UL  c                   const(x'2C')
055900140123
056000140123       //--------------------------------------------------------------
056100140123       //?Definizione prototipi e procedure.
056200140123       //--------------------------------------------------------------
056300140123
056400141210       // -?Reperimento dati tabelle?
056500141210      /copy gaitrasrc/srcProtoPI,TRULTAB
056600141210      /copy gaitrasrc/srcProtoPR,TRULTAB
056700151201
056800151201       // -?Reperimento/Aggiornamento Numeratori in GRU?
056900151201      /copy gaitrasrc/srcProtoPR,TRUL33R
057000141210
057100141210       // -?Gestione ricerca / Controllo tabelle?
057200150911      /copy gaitrasrc/srcProtoPR,TIBS02R
057300160708
057400160708      *// // -?Aggiunge/Toglie gg/mm dalla data?
057500160708     d*// XGIOLAV         pr                  extpgm('XGIOLAV')
057600160708     d*//   xGioLAVds                         likeds(XGIOLAVds)
057700140429
057800140924       // -?Variazione bolle in arrivo?
057900140924      /copy gaitrasrc/srcProtoPR,FNLR48R
058000140429
058100140429       // -?Controllo Causali Variazione bolle?
058200140429      /copy gaitrasrc/srcProtoPR,FNLR66R
058300151130
058400151130       // -?Reperimento Disposizioni di Consegna da WEB?
058500151130     d FNLRY09ds2    e ds                  inz
058600151130     d FNLRY09R        pr                  extpgm('FNLRY09R')
058700151130     d   kpjba                             likeds(KPJBA)
058800141210
058900141210       // -?Richiesta e Controllo del PINcode?
059000141210     d trulPINds     e ds                  inz
059100141210      /copy gaitrasrc/srcProtoPR,TRULPINR
059200150310
059300170518       // -?Recupero bolla mamma e ultima bolla figlia
059400170518     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
059500170518     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
059600170518
059700150310       // -?Immissione ORM
059800150310     d FIOR05R         pr                  extpgm('FIOR05R')
059900150911     d   kpjba                             likeds(kpjba)
060000150911     d   fior05ds                          likeds(FIOR05DS)
060100150318
060200160105       // -?Storico Variazioni
060300150318     d FIDNA3R         pr                  extpgm('FIDNA3R')
060400150911     d   kpjba                             likeds(kpjba)
060500150911     d   fidna0ds                          likeds(FIDNA0DS)
060600150317
060700150317       // -?Caricamento filiali abilitate
060800150911     d TRUL31R         pr                  extpgm('TRUL31R')
060900150911     d   kpjba                             likeds(KPJBA)
061000150911     d   trul31ds                          likeds(TRUL31DS)
061100150319
061200150508       // -?Selezione tab. "RGR" = Reparto Gestione R.A.?
061300150508     d tntbRGRds     e ds                  inz
061400150508     d tntbRGRr        pr                  extpgm('TNTBRGRR')
061500150508     d   kpjba                             likeds(kpjba)
061600150508
061700150319       // -?Selezione tab. "UGR" = Utenti
061800150319     d tntbUGRds     e ds                  inz
061900150319     d tntbUGRr        pr                  extpgm('TNTBUGRR')
062000150319     d   kpjba                             likeds(kpjba)
062100150915
062200150915      *// // -?Interrogazione CAP?
062300150915     d*// TISI00ds      e ds                  inz
062400150915     d*// tisi02r         pr                  extpgm('TISI02R')
062500150915     d*//   kpjba                             likeds(kpjba)
062600150915     d*//   tisi00ds                          likeds(TISI00ds)
062700150915
062800150915       // -?Interrogazione Località?
062900150915     d TISI00ds      e ds                  inz
063000150915     d TISI30ds      e ds                  inz
063100150915     d tisi30r         pr                  extpgm('TISI30R')
063200150915     d   kpjba                             likeds(kpjba)
063300150915     d   tisi00ds                          likeds(TISI00ds)
063400150915     d   tisi30ds                          likeds(TISI30ds)
063500150911
063600150911       // -?Controllo CAP ("Pretrullo TISI95R")?
063700150911     d TISI95ds      e ds                  inz
063800150911     d tisi95r         pr                  extpgm('TISI95R')
063900150911     d   tisi95ds                          likeds(TISI95ds)
064000150915
064100151130       // -?Reperimento Terminal di partenza/arrivo?
064200150915     d FNLV55ds      e ds                  inz
064300150915     d   D55tla      e                     inz('L')
064400150915     d   D55tpt      e                     inz('P')
064500150915     d fnlv55r         pr                  extpgm('FNLV55R')
064600150915     d   fnlv55ds                          likeds(FNLV55ds)
064700170113
064800170113       // -?Parametri API QCAPCMD (Process Commands)?
064900170113     d Qcmd            s           2048    inz  varying
065000170113      /copy qSysInc/qRpgleSrc,QCAPCMD
065100170113       // -?API QCAPCMD (Process Commands)?
065200170113      /copy gaitrasrc/srcProtoPR,QCAPCMD
065300170113
065400170113       // -?Parametri gestione errori API.?
065500170113      /copy qsysinc/qrpglesrc,QUSEC
065600140429
065700040323      *
065800040108      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
065900040108      *
066000151201      * -?File TIIDC02L?
066100151201     d keyTIIDC02    e ds                  extname( TIIDC02L : *key )
066200151201     d                                     prefix( k_ )  inz
066300040120      * - file FITGD01L
066400040421     c     ktgdan        klist
066500040421     c                   kfld                    v1cant
066600040421     c                   kfld                    v1cnut
066700040218      *
066800040218     c     ktgdrec       klist
066900040421     c                   kfld                    Keyant
067000040218     c                   kfld                    Keynut
067100040218     c                   kfld                    Keypdt
067200040212      * - file FITGR01L
067300040421     c     ktgran        klist
067400040421     c                   kfld                    v1cant
067500040421     c                   kfld                    v1cnut
067600141210     c                   kfld                    wTGRpdt
067700040421      *
067800040421     c     ktgran2       klist
067900040421     c                   kfld                    tgdant
068000040421     c                   kfld                    tgdnut
068100141210     c                   kfld                    wTGRpdt
068200151215      *
068300151215     c     keyFITGR01    klist
068400151215     c                   kfld                    TGDant
068500151215     c                   kfld                    TGDnut
068600151215     c                   kfld                    TGDpdt
068700151215     c                   kfld                    TGRtri
068800040421      *
068900150325      * - file FITGD32L
069000150325     c     ktgd32        klist
069100040531     c                   kfld                    v1ctor
069200040531     c                   kfld                    W_v1cogg
069300070402      * - file FITGN01L
069400070402     c     ktgn          klist
069500070402     c                   kfld                    v1cant
069600070402     c                   kfld                    v1cnut
069700070402     c                   kfld                    v1cpdt
069800070402     c                   kfld                    keytin
069900040107
070000070502      * - file FNARB01L - FNBLP01L -
070100040210     c     ksped         klist
070200040210     c                   kfld                    v1anns
070300040210     c                   kfld                    v1lnps
070400040210     c                   kfld                    v1nrss
070500040210     c                   kfld                    v1nsps
070600041209
070700041209     c     kspedw        klist
070800041209     c                   kfld                    W_aas
070900041209     c                   kfld                    W_lnp
071000041209     c                   kfld                    W_nrs
071100041209     c                   kfld                    W_nsp
071200040211      * - file FNORM01L
071300040211     c     korm          klist
071400040211     c                   kfld                    v1poeo
071500040211     c                   kfld                    v1nsor
071600040211     c                   kfld                    v1norm
071700040211     c                   kfld                    v1nrvo
071800041209
071900041209     c     kormw         klist
072000041209     c                   kfld                    W_poe
072100041209     c                   kfld                    W_nsr
072200041209     c                   kfld                    W_nor
072300041209     c                   kfld                    W_nrv
072400040426      * - file AZCLN01L
072500040426     c     kazcln        klist
072600040426     c                   kfld                    ktfp
072700040426     c                   kfld                    ktfa
072800040426     c                   kfld                    kann
072900040426     c                   kfld                    kmes
073000040210
073100040107      *===============================================================*
073200040107      *  RIEPILOGO INDICATORI                                         *
073300040107      *---------------------------------------------------------------*
073400040126      *  01    - Visualizza numero spedizione                         *
073500040126      *  02    - Visualizza numero Orm                                *
073600040126      *  03    - Visualizza chiave generica                           *
073700170220      *  04    - Abilita tasto gestione giacenza                      *
073800041005      *  05    - Sono in Sede                                         *
073900041012      *  07    - Utente abilitato a modificare tutto                  *
074000070608      *  08    - Filiale abilitata a GEO                              *
074100070625      *  09    - Bolla in partenza non borderizzata                   *
074200040324      *  10    - Posizionamento cursore                               *
074300050118      *  11    - Posizionamento cursore                               *
074400040421      *  15    - Indicatore  da video numero reclamo                  *
074500131206      *  16    - Spedizione in consegna non visualizza F17 Note Aut   *
074600150319      *  17    - Visualizzo F11=Variazioni                            *
074700040126      *  20    - SFLDSPCTL in C01                                     *
074800040126      *  21    - SFLDSP    in C01                                     *
074900151211      *  21    - SFLEND    in C01                                     *
075000040202      *  22    - SFLCLR    in C01                                     *
075100151211      *  27    - Comodo x lookup in chiera WPOG                       *
075200040109      *  28    - Errore generico dspf                                 *
075300040212      *  29    - Visualizzo dicitura "Altre note"                     *
075400120919      *  30/48 - Errore                                               *
075500150420      *  49    - Immissione: non gestisce Filiale/Reparto/Utente gest.*
075600040303      *  50    - Manutenzione usato per proteggere/non visualizzare   *
075700040303      *          campi e comandi video                                *
075800040324      *  51/54 - Protezione note                                      *
075900150421      *  55    - Errore - n° Telefono in W04                          *
076000150911      *  56    - Errore - C.A.P. errato in W04                        *
076100151223      *  57    - Posiz. cursore in W04/C01 su Reparto x Attesa Risp.  *
076200151223      *  58    - Posiz. cursore in W04/C01 su Filiale x Attesa Risp.  *
076300070328      *  60    - Protezione note dopo richiamo gestione blocchi       *
076400070329      *  61    - emissione data consegna richiesta                    *
076500151211      *  62    - Tipo/Data/Ora consegna richiesta errato/a            *
076600070403      *  63    - Non si può più dare f3                               *
076700151211      *  64    - ERRORE ora richiesta consegna                        *
076800040324      *  70    - Protezione ... non si può modificare nulla           *
076900041012      *  72    - Manutenzione e non è stato selezionato nulla         *
077000150624      *  73    - Protezione ... non si può modificare Tipo Comunicaz. *
077100070326      *  74    - Protezione ... non si può modificare motivo apertura *
077200150624      *  75    - Comodo in lookup                                     *
077300040220      *  79    - Errore campo non numerico                            *
077400040707      *  80    - Oggetto inesistente                                  *
077500040305      *  81    - Posizionamento cursore Opzione subfile               *
077600040422      *  82    - Reverse immage cursore record allocato               *
077700141215      *  83    - Campo opzione (nel sfl) protetto & non visualizzato  *
077800150317      *  84    - RA non manutenzionabile dall'utente                  *
077900040109      *  90    - Errore                                               *
078000040107      *===============================================================*
078100040107
078200040129      *
078300150319     c                   eval      *in17 = *off
078400040129      *
078500040129     c                   do        *hival
078600040213      * se sono in modalità immissione
078700040213     c                   if        wmod = 'I'
078800040213      * Pulizia del video
078900040213     C                   exsr      SR_PUL01
079000040213      *
079100040213     c                   else
079200040213      * carico il reclamo da gestire
079300040303     c                   exsr      SR_RECupero
079400040213      *
079500040213     c                   endif
079600040213
079700040213      * gestione videata di immissione/manutenzione reclamo
079800040213
079900040129     c                   exsr      SR_RECLAMO
080000041214      * verifico se modificata la testa aggiorno i file nella parte della testata
080100041214     c                   if        Mod_testa = 'S'
080200041214     c                   exsr      SR_chiudo
080300041214     c                   endif
080400040213
080500040129      * fine programma F3
080600040213
080700040304     c                   if        *inkc  OR (*INKL and *IN50)
080800041213      *
080900040315     c                   if        *inkc and *in50
081000041213      *
081100041213      * se sono stata richiamata valorizzo la ds FIDNA0DS
081200040305     c                   eval      oa0f03 = 'S'
081300040305     c                   endif
081400040305
081500040129     c                   leave
081600040129     c                   endif
081700040217      *
081800040129     c                   enddo
081900040202
082000040202     C                   IF        Wtibs69r  <>  *BLANKS
082100040202     C                   eval      I69TLA  = 'C'
082200040202     C                   call      'TIBS69R'
082300040202     C                   parm                    TIBS69DS
082400040202     C                   ENDIF
082500040617      *
082600040617     c                   clear                   tibs02ds
082700040617     c                   movel     'C'           t02tla
082800040617     c                   call      'TIBS02R'
082900040617     c                   parm                    kpjba
083000040617     c                   parm                    tibs02ds
083100040617      *
083200040617     c                   clear                   FIDNB0DS
083300040617     c                   movel     'C'           ib0tla
083400040617     c                   movel     FIDNB0DS      kpjbu
083500040617     c                   call      'FIDNB0R'
083600040617     c                   parm                    kpjba
083700040617     c                   parm                    FIDNB0DS
083800150928      *
083900150928     c                   clear                   fidnCAds
084000150928     c                   eval      IC0tla = 'C'
084100150928     c                   call      'FIDNCAR'
084200150928     c                   parm                    fidnCAds
084300040617      *
084400141210     c                   callb     'CLOTABELLA'
084500141210     c                   parm      c_Tabel       wFilTab           1
084600170113      *
084700170113     c                   clear                   TRUL33ds
084800170113     c                   eval      I33tla = 'C'
084900170113     c                   movel(p)  TRUL33ds      KPJBU
085000170113     c                   call      'TRUL33R'
085100170113     c                   parm                    KPJBA
085200170113      *
085300170113      * -?Cancellazione override sul *file AZNUM01L?
085400170113      *  ?(usato dal *pgm chiamato TRUL33R)?
085500170113      *  ?per limitare il "Tempo massimo attesa record"?
085600170113     c                   exsr      sr_DltOvrAZNUM
085700040129      *
085800040129     c                   seton                                        lr
085900160607      *
086000040129      *---------------------------------------------------------------*
086100040129      * Stacco il numero dal numeratore                               *
086200040129      *---------------------------------------------------------------*
086300040129     c     SR_NUMERO     BegSR
086400040129      *
086500040421     c                   time                    w0140
086600040421     C                   MOVE      W0140         V1CANT
086700160302      *
086800040129     C                   clear                   Trul33ds
086900170113     c*//                eval      i33tla = 'L'
087000040129     C                   z-add     0             I33ope
087100141020     C                   z-add     039           I33cnu
087200040129     C                   z-add     1             I33num
087300040421     C                   z-add     v1cant        I33aaa
087400160302      *
087500160302     c                   DoU       v1cnut > *zeros
087600160302      *
087700040129     C                   movel(p)  trul33ds      kpjbu
087800041007      * se sono in sede richiamo il CL
087900040129     C                   CALL      'TRUL33R'
088000040129     C                   PARM                    KPJBA
088100040129     C                   eval      trul33ds = kpjbu
088200160302      * SE num. "39" allocato: si prova con il "38"
088300160302     c                   IF        o33err = 02
088400160302     c                   z-add     038           I33cnu
088500160302     c                   movel(p)  trul33ds      kpjbu
088600160302     c                   call      'TRUL33R'
088700160302     c                   parm                    KPJBA
088800160302     c                   eval      trul33ds = kpjbu
088900160302     c                   ENDIF
089000040129      *
089100160302     c                   if        o33err = 02
089200160302     c                   eval      *in28 = *on
089300160302     c                   eval      *in41 = *on
089400160302     c                   movel     Msg(60)       V1Cmsg
089500160302     c                   leavesr
089600160302     c                   endif
089700160302      *
089800040129     C                   if        o33err = *zeros
089900040202     C                   z-add     o33nri        v1cnut
090000141020     c                   endif
090100160302      *
090200141020     c                   enddo
090300160302      *
090400040421      * spengo indicatore del video
090500040421     c                   setoff                                       15
090600040416      *
090700040421      * se data chiamata uguale a zero la imposto
090800040421      * imposto ora e data inserimento chiamata
090900040421     c                   if        v1cdar = *zeros
091000040416     c                   move      w0140         v1cdar
091100040421     c                   movel     w0140         v1corr
091200040421     c                   endif
091300040421
091400040416     c                   movel     w0140         W1corr
091500040416     c                   clear                   WLBdat
091600040422     c                   move      w0140         g02dat
091700040416     c                   call      'XSRDA8'
091800040416     c                   parm                    WLBdat
091900040416     c                   z-add     G02inv        w1cdar
092000040129      *
092100040129     c                   endsr
092200040202      *---------------------------------------------------------------*
092300040213      * Routine gestione SUBFILE - CONTROL   RECLAMO                  *
092400040202      *---------------------------------------------------------------*
092500040213     c     SR_RECLAMO    BegSR
092600040202
092700040202     c                   seton                                        20
092800150421     c                   eval      *in49 = ( wMod = 'I')
092900040202      *
093000040202     c                   Do        *Hival
093100070330     c                   write     fia1z01
093200040202      * emetto la videata
093300040202     c                   exfmt     fia1c01
093400040203     c                   setoff                                       281090
093500040212     c                   setoff                                       303132
093600040212     c                   setoff                                       333435
093700040212     c                   setoff                                       363738
093800040217     c                   setoff                                       394041
093900040217     c                   setoff                                       424344
094000070330     c                   setoff                                       461162
094100141029     c                   setoff                                       644748
094200170515     c                   setoff                                       5758
094300040202     c                   clear                   V1Cmsg
094400050120     c                   clear                   SV_inkd
094500111025     c                   clear                   $riemetti
094600040304
094700040304      * F12 = Ritorno al programma chiamante  (attivo solo per 50 acceso)
094800040304
094900040304     c                   If        *inkl
095000040304     c                   leave
095100040304     c                   endif
095200040216
095300040216      * F3 = Fine  se non sono in inserimento vado a fine pgm  altrimenti
095400040216      *            prima verifco che non sia stato inserito nulla altrimenti emetto
095500040216      *            un messaggio forzabile e pulisco tutto quello che è stato inserito fino adesso
095600040416      * ATTENZIONE TUTTO QUELLO CHE E' STATO INSERITO ANDRA' PERSO !!!! FORZARE F6
095700070328      *            ma solo se non è stato modificata una spedizione per lasciato avviso o per altri
095800070326      *            blocchi F3 è disabilitato
095900040223
096000040223     c                   If        *inkc
096100040223
096200040223     c                   If        Wmod <> 'I'  or
096300040223     c                             (wmod = 'I' and rec_wrt = 0 and
096400040308     c                             v1ctor = *blanks )
096500040305      *
096600040202     c                   leave
096700040202     c                   endif
096800070326      *
096900070326     c                   If        (wmod = 'I' and rec_wrt > 0) or
097000070326     c                             (wmod = 'I' and rec_wrt = 0 and
097100070326     c                             v1ctor <> *blanks )
097200070326     c                   exsr      SR_GesWin
097300040216      *
097400040216     c                   if        *inkf
097500040223     c                   eval      *inkf  = *off
097600040223     c                   eval      *inkc  = *on
097700040216     c                   leave
097800040216     c                   endif
097900040203
098000040216     c                   iter
098100040216     c                   endif
098200040223
098300040223     c                   endif
098400170116
098500170116      * F10=Immissione ORM Telefonici
098600170116      *?(Spostato PRIMA del reperimento numero R.A.)?
098700170116     c                   If        *inKJ
098800170116     c                   exsr      SR_F10
098900170116      * accendo posizionamento cursore su chiamante
099000170116     c                   seton                                        44
099100170116     c                   iter
099200170116     c                   endif
099300151211
099400040416      * se numero reclamo uguale a zero lo imposto
099500040416     c                   if        v1cnut = *zeros
099600040416     c                   exsr      SR_numero
099700160302     c                   if        *in28
099800160302     c                   iter
099900160302     c                   endif
100000040416     c                   endif
100100150609
100200150609      * F1=Interrogazione piano dei conti
100300150609
100400150609     c                   If        *inKA
100500150609     c                   exsr      SR_ricalf
100600150609      * accendo posizionamento cursore su chiamante
100700150609     c                   seton                                        44
100800150609     c                   iter
100900150609     c                   EndIf
101000040216
101100170116      *//* F10=Immissione ORM Telefonici
101200170116     c*//                If        *inKJ
101300170116     c*//                exsr      SR_F10
101400170116      *//* accendo posizionamento cursore su chiamante
101500170116     c*//                seton                                        44
101600170116     c*//                iter
101700170116     c*//                endif
101800150318
101900150318      /free
102000150318       //?F11=Visualizza Variazioni
102100150318         IF  *inkk;
102200150318           exsr sr_F11;
102300150318           *in44 = *on;   // posizione cursore su chiamante
102400150318           iter;
102500150318         ENDIF;
102600150318      /end-free
102700040213
102800040203      * Controllo il codice e recupero categoria del cliente
102900040203
103000040202     c                   if        v1cchi > 0 and W_v1cchi <> v1cchi
103100040203
103200040202      * aggancio anagrafico
103300040203
103400040202     C                   clear                   TIBS69DS
103500040202     c                   z-add     v1cchi        I69kac
103600040202     c                   z-add     v1cchi        I69kcp
103700040202     C                   call      'TIBS69R'
103800040202     C                   parm                    tibs69DS
103900040202     C                   parm                    DS_cnaco
104000040202     C                   parm                    DS_cnind
104100040202     C                   parm                    DS_cnclp
104200040202     C                   parm                    DS_fncls
104300040202     C                   eval      wtibs69r = '1'
104400040203     c                   If         O69ERR =  '1'
104500040203     C                   eval      *IN28 = *on
104600040203     c                   movel     msg(1)        V1Cmsg
104700040203     C                   eval      *IN30 = *on
104800040213     C                   iter
104900040202     c                   Else
105000050118     c                   movel     ACOrag        v1dchi
105100041206
105200130411     c                   eval      W_v1cchi = v1cchi
105300040202     c                   movel     clpclv        v1cclv
105400040202     c                   Endif
105500040203
105600040203      * se cambiato codice cliente oppure decodificato per la prima volta riemetto la videata
105700040203     c                   iter
105800040203
105900040202     c                   endif
106000151211
106100050119      * se KD prima faccio KF poi faccio kd
106200050119     c                   If        *inkd and v1ctor <> ' '
106300050119     c                   eval      SV_inkd = *inkd
106400050119     c                   eval      *inkd  = *off
106500050119     c                   eval      *inkf  = *on
106600050119     c                   endif
106700050119
106800040203      * richiamo la routine di controllo campi video
106900040203     c                   exsr      SR_CONTR01
107000040212      * se all'uscita della routine di controllo indicatore di errore generico
107100040212      * e di riemissione della videata è acceso riemetto la videata
107200040216     c                   if        *in90 or *in28
107300040212     c                   iter
107400040212     c                   endif
107500040212
107600040615      * F6 =Conferma riga  oggetto  non eseguo se 72 acceso
107700040212
107800040615     c                   If        *inKF and not *in72
107900040212     c                   exsr      SR_CONFE
108000040213      *
108100040213      * pulisco la parte del control relativa all'oggetto
108200040213      *
108300040213     c                   exsr      SR_pulctl
108400040305      * posizionamento cursore su tipo oggetto  se sono in modalità immissione
108500040305     c                   If        Wmod = 'I'
108600040213     c                   seton                                        31
108700040305      *
108800040305      * posizionamento cursore su opzione del subfile
108900040305     c                   else
109000141215     c                   eval      *in72 = *on
109100040305     c                   endif
109200040213      * carico subfile
109300040216     c                   exsr      sr_carsbf
109400040212     c                   endif
109500050119      *
109600050119     c                   if        sv_inkd = '1'
109700050119     c                   eval      *inkf = *off
109800050119     c                   eval      *inkd = *on
109900050119     c                   clear                   sv_inkd
110000050119     c                   exsr      SR_contr01
110100050119      * se all'uscita della routine di controllo indicatore di errore generico
110200050119      * e di riemissione della videata è acceso riemetto la videata
110300050119     c                   if        *in90 or *in28
110400050119     c                   iter
110500050119     c                   endif
110600050119     c                   endif
110700050119      *
110800040213
110900040213      * F4 = FINE CHIAMATA
111000040213
111100040216     c                   If        *inKD
111200040219      *
111300040219      * se devo scrivere un dettaglio prima di chiudere la chiamata il campo W_confe = ' '
111400040219      *
111500040219     c                   if        W_confe <> 'N'
111600040216     c                   exsr      SR_CONFE
111700040219     c                   eval      W_confe = ' '
111800040219     c                   endif
111900040219
112000040219     c                   exsr      SR_chiudo
112100040216
112200040216     c                   leave
112300040216
112400040216     c                   endif
112500040202      *
112600040203     c                   enddo
112700040202      *
112800040203     c                   endsr
112900040202      *
113000040203      *---------------------------------------------------------------*
113100040203      * Routine controllo campi video                                 *
113200040203      *---------------------------------------------------------------*
113300040203     c     SR_CONTR01    BegSR
113400040203
113500040203      *
113600040211    1c                   Do        *Hival
113700040219      *
113800040219      * se dato F4  FINE CHIAMATA
113900040219      *
114000041206    2c                   if        *inkd
114100070430      * controlli della parte alta della videata
114200070430     c                   exsr      sr_contrf4
114300070430     c                   if        *in28
114400070430     c                   leave
114500070430     c                   endif
114600070430      *
114700041206    2c                   endif
114800041206      *
114900040213
115000040213      * Controllo tipo comunicazione
115100040213
115200150625     c                   clear                   dCOR
115300040213    2c                   If        v1ctco <> *blank
115400040213      * Ricerca
115500040213     c     '?'           scan      v1ctco
115600040213    3c                   if        %found
115700040217     c                   clear                   tibs02ds
115800040213     C                   MOVEL     'R'           t02mod
115900040213     C                   MOVEL     knsif         t02sif
116000040213     C                   MOVEL     'COR'         t02cod
116100040213      *
116200040213     C                   CALL      'TIBS02R'
116300040213     C                   PARM                    KPJBA
116400040213     C                   PARM                    TIBS02DS
116500040213      *
116600040213    4C                   IF        T02err = *BLANKS
116700040217     C                   movel     T02uni        DCOR
116800040213     C                   movel     T02ke1        v1ctco
116900040213     C                   movel     §CORdesc      v1dtco
117000040223      *  errore (non ritorna nulla dalla selezione pulisco il campo)
117100040213     C                   ELSE
117200040223     c                   clear                   v1ctco
117300040223     c                   clear                   v1dtco
117400040213    4C                   ENDIF
117500040213      *
117600040213     c                   eval      *in90 = *on
117700040219     C                   eval      *IN41 = *on
117800040213     c                   leave
117900040213      *
118000040213   x3C                   ELSE
118100040213      *
118200040213      * Controllo
118300040217     c                   clear                   tibs02ds
118400040213     C                   MOVEL     'C'           t02mod
118500040213     C                   MOVEL     knsif         t02sif
118600040213     C                   MOVEL     'COR'         t02cod
118700040213     C                   MOVEL     v1ctco        t02ke1
118800040213      *
118900040213     C                   CALL      'TIBS02R'
119000040213     C                   PARM                    KPJBA
119100040213     C                   PARM                    TIBS02DS
119200040213      *
119300040213    4C                   IF        t02err = *BLANKS
119400040213     C                   movel     T02uni        DCOR
119500140923     c                   if        §CORTCUT =  'N'
119600140923     c                   eval      *in28 = *on
119700140923     c                   movel     MSG(43)       V1Cmsg
119800140923     C                   eval      *IN41 = *on
119900140923     c                   clear                   v1dtco
120000140923     c                   leave
120100140923     c                   endif
120200040213     C                   movel     T02ke1        v1ctco
120300040213     C                   movel     §CORdesc      v1dtco
120400040213      *
120500040213     C                   ELSE
120600040213     c                   eval      *in28 = *on
120700040223     c                   movel     MSG(17)       V1Cmsg
120800040213     C                   eval      *IN41 = *on
120900040213     c                   leave
121000040213    4C                   ENDIF
121100040213      *
121200040213    3c                   endif
121300040213      *
121400040213   x2c                   else
121500040213      *
121600040213      *  Campo Obbligatorio
121700040213     c                   eval      *in28 = *on
121800040213     c                   movel     MSG(10)       V1Cmsg
121900040223     C                   eval      *IN41 = *on
122000040213     c                   leave
122100040213    2C                   endif
122200040213
122300040213      * Controllo data e ora comunicazione
122400040213
122500040428    2C                   if        v1cdar =  *zeros
122600040428     c                   eval      *in28 = *on
122700040428     c                   movel     MSG(11)       V1Cmsg
122800040428     C                   eval      *IN42 = *on
122900040428     C                   leave
123000040428    2C                   else
123100040213     C                   clear                   wlbdat
123200040213     C                   z-add     v1cdar        G02dat
123300040213     C                   call      'XSRDA8'
123400040213     C                   parm                    wlbdat
123500040213    3C                   if        G02err = '1'
123600040213     c                   eval      *in28 = *on
123700040213     c                   movel     MSG(11)       V1Cmsg
123800040213     C                   eval      *IN42 = *on
123900040213     C                   leave
124000040213     C                   else
124100040213     C                   z-add     G02dat        v1cdar
124200040213     C                   z-add     G02inv        Wdar
124300040213    3C                   endif
124400040223      * verifico che la data non sia maggiore della data odierna
124500040223    3c                   if        wdar > dateu
124600040223     c                   eval      *in28 = *on
124700040223     c                   movel     MSG(21)       V1Cmsg
124800040223     C                   eval      *IN42 = *on
124900040223     C                   leave
125000040223    3c                   endif
125100040223
125200040213    2C                   endif
125300040213      * Ora comunicazione
125400040213    2C                   if        v1corr > *zeros
125500040213     C                   movel     v1corr        ora
125600040213     C                   move      v1corr        minuti
125700040213
125800040213    3C                   if        ora > 24 or minuti > 59
125900040213     c                   eval      *in28 = *on
126000040213     c                   eval      *in43 = *on
126100040217     C                   movel     msg(12)       v1cmsg
126200040213     c                   leave
126300040213    3C                   endif
126400040213
126500040213     C                   else
126600040213     c                   eval      *in28 = *on
126700040213     c                   eval      *in43 = *on
126800040217     C                   movel     msg(12)       v1cmsg
126900040213     c                   leave
127000040213    2C                   endif
127100040219
127200050119      * controllo del chiamante se uguale a blanks e sono in manutenzione
127300050119
127400050119    2c                   If        v1crsc = *blanks and *in50
127500050119     c                   eval      *in28 = *on
127600050119     c                   movel     MSG(15)       V1Cmsg
127700050119     C                   eval      *IN44 = *on
127800050119     c                   leave
127900050119    2c                   endif
128000040213
128100040213      * verifico se inserito indirizzo mail corretto
128200040213
128300040213    2c                   If        v1mail <> *blanks
128400150421     c                   eval      wwMail = V1mail
128500150421     c                   exsr      sr_CtrlMail
128600150421     c                   eval      V1mail = wwMail
128700150421     c                   if        *in28
128800150421     c                   leave
128900150421     c                   endif
129000040213    2c                   endif
129100151211
129200040203      *
129300040203      * gestione OGGETTO reclamo
129400040203      *
129500040203
129600160706      * F7 Bolle   Interrogazione e ricerca bolla
129700040203      *
129800040211    2C                   IF        *INKG
129900040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
130000040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
130100040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
130200040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
130300040223     c                   exsr      SR_GESWIN2
130400040223      * ritorno alla videata senza interrogare
130500040223     c                   If        *inKL
130600040223     c                   eval      *in90 = *on
130700150324     c                   if        wMod = 'I'
130800150114     c                   eval      *in48 = *on
130900150114     c                   else
131000040223     c                   eval      *in31 = *on
131100150114     c                   endif
131200040223     c                   leave
131300040223     c                   endif
131400040205      *
131500040223     c                   endif
131600040223      *
131700160706      * se non sono in sede interrogo le bolle
131800041005      *
131900041005   a2c                   if        not *in05
132000160701     c                   clear                   KPJBU
132100160701     c                   clear                   ParamU6ds
132200160701     c                   eval      ParamU6ds.PA1flg = '2'
132300160701     c                   eval      KPJBU = ParamU6ds
132400040205      * preparo l'altro parametro da passare
132500160701     c                   clear                   fnlrU6ds
132600160701     c                   eval      iLRU6sel = 'S'
132700160701     c                   eval      iLRU6ra  = 'S'
132800160701     c                   eval      iLRU6nzd = V1naz
132900160701     c                   eval      iLRU6prd = V1prov
133000160701     c                   eval      iLRU6rsd = V1Crsc
133100161027     c                   eval      iLRU6dtd = %dec( %date() - %days(7) )
133200161027     c                   eval      iLRU6dta = %dec( %date() )
133300160701      *
133400160701     c                   call      'FNLRU6R'
133500160701     c                   parm                    KPJBA
133600160701     c                   parm                    fnlrU6ds
133700160701     c                   movel     kpjbu         ParamU6ds
133800160701     c                   if        oLRU6err = 'E'
133900160719     c                   eval      *in28 = *on
134000160719     c                   eval      *in44 = *on
134100160719     c                   movel     oLRU6msg      V1Cmsg
134200160719     c                   leave
134300160701     c                   endif
134400040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
134500160701    3C                   IF        ParamU6ds.PA1f03 = *blanks  and
134600160701     C                             oLRU6tor         = 'S'      and
134700160701     C                             oLRU6err         = *blanks
134800040217     c                   eval      v1ctor = 'S'
134900160701     c                   movel     oLRU6ogg      DS_Spediz
135000040205     c                   seton                                        01
135100040205     c                   setoff                                       0203
135200040205     C                   z-add     DSspe_aas     V1anns
135300040205     C                   z-add     DSspe_lnp     V1lnps
135400040205     C                   z-add     DSspe_nrs     V1nrss
135500040205     C                   z-add     DSspe_nsp     V1nsps
135600140204      * verifico se spedizione in consegna
135700140204     c                   exsr      Ctr_specons
135800131206
135900040211    3C                   endif
136000041007      *
136100041007      * sono in sede
136200041007      *
136300041007     c                   else
136400041007     c                   clear                   fidna1ds
136500041007     c                   eval      IA1ant = v1cant
136600041007     c                   eval      IA1nut = v1cnut
136700041007     c                   eval      IA1pdt = v1cpdt
136800041007      *
136900041008     c                   eval      %SUBST(KPJBU:2:2) = '01'
137000041007      *
137100041007     c                   CALL      'TNSB50R'
137200041007     C                   PARM                    KPJBA
137300041007     C                   PARM                    FIDNA1DS
137400041007      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
137500041007    3c                   IF        IA1INS = 'S'
137600041007     c                   eval      v1ctor = 'S'
137700041007     c                   movel     IA1ogg        DS_Spediz
137800041007     c                   seton                                        01
137900041007     c                   setoff                                       0203
138000041007     C                   z-add     DSspe_aas     V1anns
138100041007     C                   z-add     DSspe_lnp     V1lnps
138200041007     C                   z-add     DSspe_nrs     V1nrss
138300041007     C                   z-add     DSspe_nsp     V1nsps
138400041007     c                   endif
138500041007      *
138600041005   a2c                   endif
138700150112      *
138800040212     c                   eval      *in90 = *on
138900150324if  3c                   if        wMod = 'I'
139000150114     c                   exsr      sr_ImpAut_Ins
139100150114x   3c                   else
139200150112     c                   eval      *in35 = *on
139300150114e   3c                   endif
139400040205     C                   LEAVE
139500040211    2C                   endif
139600040205      *
139700040205      * F09 ORM  Interrogazione e ricerca
139800040205      *
139900040211    2C                   IF        *INKI
140000040205      *
140100040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
140200040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
140300040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
140400040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
140500040223     c                   exsr      SR_GESWIN2
140600040223      * ritorno alla videata senza interrogare
140700040223     c                   If        *inKL
140800040223     c                   eval      *in90 = *on
140900150324     c                   if        wMod = 'I'
141000150114     c                   eval      *in48 = *on
141100150114     c                   else
141200040223     c                   eval      *in31 = *on
141300150114     c                   endif
141400040223     c                   leave
141500040223     c                   endif
141600040223      *
141700040223     c                   endif
141800040223      *
141900161107     c*//                clear                   FIOR010DS
142000161107     c*//                eval      IOR010ric  = '1'
142100161107     c                   reset                   FIOR010ds
142200160708      * -?Date di ritiro:?
142300160708      *  ?· DA = oggi -1?
142400160708     c                   clear                   XGIOLAVds
142500160708     c                   eval      iXGLdata = %int( %subst( %char( %dec(
142600160708     c                                              %timestamp() ) )
142700160708     c                                              : 1 : 8 ) )
142800160708     c                   eval      iXGLsub  = 'S'
142900160708     c                   eval      iXGLgg   = 1
143000160708     c                   eval      iXGLlav  = 'S'
143100160708     c                   eval      iXGLpa   = 'P'
143200160708     c                   eval      iXGLfil  = DUTpou
143300160708     c                   call      'XGIOLAV'
143400160708     c                   parm                    XGIOLAVds
143500160708     c                   if        oXGLerr  = *blanks
143600160708     c                   eval      iOR010darD = %dec( %date( oXGLdata : *iso ) )
143700160708     c                   else
143800160708     c                   eval      iOR010darD = iXGLdata
143900160708     c                   endif
144000160708      *  ?· A  = oggi +1?
144100160708     c                   clear                   XGIOLAVds
144200160708     c                   eval      iXGLdata = %int( %subst( %char( %dec(
144300160708     c                                              %timestamp() ) )
144400160708     c                                              : 1 : 8 ) )
144500160708     c                   eval      iXGLadd  = 'S'
144600160708     c                   eval      iXGLgg   = 1
144700160708     c                   eval      iXGLlav  = 'S'
144800160708     c                   eval      iXGLpa   = 'P'
144900160708     c                   eval      iXGLfil  = DUTpou
145000160708     c                   call      'XGIOLAV'
145100160708     c                   parm                    XGIOLAVds
145200160708     c                   if        oXGLerr  = *blanks
145300160708     c                   eval      iOR010darA = %dec( %date( oXGLdata : *iso ) )
145400160708     c                   else
145500160708     c                   eval      iOR010darA = iXGLdata
145600160708     c                   endif
145700160708      * -?Ragione sociale cliente Ritiro?
145800160708     c                   eval      iOR010rsr  = V1Crsc
145900160708      * -?Provincia cliente Ritiro?
146000160708     c                   eval      iOR010prr  = V1prov
146100160708      * -?Nazione cliente Ritiro?
146200160708     c                   eval      iOR010nar  = V1naz
146300160708      * -?Non Fatti?
146400161115     c                   eval      iOR010nonF = 'NO'
146500040205      * preparo l'altro parametro da passare
146600040205     c                   clear                   fidna1ds
146700040421     c                   eval      IA1ant = v1cant
146800040205     c                   eval      IA1nut = v1cnut
146900040205     c                   eval      IA1pdt = v1cpdt
147000040205      *
147100150701     c                   CALL      'FIOR010R'
147200040205     C                   PARM                    KPJBA
147300150701     C                   PARM                    FIOR010DS
147400040205     C                   PARM                    FIDNA1DS
147500040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
147600040211    3c                   IF        IA1INS = 'S'
147700040217     c                   eval      v1ctor = 'O'
147800040205     c                   movel     IA1ogg        DS_ORM
147900040205     c                   seton                                        02
148000040205     c                   setoff                                       0103
148100040205     C                   z-add     DSorm_poe     V1poeo
148200040217     C                   z-add     DSorm_nsr     V1nsor
148300040205     C                   z-add     DSorm_nor     V1norm
148400040205     C                   z-add     DSorm_nrv     V1nrvo
148500040211    3C                   endif
148600040212     c                   eval      *in90 = *on
148700150324     c                   if        wMod = 'I'
148800150114     c                   exsr      sr_ImpAut_Ins
148900150112     c                   else
149000040219     c                   eval      *in35 = *on
149100150112     c                   endif
149200040205     C                   LEAVE
149300040211    2C                   endif
149400040211      *
149500040211      * controllo la correttezza dei campi inseriti
149600040211      *
149700040218
149800040218      * se dato enter  e tutti i campi del  control (solo la parte relativa all'oggetto della
149900040218      * chiamato) non sono valorizzati e nel subfile c'è almeno un record controllo se è stata
150000040218      * richiesta la modifica di una riga già esistente
150100040218
150200040220    2c                   if        s01nrr > 0  and v1ctor = *blanks and
150300040218     c                             v1cogg = *blanks and ormvid = *zeros and
150400040218     c                             spevid = *zeros and v1ctch = *blanks and
150500040218     c                             v1cmad = *blanks and v1ccdt = *blanks and
150600040218     c                             v1not1 = *blanks and v1not2 = *blanks and
150700040218     c                             v1not3 = *blanks and v1not4 = *blanks and
150800040218     c                             v1crsp = *blanks and v1cpor = *blanks
150900040218
151000070326      * se dato F4 vado a fine altrimenti controllo se è stata fatta richiesta di gestione
151100040219      * riga di subfile
151200040220    3c                   if        *inkd
151300040219      * imposto un campo che mi dice di non eseguire la routine di conferma riga
151400040219     c                   eval      W_confe = 'N'
151500040219
151600040219     c                   leave
151700040219      *
151800040219     c                   else
151900040218     c                   exsr      SR_gessbf
152000040218
152100040218      * se scelto un record da manutenzionare esco dal ciclo per riemettere la videata con
152200040218      * i dati da manutenzionare altrimenti proseguo nei controlli
152300040220    4c                   If        Wrk_recsbf = 'Y'
152400040218     c                   eval      *in90 = *on
152500040422     C                   eval      *IN37 = *on
152600040422     C                   eval      *IN72 = *off
152700040218     c                   leave
152800040220    4c                   endif
152900040218
153000040220    3c                   endif
153100040219
153200040220    2c                   endif
153300151223      *
153400151223      * -?Non si eseguono i controlli se sono stati rilevati errori nella?
153500151223      *  ?gestione del SBF (vedi subr. sr_GesSBF)?
153600151223      *
153700151223    2c                   if        *in28
153800151223     c                   leave
153900151223    2c                   endif
154000040422      *
154100040615      * Non eseguo i controlli se sono in manutenzione e non ho selezionato nulla
154200040422      *
154300040422    2c                   If        *in72
154400040422     c                   leave
154500040422    2c                   endif
154600150112      *
154700040217      * verifico se inserito tipo oggetto
154800040217      *
154900040217    2c                   If        v1cTOR = *blank
155000040217     C                   eval      *IN28 = *on
155100040217     c                   movel     msg(13)       V1Cmsg
155200040217     C                   eval      *IN31 = *on
155300040217     c                   leave
155400040220      *
155500040220     c                   else
155600040220      * Ricerca
155700040220     c     '?'           scan      v1ctor
155800040220    3c                   if        %found
155900040220     c                   clear                   tibs02ds
156000040220     C                   MOVEL     'R'           t02mod
156100040220     C                   MOVEL     knsif         t02sif
156200040220     C                   MOVEL     'OGR'         t02cod
156300040220      *
156400040220     C                   CALL      'TIBS02R'
156500040220     C                   PARM                    KPJBA
156600040220     C                   PARM                    TIBS02DS
156700040220      *
156800040220    4C                   IF        T02err = *BLANKS
156900040220     C                   movel     T02uni        DOGR
157000040220     C                   movel     T02ke1        v1ctor
157100040220      *  errore
157200040220     C                   ELSE
157300040220     c                   clear                   v1ctor
157400040223     C                   eval      *IN31 = *on
157500040223     c                   eval      *in90 = *on
157600040223     c                   leave
157700040220    4C                   ENDIF
157800040220      *
157900040220      *
158000040220   x3C                   ELSE
158100040220      *
158200040220      * Controllo
158300040220     c                   clear                   tibs02ds
158400040220     C                   MOVEL     'C'           t02mod
158500040220     C                   MOVEL     knsif         t02sif
158600040220     C                   MOVEL     'OGR'         t02cod
158700040220     C                   MOVEL     v1ctor        t02ke1
158800040220      *
158900040220     C                   CALL      'TIBS02R'
159000040220     C                   PARM                    KPJBA
159100040220     C                   PARM                    TIBS02DS
159200040220      *
159300040220    4C                   IF        t02err = *BLANKS
159400040223     C                   movel     T02uni        DOGR
159500040220     C                   movel     T02ke1        v1ctor
159600040220      *
159700040220     C                   ELSE
159800040220     C                   eval      *IN28 = *on
159900040220     c                   movel     msg(13)       V1Cmsg
160000040220     C                   eval      *IN31 = *on
160100040220     c                   leave
160200070327    4C                   ENDIF
160300040220      *
160400040220    3c                   endif
160500040220      *
160600040223     c                   movea     §ogrccha      ccr
160700040217    2c                   endif
160800150112      *
160900040211      * tipo oggetto
161000151211      *
161100040211      *               generico e indicatore 03 spento riemetto la videata
161200040211      *
161300040217    2c                   If        v1ctor = 'G' and not *in03
161400040211     c                   setoff                                       0102
161500040217     c                   seton                                        0332
161600040223     c                   clear                   spevid
161700040223     c                   clear                   ormvid
161800170515     c                   eval      *IN04        = *off
161900040212     c                   eval      *in90 = *on
162000040211     c                   leave
162100040211    2c                   endif
162200151211      *
162300040211      *               spedizione e indicatore 01 spento riemetto la videata
162400151211      *
162500040217    2c                   If        v1ctor = 'S' and not *in01
162600040211     c                   setoff                                       0203
162700040217     c                   seton                                        0133
162800040223     c                   clear                   v1cogg
162900040223     c                   clear                   ormvid
163000040212     c                   eval      *in90 = *on
163100170515     c                   eval      *IN04        = *off
163200040211     c                   leave
163300040211    2c                   endif
163400151211      *
163500040211      *               spedizione e indicatore 02 spento riemetto la videata
163600151211      *
163700040217    2c                   If        v1ctor = 'O' and not *in02
163800040211     c                   setoff                                       0103
163900040217     c                   seton                                        0234
164000040223     c                   clear                   v1cogg
164100040223     c                   clear                   spevid
164200070412      * pulisco anche i campi per la gestione della data consegna
164300141029     c                   setoff                                       616264
164400070412     c                   eval      $ric_fnlrg2r = *off
164500070416     c                   eval      $fnarb       = *off
164600170220     c                   eval      *IN04        = *off
164700070412
164800040212     c                   eval      *in90 = *on
164900040211     c                   leave
165000040211    2c                   endif
165100040211      *
165200040707      * verifico se inserito oggetto
165300040211      *
165400040211      * generico
165500040211    2c                   If        *in03 and v1cogg = *blank
165600040211     C                   eval      *IN28 = *on
165700040211     c                   movel     msg(2)        V1Cmsg
165800040217     C                   eval      *IN32 = *on
165900170516     c                   eval      *IN04        = *off
166000040211     c                   leave
166100040211    2c                   endif
166200040211      * spedizione
166300040211    2c                   If        *in01 and (v1lnps = *zeros or v1nsps = *zeros
166400040211     c                             or v1anns = *zeros)
166500040211     C                   eval      *IN28 = *on
166600040211     c                   movel     msg(3)        V1Cmsg
166700040211     C                   eval      *IN33 = *on
166800040211     c                   leave
166900040211    2c                   endif
167000040217      *
167100040217      * verifico se inserito anno di 2
167200040217      *
167300040217    2c                   If        *in01 and  v1anns > 60 and v1anns < 100
167400040217     c                   add       1900          V1anns
167500040217    2c                   endif
167600040217
167700040217    2c                   If        *in01 and  v1anns <=  60 and v1anns < 100
167800040217     c                   add       2000          V1anns
167900040217    2c                   endif
168000040217
168100040211      * ORM
168200040211    2c                   If        *in02 and (v1poeo=*zeros or v1norm=*zeros)
168300040211     C                   eval      *IN28 = *on
168400040211     c                   movel     msg(5)        V1Cmsg
168500040211     C                   eval      *IN34 = *on
168600040211     c                   leave
168700040211    2c                   endif
168800040211      *
168900040211      * verifico se oggetto inserito esiste
169000040211      *
169100040211      * spedizione
169200151211      *
169300040211    2c                   If        *in01 and Wspevid <> spevid
169400070625     c                   eval      *in09 = *off
169500041005      * se non sono in sede
169600041005   a2c                   if        not *in05
169700040211      * verifico se esiste nelle bolle arrivi
169800040211      *
169900160104     c     KSPED         chain     Fnarb000
170000040211    3c                   If        not %found(Fnarb01l)
170100070416     c                   eval      $fnarb = *off
170200040211      * verifico se esiste nelle bolle partenze
170300040211      *
170400160104     c     KSPED         chain     Fnblp000
170500040211    4c                   If        not %found(Fnblp01l)
170600040707      * errore solo se non sono in manutenzione
170700040707    5c                   If        not *in50
170800040211     C                   eval      *IN28 = *on
170900040211     c                   movel     msg(4)        V1Cmsg
171000040211     C                   eval      *IN33 = *on
171100040211     c                   leave
171200040707      * accendo indicatore di non visualizzazine oggetto con F2
171300040707     c                   else
171400040707     c                   seton                                        80
171500040707    5c                   endif
171600070625      * se trovata in bolla partenza
171700070625     c                   else
171800070625      * verifico se si tratta di bolla ancora da borderizzare
171900070625     c                   eval      *in09 = (blpft2 = ' ' )
172000041209
172100041206    4c                   endif
172200070416      *
172300070416     c                   else
172400070416      * se trovato in arb
172500070416     c                   eval      $fnarb = *on
172600140204      * controllo e ripristino i campi della spedizione in consegna
172700140204     c                   exsr      CTR_specons
172800041206    3c                   endif
172900041005      *
173000041005      * sono in SEDE
173100041005      *
173200041005  xa2c                   else
173300041005     c     KSPED         chain     Titas30c
173400041005    3c                   If        not %found(Titas30c)
173500041005      * errore solo se non sono in manutenzione
173600041005    4c                   If        not *in50
173700041005     C                   eval      *IN28 = *on
173800041005     c                   movel     msg(4)        V1Cmsg
173900041005     C                   eval      *IN33 = *on
174000041005     c                   leave
174100130513      * accendo indicatore di non visualizzazione oggetto con F2
174200041005     c                   else
174300041005     c                   seton                                        80
174400041005    4c                   endif
174500041005
174600041005    3c                   endif
174700041005      *
174800041005   a2c                   endif
174900040211      * mi salvo il numero spedizione
175000040211     c                   z-add     spevid        Wspevid
175100150114      * ...e pulisco quello dell'ORM
175200150114     c                   clear                   WormVid
175300070412      * pulisco anche i campi per la gestione della data consegna
175400141029     c                   setoff                                       616264
175500070412     c                   eval      $ric_fnlrg2r = *off
175600150114      *
175700150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
175800150324     c                   if        wMod = 'I'
175900150114     c                   exsr      sr_ImpAut_Ins
176000150114     c                   endif
176100070412
176200040211    2c                   endif
176300040211      *
176400040211      * ORM
176500151211      *
176600040211    2c                   If        *in02 and Wormvid <> ormvid
176700040211      * verifico se esiste in archivio
176800040211      *
176900160104     c     KORM          chain     Fnorm000
177000040211    3c                   If        not %found(Fnorm01l)
177100040211     c
177200040707      * errore solo se non sono in manutenzione
177300040707    4c                   If        not *in50
177400040211     C                   eval      *IN28 = *on
177500040211     c                   movel     msg(6)        V1Cmsg
177600040211     C                   eval      *IN34 = *on
177700040211     c                   leave
177800040707     c                   else
177900040707      * accendo indicatore di non visualizzazine oggetto con F2
178000040707     c                   seton                                        80
178100040707    4c                   endif
178200041209
178300040211    3c                   endif
178400040211      * mi salvo il numero ORM
178500040211     c                   z-add     ormvid        Wormvid
178600150114      * ...e pulisco quello della spedizione
178700150114     c                   clear                   WspeVid
178800150114      *
178900150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
179000150324     c                   if        wMod = 'I'
179100150114     c                   exsr      sr_ImpAut_Ins
179200150114     c                   endif
179300150114      *
179400040211    2c                   endif
179500170516      * se non è ne ORM ne spedizione pulisco i campi di salvataggio spedizione e orm
179600170516     c                   if        *in03
179700170516     c                   clear                   wspevid
179800170516     c                   clear                   wormvid
179900170516     c                   endif
180000170605
180100170605      * verifico se bolla legata a giacenza ma solo se bolla presente in ARB e se motivo
180200170605      * apertura è 12 =  Giacenza
180300170605     c                   If        $fnarb = *on and *in01 and
180400170605     c                             v1cmad = W_gia
180500170605     c                   exsr      Sr_CtrGiacenza
180600170605     c                   else
180700170605     c                   setoff                                       04
180800170605     c                   endif
180900170605     c
181000151211      *
181100040317      * F2   Interrogazione oggetto
181200040217      *
181300040707    2c                   IF        *inKB
181400040707      * se c'è 80 acceso significa che manca l'oggetto e quindi emetto l'errore
181500040707     c                   if        *in80
181600040707     C                   eval      *IN28 = *on
181700040707     c                   movel     msg(31)       V1Cmsg
181800040707     C                   eval      *IN31 = *on
181900040707     c                   leave
182000040707     c                   endif
182100040217      * verifico se tipo oggetto inserito
182200040217    3c                   if        v1ctor <> 'S' and v1ctor <> 'O'
182300040217     C                   eval      *IN28 = *on
182400040315     c                   movel     msg(22)       V1Cmsg
182500040217     C                   eval      *IN31 = *on
182600040217     c                   leave
182700040217    3c                   endif
182800070430      * richiamo la routine di interrogazione oggetto
182900070430     c                   exsr      SR_gesf2
183000070430      * se oggetto trovato riemetto la  videata
183100070430     c                   if        *in90
183200070430     c                   leave
183300070430     c                   endif
183400040217    2c                   endif
183500040531      *
183600040531      * se sono in immissione verifico se oggetto presente in altri reclami
183700041007      * se non è un oggetto generico
183800040604      * se modificato oggetto
183900041007    2c                   If        not *in03
184000041007      *
184100041007    3c                   If        forz_rec = 'Y'
184200040604
184300041007    4c                   if        *in01
184400041209     c                   movel(p)  spevid        SW_v1cogg
184500041007    4c                   endif
184600040604
184700041007    4c                   if        *in02
184800041209     c                   movel(p)  ormvid        SW_v1cogg
184900041007    4c                   endif
185000040604
185100041007    4c                   If        SW_v1cogg <> W_v1cogg
185200040604     c                   clear                   forz_rec
185300041007    4c                   endif
185400040604
185500041007    3c                   endif
185600040604
185700041007    3c                   If        not *in50 and WRK_recsbf <> 'Y' and
185800040531     c                             forz_rec = ' '
185900040531
186000041007    4c                   if        *in01
186100041209     c                   movel(p)  spevid        W_v1cogg
186200041007    4c                   endif
186300040531
186400041007    4c                   if        *in02
186500041209     c                   movel(p)  ormvid        W_v1cogg
186600041007    4c                   endif
186700040531
186800150325      * verfiico in FITGD32L
186900040531
187000160104     c     Ktgd32        chain     fitgd32
187100150325    4c                   if        %found(fitgd32l)
187200040531     c                   eval      *in28 = *on
187300040531     c                   movel     msg(30)       V1Cmsg
187400040531     C                   eval      *IN31 = *on
187500040531     C                   eval      forz_rec = 'Y'
187600040531     c                   leave
187700041007    4c                   endif
187800040531
187900041007    3c                   endif
188000041007
188100041007    2c                   endif
188200040211      *
188300040211      * verifico l'inserimento del CHI CHIAMA
188400040220      *
188500040220     c                   clear                   v1dtch
188600040220      *
188700040220    2c                   If        v1cTch = *blank
188800040220     C                   eval      *IN28 = *on
188900040220     c                   movel     msg(07)       V1Cmsg
189000040220     C                   eval      *IN35 = *on
189100040220     c                   leave
189200040220      *
189300040220     c                   else
189400040220      * Ricerca
189500040220     c     '?'           scan      v1ctch
189600040220    3c                   if        %found
189700040220     c                   clear                   tibs02ds
189800040220     C                   MOVEL     'R'           t02mod
189900040220     C                   MOVEL     knsif         t02sif
190000040220     C                   MOVEL     'CCR'         t02cod
190100040220      *
190200040220     C                   CALL      'TIBS02R'
190300040220     C                   PARM                    KPJBA
190400040220     C                   PARM                    TIBS02DS
190500040220      *
190600040220    4C                   IF        T02err = *BLANKS
190700040220     C                   movel     T02uni        DCCR
190800040220     C                   movel     T02ke1        v1ctch
190900040220     c                   movel     §ccrdesc      v1dtch
191000040220      *  errore
191100040220     C                   ELSE
191200040220     c                   clear                   v1ctch
191300040220     c                   clear                   v1dtch
191400040220    4C                   ENDIF
191500040220      *
191600040220     C                   eval      *IN35 = *on
191700040220     c                   eval      *in90 = *on
191800040220     c                   leave
191900040220      *
192000040220   x3C                   ELSE
192100040220      *
192200040220      * Controllo
192300150112     c                   exsr      sr_Ctrl_CCR
192400150112     c                   if        *in28
192500150112     c                   leave
192600150112     c                   endif
192700040220      *
192800040220    3c                   endif
192900040220      *
193000040220    2c                   endif
193100151211
193200040220      * verifico se chi chiama è ammesso per quel tipo oggetto
193300040220
193400150421    2c                   if        %lookup( V1Ctch : CCR ) = *zero
193500040220      * errore chi chiama non ammesso
193600040211     C                   eval      *IN28 = *on
193700040220     c                   movel     msg(16)       V1Cmsg
193800040211     C                   eval      *IN35 = *on
193900040211     c                   leave
194000040211    2c                   endif
194100040211      *
194200040211      * verifico l'inserimento del MOTIVO del reclamo
194300070412
194400070412      * se modificato rispetto al precedente pulisco alcuni campi
194500070412     c                   If        v1cmad <> W_cmad
194600070412     c                   eval      $ric_fnlrg2r = *off
194700141029     c                   setoff                                       616264
194800070412     c                   eval      W_cmad = v1cmad
194900141030     c                   clear                   v1ctcr
195000141030     c                   clear                   v1cdcr
195100141030     c                   clear                   v1cocr
195200070412     c                   endif
195300070412
195400150421      * - Ricerca / Controllo
195500150421     c                   eval      wwcMAD = V1Cmad
195600150421     c                   exsr      sr_CtrlMAD
195700150421     c                   eval      V1Cmad = wwcMAD
195800150422     c                   eval      V1Dmad = wwdMAD
195900150421     c                   if        *in28  or  *in90
196000150421     c                   leave
196100150421     c                   endif
196200140212      *
196300140212      * F17=Note LDV
196400140212      *
196500140217     c                   clear                   Not_LdV
196600140217     c                   if        Wmod = 'I'  and  V1Cmad = W_Lav
196700140217     c                                         and  $Cons  = *on
196800140217     c                   eval      Not_LdV = 'S'
196900140217     c                   endif
197000140217      *
197100140212if  2c                   If        *inKR
197200140217      * - SE "Immissione" & "Obbligo Immissione Note LdV"   oppure
197300140217      *   SE "Immissione" & "Motivo" = "  2" & Spedizione in Consegna
197400140217      *   => Note all'F6
197500140217     c                   if        (Wmod = 'I'  and  §MARnotLDV = 'S')  or
197600140217     c                             (Wmod = 'I'  and  V1Cmad = W_Lav
197700140217     c                                          and  $Cons  = *on)
197800140212     C                   eval      *in28 = *on
197900140212     c                   movel     Msg(40)       V1Cmsg
198000140212     c                   leave
198100140212     c                   endif
198200140212     c                   exsr      SR_F17
198300140212     c                   leave
198400140212e   2c                   endif
198500131119      *
198600070322      *  verifico se la causale mi invita all'inserimento della data consegna richiesta
198700070326      *  se non sono in modifica , non sono in sede e sono abilitato a GEO
198800070416      *  e se non ho già richiamato il pgm dei blocchi e se sono un oggetto spedizione  presente
198900070416      *  in arrivo
199000070330    2c                   if        §marrdco = 'S'  and not *in61
199100070402     c                             and not *in74  and dutlpo <> 'S'
199200070403     c                             and $abilitato = *on and $ric_fnlrg2r = *off
199300070416     c                             and v1ctor = 'S' and $fnarb = *on
199400070322      * richiamo la routine gestione data consegna
199500070322     c                   exsr      SR_GES_DTA
199600070329    3c                   if        $riemetti = *on
199700070327     c                   seton                                        90
199800070327     c                   leave
199900070329    3c                   endif
200000070329    2c                   endif
200100070329      * Controllo tipo a data consegna richiesta
200200070403    2c                   if        v1ctcr<> *blanks and *in61 and
200300141030     c                             v1cdcr = 0 and v1cocr = 0
200400070403     c                   eval      *IN28 = *on
200500070403     c                   movel     msg(11)       V1Cmsg
200600070403     c                   eval      *IN62 = *on
200700070403     c                   leave
200800070403    3c                   endif
200900141030      * Controllo ora consegna richiesta presente senza data
201000141030    2c                   if        *in61 and  v1cdcr = 0
201100141030     c                             and v1cocr > 0
201200141030     c                   eval      *IN28 = *on
201300141030     c                   movel     msg(11)       V1Cmsg
201400141030     c                   eval      *IN62 = *on
201500141030     c                   leave
201600141030    3c                   endif
201700070329    2c                   if        v1cdcr>0 and *in61
201800070329     c* controllo formale data
201900070329     c                   clear                   wlbdat
202000070329     c                   z-add     v1cdcr        g02dat
202100070329     c                   clear                   g02err
202200070329     c                   call      'XSRDA8'
202300070329     c                   parm                    wlbdat
202400070329      * data formalmente errata
202500070329    3c                   if        g02err = '1'
202600070329     c                   eval      *IN28 = *on
202700070329     c                   movel     msg(11)       V1Cmsg
202800070329     c                   eval      *IN62 = *on
202900070329     c                   leave
203000070329    3c                   endif
203100141029      * ora formalmente errata
203200141029     C                   if        v1cocr <> *zeros
203300141030     C                   movel     v1cocr        ora
203400141029     C                   move      v1cocr        minuti
203500141029
203600141030     C                   if        ora > 24 or minuti > 59
203700141030     C                             or ora < 0
203800141029     c                   eval      *IN28 = *on
203900141029     c                   movel     msg(44)       V1Cmsg
204000141029     c                   eval      *IN64 = *on
204100141029     c                   leave
204200141029     C                   endif
204300141030
204400141030     C                   endif
204500070329      * richiamo il controllo della data da FNLR48R
204600070329     c                   z-add     g02inv        Wdcr
204700070329     c                   z-add     g02dat        v1cdcr
204800070329     c                   clear                   fnlr48ds
204900070329     c                   eval      d48cvb='CR'
205000070329     c                   eval      d48ffr='F'
205100070329     C                   z-add     v1anns        D48AAS
205200070329     C                   z-add     v1lnps        D48LNP
205300070329     C                   z-add     v1nrss        D48NRS
205400070329     c                   z-add     v1nsps        D48NSP
205500070329     c                   movel     ARBCBO        D48CBO
205600070329     c                   movel     'A'           D48TBO
205700070329     c                   movel     'G'           D48trc
205800070329     c                   movel     fnlr48ds      KPJBU
205900140429     c                   clear                   dsARBd
206000070329     c                   movel     v1ctcr        §BGTCR
206100141029     c                   z-add     v1cocr        §BGHCR
206200070329     c                   z-add     wdcr          §BGDCR
206300070329     c                   movel     arbgc1        §BGGC1
206400070329     c                   movel     arbgc2        §BGGC2
206500070329     c                   movel     arbtc1        §BGTC1
206600070329     c                   movel     arbtc2        §BGTC2
206700070329     c                   call      'FNLR48R'
206800070329     c                   parm                    KPJBA
206900070329     c                   parm                    DSARBD
207000070329     c                   parm                    DSARBK
207100070329     c                   parm                    DARBT
207200070329     c                   parm                    DSARBG
207300070329     c                   parm                    trul90ds
207400070329     c                   movel     Kpjbu         fnlr48ds
207500070329      * ritorno errore da controllo fnlr48r
207600070402    3c                   if        d48err<>*blanks
207700070329     c                   movel     d48msg        v1cmsg
207800070329     c                   eval      *IN28 = *on
207900070329     c                   eval      *IN62 = *on
208000070329     c                   leave
208100070329    3c                   endif
208200070329    2c                   endif
208300120919
208400120919      * se il motivo di apertura RA prevede l'immissione delle note le richiedo
208500120919      * obbligatorie
208600120919     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
208700131017     c                             V1not3 = *blanks and V1not4 = *blanks and
208800120919     c                             §MARnote = 'S'
208900120919     c                   eval      *in28 = *on
209000120919     c                   eval      *in48 = *on
209100120919     c                   eval      V1Cmsg = msg(38)
209200120919     c                   leave
209300120919     c                   ENDIF
209400070329
209500120913      * se il motivo di apertura RA prevede una chiusura automatica
209600120913      * imposto il motivo di chiusura se non già impostato
209700120913     c                   IF        V1Ccdt = *blanks and §MARmotch <> *blanks
209800120913     c                   eval      V1Ccdt = §MARmotch
209900120913     c                   ENDIF
210000040212      *
210100040212      * verifico l'inserimento del CODICE di CHIUSURA del reclamo
210200040220      *
210300040220     c                   clear                   v1dcdt
210400040420     c                   clear                   v1crsp
210500040212    2c                   If        v1ccdt <> *blanks
210600120913
210700120913      * se il motivo di chiusura messo dell'utente è diverso da quello
210800150720      * previsto in tabella MAR errore  se l'utente non è abilitato
210900130705      * alla modifica
211000130705     c                   If        not *in07
211100120913     c                   IF        V1Ccdt <> §MARmotch and §MARmotch <> *blanks
211200120913     c                   eval      *in28 = *on
211300120913     c                   eval      *in37 = *on
211400120913     c                   eval      V1Cmsg = MSG(37)
211500131119     c                   eval      V1Cmsg = %trim(V1Cmsg) + ' "' +
211600120913     c                             %trim(§MARmotch) + '"'
211700120913     c                   leavesr
211800120913     c                   ENDIF
211900130705     c                   Endif
212000150720
212100150720      * -?Se R.A. con Motivo Apertura " 70": la Sede NON può chiuderla?
212200150720     c                   If        *in05  and  wwcMAD = c_ReclSede
212300150720     c                   eval      *in28 = *on
212400150720     c                   eval      *in37 = *on
212500150720     c                   eval      V1Cmsg = Msg(52)
212600150720     c                   leavesr
212700150720     c                   EndIf
212800120913
212900040212     c                   clear                   tibs02ds
213000040212      *
213100040212      * Ricerca
213200040212     c     '?'           scan      v1ccdt
213300040212    3c                   if        %found
213400040217     c                   clear                   tibs02ds
213500040212     C                   MOVEL     'R'           t02mod
213600040212     C                   MOVEL     knsif         t02sif
213700040212     C                   MOVEL     'CHR'         t02cod
213800040212      *
213900040212     C                   CALL      'TIBS02R'
214000040212     C                   PARM                    KPJBA
214100040212     C                   PARM                    TIBS02DS
214200040212      *
214300040212    4C                   IF        T02err = *BLANKS
214400040212     C                   movel     T02uni        DCHR
214500040212     C                   movel     T02ke1        v1ccdt
214600040212     C                   movel     §chrdesc      v1dcdt
214700040420     C                   movel     §chrresp      v1crsp
214800040223      *  errore (non è stato selezionato nulla pulisco  il campo e mi posiziono)
214900040212     C                   ELSE
215000040223     C                   clear                   v1ccdt
215100040223     C                   clear                   v1dcdt
215200040420     C                   clear                   v1crsp
215300040212    4C                   ENDIF
215400040212      *
215500040212     c                   eval      *in90 = *on
215600040219     C                   eval      *IN37 = *on
215700040212     c                   leave
215800040212      *
215900040212   x3C                   ELSE
216000040212      *
216100040212      * Controllo
216200040217     c                   clear                   tibs02ds
216300040212     C                   MOVEL     'C'           t02mod
216400040212     C                   MOVEL     knsif         t02sif
216500040212     C                   MOVEL     'CHR'         t02cod
216600040212     C                   MOVEL     v1ccdt        t02ke1
216700040212      *
216800040212     C                   CALL      'TIBS02R'
216900040212     C                   PARM                    KPJBA
217000040212     C                   PARM                    TIBS02DS
217100040212      *
217200040212    4C                   IF        t02err = *BLANKS
217300040212     C                   movel     T02uni        DCHR
217400040212     C                   movel     §CHRdesc      v1dcdt
217500040420     C                   movel     §chrresp      v1crsp
217600040212      *
217700040212     C                   ELSE
217800040212     c                   eval      *in28 = *on
217900040223     c                   movel     MSG(19)       V1Cmsg
218000040212     C                   eval      *IN37 = *on
218100040212     c                   leave
218200040212    4C                   ENDIF
218300040212      *
218400040212    3c                   endif
218500040212      *
218600040212    2C                   endif
218700151218      *
218800151218      *
218900150624      *?Impostazione STATO (ex Priorità):?
219000150624      * Se c'è la causale chiusura metto priorità '9' (in inserimento)
219100150624      *   o '8' (in modifica - se modificata la causale Chiusura)
219200160108if  2c                   IF        V1Ccdt <> *blanks
219300150624      *
219400160108if  3c                   IF        wmod = 'I'
219500150624      * -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
219600150624      *  ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
219700150625      *  ?(già reperita nella subr. SR_CONTR01)?
219800150624     c                   if        §CORstato <> *blanks
219900150624     c                   eval      V1Cprt = §CORstato
220000150624     c                   else
220100150313     c                   eval      V1Cprt = '9'
220200150624     c                   endif
220300160108x   3c                   ELSE
220400160108sel 4c                   Select
220500160108      * -?SE modalità Modifica & R.A. Chiusa & R.A. in attesa di?
220600160108      *  ?risposta (questa) => si imposta lo Stato "0"?
220700160108      *  ?(la R.A. rimane aperta, girata in Gestione alla fil. che?
220800160108      *  ?Attende Risposta)?
220900160108w   4c                   When      V1CfilR  > *zero         and
221000160201     c                             V1CrepR <> *blank        and
221100160108     c                             (V1Ccdt <> *blank        and
221200160108     c                              dTGDfar.§TGDris = *blank   or
221300160108     c                              V1Ccdt  = *blank        and
221400160108     c                              dTGDfar.§TGDris = 'S')
221500160108     c                   eval      V1Cprt = '0'
221600160108      * -?SE modificato il codice Chiusura => si imposta lo Stato "8"?
221700160108w   4c                   When      V1ccdt <> tgdcdt
221800150324     c                   eval      V1Cprt = '8'
221900160108e   4c                   EndSl
222000160108e   3c                   ENDIF
222100150624      *
222200150624      * Altrimenti (se NON c'è la causale di chiusura) metto
222300150624      *   priorita '0' (in inserimento) o '1' (in modifica)
222400160108x   2c                   ELSE
222500150624      *
222600150421     c                   If        wmod = 'I'
222700150421     c                   eval      V1Cprt = '0'
222800150421     c                   Else
222900150313     c                   eval      V1Cprt = '1'
223000150421     c                   EndIf
223100150624      *
223200160108e   2c                   ENDIF
223300150320      *
223400150320      * Decodifico (potrebbe essere cambiato)
223500150320     c                   clear                   tibs02ds
223600150320     C                   MOVEL     'C'           t02mod
223700150320     C                   MOVEL     knsif         t02sif
223800150320     C                   MOVEL     'PRR'         t02cod
223900150320     C                   MOVEL     v1cprt        t02ke1
224000150320      *
224100150320     C                   CALL      'TIBS02R'
224200150320     C                   PARM                    KPJBA
224300150320     C                   PARM                    TIBS02DS
224400150320      *
224500150320    4C                   IF        t02err = *BLANKS
224600150320     C                   movel     T02uni        DPRR
224700150320     C                   movel     §PRRdesc      v1dprt
224800150320     c                   endif
224900151217      *
225000150324      * se RA aperta richiedo note obbligatorie
225100150324     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
225200150324     c                             V1not3 = *blanks and V1not4 = *blanks and
225300151211     c                             V1Ccdt = *blanks
225400151211     c                             and not *inks
225500150324     c                   eval      *in28 = *on
225600150324     c                   eval      *in48 = *on
225700150324     c                   eval      V1Cmsg = msg(38)
225800150324     c                   leave
225900150324     c                   ENDIF
226000160107      *
226100160204      * -?Note obbligatorie per R.A. "chiusa" (poi ri-aperta automaticam.)?
226200160204      *  ?con Attesa Risposta = "S" & Ricevuta Risposta = " "?
226300160108      *  ?(con salvataggio del tasto funzionale F6)?
226400160108     c                   eval      $SaveF6  = *inKF
226500160204if  2c                   If        Not  *inKS             and
226600160204     c                             V1Ccdt <> *blank       and
226700160204     c                             dTGDfar.§TGDatt = 'S'  and
226800160204     c                             dTGDfar.§TGDris = *blank
226900160108      *
227000160204      * -?Reperisco ULTIMO Progressivo Note inserito?
227100160107     c     kTGN          setGT     FITGN000
227200160107     c     kTGN          readPE    FITGN000
227300160108if  3c                   if        %eof(FITGN01L)
227400160107     c                   clear                   TGNpgn
227500160108e   3c                   endif
227600160108      *
227700160108if  3c                   if        TGNpgn  <= wSavePGN
227800160107     c                   eval      *inKS    = *on
227900160108e   2c                   endif
228000160108      *
228100160108e   2c                   EndIf
228200040212      *
228300040212      * F18 - GESTIONE NOTE
228400040212      *
228500040212    2C                   IF        *INKS
228600040212      *
228700040212     c                   exsr      Sr_f18
228800040212      *
228900040219     c                   seton                                        39
229000040212     c                   eval      *in90 = *on
229100040212     c                   leave
229200040212      *
229300040212    2c                   ENDIF
229400150313
229500150313      * mi salvo l'indicatore 48 per posizionamento su campo note
229600150313     c                   eval      sav_in48 = *in48
229700150313      * spengo il 48
229800150313     c                   setoff                                       48
229900160104      *
230000160104      * -?Ripristino tasto funzionale F6?
230100160104     c                   eval      *inKF = $SaveF6
230200150311
230300150420      * Controllo la Filiale Gestione (SE gestita)
230400150428     c                   IF        wMod <> 'I'
230500150420      *
230600150421      * - Ricerca / Controllo
230700150421     c                   eval      wwcPOG = V1Cpog
230800150717     c*//?già impostata? eval      wwcMAD = V1Cmad
230900150421     c                   exsr      sr_CtrlPOG
231000150421     c                   eval      V1Cpog = wwcPOG
231100150421     c                   if        *in28  or  *in90
231200150421     c                   leave
231300150421     c                   endif
231400150420      *
231500150420     c                   ENDIF
231600040212      *
231700040323      * verifico l'inserimento dell'UTENTE che ha in GESTIONE il reclamo
231800150421      * (SE gestito)
231900150622     c                   clear                   $Reparto
232000150622      *
232100150428     c                   IF        wMod <> 'I'
232200150423      *
232300150422    2c                   If        V1Crpg <> *blanks
232400150421      *
232500150428      /free
232600150429
232700150429             // -?Ricerca/Controllo Reparto Gestione?
232800150617             //  ?(NON della Sede: per Sede si possono inserire solo Utenti)?
232900150617             If  V1Cpog <> c_Sede;
233000150617
233100150617               wwcRPR = V1Crpg;
233200150617               exsr  sr_CtrlRPR;
233300150617               V1Crpg = wwcRPR;
233400150617               //V1Drpg = wwdRPR;
233500150617
233600150617               if  *in90;
233700150617                 leave;
233800150617               endif;
233900150617
234000150617               $Reparto = ( Not *in28 );
234100150617
234200150617               *in28 = *off;
234300150617               *in39 = *off;
234400150617               *in45 = *off;
234500150617               clear  V1Cmsg;
234600150617
234700150617             EndIf;
234800150429
234900150429      /end-free
235000150421      *
235100150429if  3c                   IF        Not $Reparto
235200150421      *
235300150421      * 2° controllo come Utente Gestione
235400150421      *
235500040420      * verifico se esiste utente nell'archivio profili
235600040420     c     v1crpg        chain     azute01l
235700040420      *
235800150428    5c                   if        %found(azute01l) and UTEdsc > DateU
235900150311      * se lo trovo verifico se è della filiale di gestione
236000150723    6c                   if        %editc(UTEfil:'X') <> V1Cpog
236100040420     c                   eval      *in28 = *on
236200150313     c                   eval      *in46 = *on
236300040420     c                   movel     MSG(23)       V1Cmsg
236400040420     c                   leave
236500040420    6c                   endif
236600160607      *
236700150423    5c                   else
236800160607      *
236900040420     c                   eval      *in28 = *on
237000040420     c                   eval      *in39 = *on
237100150617     c                   if        V1Cpog <> c_Sede
237200150429     c                   movel     Msg(20)       V1Cmsg
237300150617     c                   else
237400150617     c                   eval      V1Cmsg = %trimR( Msg(20) )
237500150617     c                                    + ': ammessi solo utenti di sede'
237600150617     c                   endif
237700040420     c                   leave
237800040420    5c                   endif
237900150421      *
238000150421e   3c                   ENDIF
238100160607      *
238200150422   x2c                   Else
238300150421      * se Reparto/Utente a blanks do errore
238400040422     c                   eval      *in28 = *on
238500040422     c                   eval      *in39 = *on
238600040422     c                   movel     MSG(29)       V1Cmsg
238700040422     c                   leave
238800150422    2C                   EndIf
238900150422      *
239000150422 e_1bc                   ENDIF
239100160207      *//
239200160207      *//* se arrivo qua vuol dire che non ho avuto errori sui campi
239300160207      *//* filiale e utente gestione
239400160207      *//* quindi ripristino l'indicatore 48
239500160207      *//* => meglio farlo alla fine della subroutine...
239600160207     c*//                eval      *in48 = sav_in48
239700160203      *
239800160203      /free
239900160203
240000160203         // -?Confronto tra Reparto/Filiale in Attesa Risposta?
240100160203         //            ?con Reparto/Filiale di Gestione?
240200160203         If  dTGDfar.§TGDatt = 'S'  and
240300160203             V1Cpog = V1CfilR       and  V1Crpg = V1CrepR;
240400160203           *in28  = *on;
240500160203           *in46  = *on;
240600160203           V1Cmsg = Msg(59);
240700160203           leave;
240800160203         EndIf;
240900160203
241000160203      /end-free
241100040527      *
241200040527      * se responsabilità vettore  e P.O. responsabile non valorizzato metto me stesso
241300040527      * se responsabilità no vettore  e P.O. responsabile deve essere =  a blank
241400040527      *
241500040527    2c                   if        v1crsp = 'V'  and (v1cpor = *blanks
241600040527     c                             or v1cpor = '000')
241700130513     c                   select
241800130513     c                   when      §MARffr = 'A'
241900130513     c                   select
242000130513     c                   when      *in05
242100130513     c                   move      TASlna        V1Cpor
242200130513     c                   when      $fnARB = *on
242300130513     c                   move      ARBlna        V1Cpor
242400130513     c                   other
242500130513     c                   move      BLPlna        V1Cpor
242600130513     c                   endsl
242700130513     c                   when      §MARffr = 'P'
242800130513     c                   select
242900130513     c                   when      *in05
243000130513     c                   move      TASlnp        V1Cpor
243100130513     c                   when      $fnARB = *on
243200130513     c                   move      ARBlnp        V1Cpor
243300130513     c                   other
243400130513     c                   move      BLPlnp        V1Cpor
243500130513     c                   endsl
243600130513     c                   other
243700040527     c                   move      v1cpot        v1cpor
243800130513     c                   endsl
243900040527    2c                   endif
244000040527
244100040527    2c                   if        v1crsp <> 'V'
244200040527     c                   clear                   v1cpor
244300040527     c                   clear                   v1dpor
244400040527    2c                   endif
244500040211      *
244600040212      * verifico l'inserimento del P.O. responsabile
244700040220      *
244800040220     C                   clear                   v1dpor
244900040212    2c                   If        v1cpor <> *blanks
245000040212      *
245100040212      * Ricerca
245200040212     c     '?'           scan      v1cpor
245300040212    3c                   if        %found
245400040212     C                   clear                   dstnsd
245500040212     C                   call      'TNSD24R'
245600040212     C                   parm                    §cod
245700040212     C                   parm                    §tip
245800040212     C                   parm                    §des
245900040212     C                   parm                    §pra
246000040212     C                   movel     §cod          v1cpor
246100040212     C                   movel     §des          v1dpor
246200040212     c                   eval      *in90 = *on
246300040219     C                   eval      *IN40 = *on
246400040212     c                   leave
246500040212    3C                   endif
246600040217    2C                   endif
246700040217      *
246800040217     C     v1cpor        ifeq      *blanks
246900040217     C                   move      *all'0'       v1cpor
247000040217     C                   endif
247100040212      *
247200040212     C                   testn                   v1cpor               79
247300040212     C                   move      v1cpor        w001a             1
247400040217      *
247500040217     c                   move      v1cpor        W_por
247600040212      *
247700040217    2C     *in79         ifeq      *off
247800040212     C     w001a         orlt      '0'
247900040212     c                   eval      *in28 = *on
248000040212     c                   movel     MSG(9)        V1Cmsg
248100040212     C                   eval      *IN40 = *on
248200040212     c                   leave
248300040217    2C                   endif
248400040212      * verifico se P.O. esistente
248500040217    2c                   if        W_por > 0
248600160104     C     W_por         chain     azorg
248700040212    3C                   if        %found(azorg01l)
248800040212     C                   movel     orgdes        v1dpor
248900040420      * verifico se P.O. valido
249000040617     c                   if        orgfva<>' ' or (orgfag<>'F' and orgfag<>'A'
249100040617     c                             and orgfil <> 46)
249200040420     c                   eval      *in28 = *on
249300040420     c                   movel     MSG(9)        V1Cmsg
249400040420     C                   eval      *IN40 = *on
249500040420     c                   leave
249600040420     c                   endif
249700040212      *  errore
249800040212     C                   ELSE
249900040212     c                   eval      *in28 = *on
250000040213     c                   movel     MSG(9)        V1Cmsg
250100040212     C                   eval      *IN40 = *on
250200040212     c                   leave
250300040212    3C                   ENDIF
250400040212      *
250500040217    2C                   ENDIF
250600070608
250700070626      * controllo causale variazione per bolle se esiste  e se bolla valida per la variazione
250800070608
250900070608     c                   clear                   fnlr66ds
251000070608    2c                   if        V1ccva  <> *blanks
251100070626      * per 09 acceso emetto un msg di errore
251200160208     c* permetto SOLO in arrivo la modofica delle bolle non ancora borderizzate
251300160208    3c**                 If        *in09
251400160208     c**                 eval      *in28 = *on
251500160208     c**                 movel     MSG(34)       V1Cmsg
251600160208     C**                 eval      *IN47 = *on
251700160208     c**                 leave
251800160208    3c**                 endif
251900160208     c**
252000070608     c                   movel     V1anns        d66aas
252100070608     c                   movel     V1lnps        d66lnp
252200070608     c                   movel     V1nrss        d66nrs
252300070608     c                   movel     V1nsps        d66nsp
252400071018      * determino se sono il p.o. di partenza o il p.o. di arrivo
252500071018
252600160208      * se non trovata bolla in arb errore se la bolla in FNBLP è già partita
252700160208    3c******             If        $fnarb = *off
252800160208    3c                   If        $fnarb = *off and not *in09
252900071018     c                   eval      *in28 = *on
253000071018     c                   movel     MSG(35)       V1Cmsg
253100071018     C                   eval      *IN47 = *on
253200071018     c                   leave
253300071018    3c                   endif
253400071018
253500160208     c* Se la bolla deve ancora partire uso BLPLNA
253600160208     c                   if        *in09
253700160208     c                   move      blplna        w003a
253800160208     c                   else
253900130611     c                   move      arblna        w003a
254000160208     c                   endif
254100160208     c*
254200130611     c     w003a         lookup    wPOG                                   27
254300071018    3c                   if        %found
254400071018     c                   movel     'A'           d66tbo
254500071018   x3c                   else
254600071018      * verifico se sono la linea di partenza
254700160208     c                   if        *in09
254800160208     c                   move      blplnp        w003a
254900160208     c                   else
255000130611     c                   move      arblnp        w003a
255100160208     c                   endif
255200160208     c*
255300130611     c     w003a         lookup    wPOG                                   27
255400170512    4c                   if        %found
255500071018     c                   movel     'P'           d66tbo
255600071018    4c                   endif
255700071018
255800071018    3c                   endif
255900160208
256000160208    3c                   If        *in09  and d66tbo='P'
256100160208     c                   eval      *in28 = *on
256200160208     c                   movel     MSG(34)       V1Cmsg
256300160208     C                   eval      *IN47 = *on
256400160208     c                   leave
256500160208    3c                   endif
256600071018
256700071018      * se bolla non in gestione al Po Errore
256800071018    3c                   if        d66tbo = ' '
256900071018     c                   eval      *in28 = *on
257000071018     c                   movel     MSG(36)       V1Cmsg
257100071018     C                   eval      *IN47 = *on
257200071018     c                   leave
257300071018    3c                   endif
257400070608      *
257500070608     c     '?'           scan      v1ccva                                 90
257600070608      * Ricerca causale
257700070608    3c                   if        *in90
257800070608     c                   movel     'V'           d66ges
257900070608      *
258000070608     c                   movel     fnlr66ds      kpjbu
258100070608     c                   call      'FNLR66R'
258200070608     c                   parm                    kpjba
258300070608     c                   movel     kpjbu         fnlr66ds
258400070608     c                   movel     d66cvb        V1ccva
258500070608     c                   movel     d66des        v1dcva
258600070608   X3c                   else
258700070608      * Controllo causale inserita
258800070608     c                   movel     'C'           d66ges
258900160208     c* Posso variare anche le bolle in partenza per alcune causali
259000160208     c                   movel     'S'           d66BNP
259100070608     c                   movel     V1ccva        d66cvb
259200070608      *
259300070608     c                   movel     fnlr66ds      kpjbu
259400070608     c                   call      'FNLR66R'
259500070608     c                   parm                    kpjba
259600070608     c                   movel     kpjbu         fnlr66ds
259700070608     c                   movel     d66cvb        v1ccva
259800070608     c                   MOVEL     d66des        v1dcva
259900070608      *
260000070608      * Errore
260100070608    4c                   if        d66msg <> *blank
260200070608     c                   eval      *in28 = *on
260300070608     c                   movel     MSG(34)       V1Cmsg
260400070608     C                   eval      *IN47 = *on
260500070608     c                   leave
260600070608    4c                   endif
260700070608
260800070608    3c                   endif
260900070608      *
261000070608      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
261100070608      *
261200070608     c                   clear                   fnlr48ds
261300070608     c                   movel     V1anns        d48aas
261400070608     c                   movel     V1lnps        d48lnp
261500141029     c                   movel     V1nrss        d48nrs
261600070608     c                   movel     V1nsps        d48nsp
261700070608     c                   movel     Dutpou        d48fgs
261800070608     c                   If        $fnarb = *on
261900070608     c                   movel     arbcbo        d48cbo
262000070608     c                   else
262100070608     c                   movel     blpcbo        d48cbo
262200070608     c                   endif
262300071018     c                   eval      d48tbo = d66tbo
262400070608     c                   movel     v1ccva        d48cvb
262500070608     c                   movel     'S'           d48f12
262600140429     c                   movel     'V'           d48ffr
262700140429      *
262800140429     c                   clear                   dsARBd
262900111025     c
263000070608     c                   movel     fnlr48ds      kpjbu
263100070608     c                   call      'FNLR48R'
263200070608     c                   parm                    KPJBA
263300070608     c                   parm                    DSARBD
263400070608     c                   parm                    DSARBK
263500070608     c                   parm                    DARBT
263600070608     c                   parm                    DSARBG
263700070608     c                   parm                    TRUL90DS
263800070608      *
263900070608     c                   movel     kpjbu         fnlr48ds
264000070608     C* errore
264100070608     C                   if        d48err = '1'
264200070608     c                   eval      *in28 = *on
264300070608     c                   movel     MSG(33)       V1Cmsg
264400070608     C                   eval      *IN47 = *on
264500070608     c                   leave
264600070608     c                   else
264700070608     c                   clear                   v1ccva
264800070608     c                   clear                   v1dcva
264900070608     c                   endif
265000160209      *
265100160209     c* Se no errore e no F12 --> non permetto l'uscita con F3
265200160209     c                   if        d48err = ' ' and d48f12 <> '1'
265300160209     c                   eval      $BloccoF3 = *on
265400160209     c                   eval      *in63     = $BloccoF3
265500160209     c                   endif
265600070608     C*
265700070608    2C                   endif
265800170221      *
265900170221      * F14 Gestione disopsizini di giacenza
266000170221      *
266100170512    2c                   If        *inkn = *on
266200170221     c                   exsr      sr_Ctrgiacenza
266300170221      * controllo se la giacenza è ancora gestibile altrimenti errore
266400170221     c                   If        *in04 = *off
266500170221     c                   eval      *in28 = *on
266600170526      * emetto il messaggio di errore differente se dato f14 con motivo di apertura
266700170526      * diverso da " 12" o se la giacenza non è più nelle condizioni di essere
266800170526      * gestita (esempio stato <> 20)
266900170526     c                   if        v1cmad <> W_gia
267000170526     c                   movel     Msg(64)       V1cmsg
267100170526     C                   eval      *IN36 = *on
267200170526     c                   else
267300170222     c                   if        o071esito <> ' ' and o071msg <> ' '
267400170222     c                   eval      V1cmsg = o071msg
267500170222     c                   else
267600170221     c                   movel     MSG(28)       V1Cmsg
267700170526     c                   endif
267800170526     C                   eval      *IN33 = *on
267900170222     c                   endif
268000170221     c                   leave
268100170221     c                   endif
268200170414      * verifico che non sia stato già chiamato
268300170414     c                   setoff                                       75
268400170414     c     'FNLG10R   '  lookup    £Stack                                 75
268500170512     c                   if        %found
268600170512     c                   movel     MSG(63)       V1Cmsg
268700170512     C                   eval      *IN28 = *on
268800170512     C                   eval      *IN33 = *on
268900170512     c                   leave
269000170512     c                   endif
269100170505
269200170505     c                   clear                   note_gia
269300170505
269400170221      * se non c'è errore richiamo il FNLG10R
269500170509     c***                call(e)   'QCMDEXC'
269600170509     c***                parm      C_StrCmtCtl   Qcmd1
269700170509     c***                parm                    Qlen1
269800170509     c***                if        NOT %error
269900170509     c***                eval      $StrCmtCtl =  *on
270000170509     c****               endif
270100170221      * prima richiamo il trul90 per il recupero delle stampanti
270200170221     C                   clear                   trul90ds
270300170221      * non faccio vedere il video della scelta stampanti
270400170221     C                   movel     'X'           D90rse
270500170221     C                   movel     'X'           D90rsb
270600170314     c                   movel     i071lna       par_key           3
270700170221     C                   call      'TRUL90R'
270800170221     C                   parm                    kpjba
270900170221     C                   parm                    trul90ds
271000170314     C                   parm                    par_key
271100170221      * preparo la ds per il richiamo delle disposizioni di giacenza
271200170413     c                   clear                   fnlg82ds
271300170503     c                   eval      d82ges = UteGia
271400170221     c                   eval      d82gia = 'S'
271500170221     C                   eval      d82MOB  =   d90MdB
271600170221     C                   eval      d82PRB  =   d90PRB
271700170221     C                   eval      d82MOE  =   d90MdE
271800170221     C                   eval      d82PRE  =   d90PRE
271900170221     C                   eval      d82PSS  =   d90PSS
272000170221     C                   eval      d82MDB4 =   d90MDB4
272100170221     C                   eval      d82MDB5 =   d90MDB5
272200170221     C                   eval      d82BSL  =   d90BSL
272300170221     C                   eval      d82PSL  =   d90PSL
272400170221     C                   eval      d82PRB4 =   d90PRB4
272500170221     C                   eval      d82PRB5 =   d90PRB5
272600170221      * giacenza
272700170221     c                   movel     O071fgc       d82fgc
272800170221     c                   movel     O071dap       d82agc
272900170221     c                   movel     O071ngc       d82ngc
273000170221
273100170221     c                   eval      d82comm =   'S'
273200170221     c                   movel     dutpou        d82fgs
273300170221
273400170221     c                   movel(p)  fnlg82ds      kpjbu
273500170509     c******             call      'FNLG10R'
273600170516     c                   call      'FNLG10C'
273700170221     c                   parm                    Kpjba
273800170406     c                   movel(p)  kpjbu         fnlg82ds
273900170406
274000170221
274100170221      * - Chiudo il commit control se è stato avviato da questo pgm.
274200170509     c****               if        $StrCmtCtl =  *on
274300170509     c****               call      'QCMDEXC'
274400170509     c****               parm      C_EndCmtCtl   Qcmd1
274500170509     c****               parm                    Qlen1
274600170509     c****               eval      $StrCmtCtl =  *off
274700170509     c****               endif
274800170222
274900170222      * - se il campo D82F12 è blank vuol dire che ho immesso una disposizione ???
275000170222      *   quindi spengo l'indicatore che condiziona F14 Dispo Giacenza e blocco F3
275100170222     c                   If        D82f12 <>'1'
275200170222     c                   setoff                                       04
275300170222     c                   eval      $BloccoF3 = *on
275400170222     c                   eval      *in63     = $BloccoF3
275500170508     c                   eval      note_gia = C_ImmDisp + ' ' + d82fgc + '/'  +
275600170508     c                                        d82ngc +   ' del ' +
275700170505     c                                         d82agc
275800170222     c                   endif
275900170222      *
276000170414      *
276100170414     c                   Endif
276200160207      *
276300160207      * se arrivo qua vuol dire che non ho avuto errori su
276400160207      * nessun campo, quindi ripristino l'indicatore 48
276500160207      * per posizionare (forse) il cursore sulle note.
276600160207     c                   eval      *in48 = sav_in48
276700160205      *
276800160205      /free
276900160205
277000160205         // -?Immissione R.A. non chiusa: emissione W04?
277100160205         IF  wMod = 'I'  and  V1Ccdt = *blank  and  Not $GesW04;
277200160205
277300160205           exsr  sr_GesW04;
277400160205
277500160205           // -?F12 = Ritorno?
277600160205           if  *inKL;
277700160205             *in90 = *on;
277800160205           endif;
277900160205           if  *in90;
278000160205             leave;
278100160205           endif;
278200160205
278300160205         ENDIF;
278400160205
278500160205      /end-free
278600160205      *
278700070330      * se è andato tutto ok ed è stato dato F6  confermo richiamo
278800070330    2c                   If        *inkf and *in61 and v1cdcr > 0
278900070330     c                   clear                   fnlr48ds
279000070330    3c                   if        arbtc1 = 'S' or arbtc2 = 'S'
279100070330     c                   eval      d48cvb='CS'
279200070330     c                   eval      d48ffr='V'
279300070330     c                   eval      d48f12='S'
279400070330     c                   else
279500070330     c                   eval      d48cvb='CR'
279600070330     c                   eval      d48ffr='E'
279700070330     c                   eval      d48err='Z'
279800070330    3c                   endif
279900070330     c                   z-add     v1anns        D48AAS
280000070330     c                   z-add     v1lnps        D48LNP
280100070330     c                   z-add     v1nrss        D48NRS
280200070330     c                   z-add     v1nsps        D48NSP
280300070330     c                   MOVEL     ARBCBO        D48CBO
280400070330     c                   MOVEL     'A'           D48TBO
280500070330     c                   MOVEL     'S'           D48F12
280600070330     c                   MOVEL     'G'           D48trc
280700070330     c                   MOVEL     fnlr48ds      KPJBU
280800140429     c                   clear                   dsARBd
280900070330     c                   movel     v1ctcr        §BGTCR
281000141029     c                   z-add     v1cocr        §BGHCR
281100070330     c                   z-add     wdcr          §BGDCR
281200070330     c                   movel     arbgc1        §BGGC1
281300070330     c                   movel     arbgc2        §BGGC2
281400070330     c                   movel     arbtc1        §BGTC1
281500070330     c                   movel     arbtc2        §BGTC2
281600070330     c                   call      'FNLR48R'
281700070330     c                   PARM                    KPJBA
281800070330     c                   PARM                    DSARBD
281900070330     c                   PARM                    DSARBK
282000070330     c                   PARM                    DARBT
282100070330     c                   PARM                    DSARBG
282200070330     c                   PARM                    trul90ds
282300070330     c                   MOVEL     KPJBU         fnlr48ds
282400070330      * Ritorno con errore o premuto f12
282500070330    3c                   if        d48err<>*blanks and d48err <> 'Z'
282600160207     c                   eval      *in48 = *off
282700070330     c                   eval      *IN28 = *on
282800070330     c                   eval      *IN62 = *on
282900070330     c                   movel     d48msg        v1cmsg
283000070330     c                   leave
283100070330    3c                   endif
283200070330      * se premuto f12 nulla
283300070330    3c                   if        d48f12='1'
283400070330     c                   eval      *IN90 = *on
283500070330     c                   leave
283600070330    3c                   endif
283700070330
283800070330      * preparo il campo fisso per le modifiche effettuate
283900070330     c                   If        d48err = 'Z' and d48msg <> *blanks
284000070330     c                   eval      not_dcr = 'S'
284100070330     c                   movel     d48msg        note_dcr
284200070330      * preparo un campo note
284300070330     c                   endif
284400160209      * Se no errore e no F12 --> non permetto l'uscita con F3
284500160209     c                   if        D48err = ' ' and D48f12 <> '1'
284600160209     c                   eval      $BloccoF3 = *on
284700160209     c                   eval      *in63     = $BloccoF3
284800160209     c                   endif
284900070330      * valorizzo il flag che mi guida all'inserimento delle note ldv
285000070330     c                   eval      not_ldv = 'S'
285100070330     c                   seton                                        60
285200070330    2c                   endif
285300140429      *
285400140429      /free
285500150421
285600160204         // -?Conferma modifica R.A. non chiusa con altra Filiale Gestione:?
285700160204         //  ?· emissione W04N?
285800160204         //  ?· note obbligatorie SE richiesta Risposta?
285900160204         clear  $GesW04N;
286000160204         clear  $Video;
286100160204
286200160205         IF  *inKF   =  *on    and
286300160205             wMod    <> 'I'    and  V1Ccdt =  *blank  and
286400160205             ( dTGDfar.§TGDatt = *blank   or  dTGDfar.§TGDris = 'S' )
286500160205                               and  (
286600160205             ( w_POt >  *zero  and  V1Cpog <> %editc( w_POt  : 'X' ) )  or
286700160205             ( w_POt <= *zero  and  V1Cpog <> %editc( DUTpou : 'X' ) )  or
286800160205             V1Crpg  <> KNMUS       );
286900160205
287000160205           DoW  Not  $GesW04N;
287100160205
287200160204             exsr  sr_GesW04N;
287300160205
287400160204             // -?F12 = Ritorno?
287500160204             if  *inKL;
287600160204               *in90 = *on;
287700160204             endif;
287800160204             if  *in90;
287900160204               leave;
288000160204             endif;
288100160205
288200160204             *inKF = *on;
288300160205
288400160205           EndDo;
288500160205
288600160205         ENDIF;
288700140429
288800140429         // -?Conferma R.A. con Motivo Apertura " 17"?
288900140429         //  ?(Fermo Deposito autorizza mittente)?
289000140430         //  ?- SE non si è in Sede ma si è in Arrivo -?
289100140506         //  ?- SE non si è in modifica (*in50/70/72/74) -?
289200140430         //  ?=> Variazione Bolle (solo Arrivo)?
289300140506         IF  *inKF  and  $FNARB  and  V1Cmad = c_FDEAM
289400140516                    and  Not *in50
289500140516                    and  Not *in70  and  Not *in72  and  Not *in74;
289600140506
289700140506           $RA_MA_17 = *on;
289800140429
289900140429           clear  dsARBd;
290000140429           clear  dsARBk;
290100140429           clear  dARBt;
290200140429           clear  dsARBg;
290300140429           clear  FNLR48ds;
290400140429
290500140429           D48aas = V1annS;
290600140429           D48lnp = V1lnpS;
290700140429           D48nrs = V1nrsS;
290800140429           D48nsp = V1nspS;
290900140430           D48cbo = ARBcbo;
291000140429           D48cvb = c_CauVarB_I7;
291100140429           D48trc = 'D';
291200140429           D48ffr = 'E';
291300140430           D48tbo = 'A';
291400140429
291500140430           §BDrsd = ARBrsd;
291600140430           §BDind = ARBind;
291700140430           §BDcad = ARBcad;
291800140430           §BDlod = ARBlod;
291900140430           §BDprd = ARBprd;
292000140430           §BDnzd = ARBnzd;
292100140430           §BDfin = ARBfin;
292200140506           §BDffd = 'S';
292300140430           §BDcpi = ARBcpi;
292400140506
292500140506           // -?Reperimento ragione sociale estesa del destinatario?
292600140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'D' )  FIAR4000;
292700140506           if  %found(FIAR401L);
292800140506             §BDrd2 = AR4not;
292900140506           endif;
293000140506
293100140506           // -?Reperimento referente e telefono del destinatario?
293200140506           clear  dAR5gen;
293300140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'GEN' )  FIAR5000;
293400140506           if  %found(FIAR501L)  and  AR5atb = *blank;
293500140506             dAR5gen = AR5uni;
293600140506             §BDref = §AR5ref;
293700140506             §BDtel = §AR5teld;
293800140506           endif;
293900140429
294000140429           kpjbu = FNLR48ds;
294100140429           fnlr48r ( kpjba :
294200140429                     dsARBd :
294300140429                     dsARBk :
294400140429                     dARBt :
294500140429                     dsARBg :
294600140429                     TRUL90ds );
294700140429           FNLR48ds = kpjbu;
294800140429
294900140429           select;
295000140429             //?- se rilevato errore => nulla?
295100140429             when  D48err <> *blank;
295200140429               *in28  = *on;
295300140429               *in62  = *on;
295400140429               V1Cmsg = D48msg;
295500160207               leave;
295600140429             //?- se premuto F12 => nulla?
295700160209             //?  (F12 non abilitato)?
295800160209             //when  D48f12 = *on;
295900160209             //  *in90  = *on;
296000160209             //  leave;
296100160209             //?- se NON premuto F12 => disabilitazione F3?
296200160209             when  D48err = *blank  and  D48f12 <> *on;
296300160209               $BloccoF3  = *on;
296400160209               *in63      = $BloccoF3;
296500140429           endsl;
296600140429
296700140429         ENDIF;
296800141210
296900141210         // -?Conferma R.A. con Motivo Apertura " 60"?
297000141210         //  ?(Verifica telefonica "PINcode")?
297100141210         //  ?- SE non si è in Sede ma si è in Arrivo -?
297200141210         //  ?- SE non si è in modifica (*in50/70/72/74) -?
297300141210         //  ?=> Richiesta BRTcode + PINcode?
297400141210         //  ?=> DOPO: Registrazione Esito nelle Note?
297500141210         IF  *inKF  and  $FNARB  and  V1Cmad = c_VPINC
297600141210                    and  Not *in50
297700141210                    and  Not *in70  and  Not *in72  and  Not *in74;
297800141210
297900141210           // -?Richiesta / Controllo "PINcode"?
298000141210           clear  trulPINds;
298100141210           iPINaas = V1annS;
298200141210           iPINlnp = V1lnpS;
298300141210           iPINnrs = V1nrsS;
298400141210           iPINnsp = V1nspS;
298500141210
298600141210           trulPINr ( trulPINds );
298700141212
298800141216           if  oPINfxx = '03'  and  oPINnre = *zero;
298900141216             *inKF = *off;
299000141216             *in90 = *on;
299100141216             exsr  sr_PulCtl2;
299200141216           else;
299300141216             $RA_MA_60 = *on;
299400141216             // -?N.B. - L'esito verrà registrato DOPO nelle NOTE?
299500141216           endif;
299600141210
299700141210         ENDIF;
299800150114
299900150114
300000150114         //?Posizionamento curosre sulle Note - SE Inserim. & NO errori?
300100150114         if  wMod = 'I'        and
300200150114             Not *in28         and  Not *in90                    and
300300150114             ( ( V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero )    or
300400150324               ( V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero ) );
300500150114           *in48 = *on;
300600150114         endif;
300700140429
300800140429       /end-free
300900040422      *
301000040217     c                   leave
301100040212      *
301200040211    1C                   endDO
301300040211      *
301400040211     c                   ENDSR
301500150112      *
301600150112      *---------------------------------------------------------------*
301700150114      *?Routine di Impostazione automatica di alcuni campi a video   ?*
301800150114      *?  - SE INSERIMENTO -                                         ?*
301900150112      *---------------------------------------------------------------*
302000150114     c     sr_ImpAut_Ins BEGSR
302100150112      *
302200150113sel 1c                   Select
302300150113      *
302400150112      * -?Impostazione campi (se vuoti) per Spedizione?
302500150113w   1c                   When      V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero
302600150112      * - -?Chi Chiama => Destinatario?
302700150112if  4c                   if        V1Ctch = *blank
302800150112     c                   eval      V1Ctch = 'D'
302900150112     c                   exsr      sr_Ctrl_CCR
303000150112e   4c                   endif
303100150202      * - -?Motivo Apertura => Consegna +?
303200150112if  4c                   if        V1Cmad = *blank
303300150112     c                   eval      V1Cmad = '  1'
303400150202     c                   clear                   wSaveMAD
303500150112e   4c                   endif
303600150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
303700150202if  4c                   if        V1Cmad <> wSaveMAD
303800150202     c                   eval      wSaveMAD = V1Cmad
303900150202     c                   exsr      sr_Ctrl_MAR
304000150202     c                   eval      V1Ccdt   = §MARcauChP
304100150202e   4c                   endif
304200150114      * - -?Posizionamento cursore sulle Note?
304300150114     c                   eval      *in48 = *on
304400150112      *
304500150112      * -?Impostazione campi (se vuoti) per O.R.M.?
304600150113w   1c                   When      V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero
304700150112      * - -?Chi Chiama => Mittente?
304800150112if  4c                   if        V1Ctch = *blank
304900150113     c                   eval      V1Ctch = 'M'
305000150112     c                   exsr      sr_Ctrl_CCR
305100150112e   4c                   endif
305200150112      * - -?Motivo Apertura => Ritiro?
305300150112if  4c                   if        V1Cmad = *blank
305400150112     c                   eval      V1Cmad = '  3'
305500150202     c                   clear                   wSaveMAD
305600150112e   4c                   endif
305700150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
305800150202if  4c                   if        V1Cmad <> wSaveMAD
305900150202     c                   eval      wSaveMAD = V1Cmad
306000150202     c                   exsr      sr_Ctrl_MAR
306100150202     c                   eval      V1Ccdt   = §MARcauChP
306200150202e   4c                   endif
306300150114      * - -?Posizionamento cursore sulle Note?
306400150114     c                   eval      *in48 = *on
306500150407      *
306600150407      * -?Impostazione campi (se vuoti) per Generico?
306700150407w   1c                   When      V1Ctor = 'G'
306800150407      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
306900150407if  4c                   if        V1Cmad <> wSaveMAD
307000150407     c                   eval      wSaveMAD = V1Cmad
307100150407     c                   exsr      sr_Ctrl_MAR
307200150407     c                   eval      V1Ccdt   = §MARcauChP
307300150407e   4c                   endif
307400150112      *
307500150113e   1c                   EndSl
307600150313
307700150625      * - -?STATO (ex Priorità):?
307800150625      *    ?Se viene impostata la causale chiusura metto priorità "9"?
307900150625      *    ?(in inserimento) o "8" (in modifica - SE modificata la?
308000150625      *    ?Causale Chiusura)?
308100150313     c                   IF        V1Ccdt <> *blanks
308200150402     c                   IF        wmod = 'I'
308300150625      *   -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
308400150625      *    ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
308500150625      *    ?(già reperita nella subr. SR_CONTR01)?
308600150625     c                   if        §CORstato <> *blanks
308700150625     c                   eval      V1Cprt = §CORstato
308800150625     c                   else
308900150313     c                   eval      V1Cprt = '9'
309000150625     c                   endif
309100150324     c                   ELSE
309200150324     c                   eval      V1Cprt = '8'
309300150324     c                   ENDIF
309400150313     c                   ENDIF
309500150320      *
309600150320      * Decodifico (potrebbe essere cambiato)
309700150320     c                   clear                   tibs02ds
309800150320     C                   MOVEL     'C'           t02mod
309900150320     C                   MOVEL     knsif         t02sif
310000150320     C                   MOVEL     'PRR'         t02cod
310100150320     C                   MOVEL     v1cprt        t02ke1
310200150320      *
310300150320     C                   CALL      'TIBS02R'
310400150320     C                   PARM                    KPJBA
310500150320     C                   PARM                    TIBS02DS
310600150320      *
310700150320    4C                   IF        t02err = *BLANKS
310800150320     C                   movel     T02uni        DPRR
310900150320     C                   movel     §PRRdesc      v1dprt
311000150320     c                   endif
311100150112      *
311200150112     c                   ENDSR
311300150112      *
311400150112      *---------------------------------------------------------------*
311500150112      * Routine controllo tabella "MAR" = Motivo Apertura             *
311600150112      *---------------------------------------------------------------*
311700150112     c     sr_Ctrl_MAR   BEGSR
311800150323
311900150323      * Ricerca
312000150323    3c                   if        %scan('?':V1Cmad) > 0
312100150422     c                             and       $Video <> 'W4'
312200150323     c                   clear                   tntbMARds1
312300150323      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
312400150323      *      dall'utente in APERTURA;
312500150323      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
312600150323      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
312700150323      *      in GESTIONE.
312800150323     c                   if        wMod <> 'I'
312900150323     c                   eval      iMARutUt = 'I'
313000150323     c                   endif
313100150323     c                   eval      iMARutAp = 'I'
313200150323     c                   movel(p)  tntbMARds1    KPJBU
313300150323      *
313400150323     c                   call      'TNTBMARR1'
313500150323     c                   parm                    KPJBA
313600150323     c                   movel     KPJBU         tntbMARds1
313700150323    4c                   if        oMARfxx = *blank  and  oMARerr = *off
313800150323     c                   eval      V1Cmad  = oMARcod
313900150323     C                   eval      V1Dmad  = oMARdes
314000150323      *  errore (non trovato nulla pulisco il '?' )
314100150323     C                   ELSE
314200150323     c                   clear                   V1Cmad
314300150323     c                   clear                   V1Dmad
314400150323    4C                   ENDIF
314500150422    3C                   ENDIF
314600150112      *
314700150112     c                   clear                   tibs02ds
314800150112     C                   MOVEL     'C'           t02mod
314900150112     C                   MOVEL     knsif         t02sif
315000150112     C                   MOVEL     'MAR'         t02cod
315100150422     c                   if        $Video = 'W4'
315200150422     c                   eval      T02ke1 = wwcMAD
315300150422     c                   else
315400150112     C                   MOVEL     v1cmad        t02ke1
315500150422     c                   endif
315600150112      *
315700150112     C                   CALL      'TIBS02R'
315800150112     C                   PARM                    KPJBA
315900150112     C                   PARM                    TIBS02DS
316000150112      *
316100150112    4C                   IF        t02err = *BLANKS
316200150112     C                   movel     T02uni        DMAR
316300150422     c                   if        $Video = 'W4'
316400150422     c                   eval      wwdMAD = §MARdesc
316500150422     c                   else
316600150112     C                   movel     §MARdesc      v1dmad
316700150423     c                   eval      wwdMAD = §MARdesc
316800150422     c                   endif
316900150112     c                   if        (*in74 = *off  and  §MARutAp = 'N')  or
317000150112     c                             (wMod <> 'I'   and  §MARutUt = 'N')
317100150112     c                   eval      *in28 = *on
317200150112     c                   eval      *in36 = *on
317300150112     c                   movel     msg(18)       V1Cmsg
317400150112     c                   leavesr
317500150112     c                   endif
317600150112      *
317700150112     C                   ELSE
317800150112     c                   eval      *in28 = *on
317900150112     c                   movel     msg(18)       V1Cmsg
318000150112     C                   eval      *IN36 = *on
318100150112     c                   leavesr
318200150112    4C                   ENDIF
318300150112      *
318400150112     c                   ENDSR
318500150112      *
318600150112      *---------------------------------------------------------------*
318700150112      * Routine controllo tabella "CCR" = Chi Chiama                  *
318800150112      *---------------------------------------------------------------*
318900150112     c     sr_Ctrl_CCR   BEGSR
319000150112      *
319100150112     c                   clear                   tibs02ds
319200150112     C                   MOVEL     'C'           t02mod
319300150112     C                   MOVEL     knsif         t02sif
319400150112     C                   MOVEL     'CCR'         t02cod
319500150112     C                   MOVEL     v1ctch        t02ke1
319600150112      *
319700150112     C                   CALL      'TIBS02R'
319800150112     C                   PARM                    KPJBA
319900150112     C                   PARM                    TIBS02DS
320000150112      *
320100150112    4C                   IF        t02err = *BLANKS
320200150112     C                   movel     T02uni        DCCR
320300150112     C                   movel     T02ke1        v1ctch
320400150112     c                   movel     §ccrdesc      v1dtch
320500150112      *
320600150112     C                   ELSE
320700150112     C                   eval      *IN28 = *on
320800150112     c                   movel     msg(07)       V1Cmsg
320900150112     C                   eval      *IN35 = *on
321000150112     c                   leavesr
321100150112    4C                   ENDIF
321200150112      *
321300150112     c                   ENDSR
321400170220      *---------------------------------------------------------------*
321500170220      *?Routine di Controllo spedizione legata a Giacenza            ?*
321600170220      *?  - SE INSERIMENTO -                                         ?*
321700170220      *---------------------------------------------------------------*
321800170220     c     sr_CtrGiacenzaBEGSR
321900170524      *
322000170524      * Verifico se motivo di apertura uguale a giacenza altrimenti spengo lo 04
322100170524      *
322200170524     c                   if        v1cmad <> W_gia
322300170524     c                   eval      *in04 = *off
322400170524     c                   else
322500170220      *
322600170515      * Verifico se utente autorizzato alla giacenza o come linea di partenza o come
322700170503      * linea di arrivo.
322800170515      * Se autorizzato verifico lo stato della giacenza
322900170503      * Memorizzo anche se posso operare sulla giacenza come Partenza (priorità) o Arrivo
323000170518
323100170519      * la bolla non legata  controllo abilitazione alla spedizione in base
323200170519      * alla sua lnp e lna
323300170519     c                   eval      lnp_abil = arblnp
323400170519     c                   eval      lna_abil = arblna
323500170519      *
323600170518      * aggancio il file legami bolla per determinare se ci sono dei legami e
323700170519      * in tal caso impostare la lnp (mamma) e lna (ultima figlia)
323800170518     C                   movel     *blanks       wEsito1           1
323900170518     C                   movel     *blanks       wEsito2           1
324000170519     C*
324100170519     C                   eval      wAAS = V1annS
324200170519     C                   eval      wLNP = V1lnps
324300170519     C                   eval      wNRS = V1nrss
324400170519     C                   eval      wNSP = V1nsps
324500170519     C*
324600170518     C* Chiamata metodo GetLblTyp
324700170518     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
324800170519     C                                                wAAS
324900170519     C                                               :wLNP
325000170519     C                                               :wNRS
325100170519     C                                               :wNSP
325200170518     C                                               :pOutLblTyp
325300170518     C                                               :pOutAnnoBO
325400170518     C                                               :pOutLineaParBO
325500170518     C                                               :pOutSerieBO
325600170518     C                                               :pOutNumSpedBO
325700170518     C                                               :pOutDcmBO
325800170518     C                                               :pOutCcaBO
325900170518     C                                               :pOutRblBO))
326000170519      * se bolla legata verifico se sono la bolla originale
326100170519     c                   If        wesito1 = '0'
326200170519
326300170518     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
326400170518     C                   if        pOutLblTyp <> 'O'
326500170518     C                   eval      wAAS = pOutAnnoBO
326600170518     C                   eval      wLNP = pOutLineaParBO
326700170518     C                   eval      wNRS = pOutSerieBO
326800170518     C                   eval      wNSP = pOutNumSpedBO
326900170518     C                   endif
327000170518     C*
327100170518     C* Chiamata metodo GetLastChild
327200170518     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
327300170518     C                                                wAAS
327400170518     C                                               :wLNP
327500170518     C                                               :wNRS
327600170518     C                                               :wNSP
327700170518     C                                               :pOutAnnoFI
327800170518     C                                               :pOutLineaParFI
327900170518     C                                               :pOutSerieFI
328000170518     C                                               :pOutNumSpedFI
328100170518     C                                               :pOutDcmFI
328200170518     C                                               :pOutCcaFI))
328300170519      * se bolla figlia trovata recupero le linee di arrivo dell'ultima figlia e partenza dalla
328400170519      * spedizione originale
328500170519      /free
328600170519         If  Wesito2 = '0' ;
328700170519             chain (poutannofi:poutlineaparfi:poutseriefi:poutNumspedfi) FNLBL1;
328800170519             if  %found( FNLBL01L );
328900170519                 lnp_abil = lbllpo ;
329000170519                 lna_abil = lbllan ;
329100170519             Endif ;
329200170519         Endif ;
329300170519      /end-free
329400170519
329500170519     c                   Endif
329600170519
329700170503     c                   clear                   utegia
329800170515     c                   eval      *in04 = *off
329900170515
330000170503      *
330100170519     c                   move      lnp_abil      w003a
330200170505     c     w003a         lookup    wpog                                   27
330300170503     c                   if        %found
330400170503     c                   eval      Utegia= 'P'
330500170503     c                   else
330600170519     c                   move      lna_abil      w003a
330700170505     c     w003a         lookup    wpog                                   27
330800170503     c                   if        %found
330900170503     c                   eval      Utegia= 'A'
331000170503     c                   endif
331100170503     c                   endif
331200170503      * se non è possibile gestire la spedizione non ricerco la giacenza
331300170515    2c                   If        Utegia <> ' '
331400170503
331500170220     c                   clear                   FNLG071DS
331600170220     c                   eval      I071CHIA = 'C'
331700170220     c                   eval      I071AAS = v1anns
331800170220     c                   eval      I071LNP = v1lnps
331900170220     c                   eval      I071NRS = v1nrss
332000170220     c                   eval      I071NSP = v1nsps
332100170606     c                   eval      I071LNA = lna_abil
332200170503     c                   eval      I071CHI = UteGia
332300170220     c                   call      'FNLG07R1'
332400170220     c                   parm                    kpjba
332500170220     c                   parm                    FNLG071DS
332600170220      * verifico l'Esito alla chiamata del FNLG071
332700170220     c                   if        O071esito <> ' '
332800170220     c                             or O071ngc = 0
332900170220     c                   setoff                                       04
333000170220     c                   endif
333100170220     c                   if        O071esito = ' '
333200170220     c                             and O071ngc <> 0
333300170220     c                   seton                                        04
333400170503     c                   endif
333500170503
333600170515    2c                   endif
333700170524
333800170524    2c                   endif
333900170524
334000170220     c                   ENDSR
334100070323      *
334200140429      /free
334300150421
334400150421       //--------------------------------------------------------------
334500150421       //?Gestione window W4 per immissione R.A. non chiusa            ?
334600150421       //--------------------------------------------------------------
334700150421       BEGSR  sr_GesW04;
334800150421
334900150423         $Video   = 'W4';
335000150421
335100150421         //clear  FIA1W04;  // -?NO: serve sapere se sono già stati?
335200150421                            //  ?    inseriti dati?
335300160120
335400160204         // -?Impostazione flag "Attesa Risposta"?
335500160120         $AttesaRisposta = *off;
335600150616
335700150915         // -?Impostazione gestione Reparto?
335800150915         $Reparto = ( V1Cpog <> c_Sede  or  V1Cmad = c_ReclSede );
335900150421
336000150421         W4nTel = V1nTel;
336100150421         W4mail = V1mail;
336200150421         W4cMad = V1cMad;
336300150915         clear  W4cRpr;
336400150915         clear  W4cPog;
336500151211         clear  W4cCap;
336600160607         // -?Reparto/Utente gestione?
336700150720         select;
336800150915           // -?Inserimento Reclamo da Sede (in Sede, motivo " 70"):?
336900150915           //  ?proposto reparto?
337000150720           when  *in05  and  W4cMad = c_ReclSede;
337100150915             W4cRpr = 'RAC';
337200150720           // -?Proposto Utente, non Reparto (in Sede)?
337300150720           when  Not $Reparto;
337400150720             W4cRpr = KNMUS;
337500150720           // -?Proposto Reparto (in Filiale)?
337600150720           other;
337700150720             W4cRpr = 'RAC';
337800150720         endsl;
337900160607         // -?Filiale gestione?
338000150720         if  Not *in05  or  W4cMad <> c_ReclSede;
338100160607           select;
338200160607             // -?LNA della Spedizione?
338300160607             when  V1Ctor = 'S';
338400160607               if  $FNARB;
338500160607                 W4cPog = %editc( ARBlna : 'X' );
338600160607               else;
338700160607                 W4cPog = %editc( BLPlna : 'X' );
338800160607               endif;
338900160607             // -?Filiale Ritiro dell'O.R.M.?
339000160607             when  V1Ctor = 'O';
339100160607               W4cPog = %editc( ORMpor : 'X' );
339200160607             // -?Filiale Utente altrimenti?
339300160607             other;
339400160607               W4cPog = V1Cpog;
339500160607           endsl;
339600150720         endif;
339700160128         // -?Attesa Risposta?
339800151223         Select;
339900151223           When  *in05;
340000151223             W4tgdATT = 'N';
340100151223           When  V1CfilR > *zero  and  V1CrepR <> *blank;
340200151223             W4tgdATT = 'S';
340300151223             W4repR = V1CrepR;
340400151223             W4filR = V1CfilR;
340500151223           Other;
340600151223             W4tgdATT = 'N';
340700151223             W4repR = 'RAC';
340800160129             if  w_POt = *zero;
340900160129               W4filR = %editc( DUTpou : 'X' );
341000160129             else;
341100160129               W4filR = %editc( w_POt : 'X' );
341200160129             endif;
341300151223         EndSl;
341400150428
341500150428         // -?Decodifiche dati immessi o proposti?
341600150428         wwcMad = W4cMad;
341700150428         exsr  sr_CtrlMAD;
341800150428         W4cMad = wwcMad;
341900150428         W4dMad = wwdMad;
342000150428         wwcRPR = W4cRpr;
342100150616         if  Not $Reparto;
342200150616           wwcPog = W4cPog;
342300150616           exsr  sr_CtrlRPG;
342400150616         else;
342500150616           exsr  sr_CtrlRPR;
342600150616         endif;
342700150428         W4cRpr = wwcRPR;
342800150428         W4dRpr = wwdRPR;
342900150717         //wwcMad = W4cMad;   ?(già impostata prima)?
343000150428         wwcPog = W4cPog;
343100150428         exsr  sr_CtrlPOG;
343200150428         W4cPog = wwcPog;
343300150428         W4dPog = wwdPog;
343400151211         wwcRPR  = W4repR;
343500151211         exsr  sr_CtrlRPR;
343600151211         W4repR  = wwcRPR;
343700151211         W4repRd = wwdRPR;
343800151211         wwcPog  = W4filR;
343900151211         exsr  sr_CtrlPOG;
344000151211         W4filR  = wwcPog;
344100151211         W4filRd = wwdPog;
344200150428
344300150428         *in28 = *off;
344400150428         *in90 = *off;
344500150428         *in36 = *on;     // -?Posiz. cursore su Motivo?
344600150428         *in38 = *off;    // -?Posiz. cursore su Mail?
344700150428         *in45 = *off;    // -?Posiz. cursore su Reparto Gestione?
344800150428         *in46 = *off;    // -?Posiz. cursore su Filiale Gestione?
344900150428         *in55 = *off;    // -?Posiz. cursore su Telefono?
345000150911         *in56 = *off;    // -?Posiz. cursore su C.A.P. cliente?
345100151211         *in57 = *off;    // -?Posiz. cursore su Reparto Risposta?
345200151211         *in58 = *off;    // -?Posiz. cursore su Filiale Risposta?
345300150422
345400150423         DoU  *in28 = *off  and  *in90 = *off  and  *inKF;
345500150423
345600150423           exfmt  FIA1W04;
345700150423
345800150914           clear  V1Cmsg;
345900150423           *in28 = *off;
346000150423           *in90 = *off;
346100150423           *in36 = *off;       // -?Motivo errato?
346200150423           *in38 = *off;       // -?Mail   errata?
346300150423           *in45 = *off;       // -?Reparto Gestione errato?
346400150423           *in46 = *off;       // -?Filiale Gestione errata?
346500150423           *in55 = *off;       // -?Telefono obbligatorio?
346600150911           *in56 = *off;       // -?C.A.P. cliente   errato?
346700151211           *in57 = *off;       // -?Reparto Risposta errato?
346800151211           *in58 = *off;       // -?Filiale Risposta errata?
346900150423
347000150423           // -?F12 = Ritorno al fmt D01?
347100150423           if  *inKL;
347200150423             clear  $Video;
347300150423             leavesr;
347400150423           endif;
347500150423
347600150423           // -?Num. Telefono o Mail obbligatori?
347700150423           if  W4nTel = *blank  and  W4mail = *blank;
347800150723             *in28  = *on;
347900150423             *in55  = *on;
348000150423             V1Cmsg = Msg(49);
348100150423             iter;
348200150423           endif;
348300150423
348400150423           // -?Mail errata?
348500150423           if  W4mail <> *blank;
348600150423             wwMail = W4mail;
348700150423             exsr  sr_CtrlMail;
348800150423             W4mail = wwMail;
348900150723             if  *in28  or  *in90;
349000150423               iter;
349100150423             endif;
349200150423           endif;
349300150423
349400150423           // -?Controllo Motivo Apertura Reclamo?
349500150423           wwcMad = W4cMad;
349600150423           exsr  sr_CtrlMAD;
349700150423           W4cMad = wwcMad;
349800150423           W4dMad = wwdMad;
349900150723           if  *in28  or  *in90;
350000150423             iter;
350100150423           endif;
350200171122           // - ?NON è permesso l'utilizzo del motivo " 17" (FDep) in?
350300171122           //   ?questa window (dal FRONT): la R.A. rimarrebbe aperta?
350400171122           //   ?e NON sarebbe più più gestibile?
350500171122           if  W4cMad = c_FDEAM;
350600171122             *in28 = *on;
350700171122             *in36 = *on;
350800171122             V1Cmsg = Msg(65);
350900171122             iter;
351000171122           endif;
351100150723
351200150723           // -?SE Motivo = " 70" (quindi devo essere in Sede):?
351300150723           //  ?Controllo la Filiale Gestione PRIMA dell'Utente Gestione?
351400150723           If  W4cMad = c_ReclSede;
351500150911             // ·?La Filiale Gestione potrebbe voler essere ricavata?
351600150911             //  ?dal C.A.P. del cliente?
351700150911             if  W4cCap <> *blank;
351800150911               exsr  sr_CtrlCAP;
351900150914               if  *in28;
352000150914                 iter;
352100150914               endif;
352200150911             endif;
352300150723             wwcPog = W4cPog;
352400150723             //wwcMad = W4cMad; ?(già impostata prima)?
352500150723             exsr  sr_CtrlPOG;
352600150723             W4cPog = wwcPog;
352700150723             W4dPog = wwdPog;
352800150915             if  *in28;
352900150723               iter;
353000150723             endif;
353100150723             if  W4cPog = c_Sede;
353200150723               *in28 = *on;
353300150723               *in46 = *on;
353400150723               if  Not *in05;
353500150723                 V1Cmsg = Msg(48);
353600150723               else;
353700150723                 V1Cmsg = Msg(53);
353800150723               endif;
353900150723               iter;
354000150723             endif;
354100150723           EndIf;
354200151202
354300151202           // -?SE modificato Motivo in " 18" (Reso):?
354400151202           //  ?ritorno alla videata pecedente (la R.A. va chiusa)?
354500151202           If  W4cMad = c_MotAp_Reso;
354600151202             *in90 = *on;
354700151202             leave;
354800151202           EndIf;
354900150915
355000150915           // -?Re-impostazione gestione Reparto?
355100150915           $Reparto = ( W4cPog <> c_Sede  or  W4cMad = c_ReclSede );
355200150423
355300150617           // -?Controllo Reparto Gestione (in filiale)?
355400150617           //  ?Controllo Utente  Gestione (in sede)?
355500150429           wwcRpr = W4cRpr;
355600150616           if  Not $Reparto;
355700150616             wwcPog = W4cPog;
355800150616             exsr  sr_CtrlRPG;
355900150616           else;
356000150616             exsr  sr_CtrlRPR;
356100150616           endif;
356200150429           W4cRpr = wwcRpr;
356300150429           W4dRpr = wwdRpr;
356400150915           if  *in28;
356500150429             iter;
356600150429           endif;
356700150423
356800150723           // -?SE Motivo <> " 70":?
356900150723           //  ?Controllo Filiale Gestione   DOPO?
357000150723           //  ?il Reparto Gestione (in filiale)?
357100150723           //  ?o l'Utente  Gestione (in sede)?
357200150723           If  W4cMad <> c_ReclSede;
357300150723             wwcPog = W4cPog;
357400160120             //wwcMad = W4cMad;    ?(già impostata prima)?
357500150723             exsr  sr_CtrlPOG;
357600150723             W4cPog = wwcPog;
357700150723             W4dPog = wwdPog;
357800150915             if  *in28;
357900150723               iter;
358000150723             endif;
358100150723             if  Not *in05  and  W4cPog =  c_Sede;
358200150723               *in28 = *on;
358300150723               *in46 = *on;
358400150723               V1Cmsg = Msg(48);
358500150723               iter;
358600150723             endif;
358700150723           EndIf;
358800151211
358900160120           // -?Controllo Reparto che Attende Risposta?
359000151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
359100160121           //  ?- in Sede NON viene richiesto Reparto/Filiale... -?
359200160121           If  Not *in05  and  W4tgdATT = 'S';
359300151214             if  W4repR = *blank;
359400151214               W4repR   = 'RAC';
359500151214             endif;
359600151223             wwcRpr  = W4repR;
359700151214             exsr  sr_CtrlRPR;
359800151214             W4repR  = wwcRPR;
359900151214             W4repRd = wwdRPR;
360000151223             if  *in28  or  *in90;
360100151223               *in45 = *off;
360200151223               *in57 = *on;
360300151223               if  *in28;
360400151223                 V1Cmsg = Msg(57);
360500151223               endif;
360600151214               iter;
360700151214             endif;
360800151223           Else;
360900160121             W4tgdATT = 'N';
361000151223             clear  W4repR;
361100151223             clear  W4repRd;
361200151214           EndIf;
361300151211
361400160120           // -?Controllo Filiale che Attende Risposta?
361500151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
361600160121           //  ?- in Sede NON viene richiesto Reparto/Filiale... -?
361700151223           If  Not *in05  and  W4tgdATT = 'S';
361800151214             if  W4filR = *blank;
361900160129               if  w_POt = *zero;
362000160129                 W4filR  = %editc( DUTpou : 'X' );
362100160129               else;
362200160129                 W4filR  = %editc( w_POt : 'X' );
362300160129               endif;
362400151214             endif;
362500160121             $AttesaRisposta = *on;
362600160120             //wwcRpr  = W4repR;   ?(già impostata prima)?
362700151214             wwcPog  = W4filR;
362800151214             exsr  sr_CtrlPOG;
362900151214             W4filR  = wwcPog;
363000151214             W4filRd = wwdPog;
363100160121             $AttesaRisposta = *off;
363200151222             if  *in28  or  *in90;
363300151222               *in46 = *off;
363400151222               *in58 = *on;
363500151223               if  *in28;
363600151223                 V1Cmsg = Msg(58);
363700151223               endif;
363800151214               iter;
363900151214             endif;
364000160121             if  W4filR = c_Sede;
364100160121               *in28 = *on;
364200160121               *in58 = *on;
364300160121               V1Cmsg = Msg(48);
364400160121               iter;
364500160121             endif;
364600151223           Else;
364700160121             W4tgdATT = 'N';
364800151223             clear  W4filR;
364900151223             clear  W4filRd;
365000151214           EndIf;
365100151222
365200160128           // -?Confronto tra Reparto/Filiale in Attesa Risposta?
365300151222           //            ?con Reparto/Filiale di Gestione?
365400151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
365500151223           If  Not *in05          and
365600151223               W4tgdATT = 'S'     and
365700151223               W4repR   = W4Crpr  and
365800151223               W4filR   = W4Cpog;
365900151222             *in28 = *on;
366000151222             *in57 = *on;
366100151222             V1Cmsg = Msg(59);
366200151222             iter;
366300151222           endif;
366400150423
366500150423           *in36 = *on;     // -?Posiz. cursore su Motivo?
366600150422
366700150423         EndDo;
366800150421
366900150421         V1nTel = W4nTel;
367000150421         V1mail = W4mail;
367100150421         V1Cmad = W4cMad;
367200151202
367300151202         If  W4cMad <> c_MotAp_Reso;
367400151202
367500151202           V1Crpg = W4cRpr;
367600151202           V1Cpog = W4cPog;
367700151202
367800151202           $GesW04 = *on;
367900151202
368000151202         EndIf;
368100151223
368200151223         V1CfilR = W4filR;
368300151223         V1CrepR = W4repR;
368400151202
368500151202         clear  $Video;
368600150421
368700150421       ENDSR;
368800160128
368900160128       //--------------------------------------------------------------
369000160128       //?Gestione window W4M per nuova Attesa Risposta                ?
369100160128       //--------------------------------------------------------------
369200160128       BEGSR  sr_GesW04N;
369300160128
369400160204         IF  $Video <> 'M4';
369500160204
369600160204           $Video = 'M4';
369700160204           clear  FIA1W04N;
369800160204
369900160204           // -?Attesa Risposta di default?
370000160204           If    dTGDfar.§TGDatt = *blank  or
370100160204               ( dTGDfar.§TGDatt = 'S'    and
370200160204                 dTGDfar.§TGDris = 'S' );
370300160204             W4tgdATT = 'N';
370400160204             W4repR   = 'RAC';
370500160204             if  w_POt = *zero;
370600160204               W4filR   = %editc( DUTpou : 'X' );
370700160204             else;
370800160204               W4filR   = %editc( w_POt : 'X' );
370900160204             endif;
371000160204           Else;
371100160204             W4tgdATT = dTGDfar.§TGDatt;
371200160204             W4repR   = dTGR01A.§TGRrepR;
371300160204             W4filR   = %editc( dTGD01.§TGDfilR : 'X' );
371400160204           EndIf;
371500160204
371600160204           // -?Decodifiche dati immessi o proposti?
371700160204           wwcRPR  = W4repR;
371800160204           exsr  sr_CtrlRPR;
371900160204           W4repR  = wwcRPR;
372000160204           W4repRd = wwdRPR;
372100160204           $AttesaRisposta = *on;
372200160204           wwcPog  = W4filR;
372300160204           exsr  sr_CtrlPOG;
372400160204           W4filR  = wwcPog;
372500160204           W4filRd = wwdPog;
372600160204           $AttesaRisposta = *off;
372700160204
372800160204         ENDIF;
372900160128
373000160128         *in28 = *off;
373100160128         *in90 = *off;
373200160128         *in57 = *off;    // -?Posiz. cursore su Reparto Risposta?
373300160128         *in58 = *off;    // -?Posiz. cursore su Filiale Risposta?
373400160128
373500160128         DoU  *in28 = *off  and  *in90 = *off;
373600160128
373700160128           exfmt  FIA1W04N;
373800160128
373900160128           clear  V1Cmsg;
374000160128           *in28 = *off;
374100160128           *in90 = *off;
374200160128           *in57 = *off;       // -?Reparto Risposta errato?
374300160128           *in58 = *off;       // -?Filiale Risposta errata?
374400160128
374500160128           // -?F12 = Ritorno al fmt D01?
374600160128           if  *inKL;
374700160128             clear  $Video;
374800160207             W4tgdATT = 'N';
374900160128             leavesr;
375000160128           endif;
375100160128
375200160128           // -?NON Richiesta Risposta: uscita?
375300160128           //  ?(con aperta ed in gestione ad altro Rep./Fil.)?
375400160129           If  W4tgdATT = 'N';
375500160204             $GesW04N = *on;
375600160129             clear  W4repRd;
375700160128             clear  W4repR;
375800160129             clear  V1CrepR;
375900160129             clear  W4filRd;
376000160129             W4filR  = *zero;
376100160129             V1CfilR = *zero;
376200160128             clear  $Video;
376300160128             leavesr;
376400160128           EndIf;
376500160128
376600160128           // -?Controllo Reparto che Attende Risposta?
376700160128           if  W4repR = *blank;
376800160128             W4repR   = 'RAC';
376900160128           endif;
377000160128           wwcRpr  = W4repR;
377100160128           exsr  sr_CtrlRPR;
377200160128           W4repR  = wwcRPR;
377300160128           W4repRd = wwdRPR;
377400160128           if  *in28  or  *in90;
377500160128             *in45 = *off;
377600160128             *in57 = *on;
377700160128             if  *in28;
377800160128               V1Cmsg = Msg(57);
377900160128             endif;
378000160128             iter;
378100160128           endif;
378200160128
378300160128           // -?Controllo Filiale che Attende Risposta?
378400160129           if  W4filR  = *blank;
378500160129             if  w_POt = *zero;
378600160129               W4filR  = %editc( DUTpou : 'X' );
378700160129             else;
378800160129               W4filR  = %editc( w_POt : 'X' );
378900160129             endif;
379000160128           endif;
379100160128           $AttesaRisposta = *on;
379200160128           //wwcRpr  = W4repR;   ?(già impostata prima)?
379300160128           wwcPog  = W4filR;
379400160128           exsr  sr_CtrlPOG;
379500160128           W4filR  = wwcPog;
379600160128           W4filRd = wwdPog;
379700160128           $AttesaRisposta = *off;
379800160128           if  *in28  or  *in90;
379900160128             *in46 = *off;
380000160128             *in58 = *on;
380100160128             if  *in28;
380200160128               V1Cmsg = Msg(58);
380300160128             endif;
380400160128             iter;
380500160128           endif;
380600160128           if  W4filR = c_Sede;
380700160128             *in28 = *on;
380800160128             *in58 = *on;
380900160128             V1Cmsg = Msg(48);
381000160128             iter;
381100160128           endif;
381200160128
381300160128           // -?Confronto tra Reparto/Filiale in Attesa Risposta?
381400160128           //            ?con Reparto/Filiale di Gestione?
381500160128           If  W4repR   = V1Crpg  and
381600160128               W4filR   = V1Cpog;
381700160128             *in28 = *on;
381800160128             *in57 = *on;
381900160128             V1Cmsg = Msg(59);
382000160128             iter;
382100160128           EndIf;
382200160204
382300160204           // -?Reperimento ULTIMO Progressivo Note inserito?
382400160204           setGT   ( V1Cant : V1Cnut : V1Cpdt : KEYtin )  FITGN000;
382500160204           readPE  ( V1Cant : V1Cnut : V1Cpdt : KEYtin )  FITGN000;
382600160204           if  %eof(FITGN01L);
382700160204             clear  TGNpgn;
382800160204           endif;
382900160204           // -?Note obbligatorie?
383000160204           If  TGNpgn <= wSavePGN;
383100160204             exsr  sr_F18;
383200160204             *in90 = ( oB0fxx <> *blank );
383300160204           EndIf;
383400160128
383500160128         EndDo;
383600160129
383700160204         V1CfilR  = W4filR;
383800160204         V1CrepR  = W4repR;
383900160204         $GesW04N = *on;
384000160128
384100160128       ENDSR;
384200140429
384300140429       //--------------------------------------------------------------
384400140429       //?Controlli specifici per Motivo Apertura R.A. " 17"=F.Deposito?
384500140429       //--------------------------------------------------------------
384600140429       BEGSR  sr_CtrMotAp_17;
384700140430
384800140430         Select;
384900140430
385000140430           // -?Errore SE si è in Sede?
385100140430           When  *in05;
385200140430             *in28  = *on;
385300140430             *in36  = *on;
385400140430             V1Cmsg = Msg(42);
385500140430             leavesr;
385600140430
385700140430           // -?Errore SE la spedizione NON è in Arrivo?
385800140430           When  Not $FNARB;
385900140430             *in28  = *on;
386000140430             *in36  = *on;
386100140430             V1Cmsg = Msg(42);
386200140430             leavesr;
386300140430
386400140430           // -?Errore SE la linea di partenza NON è tra le filiali in?
386500140430           //  ?gestione all'utente?
386600140430           When  %lookup( %editc( ARBlnp : 'X' ) : wPOG ) = *zero;
386700140430             *in28 = *on;
386800140430             *in36 = *on;
386900140430             V1Cmsg = Msg(41);
387000140430             leavesr;
387100140430
387200140430         EndSl;
387300140430
387400140430
387500140430         // -?Verifica SE la causale variazione bolle "I7" può essere?
387600140430         //  ?utilizzata con la spedizione in oggetto?
387700140429         clear  FNLR66ds;
387800140429
387900140507         D66ges = 'K';
388000140429         D66cvb = c_CauVarB_I7;
388100140429         D66aas = V1annS;
388200140429         D66lnp = V1lnpS;
388300140429         D66nrs = V1nrsS;
388400140429         D66nsp = V1nspS;
388500140430         D66tbo = 'A';
388600140429
388700140505         kpjbu = FNLR66ds;
388800140505         fnlr66r ( kpjba );
388900140505         FNLR66ds = kpjbu;
389000140505
389100140505         if  D66msg <> *blank;
389200140505           *in28  = *on;
389300140505           *in36  = *on;
389400140505           V1Cmsg = Msg(26);
389500140505           leavesr;
389600140505         endif;
389700140429
389800140429
389900140429         // -?Verifica SE ci sono errori nella manutenzione della?
390000140429         //  ?spedizione in oggetto?
390100140429         clear  dsARBd;
390200140429         clear  dsARBk;
390300140429         clear  dARBt;
390400140429         clear  dsARBg;
390500140429         clear  FNLR48ds;
390600140429
390700140429         D48aas = V1annS;
390800140429         D48lnp = V1lnpS;
390900140429         D48nrs = V1nrsS;
391000140429         D48nsp = V1nspS;
391100140430         D48cbo = ARBcbo;
391200140429         D48cvb = c_CauVarB_I7;
391300140429         D48trc = 'D';
391400140429         D48ffr = 'F';
391500140429         D48tbo = D66tbo;
391600140429
391700140430         §BDrsd = ARBrsd;
391800140430         §BDind = ARBind;
391900140430         §BDcad = ARBcad;
392000140430         §BDlod = ARBlod;
392100140430         §BDprd = ARBprd;
392200140430         §BDnzd = ARBnzd;
392300140430         §BDfin = ARBfin;
392400140506         §BDffd = 'S';
392500140430         §BDcpi = ARBcpi;
392600140429
392700140429         // -?Reperimento ragione sociale estesa del destinatario?
392800140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'D' )  FIAR4000;
392900140429         if  %found(FIAR401L);
393000140429           §BDrd2 = AR4not;
393100140429         endif;
393200140429
393300140429         // -?Reperimento referente e telefono del destinatario?
393400140429         clear  dAR5gen;
393500140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'GEN' )  FIAR5000;
393600140506         if  %found(FIAR501L)  and  AR5atb = *blank;
393700140429           dAR5gen = AR5uni;
393800140429           §BDref = §AR5ref;
393900140429           §BDtel = §AR5teld;
394000140429         endif;
394100140429
394200140429         kpjbu = FNLR48ds;
394300140429         fnlr48r ( kpjba :
394400140429                   dsARBd :
394500140429                   dsARBk :
394600140429                   dARBt :
394700140429                   dsARBg :
394800140429                   TRUL90ds );
394900140429         FNLR48ds = kpjbu;
395000140429
395100140429         if  D48err <> *blank;
395200140429           *in28  = *on;
395300140430           *in36  = *on;
395400140506           V1Cmsg = D48msg;
395500140429           leavesr;
395600140429         endif;
395700140429
395800140429       ENDSR;
395900151130
396000151130       //--------------------------------------------------------------
396100151203       //?Controlli specifici per Motivo Apertura R.A. " 18"=Reso      ?
396200151130       //--------------------------------------------------------------
396300151130       BEGSR  sr_CtrMotAp_18;
396400151130
396500151201         Select;
396600151201           // -?Errore SE inserimento da Sede?
396700151201           When  *in05;
396800151201             *in28  = *on;
396900151201             *in36  = *on;
397000151201             V1Cmsg = Msg(53);
397100151201             leavesr;
397200151201           // -?Errore SE R.A. non per Spedizione?
397300151201           When  V1Ctor <> 'S';
397400151201             *in28  = *on;
397500151201             *in36  = *on;
397600151201             V1Cmsg = %subst( Msg(45) : 1 : 48 );
397700151201             leavesr;
397800151201         EndSl;
397900151130
398000151130         // -?Errore SE bolla EXPORT (LNA estera)?
398100151130         clear  xx;
398200151201         if  $FNARB = *on;
398300151201           xx = %lookup( %editc( ARBlna : 'X' ) : sk_FILest );
398400151201         else;
398500151201           xx = %lookup( %editc( BLPlna : 'X' ) : sk_FILest );
398600151201         endif;
398700151201         if  xx > *zero;
398800151130           *in28  = *on;
398900151130           *in36  = *on;
399000151130           V1Cmsg = Msg(55);
399100151130           leavesr;
399200151130         endif;
399300151130
399400151130         // -?Verifica se esiste già una disposiz. di Reso/Dirottamento?
399500151130         //  ?per la stessa spedizione?
399600151130         clear  FNLRY09ds2;
399700151204         iLRY09tch  = 'T';
399800151130         iLRY09aas  = V1annS;
399900151130         iLRY09lnp  = V1lnpS;
400000151130         iLRY09nrs  = V1nrsS;
400100151130         iLRY09nsp  = V1nspS;
400200151130         kpjbu = FNLRY09ds2;
400300151130         fnlrY09r ( kpjba );
400400151130         FNLRY09ds2 = kpjbu;
400500151130         If  oLRY09esdi = *on;
400600151130           *in28  = *on;
400700151130           *in36  = *on;
400800151130           V1Cmsg = Msg(56);
400900151204           if  oLRY09reso = *on;
401000151204             xx = %scan( 'Dirottamento' : V1Cmsg );
401100151204             V1Cmsg = %replace( 'Reso        ' : V1Cmsg : xx );
401200151204             V1Cmsg = %subst( V1Cmsg : 1 : xx + 4 ) +
401300151204                      %trimL( %subst( V1Cmsg : xx + 4 ) );
401400151130           endif;
401500151130           leavesr;
401600151130         EndIf;
401700151130
401800160314         // -?Verifica se presente Giacenza aperta?
401900160314         chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  TIGCP21;
402000160314         $Giacenza = ( %found( TIGCP21L )  and
402100160314                       GCPatb = *blank     and
402200160314                       GCPfas < 40 );
402300160314         // -?SE non ho trovato il rcd di giacenza in arrivo provo in partenza?
402400160314         If  Not %found( TIGCP21L );
402500160314           chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  TIGCP000;
402600160314           $Giacenza = ( %found( TIGCP01L )  and
402700160314                         GCPatb = *blank     and
402800160314                         GCPfas < 50 );
402900160314         EndIf;
403000160314         if  $Giacenza;
403100160314           *in28  = *on;
403200160314           *in36  = *on;
403300160315           V1Cmsg = Msg(61);
403400160314           leavesr;
403500160314         EndIf;
403600160314
403700160314         // -?Verifica se bolla legata: dirottamento ammesso solo sulla?
403800160314         //  ?ultima figlia?
403900160314         chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  FNLBL000;
404000160314         if  %found( FNLBL02L );
404100160314           *in28  = *on;
404200160314           *in36  = *on;
404300160314           V1Cmsg = Msg(62);
404400160314           leavesr;
404500160314         endif;
404600160314
404700151130       ENDSR;
404800141210
404900141210       //--------------------------------------------------------------
405000141210       //?Ctrl specifici per Motivo Apertura R.A. " 60"=Verif. PINcode ?
405100141210       //--------------------------------------------------------------
405200141210       BEGSR  sr_CtrMotAp_60;
405300141210
405400141210         Select;
405500141210
405600141210           // -?Errore SE si è in Sede?
405700141210           When  *in05;
405800141210             *in28  = *on;
405900141210             *in36  = *on;
406000141210             V1Cmsg = Msg(42);
406100141210             leavesr;
406200141210
406300141212           // -?Errore SE utente NON abilitato alla modifica bolle fil.?
406400141212           When  §UTEmbf = *blank;
406500141210             *in28  = *on;
406600141210             *in36  = *on;
406700141210             V1Cmsg = Msg(27);
406800141210             leavesr;
406900141210
407000141210           // -?Errore SE la spedizione NON è in Arrivo?
407100141210           When  Not $FNARB;
407200141210             *in28  = *on;
407300141210             *in36  = *on;
407400141210             V1Cmsg = Msg(45);
407500141210             leavesr;
407600141210
407700141210         EndSl;
407800141210
407900141210
408000141210         // -?Errore SE la spedizione NON è in consegna oggi?
408100141212         exsr  ctr_SpeCons;
408200141212         if  $Cons = *off;
408300141210           *in28  = *on;
408400141210           *in36  = *on;
408500141210           V1Cmsg = Msg(45);
408600141210           leavesr;
408700141210         endif;
408800141210
408900141210         // -?Verifica SE la Particolarità Consegna prevede il PINcode?
409000141210         clear  ds7R;
409100141210         ds_TNTBE.TBEke1 = ARBgma;
409200141210         if  getTabella ( c_Tabel : '7R'  : ds_TNTBE.TBEke1 :
409300141210                          *omit : *omit : *omit :
409400141210                          *omit : *omit :
409500141210                          *omit : *omit : *omit : *omit :
409600141210                          *omit : *omit : *omit : *omit :
409700141210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
409800141210                        = *zero      AND
409900141210             ds_TNTBE.TBEatb = *blank;
410000141210           ds7R   = ds_TNTBE.TBEuni;
410100141210         endif;
410200141210         if  ds7R.§7RpinCode <> 'S';
410300141210           *in28  = *on;
410400141210           *in36  = *on;
410500141210           V1Cmsg = Msg(46);
410600141210           leavesr;
410700141210         endif;
410800141218
410900141218         // -?Verifica esistenza trk "Y" in FIAR4 per la spedizione?
411000141218         setll  ( V1annS : V1lnpS : V1nrsS : V1nspS : 'Y' )  FIAR4000;
411100141218         if  NOT %equal(FIAR401L);
411200141218           *in28  = *on;
411300141218           *in36  = *on;
411400141218           V1Cmsg = Msg(47);
411500141218           leavesr;
411600141218         endif;
411700141210
411800141210       ENDSR;
411900150717
412000150717       //--------------------------------------------------------------
412100150717       //?Ctrl specifici per Motivo Apertura R.A. " 70"=Reclamo da Sede?
412200150717       //--------------------------------------------------------------
412300150717       BEGSR  sr_CtrMotAp_70;
412400150717
412500150717         Select;
412600150717
412700150717           // -?Errore SE NON si è in Sede?
412800150717           When  Not *in05;
412900150717             *in28  = *on;
413000150717             *in36  = *on;
413100150717             V1Cmsg = Msg(51);
413200150717             leavesr;
413300150717
413400150717         EndSl;
413500150717
413600150717       ENDSR;
413700140429
413800140429      /end-free
413900150421
414000150421       //--------------------------------------------------------------
414100150421       //?Controllo Mail del cliente.                                  ?
414200150421       //--------------------------------------------------------------
414300150421     c     sr_CtrlMail   BEGSR
414400150421      *
414500150421     c                   clear                   DSEMAIL
414600150421     c                   movel     wwMail        §emlindi
414700150421     c                   call      'XEMAIL'
414800150421     c                   parm                    DSEMAIL
414900150421      * errore
415000150421    3c                   If        §emlerro = '1'
415100150421     C                   eval      *IN28 = *on
415200150421     C                   eval      *IN38 = *on
415300150421     c                   movel     §emlmsgo      V1Cmsg
415400150421     c                   leavesr
415500150421      *
415600150421     c                   else
415700150421     c                   movel     §emlindo      wwMail
415800150421    3c                   endif
415900150421      *
416000150421     c                   ENDSR
416100150421
416200150421       //--------------------------------------------------------------
416300150421       //?Controllo Motivo Apertura Reclamo.                           ?
416400150421       //--------------------------------------------------------------
416500150421     c     sr_CtrlMAD    BEGSR
416600150421      *
416700150422     c                   clear                   wwdMAD
416800150422      *
416900150421    2c                   If        wwcMAD <> *blanks
417000150421      *
417100150421     c                   clear                   tibs02ds
417200150421      *
417300150421      * Ricerca
417400150421     c     '?'           scan      wwcMAD
417500150421    3c                   if        %found
417600150421      *
417700150421     c                   clear                   tntbMARds1
417800150421      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
417900150421      *      dall'utente in APERTURA;
418000150421      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
418100150421      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
418200150421      *      in GESTIONE.
418300150421     c                   if        wMod <> 'I'
418400150421     c                   eval      iMARutUt = 'I'
418500150421     c                   endif
418600150421     c                   eval      iMARutAp = 'I'
418700150421     c                   movel(p)  tntbMARds1    KPJBU
418800150421      *
418900150421     c                   call      'TNTBMARR1'
419000150421     c                   parm                    KPJBA
419100150421      *
419200150421     c                   movel     KPJBU         tntbMARds1
419300150421    4c                   if        oMARfxx = *blank  and  oMARerr = *off
419400150421     c                   eval      wwcMAD  = oMARcod
419500150422     c                   eval      wwdMAD  = oMARdes
419600150421      *  ?Impostazione campi (se variati) ?
419700150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
419800150421      *    ?(SE variato il Motivo Apertura)?
419900150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
420000150421if  5c                   if        wMod    =  'I'       and
420100150421     c                             wwcMAD  <> wSaveMAD  and
420200150421     c                             $Video  <> 'W4'
420300150429     c                   eval      V1Cmad  =  wwcMAD
420400150429     c                   eval      wSaveMAD = V1Cmad
420500150421     c                   exsr      sr_Ctrl_MAR
420600150421     c                   eval      V1Ccdt   = §MARcauChP
420700150421e   5c                   endif
420800150421      *  errore (non trovato nulla pulisco il '?' )
420900150421     C                   ELSE
421000150421     c                   clear                   wwcMAD
421100150422     c                   clear                   wwdMAD
421200150421    4C                   ENDIF
421300150421      *
421400150421     c                   eval      *in90 = *on
421500150421     C                   eval      *IN36 = *on
421600150421     c                   leavesr
421700150421      *
421800150421   x3C                   ELSE
421900150421      *
422000150421      * Controllo
422100150421     c                   exsr      sr_Ctrl_MAR
422200150723     c                   if        *in28  or  *in90
422300150421     c                   leavesr
422400150421     c                   endif
422500150421      *  ?Impostazione campi (se variati) ?
422600150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
422700150421      *    ?(SE variato il Motivo Apertura)?
422800150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
422900150421if  4c                   if        wMod    =  'I'       and
423000150421     c                             wwcMAD  <> wSaveMAD  and
423100150421     c                             $Video  <> 'W4'
423200150421     c                   eval      wSaveMAD = wwcMAD
423300150421     c                   exsr      sr_Ctrl_MAR
423400150421     c                   eval      V1Ccdt   = §MARcauChP
423500150421e   4c                   endif
423600150421      *
423700150421    3c                   endif
423800150421      *
423900150421      * Confronto del Motivo associato al Tipo Oggetto Reclamo
424000150421      *           con quello inserito a video
424100150422      *?- SE si viene da subfile-control "C01" può essere stato variato?
424200150422      *?  (NON se si viene dalla window "W04")?
424300150421if  3c                   if        §MARtor <> *blank  and  V1Ctor <> §MARtor
424400150421     c                   clear                   dOGR_S
424500150421     c                   eval      T02mod = 'C'
424600150421     c                   eval      T02sif = knsif
424700150421     c                   eval      T02cod = 'OGR'
424800150421     c                   eval      T02ke1 = §MARtor
424900150421     c                   call      'TIBS02R'
425000150421     c                   parm                    KPJBA
425100150421     c                   parm                    TIBS02ds
425200150421if  4c                   if        T02err = *blanks
425300150421     c                   movel     T02uni        dOGR_S
425400150421e   4c                   endif
425500150421      *
425600150421     c                   eval      *in28 = *on
425700150421     c                   eval      *in31 = *on
425800150421     c                   eval      V1Cmsg = %trim(Msg(39)) +
425900150421     c                                      ' "' + §MARtor + '" = ' +
426000150421     c                                      %trim(dOGR_S.§OGRdesc)
426100150421     c                   leavesr
426200150421e   3c                   endif
426300150421      *
426400150421   x2c                   Else
426500150421      *
426600150421      *  Campo Obbligatorio
426700150421     c                   eval      *in28 = *on
426800150723     C                   eval      *IN36 = *on
426900150421     c                   movel     MSG(8)        V1Cmsg
427000150421     c                   leavesr
427100150421      *
427200150421    2C                   EndIf
427300150421      *
427400150421      *?Controlli specifici per Motivo Apertura R.A. " 17"?
427500150421      *?(Fermo Deposito autorizza mittente)?
427600150421      *?SE non protetto?
427700150421     c                   If        wwcMAD = c_FDEAM
427800150421     c                             and  Not *in50
427900150421     c                             and  Not *in70  and  Not *in72
428000150421     c                             and  Not *in74
428100150421     c                   exsr      sr_CtrMotAp_17
428200150421     c                   if        *in28  or  *in90
428300150421     c                   leavesr
428400150421     c                   endif
428500150421     c                   EndIf
428600151130      *
428700151130      *?Controlli specifici per Motivo Apertura R.A. " 18"?
428800151130      *?(Reso)?
428900151130      *?SE  INSERIMENTO  e  non protetto?
429000151130     c                   If        wwcMAD = c_MotAp_Reso  and
429100151130     c                             Wmod   = 'I'      and
429200151130     c                             Not *in50         and
429300151130     c                             Not *in70         and  Not *in72
429400151130     c                                               and  Not *in74
429500151130     c                   exsr      sr_CtrMotAp_18
429600151130     c                   if        *in28  or  *in90
429700151130     c                   leavesr
429800151130     c                   endif
429900151130     c                   EndIf
430000150421      *
430100150421      *?Controlli specifici per Motivo Apertura R.A. " 60"?
430200150421      *?(Verifica telefonica del "PINcode")?
430300150421      *?SE  INSERIMENTO  e  non protetto?
430400150421     c                   If        wwcMAD = c_VPINC  and
430500150421     c                             Wmod   = 'I'      and
430600150421     c                             Not *in50         and
430700150421     c                             Not *in70         and  Not *in72
430800150421     c                                               and  Not *in74
430900150421     c                   exsr      sr_CtrMotAp_60
431000150421     c                   if        *in28  or  *in90
431100150421     c                   leavesr
431200150421     c                   endif
431300150421     c                   EndIf
431400150717      *
431500150717      *?Controlli specifici per Motivo Apertura R.A. " 70"?
431600150717      *?(Reclamo da Sede)?
431700150717      *?SE non protetto?
431800150717     c                   If        wwcMAD = c_ReclSede
431900150717     c                             and  Not *in70  and  Not *in72
432000150717     c                                             and  Not *in74
432100150717     c                   exsr      sr_CtrMotAp_70
432200150717     c                   if        *in28  or  *in90
432300150717     c                   leavesr
432400150717     c                   endif
432500150717     c                   EndIf
432600170524      *
432700170524      *?Controlli specifici per Motivo Apertura R.A. " 12"?
432800170524      *?(Giacenza)?
432900170524      *?SE non protetto?
433000170524     c                   If        wwcMAD = W_Gia
433100170524     c                             and  Not *in70  and  Not *in72
433200170524     c                             and  Not *in50  and  Not *in74
433300170524     c                   exsr      sr_CtrGiacenza
433400170524     c                   if        *in28  or  *in90
433500170524     c                   leavesr
433600170524     c                   endif
433700170524     c                   EndIf
433800150421      *
433900150421     c                   ENDSR
434000150429
434100150616      /free
434200150616
434300150429       //--------------------------------------------------------------
434400150429       //?Controllo Reparto Gestione.                                  ?
434500150429       //--------------------------------------------------------------
434600150429       BEGSR  sr_CtrlRPR;
434700150429
434800150429         clear  wwdRPR;
434900150429
435000150429         Select;
435100150429
435200150429           When  %scan( '?' : wwcRPR ) > *zero;
435300150429             clear  wwcRPR;
435400150508             clear  tntbRGRds;
435500150508             iRGRopz = 1;
435600150508             kpjbu   = tntbRGRds;
435700150508             tntbRGRr ( KPJBA );
435800150508             tntbRGRds = kpjbu;
435900150508             If  oRGRerr = *off  and  oRGRfxx = *blank;
436000150508               wwcRPR = oRGRcod;
436100150508               wwdRPR = oRGRdes;
436200150508             endif;
436300150429             *in90 = *on;
436400150429             if  $Video = 'W4';
436500150429               *in45 = *on;
436600150429             else;
436700150429               *in39 = *on;
436800150429             endif;
436900150429             leavesr;
437000150429
437100150429           When  wwcRPR = *blank;
437200150429             *in28 = *on;
437300150429             if  $Video = 'W4';
437400150429               *in45 = *on;
437500150429             else;
437600150429               *in39 = *on;
437700150429             endif;
437800150616             if  *in05;
437900150616               V1Cmsg = Msg(20);
438000150616             else;
438100150616               V1Cmsg = Msg(50);
438200150616             endif;
438300150429             leavesr;
438400150429
438500150429           Other;
438600150429             clear  TIBS02ds;
438700150429             T02mod = 'C';
438800150429             T02sif = knsif;
438900150429             T02cod = 'RGR';
439000150429             T02ke1 = wwcRPR;
439100150429             TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
439200150429             if  T02err = *blank;
439300150429               wwdRPR = T02uni;
439400150429             else;
439500150429               *in28 = *on;
439600150429               if  $Video = 'W4';
439700150429                 *in45 = *on;
439800150429               else;
439900150429                 *in39 = *on;
440000150429               endif;
440100150616               if  *in05;
440200150616                 V1Cmsg = Msg(20);
440300150616               else;
440400150616                 V1Cmsg = Msg(50);
440500150616               endif;
440600150429               leavesr;
440700150429             endif;
440800150429
440900150429         EndSl;
441000150429
441100150429       ENDSR;
441200150616
441300150616       //--------------------------------------------------------------
441400150616       //?Controllo Utente Gestione.                                   ?
441500150616       //--------------------------------------------------------------
441600150616       BEGSR  sr_CtrlRPG;
441700150616
441800150616         clear  wwdRPR;
441900150616
442000150616         // -?Verifica se esiste utente nell'archivio profili?
442100150616         chain  ( wwcRPR )  AZUTE000;
442200150616
442300150616         If  %found(AZUTE01L)  and  UTEdsc > DateU;
442400150616
442500150616           // -?SE lo trova: verifica se è della filiale di gestione?
442600150723           if  %editc(UTEfil:'X') <> wwcPOG;
442700150616             *in28  = *on;
442800150616             *in46  = *on;
442900150616             V1Cmsg = Msg(23);
443000150616             leavesr;
443100150616           endif;
443200150616
443300150616         Else;
443400150616
443500150616           *in28 = *on;
443600150616           *in45 = *on;
443700150616           V1Cmsg = Msg(20);
443800150616           leavesr;
443900150616
444000150616         EndIf;
444100150616
444200150616       ENDSR;
444300150911
444400150911       //--------------------------------------------------------------
444500150911       //?Controllo C.A.P. cliente per reperimento Filiale Gestione    ?
444600150911       //?(per R.A. inerita in SEDE)                                   ?
444700150911       //--------------------------------------------------------------
444800150911       BEGSR  sr_CtrlCAP;
444900150911
445000150911         // -?Se CAP vuoto: uscita?
445100150914         if  W4cCap <> *blank  and  %trim( W4cCap ) = *zero;
445200150911           clear  W4cCap;
445300150911         endif;
445400150911
445500150911         if  W4cCap = *blank;
445600150911           leavesr;
445700150911         endif;
445800150915
445900150915         // -?Selezione CAP?
446000150915         if  %scan( '?' : W4cCap ) > *zero;
446100150915
446200150915           clear  W4cCap;
446300150915           clear  TISI00ds;
446400150915           D00op0 = 'L01';
446500150915           D00dri = DateU;
446600150915           clear  TISI30ds;
446700150915           I30flg = 'S';
446800150915           TISI30R ( kpjba : TISI00ds : TISI30ds );
446900150915           if  D00cap <> *blank;
447000150915             W4cCap = D00cap;
447100150915           endif;
447200150915
447300150915         endif;
447400150911
447500150911         // -?Reperimento filiale (LNA) dal CAP?
447600150911         clear  TISI95ds;
447700150911         I95tcn = '3';
447800150911         I95cap = W4cCap;
447900150911         I95dat = DateU;
448000150911
448100150911         TISI95R ( TISI95ds );
448200150911
448300150914         If  O95err = *blank;
448400150914
448500150914           if  W4cPog <> %editc( O95lna : 'X' );
448600150914             *in90 = *on;
448700150914           endif;
448800150914           W4cPog = %editc( O95lna : 'X' );
448900150914           clear  W4cCap;
449000150915
449100150915           reset  FNLV55ds;
449200150915           //D55tla = 'L';
449300150915           //D55tpt = 'P';
449400150915           D55lin = O95lna;
449500150915           D55drf = DateU;
449600150915           fnlv55r ( FNLV55ds );
449700150915           if  D55err = *blank;
449800150915             W4cPog = %editc( D55tfp : 'X' );
449900150915           endif;
450000150914
450100150914         Else;
450200150914
450300150914           *in28 = *on;
450400150914           *in56 = *on;
450500150914           V1Cmsg = Msg(54);
450600150914           leavesr;
450700150914
450800150914         EndIf;
450900150911
451000150911       ENDSR;
451100150616
451200150616      /end-free
451300150421
451400150421       //--------------------------------------------------------------
451500150421       //?Controllo Filiale Gestione.                                  ?
451600150421       //--------------------------------------------------------------
451700150421     c     sr_CtrlPOG    BEGSR
451800150421      *
451900150422     c                   clear                   wwdPOG
452000150422      *
452100150421     c                   IF        %scan('?':wwcPOG) > 0
452200150421     c                   clear                   dstnsd
452300150421     c                   call      'TNSD24R'
452400150421     c                   parm                    §cod
452500150421     c                   parm                    §tip
452600150421     c                   parm                    §des
452700150421     c                   parm                    §pra
452800150421     c                   movel     §cod          wwcPOG
452900150422     c                   movel     §des          wwdPOG
453000150421     c                   eval      *in90 = *on
453100150421     c                   eval      *in46 = *on
453200150421     c                   leavesr
453300150421     c                   ENDIF
453400150421
453500150421     c                   IF        wwcPOG = *blanks
453600150421     c                   eval      *in28  = *on
453700150421     c                   eval      *in46  = *on
453800150421     c                   eval      V1Cmsg = Msg(48)
453900150421     c                   leavesr
454000150421     c                   ENDIF
454100150421
454200150421     c                   eval      xx = 1
454300150421     c                   FOR       xx by 1 to %len(wwcPOG)
454400150421     c                   IF        %subst(wwcPOG:xx:1) <> *blanks and
454500150421     c                             %check(Digits:%subst(wwcPOG:xx:1)) > *zeros
454600150421     c                   eval      *in28 = *on
454700150421     c                   eval      *in46 = *on
454800150421     c                   eval      V1Cmsg = Msg(48)
454900150421     c                   leavesr
455000150421     c                   ENDIF
455100150421     c                   ENDFOR
455200150421
455300150421     c                   move      wwcPOG        w0030
455400160104     c     w0030         chain     AZORG
455500150717     c                   IF        not %found(AZORG01L)  OR
455600150717     c                              ORGfva <> *blanks    OR
455700150428     c                             (wwcPOG <> c_Sede  and
455800150717     c                              ORGfag <> 'F'     and
455900150723     c                              ORGfag <> 'A')
456000150421     c                   eval      *in28 = *on
456100150421     c                   eval      *in46 = *on
456200150421     c                   eval      V1Cmsg = Msg(48)
456300150421     c                   leavesr
456400150421     c                   ENDIF
456500150422     c                   movel     ORGdes        wwdPOG
456600160120      *
456700150428      *?Filiale Gestione 046 inseribile (in modifica) SOLO da utente di Sede?
456800151211     c                   If        wMod   <> 'I'    and
456900150428     c                             wwcPOG = c_Sede  and  SIMfel <> *zero
457000160121     c                                              and  NOT $AttesaRisposta
457100150429      * - prima verifica che non sia stata richiesta l'interrogazione
457200160121     c                   if        %scan( '?' : V1Crpg ) > *zero
457300150429     c                   eval      wwcRPR = V1Crpg
457400150429     c                   exsr      sr_CtrlRPR
457500160121     c                   eval      V1Crpg = wwcRPR
457600150429     c                   eval      *in28 = *off
457700150429     c                   eval      *in45 = *off
457800150429     c                   eval      *in39 = *off
457900150429     c                   clear                   V1Cmsg
458000160120     c                   endif
458100160121     c                   eval      w_Ute  = V1Crpg
458200150428     c                   exsr      sr_TabUGR
458300150428     c                   if        §UGRpofo <> %int(c_Sede)
458400150428     c                   eval      *in28 = *on
458500150428     c                   eval      *in46 = *on
458600150428     c                   eval      V1Cmsg = Msg(48)
458700150428     c                   leavesr
458800150428     c                   endif
458900150428     c                   EndIf
459000150421      *
459100150421     c                   ENDSR
459200150428
459300150428      /free
459400150428
459500150428       //--------------------------------------------------------------
459600150428       //?Reperimento dati Tab. "UGR" (Utente Gestione Reclami)        ?
459700150428       //--------------------------------------------------------------
459800150428       BEGSR  sr_TabUGR;
459900150428
460000150428         clear  dUGR;
460100150428         clear  TIBS02ds;
460200150428         T02mod = 'C';
460300150428         T02sif = KNSIF;
460400150428         T02cod = 'UGR';
460500150428         T02ke1 = w_Ute;
460600150428
460700150428         TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
460800150428
460900150428         if  T02err = *blank;
461000150428           dUGR = T02uni;
461100150428         endif;
461200150428
461300150428       ENDSR;
461400150428
461500150428      /end-free
461600140429
461700070430      *---------------------------------------------------------------*
461800070430      * Controlli parte alta del ctl per f4 fine richiesta            *
461900070430      *---------------------------------------------------------------*
462000070430     c     SR_CONTRF4    BegSR
462100070430
462200070430    1c                   do        *hival
462300070430      * se dato F4 e non è stato inserito nessun oggetto e non lo sto inserendo adesso ERRORE
462400070430    2c                   if        s01nrr = 0  and v1ctor = *blanks and
462500070430     c                             v1cogg = *blanks and ormvid = *zeros and
462600070430     c                             spevid = *zeros and v1ctch = *blanks and
462700070430     c                             v1cmad = *blanks and v1ccdt = *blanks and
462800070430     c                             v1not1 = *blanks and v1not2 = *blanks and
462900070430     c                             v1not3 = *blanks and v1not4 = *blanks and
463000070430     c                             v1crsp = *blanks and v1cpor = *blanks
463100070430     c                   eval      *in28 = *on
463200070430     c                   movel     MSG(14)       V1Cmsg
463300070430     C                   eval      *IN44 = *on
463400070430     c                   leave
463500070430    2c                   endif
463600070430      * verifco se inserito  il codice chiamante
463700070430    2c                   If        v1cchi = *zeros and wforz = ' '
463800070430      * richamo la routine di ricerca codice nel dettaglio oggetti
463900070430     c                   exsr      SR_RECCHI
464000070430      * controllo se ne ho uno da proporre
464100070430     c                   select
464200070430     c                   when      wrk_cchiA > 0
464300070430     c                   eval      wrk_cchi = wrk_cchiA
464400070430     c                   when      wrk_cchiB > 0
464500070430     c                   eval      wrk_cchi = wrk_cchiB
464600070430     c                   other
464700070430     c                   eval      wrk_cchi = wrk_cchiC
464800070430     c                   endsl
464900070430      *
465000070430    3c                   if        wrk_cchi > *zeros
465100070430     c                   eval      v1cchi = wrk_cchi
465200070430     c                   eval      SW_v1cchi = wrk_cchi
465300070430     c                   clear                   wrk_cchi
465400070430      * se ragione sociale a blank la decodifico
465500070430     C                   clear                   TIBS69DS
465600070430     c                   z-add     v1cchi        I69kac
465700070430     c                   z-add     v1cchi        I69kcp
465800070430     C                   call      'TIBS69R'
465900070430     C                   parm                    tibs69DS
466000070430     C                   parm                    DS_cnaco
466100070430     C                   parm                    DS_cnind
466200070430     C                   parm                    DS_cnclp
466300070430     C                   parm                    DS_fncls
466400070430     C                   eval      wtibs69r = '1'
466500070430    4c                   If         O69ERR =  ' '
466600070430     c                   movel     ACOrag        v1dchi
466700070430     c                   movel     clpclv        v1cclv
466800070430    4c                   Endif
466900070430      *
467000070430   x3c                   else
467100070430      *
467200070430     c                   eval      wforz = '1'
467300070430     c                   eval      *in28 = *on
467400070430     c                   movel     MSG(32)       V1Cmsg
467500070430     C                   eval      *IN10 = *on
467600070430     c                   leave
467700070430    3c                   endif
467800070430    2c                   endif
467900070430
468000070430      * controllo del chiamante se uguale a blanks ma codice cliente valorizzato non faccio nulla
468100070430
468200070430    2c                   If        v1crsc = *blanks and v1dchi = *blanks
468300070430     c                   eval      *in28 = *on
468400070430     c                   movel     MSG(15)       V1Cmsg
468500070430     C                   eval      *IN44 = *on
468600070430     c                   leave
468700070430      * Se anche il chiamante è a blank imposto la ragione sociale del cliente
468800070430     c                   else
468900070430    3c                   if        v1dchi <> *blanks AND v1crsc = *blanks
469000070430     c                   eval      v1crsc = v1dchi
469100070430    3c                   endif
469200070430    2c                   endif
469300070430      *
469400070503     c                   leave
469500070430     c                   enddo
469600070430      *
469700070430     c                   ENDSR
469800070430      *
469900070430      *---------------------------------------------------------------*
470000070430      * Gestione interrogazione oggetto                               *
470100070430      *---------------------------------------------------------------*
470200070430     c     SR_GESF2      BegSR
470300070430
470400070430    1c                   do        *hival
470500070430      * interrogazione Spedizione
470600070430      *
470700070430    2c                   if        v1ctor = 'S'
470800131119      * se non sono in sede
470900070430    3c                   if        not *in05
471000070430      *
471100160701      * verifico se programma richiamato dal PGM interrogazione  Bolle
471200070430    5c                   If        *in50
471300070430     c                   setoff                                       75
471400070430     c     'FNLR36R   '  lookup    £Stack                                 75
471500150615     c  n75'FNLS05R   '  lookup    £Stack                                 75
471600160706     c  n75'FNLRU6R1  '  lookup    £Stack                                 75
471700070430
471800070430      * se trovato vado fuori dal programma
471900070430    6c                   if        %found
472000070430     c                   eval      *in90 = *on
472100070430     c                   eval      *in31 = *on
472200170512     c                   movel     MSG(63)       V1Cmsg
472300170512     C                   eval      *IN28 = *on
472400070430     c                   leave
472500070430    6c                   endif
472600070430      *
472700070430    5c                   endif
472800070430      *
472900070430      * se richiamato
473000070430     c                   clear                   fidna1ds
473100160706      *
473200150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
473300150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene ..
473400150601    5c                   if        *in50
473500150601      *
473600150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
473700150601      * oggetto ho più di una RA
473800150601     c                   exsr      Sr_countRA
473900150601      *
474000150601     C                   ELSE
474100150601     c                   eval      IA1INS = 'I'
474200150601    5c                   endif
474300160706      *
474400160706     c                   clear                   KPJBU
474500160706     c                   clear                   ParamU6ds1
474600160706     c                   eval      ParamU6ds1.V2Caas = V1anns
474700160706     c                   eval      ParamU6ds1.COMlnp = V1lnps
474800160706     c                   eval      ParamU6ds1.V2Cnrs = V1nrss
474900160706     c                   eval      ParamU6ds1.V2Cnsp = V1nsps
475000160706     c                   eval      KPJBU = ParamU6ds1
475100160706      * preparo l'altro parametro da passare
475200160706     c                   clear                   fnlrU6ds
475300160706     c*//                eval      iLRU6sel = 'S'
475400160706     c                   eval      iLRU6ra  = 'S'
475500160706     c                   eval      eLRU6ins = iA1ins
475600160706      *
475700160706     c                   call      'FNLRU6R1'
475800160706     c                   parm                    KPJBA
475900160706     c                   parm                    fnlrU6ds
476000160706     c                   movel     kpjbu         ParamU6ds1
476100160706     c                   if        oLRU6err = 'E'
476200160706     C                   eval      *in28 = *on
476300160706     C                   eval      *in44 = *on
476400160706     c                   movel     oLRU6msg      V1Cmsg
476500160706     c                   leave
476600160706     c                   endif
476700070430      *
476800070430     c                   eval      *in90 = *on
476900070430     C                   eval      *IN31 = *on
477000070430     c                   leave
477100070430      *
477200070430      * sono in sede
477300070430      *
477400070430   x3c                   else
477500070430      *
477600070430     c     KSPED         chain     Titas30c
477700070430      * spedizione in sede
477800070430    4c                   If        %found(Titas30c)
477900070430      * verifico se programm
478000070430
478100070430    5c                   If        *in50
478200070430     c                   setoff                                       75
478300070430     c     'TNSB50R   '  lookup    £Stack                                 75
478400070430
478500070430      * se trovato vado fuori dal programma
478600070430    6c                   if        %found
478700070430     c                   eval      *in90 = *on
478800070430     c                   eval      *in31 = *on
478900070430     c                   leave
479000070430    6c                   endif
479100120330      * se non trovo il TNSB50R faccio un tentativo anche con il TNSB51R
479200120330     c                   setoff                                       75
479300120330     c     'TNSB51R   '  lookup    £Stack                                 75
479400120330
479500120330      * se trovato vado fuori dal programma
479600120330    6c                   if        %found
479700120330     c                   eval      *in90 = *on
479800120330     c                   eval      *in31 = *on
479900120330     c                   leave
480000120330    6c                   endif
480100070430      *
480200070430    5c                   endif
480300070430      *
480400070430     c                   clear                   TNSB50DS
480500070430     C                   movel     'R05'         I50OP0
480600070430     C                   z-add     V1anns        D50aas
480700070430     C                   z-add     V1lnps        D50lnp
480800070430     C                   z-add     V1nrss        D50nrs
480900070430     C                   z-add     V1nsps        D50nsp
481000070430     C                   movel(P)  TNSB50DS      KPJBU
481100070430      *
481200070430      * se richiamato
481300070430     c                   clear                   fidna1ds
481400150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
481500150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
481600150601    5c                   if        *in50
481700150601      *
481800150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
481900150601      * oggetto ho più di una RA
482000150601     c                   exsr      Sr_countRA
482100150601      *
482200150601     C                   ELSE
482300150601     c                   eval      IA1INS = 'I'
482400150601    5c                   endif
482500070430      *
482600070430     C                   call      'TNSB50R'
482700070430     C                   parm                    KPJBA
482800070430     C                   parm                    FIDNA1DS
482900070430      *
483000070430
483100070430     c                   eval      *in90 = *on
483200070430     C                   eval      *IN31 = *on
483300070430     c                   leave
483400070430
483500070430    4C                   endif
483600070430
483700070430    3c                   endif
483800070430
483900070430    2c                   endif
484000070430      *
484100070430      * interrogazione ORM
484200070430      * interrogo direttamente con il richiano di FIOR07R
484300070430      *
484400070430    2c                   if        v1ctor = 'O'
484500070430      * verifico se programma richiamato dal PGM interrogazione  ORM
484600070430
484700070430    3c                   If        *in50
484800070430     c                   setoff                                       75
484900070430     c     'FIOR07R   '  lookup    £Stack                                 75
485000070430      * se trovato vado fuori dal programma
485100070430    4c                   if        %found
485200070430     c                   eval      *in90 = *on
485300070430     c                   eval      *in31 = *on
485400070430     c                   leave
485500070430    4c                   endif
485600070430      *
485700070430    3c                   endif
485800070430      *
485900070430     C                   clear                   PARM01
486000070430     C                   move      v1poeo        ppoe
486100070430     C                   move      v1nsor        pnsr
486200070430     C                   move      v1norm        pnor
486300070430     C                   move      v1nrvo        pnrv
486400070430     C                   move      '5'           psce
486500070430      * attenzione non muovo nulla nel campo del p.o. gestione .... vediamo se funziona !!!!!!
486600070430     C                   movel(p)  PARM01        KPJBU
486700070430      * se richiamato
486800070430     c                   clear                   fidna1ds
486900150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
487000150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
487100150601    3c                   if        *in50
487200150601      *
487300150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
487400150601      * oggetto ho più di una RA
487500150601     c                   exsr      Sr_countRA
487600150601      *
487700150601     c                   else
487800150601     c                   eval      IA1INS = 'I'
487900150601    3c                   endif
488000070430      *
488100070430     C                   call      'FIOR07R'
488200070430     C                   parm                    KPJBA
488300070430     C                   parm                    FIDNA1DS
488400070430
488500070430      *
488600070430     c                   eval      *in90 = *on
488700070430     C                   eval      *IN31 = *on
488800070430     c                   leave
488900070430
489000070430    2c                   endif
489100070430
489200070430    1c                   enddo
489300070430
489400070430     c                   endsr
489500070430      *
489600070323      *---------------------------------------------------------------*
489700070323      * Routine gestione richiesta data consegna richiesta            *
489800070323      *---------------------------------------------------------------*
489900070323     c     SR_GES_DTA    BegSR
490000070323      *
490100070416      * se si tratta di una spedizione e non è stata consegnata
490200070327     c                   if        v1ctor = 'S' and arbdcm = *zeros
490300070323      * verifico se sono la filiale di arrivo della bolla
490400130611     c                   move      arblna        w003a
490500130610     c     w003a         lookup    wPOG                                   27
490600070326     c                   If        %found
490700070326      * verifico se si tratta di :
490800070326      *                            lasciato avviso
490900070326      *                            contrassegno
491000070326      *                            supermercato
491100070326      *                            appuntamento
491200070326      *                            fermo deposito
491300070327      *
491400070430      * richiamo il pgm dei blocchi FNLRG6R
491500070430     c                   clear                   FNLRG6DS
491600070430     c                   eval      ilrg6tla = 'L'
491700070430     c                   eval      ilrg6aas = v1anns
491800070430     c                   eval      ilrg6lnp = v1lnps
491900070430     c                   eval      ilrg6nrs = v1nrss
492000070430     c                   eval      ilrg6nsp = v1nsps
492100070430     c                   call      'FNLRG6R'
492200070430     c                   parm                    fnlrg6ds
492300070430     c                   movel     olrg6abi      dlrg6abi
492400070430     c
492500070327     c                   select
492600070326      * Lasciato Avviso
492700070430     c                   when      v1cmad = W_lav
492800070327      * verifico se si tratta di spedizione bloccata
492900070430     c                   If        d§lrg6tla <> ' '
493000070327     c                   eval      W_cau = 'L'
493100070327     c                   exsr      Sr_ges_bloc
493200070327     c                   else
493300070329      * verifico se la spedizione non ha blocchi di altro tipo
493400070507     c                   if        d§lrg6tca = ' ' and d§lrg6tsu = ' '
493500070507     c                             and d§lrg6tap = ' ' and d§lrg6tfd = ' '
493600070329      * se la spedizione non è bloccata per nessun altro motivo
493700070330      * accendo indicatore di emissione della data consegna richiesta
493800070412     c                   seton                                        616290
493900070329      * gestione della videata con immissione data consegna richiesta + richiamo fnlr48r
494000070329     c                   eval      $riemetti = *on
494100070411      * valorizzo il flag per non essere richiamato di nuovo
494200070411     c                   eval      $ric_fnlrg2r = *on
494300070329     c                   endif
494400070327
494500070327     c                   endif
494600070507     c                   leavesr
494700070327
494800070502      * Contrassegno   e spedizione bloccata per c/assegno
494900070502     c                   when      v1cmad = W_con  and d§lrg6tca <> ' '
495000070327     c                   eval      W_cau = 'C'
495100070327     c                   exsr      Sr_ges_bloc
495200070507     c                   leavesr
495300070327
495400070502      * Supermercato   e spediizone bloccata per supermercato
495500070507     c                   when      v1cmad = W_sup  and d§lrg6tsu <> ' '
495600070327     c                   eval      W_cau = 'S'
495700070327     c                   exsr      Sr_ges_bloc
495800070507     c                   leavesr
495900070327
496000070502      * Appuntamento   e la spedizione è bloccata per appuntamento
496100070507     c                   when      v1cmad = W_app  and d§lrg6tap <> ' '
496200070327     c                   eval      W_cau = 'A'
496300070327     c                   exsr      Sr_ges_bloc
496400070507     c                   leavesr
496500070502
496600070502      * Fermo deposito e la spedizione è bloccata per fermo deposito
496700070502     c                   when      v1cmad = W_fde  and d§lrg6tfd <> ' '
496800070327     c                   eval      W_cau = 'F'
496900070327     c                   exsr      Sr_ges_bloc
497000070327
497100070327     c                   endsl
497200070327
497300070327     c                   endif
497400070327
497500070327     c                   endif
497600070327
497700070323     c                   Endsr
497800070327      *---------------------------------------------------------------*
497900070327      * Routine gestione sblocchi spedizioni                          *
498000070327      *---------------------------------------------------------------*
498100070327     c     SR_GES_BLOC   BegSR
498200070411      *
498300070411      * valorizzo il flag per non essere richiamato di nuovo
498400070411     c                   eval      $ric_fnlrg2r = *on
498500070327      *
498600070327     c                   clear                   Fnlrg2ds
498700070327     c                   eval      ilrg2tla = 'L'
498800070327     c                   eval      ilrg2tch = 'R'
498900070327     c                   eval      ilrg2aas = v1anns
499000070327     c                   eval      ilrg2lnp = v1lnps
499100070327     c                   eval      ilrg2nrs = v1nrss
499200070327     c                   eval      ilrg2nsp = v1nsps
499300070327     c                   eval      ilrg2cau = w_cau
499400070327      * se le note a video sono valorizzate
499500070327     c                   clear                   nov
499600070327     c                   eval      nov(1) = v1not1
499700070327     c                   eval      nov(2) = v1not2
499800070327     c                   eval      nov(3) = v1not3
499900070327     c                   eval      nov(4) = v1not4
500000070327      *
500100070327     c                   movea     nov           ilrg2no1
500200070327
500300070327     c                   call      'FNLRG2R'
500400070327     c                   Parm                    kpjba
500500070327     c                   Parm                    Fnlrg2ds
500600070327
500700070327      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
500800070327     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
500900070327      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
501000070403     c                   eval      $bloccof3    = *on
501100070327      * scrittura delle note
501200070327     c                   exsr      Sr_wrt_note
501300160512     c                   eval      $Note_scritte = *on
501400070503      * verifico se ci sono altri blocchi per questa spedizione gestibili da R.A.
501500070503     c                   exsr      Sr_piublocchi
501600070507      * recupero le note
501700070507      *
501800070507     C                   clear                   FIDNB0DS
501900070507     c                   z-add     v1cant        iB0ant
502000070507     c                   z-add     v1cnut        iB0nut
502100070507     c                   z-add     v1cpdt        iB0pdt
502200070507     c                   movel     'GEN'         iB0tIN
502300070507     c                   movel     'R'           iB0flm
502400070507      *
502500070507     C                   CALL      'FIDNB0R'
502600070507     C                   PARM                    kpjba
502700070507     C                   PARM                    FIDNB0DS
502800070507      *
502900070507      * valorizzo le prime 4 note a video
503000070507      *
503100070507     c                   movel     DB0NO1        v1not1
503200070507     c                   movel     DB0NO2        v1not2
503300070507     c                   movel     DB0NO3        v1not3
503400070507     c                   movel     DB0NO4        v1not4
503500070508      * se ob0piu è valorizzato ci sono + di 4 righe
503600070508     c                   eval      *in29 = (OB0PIU = '+')
503700070327      * proteggo i campi e riemetto la videata
503800070418     c                   seton                                        6074
503900070403     c                   eval      $riemetti = *on
504000070327     c                   endif
504100070418
504200070418     c                   eval      *in37 = *on
504300070327
504400070327     c                   endsr
504500070327      *---------------------------------------------------------------*
504600070327      * Routine gestione note che vengono dalla gestione blocchi      *
504700070327      *---------------------------------------------------------------*
504800070327     c     SR_wrt_note   BegSR
504900070412      *
505000070412     c                   clear                   nora
505100070412     c                   clear                   nob
505200070413     c                   clear                   x
505300070413     c                   clear                   y
505400070327      *
505500070327     c                   movea     olrg2no1      nob(1)
505600070327     c                   movea     olrg2no2      nob(9)
505700070327      * tolgo le note a blank
505800070327     c                   do        12            x
505900070327     c                   if        nob(x) <> *blanks
506000070327     c                   add       1             y
506100070327     c                   move      nob(x)        nora(Y)
506200070327     c                   endif
506300070327     c                   enddo
506400070328      * accendo indicatore che mi segnale che ci sono altre note se ne ho + di 4
506500070328     c                   eval      *in29 = y > 4
506600070328      * registro le note sul file note
506700070328    1c                   if        y > *zeros
506800070328     c                   clear                   FIDNB0DS
506900070328     c                   eval      db0no1 = nora(1)
507000070328     c                   eval      db0no2 = nora(2)
507100070328     c                   eval      db0no3 = nora(3)
507200070328     c                   eval      db0no4 = nora(4)
507300070508
507400070508     c                   exsr      SR_add_note
507500070508
507600070328      * verifico se ho altre note da scrivere
507700070328    2c                   if        nora(5) <> *blanks
507800070328     c                   clear                   fidnb0ds
507900070328     c                   eval      db0no1 = nora(5)
508000070328     c                   eval      db0no2 = nora(6)
508100070328     c                   eval      db0no3 = nora(7)
508200070328     c                   eval      db0no4 = nora(8)
508300070328
508400070508     c                   exsr      SR_add_note
508500070328
508600070328    2c                   endif
508700070328      * verifico se ho altre note da scrivere
508800070328    2c                   if        nora(9) <> *blanks
508900070328     c                   clear                   fidnb0ds
509000070328     c                   eval      db0no1 = nora(9)
509100070328     c                   eval      db0no2 = nora(10)
509200070328     c                   eval      db0no3 = nora(11)
509300070328     c                   eval      db0no4 = nora(12)
509400070328
509500070508     c                   exsr      SR_add_note
509600070508
509700070328    2c                   endif
509800070328
509900070328    2c                   endif
510000070328
510100070328
510200070327     c                   endsr
510300070328      *
510400070508      *---------------------------------------------------------------*
510500070508      * Routine che aggiunge note al reclamo esistente                *
510600070508      *---------------------------------------------------------------*
510700070508     c     SR_add_note   BegSR
510800070508
510900070508     c                   z-add     v1cant        iB0ant
511000070508     c                   z-add     v1cnut        iB0nut
511100070508     c                   z-add     v1cpdt        iB0pdt
511200070508     c                   z-add     999           iB0pgn
511300070508     c                   movel     'GEN'         iB0tIN
511400070508     c                   eval      ib0pot =v1cpot
511500070508     c                   eval      db0pon =Dutpou
511600070508     c                   eval      db0utn =knmus
511700070508      * imposto 'W'
511800070508     c                   movel     'W'           iB0flm
511900070508      *
512000070508     c                   call      'FIDNB0R'
512100070508     c                   parm                    kpjba
512200070508     c                   parm                    fidnb0ds
512300070508
512400070508      *
512500070508     c                   endsr
512600070503      *---------------------------------------------------------------*
512700070503      * Routine gestione eventuali blocchi in spediizione             *
512800070503      *---------------------------------------------------------------*
512900070503     c     SR_piublocchi BegSR
513000070503      * salvo il precedente blocco
513100070503     c                   eval      sw_cau = W_cau
513200070508     c                   clear                   note_blocco
513300070503      * richiamo il pgm dei blocchi FNLRG6R
513400070503     c                   clear                   FNLRG6DS
513500070503     c                   eval      ilrg6tla = 'L'
513600070503     c                   eval      ilrg6aas = v1anns
513700070503     c                   eval      ilrg6lnp = v1lnps
513800070503     c                   eval      ilrg6nrs = v1nrss
513900070503     c                   eval      ilrg6nsp = v1nsps
514000070503     c                   call      'FNLRG6R'
514100070503     c                   parm                    fnlrg6ds
514200070503     c                   movel     olrg6abi      dlrg6abi
514300070503      * verifico se ci sono ulteriori blocchi a quello gestito in precedenza
514400070503     c                   select
514500070507      * C/assegno
514600070503     c                   when      d§lrg6tca <> ' ' and sw_cau <> 'C'
514700070503     c                   eval      w_cau = 'C'
514800070508     c                   eval      note_blocco = 'Abilitazione : C/assegno'
514900070503     c                   exsr      sr_ges_bloc2
515000070507      * supermercato
515100070507     c                   when      d§lrg6tsu <> ' ' and sw_cau <> 'S'
515200070507     c                   eval      w_cau = 'S'
515300070508     c                   eval      note_blocco = 'Abilitazione : Supermercato'
515400070507     c                   exsr      sr_ges_bloc2
515500070507      * appuntamento
515600070507     c                   when      d§lrg6tap <> ' ' and sw_cau <> 'A'
515700070507     c                   eval      w_cau = 'A'
515800070508     c                   eval      note_blocco = 'Abilitazione : Appuntamento'
515900070507     c                   exsr      sr_ges_bloc2
516000070507      * fermo deposito
516100070507     c                   when      d§lrg6tfd <> ' ' and sw_cau <> 'F'
516200070507     c                   eval      w_cau = 'F'
516300070508     c                   eval      note_blocco = 'Abilitazione : Fermo Deposito'
516400070507     c                   exsr      sr_ges_bloc2
516500070507      * lasciato avviso
516600070507     c                   when      d§lrg6tla <> ' ' and sw_cau <> 'L'
516700070507     c                   eval      w_cau = 'L'
516800070508     c                   eval      note_blocco= 'Abilitazione : Lasciato Avviso'
516900070507     c                   exsr      sr_ges_bloc2
517000070507
517100070507     c                   endsl
517200070507
517300070503     c                   endsr
517400070503      *
517500070503      *---------------------------------------------------------------*
517600070503      * Routine gestione blocco come seconda o terza chiamata         *
517700070503      *---------------------------------------------------------------*
517800070503     c     SR_ges_bloc2  BegSR
517900070503      *
518000070503     c                   clear                   Fnlrg2ds
518100070503     c                   eval      ilrg2tla = 'L'
518200070503     c                   eval      ilrg2tch = 'R'
518300070503     c                   eval      ilrg2aas = v1anns
518400070503     c                   eval      ilrg2lnp = v1lnps
518500070503     c                   eval      ilrg2nrs = v1nrss
518600070503     c                   eval      ilrg2nsp = v1nsps
518700070503     c                   eval      ilrg2cau = w_cau
518800070503
518900070503     c                   call      'FNLRG2R'
519000070503     c                   Parm                    kpjba
519100070503     c                   Parm                    Fnlrg2ds
519200070503
519300070503      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
519400070503     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
519500070503      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
519600070503     c                   eval      $bloccof3    = *on
519700070508      * prima di scrivere le note  scrivo il motivo del 2° blocco
519800070508     c                   clear                   FIDNB0DS
519900070508     c                   eval      db0no1 = note_blocco
520000070508     c                   exsr      sr_add_note
520100070503      * scrittura delle note
520200070503     c                   exsr      Sr_wrt_note
520300160512     c                   eval      $Note_scritte= *on
520400070503     c                   endif
520500070503
520600070503     c                   endsr
520700131206      *
520800131206      *---------------------------------------------------------------*
520900131206      * Controllo se la spedizione è in consegna                      *
521000131206      *---------------------------------------------------------------*
521100131206     c     CTR_specons   BegSR
521200131206      *
521300140204     c                   setoff                                       16
521400131206     c                   eval      $cons = *off
521500131206      * Aggancio bolle arrivi
521600160104     c     KSPED         chain     Fnarb000
521700131206     c                   If        %found(Fnarb01l)
521800131206      * se si tratta di una spedizione e non è stata consegnata
521900131206     c                   If        ARBfbc = *blanks
522000131206     c                             and  ARBddc > *zeros
522100131206     c                             and  ARBndc > *zeros
522200160317     c                             and  ARBndc <> *all'8'
522300160317     c                             and  ARBndc <> *all'9'
522400131206     c                             and (ARBdcm = *zero    or
522500131206     c                                  ARBica = *blanks  or  ARBica = 'R'
522600131206     c                              or  ARBicc = *blanks  or  ARBicc = 'R')
522700131206     c                   eval      $cons = *on
522800131206     c                   Endif
522900070328      *
523000131206     c                   Endif
523100140123      *
523200131206     c                   endsr
523300131206      *
523400040212      *---------------------------------------------------------------*
523500040212      * Routine conferma riga di dettaglio reclamo                    *
523600040212      *---------------------------------------------------------------*
523700040218     c     SR_CONFE      BegSR
523800150423      *
523900150423      * Se sono in immissione o in gestione di una riga appena inserita
524000150423      *   re-imposto la richiesta della window W04.
524100150423     c                   If        Wmod = 'I'
524200150423     c                   clear                   $GesW04
524300160128     c                   else
524400160128     c                   clear                   $GesW04N
524500150423     c                   EndIf
524600040407      *
524700040218      * se sono in immissione e non in gestione di una riga già inserita
524800040218      * pulisco il record
524900040218     c                   If        Wmod = 'I' and WRK_recsbf = ' '
525000040218     c                   clear                   fitgd000
525100040218     c                   endif
525200150603
525300150603     c                   eval      tgdprt = v1cprt                              priorità dettaglio
525400150625      *
525500041213      * se non sono in immissione verifico se è cambiato qualcosa dalla testa \del reclamo
525600041213      *
525700160217     c                   movel     TGDorr        w0040
525800041213     c                   If        Wmod <> 'I' and (tgdtco <> v1ctco or
525900160217     c                             tgddar <> wdar   or  w0040 <> v1corr or
526000041213     c                             tgdksc <> v1cchi or tgdrsc <> v1crsc or
526100041213     c                             tgdclv <> v1cclv)
526200041213     c                   eval      mod_testa = 'S'
526300041213     c                   endif
526400151202      *
526500151202      * -?Memorizzazione  immissione R.A. con Motivo Apertura " 18"?
526600151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
526700151202      *         ?& ho modificato il motivo in " 18" (Reso)?
526800151203      *  ?=> Non sarà più possibile premere F3?
526900151202     c                   clear                   $WrtTIIDC
527000151202if  1c                   If        Wmod    = 'I'           and
527100151202     c                             V1Cmad  = c_MotAp_Reso  and
527200151202     c                             ( wrk_RecSbf = *blank   or
527300151202     c                             ( wrk_RecSbf = 'Y'     and
527400151202     c                               V1Cmad    <> TGDmad ) )
527500151202     c                   eval      $WrtTIIDC = *on
527600151203     c                   eval      $RA_MA_18 = *on
527700151202e   1c                   EndIf
527800040218      *
527900040218      * oggetto
528000040218     c                   eval      tgdtor = v1ctor                              tipo oggetto reclamo
528100040218     c                   if        v1ctor = 'S'
528200040218     c                   movel     spevid        tgdogg                         spedizione
528300040218     c                   endif
528400040218     c                   if        v1ctor = 'O'
528500040218     c                   movel     ormvid        tgdogg                         ORM
528600040218     c                   endif
528700040218     c                   if        v1ctor = 'G'
528800040218     c                   movel     v1cogg        tgdogg                         generico
528900040218     c                   endif
529000040218      * motivo apertura
529100040218     c                   eval      tgdmad = v1cmad
529200141216      *
529300151216      /free
529400151216
529500151216         // -?Richiesta Risposta (Avanzamento Fase)?
529600151216         Select;
529700160207
529800160121           // -?Utente di Sede su R.A. con Attesa di Risposta?
529900160121           //  ?- CASO IMPOSSIBILE !!! -?
530000160121           When  *in05  and  dTGDfar.§TGDatt = 'S';
530100160121             // ·?Storicizzazione Variazioni Richiesta Risposta?
530200160121             wTMO = 'A';
530300160121             clear dTGMa;
530400160121             dTGMa.§MODattO = dTGDfar.§TGDatt;
530500160121             dTGMa.§MODrisO = dTGDfar.§TGDris;
530600160121             dTGMa.§MODfilO = dTGD01.§TGDfilR;
530700160121             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
530800160121             exsr  WRT_Modifiche;
530900160121             // ·?Valorizzazione campi di FITGD?
531000160121             clear  dTGDfar.§TGDatt;
531100160121             clear  dTGDfar.§TGDris;
531200160121             clear  V1CrepR;
531300160207             V1CfilR = *zero;
531400160207
531500160207           // -?R.A. "chiusa" con Attesa Risposta = "S"?
531600160207           //                ?e Ricevuta Risposta = " "?
531700160129           When  wMod           <> 'I'     and
531800160129                 V1Ccdt         <> *blank  and
531900160129                 dTGDfar.§TGDatt = 'S'     and
532000160129                 dTGDfar.§TGDris = *blank  and
532100160201                 V1CfilR         > *zero   and
532200160201                 V1CrepR        <> *blank;
532300151216             // ·?Storicizzazione Variazioni Richiesta Risposta?
532400151216             wTMO = 'A';
532500151216             clear dTGMa;
532600151216             dTGMa.§MODattO = dTGDfar.§TGDatt;
532700151216             dTGMa.§MODrisO = dTGDfar.§TGDris;
532800151216             dTGMa.§MODfilO = dTGD01.§TGDfilR;
532900151216             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
533000151223             dTGMa.§MODattN = 'S';
533100151216             dTGMa.§MODrisN = 'S';
533200160129             dTGMa.§MODfilN = %int( V1CfilR );
533300160129             dTGMa.§MODrepN = V1CrepR;
533400151216             exsr  WRT_Modifiche;
533500151216             // ·?Valorizzazione campi di FITGD?
533600151223             dTGDfar.§TGDatt = 'S';
533700151216             dTGDfar.§TGDris = 'S';
533800151216             clear  V1Ccdt;
533900151216             if  wMod = 'I';
534000160121               //V1Cpog = W4filR;  ?(già così)?
534100160121               //V1Crpg = W4repR;  ?(già così)?
534200151216             else;
534300151223               V1Cpog = V1CfilR;
534400151223               V1Crpg = V1CrepR;
534500151216             endif;
534600160207
534700160121           // -?R.A. NON chiusa con Attesa Risposta = "S"?
534800160121           //                  ?e Ricevuta Risposta = " "?
534900160121           //  ?e variati Filiale e/o Reparto che Attende Risposta?
535000160121           When  wMod           <> 'I'     and
535100160121                 V1Ccdt          = *blank  and
535200160121                 dTGDfar.§TGDatt = 'S'     and
535300160121                 dTGDfar.§TGDris = *blank  and
535400160121                 ( V1CfilR      <> %editc( dTGD01.§TGDfilR : 'X' )  or
535500160121                   V1CrepR      <> dTGR01A.§TGRrepR );
535600160121             // ·?Storicizzazione Variazioni Richiesta Risposta?
535700160121             wTMO = 'A';
535800160121             clear dTGMa;
535900160121             dTGMa.§MODattO = dTGDfar.§TGDatt;
536000160121             dTGMa.§MODrisO = dTGDfar.§TGDris;
536100160121             dTGMa.§MODfilO = dTGD01.§TGDfilR;
536200160121             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
536300160129             if  V1CfilR > *zero  and  V1CrepR <> *blank;
536400160129               dTGMa.§MODattN = 'S';
536500160129             endif;
536600160121             dTGMa.§MODfilN = %int( V1CfilR );
536700160121             dTGMa.§MODrepN = V1CrepR;
536800160121             exsr  WRT_Modifiche;
536900160121             // ·?Valorizzazione campi di FITGD?
537000160129             if  V1CfilR > *zero  and  V1CrepR <> *blank;
537100160129               dTGDfar.§TGDatt = 'S';
537200160129             else;
537300160129               clear  dTGDfar.§TGDatt;
537400160129             endif;
537500160129             clear  dTGDfar.§TGDris;
537600160129             clear  V1Ccdt;
537700160121             //V1Cpog = V1CfilR;   ?(da lasciare com'è)?
537800160121             //V1Crpg = V1CrepR;   ?(da lasciare com'è)?
537900160207
538000160105           // -?R.A. NON chiusa con Attesa Risposta = "S"?
538100160105           //                    ?e Ricevuta Risposta = "S"?
538200160105           When  wMod           <> 'I'     and
538300160105                 V1Ccdt          = *blank  and
538400160129                 dTGDfar.§TGDatt = 'S'     and
538500160129                 dTGDfar.§TGDris = 'S'     and
538600160201                 V1CfilR         > *zero   and
538700160201                 V1CrepR        <> *blank;
538800160105             // ·?Storicizzazione Variazioni Richiesta Risposta?
538900160105             wTMO = 'A';
539000160105             clear dTGMa;
539100160105             dTGMa.§MODattO = dTGDfar.§TGDatt;
539200160105             dTGMa.§MODrisO = dTGDfar.§TGDris;
539300160105             dTGMa.§MODfilO = dTGD01.§TGDfilR;
539400160105             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
539500160105             dTGMa.§MODattN = 'S';
539600160105             dTGMa.§MODfilN = %int( V1CfilR );
539700160105             dTGMa.§MODrepN = V1CrepR;
539800160105             exsr  WRT_Modifiche;
539900160105             // ·?Valorizzazione campi di FITGD?
540000160105             dTGDfar.§TGDatt = 'S';
540100160105             clear  dTGDfar.§TGDris;
540200160105             clear  V1Ccdt;
540300160107             //V1Cpog = V1CfilR;   ?(NON in NON chiusura)?
540400160107             //V1Crpg = V1CrepR;   ?(NON in NON chiusura)?
540500160207
540600160105           // -?R.A. con Attesa Risposta variata?
540700151223           When  ( V1CfilR         > *zero   and
540800160201                   V1CrepR        <> *blank  and
540900160105                   dTGDfar.§TGDatt = *blank ) OR
541000151223                 ( V1CfilR        <= *zero   and
541100160201                   V1CrepR         = *blank  and
541200160105                   dTGDfar.§TGDatt = 'S' );
541300160104             // ·?Storicizzazione Variazioni Ricevuta Risposta?
541400151222             wTMO = 'A';
541500151222             clear dTGMa;
541600151222             dTGMa.§MODattO = dTGDfar.§TGDatt;
541700151222             dTGMa.§MODrisO = dTGDfar.§TGDris;
541800151222             dTGMa.§MODfilO = dTGD01.§TGDfilR;
541900151222             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
542000160201             if  V1CfilR > *zero  and  V1CrepR <> *blank;
542100151223               dTGMa.§MODattN = 'S';
542200151222               if  wMod = 'I';
542300151222                 dTGMa.§MODfilN = %int( W4filR );
542400151222                 dTGMa.§MODrepN = W4repR;
542500151222               else;
542600151223                 dTGMa.§MODfilN = %int( V1CfilR );
542700151223                 dTGMa.§MODrepN = V1CrepR;
542800151222               endif;
542900151222             endif;
543000151222             exsr  WRT_Modifiche;
543100151222             // ·?Valorizzazione campi di FITGD?
543200160201             if  V1CfilR > *zero  and  V1CrepR <> *blank;
543300151223               dTGDfar.§TGDatt = 'S';
543400160105             else;
543500160105               clear  dTGDfar.§TGDatt;
543600151223             endif;
543700151222             clear  dTGDfar.§TGDris;
543800160207
543900151223           // -?R.A. inserita (aperta) & cliente in attesa di Risposta?
544000160216           When  wMod     = 'I'    and  V1Ccdt  = *blank  and
544100160216                 W4tgdATT = 'S'    and
544200160216                 W4filR   > *zero  and  W4repR <> *blank;
544300151223             // ·?Storicizzazione inserimento di R.A. (aperta) con?
544400151223             //  ?Richiesta Risposta?
544500160216             wTMO = 'A';
544600160216             clear  dTGMa;
544700160216             // ·?Meglio inserire anche i dati già esistenti:?
544800160216             //  ?potrei aver ora modificato la filiale e/o il?
544900160216             //  ?reparto della R.A. appena inserita (F6) ma NON?
545000160216             //  ?ancora confermata (F4)?
545100160216             dTGMa.§MODattO = dTGDfar.§TGDatt;
545200160216             dTGMa.§MODrisO = dTGDfar.§TGDris;
545300160216             dTGMa.§MODfilO = dTGD01.§TGDfilR;
545400160216             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
545500160216             dTGMa.§MODattN = W4tgdATT;
545600160216             dTGMa.§MODfilN = %int( W4filR );
545700160216             dTGMa.§MODrepN = W4repR;
545800160216             exsr  WRT_Modifiche;
545900151223             // ·?Valorizzazione campi di FITGD?
546000151216             dTGDfar.§TGDatt = W4tgdATT;
546100160108             clear  dTGDfar.§TGDris;
546200160207
546300160207           // -?R.A. chiusa dalla filiale in Attesa Risposta?
546400160207           //  ?(con Risposta Ricevuta)?
546500160207           When  wMod           <> 'I'     and
546600160207                 V1Ccdt         <> *blank  and
546700160207                 dTGDfar.§TGDatt = 'S'     and
546800160207                 dTGDfar.§TGDris = 'S';
546900160207             clear  dTGDfar.§TGDatt;
547000160207             clear  dTGDfar.§TGDris;
547100160207             V1CfilR = *zero;
547200160207             clear  V1CrepR;
547300160207
547400151223           // -?Cliente NON in attesa di Risposta?
547500150108           When  dTGDfar.§TGDris = *blank  and
547600160201                 V1CfilR        <= *zero   and
547700160201                 V1CrepR         = *blank;
547800151216             clear  dTGDfar.§TGDatt;
547900160108             clear  dTGDfar.§TGDris;
548000160207             V1CfilR = *zero;
548100160207             clear  V1CrepR;
548200160207
548300151216         EndSl;
548400151216
548500151216         TGDfar = dTGDfar;
548600151216
548700151216      /end-free
548800151216      *
548900040218      * chiusura
549000040406      * se sono in manutenzione e modifico la causale di chiusura scrivo delle note
549100150622      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
549200150622      ***?già impostato.?
549300040406     c                   if        tgdcdt <> *blank and v1ccdt <> tgdcdt
549400040406     c                             and Wmod <> 'I'
549500150318     c                   eval      wtmo = 'C'
549600150318     c                   clear                   dTGMc
549700150318     c                   eval      §MODcdto = TGDcdt
549800150318     c                   eval      §MODcdtn = V1Ccdt
549900150318     c                   exsr      WRT_modifiche
550000040406     c                   endif
550100040406      *
550200040218      * se è la prima volta che chiudo imposto anche data,ora,p.o. e profilo chiusura
550300040406      *
550400151214if  1c                   if        tgdcdt = *blank and v1ccdt <> *blank
550500151214if  2c                   if        Wmod = 'I'
550600040304      * imposto ora e data inserimento chiamata  se sono in immissione
550700040304     c                   eval      tgddcd = w1cdar                              data chiusura
550800150324     c                   eval      tgdocd = %dec(%time())                       ora chiusura
550900040304
551000040304     c                   else
551100150603      * se sono in manutenzione e modifico la causale di chiusura
551200040304      * recupero ora e data chiusura se sono in manutenzione
551300150625      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
551400150625      ***?già impostato.?
551500040304     c                   time                    w0140
551600040304
551700040304     c                   clear                   WLBdat
551800040304     c                   move      w0140         g02dat
551900040304     c                   call      'XSRDA8'
552000040304     c                   parm                    WLBdat
552100040304     c                   eval      tgddcd = g02inv                              data chiusura
552200040304     c                   movel     w0140         tgdocd                         ora chiusura
552300151214e   2c                   endif
552400040304
552500040617     c                   eval      tgdpct = v1cpot                              p.o. chiusura
552600040218     c                   eval      tgdutc = KNMUS                               utente chiusura
552700040304
552800151214e   1c                   endif
552900151216      *
553000040223      * se codice chiusura è blanks e prima non lo era pulisco i campi del file
553100040223     c                   if        tgdcdt <> *blank and v1ccdt = *blank
553200040223     c                   clear                   tgddcd                         data chiusura
553300040223     c                   clear                   tgdocd                         ora chiusura
553400040223     c                   clear                   tgdpct                         p.o. chiusura
553500040223     c                   clear                   tgdutc                         utente chiusura
553600040223     c                   endif
553700040223      *
553800040223     c                   eval      tgdcdt = v1ccdt                              codice chiusura
553900040218
554000150316      * se utente a video non è l'utente del job
554100150316      * priorità a '1'
554200150316      * imposto priorità a '0'
554300150313     c                   IF        TGDprt = '1' and
554400150316     c                             V1Crpg <> KNMUS
554500150313     c                   eval      TGDprt = '0'
554600150313     c                   ENDIF
554700150316      * se sono in manutenzione e variato utente scrivo file variazioni
554800160111      *    (o anche se variata filiale gestione)
554900160111     c*//                if        TGDrpg <> V1Crpg and wmod <> 'I'
555000160111     c                   if          wMOD   <> 'I'     AND
555100160111     c                             ( TGDrpg <> V1Crpg  or
555200160111     c                               TGDpog <> %int(V1Cpog) )
555300150316     c                   eval      wtmo = 'U'
555400150316     c                   clear                   dTGMu
555500150316     c                   eval      §MODuteo = TGDrpg
555600150316     c                   eval      §MODpogo = TGDpog
555700150316     c                   eval      §MODuten = V1Crpg
555800160121     c                   eval      §MODpogn = %dec(V1Cpog:3:0)
555900150316     c                   exsr      WRT_modifiche
556000040618     c                   endif
556100040618      *
556200150423     c                   eval      tgdpog = %dec(v1cpog:3:0)                    filiale gestione
556300150423     c                   eval      tgdrpg = v1crpg                              utente  gestione
556400160119     c                   if        V1Ccdt =  *blanks  and
556500160119     c                             V1Cpog <> c_Sede   and
556600160119     c                             ( $Reparto         or
556700160201     c                             ( V1CfilR  > *zero and
556800160201     c                               V1CrepR <> *blank ) )
556900150422     c                   eval      tgdRPR = V1Crpg                              reparto gestione
557000150422     c                   endif
557100040618     c                   eval      tgdrsp = v1crsp                              responsabilità
557200040618      *
557300040407      * P.O. responsabile
557400040407      * se sono in manutenzione e modifico il p.o.responsabile scrivo delle note
557500040407      *
557600040407     c                   if        tgdpor <> *zeros and W_por  <> tgdpor
557700040407     c                             and Wmod <> 'I'
557800150318     c                   eval      wtmo = 'R'
557900150318     c                   clear                   dTGMr
558000150318     c                   eval      §MODporo = TGDpor
558100150318     c                   eval      §MODporn = W_por
558200150318     c                   exsr      WRT_modifiche
558300040407     c                   endif
558400040407      *
558500040218     c                   eval      tgdpor = W_por                               P.O. responsabile
558600040213      *
558700151214if  1c                   If        wmod = 'I'
558800040213     c                   eval      tgddar = wdar                                dta ricez. comunicaz
558900040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
559000040217     c                   eval      tgdtco = v1ctco                              tipo comunicazione
559100040213      *
559200040213     c                   eval      tgdtch = v1ctch                              tipo chiamamte
559300040213     c                   eval      tgdclv = v1cclv                              categoria cliente
559400040213     c                   eval      tgdrsc = v1crsc                              ragione sociale
559500041209     c                   eval      tgdksc = v1cchi                              codice cliente
559600120919      * se il motivo di apertura lo richiede NON memorizzo il KSC
559700120919     c                   IF        §MARksc = 'N'
559800120919     c                   clear                   TGDksc
559900120919     c                   ENDIF
560000040218     c                   eval      tgdpot = v1cpot                              p.o. immette recla
560100040421     c                   eval      tgdant = v1cant                              Anno   reclamo
560200040218     c                   eval      tgdnut = v1cnut                              Numero reclamo
560300040218     c                   eval      tgdpdt = v1cpdt                              progressivo dettagli
560400151214e   1c                   endif
560500041213      * solo in immissione valorizzo i seguenti campi
560600040407     c                   if        wmod = 'I' and wrk_recsbf = ' '
560700040218     c                   eval      tgddac = w1cdar                              data inserimento
560800150324     c                   eval      tgdorc = %dec(%time())                       ora inserimento
560900040218     c                   eval      tgdpru = KNMUS                               utente inserimento
561000040407     c                   endif
561100150326      /free
561200151215         // -?Flag Operativi?
561300160107         clear  dTGD01.§TGDfilR;
561400160207         if  TGDcdt = *blank  and  dTGDfar.§TGDatt = 'S'
561500160207                              and  V1CfilR > *zero;
561600160129           dTGD01.§TGDfilR = %int( V1CfilR );
561700151215         endif;
561800160107         TGDflo = dTGD01;
561900151215
562000150326       //?Storicizzo Variazioni Stato
562100150326         IF  W_prt <> TGDprt;
562200150326           wtmo = 'S';
562300150326           clear dTGMs;
562400150326           §MODprto = W_prt;
562500150326           §MODprtn = TGDprt;
562600150326           exsr WRT_modifiche;
562700150326         ENDIF;
562800150326      /end-free
562900040213      *
563000040309
563100040309      * se sono in immissione e non in gestione di una riga già inserita
563200040309      * valorizzo i seguenti campi
563300040309
563400040309     c                   If        Wmod = 'I' and WRK_recsbf = ' '
563500040218     c                   write     fitgd000
563600040218     c                   FEOD      fitgd01l
563700040218     c                   add       1             rec_wrt
563800041210      * aggiungo 1 al numeratore
563900041210     c                   add       1             sv_pdt
564000040218      *
564100040218     c                   else
564200040218
564300040218     c                   update    fitgd000
564400040218      *
564500040218     c                   endif
564600151201      *
564700151201      * -?Immissione R.A. con Motivo Apertura " 18":?
564800151201      *  ?=> Generazione automatica della "Disposizione di Reso"?
564900151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
565000151202      *         ?& ho modificato il motivo in " 18" (Reso)?
565100151202     c                   If        $WrtTIIDC = *on
565200151201     c                   exsr      sr_Wrt_TIIDC
565300151202e   1c                   EndIf
565400141216      *
565500141216      * -?Immissione R.A. con Motivo Apertura " 60":?
565600141216      *  ?SE NON sono in modifica di una spedizione nella stessa telefonata?
565700141216if  1c                   If        Wmod       = 'I'      and
565800141216     c                             V1Cmad     = c_VPINC  and
565900141216     c                             WRK_recsbf = *blank
566000141216      *
566100141216      * ·?=> Registrazione automatica dell'esito nelle Note?
566200141216     c                   eval      not_PINcode = 'S'
566300141216     c                   exsr      WRT_note
566400141216      * ·?=> Registrazione automatica dell'esito nei Riferimenti PINcode?
566500141216      *?· Impostazione dati della spedizione nella ds per TGRRIF?
566600141216     c                   clear                   dTGR01
566700141216     c                   if        oPINfxx = *blank  and  oPINerr = *blank
566800141216     c                   eval      dTGR01.§TGResito = 'P'
566900141216     c                   else
567000141216     c                   eval      dTGR01.§TGResito = 'N'
567100141216     c                   endif
567200141216     c                   eval      dTGR01.§TGRfgs   = ARBifp
567300141216     c                   eval      dTGR01.§TGRnfv   = ARBndc
567400141216     c                   eval      dTGR01.§TGRdfv   = ARBddc
567500141216     c                   eval      dTGR01.§TGRpdr   = ARBpdc
567600141216      *?· Impostazione dati nel file FITGR00F?
567700141216     c                   clear                   FITGR000
567800141216     c                   eval      TGRpot = V1Cpot                              Fil. immette tel.
567900141216     c                   eval      TGRant = V1Cant                              Anno   telefonata
568000141216     c                   eval      TGRnut = V1Cnut                              Numero telefonata
568100141216     c                   eval      TGRpdt = V1Cpdt                              Progr. Dettaglio
568200141216     c                   eval      TGRtri = 'D'                                 Tipo Riferimento
568300141216     c                   eval      TGRrif = dTGR01
568400141216     c                   write     FITGR000
568500141216     c                   feod      FITGR01L
568600141216      *
568700141216e   1c                   EndIf
568800040213
568900040213      * verifico se esistono per questa riga che confermo dei record di riferimenti
569000040213      * ed evantualmente li deleto e poi riscrivo
569100040213
569200040421     c     ktgran        setll     fitgr01l
569300040213      *
569400040213     c                   do        *hival
569500040421     c     ktgran        reade     fitgr01l
569600040213      *
569700040219     c                   if        not %eof(fitgr01l)
569800040213     c                   delete    fitgr01l
569900040213     c                   else
570000040213     c                   leave
570100040213     c                   endif
570200040213      *
570300040213     c                   enddo
570400151214      *
570500040213      * se esiste referente
570600040213     c                   if        v1refe <> *blank
570700040213     c                   clear                   fitgr000
570800040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
570900040421     c                   eval      tgrant = v1cant                              Numero reclamo
571000040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
571100040213     c                   eval      tgrtri = 'R'                                 tipo  riferimento
571200040213     c                   movel     v1refe        tgrrif
571300040213     c                   write     fitgr000
571400040219     c                   feod      fitgr01l
571500040213     c                   endif
571600040213      * se esiste indirizzo e-mail
571700040213     c                   if        v1mail <> *blank
571800040213     c                   clear                   fitgr000
571900040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
572000040421     c                   eval      tgrant = v1cant                              Numero reclamo
572100040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
572200040213     c                   eval      tgrtri = 'M'                                 tipo  riferimento
572300040213     c                   movel     v1mail        tgrrif
572400040213     c                   write     fitgr000
572500040219     c                   feod      fitgr01l
572600040213     c                   endif
572700040213      * se esiste numero telefono e/o numero fax
572800040213     c                   if        v1ntel <> *blank or v1nfax <> *blank
572900040213     c                   clear                   fitgr000
573000040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
573100040421     c                   eval      tgrant = v1cant                              Numero reclamo
573200040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
573300040213     c                   eval      tgrtri = 'N'                                 tipo  riferimento
573400040213     c                   movel     v1ntel        tgrrif
573500040213     c                   move      v1nfax        tgrrif
573600040213     c                   write     fitgr000
573700040219     c                   feod      fitgr01l
573800040213     c                   endif
573900151215      * -?se Richiesta Risposta?
574000151215     c                   eval      TGRtri = 'A'
574100151215     c     keyFITGR01    setll     FITGR000
574200151215     c                   if        %equal(FITGR01L)
574300151215     c     keyFITGR01    delete    FITGR000
574400151215     c                   endif
574500151215     c                   clear                   wTGRpdt
574600160204     c                   If        V1CfilR  > *zero  and
574700160204     c                             V1CrepR <> *blank
574800160129     c*//?NON serve:?    clear                   dTGR01A
574900160129     c                   eval      dTGR01A.§TGRrepR = V1CrepR
575000151214     c                   clear                   FITGR000
575100151214     c                   eval      TGRpot = V1Cpot                              Fil. immiss. recl.
575200151214     c                   eval      TGRant = V1Cant                              Anno   reclamo
575300151214     c                   eval      TGRnut = V1Cnut                              Numero reclamo
575400151215     c                   eval      TGRpdt = TGDpdt                              Progr. reclamo
575500151214     c                   eval      TGRtri = 'A'                                 Tipo  riferimento
575600151214     c                   eval      TGRrif = dTGR01A
575700151214     c                   write     FITGR000
575800151214     c                   feod      FITGR01L
575900151214     c                   EndIf
576000040217
576100070413      * confermo le note se sono in immissione e non ho scritto nulla precedentemente
576200040217
576300160512     c****               If        Wmod = 'I' and $bloccof3 = *off
576400160512     c                   If        Wmod = 'I' and $Note_scritte= *off
576500070413     c                   clear                   FIDNB0DS
576600040421     c                   z-add     v1cant        iB0ant
576700040217     c                   z-add     v1cnut        iB0nut
576800040217     c                   z-add     v1cpdt        iB0pdt
576900040217     c                   movel     'GEN'         iB0tIN
577000141217     c                   if        WRK_RecSbf = *blank
577100141216     c                   z-add     999           iB0pgn
577200141217     c                   endif
577300040217     c                   movel     v1not1        db0no1
577400040217     c                   movel     v1not2        db0no2
577500040217     c                   movel     v1not3        db0no3
577600040217     c                   movel     v1not4        db0no4
577700040617     c                   eval      ib0pot =v1cpot
577800041013     c                   eval      db0pon =Dutpou
577900040617     c                   eval      db0utn =knmus
578000040218      * se non è mai stato chiamato imposto 'W'
578100040218     c                   if        wfidnb0r = 'Y'
578200040217     c                   movel     'C'           iB0flm
578300040218     c                   else
578400040218     c                   movel     'W'           iB0flm
578500040220
578600040218     c                   endif
578700040217      *
578800040217     C                   CALL      'FIDNB0R'
578900040217     C                   PARM                    kpjba
579000040217     C                   PARM                    FIDNB0DS
579100040217      *
579200040305     c                   endif
579300040305
579400070413      * modalità manutenzione
579500040305
579600070424    1c                   If         Wmod = 'G' or
579700070424     c                             (Wmod = 'I' and $bloccof3 = *on)
579800040305      * nel caso in cui sia stato richiamato il pgm fidnb0r per gestire le note
579900040305      * non lo richiamo + per confermarle
580000070413    2c                   if        wfidnb0r <> 'Y'
580100040305     C                   clear                   FIDNB0DS
580200040421     c                   z-add     v1cant        iB0ant
580300040305     c                   z-add     v1cnut        iB0nut
580400040305     c                   z-add     v1cpdt        iB0pdt
580500040305     c                   movel     'GEN'         iB0tIN
580600040305     c                   movel     'W'           iB0flm
580700040305     c                   eval      ib0pot =v1cpot
580800041013     c                   eval      db0pon =Dutpou
580900040305     c                   eval      db0utn =knmus
581000040305      * valorizzo il progressivo riga  scrivo la prima riga
581100040305    3c                   if        *in51 = *off and V1not1 <> *blanks
581200040305     c                   z-add     1             Ib0pgn
581300040305     c                   movel     v1not1        db0no1
581400040305      *
581500040305     C                   CALL      'FIDNB0R'
581600040305     C                   PARM                    kpjba
581700040305     C                   PARM                    FIDNB0DS
581800040305    3c                   endif
581900040305
582000040305      * valorizzo il progressivo riga  scrivo la seconda riga
582100040305    3c                   if        *in52 = *off and V1not2 <> *blanks
582200040305     c                   z-add     2             Ib0pgn
582300040305     c                   movel     v1not2        db0no1
582400040305      *
582500040305     C                   CALL      'FIDNB0R'
582600040305     C                   PARM                    kpjba
582700040305     C                   PARM                    FIDNB0DS
582800040305    3c                   endif
582900040305      * valorizzo il progressivo riga  scrivo la terza   riga
583000040305    3c                   if        *in53 = *off and V1not3 <> *blanks
583100040305     c                   z-add     3             Ib0pgn
583200131017     c                   movel     v1not3        db0no1
583300040305      *
583400040305     C                   CALL      'FIDNB0R'
583500040305     C                   PARM                    kpjba
583600040305     C                   PARM                    FIDNB0DS
583700040305    3c                   endif
583800040305      * valorizzo il progressivo riga  scrivo la quarta  riga
583900040305    3c                   if        *in54 = *off and V1not4 <> *blanks
584000040305     c                   z-add     4             Ib0pgn
584100040305     c                   movel     v1not4        db0no1
584200040305      *
584300040305     C                   CALL      'FIDNB0R'
584400040305     C                   PARM                    kpjba
584500040305     C                   PARM                    FIDNB0DS
584600040305    3c                   endif
584700040305
584800040305    2c                   endif
584900040305
585000040305    1c                   endif
585100070402
585200140205      * se impostata data consegna   o   se previste note LDV obbligatorie
585300131119      * => devo scrivere alcune note R.A. e note ldv
585400070402
585500140205    1c                   if        not_ldv = 'S'  or  §MARnotLdV = 'S'
585600070403      * se devo inserire delle note indica che ho fatto delle modifiche
585700070403      * alla data consegna e quindi impedisco l'uso del f3
585800070403     c                   eval      $bloccof3  =  *on
585900070404      * recupero le note da passare al pgm note ldv
586000070404     c                   clear                   nob
586100070404     c                   clear                   x
586200070402      * recupero le note R.A. dal file e le passo al PGM note LDV
586300070402     c     ktgn          setll     fitgn01l
586400070402     c                   do        *hival
586500070402     c     ktgn          reade     fitgn01l
586600070402
586700070402     c                   if        %eof(fitgn01l)
586800070402     c                   leave
586900070402     c                   endif
587000070402      * passo solo le prime 8(qui non ci sono ancora le modifiche registrate)
587100070402     c                   if        x < 8
587200070402     c                   add       1             x
587300070402     c                   movea     tgnnot        nob(x)
587400070402     c                   else
587500070402     c                   leave
587600070402     c                   endif
587700070402
587800070402     c                   enddo
587900070402
588000131119      * richiamo il programma note LdV in  immissione
588100131206     C                   clear                   Fnlrn6DS
588200131206     c                   eval      in6mod  = 'I'
588300131206     c                   eval      in6aas  = v1anns
588400131206     c                   eval      in6lnp  = v1lnps
588500131206     c                   eval      in6nrs  = v1nrss
588600131206     c                   eval      in6nsp  = v1nsps
588700070402      * note
588800131206     c                   movel     nob(1)        in6not1
588900131206     c                   move      nob(2)        in6not1
589000131206     c                   movel     nob(3)        in6not2
589100131206     c                   move      nob(4)        in6not2
589200131206     c                   movel     nob(5)        in6not3
589300131206     c                   move      nob(6)        in6not3
589400131206     c                   movel     nob(7)        in6not4
589500131206     c                   move      nob(8)        in6not4
589600070402     c
589700131206     c                   eval      in6pgm  = 'FIDNA1R'
589800131206      * se sono una spedizione in consegna passo i dati della distinta del file ARB
589900131206     c                   If        $cons  = *on
590000160104     c     KSPED         chain     Fnarb000
590100131206     c                   If        %found(Fnarb01l)
590200131206     c                   eval      in6fgs = arbifp
590300131206     c                   eval      in6nfv = arbndc
590400131206     c                   eval      in6dfv = arbddc
590500131206     c                   eval      in6pdr = arbpdc
590600131206     c                   eval      in6cons = 'S'
590700131206     c                   Endif
590800131206     c                   Endif
590900070402
591000131206     c                   call      'FNLRN6R'
591100070402     c                   parm                    kpjba
591200131206     c                   parm                    FNLRN6DS
591300070402
591400070402     c                   clear                   not_ldv
591500070412    1c                   endif
591600070402
591700070402      * se devo registrare delle note di modifica data consegna
591800070402      * richiesta scrivo delle note mie nelle note R.A.
591900070402      * Le Faccio uguali a quelle della Rita ?????
592000070402
592100070402      * Decodifico tipo consegna richiesta
592200070402
592300070412    1c                   If        v1cdcr > 0
592400070402     c                   select
592500070402
592600070402     c                   when      v1ctcr = 'P'
592700070404     c                   eval      W_dtcr   ='PRIMA DEL'
592800070402     c                   when      v1ctcr = 'D'
592900070404     c                   eval      W_dtcr   ='DOPO  IL '
593000070402     c                   other
593100070404     c                   eval      W_dtcr   ='      IL '
593200070402
593300070402     c                   endsl
593400070402
593500070402      * creo le note R.A.
593600070402     c                   clear                   nob
593700070402     c                   clear                   x
593800070402     c                   eval      nob(1)='Cons.Rich:'
593900070402     c                   eval      nob(1)=%trim(nob(1))+%trim(w_dtcr)
594000070402     c                   eval      nob(1)=%trim(nob(1))+' '+
594100070402     c                             %editw(v1cdcr:'  /  /    ')
594200141031    2c                   if        v1cocr>0
594300141031     c                   eval      nob(1)=%trim(nob(1))+' ore'
594400141031    3c                   if        v1ctcr=*blanks
594500141031     c                   eval      nob(1)=%trim(nob(1))+' '+
594600141031     c                             %editw(v1cocr:'  :  ')
594700141031     c                   else
594800141031     c                   eval      nob(2)=
594900141031     c                             %editw(v1cocr:'  :  ')
595000141031    3c                   endif
595100141031    2c                   endif
595200070402
595300070402     c                   clear                   not_dcr
595400070402
595500070402      * se valorizzate anche le note DCR con il msg del 48 scrivo
595600070402      * il msg in note R.A.
595700070412    2c                   if        note_dcr <> *blanks
595800141031     c                   if        v1ctcr = *blanks
595900070402     c                   movea     note_dcr      nob(2)
596000141031     c                   else
596100141031     c                   eval      %subst(nob(2):7:29)= %subst(note_dcr:1:29)
596200141031     c                   eval      nob(3) = %subst(note_dcr:30:35)
596300141031     c                   eval      nob(4) = %subst(note_dcr:65:14)
596400141031     c                   endif
596500070402      * visto che il messaggio è lungo 78 dovrebbe riempire 3 note
596600070402     c                   clear                   note_dcr
596700070412    2c                   endif
596800070402      * richiamo in scrittura il pgm delle note
596900070402     c                   clear                   FIDNB0DS
597000070402     c                   eval      db0no1 = nob(1)
597100070402     c                   eval      db0no2 = nob(2)
597200070402     c                   eval      db0no3 = nob(3)
597300070402     c                   eval      db0no4 = nob(4)
597400070402
597500070402     c                   z-add     v1cant        iB0ant
597600070402     c                   z-add     v1cnut        iB0nut
597700070402     c                   z-add     v1cpdt        iB0pdt
597800070404     c                   z-add     999           ib0pgn
597900070402     c                   movel     'GEN'         iB0tIN
598000070402     c                   eval      ib0pot =v1cpot
598100070402     c                   eval      db0pon =Dutpou
598200070402     c                   eval      db0utn =knmus
598300070402      * imposto 'W'
598400070402     c                   movel     'W'           iB0flm
598500070402      *
598600070402     c                   call      'FIDNB0R'
598700070402     c                   parm                    kpjba
598800070402     c                   parm                    fidnb0ds
598900070402
599000070412    1c                   endif
599100140506      *
599200070403      * se c'è stata almeno una riga che ha fatto modificare una data consegna richiesta
599300070403      * impedisco l'uso del tasto funzionale F3
599400070403     c                   if        $bloccof3  =  *on
599500070403     c                   seton                                        63
599600070403     c                   endif
599700140506      *
599800140506      * se è stata inserita almeno una R.A. con Motivo Apertura " 17"
599900151203      *                                   o con Motivo Apertura " 18"
600000141210      *                                   o con Motivo Apertura " 60"
600100140506      * impedisco l'uso del tasto funzionale F3
600200141210     c                   if        $RA_MA_17  =  *on  or
600300151203     c                             $RA_MA_18  =  *on  or
600400141210     c                             $RA_MA_60  =  *on
600500140506     c                   seton                                        63
600600140506     c                   endif
600700170505      * richiamo in scrittura il pgm delle note in caso di immissione disposizioni
600800170505      * di giacenza
600900170505     c                   if        note_gia <> *blank
601000170508     c                   clear                   nob
601100170505     c                   movea     note_gia      nob(1)
601200170505     c                   clear                   FIDNB0DS
601300170505     c                   eval      db0no1 = nob(1)
601400170505     c                   eval      db0no2 = nob(2)
601500170505     c                   eval      db0no3 = nob(3)
601600170505     c                   eval      db0no4 = nob(4)
601700170505
601800170505     c                   z-add     v1cant        iB0ant
601900170505     c                   z-add     v1cnut        iB0nut
602000170505     c                   z-add     v1cpdt        iB0pdt
602100170505     c                   z-add     999           ib0pgn
602200170505     c                   movel     'GEN'         iB0tIN
602300170505     c                   eval      ib0pot =v1cpot
602400170505     c                   eval      db0pon =Dutpou
602500170505     c                   eval      db0utn =knmus
602600170505      * imposto 'W'
602700170505     c                   movel     'W'           iB0flm
602800170505      *
602900170505     c                   call      'FIDNB0R'
603000170505     c                   parm                    kpjba
603100170505     c                   parm                    fidnb0ds
603200170505     c                   clear                   note_gia
603300170505
603400170505     c                   endif
603500170505
603600040213      *
603700070402
603800040213     c                   ENDSR
603900151201      /free
604000151201
604100151201       //--------------------------------------------------------------
604200151201       //?Scrittura record in TIIDC00F per Reso/Dirottamento.          ?
604300151201       //--------------------------------------------------------------
604400151201       BEGSR  sr_Wrt_TIIDC;
604500151201
604600151201           if  Not %open(TIIDC02L);
604700151201             open  TIIDC02L;
604800151201           endif;
604900151201
605000151201           // -?Reperimento eventuale capofila £6 della FGS?
605100151201           clear  FNLV55ds;
605200151201           d55tpt = '6';
605300151201           if  $FNARB = *on;
605400151201             d55lin = ARBlna;
605500151201           else;
605600151201             d55lin = BLPlna;
605700151201           endif;
605800151201           d55drf = %dec( %date() );
605900151201           FNLV55R ( FNLV55ds );
606000151202
606100151202           // -?Reperimento progressivo richiesta per la "Disposizione di Reso"?
606200151202           exsr  sr_Rep_IDCprg;
606300151201
606400151201           // -?Impostazione campi di TIIDC00F?
606500151201           clear  TIIDC000;
606600151201
606700151201if  2      If  $Ok_IDCprg;
606800151201             IDCprg = k_IDCprg;
606900151201           endif;
607000151201           IDCinsData = %int( %subst( %char( %dec( %timestamp() ) )
607100151201                                      : 1 : 14 ) );
607200151201           IDCmodAcc  = 'B';
607300151201           if  V1Ctch = 'D';
607400151201             IDCtipoCli = V1Ctch;
607500151201           else;
607600151201             IDCtipoCli = 'M';
607700151201           endif;
607800151201           if  $FNARB = *on;
607900151201             IDCwaas  = ARBaas;
608000151201             IDCwlnp  = ARBlnp;
608100151201             IDCwnrs  = ARBnrs;
608200151201             IDCwnsp  = ARBnsp;
608300151201             IDCaas   = ARBaas;
608400151201             IDClnp   = ARBlnp;
608500151201             IDCnrs   = ARBnrs;
608600151201             IDCnsp   = ARBnsp;
608700151201             //IDCfgs   = ARBlna;     ?(vedi capofile £6)?
608800151201           else;
608900151201             IDCwaas  = BLPaas;
609000151201             IDCwlnp  = BLPlnp;
609100151201             IDCwnrs  = BLPnrs;
609200151201             IDCwnsp  = BLPnsp;
609300151201             IDCaas   = BLPaas;
609400151201             IDClnp   = BLPlnp;
609500151201             IDCnrs   = BLPnrs;
609600151201             IDCnsp   = BLPnsp;
609700151201             //IDCfgs   = BLPlna;     ?(vedi capofile £6)?
609800151201           endif;
609900151201           IDCfgs     = d55tfa;
610000151201           IDCtipoDis = '5';
610100151201           IDCnewLna  = *zero;
610200151201           IDCfla     = 'R';
610300151201           IDCelaFlg  = 'S';
610400151201           IDCelaUte  = kNmUs;
610500151201
610600151201           // -?Scrittura record in TIIDC00F?
610700151201           //______________
610800151201           write  TIIDC000;
610900151201           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
611000151201
611100151201e   1  ENDSR;
611200151201
611300151201       //--------------------------------------------------------------
611400151201       //?Reperimento progressivo per disposizione di Reso/Dirottamento?
611500151201       //--------------------------------------------------------------
611600151201       BEGSR  sr_Rep_IDCprg;
611700151201
611800151201         clear  $Ok_IDCprg;
611900151201         clear  keyTIIDC02;
612000151201
612100151201         clear  TRUL33ds;
612200170113         //*·I33tla = 'L';
612300151201         clear  I33ope;
612400151217         I33cnu = 651;
612500151201         I33num = 1;
612600151201
612700151201         DoW  Not  $Ok_IDCprg;
612800151201
612900151201           kpjbu = TRUL33ds;
613000151201           TRUL33R ( kpjba );
613100151201           TRUL33ds = kpjbu;
613200151201
613300151201           if  O33err <> *zeros;
613400151201             leavesr;
613500151201           endif;
613600151201
613700151201           w0090  = O33nri;
613800151217           // -?Così (???) NON viene estratto l'anno di 2 cifre !!!?
613900151217           //k_IDCprg = %int(
614000151217           //             %subst( %editc( %subdt( %date() : *years ) : 'X' )
614100151217           //               : 3 : 2 ) + %editc( w0090 : 'X' ) );
614200151217           w0040  = %subdt( %date() : *years );
614300151217           k_IDCprg = ( %int( %subst( %editc( w0040 : 'X' ) : 3 : 2 ) )
614400151217                        * 1000000000 ) + w0090;
614500151201           setll  %kds( keyTIIDC02 )  TIIDC000;
614600151201
614700151201           // -?Se numero già presente nel file TIIDC ne cerco un altro?
614800151201           if  Not %equal(TIIDC02L);
614900151201             $Ok_IDCprg = *on;
615000151201           endif;
615100151201
615200151201         EndDo;
615300151201
615400151201       ENDSR;
615500151201
615600151201      /end-free
615700040213      *
615800040213      *---------------------------------------------------------------*
615900040213      * Routine caricamento record nel subfile                        *
616000040213      *---------------------------------------------------------------*
616100040213     c     SR_CARSBF     BegSR
616200040213
616300040216      * caricamento dettaglio reclami
616400040217     c                   z-add     *zeros        s01nrr
616500040216
616600040421     c     ktgdan        setll     Fitgd01l
616700040216      *
616800040216     c                   Do        *hival
616900040422     c     ktgdan        reade(N)  fitgd01l
617000040216
617100040216     c                   if        %eof(fitgd01l)
617200040216     c                   leave
617300040216     c                   endif
617400040216
617500040216     c                   add       1             s01nrr
617600040218
617700040218     C
617800040218      * numero riga
617900040217     c                   eval      vs1pdt = tgdpdt
618000040216
618100040216      * - OGGETTO
618200040216
618300040220     c                   clear                   vs1ogg
618400040217
618500040216      * ---------- SPEDIZIONE -----------
618600040216
618700040217     c                   if        tgdtor = 'S'
618800040218      *
618900040218     c                   movel     tgdogg        WRKspe
619000040219     c                   eval      vs1ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
619100040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
619200040219     c                             ' ' + WRKann
619300040218      *
619400040216     c                   endif
619500040216
619600040216      * ----------    ORM     -----------
619700040216
619800040217     c                   if        tgdtor = 'O'
619900040218     c                   movel     tgdogg        WRKorm
620000040218      *
620100040218     c                   eval      vs1ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
620200040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
620300040218     c                             '/' + %trim(WRKnrv)
620400040218      *
620500040216     c                   endif
620600040216
620700040216      * ----------  GENERICO  -----------
620800040216
620900040217     c                   if        tgdtor = 'G'
621000040218     c                   EVAL      VS1ogg = TGDogg
621100040216     c                   endif
621200040218
621300040218      * MOTIVO RECLAMO
621400040218
621500040218     c                   eval      vs1mad = tgdmad
621600040218      * Controllo
621700040218     c                   clear                   tibs02ds
621800040218     C                   MOVEL     'C'           t02mod
621900040218     C                   MOVEL     knsif         t02sif
622000040218     C                   MOVEL     'MAR'         t02cod
622100040218     C                   MOVEL     vs1mad        t02ke1
622200040218      *
622300040218     C                   CALL      'TIBS02R'
622400040218     C                   PARM                    KPJBA
622500040218     C                   PARM                    TIBS02DS
622600040218      *
622700040218    4C                   IF        t02err = *BLANKS
622800040218     C                   movel     T02uni        DMAR
622900040218     C                   movel     §MARdesc      vs1dmr
623000040218     c                   endif
623100141215      *
623200141215      * -?Se il motivo di apertura NON può essere usato: proteggo campo opz.?
623300141215     c                   eval      *in83 =((*in74 = *off  and  §MARutAp = 'N')
623400141215     c                                      or
623500141215     c                                     (wMod <> 'I'   and  §MARutUt = 'N'))
623600040218
623700040218      * CHIUSURA RECLAMO
623800040218
623900040218     c                   eval      vs1cdt = tgdcdt
624000040218
624100040218      * DATA CHIUSURA
624200040218
624300040220     c                   clear                   vs1dcr
624400040220
624500040218     C                   If        tgddcd > *zeros
624600040218     c                   z-add     tgddcd        data8
624700040218     c                   move      anno2         vs1dcr
624800040218     c                   move      mese          mmgg              4 0
624900040218     c                   movel     mmgg          vs1dcr
625000040218     c                   movel     giorno        vs1dcr
625100040218     c                   endif
625200040218
625300040218     c                   write     fia1s01
625400141215     c                   setoff                                       81  83
625500040218
625600040218     c                   enddo
625700040218      *
625800040218      * verifico se inserite righe ed accendo indicatori di emissione
625900040218      *
626000040218     c                   if        s01nrr > 0
626100040218     c                   seton                                        21
626200040218     c                   endif
626300040218
626400040218     c                   seton                                        20
626500040218
626600040213     c                   ENDSR
626700041209      *
626800041209      *---------------------------------------------------------------*
626900041209      * Routine recupero codice chiamante                             *
627000041209      *---------------------------------------------------------------*
627100041209     c     SR_RECCHI     BegSR
627200041209      *
627300041209     c                   clear                   Wrk_cchi
627400041209     c                   clear                   Wrk_cchiA
627500041209     c                   clear                   Wrk_cchiB
627600041209     c                   clear                   Wrk_cchiC
627700041209      *
627800041209      * rileggo il dettaglio
627900041209      *
628000041209
628100041209     c     ktgdan        setll     Fitgd01l
628200041209      *
628300041209     c                   Do        *hival
628400041209     c     ktgdan        reade(N)  fitgd01l
628500041209
628600041209     c                   if        %eof(fitgd01l)
628700041209     c                   leave
628800041209     c                   endif
628900041209
629000041209      * ---------- SPEDIZIONE -----------
629100041209
629200041209    2c                   if        tgdtor = 'S'
629300041209      *
629400041209     c                   movel     tgdogg        Wspevid
629500041230      * se non sono in sede
629600041230     c                   If        not *in05
629700041209      * Aggancio bolle arrivi
629800160104     c     KSPEDw        chain     Fnarb000
629900041209    3c                   If        %found(Fnarb01l)
630000041209      * mittente se valorizzato
630100041209     c                   move      arbccm        w0040
630200041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
630300041210     c                             w0040 <> 9999
630400041209     c                   z-add     arbccm        Wrk_cchiA
630500041209      * se trovato codice mittente prendo quello e scappo
630600041209     c                   leave
630700041209    4c                   endif
630800041209      *
630900041209     c                   z-add     arbksc        W0040
631000041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
631100041209     c                             w0040 <> 9999
631200041209      * in caso di CCM uguale a zeros valorizzo con KSC
631300041209     c                   z-add     arbksc        Wrk_cchiA
631400041209     c                   leave
631500041209    4c                   endif
631600041209      *
631700041209    3c                   endif
631800041209      * Aggancio bolle partenze se non ho trovato ancora nulla
631900160104     c     KSPEDw        chain     Fnblp000
632000041209    3c                   If        %found(Fnblp01l)
632100041209      * mittente se valorizzato
632200041210     c                   move      blpccm        w0040
632300041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
632400041210     c                             w0040 <> 9999
632500041209     c                   z-add     blpccm        Wrk_cchiA
632600041209      * se trovato codice mittente prendo quello e scappo
632700041209     c                   leave
632800041209    4c                   endif
632900041209      *
633000041209     c                   z-add     blpksc        W0040
633100041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
633200041209     c                             w0040 <> 9999
633300041209      * in caso di CCM uguale a zeros valorizzo con KSC
633400041209     c                   z-add     blpksc        Wrk_cchiA
633500041209     c                   leave
633600041209    4c                   endif
633700041209      *
633800041209    3c                   endif
633900041230      * sono in sede
634000041230     c                   else
634100041209      * Aggancio bolle di sede  se non ho trovato ancora nulla
634200041209     c     KSPEDw        chain     titas30c
634300041209    3c                   If        %found(Titas30c)
634400041209      * mittente se valorizzato
634500041209     c                   move      tasccm        w0040
634600041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
634700041210     c                             w0040 <> 9999
634800041209     c                   z-add     tasccm        Wrk_cchiA
634900041209      * se trovato codice mittente prendo quello e scappo
635000041209     c                   leave
635100041209    4c                   endif
635200041209      *
635300041209     c                   z-add     tasksc        W0040
635400041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
635500041209     c                             w0040 <> 9999
635600041209      * in caso di CCM uguale a zeros valorizzo con KSC
635700041209     c                   z-add     tasksc        Wrk_cchiA
635800041209     c                   leave
635900041209    4c                   endif
636000041209      *
636100041209    3c                   endif
636200041209      *
636300041230    3c                   endif
636400041230      *
636500041209    2c                   endif
636600041209
636700041209      * ----------    ORM     -----------
636800041209
636900041209    2c                   if        tgdtor = 'O'
637000041209     c                   movel     tgdogg        Wormvid
637100041209      *
637200160104     c     KORMw         chain     Fnorm000
637300041209    3c                   If        %found(Fnorm01l)
637400041209     c
637500041209      * ordinanante
637600041209     c                   movel     ormcor        w0070
637700041209     c                   move      w0070         w0040
637800041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
637900041209     c                             w0040 <> 9999  and Wrk_cchiA = 0
638000041209     c                   z-add     w0070         Wrk_cchiA
638100041209     c                   endif
638200041209
638300041209      * ritiro
638400041209     c                   movel     ormcra        w0070
638500041209     c                   move      w0070         w0040
638600041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
638700041209     c                             w0040 <> 9999 and Wrk_cchiB = 0
638800041209     c                   z-add     w0070         Wrk_cchiB
638900041209     c                   endif
639000041209
639100041209      * destinatario
639200041209     c                   movel     ormcrc        w0070
639300041209     c                   move      w0070         w0040
639400041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
639500041209     c                             w0040 <> 9999 and Wrk_cchiC = 0
639600041209     c                   z-add     w0070         Wrk_cchiC
639700041209     c                   endif
639800041209    3c                   endif
639900041209    2c                   endif
640000041209
640100041209      * ----------  GENERICO  -----------
640200041209      * leggo il successivo
640300041209    2c                   if        tgdtor = 'G'
640400041209     c                   iter
640500041209    2c                   endif
640600041209
640700041209     c                   enddo
640800041209
640900041209     c                   endsr
641000040203      *
641100040129      *---------------------------------------------------------------*
641200040212      * Routine pulizia  videata Immissione Reclamo  x nuovo reclamo  *
641300040129      *---------------------------------------------------------------*
641400040129     c     SR_PUL01      BegSR
641500040129      *
641600040421     C                   clear                   v1cant
641700040416     C                   clear                   v1cnut
641800040421      * accendo indicatore che non mette in inversione fondo e sottolineato il numero reclamo
641900040421     c                   seton                                        15
642000040421      *
642100040202     C                   clear                   v1ctco
642200040202     C                   clear                   v1dtco
642300040202     C                   clear                   v1cdar
642400040202     C                   clear                   v1corr
642500040202     C                   clear                   v1cchi
642600050118     C                   clear                   v1dchi
642700040202     C                   clear                   v1crsc
642800160701     C                   clear                   V1naz
642900160701     C                   clear                   V1prov
643000040129     C                   clear                   v1cclv
643100040129     C                   clear                   v1refe
643200040129     C                   clear                   v1ntel
643300040129     C                   clear                   v1mail
643400040129     C                   clear                   v1nfax
643500040218     C                   clear                   W_v1cchi
643600040219     C                   clear                   rec_wrt
643700041207     c                   clear                   Wrk_cchi
643800041207     c                   clear                   Wrk_cchiA
643900041207     c                   clear                   Wrk_cchiB
644000041207     c                   clear                   Wrk_cchiC
644100041207     c                   clear                   SW_v1cchi
644200070413     c                   eval      $bloccof3 = *off
644300160512     c                   eval      $Note_scritte = *off
644400140506     c                   eval      $RA_MA_17 = *off
644500151203     c                   eval      $RA_MA_18 = *off
644600141210     c                   eval      $RA_MA_60 = *off
644700151201     c                   clear                   $Ok_IDCprg
644800151202     c                   clear                   $WrtTIIDC
644900070403     c                   setoff                                       63
645000041210      * Imposto il numeratore
645100041210     c                   z-add     1             SV_pdt
645200040213      * pulizia video relativo oggetto
645300040213     c                   exsr      sr_pulctl
645400040213      *
645500040202      * pulizia subfile
645600040202     c                   clear                   s01nrr
645700150310      * accendo indicatore di posizionamento cursore su CHIAMANTE
645800150310     c                   seton                                        44
645900040213      * imposto il tipo comunicazione
646000040213     c                   eval      v1ctco = 'T'
646100040213      * decodifico
646200150625     c                   clear                   dCOR
646300040217     c                   clear                   tibs02ds
646400040213     C                   MOVEL     'C'           t02mod
646500040213     C                   MOVEL     knsif         t02sif
646600040213     C                   MOVEL     'COR'         t02cod
646700040213     C                   MOVEL     v1ctco        t02ke1
646800040213      *
646900040213     C                   CALL      'TIBS02R'
647000040213     C                   PARM                    KPJBA
647100040213     C                   PARM                    TIBS02DS
647200040213      *
647300040213    4C                   IF        t02err = *BLANKS
647400040213     C                   movel     T02uni        Dcor
647500040216     C                   movel     §CORdesc      v1dtco
647600040213      *
647700040213     C                   ELSE
647800040216     C                   clear                   v1dtco
647900040213    4C                   ENDIF
648000040203      *
648100040129     c                   endsr
648200040213      *
648300040213      *---------------------------------------------------------------*
648400040213      * Routine pulizia  videata Immissione Reclamo  x nuova riga     *
648500040213      *---------------------------------------------------------------*
648600040213     c     SR_PULctl     BegSR
648700040213      *
648800141215     c                   exsr      sr_PULctl2
648900040216      *
649000040216     c                   setoff                                       2120
649100040216     c                   seton                                        22
649200040216     c                   write     fia1c01
649300040216     c                   setoff                                       22
649400040213      *
649500040213     c                   ENDSR
649600141215      *
649700141215      *---------------------------------------------------------------*
649800141215      *
649900141215     c     SR_PULctl2    BegSR
650000141215      *
650100141215     C                   clear                   forz_rec
650200141215     C                   clear                   v1ctor
650300141215     C                   clear                   v1poeo
650400141215     C                   clear                   v1nsor
650500141215     C                   clear                   v1norm
650600141215     C                   clear                   v1nrvo
650700141215     C                   clear                   v1cogg
650800141215     C                   clear                   v1lnps
650900141215     C                   clear                   v1nrss
651000141215     C                   clear                   v1nsps
651100141215     C                   clear                   v1anns
651200141215     C                   clear                   v1ctch
651300141215     C                   clear                   v1dtch
651400141215     C                   clear                   W_cmad
651500150122     C                   clear                   wSaveMAD
651600141215     C                   clear                   v1cmad
651700141215     C                   clear                   v1dmad
651800141215     C                   clear                   v1ccdt
651900141215     C                   clear                   v1dcdt
652000141215     C                   clear                   v1not1
652100141215     C                   clear                   v1not2
652200141215     C                   clear                   v1not3
652300141215     C                   clear                   v1not4
652400150313     C                   clear                   v1crpg
652500150316     C                   clear                   v1cpog
652600150313     C                   clear                   v1cprt
652700150313     C                   clear                   v1dprt
652800141215     C                   clear                   v1crsp
652900141215     C                   clear                   v1cpor
653000141215     C                   clear                   v1cdcr
653100141215     C                   clear                   v1cocr
653200141215     C                   clear                   v1ctcr
653300141215     c                   clear                   v1ccva
653400141215     C                   clear                   v1dcva
653500151223     c                   clear                   V1CfilR
653600151223     c                   clear                   V1CrepR
653700160129     c                   clear                   V1Cafg
653800160129     c                   clear                   V1Carg
653900141215     c                   clear                   W_por
654000141215     C                   clear                   v1dpor
654100141215     C                   clear                   Wormvid
654200141215     C                   clear                   Wspevid
654300141215     C                   clear                   WRK_recsbf
654400141215     C                   clear                   WFIDNB0R
654500141215     c                   clear                   not_dcr
654600141215     c                   clear                   not_ldv
654700141215     c                   clear                   note_dcr
654800151217     c                   clear                   wSavePGN
654900141215      * pulisco i campi di lavoro
655000141215     c                   clear                   wforz
655100160601     c                   eval      $Note_scritte= *off
655200160601     c                   eval      $ric_fnlrg2r = *off
655300141215     c                   eval      $riemetti    = *off
655400141215     c                   eval      $fnarb       = *off
655500170220     c                   eval      *IN04        = *off
655600141217     c                   eval      $RA_MA_60    = *off
655700150326     c                   clear                   W_prt
655800160607      *
655900160215     c                   clear                   dTGD01
656000160215     c                   clear                   dTGDfar
656100160215     c                   clear                   dTGR01A
656200160607      *
656300141215      * accendo indicatore di emissione dell'oggetto generico
656400141215     c                   seton                                        03
656500141215     c                   setoff                                       0102
656600141215     c                   setoff                                       80
656700141215      * data consegna riochiesta
656800141215     c                   setoff                                       616074
656900141215      * pulizia indicatori di protezione
657000141215     c                   setoff                                       515253
657100141215     c                   setoff                                       54
657200141215      * spengo indicatore di note aggiuntive
657300141215     c                   setoff                                       29
657400170515      * spengo indicatore di gestione giacenza
657500170515     c                   setoff                                       04
657600150624      *
657700150624      * -?Spegnimento indicatore di protezione "Tipo Comunicazione"?
657800150624     c                   eval      *in73 = *off
657900151210      *
658000151210      * -?Spegnimento indicatori di gestione "Richiesta Risposta"?
658100151211     c                   eval      *in18 = *off
658200141215      *
658300141215      * imposto il numero progressivo riga solo se sono in immissione
658400141215     c                   if        wmod  = 'I'
658500141215      *
658600141215     c                   z-add     sv_pdt        v1cpdt
658700150313      * imposto la priorità a '1' quando sono in immissione
658800150313     c                   eval      V1Cprt = '1'
658900141215      * Decodifico
659000141215     c                   clear                   tibs02ds
659100141215     C                   MOVEL     'C'           t02mod
659200141215     C                   MOVEL     knsif         t02sif
659300141215     C                   MOVEL     'PRR'         t02cod
659400141215     C                   MOVEL     v1cprt        t02ke1
659500141215      *
659600141215     C                   CALL      'TIBS02R'
659700141215     C                   PARM                    KPJBA
659800141215     C                   PARM                    TIBS02DS
659900141215      *
660000141215     C                   if        t02err = *BLANKS
660100141215     C                   movel     T02uni        DPRR
660200141215     C                   movel     §PRRdesc      v1dprt
660300141215     c                   endif
660400160607      *
660500150313      * Imposto utente e filiale gestione
660600150313     c                   IF        w_pot > 0
660700150313     c                   eval      V1Cpog = %editc(w_pot:'X')
660800150313     c                   ELSE
660900150313     c                   eval      V1Cpog = %editc(DUTpou:'X')
661000150313     c                   ENDIF
661100150313     c                   eval      V1Crpg = KNMUS
661200141215      *
661300141215     c                   endif
661400141215      *
661500141215     c                   EndSR
661600141215      *
661700040218      *****************************************************************
661800040218      *  Gestione del subfile Manutenzione riga reclamo
661900040218      *****************************************************************
662000040218     c     SR_GESSBF     begsr
662100160607      *
662200040218     c                   do        *hival
662300160607      *
662400040218     c                   readc     fia1s01
662500150616     c                   eval      *in84 = *off
662600160607      *
662700161027     c                   If        %eof(fidnA1d)
662800040218     c                   leave
662900040218     c                   endif
663000160607      *
663100040218     c                   if        vs1opz = '2'
663200160607      *
663300150317      /free
663400150317       //?Controllo se l'utente è abiliato alla filiale gestione della RA da manutenzionare
663500150616       //?(SE NON SONO IN SEDE)
663600150317         *in84 = *off;
663700150317         chain(n) (V1Cant:V1Cnut:VS1pdt) FITGD01L;
663800150317         IF  %found(FITGD01L) and
663900150616             NOT *in05        and
664000150318             %lookup(%editc(TGDpog:'X'):sk_wFIL) = 0;
664100150317           *in84 = *on;
664200151223           *in28 = *on;
664300160225           //V1Cmsg = 'Richiesta NON gestibile dall''utente';
664400160225           V1Cmsg = Msg(24);
664500151223           clear  VS1opz;
664600150317           update FIA1S01;
664700150317           leave;
664800150317         ENDIF;
664900150724
665000150724         // -?Controllo anche se R.A. con Motivo Apertura = " 70"?
665100150724         //  ?(Reclamo da Sede) - in gestione ad una filiale?
665200150724         *in84 = *off;
665300150724         If  TGDmad = c_ReclSede  and  TGDpog <> %int(c_Sede)  and  *in05;
665400150724           *in84 = *on;
665500151223           *in28 = *on;
665600160225           //V1Cmsg = 'Richiesta NON gestibile dall''utente';
665700160225           V1Cmsg = Msg(24);
665800151223           clear  VS1opz;
665900150724           update  FIA1S01;
666000150724           leave;
666100150724         EndIf;
666200150317      /end-free
666300160607      *
666400040218      * valorizzo un campo di comodo per differenziare un recupero da subfile
666500040218      * da un recupero da richiamo da altro PGM
666600040218     c                   eval      WRK_recsbf = 'Y'
666700160607      *
666800040421     c                   eval      keyant = v1cant
666900040218     c                   eval      keynut = v1cnut
667000040218     c                   eval      keypdt = vs1pdt
667100040218     c                   exsr      SR_recogg
667200141215     c  n82              clear                   vs1opz
667300040218     c                   update    fia1s01
667400040218      * esco dal ciclo per la gestione del dettaglio scelto
667500160607      *
667600040218     c                   leave
667700040218     c                   endif
667800160607      *
667900040218     c                   enddo
668000040218      *
668100040218     c                   ENDSR
668200040218      *****************************************************************
668300040218      *  Recupero Dettaglio Reclamo in manutenzione
668400040218      *****************************************************************
668500040218     c     SR_RECOGG     begsr
668600040303
668700040303     c                   setoff                                       515253
668800040422     c                   setoff                                       547082
668900151223     c                   setoff                                       748384
669000150625      * -?Proteggo il Tipo Comunicazione SE sono in MODIFICA?
669100150625     c                   eval      *in73 = (wMod <> 'I')
669200150326
669300150326     c                   clear                   W_prt
669400151211     c                   clear                   dTGD01
669500151211     c                   clear                   dTGDfar
669600160126     c                   clear                   dTGR01A
669700040218      *
669800040422     c     Ktgdrec       chain(e)  fitgd01l
669900040218
670000040422    1c                   if        %error
670100040422      * emetto l'errore
670200040422     c                   seton                                        82
670300151223     c                   eval      *in28 = *on
670400160225     c*//                eval      V1Cmsg = 'Richiesta NON gestibile +
670500160225     c*//                                    dall''utente perchè R.A. +
670600160225     c*//                                    impegnata da altro utente'
670700160225     c                   eval      V1Cmsg = Msg(25)
670800040422     c                   else
670900040422      *
671000151210   1Bc                   if        %found(fitgd01l)
671100170517     c                   setoff                                       04
671200040218
671300040218      * numero riga
671400040218     c                   eval      v1cpdt = tgdpdt
671500040218
671600040218      * - OGGETTO
671700040218
671800040218     c                   eval      v1ctor = tgdtor
671900040218
672000040218      * ---------- SPEDIZIONE -----------
672100040218
672200040218    2c                   if        tgdtor = 'S'
672300040218     c                   movel     tgdogg        spevid
672400040218     c                   seton                                        01
672500040218     c                   setoff                                       0203
672600140204     c  n05              exsr      CTR_specons
672700170517     c                   exsr      SR_ctrgiacenza
672800040218      *
672900040218    2c                   endif
673000040218
673100040218      * ----------    ORM     -----------
673200040218
673300040218    2c                   if        tgdtor = 'O'
673400040218     c                   movel     tgdogg        ormvid
673500040218     c                   seton                                        02
673600040218     c                   setoff                                       0103
673700040218    2c                   endif
673800040218
673900040218      * ----------  GENERICO  -----------
674000040218
674100040218    2c                   if        tgdtor = 'G'
674200040218     c                   movel     tgdogg        v1cogg
674300040218     c                   seton                                        03
674400040218     c                   setoff                                       0102
674500040218    2c                   endif
674600040218      * chi chiama
674700040218     c                   eval      v1ctch = tgdtch
674800040304     c                   clear                   tibs02ds
674900040304     C                   MOVEL     'C'           t02mod
675000040304     C                   MOVEL     knsif         t02sif
675100040304     C                   MOVEL     'CCR'         t02cod
675200040304     C                   MOVEL     v1ctch        t02ke1
675300040304      *
675400040304     C                   CALL      'TIBS02R'
675500040304     C                   PARM                    KPJBA
675600040304     C                   PARM                    TIBS02DS
675700040304      *
675800040304     C                   movel     T02uni        DCCR
675900040304     c                   movel     §ccrdesc      v1dtch
676000040218      * motivo
676100040218     c                   eval      v1cmad = tgdmad
676200150410     c                   eval      wSaveMAD = V1Cmad
676300040218      * Decodifica
676400040218     c                   clear                   tibs02ds
676500040218     C                   MOVEL     'C'           t02mod
676600040218     C                   MOVEL     knsif         t02sif
676700040218     C                   MOVEL     'MAR'         t02cod
676800040218     C                   MOVEL     v1cmad        t02ke1
676900040218      *
677000040218     C                   CALL      'TIBS02R'
677100040218     C                   PARM                    KPJBA
677200040218     C                   PARM                    TIBS02DS
677300040218      *
677400040218    2C                   IF        t02err = *BLANKS
677500040218     C                   movel     T02uni        DMAR
677600040218     C                   movel     §MARdesc      v1dmad
677700070323      * se è un motivo di apertura che deve essere protetto in manutenzione accendo il 74
677800070323     c                   if        §marprot = 'S'
677900070323     c                   eval      *in74 = *on
678000070323     c                   endif
678100070323
678200040218    2c                   endif
678300040218      * chiusura reclamo
678400040218     c                   eval      v1ccdt = tgdcdt
678500040218      * Decodifica
678600040218     c                   clear                   tibs02ds
678700040218     C                   MOVEL     'C'           t02mod
678800040218     C                   MOVEL     knsif         t02sif
678900040218     C                   MOVEL     'CHR'         t02cod
679000040218     C                   MOVEL     v1ccdt        t02ke1
679100040218      *
679200040218     C                   CALL      'TIBS02R'
679300040218     C                   PARM                    KPJBA
679400040218     C                   PARM                    TIBS02DS
679500040218      *
679600040218    2C                   IF        t02err = *BLANKS
679700040218     C                   movel     T02uni        DCHR
679800040218     C                   movel     §CHRdesc      v1dcdt
679900040218    2c                   endif
680000150624      *
680100040218      * recupero le note
680200040218      *
680300040218     C                   clear                   FIDNB0DS
680400040421     c                   z-add     v1cant        iB0ant
680500040218     c                   z-add     v1cnut        iB0nut
680600040218     c                   z-add     v1cpdt        iB0pdt
680700040218     c                   movel     'GEN'         iB0tIN
680800040218     c                   movel     'R'           iB0flm
680900040218      *
681000040218     C                   CALL      'FIDNB0R'
681100040218     C                   PARM                    kpjba
681200040218     C                   PARM                    FIDNB0DS
681300040218      *
681400040218      * valorizzo le prime 4 note a video
681500040218      *
681600040218     c                   movel     DB0NO1        v1not1
681700040218     c                   movel     DB0NO2        v1not2
681800040218     c                   movel     DB0NO3        v1not3
681900040218     c                   movel     DB0NO4        v1not4
682000040309      * in caso di manutenzione non do la possibilità di modifcare niente
682100040309     c                   if        Wmod = 'G'
682200040309     c                   seton                                        515253
682300040309     c                   seton                                        54
682400040309     c                   ENDIF
682500070413      * se è un motivo di apertura che deve essere protetto in manutenzione proteggo le note
682600070413     c                   if        §marprot = 'S' and wmod = 'I'
682700070413     c                   eval      *in51 = db0no1 <> *blanks
682800070413     c                   eval      *in52 = db0no2 <> *blanks
682900070413     c                   eval      *in53 = db0no3 <> *blanks
683000070413     c                   eval      *in54 = db0no4 <> *blanks
683100070413     c                   endif
683200040218      *
683300040218      * se ob0piu è valorizzato ci sono + di 4 righe
683400040218     c                   eval      *in29 = (OB0PIU = '+')
683500151217      *
683600151217      * -?Memorizzo ULTIMO Progressivo Note inserito per la R.A.?
683700151217     c                   clear                   wSavePGN
683800151217     c     kTGN          setGT     FITGN000
683900151217     c     kTGN          readPE    FITGN000
684000151217     c                   if        NOT %eof(FITGN01L)
684100151217     c                   eval      wSavePGN = TGNpgn
684200151217     c                   endif
684300151217      *
684400040218      * reparto gestione
684500150312     c                   eval      v1cpog = %editc(tgdpog:'X')
684600040218     c                   eval      v1crpg = tgdrpg
684700040218      * priorità
684800040218     c                   eval      v1cprt = tgdprt
684900150326     c                   eval      W_prt  = tgdprt
685000150313      * se non è chiuso imposto utente e filiale da azute
685100150313      * e priorità a '1' se è a '0'
685200160129if  2c                   IF        V1Ccdt = *blanks
685300150313     c                   eval      V1Crpg = KNMUS
685400160129     c                   if        w_POt  = *zero
685500160129     c                   eval      V1Cpog = %editc( DUTpou : 'X' )
685600160129     c                   else
685700160129     c                   eval      V1Cpog = %editc( w_POt : 'X' )
685800160129     c                   endif
685900150313     c                   IF        V1Cprt = '0'
686000150313     c                   eval      V1Cprt = '1'
686100150313     c                   ENDIF
686200160129e   2c                   ENDIF
686300040323      *
686400040323      * Decodifico
686500040323     c                   clear                   tibs02ds
686600040323     C                   MOVEL     'C'           t02mod
686700040323     C                   MOVEL     knsif         t02sif
686800040323     C                   MOVEL     'PRR'         t02cod
686900040323     C                   MOVEL     v1cprt        t02ke1
687000040323      *
687100040323     C                   CALL      'TIBS02R'
687200040323     C                   PARM                    KPJBA
687300040323     C                   PARM                    TIBS02DS
687400040323      *
687500040323    4C                   IF        t02err = *BLANKS
687600040323     C                   movel     T02uni        DPRR
687700040323     C                   movel     §PRRdesc      v1dprt
687800040323     c                   endif
687900150625      *
688000040218      * responsabilità
688100040218     c                   eval      v1crsp = tgdrsp
688200040218      *
688300040218      * P.O. responsabilità
688400040218     c                   movel     tgdpor        v1cpor
688500040218      * decodifica
688600040218    2c                   if        tgdpor > 0
688700160104     C     tgdpor        chain     azorg
688800040218    3C                   if        %found(azorg01l)
688900040218     C                   movel     orgdes        v1dpor
689000040218    3c                   endif
689100040218    2c                   endif
689200151211      *
689300160104      * -?Richiesta/Ricevuta Risposta?
689400151214     c                   if        TGDflo <> *blank
689500160218if  3c                   if        %subst( TGDflo : 2 : 3) > *zero
689600151211     c                   eval      dTGD01  = TGDflo
689700160218x   3c                   else
689800160218     c                   eval      dTGD01.§TGDaRap =
689900160218     c                             %subst( TGDflo : 1 : 1)
690000160218e   3c                   endif
690100151214     c                   endif
690200151211     c                   eval      dTGDfar = TGDfar
690300040218
690400151210   1Bc                   endif
690500160108
690600160108      /free
690700160108
690800160128         // -?Filiale che Attende Risposta (o che l'ha già Ricevuta)?
690900160108         clear  V1CfilR;
691000160128         if  dTGD01.§TGDfilR > *zero;
691100160128           V1CfilR = %editc( dTGD01.§TGDfilR : 'X' );
691200160128         endif;
691300160108
691400160128         // -?Reparto che Attende Risposta (o che l'ha già Ricevuta)?
691500160128         clear  V1CrepR;
691600160128         TGRtri = 'A';
691700160128         chain(N)  ( TGDant : TGDnut : TGDpdt : TGRtri )  FITGR000;
691800160128         if  %found(FITGR01L);
691900160128           dTGR01A = TGRrif;
692000160128         endif;
692100160128         V1CrepR = dTGR01A.§TGRrepR;
692200160108
692300160128         // -?Visualizzazione "Attesa o Ricevuta Risposta"?
692400160128         clear  V1AttRis;
692500160128         Select;
692600160203           When  dTGDfar.§TGDatt = 'S'  and  dTGDfar.§TGDris = 'S';
692700160201             exsr  sr_ReperimentoRispostaRicevuta;
692800160201             if  WrepR  = *blank;
692900160203               V1AttRis = 'Risposta di Ritorno   RICEVUTA   dal Reparto '
693000160203                        + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
693100160203                        + %editc( dTGD01.§TGDfilR : 'X' );
693200160201             else;
693300160203               V1AttRis = 'Risposta di Ritorno   RICEVUTA   dall''Utente '
693400160201                        + %trimR( WrepR ) + ' della Filiale '
693500160201                        + WfilR;
693600160201             endif;
693700160128           When  dTGDfar.§TGDatt = 'S';
693800160203             V1AttRis = 'Risposta di Ritorno   ATTESA   dal Reparto '
693900160203                      + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
694000160203                      + %editc( dTGD01.§TGDfilR : 'X' );
694100160128         EndSl;
694200160129
694300160129         // -?Visualizzazione Reparto/Filiale che ha attualmente in?
694400160129         //  ?gestione la R.A.?
694500160129         //If  dTGDfar.§TGDatt = 'S';
694600160129           if  V1Crpg <> TGDrpg;
694700160129              V1Carg = '(' + %trimR( TGDrpg ) + ')';
694800160129           endif;
694900160129           if  V1Cpog <> %editc( TGDpog : 'X' );
695000160129              V1Cafg = '(' + %editc( TGDpog : 'X' ) + ')';
695100160129           endif;
695200160129         //EndIf;
695300160108
695400160108      /end-free
695500151211
695600040324      * posizionamento cursore su causale chiusura
695700040324     c                   seton                                        37
695800040324      * Se reclamo chiuso controllo la data chiusura se sono passati più dei
695900040324      * giorni massimi per variare un reclamo chiuso (definiti nella tabella
696000040324      * dati standard REC) proteggo tutti i campi ed emetto un msg di avvertimento
696100041012      * Se utente abilitato a modificare tutto (esempio uff. qualità) non faccio il
696200041012      * controllo (*in07 acceso)
696300041012     c                   if        tgddcd > *zeros and *in50 and not *in07
696400040426
696500040324     c     *iso          movel     tgddcd        Wdataeur
696600040426     c                   z-add     0             giorni
696700040426      *
696800040426     c                   dow       giorni < §recnmgm
696900040426     c                   adddur    1:*d          Wdataeur
697000040426     c                   extrct    wdataeur:*y   kann
697100040426     c                   extrct    wdataeur:*m   kmes
697200040426     c                   extrct    wdataeur:*d   wgiorno
697300040426     c                   clear                   ktfa
697400040426     c                   eval      ktfp = tgdpot
697500160104     c     kazcln        chain     azcln000
697600040426     c                   if        not%found(azcln01l)
697700040426     c                   leave
697800040426     c                   endif
697900040426     c                   if        MAT(wgiorno) =  'F'
698000040426     c                             or  POM(wgiorno) =  'F'
698100040426     c                   iter
698200040426     c                   else
698300040426     c                   add       1             giorni
698400040426     c                   endif
698500040426     c                   enddo
698600040426      *
698700040324     c                   if        wdataeur < wdataoggi
698800040324      * se non sono in immissione proteggo tutti i campi
698900040324     c                   eval      *in70 = *on
699000151223     c                   eval      *in28 = *on
699100151223     c                   eval      V1Cmsg = 'Richiesta chiusa non è più +
699200151223     c                                       modificabile'
699300040324      * posizionamento cursore su opzione subfile
699400141215      * ·?SE non protetta?
699500141215     c                   eval      *in81 = ((*in74 = *on or  §MARutAp <> 'N')
699600141215     c                                       and
699700141215     c                                      (wMod = 'I'  or  §MARutUt <> 'N'))
699800040324     c                   eval      *in37 = *off
699900040324      *
700000040324     c                   endif
700100040324      *
700200040324     c                   endif
700300040422      *
700400151210    1c                   endif
700500150319
700600150319      /free
700700150319       //?Controllo se attivare o no il tasto F11=Variazioni
700800150319         *in17 = *off;
700900150319         chain (V1Cant:V1Cnut:VS1pdt) FITGM01L;
701000150319         IF  %found(FITGM01L);
701100150319           *in17 = *on;
701200150319         ENDIF;
701300150319      /end-free
701400150319
701500040218      *
701600040218     c                   ENDSR
701700160201
701800160201      /free
701900160201
702000160201       BEGSR  sr_ReperimentoRispostaRicevuta;
702100160201
702200160201         clear  WrepR;
702300160201         clear  WfilR;
702400160201
702500160204         setGT  ( V1Cant : V1Cnut : V1Cpdt : 'A' )  FITGM000;
702600160204         readPE ( V1Cant : V1Cnut : V1Cpdt : 'A' )  FITGM000;
702700160201
702800160204         if  %eof( FITGM01L );
702900160201           leavesr;
703000160201         endif;
703100160201
703200160201         WrepR = TGMutn;
703300160201         WfilR = %editc( TGMpon : 'X' );
703400160201
703500160201       ENDSR;
703600160201
703700160201      /end-free
703800160128
703900040219      *---------------------------------------------------------------*
704000040219      * Routine chiusura chiamata                                     *
704100040219      *---------------------------------------------------------------*
704200040219     c     SR_CHIUDO     BegSR
704300040219
704400040219      * per ogni record di dettaglio reclamo aggiorno i dati della parte alta
704500040219      * del video
704600040219
704700040421     c     Ktgdan        setll     Fitgd01l
704800040219      *
704900040219     c                   Do        *hival
705000160216     c     Ktgdan        reade(E)  fitgd01l
705100040219
705200160216      * -?Record allocato?
705300160216     c                   if        %error
705400160216     c     kTGDan        reade(N)  FITGD000
705500160216     c                   iter
705600160216     c                   endif
705700160216      * -?End Of File?
705800040219     c                   if        %eof(fitgd01l)
705900040219     c                   leave
706000040219     c                   endif
706100040219
706200040219     c                   eval      tgddar = wdar                                dta ricez. comunicaz
706300040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
706400040219     c                   eval      tgdtco = v1ctco                              tipo comunicazione
706500040219     c                   eval      tgdrsc = v1crsc                              rag.sociale chiamant
706600041213      * se codice cliente diverso da prima scrivo una nota
706700041214     c                   if        tgdksc <> v1cchi and mod_testa = 'S'
706800150318     c                   eval      wtmo = 'K'
706900150318     c                   clear                   dTGMk
707000150318     c                   eval      §MODksco = TGDksc
707100150318     c                   eval      §MODkscn = V1Cchi
707200150318     c                   exsr      WRT_modifiche
707300041213     c                   endif
707400041213
707500041231     c                   eval      tgdksc = v1cchi                              codice cliente
707600040219     c                   eval      tgdclv = v1cclv                              categoria chiamante
707700121008
707800121008      * se il motivo di apertura lo richiede NON memorizzo il KSC
707900121008     c                   clear                   tibs02ds
708000121008     c                   eval      T02mod = 'C'
708100121008     c                   eval      T02sif = knsif
708200121008     c                   eval      T02cod = 'MAR'
708300121008     c                   eval      T02ke1 = TGDmad
708400121008     c                   call      'TIBS02R'
708500121008     c                   parm                    KPJBA
708600121008     c                   parm                    TIBS02DS
708700121008     c                   eval      dMAR = T02uni
708800121008     c                   IF        §MARksc = 'N'
708900121008     c                   clear                   TGDksc
709000121008     c                   ENDIF
709100150625      *
709200150625      * -?SE premuto F4=Fine chiamata (abilitato SOLO in immissione)?
709300150625      *  ? & R.A. chiusa?
709400150625      *  ? & Tipo Comunicazione con Stato (ex Priorità) in tab. "COR"?
709500150625      *  ?=> lo imposto in TGDPRT?
709600150625     c                   clear                   wSavePRT
709700150625     c                   if             *inKD
709800150625     c                             and  TGDcdt    <> *blanks
709900150625     c                   eval      wSavePRT = TGDprt
710000150625     c                   if             §CORstato <> *blanks
710100150625     c                   eval      TGDprt = §CORstato
710200150625     c                   else
710300150625     c                   eval      TGDprt = '9'
710400150625     c                   endif
710500150625      /free
710600150625         // -?Storicizzo Variazioni Stato?
710700150625         IF  wSavePRT <> TGDprt;
710800150625           wtmo = 'S';
710900150625           clear dTGMs;
711000150625           §MODprto = wSavePRT;
711100150625           §MODprtn = TGDprt;
711200150625           exsr WRT_modifiche;
711300150625         ENDIF;
711400150625      /end-free
711500150625     c                   endif
711600040219
711700040219     c                   update    fitgd000
711800040219
711900040219     c                   enddo
712000040219
712100040219      * verifico se esistono per questa riga che confermo dei record di riferimenti
712200040219      * ed evantualmente li deleto e poi riscrivo
712300040219
712400040421     c     ktgran        setll     fitgr01l
712500040219      *
712600040219     c                   do        *hival
712700040421     c     ktgran        reade     fitgr01l
712800040219      *
712900040219     c                   if        not %eof(fitgr01l)
713000040219     c                   delete    fitgr01l
713100040219     c                   else
713200040219     c                   leave
713300040219     c                   endif
713400040219      *
713500040219     c                   enddo
713600040219      * se esiste referente
713700040219     c                   if        v1refe <> *blank
713800040219     c                   clear                   fitgr000
713900040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
714000040421     c                   eval      tgrant = v1cant                              Numero reclamo
714100040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
714200040219     c                   eval      tgrtri = 'R'                                 tipo  riferimento
714300040219     c                   movel     v1refe        tgrrif
714400040219     c                   write     fitgr000
714500040219     c                   endif
714600040219      * se esiste indirizzo e-mail
714700040219     c                   if        v1mail <> *blank
714800040219     c                   clear                   fitgr000
714900040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
715000040421     c                   eval      tgrant = v1cant                              Numero reclamo
715100040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
715200040219     c                   eval      tgrtri = 'M'                                 tipo  riferimento
715300040219     c                   movel     v1mail        tgrrif
715400040219     c                   write     fitgr000
715500040219     c                   endif
715600040219      * se esiste numero telefono e/o numero fax
715700040219     c                   if        v1ntel <> *blank or v1nfax <> *blank
715800040219     c                   clear                   fitgr000
715900040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
716000040421     c                   eval      tgrant = v1cant                              Numero reclamo
716100040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
716200040219     c                   eval      tgrtri = 'N'                                 tipo  riferimento
716300040219     c                   movel     v1ntel        tgrrif
716400040219     c                   move      v1nfax        tgrrif
716500040219     c                   write     fitgr000
716600040219     c                   endif
716700040219      *
716800040219     c                   ENDSR
716900040219      *
717000040216      *****************************************************************
717100040216      *  Recupero Reclamo in manutenzione
717200040216      *****************************************************************
717300040218     c     SR_RECUPERO   begsr
717400041213      *
717500041213     c                   clear                   mod_testa
717600151202     c                   clear                   $WrtTIIDC
717700150625      *
717800150625      * -?In MODIFICA: proteggo SEMPRE il Tipo Comunicazione?
717900150625     c                   eval      *in73  = *on
718000040302
718100040302      * preparo la chiave con i dati passati
718200040302
718300040421     c                   eval      keyant = ia0ant
718400040302     c                   eval      keynut = ia0nut
718500040302     c                   eval      keypdt = ia0pdt
718600040304     c                   eval      v1cpdt = ia0pdt
718700040304     c                   eval      v1cnut = ia0nut
718800040421     c                   eval      v1cant = ia0ant
718900040304      * carico il subfile
719000040304     c                   exsr      SR_CARSBF
719100040304
719200040422      * dettaglio
719300040422     c                   exsr      SR_recogg
719400040422
719500040302      * valorizzo anche la parte alta della videata
719600040302
719700040303     c                   eval      v1cpot = tgdpot
719800040303     c                   eval      v1ctco = tgdtco
719900040303      * decodifico
720000150625     c                   clear                   dCOR
720100040303     c                   clear                   tibs02ds
720200040303     C                   MOVEL     'C'           t02mod
720300040303     C                   MOVEL     knsif         t02sif
720400040303     C                   MOVEL     'COR'         t02cod
720500040303     C                   MOVEL     v1ctco        t02ke1
720600040303      *
720700040303     C                   CALL      'TIBS02R'
720800040303     C                   PARM                    KPJBA
720900040303     C                   PARM                    TIBS02DS
721000040303      *
721100040303     C                   movel     T02uni        DCOR
721200040303     C                   movel     §CORdesc      v1dtco
721300040303
721400040303     c                   eval      G02inv = tgddar
721500040303     c                   eval      G02err = '3'
721600040303     c                   call      'XSRDA8'
721700040303     c                   parm                    WLBdat
721800040303     c                   z-add     G02dat        v1cdar                         data ricez.comunicaz
721900040303      *
722000040304     c                   MOVEL     tgdorr        v1corr                         ora  ricez.comunicaz
722100040303
722200040303     c                   clear                   v1cchi
722300040303
722400040303     c                   eval      v1crsc = tgdrsc
722500041209     c                   eval      v1cchi = tgdksc
722600070403     c                   eval      W_v1cchi = tgdksc
722700050118      * decodifica
722800050118
722900050118     C                   clear                   TIBS69DS
723000050118     c                   z-add     v1cchi        I69kac
723100050118     C                   call      'TIBS69R'
723200050118     C                   parm                    tibs69DS
723300050118     C                   parm                    DS_cnaco
723400050118     C                   parm                    DS_cnind
723500050118     C                   parm                    DS_cnclp
723600050118     C                   parm                    DS_fncls
723700050118     C                   eval      wtibs69r = '1'
723800050118     c                   If        o69err = ' '
723900050118     c                   movel     ACOrag        v1dchi
724000050118     c                   endif
724100050118
724200040303     c                   eval      v1cclv = tgdclv
724300040303
724400040303      * recupero i riferimenti reclamo
724500040303
724600040303     c                   clear                   v1refe
724700040303     c                   clear                   v1ntel
724800040303     c                   clear                   v1mail
724900040303     c                   clear                   v1nfax
725000040303      *
725100040421     c     Ktgran2       setll     fitgr01l
725200040303     c                   do        *hival
725300040421     c     Ktgran2       reade     fitgr01l
725400040303
725500040303     c                   if        %eof(fitgr01l)
725600040303     c                   leave
725700040303     c                   endif
725800040303
725900040303      * referente
726000040303     c                   if        tgrtri = 'R'
726100040303     c                   eval      v1refe = tgrrif
726200040303     c                   endif
726300040303      * mail
726400040303     c                   if        tgrtri = 'M'
726500040303     c                   eval      v1mail = tgrrif
726600040303     c                   endif
726700040303      * numeri telefonoici
726800040303     c                   if        tgrtri = 'N'
726900040303     c                   movel     tgrrif        v1ntel
727000040303     c                   move      tgrrif        v1nfax
727100040303     c                   endif
727200040303
727300040303     c                   enddo
727400151216      *
727500040216     c                   ENDSR
727600150609
727700150609      *****************************************************************
727800150609      *  Ricerca alfabetica
727900150609      *****************************************************************
728000150609     c     SR_RICALF     begsr
728100150609      *
728200150609     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
728300150609     C                   Z-ADD     3             PAXkut                         tipo estrazione
728400150609     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
728500150609     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
728600150609     c                   movel(P)  V1dchi        PAXrag
728700150609     C                   z-add     1             PAXnum
728800150609      *
728900150609     C                   CALL      'XALFA3BR'
729000150609     C                   PARM                    PAXDUT           30
729100150609     C                   PARM                    PAXkut            1 0
729200150609     C                   PARM                    PAXrag           48
729300150609     C                   PARM                    PAXkcc            4 0
729400150609     C                   PARM                    PAXsta            1 0
729500150609     C                   PARM      *blanks       PAXflr           90
729600150609     C                   PARM      *blanks       PAXdit            3
729700150609     C                   PARM                    PAXnum            2 0
729800150609     C                   PARM                    PAXkcm           80
729900150609     C                   PARM                    PAXksm          140
730000150609     C                   PARM                    PAXkdm           60
730100150609      *
730200150609      * NON TROVATO O PREMUTO CMD12
730300150609    2C                   If        paxsta > -1
730400150609     C                   movel     PAXksm        V1Cchi
730500150609     C                   movel     PAXrag        V1dchi
730600150609     c                   endif
730700150609      *
730800150609     c                   endsr
730900150310
731000150310      /free
731100150310       //--------------------------------------------------------------
731200150318       //?F10 = Immissione ORM.
731300150310       //--------------------------------------------------------------
731400150310       BEGSR  SR_F10;
731500150310
731600150417         clear parm01;
731700150417         clear FIOR05DS;
731800150417         pfgs = DUTpou;
731900150417         ppoe = DUTpou;
732000150417         psce = '1';
732100150417         kpjbu = parm01;
732200150417         fior05r (kpjba:fior05ds);
732300150310
732400150310       ENDSR;
732500150318
732600150318       //--------------------------------------------------------------
732700150318       //?F11 = Visualizza Variazioni.
732800150318       //--------------------------------------------------------------
732900150318       BEGSR  SR_F11;
733000150318
733100150318         fidna3r (kpjba:fidna0ds);
733200150318
733300150318       ENDSR;
733400150310      /end-free
733500150310
733600070330      ****************************************************************
733700070330      * F17 = GESTIONE NOTE LDV
733800070330      ****************************************************************
733900070330     C     Sr_f17        BEGSR
734000070330      *
734100131206     c                   clear                   Fnlrn6DS
734200070423
734300070423      * se al momento del richiamo delle note LDV ho delle note RA propongo il
734400070423      * pgm delle note in immissione
734500070423      * se la linea di arrivo della bolla  è abilitata a GEO e l'ho trovata in fnarb
734600070423
734700131206     c                   eval      in6mod  = 'V'
734800140205      * note valorizzate   o   prevista obbligatorietà (in tab. "MAR")
734900070423     c                   If        (v1not1 <> *blanks or v1not2 <> *blanks  or
735000140205     c                              v1not3 <> *blanks or v1not4 <> *blanks  or
735100140205     c                              §MARnotLdV = 'S')    and
735200070423      * po abilitato a geo e bolla in arb  e non sono la sede
735300070423     c                             $abilitato = *on and $fnarb = *on
735400070423      * verifico se sono la filiale di arrivo della bolla
735500130611     c                   move      arblna        w003a
735600130610     c     w003a         lookup    wPOG                                   27
735700070423     c                   If        %found
735800131206     c                   eval      in6mod  = 'I'
735900131206     c                   movel     v1not1        in6not1
736000131206     c                   move      v1not2        in6not1
736100131206     c                   movel     v1not3        in6not2
736200131206     c                   move      v1not4        in6not2
736300070423     c                   endif
736400070423     c                   endif
736500131206     c                   eval      in6aas  = v1anns
736600131206     c                   eval      in6lnp  = v1lnps
736700131206     c                   eval      in6nrs  = v1nrss
736800131206     c                   eval      in6nsp  = v1nsps
736900131206     c                   eval      in6pgm  = 'FIDNA1R'
737000140205      *
737100140205      * se sono una spedizione in consegna passo i dati della distinta del file ARB
737200140205     c                   If        $cons  = *on
737300160104     c     KSPED         chain     Fnarb000
737400140205     c                   If        %found(Fnarb01l)
737500140205     c                   eval      in6fgs = arbifp
737600140205     c                   eval      in6nfv = arbndc
737700140205     c                   eval      in6dfv = arbddc
737800140205     c                   eval      in6pdr = arbpdc
737900140205     c                   eval      in6cons = 'S'
738000140205     c                   Endif
738100140205     c                   Endif
738200070330
738300131206     c                   call      'FNLRN6R'
738400070330     c                   parm                    kpjba
738500131206     c                   parm                    FNLRN6DS
738600070330
738700070330     c                   endsr
738800040212      ****************************************************************
738900040212      * F18 = GESTIONE NOTE
739000040212      ****************************************************************
739100040212     C     Sr_f18        BEGSR
739200040212      *
739300040212     C                   clear                   FIDNB0DS
739400040218     c                   z-add     v1cpot        iB0pot
739500040421     c                   z-add     v1cant        iB0ant
739600040212     c                   z-add     v1cnut        iB0nut
739700040212     c                   z-add     v1cpdt        iB0pdt
739800040218
739900040219     c                   z-add     Wdar          ib0dar
740000040421     c                   movel(p)  v1corr        ib0orr
740100040218
740200040218     c                   move      v1cmad        ib0mad
740300040218     c                   movel     v1dmad        ib0dma
740400040218
740500040212     c                   movel     'GEN'         iB0tIN
740600040218
740700040218      * - OGGETTO
740800040218
740900040218      * ---------- SPEDIZIONE -----------
741000040218
741100040218     c                   if        v1ctor = 'S'
741200040218
741300040218     c                   movel     spevid        WRKspe
741400040218
741500040219     c                   eval      ib0ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
741600040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
741700040219     c                             ' del ' + WRKann
741800040218      *
741900040218     c                   endif
742000040218
742100040218      * ----------    ORM     -----------
742200040218
742300040218     c                   if        v1ctor = 'O'
742400040218
742500040218     c                   movel     ormvid        WRKorm
742600040218
742700040218     c                   eval      ib0ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
742800040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
742900040218     c                             '/' + %trim(WRKnrv)
743000040218      *
743100040218     c                   endif
743200040218
743300040218      * ----------  GENERICO  -----------
743400040218
743500040218     c                   if        v1ctor = 'G'
743600040218     c                   movel     v1cogg        ib0ogg
743700040218     c                   endif
743800040218
743900040212     c                   movel     v1not1        db0no1
744000040212     c                   movel     v1not2        db0no2
744100040212     c                   movel     v1not3        db0no3
744200040212     c                   movel     v1not4        db0no4
744300040219      * se sono in modalità immissione e richiamo una riga da subfile richiamo il pgm note con
744400040219      * FLM = ' ' altrimenti con "M"
744500040305     c                   if        Wmod = 'I'
744600040305     c                   if        wrk_recsbf = 'Y'
744700040219     c                   movel     ' '           iB0flm
744800040219     c                   else
744900040212     c                   movel     'M'           iB0flm
745000040219     c                   endif
745100040305      * modalità manutenzione
745200040305     c                   else
745300040305     c                   movel     'V'           iB0flm
745400040305     c                   endif
745500070413      * se ho già scritto delle note anche se sono in immissione perchè provengo
745600070413      * da gestione dei blocchi consegne vado nel pgm gestione note ijn manutenzione
745700160601     c****               If        Wmod = 'I' and $bloccof3 = *on
745800160601     c                   If        Wmod = 'I' and $Note_scritte= *oN
745900070413     c                   movel     'V'           iB0flm
746000070413     c                   endif
746100040617      * aggiunto valorizzazione DB0PON
746200040617      * se sono utente particolare che ha un P.O. impostato da tabella passo il P.O. in db0pon
746300040617     c                   eval      db0pon = W_pot
746400040212      *
746500040212     C                   CALL      'FIDNB0R'
746600040212     C                   PARM                    kpjba
746700040217     C                   PARM                    FIDNB0DS
746800040212      *
746900040212      * valorizzo le prime 4 note a video
747000040212      *
747100040212     c                   movel     DB0NO1        v1not1
747200040212     c                   movel     DB0NO2        v1not2
747300040212     c                   movel     DB0NO3        v1not3
747400040212     c                   movel     DB0NO4        v1not4
747500040213      *
747600040213      * se ob0piu è valorizzato ci sono + di 4 righe
747700040213     c                   eval      *in29 = (OB0PIU = '+')
747800040218      *
747900040218      * VALORIZZO FLAG RICHIAMO FIDNB0R
748000040218      *
748100040218     C                   eval      wfidnb0r = 'Y'
748200040212      *
748300040212     c                   ENDSR
748400040216      *
748500040216      *---------------------------------------------------------------*
748600040216      * Gestione WINDOW                                               *
748700040216      *---------------------------------------------------------------*
748800040216     c     SR_GESWIN     BegSR
748900040216
749000040216     c                   do        *hival
749100040216      *
749200040216     c                   exfmt     fia1w01
749300040216      * KF = conferma fine lavoro perdita dei dati
749400040216     c                   if        *inkf
749500040220      * disalloco il file dettagli
749600040220     c                   unlock    fitgd01l
749700040220      *
749800150928     c                   clear                   fidnCAds
749900040421     c                   z-add     v1cant        ic0ant
750000040216     c                   z-add     v1cnut        ic0nut
750100040216     c                   z-add     999           ic0pdt
750200040216      *
750300150928     c                   call      'FIDNCAR'
750400150928     c                   parm                    fidnCAds
750500040216      *
750600040216     c                   leave
750700040216     c                   endif
750800040216      * KL = ritorna alla videata precedente senza deletare nulla
750900040216     c                   if        *inkl
751000040216     c                   leave
751100040216     c                   endif
751200040216
751300040216     c                   enddo
751400040216
751500040216     c                   endsr
751600040223      *---------------------------------------------------------------*
751700040223      * Gestione WINDOW     2                                         *
751800040223      *---------------------------------------------------------------*
751900040223     c     SR_GESWIN2    BegSR
752000040223
752100040223      *
752200040223     c                   exfmt     fia1w02
752300040223      * ENTER = conferma interrogazione bolle arrivi / partenze / ORM
752400040223      * KL = ritorna alla videata
752500040223
752600040223     c                   endsr
752700040406      *---------------------------------------------------------------*
752800040406      * Routine scrittura note aggiuntive automatiche                 *
752900040406      *---------------------------------------------------------------*
753000040406     c     WRT_NOTE      BegSR
753100040406
753200040406     C                   clear                   FIDNB0DS
753300040407      * chiusura
753400141210      *?Verifica telefonica "PINcode"?
753500141210if  1c                   If        Not_PINcode = 'S'
753600141211if  2c                   if        oPINfxx = *blank  and  oPINerr = *blank
753700141210     c                   eval      dB0no1 = 'ESITO: POSITIVO'
753800141210x   2c                   else
753900141210     c                   eval      dB0no1 = 'ESITO: NEGATIVO'
754000141210e   2c                   endif
754100141215     c                   clear                   WLBdat
754200141210     c                   eval      G02inv = ARBddc
754300141215     c                   eval      G02err = '3'
754400141210     c                   call      'XSRDA8'
754500141210     c                   parm                    WLBdat
754600141216     c                   eval      dB0no2 = 'DIST: ' +
754700141210     c                                      %subst( %editc( ARBifp : 'X' ) :
754800141210     c                                              %len( ARBifp ) - 2 : 3 ) +
754900141216     c                                      ' ' +
755000141210     c                                      %trim( %editc( ARBndc : '3' ) ) +
755100141210     c                                      ' DEL ' +
755200141211     c                                      %trim( %editc( G02dat : 'Y' ) )
755300141211     c                   eval      dB0no3 = 'AUT: ' +
755400141210     c                                      %trim( %editc( ARBpdc : 'X' ) )
755500141210e   1c                   EndIf
755600040407
755700040407
755800041214     c                   z-add     tgdant        iB0ant
755900041214     c                   z-add     tgdnut        iB0nut
756000041214     c                   z-add     tgdpdt        iB0pdt
756100040406     c                   movel     'GEN'         iB0tIN
756200141216     c                   z-add     999           iB0pgn
756300040406     c                   movel     'W'           iB0flm
756400050111     c                   eval      db0sts ='N'
756500041214     c                   eval      ib0pot =tgdpot
756600041013     c                   eval      db0pon =Dutpou
756700040406     c                   eval      db0utn =knmus
756800040406
756900040406     C                   CALL      'FIDNB0R'
757000040406     C                   PARM                    kpjba
757100040406     C                   PARM                    FIDNB0DS
757200040407
757300141210     c                   clear                   Not_PINcode
757400040406
757500040406     c                   endSR
757600150316
757700150316      /free
757800150316       //---------------------------------------------------------------*
757900150316       // Routine scrittura file modifiche                              *
758000150316       //---------------------------------------------------------------*
758100150316       BEGSR WRT_Modifiche;
758200150316
758300150316         clear FITGM000;
758400160105         if  wTMO = 'A';
758500160105           TGMpot = V1Cpot;
758600160105           TGMant = V1Cant;
758700160105           TGMnut = V1Cnut;
758800160105           TGMpdt = V1Cpdt;
758900160105         else;
759000160105           TGMpot = TGDpot;
759100160105           TGMant = TGDant;
759200160105           TGMnut = TGDnut;
759300151214           TGMpdt = TGDpdt;
759400151214         endif;
759500150316         TGMtmo = wtmo;
759600150316         SELECT;
759700151214         WHEN  TGMtmo = 'A';
759800151214           TGMmod = dTGMa;
759900150318         WHEN  TGMtmo = 'C';
760000150318           TGMmod = dTGMc;
760100150318         WHEN  TGMtmo = 'K';
760200150318           TGMmod = dTGMk;
760300150318         WHEN  TGMtmo = 'R';
760400150318           TGMmod = dTGMr;
760500150326         WHEN  TGMtmo = 'S';
760600150326           TGMmod = dTGMs;
760700150316         WHEN  TGMtmo = 'U';
760800150316           TGMmod = dTGMu;
760900150316         ENDSL;
761000150316         TGMdmo = %dec(%date());
761100150316         TGMomo = %dec(%time());
761200150316         TGMutn = knmus;
761300150316         TGMpon = DUTpou;
761400150318         TGMnoj = knmeb;
761500150316         write FITGM000;
761600150319         feod FITGM01L;
761700150316
761800150316       ENDSR;
761900150316      /end-free
762000150316
762100150316      *
762200040126      *---------------------------------------------------------------*
762300040126      * Routine Iniziale                                              *
762400040126      *---------------------------------------------------------------*
762500040126     c     *InzSR        BegSR
762600040126      *
762700040129     c     *Entry        plist
762800040129     c                   parm                    KPJBA
762900040213     c                   parm                    FIDNA0DS
763000040126      * Reperisco data corrente
763100040126     c                   time                    w0140
763200040126     c                   move      w0140         wData
763300040126     c                   movel     w0140         wOra
763400040126     c                   clear                   WLBdat
763500040126     c                   eval      G02dat = wData
763600040126     c                   call      'XSRDA8'
763700040126     c                   parm                    WLBdat
763800040126     c                   z-add     G02inv        DateU
763900040324     C     *iso          movel     dateu         Wdataoggi
764000040126      *
764100040126      * Reperisco dati job
764200040126     c                   exsr      DatiJob
764300040126     c                   movel     SDSpgm        VTCpgm
764400130806
764500130806      * Imposto le abilitazioni dell'utente
764600130806     c                   eval      dute01 = UTEfaf
764700041005
764800041005      * Se sono in sede accendo 05
764900041005     c                   eval      *in05=(simfel = 0)
765000041005
765100040213      * Se ricevuto parametro per manutenzione  non valorizzo il p.o. emittente con quello
765200040213      * in linea
765300040213     c                   if        parms   > 1
765400040213      * modalità GESTIONE
765500040213     c                   eval      wmod   = 'G'
765600040217     c                   eval      v1tmod = tit2
765700040303     c                   eval      *in50 = *on
765800040420      * reperisco i programmi che hanno richiamato questo pgm
765900040420     c                   Call      'XRTVSTKR'
766000040420     C                   Parm                    PStack         1000
766100040420     C                   Parm                    PStartPoint       3 0
766200040420     C                   Parm                    PEntries          3 0
766300040420     c                   Movel     Pstack        DSstack
766400040213     c                   else
766500040213     c                   eval      v1cpot = dutpou
766600040616      *
766700040213      * modalità IMMISSIONE
766800040213     c                   eval      wmod   = 'I'
766900040217     c                   eval      v1tmod = tit1
767000040213     c                   endif
767100040617      * verifico se sto inserendo o modificando un reclamo da parte di un utente
767200040617      * che deve avere come p.o. apertura e gestione quello della tabella UGR
767300040617      * tabella UGR
767400040617     c                   movel     KNMUS         w_ute
767500040617     c                   clear                   tibs02ds
767600040617     C                   MOVEL     'C'           t02mod
767700040617     C                   MOVEL     knsif         t02sif
767800040617     C                   MOVEL     'UGR'         t02cod
767900040617     C                   MOVEL     W_ute         t02ke1
768000040617      *
768100040617     C                   CALL      'TIBS02R'
768200040617     C                   PARM                    KPJBA
768300040617     C                   PARM                    TIBS02DS
768400040617      *
768500040617     C                   IF        t02err = *BLANKS
768600040617      *
768700040617     c                   movel     t02uni        DUGR
768800040617      * se P.O. da forzare diverso da zero imposto in automatico
768900040617     c                   if        §UGRpofo > 0
769000040617     c                   eval      v1cpot = §UGRpofo
769100040617     c                   eval      W_pot  = §UGRpofo
769200040617     c                   endif
769300040617      *
769400040617     c                   endif
769500040324      * reperisco i dati standard RECLAMI
769600040324      * Controllo
769700040324     c                   clear                   tibs02ds
769800040324     C                   MOVEL     'C'           t02mod
769900040324     C                   MOVEL     knsif         t02sif
770000040324     C                   MOVEL     'REC'         t02cod
770100040324     C                   MOVEL     '1'           t02ke1
770200040324      *
770300040324     C                   CALL      'TIBS02R'
770400040324     C                   PARM                    KPJBA
770500040324     C                   PARM                    TIBS02DS
770600040324      *
770700040324     C                   IF        t02err = *BLANKS
770800040324     C                   movel     T02uni        DREC
770900040324     c                   endif
771000041012      * verifico se sono un utente che può modficare tutto senza controlli
771100130806     c                   if        §UTEvrac = 'S'
771200041012     c                   eval      *in07 = *on
771300041012     c                   endif
771400041005      * se sono in sede apro il file TITAS
771500041005     c                   If        *in05
771600041005     c                   open      titas30c
771700041005     c                   else
771800041005     c                   open      fnblp01l
771900041005     c                   open      fnarb01l
772000140429     c                   open      FIAR401L
772100140429     c                   open      FIAR501L
772200041005     c                   endif
772300151130      *
772400151130      /free
772500170113
772600170113         // -?Esecuzione override al *file AZNUM01L?
772700170113         //  ?(usato dal *pgm chiamato TRUL33R)?
772800170113         //  ?per limitare il "Tempo massimo attesa record"?
772900170113         exsr  sr_OvrAZNUM;
773000151130
773100151130         // -?Caricamento filiali x Estero?
773200151130         clear  TRUL31ds;
773300151130         I31abi = 'EE';
773400151130         trul31r ( KPJBA : TRUL31ds );
773500151130         wFILest_ds = O31pog;
773600151130
773700151130      /end-free
773800151130      *
773900070323      * se non sono in sede verifico se il PO è abilitato a Geo telefonate e mi carico la £6
774000070323     c                   if        not *in05
774100070323     c                   exsr      sr_carl6
774200070323     c                   endif
774300040126      *
774400040126     c                   EndSr
774500040126
774600070323      *---------------------------------------------------------------*
774700070323      * Recupero i P.O.  della £6                                     *
774800070323      *---------------------------------------------------------------*
774900070323     c     Sr_CARL6      begsr
775000070323
775100070323      * Recupero i PO della £6
775200070323      *
775300070323     c                   clear                   trul06ds
775400070323     c                   move      '£6'          d06cod
775500070323     c                   movel     Dutpou        d06key
775600070323     c                   movel(p)  trul06ds      KPJBU
775700070323
775800070323     c                   call      'TRUL06R'
775900070323     c                   parm                    kpjba
776000070323     c                   movel     kpjbu         trul06ds
776100070323     c                   movea     lin           L6
776200070323
776300070323      * verico se PO abilitato alla gestione telefonate GEO
776400070323
776500070323     c                   clear                   og148
776600160104     c     l6(1)         chain     azorg
776700070323     c                   if        %found(azorg01l)
776800070323     c                   movel     orgde8        og148
776900070323     c                   endif
777000070323
777100070323     c                   If        §oggeot = 'S'
777200070323     c                   eval      $abilitato = *on
777300070608     c                   eval      *in08 = *on
777400070323     c                   endif
777500130610
777600130611      /free
777700130611       //?Imposto abilitazione utente
777800130611         clear wpog;
777900130611         SELECT;
778000130611       //?Imposto abilitazione manutenzione bolle di filiale
778100130611           WHEN  §UTEmbf <> *blanks;
778200130611             wabi = §UTEmbf;
778300130611           WHEN  UTEaut <> *blanks;
778400130611             wabi = UTEaut;
778500130611           WHEN  DUTlpo = '1';
778600130611             wabi = 'TP';
778700130611           WHEN  DUTlpo = '2';
778800130611             wabi = 'PO';
778900130611         ENDSL;
779000140123
779100130611      /end-free
779200130611
779300130610      * controllo se ok l'abilitazione dell'utente
779400130611     c                   clear                   TIBS02DS
779500130611     c                   clear                   dLAT
779600130611     c                   eval      T02mod = 'C'
779700130611     c                   eval      T02sif = knsif
779800130611     c                   eval      T02cod = 'LAT'
779900130611     c                   movel(p)  wabi          T02ke1
780000130611     c                   call      'TIBS02R'
780100130611     c                   parm                    kpjba
780200130611     c                   parm                    TIBS02DS
780300130611if  1c                   IF        T02err = *blanks
780400130611     c                   eval      dLAT = T02uni
780500130611e   1c                   ENDIF
780600130611
780700130611      * Se abilitazione trovata e OK
780800130611if  1c                   IF        T02err = *blanks and §LATabi = *blanks
780900130610      * Reperimento dei P.O. gestibili dall'utente
781000130610     c                   clear                   TRUL31DS
781100130610     c                   eval      I31abi = wabi
781200130610     c                   eval      I31cdi = DUTdis
781300130610     c                   eval      I31car = DUTare
781400130610     c                   eval      I31cpo = DUTpou
781500130610     c                   call      'TRUL31R'
781600130611     c                   parm                    kpjba
781700130610     c                   parm                    TRUL31DS
781800130611if  2c                   IF        O31pog > *zeros
781900130610     c                   movea     O31pog        wpog
782000130611x   2c                   ENDIF
782100130611    1c                   ENDIF
782200130611
782300130611      * se sk delle filiali abilitate all'utente vuota impsoto il primo elemento della £6
782400130611     c                   IF        wpog(1) = *blanks
782500130611     c                   eval      wpog(1) = %editc(l6(1):'X')
782600130611     c                   ENDIF
782700150317
782800150317      /free
782900150317       //?Imposto abilitazione utente x manutenzione RA
783000150317         clear wFIL_ds;
783100150317         SELECT;
783200150317         WHEN  §UTErac <> *blanks;
783300150317           wabi = §UTErac;
783400150317         WHEN  UTEaut <> *blanks;
783500150317           wabi = UTEaut;
783600150317         WHEN  DUTlpo = '1';
783700150317           wabi = 'TP';
783800150317         WHEN  DUTlpo = '2';
783900150317           wabi = 'PO';
784000150317         ENDSL;
784100150317       //?Reperimento delle filiali gestibili dall'utente per manutenzione RA
784200150317         clear TRUL31DS;
784300150317         I31abi = wabi;
784400150317         I31cdi = DUTdis;
784500150317         I31car = DUTare;
784600150317         I31cpo = DUTpou;
784700150317         trul31r (kpjba : TRUL31DS);
784800150317         wFIL_ds = O31pog;
784900150317         IF  wFIL_ds = *blanks;
785000150317           sk_wFIL(01) = %editc(DUTpou:'X');
785100150317         ENDIF;
785200150317      /end-free
785300070323
785400070323     c                   endsr
785500070323
785600150601      *---------------------------------------------------------------*
785700150601      * Conto quante RA ci sono a fronte dello stesso oggetto         *
785800150601      *---------------------------------------------------------------*
785900150601     c     Sr_countRA    BegSr
786000150601      *
786100150601      *
786200150603     c                   if        V1ctor = 'O'
786300150603     c                   movel(p)  ormvid        W_V1cogg
786400150603     c                   endif
786500150603     c                   if        V1ctor = 'S'
786600150603     c                   movel(p)  spevid        W_V1cogg
786700150603     c                   endif
786800150603      *
786900160104     c     Ktgd32        setll     fitgd32
787000160104     c     Ktgd32        reade     fitgd32
787100150603     c                   if        %eof(Fitgd32l)
787200150603     c                   eval      IA1INS = 'N'
787300150603     c                   else
787400160104     c     Ktgd32        reade     fitgd32
787500150603      * se c'è una sola RA non abilito F16 nei programmi di interrogazione bolle o ORM
787600150603      * se più di una lo abilito
787700150603     c                   if        not %eof(Fitgd32l)
787800150603     c                   eval      IA1INS = 'I'
787900150603     c                   else
788000150603     c                   eval      IA1INS = 'N'
788100150603     c                   Endif
788200150603
788300150603     c                   Endif
788400150603
788500150601     c                   endsr
788600150601
788700040126      *---------------------------------------------------------------*
788800040126      * Reperimento Dati del job (Utente/Operativi)                   *
788900040126      *---------------------------------------------------------------*
789000040126     c     DatiJob       BegSr
789100040126      *
789200040126     c     *dtaara       define    §azute        azuteds
789300040126     c     *dtaara       define    §datiute      ddatiute
789400040126      *
789500040126     c                   in(E)     *dtaara
789600040126     c                   IF        %ERROR or RSUT = *blanks
789700040126     c                   clear                   Tibs34Ds
789800040126     c                   call      'TIBS34R'
789900040126     c                   parm                    Tibs34Ds
790000040126     c                   in        *dtaara
790100040126     c                   ENDIF
790200040126      *
790300040126     c                   EndSr
790400170113      *
790500170113      /free
790600170113
790700170113       //--------------------------------------------------------------
790800170113       //?Esecuzione OVERRIDE al *file del numeratore R.A. per limitare?
790900170113       //?il "Tempo massimo attesa record" ad un secondo.              ?
791000170113       //--------------------------------------------------------------
791100170113       BEGSR  sr_OvrAZNUM;
791200170113
791300170113         Qcmd = 'ovrdbf file(AZNUM01L) WAITRCD(1) ovrscope(*callLvl)';
791400170113
791500170113         exsr  sr_ExecCmd;
791600170113
791700170113       ENDSR;
791800170113
791900170113       //--------------------------------------------------------------
792000170113       //?Cancellazione OVERRIDE dal *file del numeratore R.A. per     ?
792100170113       //?limitare il "Tempo massimo attesa record" ad un secondo.     ?
792200170113       //--------------------------------------------------------------
792300170113       BEGSR  sr_DltOvrAZNUM;
792400170113
792500170113         Qcmd = 'dltovr file(AZNUM01L) lvl(*)';
792600170113
792700170113         exsr  sr_ExecCmd;
792800170113
792900170113       ENDSR;
793000170113
793100170113       //--------------------------------------------------------------
793200170113       //?Esecuzione del comando (già impostato).                      ?
793300170113       //--------------------------------------------------------------
793400170113       BEGSR  sr_ExecCmd;
793500170113
793600170113         clear Qcap0100;
793700170113         Qcabcsdh = *off;
793800170113         Qcapa    = *off;
793900170113         Qcacmdss = *off;
794000170113         Qcaerved = *allX'00';
794100170113
794200170113         clear Qusec;
794300170113         Qusbprv  = %size(Qusec);
794400170113
794500170113         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
794600170113                           %size(Qcap0100) : 'CPOP0100' : *omit :
794700170113                           0 : 0 : Qusec);
794800170113
794900170113         //if  Qusei <> *blank;
795000170113         //  ...;
795100170113         //endif;
795200170113
795300170113       ENDSR;
795400170113
795500170113      /end-free
795600040126
795700130513** -?Msg:?Messaggi di Errore?------------------------------------------------*
795800040205Codice cliente errato                                                                    1
795900040610Inserire l'oggetto della richiesta                                                       2
796000040211Inserire correttamente numero Spedizione                                                 3
796100040211Spedizione inesistente                                                                   4
796200040211Inserire correttamente numero ORM                                                        5
796300040211Numero ORM inesistente                                                                   6
796400040211Inserire correttamente il "CHI CHIAMA"                                                   7
796500040610Inserire motivo apertura richiesta                                                       8
796600150318Filiale responsabile errata                                                              9
796700040213Inserire Tipo Comunicazione                                                             10
796800040217Data errata                                                                             11
796900040217Ora errata                                                                              12
797000040217Inserire il tipo oggetto                                                                13
797100040610Prima di dare F4 inserire almeno una richiesta                                          14
797200040219Inserire la Ragione Sociale del chiamante                                               15
797300040220Per questo oggetto 'Chi Chiama' non ammesso                                             16
797400040223Tipo comunicazione errata                                                               17
797500040610Motivo apertura richiesta errato                                                        18
797600040610Codice chiusura richiesta errato                                                        19
797700150429Reparto/Utente in gestione errato                                                       20*#
797800040610Data richiesta non deve essere maggiore della data odierna                              21
797900040315Non si può interrogare un oggetto generico                                              22
798000150318Filiale di appartenenza dell'utente diversa dalla filiale gestione                      23
798100160225Richiesta NON gestibile dall'utente                                                     24 +
798200160225Richiesta NON gestibile dall'utente perchè R.A. impegnata da altro utente               25 +
798300160225Se responsabilità Vettore è obbligatorio inserire la Fil. responsabile                  26
798400040610Utente non abilitato alla gestione richieste                                            27
798500170222Impossibile gestire le disposizioni in Giacenza                                         28
798600150429Inserire reparto/utente in gestione                                                     29 #
798700040610Oggetto presente in altre richieste Enter per forzare                                   30
798800040707Oggetto non presente in archivi                                                         31
798900041210Codice Chiamante mancante F4 per forzare la chiusura                                    32
799000070608Variazione non effettuata                                                               33
799100070608Causale variazione errata o non ammessa                                                 34
799200071019Variazione non consentita                                                               35
799300071018Variazione non consentita bolla non in gestione alla Filiale                            36
799400120914Questa Richiesta Assistenza va chiusa con Codice Chiusura                               37
799500120919Immettere le note                                                                       38
799600131119Per il motivo inserito l'oggetto dev'essere                                             39
799700140213Non utilizzabile. Proposto in automatico confermando la R.A.                            40
799800140430Variazione possibile solo su bolle in PARTENZA dalla propria filiale                    41 <
799900140430Motivo Apertura " 17" utilizzabile solo su bolle in ARRIVO                              42 <
800000140923Tipo comunicazione non utilizzabile in gestione R.A.                                    43
800100141029Ora richiesta consegna errata                                                           44
800200141210Motivo Apertura utilizzabile solo per spedizioni in consegna oggi                       45 «
800300141210Motivo Apertura utilizzabile solo per spedizioni che prevedono un "PINcode"             46 «
800400141218Manca il "PINcode" per questa spedizione                                                47 «
800500150429Filiale gestione errata                                                                 48
800600150422Inserire il telefono e/o la mail                                                        49 #
800700150422Reparto gestione errato                                                                 50 #
800800150717Motivo Apertura impostabile solo da utenti di sede                                      51
800900150723Chiusura non consentita da SEDE                                                         52
801000150723Richiesta Assistenza non gestibile da SEDE                                              53
801100150911C.A.P. errato                                                                           54
801200151130Motivo Apertura " 18" NON utilizzabile per bolle EXPORT                                 55
801300151130Esiste già almeno una disposizione di Dirottamento per questa spedizione                56
801400160120Reparto che Attende Risposta errato                                                     57 ¥
801500160120Filiale che Attende Risposta errata                                                     58 ¥
801600160120Reparto/Filiale in Attesa Risposta NON può coincidere con quello di Gestione            59
801700160302ATTENZIONE: numeratori per le R.A. bloccati da altri lavori.  AVVISARE IL CED           60
801800160315Presente Giacenza aperta: Motivo apertura " 18" NON utilizzabile                        61
801900160314Bolla legata: il reso può essere richiesto solo sulla figlia                            62
802000170516Funzione non eseguibile Programma richiamato in precedenza                              63
802100170526Impossibile gestire le disposizioni in Giacenza con Motivo Apertura inserito            64
802200171123Motivo Apertura " 17" NON utilizzabile in questa videata                                65 §
