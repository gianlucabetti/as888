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
004000160314     fFNLBL02L  if   e           k disk
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
007600160314     d MSG             S             78    DIM(062) CTDATA PERRCD(1)            MSG VIDEO
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
024800160706     d  PARflo               166    256
024900160701      *
025000160706      * -?Altri Parametri da passare al pgm interrogaz. Bolle di Filiale?
025100160701     d fnlrU6ds      e ds                  inz
025200040109      *
025300040211      * - Parametri da passare al pgm di visualizzazione ORM
025400040211     D Parm01          DS
025500040211     D  ppoe                          3  0
025600040211     D  pnor                          7  0
025700040211     D  pnsr                          2  0
025800040211     D  pnrv                          2  0
025900040211     D  psce                          1
026000040211     D  pfgs                          3  0
026100040211     D  ppor                          3  0
026200040211     D  pdtr                          8  0
026300040211     D  pmdb                         10
026400040211     D  pprb                         10
026500040211     D  pdts                          8  0
026600040211     D  prmp                          1
026700040211     D  pbrc                          1
026800040211     D  pref                          2
026900040211     D  pmio                          1
027000041005      * - Parametri da passare al pgm di visualizzazione Bolle di sede
027100041005     D TNSB50DS      E DS
027200040211      *
027300040205      * - Esplosione oggetto spedizione
027400040205     D DS_spediz       DS                  INZ
027500040219     D  DSspe_lnp              1      3  0
027600040219     D  DSspe_nrs              4      5  0
027700040219     D  DSspe_nsp              6     12  0
027800040219     D  DSspe_aas             13     16  0
027900040205      * - Esplosione oggetto ORM
028000040205     D DS_orm          DS                  INZ
028100040205     D  DSorm_poe              1      3  0
028200040205     D  DSorm_nsr              4      5  0
028300040205     D  DSorm_nor              6     12  0
028400141212     D  DSorm_nrv             13     14  0
028500040205      *
028600040205      * - Parametri da passare al pgm interrogazione ORM
028700161107     D FIOR010DS     E DS                  inz
028800161107     d   IOR010ric   e                     inz('1')
028900161107     d   IOR010rsrX  e                     inz('X')
029000161107     d   IOR010inrX  e                     inz('X')
029100161107     d   IOR010lorX  e                     inz('X')
029200161107     d   IOR010rsoX  e                     inz('X')
029300161107     d   IOR010rscX  e                     inz('X')
029400070327      *
029500040109     d Status         sds
029600040109     d  SDSpgm           *proc
029700040213     d  PARMS            *parms
029800040107      *
029900170113     d*// dspINFOds       ds           512
030000170113     d*//  di_TASTO             369    369
030100170113     d*//  di_NRG               370    370
030200170113     d*//  di_SFLnrr            378    379B 0
030300040109      *
030400040109     d WLBdat          ds                  inz
030500040109     d  G02dat                 1      8  0 inz
030600040109     d  G02inv                 9     16  0 inz
030700040109     d  G02err                17     17    inz('3')
030800040109     d  G02tgi                18     22  0 inz
030900040211      * - DS di comodo    ORM
031000040211     D                 DS
031100040211     D  W_poe                         3  0
031200040211     D  W_nsr                         2  0
031300040211     D  W_nor                         7  0
031400040211     D  W_nrv                         2  0
031500040303     D  Wormvid                1     14  0 inz
031600040211      *
031700040211     D                 DS
031800040211     D  v1poeo                        3  0
031900040211     D  v1nsor                        2  0
032000040211     D  v1norm                        7  0
032100040211     D  v1nrvo                        2  0
032200040303     D  ormvid                 1     14  0 inz
032300040216      *
032400040216     D                 DS
032500040216     D  WRKpoe                        3
032600040216     D  WRKnso                        2
032700040216     D  WRKnor                        7
032800040216     D  WRKnrv                        2
032900040216     D  WRKORM                 1     14
033000040211      * - DS di comodo    spedizione
033100040211     D                 DS
033200040211     D  W_lnp                         3  0
033300040211     D  W_nrs                         2  0
033400040211     D  W_nsp                         7  0
033500040211     D  W_aas                         4  0
033600040303     D  Wspevid                1     16  0 inz
033700040211      *
033800040211     D                 DS
033900040211     D  v1lnps                        3  0
034000040211     D  v1nrss                        2  0
034100040211     D  v1nsps                        7  0
034200040217     D  v1anns                        4  0
034300040303     D  spevid                 1     16  0 inz
034400040216     D                 DS
034500040219     D  WRKlnp                        3
034600040219     D  WRKnrs                        2
034700040219     D  WRKnsp                        7
034800040219     D  WRKann                        4
034900040219     D  WRKSPE                 1     16
035000040216      * - DS per girare le date
035100040216     D                 DS
035200040216     D  anno4                  1      4  0
035300040216     D  anno2                  3      4  0
035400040216     D  mese                   5      6  0
035500040216     D  giorno                 7      8  0
035600040216     D  data8                  1      8  0
035700040216
035800040212      * - DS Parametri per ricerca Punto Operativo
035900040212     D Dstnsd          DS
036000040212     D  §cod                          3
036100040212     D  §tip                          1
036200040212     D  §des                         25
036300040212     D  §pra                          3
036400040420
036500040420      * - DS Parametri per ricerca Programmi chiamanti
036600040420     D DSStack         ds
036700040420     D  £Stack                       10    Dim(100)
036800040212
036900040426      * - DS Controllo giorni festivi
037000040426     d clnmat          ds
037100040426     d  mat                    1     31    dim(31)
037200040426     d clnpom          ds
037300040426     d  pom                    1     31    dim(31)
037400150317
037500150317     D wFIL_ds         ds
037600150317     d   sk_wFIL               1    750    dim(250)
037700151130
037800151130     d wFILest_ds      ds
037900151130     d   sk_FILest             1    750    dim(250)
038000150114
038100040107      *
038200040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
038300040107      *
038400040109      * - Campi di comodo
038500040108     d w0140           s             14  0
038600040108     d wData           s              8  0
038700040120     d wOra            s              6  0
038800040108     d DateU           s              8  0
038900040213     d ora             s              2  0
039000040213     d minuti          s              2  0
039100040426     d wgiorno         s              2  0
039200151201     d w0090           s              9  0
039300041206     d w0070           s              7  0
039400041206     d w0040           s              4  0
039500130611     d w003a           s              3
039600040212     d wtibs69r        s              1
039700040212     d w1cdar          s              8  0
039800040217     d w1corr          s              6  0
039900040213     d wmod            s              1
040000040217     d rec_wrt         s              3  0
040100040217     d s01nrr          s              4  0
040200040217     d wdar            s              8  0
040300070329     d wdcr            s              8  0
040400040426     d giorni          s              3  0
040500041206     d wforz           s              1
040600040218     d wrk_recsbf      s              1
040700040218     d Wfidnb0r        s              1
040800070327     d W_cau           s              1
040900040219     d W_confe         s              1
041000040323     d W_ute           s              3
041100040617     d W_pot           s              3  0
041200070330     d Not_dcr         s              1
041300070330     d Not_ldv         s              1
041400141210     d Not_PINcode     s              1    inz
041500070402     d w_dtcr          s             11
041600040531     d forz_rec        s              1
041700041213     d mod_testa       s              1
041800050119     d Sv_inkd         s              1
041900070330     d Note_dcr        s             78
042000130610     d wabi            s                   like(UteAut)
042100150313     d w0030           s              3  0
042200150313     d sav_in48        s              1a
042300150316     d wtmo            s                   like(TGMtmo)
042400150422     d $Video          s              2    inz
042500150422     d $GesW04         s               n   inz
042600160128     d $GesW04N        s               n   inz
042700150422     d $Reparto        s               n   inz
042800160104     d $SaveF6         s               n   inz
042900150421     d wwMail          s                   like(V1mail)  inz
043000150421     d wwcMAD          s                   like(V1Cmad)  inz
043100150422     d wwdMAD          s                   like(V1Dmad)  inz
043200150421     d wwcPOG          s                   like(V1Cpog)  inz
043300150422     d wwdPOG          s                   like(W4Dpog)  inz
043400150423     d wwcRPR          s                   like(W4Crpr)  inz
043500150423     d wwdRPR          s                   like(W4Drpr)  inz
043600160201     d WrepR           s                   like(V1CrepR) inz
043700160201     d WfilR           s                   like(V1CfilR) inz
043800070329
043900141212     d xx              s              3  0 inz
044000070329     d x               s              2  0
044100070329     d y               s              2  0
044200070329      *
044300040426
044400040426     d ktfp            s                   like(clntfp)
044500040426     d ktfa            s                   like(clntfa)
044600040426     d kann            s                   like(clnann)
044700040426     d kmes            s                   like(clnmes)
044800040324
044900070327     d W_LAV           s                   like(tgdmad) inz('  2')
045000070327     d W_CON           s                   like(tgdmad) inz(' 10')
045100070327     d W_SUP           s                   like(tgdmad) inz(' 14')
045200070327     d W_APP           s                   like(tgdmad) inz(' 15')
045300070327     d W_FDE           s                   like(tgdmad) inz(' 16')
045400140429     d c_FDEAM         c                   const(' 17')
045500151130     d c_MotAp_Reso    c                   const(' 18')
045600141210     d c_VPINc         c                   const(' 60')
045700150717     d c_ReclSede      c                   const(' 70')
045800070327
045900040324     D Wdataeur        S               D   DATFMT(*eur)
046000040324     D Wdataoggi       S               D   DATFMT(*eur)
046100040216
046200040421     d Keyant          s                   like(tgdant)
046300040218     d Keynut          s                   like(tgdnut)
046400040218     d Keypdt          s                   like(tgdpdt)
046500040212     d Keytri          s                   like(tgrtri)
046600070402     d Keytin          s                   like(tgntin) inz('GEN')
046700040217     d W_v1cchi        s                   like(v1cchi)
046800041207     d Wrk_cchiA       s                   like(v1cchi)
046900041207     d Wrk_cchiB       s                   like(v1cchi)
047000041207     d Wrk_cchiC       s                   like(v1cchi)
047100041207     d Wrk_cchi        s                   like(v1cchi)
047200041206     d SW_v1cchi       s                   like(v1cchi)
047300070503     d SW_cau          s                   like(W_cau)
047400070508     d note_blocco     s                   like(tgnnot)
047500040218     d W_por           s                   like(orgfil)
047600070412     d W_Cmad          s                   like(v1cmad)
047700150122     d wSaveMAD        s                   like(V1Cmad) inz
047800040531     d W_v1cogg        s                   like(tgdogg)
047900040604     d SW_v1cogg       s                   like(tgdogg)
048000041210     d Sv_pdt          s                   like(v1cpdt)
048100141210     d wTGRpdt         s                   like(TGRpdt) inz
048200150326     d W_prt           s                   like(TGDprt)
048300150625     d wSavePRT        s                   like(TGDprt) inz
048400151217     d wSavePGN        s                   like(TGNpgn) inz
048500040217
048600070322      * - Flags booleani
048700131119     d $ric_fnlrg2r    s               n   inz(*off)                            richiamo FNLRG2R
048800131119     d $abilitato      s               n   inz(*off)                            abilitato a geo
048900131119     d $riemetti       s               n   inz(*off)                            riemissione
049000131119     d $bloccof3       s               n   inz(*off)                            blocco f3
049100160512     d $Note_scritte   s               n   inz(*off)                            indica la scrit.note
049200131206     d $cons           s               n   inz(*off)                            spediz.in consegna
049300131119     d $fnarb          s               n   inz(*off)                            trovato fnarb
049400160314     d $Giacenza       s               n   inz
049500140506     d $RA_MA_17       s               n   inz                                  RA con mot.ap. 17
049600151203     d $RA_MA_18       s               n   inz                                  RA con mot.ap. 18
049700141210     d $RA_MA_60       s               n   inz                                  RA con mot.ap. 60
049800151201     d $Ok_IDCprg      s               n   inz
049900151202     d $WrtTIIDC       s               n   inz
050000160120     d $AttesaRisposta...
050100160120     d                 s               n   inz
050200040217      *
050300040217      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
050400040217      *
050500040217
050600040610     D TIT1            C                   CONST('*** IMMISSIONE RICHIESTE ***')
050700040610     D TIT2            C                   CONST('** MANUTENZIONE RICHIESTE **')
050800041213
050900140429     d c_CauVarB_I7    c                   const('I7')
051000150421
051100150421     d c_Sede          c                   const('046')
051200150311
051300150311     d Digits          c                   const('0123456789')
051400160105
051500160105       // -?Attributi di Visualizzazione?
051600160105     d c_DspAtr_ND     c                   const(x'27')
051700160105     d c_DspAtrNorm    c                   const(x'20')
051800160105     d c_DspAtr_RI     c                   const(x'21')
051900160105     d c_DspAtr_HI     c                   const(x'22')
052000160105     d c_DspAtr_BL     c                   const(x'28')
052100160105     d c_DspAtrWht     c                   const(x'22')
052200160105     d c_DspAtrWht_RI  c                   const(x'23')
052300160105     d c_DspAtrWht_UL  c                   const(x'26')
052400160105     d c_DspAtrRed     c                   const(x'28')
052500160105     d c_DspAtrRed_RI  c                   const(x'29')
052600160105     d c_DspAtrRed_BL  c                   const(x'2A')
052700160105     d c_DspAtrRed_UL  c                   const(x'2C')
052800140123
052900140123       //--------------------------------------------------------------
053000140123       //?Definizione prototipi e procedure.
053100140123       //--------------------------------------------------------------
053200140123
053300141210       // -?Reperimento dati tabelle?
053400141210      /copy gaitrasrc/srcProtoPI,TRULTAB
053500141210      /copy gaitrasrc/srcProtoPR,TRULTAB
053600151201
053700151201       // -?Reperimento/Aggiornamento Numeratori in GRU?
053800151201      /copy gaitrasrc/srcProtoPR,TRUL33R
053900141210
054000141210       // -?Gestione ricerca / Controllo tabelle?
054100150911      /copy gaitrasrc/srcProtoPR,TIBS02R
054200160708
054300160708      *// // -?Aggiunge/Toglie gg/mm dalla data?
054400160708     d*// XGIOLAV         pr                  extpgm('XGIOLAV')
054500160708     d*//   xGioLAVds                         likeds(XGIOLAVds)
054600140429
054700140924       // -?Variazione bolle in arrivo?
054800140924      /copy gaitrasrc/srcProtoPR,FNLR48R
054900140429
055000140429       // -?Controllo Causali Variazione bolle?
055100140429      /copy gaitrasrc/srcProtoPR,FNLR66R
055200151130
055300151130       // -?Reperimento Disposizioni di Consegna da WEB?
055400151130     d FNLRY09ds2    e ds                  inz
055500151130     d FNLRY09R        pr                  extpgm('FNLRY09R')
055600151130     d   kpjba                             likeds(KPJBA)
055700141210
055800141210       // -?Richiesta e Controllo del PINcode?
055900141210     d trulPINds     e ds                  inz
056000141210      /copy gaitrasrc/srcProtoPR,TRULPINR
056100150310
056200150310       // -?Immissione ORM
056300150310     d FIOR05R         pr                  extpgm('FIOR05R')
056400150911     d   kpjba                             likeds(kpjba)
056500150911     d   fior05ds                          likeds(FIOR05DS)
056600150318
056700160105       // -?Storico Variazioni
056800150318     d FIDNA3R         pr                  extpgm('FIDNA3R')
056900150911     d   kpjba                             likeds(kpjba)
057000150911     d   fidna0ds                          likeds(FIDNA0DS)
057100150317
057200150317       // -?Caricamento filiali abilitate
057300150911     d TRUL31R         pr                  extpgm('TRUL31R')
057400150911     d   kpjba                             likeds(KPJBA)
057500150911     d   trul31ds                          likeds(TRUL31DS)
057600150319
057700150508       // -?Selezione tab. "RGR" = Reparto Gestione R.A.?
057800150508     d tntbRGRds     e ds                  inz
057900150508     d tntbRGRr        pr                  extpgm('TNTBRGRR')
058000150508     d   kpjba                             likeds(kpjba)
058100150508
058200150319       // -?Selezione tab. "UGR" = Utenti
058300150319     d tntbUGRds     e ds                  inz
058400150319     d tntbUGRr        pr                  extpgm('TNTBUGRR')
058500150319     d   kpjba                             likeds(kpjba)
058600150915
058700150915      *// // -?Interrogazione CAP?
058800150915     d*// TISI00ds      e ds                  inz
058900150915     d*// tisi02r         pr                  extpgm('TISI02R')
059000150915     d*//   kpjba                             likeds(kpjba)
059100150915     d*//   tisi00ds                          likeds(TISI00ds)
059200150915
059300150915       // -?Interrogazione Località?
059400150915     d TISI00ds      e ds                  inz
059500150915     d TISI30ds      e ds                  inz
059600150915     d tisi30r         pr                  extpgm('TISI30R')
059700150915     d   kpjba                             likeds(kpjba)
059800150915     d   tisi00ds                          likeds(TISI00ds)
059900150915     d   tisi30ds                          likeds(TISI30ds)
060000150911
060100150911       // -?Controllo CAP ("Pretrullo TISI95R")?
060200150911     d TISI95ds      e ds                  inz
060300150911     d tisi95r         pr                  extpgm('TISI95R')
060400150911     d   tisi95ds                          likeds(TISI95ds)
060500150915
060600151130       // -?Reperimento Terminal di partenza/arrivo?
060700150915     d FNLV55ds      e ds                  inz
060800150915     d   D55tla      e                     inz('L')
060900150915     d   D55tpt      e                     inz('P')
061000150915     d fnlv55r         pr                  extpgm('FNLV55R')
061100150915     d   fnlv55ds                          likeds(FNLV55ds)
061200170113
061300170113       // -?Parametri API QCAPCMD (Process Commands)?
061400170113     d Qcmd            s           2048    inz  varying
061500170113      /copy qSysInc/qRpgleSrc,QCAPCMD
061600170113       // -?API QCAPCMD (Process Commands)?
061700170113      /copy gaitrasrc/srcProtoPR,QCAPCMD
061800170113
061900170113       // -?Parametri gestione errori API.?
062000170113      /copy qsysinc/qrpglesrc,QUSEC
062100140429
062200040323      *
062300040108      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
062400040108      *
062500151201      * -?File TIIDC02L?
062600151201     d keyTIIDC02    e ds                  extname( TIIDC02L : *key )
062700151201     d                                     prefix( k_ )  inz
062800040120      * - file FITGD01L
062900040421     c     ktgdan        klist
063000040421     c                   kfld                    v1cant
063100040421     c                   kfld                    v1cnut
063200040218      *
063300040218     c     ktgdrec       klist
063400040421     c                   kfld                    Keyant
063500040218     c                   kfld                    Keynut
063600040218     c                   kfld                    Keypdt
063700040212      * - file FITGR01L
063800040421     c     ktgran        klist
063900040421     c                   kfld                    v1cant
064000040421     c                   kfld                    v1cnut
064100141210     c                   kfld                    wTGRpdt
064200040421      *
064300040421     c     ktgran2       klist
064400040421     c                   kfld                    tgdant
064500040421     c                   kfld                    tgdnut
064600141210     c                   kfld                    wTGRpdt
064700151215      *
064800151215     c     keyFITGR01    klist
064900151215     c                   kfld                    TGDant
065000151215     c                   kfld                    TGDnut
065100151215     c                   kfld                    TGDpdt
065200151215     c                   kfld                    TGRtri
065300040421      *
065400150325      * - file FITGD32L
065500150325     c     ktgd32        klist
065600040531     c                   kfld                    v1ctor
065700040531     c                   kfld                    W_v1cogg
065800070402      * - file FITGN01L
065900070402     c     ktgn          klist
066000070402     c                   kfld                    v1cant
066100070402     c                   kfld                    v1cnut
066200070402     c                   kfld                    v1cpdt
066300070402     c                   kfld                    keytin
066400040107
066500070502      * - file FNARB01L - FNBLP01L -
066600040210     c     ksped         klist
066700040210     c                   kfld                    v1anns
066800040210     c                   kfld                    v1lnps
066900040210     c                   kfld                    v1nrss
067000040210     c                   kfld                    v1nsps
067100041209
067200041209     c     kspedw        klist
067300041209     c                   kfld                    W_aas
067400041209     c                   kfld                    W_lnp
067500041209     c                   kfld                    W_nrs
067600041209     c                   kfld                    W_nsp
067700040211      * - file FNORM01L
067800040211     c     korm          klist
067900040211     c                   kfld                    v1poeo
068000040211     c                   kfld                    v1nsor
068100040211     c                   kfld                    v1norm
068200040211     c                   kfld                    v1nrvo
068300041209
068400041209     c     kormw         klist
068500041209     c                   kfld                    W_poe
068600041209     c                   kfld                    W_nsr
068700041209     c                   kfld                    W_nor
068800041209     c                   kfld                    W_nrv
068900040426      * - file AZCLN01L
069000040426     c     kazcln        klist
069100040426     c                   kfld                    ktfp
069200040426     c                   kfld                    ktfa
069300040426     c                   kfld                    kann
069400040426     c                   kfld                    kmes
069500040210
069600040107      *===============================================================*
069700040107      *  RIEPILOGO INDICATORI                                         *
069800040107      *---------------------------------------------------------------*
069900040126      *  01    - Visualizza numero spedizione                         *
070000040126      *  02    - Visualizza numero Orm                                *
070100040126      *  03    - Visualizza chiave generica                           *
070200041005      *  05    - Sono in Sede                                         *
070300041012      *  07    - Utente abilitato a modificare tutto                  *
070400070608      *  08    - Filiale abilitata a GEO                              *
070500070625      *  09    - Bolla in partenza non borderizzata                   *
070600040324      *  10    - Posizionamento cursore                               *
070700050118      *  11    - Posizionamento cursore                               *
070800040421      *  15    - Indicatore  da video numero reclamo                  *
070900131206      *  16    - Spedizione in consegna non visualizza F17 Note Aut   *
071000150319      *  17    - Visualizzo F11=Variazioni                            *
071100040126      *  20    - SFLDSPCTL in C01                                     *
071200040126      *  21    - SFLDSP    in C01                                     *
071300151211      *  21    - SFLEND    in C01                                     *
071400040202      *  22    - SFLCLR    in C01                                     *
071500151211      *  27    - Comodo x lookup in chiera WPOG                       *
071600040109      *  28    - Errore generico dspf                                 *
071700040212      *  29    - Visualizzo dicitura "Altre note"                     *
071800120919      *  30/48 - Errore                                               *
071900150420      *  49    - Immissione: non gestisce Filiale/Reparto/Utente gest.*
072000040303      *  50    - Manutenzione usato per proteggere/non visualizzare   *
072100040303      *          campi e comandi video                                *
072200040324      *  51/54 - Protezione note                                      *
072300150421      *  55    - Errore - n° Telefono in W04                          *
072400150911      *  56    - Errore - C.A.P. errato in W04                        *
072500151223      *  57    - Posiz. cursore in W04/C01 su Reparto x Attesa Risp.  *
072600151223      *  58    - Posiz. cursore in W04/C01 su Filiale x Attesa Risp.  *
072700070328      *  60    - Protezione note dopo richiamo gestione blocchi       *
072800070329      *  61    - emissione data consegna richiesta                    *
072900151211      *  62    - Tipo/Data/Ora consegna richiesta errato/a            *
073000070403      *  63    - Non si può più dare f3                               *
073100151211      *  64    - ERRORE ora richiesta consegna                        *
073200040324      *  70    - Protezione ... non si può modificare nulla           *
073300041012      *  72    - Manutenzione e non è stato selezionato nulla         *
073400150624      *  73    - Protezione ... non si può modificare Tipo Comunicaz. *
073500070326      *  74    - Protezione ... non si può modificare motivo apertura *
073600150624      *  75    - Comodo in lookup                                     *
073700040220      *  79    - Errore campo non numerico                            *
073800040707      *  80    - Oggetto inesistente                                  *
073900040305      *  81    - Posizionamento cursore Opzione subfile               *
074000040422      *  82    - Reverse immage cursore record allocato               *
074100141215      *  83    - Campo opzione (nel sfl) protetto & non visualizzato  *
074200150317      *  84    - RA non manutenzionabile dall'utente                  *
074300040109      *  90    - Errore                                               *
074400040107      *===============================================================*
074500040107
074600040129      *
074700150319     c                   eval      *in17 = *off
074800040129      *
074900040129     c                   do        *hival
075000040213      * se sono in modalità immissione
075100040213     c                   if        wmod = 'I'
075200040213      * Pulizia del video
075300040213     C                   exsr      SR_PUL01
075400040213      *
075500040213     c                   else
075600040213      * carico il reclamo da gestire
075700040303     c                   exsr      SR_RECupero
075800040213      *
075900040213     c                   endif
076000040213
076100040213      * gestione videata di immissione/manutenzione reclamo
076200040213
076300040129     c                   exsr      SR_RECLAMO
076400041214      * verifico se modificata la testa aggiorno i file nella parte della testata
076500041214     c                   if        Mod_testa = 'S'
076600041214     c                   exsr      SR_chiudo
076700041214     c                   endif
076800040213
076900040129      * fine programma F3
077000040213
077100040304     c                   if        *inkc  OR (*INKL and *IN50)
077200041213      *
077300040315     c                   if        *inkc and *in50
077400041213      *
077500041213      * se sono stata richiamata valorizzo la ds FIDNA0DS
077600040305     c                   eval      oa0f03 = 'S'
077700040305     c                   endif
077800040305
077900040129     c                   leave
078000040129     c                   endif
078100040217      *
078200040129     c                   enddo
078300040202
078400040202     C                   IF        Wtibs69r  <>  *BLANKS
078500040202     C                   eval      I69TLA  = 'C'
078600040202     C                   call      'TIBS69R'
078700040202     C                   parm                    TIBS69DS
078800040202     C                   ENDIF
078900040617      *
079000040617     c                   clear                   tibs02ds
079100040617     c                   movel     'C'           t02tla
079200040617     c                   call      'TIBS02R'
079300040617     c                   parm                    kpjba
079400040617     c                   parm                    tibs02ds
079500040617      *
079600040617     c                   clear                   FIDNB0DS
079700040617     c                   movel     'C'           ib0tla
079800040617     c                   movel     FIDNB0DS      kpjbu
079900040617     c                   call      'FIDNB0R'
080000040617     c                   parm                    kpjba
080100040617     c                   parm                    FIDNB0DS
080200150928      *
080300150928     c                   clear                   fidnCAds
080400150928     c                   eval      IC0tla = 'C'
080500150928     c                   call      'FIDNCAR'
080600150928     c                   parm                    fidnCAds
080700040617      *
080800141210     c                   callb     'CLOTABELLA'
080900141210     c                   parm      c_Tabel       wFilTab           1
081000170113      *
081100170113     c                   clear                   TRUL33ds
081200170113     c                   eval      I33tla = 'C'
081300170113     c                   movel(p)  TRUL33ds      KPJBU
081400170113     c                   call      'TRUL33R'
081500170113     c                   parm                    KPJBA
081600170113      *
081700170113      * -?Cancellazione override sul *file AZNUM01L?
081800170113      *  ?(usato dal *pgm chiamato TRUL33R)?
081900170113      *  ?per limitare il "Tempo massimo attesa record"?
082000170113     c                   exsr      sr_DltOvrAZNUM
082100040129      *
082200040129     c                   seton                                        lr
082300160607      *
082400040129      *---------------------------------------------------------------*
082500040129      * Stacco il numero dal numeratore                               *
082600040129      *---------------------------------------------------------------*
082700040129     c     SR_NUMERO     BegSR
082800040129      *
082900040421     c                   time                    w0140
083000040421     C                   MOVE      W0140         V1CANT
083100160302      *
083200040129     C                   clear                   Trul33ds
083300170113     c*//                eval      i33tla = 'L'
083400040129     C                   z-add     0             I33ope
083500141020     C                   z-add     039           I33cnu
083600040129     C                   z-add     1             I33num
083700040421     C                   z-add     v1cant        I33aaa
083800160302      *
083900160302     c                   DoU       v1cnut > *zeros
084000160302      *
084100040129     C                   movel(p)  trul33ds      kpjbu
084200041007      * se sono in sede richiamo il CL
084300040129     C                   CALL      'TRUL33R'
084400040129     C                   PARM                    KPJBA
084500040129     C                   eval      trul33ds = kpjbu
084600160302      * SE num. "39" allocato: si prova con il "38"
084700160302     c                   IF        o33err = 02
084800160302     c                   z-add     038           I33cnu
084900160302     c                   movel(p)  trul33ds      kpjbu
085000160302     c                   call      'TRUL33R'
085100160302     c                   parm                    KPJBA
085200160302     c                   eval      trul33ds = kpjbu
085300160302     c                   ENDIF
085400040129      *
085500160302     c                   if        o33err = 02
085600160302     c                   eval      *in28 = *on
085700160302     c                   eval      *in41 = *on
085800160302     c                   movel     Msg(60)       V1Cmsg
085900160302     c                   leavesr
086000160302     c                   endif
086100160302      *
086200040129     C                   if        o33err = *zeros
086300040202     C                   z-add     o33nri        v1cnut
086400141020     c                   endif
086500160302      *
086600141020     c                   enddo
086700160302      *
086800040421      * spengo indicatore del video
086900040421     c                   setoff                                       15
087000040416      *
087100040421      * se data chiamata uguale a zero la imposto
087200040421      * imposto ora e data inserimento chiamata
087300040421     c                   if        v1cdar = *zeros
087400040416     c                   move      w0140         v1cdar
087500040421     c                   movel     w0140         v1corr
087600040421     c                   endif
087700040421
087800040416     c                   movel     w0140         W1corr
087900040416     c                   clear                   WLBdat
088000040422     c                   move      w0140         g02dat
088100040416     c                   call      'XSRDA8'
088200040416     c                   parm                    WLBdat
088300040416     c                   z-add     G02inv        w1cdar
088400040129      *
088500040129     c                   endsr
088600040202      *---------------------------------------------------------------*
088700040213      * Routine gestione SUBFILE - CONTROL   RECLAMO                  *
088800040202      *---------------------------------------------------------------*
088900040213     c     SR_RECLAMO    BegSR
089000040202
089100040202     c                   seton                                        20
089200150421     c                   eval      *in49 = ( wMod = 'I')
089300040202      *
089400040202     c                   Do        *Hival
089500070330     c                   write     fia1z01
089600040202      * emetto la videata
089700040202     c                   exfmt     fia1c01
089800040203     c                   setoff                                       281090
089900040212     c                   setoff                                       303132
090000040212     c                   setoff                                       333435
090100040212     c                   setoff                                       363738
090200040217     c                   setoff                                       394041
090300040217     c                   setoff                                       424344
090400070330     c                   setoff                                       461162
090500141029     c                   setoff                                       644748
090600160105     c                   setoff                                       5758
090700040202     c                   clear                   V1Cmsg
090800050120     c                   clear                   SV_inkd
090900111025     c                   clear                   $riemetti
091000040304
091100040304      * F12 = Ritorno al programma chiamante  (attivo solo per 50 acceso)
091200040304
091300040304     c                   If        *inkl
091400040304     c                   leave
091500040304     c                   endif
091600040216
091700040216      * F3 = Fine  se non sono in inserimento vado a fine pgm  altrimenti
091800040216      *            prima verifco che non sia stato inserito nulla altrimenti emetto
091900040216      *            un messaggio forzabile e pulisco tutto quello che è stato inserito fino adesso
092000040416      * ATTENZIONE TUTTO QUELLO CHE E' STATO INSERITO ANDRA' PERSO !!!! FORZARE F6
092100070328      *            ma solo se non è stato modificata una spedizione per lasciato avviso o per altri
092200070326      *            blocchi F3 è disabilitato
092300040223
092400040223     c                   If        *inkc
092500040223
092600040223     c                   If        Wmod <> 'I'  or
092700040223     c                             (wmod = 'I' and rec_wrt = 0 and
092800040308     c                             v1ctor = *blanks )
092900040305      *
093000040202     c                   leave
093100040202     c                   endif
093200070326      *
093300070326     c                   If        (wmod = 'I' and rec_wrt > 0) or
093400070326     c                             (wmod = 'I' and rec_wrt = 0 and
093500070326     c                             v1ctor <> *blanks )
093600070326     c                   exsr      SR_GesWin
093700040216      *
093800040216     c                   if        *inkf
093900040223     c                   eval      *inkf  = *off
094000040223     c                   eval      *inkc  = *on
094100040216     c                   leave
094200040216     c                   endif
094300040203
094400040216     c                   iter
094500040216     c                   endif
094600040223
094700040223     c                   endif
094800170116
094900170116      * F10=Immissione ORM Telefonici
095000170116      *?(Spostato PRIMA del reperimento numero R.A.)?
095100170116     c                   If        *inKJ
095200170116     c                   exsr      SR_F10
095300170116      * accendo posizionamento cursore su chiamante
095400170116     c                   seton                                        44
095500170116     c                   iter
095600170116     c                   endif
095700151211
095800040416      * se numero reclamo uguale a zero lo imposto
095900040416     c                   if        v1cnut = *zeros
096000040416     c                   exsr      SR_numero
096100160302     c                   if        *in28
096200160302     c                   iter
096300160302     c                   endif
096400040416     c                   endif
096500150609
096600150609      * F1=Interrogazione piano dei conti
096700150609
096800150609     c                   If        *inKA
096900150609     c                   exsr      SR_ricalf
097000150609      * accendo posizionamento cursore su chiamante
097100150609     c                   seton                                        44
097200150609     c                   iter
097300150609     c                   EndIf
097400040216
097500170116      *//* F10=Immissione ORM Telefonici
097600170116     c*//                If        *inKJ
097700170116     c*//                exsr      SR_F10
097800170116      *//* accendo posizionamento cursore su chiamante
097900170116     c*//                seton                                        44
098000170116     c*//                iter
098100170116     c*//                endif
098200150318
098300150318      /free
098400150318       //?F11=Visualizza Variazioni
098500150318         IF  *inkk;
098600150318           exsr sr_F11;
098700150318           *in44 = *on;   // posizione cursore su chiamante
098800150318           iter;
098900150318         ENDIF;
099000150318      /end-free
099100040213
099200040203      * Controllo il codice e recupero categoria del cliente
099300040203
099400040202     c                   if        v1cchi > 0 and W_v1cchi <> v1cchi
099500040203
099600040202      * aggancio anagrafico
099700040203
099800040202     C                   clear                   TIBS69DS
099900040202     c                   z-add     v1cchi        I69kac
100000040202     c                   z-add     v1cchi        I69kcp
100100040202     C                   call      'TIBS69R'
100200040202     C                   parm                    tibs69DS
100300040202     C                   parm                    DS_cnaco
100400040202     C                   parm                    DS_cnind
100500040202     C                   parm                    DS_cnclp
100600040202     C                   parm                    DS_fncls
100700040202     C                   eval      wtibs69r = '1'
100800040203     c                   If         O69ERR =  '1'
100900040203     C                   eval      *IN28 = *on
101000040203     c                   movel     msg(1)        V1Cmsg
101100040203     C                   eval      *IN30 = *on
101200040213     C                   iter
101300040202     c                   Else
101400050118     c                   movel     ACOrag        v1dchi
101500041206
101600130411     c                   eval      W_v1cchi = v1cchi
101700040202     c                   movel     clpclv        v1cclv
101800040202     c                   Endif
101900040203
102000040203      * se cambiato codice cliente oppure decodificato per la prima volta riemetto la videata
102100040203     c                   iter
102200040203
102300040202     c                   endif
102400151211
102500050119      * se KD prima faccio KF poi faccio kd
102600050119     c                   If        *inkd and v1ctor <> ' '
102700050119     c                   eval      SV_inkd = *inkd
102800050119     c                   eval      *inkd  = *off
102900050119     c                   eval      *inkf  = *on
103000050119     c                   endif
103100050119
103200040203      * richiamo la routine di controllo campi video
103300040203     c                   exsr      SR_CONTR01
103400040212      * se all'uscita della routine di controllo indicatore di errore generico
103500040212      * e di riemissione della videata è acceso riemetto la videata
103600040216     c                   if        *in90 or *in28
103700040212     c                   iter
103800040212     c                   endif
103900040212
104000040615      * F6 =Conferma riga  oggetto  non eseguo se 72 acceso
104100040212
104200040615     c                   If        *inKF and not *in72
104300040212     c                   exsr      SR_CONFE
104400040213      *
104500040213      * pulisco la parte del control relativa all'oggetto
104600040213      *
104700040213     c                   exsr      SR_pulctl
104800040305      * posizionamento cursore su tipo oggetto  se sono in modalità immissione
104900040305     c                   If        Wmod = 'I'
105000040213     c                   seton                                        31
105100040305      *
105200040305      * posizionamento cursore su opzione del subfile
105300040305     c                   else
105400141215     c                   eval      *in72 = *on
105500040305     c                   endif
105600040213      * carico subfile
105700040216     c                   exsr      sr_carsbf
105800040212     c                   endif
105900050119      *
106000050119     c                   if        sv_inkd = '1'
106100050119     c                   eval      *inkf = *off
106200050119     c                   eval      *inkd = *on
106300050119     c                   clear                   sv_inkd
106400050119     c                   exsr      SR_contr01
106500050119      * se all'uscita della routine di controllo indicatore di errore generico
106600050119      * e di riemissione della videata è acceso riemetto la videata
106700050119     c                   if        *in90 or *in28
106800050119     c                   iter
106900050119     c                   endif
107000050119     c                   endif
107100050119      *
107200040213
107300040213      * F4 = FINE CHIAMATA
107400040213
107500040216     c                   If        *inKD
107600040219      *
107700040219      * se devo scrivere un dettaglio prima di chiudere la chiamata il campo W_confe = ' '
107800040219      *
107900040219     c                   if        W_confe <> 'N'
108000040216     c                   exsr      SR_CONFE
108100040219     c                   eval      W_confe = ' '
108200040219     c                   endif
108300040219
108400040219     c                   exsr      SR_chiudo
108500040216
108600040216     c                   leave
108700040216
108800040216     c                   endif
108900040202      *
109000040203     c                   enddo
109100040202      *
109200040203     c                   endsr
109300040202      *
109400040203      *---------------------------------------------------------------*
109500040203      * Routine controllo campi video                                 *
109600040203      *---------------------------------------------------------------*
109700040203     c     SR_CONTR01    BegSR
109800040203
109900040203      *
110000040211    1c                   Do        *Hival
110100040219      *
110200040219      * se dato F4  FINE CHIAMATA
110300040219      *
110400041206    2c                   if        *inkd
110500070430      * controlli della parte alta della videata
110600070430     c                   exsr      sr_contrf4
110700070430     c                   if        *in28
110800070430     c                   leave
110900070430     c                   endif
111000070430      *
111100041206    2c                   endif
111200041206      *
111300040213
111400040213      * Controllo tipo comunicazione
111500040213
111600150625     c                   clear                   dCOR
111700040213    2c                   If        v1ctco <> *blank
111800040213      * Ricerca
111900040213     c     '?'           scan      v1ctco
112000040213    3c                   if        %found
112100040217     c                   clear                   tibs02ds
112200040213     C                   MOVEL     'R'           t02mod
112300040213     C                   MOVEL     knsif         t02sif
112400040213     C                   MOVEL     'COR'         t02cod
112500040213      *
112600040213     C                   CALL      'TIBS02R'
112700040213     C                   PARM                    KPJBA
112800040213     C                   PARM                    TIBS02DS
112900040213      *
113000040213    4C                   IF        T02err = *BLANKS
113100040217     C                   movel     T02uni        DCOR
113200040213     C                   movel     T02ke1        v1ctco
113300040213     C                   movel     §CORdesc      v1dtco
113400040223      *  errore (non ritorna nulla dalla selezione pulisco il campo)
113500040213     C                   ELSE
113600040223     c                   clear                   v1ctco
113700040223     c                   clear                   v1dtco
113800040213    4C                   ENDIF
113900040213      *
114000040213     c                   eval      *in90 = *on
114100040219     C                   eval      *IN41 = *on
114200040213     c                   leave
114300040213      *
114400040213   x3C                   ELSE
114500040213      *
114600040213      * Controllo
114700040217     c                   clear                   tibs02ds
114800040213     C                   MOVEL     'C'           t02mod
114900040213     C                   MOVEL     knsif         t02sif
115000040213     C                   MOVEL     'COR'         t02cod
115100040213     C                   MOVEL     v1ctco        t02ke1
115200040213      *
115300040213     C                   CALL      'TIBS02R'
115400040213     C                   PARM                    KPJBA
115500040213     C                   PARM                    TIBS02DS
115600040213      *
115700040213    4C                   IF        t02err = *BLANKS
115800040213     C                   movel     T02uni        DCOR
115900140923     c                   if        §CORTCUT =  'N'
116000140923     c                   eval      *in28 = *on
116100140923     c                   movel     MSG(43)       V1Cmsg
116200140923     C                   eval      *IN41 = *on
116300140923     c                   clear                   v1dtco
116400140923     c                   leave
116500140923     c                   endif
116600040213     C                   movel     T02ke1        v1ctco
116700040213     C                   movel     §CORdesc      v1dtco
116800040213      *
116900040213     C                   ELSE
117000040213     c                   eval      *in28 = *on
117100040223     c                   movel     MSG(17)       V1Cmsg
117200040213     C                   eval      *IN41 = *on
117300040213     c                   leave
117400040213    4C                   ENDIF
117500040213      *
117600040213    3c                   endif
117700040213      *
117800040213   x2c                   else
117900040213      *
118000040213      *  Campo Obbligatorio
118100040213     c                   eval      *in28 = *on
118200040213     c                   movel     MSG(10)       V1Cmsg
118300040223     C                   eval      *IN41 = *on
118400040213     c                   leave
118500040213    2C                   endif
118600040213
118700040213      * Controllo data e ora comunicazione
118800040213
118900040428    2C                   if        v1cdar =  *zeros
119000040428     c                   eval      *in28 = *on
119100040428     c                   movel     MSG(11)       V1Cmsg
119200040428     C                   eval      *IN42 = *on
119300040428     C                   leave
119400040428    2C                   else
119500040213     C                   clear                   wlbdat
119600040213     C                   z-add     v1cdar        G02dat
119700040213     C                   call      'XSRDA8'
119800040213     C                   parm                    wlbdat
119900040213    3C                   if        G02err = '1'
120000040213     c                   eval      *in28 = *on
120100040213     c                   movel     MSG(11)       V1Cmsg
120200040213     C                   eval      *IN42 = *on
120300040213     C                   leave
120400040213     C                   else
120500040213     C                   z-add     G02dat        v1cdar
120600040213     C                   z-add     G02inv        Wdar
120700040213    3C                   endif
120800040223      * verifico che la data non sia maggiore della data odierna
120900040223    3c                   if        wdar > dateu
121000040223     c                   eval      *in28 = *on
121100040223     c                   movel     MSG(21)       V1Cmsg
121200040223     C                   eval      *IN42 = *on
121300040223     C                   leave
121400040223    3c                   endif
121500040223
121600040213    2C                   endif
121700040213      * Ora comunicazione
121800040213    2C                   if        v1corr > *zeros
121900040213     C                   movel     v1corr        ora
122000040213     C                   move      v1corr        minuti
122100040213
122200040213    3C                   if        ora > 24 or minuti > 59
122300040213     c                   eval      *in28 = *on
122400040213     c                   eval      *in43 = *on
122500040217     C                   movel     msg(12)       v1cmsg
122600040213     c                   leave
122700040213    3C                   endif
122800040213
122900040213     C                   else
123000040213     c                   eval      *in28 = *on
123100040213     c                   eval      *in43 = *on
123200040217     C                   movel     msg(12)       v1cmsg
123300040213     c                   leave
123400040213    2C                   endif
123500040219
123600050119      * controllo del chiamante se uguale a blanks e sono in manutenzione
123700050119
123800050119    2c                   If        v1crsc = *blanks and *in50
123900050119     c                   eval      *in28 = *on
124000050119     c                   movel     MSG(15)       V1Cmsg
124100050119     C                   eval      *IN44 = *on
124200050119     c                   leave
124300050119    2c                   endif
124400040213
124500040213      * verifico se inserito indirizzo mail corretto
124600040213
124700040213    2c                   If        v1mail <> *blanks
124800150421     c                   eval      wwMail = V1mail
124900150421     c                   exsr      sr_CtrlMail
125000150421     c                   eval      V1mail = wwMail
125100150421     c                   if        *in28
125200150421     c                   leave
125300150421     c                   endif
125400040213    2c                   endif
125500151211
125600040203      *
125700040203      * gestione OGGETTO reclamo
125800040203      *
125900040203
126000160706      * F7 Bolle   Interrogazione e ricerca bolla
126100040203      *
126200040211    2C                   IF        *INKG
126300040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
126400040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
126500040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
126600040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
126700040223     c                   exsr      SR_GESWIN2
126800040223      * ritorno alla videata senza interrogare
126900040223     c                   If        *inKL
127000040223     c                   eval      *in90 = *on
127100150324     c                   if        wMod = 'I'
127200150114     c                   eval      *in48 = *on
127300150114     c                   else
127400040223     c                   eval      *in31 = *on
127500150114     c                   endif
127600040223     c                   leave
127700040223     c                   endif
127800040205      *
127900040223     c                   endif
128000040223      *
128100160706      * se non sono in sede interrogo le bolle
128200041005      *
128300041005   a2c                   if        not *in05
128400160701     c                   clear                   KPJBU
128500160701     c                   clear                   ParamU6ds
128600160701     c                   eval      ParamU6ds.PA1flg = '2'
128700160701     c                   eval      KPJBU = ParamU6ds
128800040205      * preparo l'altro parametro da passare
128900160701     c                   clear                   fnlrU6ds
129000160701     c                   eval      iLRU6sel = 'S'
129100160701     c                   eval      iLRU6ra  = 'S'
129200160701     c                   eval      iLRU6nzd = V1naz
129300160701     c                   eval      iLRU6prd = V1prov
129400160701     c                   eval      iLRU6rsd = V1Crsc
129500161027     c                   eval      iLRU6dtd = %dec( %date() - %days(7) )
129600161027     c                   eval      iLRU6dta = %dec( %date() )
129700160701      *
129800160701     c                   call      'FNLRU6R'
129900160701     c                   parm                    KPJBA
130000160701     c                   parm                    fnlrU6ds
130100160701     c                   movel     kpjbu         ParamU6ds
130200160701     c                   if        oLRU6err = 'E'
130300160719     c                   eval      *in28 = *on
130400160719     c                   eval      *in44 = *on
130500160719     c                   movel     oLRU6msg      V1Cmsg
130600160719     c                   leave
130700160701     c                   endif
130800040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
130900160701    3C                   IF        ParamU6ds.PA1f03 = *blanks  and
131000160701     C                             oLRU6tor         = 'S'      and
131100160701     C                             oLRU6err         = *blanks
131200040217     c                   eval      v1ctor = 'S'
131300160701     c                   movel     oLRU6ogg      DS_Spediz
131400040205     c                   seton                                        01
131500040205     c                   setoff                                       0203
131600040205     C                   z-add     DSspe_aas     V1anns
131700040205     C                   z-add     DSspe_lnp     V1lnps
131800040205     C                   z-add     DSspe_nrs     V1nrss
131900040205     C                   z-add     DSspe_nsp     V1nsps
132000140204      * verifico se spedizione in consegna
132100140204     c                   exsr      Ctr_specons
132200131206
132300040211    3C                   endif
132400041007      *
132500041007      * sono in sede
132600041007      *
132700041007     c                   else
132800041007     c                   clear                   fidna1ds
132900041007     c                   eval      IA1ant = v1cant
133000041007     c                   eval      IA1nut = v1cnut
133100041007     c                   eval      IA1pdt = v1cpdt
133200041007      *
133300041008     c                   eval      %SUBST(KPJBU:2:2) = '01'
133400041007      *
133500041007     c                   CALL      'TNSB50R'
133600041007     C                   PARM                    KPJBA
133700041007     C                   PARM                    FIDNA1DS
133800041007      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
133900041007    3c                   IF        IA1INS = 'S'
134000041007     c                   eval      v1ctor = 'S'
134100041007     c                   movel     IA1ogg        DS_Spediz
134200041007     c                   seton                                        01
134300041007     c                   setoff                                       0203
134400041007     C                   z-add     DSspe_aas     V1anns
134500041007     C                   z-add     DSspe_lnp     V1lnps
134600041007     C                   z-add     DSspe_nrs     V1nrss
134700041007     C                   z-add     DSspe_nsp     V1nsps
134800041007     c                   endif
134900041007      *
135000041005   a2c                   endif
135100150112      *
135200040212     c                   eval      *in90 = *on
135300150324if  3c                   if        wMod = 'I'
135400150114     c                   exsr      sr_ImpAut_Ins
135500150114x   3c                   else
135600150112     c                   eval      *in35 = *on
135700150114e   3c                   endif
135800040205     C                   LEAVE
135900040211    2C                   endif
136000040205      *
136100040205      * F09 ORM  Interrogazione e ricerca
136200040205      *
136300040211    2C                   IF        *INKI
136400040205      *
136500040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
136600040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
136700040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
136800040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
136900040223     c                   exsr      SR_GESWIN2
137000040223      * ritorno alla videata senza interrogare
137100040223     c                   If        *inKL
137200040223     c                   eval      *in90 = *on
137300150324     c                   if        wMod = 'I'
137400150114     c                   eval      *in48 = *on
137500150114     c                   else
137600040223     c                   eval      *in31 = *on
137700150114     c                   endif
137800040223     c                   leave
137900040223     c                   endif
138000040223      *
138100040223     c                   endif
138200040223      *
138300161107     c*//                clear                   FIOR010DS
138400161107     c*//                eval      IOR010ric  = '1'
138500161107     c                   reset                   FIOR010ds
138600160708      * -?Date di ritiro:?
138700160708      *  ?· DA = oggi -1?
138800160708     c                   clear                   XGIOLAVds
138900160708     c                   eval      iXGLdata = %int( %subst( %char( %dec(
139000160708     c                                              %timestamp() ) )
139100160708     c                                              : 1 : 8 ) )
139200160708     c                   eval      iXGLsub  = 'S'
139300160708     c                   eval      iXGLgg   = 1
139400160708     c                   eval      iXGLlav  = 'S'
139500160708     c                   eval      iXGLpa   = 'P'
139600160708     c                   eval      iXGLfil  = DUTpou
139700160708     c                   call      'XGIOLAV'
139800160708     c                   parm                    XGIOLAVds
139900160708     c                   if        oXGLerr  = *blanks
140000160708     c                   eval      iOR010darD = %dec( %date( oXGLdata : *iso ) )
140100160708     c                   else
140200160708     c                   eval      iOR010darD = iXGLdata
140300160708     c                   endif
140400160708      *  ?· A  = oggi +1?
140500160708     c                   clear                   XGIOLAVds
140600160708     c                   eval      iXGLdata = %int( %subst( %char( %dec(
140700160708     c                                              %timestamp() ) )
140800160708     c                                              : 1 : 8 ) )
140900160708     c                   eval      iXGLadd  = 'S'
141000160708     c                   eval      iXGLgg   = 1
141100160708     c                   eval      iXGLlav  = 'S'
141200160708     c                   eval      iXGLpa   = 'P'
141300160708     c                   eval      iXGLfil  = DUTpou
141400160708     c                   call      'XGIOLAV'
141500160708     c                   parm                    XGIOLAVds
141600160708     c                   if        oXGLerr  = *blanks
141700160708     c                   eval      iOR010darA = %dec( %date( oXGLdata : *iso ) )
141800160708     c                   else
141900160708     c                   eval      iOR010darA = iXGLdata
142000160708     c                   endif
142100160708      * -?Ragione sociale cliente Ritiro?
142200160708     c                   eval      iOR010rsr  = V1Crsc
142300160708      * -?Provincia cliente Ritiro?
142400160708     c                   eval      iOR010prr  = V1prov
142500160708      * -?Nazione cliente Ritiro?
142600160708     c                   eval      iOR010nar  = V1naz
142700160708      * -?Non Fatti?
142800161115     c                   eval      iOR010nonF = 'NO'
142900040205      * preparo l'altro parametro da passare
143000040205     c                   clear                   fidna1ds
143100040421     c                   eval      IA1ant = v1cant
143200040205     c                   eval      IA1nut = v1cnut
143300040205     c                   eval      IA1pdt = v1cpdt
143400040205      *
143500150701     c                   CALL      'FIOR010R'
143600040205     C                   PARM                    KPJBA
143700150701     C                   PARM                    FIOR010DS
143800040205     C                   PARM                    FIDNA1DS
143900040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
144000040211    3c                   IF        IA1INS = 'S'
144100040217     c                   eval      v1ctor = 'O'
144200040205     c                   movel     IA1ogg        DS_ORM
144300040205     c                   seton                                        02
144400040205     c                   setoff                                       0103
144500040205     C                   z-add     DSorm_poe     V1poeo
144600040217     C                   z-add     DSorm_nsr     V1nsor
144700040205     C                   z-add     DSorm_nor     V1norm
144800040205     C                   z-add     DSorm_nrv     V1nrvo
144900040211    3C                   endif
145000040212     c                   eval      *in90 = *on
145100150324     c                   if        wMod = 'I'
145200150114     c                   exsr      sr_ImpAut_Ins
145300150112     c                   else
145400040219     c                   eval      *in35 = *on
145500150112     c                   endif
145600040205     C                   LEAVE
145700040211    2C                   endif
145800040211      *
145900040211      * controllo la correttezza dei campi inseriti
146000040211      *
146100040218
146200040218      * se dato enter  e tutti i campi del  control (solo la parte relativa all'oggetto della
146300040218      * chiamato) non sono valorizzati e nel subfile c'è almeno un record controllo se è stata
146400040218      * richiesta la modifica di una riga già esistente
146500040218
146600040220    2c                   if        s01nrr > 0  and v1ctor = *blanks and
146700040218     c                             v1cogg = *blanks and ormvid = *zeros and
146800040218     c                             spevid = *zeros and v1ctch = *blanks and
146900040218     c                             v1cmad = *blanks and v1ccdt = *blanks and
147000040218     c                             v1not1 = *blanks and v1not2 = *blanks and
147100040218     c                             v1not3 = *blanks and v1not4 = *blanks and
147200040218     c                             v1crsp = *blanks and v1cpor = *blanks
147300040218
147400070326      * se dato F4 vado a fine altrimenti controllo se è stata fatta richiesta di gestione
147500040219      * riga di subfile
147600040220    3c                   if        *inkd
147700040219      * imposto un campo che mi dice di non eseguire la routine di conferma riga
147800040219     c                   eval      W_confe = 'N'
147900040219
148000040219     c                   leave
148100040219      *
148200040219     c                   else
148300040218     c                   exsr      SR_gessbf
148400040218
148500040218      * se scelto un record da manutenzionare esco dal ciclo per riemettere la videata con
148600040218      * i dati da manutenzionare altrimenti proseguo nei controlli
148700040220    4c                   If        Wrk_recsbf = 'Y'
148800040218     c                   eval      *in90 = *on
148900040422     C                   eval      *IN37 = *on
149000040422     C                   eval      *IN72 = *off
149100040218     c                   leave
149200040220    4c                   endif
149300040218
149400040220    3c                   endif
149500040219
149600040220    2c                   endif
149700151223      *
149800151223      * -?Non si eseguono i controlli se sono stati rilevati errori nella?
149900151223      *  ?gestione del SBF (vedi subr. sr_GesSBF)?
150000151223      *
150100151223    2c                   if        *in28
150200151223     c                   leave
150300151223    2c                   endif
150400040422      *
150500040615      * Non eseguo i controlli se sono in manutenzione e non ho selezionato nulla
150600040422      *
150700040422    2c                   If        *in72
150800040422     c                   leave
150900040422    2c                   endif
151000150112      *
151100040217      * verifico se inserito tipo oggetto
151200040217      *
151300040217    2c                   If        v1cTOR = *blank
151400040217     C                   eval      *IN28 = *on
151500040217     c                   movel     msg(13)       V1Cmsg
151600040217     C                   eval      *IN31 = *on
151700040217     c                   leave
151800040220      *
151900040220     c                   else
152000040220      * Ricerca
152100040220     c     '?'           scan      v1ctor
152200040220    3c                   if        %found
152300040220     c                   clear                   tibs02ds
152400040220     C                   MOVEL     'R'           t02mod
152500040220     C                   MOVEL     knsif         t02sif
152600040220     C                   MOVEL     'OGR'         t02cod
152700040220      *
152800040220     C                   CALL      'TIBS02R'
152900040220     C                   PARM                    KPJBA
153000040220     C                   PARM                    TIBS02DS
153100040220      *
153200040220    4C                   IF        T02err = *BLANKS
153300040220     C                   movel     T02uni        DOGR
153400040220     C                   movel     T02ke1        v1ctor
153500040220      *  errore
153600040220     C                   ELSE
153700040220     c                   clear                   v1ctor
153800040223     C                   eval      *IN31 = *on
153900040223     c                   eval      *in90 = *on
154000040223     c                   leave
154100040220    4C                   ENDIF
154200040220      *
154300040220      *
154400040220   x3C                   ELSE
154500040220      *
154600040220      * Controllo
154700040220     c                   clear                   tibs02ds
154800040220     C                   MOVEL     'C'           t02mod
154900040220     C                   MOVEL     knsif         t02sif
155000040220     C                   MOVEL     'OGR'         t02cod
155100040220     C                   MOVEL     v1ctor        t02ke1
155200040220      *
155300040220     C                   CALL      'TIBS02R'
155400040220     C                   PARM                    KPJBA
155500040220     C                   PARM                    TIBS02DS
155600040220      *
155700040220    4C                   IF        t02err = *BLANKS
155800040223     C                   movel     T02uni        DOGR
155900040220     C                   movel     T02ke1        v1ctor
156000040220      *
156100040220     C                   ELSE
156200040220     C                   eval      *IN28 = *on
156300040220     c                   movel     msg(13)       V1Cmsg
156400040220     C                   eval      *IN31 = *on
156500040220     c                   leave
156600070327    4C                   ENDIF
156700040220      *
156800040220    3c                   endif
156900040220      *
157000040223     c                   movea     §ogrccha      ccr
157100040217    2c                   endif
157200150112      *
157300040211      * tipo oggetto
157400151211      *
157500040211      *               generico e indicatore 03 spento riemetto la videata
157600040211      *
157700040217    2c                   If        v1ctor = 'G' and not *in03
157800040211     c                   setoff                                       0102
157900040217     c                   seton                                        0332
158000040223     c                   clear                   spevid
158100040223     c                   clear                   ormvid
158200040212     c                   eval      *in90 = *on
158300040211     c                   leave
158400040211    2c                   endif
158500151211      *
158600040211      *               spedizione e indicatore 01 spento riemetto la videata
158700151211      *
158800040217    2c                   If        v1ctor = 'S' and not *in01
158900040211     c                   setoff                                       0203
159000040217     c                   seton                                        0133
159100040223     c                   clear                   v1cogg
159200040223     c                   clear                   ormvid
159300040212     c                   eval      *in90 = *on
159400040211     c                   leave
159500040211    2c                   endif
159600151211      *
159700040211      *               spedizione e indicatore 02 spento riemetto la videata
159800151211      *
159900040217    2c                   If        v1ctor = 'O' and not *in02
160000040211     c                   setoff                                       0103
160100040217     c                   seton                                        0234
160200040223     c                   clear                   v1cogg
160300040223     c                   clear                   spevid
160400070412      * pulisco anche i campi per la gestione della data consegna
160500141029     c                   setoff                                       616264
160600070412     c                   eval      $ric_fnlrg2r = *off
160700070416     c                   eval      $fnarb       = *off
160800070412
160900040212     c                   eval      *in90 = *on
161000040211     c                   leave
161100040211    2c                   endif
161200040211      *
161300040707      * verifico se inserito oggetto
161400040211      *
161500040211      * generico
161600040211    2c                   If        *in03 and v1cogg = *blank
161700040211     C                   eval      *IN28 = *on
161800040211     c                   movel     msg(2)        V1Cmsg
161900040217     C                   eval      *IN32 = *on
162000040211     c                   leave
162100040211    2c                   endif
162200040211      * spedizione
162300040211    2c                   If        *in01 and (v1lnps = *zeros or v1nsps = *zeros
162400040211     c                             or v1anns = *zeros)
162500040211     C                   eval      *IN28 = *on
162600040211     c                   movel     msg(3)        V1Cmsg
162700040211     C                   eval      *IN33 = *on
162800040211     c                   leave
162900040211    2c                   endif
163000040217      *
163100040217      * verifico se inserito anno di 2
163200040217      *
163300040217    2c                   If        *in01 and  v1anns > 60 and v1anns < 100
163400040217     c                   add       1900          V1anns
163500040217    2c                   endif
163600040217
163700040217    2c                   If        *in01 and  v1anns <=  60 and v1anns < 100
163800040217     c                   add       2000          V1anns
163900040217    2c                   endif
164000040217
164100040211      * ORM
164200040211    2c                   If        *in02 and (v1poeo=*zeros or v1norm=*zeros)
164300040211     C                   eval      *IN28 = *on
164400040211     c                   movel     msg(5)        V1Cmsg
164500040211     C                   eval      *IN34 = *on
164600040211     c                   leave
164700040211    2c                   endif
164800040211      *
164900040211      * verifico se oggetto inserito esiste
165000040211      *
165100040211      * spedizione
165200151211      *
165300040211    2c                   If        *in01 and Wspevid <> spevid
165400070625     c                   eval      *in09 = *off
165500041005      * se non sono in sede
165600041005   a2c                   if        not *in05
165700040211      * verifico se esiste nelle bolle arrivi
165800040211      *
165900160104     c     KSPED         chain     Fnarb000
166000040211    3c                   If        not %found(Fnarb01l)
166100070416     c                   eval      $fnarb = *off
166200040211      * verifico se esiste nelle bolle partenze
166300040211      *
166400160104     c     KSPED         chain     Fnblp000
166500040211    4c                   If        not %found(Fnblp01l)
166600040707      * errore solo se non sono in manutenzione
166700040707    5c                   If        not *in50
166800040211     C                   eval      *IN28 = *on
166900040211     c                   movel     msg(4)        V1Cmsg
167000040211     C                   eval      *IN33 = *on
167100040211     c                   leave
167200040707      * accendo indicatore di non visualizzazine oggetto con F2
167300040707     c                   else
167400040707     c                   seton                                        80
167500040707    5c                   endif
167600070625      * se trovata in bolla partenza
167700070625     c                   else
167800070625      * verifico se si tratta di bolla ancora da borderizzare
167900070625     c                   eval      *in09 = (blpft2 = ' ' )
168000041209
168100041206    4c                   endif
168200070416      *
168300070416     c                   else
168400070416      * se trovato in arb
168500070416     c                   eval      $fnarb = *on
168600140204      * controllo e ripristino i campi della spedizione in consegna
168700140204     c                   exsr      CTR_specons
168800041206    3c                   endif
168900041005      *
169000041005      * sono in SEDE
169100041005      *
169200041005  xa2c                   else
169300041005     c     KSPED         chain     Titas30c
169400041005    3c                   If        not %found(Titas30c)
169500041005      * errore solo se non sono in manutenzione
169600041005    4c                   If        not *in50
169700041005     C                   eval      *IN28 = *on
169800041005     c                   movel     msg(4)        V1Cmsg
169900041005     C                   eval      *IN33 = *on
170000041005     c                   leave
170100130513      * accendo indicatore di non visualizzazione oggetto con F2
170200041005     c                   else
170300041005     c                   seton                                        80
170400041005    4c                   endif
170500041005
170600041005    3c                   endif
170700041005      *
170800041005   a2c                   endif
170900040211      * mi salvo il numero spedizione
171000040211     c                   z-add     spevid        Wspevid
171100150114      * ...e pulisco quello dell'ORM
171200150114     c                   clear                   WormVid
171300070412      * pulisco anche i campi per la gestione della data consegna
171400141029     c                   setoff                                       616264
171500070412     c                   eval      $ric_fnlrg2r = *off
171600150114      *
171700150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
171800150324     c                   if        wMod = 'I'
171900150114     c                   exsr      sr_ImpAut_Ins
172000150114     c                   endif
172100070412
172200040211    2c                   endif
172300040211      *
172400040211      * ORM
172500151211      *
172600040211    2c                   If        *in02 and Wormvid <> ormvid
172700040211      * verifico se esiste in archivio
172800040211      *
172900160104     c     KORM          chain     Fnorm000
173000040211    3c                   If        not %found(Fnorm01l)
173100040211     c
173200040707      * errore solo se non sono in manutenzione
173300040707    4c                   If        not *in50
173400040211     C                   eval      *IN28 = *on
173500040211     c                   movel     msg(6)        V1Cmsg
173600040211     C                   eval      *IN34 = *on
173700040211     c                   leave
173800040707     c                   else
173900040707      * accendo indicatore di non visualizzazine oggetto con F2
174000040707     c                   seton                                        80
174100040707    4c                   endif
174200041209
174300040211    3c                   endif
174400040211      * mi salvo il numero ORM
174500040211     c                   z-add     ormvid        Wormvid
174600150114      * ...e pulisco quello della spedizione
174700150114     c                   clear                   WspeVid
174800150114      *
174900150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
175000150324     c                   if        wMod = 'I'
175100150114     c                   exsr      sr_ImpAut_Ins
175200150114     c                   endif
175300150114      *
175400040211    2c                   endif
175500151211      *
175600040317      * F2   Interrogazione oggetto
175700040217      *
175800040707    2c                   IF        *inKB
175900040707      * se c'è 80 acceso significa che manca l'oggetto e quindi emetto l'errore
176000040707     c                   if        *in80
176100040707     C                   eval      *IN28 = *on
176200040707     c                   movel     msg(31)       V1Cmsg
176300040707     C                   eval      *IN31 = *on
176400040707     c                   leave
176500040707     c                   endif
176600040217      * verifico se tipo oggetto inserito
176700040217    3c                   if        v1ctor <> 'S' and v1ctor <> 'O'
176800040217     C                   eval      *IN28 = *on
176900040315     c                   movel     msg(22)       V1Cmsg
177000040217     C                   eval      *IN31 = *on
177100040217     c                   leave
177200040217    3c                   endif
177300070430      * richiamo la routine di interrogazione oggetto
177400070430     c                   exsr      SR_gesf2
177500070430      * se oggetto trovato riemetto la  videata
177600070430     c                   if        *in90
177700070430     c                   leave
177800070430     c                   endif
177900040217    2c                   endif
178000040531      *
178100040531      * se sono in immissione verifico se oggetto presente in altri reclami
178200041007      * se non è un oggetto generico
178300040604      * se modificato oggetto
178400041007    2c                   If        not *in03
178500041007      *
178600041007    3c                   If        forz_rec = 'Y'
178700040604
178800041007    4c                   if        *in01
178900041209     c                   movel(p)  spevid        SW_v1cogg
179000041007    4c                   endif
179100040604
179200041007    4c                   if        *in02
179300041209     c                   movel(p)  ormvid        SW_v1cogg
179400041007    4c                   endif
179500040604
179600041007    4c                   If        SW_v1cogg <> W_v1cogg
179700040604     c                   clear                   forz_rec
179800041007    4c                   endif
179900040604
180000041007    3c                   endif
180100040604
180200041007    3c                   If        not *in50 and WRK_recsbf <> 'Y' and
180300040531     c                             forz_rec = ' '
180400040531
180500041007    4c                   if        *in01
180600041209     c                   movel(p)  spevid        W_v1cogg
180700041007    4c                   endif
180800040531
180900041007    4c                   if        *in02
181000041209     c                   movel(p)  ormvid        W_v1cogg
181100041007    4c                   endif
181200040531
181300150325      * verfiico in FITGD32L
181400040531
181500160104     c     Ktgd32        chain     fitgd32
181600150325    4c                   if        %found(fitgd32l)
181700040531     c                   eval      *in28 = *on
181800040531     c                   movel     msg(30)       V1Cmsg
181900040531     C                   eval      *IN31 = *on
182000040531     C                   eval      forz_rec = 'Y'
182100040531     c                   leave
182200041007    4c                   endif
182300040531
182400041007    3c                   endif
182500041007
182600041007    2c                   endif
182700040211      *
182800040211      * verifico l'inserimento del CHI CHIAMA
182900040220      *
183000040220     c                   clear                   v1dtch
183100040220      *
183200040220    2c                   If        v1cTch = *blank
183300040220     C                   eval      *IN28 = *on
183400040220     c                   movel     msg(07)       V1Cmsg
183500040220     C                   eval      *IN35 = *on
183600040220     c                   leave
183700040220      *
183800040220     c                   else
183900040220      * Ricerca
184000040220     c     '?'           scan      v1ctch
184100040220    3c                   if        %found
184200040220     c                   clear                   tibs02ds
184300040220     C                   MOVEL     'R'           t02mod
184400040220     C                   MOVEL     knsif         t02sif
184500040220     C                   MOVEL     'CCR'         t02cod
184600040220      *
184700040220     C                   CALL      'TIBS02R'
184800040220     C                   PARM                    KPJBA
184900040220     C                   PARM                    TIBS02DS
185000040220      *
185100040220    4C                   IF        T02err = *BLANKS
185200040220     C                   movel     T02uni        DCCR
185300040220     C                   movel     T02ke1        v1ctch
185400040220     c                   movel     §ccrdesc      v1dtch
185500040220      *  errore
185600040220     C                   ELSE
185700040220     c                   clear                   v1ctch
185800040220     c                   clear                   v1dtch
185900040220    4C                   ENDIF
186000040220      *
186100040220     C                   eval      *IN35 = *on
186200040220     c                   eval      *in90 = *on
186300040220     c                   leave
186400040220      *
186500040220   x3C                   ELSE
186600040220      *
186700040220      * Controllo
186800150112     c                   exsr      sr_Ctrl_CCR
186900150112     c                   if        *in28
187000150112     c                   leave
187100150112     c                   endif
187200040220      *
187300040220    3c                   endif
187400040220      *
187500040220    2c                   endif
187600151211
187700040220      * verifico se chi chiama è ammesso per quel tipo oggetto
187800040220
187900150421    2c                   if        %lookup( V1Ctch : CCR ) = *zero
188000040220      * errore chi chiama non ammesso
188100040211     C                   eval      *IN28 = *on
188200040220     c                   movel     msg(16)       V1Cmsg
188300040211     C                   eval      *IN35 = *on
188400040211     c                   leave
188500040211    2c                   endif
188600040211      *
188700040211      * verifico l'inserimento del MOTIVO del reclamo
188800070412
188900070412      * se modificato rispetto al precedente pulisco alcuni campi
189000070412     c                   If        v1cmad <> W_cmad
189100070412     c                   eval      $ric_fnlrg2r = *off
189200141029     c                   setoff                                       616264
189300070412     c                   eval      W_cmad = v1cmad
189400141030     c                   clear                   v1ctcr
189500141030     c                   clear                   v1cdcr
189600141030     c                   clear                   v1cocr
189700070412     c                   endif
189800070412
189900150421      * - Ricerca / Controllo
190000150421     c                   eval      wwcMAD = V1Cmad
190100150421     c                   exsr      sr_CtrlMAD
190200150421     c                   eval      V1Cmad = wwcMAD
190300150422     c                   eval      V1Dmad = wwdMAD
190400150421     c                   if        *in28  or  *in90
190500150421     c                   leave
190600150421     c                   endif
190700140212      *
190800140212      * F17=Note LDV
190900140212      *
191000140217     c                   clear                   Not_LdV
191100140217     c                   if        Wmod = 'I'  and  V1Cmad = W_Lav
191200140217     c                                         and  $Cons  = *on
191300140217     c                   eval      Not_LdV = 'S'
191400140217     c                   endif
191500140217      *
191600140212if  2c                   If        *inKR
191700140217      * - SE "Immissione" & "Obbligo Immissione Note LdV"   oppure
191800140217      *   SE "Immissione" & "Motivo" = "  2" & Spedizione in Consegna
191900140217      *   => Note all'F6
192000140217     c                   if        (Wmod = 'I'  and  §MARnotLDV = 'S')  or
192100140217     c                             (Wmod = 'I'  and  V1Cmad = W_Lav
192200140217     c                                          and  $Cons  = *on)
192300140212     C                   eval      *in28 = *on
192400140212     c                   movel     Msg(40)       V1Cmsg
192500140212     c                   leave
192600140212     c                   endif
192700140212     c                   exsr      SR_F17
192800140212     c                   leave
192900140212e   2c                   endif
193000131119      *
193100070322      *  verifico se la causale mi invita all'inserimento della data consegna richiesta
193200070326      *  se non sono in modifica , non sono in sede e sono abilitato a GEO
193300070416      *  e se non ho già richiamato il pgm dei blocchi e se sono un oggetto spedizione  presente
193400070416      *  in arrivo
193500070330    2c                   if        §marrdco = 'S'  and not *in61
193600070402     c                             and not *in74  and dutlpo <> 'S'
193700070403     c                             and $abilitato = *on and $ric_fnlrg2r = *off
193800070416     c                             and v1ctor = 'S' and $fnarb = *on
193900070322      * richiamo la routine gestione data consegna
194000070322     c                   exsr      SR_GES_DTA
194100070329    3c                   if        $riemetti = *on
194200070327     c                   seton                                        90
194300070327     c                   leave
194400070329    3c                   endif
194500070329    2c                   endif
194600070329      * Controllo tipo a data consegna richiesta
194700070403    2c                   if        v1ctcr<> *blanks and *in61 and
194800141030     c                             v1cdcr = 0 and v1cocr = 0
194900070403     c                   eval      *IN28 = *on
195000070403     c                   movel     msg(11)       V1Cmsg
195100070403     c                   eval      *IN62 = *on
195200070403     c                   leave
195300070403    3c                   endif
195400141030      * Controllo ora consegna richiesta presente senza data
195500141030    2c                   if        *in61 and  v1cdcr = 0
195600141030     c                             and v1cocr > 0
195700141030     c                   eval      *IN28 = *on
195800141030     c                   movel     msg(11)       V1Cmsg
195900141030     c                   eval      *IN62 = *on
196000141030     c                   leave
196100141030    3c                   endif
196200070329    2c                   if        v1cdcr>0 and *in61
196300070329     c* controllo formale data
196400070329     c                   clear                   wlbdat
196500070329     c                   z-add     v1cdcr        g02dat
196600070329     c                   clear                   g02err
196700070329     c                   call      'XSRDA8'
196800070329     c                   parm                    wlbdat
196900070329      * data formalmente errata
197000070329    3c                   if        g02err = '1'
197100070329     c                   eval      *IN28 = *on
197200070329     c                   movel     msg(11)       V1Cmsg
197300070329     c                   eval      *IN62 = *on
197400070329     c                   leave
197500070329    3c                   endif
197600141029      * ora formalmente errata
197700141029     C                   if        v1cocr <> *zeros
197800141030     C                   movel     v1cocr        ora
197900141029     C                   move      v1cocr        minuti
198000141029
198100141030     C                   if        ora > 24 or minuti > 59
198200141030     C                             or ora < 0
198300141029     c                   eval      *IN28 = *on
198400141029     c                   movel     msg(44)       V1Cmsg
198500141029     c                   eval      *IN64 = *on
198600141029     c                   leave
198700141029     C                   endif
198800141030
198900141030     C                   endif
199000070329      * richiamo il controllo della data da FNLR48R
199100070329     c                   z-add     g02inv        Wdcr
199200070329     c                   z-add     g02dat        v1cdcr
199300070329     c                   clear                   fnlr48ds
199400070329     c                   eval      d48cvb='CR'
199500070329     c                   eval      d48ffr='F'
199600070329     C                   z-add     v1anns        D48AAS
199700070329     C                   z-add     v1lnps        D48LNP
199800070329     C                   z-add     v1nrss        D48NRS
199900070329     c                   z-add     v1nsps        D48NSP
200000070329     c                   movel     ARBCBO        D48CBO
200100070329     c                   movel     'A'           D48TBO
200200070329     c                   movel     'G'           D48trc
200300070329     c                   movel     fnlr48ds      KPJBU
200400140429     c                   clear                   dsARBd
200500070329     c                   movel     v1ctcr        §BGTCR
200600141029     c                   z-add     v1cocr        §BGHCR
200700070329     c                   z-add     wdcr          §BGDCR
200800070329     c                   movel     arbgc1        §BGGC1
200900070329     c                   movel     arbgc2        §BGGC2
201000070329     c                   movel     arbtc1        §BGTC1
201100070329     c                   movel     arbtc2        §BGTC2
201200070329     c                   call      'FNLR48R'
201300070329     c                   parm                    KPJBA
201400070329     c                   parm                    DSARBD
201500070329     c                   parm                    DSARBK
201600070329     c                   parm                    DARBT
201700070329     c                   parm                    DSARBG
201800070329     c                   parm                    trul90ds
201900070329     c                   movel     Kpjbu         fnlr48ds
202000070329      * ritorno errore da controllo fnlr48r
202100070402    3c                   if        d48err<>*blanks
202200070329     c                   movel     d48msg        v1cmsg
202300070329     c                   eval      *IN28 = *on
202400070329     c                   eval      *IN62 = *on
202500070329     c                   leave
202600070329    3c                   endif
202700070329    2c                   endif
202800120919
202900120919      * se il motivo di apertura RA prevede l'immissione delle note le richiedo
203000120919      * obbligatorie
203100120919     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
203200131017     c                             V1not3 = *blanks and V1not4 = *blanks and
203300120919     c                             §MARnote = 'S'
203400120919     c                   eval      *in28 = *on
203500120919     c                   eval      *in48 = *on
203600120919     c                   eval      V1Cmsg = msg(38)
203700120919     c                   leave
203800120919     c                   ENDIF
203900070329
204000120913      * se il motivo di apertura RA prevede una chiusura automatica
204100120913      * imposto il motivo di chiusura se non già impostato
204200120913     c                   IF        V1Ccdt = *blanks and §MARmotch <> *blanks
204300120913     c                   eval      V1Ccdt = §MARmotch
204400120913     c                   ENDIF
204500040212      *
204600040212      * verifico l'inserimento del CODICE di CHIUSURA del reclamo
204700040220      *
204800040220     c                   clear                   v1dcdt
204900040420     c                   clear                   v1crsp
205000040212    2c                   If        v1ccdt <> *blanks
205100120913
205200120913      * se il motivo di chiusura messo dell'utente è diverso da quello
205300150720      * previsto in tabella MAR errore  se l'utente non è abilitato
205400130705      * alla modifica
205500130705     c                   If        not *in07
205600120913     c                   IF        V1Ccdt <> §MARmotch and §MARmotch <> *blanks
205700120913     c                   eval      *in28 = *on
205800120913     c                   eval      *in37 = *on
205900120913     c                   eval      V1Cmsg = MSG(37)
206000131119     c                   eval      V1Cmsg = %trim(V1Cmsg) + ' "' +
206100120913     c                             %trim(§MARmotch) + '"'
206200120913     c                   leavesr
206300120913     c                   ENDIF
206400130705     c                   Endif
206500150720
206600150720      * -?Se R.A. con Motivo Apertura " 70": la Sede NON può chiuderla?
206700150720     c                   If        *in05  and  wwcMAD = c_ReclSede
206800150720     c                   eval      *in28 = *on
206900150720     c                   eval      *in37 = *on
207000150720     c                   eval      V1Cmsg = Msg(52)
207100150720     c                   leavesr
207200150720     c                   EndIf
207300120913
207400040212     c                   clear                   tibs02ds
207500040212      *
207600040212      * Ricerca
207700040212     c     '?'           scan      v1ccdt
207800040212    3c                   if        %found
207900040217     c                   clear                   tibs02ds
208000040212     C                   MOVEL     'R'           t02mod
208100040212     C                   MOVEL     knsif         t02sif
208200040212     C                   MOVEL     'CHR'         t02cod
208300040212      *
208400040212     C                   CALL      'TIBS02R'
208500040212     C                   PARM                    KPJBA
208600040212     C                   PARM                    TIBS02DS
208700040212      *
208800040212    4C                   IF        T02err = *BLANKS
208900040212     C                   movel     T02uni        DCHR
209000040212     C                   movel     T02ke1        v1ccdt
209100040212     C                   movel     §chrdesc      v1dcdt
209200040420     C                   movel     §chrresp      v1crsp
209300040223      *  errore (non è stato selezionato nulla pulisco  il campo e mi posiziono)
209400040212     C                   ELSE
209500040223     C                   clear                   v1ccdt
209600040223     C                   clear                   v1dcdt
209700040420     C                   clear                   v1crsp
209800040212    4C                   ENDIF
209900040212      *
210000040212     c                   eval      *in90 = *on
210100040219     C                   eval      *IN37 = *on
210200040212     c                   leave
210300040212      *
210400040212   x3C                   ELSE
210500040212      *
210600040212      * Controllo
210700040217     c                   clear                   tibs02ds
210800040212     C                   MOVEL     'C'           t02mod
210900040212     C                   MOVEL     knsif         t02sif
211000040212     C                   MOVEL     'CHR'         t02cod
211100040212     C                   MOVEL     v1ccdt        t02ke1
211200040212      *
211300040212     C                   CALL      'TIBS02R'
211400040212     C                   PARM                    KPJBA
211500040212     C                   PARM                    TIBS02DS
211600040212      *
211700040212    4C                   IF        t02err = *BLANKS
211800040212     C                   movel     T02uni        DCHR
211900040212     C                   movel     §CHRdesc      v1dcdt
212000040420     C                   movel     §chrresp      v1crsp
212100040212      *
212200040212     C                   ELSE
212300040212     c                   eval      *in28 = *on
212400040223     c                   movel     MSG(19)       V1Cmsg
212500040212     C                   eval      *IN37 = *on
212600040212     c                   leave
212700040212    4C                   ENDIF
212800040212      *
212900040212    3c                   endif
213000040212      *
213100040212    2C                   endif
213200151218      *
213300151218      *
213400150624      *?Impostazione STATO (ex Priorità):?
213500150624      * Se c'è la causale chiusura metto priorità '9' (in inserimento)
213600150624      *   o '8' (in modifica - se modificata la causale Chiusura)
213700160108if  2c                   IF        V1Ccdt <> *blanks
213800150624      *
213900160108if  3c                   IF        wmod = 'I'
214000150624      * -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
214100150624      *  ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
214200150625      *  ?(già reperita nella subr. SR_CONTR01)?
214300150624     c                   if        §CORstato <> *blanks
214400150624     c                   eval      V1Cprt = §CORstato
214500150624     c                   else
214600150313     c                   eval      V1Cprt = '9'
214700150624     c                   endif
214800160108x   3c                   ELSE
214900160108sel 4c                   Select
215000160108      * -?SE modalità Modifica & R.A. Chiusa & R.A. in attesa di?
215100160108      *  ?risposta (questa) => si imposta lo Stato "0"?
215200160108      *  ?(la R.A. rimane aperta, girata in Gestione alla fil. che?
215300160108      *  ?Attende Risposta)?
215400160108w   4c                   When      V1CfilR  > *zero         and
215500160201     c                             V1CrepR <> *blank        and
215600160108     c                             (V1Ccdt <> *blank        and
215700160108     c                              dTGDfar.§TGDris = *blank   or
215800160108     c                              V1Ccdt  = *blank        and
215900160108     c                              dTGDfar.§TGDris = 'S')
216000160108     c                   eval      V1Cprt = '0'
216100160108      * -?SE modificato il codice Chiusura => si imposta lo Stato "8"?
216200160108w   4c                   When      V1ccdt <> tgdcdt
216300150324     c                   eval      V1Cprt = '8'
216400160108e   4c                   EndSl
216500160108e   3c                   ENDIF
216600150624      *
216700150624      * Altrimenti (se NON c'è la causale di chiusura) metto
216800150624      *   priorita '0' (in inserimento) o '1' (in modifica)
216900160108x   2c                   ELSE
217000150624      *
217100150421     c                   If        wmod = 'I'
217200150421     c                   eval      V1Cprt = '0'
217300150421     c                   Else
217400150313     c                   eval      V1Cprt = '1'
217500150421     c                   EndIf
217600150624      *
217700160108e   2c                   ENDIF
217800150320      *
217900150320      * Decodifico (potrebbe essere cambiato)
218000150320     c                   clear                   tibs02ds
218100150320     C                   MOVEL     'C'           t02mod
218200150320     C                   MOVEL     knsif         t02sif
218300150320     C                   MOVEL     'PRR'         t02cod
218400150320     C                   MOVEL     v1cprt        t02ke1
218500150320      *
218600150320     C                   CALL      'TIBS02R'
218700150320     C                   PARM                    KPJBA
218800150320     C                   PARM                    TIBS02DS
218900150320      *
219000150320    4C                   IF        t02err = *BLANKS
219100150320     C                   movel     T02uni        DPRR
219200150320     C                   movel     §PRRdesc      v1dprt
219300150320     c                   endif
219400151217      *
219500150324      * se RA aperta richiedo note obbligatorie
219600150324     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
219700150324     c                             V1not3 = *blanks and V1not4 = *blanks and
219800151211     c                             V1Ccdt = *blanks
219900151211     c                             and not *inks
220000150324     c                   eval      *in28 = *on
220100150324     c                   eval      *in48 = *on
220200150324     c                   eval      V1Cmsg = msg(38)
220300150324     c                   leave
220400150324     c                   ENDIF
220500160107      *
220600160204      * -?Note obbligatorie per R.A. "chiusa" (poi ri-aperta automaticam.)?
220700160204      *  ?con Attesa Risposta = "S" & Ricevuta Risposta = " "?
220800160108      *  ?(con salvataggio del tasto funzionale F6)?
220900160108     c                   eval      $SaveF6  = *inKF
221000160204if  2c                   If        Not  *inKS             and
221100160204     c                             V1Ccdt <> *blank       and
221200160204     c                             dTGDfar.§TGDatt = 'S'  and
221300160204     c                             dTGDfar.§TGDris = *blank
221400160108      *
221500160204      * -?Reperisco ULTIMO Progressivo Note inserito?
221600160107     c     kTGN          setGT     FITGN000
221700160107     c     kTGN          readPE    FITGN000
221800160108if  3c                   if        %eof(FITGN01L)
221900160107     c                   clear                   TGNpgn
222000160108e   3c                   endif
222100160108      *
222200160108if  3c                   if        TGNpgn  <= wSavePGN
222300160107     c                   eval      *inKS    = *on
222400160108e   2c                   endif
222500160108      *
222600160108e   2c                   EndIf
222700040212      *
222800040212      * F18 - GESTIONE NOTE
222900040212      *
223000040212    2C                   IF        *INKS
223100040212      *
223200040212     c                   exsr      Sr_f18
223300040212      *
223400040219     c                   seton                                        39
223500040212     c                   eval      *in90 = *on
223600040212     c                   leave
223700040212      *
223800040212    2c                   ENDIF
223900150313
224000150313      * mi salvo l'indicatore 48 per posizionamento su campo note
224100150313     c                   eval      sav_in48 = *in48
224200150313      * spengo il 48
224300150313     c                   setoff                                       48
224400160104      *
224500160104      * -?Ripristino tasto funzionale F6?
224600160104     c                   eval      *inKF = $SaveF6
224700150311
224800150420      * Controllo la Filiale Gestione (SE gestita)
224900150428     c                   IF        wMod <> 'I'
225000150420      *
225100150421      * - Ricerca / Controllo
225200150421     c                   eval      wwcPOG = V1Cpog
225300150717     c*//?già impostata? eval      wwcMAD = V1Cmad
225400150421     c                   exsr      sr_CtrlPOG
225500150421     c                   eval      V1Cpog = wwcPOG
225600150421     c                   if        *in28  or  *in90
225700150421     c                   leave
225800150421     c                   endif
225900150420      *
226000150420     c                   ENDIF
226100040212      *
226200040323      * verifico l'inserimento dell'UTENTE che ha in GESTIONE il reclamo
226300150421      * (SE gestito)
226400150622     c                   clear                   $Reparto
226500150622      *
226600150428     c                   IF        wMod <> 'I'
226700150423      *
226800150422    2c                   If        V1Crpg <> *blanks
226900150421      *
227000150428      /free
227100150429
227200150429             // -?Ricerca/Controllo Reparto Gestione?
227300150617             //  ?(NON della Sede: per Sede si possono inserire solo Utenti)?
227400150617             If  V1Cpog <> c_Sede;
227500150617
227600150617               wwcRPR = V1Crpg;
227700150617               exsr  sr_CtrlRPR;
227800150617               V1Crpg = wwcRPR;
227900150617               //V1Drpg = wwdRPR;
228000150617
228100150617               if  *in90;
228200150617                 leave;
228300150617               endif;
228400150617
228500150617               $Reparto = ( Not *in28 );
228600150617
228700150617               *in28 = *off;
228800150617               *in39 = *off;
228900150617               *in45 = *off;
229000150617               clear  V1Cmsg;
229100150617
229200150617             EndIf;
229300150429
229400150429      /end-free
229500150421      *
229600150429if  3c                   IF        Not $Reparto
229700150421      *
229800150421      * 2° controllo come Utente Gestione
229900150421      *
230000040420      * verifico se esiste utente nell'archivio profili
230100040420     c     v1crpg        chain     azute01l
230200040420      *
230300150428    5c                   if        %found(azute01l) and UTEdsc > DateU
230400150311      * se lo trovo verifico se è della filiale di gestione
230500150723    6c                   if        %editc(UTEfil:'X') <> V1Cpog
230600040420     c                   eval      *in28 = *on
230700150313     c                   eval      *in46 = *on
230800040420     c                   movel     MSG(23)       V1Cmsg
230900040420     c                   leave
231000040420    6c                   endif
231100160607      *
231200150423    5c                   else
231300160607      *
231400040420     c                   eval      *in28 = *on
231500040420     c                   eval      *in39 = *on
231600150617     c                   if        V1Cpog <> c_Sede
231700150429     c                   movel     Msg(20)       V1Cmsg
231800150617     c                   else
231900150617     c                   eval      V1Cmsg = %trimR( Msg(20) )
232000150617     c                                    + ': ammessi solo utenti di sede'
232100150617     c                   endif
232200040420     c                   leave
232300040420    5c                   endif
232400150421      *
232500150421e   3c                   ENDIF
232600160607      *
232700150422   x2c                   Else
232800150421      * se Reparto/Utente a blanks do errore
232900040422     c                   eval      *in28 = *on
233000040422     c                   eval      *in39 = *on
233100040422     c                   movel     MSG(29)       V1Cmsg
233200040422     c                   leave
233300150422    2C                   EndIf
233400150422      *
233500150422 e_1bc                   ENDIF
233600160207      *//
233700160207      *//* se arrivo qua vuol dire che non ho avuto errori sui campi
233800160207      *//* filiale e utente gestione
233900160207      *//* quindi ripristino l'indicatore 48
234000160207      *//* => meglio farlo alla fine della subroutine...
234100160207     c*//                eval      *in48 = sav_in48
234200160203      *
234300160203      /free
234400160203
234500160203         // -?Confronto tra Reparto/Filiale in Attesa Risposta?
234600160203         //            ?con Reparto/Filiale di Gestione?
234700160203         If  dTGDfar.§TGDatt = 'S'  and
234800160203             V1Cpog = V1CfilR       and  V1Crpg = V1CrepR;
234900160203           *in28  = *on;
235000160203           *in46  = *on;
235100160203           V1Cmsg = Msg(59);
235200160203           leave;
235300160203         EndIf;
235400160203
235500160203      /end-free
235600040527      *
235700040527      * se responsabilità vettore  e P.O. responsabile non valorizzato metto me stesso
235800040527      * se responsabilità no vettore  e P.O. responsabile deve essere =  a blank
235900040527      *
236000040527    2c                   if        v1crsp = 'V'  and (v1cpor = *blanks
236100040527     c                             or v1cpor = '000')
236200130513     c                   select
236300130513     c                   when      §MARffr = 'A'
236400130513     c                   select
236500130513     c                   when      *in05
236600130513     c                   move      TASlna        V1Cpor
236700130513     c                   when      $fnARB = *on
236800130513     c                   move      ARBlna        V1Cpor
236900130513     c                   other
237000130513     c                   move      BLPlna        V1Cpor
237100130513     c                   endsl
237200130513     c                   when      §MARffr = 'P'
237300130513     c                   select
237400130513     c                   when      *in05
237500130513     c                   move      TASlnp        V1Cpor
237600130513     c                   when      $fnARB = *on
237700130513     c                   move      ARBlnp        V1Cpor
237800130513     c                   other
237900130513     c                   move      BLPlnp        V1Cpor
238000130513     c                   endsl
238100130513     c                   other
238200040527     c                   move      v1cpot        v1cpor
238300130513     c                   endsl
238400040527    2c                   endif
238500040527
238600040527    2c                   if        v1crsp <> 'V'
238700040527     c                   clear                   v1cpor
238800040527     c                   clear                   v1dpor
238900040527    2c                   endif
239000040211      *
239100040212      * verifico l'inserimento del P.O. responsabile
239200040220      *
239300040220     C                   clear                   v1dpor
239400040212    2c                   If        v1cpor <> *blanks
239500040212      *
239600040212      * Ricerca
239700040212     c     '?'           scan      v1cpor
239800040212    3c                   if        %found
239900040212     C                   clear                   dstnsd
240000040212     C                   call      'TNSD24R'
240100040212     C                   parm                    §cod
240200040212     C                   parm                    §tip
240300040212     C                   parm                    §des
240400040212     C                   parm                    §pra
240500040212     C                   movel     §cod          v1cpor
240600040212     C                   movel     §des          v1dpor
240700040212     c                   eval      *in90 = *on
240800040219     C                   eval      *IN40 = *on
240900040212     c                   leave
241000040212    3C                   endif
241100040217    2C                   endif
241200040217      *
241300040217     C     v1cpor        ifeq      *blanks
241400040217     C                   move      *all'0'       v1cpor
241500040217     C                   endif
241600040212      *
241700040212     C                   testn                   v1cpor               79
241800040212     C                   move      v1cpor        w001a             1
241900040217      *
242000040217     c                   move      v1cpor        W_por
242100040212      *
242200040217    2C     *in79         ifeq      *off
242300040212     C     w001a         orlt      '0'
242400040212     c                   eval      *in28 = *on
242500040212     c                   movel     MSG(9)        V1Cmsg
242600040212     C                   eval      *IN40 = *on
242700040212     c                   leave
242800040217    2C                   endif
242900040212      * verifico se P.O. esistente
243000040217    2c                   if        W_por > 0
243100160104     C     W_por         chain     azorg
243200040212    3C                   if        %found(azorg01l)
243300040212     C                   movel     orgdes        v1dpor
243400040420      * verifico se P.O. valido
243500040617     c                   if        orgfva<>' ' or (orgfag<>'F' and orgfag<>'A'
243600040617     c                             and orgfil <> 46)
243700040420     c                   eval      *in28 = *on
243800040420     c                   movel     MSG(9)        V1Cmsg
243900040420     C                   eval      *IN40 = *on
244000040420     c                   leave
244100040420     c                   endif
244200040212      *  errore
244300040212     C                   ELSE
244400040212     c                   eval      *in28 = *on
244500040213     c                   movel     MSG(9)        V1Cmsg
244600040212     C                   eval      *IN40 = *on
244700040212     c                   leave
244800040212    3C                   ENDIF
244900040212      *
245000040217    2C                   ENDIF
245100070608
245200070626      * controllo causale variazione per bolle se esiste  e se bolla valida per la variazione
245300070608
245400070608     c                   clear                   fnlr66ds
245500070608    2c                   if        V1ccva  <> *blanks
245600070626      * per 09 acceso emetto un msg di errore
245700160208     c* permetto SOLO in arrivo la modofica delle bolle non ancora borderizzate
245800160208    3c**                 If        *in09
245900160208     c**                 eval      *in28 = *on
246000160208     c**                 movel     MSG(34)       V1Cmsg
246100160208     C**                 eval      *IN47 = *on
246200160208     c**                 leave
246300160208    3c**                 endif
246400160208     c**
246500070608     c                   movel     V1anns        d66aas
246600070608     c                   movel     V1lnps        d66lnp
246700070608     c                   movel     V1nrss        d66nrs
246800070608     c                   movel     V1nsps        d66nsp
246900071018      * determino se sono il p.o. di partenza o il p.o. di arrivo
247000071018
247100160208      * se non trovata bolla in arb errore se la bolla in FNBLP è già partita
247200160208    3c******             If        $fnarb = *off
247300160208    3c                   If        $fnarb = *off and not *in09
247400071018     c                   eval      *in28 = *on
247500071018     c                   movel     MSG(35)       V1Cmsg
247600071018     C                   eval      *IN47 = *on
247700071018     c                   leave
247800071018    3c                   endif
247900071018
248000160208     c* Se la bolla deve ancora partire uso BLPLNA
248100160208     c                   if        *in09
248200160208     c                   move      blplna        w003a
248300160208     c                   else
248400130611     c                   move      arblna        w003a
248500160208     c                   endif
248600160208     c*
248700130611     c     w003a         lookup    wPOG                                   27
248800071018    3c                   if        %found
248900071018     c                   movel     'A'           d66tbo
249000071018   x3c                   else
249100071018      * verifico se sono la linea di partenza
249200160208     c                   if        *in09
249300160208     c                   move      blplnp        w003a
249400160208     c                   else
249500130611     c                   move      arblnp        w003a
249600160208     c                   endif
249700160208     c*
249800130611     c     w003a         lookup    wPOG                                   27
249900130611    3c                   if        %found
250000071018     c                   movel     'P'           d66tbo
250100071018    4c                   endif
250200071018
250300071018    3c                   endif
250400160208
250500160208    3c                   If        *in09  and d66tbo='P'
250600160208     c                   eval      *in28 = *on
250700160208     c                   movel     MSG(34)       V1Cmsg
250800160208     C                   eval      *IN47 = *on
250900160208     c                   leave
251000160208    3c                   endif
251100071018
251200071018      * se bolla non in gestione al Po Errore
251300071018    3c                   if        d66tbo = ' '
251400071018     c                   eval      *in28 = *on
251500071018     c                   movel     MSG(36)       V1Cmsg
251600071018     C                   eval      *IN47 = *on
251700071018     c                   leave
251800071018    3c                   endif
251900070608      *
252000070608     c     '?'           scan      v1ccva                                 90
252100070608      * Ricerca causale
252200070608    3c                   if        *in90
252300070608     c                   movel     'V'           d66ges
252400070608      *
252500070608     c                   movel     fnlr66ds      kpjbu
252600070608     c                   call      'FNLR66R'
252700070608     c                   parm                    kpjba
252800070608     c                   movel     kpjbu         fnlr66ds
252900070608     c                   movel     d66cvb        V1ccva
253000070608     c                   movel     d66des        v1dcva
253100070608   X3c                   else
253200070608      * Controllo causale inserita
253300070608     c                   movel     'C'           d66ges
253400160208     c* Posso variare anche le bolle in partenza per alcune causali
253500160208     c                   movel     'S'           d66BNP
253600070608     c                   movel     V1ccva        d66cvb
253700070608      *
253800070608     c                   movel     fnlr66ds      kpjbu
253900070608     c                   call      'FNLR66R'
254000070608     c                   parm                    kpjba
254100070608     c                   movel     kpjbu         fnlr66ds
254200070608     c                   movel     d66cvb        v1ccva
254300070608     c                   MOVEL     d66des        v1dcva
254400070608      *
254500070608      * Errore
254600070608    4c                   if        d66msg <> *blank
254700070608     c                   eval      *in28 = *on
254800070608     c                   movel     MSG(34)       V1Cmsg
254900070608     C                   eval      *IN47 = *on
255000070608     c                   leave
255100070608    4c                   endif
255200070608
255300070608    3c                   endif
255400070608      *
255500070608      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
255600070608      *
255700070608     c                   clear                   fnlr48ds
255800070608     c                   movel     V1anns        d48aas
255900070608     c                   movel     V1lnps        d48lnp
256000141029     c                   movel     V1nrss        d48nrs
256100070608     c                   movel     V1nsps        d48nsp
256200070608     c                   movel     Dutpou        d48fgs
256300070608     c                   If        $fnarb = *on
256400070608     c                   movel     arbcbo        d48cbo
256500070608     c                   else
256600070608     c                   movel     blpcbo        d48cbo
256700070608     c                   endif
256800071018     c                   eval      d48tbo = d66tbo
256900070608     c                   movel     v1ccva        d48cvb
257000070608     c                   movel     'S'           d48f12
257100140429     c                   movel     'V'           d48ffr
257200140429      *
257300140429     c                   clear                   dsARBd
257400111025     c
257500070608     c                   movel     fnlr48ds      kpjbu
257600070608     c                   call      'FNLR48R'
257700070608     c                   parm                    KPJBA
257800070608     c                   parm                    DSARBD
257900070608     c                   parm                    DSARBK
258000070608     c                   parm                    DARBT
258100070608     c                   parm                    DSARBG
258200070608     c                   parm                    TRUL90DS
258300070608      *
258400070608     c                   movel     kpjbu         fnlr48ds
258500070608     C* errore
258600070608     C                   if        d48err = '1'
258700070608     c                   eval      *in28 = *on
258800070608     c                   movel     MSG(33)       V1Cmsg
258900070608     C                   eval      *IN47 = *on
259000070608     c                   leave
259100070608     c                   else
259200070608     c                   clear                   v1ccva
259300070608     c                   clear                   v1dcva
259400070608     c                   endif
259500160209      *
259600160209     c* Se no errore e no F12 --> non permetto l'uscita con F3
259700160209     c                   if        d48err = ' ' and d48f12 <> '1'
259800160209     c                   eval      $BloccoF3 = *on
259900160209     c                   eval      *in63     = $BloccoF3
260000160209     c                   endif
260100070608     C*
260200070608    2C                   endif
260300160207      *
260400160207      * se arrivo qua vuol dire che non ho avuto errori su
260500160207      * nessun campo, quindi ripristino l'indicatore 48
260600160207      * per posizionare (forse) il cursore sulle note.
260700160207     c                   eval      *in48 = sav_in48
260800160205      *
260900160205      /free
261000160205
261100160205         // -?Immissione R.A. non chiusa: emissione W04?
261200160205         IF  wMod = 'I'  and  V1Ccdt = *blank  and  Not $GesW04;
261300160205
261400160205           exsr  sr_GesW04;
261500160205
261600160205           // -?F12 = Ritorno?
261700160205           if  *inKL;
261800160205             *in90 = *on;
261900160205           endif;
262000160205           if  *in90;
262100160205             leave;
262200160205           endif;
262300160205
262400160205         ENDIF;
262500160205
262600160205      /end-free
262700160205      *
262800070330      * se è andato tutto ok ed è stato dato F6  confermo richiamo
262900070330    2c                   If        *inkf and *in61 and v1cdcr > 0
263000070330     c                   clear                   fnlr48ds
263100070330    3c                   if        arbtc1 = 'S' or arbtc2 = 'S'
263200070330     c                   eval      d48cvb='CS'
263300070330     c                   eval      d48ffr='V'
263400070330     c                   eval      d48f12='S'
263500070330     c                   else
263600070330     c                   eval      d48cvb='CR'
263700070330     c                   eval      d48ffr='E'
263800070330     c                   eval      d48err='Z'
263900070330    3c                   endif
264000070330     c                   z-add     v1anns        D48AAS
264100070330     c                   z-add     v1lnps        D48LNP
264200070330     c                   z-add     v1nrss        D48NRS
264300070330     c                   z-add     v1nsps        D48NSP
264400070330     c                   MOVEL     ARBCBO        D48CBO
264500070330     c                   MOVEL     'A'           D48TBO
264600070330     c                   MOVEL     'S'           D48F12
264700070330     c                   MOVEL     'G'           D48trc
264800070330     c                   MOVEL     fnlr48ds      KPJBU
264900140429     c                   clear                   dsARBd
265000070330     c                   movel     v1ctcr        §BGTCR
265100141029     c                   z-add     v1cocr        §BGHCR
265200070330     c                   z-add     wdcr          §BGDCR
265300070330     c                   movel     arbgc1        §BGGC1
265400070330     c                   movel     arbgc2        §BGGC2
265500070330     c                   movel     arbtc1        §BGTC1
265600070330     c                   movel     arbtc2        §BGTC2
265700070330     c                   call      'FNLR48R'
265800070330     c                   PARM                    KPJBA
265900070330     c                   PARM                    DSARBD
266000070330     c                   PARM                    DSARBK
266100070330     c                   PARM                    DARBT
266200070330     c                   PARM                    DSARBG
266300070330     c                   PARM                    trul90ds
266400070330     c                   MOVEL     KPJBU         fnlr48ds
266500070330      * Ritorno con errore o premuto f12
266600070330    3c                   if        d48err<>*blanks and d48err <> 'Z'
266700160207     c                   eval      *in48 = *off
266800070330     c                   eval      *IN28 = *on
266900070330     c                   eval      *IN62 = *on
267000070330     c                   movel     d48msg        v1cmsg
267100070330     c                   leave
267200070330    3c                   endif
267300070330      * se premuto f12 nulla
267400070330    3c                   if        d48f12='1'
267500070330     c                   eval      *IN90 = *on
267600070330     c                   leave
267700070330    3c                   endif
267800070330
267900070330      * preparo il campo fisso per le modifiche effettuate
268000070330     c                   If        d48err = 'Z' and d48msg <> *blanks
268100070330     c                   eval      not_dcr = 'S'
268200070330     c                   movel     d48msg        note_dcr
268300070330      * preparo un campo note
268400070330     c                   endif
268500160209      * Se no errore e no F12 --> non permetto l'uscita con F3
268600160209     c                   if        D48err = ' ' and D48f12 <> '1'
268700160209     c                   eval      $BloccoF3 = *on
268800160209     c                   eval      *in63     = $BloccoF3
268900160209     c                   endif
269000070330      * valorizzo il flag che mi guida all'inserimento delle note ldv
269100070330     c                   eval      not_ldv = 'S'
269200070330     c                   seton                                        60
269300070330    2c                   endif
269400140429      *
269500140429      /free
269600150421
269700160204         // -?Conferma modifica R.A. non chiusa con altra Filiale Gestione:?
269800160204         //  ?· emissione W04N?
269900160204         //  ?· note obbligatorie SE richiesta Risposta?
270000160204         clear  $GesW04N;
270100160204         clear  $Video;
270200160204
270300160205         IF  *inKF   =  *on    and
270400160205             wMod    <> 'I'    and  V1Ccdt =  *blank  and
270500160205             ( dTGDfar.§TGDatt = *blank   or  dTGDfar.§TGDris = 'S' )
270600160205                               and  (
270700160205             ( w_POt >  *zero  and  V1Cpog <> %editc( w_POt  : 'X' ) )  or
270800160205             ( w_POt <= *zero  and  V1Cpog <> %editc( DUTpou : 'X' ) )  or
270900160205             V1Crpg  <> KNMUS       );
271000160205
271100160205           DoW  Not  $GesW04N;
271200160205
271300160204             exsr  sr_GesW04N;
271400160205
271500160204             // -?F12 = Ritorno?
271600160204             if  *inKL;
271700160204               *in90 = *on;
271800160204             endif;
271900160204             if  *in90;
272000160204               leave;
272100160204             endif;
272200160205
272300160204             *inKF = *on;
272400160205
272500160205           EndDo;
272600160205
272700160205         ENDIF;
272800140429
272900140429         // -?Conferma R.A. con Motivo Apertura " 17"?
273000140429         //  ?(Fermo Deposito autorizza mittente)?
273100140430         //  ?- SE non si è in Sede ma si è in Arrivo -?
273200140506         //  ?- SE non si è in modifica (*in50/70/72/74) -?
273300140430         //  ?=> Variazione Bolle (solo Arrivo)?
273400140506         IF  *inKF  and  $FNARB  and  V1Cmad = c_FDEAM
273500140516                    and  Not *in50
273600140516                    and  Not *in70  and  Not *in72  and  Not *in74;
273700140506
273800140506           $RA_MA_17 = *on;
273900140429
274000140429           clear  dsARBd;
274100140429           clear  dsARBk;
274200140429           clear  dARBt;
274300140429           clear  dsARBg;
274400140429           clear  FNLR48ds;
274500140429
274600140429           D48aas = V1annS;
274700140429           D48lnp = V1lnpS;
274800140429           D48nrs = V1nrsS;
274900140429           D48nsp = V1nspS;
275000140430           D48cbo = ARBcbo;
275100140429           D48cvb = c_CauVarB_I7;
275200140429           D48trc = 'D';
275300140429           D48ffr = 'E';
275400140430           D48tbo = 'A';
275500140429
275600140430           §BDrsd = ARBrsd;
275700140430           §BDind = ARBind;
275800140430           §BDcad = ARBcad;
275900140430           §BDlod = ARBlod;
276000140430           §BDprd = ARBprd;
276100140430           §BDnzd = ARBnzd;
276200140430           §BDfin = ARBfin;
276300140506           §BDffd = 'S';
276400140430           §BDcpi = ARBcpi;
276500140506
276600140506           // -?Reperimento ragione sociale estesa del destinatario?
276700140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'D' )  FIAR4000;
276800140506           if  %found(FIAR401L);
276900140506             §BDrd2 = AR4not;
277000140506           endif;
277100140506
277200140506           // -?Reperimento referente e telefono del destinatario?
277300140506           clear  dAR5gen;
277400140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'GEN' )  FIAR5000;
277500140506           if  %found(FIAR501L)  and  AR5atb = *blank;
277600140506             dAR5gen = AR5uni;
277700140506             §BDref = §AR5ref;
277800140506             §BDtel = §AR5teld;
277900140506           endif;
278000140429
278100140429           kpjbu = FNLR48ds;
278200140429           fnlr48r ( kpjba :
278300140429                     dsARBd :
278400140429                     dsARBk :
278500140429                     dARBt :
278600140429                     dsARBg :
278700140429                     TRUL90ds );
278800140429           FNLR48ds = kpjbu;
278900140429
279000140429           select;
279100140429             //?- se rilevato errore => nulla?
279200140429             when  D48err <> *blank;
279300140429               *in28  = *on;
279400140429               *in62  = *on;
279500140429               V1Cmsg = D48msg;
279600160207               leave;
279700140429             //?- se premuto F12 => nulla?
279800160209             //?  (F12 non abilitato)?
279900160209             //when  D48f12 = *on;
280000160209             //  *in90  = *on;
280100160209             //  leave;
280200160209             //?- se NON premuto F12 => disabilitazione F3?
280300160209             when  D48err = *blank  and  D48f12 <> *on;
280400160209               $BloccoF3  = *on;
280500160209               *in63      = $BloccoF3;
280600140429           endsl;
280700140429
280800140429         ENDIF;
280900141210
281000141210         // -?Conferma R.A. con Motivo Apertura " 60"?
281100141210         //  ?(Verifica telefonica "PINcode")?
281200141210         //  ?- SE non si è in Sede ma si è in Arrivo -?
281300141210         //  ?- SE non si è in modifica (*in50/70/72/74) -?
281400141210         //  ?=> Richiesta BRTcode + PINcode?
281500141210         //  ?=> DOPO: Registrazione Esito nelle Note?
281600141210         IF  *inKF  and  $FNARB  and  V1Cmad = c_VPINC
281700141210                    and  Not *in50
281800141210                    and  Not *in70  and  Not *in72  and  Not *in74;
281900141210
282000141210           // -?Richiesta / Controllo "PINcode"?
282100141210           clear  trulPINds;
282200141210           iPINaas = V1annS;
282300141210           iPINlnp = V1lnpS;
282400141210           iPINnrs = V1nrsS;
282500141210           iPINnsp = V1nspS;
282600141210
282700141210           trulPINr ( trulPINds );
282800141212
282900141216           if  oPINfxx = '03'  and  oPINnre = *zero;
283000141216             *inKF = *off;
283100141216             *in90 = *on;
283200141216             exsr  sr_PulCtl2;
283300141216           else;
283400141216             $RA_MA_60 = *on;
283500141216             // -?N.B. - L'esito verrà registrato DOPO nelle NOTE?
283600141216           endif;
283700141210
283800141210         ENDIF;
283900150114
284000150114
284100150114         //?Posizionamento curosre sulle Note - SE Inserim. & NO errori?
284200150114         if  wMod = 'I'        and
284300150114             Not *in28         and  Not *in90                    and
284400150114             ( ( V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero )    or
284500150324               ( V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero ) );
284600150114           *in48 = *on;
284700150114         endif;
284800140429
284900140429       /end-free
285000040422      *
285100040217     c                   leave
285200040212      *
285300040211    1C                   endDO
285400040211      *
285500040211     c                   ENDSR
285600150112      *
285700150112      *---------------------------------------------------------------*
285800150114      *?Routine di Impostazione automatica di alcuni campi a video   ?*
285900150114      *?  - SE INSERIMENTO -                                         ?*
286000150112      *---------------------------------------------------------------*
286100150114     c     sr_ImpAut_Ins BEGSR
286200150112      *
286300150113sel 1c                   Select
286400150113      *
286500150112      * -?Impostazione campi (se vuoti) per Spedizione?
286600150113w   1c                   When      V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero
286700150112      * - -?Chi Chiama => Destinatario?
286800150112if  4c                   if        V1Ctch = *blank
286900150112     c                   eval      V1Ctch = 'D'
287000150112     c                   exsr      sr_Ctrl_CCR
287100150112e   4c                   endif
287200150202      * - -?Motivo Apertura => Consegna +?
287300150112if  4c                   if        V1Cmad = *blank
287400150112     c                   eval      V1Cmad = '  1'
287500150202     c                   clear                   wSaveMAD
287600150112e   4c                   endif
287700150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
287800150202if  4c                   if        V1Cmad <> wSaveMAD
287900150202     c                   eval      wSaveMAD = V1Cmad
288000150202     c                   exsr      sr_Ctrl_MAR
288100150202     c                   eval      V1Ccdt   = §MARcauChP
288200150202e   4c                   endif
288300150114      * - -?Posizionamento cursore sulle Note?
288400150114     c                   eval      *in48 = *on
288500150112      *
288600150112      * -?Impostazione campi (se vuoti) per O.R.M.?
288700150113w   1c                   When      V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero
288800150112      * - -?Chi Chiama => Mittente?
288900150112if  4c                   if        V1Ctch = *blank
289000150113     c                   eval      V1Ctch = 'M'
289100150112     c                   exsr      sr_Ctrl_CCR
289200150112e   4c                   endif
289300150112      * - -?Motivo Apertura => Ritiro?
289400150112if  4c                   if        V1Cmad = *blank
289500150112     c                   eval      V1Cmad = '  3'
289600150202     c                   clear                   wSaveMAD
289700150112e   4c                   endif
289800150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
289900150202if  4c                   if        V1Cmad <> wSaveMAD
290000150202     c                   eval      wSaveMAD = V1Cmad
290100150202     c                   exsr      sr_Ctrl_MAR
290200150202     c                   eval      V1Ccdt   = §MARcauChP
290300150202e   4c                   endif
290400150114      * - -?Posizionamento cursore sulle Note?
290500150114     c                   eval      *in48 = *on
290600150407      *
290700150407      * -?Impostazione campi (se vuoti) per Generico?
290800150407w   1c                   When      V1Ctor = 'G'
290900150407      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
291000150407if  4c                   if        V1Cmad <> wSaveMAD
291100150407     c                   eval      wSaveMAD = V1Cmad
291200150407     c                   exsr      sr_Ctrl_MAR
291300150407     c                   eval      V1Ccdt   = §MARcauChP
291400150407e   4c                   endif
291500150112      *
291600150113e   1c                   EndSl
291700150313
291800150625      * - -?STATO (ex Priorità):?
291900150625      *    ?Se viene impostata la causale chiusura metto priorità "9"?
292000150625      *    ?(in inserimento) o "8" (in modifica - SE modificata la?
292100150625      *    ?Causale Chiusura)?
292200150313     c                   IF        V1Ccdt <> *blanks
292300150402     c                   IF        wmod = 'I'
292400150625      *   -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
292500150625      *    ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
292600150625      *    ?(già reperita nella subr. SR_CONTR01)?
292700150625     c                   if        §CORstato <> *blanks
292800150625     c                   eval      V1Cprt = §CORstato
292900150625     c                   else
293000150313     c                   eval      V1Cprt = '9'
293100150625     c                   endif
293200150324     c                   ELSE
293300150324     c                   eval      V1Cprt = '8'
293400150324     c                   ENDIF
293500150313     c                   ENDIF
293600150320      *
293700150320      * Decodifico (potrebbe essere cambiato)
293800150320     c                   clear                   tibs02ds
293900150320     C                   MOVEL     'C'           t02mod
294000150320     C                   MOVEL     knsif         t02sif
294100150320     C                   MOVEL     'PRR'         t02cod
294200150320     C                   MOVEL     v1cprt        t02ke1
294300150320      *
294400150320     C                   CALL      'TIBS02R'
294500150320     C                   PARM                    KPJBA
294600150320     C                   PARM                    TIBS02DS
294700150320      *
294800150320    4C                   IF        t02err = *BLANKS
294900150320     C                   movel     T02uni        DPRR
295000150320     C                   movel     §PRRdesc      v1dprt
295100150320     c                   endif
295200150112      *
295300150112     c                   ENDSR
295400150112      *
295500150112      *---------------------------------------------------------------*
295600150112      * Routine controllo tabella "MAR" = Motivo Apertura             *
295700150112      *---------------------------------------------------------------*
295800150112     c     sr_Ctrl_MAR   BEGSR
295900150323
296000150323      * Ricerca
296100150323    3c                   if        %scan('?':V1Cmad) > 0
296200150422     c                             and       $Video <> 'W4'
296300150323     c                   clear                   tntbMARds1
296400150323      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
296500150323      *      dall'utente in APERTURA;
296600150323      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
296700150323      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
296800150323      *      in GESTIONE.
296900150323     c                   if        wMod <> 'I'
297000150323     c                   eval      iMARutUt = 'I'
297100150323     c                   endif
297200150323     c                   eval      iMARutAp = 'I'
297300150323     c                   movel(p)  tntbMARds1    KPJBU
297400150323      *
297500150323     c                   call      'TNTBMARR1'
297600150323     c                   parm                    KPJBA
297700150323     c                   movel     KPJBU         tntbMARds1
297800150323    4c                   if        oMARfxx = *blank  and  oMARerr = *off
297900150323     c                   eval      V1Cmad  = oMARcod
298000150323     C                   eval      V1Dmad  = oMARdes
298100150323      *  errore (non trovato nulla pulisco il '?' )
298200150323     C                   ELSE
298300150323     c                   clear                   V1Cmad
298400150323     c                   clear                   V1Dmad
298500150323    4C                   ENDIF
298600150422    3C                   ENDIF
298700150112      *
298800150112     c                   clear                   tibs02ds
298900150112     C                   MOVEL     'C'           t02mod
299000150112     C                   MOVEL     knsif         t02sif
299100150112     C                   MOVEL     'MAR'         t02cod
299200150422     c                   if        $Video = 'W4'
299300150422     c                   eval      T02ke1 = wwcMAD
299400150422     c                   else
299500150112     C                   MOVEL     v1cmad        t02ke1
299600150422     c                   endif
299700150112      *
299800150112     C                   CALL      'TIBS02R'
299900150112     C                   PARM                    KPJBA
300000150112     C                   PARM                    TIBS02DS
300100150112      *
300200150112    4C                   IF        t02err = *BLANKS
300300150112     C                   movel     T02uni        DMAR
300400150422     c                   if        $Video = 'W4'
300500150422     c                   eval      wwdMAD = §MARdesc
300600150422     c                   else
300700150112     C                   movel     §MARdesc      v1dmad
300800150423     c                   eval      wwdMAD = §MARdesc
300900150422     c                   endif
301000150112     c                   if        (*in74 = *off  and  §MARutAp = 'N')  or
301100150112     c                             (wMod <> 'I'   and  §MARutUt = 'N')
301200150112     c                   eval      *in28 = *on
301300150112     c                   eval      *in36 = *on
301400150112     c                   movel     msg(18)       V1Cmsg
301500150112     c                   leavesr
301600150112     c                   endif
301700150112      *
301800150112     C                   ELSE
301900150112     c                   eval      *in28 = *on
302000150112     c                   movel     msg(18)       V1Cmsg
302100150112     C                   eval      *IN36 = *on
302200150112     c                   leavesr
302300150112    4C                   ENDIF
302400150112      *
302500150112     c                   ENDSR
302600150112      *
302700150112      *---------------------------------------------------------------*
302800150112      * Routine controllo tabella "CCR" = Chi Chiama                  *
302900150112      *---------------------------------------------------------------*
303000150112     c     sr_Ctrl_CCR   BEGSR
303100150112      *
303200150112     c                   clear                   tibs02ds
303300150112     C                   MOVEL     'C'           t02mod
303400150112     C                   MOVEL     knsif         t02sif
303500150112     C                   MOVEL     'CCR'         t02cod
303600150112     C                   MOVEL     v1ctch        t02ke1
303700150112      *
303800150112     C                   CALL      'TIBS02R'
303900150112     C                   PARM                    KPJBA
304000150112     C                   PARM                    TIBS02DS
304100150112      *
304200150112    4C                   IF        t02err = *BLANKS
304300150112     C                   movel     T02uni        DCCR
304400150112     C                   movel     T02ke1        v1ctch
304500150112     c                   movel     §ccrdesc      v1dtch
304600150112      *
304700150112     C                   ELSE
304800150112     C                   eval      *IN28 = *on
304900150112     c                   movel     msg(07)       V1Cmsg
305000150112     C                   eval      *IN35 = *on
305100150112     c                   leavesr
305200150112    4C                   ENDIF
305300150112      *
305400150112     c                   ENDSR
305500070323      *
305600140429      /free
305700150421
305800150421       //--------------------------------------------------------------
305900150421       //?Gestione window W4 per immissione R.A. non chiusa            ?
306000150421       //--------------------------------------------------------------
306100150421       BEGSR  sr_GesW04;
306200150421
306300150423         $Video   = 'W4';
306400150421
306500150421         //clear  FIA1W04;  // -?NO: serve sapere se sono già stati?
306600150421                            //  ?    inseriti dati?
306700160120
306800160204         // -?Impostazione flag "Attesa Risposta"?
306900160120         $AttesaRisposta = *off;
307000150616
307100150915         // -?Impostazione gestione Reparto?
307200150915         $Reparto = ( V1Cpog <> c_Sede  or  V1Cmad = c_ReclSede );
307300150421
307400150421         W4nTel = V1nTel;
307500150421         W4mail = V1mail;
307600150421         W4cMad = V1cMad;
307700150915         clear  W4cRpr;
307800150915         clear  W4cPog;
307900151211         clear  W4cCap;
308000160607         // -?Reparto/Utente gestione?
308100150720         select;
308200150915           // -?Inserimento Reclamo da Sede (in Sede, motivo " 70"):?
308300150915           //  ?proposto reparto?
308400150720           when  *in05  and  W4cMad = c_ReclSede;
308500150915             W4cRpr = 'RAC';
308600150720           // -?Proposto Utente, non Reparto (in Sede)?
308700150720           when  Not $Reparto;
308800150720             W4cRpr = KNMUS;
308900150720           // -?Proposto Reparto (in Filiale)?
309000150720           other;
309100150720             W4cRpr = 'RAC';
309200150720         endsl;
309300160607         // -?Filiale gestione?
309400150720         if  Not *in05  or  W4cMad <> c_ReclSede;
309500160607           select;
309600160607             // -?LNA della Spedizione?
309700160607             when  V1Ctor = 'S';
309800160607               if  $FNARB;
309900160607                 W4cPog = %editc( ARBlna : 'X' );
310000160607               else;
310100160607                 W4cPog = %editc( BLPlna : 'X' );
310200160607               endif;
310300160607             // -?Filiale Ritiro dell'O.R.M.?
310400160607             when  V1Ctor = 'O';
310500160607               W4cPog = %editc( ORMpor : 'X' );
310600160607             // -?Filiale Utente altrimenti?
310700160607             other;
310800160607               W4cPog = V1Cpog;
310900160607           endsl;
311000150720         endif;
311100160128         // -?Attesa Risposta?
311200151223         Select;
311300151223           When  *in05;
311400151223             W4tgdATT = 'N';
311500151223           When  V1CfilR > *zero  and  V1CrepR <> *blank;
311600151223             W4tgdATT = 'S';
311700151223             W4repR = V1CrepR;
311800151223             W4filR = V1CfilR;
311900151223           Other;
312000151223             W4tgdATT = 'N';
312100151223             W4repR = 'RAC';
312200160129             if  w_POt = *zero;
312300160129               W4filR = %editc( DUTpou : 'X' );
312400160129             else;
312500160129               W4filR = %editc( w_POt : 'X' );
312600160129             endif;
312700151223         EndSl;
312800150428
312900150428         // -?Decodifiche dati immessi o proposti?
313000150428         wwcMad = W4cMad;
313100150428         exsr  sr_CtrlMAD;
313200150428         W4cMad = wwcMad;
313300150428         W4dMad = wwdMad;
313400150428         wwcRPR = W4cRpr;
313500150616         if  Not $Reparto;
313600150616           wwcPog = W4cPog;
313700150616           exsr  sr_CtrlRPG;
313800150616         else;
313900150616           exsr  sr_CtrlRPR;
314000150616         endif;
314100150428         W4cRpr = wwcRPR;
314200150428         W4dRpr = wwdRPR;
314300150717         //wwcMad = W4cMad;   ?(già impostata prima)?
314400150428         wwcPog = W4cPog;
314500150428         exsr  sr_CtrlPOG;
314600150428         W4cPog = wwcPog;
314700150428         W4dPog = wwdPog;
314800151211         wwcRPR  = W4repR;
314900151211         exsr  sr_CtrlRPR;
315000151211         W4repR  = wwcRPR;
315100151211         W4repRd = wwdRPR;
315200151211         wwcPog  = W4filR;
315300151211         exsr  sr_CtrlPOG;
315400151211         W4filR  = wwcPog;
315500151211         W4filRd = wwdPog;
315600150428
315700150428         *in28 = *off;
315800150428         *in90 = *off;
315900150428         *in36 = *on;     // -?Posiz. cursore su Motivo?
316000150428         *in38 = *off;    // -?Posiz. cursore su Mail?
316100150428         *in45 = *off;    // -?Posiz. cursore su Reparto Gestione?
316200150428         *in46 = *off;    // -?Posiz. cursore su Filiale Gestione?
316300150428         *in55 = *off;    // -?Posiz. cursore su Telefono?
316400150911         *in56 = *off;    // -?Posiz. cursore su C.A.P. cliente?
316500151211         *in57 = *off;    // -?Posiz. cursore su Reparto Risposta?
316600151211         *in58 = *off;    // -?Posiz. cursore su Filiale Risposta?
316700150422
316800150423         DoU  *in28 = *off  and  *in90 = *off  and  *inKF;
316900150423
317000150423           exfmt  FIA1W04;
317100150423
317200150914           clear  V1Cmsg;
317300150423           *in28 = *off;
317400150423           *in90 = *off;
317500150423           *in36 = *off;       // -?Motivo errato?
317600150423           *in38 = *off;       // -?Mail   errata?
317700150423           *in45 = *off;       // -?Reparto Gestione errato?
317800150423           *in46 = *off;       // -?Filiale Gestione errata?
317900150423           *in55 = *off;       // -?Telefono obbligatorio?
318000150911           *in56 = *off;       // -?C.A.P. cliente   errato?
318100151211           *in57 = *off;       // -?Reparto Risposta errato?
318200151211           *in58 = *off;       // -?Filiale Risposta errata?
318300150423
318400150423           // -?F12 = Ritorno al fmt D01?
318500150423           if  *inKL;
318600150423             clear  $Video;
318700150423             leavesr;
318800150423           endif;
318900150423
319000150423           // -?Num. Telefono o Mail obbligatori?
319100150423           if  W4nTel = *blank  and  W4mail = *blank;
319200150723             *in28  = *on;
319300150423             *in55  = *on;
319400150423             V1Cmsg = Msg(49);
319500150423             iter;
319600150423           endif;
319700150423
319800150423           // -?Mail errata?
319900150423           if  W4mail <> *blank;
320000150423             wwMail = W4mail;
320100150423             exsr  sr_CtrlMail;
320200150423             W4mail = wwMail;
320300150723             if  *in28  or  *in90;
320400150423               iter;
320500150423             endif;
320600150423           endif;
320700150423
320800150423           // -?Controllo Motivo Apertura Reclamo?
320900150423           wwcMad = W4cMad;
321000150423           exsr  sr_CtrlMAD;
321100150423           W4cMad = wwcMad;
321200150423           W4dMad = wwdMad;
321300150723           if  *in28  or  *in90;
321400150423             iter;
321500150423           endif;
321600150723
321700150723           // -?SE Motivo = " 70" (quindi devo essere in Sede):?
321800150723           //  ?Controllo la Filiale Gestione PRIMA dell'Utente Gestione?
321900150723           If  W4cMad = c_ReclSede;
322000150911             // ·?La Filiale Gestione potrebbe voler essere ricavata?
322100150911             //  ?dal C.A.P. del cliente?
322200150911             if  W4cCap <> *blank;
322300150911               exsr  sr_CtrlCAP;
322400150914               if  *in28;
322500150914                 iter;
322600150914               endif;
322700150911             endif;
322800150723             wwcPog = W4cPog;
322900150723             //wwcMad = W4cMad; ?(già impostata prima)?
323000150723             exsr  sr_CtrlPOG;
323100150723             W4cPog = wwcPog;
323200150723             W4dPog = wwdPog;
323300150915             if  *in28;
323400150723               iter;
323500150723             endif;
323600150723             if  W4cPog = c_Sede;
323700150723               *in28 = *on;
323800150723               *in46 = *on;
323900150723               if  Not *in05;
324000150723                 V1Cmsg = Msg(48);
324100150723               else;
324200150723                 V1Cmsg = Msg(53);
324300150723               endif;
324400150723               iter;
324500150723             endif;
324600150723           EndIf;
324700151202
324800151202           // -?SE modificato Motivo in " 18" (Reso):?
324900151202           //  ?ritorno alla videata pecedente (la R.A. va chiusa)?
325000151202           If  W4cMad = c_MotAp_Reso;
325100151202             *in90 = *on;
325200151202             leave;
325300151202           EndIf;
325400150915
325500150915           // -?Re-impostazione gestione Reparto?
325600150915           $Reparto = ( W4cPog <> c_Sede  or  W4cMad = c_ReclSede );
325700150423
325800150617           // -?Controllo Reparto Gestione (in filiale)?
325900150617           //  ?Controllo Utente  Gestione (in sede)?
326000150429           wwcRpr = W4cRpr;
326100150616           if  Not $Reparto;
326200150616             wwcPog = W4cPog;
326300150616             exsr  sr_CtrlRPG;
326400150616           else;
326500150616             exsr  sr_CtrlRPR;
326600150616           endif;
326700150429           W4cRpr = wwcRpr;
326800150429           W4dRpr = wwdRpr;
326900150915           if  *in28;
327000150429             iter;
327100150429           endif;
327200150423
327300150723           // -?SE Motivo <> " 70":?
327400150723           //  ?Controllo Filiale Gestione   DOPO?
327500150723           //  ?il Reparto Gestione (in filiale)?
327600150723           //  ?o l'Utente  Gestione (in sede)?
327700150723           If  W4cMad <> c_ReclSede;
327800150723             wwcPog = W4cPog;
327900160120             //wwcMad = W4cMad;    ?(già impostata prima)?
328000150723             exsr  sr_CtrlPOG;
328100150723             W4cPog = wwcPog;
328200150723             W4dPog = wwdPog;
328300150915             if  *in28;
328400150723               iter;
328500150723             endif;
328600150723             if  Not *in05  and  W4cPog =  c_Sede;
328700150723               *in28 = *on;
328800150723               *in46 = *on;
328900150723               V1Cmsg = Msg(48);
329000150723               iter;
329100150723             endif;
329200150723           EndIf;
329300151211
329400160120           // -?Controllo Reparto che Attende Risposta?
329500151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
329600160121           //  ?- in Sede NON viene richiesto Reparto/Filiale... -?
329700160121           If  Not *in05  and  W4tgdATT = 'S';
329800151214             if  W4repR = *blank;
329900151214               W4repR   = 'RAC';
330000151214             endif;
330100151223             wwcRpr  = W4repR;
330200151214             exsr  sr_CtrlRPR;
330300151214             W4repR  = wwcRPR;
330400151214             W4repRd = wwdRPR;
330500151223             if  *in28  or  *in90;
330600151223               *in45 = *off;
330700151223               *in57 = *on;
330800151223               if  *in28;
330900151223                 V1Cmsg = Msg(57);
331000151223               endif;
331100151214               iter;
331200151214             endif;
331300151223           Else;
331400160121             W4tgdATT = 'N';
331500151223             clear  W4repR;
331600151223             clear  W4repRd;
331700151214           EndIf;
331800151211
331900160120           // -?Controllo Filiale che Attende Risposta?
332000151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
332100160121           //  ?- in Sede NON viene richiesto Reparto/Filiale... -?
332200151223           If  Not *in05  and  W4tgdATT = 'S';
332300151214             if  W4filR = *blank;
332400160129               if  w_POt = *zero;
332500160129                 W4filR  = %editc( DUTpou : 'X' );
332600160129               else;
332700160129                 W4filR  = %editc( w_POt : 'X' );
332800160129               endif;
332900151214             endif;
333000160121             $AttesaRisposta = *on;
333100160120             //wwcRpr  = W4repR;   ?(già impostata prima)?
333200151214             wwcPog  = W4filR;
333300151214             exsr  sr_CtrlPOG;
333400151214             W4filR  = wwcPog;
333500151214             W4filRd = wwdPog;
333600160121             $AttesaRisposta = *off;
333700151222             if  *in28  or  *in90;
333800151222               *in46 = *off;
333900151222               *in58 = *on;
334000151223               if  *in28;
334100151223                 V1Cmsg = Msg(58);
334200151223               endif;
334300151214               iter;
334400151214             endif;
334500160121             if  W4filR = c_Sede;
334600160121               *in28 = *on;
334700160121               *in58 = *on;
334800160121               V1Cmsg = Msg(48);
334900160121               iter;
335000160121             endif;
335100151223           Else;
335200160121             W4tgdATT = 'N';
335300151223             clear  W4filR;
335400151223             clear  W4filRd;
335500151214           EndIf;
335600151222
335700160128           // -?Confronto tra Reparto/Filiale in Attesa Risposta?
335800151222           //            ?con Reparto/Filiale di Gestione?
335900151223           //  ?(solo in Filiale - NON in sede - e se Attesa Risposta)?
336000151223           If  Not *in05          and
336100151223               W4tgdATT = 'S'     and
336200151223               W4repR   = W4Crpr  and
336300151223               W4filR   = W4Cpog;
336400151222             *in28 = *on;
336500151222             *in57 = *on;
336600151222             V1Cmsg = Msg(59);
336700151222             iter;
336800151222           endif;
336900150423
337000150423           *in36 = *on;     // -?Posiz. cursore su Motivo?
337100150422
337200150423         EndDo;
337300150421
337400150421         V1nTel = W4nTel;
337500150421         V1mail = W4mail;
337600150421         V1Cmad = W4cMad;
337700151202
337800151202         If  W4cMad <> c_MotAp_Reso;
337900151202
338000151202           V1Crpg = W4cRpr;
338100151202           V1Cpog = W4cPog;
338200151202
338300151202           $GesW04 = *on;
338400151202
338500151202         EndIf;
338600151223
338700151223         V1CfilR = W4filR;
338800151223         V1CrepR = W4repR;
338900151202
339000151202         clear  $Video;
339100150421
339200150421       ENDSR;
339300160128
339400160128       //--------------------------------------------------------------
339500160128       //?Gestione window W4M per nuova Attesa Risposta                ?
339600160128       //--------------------------------------------------------------
339700160128       BEGSR  sr_GesW04N;
339800160128
339900160204         IF  $Video <> 'M4';
340000160204
340100160204           $Video = 'M4';
340200160204           clear  FIA1W04N;
340300160204
340400160204           // -?Attesa Risposta di default?
340500160204           If    dTGDfar.§TGDatt = *blank  or
340600160204               ( dTGDfar.§TGDatt = 'S'    and
340700160204                 dTGDfar.§TGDris = 'S' );
340800160204             W4tgdATT = 'N';
340900160204             W4repR   = 'RAC';
341000160204             if  w_POt = *zero;
341100160204               W4filR   = %editc( DUTpou : 'X' );
341200160204             else;
341300160204               W4filR   = %editc( w_POt : 'X' );
341400160204             endif;
341500160204           Else;
341600160204             W4tgdATT = dTGDfar.§TGDatt;
341700160204             W4repR   = dTGR01A.§TGRrepR;
341800160204             W4filR   = %editc( dTGD01.§TGDfilR : 'X' );
341900160204           EndIf;
342000160204
342100160204           // -?Decodifiche dati immessi o proposti?
342200160204           wwcRPR  = W4repR;
342300160204           exsr  sr_CtrlRPR;
342400160204           W4repR  = wwcRPR;
342500160204           W4repRd = wwdRPR;
342600160204           $AttesaRisposta = *on;
342700160204           wwcPog  = W4filR;
342800160204           exsr  sr_CtrlPOG;
342900160204           W4filR  = wwcPog;
343000160204           W4filRd = wwdPog;
343100160204           $AttesaRisposta = *off;
343200160204
343300160204         ENDIF;
343400160128
343500160128         *in28 = *off;
343600160128         *in90 = *off;
343700160128         *in57 = *off;    // -?Posiz. cursore su Reparto Risposta?
343800160128         *in58 = *off;    // -?Posiz. cursore su Filiale Risposta?
343900160128
344000160128         DoU  *in28 = *off  and  *in90 = *off;
344100160128
344200160128           exfmt  FIA1W04N;
344300160128
344400160128           clear  V1Cmsg;
344500160128           *in28 = *off;
344600160128           *in90 = *off;
344700160128           *in57 = *off;       // -?Reparto Risposta errato?
344800160128           *in58 = *off;       // -?Filiale Risposta errata?
344900160128
345000160128           // -?F12 = Ritorno al fmt D01?
345100160128           if  *inKL;
345200160128             clear  $Video;
345300160207             W4tgdATT = 'N';
345400160128             leavesr;
345500160128           endif;
345600160128
345700160128           // -?NON Richiesta Risposta: uscita?
345800160128           //  ?(con aperta ed in gestione ad altro Rep./Fil.)?
345900160129           If  W4tgdATT = 'N';
346000160204             $GesW04N = *on;
346100160129             clear  W4repRd;
346200160128             clear  W4repR;
346300160129             clear  V1CrepR;
346400160129             clear  W4filRd;
346500160129             W4filR  = *zero;
346600160129             V1CfilR = *zero;
346700160128             clear  $Video;
346800160128             leavesr;
346900160128           EndIf;
347000160128
347100160128           // -?Controllo Reparto che Attende Risposta?
347200160128           if  W4repR = *blank;
347300160128             W4repR   = 'RAC';
347400160128           endif;
347500160128           wwcRpr  = W4repR;
347600160128           exsr  sr_CtrlRPR;
347700160128           W4repR  = wwcRPR;
347800160128           W4repRd = wwdRPR;
347900160128           if  *in28  or  *in90;
348000160128             *in45 = *off;
348100160128             *in57 = *on;
348200160128             if  *in28;
348300160128               V1Cmsg = Msg(57);
348400160128             endif;
348500160128             iter;
348600160128           endif;
348700160128
348800160128           // -?Controllo Filiale che Attende Risposta?
348900160129           if  W4filR  = *blank;
349000160129             if  w_POt = *zero;
349100160129               W4filR  = %editc( DUTpou : 'X' );
349200160129             else;
349300160129               W4filR  = %editc( w_POt : 'X' );
349400160129             endif;
349500160128           endif;
349600160128           $AttesaRisposta = *on;
349700160128           //wwcRpr  = W4repR;   ?(già impostata prima)?
349800160128           wwcPog  = W4filR;
349900160128           exsr  sr_CtrlPOG;
350000160128           W4filR  = wwcPog;
350100160128           W4filRd = wwdPog;
350200160128           $AttesaRisposta = *off;
350300160128           if  *in28  or  *in90;
350400160128             *in46 = *off;
350500160128             *in58 = *on;
350600160128             if  *in28;
350700160128               V1Cmsg = Msg(58);
350800160128             endif;
350900160128             iter;
351000160128           endif;
351100160128           if  W4filR = c_Sede;
351200160128             *in28 = *on;
351300160128             *in58 = *on;
351400160128             V1Cmsg = Msg(48);
351500160128             iter;
351600160128           endif;
351700160128
351800160128           // -?Confronto tra Reparto/Filiale in Attesa Risposta?
351900160128           //            ?con Reparto/Filiale di Gestione?
352000160128           If  W4repR   = V1Crpg  and
352100160128               W4filR   = V1Cpog;
352200160128             *in28 = *on;
352300160128             *in57 = *on;
352400160128             V1Cmsg = Msg(59);
352500160128             iter;
352600160128           EndIf;
352700160204
352800160204           // -?Reperimento ULTIMO Progressivo Note inserito?
352900160204           setGT   ( V1Cant : V1Cnut : V1Cpdt : KEYtin )  FITGN000;
353000160204           readPE  ( V1Cant : V1Cnut : V1Cpdt : KEYtin )  FITGN000;
353100160204           if  %eof(FITGN01L);
353200160204             clear  TGNpgn;
353300160204           endif;
353400160204           // -?Note obbligatorie?
353500160204           If  TGNpgn <= wSavePGN;
353600160204             exsr  sr_F18;
353700160204             *in90 = ( oB0fxx <> *blank );
353800160204           EndIf;
353900160128
354000160128         EndDo;
354100160129
354200160204         V1CfilR  = W4filR;
354300160204         V1CrepR  = W4repR;
354400160204         $GesW04N = *on;
354500160128
354600160128       ENDSR;
354700140429
354800140429       //--------------------------------------------------------------
354900140429       //?Controlli specifici per Motivo Apertura R.A. " 17"=F.Deposito?
355000140429       //--------------------------------------------------------------
355100140429       BEGSR  sr_CtrMotAp_17;
355200140430
355300140430         Select;
355400140430
355500140430           // -?Errore SE si è in Sede?
355600140430           When  *in05;
355700140430             *in28  = *on;
355800140430             *in36  = *on;
355900140430             V1Cmsg = Msg(42);
356000140430             leavesr;
356100140430
356200140430           // -?Errore SE la spedizione NON è in Arrivo?
356300140430           When  Not $FNARB;
356400140430             *in28  = *on;
356500140430             *in36  = *on;
356600140430             V1Cmsg = Msg(42);
356700140430             leavesr;
356800140430
356900140430           // -?Errore SE la linea di partenza NON è tra le filiali in?
357000140430           //  ?gestione all'utente?
357100140430           When  %lookup( %editc( ARBlnp : 'X' ) : wPOG ) = *zero;
357200140430             *in28 = *on;
357300140430             *in36 = *on;
357400140430             V1Cmsg = Msg(41);
357500140430             leavesr;
357600140430
357700140430         EndSl;
357800140430
357900140430
358000140430         // -?Verifica SE la causale variazione bolle "I7" può essere?
358100140430         //  ?utilizzata con la spedizione in oggetto?
358200140429         clear  FNLR66ds;
358300140429
358400140507         D66ges = 'K';
358500140429         D66cvb = c_CauVarB_I7;
358600140429         D66aas = V1annS;
358700140429         D66lnp = V1lnpS;
358800140429         D66nrs = V1nrsS;
358900140429         D66nsp = V1nspS;
359000140430         D66tbo = 'A';
359100140429
359200140505         kpjbu = FNLR66ds;
359300140505         fnlr66r ( kpjba );
359400140505         FNLR66ds = kpjbu;
359500140505
359600140505         if  D66msg <> *blank;
359700140505           *in28  = *on;
359800140505           *in36  = *on;
359900140505           V1Cmsg = Msg(26);
360000140505           leavesr;
360100140505         endif;
360200140429
360300140429
360400140429         // -?Verifica SE ci sono errori nella manutenzione della?
360500140429         //  ?spedizione in oggetto?
360600140429         clear  dsARBd;
360700140429         clear  dsARBk;
360800140429         clear  dARBt;
360900140429         clear  dsARBg;
361000140429         clear  FNLR48ds;
361100140429
361200140429         D48aas = V1annS;
361300140429         D48lnp = V1lnpS;
361400140429         D48nrs = V1nrsS;
361500140429         D48nsp = V1nspS;
361600140430         D48cbo = ARBcbo;
361700140429         D48cvb = c_CauVarB_I7;
361800140429         D48trc = 'D';
361900140429         D48ffr = 'F';
362000140429         D48tbo = D66tbo;
362100140429
362200140430         §BDrsd = ARBrsd;
362300140430         §BDind = ARBind;
362400140430         §BDcad = ARBcad;
362500140430         §BDlod = ARBlod;
362600140430         §BDprd = ARBprd;
362700140430         §BDnzd = ARBnzd;
362800140430         §BDfin = ARBfin;
362900140506         §BDffd = 'S';
363000140430         §BDcpi = ARBcpi;
363100140429
363200140429         // -?Reperimento ragione sociale estesa del destinatario?
363300140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'D' )  FIAR4000;
363400140429         if  %found(FIAR401L);
363500140429           §BDrd2 = AR4not;
363600140429         endif;
363700140429
363800140429         // -?Reperimento referente e telefono del destinatario?
363900140429         clear  dAR5gen;
364000140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'GEN' )  FIAR5000;
364100140506         if  %found(FIAR501L)  and  AR5atb = *blank;
364200140429           dAR5gen = AR5uni;
364300140429           §BDref = §AR5ref;
364400140429           §BDtel = §AR5teld;
364500140429         endif;
364600140429
364700140429         kpjbu = FNLR48ds;
364800140429         fnlr48r ( kpjba :
364900140429                   dsARBd :
365000140429                   dsARBk :
365100140429                   dARBt :
365200140429                   dsARBg :
365300140429                   TRUL90ds );
365400140429         FNLR48ds = kpjbu;
365500140429
365600140429         if  D48err <> *blank;
365700140429           *in28  = *on;
365800140430           *in36  = *on;
365900140506           V1Cmsg = D48msg;
366000140429           leavesr;
366100140429         endif;
366200140429
366300140429       ENDSR;
366400151130
366500151130       //--------------------------------------------------------------
366600151203       //?Controlli specifici per Motivo Apertura R.A. " 18"=Reso      ?
366700151130       //--------------------------------------------------------------
366800151130       BEGSR  sr_CtrMotAp_18;
366900151130
367000151201         Select;
367100151201           // -?Errore SE inserimento da Sede?
367200151201           When  *in05;
367300151201             *in28  = *on;
367400151201             *in36  = *on;
367500151201             V1Cmsg = Msg(53);
367600151201             leavesr;
367700151201           // -?Errore SE R.A. non per Spedizione?
367800151201           When  V1Ctor <> 'S';
367900151201             *in28  = *on;
368000151201             *in36  = *on;
368100151201             V1Cmsg = %subst( Msg(45) : 1 : 48 );
368200151201             leavesr;
368300151201         EndSl;
368400151130
368500151130         // -?Errore SE bolla EXPORT (LNA estera)?
368600151130         clear  xx;
368700151201         if  $FNARB = *on;
368800151201           xx = %lookup( %editc( ARBlna : 'X' ) : sk_FILest );
368900151201         else;
369000151201           xx = %lookup( %editc( BLPlna : 'X' ) : sk_FILest );
369100151201         endif;
369200151201         if  xx > *zero;
369300151130           *in28  = *on;
369400151130           *in36  = *on;
369500151130           V1Cmsg = Msg(55);
369600151130           leavesr;
369700151130         endif;
369800151130
369900151130         // -?Verifica se esiste già una disposiz. di Reso/Dirottamento?
370000151130         //  ?per la stessa spedizione?
370100151130         clear  FNLRY09ds2;
370200151204         iLRY09tch  = 'T';
370300151130         iLRY09aas  = V1annS;
370400151130         iLRY09lnp  = V1lnpS;
370500151130         iLRY09nrs  = V1nrsS;
370600151130         iLRY09nsp  = V1nspS;
370700151130         kpjbu = FNLRY09ds2;
370800151130         fnlrY09r ( kpjba );
370900151130         FNLRY09ds2 = kpjbu;
371000151130         If  oLRY09esdi = *on;
371100151130           *in28  = *on;
371200151130           *in36  = *on;
371300151130           V1Cmsg = Msg(56);
371400151204           if  oLRY09reso = *on;
371500151204             xx = %scan( 'Dirottamento' : V1Cmsg );
371600151204             V1Cmsg = %replace( 'Reso        ' : V1Cmsg : xx );
371700151204             V1Cmsg = %subst( V1Cmsg : 1 : xx + 4 ) +
371800151204                      %trimL( %subst( V1Cmsg : xx + 4 ) );
371900151130           endif;
372000151130           leavesr;
372100151130         EndIf;
372200151130
372300160314         // -?Verifica se presente Giacenza aperta?
372400160314         chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  TIGCP21;
372500160314         $Giacenza = ( %found( TIGCP21L )  and
372600160314                       GCPatb = *blank     and
372700160314                       GCPfas < 40 );
372800160314         // -?SE non ho trovato il rcd di giacenza in arrivo provo in partenza?
372900160314         If  Not %found( TIGCP21L );
373000160314           chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  TIGCP000;
373100160314           $Giacenza = ( %found( TIGCP01L )  and
373200160314                         GCPatb = *blank     and
373300160314                         GCPfas < 50 );
373400160314         EndIf;
373500160314         if  $Giacenza;
373600160314           *in28  = *on;
373700160314           *in36  = *on;
373800160315           V1Cmsg = Msg(61);
373900160314           leavesr;
374000160314         EndIf;
374100160314
374200160314         // -?Verifica se bolla legata: dirottamento ammesso solo sulla?
374300160314         //  ?ultima figlia?
374400160314         chain  ( V1annS : V1lnpS : V1nrsS : V1nspS )  FNLBL000;
374500160314         if  %found( FNLBL02L );
374600160314           *in28  = *on;
374700160314           *in36  = *on;
374800160314           V1Cmsg = Msg(62);
374900160314           leavesr;
375000160314         endif;
375100160314
375200151130       ENDSR;
375300141210
375400141210       //--------------------------------------------------------------
375500141210       //?Ctrl specifici per Motivo Apertura R.A. " 60"=Verif. PINcode ?
375600141210       //--------------------------------------------------------------
375700141210       BEGSR  sr_CtrMotAp_60;
375800141210
375900141210         Select;
376000141210
376100141210           // -?Errore SE si è in Sede?
376200141210           When  *in05;
376300141210             *in28  = *on;
376400141210             *in36  = *on;
376500141210             V1Cmsg = Msg(42);
376600141210             leavesr;
376700141210
376800141212           // -?Errore SE utente NON abilitato alla modifica bolle fil.?
376900141212           When  §UTEmbf = *blank;
377000141210             *in28  = *on;
377100141210             *in36  = *on;
377200141210             V1Cmsg = Msg(27);
377300141210             leavesr;
377400141210
377500141210           // -?Errore SE la spedizione NON è in Arrivo?
377600141210           When  Not $FNARB;
377700141210             *in28  = *on;
377800141210             *in36  = *on;
377900141210             V1Cmsg = Msg(45);
378000141210             leavesr;
378100141210
378200141210         EndSl;
378300141210
378400141210
378500141210         // -?Errore SE la spedizione NON è in consegna oggi?
378600141212         exsr  ctr_SpeCons;
378700141212         if  $Cons = *off;
378800141210           *in28  = *on;
378900141210           *in36  = *on;
379000141210           V1Cmsg = Msg(45);
379100141210           leavesr;
379200141210         endif;
379300141210
379400141210         // -?Verifica SE la Particolarità Consegna prevede il PINcode?
379500141210         clear  ds7R;
379600141210         ds_TNTBE.TBEke1 = ARBgma;
379700141210         if  getTabella ( c_Tabel : '7R'  : ds_TNTBE.TBEke1 :
379800141210                          *omit : *omit : *omit :
379900141210                          *omit : *omit :
380000141210                          *omit : *omit : *omit : *omit :
380100141210                          *omit : *omit : *omit : *omit :
380200141210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
380300141210                        = *zero      AND
380400141210             ds_TNTBE.TBEatb = *blank;
380500141210           ds7R   = ds_TNTBE.TBEuni;
380600141210         endif;
380700141210         if  ds7R.§7RpinCode <> 'S';
380800141210           *in28  = *on;
380900141210           *in36  = *on;
381000141210           V1Cmsg = Msg(46);
381100141210           leavesr;
381200141210         endif;
381300141218
381400141218         // -?Verifica esistenza trk "Y" in FIAR4 per la spedizione?
381500141218         setll  ( V1annS : V1lnpS : V1nrsS : V1nspS : 'Y' )  FIAR4000;
381600141218         if  NOT %equal(FIAR401L);
381700141218           *in28  = *on;
381800141218           *in36  = *on;
381900141218           V1Cmsg = Msg(47);
382000141218           leavesr;
382100141218         endif;
382200141210
382300141210       ENDSR;
382400150717
382500150717       //--------------------------------------------------------------
382600150717       //?Ctrl specifici per Motivo Apertura R.A. " 70"=Reclamo da Sede?
382700150717       //--------------------------------------------------------------
382800150717       BEGSR  sr_CtrMotAp_70;
382900150717
383000150717         Select;
383100150717
383200150717           // -?Errore SE NON si è in Sede?
383300150717           When  Not *in05;
383400150717             *in28  = *on;
383500150717             *in36  = *on;
383600150717             V1Cmsg = Msg(51);
383700150717             leavesr;
383800150717
383900150717         EndSl;
384000150717
384100150717       ENDSR;
384200140429
384300140429      /end-free
384400150421
384500150421       //--------------------------------------------------------------
384600150421       //?Controllo Mail del cliente.                                  ?
384700150421       //--------------------------------------------------------------
384800150421     c     sr_CtrlMail   BEGSR
384900150421      *
385000150421     c                   clear                   DSEMAIL
385100150421     c                   movel     wwMail        §emlindi
385200150421     c                   call      'XEMAIL'
385300150421     c                   parm                    DSEMAIL
385400150421      * errore
385500150421    3c                   If        §emlerro = '1'
385600150421     C                   eval      *IN28 = *on
385700150421     C                   eval      *IN38 = *on
385800150421     c                   movel     §emlmsgo      V1Cmsg
385900150421     c                   leavesr
386000150421      *
386100150421     c                   else
386200150421     c                   movel     §emlindo      wwMail
386300150421    3c                   endif
386400150421      *
386500150421     c                   ENDSR
386600150421
386700150421       //--------------------------------------------------------------
386800150421       //?Controllo Motivo Apertura Reclamo.                           ?
386900150421       //--------------------------------------------------------------
387000150421     c     sr_CtrlMAD    BEGSR
387100150421      *
387200150422     c                   clear                   wwdMAD
387300150422      *
387400150421    2c                   If        wwcMAD <> *blanks
387500150421      *
387600150421     c                   clear                   tibs02ds
387700150421      *
387800150421      * Ricerca
387900150421     c     '?'           scan      wwcMAD
388000150421    3c                   if        %found
388100150421      *
388200150421     c                   clear                   tntbMARds1
388300150421      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
388400150421      *      dall'utente in APERTURA;
388500150421      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
388600150421      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
388700150421      *      in GESTIONE.
388800150421     c                   if        wMod <> 'I'
388900150421     c                   eval      iMARutUt = 'I'
389000150421     c                   endif
389100150421     c                   eval      iMARutAp = 'I'
389200150421     c                   movel(p)  tntbMARds1    KPJBU
389300150421      *
389400150421     c                   call      'TNTBMARR1'
389500150421     c                   parm                    KPJBA
389600150421      *
389700150421     c                   movel     KPJBU         tntbMARds1
389800150421    4c                   if        oMARfxx = *blank  and  oMARerr = *off
389900150421     c                   eval      wwcMAD  = oMARcod
390000150422     c                   eval      wwdMAD  = oMARdes
390100150421      *  ?Impostazione campi (se variati) ?
390200150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
390300150421      *    ?(SE variato il Motivo Apertura)?
390400150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
390500150421if  5c                   if        wMod    =  'I'       and
390600150421     c                             wwcMAD  <> wSaveMAD  and
390700150421     c                             $Video  <> 'W4'
390800150429     c                   eval      V1Cmad  =  wwcMAD
390900150429     c                   eval      wSaveMAD = V1Cmad
391000150421     c                   exsr      sr_Ctrl_MAR
391100150421     c                   eval      V1Ccdt   = §MARcauChP
391200150421e   5c                   endif
391300150421      *  errore (non trovato nulla pulisco il '?' )
391400150421     C                   ELSE
391500150421     c                   clear                   wwcMAD
391600150422     c                   clear                   wwdMAD
391700150421    4C                   ENDIF
391800150421      *
391900150421     c                   eval      *in90 = *on
392000150421     C                   eval      *IN36 = *on
392100150421     c                   leavesr
392200150421      *
392300150421   x3C                   ELSE
392400150421      *
392500150421      * Controllo
392600150421     c                   exsr      sr_Ctrl_MAR
392700150723     c                   if        *in28  or  *in90
392800150421     c                   leavesr
392900150421     c                   endif
393000150421      *  ?Impostazione campi (se variati) ?
393100150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
393200150421      *    ?(SE variato il Motivo Apertura)?
393300150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
393400150421if  4c                   if        wMod    =  'I'       and
393500150421     c                             wwcMAD  <> wSaveMAD  and
393600150421     c                             $Video  <> 'W4'
393700150421     c                   eval      wSaveMAD = wwcMAD
393800150421     c                   exsr      sr_Ctrl_MAR
393900150421     c                   eval      V1Ccdt   = §MARcauChP
394000150421e   4c                   endif
394100150421      *
394200150421    3c                   endif
394300150421      *
394400150421      * Confronto del Motivo associato al Tipo Oggetto Reclamo
394500150421      *           con quello inserito a video
394600150422      *?- SE si viene da subfile-control "C01" può essere stato variato?
394700150422      *?  (NON se si viene dalla window "W04")?
394800150421if  3c                   if        §MARtor <> *blank  and  V1Ctor <> §MARtor
394900150421     c                   clear                   dOGR_S
395000150421     c                   eval      T02mod = 'C'
395100150421     c                   eval      T02sif = knsif
395200150421     c                   eval      T02cod = 'OGR'
395300150421     c                   eval      T02ke1 = §MARtor
395400150421     c                   call      'TIBS02R'
395500150421     c                   parm                    KPJBA
395600150421     c                   parm                    TIBS02ds
395700150421if  4c                   if        T02err = *blanks
395800150421     c                   movel     T02uni        dOGR_S
395900150421e   4c                   endif
396000150421      *
396100150421     c                   eval      *in28 = *on
396200150421     c                   eval      *in31 = *on
396300150421     c                   eval      V1Cmsg = %trim(Msg(39)) +
396400150421     c                                      ' "' + §MARtor + '" = ' +
396500150421     c                                      %trim(dOGR_S.§OGRdesc)
396600150421     c                   leavesr
396700150421e   3c                   endif
396800150421      *
396900150421   x2c                   Else
397000150421      *
397100150421      *  Campo Obbligatorio
397200150421     c                   eval      *in28 = *on
397300150723     C                   eval      *IN36 = *on
397400150421     c                   movel     MSG(8)        V1Cmsg
397500150421     c                   leavesr
397600150421      *
397700150421    2C                   EndIf
397800150421      *
397900150421      *?Controlli specifici per Motivo Apertura R.A. " 17"?
398000150421      *?(Fermo Deposito autorizza mittente)?
398100150421      *?SE non protetto?
398200150421     c                   If        wwcMAD = c_FDEAM
398300150421     c                             and  Not *in50
398400150421     c                             and  Not *in70  and  Not *in72
398500150421     c                             and  Not *in74
398600150421     c                   exsr      sr_CtrMotAp_17
398700150421     c                   if        *in28  or  *in90
398800150421     c                   leavesr
398900150421     c                   endif
399000150421     c                   EndIf
399100151130      *
399200151130      *?Controlli specifici per Motivo Apertura R.A. " 18"?
399300151130      *?(Reso)?
399400151130      *?SE  INSERIMENTO  e  non protetto?
399500151130     c                   If        wwcMAD = c_MotAp_Reso  and
399600151130     c                             Wmod   = 'I'      and
399700151130     c                             Not *in50         and
399800151130     c                             Not *in70         and  Not *in72
399900151130     c                                               and  Not *in74
400000151130     c                   exsr      sr_CtrMotAp_18
400100151130     c                   if        *in28  or  *in90
400200151130     c                   leavesr
400300151130     c                   endif
400400151130     c                   EndIf
400500150421      *
400600150421      *?Controlli specifici per Motivo Apertura R.A. " 60"?
400700150421      *?(Verifica telefonica del "PINcode")?
400800150421      *?SE  INSERIMENTO  e  non protetto?
400900150421     c                   If        wwcMAD = c_VPINC  and
401000150421     c                             Wmod   = 'I'      and
401100150421     c                             Not *in50         and
401200150421     c                             Not *in70         and  Not *in72
401300150421     c                                               and  Not *in74
401400150421     c                   exsr      sr_CtrMotAp_60
401500150421     c                   if        *in28  or  *in90
401600150421     c                   leavesr
401700150421     c                   endif
401800150421     c                   EndIf
401900150717      *
402000150717      *?Controlli specifici per Motivo Apertura R.A. " 70"?
402100150717      *?(Reclamo da Sede)?
402200150717      *?SE non protetto?
402300150717     c                   If        wwcMAD = c_ReclSede
402400150717     c                             and  Not *in70  and  Not *in72
402500150717     c                                             and  Not *in74
402600150717     c                   exsr      sr_CtrMotAp_70
402700150717     c                   if        *in28  or  *in90
402800150717     c                   leavesr
402900150717     c                   endif
403000150717     c                   EndIf
403100150421      *
403200150421     c                   ENDSR
403300150429
403400150616      /free
403500150616
403600150429       //--------------------------------------------------------------
403700150429       //?Controllo Reparto Gestione.                                  ?
403800150429       //--------------------------------------------------------------
403900150429       BEGSR  sr_CtrlRPR;
404000150429
404100150429         clear  wwdRPR;
404200150429
404300150429         Select;
404400150429
404500150429           When  %scan( '?' : wwcRPR ) > *zero;
404600150429             clear  wwcRPR;
404700150508             clear  tntbRGRds;
404800150508             iRGRopz = 1;
404900150508             kpjbu   = tntbRGRds;
405000150508             tntbRGRr ( KPJBA );
405100150508             tntbRGRds = kpjbu;
405200150508             If  oRGRerr = *off  and  oRGRfxx = *blank;
405300150508               wwcRPR = oRGRcod;
405400150508               wwdRPR = oRGRdes;
405500150508             endif;
405600150429             *in90 = *on;
405700150429             if  $Video = 'W4';
405800150429               *in45 = *on;
405900150429             else;
406000150429               *in39 = *on;
406100150429             endif;
406200150429             leavesr;
406300150429
406400150429           When  wwcRPR = *blank;
406500150429             *in28 = *on;
406600150429             if  $Video = 'W4';
406700150429               *in45 = *on;
406800150429             else;
406900150429               *in39 = *on;
407000150429             endif;
407100150616             if  *in05;
407200150616               V1Cmsg = Msg(20);
407300150616             else;
407400150616               V1Cmsg = Msg(50);
407500150616             endif;
407600150429             leavesr;
407700150429
407800150429           Other;
407900150429             clear  TIBS02ds;
408000150429             T02mod = 'C';
408100150429             T02sif = knsif;
408200150429             T02cod = 'RGR';
408300150429             T02ke1 = wwcRPR;
408400150429             TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
408500150429             if  T02err = *blank;
408600150429               wwdRPR = T02uni;
408700150429             else;
408800150429               *in28 = *on;
408900150429               if  $Video = 'W4';
409000150429                 *in45 = *on;
409100150429               else;
409200150429                 *in39 = *on;
409300150429               endif;
409400150616               if  *in05;
409500150616                 V1Cmsg = Msg(20);
409600150616               else;
409700150616                 V1Cmsg = Msg(50);
409800150616               endif;
409900150429               leavesr;
410000150429             endif;
410100150429
410200150429         EndSl;
410300150429
410400150429       ENDSR;
410500150616
410600150616       //--------------------------------------------------------------
410700150616       //?Controllo Utente Gestione.                                   ?
410800150616       //--------------------------------------------------------------
410900150616       BEGSR  sr_CtrlRPG;
411000150616
411100150616         clear  wwdRPR;
411200150616
411300150616         // -?Verifica se esiste utente nell'archivio profili?
411400150616         chain  ( wwcRPR )  AZUTE000;
411500150616
411600150616         If  %found(AZUTE01L)  and  UTEdsc > DateU;
411700150616
411800150616           // -?SE lo trova: verifica se è della filiale di gestione?
411900150723           if  %editc(UTEfil:'X') <> wwcPOG;
412000150616             *in28  = *on;
412100150616             *in46  = *on;
412200150616             V1Cmsg = Msg(23);
412300150616             leavesr;
412400150616           endif;
412500150616
412600150616         Else;
412700150616
412800150616           *in28 = *on;
412900150616           *in45 = *on;
413000150616           V1Cmsg = Msg(20);
413100150616           leavesr;
413200150616
413300150616         EndIf;
413400150616
413500150616       ENDSR;
413600150911
413700150911       //--------------------------------------------------------------
413800150911       //?Controllo C.A.P. cliente per reperimento Filiale Gestione    ?
413900150911       //?(per R.A. inerita in SEDE)                                   ?
414000150911       //--------------------------------------------------------------
414100150911       BEGSR  sr_CtrlCAP;
414200150911
414300150911         // -?Se CAP vuoto: uscita?
414400150914         if  W4cCap <> *blank  and  %trim( W4cCap ) = *zero;
414500150911           clear  W4cCap;
414600150911         endif;
414700150911
414800150911         if  W4cCap = *blank;
414900150911           leavesr;
415000150911         endif;
415100150915
415200150915         // -?Selezione CAP?
415300150915         if  %scan( '?' : W4cCap ) > *zero;
415400150915
415500150915           clear  W4cCap;
415600150915           clear  TISI00ds;
415700150915           D00op0 = 'L01';
415800150915           D00dri = DateU;
415900150915           clear  TISI30ds;
416000150915           I30flg = 'S';
416100150915           TISI30R ( kpjba : TISI00ds : TISI30ds );
416200150915           if  D00cap <> *blank;
416300150915             W4cCap = D00cap;
416400150915           endif;
416500150915
416600150915         endif;
416700150911
416800150911         // -?Reperimento filiale (LNA) dal CAP?
416900150911         clear  TISI95ds;
417000150911         I95tcn = '3';
417100150911         I95cap = W4cCap;
417200150911         I95dat = DateU;
417300150911
417400150911         TISI95R ( TISI95ds );
417500150911
417600150914         If  O95err = *blank;
417700150914
417800150914           if  W4cPog <> %editc( O95lna : 'X' );
417900150914             *in90 = *on;
418000150914           endif;
418100150914           W4cPog = %editc( O95lna : 'X' );
418200150914           clear  W4cCap;
418300150915
418400150915           reset  FNLV55ds;
418500150915           //D55tla = 'L';
418600150915           //D55tpt = 'P';
418700150915           D55lin = O95lna;
418800150915           D55drf = DateU;
418900150915           fnlv55r ( FNLV55ds );
419000150915           if  D55err = *blank;
419100150915             W4cPog = %editc( D55tfp : 'X' );
419200150915           endif;
419300150914
419400150914         Else;
419500150914
419600150914           *in28 = *on;
419700150914           *in56 = *on;
419800150914           V1Cmsg = Msg(54);
419900150914           leavesr;
420000150914
420100150914         EndIf;
420200150911
420300150911       ENDSR;
420400150616
420500150616      /end-free
420600150421
420700150421       //--------------------------------------------------------------
420800150421       //?Controllo Filiale Gestione.                                  ?
420900150421       //--------------------------------------------------------------
421000150421     c     sr_CtrlPOG    BEGSR
421100150421      *
421200150422     c                   clear                   wwdPOG
421300150422      *
421400150421     c                   IF        %scan('?':wwcPOG) > 0
421500150421     c                   clear                   dstnsd
421600150421     c                   call      'TNSD24R'
421700150421     c                   parm                    §cod
421800150421     c                   parm                    §tip
421900150421     c                   parm                    §des
422000150421     c                   parm                    §pra
422100150421     c                   movel     §cod          wwcPOG
422200150422     c                   movel     §des          wwdPOG
422300150421     c                   eval      *in90 = *on
422400150421     c                   eval      *in46 = *on
422500150421     c                   leavesr
422600150421     c                   ENDIF
422700150421
422800150421     c                   IF        wwcPOG = *blanks
422900150421     c                   eval      *in28  = *on
423000150421     c                   eval      *in46  = *on
423100150421     c                   eval      V1Cmsg = Msg(48)
423200150421     c                   leavesr
423300150421     c                   ENDIF
423400150421
423500150421     c                   eval      xx = 1
423600150421     c                   FOR       xx by 1 to %len(wwcPOG)
423700150421     c                   IF        %subst(wwcPOG:xx:1) <> *blanks and
423800150421     c                             %check(Digits:%subst(wwcPOG:xx:1)) > *zeros
423900150421     c                   eval      *in28 = *on
424000150421     c                   eval      *in46 = *on
424100150421     c                   eval      V1Cmsg = Msg(48)
424200150421     c                   leavesr
424300150421     c                   ENDIF
424400150421     c                   ENDFOR
424500150421
424600150421     c                   move      wwcPOG        w0030
424700160104     c     w0030         chain     AZORG
424800150717     c                   IF        not %found(AZORG01L)  OR
424900150717     c                              ORGfva <> *blanks    OR
425000150428     c                             (wwcPOG <> c_Sede  and
425100150717     c                              ORGfag <> 'F'     and
425200150723     c                              ORGfag <> 'A')
425300150421     c                   eval      *in28 = *on
425400150421     c                   eval      *in46 = *on
425500150421     c                   eval      V1Cmsg = Msg(48)
425600150421     c                   leavesr
425700150421     c                   ENDIF
425800150422     c                   movel     ORGdes        wwdPOG
425900160120      *
426000150428      *?Filiale Gestione 046 inseribile (in modifica) SOLO da utente di Sede?
426100151211     c                   If        wMod   <> 'I'    and
426200150428     c                             wwcPOG = c_Sede  and  SIMfel <> *zero
426300160121     c                                              and  NOT $AttesaRisposta
426400150429      * - prima verifica che non sia stata richiesta l'interrogazione
426500160121     c                   if        %scan( '?' : V1Crpg ) > *zero
426600150429     c                   eval      wwcRPR = V1Crpg
426700150429     c                   exsr      sr_CtrlRPR
426800160121     c                   eval      V1Crpg = wwcRPR
426900150429     c                   eval      *in28 = *off
427000150429     c                   eval      *in45 = *off
427100150429     c                   eval      *in39 = *off
427200150429     c                   clear                   V1Cmsg
427300160120     c                   endif
427400160121     c                   eval      w_Ute  = V1Crpg
427500150428     c                   exsr      sr_TabUGR
427600150428     c                   if        §UGRpofo <> %int(c_Sede)
427700150428     c                   eval      *in28 = *on
427800150428     c                   eval      *in46 = *on
427900150428     c                   eval      V1Cmsg = Msg(48)
428000150428     c                   leavesr
428100150428     c                   endif
428200150428     c                   EndIf
428300150421      *
428400150421     c                   ENDSR
428500150428
428600150428      /free
428700150428
428800150428       //--------------------------------------------------------------
428900150428       //?Reperimento dati Tab. "UGR" (Utente Gestione Reclami)        ?
429000150428       //--------------------------------------------------------------
429100150428       BEGSR  sr_TabUGR;
429200150428
429300150428         clear  dUGR;
429400150428         clear  TIBS02ds;
429500150428         T02mod = 'C';
429600150428         T02sif = KNSIF;
429700150428         T02cod = 'UGR';
429800150428         T02ke1 = w_Ute;
429900150428
430000150428         TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
430100150428
430200150428         if  T02err = *blank;
430300150428           dUGR = T02uni;
430400150428         endif;
430500150428
430600150428       ENDSR;
430700150428
430800150428      /end-free
430900140429
431000070430      *---------------------------------------------------------------*
431100070430      * Controlli parte alta del ctl per f4 fine richiesta            *
431200070430      *---------------------------------------------------------------*
431300070430     c     SR_CONTRF4    BegSR
431400070430
431500070430    1c                   do        *hival
431600070430      * se dato F4 e non è stato inserito nessun oggetto e non lo sto inserendo adesso ERRORE
431700070430    2c                   if        s01nrr = 0  and v1ctor = *blanks and
431800070430     c                             v1cogg = *blanks and ormvid = *zeros and
431900070430     c                             spevid = *zeros and v1ctch = *blanks and
432000070430     c                             v1cmad = *blanks and v1ccdt = *blanks and
432100070430     c                             v1not1 = *blanks and v1not2 = *blanks and
432200070430     c                             v1not3 = *blanks and v1not4 = *blanks and
432300070430     c                             v1crsp = *blanks and v1cpor = *blanks
432400070430     c                   eval      *in28 = *on
432500070430     c                   movel     MSG(14)       V1Cmsg
432600070430     C                   eval      *IN44 = *on
432700070430     c                   leave
432800070430    2c                   endif
432900070430      * verifco se inserito  il codice chiamante
433000070430    2c                   If        v1cchi = *zeros and wforz = ' '
433100070430      * richamo la routine di ricerca codice nel dettaglio oggetti
433200070430     c                   exsr      SR_RECCHI
433300070430      * controllo se ne ho uno da proporre
433400070430     c                   select
433500070430     c                   when      wrk_cchiA > 0
433600070430     c                   eval      wrk_cchi = wrk_cchiA
433700070430     c                   when      wrk_cchiB > 0
433800070430     c                   eval      wrk_cchi = wrk_cchiB
433900070430     c                   other
434000070430     c                   eval      wrk_cchi = wrk_cchiC
434100070430     c                   endsl
434200070430      *
434300070430    3c                   if        wrk_cchi > *zeros
434400070430     c                   eval      v1cchi = wrk_cchi
434500070430     c                   eval      SW_v1cchi = wrk_cchi
434600070430     c                   clear                   wrk_cchi
434700070430      * se ragione sociale a blank la decodifico
434800070430     C                   clear                   TIBS69DS
434900070430     c                   z-add     v1cchi        I69kac
435000070430     c                   z-add     v1cchi        I69kcp
435100070430     C                   call      'TIBS69R'
435200070430     C                   parm                    tibs69DS
435300070430     C                   parm                    DS_cnaco
435400070430     C                   parm                    DS_cnind
435500070430     C                   parm                    DS_cnclp
435600070430     C                   parm                    DS_fncls
435700070430     C                   eval      wtibs69r = '1'
435800070430    4c                   If         O69ERR =  ' '
435900070430     c                   movel     ACOrag        v1dchi
436000070430     c                   movel     clpclv        v1cclv
436100070430    4c                   Endif
436200070430      *
436300070430   x3c                   else
436400070430      *
436500070430     c                   eval      wforz = '1'
436600070430     c                   eval      *in28 = *on
436700070430     c                   movel     MSG(32)       V1Cmsg
436800070430     C                   eval      *IN10 = *on
436900070430     c                   leave
437000070430    3c                   endif
437100070430    2c                   endif
437200070430
437300070430      * controllo del chiamante se uguale a blanks ma codice cliente valorizzato non faccio nulla
437400070430
437500070430    2c                   If        v1crsc = *blanks and v1dchi = *blanks
437600070430     c                   eval      *in28 = *on
437700070430     c                   movel     MSG(15)       V1Cmsg
437800070430     C                   eval      *IN44 = *on
437900070430     c                   leave
438000070430      * Se anche il chiamante è a blank imposto la ragione sociale del cliente
438100070430     c                   else
438200070430    3c                   if        v1dchi <> *blanks AND v1crsc = *blanks
438300070430     c                   eval      v1crsc = v1dchi
438400070430    3c                   endif
438500070430    2c                   endif
438600070430      *
438700070503     c                   leave
438800070430     c                   enddo
438900070430      *
439000070430     c                   ENDSR
439100070430      *
439200070430      *---------------------------------------------------------------*
439300070430      * Gestione interrogazione oggetto                               *
439400070430      *---------------------------------------------------------------*
439500070430     c     SR_GESF2      BegSR
439600070430
439700070430    1c                   do        *hival
439800070430      * interrogazione Spedizione
439900070430      *
440000070430    2c                   if        v1ctor = 'S'
440100131119      * se non sono in sede
440200070430    3c                   if        not *in05
440300070430      *
440400160701      * verifico se programma richiamato dal PGM interrogazione  Bolle
440500070430    5c                   If        *in50
440600070430     c                   setoff                                       75
440700070430     c     'FNLR36R   '  lookup    £Stack                                 75
440800150615     c  n75'FNLS05R   '  lookup    £Stack                                 75
440900160706     c  n75'FNLRU6R1  '  lookup    £Stack                                 75
441000070430
441100070430      * se trovato vado fuori dal programma
441200070430    6c                   if        %found
441300070430     c                   eval      *in90 = *on
441400070430     c                   eval      *in31 = *on
441500070430     c                   leave
441600070430    6c                   endif
441700070430      *
441800070430    5c                   endif
441900070430      *
442000070430      * se richiamato
442100070430     c                   clear                   fidna1ds
442200160706      *
442300150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
442400150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene ..
442500150601    5c                   if        *in50
442600150601      *
442700150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
442800150601      * oggetto ho più di una RA
442900150601     c                   exsr      Sr_countRA
443000150601      *
443100150601     C                   ELSE
443200150601     c                   eval      IA1INS = 'I'
443300150601    5c                   endif
443400160706      *
443500160706     c                   clear                   KPJBU
443600160706     c                   clear                   ParamU6ds1
443700160706     c                   eval      ParamU6ds1.V2Caas = V1anns
443800160706     c                   eval      ParamU6ds1.COMlnp = V1lnps
443900160706     c                   eval      ParamU6ds1.V2Cnrs = V1nrss
444000160706     c                   eval      ParamU6ds1.V2Cnsp = V1nsps
444100160706     c                   eval      KPJBU = ParamU6ds1
444200160706      * preparo l'altro parametro da passare
444300160706     c                   clear                   fnlrU6ds
444400160706     c*//                eval      iLRU6sel = 'S'
444500160706     c                   eval      iLRU6ra  = 'S'
444600160706     c                   eval      eLRU6ins = iA1ins
444700160706      *
444800160706     c                   call      'FNLRU6R1'
444900160706     c                   parm                    KPJBA
445000160706     c                   parm                    fnlrU6ds
445100160706     c                   movel     kpjbu         ParamU6ds1
445200160706     c                   if        oLRU6err = 'E'
445300160706     C                   eval      *in28 = *on
445400160706     C                   eval      *in44 = *on
445500160706     c                   movel     oLRU6msg      V1Cmsg
445600160706     c                   leave
445700160706     c                   endif
445800070430      *
445900070430     c                   eval      *in90 = *on
446000070430     C                   eval      *IN31 = *on
446100070430     c                   leave
446200070430      *
446300070430      * sono in sede
446400070430      *
446500070430   x3c                   else
446600070430      *
446700070430     c     KSPED         chain     Titas30c
446800070430      * spedizione in sede
446900070430    4c                   If        %found(Titas30c)
447000070430      * verifico se programm
447100070430
447200070430    5c                   If        *in50
447300070430     c                   setoff                                       75
447400070430     c     'TNSB50R   '  lookup    £Stack                                 75
447500070430
447600070430      * se trovato vado fuori dal programma
447700070430    6c                   if        %found
447800070430     c                   eval      *in90 = *on
447900070430     c                   eval      *in31 = *on
448000070430     c                   leave
448100070430    6c                   endif
448200120330      * se non trovo il TNSB50R faccio un tentativo anche con il TNSB51R
448300120330     c                   setoff                                       75
448400120330     c     'TNSB51R   '  lookup    £Stack                                 75
448500120330
448600120330      * se trovato vado fuori dal programma
448700120330    6c                   if        %found
448800120330     c                   eval      *in90 = *on
448900120330     c                   eval      *in31 = *on
449000120330     c                   leave
449100120330    6c                   endif
449200070430      *
449300070430    5c                   endif
449400070430      *
449500070430     c                   clear                   TNSB50DS
449600070430     C                   movel     'R05'         I50OP0
449700070430     C                   z-add     V1anns        D50aas
449800070430     C                   z-add     V1lnps        D50lnp
449900070430     C                   z-add     V1nrss        D50nrs
450000070430     C                   z-add     V1nsps        D50nsp
450100070430     C                   movel(P)  TNSB50DS      KPJBU
450200070430      *
450300070430      * se richiamato
450400070430     c                   clear                   fidna1ds
450500150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
450600150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
450700150601    5c                   if        *in50
450800150601      *
450900150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
451000150601      * oggetto ho più di una RA
451100150601     c                   exsr      Sr_countRA
451200150601      *
451300150601     C                   ELSE
451400150601     c                   eval      IA1INS = 'I'
451500150601    5c                   endif
451600070430      *
451700070430     C                   call      'TNSB50R'
451800070430     C                   parm                    KPJBA
451900070430     C                   parm                    FIDNA1DS
452000070430      *
452100070430
452200070430     c                   eval      *in90 = *on
452300070430     C                   eval      *IN31 = *on
452400070430     c                   leave
452500070430
452600070430    4C                   endif
452700070430
452800070430    3c                   endif
452900070430
453000070430    2c                   endif
453100070430      *
453200070430      * interrogazione ORM
453300070430      * interrogo direttamente con il richiano di FIOR07R
453400070430      *
453500070430    2c                   if        v1ctor = 'O'
453600070430      * verifico se programma richiamato dal PGM interrogazione  ORM
453700070430
453800070430    3c                   If        *in50
453900070430     c                   setoff                                       75
454000070430     c     'FIOR07R   '  lookup    £Stack                                 75
454100070430      * se trovato vado fuori dal programma
454200070430    4c                   if        %found
454300070430     c                   eval      *in90 = *on
454400070430     c                   eval      *in31 = *on
454500070430     c                   leave
454600070430    4c                   endif
454700070430      *
454800070430    3c                   endif
454900070430      *
455000070430     C                   clear                   PARM01
455100070430     C                   move      v1poeo        ppoe
455200070430     C                   move      v1nsor        pnsr
455300070430     C                   move      v1norm        pnor
455400070430     C                   move      v1nrvo        pnrv
455500070430     C                   move      '5'           psce
455600070430      * attenzione non muovo nulla nel campo del p.o. gestione .... vediamo se funziona !!!!!!
455700070430     C                   movel(p)  PARM01        KPJBU
455800070430      * se richiamato
455900070430     c                   clear                   fidna1ds
456000150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
456100150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
456200150601    3c                   if        *in50
456300150601      *
456400150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
456500150601      * oggetto ho più di una RA
456600150601     c                   exsr      Sr_countRA
456700150601      *
456800150601     c                   else
456900150601     c                   eval      IA1INS = 'I'
457000150601    3c                   endif
457100070430      *
457200070430     C                   call      'FIOR07R'
457300070430     C                   parm                    KPJBA
457400070430     C                   parm                    FIDNA1DS
457500070430
457600070430      *
457700070430     c                   eval      *in90 = *on
457800070430     C                   eval      *IN31 = *on
457900070430     c                   leave
458000070430
458100070430    2c                   endif
458200070430
458300070430    1c                   enddo
458400070430
458500070430     c                   endsr
458600070430      *
458700070323      *---------------------------------------------------------------*
458800070323      * Routine gestione richiesta data consegna richiesta            *
458900070323      *---------------------------------------------------------------*
459000070323     c     SR_GES_DTA    BegSR
459100070323      *
459200070416      * se si tratta di una spedizione e non è stata consegnata
459300070327     c                   if        v1ctor = 'S' and arbdcm = *zeros
459400070323      * verifico se sono la filiale di arrivo della bolla
459500130611     c                   move      arblna        w003a
459600130610     c     w003a         lookup    wPOG                                   27
459700070326     c                   If        %found
459800070326      * verifico se si tratta di :
459900070326      *                            lasciato avviso
460000070326      *                            contrassegno
460100070326      *                            supermercato
460200070326      *                            appuntamento
460300070326      *                            fermo deposito
460400070327      *
460500070430      * richiamo il pgm dei blocchi FNLRG6R
460600070430     c                   clear                   FNLRG6DS
460700070430     c                   eval      ilrg6tla = 'L'
460800070430     c                   eval      ilrg6aas = v1anns
460900070430     c                   eval      ilrg6lnp = v1lnps
461000070430     c                   eval      ilrg6nrs = v1nrss
461100070430     c                   eval      ilrg6nsp = v1nsps
461200070430     c                   call      'FNLRG6R'
461300070430     c                   parm                    fnlrg6ds
461400070430     c                   movel     olrg6abi      dlrg6abi
461500070430     c
461600070327     c                   select
461700070326      * Lasciato Avviso
461800070430     c                   when      v1cmad = W_lav
461900070327      * verifico se si tratta di spedizione bloccata
462000070430     c                   If        d§lrg6tla <> ' '
462100070327     c                   eval      W_cau = 'L'
462200070327     c                   exsr      Sr_ges_bloc
462300070327     c                   else
462400070329      * verifico se la spedizione non ha blocchi di altro tipo
462500070507     c                   if        d§lrg6tca = ' ' and d§lrg6tsu = ' '
462600070507     c                             and d§lrg6tap = ' ' and d§lrg6tfd = ' '
462700070329      * se la spedizione non è bloccata per nessun altro motivo
462800070330      * accendo indicatore di emissione della data consegna richiesta
462900070412     c                   seton                                        616290
463000070329      * gestione della videata con immissione data consegna richiesta + richiamo fnlr48r
463100070329     c                   eval      $riemetti = *on
463200070411      * valorizzo il flag per non essere richiamato di nuovo
463300070411     c                   eval      $ric_fnlrg2r = *on
463400070329     c                   endif
463500070327
463600070327     c                   endif
463700070507     c                   leavesr
463800070327
463900070502      * Contrassegno   e spedizione bloccata per c/assegno
464000070502     c                   when      v1cmad = W_con  and d§lrg6tca <> ' '
464100070327     c                   eval      W_cau = 'C'
464200070327     c                   exsr      Sr_ges_bloc
464300070507     c                   leavesr
464400070327
464500070502      * Supermercato   e spediizone bloccata per supermercato
464600070507     c                   when      v1cmad = W_sup  and d§lrg6tsu <> ' '
464700070327     c                   eval      W_cau = 'S'
464800070327     c                   exsr      Sr_ges_bloc
464900070507     c                   leavesr
465000070327
465100070502      * Appuntamento   e la spedizione è bloccata per appuntamento
465200070507     c                   when      v1cmad = W_app  and d§lrg6tap <> ' '
465300070327     c                   eval      W_cau = 'A'
465400070327     c                   exsr      Sr_ges_bloc
465500070507     c                   leavesr
465600070502
465700070502      * Fermo deposito e la spedizione è bloccata per fermo deposito
465800070502     c                   when      v1cmad = W_fde  and d§lrg6tfd <> ' '
465900070327     c                   eval      W_cau = 'F'
466000070327     c                   exsr      Sr_ges_bloc
466100070327
466200070327     c                   endsl
466300070327
466400070327     c                   endif
466500070327
466600070327     c                   endif
466700070327
466800070323     c                   Endsr
466900070327      *---------------------------------------------------------------*
467000070327      * Routine gestione sblocchi spedizioni                          *
467100070327      *---------------------------------------------------------------*
467200070327     c     SR_GES_BLOC   BegSR
467300070411      *
467400070411      * valorizzo il flag per non essere richiamato di nuovo
467500070411     c                   eval      $ric_fnlrg2r = *on
467600070327      *
467700070327     c                   clear                   Fnlrg2ds
467800070327     c                   eval      ilrg2tla = 'L'
467900070327     c                   eval      ilrg2tch = 'R'
468000070327     c                   eval      ilrg2aas = v1anns
468100070327     c                   eval      ilrg2lnp = v1lnps
468200070327     c                   eval      ilrg2nrs = v1nrss
468300070327     c                   eval      ilrg2nsp = v1nsps
468400070327     c                   eval      ilrg2cau = w_cau
468500070327      * se le note a video sono valorizzate
468600070327     c                   clear                   nov
468700070327     c                   eval      nov(1) = v1not1
468800070327     c                   eval      nov(2) = v1not2
468900070327     c                   eval      nov(3) = v1not3
469000070327     c                   eval      nov(4) = v1not4
469100070327      *
469200070327     c                   movea     nov           ilrg2no1
469300070327
469400070327     c                   call      'FNLRG2R'
469500070327     c                   Parm                    kpjba
469600070327     c                   Parm                    Fnlrg2ds
469700070327
469800070327      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
469900070327     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
470000070327      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
470100070403     c                   eval      $bloccof3    = *on
470200070327      * scrittura delle note
470300070327     c                   exsr      Sr_wrt_note
470400160512     c                   eval      $Note_scritte = *on
470500070503      * verifico se ci sono altri blocchi per questa spedizione gestibili da R.A.
470600070503     c                   exsr      Sr_piublocchi
470700070507      * recupero le note
470800070507      *
470900070507     C                   clear                   FIDNB0DS
471000070507     c                   z-add     v1cant        iB0ant
471100070507     c                   z-add     v1cnut        iB0nut
471200070507     c                   z-add     v1cpdt        iB0pdt
471300070507     c                   movel     'GEN'         iB0tIN
471400070507     c                   movel     'R'           iB0flm
471500070507      *
471600070507     C                   CALL      'FIDNB0R'
471700070507     C                   PARM                    kpjba
471800070507     C                   PARM                    FIDNB0DS
471900070507      *
472000070507      * valorizzo le prime 4 note a video
472100070507      *
472200070507     c                   movel     DB0NO1        v1not1
472300070507     c                   movel     DB0NO2        v1not2
472400070507     c                   movel     DB0NO3        v1not3
472500070507     c                   movel     DB0NO4        v1not4
472600070508      * se ob0piu è valorizzato ci sono + di 4 righe
472700070508     c                   eval      *in29 = (OB0PIU = '+')
472800070327      * proteggo i campi e riemetto la videata
472900070418     c                   seton                                        6074
473000070403     c                   eval      $riemetti = *on
473100070327     c                   endif
473200070418
473300070418     c                   eval      *in37 = *on
473400070327
473500070327     c                   endsr
473600070327      *---------------------------------------------------------------*
473700070327      * Routine gestione note che vengono dalla gestione blocchi      *
473800070327      *---------------------------------------------------------------*
473900070327     c     SR_wrt_note   BegSR
474000070412      *
474100070412     c                   clear                   nora
474200070412     c                   clear                   nob
474300070413     c                   clear                   x
474400070413     c                   clear                   y
474500070327      *
474600070327     c                   movea     olrg2no1      nob(1)
474700070327     c                   movea     olrg2no2      nob(9)
474800070327      * tolgo le note a blank
474900070327     c                   do        12            x
475000070327     c                   if        nob(x) <> *blanks
475100070327     c                   add       1             y
475200070327     c                   move      nob(x)        nora(Y)
475300070327     c                   endif
475400070327     c                   enddo
475500070328      * accendo indicatore che mi segnale che ci sono altre note se ne ho + di 4
475600070328     c                   eval      *in29 = y > 4
475700070328      * registro le note sul file note
475800070328    1c                   if        y > *zeros
475900070328     c                   clear                   FIDNB0DS
476000070328     c                   eval      db0no1 = nora(1)
476100070328     c                   eval      db0no2 = nora(2)
476200070328     c                   eval      db0no3 = nora(3)
476300070328     c                   eval      db0no4 = nora(4)
476400070508
476500070508     c                   exsr      SR_add_note
476600070508
476700070328      * verifico se ho altre note da scrivere
476800070328    2c                   if        nora(5) <> *blanks
476900070328     c                   clear                   fidnb0ds
477000070328     c                   eval      db0no1 = nora(5)
477100070328     c                   eval      db0no2 = nora(6)
477200070328     c                   eval      db0no3 = nora(7)
477300070328     c                   eval      db0no4 = nora(8)
477400070328
477500070508     c                   exsr      SR_add_note
477600070328
477700070328    2c                   endif
477800070328      * verifico se ho altre note da scrivere
477900070328    2c                   if        nora(9) <> *blanks
478000070328     c                   clear                   fidnb0ds
478100070328     c                   eval      db0no1 = nora(9)
478200070328     c                   eval      db0no2 = nora(10)
478300070328     c                   eval      db0no3 = nora(11)
478400070328     c                   eval      db0no4 = nora(12)
478500070328
478600070508     c                   exsr      SR_add_note
478700070508
478800070328    2c                   endif
478900070328
479000070328    2c                   endif
479100070328
479200070328
479300070327     c                   endsr
479400070328      *
479500070508      *---------------------------------------------------------------*
479600070508      * Routine che aggiunge note al reclamo esistente                *
479700070508      *---------------------------------------------------------------*
479800070508     c     SR_add_note   BegSR
479900070508
480000070508     c                   z-add     v1cant        iB0ant
480100070508     c                   z-add     v1cnut        iB0nut
480200070508     c                   z-add     v1cpdt        iB0pdt
480300070508     c                   z-add     999           iB0pgn
480400070508     c                   movel     'GEN'         iB0tIN
480500070508     c                   eval      ib0pot =v1cpot
480600070508     c                   eval      db0pon =Dutpou
480700070508     c                   eval      db0utn =knmus
480800070508      * imposto 'W'
480900070508     c                   movel     'W'           iB0flm
481000070508      *
481100070508     c                   call      'FIDNB0R'
481200070508     c                   parm                    kpjba
481300070508     c                   parm                    fidnb0ds
481400070508
481500070508      *
481600070508     c                   endsr
481700070503      *---------------------------------------------------------------*
481800070503      * Routine gestione eventuali blocchi in spediizione             *
481900070503      *---------------------------------------------------------------*
482000070503     c     SR_piublocchi BegSR
482100070503      * salvo il precedente blocco
482200070503     c                   eval      sw_cau = W_cau
482300070508     c                   clear                   note_blocco
482400070503      * richiamo il pgm dei blocchi FNLRG6R
482500070503     c                   clear                   FNLRG6DS
482600070503     c                   eval      ilrg6tla = 'L'
482700070503     c                   eval      ilrg6aas = v1anns
482800070503     c                   eval      ilrg6lnp = v1lnps
482900070503     c                   eval      ilrg6nrs = v1nrss
483000070503     c                   eval      ilrg6nsp = v1nsps
483100070503     c                   call      'FNLRG6R'
483200070503     c                   parm                    fnlrg6ds
483300070503     c                   movel     olrg6abi      dlrg6abi
483400070503      * verifico se ci sono ulteriori blocchi a quello gestito in precedenza
483500070503     c                   select
483600070507      * C/assegno
483700070503     c                   when      d§lrg6tca <> ' ' and sw_cau <> 'C'
483800070503     c                   eval      w_cau = 'C'
483900070508     c                   eval      note_blocco = 'Abilitazione : C/assegno'
484000070503     c                   exsr      sr_ges_bloc2
484100070507      * supermercato
484200070507     c                   when      d§lrg6tsu <> ' ' and sw_cau <> 'S'
484300070507     c                   eval      w_cau = 'S'
484400070508     c                   eval      note_blocco = 'Abilitazione : Supermercato'
484500070507     c                   exsr      sr_ges_bloc2
484600070507      * appuntamento
484700070507     c                   when      d§lrg6tap <> ' ' and sw_cau <> 'A'
484800070507     c                   eval      w_cau = 'A'
484900070508     c                   eval      note_blocco = 'Abilitazione : Appuntamento'
485000070507     c                   exsr      sr_ges_bloc2
485100070507      * fermo deposito
485200070507     c                   when      d§lrg6tfd <> ' ' and sw_cau <> 'F'
485300070507     c                   eval      w_cau = 'F'
485400070508     c                   eval      note_blocco = 'Abilitazione : Fermo Deposito'
485500070507     c                   exsr      sr_ges_bloc2
485600070507      * lasciato avviso
485700070507     c                   when      d§lrg6tla <> ' ' and sw_cau <> 'L'
485800070507     c                   eval      w_cau = 'L'
485900070508     c                   eval      note_blocco= 'Abilitazione : Lasciato Avviso'
486000070507     c                   exsr      sr_ges_bloc2
486100070507
486200070507     c                   endsl
486300070507
486400070503     c                   endsr
486500070503      *
486600070503      *---------------------------------------------------------------*
486700070503      * Routine gestione blocco come seconda o terza chiamata         *
486800070503      *---------------------------------------------------------------*
486900070503     c     SR_ges_bloc2  BegSR
487000070503      *
487100070503     c                   clear                   Fnlrg2ds
487200070503     c                   eval      ilrg2tla = 'L'
487300070503     c                   eval      ilrg2tch = 'R'
487400070503     c                   eval      ilrg2aas = v1anns
487500070503     c                   eval      ilrg2lnp = v1lnps
487600070503     c                   eval      ilrg2nrs = v1nrss
487700070503     c                   eval      ilrg2nsp = v1nsps
487800070503     c                   eval      ilrg2cau = w_cau
487900070503
488000070503     c                   call      'FNLRG2R'
488100070503     c                   Parm                    kpjba
488200070503     c                   Parm                    Fnlrg2ds
488300070503
488400070503      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
488500070503     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
488600070503      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
488700070503     c                   eval      $bloccof3    = *on
488800070508      * prima di scrivere le note  scrivo il motivo del 2° blocco
488900070508     c                   clear                   FIDNB0DS
489000070508     c                   eval      db0no1 = note_blocco
489100070508     c                   exsr      sr_add_note
489200070503      * scrittura delle note
489300070503     c                   exsr      Sr_wrt_note
489400160512     c                   eval      $Note_scritte= *on
489500070503     c                   endif
489600070503
489700070503     c                   endsr
489800131206      *
489900131206      *---------------------------------------------------------------*
490000131206      * Controllo se la spedizione è in consegna                      *
490100131206      *---------------------------------------------------------------*
490200131206     c     CTR_specons   BegSR
490300131206      *
490400140204     c                   setoff                                       16
490500131206     c                   eval      $cons = *off
490600131206      * Aggancio bolle arrivi
490700160104     c     KSPED         chain     Fnarb000
490800131206     c                   If        %found(Fnarb01l)
490900131206      * se si tratta di una spedizione e non è stata consegnata
491000131206     c                   If        ARBfbc = *blanks
491100131206     c                             and  ARBddc > *zeros
491200131206     c                             and  ARBndc > *zeros
491300160317     c                             and  ARBndc <> *all'8'
491400160317     c                             and  ARBndc <> *all'9'
491500131206     c                             and (ARBdcm = *zero    or
491600131206     c                                  ARBica = *blanks  or  ARBica = 'R'
491700131206     c                              or  ARBicc = *blanks  or  ARBicc = 'R')
491800131206     c                   eval      $cons = *on
491900131206     c                   Endif
492000070328      *
492100131206     c                   Endif
492200140123      *
492300131206     c                   endsr
492400131206      *
492500040212      *---------------------------------------------------------------*
492600040212      * Routine conferma riga di dettaglio reclamo                    *
492700040212      *---------------------------------------------------------------*
492800040218     c     SR_CONFE      BegSR
492900150423      *
493000150423      * Se sono in immissione o in gestione di una riga appena inserita
493100150423      *   re-imposto la richiesta della window W04.
493200150423     c                   If        Wmod = 'I'
493300150423     c                   clear                   $GesW04
493400160128     c                   else
493500160128     c                   clear                   $GesW04N
493600150423     c                   EndIf
493700040407      *
493800040218      * se sono in immissione e non in gestione di una riga già inserita
493900040218      * pulisco il record
494000040218     c                   If        Wmod = 'I' and WRK_recsbf = ' '
494100040218     c                   clear                   fitgd000
494200040218     c                   endif
494300150603
494400150603     c                   eval      tgdprt = v1cprt                              priorità dettaglio
494500150625      *
494600041213      * se non sono in immissione verifico se è cambiato qualcosa dalla testa \del reclamo
494700041213      *
494800160217     c                   movel     TGDorr        w0040
494900041213     c                   If        Wmod <> 'I' and (tgdtco <> v1ctco or
495000160217     c                             tgddar <> wdar   or  w0040 <> v1corr or
495100041213     c                             tgdksc <> v1cchi or tgdrsc <> v1crsc or
495200041213     c                             tgdclv <> v1cclv)
495300041213     c                   eval      mod_testa = 'S'
495400041213     c                   endif
495500151202      *
495600151202      * -?Memorizzazione  immissione R.A. con Motivo Apertura " 18"?
495700151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
495800151202      *         ?& ho modificato il motivo in " 18" (Reso)?
495900151203      *  ?=> Non sarà più possibile premere F3?
496000151202     c                   clear                   $WrtTIIDC
496100151202if  1c                   If        Wmod    = 'I'           and
496200151202     c                             V1Cmad  = c_MotAp_Reso  and
496300151202     c                             ( wrk_RecSbf = *blank   or
496400151202     c                             ( wrk_RecSbf = 'Y'     and
496500151202     c                               V1Cmad    <> TGDmad ) )
496600151202     c                   eval      $WrtTIIDC = *on
496700151203     c                   eval      $RA_MA_18 = *on
496800151202e   1c                   EndIf
496900040218      *
497000040218      * oggetto
497100040218     c                   eval      tgdtor = v1ctor                              tipo oggetto reclamo
497200040218     c                   if        v1ctor = 'S'
497300040218     c                   movel     spevid        tgdogg                         spedizione
497400040218     c                   endif
497500040218     c                   if        v1ctor = 'O'
497600040218     c                   movel     ormvid        tgdogg                         ORM
497700040218     c                   endif
497800040218     c                   if        v1ctor = 'G'
497900040218     c                   movel     v1cogg        tgdogg                         generico
498000040218     c                   endif
498100040218      * motivo apertura
498200040218     c                   eval      tgdmad = v1cmad
498300141216      *
498400151216      /free
498500151216
498600151216         // -?Richiesta Risposta (Avanzamento Fase)?
498700151216         Select;
498800160207
498900160121           // -?Utente di Sede su R.A. con Attesa di Risposta?
499000160121           //  ?- CASO IMPOSSIBILE !!! -?
499100160121           When  *in05  and  dTGDfar.§TGDatt = 'S';
499200160121             // ·?Storicizzazione Variazioni Richiesta Risposta?
499300160121             wTMO = 'A';
499400160121             clear dTGMa;
499500160121             dTGMa.§MODattO = dTGDfar.§TGDatt;
499600160121             dTGMa.§MODrisO = dTGDfar.§TGDris;
499700160121             dTGMa.§MODfilO = dTGD01.§TGDfilR;
499800160121             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
499900160121             exsr  WRT_Modifiche;
500000160121             // ·?Valorizzazione campi di FITGD?
500100160121             clear  dTGDfar.§TGDatt;
500200160121             clear  dTGDfar.§TGDris;
500300160121             clear  V1CrepR;
500400160207             V1CfilR = *zero;
500500160207
500600160207           // -?R.A. "chiusa" con Attesa Risposta = "S"?
500700160207           //                ?e Ricevuta Risposta = " "?
500800160129           When  wMod           <> 'I'     and
500900160129                 V1Ccdt         <> *blank  and
501000160129                 dTGDfar.§TGDatt = 'S'     and
501100160129                 dTGDfar.§TGDris = *blank  and
501200160201                 V1CfilR         > *zero   and
501300160201                 V1CrepR        <> *blank;
501400151216             // ·?Storicizzazione Variazioni Richiesta Risposta?
501500151216             wTMO = 'A';
501600151216             clear dTGMa;
501700151216             dTGMa.§MODattO = dTGDfar.§TGDatt;
501800151216             dTGMa.§MODrisO = dTGDfar.§TGDris;
501900151216             dTGMa.§MODfilO = dTGD01.§TGDfilR;
502000151216             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
502100151223             dTGMa.§MODattN = 'S';
502200151216             dTGMa.§MODrisN = 'S';
502300160129             dTGMa.§MODfilN = %int( V1CfilR );
502400160129             dTGMa.§MODrepN = V1CrepR;
502500151216             exsr  WRT_Modifiche;
502600151216             // ·?Valorizzazione campi di FITGD?
502700151223             dTGDfar.§TGDatt = 'S';
502800151216             dTGDfar.§TGDris = 'S';
502900151216             clear  V1Ccdt;
503000151216             if  wMod = 'I';
503100160121               //V1Cpog = W4filR;  ?(già così)?
503200160121               //V1Crpg = W4repR;  ?(già così)?
503300151216             else;
503400151223               V1Cpog = V1CfilR;
503500151223               V1Crpg = V1CrepR;
503600151216             endif;
503700160207
503800160121           // -?R.A. NON chiusa con Attesa Risposta = "S"?
503900160121           //                  ?e Ricevuta Risposta = " "?
504000160121           //  ?e variati Filiale e/o Reparto che Attende Risposta?
504100160121           When  wMod           <> 'I'     and
504200160121                 V1Ccdt          = *blank  and
504300160121                 dTGDfar.§TGDatt = 'S'     and
504400160121                 dTGDfar.§TGDris = *blank  and
504500160121                 ( V1CfilR      <> %editc( dTGD01.§TGDfilR : 'X' )  or
504600160121                   V1CrepR      <> dTGR01A.§TGRrepR );
504700160121             // ·?Storicizzazione Variazioni Richiesta Risposta?
504800160121             wTMO = 'A';
504900160121             clear dTGMa;
505000160121             dTGMa.§MODattO = dTGDfar.§TGDatt;
505100160121             dTGMa.§MODrisO = dTGDfar.§TGDris;
505200160121             dTGMa.§MODfilO = dTGD01.§TGDfilR;
505300160121             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
505400160129             if  V1CfilR > *zero  and  V1CrepR <> *blank;
505500160129               dTGMa.§MODattN = 'S';
505600160129             endif;
505700160121             dTGMa.§MODfilN = %int( V1CfilR );
505800160121             dTGMa.§MODrepN = V1CrepR;
505900160121             exsr  WRT_Modifiche;
506000160121             // ·?Valorizzazione campi di FITGD?
506100160129             if  V1CfilR > *zero  and  V1CrepR <> *blank;
506200160129               dTGDfar.§TGDatt = 'S';
506300160129             else;
506400160129               clear  dTGDfar.§TGDatt;
506500160129             endif;
506600160129             clear  dTGDfar.§TGDris;
506700160129             clear  V1Ccdt;
506800160121             //V1Cpog = V1CfilR;   ?(da lasciare com'è)?
506900160121             //V1Crpg = V1CrepR;   ?(da lasciare com'è)?
507000160207
507100160105           // -?R.A. NON chiusa con Attesa Risposta = "S"?
507200160105           //                    ?e Ricevuta Risposta = "S"?
507300160105           When  wMod           <> 'I'     and
507400160105                 V1Ccdt          = *blank  and
507500160129                 dTGDfar.§TGDatt = 'S'     and
507600160129                 dTGDfar.§TGDris = 'S'     and
507700160201                 V1CfilR         > *zero   and
507800160201                 V1CrepR        <> *blank;
507900160105             // ·?Storicizzazione Variazioni Richiesta Risposta?
508000160105             wTMO = 'A';
508100160105             clear dTGMa;
508200160105             dTGMa.§MODattO = dTGDfar.§TGDatt;
508300160105             dTGMa.§MODrisO = dTGDfar.§TGDris;
508400160105             dTGMa.§MODfilO = dTGD01.§TGDfilR;
508500160105             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
508600160105             dTGMa.§MODattN = 'S';
508700160105             dTGMa.§MODfilN = %int( V1CfilR );
508800160105             dTGMa.§MODrepN = V1CrepR;
508900160105             exsr  WRT_Modifiche;
509000160105             // ·?Valorizzazione campi di FITGD?
509100160105             dTGDfar.§TGDatt = 'S';
509200160105             clear  dTGDfar.§TGDris;
509300160105             clear  V1Ccdt;
509400160107             //V1Cpog = V1CfilR;   ?(NON in NON chiusura)?
509500160107             //V1Crpg = V1CrepR;   ?(NON in NON chiusura)?
509600160207
509700160105           // -?R.A. con Attesa Risposta variata?
509800151223           When  ( V1CfilR         > *zero   and
509900160201                   V1CrepR        <> *blank  and
510000160105                   dTGDfar.§TGDatt = *blank ) OR
510100151223                 ( V1CfilR        <= *zero   and
510200160201                   V1CrepR         = *blank  and
510300160105                   dTGDfar.§TGDatt = 'S' );
510400160104             // ·?Storicizzazione Variazioni Ricevuta Risposta?
510500151222             wTMO = 'A';
510600151222             clear dTGMa;
510700151222             dTGMa.§MODattO = dTGDfar.§TGDatt;
510800151222             dTGMa.§MODrisO = dTGDfar.§TGDris;
510900151222             dTGMa.§MODfilO = dTGD01.§TGDfilR;
511000151222             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
511100160201             if  V1CfilR > *zero  and  V1CrepR <> *blank;
511200151223               dTGMa.§MODattN = 'S';
511300151222               if  wMod = 'I';
511400151222                 dTGMa.§MODfilN = %int( W4filR );
511500151222                 dTGMa.§MODrepN = W4repR;
511600151222               else;
511700151223                 dTGMa.§MODfilN = %int( V1CfilR );
511800151223                 dTGMa.§MODrepN = V1CrepR;
511900151222               endif;
512000151222             endif;
512100151222             exsr  WRT_Modifiche;
512200151222             // ·?Valorizzazione campi di FITGD?
512300160201             if  V1CfilR > *zero  and  V1CrepR <> *blank;
512400151223               dTGDfar.§TGDatt = 'S';
512500160105             else;
512600160105               clear  dTGDfar.§TGDatt;
512700151223             endif;
512800151222             clear  dTGDfar.§TGDris;
512900160207
513000151223           // -?R.A. inserita (aperta) & cliente in attesa di Risposta?
513100160216           When  wMod     = 'I'    and  V1Ccdt  = *blank  and
513200160216                 W4tgdATT = 'S'    and
513300160216                 W4filR   > *zero  and  W4repR <> *blank;
513400151223             // ·?Storicizzazione inserimento di R.A. (aperta) con?
513500151223             //  ?Richiesta Risposta?
513600160216             wTMO = 'A';
513700160216             clear  dTGMa;
513800160216             // ·?Meglio inserire anche i dati già esistenti:?
513900160216             //  ?potrei aver ora modificato la filiale e/o il?
514000160216             //  ?reparto della R.A. appena inserita (F6) ma NON?
514100160216             //  ?ancora confermata (F4)?
514200160216             dTGMa.§MODattO = dTGDfar.§TGDatt;
514300160216             dTGMa.§MODrisO = dTGDfar.§TGDris;
514400160216             dTGMa.§MODfilO = dTGD01.§TGDfilR;
514500160216             dTGMa.§MODrepO = dTGR01A.§TGRrepR;
514600160216             dTGMa.§MODattN = W4tgdATT;
514700160216             dTGMa.§MODfilN = %int( W4filR );
514800160216             dTGMa.§MODrepN = W4repR;
514900160216             exsr  WRT_Modifiche;
515000151223             // ·?Valorizzazione campi di FITGD?
515100151216             dTGDfar.§TGDatt = W4tgdATT;
515200160108             clear  dTGDfar.§TGDris;
515300160207
515400160207           // -?R.A. chiusa dalla filiale in Attesa Risposta?
515500160207           //  ?(con Risposta Ricevuta)?
515600160207           When  wMod           <> 'I'     and
515700160207                 V1Ccdt         <> *blank  and
515800160207                 dTGDfar.§TGDatt = 'S'     and
515900160207                 dTGDfar.§TGDris = 'S';
516000160207             clear  dTGDfar.§TGDatt;
516100160207             clear  dTGDfar.§TGDris;
516200160207             V1CfilR = *zero;
516300160207             clear  V1CrepR;
516400160207
516500151223           // -?Cliente NON in attesa di Risposta?
516600150108           When  dTGDfar.§TGDris = *blank  and
516700160201                 V1CfilR        <= *zero   and
516800160201                 V1CrepR         = *blank;
516900151216             clear  dTGDfar.§TGDatt;
517000160108             clear  dTGDfar.§TGDris;
517100160207             V1CfilR = *zero;
517200160207             clear  V1CrepR;
517300160207
517400151216         EndSl;
517500151216
517600151216         TGDfar = dTGDfar;
517700151216
517800151216      /end-free
517900151216      *
518000040218      * chiusura
518100040406      * se sono in manutenzione e modifico la causale di chiusura scrivo delle note
518200150622      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
518300150622      ***?già impostato.?
518400040406     c                   if        tgdcdt <> *blank and v1ccdt <> tgdcdt
518500040406     c                             and Wmod <> 'I'
518600150318     c                   eval      wtmo = 'C'
518700150318     c                   clear                   dTGMc
518800150318     c                   eval      §MODcdto = TGDcdt
518900150318     c                   eval      §MODcdtn = V1Ccdt
519000150318     c                   exsr      WRT_modifiche
519100040406     c                   endif
519200040406      *
519300040218      * se è la prima volta che chiudo imposto anche data,ora,p.o. e profilo chiusura
519400040406      *
519500151214if  1c                   if        tgdcdt = *blank and v1ccdt <> *blank
519600151214if  2c                   if        Wmod = 'I'
519700040304      * imposto ora e data inserimento chiamata  se sono in immissione
519800040304     c                   eval      tgddcd = w1cdar                              data chiusura
519900150324     c                   eval      tgdocd = %dec(%time())                       ora chiusura
520000040304
520100040304     c                   else
520200150603      * se sono in manutenzione e modifico la causale di chiusura
520300040304      * recupero ora e data chiusura se sono in manutenzione
520400150625      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
520500150625      ***?già impostato.?
520600040304     c                   time                    w0140
520700040304
520800040304     c                   clear                   WLBdat
520900040304     c                   move      w0140         g02dat
521000040304     c                   call      'XSRDA8'
521100040304     c                   parm                    WLBdat
521200040304     c                   eval      tgddcd = g02inv                              data chiusura
521300040304     c                   movel     w0140         tgdocd                         ora chiusura
521400151214e   2c                   endif
521500040304
521600040617     c                   eval      tgdpct = v1cpot                              p.o. chiusura
521700040218     c                   eval      tgdutc = KNMUS                               utente chiusura
521800040304
521900151214e   1c                   endif
522000151216      *
522100040223      * se codice chiusura è blanks e prima non lo era pulisco i campi del file
522200040223     c                   if        tgdcdt <> *blank and v1ccdt = *blank
522300040223     c                   clear                   tgddcd                         data chiusura
522400040223     c                   clear                   tgdocd                         ora chiusura
522500040223     c                   clear                   tgdpct                         p.o. chiusura
522600040223     c                   clear                   tgdutc                         utente chiusura
522700040223     c                   endif
522800040223      *
522900040223     c                   eval      tgdcdt = v1ccdt                              codice chiusura
523000040218
523100150316      * se utente a video non è l'utente del job
523200150316      * priorità a '1'
523300150316      * imposto priorità a '0'
523400150313     c                   IF        TGDprt = '1' and
523500150316     c                             V1Crpg <> KNMUS
523600150313     c                   eval      TGDprt = '0'
523700150313     c                   ENDIF
523800150316      * se sono in manutenzione e variato utente scrivo file variazioni
523900160111      *    (o anche se variata filiale gestione)
524000160111     c*//                if        TGDrpg <> V1Crpg and wmod <> 'I'
524100160111     c                   if          wMOD   <> 'I'     AND
524200160111     c                             ( TGDrpg <> V1Crpg  or
524300160111     c                               TGDpog <> %int(V1Cpog) )
524400150316     c                   eval      wtmo = 'U'
524500150316     c                   clear                   dTGMu
524600150316     c                   eval      §MODuteo = TGDrpg
524700150316     c                   eval      §MODpogo = TGDpog
524800150316     c                   eval      §MODuten = V1Crpg
524900160121     c                   eval      §MODpogn = %dec(V1Cpog:3:0)
525000150316     c                   exsr      WRT_modifiche
525100040618     c                   endif
525200040618      *
525300150423     c                   eval      tgdpog = %dec(v1cpog:3:0)                    filiale gestione
525400150423     c                   eval      tgdrpg = v1crpg                              utente  gestione
525500160119     c                   if        V1Ccdt =  *blanks  and
525600160119     c                             V1Cpog <> c_Sede   and
525700160119     c                             ( $Reparto         or
525800160201     c                             ( V1CfilR  > *zero and
525900160201     c                               V1CrepR <> *blank ) )
526000150422     c                   eval      tgdRPR = V1Crpg                              reparto gestione
526100150422     c                   endif
526200040618     c                   eval      tgdrsp = v1crsp                              responsabilità
526300040618      *
526400040407      * P.O. responsabile
526500040407      * se sono in manutenzione e modifico il p.o.responsabile scrivo delle note
526600040407      *
526700040407     c                   if        tgdpor <> *zeros and W_por  <> tgdpor
526800040407     c                             and Wmod <> 'I'
526900150318     c                   eval      wtmo = 'R'
527000150318     c                   clear                   dTGMr
527100150318     c                   eval      §MODporo = TGDpor
527200150318     c                   eval      §MODporn = W_por
527300150318     c                   exsr      WRT_modifiche
527400040407     c                   endif
527500040407      *
527600040218     c                   eval      tgdpor = W_por                               P.O. responsabile
527700040213      *
527800151214if  1c                   If        wmod = 'I'
527900040213     c                   eval      tgddar = wdar                                dta ricez. comunicaz
528000040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
528100040217     c                   eval      tgdtco = v1ctco                              tipo comunicazione
528200040213      *
528300040213     c                   eval      tgdtch = v1ctch                              tipo chiamamte
528400040213     c                   eval      tgdclv = v1cclv                              categoria cliente
528500040213     c                   eval      tgdrsc = v1crsc                              ragione sociale
528600041209     c                   eval      tgdksc = v1cchi                              codice cliente
528700120919      * se il motivo di apertura lo richiede NON memorizzo il KSC
528800120919     c                   IF        §MARksc = 'N'
528900120919     c                   clear                   TGDksc
529000120919     c                   ENDIF
529100040218     c                   eval      tgdpot = v1cpot                              p.o. immette recla
529200040421     c                   eval      tgdant = v1cant                              Anno   reclamo
529300040218     c                   eval      tgdnut = v1cnut                              Numero reclamo
529400040218     c                   eval      tgdpdt = v1cpdt                              progressivo dettagli
529500151214e   1c                   endif
529600041213      * solo in immissione valorizzo i seguenti campi
529700040407     c                   if        wmod = 'I' and wrk_recsbf = ' '
529800040218     c                   eval      tgddac = w1cdar                              data inserimento
529900150324     c                   eval      tgdorc = %dec(%time())                       ora inserimento
530000040218     c                   eval      tgdpru = KNMUS                               utente inserimento
530100040407     c                   endif
530200150326      /free
530300151215         // -?Flag Operativi?
530400160107         clear  dTGD01.§TGDfilR;
530500160207         if  TGDcdt = *blank  and  dTGDfar.§TGDatt = 'S'
530600160207                              and  V1CfilR > *zero;
530700160129           dTGD01.§TGDfilR = %int( V1CfilR );
530800151215         endif;
530900160107         TGDflo = dTGD01;
531000151215
531100150326       //?Storicizzo Variazioni Stato
531200150326         IF  W_prt <> TGDprt;
531300150326           wtmo = 'S';
531400150326           clear dTGMs;
531500150326           §MODprto = W_prt;
531600150326           §MODprtn = TGDprt;
531700150326           exsr WRT_modifiche;
531800150326         ENDIF;
531900150326      /end-free
532000040213      *
532100040309
532200040309      * se sono in immissione e non in gestione di una riga già inserita
532300040309      * valorizzo i seguenti campi
532400040309
532500040309     c                   If        Wmod = 'I' and WRK_recsbf = ' '
532600040218     c                   write     fitgd000
532700040218     c                   FEOD      fitgd01l
532800040218     c                   add       1             rec_wrt
532900041210      * aggiungo 1 al numeratore
533000041210     c                   add       1             sv_pdt
533100040218      *
533200040218     c                   else
533300040218
533400040218     c                   update    fitgd000
533500040218      *
533600040218     c                   endif
533700151201      *
533800151201      * -?Immissione R.A. con Motivo Apertura " 18":?
533900151201      *  ?=> Generazione automatica della "Disposizione di Reso"?
534000151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
534100151202      *         ?& ho modificato il motivo in " 18" (Reso)?
534200151202     c                   If        $WrtTIIDC = *on
534300151201     c                   exsr      sr_Wrt_TIIDC
534400151202e   1c                   EndIf
534500141216      *
534600141216      * -?Immissione R.A. con Motivo Apertura " 60":?
534700141216      *  ?SE NON sono in modifica di una spedizione nella stessa telefonata?
534800141216if  1c                   If        Wmod       = 'I'      and
534900141216     c                             V1Cmad     = c_VPINC  and
535000141216     c                             WRK_recsbf = *blank
535100141216      *
535200141216      * ·?=> Registrazione automatica dell'esito nelle Note?
535300141216     c                   eval      not_PINcode = 'S'
535400141216     c                   exsr      WRT_note
535500141216      * ·?=> Registrazione automatica dell'esito nei Riferimenti PINcode?
535600141216      *?· Impostazione dati della spedizione nella ds per TGRRIF?
535700141216     c                   clear                   dTGR01
535800141216     c                   if        oPINfxx = *blank  and  oPINerr = *blank
535900141216     c                   eval      dTGR01.§TGResito = 'P'
536000141216     c                   else
536100141216     c                   eval      dTGR01.§TGResito = 'N'
536200141216     c                   endif
536300141216     c                   eval      dTGR01.§TGRfgs   = ARBifp
536400141216     c                   eval      dTGR01.§TGRnfv   = ARBndc
536500141216     c                   eval      dTGR01.§TGRdfv   = ARBddc
536600141216     c                   eval      dTGR01.§TGRpdr   = ARBpdc
536700141216      *?· Impostazione dati nel file FITGR00F?
536800141216     c                   clear                   FITGR000
536900141216     c                   eval      TGRpot = V1Cpot                              Fil. immette tel.
537000141216     c                   eval      TGRant = V1Cant                              Anno   telefonata
537100141216     c                   eval      TGRnut = V1Cnut                              Numero telefonata
537200141216     c                   eval      TGRpdt = V1Cpdt                              Progr. Dettaglio
537300141216     c                   eval      TGRtri = 'D'                                 Tipo Riferimento
537400141216     c                   eval      TGRrif = dTGR01
537500141216     c                   write     FITGR000
537600141216     c                   feod      FITGR01L
537700141216      *
537800141216e   1c                   EndIf
537900040213
538000040213      * verifico se esistono per questa riga che confermo dei record di riferimenti
538100040213      * ed evantualmente li deleto e poi riscrivo
538200040213
538300040421     c     ktgran        setll     fitgr01l
538400040213      *
538500040213     c                   do        *hival
538600040421     c     ktgran        reade     fitgr01l
538700040213      *
538800040219     c                   if        not %eof(fitgr01l)
538900040213     c                   delete    fitgr01l
539000040213     c                   else
539100040213     c                   leave
539200040213     c                   endif
539300040213      *
539400040213     c                   enddo
539500151214      *
539600040213      * se esiste referente
539700040213     c                   if        v1refe <> *blank
539800040213     c                   clear                   fitgr000
539900040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
540000040421     c                   eval      tgrant = v1cant                              Numero reclamo
540100040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
540200040213     c                   eval      tgrtri = 'R'                                 tipo  riferimento
540300040213     c                   movel     v1refe        tgrrif
540400040213     c                   write     fitgr000
540500040219     c                   feod      fitgr01l
540600040213     c                   endif
540700040213      * se esiste indirizzo e-mail
540800040213     c                   if        v1mail <> *blank
540900040213     c                   clear                   fitgr000
541000040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
541100040421     c                   eval      tgrant = v1cant                              Numero reclamo
541200040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
541300040213     c                   eval      tgrtri = 'M'                                 tipo  riferimento
541400040213     c                   movel     v1mail        tgrrif
541500040213     c                   write     fitgr000
541600040219     c                   feod      fitgr01l
541700040213     c                   endif
541800040213      * se esiste numero telefono e/o numero fax
541900040213     c                   if        v1ntel <> *blank or v1nfax <> *blank
542000040213     c                   clear                   fitgr000
542100040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
542200040421     c                   eval      tgrant = v1cant                              Numero reclamo
542300040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
542400040213     c                   eval      tgrtri = 'N'                                 tipo  riferimento
542500040213     c                   movel     v1ntel        tgrrif
542600040213     c                   move      v1nfax        tgrrif
542700040213     c                   write     fitgr000
542800040219     c                   feod      fitgr01l
542900040213     c                   endif
543000151215      * -?se Richiesta Risposta?
543100151215     c                   eval      TGRtri = 'A'
543200151215     c     keyFITGR01    setll     FITGR000
543300151215     c                   if        %equal(FITGR01L)
543400151215     c     keyFITGR01    delete    FITGR000
543500151215     c                   endif
543600151215     c                   clear                   wTGRpdt
543700160204     c                   If        V1CfilR  > *zero  and
543800160204     c                             V1CrepR <> *blank
543900160129     c*//?NON serve:?    clear                   dTGR01A
544000160129     c                   eval      dTGR01A.§TGRrepR = V1CrepR
544100151214     c                   clear                   FITGR000
544200151214     c                   eval      TGRpot = V1Cpot                              Fil. immiss. recl.
544300151214     c                   eval      TGRant = V1Cant                              Anno   reclamo
544400151214     c                   eval      TGRnut = V1Cnut                              Numero reclamo
544500151215     c                   eval      TGRpdt = TGDpdt                              Progr. reclamo
544600151214     c                   eval      TGRtri = 'A'                                 Tipo  riferimento
544700151214     c                   eval      TGRrif = dTGR01A
544800151214     c                   write     FITGR000
544900151214     c                   feod      FITGR01L
545000151214     c                   EndIf
545100040217
545200070413      * confermo le note se sono in immissione e non ho scritto nulla precedentemente
545300040217
545400160512     c****               If        Wmod = 'I' and $bloccof3 = *off
545500160512     c                   If        Wmod = 'I' and $Note_scritte= *off
545600070413     c                   clear                   FIDNB0DS
545700040421     c                   z-add     v1cant        iB0ant
545800040217     c                   z-add     v1cnut        iB0nut
545900040217     c                   z-add     v1cpdt        iB0pdt
546000040217     c                   movel     'GEN'         iB0tIN
546100141217     c                   if        WRK_RecSbf = *blank
546200141216     c                   z-add     999           iB0pgn
546300141217     c                   endif
546400040217     c                   movel     v1not1        db0no1
546500040217     c                   movel     v1not2        db0no2
546600040217     c                   movel     v1not3        db0no3
546700040217     c                   movel     v1not4        db0no4
546800040617     c                   eval      ib0pot =v1cpot
546900041013     c                   eval      db0pon =Dutpou
547000040617     c                   eval      db0utn =knmus
547100040218      * se non è mai stato chiamato imposto 'W'
547200040218     c                   if        wfidnb0r = 'Y'
547300040217     c                   movel     'C'           iB0flm
547400040218     c                   else
547500040218     c                   movel     'W'           iB0flm
547600040220
547700040218     c                   endif
547800040217      *
547900040217     C                   CALL      'FIDNB0R'
548000040217     C                   PARM                    kpjba
548100040217     C                   PARM                    FIDNB0DS
548200040217      *
548300040305     c                   endif
548400040305
548500070413      * modalità manutenzione
548600040305
548700070424    1c                   If         Wmod = 'G' or
548800070424     c                             (Wmod = 'I' and $bloccof3 = *on)
548900040305      * nel caso in cui sia stato richiamato il pgm fidnb0r per gestire le note
549000040305      * non lo richiamo + per confermarle
549100070413    2c                   if        wfidnb0r <> 'Y'
549200040305     C                   clear                   FIDNB0DS
549300040421     c                   z-add     v1cant        iB0ant
549400040305     c                   z-add     v1cnut        iB0nut
549500040305     c                   z-add     v1cpdt        iB0pdt
549600040305     c                   movel     'GEN'         iB0tIN
549700040305     c                   movel     'W'           iB0flm
549800040305     c                   eval      ib0pot =v1cpot
549900041013     c                   eval      db0pon =Dutpou
550000040305     c                   eval      db0utn =knmus
550100040305      * valorizzo il progressivo riga  scrivo la prima riga
550200040305    3c                   if        *in51 = *off and V1not1 <> *blanks
550300040305     c                   z-add     1             Ib0pgn
550400040305     c                   movel     v1not1        db0no1
550500040305      *
550600040305     C                   CALL      'FIDNB0R'
550700040305     C                   PARM                    kpjba
550800040305     C                   PARM                    FIDNB0DS
550900040305    3c                   endif
551000040305
551100040305      * valorizzo il progressivo riga  scrivo la seconda riga
551200040305    3c                   if        *in52 = *off and V1not2 <> *blanks
551300040305     c                   z-add     2             Ib0pgn
551400040305     c                   movel     v1not2        db0no1
551500040305      *
551600040305     C                   CALL      'FIDNB0R'
551700040305     C                   PARM                    kpjba
551800040305     C                   PARM                    FIDNB0DS
551900040305    3c                   endif
552000040305      * valorizzo il progressivo riga  scrivo la terza   riga
552100040305    3c                   if        *in53 = *off and V1not3 <> *blanks
552200040305     c                   z-add     3             Ib0pgn
552300131017     c                   movel     v1not3        db0no1
552400040305      *
552500040305     C                   CALL      'FIDNB0R'
552600040305     C                   PARM                    kpjba
552700040305     C                   PARM                    FIDNB0DS
552800040305    3c                   endif
552900040305      * valorizzo il progressivo riga  scrivo la quarta  riga
553000040305    3c                   if        *in54 = *off and V1not4 <> *blanks
553100040305     c                   z-add     4             Ib0pgn
553200040305     c                   movel     v1not4        db0no1
553300040305      *
553400040305     C                   CALL      'FIDNB0R'
553500040305     C                   PARM                    kpjba
553600040305     C                   PARM                    FIDNB0DS
553700040305    3c                   endif
553800040305
553900040305    2c                   endif
554000040305
554100040305    1c                   endif
554200070402
554300140205      * se impostata data consegna   o   se previste note LDV obbligatorie
554400131119      * => devo scrivere alcune note R.A. e note ldv
554500070402
554600140205    1c                   if        not_ldv = 'S'  or  §MARnotLdV = 'S'
554700070403      * se devo inserire delle note indica che ho fatto delle modifiche
554800070403      * alla data consegna e quindi impedisco l'uso del f3
554900070403     c                   eval      $bloccof3  =  *on
555000070404      * recupero le note da passare al pgm note ldv
555100070404     c                   clear                   nob
555200070404     c                   clear                   x
555300070402      * recupero le note R.A. dal file e le passo al PGM note LDV
555400070402     c     ktgn          setll     fitgn01l
555500070402     c                   do        *hival
555600070402     c     ktgn          reade     fitgn01l
555700070402
555800070402     c                   if        %eof(fitgn01l)
555900070402     c                   leave
556000070402     c                   endif
556100070402      * passo solo le prime 8(qui non ci sono ancora le modifiche registrate)
556200070402     c                   if        x < 8
556300070402     c                   add       1             x
556400070402     c                   movea     tgnnot        nob(x)
556500070402     c                   else
556600070402     c                   leave
556700070402     c                   endif
556800070402
556900070402     c                   enddo
557000070402
557100131119      * richiamo il programma note LdV in  immissione
557200131206     C                   clear                   Fnlrn6DS
557300131206     c                   eval      in6mod  = 'I'
557400131206     c                   eval      in6aas  = v1anns
557500131206     c                   eval      in6lnp  = v1lnps
557600131206     c                   eval      in6nrs  = v1nrss
557700131206     c                   eval      in6nsp  = v1nsps
557800070402      * note
557900131206     c                   movel     nob(1)        in6not1
558000131206     c                   move      nob(2)        in6not1
558100131206     c                   movel     nob(3)        in6not2
558200131206     c                   move      nob(4)        in6not2
558300131206     c                   movel     nob(5)        in6not3
558400131206     c                   move      nob(6)        in6not3
558500131206     c                   movel     nob(7)        in6not4
558600131206     c                   move      nob(8)        in6not4
558700070402     c
558800131206     c                   eval      in6pgm  = 'FIDNA1R'
558900131206      * se sono una spedizione in consegna passo i dati della distinta del file ARB
559000131206     c                   If        $cons  = *on
559100160104     c     KSPED         chain     Fnarb000
559200131206     c                   If        %found(Fnarb01l)
559300131206     c                   eval      in6fgs = arbifp
559400131206     c                   eval      in6nfv = arbndc
559500131206     c                   eval      in6dfv = arbddc
559600131206     c                   eval      in6pdr = arbpdc
559700131206     c                   eval      in6cons = 'S'
559800131206     c                   Endif
559900131206     c                   Endif
560000070402
560100131206     c                   call      'FNLRN6R'
560200070402     c                   parm                    kpjba
560300131206     c                   parm                    FNLRN6DS
560400070402
560500070402     c                   clear                   not_ldv
560600070412    1c                   endif
560700070402
560800070402      * se devo registrare delle note di modifica data consegna
560900070402      * richiesta scrivo delle note mie nelle note R.A.
561000070402      * Le Faccio uguali a quelle della Rita ?????
561100070402
561200070402      * Decodifico tipo consegna richiesta
561300070402
561400070412    1c                   If        v1cdcr > 0
561500070402     c                   select
561600070402
561700070402     c                   when      v1ctcr = 'P'
561800070404     c                   eval      W_dtcr   ='PRIMA DEL'
561900070402     c                   when      v1ctcr = 'D'
562000070404     c                   eval      W_dtcr   ='DOPO  IL '
562100070402     c                   other
562200070404     c                   eval      W_dtcr   ='      IL '
562300070402
562400070402     c                   endsl
562500070402
562600070402      * creo le note R.A.
562700070402     c                   clear                   nob
562800070402     c                   clear                   x
562900070402     c                   eval      nob(1)='Cons.Rich:'
563000070402     c                   eval      nob(1)=%trim(nob(1))+%trim(w_dtcr)
563100070402     c                   eval      nob(1)=%trim(nob(1))+' '+
563200070402     c                             %editw(v1cdcr:'  /  /    ')
563300141031    2c                   if        v1cocr>0
563400141031     c                   eval      nob(1)=%trim(nob(1))+' ore'
563500141031    3c                   if        v1ctcr=*blanks
563600141031     c                   eval      nob(1)=%trim(nob(1))+' '+
563700141031     c                             %editw(v1cocr:'  :  ')
563800141031     c                   else
563900141031     c                   eval      nob(2)=
564000141031     c                             %editw(v1cocr:'  :  ')
564100141031    3c                   endif
564200141031    2c                   endif
564300070402
564400070402     c                   clear                   not_dcr
564500070402
564600070402      * se valorizzate anche le note DCR con il msg del 48 scrivo
564700070402      * il msg in note R.A.
564800070412    2c                   if        note_dcr <> *blanks
564900141031     c                   if        v1ctcr = *blanks
565000070402     c                   movea     note_dcr      nob(2)
565100141031     c                   else
565200141031     c                   eval      %subst(nob(2):7:29)= %subst(note_dcr:1:29)
565300141031     c                   eval      nob(3) = %subst(note_dcr:30:35)
565400141031     c                   eval      nob(4) = %subst(note_dcr:65:14)
565500141031     c                   endif
565600070402      * visto che il messaggio è lungo 78 dovrebbe riempire 3 note
565700070402     c                   clear                   note_dcr
565800070412    2c                   endif
565900070402      * richiamo in scrittura il pgm delle note
566000070402     c                   clear                   FIDNB0DS
566100070402     c                   eval      db0no1 = nob(1)
566200070402     c                   eval      db0no2 = nob(2)
566300070402     c                   eval      db0no3 = nob(3)
566400070402     c                   eval      db0no4 = nob(4)
566500070402
566600070402     c                   z-add     v1cant        iB0ant
566700070402     c                   z-add     v1cnut        iB0nut
566800070402     c                   z-add     v1cpdt        iB0pdt
566900070404     c                   z-add     999           ib0pgn
567000070402     c                   movel     'GEN'         iB0tIN
567100070402     c                   eval      ib0pot =v1cpot
567200070402     c                   eval      db0pon =Dutpou
567300070402     c                   eval      db0utn =knmus
567400070402      * imposto 'W'
567500070402     c                   movel     'W'           iB0flm
567600070402      *
567700070402     c                   call      'FIDNB0R'
567800070402     c                   parm                    kpjba
567900070402     c                   parm                    fidnb0ds
568000070402
568100070412    1c                   endif
568200140506      *
568300070403      * se c'è stata almeno una riga che ha fatto modificare una data consegna richiesta
568400070403      * impedisco l'uso del tasto funzionale F3
568500070403     c                   if        $bloccof3  =  *on
568600070403     c                   seton                                        63
568700070403     c                   endif
568800140506      *
568900140506      * se è stata inserita almeno una R.A. con Motivo Apertura " 17"
569000151203      *                                   o con Motivo Apertura " 18"
569100141210      *                                   o con Motivo Apertura " 60"
569200140506      * impedisco l'uso del tasto funzionale F3
569300141210     c                   if        $RA_MA_17  =  *on  or
569400151203     c                             $RA_MA_18  =  *on  or
569500141210     c                             $RA_MA_60  =  *on
569600140506     c                   seton                                        63
569700140506     c                   endif
569800040213      *
569900070402
570000040213     c                   ENDSR
570100151201      /free
570200151201
570300151201       //--------------------------------------------------------------
570400151201       //?Scrittura record in TIIDC00F per Reso/Dirottamento.          ?
570500151201       //--------------------------------------------------------------
570600151201       BEGSR  sr_Wrt_TIIDC;
570700151201
570800151201           if  Not %open(TIIDC02L);
570900151201             open  TIIDC02L;
571000151201           endif;
571100151201
571200151201           // -?Reperimento eventuale capofila £6 della FGS?
571300151201           clear  FNLV55ds;
571400151201           d55tpt = '6';
571500151201           if  $FNARB = *on;
571600151201             d55lin = ARBlna;
571700151201           else;
571800151201             d55lin = BLPlna;
571900151201           endif;
572000151201           d55drf = %dec( %date() );
572100151201           FNLV55R ( FNLV55ds );
572200151202
572300151202           // -?Reperimento progressivo richiesta per la "Disposizione di Reso"?
572400151202           exsr  sr_Rep_IDCprg;
572500151201
572600151201           // -?Impostazione campi di TIIDC00F?
572700151201           clear  TIIDC000;
572800151201
572900151201if  2      If  $Ok_IDCprg;
573000151201             IDCprg = k_IDCprg;
573100151201           endif;
573200151201           IDCinsData = %int( %subst( %char( %dec( %timestamp() ) )
573300151201                                      : 1 : 14 ) );
573400151201           IDCmodAcc  = 'B';
573500151201           if  V1Ctch = 'D';
573600151201             IDCtipoCli = V1Ctch;
573700151201           else;
573800151201             IDCtipoCli = 'M';
573900151201           endif;
574000151201           if  $FNARB = *on;
574100151201             IDCwaas  = ARBaas;
574200151201             IDCwlnp  = ARBlnp;
574300151201             IDCwnrs  = ARBnrs;
574400151201             IDCwnsp  = ARBnsp;
574500151201             IDCaas   = ARBaas;
574600151201             IDClnp   = ARBlnp;
574700151201             IDCnrs   = ARBnrs;
574800151201             IDCnsp   = ARBnsp;
574900151201             //IDCfgs   = ARBlna;     ?(vedi capofile £6)?
575000151201           else;
575100151201             IDCwaas  = BLPaas;
575200151201             IDCwlnp  = BLPlnp;
575300151201             IDCwnrs  = BLPnrs;
575400151201             IDCwnsp  = BLPnsp;
575500151201             IDCaas   = BLPaas;
575600151201             IDClnp   = BLPlnp;
575700151201             IDCnrs   = BLPnrs;
575800151201             IDCnsp   = BLPnsp;
575900151201             //IDCfgs   = BLPlna;     ?(vedi capofile £6)?
576000151201           endif;
576100151201           IDCfgs     = d55tfa;
576200151201           IDCtipoDis = '5';
576300151201           IDCnewLna  = *zero;
576400151201           IDCfla     = 'R';
576500151201           IDCelaFlg  = 'S';
576600151201           IDCelaUte  = kNmUs;
576700151201
576800151201           // -?Scrittura record in TIIDC00F?
576900151201           //______________
577000151201           write  TIIDC000;
577100151201           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
577200151201
577300151201e   1  ENDSR;
577400151201
577500151201       //--------------------------------------------------------------
577600151201       //?Reperimento progressivo per disposizione di Reso/Dirottamento?
577700151201       //--------------------------------------------------------------
577800151201       BEGSR  sr_Rep_IDCprg;
577900151201
578000151201         clear  $Ok_IDCprg;
578100151201         clear  keyTIIDC02;
578200151201
578300151201         clear  TRUL33ds;
578400170113         //*·I33tla = 'L';
578500151201         clear  I33ope;
578600151217         I33cnu = 651;
578700151201         I33num = 1;
578800151201
578900151201         DoW  Not  $Ok_IDCprg;
579000151201
579100151201           kpjbu = TRUL33ds;
579200151201           TRUL33R ( kpjba );
579300151201           TRUL33ds = kpjbu;
579400151201
579500151201           if  O33err <> *zeros;
579600151201             leavesr;
579700151201           endif;
579800151201
579900151201           w0090  = O33nri;
580000151217           // -?Così (???) NON viene estratto l'anno di 2 cifre !!!?
580100151217           //k_IDCprg = %int(
580200151217           //             %subst( %editc( %subdt( %date() : *years ) : 'X' )
580300151217           //               : 3 : 2 ) + %editc( w0090 : 'X' ) );
580400151217           w0040  = %subdt( %date() : *years );
580500151217           k_IDCprg = ( %int( %subst( %editc( w0040 : 'X' ) : 3 : 2 ) )
580600151217                        * 1000000000 ) + w0090;
580700151201           setll  %kds( keyTIIDC02 )  TIIDC000;
580800151201
580900151201           // -?Se numero già presente nel file TIIDC ne cerco un altro?
581000151201           if  Not %equal(TIIDC02L);
581100151201             $Ok_IDCprg = *on;
581200151201           endif;
581300151201
581400151201         EndDo;
581500151201
581600151201       ENDSR;
581700151201
581800151201      /end-free
581900040213      *
582000040213      *---------------------------------------------------------------*
582100040213      * Routine caricamento record nel subfile                        *
582200040213      *---------------------------------------------------------------*
582300040213     c     SR_CARSBF     BegSR
582400040213
582500040216      * caricamento dettaglio reclami
582600040217     c                   z-add     *zeros        s01nrr
582700040216
582800040421     c     ktgdan        setll     Fitgd01l
582900040216      *
583000040216     c                   Do        *hival
583100040422     c     ktgdan        reade(N)  fitgd01l
583200040216
583300040216     c                   if        %eof(fitgd01l)
583400040216     c                   leave
583500040216     c                   endif
583600040216
583700040216     c                   add       1             s01nrr
583800040218
583900040218     C
584000040218      * numero riga
584100040217     c                   eval      vs1pdt = tgdpdt
584200040216
584300040216      * - OGGETTO
584400040216
584500040220     c                   clear                   vs1ogg
584600040217
584700040216      * ---------- SPEDIZIONE -----------
584800040216
584900040217     c                   if        tgdtor = 'S'
585000040218      *
585100040218     c                   movel     tgdogg        WRKspe
585200040219     c                   eval      vs1ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
585300040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
585400040219     c                             ' ' + WRKann
585500040218      *
585600040216     c                   endif
585700040216
585800040216      * ----------    ORM     -----------
585900040216
586000040217     c                   if        tgdtor = 'O'
586100040218     c                   movel     tgdogg        WRKorm
586200040218      *
586300040218     c                   eval      vs1ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
586400040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
586500040218     c                             '/' + %trim(WRKnrv)
586600040218      *
586700040216     c                   endif
586800040216
586900040216      * ----------  GENERICO  -----------
587000040216
587100040217     c                   if        tgdtor = 'G'
587200040218     c                   EVAL      VS1ogg = TGDogg
587300040216     c                   endif
587400040218
587500040218      * MOTIVO RECLAMO
587600040218
587700040218     c                   eval      vs1mad = tgdmad
587800040218      * Controllo
587900040218     c                   clear                   tibs02ds
588000040218     C                   MOVEL     'C'           t02mod
588100040218     C                   MOVEL     knsif         t02sif
588200040218     C                   MOVEL     'MAR'         t02cod
588300040218     C                   MOVEL     vs1mad        t02ke1
588400040218      *
588500040218     C                   CALL      'TIBS02R'
588600040218     C                   PARM                    KPJBA
588700040218     C                   PARM                    TIBS02DS
588800040218      *
588900040218    4C                   IF        t02err = *BLANKS
589000040218     C                   movel     T02uni        DMAR
589100040218     C                   movel     §MARdesc      vs1dmr
589200040218     c                   endif
589300141215      *
589400141215      * -?Se il motivo di apertura NON può essere usato: proteggo campo opz.?
589500141215     c                   eval      *in83 =((*in74 = *off  and  §MARutAp = 'N')
589600141215     c                                      or
589700141215     c                                     (wMod <> 'I'   and  §MARutUt = 'N'))
589800040218
589900040218      * CHIUSURA RECLAMO
590000040218
590100040218     c                   eval      vs1cdt = tgdcdt
590200040218
590300040218      * DATA CHIUSURA
590400040218
590500040220     c                   clear                   vs1dcr
590600040220
590700040218     C                   If        tgddcd > *zeros
590800040218     c                   z-add     tgddcd        data8
590900040218     c                   move      anno2         vs1dcr
591000040218     c                   move      mese          mmgg              4 0
591100040218     c                   movel     mmgg          vs1dcr
591200040218     c                   movel     giorno        vs1dcr
591300040218     c                   endif
591400040218
591500040218     c                   write     fia1s01
591600141215     c                   setoff                                       81  83
591700040218
591800040218     c                   enddo
591900040218      *
592000040218      * verifico se inserite righe ed accendo indicatori di emissione
592100040218      *
592200040218     c                   if        s01nrr > 0
592300040218     c                   seton                                        21
592400040218     c                   endif
592500040218
592600040218     c                   seton                                        20
592700040218
592800040213     c                   ENDSR
592900041209      *
593000041209      *---------------------------------------------------------------*
593100041209      * Routine recupero codice chiamante                             *
593200041209      *---------------------------------------------------------------*
593300041209     c     SR_RECCHI     BegSR
593400041209      *
593500041209     c                   clear                   Wrk_cchi
593600041209     c                   clear                   Wrk_cchiA
593700041209     c                   clear                   Wrk_cchiB
593800041209     c                   clear                   Wrk_cchiC
593900041209      *
594000041209      * rileggo il dettaglio
594100041209      *
594200041209
594300041209     c     ktgdan        setll     Fitgd01l
594400041209      *
594500041209     c                   Do        *hival
594600041209     c     ktgdan        reade(N)  fitgd01l
594700041209
594800041209     c                   if        %eof(fitgd01l)
594900041209     c                   leave
595000041209     c                   endif
595100041209
595200041209      * ---------- SPEDIZIONE -----------
595300041209
595400041209    2c                   if        tgdtor = 'S'
595500041209      *
595600041209     c                   movel     tgdogg        Wspevid
595700041230      * se non sono in sede
595800041230     c                   If        not *in05
595900041209      * Aggancio bolle arrivi
596000160104     c     KSPEDw        chain     Fnarb000
596100041209    3c                   If        %found(Fnarb01l)
596200041209      * mittente se valorizzato
596300041209     c                   move      arbccm        w0040
596400041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
596500041210     c                             w0040 <> 9999
596600041209     c                   z-add     arbccm        Wrk_cchiA
596700041209      * se trovato codice mittente prendo quello e scappo
596800041209     c                   leave
596900041209    4c                   endif
597000041209      *
597100041209     c                   z-add     arbksc        W0040
597200041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
597300041209     c                             w0040 <> 9999
597400041209      * in caso di CCM uguale a zeros valorizzo con KSC
597500041209     c                   z-add     arbksc        Wrk_cchiA
597600041209     c                   leave
597700041209    4c                   endif
597800041209      *
597900041209    3c                   endif
598000041209      * Aggancio bolle partenze se non ho trovato ancora nulla
598100160104     c     KSPEDw        chain     Fnblp000
598200041209    3c                   If        %found(Fnblp01l)
598300041209      * mittente se valorizzato
598400041210     c                   move      blpccm        w0040
598500041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
598600041210     c                             w0040 <> 9999
598700041209     c                   z-add     blpccm        Wrk_cchiA
598800041209      * se trovato codice mittente prendo quello e scappo
598900041209     c                   leave
599000041209    4c                   endif
599100041209      *
599200041209     c                   z-add     blpksc        W0040
599300041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
599400041209     c                             w0040 <> 9999
599500041209      * in caso di CCM uguale a zeros valorizzo con KSC
599600041209     c                   z-add     blpksc        Wrk_cchiA
599700041209     c                   leave
599800041209    4c                   endif
599900041209      *
600000041209    3c                   endif
600100041230      * sono in sede
600200041230     c                   else
600300041209      * Aggancio bolle di sede  se non ho trovato ancora nulla
600400041209     c     KSPEDw        chain     titas30c
600500041209    3c                   If        %found(Titas30c)
600600041209      * mittente se valorizzato
600700041209     c                   move      tasccm        w0040
600800041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
600900041210     c                             w0040 <> 9999
601000041209     c                   z-add     tasccm        Wrk_cchiA
601100041209      * se trovato codice mittente prendo quello e scappo
601200041209     c                   leave
601300041209    4c                   endif
601400041209      *
601500041209     c                   z-add     tasksc        W0040
601600041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
601700041209     c                             w0040 <> 9999
601800041209      * in caso di CCM uguale a zeros valorizzo con KSC
601900041209     c                   z-add     tasksc        Wrk_cchiA
602000041209     c                   leave
602100041209    4c                   endif
602200041209      *
602300041209    3c                   endif
602400041209      *
602500041230    3c                   endif
602600041230      *
602700041209    2c                   endif
602800041209
602900041209      * ----------    ORM     -----------
603000041209
603100041209    2c                   if        tgdtor = 'O'
603200041209     c                   movel     tgdogg        Wormvid
603300041209      *
603400160104     c     KORMw         chain     Fnorm000
603500041209    3c                   If        %found(Fnorm01l)
603600041209     c
603700041209      * ordinanante
603800041209     c                   movel     ormcor        w0070
603900041209     c                   move      w0070         w0040
604000041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
604100041209     c                             w0040 <> 9999  and Wrk_cchiA = 0
604200041209     c                   z-add     w0070         Wrk_cchiA
604300041209     c                   endif
604400041209
604500041209      * ritiro
604600041209     c                   movel     ormcra        w0070
604700041209     c                   move      w0070         w0040
604800041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
604900041209     c                             w0040 <> 9999 and Wrk_cchiB = 0
605000041209     c                   z-add     w0070         Wrk_cchiB
605100041209     c                   endif
605200041209
605300041209      * destinatario
605400041209     c                   movel     ormcrc        w0070
605500041209     c                   move      w0070         w0040
605600041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
605700041209     c                             w0040 <> 9999 and Wrk_cchiC = 0
605800041209     c                   z-add     w0070         Wrk_cchiC
605900041209     c                   endif
606000041209    3c                   endif
606100041209    2c                   endif
606200041209
606300041209      * ----------  GENERICO  -----------
606400041209      * leggo il successivo
606500041209    2c                   if        tgdtor = 'G'
606600041209     c                   iter
606700041209    2c                   endif
606800041209
606900041209     c                   enddo
607000041209
607100041209     c                   endsr
607200040203      *
607300040129      *---------------------------------------------------------------*
607400040212      * Routine pulizia  videata Immissione Reclamo  x nuovo reclamo  *
607500040129      *---------------------------------------------------------------*
607600040129     c     SR_PUL01      BegSR
607700040129      *
607800040421     C                   clear                   v1cant
607900040416     C                   clear                   v1cnut
608000040421      * accendo indicatore che non mette in inversione fondo e sottolineato il numero reclamo
608100040421     c                   seton                                        15
608200040421      *
608300040202     C                   clear                   v1ctco
608400040202     C                   clear                   v1dtco
608500040202     C                   clear                   v1cdar
608600040202     C                   clear                   v1corr
608700040202     C                   clear                   v1cchi
608800050118     C                   clear                   v1dchi
608900040202     C                   clear                   v1crsc
609000160701     C                   clear                   V1naz
609100160701     C                   clear                   V1prov
609200040129     C                   clear                   v1cclv
609300040129     C                   clear                   v1refe
609400040129     C                   clear                   v1ntel
609500040129     C                   clear                   v1mail
609600040129     C                   clear                   v1nfax
609700040218     C                   clear                   W_v1cchi
609800040219     C                   clear                   rec_wrt
609900041207     c                   clear                   Wrk_cchi
610000041207     c                   clear                   Wrk_cchiA
610100041207     c                   clear                   Wrk_cchiB
610200041207     c                   clear                   Wrk_cchiC
610300041207     c                   clear                   SW_v1cchi
610400070413     c                   eval      $bloccof3 = *off
610500160512     c                   eval      $Note_scritte = *off
610600140506     c                   eval      $RA_MA_17 = *off
610700151203     c                   eval      $RA_MA_18 = *off
610800141210     c                   eval      $RA_MA_60 = *off
610900151201     c                   clear                   $Ok_IDCprg
611000151202     c                   clear                   $WrtTIIDC
611100070403     c                   setoff                                       63
611200041210      * Imposto il numeratore
611300041210     c                   z-add     1             SV_pdt
611400040213      * pulizia video relativo oggetto
611500040213     c                   exsr      sr_pulctl
611600040213      *
611700040202      * pulizia subfile
611800040202     c                   clear                   s01nrr
611900150310      * accendo indicatore di posizionamento cursore su CHIAMANTE
612000150310     c                   seton                                        44
612100040213      * imposto il tipo comunicazione
612200040213     c                   eval      v1ctco = 'T'
612300040213      * decodifico
612400150625     c                   clear                   dCOR
612500040217     c                   clear                   tibs02ds
612600040213     C                   MOVEL     'C'           t02mod
612700040213     C                   MOVEL     knsif         t02sif
612800040213     C                   MOVEL     'COR'         t02cod
612900040213     C                   MOVEL     v1ctco        t02ke1
613000040213      *
613100040213     C                   CALL      'TIBS02R'
613200040213     C                   PARM                    KPJBA
613300040213     C                   PARM                    TIBS02DS
613400040213      *
613500040213    4C                   IF        t02err = *BLANKS
613600040213     C                   movel     T02uni        Dcor
613700040216     C                   movel     §CORdesc      v1dtco
613800040213      *
613900040213     C                   ELSE
614000040216     C                   clear                   v1dtco
614100040213    4C                   ENDIF
614200040203      *
614300040129     c                   endsr
614400040213      *
614500040213      *---------------------------------------------------------------*
614600040213      * Routine pulizia  videata Immissione Reclamo  x nuova riga     *
614700040213      *---------------------------------------------------------------*
614800040213     c     SR_PULctl     BegSR
614900040213      *
615000141215     c                   exsr      sr_PULctl2
615100040216      *
615200040216     c                   setoff                                       2120
615300040216     c                   seton                                        22
615400040216     c                   write     fia1c01
615500040216     c                   setoff                                       22
615600040213      *
615700040213     c                   ENDSR
615800141215      *
615900141215      *---------------------------------------------------------------*
616000141215      *
616100141215     c     SR_PULctl2    BegSR
616200141215      *
616300141215     C                   clear                   forz_rec
616400141215     C                   clear                   v1ctor
616500141215     C                   clear                   v1poeo
616600141215     C                   clear                   v1nsor
616700141215     C                   clear                   v1norm
616800141215     C                   clear                   v1nrvo
616900141215     C                   clear                   v1cogg
617000141215     C                   clear                   v1lnps
617100141215     C                   clear                   v1nrss
617200141215     C                   clear                   v1nsps
617300141215     C                   clear                   v1anns
617400141215     C                   clear                   v1ctch
617500141215     C                   clear                   v1dtch
617600141215     C                   clear                   W_cmad
617700150122     C                   clear                   wSaveMAD
617800141215     C                   clear                   v1cmad
617900141215     C                   clear                   v1dmad
618000141215     C                   clear                   v1ccdt
618100141215     C                   clear                   v1dcdt
618200141215     C                   clear                   v1not1
618300141215     C                   clear                   v1not2
618400141215     C                   clear                   v1not3
618500141215     C                   clear                   v1not4
618600150313     C                   clear                   v1crpg
618700150316     C                   clear                   v1cpog
618800150313     C                   clear                   v1cprt
618900150313     C                   clear                   v1dprt
619000141215     C                   clear                   v1crsp
619100141215     C                   clear                   v1cpor
619200141215     C                   clear                   v1cdcr
619300141215     C                   clear                   v1cocr
619400141215     C                   clear                   v1ctcr
619500141215     c                   clear                   v1ccva
619600141215     C                   clear                   v1dcva
619700151223     c                   clear                   V1CfilR
619800151223     c                   clear                   V1CrepR
619900160129     c                   clear                   V1Cafg
620000160129     c                   clear                   V1Carg
620100141215     c                   clear                   W_por
620200141215     C                   clear                   v1dpor
620300141215     C                   clear                   Wormvid
620400141215     C                   clear                   Wspevid
620500141215     C                   clear                   WRK_recsbf
620600141215     C                   clear                   WFIDNB0R
620700141215     c                   clear                   not_dcr
620800141215     c                   clear                   not_ldv
620900141215     c                   clear                   note_dcr
621000151217     c                   clear                   wSavePGN
621100141215      * pulisco i campi di lavoro
621200141215     c                   clear                   wforz
621300160601     c                   eval      $Note_scritte= *off
621400160601     c                   eval      $ric_fnlrg2r = *off
621500141215     c                   eval      $riemetti    = *off
621600141215     c                   eval      $fnarb       = *off
621700141217     c                   eval      $RA_MA_60    = *off
621800150326     c                   clear                   W_prt
621900160607      *
622000160215     c                   clear                   dTGD01
622100160215     c                   clear                   dTGDfar
622200160215     c                   clear                   dTGR01A
622300160607      *
622400141215      * accendo indicatore di emissione dell'oggetto generico
622500141215     c                   seton                                        03
622600141215     c                   setoff                                       0102
622700141215     c                   setoff                                       80
622800141215      * data consegna riochiesta
622900141215     c                   setoff                                       616074
623000141215      * pulizia indicatori di protezione
623100141215     c                   setoff                                       515253
623200141215     c                   setoff                                       54
623300141215      * spengo indicatore di note aggiuntive
623400141215     c                   setoff                                       29
623500150624      *
623600150624      * -?Spegnimento indicatore di protezione "Tipo Comunicazione"?
623700150624     c                   eval      *in73 = *off
623800151210      *
623900151210      * -?Spegnimento indicatori di gestione "Richiesta Risposta"?
624000151211     c                   eval      *in18 = *off
624100141215      *
624200141215      * imposto il numero progressivo riga solo se sono in immissione
624300141215     c                   if        wmod  = 'I'
624400141215      *
624500141215     c                   z-add     sv_pdt        v1cpdt
624600150313      * imposto la priorità a '1' quando sono in immissione
624700150313     c                   eval      V1Cprt = '1'
624800141215      * Decodifico
624900141215     c                   clear                   tibs02ds
625000141215     C                   MOVEL     'C'           t02mod
625100141215     C                   MOVEL     knsif         t02sif
625200141215     C                   MOVEL     'PRR'         t02cod
625300141215     C                   MOVEL     v1cprt        t02ke1
625400141215      *
625500141215     C                   CALL      'TIBS02R'
625600141215     C                   PARM                    KPJBA
625700141215     C                   PARM                    TIBS02DS
625800141215      *
625900141215     C                   if        t02err = *BLANKS
626000141215     C                   movel     T02uni        DPRR
626100141215     C                   movel     §PRRdesc      v1dprt
626200141215     c                   endif
626300160607      *
626400150313      * Imposto utente e filiale gestione
626500150313     c                   IF        w_pot > 0
626600150313     c                   eval      V1Cpog = %editc(w_pot:'X')
626700150313     c                   ELSE
626800150313     c                   eval      V1Cpog = %editc(DUTpou:'X')
626900150313     c                   ENDIF
627000150313     c                   eval      V1Crpg = KNMUS
627100141215      *
627200141215     c                   endif
627300141215      *
627400141215     c                   EndSR
627500141215      *
627600040218      *****************************************************************
627700040218      *  Gestione del subfile Manutenzione riga reclamo
627800040218      *****************************************************************
627900040218     c     SR_GESSBF     begsr
628000160607      *
628100040218     c                   do        *hival
628200160607      *
628300040218     c                   readc     fia1s01
628400150616     c                   eval      *in84 = *off
628500160607      *
628600161027     c                   If        %eof(fidnA1d)
628700040218     c                   leave
628800040218     c                   endif
628900160607      *
629000040218     c                   if        vs1opz = '2'
629100160607      *
629200150317      /free
629300150317       //?Controllo se l'utente è abiliato alla filiale gestione della RA da manutenzionare
629400150616       //?(SE NON SONO IN SEDE)
629500150317         *in84 = *off;
629600150317         chain(n) (V1Cant:V1Cnut:VS1pdt) FITGD01L;
629700150317         IF  %found(FITGD01L) and
629800150616             NOT *in05        and
629900150318             %lookup(%editc(TGDpog:'X'):sk_wFIL) = 0;
630000150317           *in84 = *on;
630100151223           *in28 = *on;
630200160225           //V1Cmsg = 'Richiesta NON gestibile dall''utente';
630300160225           V1Cmsg = Msg(24);
630400151223           clear  VS1opz;
630500150317           update FIA1S01;
630600150317           leave;
630700150317         ENDIF;
630800150724
630900150724         // -?Controllo anche se R.A. con Motivo Apertura = " 70"?
631000150724         //  ?(Reclamo da Sede) - in gestione ad una filiale?
631100150724         *in84 = *off;
631200150724         If  TGDmad = c_ReclSede  and  TGDpog <> %int(c_Sede)  and  *in05;
631300150724           *in84 = *on;
631400151223           *in28 = *on;
631500160225           //V1Cmsg = 'Richiesta NON gestibile dall''utente';
631600160225           V1Cmsg = Msg(24);
631700151223           clear  VS1opz;
631800150724           update  FIA1S01;
631900150724           leave;
632000150724         EndIf;
632100150317      /end-free
632200160607      *
632300040218      * valorizzo un campo di comodo per differenziare un recupero da subfile
632400040218      * da un recupero da richiamo da altro PGM
632500040218     c                   eval      WRK_recsbf = 'Y'
632600160607      *
632700040421     c                   eval      keyant = v1cant
632800040218     c                   eval      keynut = v1cnut
632900040218     c                   eval      keypdt = vs1pdt
633000040218     c                   exsr      SR_recogg
633100141215     c  n82              clear                   vs1opz
633200040218     c                   update    fia1s01
633300040218      * esco dal ciclo per la gestione del dettaglio scelto
633400160607      *
633500040218     c                   leave
633600040218     c                   endif
633700160607      *
633800040218     c                   enddo
633900040218      *
634000040218     c                   ENDSR
634100040218      *****************************************************************
634200040218      *  Recupero Dettaglio Reclamo in manutenzione
634300040218      *****************************************************************
634400040218     c     SR_RECOGG     begsr
634500040303
634600040303     c                   setoff                                       515253
634700040422     c                   setoff                                       547082
634800151223     c                   setoff                                       748384
634900150625      * -?Proteggo il Tipo Comunicazione SE sono in MODIFICA?
635000150625     c                   eval      *in73 = (wMod <> 'I')
635100150326
635200150326     c                   clear                   W_prt
635300151211     c                   clear                   dTGD01
635400151211     c                   clear                   dTGDfar
635500160126     c                   clear                   dTGR01A
635600040218      *
635700040422     c     Ktgdrec       chain(e)  fitgd01l
635800040218
635900040422    1c                   if        %error
636000040422      * emetto l'errore
636100040422     c                   seton                                        82
636200151223     c                   eval      *in28 = *on
636300160225     c*//                eval      V1Cmsg = 'Richiesta NON gestibile +
636400160225     c*//                                    dall''utente perchè R.A. +
636500160225     c*//                                    impegnata da altro utente'
636600160225     c                   eval      V1Cmsg = Msg(25)
636700040422     c                   else
636800040422      *
636900151210   1Bc                   if        %found(fitgd01l)
637000040218
637100040218      * numero riga
637200040218     c                   eval      v1cpdt = tgdpdt
637300040218
637400040218      * - OGGETTO
637500040218
637600040218     c                   eval      v1ctor = tgdtor
637700040218
637800040218      * ---------- SPEDIZIONE -----------
637900040218
638000040218    2c                   if        tgdtor = 'S'
638100040218     c                   movel     tgdogg        spevid
638200040218     c                   seton                                        01
638300040218     c                   setoff                                       0203
638400140204     c  n05              exsr      CTR_specons
638500040218      *
638600040218    2c                   endif
638700040218
638800040218      * ----------    ORM     -----------
638900040218
639000040218    2c                   if        tgdtor = 'O'
639100040218     c                   movel     tgdogg        ormvid
639200040218     c                   seton                                        02
639300040218     c                   setoff                                       0103
639400040218    2c                   endif
639500040218
639600040218      * ----------  GENERICO  -----------
639700040218
639800040218    2c                   if        tgdtor = 'G'
639900040218     c                   movel     tgdogg        v1cogg
640000040218     c                   seton                                        03
640100040218     c                   setoff                                       0102
640200040218    2c                   endif
640300040218      * chi chiama
640400040218     c                   eval      v1ctch = tgdtch
640500040304     c                   clear                   tibs02ds
640600040304     C                   MOVEL     'C'           t02mod
640700040304     C                   MOVEL     knsif         t02sif
640800040304     C                   MOVEL     'CCR'         t02cod
640900040304     C                   MOVEL     v1ctch        t02ke1
641000040304      *
641100040304     C                   CALL      'TIBS02R'
641200040304     C                   PARM                    KPJBA
641300040304     C                   PARM                    TIBS02DS
641400040304      *
641500040304     C                   movel     T02uni        DCCR
641600040304     c                   movel     §ccrdesc      v1dtch
641700040218      * motivo
641800040218     c                   eval      v1cmad = tgdmad
641900150410     c                   eval      wSaveMAD = V1Cmad
642000040218      * Decodifica
642100040218     c                   clear                   tibs02ds
642200040218     C                   MOVEL     'C'           t02mod
642300040218     C                   MOVEL     knsif         t02sif
642400040218     C                   MOVEL     'MAR'         t02cod
642500040218     C                   MOVEL     v1cmad        t02ke1
642600040218      *
642700040218     C                   CALL      'TIBS02R'
642800040218     C                   PARM                    KPJBA
642900040218     C                   PARM                    TIBS02DS
643000040218      *
643100040218    2C                   IF        t02err = *BLANKS
643200040218     C                   movel     T02uni        DMAR
643300040218     C                   movel     §MARdesc      v1dmad
643400070323      * se è un motivo di apertura che deve essere protetto in manutenzione accendo il 74
643500070323     c                   if        §marprot = 'S'
643600070323     c                   eval      *in74 = *on
643700070323     c                   endif
643800070323
643900040218    2c                   endif
644000040218      * chiusura reclamo
644100040218     c                   eval      v1ccdt = tgdcdt
644200040218      * Decodifica
644300040218     c                   clear                   tibs02ds
644400040218     C                   MOVEL     'C'           t02mod
644500040218     C                   MOVEL     knsif         t02sif
644600040218     C                   MOVEL     'CHR'         t02cod
644700040218     C                   MOVEL     v1ccdt        t02ke1
644800040218      *
644900040218     C                   CALL      'TIBS02R'
645000040218     C                   PARM                    KPJBA
645100040218     C                   PARM                    TIBS02DS
645200040218      *
645300040218    2C                   IF        t02err = *BLANKS
645400040218     C                   movel     T02uni        DCHR
645500040218     C                   movel     §CHRdesc      v1dcdt
645600040218    2c                   endif
645700150624      *
645800040218      * recupero le note
645900040218      *
646000040218     C                   clear                   FIDNB0DS
646100040421     c                   z-add     v1cant        iB0ant
646200040218     c                   z-add     v1cnut        iB0nut
646300040218     c                   z-add     v1cpdt        iB0pdt
646400040218     c                   movel     'GEN'         iB0tIN
646500040218     c                   movel     'R'           iB0flm
646600040218      *
646700040218     C                   CALL      'FIDNB0R'
646800040218     C                   PARM                    kpjba
646900040218     C                   PARM                    FIDNB0DS
647000040218      *
647100040218      * valorizzo le prime 4 note a video
647200040218      *
647300040218     c                   movel     DB0NO1        v1not1
647400040218     c                   movel     DB0NO2        v1not2
647500040218     c                   movel     DB0NO3        v1not3
647600040218     c                   movel     DB0NO4        v1not4
647700040309      * in caso di manutenzione non do la possibilità di modifcare niente
647800040309     c                   if        Wmod = 'G'
647900040309     c                   seton                                        515253
648000040309     c                   seton                                        54
648100040309     c                   ENDIF
648200070413      * se è un motivo di apertura che deve essere protetto in manutenzione proteggo le note
648300070413     c                   if        §marprot = 'S' and wmod = 'I'
648400070413     c                   eval      *in51 = db0no1 <> *blanks
648500070413     c                   eval      *in52 = db0no2 <> *blanks
648600070413     c                   eval      *in53 = db0no3 <> *blanks
648700070413     c                   eval      *in54 = db0no4 <> *blanks
648800070413     c                   endif
648900040218      *
649000040218      * se ob0piu è valorizzato ci sono + di 4 righe
649100040218     c                   eval      *in29 = (OB0PIU = '+')
649200151217      *
649300151217      * -?Memorizzo ULTIMO Progressivo Note inserito per la R.A.?
649400151217     c                   clear                   wSavePGN
649500151217     c     kTGN          setGT     FITGN000
649600151217     c     kTGN          readPE    FITGN000
649700151217     c                   if        NOT %eof(FITGN01L)
649800151217     c                   eval      wSavePGN = TGNpgn
649900151217     c                   endif
650000151217      *
650100040218      * reparto gestione
650200150312     c                   eval      v1cpog = %editc(tgdpog:'X')
650300040218     c                   eval      v1crpg = tgdrpg
650400040218      * priorità
650500040218     c                   eval      v1cprt = tgdprt
650600150326     c                   eval      W_prt  = tgdprt
650700150313      * se non è chiuso imposto utente e filiale da azute
650800150313      * e priorità a '1' se è a '0'
650900160129if  2c                   IF        V1Ccdt = *blanks
651000150313     c                   eval      V1Crpg = KNMUS
651100160129     c                   if        w_POt  = *zero
651200160129     c                   eval      V1Cpog = %editc( DUTpou : 'X' )
651300160129     c                   else
651400160129     c                   eval      V1Cpog = %editc( w_POt : 'X' )
651500160129     c                   endif
651600150313     c                   IF        V1Cprt = '0'
651700150313     c                   eval      V1Cprt = '1'
651800150313     c                   ENDIF
651900160129e   2c                   ENDIF
652000040323      *
652100040323      * Decodifico
652200040323     c                   clear                   tibs02ds
652300040323     C                   MOVEL     'C'           t02mod
652400040323     C                   MOVEL     knsif         t02sif
652500040323     C                   MOVEL     'PRR'         t02cod
652600040323     C                   MOVEL     v1cprt        t02ke1
652700040323      *
652800040323     C                   CALL      'TIBS02R'
652900040323     C                   PARM                    KPJBA
653000040323     C                   PARM                    TIBS02DS
653100040323      *
653200040323    4C                   IF        t02err = *BLANKS
653300040323     C                   movel     T02uni        DPRR
653400040323     C                   movel     §PRRdesc      v1dprt
653500040323     c                   endif
653600150625      *
653700040218      * responsabilità
653800040218     c                   eval      v1crsp = tgdrsp
653900040218      *
654000040218      * P.O. responsabilità
654100040218     c                   movel     tgdpor        v1cpor
654200040218      * decodifica
654300040218    2c                   if        tgdpor > 0
654400160104     C     tgdpor        chain     azorg
654500040218    3C                   if        %found(azorg01l)
654600040218     C                   movel     orgdes        v1dpor
654700040218    3c                   endif
654800040218    2c                   endif
654900151211      *
655000160104      * -?Richiesta/Ricevuta Risposta?
655100151214     c                   if        TGDflo <> *blank
655200160218if  3c                   if        %subst( TGDflo : 2 : 3) > *zero
655300151211     c                   eval      dTGD01  = TGDflo
655400160218x   3c                   else
655500160218     c                   eval      dTGD01.§TGDaRap =
655600160218     c                             %subst( TGDflo : 1 : 1)
655700160218e   3c                   endif
655800151214     c                   endif
655900151211     c                   eval      dTGDfar = TGDfar
656000040218
656100151210   1Bc                   endif
656200160108
656300160108      /free
656400160108
656500160128         // -?Filiale che Attende Risposta (o che l'ha già Ricevuta)?
656600160108         clear  V1CfilR;
656700160128         if  dTGD01.§TGDfilR > *zero;
656800160128           V1CfilR = %editc( dTGD01.§TGDfilR : 'X' );
656900160128         endif;
657000160108
657100160128         // -?Reparto che Attende Risposta (o che l'ha già Ricevuta)?
657200160128         clear  V1CrepR;
657300160128         TGRtri = 'A';
657400160128         chain(N)  ( TGDant : TGDnut : TGDpdt : TGRtri )  FITGR000;
657500160128         if  %found(FITGR01L);
657600160128           dTGR01A = TGRrif;
657700160128         endif;
657800160128         V1CrepR = dTGR01A.§TGRrepR;
657900160108
658000160128         // -?Visualizzazione "Attesa o Ricevuta Risposta"?
658100160128         clear  V1AttRis;
658200160128         Select;
658300160203           When  dTGDfar.§TGDatt = 'S'  and  dTGDfar.§TGDris = 'S';
658400160201             exsr  sr_ReperimentoRispostaRicevuta;
658500160201             if  WrepR  = *blank;
658600160203               V1AttRis = 'Risposta di Ritorno   RICEVUTA   dal Reparto '
658700160203                        + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
658800160203                        + %editc( dTGD01.§TGDfilR : 'X' );
658900160201             else;
659000160203               V1AttRis = 'Risposta di Ritorno   RICEVUTA   dall''Utente '
659100160201                        + %trimR( WrepR ) + ' della Filiale '
659200160201                        + WfilR;
659300160201             endif;
659400160128           When  dTGDfar.§TGDatt = 'S';
659500160203             V1AttRis = 'Risposta di Ritorno   ATTESA   dal Reparto '
659600160203                      + %trimR( dTGR01a.§TGRrepR ) + ' della Filiale '
659700160203                      + %editc( dTGD01.§TGDfilR : 'X' );
659800160128         EndSl;
659900160129
660000160129         // -?Visualizzazione Reparto/Filiale che ha attualmente in?
660100160129         //  ?gestione la R.A.?
660200160129         //If  dTGDfar.§TGDatt = 'S';
660300160129           if  V1Crpg <> TGDrpg;
660400160129              V1Carg = '(' + %trimR( TGDrpg ) + ')';
660500160129           endif;
660600160129           if  V1Cpog <> %editc( TGDpog : 'X' );
660700160129              V1Cafg = '(' + %editc( TGDpog : 'X' ) + ')';
660800160129           endif;
660900160129         //EndIf;
661000160108
661100160108      /end-free
661200151211
661300040324      * posizionamento cursore su causale chiusura
661400040324     c                   seton                                        37
661500040324      * Se reclamo chiuso controllo la data chiusura se sono passati più dei
661600040324      * giorni massimi per variare un reclamo chiuso (definiti nella tabella
661700040324      * dati standard REC) proteggo tutti i campi ed emetto un msg di avvertimento
661800041012      * Se utente abilitato a modificare tutto (esempio uff. qualità) non faccio il
661900041012      * controllo (*in07 acceso)
662000041012     c                   if        tgddcd > *zeros and *in50 and not *in07
662100040426
662200040324     c     *iso          movel     tgddcd        Wdataeur
662300040426     c                   z-add     0             giorni
662400040426      *
662500040426     c                   dow       giorni < §recnmgm
662600040426     c                   adddur    1:*d          Wdataeur
662700040426     c                   extrct    wdataeur:*y   kann
662800040426     c                   extrct    wdataeur:*m   kmes
662900040426     c                   extrct    wdataeur:*d   wgiorno
663000040426     c                   clear                   ktfa
663100040426     c                   eval      ktfp = tgdpot
663200160104     c     kazcln        chain     azcln000
663300040426     c                   if        not%found(azcln01l)
663400040426     c                   leave
663500040426     c                   endif
663600040426     c                   if        MAT(wgiorno) =  'F'
663700040426     c                             or  POM(wgiorno) =  'F'
663800040426     c                   iter
663900040426     c                   else
664000040426     c                   add       1             giorni
664100040426     c                   endif
664200040426     c                   enddo
664300040426      *
664400040324     c                   if        wdataeur < wdataoggi
664500040324      * se non sono in immissione proteggo tutti i campi
664600040324     c                   eval      *in70 = *on
664700151223     c                   eval      *in28 = *on
664800151223     c                   eval      V1Cmsg = 'Richiesta chiusa non è più +
664900151223     c                                       modificabile'
665000040324      * posizionamento cursore su opzione subfile
665100141215      * ·?SE non protetta?
665200141215     c                   eval      *in81 = ((*in74 = *on or  §MARutAp <> 'N')
665300141215     c                                       and
665400141215     c                                      (wMod = 'I'  or  §MARutUt <> 'N'))
665500040324     c                   eval      *in37 = *off
665600040324      *
665700040324     c                   endif
665800040324      *
665900040324     c                   endif
666000040422      *
666100151210    1c                   endif
666200150319
666300150319      /free
666400150319       //?Controllo se attivare o no il tasto F11=Variazioni
666500150319         *in17 = *off;
666600150319         chain (V1Cant:V1Cnut:VS1pdt) FITGM01L;
666700150319         IF  %found(FITGM01L);
666800150319           *in17 = *on;
666900150319         ENDIF;
667000150319      /end-free
667100150319
667200040218      *
667300040218     c                   ENDSR
667400160201
667500160201      /free
667600160201
667700160201       BEGSR  sr_ReperimentoRispostaRicevuta;
667800160201
667900160201         clear  WrepR;
668000160201         clear  WfilR;
668100160201
668200160204         setGT  ( V1Cant : V1Cnut : V1Cpdt : 'A' )  FITGM000;
668300160204         readPE ( V1Cant : V1Cnut : V1Cpdt : 'A' )  FITGM000;
668400160201
668500160204         if  %eof( FITGM01L );
668600160201           leavesr;
668700160201         endif;
668800160201
668900160201         WrepR = TGMutn;
669000160201         WfilR = %editc( TGMpon : 'X' );
669100160201
669200160201       ENDSR;
669300160201
669400160201      /end-free
669500160128
669600040219      *---------------------------------------------------------------*
669700040219      * Routine chiusura chiamata                                     *
669800040219      *---------------------------------------------------------------*
669900040219     c     SR_CHIUDO     BegSR
670000040219
670100040219      * per ogni record di dettaglio reclamo aggiorno i dati della parte alta
670200040219      * del video
670300040219
670400040421     c     Ktgdan        setll     Fitgd01l
670500040219      *
670600040219     c                   Do        *hival
670700160216     c     Ktgdan        reade(E)  fitgd01l
670800040219
670900160216      * -?Record allocato?
671000160216     c                   if        %error
671100160216     c     kTGDan        reade(N)  FITGD000
671200160216     c                   iter
671300160216     c                   endif
671400160216      * -?End Of File?
671500040219     c                   if        %eof(fitgd01l)
671600040219     c                   leave
671700040219     c                   endif
671800040219
671900040219     c                   eval      tgddar = wdar                                dta ricez. comunicaz
672000040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
672100040219     c                   eval      tgdtco = v1ctco                              tipo comunicazione
672200040219     c                   eval      tgdrsc = v1crsc                              rag.sociale chiamant
672300041213      * se codice cliente diverso da prima scrivo una nota
672400041214     c                   if        tgdksc <> v1cchi and mod_testa = 'S'
672500150318     c                   eval      wtmo = 'K'
672600150318     c                   clear                   dTGMk
672700150318     c                   eval      §MODksco = TGDksc
672800150318     c                   eval      §MODkscn = V1Cchi
672900150318     c                   exsr      WRT_modifiche
673000041213     c                   endif
673100041213
673200041231     c                   eval      tgdksc = v1cchi                              codice cliente
673300040219     c                   eval      tgdclv = v1cclv                              categoria chiamante
673400121008
673500121008      * se il motivo di apertura lo richiede NON memorizzo il KSC
673600121008     c                   clear                   tibs02ds
673700121008     c                   eval      T02mod = 'C'
673800121008     c                   eval      T02sif = knsif
673900121008     c                   eval      T02cod = 'MAR'
674000121008     c                   eval      T02ke1 = TGDmad
674100121008     c                   call      'TIBS02R'
674200121008     c                   parm                    KPJBA
674300121008     c                   parm                    TIBS02DS
674400121008     c                   eval      dMAR = T02uni
674500121008     c                   IF        §MARksc = 'N'
674600121008     c                   clear                   TGDksc
674700121008     c                   ENDIF
674800150625      *
674900150625      * -?SE premuto F4=Fine chiamata (abilitato SOLO in immissione)?
675000150625      *  ? & R.A. chiusa?
675100150625      *  ? & Tipo Comunicazione con Stato (ex Priorità) in tab. "COR"?
675200150625      *  ?=> lo imposto in TGDPRT?
675300150625     c                   clear                   wSavePRT
675400150625     c                   if             *inKD
675500150625     c                             and  TGDcdt    <> *blanks
675600150625     c                   eval      wSavePRT = TGDprt
675700150625     c                   if             §CORstato <> *blanks
675800150625     c                   eval      TGDprt = §CORstato
675900150625     c                   else
676000150625     c                   eval      TGDprt = '9'
676100150625     c                   endif
676200150625      /free
676300150625         // -?Storicizzo Variazioni Stato?
676400150625         IF  wSavePRT <> TGDprt;
676500150625           wtmo = 'S';
676600150625           clear dTGMs;
676700150625           §MODprto = wSavePRT;
676800150625           §MODprtn = TGDprt;
676900150625           exsr WRT_modifiche;
677000150625         ENDIF;
677100150625      /end-free
677200150625     c                   endif
677300040219
677400040219     c                   update    fitgd000
677500040219
677600040219     c                   enddo
677700040219
677800040219      * verifico se esistono per questa riga che confermo dei record di riferimenti
677900040219      * ed evantualmente li deleto e poi riscrivo
678000040219
678100040421     c     ktgran        setll     fitgr01l
678200040219      *
678300040219     c                   do        *hival
678400040421     c     ktgran        reade     fitgr01l
678500040219      *
678600040219     c                   if        not %eof(fitgr01l)
678700040219     c                   delete    fitgr01l
678800040219     c                   else
678900040219     c                   leave
679000040219     c                   endif
679100040219      *
679200040219     c                   enddo
679300040219      * se esiste referente
679400040219     c                   if        v1refe <> *blank
679500040219     c                   clear                   fitgr000
679600040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
679700040421     c                   eval      tgrant = v1cant                              Numero reclamo
679800040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
679900040219     c                   eval      tgrtri = 'R'                                 tipo  riferimento
680000040219     c                   movel     v1refe        tgrrif
680100040219     c                   write     fitgr000
680200040219     c                   endif
680300040219      * se esiste indirizzo e-mail
680400040219     c                   if        v1mail <> *blank
680500040219     c                   clear                   fitgr000
680600040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
680700040421     c                   eval      tgrant = v1cant                              Numero reclamo
680800040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
680900040219     c                   eval      tgrtri = 'M'                                 tipo  riferimento
681000040219     c                   movel     v1mail        tgrrif
681100040219     c                   write     fitgr000
681200040219     c                   endif
681300040219      * se esiste numero telefono e/o numero fax
681400040219     c                   if        v1ntel <> *blank or v1nfax <> *blank
681500040219     c                   clear                   fitgr000
681600040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
681700040421     c                   eval      tgrant = v1cant                              Numero reclamo
681800040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
681900040219     c                   eval      tgrtri = 'N'                                 tipo  riferimento
682000040219     c                   movel     v1ntel        tgrrif
682100040219     c                   move      v1nfax        tgrrif
682200040219     c                   write     fitgr000
682300040219     c                   endif
682400040219      *
682500040219     c                   ENDSR
682600040219      *
682700040216      *****************************************************************
682800040216      *  Recupero Reclamo in manutenzione
682900040216      *****************************************************************
683000040218     c     SR_RECUPERO   begsr
683100041213      *
683200041213     c                   clear                   mod_testa
683300151202     c                   clear                   $WrtTIIDC
683400150625      *
683500150625      * -?In MODIFICA: proteggo SEMPRE il Tipo Comunicazione?
683600150625     c                   eval      *in73  = *on
683700040302
683800040302      * preparo la chiave con i dati passati
683900040302
684000040421     c                   eval      keyant = ia0ant
684100040302     c                   eval      keynut = ia0nut
684200040302     c                   eval      keypdt = ia0pdt
684300040304     c                   eval      v1cpdt = ia0pdt
684400040304     c                   eval      v1cnut = ia0nut
684500040421     c                   eval      v1cant = ia0ant
684600040304      * carico il subfile
684700040304     c                   exsr      SR_CARSBF
684800040304
684900040422      * dettaglio
685000040422     c                   exsr      SR_recogg
685100040422
685200040302      * valorizzo anche la parte alta della videata
685300040302
685400040303     c                   eval      v1cpot = tgdpot
685500040303     c                   eval      v1ctco = tgdtco
685600040303      * decodifico
685700150625     c                   clear                   dCOR
685800040303     c                   clear                   tibs02ds
685900040303     C                   MOVEL     'C'           t02mod
686000040303     C                   MOVEL     knsif         t02sif
686100040303     C                   MOVEL     'COR'         t02cod
686200040303     C                   MOVEL     v1ctco        t02ke1
686300040303      *
686400040303     C                   CALL      'TIBS02R'
686500040303     C                   PARM                    KPJBA
686600040303     C                   PARM                    TIBS02DS
686700040303      *
686800040303     C                   movel     T02uni        DCOR
686900040303     C                   movel     §CORdesc      v1dtco
687000040303
687100040303     c                   eval      G02inv = tgddar
687200040303     c                   eval      G02err = '3'
687300040303     c                   call      'XSRDA8'
687400040303     c                   parm                    WLBdat
687500040303     c                   z-add     G02dat        v1cdar                         data ricez.comunicaz
687600040303      *
687700040304     c                   MOVEL     tgdorr        v1corr                         ora  ricez.comunicaz
687800040303
687900040303     c                   clear                   v1cchi
688000040303
688100040303     c                   eval      v1crsc = tgdrsc
688200041209     c                   eval      v1cchi = tgdksc
688300070403     c                   eval      W_v1cchi = tgdksc
688400050118      * decodifica
688500050118
688600050118     C                   clear                   TIBS69DS
688700050118     c                   z-add     v1cchi        I69kac
688800050118     C                   call      'TIBS69R'
688900050118     C                   parm                    tibs69DS
689000050118     C                   parm                    DS_cnaco
689100050118     C                   parm                    DS_cnind
689200050118     C                   parm                    DS_cnclp
689300050118     C                   parm                    DS_fncls
689400050118     C                   eval      wtibs69r = '1'
689500050118     c                   If        o69err = ' '
689600050118     c                   movel     ACOrag        v1dchi
689700050118     c                   endif
689800050118
689900040303     c                   eval      v1cclv = tgdclv
690000040303
690100040303      * recupero i riferimenti reclamo
690200040303
690300040303     c                   clear                   v1refe
690400040303     c                   clear                   v1ntel
690500040303     c                   clear                   v1mail
690600040303     c                   clear                   v1nfax
690700040303      *
690800040421     c     Ktgran2       setll     fitgr01l
690900040303     c                   do        *hival
691000040421     c     Ktgran2       reade     fitgr01l
691100040303
691200040303     c                   if        %eof(fitgr01l)
691300040303     c                   leave
691400040303     c                   endif
691500040303
691600040303      * referente
691700040303     c                   if        tgrtri = 'R'
691800040303     c                   eval      v1refe = tgrrif
691900040303     c                   endif
692000040303      * mail
692100040303     c                   if        tgrtri = 'M'
692200040303     c                   eval      v1mail = tgrrif
692300040303     c                   endif
692400040303      * numeri telefonoici
692500040303     c                   if        tgrtri = 'N'
692600040303     c                   movel     tgrrif        v1ntel
692700040303     c                   move      tgrrif        v1nfax
692800040303     c                   endif
692900040303
693000040303     c                   enddo
693100151216      *
693200040216     c                   ENDSR
693300150609
693400150609      *****************************************************************
693500150609      *  Ricerca alfabetica
693600150609      *****************************************************************
693700150609     c     SR_RICALF     begsr
693800150609      *
693900150609     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
694000150609     C                   Z-ADD     3             PAXkut                         tipo estrazione
694100150609     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
694200150609     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
694300150609     c                   movel(P)  V1dchi        PAXrag
694400150609     C                   z-add     1             PAXnum
694500150609      *
694600150609     C                   CALL      'XALFA3BR'
694700150609     C                   PARM                    PAXDUT           30
694800150609     C                   PARM                    PAXkut            1 0
694900150609     C                   PARM                    PAXrag           48
695000150609     C                   PARM                    PAXkcc            4 0
695100150609     C                   PARM                    PAXsta            1 0
695200150609     C                   PARM      *blanks       PAXflr           90
695300150609     C                   PARM      *blanks       PAXdit            3
695400150609     C                   PARM                    PAXnum            2 0
695500150609     C                   PARM                    PAXkcm           80
695600150609     C                   PARM                    PAXksm          140
695700150609     C                   PARM                    PAXkdm           60
695800150609      *
695900150609      * NON TROVATO O PREMUTO CMD12
696000150609    2C                   If        paxsta > -1
696100150609     C                   movel     PAXksm        V1Cchi
696200150609     C                   movel     PAXrag        V1dchi
696300150609     c                   endif
696400150609      *
696500150609     c                   endsr
696600150310
696700150310      /free
696800150310       //--------------------------------------------------------------
696900150318       //?F10 = Immissione ORM.
697000150310       //--------------------------------------------------------------
697100150310       BEGSR  SR_F10;
697200150310
697300150417         clear parm01;
697400150417         clear FIOR05DS;
697500150417         pfgs = DUTpou;
697600150417         ppoe = DUTpou;
697700150417         psce = '1';
697800150417         kpjbu = parm01;
697900150417         fior05r (kpjba:fior05ds);
698000150310
698100150310       ENDSR;
698200150318
698300150318       //--------------------------------------------------------------
698400150318       //?F11 = Visualizza Variazioni.
698500150318       //--------------------------------------------------------------
698600150318       BEGSR  SR_F11;
698700150318
698800150318         fidna3r (kpjba:fidna0ds);
698900150318
699000150318       ENDSR;
699100150310      /end-free
699200150310
699300070330      ****************************************************************
699400070330      * F17 = GESTIONE NOTE LDV
699500070330      ****************************************************************
699600070330     C     Sr_f17        BEGSR
699700070330      *
699800131206     c                   clear                   Fnlrn6DS
699900070423
700000070423      * se al momento del richiamo delle note LDV ho delle note RA propongo il
700100070423      * pgm delle note in immissione
700200070423      * se la linea di arrivo della bolla  è abilitata a GEO e l'ho trovata in fnarb
700300070423
700400131206     c                   eval      in6mod  = 'V'
700500140205      * note valorizzate   o   prevista obbligatorietà (in tab. "MAR")
700600070423     c                   If        (v1not1 <> *blanks or v1not2 <> *blanks  or
700700140205     c                              v1not3 <> *blanks or v1not4 <> *blanks  or
700800140205     c                              §MARnotLdV = 'S')    and
700900070423      * po abilitato a geo e bolla in arb  e non sono la sede
701000070423     c                             $abilitato = *on and $fnarb = *on
701100070423      * verifico se sono la filiale di arrivo della bolla
701200130611     c                   move      arblna        w003a
701300130610     c     w003a         lookup    wPOG                                   27
701400070423     c                   If        %found
701500131206     c                   eval      in6mod  = 'I'
701600131206     c                   movel     v1not1        in6not1
701700131206     c                   move      v1not2        in6not1
701800131206     c                   movel     v1not3        in6not2
701900131206     c                   move      v1not4        in6not2
702000070423     c                   endif
702100070423     c                   endif
702200131206     c                   eval      in6aas  = v1anns
702300131206     c                   eval      in6lnp  = v1lnps
702400131206     c                   eval      in6nrs  = v1nrss
702500131206     c                   eval      in6nsp  = v1nsps
702600131206     c                   eval      in6pgm  = 'FIDNA1R'
702700140205      *
702800140205      * se sono una spedizione in consegna passo i dati della distinta del file ARB
702900140205     c                   If        $cons  = *on
703000160104     c     KSPED         chain     Fnarb000
703100140205     c                   If        %found(Fnarb01l)
703200140205     c                   eval      in6fgs = arbifp
703300140205     c                   eval      in6nfv = arbndc
703400140205     c                   eval      in6dfv = arbddc
703500140205     c                   eval      in6pdr = arbpdc
703600140205     c                   eval      in6cons = 'S'
703700140205     c                   Endif
703800140205     c                   Endif
703900070330
704000131206     c                   call      'FNLRN6R'
704100070330     c                   parm                    kpjba
704200131206     c                   parm                    FNLRN6DS
704300070330
704400070330     c                   endsr
704500040212      ****************************************************************
704600040212      * F18 = GESTIONE NOTE
704700040212      ****************************************************************
704800040212     C     Sr_f18        BEGSR
704900040212      *
705000040212     C                   clear                   FIDNB0DS
705100040218     c                   z-add     v1cpot        iB0pot
705200040421     c                   z-add     v1cant        iB0ant
705300040212     c                   z-add     v1cnut        iB0nut
705400040212     c                   z-add     v1cpdt        iB0pdt
705500040218
705600040219     c                   z-add     Wdar          ib0dar
705700040421     c                   movel(p)  v1corr        ib0orr
705800040218
705900040218     c                   move      v1cmad        ib0mad
706000040218     c                   movel     v1dmad        ib0dma
706100040218
706200040212     c                   movel     'GEN'         iB0tIN
706300040218
706400040218      * - OGGETTO
706500040218
706600040218      * ---------- SPEDIZIONE -----------
706700040218
706800040218     c                   if        v1ctor = 'S'
706900040218
707000040218     c                   movel     spevid        WRKspe
707100040218
707200040219     c                   eval      ib0ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
707300040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
707400040219     c                             ' del ' + WRKann
707500040218      *
707600040218     c                   endif
707700040218
707800040218      * ----------    ORM     -----------
707900040218
708000040218     c                   if        v1ctor = 'O'
708100040218
708200040218     c                   movel     ormvid        WRKorm
708300040218
708400040218     c                   eval      ib0ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
708500040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
708600040218     c                             '/' + %trim(WRKnrv)
708700040218      *
708800040218     c                   endif
708900040218
709000040218      * ----------  GENERICO  -----------
709100040218
709200040218     c                   if        v1ctor = 'G'
709300040218     c                   movel     v1cogg        ib0ogg
709400040218     c                   endif
709500040218
709600040212     c                   movel     v1not1        db0no1
709700040212     c                   movel     v1not2        db0no2
709800040212     c                   movel     v1not3        db0no3
709900040212     c                   movel     v1not4        db0no4
710000040219      * se sono in modalità immissione e richiamo una riga da subfile richiamo il pgm note con
710100040219      * FLM = ' ' altrimenti con "M"
710200040305     c                   if        Wmod = 'I'
710300040305     c                   if        wrk_recsbf = 'Y'
710400040219     c                   movel     ' '           iB0flm
710500040219     c                   else
710600040212     c                   movel     'M'           iB0flm
710700040219     c                   endif
710800040305      * modalità manutenzione
710900040305     c                   else
711000040305     c                   movel     'V'           iB0flm
711100040305     c                   endif
711200070413      * se ho già scritto delle note anche se sono in immissione perchè provengo
711300070413      * da gestione dei blocchi consegne vado nel pgm gestione note ijn manutenzione
711400160601     c****               If        Wmod = 'I' and $bloccof3 = *on
711500160601     c                   If        Wmod = 'I' and $Note_scritte= *oN
711600070413     c                   movel     'V'           iB0flm
711700070413     c                   endif
711800040617      * aggiunto valorizzazione DB0PON
711900040617      * se sono utente particolare che ha un P.O. impostato da tabella passo il P.O. in db0pon
712000040617     c                   eval      db0pon = W_pot
712100040212      *
712200040212     C                   CALL      'FIDNB0R'
712300040212     C                   PARM                    kpjba
712400040217     C                   PARM                    FIDNB0DS
712500040212      *
712600040212      * valorizzo le prime 4 note a video
712700040212      *
712800040212     c                   movel     DB0NO1        v1not1
712900040212     c                   movel     DB0NO2        v1not2
713000040212     c                   movel     DB0NO3        v1not3
713100040212     c                   movel     DB0NO4        v1not4
713200040213      *
713300040213      * se ob0piu è valorizzato ci sono + di 4 righe
713400040213     c                   eval      *in29 = (OB0PIU = '+')
713500040218      *
713600040218      * VALORIZZO FLAG RICHIAMO FIDNB0R
713700040218      *
713800040218     C                   eval      wfidnb0r = 'Y'
713900040212      *
714000040212     c                   ENDSR
714100040216      *
714200040216      *---------------------------------------------------------------*
714300040216      * Gestione WINDOW                                               *
714400040216      *---------------------------------------------------------------*
714500040216     c     SR_GESWIN     BegSR
714600040216
714700040216     c                   do        *hival
714800040216      *
714900040216     c                   exfmt     fia1w01
715000040216      * KF = conferma fine lavoro perdita dei dati
715100040216     c                   if        *inkf
715200040220      * disalloco il file dettagli
715300040220     c                   unlock    fitgd01l
715400040220      *
715500150928     c                   clear                   fidnCAds
715600040421     c                   z-add     v1cant        ic0ant
715700040216     c                   z-add     v1cnut        ic0nut
715800040216     c                   z-add     999           ic0pdt
715900040216      *
716000150928     c                   call      'FIDNCAR'
716100150928     c                   parm                    fidnCAds
716200040216      *
716300040216     c                   leave
716400040216     c                   endif
716500040216      * KL = ritorna alla videata precedente senza deletare nulla
716600040216     c                   if        *inkl
716700040216     c                   leave
716800040216     c                   endif
716900040216
717000040216     c                   enddo
717100040216
717200040216     c                   endsr
717300040223      *---------------------------------------------------------------*
717400040223      * Gestione WINDOW     2                                         *
717500040223      *---------------------------------------------------------------*
717600040223     c     SR_GESWIN2    BegSR
717700040223
717800040223      *
717900040223     c                   exfmt     fia1w02
718000040223      * ENTER = conferma interrogazione bolle arrivi / partenze / ORM
718100040223      * KL = ritorna alla videata
718200040223
718300040223     c                   endsr
718400040406      *---------------------------------------------------------------*
718500040406      * Routine scrittura note aggiuntive automatiche                 *
718600040406      *---------------------------------------------------------------*
718700040406     c     WRT_NOTE      BegSR
718800040406
718900040406     C                   clear                   FIDNB0DS
719000040407      * chiusura
719100141210      *?Verifica telefonica "PINcode"?
719200141210if  1c                   If        Not_PINcode = 'S'
719300141211if  2c                   if        oPINfxx = *blank  and  oPINerr = *blank
719400141210     c                   eval      dB0no1 = 'ESITO: POSITIVO'
719500141210x   2c                   else
719600141210     c                   eval      dB0no1 = 'ESITO: NEGATIVO'
719700141210e   2c                   endif
719800141215     c                   clear                   WLBdat
719900141210     c                   eval      G02inv = ARBddc
720000141215     c                   eval      G02err = '3'
720100141210     c                   call      'XSRDA8'
720200141210     c                   parm                    WLBdat
720300141216     c                   eval      dB0no2 = 'DIST: ' +
720400141210     c                                      %subst( %editc( ARBifp : 'X' ) :
720500141210     c                                              %len( ARBifp ) - 2 : 3 ) +
720600141216     c                                      ' ' +
720700141210     c                                      %trim( %editc( ARBndc : '3' ) ) +
720800141210     c                                      ' DEL ' +
720900141211     c                                      %trim( %editc( G02dat : 'Y' ) )
721000141211     c                   eval      dB0no3 = 'AUT: ' +
721100141210     c                                      %trim( %editc( ARBpdc : 'X' ) )
721200141210e   1c                   EndIf
721300040407
721400040407
721500041214     c                   z-add     tgdant        iB0ant
721600041214     c                   z-add     tgdnut        iB0nut
721700041214     c                   z-add     tgdpdt        iB0pdt
721800040406     c                   movel     'GEN'         iB0tIN
721900141216     c                   z-add     999           iB0pgn
722000040406     c                   movel     'W'           iB0flm
722100050111     c                   eval      db0sts ='N'
722200041214     c                   eval      ib0pot =tgdpot
722300041013     c                   eval      db0pon =Dutpou
722400040406     c                   eval      db0utn =knmus
722500040406
722600040406     C                   CALL      'FIDNB0R'
722700040406     C                   PARM                    kpjba
722800040406     C                   PARM                    FIDNB0DS
722900040407
723000141210     c                   clear                   Not_PINcode
723100040406
723200040406     c                   endSR
723300150316
723400150316      /free
723500150316       //---------------------------------------------------------------*
723600150316       // Routine scrittura file modifiche                              *
723700150316       //---------------------------------------------------------------*
723800150316       BEGSR WRT_Modifiche;
723900150316
724000150316         clear FITGM000;
724100160105         if  wTMO = 'A';
724200160105           TGMpot = V1Cpot;
724300160105           TGMant = V1Cant;
724400160105           TGMnut = V1Cnut;
724500160105           TGMpdt = V1Cpdt;
724600160105         else;
724700160105           TGMpot = TGDpot;
724800160105           TGMant = TGDant;
724900160105           TGMnut = TGDnut;
725000151214           TGMpdt = TGDpdt;
725100151214         endif;
725200150316         TGMtmo = wtmo;
725300150316         SELECT;
725400151214         WHEN  TGMtmo = 'A';
725500151214           TGMmod = dTGMa;
725600150318         WHEN  TGMtmo = 'C';
725700150318           TGMmod = dTGMc;
725800150318         WHEN  TGMtmo = 'K';
725900150318           TGMmod = dTGMk;
726000150318         WHEN  TGMtmo = 'R';
726100150318           TGMmod = dTGMr;
726200150326         WHEN  TGMtmo = 'S';
726300150326           TGMmod = dTGMs;
726400150316         WHEN  TGMtmo = 'U';
726500150316           TGMmod = dTGMu;
726600150316         ENDSL;
726700150316         TGMdmo = %dec(%date());
726800150316         TGMomo = %dec(%time());
726900150316         TGMutn = knmus;
727000150316         TGMpon = DUTpou;
727100150318         TGMnoj = knmeb;
727200150316         write FITGM000;
727300150319         feod FITGM01L;
727400150316
727500150316       ENDSR;
727600150316      /end-free
727700150316
727800150316      *
727900040126      *---------------------------------------------------------------*
728000040126      * Routine Iniziale                                              *
728100040126      *---------------------------------------------------------------*
728200040126     c     *InzSR        BegSR
728300040126      *
728400040129     c     *Entry        plist
728500040129     c                   parm                    KPJBA
728600040213     c                   parm                    FIDNA0DS
728700040126      * Reperisco data corrente
728800040126     c                   time                    w0140
728900040126     c                   move      w0140         wData
729000040126     c                   movel     w0140         wOra
729100040126     c                   clear                   WLBdat
729200040126     c                   eval      G02dat = wData
729300040126     c                   call      'XSRDA8'
729400040126     c                   parm                    WLBdat
729500040126     c                   z-add     G02inv        DateU
729600040324     C     *iso          movel     dateu         Wdataoggi
729700040126      *
729800040126      * Reperisco dati job
729900040126     c                   exsr      DatiJob
730000040126     c                   movel     SDSpgm        VTCpgm
730100130806
730200130806      * Imposto le abilitazioni dell'utente
730300130806     c                   eval      dute01 = UTEfaf
730400041005
730500041005      * Se sono in sede accendo 05
730600041005     c                   eval      *in05=(simfel = 0)
730700041005
730800040213      * Se ricevuto parametro per manutenzione  non valorizzo il p.o. emittente con quello
730900040213      * in linea
731000040213     c                   if        parms   > 1
731100040213      * modalità GESTIONE
731200040213     c                   eval      wmod   = 'G'
731300040217     c                   eval      v1tmod = tit2
731400040303     c                   eval      *in50 = *on
731500040420      * reperisco i programmi che hanno richiamato questo pgm
731600040420     c                   Call      'XRTVSTKR'
731700040420     C                   Parm                    PStack         1000
731800040420     C                   Parm                    PStartPoint       3 0
731900040420     C                   Parm                    PEntries          3 0
732000040420     c                   Movel     Pstack        DSstack
732100040213     c                   else
732200040213     c                   eval      v1cpot = dutpou
732300040616      *
732400040213      * modalità IMMISSIONE
732500040213     c                   eval      wmod   = 'I'
732600040217     c                   eval      v1tmod = tit1
732700040213     c                   endif
732800040617      * verifico se sto inserendo o modificando un reclamo da parte di un utente
732900040617      * che deve avere come p.o. apertura e gestione quello della tabella UGR
733000040617      * tabella UGR
733100040617     c                   movel     KNMUS         w_ute
733200040617     c                   clear                   tibs02ds
733300040617     C                   MOVEL     'C'           t02mod
733400040617     C                   MOVEL     knsif         t02sif
733500040617     C                   MOVEL     'UGR'         t02cod
733600040617     C                   MOVEL     W_ute         t02ke1
733700040617      *
733800040617     C                   CALL      'TIBS02R'
733900040617     C                   PARM                    KPJBA
734000040617     C                   PARM                    TIBS02DS
734100040617      *
734200040617     C                   IF        t02err = *BLANKS
734300040617      *
734400040617     c                   movel     t02uni        DUGR
734500040617      * se P.O. da forzare diverso da zero imposto in automatico
734600040617     c                   if        §UGRpofo > 0
734700040617     c                   eval      v1cpot = §UGRpofo
734800040617     c                   eval      W_pot  = §UGRpofo
734900040617     c                   endif
735000040617      *
735100040617     c                   endif
735200040324      * reperisco i dati standard RECLAMI
735300040324      * Controllo
735400040324     c                   clear                   tibs02ds
735500040324     C                   MOVEL     'C'           t02mod
735600040324     C                   MOVEL     knsif         t02sif
735700040324     C                   MOVEL     'REC'         t02cod
735800040324     C                   MOVEL     '1'           t02ke1
735900040324      *
736000040324     C                   CALL      'TIBS02R'
736100040324     C                   PARM                    KPJBA
736200040324     C                   PARM                    TIBS02DS
736300040324      *
736400040324     C                   IF        t02err = *BLANKS
736500040324     C                   movel     T02uni        DREC
736600040324     c                   endif
736700041012      * verifico se sono un utente che può modficare tutto senza controlli
736800130806     c                   if        §UTEvrac = 'S'
736900041012     c                   eval      *in07 = *on
737000041012     c                   endif
737100041005      * se sono in sede apro il file TITAS
737200041005     c                   If        *in05
737300041005     c                   open      titas30c
737400041005     c                   else
737500041005     c                   open      fnblp01l
737600041005     c                   open      fnarb01l
737700140429     c                   open      FIAR401L
737800140429     c                   open      FIAR501L
737900041005     c                   endif
738000151130      *
738100151130      /free
738200170113
738300170113         // -?Esecuzione override al *file AZNUM01L?
738400170113         //  ?(usato dal *pgm chiamato TRUL33R)?
738500170113         //  ?per limitare il "Tempo massimo attesa record"?
738600170113         exsr  sr_OvrAZNUM;
738700151130
738800151130         // -?Caricamento filiali x Estero?
738900151130         clear  TRUL31ds;
739000151130         I31abi = 'EE';
739100151130         trul31r ( KPJBA : TRUL31ds );
739200151130         wFILest_ds = O31pog;
739300151130
739400151130      /end-free
739500151130      *
739600070323      * se non sono in sede verifico se il PO è abilitato a Geo telefonate e mi carico la £6
739700070323     c                   if        not *in05
739800070323     c                   exsr      sr_carl6
739900070323     c                   endif
740000040126      *
740100040126     c                   EndSr
740200040126
740300070323      *---------------------------------------------------------------*
740400070323      * Recupero i P.O.  della £6                                     *
740500070323      *---------------------------------------------------------------*
740600070323     c     Sr_CARL6      begsr
740700070323
740800070323      * Recupero i PO della £6
740900070323      *
741000070323     c                   clear                   trul06ds
741100070323     c                   move      '£6'          d06cod
741200070323     c                   movel     Dutpou        d06key
741300070323     c                   movel(p)  trul06ds      KPJBU
741400070323
741500070323     c                   call      'TRUL06R'
741600070323     c                   parm                    kpjba
741700070323     c                   movel     kpjbu         trul06ds
741800070323     c                   movea     lin           L6
741900070323
742000070323      * verico se PO abilitato alla gestione telefonate GEO
742100070323
742200070323     c                   clear                   og148
742300160104     c     l6(1)         chain     azorg
742400070323     c                   if        %found(azorg01l)
742500070323     c                   movel     orgde8        og148
742600070323     c                   endif
742700070323
742800070323     c                   If        §oggeot = 'S'
742900070323     c                   eval      $abilitato = *on
743000070608     c                   eval      *in08 = *on
743100070323     c                   endif
743200130610
743300130611      /free
743400130611       //?Imposto abilitazione utente
743500130611         clear wpog;
743600130611         SELECT;
743700130611       //?Imposto abilitazione manutenzione bolle di filiale
743800130611           WHEN  §UTEmbf <> *blanks;
743900130611             wabi = §UTEmbf;
744000130611           WHEN  UTEaut <> *blanks;
744100130611             wabi = UTEaut;
744200130611           WHEN  DUTlpo = '1';
744300130611             wabi = 'TP';
744400130611           WHEN  DUTlpo = '2';
744500130611             wabi = 'PO';
744600130611         ENDSL;
744700140123
744800130611      /end-free
744900130611
745000130610      * controllo se ok l'abilitazione dell'utente
745100130611     c                   clear                   TIBS02DS
745200130611     c                   clear                   dLAT
745300130611     c                   eval      T02mod = 'C'
745400130611     c                   eval      T02sif = knsif
745500130611     c                   eval      T02cod = 'LAT'
745600130611     c                   movel(p)  wabi          T02ke1
745700130611     c                   call      'TIBS02R'
745800130611     c                   parm                    kpjba
745900130611     c                   parm                    TIBS02DS
746000130611if  1c                   IF        T02err = *blanks
746100130611     c                   eval      dLAT = T02uni
746200130611e   1c                   ENDIF
746300130611
746400130611      * Se abilitazione trovata e OK
746500130611if  1c                   IF        T02err = *blanks and §LATabi = *blanks
746600130610      * Reperimento dei P.O. gestibili dall'utente
746700130610     c                   clear                   TRUL31DS
746800130610     c                   eval      I31abi = wabi
746900130610     c                   eval      I31cdi = DUTdis
747000130610     c                   eval      I31car = DUTare
747100130610     c                   eval      I31cpo = DUTpou
747200130610     c                   call      'TRUL31R'
747300130611     c                   parm                    kpjba
747400130610     c                   parm                    TRUL31DS
747500130611if  2c                   IF        O31pog > *zeros
747600130610     c                   movea     O31pog        wpog
747700130611x   2c                   ENDIF
747800130611    1c                   ENDIF
747900130611
748000130611      * se sk delle filiali abilitate all'utente vuota impsoto il primo elemento della £6
748100130611     c                   IF        wpog(1) = *blanks
748200130611     c                   eval      wpog(1) = %editc(l6(1):'X')
748300130611     c                   ENDIF
748400150317
748500150317      /free
748600150317       //?Imposto abilitazione utente x manutenzione RA
748700150317         clear wFIL_ds;
748800150317         SELECT;
748900150317         WHEN  §UTErac <> *blanks;
749000150317           wabi = §UTErac;
749100150317         WHEN  UTEaut <> *blanks;
749200150317           wabi = UTEaut;
749300150317         WHEN  DUTlpo = '1';
749400150317           wabi = 'TP';
749500150317         WHEN  DUTlpo = '2';
749600150317           wabi = 'PO';
749700150317         ENDSL;
749800150317       //?Reperimento delle filiali gestibili dall'utente per manutenzione RA
749900150317         clear TRUL31DS;
750000150317         I31abi = wabi;
750100150317         I31cdi = DUTdis;
750200150317         I31car = DUTare;
750300150317         I31cpo = DUTpou;
750400150317         trul31r (kpjba : TRUL31DS);
750500150317         wFIL_ds = O31pog;
750600150317         IF  wFIL_ds = *blanks;
750700150317           sk_wFIL(01) = %editc(DUTpou:'X');
750800150317         ENDIF;
750900150317      /end-free
751000070323
751100070323     c                   endsr
751200070323
751300150601      *---------------------------------------------------------------*
751400150601      * Conto quante RA ci sono a fronte dello stesso oggetto         *
751500150601      *---------------------------------------------------------------*
751600150601     c     Sr_countRA    BegSr
751700150601      *
751800150601      *
751900150603     c                   if        V1ctor = 'O'
752000150603     c                   movel(p)  ormvid        W_V1cogg
752100150603     c                   endif
752200150603     c                   if        V1ctor = 'S'
752300150603     c                   movel(p)  spevid        W_V1cogg
752400150603     c                   endif
752500150603      *
752600160104     c     Ktgd32        setll     fitgd32
752700160104     c     Ktgd32        reade     fitgd32
752800150603     c                   if        %eof(Fitgd32l)
752900150603     c                   eval      IA1INS = 'N'
753000150603     c                   else
753100160104     c     Ktgd32        reade     fitgd32
753200150603      * se c'è una sola RA non abilito F16 nei programmi di interrogazione bolle o ORM
753300150603      * se più di una lo abilito
753400150603     c                   if        not %eof(Fitgd32l)
753500150603     c                   eval      IA1INS = 'I'
753600150603     c                   else
753700150603     c                   eval      IA1INS = 'N'
753800150603     c                   Endif
753900150603
754000150603     c                   Endif
754100150603
754200150601     c                   endsr
754300150601
754400040126      *---------------------------------------------------------------*
754500040126      * Reperimento Dati del job (Utente/Operativi)                   *
754600040126      *---------------------------------------------------------------*
754700040126     c     DatiJob       BegSr
754800040126      *
754900040126     c     *dtaara       define    §azute        azuteds
755000040126     c     *dtaara       define    §datiute      ddatiute
755100040126      *
755200040126     c                   in(E)     *dtaara
755300040126     c                   IF        %ERROR or RSUT = *blanks
755400040126     c                   clear                   Tibs34Ds
755500040126     c                   call      'TIBS34R'
755600040126     c                   parm                    Tibs34Ds
755700040126     c                   in        *dtaara
755800040126     c                   ENDIF
755900040126      *
756000040126     c                   EndSr
756100170113      *
756200170113      /free
756300170113
756400170113       //--------------------------------------------------------------
756500170113       //?Esecuzione OVERRIDE al *file del numeratore R.A. per limitare?
756600170113       //?il "Tempo massimo attesa record" ad un secondo.              ?
756700170113       //--------------------------------------------------------------
756800170113       BEGSR  sr_OvrAZNUM;
756900170113
757000170113         Qcmd = 'ovrdbf file(AZNUM01L) WAITRCD(1) ovrscope(*callLvl)';
757100170113
757200170113         exsr  sr_ExecCmd;
757300170113
757400170113       ENDSR;
757500170113
757600170113       //--------------------------------------------------------------
757700170113       //?Cancellazione OVERRIDE dal *file del numeratore R.A. per     ?
757800170113       //?limitare il "Tempo massimo attesa record" ad un secondo.     ?
757900170113       //--------------------------------------------------------------
758000170113       BEGSR  sr_DltOvrAZNUM;
758100170113
758200170113         Qcmd = 'dltovr file(AZNUM01L) lvl(*)';
758300170113
758400170113         exsr  sr_ExecCmd;
758500170113
758600170113       ENDSR;
758700170113
758800170113       //--------------------------------------------------------------
758900170113       //?Esecuzione del comando (già impostato).                      ?
759000170113       //--------------------------------------------------------------
759100170113       BEGSR  sr_ExecCmd;
759200170113
759300170113         clear Qcap0100;
759400170113         Qcabcsdh = *off;
759500170113         Qcapa    = *off;
759600170113         Qcacmdss = *off;
759700170113         Qcaerved = *allX'00';
759800170113
759900170113         clear Qusec;
760000170113         Qusbprv  = %size(Qusec);
760100170113
760200170113         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
760300170113                           %size(Qcap0100) : 'CPOP0100' : *omit :
760400170113                           0 : 0 : Qusec);
760500170113
760600170113         //if  Qusei <> *blank;
760700170113         //  ...;
760800170113         //endif;
760900170113
761000170113       ENDSR;
761100170113
761200170113      /end-free
761300040126
761400130513** -?Msg:?Messaggi di Errore?------------------------------------------------*
761500040205Codice cliente errato                                                                    1
761600040610Inserire l'oggetto della richiesta                                                       2
761700040211Inserire correttamente numero Spedizione                                                 3
761800040211Spedizione inesistente                                                                   4
761900040211Inserire correttamente numero ORM                                                        5
762000040211Numero ORM inesistente                                                                   6
762100040211Inserire correttamente il "CHI CHIAMA"                                                   7
762200040610Inserire motivo apertura richiesta                                                       8
762300150318Filiale responsabile errata                                                              9
762400040213Inserire Tipo Comunicazione                                                             10
762500040217Data errata                                                                             11
762600040217Ora errata                                                                              12
762700040217Inserire il tipo oggetto                                                                13
762800040610Prima di dare F4 inserire almeno una richiesta                                          14
762900040219Inserire la Ragione Sociale del chiamante                                               15
763000040220Per questo oggetto 'Chi Chiama' non ammesso                                             16
763100040223Tipo comunicazione errata                                                               17
763200040610Motivo apertura richiesta errato                                                        18
763300040610Codice chiusura richiesta errato                                                        19
763400150429Reparto/Utente in gestione errato                                                       20*#
763500040610Data richiesta non deve essere maggiore della data odierna                              21
763600040315Non si può interrogare un oggetto generico                                              22
763700150318Filiale di appartenenza dell'utente diversa dalla filiale gestione                      23
763800160225Richiesta NON gestibile dall'utente                                                     24 +
763900160225Richiesta NON gestibile dall'utente perchè R.A. impegnata da altro utente               25 +
764000160225Se responsabilità Vettore è obbligatorio inserire la Fil. responsabile                  26
764100040610Utente non abilitato alla gestione richieste                                            27
764200150429Inserire utente in gestione esistente nei profili della Filiale                         28*Libero
764300150429Inserire reparto/utente in gestione                                                     29 #
764400040610Oggetto presente in altre richieste Enter per forzare                                   30
764500040707Oggetto non presente in archivi                                                         31
764600041210Codice Chiamante mancante F4 per forzare la chiusura                                    32
764700070608Variazione non effettuata                                                               33
764800070608Causale variazione errata o non ammessa                                                 34
764900071019Variazione non consentita                                                               35
765000071018Variazione non consentita bolla non in gestione alla Filiale                            36
765100120914Questa Richiesta Assistenza va chiusa con Codice Chiusura                               37
765200120919Immettere le note                                                                       38
765300131119Per il motivo inserito l'oggetto dev'essere                                             39
765400140213Non utilizzabile. Proposto in automatico confermando la R.A.                            40
765500140430Variazione possibile solo su bolle in PARTENZA dalla propria filiale                    41 <
765600140430Motivo Apertura " 17" utilizzabile solo su bolle in ARRIVO                              42 <
765700140923Tipo comunicazione non utilizzabile in gestione R.A.                                    43
765800141029Ora richiesta consegna errata                                                           44
765900141210Motivo Apertura utilizzabile solo per spedizioni in consegna oggi                       45 «
766000141210Motivo Apertura utilizzabile solo per spedizioni che prevedono un "PINcode"             46 «
766100141218Manca il "PINcode" per questa spedizione                                                47 «
766200150429Filiale gestione errata                                                                 48
766300150422Inserire il telefono e/o la mail                                                        49 #
766400150422Reparto gestione errato                                                                 50 #
766500150717Motivo Apertura impostabile solo da utenti di sede                                      51
766600150723Chiusura non consentita da SEDE                                                         52
766700150723Richiesta Assistenza non gestibile da SEDE                                              53
766800150911C.A.P. errato                                                                           54
766900151130Motivo Apertura " 18" NON utilizzabile per bolle EXPORT                                 55
767000151130Esiste già almeno una disposizione di Dirottamento per questa spedizione                56
767100160120Reparto che Attende Risposta errato                                                     57 ¥
767200160120Filiale che Attende Risposta errata                                                     58 ¥
767300160120Reparto/Filiale in Attesa Risposta NON può coincidere con quello di Gestione            59
767400160302ATTENZIONE: numeratori per le R.A. bloccati da altri lavori.  AVVISARE IL CED           60
767500160315Presente Giacenza aperta: Motivo apertura " 18" NON utilizzabile                        61
767600160314Bolla legata: il reso può essere richiesto solo sulla figlia                            62
