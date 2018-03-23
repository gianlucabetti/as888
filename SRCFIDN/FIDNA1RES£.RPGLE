000100040107      *---------------------------------------------------------------*
000200040610      * GESTIONE RICHIESTE ASSISTENZA CLIENTI                         *
000300040107      *---------------------------------------------------------------*
000400070430      *---------------------------------------------------------------*
000500070430      *                                                               *
000600070430      *   ATTENZIONE PER COMPILARE BISOGNA AVERE ANCHE LA GAITRAGRPS  *
000700070430      *   NELLA LISTA DELLE LIBRERIE                                  *
000800130513      *   COMPILARE CON 14 ED IMPOSTARE I SEGUENTI PARAMETRI          *
000900070430      *   DFTACTGRP(*NO) E ACTGRP(*NEW)                               *
001000070430      *                                                               *
001100070430      *---------------------------------------------------------------*
001200151201
001300151201       //--------------------------------------------------------------
001400151201       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001500151201       //--------------------------------------------------------------
001600151201
001700151201     /*PRM  dbgview(*source)
001800151201     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001900151201     /*CMD         ovrscope(*calllvl)
002000151201     /*END  dltovr file(TITAS30C) lvl(*)
002100151201     /*END
002200151201
002300151201       //--------------------------------------------------------------
002400151201       //?Specifiche di controllo.                                     ?
002500151201       //--------------------------------------------------------------
002600040107
002700040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002800131119     H DFTACTGRP(*NO) ACTGRP(*NEW)
002900141210     h bnddir('TRUL')
003000040107
003100151201       //--------------------------------------------------------------
003200151201       //?Dichiarazione file.                                          ?
003300151201       //--------------------------------------------------------------
003400040107
003500040126     fFITGD01L  uf a e           k disk
003600040126     fFITGR01L  uf a e           k disk
003700150325     fFITGD32L  if   e           k disk    rename(fitgd000:fitgd32)
003800150603     f                                     prefix(K_)
003900070402     fFITGN01L  if   e           k disk
004000150319     fFITGM01L  if a e           k disk
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
005500150610     fFIDNA1D   cf   e             workstn sfile(FIA1S01:S01nrr)
005600040107
005700040107      *---------------------------------------------------------------*
005800070327      *  MOTIVI DI APERTURA FISSATI A PROGRAMMA
005900070327      *---------------------------------------------------------------*
006000070327      *   2 = LASCIATO AVVISO                               W_LAV
006100070327      *  10 = CONTRASSEGNO                                  W_CON
006200070327      *  14 = SUPERMERCATO                                  W_SUP
006300070327      *  15 = APPUNTAMENTO                                  W_APP
006400070327      *  16 = FERMO DEPOSITO                                W_FDE
006500140429      *  17 = FERMO DEPOSITO AUTORIZZA MITTENTE             C_FDEAM
006600151130      *  18 = RESO                                          C_MOTAP_RESO
006700141210      *  60 = VERIFICA PIN-CODE TELEFONICA                  C_VPINC
006800150720      *  70 = RECLAMO DA SEDE                               C_RECLSEDE
006900070327      *---------------------------------------------------------------*
007000040107
007100040107      *
007200040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
007300040107      *
007400040205
007500151130     d MSG             S             78    DIM(056) CTDATA PERRCD(1)            MSG VIDEO
007600040531     D CCR             S              1    DIM(20)                              Chi chiama
007700070323      *
007800070323     d L6              s              3  0 dim(30)                              po £6
007900130610     D wpog            s              3    dim(250)
008000070327
008100070327     D NOB             S             35    DIM(12)                              Note
008200070327     D NORA            S             35    DIM(12)                              Note da scrivere
008300070327     D NOV             S             35    DIM(04)                              Note a video
008400070327
008500070323      *
008600040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
008700040107      *
008800040107     d KPJBA         e ds
008900040213      * - Parametri per gestione reclami già esistenti
009000040213     d FIDNA0DS      e ds
009100040213      * - Parametri ritorno da interrigazione oggetto
009200040213     d FIDNA1DS      e ds
009300040203      * - Parametri note
009400040107     d FIDNB0DS      e ds
009500040217      * - Parametri cancellazione reclamo
009600150928     d FIDNCAds      e ds                  inz
009700070330      * - Parametri per richiamo pgm note per Aut / ldv
009800131206     d FNLRN6DS      e ds
009900070329      * - Parametri per richiamo pgm blocchi
010000070329     d FNLRG2DS      e ds
010100070430      * - Parametri per richiamo pgm recupero blocchi
010200070430     d FNLRG6DS      E DS
010300070430     d DLRG6ABI      E DS
010400070329      * - Parametri per richiamo pgm FNLR48R
010500070329     D fnlr48ds      E DS
010600070608      * - Parametri per richiamo pgm FNLR66R controllo causali variazioni bolle
010700070608     D fnlr66ds      E DS
010800150310      * - Parametri per richiamo pgm FIOR05R immissione ORM
010900150310     d FIOR05DS      e ds
011000070329      *
011100070329     D DSARBD        E DS
011200070329     D DSARBG        E DS
011300070329     D DARBT         E DS
011400070329     D DSARBK        E DS
011500070329     D trul90ds      E DS
011600140924     D DSARBN        E DS
011700040108      *
011800040108      * - Parametri x Controllo profilo utenti
011900040108     d TIBS34DS      e ds                  inz
012000040108      * - Ds di riferimento al file esterno AZUTE00F
012100040108     d AZUTEDS       e ds                  extname(AZUTE00F)
012200040108      * - Ds per dati organigramma
012300040108     d DDatiUte      e ds
012400070323      * abilitazioni
012500070323     D OG148         E DS
012600070323      * DS per TRUL06R - Caricamento £X
012700070323     d TRUL06DS      E DS
012800070323     d  LIN                    1     90  0
012900070323     d                                     DIM(30)
013000040202      * - Ds per Anagrafiche Clienti
013100040202     d TIBS69DS      E DS
013200040202     d DS_cnaco      E DS                  extname(CNACO00F)
013300040202     d DS_cnind      E DS                  extname(CNIND00F)
013400040202     d DS_cnclp      E DS                  extname(CNCLP00F)
013500040202     d DS_fncls      E DS                  extname(FNCLS00F)
013600040107      *
013700040129      * - Parametri x Numeratore Reclami
013800040212     d TRUL33ds      e ds
013900040109      * - Tabelle
014000040129     d TIBS02ds      e ds                  inz
014100131119     d tntbMARds1    e ds                  inz
014200130610
014300130610     D TRUL31DS      E DS
014400130610     d dLat          e ds
014500130610     d dute01        e ds
014600070327      *
014700040212     d DMAR          e ds
014800040212     d DCHR          e ds
014900040212     d DCMU          e ds
015000040217     d DCOR          e ds
015100130514     d DOGR          e ds
015200130514     d dOGR_S        e ds                  extname(dOGR)  qualified
015300130514     d                                     inz
015400040220     d DCCR          e ds
015500040323     d DPRR          e ds
015600040323     d DUGR          e ds
015700040323     d DREC          e ds
015800141210      *
015900141210     d ds7R          e ds                  inz  qualified
016000140429      *
016100140429      * -?Estensione TESTATA BOLLA: tipo record "GEN"?
016200140429     d dAR5gen       e ds                  inz
016300141210      *
016400141210      * -?Gestione Telefonate - Riferimenti Chiamante: Riferimento "PINcode"?
016500141210     d dTGR01        e ds                  inz  qualified
016600150318
016700150318      * -?DS Modifiche Causale Chiusura FITGM
016800150318     d dTGMc         E DS
016900150318
017000150318      * -?DS Modifiche Cod. Chiamante FITGM
017100150318     d dTGMK         E DS
017200150318
017300150318      * -?DS Modifiche FIl. Responsabile FITGM
017400150318     d dTGMR         E DS
017500150326
017600150326      * -?DS Modifiche Stato Responsabile FITGM
017700150326     d dTGMS         E DS
017800150316
017900150316      * -?DS Modifiche Utente FITGM
018000150316     d dTGMu         E DS
018100131119      *
018200131119      * - Parametri x controllo indirizzo e-mail
018300040217     d DSEMAIL       E DS
018400040109      *
018500040205      * - Parametri da passare al pgm interrogazione bolle ARRIVI
018600040205     D DS_RICBOLA      DS                  INZ
018700040205     D  DSRBA_aas             14     17  0
018800040205     D  DSRBA_lnp             18     20  0
018900040205     D  DSRBA_nrs             21     22  0
019000040205     D  DSRBA_nsp             23     29  0
019100040205     D  DSRBA_f03             30     30
019200040205     D  DSRBA_flg             31     31
019300040205      * - Parametri da passare al pgm interrogazione bolle PARTENZE
019400040205     D DS_RICBOLP      DS                  INZ
019500040205     D  DSRBP_aas              1      4  0
019600040205     D  DSRBP_lnp              5      7  0
019700040205     D  DSRBP_nrs              8      9  0
019800040205     D  DSRBP_nsp             10     16  0
019900040205     D  DSRBP_f03             17     17
020000040205     D  DSRBP_flg             31     31
020100040211      * - Parametri da passare al pgm di visualizzazione Bolle PARTENZE
020200040211     D DS_VISBOLP      DS                  INZ
020300040211     D  DSVBP_aas              1      4  0
020400040211     D  DSVBP_lnp              5      7  0
020500040211     D  DSVBP_nrs              8      9  0
020600040211     D  DSVBP_nsp             10     16  0
020700040211     D  DSVBP_f03             17     17
020800040211     D  DSVBP_rsut            23     42
020900040211     D  DSVBP_sim             44     46  0
021000040211      * - Parametri da passare al pgm di visualizzazione ORM
021100040211     D Parm01          DS
021200040211     D  ppoe                          3  0
021300040211     D  pnor                          7  0
021400040211     D  pnsr                          2  0
021500040211     D  pnrv                          2  0
021600040211     D  psce                          1
021700040211     D  pfgs                          3  0
021800040211     D  ppor                          3  0
021900040211     D  pdtr                          8  0
022000040211     D  pmdb                         10
022100040211     D  pprb                         10
022200040211     D  pdts                          8  0
022300040211     D  prmp                          1
022400040211     D  pbrc                          1
022500040211     D  pref                          2
022600040211     D  pmio                          1
022700041005      * - Parametri da passare al pgm di visualizzazione Bolle di sede
022800041005     D TNSB50DS      E DS
022900040211      *
023000040205      * - Esplosione oggetto spedizione
023100040205     D DS_spediz       DS                  INZ
023200040219     D  DSspe_lnp              1      3  0
023300040219     D  DSspe_nrs              4      5  0
023400040219     D  DSspe_nsp              6     12  0
023500040219     D  DSspe_aas             13     16  0
023600040205      * - Esplosione oggetto ORM
023700040205     D DS_orm          DS                  INZ
023800040205     D  DSorm_poe              1      3  0
023900040205     D  DSorm_nsr              4      5  0
024000040205     D  DSorm_nor              6     12  0
024100141212     D  DSorm_nrv             13     14  0
024200040205      *
024300040205      * - Parametri da passare al pgm interrogazione ORM
024400150701     D FIOR010DS     E DS
024500070327      *
024600040109     d Status         sds
024700040109     d  SDSpgm           *proc
024800040213     d  PARMS            *parms
024900040107      *
025000040120     d dspINFOds       ds           512
025100040120     d  di_TASTO             369    369
025200040120     d  di_NRG               370    370
025300040120     d  di_SFLnrr            378    379B 0
025400040109      *
025500040109     d WLBdat          ds                  inz
025600040109     d  G02dat                 1      8  0 inz
025700040109     d  G02inv                 9     16  0 inz
025800040109     d  G02err                17     17    inz('3')
025900040109     d  G02tgi                18     22  0 inz
026000040211      * - DS di comodo    ORM
026100040211     D                 DS
026200040211     D  W_poe                         3  0
026300040211     D  W_nsr                         2  0
026400040211     D  W_nor                         7  0
026500040211     D  W_nrv                         2  0
026600040303     D  Wormvid                1     14  0 inz
026700040211      *
026800040211     D                 DS
026900040211     D  v1poeo                        3  0
027000040211     D  v1nsor                        2  0
027100040211     D  v1norm                        7  0
027200040211     D  v1nrvo                        2  0
027300040303     D  ormvid                 1     14  0 inz
027400040216      *
027500040216     D                 DS
027600040216     D  WRKpoe                        3
027700040216     D  WRKnso                        2
027800040216     D  WRKnor                        7
027900040216     D  WRKnrv                        2
028000040216     D  WRKORM                 1     14
028100040211      * - DS di comodo    spedizione
028200040211     D                 DS
028300040211     D  W_lnp                         3  0
028400040211     D  W_nrs                         2  0
028500040211     D  W_nsp                         7  0
028600040211     D  W_aas                         4  0
028700040303     D  Wspevid                1     16  0 inz
028800040211      *
028900040211     D                 DS
029000040211     D  v1lnps                        3  0
029100040211     D  v1nrss                        2  0
029200040211     D  v1nsps                        7  0
029300040217     D  v1anns                        4  0
029400040303     D  spevid                 1     16  0 inz
029500040216     D                 DS
029600040219     D  WRKlnp                        3
029700040219     D  WRKnrs                        2
029800040219     D  WRKnsp                        7
029900040219     D  WRKann                        4
030000040219     D  WRKSPE                 1     16
030100040216      * - DS per girare le date
030200040216     D                 DS
030300040216     D  anno4                  1      4  0
030400040216     D  anno2                  3      4  0
030500040216     D  mese                   5      6  0
030600040216     D  giorno                 7      8  0
030700040216     D  data8                  1      8  0
030800040216
030900040212      * - DS Parametri per ricerca Punto Operativo
031000040212     D Dstnsd          DS
031100040212     D  §cod                          3
031200040212     D  §tip                          1
031300040212     D  §des                         25
031400040212     D  §pra                          3
031500040420
031600040420      * - DS Parametri per ricerca Programmi chiamanti
031700040420     D DSStack         ds
031800040420     D  £Stack                       10    Dim(100)
031900040212
032000040426      * - DS Controllo giorni festivi
032100040426     d clnmat          ds
032200040426     d  mat                    1     31    dim(31)
032300040426     d clnpom          ds
032400040426     d  pom                    1     31    dim(31)
032500150317
032600150317     D wFIL_ds         ds
032700150317     d   sk_wFIL               1    750    dim(250)
032800151130
032900151130     d wFILest_ds      ds
033000151130     d   sk_FILest             1    750    dim(250)
033100150114
033200040107      *
033300040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
033400040107      *
033500040109      * - Campi di comodo
033600040108     d w0140           s             14  0
033700040108     d wData           s              8  0
033800040120     d wOra            s              6  0
033900040108     d DateU           s              8  0
034000040213     d ora             s              2  0
034100040213     d minuti          s              2  0
034200040426     d wgiorno         s              2  0
034300151201     d w0090           s              9  0
034400041206     d w0070           s              7  0
034500041206     d w0040           s              4  0
034600130611     d w003a           s              3
034700040212     d wtibs69r        s              1
034800040212     d w1cdar          s              8  0
034900040217     d w1corr          s              6  0
035000040213     d wmod            s              1
035100040217     d rec_wrt         s              3  0
035200040217     d s01nrr          s              4  0
035300040217     d wdar            s              8  0
035400070329     d wdcr            s              8  0
035500040426     d giorni          s              3  0
035600041206     d wforz           s              1
035700040218     d wrk_recsbf      s              1
035800040218     d Wfidnb0r        s              1
035900070327     d W_cau           s              1
036000040219     d W_confe         s              1
036100040323     d W_ute           s              3
036200040617     d W_pot           s              3  0
036300040422     d W_alloc         s              1
036400070330     d Not_dcr         s              1
036500070330     d Not_ldv         s              1
036600141210     d Not_PINcode     s              1    inz
036700070402     d w_dtcr          s             11
036800040531     d forz_rec        s              1
036900041213     d mod_testa       s              1
037000050119     d Sv_inkd         s              1
037100070330     d Note_dcr        s             78
037200130610     d wabi            s                   like(UteAut)
037300150313     d w0030           s              3  0
037400150313     d sav_in48        s              1a
037500150316     d wtmo            s                   like(TGMtmo)
037600150422     d $Video          s              2    inz
037700150422     d $GesW04         s               n   inz
037800150422     d $Reparto        s               n   inz
037900150421     d wwMail          s                   like(V1mail)  inz
038000150421     d wwcMAD          s                   like(V1Cmad)  inz
038100150422     d wwdMAD          s                   like(V1Dmad)  inz
038200150421     d wwcPOG          s                   like(V1Cpog)  inz
038300150422     d wwdPOG          s                   like(W4Dpog)  inz
038400150423     d wwcRPR          s                   like(W4Crpr)  inz
038500150423     d wwdRPR          s                   like(W4Drpr)  inz
038600070329
038700141212     d xx              s              3  0 inz
038800070329     d x               s              2  0
038900070329     d y               s              2  0
039000070329      *
039100040426
039200040426     d ktfp            s                   like(clntfp)
039300040426     d ktfa            s                   like(clntfa)
039400040426     d kann            s                   like(clnann)
039500040426     d kmes            s                   like(clnmes)
039600040324
039700070327     d W_LAV           s                   like(tgdmad) inz('  2')
039800070327     d W_CON           s                   like(tgdmad) inz(' 10')
039900070327     d W_SUP           s                   like(tgdmad) inz(' 14')
040000070327     d W_APP           s                   like(tgdmad) inz(' 15')
040100070327     d W_FDE           s                   like(tgdmad) inz(' 16')
040200140429     d c_FDEAM         c                   const(' 17')
040300151130     d c_MotAp_Reso    c                   const(' 18')
040400141210     d c_VPINc         c                   const(' 60')
040500150717     d c_ReclSede      c                   const(' 70')
040600070327
040700040324     D Wdataeur        S               D   DATFMT(*eur)
040800040324     D Wdataoggi       S               D   DATFMT(*eur)
040900040216
041000040421     d Keyant          s                   like(tgdant)
041100040218     d Keynut          s                   like(tgdnut)
041200040218     d Keypdt          s                   like(tgdpdt)
041300040212     d Keytri          s                   like(tgrtri)
041400070402     d Keytin          s                   like(tgntin) inz('GEN')
041500040217     d W_v1cchi        s                   like(v1cchi)
041600041207     d Wrk_cchiA       s                   like(v1cchi)
041700041207     d Wrk_cchiB       s                   like(v1cchi)
041800041207     d Wrk_cchiC       s                   like(v1cchi)
041900041207     d Wrk_cchi        s                   like(v1cchi)
042000041206     d SW_v1cchi       s                   like(v1cchi)
042100070503     d SW_cau          s                   like(W_cau)
042200070508     d note_blocco     s                   like(tgnnot)
042300040218     d W_por           s                   like(orgfil)
042400070412     d W_Cmad          s                   like(v1cmad)
042500150122     d wSaveMAD        s                   like(V1Cmad) inz
042600040531     d W_v1cogg        s                   like(tgdogg)
042700040604     d SW_v1cogg       s                   like(tgdogg)
042800041210     d Sv_pdt          s                   like(v1cpdt)
042900141210     d wTGRpdt         s                   like(TGRpdt) inz
043000150326     d W_prt           s                   like(TGDprt)
043100150625     d wSavePRT        s                   like(TGDprt) inz
043200040217
043300070322      * - Flags booleani
043400131119     d $ric_fnlrg2r    s               n   inz(*off)                            richiamo FNLRG2R
043500131119     d $abilitato      s               n   inz(*off)                            abilitato a geo
043600131119     d $riemetti       s               n   inz(*off)                            riemissione
043700131119     d $bloccof3       s               n   inz(*off)                            blocco f3
043800131206     d $cons           s               n   inz(*off)                            spediz.in consegna
043900131119     d $fnarb          s               n   inz(*off)                            trovato fnarb
044000140506     d $RA_MA_17       s               n   inz                                  RA con mot.ap. 17
044100151203     d $RA_MA_18       s               n   inz                                  RA con mot.ap. 18
044200141210     d $RA_MA_60       s               n   inz                                  RA con mot.ap. 60
044300151201     d $Ok_IDCprg      s               n   inz
044400151202     d $WrtTIIDC       s               n   inz
044500040217      *
044600040217      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
044700040217      *
044800040217
044900040610     D TIT1            C                   CONST('*** IMMISSIONE RICHIESTE ***')
045000040610     D TIT2            C                   CONST('** MANUTENZIONE RICHIESTE **')
045100041213
045200140429     d c_CauVarB_I7    c                   const('I7')
045300150421
045400150421     d c_Sede          c                   const('046')
045500150311
045600150311     d Digits          c                   const('0123456789')
045700140123
045800140123       //--------------------------------------------------------------
045900140123       //?Definizione prototipi e procedure.
046000140123       //--------------------------------------------------------------
046100140123
046200141210       // -?Reperimento dati tabelle?
046300141210      /copy gaitrasrc/srcProtoPI,TRULTAB
046400141210      /copy gaitrasrc/srcProtoPR,TRULTAB
046500151201
046600151201       // -?Reperimento/Aggiornamento Numeratori in GRU?
046700151201      /copy gaitrasrc/srcProtoPR,TRUL33R
046800141210
046900141210       // -?Gestione ricerca / Controllo tabelle?
047000150911      /copy gaitrasrc/srcProtoPR,TIBS02R
047100140429
047200140924       // -?Variazione bolle in arrivo?
047300140924      /copy gaitrasrc/srcProtoPR,FNLR48R
047400140429
047500140429       // -?Controllo Causali Variazione bolle?
047600140429      /copy gaitrasrc/srcProtoPR,FNLR66R
047700151130
047800151130       // -?Reperimento Disposizioni di Consegna da WEB?
047900151130     d FNLRY09ds2    e ds                  inz
048000151130     d FNLRY09R        pr                  extpgm('FNLRY09R')
048100151130     d   kpjba                             likeds(KPJBA)
048200141210
048300141210       // -?Richiesta e Controllo del PINcode?
048400141210     d trulPINds     e ds                  inz
048500141210      /copy gaitrasrc/srcProtoPR,TRULPINR
048600150310
048700150310       // -?Immissione ORM
048800150310     d FIOR05R         pr                  extpgm('FIOR05R')
048900150911     d   kpjba                             likeds(kpjba)
049000150911     d   fior05ds                          likeds(FIOR05DS)
049100150318
049200150318       // -?Storico Variazini
049300150318     d FIDNA3R         pr                  extpgm('FIDNA3R')
049400150911     d   kpjba                             likeds(kpjba)
049500150911     d   fidna0ds                          likeds(FIDNA0DS)
049600150317
049700150317       // -?Caricamento filiali abilitate
049800150911     d TRUL31R         pr                  extpgm('TRUL31R')
049900150911     d   kpjba                             likeds(KPJBA)
050000150911     d   trul31ds                          likeds(TRUL31DS)
050100150319
050200150508       // -?Selezione tab. "RGR" = Reparto Gestione R.A.?
050300150508     d tntbRGRds     e ds                  inz
050400150508     d tntbRGRr        pr                  extpgm('TNTBRGRR')
050500150508     d   kpjba                             likeds(kpjba)
050600150508
050700150319       // -?Selezione tab. "UGR" = Utenti
050800150319     d tntbUGRds     e ds                  inz
050900150319     d tntbUGRr        pr                  extpgm('TNTBUGRR')
051000150319     d   kpjba                             likeds(kpjba)
051100150915
051200150915      *// // -?Interrogazione CAP?
051300150915     d*// TISI00ds      e ds                  inz
051400150915     d*// tisi02r         pr                  extpgm('TISI02R')
051500150915     d*//   kpjba                             likeds(kpjba)
051600150915     d*//   tisi00ds                          likeds(TISI00ds)
051700150915
051800150915       // -?Interrogazione Località?
051900150915     d TISI00ds      e ds                  inz
052000150915     d TISI30ds      e ds                  inz
052100150915     d tisi30r         pr                  extpgm('TISI30R')
052200150915     d   kpjba                             likeds(kpjba)
052300150915     d   tisi00ds                          likeds(TISI00ds)
052400150915     d   tisi30ds                          likeds(TISI30ds)
052500150911
052600150911       // -?Controllo CAP ("Pretrullo TISI95R")?
052700150911     d TISI95ds      e ds                  inz
052800150911     d tisi95r         pr                  extpgm('TISI95R')
052900150911     d   tisi95ds                          likeds(TISI95ds)
053000150915
053100151130       // -?Reperimento Terminal di partenza/arrivo?
053200150915     d FNLV55ds      e ds                  inz
053300150915     d   D55tla      e                     inz('L')
053400150915     d   D55tpt      e                     inz('P')
053500150915     d fnlv55r         pr                  extpgm('FNLV55R')
053600150915     d   fnlv55ds                          likeds(FNLV55ds)
053700140429
053800040323      *
053900040108      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
054000040108      *
054100151201      * -?File TIIDC02L?
054200151201     d keyTIIDC02    e ds                  extname( TIIDC02L : *key )
054300151201     d                                     prefix( k_ )  inz
054400040120      * - file FITGD01L
054500040421     c     ktgdan        klist
054600040421     c                   kfld                    v1cant
054700040421     c                   kfld                    v1cnut
054800040212     c     ktgd          klist
054900040421     c                   kfld                    v1cant
055000040212     c                   kfld                    v1cnut
055100040212     c                   kfld                    v1cpdt
055200040218      *
055300040218     c     ktgdrec       klist
055400040421     c                   kfld                    Keyant
055500040218     c                   kfld                    Keynut
055600040218     c                   kfld                    Keypdt
055700040212      * - file FITGR01L
055800141211     c     kTGRan0       klist
055900141211     c                   kfld                    v1cant
056000141211     c                   kfld                    v1cnut
056100040421     c     ktgran        klist
056200040421     c                   kfld                    v1cant
056300040421     c                   kfld                    v1cnut
056400141210     c                   kfld                    wTGRpdt
056500040421      *
056600040421     c     ktgran2       klist
056700040421     c                   kfld                    tgdant
056800040421     c                   kfld                    tgdnut
056900141210     c                   kfld                    wTGRpdt
057000040421      *
057100150325      * - file FITGD32L
057200150325     c     ktgd32        klist
057300040531     c                   kfld                    v1ctor
057400040531     c                   kfld                    W_v1cogg
057500070402      * - file FITGN01L
057600070402     c     ktgn          klist
057700070402     c                   kfld                    v1cant
057800070402     c                   kfld                    v1cnut
057900070402     c                   kfld                    v1cpdt
058000070402     c                   kfld                    keytin
058100040107
058200070502      * - file FNARB01L - FNBLP01L -
058300040210     c     ksped         klist
058400040210     c                   kfld                    v1anns
058500040210     c                   kfld                    v1lnps
058600040210     c                   kfld                    v1nrss
058700040210     c                   kfld                    v1nsps
058800041209
058900041209     c     kspedw        klist
059000041209     c                   kfld                    W_aas
059100041209     c                   kfld                    W_lnp
059200041209     c                   kfld                    W_nrs
059300041209     c                   kfld                    W_nsp
059400040211      * - file FNORM01L
059500040211     c     korm          klist
059600040211     c                   kfld                    v1poeo
059700040211     c                   kfld                    v1nsor
059800040211     c                   kfld                    v1norm
059900040211     c                   kfld                    v1nrvo
060000041209
060100041209     c     kormw         klist
060200041209     c                   kfld                    W_poe
060300041209     c                   kfld                    W_nsr
060400041209     c                   kfld                    W_nor
060500041209     c                   kfld                    W_nrv
060600040426      * - file AZCLN01L
060700040426     c     kazcln        klist
060800040426     c                   kfld                    ktfp
060900040426     c                   kfld                    ktfa
061000040426     c                   kfld                    kann
061100040426     c                   kfld                    kmes
061200040210
061300040107      *===============================================================*
061400040107      *  RIEPILOGO INDICATORI                                         *
061500040107      *---------------------------------------------------------------*
061600040126      *  01    - Visualizza numero spedizione                         *
061700040126      *  02    - Visualizza numero Orm                                *
061800040126      *  03    - Visualizza chiave generica                           *
061900041005      *  05    - Sono in Sede                                         *
062000041012      *  07    - Utente abilitato a modificare tutto                  *
062100070608      *  08    - Filiale abilitata a GEO                              *
062200070625      *  09    - Bolla in partenza non borderizzata                   *
062300040324      *  10    - Posizionamento cursore                               *
062400050118      *  11    - Posizionamento cursore                               *
062500040421      *  15    - Indicatore  da video numero reclamo                  *
062600131206      *  16    - Spedizione in consegna non visualizza F17 Note Aut   *
062700150319      *  17    - Visualizzo F11=Variazioni                            *
062800040126      *  20    - SFLDSPCTL in C01                                     *
062900040126      *  21    - SFLDSP    in C01                                     *
063000040126      *  21    - SFLEND    in C01                                     *
063100040202      *  22    - SFLCLR    in C01                                     *
063200040109      *  28    - Errore generico dspf                                 *
063300040212      *  29    - Visualizzo dicitura "Altre note"                     *
063400120919      *  30/48 - Errore                                               *
063500150420      *  49    - Immissione: non gestisce Filiale/Reparto/Utente gest.*
063600040303      *  50    - Manutenzione usato per proteggere/non visualizzare   *
063700040303      *          campi e comandi video                                *
063800040324      *  51/54 - Protezione note                                      *
063900150421      *  55    - Errore - n° Telefono in W04                          *
064000150911      *  56    - Errore - C.A.P. errato in W04                        *
064100070328      *  60    - Protezione note dopo richiamo gestione blocchi       *
064200070329      *  61    - emissione data consegna richiesta                    *
064300070403      *  63    - Non si può più dare f3                               *
064400040324      *  70    - Protezione ... non si può modificare nulla           *
064500041012      *  72    - Manutenzione e non è stato selezionato nulla         *
064600150624      *  73    - Protezione ... non si può modificare Tipo Comunicaz. *
064700070326      *  74    - Protezione ... non si può modificare motivo apertura *
064800150624      *  75    - Comodo in lookup                                     *
064900040220      *  79    - Errore campo non numerico                            *
065000040707      *  80    - Oggetto inesistente                                  *
065100040305      *  81    - Posizionamento cursore Opzione subfile               *
065200040422      *  82    - Reverse immage cursore record allocato               *
065300141215      *  83    - Campo opzione (nel sfl) protetto & non visualizzato  *
065400150317      *  84    - RA non manutenzionabile dall'utente                  *
065500040109      *  90    - Errore                                               *
065600040107      *===============================================================*
065700040107
065800040129      *
065900150319     c                   eval      *in17 = *off
066000040129      *
066100040129     c                   do        *hival
066200040213      * se sono in modalità immissione
066300040213     c                   if        wmod = 'I'
066400040213      * Pulizia del video
066500040213     C                   exsr      SR_PUL01
066600040213      *
066700040213     c                   else
066800040213      * carico il reclamo da gestire
066900040303     c                   exsr      SR_RECupero
067000040213      *
067100040213     c                   endif
067200040213
067300040213      * gestione videata di immissione/manutenzione reclamo
067400040213
067500040129     c                   exsr      SR_RECLAMO
067600041214      * verifico se modificata la testa aggiorno i file nella parte della testata
067700041214     c                   if        Mod_testa = 'S'
067800041214     c                   exsr      SR_chiudo
067900041214     c                   endif
068000040213
068100040129      * fine programma F3
068200040213
068300040304     c                   if        *inkc  OR (*INKL and *IN50)
068400041213      *
068500040315     c                   if        *inkc and *in50
068600041213      *
068700041213      * se sono stata richiamata valorizzo la ds FIDNA0DS
068800040305     c                   eval      oa0f03 = 'S'
068900040305     c                   endif
069000040305
069100040129     c                   leave
069200040129     c                   endif
069300040217      *
069400040129     c                   enddo
069500040202
069600040202     C                   IF        Wtibs69r  <>  *BLANKS
069700040202     C                   eval      I69TLA  = 'C'
069800040202     C                   call      'TIBS69R'
069900040202     C                   parm                    TIBS69DS
070000040202     C                   ENDIF
070100040617      *
070200040617     c                   clear                   tibs02ds
070300040617     c                   movel     'C'           t02tla
070400040617     c                   call      'TIBS02R'
070500040617     c                   parm                    kpjba
070600040617     c                   parm                    tibs02ds
070700040617      *
070800040617     c                   clear                   FIDNB0DS
070900040617     c                   movel     'C'           ib0tla
071000040617     c                   movel     FIDNB0DS      kpjbu
071100040617     c                   call      'FIDNB0R'
071200040617     c                   parm                    kpjba
071300040617     c                   parm                    FIDNB0DS
071400150928      *
071500150928     c                   clear                   fidnCAds
071600150928     c                   eval      IC0tla = 'C'
071700150928     c                   call      'FIDNCAR'
071800150928     c                   parm                    fidnCAds
071900040617      *
072000141210     c                   callb     'CLOTABELLA'
072100141210     c                   parm      c_Tabel       wFilTab           1
072200141210
072300040129      *
072400040129     c                   seton                                        lr
072500040129      *---------------------------------------------------------------*
072600040129      * Stacco il numero dal numeratore                               *
072700040129      *---------------------------------------------------------------*
072800040129     c     SR_NUMERO     BegSR
072900040129      *
073000040421      *
073100040421     c                   time                    w0140
073200040421     C                   MOVE      W0140         V1CANT
073300040421
073400141020     c                   dou       v1cnut > *zeros
073500040129     C                   clear                   Trul33ds
073600040129     c                   eval      i33tla = 'L'
073700040129     C                   z-add     0             I33ope
073800141020     C                   z-add     039           I33cnu
073900040129     C                   z-add     1             I33num
074000040421     C                   z-add     v1cant        I33aaa
074100040129     C                   movel(p)  trul33ds      kpjbu
074200041007      * se sono in sede richiamo il CL
074300041007     c                   If        *in05
074400041007     C                   CALL      'TRUL33C'
074500041007     C                   PARM                    KPJBA
074600041007     c                   else
074700040129     C                   CALL      'TRUL33R'
074800040129     C                   PARM                    KPJBA
074900041007     c                   endif
075000040129     C                   eval      trul33ds = kpjbu
075100040129      *
075200040129     C                   if        o33err = *zeros
075300040202     C                   z-add     o33nri        v1cnut
075400141020     c                   endif
075500141020     c
075600141020     c                   enddo
075700040421      * spengo indicatore del video
075800040421     c                   setoff                                       15
075900040416      *
076000040421      * se data chiamata uguale a zero la imposto
076100040421      * imposto ora e data inserimento chiamata
076200040421     c                   if        v1cdar = *zeros
076300040416     c                   move      w0140         v1cdar
076400040421     c                   movel     w0140         v1corr
076500040421     c                   endif
076600040421
076700040416     c                   movel     w0140         W1corr
076800040416     c                   clear                   WLBdat
076900040422     c                   move      w0140         g02dat
077000040416     c                   call      'XSRDA8'
077100040416     c                   parm                    WLBdat
077200040416     c                   z-add     G02inv        w1cdar
077300040129      *
077400040129     c                   endsr
077500040202      *---------------------------------------------------------------*
077600040213      * Routine gestione SUBFILE - CONTROL   RECLAMO                  *
077700040202      *---------------------------------------------------------------*
077800040213     c     SR_RECLAMO    BegSR
077900040202
078000040202     c                   seton                                        20
078100150421     c                   eval      *in49 = ( wMod = 'I')
078200040202      *
078300040202     c                   Do        *Hival
078400070330     c                   write     fia1z01
078500040202      * emetto la videata
078600040202     c                   exfmt     fia1c01
078700040203     c                   setoff                                       281090
078800040212     c                   setoff                                       303132
078900040212     c                   setoff                                       333435
079000040212     c                   setoff                                       363738
079100040217     c                   setoff                                       394041
079200040217     c                   setoff                                       424344
079300070330     c                   setoff                                       461162
079400141029     c                   setoff                                       644748
079500040202     c                   clear                   V1Cmsg
079600050120     c                   clear                   SV_inkd
079700111025     c                   clear                   $riemetti
079800040304
079900040304      * F12 = Ritorno al programma chiamante  (attivo solo per 50 acceso)
080000040304
080100040304     c                   If        *inkl
080200040304     c                   leave
080300040304     c                   endif
080400040216
080500040216      * F3 = Fine  se non sono in inserimento vado a fine pgm  altrimenti
080600040216      *            prima verifco che non sia stato inserito nulla altrimenti emetto
080700040216      *            un messaggio forzabile e pulisco tutto quello che è stato inserito fino adesso
080800040416      * ATTENZIONE TUTTO QUELLO CHE E' STATO INSERITO ANDRA' PERSO !!!! FORZARE F6
080900070328      *            ma solo se non è stato modificata una spedizione per lasciato avviso o per altri
081000070326      *            blocchi F3 è disabilitato
081100040223
081200040223     c                   If        *inkc
081300040223
081400040223     c                   If        Wmod <> 'I'  or
081500040223     c                             (wmod = 'I' and rec_wrt = 0 and
081600040308     c                             v1ctor = *blanks )
081700040305      *
081800040202     c                   leave
081900040202     c                   endif
082000070326      *
082100070326     c                   If        (wmod = 'I' and rec_wrt > 0) or
082200070326     c                             (wmod = 'I' and rec_wrt = 0 and
082300070326     c                             v1ctor <> *blanks )
082400070326     c                   exsr      SR_GesWin
082500040216      *
082600040216     c                   if        *inkf
082700040223     c                   eval      *inkf  = *off
082800040223     c                   eval      *inkc  = *on
082900040216     c                   leave
083000040216     c                   endif
083100040203
083200040216     c                   iter
083300040216     c                   endif
083400040223
083500040223     c                   endif
083600040416      * se numero reclamo uguale a zero lo imposto
083700040416     c                   if        v1cnut = *zeros
083800040416     c                   exsr      SR_numero
083900040416     c                   endif
084000150609
084100150609      * F1=Interrogazione piano dei conti
084200150609
084300150609     c                   If        *inKA
084400150609     c                   exsr      SR_ricalf
084500150609      * accendo posizionamento cursore su chiamante
084600150609     c                   seton                                        44
084700150609     c                   iter
084800150609     c                   EndIf
084900040216
085000150310      * F10=Immissione ORM Telefonici
085100040202     c                   If        *inKJ
085200150310     c                   exsr      SR_F10
085300040217      * accendo posizionamento cursore su chiamante
085400040217     c                   seton                                        44
085500040421     c                   iter
085600040203     c                   endif
085700150318
085800150318      /free
085900150318       //?F11=Visualizza Variazioni
086000150318         IF  *inkk;
086100150318           exsr sr_F11;
086200150318           *in44 = *on;   // posizione cursore su chiamante
086300150318           iter;
086400150318         ENDIF;
086500150318      /end-free
086600040213
086700040203      * Controllo il codice e recupero categoria del cliente
086800040203
086900040202     c                   if        v1cchi > 0 and W_v1cchi <> v1cchi
087000040203
087100040202      * aggancio anagrafico
087200040203
087300040202     C                   clear                   TIBS69DS
087400040202     c                   z-add     v1cchi        I69kac
087500040202     c                   z-add     v1cchi        I69kcp
087600040202     C                   call      'TIBS69R'
087700040202     C                   parm                    tibs69DS
087800040202     C                   parm                    DS_cnaco
087900040202     C                   parm                    DS_cnind
088000040202     C                   parm                    DS_cnclp
088100040202     C                   parm                    DS_fncls
088200040202     C                   eval      wtibs69r = '1'
088300040203     c                   If         O69ERR =  '1'
088400040203     C                   eval      *IN28 = *on
088500040203     c                   movel     msg(1)        V1Cmsg
088600040203     C                   eval      *IN30 = *on
088700040213     C                   iter
088800040202     c                   Else
088900050118     c                   movel     ACOrag        v1dchi
089000041206
089100130411     c                   eval      W_v1cchi = v1cchi
089200040202     c                   movel     clpclv        v1cclv
089300040202     c                   Endif
089400040203
089500040203      * se cambiato codice cliente oppure decodificato per la prima volta riemetto la videata
089600040203
089700040203     c                   iter
089800040203
089900040202     c                   endif
090000050119      * se KD prima faccio KF poi faccio kd
090100050119     c                   If        *inkd and v1ctor <> ' '
090200050119     c                   eval      SV_inkd = *inkd
090300050119     c                   eval      *inkd  = *off
090400050119     c                   eval      *inkf  = *on
090500050119     c                   endif
090600050119
090700040203      * richiamo la routine di controllo campi video
090800040203     c                   exsr      SR_CONTR01
090900040212      * se all'uscita della routine di controllo indicatore di errore generico
091000040212      * e di riemissione della videata è acceso riemetto la videata
091100040216     c                   if        *in90 or *in28
091200040212     c                   iter
091300040212     c                   endif
091400040212
091500040615      * F6 =Conferma riga  oggetto  non eseguo se 72 acceso
091600040212
091700040615     c                   If        *inKF and not *in72
091800040212     c                   exsr      SR_CONFE
091900040213      *
092000040213      * pulisco la parte del control relativa all'oggetto
092100040213      *
092200040213     c                   exsr      SR_pulctl
092300040305      * posizionamento cursore su tipo oggetto  se sono in modalità immissione
092400040305     c                   If        Wmod = 'I'
092500040213     c                   seton                                        31
092600040305      *
092700040305      * posizionamento cursore su opzione del subfile
092800040305     c                   else
092900141215     c                   eval      *in72 = *on
093000040305     c                   endif
093100040213      * carico subfile
093200040216     c                   exsr      sr_carsbf
093300040212     c                   endif
093400050119      *
093500050119     c                   if        sv_inkd = '1'
093600050119     c                   eval      *inkf = *off
093700050119     c                   eval      *inkd = *on
093800050119     c                   clear                   sv_inkd
093900050119     c                   exsr      SR_contr01
094000050119      * se all'uscita della routine di controllo indicatore di errore generico
094100050119      * e di riemissione della videata è acceso riemetto la videata
094200050119     c                   if        *in90 or *in28
094300050119     c                   iter
094400050119     c                   endif
094500050119     c                   endif
094600050119      *
094700040213
094800040213      * F4 = FINE CHIAMATA
094900040213
095000040216     c                   If        *inKD
095100040219      *
095200040219      * se devo scrivere un dettaglio prima di chiudere la chiamata il campo W_confe = ' '
095300040219      *
095400040219     c                   if        W_confe <> 'N'
095500040216     c                   exsr      SR_CONFE
095600040219     c                   eval      W_confe = ' '
095700040219     c                   endif
095800040219
095900040219     c                   exsr      SR_chiudo
096000040216
096100040216     c                   leave
096200040216
096300040216     c                   endif
096400040202      *
096500040203     c                   enddo
096600040202      *
096700040203     c                   endsr
096800040202      *
096900040203      *---------------------------------------------------------------*
097000040203      * Routine controllo campi video                                 *
097100040203      *---------------------------------------------------------------*
097200040203     c     SR_CONTR01    BegSR
097300040203
097400040203      *
097500040211    1c                   Do        *Hival
097600040219      *
097700040219      * se dato F4  FINE CHIAMATA
097800040219      *
097900041206    2c                   if        *inkd
098000070430      * controlli della parte alta della videata
098100070430     c                   exsr      sr_contrf4
098200070430     c                   if        *in28
098300070430     c                   leave
098400070430     c                   endif
098500070430      *
098600041206    2c                   endif
098700041206      *
098800040213
098900040213      * Controllo tipo comunicazione
099000040213
099100150625     c                   clear                   dCOR
099200040213    2c                   If        v1ctco <> *blank
099300040213      * Ricerca
099400040213     c     '?'           scan      v1ctco
099500040213    3c                   if        %found
099600040217     c                   clear                   tibs02ds
099700040213     C                   MOVEL     'R'           t02mod
099800040213     C                   MOVEL     knsif         t02sif
099900040213     C                   MOVEL     'COR'         t02cod
100000040213      *
100100040213     C                   CALL      'TIBS02R'
100200040213     C                   PARM                    KPJBA
100300040213     C                   PARM                    TIBS02DS
100400040213      *
100500040213    4C                   IF        T02err = *BLANKS
100600040217     C                   movel     T02uni        DCOR
100700040213     C                   movel     T02ke1        v1ctco
100800040213     C                   movel     §CORdesc      v1dtco
100900040223      *  errore (non ritorna nulla dalla selezione pulisco il campo)
101000040213     C                   ELSE
101100040223     c                   clear                   v1ctco
101200040223     c                   clear                   v1dtco
101300040213    4C                   ENDIF
101400040213      *
101500040213     c                   eval      *in90 = *on
101600040219     C                   eval      *IN41 = *on
101700040213     c                   leave
101800040213      *
101900040213   x3C                   ELSE
102000040213      *
102100040213      * Controllo
102200040217     c                   clear                   tibs02ds
102300040213     C                   MOVEL     'C'           t02mod
102400040213     C                   MOVEL     knsif         t02sif
102500040213     C                   MOVEL     'COR'         t02cod
102600040213     C                   MOVEL     v1ctco        t02ke1
102700040213      *
102800040213     C                   CALL      'TIBS02R'
102900040213     C                   PARM                    KPJBA
103000040213     C                   PARM                    TIBS02DS
103100040213      *
103200040213    4C                   IF        t02err = *BLANKS
103300040213     C                   movel     T02uni        DCOR
103400140923     c                   if        §CORTCUT =  'N'
103500140923     c                   eval      *in28 = *on
103600140923     c                   movel     MSG(43)       V1Cmsg
103700140923     C                   eval      *IN41 = *on
103800140923     c                   clear                   v1dtco
103900140923     c                   leave
104000140923     c                   endif
104100040213     C                   movel     T02ke1        v1ctco
104200040213     C                   movel     §CORdesc      v1dtco
104300040213      *
104400040213     C                   ELSE
104500040213     c                   eval      *in28 = *on
104600040223     c                   movel     MSG(17)       V1Cmsg
104700040213     C                   eval      *IN41 = *on
104800040213     c                   leave
104900040213    4C                   ENDIF
105000040213      *
105100040213    3c                   endif
105200040213      *
105300040213   x2c                   else
105400040213      *
105500040213      *  Campo Obbligatorio
105600040213     c                   eval      *in28 = *on
105700040213     c                   movel     MSG(10)       V1Cmsg
105800040223     C                   eval      *IN41 = *on
105900040213     c                   leave
106000040213    2C                   endif
106100040213
106200040213      * Controllo data e ora comunicazione
106300040213
106400040428    2C                   if        v1cdar =  *zeros
106500040428     c                   eval      *in28 = *on
106600040428     c                   movel     MSG(11)       V1Cmsg
106700040428     C                   eval      *IN42 = *on
106800040428     C                   leave
106900040428    2C                   else
107000040213     C                   clear                   wlbdat
107100040213     C                   z-add     v1cdar        G02dat
107200040213     C                   call      'XSRDA8'
107300040213     C                   parm                    wlbdat
107400040213    3C                   if        G02err = '1'
107500040213     c                   eval      *in28 = *on
107600040213     c                   movel     MSG(11)       V1Cmsg
107700040213     C                   eval      *IN42 = *on
107800040213     C                   leave
107900040213     C                   else
108000040213     C                   z-add     G02dat        v1cdar
108100040213     C                   z-add     G02inv        Wdar
108200040213    3C                   endif
108300040223      * verifico che la data non sia maggiore della data odierna
108400040223    3c                   if        wdar > dateu
108500040223     c                   eval      *in28 = *on
108600040223     c                   movel     MSG(21)       V1Cmsg
108700040223     C                   eval      *IN42 = *on
108800040223     C                   leave
108900040223    3c                   endif
109000040223
109100040213    2C                   endif
109200040213      * Ora comunicazione
109300040213    2C                   if        v1corr > *zeros
109400040213     C                   movel     v1corr        ora
109500040213     C                   move      v1corr        minuti
109600040213
109700040213    3C                   if        ora > 24 or minuti > 59
109800040213     c                   eval      *in28 = *on
109900040213     c                   eval      *in43 = *on
110000040217     C                   movel     msg(12)       v1cmsg
110100040213     c                   leave
110200040213    3C                   endif
110300040213
110400040213     C                   else
110500040213     c                   eval      *in28 = *on
110600040213     c                   eval      *in43 = *on
110700040217     C                   movel     msg(12)       v1cmsg
110800040213     c                   leave
110900040213    2C                   endif
111000040219
111100050119      * controllo del chiamante se uguale a blanks e sono in manutenzione
111200050119
111300050119    2c                   If        v1crsc = *blanks and *in50
111400050119     c                   eval      *in28 = *on
111500050119     c                   movel     MSG(15)       V1Cmsg
111600050119     C                   eval      *IN44 = *on
111700050119     c                   leave
111800050119    2c                   endif
111900040213
112000040213      * verifico se inserito indirizzo mail corretto
112100040213
112200040213    2c                   If        v1mail <> *blanks
112300150421     c                   eval      wwMail = V1mail
112400150421     c                   exsr      sr_CtrlMail
112500150421     c                   eval      V1mail = wwMail
112600150421     c                   if        *in28
112700150421     c                   leave
112800150421     c                   endif
112900040213    2c                   endif
113000040203      *
113100040203      * gestione OGGETTO reclamo
113200040203      *
113300040203
113400040203      * F7 Bolle Arrivi  Interrogazione e ricerca bolla
113500040203      *
113600040211    2C                   IF        *INKG
113700040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
113800040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
113900040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
114000040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
114100040223     c                   exsr      SR_GESWIN2
114200040223      * ritorno alla videata senza interrogare
114300040223     c                   If        *inKL
114400040223     c                   eval      *in90 = *on
114500150324     c                   if        wMod = 'I'
114600150114     c                   eval      *in48 = *on
114700150114     c                   else
114800040223     c                   eval      *in31 = *on
114900150114     c                   endif
115000040223     c                   leave
115100040223     c                   endif
115200040205      *
115300040223     c                   endif
115400040223      *
115500041005      * se non sono in sede interrogo le bolle Arrivi
115600041005      *
115700041005   a2c                   if        not *in05
115800040203     c                   clear                   DS_ricbolA
115900040220     C                   MOVEL     2             DSRBA_flg
116000040203     C                   MOVEL(P)  DS_ricbola    KPJBU
116100040205      * preparo l'altro parametro da passare
116200040205     c                   clear                   fidna1ds
116300040421     c                   eval      IA1ant = v1cant
116400040205     c                   eval      IA1nut = v1cnut
116500040205     c                   eval      IA1pdt = v1cpdt
116600040205      *
116700040205     c                   CALL      'FNLR36R'
116800040203     C                   PARM                    KPJBA
116900040205     C                   PARM                    FIDNA1DS
117000040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
117100040211    3c                   IF        IA1INS = 'S'
117200040217     c                   eval      v1ctor = 'S'
117300040205     c                   movel     IA1ogg        DS_Spediz
117400040205     c                   seton                                        01
117500040205     c                   setoff                                       0203
117600040205     C                   z-add     DSspe_aas     V1anns
117700040205     C                   z-add     DSspe_lnp     V1lnps
117800040205     C                   z-add     DSspe_nrs     V1nrss
117900040205     C                   z-add     DSspe_nsp     V1nsps
118000140204      * verifico se spedizione in consegna
118100140204     c                   exsr      Ctr_specons
118200131206
118300040211    3C                   endif
118400041007      *
118500041007      * sono in sede
118600041007      *
118700041007     c                   else
118800041007     c                   clear                   fidna1ds
118900041007     c                   eval      IA1ant = v1cant
119000041007     c                   eval      IA1nut = v1cnut
119100041007     c                   eval      IA1pdt = v1cpdt
119200041007      *
119300041008     c                   eval      %SUBST(KPJBU:2:2) = '01'
119400041007      *
119500041007     c                   CALL      'TNSB50R'
119600041007     C                   PARM                    KPJBA
119700041007     C                   PARM                    FIDNA1DS
119800041007      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
119900041007    3c                   IF        IA1INS = 'S'
120000041007     c                   eval      v1ctor = 'S'
120100041007     c                   movel     IA1ogg        DS_Spediz
120200041007     c                   seton                                        01
120300041007     c                   setoff                                       0203
120400041007     C                   z-add     DSspe_aas     V1anns
120500041007     C                   z-add     DSspe_lnp     V1lnps
120600041007     C                   z-add     DSspe_nrs     V1nrss
120700041007     C                   z-add     DSspe_nsp     V1nsps
120800041007     c                   endif
120900041007      *
121000041005   a2c                   endif
121100150112      *
121200040212     c                   eval      *in90 = *on
121300150324if  3c                   if        wMod = 'I'
121400150114     c                   exsr      sr_ImpAut_Ins
121500150114x   3c                   else
121600150112     c                   eval      *in35 = *on
121700150114e   3c                   endif
121800040205     C                   LEAVE
121900040211    2C                   endif
122000040203      *
122100040205
122200040317      * F8  Bolle Partenza Interrogazione e ricerca bolla
122300040205      *
122400040317    2C                   IF        *INKH
122500040223      *
122600040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
122700040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
122800040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
122900040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
123000040223     c                   exsr      SR_GESWIN2
123100040223      * ritorno alla videata senza interrogare
123200040223     c                   If        *inKL
123300040223     c                   eval      *in90 = *on
123400150324     c                   if        wMod = 'I'
123500150114     c                   eval      *in48 = *on
123600150114     c                   else
123700040223     c                   eval      *in31 = *on
123800150114     c                   endif
123900040223     c                   leave
124000040223     c                   endif
124100040223      *
124200040223     c                   endif
124300040205      *
124400040205     c                   clear                   DS_ricbolP
124500040205     C                   MOVEL(P)  DS_ricbolp    KPJBU
124600040205      * preparo l'altro parametro da passare
124700040205     c                   clear                   fidna1ds
124800040421     c                   eval      IA1ant = v1cant
124900040205     c                   eval      IA1nut = v1cnut
125000040205     c                   eval      IA1pdt = v1cpdt
125100040205      *
125200040205     c                   CALL      'FNLS04R'
125300040205     C                   PARM                    KPJBA
125400040205     C                   PARM                    FIDNA1DS
125500040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
125600040211    3c                   IF        IA1INS = 'S'
125700040217     c                   eval      v1ctor = 'S'
125800040205     c                   movel     IA1ogg        DS_Spediz
125900040205     c                   seton                                        01
126000040205     c                   setoff                                       0203
126100040205     C                   z-add     DSspe_aas     V1anns
126200040205     C                   z-add     DSspe_lnp     V1lnps
126300040205     C                   z-add     DSspe_nrs     V1nrss
126400040205     C                   z-add     DSspe_nsp     V1nsps
126500140204      * verifico se spedizione in consegna
126600140204     c                   exsr      Ctr_specons
126700131206
126800040211    3C                   endif
126900040212     c                   eval      *in90 = *on
127000150324     c                   if        wMod = 'I'
127100150114     c                   exsr      sr_ImpAut_Ins
127200150112     c                   else
127300040219     c                   eval      *in35 = *on
127400150112     c                   endif
127500040205     C                   LEAVE
127600040211    2C                   endif
127700040205      *
127800040205
127900040205      * F09 ORM  Interrogazione e ricerca
128000040205      *
128100040211    2C                   IF        *INKI
128200040205      *
128300040223      * verifico se oggetto già inserito emetto un messaggio di errore forzabile
128400040223     c                   if        ( (v1ctor = 'G' and v1cogg <>
128500040223     c                             *blank) OR (v1ctor = 'S' and spevid > *zeros)
128600040223     c                             OR (v1ctor = 'O' and ormvid > *zeros) )
128700040223     c                   exsr      SR_GESWIN2
128800040223      * ritorno alla videata senza interrogare
128900040223     c                   If        *inKL
129000040223     c                   eval      *in90 = *on
129100150324     c                   if        wMod = 'I'
129200150114     c                   eval      *in48 = *on
129300150114     c                   else
129400040223     c                   eval      *in31 = *on
129500150114     c                   endif
129600040223     c                   leave
129700040223     c                   endif
129800040223      *
129900040223     c                   endif
130000040223      *
130100150701     c                   clear                   FIOR010DS
130200150701     c                   eval      IOR010ric = '1'
130300040205      * preparo l'altro parametro da passare
130400040205     c                   clear                   fidna1ds
130500040421     c                   eval      IA1ant = v1cant
130600040205     c                   eval      IA1nut = v1cnut
130700040205     c                   eval      IA1pdt = v1cpdt
130800040205      *
130900150701     c                   CALL      'FIOR010R'
131000040205     C                   PARM                    KPJBA
131100150701     C                   PARM                    FIOR010DS
131200040205     C                   PARM                    FIDNA1DS
131300040205      * verifico se ritorna il flag di inserimento uguale a S imposto la chiave dell'oggetto
131400040211    3c                   IF        IA1INS = 'S'
131500040217     c                   eval      v1ctor = 'O'
131600040205     c                   movel     IA1ogg        DS_ORM
131700040205     c                   seton                                        02
131800040205     c                   setoff                                       0103
131900040205     C                   z-add     DSorm_poe     V1poeo
132000040217     C                   z-add     DSorm_nsr     V1nsor
132100040205     C                   z-add     DSorm_nor     V1norm
132200040205     C                   z-add     DSorm_nrv     V1nrvo
132300040211    3C                   endif
132400040212     c                   eval      *in90 = *on
132500150324     c                   if        wMod = 'I'
132600150114     c                   exsr      sr_ImpAut_Ins
132700150112     c                   else
132800040219     c                   eval      *in35 = *on
132900150112     c                   endif
133000040205     C                   LEAVE
133100040211    2C                   endif
133200040211      *
133300040211      * controllo la correttezza dei campi inseriti
133400040211      *
133500040218
133600040218      * se dato enter  e tutti i campi del  control (solo la parte relativa all'oggetto della
133700040218      * chiamato) non sono valorizzati e nel subfile c'è almeno un record controllo se è stata
133800040218      * richiesta la modifica di una riga già esistente
133900040218
134000040220    2c                   if        s01nrr > 0  and v1ctor = *blanks and
134100040218     c                             v1cogg = *blanks and ormvid = *zeros and
134200040218     c                             spevid = *zeros and v1ctch = *blanks and
134300040218     c                             v1cmad = *blanks and v1ccdt = *blanks and
134400040218     c                             v1not1 = *blanks and v1not2 = *blanks and
134500040218     c                             v1not3 = *blanks and v1not4 = *blanks and
134600150313     c*****                        v1crpg = *blanks and V1Cpog = *blanks and
134700040218     c                             v1crsp = *blanks and v1cpor = *blanks
134800040218
134900070326      * se dato F4 vado a fine altrimenti controllo se è stata fatta richiesta di gestione
135000040219      * riga di subfile
135100040220    3c                   if        *inkd
135200040219      * imposto un campo che mi dice di non eseguire la routine di conferma riga
135300040219     c                   eval      W_confe = 'N'
135400040219
135500040219     c                   leave
135600040219      *
135700040219     c                   else
135800040218     c                   exsr      SR_gessbf
135900040218
136000040218      * se scelto un record da manutenzionare esco dal ciclo per riemettere la videata con
136100040218      * i dati da manutenzionare altrimenti proseguo nei controlli
136200040220    4c                   If        Wrk_recsbf = 'Y'
136300040218     c                   eval      *in90 = *on
136400040422     C                   eval      *IN37 = *on
136500040422     C                   eval      *IN72 = *off
136600040218     c                   leave
136700040220    4c                   endif
136800040218
136900040220    3c                   endif
137000040219
137100040220    2c                   endif
137200040422      *
137300040615      * Non eseguo i controlli se sono in manutenzione e non ho selezionato nulla
137400040422      *
137500040422    2c                   If        *in72
137600040422     c                   leave
137700040422    2c                   endif
137800150112      *
137900040217      * verifico se inserito tipo oggetto
138000040217      *
138100040217    2c                   If        v1cTOR = *blank
138200040217     C                   eval      *IN28 = *on
138300040217     c                   movel     msg(13)       V1Cmsg
138400040217     C                   eval      *IN31 = *on
138500040217     c                   leave
138600040220      *
138700040220     c                   else
138800040220      * Ricerca
138900040220     c     '?'           scan      v1ctor
139000040220    3c                   if        %found
139100040220     c                   clear                   tibs02ds
139200040220     C                   MOVEL     'R'           t02mod
139300040220     C                   MOVEL     knsif         t02sif
139400040220     C                   MOVEL     'OGR'         t02cod
139500040220      *
139600040220     C                   CALL      'TIBS02R'
139700040220     C                   PARM                    KPJBA
139800040220     C                   PARM                    TIBS02DS
139900040220      *
140000040220    4C                   IF        T02err = *BLANKS
140100040220     C                   movel     T02uni        DOGR
140200040220     C                   movel     T02ke1        v1ctor
140300040220      *  errore
140400040220     C                   ELSE
140500040220     c                   clear                   v1ctor
140600040223     C                   eval      *IN31 = *on
140700040223     c                   eval      *in90 = *on
140800040223     c                   leave
140900040220    4C                   ENDIF
141000040220      *
141100040220      *
141200040220   x3C                   ELSE
141300040220      *
141400040220      * Controllo
141500040220     c                   clear                   tibs02ds
141600040220     C                   MOVEL     'C'           t02mod
141700040220     C                   MOVEL     knsif         t02sif
141800040220     C                   MOVEL     'OGR'         t02cod
141900040220     C                   MOVEL     v1ctor        t02ke1
142000040220      *
142100040220     C                   CALL      'TIBS02R'
142200040220     C                   PARM                    KPJBA
142300040220     C                   PARM                    TIBS02DS
142400040220      *
142500040220    4C                   IF        t02err = *BLANKS
142600040223     C                   movel     T02uni        DOGR
142700040220     C                   movel     T02ke1        v1ctor
142800040220      *
142900040220     C                   ELSE
143000040220     C                   eval      *IN28 = *on
143100040220     c                   movel     msg(13)       V1Cmsg
143200040220     C                   eval      *IN31 = *on
143300040220     c                   leave
143400070327    4C                   ENDIF
143500040220      *
143600040220    3c                   endif
143700040220      *
143800040223     c                   movea     §ogrccha      ccr
143900040217    2c                   endif
144000150112      *
144100040211      * tipo oggetto
144200040211      *               generico e indicatore 03 spento riemetto la videata
144300040211      *
144400040217    2c                   If        v1ctor = 'G' and not *in03
144500040211     c                   setoff                                       0102
144600040217     c                   seton                                        0332
144700040223     c                   clear                   spevid
144800040223     c                   clear                   ormvid
144900040212     c                   eval      *in90 = *on
145000040211     c                   leave
145100040211    2c                   endif
145200040211      *               spedizione e indicatore 01 spento riemetto la videata
145300040217    2c                   If        v1ctor = 'S' and not *in01
145400040211     c                   setoff                                       0203
145500040217     c                   seton                                        0133
145600040223     c                   clear                   v1cogg
145700040223     c                   clear                   ormvid
145800040212     c                   eval      *in90 = *on
145900040211     c                   leave
146000040211    2c                   endif
146100040211      *               spedizione e indicatore 02 spento riemetto la videata
146200040217    2c                   If        v1ctor = 'O' and not *in02
146300040211     c                   setoff                                       0103
146400040217     c                   seton                                        0234
146500040223     c                   clear                   v1cogg
146600040223     c                   clear                   spevid
146700070412      * pulisco anche i campi per la gestione della data consegna
146800141029     c                   setoff                                       616264
146900070412     c                   eval      $ric_fnlrg2r = *off
147000070416     c                   eval      $fnarb       = *off
147100070412
147200040212     c                   eval      *in90 = *on
147300040211     c                   leave
147400040211    2c                   endif
147500040211      *
147600040707      * verifico se inserito oggetto
147700040211      *
147800040211      * generico
147900040211    2c                   If        *in03 and v1cogg = *blank
148000040211     C                   eval      *IN28 = *on
148100040211     c                   movel     msg(2)        V1Cmsg
148200040217     C                   eval      *IN32 = *on
148300040211     c                   leave
148400040211    2c                   endif
148500040211      * spedizione
148600040211    2c                   If        *in01 and (v1lnps = *zeros or v1nsps = *zeros
148700040211     c                             or v1anns = *zeros)
148800040211     C                   eval      *IN28 = *on
148900040211     c                   movel     msg(3)        V1Cmsg
149000040211     C                   eval      *IN33 = *on
149100040211     c                   leave
149200040211    2c                   endif
149300040217      *
149400040217      * verifico se inserito anno di 2
149500040217      *
149600040217    2c                   If        *in01 and  v1anns > 60 and v1anns < 100
149700040217     c                   add       1900          V1anns
149800040217    2c                   endif
149900040217
150000040217    2c                   If        *in01 and  v1anns <=  60 and v1anns < 100
150100040217     c                   add       2000          V1anns
150200040217    2c                   endif
150300040217
150400040211      * ORM
150500040211    2c                   If        *in02 and (v1poeo=*zeros or v1norm=*zeros)
150600040211     C                   eval      *IN28 = *on
150700040211     c                   movel     msg(5)        V1Cmsg
150800040211     C                   eval      *IN34 = *on
150900040211     c                   leave
151000040211    2c                   endif
151100040211      *
151200040211      * verifico se oggetto inserito esiste
151300040211      *
151400040211      * spedizione
151500040211    2c                   If        *in01 and Wspevid <> spevid
151600070625     c                   eval      *in09 = *off
151700041005      * se non sono in sede
151800041005   a2c                   if        not *in05
151900040211      * verifico se esiste nelle bolle arrivi
152000040211      *
152100040211     c     KSPED         chain     Fnarb01l
152200040211    3c                   If        not %found(Fnarb01l)
152300070416     c                   eval      $fnarb = *off
152400040211      * verifico se esiste nelle bolle partenze
152500040211      *
152600040211     c     KSPED         chain     Fnblp01l
152700040211    4c                   If        not %found(Fnblp01l)
152800040707      * errore solo se non sono in manutenzione
152900040707    5c                   If        not *in50
153000040211     C                   eval      *IN28 = *on
153100040211     c                   movel     msg(4)        V1Cmsg
153200040211     C                   eval      *IN33 = *on
153300040211     c                   leave
153400040707      * accendo indicatore di non visualizzazine oggetto con F2
153500040707     c                   else
153600040707     c                   seton                                        80
153700040707    5c                   endif
153800070625      * se trovata in bolla partenza
153900070625     c                   else
154000070625      * verifico se si tratta di bolla ancora da borderizzare
154100070625     c                   eval      *in09 = (blpft2 = ' ' )
154200041209
154300041206    4c                   endif
154400070416      *
154500070416     c                   else
154600070416      * se trovato in arb
154700070416     c                   eval      $fnarb = *on
154800140204      * controllo e ripristino i campi della spedizione in consegna
154900140204     c                   exsr      CTR_specons
155000041206    3c                   endif
155100041005      *
155200041005      * sono in SEDE
155300041005      *
155400041005  xa2c                   else
155500041005     c     KSPED         chain     Titas30c
155600041005    3c                   If        not %found(Titas30c)
155700041005      * errore solo se non sono in manutenzione
155800041005    4c                   If        not *in50
155900041005     C                   eval      *IN28 = *on
156000041005     c                   movel     msg(4)        V1Cmsg
156100041005     C                   eval      *IN33 = *on
156200041005     c                   leave
156300130513      * accendo indicatore di non visualizzazione oggetto con F2
156400041005     c                   else
156500041005     c                   seton                                        80
156600041005    4c                   endif
156700041005
156800041005    3c                   endif
156900041005      *
157000041005   a2c                   endif
157100040211      * mi salvo il numero spedizione
157200040211     c                   z-add     spevid        Wspevid
157300150114      * ...e pulisco quello dell'ORM
157400150114     c                   clear                   WormVid
157500070412      * pulisco anche i campi per la gestione della data consegna
157600141029     c                   setoff                                       616264
157700070412     c                   eval      $ric_fnlrg2r = *off
157800150114      *
157900150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
158000150324     c                   if        wMod = 'I'
158100150114     c                   exsr      sr_ImpAut_Ins
158200150114     c                   endif
158300070412
158400040211    2c                   endif
158500040211      *
158600040211      * ORM
158700040211    2c                   If        *in02 and Wormvid <> ormvid
158800040211      * verifico se esiste in archivio
158900040211      *
159000040211     c     KORM          chain     Fnorm01l
159100040211    3c                   If        not %found(Fnorm01l)
159200040211     c
159300040707      * errore solo se non sono in manutenzione
159400040707    4c                   If        not *in50
159500040211     C                   eval      *IN28 = *on
159600040211     c                   movel     msg(6)        V1Cmsg
159700040211     C                   eval      *IN34 = *on
159800040211     c                   leave
159900040707     c                   else
160000040707      * accendo indicatore di non visualizzazine oggetto con F2
160100040707     c                   seton                                        80
160200040707    4c                   endif
160300041209
160400040211    3c                   endif
160500040211      * mi salvo il numero ORM
160600040211     c                   z-add     ormvid        Wormvid
160700150114      * ...e pulisco quello della spedizione
160800150114     c                   clear                   WspeVid
160900150114      *
161000150114      *?Automatismi per fase di INSERIMENTO (richiesti da Rossana)?
161100150324     c                   if        wMod = 'I'
161200150114     c                   exsr      sr_ImpAut_Ins
161300150114     c                   endif
161400150114      *
161500040211    2c                   endif
161600040217
161700040317      * F2   Interrogazione oggetto
161800040217      *
161900040707    2c                   IF        *inKB
162000040707      * se c'è 80 acceso significa che manca l'oggetto e quindi emetto l'errore
162100040707     c                   if        *in80
162200040707     C                   eval      *IN28 = *on
162300040707     c                   movel     msg(31)       V1Cmsg
162400040707     C                   eval      *IN31 = *on
162500040707     c                   leave
162600040707     c                   endif
162700040217      * verifico se tipo oggetto inserito
162800040217    3c                   if        v1ctor <> 'S' and v1ctor <> 'O'
162900040217     C                   eval      *IN28 = *on
163000040315     c                   movel     msg(22)       V1Cmsg
163100040217     C                   eval      *IN31 = *on
163200040217     c                   leave
163300040217    3c                   endif
163400070430      * richiamo la routine di interrogazione oggetto
163500070430     c                   exsr      SR_gesf2
163600070430      * se oggetto trovato riemetto la  videata
163700070430     c                   if        *in90
163800070430     c                   leave
163900070430     c                   endif
164000040217    2c                   endif
164100040531      *
164200040531      * se sono in immissione verifico se oggetto presente in altri reclami
164300041007      * se non è un oggetto generico
164400040604      * se modificato oggetto
164500041007    2c                   If        not *in03
164600041007      *
164700041007    3c                   If        forz_rec = 'Y'
164800040604
164900041007    4c                   if        *in01
165000041209     c                   movel(p)  spevid        SW_v1cogg
165100041007    4c                   endif
165200040604
165300041007    4c                   if        *in02
165400041209     c                   movel(p)  ormvid        SW_v1cogg
165500041007    4c                   endif
165600040604
165700041007    4c                   If        SW_v1cogg <> W_v1cogg
165800040604     c                   clear                   forz_rec
165900041007    4c                   endif
166000040604
166100041007    3c                   endif
166200040604
166300041007    3c                   If        not *in50 and WRK_recsbf <> 'Y' and
166400040531     c                             forz_rec = ' '
166500040531
166600041007    4c                   if        *in01
166700041209     c                   movel(p)  spevid        W_v1cogg
166800041007    4c                   endif
166900040531
167000041007    4c                   if        *in02
167100041209     c                   movel(p)  ormvid        W_v1cogg
167200041007    4c                   endif
167300040531
167400150325      * verfiico in FITGD32L
167500040531
167600150325     c     Ktgd32        chain     fitgd32l
167700150325    4c                   if        %found(fitgd32l)
167800040531     c                   eval      *in28 = *on
167900040531     c                   movel     msg(30)       V1Cmsg
168000040531     C                   eval      *IN31 = *on
168100040531     C                   eval      forz_rec = 'Y'
168200040531     c                   leave
168300041007    4c                   endif
168400040531
168500041007    3c                   endif
168600041007
168700041007    2c                   endif
168800040211      *
168900040211      * verifico l'inserimento del CHI CHIAMA
169000040220      *
169100040220     c                   clear                   v1dtch
169200040220      *
169300040220    2c                   If        v1cTch = *blank
169400040220     C                   eval      *IN28 = *on
169500040220     c                   movel     msg(07)       V1Cmsg
169600040220     C                   eval      *IN35 = *on
169700040220     c                   leave
169800040220      *
169900040220     c                   else
170000040220      * Ricerca
170100040220     c     '?'           scan      v1ctch
170200040220    3c                   if        %found
170300040220     c                   clear                   tibs02ds
170400040220     C                   MOVEL     'R'           t02mod
170500040220     C                   MOVEL     knsif         t02sif
170600040220     C                   MOVEL     'CCR'         t02cod
170700040220      *
170800040220     C                   CALL      'TIBS02R'
170900040220     C                   PARM                    KPJBA
171000040220     C                   PARM                    TIBS02DS
171100040220      *
171200040220    4C                   IF        T02err = *BLANKS
171300040220     C                   movel     T02uni        DCCR
171400040220     C                   movel     T02ke1        v1ctch
171500040220     c                   movel     §ccrdesc      v1dtch
171600040220      *  errore
171700040220     C                   ELSE
171800040220     c                   clear                   v1ctch
171900040220     c                   clear                   v1dtch
172000040220    4C                   ENDIF
172100040220      *
172200040220     C                   eval      *IN35 = *on
172300040220     c                   eval      *in90 = *on
172400040220     c                   leave
172500040220      *
172600040220   x3C                   ELSE
172700040220      *
172800040220      * Controllo
172900150112     c                   exsr      sr_Ctrl_CCR
173000150112     c                   if        *in28
173100150112     c                   leave
173200150112     c                   endif
173300040220      *
173400040220    3c                   endif
173500040220      *
173600040220    2c                   endif
173700040220      * verifico se chi chiama è ammesso per quel tipo oggetto
173800040220
173900150421    2c*//  v1ctch        lookup    ccr(1)                                 55
174000150421     c*//                if        not %found
174100150421    2c                   if        %lookup( V1Ctch : CCR ) = *zero
174200040220      * errore chi chiama non ammesso
174300040211     C                   eval      *IN28 = *on
174400040220     c                   movel     msg(16)       V1Cmsg
174500040211     C                   eval      *IN35 = *on
174600040211     c                   leave
174700040211    2c                   endif
174800040211      *
174900040211      * verifico l'inserimento del MOTIVO del reclamo
175000070412
175100070412      * se modificato rispetto al precedente pulisco alcuni campi
175200070412     c                   If        v1cmad <> W_cmad
175300070412     c                   eval      $ric_fnlrg2r = *off
175400141029     c                   setoff                                       616264
175500070412     c                   eval      W_cmad = v1cmad
175600141030     c                   clear                   v1ctcr
175700141030     c                   clear                   v1cdcr
175800141030     c                   clear                   v1cocr
175900070412     c                   endif
176000070412
176100150421      * - Ricerca / Controllo
176200150421     c                   eval      wwcMAD = V1Cmad
176300150421     c                   exsr      sr_CtrlMAD
176400150421     c                   eval      V1Cmad = wwcMAD
176500150422     c                   eval      V1Dmad = wwdMAD
176600150421     c                   if        *in28  or  *in90
176700150421     c                   leave
176800150421     c                   endif
176900140212      *
177000140212      * F17=Note LDV
177100140212      *
177200140217     c                   clear                   Not_LdV
177300140217     c                   if        Wmod = 'I'  and  V1Cmad = W_Lav
177400140217     c                                         and  $Cons  = *on
177500140217     c                   eval      Not_LdV = 'S'
177600140217     c                   endif
177700140217      *
177800140212if  2c                   If        *inKR
177900140217      * - SE "Immissione" & "Obbligo Immissione Note LdV"   oppure
178000140217      *   SE "Immissione" & "Motivo" = "  2" & Spedizione in Consegna
178100140217      *   => Note all'F6
178200140217     c                   if        (Wmod = 'I'  and  §MARnotLDV = 'S')  or
178300140217     c                             (Wmod = 'I'  and  V1Cmad = W_Lav
178400140217     c                                          and  $Cons  = *on)
178500140212     C                   eval      *in28 = *on
178600140212     c                   movel     Msg(40)       V1Cmsg
178700140212     c                   leave
178800140212     c                   endif
178900140212     c                   exsr      SR_F17
179000140212     c                   leave
179100140212e   2c                   endif
179200131119      *
179300070322      *  verifico se la causale mi invita all'inserimento della data consegna richiesta
179400070326      *  se non sono in modifica , non sono in sede e sono abilitato a GEO
179500070416      *  e se non ho già richiamato il pgm dei blocchi e se sono un oggetto spedizione  presente
179600070416      *  in arrivo
179700070330    2c                   if        §marrdco = 'S'  and not *in61
179800070402     c                             and not *in74  and dutlpo <> 'S'
179900070403     c                             and $abilitato = *on and $ric_fnlrg2r = *off
180000070416     c                             and v1ctor = 'S' and $fnarb = *on
180100070322      * richiamo la routine gestione data consegna
180200070322     c                   exsr      SR_GES_DTA
180300070329    3c                   if        $riemetti = *on
180400070327     c                   seton                                        90
180500070327     c                   leave
180600070329    3c                   endif
180700070329    2c                   endif
180800070329      * Controllo tipo a data consegna richiesta
180900070403    2c                   if        v1ctcr<> *blanks and *in61 and
181000141030     c                             v1cdcr = 0 and v1cocr = 0
181100070403     c                   eval      *IN28 = *on
181200070403     c                   movel     msg(11)       V1Cmsg
181300070403     c                   eval      *IN62 = *on
181400070403     c                   leave
181500070403    3c                   endif
181600141030      * Controllo ora consegna richiesta presente senza data
181700141030    2c                   if        *in61 and  v1cdcr = 0
181800141030     c                             and v1cocr > 0
181900141030     c                   eval      *IN28 = *on
182000141030     c                   movel     msg(11)       V1Cmsg
182100141030     c                   eval      *IN62 = *on
182200141030     c                   leave
182300141030    3c                   endif
182400070329    2c                   if        v1cdcr>0 and *in61
182500070329     c* controllo formale data
182600070329     c                   clear                   wlbdat
182700070329     c                   z-add     v1cdcr        g02dat
182800070329     c                   clear                   g02err
182900070329     c                   call      'XSRDA8'
183000070329     c                   parm                    wlbdat
183100070329      * data formalmente errata
183200070329    3c                   if        g02err = '1'
183300070329     c                   eval      *IN28 = *on
183400070329     c                   movel     msg(11)       V1Cmsg
183500070329     c                   eval      *IN62 = *on
183600070329     c                   leave
183700070329    3c                   endif
183800141029      * ora formalmente errata
183900141029     C                   if        v1cocr <> *zeros
184000141030     C                   movel     v1cocr        ora
184100141029     C                   move      v1cocr        minuti
184200141029
184300141030     C                   if        ora > 24 or minuti > 59
184400141030     C                             or ora < 0
184500141029     c                   eval      *IN28 = *on
184600141029     c                   movel     msg(44)       V1Cmsg
184700141029     c                   eval      *IN64 = *on
184800141029     c                   leave
184900141029     C                   endif
185000141030
185100141030     C                   endif
185200070329      * richiamo il controllo della data da FNLR48R
185300070329     c                   z-add     g02inv        Wdcr
185400070329     c                   z-add     g02dat        v1cdcr
185500070329     c                   clear                   fnlr48ds
185600070329     c                   eval      d48cvb='CR'
185700070329     c                   eval      d48ffr='F'
185800070329     C                   z-add     v1anns        D48AAS
185900070329     C                   z-add     v1lnps        D48LNP
186000070329     C                   z-add     v1nrss        D48NRS
186100070329     c                   z-add     v1nsps        D48NSP
186200070329     c                   movel     ARBCBO        D48CBO
186300070329     c                   movel     'A'           D48TBO
186400070329     c                   movel     'G'           D48trc
186500070329     c                   movel     fnlr48ds      KPJBU
186600140429     c                   clear                   dsARBd
186700070329     c                   movel     v1ctcr        §BGTCR
186800141029     c                   z-add     v1cocr        §BGHCR
186900070329     c                   z-add     wdcr          §BGDCR
187000070329     c                   movel     arbgc1        §BGGC1
187100070329     c                   movel     arbgc2        §BGGC2
187200070329     c                   movel     arbtc1        §BGTC1
187300070329     c                   movel     arbtc2        §BGTC2
187400070329     c                   call      'FNLR48R'
187500070329     c                   parm                    KPJBA
187600070329     c                   parm                    DSARBD
187700070329     c                   parm                    DSARBK
187800070329     c                   parm                    DARBT
187900070329     c                   parm                    DSARBG
188000070329     c                   parm                    trul90ds
188100070329     c                   movel     Kpjbu         fnlr48ds
188200070329      * ritorno errore da controllo fnlr48r
188300070402    3c                   if        d48err<>*blanks
188400070329     c                   movel     d48msg        v1cmsg
188500070329     c                   eval      *IN28 = *on
188600070329     c                   eval      *IN62 = *on
188700070329     c                   leave
188800070329    3c                   endif
188900070329    2c                   endif
189000120919
189100120919      * se il motivo di apertura RA prevede l'immissione delle note le richiedo
189200120919      * obbligatorie
189300120919     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
189400131017     c                             V1not3 = *blanks and V1not4 = *blanks and
189500120919     c                             §MARnote = 'S'
189600120919     c                   eval      *in28 = *on
189700120919     c                   eval      *in48 = *on
189800120919     c                   eval      V1Cmsg = msg(38)
189900120919     c                   leave
190000120919     c                   ENDIF
190100070329
190200120913      * se il motivo di apertura RA prevede una chiusura automatica
190300120913      * imposto il motivo di chiusura se non già impostato
190400120913     c                   IF        V1Ccdt = *blanks and §MARmotch <> *blanks
190500120913     c                   eval      V1Ccdt = §MARmotch
190600120913     c                   ENDIF
190700040212      *
190800040212      * verifico l'inserimento del CODICE di CHIUSURA del reclamo
190900040220      *
191000040220     c                   clear                   v1dcdt
191100040420     c                   clear                   v1crsp
191200040212    2c                   If        v1ccdt <> *blanks
191300120913
191400120913      * se il motivo di chiusura messo dell'utente è diverso da quello
191500150720      * previsto in tabella MAR errore  se l'utente non è abilitato
191600130705      * alla modifica
191700130705     c                   If        not *in07
191800120913     c                   IF        V1Ccdt <> §MARmotch and §MARmotch <> *blanks
191900120913     c                   eval      *in28 = *on
192000120913     c                   eval      *in37 = *on
192100120913     c                   eval      V1Cmsg = MSG(37)
192200131119     c                   eval      V1Cmsg = %trim(V1Cmsg) + ' "' +
192300120913     c                             %trim(§MARmotch) + '"'
192400120913     c                   leavesr
192500120913     c                   ENDIF
192600130705     c                   Endif
192700150720
192800150720      * -?Se R.A. con Motivo Apertura " 70": la Sede NON può chiuderla?
192900150720     c                   If        *in05  and  wwcMAD = c_ReclSede
193000150720     c                   eval      *in28 = *on
193100150720     c                   eval      *in37 = *on
193200150720     c                   eval      V1Cmsg = Msg(52)
193300150720     c                   leavesr
193400150720     c                   EndIf
193500120913
193600040212     c                   clear                   tibs02ds
193700040212      *
193800040212      * Ricerca
193900040212     c     '?'           scan      v1ccdt
194000040212    3c                   if        %found
194100040217     c                   clear                   tibs02ds
194200040212     C                   MOVEL     'R'           t02mod
194300040212     C                   MOVEL     knsif         t02sif
194400040212     C                   MOVEL     'CHR'         t02cod
194500040212      *
194600040212     C                   CALL      'TIBS02R'
194700040212     C                   PARM                    KPJBA
194800040212     C                   PARM                    TIBS02DS
194900040212      *
195000040212    4C                   IF        T02err = *BLANKS
195100040212     C                   movel     T02uni        DCHR
195200040212     C                   movel     T02ke1        v1ccdt
195300040212     C                   movel     §chrdesc      v1dcdt
195400040420     C                   movel     §chrresp      v1crsp
195500040223      *  errore (non è stato selezionato nulla pulisco  il campo e mi posiziono)
195600040212     C                   ELSE
195700040223     C                   clear                   v1ccdt
195800040223     C                   clear                   v1dcdt
195900040420     C                   clear                   v1crsp
196000040212    4C                   ENDIF
196100040212      *
196200040212     c                   eval      *in90 = *on
196300040219     C                   eval      *IN37 = *on
196400040212     c                   leave
196500040212      *
196600040212   x3C                   ELSE
196700040212      *
196800040212      * Controllo
196900040217     c                   clear                   tibs02ds
197000040212     C                   MOVEL     'C'           t02mod
197100040212     C                   MOVEL     knsif         t02sif
197200040212     C                   MOVEL     'CHR'         t02cod
197300040212     C                   MOVEL     v1ccdt        t02ke1
197400040212      *
197500040212     C                   CALL      'TIBS02R'
197600040212     C                   PARM                    KPJBA
197700040212     C                   PARM                    TIBS02DS
197800040212      *
197900040212    4C                   IF        t02err = *BLANKS
198000040212     C                   movel     T02uni        DCHR
198100040212     C                   movel     §CHRdesc      v1dcdt
198200040420     C                   movel     §chrresp      v1crsp
198300040212      *
198400040212     C                   ELSE
198500040212     c                   eval      *in28 = *on
198600040223     c                   movel     MSG(19)       V1Cmsg
198700040212     C                   eval      *IN37 = *on
198800040212     c                   leave
198900040212    4C                   ENDIF
199000040212      *
199100040212    3c                   endif
199200040212      *
199300040212    2C                   endif
199400150313
199500150624      *?Impostazione STATO (ex Priorità):?
199600150624      * Se c'è la causale chiusura metto priorità '9' (in inserimento)
199700150624      *   o '8' (in modifica - se modificata la causale Chiusura)
199800150313     c                   IF        V1Ccdt <> *blanks
199900150624      *
200000150324     c                   IF        wmod = 'I'
200100150624      * -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
200200150624      *  ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
200300150625      *  ?(già reperita nella subr. SR_CONTR01)?
200400150624     c                   if        §CORstato <> *blanks
200500150624     c                   eval      V1Cprt = §CORstato
200600150624     c                   else
200700150313     c                   eval      V1Cprt = '9'
200800150624     c                   endif
200900150324     c                   ELSE
201000150622     c                   If        V1ccdt <> tgdcdt
201100150324     c                   eval      V1Cprt = '8'
201200150622     c                   endif
201300150324     c                   ENDIF
201400150624      *
201500150624      * Altrimenti (se NON c'è la causale di chiusura) metto
201600150624      *   priorita '0' (in inserimento) o '1' (in modifica)
201700150313     c                   ELSE
201800150624      *
201900150421     c                   If        wmod = 'I'
202000150421     c                   eval      V1Cprt = '0'
202100150421     c                   Else
202200150313     c                   eval      V1Cprt = '1'
202300150421     c                   EndIf
202400150624      *
202500150313     c                   ENDIF
202600150320      *
202700150320      * Decodifico (potrebbe essere cambiato)
202800150320     c                   clear                   tibs02ds
202900150320     C                   MOVEL     'C'           t02mod
203000150320     C                   MOVEL     knsif         t02sif
203100150320     C                   MOVEL     'PRR'         t02cod
203200150320     C                   MOVEL     v1cprt        t02ke1
203300150320      *
203400150320     C                   CALL      'TIBS02R'
203500150320     C                   PARM                    KPJBA
203600150320     C                   PARM                    TIBS02DS
203700150320      *
203800150320    4C                   IF        t02err = *BLANKS
203900150320     C                   movel     T02uni        DPRR
204000150320     C                   movel     §PRRdesc      v1dprt
204100150320     c                   endif
204200150324
204300150324      * se RA aperta richiedo note obbligatorie
204400150324     c                   IF        V1not1 = *blanks and V1not2 = *blanks and
204500150324     c                             V1not3 = *blanks and V1not4 = *blanks and
204600150324     c                             V1Ccdt = *blanks
204700150407     c                             and not *inks
204800150324     c                   eval      *in28 = *on
204900150324     c                   eval      *in48 = *on
205000150324     c                   eval      V1Cmsg = msg(38)
205100150324     c                   leave
205200150324     c                   ENDIF
205300040212      *
205400040212      * F18 - GESTIONE NOTE
205500040212      *
205600040212    2C                   IF        *INKS
205700040212      *
205800040212     c                   exsr      Sr_f18
205900040212      *
206000040219     c                   seton                                        39
206100040212     c                   eval      *in90 = *on
206200040212     c                   leave
206300040212      *
206400040212    2c                   ENDIF
206500150313
206600150313      * mi salvo l'indicatore 48 per posizionamento su campo note
206700150313     c                   eval      sav_in48 = *in48
206800150313      * spengo il 48
206900150313     c                   setoff                                       48
207000150311
207100150420      * Controllo la Filiale Gestione (SE gestita)
207200150428     c*//                IF        Not *in49
207300150428     c                   IF        wMod <> 'I'
207400150420      *
207500150421      * - Ricerca / Controllo
207600150421     c                   eval      wwcPOG = V1Cpog
207700150717     c*//?già impostata? eval      wwcMAD = V1Cmad
207800150421     c                   exsr      sr_CtrlPOG
207900150421     c                   eval      V1Cpog = wwcPOG
208000150421     c                   if        *in28  or  *in90
208100150421     c                   leave
208200150421     c                   endif
208300150420      *
208400150420     c                   ENDIF
208500040212      *
208600040323      * verifico l'inserimento dell'UTENTE che ha in GESTIONE il reclamo
208700150421      * (SE gestito)
208800150622     c                   clear                   $Reparto
208900150622      *
209000150428   1bc*//                IF        Not *in49
209100150428     c                   IF        wMod <> 'I'
209200150423      *
209300150422    2c                   If        V1Crpg <> *blanks
209400150421      *
209500150428
209600150428      /free
209700150429
209800150429             // -?Ricerca/Controllo Reparto Gestione?
209900150617             //  ?(NON della Sede: per Sede si possono inserire solo Utenti)?
210000150617             If  V1Cpog <> c_Sede;
210100150617
210200150617               wwcRPR = V1Crpg;
210300150617               exsr  sr_CtrlRPR;
210400150617               V1Crpg = wwcRPR;
210500150617               //V1Drpg = wwdRPR;
210600150617
210700150617               if  *in90;
210800150617                 leave;
210900150617               endif;
211000150617
211100150617               $Reparto = ( Not *in28 );
211200150617
211300150617               *in28 = *off;
211400150617               *in39 = *off;
211500150617               *in45 = *off;
211600150617               clear  V1Cmsg;
211700150617
211800150617             EndIf;
211900150429
212000150429      /end-free
212100150421      *
212200150429if  3c                   IF        Not $Reparto
212300150421      *
212400150421      * 2° controllo come Utente Gestione
212500150421      *
212600040420      * verifico se esiste utente nell'archivio profili
212700040420     c     v1crpg        chain     azute01l
212800040420      *
212900150428    5c                   if        %found(azute01l) and UTEdsc > DateU
213000150311      * se lo trovo verifico se è della filiale di gestione
213100150723    6c                   if        %editc(UTEfil:'X') <> V1Cpog
213200040420     c                   eval      *in28 = *on
213300150313     c                   eval      *in46 = *on
213400040420     c                   movel     MSG(23)       V1Cmsg
213500040420     c                   leave
213600040420    6c                   endif
213700040420
213800150423    5c                   else
213900040420
214000040420     c                   eval      *in28 = *on
214100040420     c                   eval      *in39 = *on
214200150429     c*//                movel     MSG(28)       V1Cmsg
214300150617     c                   if        V1Cpog <> c_Sede
214400150429     c                   movel     Msg(20)       V1Cmsg
214500150617     c                   else
214600150617     c                   eval      V1Cmsg = %trimR( Msg(20) )
214700150617     c                                    + ': ammessi solo utenti di sede'
214800150617     c                   endif
214900040420     c                   leave
215000040420    5c                   endif
215100040420
215200150421      *
215300150421e   3c                   ENDIF
215400040420
215500150422   x2c                   Else
215600150421      * se Reparto/Utente a blanks do errore
215700040422     c                   eval      *in28 = *on
215800040422     c                   eval      *in39 = *on
215900040422     c                   movel     MSG(29)       V1Cmsg
216000040422     c                   leave
216100150422    2C                   EndIf
216200150422      *
216300150422 e_1bc                   ENDIF
216400150313
216500150313      * se arrivo qua vuol dire che non ho avuto errori sui campi
216600150313      * filiale e utente gestione
216700150313      * quindi ripristino l'indicatore 48
216800150313     c                   eval      *in48 = sav_in48
216900150313
217000040527      *
217100040527      * se responsabilità vettore  e P.O. responsabile non valorizzato metto me stesso
217200040527      * se responsabilità no vettore  e P.O. responsabile deve essere =  a blank
217300040527      *
217400040527    2c                   if        v1crsp = 'V'  and (v1cpor = *blanks
217500040527     c                             or v1cpor = '000')
217600130513     c                   select
217700130513     c                   when      §MARffr = 'A'
217800130513     c                   select
217900130513     c                   when      *in05
218000130513     c                   move      TASlna        V1Cpor
218100130513     c                   when      $fnARB = *on
218200130513     c                   move      ARBlna        V1Cpor
218300130513     c                   other
218400130513     c                   move      BLPlna        V1Cpor
218500130513     c                   endsl
218600130513     c                   when      §MARffr = 'P'
218700130513     c                   select
218800130513     c                   when      *in05
218900130513     c                   move      TASlnp        V1Cpor
219000130513     c                   when      $fnARB = *on
219100130513     c                   move      ARBlnp        V1Cpor
219200130513     c                   other
219300130513     c                   move      BLPlnp        V1Cpor
219400130513     c                   endsl
219500130513     c                   other
219600040527     c                   move      v1cpot        v1cpor
219700130513     c                   endsl
219800040527    2c                   endif
219900040527
220000040527    2c                   if        v1crsp <> 'V'
220100040527     c                   clear                   v1cpor
220200040527     c                   clear                   v1dpor
220300040527    2c                   endif
220400040211      *
220500040212      * verifico l'inserimento del P.O. responsabile
220600040220      *
220700040220     C                   clear                   v1dpor
220800040212    2c                   If        v1cpor <> *blanks
220900040212      *
221000040212      * Ricerca
221100040212     c     '?'           scan      v1cpor
221200040212    3c                   if        %found
221300040212     C                   clear                   dstnsd
221400040212     C                   call      'TNSD24R'
221500040212     C                   parm                    §cod
221600040212     C                   parm                    §tip
221700040212     C                   parm                    §des
221800040212     C                   parm                    §pra
221900040212     C                   movel     §cod          v1cpor
222000040212     C                   movel     §des          v1dpor
222100040212     c                   eval      *in90 = *on
222200040219     C                   eval      *IN40 = *on
222300040212     c                   leave
222400040212    3C                   endif
222500040217    2C                   endif
222600040217      *
222700040217     C     v1cpor        ifeq      *blanks
222800040217     C                   move      *all'0'       v1cpor
222900040217     C                   endif
223000040212      *
223100040212     C                   testn                   v1cpor               79
223200040212     C                   move      v1cpor        w001a             1
223300040217      *
223400040217     c                   move      v1cpor        W_por
223500040212      *
223600040217    2C     *in79         ifeq      *off
223700040212     C     w001a         orlt      '0'
223800040212     c                   eval      *in28 = *on
223900040212     c                   movel     MSG(9)        V1Cmsg
224000040212     C                   eval      *IN40 = *on
224100040212     c                   leave
224200040217    2C                   endif
224300040212      * verifico se P.O. esistente
224400040217    2c                   if        W_por > 0
224500040217     C     W_por         chain     azorg01l
224600040212    3C                   if        %found(azorg01l)
224700040212     C                   movel     orgdes        v1dpor
224800040420      * verifico se P.O. valido
224900040617     c                   if        orgfva<>' ' or (orgfag<>'F' and orgfag<>'A'
225000040617     c                             and orgfil <> 46)
225100040420     c                   eval      *in28 = *on
225200040420     c                   movel     MSG(9)        V1Cmsg
225300040420     C                   eval      *IN40 = *on
225400040420     c                   leave
225500040420     c                   endif
225600040212      *  errore
225700040212     C                   ELSE
225800040212     c                   eval      *in28 = *on
225900040213     c                   movel     MSG(9)        V1Cmsg
226000040212     C                   eval      *IN40 = *on
226100040212     c                   leave
226200040212    3C                   ENDIF
226300040212      *
226400040217    2C                   ENDIF
226500070608
226600070626      * controllo causale variazione per bolle se esiste  e se bolla valida per la variazione
226700070608
226800070608     c                   clear                   fnlr66ds
226900070608    2c                   if        V1ccva  <> *blanks
227000070626      * per 09 acceso emetto un msg di errore
227100071018    3c                   If        *in09
227200070626     c                   eval      *in28 = *on
227300071018     c                   movel     MSG(34)       V1Cmsg
227400070626     C                   eval      *IN47 = *on
227500070626     c                   leave
227600071018    3c                   endif
227700070608     c                   movel     V1anns        d66aas
227800070608     c                   movel     V1lnps        d66lnp
227900070608     c                   movel     V1nrss        d66nrs
228000070608     c                   movel     V1nsps        d66nsp
228100071018      * determino se sono il p.o. di partenza o il p.o. di arrivo
228200071018
228300071018      * se non trovata bolla in arb errore
228400071018    3c                   If        $fnarb = *off
228500071018     c                   eval      *in28 = *on
228600071018     c                   movel     MSG(35)       V1Cmsg
228700071018     C                   eval      *IN47 = *on
228800071018     c                   leave
228900071018    3c                   endif
229000071018
229100130611     c                   move      arblna        w003a
229200130611     c     w003a         lookup    wPOG                                   27
229300071018    3c                   if        %found
229400071018     c                   movel     'A'           d66tbo
229500071018   x3c                   else
229600071018      * verifico se sono la linea di partenza
229700130611     c                   move      arblnp        w003a
229800130611     c     w003a         lookup    wPOG                                   27
229900130611    3c                   if        %found
230000071018     c                   movel     'P'           d66tbo
230100071018    4c                   endif
230200071018
230300071018    3c                   endif
230400071018
230500071018      * se bolla non in gestione al Po Errore
230600071018    3c                   if        d66tbo = ' '
230700071018     c                   eval      *in28 = *on
230800071018     c                   movel     MSG(36)       V1Cmsg
230900071018     C                   eval      *IN47 = *on
231000071018     c                   leave
231100071018    3c                   endif
231200070608      *
231300070608     c     '?'           scan      v1ccva                                 90
231400070608      * Ricerca causale
231500070608    3c                   if        *in90
231600070608     c                   movel     'V'           d66ges
231700070608      *
231800070608     c                   movel     fnlr66ds      kpjbu
231900070608     c                   call      'FNLR66R'
232000070608     c                   parm                    kpjba
232100070608     c                   movel     kpjbu         fnlr66ds
232200070608     c                   movel     d66cvb        V1ccva
232300070608     c                   movel     d66des        v1dcva
232400070608   X3c                   else
232500070608      * Controllo causale inserita
232600070608     c                   movel     'C'           d66ges
232700070608     c                   movel     V1ccva        d66cvb
232800070608      *
232900070608     c                   movel     fnlr66ds      kpjbu
233000070608     c                   call      'FNLR66R'
233100070608     c                   parm                    kpjba
233200070608     c                   movel     kpjbu         fnlr66ds
233300070608     c                   movel     d66cvb        v1ccva
233400070608     c                   MOVEL     d66des        v1dcva
233500070608      *
233600070608      * Errore
233700070608    4c                   if        d66msg <> *blank
233800070608     c                   eval      *in28 = *on
233900070608     c                   movel     MSG(34)       V1Cmsg
234000070608     C                   eval      *IN47 = *on
234100070608     c                   leave
234200070608    4c                   endif
234300070608
234400070608    3c                   endif
234500070608      *
234600070608      * RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
234700070608      *
234800070608     c                   clear                   fnlr48ds
234900070608     c                   movel     V1anns        d48aas
235000070608     c                   movel     V1lnps        d48lnp
235100141029     c                   movel     V1nrss        d48nrs
235200070608     c                   movel     V1nsps        d48nsp
235300070608     c                   movel     Dutpou        d48fgs
235400070608     c                   If        $fnarb = *on
235500070608     c                   movel     arbcbo        d48cbo
235600070608     c                   else
235700070608     c                   movel     blpcbo        d48cbo
235800070608     c                   endif
235900071018     c                   eval      d48tbo = d66tbo
236000070608     c                   movel     v1ccva        d48cvb
236100070608     c                   movel     'S'           d48f12
236200140429     c                   movel     'V'           d48ffr
236300140429      *
236400140429     c                   clear                   dsARBd
236500111025     c
236600070608     c                   movel     fnlr48ds      kpjbu
236700070608     c                   call      'FNLR48R'
236800070608     c                   parm                    KPJBA
236900070608     c                   parm                    DSARBD
237000070608     c                   parm                    DSARBK
237100070608     c                   parm                    DARBT
237200070608     c                   parm                    DSARBG
237300070608     c                   parm                    TRUL90DS
237400070608      *
237500070608     c                   movel     kpjbu         fnlr48ds
237600070608     C* errore
237700070608     C                   if        d48err = '1'
237800070608     c                   eval      *in28 = *on
237900070608     c                   movel     MSG(33)       V1Cmsg
238000070608     C                   eval      *IN47 = *on
238100070608     c                   leave
238200070608     c                   else
238300070608     c                   clear                   v1ccva
238400070608     c                   clear                   v1dcva
238500070608     c                   endif
238600070608     C*
238700070608    2C                   endif
238800070330      * se è andato tutto ok ed è stato dato F6  confermo richiamo
238900070330    2c                   If        *inkf and *in61 and v1cdcr > 0
239000070330     c                   clear                   fnlr48ds
239100070330    3c                   if        arbtc1 = 'S' or arbtc2 = 'S'
239200070330     c                   eval      d48cvb='CS'
239300070330     c                   eval      d48ffr='V'
239400070330     c                   eval      d48f12='S'
239500070330     c                   else
239600070330     c                   eval      d48cvb='CR'
239700070330     c                   eval      d48ffr='E'
239800070330     c                   eval      d48err='Z'
239900070330    3c                   endif
240000070330     c                   z-add     v1anns        D48AAS
240100070330     c                   z-add     v1lnps        D48LNP
240200070330     c                   z-add     v1nrss        D48NRS
240300070330     c                   z-add     v1nsps        D48NSP
240400070330     c                   MOVEL     ARBCBO        D48CBO
240500070330     c                   MOVEL     'A'           D48TBO
240600070330     c                   MOVEL     'S'           D48F12
240700070330     c                   MOVEL     'G'           D48trc
240800070330     c                   MOVEL     fnlr48ds      KPJBU
240900140429     c                   clear                   dsARBd
241000070330     c                   movel     v1ctcr        §BGTCR
241100141029     c                   z-add     v1cocr        §BGHCR
241200070330     c                   z-add     wdcr          §BGDCR
241300070330     c                   movel     arbgc1        §BGGC1
241400070330     c                   movel     arbgc2        §BGGC2
241500070330     c                   movel     arbtc1        §BGTC1
241600070330     c                   movel     arbtc2        §BGTC2
241700070330     c                   call      'FNLR48R'
241800070330     c                   PARM                    KPJBA
241900070330     c                   PARM                    DSARBD
242000070330     c                   PARM                    DSARBK
242100070330     c                   PARM                    DARBT
242200070330     c                   PARM                    DSARBG
242300070330     c                   PARM                    trul90ds
242400070330     c                   MOVEL     KPJBU         fnlr48ds
242500070330      * Ritorno con errore o premuto f12
242600070330    3c                   if        d48err<>*blanks and d48err <> 'Z'
242700070330     c                   eval      *IN28 = *on
242800070330     c                   eval      *IN62 = *on
242900070330     c                   movel     d48msg        v1cmsg
243000070330     c                   leave
243100070330    3c                   endif
243200070330      * se premuto f12 nulla
243300070330    3c                   if        d48f12='1'
243400070330     c                   eval      *IN90 = *on
243500070330     c                   leave
243600070330    3c                   endif
243700070330
243800070330      * preparo il campo fisso per le modifiche effettuate
243900070330     c                   If        d48err = 'Z' and d48msg <> *blanks
244000070330     c                   eval      not_dcr = 'S'
244100070330     c                   movel     d48msg        note_dcr
244200070330      * preparo un campo note
244300070330     c                   endif
244400070330      * valorizzo il flag che mi guida all'inserimento delle note ldv
244500070330     c                   eval      not_ldv = 'S'
244600070330     c                   seton                                        60
244700070330    2c                   endif
244800140429      *
244900140429      /free
245000150421
245100150421         // -?Immissione R.A. non chiusa: emissione W04?
245200150422         //IF  *inKF  and  wMod = 'I'  and  V1Ccdt = *blank;
245300150422         IF  wMod = 'I'  and  V1Ccdt = *blank  and  Not $GesW04;
245400150421            exsr  sr_GesW04;
245500150422             // -?F12 = Ritorno?
245600150422             if  *inKL;
245700150422               *in90 = *on;
245800151202             endif;
245900151202             if  *in90;
246000150422               leave;
246100150422             endif;
246200150421         ENDIF;
246300140429
246400140429         // -?Conferma R.A. con Motivo Apertura " 17"?
246500140429         //  ?(Fermo Deposito autorizza mittente)?
246600140430         //  ?- SE non si è in Sede ma si è in Arrivo -?
246700140506         //  ?- SE non si è in modifica (*in50/70/72/74) -?
246800140430         //  ?=> Variazione Bolle (solo Arrivo)?
246900140506         IF  *inKF  and  $FNARB  and  V1Cmad = c_FDEAM
247000140516                    and  Not *in50
247100140516                    and  Not *in70  and  Not *in72  and  Not *in74;
247200140506
247300140506           $RA_MA_17 = *on;
247400140429
247500140429           clear  dsARBd;
247600140429           clear  dsARBk;
247700140429           clear  dARBt;
247800140429           clear  dsARBg;
247900140429           clear  FNLR48ds;
248000140429
248100140429           D48aas = V1annS;
248200140429           D48lnp = V1lnpS;
248300140429           D48nrs = V1nrsS;
248400140429           D48nsp = V1nspS;
248500140430           D48cbo = ARBcbo;
248600140429           D48cvb = c_CauVarB_I7;
248700140429           D48trc = 'D';
248800140429           D48ffr = 'E';
248900140430           D48tbo = 'A';
249000140429
249100140430           §BDrsd = ARBrsd;
249200140430           §BDind = ARBind;
249300140430           §BDcad = ARBcad;
249400140430           §BDlod = ARBlod;
249500140430           §BDprd = ARBprd;
249600140430           §BDnzd = ARBnzd;
249700140430           §BDfin = ARBfin;
249800140506           §BDffd = 'S';
249900140430           §BDcpi = ARBcpi;
250000140506
250100140506           // -?Reperimento ragione sociale estesa del destinatario?
250200140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'D' )  FIAR4000;
250300140506           if  %found(FIAR401L);
250400140506             §BDrd2 = AR4not;
250500140506           endif;
250600140506
250700140506           // -?Reperimento referente e telefono del destinatario?
250800140506           clear  dAR5gen;
250900140506           chain  ( D48aas : D48lnp : D48nrs : D48nsp : 'GEN' )  FIAR5000;
251000140506           if  %found(FIAR501L)  and  AR5atb = *blank;
251100140506             dAR5gen = AR5uni;
251200140506             §BDref = §AR5ref;
251300140506             §BDtel = §AR5teld;
251400140506           endif;
251500140429
251600140429           kpjbu = FNLR48ds;
251700140429           fnlr48r ( kpjba :
251800140429                     dsARBd :
251900140429                     dsARBk :
252000140429                     dARBt :
252100140429                     dsARBg :
252200140429                     TRUL90ds );
252300140429           FNLR48ds = kpjbu;
252400140429
252500140429           select;
252600140429             //?- se rilevato errore => nulla?
252700140429             when  D48err <> *blank;
252800140429               *in28  = *on;
252900140429               *in62  = *on;
253000140429               V1Cmsg = D48msg;
253100140429               leavesr;
253200140429             //?- se premuto F12 => nulla?
253300140429             when  D48f12 = '1';
253400140429               *in90  = *on;
253500140429               leave;
253600140429           endsl;
253700140429
253800140429         ENDIF;
253900141210
254000141210         // -?Conferma R.A. con Motivo Apertura " 60"?
254100141210         //  ?(Verifica telefonica "PINcode")?
254200141210         //  ?- SE non si è in Sede ma si è in Arrivo -?
254300141210         //  ?- SE non si è in modifica (*in50/70/72/74) -?
254400141210         //  ?=> Richiesta BRTcode + PINcode?
254500141210         //  ?=> DOPO: Registrazione Esito nelle Note?
254600141210         IF  *inKF  and  $FNARB  and  V1Cmad = c_VPINC
254700141210                    and  Not *in50
254800141210                    and  Not *in70  and  Not *in72  and  Not *in74;
254900141210
255000141210           // -?Richiesta / Controllo "PINcode"?
255100141210           clear  trulPINds;
255200141210           iPINaas = V1annS;
255300141210           iPINlnp = V1lnpS;
255400141210           iPINnrs = V1nrsS;
255500141210           iPINnsp = V1nspS;
255600141210
255700141210           trulPINr ( trulPINds );
255800141212
255900141216           if  oPINfxx = '03'  and  oPINnre = *zero;
256000141216             *inKF = *off;
256100141216             *in90 = *on;
256200141216             exsr  sr_PulCtl2;
256300141216           else;
256400141216             $RA_MA_60 = *on;
256500141216             // -?N.B. - L'esito verrà registrato DOPO nelle NOTE?
256600141216           endif;
256700141210
256800141210         ENDIF;
256900150114
257000150114
257100150114         //?Posizionamento curosre sulle Note - SE Inserim. & NO errori?
257200150114         if  wMod = 'I'        and
257300150114             Not *in28         and  Not *in90                    and
257400150114             ( ( V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero )    or
257500150324               ( V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero ) );
257600150114           *in48 = *on;
257700150114         endif;
257800140429
257900140429       /end-free
258000040422      *
258100040217     c                   leave
258200040212      *
258300040211    1C                   endDO
258400040211      *
258500040211     c                   ENDSR
258600150112      *
258700150112      *---------------------------------------------------------------*
258800150114      *?Routine di Impostazione automatica di alcuni campi a video   ?*
258900150114      *?  - SE INSERIMENTO -                                         ?*
259000150112      *---------------------------------------------------------------*
259100150114     c     sr_ImpAut_Ins BEGSR
259200150112      *
259300150113sel 1c                   Select
259400150113      *
259500150112      * -?Impostazione campi (se vuoti) per Spedizione?
259600150113w   1c                   When      V1Ctor = 'S'  and  (V1lnpS+V1nspS) > *zero
259700150112      * - -?Chi Chiama => Destinatario?
259800150112if  4c                   if        V1Ctch = *blank
259900150112     c                   eval      V1Ctch = 'D'
260000150112     c                   exsr      sr_Ctrl_CCR
260100150112e   4c                   endif
260200150202      * - -?Motivo Apertura => Consegna +?
260300150112if  4c                   if        V1Cmad = *blank
260400150112     c                   eval      V1Cmad = '  1'
260500150202     c                   clear                   wSaveMAD
260600150112e   4c                   endif
260700150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
260800150202if  4c                   if        V1Cmad <> wSaveMAD
260900150202     c                   eval      wSaveMAD = V1Cmad
261000150202     c                   exsr      sr_Ctrl_MAR
261100150202     c                   eval      V1Ccdt   = §MARcauChP
261200150202e   4c                   endif
261300150114      * - -?Posizionamento cursore sulle Note?
261400150114     c                   eval      *in48 = *on
261500150112      *
261600150112      * -?Impostazione campi (se vuoti) per O.R.M.?
261700150113w   1c                   When      V1Ctor = 'O'  and  (V1poeO+V1nOrm) > *zero
261800150112      * - -?Chi Chiama => Mittente?
261900150112if  4c                   if        V1Ctch = *blank
262000150113     c                   eval      V1Ctch = 'M'
262100150112     c                   exsr      sr_Ctrl_CCR
262200150112e   4c                   endif
262300150112      * - -?Motivo Apertura => Ritiro?
262400150112if  4c                   if        V1Cmad = *blank
262500150112     c                   eval      V1Cmad = '  3'
262600150202     c                   clear                   wSaveMAD
262700150112e   4c                   endif
262800150202      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
262900150202if  4c                   if        V1Cmad <> wSaveMAD
263000150202     c                   eval      wSaveMAD = V1Cmad
263100150202     c                   exsr      sr_Ctrl_MAR
263200150202     c                   eval      V1Ccdt   = §MARcauChP
263300150202e   4c                   endif
263400150114      * - -?Posizionamento cursore sulle Note?
263500150114     c                   eval      *in48 = *on
263600150407      *
263700150407      * -?Impostazione campi (se vuoti) per Generico?
263800150407w   1c                   When      V1Ctor = 'G'
263900150407      * - -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
264000150407if  4c                   if        V1Cmad <> wSaveMAD
264100150407     c                   eval      wSaveMAD = V1Cmad
264200150407     c                   exsr      sr_Ctrl_MAR
264300150407     c                   eval      V1Ccdt   = §MARcauChP
264400150407e   4c                   endif
264500150112      *
264600150113e   1c                   EndSl
264700150313
264800150625      * - -?STATO (ex Priorità):?
264900150625      *    ?Se viene impostata la causale chiusura metto priorità "9"?
265000150625      *    ?(in inserimento) o "8" (in modifica - SE modificata la?
265100150625      *    ?Causale Chiusura)?
265200150313     c                   IF        V1Ccdt <> *blanks
265300150402     c                   IF        wmod = 'I'
265400150625      *   -?SE modalità Immissione & R.A. Chiusa & Stato previsto in?
265500150625      *    ?tab. "COR" => si imposta lo Stato previsto in tab. "COR"?
265600150625      *    ?(già reperita nella subr. SR_CONTR01)?
265700150625     c                   if        §CORstato <> *blanks
265800150625     c                   eval      V1Cprt = §CORstato
265900150625     c                   else
266000150313     c                   eval      V1Cprt = '9'
266100150625     c                   endif
266200150324     c                   ELSE
266300150324     c                   eval      V1Cprt = '8'
266400150324     c                   ENDIF
266500150313     c                   ENDIF
266600150320      *
266700150320      * Decodifico (potrebbe essere cambiato)
266800150320     c                   clear                   tibs02ds
266900150320     C                   MOVEL     'C'           t02mod
267000150320     C                   MOVEL     knsif         t02sif
267100150320     C                   MOVEL     'PRR'         t02cod
267200150320     C                   MOVEL     v1cprt        t02ke1
267300150320      *
267400150320     C                   CALL      'TIBS02R'
267500150320     C                   PARM                    KPJBA
267600150320     C                   PARM                    TIBS02DS
267700150320      *
267800150320    4C                   IF        t02err = *BLANKS
267900150320     C                   movel     T02uni        DPRR
268000150320     C                   movel     §PRRdesc      v1dprt
268100150320     c                   endif
268200150112      *
268300150112     c                   ENDSR
268400150112      *
268500150112      *---------------------------------------------------------------*
268600150112      * Routine controllo tabella "MAR" = Motivo Apertura             *
268700150112      *---------------------------------------------------------------*
268800150112     c     sr_Ctrl_MAR   BEGSR
268900150323
269000150323      * Ricerca
269100150323    3c                   if        %scan('?':V1Cmad) > 0
269200150422     c                             and       $Video <> 'W4'
269300150323     c                   clear                   tntbMARds1
269400150323      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
269500150323      *      dall'utente in APERTURA;
269600150323      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
269700150323      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
269800150323      *      in GESTIONE.
269900150323     c                   if        wMod <> 'I'
270000150323     c                   eval      iMARutUt = 'I'
270100150323     c                   endif
270200150323     c                   eval      iMARutAp = 'I'
270300150323     c                   movel(p)  tntbMARds1    KPJBU
270400150323      *
270500150323     c                   call      'TNTBMARR1'
270600150323     c                   parm                    KPJBA
270700150323     c                   movel     KPJBU         tntbMARds1
270800150323    4c                   if        oMARfxx = *blank  and  oMARerr = *off
270900150323     c                   eval      V1Cmad  = oMARcod
271000150323     C                   eval      V1Dmad  = oMARdes
271100150323      *  errore (non trovato nulla pulisco il '?' )
271200150323     C                   ELSE
271300150323     c                   clear                   V1Cmad
271400150323     c                   clear                   V1Dmad
271500150323    4C                   ENDIF
271600150422    3C                   ENDIF
271700150112      *
271800150112     c                   clear                   tibs02ds
271900150112     C                   MOVEL     'C'           t02mod
272000150112     C                   MOVEL     knsif         t02sif
272100150112     C                   MOVEL     'MAR'         t02cod
272200150422     c                   if        $Video = 'W4'
272300150422     c                   eval      T02ke1 = wwcMAD
272400150422     c                   else
272500150112     C                   MOVEL     v1cmad        t02ke1
272600150422     c                   endif
272700150112      *
272800150112     C                   CALL      'TIBS02R'
272900150112     C                   PARM                    KPJBA
273000150112     C                   PARM                    TIBS02DS
273100150112      *
273200150112    4C                   IF        t02err = *BLANKS
273300150112     C                   movel     T02uni        DMAR
273400150422     c                   if        $Video = 'W4'
273500150422     c                   eval      wwdMAD = §MARdesc
273600150422     c                   else
273700150112     C                   movel     §MARdesc      v1dmad
273800150423     c                   eval      wwdMAD = §MARdesc
273900150422     c                   endif
274000150112     c                   if        (*in74 = *off  and  §MARutAp = 'N')  or
274100150112     c                             (wMod <> 'I'   and  §MARutUt = 'N')
274200150112     c                   eval      *in28 = *on
274300150112     c                   eval      *in36 = *on
274400150112     c                   movel     msg(18)       V1Cmsg
274500150112     c                   leavesr
274600150112     c                   endif
274700150112      *
274800150112     C                   ELSE
274900150112     c                   eval      *in28 = *on
275000150112     c                   movel     msg(18)       V1Cmsg
275100150112     C                   eval      *IN36 = *on
275200150112     c                   leavesr
275300150112    4C                   ENDIF
275400150112      *
275500150112     c                   ENDSR
275600150112      *
275700150112      *---------------------------------------------------------------*
275800150112      * Routine controllo tabella "CCR" = Chi Chiama                  *
275900150112      *---------------------------------------------------------------*
276000150112     c     sr_Ctrl_CCR   BEGSR
276100150112      *
276200150112     c                   clear                   tibs02ds
276300150112     C                   MOVEL     'C'           t02mod
276400150112     C                   MOVEL     knsif         t02sif
276500150112     C                   MOVEL     'CCR'         t02cod
276600150112     C                   MOVEL     v1ctch        t02ke1
276700150112      *
276800150112     C                   CALL      'TIBS02R'
276900150112     C                   PARM                    KPJBA
277000150112     C                   PARM                    TIBS02DS
277100150112      *
277200150112    4C                   IF        t02err = *BLANKS
277300150112     C                   movel     T02uni        DCCR
277400150112     C                   movel     T02ke1        v1ctch
277500150112     c                   movel     §ccrdesc      v1dtch
277600150112      *
277700150112     C                   ELSE
277800150112     C                   eval      *IN28 = *on
277900150112     c                   movel     msg(07)       V1Cmsg
278000150112     C                   eval      *IN35 = *on
278100150112     c                   leavesr
278200150112    4C                   ENDIF
278300150112      *
278400150112     c                   ENDSR
278500070323      *
278600140429      /free
278700150421
278800150421       //--------------------------------------------------------------
278900150421       //?Gestione window W4 per immissione R.A. non chiusa            ?
279000150421       //--------------------------------------------------------------
279100150421       BEGSR  sr_GesW04;
279200150421
279300150423         $Video   = 'W4';
279400150421
279500150421         //clear  FIA1W04;  // -?NO: serve sapere se sono già stati?
279600150421                            //  ?    inseriti dati?
279700150914         clear  W4cCap;
279800150616
279900150915         // -?Impostazione gestione Reparto?
280000150915         $Reparto = ( V1Cpog <> c_Sede  or  V1Cmad = c_ReclSede );
280100150421
280200150421         W4nTel = V1nTel;
280300150421         W4mail = V1mail;
280400150421         W4cMad = V1cMad;
280500150915         clear  W4cRpr;
280600150915         clear  W4cPog;
280700150720         select;
280800150915           // -?Inserimento Reclamo da Sede (in Sede, motivo " 70"):?
280900150915           //  ?proposto reparto?
281000150720           when  *in05  and  W4cMad = c_ReclSede;
281100150915             W4cRpr = 'RAC';
281200150720           // -?Proposto Utente, non Reparto (in Sede)?
281300150720           when  Not $Reparto;
281400150720             W4cRpr = KNMUS;
281500150720           // -?Proposto Reparto (in Filiale)?
281600150720           other;
281700150720             W4cRpr = 'RAC';
281800150720         endsl;
281900150720         if  Not *in05  or  W4cMad <> c_ReclSede;
282000150720           W4cPog = V1Cpog;
282100150720         endif;
282200150428
282300150428         // -?Decodifiche dati immessi o proposti?
282400150428         wwcMad = W4cMad;
282500150428         exsr  sr_CtrlMAD;
282600150428         W4cMad = wwcMad;
282700150428         W4dMad = wwdMad;
282800150428         wwcRPR = W4cRpr;
282900150616         if  Not $Reparto;
283000150616           wwcPog = W4cPog;
283100150616           exsr  sr_CtrlRPG;
283200150616         else;
283300150616           exsr  sr_CtrlRPR;
283400150616         endif;
283500150428         W4cRpr = wwcRPR;
283600150428         W4dRpr = wwdRPR;
283700150717         //wwcMad = W4cMad;   ?(già impostata prima)?
283800150428         wwcPog = W4cPog;
283900150428         exsr  sr_CtrlPOG;
284000150428         W4cPog = wwcPog;
284100150428         W4dPog = wwdPog;
284200150428
284300150428         *in28 = *off;
284400150428         *in90 = *off;
284500150428         *in36 = *on;     // -?Posiz. cursore su Motivo?
284600150428         *in38 = *off;    // -?Posiz. cursore su Mail?
284700150428         *in45 = *off;    // -?Posiz. cursore su Reparto Gestione?
284800150428         *in46 = *off;    // -?Posiz. cursore su Filiale Gestione?
284900150428         *in55 = *off;    // -?Posiz. cursore su Telefono?
285000150911         *in56 = *off;    // -?Posiz. cursore su C.A.P. cliente?
285100150422
285200150423         DoU  *in28 = *off  and  *in90 = *off  and  *inKF;
285300150423
285400150423           exfmt  FIA1W04;
285500150423
285600150914           clear  V1Cmsg;
285700150423           *in28 = *off;
285800150423           *in90 = *off;
285900150423           *in36 = *off;       // -?Motivo errato?
286000150423           *in38 = *off;       // -?Mail   errata?
286100150423           *in45 = *off;       // -?Reparto Gestione errato?
286200150423           *in46 = *off;       // -?Filiale Gestione errata?
286300150423           *in55 = *off;       // -?Telefono obbligatorio?
286400150911           *in56 = *off;       // -?C.A.P. cliente   errato?
286500150423
286600150423           // -?F12 = Ritorno al fmt D01?
286700150423           if  *inKL;
286800150423             clear  $Video;
286900150423             leavesr;
287000150423           endif;
287100150423
287200150423           // -?Num. Telefono o Mail obbligatori?
287300150423           if  W4nTel = *blank  and  W4mail = *blank;
287400150723             *in28  = *on;
287500150423             *in55  = *on;
287600150423             V1Cmsg = Msg(49);
287700150423             iter;
287800150423           endif;
287900150423
288000150423           // -?Mail errata?
288100150423           if  W4mail <> *blank;
288200150423             wwMail = W4mail;
288300150423             exsr  sr_CtrlMail;
288400150423             W4mail = wwMail;
288500150723             if  *in28  or  *in90;
288600150423               iter;
288700150423             endif;
288800150423           endif;
288900150423
289000150423           // -?Controllo Motivo Apertura Reclamo?
289100150423           wwcMad = W4cMad;
289200150423           exsr  sr_CtrlMAD;
289300150423           W4cMad = wwcMad;
289400150423           W4dMad = wwdMad;
289500150723           if  *in28  or  *in90;
289600150423             iter;
289700150423           endif;
289800150723
289900150723           // -?SE Motivo = " 70" (quindi devo essere in Sede):?
290000150723           //  ?Controllo la Filiale Gestione PRIMA dell'Utente Gestione?
290100150723           If  W4cMad = c_ReclSede;
290200150911             // ·?La Filiale Gestione potrebbe voler essere ricavata?
290300150911             //  ?dal C.A.P. del cliente?
290400150911             if  W4cCap <> *blank;
290500150911               exsr  sr_CtrlCAP;
290600150914               if  *in28;
290700150914                 iter;
290800150914               endif;
290900150911             endif;
291000150723             wwcPog = W4cPog;
291100150723             //wwcMad = W4cMad; ?(già impostata prima)?
291200150723             exsr  sr_CtrlPOG;
291300150723             W4cPog = wwcPog;
291400150723             W4dPog = wwdPog;
291500150915             if  *in28;
291600150723               iter;
291700150723             endif;
291800150723             //if  ( *in05      and  W4cPog <> c_Sede )  or
291900150723             //    ( Not *in05  and  W4cPog =  c_Sede );
292000150723             if  W4cPog = c_Sede;
292100150723               *in28 = *on;
292200150723               *in46 = *on;
292300150723               if  Not *in05;
292400150723                 V1Cmsg = Msg(48);
292500150723               else;
292600150723                 V1Cmsg = Msg(53);
292700150723               endif;
292800150723               iter;
292900150723             endif;
293000150723           EndIf;
293100151202
293200151202           // -?SE modificato Motivo in " 18" (Reso):?
293300151202           //  ?ritorno alla videata pecedente (la R.A. va chiusa)?
293400151202           If  W4cMad = c_MotAp_Reso;
293500151202             *in90 = *on;
293600151202             leave;
293700151202           EndIf;
293800150915
293900150915           // -?Re-impostazione gestione Reparto?
294000150915           $Reparto = ( W4cPog <> c_Sede  or  W4cMad = c_ReclSede );
294100150423
294200150617           // -?Controllo Reparto Gestione (in filiale)?
294300150617           //  ?Controllo Utente  Gestione (in sede)?
294400150429           wwcRpr = W4cRpr;
294500150616           if  Not $Reparto;
294600150616             wwcPog = W4cPog;
294700150616             exsr  sr_CtrlRPG;
294800150616           else;
294900150616             exsr  sr_CtrlRPR;
295000150616           endif;
295100150429           W4cRpr = wwcRpr;
295200150429           W4dRpr = wwdRpr;
295300150915           if  *in28;
295400150429             iter;
295500150429           endif;
295600150423
295700150723           // -?SE Motivo <> " 70":?
295800150723           //  ?Controllo Filiale Gestione   DOPO?
295900150723           //  ?il Reparto Gestione (in filiale)?
296000150723           //  ?o l'Utente  Gestione (in sede)?
296100150723           If  W4cMad <> c_ReclSede;
296200150723             wwcPog = W4cPog;
296300150723             //wwcMad = W4cMad; ?(già impostata prima)?
296400150723             exsr  sr_CtrlPOG;
296500150723             W4cPog = wwcPog;
296600150723             W4dPog = wwdPog;
296700150915             if  *in28;
296800150723               iter;
296900150723             endif;
297000150723             //if  ( *in05      and  W4cPog <> c_Sede )  or
297100150723             //    ( Not *in05  and  W4cPog =  c_Sede );
297200150723             if  Not *in05  and  W4cPog =  c_Sede;
297300150723               *in28 = *on;
297400150723               *in46 = *on;
297500150723               V1Cmsg = Msg(48);
297600150723               iter;
297700150723             endif;
297800150723           EndIf;
297900150423
298000150423           *in36 = *on;     // -?Posiz. cursore su Motivo?
298100150422
298200150423         EndDo;
298300150421
298400150421         V1nTel = W4nTel;
298500150421         V1mail = W4mail;
298600150421         V1Cmad = W4cMad;
298700151202
298800151202         If  W4cMad <> c_MotAp_Reso;
298900151202
299000151202           V1Crpg = W4cRpr;
299100151202           V1Cpog = W4cPog;
299200151202
299300151202           $GesW04 = *on;
299400151202
299500151202         EndIf;
299600151202
299700151202         clear  $Video;
299800150421
299900150421       ENDSR;
300000140429
300100140429       //--------------------------------------------------------------
300200140429       //?Controlli specifici per Motivo Apertura R.A. " 17"=F.Deposito?
300300140429       //--------------------------------------------------------------
300400140429       BEGSR  sr_CtrMotAp_17;
300500140430
300600140430         Select;
300700140430
300800140430           // -?Errore SE si è in Sede?
300900140430           When  *in05;
301000140430             *in28  = *on;
301100140430             *in36  = *on;
301200140430             V1Cmsg = Msg(42);
301300140430             leavesr;
301400140430
301500140430           // -?Errore SE la spedizione NON è in Arrivo?
301600140430           When  Not $FNARB;
301700140430             *in28  = *on;
301800140430             *in36  = *on;
301900140430             V1Cmsg = Msg(42);
302000140430             leavesr;
302100140430
302200140430           // -?Errore SE la linea di partenza NON è tra le filiali in?
302300140430           //  ?gestione all'utente?
302400140430           When  %lookup( %editc( ARBlnp : 'X' ) : wPOG ) = *zero;
302500140430             *in28 = *on;
302600140430             *in36 = *on;
302700140430             V1Cmsg = Msg(41);
302800140430             leavesr;
302900140430
303000140430         EndSl;
303100140430
303200140430
303300140430         // -?Verifica SE la causale variazione bolle "I7" può essere?
303400140430         //  ?utilizzata con la spedizione in oggetto?
303500140429         clear  FNLR66ds;
303600140429
303700140507         D66ges = 'K';
303800140429         D66cvb = c_CauVarB_I7;
303900140429         D66aas = V1annS;
304000140429         D66lnp = V1lnpS;
304100140429         D66nrs = V1nrsS;
304200140429         D66nsp = V1nspS;
304300140430         D66tbo = 'A';
304400140429
304500140505         kpjbu = FNLR66ds;
304600140505         fnlr66r ( kpjba );
304700140505         FNLR66ds = kpjbu;
304800140505
304900140505         if  D66msg <> *blank;
305000140505           *in28  = *on;
305100140505           *in36  = *on;
305200140505           V1Cmsg = Msg(26);
305300140505           leavesr;
305400140505         endif;
305500140429
305600140429
305700140429         // -?Verifica SE ci sono errori nella manutenzione della?
305800140429         //  ?spedizione in oggetto?
305900140429         clear  dsARBd;
306000140429         clear  dsARBk;
306100140429         clear  dARBt;
306200140429         clear  dsARBg;
306300140429         clear  FNLR48ds;
306400140429
306500140429         D48aas = V1annS;
306600140429         D48lnp = V1lnpS;
306700140429         D48nrs = V1nrsS;
306800140429         D48nsp = V1nspS;
306900140430         D48cbo = ARBcbo;
307000140429         D48cvb = c_CauVarB_I7;
307100140429         D48trc = 'D';
307200140429         D48ffr = 'F';
307300140429         D48tbo = D66tbo;
307400140429
307500140430         §BDrsd = ARBrsd;
307600140430         §BDind = ARBind;
307700140430         §BDcad = ARBcad;
307800140430         §BDlod = ARBlod;
307900140430         §BDprd = ARBprd;
308000140430         §BDnzd = ARBnzd;
308100140430         §BDfin = ARBfin;
308200140506         §BDffd = 'S';
308300140430         §BDcpi = ARBcpi;
308400140429
308500140429         // -?Reperimento ragione sociale estesa del destinatario?
308600140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'D' )  FIAR4000;
308700140429         if  %found(FIAR401L);
308800140429           §BDrd2 = AR4not;
308900140429         endif;
309000140429
309100140429         // -?Reperimento referente e telefono del destinatario?
309200140429         clear  dAR5gen;
309300140429         chain  ( D66aas : D66lnp : D66nrs : D66nsp : 'GEN' )  FIAR5000;
309400140506         if  %found(FIAR501L)  and  AR5atb = *blank;
309500140429           dAR5gen = AR5uni;
309600140429           §BDref = §AR5ref;
309700140429           §BDtel = §AR5teld;
309800140429         endif;
309900140429
310000140429         kpjbu = FNLR48ds;
310100140429         fnlr48r ( kpjba :
310200140429                   dsARBd :
310300140429                   dsARBk :
310400140429                   dARBt :
310500140429                   dsARBg :
310600140429                   TRUL90ds );
310700140429         FNLR48ds = kpjbu;
310800140429
310900140429         if  D48err <> *blank;
311000140429           *in28  = *on;
311100140430           *in36  = *on;
311200140506           V1Cmsg = D48msg;
311300140429           leavesr;
311400140429         endif;
311500140429
311600140429       ENDSR;
311700151130
311800151130       //--------------------------------------------------------------
311900151203       //?Controlli specifici per Motivo Apertura R.A. " 18"=Reso      ?
312000151130       //--------------------------------------------------------------
312100151130       BEGSR  sr_CtrMotAp_18;
312200151130
312300151201         Select;
312400151201           // -?Errore SE inserimento da Sede?
312500151201           When  *in05;
312600151201             *in28  = *on;
312700151201             *in36  = *on;
312800151201             V1Cmsg = Msg(53);
312900151201             leavesr;
313000151201           // -?Errore SE R.A. non per Spedizione?
313100151201           When  V1Ctor <> 'S';
313200151201             *in28  = *on;
313300151201             *in36  = *on;
313400151201             V1Cmsg = %subst( Msg(45) : 1 : 48 );
313500151201             leavesr;
313600151201         EndSl;
313700151130
313800151130         // -?Errore SE bolla EXPORT (LNA estera)?
313900151130         clear  xx;
314000151201         if  $FNARB = *on;
314100151201           xx = %lookup( %editc( ARBlna : 'X' ) : sk_FILest );
314200151201         else;
314300151201           xx = %lookup( %editc( BLPlna : 'X' ) : sk_FILest );
314400151201         endif;
314500151201         if  xx > *zero;
314600151130           *in28  = *on;
314700151130           *in36  = *on;
314800151130           V1Cmsg = Msg(55);
314900151130           leavesr;
315000151130         endif;
315100151130
315200151130         // -?Verifica se esiste già una disposiz. di Reso/Dirottamento?
315300151130         //  ?per la stessa spedizione?
315400151130         clear  FNLRY09ds2;
315500151204         iLRY09tch  = 'T';
315600151130         iLRY09aas  = V1annS;
315700151130         iLRY09lnp  = V1lnpS;
315800151130         iLRY09nrs  = V1nrsS;
315900151130         iLRY09nsp  = V1nspS;
316000151130         kpjbu = FNLRY09ds2;
316100151130         fnlrY09r ( kpjba );
316200151130         FNLRY09ds2 = kpjbu;
316300151130         If  oLRY09esdi = *on;
316400151130           *in28  = *on;
316500151130           *in36  = *on;
316600151130           V1Cmsg = Msg(56);
316700151204           if  oLRY09reso = *on;
316800151204             xx = %scan( 'Dirottamento' : V1Cmsg );
316900151204             V1Cmsg = %replace( 'Reso        ' : V1Cmsg : xx );
317000151204             V1Cmsg = %subst( V1Cmsg : 1 : xx + 4 ) +
317100151204                      %trimL( %subst( V1Cmsg : xx + 4 ) );
317200151130           endif;
317300151130           leavesr;
317400151130         EndIf;
317500151130
317600151130       ENDSR;
317700141210
317800141210       //--------------------------------------------------------------
317900141210       //?Ctrl specifici per Motivo Apertura R.A. " 60"=Verif. PINcode ?
318000141210       //--------------------------------------------------------------
318100141210       BEGSR  sr_CtrMotAp_60;
318200141210
318300141210         Select;
318400141210
318500141210           // -?Errore SE si è in Sede?
318600141210           When  *in05;
318700141210             *in28  = *on;
318800141210             *in36  = *on;
318900141210             V1Cmsg = Msg(42);
319000141210             leavesr;
319100141210
319200141212           // -?Errore SE utente NON abilitato alla modifica bolle fil.?
319300141212           When  §UTEmbf = *blank;
319400141210             *in28  = *on;
319500141210             *in36  = *on;
319600141210             V1Cmsg = Msg(27);
319700141210             leavesr;
319800141210
319900141210           // -?Errore SE la spedizione NON è in Arrivo?
320000141210           When  Not $FNARB;
320100141210             *in28  = *on;
320200141210             *in36  = *on;
320300141210             V1Cmsg = Msg(45);
320400141210             leavesr;
320500141210
320600141210         EndSl;
320700141210
320800141210
320900141210         // -?Errore SE la spedizione NON è in consegna oggi?
321000141212         exsr  ctr_SpeCons;
321100141212         if  $Cons = *off;
321200141210           *in28  = *on;
321300141210           *in36  = *on;
321400141210           V1Cmsg = Msg(45);
321500141210           leavesr;
321600141210         endif;
321700141210
321800141210         // -?Verifica SE la Particolarità Consegna prevede il PINcode?
321900141210         clear  ds7R;
322000141210         ds_TNTBE.TBEke1 = ARBgma;
322100141210         if  getTabella ( c_Tabel : '7R'  : ds_TNTBE.TBEke1 :
322200141210                          *omit : *omit : *omit :
322300141210                          *omit : *omit :
322400141210                          *omit : *omit : *omit : *omit :
322500141210                          *omit : *omit : *omit : *omit :
322600141210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
322700141210                        = *zero      AND
322800141210             ds_TNTBE.TBEatb = *blank;
322900141210           ds7R   = ds_TNTBE.TBEuni;
323000141210         endif;
323100141210         if  ds7R.§7RpinCode <> 'S';
323200141210           *in28  = *on;
323300141210           *in36  = *on;
323400141210           V1Cmsg = Msg(46);
323500141210           leavesr;
323600141210         endif;
323700141218
323800141218         // -?Verifica esistenza trk "Y" in FIAR4 per la spedizione?
323900141218         setll  ( V1annS : V1lnpS : V1nrsS : V1nspS : 'Y' )  FIAR4000;
324000141218         if  NOT %equal(FIAR401L);
324100141218           *in28  = *on;
324200141218           *in36  = *on;
324300141218           V1Cmsg = Msg(47);
324400141218           leavesr;
324500141218         endif;
324600141210
324700141210       ENDSR;
324800150717
324900150717       //--------------------------------------------------------------
325000150717       //?Ctrl specifici per Motivo Apertura R.A. " 70"=Reclamo da Sede?
325100150717       //--------------------------------------------------------------
325200150717       BEGSR  sr_CtrMotAp_70;
325300150717
325400150717         Select;
325500150717
325600150717           // -?Errore SE NON si è in Sede?
325700150717           When  Not *in05;
325800150717             *in28  = *on;
325900150717             *in36  = *on;
326000150717             V1Cmsg = Msg(51);
326100150717             leavesr;
326200150717
326300150717         EndSl;
326400150717
326500150717       ENDSR;
326600140429
326700140429      /end-free
326800150421
326900150421       //--------------------------------------------------------------
327000150421       //?Controllo Mail del cliente.                                  ?
327100150421       //--------------------------------------------------------------
327200150421     c     sr_CtrlMail   BEGSR
327300150421      *
327400150421     c                   clear                   DSEMAIL
327500150421     c                   movel     wwMail        §emlindi
327600150421     c                   call      'XEMAIL'
327700150421     c                   parm                    DSEMAIL
327800150421      * errore
327900150421    3c                   If        §emlerro = '1'
328000150421     C                   eval      *IN28 = *on
328100150421     C                   eval      *IN38 = *on
328200150421     c                   movel     §emlmsgo      V1Cmsg
328300150421     c                   leavesr
328400150421      *
328500150421     c                   else
328600150421     c                   movel     §emlindo      wwMail
328700150421    3c                   endif
328800150421      *
328900150421     c                   ENDSR
329000150421
329100150421       //--------------------------------------------------------------
329200150421       //?Controllo Motivo Apertura Reclamo.                           ?
329300150421       //--------------------------------------------------------------
329400150421     c     sr_CtrlMAD    BEGSR
329500150421      *
329600150422     c                   clear                   wwdMAD
329700150422      *
329800150421    2c                   If        wwcMAD <> *blanks
329900150421      *
330000150421     c                   clear                   tibs02ds
330100150421      *
330200150421      * Ricerca
330300150421     c     '?'           scan      wwcMAD
330400150421    3c                   if        %found
330500150421      *
330600150421     c                   clear                   tntbMARds1
330700150421      * - SE sono in IMMISSIONE: includo i soli motivi apertura utilizzabili
330800150421      *      dall'utente in APERTURA;
330900150421      *   SE sono in GESTIONE: includo sia i motivi apertura utilizzabili
331000150421      *      dall'utente in APERTURA, che quelli utilizzabili dall'utente
331100150421      *      in GESTIONE.
331200150421     c                   if        wMod <> 'I'
331300150421     c                   eval      iMARutUt = 'I'
331400150421     c                   endif
331500150421     c                   eval      iMARutAp = 'I'
331600150421     c                   movel(p)  tntbMARds1    KPJBU
331700150421      *
331800150421     c                   call      'TNTBMARR1'
331900150421     c                   parm                    KPJBA
332000150421      *
332100150421     c                   movel     KPJBU         tntbMARds1
332200150421    4c                   if        oMARfxx = *blank  and  oMARerr = *off
332300150421     c                   eval      wwcMAD  = oMARcod
332400150422     c                   eval      wwdMAD  = oMARdes
332500150421      *  ?Impostazione campi (se variati) ?
332600150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
332700150421      *    ?(SE variato il Motivo Apertura)?
332800150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
332900150421if  5c                   if        wMod    =  'I'       and
333000150421     c                             wwcMAD  <> wSaveMAD  and
333100150421     c                             $Video  <> 'W4'
333200150429     c                   eval      V1Cmad  =  wwcMAD
333300150429     c                   eval      wSaveMAD = V1Cmad
333400150421     c                   exsr      sr_Ctrl_MAR
333500150421     c                   eval      V1Ccdt   = §MARcauChP
333600150421e   5c                   endif
333700150421      *  errore (non trovato nulla pulisco il '?' )
333800150421     C                   ELSE
333900150421     c                   clear                   wwcMAD
334000150422     c                   clear                   wwdMAD
334100150421    4C                   ENDIF
334200150421      *
334300150421     c                   eval      *in90 = *on
334400150421     C                   eval      *IN36 = *on
334500150421     c                   leavesr
334600150421      *
334700150421   x3C                   ELSE
334800150421      *
334900150421      * Controllo
335000150421     c                   exsr      sr_Ctrl_MAR
335100150723     c                   if        *in28  or  *in90
335200150421     c                   leavesr
335300150421     c                   endif
335400150421      *  ?Impostazione campi (se variati) ?
335500150421      *   -?Causale di Chiusura (proposta) => dalla tab. "MAR" x Motivo Apertura?
335600150421      *    ?(SE variato il Motivo Apertura)?
335700150422      *    ?SE si viene da subfile-control "C01" (NON dalla window "W04")?
335800150421if  4c                   if        wMod    =  'I'       and
335900150421     c                             wwcMAD  <> wSaveMAD  and
336000150421     c                             $Video  <> 'W4'
336100150421     c                   eval      wSaveMAD = wwcMAD
336200150421     c                   exsr      sr_Ctrl_MAR
336300150421     c                   eval      V1Ccdt   = §MARcauChP
336400150421e   4c                   endif
336500150421      *
336600150421    3c                   endif
336700150421      *
336800150421      * Confronto del Motivo associato al Tipo Oggetto Reclamo
336900150421      *           con quello inserito a video
337000150422      *?- SE si viene da subfile-control "C01" può essere stato variato?
337100150422      *?  (NON se si viene dalla window "W04")?
337200150421if  3c                   if        §MARtor <> *blank  and  V1Ctor <> §MARtor
337300150421     c                   clear                   dOGR_S
337400150421     c                   eval      T02mod = 'C'
337500150421     c                   eval      T02sif = knsif
337600150421     c                   eval      T02cod = 'OGR'
337700150421     c                   eval      T02ke1 = §MARtor
337800150421     c                   call      'TIBS02R'
337900150421     c                   parm                    KPJBA
338000150421     c                   parm                    TIBS02ds
338100150421if  4c                   if        T02err = *blanks
338200150421     c                   movel     T02uni        dOGR_S
338300150421e   4c                   endif
338400150421      *
338500150421     c                   eval      *in28 = *on
338600150723     c*//                eval      *in90 = *on
338700150421     c                   eval      *in31 = *on
338800150421     c                   eval      V1Cmsg = %trim(Msg(39)) +
338900150421     c                                      ' "' + §MARtor + '" = ' +
339000150421     c                                      %trim(dOGR_S.§OGRdesc)
339100150421     c                   leavesr
339200150421e   3c                   endif
339300150421      *
339400150421   x2c                   Else
339500150421      *
339600150421      *  Campo Obbligatorio
339700150421     c                   eval      *in28 = *on
339800150723     C                   eval      *IN36 = *on
339900150421     c                   movel     MSG(8)        V1Cmsg
340000150421     c                   leavesr
340100150421      *
340200150421    2C                   EndIf
340300150421      *
340400150421      *?Controlli specifici per Motivo Apertura R.A. " 17"?
340500150421      *?(Fermo Deposito autorizza mittente)?
340600150421      *?SE non protetto?
340700150421     c                   If        wwcMAD = c_FDEAM
340800150421     c                             and  Not *in50
340900150421     c                             and  Not *in70  and  Not *in72
341000150421     c                             and  Not *in74
341100150421     c                   exsr      sr_CtrMotAp_17
341200150421     c                   if        *in28  or  *in90
341300150421     c                   leavesr
341400150421     c                   endif
341500150421     c                   EndIf
341600151130      *
341700151130      *?Controlli specifici per Motivo Apertura R.A. " 18"?
341800151130      *?(Reso)?
341900151130      *?SE  INSERIMENTO  e  non protetto?
342000151130     c                   If        wwcMAD = c_MotAp_Reso  and
342100151130     c                             Wmod   = 'I'      and
342200151130     c                             Not *in50         and
342300151130     c                             Not *in70         and  Not *in72
342400151130     c                                               and  Not *in74
342500151130     c                   exsr      sr_CtrMotAp_18
342600151130     c                   if        *in28  or  *in90
342700151130     c                   leavesr
342800151130     c                   endif
342900151130     c                   EndIf
343000150421      *
343100150421      *?Controlli specifici per Motivo Apertura R.A. " 60"?
343200150421      *?(Verifica telefonica del "PINcode")?
343300150421      *?SE  INSERIMENTO  e  non protetto?
343400150421     c                   If        wwcMAD = c_VPINC  and
343500150421     c                             Wmod   = 'I'      and
343600150421     c                             Not *in50         and
343700150421     c                             Not *in70         and  Not *in72
343800150421     c                                               and  Not *in74
343900150421     c                   exsr      sr_CtrMotAp_60
344000150421     c                   if        *in28  or  *in90
344100150421     c                   leavesr
344200150421     c                   endif
344300150421     c                   EndIf
344400150717      *
344500150717      *?Controlli specifici per Motivo Apertura R.A. " 70"?
344600150717      *?(Reclamo da Sede)?
344700150717      *?SE non protetto?
344800150717     c                   If        wwcMAD = c_ReclSede
344900150717     c                             and  Not *in70  and  Not *in72
345000150717     c                                             and  Not *in74
345100150717     c                   exsr      sr_CtrMotAp_70
345200150717     c                   if        *in28  or  *in90
345300150717     c                   leavesr
345400150717     c                   endif
345500150717     c                   EndIf
345600150421      *
345700150421     c                   ENDSR
345800150429
345900150616      /free
346000150616
346100150429       //--------------------------------------------------------------
346200150429       //?Controllo Reparto Gestione.                                  ?
346300150429       //--------------------------------------------------------------
346400150429       BEGSR  sr_CtrlRPR;
346500150429
346600150429         clear  wwdRPR;
346700150429
346800150429         Select;
346900150429
347000150429           When  %scan( '?' : wwcRPR ) > *zero;
347100150429             clear  wwcRPR;
347200150508             clear  tntbRGRds;
347300150508             iRGRopz = 1;
347400150508             kpjbu   = tntbRGRds;
347500150508             tntbRGRr ( KPJBA );
347600150508             tntbRGRds = kpjbu;
347700150508             If  oRGRerr = *off  and  oRGRfxx = *blank;
347800150508               wwcRPR = oRGRcod;
347900150508               wwdRPR = oRGRdes;
348000150508             endif;
348100150429             *in90 = *on;
348200150429             if  $Video = 'W4';
348300150429               *in45 = *on;
348400150429             else;
348500150429               *in39 = *on;
348600150429             endif;
348700150429             leavesr;
348800150429
348900150429           When  wwcRPR = *blank;
349000150429             *in28 = *on;
349100150429             if  $Video = 'W4';
349200150429               *in45 = *on;
349300150429             else;
349400150429               *in39 = *on;
349500150429             endif;
349600150616             if  *in05;
349700150616               V1Cmsg = Msg(20);
349800150616             else;
349900150616               V1Cmsg = Msg(50);
350000150616             endif;
350100150429             leavesr;
350200150429
350300150429           Other;
350400150429             clear  TIBS02ds;
350500150429             T02mod = 'C';
350600150429             T02sif = knsif;
350700150429             T02cod = 'RGR';
350800150429             T02ke1 = wwcRPR;
350900150429             TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
351000150429             if  T02err = *blank;
351100150429               wwdRPR = T02uni;
351200150429             else;
351300150429               *in28 = *on;
351400150429               if  $Video = 'W4';
351500150429                 *in45 = *on;
351600150429               else;
351700150429                 *in39 = *on;
351800150429               endif;
351900150616               if  *in05;
352000150616                 V1Cmsg = Msg(20);
352100150616               else;
352200150616                 V1Cmsg = Msg(50);
352300150616               endif;
352400150429               leavesr;
352500150429             endif;
352600150429
352700150429         EndSl;
352800150429
352900150429       ENDSR;
353000150616
353100150616       //--------------------------------------------------------------
353200150616       //?Controllo Utente Gestione.                                   ?
353300150616       //--------------------------------------------------------------
353400150616       BEGSR  sr_CtrlRPG;
353500150616
353600150616         clear  wwdRPR;
353700150616
353800150616         // -?Verifica se esiste utente nell'archivio profili?
353900150616         chain  ( wwcRPR )  AZUTE000;
354000150616
354100150616         If  %found(AZUTE01L)  and  UTEdsc > DateU;
354200150616
354300150616           // -?SE lo trova: verifica se è della filiale di gestione?
354400150723           if  %editc(UTEfil:'X') <> wwcPOG;
354500150616             *in28  = *on;
354600150616             *in46  = *on;
354700150616             V1Cmsg = Msg(23);
354800150616             leavesr;
354900150616           endif;
355000150616
355100150616         Else;
355200150616
355300150616           *in28 = *on;
355400150616           *in45 = *on;
355500150616           V1Cmsg = Msg(20);
355600150616           leavesr;
355700150616
355800150616         EndIf;
355900150616
356000150616       ENDSR;
356100150911
356200150911       //--------------------------------------------------------------
356300150911       //?Controllo C.A.P. cliente per reperimento Filiale Gestione    ?
356400150911       //?(per R.A. inerita in SEDE)                                   ?
356500150911       //--------------------------------------------------------------
356600150911       BEGSR  sr_CtrlCAP;
356700150911
356800150911         // -?Se CAP vuoto: uscita?
356900150914         if  W4cCap <> *blank  and  %trim( W4cCap ) = *zero;
357000150911           clear  W4cCap;
357100150911         endif;
357200150911
357300150911         if  W4cCap = *blank;
357400150911           leavesr;
357500150911         endif;
357600150915
357700150915         // -?Selezione CAP?
357800150915         if  %scan( '?' : W4cCap ) > *zero;
357900150915
358000150915           clear  W4cCap;
358100150915           clear  TISI00ds;
358200150915           D00op0 = 'L01';
358300150915           D00dri = DateU;
358400150915           clear  TISI30ds;
358500150915           I30flg = 'S';
358600150915           TISI30R ( kpjba : TISI00ds : TISI30ds );
358700150915           if  D00cap <> *blank;
358800150915             W4cCap = D00cap;
358900150915           endif;
359000150915
359100150915         endif;
359200150911
359300150911         // -?Reperimento filiale (LNA) dal CAP?
359400150911         clear  TISI95ds;
359500150911         I95tcn = '3';
359600150911         I95cap = W4cCap;
359700150911         I95dat = DateU;
359800150911
359900150911         TISI95R ( TISI95ds );
360000150911
360100150914         If  O95err = *blank;
360200150914
360300150914           if  W4cPog <> %editc( O95lna : 'X' );
360400150914             *in90 = *on;
360500150914           endif;
360600150914           W4cPog = %editc( O95lna : 'X' );
360700150914           //W4cCap = O95cap;
360800150914           clear  W4cCap;
360900150915
361000150915           reset  FNLV55ds;
361100150915           //D55tla = 'L';
361200150915           //D55tpt = 'P';
361300150915           D55lin = O95lna;
361400150915           D55drf = DateU;
361500150915           fnlv55r ( FNLV55ds );
361600150915           if  D55err = *blank;
361700150915             W4cPog = %editc( D55tfp : 'X' );
361800150915           endif;
361900150914
362000150914         Else;
362100150914
362200150914           *in28 = *on;
362300150914           *in56 = *on;
362400150914           V1Cmsg = Msg(54);
362500150914           leavesr;
362600150914
362700150914         EndIf;
362800150911
362900150911       ENDSR;
363000150616
363100150616      /end-free
363200150421
363300150421       //--------------------------------------------------------------
363400150421       //?Controllo Filiale Gestione.                                  ?
363500150421       //--------------------------------------------------------------
363600150421     c     sr_CtrlPOG    BEGSR
363700150421      *
363800150422     c                   clear                   wwdPOG
363900150422      *
364000150421     c                   IF        %scan('?':wwcPOG) > 0
364100150421     c                   clear                   dstnsd
364200150421     c                   call      'TNSD24R'
364300150421     c                   parm                    §cod
364400150421     c                   parm                    §tip
364500150421     c                   parm                    §des
364600150421     c                   parm                    §pra
364700150421     c                   movel     §cod          wwcPOG
364800150422     c                   movel     §des          wwdPOG
364900150421     c                   eval      *in90 = *on
365000150421     c                   eval      *in46 = *on
365100150421     c                   leavesr
365200150421     c                   ENDIF
365300150421
365400150421     c                   IF        wwcPOG = *blanks
365500150421     c                   eval      *in28  = *on
365600150421     c                   eval      *in46  = *on
365700150421     c                   eval      V1Cmsg = Msg(48)
365800150421     c                   leavesr
365900150421     c                   ENDIF
366000150421
366100150421     c                   eval      xx = 1
366200150421     c                   FOR       xx by 1 to %len(wwcPOG)
366300150421     c                   IF        %subst(wwcPOG:xx:1) <> *blanks and
366400150421     c                             %check(Digits:%subst(wwcPOG:xx:1)) > *zeros
366500150421     c                   eval      *in28 = *on
366600150421     c                   eval      *in46 = *on
366700150421     c                   eval      V1Cmsg = Msg(48)
366800150421     c                   leavesr
366900150421     c                   ENDIF
367000150421     c                   ENDFOR
367100150421
367200150421     c                   move      wwcPOG        w0030
367300150421     c     w0030         chain     AZORG01L
367400150717     c                   IF        not %found(AZORG01L)  OR
367500150717     c                              ORGfva <> *blanks    OR
367600150428     c                             (wwcPOG <> c_Sede  and
367700150717     c                              ORGfag <> 'F'     and
367800150723     c                              ORGfag <> 'A')
367900150723     c*//                                                OR
368000150723     c*//                          (wwcMAD =  c_ReclSede  and
368100150723     c*//                           wwcPOG =  c_Sede)
368200150421     c                   eval      *in28 = *on
368300150421     c                   eval      *in46 = *on
368400150421     c                   eval      V1Cmsg = Msg(48)
368500150421     c                   leavesr
368600150421     c                   ENDIF
368700150422     c                   movel     ORGdes        wwdPOG
368800150428      *?Filiale Gestione 046 inseribile (in modifica) SOLO da utente di Sede?
368900150428     c                   If        wMod <> 'I'      and
369000150428     c                             wwcPOG = c_Sede  and  SIMfel <> *zero
369100150429      * - prima verifica che non sia stata richiesta l'interrogazione
369200150429     c                   eval      wwcRPR = V1Crpg
369300150429     c                   exsr      sr_CtrlRPR
369400150429     c                   eval      V1Crpg = wwcRPR
369500150429     c                   eval      *in28 = *off
369600150429     c                   eval      *in45 = *off
369700150429     c                   eval      *in39 = *off
369800150429     c                   clear                   V1Cmsg
369900150428     c                   eval      w_Ute  = V1Crpg
370000150428     c                   exsr      sr_TabUGR
370100150428     c                   if        §UGRpofo <> %int(c_Sede)
370200150428     c                   eval      *in28 = *on
370300150428     c                   eval      *in46 = *on
370400150428     c                   eval      V1Cmsg = Msg(48)
370500150428     c                   leavesr
370600150428     c                   endif
370700150428     c                   EndIf
370800150421      *
370900150421     c                   ENDSR
371000150428
371100150428      /free
371200150428
371300150428       //--------------------------------------------------------------
371400150428       //?Reperimento dati Tab. "UGR" (Utente Gestione Reclami)        ?
371500150428       //--------------------------------------------------------------
371600150428       BEGSR  sr_TabUGR;
371700150428
371800150428         clear  dUGR;
371900150428         clear  TIBS02ds;
372000150428         T02mod = 'C';
372100150428         T02sif = KNSIF;
372200150428         T02cod = 'UGR';
372300150428         T02ke1 = w_Ute;
372400150428
372500150428         TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
372600150428
372700150428         if  T02err = *blank;
372800150428           dUGR = T02uni;
372900150428         endif;
373000150428
373100150428       ENDSR;
373200150428
373300150428      /end-free
373400140429
373500070430      *---------------------------------------------------------------*
373600070430      * Controlli parte alta del ctl per f4 fine richiesta            *
373700070430      *---------------------------------------------------------------*
373800070430     c     SR_CONTRF4    BegSR
373900070430
374000070430    1c                   do        *hival
374100070430      * se dato F4 e non è stato inserito nessun oggetto e non lo sto inserendo adesso ERRORE
374200070430    2c                   if        s01nrr = 0  and v1ctor = *blanks and
374300070430     c                             v1cogg = *blanks and ormvid = *zeros and
374400070430     c                             spevid = *zeros and v1ctch = *blanks and
374500070430     c                             v1cmad = *blanks and v1ccdt = *blanks and
374600070430     c                             v1not1 = *blanks and v1not2 = *blanks and
374700070430     c                             v1not3 = *blanks and v1not4 = *blanks and
374800141218     c
374900070430     c                             v1crsp = *blanks and v1cpor = *blanks
375000070430     c                   eval      *in28 = *on
375100070430     c                   movel     MSG(14)       V1Cmsg
375200070430     C                   eval      *IN44 = *on
375300070430     c                   leave
375400070430    2c                   endif
375500070430      * verifco se inserito  il codice chiamante
375600070430    2c                   If        v1cchi = *zeros and wforz = ' '
375700070430      * richamo la routine di ricerca codice nel dettaglio oggetti
375800070430     c                   exsr      SR_RECCHI
375900070430      * controllo se ne ho uno da proporre
376000070430     c                   select
376100070430     c                   when      wrk_cchiA > 0
376200070430     c                   eval      wrk_cchi = wrk_cchiA
376300070430     c                   when      wrk_cchiB > 0
376400070430     c                   eval      wrk_cchi = wrk_cchiB
376500070430     c                   other
376600070430     c                   eval      wrk_cchi = wrk_cchiC
376700070430     c                   endsl
376800070430      *
376900070430    3c                   if        wrk_cchi > *zeros
377000070430     c                   eval      v1cchi = wrk_cchi
377100070430     c                   eval      SW_v1cchi = wrk_cchi
377200070430     c                   clear                   wrk_cchi
377300070430      * se ragione sociale a blank la decodifico
377400070430     C                   clear                   TIBS69DS
377500070430     c                   z-add     v1cchi        I69kac
377600070430     c                   z-add     v1cchi        I69kcp
377700070430     C                   call      'TIBS69R'
377800070430     C                   parm                    tibs69DS
377900070430     C                   parm                    DS_cnaco
378000070430     C                   parm                    DS_cnind
378100070430     C                   parm                    DS_cnclp
378200070430     C                   parm                    DS_fncls
378300070430     C                   eval      wtibs69r = '1'
378400070430    4c                   If         O69ERR =  ' '
378500070430     c                   movel     ACOrag        v1dchi
378600070430     c                   movel     clpclv        v1cclv
378700070430    4c                   Endif
378800070430      *
378900070430   x3c                   else
379000070430      *
379100070430     c                   eval      wforz = '1'
379200070430     c                   eval      *in28 = *on
379300070430     c                   movel     MSG(32)       V1Cmsg
379400070430     C                   eval      *IN10 = *on
379500070430     c                   leave
379600070430    3c                   endif
379700070430    2c                   endif
379800070430
379900070430      * controllo del chiamante se uguale a blanks ma codice cliente valorizzato non faccio nulla
380000070430
380100070430    2c                   If        v1crsc = *blanks and v1dchi = *blanks
380200070430     c                   eval      *in28 = *on
380300070430     c                   movel     MSG(15)       V1Cmsg
380400070430     C                   eval      *IN44 = *on
380500070430     c                   leave
380600070430      * Se anche il chiamante è a blank imposto la ragione sociale del cliente
380700070430     c                   else
380800070430    3c                   if        v1dchi <> *blanks AND v1crsc = *blanks
380900070430     c                   eval      v1crsc = v1dchi
381000070430    3c                   endif
381100070430    2c                   endif
381200070430      *
381300070503     c                   leave
381400070430     c                   enddo
381500070430      *
381600070430     c                   ENDSR
381700070430      *
381800070430      *---------------------------------------------------------------*
381900070430      * Gestione interrogazione oggetto                               *
382000070430      *---------------------------------------------------------------*
382100070430     c     SR_GESF2      BegSR
382200070430
382300070430    1c                   do        *hival
382400070430      * interrogazione Spedizione
382500070430      *
382600070430    2c                   if        v1ctor = 'S'
382700131119      * se non sono in sede
382800070430    3c                   if        not *in05
382900070430      *
383000070430     c     KSPED         chain     Fnarb01l
383100070430      * spedizione in arrivo
383200070430    4c                   If        %found(Fnarb01l)
383300070430      * verifico se programm
383400070430
383500070430    5c                   If        *in50
383600070430     c                   setoff                                       75
383700070430     c     'FNLR36R   '  lookup    £Stack                                 75
383800150615     c  n75'FNLS05R   '  lookup    £Stack                                 75
383900070430
384000070430      * se trovato vado fuori dal programma
384100070430    6c                   if        %found
384200070430     c                   eval      *in90 = *on
384300070430     c                   eval      *in31 = *on
384400070430     c                   leave
384500070430    6c                   endif
384600070430      *
384700070430    5c                   endif
384800070430      *
384900070430     c                   clear                   DS_ricbolA
385000070430     C                   z-add     V1anns        DSRBA_aas
385100070430     C                   z-add     V1lnps        DSRBA_lnp
385200070430     C                   z-add     V1nrss        DSRBA_nrs
385300070430     C                   z-add     V1nsps        DSRBA_nsp
385400070430     C                   movel     1             DSRBA_flg
385500070430     C                   movel(P)  DS_ricbolA    KPJBU
385600070430      * se richiamato
385700070430     c                   clear                   fidna1ds
385800150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
385900150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene ..
386000150601     c
386100150601    5c                   if        *in50
386200150601      *
386300150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
386400150601      * oggetto ho più di una RA
386500150601     c                   exsr      Sr_countRA
386600150601      *
386700150601     C                   ELSE
386800150601     c                   eval      IA1INS = 'I'
386900150601    5c                   endif
387000070430     C                   call      'FNLR36R'
387100070430     C                   parm                    KPJBA
387200070430     C                   parm                    FIDNA1DS
387300070430      *
387400070430
387500070430     c                   eval      *in90 = *on
387600070430     C                   eval      *IN31 = *on
387700070430     c                   leave
387800070430
387900070430    4C                   endif
388000070430      *
388100070430     c     KSPED         chain     Fnblp01l
388200070430      * spedizione in partenza
388300070430    4c                   If        %found(Fnblp01l)
388400070430      * verifico se programma richiamato dal PGM interrogazione  Bolle Partenza
388500070430
388600070430    5c                   If        *in50
388700070430     c                   setoff                                       75
388800070430     c     'FNLS05R   '  lookup    £Stack                                 75
388900150615     c  n75'FNLR36R   '  lookup    £Stack                                 75
389000070430      * se trovato vado fuori dal programma
389100070430    6c                   if        %found
389200070430     c                   eval      *in90 = *on
389300070430     c                   eval      *in31 = *on
389400070430     c                   leave
389500070430    6c                   endif
389600070430      *
389700070430    5c                   endif
389800070430      *
389900070430     c                   clear                   DSVBP_F03
390000070430     C                   z-add     V1anns        DSVBP_aas
390100070430     C                   z-add     V1lnps        DSVBP_lnp
390200070430     C                   z-add     V1nrss        DSVBP_nrs
390300070430     C                   z-add     V1nsps        DSVBP_nsp
390400070430     c                   movel(P)  RSUT          DSVBP_rsut
390500070430     C                   z-add     simfel        DSVBP_sim
390600070430     C                   movel(P)  DS_visbolp    KPJBU
390700070430      * se richiamato
390800070430     c                   clear                   fidna1ds
390900150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
391000150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
391100150601    5c                   if        *in50
391200150601      *
391300150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
391400150601      * oggetto ho più di una RA
391500150601     c                   exsr      Sr_countRA
391600150601      *
391700150601     c                   else
391800150601     c                   eval      IA1INS = 'I'
391900150601    5c                   endif
392000070430      *
392100070430     C                   call      'FNLS05R'
392200070430     C                   parm                    KPJBA
392300070430     C                   parm                    fidna1ds
392400070430      *
392500070430     c                   eval      *in90 = *on
392600070430     C                   eval      *IN31 = *on
392700070430     c                   leave
392800070430
392900070430    4C                   endif
393000070430      *
393100070430      * sono in sede
393200070430      *
393300070430   x3c                   else
393400070430      *
393500070430     c     KSPED         chain     Titas30c
393600070430      * spedizione in sede
393700070430    4c                   If        %found(Titas30c)
393800070430      * verifico se programm
393900070430
394000070430    5c                   If        *in50
394100070430     c                   setoff                                       75
394200070430     c     'TNSB50R   '  lookup    £Stack                                 75
394300070430
394400070430      * se trovato vado fuori dal programma
394500070430    6c                   if        %found
394600070430     c                   eval      *in90 = *on
394700070430     c                   eval      *in31 = *on
394800070430     c                   leave
394900070430    6c                   endif
395000120330      * se non trovo il TNSB50R faccio un tentativo anche con il TNSB51R
395100120330     c                   setoff                                       75
395200120330     c     'TNSB51R   '  lookup    £Stack                                 75
395300120330
395400120330      * se trovato vado fuori dal programma
395500120330    6c                   if        %found
395600120330     c                   eval      *in90 = *on
395700120330     c                   eval      *in31 = *on
395800120330     c                   leave
395900120330    6c                   endif
396000070430      *
396100070430    5c                   endif
396200070430      *
396300070430     c                   clear                   TNSB50DS
396400070430     C                   movel     'R05'         I50OP0
396500070430     C                   z-add     V1anns        D50aas
396600070430     C                   z-add     V1lnps        D50lnp
396700070430     C                   z-add     V1nrss        D50nrs
396800070430     C                   z-add     V1nsps        D50nsp
396900070430     C                   movel(P)  TNSB50DS      KPJBU
397000070430      *
397100070430      * se richiamato
397200070430     c                   clear                   fidna1ds
397300150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
397400150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
397500150601    5c                   if        *in50
397600150601      *
397700150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
397800150601      * oggetto ho più di una RA
397900150601     c                   exsr      Sr_countRA
398000150601      *
398100150601     C                   ELSE
398200150601     c                   eval      IA1INS = 'I'
398300150601    5c                   endif
398400070430      *
398500070430     C                   call      'TNSB50R'
398600070430     C                   parm                    KPJBA
398700070430     C                   parm                    FIDNA1DS
398800070430      *
398900070430
399000070430     c                   eval      *in90 = *on
399100070430     C                   eval      *IN31 = *on
399200070430     c                   leave
399300070430
399400070430    4C                   endif
399500070430
399600070430    3c                   endif
399700070430
399800070430    2c                   endif
399900070430      *
400000070430      * interrogazione ORM
400100070430      * interrogo direttamente con il richiano di FIOR07R
400200070430      *
400300070430    2c                   if        v1ctor = 'O'
400400070430      * verifico se programma richiamato dal PGM interrogazione  ORM
400500070430
400600070430    3c                   If        *in50
400700070430     c                   setoff                                       75
400800070430     c     'FIOR07R   '  lookup    £Stack                                 75
400900070430      * se trovato vado fuori dal programma
401000070430    4c                   if        %found
401100070430     c                   eval      *in90 = *on
401200070430     c                   eval      *in31 = *on
401300070430     c                   leave
401400070430    4c                   endif
401500070430      *
401600070430    3c                   endif
401700070430      *
401800070430     C                   clear                   PARM01
401900070430     C                   move      v1poeo        ppoe
402000070430     C                   move      v1nsor        pnsr
402100070430     C                   move      v1norm        pnor
402200070430     C                   move      v1nrvo        pnrv
402300070430     C                   move      '5'           psce
402400070430      * attenzione non muovo nulla nel campo del p.o. gestione .... vediamo se funziona !!!!!!
402500070430     C                   movel(p)  PARM01        KPJBU
402600070430      * se richiamato
402700070430     c                   clear                   fidna1ds
402800150601      * dal 2004 ad oggi abbiamo sempre impedito la visualizzaizone del F16 se vengo da gestione
402900150601      * RA ... non ci ricordiamo il perchè ma proviamo a vedere se può andare bene
403000150601    3c                   if        *in50
403100150601      *
403200150601      * attivo il richiamo del tasto F16 nei programmi chiamanti solo se a fronte dello stesso
403300150601      * oggetto ho più di una RA
403400150601     c                   exsr      Sr_countRA
403500150601      *
403600150601     c                   else
403700150601     c                   eval      IA1INS = 'I'
403800150601    3c                   endif
403900070430      *
404000070430     C                   call      'FIOR07R'
404100070430     C                   parm                    KPJBA
404200070430     C                   parm                    FIDNA1DS
404300070430
404400070430      *
404500070430     c                   eval      *in90 = *on
404600070430     C                   eval      *IN31 = *on
404700070430     c                   leave
404800070430
404900070430    2c                   endif
405000070430
405100070430    1c                   enddo
405200070430
405300070430     c                   endsr
405400070430      *
405500070323      *---------------------------------------------------------------*
405600070323      * Routine gestione richiesta data consegna richiesta            *
405700070323      *---------------------------------------------------------------*
405800070323     c     SR_GES_DTA    BegSR
405900070323      *
406000070416      * se si tratta di una spedizione e non è stata consegnata
406100070327     c                   if        v1ctor = 'S' and arbdcm = *zeros
406200070323      * verifico se sono la filiale di arrivo della bolla
406300130611     c                   move      arblna        w003a
406400130610     c     w003a         lookup    wPOG                                   27
406500070326     c                   If        %found
406600070326      * verifico se si tratta di :
406700070326      *                            lasciato avviso
406800070326      *                            contrassegno
406900070326      *                            supermercato
407000070326      *                            appuntamento
407100070326      *                            fermo deposito
407200070327      *
407300070430      * richiamo il pgm dei blocchi FNLRG6R
407400070430     c                   clear                   FNLRG6DS
407500070430     c                   eval      ilrg6tla = 'L'
407600070430     c                   eval      ilrg6aas = v1anns
407700070430     c                   eval      ilrg6lnp = v1lnps
407800070430     c                   eval      ilrg6nrs = v1nrss
407900070430     c                   eval      ilrg6nsp = v1nsps
408000070430     c                   call      'FNLRG6R'
408100070430     c                   parm                    fnlrg6ds
408200070430     c                   movel     olrg6abi      dlrg6abi
408300070430     c
408400070327     c                   select
408500070326      * Lasciato Avviso
408600070430     c                   when      v1cmad = W_lav
408700070327      * verifico se si tratta di spedizione bloccata
408800070430     c                   If        d§lrg6tla <> ' '
408900070327     c                   eval      W_cau = 'L'
409000070327     c                   exsr      Sr_ges_bloc
409100070327     c                   else
409200070329      * verifico se la spedizione non ha blocchi di altro tipo
409300070507     c                   if        d§lrg6tca = ' ' and d§lrg6tsu = ' '
409400070507     c                             and d§lrg6tap = ' ' and d§lrg6tfd = ' '
409500070329      * se la spedizione non è bloccata per nessun altro motivo
409600070330      * accendo indicatore di emissione della data consegna richiesta
409700070412     c                   seton                                        616290
409800070329      * gestione della videata con immissione data consegna richiesta + richiamo fnlr48r
409900070329     c                   eval      $riemetti = *on
410000070411      * valorizzo il flag per non essere richiamato di nuovo
410100070411     c                   eval      $ric_fnlrg2r = *on
410200070329     c                   endif
410300070327
410400070327     c                   endif
410500070507     c                   leavesr
410600070327
410700070502      * Contrassegno   e spedizione bloccata per c/assegno
410800070502     c                   when      v1cmad = W_con  and d§lrg6tca <> ' '
410900070327     c                   eval      W_cau = 'C'
411000070327     c                   exsr      Sr_ges_bloc
411100070507     c                   leavesr
411200070327
411300070502      * Supermercato   e spediizone bloccata per supermercato
411400070507     c                   when      v1cmad = W_sup  and d§lrg6tsu <> ' '
411500070327     c                   eval      W_cau = 'S'
411600070327     c                   exsr      Sr_ges_bloc
411700070507     c                   leavesr
411800070327
411900070502      * Appuntamento   e la spedizione è bloccata per appuntamento
412000070507     c                   when      v1cmad = W_app  and d§lrg6tap <> ' '
412100070327     c                   eval      W_cau = 'A'
412200070327     c                   exsr      Sr_ges_bloc
412300070507     c                   leavesr
412400070502
412500070502      * Fermo deposito e la spedizione è bloccata per fermo deposito
412600070502     c                   when      v1cmad = W_fde  and d§lrg6tfd <> ' '
412700070327     c                   eval      W_cau = 'F'
412800070327     c                   exsr      Sr_ges_bloc
412900070327
413000070327     c                   endsl
413100070327
413200070327     c                   endif
413300070327
413400070327     c                   endif
413500070327
413600070323     c                   Endsr
413700070327      *---------------------------------------------------------------*
413800070327      * Routine gestione sblocchi spedizioni                          *
413900070327      *---------------------------------------------------------------*
414000070327     c     SR_GES_BLOC   BegSR
414100070411      *
414200070411      * valorizzo il flag per non essere richiamato di nuovo
414300070411     c                   eval      $ric_fnlrg2r = *on
414400070327      *
414500070327     c                   clear                   Fnlrg2ds
414600070327     c                   eval      ilrg2tla = 'L'
414700070327     c                   eval      ilrg2tch = 'R'
414800070327     c                   eval      ilrg2aas = v1anns
414900070327     c                   eval      ilrg2lnp = v1lnps
415000070327     c                   eval      ilrg2nrs = v1nrss
415100070327     c                   eval      ilrg2nsp = v1nsps
415200070327     c                   eval      ilrg2cau = w_cau
415300070327      * se le note a video sono valorizzate
415400070327     c                   clear                   nov
415500070327     c                   eval      nov(1) = v1not1
415600070327     c                   eval      nov(2) = v1not2
415700070327     c                   eval      nov(3) = v1not3
415800070327     c                   eval      nov(4) = v1not4
415900070327      *
416000070327     c                   movea     nov           ilrg2no1
416100070327
416200070327     c                   call      'FNLRG2R'
416300070327     c                   Parm                    kpjba
416400070327     c                   Parm                    Fnlrg2ds
416500070327
416600070327      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
416700070327     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
416800070327      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
416900070403     c                   eval      $bloccof3    = *on
417000070327      * scrittura delle note
417100070327     c                   exsr      Sr_wrt_note
417200070503      * verifico se ci sono altri blocchi per questa spedizione gestibili da R.A.
417300070503     c                   exsr      Sr_piublocchi
417400070507      * recupero le note
417500070507      *
417600070507     C                   clear                   FIDNB0DS
417700070507     c                   z-add     v1cant        iB0ant
417800070507     c                   z-add     v1cnut        iB0nut
417900070507     c                   z-add     v1cpdt        iB0pdt
418000070507     c                   movel     'GEN'         iB0tIN
418100070507     c                   movel     'R'           iB0flm
418200070507      *
418300070507     C                   CALL      'FIDNB0R'
418400070507     C                   PARM                    kpjba
418500070507     C                   PARM                    FIDNB0DS
418600070507      *
418700070507      * valorizzo le prime 4 note a video
418800070507      *
418900070507     c                   movel     DB0NO1        v1not1
419000070507     c                   movel     DB0NO2        v1not2
419100070507     c                   movel     DB0NO3        v1not3
419200070507     c                   movel     DB0NO4        v1not4
419300070508      * se ob0piu è valorizzato ci sono + di 4 righe
419400070508     c                   eval      *in29 = (OB0PIU = '+')
419500070327      * proteggo i campi e riemetto la videata
419600070418     c                   seton                                        6074
419700070403     c                   eval      $riemetti = *on
419800070327     c                   endif
419900070418
420000070418     c                   eval      *in37 = *on
420100070327
420200070327     c                   endsr
420300070327      *---------------------------------------------------------------*
420400070327      * Routine gestione note che vengono dalla gestione blocchi      *
420500070327      *---------------------------------------------------------------*
420600070327     c     SR_wrt_note   BegSR
420700070412      *
420800070412     c                   clear                   nora
420900070412     c                   clear                   nob
421000070413     c                   clear                   x
421100070413     c                   clear                   y
421200070327      *
421300070327     c                   movea     olrg2no1      nob(1)
421400070327     c                   movea     olrg2no2      nob(9)
421500070327      * tolgo le note a blank
421600070327     c                   do        12            x
421700070327     c                   if        nob(x) <> *blanks
421800070327     c                   add       1             y
421900070327     c                   move      nob(x)        nora(Y)
422000070327     c                   endif
422100070327     c                   enddo
422200070328      * accendo indicatore che mi segnale che ci sono altre note se ne ho + di 4
422300070328     c                   eval      *in29 = y > 4
422400070328      * registro le note sul file note
422500070328    1c                   if        y > *zeros
422600070328     c                   clear                   FIDNB0DS
422700070328     c                   eval      db0no1 = nora(1)
422800070328     c                   eval      db0no2 = nora(2)
422900070328     c                   eval      db0no3 = nora(3)
423000070328     c                   eval      db0no4 = nora(4)
423100070508
423200070508     c                   exsr      SR_add_note
423300070508
423400070328      * verifico se ho altre note da scrivere
423500070328    2c                   if        nora(5) <> *blanks
423600070328     c                   clear                   fidnb0ds
423700070328     c                   eval      db0no1 = nora(5)
423800070328     c                   eval      db0no2 = nora(6)
423900070328     c                   eval      db0no3 = nora(7)
424000070328     c                   eval      db0no4 = nora(8)
424100070328
424200070508     c                   exsr      SR_add_note
424300070328
424400070328    2c                   endif
424500070328      * verifico se ho altre note da scrivere
424600070328    2c                   if        nora(9) <> *blanks
424700070328     c                   clear                   fidnb0ds
424800070328     c                   eval      db0no1 = nora(9)
424900070328     c                   eval      db0no2 = nora(10)
425000070328     c                   eval      db0no3 = nora(11)
425100070328     c                   eval      db0no4 = nora(12)
425200070328
425300070508     c                   exsr      SR_add_note
425400070508
425500070328    2c                   endif
425600070328
425700070328    2c                   endif
425800070328
425900070328
426000070327     c                   endsr
426100070328      *
426200070508      *---------------------------------------------------------------*
426300070508      * Routine che aggiunge note al reclamo esistente                *
426400070508      *---------------------------------------------------------------*
426500070508     c     SR_add_note   BegSR
426600070508
426700070508     c                   z-add     v1cant        iB0ant
426800070508     c                   z-add     v1cnut        iB0nut
426900070508     c                   z-add     v1cpdt        iB0pdt
427000070508     c                   z-add     999           iB0pgn
427100070508     c                   movel     'GEN'         iB0tIN
427200070508     c                   eval      ib0pot =v1cpot
427300070508     c                   eval      db0pon =Dutpou
427400070508     c                   eval      db0utn =knmus
427500070508      * imposto 'W'
427600070508     c                   movel     'W'           iB0flm
427700070508      *
427800070508     c                   call      'FIDNB0R'
427900070508     c                   parm                    kpjba
428000070508     c                   parm                    fidnb0ds
428100070508
428200070508      *
428300070508     c                   endsr
428400070503      *---------------------------------------------------------------*
428500070503      * Routine gestione eventuali blocchi in spediizione             *
428600070503      *---------------------------------------------------------------*
428700070503     c     SR_piublocchi BegSR
428800070503      * salvo il precedente blocco
428900070503     c                   eval      sw_cau = W_cau
429000070508     c                   clear                   note_blocco
429100070503      * richiamo il pgm dei blocchi FNLRG6R
429200070503     c                   clear                   FNLRG6DS
429300070503     c                   eval      ilrg6tla = 'L'
429400070503     c                   eval      ilrg6aas = v1anns
429500070503     c                   eval      ilrg6lnp = v1lnps
429600070503     c                   eval      ilrg6nrs = v1nrss
429700070503     c                   eval      ilrg6nsp = v1nsps
429800070503     c                   call      'FNLRG6R'
429900070503     c                   parm                    fnlrg6ds
430000070503     c                   movel     olrg6abi      dlrg6abi
430100070503      * verifico se ci sono ulteriori blocchi a quello gestito in precedenza
430200070503     c                   select
430300070507      * C/assegno
430400070503     c                   when      d§lrg6tca <> ' ' and sw_cau <> 'C'
430500070503     c                   eval      w_cau = 'C'
430600070508     c                   eval      note_blocco = 'Abilitazione : C/assegno'
430700070503     c                   exsr      sr_ges_bloc2
430800070507      * supermercato
430900070507     c                   when      d§lrg6tsu <> ' ' and sw_cau <> 'S'
431000070507     c                   eval      w_cau = 'S'
431100070508     c                   eval      note_blocco = 'Abilitazione : Supermercato'
431200070507     c                   exsr      sr_ges_bloc2
431300070507      * appuntamento
431400070507     c                   when      d§lrg6tap <> ' ' and sw_cau <> 'A'
431500070507     c                   eval      w_cau = 'A'
431600070508     c                   eval      note_blocco = 'Abilitazione : Appuntamento'
431700070507     c                   exsr      sr_ges_bloc2
431800070507      * fermo deposito
431900070507     c                   when      d§lrg6tfd <> ' ' and sw_cau <> 'F'
432000070507     c                   eval      w_cau = 'F'
432100070508     c                   eval      note_blocco = 'Abilitazione : Fermo Deposito'
432200070507     c                   exsr      sr_ges_bloc2
432300070507      * lasciato avviso
432400070507     c                   when      d§lrg6tla <> ' ' and sw_cau <> 'L'
432500070507     c                   eval      w_cau = 'L'
432600070508     c                   eval      note_blocco= 'Abilitazione : Lasciato Avviso'
432700070507     c                   exsr      sr_ges_bloc2
432800070507
432900070507     c                   endsl
433000070507
433100070503     c                   endsr
433200070503      *
433300070503      *---------------------------------------------------------------*
433400070503      * Routine gestione blocco come seconda o terza chiamata         *
433500070503      *---------------------------------------------------------------*
433600070503     c     SR_ges_bloc2  BegSR
433700070503      *
433800070503     c                   clear                   Fnlrg2ds
433900070503     c                   eval      ilrg2tla = 'L'
434000070503     c                   eval      ilrg2tch = 'R'
434100070503     c                   eval      ilrg2aas = v1anns
434200070503     c                   eval      ilrg2lnp = v1lnps
434300070503     c                   eval      ilrg2nrs = v1nrss
434400070503     c                   eval      ilrg2nsp = v1nsps
434500070503     c                   eval      ilrg2cau = w_cau
434600070503
434700070503     c                   call      'FNLRG2R'
434800070503     c                   Parm                    kpjba
434900070503     c                   Parm                    Fnlrg2ds
435000070503
435100070503      * verifico dalle note relative alle modifiche se è stato fatto qualcosa
435200070503     c                   if        olrg2no1 <> *blanks or olrg2no2 <> *blanks
435300070503      * scrivo le note e segnalo il fatto che sono già passata al pgm dei blocchi
435400070503     c                   eval      $bloccof3    = *on
435500070508      * prima di scrivere le note  scrivo il motivo del 2° blocco
435600070508     c                   clear                   FIDNB0DS
435700070508     c                   eval      db0no1 = note_blocco
435800070508     c                   exsr      sr_add_note
435900070503      * scrittura delle note
436000070503     c                   exsr      Sr_wrt_note
436100070503     c                   endif
436200070503
436300070503     c                   endsr
436400131206      *
436500131206      *---------------------------------------------------------------*
436600131206      * Controllo se la spedizione è in consegna                      *
436700131206      *---------------------------------------------------------------*
436800131206     c     CTR_specons   BegSR
436900131206      *
437000140204     c                   setoff                                       16
437100131206     c                   eval      $cons = *off
437200131206      * Aggancio bolle arrivi
437300131206     c     KSPED         chain     Fnarb01l
437400131206     c                   If        %found(Fnarb01l)
437500131206      * se si tratta di una spedizione e non è stata consegnata
437600131206     c                   If        ARBfbc = *blanks
437700131206     c                             and  ARBddc > *zeros
437800131206     c                             and  ARBndc > *zeros
437900131206     c                             and (ARBdcm = *zero    or
438000131206     c                                  ARBica = *blanks  or  ARBica = 'R'
438100131206     c                              or  ARBicc = *blanks  or  ARBicc = 'R')
438200131206     c                   eval      $cons = *on
438300131206     c                   Endif
438400070328      *
438500131206     c                   Endif
438600140123      *
438700131206     c                   endsr
438800131206      *
438900040212      *---------------------------------------------------------------*
439000040212      * Routine conferma riga di dettaglio reclamo                    *
439100040212      *---------------------------------------------------------------*
439200040218     c     SR_CONFE      BegSR
439300150423      *
439400150423      * Se sono in immissione o in gestione di una riga appena inserita
439500150423      *   re-imposto la richiesta della window W04.
439600150423     c                   If        Wmod = 'I'
439700150423     c                   clear                   $GesW04
439800150423     c                   EndIf
439900040407      *
440000040218      * se sono in immissione e non in gestione di una riga già inserita
440100040218      * pulisco il record
440200040218     c                   If        Wmod = 'I' and WRK_recsbf = ' '
440300040218     c                   clear                   fitgd000
440400040218     c                   endif
440500150603
440600150603     c                   eval      tgdprt = v1cprt                              priorità dettaglio
440700150625      *
440800041213      * se non sono in immissione verifico se è cambiato qualcosa dalla testa \del reclamo
440900041213      *
441000041213     c                   If        Wmod <> 'I' and (tgdtco <> v1ctco or
441100041213     c                             tgddar <> wdar   or tgdorr <> v1corr or
441200041213     c                             tgdksc <> v1cchi or tgdrsc <> v1crsc or
441300041213     c                             tgdclv <> v1cclv)
441400041213     c                   eval      mod_testa = 'S'
441500041213     c                   endif
441600151202      *
441700151202      * -?Memorizzazione  immissione R.A. con Motivo Apertura " 18"?
441800151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
441900151202      *         ?& ho modificato il motivo in " 18" (Reso)?
442000151203      *  ?=> Non sarà più possibile premere F3?
442100151202     c                   clear                   $WrtTIIDC
442200151202if  1c                   If        Wmod    = 'I'           and
442300151202     c                             V1Cmad  = c_MotAp_Reso  and
442400151202     c                             ( wrk_RecSbf = *blank   or
442500151202     c                             ( wrk_RecSbf = 'Y'     and
442600151202     c                               V1Cmad    <> TGDmad ) )
442700151202     c                   eval      $WrtTIIDC = *on
442800151203     c                   eval      $RA_MA_18 = *on
442900151202e   1c                   EndIf
443000040218      *
443100040218      * oggetto
443200040218     c                   eval      tgdtor = v1ctor                              tipo oggetto reclamo
443300040218     c                   if        v1ctor = 'S'
443400040218     c                   movel     spevid        tgdogg                         spedizione
443500040218     c                   endif
443600040218     c                   if        v1ctor = 'O'
443700040218     c                   movel     ormvid        tgdogg                         ORM
443800040218     c                   endif
443900040218     c                   if        v1ctor = 'G'
444000040218     c                   movel     v1cogg        tgdogg                         generico
444100040218     c                   endif
444200040218      * motivo apertura
444300040218     c                   eval      tgdmad = v1cmad
444400141216      *
444500040218      * chiusura
444600040406      * se sono in manutenzione e modifico la causale di chiusura scrivo delle note
444700150622      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
444800150622      ***?già impostato.?
444900040406     c                   if        tgdcdt <> *blank and v1ccdt <> tgdcdt
445000040406     c                             and Wmod <> 'I'
445100150318     c                   eval      wtmo = 'C'
445200150318     c                   clear                   dTGMc
445300150318     c                   eval      §MODcdto = TGDcdt
445400150318     c                   eval      §MODcdtn = V1Ccdt
445500150318     c                   exsr      WRT_modifiche
445600040406     c                   endif
445700040406      *
445800040218      * se è la prima volta che chiudo imposto anche data,ora,p.o. e profilo chiusura
445900040406      *
446000040218     c                   if        tgdcdt = *blank and v1ccdt <> *blank
446100040304     c                   if        Wmod = 'I'
446200040304      * imposto ora e data inserimento chiamata  se sono in immissione
446300040304     c                   eval      tgddcd = w1cdar                              data chiusura
446400150324     c                   eval      tgdocd = %dec(%time())                       ora chiusura
446500040304
446600040304     c                   else
446700150603      * se sono in manutenzione e modifico la causale di chiusura
446800040304      * recupero ora e data chiusura se sono in manutenzione
446900150625      ***?e NON imposto più lo stato ad 8 (Chiusa da back end) in quanto?
447000150625      ***?già impostato.?
447100040304     c                   time                    w0140
447200040304
447300040304     c                   clear                   WLBdat
447400040304     c                   move      w0140         g02dat
447500040304     c                   call      'XSRDA8'
447600040304     c                   parm                    WLBdat
447700040304     c                   eval      tgddcd = g02inv                              data chiusura
447800040304     c                   movel     w0140         tgdocd                         ora chiusura
447900040304     c                   endif
448000040304
448100040617     c                   eval      tgdpct = v1cpot                              p.o. chiusura
448200040218     c                   eval      tgdutc = KNMUS                               utente chiusura
448300040304
448400040218     c                   endif
448500040223      * se codice chiusura è blanks e prima non lo era pulisco i campi del file
448600040223     c                   if        tgdcdt <> *blank and v1ccdt = *blank
448700040223     c                   clear                   tgddcd                         data chiusura
448800040223     c                   clear                   tgdocd                         ora chiusura
448900040223     c                   clear                   tgdpct                         p.o. chiusura
449000040223     c                   clear                   tgdutc                         utente chiusura
449100040223     c                   endif
449200040223      *
449300040223     c                   eval      tgdcdt = v1ccdt                              codice chiusura
449400040218
449500150316      * se utente a video non è l'utente del job
449600150316      * priorità a '1'
449700150316      * imposto priorità a '0'
449800150313     c                   IF        TGDprt = '1' and
449900150316     c                             V1Crpg <> KNMUS
450000150313     c                   eval      TGDprt = '0'
450100150313     c                   ENDIF
450200150316      * se sono in manutenzione e variato utente scrivo file variazioni
450300150316     c                   if        TGDrpg <> V1Crpg and wmod <> 'I'
450400150316     c                   eval      wtmo = 'U'
450500150316     c                   clear                   dTGMu
450600150316     c                   eval      §MODuteo = TGDrpg
450700150316     c                   eval      §MODpogo = TGDpog
450800150316     c                   eval      §MODuten = V1Crpg
450900150316     c                   eval      §MODpogn = %dec(V1Cpog:3:0)
451000150316     c                   exsr      WRT_modifiche
451100040618     c                   endif
451200040618      *
451300150423     c                   eval      tgdpog = %dec(v1cpog:3:0)                    filiale gestione
451400150423     c                   eval      tgdrpg = v1crpg                              utente  gestione
451500150622     c                   if        $Reparto           and
451600150622     c                             V1Ccdt =  *blanks  and
451700150622     c                             V1Cpog <> c_Sede
451800150422     c                   eval      tgdRPR = V1Crpg                              reparto gestione
451900150422     c                   endif
452000040618     c                   eval      tgdrsp = v1crsp                              responsabilità
452100040618      *
452200040407      * P.O. responsabile
452300040407      * se sono in manutenzione e modifico il p.o.responsabile scrivo delle note
452400040407      *
452500040407     c                   if        tgdpor <> *zeros and W_por  <> tgdpor
452600040407     c                             and Wmod <> 'I'
452700150318     c                   eval      wtmo = 'R'
452800150318     c                   clear                   dTGMr
452900150318     c                   eval      §MODporo = TGDpor
453000150318     c                   eval      §MODporn = W_por
453100150318     c                   exsr      WRT_modifiche
453200040407     c                   endif
453300040407      *
453400040218     c                   eval      tgdpor = W_por                               P.O. responsabile
453500040218
453600040212      *
453700040213      *
453800041213     c                   If        wmod = 'I'
453900040213     c                   eval      tgddar = wdar                                dta ricez. comunicaz
454000040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
454100040217     c                   eval      tgdtco = v1ctco                              tipo comunicazione
454200040213      *
454300040213     c                   eval      tgdtch = v1ctch                              tipo chiamamte
454400040213     c                   eval      tgdclv = v1cclv                              categoria cliente
454500040213     c                   eval      tgdrsc = v1crsc                              ragione sociale
454600041209     c                   eval      tgdksc = v1cchi                              codice cliente
454700120919      * se il motivo di apertura lo richiede NON memorizzo il KSC
454800120919     c                   IF        §MARksc = 'N'
454900120919     c                   clear                   TGDksc
455000120919     c                   ENDIF
455100040218     c                   eval      tgdpot = v1cpot                              p.o. immette recla
455200040421     c                   eval      tgdant = v1cant                              Anno   reclamo
455300040218     c                   eval      tgdnut = v1cnut                              Numero reclamo
455400040218     c                   eval      tgdpdt = v1cpdt                              progressivo dettagli
455500041213     c                   endif
455600041213      * solo in immissione valorizzo i seguenti campi
455700040407     c                   if        wmod = 'I' and wrk_recsbf = ' '
455800040218     c                   eval      tgddac = w1cdar                              data inserimento
455900150324     c                   eval      tgdorc = %dec(%time())                       ora inserimento
456000040218     c                   eval      tgdpru = KNMUS                               utente inserimento
456100040407     c                   endif
456200150326      /free
456300150326       //?Storicizzo Variazioni Stato
456400150326         IF  W_prt <> TGDprt;
456500150326           wtmo = 'S';
456600150326           clear dTGMs;
456700150326           §MODprto = W_prt;
456800150326           §MODprtn = TGDprt;
456900150326           exsr WRT_modifiche;
457000150326         ENDIF;
457100150326      /end-free
457200040213      *
457300040309
457400040309      * se sono in immissione e non in gestione di una riga già inserita
457500040309      * valorizzo i seguenti campi
457600040309
457700040309     c                   If        Wmod = 'I' and WRK_recsbf = ' '
457800040218     c                   write     fitgd000
457900040218     c                   FEOD      fitgd01l
458000040218     c                   add       1             rec_wrt
458100041210      * aggiungo 1 al numeratore
458200041210     c                   add       1             sv_pdt
458300040218      *
458400040218     c                   else
458500040218
458600040218     c                   update    fitgd000
458700040218      *
458800040218     c                   endif
458900151201      *
459000151201      * -?Immissione R.A. con Motivo Apertura " 18":?
459100151201      *  ?=> Generazione automatica della "Disposizione di Reso"?
459200151202      *  ?ANCHE SE sono in modifica di una spediz. nella stessa telefonata?
459300151202      *         ?& ho modificato il motivo in " 18" (Reso)?
459400151202     c                   If        $WrtTIIDC = *on
459500151201     c                   exsr      sr_Wrt_TIIDC
459600151202e   1c                   EndIf
459700141216      *
459800141216      * -?Immissione R.A. con Motivo Apertura " 60":?
459900141216      *  ?SE NON sono in modifica di una spedizione nella stessa telefonata?
460000141216if  1c                   If        Wmod       = 'I'      and
460100141216     c                             V1Cmad     = c_VPINC  and
460200141216     c                             WRK_recsbf = *blank
460300141216      *
460400141216      * ·?=> Registrazione automatica dell'esito nelle Note?
460500141216     c                   eval      not_PINcode = 'S'
460600141216     c                   exsr      WRT_note
460700141216      * ·?=> Registrazione automatica dell'esito nei Riferimenti PINcode?
460800141216      *?· Impostazione dati della spedizione nella ds per TGRRIF?
460900141216     c                   clear                   dTGR01
461000141216     c                   if        oPINfxx = *blank  and  oPINerr = *blank
461100141216     c                   eval      dTGR01.§TGResito = 'P'
461200141216     c                   else
461300141216     c                   eval      dTGR01.§TGResito = 'N'
461400141216     c                   endif
461500141216     c                   eval      dTGR01.§TGRfgs   = ARBifp
461600141216     c                   eval      dTGR01.§TGRnfv   = ARBndc
461700141216     c                   eval      dTGR01.§TGRdfv   = ARBddc
461800141216     c                   eval      dTGR01.§TGRpdr   = ARBpdc
461900141216      *?· Impostazione dati nel file FITGR00F?
462000141216     c                   clear                   FITGR000
462100141216     c                   eval      TGRpot = V1Cpot                              Fil. immette tel.
462200141216     c                   eval      TGRant = V1Cant                              Anno   telefonata
462300141216     c                   eval      TGRnut = V1Cnut                              Numero telefonata
462400141216     c                   eval      TGRpdt = V1Cpdt                              Progr. Dettaglio
462500141216     c                   eval      TGRtri = 'D'                                 Tipo Riferimento
462600141216     c                   eval      TGRrif = dTGR01
462700141216     c                   write     FITGR000
462800141216     c                   feod      FITGR01L
462900141216      *
463000141216e   1c                   EndIf
463100040213
463200040213      * verifico se esistono per questa riga che confermo dei record di riferimenti
463300040213      * ed evantualmente li deleto e poi riscrivo
463400040213
463500040421     c     ktgran        setll     fitgr01l
463600040213      *
463700040213     c                   do        *hival
463800040421     c     ktgran        reade     fitgr01l
463900040213      *
464000040219     c                   if        not %eof(fitgr01l)
464100040213     c                   delete    fitgr01l
464200040213     c                   else
464300040213     c                   leave
464400040213     c                   endif
464500040213      *
464600040213     c                   enddo
464700040213      * se esiste referente
464800040213     c                   if        v1refe <> *blank
464900040213     c                   clear                   fitgr000
465000040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
465100040421     c                   eval      tgrant = v1cant                              Numero reclamo
465200040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
465300040213     c                   eval      tgrtri = 'R'                                 tipo  riferimento
465400040213     c                   movel     v1refe        tgrrif
465500040213     c                   write     fitgr000
465600040219     c                   feod      fitgr01l
465700040213     c                   endif
465800040213      * se esiste indirizzo e-mail
465900040213     c                   if        v1mail <> *blank
466000040213     c                   clear                   fitgr000
466100040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
466200040421     c                   eval      tgrant = v1cant                              Numero reclamo
466300040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
466400040213     c                   eval      tgrtri = 'M'                                 tipo  riferimento
466500040213     c                   movel     v1mail        tgrrif
466600040213     c                   write     fitgr000
466700040219     c                   feod      fitgr01l
466800040213     c                   endif
466900040213      * se esiste numero telefono e/o numero fax
467000040213     c                   if        v1ntel <> *blank or v1nfax <> *blank
467100040213     c                   clear                   fitgr000
467200040213     c                   eval      tgrpot = v1cpot                              p.o. immette recla
467300040421     c                   eval      tgrant = v1cant                              Numero reclamo
467400040213     c                   eval      tgrnut = v1cnut                              Numero reclamo
467500040213     c                   eval      tgrtri = 'N'                                 tipo  riferimento
467600040213     c                   movel     v1ntel        tgrrif
467700040213     c                   move      v1nfax        tgrrif
467800040213     c                   write     fitgr000
467900040219     c                   feod      fitgr01l
468000040213     c                   endif
468100040217
468200070413      * confermo le note se sono in immissione e non ho scritto nulla precedentemente
468300040217
468400070413     c                   If        Wmod = 'I' and $bloccof3 = *off
468500070413     c                   clear                   FIDNB0DS
468600040421     c                   z-add     v1cant        iB0ant
468700040217     c                   z-add     v1cnut        iB0nut
468800040217     c                   z-add     v1cpdt        iB0pdt
468900040217     c                   movel     'GEN'         iB0tIN
469000141217     c                   if        WRK_RecSbf = *blank
469100141216     c                   z-add     999           iB0pgn
469200141217     c                   endif
469300040217     c                   movel     v1not1        db0no1
469400040217     c                   movel     v1not2        db0no2
469500040217     c                   movel     v1not3        db0no3
469600040217     c                   movel     v1not4        db0no4
469700040617     c                   eval      ib0pot =v1cpot
469800041013     c                   eval      db0pon =Dutpou
469900040617     c                   eval      db0utn =knmus
470000040218      * se non è mai stato chiamato imposto 'W'
470100040218     c                   if        wfidnb0r = 'Y'
470200040217     c                   movel     'C'           iB0flm
470300040218     c                   else
470400040218     c                   movel     'W'           iB0flm
470500040220
470600040218     c                   endif
470700040217      *
470800040217     C                   CALL      'FIDNB0R'
470900040217     C                   PARM                    kpjba
471000040217     C                   PARM                    FIDNB0DS
471100040217      *
471200040305     c                   endif
471300040305
471400070413      * modalità manutenzione
471500040305
471600070424    1c                   If         Wmod = 'G' or
471700070424     c                             (Wmod = 'I' and $bloccof3 = *on)
471800040305      * nel caso in cui sia stato richiamato il pgm fidnb0r per gestire le note
471900040305      * non lo richiamo + per confermarle
472000070413    2c                   if        wfidnb0r <> 'Y'
472100040305     C                   clear                   FIDNB0DS
472200040421     c                   z-add     v1cant        iB0ant
472300040305     c                   z-add     v1cnut        iB0nut
472400040305     c                   z-add     v1cpdt        iB0pdt
472500040305     c                   movel     'GEN'         iB0tIN
472600040305     c                   movel     'W'           iB0flm
472700040305     c                   eval      ib0pot =v1cpot
472800041013     c                   eval      db0pon =Dutpou
472900040305     c                   eval      db0utn =knmus
473000040305      * valorizzo il progressivo riga  scrivo la prima riga
473100040305    3c                   if        *in51 = *off and V1not1 <> *blanks
473200040305     c                   z-add     1             Ib0pgn
473300040305     c                   movel     v1not1        db0no1
473400040305      *
473500040305     C                   CALL      'FIDNB0R'
473600040305     C                   PARM                    kpjba
473700040305     C                   PARM                    FIDNB0DS
473800040305    3c                   endif
473900040305
474000040305      * valorizzo il progressivo riga  scrivo la seconda riga
474100040305    3c                   if        *in52 = *off and V1not2 <> *blanks
474200040305     c                   z-add     2             Ib0pgn
474300040305     c                   movel     v1not2        db0no1
474400040305      *
474500040305     C                   CALL      'FIDNB0R'
474600040305     C                   PARM                    kpjba
474700040305     C                   PARM                    FIDNB0DS
474800040305    3c                   endif
474900040305      * valorizzo il progressivo riga  scrivo la terza   riga
475000040305    3c                   if        *in53 = *off and V1not3 <> *blanks
475100040305     c                   z-add     3             Ib0pgn
475200131017     c                   movel     v1not3        db0no1
475300040305      *
475400040305     C                   CALL      'FIDNB0R'
475500040305     C                   PARM                    kpjba
475600040305     C                   PARM                    FIDNB0DS
475700040305    3c                   endif
475800040305      * valorizzo il progressivo riga  scrivo la quarta  riga
475900040305    3c                   if        *in54 = *off and V1not4 <> *blanks
476000040305     c                   z-add     4             Ib0pgn
476100040305     c                   movel     v1not4        db0no1
476200040305      *
476300040305     C                   CALL      'FIDNB0R'
476400040305     C                   PARM                    kpjba
476500040305     C                   PARM                    FIDNB0DS
476600040305    3c                   endif
476700040305
476800040305    2c                   endif
476900040305
477000040305    1c                   endif
477100070402
477200140205      * se impostata data consegna   o   se previste note LDV obbligatorie
477300131119      * => devo scrivere alcune note R.A. e note ldv
477400070402
477500140205    1c                   if        not_ldv = 'S'  or  §MARnotLdV = 'S'
477600070403      * se devo inserire delle note indica che ho fatto delle modifiche
477700070403      * alla data consegna e quindi impedisco l'uso del f3
477800070403     c                   eval      $bloccof3  =  *on
477900070404      * recupero le note da passare al pgm note ldv
478000070404     c                   clear                   nob
478100070404     c                   clear                   x
478200070402      * recupero le note R.A. dal file e le passo al PGM note LDV
478300070402     c     ktgn          setll     fitgn01l
478400070402     c                   do        *hival
478500070402     c     ktgn          reade     fitgn01l
478600070402
478700070402     c                   if        %eof(fitgn01l)
478800070402     c                   leave
478900070402     c                   endif
479000070402      * passo solo le prime 8(qui non ci sono ancora le modifiche registrate)
479100070402     c                   if        x < 8
479200070402     c                   add       1             x
479300070402     c                   movea     tgnnot        nob(x)
479400070402     c                   else
479500070402     c                   leave
479600070402     c                   endif
479700070402
479800070402     c                   enddo
479900070402
480000131119      * richiamo il programma note LdV in  immissione
480100131206     C                   clear                   Fnlrn6DS
480200131206     c                   eval      in6mod  = 'I'
480300131206     c                   eval      in6aas  = v1anns
480400131206     c                   eval      in6lnp  = v1lnps
480500131206     c                   eval      in6nrs  = v1nrss
480600131206     c                   eval      in6nsp  = v1nsps
480700070402      * note
480800131206     c                   movel     nob(1)        in6not1
480900131206     c                   move      nob(2)        in6not1
481000131206     c                   movel     nob(3)        in6not2
481100131206     c                   move      nob(4)        in6not2
481200131206     c                   movel     nob(5)        in6not3
481300131206     c                   move      nob(6)        in6not3
481400131206     c                   movel     nob(7)        in6not4
481500131206     c                   move      nob(8)        in6not4
481600070402     c
481700131206     c                   eval      in6pgm  = 'FIDNA1R'
481800131206      * se sono una spedizione in consegna passo i dati della distinta del file ARB
481900131206     c                   If        $cons  = *on
482000131206     c     KSPED         chain     Fnarb01l
482100131206     c                   If        %found(Fnarb01l)
482200131206     c                   eval      in6fgs = arbifp
482300131206     c                   eval      in6nfv = arbndc
482400131206     c                   eval      in6dfv = arbddc
482500131206     c                   eval      in6pdr = arbpdc
482600131206     c                   eval      in6cons = 'S'
482700131206     c                   Endif
482800131206     c                   Endif
482900070402
483000131206     c                   call      'FNLRN6R'
483100070402     c                   parm                    kpjba
483200131206     c                   parm                    FNLRN6DS
483300070402
483400070402     c                   clear                   not_ldv
483500070412    1c                   endif
483600070402
483700070402      * se devo registrare delle note di modifica data consegna
483800070402      * richiesta scrivo delle note mie nelle note R.A.
483900070402      * Le Faccio uguali a quelle della Rita ?????
484000070402
484100070402      * Decodifico tipo consegna richiesta
484200070402
484300070412    1c                   If        v1cdcr > 0
484400070402     c                   select
484500070402
484600070402     c                   when      v1ctcr = 'P'
484700070404     c                   eval      W_dtcr   ='PRIMA DEL'
484800070402     c                   when      v1ctcr = 'D'
484900070404     c                   eval      W_dtcr   ='DOPO  IL '
485000070402     c                   other
485100070404     c                   eval      W_dtcr   ='      IL '
485200070402
485300070402     c                   endsl
485400070402
485500070402      * creo le note R.A.
485600070402     c                   clear                   nob
485700070402     c                   clear                   x
485800070402     c                   eval      nob(1)='Cons.Rich:'
485900070402     c                   eval      nob(1)=%trim(nob(1))+%trim(w_dtcr)
486000070402     c                   eval      nob(1)=%trim(nob(1))+' '+
486100070402     c                             %editw(v1cdcr:'  /  /    ')
486200141031    2c                   if        v1cocr>0
486300141031     c                   eval      nob(1)=%trim(nob(1))+' ore'
486400141031    3c                   if        v1ctcr=*blanks
486500141031     c                   eval      nob(1)=%trim(nob(1))+' '+
486600141031     c                             %editw(v1cocr:'  :  ')
486700141031     c                   else
486800141031     c                   eval      nob(2)=
486900141031     c                             %editw(v1cocr:'  :  ')
487000141031    3c                   endif
487100141031    2c                   endif
487200070402
487300070402     c                   clear                   not_dcr
487400070402
487500070402      * se valorizzate anche le note DCR con il msg del 48 scrivo
487600070402      * il msg in note R.A.
487700070412    2c                   if        note_dcr <> *blanks
487800141031     c                   if        v1ctcr = *blanks
487900070402     c                   movea     note_dcr      nob(2)
488000141031     c                   else
488100141031     c                   eval      %subst(nob(2):7:29)= %subst(note_dcr:1:29)
488200141031     c                   eval      nob(3) = %subst(note_dcr:30:35)
488300141031     c                   eval      nob(4) = %subst(note_dcr:65:14)
488400141031     c                   endif
488500070402      * visto che il messaggio è lungo 78 dovrebbe riempire 3 note
488600070402     c                   clear                   note_dcr
488700070412    2c                   endif
488800070402      * richiamo in scrittura il pgm delle note
488900070402     c                   clear                   FIDNB0DS
489000070402     c                   eval      db0no1 = nob(1)
489100070402     c                   eval      db0no2 = nob(2)
489200070402     c                   eval      db0no3 = nob(3)
489300070402     c                   eval      db0no4 = nob(4)
489400070402
489500070402     c                   z-add     v1cant        iB0ant
489600070402     c                   z-add     v1cnut        iB0nut
489700070402     c                   z-add     v1cpdt        iB0pdt
489800070404     c                   z-add     999           ib0pgn
489900070402     c                   movel     'GEN'         iB0tIN
490000070402     c                   eval      ib0pot =v1cpot
490100070402     c                   eval      db0pon =Dutpou
490200070402     c                   eval      db0utn =knmus
490300070402      * imposto 'W'
490400070402     c                   movel     'W'           iB0flm
490500070402      *
490600070402     c                   call      'FIDNB0R'
490700070402     c                   parm                    kpjba
490800070402     c                   parm                    fidnb0ds
490900070402
491000070412    1c                   endif
491100140506      *
491200070403      * se c'è stata almeno una riga che ha fatto modificare una data consegna richiesta
491300070403      * impedisco l'uso del tasto funzionale F3
491400070403     c                   if        $bloccof3  =  *on
491500070403     c                   seton                                        63
491600070403     c                   endif
491700140506      *
491800140506      * se è stata inserita almeno una R.A. con Motivo Apertura " 17"
491900151203      *                                   o con Motivo Apertura " 18"
492000141210      *                                   o con Motivo Apertura " 60"
492100140506      * impedisco l'uso del tasto funzionale F3
492200141210     c                   if        $RA_MA_17  =  *on  or
492300151203     c                             $RA_MA_18  =  *on  or
492400141210     c                             $RA_MA_60  =  *on
492500140506     c                   seton                                        63
492600140506     c                   endif
492700040213      *
492800070402
492900040213     c                   ENDSR
493000151201      /free
493100151201
493200151201       //--------------------------------------------------------------
493300151201       //?Scrittura record in TIIDC00F per Reso/Dirottamento.          ?
493400151201       //--------------------------------------------------------------
493500151201       BEGSR  sr_Wrt_TIIDC;
493600151201
493700151201           if  Not %open(TIIDC02L);
493800151201             open  TIIDC02L;
493900151201           endif;
494000151201
494100151201           // -?Reperimento eventuale capofila £6 della FGS?
494200151201           clear  FNLV55ds;
494300151201           d55tpt = '6';
494400151201           if  $FNARB = *on;
494500151201             d55lin = ARBlna;
494600151201           else;
494700151201             d55lin = BLPlna;
494800151201           endif;
494900151201           d55drf = %dec( %date() );
495000151201           FNLV55R ( FNLV55ds );
495100151202
495200151202           // -?Reperimento progressivo richiesta per la "Disposizione di Reso"?
495300151202           exsr  sr_Rep_IDCprg;
495400151201
495500151201           // -?Impostazione campi di TIIDC00F?
495600151201           clear  TIIDC000;
495700151201
495800151201if  2      If  $Ok_IDCprg;
495900151201             IDCprg = k_IDCprg;
496000151201           //else;
496100151201             //clear  IDCprg;
496200151201           endif;
496300151201           IDCinsData = %int( %subst( %char( %dec( %timestamp() ) )
496400151201                                      : 1 : 14 ) );
496500151201           IDCmodAcc  = 'B';
496600151201           if  V1Ctch = 'D';
496700151201             IDCtipoCli = V1Ctch;
496800151201           else;
496900151201             IDCtipoCli = 'M';
497000151201           endif;
497100151201           if  $FNARB = *on;
497200151201             IDCwaas  = ARBaas;
497300151201             IDCwlnp  = ARBlnp;
497400151201             IDCwnrs  = ARBnrs;
497500151201             IDCwnsp  = ARBnsp;
497600151201             IDCaas   = ARBaas;
497700151201             IDClnp   = ARBlnp;
497800151201             IDCnrs   = ARBnrs;
497900151201             IDCnsp   = ARBnsp;
498000151201             //IDCfgs   = ARBlna;     ?(vedi capofile £6)?
498100151201           else;
498200151201             IDCwaas  = BLPaas;
498300151201             IDCwlnp  = BLPlnp;
498400151201             IDCwnrs  = BLPnrs;
498500151201             IDCwnsp  = BLPnsp;
498600151201             IDCaas   = BLPaas;
498700151201             IDClnp   = BLPlnp;
498800151201             IDCnrs   = BLPnrs;
498900151201             IDCnsp   = BLPnsp;
499000151201             //IDCfgs   = BLPlna;     ?(vedi capofile £6)?
499100151201           endif;
499200151201           IDCfgs     = d55tfa;
499300151201           IDCtipoDis = '5';
499400151201           IDCnewLna  = *zero;
499500151201           IDCfla     = 'R';
499600151201           IDCelaFlg  = 'S';
499700151201           IDCelaUte  = kNmUs;
499800151201
499900151201           // -?Scrittura record in TIIDC00F?
500000151201           //______________
500100151201           write  TIIDC000;
500200151201           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
500300151201
500400151201e   1  ENDSR;
500500151201
500600151201       //--------------------------------------------------------------
500700151201       //?Reperimento progressivo per disposizione di Reso/Dirottamento?
500800151201       //--------------------------------------------------------------
500900151201       BEGSR  sr_Rep_IDCprg;
501000151201
501100151201         clear  $Ok_IDCprg;
501200151201         clear  keyTIIDC02;
501300151201
501400151201         clear  TRUL33ds;
501500151201         I33tla = 'L';
501600151201         clear  I33ope;
501700151217         I33cnu = 651;
501800151201         I33num = 1;
501900151201
502000151201         DoW  Not  $Ok_IDCprg;
502100151201
502200151201           kpjbu = TRUL33ds;
502300151201           TRUL33R ( kpjba );
502400151201           TRUL33ds = kpjbu;
502500151201
502600151201           if  O33err <> *zeros;
502700151201             leavesr;
502800151201           endif;
502900151201
503000151201           w0090  = O33nri;
503100151217           w0040  = %subdt( %date() : *years );
503200151217           k_IDCprg = ( %int( %subst( %editc( w0040 : 'X' ) : 3 : 2 ) )
503300151217                        * 1000000000 ) + w0090;
503400151201           setll  %kds( keyTIIDC02 )  TIIDC000;
503500151201
503600151201           // -?Se numero già presente nel file TIIDC ne cerco un altro?
503700151201           if  Not %equal(TIIDC02L);
503800151201             $Ok_IDCprg = *on;
503900151201           endif;
504000151201
504100151201         EndDo;
504200151201
504300151201       ENDSR;
504400151201
504500151201      /end-free
504600040213      *
504700040213      *---------------------------------------------------------------*
504800040213      * Routine caricamento record nel subfile                        *
504900040213      *---------------------------------------------------------------*
505000040213     c     SR_CARSBF     BegSR
505100040213
505200040216      * caricamento dettaglio reclami
505300040217     c                   z-add     *zeros        s01nrr
505400040216
505500040421     c     ktgdan        setll     Fitgd01l
505600040216      *
505700040216     c                   Do        *hival
505800040422     c     ktgdan        reade(N)  fitgd01l
505900040216
506000040216     c                   if        %eof(fitgd01l)
506100040216     c                   leave
506200040216     c                   endif
506300040216
506400040216     c                   add       1             s01nrr
506500040218
506600040218     C
506700040218      * numero riga
506800040217     c                   eval      vs1pdt = tgdpdt
506900040216
507000040216      * - OGGETTO
507100040216
507200040220     c                   clear                   vs1ogg
507300040217
507400040216      * ---------- SPEDIZIONE -----------
507500040216
507600040217     c                   if        tgdtor = 'S'
507700040218      *
507800040218     c                   movel     tgdogg        WRKspe
507900040219     c                   eval      vs1ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
508000040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
508100040219     c                             ' ' + WRKann
508200040218      *
508300040216     c                   endif
508400040216
508500040216      * ----------    ORM     -----------
508600040216
508700040217     c                   if        tgdtor = 'O'
508800040218     c                   movel     tgdogg        WRKorm
508900040218      *
509000040218     c                   eval      vs1ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
509100040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
509200040218     c                             '/' + %trim(WRKnrv)
509300040218      *
509400040216     c                   endif
509500040216
509600040216      * ----------  GENERICO  -----------
509700040216
509800040217     c                   if        tgdtor = 'G'
509900040218     c                   EVAL      VS1ogg = TGDogg
510000040216     c                   endif
510100040218
510200040218      * MOTIVO RECLAMO
510300040218
510400040218     c                   eval      vs1mad = tgdmad
510500040218      * Controllo
510600040218     c                   clear                   tibs02ds
510700040218     C                   MOVEL     'C'           t02mod
510800040218     C                   MOVEL     knsif         t02sif
510900040218     C                   MOVEL     'MAR'         t02cod
511000040218     C                   MOVEL     vs1mad        t02ke1
511100040218      *
511200040218     C                   CALL      'TIBS02R'
511300040218     C                   PARM                    KPJBA
511400040218     C                   PARM                    TIBS02DS
511500040218      *
511600040218    4C                   IF        t02err = *BLANKS
511700040218     C                   movel     T02uni        DMAR
511800040218     C                   movel     §MARdesc      vs1dmr
511900040218     c                   endif
512000141215      *
512100141215      * -?Se il motivo di apertura NON può essere usato: proteggo campo opz.?
512200141215     c                   eval      *in83 =((*in74 = *off  and  §MARutAp = 'N')
512300141215     c                                      or
512400141215     c                                     (wMod <> 'I'   and  §MARutUt = 'N'))
512500040218
512600040218      * CHIUSURA RECLAMO
512700040218
512800040218     c                   eval      vs1cdt = tgdcdt
512900040218
513000040218      * DATA CHIUSURA
513100040218
513200040220     c                   clear                   vs1dcr
513300040220
513400040218     C                   If        tgddcd > *zeros
513500040218     c                   z-add     tgddcd        data8
513600040218     c                   move      anno2         vs1dcr
513700040218     c                   move      mese          mmgg              4 0
513800040218     c                   movel     mmgg          vs1dcr
513900040218     c                   movel     giorno        vs1dcr
514000040218     c                   endif
514100040218
514200040218     c                   write     fia1s01
514300141215     c                   setoff                                       81  83
514400040218
514500040218     c                   enddo
514600040218      *
514700040218      * verifico se inserite righe ed accendo indicatori di emissione
514800040218      *
514900040218     c                   if        s01nrr > 0
515000040218     c                   seton                                        21
515100040218     c                   endif
515200040218
515300040218     c                   seton                                        20
515400040218
515500040213     c                   ENDSR
515600041209      *
515700041209      *---------------------------------------------------------------*
515800041209      * Routine recupero codice chiamante                             *
515900041209      *---------------------------------------------------------------*
516000041209     c     SR_RECCHI     BegSR
516100041209      *
516200041209     c                   clear                   Wrk_cchi
516300041209     c                   clear                   Wrk_cchiA
516400041209     c                   clear                   Wrk_cchiB
516500041209     c                   clear                   Wrk_cchiC
516600041209      *
516700041209      * rileggo il dettaglio
516800041209      *
516900041209
517000041209     c     ktgdan        setll     Fitgd01l
517100041209      *
517200041209     c                   Do        *hival
517300041209     c     ktgdan        reade(N)  fitgd01l
517400041209
517500041209     c                   if        %eof(fitgd01l)
517600041209     c                   leave
517700041209     c                   endif
517800041209
517900041209      * ---------- SPEDIZIONE -----------
518000041209
518100041209    2c                   if        tgdtor = 'S'
518200041209      *
518300041209     c                   movel     tgdogg        Wspevid
518400041230      * se non sono in sede
518500041230     c                   If        not *in05
518600041209      * Aggancio bolle arrivi
518700041209     c     KSPEDw        chain     Fnarb01l
518800041209    3c                   If        %found(Fnarb01l)
518900041209      * mittente se valorizzato
519000041209     c                   move      arbccm        w0040
519100041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
519200041210     c                             w0040 <> 9999
519300041209     c                   z-add     arbccm        Wrk_cchiA
519400041209      * se trovato codice mittente prendo quello e scappo
519500041209     c                   leave
519600041209    4c                   endif
519700041209      *
519800041209     c                   z-add     arbksc        W0040
519900041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
520000041209     c                             w0040 <> 9999
520100041209      * in caso di CCM uguale a zeros valorizzo con KSC
520200041209     c                   z-add     arbksc        Wrk_cchiA
520300041209     c                   leave
520400041209    4c                   endif
520500041209      *
520600041209    3c                   endif
520700041209      * Aggancio bolle partenze se non ho trovato ancora nulla
520800041209     c     KSPEDw        chain     Fnblp01l
520900041209    3c                   If        %found(Fnblp01l)
521000041209      * mittente se valorizzato
521100041210     c                   move      blpccm        w0040
521200041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
521300041210     c                             w0040 <> 9999
521400041209     c                   z-add     blpccm        Wrk_cchiA
521500041209      * se trovato codice mittente prendo quello e scappo
521600041209     c                   leave
521700041209    4c                   endif
521800041209      *
521900041209     c                   z-add     blpksc        W0040
522000041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
522100041209     c                             w0040 <> 9999
522200041209      * in caso di CCM uguale a zeros valorizzo con KSC
522300041209     c                   z-add     blpksc        Wrk_cchiA
522400041209     c                   leave
522500041209    4c                   endif
522600041209      *
522700041209    3c                   endif
522800041230      * sono in sede
522900041230     c                   else
523000041209      * Aggancio bolle di sede  se non ho trovato ancora nulla
523100041209     c     KSPEDw        chain     titas30c
523200041209    3c                   If        %found(Titas30c)
523300041209      * mittente se valorizzato
523400041209     c                   move      tasccm        w0040
523500041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
523600041210     c                             w0040 <> 9999
523700041209     c                   z-add     tasccm        Wrk_cchiA
523800041209      * se trovato codice mittente prendo quello e scappo
523900041209     c                   leave
524000041209    4c                   endif
524100041209      *
524200041209     c                   z-add     tasksc        W0040
524300041209    4c                   if        w0040 <> *zeros  and w0040 <> 8888 and
524400041209     c                             w0040 <> 9999
524500041209      * in caso di CCM uguale a zeros valorizzo con KSC
524600041209     c                   z-add     tasksc        Wrk_cchiA
524700041209     c                   leave
524800041209    4c                   endif
524900041209      *
525000041209    3c                   endif
525100041209      *
525200041230    3c                   endif
525300041230      *
525400041209    2c                   endif
525500041209
525600041209      * ----------    ORM     -----------
525700041209
525800041209    2c                   if        tgdtor = 'O'
525900041209     c                   movel     tgdogg        Wormvid
526000041209      *
526100041209     c     KORMw         chain     Fnorm01l
526200041209    3c                   If        %found(Fnorm01l)
526300041209     c
526400041209      * ordinanante
526500041209     c                   movel     ormcor        w0070
526600041209     c                   move      w0070         w0040
526700041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
526800041209     c                             w0040 <> 9999  and Wrk_cchiA = 0
526900041209     c                   z-add     w0070         Wrk_cchiA
527000041209     c                   endif
527100041209
527200041209      * ritiro
527300041209     c                   movel     ormcra        w0070
527400041209     c                   move      w0070         w0040
527500041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
527600041209     c                             w0040 <> 9999 and Wrk_cchiB = 0
527700041209     c                   z-add     w0070         Wrk_cchiB
527800041209     c                   endif
527900041209
528000041209      * destinatario
528100041209     c                   movel     ormcrc        w0070
528200041209     c                   move      w0070         w0040
528300041209     c                   if        w0040 <> *zeros  and w0040 <> 8888 and
528400041209     c                             w0040 <> 9999 and Wrk_cchiC = 0
528500041209     c                   z-add     w0070         Wrk_cchiC
528600041209     c                   endif
528700041209    3c                   endif
528800041209    2c                   endif
528900041209
529000041209      * ----------  GENERICO  -----------
529100041209      * leggo il successivo
529200041209    2c                   if        tgdtor = 'G'
529300041209     c                   iter
529400041209    2c                   endif
529500041209
529600041209     c                   enddo
529700041209
529800041209     c                   endsr
529900040203      *
530000040129      *---------------------------------------------------------------*
530100040212      * Routine pulizia  videata Immissione Reclamo  x nuovo reclamo  *
530200040129      *---------------------------------------------------------------*
530300040129     c     SR_PUL01      BegSR
530400040129      *
530500040421     C                   clear                   v1cant
530600040416     C                   clear                   v1cnut
530700040421      * accendo indicatore che non mette in inversione fondo e sottolineato il numero reclamo
530800040421     c                   seton                                        15
530900040421      *
531000040202     C                   clear                   v1ctco
531100040202     C                   clear                   v1dtco
531200040202     C                   clear                   v1cdar
531300040202     C                   clear                   v1corr
531400040202     C                   clear                   v1cchi
531500050118     C                   clear                   v1dchi
531600040202     C                   clear                   v1crsc
531700040129     C                   clear                   v1cclv
531800040129     C                   clear                   v1refe
531900040129     C                   clear                   v1ntel
532000040129     C                   clear                   v1mail
532100040129     C                   clear                   v1nfax
532200040218     C                   clear                   W_v1cchi
532300040219     C                   clear                   rec_wrt
532400041207     c                   clear                   Wrk_cchi
532500041207     c                   clear                   Wrk_cchiA
532600041207     c                   clear                   Wrk_cchiB
532700041207     c                   clear                   Wrk_cchiC
532800041207     c                   clear                   SW_v1cchi
532900070413     c                   eval      $bloccof3 = *off
533000140506     c                   eval      $RA_MA_17 = *off
533100151203     c                   eval      $RA_MA_18 = *off
533200141210     c                   eval      $RA_MA_60 = *off
533300151201     c                   clear                   $Ok_IDCprg
533400151202     c                   clear                   $WrtTIIDC
533500070403     c                   setoff                                       63
533600041210      * Imposto il numeratore
533700041210     c                   z-add     1             SV_pdt
533800040213      * pulizia video relativo oggetto
533900040213     c                   exsr      sr_pulctl
534000040213      *
534100040202      * pulizia subfile
534200040202     c                   clear                   s01nrr
534300150310      * accendo indicatore di posizionamento cursore su CHIAMANTE
534400150310     c                   seton                                        44
534500040213      * imposto il tipo comunicazione
534600040213     c                   eval      v1ctco = 'T'
534700040213      * decodifico
534800150625     c                   clear                   dCOR
534900040217     c                   clear                   tibs02ds
535000040213     C                   MOVEL     'C'           t02mod
535100040213     C                   MOVEL     knsif         t02sif
535200040213     C                   MOVEL     'COR'         t02cod
535300040213     C                   MOVEL     v1ctco        t02ke1
535400040213      *
535500040213     C                   CALL      'TIBS02R'
535600040213     C                   PARM                    KPJBA
535700040213     C                   PARM                    TIBS02DS
535800040213      *
535900040213    4C                   IF        t02err = *BLANKS
536000040213     C                   movel     T02uni        Dcor
536100040216     C                   movel     §CORdesc      v1dtco
536200040213      *
536300040213     C                   ELSE
536400040216     C                   clear                   v1dtco
536500040213    4C                   ENDIF
536600040203      *
536700040129     c                   endsr
536800040213      *
536900040213      *---------------------------------------------------------------*
537000040213      * Routine pulizia  videata Immissione Reclamo  x nuova riga     *
537100040213      *---------------------------------------------------------------*
537200040213     c     SR_PULctl     BegSR
537300040213      *
537400141215     c                   exsr      sr_PULctl2
537500040216      *
537600040216     c                   setoff                                       2120
537700040216     c                   seton                                        22
537800040216     c                   write     fia1c01
537900040216     c                   setoff                                       22
538000040213      *
538100040213     c                   ENDSR
538200141215      *
538300141215      *---------------------------------------------------------------*
538400141215      *
538500141215     c     SR_PULctl2    BegSR
538600141215      *
538700141215     C                   clear                   forz_rec
538800141215     C                   clear                   v1ctor
538900141215     C                   clear                   v1poeo
539000141215     C                   clear                   v1nsor
539100141215     C                   clear                   v1norm
539200141215     C                   clear                   v1nrvo
539300141215     C                   clear                   v1cogg
539400141215     C                   clear                   v1lnps
539500141215     C                   clear                   v1nrss
539600141215     C                   clear                   v1nsps
539700141215     C                   clear                   v1anns
539800141215     C                   clear                   v1ctch
539900141215     C                   clear                   v1dtch
540000141215     C                   clear                   W_cmad
540100150122     C                   clear                   wSaveMAD
540200141215     C                   clear                   v1cmad
540300141215     C                   clear                   v1dmad
540400141215     C                   clear                   v1ccdt
540500141215     C                   clear                   v1dcdt
540600141215     C                   clear                   v1not1
540700141215     C                   clear                   v1not2
540800141215     C                   clear                   v1not3
540900141215     C                   clear                   v1not4
541000150313     C                   clear                   v1crpg
541100150316     C                   clear                   v1cpog
541200150313     C                   clear                   v1cprt
541300150313     C                   clear                   v1dprt
541400141215     C                   clear                   v1crsp
541500141215     C                   clear                   v1cpor
541600141215     C                   clear                   v1cdcr
541700141215     C                   clear                   v1cocr
541800141215     C                   clear                   v1ctcr
541900141215     c                   clear                   v1ccva
542000141215     C                   clear                   v1dcva
542100141215     c                   clear                   W_por
542200141215     C                   clear                   v1dpor
542300141215     C                   clear                   Wormvid
542400141215     C                   clear                   Wspevid
542500141215     C                   clear                   WRK_recsbf
542600141215     C                   clear                   WFIDNB0R
542700141215     c                   clear                   not_dcr
542800141215     c                   clear                   not_ldv
542900141215     c                   clear                   note_dcr
543000141215      * pulisco i campi di lavoro
543100141215     c                   clear                   wforz
543200141215     c                   eval      $ric_fnlrg2r = *off
543300141215     c                   eval      $riemetti    = *off
543400141215     c                   eval      $fnarb       = *off
543500141217     c                   eval      $RA_MA_60    = *off
543600150326     c                   clear                   W_prt
543700141215
543800141215      * accendo indicatore di emissione dell'oggetto generico
543900141215     c                   seton                                        03
544000141215     c                   setoff                                       0102
544100141215     c                   setoff                                       80
544200141215      * data consegna riochiesta
544300141215     c                   setoff                                       616074
544400141215      * pulizia indicatori di protezione
544500141215     c                   setoff                                       515253
544600141215     c                   setoff                                       54
544700141215      * spengo indicatore di note aggiuntive
544800141215     c                   setoff                                       29
544900150624      *
545000150624      * -?Spegnimento indicatore di protezione "Tipo Comunicazione"?
545100150624     c                   eval      *in73 = *off
545200141215      *
545300141215      * imposto il numero progressivo riga solo se sono in immissione
545400141215     c                   if        wmod  = 'I'
545500141215      *
545600141215     c                   z-add     sv_pdt        v1cpdt
545700150313      * imposto la priorità a '1' quando sono in immissione
545800150313     c                   eval      V1Cprt = '1'
545900141215      * Decodifico
546000141215     c                   clear                   tibs02ds
546100141215     C                   MOVEL     'C'           t02mod
546200141215     C                   MOVEL     knsif         t02sif
546300141215     C                   MOVEL     'PRR'         t02cod
546400141215     C                   MOVEL     v1cprt        t02ke1
546500141215      *
546600141215     C                   CALL      'TIBS02R'
546700141215     C                   PARM                    KPJBA
546800141215     C                   PARM                    TIBS02DS
546900141215      *
547000141215     C                   if        t02err = *BLANKS
547100141215     C                   movel     T02uni        DPRR
547200141215     C                   movel     §PRRdesc      v1dprt
547300141215     c                   endif
547400150313      * Imposto utente e filiale gestione
547500150313     c                   IF        w_pot > 0
547600150313     c                   eval      V1Cpog = %editc(w_pot:'X')
547700150313     c                   ELSE
547800150313     c                   eval      V1Cpog = %editc(DUTpou:'X')
547900150313     c                   ENDIF
548000150313     c                   eval      V1Crpg = KNMUS
548100141215      *
548200141215     c                   endif
548300141215      *
548400141215     c                   EndSR
548500141215      *
548600040218      *****************************************************************
548700040218      *  Gestione del subfile Manutenzione riga reclamo
548800040218      *****************************************************************
548900040218     c     SR_GESSBF     begsr
549000040218
549100040218     c                   do        *hival
549200040218
549300040218     c                   readc     fia1s01
549400150616     c                   eval      *in84 = *off
549500040218
549600150610     c                   If        %eof(fidna1d)
549700040218     c                   leave
549800040218     c                   endif
549900040218
550000040218     c                   if        vs1opz = '2'
550100150317
550200150317      /free
550300150317       //?Controllo se l'utente è abiliato alla filiale gestione della RA da manutenzionare
550400150616       //?(SE NON SONO IN SEDE)
550500150317         *in84 = *off;
550600150317         chain(n) (V1Cant:V1Cnut:VS1pdt) FITGD01L;
550700150317         IF  %found(FITGD01L) and
550800150616             NOT *in05        and
550900150318             %lookup(%editc(TGDpog:'X'):sk_wFIL) = 0;
551000150317           *in84 = *on;
551100150317           update FIA1S01;
551200150317           leave;
551300150317         ENDIF;
551400150724
551500150724         // -?Controllo anche se R.A. con Motivo Apertura = " 70"?
551600150724         //  ?(Reclamo da Sede) - in gestione ad una filiale?
551700150724         *in84 = *off;
551800150724         If  TGDmad = c_ReclSede  and  TGDpog <> %int(c_Sede)  and  *in05;
551900150724           *in84 = *on;
552000150724           update  FIA1S01;
552100150724           leave;
552200150724         EndIf;
552300150317      /end-free
552400150317
552500040218      * valorizzo un campo di comodo per differenziare un recupero da subfile
552600040218      * da un recupero da richiamo da altro PGM
552700040218     c                   eval      WRK_recsbf = 'Y'
552800040218
552900040421     c                   eval      keyant = v1cant
553000040218     c                   eval      keynut = v1cnut
553100040218     c                   eval      keypdt = vs1pdt
553200040218     c                   exsr      SR_recogg
553300141215     c  n82              clear                   vs1opz
553400040218     c                   update    fia1s01
553500040218      * esco dal ciclo per la gestione del dettaglio scelto
553600040218
553700040218     c                   leave
553800040218     c                   endif
553900040218
554000040218     c                   enddo
554100040218      *
554200040218     c                   ENDSR
554300040218      *****************************************************************
554400040218      *  Recupero Dettaglio Reclamo in manutenzione
554500040218      *****************************************************************
554600040218     c     SR_RECOGG     begsr
554700040303
554800040303     c                   setoff                                       515253
554900040422     c                   setoff                                       547082
555000141215     c                   setoff                                       74  83
555100150625      * -?Proteggo il Tipo Comunicazione SE sono in MODIFICA?
555200150625     c                   eval      *in73 = (wMod <> 'I')
555300150326
555400150326     c                   clear                   W_prt
555500040218      *
555600040422     c     Ktgdrec       chain(e)  fitgd01l
555700040218
555800040422    1c                   if        %error
555900040422      * emetto l'errore
556000040422     c                   seton                                        82
556100040422     c                   else
556200040422      *
556300040218    1c                   if        %found(fitgd01l)
556400040218
556500040218      * numero riga
556600040218     c                   eval      v1cpdt = tgdpdt
556700040218
556800040218      * - OGGETTO
556900040218
557000040218     c                   eval      v1ctor = tgdtor
557100040218
557200040218      * ---------- SPEDIZIONE -----------
557300040218
557400040218    2c                   if        tgdtor = 'S'
557500040218     c                   movel     tgdogg        spevid
557600040218     c                   seton                                        01
557700040218     c                   setoff                                       0203
557800140204     c  n05              exsr      CTR_specons
557900040218      *
558000040218    2c                   endif
558100040218
558200040218      * ----------    ORM     -----------
558300040218
558400040218    2c                   if        tgdtor = 'O'
558500040218     c                   movel     tgdogg        ormvid
558600040218     c                   seton                                        02
558700040218     c                   setoff                                       0103
558800040218    2c                   endif
558900040218
559000040218      * ----------  GENERICO  -----------
559100040218
559200040218    2c                   if        tgdtor = 'G'
559300040218     c                   movel     tgdogg        v1cogg
559400040218     c                   seton                                        03
559500040218     c                   setoff                                       0102
559600040218    2c                   endif
559700040218      * chi chiama
559800040218     c                   eval      v1ctch = tgdtch
559900040304     c                   clear                   tibs02ds
560000040304     C                   MOVEL     'C'           t02mod
560100040304     C                   MOVEL     knsif         t02sif
560200040304     C                   MOVEL     'CCR'         t02cod
560300040304     C                   MOVEL     v1ctch        t02ke1
560400040304      *
560500040304     C                   CALL      'TIBS02R'
560600040304     C                   PARM                    KPJBA
560700040304     C                   PARM                    TIBS02DS
560800040304      *
560900040304     C                   movel     T02uni        DCCR
561000040304     c                   movel     §ccrdesc      v1dtch
561100040218      * motivo
561200040218     c                   eval      v1cmad = tgdmad
561300150410     c                   eval      wSaveMAD = V1Cmad
561400040218      * Decodifica
561500040218     c                   clear                   tibs02ds
561600040218     C                   MOVEL     'C'           t02mod
561700040218     C                   MOVEL     knsif         t02sif
561800040218     C                   MOVEL     'MAR'         t02cod
561900040218     C                   MOVEL     v1cmad        t02ke1
562000040218      *
562100040218     C                   CALL      'TIBS02R'
562200040218     C                   PARM                    KPJBA
562300040218     C                   PARM                    TIBS02DS
562400040218      *
562500040218    2C                   IF        t02err = *BLANKS
562600040218     C                   movel     T02uni        DMAR
562700040218     C                   movel     §MARdesc      v1dmad
562800070323      * se è un motivo di apertura che deve essere protetto in manutenzione accendo il 74
562900070323     c                   if        §marprot = 'S'
563000070323     c                   eval      *in74 = *on
563100070323     c                   endif
563200070323
563300040218    2c                   endif
563400040218      * chiusura reclamo
563500040218     c                   eval      v1ccdt = tgdcdt
563600040218      * Decodifica
563700040218     c                   clear                   tibs02ds
563800040218     C                   MOVEL     'C'           t02mod
563900040218     C                   MOVEL     knsif         t02sif
564000040218     C                   MOVEL     'CHR'         t02cod
564100040218     C                   MOVEL     v1ccdt        t02ke1
564200040218      *
564300040218     C                   CALL      'TIBS02R'
564400040218     C                   PARM                    KPJBA
564500040218     C                   PARM                    TIBS02DS
564600040218      *
564700040218    2C                   IF        t02err = *BLANKS
564800040218     C                   movel     T02uni        DCHR
564900040218     C                   movel     §CHRdesc      v1dcdt
565000040218    2c                   endif
565100150624      *
565200040218      * recupero le note
565300040218      *
565400040218     C                   clear                   FIDNB0DS
565500040421     c                   z-add     v1cant        iB0ant
565600040218     c                   z-add     v1cnut        iB0nut
565700040218     c                   z-add     v1cpdt        iB0pdt
565800040218     c                   movel     'GEN'         iB0tIN
565900040218     c                   movel     'R'           iB0flm
566000040218      *
566100040218     C                   CALL      'FIDNB0R'
566200040218     C                   PARM                    kpjba
566300040218     C                   PARM                    FIDNB0DS
566400040218      *
566500040218      * valorizzo le prime 4 note a video
566600040218      *
566700040218     c                   movel     DB0NO1        v1not1
566800040218     c                   movel     DB0NO2        v1not2
566900040218     c                   movel     DB0NO3        v1not3
567000040218     c                   movel     DB0NO4        v1not4
567100040309      * in caso di manutenzione non do la possibilità di modifcare niente
567200040309     c                   if        Wmod = 'G'
567300040309     c                   seton                                        515253
567400040309     c                   seton                                        54
567500040309     c                   ENDIF
567600070413      * se è un motivo di apertura che deve essere protetto in manutenzione proteggo le note
567700070413     c                   if        §marprot = 'S' and wmod = 'I'
567800070413     c                   eval      *in51 = db0no1 <> *blanks
567900070413     c                   eval      *in52 = db0no2 <> *blanks
568000070413     c                   eval      *in53 = db0no3 <> *blanks
568100070413     c                   eval      *in54 = db0no4 <> *blanks
568200070413     c                   endif
568300040218      *
568400040218      * se ob0piu è valorizzato ci sono + di 4 righe
568500040218     c                   eval      *in29 = (OB0PIU = '+')
568600040218      * reparto gestione
568700150312     c                   eval      v1cpog = %editc(tgdpog:'X')
568800040218     c                   eval      v1crpg = tgdrpg
568900040218      * priorità
569000040218     c                   eval      v1cprt = tgdprt
569100150326     c                   eval      W_prt  = tgdprt
569200150313      * se non è chiuso imposto utente e filiale da azute
569300150313      * e priorità a '1' se è a '0'
569400150313     c                   IF        V1Ccdt = *blanks
569500150313     c                   eval      V1Crpg = KNMUS
569600150313     c                   eval      V1Cpog = %editc(DUTpou:'X')
569700150313     c                   IF        V1Cprt = '0'
569800150313     c                   eval      V1Cprt = '1'
569900150313     c                   ENDIF
570000150313     c                   ENDIF
570100040323      *
570200040323      * Decodifico
570300040323     c                   clear                   tibs02ds
570400040323     C                   MOVEL     'C'           t02mod
570500040323     C                   MOVEL     knsif         t02sif
570600040323     C                   MOVEL     'PRR'         t02cod
570700040323     C                   MOVEL     v1cprt        t02ke1
570800040323      *
570900040323     C                   CALL      'TIBS02R'
571000040323     C                   PARM                    KPJBA
571100040323     C                   PARM                    TIBS02DS
571200040323      *
571300040323    4C                   IF        t02err = *BLANKS
571400040323     C                   movel     T02uni        DPRR
571500040323     C                   movel     §PRRdesc      v1dprt
571600040323     c                   endif
571700150625      *
571800040218      * responsabilità
571900040218     c                   eval      v1crsp = tgdrsp
572000040218      *
572100040218      * P.O. responsabilità
572200040218     c                   movel     tgdpor        v1cpor
572300040218      * decodifica
572400040218    2c                   if        tgdpor > 0
572500040218     C     tgdpor        chain     azorg01l
572600040218    3C                   if        %found(azorg01l)
572700040218     C                   movel     orgdes        v1dpor
572800040218    3c                   endif
572900040218    2c                   endif
573000040218
573100040218    1c                   endif
573200040324      * posizionamento cursore su causale chiusura
573300040324     c                   seton                                        37
573400040324      * Se reclamo chiuso controllo la data chiusura se sono passati più dei
573500040324      * giorni massimi per variare un reclamo chiuso (definiti nella tabella
573600040324      * dati standard REC) proteggo tutti i campi ed emetto un msg di avvertimento
573700041012      * Se utente abilitato a modificare tutto (esempio uff. qualità) non faccio il
573800041012      * controllo (*in07 acceso)
573900041012     c                   if        tgddcd > *zeros and *in50 and not *in07
574000040426
574100040324     c     *iso          movel     tgddcd        Wdataeur
574200040426     c                   z-add     0             giorni
574300040426      *
574400040426     c                   dow       giorni < §recnmgm
574500040426     c                   adddur    1:*d          Wdataeur
574600040426     c                   extrct    wdataeur:*y   kann
574700040426     c                   extrct    wdataeur:*m   kmes
574800040426     c                   extrct    wdataeur:*d   wgiorno
574900040426     c                   clear                   ktfa
575000040426     c                   eval      ktfp = tgdpot
575100040426     c     kazcln        chain     azcln01l
575200040426     c                   if        not%found(azcln01l)
575300040426     c                   leave
575400040426     c                   endif
575500040426     c                   if        MAT(wgiorno) =  'F'
575600040426     c                             or  POM(wgiorno) =  'F'
575700040426     c                   iter
575800040426     c                   else
575900040426     c                   add       1             giorni
576000040426     c                   endif
576100040426     c                   enddo
576200040426      *
576300040324     c                   if        wdataeur < wdataoggi
576400040324      * se non sono in immissione proteggo tutti i campi
576500040324     c                   eval      *in70 = *on
576600040324      * posizionamento cursore su opzione subfile
576700141215      * ·?SE non protetta?
576800141215     c                   eval      *in81 = ((*in74 = *on or  §MARutAp <> 'N')
576900141215     c                                       and
577000141215     c                                      (wMod = 'I'  or  §MARutUt <> 'N'))
577100040324     c                   eval      *in37 = *off
577200040324      *
577300040324     c                   endif
577400040324      *
577500040324     c                   endif
577600040422      *
577700040422     c                   endif
577800150319
577900150319      /free
578000150319       //?Controllo se attivare o no il tasto F11=Variazioni
578100150319         *in17 = *off;
578200150319         chain (V1Cant:V1Cnut:VS1pdt) FITGM01L;
578300150319         IF  %found(FITGM01L);
578400150319           *in17 = *on;
578500150319         ENDIF;
578600150319      /end-free
578700150319
578800040218      *
578900040218     c                   ENDSR
579000040219      *---------------------------------------------------------------*
579100040219      * Routine chiusura chiamata                                     *
579200040219      *---------------------------------------------------------------*
579300040219     c     SR_CHIUDO     BegSR
579400040219
579500040219      * per ogni record di dettaglio reclamo aggiorno i dati della parte alta
579600040219      * del video
579700040219
579800040421     c     Ktgdan        setll     Fitgd01l
579900040219      *
580000040219     c                   Do        *hival
580100040421     c     Ktgdan        reade     fitgd01l
580200040219
580300040219     c                   if        %eof(fitgd01l)
580400040219     c                   leave
580500040219     c                   endif
580600040219
580700040219     c                   eval      tgddar = wdar                                dta ricez. comunicaz
580800040219     c                   movel(p)  v1corr        tgdorr                         ora ricez. comunicaz
580900040219     c                   eval      tgdtco = v1ctco                              tipo comunicazione
581000040219     c                   eval      tgdrsc = v1crsc                              rag.sociale chiamant
581100041213      * se codice cliente diverso da prima scrivo una nota
581200041214     c                   if        tgdksc <> v1cchi and mod_testa = 'S'
581300150318     c                   eval      wtmo = 'K'
581400150318     c                   clear                   dTGMk
581500150318     c                   eval      §MODksco = TGDksc
581600150318     c                   eval      §MODkscn = V1Cchi
581700150318     c                   exsr      WRT_modifiche
581800041213     c                   endif
581900041213
582000041231     c                   eval      tgdksc = v1cchi                              codice cliente
582100040219     c                   eval      tgdclv = v1cclv                              categoria chiamante
582200121008
582300121008      * se il motivo di apertura lo richiede NON memorizzo il KSC
582400121008     c                   clear                   tibs02ds
582500121008     c                   eval      T02mod = 'C'
582600121008     c                   eval      T02sif = knsif
582700121008     c                   eval      T02cod = 'MAR'
582800121008     c                   eval      T02ke1 = TGDmad
582900121008     c                   call      'TIBS02R'
583000121008     c                   parm                    KPJBA
583100121008     c                   parm                    TIBS02DS
583200121008     c                   eval      dMAR = T02uni
583300121008     c                   IF        §MARksc = 'N'
583400121008     c                   clear                   TGDksc
583500121008     c                   ENDIF
583600150625      *
583700150625      * -?SE premuto F4=Fine chiamata (abilitato SOLO in immissione)?
583800150625      *  ? & R.A. chiusa?
583900150625      *  ? & Tipo Comunicazione con Stato (ex Priorità) in tab. "COR"?
584000150625      *  ?=> lo imposto in TGDPRT?
584100150625     c                   clear                   wSavePRT
584200150625     c                   if             *inKD
584300150625     c                             and  TGDcdt    <> *blanks
584400150625     c                   eval      wSavePRT = TGDprt
584500150625     c                   if             §CORstato <> *blanks
584600150625     c                   eval      TGDprt = §CORstato
584700150625     c                   else
584800150625     c                   eval      TGDprt = '9'
584900150625     c                   endif
585000150625      /free
585100150625         // -?Storicizzo Variazioni Stato?
585200150625         IF  wSavePRT <> TGDprt;
585300150625           wtmo = 'S';
585400150625           clear dTGMs;
585500150625           §MODprto = wSavePRT;
585600150625           §MODprtn = TGDprt;
585700150625           exsr WRT_modifiche;
585800150625         ENDIF;
585900150625      /end-free
586000150625     c                   endif
586100040219
586200040219     c                   update    fitgd000
586300040219
586400040219     c                   enddo
586500040219
586600040219      * verifico se esistono per questa riga che confermo dei record di riferimenti
586700040219      * ed evantualmente li deleto e poi riscrivo
586800040219
586900040421     c     ktgran        setll     fitgr01l
587000040219      *
587100040219     c                   do        *hival
587200040421     c     ktgran        reade     fitgr01l
587300040219      *
587400040219     c                   if        not %eof(fitgr01l)
587500040219     c                   delete    fitgr01l
587600040219     c                   else
587700040219     c                   leave
587800040219     c                   endif
587900040219      *
588000040219     c                   enddo
588100040219      * se esiste referente
588200040219     c                   if        v1refe <> *blank
588300040219     c                   clear                   fitgr000
588400040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
588500040421     c                   eval      tgrant = v1cant                              Numero reclamo
588600040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
588700040219     c                   eval      tgrtri = 'R'                                 tipo  riferimento
588800040219     c                   movel     v1refe        tgrrif
588900040219     c                   write     fitgr000
589000040219     c                   endif
589100040219      * se esiste indirizzo e-mail
589200040219     c                   if        v1mail <> *blank
589300040219     c                   clear                   fitgr000
589400040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
589500040421     c                   eval      tgrant = v1cant                              Numero reclamo
589600040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
589700040219     c                   eval      tgrtri = 'M'                                 tipo  riferimento
589800040219     c                   movel     v1mail        tgrrif
589900040219     c                   write     fitgr000
590000040219     c                   endif
590100040219      * se esiste numero telefono e/o numero fax
590200040219     c                   if        v1ntel <> *blank or v1nfax <> *blank
590300040219     c                   clear                   fitgr000
590400040219     c                   eval      tgrpot = v1cpot                              p.o. immette recla
590500040421     c                   eval      tgrant = v1cant                              Numero reclamo
590600040219     c                   eval      tgrnut = v1cnut                              Numero reclamo
590700040219     c                   eval      tgrtri = 'N'                                 tipo  riferimento
590800040219     c                   movel     v1ntel        tgrrif
590900040219     c                   move      v1nfax        tgrrif
591000040219     c                   write     fitgr000
591100040219     c                   endif
591200040219      *
591300040219     c                   ENDSR
591400040219      *
591500040216      *****************************************************************
591600040216      *  Recupero Reclamo in manutenzione
591700040216      *****************************************************************
591800040218     c     SR_RECUPERO   begsr
591900041213      *
592000041213     c                   clear                   mod_testa
592100151202     c                   clear                   $WrtTIIDC
592200150625      *
592300150625      * -?In MODIFICA: proteggo SEMPRE il Tipo Comunicazione?
592400150625     c*//                eval      *in73 = (wMod <> 'I')
592500150625     c                   eval      *in73  = *on
592600040302
592700040302      * preparo la chiave con i dati passati
592800040302
592900040421     c                   eval      keyant = ia0ant
593000040302     c                   eval      keynut = ia0nut
593100040302     c                   eval      keypdt = ia0pdt
593200040304     c                   eval      v1cpdt = ia0pdt
593300040304     c                   eval      v1cnut = ia0nut
593400040421     c                   eval      v1cant = ia0ant
593500040304      * carico il subfile
593600040304     c                   exsr      SR_CARSBF
593700040304
593800040422      * dettaglio
593900040422     c                   exsr      SR_recogg
594000040422
594100040302      * valorizzo anche la parte alta della videata
594200040302
594300040303     c                   eval      v1cpot = tgdpot
594400040303     c                   eval      v1ctco = tgdtco
594500040303      * decodifico
594600150625     c                   clear                   dCOR
594700040303     c                   clear                   tibs02ds
594800040303     C                   MOVEL     'C'           t02mod
594900040303     C                   MOVEL     knsif         t02sif
595000040303     C                   MOVEL     'COR'         t02cod
595100040303     C                   MOVEL     v1ctco        t02ke1
595200040303      *
595300040303     C                   CALL      'TIBS02R'
595400040303     C                   PARM                    KPJBA
595500040303     C                   PARM                    TIBS02DS
595600040303      *
595700040303     C                   movel     T02uni        DCOR
595800040303     C                   movel     §CORdesc      v1dtco
595900040303
596000040303     c                   eval      G02inv = tgddar
596100040303     c                   eval      G02err = '3'
596200040303     c                   call      'XSRDA8'
596300040303     c                   parm                    WLBdat
596400040303     c                   z-add     G02dat        v1cdar                         data ricez.comunicaz
596500040303      *
596600040304     c                   MOVEL     tgdorr        v1corr                         ora  ricez.comunicaz
596700040303
596800040303     c                   clear                   v1cchi
596900040303
597000040303     c                   eval      v1crsc = tgdrsc
597100041209     c                   eval      v1cchi = tgdksc
597200070403     c                   eval      W_v1cchi = tgdksc
597300050118      * decodifica
597400050118
597500050118     C                   clear                   TIBS69DS
597600050118     c                   z-add     v1cchi        I69kac
597700050118     C                   call      'TIBS69R'
597800050118     C                   parm                    tibs69DS
597900050118     C                   parm                    DS_cnaco
598000050118     C                   parm                    DS_cnind
598100050118     C                   parm                    DS_cnclp
598200050118     C                   parm                    DS_fncls
598300050118     C                   eval      wtibs69r = '1'
598400050118     c                   If        o69err = ' '
598500050118     c                   movel     ACOrag        v1dchi
598600050118     c                   endif
598700050118
598800040303     c                   eval      v1cclv = tgdclv
598900040303
599000040303      * recupero i riferimenti reclamo
599100040303
599200040303     c                   clear                   v1refe
599300040303     c                   clear                   v1ntel
599400040303     c                   clear                   v1mail
599500040303     c                   clear                   v1nfax
599600040303      *
599700040421     c     Ktgran2       setll     fitgr01l
599800040303     c                   do        *hival
599900040421     c     Ktgran2       reade     fitgr01l
600000040303
600100040303     c                   if        %eof(fitgr01l)
600200040303     c                   leave
600300040303     c                   endif
600400040303
600500040303      * referente
600600040303     c                   if        tgrtri = 'R'
600700040303     c                   eval      v1refe = tgrrif
600800040303     c                   endif
600900040303      * mail
601000040303     c                   if        tgrtri = 'M'
601100040303     c                   eval      v1mail = tgrrif
601200040303     c                   endif
601300040303      * numeri telefonoici
601400040303     c                   if        tgrtri = 'N'
601500040303     c                   movel     tgrrif        v1ntel
601600040303     c                   move      tgrrif        v1nfax
601700040303     c                   endif
601800040303
601900040303     c                   enddo
602000040303
602100040216     c                   ENDSR
602200150609
602300150609      *****************************************************************
602400150609      *  Ricerca alfabetica
602500150609      *****************************************************************
602600150609     c     SR_RICALF     begsr
602700150609      *
602800150609     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
602900150609     C                   Z-ADD     3             PAXkut                         tipo estrazione
603000150609     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
603100150609     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
603200150609     c                   movel(P)  V1dchi        PAXrag
603300150609     C                   z-add     1             PAXnum
603400150609      *
603500150609     C                   CALL      'XALFA3BR'
603600150609     C                   PARM                    PAXDUT           30
603700150609     C                   PARM                    PAXkut            1 0
603800150609     C                   PARM                    PAXrag           48
603900150609     C                   PARM                    PAXkcc            4 0
604000150609     C                   PARM                    PAXsta            1 0
604100150609     C                   PARM      *blanks       PAXflr           90
604200150609     C                   PARM      *blanks       PAXdit            3
604300150609     C                   PARM                    PAXnum            2 0
604400150609     C                   PARM                    PAXkcm           80
604500150609     C                   PARM                    PAXksm          140
604600150609     C                   PARM                    PAXkdm           60
604700150609      *
604800150609      * NON TROVATO O PREMUTO CMD12
604900150609    2C                   If        paxsta > -1
605000150609     C                   movel     PAXksm        V1Cchi
605100150609     C                   movel     PAXrag        V1dchi
605200150609     c                   endif
605300150609      *
605400150609     c                   endsr
605500150310
605600150310      /free
605700150310       //--------------------------------------------------------------
605800150318       //?F10 = Immissione ORM.
605900150310       //--------------------------------------------------------------
606000150310       BEGSR  SR_F10;
606100150310
606200150417         clear parm01;
606300150417         clear FIOR05DS;
606400150417         pfgs = DUTpou;
606500150417         ppoe = DUTpou;
606600150417         psce = '1';
606700150417         kpjbu = parm01;
606800150417         fior05r (kpjba:fior05ds);
606900150310
607000150310       ENDSR;
607100150318
607200150318       //--------------------------------------------------------------
607300150318       //?F11 = Visualizza Variazioni.
607400150318       //--------------------------------------------------------------
607500150318       BEGSR  SR_F11;
607600150318
607700150318         fidna3r (kpjba:fidna0ds);
607800150318
607900150318       ENDSR;
608000150310      /end-free
608100150310
608200070330      ****************************************************************
608300070330      * F17 = GESTIONE NOTE LDV
608400070330      ****************************************************************
608500070330     C     Sr_f17        BEGSR
608600070330      *
608700131206     c                   clear                   Fnlrn6DS
608800070423
608900070423      * se al momento del richiamo delle note LDV ho delle note RA propongo il
609000070423      * pgm delle note in immissione
609100070423      * se la linea di arrivo della bolla  è abilitata a GEO e l'ho trovata in fnarb
609200070423
609300131206     c                   eval      in6mod  = 'V'
609400140205      * note valorizzate   o   prevista obbligatorietà (in tab. "MAR")
609500070423     c                   If        (v1not1 <> *blanks or v1not2 <> *blanks  or
609600140205     c                              v1not3 <> *blanks or v1not4 <> *blanks  or
609700140205     c                              §MARnotLdV = 'S')    and
609800070423      * po abilitato a geo e bolla in arb  e non sono la sede
609900070423     c                             $abilitato = *on and $fnarb = *on
610000070423      * verifico se sono la filiale di arrivo della bolla
610100130611     c                   move      arblna        w003a
610200130610     c     w003a         lookup    wPOG                                   27
610300070423     c                   If        %found
610400131206     c                   eval      in6mod  = 'I'
610500131206     c                   movel     v1not1        in6not1
610600131206     c                   move      v1not2        in6not1
610700131206     c                   movel     v1not3        in6not2
610800131206     c                   move      v1not4        in6not2
610900070423     c                   endif
611000070423     c                   endif
611100131206     c                   eval      in6aas  = v1anns
611200131206     c                   eval      in6lnp  = v1lnps
611300131206     c                   eval      in6nrs  = v1nrss
611400131206     c                   eval      in6nsp  = v1nsps
611500131206     c                   eval      in6pgm  = 'FIDNA1R'
611600140205      *
611700140205      * se sono una spedizione in consegna passo i dati della distinta del file ARB
611800140205     c                   If        $cons  = *on
611900140205     c     KSPED         chain     Fnarb01l
612000140205     c                   If        %found(Fnarb01l)
612100140205     c                   eval      in6fgs = arbifp
612200140205     c                   eval      in6nfv = arbndc
612300140205     c                   eval      in6dfv = arbddc
612400140205     c                   eval      in6pdr = arbpdc
612500140205     c                   eval      in6cons = 'S'
612600140205     c                   Endif
612700140205     c                   Endif
612800070330
612900131206     c                   call      'FNLRN6R'
613000070330     c                   parm                    kpjba
613100131206     c                   parm                    FNLRN6DS
613200070330
613300070330     c                   endsr
613400040212      ****************************************************************
613500040212      * F18 = GESTIONE NOTE
613600040212      ****************************************************************
613700040212     C     Sr_f18        BEGSR
613800040212      *
613900040212     C                   clear                   FIDNB0DS
614000040218     c                   z-add     v1cpot        iB0pot
614100040421     c                   z-add     v1cant        iB0ant
614200040212     c                   z-add     v1cnut        iB0nut
614300040212     c                   z-add     v1cpdt        iB0pdt
614400040218
614500040219     c                   z-add     Wdar          ib0dar
614600040421     c                   movel(p)  v1corr        ib0orr
614700040218
614800040218     c                   move      v1cmad        ib0mad
614900040218     c                   movel     v1dmad        ib0dma
615000040218
615100040212     c                   movel     'GEN'         iB0tIN
615200040218
615300040218      * - OGGETTO
615400040218
615500040218      * ---------- SPEDIZIONE -----------
615600040218
615700040218     c                   if        v1ctor = 'S'
615800040218
615900040218     c                   movel     spevid        WRKspe
616000040218
616100040219     c                   eval      ib0ogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
616200040219     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
616300040219     c                             ' del ' + WRKann
616400040218      *
616500040218     c                   endif
616600040218
616700040218      * ----------    ORM     -----------
616800040218
616900040218     c                   if        v1ctor = 'O'
617000040218
617100040218     c                   movel     ormvid        WRKorm
617200040218
617300040218     c                   eval      ib0ogg = 'ORM ' + %trim(WRKpoe) +  '/' +
617400040218     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
617500040218     c                             '/' + %trim(WRKnrv)
617600040218      *
617700040218     c                   endif
617800040218
617900040218      * ----------  GENERICO  -----------
618000040218
618100040218     c                   if        v1ctor = 'G'
618200040218     c                   movel     v1cogg        ib0ogg
618300040218     c                   endif
618400040218
618500040212     c                   movel     v1not1        db0no1
618600040212     c                   movel     v1not2        db0no2
618700040212     c                   movel     v1not3        db0no3
618800040212     c                   movel     v1not4        db0no4
618900040219      * se sono in modalità immissione e richiamo una riga da subfile richiamo il pgm note con
619000040219      * FLM = ' ' altrimenti con "M"
619100040305     c                   if        Wmod = 'I'
619200040305     c                   if        wrk_recsbf = 'Y'
619300040219     c                   movel     ' '           iB0flm
619400040219     c                   else
619500040212     c                   movel     'M'           iB0flm
619600040219     c                   endif
619700040305      * modalità manutenzione
619800040305     c                   else
619900040305     c                   movel     'V'           iB0flm
620000040305     c                   endif
620100070413      * se ho già scritto delle note anche se sono in immissione perchè provengo
620200070413      * da gestione dei blocchi consegne vado nel pgm gestione note ijn manutenzione
620300070413     c                   If        Wmod = 'I' and $bloccof3 = *on
620400070413     c                   movel     'V'           iB0flm
620500070413     c                   endif
620600040617      * aggiunto valorizzazione DB0PON
620700040617      * se sono utente particolare che ha un P.O. impostato da tabella passo il P.O. in db0pon
620800040617     c                   eval      db0pon = W_pot
620900040212      *
621000040212     C                   CALL      'FIDNB0R'
621100040212     C                   PARM                    kpjba
621200040217     C                   PARM                    FIDNB0DS
621300040212      *
621400040212      * valorizzo le prime 4 note a video
621500040212      *
621600040212     c                   movel     DB0NO1        v1not1
621700040212     c                   movel     DB0NO2        v1not2
621800040212     c                   movel     DB0NO3        v1not3
621900040212     c                   movel     DB0NO4        v1not4
622000040213      *
622100040213      * se ob0piu è valorizzato ci sono + di 4 righe
622200040213     c                   eval      *in29 = (OB0PIU = '+')
622300040218      *
622400040218      * VALORIZZO FLAG RICHIAMO FIDNB0R
622500040218      *
622600040218     C                   eval      wfidnb0r = 'Y'
622700040212      *
622800040212     c                   ENDSR
622900040216      *
623000040216      *---------------------------------------------------------------*
623100040216      * Gestione WINDOW                                               *
623200040216      *---------------------------------------------------------------*
623300040216     c     SR_GESWIN     BegSR
623400040216
623500040216     c                   do        *hival
623600040216      *
623700040216     c                   exfmt     fia1w01
623800040216      * KF = conferma fine lavoro perdita dei dati
623900040216     c                   if        *inkf
624000040220      * disalloco il file dettagli
624100040220     c                   unlock    fitgd01l
624200040220      *
624300150928     c                   clear                   fidnCAds
624400040421     c                   z-add     v1cant        ic0ant
624500040216     c                   z-add     v1cnut        ic0nut
624600040216     c                   z-add     999           ic0pdt
624700040216      *
624800150928     c                   call      'FIDNCAR'
624900150928     c                   parm                    fidnCAds
625000040216      *
625100040216     c                   leave
625200040216     c                   endif
625300040216      * KL = ritorna alla videata precedente senza deletare nulla
625400040216     c                   if        *inkl
625500040216     c                   leave
625600040216     c                   endif
625700040216
625800040216     c                   enddo
625900040216
626000040216     c                   endsr
626100040223      *---------------------------------------------------------------*
626200040223      * Gestione WINDOW     2                                         *
626300040223      *---------------------------------------------------------------*
626400040223     c     SR_GESWIN2    BegSR
626500040223
626600040223      *
626700040223     c                   exfmt     fia1w02
626800040223      * ENTER = conferma interrogazione bolle arrivi / partenze / ORM
626900040223      * KL = ritorna alla videata
627000040223
627100040223     c                   endsr
627200040406      *---------------------------------------------------------------*
627300040406      * Routine scrittura note aggiuntive automatiche                 *
627400040406      *---------------------------------------------------------------*
627500040406     c     WRT_NOTE      BegSR
627600040406
627700040406     C                   clear                   FIDNB0DS
627800040407      * chiusura
627900141210      *?Verifica telefonica "PINcode"?
628000141210if  1c                   If        Not_PINcode = 'S'
628100141211if  2c                   if        oPINfxx = *blank  and  oPINerr = *blank
628200141210     c                   eval      dB0no1 = 'ESITO: POSITIVO'
628300141210x   2c                   else
628400141210     c                   eval      dB0no1 = 'ESITO: NEGATIVO'
628500141210e   2c                   endif
628600141215     c                   clear                   WLBdat
628700141210     c                   eval      G02inv = ARBddc
628800141215     c                   eval      G02err = '3'
628900141210     c                   call      'XSRDA8'
629000141210     c                   parm                    WLBdat
629100141216     c                   eval      dB0no2 = 'DIST: ' +
629200141210     c                                      %subst( %editc( ARBifp : 'X' ) :
629300141210     c                                              %len( ARBifp ) - 2 : 3 ) +
629400141216     c                                      ' ' +
629500141210     c                                      %trim( %editc( ARBndc : '3' ) ) +
629600141210     c                                      ' DEL ' +
629700141211     c                                      %trim( %editc( G02dat : 'Y' ) )
629800141211     c                   eval      dB0no3 = 'AUT: ' +
629900141210     c                                      %trim( %editc( ARBpdc : 'X' ) )
630000141210e   1c                   EndIf
630100040407
630200040407
630300041214     c                   z-add     tgdant        iB0ant
630400041214     c                   z-add     tgdnut        iB0nut
630500041214     c                   z-add     tgdpdt        iB0pdt
630600040406     c                   movel     'GEN'         iB0tIN
630700141216     c                   z-add     999           iB0pgn
630800040406     c                   movel     'W'           iB0flm
630900050111     c                   eval      db0sts ='N'
631000041214     c                   eval      ib0pot =tgdpot
631100041013     c                   eval      db0pon =Dutpou
631200040406     c                   eval      db0utn =knmus
631300040406
631400040406     C                   CALL      'FIDNB0R'
631500040406     C                   PARM                    kpjba
631600040406     C                   PARM                    FIDNB0DS
631700040407
631800141210     c                   clear                   Not_PINcode
631900040406
632000040406     c                   endSR
632100150316
632200150316      /free
632300150316       //---------------------------------------------------------------*
632400150316       // Routine scrittura file modifiche                              *
632500150316       //---------------------------------------------------------------*
632600150316       BEGSR WRT_Modifiche;
632700150316
632800150316         clear FITGM000;
632900150316         TGMpot = TGDpot;
633000150316         TGMant = TGDant;
633100150316         TGMnut = TGDnut;
633200150316         TGMpdt = TGDpdt;
633300150316         TGMtmo = wtmo;
633400150316         SELECT;
633500150318         WHEN  TGMtmo = 'C';
633600150318           TGMmod = dTGMc;
633700150318         WHEN  TGMtmo = 'K';
633800150318           TGMmod = dTGMk;
633900150318         WHEN  TGMtmo = 'R';
634000150318           TGMmod = dTGMr;
634100150326         WHEN  TGMtmo = 'S';
634200150326           TGMmod = dTGMs;
634300150316         WHEN  TGMtmo = 'U';
634400150316           TGMmod = dTGMu;
634500150316         ENDSL;
634600150316         TGMdmo = %dec(%date());
634700150316         TGMomo = %dec(%time());
634800150316         TGMutn = knmus;
634900150316         TGMpon = DUTpou;
635000150318         TGMnoj = knmeb;
635100150316         write FITGM000;
635200150319         feod FITGM01L;
635300150316
635400150316       ENDSR;
635500150316      /end-free
635600150316
635700150316      *
635800040126      *---------------------------------------------------------------*
635900040126      * Routine Iniziale                                              *
636000040126      *---------------------------------------------------------------*
636100040126     c     *InzSR        BegSR
636200040126      *
636300040129     c     *Entry        plist
636400040129     c                   parm                    KPJBA
636500040213     c                   parm                    FIDNA0DS
636600040126      * Reperisco data corrente
636700040126     c                   time                    w0140
636800040126     c                   move      w0140         wData
636900040126     c                   movel     w0140         wOra
637000040126     c                   clear                   WLBdat
637100040126     c                   eval      G02dat = wData
637200040126     c                   call      'XSRDA8'
637300040126     c                   parm                    WLBdat
637400040126     c                   z-add     G02inv        DateU
637500040324     C     *iso          movel     dateu         Wdataoggi
637600040126      *
637700040126      * Reperisco dati job
637800040126     c                   exsr      DatiJob
637900040126     c                   movel     SDSpgm        VTCpgm
638000130806
638100130806      * Imposto le abilitazioni dell'utente
638200130806     c                   eval      dute01 = UTEfaf
638300041005
638400041005      * Se sono in sede accendo 05
638500041005     c                   eval      *in05=(simfel = 0)
638600041005
638700040213      * Se ricevuto parametro per manutenzione  non valorizzo il p.o. emittente con quello
638800040213      * in linea
638900040213     c                   if        parms   > 1
639000040213      * modalità GESTIONE
639100040213     c                   eval      wmod   = 'G'
639200040217     c                   eval      v1tmod = tit2
639300040303     c                   eval      *in50 = *on
639400040420      * reperisco i programmi che hanno richiamato questo pgm
639500040420     c                   Call      'XRTVSTKR'
639600040420     C                   Parm                    PStack         1000
639700040420     C                   Parm                    PStartPoint       3 0
639800040420     C                   Parm                    PEntries          3 0
639900040420     c                   Movel     Pstack        DSstack
640000040213     c                   else
640100040213     c                   eval      v1cpot = dutpou
640200040616      *
640300040213      * modalità IMMISSIONE
640400040213     c                   eval      wmod   = 'I'
640500040217     c                   eval      v1tmod = tit1
640600040213     c                   endif
640700040617      * verifico se sto inserendo o modificando un reclamo da parte di un utente
640800040617      * che deve avere come p.o. apertura e gestione quello della tabella UGR
640900040617      * tabella UGR
641000040617     c                   movel     KNMUS         w_ute
641100040617     c                   clear                   tibs02ds
641200040617     C                   MOVEL     'C'           t02mod
641300040617     C                   MOVEL     knsif         t02sif
641400040617     C                   MOVEL     'UGR'         t02cod
641500040617     C                   MOVEL     W_ute         t02ke1
641600040617      *
641700040617     C                   CALL      'TIBS02R'
641800040617     C                   PARM                    KPJBA
641900040617     C                   PARM                    TIBS02DS
642000040617      *
642100040617     C                   IF        t02err = *BLANKS
642200040617      *
642300040617     c                   movel     t02uni        DUGR
642400040617      * se P.O. da forzare diverso da zero imposto in automatico
642500040617     c                   if        §UGRpofo > 0
642600040617     c                   eval      v1cpot = §UGRpofo
642700040617     c                   eval      W_pot  = §UGRpofo
642800040617     c                   endif
642900040617      *
643000040617     c                   endif
643100040324      * reperisco i dati standard RECLAMI
643200040324      * Controllo
643300040324     c                   clear                   tibs02ds
643400040324     C                   MOVEL     'C'           t02mod
643500040324     C                   MOVEL     knsif         t02sif
643600040324     C                   MOVEL     'REC'         t02cod
643700040324     C                   MOVEL     '1'           t02ke1
643800040324      *
643900040324     C                   CALL      'TIBS02R'
644000040324     C                   PARM                    KPJBA
644100040324     C                   PARM                    TIBS02DS
644200040324      *
644300040324     C                   IF        t02err = *BLANKS
644400040324     C                   movel     T02uni        DREC
644500040324     c                   endif
644600041012      * verifico se sono un utente che può modficare tutto senza controlli
644700130806     c                   if        §UTEvrac = 'S'
644800041012     c                   eval      *in07 = *on
644900041012     c                   endif
645000041005      * se sono in sede apro il file TITAS
645100041005     c                   If        *in05
645200041005     c                   open      titas30c
645300041005     c                   else
645400041005     c                   open      fnblp01l
645500041005     c                   open      fnarb01l
645600140429     c                   open      FIAR401L
645700140429     c                   open      FIAR501L
645800041005     c                   endif
645900151130      *
646000151130      /free
646100151130
646200151130         // -?Caricamento filiali x Estero?
646300151130         clear  TRUL31ds;
646400151130         I31abi = 'EE';
646500151130         trul31r ( KPJBA : TRUL31ds );
646600151130         wFILest_ds = O31pog;
646700151130
646800151130      /end-free
646900151130      *
647000070323      * se non sono in sede verifico se il PO è abilitato a Geo telefonate e mi carico la £6
647100070323     c                   if        not *in05
647200070323     c                   exsr      sr_carl6
647300070323     c                   endif
647400040126      *
647500040126     c                   EndSr
647600040126
647700070323      *---------------------------------------------------------------*
647800070323      * Recupero i P.O.  della £6                                     *
647900070323      *---------------------------------------------------------------*
648000070323     c     Sr_CARL6      begsr
648100070323
648200070323      * Recupero i PO della £6
648300070323      *
648400070323     c                   clear                   trul06ds
648500070323     c                   move      '£6'          d06cod
648600070323     c                   movel     Dutpou        d06key
648700070323     c                   movel(p)  trul06ds      KPJBU
648800070323
648900070323     c                   call      'TRUL06R'
649000070323     c                   parm                    kpjba
649100070323     c                   movel     kpjbu         trul06ds
649200070323     c                   movea     lin           L6
649300070323
649400070323      * verico se PO abilitato alla gestione telefonate GEO
649500070323
649600070323     c                   clear                   og148
649700070323     c     l6(1)         chain     azorg01l
649800070323     c                   if        %found(azorg01l)
649900070323     c                   movel     orgde8        og148
650000070323     c                   endif
650100070323
650200070323     c                   If        §oggeot = 'S'
650300070323     c                   eval      $abilitato = *on
650400070608     c                   eval      *in08 = *on
650500070323     c                   endif
650600130610
650700130611      /free
650800130611       //?Imposto abilitazione utente
650900130611         clear wpog;
651000130611         SELECT;
651100130611       //?Imposto abilitazione manutenzione bolle di filiale
651200130611           WHEN  §UTEmbf <> *blanks;
651300130611             wabi = §UTEmbf;
651400130611           WHEN  UTEaut <> *blanks;
651500130611             wabi = UTEaut;
651600130611           WHEN  DUTlpo = '1';
651700130611             wabi = 'TP';
651800130611           WHEN  DUTlpo = '2';
651900130611             wabi = 'PO';
652000130611         ENDSL;
652100140123
652200130611      /end-free
652300130611
652400130610      * controllo se ok l'abilitazione dell'utente
652500130611     c                   clear                   TIBS02DS
652600130611     c                   clear                   dLAT
652700130611     c                   eval      T02mod = 'C'
652800130611     c                   eval      T02sif = knsif
652900130611     c                   eval      T02cod = 'LAT'
653000130611     c                   movel(p)  wabi          T02ke1
653100130611     c                   call      'TIBS02R'
653200130611     c                   parm                    kpjba
653300130611     c                   parm                    TIBS02DS
653400130611if  1c                   IF        T02err = *blanks
653500130611     c                   eval      dLAT = T02uni
653600130611e   1c                   ENDIF
653700130611
653800130611      * Se abilitazione trovata e OK
653900130611if  1c                   IF        T02err = *blanks and §LATabi = *blanks
654000130610      * Reperimento dei P.O. gestibili dall'utente
654100130610     c                   clear                   TRUL31DS
654200130610     c                   eval      I31abi = wabi
654300130610     c                   eval      I31cdi = DUTdis
654400130610     c                   eval      I31car = DUTare
654500130610     c                   eval      I31cpo = DUTpou
654600130610     c                   call      'TRUL31R'
654700130611     c                   parm                    kpjba
654800130610     c                   parm                    TRUL31DS
654900130611if  2c                   IF        O31pog > *zeros
655000130610     c                   movea     O31pog        wpog
655100130611x   2c                   ENDIF
655200130611    1c                   ENDIF
655300130611
655400130611      * se sk delle filiali abilitate all'utente vuota impsoto il primo elemento della £6
655500130611     c                   IF        wpog(1) = *blanks
655600130611     c                   eval      wpog(1) = %editc(l6(1):'X')
655700130611     c                   ENDIF
655800150317
655900150317      /free
656000150317       //?Imposto abilitazione utente x manutenzione RA
656100150317         clear wFIL_ds;
656200150317         SELECT;
656300150317         WHEN  §UTErac <> *blanks;
656400150317           wabi = §UTErac;
656500150317         WHEN  UTEaut <> *blanks;
656600150317           wabi = UTEaut;
656700150317         WHEN  DUTlpo = '1';
656800150317           wabi = 'TP';
656900150317         WHEN  DUTlpo = '2';
657000150317           wabi = 'PO';
657100150317         ENDSL;
657200150317       //?Reperimento delle filiali gestibili dall'utente per manutenzione RA
657300150317         clear TRUL31DS;
657400150317         I31abi = wabi;
657500150317         I31cdi = DUTdis;
657600150317         I31car = DUTare;
657700150317         I31cpo = DUTpou;
657800150317         trul31r (kpjba : TRUL31DS);
657900150317         wFIL_ds = O31pog;
658000150317         IF  wFIL_ds = *blanks;
658100150317           sk_wFIL(01) = %editc(DUTpou:'X');
658200150317         ENDIF;
658300150317      /end-free
658400070323
658500070323     c                   endsr
658600070323
658700150601      *---------------------------------------------------------------*
658800150601      * Conto quante RA ci sono a fronte dello stesso oggetto         *
658900150601      *---------------------------------------------------------------*
659000150601     c     Sr_countRA    BegSr
659100150601      *
659200150601      *
659300150603     c                   if        V1ctor = 'O'
659400150603     c                   movel(p)  ormvid        W_V1cogg
659500150603     c                   endif
659600150603     c                   if        V1ctor = 'S'
659700150603     c                   movel(p)  spevid        W_V1cogg
659800150603     c                   endif
659900150603      *
660000150603     c     Ktgd32        setll     fitgd32l
660100150603     c     Ktgd32        reade     fitgd32l
660200150603     c                   if        %eof(Fitgd32l)
660300150603     c                   eval      IA1INS = 'N'
660400150603     c                   else
660500150603     c     Ktgd32        reade     fitgd32l
660600150603      * se c'è una sola RA non abilito F16 nei programmi di interrogazione bolle o ORM
660700150603      * se più di una lo abilito
660800150603     c                   if        not %eof(Fitgd32l)
660900150603     c                   eval      IA1INS = 'I'
661000150603     c                   else
661100150603     c                   eval      IA1INS = 'N'
661200150603     c                   Endif
661300150603
661400150603     c                   Endif
661500150603
661600150601     c                   endsr
661700150601
661800040126      *---------------------------------------------------------------*
661900040126      * Reperimento Dati del job (Utente/Operativi)                   *
662000040126      *---------------------------------------------------------------*
662100040126     c     DatiJob       BegSr
662200040126      *
662300040126     c     *dtaara       define    §azute        azuteds
662400040126     c     *dtaara       define    §datiute      ddatiute
662500040126      *
662600040126     c                   in(E)     *dtaara
662700040126     c                   IF        %ERROR or RSUT = *blanks
662800040126     c                   clear                   Tibs34Ds
662900040126     c                   call      'TIBS34R'
663000040126     c                   parm                    Tibs34Ds
663100040126     c                   in        *dtaara
663200040126     c                   ENDIF
663300040126      *
663400040126     c                   EndSr
663500040126
663600130513** -?Msg:?Messaggi di Errore?------------------------------------------------*
663700040205Codice cliente errato                                                                    1
663800040610Inserire l'oggetto della richiesta                                                       2
663900040211Inserire correttamente numero Spedizione                                                 3
664000040211Spedizione inesistente                                                                   4
664100040211Inserire correttamente numero ORM                                                        5
664200040211Numero ORM inesistente                                                                   6
664300040211Inserire correttamente il "CHI CHIAMA"                                                   7
664400040610Inserire motivo apertura richiesta                                                       8
664500150318Filiale responsabile errata                                                              9
664600040213Inserire Tipo Comunicazione                                                             10
664700040217Data errata                                                                             11
664800040217Ora errata                                                                              12
664900040217Inserire il tipo oggetto                                                                13
665000040610Prima di dare F4 inserire almeno una richiesta                                          14
665100040219Inserire la Ragione Sociale del chiamante                                               15
665200040220Per questo oggetto 'Chi Chiama' non ammesso                                             16
665300040223Tipo comunicazione errata                                                               17
665400040610Motivo apertura richiesta errato                                                        18
665500040610Codice chiusura richiesta errato                                                        19
665600150429Reparto/Utente in gestione errato                                                       20*#
665700040610Data richiesta non deve essere maggiore della data odierna                              21
665800040315Non si può interrogare un oggetto generico                                              22
665900150318Filiale di appartenenza dell'utente diversa dalla filiale gestione                      23
666000150312Inserire priorità richiesta                                                             24Libero
666100150312Priorità richiesta errata                                                               25Libero
666200150318Se responsabilità Vettore è obbligatorio inserire la Fil. responsabile                  26 LIBERO
666300040610Utente non abilitato alla gestione richieste                                            27
666400150429Inserire utente in gestione esistente nei profili della Filiale                         28*Libero
666500150429Inserire reparto/utente in gestione                                                     29 #
666600040610Oggetto presente in altre richieste Enter per forzare                                   30
666700040707Oggetto non presente in archivi                                                         31
666800041210Codice Chiamante mancante F4 per forzare la chiusura                                    32
666900070608Variazione non effettuata                                                               33
667000070608Causale variazione errata o non ammessa                                                 34
667100071019Variazione non consentita                                                               35
667200071018Variazione non consentita bolla non in gestione alla Filiale                            36
667300120914Questa Richiesta Assistenza va chiusa con Codice Chiusura                               37
667400120919Immettere le note                                                                       38
667500131119Per il motivo inserito l'oggetto dev'essere                                             39
667600140213Non utilizzabile. Proposto in automatico confermando la R.A.                            40
667700140430Variazione possibile solo su bolle in PARTENZA dalla propria filiale                    41 <
667800140430Motivo Apertura " 17" utilizzabile solo su bolle in ARRIVO                              42 <
667900140923Tipo comunicazione non utilizzabile in gestione R.A.                                    43
668000141029Ora richiesta consegna errata                                                           44
668100141210Motivo Apertura utilizzabile solo per spedizioni in consegna oggi                       45 «
668200141210Motivo Apertura utilizzabile solo per spedizioni che prevedono un "PINcode"             46 «
668300141218Manca il "PINcode" per questa spedizione                                                47 «
668400150429Filiale gestione errata                                                                 48
668500150422Inserire il telefono e/o la mail                                                        49 #
668600150422Reparto gestione errato                                                                 50 #
668700150717Motivo Apertura impostabile solo da utenti di sede                                      51
668800150723Chiusura non consentita da SEDE                                                         52
668900150723Richiesta Assistenza non gestibile da SEDE                                              53
669000150911C.A.P. errato                                                                           54
669100151130Motivo Apertura " 18" NON utilizzabile per bolle EXPORT                                 55
669200151130Esiste già almeno una disposizione di Dirottamento per questa spedizione                56
