000100141128       //==============================================================
000200150112       //?TRKC20R * Interrogazione clienti in Campagna                 ?
000300141128       //==============================================================
000400141128
000500141128       //--------------------------------------------------------------
000600141128       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700141128       //--------------------------------------------------------------
000800141128
000900141128     /*PRM  dbgview(*source)
001000141128     /*END
001100141128
001200141128       //--------------------------------------------------------------
001300141128       //?Specifiche di controllo.                                     ?
001400141128       //--------------------------------------------------------------
001500141128
001600141128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700141201     h dftactgrp(*no) actgrp(*caller)
001800141128
001900141128       //--------------------------------------------------------------
002000141128       //?Dichiarazione file.                                          ?
002100141128       //--------------------------------------------------------------
002200141128
002300141128       // -?Campagne Commerciali?
002400141128     fTICMP01L  if   e           k disk
002500141128
002600141128       // -?Clienti in Campagne Commerciali?
002700141128     fTICMC01L  if   e           k disk
002800141128
002900141128       // -?Anagrafica Commerciali?
003000141128     fAZCMM01L  if   e           k disk
003100141128
003200141128       // -?Organigramma filiale/aziendale?
003300141128     fAZORG01L  if   e           k disk
003400141128
003500141128       // -?Tabelle?
003600141128     fTABEL00F  if   e           k disk
003700141128     fTNTBE01L  if   e           k disk
003800141128
003900141128       // -?Video Gestione?
004000150112     fTRKC20D   cf   e             workstn
004100141128     f                                     indds( IndDspF )
004200141128     f                                     infds( InfDspF )
004300141128
004400141128       //--------------------------------------------------------------
004500141128       //?Definizione costanti.                                        ?
004600141128       //--------------------------------------------------------------
004700141128
004800141128       // -?Codice autorizzazione profilo Tariffe Clienti?
004900141128     d c_Aut_TC        c                   const('§utegtc')
005000141128
005100141128       // -?Codice Utente fisso (in tabelle, anagrafiche, ecc.)?
005200141128     d c_KUT           c                   const(1)
005300141128
005400141128       // -?Tasti funzionali a video?
005500141128     d c_F01           c                   const(x'31')
005600141128     d c_F02           c                   const(x'32')
005700141128     d c_F03           c                   const(x'33')
005800141128     d c_F04           c                   const(x'34')
005900141128     d c_F05           c                   const(x'35')
006000141128     d c_F06           c                   const(x'36')
006100141128     d c_F07           c                   const(x'37')
006200141128     d c_F08           c                   const(x'38')
006300141128     d c_F09           c                   const(x'39')
006400141128     d c_F10           c                   const(x'3A')
006500141128     d c_F11           c                   const(x'3B')
006600141128     d c_F12           c                   const(x'3C')
006700141128     d c_F13           c                   const(x'B1')
006800141128     d c_F14           c                   const(x'B2')
006900141128     d c_F15           c                   const(x'B3')
007000141128     d c_F16           c                   const(x'B4')
007100141128     d c_F17           c                   const(x'B5')
007200141128     d c_F18           c                   const(x'B6')
007300141128     d c_F19           c                   const(x'B7')
007400141128     d c_F20           c                   const(x'B8')
007500141128     d c_F21           c                   const(x'B9')
007600141128     d c_F22           c                   const(x'BA')
007700141128     d c_F23           c                   const(x'BB')
007800141128     d c_F24           c                   const(x'BC')
007900141128     d c_Enter         c                   const(x'F1')
008000141128     d c_RollDown      c                   const(x'F4')
008100141128     d c_RollUp        c                   const(x'F5')
008200141128
008300141128       // -?Costante per controllo "caratteri solo numerici"?
008400141128     d c_Digits        c                   const('0123456789')
008500141128
008600141128       //--------------------------------------------------------------
008700141128       //?Definizione schiere.                                         ?
008800141128       //--------------------------------------------------------------
008900141128
009000141128       // -?Messaggi di errore?
009100141128     d sk_Msg          s             78    dim(27)  ctdata  perrcd( 1)
009200141128
009300141128       // -?Filiali e Aree?
009400141128     d c_Max_Fil       c                   const(350)
009500141128     d sk_Fil          s                   like(ORGfil)
009600141128     d                                     dim(c_Max_Fil)  inz
009700141128     d sk_Car          s                   like(ORGcar)
009800141128     d                                     dim(c_Max_Fil)  inz(*hival)
009900141128
010000141128       // -?Codici importanza clienti da tabella "IC"?
010100141128     d sk_IC           s              1    dim(10)
010200141128
010300141128       // -?Categorie merceologiche da tabella "1L"?
010400141128     d sk_1L           s              5    dim(50)
010500141128
010600141128       // -?Concorrenti  possibili da tab. "IFS"/10?
010700141128     d sk_IFS_conc     s              4    dim(30)
010800141128       // -?Concorrenti Logistica possibili da tab. "IFS"/40?
010900141128     d sk_IFS_log      s              4    dim(30)
011000141128       // -?Paesi Estero possibili da tab. "IFS"/50?
011100141128     d sk_IFS_paesi    s              4    dim(30)
011200141128
011300141128       //--------------------------------------------------------------
011400141128       //?Definizione aree dati.                                       ?
011500141128       //--------------------------------------------------------------
011600141128
011700141128       // -?Dati utente?
011800141128     d §AzUte        e ds                  extname(AZUTE00F)
011900141128     d                                     dtaara
012000141128     d §DatiUte      e ds                  extname(dDatiUte)
012100141128     d                                     dtaara
012200141128
012300141128       //--------------------------------------------------------------
012400141128       //?Definizione strutture dati.                                  ?
012500141128       //--------------------------------------------------------------
012600141128
012700141128       // -?Status ds?
012800141128     d Status         sds
012900141128     d   SDSpgm          *proc
013000141128
013100141128       // -?InfDS?
013200141128     d InfDspF         ds
013300141128     d   dsp_aid             369    369a
013400141128     d*//sfl_rrn             376    377i 0
013500141128     d*//min_nrr             378    379i 0
013600141128     d*//num_rcds            380    381i 0
013700141128
013800141128       // -?Indicatori su DspF?
013900141128     d IndDspF         ds
014000141128         // -?Condizionamento/Evidenziazione Tasti Funzionali?
014100141128     d   F5_Evid                       n   overlay( IndDspF : 05 )
014200141128         // -?Emissione messaggio di errore?
014300141128     d   errMessage                    n   overlay( IndDspF : 28 )
014400141128     d   VisualCAR                     n   overlay( IndDspF : 41 )
014500141128         // -?Posizionamento cursore & segnalazione errore?
014600141128     d   PosCurKSC                     n   overlay( IndDspF : 51 )
014700141128     d   PosCurOSF                     n   overlay( IndDspF : 52 )
014800141128     d   PosCurCIC1                    n   overlay( IndDspF : 53 )
014900141128     d   PosCurCIC2                    n   overlay( IndDspF : 54 )
015000141128     d   PosCurCIC3                    n   overlay( IndDspF : 55 )
015100141128     d   PosCurCIC4                    n   overlay( IndDspF : 56 )
015200141128     d   PosCurCIC5                    n   overlay( IndDspF : 57 )
015300141128     d   PosCurPRV                     n   overlay( IndDspF : 58 )
015400141128     d   PosCurCAP                     n   overlay( IndDspF : 59 )
015500141128     d   PosCurFLT                     n   overlay( IndDspF : 60 )
015600141128     d   PosCurCAR                     n   overlay( IndDspF : 61 )
015700141128     d   PosCurSCT1                    n   overlay( IndDspF : 62 )
015800141128     d   PosCurSCT2                    n   overlay( IndDspF : 63 )
015900141128     d   PosCurSCT3                    n   overlay( IndDspF : 64 )
016000141128     d   PosCurCMU                     n   overlay( IndDspF : 65 )
016100141128     d   PosCurNCM                     n   overlay( IndDspF : 66 )
016200141128     d   PosCurTCM                     n   overlay( IndDspF : 67 )
016300141128     d   PosCurEAT                     n   overlay( IndDspF : 68 )
016400141128     d   PosCurBRTda                   n   overlay( IndDspF : 69 )
016500141128     d   PosCurBRTa                    n   overlay( IndDspF : 70 )
016600141128     d   PosCurNRFda                   n   overlay( IndDspF : 71 )
016700141128     d   PosCurNRFa                    n   overlay( IndDspF : 72 )
016800141128     d   PosCurNROda                   n   overlay( IndDspF : 73 )
016900141128     d   PosCurNROa                    n   overlay( IndDspF : 74 )
017000141128     d   PosCurNERda                   n   overlay( IndDspF : 75 )
017100141128     d   PosCurNERa                    n   overlay( IndDspF : 76 )
017200141128     d   PosCurNAEda                   n   overlay( IndDspF : 77 )
017300141128     d   PosCurNAEa                    n   overlay( IndDspF : 78 )
017400141128     d   PosCurNTRda                   n   overlay( IndDspF : 79 )
017500141128     d   PosCurNTRa                    n   overlay( IndDspF : 80 )
017600141128     d   PosCurEST1                    n   overlay( IndDspF : 81 )
017700141128     d   PosCurEST2                    n   overlay( IndDspF : 82 )
017800141128     d   PosCurEST3                    n   overlay( IndDspF : 83 )
017900141128     d   PosCurPKGda                   n   overlay( IndDspF : 84 )
018000141128     d   PosCurPKGa                    n   overlay( IndDspF : 85 )
018100141128     d   PosCurCONC1                   n   overlay( IndDspF : 86 )
018200141128     d   PosCurCONC2                   n   overlay( IndDspF : 87 )
018300141128     d   PosCurCONC3                   n   overlay( IndDspF : 88 )
018400141128     d   PosCurINT                     n   overlay( IndDspF : 89 )
018500141128     d   PosCurDIL1                    n   overlay( IndDspF : 90 )
018600141128     d   PosCurDIL2                    n   overlay( IndDspF : 91 )
018700141128     d   PosCurLOS                     n   overlay( IndDspF : 92 )
018800141128     d   PosCurCONCL1                  n   overlay( IndDspF : 93 )
018900141128     d   PosCurCONCL2                  n   overlay( IndDspF : 94 )
019000141128     d   PosCurCONCL3                  n   overlay( IndDspF : 95 )
019100141128         // -?Riemissione videata?
019200141128     d   errGenerico                   n   overlay( IndDspF : 99 )
019300141128
019400141128       // -?Campo unico per Categoria Merceologica?
019500141128     d V01sct_ds       ds                  inz
019600141128     d   V01sct1                           inz
019700141128     d   V01sct2                           inz
019800141128     d   V01sct3                           inz
019900141128     d     sk_V01sct                       overlay( V01sct_ds : 1 )
020000141128     d                                     like(V01sct1)  dim(3)
020100141128     d                                     inz
020200141128
020300141128       // -?Campo unico per Importanza Clienti?
020400141128     d V01cic_ds       ds                  inz
020500141128     d   V01cic1                           inz
020600141128     d   V01cic2                           inz
020700141128     d   V01cic3                           inz
020800141128     d   V01cic4                           inz
020900141128     d   V01cic5                           inz
021000141128     d     sk_V01cic                       overlay( V01cic_ds : 1 )
021100141128     d                                     like(V01cic1)  dim(5)
021200141128     d                                     inz
021300141128
021400141128       // -?Campo unico per Paesi Estero?
021500141128     d V02est_ds       ds                  inz
021600141128     d   V02est1                           inz
021700141128     d   V02est2                           inz
021800141128     d   V02est3                           inz
021900141128     d     sk_V02est                       overlay( V02est_ds : 1 )
022000141128     d                                     like(V02est1)  dim(3)
022100141128     d                                     inz
022200141128
022300141128       // -?Campo unico per Concorrenti?
022400141128     d V02conc_ds      ds                  inz
022500141128     d   V02conc1                          inz
022600141128     d   V02conc2                          inz
022700141128     d   V02conc3                          inz
022800141128     d     sk_V02conc                      overlay( V02conc_ds : 1 )
022900141128     d                                     like(V02conc1)  dim(3)
023000141128     d                                     inz
023100141128
023200141128       // -?Campo unico per Concorrenti Logistica?
023300141128     d V02concL_ds     ds                  inz
023400141128     d   V02concL1                         inz
023500141128     d   V02concL2                         inz
023600141128     d   V02concL3                         inz
023700141128     d     sk_V02concL...
023800141128     d                                     overlay( V02concL_ds : 1 )
023900141128     d                                     like(V02concL1)  dim(3)
024000141128     d                                     inz
024100141128
024200141128       // -?Parametri ricevuti?
024300141128     d KPJBA         e ds
024400150112     d TRKC20ds      e ds
024500141128
024600150112       // -?Parametri aggiuntivi per TNTAI1R e TRKC21R?
024700150112     d TRKC20ds2     e ds                  extname(TRKC20ds)
024800141128     d                                     inz  qualified
024900141128
025000141128       // -?Tabella 05 = Codici Area?
025100141128     d ds05          e ds                  inz  qualified
025200141128
025300141128      *// // -?Tabella IC = Importanza Clienti?
025400141128     d*// dsIC          e ds                  based(nullPtr)  qualified
025500141128
025600141128       //--------------------------------------------------------------
025700141128       //?Definizione variabili globali.                               ?
025800141128       //--------------------------------------------------------------
025900141128
026000141128       // -?Flags booleani?
026100141128     d $Fine           s               n   inz
026200141128     d $InzD01         s               n   inz(*on)
026300141128     d $InzD02         s               n   inz(*on)
026400141128
026500141128       // -?Indici di schiera?
026600141128     d xx              s              4  0 inz
026700141128     d yy              s              4  0 inz
026800141128
026900141128       // -?Campi associati al video?
027000141128     d $Video          s              2    inz('D1')
027100141128
027200141128       // -?Campi di Comodo?
027300141128     d wDIL1           s              8  0 inz
027400141128     d wDIL2           s              8  0 inz
027500141128
027600141128       //--------------------------------------------------------------
027700141128       //?Definizione prototipi procedure usate.                       ?
027800141128       //--------------------------------------------------------------
027900141128
028000141128       // -?Reperimento dati utente?
028100141128     d TIBS34ds      e ds                  inz
028200141128      /copy gaitrasrc/srcProtoPR,TIBS34R
028300141128
028400141128       // -?Controllo abilitazioni utente?
028500141128     d TNTAA1ds      e ds                  inz
028600141128      /copy gaitrasrc/srcProtoPR,TNTAA1C
028700141128
028800141128       // -?Caricamento Filiali gestite?
028900141128     d TRUL31ds      e ds                  inz
029000141128     d   sk_POG                       3    overlay( O31pog )  dim(250)
029100141128      /copy gaitrasrc/srcProtoPR,TRUL31R
029200141128
029300141128       // -?Controllo/Decodifica cliente?
029400141128      /copy gaitrasrc/srcProtoPI,TIBS69R
029500141128      /copy gaitrasrc/srcProtoPR,TIBS69R
029600141128
029700141128       // -?Controllo congruenza CAP e Provincia?
029800141128     d FNLV13ds      e ds
029900141128     d   I13af0      e                     inz('S')
030000141128     d   I13cng      e                     inz('S')
030100141128     d TISI95ds      e ds                  inz
030200141128     d   I95tcn      e                     inz('3')
030300141128     d fnlv13r         pr                  extpgm('FNLV13R')
030400141128     d  kpjba                              likeds(kpjba)    const
030500141128     d  fnlv13ds                           likeds(FNLV13ds)
030600141128     d  tisi95ds                           likeds(TISI95ds)
030700141128
030800141128       // -?Selezione di un Commerciale (unificante e non)?
030900141128      /copy gaitrasrc/srcprotoPI,TRMK43R_1
031000141128      /copy gaitrasrc/srcprotoPR,TRMK43R
031100141128
031200141128       // -?Interrogazione Campagne?
031300150112     d TRKC02ds      e ds                  inz
031400150112     d trkc02r         pr                  extpgm('TRKC02R')
031500141128     d   kpjba                             likeds(kpjba)
031600150112     d   trkc02ds                          likeds(TRKC02ds)
031700141128
031800141128       // -?Interrogazione Clienti in Campagna?
031900150112     d TRKC21ds      e ds                  inz  qualified
032000150112     d trkc21r         pr                  extpgm('TRKC21R')
032100141128     d   kpjba                             likeds(kpjba)
032200150112     d   trkc21ds                          likeds(TRKC21ds)
032300150112     d   trkc20ds2                         likeds(TRKC20ds)
032400141128
032500141128       // -?Interrogazione Clienti (NON in Campagna)?
032600141128     d TNTAI1ds      e ds                  inz  qualified
032700141128     d tntaI1r         pr                  extpgm('TNTAI1R')
032800141128     d   kpjba                             likeds(kpjba)
032900141128     d   tntaI1ds                          likeds(TNTAI1ds)
033000150112     d   trkc20ds2                         likeds(TRKC20ds)
033100141128
033200141128       // -?Interrogazione singolo Cliente?
033300141128     d TNTAI2ds      e ds                  inz  qualified
033400141128     d tntaI2r         pr                  extpgm('TNTAI2R')
033500141128     d   kpjba                             likeds(kpjba)
033600141128     d   tntaI2ds                          likeds(TNTAI2ds)
033700141128
033800141128       // -?Ricerca Tabelle (selezione multipla) - TABEL00F?
033900141128     d idTabella       s              2    inz
034000141128     d Ordinamento     s              1    inz
034100141128     d idElemento      s              8    inz
034200141128     d TastoUscita     s              1    inz
034300141128      /copy gaitrasrc/srcProtoPR,TRUL19R
034400141128
034500141128       // -?Ricerca/Controllo tabelle - TNTBE00F?
034600141128     d TIBS02ds      e ds                  inz
034700141128      /copy gaitrasrc/srcProtoPR,TIBS02R
034800141128
034900141128      *// // -?Reperimento dati tabelle (TABEL00F & TNTBE00F)?
035000141128      *///copy gaitrasrc/srcProtoPI,TRULTAB
035100141128      *///copy gaitrasrc/srcProtoPR,TRULTAB
035200141128
035300141128      *// // -?Controllo se cliente in Campagna?
035400150112     d*// TRKC70ds      e ds                  inz
035500150112     d*// TRKC70R         pr                  extpgm('TRKC70R')
035600141128     d*//   kpjba                             likeds(kpjba)
035700150112     d*//   trkc70ds                          likeds(TRKC70ds)
035800141128
035900141128       // -?Controllo/Inversione Date?
036000141128     d WLBdat          ds                  inz
036100141128     d   G02dat                1      8  0 inz
036200141128     d   G02inv                9     16  0 inz
036300141128     d   G02err               17     17    inz
036400141128     d   G02tgi               18     22  0 inz
036500141128      /copy gaitrasrc/srcProtoPR,XSRDA8
036600141128
036700141128       //--------------------------------------------------------------
036800141128       //?Definizione key-list.                                        ?
036900141128       //--------------------------------------------------------------
037000141128
037100141128       // -?File TICMP01L?
037200141128     d keyTICMP01    e ds                  extname( TICMP01L : *key )
037300141128     d                                     prefix(k_)  inz
037400141128
037500141128       // -?File TICMC01L?
037600141128     d keyTICMC01    e ds                  extname( TICMC01L : *key )
037700141128     d                                     prefix(k_)  inz
037800141128
037900141128       // -?File AZCMM01L?
038000141128     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
038100141128     d                                     prefix(k_)  inz
038200141128
038300141128       // -?File TABEL00F?
038400141128     d keyTABEL00    e ds                  extname( TABEL00F : *key )
038500141128     d                                     prefix(k_)  inz
038600141128
038700141128       // -?File TNTBE01L?
038800141128     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
038900141128     d                                     prefix(k_)  inz
039000141128
039100141128       //--------------------------------------------------------------
039200141128       //?M A I N - L I N E                                            ?
039300141128       //--------------------------------------------------------------
039400141128
039500141128     c     *Entry        plist
039600141128     c                   parm                    KPJBA
039700150112     c                   parm                    TRKC20ds
039800141128
039900141128      /free
040000141128
040100141128       // -?Operazioni iniziali?
040200141128       exsr  sr_RoutInz;
040300141128
040400141128       // -?Ciclo di gestione del file video?
040500141128       DoW  $Fine = *off;
040600141128
040700141128         select;
040800141128
040900141128           // -?Gestione videata D1 (parzializzazioni)?
041000141128           when  $Video = 'D1';
041100141128             exsr  sr_GesD01;
041200141128
041300141128           // -?Gestione videata D2 (altre parzializzazioni)?
041400141128           when  $Video = 'D2';
041500141128             exsr  sr_GesD02;
041600141128
041700141128           // -?? ? ??
041800141128           other;
041900141128             $Fine = *on;
042000141128
042100141128         endsl;
042200141128
042300141128       EndDo;
042400141128
042500141128       // -?Operazioni finali?
042600141128       exsr  sr_RoutEnd;
042700141128
042800141128       //--------------------------------------------------------------
042900141128       //?Operazioni iniziali.                                         ?
043000141128       //--------------------------------------------------------------
043100141128       BEGSR  sr_RoutInz;
043200141128
043300141128         // -?Impostazione chiusura?
043400141128         *inLR = *on;
043500141128
043600141128         // -?Pulizia parametri di solo Output?
043700150112         clear  oKC20fxx;
043800150112         clear  oKC20err;
043900150112         clear  oKC20msg;
044000141128
044100141128         // -?Impostazione nome programma a video?
044200141128         T01pgm = SDSpgm;
044300141128
044400141128         // -?Impostazione campi chiave fissi?
044500141128         k_TBLkut = c_KUT;
044600141128
044700141128         // -?Reperimento dati job?
044800141128         exsr  sr_DatiJob;
044900150112         if  iKC20abi = *blank;
045000150112           iKC20abi = UTEaut;
045100141128         endif;
045200141128
045300141128         // -?Controllo Autorizzazione dell'Utente alle Tariffe Clienti?
045400141128         //  ?(GIÀ ESEGUITO DAL CHIAMANTE - vedi TNTA87R e TRMK18R)?
045500141128         //reset  TNTAA1ds;
045600141128         //ITAA1caut = c_Aut_TC;
045700141128         //kPJbU = TNTAA1ds;
045800141128         //TNTAA1C ( kPJbA );
045900141128         //TNTAA1ds = kPJbU;
046000141128         //If  oTAA1fabi = 'N';
046100141128         //  $Fine    = *on;
046200150112         //  oKC20err = *on;
046300150112         //  oKC20msg = sk_Msg(01);
046400141128         //  leavesr;
046500141128         //EndIf;
046600141128
046700141128         // -?Caricamento filiali abilitate al profilo con autorizzaz.?
046800141128         //  ?a gestione Tariffe Clienti (ricevuta dal *pgm chiamante)?
046900141128         clear  TRUL31ds;
047000150112         I31abi = iKC20abi;
047100141128         I31cdi = DUTdis;
047200141128         I31car = DUTare;
047300141128         I31cpo = DUTpou;
047400141128         TRUL31R ( kpjba : TRUL31ds );
047500141128         If  O31pog <= *zero;
047600141128           $Fine    = *on;
047700150112           oKC20err = *on;
047800150112           oKC20msg = sk_Msg(01);
047900141128           leavesr;
048000141128         EndIf;
048100141128
048200141128         // -?Intabellamento Filiali e relative Aree?
048300141128         clear  xx;
048400141128         setll  *loval  AZORG;
048500141128         read  AZORG;
048600141128
048700141128         DoW  NOT  %eof(AZORG01L);
048800141128
048900141128           if  ORGfva = *blank;
049000141128             xx += 1;
049100141128             sk_Fil(xx) = ORGfil;
049200141128             sk_Car(xx) = ORGcar;
049300141128           endif;
049400141128
049500141128           read  AZORG;
049600141128
049700141128         EndDo;
049800141128
049900141128         // -?Intabellamento Importanza Clienti?
050000141128         clear  xx;
050100141128         k_TBLcod = 'IC';
050200141128         setll  %kds( keyTABEL00 : 2 )  TABEL;
050300141128         reade  %kds( keyTABEL00 : 2 )  TABEL;
050400141128
050500141128         DoW Not %eof(TABEL00F);
050600141128
050700141128           if  TBLflg = *blank;
050800141128             xx += 1;
050900141128             sk_IC(xx) = %subst( TBLkey : 1 : 1 );
051000141128           endif;
051100141128
051200141128           reade  %kds( keyTABEL00 : 2 )  TABEL;
051300141128
051400141128         EndDo;
051500141128
051600141128         // -?Intabellamento Categorie Merceologiche?
051700141128         clear  xx;
051800141128         k_TBLcod = '1L';
051900141128         setll  %kds( keyTABEL00 : 2 )  TABEL;
052000141128         reade  %kds( keyTABEL00 : 2 )  TABEL;
052100141128
052200141128         DoW Not %eof(TABEL00F);
052300141128
052400141128           if  TBLflg = *blank;
052500141128             xx += 1;
052600141128             sk_1L(xx) = %subst( TBLkey : 1 : 5 );
052700141128           endif;
052800141128
052900141128           reade  %kds( keyTABEL00 : 2 )  TABEL;
053000141128
053100141128         EndDo;
053200141128
053300141128         // -?Intabellamento Concorrenti?
053400141128         clear xx;
053500141128         clear  keyTNTBE01;
053600141128         k_TBEcod = 'IFS';
053700141128         k_TBEke1 = '10';
053800141128         setll  %kds( keyTNTBE01 : 2 )  TNTBE000;
053900141128         reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
054000141128
054100141128         DoW  Not %eof(TNTBE01L);
054200141128           if  TBEatb = *blank;
054300141128             xx += 1;
054400141128             sk_IFS_conc(xx) = %subst( TBEke2 : 1 : 4 );
054500141128           endif;
054600141128           reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
054700141128         EndDo;
054800141128
054900141128         // -?Intabellamento Concorrenti Logistica?
055000141128         clear xx;
055100141128         clear  keyTNTBE01;
055200141128         k_TBEcod = 'IFS';
055300141128         k_TBEke1 = '30';
055400141128         setll  %kds( keyTNTBE01 : 2 )  TNTBE000;
055500141128         reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
055600141128
055700141128         DoW  Not %eof(TNTBE01L);
055800141128           if  TBEatb = *blank;
055900141128             xx += 1;
056000141128             sk_IFS_log(xx) = %subst( TBEke2 : 1 : 4 );
056100141128           endif;
056200141128           reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
056300141128         EndDo;
056400141128
056500141128         // -?Intabellamento Paesi Estero?
056600141128         clear xx;
056700141128         clear  keyTNTBE01;
056800141128         k_TBEcod = 'IFS';
056900141128         k_TBEke1 = '50';
057000141128         setll  %kds( keyTNTBE01 : 2 )  TNTBE000;
057100141128         reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
057200141128
057300141128         DoW  Not %eof(TNTBE01L);
057400141128           if  TBEatb = *blank;
057500141128             xx += 1;
057600141128             sk_IFS_paesi(xx) = %subst( TBEke2 : 1 : 4 );
057700141128           endif;
057800141128           reade  %kds( keyTNTBE01 : 2 )  TNTBE000;
057900141128         EndDo;
058000141128
058100141128       ENDSR;
058200141128
058300141128       //--------------------------------------------------------------
058400141128       //?Reperimento Dati del job (Utente/Operativi).                 ?
058500141128       //--------------------------------------------------------------
058600141128       BEGSR  sr_DatiJob;
058700141128
058800141128         in(e) §AzUte;
058900141128         if NOT %error;
059000141128           in(e) §DatiUte;
059100141128         endif;
059200141128         if %error or RSut = *blanks;
059300141128           clear TIBS34ds;
059400141128           tibs34r ( tibs34ds );
059500141128           in §AzUte;
059600141128           in §DatiUte;
059700141128         endif;
059800141128
059900141128       ENDSR;
060000141128
060100141128       //--------------------------------------------------------------
060200141128       //?Gestione videata D01.                                        ?
060300141128       //--------------------------------------------------------------
060400141128       BEGSR  sr_GesD01;
060500141128
060600141128         // -?Inizializzazione videata?
060700141128         select;
060800141128           when  $InzD01 = *on;
060900141128             exsr  sr_InzD01;
061000141128             $InzD01 = *off;
061100141128           when  errGenerico = *off;
061200141128             //clear  V01ksu;
061300141128             clear  V01ksc;
061400141202             clear  V1Dksc;
061500141128             //clear  V01cpo;
061600141128         endsl;
061700141128
061800141128         // -?Emissione testata?
061900150112         write  KC20T01;
062000141128
062100141128         // -?Emissione videata?
062200150112         exfmt  KC20D01;
062300141128
062400141128         clear  errGenerico;
062500141128         clear  errMessage;
062600141128         clear  V1Dmsg;
062700141128
062800141128         Select;
062900141128
063000141128           // -?F3=Fine?
063100141128           when  dsp_aid = c_F03;
063200150112             oKC20fxx = '03';
063300141128             $Fine  = *on;
063400141128
063500141128           // -?F5=Altre Parzializzazioni?
063600141128           when  dsp_aid = c_F05;
063700141128             exsr  sr_CtrD01;
063800141128             if  errGenerico;
063900141128               leavesr;
064000141128             endif;
064100141128             exsr  sr_F05D01;
064200141128
064300141128           // -?F11=Pulizia?
064400141128           WHEN  dsp_aid = c_F11;
064500141128             exsr  sr_InzD02;
064600141128             exsr  sr_CtrD02;
064700141128             $InzD01 = *on;
064800141128
064900141128           // -?Invio?
065000141128           Other;
065100141128             exsr  sr_CtrD01;
065200141128             if  errGenerico;
065300141128               leavesr;
065400141128             endif;
065500141128             exsr  sr_CallPGM;
065600141128
065700141128         EndSl;
065800141128
065900141128       ENDSR;
066000141128
066100141128       //--------------------------------------------------------------
066200141128       //?Inizializzazione videata D01.                                ?
066300141128       //--------------------------------------------------------------
066400141128       BEGSR  sr_InzD01;
066500141128
066600150112         clear  KC20D01;
066700141128
066800141128         // -?Abilitazione richiesta Area?
066900141128         VisualCAR = ( oTAA1cabi = 'AZ'  or  oTAA1cabi = 'DI' );
067000141128
067100141203         // -?SE non è stato premuto F11=Pulizia: si reimpostano?
067200141203         //  ?il codice Cliente e/o il numero Campagna (con gli?
067300141203         //  ?ultimi usati - che sono quelli ricevuti)?
067400141203         If  dsp_aid <> c_F11;
067500141203
067600141203           // -?Codice Cliente?
067700150112           V01ksc = KC20ksc;
067800141203
067900141203           // -?Campagna Commerciale?
068000150112           V01ncm = %editc( KC20ncm : 'Z' );
068100141203
068200141203         EndIf;
068300141128
068400141128         // -?Ordinamento di default: per Importanza Cliente?
068500141128         V01osf = 'I';
068600141128
068700141128         // -?Selezione dei Solo Unificanti?
068800141128         V01uni = 'S';
068900141128
069000141128         // -?Selezione dei Bloccati?
069100141128         V01blc = 'N';
069200141128
069300141128       ENDSR;
069400141128
069500141128       //--------------------------------------------------------------
069600141128       //?Gestione tasto funzionale F5=Altre Parzializzazioni da D01.  ?
069700141128       //--------------------------------------------------------------
069800141128       BEGSR  sr_F05D01;
069900141128
070000141128         $Video  = 'D2';
070100141128         //$InzD02 = *on;     ?NO !!!?
070200141128
070300141128       ENDSR;
070400141128
070500141128       //--------------------------------------------------------------
070600141128       //?Controllo dati in videata D01.                               ?
070700141128       //--------------------------------------------------------------
070800141128       BEGSR  sr_CtrD01;
070900141128
071000141128         // -?Spegnimento degli indicatori di errore?
071100141128         %subst(IndDspF : 50) = *off;
071200141128
071300141128         // -?Codice Cliente?
071400141128         clear  V1Dksc;
071500141128         clear  TIBS69ds;
071600141128         I69sif = knsif;
071700141128         I69kcc = DUTkci;
071800141128         I69kac = V01ksc;
071900141128         tibs69r( tibs69ds :
072000141128                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
072100141128         if  O69err = *on;
072200141128           V1Dmsg = sk_Msg(27);
072300141128           ErrGenerico = *on;
072400141128           ErrMessage  = *on;
072500141128           PosCurKSC   = *on;
072600141128           leavesr;
072700141128         endif;
072800141128         V1Dksc = %subst( ACOrag : 1 : %len(V1Dksc) );
072900141128
073000141128         // -?SE richiesto singolo cliente: nessun altro controllo?
073100141128         if  V01ksc > *zero;
073200141128           leavesr;
073300141128         endif;
073400141128
073500141128
073600141128         // -?Ordinamento Elenco?
073700141128         if  V01osf <> 'R'  and  V01osf <> 'I';
073800141128           errGenerico = *on;
073900141128           errMessage  = *on;
074000141128           PosCurOSF   = *on;
074100141128           V1Dmsg = sk_Msg(02);
074200141128           leavesr;
074300141128         endif;
074400141128
074500141128         Select;
074600141128
074700141128           // -?SE inserita la Ragione Sociale NON è previsto?
074800141128           //  ?l'ordinamento per importanza clienti?
074900141128           When V01rag <> *blank  and  V01osf <> 'R';
075000141128
075100141128           // -?SE inserita selezione per Trattativa: è obbligatorio?
075200141128           //  ?l'inserimento di almeno una parzializzazione?
075300141128           When  V01ttr <> *blank  and
075400141128                 V01rag  = *blank  and
075500141128                 V01cic_ds  = *blank  and
075600141128                 V01loc  = *blank  and
075700141128                 V01prv  = *blank  and  V01cap = *blank  and
075800141128                 V01flt  = *zero   and  V01car = *blank  and
075900141128                 V01piv  = *blank  and  V01cdf = *blank  and
076000141128                 V01sct_ds  = *blank;
076100141128             errGenerico = *on;
076200141128             errMessage  = *on;
076300141128             PosCurOSF   = *on;
076400141128             V1Dmsg = sk_Msg(03);
076500141128             leavesr;
076600141128
076700141128         EndSl;
076800141128
076900141128         // -?Codice Importanza Cliente?
077000141128         For  xx = 1  To  %elem( sk_V01cic );
077100141128
077200141128           IF  sk_V01cic( xx ) <> *blank;
077300141128
077400141128             // ·?Ricerca?
077500141128             If  %scan( '?' : sk_V01cic( xx ) ) > *zero;
077600141128               clear  sk_V01cic( xx );
077700141128               idTabella = 'IC';
077800141128               Tabel_Ricerca ( idTabella  : Ordinamento :
077900141128                               idElemento : tastoUscita );
078000141128               sk_V01cic( xx ) = idElemento;
078100141128               %subst( IndDspF : 52 + xx : 1 ) = *on;
078200141128               errGenerico = *on;
078300141128             EndIf;
078400141128
078500141128             // ·?Controllo?
078600141128             If  %lookup( sk_V01cic( xx ) : sk_IC ) = *zero;
078700141128               %subst( IndDspF : 52 + xx : 1 ) = *on;
078800141128               errGenerico = *on;
078900141128               errMessage  = *on;
079000141128               V1Dmsg = sk_Msg(04);
079100141128               leavesr;
079200141128             EndIf;
079300141128
079400141128           ENDIF;
079500141128
079600141128         EndFor;
079700141128
079800141128         // -?SE inserito il CAP o la Località è obbligatoria la?
079900141128         //  ?Provincia?
080000141128         If  ( V01cap <> *blank  or  V01loc <> *blank )  and
080100141128               V01prv  = *blank;
080200141128           errMessage  = *on;
080300141128           errGenerico = *on;
080400141128           PosCurPRV   = *on;
080500141128           V1Dmsg = sk_Msg(05);
080600141128           leavesr;
080700141128         EndIf;
080800141128
080900141128         // -?Controllo congruenza tra CAP e Provincia?
081000141128         If  V01cap <> *blank;
081100141128
081200141128           reset  FNLV13ds;
081300141128           reset  TISI95ds;
081400141128           I95cap = V01cap;
081500141128           I95prv = V01prv;
081600141128           fnlv13r ( kpjba : fnlv13ds : tisi95ds );
081700141128
081800141128           if  O13err <> *blank;
081900141128             errMessage  = *on;
082000141128             errGenerico = *on;
082100141128             select;
082200141128               when  O13err = '4';
082300141128                 PosCurPRV  = *on;
082400141128               when  O13err = '5';
082500141128                 PosCurCAP  = *on;
082600141128             endsl;
082700141128             V1Dmsg = O13msg;
082800141128             leavesr;
082900141128           endif;
083000141128
083100141128         EndIf;
083200141128
083300141128         // -?Filiale Appartenenza?
083400141128         If  V01flt > *zero;
083500141128           chain  ( V01flt )  AZORG;
083600141128           if  Not %found(AZORG01L)  or  ORGfva <> *blank;
083700141128             errGenerico = *on;
083800141128             errMessage  = *on;
083900141128             PosCurFLT   = *on;
084000141128             V1Dmsg = sk_Msg(06);
084100141128             leavesr;
084200141128           endif;
084300141128         EndIf;
084400141128
084500141128         // -?Codice Area?
084600141128         clear  V1Dcar;
084700141128         if  V01car = *zero;
084800141128           clear  V01car;
084900141128         endif;
085000141128         k_TBLcod = '05';
085100141128         k_TBLkey = V01car;
085200141128         Select;
085300141128
085400141128           // -?Ricerca?
085500141128           When  %scan( '?' : V01car ) > *zero;
085600141128             clear  V01car;
085700141128             //clear w030a;
085800141128             //x§taber( k_TBLkut : k_TBLcod : k_TBLkey : w030a);
085900141128             //if  k_TBLkey <> *blank;
086000141128             //  V01car = k_TBLkey;
086100141128             //  V1Dcar = %subst( w030a : 1 : %len(V1Dcar) );
086200141128             //endif;
086300141128             idTabella = k_TBLcod;
086400141128             Tabel_Ricerca ( idTabella  : Ordinamento :
086500141128                             idElemento : tastoUscita );
086600141128             V01car      = idElemento;
086700141128             PosCurCAR   = *on;
086800141128             errGenerico = *on;
086900141128             leavesr;
087000141128
087100141128           // ·?Controllo?
087200141128           When  V01car <> *blank;
087300141128             clear  ds05;
087400141128             chain  %kds( keyTABEL00 )  TABEL;
087500141128             if  Not %found(TABEL00F)  or  TBLflg <> *blank;
087600141128               errGenerico = *on;
087700141128               errMessage  = *on;
087800141128               PosCurCAR   = *on;
087900141128               V1Dmsg = sk_Msg(07);
088000141128               leavesr;
088100141128             endif;
088200141128             ds05   = TBLuni;
088300141128             V1Dcar = ds05.§05des;
088400141128
088500141128         EndSl;
088600141128
088700141128         // -?Categoria merceologica?
088800141128         For  xx = 1  To  %elem( sk_V01sct );
088900141128
089000141128           If  sk_V01sct( xx ) <> *blank  and  sk_V01sct( xx ) <> *zero;
089100141128
089200141128             // ·?Ricerca?
089300141128             if  %scan( '?' : sk_V01sct( xx ) ) > *zero;
089400141128               clear  sk_V01sct( xx );
089500141128               idTabella = '1L';
089600141128               Tabel_Ricerca ( idTabella  : Ordinamento :
089700141128                               idElemento : tastoUscita );
089800141128               sk_V01cic( xx ) = idElemento;
089900141128               %subst( IndDspF : 61 + xx : 1 ) = *on;
090000141128               errGenerico = *on;
090100141128             endif;
090200141128
090300141128             // ·?Controllo?
090400141128             if  %lookup( sk_V01sct( xx ) : sk_1L ) = *zero;
090500141128               %subst( IndDspF : 61 + xx : 1 ) = *on;
090600141128               errGenerico = *on;
090700141128               errMessage  = *on;
090800141128               V1Dmsg = sk_Msg(08);
090900141128               leavesr;
091000141128             endif;
091100141128
091200141128           EndIf;
091300141128
091400141128         EndFor;
091500141128
091600141128         // -?Commerciale Unificante?
091700141128         clear  V1Dcmu;
091800141128         if  V01cmu = *zero;
091900141128           clear  V01cmu;
092000141128         endif;
092100141128         Select ;
092200141128
092300141128           // ·?Non inserito?
092400141128           When  V01cmu = *blank  or  V01cmu = *zero;
092500141128
092600141128           // ·?Ricerca?
092700141128           When  %scan( '?' : V01cmu ) > *zero;
092800141128             clear  V01cmu;
092900141128             clear  TRMK43ds;
093000141128             iMK43solU = 'S';
093100141128             iMK43fil  = DUTpou;
093200141128             TRMK43R ( kpjba : TRMK43ds );
093300141128             if  oMK43err = *off  and  oMK43fxx = *blank;
093400141128               V01cmu  = %editc( oMK43cmm : 'X' );
093500141128               V1Dcmu  = oMK43des;
093600141128             endif;
093700141128             ErrGenerico = *on;
093800141128             PosCurCMU   = *on;
093900141128             if  oMK43err = *on;
094000141128               ErrMessage = *on;
094100141128               V1Dmsg = oMK43msg;
094200141128             endif;
094300141128             leavesr;
094400141128
094500141128           // ·?Controllo numericità?
094600141128           When  %check( c_Digits : V01cmu ) > *zero;
094700141128             ErrMessage  = *on;
094800141128             ErrGenerico = *on;
094900141128             PosCurCMU   = *on;
095000141128             V1Dmsg = sk_Msg(09);
095100141128             leavesr;
095200141128
095300141128           // ·?Controllo validità / particolarità di attività?
095400141128           Other;
095500141128             k_CMMcod = %int( V01cmu );
095600141128             chain  %kds(keyAZCMM01)  AZCMM000;
095700141128             if  NOT %found(AZCMM01L)  or
095800141128                 CMMpar <> *blank;       // ·?Verifica SE codice attivo?
095900141128               errGenerico = *on;
096000141128               errMessage  = *on;
096100141128               PosCurCMU   = *on;
096200141128               V1Dmsg = sk_Msg(09);
096300141128               leavesr;
096400141128             endif;
096500141128             V1Dcmu = CMMdes;
096600141128
096700141128         EndSl;
096800141128
096900141128         If  V01cmu > *zero;
097000141128
097100141128           // -?Abilitazione utente al commerciale?
097200141128           reset  TNTAA1ds;
097300141128           ITAA1caut = c_Aut_TC;
097400141128           ITAA1cmm  = %int( V01cmu );
097500141128           kpjbu     = TNTAA1ds;
097600141128           TNTAA1C ( kpjba );
097700141128           TNTAA1ds = kpjbu;
097800141128           if  oTAA1fabi = 'N';
097900141128             errGenerico = *on;
098000141128             errMessage  = *on;
098100141128             PosCurCMU   = *on;
098200141128             V1Dmsg = sk_Msg(09);
098300141128             leavesr;
098400141128           endif;
098500141128
098600141128           // -?Selezionabili SOLO commerciali unificanti?
098700141128           if  %int( V01cmu ) <> CMMuni;
098800141128             errGenerico = *on;
098900141128             errMessage  = *on;
099000141128             PosCurCMU   = *on;
099100141128             V1Dmsg = sk_Msg(10);
099200141128             leavesr;
099300141128           endif;
099400141128
099500141128         EndIf;
099600141128
099700150112         clear  KC20ncm;
099800150112         clear  KC20ksc;
099900141128
100000141128         // -?Campagna Commerciale?
100100141128         clear  V1Dncm;
100200141128
100300141128         // ·?Ricerca?
100400141128         If  %scan( '?' : V01ncm ) > *zero;
100500141128           clear  V01ncm;
100600150112           clear  TRKC02ds;
100700150112           iKC02ric = 'R';
100800150112           trkc02r ( kpjba : TRKC02ds );
100900150112           if  oKC02ncm <> *zero;
101000150112             V01ncm = %editc( OKC02ncm : '3' );
101100141128           endif;
101200141128           PosCurNCM   = *on;
101300141128           errGenerico = *on;
101400150112           if  oKC02err = *on;
101500141128             ErrMessage = *on;
101600150112             V1Dmsg = oKC02msg;
101700141128             leavesr;
101800141128           endif;
101900141128           // ·?Meglio eseguire il controllo per verificare quale?
102000141128           //  ?cliente richiedere?
102100141128           //leavesr;
102200141128         EndIf;
102300141128
102400141128         // ·?Controllo?
102500141128         if  V01ncm = *blank;
102600141128           V01ncm = *zero;
102700141128         endif;
102800141128
102900141128         select;
103000141128
103100141128           // ·?NON più obbligatorio?
103200141128           when  V01ncm = *blank  or  %trim( V01ncm ) = *zero;
103300141128             clear  V01ncm;
103400141128
103500141128           // ·?NON numerico?
103600141128           when  %check( c_Digits : %trim( V01ncm ) ) > *zero;
103700141128             errGenerico = *on;
103800141128             errMessage  = *on;
103900141128             PosCurNCM   = *on;
104000141128             V1Dmsg = sk_Msg(11);
104100141128             leavesr;
104200141128
104300141128           // ·?Controllo?
104400141128           other;
104500141128             If  k_CMPncm <> %int( %trim( V01ncm ) );
104600141128
104700141128               clear  keyTICMP01;
104800141128               k_CMPncm = %int( %trim( V01ncm ) );
104900141128               chain  %kds( keyTICMP01 )  TICMP000;
105000141128               if  Not %found( TICMP01L );
105100141128                 errGenerico = *on;
105200141128                 errMessage  = *on;
105300141128                 PosCurNCM   = *on;
105400141128                 V1Dmsg = sk_Msg(11);
105500141128                 leavesr;
105600141128               endif;
105700141128               V1Dncm = CMPdes;
105800150219               select;
105900150219                 //when  %dec( %date() ) < CMPdic;
106000150219                 //  errMessage  = *on;
106100150219                 //  errGenerico = *on;
106200150219                 //  PosCurNCM   = *on;
106300150219                 //  V1Dmsg = %trimR( sK_Msg(11) ) + ': non è ancora attiva';
106400150219                 //  leavesr;
106500150219                 when  %dec( %date() ) > CMPdfc;
106600150219                   errMessage  = *on;
106700150219                   errGenerico = *on;
106800150219                   PosCurNCM   = *on;
106900150219                   V1Dmsg = %trimR( sK_Msg(11) ) + ': è già scaduta';
107000150219                   leavesr;
107100150219               endsl;
107200141128
107300141128               // -?RE-impostazione del cliente in Campagna Commerciale?
107400141128               //  ?(da richiedere a video - D01 - e da visualizzare nel?
107500141128               //  ?subfile - S01)?
107600141128               clear  keyTICMC01;
107700141128               k_CMCncm = k_CMPncm;
107800141128               chain  %kds( keyTICMC01 : 1 )  TICMC000;
107900141201
108000141201               select;
108100141201                 when  Not %found( TICMC01L );
108200141201                   errGenerico = *on;
108300141201                   errMessage  = *on;
108400141201                   PosCurNCM   = *on;
108500141201                   V1Dmsg = sk_Msg(11);
108600141201                   leavesr;
108700141201                 when  CMCksu <> *zero  and  V01uni = 'N';
108800141201                   errGenerico = *on;
108900141201                   errMessage  = *on;
109000141201                   PosCurNCM   = *on;
109100141201                   V1Dmsg = %trimR( sk_Msg(11) ) +
109200141201                            ': prevista per Unificanti - qui scartati';
109300141201                   leavesr;
109400141201                 when  CMCksu =  *zero  and  V01uni = 'S';
109500141201                   errGenerico = *on;
109600141201                   errMessage  = *on;
109700141201                   PosCurNCM   = *on;
109800141201                   V1Dmsg = %trimR( sk_Msg(11) ) +
109900141201                            ': prevista per NON Unificanti - qui selezionati';
110000141201                   leavesr;
110100141201               endsl;
110200141128
110300141128             EndIf;
110400141128
110500141201             if  V01ncm <> *blank  and  %trim( V01ncm ) <> *zero;
110600150112               KC20ncm = k_CMPncm;
110700141128               select;
110800141128                 when  CMCksu <> *zero;
110900150112                   KC20ksc = V01ksc;
111000141128                 when  CMCksc <> *zero;
111100150112                   KC20ksc = V01ksc;
111200141128               endsl;
111300141201             endif;
111400141128
111500141128         endsl;
111600141128
111700150127         // -?Parzializzazioni sulla Campagna: con Trattativa?
111800141128         if  ( V01ncm  = *blank  or  %trim( V01ncm ) = *zero )  and
111900141202               V01tcm <> *blank;
112000141128           errGenerico = *on;
112100141128           errMessage  = *on;
112200141202           PosCurTCM   = *on;
112300141128           V1Dmsg = sk_Msg(12);
112400141128           leavesr;
112500141128         endif;
112600150127
112700150127         // -?Parzializzazioni sulla Campagna: con Attività?
112800150127         if  ( V01ncm  = *blank  or  %trim( V01ncm ) = *zero )  and
112900150127               V01eat <> *blank;
113000150127           errGenerico = *on;
113100150127           errMessage  = *on;
113200150127           PosCurEAT   = *on;
113300150127           V1Dmsg = sk_Msg(12);
113400150127           leavesr;
113500150127         endif;
113600141128
113700141128
113800141128         // -?Singolo cliente in Campagna Commerciale?
113900141128         If    V01ksc <> *zero   and
114000141128             ( V01ncm  = *blank  or  %trim( V01ncm ) = *zero );
114100141128
114200141128           clear  keyTICMC01;
114300150112           k_CMCncm = KC20ncm;
114400141128           select;
114500141128             when  CMCksu <> *zero;
114600150112               k_CMCksu = KC20ksc;
114700141128             when  CMCksc <> *zero;
114800150112               k_CMCksc = KC20ksc;
114900141128           endsl;
115000141128           setll  %kds( keyTICMC01 )  TICMC000;
115100141128           if  Not %equal( TICMC01L );
115200141128             errGenerico = *on;
115300141128             errMessage  = *on;
115400141128             PosCurKSC   = *on;
115500141128             V1Dmsg = sk_Msg(13);
115600141128             leavesr;
115700141128           endif;
115800141128
115900141128         EndIf;
116000141128
116100141128       ENDSR;
116200141128
116300141128       //--------------------------------------------------------------
116400141128       //?Gestione videata D02.                                        ?
116500141128       //--------------------------------------------------------------
116600141128       BEGSR  sr_GesD02;
116700141128
116800141128         // -?Inizializzazione videata?
116900141128         if  $InzD02 = *on;
117000141128           exsr  sr_InzD02;
117100141128           $InzD02 = *off;
117200141128         endif;
117300141128
117400141128         // -?Emissione testata?
117500150112         write  KC20T01;
117600141128
117700141128         // -?Emissione videata?
117800150112         exfmt  KC20D02;
117900141128
118000141128         clear  errGenerico;
118100141128         clear  errMessage;
118200141128         clear  V1Dmsg;
118300141128
118400141128         Select;
118500141128
118600141128           // -?F3=Fine?
118700141128           when  dsp_aid = c_F03;
118800150112             oKC20fxx = '03';
118900141128             $Fine  = *on;
119000141128
119100141128           // -?Invio?
119200141128           Other;
119300141128             exsr  sr_CtrD02;
119400141128             if  errGenerico;
119500141128               leavesr;
119600141128             endif;
119700141128             $Video = 'D1';
119800141128
119900141128         EndSl;
120000141128
120100141128       ENDSR;
120200141128
120300141128       //--------------------------------------------------------------
120400141128       //?Inizializzazione videata D02.                                ?
120500141128       //--------------------------------------------------------------
120600141128       BEGSR  sr_InzD02;
120700141128
120800150112         clear  KC20D02;
120900141128
121000141128       ENDSR;
121100141128
121200141128       //--------------------------------------------------------------
121300141128       //?Controllo dati in videata D02.                               ?
121400141128       //--------------------------------------------------------------
121500141128       BEGSR  sr_CtrD02;
121600141128
121700141128         // -?Affidato a BRT?
121800141128         Select;
121900141128           // ·?Nessun dato inserito?
122000141128           When  V02brtDa  = *zero  and  V02brtA  = *zero;
122100141128           // ·?Obbligatori entrambi se immesso uno dei due?
122200141128           When  V02brtDa <> *zero  and  V02brtA  = *zero;
122300141128             ErrMessage  = *on;
122400141128             ErrGenerico = *on;
122500141128             PosCurBRTa  = *on;
122600141128             V2Dmsg = sk_Msg(14);
122700141128             leavesr;
122800141128           When  V02brtDa  = *zero  and  V02brtA <> *zero;
122900141128             ErrMessage  = *on;
123000141128             ErrGenerico = *on;
123100141128             PosCurBRTda = *on;
123200141128             V2Dmsg = sk_Msg(14);
123300141128             leavesr;
123400141128           // ·?Controllo congruenza tra "DA" e "A"?
123500141128           When  V02brtDa > V02brtA;
123600141128             ErrMessage  = *on;
123700141128             ErrGenerico = *on;
123800141128             PosCurBRTda = *on;
123900141128             V2Dmsg = sk_Msg(15);
124000141128             leavesr;
124100141128         EndSl;
124200141128
124300141128         // -?NON affidato a BRT: Italia-Parcel?
124400141128         Select;
124500141128           When  V02nrfDa  = *zero  and  V02nrfA  = *zero;
124600141128           // ·?Obbligatori entrambi se immesso uno dei due?
124700141128           When  V02nrfDa <> *zero  and  V02nrfA  = *zero;
124800141128             ErrMessage  = *on;
124900141128             ErrGenerico = *on;
125000141128             PosCurNRFa  = *on;
125100141128             V2Dmsg = sk_Msg(16);
125200141128             leavesr;
125300141128           When  V02nrfDa  = *zero  and  V02nrfA <> *zero;
125400141128             ErrMessage  = *on;
125500141128             ErrGenerico = *on;
125600141128             PosCurNRFda = *on;
125700141128             V2Dmsg = sk_Msg(16);
125800141128             leavesr;
125900141128           // ·?Controllo congruenza tra "DA" e "A"?
126000141128           When  V02nrfDa > V02nrfA;
126100141128             ErrMessage  = *on;
126200141128             ErrGenerico = *on;
126300141128             PosCurNRFda = *on;
126400141128             V2Dmsg = sk_Msg(17);
126500141128             leavesr;
126600141128         EndSl;
126700141128
126800141128         // -?NON affidato a BRT: Italia-Messaggeria?
126900141128         Select;
127000141128           When  V02nroDa  = *zero  and  V02nroA  = *zero;
127100141128           // ·?Obbligatori entrambi se immesso uno dei due?
127200141128           When  V02nroDa <> *zero  and  V02nroA  = *zero;
127300141128             ErrMessage  = *on;
127400141128             ErrGenerico = *on;
127500141128             PosCurNROa  = *on;
127600141128             V2Dmsg = sk_Msg(16);
127700141128             leavesr;
127800141128           When  V02nroDa  = *zero  and  V02nroA <> *zero;
127900141128             ErrMessage  = *on;
128000141128             ErrGenerico = *on;
128100141128             PosCurNROda = *on;
128200141128             V2Dmsg = sk_Msg(16);
128300141128             leavesr;
128400141128           // ·?Controllo congruenza tra "DA" e "A"?
128500141128           When  V02nroDa > V02nroA;
128600141128             ErrMessage  = *on;
128700141128             ErrGenerico = *on;
128800141128             PosCurNROda = *on;
128900141128             V2Dmsg = sk_Msg(17);
129000141128             leavesr;
129100141128         EndSl;
129200141128
129300141128         // -?NON affidato a BRT: Estero-Via Aerea?
129400141128         Select;
129500141128           When  V02nerDa  = *zero  and  V02nerA  = *zero;
129600141128           // ·?Obbligatori entrambi se immesso uno dei due?
129700141128           When  V02nerDa <> *zero  and  V02nerA  = *zero;
129800141128             ErrMessage  = *on;
129900141128             ErrGenerico = *on;
130000141128             PosCurNERa  = *on;
130100141128             V2Dmsg = sk_Msg(16);
130200141128             leavesr;
130300141128           When  V02nerDa  = *zero  and  V02nerA <> *zero;
130400141128             ErrMessage  = *on;
130500141128             ErrGenerico = *on;
130600141128             PosCurNERda = *on;
130700141128             V2Dmsg = sk_Msg(16);
130800141128             leavesr;
130900141128           // ·?Controllo congruenza tra "DA" e "A"?
131000141128           When  V02nerDa > V02nerA;
131100141128             ErrMessage  = *on;
131200141128             ErrGenerico = *on;
131300141128             PosCurNERda = *on;
131400141128             V2Dmsg = sk_Msg(17);
131500141128             leavesr;
131600141128         EndSl;
131700141128
131800141128         // -?NON affidato a BRT: Estero-Servizio Camionistico?
131900141128         Select;
132000141128           When  V02naeDa  = *zero  and  V02naeA  = *zero;
132100141128           // ·?Obbligatori entrambi se immesso uno dei due?
132200141128           When  V02naeDa <> *zero  and  V02naeA  = *zero;
132300141128             ErrMessage  = *on;
132400141128             ErrGenerico = *on;
132500141128             PosCurNAEa  = *on;
132600141128             V2Dmsg = sk_Msg(16);
132700141128             leavesr;
132800141128           When  V02naeDa  = *zero  and  V02naeA <> *zero;
132900141128             ErrMessage  = *on;
133000141128             ErrGenerico = *on;
133100141128             PosCurNAEda = *on;
133200141128             V2Dmsg = sk_Msg(16);
133300141128             leavesr;
133400141128           // ·?Controllo congruenza tra "DA" e "A"?
133500141128           When  V02naeDa > V02naeA;
133600141128             ErrMessage  = *on;
133700141128             ErrGenerico = *on;
133800141128             PosCurNAEda = *on;
133900141128             V2Dmsg = sk_Msg(17);
134000141128             leavesr;
134100141128         EndSl;
134200141128
134300141128         // -?NON affidato a BRT: Altro (es. Completi, ADR)?
134400141128         Select;
134500141128           When  V02ntrDa  = *zero  and  V02ntrA  = *zero;
134600141128           // ·?Obbligatori entrambi se immesso uno dei due?
134700141128           When  V02ntrDa <> *zero  and  V02ntrA  = *zero;
134800141128             ErrMessage  = *on;
134900141128             ErrGenerico = *on;
135000141128             PosCurNTRa  = *on;
135100141128             V2Dmsg = sk_Msg(16);
135200141128             leavesr;
135300141128           When  V02ntrDa  = *zero  and  V02ntrA <> *zero;
135400141128             ErrMessage  = *on;
135500141128             ErrGenerico = *on;
135600141128             PosCurNTRda = *on;
135700141128             V2Dmsg = sk_Msg(16);
135800141128             leavesr;
135900141128           // ·?Controllo congruenza tra "DA" e "A"?
136000141128           When  V02ntrDa > V02naeA;
136100141128             ErrMessage  = *on;
136200141128             ErrGenerico = *on;
136300141128             PosCurNTRda = *on;
136400141128             V2Dmsg = sk_Msg(17);
136500141128             leavesr;
136600141128         EndSl;
136700141128
136800141128         // -?Paesi estero?
136900141128         For  xx = 1  To  %elem( sk_V02est );
137000141128
137100141128           If  sk_V02est( xx ) <> *blank;
137200141128
137300141128             // ·?Ricerca?
137400141128             If  %scan( '?' : sk_V02est( xx ) ) > *zero;
137500141128               clear  sk_V02est( xx );
137600141128               clear  TIBS02ds;
137700141128               T02mod = 'R';
137800141128               T02cod = 'IFS';
137900141128               T02ke1 = '50';
138000141128               T02sif = KNSIF;
138100141128               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
138200141128               if  T02err = *blank;
138300141128                 sk_V02est( xx ) = T02ke2;
138400141128                 %subst( IndDspF : 80 + xx : 1 ) = *on;
138500141128                 errGenerico = *on;
138600141128               endif;
138700141128             EndIf;
138800141128
138900141128             // ·?Controllo?
139000141128             if  %lookup( sk_V02est( xx ) : sk_IFS_paesi ) = *zero;
139100141128               %subst( IndDspF : 80 + xx : 1 ) = *on;
139200141128               errGenerico = *on;
139300141128               errMessage  = *on;
139400141128               V2Dmsg = sk_Msg(18);
139500141128               leavesr;
139600141128             endif;
139700141128
139800141128           EndIf;
139900141128
140000141128         EndFor;
140100141128
140200141128         // -?Peso Medio a Spedizione?
140300141128         Select;
140400141128           When  V02pkgDa  = *zero  and  V02pkgA  = *zero;
140500141128           // ·?Obbligatori entrambi se immesso uno dei due?
140600141128           When  V02pkgDa <> *zero  and  V02pkgA  = *zero;
140700141128             ErrMessage  = *on;
140800141128             ErrGenerico = *on;
140900141128             PosCurPKGa  = *on;
141000141128             V2Dmsg = sk_Msg(19);
141100141128             leavesr;
141200141128           When  V02pkgDa  = *zero  and  V02pkgA <> *zero;
141300141128             ErrMessage  = *on;
141400141128             ErrGenerico = *on;
141500141128             PosCurPKGda = *on;
141600141128             V2Dmsg = sk_Msg(19);
141700141128             leavesr;
141800141128           // ·?Controllo congruenza tra "DA" e "A"?
141900141128           When  V02pkgDa > V02pkgA;
142000141128             ErrMessage  = *on;
142100141128             ErrGenerico = *on;
142200141128             PosCurPKGda = *on;
142300141128             V2Dmsg = sk_Msg(20);
142400141128             leavesr;
142500141128         EndSl;
142600141128
142700141128         // -?Concorrenti?
142800141128         For  xx = 1  To  %elem( sk_V02conc );
142900141128
143000141128           If  sk_V02conc( xx ) <> *blank;
143100141128
143200141128             // ·?Ricerca?
143300141128             If  %scan( '?' : sk_V02conc( xx ) ) > *zero;
143400141128               clear  sk_V02conc( xx );
143500141128               clear  TIBS02ds;
143600141128               T02mod = 'R';
143700141128               T02cod = 'IFS';
143800141128               T02ke1 = '10';
143900141128               T02sif = KNSIF;
144000141128               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
144100141128               if  T02err = *blank;
144200141128                 sk_V02conc( xx ) = T02ke2;
144300141128                 %subst( IndDspF : 85 + xx : 1 ) = *on;
144400141128                 errGenerico = *on;
144500141128               endif;
144600141128             EndIf;
144700141128
144800141128             // ·?Controllo?
144900141128             if  %lookup( sk_V02conc( xx ) : sk_IFS_conc ) = *zero;
145000141128               %subst( IndDspF : 85 + xx : 1 ) = *on;
145100141128               errGenerico = *on;
145200141128               errMessage  = *on;
145300141128               V2Dmsg = sk_Msg(21);
145400141128               leavesr;
145500141128             endif;
145600141128
145700141128           EndIf;
145800141128
145900141128         EndFor;
146000141128
146100141128         // -?LOGISTICA?
146200141128
146300141128         // -?Interessati alla Logistica obbligatorio?
146400141128         //  ?SE inserite le data di immissione interessamento alla logistica?
146500141128         if  V02int  = *blank  and  ( V02dil1 > *zero
146600141128                                or    V02dil2 > *zero );
146700141128           ErrMessage  = *on;
146800141128           ErrGenerico = *on;
146900141128           PosCurINT   = *on;
147000141128           V2Dmsg = sk_Msg(22);
147100141128           leavesr;
147200141128         endif;
147300141128
147400141128         // -?Controllo la correttezza delle date immissione interesse?
147500141128         //  ?logistica?
147600141128         If  V02dil1 > *zero;
147700141128           clear  WLBdat;
147800141128           G02dat = V02dil1;
147900141128           XSRDA8 ( WLBdat );
148000141128           if  G02err = *on;
148100141128             ErrMessage  = *on;
148200141128             ErrGenerico = *on;
148300141128             PosCurDIL1  = *on;
148400141128             V2Dmsg = sk_Msg(23);
148500141128             leavesr;
148600141128           endif;
148700141128           V02dil1  = G02dat;
148800141128           wDIL1    = G02inv;
148900141128         EndIf;
149000141128         If  V02dil2 > *zero;
149100141128           clear  WLBdat;
149200141128           G02dat = V02dil2;
149300141128           XSRDA8 ( WLBdat );
149400141128           if  G02err = *on;
149500141128             ErrMessage  = *on;
149600141128             ErrGenerico = *on;
149700141128             PosCurDIL2  = *on;
149800141128             V2Dmsg = sk_Msg(23);
149900141128             leavesr;
150000141128           endif;
150100141128           V02dil2  = G02dat;
150200141128           wDIL2    = G02inv;
150300141128         EndIf;
150400141128
150500141128         // -?SE impostata data "dal" e non "al":?
150600141128         //  ?si imposta "al" = data del giorno?
150700141128         if  V02dil1 > *zero  and  V02dil2 = *zero;
150800141128           wDIL2   = %dec( %date() );
150900141128           V02dil2 = %dec( %date( wDIL2 : *iso ) );
151000141128         endif;
151100141128
151200141128         // -?"DAL" congruente con "AL"?
151300141128         if  wDIL1 > *zero  and  wDIL2 = *zero;
151400141128           ErrMessage  = *on;
151500141128           ErrGenerico = *on;
151600141128           PosCurDIL1  = *on;
151700141128           V2Dmsg = sk_Msg(24);
151800141128           leavesr;
151900141128         endif;
152000141128         if  wDIL1 > *zero  and  wDIL2 > *zero  and  wDIL1 > wDIL2;
152100141128           ErrMessage  = *on;
152200141128           ErrGenerico = *on;
152300141128           PosCurDIL2  = *on;
152400141128           V2Dmsg = sk_Msg(24);
152500141128           leavesr;
152600141128         endif;
152700141128
152800141128         // -?Se inseriti Concorrenti: ci deve essere anche la?
152900141128         //  ?Logistica in outsourcing?
153000141128         if  V02concL_ds <> *blank  and  V02los <> 'S';
153100141128           ErrMessage  = *on;
153200141128           ErrGenerico = *on;
153300141128           PosCurLOS   = *on;
153400141128           V2Dmsg = sk_Msg(25);
153500141128           leavesr;
153600141128         endif;
153700141128
153800141128         // -?Concorrenti (di Logistica)?
153900141128         For  xx = 1  To  %elem( sk_V02concL );
154000141128
154100141128           If  sk_V02concL( xx ) <> *blank;
154200141128
154300141128             // ·?Ricerca?
154400141128             If  %scan( '?' : sk_V02concL( xx ) ) > *zero;
154500141128               clear  sk_V02concL( xx );
154600141128               clear  TIBS02ds;
154700141128               T02mod = 'R';
154800141128               T02cod = 'IFS';
154900141128               T02ke1 = '40';
155000141128               T02sif = KNSIF;
155100141128               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
155200141128               if  T02err = *blank;
155300141128                 sk_V02concL( xx ) = T02ke2;
155400141128                 %subst( IndDspF : 92 + xx : 1 ) = *on;
155500141128                 errGenerico = *on;
155600141128               endif;
155700141128             EndIf;
155800141128
155900141128             // ·?Controllo?
156000141128             if  %lookup( sk_V02concL( xx ) : sk_IFS_log ) = *zero;
156100141128               %subst( IndDspF : 92 + xx : 1 ) = *on;
156200141128               errGenerico = *on;
156300141128               errMessage  = *on;
156400141128               V2Dmsg = sk_Msg(26);
156500141128               leavesr;
156600141128             endif;
156700141128
156800141128           EndIf;
156900141128
157000141128         EndFor;
157100141128
157200141128         // -?Evidenziazione F5 SE inserite delle selezioni?
157300141128         F5_Evid = ( V02brtDa <> *zero   or
157400141128                     V02brtA  <> *zero   or
157500141128                     V02nrfDa <> *zero   or
157600141128                     V02nroDa <> *zero   or
157700141128                     V02nerDa <> *zero   or
157800141128                     V02naeDa <> *zero   or
157900141128                     V02ntrDa <> *zero   or
158000141128                     V02est_ds   <> *blank  or
158100141128                     V02pkgDa <> *zero   or
158200141128                     V02shp   <> *blank  or
158300141128                     V02secm  <> *blank  or
158400141128                     V02sbx   <> *blank  or
158500141128                     V02conc_ds  <> *blank  or
158600141128                     V02int   <> *blank  or
158700141128                     V02los   <> *blank  or
158800141128                     V02concL_ds <> *blank );
158900141128
159000141128       ENDSR;
159100141128
159200141128       //--------------------------------------------------------------
159300150112       //?Richiamo *pgm in base alle selezioni (TNTAI1R o TRKC21R).    ?
159400141128       //--------------------------------------------------------------
159500141128       BEGSR  sr_CallPGM;
159600141128
159700141128         Select;
159800141128
159900141128           // -?Specificato un Cliente?
160000141128           When  V01ksc <> *zero;
160100141128             exsr  sr_Call_TNTAI2R;
160200141128
160300141128           // -?Specificata una Campagna Commerciale?
160400141128           When  V01ncm <> *blank  and  %int( %trim( V01ncm ) ) > *zero;
160500150112             exsr  sr_Call_TRKC21R;
160600141128
160700141128           // -?Altrimenti:?
160800141128           Other;
160900141128             exsr  sr_Call_TNTAI1R;
161000141128
161100141128         EndSl;
161200141128
161300141128       ENDSR;
161400141128
161500141128       //--------------------------------------------------------------
161600141128       //?Richiamo *pgm Interrogazione singolo Cliente.                ?
161700141128       //--------------------------------------------------------------
161800141128       BEGSR  sr_Call_TNTAI2R;
161900141128
162000141128         clear  TNTAI2ds;
162100150112         tntaI2ds.iTAI2ric  = iKC20ric;
162200141128         tntaI2ds.iTAI2ksc  = V01ksc;
162300141128         tntaI2ds.iTAI2comD = V1Dncm;
162400141128
162500141128         tntaI2r ( kpjba : TNTAI2ds );
162600141128
162700141128         Select;
162800141128
162900141128           // ·?Rilevato errore?
163000141128           When  tntaI2ds.oTAI2err = *on;
163100141128             ErrGenerico = *on;
163200141128             ErrMessage  = *on;
163300141128             V1Dmsg = tntaI2ds.oTAI2msg;
163400141128
163500141128           // ·?Uscita con F3?
163600141128           When  tntaI2ds.oTAI2f03 = *on;
163700141128             $Fine = *on;
163800141128
163900141128           //// ·?Uscita con F12?
164000141128           //When  tntaI2ds.oTAI2f12 = *on;
164100141128
164200141128           //// ·?Uscita "regolare"?
164300141128           //Other;
164400141128
164500141128         EndSl;
164600141128
164700141128       ENDSR;
164800141128
164900141128       //--------------------------------------------------------------
165000141128       //?Richiamo *pgm Interrogazione Clienti in Campagna.            ?
165100141128       //--------------------------------------------------------------
165200150112       BEGSR  sr_Call_TRKC21R;
165300141128
165400150112         clear  TRKC21ds;
165500150112         TRKC21ds.iKC21abi = iKC20abi;
165600150112         TRKC21ds.iKC21ric = iKC20ric;
165700150112         TRKC21ds.iKC21ncm = %int( %trim( V01ncm ) );
165800150112         TRKC21ds.iKC21ksu = V01ksc;
165900141128
166000150112         exsr  sr_Prep_TRKC20ds2;
166100141128
166200150112         trkc21r ( kpjba : TRKC21ds : TRKC20ds2 );
166300141128
166400141128         Select;
166500141128
166600141128           // ·?Rilevato errore?
166700150112           When  TRKC21ds.oKC21err = *on;
166800141128             ErrGenerico = *on;
166900141128             ErrMessage  = *on;
167000150112             V1Dmsg = TRKC21ds.oKC21msg;
167100141128
167200141128           // ·?Uscita con F3?
167300150112           When  TRKC21ds.oKC21fxx = '03';
167400141128             $Fine = *on;
167500141128
167600141128           //// ·?Uscita con F12?
167700150112           //When  TRKC21ds.oKC21fxx = '12';
167800141128
167900141201           // ·?Uscita "regolare"?
168000141201           //  ?(se selezionato un cliente => uscita)?
168100141201           Other;
168200150112             if  TRKC21ds.iKC21ric = *blank;
168300150112               KC20ksc = TRKC21ds.oKC21ksc;
168400141201               $Fine   = *on;
168500141201             endif;
168600141128
168700141128         EndSl;
168800141128
168900141128       ENDSR;
169000141128
169100141128       //--------------------------------------------------------------
169200141128       //?Richiamo *pgm Interrogazione Clienti NON in Campagna.        ?
169300141128       //--------------------------------------------------------------
169400141128       BEGSR  sr_Call_TNTAI1R;
169500141128
169600141128         clear  TNTAI1ds;
169700150112         if  iKC20ric = *blank;
169800141201           tntaI1ds.iTAI1ric = 'S';
169900141201         else;
170000150112           tntaI1ds.iTAI1ric = iKC20ric;
170100141201         endif;
170200150112         tntaI1ds.iTAI1abi = iKC20abi;
170300141128         tntaI1ds.iTAI1aut = '*C';
170400141128         if  V01cmu <> *blank;
170500141128           tntaI1ds.iTAI1com  = %dec( V01cmu : 7 : 0 );
170600141128           tntaI1ds.iTAI1comd = V1Dcmu;
170700141128         endif;
170800141128
170900150112         exsr  sr_Prep_TRKC20ds2;
171000141128
171100150112         tntaI1r ( kpjba : TNTAI1ds : TRKC20ds2 );
171200141128
171300141128         Select;
171400141128
171500141128           // ·?Rilevato errore?
171600141128           When  tntaI1ds.oTAI1err = *on;
171700141128             ErrGenerico = *on;
171800141128             ErrMessage  = *on;
171900141128             V1Dmsg = tntaI1ds.oTAI1msg;
172000141128
172100141128           // ·?Uscita con F3?
172200141128           When  tntaI1ds.oTAI1f03 = *on;
172300141128             $Fine = *on;
172400141128
172500141128           //// ·?Uscita con F12?
172600141128           //When  tntaI1ds.oTAI1f12 = *on;
172700141128
172800141201           // ·?Uscita "regolare"?
172900141201           //  ?(se selezionato un cliente => uscita)?
173000141201           Other;
173100141201             if  tntaI1ds.iTAI1ric = 'S';
173200150112               KC20ksc = tntaI1ds.oTAI1ksc;
173300141201               $Fine   = *on;
173400141201             endif;
173500141128
173600141128         EndSl;
173700141128
173800141128       ENDSR;
173900141128
174000141128       //--------------------------------------------------------------
174100141128       //?Operazioni finali.                                           ?
174200141128       //--------------------------------------------------------------
174300150112       BEGSR  sr_Prep_TRKC20ds2;
174400141128
174500150112         clear  TRKC20ds2;
174600141128
174700150112         TRKC20ds2.iKC20ric  = iKC20ric;
174800150112         TRKC20ds2.iKC20abi  = iKC20abi;
174900150112         TRKC20ds2.KC20ksc   = V01ksc;
175000150112         TRKC20ds2.KC20osf   = V01osf;
175100150112         TRKC20ds2.KC20rag   = V01rag;
175200150112         TRKC20ds2.KC20cic1  = V01cic1;
175300150112         TRKC20ds2.KC20cic2  = V01cic2;
175400150112         TRKC20ds2.KC20cic3  = V01cic3;
175500150112         TRKC20ds2.KC20cic4  = V01cic4;
175600150112         TRKC20ds2.KC20cic5  = V01cic5;
175700150112         TRKC20ds2.KC20loc   = V01loc;
175800150112         TRKC20ds2.KC20prv   = V01prv;
175900150112         TRKC20ds2.KC20cap   = V01cap;
176000150112         TRKC20ds2.KC20flt   = V01flt;
176100141128         if  V01car <> *blank;
176200150112           TRKC20ds2.KC20car   = %int( V01car );
176300141128         endif;
176400150112         TRKC20ds2.KC20piv   = V01piv;
176500150112         TRKC20ds2.KC20cdf   = V01cdf;
176600141128         if  V01sct1 <> *blank;
176700150112           TRKC20ds2.KC20sct1  = %int( V01sct1 );
176800141128         endif;
176900141128         if  V01sct2 <> *blank;
177000150112           TRKC20ds2.KC20sct2  = %int( V01sct2 );
177100141128         endif;
177200141128         if  V01sct3 <> *blank;
177300150112           TRKC20ds2.KC20sct3  = %int( V01sct3 );
177400141128         endif;
177500141128         if  V01cmu <> *blank;
177600150112           TRKC20ds2.KC20cmu   = %int( V01cmu );
177700141128         endif;
177800150112         TRKC20ds2.KC20uni   = V01uni;
177900150112         TRKC20ds2.KC20blc   = V01blc;
178000150112         TRKC20ds2.KC20ttr   = V01ttr;
178100141201         if  V01ncm <> *blank;
178200150112           TRKC20ds2.KC20ncm   = %int( %trim( V01ncm ) );
178300141201         endif;
178400150112         TRKC20ds2.KC20tcm   = V01tcm;
178500150112         TRKC20ds2.KC20eat   = V01eat;
178600150112         TRKC20ds2.KC20brtI  = V02brtDa;
178700150112         TRKC20ds2.KC20brtF  = V02brtA;
178800150112         TRKC20ds2.KC20nrfI  = V02nrfDa;
178900150112         TRKC20ds2.KC20nrfF  = V02nrfA;
179000150112         TRKC20ds2.KC20nroI  = V02nroDa;
179100150112         TRKC20ds2.KC20nroF  = V02nroA;
179200150112         TRKC20ds2.KC20nerI  = V02nerDa;
179300150112         TRKC20ds2.KC20nerF  = V02nerA;
179400150112         TRKC20ds2.KC20naeI  = V02naeDa;
179500150112         TRKC20ds2.KC20naeF  = V02naeA;
179600150112         TRKC20ds2.KC20ntrI  = V02ntrDa;
179700150112         TRKC20ds2.KC20ntrF  = V02ntrA;
179800150112         TRKC20ds2.KC20est1  = V02est1;
179900150112         TRKC20ds2.KC20est2  = V02est2;
180000150112         TRKC20ds2.KC20est3  = V02est3;
180100150112         TRKC20ds2.KC20pkgI  = V02pkgDa;
180200150112         TRKC20ds2.KC20pkgF  = V02pkgA;
180300150112         TRKC20ds2.KC20shp   = V02shp;
180400150112         TRKC20ds2.KC20secm  = V02secm;
180500150112         TRKC20ds2.KC20sbx   = V02sbx;
180600150112         TRKC20ds2.KC20conc1 = V02conc1;
180700150112         TRKC20ds2.KC20conc2 = V02conc2;
180800150112         TRKC20ds2.KC20conc3 = V02conc3;
180900150112         TRKC20ds2.KC20int   = V02int;
181000150112         TRKC20ds2.KC20dil1  = wDIL1;
181100150112         TRKC20ds2.KC20dil2  = wDIL2;
181200150112         TRKC20ds2.KC20los   = V02los;
181300150112         TRKC20ds2.KC20conL1 = V02concL1;
181400150112         TRKC20ds2.KC20conL2 = V02concL2;
181500150112         TRKC20ds2.KC20conL3 = V02concL3;
181600141128
181700141128       ENDSR;
181800141128
181900141128       //--------------------------------------------------------------
182000141128       //?Operazioni finali.                                           ?
182100141128       //--------------------------------------------------------------
182200141128       BEGSR  sr_RoutEnd;
182300141128
182400141128         // -?Chiusura applicazioni precedentemente aperte?
182500141128         clear  TNTAA1ds;
182600141128         iTAA1tla = 'C';
182700141128         clear  KPJBU;
182800141128         KPJBU = TNTAA1ds;
182900141128         TNTAA1C ( kpjba );
183000141128
183100141128         // -?Chiusura *pgm?
183200141128         return;
183300141128
183400141128       ENDSR;
183500141128
183600141128      /end-free
183700141128
183800141128       //--------------------------------------------------------------
183900141128       //?Schiere a tempo di compilazione.
184000141128       //--------------------------------------------------------------
184100141128
184200141128** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
184300141128Utente non abilitato alla funzione                                              1
184400141128Ordinamento errato                                                              2
184500141128Se richiesto "Trattative in corso" immettere almeno un dato anagrafico          3
184600141128Importanza cliente errata                                                       4
184700141128Se parzializzazione per CAP o Località  immettere la Provincia                  5
184800141128Filiale di Appartenenza errata                                                  6
184900141128Codice area errato                                                              7
185000141128Categoria merceologica errata                                                   8
185100141128Codice commerciale errato  o  non in gestione all'utente                        9
185200141128Richiedere solo commerciali UNIFICANTI                                         10
185300141128Campagna Commerciale errata                                                    11
185400141128Selezione che richiede una Campagna                                            12
185500141128Cliente non trovato nella campagna commerciale indicata                        13
185600141128Immettere la percentuale traffico affidato a BRT                               14
185700141128Traffico Affidato a BRT "DA" incongruente con Traffico Affidato a BRT "A"      15
185800141128Immettere Valore                                                               16
185900141128Valore "DA" incongruente con Valore "A"                                        17
186000141128Paese estero errato                                                            18
186100141128Immettere Peso medio a Spedizione                                              19
186200141128Peso Medio Spedizione "DA" incongruente con Peso Medio Spedizione "A"          20
186300141128Concorrente errato                                                             21
186400141128Se non richiesti CLIENTI interessati a LOGISTICA non immettere le date         22
186500141128Data errata                                                                    23
186600141128Data "DAL" incongruente con data "AL"                                          24
186700141128Immettere i Concorrenti SOLO se selezionata Logistica in outsourcing           25
186800141128Concorrente Logistica errato                                                   26
186900141128Codice Cliente errato                                                          27
