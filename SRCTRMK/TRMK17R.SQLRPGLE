000100130808     /*END
000200091221      //--------------------------------------------------------------
000300091218      //?TRMK17R - INSERIMENTO ATTIVITÀ COMMERCIALI
000400091221      //--------------------------------------------------------------
000500091221
000600091218     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700090610     h dftactgrp(*no) actgrp(*caller)
000800091221
000900091221      //---------------------------------------------------------------
001000091221      //?Dichiarazione file.
001100091221      //---------------------------------------------------------------
001200100331      // - Calendario
001300100331     fAZCLN01L  if   e           k disk
001400100331
001500100331      // - Anagrafica clienti
001600100331     fCNACO00F  if   e           k disk
001700100331     fCNACO16L  if   e           k disk    rename(cnaco000:cnaco16)
001800130808
001900130808      // -?Anagrafica Commerciali?
002000130808     fAZCMM01L  if   e           k disk
002100100331
002200100331      // - Tabelle
002300100331     fTABEL00F  if   e           k disk
002400100331
002500100331      // - Rubrica
002600100331     fTFNTC01L  if   e           k disk
002700091221
002800091221      // - Attività commerciale
002900091218     fTIATC01L  o  a e           k disk    commit(IMK17fcmt) usropn
003000100311     fTIATC04L  uf   e           k disk    commit(IMK17fcmt) usropn
003100100311     f                                     rename(TIATC000:TIATC04)
003200100311      // - Note
003300100311     fTICPN03L  uf   e           k disk
003400100331
003500091221      // - Trattative
003600091221     fTIVIS05L  if   e           k disk
003700100331
003800100322      // - Offerte
003900101215     fTIVOF11L  if   e           k disk
004000091221      // - Anagrafica potenziali
004100110223     fTNCPO01L  uf   e           k disk
004200091221
004300091221      // - Video
004400090611     fTRMK17D   cf   e             workstn
004500091229     f                                     sfile(MK17S02 : S02nrr)
004600091221     f                                     indds(IndDspF)
004700091221     f                                     infds(InfDspF)
004800091221
004900091221      //---------------------------------------------------------------
005000091221      //?Definizione costanti.
005100091221      //---------------------------------------------------------------
005200091221
005300091221      // - Tasti funzionali a video
005400091221     d c_F01           c                   const(x'31')
005500091221     d c_F02           c                   const(x'32')
005600091221     d c_F03           c                   const(x'33')
005700091221     d c_F04           c                   const(x'34')
005800091221     d c_F05           c                   const(x'35')
005900091221     d c_F06           c                   const(x'36')
006000091221     d c_F07           c                   const(x'37')
006100091221     d c_F08           c                   const(x'38')
006200091221     d c_F09           c                   const(x'39')
006300091221     d c_F10           c                   const(x'3A')
006400091221     d c_F11           c                   const(x'3B')
006500091221     d c_F12           c                   const(x'3C')
006600091221     d c_F13           c                   const(x'B1')
006700091221     d c_F14           c                   const(x'B2')
006800091221     d c_F15           c                   const(x'B3')
006900091221     d c_F16           c                   const(x'B4')
007000091221     d c_F17           c                   const(x'B5')
007100091221     d c_F18           c                   const(x'B6')
007200091221     d c_F19           c                   const(x'B7')
007300091221     d c_F20           c                   const(x'B8')
007400091221     d c_F21           c                   const(x'B9')
007500091221     d c_F22           c                   const(x'BA')
007600091221     d c_F23           c                   const(x'BB')
007700091221     d c_F24           c                   const(x'BC')
007800091221     d c_Enter         c                   const(x'F1')
007900091230
008000091230     d Digits          c                   const('0123456789')
008100091221
008200091221      //---------------------------------------------------------------
008300091221      //?Definizione schiere.
008400091221      //---------------------------------------------------------------
008500091230
008600091230      // - Agenti
008700100205     d $Age            s              7  0 dim(72)
008800110113
008900110113      // - Tipi trattativa tabella CCO
009000110113     d §CCOtpvds       ds            10    inz
009100110113     d  $Tpv                          1    inz  dim(10)
009200091221
009300091221      // - Messaggi di errore
009400140227     d $Msg            s             78    dim(26) ctdata perrcd(1)
009500091218
009600091221      //---------------------------------------------------------------
009700091221      //?Definizione aree dati.
009800091221      //---------------------------------------------------------------
009900091221
010000090610      // - Dati utente
010100091221     d §AzUte        e ds                  extname(AZUTE00F)
010200091221     d                                     dtaara
010300091221     d §DatiUte      e ds                  extname(dDatiUte)
010400091221     d                                     dtaara
010500091221
010600091221      //---------------------------------------------------------------
010700091221      //?Definizione strutture dati.
010800091221      //---------------------------------------------------------------
010900091221
011000091221      // - Status
011100091221     d Psds           sds
011200091221     d   SDSpgm          *proc
011300091221
011400091221      // - InfDS
011500091221     d InfDspF         ds
011600091221     d  dsp_aid              369    369a                                        AID byte
011700130808     d*//dsp_rig             370    370
011800130808     d*//dsp_col             371    371
011900091221
012000091221      // - Indicatori su DspF
012100091221     d IndDspF         ds
012200091229        // - Indicatori di gestione del subfile
012300091229     d  SflDsp_N                      1n   overlay(IndDspF : 30)
012400091229     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
012500091229     d  SflEnd                        1n   overlay(IndDspF : 32)
012600091218        // - Indicatori di errore
012700091218     d  ErrMessage                    1n   overlay(IndDspF : 28)
012800091222        // - Indicatore di visualizzazione campi
012900091222     d  Appuntamento                   n   overlay(IndDspF : 40)
013000110307     d  Blocco                         n   overlay(IndDspF : 41)
013100091228        // - Abilitazione tasti funzionali
013200100331     d  F08Attivo                      n   overlay(IndDspF : 08)
013300091229     d  F12Attivo                      n   overlay(IndDspF : 12)
013400091229     d  F18Attivo                      n   overlay(IndDspF : 18)
013500091222        // - Indicatore di protezione campi
013600091221     d  ProteggiATT                    n   overlay(IndDspF : 25)
013700091221     d  ProteggiCAU                    n   overlay(IndDspF : 26)
013800091223     d  ProteggiCMM                    n   overlay(IndDspF : 27)
013900091223     d  ProteggiDAD                    n   overlay(IndDspF : 29)
014000091222        // - Indicatore di pozionamento cursore
014100091223     d  PosCurATT                     1n   overlay(IndDspF : 50)
014200091223     d  PosCurCAU                     1n   overlay(IndDspF : 51)
014300091223     d  PosCurDAD                     1n   overlay(IndDspF : 52)
014400091223     d  PosCurHDA                     1n   overlay(IndDspF : 53)
014500091223     d  PosCurCMM                     1n   overlay(IndDspF : 54)
014600091223     d  PosCurOII                     1n   overlay(IndDspF : 55)
014700091223     d  PosCurOFI                     1n   overlay(IndDspF : 56)
014800091223     d  PosCurCO3                     1n   overlay(IndDspF : 57)
014900091229     d  PosCurDADF                    1n   overlay(IndDspF : 58)
015000100311     d  PosCurNO1                     1n   overlay(IndDspF : 59)
015100110307     d  PosCurBlc                     1n   overlay(IndDspF : 60)
015200091221        // - Indicatori di errore generico
015300091218     d  ErrGenerico                   1n   overlay(IndDspF : 99)
015400091221
015500091218      // Campi di comodo per gestione indicatori a video
015600091221     d WindDspF        ds                  inz
015700091221     d   WindDspFa             1     49    inz(*zero)
015800091221     d   WindDspFb            50     99    inz(*zero)
015900091223
016000091223      // - Mattino/Pomeriggio calendario
016100091223     d CLNmat          DS
016200091223     d  mat                    1     31    dim(31)
016300091223     d CLNpom          DS
016400091223     d  pom                    1     31    dim(31)
016500091221
016600091218      // - Controllo data
016700091221     d wlbdat          ds                  inz
016800091221     d  g02dat                 1      8  0
016900091221     d  g02inv                 9     16  0
017000091221     d  g02err                17     17
017100091221     d  g02tgi                18     22  0
017200091221
017300091221      // - Parametri ricevuti
017400091221     d KPJBA         e ds
017500091221     d TRMK17DS      e ds
017600100506
017700091218      // - Ricerca/Controllo tabelle
017800091218     d TIBS02ds      e ds                  inz
017900091221
018000091221      // - Reperimento dati utente
018100091221     d TIBS34ds      e ds
018200091221     d dUte01        e ds
018300100208
018400100208      // - controllo abilitazioni
018500100208     d TNTAA1DS      e ds                  inz
018600100927
018700100927      // - Gestione Rubrica
018800100927     d TNTA12DS      e ds                  inz
018900091222
019000100120      // - Controllo se ci sono trattative aperte
019100100120     d TNTA43ds      e ds                  inz
019200100120
019300091222      // - Controllo se commerciale diverso da commerciale trattativa
019400091222     d TNTA45ds      e ds                  inz
019500110315
019600110315      // - Stampa lettara testo 54 - Blocco clienti
019700110315     d TNTA53ds      e ds                  inz
019800101215
019900101215      // - Gestione data caldo
020000101215     d TNTA63ds      e ds                  inz
020100091218
020200091223      // - Ricerca Causale attività
020300091223     d TNTB74ds      e ds                  inz
020400100504
020500100504      // - Ricerca Tipo attività
020600100504     d TNTB81ds      e ds                  inz
020700110223
020800110223      // - Calcolo categoria potenziale
020900110223     d TRMK05ds      e ds
021000091223
021100091218      // -  Gestione Note clienti/potenziali
021200091218     d TRMK20ds      e ds                  inz
021300091222
021400091222      // -  Inserimento Affiancamento
021500091222     d TRMK22ds      e ds
021600100331
021700100331      // -  Invio mail conferma appuntamento
021800100915     d TRMK23ds1     e ds
021900091218
022000091218      // - Agenda commerciale
022100091218     d TRMK82ds      e ds
022200091218
022300091218      // -  Controllo se commerciale impegnato
022400091218     d TRMK84ds      e ds
022500100120
022600100120      // -  Controllo se ci sono altre attività aperte
022700100120     d TRMK85ds      e ds                  inz
022800091218
022900091218      // - Reperimento filiali
023000091218     d TRUL31DS      e ds
023100091218     d POG                    10    759    DIM(250)
023200091218
023300091218      // - Ricerca ultimo numero attività
023400091218     d TRUL33ds      e ds                  inz
023500100322
023600100322      // - Aggiunge/Toglie gg/mm dalla data
023700100322     d XGIOLAVds     e ds                  inz
023800110223
023900110223      // - Dati FLO file TIATC00F
024000110223     d dATC01        e ds                  inz
024100091221
024200091221      // - Tabella CCO = Causale contatto
024300091221     d dCCO          e ds                  inz
024400091221
024500110502      // - Tabella CPO = Categoria potenziale
024600110502     d dCPO          e ds                  inz
024700110502
024800091221      // - Tabella TTA = Tipo attività
024900091221     d dTTA          e ds                  inz
025000110323
025100110323      // - Tabella BI = Causali Blocco Cliente
025200110323     d dsBI          e ds                  inz
025300091221
025400091221      //---------------------------------------------------------------
025500091221      //?Definizione variabili globali.
025600091221      //---------------------------------------------------------------
025700091221
025800091221      // - Flags booleani
025900100315     d $Att80          s               n   inz(*off)
026000100114     d $End            s               n   inz(*off)
026100091218     d $EoF            s               n   inz(*off)
026200091218     d $ErrGrave       s               n   inz(*off)
026300091223     d $Festa          s               n   inz(*off)
026400091221     d $Fine           s               n   inz(*off)
026500100119     d $ForzaCMM       s               n   inz(*off)
026600100119     d $ForzaImpegno   s               n   inz(*off)
026700100310     d $ForzaInt       s               n   inz(*off)
026800140227     d $ForzaFerie     s               n   inz
026900091230     d $InzC02         s               n   inz(*off)
027000091230     d $InzDP2         s               n   inz(*off)
027100091230     d $InzDT2         s               n   inz(*off)
027200091223     d $InzD01         s               n   inz(*off)
027300091230     d $InzT01         s               n   inz(*on)
027400091229     d $InzT02         s               n   inz(*off)
027500100311     d $InzW01         s               n   inz(*on)
027600091230     d $NoAge          s               n   inz(*on)
027700091221
027800091221      // - Indici di schiera
027900091221     d xx              s              3  0 inz
028000091230     d yy              s              3  0 inz
028100091221
028200091221      // - Campi associati al video
028300091223     d $Video          s              2    inz('T1')
028400091229     d S02nrr          s              4  0 inz
028500091221
028600091221      // - Campi di comodo data
028700091221     d  data_eur       s               d   Datfmt(*eur)
028800091229     d  data_iso       s               d   Datfmt(*iso)
028900091229     d  data_isof      s               d   Datfmt(*iso)
029000100114
029100100114       // - Stringa SQL da eseguire
029200131108     d wSQL            s           2048    Varying        inz
029300091221
029400091221      // - Campi di comodo
029500131108     d wCMMcod         s                   like(CMMcod)   inz
029600100310     d sav_V01cmm      s                   like(V01cmm)
029700091223     d wanno           s              4  0
029800131108     d wdata           s                   like(ATCdco)   inz
029900091223     d wgiorno         s              2  0
030000091223     d wmese           s              2  0
030100131108     d wdad            s                   like(atcdad)   inz
030200131108     d wdadf           s                   like(atcdad)   inz
030300100331     d wrst            s                   like(IMK23rst)
030400100331     d wemr            s                   like(IMK23emr)
030500091223     d w0030           s              3  0
030600111123     d woggi           s              8  0 inz
030700110307
030800110307     d idTabella       s              2
030900110307     d Ordinamento     s              1
031000110307     d idElemento      s              8
031100110307     d TastoUscita     s              1
031200091221
031300091221      //---------------------------------------------------------------
031400091221      //?Definizione procedure usate.
031500091221      //---------------------------------------------------------------
031600100507
031700100120      // - Controllo altre trattative aperte
031800100120     d Tnta43R         pr                  extpgm('TNTA43R')
031900100120     d  tnta43ds                           likeds(tnta43ds)
032000091222
032100091222      // - Controllo commerciale con trattativa
032200091222     d Tnta45r         pr                  extpgm('TNTA45R')
032300091222     d  tnta45ds                           likeds(tnta45ds)
032400110315
032500110315      // - Stampa testo 54
032600110315     d Tnta53r         pr                  extpgm('TNTA53R')
032700110315     d  kpjba                              likeds(kpjba)
032800110315     d  tnta53ds                           likeds(tnta53ds)
032900101215
033000101215      // - Gestione data caldo
033100101215     d Tnta63r         pr                  extpgm('TNTA63R')
033200101215     d  kpjba                              likeds(kpjba)
033300101215     d  tnta63ds                           likeds(tnta63ds)
033400100504
033500100504      // - Interrogazione/Selezione tipo Attività
033600100504     d Tntb81r1        pr                  extpgm('TNTB81R1')
033700100504     d  kpjba                              likeds(kpjba)
033800100504     d  tntb81ds                           likeds(tntb81ds)
033900110223
034000110223      // - Calcolo categoria potenziale
034100110223     d Trmk05r         pr                  extpgm('TRMK05R')
034200110223     d  kpjba                              likeds(KPJBA)
034300110223     d  trmk05ds                           likeds(TRMK05ds)
034400091222
034500091222      // - Affiancamento
034600091222     d Trmk22r         pr                  extpgm('TRMK22R')
034700091222     d  kpjba                              likeds(kpjba)
034800091222     d  trmk22ds                           likeds(trmk22ds)
034900091218
035000091218      // - Agenda
035100091218     d Trmk82r         pr                  extpgm('TRMK82R')
035200091218     d  kpjba                              likeds(kpjba)
035300091229     d Trmk82c         pr                  extpgm('TRMK82C')
035400091229     d  kpjba                              likeds(kpjba)
035500091218
035600091218      // - Controllo agenda
035700091218     d Trmk84r         pr                  extpgm('TRMK84R')
035800091218     d  trmk84ds                           likeds(trmk84ds)
035900100120
036000100120      // - Controllo altre attività aperte
036100100120     d Trmk85r         pr                  extpgm('TRMK85R')
036200100120     d  trmk85ds                           likeds(trmk85ds)
036300121127
036400121127      // - Interrogazione tabella BI
036500121127     d TRTBBIR         pr                  extpgm('TRTBBIR')
036600121127     d  kpjba                              likeds(kpjba)
036700100322
036800100322      // - Aggiunge/Toglie gg/mm dalla data
036900100322     d Xgiolav         pr                  extpgm('XGIOLAV')
037000100322     d  xgiolavds                          likeds(xgiolavds)
037100091221
037200091221      //---------------------------------------------------------------
037300091221      //?prototipi
037400091221      //---------------------------------------------------------------
037500091221      /copy gaitrasrc/srcprotopr,tibs02r
037600091221      /copy gaitrasrc/srcprotopr,tibs34r
037700100208      /copy gaitrasrc/srcprotopr,tntaa1c
037800100927      /copy gaitrasrc/srcprotopr,tnta12r
037900091223      /copy gaitrasrc/srcprotopr,tntb74r
038000091221      /copy gaitrasrc/srcprotopr,trmk20r
038100100915      /copy gaitrasrc/srcprotopr,trmk23r1
038200130808      /copy gaitrasrc/srcprotoPI,TRMK43R_1
038300130808      /copy gaitrasrc/srcprotoPR,TRMK43R
038400110307      /copy gaitrasrc/srcprotopr,trul19r
038500091221      /copy gaitrasrc/srcprotopr,trul31r
038600091221      /copy gaitrasrc/srcprotopr,trul33r
038700091223      /copy gaitrasrc/srcprotopr,xsrda8
038800091221
038900091221      //---------------------------------------------------------------
039000091221      //?Definizione key-list.
039100091221      //---------------------------------------------------------------
039200091223
039300091223       // - File TABEL00F
039400091223     d k03tabel      e ds                  extname(TABEL00F:*key)
039500091223     d                                     prefix(k_)
039600091223
039700091223       // - File AZCLN01L
039800091223     d k04azcln      e ds                  extname(AZCLN01L:*key)
039900091223     d                                     prefix(k_)
040000091223     d                                     inz
040100091221
040200091221      //---------------------------------------------------------------
040300091221      //?Riepilogo indicatori.
040400091221      //---------------------------------------------------------------
040500091221      // 25    : Protezione : Tipo attività
040600091221      // 26    : Protezione : Causale attività
040700091223      // 27    : Protezione : Commerciale attività
040800091221      // 28    : Emissione messaggio di errore a video
040900091223      // 29    : Protezione : Data attività
041000091229      // 30    : Condiziona SFLDSP
041100091229      // 31    : Condiziona SFLDSPCTL
041200091229      // 32    : Condiziona SFLEND
041300091221      // 50    : Errore: Tipo attività
041400091221      // 51    : Errore: Causale attività
041500091222      // 52    : Errore: Data attività
041600091222      // 53    : Errore: Ora attività
041700091222      // 54    : Errore: Commerciale
041800091222      // 55    : Errore: Ora inizio attività
041900091222      // 56    : Errore: Ora fine   attività
042000091222      // 57    : Errore: Commerciale che inserisce attività
042100091221      // 99    : Generico di Errore
042200091221      //---------------------------------------------------------------
042300091221
042400091221      //---------------------------------------------------------------
042500091221      //?M A I N - L I N E
042600091221      //---------------------------------------------------------------
042700091221
042800091221     c     *Entry        plist
042900091221     c                   parm                    KPJBA
043000091221     c                   parm                    trmk17ds
043100091221
043200091221      /free
043300091221
043400091218       //?Operazioni iniziali
043500091221       exsr RoutInz;
043600110113
043700110113       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
043800091221
043900091221       // Gestione video
044000091221       DOW $Fine = *off;
044100091221         select;
044200091223           when $Video = 'T1';
044300091223             exsr GesT01;
044400091223           when $Video = 'D1';
044500091221             exsr GesD01;
044600100311           when $Video = 'W1';
044700100311             exsr GesW01;
044800091229           when $Video = 'T2';
044900091229             exsr GesT02;
045000091229           when $Video = 'DP';
045100091229             exsr GesDP2;
045200091229           when $Video = 'DT';
045300091229             exsr GesDT2;
045400091229           when $Video = 'C2';
045500091229             exsr GesC02;
045600091221           other;
045700091221             $Fine = *on;
045800091221         endsl;
045900091221       ENDDO;
046000091221
046100091221       // Operazioni finali
046200091221       exsr RoutEnd;
046300100114
046400100114       //--------------------------------------------------------------
046500100114       //?Operazioni iniziali.
046600100114       //--------------------------------------------------------------
046700100114       BEGSR RoutInz;
046800100114
046900100114         // Impostazione campi "fissi"
047000100114         k_TBLkut = 1;
047100100114
047200100114         // Reperimento dati job
047300100114         exsr DatiJob;
047400111123
047500111123         // Imposto la data del giorno
047600111123         woggi = %dec(%date());
047700100114
047800100208         // Controllo se l'utente è abilitato alla funzione
047900100208         reset TNTAA1DS;
048000100208         ITAA1caut = '§utegtc';
048100100208         kpjbu     = TNTAA1DS;
048200100208         tntaa1c (kpjba);
048300100208         TNTAA1DS = kpjbu;
048400100208
048500100208         IF  OTAA1fabi = 'N';
048600100208           $Fine = *on;
048700100208           $ErrGrave   = *on;
048800100208           V01msg = $Msg(01);
048900100208           leavesr;
049000100208         ENDIF;
049100100208
049200100208         clear TRUL31DS;
049300100208         I31abi = OTAA1cabi;
049400100208         I31cdi = DUTdis;
049500100208         I31car = DUTare;
049600100208         I31cpo = DUTpou;
049700100208         callp trul31r (kpjba : trul31ds);
049800100208         IF  O31pog <= *zeros;
049900100208           $Fine = *on;
050000100208           $ErrGrave = *on;
050100100208           V01msg = $Msg(01);
050200100208           leavesr;
050300100208         ENDIF;
050400100114
050500100114         //?Flag commit
050600100114         IF IMK17fcmt <> '1';
050700100114           IMK17fcmt = '0';
050800100114           IMK17cmt = 'N';
050900100114         ENDIF;
051000100114
051100100114         open tiatc01l ;
051200100311         open tiatc04l ;
051300100114
051400100114         //?Controllo congruenza flag commit
051500100114         //?se apertura file sotto commit si deve specificare se è questo pgm
051600100114         //?a dover fare il commit o no
051700100114         IF  IMK17fcmt = '1' and IMK17cmt <> 'N' and IMK17cmt <> 'S';
051800100114           $Fine = *on;
051900100114           ErrGenerico = *on;
052000100114           V01msg = $Msg(02);
052100100114           leavesr;
052200100114         ENDIF;
052300100114
052400100114         //?Controllo congruenza tra campi passati ed eventuali protezione campi
052500100114         IF  IMK17att = *blanks and IMK17patt <> *blanks;
052600100114           $Fine = *on;
052700100114           ErrGenerico = *on;
052800100114           V01msg = $Msg(03);
052900100114           leavesr;
053000100114         ENDIF;
053100100114         IF  IMK17cau = *blanks and IMK17pcau <> *blanks;
053200100114           $Fine = *on;
053300100114           ErrGenerico = *on;
053400100114           V01msg = $Msg(03);
053500100114           leavesr;
053600100114         ENDIF;
053700100114         IF  IMK17cmm = *zeros and IMK17pcmm <> *blanks;
053800100114           $Fine = *on;
053900100114           ErrGenerico = *on;
054000100114           V01msg = $Msg(03);
054100100114           leavesr;
054200100114         ENDIF;
054300100114         IF  IMK17dad = *zeros and IMK17pdad <> *blanks;
054400100114           $Fine = *on;
054500100114           ErrGenerico = *on;
054600100114           V01msg = $Msg(03);
054700100114           leavesr;
054800100114         ENDIF;
054900100114
055000100114         //?Se non ho potenziale, cliente e trattativa posso inserire solo
055100100114         //?attività d'ufficio o ferie
055200100114         IF  IMK17cpo = 0 and IMK17ksc = 0 and IMK17nrv = 0;
055300100114           $InzT01 = *off;
055400100114           $Video  = 'T2';
055500100114           $InzT02 = *on;
055600100114         ENDIF;
055700100114
055800100114         //?Se tipo attività bloccata e causale bloccata passo alla videata sucessiva
055900100114         //?do per assunto che il blocco del tipo attività e della causale
056000100114         //?non rigurarda le attività d'ufficio ma solo quelle legate al cliente
056100100114         IF  IMK17patt = 'S' and IMK17pcau = 'S';
056200100114           exsr InzT01;
056300100114           $InzT01 = *off;
056400100114           $Video  = 'D1';
056500100114           $InzD01 = *on;
056600100114         ENDIF;
056700100114
056800100114         //?Attivo F12=Ritorno
056900100114         F12Attivo = (IMK17F12 <> 'S');
057000100114
057100100114       ENDSR;
057200100114
057300100114       //--------------------------------------------------------------
057400100114       //?Reperimento Dati del job (Utente/Operativi).
057500100114       //--------------------------------------------------------------
057600100114       BEGSR DatiJob;
057700100114
057800100114         in(E) §AzUte;
057900100114         if NOT %error;
058000100114           in(E) §DatiUte;
058100100114         endif;
058200100114         if %error or RSut = *blanks;
058300100114           clear TIBS34ds;
058400100114           tibs34r(tibs34ds);
058500100114           in §AzUte;
058600100114           in §DatiUte;
058700100114         endif;
058800100114
058900100114       ENDSR;
059000091221
059100091221       //--------------------------------------------------------------
059200091223       //?Gestione videata T01 - Richiesta tipo attività/causale.
059300091221       //--------------------------------------------------------------
059400091223       BEGSR GesT01;
059500091221
059600091221         // Inizializzazione videata
059700091223         if $InzT01 = *on;
059800091223           exsr InzT01;
059900091223           $InzT01 = *off;
060000091221         endif;
060100091221
060200091229       //?Emissione videata
060300091223         exfmt MK17T01;
060400091221         errMessage   = *off;
060500091221         errGenerico  = *off;
060600091221         clear V01msg;
060700091230
060800091230         SELECT;
060900091230
061000091230       //?F12 = Ritorno
061100091230           WHEN dsp_aid = c_F12;
061200100108             exsr F12T01;
061300091230             leavesr ;
061400091221
061500091221       //?Enter
061600091230           OTHER;
061700091221       //?Controllo i dati
061800091230             exsr CtrT01;
061900091230             IF  ErrGenerico;
062000091230               leavesr;
062100091230             ENDIF;
062200091230         ENDSL;
062300091221
062400091221       //?Emetto la videata sucessiva in base al tipo attività
062500091221       //?se ufficio o no
062600091221         IF  §TTAleg = 'S';
062700091223           $Video = 'D1';
062800091223           $InzD01 = *on;
062900091221         ELSE;
063000091223           $Video = 'T2';
063100091223           $InzT02 = *on;
063200091221         ENDIF;
063300091222
063400091222       //?Attività APPUNTAMENTO
063500091222         Appuntamento = (V01att = 'A');
063600091221
063700091221       ENDSR;
063800091221
063900091221       //--------------------------------------------------------------
064000091223       //?Caricamento videata T01.
064100091221       //--------------------------------------------------------------
064200091223       BEGSR InzT01;
064300091221
064400110502         clear CPOfls;
064500091223         clear MK17T01;
064600091223         V01pgm = SDSpgm;
064700091228         V01sut = rsut;
064800091228         V01sif = knsif;
064900091228         V01mus = knmus;
065000091221
065100091221       //?Potenziale
065200091222         IF  IMK17cpo > 0;
065300091223           V01cpo = IMK17cpo;
065400110223           chain(n) V01cpo TNCPO01L;
065500091222           IF  %found(TNCPO01L);
065600091222             V01dcpo = CPOrag;
065700091222           ENDIF;
065800091223         ELSE;
065900091223           V01dcpo = '????????????';
066000091222         ENDIF;
066100091221
066200091221       //?Cliente
066300091221         IF  IMK17ksc > 0;
066400091222           V01ksc = IMK17ksc;
066500091221           acokut = 1;
066600091221           acokcc = dutkci ;
066700091222           chain (acokut:acokcc:V01ksc) CNACO00F;
066800091223           IF  %found(CNACO00F);
066900091221             V01drag = ACOrag;
067000091221           ENDIF;
067100091221         ENDIF;
067200091221
067300091221       //?Trattativa
067400091221         IF  IMK17nrv > 0;
067500091222           V01nrv = IMK17nrv;
067600091222           chain V01nrv TIVIS05L;
067700091221           IF  %found(TIVIS05L);
067800091221             data_iso = (%date(VISdat));
067900091221             data_eur = data_iso;
068000091223             V01dnrv  = %dec(data_eur);
068100091223             V01com   = viscmm;
068200100303           ELSE ;
068300100303       // se trattativa inesistente perchè non ancora creata recupero i dati dalla ds
068400100303             V01dnrv  = imk17dat ;
068500100303             v01com   = imk17cgt ;
068600091221           ENDIF;
068700100311
068800091223       //?Commerciale trattativa
068900091223           IF  V01com > 0;
069000130808             chain  (V01com)  AZCMM000;
069100130808             IF  %found(AZCMM01L);
069200130808               V01dcom = CMMdes;
069300091223             ENDIF;
069400091223           ENDIF;
069500091223         ENDIF;
069600091221
069700091221       //?Tipo attività
069800091221         IF  IMK17att <> *blanks;
069900091230           V01att = IMK17att;
070000100113           ProteggiATT = (IMK17patt = 'S');
070100091221           clear TIBS02ds;
070200091221           clear dTTA;
070300091221           T02mod = 'C';
070400091221           T02cod = 'TTA';
070500091222           T02ke1 = V01att;
070600091221           T02sif = KNSIF;
070700091221           TNTBE_RicercaControllo (kpjba : tibs02ds);
070800091221           IF  T02err = *blanks;
070900091221             dTTA = T02uni;
071000091221           ENDIF;
071100100113           V01datt = §TTAdes;
071200091221         ENDIF;
071300091221
071400091221       //?Causale
071500091221         IF  IMK17cau <> *blanks;
071600091222           V01cau = IMK17cau;
071700091221           ProteggiCAU = (IMK17pcau = 'S');
071800091221           clear TIBS02ds;
071900091221           clear dCCO;
072000091221           T02mod = 'C';
072100091221           T02cod = 'CCO';
072200091222           T02ke1 = V01cau;
072300091221           T02sif = KNSIF;
072400091221           TNTBE_RicercaControllo (kpjba : tibs02ds);
072500091221           IF  T02err = *blanks;
072600091221             dCCO = T02uni;
072700091221           ENDIF;
072800091221           V01dcau = §CCOdes;
072900110307           //?Imposto se attività di blocco cliente
073000110307           Blocco = (§CCOblc <> *blanks);
073100091221         ENDIF;
073200100120
073300100120       //?Verifico se ci sono altre trattative aperte o attività aperte
073400100120         clear TNTA43ds;
073500100120         ITA43cpo = V01cpo;
073600100120         ITA43ksc = V01ksc;
073700100120         ITA43nrv = V01nrv;
073800100120         Tnta43r (tnta43ds);
073900100120       //?Se trovo almeno un'altra trattativa aperte imposto la costante a video
074000100120         IF  OTA43nrv > 0;
074100100120           evalr V01segn = 'TRATTATIVE APERTE';
074200100120         ENDIF;
074300100120       //?Se non ho trovato altre trattative provo con altre attività
074400100120         IF  V01segn = ' ';
074500100120           clear TRMK85ds;
074600100120           IMK85cpo = V01cpo;
074700100120           IMK85ksc = V01ksc;
074800100120           IMK85nrv = V01nrv;
074900100120           Trmk85r (trmk85ds);
075000100120         //?se torna errore <> "9" ho trovato delle attività aperte
075100100120           IF  OMK85err <> *blanks and OMK85err <> '9';
075200100120         //?imposto la costante a video
075300100120             evalr V01segn = 'ATTIVITA'' APERTE';
075400100120           ENDIF;
075500100120         ENDIF;
075600110502
075700110502       //?Categoria potenziale
075800110502         IF  CPOfls <> *blanks;
075900110502           clear TIBS02ds;
076000110502           clear dCPO;
076100110502           T02mod = 'C';
076200110502           T02cod = 'CPO';
076300110502           T02ke1 = CPOfls;
076400110502           T02sif = KNSIF;
076500110502           TNTBE_RicercaControllo (kpjba : tibs02ds);
076600110502           IF  T02err = *blanks;
076700110502             dCPO = T02uni;
076800110502           ENDIF;
076900110502         ENDIF;
077000091221
077100091221       ENDSR;
077200091221
077300091221       //--------------------------------------------------------------
077400091223       //?Controllo videata T01.
077500091221       //--------------------------------------------------------------
077600091223       BEGSR CtrT01;
077700091221
077800091221         WindDspF = IndDspF;
077900091221         reset WindDspFb;
078000091221         IndDspF  = WindDspF;
078100100113
078200100113       //?Tipo attività
078300100113         IF  not ProteggiATT;
078400100113           exsr CtrATTT01;
078500100113           IF  ErrGenerico;
078600100113             leavesr;
078700100113           ENDIF;
078800100113         ENDIF;
078900100113
079000100113       //?Causale
079100100113         IF  not ProteggiCAU;
079200100113           exsr CtrCAUT01;
079300100113           IF  ErrGenerico;
079400100113             leavesr;
079500100113           ENDIF;
079600100113         ENDIF;
079700100113
079800100113       ENDSR;
079900100113
080000100113       //--------------------------------------------------------------
080100100113       //?Controllo tipo attività T01.
080200100113       //--------------------------------------------------------------
080300100113       BEGSR CtrATTT01;
080400091221
080500091221       //?Tipo attività  -  Ricerca
080600091222         IF  %scan('?' : V01att) > *zero;
080700091221           ErrGenerico = *on;
080800091223           PosCurATT   = *on;
080900091221           clear dTTA;
081000100504           clear TNTB81DS;
081100100504           ITTAleg = 'S';
081200100504           tntb81R1 (kpjba : TNTB81ds);
081300100504           IF  OTTAerr = '1';
081400100504             ErrMessage  = *on;
081500100504             V01msg = T02msg;
081600100504             leavesr;
081700100504           ENDIF;
081800100504             V01att  = OTTAke1;
081900100504             dTTA    = OTTAuni;
082000100113             V01datt = §TTAdes;
082100091221         ENDIF;
082200091221
082300091221       //?Tipo attività  -  Obbligatoria
082400091222         IF  V01att = *blanks ;
082500091221           ErrMessage  = *on;
082600091221           ErrGenerico = *on;
082700091223           PosCurATT   = *on;
082800091223           V01msg      = $Msg(04);
082900091221           leavesr;
083000091221         ENDIF;
083100091221
083200091221       //?Tipo attività  -  Controllo
083300091221         clear TIBS02ds;
083400091221         clear dTTA;
083500091221         T02mod = 'C';
083600091221         T02cod = 'TTA';
083700091222         T02ke1 = V01att;
083800091221         T02sif = KNSIF;
083900091221         TNTBE_RicercaControllo  (kpjba : tibs02ds);
084000091221         IF  T02err = *blanks;
084100091221           dTTA = t02uni  ;
084200091221         ELSE;
084300091221           ErrMessage  = *on;
084400091221           ErrGenerico = *on;
084500091223           PosCurATT   = *on;
084600091223           V01msg      = $Msg(04);
084700091221           leavesr;
084800091221         ENDIF;
084900100113         V01datt = §TTAdes;
085000091222
085100091222       //?Tipo attività  -  Legata a cliente deve esserci il potenziale
085200091222         IF  §TTAleg = 'S' and IMK17cpo = 0;
085300091222           ErrMessage  = *on;
085400091222           ErrGenerico = *on;
085500091223           PosCurATT   = *on;
085600091223           V01msg      = $Msg(04);
085700091222           V01msg      = %trim(V01msg) + ' Manca il potenziale';
085800091222           leavesr;
085900091222         ENDIF;
086000091222
086100091222       //?Tipo attività  -  Appuntamento/Offerta non ammesso se non c'è trattativa
086200091222         IF  (V01att = 'A' or V01att = 'O')  and V01nrv = 0;
086300091222           ErrMessage  = *on;
086400091222           ErrGenerico = *on;
086500091223           PosCurATT   = *on;
086600091223           V01msg      = $Msg(04);
086700091223           V01msg      = %trim(V01msg) + '. Manca la trattativa';
086800091222           leavesr;
086900091222         ENDIF;
087000091223
087100091223       //?Tipo attività  -  Accetto solo attività legata al cliente
087200091223       //?                  da questa videata
087300091223         IF  §TTAleg <> 'S';
087400091223           ErrMessage  = *on;
087500091223           ErrGenerico = *on;
087600091223           PosCurATT   = *on;
087700091223           V01msg      = $Msg(04);
087800091223           V01msg      = %trim(V01msg) + '. Inserita attività ufficio.';
087900091223           leavesr;
088000091223         ENDIF;
088100091222
088200100113       ENDSR;
088300100113
088400100113       //--------------------------------------------------------------
088500100113       //?Controllo causale T01.
088600100113       //--------------------------------------------------------------
088700100113       BEGSR CtrCAUT01;
088800091221
088900091221       //?Causale  -  Ricerca
089000091222         IF  %scan('?' : V01cau) > *zero;
089100091221           ErrGenerico = *on;
089200091223           PosCurCAU   = *on;
089300091221           clear dCCO;
089400091223           clear TNTB74ds;
089500091223           ICCOatt = V01att;
089600091223           ICCOuti = 'S';
089700110502         //?Passo categoria del potenziale
089800110502           ICCOcod = CPOfls;
089900100322         //?flag x attività con o senza trattativa
090000100322           IF  V01nrv > 0;
090100100322             ICCOtra = 'S';
090200110113             ICCOtpv = VIStpv;
090300100322           ELSE;
090400100322             ICCOtra = 'N';
090500110113             clear ICCOtpv;
090600100322           ENDIF;
090700100330         //?flag x trattativa con o senza offerta
090800100330           ICCOoff = 'N';
090900101215           setll V01nrv TIVOF11L;
091000101215           reade V01nrv TIVOF11L;
091100101215           DOW  not %eof(TIVOF11L);
091200100422             IF  VOFeso <> 'H' and vofeso <> '*' ;
091300100330               ICCOoff = 'S';
091400100330               leave;
091500100330             ENDIF;
091600101215             reade V01nrv TIVOF11L;
091700100330           ENDDO;
091800100330
091900091223           tntb74R  (kpjba : TNTB74ds);
092000100330
092100100311           IF  OCCOerr = '1';
092200100310             ErrMessage  = *on;
092300100310             V01msg = T02msg;
092400100310             leavesr;
092500100310           ENDIF;
092600100310
092700100310           V01cau = OCCOke1;
092800100310           dCCO   = OCCOuni;
092900091221           V01dcau = §ccodes;
093000091221         ENDIF;
093100091221
093200091221       //?Causale  -  Obbligatoria
093300091222         IF  V01cau = *blanks ;
093400091221           ErrMessage  = *on;
093500091221           ErrGenerico = *on;
093600091223           PosCurCAU   = *on;
093700091223           V01msg      = $Msg(05);
093800091221           leavesr;
093900091221         ENDIF;
094000091221
094100091221       //?Causale  -  Controllo
094200091221         clear TIBS02ds;
094300091221         clear dCCO;
094400091221         T02mod = 'C';
094500091221         T02cod = 'CCO';
094600091222         T02ke1 = V01cau;
094700091221         T02sif = KNSIF;
094800091221         TNTBE_RicercaControllo (kpjba : tibs02ds);
094900100310         IF  T02err <> *blanks;
095000091221           ErrMessage  = *on;
095100091221           ErrGenerico = *on;
095200091223           PosCurCAU   = *on;
095300091223           V01msg      = $Msg(05);
095400091221           leavesr;
095500091221         ENDIF;
095600100310
095700100310         dCCO = t02uni  ;
095800110113         §CCOtpvds = §CCOtpv;
095900091223         V01dcau = §ccodes;
096000091223
096100091223       //?Causale  -  Controllo se in uso agli utenti
096200091223         IF  §CCOuti = 'N';
096300091223           ErrMessage  = *on;
096400091223           ErrGenerico = *on;
096500091223           PosCurCAU   = *on;
096600091223           V01msg      = $Msg(05);
096700091223           leavesr;
096800091223         ENDIF;
096900091223
097000091223       //?Causale  -  Controllo se compatibile con tipo attività
097100091223         IF  §CCOatt <> V01att;
097200091223           ErrMessage  = *on;
097300091223           ErrGenerico = *on;
097400091223           PosCurCAU   = *on;
097500091223           V01msg      = $Msg(05);
097600091223           leavesr;
097700091223         ENDIF;
097800100310
097900110502       //?Causale  -  Controllo se utilizzabile in base alla categoria del potenziale
098000110502         IF  §CCOcpo <> *blanks;
098100110502           IF %scan(CPOfls:§CCOcpo) = 0;
098200110502             ErrMessage  = *on;
098300110502             ErrGenerico = *on;
098400110502             PosCurCAU   = *on;
098500110502             V01msg      = $Msg(16);
098600110503             V01msg      = %trim(V01msg) + ' ' + §CPOdes;
098700110502             leavesr;
098800110502           ENDIF;
098900100310         ENDIF;
099000100310
099100100310       //?Causale  -  Controllo se NON deve essere legata a trattativa
099200100310         IF  §CCOtra = 'N' and V01nrv > 0;
099300100310           ErrMessage  = *on;
099400100310           ErrGenerico = *on;
099500100310           PosCurCAU   = *on;
099600100310           V01msg      = $Msg(18);
099700100310           leavesr;
099800100310         ENDIF;
099900100322
100000100322       //?Causale  -  Controllo se deve essere legata a trattativa
100100100322         IF  §CCOtra = 'S' and V01nrv = 0;
100200100322           ErrMessage  = *on;
100300100322           ErrGenerico = *on;
100400100322           PosCurCAU   = *on;
100500100322           V01msg      = $Msg(20);
100600100322           leavesr;
100700100322         ENDIF;
100800110113
100900110113       //?Causale  -  Controllo se utilizzabile in base al tipo trattativa
101000110113         IF  V01nrv > 0 and §CCOtpv <> *blanks and
101100110113             %lookup(VIStpv:$Tpv) = 0;
101200110113           ErrMessage  = *on;
101300110113           ErrGenerico = *on;
101400110113           PosCurCAU   = *on;
101500110113           V01msg      = $Msg(24);
101600110113           V01msg      = %trim(V01msg) + ' ' + VIStpv;
101700110113           leavesr;
101800110113         ENDIF;
101900100322
102000100322       //?Causale  -  Controllo se deve essere utlizzata solo in presenza di offerte
102100100322         IF  §CCOoff = 'S' and V01nrv > 0;
102200101215           setll V01nrv TIVOF11L;
102300100322           IF  not %equal;
102400100322             ErrMessage  = *on;
102500100322             ErrGenerico = *on;
102600100322             PosCurCAU   = *on;
102700100322             V01msg      = $Msg(21);
102800100322             leavesr;
102900100422           else ;
103000101215             reade v01nrv tivof11l ;
103100101215             dow not %eof(tivof11l) ;
103200100422                 if vofeso  =  '*' ;
103300100422                    errMessage = *on ;
103400100422                 else ;
103500100422                    errMessage = *off;
103600100422                    leave ;
103700100422                 endif ;
103800101215                 reade v01nrv tivof11l ;
103900100422             enddo ;
104000100422         // se ho trovato tutti annullati emetto errore
104100100422             If errMessage  = *on;
104200100422                errGenerico = *on;
104300100422                PosCurCAU   = *on;
104400100422                V01msg      = $Msg(21);
104500100422                leavesr;
104600100422             endif ;
104700100422           endif ;
104800100322         ENDIF;
104900100322       //?Causale  -  Controllo se NON è utilizzabile in presenza di offerte
105000100322         IF  §CCOoff = 'N' and V01nrv > 0;
105100101215           setll V01nrv TIVOF11L;
105200101215           reade V01nrv TIVOF11L;
105300101215           DOW  not %eof(TIVOF11L);
105400100422             IF  VOFeso <> 'H'and vofeso <> '*' ;
105500100322               ErrMessage  = *on;
105600100322               ErrGenerico = *on;
105700100322               PosCurCAU   = *on;
105800100322               V01msg      = $Msg(22);
105900100322               leavesr;
106000100322             ENDIF;
106100101215             reade V01nrv TIVOF11L;
106200100322           ENDDO;
106300100322         ENDIF;
106400100311
106500100311       //?Causale  -  Controllo se richiamato da stampa trattative non posso usare 80
106600100311       //?            CHIODO 80
106700100311         IF  IMK17stp = 'S' and V01cau = '80';
106800100311           ErrMessage  = *on;
106900100311           ErrGenerico = *on;
107000100311           PosCurCAU   = *on;
107100100311           V01msg      = $Msg(05);
107200100311           V01msg      = %trim(V01msg) +
107300100311                       '. Non utilizzabile con questa funzione';
107400100311           leavesr;
107500100311         ENDIF;
107600110307
107700110307       //?Causale  -  Controllo se causale che deve richiamare pgm blocco clienti
107800110307       //?            in questo caso devo richiedere obbligatoria la causale
107900110307         Blocco = (§CCOblc <> *blanks);
108000091221
108100091221       ENDSR;
108200100108
108300100108       //--------------------------------------------------------------
108400100108       //?Gestione tasto funzionale F12 da videata T01.
108500100108       //?F12=Ritorno
108600100108       //--------------------------------------------------------------
108700100108       BEGSR F12T01;
108800100108
108900100108         // Ritorno al pgm richiamante
109000100203         OMK17f12 = 'S';
109100100108         $Fine  = *on ;
109200100108
109300100108       ENDSR;
109400091221
109500091221       //--------------------------------------------------------------
109600091223       //?Gestione videata D01 - Immissione dati attività cliente.
109700091221       //--------------------------------------------------------------
109800091223       BEGSR GesD01;
109900091221
110000091221         // Inizializzazione videata
110100091223         if $InzD01 = *on;
110200091223           exsr InzD01;
110300091223           $InzD01 = *off;
110400101215
110500101215         //?Se la causale immessa richiede il richiamo del pgm per la data cliente caldo
110600101215         //?prima di proseguire con la creazione dell'attività richiamo il pgm per gestire la
110700101215         //?data presunto affidamento merce
110800101215           IF  §CCOhot = 'S';
110900101215            write MK17D01;
111000101215            exsr Ric_PgmHot;
111100101215            //?Se F12 da gestione data caldo torno alla testata dove richiedo la causale
111200101215             IF  OTA63F12 = 'S';
111300101215               $Video = 'T1';
111400101215               $InzD01 = *off;
111500101215               leavesr;
111600101215             ENDIF;
111700101215            //?Se tutto ok imposto la data presunto affidamento nella data esecuzione
111800101215             data_eur = %date(OTA63dpa:*iso);
111900101215             V01dad   = %dec(data_eur);
112000101215           ENDIF;
112100101215
112200091221         endif;
112300091221
112400091229       //?Emissione videata
112500091223         exfmt MK17D01;
112600091221         errMessage   = *off;
112700091221         errGenerico  = *off;
112800091223         clear V01msg;
112900091221
113000091221         SELECT;
113100091221
113200091221       //?F12 = Ritorno
113300091221           WHEN dsp_aid = c_F12;
113400091223             exsr F12D01;
113500091221             leavesr ;
113600100927
113700100927       //?F02 = Rubrica
113800100927           WHEN dsp_aid = c_F02;
113900100927             exsr F02D01;
114000100927             leavesr ;
114100091221
114200091221       //?F11 = Agenda
114300091221           WHEN dsp_aid = c_F11;
114400091223             exsr F11D01;
114500091221             leavesr ;
114600091221
114700091221       //?F18 = Note
114800091221           WHEN dsp_aid = c_F18;
114900091223             exsr F18D01;
115000091221             leavesr ;
115100091221
115200091222       //?F06 = Conferma
115300091221           WHEN dsp_aid = c_F06;
115400091221             //?prima faccio i controlli
115500091223             exsr CtrD01;
115600091221             IF  ErrGenerico;
115700091221               leavesr;
115800091221             ENDIF;
115900091221             //?tutto ok confermo i dati
116000091223             exsr F06D01;
116100091221             leavesr ;
116200100331
116300100331       //?F08 = Conferma + Mail appuntamento
116400100331           WHEN dsp_aid = c_F08;
116500100331             //?prima faccio i controlli
116600100331             exsr CtrD01;
116700100331             IF  ErrGenerico;
116800100331               leavesr;
116900100331             ENDIF;
117000100331             //?tutto ok confermo i dati
117100100331             exsr F08D01;
117200100331             leavesr ;
117300091221
117400091221       //?Enter
117500091221           OTHER;
117600091221       //?Controllo i dati
117700091223            exsr CtrD01;
117800091221            IF  ErrGenerico;
117900091221              leavesr;
118000091221            ENDIF;
118100091221
118200091221         ENDSL;
118300091221
118400091221       ENDSR;
118500091221
118600091221       //--------------------------------------------------------------
118700091223       //?Caricamento videata D01.
118800091221       //--------------------------------------------------------------
118900091223       BEGSR InzD01;
119000091221
119100091230         clear V01cmm;
119200091230         clear V01dcmm;
119300091230         clear V01dad;
119400091230         clear V01hda;
119500091230         clear V01oii;
119600091230         clear V01ofi;
119700091230         clear V01co3;
119800091230         clear V01dco3;
119900110307         clear V01dblc;
120000091230         V01aff = 'N';
120100110412         V01stp = 'S';
120200091223         $ForzaImpegno = *off;
120300100119         $ForzaCMM     = *off;
120400100310         $ForzaInt     = *off;
120500100310         clear sav_V01cmm;
120600091223
120700100205       //?Imposto commerciale se passato
120800100208         IF  IMK17cmm > 0 and IMK17pcmm = *blanks;
120900100208       //?ma se non è richiesta la protezione e l'utente non è abilitato al commerciale
121000100208       //?non propongo il dato
121100100208           clear TNTAA1DS;
121200100208           ITAA1caut = '§utegtc';
121300100208           ITAA1cmm  = IMK17cmm;
121400100208           kpjbu     = tntaa1ds;
121500100208           tntaa1c (kpjba);
121600100208           TNTAA1DS = kpjbu;
121700100208           IF  OTAA1fabi = 'N';
121800100208             clear IMK17cmm;
121900100208           ENDIF;
122000100205         ENDIF;
122100091223       //?Imposto commerciale se passato
122200091223         IF  IMK17cmm > 0;
122300091223           V01cmm = %editc(IMK17cmm:'X');
122400130808           chain  (iMK17cmm)  AZCMM000;
122500130808           IF  %found(AZCMM01L);
122600130808             V01dcmm = CMMdes;
122700091223           ENDIF;
122800091223         //?proteggo se richiesto
122900091223           ProteggiCMM = (IMK17pcmm = 'S');
123000091223         ENDIF;
123100091223
123200091223       //?Imposto data attività se passata
123300091223         IF  IMK17dad > 0;
123400091223           data_eur = %date(IMK17dad:*iso);
123500091223           V01dad   = %dec(data_eur);
123600091223         //?proteggo se richiesto
123700091223           ProteggiDAD = (IMK17pdad = 'S');
123800091223         ENDIF;
123900091223
124000100205       //?Imposto commerciale che inserisce attività se passato
124100100208         IF  IMK17co3 > 0;
124200100208       //?ma solo se l'utente è abilitato al commerciale
124300100208           clear TNTAA1DS;
124400120810           ITAA1tipo = 'M';
124500120810           ITAA1caut = '§utegtc';
124600100208           ITAA1cmm  = IMK17co3;
124700100521           ITAA1abi = 'RA';
124800100208           kpjbu     = tntaa1ds;
124900100208           tntaa1c (kpjba);
125000100208           TNTAA1DS = kpjbu;
125100100208           IF  OTAA1fabi = 'N';
125200100205            clear IMK17co3;
125300100208           ENDIF;
125400100205         ENDIF;
125500091223       //?Imposto commerciale che inserisce attività se passato
125600091223         IF  IMK17co3 > 0;
125700100205           V01co3 = %editc(IMK17co3:'X');
125800130808           chain   (iMK17co3)  AZCMM000;
125900130808           IF  %found(AZCMM01L);
126000130808             V01dco3 = CMMdes;
126100100205           ENDIF;
126200091223         ENDIF;
126300091230
126400091230       //?Posiziono il cursore
126500091230         SELECT;
126600091230       //?Sul commerciale se NON passato
126700091230           WHEN IMK17cmm = 0;
126800091230             PosCurCMM = *on;
126900091230       //?Sulla data se commerciale passato
127000091230           WHEN IMK17cmm > 0;
127100091230             PosCurDAD = *on;
127200091230       //?Su ora attività se passata la data
127300091230           WHEN IMK17dad > 0;
127400091230             PosCurHDA = *on;
127500091230         ENDSL;
127600091230
127700100311       //?Controllo se per la trattativa ci sono attività 80 aperte
127800100311       //?CHIODO 80
127900100315       $Att80 = *off;
128000100311       IF  IMK17nrv > 0;
128100100322         setll (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
128200100322         reade(n) (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
128300100311         DOW not %eof(TIATC04L);
128400100510           IF  ATCcad = '80' and ATCatb = *blanks;
128500100315             $Att80 = *on;
128600100311             leave;
128700100311           ENDIF;
128800100322         reade(n) (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
128900100311         ENDDO;
129000100311       ENDIF;
129100100322
129200100322       //?Se causale prevede il n. di gg imposto la data attività
129300100326       //?e non ho già una data impostata
129400100331       //?e se non è un appuntamento
129500100924       IF  §CCOupm <> *blanks and IMK17dad = 0 and not Appuntamento;
129600100924         //?se il segno è '=' metto la data del giorno
129700100924         IF  §CCOupm = '=';
129800100924           wdata = %dec(%date());
129900100924           data_iso = %date(wdata:*iso);
130000100924           data_eur = data_iso;
130100100924           V01dad = %dec(data_eur);
130200100924         ELSE;
130300100924         //?se il segno è '-' o '+'  richiamo xgiolav
130400100924           clear XGIOLAVDS;
130500100924           IXGLdata = %dec(%date());
130600100924           IF  §CCOupm = '+';
130700100924             IXGLadd  = 'S';
130800100924           ENDIF;
130900100924           IF  §CCOupm = '-';
131000100924             IXGLsub  = 'S';
131100100924           ENDIF;
131200100924           IXGLgg   = §CCOgio;
131300100924           IXGLpa   = 'P';
131400100924           IF  V01cmm > *zeros;
131500100924             IXGLfil = %dec(%subst(V01cmm:1:3):3:0);
131600100924           ELSE;
131700100924             IXGLfil = DUTpou;
131800100924           ENDIF;
131900100924           XGIOLAV (xgiolavds);
132000100924           IF  OXGLerr = *blanks;
132100100924             data_eur = %date(OXGLdata:*iso);
132200100924             V01dad   = %dec(data_eur);
132300100924           ENDIF;
132400100924         ENDIF;
132500100322       ENDIF;
132600091221
132700091221       ENDSR;
132800091221
132900091221       //--------------------------------------------------------------
133000091223       //?Controllo videata D01.
133100091221       //--------------------------------------------------------------
133200091223       BEGSR CtrD01;
133300091221
133400091221         WindDspF = IndDspF;
133500091221         reset WindDspFb;
133600091221         IndDspF  = WindDspF;
133700100119
133800100119       //?Commerciale  -  Ricerca
133900100119         IF  %scan('?' : V01cmm) > *zero;
134000100119           ErrGenerico = *on;
134100100119           PosCurCMM   = *on;
134200130808           clear  TRMK43ds;
134300130808           iMK43fil  = DUTpou;
134400130808           TRMK43R (kpjba : TRMK43ds);
134500130808           if  oMK43err = *off  and  oMK43fxx = *blank;
134600130808             V01cmm  = %editc( oMK43cmm : 'X' );
134700130808             V01dcmm = oMK43des;
134800130808           endif;
134900100119           leavesr;
135000100119         ENDIF;
135100100119
135200100119       //?Commerciale  -  Obbligatorio
135300100119         IF  V01cmm = *blanks  or  V01cmm = *zero;
135400100119           ErrMessage  = *on;
135500100119           ErrGenerico = *on;
135600100119           PosCurCMM   = *on;
135700100119           V01msg      = $Msg(08);
135800100119           leavesr;
135900100119         ENDIF;
136000100119
136100100119         IF  %check(digits:V01cmm) > 0;
136200100119           ErrMessage  = *on;
136300100119           ErrGenerico = *on;
136400100119           PosCurCMM   = *on;
136500100119           V01msg      = $Msg(08);
136600100119           leavesr;
136700100119         ENDIF;
136800100119
136900100205       //?Commerciale NON protetto
137000100205         IF  not ProteggiCMM;
137100100310
137200100310       //?Se commerciale diverso dal precedente spengo i flag di forzatura
137300100310            IF  V01cmm <> sav_V01cmm;
137400100310              $ForzaImpegno = *off;
137500100310              $ForzaCMM     = *off;
137600100310              $ForzaInt     = *off;
137700100310            ENDIF;
137800100310
137900100119       //?Commerciale  -  Controllo
138000130808           CMMcod = %int(V01cmm);
138100130808           chain   (CMMcod)  AZCMM000;
138200130808           IF  NOT %found(AZCMM01L);
138300100205             ErrMessage  = *on;
138400100205             ErrGenerico = *on;
138500100205             PosCurCMM   = *on;
138600100205             V01msg      = $Msg(08);
138700100205             leavesr;
138800100205           ENDIF;
138900100205
139000130808           V01dcmm = CMMdes;
139100100310           sav_V01cmm = V01cmm;
139200100209
139300100209         //?commerciale valido no vari o clienti inattivi
139400130808           IF  CMMpar <> *blanks;
139500100209             ErrMessage  = *on;
139600100209             ErrGenerico = *on;
139700100209             PosCurCMM   = *on;
139800100209             V01msg      = $Msg(08);
139900100209             leavesr;
140000100209           ENDIF;
140100111123
140200111123         //?commerciale valido data inizio e fine attività
140300130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
140400111123             ErrMessage  = *on;
140500111123             ErrGenerico = *on;
140600111123             PosCurCMM   = *on;
140700111123             V01msg      = $Msg(08);
140800111123             leavesr;
140900111123           ENDIF;
141000100310
141100100310         //?controllo se attività può essere eseguita dal commerciale interno
141200130808           IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
141300100310             SELECT;
141400100310               WHEN  not $ForzaInt and §CCOcoi = 'F';
141500100310                 ErrMessage  = *on;
141600100310                 ErrGenerico = *on;
141700100310                 PosCurCMM   = *on;
141800100310                 V01msg      = $Msg(15);
141900100310                 V01msg      = %trim(V01msg) + ' Enter per forzare';
142000100310                 $ForzaInt = *on;
142100100310                 leavesr;
142200100310               WHEN  §CCOcoi = 'N';
142300100310                 ErrMessage  = *on;
142400100310                 ErrGenerico = *on;
142500100310                 PosCurCMM   = *on;
142600100310                 V01msg      = $Msg(15);
142700100310                 leavesr;
142800100310             ENDSL;
142900100310           ENDIF;
143000100205
143100100205         //?utente abilitato al commerciale
143200100208           clear TNTAA1DS;
143300120810           ITAA1tipo = 'M';
143400120810           ITAA1caut = '§utegtc';
143500100208           ITAA1cmm  = %dec(V01cmm:7:0);
143600100521           ITAA1abi = 'RA';
143700100208           kpjbu     = tntaa1ds;
143800100208           tntaa1c (kpjba);
143900100208           TNTAA1DS = kpjbu;
144000100208           IF  OTAA1fabi = 'N';
144100100205             ErrMessage  = *on;
144200100205             ErrGenerico = *on;
144300100205             PosCurCMM   = *on;
144400100208             V01msg      = $Msg(08);
144500100205             leavesr;
144600100205           ENDIF;
144700100205         ENDIF;
144800110505
144900110505         //?Se attività di blocco cliente
145000110505         IF  Blocco;
145100110505           //?Il commerciale che deve eseguire l'attività deve avere la filiale = alla
145200110505           //?filiale del cliente
145300110505           IF  %subst(V01cmm:1:3) <> %subst(%editc(V01ksc:'X'):1:3);
145400110505             ErrMessage  = *on;
145500110505             ErrGenerico = *on;
145600110505             PosCurCMM   = *on;
145700110505             V01msg      = $Msg(17);
145800110505             leavesr;
145900110505           ENDIF;
146000110505         ENDIF;
146100100202
146200100202         //?Se sto pianificando una nuova attività che apre trattativa
146300100202         //?devo controllare che non ci sia già una trattativa aperte
146400100202         //?per lo stesso potenziale e commerciale
146500100202         IF  §CCOapt = 'S' and V01nrv = 0;
146600100202           clear tnta43ds ;
146700100202           ITA43cpo = V01cpo;
146800100202           ITA43ksc = v01ksc;
146900100202           ITA43cmm = %dec(V01cmm:7:0);
147000100202           tnta43r (TNTA43DS) ;
147100100202           IF  OTA43err = '+';
147200100202             ErrMessage  = *on;
147300100202             ErrGenerico = *on;
147400100202             PosCurCmm   = *on;
147500100202             V01msg      = OTA43msg;
147600100202             leavesr;
147700100202           ENDIF;
147800100202         ENDIF;
147900091221
148000091222       //?Data attività  -  obbligatoria
148100171221         IF  V01dad <= 0;
148200091222           ErrMessage  = *on;
148300091222           ErrGenerico = *on;
148400091223           PosCurDAD   = *on;
148500091223           V01msg      = $Msg(06);
148600171222           if  V01dad  < *zeros;
148700171222             V01msg    = %trimR( V01msg ) + ' (negativa)';
148800171222           endif;
148900091222           leavesr;
149000091222         ENDIF;
149100091222
149200091222       //?Data attività  -  controllo
149300091222         clear wlbdat;
149400091223         G02dat = V01dad;
149500091222         xsrda8(wlbdat);
149600091222         IF  G02err = '1';
149700091222           ErrMessage  = *on;
149800091222           ErrGenerico = *on;
149900091223           PosCurDAD   = *on;
150000091223           V01msg      = $Msg(06);
150100091222           leavesr;
150200091222         ENDIF;
150300091222
150400091223         V01dad = G02dat;
150500091223         wdad   = G02inv;
150600100322
150700100322       //?controllo che la data non sia inferiore a 3 gg lavorativi da oggi
150800100322         clear XGIOLAVDS;
150900100322         IXGLdata = %dec(%date());
151000100322         IXGLsub  = 'S';
151100100322         IXGLgg   = 3;
151200100322         IXGLpa   = 'P';
151300100322         IXGLlav  = 'S';
151400100322         IXGLfil = %dec(%subst(V01cmm:1:3):3:0);
151500100322         XGIOLAV (xgiolavds);
151600100322         IF  wdad < OXGLdata;
151700100322           ErrMessage  = *on;
151800100322           ErrGenerico = *on;
151900100322           PosCurDAD   = *on;
152000100322           V01msg      = $Msg(06);
152100100322           V01msg      = %trim(V01msg) + ' ' +
152200100322           'Non può essere inferiore a 3 gg.lavorativi';
152300100322           leavesr;
152400100322         ENDIF;
152500100322
152600100322       //?controllo che la data non maggiore di 14 mesi da oggi
152700100322         clear XGIOLAVDS;
152800100322         IXGLdata = %dec(%date());
152900100322         IXGLadd  = 'S';
153000100322         IXGLmm   = 14;
153100100322         IXGLpa   = 'P';
153200100322         IXGLfil = %dec(%subst(V01cmm:1:3):3:0);
153300100322         XGIOLAV (xgiolavds);
153400100322         IF  wdad > OXGLdata;
153500100322           ErrMessage  = *on;
153600100322           ErrGenerico = *on;
153700100322           PosCurDAD   = *on;
153800100322           V01msg      = $Msg(06);
153900100322           V01msg      = %trim(V01msg) + ' ' +
154000100322           'Non può essere maggiore di 14 mesi';
154100100322           leavesr;
154200100322         ENDIF;
154300100119
154400100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
154500100119         IF  wdad < 20000101 or wdad > 20391231;
154600100119           ErrMessage  = *on;
154700100119           ErrGenerico = *on;
154800100119           PosCurDAD   = *on;
154900100119           V01msg      = $Msg(06);
155000100119           leavesr;
155100100119         ENDIF;
155200100119
155300091223         data_iso = (%date(wdad));
155400091223
155500091223       //?Data attività  -  no festivo
155600091223         w0030   = %dec(%subst(V01cmm:1:3):3:0);
155700091223         wanno   = %subdt(data_ISO:*years);
155800091223         wmese   = %subdt(data_ISO:*months);
155900091223         wgiorno = %subdt(data_ISO:*days);
156000100420         // exsr CtrFesta;
156100091223         IF  $Festa;
156200091223           ErrMessage  = *on;
156300091223           ErrGenerico = *on;
156400091223           PosCurDAD   = *on;
156500091228           V01msg      = $Msg(06);
156600091223           V01msg      = %trim(V01msg) + '. Immesso giorno festivo!';
156700091223           leavesr;
156800091223         ENDIF;
156900091222
157000091222       //?Ora attività  -  obbligatoria
157100171221         IF  V01hda <= 0;
157200091222           ErrMessage  = *on;
157300091222           ErrGenerico = *on;
157400091223           PosCurHDA   = *on;
157500091223           V01msg      = $Msg(07);
157600171222           if  V01hda  < *zeros;
157700171222             V01msg    = %trimR( V01msg ) + ' (negativa)';
157800171222           endif;
157900091222           leavesr;
158000091222         ENDIF;
158100091222
158200091222       //?Ora attività  -  controllo
158300120104         IF  %dec(%subst(%editc(V01hda:'X'):1:2):2:0) > 23 or
158400091223             %dec(%subst(%editc(V01hda:'X'):3:2):2:0) > 59;
158500091222           ErrMessage  = *on;
158600091222           ErrGenerico = *on;
158700091223           PosCurHDA   = *on;
158800091223           V01msg      = $Msg(07);
158900091222           leavesr;
159000091222         ENDIF;
159100091222
159200091222       //?Se APPUNTAMENTO
159300091222         IF  Appuntamento;
159400091222
159500091222         //?Verifico se il commerciale dell'appuntamento è diverso dal
159600091222         //?commerciale della trattativa
159700091222           clear tnta45ds ;
159800091222           ITA45tip = 'C';
159900091222           ITA45nrv = V01nrv;
160000091223           ITA45cmm = %dec(V01cmm:7:0);
160100091222           tnta45r (tnta45ds) ;
160200100310           IF  OTA45err = 'C' and not $ForzaCMM;
160300091222             ErrMessage  = *on;
160400091222             ErrGenerico = *on;
160500091223             PosCurCMM   = *on;
160600091222             V01msg = OTA45msg;
160700100310             V01msg = %trim(V01msg) + ' Enter per forzare';
160800100119             $ForzaCMM = *on;
160900100310             sav_V01cmm = V01cmm;
161000091222             leavesr;
161100091222           ENDIF;
161200091222
161300091222         //?Impegno  ORA inizio  -  obbligatorio
161400171221           IF  V01oii <= 0;
161500091222             ErrMessage  = *on;
161600091222             ErrGenerico = *on;
161700091223             PosCurOII   = *on;
161800091223             V01msg      = $Msg(07);
161900171222             if  V01oii  < *zeros;
162000171222               V01msg    = %trimR( V01msg ) + ' (negativa)';
162100171222             endif;
162200091222             leavesr;
162300091222           ENDIF;
162400091222
162500091222       //?Impegno  ORA inizio  -  controllo
162600120104           IF  %dec(%subst(%editc(V01oii:'X'):1:2):2:0) > 23 or
162700091223               %dec(%subst(%editc(V01oii:'X'):3:2):2:0) > 59;
162800091222             ErrMessage  = *on;
162900091222             ErrGenerico = *on;
163000091223             PosCurOII   = *on;
163100091223             V01msg      = $Msg(07);
163200091222             leavesr;
163300091222           ENDIF;
163400091222
163500091222       //?Impegno  ORA fine    -  obbligatorio
163600171221           IF  V01ofi <= 0;
163700091222             ErrMessage  = *on;
163800091222             ErrGenerico = *on;
163900091223             PosCurOFI   = *on;
164000091223             V01msg      = $Msg(07);
164100171222             if  V01ofi  < *zeros;
164200171222               V01msg    = %trimR( V01msg ) + ' (negativa)';
164300171222             endif;
164400091222             leavesr;
164500091222           ENDIF;
164600091222
164700091222       //?Impegno  ORA fine    -  controllo
164800120104           IF  %dec(%subst(%editc(V01ofi:'X'):1:2):2:0) > 23 or
164900091223               %dec(%subst(%editc(V01ofi:'X'):3:2):2:0) > 59;
165000091222             ErrMessage  = *on;
165100091222             ErrGenerico = *on;
165200091223             PosCurOFI   = *on;
165300091223             V01msg      = $Msg(07);
165400091222             leavesr;
165500091222           ENDIF;
165600091222
165700091222       //?Impegno  ORA inizio  -  congruente con ORA fine
165800091223           IF  V01oii > 0  and  V01ofi = 0;
165900091222             ErrMessage  = *on;
166000091222             ErrGenerico = *on;
166100091223             PosCurOFI   = *on;
166200091223             V01msg      = $Msg(09);
166300091222             leavesr;
166400091222           ENDIF;
166500091223           IF  V01oii > 0  and  V01ofi > 0  and  V01oii > V01ofi ;
166600091222             ErrMessage  = *on;
166700091222             ErrGenerico = *on;
166800091223             PosCurOFI   = *on;
166900091223             V01msg      = $Msg(09);
167000091222             leavesr;
167100091222           ENDIF;
167200091222
167300091222         //?Impegno  ORA inizio e fine  -  congruenti con ORA attività
167400091223           IF  V01oii > 0  and  V01ofi > 0  and
167500091223              (V01oii > V01hda  or  V01ofi < V01hda);
167600091222             ErrMessage  = *on;
167700091222             ErrGenerico = *on;
167800091223             PosCurOFI   = *on;
167900091223             V01msg      = $Msg(10);
168000091222             leavesr;
168100091222           ENDIF;
168200091222
168300091222         //?Verifico se il commerciale è libero
168400091222           clear trmk84ds;
168500091223           IMK84cmm = %dec(V01cmm:7:0);
168600091223           IMK84dad = wdad;
168700091223           IMK84oii = V01oii;
168800091223           IMK84ofi = V01ofi;
168900091222           trmk84r (trmk84ds);
169000091222         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
169100091222           IF  OMK84err <> *blanks and OMK84err <> '9' and
169200091222               Not $ForzaImpegno;
169300091222             ErrGenerico = *on;
169400091222             ErrMessage  = *on;
169500091223             PosCurCMM   = *on;
169600091223             V01msg = OMK84msg;
169700091229             V01msg = %trim(V01msg) + '. Enter per forzare.';
169800091222             $ForzaImpegno = *on;
169900091222             leavesr;
170000091222           ENDIF;
170100091222
170200091222         ENDIF;
170300100125
170400100125       //?Commerciale che inserisce attività -  Ricerca
170500100125         IF  %scan('?' : V01co3) > *zero;
170600100125           ErrGenerico = *on;
170700100125           PosCurCO3   = *on;
170800130808           clear  TRMK43ds;
170900130808           iMK43fil  = DUTpou;
171000130808           TRMK43R (kpjba : TRMK43ds);
171100130808           if  oMK43err = *off  and  oMK43fxx = *blank;
171200130808             V01co3  = %editc( oMK43cmm : 'X' );
171300130808             V01dco3 = oMK43des;
171400130808           endif;
171500100125           leavesr;
171600100125         ENDIF;
171700100125
171800100125       //?Commerciale che inserisce attività  -  Obbligatorio
171900100125         IF  V01co3 = *blanks  or  V01co3 = *zero;
172000100125           ErrMessage  = *on;
172100100125           ErrGenerico = *on;
172200100125           PosCurCO3   = *on;
172300100125           V01msg      = $Msg(08);
172400100125           leavesr;
172500100125         ENDIF;
172600100125
172700100125         IF  %check(digits:V01co3) > 0;
172800100125           ErrMessage  = *on;
172900100125           ErrGenerico = *on;
173000100315           PosCurCO3   = *on;
173100100125           V01msg      = $Msg(08);
173200100125           leavesr;
173300100125         ENDIF;
173400091222
173500091222       //?Commerciale che inserisce attività  -  Controllo
173600130808         CMMcod = %int(V01co3);
173700130808         chain  (CMMcod)  AZCMM000;
173800130808         IF  NOT %found(AZCMM01L);
173900091222           ErrMessage  = *on;
174000091222           ErrGenerico = *on;
174100091223           PosCurCO3   = *on;
174200091223           V01msg      = $Msg(08);
174300091222           leavesr;
174400091222         ENDIF;
174500091222
174600130808         V01dco3 = CMMdes;
174700100209
174800100209         //?commerciale valido no vari o clienti inattivi
174900130808           IF  CMMpar <> *blanks;
175000100209             ErrMessage  = *on;
175100100209             ErrGenerico = *on;
175200100209             PosCurCO3   = *on;
175300100209             V01msg      = $Msg(08);
175400100209             leavesr;
175500100209           ENDIF;
175600111123
175700111123         //?commerciale valido data inizio e fine attività
175800130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
175900111123             ErrMessage  = *on;
176000111123             ErrGenerico = *on;
176100111123             PosCurCO3   = *on;
176200111123             V01msg      = $Msg(08);
176300111123             leavesr;
176400111123           ENDIF;
176500100205
176600100205         //?utente abilitato al commerciale
176700100208         clear TNTAA1DS;
176800120810         ITAA1tipo = 'M';
176900120810         ITAA1caut = '§utegtc';
177000100208         ITAA1cmm  = %dec(V01co3:7:0);
177100100521         ITAA1abi = 'RA';
177200100208         kpjbu     = tntaa1ds;
177300100208         tntaa1c (kpjba);
177400100208         TNTAA1DS = kpjbu;
177500100208         IF  OTAA1fabi = 'N';
177600100205           ErrMessage  = *on;
177700100205           ErrGenerico = *on;
177800100205           PosCurCO3   = *on;
177900100208           V01msg      = $Msg(08);
178000100205           leavesr;
178100100205         ENDIF;
178200110307
178300110307         //?Se blocco cliente
178400110307         IF  Blocco;
178500110323           clear dsBI;
178600110307           //?Richiedo causale blocco obbligatoria
178700110307           IF  V01blc = *blanks;
178800110307             ErrMessage  = *on;
178900110307             ErrGenerico = *on;
179000110307             PosCurBlc   = *on;
179100110307             V01msg      = $Msg(25);
179200110307             leavesr;
179300110307           ENDIF;
179400110307           //?Ricerca causale blocco obbligatoria
179500110307           IF  %scan('?':V01blc) > 0;
179600110307             ErrGenerico = *on;
179700110307             PosCurBlc   = *on;
179800121127             //idTabella = 'BI';
179900121127             //clear Ordinamento;
180000121127             //clear idElemento;
180100121127             //clear TastoUscita;
180200121127             //Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
180300121127             //V01blc = idElemento;
180400121127             clear kpjbu;
180500121127             TRTBBIR (kpjba);
180600121127             V01blc = %subst(kpjbu:1:3);
180700110307           ENDIF;
180800110307           IF  V01blc <> *blanks and V01blc <> *zeros ;
180900110307             //?Ammessi solo numeri
181000110307             IF  %check(digits:V01blc) > 0;
181100110307               ErrMessage  = *on;
181200110307               ErrGenerico = *on;
181300110307               PosCurBlc   = *on;
181400110307               V01msg      = $Msg(25);
181500110307               leavesr;
181600110307             ENDIF;
181700110307             //?Controllo validità
181800110307             k_TBLcod = 'BI';
181900110307             k_TBLkey = V01blc;
182000110307             chain %kds(K03tabel) TABEL00F;
182100110307             IF  not %found(TABEL00F)
182200110307                 or  TBLflg <> *blanks;
182300110307               ErrMessage  = *on;
182400110307               ErrGenerico = *on;
182500110307               PosCurBlc   = *on;
182600110307               V01msg      = $Msg(25);
182700110307               leavesr;
182800110307             ENDIF;
182900110323             dsBI = TBLuni;
183000110323             V01dblc = §BIdes;
183100121127             //?Controllo se causale ancora utilizzabile
183200121127             IF  §BIuso = 'N';
183300121127               ErrMessage  = *on;
183400121127               ErrGenerico = *on;
183500121127               PosCurBlc   = *on;
183600121127               V01msg      = 'Causale non più utilizzabile';
183700121127               leavesr;
183800121127             ENDIF;
183900110307           ENDIF;
184000110307         ENDIF;
184100100311
184200100311         //?richiedo note obbligatorie se la causale attività lo prevede
184300100311         IF  V01nota1 = *blanks and V01nota2 = *blanks and §CCOnot = 'S';
184400110330           PosCurBlc   = *off;
184500100311           ErrMessage  = *on;
184600100311           ErrGenerico = *on;
184700100311           PosCurNO1   = *on;
184800100311           V01msg      = $Msg(19);
184900100311           leavesr;
185000100311         ENDIF;
185100100331
185200100331       //?Se F08 = Conferma + Mail appuntamento
185300100331       //?devo controllare che sia memorizzato il responsabile trasporti e l'indirizzo mail
185400100331       //?del responsabile trasporti
185500100331           IF  dsp_aid = c_F08;
185600100331             exsr CtrRespTra;
185700100331           ENDIF;
185800091221
185900091221       ENDSR;
186000100331
186100100331       //--------------------------------------------------------------
186200100331       //?Controllo responsabile trasporti.
186300100331       //--------------------------------------------------------------
186400100331       BEGSR CtrResptra;
186500100331
186600100331         clear NTCapl;
186700100331         clear NTCnk1;
186800100331         clear NTCnk2;
186900100331         clear wrst;
187000100331         clear wemr;
187100100331
187200100331         SELECT;
187300100331
187400100331       //?Se c'è il cliente recupero la rubrica del cliente
187500100331           WHEN  V01ksc > 0;
187600100331             NTCapl = 'C';
187700100331             NTCnk1 = %editc(DUTkci:'X') + %editc(V01ksc:'X');
187800100331       //?Se c'è la trattativa recupero la rubrica della trattativa
187900100331           WHEN  V01nrv > 0;
188000100331             NTCapl = 'T';
188100100331             NTCnk1 = %editc(DUTkci:'X') + %editc(V01nrv:'X');
188200100331       //?Altrimenti recupero la rubrica del potenziale
188300100331           OTHER;
188400100331             NTCapl = 'P';
188500100331             NTCnk1 = %editc(V01cpo:'X');
188600100331         ENDSL;
188700100331
188800100331       //?Nominativo responsabile trasporti
188900100331         NTCtnt = '05';
189000100331         chain (NTCapl:NTCnk1:Ntcnk2:Ntctnt) TFNTC01L;
189100100331         IF  not %found(TFNTC01L);
189200100331           ErrMessage  = *on;
189300100331           ErrGenerico = *on;
189400100331           V01msg      = $Msg(23);
189500100331           leavesr;
189600100331         ENDIF;
189700100331         wrst = NTCrnt;
189800100331
189900100331       //?E-mail responsabile trasporti
190000100331         NTCtnt = '06';
190100100331         chain (NTCapl:NTCnk1:Ntcnk2:Ntctnt) TFNTC01L;
190200100331         IF  not %found(TFNTC01L);
190300100331           ErrMessage  = *on;
190400100331           ErrGenerico = *on;
190500100331           V01msg      = $Msg(23);
190600100331           leavesr;
190700100331         ENDIF;
190800100331         wemr = NTCrnt;
190900100331
191000100331       ENDSR;
191100100927
191200100927       //--------------------------------------------------------------
191300100927       //?Gestione tasto funzionale F02 da videata D01.
191400100927       //?F02=Rubrica
191500100927       //--------------------------------------------------------------
191600100927       BEGSR F02D01;
191700100927
191800100927         clear TNTA12DS;
191900100927
192000100927         SELECT;
192100100927           WHEN  V01ksc > 0;
192200100927             TA12apl = 'C';
192300100927             TA12ksc = V01ksc;
192400100927             TA12rag = V01drag;
192500100927           WHEN  V01nrv > 0;
192600100927             TA12apl = 'T';
192700100927             TA12nrv = V01nrv;
192800100927           OTHER;
192900100927             TA12apl = 'P';
193000100927             TA12pot = V01cpo;
193100100927             TA12rag = V01dcpo;
193200100927         ENDSL;
193300100927         tnta12R (kpjba:TNTA12ds);
193400100927         IF  TA12err <> ' ';
193500100927           ErrGenerico = *on;
193600100927           ErrMessage  = *on;
193700100927           V01msg      = ta12msg ;
193800100927           leavesr;
193900100927         ENDIF;
194000100927
194100100927       ENDSR;
194200091222
194300091222       //--------------------------------------------------------------
194400091223       //?Gestione tasto funzionale F06 da videata D01.
194500091222       //?F06=Conferma
194600091222       //--------------------------------------------------------------
194700091223       BEGSR F06D01;
194800091223
194900091223       //?Stacco nuovo numero attività
195000091223         exsr NewAttivita;
195100091223       //?se errore vado subito a fine
195200091223         IF  O33Err <> 0;
195300091223           $Fine = *on;
195400091223           ErrGenerico = *on;
195500091223           V01msg = O33msg;
195600091223           leavesr;
195700091223         ENDIF;
195800091223
195900091223       //?Scrivo nuova attività
196000110223         clear dATC01;
196100091223         clear tiatc000;
196200091223         atctat  = V01att;
196300091223         atcatn  = O33Nrf;
196400091223         atcatnp = 1;
196500091223         atccpo  = IMK17cpo;
196600091223         atcksc  = IMK17ksc;
196700110503         atcprg  = V01blc;
196800091223         atcnrv  = IMK17nrv;
196900091223         atccad  = V01cau;
197000091223         atcdad  = wdad;
197100091223         atchda  = V01hda * 100;
197200091223         atccmm  = %dec(V01cmm:7:0);
197300091223         atcoii  = V01oii;
197400091223         atcofi  = V01ofi;
197500091223         atcco3  = %dec(V01co3:7:0);
197600091223         atcdim  = %dec(%date());
197700091223         atchim  = %dec(%time());
197800091223         atcpri  = knmus ;
197900091223
198000100301       //?Verifico se attività CREATA su codificato o potenziale
198100110502         IF  CPOfls = 'C';
198200110502           atccnw = 'C';
198300110502         ELSE;
198400110502           atccnw = 'P';
198500110502         ENDIF;
198600110223
198700110223       //?Memorizzo la categoria del potenziale all'apertura dell'attività
198800110223         §ATCcapo1 = CPOfls;
198900110223         ATCflo = dATC01;
199000091223
199100091223         write TIATC000;
199200091223         feod  TIATC01L;
199300110307
199400110307       //?Scrivo la nota con la causale blocco cliente
199500110314       //?più eventuale nota immessa a video, solo queste perchè ho bloccato F18
199600110314       //?in caso di causale 71 blocco cliente
199700110307         IF  Blocco;
199800110307           clear TRMK20ds;
199900110307           IMK20tla  = 'L';
200000110307           IMK20flm  = 'C';
200100110307           IMK20fno  = 'S';
200200110307           IMK20cpo  = atccpo;
200300110307           IMK20ksc  = atcksc;
200400110307           IMK20nrv  = atcnrv;
200500110307           IMK20tat  = atctat;
200600110307           IMK20atn  = atcatn;
200700110307           IMK20atnp = atcatnp - 1;
200800110307           EMK20no1 = 'Bloccare il cliente con la causale: ' +
200900110407                       V01blc + ' - ' + V01dblc;
201000110314           EMK20no2 = V01nota1 + V01nota2;
201100110307           trmk20r (kpjba : TRMK20ds);
201200110314         ELSE;
201300091223
201400091223       //?Se ci sono note le registro
201500100607         IF  V01nota1 <> *blanks or V01nota2 <> *blanks
201600100607             or V01notah <> *blanks;
201700100311           clear TRMK20ds;
201800100311           IMK20tla  = 'L';
201900100311           IMK20flm  = 'C';
202000100929           IMK20fno  = 'S';
202100100311           IMK20cpo  = atccpo;
202200100311           IMK20ksc  = atcksc;
202300100311           IMK20nrv  = atcnrv;
202400100311           IMK20tat  = atctat;
202500100311           IMK20atn  = atcatn;
202600100311           IMK20atnp = atcatnp - 1;
202700100311           EMK20no1 = V01nota1 + V01nota2;
202800100607           EMK20no2 = V01notah;
202900100311           trmk20r (kpjba : TRMK20ds);
203000100311         ENDIF;
203100110314         ENDIF;
203200091223
203300091223       //?Se APPUNTAMENTO controllo se devo richiamare gestione affiancamento
203400091223         IF  Appuntamento and V01aff = 'S';
203500091223           exsr Ric_Affianca;
203600091223           //?se errore o F12 devo tornare all'appuntamento
203700091223           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
203800091223             leavesr;
203900091223           ENDIF;
204000091223         ENDIF;
204100110412
204200110412       //?Se potenziale in categoria "E" --> Eliminabile
204300110412       //?ricalcolo la categoria del potenziale e se variata l'aggiorno
204400110412         IF  CPOfls = 'E';
204500110412           clear trmk05ds;
204600110412           IMK05cpo = CPOcpo;
204700110412           trmk05r (kpjba : TRMK05DS);
204800110412         //?se categoria variata aggiorno potenziale
204900110412           IF  OMK05err = *blanks and OMK05cat <> CPOfls;
205000110412             chain(e) CPOcpo TNCPO01L;
205100110412             IF  not %error and %found(TNCPO01L);
205200110412               CPOfls = OMK05cat;
205300110412               update TNCPO000;
205400110412             ENDIF;
205500110412           ENDIF;
205600110412         ENDIF;
205700100311
205800100311       //?Se richiamto da stampa trattativa e ci sono attività 80 aperte
205900100311       //?Window di richiesta per annullo attività 80 aperte
206000100315         IF  IMK17stp = 'S' and  $Att80;
206100100311           $InzW01 = *on;
206200100311           $Video  = 'W1';
206300100311           leavesr;
206400100311         ENDIF;
206500110223
206600110223       //?Se attività che lo richiede stampo lettera
206700110407       //?e se a video è stata richiesta la stampa
206800110412         IF  §CCOstl = 'S' and V01stp = 'S';
206900110315           clear TNTA53ds;
207000110315           DTA53ksc = ATCksc;
207100110412           DTA53fil = DUTpou;
207200110315           tnta53r (kpjba:tnta53ds);
207300110223         ENDIF;
207400091222
207500091223       //?Ritorno al programma chiamante
207600100331         IF  dsp_aid = c_F06;
207700100331           $Fine = *on ;
207800100331         ENDIF;
207900091222
208000091222       ENDSR;
208100100331
208200100331       //--------------------------------------------------------------
208300100331       //?Gestione tasto funzionale F08 da videata D01.
208400100331       //?F08=Conferma
208500100331       //--------------------------------------------------------------
208600100331       BEGSR F08D01;
208700100331
208800100331       //?Confermo i dati dell'appuntamento
208900100331         exsr F06D01;
209000100916
209100100916       //?Richiamo pgm per richiesta dati mail + invio mail
209200100916         clear TRMK23ds1;
209300100916         IMK23f12 = 'S';
209400100916         IMK23cma = ATCco3;
209500100916         IMK23cmm = ATCcmm;
209600100916         IMK23dta = ATCdad;
209700100916         IMK23ora = %dec(%subst(%editc(ATChda:'X'):1:4):4:0);
209800100916         IMK23rst = wrst;
209900100916         IMK23emr = wemr;
210000100916         trmk23r1 (kpjba : TRMK23ds1);
210100100331
210200100331       //?Ritorno al programma chiamante
210300100331         $Fine = *on ;
210400100331
210500100331       ENDSR;
210600091222
210700091222       //--------------------------------------------------------------
210800091223       //?Gestione tasto funzionale F11 da videata D01.
210900091222       //?F11=Agenda
211000091222       //--------------------------------------------------------------
211100091223       BEGSR F11D01;
211200091222
211300091229         clear TRMK82ds;
211400100112         IMK82rich = 'A';
211500100929         IF  V01cmm <> *blanks and V02cmm > *zeros;
211600100929           IMK82cage = %dec(V01cmm:7:0);
211700100929         ENDIF;
211800091222         kpjbu = trmk82ds;
211900091222         trmk82r (kpjba);
212000091222
212100091222       ENDSR;
212200091221
212300091221       //--------------------------------------------------------------
212400091223       //?Gestione tasto funzionale F12 da videata D01.
212500091221       //?F12=Ritorno
212600091221       //--------------------------------------------------------------
212700091223       BEGSR F12D01;
212800100114
212900100114       //?Ritorno al programma chiamante se tipo attività e causale protetti
213000100114         IF  IMK17patt = 'S' and IMK17pcau = 'S';
213100100203           OMK17f12 = 'S';
213200100114           $Fine  = *on ;
213300100114           leavesr;
213400100114         ENDIF;
213500091221
213600100108         $Video = 'T1';
213700100108         $InzT01 = *on;
213800091221
213900091221       ENDSR;
214000091221
214100091221       //--------------------------------------------------------------
214200091223       //?Gestione tasto funzionale F18 da videata D01.
214300091221       //?F18=Note
214400091221       //--------------------------------------------------------------
214500091223       BEGSR F18D01;
214600091221
214700100114       //?Richiamo il pgm gestione note TRMK20R
214800091221         clear TRMK20ds ;
214900091221         IMK20flm = 'M';
215000091221         IMK20cpo = IMK17cpo ;
215100091221         IMK20ksc = IMK17ksc ;
215200091221         IMK20nrv = IMK17nrv ;
215300100311         EMK20no1 = V01nota1 + V01nota2;
215400100607         EMK20no2 = V01notah;
215500091221         trmk20r ( kpjba : trmk20ds );
215600100311         V01nota1 = %subst(EMK20no1:1:45);
215700100311         V01nota2 = %subst(EMK20no1:46:45);
215800100607         V01notah = EMK20no2;
215900091221
216000091221       ENDSR;
216100100311
216200100311       //--------------------------------------------------------------
216300100311       //?Gestione videata W01 - Richiesta annullo attività 80.
216400100311       //--------------------------------------------------------------
216500100311       BEGSR GesW01;
216600100311
216700100311         // Inizializzazione videata
216800100311         if $InzW01 = *on;
216900100311           exsr InzW01;
217000100311           $InzW01 = *off;
217100100311         endif;
217200100311
217300100311       //?Emissione videata
217400100311         exfmt MK17W01;
217500100311         errMessage   = *off;
217600100311         errGenerico  = *off;
217700100311         clear W01msg;
217800100311
217900100311         SELECT;
218000100311
218100100311       //?F06 = Conferma
218200100311           WHEN dsp_aid = c_F06;
218300100311             exsr F06W01;
218400100311             leavesr ;
218500100311
218600100311       //?Enter
218700100311           OTHER;
218800100311       //?Controllo i dati
218900100311             exsr CtrW01;
219000100311             IF  ErrGenerico;
219100100311               leavesr;
219200100311             ENDIF;
219300100311         ENDSL;
219400100311
219500100311       ENDSR;
219600100311
219700100311       //--------------------------------------------------------------
219800100311       //?Caricamento videata W01.
219900100311       //--------------------------------------------------------------
220000100311       BEGSR InzW01;
220100100311
220200100311         clear MK17W01;
220300100311
220400100311       //?Causale 80 + decodifica
220500100311         clear TIBS02ds;
220600100311         clear dCCO;
220700100311         T02mod = 'C';
220800100311         T02cod = 'CCO';
220900100311         T02ke1 = '80';
221000100311         T02sif = KNSIF;
221100100311         TNTBE_RicercaControllo (kpjba : tibs02ds);
221200100311         IF  T02err = *blanks;
221300100311           dCCO = T02uni;
221400100311         ENDIF;
221500100311         W01att = '80 - ' + §CCOdes;
221600100311         W01att = %trim(W01att) + ' aperte ?';
221700100524         W01SINO = ' ';
221800100311
221900100311       ENDSR;
222000100311
222100100311       //--------------------------------------------------------------
222200100311       //?Controllo videata W01.
222300100311       //--------------------------------------------------------------
222400100311       BEGSR CtrW01;
222500100311
222600100311         WindDspF = IndDspF;
222700100311         reset WindDspFb;
222800100311         IndDspF  = WindDspF;
222900100311
223000100311       //?SI/NO
223100100311         IF  W01sino = *blanks;
223200100311           ErrMessage  = *on;
223300100311           ErrGenerico = *on;
223400100311           W01msg      = 'Effettuare una scelta';
223500100311           leavesr;
223600100311         ENDIF;
223700100311
223800100311       ENDSR;
223900100311
224000100311       //--------------------------------------------------------------
224100100311       //?Gestione tasto funzionale F06 da videata W01.
224200100311       //?F06=Conferma
224300100311       //--------------------------------------------------------------
224400100311       BEGSR F06W01;
224500100311
224600100311       //?E' stato scelto di annullare le attività 80 aperte
224700100524         IF  W01sino = 'S';
224800100322           setll (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
224900100322           reade (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
225000100311           DOW not %eof(TIATC04L);
225100100311             IF  ATCcad = '80';
225200100311       //?Se attività 80 con progressivo 1 devo cancellare anche le relative note
225300100311               IF  ATCatnp = 1;
225400100311                 exsr sr_DelNote80;
225500100311               ENDIF;
225600100311               delete TIATC04;
225700100311             ELSE;
225800100311               update TIATC04;
225900100311             ENDIF;
226000100322             reade (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
226100100311           ENDDO;
226200100311         ENDIF;
226300100311
226400100311       //?Ritorno al programma chiamante
226500100315         $Fine = *on ;
226600100311
226700100311       ENDSR;
226800100311
226900100311       //--------------------------------------------------------------
227000100311       //?Cancello note relative ad attività 80 progressivo 1.
227100100311       //--------------------------------------------------------------
227200100311       BEGSR sr_DelNote80;
227300100311
227400100311         CPNcpo  = ATCcpo;
227500100311         CPNtat  = ATCtat;
227600100311         CPNatn  = ATCatn;
227700100311         CPNatnp = 0;
227800100311         setll (cpncpo:cpntat:cpnatn:cpnatnp) TICPN03L;
227900100311         reade (cpncpo:cpntat:cpnatn:cpnatnp) TICPN03L;
228000100311         DOW not %eof(TICPN03L);
228100100311           delete TICPN000;
228200100311           reade (cpncpo:cpntat:cpnatn:cpnatnp) TICPN03L;
228300100311         ENDDO;
228400100311
228500100311       ENDSR;
228600091223
228700091223       //--------------------------------------------------------------
228800091223       //?Gestione videata T02 - Immissione dati attività ufficio.
228900091223       //--------------------------------------------------------------
229000091223       BEGSR GesT02;
229100091223
229200091223         // Inizializzazione videata
229300091223         if $InzT02 = *on;
229400091223           exsr InzT02;
229500091223           $InzT02 = *off;
229600091223         endif;
229700091223
229800091229       //?Emissione videata
229900091223         exfmt MK17T02;
230000091223         errMessage   = *off;
230100091223         errGenerico  = *off;
230200091223         clear V02msg;
230300091228
230400091228         SELECT;
230500091228
230600091228       //?F12 = Ritorno
230700091228           WHEN dsp_aid = c_F12;
230800100112             exsr F12T01;
230900091228             leavesr ;
231000091223
231100091223       //?Enter
231200091228           OTHER;
231300091223       //?Controllo i dati
231400091228             exsr CtrT02;
231500091228             IF  ErrGenerico;
231600091228               leavesr;
231700091228             ENDIF;
231800091228         ENDSL;
231900091223
232000091223       //?Emetto la videata sucessiva in base al tipo attività
232100091223         SELECT;
232200091223       //?parziale in giornata
232300091229           WHEN  V02cau = 'FP' or V02cau = 'UF';
232400091229             $Video = 'DP';
232500091229             $InzDP2 = *on;
232600091223       //?totale su più giorni
232700091229           WHEN  V02cau = 'FT';
232800091223             $Video = 'DT';
232900091223             $InzDT2 = *on;
233000091229       //?Riunione commerciale in giornata con 1 o più commerciali
233100091223           OTHER;
233200091223             $Video = 'C2';
233300091223             $InzC02 = *on;
233400091223         ENDSL;
233500091223
233600091223       ENDSR;
233700091223
233800091223       //--------------------------------------------------------------
233900091223       //?Caricamento videata T02.
234000091223       //--------------------------------------------------------------
234100091223       BEGSR InzT02;
234200091223
234300091223         clear MK17T02;
234400091223         V02pgm = SDSpgm;
234500091228         V02sut = rsut;
234600091228         V02sif = knsif;
234700091228         V02mus = knmus;
234800091223
234900091223       //?Tipo attività
235000091223         IF  IMK17att <> *blanks;
235100100113           V02att = IMK17att;
235200100113           ProteggiATT = (IMK17patt = 'S');
235300091223           clear TIBS02ds;
235400091223           clear dTTA;
235500091223           T02mod = 'C';
235600091223           T02cod = 'TTA';
235700091223           T02ke1 = V02att;
235800091223           T02sif = KNSIF;
235900091223           TNTBE_RicercaControllo (kpjba : tibs02ds);
236000091223           IF  T02err = *blanks;
236100091223             dTTA = T02uni;
236200091223           ENDIF;
236300100113           V02datt = §TTAdes;
236400091223         ENDIF;
236500091223
236600091223       //?Causale
236700091223         IF  IMK17cau <> *blanks;
236800091223           V02cau = IMK17cau;
236900091223           ProteggiCAU = (IMK17pcau = 'S');
237000091223           clear TIBS02ds;
237100091223           clear dCCO;
237200091223           T02mod = 'C';
237300091223           T02cod = 'CCO';
237400091223           T02ke1 = V02cau;
237500091223           T02sif = KNSIF;
237600091223           TNTBE_RicercaControllo (kpjba : tibs02ds);
237700091223           IF  T02err = *blanks;
237800091223             dCCO = T02uni;
237900091223           ENDIF;
238000091223           V02dcau = §CCOdes;
238100091223         ENDIF;
238200091223
238300091223       ENDSR;
238400091223
238500091223       //--------------------------------------------------------------
238600091223       //?Controllo videata T02.
238700091223       //--------------------------------------------------------------
238800091223       BEGSR CtrT02;
238900091223
239000091223         WindDspF = IndDspF;
239100091223         reset WindDspFb;
239200091223         IndDspF  = WindDspF;
239300100113
239400100113       //?Tipo attività
239500100113         IF  not ProteggiATT;
239600100113           exsr CtrATTT02;
239700100113           IF  ErrGenerico;
239800100113             leavesr;
239900100113           ENDIF;
240000100113         ENDIF;
240100100113
240200100113       //?Causale
240300100113         IF  not ProteggiCAU;
240400100113           exsr CtrCAUT02;
240500100113           IF  ErrGenerico;
240600100113             leavesr;
240700100113           ENDIF;
240800100113         ENDIF;
240900100113
241000100113       ENDSR;
241100100113
241200100113       //--------------------------------------------------------------
241300100113       //?Controllo tipo attività T02.
241400100113       //--------------------------------------------------------------
241500100113       BEGSR CtrATTT02;
241600091223
241700091223       //?Tipo attività  -  Ricerca
241800091223         IF  %scan('?' : V02att) > *zero;
241900091223           ErrGenerico = *on;
242000091223           PosCurATT   = *on;
242100091223           clear dTTA;
242200100504           clear TNTB81DS;
242300100504           ITTAleg = 'N';
242400100504           tntb81R1 (kpjba : TNTB81ds);
242500100504           IF  OTTAerr = '1';
242600100504             ErrMessage  = *on;
242700100504             V02msg = T02msg;
242800100504             leavesr;
242900100504           ENDIF;
243000100504
243100100504             V02att  = OTTAke1;
243200100504             dTTA    = OTTAuni;
243300100504             V02datt = §TTAdes;
243400091223         ENDIF;
243500091223
243600091223       //?Tipo attività  -  Obbligatoria
243700091223         IF  V02att = *blanks ;
243800091223           ErrMessage  = *on;
243900091223           ErrGenerico = *on;
244000091223           PosCurATT   = *on;
244100091223           V02msg      = $Msg(04);
244200091223           leavesr;
244300091223         ENDIF;
244400091223
244500091223       //?Tipo attività  -  Controllo
244600091223         clear TIBS02ds;
244700091223         clear dTTA;
244800091223         T02mod = 'C';
244900091223         T02cod = 'TTA';
245000091223         T02ke1 = V02att;
245100091223         T02sif = KNSIF;
245200091223         TNTBE_RicercaControllo  (kpjba : tibs02ds);
245300091223         IF  T02err = *blanks;
245400091223           dTTA = t02uni  ;
245500091223         ELSE;
245600091223           ErrMessage  = *on;
245700091223           ErrGenerico = *on;
245800091223           PosCurATT   = *on;
245900091223           V02msg      = $Msg(04);
246000091223           leavesr;
246100091223         ENDIF;
246200100113         V02datt = §TTAdes;
246300091223
246400091223       //?Tipo attività  -  Accetto solo attività d'ufficio
246500091223       //?                  da questa videata
246600091223         IF  §TTAleg = 'S';
246700091223           ErrMessage  = *on;
246800091223           ErrGenerico = *on;
246900091223           PosCurATT   = *on;
247000091223           V02msg      = $Msg(04);
247100091228           V02msg      = %trim(V02msg) +
247200091228                         '. Inserita attività legata a cliente.';
247300091223           leavesr;
247400091223         ENDIF;
247500100113
247600100113       ENDSR;
247700100113
247800100113       //--------------------------------------------------------------
247900100113       //?Controllo causale T02.
248000100113       //--------------------------------------------------------------
248100100113       BEGSR CtrCAUT02;
248200091223
248300091223       //?Causale  -  Ricerca
248400091223         IF  %scan('?' : V02cau) > *zero;
248500091223           ErrGenerico = *on;
248600091223           PosCurCAU   = *on;
248700091223           clear dCCO;
248800091223           clear TNTB74ds;
248900091223           ICCOatt = V02att;
249000091223           ICCOuti = 'S';
249100091223           tntb74R  (kpjba : TNTB74ds);
249200091223           IF  OCCOerr = *off;
249300091223             V02cau = OCCOke1;
249400091223             dCCO   = OCCOuni;
249500091223             leavesr;
249600091223           ELSE;
249700091223             ErrMessage  = *on;
249800091223             V02msg = T02msg;
249900091223             leavesr;
250000091223           ENDIF;
250100091223
250200091223           V02dcau = §ccodes;
250300091223         ENDIF;
250400091223
250500091223       //?Causale  -  Obbligatoria
250600091223         IF  V02cau = *blanks ;
250700091223           ErrMessage  = *on;
250800091223           ErrGenerico = *on;
250900091223           PosCurCAU   = *on;
251000091223           V02msg      = $Msg(05);
251100091223           leavesr;
251200091223         ENDIF;
251300091223
251400091223       //?Causale  -  Controllo
251500091223         clear TIBS02ds;
251600091223         clear dCCO;
251700091223         T02mod = 'C';
251800091223         T02cod = 'CCO';
251900091223         T02ke1 = V02cau;
252000091223         T02sif = KNSIF;
252100091223         TNTBE_RicercaControllo (kpjba : tibs02ds);
252200091223         IF  T02err = *blanks;
252300091223           dCCO = t02uni  ;
252400091223         ELSE;
252500091223           ErrMessage  = *on;
252600091223           ErrGenerico = *on;
252700091223           PosCurCAU   = *on;
252800091223           V02msg      = $Msg(05);
252900091223           leavesr;
253000091223         ENDIF;
253100091223
253200091223         V02dcau = §ccodes;
253300091223
253400091223       //?Causale  -  Controllo se in uso agli utenti
253500091223         IF  §CCOuti = 'N';
253600091223           ErrMessage  = *on;
253700091223           ErrGenerico = *on;
253800091223           PosCurCAU   = *on;
253900091223           V02msg      = $Msg(05);
254000091223           leavesr;
254100091223         ENDIF;
254200091223
254300091223       //?Causale  -  Controllo se compatibile con tipo attività
254400091223         IF  §CCOatt <> V02att;
254500091223           ErrMessage  = *on;
254600091223           ErrGenerico = *on;
254700091223           PosCurCAU   = *on;
254800091223           V02msg      = $Msg(05);
254900091223           leavesr;
255000091223         ENDIF;
255100091223
255200091223       ENDSR;
255300091223
255400091223       //--------------------------------------------------------------
255500091223       //?Gestione videata DP2.
255600091223       //--------------------------------------------------------------
255700091223       BEGSR GesDP2;
255800091223
255900091223         // Inizializzazione videata
256000091223         if $InzDP2 = *on;
256100091223           exsr InzDP2;
256200091223           $InzDP2 = *off;
256300091223         endif;
256400091223
256500091229       //?Emissione videata
256600091223         exfmt MK17DP2;
256700091223         errMessage   = *off;
256800091223         errGenerico  = *off;
256900091223         clear V02msg;
257000091223
257100091223         SELECT;
257200091223
257300091223       //?F12 = Ritorno
257400091223           WHEN dsp_aid = c_F12;
257500100112             exsr F12D02;
257600091223             leavesr ;
257700091223
257800091223       //?F11 = Agenda
257900091223           WHEN dsp_aid = c_F11;
258000091229             exsr F11D02;
258100091223             leavesr ;
258200091223
258300091223       //?F18 = Note
258400091223           WHEN dsp_aid = c_F18;
258500091223             exsr F18D01;
258600091223             leavesr ;
258700091223
258800091223       //?F06 = Conferma
258900091223           WHEN dsp_aid = c_F06;
259000091223             //?prima faccio i controlli
259100091223             exsr CtrDP2;
259200091223             IF  ErrGenerico;
259300091223               leavesr;
259400091223             ENDIF;
259500091223             //?tutto ok confermo i dati
259600091223             exsr F06DP2;
259700091223             leavesr ;
259800091223
259900091223       //?Enter
260000091223           OTHER;
260100091223       //?Controllo i dati
260200091223            exsr CtrDP2;
260300091223            IF  ErrGenerico;
260400091223              leavesr;
260500091223            ENDIF;
260600091223
260700091223         ENDSL;
260800091223
260900091223       ENDSR;
261000091223
261100091223       //--------------------------------------------------------------
261200091223       //?Caricamento videata DP2.
261300091223       //--------------------------------------------------------------
261400091223       BEGSR InzDP2;
261500091223
261600091229         clear V02cmm;
261700091229         clear v02dcmm;
261800091229         clear v02dad;
261900091229         clear v02oii;
262000091229         clear v02ofi;
262100091223
262200091223         $ForzaImpegno = *off;
262300100310         $ForzaInt     = *off;
262400100310         clear sav_V01cmm;
262500091223
262600100205       //?Imposto commerciale se passato
262700100208         IF  IMK17cmm > 0 and IMK17pcmm = *blanks;
262800100208       //?ma se non è richiesta la protezione e l'utente non è abilitato al
262900100208       //?commerciale non propongo il dato
263000100208           clear TNTAA1DS;
263100120810           ITAA1tipo = 'M';
263200120810           ITAA1caut = '§utegtc';
263300100208           ITAA1cmm  = IMK17cmm;
263400100521           ITAA1abi = 'RA';
263500100208           kpjbu     = tntaa1ds;
263600100208           tntaa1c (kpjba);
263700100208           TNTAA1DS = kpjbu;
263800100208           IF  OTAA1fabi = 'N';
263900100208             clear IMK17cmm;
264000100208           ENDIF;
264100100205         ENDIF;
264200091223       //?Imposto commerciale se passato
264300091223         IF  IMK17cmm > 0;
264400091223           V02cmm = %editc(IMK17cmm:'X');
264500130808           chain  (iMK17cmm)  AZCMM000;
264600130808           IF  %found(AZCMM01L);
264700130808             V02dcmm = CMMdes;
264800130808           ENDIF;
264900091223         //?proteggo se richiesto
265000091223           ProteggiCMM = (IMK17pcmm = 'S');
265100091223         ENDIF;
265200091223
265300091223       //?Imposto data attività se passata
265400091223         IF  IMK17dad > 0;
265500091223           data_eur = %date(IMK17dad:*iso);
265600091223           V02dad   = %dec(data_eur);
265700091223         //?proteggo se richiesto
265800091223           ProteggiDAD = (IMK17pdad = 'S');
265900091223         ENDIF;
266000091228
266100091229       //?Posiziono il cursore
266200091229         SELECT;
266300091229       //?Sul commerciale se NON passato
266400091229           WHEN IMK17cmm = 0;
266500091229             PosCurCMM = *on;
266600091229       //?Sulla data se commerciale passato
266700091229           WHEN IMK17cmm > 0;
266800091229             PosCurDAD = *on;
266900091229       //?Su ora inizio impegno se passata la data
267000091229           WHEN IMK17dad > 0;
267100091229             PosCurOII = *on;
267200091229         ENDSL;
267300091223
267400091223       ENDSR;
267500091223
267600091223       //--------------------------------------------------------------
267700091223       //?Controllo videata DP2.
267800091223       //--------------------------------------------------------------
267900091223       BEGSR CtrDP2;
268000091223
268100091223         WindDspF = IndDspF;
268200091223         reset WindDspFb;
268300091223         IndDspF  = WindDspF;
268400091228
268500091228       //?Commerciale  -  Ricerca
268600091228         IF  %scan('?' : V02cmm) > *zero;
268700091228           ErrGenerico = *on;
268800091228           PosCurCMM   = *on;
268900130808           clear  TRMK43ds;
269000130808           iMK43solU = 'S';
269100130808           iMK43fil  = DUTpou;
269200130808           TRMK43R (kpjba : TRMK43ds);
269300130808           if  oMK43err = *off  and  oMK43fxx = *blank;
269400130808             V02cmm  = %editc( oMK43cmm : 'X' );
269500130808             V02dcmm = oMK43des;
269600130808           endif;
269700091228           leavesr;
269800091228         ENDIF;
269900091228
270000091228       //?Commerciale  -  Obbligatorio
270100091228         IF  V02cmm = *blanks  or  V02cmm = *zero;
270200091228           ErrMessage  = *on;
270300091228           ErrGenerico = *on;
270400091228           PosCurCMM   = *on;
270500091228           V02msg      = $Msg(08);
270600091228           leavesr;
270700091228         ENDIF;
270800091230
270900091230         IF  %check(digits:V02cmm) > 0;
271000091230           ErrMessage  = *on;
271100091230           ErrGenerico = *on;
271200091230           PosCurCMM   = *on;
271300091230           V02msg      = $Msg(08);
271400091230           leavesr;
271500091230         ENDIF;
271600091228
271700100205       //?Commerciale NON protetto
271800100205         IF  not ProteggiCMM;
271900100310
272000100310       //?Se commerciale diverso dal precedente spengo i flag di forzatura
272100100310            IF  V02cmm <> sav_V01cmm;
272200100310              $ForzaImpegno = *off;
272300100310              $ForzaInt     = *off;
272400100310            ENDIF;
272500100310
272600091228       //?Commerciale  -  Controllo
272700130808           CMMcod = %int(V02cmm);
272800130808           chain  (CMMcod)  AZCMM000;
272900130808           IF  NOT %found(AZCMM01L);
273000100205             ErrMessage  = *on;
273100100205             ErrGenerico = *on;
273200100205             PosCurCMM   = *on;
273300100205             V02msg      = $Msg(08);
273400100205             leavesr;
273500100205           ENDIF;
273600091228
273700130808           V02dcmm = CMMdes;
273800100310           sav_V01cmm = V02cmm;
273900110518
274000110518         //?commerciale valido no vari o clienti inattivi
274100130808           IF  CMMpar <> *blanks;
274200110518             ErrMessage  = *on;
274300110518             ErrGenerico = *on;
274400110518             PosCurCMM   = *on;
274500110518             V02msg      = $Msg(08);
274600110518             leavesr;
274700110518           ENDIF;
274800111123
274900111123         //?commerciale valido data inizio e fine attività
275000130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
275100111123             ErrMessage  = *on;
275200111123             ErrGenerico = *on;
275300111123             PosCurCMM   = *on;
275400111123             V02msg      = $Msg(08);
275500111123             leavesr;
275600111123           ENDIF;
275700100310
275800100310         //?controllo se attività può essere eseguita dal commerciale interno
275900130808           IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
276000100310             SELECT;
276100100310               WHEN  not $ForzaInt and §CCOcoi = 'F';
276200100310                 ErrMessage  = *on;
276300100310                 ErrGenerico = *on;
276400100310                 PosCurCMM   = *on;
276500100310                 V02msg      = $Msg(15);
276600100310                 V02msg      = %trim(V02msg) + ' Enter per forzare';
276700100310                 $ForzaInt = *on;
276800100310                 leavesr;
276900100310               WHEN  §CCOcoi = 'N';
277000100310                 ErrMessage  = *on;
277100100310                 ErrGenerico = *on;
277200100310                 PosCurCMM   = *on;
277300100310                 V02msg      = $Msg(15);
277400100310                 leavesr;
277500100310             ENDSL;
277600100310           ENDIF;
277700091228
277800091228       //?Commerciale  -  solo unificante
277900130808           IF  %dec(V02cmm:7:0) <> CMMuni;
278000100205             ErrMessage  = *on;
278100100205             ErrGenerico = *on;
278200100205             PosCurCMM   = *on;
278300100210             V02msg      = $Msg(14);
278400100205             leavesr;
278500100205           ENDIF;
278600100205
278700100205         //?utente abilitato al commerciale
278800100208           clear TNTAA1DS;
278900120810           ITAA1tipo = 'M';
279000120810           ITAA1caut = '§utegtc';
279100100208           ITAA1cmm  = %dec(V02cmm:7:0);
279200100521           ITAA1abi = 'RA';
279300100208           kpjbu     = tntaa1ds;
279400100208           tntaa1c (kpjba);
279500100208           TNTAA1DS = kpjbu;
279600100208           IF  OTAA1fabi = 'N';
279700100205             ErrMessage  = *on;
279800100205             ErrGenerico = *on;
279900100205             PosCurCMM   = *on;
280000100208             V02msg      = $Msg(08);
280100100205             leavesr;
280200100205           ENDIF;
280300100205         ENDIF;
280400091223
280500091223       //?Data attività  -  obbligatoria
280600091223         IF  V02dad = 0;
280700091223           ErrMessage  = *on;
280800091223           ErrGenerico = *on;
280900091223           PosCurDAD   = *on;
281000091223           V02msg      = $Msg(06);
281100091223           leavesr;
281200091223         ENDIF;
281300091223
281400091223       //?Data attività  -  controllo
281500091223         clear wlbdat;
281600091223         G02dat = V02dad;
281700091223         xsrda8(wlbdat);
281800091223         IF  G02err = '1';
281900091223           ErrMessage  = *on;
282000091223           ErrGenerico = *on;
282100091223           PosCurDAD   = *on;
282200091223           V02msg      = $Msg(06);
282300091223           leavesr;
282400091223         ENDIF;
282500091223
282600091223         V02dad = G02dat;
282700091223         wdad   = G02inv;
282800100119
282900100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
283000100119         IF  wdad < 20000101 or wdad > 20391231;
283100100119           ErrMessage  = *on;
283200100119           ErrGenerico = *on;
283300100119           PosCurDAD   = *on;
283400100119           V02msg      = $Msg(06);
283500100119           leavesr;
283600100119         ENDIF;
283700100119
283800091223         data_iso = (%date(wdad));
283900091223
284000091223       //?Data attività  -  no festivo
284100091223         w0030   = %dec(%subst(V02cmm:1:3):3:0);
284200091223         wanno   = %subdt(data_ISO:*years);
284300091223         wmese   = %subdt(data_ISO:*months);
284400091223         wgiorno = %subdt(data_ISO:*days);
284500100423         exsr CtrFesta;
284600091223         IF  $Festa;
284700091223           ErrMessage  = *on;
284800091223           ErrGenerico = *on;
284900091223           PosCurDAD   = *on;
285000091228           V02msg      = $Msg(06);
285100091223           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
285200091223           leavesr;
285300091223         ENDIF;
285400091223
285500091223       //?Impegno  ORA inizio  -  obbligatorio
285600171221         IF  V02oii <= 0;
285700091223           ErrMessage  = *on;
285800091223           ErrGenerico = *on;
285900091223           PosCurOII   = *on;
286000091223           V02msg      = $Msg(07);
286100171222           if  V02oii  < *zeros;
286200171222             V02msg    = %trimR( V02msg ) + ' (negativa)';
286300171222           endif;
286400091223           leavesr;
286500091223         ENDIF;
286600091223
286700091223       //?Impegno  ORA inizio  -  controllo
286800120104         IF  %dec(%subst(%editc(V02oii:'X'):1:2):2:0) > 23 or
286900091228             %dec(%subst(%editc(V02oii:'X'):3:2):2:0) > 59;
287000091223           ErrMessage  = *on;
287100091223           ErrGenerico = *on;
287200091223           PosCurOII   = *on;
287300091223           V02msg      = $Msg(07);
287400091223           leavesr;
287500091223         ENDIF;
287600091223
287700091223       //?Impegno  ORA fine    -  obbligatorio
287800171221         IF  V02ofi <= 0;
287900091223           ErrMessage  = *on;
288000091223           ErrGenerico = *on;
288100091223           PosCurOFI   = *on;
288200091223           V02msg      = $Msg(07);
288300171222           if  V02ofi  < *zeros;
288400171222             V02msg    = %trimR( V02msg ) + ' (negativa)';
288500171222           endif;
288600091223           leavesr;
288700091223         ENDIF;
288800091223
288900091223       //?Impegno  ORA fine    -  controllo
289000120104         IF  %dec(%subst(%editc(V02ofi:'X'):1:2):2:0) > 23 or
289100091228             %dec(%subst(%editc(V02ofi:'X'):3:2):2:0) > 59;
289200091223           ErrMessage  = *on;
289300091223           ErrGenerico = *on;
289400091223           PosCurOFI   = *on;
289500091223           V02msg      = $Msg(07);
289600091223           leavesr;
289700091223         ENDIF;
289800091223
289900091223       //?Impegno  ORA inizio  -  congruente con ORA fine
290000091223         IF  V02oii > 0  and  V02ofi = 0;
290100091223           ErrMessage  = *on;
290200091223           ErrGenerico = *on;
290300091223           PosCurOFI   = *on;
290400091223           V02msg      = $Msg(09);
290500091223           leavesr;
290600091223         ENDIF;
290700091223         IF  V02oii > 0  and  V02ofi > 0  and  V02oii > V02ofi ;
290800091223           ErrMessage  = *on;
290900091223           ErrGenerico = *on;
291000091223           PosCurOFI   = *on;
291100091223           V02msg      = $Msg(09);
291200091223           leavesr;
291300091223         ENDIF;
291400091223
291500091223       //?Verifico se il commerciale è libero
291600091223         clear trmk84ds;
291700091223         IMK84cmm = %dec(V02cmm:7:0);
291800091223         IMK84dad = wdad;
291900091223         IMK84oii = V02oii;
292000091223         IMK84ofi = V02ofi;
292100091223         trmk84r (trmk84ds);
292200091223       //?se torna errore <> "9" vuol dire che il commerciale è impegnato
292300091223         IF  OMK84err <> *blanks and OMK84err <> '9' and
292400091223             Not $ForzaImpegno;
292500091223           ErrGenerico = *on;
292600091223           ErrMessage  = *on;
292700091223           PosCurCMM   = *on;
292800091223           V02msg = OMK84msg;
292900091229           V02msg = %trim(V02msg) + '. Enter per forzare.';
293000091223           $ForzaImpegno = *on;
293100091223           leavesr;
293200091223         ENDIF;
293300091223
293400091223       ENDSR;
293500091229
293600091229       //--------------------------------------------------------------
293700091229       //?Gestione tasto funzionale F06 da videata DP2.
293800091229       //?F06=Conferma
293900091229       //--------------------------------------------------------------
294000091229       BEGSR F06DP2;
294100091229
294200091229       //?Stacco nuovo numero attività
294300091229         exsr NewAttivita;
294400091229       //?se errore vado subito a fine
294500091229         IF  O33Err <> 0;
294600091229           $Fine = *on;
294700091229           ErrGenerico = *on;
294800091229           V02msg = O33msg;
294900091229           leavesr;
295000091229         ENDIF;
295100091229
295200091229       //?Scrivo nuova attività
295300091229         clear tiatc000;
295400091229         atctat  = V02att;
295500091229         atcatn  = O33Nrf;
295600091229         atcatnp = 1;
295700091229         atccpo  = IMK17cpo;
295800091229         atcksc  = IMK17ksc;
295900091229         atcnrv  = IMK17nrv;
296000091229         atccad  = V02cau;
296100091229         atcdad  = wdad;
296200091229         atchda  = V02oii * 100;
296300091229         atccmm  = %dec(V02cmm:7:0);
296400091229         atcoii  = V02oii;
296500091229         atcofi  = V02ofi;
296600091229         atcdim  = %dec(%date());
296700091229         atchim  = %dec(%time());
296800091229         atcpri  = knmus ;
296900091229
297000091229         write TIATC000;
297100091229         feod  TIATC01L;
297200091229
297300091229       //?Se ci sono note le registro
297400091229         clear TRMK20ds;
297500091229         IMK20flm  = 'C';
297600091229         IMK20cpo  = atccpo;
297700100311         IMK20ksc  = atcksc;
297800100311         IMK20nrv  = atcnrv;
297900091229         IMK20tat  = atctat;
298000091229         IMK20atn  = atcatn;
298100091229         IMK20atnp = atcatnp - 1;
298200091229         trmk20r (kpjba : TRMK20ds);
298300091229
298400091229       //?Ritorno al programma chiamante
298500091229         $Fine   = *on ;
298600091229
298700091229       ENDSR;
298800091229
298900091229       //--------------------------------------------------------------
299000091229       //?Gestione videata DT2.
299100091229       //--------------------------------------------------------------
299200091229       BEGSR GesDT2;
299300091229
299400091229         // Inizializzazione videata
299500091229         if $InzDT2 = *on;
299600091229           exsr InzDT2;
299700091229           $InzDT2 = *off;
299800091229         endif;
299900091229
300000091229       //?Emissione videata
300100091229         exfmt MK17DT2;
300200091229         errMessage   = *off;
300300091229         errGenerico  = *off;
300400091229         clear V02msg;
300500091229
300600091229         SELECT;
300700091229
300800091229       //?F12 = Ritorno
300900091229           WHEN dsp_aid = c_F12;
301000100112             exsr F12D02;
301100091229             leavesr ;
301200091229
301300091229       //?F11 = Agenda
301400091229           WHEN dsp_aid = c_F11;
301500091229             exsr F11D02;
301600091229             leavesr ;
301700091229
301800091229       //?F18 = Note
301900091229           WHEN dsp_aid = c_F18;
302000091229             exsr F18D01;
302100091229             leavesr ;
302200091229
302300091229       //?F06 = Conferma
302400091229           WHEN dsp_aid = c_F06;
302500091229             //?prima faccio i controlli
302600091229             exsr CtrDT2;
302700091229             IF  ErrGenerico;
302800091229               leavesr;
302900091229             ENDIF;
303000091229             //?tutto ok confermo i dati
303100091229             exsr F06DT2;
303200091229             leavesr ;
303300091229
303400091229       //?Enter
303500091229           OTHER;
303600091229       //?Controllo i dati
303700091229            exsr CtrDT2;
303800091229            IF  ErrGenerico;
303900091229              leavesr;
304000091229            ENDIF;
304100091229
304200091229         ENDSL;
304300091229
304400091229       ENDSR;
304500091229
304600091229       //--------------------------------------------------------------
304700091229       //?Caricamento videata DT2.
304800091229       //--------------------------------------------------------------
304900091229       BEGSR InzDT2;
305000091229
305100091229         clear V02cmm;
305200091229         clear v02dcmm;
305300091229         clear v02dadi;
305400091229         clear v02dadf;
305500091229
305600091229         $ForzaImpegno = *off;
305700100310         $ForzaInt     = *off;
305800140227         $ForzaFerie   = *off;
305900100310         clear sav_V01cmm;
306000091229
306100100205       //?Imposto commerciale se passato
306200100208         IF  IMK17cmm > 0 and IMK17pcmm = *blanks;
306300100208       //?ma se non è richiesta la protezione e l'utente non è abilitato al
306400100208       //?commerciale non propongo il dato
306500100208           clear TNTAA1DS;
306600120810           ITAA1tipo = 'M';
306700120810           ITAA1caut = '§utegtc';
306800100208           ITAA1cmm  = IMK17cmm;
306900100521           ITAA1abi = 'RA';
307000100208           kpjbu     = tntaa1ds;
307100100208           tntaa1c (kpjba);
307200100208           TNTAA1DS = kpjbu;
307300100208           IF  OTAA1fabi = 'N';
307400100208             clear IMK17cmm;
307500100208           ENDIF;
307600100205         ENDIF;
307700091229       //?Imposto commerciale se passato
307800091229         IF  IMK17cmm > 0;
307900091229           V02cmm = %editc(IMK17cmm:'X');
308000130808           chain   (iMK17cmm) AZCMM000;
308100130808           IF  %found(AZCMM01L);
308200130808             V02dcmm = CMMdes;
308300091229           ENDIF;
308400091229         //?proteggo se richiesto
308500091229           ProteggiCMM = (IMK17pcmm = 'S');
308600091229         ENDIF;
308700091229
308800091229       //?Imposto data attività se passata
308900091229         IF  IMK17dad > 0;
309000091229           data_eur = %date(IMK17dad:*iso);
309100091229           V02dadi  = %dec(data_eur);
309200091229         //?proteggo se richiesto
309300091229           ProteggiDAD = (IMK17pdad = 'S');
309400091229         ENDIF;
309500091229
309600091229       //?Posiziono il cursore
309700091229         SELECT;
309800091229       //?Sul commerciale se NON passato
309900091229           WHEN IMK17cmm = 0;
310000091229             PosCurCMM = *on;
310100091229       //?Sulla data se commerciale passato
310200091229           WHEN IMK17cmm > 0;
310300091229             PosCurDAD = *on;
310400091229       //?Sulla data fine se passata la data inizio
310500091229           WHEN IMK17dad > 0;
310600091229             PosCurDADF = *on;
310700091229         ENDSL;
310800091229
310900091229       ENDSR;
311000091229
311100091229       //--------------------------------------------------------------
311200091229       //?Controllo videata DT2.
311300091229       //--------------------------------------------------------------
311400091229       BEGSR CtrDT2;
311500091229
311600091229         WindDspF = IndDspF;
311700091229         reset WindDspFb;
311800091229         IndDspF  = WindDspF;
311900091229
312000091229       //?Commerciale  -  Ricerca
312100091229         IF  %scan('?' : V02cmm) > *zero;
312200091229           ErrGenerico = *on;
312300091229           PosCurCMM   = *on;
312400130808           clear  TRMK43ds;
312500130808           iMK43solU = 'S';
312600130808           iMK43fil  = DUTpou;
312700130808           TRMK43R (kpjba : TRMK43ds);
312800130808           if  oMK43err = *off  and  oMK43fxx = *blank;
312900130808             V02cmm  = %editc( oMK43cmm : 'X' );
313000130808             V02dcmm = oMK43des;
313100130808           endif;
313200091229           leavesr;
313300091229         ENDIF;
313400091229
313500091229       //?Commerciale  -  Obbligatorio
313600091229         IF  V02cmm = *blanks  or  V02cmm = *zero;
313700091229           ErrMessage  = *on;
313800091229           ErrGenerico = *on;
313900091229           PosCurCMM   = *on;
314000091229           V02msg      = $Msg(08);
314100091229           leavesr;
314200091229         ENDIF;
314300091230
314400091230         IF  %check(digits:V02cmm) > 0;
314500091230           ErrMessage  = *on;
314600091230           ErrGenerico = *on;
314700091230           PosCurCMM   = *on;
314800091230           V02msg      = $Msg(08);
314900091230           leavesr;
315000091230         ENDIF;
315100091229
315200100205       //?Commerciale NON protetto
315300100205         IF  not ProteggiCMM;
315400100310
315500100310       //?Se commerciale diverso dal precedente spengo i flag di forzatura
315600100310            IF  V02cmm <> sav_V01cmm;
315700100310              $ForzaImpegno = *off;
315800100310              $ForzaInt     = *off;
315900100310            ENDIF;
316000100310
316100091229       //?Commerciale  -  Controllo
316200130808           CMMcod = %int(V02cmm);
316300130808           chain  (CMMcod)  AZCMM000;
316400130808           IF  NOT %found(AZCMM01L);
316500100205             ErrMessage  = *on;
316600100205             ErrGenerico = *on;
316700100205             PosCurCMM   = *on;
316800100205             V02msg      = $Msg(08);
316900100205             leavesr;
317000100205           ENDIF;
317100091229
317200130808           V02dcmm = CMMdes;
317300100310           sav_V01cmm = V02cmm;
317400110518
317500110518         //?commerciale valido no vari o clienti inattivi
317600130808           IF  CMMpar <> *blanks;
317700110518             ErrMessage  = *on;
317800110518             ErrGenerico = *on;
317900110518             PosCurCMM   = *on;
318000110518             V02msg      = $Msg(08);
318100110518             leavesr;
318200110518           ENDIF;
318300111123
318400111123         //?commerciale valido data inizio e fine attività
318500130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
318600111123             ErrMessage  = *on;
318700111123             ErrGenerico = *on;
318800111123             PosCurCMM   = *on;
318900111123             V02msg      = $Msg(08);
319000111123             leavesr;
319100111123           ENDIF;
319200100310
319300100310         //?controllo se attività può essere eseguita dal commerciale interno
319400130808           IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
319500100310             SELECT;
319600100310               WHEN  not $ForzaInt and §CCOcoi = 'F';
319700100310                 ErrMessage  = *on;
319800100310                 ErrGenerico = *on;
319900100310                 PosCurCMM   = *on;
320000100310                 V02msg      = $Msg(15);
320100100310                 V02msg      = %trim(V02msg) + ' Enter per forzare';
320200100310                 $ForzaInt = *on;
320300100310                 leavesr;
320400100310               WHEN  §CCOcoi = 'N';
320500100310                 ErrMessage  = *on;
320600100310                 ErrGenerico = *on;
320700100310                 PosCurCMM   = *on;
320800100310                 V02msg      = $Msg(15);
320900100310                 leavesr;
321000100310             ENDSL;
321100100310           ENDIF;
321200091229
321300091229       //?Commerciale  -  solo unificante
321400130808           IF  %dec(V02cmm:7:0) <> CMMuni;
321500100205             ErrMessage  = *on;
321600100205             ErrGenerico = *on;
321700100205             PosCurCMM   = *on;
321800100210             V02msg      = $Msg(14);
321900100205             leavesr;
322000100205           ENDIF;
322100100205
322200100205         //?utente abilitato al commerciale
322300100208           clear TNTAA1DS;
322400120810           ITAA1tipo = 'M';
322500120810           ITAA1caut = '§utegtc';
322600100208           ITAA1cmm  = %dec(V02cmm:7:0);
322700100521           ITAA1abi = 'RA';
322800100208           kpjbu     = tntaa1ds;
322900100208           tntaa1c (kpjba);
323000100208           TNTAA1DS = kpjbu;
323100100208           IF  OTAA1fabi = 'N';
323200100205             ErrMessage  = *on;
323300100205             ErrGenerico = *on;
323400100205             PosCurCMM   = *on;
323500100208             V02msg      = $Msg(08);
323600100205             leavesr;
323700100205           ENDIF;
323800100205         ENDIF;
323900091229
324000091229       //?Data inizio attività  -  obbligatoria
324100091229         IF  V02dadi = 0;
324200091229           ErrMessage  = *on;
324300091229           ErrGenerico = *on;
324400091229           PosCurDAD   = *on;
324500091229           V02msg      = $Msg(06);
324600091229           leavesr;
324700091229         ENDIF;
324800091229
324900091229       //?Data inizio attività  -  controllo
325000091229         clear wlbdat;
325100091229         G02dat = V02dadi;
325200091229         xsrda8(wlbdat);
325300091229         IF  G02err = '1';
325400091229           ErrMessage  = *on;
325500091229           ErrGenerico = *on;
325600091229           PosCurDAD   = *on;
325700091229           V02msg      = $Msg(06);
325800091229           leavesr;
325900091229         ENDIF;
326000091229
326100091229         V02dadi  = G02dat;
326200091229         wdad     = G02inv;
326300100119
326400100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
326500100119         IF  wdad < 20000101 or wdad > 20391231;
326600100119           ErrMessage  = *on;
326700100119           ErrGenerico = *on;
326800100119           PosCurDAD   = *on;
326900100119           V02msg      = $Msg(06);
327000100119           leavesr;
327100100119         ENDIF;
327200100119
327300091229         data_iso = (%date(wdad));
327400091229
327500091229       //?Data inizio attività  -  no festivo
327600091229         w0030   = %dec(%subst(V02cmm:1:3):3:0);
327700091229         wanno   = %subdt(data_ISO:*years);
327800091229         wmese   = %subdt(data_ISO:*months);
327900091229         wgiorno = %subdt(data_ISO:*days);
328000100423         exsr CtrFesta;
328100091229         IF  $Festa;
328200091229           ErrMessage  = *on;
328300091229           ErrGenerico = *on;
328400091229           PosCurDAD   = *on;
328500091229           V02msg      = $Msg(06);
328600091229           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
328700091229           leavesr;
328800091229         ENDIF;
328900091229
329000091229       //?Data fine attività  -  obbligatoria
329100091229         IF  V02dadf = 0;
329200091229           ErrMessage  = *on;
329300091229           ErrGenerico = *on;
329400091229           PosCurDADF  = *on;
329500091229           V02msg      = $Msg(06);
329600091229           leavesr;
329700091229         ENDIF;
329800091229
329900100423       //?Data fine attività  -  controllo
330000091229         clear wlbdat;
330100091229         G02dat = V02dadf;
330200091229         xsrda8(wlbdat);
330300091229         IF  G02err = '1';
330400091229           ErrMessage  = *on;
330500091229           ErrGenerico = *on;
330600091229           PosCurDADF  = *on;
330700091229           V02msg      = $Msg(06);
330800091229           leavesr;
330900091229         ENDIF;
331000091229
331100091229         V02dadf  = G02dat;
331200091229         wdadf    = G02inv;
331300100119
331400100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
331500100119         IF  wdadf < 20000101 or wdadf > 20391231;
331600100119           ErrMessage  = *on;
331700100119           ErrGenerico = *on;
331800100119           PosCurDADF  = *on;
331900100119           V02msg      = $Msg(06);
332000100119           leavesr;
332100100119         ENDIF;
332200100119
332300091229         data_iso = (%date(wdadf));
332400091229
332500100423       //?Data fine attività  -  no festivo
332600091229         w0030   = %dec(%subst(V02cmm:1:3):3:0);
332700091229         wanno   = %subdt(data_ISO:*years);
332800091229         wmese   = %subdt(data_ISO:*months);
332900091229         wgiorno = %subdt(data_ISO:*days);
333000100423         exsr CtrFesta;
333100091229         IF  $Festa;
333200091229           ErrMessage  = *on;
333300091229           ErrGenerico = *on;
333400091229           PosCurDADF  = *on;
333500091229           V02msg      = $Msg(06);
333600091229           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
333700091229           leavesr;
333800091229         ENDIF;
333900091229
334000091229       //?Data inizio attività  -  congruente con data fine attività
334100091229         IF  wdad > 0  and  wdadf = 0;
334200091229           ErrMessage  = *on;
334300091229           ErrGenerico = *on;
334400091229           PosCurDADF  = *on;
334500091229           V02msg      = $Msg(11);
334600091229           leavesr;
334700091229         ENDIF;
334800091229         IF  wdad > 0  and  wdadf > 0  and  wdad > wdadf;
334900091229           ErrMessage  = *on;
335000091229           ErrGenerico = *on;
335100091229           PosCurDADF  = *on;
335200091229           V02msg      = $Msg(11);
335300091229           leavesr;
335400091229         ENDIF;
335500140227
335600140227         // -?Impostazione delle 2 date in formato *ISO
335700140227         data_isof = (%date(wdadf));
335800140227         data_iso  = (%date(wdad));
335900140227
336000140227         // -?Controllo periodo: NON può essere superiore alle 3 settimane
336100140227         //  ?N.B.?- Ora questa videata (e quindi questo controllo) è
336200140227         //  ?prevista solo per: · Attività = "F"  (FERIE/ASSENZE)
336300140227         //  ?                   · Causale  = "FT" (FERIE/ASSENZA TOTALE)
336400140227         If  NOT $ForzaFerie  and
336500140227             %diff( Data_IsoF : Data_Iso : *days ) > 21;
336600140227           $ForzaFerie = *on;
336700140227           ErrGenerico = *on;
336800140227           ErrMessage  = *on;
336900140227           PosCurDADF  = *on;
337000140227           V02msg      = $Msg(26);
337100140227           leavesr;
337200140227         EndIf;
337300091229
337400091229       //?Verifico se il commerciale è libero
337500091229       //?controllo tutti i giorni inseriti a video
337600091229       //?anche i festivi...teoricamente nessuna attività viene fatta di festa
337700091229         DOW  data_iso <= data_isof;
337800091229           clear trmk84ds;
337900091229           IMK84cmm = %dec(V02cmm:7:0);
338000091229           IMK84dad = %dec(data_iso);
338100091229           IMK84oii = 0001;
338200091229           IMK84ofi = 2359;
338300091229           trmk84r (trmk84ds);
338400091229         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
338500091229           IF  OMK84err <> *blanks and OMK84err <> '9' and
338600091229               Not $ForzaImpegno;
338700091229             ErrGenerico = *on;
338800091229             ErrMessage  = *on;
338900091229             PosCurCMM   = *on;
339000091229             V02msg = OMK84msg;
339100091229             V02msg = %trim(V02msg) + '. Enter per forzare.';
339200091229             $ForzaImpegno = *on;
339300091229             leavesr;
339400091229           ENDIF;
339500091229           data_iso += %days(1);
339600091229         ENDDO;
339700091229
339800091229       ENDSR;
339900091229
340000091229       //--------------------------------------------------------------
340100091229       //?Gestione tasto funzionale F06 da videata DT2.
340200091229       //?F06=Conferma
340300091229       //--------------------------------------------------------------
340400091229       BEGSR F06DT2;
340500091229
340600091229       //?Scrivo nuova attività
340700091229         data_iso  = (%date(wdad));
340800091229       //?1 rcd per ogni giorno inserito a video
340900091229         DOW  data_iso <= data_isof;
341000100929         //?controllo che non sia festivo
341100100929           w0030   = %dec(%subst(V02cmm:1:3):3:0);
341200100929           wanno   = %subdt(data_ISO:*years);
341300100929           wmese   = %subdt(data_ISO:*months);
341400100929           wgiorno = %subdt(data_ISO:*days);
341500100929           exsr CtrFesta;
341600100929           IF  $Festa;
341700100929             data_iso += %days(1);
341800100929             iter;
341900100929           ENDIF;
342000100929
342100100929         //?Stacco nuovo numero attività
342200100929           exsr NewAttivita;
342300100929         //?se errore vado subito a fine
342400100929           IF  O33Err <> 0;
342500100929             $Fine = *on;
342600100929             ErrGenerico = *on;
342700100929             V02msg = O33msg;
342800100929             leavesr;
342900100929           ENDIF;
343000100929
343100091229           clear tiatc000;
343200091229           atctat  = V02att;
343300091229           atcatn  = O33Nrf;
343400100929           atcatnp = 1;
343500091229           atccpo  = IMK17cpo;
343600091229           atcksc  = IMK17ksc;
343700091229           atcnrv  = IMK17nrv;
343800091229           atccad  = V02cau;
343900091229           atcdad  = %dec(data_iso);
344000100108           atchda  = 090000;
344100091229           atccmm  = %dec(V02cmm:7:0);
344200100108           atcoii  = 0001;
344300091229           atcofi  = 2359;
344400091229           atcdim  = %dec(%date());
344500091229           atchim  = %dec(%time());
344600091229           atcpri  = knmus ;
344700091229
344800091229           write TIATC000;
344900091229           feod  TIATC01L;
345000100929
345100100929         //?Se ci sono note le registro
345200100929           clear TRMK20ds;
345300100929           IMK20flm  = 'C';
345400100929           IMK20cpo  = atccpo;
345500100929           IMK20ksc  = atcksc;
345600100929           IMK20nrv  = atcnrv;
345700100929           IMK20tat  = atctat;
345800100929           IMK20atn  = atcatn;
345900100929           IMK20atnp = 0;
346000100929           trmk20r (kpjba : TRMK20ds);
346100091229
346200091229           data_iso += %days(1);
346300091229         ENDDO;
346400091229
346500091229       //?Ritorno al programma chiamante
346600091229         $Fine   = *on ;
346700091229
346800091229       ENDSR;
346900091229
347000091229       //--------------------------------------------------------------
347100091229       //?Gestione videata C02.
347200091229       //--------------------------------------------------------------
347300091229       BEGSR GesC02;
347400091229
347500091229         // Inizializzazione videata
347600091229         if $InzC02 = *on;
347700091229           exsr InzC02;
347800091229           $InzC02 = *off;
347900091229         endif;
348000091229
348100091229         $Eof   = *off;
348200091229
348300091229       //?Emissione videata
348400091229         sfldsp_n = *off;
348500091229         write MK17Z02;
348600091229         exfmt MK17C02;
348700091229         ErrMessage   = *off;
348800091229         ErrGenerico  = *off;
348900091229         clear V02msg;
349000091229
349100091229         SELECT;
349200091229
349300091229       //?F12 = Ritorno
349400091229           WHEN dsp_aid = c_F12;
349500100205             exsr F12D02;
349600091229             leavesr ;
349700091229
349800091229       //?F11 = Agenda
349900091229           WHEN dsp_aid = c_F11;
350000091230             exsr F11D02;
350100091229             leavesr ;
350200091229
350300091229       //?F18 = Note
350400091229           WHEN dsp_aid = c_F18;
350500091229             exsr F18D01;
350600091229             leavesr ;
350700091229
350800091229       //?F06 = Conferma
350900091229           WHEN dsp_aid = c_F06;
351000091229             //?prima faccio i controlli
351100091229             exsr CtrC02;
351200091229             IF  ErrGenerico;
351300091229               leavesr;
351400091229             ENDIF;
351500091229             //?tutto ok confermo i dati
351600091229             exsr F06C02;
351700091229             leavesr ;
351800091229
351900091229       //?Enter
352000091229           OTHER;
352100091229       //?Controllo i dati
352200091229            exsr CtrC02;
352300091229            IF  ErrGenerico;
352400091229              leavesr;
352500091229            ENDIF;
352600091229
352700091229         ENDSL;
352800091229
352900091229       ENDSR;
353000091229
353100091229       //--------------------------------------------------------------
353200091229       //?Caricamento videata C02.
353300091229       //--------------------------------------------------------------
353400091229       BEGSR InzC02;
353500091229
353600091229         //?Pulizia subfile
353700091229         exsr PulS02;
353800091229
353900091229       //?Imposto i campi del control
354000091229         VC2pgm  = V02pgm;
354100091229         VC2sut  = V02sut;
354200091229         VC2sif  = V02sif;
354300091229         VC2mus  = V02mus;
354400091229         VC2att  = V02att;
354500100113         VC2datt = V02datt;
354600091229         VC2cau  = V02cau;
354700091229         VC2dcau = V02dcau;
354800100205         clear V02dad;
354900100205         clear V02oii;
355000100205         clear V02ofi;
355100091229
355200091229       //?Imposto data attività se passata
355300091229         IF  IMK17dad > 0;
355400091229           data_eur = %date(IMK17dad:*iso);
355500091229           V02dad   = %dec(data_eur);
355600091229         //?proteggo se richiesto
355700091229           ProteggiDAD = (IMK17pdad = 'S');
355800091229         ENDIF;
355900091229
356000091229       //?Posiziono il cursore
356100091229         SELECT;
356200091229       //?Sulla data se NON passata
356300091229           WHEN IMK17dad = 0;
356400091229             PosCurDAD = *on;
356500091229       //?Su ora inizio impegno se passata la data
356600091229           WHEN IMK17dad > 0;
356700091229             PosCurOII = *on;
356800091229         ENDSL;
356900091229
357000091229       //?Carico subfile
357100091229         exsr CarS02;
357200091229
357300091229       ENDSR;
357400091229
357500091229       //--------------------------------------------------------------
357600091229       //?Pulizia Subfile S02.
357700091229       //--------------------------------------------------------------
357800091229       BEGSR PulS02;
357900091229
358000091229         //?Pulizia subfile
358100091229         SflDsp_N    = *on;
358200091229         SflDspCtl_N = *on;
358300091229         write  MK17C02;
358400091229         SflDspCtl_N = *off;
358500091229         SflEnd      = *off;
358600091229
358700100205         V02rcd = 1;
358800091229         clear V02msg;
358900091229         ErrMessage  = *off;
359000091229         ErrGenerico = *off;
359100091229         clear S02Nrr;
359200091229
359300091229       ENDSR ;
359400091229
359500091229       //--------------------------------------------------------------
359600091229       //?Carico subfile S02.
359700091229       //--------------------------------------------------------------
359800091229       BEGSR CarS02;
359900100217
360000100304         //?Se richiesta riunione d'area 'RA' devo caricare gli agenti di area
360100100304         IF   V02cau = 'RA';
360200100217         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
360300100304           IF  OTAA1cabi = 'AZ';
360400100304             clear TRUL31DS;
360500100304             I31abi = 'RA';
360600100304             I31cdi = DUTdis;
360700100304             I31car = DUTare;
360800100304             I31cpo = DUTpou;
360900100304             callp trul31r (kpjba : trul31ds);
361000100304             ENDIF;
361100091230
361200100114         //?Carico sk di comodo con i commerciali d'area
361300100304           exsr CarAge;
361400100304         ENDIF;
361500100304         //?Se richiesta riunione commerciale 'RC' NON devo caricare gli agenti
361600100304         IF   V02cau = 'RC';
361700100304           clear $Age;
361800100304         ENDIF;
361900091229
362000100114         //?Carico subfile
362100100114         xx = 1;
362200100114         FOR xx by 1 to %elem($Age);
362300100114           IF  $Age(xx) = 0;
362400100114             iter;
362500100114           ENDIF;
362600130808           V02cmm = %editc($Age(xx):'X');
362700130808           chain  ($Age(xx))  AZCMM000;
362800130808           IF  %found(AZCMM01L);
362900130808             V02dcmm = CMMdes;
363000130808           ENDIF;
363100100310           VH2savcmm = V02cmm;
363200100310           clear VH2fimp;
363300100310           clear VH2fint;
363400100310           S02Nrr += 1;
363500100114           write MK17S02;
363600100114         ENDFOR;
363700091230
363800100114         //?Il resto a 0
363900100114         IF  S02Nrr < %elem($Age);
364000100114           yy = %elem($Age) - S02Nrr;
364100100205         //?faccio un accrocchio per visualizzare solo 1 pagina 2 pagine 3 pagine
364200100205           IF  S02Nrr > 1  and S02nrr < 25;
364300100205             yy = 24 - s02nrr;
364400100205           ENDIF;
364500100205           IF  S02Nrr > 24 and S02nrr < 49;
364600100205             yy = 48 - s02nrr;
364700100205           ENDIF;
364800100205           IF  S02Nrr > 48;
364900100205             yy = 72 - s02nrr;
365000100205           ENDIF;
365100100114           xx = 1;
365200100114           FOR xx by 1 to yy;
365300100114             clear MK17S02;
365400100114             S02Nrr += 1;
365500100114             write MK17S02;
365600100114           ENDFOR;
365700100114         ENDIF;
365800091229
365900091229         SflEnd = *on;
366000091229
366100091229       ENDSR;
366200100114
366300100114       //--------------------------------------------------------------
366400100114       //?Carico sk agenti d'area.
366500100114       //--------------------------------------------------------------
366600100114       BEGSR CarAge;
366700100114
366800100203         $End = *off;
366900100114         clear $AGE;
367000100205
367100100205         //?Carico gli agenti unificanti delle filiali gestite dall'utente
367200100114
367300100114         //?Imposto la stringa per SQL
367400131108         wSQL = 'select CMMcod +
367500131108                   from AZCMM00F +
367600131108                  where CMMatb = '' '' and +
367700131108                        substr( digits( CMMuni ), 1, 3) in (';
367800100114
367900100114         yy = 0;
368000100114         xx = 1;
368100100114         FOR xx by 1 to %elem(pog);
368200100114           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
368300100114             IF yy > 0;
368400100114               wSQL += ', ';
368500100114             ELSE;
368600100114               yy = 1;
368700100114             ENDIF;
368800100114             wSQL += '''';
368900100114             wSQL += pog(xx);
369000100114             wSQL += '''';
369100100114           ENDIF;
369200100114         ENDFOR;
369300100114
369400131108         wSQL += ') and CMMcod = CMMuni and +
369500131108                    substr( digits( CMMuni ), 4, 1) = ''0'' and +
369600131118                    CMMpar = '' '' and +
369700131118                    CMMfun NOT in (''COMIN'', ''ASC  '', ''SA   '')';
369800100114
369900131108         wSQL += ' order by CMMcod +
370000100114                   for fetch only';
370100100114
370200100114         //?Dichiarazione cursore
370300100114         exec sql
370400100114           prepare S1   from :wSQL;
370500100114         exec sql
370600100114           declare AREA cursor for S1;
370700100114
370800100114         //?Apertura del cursore
370900100114         exec sql
371000100114           open AREA;
371100100114
371200100114         IF sqlcode < 0;
371300100114           $End = *on;
371400100114         ENDIF;
371500100114
371600100114         yy = 0;
371700100114         DOW not $End;
371800131108
371900100114           exec sql
372000131108             fetch next from AREA into :wCMMcod;
372100131108
372200100114           IF sqlcod = 100 or sqlcod < 0;
372300100114             $End = *on;
372400100114             leave;
372500100114           ENDIF;
372600100114
372700131108           yy += 1;
372800131108           $AGE(yy) = wCMMcod;
372900100114
373000100114         ENDDO;
373100100114
373200100114         exec sql
373300100114           close AREA;
373400100114
373500100114       ENDSR;
373600091229
373700091229       //--------------------------------------------------------------
373800091229       //?Controllo videata C02.
373900091229       //--------------------------------------------------------------
374000091229       BEGSR CtrC02;
374100091229
374200091229         WindDspF = IndDspF;
374300091229         reset WindDspFb;
374400091229         IndDspF  = WindDspF;
374500091230
374600091230       //?Prima controllo i dati immessi nel control
374700091230
374800091230       //?Data attività  -  obbligatoria
374900091230         IF  V02dad = 0;
375000091230           ErrMessage  = *on;
375100091230           ErrGenerico = *on;
375200091230           PosCurDAD   = *on;
375300091230           V02msg      = $Msg(06);
375400091230           leavesr;
375500091230         ENDIF;
375600091230
375700091230       //?Data attività  -  controllo
375800091230         clear wlbdat;
375900091230         G02dat = V02dad;
376000091230         xsrda8(wlbdat);
376100091230         IF  G02err = '1';
376200091230           ErrMessage  = *on;
376300091230           ErrGenerico = *on;
376400091230           PosCurDAD   = *on;
376500091230           V02msg      = $Msg(06);
376600091230           leavesr;
376700091230         ENDIF;
376800091230
376900091230         V02dad = G02dat;
377000091230         wdad   = G02inv;
377100100119
377200100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
377300100119         IF  wdad < 20000101 or wdad > 20391231;
377400100119           ErrMessage  = *on;
377500100119           ErrGenerico = *on;
377600100119           PosCurDAD   = *on;
377700100119           V02msg      = $Msg(06);
377800100119           leavesr;
377900100119         ENDIF;
378000100119
378100091230         data_iso = (%date(wdad));
378200091230
378300091230       //?Data attività  -  no festivo
378400091230         w0030   = dutpou;
378500091230         wanno   = %subdt(data_ISO:*years);
378600091230         wmese   = %subdt(data_ISO:*months);
378700091230         wgiorno = %subdt(data_ISO:*days);
378800091230         exsr CtrFesta;
378900091230         IF  $Festa;
379000091230           ErrMessage  = *on;
379100091230           ErrGenerico = *on;
379200091230           PosCurDAD   = *on;
379300091230           V02msg      = $Msg(06);
379400091230           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
379500091230           leavesr;
379600091230         ENDIF;
379700091230
379800091230       //?Impegno  ORA inizio  -  obbligatorio
379900171221         IF  V02oii <= 0;
380000091230           ErrMessage  = *on;
380100091230           ErrGenerico = *on;
380200091230           PosCurOII   = *on;
380300091230           V02msg      = $Msg(07);
380400171222           if  V02oii  < *zeros;
380500171222             V02msg    = %trimR( V02msg ) + ' (negativa)';
380600171222           endif;
380700091230           leavesr;
380800091230         ENDIF;
380900091230
381000091230       //?Impegno  ORA inizio  -  controllo
381100120104         IF  %dec(%subst(%editc(V02oii:'X'):1:2):2:0) > 23 or
381200091230             %dec(%subst(%editc(V02oii:'X'):3:2):2:0) > 59;
381300091230           ErrMessage  = *on;
381400091230           ErrGenerico = *on;
381500091230           PosCurOII   = *on;
381600091230           V02msg      = $Msg(07);
381700091230           leavesr;
381800091230         ENDIF;
381900091230
382000091230       //?Impegno  ORA fine    -  obbligatorio
382100171221         IF  V02ofi <= 0;
382200091230           ErrMessage  = *on;
382300091230           ErrGenerico = *on;
382400091230           PosCurOFI   = *on;
382500091230           V02msg      = $Msg(07);
382600171222           if  V02ofi  < *zeros;
382700171222             V02msg    = %trimR( V02msg ) + ' (negativa)';
382800171222           endif;
382900091230           leavesr;
383000091230         ENDIF;
383100091230
383200091230       //?Impegno  ORA fine    -  controllo
383300120104         IF  %dec(%subst(%editc(V02ofi:'X'):1:2):2:0) > 23 or
383400091230             %dec(%subst(%editc(V02ofi:'X'):3:2):2:0) > 59;
383500091230           ErrMessage  = *on;
383600091230           ErrGenerico = *on;
383700091230           PosCurOFI   = *on;
383800091230           V02msg      = $Msg(07);
383900091230           leavesr;
384000091230         ENDIF;
384100091230
384200091230       //?Impegno  ORA inizio  -  congruente con ORA fine
384300091230         IF  V02oii > 0  and  V02ofi = 0;
384400091230           ErrMessage  = *on;
384500091230           ErrGenerico = *on;
384600091230           PosCurOFI   = *on;
384700091230           V02msg      = $Msg(09);
384800091230           leavesr;
384900091230         ENDIF;
385000091230         IF  V02oii > 0  and  V02ofi > 0  and  V02oii > V02ofi ;
385100091230           ErrMessage  = *on;
385200091230           ErrGenerico = *on;
385300091230           PosCurOFI   = *on;
385400091230           V02msg      = $Msg(09);
385500091230           leavesr;
385600091230         ENDIF;
385700091229
385800091230       //?Se tutto ok passo a verificare i dati immessi nel subfile
385900091229         clear S02Nrr;
386000091230
386100091230       //?Pulisco sk di comodo commerciali
386200091230         clear $AGE;
386300091229
386400091229         DOW  not $EoF;
386500091229           S02Nrr += 1;
386600091229           chain S02Nrr MK17S02;
386700091229           IF  not %found;
386800091229             $EoF = *on;
386900091229             leave;
387000091229           ENDIF;
387100091229
387200091230         //?Se non c'è il codice commerciale ritorno a leggere il subfile
387300100114           IF  V02cmm = *blanks;
387400091229             iter;
387500091229           ENDIF;
387600100114         //?Se il codice commerciale è a 0 ripulisco i campi e ritorno a leggere
387700100114         //?il subfile
387800100114           IF  V02cmm = *zeros;
387900100114             clear V02cmm;
388000100114             clear V02dcmm;
388100100114             V02Rcd  = S02Nrr;
388200100114             update MK17S02;
388300100114             iter;
388400100114           ENDIF;
388500100310
388600100310            IF  V02cmm <> VH2savcmm;
388700100310              clear VH2fimp;
388800100310              clear VH2fint;
388900100310            ENDIF;
389000091229
389100091230         //?Commerciale  -  Ricerca
389200091230           IF  %scan('?' : V02cmm) > *zero;
389300091230             ErrGenerico = *on;
389400091230             PosCurCMM   = *on;
389500130808             iMK43solU = 'S';
389600130808             iMK43fil  = %int(%subst(%editc(IMK17cmm:'X'):1:3));
389700130808             TRMK43R (kpjba : TRMK43ds);
389800130808             if  oMK43err = *off  and  oMK43fxx = *blank;
389900130808               V02cmm  = %editc( oMK43cmm : 'X' );
390000130808               V02dcmm = oMK43des;
390100130808             endif;
390200091230             V02Rcd  = S02Nrr;
390300091230             update MK17S02;
390400091230             $EoF = *on;
390500091230             leave;
390600091230           ENDIF;
390700091230
390800091230           IF  %check(digits:V02cmm) > 0;
390900091230             ErrMessage  = *on;
391000091230             ErrGenerico = *on;
391100091230             PosCurCMM   = *on;
391200091230             V02msg      = $Msg(08);
391300091230             V02Rcd      = S02Nrr;
391400091230             update MK17S02;
391500091230             $EoF = *on;
391600091230             leave;
391700091230           ENDIF;
391800091229
391900091230         //?Commerciale  -  Controllo
392000091230           IF  V02cmm > *zeros;
392100091230             clear V02dcmm;
392200130808             CMmcod = %int(V02cmm);
392300130808             chain  (CMMcod)  AZCMM000;
392400130808             IF  NOT %found(AZCMM01L);
392500091230               ErrMessage  = *on;
392600091230               ErrGenerico = *on;
392700091230               PosCurCMM   = *on;
392800091230               V02msg      = $Msg(08);
392900091230               V02Rcd      = S02Nrr;
393000091230               update MK17S02;
393100091230               $EoF = *on;
393200091230               leave;
393300100205             ENDIF;
393400100209
393500130808             V02dcmm = CMMdes;
393600100310             VH2savcmm = V02cmm;
393700100209
393800100209         //?commerciale valido no vari o clienti inattivi
393900130808             IF  CMMpar <> *blanks;
394000100209               ErrMessage  = *on;
394100100209               ErrGenerico = *on;
394200100209               PosCurCMM   = *on;
394300100209               V02msg      = $Msg(08);
394400100209               V02Rcd      = S02Nrr;
394500100209               update MK17S02;
394600100209               $EoF = *on;
394700100209               leave;
394800100209             ENDIF;
394900111123
395000111123         //?commerciale valido data inizio e fine attività
395100130808             IF  CMMdtIni > woggi or CMMdtFin < woggi;
395200111123               ErrMessage  = *on;
395300111123               ErrGenerico = *on;
395400111123               PosCurCMM   = *on;
395500111123               V02msg      = $Msg(08);
395600111123               V02Rcd      = S02Nrr;
395700111123               update MK17S02;
395800111123               $EoF = *on;
395900111123               leave;
396000111123             ENDIF;
396100100310
396200100310         //?controllo se attività può essere eseguita dal commerciale interno
396300130808             IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
396400100310               SELECT;
396500100310                 WHEN  VH2fint = *blanks and §CCOcoi = 'F';
396600100310                   ErrMessage  = *on;
396700100310                   ErrGenerico = *on;
396800100310                   PosCurCMM   = *on;
396900100310                   V02msg      = $Msg(15);
397000100310                   V02msg      = %trim(V02msg) + ' Enter per forzare';
397100100310                   VH2fint     = '1';
397200100310                   V02Rcd      = S02Nrr;
397300100310                   update MK17S02;
397400100310                   $EoF = *on;
397500100310                   leave;
397600100310                 WHEN  §CCOcoi = 'N';
397700100310                   ErrMessage  = *on;
397800100310                   ErrGenerico = *on;
397900100310                   PosCurCMM   = *on;
398000100310                   V02msg      = $Msg(15);
398100100310                   V02Rcd      = S02Nrr;
398200100310                   update MK17S02;
398300100310                   $EoF = *on;
398400100310                   leave;
398500100310             ENDSL;
398600100310           ENDIF;
398700091229
398800091230         //?Commerciale  -  solo unificante
398900130808             IF  %dec(V02cmm:7:0) <> CMMuni;
399000100209               ErrMessage  = *on;
399100100209               ErrGenerico = *on;
399200100209               PosCurCMM   = *on;
399300100210               V02msg      = $Msg(14);
399400100209               V02Rcd      = S02Nrr;
399500100209               update MK17S02;
399600100209               $EoF = *on;
399700100209               leave;
399800100209             ENDIF;
399900100208
400000100205         //?utente abilitato al commerciale
400100100209             clear TNTAA1DS;
400200120810             ITAA1tipo = 'M';
400300120810             ITAA1caut = '§utegtc';
400400100209             ITAA1cmm  = %dec(V02cmm:7:0);
400500100521             ITAA1abi = 'RA';
400600100209             kpjbu     = tntaa1ds;
400700100209             tntaa1c (kpjba);
400800100209             TNTAA1DS = kpjbu;
400900100209             IF  OTAA1fabi = 'N';
401000100209               ErrMessage  = *on;
401100100209               ErrGenerico = *on;
401200100209               PosCurCMM   = *on;
401300100209               V02msg      = $Msg(08);
401400100209               V02Rcd      = S02Nrr;
401500100209               update MK17S02;
401600100209               $EoF = *on;
401700100209               leave;
401800100209             ENDIF;
401900100209           ENDIF;
402000091230
402100091230         //?Incremento la schiera di comodo commerciali
402200100114           xx = %lookup(%dec(V02cmm:7:0) : $AGE);
402300100114           IF  xx > 0;
402400100114             ErrMessage  = *on;
402500100114             ErrGenerico = *on;
402600100114             PosCurCMM   = *on;
402700100114             V02msg      = $Msg(13);
402800100114             V02Rcd      = S02Nrr;
402900100114             update MK17S02;
403000100114             $EoF = *on;
403100100114             leave;
403200100114           ENDIF;
403300091230           $AGE(S02Nrr) = %dec(V02cmm:7:0);
403400091229
403500091230         //?Verifico se il commerciale è libero
403600091230           clear trmk84ds;
403700091230           IMK84cmm = %dec(V02cmm:7:0);
403800091230           IMK84dad = wdad;
403900091230           IMK84oii = V02oii;
404000091230           IMK84ofi = V02ofi;
404100091230           trmk84r (trmk84ds);
404200091230         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
404300091230           IF  OMK84err <> *blanks and OMK84err <> '9' and
404400100310               VH2fimp = *blanks;
404500091230             ErrGenerico = *on;
404600091230             ErrMessage  = *on;
404700091230             PosCurCMM   = *on;
404800091230             V02msg = OMK84msg;
404900091230             V02msg = %trim(V02msg) + '. Enter per forzare.';
405000091230             V02Rcd      = S02Nrr;
405100100310             VH2fimp = '1';
405200091230             update MK17S02;
405300091230             $EoF = *on;
405400091230             leave;
405500091230           ENDIF;
405600091230
405700091230           V02Rcd = S02Nrr;
405800091230           update MK17S02;
405900091230         ENDDO;
406000091230
406100100203         IF  ErrGenerico;
406200100203           leavesr;
406300100203         ENDIF;
406400100203
406500091230         //?Controllo se ho caricato almeno un commerciale
406600100203         xx = 1;
406700100203         $NoAge = *on;
406800100203         FOR xx by 1 to %elem($Age);
406900100203           IF  $age(xx) > 0;
407000100203             $NoAge = *off;
407100100203           ENDIF;
407200100203         ENDFOR;
407300091230
407400091230         //?Se non ho caricato commerciali errore
407500100203         IF  $NoAge;
407600100203           ErrMessage  = *on;
407700100203           ErrGenerico = *on;
407800100203           V02msg      = $Msg(12);
407900100203         ENDIF;
408000091229
408100091229       ENDSR;
408200091230
408300091230       //--------------------------------------------------------------
408400091230       //?Gestione tasto funzionale F06 da videata C02.
408500091230       //?F06=Conferma
408600091230       //--------------------------------------------------------------
408700091230       BEGSR F06C02;
408800091230
408900091230       //?Scrivo nuova attività
409000091230       //?1 rcd per ogni commerciale inserito a video
409100091230         $Eof = *off;
409200091230         clear S02Nrr;
409300091230
409400091230         //?Leggo subfile
409500091230         DOW  not $EoF;
409600091230           S02Nrr += 1;
409700091230           chain S02Nrr MK17S02;
409800091230           IF  not %found;
409900091230             $EoF = *on;
410000091230             leave;
410100091230           ENDIF;
410200091230           IF  V02cmm = *blanks;
410300091230             iter;
410400091230           ENDIF;
410500100929
410600100929         //?Stacco nuovo numero attività
410700100929           exsr NewAttivita;
410800100929         //?se errore vado subito a fine
410900100929           IF  O33Err <> 0;
411000100929             $Fine = *on;
411100100929             ErrGenerico = *on;
411200100929             V02msg = O33msg;
411300100929             leavesr;
411400100929           ENDIF;
411500091230
411600091230           clear TIATC000;
411700091230           atctat  = V02att;
411800091230           atcatn  = O33Nrf;
411900100929           atcatnp = 1;
412000091230           atccpo  = IMK17cpo;
412100091230           atcksc  = IMK17ksc;
412200091230           atcnrv  = IMK17nrv;
412300091230           atccad  = V02cau;
412400091230           atcdad  = wdad;
412500100108           atchda  = V02oii * 100;
412600091230           atccmm  = %dec(V02cmm:7:0);
412700091230           atcoii  = V02oii;
412800091230           atcofi  = V02ofi;
412900091230           atcdim  = %dec(%date());
413000091230           atchim  = %dec(%time());
413100091230           atcpri  = knmus ;
413200091230
413300091230           write TIATC000;
413400091230           feod  TIATC01L;
413500100929
413600100929         //?Se ci sono note le registro
413700100929           clear TRMK20ds;
413800100929           IMK20flm  = 'C';
413900100929           IMK20cpo  = atccpo;
414000100929           IMK20ksc  = atcksc;
414100100929           IMK20nrv  = atcnrv;
414200100929           IMK20tat  = atctat;
414300100929           IMK20atn  = atcatn;
414400100929           IMK20atnp = 0;
414500100929           trmk20r (kpjba : TRMK20ds);
414600091230
414700091230         ENDDO;
414800091230
414900091230       //?Ritorno al programma chiamante
415000091230         $Fine   = *on ;
415100091230
415200091230       ENDSR;
415300091229
415400091229       //--------------------------------------------------------------
415500091229       //?Gestione tasto funzionale F11 da videata DP2/DT2.
415600091229       //?F11=Agenda
415700091229       //--------------------------------------------------------------
415800091229       BEGSR F11D02;
415900091229
416000091229         clear TRMK82ds;
416100091229         IMK82rich = 'A';
416200091230
416300091230       //?Imposto il commerciale se inserito (può succedere nel caso di riunione commerciale)
416400091230         IF  V02cmm <> *blanks and V02cmm > *zeros;
416500091230           IMK82cage = %dec(V02cmm:7:0);
416600091230         ENDIF;
416700091229
416800091229       //?Imposto la data di work
416900091229         IF  wdad > 0;
417000091229           IMK82data = wdad;
417100091229         ENDIF;
417200091229
417300091229         kpjbu = trmk82ds;
417400091229         trmk82c (kpjba);
417500091229
417600091229       ENDSR;
417700100112
417800100112       //--------------------------------------------------------------
417900100112       //?Gestione tasto funzionale F12 da videata DP2/DT2.
418000100112       //?F12=Ritorno
418100100112       //--------------------------------------------------------------
418200100112       BEGSR F12D02;
418300100112
418400100112         $Video = 'T2';
418500100112         $InzT02 = *on;
418600100112
418700100112       ENDSR;
418800091223
418900091223       //--------------------------------------------------------------
419000091223       //?Controllo se giorno festivo.
419100091223       //--------------------------------------------------------------
419200091223       BEGSR CtrFesta;
419300091223
419400091223         $Festa = *off;
419500091223
419600091229         clear k_clntfa;
419700091229         k_clntfp = w0030;
419800091223         k_clnann = wanno;
419900091223         k_clnmes = wmese;
420000091223         chain %kds(K04azcln) AZCLN01L;
420100091223         IF  %found(AZCLN01L);
420200091223           IF  mat(wgiorno) = 'F'  or
420300091223               pom(wgiorno) = 'F';
420400091223             $Festa = *on;
420500091223           ENDIF;
420600091223         ENDIF;
420700091223
420800091223       ENDSR;
420900091223
421000091223       //--------------------------------------------------------------
421100091223       //?Stacco nuovo numero attività.
421200091223       //--------------------------------------------------------------
421300091223       BEGSR NewAttivita;
421400091223
421500091223         clear TRUL33ds;
421600091223         I33tla = 'L';
421700091223         I33ope = 0;
421800091223         I33cnu = 070;
421900091223         I33num = 1;
422000091223         I33aaa = *YEAR;
422100091223         kpjbu  = TRUL33ds;
422200091223         trul33r (kpjba);
422300091223         TRUL33ds = kpjbu;
422400091223
422500091223       ENDSR;
422600091223
422700091223       //--------------------------------------------------------------
422800091223       //?Richiamo il programma di inserimento affiancamento.
422900091223       //--------------------------------------------------------------
423000091223       BEGSR Ric_Affianca;
423100091223
423200091223         clear TRMK22ds;
423300091230         IMK22fcmt = IMK17fcmt;
423400091230         IMK22cmt  = IMK17cmt;
423500091230         IMK22fle  = 'I';
423600091230         IMK22cmm  = atccmm;
423700091230         IMK22dad  = atcdad;
423800091230         IMK22hda  = atchda;
423900091230         IMK22oii  = atcoii;
424000091230         IMK22ofi  = atcofi;
424100091230         IMK22tat  = atctat;
424200091230         IMK22atn  = atcatn;
424300091230         IMK22atnp = atcatnp;
424400091223
424500091223         trmk22r (kpjba : TRMK22ds);
424600091223
424700091223         //?Se ritorna errore lo visualizzo e faccio rollback
424800091223         IF  OMK22err <> *blanks;
424900091223           ErrMessage  = *on;
425000091223           ErrGenerico = *on;
425100091223           V01msg = OMK22msg;
425200091223           IF  IMK17cmt = 'S';
425300091223             rolbk;
425400091223           ENDIF;
425500091223           leavesr;
425600091223         ENDIF;
425700091223
425800091223         //?Se F12 da gestione affiancamento rollback
425900091223         IF  IMK17cmt = 'S' and OMK22F12 = 'S';
426000091223           rolbk;
426100091223         ENDIF;
426200091223
426300091223         //?Se non ci sono errori e non è stato premuto F12
426400091223         //?confermo tutto
426500091230         IF  IMK17cmt = 'S' and OMK22err = *blanks and OMK22f12 = *blanks;
426600091223           commit;
426700091223         ENDIF;
426800091223
426900091223       ENDSR;
427000101215
427100101215       //--------------------------------------------------------------
427200101215       //?Richiamo il programma di gestione data affidamento merce.
427300101215       //--------------------------------------------------------------
427400101215       BEGSR Ric_PgmHot;
427500101215
427600101215         clear TNTA63ds;
427700101215         ITA63fcmt = IMK17fcmt;
427800101215         ITA63cmt  = IMK17cmt;
427900101215         ITA63fmt  = '3';
428000110118         ITA63fle  = 'I';
428100101215         ITA63nrv  = IMK17nrv;
428200101215
428300101215         tnta63r (kpjba : TNTA63ds);
428400101215
428500101215       ENDSR;
428600091221
428700091221       //--------------------------------------------------------------
428800091221       //?Operazioni finali.
428900091221       //--------------------------------------------------------------
429000091221       BEGSR RoutEnd;
429100091221
429200091221?        //?Ritorno errore
429300091221         IF  ErrGenerico;
429400091221           OMK17err = 'E';
429500091221           OMK17msg = V01msg;
429600091221         ENDIF;
429700091221
429800091221?        //?Comitto se richiesto e se non ci sono errori
429900091221         IF  IMK17cmt = 'S' and OMK17err = ' ';
430000091221           commit;
430100091221         ENDIF;
430200091221
430300091221         IF  IMK17cmt = 'S' and OMK17err <> ' ';
430400091221           rolbk;
430500091221         ENDIF;
430600100929
430700100929?      //?Richiamo TRMK20R per chiusura dati
430800100929         clear TRMK20ds;
430900100929         IMK20tla  = 'C';
431000100929         trmk20r (kpjba : TRMK20ds);
431100091221
431200091221         *inLR = *on;
431300091221         return;
431400091221
431500091221       ENDSR;
431600091221
431700091221      /end-free
431800091221       //--------------------------------------------------------------
431900091221       //?Schiere a tempo di compilazione.
432000091221       //--------------------------------------------------------------
432100091221
432200091221** - $MSG -------------------------------------------------------------------*
432300091221Utente non abilitato alla funzione                                             01
432400091221Dati commit non specificati. Avvisare CED!                                     02
432500091223Incongruenza tra campi passati e protezione campi. Avvisare CED!               03
432600091223Tipo attività errata                                                           04
432700091223Causale attività errata                                                        05
432800091223Data errata                                                                    06
432900091223Ora errata                                                                     07
433000100208Commerciale errato o non in gestione all'utente                                08
433100091223Ora inizio impegno incongruente con ora fine impegno                           09
433200091223Ore inizio e fine impegno incongruenti con ora appuntamento                    10
433300091229Data inizio attività incongruente con data fine attività                       11
433400091230Immettere almeno un commerciale                                                12
433500100114Commerciale già inserito                                                       13
433600100210Commerciale errato. Immettere solo commerciali UNIFICANTI!                     14
433700100310Attività non possibile per commerciale interno.                                15
433800110502Causale NON utilizzabile per potenziale in categoria xxxxxxxxxxxxxxxxxxxx      16
433900110505Fil. del codice commerciale incongruente con la fil. del cliente               17
434000100310Causale non utilizzabile legata ad una trattativa                              18
434100100311Immettere le note                                                              19
434200100322Causale utilizzabile solo se legata ad una trattativa                          20
434300100322Causale non utilizzabile in presenza di trattativa senza offerta               21
434400100322Causale non utilizzabile in quanto attività legata a trattativa con offerta    22
434500100331Non trovati il Nome e/o indirizzo e-mail Resp.Trasporti. F08 non possibile.    23
434600110113Causale non utilizzabile per trattativa di tipo                                24
434700110307Causale blocco cliente errata                                                  25
434800140227Periodo di ferie  SUPERIORE  a 3 settimane - "Invio" per continuare            26
