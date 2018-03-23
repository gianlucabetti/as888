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
148100091223         IF  V01dad = 0;
148200091222           ErrMessage  = *on;
148300091222           ErrGenerico = *on;
148400091223           PosCurDAD   = *on;
148500091223           V01msg      = $Msg(06);
148600091222           leavesr;
148700091222         ENDIF;
148800091222
148900091222       //?Data attività  -  controllo
149000091222         clear wlbdat;
149100091223         G02dat = V01dad;
149200091222         xsrda8(wlbdat);
149300091222         IF  G02err = '1';
149400091222           ErrMessage  = *on;
149500091222           ErrGenerico = *on;
149600091223           PosCurDAD   = *on;
149700091223           V01msg      = $Msg(06);
149800091222           leavesr;
149900091222         ENDIF;
150000091222
150100091223         V01dad = G02dat;
150200091223         wdad   = G02inv;
150300100322
150400100322       //?controllo che la data non sia inferiore a 3 gg lavorativi da oggi
150500100322         clear XGIOLAVDS;
150600100322         IXGLdata = %dec(%date());
150700100322         IXGLsub  = 'S';
150800100322         IXGLgg   = 3;
150900100322         IXGLpa   = 'P';
151000100322         IXGLlav  = 'S';
151100100322         IXGLfil = %dec(%subst(V01cmm:1:3):3:0);
151200100322         XGIOLAV (xgiolavds);
151300100322         IF  wdad < OXGLdata;
151400100322           ErrMessage  = *on;
151500100322           ErrGenerico = *on;
151600100322           PosCurDAD   = *on;
151700100322           V01msg      = $Msg(06);
151800100322           V01msg      = %trim(V01msg) + ' ' +
151900100322           'Non può essere inferiore a 3 gg.lavorativi';
152000100322           leavesr;
152100100322         ENDIF;
152200100322
152300100322       //?controllo che la data non maggiore di 14 mesi da oggi
152400100322         clear XGIOLAVDS;
152500100322         IXGLdata = %dec(%date());
152600100322         IXGLadd  = 'S';
152700100322         IXGLmm   = 14;
152800100322         IXGLpa   = 'P';
152900100322         IXGLfil = %dec(%subst(V01cmm:1:3):3:0);
153000100322         XGIOLAV (xgiolavds);
153100100322         IF  wdad > OXGLdata;
153200100322           ErrMessage  = *on;
153300100322           ErrGenerico = *on;
153400100322           PosCurDAD   = *on;
153500100322           V01msg      = $Msg(06);
153600100322           V01msg      = %trim(V01msg) + ' ' +
153700100322           'Non può essere maggiore di 14 mesi';
153800100322           leavesr;
153900100322         ENDIF;
154000100119
154100100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
154200100119         IF  wdad < 20000101 or wdad > 20391231;
154300100119           ErrMessage  = *on;
154400100119           ErrGenerico = *on;
154500100119           PosCurDAD   = *on;
154600100119           V01msg      = $Msg(06);
154700100119           leavesr;
154800100119         ENDIF;
154900100119
155000091223         data_iso = (%date(wdad));
155100091223
155200091223       //?Data attività  -  no festivo
155300091223         w0030   = %dec(%subst(V01cmm:1:3):3:0);
155400091223         wanno   = %subdt(data_ISO:*years);
155500091223         wmese   = %subdt(data_ISO:*months);
155600091223         wgiorno = %subdt(data_ISO:*days);
155700100420         // exsr CtrFesta;
155800091223         IF  $Festa;
155900091223           ErrMessage  = *on;
156000091223           ErrGenerico = *on;
156100091223           PosCurDAD   = *on;
156200091228           V01msg      = $Msg(06);
156300091223           V01msg      = %trim(V01msg) + '. Immesso giorno festivo!';
156400091223           leavesr;
156500091223         ENDIF;
156600091222
156700091222       //?Ora attività  -  obbligatoria
156800091223         IF  V01hda = 0;
156900091222           ErrMessage  = *on;
157000091222           ErrGenerico = *on;
157100091223           PosCurHDA   = *on;
157200091223           V01msg      = $Msg(07);
157300091222           leavesr;
157400091222         ENDIF;
157500091222
157600091222       //?Ora attività  -  controllo
157700120104         IF  %dec(%subst(%editc(V01hda:'X'):1:2):2:0) > 23 or
157800091223             %dec(%subst(%editc(V01hda:'X'):3:2):2:0) > 59;
157900091222           ErrMessage  = *on;
158000091222           ErrGenerico = *on;
158100091223           PosCurHDA   = *on;
158200091223           V01msg      = $Msg(07);
158300091222           leavesr;
158400091222         ENDIF;
158500091222
158600091222       //?Se APPUNTAMENTO
158700091222         IF  Appuntamento;
158800091222
158900091222         //?Verifico se il commerciale dell'appuntamento è diverso dal
159000091222         //?commerciale della trattativa
159100091222           clear tnta45ds ;
159200091222           ITA45tip = 'C';
159300091222           ITA45nrv = V01nrv;
159400091223           ITA45cmm = %dec(V01cmm:7:0);
159500091222           tnta45r (tnta45ds) ;
159600100310           IF  OTA45err = 'C' and not $ForzaCMM;
159700091222             ErrMessage  = *on;
159800091222             ErrGenerico = *on;
159900091223             PosCurCMM   = *on;
160000091222             V01msg = OTA45msg;
160100100310             V01msg = %trim(V01msg) + ' Enter per forzare';
160200100119             $ForzaCMM = *on;
160300100310             sav_V01cmm = V01cmm;
160400091222             leavesr;
160500091222           ENDIF;
160600091222
160700091222         //?Impegno  ORA inizio  -  obbligatorio
160800091223           IF  V01oii = 0;
160900091222             ErrMessage  = *on;
161000091222             ErrGenerico = *on;
161100091223             PosCurOII   = *on;
161200091223             V01msg      = $Msg(07);
161300091222             leavesr;
161400091222           ENDIF;
161500091222
161600091222       //?Impegno  ORA inizio  -  controllo
161700120104           IF  %dec(%subst(%editc(V01oii:'X'):1:2):2:0) > 23 or
161800091223               %dec(%subst(%editc(V01oii:'X'):3:2):2:0) > 59;
161900091222             ErrMessage  = *on;
162000091222             ErrGenerico = *on;
162100091223             PosCurOII   = *on;
162200091223             V01msg      = $Msg(07);
162300091222             leavesr;
162400091222           ENDIF;
162500091222
162600091222       //?Impegno  ORA fine    -  obbligatorio
162700091223           IF  V01ofi = 0;
162800091222             ErrMessage  = *on;
162900091222             ErrGenerico = *on;
163000091223             PosCurOFI   = *on;
163100091223             V01msg      = $Msg(07);
163200091222             leavesr;
163300091222           ENDIF;
163400091222
163500091222       //?Impegno  ORA fine    -  controllo
163600120104           IF  %dec(%subst(%editc(V01ofi:'X'):1:2):2:0) > 23 or
163700091223               %dec(%subst(%editc(V01ofi:'X'):3:2):2:0) > 59;
163800091222             ErrMessage  = *on;
163900091222             ErrGenerico = *on;
164000091223             PosCurOFI   = *on;
164100091223             V01msg      = $Msg(07);
164200091222             leavesr;
164300091222           ENDIF;
164400091222
164500091222       //?Impegno  ORA inizio  -  congruente con ORA fine
164600091223           IF  V01oii > 0  and  V01ofi = 0;
164700091222             ErrMessage  = *on;
164800091222             ErrGenerico = *on;
164900091223             PosCurOFI   = *on;
165000091223             V01msg      = $Msg(09);
165100091222             leavesr;
165200091222           ENDIF;
165300091223           IF  V01oii > 0  and  V01ofi > 0  and  V01oii > V01ofi ;
165400091222             ErrMessage  = *on;
165500091222             ErrGenerico = *on;
165600091223             PosCurOFI   = *on;
165700091223             V01msg      = $Msg(09);
165800091222             leavesr;
165900091222           ENDIF;
166000091222
166100091222         //?Impegno  ORA inizio e fine  -  congruenti con ORA attività
166200091223           IF  V01oii > 0  and  V01ofi > 0  and
166300091223              (V01oii > V01hda  or  V01ofi < V01hda);
166400091222             ErrMessage  = *on;
166500091222             ErrGenerico = *on;
166600091223             PosCurOFI   = *on;
166700091223             V01msg      = $Msg(10);
166800091222             leavesr;
166900091222           ENDIF;
167000091222
167100091222         //?Verifico se il commerciale è libero
167200091222           clear trmk84ds;
167300091223           IMK84cmm = %dec(V01cmm:7:0);
167400091223           IMK84dad = wdad;
167500091223           IMK84oii = V01oii;
167600091223           IMK84ofi = V01ofi;
167700091222           trmk84r (trmk84ds);
167800091222         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
167900091222           IF  OMK84err <> *blanks and OMK84err <> '9' and
168000091222               Not $ForzaImpegno;
168100091222             ErrGenerico = *on;
168200091222             ErrMessage  = *on;
168300091223             PosCurCMM   = *on;
168400091223             V01msg = OMK84msg;
168500091229             V01msg = %trim(V01msg) + '. Enter per forzare.';
168600091222             $ForzaImpegno = *on;
168700091222             leavesr;
168800091222           ENDIF;
168900091222
169000091222         ENDIF;
169100100125
169200100125       //?Commerciale che inserisce attività -  Ricerca
169300100125         IF  %scan('?' : V01co3) > *zero;
169400100125           ErrGenerico = *on;
169500100125           PosCurCO3   = *on;
169600130808           clear  TRMK43ds;
169700130808           iMK43fil  = DUTpou;
169800130808           TRMK43R (kpjba : TRMK43ds);
169900130808           if  oMK43err = *off  and  oMK43fxx = *blank;
170000130808             V01co3  = %editc( oMK43cmm : 'X' );
170100130808             V01dco3 = oMK43des;
170200130808           endif;
170300100125           leavesr;
170400100125         ENDIF;
170500100125
170600100125       //?Commerciale che inserisce attività  -  Obbligatorio
170700100125         IF  V01co3 = *blanks  or  V01co3 = *zero;
170800100125           ErrMessage  = *on;
170900100125           ErrGenerico = *on;
171000100125           PosCurCO3   = *on;
171100100125           V01msg      = $Msg(08);
171200100125           leavesr;
171300100125         ENDIF;
171400100125
171500100125         IF  %check(digits:V01co3) > 0;
171600100125           ErrMessage  = *on;
171700100125           ErrGenerico = *on;
171800100315           PosCurCO3   = *on;
171900100125           V01msg      = $Msg(08);
172000100125           leavesr;
172100100125         ENDIF;
172200091222
172300091222       //?Commerciale che inserisce attività  -  Controllo
172400130808         CMMcod = %int(V01co3);
172500130808         chain  (CMMcod)  AZCMM000;
172600130808         IF  NOT %found(AZCMM01L);
172700091222           ErrMessage  = *on;
172800091222           ErrGenerico = *on;
172900091223           PosCurCO3   = *on;
173000091223           V01msg      = $Msg(08);
173100091222           leavesr;
173200091222         ENDIF;
173300091222
173400130808         V01dco3 = CMMdes;
173500100209
173600100209         //?commerciale valido no vari o clienti inattivi
173700130808           IF  CMMpar <> *blanks;
173800100209             ErrMessage  = *on;
173900100209             ErrGenerico = *on;
174000100209             PosCurCO3   = *on;
174100100209             V01msg      = $Msg(08);
174200100209             leavesr;
174300100209           ENDIF;
174400111123
174500111123         //?commerciale valido data inizio e fine attività
174600130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
174700111123             ErrMessage  = *on;
174800111123             ErrGenerico = *on;
174900111123             PosCurCO3   = *on;
175000111123             V01msg      = $Msg(08);
175100111123             leavesr;
175200111123           ENDIF;
175300100205
175400100205         //?utente abilitato al commerciale
175500100208         clear TNTAA1DS;
175600120810         ITAA1tipo = 'M';
175700120810         ITAA1caut = '§utegtc';
175800100208         ITAA1cmm  = %dec(V01co3:7:0);
175900100521         ITAA1abi = 'RA';
176000100208         kpjbu     = tntaa1ds;
176100100208         tntaa1c (kpjba);
176200100208         TNTAA1DS = kpjbu;
176300100208         IF  OTAA1fabi = 'N';
176400100205           ErrMessage  = *on;
176500100205           ErrGenerico = *on;
176600100205           PosCurCO3   = *on;
176700100208           V01msg      = $Msg(08);
176800100205           leavesr;
176900100205         ENDIF;
177000110307
177100110307         //?Se blocco cliente
177200110307         IF  Blocco;
177300110323           clear dsBI;
177400110307           //?Richiedo causale blocco obbligatoria
177500110307           IF  V01blc = *blanks;
177600110307             ErrMessage  = *on;
177700110307             ErrGenerico = *on;
177800110307             PosCurBlc   = *on;
177900110307             V01msg      = $Msg(25);
178000110307             leavesr;
178100110307           ENDIF;
178200110307           //?Ricerca causale blocco obbligatoria
178300110307           IF  %scan('?':V01blc) > 0;
178400110307             ErrGenerico = *on;
178500110307             PosCurBlc   = *on;
178600121127             //idTabella = 'BI';
178700121127             //clear Ordinamento;
178800121127             //clear idElemento;
178900121127             //clear TastoUscita;
179000121127             //Tabel_Ricerca (idTabella:Ordinamento:idElemento:TastoUscita);
179100121127             //V01blc = idElemento;
179200121127             clear kpjbu;
179300121127             TRTBBIR (kpjba);
179400121127             V01blc = %subst(kpjbu:1:3);
179500110307           ENDIF;
179600110307           IF  V01blc <> *blanks and V01blc <> *zeros ;
179700110307             //?Ammessi solo numeri
179800110307             IF  %check(digits:V01blc) > 0;
179900110307               ErrMessage  = *on;
180000110307               ErrGenerico = *on;
180100110307               PosCurBlc   = *on;
180200110307               V01msg      = $Msg(25);
180300110307               leavesr;
180400110307             ENDIF;
180500110307             //?Controllo validità
180600110307             k_TBLcod = 'BI';
180700110307             k_TBLkey = V01blc;
180800110307             chain %kds(K03tabel) TABEL00F;
180900110307             IF  not %found(TABEL00F)
181000110307                 or  TBLflg <> *blanks;
181100110307               ErrMessage  = *on;
181200110307               ErrGenerico = *on;
181300110307               PosCurBlc   = *on;
181400110307               V01msg      = $Msg(25);
181500110307               leavesr;
181600110307             ENDIF;
181700110323             dsBI = TBLuni;
181800110323             V01dblc = §BIdes;
181900121127             //?Controllo se causale ancora utilizzabile
182000121127             IF  §BIuso = 'N';
182100121127               ErrMessage  = *on;
182200121127               ErrGenerico = *on;
182300121127               PosCurBlc   = *on;
182400121127               V01msg      = 'Causale non più utilizzabile';
182500121127               leavesr;
182600121127             ENDIF;
182700110307           ENDIF;
182800110307         ENDIF;
182900100311
183000100311         //?richiedo note obbligatorie se la causale attività lo prevede
183100100311         IF  V01nota1 = *blanks and V01nota2 = *blanks and §CCOnot = 'S';
183200110330           PosCurBlc   = *off;
183300100311           ErrMessage  = *on;
183400100311           ErrGenerico = *on;
183500100311           PosCurNO1   = *on;
183600100311           V01msg      = $Msg(19);
183700100311           leavesr;
183800100311         ENDIF;
183900100331
184000100331       //?Se F08 = Conferma + Mail appuntamento
184100100331       //?devo controllare che sia memorizzato il responsabile trasporti e l'indirizzo mail
184200100331       //?del responsabile trasporti
184300100331           IF  dsp_aid = c_F08;
184400100331             exsr CtrRespTra;
184500100331           ENDIF;
184600091221
184700091221       ENDSR;
184800100331
184900100331       //--------------------------------------------------------------
185000100331       //?Controllo responsabile trasporti.
185100100331       //--------------------------------------------------------------
185200100331       BEGSR CtrResptra;
185300100331
185400100331         clear NTCapl;
185500100331         clear NTCnk1;
185600100331         clear NTCnk2;
185700100331         clear wrst;
185800100331         clear wemr;
185900100331
186000100331         SELECT;
186100100331
186200100331       //?Se c'è il cliente recupero la rubrica del cliente
186300100331           WHEN  V01ksc > 0;
186400100331             NTCapl = 'C';
186500100331             NTCnk1 = %editc(DUTkci:'X') + %editc(V01ksc:'X');
186600100331       //?Se c'è la trattativa recupero la rubrica della trattativa
186700100331           WHEN  V01nrv > 0;
186800100331             NTCapl = 'T';
186900100331             NTCnk1 = %editc(DUTkci:'X') + %editc(V01nrv:'X');
187000100331       //?Altrimenti recupero la rubrica del potenziale
187100100331           OTHER;
187200100331             NTCapl = 'P';
187300100331             NTCnk1 = %editc(V01cpo:'X');
187400100331         ENDSL;
187500100331
187600100331       //?Nominativo responsabile trasporti
187700100331         NTCtnt = '05';
187800100331         chain (NTCapl:NTCnk1:Ntcnk2:Ntctnt) TFNTC01L;
187900100331         IF  not %found(TFNTC01L);
188000100331           ErrMessage  = *on;
188100100331           ErrGenerico = *on;
188200100331           V01msg      = $Msg(23);
188300100331           leavesr;
188400100331         ENDIF;
188500100331         wrst = NTCrnt;
188600100331
188700100331       //?E-mail responsabile trasporti
188800100331         NTCtnt = '06';
188900100331         chain (NTCapl:NTCnk1:Ntcnk2:Ntctnt) TFNTC01L;
189000100331         IF  not %found(TFNTC01L);
189100100331           ErrMessage  = *on;
189200100331           ErrGenerico = *on;
189300100331           V01msg      = $Msg(23);
189400100331           leavesr;
189500100331         ENDIF;
189600100331         wemr = NTCrnt;
189700100331
189800100331       ENDSR;
189900100927
190000100927       //--------------------------------------------------------------
190100100927       //?Gestione tasto funzionale F02 da videata D01.
190200100927       //?F02=Rubrica
190300100927       //--------------------------------------------------------------
190400100927       BEGSR F02D01;
190500100927
190600100927         clear TNTA12DS;
190700100927
190800100927         SELECT;
190900100927           WHEN  V01ksc > 0;
191000100927             TA12apl = 'C';
191100100927             TA12ksc = V01ksc;
191200100927             TA12rag = V01drag;
191300100927           WHEN  V01nrv > 0;
191400100927             TA12apl = 'T';
191500100927             TA12nrv = V01nrv;
191600100927           OTHER;
191700100927             TA12apl = 'P';
191800100927             TA12pot = V01cpo;
191900100927             TA12rag = V01dcpo;
192000100927         ENDSL;
192100100927         tnta12R (kpjba:TNTA12ds);
192200100927         IF  TA12err <> ' ';
192300100927           ErrGenerico = *on;
192400100927           ErrMessage  = *on;
192500100927           V01msg      = ta12msg ;
192600100927           leavesr;
192700100927         ENDIF;
192800100927
192900100927       ENDSR;
193000091222
193100091222       //--------------------------------------------------------------
193200091223       //?Gestione tasto funzionale F06 da videata D01.
193300091222       //?F06=Conferma
193400091222       //--------------------------------------------------------------
193500091223       BEGSR F06D01;
193600091223
193700091223       //?Stacco nuovo numero attività
193800091223         exsr NewAttivita;
193900091223       //?se errore vado subito a fine
194000091223         IF  O33Err <> 0;
194100091223           $Fine = *on;
194200091223           ErrGenerico = *on;
194300091223           V01msg = O33msg;
194400091223           leavesr;
194500091223         ENDIF;
194600091223
194700091223       //?Scrivo nuova attività
194800110223         clear dATC01;
194900091223         clear tiatc000;
195000091223         atctat  = V01att;
195100091223         atcatn  = O33Nrf;
195200091223         atcatnp = 1;
195300091223         atccpo  = IMK17cpo;
195400091223         atcksc  = IMK17ksc;
195500110503         atcprg  = V01blc;
195600091223         atcnrv  = IMK17nrv;
195700091223         atccad  = V01cau;
195800091223         atcdad  = wdad;
195900091223         atchda  = V01hda * 100;
196000091223         atccmm  = %dec(V01cmm:7:0);
196100091223         atcoii  = V01oii;
196200091223         atcofi  = V01ofi;
196300091223         atcco3  = %dec(V01co3:7:0);
196400091223         atcdim  = %dec(%date());
196500091223         atchim  = %dec(%time());
196600091223         atcpri  = knmus ;
196700091223
196800100301       //?Verifico se attività CREATA su codificato o potenziale
196900110502         IF  CPOfls = 'C';
197000110502           atccnw = 'C';
197100110502         ELSE;
197200110502           atccnw = 'P';
197300110502         ENDIF;
197400110223
197500110223       //?Memorizzo la categoria del potenziale all'apertura dell'attività
197600110223         §ATCcapo1 = CPOfls;
197700110223         ATCflo = dATC01;
197800091223
197900091223         write TIATC000;
198000091223         feod  TIATC01L;
198100110307
198200110307       //?Scrivo la nota con la causale blocco cliente
198300110314       //?più eventuale nota immessa a video, solo queste perchè ho bloccato F18
198400110314       //?in caso di causale 71 blocco cliente
198500110307         IF  Blocco;
198600110307           clear TRMK20ds;
198700110307           IMK20tla  = 'L';
198800110307           IMK20flm  = 'C';
198900110307           IMK20fno  = 'S';
199000110307           IMK20cpo  = atccpo;
199100110307           IMK20ksc  = atcksc;
199200110307           IMK20nrv  = atcnrv;
199300110307           IMK20tat  = atctat;
199400110307           IMK20atn  = atcatn;
199500110307           IMK20atnp = atcatnp - 1;
199600110307           EMK20no1 = 'Bloccare il cliente con la causale: ' +
199700110407                       V01blc + ' - ' + V01dblc;
199800110314           EMK20no2 = V01nota1 + V01nota2;
199900110307           trmk20r (kpjba : TRMK20ds);
200000110314         ELSE;
200100091223
200200091223       //?Se ci sono note le registro
200300100607         IF  V01nota1 <> *blanks or V01nota2 <> *blanks
200400100607             or V01notah <> *blanks;
200500100311           clear TRMK20ds;
200600100311           IMK20tla  = 'L';
200700100311           IMK20flm  = 'C';
200800100929           IMK20fno  = 'S';
200900100311           IMK20cpo  = atccpo;
201000100311           IMK20ksc  = atcksc;
201100100311           IMK20nrv  = atcnrv;
201200100311           IMK20tat  = atctat;
201300100311           IMK20atn  = atcatn;
201400100311           IMK20atnp = atcatnp - 1;
201500100311           EMK20no1 = V01nota1 + V01nota2;
201600100607           EMK20no2 = V01notah;
201700100311           trmk20r (kpjba : TRMK20ds);
201800100311         ENDIF;
201900110314         ENDIF;
202000091223
202100091223       //?Se APPUNTAMENTO controllo se devo richiamare gestione affiancamento
202200091223         IF  Appuntamento and V01aff = 'S';
202300091223           exsr Ric_Affianca;
202400091223           //?se errore o F12 devo tornare all'appuntamento
202500091223           IF  OMK22f12 = 'S' or OMK22err <> *blanks;
202600091223             leavesr;
202700091223           ENDIF;
202800091223         ENDIF;
202900110412
203000110412       //?Se potenziale in categoria "E" --> Eliminabile
203100110412       //?ricalcolo la categoria del potenziale e se variata l'aggiorno
203200110412         IF  CPOfls = 'E';
203300110412           clear trmk05ds;
203400110412           IMK05cpo = CPOcpo;
203500110412           trmk05r (kpjba : TRMK05DS);
203600110412         //?se categoria variata aggiorno potenziale
203700110412           IF  OMK05err = *blanks and OMK05cat <> CPOfls;
203800110412             chain(e) CPOcpo TNCPO01L;
203900110412             IF  not %error and %found(TNCPO01L);
204000110412               CPOfls = OMK05cat;
204100110412               update TNCPO000;
204200110412             ENDIF;
204300110412           ENDIF;
204400110412         ENDIF;
204500100311
204600100311       //?Se richiamto da stampa trattativa e ci sono attività 80 aperte
204700100311       //?Window di richiesta per annullo attività 80 aperte
204800100315         IF  IMK17stp = 'S' and  $Att80;
204900100311           $InzW01 = *on;
205000100311           $Video  = 'W1';
205100100311           leavesr;
205200100311         ENDIF;
205300110223
205400110223       //?Se attività che lo richiede stampo lettera
205500110407       //?e se a video è stata richiesta la stampa
205600110412         IF  §CCOstl = 'S' and V01stp = 'S';
205700110315           clear TNTA53ds;
205800110315           DTA53ksc = ATCksc;
205900110412           DTA53fil = DUTpou;
206000110315           tnta53r (kpjba:tnta53ds);
206100110223         ENDIF;
206200091222
206300091223       //?Ritorno al programma chiamante
206400100331         IF  dsp_aid = c_F06;
206500100331           $Fine = *on ;
206600100331         ENDIF;
206700091222
206800091222       ENDSR;
206900100331
207000100331       //--------------------------------------------------------------
207100100331       //?Gestione tasto funzionale F08 da videata D01.
207200100331       //?F08=Conferma
207300100331       //--------------------------------------------------------------
207400100331       BEGSR F08D01;
207500100331
207600100331       //?Confermo i dati dell'appuntamento
207700100331         exsr F06D01;
207800100916
207900100916       //?Richiamo pgm per richiesta dati mail + invio mail
208000100916         clear TRMK23ds1;
208100100916         IMK23f12 = 'S';
208200100916         IMK23cma = ATCco3;
208300100916         IMK23cmm = ATCcmm;
208400100916         IMK23dta = ATCdad;
208500100916         IMK23ora = %dec(%subst(%editc(ATChda:'X'):1:4):4:0);
208600100916         IMK23rst = wrst;
208700100916         IMK23emr = wemr;
208800100916         trmk23r1 (kpjba : TRMK23ds1);
208900100331
209000100331       //?Ritorno al programma chiamante
209100100331         $Fine = *on ;
209200100331
209300100331       ENDSR;
209400091222
209500091222       //--------------------------------------------------------------
209600091223       //?Gestione tasto funzionale F11 da videata D01.
209700091222       //?F11=Agenda
209800091222       //--------------------------------------------------------------
209900091223       BEGSR F11D01;
210000091222
210100091229         clear TRMK82ds;
210200100112         IMK82rich = 'A';
210300100929         IF  V01cmm <> *blanks and V02cmm > *zeros;
210400100929           IMK82cage = %dec(V01cmm:7:0);
210500100929         ENDIF;
210600091222         kpjbu = trmk82ds;
210700091222         trmk82r (kpjba);
210800091222
210900091222       ENDSR;
211000091221
211100091221       //--------------------------------------------------------------
211200091223       //?Gestione tasto funzionale F12 da videata D01.
211300091221       //?F12=Ritorno
211400091221       //--------------------------------------------------------------
211500091223       BEGSR F12D01;
211600100114
211700100114       //?Ritorno al programma chiamante se tipo attività e causale protetti
211800100114         IF  IMK17patt = 'S' and IMK17pcau = 'S';
211900100203           OMK17f12 = 'S';
212000100114           $Fine  = *on ;
212100100114           leavesr;
212200100114         ENDIF;
212300091221
212400100108         $Video = 'T1';
212500100108         $InzT01 = *on;
212600091221
212700091221       ENDSR;
212800091221
212900091221       //--------------------------------------------------------------
213000091223       //?Gestione tasto funzionale F18 da videata D01.
213100091221       //?F18=Note
213200091221       //--------------------------------------------------------------
213300091223       BEGSR F18D01;
213400091221
213500100114       //?Richiamo il pgm gestione note TRMK20R
213600091221         clear TRMK20ds ;
213700091221         IMK20flm = 'M';
213800091221         IMK20cpo = IMK17cpo ;
213900091221         IMK20ksc = IMK17ksc ;
214000091221         IMK20nrv = IMK17nrv ;
214100100311         EMK20no1 = V01nota1 + V01nota2;
214200100607         EMK20no2 = V01notah;
214300091221         trmk20r ( kpjba : trmk20ds );
214400100311         V01nota1 = %subst(EMK20no1:1:45);
214500100311         V01nota2 = %subst(EMK20no1:46:45);
214600100607         V01notah = EMK20no2;
214700091221
214800091221       ENDSR;
214900100311
215000100311       //--------------------------------------------------------------
215100100311       //?Gestione videata W01 - Richiesta annullo attività 80.
215200100311       //--------------------------------------------------------------
215300100311       BEGSR GesW01;
215400100311
215500100311         // Inizializzazione videata
215600100311         if $InzW01 = *on;
215700100311           exsr InzW01;
215800100311           $InzW01 = *off;
215900100311         endif;
216000100311
216100100311       //?Emissione videata
216200100311         exfmt MK17W01;
216300100311         errMessage   = *off;
216400100311         errGenerico  = *off;
216500100311         clear W01msg;
216600100311
216700100311         SELECT;
216800100311
216900100311       //?F06 = Conferma
217000100311           WHEN dsp_aid = c_F06;
217100100311             exsr F06W01;
217200100311             leavesr ;
217300100311
217400100311       //?Enter
217500100311           OTHER;
217600100311       //?Controllo i dati
217700100311             exsr CtrW01;
217800100311             IF  ErrGenerico;
217900100311               leavesr;
218000100311             ENDIF;
218100100311         ENDSL;
218200100311
218300100311       ENDSR;
218400100311
218500100311       //--------------------------------------------------------------
218600100311       //?Caricamento videata W01.
218700100311       //--------------------------------------------------------------
218800100311       BEGSR InzW01;
218900100311
219000100311         clear MK17W01;
219100100311
219200100311       //?Causale 80 + decodifica
219300100311         clear TIBS02ds;
219400100311         clear dCCO;
219500100311         T02mod = 'C';
219600100311         T02cod = 'CCO';
219700100311         T02ke1 = '80';
219800100311         T02sif = KNSIF;
219900100311         TNTBE_RicercaControllo (kpjba : tibs02ds);
220000100311         IF  T02err = *blanks;
220100100311           dCCO = T02uni;
220200100311         ENDIF;
220300100311         W01att = '80 - ' + §CCOdes;
220400100311         W01att = %trim(W01att) + ' aperte ?';
220500100524         W01SINO = ' ';
220600100311
220700100311       ENDSR;
220800100311
220900100311       //--------------------------------------------------------------
221000100311       //?Controllo videata W01.
221100100311       //--------------------------------------------------------------
221200100311       BEGSR CtrW01;
221300100311
221400100311         WindDspF = IndDspF;
221500100311         reset WindDspFb;
221600100311         IndDspF  = WindDspF;
221700100311
221800100311       //?SI/NO
221900100311         IF  W01sino = *blanks;
222000100311           ErrMessage  = *on;
222100100311           ErrGenerico = *on;
222200100311           W01msg      = 'Effettuare una scelta';
222300100311           leavesr;
222400100311         ENDIF;
222500100311
222600100311       ENDSR;
222700100311
222800100311       //--------------------------------------------------------------
222900100311       //?Gestione tasto funzionale F06 da videata W01.
223000100311       //?F06=Conferma
223100100311       //--------------------------------------------------------------
223200100311       BEGSR F06W01;
223300100311
223400100311       //?E' stato scelto di annullare le attività 80 aperte
223500100524         IF  W01sino = 'S';
223600100322           setll (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
223700100322           reade (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
223800100311           DOW not %eof(TIATC04L);
223900100311             IF  ATCcad = '80';
224000100311       //?Se attività 80 con progressivo 1 devo cancellare anche le relative note
224100100311               IF  ATCatnp = 1;
224200100311                 exsr sr_DelNote80;
224300100311               ENDIF;
224400100311               delete TIATC04;
224500100311             ELSE;
224600100311               update TIATC04;
224700100311             ENDIF;
224800100322             reade (IMK17cpo:IMK17ksc:IMK17nrv:0) TIATC04L;
224900100311           ENDDO;
225000100311         ENDIF;
225100100311
225200100311       //?Ritorno al programma chiamante
225300100315         $Fine = *on ;
225400100311
225500100311       ENDSR;
225600100311
225700100311       //--------------------------------------------------------------
225800100311       //?Cancello note relative ad attività 80 progressivo 1.
225900100311       //--------------------------------------------------------------
226000100311       BEGSR sr_DelNote80;
226100100311
226200100311         CPNcpo  = ATCcpo;
226300100311         CPNtat  = ATCtat;
226400100311         CPNatn  = ATCatn;
226500100311         CPNatnp = 0;
226600100311         setll (cpncpo:cpntat:cpnatn:cpnatnp) TICPN03L;
226700100311         reade (cpncpo:cpntat:cpnatn:cpnatnp) TICPN03L;
226800100311         DOW not %eof(TICPN03L);
226900100311           delete TICPN000;
227000100311           reade (cpncpo:cpntat:cpnatn:cpnatnp) TICPN03L;
227100100311         ENDDO;
227200100311
227300100311       ENDSR;
227400091223
227500091223       //--------------------------------------------------------------
227600091223       //?Gestione videata T02 - Immissione dati attività ufficio.
227700091223       //--------------------------------------------------------------
227800091223       BEGSR GesT02;
227900091223
228000091223         // Inizializzazione videata
228100091223         if $InzT02 = *on;
228200091223           exsr InzT02;
228300091223           $InzT02 = *off;
228400091223         endif;
228500091223
228600091229       //?Emissione videata
228700091223         exfmt MK17T02;
228800091223         errMessage   = *off;
228900091223         errGenerico  = *off;
229000091223         clear V02msg;
229100091228
229200091228         SELECT;
229300091228
229400091228       //?F12 = Ritorno
229500091228           WHEN dsp_aid = c_F12;
229600100112             exsr F12T01;
229700091228             leavesr ;
229800091223
229900091223       //?Enter
230000091228           OTHER;
230100091223       //?Controllo i dati
230200091228             exsr CtrT02;
230300091228             IF  ErrGenerico;
230400091228               leavesr;
230500091228             ENDIF;
230600091228         ENDSL;
230700091223
230800091223       //?Emetto la videata sucessiva in base al tipo attività
230900091223         SELECT;
231000091223       //?parziale in giornata
231100091229           WHEN  V02cau = 'FP' or V02cau = 'UF';
231200091229             $Video = 'DP';
231300091229             $InzDP2 = *on;
231400091223       //?totale su più giorni
231500091229           WHEN  V02cau = 'FT';
231600091223             $Video = 'DT';
231700091223             $InzDT2 = *on;
231800091229       //?Riunione commerciale in giornata con 1 o più commerciali
231900091223           OTHER;
232000091223             $Video = 'C2';
232100091223             $InzC02 = *on;
232200091223         ENDSL;
232300091223
232400091223       ENDSR;
232500091223
232600091223       //--------------------------------------------------------------
232700091223       //?Caricamento videata T02.
232800091223       //--------------------------------------------------------------
232900091223       BEGSR InzT02;
233000091223
233100091223         clear MK17T02;
233200091223         V02pgm = SDSpgm;
233300091228         V02sut = rsut;
233400091228         V02sif = knsif;
233500091228         V02mus = knmus;
233600091223
233700091223       //?Tipo attività
233800091223         IF  IMK17att <> *blanks;
233900100113           V02att = IMK17att;
234000100113           ProteggiATT = (IMK17patt = 'S');
234100091223           clear TIBS02ds;
234200091223           clear dTTA;
234300091223           T02mod = 'C';
234400091223           T02cod = 'TTA';
234500091223           T02ke1 = V02att;
234600091223           T02sif = KNSIF;
234700091223           TNTBE_RicercaControllo (kpjba : tibs02ds);
234800091223           IF  T02err = *blanks;
234900091223             dTTA = T02uni;
235000091223           ENDIF;
235100100113           V02datt = §TTAdes;
235200091223         ENDIF;
235300091223
235400091223       //?Causale
235500091223         IF  IMK17cau <> *blanks;
235600091223           V02cau = IMK17cau;
235700091223           ProteggiCAU = (IMK17pcau = 'S');
235800091223           clear TIBS02ds;
235900091223           clear dCCO;
236000091223           T02mod = 'C';
236100091223           T02cod = 'CCO';
236200091223           T02ke1 = V02cau;
236300091223           T02sif = KNSIF;
236400091223           TNTBE_RicercaControllo (kpjba : tibs02ds);
236500091223           IF  T02err = *blanks;
236600091223             dCCO = T02uni;
236700091223           ENDIF;
236800091223           V02dcau = §CCOdes;
236900091223         ENDIF;
237000091223
237100091223       ENDSR;
237200091223
237300091223       //--------------------------------------------------------------
237400091223       //?Controllo videata T02.
237500091223       //--------------------------------------------------------------
237600091223       BEGSR CtrT02;
237700091223
237800091223         WindDspF = IndDspF;
237900091223         reset WindDspFb;
238000091223         IndDspF  = WindDspF;
238100100113
238200100113       //?Tipo attività
238300100113         IF  not ProteggiATT;
238400100113           exsr CtrATTT02;
238500100113           IF  ErrGenerico;
238600100113             leavesr;
238700100113           ENDIF;
238800100113         ENDIF;
238900100113
239000100113       //?Causale
239100100113         IF  not ProteggiCAU;
239200100113           exsr CtrCAUT02;
239300100113           IF  ErrGenerico;
239400100113             leavesr;
239500100113           ENDIF;
239600100113         ENDIF;
239700100113
239800100113       ENDSR;
239900100113
240000100113       //--------------------------------------------------------------
240100100113       //?Controllo tipo attività T02.
240200100113       //--------------------------------------------------------------
240300100113       BEGSR CtrATTT02;
240400091223
240500091223       //?Tipo attività  -  Ricerca
240600091223         IF  %scan('?' : V02att) > *zero;
240700091223           ErrGenerico = *on;
240800091223           PosCurATT   = *on;
240900091223           clear dTTA;
241000100504           clear TNTB81DS;
241100100504           ITTAleg = 'N';
241200100504           tntb81R1 (kpjba : TNTB81ds);
241300100504           IF  OTTAerr = '1';
241400100504             ErrMessage  = *on;
241500100504             V02msg = T02msg;
241600100504             leavesr;
241700100504           ENDIF;
241800100504
241900100504             V02att  = OTTAke1;
242000100504             dTTA    = OTTAuni;
242100100504             V02datt = §TTAdes;
242200091223         ENDIF;
242300091223
242400091223       //?Tipo attività  -  Obbligatoria
242500091223         IF  V02att = *blanks ;
242600091223           ErrMessage  = *on;
242700091223           ErrGenerico = *on;
242800091223           PosCurATT   = *on;
242900091223           V02msg      = $Msg(04);
243000091223           leavesr;
243100091223         ENDIF;
243200091223
243300091223       //?Tipo attività  -  Controllo
243400091223         clear TIBS02ds;
243500091223         clear dTTA;
243600091223         T02mod = 'C';
243700091223         T02cod = 'TTA';
243800091223         T02ke1 = V02att;
243900091223         T02sif = KNSIF;
244000091223         TNTBE_RicercaControllo  (kpjba : tibs02ds);
244100091223         IF  T02err = *blanks;
244200091223           dTTA = t02uni  ;
244300091223         ELSE;
244400091223           ErrMessage  = *on;
244500091223           ErrGenerico = *on;
244600091223           PosCurATT   = *on;
244700091223           V02msg      = $Msg(04);
244800091223           leavesr;
244900091223         ENDIF;
245000100113         V02datt = §TTAdes;
245100091223
245200091223       //?Tipo attività  -  Accetto solo attività d'ufficio
245300091223       //?                  da questa videata
245400091223         IF  §TTAleg = 'S';
245500091223           ErrMessage  = *on;
245600091223           ErrGenerico = *on;
245700091223           PosCurATT   = *on;
245800091223           V02msg      = $Msg(04);
245900091228           V02msg      = %trim(V02msg) +
246000091228                         '. Inserita attività legata a cliente.';
246100091223           leavesr;
246200091223         ENDIF;
246300100113
246400100113       ENDSR;
246500100113
246600100113       //--------------------------------------------------------------
246700100113       //?Controllo causale T02.
246800100113       //--------------------------------------------------------------
246900100113       BEGSR CtrCAUT02;
247000091223
247100091223       //?Causale  -  Ricerca
247200091223         IF  %scan('?' : V02cau) > *zero;
247300091223           ErrGenerico = *on;
247400091223           PosCurCAU   = *on;
247500091223           clear dCCO;
247600091223           clear TNTB74ds;
247700091223           ICCOatt = V02att;
247800091223           ICCOuti = 'S';
247900091223           tntb74R  (kpjba : TNTB74ds);
248000091223           IF  OCCOerr = *off;
248100091223             V02cau = OCCOke1;
248200091223             dCCO   = OCCOuni;
248300091223             leavesr;
248400091223           ELSE;
248500091223             ErrMessage  = *on;
248600091223             V02msg = T02msg;
248700091223             leavesr;
248800091223           ENDIF;
248900091223
249000091223           V02dcau = §ccodes;
249100091223         ENDIF;
249200091223
249300091223       //?Causale  -  Obbligatoria
249400091223         IF  V02cau = *blanks ;
249500091223           ErrMessage  = *on;
249600091223           ErrGenerico = *on;
249700091223           PosCurCAU   = *on;
249800091223           V02msg      = $Msg(05);
249900091223           leavesr;
250000091223         ENDIF;
250100091223
250200091223       //?Causale  -  Controllo
250300091223         clear TIBS02ds;
250400091223         clear dCCO;
250500091223         T02mod = 'C';
250600091223         T02cod = 'CCO';
250700091223         T02ke1 = V02cau;
250800091223         T02sif = KNSIF;
250900091223         TNTBE_RicercaControllo (kpjba : tibs02ds);
251000091223         IF  T02err = *blanks;
251100091223           dCCO = t02uni  ;
251200091223         ELSE;
251300091223           ErrMessage  = *on;
251400091223           ErrGenerico = *on;
251500091223           PosCurCAU   = *on;
251600091223           V02msg      = $Msg(05);
251700091223           leavesr;
251800091223         ENDIF;
251900091223
252000091223         V02dcau = §ccodes;
252100091223
252200091223       //?Causale  -  Controllo se in uso agli utenti
252300091223         IF  §CCOuti = 'N';
252400091223           ErrMessage  = *on;
252500091223           ErrGenerico = *on;
252600091223           PosCurCAU   = *on;
252700091223           V02msg      = $Msg(05);
252800091223           leavesr;
252900091223         ENDIF;
253000091223
253100091223       //?Causale  -  Controllo se compatibile con tipo attività
253200091223         IF  §CCOatt <> V02att;
253300091223           ErrMessage  = *on;
253400091223           ErrGenerico = *on;
253500091223           PosCurCAU   = *on;
253600091223           V02msg      = $Msg(05);
253700091223           leavesr;
253800091223         ENDIF;
253900091223
254000091223       ENDSR;
254100091223
254200091223       //--------------------------------------------------------------
254300091223       //?Gestione videata DP2.
254400091223       //--------------------------------------------------------------
254500091223       BEGSR GesDP2;
254600091223
254700091223         // Inizializzazione videata
254800091223         if $InzDP2 = *on;
254900091223           exsr InzDP2;
255000091223           $InzDP2 = *off;
255100091223         endif;
255200091223
255300091229       //?Emissione videata
255400091223         exfmt MK17DP2;
255500091223         errMessage   = *off;
255600091223         errGenerico  = *off;
255700091223         clear V02msg;
255800091223
255900091223         SELECT;
256000091223
256100091223       //?F12 = Ritorno
256200091223           WHEN dsp_aid = c_F12;
256300100112             exsr F12D02;
256400091223             leavesr ;
256500091223
256600091223       //?F11 = Agenda
256700091223           WHEN dsp_aid = c_F11;
256800091229             exsr F11D02;
256900091223             leavesr ;
257000091223
257100091223       //?F18 = Note
257200091223           WHEN dsp_aid = c_F18;
257300091223             exsr F18D01;
257400091223             leavesr ;
257500091223
257600091223       //?F06 = Conferma
257700091223           WHEN dsp_aid = c_F06;
257800091223             //?prima faccio i controlli
257900091223             exsr CtrDP2;
258000091223             IF  ErrGenerico;
258100091223               leavesr;
258200091223             ENDIF;
258300091223             //?tutto ok confermo i dati
258400091223             exsr F06DP2;
258500091223             leavesr ;
258600091223
258700091223       //?Enter
258800091223           OTHER;
258900091223       //?Controllo i dati
259000091223            exsr CtrDP2;
259100091223            IF  ErrGenerico;
259200091223              leavesr;
259300091223            ENDIF;
259400091223
259500091223         ENDSL;
259600091223
259700091223       ENDSR;
259800091223
259900091223       //--------------------------------------------------------------
260000091223       //?Caricamento videata DP2.
260100091223       //--------------------------------------------------------------
260200091223       BEGSR InzDP2;
260300091223
260400091229         clear V02cmm;
260500091229         clear v02dcmm;
260600091229         clear v02dad;
260700091229         clear v02oii;
260800091229         clear v02ofi;
260900091223
261000091223         $ForzaImpegno = *off;
261100100310         $ForzaInt     = *off;
261200100310         clear sav_V01cmm;
261300091223
261400100205       //?Imposto commerciale se passato
261500100208         IF  IMK17cmm > 0 and IMK17pcmm = *blanks;
261600100208       //?ma se non è richiesta la protezione e l'utente non è abilitato al
261700100208       //?commerciale non propongo il dato
261800100208           clear TNTAA1DS;
261900120810           ITAA1tipo = 'M';
262000120810           ITAA1caut = '§utegtc';
262100100208           ITAA1cmm  = IMK17cmm;
262200100521           ITAA1abi = 'RA';
262300100208           kpjbu     = tntaa1ds;
262400100208           tntaa1c (kpjba);
262500100208           TNTAA1DS = kpjbu;
262600100208           IF  OTAA1fabi = 'N';
262700100208             clear IMK17cmm;
262800100208           ENDIF;
262900100205         ENDIF;
263000091223       //?Imposto commerciale se passato
263100091223         IF  IMK17cmm > 0;
263200091223           V02cmm = %editc(IMK17cmm:'X');
263300130808           chain  (iMK17cmm)  AZCMM000;
263400130808           IF  %found(AZCMM01L);
263500130808             V02dcmm = CMMdes;
263600130808           ENDIF;
263700091223         //?proteggo se richiesto
263800091223           ProteggiCMM = (IMK17pcmm = 'S');
263900091223         ENDIF;
264000091223
264100091223       //?Imposto data attività se passata
264200091223         IF  IMK17dad > 0;
264300091223           data_eur = %date(IMK17dad:*iso);
264400091223           V02dad   = %dec(data_eur);
264500091223         //?proteggo se richiesto
264600091223           ProteggiDAD = (IMK17pdad = 'S');
264700091223         ENDIF;
264800091228
264900091229       //?Posiziono il cursore
265000091229         SELECT;
265100091229       //?Sul commerciale se NON passato
265200091229           WHEN IMK17cmm = 0;
265300091229             PosCurCMM = *on;
265400091229       //?Sulla data se commerciale passato
265500091229           WHEN IMK17cmm > 0;
265600091229             PosCurDAD = *on;
265700091229       //?Su ora inizio impegno se passata la data
265800091229           WHEN IMK17dad > 0;
265900091229             PosCurOII = *on;
266000091229         ENDSL;
266100091223
266200091223       ENDSR;
266300091223
266400091223       //--------------------------------------------------------------
266500091223       //?Controllo videata DP2.
266600091223       //--------------------------------------------------------------
266700091223       BEGSR CtrDP2;
266800091223
266900091223         WindDspF = IndDspF;
267000091223         reset WindDspFb;
267100091223         IndDspF  = WindDspF;
267200091228
267300091228       //?Commerciale  -  Ricerca
267400091228         IF  %scan('?' : V02cmm) > *zero;
267500091228           ErrGenerico = *on;
267600091228           PosCurCMM   = *on;
267700130808           clear  TRMK43ds;
267800130808           iMK43solU = 'S';
267900130808           iMK43fil  = DUTpou;
268000130808           TRMK43R (kpjba : TRMK43ds);
268100130808           if  oMK43err = *off  and  oMK43fxx = *blank;
268200130808             V02cmm  = %editc( oMK43cmm : 'X' );
268300130808             V02dcmm = oMK43des;
268400130808           endif;
268500091228           leavesr;
268600091228         ENDIF;
268700091228
268800091228       //?Commerciale  -  Obbligatorio
268900091228         IF  V02cmm = *blanks  or  V02cmm = *zero;
269000091228           ErrMessage  = *on;
269100091228           ErrGenerico = *on;
269200091228           PosCurCMM   = *on;
269300091228           V02msg      = $Msg(08);
269400091228           leavesr;
269500091228         ENDIF;
269600091230
269700091230         IF  %check(digits:V02cmm) > 0;
269800091230           ErrMessage  = *on;
269900091230           ErrGenerico = *on;
270000091230           PosCurCMM   = *on;
270100091230           V02msg      = $Msg(08);
270200091230           leavesr;
270300091230         ENDIF;
270400091228
270500100205       //?Commerciale NON protetto
270600100205         IF  not ProteggiCMM;
270700100310
270800100310       //?Se commerciale diverso dal precedente spengo i flag di forzatura
270900100310            IF  V02cmm <> sav_V01cmm;
271000100310              $ForzaImpegno = *off;
271100100310              $ForzaInt     = *off;
271200100310            ENDIF;
271300100310
271400091228       //?Commerciale  -  Controllo
271500130808           CMMcod = %int(V02cmm);
271600130808           chain  (CMMcod)  AZCMM000;
271700130808           IF  NOT %found(AZCMM01L);
271800100205             ErrMessage  = *on;
271900100205             ErrGenerico = *on;
272000100205             PosCurCMM   = *on;
272100100205             V02msg      = $Msg(08);
272200100205             leavesr;
272300100205           ENDIF;
272400091228
272500130808           V02dcmm = CMMdes;
272600100310           sav_V01cmm = V02cmm;
272700110518
272800110518         //?commerciale valido no vari o clienti inattivi
272900130808           IF  CMMpar <> *blanks;
273000110518             ErrMessage  = *on;
273100110518             ErrGenerico = *on;
273200110518             PosCurCMM   = *on;
273300110518             V02msg      = $Msg(08);
273400110518             leavesr;
273500110518           ENDIF;
273600111123
273700111123         //?commerciale valido data inizio e fine attività
273800130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
273900111123             ErrMessage  = *on;
274000111123             ErrGenerico = *on;
274100111123             PosCurCMM   = *on;
274200111123             V02msg      = $Msg(08);
274300111123             leavesr;
274400111123           ENDIF;
274500100310
274600100310         //?controllo se attività può essere eseguita dal commerciale interno
274700130808           IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
274800100310             SELECT;
274900100310               WHEN  not $ForzaInt and §CCOcoi = 'F';
275000100310                 ErrMessage  = *on;
275100100310                 ErrGenerico = *on;
275200100310                 PosCurCMM   = *on;
275300100310                 V02msg      = $Msg(15);
275400100310                 V02msg      = %trim(V02msg) + ' Enter per forzare';
275500100310                 $ForzaInt = *on;
275600100310                 leavesr;
275700100310               WHEN  §CCOcoi = 'N';
275800100310                 ErrMessage  = *on;
275900100310                 ErrGenerico = *on;
276000100310                 PosCurCMM   = *on;
276100100310                 V02msg      = $Msg(15);
276200100310                 leavesr;
276300100310             ENDSL;
276400100310           ENDIF;
276500091228
276600091228       //?Commerciale  -  solo unificante
276700130808           IF  %dec(V02cmm:7:0) <> CMMuni;
276800100205             ErrMessage  = *on;
276900100205             ErrGenerico = *on;
277000100205             PosCurCMM   = *on;
277100100210             V02msg      = $Msg(14);
277200100205             leavesr;
277300100205           ENDIF;
277400100205
277500100205         //?utente abilitato al commerciale
277600100208           clear TNTAA1DS;
277700120810           ITAA1tipo = 'M';
277800120810           ITAA1caut = '§utegtc';
277900100208           ITAA1cmm  = %dec(V02cmm:7:0);
278000100521           ITAA1abi = 'RA';
278100100208           kpjbu     = tntaa1ds;
278200100208           tntaa1c (kpjba);
278300100208           TNTAA1DS = kpjbu;
278400100208           IF  OTAA1fabi = 'N';
278500100205             ErrMessage  = *on;
278600100205             ErrGenerico = *on;
278700100205             PosCurCMM   = *on;
278800100208             V02msg      = $Msg(08);
278900100205             leavesr;
279000100205           ENDIF;
279100100205         ENDIF;
279200091223
279300091223       //?Data attività  -  obbligatoria
279400091223         IF  V02dad = 0;
279500091223           ErrMessage  = *on;
279600091223           ErrGenerico = *on;
279700091223           PosCurDAD   = *on;
279800091223           V02msg      = $Msg(06);
279900091223           leavesr;
280000091223         ENDIF;
280100091223
280200091223       //?Data attività  -  controllo
280300091223         clear wlbdat;
280400091223         G02dat = V02dad;
280500091223         xsrda8(wlbdat);
280600091223         IF  G02err = '1';
280700091223           ErrMessage  = *on;
280800091223           ErrGenerico = *on;
280900091223           PosCurDAD   = *on;
281000091223           V02msg      = $Msg(06);
281100091223           leavesr;
281200091223         ENDIF;
281300091223
281400091223         V02dad = G02dat;
281500091223         wdad   = G02inv;
281600100119
281700100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
281800100119         IF  wdad < 20000101 or wdad > 20391231;
281900100119           ErrMessage  = *on;
282000100119           ErrGenerico = *on;
282100100119           PosCurDAD   = *on;
282200100119           V02msg      = $Msg(06);
282300100119           leavesr;
282400100119         ENDIF;
282500100119
282600091223         data_iso = (%date(wdad));
282700091223
282800091223       //?Data attività  -  no festivo
282900091223         w0030   = %dec(%subst(V02cmm:1:3):3:0);
283000091223         wanno   = %subdt(data_ISO:*years);
283100091223         wmese   = %subdt(data_ISO:*months);
283200091223         wgiorno = %subdt(data_ISO:*days);
283300100423         exsr CtrFesta;
283400091223         IF  $Festa;
283500091223           ErrMessage  = *on;
283600091223           ErrGenerico = *on;
283700091223           PosCurDAD   = *on;
283800091228           V02msg      = $Msg(06);
283900091223           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
284000091223           leavesr;
284100091223         ENDIF;
284200091223
284300091223       //?Impegno  ORA inizio  -  obbligatorio
284400091223         IF  V02oii = 0;
284500091223           ErrMessage  = *on;
284600091223           ErrGenerico = *on;
284700091223           PosCurOII   = *on;
284800091223           V02msg      = $Msg(07);
284900091223           leavesr;
285000091223         ENDIF;
285100091223
285200091223       //?Impegno  ORA inizio  -  controllo
285300120104         IF  %dec(%subst(%editc(V02oii:'X'):1:2):2:0) > 23 or
285400091228             %dec(%subst(%editc(V02oii:'X'):3:2):2:0) > 59;
285500091223           ErrMessage  = *on;
285600091223           ErrGenerico = *on;
285700091223           PosCurOII   = *on;
285800091223           V02msg      = $Msg(07);
285900091223           leavesr;
286000091223         ENDIF;
286100091223
286200091223       //?Impegno  ORA fine    -  obbligatorio
286300091223         IF  V02ofi = 0;
286400091223           ErrMessage  = *on;
286500091223           ErrGenerico = *on;
286600091223           PosCurOFI   = *on;
286700091223           V02msg      = $Msg(07);
286800091223           leavesr;
286900091223         ENDIF;
287000091223
287100091223       //?Impegno  ORA fine    -  controllo
287200120104         IF  %dec(%subst(%editc(V02ofi:'X'):1:2):2:0) > 23 or
287300091228             %dec(%subst(%editc(V02ofi:'X'):3:2):2:0) > 59;
287400091223           ErrMessage  = *on;
287500091223           ErrGenerico = *on;
287600091223           PosCurOFI   = *on;
287700091223           V02msg      = $Msg(07);
287800091223           leavesr;
287900091223         ENDIF;
288000091223
288100091223       //?Impegno  ORA inizio  -  congruente con ORA fine
288200091223         IF  V02oii > 0  and  V02ofi = 0;
288300091223           ErrMessage  = *on;
288400091223           ErrGenerico = *on;
288500091223           PosCurOFI   = *on;
288600091223           V02msg      = $Msg(09);
288700091223           leavesr;
288800091223         ENDIF;
288900091223         IF  V02oii > 0  and  V02ofi > 0  and  V02oii > V02ofi ;
289000091223           ErrMessage  = *on;
289100091223           ErrGenerico = *on;
289200091223           PosCurOFI   = *on;
289300091223           V02msg      = $Msg(09);
289400091223           leavesr;
289500091223         ENDIF;
289600091223
289700091223       //?Verifico se il commerciale è libero
289800091223         clear trmk84ds;
289900091223         IMK84cmm = %dec(V02cmm:7:0);
290000091223         IMK84dad = wdad;
290100091223         IMK84oii = V02oii;
290200091223         IMK84ofi = V02ofi;
290300091223         trmk84r (trmk84ds);
290400091223       //?se torna errore <> "9" vuol dire che il commerciale è impegnato
290500091223         IF  OMK84err <> *blanks and OMK84err <> '9' and
290600091223             Not $ForzaImpegno;
290700091223           ErrGenerico = *on;
290800091223           ErrMessage  = *on;
290900091223           PosCurCMM   = *on;
291000091223           V02msg = OMK84msg;
291100091229           V02msg = %trim(V02msg) + '. Enter per forzare.';
291200091223           $ForzaImpegno = *on;
291300091223           leavesr;
291400091223         ENDIF;
291500091223
291600091223       ENDSR;
291700091229
291800091229       //--------------------------------------------------------------
291900091229       //?Gestione tasto funzionale F06 da videata DP2.
292000091229       //?F06=Conferma
292100091229       //--------------------------------------------------------------
292200091229       BEGSR F06DP2;
292300091229
292400091229       //?Stacco nuovo numero attività
292500091229         exsr NewAttivita;
292600091229       //?se errore vado subito a fine
292700091229         IF  O33Err <> 0;
292800091229           $Fine = *on;
292900091229           ErrGenerico = *on;
293000091229           V02msg = O33msg;
293100091229           leavesr;
293200091229         ENDIF;
293300091229
293400091229       //?Scrivo nuova attività
293500091229         clear tiatc000;
293600091229         atctat  = V02att;
293700091229         atcatn  = O33Nrf;
293800091229         atcatnp = 1;
293900091229         atccpo  = IMK17cpo;
294000091229         atcksc  = IMK17ksc;
294100091229         atcnrv  = IMK17nrv;
294200091229         atccad  = V02cau;
294300091229         atcdad  = wdad;
294400091229         atchda  = V02oii * 100;
294500091229         atccmm  = %dec(V02cmm:7:0);
294600091229         atcoii  = V02oii;
294700091229         atcofi  = V02ofi;
294800091229         atcdim  = %dec(%date());
294900091229         atchim  = %dec(%time());
295000091229         atcpri  = knmus ;
295100091229
295200091229         write TIATC000;
295300091229         feod  TIATC01L;
295400091229
295500091229       //?Se ci sono note le registro
295600091229         clear TRMK20ds;
295700091229         IMK20flm  = 'C';
295800091229         IMK20cpo  = atccpo;
295900100311         IMK20ksc  = atcksc;
296000100311         IMK20nrv  = atcnrv;
296100091229         IMK20tat  = atctat;
296200091229         IMK20atn  = atcatn;
296300091229         IMK20atnp = atcatnp - 1;
296400091229         trmk20r (kpjba : TRMK20ds);
296500091229
296600091229       //?Ritorno al programma chiamante
296700091229         $Fine   = *on ;
296800091229
296900091229       ENDSR;
297000091229
297100091229       //--------------------------------------------------------------
297200091229       //?Gestione videata DT2.
297300091229       //--------------------------------------------------------------
297400091229       BEGSR GesDT2;
297500091229
297600091229         // Inizializzazione videata
297700091229         if $InzDT2 = *on;
297800091229           exsr InzDT2;
297900091229           $InzDT2 = *off;
298000091229         endif;
298100091229
298200091229       //?Emissione videata
298300091229         exfmt MK17DT2;
298400091229         errMessage   = *off;
298500091229         errGenerico  = *off;
298600091229         clear V02msg;
298700091229
298800091229         SELECT;
298900091229
299000091229       //?F12 = Ritorno
299100091229           WHEN dsp_aid = c_F12;
299200100112             exsr F12D02;
299300091229             leavesr ;
299400091229
299500091229       //?F11 = Agenda
299600091229           WHEN dsp_aid = c_F11;
299700091229             exsr F11D02;
299800091229             leavesr ;
299900091229
300000091229       //?F18 = Note
300100091229           WHEN dsp_aid = c_F18;
300200091229             exsr F18D01;
300300091229             leavesr ;
300400091229
300500091229       //?F06 = Conferma
300600091229           WHEN dsp_aid = c_F06;
300700091229             //?prima faccio i controlli
300800091229             exsr CtrDT2;
300900091229             IF  ErrGenerico;
301000091229               leavesr;
301100091229             ENDIF;
301200091229             //?tutto ok confermo i dati
301300091229             exsr F06DT2;
301400091229             leavesr ;
301500091229
301600091229       //?Enter
301700091229           OTHER;
301800091229       //?Controllo i dati
301900091229            exsr CtrDT2;
302000091229            IF  ErrGenerico;
302100091229              leavesr;
302200091229            ENDIF;
302300091229
302400091229         ENDSL;
302500091229
302600091229       ENDSR;
302700091229
302800091229       //--------------------------------------------------------------
302900091229       //?Caricamento videata DT2.
303000091229       //--------------------------------------------------------------
303100091229       BEGSR InzDT2;
303200091229
303300091229         clear V02cmm;
303400091229         clear v02dcmm;
303500091229         clear v02dadi;
303600091229         clear v02dadf;
303700091229
303800091229         $ForzaImpegno = *off;
303900100310         $ForzaInt     = *off;
304000140227         $ForzaFerie   = *off;
304100100310         clear sav_V01cmm;
304200091229
304300100205       //?Imposto commerciale se passato
304400100208         IF  IMK17cmm > 0 and IMK17pcmm = *blanks;
304500100208       //?ma se non è richiesta la protezione e l'utente non è abilitato al
304600100208       //?commerciale non propongo il dato
304700100208           clear TNTAA1DS;
304800120810           ITAA1tipo = 'M';
304900120810           ITAA1caut = '§utegtc';
305000100208           ITAA1cmm  = IMK17cmm;
305100100521           ITAA1abi = 'RA';
305200100208           kpjbu     = tntaa1ds;
305300100208           tntaa1c (kpjba);
305400100208           TNTAA1DS = kpjbu;
305500100208           IF  OTAA1fabi = 'N';
305600100208             clear IMK17cmm;
305700100208           ENDIF;
305800100205         ENDIF;
305900091229       //?Imposto commerciale se passato
306000091229         IF  IMK17cmm > 0;
306100091229           V02cmm = %editc(IMK17cmm:'X');
306200130808           chain   (iMK17cmm) AZCMM000;
306300130808           IF  %found(AZCMM01L);
306400130808             V02dcmm = CMMdes;
306500091229           ENDIF;
306600091229         //?proteggo se richiesto
306700091229           ProteggiCMM = (IMK17pcmm = 'S');
306800091229         ENDIF;
306900091229
307000091229       //?Imposto data attività se passata
307100091229         IF  IMK17dad > 0;
307200091229           data_eur = %date(IMK17dad:*iso);
307300091229           V02dadi  = %dec(data_eur);
307400091229         //?proteggo se richiesto
307500091229           ProteggiDAD = (IMK17pdad = 'S');
307600091229         ENDIF;
307700091229
307800091229       //?Posiziono il cursore
307900091229         SELECT;
308000091229       //?Sul commerciale se NON passato
308100091229           WHEN IMK17cmm = 0;
308200091229             PosCurCMM = *on;
308300091229       //?Sulla data se commerciale passato
308400091229           WHEN IMK17cmm > 0;
308500091229             PosCurDAD = *on;
308600091229       //?Sulla data fine se passata la data inizio
308700091229           WHEN IMK17dad > 0;
308800091229             PosCurDADF = *on;
308900091229         ENDSL;
309000091229
309100091229       ENDSR;
309200091229
309300091229       //--------------------------------------------------------------
309400091229       //?Controllo videata DT2.
309500091229       //--------------------------------------------------------------
309600091229       BEGSR CtrDT2;
309700091229
309800091229         WindDspF = IndDspF;
309900091229         reset WindDspFb;
310000091229         IndDspF  = WindDspF;
310100091229
310200091229       //?Commerciale  -  Ricerca
310300091229         IF  %scan('?' : V02cmm) > *zero;
310400091229           ErrGenerico = *on;
310500091229           PosCurCMM   = *on;
310600130808           clear  TRMK43ds;
310700130808           iMK43solU = 'S';
310800130808           iMK43fil  = DUTpou;
310900130808           TRMK43R (kpjba : TRMK43ds);
311000130808           if  oMK43err = *off  and  oMK43fxx = *blank;
311100130808             V02cmm  = %editc( oMK43cmm : 'X' );
311200130808             V02dcmm = oMK43des;
311300130808           endif;
311400091229           leavesr;
311500091229         ENDIF;
311600091229
311700091229       //?Commerciale  -  Obbligatorio
311800091229         IF  V02cmm = *blanks  or  V02cmm = *zero;
311900091229           ErrMessage  = *on;
312000091229           ErrGenerico = *on;
312100091229           PosCurCMM   = *on;
312200091229           V02msg      = $Msg(08);
312300091229           leavesr;
312400091229         ENDIF;
312500091230
312600091230         IF  %check(digits:V02cmm) > 0;
312700091230           ErrMessage  = *on;
312800091230           ErrGenerico = *on;
312900091230           PosCurCMM   = *on;
313000091230           V02msg      = $Msg(08);
313100091230           leavesr;
313200091230         ENDIF;
313300091229
313400100205       //?Commerciale NON protetto
313500100205         IF  not ProteggiCMM;
313600100310
313700100310       //?Se commerciale diverso dal precedente spengo i flag di forzatura
313800100310            IF  V02cmm <> sav_V01cmm;
313900100310              $ForzaImpegno = *off;
314000100310              $ForzaInt     = *off;
314100100310            ENDIF;
314200100310
314300091229       //?Commerciale  -  Controllo
314400130808           CMMcod = %int(V02cmm);
314500130808           chain  (CMMcod)  AZCMM000;
314600130808           IF  NOT %found(AZCMM01L);
314700100205             ErrMessage  = *on;
314800100205             ErrGenerico = *on;
314900100205             PosCurCMM   = *on;
315000100205             V02msg      = $Msg(08);
315100100205             leavesr;
315200100205           ENDIF;
315300091229
315400130808           V02dcmm = CMMdes;
315500100310           sav_V01cmm = V02cmm;
315600110518
315700110518         //?commerciale valido no vari o clienti inattivi
315800130808           IF  CMMpar <> *blanks;
315900110518             ErrMessage  = *on;
316000110518             ErrGenerico = *on;
316100110518             PosCurCMM   = *on;
316200110518             V02msg      = $Msg(08);
316300110518             leavesr;
316400110518           ENDIF;
316500111123
316600111123         //?commerciale valido data inizio e fine attività
316700130808           IF  CMMdtIni > woggi or CMMdtFin < woggi;
316800111123             ErrMessage  = *on;
316900111123             ErrGenerico = *on;
317000111123             PosCurCMM   = *on;
317100111123             V02msg      = $Msg(08);
317200111123             leavesr;
317300111123           ENDIF;
317400100310
317500100310         //?controllo se attività può essere eseguita dal commerciale interno
317600130808           IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
317700100310             SELECT;
317800100310               WHEN  not $ForzaInt and §CCOcoi = 'F';
317900100310                 ErrMessage  = *on;
318000100310                 ErrGenerico = *on;
318100100310                 PosCurCMM   = *on;
318200100310                 V02msg      = $Msg(15);
318300100310                 V02msg      = %trim(V02msg) + ' Enter per forzare';
318400100310                 $ForzaInt = *on;
318500100310                 leavesr;
318600100310               WHEN  §CCOcoi = 'N';
318700100310                 ErrMessage  = *on;
318800100310                 ErrGenerico = *on;
318900100310                 PosCurCMM   = *on;
319000100310                 V02msg      = $Msg(15);
319100100310                 leavesr;
319200100310             ENDSL;
319300100310           ENDIF;
319400091229
319500091229       //?Commerciale  -  solo unificante
319600130808           IF  %dec(V02cmm:7:0) <> CMMuni;
319700100205             ErrMessage  = *on;
319800100205             ErrGenerico = *on;
319900100205             PosCurCMM   = *on;
320000100210             V02msg      = $Msg(14);
320100100205             leavesr;
320200100205           ENDIF;
320300100205
320400100205         //?utente abilitato al commerciale
320500100208           clear TNTAA1DS;
320600120810           ITAA1tipo = 'M';
320700120810           ITAA1caut = '§utegtc';
320800100208           ITAA1cmm  = %dec(V02cmm:7:0);
320900100521           ITAA1abi = 'RA';
321000100208           kpjbu     = tntaa1ds;
321100100208           tntaa1c (kpjba);
321200100208           TNTAA1DS = kpjbu;
321300100208           IF  OTAA1fabi = 'N';
321400100205             ErrMessage  = *on;
321500100205             ErrGenerico = *on;
321600100205             PosCurCMM   = *on;
321700100208             V02msg      = $Msg(08);
321800100205             leavesr;
321900100205           ENDIF;
322000100205         ENDIF;
322100091229
322200091229       //?Data inizio attività  -  obbligatoria
322300091229         IF  V02dadi = 0;
322400091229           ErrMessage  = *on;
322500091229           ErrGenerico = *on;
322600091229           PosCurDAD   = *on;
322700091229           V02msg      = $Msg(06);
322800091229           leavesr;
322900091229         ENDIF;
323000091229
323100091229       //?Data inizio attività  -  controllo
323200091229         clear wlbdat;
323300091229         G02dat = V02dadi;
323400091229         xsrda8(wlbdat);
323500091229         IF  G02err = '1';
323600091229           ErrMessage  = *on;
323700091229           ErrGenerico = *on;
323800091229           PosCurDAD   = *on;
323900091229           V02msg      = $Msg(06);
324000091229           leavesr;
324100091229         ENDIF;
324200091229
324300091229         V02dadi  = G02dat;
324400091229         wdad     = G02inv;
324500100119
324600100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
324700100119         IF  wdad < 20000101 or wdad > 20391231;
324800100119           ErrMessage  = *on;
324900100119           ErrGenerico = *on;
325000100119           PosCurDAD   = *on;
325100100119           V02msg      = $Msg(06);
325200100119           leavesr;
325300100119         ENDIF;
325400100119
325500091229         data_iso = (%date(wdad));
325600091229
325700091229       //?Data inizio attività  -  no festivo
325800091229         w0030   = %dec(%subst(V02cmm:1:3):3:0);
325900091229         wanno   = %subdt(data_ISO:*years);
326000091229         wmese   = %subdt(data_ISO:*months);
326100091229         wgiorno = %subdt(data_ISO:*days);
326200100423         exsr CtrFesta;
326300091229         IF  $Festa;
326400091229           ErrMessage  = *on;
326500091229           ErrGenerico = *on;
326600091229           PosCurDAD   = *on;
326700091229           V02msg      = $Msg(06);
326800091229           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
326900091229           leavesr;
327000091229         ENDIF;
327100091229
327200091229       //?Data fine attività  -  obbligatoria
327300091229         IF  V02dadf = 0;
327400091229           ErrMessage  = *on;
327500091229           ErrGenerico = *on;
327600091229           PosCurDADF  = *on;
327700091229           V02msg      = $Msg(06);
327800091229           leavesr;
327900091229         ENDIF;
328000091229
328100100423       //?Data fine attività  -  controllo
328200091229         clear wlbdat;
328300091229         G02dat = V02dadf;
328400091229         xsrda8(wlbdat);
328500091229         IF  G02err = '1';
328600091229           ErrMessage  = *on;
328700091229           ErrGenerico = *on;
328800091229           PosCurDADF  = *on;
328900091229           V02msg      = $Msg(06);
329000091229           leavesr;
329100091229         ENDIF;
329200091229
329300091229         V02dadf  = G02dat;
329400091229         wdadf    = G02inv;
329500100119
329600100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
329700100119         IF  wdadf < 20000101 or wdadf > 20391231;
329800100119           ErrMessage  = *on;
329900100119           ErrGenerico = *on;
330000100119           PosCurDADF  = *on;
330100100119           V02msg      = $Msg(06);
330200100119           leavesr;
330300100119         ENDIF;
330400100119
330500091229         data_iso = (%date(wdadf));
330600091229
330700100423       //?Data fine attività  -  no festivo
330800091229         w0030   = %dec(%subst(V02cmm:1:3):3:0);
330900091229         wanno   = %subdt(data_ISO:*years);
331000091229         wmese   = %subdt(data_ISO:*months);
331100091229         wgiorno = %subdt(data_ISO:*days);
331200100423         exsr CtrFesta;
331300091229         IF  $Festa;
331400091229           ErrMessage  = *on;
331500091229           ErrGenerico = *on;
331600091229           PosCurDADF  = *on;
331700091229           V02msg      = $Msg(06);
331800091229           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
331900091229           leavesr;
332000091229         ENDIF;
332100091229
332200091229       //?Data inizio attività  -  congruente con data fine attività
332300091229         IF  wdad > 0  and  wdadf = 0;
332400091229           ErrMessage  = *on;
332500091229           ErrGenerico = *on;
332600091229           PosCurDADF  = *on;
332700091229           V02msg      = $Msg(11);
332800091229           leavesr;
332900091229         ENDIF;
333000091229         IF  wdad > 0  and  wdadf > 0  and  wdad > wdadf;
333100091229           ErrMessage  = *on;
333200091229           ErrGenerico = *on;
333300091229           PosCurDADF  = *on;
333400091229           V02msg      = $Msg(11);
333500091229           leavesr;
333600091229         ENDIF;
333700140227
333800140227         // -?Impostazione delle 2 date in formato *ISO
333900140227         data_isof = (%date(wdadf));
334000140227         data_iso  = (%date(wdad));
334100140227
334200140227         // -?Controllo periodo: NON può essere superiore alle 3 settimane
334300140227         //  ?N.B.?- Ora questa videata (e quindi questo controllo) è
334400140227         //  ?prevista solo per: · Attività = "F"  (FERIE/ASSENZE)
334500140227         //  ?                   · Causale  = "FT" (FERIE/ASSENZA TOTALE)
334600140227         If  NOT $ForzaFerie  and
334700140227             %diff( Data_IsoF : Data_Iso : *days ) > 21;
334800140227           $ForzaFerie = *on;
334900140227           ErrGenerico = *on;
335000140227           ErrMessage  = *on;
335100140227           PosCurDADF  = *on;
335200140227           V02msg      = $Msg(26);
335300140227           leavesr;
335400140227         EndIf;
335500091229
335600091229       //?Verifico se il commerciale è libero
335700091229       //?controllo tutti i giorni inseriti a video
335800091229       //?anche i festivi...teoricamente nessuna attività viene fatta di festa
335900091229         DOW  data_iso <= data_isof;
336000091229           clear trmk84ds;
336100091229           IMK84cmm = %dec(V02cmm:7:0);
336200091229           IMK84dad = %dec(data_iso);
336300091229           IMK84oii = 0001;
336400091229           IMK84ofi = 2359;
336500091229           trmk84r (trmk84ds);
336600091229         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
336700091229           IF  OMK84err <> *blanks and OMK84err <> '9' and
336800091229               Not $ForzaImpegno;
336900091229             ErrGenerico = *on;
337000091229             ErrMessage  = *on;
337100091229             PosCurCMM   = *on;
337200091229             V02msg = OMK84msg;
337300091229             V02msg = %trim(V02msg) + '. Enter per forzare.';
337400091229             $ForzaImpegno = *on;
337500091229             leavesr;
337600091229           ENDIF;
337700091229           data_iso += %days(1);
337800091229         ENDDO;
337900091229
338000091229       ENDSR;
338100091229
338200091229       //--------------------------------------------------------------
338300091229       //?Gestione tasto funzionale F06 da videata DT2.
338400091229       //?F06=Conferma
338500091229       //--------------------------------------------------------------
338600091229       BEGSR F06DT2;
338700091229
338800091229       //?Scrivo nuova attività
338900091229         data_iso  = (%date(wdad));
339000091229       //?1 rcd per ogni giorno inserito a video
339100091229         DOW  data_iso <= data_isof;
339200100929         //?controllo che non sia festivo
339300100929           w0030   = %dec(%subst(V02cmm:1:3):3:0);
339400100929           wanno   = %subdt(data_ISO:*years);
339500100929           wmese   = %subdt(data_ISO:*months);
339600100929           wgiorno = %subdt(data_ISO:*days);
339700100929           exsr CtrFesta;
339800100929           IF  $Festa;
339900100929             data_iso += %days(1);
340000100929             iter;
340100100929           ENDIF;
340200100929
340300100929         //?Stacco nuovo numero attività
340400100929           exsr NewAttivita;
340500100929         //?se errore vado subito a fine
340600100929           IF  O33Err <> 0;
340700100929             $Fine = *on;
340800100929             ErrGenerico = *on;
340900100929             V02msg = O33msg;
341000100929             leavesr;
341100100929           ENDIF;
341200100929
341300091229           clear tiatc000;
341400091229           atctat  = V02att;
341500091229           atcatn  = O33Nrf;
341600100929           atcatnp = 1;
341700091229           atccpo  = IMK17cpo;
341800091229           atcksc  = IMK17ksc;
341900091229           atcnrv  = IMK17nrv;
342000091229           atccad  = V02cau;
342100091229           atcdad  = %dec(data_iso);
342200100108           atchda  = 090000;
342300091229           atccmm  = %dec(V02cmm:7:0);
342400100108           atcoii  = 0001;
342500091229           atcofi  = 2359;
342600091229           atcdim  = %dec(%date());
342700091229           atchim  = %dec(%time());
342800091229           atcpri  = knmus ;
342900091229
343000091229           write TIATC000;
343100091229           feod  TIATC01L;
343200100929
343300100929         //?Se ci sono note le registro
343400100929           clear TRMK20ds;
343500100929           IMK20flm  = 'C';
343600100929           IMK20cpo  = atccpo;
343700100929           IMK20ksc  = atcksc;
343800100929           IMK20nrv  = atcnrv;
343900100929           IMK20tat  = atctat;
344000100929           IMK20atn  = atcatn;
344100100929           IMK20atnp = 0;
344200100929           trmk20r (kpjba : TRMK20ds);
344300091229
344400091229           data_iso += %days(1);
344500091229         ENDDO;
344600091229
344700091229       //?Ritorno al programma chiamante
344800091229         $Fine   = *on ;
344900091229
345000091229       ENDSR;
345100091229
345200091229       //--------------------------------------------------------------
345300091229       //?Gestione videata C02.
345400091229       //--------------------------------------------------------------
345500091229       BEGSR GesC02;
345600091229
345700091229         // Inizializzazione videata
345800091229         if $InzC02 = *on;
345900091229           exsr InzC02;
346000091229           $InzC02 = *off;
346100091229         endif;
346200091229
346300091229         $Eof   = *off;
346400091229
346500091229       //?Emissione videata
346600091229         sfldsp_n = *off;
346700091229         write MK17Z02;
346800091229         exfmt MK17C02;
346900091229         ErrMessage   = *off;
347000091229         ErrGenerico  = *off;
347100091229         clear V02msg;
347200091229
347300091229         SELECT;
347400091229
347500091229       //?F12 = Ritorno
347600091229           WHEN dsp_aid = c_F12;
347700100205             exsr F12D02;
347800091229             leavesr ;
347900091229
348000091229       //?F11 = Agenda
348100091229           WHEN dsp_aid = c_F11;
348200091230             exsr F11D02;
348300091229             leavesr ;
348400091229
348500091229       //?F18 = Note
348600091229           WHEN dsp_aid = c_F18;
348700091229             exsr F18D01;
348800091229             leavesr ;
348900091229
349000091229       //?F06 = Conferma
349100091229           WHEN dsp_aid = c_F06;
349200091229             //?prima faccio i controlli
349300091229             exsr CtrC02;
349400091229             IF  ErrGenerico;
349500091229               leavesr;
349600091229             ENDIF;
349700091229             //?tutto ok confermo i dati
349800091229             exsr F06C02;
349900091229             leavesr ;
350000091229
350100091229       //?Enter
350200091229           OTHER;
350300091229       //?Controllo i dati
350400091229            exsr CtrC02;
350500091229            IF  ErrGenerico;
350600091229              leavesr;
350700091229            ENDIF;
350800091229
350900091229         ENDSL;
351000091229
351100091229       ENDSR;
351200091229
351300091229       //--------------------------------------------------------------
351400091229       //?Caricamento videata C02.
351500091229       //--------------------------------------------------------------
351600091229       BEGSR InzC02;
351700091229
351800091229         //?Pulizia subfile
351900091229         exsr PulS02;
352000091229
352100091229       //?Imposto i campi del control
352200091229         VC2pgm  = V02pgm;
352300091229         VC2sut  = V02sut;
352400091229         VC2sif  = V02sif;
352500091229         VC2mus  = V02mus;
352600091229         VC2att  = V02att;
352700100113         VC2datt = V02datt;
352800091229         VC2cau  = V02cau;
352900091229         VC2dcau = V02dcau;
353000100205         clear V02dad;
353100100205         clear V02oii;
353200100205         clear V02ofi;
353300091229
353400091229       //?Imposto data attività se passata
353500091229         IF  IMK17dad > 0;
353600091229           data_eur = %date(IMK17dad:*iso);
353700091229           V02dad   = %dec(data_eur);
353800091229         //?proteggo se richiesto
353900091229           ProteggiDAD = (IMK17pdad = 'S');
354000091229         ENDIF;
354100091229
354200091229       //?Posiziono il cursore
354300091229         SELECT;
354400091229       //?Sulla data se NON passata
354500091229           WHEN IMK17dad = 0;
354600091229             PosCurDAD = *on;
354700091229       //?Su ora inizio impegno se passata la data
354800091229           WHEN IMK17dad > 0;
354900091229             PosCurOII = *on;
355000091229         ENDSL;
355100091229
355200091229       //?Carico subfile
355300091229         exsr CarS02;
355400091229
355500091229       ENDSR;
355600091229
355700091229       //--------------------------------------------------------------
355800091229       //?Pulizia Subfile S02.
355900091229       //--------------------------------------------------------------
356000091229       BEGSR PulS02;
356100091229
356200091229         //?Pulizia subfile
356300091229         SflDsp_N    = *on;
356400091229         SflDspCtl_N = *on;
356500091229         write  MK17C02;
356600091229         SflDspCtl_N = *off;
356700091229         SflEnd      = *off;
356800091229
356900100205         V02rcd = 1;
357000091229         clear V02msg;
357100091229         ErrMessage  = *off;
357200091229         ErrGenerico = *off;
357300091229         clear S02Nrr;
357400091229
357500091229       ENDSR ;
357600091229
357700091229       //--------------------------------------------------------------
357800091229       //?Carico subfile S02.
357900091229       //--------------------------------------------------------------
358000091229       BEGSR CarS02;
358100100217
358200100304         //?Se richiesta riunione d'area 'RA' devo caricare gli agenti di area
358300100304         IF   V02cau = 'RA';
358400100217         //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
358500100304           IF  OTAA1cabi = 'AZ';
358600100304             clear TRUL31DS;
358700100304             I31abi = 'RA';
358800100304             I31cdi = DUTdis;
358900100304             I31car = DUTare;
359000100304             I31cpo = DUTpou;
359100100304             callp trul31r (kpjba : trul31ds);
359200100304             ENDIF;
359300091230
359400100114         //?Carico sk di comodo con i commerciali d'area
359500100304           exsr CarAge;
359600100304         ENDIF;
359700100304         //?Se richiesta riunione commerciale 'RC' NON devo caricare gli agenti
359800100304         IF   V02cau = 'RC';
359900100304           clear $Age;
360000100304         ENDIF;
360100091229
360200100114         //?Carico subfile
360300100114         xx = 1;
360400100114         FOR xx by 1 to %elem($Age);
360500100114           IF  $Age(xx) = 0;
360600100114             iter;
360700100114           ENDIF;
360800130808           V02cmm = %editc($Age(xx):'X');
360900130808           chain  ($Age(xx))  AZCMM000;
361000130808           IF  %found(AZCMM01L);
361100130808             V02dcmm = CMMdes;
361200130808           ENDIF;
361300100310           VH2savcmm = V02cmm;
361400100310           clear VH2fimp;
361500100310           clear VH2fint;
361600100310           S02Nrr += 1;
361700100114           write MK17S02;
361800100114         ENDFOR;
361900091230
362000100114         //?Il resto a 0
362100100114         IF  S02Nrr < %elem($Age);
362200100114           yy = %elem($Age) - S02Nrr;
362300100205         //?faccio un accrocchio per visualizzare solo 1 pagina 2 pagine 3 pagine
362400100205           IF  S02Nrr > 1  and S02nrr < 25;
362500100205             yy = 24 - s02nrr;
362600100205           ENDIF;
362700100205           IF  S02Nrr > 24 and S02nrr < 49;
362800100205             yy = 48 - s02nrr;
362900100205           ENDIF;
363000100205           IF  S02Nrr > 48;
363100100205             yy = 72 - s02nrr;
363200100205           ENDIF;
363300100114           xx = 1;
363400100114           FOR xx by 1 to yy;
363500100114             clear MK17S02;
363600100114             S02Nrr += 1;
363700100114             write MK17S02;
363800100114           ENDFOR;
363900100114         ENDIF;
364000091229
364100091229         SflEnd = *on;
364200091229
364300091229       ENDSR;
364400100114
364500100114       //--------------------------------------------------------------
364600100114       //?Carico sk agenti d'area.
364700100114       //--------------------------------------------------------------
364800100114       BEGSR CarAge;
364900100114
365000100203         $End = *off;
365100100114         clear $AGE;
365200100205
365300100205         //?Carico gli agenti unificanti delle filiali gestite dall'utente
365400100114
365500100114         //?Imposto la stringa per SQL
365600131108         wSQL = 'select CMMcod +
365700131108                   from AZCMM00F +
365800131108                  where CMMatb = '' '' and +
365900131108                        substr( digits( CMMuni ), 1, 3) in (';
366000100114
366100100114         yy = 0;
366200100114         xx = 1;
366300100114         FOR xx by 1 to %elem(pog);
366400100114           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
366500100114             IF yy > 0;
366600100114               wSQL += ', ';
366700100114             ELSE;
366800100114               yy = 1;
366900100114             ENDIF;
367000100114             wSQL += '''';
367100100114             wSQL += pog(xx);
367200100114             wSQL += '''';
367300100114           ENDIF;
367400100114         ENDFOR;
367500100114
367600131108         wSQL += ') and CMMcod = CMMuni and +
367700131108                    substr( digits( CMMuni ), 4, 1) = ''0'' and +
367800131118                    CMMpar = '' '' and +
367900131118                    CMMfun NOT in (''COMIN'', ''ASC  '', ''SA   '')';
368000100114
368100131108         wSQL += ' order by CMMcod +
368200100114                   for fetch only';
368300100114
368400100114         //?Dichiarazione cursore
368500100114         exec sql
368600100114           prepare S1   from :wSQL;
368700100114         exec sql
368800100114           declare AREA cursor for S1;
368900100114
369000100114         //?Apertura del cursore
369100100114         exec sql
369200100114           open AREA;
369300100114
369400100114         IF sqlcode < 0;
369500100114           $End = *on;
369600100114         ENDIF;
369700100114
369800100114         yy = 0;
369900100114         DOW not $End;
370000131108
370100100114           exec sql
370200131108             fetch next from AREA into :wCMMcod;
370300131108
370400100114           IF sqlcod = 100 or sqlcod < 0;
370500100114             $End = *on;
370600100114             leave;
370700100114           ENDIF;
370800100114
370900131108           yy += 1;
371000131108           $AGE(yy) = wCMMcod;
371100100114
371200100114         ENDDO;
371300100114
371400100114         exec sql
371500100114           close AREA;
371600100114
371700100114       ENDSR;
371800091229
371900091229       //--------------------------------------------------------------
372000091229       //?Controllo videata C02.
372100091229       //--------------------------------------------------------------
372200091229       BEGSR CtrC02;
372300091229
372400091229         WindDspF = IndDspF;
372500091229         reset WindDspFb;
372600091229         IndDspF  = WindDspF;
372700091230
372800091230       //?Prima controllo i dati immessi nel control
372900091230
373000091230       //?Data attività  -  obbligatoria
373100091230         IF  V02dad = 0;
373200091230           ErrMessage  = *on;
373300091230           ErrGenerico = *on;
373400091230           PosCurDAD   = *on;
373500091230           V02msg      = $Msg(06);
373600091230           leavesr;
373700091230         ENDIF;
373800091230
373900091230       //?Data attività  -  controllo
374000091230         clear wlbdat;
374100091230         G02dat = V02dad;
374200091230         xsrda8(wlbdat);
374300091230         IF  G02err = '1';
374400091230           ErrMessage  = *on;
374500091230           ErrGenerico = *on;
374600091230           PosCurDAD   = *on;
374700091230           V02msg      = $Msg(06);
374800091230           leavesr;
374900091230         ENDIF;
375000091230
375100091230         V02dad = G02dat;
375200091230         wdad   = G02inv;
375300100119
375400100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
375500100119         IF  wdad < 20000101 or wdad > 20391231;
375600100119           ErrMessage  = *on;
375700100119           ErrGenerico = *on;
375800100119           PosCurDAD   = *on;
375900100119           V02msg      = $Msg(06);
376000100119           leavesr;
376100100119         ENDIF;
376200100119
376300091230         data_iso = (%date(wdad));
376400091230
376500091230       //?Data attività  -  no festivo
376600091230         w0030   = dutpou;
376700091230         wanno   = %subdt(data_ISO:*years);
376800091230         wmese   = %subdt(data_ISO:*months);
376900091230         wgiorno = %subdt(data_ISO:*days);
377000091230         exsr CtrFesta;
377100091230         IF  $Festa;
377200091230           ErrMessage  = *on;
377300091230           ErrGenerico = *on;
377400091230           PosCurDAD   = *on;
377500091230           V02msg      = $Msg(06);
377600091230           V02msg      = %trim(V02msg) + '. Immesso giorno festivo!';
377700091230           leavesr;
377800091230         ENDIF;
377900091230
378000091230       //?Impegno  ORA inizio  -  obbligatorio
378100091230         IF  V02oii = 0;
378200091230           ErrMessage  = *on;
378300091230           ErrGenerico = *on;
378400091230           PosCurOII   = *on;
378500091230           V02msg      = $Msg(07);
378600091230           leavesr;
378700091230         ENDIF;
378800091230
378900091230       //?Impegno  ORA inizio  -  controllo
379000120104         IF  %dec(%subst(%editc(V02oii:'X'):1:2):2:0) > 23 or
379100091230             %dec(%subst(%editc(V02oii:'X'):3:2):2:0) > 59;
379200091230           ErrMessage  = *on;
379300091230           ErrGenerico = *on;
379400091230           PosCurOII   = *on;
379500091230           V02msg      = $Msg(07);
379600091230           leavesr;
379700091230         ENDIF;
379800091230
379900091230       //?Impegno  ORA fine    -  obbligatorio
380000091230         IF  V02ofi = 0;
380100091230           ErrMessage  = *on;
380200091230           ErrGenerico = *on;
380300091230           PosCurOFI   = *on;
380400091230           V02msg      = $Msg(07);
380500091230           leavesr;
380600091230         ENDIF;
380700091230
380800091230       //?Impegno  ORA fine    -  controllo
380900120104         IF  %dec(%subst(%editc(V02ofi:'X'):1:2):2:0) > 23 or
381000091230             %dec(%subst(%editc(V02ofi:'X'):3:2):2:0) > 59;
381100091230           ErrMessage  = *on;
381200091230           ErrGenerico = *on;
381300091230           PosCurOFI   = *on;
381400091230           V02msg      = $Msg(07);
381500091230           leavesr;
381600091230         ENDIF;
381700091230
381800091230       //?Impegno  ORA inizio  -  congruente con ORA fine
381900091230         IF  V02oii > 0  and  V02ofi = 0;
382000091230           ErrMessage  = *on;
382100091230           ErrGenerico = *on;
382200091230           PosCurOFI   = *on;
382300091230           V02msg      = $Msg(09);
382400091230           leavesr;
382500091230         ENDIF;
382600091230         IF  V02oii > 0  and  V02ofi > 0  and  V02oii > V02ofi ;
382700091230           ErrMessage  = *on;
382800091230           ErrGenerico = *on;
382900091230           PosCurOFI   = *on;
383000091230           V02msg      = $Msg(09);
383100091230           leavesr;
383200091230         ENDIF;
383300091229
383400091230       //?Se tutto ok passo a verificare i dati immessi nel subfile
383500091229         clear S02Nrr;
383600091230
383700091230       //?Pulisco sk di comodo commerciali
383800091230         clear $AGE;
383900091229
384000091229         DOW  not $EoF;
384100091229           S02Nrr += 1;
384200091229           chain S02Nrr MK17S02;
384300091229           IF  not %found;
384400091229             $EoF = *on;
384500091229             leave;
384600091229           ENDIF;
384700091229
384800091230         //?Se non c'è il codice commerciale ritorno a leggere il subfile
384900100114           IF  V02cmm = *blanks;
385000091229             iter;
385100091229           ENDIF;
385200100114         //?Se il codice commerciale è a 0 ripulisco i campi e ritorno a leggere
385300100114         //?il subfile
385400100114           IF  V02cmm = *zeros;
385500100114             clear V02cmm;
385600100114             clear V02dcmm;
385700100114             V02Rcd  = S02Nrr;
385800100114             update MK17S02;
385900100114             iter;
386000100114           ENDIF;
386100100310
386200100310            IF  V02cmm <> VH2savcmm;
386300100310              clear VH2fimp;
386400100310              clear VH2fint;
386500100310            ENDIF;
386600091229
386700091230         //?Commerciale  -  Ricerca
386800091230           IF  %scan('?' : V02cmm) > *zero;
386900091230             ErrGenerico = *on;
387000091230             PosCurCMM   = *on;
387100130808             iMK43solU = 'S';
387200130808             iMK43fil  = %int(%subst(%editc(IMK17cmm:'X'):1:3));
387300130808             TRMK43R (kpjba : TRMK43ds);
387400130808             if  oMK43err = *off  and  oMK43fxx = *blank;
387500130808               V02cmm  = %editc( oMK43cmm : 'X' );
387600130808               V02dcmm = oMK43des;
387700130808             endif;
387800091230             V02Rcd  = S02Nrr;
387900091230             update MK17S02;
388000091230             $EoF = *on;
388100091230             leave;
388200091230           ENDIF;
388300091230
388400091230           IF  %check(digits:V02cmm) > 0;
388500091230             ErrMessage  = *on;
388600091230             ErrGenerico = *on;
388700091230             PosCurCMM   = *on;
388800091230             V02msg      = $Msg(08);
388900091230             V02Rcd      = S02Nrr;
389000091230             update MK17S02;
389100091230             $EoF = *on;
389200091230             leave;
389300091230           ENDIF;
389400091229
389500091230         //?Commerciale  -  Controllo
389600091230           IF  V02cmm > *zeros;
389700091230             clear V02dcmm;
389800130808             CMmcod = %int(V02cmm);
389900130808             chain  (CMMcod)  AZCMM000;
390000130808             IF  NOT %found(AZCMM01L);
390100091230               ErrMessage  = *on;
390200091230               ErrGenerico = *on;
390300091230               PosCurCMM   = *on;
390400091230               V02msg      = $Msg(08);
390500091230               V02Rcd      = S02Nrr;
390600091230               update MK17S02;
390700091230               $EoF = *on;
390800091230               leave;
390900100205             ENDIF;
391000100209
391100130808             V02dcmm = CMMdes;
391200100310             VH2savcmm = V02cmm;
391300100209
391400100209         //?commerciale valido no vari o clienti inattivi
391500130808             IF  CMMpar <> *blanks;
391600100209               ErrMessage  = *on;
391700100209               ErrGenerico = *on;
391800100209               PosCurCMM   = *on;
391900100209               V02msg      = $Msg(08);
392000100209               V02Rcd      = S02Nrr;
392100100209               update MK17S02;
392200100209               $EoF = *on;
392300100209               leave;
392400100209             ENDIF;
392500111123
392600111123         //?commerciale valido data inizio e fine attività
392700130808             IF  CMMdtIni > woggi or CMMdtFin < woggi;
392800111123               ErrMessage  = *on;
392900111123               ErrGenerico = *on;
393000111123               PosCurCMM   = *on;
393100111123               V02msg      = $Msg(08);
393200111123               V02Rcd      = S02Nrr;
393300111123               update MK17S02;
393400111123               $EoF = *on;
393500111123               leave;
393600111123             ENDIF;
393700100310
393800100310         //?controllo se attività può essere eseguita dal commerciale interno
393900130808             IF  CMMfun = 'COMIN' or CMMfun = 'ASC' or CMMfun = 'SA';
394000100310               SELECT;
394100100310                 WHEN  VH2fint = *blanks and §CCOcoi = 'F';
394200100310                   ErrMessage  = *on;
394300100310                   ErrGenerico = *on;
394400100310                   PosCurCMM   = *on;
394500100310                   V02msg      = $Msg(15);
394600100310                   V02msg      = %trim(V02msg) + ' Enter per forzare';
394700100310                   VH2fint     = '1';
394800100310                   V02Rcd      = S02Nrr;
394900100310                   update MK17S02;
395000100310                   $EoF = *on;
395100100310                   leave;
395200100310                 WHEN  §CCOcoi = 'N';
395300100310                   ErrMessage  = *on;
395400100310                   ErrGenerico = *on;
395500100310                   PosCurCMM   = *on;
395600100310                   V02msg      = $Msg(15);
395700100310                   V02Rcd      = S02Nrr;
395800100310                   update MK17S02;
395900100310                   $EoF = *on;
396000100310                   leave;
396100100310             ENDSL;
396200100310           ENDIF;
396300091229
396400091230         //?Commerciale  -  solo unificante
396500130808             IF  %dec(V02cmm:7:0) <> CMMuni;
396600100209               ErrMessage  = *on;
396700100209               ErrGenerico = *on;
396800100209               PosCurCMM   = *on;
396900100210               V02msg      = $Msg(14);
397000100209               V02Rcd      = S02Nrr;
397100100209               update MK17S02;
397200100209               $EoF = *on;
397300100209               leave;
397400100209             ENDIF;
397500100208
397600100205         //?utente abilitato al commerciale
397700100209             clear TNTAA1DS;
397800120810             ITAA1tipo = 'M';
397900120810             ITAA1caut = '§utegtc';
398000100209             ITAA1cmm  = %dec(V02cmm:7:0);
398100100521             ITAA1abi = 'RA';
398200100209             kpjbu     = tntaa1ds;
398300100209             tntaa1c (kpjba);
398400100209             TNTAA1DS = kpjbu;
398500100209             IF  OTAA1fabi = 'N';
398600100209               ErrMessage  = *on;
398700100209               ErrGenerico = *on;
398800100209               PosCurCMM   = *on;
398900100209               V02msg      = $Msg(08);
399000100209               V02Rcd      = S02Nrr;
399100100209               update MK17S02;
399200100209               $EoF = *on;
399300100209               leave;
399400100209             ENDIF;
399500100209           ENDIF;
399600091230
399700091230         //?Incremento la schiera di comodo commerciali
399800100114           xx = %lookup(%dec(V02cmm:7:0) : $AGE);
399900100114           IF  xx > 0;
400000100114             ErrMessage  = *on;
400100100114             ErrGenerico = *on;
400200100114             PosCurCMM   = *on;
400300100114             V02msg      = $Msg(13);
400400100114             V02Rcd      = S02Nrr;
400500100114             update MK17S02;
400600100114             $EoF = *on;
400700100114             leave;
400800100114           ENDIF;
400900091230           $AGE(S02Nrr) = %dec(V02cmm:7:0);
401000091229
401100091230         //?Verifico se il commerciale è libero
401200091230           clear trmk84ds;
401300091230           IMK84cmm = %dec(V02cmm:7:0);
401400091230           IMK84dad = wdad;
401500091230           IMK84oii = V02oii;
401600091230           IMK84ofi = V02ofi;
401700091230           trmk84r (trmk84ds);
401800091230         //?se torna errore <> "9" vuol dire che il commerciale è impegnato
401900091230           IF  OMK84err <> *blanks and OMK84err <> '9' and
402000100310               VH2fimp = *blanks;
402100091230             ErrGenerico = *on;
402200091230             ErrMessage  = *on;
402300091230             PosCurCMM   = *on;
402400091230             V02msg = OMK84msg;
402500091230             V02msg = %trim(V02msg) + '. Enter per forzare.';
402600091230             V02Rcd      = S02Nrr;
402700100310             VH2fimp = '1';
402800091230             update MK17S02;
402900091230             $EoF = *on;
403000091230             leave;
403100091230           ENDIF;
403200091230
403300091230           V02Rcd = S02Nrr;
403400091230           update MK17S02;
403500091230         ENDDO;
403600091230
403700100203         IF  ErrGenerico;
403800100203           leavesr;
403900100203         ENDIF;
404000100203
404100091230         //?Controllo se ho caricato almeno un commerciale
404200100203         xx = 1;
404300100203         $NoAge = *on;
404400100203         FOR xx by 1 to %elem($Age);
404500100203           IF  $age(xx) > 0;
404600100203             $NoAge = *off;
404700100203           ENDIF;
404800100203         ENDFOR;
404900091230
405000091230         //?Se non ho caricato commerciali errore
405100100203         IF  $NoAge;
405200100203           ErrMessage  = *on;
405300100203           ErrGenerico = *on;
405400100203           V02msg      = $Msg(12);
405500100203         ENDIF;
405600091229
405700091229       ENDSR;
405800091230
405900091230       //--------------------------------------------------------------
406000091230       //?Gestione tasto funzionale F06 da videata C02.
406100091230       //?F06=Conferma
406200091230       //--------------------------------------------------------------
406300091230       BEGSR F06C02;
406400091230
406500091230       //?Scrivo nuova attività
406600091230       //?1 rcd per ogni commerciale inserito a video
406700091230         $Eof = *off;
406800091230         clear S02Nrr;
406900091230
407000091230         //?Leggo subfile
407100091230         DOW  not $EoF;
407200091230           S02Nrr += 1;
407300091230           chain S02Nrr MK17S02;
407400091230           IF  not %found;
407500091230             $EoF = *on;
407600091230             leave;
407700091230           ENDIF;
407800091230           IF  V02cmm = *blanks;
407900091230             iter;
408000091230           ENDIF;
408100100929
408200100929         //?Stacco nuovo numero attività
408300100929           exsr NewAttivita;
408400100929         //?se errore vado subito a fine
408500100929           IF  O33Err <> 0;
408600100929             $Fine = *on;
408700100929             ErrGenerico = *on;
408800100929             V02msg = O33msg;
408900100929             leavesr;
409000100929           ENDIF;
409100091230
409200091230           clear TIATC000;
409300091230           atctat  = V02att;
409400091230           atcatn  = O33Nrf;
409500100929           atcatnp = 1;
409600091230           atccpo  = IMK17cpo;
409700091230           atcksc  = IMK17ksc;
409800091230           atcnrv  = IMK17nrv;
409900091230           atccad  = V02cau;
410000091230           atcdad  = wdad;
410100100108           atchda  = V02oii * 100;
410200091230           atccmm  = %dec(V02cmm:7:0);
410300091230           atcoii  = V02oii;
410400091230           atcofi  = V02ofi;
410500091230           atcdim  = %dec(%date());
410600091230           atchim  = %dec(%time());
410700091230           atcpri  = knmus ;
410800091230
410900091230           write TIATC000;
411000091230           feod  TIATC01L;
411100100929
411200100929         //?Se ci sono note le registro
411300100929           clear TRMK20ds;
411400100929           IMK20flm  = 'C';
411500100929           IMK20cpo  = atccpo;
411600100929           IMK20ksc  = atcksc;
411700100929           IMK20nrv  = atcnrv;
411800100929           IMK20tat  = atctat;
411900100929           IMK20atn  = atcatn;
412000100929           IMK20atnp = 0;
412100100929           trmk20r (kpjba : TRMK20ds);
412200091230
412300091230         ENDDO;
412400091230
412500091230       //?Ritorno al programma chiamante
412600091230         $Fine   = *on ;
412700091230
412800091230       ENDSR;
412900091229
413000091229       //--------------------------------------------------------------
413100091229       //?Gestione tasto funzionale F11 da videata DP2/DT2.
413200091229       //?F11=Agenda
413300091229       //--------------------------------------------------------------
413400091229       BEGSR F11D02;
413500091229
413600091229         clear TRMK82ds;
413700091229         IMK82rich = 'A';
413800091230
413900091230       //?Imposto il commerciale se inserito (può succedere nel caso di riunione commerciale)
414000091230         IF  V02cmm <> *blanks and V02cmm > *zeros;
414100091230           IMK82cage = %dec(V02cmm:7:0);
414200091230         ENDIF;
414300091229
414400091229       //?Imposto la data di work
414500091229         IF  wdad > 0;
414600091229           IMK82data = wdad;
414700091229         ENDIF;
414800091229
414900091229         kpjbu = trmk82ds;
415000091229         trmk82c (kpjba);
415100091229
415200091229       ENDSR;
415300100112
415400100112       //--------------------------------------------------------------
415500100112       //?Gestione tasto funzionale F12 da videata DP2/DT2.
415600100112       //?F12=Ritorno
415700100112       //--------------------------------------------------------------
415800100112       BEGSR F12D02;
415900100112
416000100112         $Video = 'T2';
416100100112         $InzT02 = *on;
416200100112
416300100112       ENDSR;
416400091223
416500091223       //--------------------------------------------------------------
416600091223       //?Controllo se giorno festivo.
416700091223       //--------------------------------------------------------------
416800091223       BEGSR CtrFesta;
416900091223
417000091223         $Festa = *off;
417100091223
417200091229         clear k_clntfa;
417300091229         k_clntfp = w0030;
417400091223         k_clnann = wanno;
417500091223         k_clnmes = wmese;
417600091223         chain %kds(K04azcln) AZCLN01L;
417700091223         IF  %found(AZCLN01L);
417800091223           IF  mat(wgiorno) = 'F'  or
417900091223               pom(wgiorno) = 'F';
418000091223             $Festa = *on;
418100091223           ENDIF;
418200091223         ENDIF;
418300091223
418400091223       ENDSR;
418500091223
418600091223       //--------------------------------------------------------------
418700091223       //?Stacco nuovo numero attività.
418800091223       //--------------------------------------------------------------
418900091223       BEGSR NewAttivita;
419000091223
419100091223         clear TRUL33ds;
419200091223         I33tla = 'L';
419300091223         I33ope = 0;
419400091223         I33cnu = 070;
419500091223         I33num = 1;
419600091223         I33aaa = *YEAR;
419700091223         kpjbu  = TRUL33ds;
419800091223         trul33r (kpjba);
419900091223         TRUL33ds = kpjbu;
420000091223
420100091223       ENDSR;
420200091223
420300091223       //--------------------------------------------------------------
420400091223       //?Richiamo il programma di inserimento affiancamento.
420500091223       //--------------------------------------------------------------
420600091223       BEGSR Ric_Affianca;
420700091223
420800091223         clear TRMK22ds;
420900091230         IMK22fcmt = IMK17fcmt;
421000091230         IMK22cmt  = IMK17cmt;
421100091230         IMK22fle  = 'I';
421200091230         IMK22cmm  = atccmm;
421300091230         IMK22dad  = atcdad;
421400091230         IMK22hda  = atchda;
421500091230         IMK22oii  = atcoii;
421600091230         IMK22ofi  = atcofi;
421700091230         IMK22tat  = atctat;
421800091230         IMK22atn  = atcatn;
421900091230         IMK22atnp = atcatnp;
422000091223
422100091223         trmk22r (kpjba : TRMK22ds);
422200091223
422300091223         //?Se ritorna errore lo visualizzo e faccio rollback
422400091223         IF  OMK22err <> *blanks;
422500091223           ErrMessage  = *on;
422600091223           ErrGenerico = *on;
422700091223           V01msg = OMK22msg;
422800091223           IF  IMK17cmt = 'S';
422900091223             rolbk;
423000091223           ENDIF;
423100091223           leavesr;
423200091223         ENDIF;
423300091223
423400091223         //?Se F12 da gestione affiancamento rollback
423500091223         IF  IMK17cmt = 'S' and OMK22F12 = 'S';
423600091223           rolbk;
423700091223         ENDIF;
423800091223
423900091223         //?Se non ci sono errori e non è stato premuto F12
424000091223         //?confermo tutto
424100091230         IF  IMK17cmt = 'S' and OMK22err = *blanks and OMK22f12 = *blanks;
424200091223           commit;
424300091223         ENDIF;
424400091223
424500091223       ENDSR;
424600101215
424700101215       //--------------------------------------------------------------
424800101215       //?Richiamo il programma di gestione data affidamento merce.
424900101215       //--------------------------------------------------------------
425000101215       BEGSR Ric_PgmHot;
425100101215
425200101215         clear TNTA63ds;
425300101215         ITA63fcmt = IMK17fcmt;
425400101215         ITA63cmt  = IMK17cmt;
425500101215         ITA63fmt  = '3';
425600110118         ITA63fle  = 'I';
425700101215         ITA63nrv  = IMK17nrv;
425800101215
425900101215         tnta63r (kpjba : TNTA63ds);
426000101215
426100101215       ENDSR;
426200091221
426300091221       //--------------------------------------------------------------
426400091221       //?Operazioni finali.
426500091221       //--------------------------------------------------------------
426600091221       BEGSR RoutEnd;
426700091221
426800091221?        //?Ritorno errore
426900091221         IF  ErrGenerico;
427000091221           OMK17err = 'E';
427100091221           OMK17msg = V01msg;
427200091221         ENDIF;
427300091221
427400091221?        //?Comitto se richiesto e se non ci sono errori
427500091221         IF  IMK17cmt = 'S' and OMK17err = ' ';
427600091221           commit;
427700091221         ENDIF;
427800091221
427900091221         IF  IMK17cmt = 'S' and OMK17err <> ' ';
428000091221           rolbk;
428100091221         ENDIF;
428200100929
428300100929?      //?Richiamo TRMK20R per chiusura dati
428400100929         clear TRMK20ds;
428500100929         IMK20tla  = 'C';
428600100929         trmk20r (kpjba : TRMK20ds);
428700091221
428800091221         *inLR = *on;
428900091221         return;
429000091221
429100091221       ENDSR;
429200091221
429300091221      /end-free
429400091221       //--------------------------------------------------------------
429500091221       //?Schiere a tempo di compilazione.
429600091221       //--------------------------------------------------------------
429700091221
429800091221** - $MSG -------------------------------------------------------------------*
429900091221Utente non abilitato alla funzione                                             01
430000091221Dati commit non specificati. Avvisare CED!                                     02
430100091223Incongruenza tra campi passati e protezione campi. Avvisare CED!               03
430200091223Tipo attività errata                                                           04
430300091223Causale attività errata                                                        05
430400091223Data errata                                                                    06
430500091223Ora errata                                                                     07
430600100208Commerciale errato o non in gestione all'utente                                08
430700091223Ora inizio impegno incongruente con ora fine impegno                           09
430800091223Ore inizio e fine impegno incongruenti con ora appuntamento                    10
430900091229Data inizio attività incongruente con data fine attività                       11
431000091230Immettere almeno un commerciale                                                12
431100100114Commerciale già inserito                                                       13
431200100210Commerciale errato. Immettere solo commerciali UNIFICANTI!                     14
431300100310Attività non possibile per commerciale interno.                                15
431400110502Causale NON utilizzabile per potenziale in categoria xxxxxxxxxxxxxxxxxxxx      16
431500110505Fil. del codice commerciale incongruente con la fil. del cliente               17
431600100310Causale non utilizzabile legata ad una trattativa                              18
431700100311Immettere le note                                                              19
431800100322Causale utilizzabile solo se legata ad una trattativa                          20
431900100322Causale non utilizzabile in presenza di trattativa senza offerta               21
432000100322Causale non utilizzabile in quanto attività legata a trattativa con offerta    22
432100100331Non trovati il Nome e/o indirizzo e-mail Resp.Trasporti. F08 non possibile.    23
432200110113Causale non utilizzabile per trattativa di tipo                                24
432300110307Causale blocco cliente errata                                                  25
432400140227Periodo di ferie  SUPERIORE  a 3 settimane - "Invio" per continuare            26
