000100141107      //--------------------------------------------------------------
000200150202      //?TRKC50R - CCREA WF CLIENTI IN CAMPAGNA
000300141107      //--------------------------------------------------------------
000400141107
000500141107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150202     h dftactgrp(*no) actgrp(*caller)
000700141107
000800141107      //---------------------------------------------------------------
000900141107      //?Dichiarazione file.
001000141107      //---------------------------------------------------------------
001100141107
001200141107      // - File anagrafiche Campagne
001300141107     fTICMP01L  if   e           k disk
001400141107
001500150202      // - Video
001600150202     fTRKC50D   cf   e             workstn
001700141107     f                                     indds(IndDspF)
001800141107     f                                     infds(InfDspF)
001900141107
002000141107      //---------------------------------------------------------------
002100141107      //?Definizione costanti.
002200141107      //---------------------------------------------------------------
002300141107
002400141107      // - Tasti funzionali a video
002500141107     d c_F01           c                   const(x'31')
002600141107     d c_F02           c                   const(x'32')
002700141107     d c_F03           c                   const(x'33')
002800141107     d c_F04           c                   const(x'34')
002900141107     d c_F05           c                   const(x'35')
003000141107     d c_F06           c                   const(x'36')
003100141107     d c_F07           c                   const(x'37')
003200141107     d c_F08           c                   const(x'38')
003300141107     d c_F09           c                   const(x'39')
003400141107     d c_F10           c                   const(x'3A')
003500141107     d c_F11           c                   const(x'3B')
003600141107     d c_F12           c                   const(x'3C')
003700141107     d c_F13           c                   const(x'B1')
003800141107     d c_F14           c                   const(x'B2')
003900141107     d c_F15           c                   const(x'B3')
004000141107     d c_F16           c                   const(x'B4')
004100141107     d c_F17           c                   const(x'B5')
004200141107     d c_F18           c                   const(x'B6')
004300141107     d c_F19           c                   const(x'B7')
004400141107     d c_F20           c                   const(x'B8')
004500141107     d c_F21           c                   const(x'B9')
004600141107     d c_F22           c                   const(x'BA')
004700141107     d c_F23           c                   const(x'BB')
004800141107     d c_F24           c                   const(x'BC')
004900141107     d c_Enter         c                   const(x'F1')
005000141107     d c_RollDown      c                   const(x'F4')
005100141107     d c_RollUp        c                   const(x'F5')
005200141107
005300141107     d Digits          c                   const('0123456789')
005400141107
005500141107      //---------------------------------------------------------------
005600141107      //?Definizione schiere.
005700141107      //---------------------------------------------------------------
005800141107
005900141107      // - Messaggi di errore
006000170309     d Msg             s             78    dim(20) ctdata perrcd(1)
006100141107
006200141107      //---------------------------------------------------------------
006300141107      //?Definizione aree dati.
006400141107      //---------------------------------------------------------------
006500141107
006600141107      // - Dati utente
006700141107     d §AzUte        e ds                  extname(AZUTE00F)
006800141107     d                                     dtaara
006900141107     d §DatiUte      e ds                  extname(dDatiUte)
007000141107     d                                     dtaara
007100141107
007200141107      //---------------------------------------------------------------
007300141107      //?Definizione strutture dati.
007400141107      //---------------------------------------------------------------
007500141107
007600141107      // - Status
007700141107     d Psds           sds
007800141107     d   SDSpgm          *proc
007900141107
008000141107      // - InfDS
008100141107     d InfDspF         ds
008200141107     d  dsp_aid              369    369a
008300141107     d  sfl_rrn              376    377i 0
008400141107     d  min_nrr              378    379i 0
008500141107     d  num_rcds             380    381i 0
008600141107
008700141107      // - Indicatori su DspF
008800141107     d IndDspF         ds
008900141107        // - Indicatori di errore in videata
009000141107     d  ErrMessage                    1n   overlay(IndDspF : 28)
009100141107        // - Indicatori di errore
009200150202     d  PosCurNCM                     1n   overlay(IndDspF : 50)
009300141107        // - Indicatori di errore generico
009400141107     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009500141107
009600141107     d WindDspF        ds                  inz
009700141107     d  WindDspFa              1     49    inz(*zero)
009800141107     d  WindDspFb             50     99    inz(*zero)
009900141107
010000141107      // - Parametri ricevuti
010100141107     d KPJBA         e ds
010200141107
010300150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
010400150112     d TRKC02DS      e ds
010500141107
010600150202      // - Crea WF Clienti in Campagna
010700150202     d TRKC51DS      e ds
010800141107
010900141107      // - Reperimento dati utente
011000141107     d TIBS34DS      e ds
011100141107
011101170309      // - DS del campo CMPFLO di ticmp00f
011102170309     d DCMP01        e ds
011103170309
011200141107      //---------------------------------------------------------------
011300141107      //?Definizione variabili globali.
011400141107      //---------------------------------------------------------------
011500141107
011600141107      // - Flags booleani
011700150122     d ErrGrave        s               n   inz(*off)
011800141107     d Fine            s               n   inz(*off)
011900141107     d wEnd            s               n   inz(*off)
012000141107     d wInzD02         s               n   inz(*off)
012100141107
012200141107      // - Indici di schiera
012300141107     d xx              s              4s 0 inz
012400141107
012500141107      // - Campi associati al video
012600141107     d Video           s              2a   inz('S2')
012700141107
012800141107      // - Campi di comodo data
012900141107     d Data_EUR        s               d   datfmt(*eur)
013000141107     d Data_ISO        s               d   datfmt(*iso)
013100141107
013200141107      // - Campi di comodo
013300150202     d comando         s            512a   varying
013400141107     d Oggi            s              8s 0 inz
013500141107
013600141107      //---------------------------------------------------------------
013700141107      //?Definizione procedure usate.
013800141107      //---------------------------------------------------------------
013900141107
014000141107      // - Sottomissione lavoro batch
014100141107     d bch10           pr                  extpgm('BCH10')
014200141107     d  kpjba                              likeds(KPJBA)
014300141107
014400141107       // - Interrogazione anagrafica campagne
014500150112     d TRKC02R         pr                  extpgm('TRKC02R')
014600141107     d  kpjba                              likeds(kpjba)
014700150112     d  trkc02ds                           likeds(TRKC02DS)
014800141107
014900141107      // - Richiamo diretto lavoro batch
015000150202     d TRKC51R         pr                  extpgm('TRKC51R')
015100141107     d  kpjba                              likeds(KPJBA)
015200141107
015300141107      //---------------------------------------------------------------
015400141107      //?Definizione Prototipi.
015500141107      //---------------------------------------------------------------
015600141107      /copy gaitrasrc/srcprotopr,TIBS34R
015700141107      /copy gaitrasrc/srcprotopr,SYSTEM
015800141107
015900141107      //---------------------------------------------------------------
016000141107      //?Definizione key-list.
016100141107      //---------------------------------------------------------------
016200141107
016300141107      //---------------------------------------------------------------
016400141107      //?M A I N - L I N E
016500141107      //---------------------------------------------------------------
016600141107
016700141107     c     *Entry        plist
016800141107     c                   parm                    kpjba
016900141107
017000141107      /free
017100141107
017200141107       //?Operazioni iniziali
017300141107       exsr RoutInz;
017400141107
017500141107       //?Gestione video
017600141107       DOW  Fine = *off;
017700141107         exsr GesD02;
017800141107       ENDDO;
017900141107
018000141107       //?Operazioni finali
018100141107       exsr RoutEnd;
018200141107
018300141107       //--------------------------------------------------------------
018400141107       //?Operazioni iniziali.
018500141107       //--------------------------------------------------------------
018600141107       BEGSR RoutInz;
018700141107
018800141107       //?Impostazione campi "fissi"
018900141107         V01pgm = SDSpgm;
019000141107         wInzD02 = *on;
019100141107
019200141107       //?Imposto oggi
019300141107         Oggi = %dec(%date());
019400141107
019500141107       //?Reperimento dati job
019600141107         exsr DatiJob;
019700150122
019800150122       //?Abilitato solo a utenti di SEDE
019900150122         IF  DUTpou <> 046;
020000150122           ErrGrave = *on;
020100150122           leavesr;
020200150122         ENDIF;
020300141107
020400141107       ENDSR;
020500141107
020600141107       //--------------------------------------------------------------
020700141107       //?Reperimento Dati del job (Utente/Operativi).
020800141107       //--------------------------------------------------------------
020900141107       BEGSR DatiJob;
021000141107
021100141107         in(E) §AzUte;
021200141107         IF  NOT %error;
021300141107           in(E) §DatiUte;
021400141107         ENDIF;
021500141107         IF  %error or RSut = *blanks;
021600141107           clear TIBS34ds;
021700141107           tibs34r(tibs34ds);
021800141107           in §AzUte;
021900141107           in §DatiUte;
022000141107         ENDIF;
022100141107
022200141107       ENDSR;
022300141107
022400141107       //--------------------------------------------------------------
022500141107       //?Gestione videata D02.
022600141107       //--------------------------------------------------------------
022700141107       BEGSR GesD02;
022800141107
022900141107       //?Inizializzazione videata
023000141107         IF  wInzD02;
023100141107           exsr InzD02;
023200141107           wInzD02 = *off;
023300141107         ENDIF;
023400150122
023500150122       //?Se errore grave emetto messaggio poi esco
023600150122         IF  ErrGrave;
023700150122           ErrMessage  = *on;
023800150122           ErrGenerico = *on;
023900150202           V02msg = Msg(02);
024000150122         ENDIF;
024100141107
024200141107       //?Emissione Testata
024300150202         write  KC50T01;
024400141107
024500141107       //?Emissione videata
024600150202         exfmt  KC50D02;
024700141107         reset ErrMessage;
024800141107         reset ErrGenerico;
024900141107         clear V02msg;
025000141107
025100141107         SELECT;
025200150122
025300150122       //?- Errore Grave esco
025400150122           WHEN  ErrGrave;
025500150122             Fine = *on;
025600141107
025700141107       //?- F03=Fine
025800141107           WHEN  dsp_aid = c_F03;
025900141107             exsr F03D02;
026000141107
026100141107       //?- F06=Conferma
026200141107           WHEN  dsp_aid = c_F06;
026300141107             exsr F06D02;
026400150202
026500150202       //?- F09=Visualizza File
026600150202           WHEN  dsp_aid = c_F09;
026700150202             exsr F09D02;
026800141107
026900141107       //?Invio
027000141107           OTHER;
027100141107             exsr ContrD02;
027200141107             IF  ErrGenerico;
027300141107               leavesr;
027400141107             ENDIF;
027500141107
027600141107         ENDSL;
027700141107
027800141107       ENDSR;
027900141107
028000141107       //--------------------------------------------------------------
028100141107       //?Inizializzazione Videata D02.
028200141107       //--------------------------------------------------------------
028300141107       BEGSR InzD02;
028400141107
028500141107       //?Pulizia videata
028600150202         clear KC50D02;
028700141107
028800141107       ENDSR;
028900141107
029000141107       //--------------------------------------------------------------
029100141107       //?Controlla Videata D02.
029200141107       //--------------------------------------------------------------
029300141107       BEGSR ContrD02;
029400141107
029500141107         WindDspF = IndDspF;
029600141107         reset WindDspFb;
029700141107         IndDspF  = WindDspF;
029800141107
029900141107       //?Campagna Commerciale Obbligatoria
030000141107         clear V02des;
030100141107         IF  V02ncm = *blanks;
030200141107           ErrMessage  = *on;
030300141107           ErrGenerico = *on;
030400141107           PosCurNCM   = *on;
030500150202           V02msg = Msg(01);
030600141107           leavesr;
030700141107         ENDIF;
030800141107       //?Ricerca Campagna Commerciale
030900141107         IF  %scan('?':V02ncm) > 0;
031000150112           clear TRKC02DS;
031100170309           IKC02ric = 'R';
031200150112           trkc02r (kpjba:trkc02ds);
031300141107           ErrGenerico = *on;
031400141107           PosCurNCM   = *on;
031500170313           V02ncm = %editc(OKC02ncm:'X');
031600141107         ENDIF;
031700141107       //?Accetto solo dati numerici
031800141107         xx = 1;
031900141107         FOR xx by 1 to %len(V02ncm);
032000141107           IF  %subst(V02ncm:xx:1) <> *blanks and
032100141107               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
032200141107             ErrMessage  = *on;
032300141107             ErrGenerico = *on;
032400141107             PosCurNCM   = *on;
032500150202             V02msg = Msg(01);
032600141107             leavesr;
032700141107           ENDIF;
032800141107         ENDFOR;
032900141107       //?Deve essere una Campagna Commerciale valida
033000141107         CMPncm = %dec(V02ncm:7:0);
033100141107         chain CMPncm TICMP01L;
033200141107         IF  not %found(TICMP01L);
033300141107           ErrMessage  = *on;
033400141107           ErrGenerico = *on;
033500141107           PosCurNCM   = *on;
033600150202           V02msg = Msg(01);
033700141107           leavesr;
033701170309         else ;
033702170309        // controllo se la campagna può essere scaricata sul file per il presidio vendite
033703170309           dcmp01 = cmpflo;
033704170309           If §CMPWFCMC = 'N';
033705170309              ErrMessage  = *on;
033706170309              ErrGenerico = *on;
033707170309              PosCurNCM   = *on;
033708170309              V02msg = Msg(03);
033709170309              leavesr;
033710170309           Endif ;
033800141107         ENDIF;
033900141107       //?Decodifico Campagna Commerciale
034000141107         V02des = CMPdes;
034100141107
034200141107       ENDSR;
034300141107
034400141107       //--------------------------------------------------------------
034500141107       //?Gestione tasto funzionale F03 da videata D02
034600141107       //?F03=Fine
034700141107       //--------------------------------------------------------------
034800141107       BEGSR F03D02;
034900141107
035000141107       //?Chiusura del programma
035100141107         Fine = *on;
035200141107
035300141107       ENDSR;
035400141107
035500141107       //--------------------------------------------------------------
035600141107       //?Gestione tasto funzionale F06 da videata D02
035700141107       //?F06=Conferma
035800141107       //--------------------------------------------------------------
035900141107       BEGSR F06D02;
036000141107
036100150202         clear TRKC51DS;
036200150202         IKC51ncm = %dec(V02ncm:7:0);
036300141107
036400150202         kpjbu = TRKC51DS;
036500150202         kcoaz = 'KC51';
036600141107         clear kbuff;
036700141107
036800141107       //?Lancio pgm di aggiornamento
036900141107       //?Richiamo interattivo
037000141107         IF  knmus = *all'1';
037100150202           TRKC51R(kpjba);
037200141107       //?Richiamo batch
037300141107         ELSE;
037400141107           BCH10(kpjba);
037500141107         ENDIF;
037600141107
037700141107       //?Chiusura del programma
037800141107         Fine = *on;
037900141107
038000141107       ENDSR;
038100150202
038200150202       //--------------------------------------------------------------
038300150202       //?Gestione tasto funzionale F09 da videata D02
038400150202       //?F09=Visualizza file
038500150202       //--------------------------------------------------------------
038600150202       BEGSR F09D02;
038700150202
038800150202         comando = 'DSPDBF FILE(GAITRAAZM/WFCMC00F) +
038900150202         mbr(*first) output(*)';
039000150202         IF  ExecuteCommand (comando) > 0 ;
039100150202           exsr RoutEnd;
039200150202         ENDIF;
039300150202
039400150202       ENDSR;
039500141107
039600141107       //--------------------------------------------------------------
039700141107       //?Operazioni finali.
039800141107       //--------------------------------------------------------------
039900141107       BEGSR RoutEnd;
040000141107
040100141107         *inLR = *on;
040200141107         return;
040300141107
040400141107       ENDSR;
040500141107
040600141107      /end-free
040700141107
040800141107       //--------------------------------------------------------------
040900141107       //?Schiere a tempo di compilazione.
041000141107       //--------------------------------------------------------------
041100141107
041200141107** -- MSG -------------------------------------------------------------------*
041300150202Campagna Commerciale errata                                                    1
041400150202Utente non abilitato alla Funzione.                                            2
041500170309Non è possibile effettuare la creazione del file per questa Campagna           3
