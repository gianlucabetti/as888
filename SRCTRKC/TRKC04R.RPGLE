000100141107      //--------------------------------------------------------------
000200150112      //?TRKC04R - OBIETTIVO DA CONFRONTO FATTURAZIONE
000300141107      //--------------------------------------------------------------
000400141107
000500141107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141107     h dftactgrp(*no) actgrp(*caller) bnddir('TRUL')
000700141107
000800141107      //---------------------------------------------------------------
000900141107      //?Dichiarazione file.
001000141107      //---------------------------------------------------------------
001100141107
001200141107      // - File anagrafiche Campagne
001300141107     fTICMP01L  if   e           k disk
001400141107
001500150202      // - Video
001600150112     fTRKC04D   cf   e             workstn
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
005500141107     d wlib            c                   const('EDPCONF_TA')
005600141107
005700141107      //---------------------------------------------------------------
005800141107      //?Definizione schiere.
005900141107      //---------------------------------------------------------------
006000141107
006100141107      // - Messaggi di errore
006200141107     d Msg             s             78    dim(20) ctdata perrcd(1)
006300141107
006400141107      //---------------------------------------------------------------
006500141107      //?Definizione aree dati.
006600141107      //---------------------------------------------------------------
006700141107
006800141107      // - Dati utente
006900141107     d §AzUte        e ds                  extname(AZUTE00F)
007000141107     d                                     dtaara
007100141107     d §DatiUte      e ds                  extname(dDatiUte)
007200141107     d                                     dtaara
007300141107
007400141107      //---------------------------------------------------------------
007500141107      //?Definizione strutture dati.
007600141107      //---------------------------------------------------------------
007700141107
007800141107      // - Status
007900141107     d Psds           sds
008000141107     d   SDSpgm          *proc
008100141107
008200141107      // - InfDS
008300141107     d InfDspF         ds
008400141107     d  dsp_aid              369    369a
008500141107     d  sfl_rrn              376    377i 0
008600141107     d  min_nrr              378    379i 0
008700141107     d  num_rcds             380    381i 0
008800141107
008900141107      // - Indicatori su DspF
009000141107     d IndDspF         ds
009100141107        // - Indicatori di errore in videata
009200141107     d  ErrMessage                    1n   overlay(IndDspF : 28)
009300141107        // - Indicatori di errore
009400141107     d  PosCurLIB                     1n   overlay(IndDspF : 50)
009500141107     d  PosCurNCM                     1n   overlay(IndDspF : 51)
009600141107        // - Indicatori di errore generico
009700141107     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009800141107
009900141107     d WindDspF        ds                  inz
010000141107     d  WindDspFa              1     49    inz(*zero)
010100141107     d  WindDspFb             50     99    inz(*zero)
010200141107
010300141107      // - Parametri ricevuti
010400141107     d KPJBA         e ds
010500141107
010600150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
010700150112     d TRKC02DS      e ds
010800141107
010900141107      // - Aggiornamento Obiettivo confronto fatturazione
011000150112     d TRKC05DS      e ds
011100141107
011200141107      // - Reperimento dati utente
011300141107     d TIBS34DS      e ds
011400141107
011500141107      //---------------------------------------------------------------
011600141107      //?Definizione variabili globali.
011700141107      //---------------------------------------------------------------
011800141107
011900141107      // - Flags booleani
012000150122     d ErrGrave        s               n   inz(*off)
012100141107     d Fine            s               n   inz(*off)
012200141107     d wEnd            s               n   inz(*off)
012300141107     d wInzD02         s               n   inz(*off)
012400141107
012500141107      // - Indici di schiera
012600141107     d xx              s              4s 0 inz
012700141107
012800141107      // - Campi associati al video
012900141107     d Video           s              2a   inz('S2')
013000141107
013100141107      // - Campi di comodo data
013200141107     d Data_EUR        s               d   datfmt(*eur)
013300141107     d Data_ISO        s               d   datfmt(*iso)
013400141107
013500141107      // - Campi di comodo
013600141107     d cmd             s            512a   varying
013700150505     d*// Oggi            s              8s 0 inz
013800141107
013900141107      //---------------------------------------------------------------
014000141107      //?Definizione procedure usate.
014100141107      //---------------------------------------------------------------
014200141107
014300141107      // - Sottomissione lavoro batch
014400141107     d bch10           pr                  extpgm('BCH10')
014500141107     d  kpjba                              likeds(KPJBA)
014600141107
014700141107       // - Interrogazione anagrafica campagne
014800150112     d TRKC02R         pr                  extpgm('TRKC02R')
014900141107     d  kpjba                              likeds(kpjba)
015000150112     d  trkc02ds                           likeds(TRKC02DS)
015100141107
015200141107      // - Richiamo diretto lavoro batch
015300150122     d TRKC05C         pr                  extpgm('TRKC05C')
015400141107     d  kpjba                              likeds(KPJBA)
015500141107
015600141107      //---------------------------------------------------------------
015700141107      //?Definizione Prototipi.
015800141107      //---------------------------------------------------------------
015900141107      /copy gaitrasrc/srcprotopr,TIBS34R
016000141107      /copy gaitrasrc/srcprotopr,SYSTEM
016100141107
016200141107      //---------------------------------------------------------------
016300141107      //?Definizione key-list.
016400141107      //---------------------------------------------------------------
016500141107
016600141107      //---------------------------------------------------------------
016700141107      //?M A I N - L I N E
016800141107      //---------------------------------------------------------------
016900141107
017000141107     c     *Entry        plist
017100141107     c                   parm                    kpjba
017200141107
017300141107      /free
017400141107
017500141107       //?Operazioni iniziali
017600141107       exsr RoutInz;
017700141107
017800141107       //?Gestione video
017900141107       DOW  Fine = *off;
018000141107         exsr GesD02;
018100141107       ENDDO;
018200141107
018300141107       //?Operazioni finali
018400141107       exsr RoutEnd;
018500141107
018600141107       //--------------------------------------------------------------
018700141107       //?Operazioni iniziali.
018800141107       //--------------------------------------------------------------
018900141107       BEGSR RoutInz;
019000141107
019100141107       //?Impostazione campi "fissi"
019200141107         V01pgm = SDSpgm;
019300141107         wInzD02 = *on;
019400141107
019500150505       ////?Imposto oggi
019600150505         //Oggi = %dec(%date());
019700141107
019800141107       //?Reperimento dati job
019900141107         exsr DatiJob;
020000150122
020100150122       //?Abilitato solo a utenti di SEDE
020200150122         IF  DUTpou <> 046;
020300150122           ErrGrave = *on;
020400150122           leavesr;
020500150122         ENDIF;
020600141107
020700141107       ENDSR;
020800141107
020900141107       //--------------------------------------------------------------
021000141107       //?Reperimento Dati del job (Utente/Operativi).
021100141107       //--------------------------------------------------------------
021200141107       BEGSR DatiJob;
021300141107
021400141107         in(E) §AzUte;
021500141107         IF  NOT %error;
021600141107           in(E) §DatiUte;
021700141107         ENDIF;
021800141107         IF  %error or RSut = *blanks;
021900141107           clear TIBS34ds;
022000141107           tibs34r(tibs34ds);
022100141107           in §AzUte;
022200141107           in §DatiUte;
022300141107         ENDIF;
022400141107
022500141107       ENDSR;
022600141107
022700141107       //--------------------------------------------------------------
022800141107       //?Gestione videata D02.
022900141107       //--------------------------------------------------------------
023000141107       BEGSR GesD02;
023100141107
023200141107       //?Inizializzazione videata
023300141107         IF  wInzD02;
023400141107           exsr InzD02;
023500141107           wInzD02 = *off;
023600141107         ENDIF;
023700150122
023800150122       //?Se errore grave emetto messaggio poi esco
023900150122         IF  ErrGrave;
024000150122           ErrMessage  = *on;
024100150122           ErrGenerico = *on;
024200150122           V02msg = Msg(03);
024300150122         ENDIF;
024400141107
024500141107       //?Emissione Testata
024600150112         write  KC04T01;
024700141107
024800141107       //?Emissione videata
024900150112         exfmt  KC04D02;
025000141107         reset ErrMessage;
025100141107         reset ErrGenerico;
025200141107         clear V02msg;
025300141107
025400141107         SELECT;
025500150122
025600150122       //?- Errore Grave esco
025700150122           WHEN  ErrGrave;
025800150122             Fine = *on;
025900141107
026000141107       //?- F03=Fine
026100141107           WHEN  dsp_aid = c_F03;
026200141107             exsr F03D02;
026300141107
026400141107       //?- F06=Conferma
026500141107           WHEN  dsp_aid = c_F06;
026600141107             exsr F06D02;
026700141107
026800141107       //?Invio
026900141107           OTHER;
027000141107             exsr ContrD02;
027100141107             IF  ErrGenerico;
027200141107               leavesr;
027300141107             ENDIF;
027400141107
027500141107         ENDSL;
027600141107
027700141107       ENDSR;
027800141107
027900141107       //--------------------------------------------------------------
028000141107       //?Inizializzazione Videata D02.
028100141107       //--------------------------------------------------------------
028200141107       BEGSR InzD02;
028300141107
028400141107       //?Pulizia videata
028500150112         clear KC04D02;
028600141107
028700141107         V02lib = wlib;
028800141107
028900141107       ENDSR;
029000141107
029100141107       //--------------------------------------------------------------
029200141107       //?Controlla Videata D02.
029300141107       //--------------------------------------------------------------
029400141107       BEGSR ContrD02;
029500141107
029600141107         WindDspF = IndDspF;
029700141107         reset WindDspFb;
029800141107         IndDspF  = WindDspF;
029900141107
030000141107       //?Campagna Commerciale Obbligatoria
030100141107         clear V02des;
030200141107         IF  V02ncm = *blanks;
030300141107           ErrMessage  = *on;
030400141107           ErrGenerico = *on;
030500141107           PosCurNCM   = *on;
030600141107           V02msg = Msg(02);
030700141107           leavesr;
030800141107         ENDIF;
030900141107       //?Ricerca Campagna Commerciale
031000141107         IF  %scan('?':V02ncm) > 0;
031100150112           clear TRKC02DS;
031200150112           IKC02ric = 'S';
031300150112           trkc02r (kpjba:trkc02ds);
031400141107           ErrGenerico = *on;
031500141107           PosCurNCM   = *on;
031600150112           V02ncm = %editc(OKC02ncm:'X');
031700141107         ENDIF;
031800141107       //?Accetto solo dati numerici
031900141107         xx = 1;
032000141107         FOR xx by 1 to %len(V02ncm);
032100141107           IF  %subst(V02ncm:xx:1) <> *blanks and
032200141107               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
032300141107             ErrMessage  = *on;
032400141107             ErrGenerico = *on;
032500141107             PosCurNCM   = *on;
032600141107             V02msg = Msg(02);
032700141107             leavesr;
032800141107           ENDIF;
032900141107         ENDFOR;
033000141107       //?Deve essere una Campagna Commerciale valida
033100141107         CMPncm = %dec(V02ncm:7:0);
033200141107         chain CMPncm TICMP01L;
033300141107         IF  not %found(TICMP01L);
033400141107           ErrMessage  = *on;
033500141107           ErrGenerico = *on;
033600141107           PosCurNCM   = *on;
033700141107           V02msg = Msg(02);
033800141107           leavesr;
033900141107         ENDIF;
034000141107       //?Decodifico Campagna Commerciale
034100141107         V02des = CMPdes;
034200150505       ////?Campagna Commerciale già finita
034300150505         //IF  CMPdfc < Oggi;
034400150505         //  ErrMessage  = *on;
034500150505         //  ErrGenerico = *on;
034600150505         //  PosCurNCM   = *on;
034700150505         //  V02msg = %trim(Msg(02)) + '. Già chiusa';
034800150505         //  leavesr;
034900150505         //ENDIF;
035000141107
035100141107       //?Libreria Obbligatoria
035200141107         IF  V02lib = *zeros;
035300141107           ErrMessage  = *on;
035400141107           ErrGenerico = *on;
035500141107           PosCurLIB   = *on;
035600141107           V02msg = Msg(01);
035700141107           leavesr;
035800141107         ENDIF;
035900141107
036000141107       //?Libreria valida
036100141107         cmd = 'CHKOBJ obj(QSYS/' +
036200141107                %trim(V02lib) + ') objtype(*LIB)';
036300141107         IF  ExecuteCommand (cmd) = 0 ;
036400141107         ELSE;
036500141107           ErrMessage  = *on;
036600141107           ErrGenerico = *on;
036700141107           PosCurLIB   = *on;
036800141107           V02msg = Msg(01);
036900141107           leavesr;
037000141107         ENDIF;
037100141107
037200141107       ENDSR;
037300141107
037400141107       //--------------------------------------------------------------
037500141107       //?Gestione tasto funzionale F03 da videata D02
037600141107       //?F03=Fine
037700141107       //--------------------------------------------------------------
037800141107       BEGSR F03D02;
037900141107
038000141107       //?Chiusura del programma
038100141107         Fine = *on;
038200141107
038300141107       ENDSR;
038400141107
038500141107       //--------------------------------------------------------------
038600141107       //?Gestione tasto funzionale F06 da videata D02
038700141107       //?F06=Conferma
038800141107       //--------------------------------------------------------------
038900141107       BEGSR F06D02;
039000141107
039100150112         clear TRKC05DS;
039200150122         IKC05lib = V02lib;
039300150112         IKC05ncm = %dec(V02ncm:7:0);
039400141107
039500150112         kpjbu = TRKC05DS;
039600150112         kcoaz = 'KC05';
039700141107         clear kbuff;
039800141107
039900141107       //?Lancio pgm di aggiornamento
040000141107       //?Richiamo interattivo
040100141107         IF  knmus = *all'1';
040200150122           TRKC05C(kpjba);
040300141107       //?Richiamo batch
040400141107         ELSE;
040500141107           BCH10(kpjba);
040600141107         ENDIF;
040700141107
040800141107       //?Chiusura del programma
040900141107         Fine = *on;
041000141107
041100141107       ENDSR;
041200141107
041300141107       //--------------------------------------------------------------
041400141107       //?Operazioni finali.
041500141107       //--------------------------------------------------------------
041600141107       BEGSR RoutEnd;
041700141107
041800141107         *inLR = *on;
041900141107         return;
042000141107
042100141107       ENDSR;
042200141107
042300141107      /end-free
042400141107
042500141107       //--------------------------------------------------------------
042600141107       //?Schiere a tempo di compilazione.
042700141107       //--------------------------------------------------------------
042800141107
042900141107** -- MSG -------------------------------------------------------------------*
043000141107Libreria errata o non esistente                                                1
043100141107Campagna Commerciale errata                                                    2
043200150122Utente non abilitato alla Funzione.                                            3
