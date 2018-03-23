000100141106      //--------------------------------------------------------------
000200150112      //?TRKC03R - STORICO
000300141106      //--------------------------------------------------------------
000400141106
000500141106     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141106
000700141106      //---------------------------------------------------------------
000800141106      //?Dichiarazione file.
000900141106      //---------------------------------------------------------------
001000150126
001100150126      // - Clienti in Campagna Commerciale
001200150126     fTICMC01L  if   e           k disk
001300141106
001400141106      // - File Fasi Campagna
001500141113     fTICMF02L  if   e           k disk
001600141114
001700141114      // - Anagrafica Campagna
001800141114     fTICMP01L  if   e           k disk
001900141107
002000141107      // - Anagrafica Potenziali
002100141107     fTNCPO01L  if   e           k disk
002200141106
002300141106      // - Video Storico Fasi
002400150112     fTRKC03D   cf   e             workstn
002500150112     f                                     sfile(KC03S02 : S02nrr)
002600141106     f                                     indds(IndDspF)
002700141106     f                                     infds(InfDspF)
002800141106
002900141106      //---------------------------------------------------------------
003000141106      //?Definizione costanti.
003100141106      //---------------------------------------------------------------
003200141106
003300141106      // - Tasti funzionali a video
003400141106     d c_F01           c                   const(x'31')
003500141106     d c_F02           c                   const(x'32')
003600141106     d c_F03           c                   const(x'33')
003700141106     d c_F04           c                   const(x'34')
003800141106     d c_F05           c                   const(x'35')
003900141106     d c_F06           c                   const(x'36')
004000141106     d c_F07           c                   const(x'37')
004100141106     d c_F08           c                   const(x'38')
004200141106     d c_F09           c                   const(x'39')
004300141106     d c_F10           c                   const(x'3A')
004400141106     d c_F11           c                   const(x'3B')
004500141106     d c_F12           c                   const(x'3C')
004600141106     d c_F13           c                   const(x'B1')
004700141106     d c_F14           c                   const(x'B2')
004800141106     d c_F15           c                   const(x'B3')
004900141106     d c_F16           c                   const(x'B4')
005000141106     d c_F17           c                   const(x'B5')
005100141106     d c_F18           c                   const(x'B6')
005200141106     d c_F19           c                   const(x'B7')
005300141106     d c_F20           c                   const(x'B8')
005400141106     d c_F21           c                   const(x'B9')
005500141106     d c_F22           c                   const(x'BA')
005600141106     d c_F23           c                   const(x'BB')
005700141106     d c_F24           c                   const(x'BC')
005800141106     d c_Enter         c                   const(x'F1')
005900141106     d c_RollDown      c                   const(x'F4')
006000141106     d c_RollUp        c                   const(x'F5')
006100150224
006200150224     d FaseObjCf       c                   const(' CF')
006300141106
006400141106      //---------------------------------------------------------------
006500141106      //?Definizione schiere.
006600141106      //---------------------------------------------------------------
006700141114
006800141114      // - Messaggi di errore
006900141118     d sk_Msg          s             78    dim(20) ctdata perrcd(1)
007000141106
007100141106      //---------------------------------------------------------------
007200141106      //?Definizione aree dati.
007300141106      //---------------------------------------------------------------
007400141106
007500141106      // - Dati utente
007600141106     d §AzUte        e ds                  extname(AZUTE00F)
007700141106     d                                     dtaara
007800141106     d §DatiUte      e ds                  extname(dDatiUte)
007900141106     d                                     dtaara
008000141106
008100141106      //---------------------------------------------------------------
008200141106      //?Definizione strutture dati.
008300141106      //---------------------------------------------------------------
008400141106
008500141106      // - Status
008600141106     d Psds           sds
008700141106     d   SDSpgm          *proc
008800141106
008900141106      // - InfDS
009000141106     d InfDspF         ds
009100141106     d  dsp_aid              369    369a
009200141106     d  sfl_rrn              376    377i 0
009300141106     d  min_nrr              378    379i 0
009400141106     d  num_rcds             380    381i 0
009500141106
009600141106      // - Indicatori su DspF
009700141106     d IndDspF         ds
009800150123        // - Attributi di Visualizzazione
009900150123     d  EvidenzF18                     n   overlay(IndDspF : 18)
010000141106        // - Indicatori di errore in videata
010100141118     d  ErrMessage                     n   overlay(IndDspF : 28)
010200141106        // - Indicatori di gestione del subfile
010300141118     d  SflDsp                         n   overlay(IndDspF : 30)
010400141118     d  SflDspCtl                      n   overlay(IndDspF : 31)
010500141118     d  SflNxtChg                      n   overlay(IndDspF : 32)
010600141118     d  SflEnd                         n   overlay(IndDspF : 33)
010700141106        // - Indicatori di visualizzazione
010800141118     d  Cliente                        n   overlay(IndDspF : 41)
010900141118     d  Potenziale                     n   overlay(IndDspF : 42)
011000141118     d  Unificante                     n   overlay(IndDspF : 43)
011100141118        // - Indicatori di errore
011200141118     d  PosCurOPZ                      n   overlay(IndDspF : 50)
011300141106        // - Indicatori di errore generico
011400141118     d  ErrGenerico                    n   overlay(IndDspF : 99)
011500141106
011600141106     d WindDspF        ds                  inz
011700141106     d  WindDspFa              1     49    inz(*zero)
011800141106     d  WindDspFb             50     99    inz(*zero)
011900141106
012000141106      // - Parametri ricevuti
012100141106     d KPJBA         e ds
012200150112     d TRKC03DS      e ds
012300141106
012400141106      // - Ricerca/Controllo tabelle
012500141106     d TIBS02DS      e ds                  inz
012600141106
012700141106      // - Reperimento dati utente
012800141106     d TIBS34DS      e ds
012900141114
013000141114      // - Reperimento dati Anagrafica Clienti
013100141114      /copy gaitrasrc/srcprotopi,TIBS69R
013200141106
013300150126       // -?Tabella ECM - Causale Chiusura Campagna?
013400150126     d dECM          e ds                  inz
013500141106      // - Tabella FCM - Fase Campagna
013600141106     d dFCM          e ds
013700141107
013800141107      // - DS x data lunga 8
013900141107     d                 ds
014000141107     d  Data8                  1      8  0
014100141107     d  Data8aa                1      4  0
014200141107     d  Data8mm                5      6  0
014300141107     d  Data8gg                7      8  0
014400141107
014500141107      // - DS x data lunga 6
014600141107     d                 ds
014700141107     d  Data6                  1      6  0
014800141107     d  Data6gg                1      2  0
014900141107     d  Data6mm                3      4  0
015000141107     d  Data6aa                5      6  0
015100141106
015200141106      //---------------------------------------------------------------
015300141106      //?Definizione variabili globali.
015400141106      //---------------------------------------------------------------
015500141106
015600141106      // - Flags booleani
015700141106     d EndS02          s               n   inz(*off)
015800141106     d Fine            s               n   inz(*off)
015900141106     d wEnd            s               n   inz(*off)
016000141106     d wInzS02         s               n   inz(*off)
016100141106     d wInzD03         s               n   inz(*off)
016200141106
016300141106      // - Campi associati al video
016400141106     d Video           s              2a   inz('S2')
016500141106     d S02nrr          s              4s 0 inz
016600141106
016700141106      // - Campi di comodo data
016800141106     d Data_EUR        s               d   datfmt(*eur)
016900141106     d Data_ISO        s               d   datfmt(*iso)
017000141106
017100141106      // - Campi di comodo
017200141106     d Oggi            s              8s 0 inz
017300150116     d wPos            s              3  0 inz
017400141106
017500141106      //---------------------------------------------------------------
017600141106      //?Definizione procedure usate.
017700141106      //---------------------------------------------------------------
017800141106
017900141106      //---------------------------------------------------------------
018000141106      //?Definizione Prototipi.
018100141106      //---------------------------------------------------------------
018200141106      /copy gaitrasrc/srcprotopr,TIBS02R
018300141106      /copy gaitrasrc/srcprotopr,TIBS34R
018400141114      /copy gaitrasrc/srcprotopr,TIBS69R
018500141118
018600141118       // -?Gestione Note (per visualizzazione)?
018700150112     d TRKC10ds      e ds                  inz
018800150112     d   iKC10tla    e                     inz('L')
018900150123     d*//iKC10flm    e                     inz('I')
019000150112     d TRKC10R         pr                  extpgm('TRKC10R')
019100141118     d   kpjba                             likeds(KPJBA)
019200150112     d   trkc10ds                          likeds(TRKC10ds)
019300141106
019400141106      //---------------------------------------------------------------
019500141106      //?Definizione key-list.
019600141106      //---------------------------------------------------------------
019700150126
019800150126       // -?File TICMC01L?
019900150126     d keyTICMC01    e ds                  extname(TICMC01L : *key)
020000150126     d                                     prefix(k_)   inz
020100141106
020200141106      //---------------------------------------------------------------
020300141106      //?M A I N - L I N E
020400141106      //---------------------------------------------------------------
020500141106
020600141106     c     *Entry        plist
020700141106     c                   parm                    kpjba
020800150112     c                   parm                    TRKC03ds
020900141106
021000141106      /free
021100141106
021200141106       //?Operazioni iniziali
021300141106       exsr RoutInz;
021400141114
021500141114       //?Controllo dati passati
021600141114       exsr Controlla;
021700141106
021800141106       //?Gestione video
021900141106       DOW  Fine = *off;
022000141106         SELECT;
022100141106           WHEN  Video = 'S2';
022200141106             exsr GesS02;
022300141106           OTHER;
022400141106             Fine = *on;
022500141106         ENDSL;
022600141106       ENDDO;
022700141106
022800141106       //?Operazioni finali
022900141106       exsr RoutEnd;
023000141106
023100141106       //--------------------------------------------------------------
023200141106       //?Operazioni iniziali.
023300141106       //--------------------------------------------------------------
023400141106       BEGSR RoutInz;
023500141106
023600141106       //?Impostazione campi "fissi"
023700141106         V01pgm = SDSpgm;
023800141106         Video = 'S2';
023900141106         wInzS02 = *on;
024000141104
024100141106       //?Imposto oggi
024200141106         Oggi = %dec(%date());
024300141106
024400141106       //?Reperimento dati job
024500141106         exsr DatiJob;
024600141106
024700141106       ENDSR;
024800141106
024900141106       //--------------------------------------------------------------
025000141106       //?Reperimento Dati del job (Utente/Operativi).
025100141106       //--------------------------------------------------------------
025200141106       BEGSR DatiJob;
025300141106
025400141106         in(E) §AzUte;
025500141106         IF  NOT %error;
025600141106           in(E) §DatiUte;
025700141106         ENDIF;
025800141106         IF  %error or RSut = *blanks;
025900141106           clear TIBS34ds;
026000141106           tibs34r(tibs34ds);
026100141106           in §AzUte;
026200141106           in §DatiUte;
026300141106         ENDIF;
026400141106
026500141106       ENDSR;
026600141114
026700141114       //--------------------------------------------------------------
026800141114       //?Controllo i dati passati.
026900141114       //--------------------------------------------------------------
027000141114       BEGSR Controlla;
027100141114
027200141114       //?Campagna obbligatoria
027300150112         IF  IKC03ncm = 0;
027400150112           OKC03err = '1';
027500150112           OKC03msg = sk_Msg(01);
027600141114           exsr RoutEnd;
027700141114         ENDIF;
027800141114       //?Deve essere una campagna valida
027900150112         chain (IKC03ncm) TICMP01L;
028000141114         IF  not %found(TICMP01L) or CMPdfc < Oggi;
028100150112           OKC03err = '1';
028200150112           OKC03msg = sk_Msg(01);
028300141114           exsr RoutEnd;
028400141114         ENDIF;
028500141114
028600141114       //?Almeno 1 tra KSU/KSC/CPO deve esserci
028700150112         IF  IKC03ksu = 0 and IKC03ksc = 0 and IKC03cpo = 0;
028800150112           OKC03err = '1';
028900150112           OKC03msg = sk_Msg(02);
029000141114           exsr RoutEnd;
029100141114         ENDIF;
029200141114       //?Deve essere una codice valido
029300150112         IF  IKC03ksu > 0;
029400141114           clear TIBS69DS;
029500150112           I69kac = %dec(IKC03ksu:7:0);
029600141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
029700141114           IF  O69err <> *blanks;
029800150112             OKC03err = '1';
029900150112             OKC03msg = sk_Msg(02);
030000141114             exsr RoutEnd;
030100141114           ENDIF;
030200141114         ENDIF;
030300150112         IF  IKC03ksc > 0;
030400141114           clear TIBS69DS;
030500150112           I69kac = %dec(IKC03ksc:7:0);
030600141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
030700141114           IF  O69err <> *blanks;
030800150112             OKC03err = '1';
030900150112             OKC03msg = sk_Msg(02);
031000141114             exsr RoutEnd;
031100141114           ENDIF;
031200141114         ENDIF;
031300150112         IF  IKC03cpo > 0;
031400150112           chain (IKC03cpo) TNCPO01L;
031500141114           IF  not %found(TNCPO01L);
031600150112             OKC03err = '1';
031700150112             OKC03msg = sk_Msg(02);
031800141114             exsr RoutEnd;
031900141114           ENDIF;
032000141114         ENDIF;
032100141114
032200141114       ENDSR;
032300141106
032400141106       //--------------------------------------------------------------
032500141106       //?Gestione videata S02.
032600141106       //--------------------------------------------------------------
032700141106       BEGSR GesS02;
032800141106
032900141106       //?Inizializzazione videata
033000141106         IF  wInzS02;
033100141106           exsr InzS02;
033200141106           wInzS02 = *off;
033300141106           SflEnd = *on;
033400141106         ENDIF;
033500141106
033600141106       //?Visualizzazione del SFL (se ci sono dati)
033700141106         SflDsp = (S02nrr <= *zeros);
033800141106
033900141106       //?Posizionamento cursore
034000141106         SELECT;
034100141106         WHEN  S02nrr > *zero and V02rcd = *zeros;
034200141106           V02rcd = 1;
034300141106         WHEN S02nrr = *zeros;
034400141106           clear V02rcd;
034500141106         OTHER;
034600141106           V02rcd = S02nrr;
034700141106         ENDSL;
034800141106
034900141106         V02csr = V02rcd;
035000141107
035100141106
035200141106       //?Emissione Testata e Piede con tasti funzionali abilitati
035300150112         write  KC03T01;
035400150112         write  KC03P02;
035500141106
035600141106       //?Emissione videata
035700150112         exfmt  KC03C02;
035800141106         reset ErrMessage;
035900141106         reset ErrGenerico;
036000141106
036100141106         SELECT;
036200141106
036300141113       //?- F12=Ritorno
036400141113           WHEN  dsp_aid = c_F12;
036500141113             exsr F12S02;
036600141118
036700141118       //?- F18=Note (tutte)
036800141118           WHEN  dsp_aid = c_F18;
036900150126             clear  keyTICMC01;
037000150126             k_CMCncm = V02ncm;
037100150126             k_CMCksu = V02ksu;
037200150126             k_CMCksc = V02ksc;
037300150126             k_CMCcpo = V02cpo;
037400150126             chain  %kds( keyTICMC01 )  TICMC000;
037500150112             reset  TRKC10ds;
037600150126             //iKC10acm = *all'9';
037700150126             if  %found( TICMC01L );
037800150126               iKC10acm = CMCufe;
037900150126             endif;
038000141118             exsr  sr_F18S02;
038100141106
038200141106       //?Invio
038300141106           OTHER;
038400150114             IF  S02nrr > 0;
038500150114               exsr  sr_OpzS02;
038600150114             ENDIF;
038700141106
038800141106         ENDSL;
038900141106
039000141106       ENDSR;
039100141106
039200141106       //--------------------------------------------------------------
039300141106       //?Inizializzazione videata S02.
039400141106       //--------------------------------------------------------------
039500141106       BEGSR InzS02;
039600141106
039700141106         EndS02 = *off;
039800141106         Cliente = *off;
039900141106         Potenziale = *off;
040000141107         Unificante = *off;
040100141118
040200141118       //?Campagna
040300150112         V02ncm = iKC03ncm;
040400141118         V02des = CMPdes;
040500141118
040600141118       //?Visualizzo Unificante/Cliente/Potenziale
040700141118         SELECT;
040800150112         WHEN  IKC03ksu > 0;
040900141118           Unificante = *on;
041000150112           V02ksu = IKC03ksu;
041100141118           V02rag = ACOrag;
041200150112         WHEN  IKC03ksc > 0;
041300141118           Cliente = *on;
041400150112           V02ksc = IKC03ksc;
041500141118           V02rag = ACOrag;
041600150112         WHEN  IKC03cpo > 0;
041700141118           Potenziale = *on;
041800150112           V02cpo = IKC03cpo;
041900141118           V02cpod = CPOrag;
042000141118         ENDSL;
042100141106
042200141106       //?Pulizia subfile
042300141106         exsr PulS02;
042400141106
042500141106       //?Caricamento subfile
042600141106         exsr Ries02;
042700141106
042800141106       ENDSR;
042900141106
043000141106       //--------------------------------------------------------------
043100141106       //?Pulizia Subfile S02.
043200141106       //--------------------------------------------------------------
043300141106       BEGSR PulS02;
043400141106
043500141106       //?Pulizia subfile
043600141106         SflDsp    = *on;
043700141106         SflDspCtl = *on;
043800150112         write KC03C02;
043900141106         SflDspCtl = *off;
044000141106         SflEnd    = *off;
044100141106
044200141106         clear V02rcd;
044300141106         clear V02csr;
044400141106         clear S02nrr;
044500141106
044600141106         ErrMessage  = *off;
044700141106         ErrGenerico = *off;
044800141106
044900141106         WindDspF = IndDspF;
045000141106         reset WindDspFb;
045100141106         IndDspF  = WindDspF;
045200141106
045300141106       ENDSR;
045400141104
045500141106       //--------------------------------------------------------------
045600141106       //?Caricamento Subfile S02.
045700141106       //--------------------------------------------------------------
045800141106       BEGSR RieS02;
045900141106
046000141106         EndS02 = *off;
046100141106
046200141106         //?Leggo file TICMF
046300150112         setll (IKC03ncm:IKC03ksu:IKC03ksc:IKC03cpo) TICMF02L;
046400150112         reade (IKC03ncm:IKC03ksu:IKC03ksc:IKC03cpo) TICMF02L;
046500141113         DOW  not %eof(TICMF02L);
046600141104         //?Carico i dati nel subfile
046700141104           exsr CarS02;
046800150112           reade (IKC03ncm:IKC03ksu:IKC03ksc:IKC03cpo) TICMF02L;
046900141104         ENDDO;
047000150123
047100150123         // -?Verifica esistenza di Note inserite?
047200150123         //  ?(di qualunque fase...)?
047300150127         //  ?per (ri)mettere in Reverse Image l'indicazione F18?
047400150127         exsr  sr_CtrlNote;
047500141104
047600141104       ENDSR;
047700141104
047800141104       //--------------------------------------------------------------
047900141104       //?Carico i dati nel Subfile S02.
048000141104       //--------------------------------------------------------------
048100141104       BEGSR CarS02;
048200141104
048300150112         clear  KC03S02;
048400141104
048500141104       //?Imposto i campi
048600150128         // -?Fase Campagna?
048700141118         VH2acm = CMFacm;
048800141118
048900150128         // -?Decodifica Fase Campagna?
049000141106         clear dFCM;
049100141106         clear TIBS02DS;
049200141106         T02mod = 'C';
049300141106         T02cod = 'FCM';
049400141112         T02ke1 = CMFacm;
049500141106         T02sif = KNSIF;
049600141106         TNTBE_RicercaControllo  (kpjba : tibs02ds);
049700141106         IF  T02err = *blanks;
049800141107           dFCM = T02uni;
049900141106         ENDIF;
050000141106         VS2des = §FCMdes;
050100141107
050200150128         // -?Percentuale o Causale Chiusura?
050300141125         //VS2obj = CMFpea;
050400150126         clear dECM;
050500150119         select;
050600150128           // -?% da visualizzare   (§FCMPEAS = *blank) &?
050700150128           //  ?Fase da confrontare (CMFNOCF  = *blank)?
050800150128           when  §FCMpeaS <> 'N'  and  CMFnoCF <> 'N';
050900150119             VS2obj = %editc( CMFpea : 'L' );
051000150128           // -?% da NON visualizzare   (§FCMPEAS = "N") &?
051100150128           //  ?Causale Chiusura        (CMFCCH  <> *blank)?
051200150128           when  §FCMpeaS =  'N'  and  CMFcch  <> *blank;
051300150123             VS2obj = '--' + CMFcch + '--';
051400150123             //wPos = ( ( %len(VS2obj) - %len(CMFcch) ) / 2 ) + 1;
051500150123             //%subst( VS2obj : wPos ) = CMFcch;
051600150126             clear TIBS02DS;
051700150126             T02mod = 'C';
051800150126             T02cod = 'ECM';
051900150126             T02ke1 = CMFcch;
052000150126             T02sif = KNSIF;
052100150126             TNTBE_RicercaControllo  (kpjba : tibs02ds);
052200150126             if  T02err = *blanks;
052300150126               dECM = T02uni;
052400150126             endif;
052500150126             VS2ecmD = §ECMdes;
052600150119         endsl;
052700150128
052800150128         // -?Confronto Fatturato?
052900150224         IF  CMFacm = FaseObjCf;
053000150128         If  CMFaaCF <> *zero  or  CMFmmCF <> *zero;
053100150224           VS2cfp = 'Fatturato del ' + %editc( CMFmmCF : '3' ) +
053200150224                                 '/' + %editc( CMFaaCF : 'X' );
053300150128         EndIf;
053400150205         IF  CMFnoCF = 'N';
053500150205             VS2ecmD = 'Non Confrontato';
053600150205         ENDIF;
053700150224         ENDIF;
053800141105
053900150128         // -?Esecuzione Fase:?
054000150128         // ·?Data (gg/mm/aa)?
054100141107         IF  CMFdfa > 0;
054200141107           Data8 = CMFdfa;
054300141107           Data6gg = Data8gg;
054400141107           Data6mm = Data8mm;
054500141107           Data6aa = %dec(%subst(%editc(Data8aa:'X'):3:2):2:0);
054600141107           VS2data = Data6;
054700141105         ENDIF;
054800150128         // ·?Ora (hh:mm)?
054900150126         VS2ora = CMFhfc / 100;
055000150128         // ·?Utente?
055100141107         VS2ute = CMFpru;
055200150128         // ·?Numero Trattativa?
055300141107         VS2nrv = CMFnrv;
055400141104
055500141104         S02nrr += 1;
055600150112         write  KC03S02;
055700141104
055800141104       ENDSR;
055900141104
056000141104       //--------------------------------------------------------------
056100141113       //?Gestione tasto funzionale F12 da videata S02
056200141113       //?F12=Ritorno
056300141104       //--------------------------------------------------------------
056400141113       BEGSR F12S02;
056500141104
056600141104       //?Chiusura del programma
056700141104         Fine = *on;
056800141104
056900141104       ENDSR;
057000141118
057100141118       //--------------------------------------------------------------
057200141118       //?Gestione tasto funzionale F18 da videata S02
057300141118       //?F18=Note
057400141118       //--------------------------------------------------------------
057500141118       BEGSR  sr_F18S02;
057600141118
057700150112         //reset  TRKC10ds;        ?(già fatto)?
057800150112         //iKC10tla = 'L';         ?(già così)?
057900150127         //iKC10flm = 'I'; ?       ?(già impostato)?
058000150112         iKC10ncm = V02ncm;
058100150112         iKC10ksu = V02ksu;
058200150112         iKC10ksc = V02ksc;
058300150112         iKC10cpo = V02cpo;
058400150112         //iKC10acm = *all'9';     ?(già impostato)?
058500150112         iKC10des = V02des;
058600141118         if  V02cpo > 0;
058700150112           iKC10rsc = V02cpoD;
058800141118         else;
058900150112           iKC10rsc = V02rag;
059000141118         endif;
059100141118
059200150112         trkc10r ( kpjba : TRKC10ds );
059300150127
059400150127         // -?Verifica se inserite Note?
059500150127         //  ?per mettere in Reverse Image l'indicazione F18?
059600150127         if  Not EvidenzF18;
059700150127           exsr  sr_CtrlNote;
059800150127         endif;
059900141118
060000141118       ENDSR;
060100141118
060200141118       //--------------------------------------------------------------
060300150127       //?Gestione opzioni dal subfile S02.
060400141118       //--------------------------------------------------------------
060500141118       BEGSR  sr_OpzS02;
060600141118
060700141118         // -?Ciclo di lettura subfile?
060800150112         readc  KC03S02;
060900141118
061000150112         DOW  NOT  %eof(TRKC03D);
061100141118
061200141118           %subst(IndDspF : 50) = *off;
061300141118           SflNxtChg = *off;
061400141118           V02rcd    = S02nrr;
061500141118
061600141118           Select;
061700141118
061800141118             // -?Nessuna opzione?
061900141118             When  VS2opz = *blank;
062000141118
062100141118             // -?1 = Scelta?
062200141118             When  VS2opz = 'N';
062300150112               reset  TRKC10ds;
062400150127               iKC10flm = 'I';
062500150112               iKC10acm = VH2acm;
062600141118               exsr  sr_F18S02;
062700141118
062800141118             // -?? = Opzione NON valida?
062900141118             Other;
063000141118               ErrGenerico = *on;
063100141118               ErrMessage  = *on;
063200141118               PosCurOpz   = *on;
063300141118               VIDmsg = sk_Msg(03);
063400141118
063500141118           EndSl;
063600141118
063700141118           // -?Aggiornamento subfile?
063800141118           Select;
063900141118             When  ErrMessage;
064000141118               SflNxtChg = *on;
064100141118               V02csr    = V02rcd;
064200141118             When  ErrGenerico;
064300141118               SflNxtChg = *on;
064400141118               V02csr    = V02rcd;
064500141118               clear  VS2opz;
064600141118             When  VS2opz = '1';
064700141118               SflNxtChg = *on;
064800141118               V02csr    = V02rcd;
064900141118             Other;
065000141118               V02csr    = V02rcd;
065100141118               clear  VS2opz;
065200141118           EndSl;
065300141118
065400150112           update  KC03S02;
065500141118
065600141118           // -?Uscita dal ciclo di lettura?
065700141118           if  ErrMessage  or  ErrGenerico;
065800141118             leave;
065900141118           endif;
066000141118
066100141118           // -?Lettura rec. variato sucessivo?
066200150112           readc  KC03S02;
066300141118
066400141118         EndDo;
066500141118
066600141118       ENDSR;
066700150127
066800150127       //--------------------------------------------------------------
066900150127       //?Verifica esistenza Note (di qualsiasi fase).
067000150127       //--------------------------------------------------------------
067100150127       BEGSR  sr_CtrlNote;
067200150127
067300150127         clear  TRKC10ds;
067400150127         iKC10flm = 'R';
067500150127         iKC10ncm = iKC03ncm;
067600150127         iKC10ksu = iKC03ksu;
067700150127         iKC10ksc = iKC03ksc;
067800150127         iKC10cpo = iKC03cpo;
067900150127         iKC10acm = *all'9';
068000150127
068100150127         trkc10r ( kpjba : TRKC10ds );
068200150127
068300150127         EvidenzF18 = (eKC10no1 + eKC10no2 <> *blank);
068400150127
068500150127       ENDSR;
068600141104
068700141104       //--------------------------------------------------------------
068800141104       //?Operazioni finali.
068900141104       //--------------------------------------------------------------
069000141104       BEGSR RoutEnd;
069100141104
069200141104         *inLR = *on;
069300141104         return;
069400141104
069500141104       ENDSR;
069600141104
069700141104      /end-free
069800141104
069900141104       //--------------------------------------------------------------
070000141104       //?Schiere a tempo di compilazione.
070100141104       //--------------------------------------------------------------
070200141104
070300141118** -- sk_MSG ----------------------------------------------------------------*
070400141114Campagna commerciale errata                                                    1
070500141114Cliente/Potenziale errato                                                      2
070600141118Opzione errata                                                                 3
