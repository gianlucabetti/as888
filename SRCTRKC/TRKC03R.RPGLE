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
017200150505     d*// Oggi            s              8s 0 inz
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
024100150505       ////?Imposto oggi
024200150505         //Oggi = %dec(%date());
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
028000150505         //IF  not %found(TICMP01L) or CMPdfc < Oggi;
028100150505         IF  not %found(TICMP01L);
028200150112           OKC03err = '1';
028300150112           OKC03msg = sk_Msg(01);
028400141114           exsr RoutEnd;
028500141114         ENDIF;
028600141114
028700141114       //?Almeno 1 tra KSU/KSC/CPO deve esserci
028800150112         IF  IKC03ksu = 0 and IKC03ksc = 0 and IKC03cpo = 0;
028900150112           OKC03err = '1';
029000150112           OKC03msg = sk_Msg(02);
029100141114           exsr RoutEnd;
029200141114         ENDIF;
029300141114       //?Deve essere una codice valido
029400150112         IF  IKC03ksu > 0;
029500141114           clear TIBS69DS;
029600150112           I69kac = %dec(IKC03ksu:7:0);
029700141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
029800141114           IF  O69err <> *blanks;
029900150112             OKC03err = '1';
030000150112             OKC03msg = sk_Msg(02);
030100141114             exsr RoutEnd;
030200141114           ENDIF;
030300141114         ENDIF;
030400150112         IF  IKC03ksc > 0;
030500141114           clear TIBS69DS;
030600150112           I69kac = %dec(IKC03ksc:7:0);
030700141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
030800141114           IF  O69err <> *blanks;
030900150112             OKC03err = '1';
031000150112             OKC03msg = sk_Msg(02);
031100141114             exsr RoutEnd;
031200141114           ENDIF;
031300141114         ENDIF;
031400150112         IF  IKC03cpo > 0;
031500150112           chain (IKC03cpo) TNCPO01L;
031600141114           IF  not %found(TNCPO01L);
031700150112             OKC03err = '1';
031800150112             OKC03msg = sk_Msg(02);
031900141114             exsr RoutEnd;
032000141114           ENDIF;
032100141114         ENDIF;
032200141114
032300141114       ENDSR;
032400141106
032500141106       //--------------------------------------------------------------
032600141106       //?Gestione videata S02.
032700141106       //--------------------------------------------------------------
032800141106       BEGSR GesS02;
032900141106
033000141106       //?Inizializzazione videata
033100141106         IF  wInzS02;
033200141106           exsr InzS02;
033300141106           wInzS02 = *off;
033400141106           SflEnd = *on;
033500141106         ENDIF;
033600141106
033700141106       //?Visualizzazione del SFL (se ci sono dati)
033800141106         SflDsp = (S02nrr <= *zeros);
033900141106
034000141106       //?Posizionamento cursore
034100141106         SELECT;
034200141106         WHEN  S02nrr > *zero and V02rcd = *zeros;
034300141106           V02rcd = 1;
034400141106         WHEN S02nrr = *zeros;
034500141106           clear V02rcd;
034600141106         OTHER;
034700141106           V02rcd = S02nrr;
034800141106         ENDSL;
034900141106
035000141106         V02csr = V02rcd;
035100141107
035200141106
035300141106       //?Emissione Testata e Piede con tasti funzionali abilitati
035400150112         write  KC03T01;
035500150112         write  KC03P02;
035600141106
035700141106       //?Emissione videata
035800150112         exfmt  KC03C02;
035900141106         reset ErrMessage;
036000141106         reset ErrGenerico;
036100141106
036200141106         SELECT;
036300141106
036400141113       //?- F12=Ritorno
036500141113           WHEN  dsp_aid = c_F12;
036600141113             exsr F12S02;
036700141118
036800141118       //?- F18=Note (tutte)
036900141118           WHEN  dsp_aid = c_F18;
037000150126             clear  keyTICMC01;
037100150126             k_CMCncm = V02ncm;
037200150126             k_CMCksu = V02ksu;
037300150126             k_CMCksc = V02ksc;
037400150126             k_CMCcpo = V02cpo;
037500150126             chain  %kds( keyTICMC01 )  TICMC000;
037600150112             reset  TRKC10ds;
037700150126             //iKC10acm = *all'9';
037800150126             if  %found( TICMC01L );
037900150126               iKC10acm = CMCufe;
038000150126             endif;
038100141118             exsr  sr_F18S02;
038200141106
038300141106       //?Invio
038400141106           OTHER;
038500150114             IF  S02nrr > 0;
038600150114               exsr  sr_OpzS02;
038700150114             ENDIF;
038800141106
038900141106         ENDSL;
039000141106
039100141106       ENDSR;
039200141106
039300141106       //--------------------------------------------------------------
039400141106       //?Inizializzazione videata S02.
039500141106       //--------------------------------------------------------------
039600141106       BEGSR InzS02;
039700141106
039800141106         EndS02 = *off;
039900141106         Cliente = *off;
040000141106         Potenziale = *off;
040100141107         Unificante = *off;
040200141118
040300141118       //?Campagna
040400150112         V02ncm = iKC03ncm;
040500141118         V02des = CMPdes;
040600141118
040700141118       //?Visualizzo Unificante/Cliente/Potenziale
040800141118         SELECT;
040900150112         WHEN  IKC03ksu > 0;
041000141118           Unificante = *on;
041100150112           V02ksu = IKC03ksu;
041200141118           V02rag = ACOrag;
041300150112         WHEN  IKC03ksc > 0;
041400141118           Cliente = *on;
041500150112           V02ksc = IKC03ksc;
041600141118           V02rag = ACOrag;
041700150112         WHEN  IKC03cpo > 0;
041800141118           Potenziale = *on;
041900150112           V02cpo = IKC03cpo;
042000141118           V02cpod = CPOrag;
042100141118         ENDSL;
042200141106
042300141106       //?Pulizia subfile
042400141106         exsr PulS02;
042500141106
042600141106       //?Caricamento subfile
042700141106         exsr Ries02;
042800141106
042900141106       ENDSR;
043000141106
043100141106       //--------------------------------------------------------------
043200141106       //?Pulizia Subfile S02.
043300141106       //--------------------------------------------------------------
043400141106       BEGSR PulS02;
043500141106
043600141106       //?Pulizia subfile
043700141106         SflDsp    = *on;
043800141106         SflDspCtl = *on;
043900150112         write KC03C02;
044000141106         SflDspCtl = *off;
044100141106         SflEnd    = *off;
044200141106
044300141106         clear V02rcd;
044400141106         clear V02csr;
044500141106         clear S02nrr;
044600141106
044700141106         ErrMessage  = *off;
044800141106         ErrGenerico = *off;
044900141106
045000141106         WindDspF = IndDspF;
045100141106         reset WindDspFb;
045200141106         IndDspF  = WindDspF;
045300141106
045400141106       ENDSR;
045500141104
045600141106       //--------------------------------------------------------------
045700141106       //?Caricamento Subfile S02.
045800141106       //--------------------------------------------------------------
045900141106       BEGSR RieS02;
046000141106
046100141106         EndS02 = *off;
046200141106
046300141106         //?Leggo file TICMF
046400150112         setll (IKC03ncm:IKC03ksu:IKC03ksc:IKC03cpo) TICMF02L;
046500150112         reade (IKC03ncm:IKC03ksu:IKC03ksc:IKC03cpo) TICMF02L;
046600141113         DOW  not %eof(TICMF02L);
046700141104         //?Carico i dati nel subfile
046800141104           exsr CarS02;
046900150112           reade (IKC03ncm:IKC03ksu:IKC03ksc:IKC03cpo) TICMF02L;
047000141104         ENDDO;
047100150123
047200150123         // -?Verifica esistenza di Note inserite?
047300150123         //  ?(di qualunque fase...)?
047400150127         //  ?per (ri)mettere in Reverse Image l'indicazione F18?
047500150127         exsr  sr_CtrlNote;
047600141104
047700141104       ENDSR;
047800141104
047900141104       //--------------------------------------------------------------
048000141104       //?Carico i dati nel Subfile S02.
048100141104       //--------------------------------------------------------------
048200141104       BEGSR CarS02;
048300141104
048400150112         clear  KC03S02;
048500141104
048600141104       //?Imposto i campi
048700150128         // -?Fase Campagna?
048800141118         VH2acm = CMFacm;
048900141118
049000150128         // -?Decodifica Fase Campagna?
049100141106         clear dFCM;
049200141106         clear TIBS02DS;
049300141106         T02mod = 'C';
049400141106         T02cod = 'FCM';
049500141112         T02ke1 = CMFacm;
049600141106         T02sif = KNSIF;
049700141106         TNTBE_RicercaControllo  (kpjba : tibs02ds);
049800141106         IF  T02err = *blanks;
049900141107           dFCM = T02uni;
050000141106         ENDIF;
050100141106         VS2des = §FCMdes;
050200141107
050300150128         // -?Percentuale o Causale Chiusura?
050400141125         //VS2obj = CMFpea;
050500150126         clear dECM;
050600150119         select;
050700150128           // -?% da visualizzare   (§FCMPEAS = *blank) &?
050800150128           //  ?Fase da confrontare (CMFNOCF  = *blank)?
050900150128           when  §FCMpeaS <> 'N'  and  CMFnoCF <> 'N';
051000150119             VS2obj = %editc( CMFpea : 'L' );
051100150128           // -?% da NON visualizzare   (§FCMPEAS = "N") &?
051200150128           //  ?Causale Chiusura        (CMFCCH  <> *blank)?
051300150128           when  §FCMpeaS =  'N'  and  CMFcch  <> *blank;
051400150123             VS2obj = '--' + CMFcch + '--';
051500150123             //wPos = ( ( %len(VS2obj) - %len(CMFcch) ) / 2 ) + 1;
051600150123             //%subst( VS2obj : wPos ) = CMFcch;
051700150126             clear TIBS02DS;
051800150126             T02mod = 'C';
051900150126             T02cod = 'ECM';
052000150126             T02ke1 = CMFcch;
052100150126             T02sif = KNSIF;
052200150126             TNTBE_RicercaControllo  (kpjba : tibs02ds);
052300150126             if  T02err = *blanks;
052400150126               dECM = T02uni;
052500150126             endif;
052600150126             VS2ecmD = §ECMdes;
052700150119         endsl;
052800150128
052900150128         // -?Confronto Fatturato?
053000150224         IF  CMFacm = FaseObjCf;
053100150128         If  CMFaaCF <> *zero  or  CMFmmCF <> *zero;
053200150224           VS2cfp = 'Fatturato del ' + %editc( CMFmmCF : '3' ) +
053300150224                                 '/' + %editc( CMFaaCF : 'X' );
053400150128         EndIf;
053500150205         IF  CMFnoCF = 'N';
053600150205             VS2ecmD = 'Non Confrontato';
053700150205         ENDIF;
053800150224         ENDIF;
053900141105
054000150128         // -?Esecuzione Fase:?
054100150128         // ·?Data (gg/mm/aa)?
054200141107         IF  CMFdfa > 0;
054300141107           Data8 = CMFdfa;
054400141107           Data6gg = Data8gg;
054500141107           Data6mm = Data8mm;
054600141107           Data6aa = %dec(%subst(%editc(Data8aa:'X'):3:2):2:0);
054700141107           VS2data = Data6;
054800141105         ENDIF;
054900150128         // ·?Ora (hh:mm)?
055000150126         VS2ora = CMFhfc / 100;
055100150128         // ·?Utente?
055200141107         VS2ute = CMFpru;
055300150128         // ·?Numero Trattativa?
055400141107         VS2nrv = CMFnrv;
055500141104
055600141104         S02nrr += 1;
055700150112         write  KC03S02;
055800141104
055900141104       ENDSR;
056000141104
056100141104       //--------------------------------------------------------------
056200141113       //?Gestione tasto funzionale F12 da videata S02
056300141113       //?F12=Ritorno
056400141104       //--------------------------------------------------------------
056500141113       BEGSR F12S02;
056600141104
056700141104       //?Chiusura del programma
056800141104         Fine = *on;
056900141104
057000141104       ENDSR;
057100141118
057200141118       //--------------------------------------------------------------
057300141118       //?Gestione tasto funzionale F18 da videata S02
057400141118       //?F18=Note
057500141118       //--------------------------------------------------------------
057600141118       BEGSR  sr_F18S02;
057700141118
057800150112         //reset  TRKC10ds;        ?(già fatto)?
057900150112         //iKC10tla = 'L';         ?(già così)?
058000150127         //iKC10flm = 'I'; ?       ?(già impostato)?
058100150112         iKC10ncm = V02ncm;
058200150112         iKC10ksu = V02ksu;
058300150112         iKC10ksc = V02ksc;
058400150112         iKC10cpo = V02cpo;
058500150112         //iKC10acm = *all'9';     ?(già impostato)?
058600150112         iKC10des = V02des;
058700141118         if  V02cpo > 0;
058800150112           iKC10rsc = V02cpoD;
058900141118         else;
059000150112           iKC10rsc = V02rag;
059100141118         endif;
059200141118
059300150112         trkc10r ( kpjba : TRKC10ds );
059400150127
059500150127         // -?Verifica se inserite Note?
059600150127         //  ?per mettere in Reverse Image l'indicazione F18?
059700150127         if  Not EvidenzF18;
059800150127           exsr  sr_CtrlNote;
059900150127         endif;
060000141118
060100141118       ENDSR;
060200141118
060300141118       //--------------------------------------------------------------
060400150127       //?Gestione opzioni dal subfile S02.
060500141118       //--------------------------------------------------------------
060600141118       BEGSR  sr_OpzS02;
060700141118
060800141118         // -?Ciclo di lettura subfile?
060900150112         readc  KC03S02;
061000141118
061100150112         DOW  NOT  %eof(TRKC03D);
061200141118
061300141118           %subst(IndDspF : 50) = *off;
061400141118           SflNxtChg = *off;
061500141118           V02rcd    = S02nrr;
061600141118
061700141118           Select;
061800141118
061900141118             // -?Nessuna opzione?
062000141118             When  VS2opz = *blank;
062100141118
062200141118             // -?1 = Scelta?
062300141118             When  VS2opz = 'N';
062400150112               reset  TRKC10ds;
062500150127               iKC10flm = 'I';
062600150112               iKC10acm = VH2acm;
062700141118               exsr  sr_F18S02;
062800141118
062900141118             // -?? = Opzione NON valida?
063000141118             Other;
063100141118               ErrGenerico = *on;
063200141118               ErrMessage  = *on;
063300141118               PosCurOpz   = *on;
063400141118               VIDmsg = sk_Msg(03);
063500141118
063600141118           EndSl;
063700141118
063800141118           // -?Aggiornamento subfile?
063900141118           Select;
064000141118             When  ErrMessage;
064100141118               SflNxtChg = *on;
064200141118               V02csr    = V02rcd;
064300141118             When  ErrGenerico;
064400141118               SflNxtChg = *on;
064500141118               V02csr    = V02rcd;
064600141118               clear  VS2opz;
064700141118             When  VS2opz = '1';
064800141118               SflNxtChg = *on;
064900141118               V02csr    = V02rcd;
065000141118             Other;
065100141118               V02csr    = V02rcd;
065200141118               clear  VS2opz;
065300141118           EndSl;
065400141118
065500150112           update  KC03S02;
065600141118
065700141118           // -?Uscita dal ciclo di lettura?
065800141118           if  ErrMessage  or  ErrGenerico;
065900141118             leave;
066000141118           endif;
066100141118
066200141118           // -?Lettura rec. variato sucessivo?
066300150112           readc  KC03S02;
066400141118
066500141118         EndDo;
066600141118
066700141118       ENDSR;
066800150127
066900150127       //--------------------------------------------------------------
067000150127       //?Verifica esistenza Note (di qualsiasi fase).
067100150127       //--------------------------------------------------------------
067200150127       BEGSR  sr_CtrlNote;
067300150127
067400150127         clear  TRKC10ds;
067500150127         iKC10flm = 'R';
067600150127         iKC10ncm = iKC03ncm;
067700150127         iKC10ksu = iKC03ksu;
067800150127         iKC10ksc = iKC03ksc;
067900150127         iKC10cpo = iKC03cpo;
068000150127         iKC10acm = *all'9';
068100150127
068200150127         trkc10r ( kpjba : TRKC10ds );
068300150127
068400150127         EvidenzF18 = (eKC10no1 + eKC10no2 <> *blank);
068500150127
068600150127       ENDSR;
068700141104
068800141104       //--------------------------------------------------------------
068900141104       //?Operazioni finali.
069000141104       //--------------------------------------------------------------
069100141104       BEGSR RoutEnd;
069200141104
069300141104         *inLR = *on;
069400141104         return;
069500141104
069600141104       ENDSR;
069700141104
069800141104      /end-free
069900141104
070000141104       //--------------------------------------------------------------
070100141104       //?Schiere a tempo di compilazione.
070200141104       //--------------------------------------------------------------
070300141104
070400141118** -- sk_MSG ----------------------------------------------------------------*
070500141114Campagna commerciale errata                                                    1
070600141114Cliente/Potenziale errato                                                      2
070700141118Opzione errata                                                                 3
