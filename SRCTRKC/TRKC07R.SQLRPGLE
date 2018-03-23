000100141114      //--------------------------------------------------------------
000200150112      //?TRKC07R - AVANZAMENTO CAMPAGNA
000300141114      //--------------------------------------------------------------
000400141114
000500141114     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141114     h dftactgrp(*no) actgrp(*caller)
000700141114
000800141114      //---------------------------------------------------------------
000900141114      //?Dichiarazione file.
001000141114      //---------------------------------------------------------------
001100141114
001200141114      // - Anagrafica Commerciali
001300141114     fAZCMM01L  if   e           k disk
001400141114
001500141114      // - File Fasi Campagna
001600141114     fTICMF01L  if   e           k disk
001700141114
001800141114      // - File Info Cliente in Campagna
001900141114     fTICMI01L  if   e           k disk
002000141114
002100141114      // - Anagrafica Campagna
002200141114     fTICMP01L  if   e           k disk
002300141114
002400141114      // - File Anagrafiche Potenziali
002500141114     fTNCPO01L  if   e           k disk
002600141114
002700141114      // - Video Aggiunta Cliente in Campagna
002800150223     fTRKC07D   cf   e             workstn
002900141114     f                                     indds(IndDspF)
003000141114     f                                     infds(InfDspF)
003100141114
003200141114      //---------------------------------------------------------------
003300141114      //?Definizione costanti.
003400141114      //---------------------------------------------------------------
003500141114
003600141114      // - Tasti funzionali a video
003700141114     d c_F01           c                   const(x'31')
003800141114     d c_F02           c                   const(x'32')
003900141114     d c_F03           c                   const(x'33')
004000141114     d c_F04           c                   const(x'34')
004100141114     d c_F05           c                   const(x'35')
004200141114     d c_F06           c                   const(x'36')
004300141114     d c_F07           c                   const(x'37')
004400141114     d c_F08           c                   const(x'38')
004500141114     d c_F09           c                   const(x'39')
004600141114     d c_F10           c                   const(x'3A')
004700141114     d c_F11           c                   const(x'3B')
004800141114     d c_F12           c                   const(x'3C')
004900141114     d c_F13           c                   const(x'B1')
005000141114     d c_F14           c                   const(x'B2')
005100141114     d c_F15           c                   const(x'B3')
005200141114     d c_F16           c                   const(x'B4')
005300141114     d c_F17           c                   const(x'B5')
005400141114     d c_F18           c                   const(x'B6')
005500141114     d c_F19           c                   const(x'B7')
005600141114     d c_F20           c                   const(x'B8')
005700141114     d c_F21           c                   const(x'B9')
005800141114     d c_F22           c                   const(x'BA')
005900141114     d c_F23           c                   const(x'BB')
006000141114     d c_F24           c                   const(x'BC')
006100141114     d c_Enter         c                   const(x'F1')
006200141114     d c_RollDown      c                   const(x'F4')
006300141114     d c_RollUp        c                   const(x'F5')
006400141114
006500150216       // -?Costante per controllo "caratteri solo numerici"?
006600150216     d c_Digits        c                   const('0123456789,')
006700141114
006800141114     d FaseObj         c                   const(' 10')
006900141114     d FaseObjProp     c                   const(' 20')
007000141114     d FaseObjFine     c                   const(' 30')
007100141118     d FaseObjTtr      c                   const(' TR')
007200150220     d FaseObjTf       c                   const(' TF')
007300141118     d FaseObjCf       c                   const(' CF')
007400141114     d FaseChiudi      c                   const(' 90')
007500150123
007600150123       // -?Causale Chiusura Automatica per "Nessun Aumento"?
007700150123     d c_CausaleChiusura...
007800150123     d                 c                   const('NA')
007900150120
008000150120       // -?Attributi di Visualizzazione?
008100150120     d c_DspAtr_Norm   c                   const(x'20')
008200150120     d c_DspAtr_RI     c                   const(x'21')
008300150120     d c_DspAtr_HI     c                   const(x'22')
008400150120     d c_DspAtr_RIHI   c                   const(x'23')
008500141114
008600141114      //---------------------------------------------------------------
008700141114      //?Definizione schiere.
008800141114      //---------------------------------------------------------------
008900141114
009000141114      // - Messaggi di errore
009100150306     d Msg             s             78    dim(11) ctdata perrcd(1)
009200150123
009300150123       // -?Mesi dell'Anno?
009400150123     d sk_Mesi         s              9    dim(12)  ctdata  perrcd( 1)
009500141114
009600141114      //---------------------------------------------------------------
009700141114      //?Definizione aree dati.
009800141114      //---------------------------------------------------------------
009900141114
010000141114      // - Dati utente
010100141114     d §AzUte        e ds                  extname(AZUTE00F)
010200141114     d                                     dtaara
010300141114     d §DatiUte      e ds                  extname(dDatiUte)
010400141114     d                                     dtaara
010500141114
010600141114      //---------------------------------------------------------------
010700141114      //?Definizione strutture dati.
010800141114      //---------------------------------------------------------------
010900141114
011000141114      // - Status
011100141114     d Psds           sds
011200141114     d   SDSpgm          *proc
011300141114
011400141114      // - InfDS
011500141114     d InfDspF         ds
011600141114     d  dsp_aid              369    369a
011700141114     d  sfl_rrn              376    377i 0
011800141114     d  min_nrr              378    379i 0
011900141114     d  num_rcds             380    381i 0
012000141114
012100141114      // - Indicatori su DspF
012200141114     d IndDspF         ds
012300150115        // - Abilita F5=Ripristino
012400150115     d  AbilitaF5                     1n   overlay(IndDspF : 05)
012500150120     d  EvidenzF18                     n   overlay(IndDspF : 18)
012600141114        // - Indicatori di errore in videata
012700141114     d  ErrMessage                    1n   overlay(IndDspF : 28)
012800141114        // - Indicatori di visualizzazione campi
012900141114     d  VisOBJ                        1n   overlay(IndDspF : 40)
013000141114     d  VisOBJProp                    1n   overlay(IndDspF : 41)
013100141114     d  VisOBJFine                    1n   overlay(IndDspF : 42)
013200141114     d  VisCCH                        1n   overlay(IndDspF : 43)
013300150120     d  VisNOTEo                       n   overlay(IndDspF : 44)
013400150128     d  VisObjTTR                      n   overlay(IndDspF : 45)
013500150128     d  VisObjCFA                      n   overlay(IndDspF : 46)
013600150220     d  VisFaseTF                      n   overlay(IndDspF : 47)
013700150220     d  GesFaseTF                      n   overlay(IndDspF : 48)
013800141114        // - Indicatori di errore
013900141114     d  PosCurOBJ                     1n   overlay(IndDspF : 50)
014000141114     d  PosCurOBJprop                 1n   overlay(IndDspF : 51)
014100141114     d  PosCurOBJfine                 1n   overlay(IndDspF : 52)
014200141114     d  PosCurCCH                     1n   overlay(IndDspF : 53)
014300141114     d  PosCurNOTA1                   1n   overlay(IndDspF : 54)
014400150220     d  PosCurFaseTF                   n   overlay(IndDspF : 55)
014500150220     d  PosCurDecTar                   n   overlay(IndDspF : 56)
014600141114        // - Indicatori di errore generico
014700141114     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014800141114
014900141114     d WindDspF        ds                  inz
015000141114     d  WindDspFa              1     49    inz(*zero)
015100141114     d  WindDspFb             50     99    inz(*zero)
015200150220
015300150220     d wMmAaaa_ds      ds             6    inz  qualified
015400150220     d   wMm                          2s 0 inz
015500150220     d   wAaaa                        4s 0 inz
015600141114
015700141114      // - Parametri ricevuti
015800141114     d KPJBA         e ds
015900150112     d TRKC07DS      e ds
016000141118
016100141118      // - Storico
016200150112     d TRKC03DS      e ds                  inz
016300141114
016400141114      // - Note
016500150112     d TRKC10DS      e ds                  inz
016600141114
016700141114      // - Scrittura fase
016800150112     d TRKC71DS      e ds                  inz
016900150116
017000150116      // - Ripristino Cliente
017100150116     d TRKC74DS      e ds                  inz
017200150224
017300150224      // - Dati da Visualizzare o Parzializzazioni Trattative
017400150224     d TRKC76DS      e ds                  inz
017500141114
017600141114      // - Ricerca/Controllo tabelle
017700141114     d TIBS02DS      e ds                  inz
017800141114
017900141114      // - Reperimento dati utente
018000141114     d TIBS34DS      e ds
018100141114
018200141114      // - Reperimento dati Anagrafica Clienti
018300141114      /copy gaitrasrc/srcprotopi,TIBS69R
018400150115
018500150115      // - Tabella ECM - Causali chiusura campagna
018600150115     d dECM          e ds
018700141114
018800141114      // - Tabella FCM - Fase Campagna
018900141114     d dFCM          e ds
019000141114
019100141114      //---------------------------------------------------------------
019200141114      //?Definizione variabili globali.
019300141114      //---------------------------------------------------------------
019400141114
019500141114      // - Flags booleani
019600141114     d Fine            s               n   inz(*off)
019700141114     d wEnd            s               n   inz(*off)
019800141114     d wInzD02         s               n   inz(*off)
019900141118     d wInzD03         s               n   inz(*off)
020000150224     d wTRKC76         s               n   inz(*off)
020100150223     d $ObiettProp     s               n   inz
020200150223     d $ObiettFine     s               n   inz
020300141114
020400141114      // - Campi associati al video
020500141118     d Video           s              2a   inz
020600141114
020700141114      // - Campi di comodo data
020800141114     d Data_EUR        s               d   datfmt(*eur)
020900141114     d Data_ISO        s               d   datfmt(*iso)
021000141114
021100141114      // - Campi di comodo
021200141114     d Oggi            s              8s 0 inz
021300150123     d wAdesso         s              6s 0 inz
021400150306     d wAnno           s              4s 0 inz
021500150306     d wAnno1          s              4s 0 inz
021600150306     d wAnno2          s              4s 0 inz
021700150224     d wfase           s                   like(CMFacm) inz
021800141114     d wpos            s              5i 0 inz
021900141114     d wposda          s              5i 0 inz
022000150216     d W02objFine      s              5  2 inz
022100150216     d w0115           s             11  5 inz
022200141114
022300141114      //---------------------------------------------------------------
022400141114      //?Definizione procedure usate.
022500141114      //---------------------------------------------------------------
022600141118
022700141118      // - Pgm gestione STORICO
022800150112     d TRKC03R         pr                  extpgm('TRKC03R')
022900141118     d  kpjba                              likeds(kpjba)
023000150112     d  trkc03ds                           likeds(TRKC03DS)
023100141114
023200141114      // - Pgm gestione NOTE
023300150112     d TRKC10R         pr                  extpgm('TRKC10R')
023400141114     d  kpjba                              likeds(kpjba)
023500150112     d  trkc10ds                           likeds(TRKC10DS)
023600141114
023700141114      // - Pgm Scrive fase se cliente in Campagna
023800150112     d TRKC71R         pr                  extpgm('TRKC71R')
023900141114     d  kpjba                              likeds(kpjba)
024000150112     d  trkc71ds                           likeds(TRKC71DS)
024100150116
024200150116      // - Pgm per rispristinare cliente in Campagna
024300150116     d TRKC74R         pr                  extpgm('TRKC74R')
024400150116     d  kpjba                              likeds(kpjba)
024500150224
024600150224      // - Dati da Visualizzare o Parzializzazioni Trattative
024700150224     d TRKC76R         pr                  extpgm('TRKC76R')
024800150224     d  kpjba                              likeds(kpjba)
024900150224     d  trkc76ds                           likeds(TRKC76DS)
025000150204
025100150204       // -?Selezione/Gestione tab. "ECM" = Causali Esclusione Campagna?
025200150204     d tntbECMds     e ds                  inz
025300150204     d tntbECMr        pr                  extpgm('TNTBECMR')
025400150204     d   kpjba                             likeds(kpjba)
025500150220
025600150220       // -?Controllo/Inversione data?
025700150220     d WLBdat          ds                  inz
025800150220     d   G08dat                       8  0 inz
025900150220     d   G08inv                       8  0 inz
026000150220     d   G08err                       1    inz('3')
026100150220     d   G08tgi                       5  0 inz
026200150220      /copy gaitrasrc/srcProtoPR,XSRDA8
026300141114
026400141114      //---------------------------------------------------------------
026500141114      //?Definizione Prototipi.
026600141114      //---------------------------------------------------------------
026700141114      /copy gaitrasrc/srcprotopr,TIBS02R
026800141114      /copy gaitrasrc/srcprotopr,TIBS34R
026900141114      /copy gaitrasrc/srcprotopr,TIBS69R
027000141114
027100141114      //---------------------------------------------------------------
027200141114      //?Definizione key-list.
027300141114      //---------------------------------------------------------------
027400141114
027500141114      //---------------------------------------------------------------
027600141114      //?M A I N - L I N E
027700141114      //---------------------------------------------------------------
027800141114
027900141114     c     *Entry        plist
028000141114     c                   parm                    kpjba
028100150112     c                   parm                    trkc07ds
028200141114
028300141114      /free
028400141114
028500141114       //?Operazioni iniziali
028600141114       exsr RoutInz;
028700141114
028800141114       //?Controllo dati passati
028900141114       exsr Controlla;
029000141114
029100141114       //?Gestione video
029200141114       DOW  Fine = *off;
029300141114         SELECT;
029400141114           WHEN  Video = 'D2';
029500141114             exsr GesD02;
029600141118           WHEN  Video = 'D3';
029700141118             exsr GesD03;
029800141114           OTHER;
029900141114             Fine = *on;
030000141114         ENDSL;
030100141114       ENDDO;
030200141114
030300141114       //?Operazioni finali
030400141114       exsr RoutEnd;
030500141114
030600141114       //--------------------------------------------------------------
030700141114       //?Operazioni iniziali.
030800141114       //--------------------------------------------------------------
030900141114       BEGSR RoutInz;
031000141114
031100141114         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
031200141114
031300141114       //?Impostazione campi "fissi"
031400141114         V01pgm = SDSpgm;
031500141118
031600141118       //?Se richiamato per modifica apro video D2
031700150112         IF  IKC07ric = '2';
031800141118           Video = 'D2';
031900141118           wInzD02 = *on;
032000141118         ENDIF;
032100150115       //?Se richiamato per interrogazione/ripristino apro video D3
032200150115         IF  IKC07ric = '5' or IKC07ric = 'R';
032300141118           Video = 'D3';
032400141118           wInzD03 = *on;
032500141118         ENDIF;
032600141114
032700141114       //?Imposto oggi
032800141114         Oggi = %dec(%date());
032900150306
033000150306       //?Anno in corso
033100150306         Data_ISO = %date(Oggi);
033200150306         wAnno    = %subdt(Data_ISO:*years);
033300150306       //?Anno in corso + 1
033400150306         Data_ISO += %years(1);
033500150306         wAnno1   = %subdt(Data_ISO:*years);
033600150306       //?Anno in corso - 1
033700150306         Data_ISO = %date(Oggi);
033800150306         Data_ISO -= %years(1);
033900150306         wAnno2   = %subdt(Data_ISO:*years);
034000141114
034100141114       //?Reperimento dati job
034200141114         exsr DatiJob;
034300141117
034400141117       //?Pulisco dati output
034500150112         clear OKC07F12;
034600150112         clear OKC07err;
034700150112         clear OKC07msg;
034800141114
034900141114       ENDSR;
035000141114
035100141114       //--------------------------------------------------------------
035200141114       //?Reperimento Dati del job (Utente/Operativi).
035300141114       //--------------------------------------------------------------
035400141114       BEGSR DatiJob;
035500141114
035600141114         in(E) §AzUte;
035700141114         IF  NOT %error;
035800141114           in(E) §DatiUte;
035900141114         ENDIF;
036000141114         IF  %error or RSut = *blanks;
036100141114           clear TIBS34ds;
036200141114           tibs34r(tibs34ds);
036300141114           in §AzUte;
036400141114           in §DatiUte;
036500141114         ENDIF;
036600141114
036700141114       ENDSR;
036800141114
036900141114       //--------------------------------------------------------------
037000141114       //?Controllo i dati passati.
037100141114       //--------------------------------------------------------------
037200141114       BEGSR Controlla;
037300141114
037400141114       //?Controllo i dati passati
037500150112         IF  IKC07ncm = 0;
037600150112           OKC07err = '1';
037700150112           OKC07msg = Msg(01);
037800141114           leavesr;
037900141114         ENDIF;
038000150112         chain (IKC07ncm) TICMP01L;
038100150505         //IF  not %found(TICMP01L) or CMPdfc < Oggi;
038200150505         IF  not %found(TICMP01L) or ( CMPdfc < Oggi and
038300150505                                     IKC07ric <> '5' );
038400150112           OKC07err = '1';
038500150112           OKC07msg = Msg(01);
038600141114           exsr RoutEnd;
038700141114         ENDIF;
038800141114
038900141114       //?Almeno 1 tra KSU/KSC/CPO deve esserci
039000150112         IF  IKC07ksu = 0 and IKC07ksc = 0 and IKC07cpo = 0;
039100150112           OKC07err = 'E';
039200150112           OKC07msg = Msg(02);
039300141114           exsr RoutEnd;
039400141114         ENDIF;
039500141114
039600141114       //?Deve essere una codice valido
039700150112         IF  IKC07ksu > 0;
039800141114           clear TIBS69DS;
039900150112           I69kac = %dec(IKC07ksu:7:0);
040000141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
040100141114           IF  O69err <> *blanks;
040200150112             OKC07err = 'E';
040300150112             OKC07msg = Msg(02);
040400141114             exsr RoutEnd;
040500141114           ENDIF;
040600141114         ENDIF;
040700150112         IF  IKC07ksc > 0;
040800141114           clear TIBS69DS;
040900150112           I69kac = %dec(IKC07ksc:7:0);
041000141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
041100141114           IF  O69err <> *blanks;
041200150112             OKC07err = 'E';
041300150112             OKC07msg = Msg(02);
041400141114             exsr RoutEnd;
041500141114           ENDIF;
041600141114         ENDIF;
041700150112         IF  IKC07cpo > 0;
041800150112           chain (IKC07cpo) TNCPO01L;
041900141114           IF  not %found(TNCPO01L);
042000150112             OKC07err = 'E';
042100150112             OKC07msg = Msg(02);
042200141114             exsr RoutEnd;
042300141114           ENDIF;
042400141114         ENDIF;
042500141114
042600141114       //?Fase obbligatoria
042700141118       //?solo in modifica
042800150112         IF  IKC07acm = *blanks and IKC07ric <> '5';
042900150112           OKC07err = 'E';
043000150112           OKC07msg = Msg(03);
043100141114           exsr RoutEnd;
043200141114         ENDIF;
043300141114
043400141114       //?Deve essere una fase valida
043500150112         IF  IKC07acm <> *blanks;
043600141118           clear TIBS02DS;
043700141118           clear dFCM;
043800141118           T02mod = 'C';
043900141118           T02cod = 'FCM';
044000150112           T02ke1 = IKC07acm;
044100141118           T02sif = KNSIF;
044200141118           TNTBE_RicercaControllo  (kpjba : tibs02ds);
044300141118           IF  T02err <> *blanks;
044400150112             OKC07err = 'E';
044500150112             OKC07msg = Msg(03);
044600141118             exsr RoutEnd;
044700141118           ENDIF;
044800141114       //?Dati della fase
044900141118           dFCM = T02uni;
045000141118         ENDIF;
045100141114
045200141114       ENDSR;
045300141114
045400141110       //--------------------------------------------------------------
045500141110       //?Gestione videata D02.
045600141110       //--------------------------------------------------------------
045700141110       BEGSR GesD02;
045800141110
045900141110       //?Inizializzazione videata
046000141110         IF  wInzD02;
046100141110           exsr InzD02;
046200141110           wInzD02 = *off;
046300141110         ENDIF;
046400141110
046500141110       //?Emissione Testata
046600150112         write  KC07T01;
046700141110
046800141110       //?Emissione videata
046900150112         exfmt  KC07D02;
047000141110         reset ErrMessage;
047100141110         reset ErrGenerico;
047200141110         clear V02msg;
047300141110
047400141110         SELECT;
047500141110
047600141110       //?- F06=Conferma
047700141110           WHEN  dsp_aid = c_F06;
047800141110             exsr ContrD02;
047900141110             IF  ErrGenerico;
048000141110               leavesr;
048100141110             ENDIF;
048200141110             exsr F06D02;
048300141110
048400141110       //?- F12=Ritorno
048500141110           WHEN  dsp_aid = c_F12;
048600141110             exsr F12D02;
048700141114
048800141114       //?- F18=Note
048900141114           WHEN  dsp_aid = c_F18;
049000141114             exsr F18D02;
049100141110
049200141110       //?Invio
049300141110           OTHER;
049400141110             exsr ContrD02;
049500141110             IF  ErrGenerico;
049600141110               leavesr;
049700141110             ENDIF;
049800141110
049900141110         ENDSL;
050000141110
050100141110       ENDSR;
050200141110
050300141110       //--------------------------------------------------------------
050400141110       //?Inizializzazione Videata D02.
050500141110       //--------------------------------------------------------------
050600141110       BEGSR InzD02;
050700141110
050800141110       //?Pulizia videata
050900150112         clear KC07D02;
051000150216
051100150216         clear  W02objFine;
051200141114
051300141114         clear wpos;
051400141114         clear wposda;
051500141114         wpos = %checkr(' ':§FCMdes);
051600141114         wposda = (%len(§FCMdes) - (wpos - 1)) / 2;
051700141114         %subst(V01fased:wposda:wpos) = §FCMdes;
051800141110
051900141118       //?Carico le parti comuni alle due videate
052000141118         exsr PartiComuni;
052100141114
052200141114       //?Obiettivi/Causale
052300141114         VisOBJ = *off;
052400141114         VisOBJprop = *off;
052500141114         VisOBJfine = *off;
052600141114         VisCCH = *off;
052700150220         VisFaseTF = *off;
052800150220         GesFaseTF = *off;
052900141114         V02objs = '+';
053000141114         V02objps = '+';
053100141114         V02objfs = '+';
053200150220         V02objTtrS = '+';
053300141118         PosCurOBJ = *off;
053400141118         PosCurOBJprop = *off;
053500141118         PosCurOBJfine = *off;
053600150223         $ObiettProp = *off;
053700150223         $ObiettFine = *off;
053800141118
053900141114       //?cerco Obiettivo Iniziale
054000150112         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
054100141114         IF  %found(TICMF01L);
054200141114           IF  CMFpea < 0;
054300141114             V02objs = '-';
054400141114           ENDIF;
054500141114           V02obj = %abs(CMFpea);
054600141114         ENDIF;
054700141114       //?cerco Obiettivo Proposto
054800141114         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
054900141114         IF  %found(TICMF01L);
055000141114           IF  CMFpea < 0;
055100141114             V02objps = '-';
055200141114           ENDIF;
055300141114           V02objprop = %abs(CMFpea);
055400150223           $ObiettProp = *on;
055500141114         ENDIF;
055600150220         // -?cerco Trattativa Forzata?
055700150220         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTf) TICMF01L;
055800150220         If  %found(TICMF01L);
055900150220           if  CMFpea < 0;
056000150220             V02objTtrS = '-';
056100150220           endif;
056200150220           V02objTtr  = %abs(CMFpea);
056300150220           V02maDt    = ( CMFmmCF * 10000 ) + CMFaaCF;
056400150220         EndIf;
056500141114       //?cerco Obiettivo Finale
056600141114         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
056700141114         IF  %found(TICMF01L);
056800141114           IF  CMFpea < 0;
056900141114             V02objfs = '-';
057000141114           ENDIF;
057100150216           W02objfine = %abs(CMFpea);
057200150216           V02objfinA = %editc( W02objfine : '3' );
057300150223           $ObiettFine = *on;
057400141114         ENDIF;
057500141114
057600141114         SELECT;
057700141114       //?se fase obiettivo iniziale visualizzo solo l'iniziale
057800150112         WHEN  IKC07acm = FaseObj;
057900141114           VisOBJ = *on;
058000141118           PoscurOBJ = *on;
058100141114       //?Obiettivo Proposto
058200141114       //?se fase obiettivo proposto visualizzo il proposto
058300141114       //?visualizzo anche l'obittivo iniziale ma protetto
058400150112         WHEN  IKC07acm = FaseObjProp;
058500141114           VisOBJ = *on;
058600141114           VisOBJprop = *on;
058700141118           PoscurOBJprop = *on;
058800150220         // -?Trattativa Forzata?
058900150220         WHEN  iKC07acm = FaseObjTF;
059000150220           VisOBJ = *on;
059100150223           VisOBJprop = $ObiettProp;
059200150223           VisOBJfine = $ObiettFine;
059300150220           VisFaseTF  = *on;
059400150220           GesFaseTF  = *on;
059500150220           PosCurFaseTF = *on;
059600141114       //?Obiettivo Finale
059700141114       //?se fase obiettivo finale visualizzo il finale
059800141114       //?visualizzo anche l'obittivo iniziale e il proposto ma protetti
059900150112         WHEN  IKC07acm = FaseObjFine;
060000141114           VisOBJ = *on;
060100141114           VisOBJprop = *on;
060200141114           VisOBJfine = *on;
060300141118           PoscurOBJfine = *on;
060400150220         // -?Causale Esclusione Cliente?
060500150112         WHEN  IKC07acm = FaseChiudi;
060600141114           VisOBJ = *on;
060700141114           VisOBJprop = *on;
060800141114           VisOBJfine = *on;
060900141114           VisCCH = *on;
061000141114         ENDSL;
061100150220
061200150220         // -?Trattativa Forzata?
061300150220         if  iKC07acm <> FaseObjTF;
061400150220           VisFaseTF = ( V02objTtr <> *zero );
061500150220         endif;
061600150128
061700150128         // -?Obiettivo Confronto Fatturazione?
061800150128         //  ?reperimento anno/mese?
061900150128         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjCf) TICMF01L;
062000150128         if  %found(TICMF01L)   and  CMFnoCF = *blank  and
062100150128             ( CMFaaCF <> *zero  or  CMFmmCF <> *zero );
062200150128           V02cfp = 'Fatturato del ' + %editc( CMFaaCF : '3' ) +
062300150128                                 '/' + %editc( CMFmmCF : 'X' );
062400150128         endif;
062500141110
062600141110       ENDSR;
062700141110
062800141110       //--------------------------------------------------------------
062900141110       //?Controlla Videata D02.
063000141110       //--------------------------------------------------------------
063100141110       BEGSR ContrD02;
063200141110
063300141110         WindDspF = IndDspF;
063400141110         reset WindDspFb;
063500141110         IndDspF  = WindDspF;
063600141121
063700141121         clear V02cchd;
063800141114
063900141114       //?Causale chiusura campagna
064000141121         IF  VisCCH;
064100141114         //?ricerca
064200141114           IF  %scan('?':V02cch) > 0;
064300150204             clear  tntbECMds;
064400150204             bECMfun = '1';
064500150204             bECMuti = 'S';
064600150204             kpjbu = tntbECMds;
064700150204             tntbECMr ( kpjba );
064800150204             tntbECMds = kpjbu;
064900150204             if  bECMesito = *off    and
065000150204                 bECMricez = *blank  and
065100150204                 bECMopzio = '01';
065200150204               V02cch  = bECMke1;
065300150204               V02cchd = bECMdes;
065400150204             endif;
065500141114           ENDIF;
065600141114         //?obbligatoria
065700141114           IF  V02cch = *blanks;
065800141114             ErrMessage  = *on;
065900141114             ErrGenerico = *on;
066000141114             V02msg = Msg(05);
066100141114             PosCurCCH = *on;
066200141114             leavesr;
066300141114           ENDIF;
066400141114         //?valida
066500150115           clear TIBS02DS;
066600150115           clear dECM;
066700141114           T02mod = 'C';
066800141114           T02cod = 'ECM';
066900141114           T02ke1 = V02cch;
067000141114           T02sif = KNSIF;
067100141114           TNTBE_RicercaControllo  (kpjba : tibs02ds);
067200141114           IF  T02err <> *blanks;
067300141114             ErrMessage  = *on;
067400141114             ErrGenerico = *on;
067500141114             V02msg = Msg(05);
067600141114             PosCurCCH = *on;
067700141114             leavesr;
067800141114           ENDIF;
067900150115           dECM = T02uni;
068000150115           V02cchd = §ECMdes;
068100150115         //?no automatica
068200150115           IF  §ECMaut <> *blanks;
068300150115             ErrMessage  = *on;
068400150115             ErrGenerico = *on;
068500150115             V02msg = %trim(Msg(05)) + '. Immessa causale automatica';
068600150115             PosCurCCH = *on;
068700150115             leavesr;
068800150115           ENDIF;
068900141121         ENDIF;
069000150216
069100150216         // -?Controllo dell'Obiettivo Finale (alfanumerico)?
069200150216         IF  VisObjFine;
069300150216
069400150216           // ·?Controllo inserimento?
069500150216           if  V02objFinA = *blank;
069600150216             PosCurOBJfine = *on;
069700150216             ErrMessage  = *on;
069800150216             ErrGenerico = *on;
069900150216             V02msg = Msg(07);
070000150216             leavesr;
070100150216           endif;
070200150216
070300150216           V02objFinA = %xlate( ' ' : '0' : V02objFinA );
070400150216           // ·?Controllo numericità?
070500150216           if  %check( c_Digits : %trim(V02objFinA) ) > *zero;
070600150216             PosCurOBJfine = *on;
070700150216             ErrMessage  = *on;
070800150216             ErrGenerico = *on;
070900150216             V02msg = Msg(08);
071000150216             leavesr;
071100150216           endif;
071200150216
071300150216           // ·?Controllo virgole inserite (max 1 ",")?
071400150216           clear  wPos;
071500150216           wPos = %scan( ',' : V02objFinA );
071600150216           if  wPos > *zero  and  wPos < %len( V02objFinA );
071700150216             wPosDa = %scan( ',' : V02objFinA : wPos + 1 );
071800150216             if  wPosDa > *zero;
071900150216               PosCurOBJfine = *on;
072000150216               ErrMessage  = *on;
072100150216               ErrGenerico = *on;
072200150216               V02msg = Msg(08);
072300150216               leavesr;
072400150216             endif;
072500150216           endif;
072600150216
072700150216           //// ·?Controllo numero interi inseriti (> *zero)?
072800150216           //if  wPos > 1      and
072900150216           //    %int( %trimR( %subst( V02objFinA : 1 : wPos - 1 ) ) ) > 999;
073000150216           //  PosCurOBJfine = *on;
073100150216           //  ErrMessage  = *on;
073200150216           //  ErrGenerico = *on;
073300150216           //  V02msg = Msg(08);
073400150216           //  leavesr;
073500150216           //endif;
073600150216           //// ·?Controllo numero decimali inseriti (> *zero)?
073700150216           //if  wPos > *zero  and
073800150216           //    %len( %trimR( %subst( V02objFinA : wPos + 1 ) ) ) > 2  and
073900150216           //    %int( %trimR( %subst( V02objFinA : wPos + 3 ) ) ) <> *zero;
074000150216           //  PosCurOBJfine = *on;
074100150216           //  ErrMessage  = *on;
074200150216           //  ErrGenerico = *on;
074300150216           //  V02msg = Msg(08);
074400150216           //  leavesr;
074500150216           //endif;
074600150216
074700150216           // ·?Verifica correttezza % numerica (3 interi + 2 decimali)?
074800150216           clear  W02objFine;
074900150216           eval w0115 = %abs( %dec( %trim( V02objFinA ) : 11 : 5 ) );
075000150216           if  w0115 < 999,99;
075100150216             eval(H) W02objFine = %abs( %dec( %trim( V02objFinA ) : 5 : 2 ) );
075200150216           endif;
075300150216           if  w0115 <> W02objFine;
075400150216             PosCurOBJfine = *on;
075500150216             ErrMessage  = *on;
075600150216             ErrGenerico = *on;
075700150216             V02msg = Msg(08);
075800150216             leavesr;
075900150216           endif;
076000150216
076100150216           // ·?Re-impostazione % elaborata a video nel campo alfa?
076200150216           //W02objFine = %abs( %dec( %trim( V02objFinA ) : 5 : 2 ) );
076300150216           V02objFinA = %editc( W02objFine : '3' );
076400150216
076500150216         ENDIF;
076600141121
076700141121       //?Se obiettivo a 0 non posso mettere in negativo
076800141121         SELECT;
076900150216         WHEN  VisObjFine and W02objfine = 0 and V02objfs = '-';
077000141121           ErrMessage  = *on;
077100141121           ErrGenerico = *on;
077200141121           V02msg = Msg(04);
077300141121           PosCurOBJfine = *on;
077400141121         WHEN  VisObjProp and V02objprop = 0 and V02objps = '-';
077500141121           ErrMessage  = *on;
077600141121           ErrGenerico = *on;
077700141121           V02msg = Msg(04);
077800141121           PosCurOBJprop = *on;
077900141121         WHEN  VisObj and V02obj = 0 and V02objs = '-';
078000141121           ErrMessage  = *on;
078100141121           ErrGenerico = *on;
078200141121           V02msg = Msg(04);
078300141121           PosCurOBJ = *on;
078400141121         ENDSL;
078500150220
078600150220         // -?Ricevuta fase "TF" = Trattativa Forzata?
078700150220         clear  wMmAaaa_ds;
078800150220         IF  iKC07acm = FaseObjTF;
078900150220           // -?% Andamento Trattativa obbligatoria?
079000150220           If  V02objTtr = *zero;
079100150220             ErrMessage  = *on;
079200150220             ErrGenerico = *on;
079300150220             V02msg = Msg(09);
079400150220             PosCurFaseTF = *on;
079500150220             leavesr;
079600150220           EndIf;
079700150220           // -?Decorrenza Tariffa obbligatoria/formalmente errata?
079800150220           wMmAaaa_ds = %editc( V02maDT : 'X' );
079900150220           clear WLBdat;
080000150220           if  wMmAaaa_ds.wMm  > *zero;       //?Inserito anno di 4 cifre?
080100150220             G08dat = 01000000 + V02maDT;
080200150220           else;                              //?Inserito anno di 2 cifre?
080300150220             G08dat = 00010000 + V02maDT;
080400150220           endif;
080500150220           XSRDA8(WLBdat);
080600150220           if  G08err = *on;
080700150220             V02msg = Msg(10);
080800150220             ErrMessage   = *on;
080900150220             ErrGenerico  = *on;
081000150220             PosCurDecTar = *on;
081100150220             leavesr;
081200150220           endif;
081300150220           V02maDT = G08dat - 01000000;
081400150220           wMmAaaa_ds = %editc( V02maDT : 'X' );
081500150306         //?Verifico che l'anno sia o anno in corso o anno in corso -1 o
081600150306         //?anno in corso +1
081700150306           IF wMmAaaa_ds.wAaaa <> wAnno and
081800150306              wMmAaaa_ds.wAaaa <> wAnno1 and
081900150306              wMmAaaa_ds.wAaaa <> wAnno2;
082000150306             V02msg = Msg(11);
082100150306             ErrMessage   = *on;
082200150306             ErrGenerico  = *on;
082300150306             PosCurDecTar = *on;
082400150306             leavesr;
082500150306           ENDIF;
082600150220         ENDIF;
082700141114
082800141121       //?Note obbligatorie se richiesto da tabella fase
082900141114         IF  V02nota1 = *blanks and V02nota2 = *blanks and
083000141114             §FCMnot = 'S';
083100141114           ErrMessage  = *on;
083200141114           ErrGenerico = *on;
083300141114           V02msg = Msg(06);
083400141114           PosCurNOTA1 = *on;
083500141114           leavesr;
083600141114         ENDIF;
083700141121
083800141121       //?Note obbligatorie se obiettivo iniziale <> da obiettivo proposto
083900141121         IF  V02nota1 = *blanks and V02nota2 = *blanks and
084000141121             VisOBJprop and not VisOBJfine and
084100141121             (V02obj <> V02objprop or V02objs <> V02objps);
084200141121           ErrMessage  = *on;
084300141121           ErrGenerico = *on;
084400141121           V02msg = Msg(06);
084500141121           PosCurNOTA1 = *on;
084600141121         ENDIF;
084700150220         // -?...o SE ricevuta fase "TF" = Trattativa Forzata?
084800150220         IF  iKC07acm = FaseObjTF  and
084900150220             V02nota1 = *blank     and  V02nota2 = *blank;
085000150220           ErrMessage  = *on;
085100150220           ErrGenerico = *on;
085200150220           V02msg = Msg(06);
085300150220           PosCurNOTA1 = *on;
085400150220         ENDIF;
085500141110
085600141110       ENDSR;
085700141110
085800141110       //--------------------------------------------------------------
085900141110       //?Gestione tasto funzionale F06 da videata D02
086000141110       //?F06=Conferma
086100141110       //--------------------------------------------------------------
086200141110       BEGSR F06D02;
086300141114
086400141114       //?Scrivo la fase
086500150112         clear TRKC71DS;
086600150112         IKC71ncm = V02ncm;
086700150112         IKC71ksu = V02ksu;
086800150112         IKC71ksc = V02ksc;
086900150112         IKC71cpo = V02cpo;
087000150112         IKC71acm = IKC07acm;
087100141114         SELECT;
087200141114         WHEN VisCCH;
087300150112           IKC71cch = V02cch;
087400150224         WHEN  GesFaseTF;
087500150224           iKC71pea = V02objTtr;
087600150224           if  V02objTtrS = '-';
087700150224             iKC71pea = iKC71pea * -1;
087800150224           endif;
087900150224           wMmAaaa_ds = %editc( V02maDT : 'X' );
088000150224           iKC71aaCF  = wMmAaaa_ds.wAaaa;
088100150224           iKC71mmCF  = wMmAaaa_ds.wMm;
088200141114         WHEN VisObjFine;
088300150216           IKC71pea = W02objfine;
088400141114           IF  V02objfs = '-';
088500150112             IKC71pea = IKC71pea * -1;
088600141114           ENDIF;
088700141114         WHEN VisObjProp;
088800150112           IKC71pea = V02objprop;
088900141114           IF  V02objps = '-';
089000150112             IKC71pea = IKC71pea * -1;
089100141114           ENDIF;
089200141114         WHEN VisObj;
089300150112           IKC71pea = V02obj;
089400141114           IF  V02objs = '-';
089500150112             IKC71pea = IKC71pea * -1;
089600141114           ENDIF;
089700141114         ENDSL;
089800150112         TRKC71R (kpjba:TRKC71DS);
089900150112         IF  OKC71err <> *blanks;
090000141126           ErrMessage  = *on;
090100141126           ErrGenerico = *on;
090200150112           V02msg = OKC71msg;
090300141126           leavesr;
090400141126         ENDIF;
090500150123
090600150123         wAdesso = %dec( %time() + %seconds(1) );
090700141114
090800141114       //?Scrivo le note
090900141114         IF  V02nota1 <> *blanks or V02nota2 <> *blanks;
091000150112           clear TRKC10DS;
091100150112           IKC10tla = 'L';
091200150112           IKC10flm = 'C';
091300150112           IKC10fno = 'S';
091400150112           IKC10ncm = V02ncm;
091500150112           IKC10ksu = V02ksu;
091600150112           IKC10ksc = V02ksc;
091700150112           IKC10cpo = V02cpo;
091800150112           IKC10acm = IKC07acm;
091900150112           IKC10des = V02des;
092000141114           IF  V02cpo > 0;
092100141114           ELSE;
092200150112             IKC10rsc = V02rag;
092300141114           ENDIF;
092400150112           EKC10no1 = V02nota1;
092500150112           EKC10no2 = V02nota2;
092600150112           EKC10dim = %dec(%date());
092700150112           EKC10him = %dec(%time());
092800150112           trkc10r (kpjba:TRKC10DS);
092900150120           V02notaA = oKC10piu;
093000141114         ENDIF;
093100150123
093200150123
093300150216         //// -?Registrazione Fase di Chiusura (" 90") - Automatica?
093400150216         //If  VisObjFine  and  W02objfine = *zero;
093500150216         //
093600150216         //  clear  TRKC71ds;
093700150216         //  iKC71ncm = V02ncm;
093800150216         //  iKC71ksu = V02ksu;
093900150216         //  iKC71ksc = V02ksc;
094000150216         //  iKC71cpo = V02cpo;
094100150216         //  iKC71acm = FaseChiudi;          //?" 90"?
094200150216         //  iKC71cch = c_CausaleChiusura;   //?"NA"?
094300150216         //  iKC71aut = 'A';                 //?(Fase Automatica)?
094400150216         //  iKC71hfc = wAdesso;             //?(Ora da forzare: successiva di 1 sec.)?
094500150216         //
094600150216         //  TRKC71R ( kpjba : TRKC71ds );
094700150216         //
094800150216         //  if  oKC71err <> *blank;
094900150216         //    ErrMessage  = *on;
095000150216         //    ErrGenerico = *on;
095100150216         //    V02msg = oKC71msg;
095200150216         //    leavesr;
095300150216         //  endif;
095400150216         //
095500150216         //EndIf;
095600150123
095700141110
095800141110       //?Chiusura del programma
095900141110         Fine = *on;
096000141110
096100141110       ENDSR;
096200141110
096300141110       //--------------------------------------------------------------
096400141110       //?Gestione tasto funzionale F12 da videata D02
096500141110       //?F12=Ritorno
096600141110       //--------------------------------------------------------------
096700141110       BEGSR F12D02;
096800141117
096900141117       //?Imposto F12
097000150112         OKC07F12 = 'S';
097100141110
097200141110       //?Chiusura del programma
097300141110         Fine = *on;
097400141110
097500141110       ENDSR;
097600141114
097700141114       //--------------------------------------------------------------
097800141114       //?Gestione tasto funzionale F18 da videata D02
097900141114       //?F18=Note
098000141114       //--------------------------------------------------------------
098100141114       BEGSR F18D02;
098200141114
098300150112         clear TRKC10DS;
098400150112         IKC10flm = 'M';
098500150112         IKC10ncm = V02ncm;
098600150112         IKC10ksu = V02ksu;
098700150112         IKC10ksc = V02ksc;
098800150112         IKC10cpo = V02cpo;
098900150112         IKC10acm = IKC07acm;
099000150112         IKC10des = V02des;
099100141114         IF  V02cpo > 0;
099200141114         ELSE;
099300150112           IKC10rsc = V02rag;
099400141114         ENDIF;
099500150112         EKC10dim = %dec(%date());
099600150112         EKC10him = %dec(%time());
099700150112         EKC10no1 = V02nota1;
099800150112         EKC10no2 = V02nota2;
099900150112         trkc10r (kpjba:TRKC10DS);
100000150112         V02nota1 = EKC10no1;
100100150112         V02nota2 = EKC10no2;
100200150120         V02notaA = oKC10piu;
100300141114
100400141114       ENDSR;
100500141118
100600141118       //--------------------------------------------------------------
100700141118       //?Gestione videata D03.
100800141118       //--------------------------------------------------------------
100900141118       BEGSR GesD03;
101000141118
101100141118       //?Inizializzazione videata
101200141118         IF  wInzD03;
101300141118           exsr InzD03;
101400141118           wInzD03 = *off;
101500141118         ENDIF;
101600141118
101700141118       //?Emissione Testata
101800150112         write  KC07T01;
101900141118
102000141118       //?Emissione videata
102100150112         exfmt  KC07D03;
102200141118         reset ErrMessage;
102300141118         reset ErrGenerico;
102400141118
102500141118         SELECT;
102600150115
102700150115       //?- F05=Ripristino
102800150115           WHEN  dsp_aid = c_F05;
102900150115             exsr F05D03;
103000141118
103100141118       //?- F08=Storico
103200141118           WHEN  dsp_aid = c_F08;
103300141118             exsr F08D03;
103400141118
103500141118       //?- F12=Ritorno
103600141118           WHEN  dsp_aid = c_F12;
103700141118             exsr F12D02;
103800141118
103900141118       //?- F18=Note
104000141118           WHEN  dsp_aid = c_F18;
104100141118             exsr F18D03;
104200141118
104300141118         ENDSL;
104400141118
104500141118       ENDSR;
104600141118
104700141118       //--------------------------------------------------------------
104800141118       //?Inizializzazione Videata D03.
104900141118       //--------------------------------------------------------------
105000141118       BEGSR InzD03;
105100141118
105200141118       //?Pulizia videata
105300150112         clear KC07D03;
105400150115
105500150115       //?Se richiamato per ripristino abilito F5=Ripristino
105600150115         AbilitaF5 = (IKC07ric = 'R');
105700141118
105800141118       //?Carico le parti comuni alle due videate
105900141118         exsr PartiComuni;
106000141118
106100141118       //?cerco Obiettivo Iniziale
106200141118         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
106300141118         IF  %found(TICMF01L);
106400150224           V02objA = %editc( CMFpea : 'J' );
106500141118         ENDIF;
106600141118       //?cerco Obiettivo Proposto
106700141118         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
106800141118         IF  %found(TICMF01L);
106900150224           V02objproA = %editc( CMFpea : 'J' );
107000141118         ENDIF;
107100141118       //?cerco Obiettivo Finale
107200141118         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
107300141118         IF  %found(TICMF01L);
107400150224           V02objfin  = %editc( CMFpea : 'J' );
107500141118         ENDIF;
107600141118       //?cerco Obiettivo Trattativa
107700150224         wfase = FaseObjTtr;
107800150224         exsr CallTRKC76;
107900150224         IF  OKC76err = *blanks;
108000150224           V02objTtrA = OKC76peaa;
108100150224           V02esito  = OKC76flag;
108200150224           IF  OKC76anno > 0;
108300150224             V02decot = 'Decorrenza Tariffa: ' +
108400150224                         %trim(%editc(OKC76mese:'3')) +
108500150224                        '/' + %editc(OKC76anno:'X');
108600150224           ENDIF;
108700150225           V02forza = OKC76forza;
108800150224           VisObjTTR = *on;
108900150224         ENDIF;
109000141118       //?cerco Obiettivo Confronto Fatturazione
109100150224         wfase = FaseObjCf;
109200150224         exsr CallTRKC76;
109300150224         IF  OKC76err = *blanks;
109400150224           V02objcfA = OKC76peaa;
109500150224           IF  OKC76anno > 0;
109600150224             V02cfp = 'Fatturato del ' + %trim(%editc(OKC76mese:'3')) +
109700150224                                   '/' + %editc(OKC76anno:'X');
109800150224           ENDIF;
109900150224           VisOBJcfa = *on;
110000141118         ENDIF;
110100150121
110200141118       //?cerco Causale chiusura
110300141118         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseChiudi) TICMF01L;
110400141118         IF  %found(TICMF01L);
110500141118           V02cch = CMFcch;
110600141118         //?decodifico Causale chiusura
110700141118           T02mod = 'C';
110800141118           T02cod = 'ECM';
110900141118           T02ke1 = V02cch;
111000141118           T02sif = KNSIF;
111100141118           TNTBE_RicercaControllo  (kpjba : tibs02ds);
111200150128           V02cchD = T02uni;
111300141118         ENDIF;
111400150128
111500150128         // -?Impostazione Attributi di Visualizzazione:?
111600150128         // ·?Fase Obiettivo Iniziale?
111700150128         VisOBJ     = ( V02objA    <> *blank );
111800150128         // ·?Fase Obiettivo Proposto?
111900150128         VisOBJprop = ( V02objProA <> *blank );
112000150128         // ·?Fase Obiettivo Finale?
112100150225         VisOBJfine = ( V02objFin  <> *blank );
112200150128         VisOBJcfa  = ( V02objCfA  <> *blank );
112300150128         // ·?Fase Esclusione Campagna?
112400150128         VisCCH     = ( V02cchd    <> *blank );
112500141118
112600141118       ENDSR;
112700150115
112800150115       //--------------------------------------------------------------
112900150115       //?Gestione tasto funzionale F05 da videata D03
113000150115       //?F05=Ripristino
113100150115       //--------------------------------------------------------------
113200150115       BEGSR F05D03;
113300150116
113400150116         clear TRKC74DS;
113500150116         IKC74ncm = V02ncm;
113600150116         IKC74ksu = V02ksu;
113700150116         IKC74ksc = V02ksc;
113800150116         IKC74cpo = V02cpo;
113900150116         kpjbu = TRKC74DS;
114000150116         TRKC74R (kpjba);
114100150116         TRKC74DS = kpjbu;
114200150116         IF  OKC74err = '1';
114300150116           ErrMessage  = *on;
114400150116           ErrGenerico = *on;
114500150116           V02msg = OKC74msg;
114600150116           leavesr;
114700150116         ENDIF;
114800150116
114900150116       //?Chiusura del programma
115000150116         Fine = *on;
115100150115
115200150115       ENDSR;
115300141118
115400141118       //--------------------------------------------------------------
115500141118       //?Gestione tasto funzionale F08 da videata D03
115600141118       //?F08=Storico
115700141118       //--------------------------------------------------------------
115800141118       BEGSR F08D03;
115900141118
116000141118       //?Scrivo la fase
116100150112         clear TRKC03DS;
116200150112         IKC03ncm = V02ncm;
116300150112         IKC03ksu = V02ksu;
116400150112         IKC03ksc = V02ksc;
116500150112         IKC03cpo = V02cpo;
116600150112         TRKC03R (kpjba:TRKC03DS);
116700141118
116800141118       ENDSR;
116900141118
117000141118       //--------------------------------------------------------------
117100141118       //?Gestione tasto funzionale F18 da videata D03
117200141118       //?F18=Note
117300141118       //--------------------------------------------------------------
117400141118       BEGSR F18D03;
117500141118
117600150112         clear TRKC10DS;
117700150112         IKC10tla = 'L';
117800150112         IKC10flm = 'I';
117900150112         IKC10ncm = V02ncm;
118000150112         IKC10ksu = V02ksu;
118100150112         IKC10ksc = V02ksc;
118200150112         IKC10cpo = V02cpo;
118300150112         IKC10acm = IKC07acm;
118400150112         IKC10des = V02des;
118500141118         IF  V02cpo > 0;
118600141118         ELSE;
118700150112           IKC10rsc = V02rag;
118800141118         ENDIF;
118900150112         EKC10dim = %dec(%date());
119000150112         EKC10him = %dec(%time());
119100150112         EKC10no1 = V02nota1;
119200150112         EKC10no2 = V02nota2;
119300150112         trkc10r (kpjba:TRKC10DS);
119400150112         V02nota1 = EKC10no1;
119500150112         V02nota2 = EKC10no2;
119600150120         V02notaA = oKC10piu;
119700141118
119800141118       ENDSR;
119900141118
120000141118       //--------------------------------------------------------------
120100141118       //?Carico le parti comuni alle due videate.
120200141118       //--------------------------------------------------------------
120300141118       BEGSR PartiComuni;
120400141118
120500150112         V02ncm = IKC07ncm;
120600141118         V02des = CMPdes;
120700141202         V02tpc = CMPtpc;
120800141202         clear TIBS02DS;
120900141202         T02mod = 'C';
121000141202         T02cod = 'TCM';
121100141202         T02ke1 = V02tpc;
121200141202         T02sif = KNSIF;
121300141202         TNTBE_RicercaControllo  (kpjba : tibs02ds);
121400141202         IF  T02err = *blanks;
121500141202           V02dtpc = T02uni;
121600141202         ENDIF;
121700150306
121800150306         clear wlbdat;
121900150306         G08inv = CMPdic;
122000150306         G08err = '3';
122100150306         XSRDA8(WLBdat);
122200150306         V02dic = G08dat;
122300141118
122400150306         clear wlbdat;
122500150306         G08inv = CMPdfc;
122600150306         G08err = '3';
122700150306         XSRDA8(WLBdat);
122800150306         V02dfc = G08dat;
122900141118
123000150112         V02ksu = IKC07ksu;
123100141118         V02rag = ACOrag;
123200150112         V02ksc = IKC07ksc;
123300150112         V02cpo = IKC07cpo;
123400141118
123500141118         chain (V02ncm:V02ksu:V02ksc:V02cpo) TICMI01L;
123600141118         IF  not %found(TICMI01L);
123700141118           leavesr;
123800141118         ENDIF;
123900141118
124000141118         V02clv = CMIclv;
124100141118
124200141118         V02cmm = CMIcmm;
124300141118         chain CMIcmm AZCMM01L;
124400141118         IF  %found(AZCMM01L);
124500141118           V02cmmd = CMMdes;
124600141118         ENDIF;
124700141118
124800141118         V02car = CMIacm;
124900141118         V02dis = CMIdcm;
125000150120         //V02cln = CMIcln;
125100150120         //V02newmese = CMImes;
125200150120         select;
125300150120           when  CMIcln = 'N';
125400150120             V02clnMm = 'Nuovo';
125500150120           when  CMIcln = 'A';
125600150120             V02clnMM = 'Acquisito';
125700150120           other;
125800150123             V02clnMM = 'Nuovo/Acquisito ' + CMIcln + ' ?????';
125900150120         endsl;
126000150120         if  CMImes > *zero  and  ( CMIcln = 'N'  or
126100150120                                    CMIcln = 'A' );
126200150123           V02clnMM = %trim( V02clnMM ) + ' dal mese di';
126300150123           if  CMImes < 13;
126400150123             V02clnMM = %trim( V02clnMM ) + ' ' + sk_Mesi( CMImes );
126500150123           else;
126600150123             V02clnMM = %trim( V02clnMM ) + ' ' + %editc( CMImes : 'X' )
126700150123                                                + ' ?????';
126800150123           endif;
126900150120         endif;
127000141118         IF  CMIist = 'S';
127100141118           V02ist = 'SI';
127200141118         ENDIF;
127300141118         IF  CMIist = 'N';
127400141118           V02ist = 'NO';
127500141118         ENDIF;
127600141118         V02del = CMIpde;
127700141118         V02ric = CMIric;
127800141118         V02spe = CMInsp;
127900141118         V02pkg = CMIpme;
128000150120
128100150121         // -?Caricam. delle prime 2 righe delle ultime Note inserite?
128200150121         //  ?(di qualunque fase...)?
128300150120         clear  TRKC10ds;
128400150120         iKC10flm = 'R';
128500150120         iKC10ncm = V02ncm;
128600150120         iKC10ksu = V02ksu;
128700150120         iKC10ksc = V02ksc;
128800150120         iKC10cpo = V02cpo;
128900150121         iKC10acm = *all'9';
129000150120         iKC10des = V02des;
129100150120         iKC10rsc = V02rag;
129200150120         trkc10r ( kpjba : TRKC10ds );
129300150120         V02nota1o = eKC10no1;
129400150120         V02nota2o = eKC10no2;
129500150120         V02notaAo = oKC10piu;
129600150121
129700150121         VisNoteO   = (eKC10no1 + eKC10no2 <> *blank);
129800150121         EvidenzF18 = VisNoteO;
129900141118
130000141118       ENDSR;
130100150224
130200150224       //--------------------------------------------------------------
130300150224       //?Richiamo pgm TRKC76R.
130400150224       //--------------------------------------------------------------
130500150224       BEGSR CallTRKC76;
130600150224
130700150224         wTRKC76 = *on;
130800150224
130900150224         clear TRKC76DS;
131000150224         IKC76ric = 'F';
131100150224         IKC76ncm = CMIncm;
131200150224         IKC76ksu = CMIksu;
131300150224         IKC76ksc = CMIksc;
131400150224         IKC76cpo = CMIcpo;
131500150224         IKC76acm = wFase;
131600150224         IKC76istat = CMIist;
131700150224
131800150224         trkc76r (kpjba:TRKC76DS);
131900150224
132000150224       ENDSR;
132100141110
132200141110       //--------------------------------------------------------------
132300141110       //?Operazioni finali.
132400141110       //--------------------------------------------------------------
132500141110       BEGSR RoutEnd;
132600150224
132700150224       //?Se richiamato chiudo i file per il pgm TRKC76R
132800150224         IF  wTRKC76;
132900150224           clear TRKC76DS;
133000150224           IKC76ric = 'C';
133100150224           trkc76r (kpjba:TRKC76DS);
133200150224         ENDIF;
133300141110
133400141110         *inLR = *on;
133500141110         return;
133600141110
133700141110       ENDSR;
133800141110
133900141110       //--------------------------------------------------------------
134000141110
134100141110      /end-free
134200141110
134300141110       //--------------------------------------------------------------
134400141110       //?Schiere a tempo di compilazione.
134500141110       //--------------------------------------------------------------
134600141110
134700141110** -- MSG -------------------------------------------------------------------*
134800141114Campagna commerciale errata                                                    1
134900141114Cliente/Potenziale errato                                                      2
135000141114Fase Errata                                                                    3
135100141121Se % a 0 non può essere in negativo                                            4
135200141114Causale Errata                                                                 5
135300141114Immettere le note                                                              6
135400150218Immettere la percentuale                                                       7
135500150216Percentuale errata                                                             8
135600150220Percentuale Andamento Trattativa obbligatoria                                  9
135700150223Decorrenza tariffa errata                                                      10
135800150306Anno decorrenza tariffa errato                                                 11
135900150123** -?sk_Mesi?
136000150123Gennaio  | 1
136100150123Febbraio | 2
136200150123Marzo    | 3
136300150123Aprile   | 4
136400150123Maggio   | 5
136500150123Giugno   | 6
136600150123Luglio   | 7
136700150123Agosto   | 8
136800150123Settembre| 9
136900150123Ottobre  |10
137000150123Novembre |11
137100150123Dicembre |12
