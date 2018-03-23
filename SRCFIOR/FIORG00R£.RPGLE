000100160420      //--------------------------------------------------------------
000200160421      //?FIORG00R - GENERA ARCHIVIO RITIRI PER TELLUS - DOWNLOAD
000300160420      //--------------------------------------------------------------
000400160420     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160420     h dftactgrp(*no)
000600160420
000700160420      //---------------------------------------------------------------
000800160420      //?Dichiarazione file.
000900160420      //---------------------------------------------------------------
001000160420      // - Organigramma
001100160420     fAZORG01L  if   e           k disk
001200160420
001300160420
001400160420      // - Video Gestione Campagne
001500160421     fFIORG00D  cf   e             workstn
001600160420     f                                     indds(IndDspF)
001700160420     f                                     infds(InfDspF)
001800160420
001900160420      //---------------------------------------------------------------
002000160420      //?Definizione costanti.
002100160420      //---------------------------------------------------------------
002200160420      // - Tasti funzionali a video
002300160420     d c_F01           c                   const(x'31')
002400160420     d c_F02           c                   const(x'32')
002500160420     d c_F03           c                   const(x'33')
002600160420     d c_F04           c                   const(x'34')
002700160420     d c_F05           c                   const(x'35')
002800160420     d c_F06           c                   const(x'36')
002900160420     d c_F07           c                   const(x'37')
003000160420     d c_F08           c                   const(x'38')
003100160420     d c_F09           c                   const(x'39')
003200160420     d c_F10           c                   const(x'3A')
003300160420     d c_F11           c                   const(x'3B')
003400160420     d c_F12           c                   const(x'3C')
003500160420     d c_F13           c                   const(x'B1')
003600160420     d c_F14           c                   const(x'B2')
003700160420     d c_F15           c                   const(x'B3')
003800160420     d c_F16           c                   const(x'B4')
003900160420     d c_F17           c                   const(x'B5')
004000160420     d c_F18           c                   const(x'B6')
004100160420     d c_F19           c                   const(x'B7')
004200160420     d c_F20           c                   const(x'B8')
004300160420     d c_F21           c                   const(x'B9')
004400160420     d c_F22           c                   const(x'BA')
004500160420     d c_F23           c                   const(x'BB')
004600160420     d c_F24           c                   const(x'BC')
004700160420     d c_Enter         c                   const(x'F1')
004800160420     d c_RollDown      c                   const(x'F4')
004900160420     d c_RollUp        c                   const(x'F5')
005000160420
005100160420     d Digits          c                   const('0123456789')
005200160420
005300160420      //---------------------------------------------------------------
005400160420      //?Definizione schiere.
005500160420      //---------------------------------------------------------------
005600160420      // - Messaggi di errore
005700160420     d Msg             s             78    dim(50) ctdata perrcd(1)
005800160420
005900160420      //---------------------------------------------------------------
006000160420      //?Definizione aree dati.
006100160420      //---------------------------------------------------------------
006200160420      // - Dati utente
006300160420     d §AzUte        e ds                  extname(AZUTE00F)
006400160420     d                                     dtaara
006500160420     d §DatiUte      e ds                  extname(dDatiUte)
006600160420     d                                     dtaara
006700160420
006800160420      //---------------------------------------------------------------
006900160420      //?Definizione strutture dati.
007000160420      //---------------------------------------------------------------
007100160420      // - Status
007200160420     d Psds           sds
007300160420     d   SDSpgm          *proc
007400160420
007500160420      // - InfDS
007600160420     d InfDspF         ds
007700160420     d  dsp_aid              369    369a
007800160420
007900160420      // - Indicatori su DspF
008000160420     d IndDspF         ds
008100160420        // - Indicatori di errore in videata
008200160420     d  ErrMessage                    1n   overlay(IndDspF : 28)
008300160420        // - Indicatori di Visualizzazione/Protezione Campi
008400160420     d  UtenteEDP                     1n   overlay(IndDspF : 41)
008500160420     d  Distinta                      1n   overlay(IndDspF : 42)
008600160420     d  Giro                          1n   overlay(IndDspF : 43)
008700160420        // - Indicatori di errore
008800160420     d  PosCurPOR                     1n   overlay(IndDspF : 50)
008900160420        // - Indicatori di errore generico
009000160420     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009100160420
009200160420     d WindDspF        ds                  inz
009300160420     d  WindDspFa              1     49    inz(*zero)
009400160420     d  WindDspFb             50     99    inz(*zero)
009500160420
009600160420      // - Parametri ricevuti
009700160420     d KPJBA         e ds
009800160421     d FIORG01DS     e ds
009900160421
010000160421      // - Controllo Terminal
010100160421     d FNLV55DS      e ds                  inz
010200160420
010300160420      // - Ricerca/Controllo tabelle
010400160420     d TIBS02DS      e ds                  inz
010500160421
010600160421      // - Reperimento dati utente
010700160421     d TIBS34DS      e ds
010800160420
010900160420      // - Reperimento filiali gestite dall'utente
011000160420     d TRUL31DS      e ds
011100160420     d  POG                   10    759    dim(250)
011200160614
011300160614      // - Reperimento filiali abilitate alla procedura
011400160614     d TRULVPODS     e ds
011500160614     d  fil868                16    765    dim(250)
011600160420
011700160420      // - Tabella LAT - Autorizzazioni
011800160420     d dLAT          e ds
011900160421
012000160421      // - Tabella VPO - Tellus
012100160421     d dVPOTELLUS    e ds
012200160421
012300160421      // ds per campo ORGDE7 di AZORG
012400160421     d OG147         e ds
012500160421
012600160421      // ds per campo ORGDE8 di AZORG
012700160421     d OG148         e ds
012800160420
012900160420      //---------------------------------------------------------------
013000160420      //?Definizione variabili globali.
013100160420      //---------------------------------------------------------------
013200160420      // - Flags booleani
013300160420     d ErrGrave        s               n   inz(*off)
013400160420     d Fine            s               n   inz(*off)
013500160420     d wEnd            s               n   inz(*off)
013600160420     d wInzD01         s               n   inz(*off)
013700160420
013800160420      // - Indici di schiera
013900160420     d xx              s              4s 0 inz
014000160420
014100160420      // - Campi associati al video
014200160420     d Video           s              2a   inz
014300160420
014400160420      // - Campi di comodo data
014500160420     d Data_EUR        s               d   datfmt(*eur)
014600160420     d Data_ISO        s               d   datfmt(*iso)
014700160420
014800160420      // - Campi di comodo
014900160420     d Oggi            s              8s 0 inz
015000160420     d wabi            s              2a   inz
015100160421     d wdata           s              8s 0 inz
015200160421     d wmsg            s                   like(V01msg) inz
015300160421     d wpor            s                   like(V01por) inz
015400160421     d w025a           s             25a   inz
015500160420
015600160420      //---------------------------------------------------------------
015700160421      //?Definizione Procedure usate.
015800160420      //---------------------------------------------------------------
015900160421      // - Sottomissione lavoro batch
016000160421     d bch10           pr                  extpgm('BCH10')
016100160421     d  kpjba                              likeds(KPJBA)
016200160421
016300160421       // - Scambio dati TELLUS
016400160421     d FIORG01R        pr                  extpgm('FIORG01R')
016500160421     d  kpjba                              likeds(KPJBA)
016600160421
016700160421       // - Controllo Terminal
016800160421     d FNLV55R         pr                  extpgm('FNLV55R')
016900160421     d  fnlv55ds                           likeds(fnlv55ds)
017000160421
017100160420       // - Caricamento Filiali in gestione
017200160420     d TRUL31R         pr                  extpgm('TRUL31R')
017300160420     d  kpjba                              likeds(kpjba)
017400160420     d  trul31ds                           likeds(trul31ds)
017500160614
017600160614       // - Caricamento Filiali abilitate
017700160614     d TRULVPOR        pr                  extpgm('TRULVPOR')
017800160614     d  trulvpods                          likeds(trulvpods)
017900160420
018000160420      //---------------------------------------------------------------
018100160420      //?Definizione Prototipi.
018200160420      //---------------------------------------------------------------
018300160420      /copy gaitrasrc/srcprotopr,TIBS02R
018400160420      /copy gaitrasrc/srcprotopr,TIBS34R
018500160420
018600160420      //---------------------------------------------------------------
018700160420      //?Definizione key-list.
018800160420      //---------------------------------------------------------------
018900160420
019000160420      //---------------------------------------------------------------
019100160420      //?M A I N - L I N E
019200160420      //---------------------------------------------------------------
019300160420
019400160420     c     *Entry        plist
019500160420     c                   parm                    kpjba
019600160420
019700160420      /free
019800160420
019900160420       //?Operazioni iniziali
020000160420       exsr RoutInz;
020100160420
020200160420       //?Gestione video
020300160420       DOW  Fine = *off;
020400160420         SELECT;
020500160420         WHEN  Video = 'D1';
020600160420           exsr GesD01;
020700160420         OTHER;
020800160420           Fine = *on;
020900160420         ENDSL;
021000160420       ENDDO;
021100160420
021200160420       //?Operazioni finali
021300160420       exsr RoutEnd;
021400160420
021500160420       //--------------------------------------------------------------
021600160420       //?Operazioni iniziali.
021700160420       //--------------------------------------------------------------
021800160420       BEGSR RoutInz;
021900160420
022000160420       //?Reperimento dati job
022100160420         exsr DatiJob;
022200160420
022300160420       //?Impostazione campi "fissi"
022400160420         V01pgm = SDSpgm;
022500160420         Video = 'D1';
022600160420         wInzD01 = *on;
022700160420
022800160420       //?Imposto oggi
022900160420         Oggi = %dec(%date());
023000160420
023100160420       //?Se ho già un errore grave esco
023200160420         IF  ErrGrave;
023300160420           leavesr;
023400160420         ENDIF;
023500160420
023600160420       //?Carico le filiali abilitate all'utente
023700160420         clear TRUL31DS;
023800160420         I31abi = wabi;
023900160420         I31cdi = DUTdis;
024000160420         I31car = DUTare;
024100160420         I31cpo = DUTpou;
024200160420         TRUL31R (kpjba:trul31ds);
024300160420         IF  O31pog <= *zeros;
024400160420           ErrGrave = *on;
024500160420           leavesr;
024600160420         ENDIF;
024700160421
024800160428       //?Aggancio tabella VPO 'TELLUSORM'
024900160421         clear dVPOTELLUS;
025000160421         clear TIBS02DS;
025100160421         T02mod = 'C';
025200160421         T02sif = knsif;
025300160421         T02cod = 'VPO';
025400160428         T02ke1 = 'TELLUSORM';
025500160421         TNTBE_RicercaControllo (kpjba:tibs02ds);
025600160421         IF  T02err = *blanks;
025700160421           dVPOTELLUS = T02uni;
025800160421         ENDIF;
025900160614
026000160614       //?Carico le filiali abilitate alla procedura
026100160614         clear TRULVPODS;
026200160614         IVPOke1 = 'DECOFI868';
026300160614         trulvpor (trulvpods);
026400160420
026500160420       ENDSR;
026600160420
026700160420       //--------------------------------------------------------------
026800160420       //?Reperimento Dati del job (Utente/Operativi).
026900160420       //--------------------------------------------------------------
027000160420       BEGSR DatiJob;
027100160420
027200160420         in(E) §AzUte;
027300160420         IF  NOT %error;
027400160420           in(E) §DatiUte;
027500160420         ENDIF;
027600160420         IF  %error or RSut = *blanks;
027700160420           clear TIBS34ds;
027800160420           tibs34r(tibs34ds);
027900160420           in §AzUte;
028000160420           in §DatiUte;
028100160420         ENDIF;
028200160420
028300160420         clear wabi;
028400160420         clear dLAT;
028500160420
028600160420       //?Controllo autorizzazioni profilo
028700160420         SELECT;
028800160420         WHEN  DUTerr = 'E';
028900160420           ErrGrave = *on;
029000160420           leavesr;
029100160420         WHEN  UTEaut <> *blanks;
029200160420           wabi = UTEaut;
029300160420         OTHER;
029400160420           IF  DUTlpo = '1';
029500160420             wabi = 'TP';
029600160420           ENDIF;
029700160420           IF  DUTlpo = '2';
029800160420             wabi = 'PO';
029900160420           ENDIF;
030000160420         ENDSL;
030100160420
030200160420       //?Controllo se l'autorizzazione è valida
030300160421         clear dLAT;
030400160420         clear TIBS02DS;
030500160420         T02mod = 'C';
030600160420         T02sif = knsif;
030700160420         T02cod = 'LAT';
030800160420         T02ke1 = wabi;
030900160420         TNTBE_RicercaControllo (kpjba:tibs02ds);
031000160420         dLAT = T02uni;
031100160420         IF  T02err <> *blanks or §LATabi = 'S';
031200160420           ErrGrave = *on;
031300160420           leavesr;
031400160420         ENDIF;
031500160420
031600160420       ENDSR;
031700160420
031800160420       //--------------------------------------------------------------
031900160420       //?Gestione videata D01.
032000160420       //--------------------------------------------------------------
032100160420       BEGSR GesD01;
032200160420
032300160420       //?Inizializzazione videata
032400160420         IF  wInzD01;
032500160420           exsr InzD01;
032600160420           wInzD01 = *off;
032700160420         ENDIF;
032800160420
032900160420       //?Se errore grave emetto messaggio poi esco
033000160420         IF  ErrGrave;
033100160420           ErrMessage  = *on;
033200160420           ErrGenerico = *on;
033300160420           V01msg = Msg(01);
033400160420         ENDIF;
033500160420
033600160420       //?Emissione Testata
033700160421         write ORG00T01;
033800160420
033900160420       //?Emissione videata
034000160421         exfmt ORG00D01;
034100160420         reset ErrMessage;
034200160420         reset ErrGenerico;
034300160420         clear V01msg;
034400160420
034500160420         SELECT;
034600160420
034700160420       //?- Errore Grave esco
034800160420         WHEN  ErrGrave;
034900160420           Fine = *on;
035000160420
035100160420       //?- F03=Fine
035200160420         WHEN  dsp_aid = c_F03;
035300160420           exsr F03D01;
035400160420
035500160420       //?- F06=Conferma
035600160420         WHEN  dsp_aid = c_F06;
035700160614           exsr ContrD01;
035800160614           IF  ErrGenerico;
035900160614             leavesr;
036000160614           ENDIF;
036100160420           exsr F06D01;
036200160421           IF  ErrGenerico;
036300160421             leavesr;
036400160421           ENDIF;
036500160420
036600160420       //?Invio
036700160420         OTHER;
036800160420           exsr ContrD01;
036900160420           IF  ErrGenerico;
037000160420             leavesr;
037100160420           ENDIF;
037200160420
037300160420         ENDSL;
037400160420
037500160420       ENDSR;
037600160420
037700160420       //--------------------------------------------------------------
037800160420       //?Inizializzazione Videata D01.
037900160420       //--------------------------------------------------------------
038000160420       BEGSR InzD01;
038100160420
038200160420       //?Pulizia videata
038300160421         clear ORG00D01;
038400160420         UtenteEDP = *off;
038500160420         Distinta = *off;
038600160420         Giro = *off;
038700160420
038800160420
038900160420       //?Utente EDP
039000160420         UtenteEDP = %subst(knmus:1:3) = 'EDP';
039100160421         IF  UtenteEDP;
039200160421           Distinta = *on;
039300160421           Giro     = *on;
039400160421         ENDIF;
039500160420
039600160420       //?Imposto la filiale ritiro
039700160420         V01por  = DUTpou;
039800160420         V01pord = DUTdpo;
039900160421
040000160421       //?Dati filiale utente
040100160421         clear wdata;
040200160421         clear OG147;
040300160421         chain (V01por) AZORG01L;
040400160421         IF  %found(AZORG01L);
040500160421           OG147 = ORGde7;
040600160421         ENDIF;
040700160421         IF  §OGddao <> *blanks and §OGddao > *zeros and
040800160421             §OGddao <> '20391231';
040900160421           wdata = %dec(§OGddao:8:0);
041000160421         ENDIF;
041100160421         IF  wdata = 0 or wdata > Oggi;
041200160421           Distinta = *on;
041300160421         ELSE;
041400160421           Giro = *on;
041500160421           V01giro = 'S';
041600160421         ENDIF;
041700160421
041800160421       //?Imposto dati per parte EDP
041900160421         V01data = Oggi;
042000160421         V01ritmax = §VPObomax;
042100160420
042200160420       ENDSR;
042300160420
042400160420       //--------------------------------------------------------------
042500160421       //?Gestione tasto funzionale F03 da videata D01
042600160420       //?F03=Fine
042700160420       //--------------------------------------------------------------
042800160421       BEGSR F03D01;
042900160420
043000160420       //?Chiusura del programma
043100160420         Fine = *on;
043200160420
043300160420       ENDSR;
043400160420
043500160420       //--------------------------------------------------------------
043600160421       //?Gestione tasto funzionale F06 da videata D01
043700160421       //?F06=Conferma
043800160420       //--------------------------------------------------------------
043900160421       BEGSR F06D01;
044000160420
044100160421       //?Se ho un nr. minimo di ritiri da elaborare
044200160421       //?faccio prima un giro di controllo
044300160421         IF  §VPObotlls > 0;
044400160421           exsr GiroControllo;
044500160421           IF  OORG01min  = 'S';
044600160421             ErrGenerico = *on;
044700160421             ErrMessage  = *on;
044800160421             PosCurPOR   = *on;
044900160421             V01msg      = Msg(05);
045000160421             %subst(V01msg:15:3) = %editc(OORG01nrit:'Z');
045100160421             leavesr;
045200160421           ENDIF;
045300160421         ENDIF;
045400160421
045500160421       //?Elaborazione definitiva
045600160421         clear FIORG01DS;
045700160421         IORG01por  = V01por;
045800160421         IORG01data = V01data;
045900160421         IORG01dist = V01dist;
046000160421         IORG01giro = V01giro;
046100160421         IORG01test = V01test;
046200160421         kpjbu = FIORG01DS;
046300160421         kcoaz = 'ORG1';
046400160421         clear kbuff;
046500160421         BCH10(kpjba);
046600161118
046700161118       //?Chiusura del programma
046800161118         Fine = *on;
046900160420
047000160420       ENDSR;
047100160420
047200160420       //--------------------------------------------------------------
047300160421       //?Controlla Videata D01.
047400160420       //--------------------------------------------------------------
047500160421       BEGSR ContrD01;
047600160420
047700160420         WindDspF = IndDspF;
047800160420         reset WindDspFb;
047900160420         IndDspF  = WindDspF;
048000160420
048100160420       //?Filiale Ritiro
048200160421         wpor = V01por;
048300160420         exsr FilialeRitiro;
048400160421         V01por  = wpor;
048500160421         V01pord = w025a;
048600160421         V01msg = wmsg;
048700160420         IF  ErrGenerico;
048800160420           leavesr;
048900160420         ENDIF;
049000160421
049100160421       ENDSR;
049200160421
049300160421       //--------------------------------------------------------------
049400160421       //?Controllo Filiale Ritiro;
049500160421       //--------------------------------------------------------------
049600160421       BEGSR FilialeRitiro;
049700160421
049800160421         IF  wpor = *zeros;
049900160421           ErrGenerico = *on;
050000160421           ErrMessage  = *on;
050100160421           PosCurPOR   = *on;
050200160421           wmsg        = Msg(02);
050300160421           leavesr;
050400160421         ENDIF;
050500160421
050600160421       //?Deve esistere la Filiale
050700160421         clear wdata;
050800160421         clear OG147;
050900160421         clear OG148;
051000160421         chain (V01por) AZORG01L;
051100160421         IF  not %found(AZORG01L) or ORGfva <> *blanks;
051200160421           ErrGenerico = *on;
051300160421           ErrMessage  = *on;
051400160421           PosCurPOR   = *on;
051500160421           wmsg        = Msg(02);
051600160421           leavesr;
051700160421         ENDIF;
051800160421         OG147 = ORGde7;
051900160421         OG148 = ORGde8;
052000160421
052100160421       //?Decodifico filiale
052200160421         w025a = ORGdes;
052300160421
052400160421       //?Filiale in gestione all'utente
052500160421       //?se NON EPD
052600160421         IF  %lookup(%editc(V01por:'X'):pog) = 0 and not UtenteEDP;
052700160421           ErrGenerico = *on;
052800160421           ErrMessage  = *on;
052900160421           PosCurPOR   = *on;
053000160614           wmsg        = Msg(02);
053100160421           leavesr;
053200160421         ENDIF;
053300160614
053400160614       //?Deve essere una filiale già abilitata al lancio
053500160614         IF  %lookup(%editc(V01por:'X'):fil868) = 0 and
053600160614             %lookup('999':fil868) = 0;
053700160614           ErrGenerico = *on;
053800160614           ErrMessage  = *on;
053900160614           PosCurPOR   = *on;
054000160614           wmsg        = Msg(04);
054100160614           leavesr;
054200160614         ENDIF;
054300160421
054400160421       //?Non può essere una filiale di £6
054500160421         clear FNLV55DS;
054600160421         D55tpt = '6';
054700160421         D55lin = wpor;
054800160421         D55drf = V01data;
054900160421         fnlv55r (FNLV55DS);
055000160421         IF  D55tfa <> wpor;
055100160421           ErrGenerico = *on;
055200160421           ErrMessage  = *on;
055300160421           PosCurPOR   = *on;
055400160614           wmsg        = Msg(03);
055500160421           %subst(V01msg:25:3) = %editc(D55tfa:'X');
055600160421           leavesr;
055700160421         ENDIF;
055800160421
055900160421       //?Deve essere una filiale abilitata a TELLUS
056000160421       //?se NON EPD
056100160421         IF  not UtenteEDP and §OGtlls = *blanks;
056200160421           ErrGenerico = *on;
056300160421           ErrMessage  = *on;
056400160421           PosCurPOR   = *on;
056500160614           wmsg        = Msg(04);
056600160421         ENDIF;
056700160421
056800160421       //?Verifica se presente la data x consegne automatiche
056900160421         IF  §OGddao <> *blanks and §OGddao > *zeros and
057000160421             §OGddao <> '20391231';
057100160421           wdata = %dec(§OGddao:8:0);
057200160421         ENDIF;
057300160421
057400160421       //?Se prima Distinta era spento e ora invece lo devo accendere
057500160421       //?riemetto la videata
057600160421         IF  wdata = 0 or wdata > V01data;
057700160421           IF  not Distinta;
057800160421             Distinta = *on;
057900160421             Giro = *off;
058000160421             clear V01giro;
058100160421             ErrGenerico = *on;
058200160421           ENDIF;
058300160421         ELSE;
058400160421       //?Se prima Giro era spento e ora invece lo devo accendere
058500160421       //?riemetto la videata
058600160421           IF  not Giro;
058700160421             Distinta = *off;
058800160421             Giro = *on;
058900160421             V01giro = 'S';
059000160421             ErrGenerico = *on;
059100160421           ENDIF;
059200160421         ENDIF;
059300160421
059400160421       ENDSR;
059500160421
059600160421       //--------------------------------------------------------------
059700160421       //?Lancio elaborazione per giro di controllo.
059800160421       //--------------------------------------------------------------
059900160421       BEGSR GiroControllo;
060000160421
060100160421         clear FIORG01DS;
060200160421         IORG01por  = V01por;
060300160421         IORG01data = V01data;
060400160421         IORG01dist = V01dist;
060500160421         IORG01giro = V01giro;
060600160421         IORG01test = V01test;
060700160421         IORG01ctrl = 'S';
060800160421         IORG01nrit = §VPObotlls;
060900160421         kpjbu = FIORG01DS;
061000160421         FIORG01R (kpjba);
061100160421         FIORG01DS = kpjbu;
061200160421
061300160421       ENDSR;
061400160421
061500160421       //--------------------------------------------------------------
061600160421       //?Operazioni finali.
061700160421       //--------------------------------------------------------------
061800160421       BEGSR RoutEnd;
061900160421
062000160421         *inLR = *on;
062100160421         return;
062200160421
062300160421       ENDSR;
062400160421
062500160421      /end-free
062600160421
062700160421       //--------------------------------------------------------------
062800160421       //?Schiere a tempo di compilazione.
062900160421       //--------------------------------------------------------------
063000160421
063100160421** -- MSG -------------------------------------------------------------------*
063200160420Utente non abilitato alla funzione                                            01
063300160421Immettere una filiale ritiro valida o in gestione all'utente                  02
063400160421La filiale è gestita da xxx: effettuare il lancio con la filiale in gestione  03
063500160421Filiale non abilitata allo scambio dati con TELLUS !!!                        04
063600160421Presenti solo xxx ORM senza giro: attribuire Manualmente quelli senza giro    05
