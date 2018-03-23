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
011200160420
011300160420      // - Tabella LAT - Autorizzazioni
011400160420     d dLAT          e ds
011500160421
011600160421      // - Tabella VPO - Tellus
011700160421     d dVPOTELLUS    e ds
011800160421
011900160421      // ds per campo ORGDE7 di AZORG
012000160421     d OG147         e ds
012100160421
012200160421      // ds per campo ORGDE8 di AZORG
012300160421     d OG148         e ds
012400160420
012500160420      //---------------------------------------------------------------
012600160420      //?Definizione variabili globali.
012700160420      //---------------------------------------------------------------
012800160420      // - Flags booleani
012900160420     d ErrGrave        s               n   inz(*off)
013000160420     d Fine            s               n   inz(*off)
013100160420     d wEnd            s               n   inz(*off)
013200160420     d wInzD01         s               n   inz(*off)
013300160420
013400160420      // - Indici di schiera
013500160420     d xx              s              4s 0 inz
013600160420
013700160420      // - Campi associati al video
013800160420     d Video           s              2a   inz
013900160420
014000160420      // - Campi di comodo data
014100160420     d Data_EUR        s               d   datfmt(*eur)
014200160420     d Data_ISO        s               d   datfmt(*iso)
014300160420
014400160420      // - Campi di comodo
014500160420     d Oggi            s              8s 0 inz
014600160420     d wabi            s              2a   inz
014700160421     d wdata           s              8s 0 inz
014800160421     d wmsg            s                   like(V01msg) inz
014900160421     d wpor            s                   like(V01por) inz
015000160421     d w025a           s             25a   inz
015100160420
015200160420      //---------------------------------------------------------------
015300160421      //?Definizione Procedure usate.
015400160420      //---------------------------------------------------------------
015500160421      // - Sottomissione lavoro batch
015600160421     d bch10           pr                  extpgm('BCH10')
015700160421     d  kpjba                              likeds(KPJBA)
015800160421
015900160421       // - Scambio dati TELLUS
016000160421     d FIORG01R        pr                  extpgm('FIORG01R')
016100160421     d  kpjba                              likeds(KPJBA)
016200160421
016300160421       // - Controllo Terminal
016400160421     d FNLV55R         pr                  extpgm('FNLV55R')
016500160421     d  fnlv55ds                           likeds(fnlv55ds)
016600160421
016700160420       // - Caricamento Filiali in gestione
016800160420     d TRUL31R         pr                  extpgm('TRUL31R')
016900160420     d  kpjba                              likeds(kpjba)
017000160420     d  trul31ds                           likeds(trul31ds)
017100160420
017200160420      //---------------------------------------------------------------
017300160420      //?Definizione Prototipi.
017400160420      //---------------------------------------------------------------
017500160420      /copy gaitrasrc/srcprotopr,TIBS02R
017600160420      /copy gaitrasrc/srcprotopr,TIBS34R
017700160420
017800160420      //---------------------------------------------------------------
017900160420      //?Definizione key-list.
018000160420      //---------------------------------------------------------------
018100160420
018200160420      //---------------------------------------------------------------
018300160420      //?M A I N - L I N E
018400160420      //---------------------------------------------------------------
018500160420
018600160420     c     *Entry        plist
018700160420     c                   parm                    kpjba
018800160420
018900160420      /free
019000160420
019100160420       //?Operazioni iniziali
019200160420       exsr RoutInz;
019300160420
019400160420       //?Gestione video
019500160420       DOW  Fine = *off;
019600160420         SELECT;
019700160420         WHEN  Video = 'D1';
019800160420           exsr GesD01;
019900160420         OTHER;
020000160420           Fine = *on;
020100160420         ENDSL;
020200160420       ENDDO;
020300160420
020400160420       //?Operazioni finali
020500160420       exsr RoutEnd;
020600160420
020700160420       //--------------------------------------------------------------
020800160420       //?Operazioni iniziali.
020900160420       //--------------------------------------------------------------
021000160420       BEGSR RoutInz;
021100160420
021200160420       //?Reperimento dati job
021300160420         exsr DatiJob;
021400160420
021500160420       //?Impostazione campi "fissi"
021600160420         V01pgm = SDSpgm;
021700160420         Video = 'D1';
021800160420         wInzD01 = *on;
021900160420
022000160420       //?Imposto oggi
022100160420         Oggi = %dec(%date());
022200160420
022300160420       //?Se ho già un errore grave esco
022400160420         IF  ErrGrave;
022500160420           leavesr;
022600160420         ENDIF;
022700160420
022800160420       //?Carico le filiali abilitate all'utente
022900160420         clear TRUL31DS;
023000160420         I31abi = wabi;
023100160420         I31cdi = DUTdis;
023200160420         I31car = DUTare;
023300160420         I31cpo = DUTpou;
023400160420         TRUL31R (kpjba:trul31ds);
023500160420         IF  O31pog <= *zeros;
023600160420           ErrGrave = *on;
023700160420           leavesr;
023800160420         ENDIF;
023900160421
024000160428       //?Aggancio tabella VPO 'TELLUSORM'
024100160421         clear dVPOTELLUS;
024200160421         clear TIBS02DS;
024300160421         T02mod = 'C';
024400160421         T02sif = knsif;
024500160421         T02cod = 'VPO';
024600160428         T02ke1 = 'TELLUSORM';
024700160421         TNTBE_RicercaControllo (kpjba:tibs02ds);
024800160421         IF  T02err = *blanks;
024900160421           dVPOTELLUS = T02uni;
025000160421         ENDIF;
025100160420
025200160420       ENDSR;
025300160420
025400160420       //--------------------------------------------------------------
025500160420       //?Reperimento Dati del job (Utente/Operativi).
025600160420       //--------------------------------------------------------------
025700160420       BEGSR DatiJob;
025800160420
025900160420         in(E) §AzUte;
026000160420         IF  NOT %error;
026100160420           in(E) §DatiUte;
026200160420         ENDIF;
026300160420         IF  %error or RSut = *blanks;
026400160420           clear TIBS34ds;
026500160420           tibs34r(tibs34ds);
026600160420           in §AzUte;
026700160420           in §DatiUte;
026800160420         ENDIF;
026900160420
027000160420         clear wabi;
027100160420         clear dLAT;
027200160420
027300160420       //?Controllo autorizzazioni profilo
027400160420         SELECT;
027500160420         WHEN  DUTerr = 'E';
027600160420           ErrGrave = *on;
027700160420           leavesr;
027800160420         WHEN  UTEaut <> *blanks;
027900160420           wabi = UTEaut;
028000160420         OTHER;
028100160420           IF  DUTlpo = '1';
028200160420             wabi = 'TP';
028300160420           ENDIF;
028400160420           IF  DUTlpo = '2';
028500160420             wabi = 'PO';
028600160420           ENDIF;
028700160420         ENDSL;
028800160420
028900160420       //?Controllo se l'autorizzazione è valida
029000160421         clear dLAT;
029100160420         clear TIBS02DS;
029200160420         T02mod = 'C';
029300160420         T02sif = knsif;
029400160420         T02cod = 'LAT';
029500160420         T02ke1 = wabi;
029600160420         TNTBE_RicercaControllo (kpjba:tibs02ds);
029700160420         dLAT = T02uni;
029800160420         IF  T02err <> *blanks or §LATabi = 'S';
029900160420           ErrGrave = *on;
030000160420           leavesr;
030100160420         ENDIF;
030200160420
030300160420       ENDSR;
030400160420
030500160420       //--------------------------------------------------------------
030600160420       //?Gestione videata D01.
030700160420       //--------------------------------------------------------------
030800160420       BEGSR GesD01;
030900160420
031000160420       //?Inizializzazione videata
031100160420         IF  wInzD01;
031200160420           exsr InzD01;
031300160420           wInzD01 = *off;
031400160420         ENDIF;
031500160420
031600160420       //?Se errore grave emetto messaggio poi esco
031700160420         IF  ErrGrave;
031800160420           ErrMessage  = *on;
031900160420           ErrGenerico = *on;
032000160420           V01msg = Msg(01);
032100160420         ENDIF;
032200160420
032300160420       //?Emissione Testata
032400160421         write ORG00T01;
032500160420
032600160420       //?Emissione videata
032700160421         exfmt ORG00D01;
032800160420         reset ErrMessage;
032900160420         reset ErrGenerico;
033000160420         clear V01msg;
033100160420
033200160420         SELECT;
033300160420
033400160420       //?- Errore Grave esco
033500160420         WHEN  ErrGrave;
033600160420           Fine = *on;
033700160420
033800160420       //?- F03=Fine
033900160420         WHEN  dsp_aid = c_F03;
034000160420           exsr F03D01;
034100160420
034200160420       //?- F06=Conferma
034300160420         WHEN  dsp_aid = c_F06;
034400160420           exsr F06D01;
034500160421           IF  ErrGenerico;
034600160421             leavesr;
034700160421           ENDIF;
034800160420
034900160420       //?Invio
035000160420         OTHER;
035100160420           exsr ContrD01;
035200160420           IF  ErrGenerico;
035300160420             leavesr;
035400160420           ENDIF;
035500160420
035600160420         ENDSL;
035700160420
035800160420       ENDSR;
035900160420
036000160420       //--------------------------------------------------------------
036100160420       //?Inizializzazione Videata D01.
036200160420       //--------------------------------------------------------------
036300160420       BEGSR InzD01;
036400160420
036500160420       //?Pulizia videata
036600160421         clear ORG00D01;
036700160420         UtenteEDP = *off;
036800160420         Distinta = *off;
036900160420         Giro = *off;
037000160420
037100160420
037200160420       //?Utente EDP
037300160420         UtenteEDP = %subst(knmus:1:3) = 'EDP';
037400160421         IF  UtenteEDP;
037500160421           Distinta = *on;
037600160421           Giro     = *on;
037700160421         ENDIF;
037800160420
037900160420       //?Imposto la filiale ritiro
038000160420         V01por  = DUTpou;
038100160420         V01pord = DUTdpo;
038200160421
038300160421       //?Dati filiale utente
038400160421         clear wdata;
038500160421         clear OG147;
038600160421         chain (V01por) AZORG01L;
038700160421         IF  %found(AZORG01L);
038800160421           OG147 = ORGde7;
038900160421         ENDIF;
039000160421         IF  §OGddao <> *blanks and §OGddao > *zeros and
039100160421             §OGddao <> '20391231';
039200160421           wdata = %dec(§OGddao:8:0);
039300160421         ENDIF;
039400160421         IF  wdata = 0 or wdata > Oggi;
039500160421           Distinta = *on;
039600160421         ELSE;
039700160421           Giro = *on;
039800160421           V01giro = 'S';
039900160421         ENDIF;
040000160421
040100160421       //?Imposto dati per parte EDP
040200160421         V01data = Oggi;
040300160421         V01ritmax = §VPObomax;
040400160420
040500160420       ENDSR;
040600160420
040700160420       //--------------------------------------------------------------
040800160421       //?Gestione tasto funzionale F03 da videata D01
040900160420       //?F03=Fine
041000160420       //--------------------------------------------------------------
041100160421       BEGSR F03D01;
041200160420
041300160420       //?Chiusura del programma
041400160420         Fine = *on;
041500160420
041600160420       ENDSR;
041700160420
041800160420       //--------------------------------------------------------------
041900160421       //?Gestione tasto funzionale F06 da videata D01
042000160421       //?F06=Conferma
042100160420       //--------------------------------------------------------------
042200160421       BEGSR F06D01;
042300160420
042400160421       //?Se ho un nr. minimo di ritiri da elaborare
042500160421       //?faccio prima un giro di controllo
042600160421         IF  §VPObotlls > 0;
042700160421           exsr GiroControllo;
042800160421           IF  OORG01min  = 'S';
042900160421             ErrGenerico = *on;
043000160421             ErrMessage  = *on;
043100160421             PosCurPOR   = *on;
043200160421             V01msg      = Msg(05);
043300160421             %subst(V01msg:15:3) = %editc(OORG01nrit:'Z');
043400160421             leavesr;
043500160421           ENDIF;
043600160421         ENDIF;
043700160421
043800160421       //?Elaborazione definitiva
043900160421         clear FIORG01DS;
044000160421         IORG01por  = V01por;
044100160421         IORG01data = V01data;
044200160421         IORG01dist = V01dist;
044300160421         IORG01giro = V01giro;
044400160421         IORG01test = V01test;
044500160421         kpjbu = FIORG01DS;
044600160421         kcoaz = 'ORG1';
044700160421         clear kbuff;
044800160421         BCH10(kpjba);
044900160420
045000160420       ENDSR;
045100160420
045200160420       //--------------------------------------------------------------
045300160421       //?Controlla Videata D01.
045400160420       //--------------------------------------------------------------
045500160421       BEGSR ContrD01;
045600160420
045700160420         WindDspF = IndDspF;
045800160420         reset WindDspFb;
045900160420         IndDspF  = WindDspF;
046000160420
046100160420       //?Filiale Ritiro
046200160421         wpor = V01por;
046300160420         exsr FilialeRitiro;
046400160421         V01por  = wpor;
046500160421         V01pord = w025a;
046600160421         V01msg = wmsg;
046700160420         IF  ErrGenerico;
046800160420           leavesr;
046900160420         ENDIF;
047000160421
047100160421       ENDSR;
047200160421
047300160421       //--------------------------------------------------------------
047400160421       //?Controllo Filiale Ritiro;
047500160421       //--------------------------------------------------------------
047600160421       BEGSR FilialeRitiro;
047700160421
047800160421         IF  wpor = *zeros;
047900160421           ErrGenerico = *on;
048000160421           ErrMessage  = *on;
048100160421           PosCurPOR   = *on;
048200160421           wmsg        = Msg(02);
048300160421           leavesr;
048400160421         ENDIF;
048500160421
048600160421       //?Deve esistere la Filiale
048700160421         clear wdata;
048800160421         clear OG147;
048900160421         clear OG148;
049000160421         chain (V01por) AZORG01L;
049100160421         IF  not %found(AZORG01L) or ORGfva <> *blanks;
049200160421           ErrGenerico = *on;
049300160421           ErrMessage  = *on;
049400160421           PosCurPOR   = *on;
049500160421           wmsg        = Msg(02);
049600160421           leavesr;
049700160421         ENDIF;
049800160421         OG147 = ORGde7;
049900160421         OG148 = ORGde8;
050000160421
050100160421       //?Decodifico filiale
050200160421         w025a = ORGdes;
050300160421
050400160421       //?Filiale in gestione all'utente
050500160421       //?se NON EPD
050600160421         IF  %lookup(%editc(V01por:'X'):pog) = 0 and not UtenteEDP;
050700160421           ErrGenerico = *on;
050800160421           ErrMessage  = *on;
050900160421           PosCurPOR   = *on;
051000160421           V01msg      = Msg(02);
051100160421           leavesr;
051200160421         ENDIF;
051300160421
051400160421       //?Non può essere una filiale di £6
051500160421         clear FNLV55DS;
051600160421         D55tpt = '6';
051700160421         D55lin = wpor;
051800160421         D55drf = V01data;
051900160421         fnlv55r (FNLV55DS);
052000160421         IF  D55tfa <> wpor;
052100160421           ErrGenerico = *on;
052200160421           ErrMessage  = *on;
052300160421           PosCurPOR   = *on;
052400160421           V01msg      = Msg(03);
052500160421           %subst(V01msg:25:3) = %editc(D55tfa:'X');
052600160421           leavesr;
052700160421         ENDIF;
052800160421
052900160421       //?Deve essere una filiale abilitata a TELLUS
053000160421       //?se NON EPD
053100160421         IF  not UtenteEDP and §OGtlls = *blanks;
053200160421           ErrGenerico = *on;
053300160421           ErrMessage  = *on;
053400160421           PosCurPOR   = *on;
053500160421           V01msg      = Msg(04);
053600160421         ENDIF;
053700160421
053800160421       //?Verifica se presente la data x consegne automatiche
053900160421         IF  §OGddao <> *blanks and §OGddao > *zeros and
054000160421             §OGddao <> '20391231';
054100160421           wdata = %dec(§OGddao:8:0);
054200160421         ENDIF;
054300160421
054400160421       //?Se prima Distinta era spento e ora invece lo devo accendere
054500160421       //?riemetto la videata
054600160421         IF  wdata = 0 or wdata > V01data;
054700160421           IF  not Distinta;
054800160421             Distinta = *on;
054900160421             Giro = *off;
055000160421             clear V01giro;
055100160421             ErrGenerico = *on;
055200160421           ENDIF;
055300160421         ELSE;
055400160421       //?Se prima Giro era spento e ora invece lo devo accendere
055500160421       //?riemetto la videata
055600160421           IF  not Giro;
055700160421             Distinta = *off;
055800160421             Giro = *on;
055900160421             V01giro = 'S';
056000160421             ErrGenerico = *on;
056100160421           ENDIF;
056200160421         ENDIF;
056300160421
056400160421       ENDSR;
056500160421
056600160421       //--------------------------------------------------------------
056700160421       //?Lancio elaborazione per giro di controllo.
056800160421       //--------------------------------------------------------------
056900160421       BEGSR GiroControllo;
057000160421
057100160421         clear FIORG01DS;
057200160421         IORG01por  = V01por;
057300160421         IORG01data = V01data;
057400160421         IORG01dist = V01dist;
057500160421         IORG01giro = V01giro;
057600160421         IORG01test = V01test;
057700160421         IORG01ctrl = 'S';
057800160421         IORG01nrit = §VPObotlls;
057900160421         kpjbu = FIORG01DS;
058000160421         FIORG01R (kpjba);
058100160421         FIORG01DS = kpjbu;
058200160421
058300160421       ENDSR;
058400160421
058500160421       //--------------------------------------------------------------
058600160421       //?Operazioni finali.
058700160421       //--------------------------------------------------------------
058800160421       BEGSR RoutEnd;
058900160421
059000160421         *inLR = *on;
059100160421         return;
059200160421
059300160421       ENDSR;
059400160421
059500160421      /end-free
059600160421
059700160421       //--------------------------------------------------------------
059800160421       //?Schiere a tempo di compilazione.
059900160421       //--------------------------------------------------------------
060000160421
060100160421** -- MSG -------------------------------------------------------------------*
060200160420Utente non abilitato alla funzione                                            01
060300160421Immettere una filiale ritiro valida o in gestione all'utente                  02
060400160421La filiale è gestita da xxx: effettuare il lancio con la filiale in gestione  03
060500160421Filiale non abilitata allo scambio dati con TELLUS !!!                        04
060600160421Presenti solo xxx ORM senza giro: attribuire Manualmente quelli senza giro    05
