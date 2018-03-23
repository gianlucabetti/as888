000100090520       //--------------------------------------------------------------
000200160929       //?TNTBACA1R * Selezione tab. "ACA" = Causali Attività            ?
000300090520       //--------------------------------------------------------------
000400090520     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500090520
000600090520       //--------------------------------------------------------------
000700090520       //?Dichiarazione file.                                          ?
000800090520       //--------------------------------------------------------------
000900090520       // - Tabelle
001000100309     fTNTBE01L  if   e           k disk
001100090520
001200090520       // - Video
001300160929     fTNTACA1D  cf   e             workstn
001400160929     f                                     sfile(TBACA1S01:S01nrr)
001500090520     f                                     indds(IndDspF)
001600090520     f                                     infds(InfDspF)
001700090520
001800090520       //--------------------------------------------------------------
001900090520       //?Definizione costanti.                                        ?
002000090520       //--------------------------------------------------------------
002100090520       // - Codice tabella in gestione
002200160929     d c_Tab           c                   const('ACA')
002300090520
002400090520       // - Numero di record in ciascuna videata di subfile
002500090520     d c_SflPag        c                   const(16)
002600090520
002700090520       // - Numero massimo di record gestibili
002800110331     d c_MaxRec        c                   const(9999)
002900090520
003000090520       // - Tasti funzionali a video
003100090520     d c_F01           c                   const(x'31')
003200090520     d c_F02           c                   const(x'32')
003300090520     d c_F03           c                   const(x'33')
003400100504     d c_F04           c                   const(x'34')
003500090520     d c_F05           c                   const(x'35')
003600090520     d c_F06           c                   const(x'36')
003700090520     d c_F07           c                   const(x'37')
003800090520     d c_F08           c                   const(x'38')
003900090520     d c_F09           c                   const(x'39')
004000090520     d c_F10           c                   const(x'3A')
004100100504     d c_F11           c                   const(x'3B')
004200090520     d c_F12           c                   const(x'3C')
004300090520     d c_F13           c                   const(x'B1')
004400090520     d c_F14           c                   const(x'B2')
004500090520     d c_F15           c                   const(x'B3')
004600090520     d c_F16           c                   const(x'B4')
004700090520     d c_F17           c                   const(x'B5')
004800090520     d c_F18           c                   const(x'B6')
004900090520     d c_F19           c                   const(x'B7')
005000090520     d c_F20           c                   const(x'B8')
005100090520     d c_F21           c                   const(x'B9')
005200090520     d c_F22           c                   const(x'BA')
005300090520     d c_F23           c                   const(x'BB')
005400090520     d c_F24           c                   const(x'BC')
005500090520     d c_Enter         c                   const(x'F1')
005600090520     d c_RollDown      c                   const(x'F4')
005700090520     d c_RollUp        c                   const(x'F5')
005800090520
005900090520       //--------------------------------------------------------------
006000090520       //?Definizione schiere.                                         ?
006100090520       //--------------------------------------------------------------
006200090520       // - Messaggi di errore
006300160929     d Msg             s             78    dim( 2)  ctdata  perrcd(1)
006400090520
006500090520       //--------------------------------------------------------------
006600090520       //?Definizione aree dati.                                       ?
006700090520       //--------------------------------------------------------------
006800090520       // - Dati utente
006900090520     d §AzUte        e ds                  extname(AZUTE00F)
007000090520     d                                     dtaara
007100090520     d §DatiUte      e ds                  extname(dDatiUte)
007200090520     d                                     dtaara
007300090520
007400090520       //--------------------------------------------------------------
007500090520       //?Definizione strutture dati.                                  ?
007600090520       //--------------------------------------------------------------
007700090520       // - Status ds
007800090520     d Status         sds
007900090520     d  SDSpgm           *proc
008000090521     d  SDSprm           *parms
008100090520
008200090520       // - InfDS
008300090520     d InfDspF         ds
008400160929     d  dsp_aid              369    369a
008500160929     d  sfl_rrn              376    377i 0
008600160929     d  min_nrr              378    379i 0
008700160929     d  num_rcds             380    381i 0
008800090520
008900090520       // - Indicatori su DspF
009000090520     d IndDspF         ds                  inz
009100160929       // - Abilitazione tasti funzionali
009200160929     d  F3Attivo                       n   overlay(IndDspF : 03)
009300160929     d  F6Attivo                       n   overlay(IndDspF : 06)
009400160929     d  F12Attivo                      n   overlay(IndDspF : 12)
009500160929       // - Emissione messaggio di errore
009600160929     d  ErrMessage                     n   overlay(IndDspF : 28)
009700160929       // - Indicatori di gestione del subfile
009800160929     d  SflDsp                         n   overlay(IndDspF : 30)
009900160929     d  SflDspCtl                      n   overlay(IndDspF : 31)
010000160929     d  SflNxtChg                      n   overlay(IndDspF : 32)
010100160929     d  SflEnd                         n   overlay(IndDspF : 33)
010200160929       // - Indicatori per Attibuti di visualizzazione
010300160929       // - Posizionamento cursore & segnalazione errore
010400160929     d  PosCurOPZ                      n   overlay(IndDspF : 50)
010500160929       //   - Riemissione videata
010600160929     d  ErrGenerico                    n   overlay(IndDspF : 99)
010700110429
010800090520       // - Parametri ricevuti
010900090520     d KPJBA         e ds
011000160929     d TNTACA1DS     e ds                  inz
011100090520
011200090520       // - Parametri per Reperimento dati utente
011300160929     d TIBS34DS      e ds                  inz
011400090520
011500160929       // - Tab. ACA = Causali Attività Autisti
011600160929     d dACA          e ds                  inz
011700090520
011800110405      // - Ricerca/Controllo tabelle
011900160929     d TIBS02DS      e ds                  inz
012000110405
012100090520       //--------------------------------------------------------------
012200090520       //?Definizione variabili globali.                               ?
012300090520       //--------------------------------------------------------------
012400090520       // - Flags booleani
012500160929     d Fine            s               n   inz
012600160929     d RecOK           s               n   inz
012700160929     d wInzS01         s               n   inz(*on)
012800160929     d wCalled         s               n   inz
012900090521
013000090521       // - Parametri di input facoltativi
013100160929     d p_TNTBACA1DS    s                   like(TNTBACA1DS)
013200090520
013300090520       // - Variabili per la gestione del video
013400160929     d Video           s              2    inz('S1')
013500090520     d W_SflPag        s              3  0 inz
013600090520     d wPag            s              4  0 inz
013700090520     d wRig            s              3  0 inz
013800090521     d S01nrr          s                   like(C1RcdNbr) inz
013900090521     d SavS01csr       s                   like(C1RcdNbr) inz
014000090520
014100090520       //--------------------------------------------------------------
014200090520       //?Definizione prototipi procedure.                             ?
014300090520       //--------------------------------------------------------------
014400090520       // - Reperimento dati utente
014500090520      /copy gaitrasrc/srcProtoPR,TIBS34R
014600110405
014700110405      /copy gaitrasrc/srcprotopr,tibs02r
014800090520
014900090520       //--------------------------------------------------------------
015000090520       //?Definizione key-list.                                        ?
015100090520       //--------------------------------------------------------------
015200090520       // - File TNTBE01L
015300090520     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
015400090520     d                                     prefix(k_)   inz
015500090520
015600090520       //--------------------------------------------------------------
015700090520       //?Riepilogo indicatori utilizzati.                             ?
015800090520       //--------------------------------------------------------------
015900160929
016000090520       //--------------------------------------------------------------
016100090520
016200090520       //--------------------------------------------------------------
016300090520       //?M A I N - L I N E                                            ?
016400090520       //--------------------------------------------------------------
016500160929     c     *ENTRY        plist
016600090520     c                   parm                    KPJBA
016700160929     c                   parm                    p_TNTBACA1DS
016800090520
016900090520      /free
017000090520
017100160929       // Operazioni iniziali?
017200160929       exsr RoutInz;
017300090520
017400160929       // Ciclo di gestione del file video?
017500160929       DOW  Fine = *off;
017600160929         SELECT;
017700160929         // Gestione videata S1 (subfile)
017800160929         WHEN  Video = 'S1';
017900160929           exsr GesS01;
018000160929         OTHER;
018100160929           Fine = *on;
018200160929         ENDSL;
018300160929       ENDDO;
018400090520
018500160929       // Operazioni finali?
018600160929       exsr RoutEnd;
018700090521
018800090520       //--------------------------------------------------------------
018900090520       //?Operazioni iniziali.                                         ?
019000090520       //--------------------------------------------------------------
019100160929       BEGSR RoutInz;
019200160929
019300160929       // Reperimento dati job?
019400160929         exsr DatiJob;
019500160929
019600160929       // Impostazione nome programma a video?
019700160929         V1Tpgm = SDSpgm;
019800090520
019900160929       // Verifica se ricevuti parametri?
020000160929         IF  SDSprm > 1;
020100160929           TNTBACA1ds = p_TNTBAC1DS;
020200160929           wCalled = *on;
020300160929         ELSE;
020400160929           clear TNTBACA1DS;
020500160929           wCalled = *off;
020600160929         ENDIF;
020700090520
020800160929       // Impostazione chiusura?
020900160929         SELECT;
021000160929         WHEN  wCalled and IACAtla = *blank;
021100160929           *inRT = *on;
021200160929         WHEN  wCalled and IACAtla = 'L';
021300160929           *inLR = *on;
021400160929         WHEN  wCalled and IACAtla = 'C';
021500160929           *inLR = *on;
021600160929           Fine = *on;
021700160929           leavesr;
021800160929         OTHER;
021900160929           *inLR = *on;
022000160929         ENDSL;
022100090520
022200160929       // Reperimento dati job?
022300160929         exsr DatiJob;
022400090520
022500160929       // Impostazione nome programma a video?
022600090520         V1Tpgm = SDSpgm;
022700090520
022800160929       // Impostazione iniziale / pulizia dei campi chiave?
022900090520         k_TBEcod = c_Tab;
023000160929         clear k_TBEke1;
023100160929         clear k_TBEke2;
023200160929         clear k_TBElin;
023300160929         clear k_TBEsif;
023400090521
023500160929       // Impostazione iniziale parametri di output?
023600160929         clear OACAke1;
023700160929         clear OACAuni;
023800160929         clear OACAfxx;
023900160929         OACAerr = *off;
024000160929         clear OACAmsg;
024100090521
024200160929       // Re-impostazione flags booleani (fosse mai il 2° richiamo)?
024300160929         reset Fine;
024400160929         reset Video;
024500160929         reset wInzS01;
024600110405
024700160929       // verifico se passata una causale se si trattta di una causle su cliente che non spedisce
024800160929         IF  IACAcau <> '  ' ;
024900160929           clear dACA;
025000160929           clear TIBS02ds;
025100160929           T02mod = 'C';
025200160929           T02cod = 'ACA';
025300160929           T02ke1 = IACAaca  ;
025400160929           T02sif = KNSIF;
025500160929           TNTBE_RicercaControllo  (kpjba : tibs02ds);
025600160929           IF  T02err = *blanks;
025700160929             dACA = t02uni ;
025800160929           ENDIF;
025900160929         ENDIF ;
026000090520
026100090520       ENDSR;
026200160929
026300160929       //--------------------------------------------------------------
026400160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
026500160929       //--------------------------------------------------------------
026600160929       BEGSR DatiJob;
026700160929
026800160929         in(e) §AzUte;
026900160929         IF  not %error;
027000160929           in(e) §DatiUte;
027100160929         ENDIF;
027200160929         IF  %error or RSut = *blank;
027300160929           tibs34r ( tibs34ds );
027400160929           in §AzUte;
027500160929           in §DatiUte;
027600160929         ENDIF;
027700160929
027800160929       ENDSR;
027900090520
028000090520       //--------------------------------------------------------------
028100090520       //?Gestione subfile S01.                                        ?
028200090520       //--------------------------------------------------------------
028300160929       BEGSR GesS01;
028400090520
028500160929       // Inizializzazione videata?
028600160929         IF  wInzS01;
028700160929           exsr InzS01;
028800160929           wInzS01 = *off;
028900160929         ENDIF;
029000090520
029100160929       // Emissione Testata & Piede?
029200160929         write TBACA1T01;
029300160929         write TBACA1P01;
029400090520
029500160929       // Posizionamento cursore?
029600160929         IF  C1CsrRrn > *zero;
029700090520           C1RcdNbr = C1CsrRrn;
029800160929         ELSE;
029900090520           C1RcdNbr = 1;
030000160929           write TBACA1S00;
030100160929         ENDIF;
030200090520
030300160929       // Emissione videata?
030400160929         exfmt TBACA1C01;
030500090520
030600160929         reset ErrMessage;
030700160929         reset ErrGenerico;
030800160929         clear V1Dmsg;
030900090520         SELECT;
031000090520
031100160929         // F3=Fine?
031200160929         WHEN  dsp_aid = c_F03;
031300160929           OACAfxx = '1';
031400160929           Fine = *on;
031500090520
031600160929         // F5=Rivisualizza?
031700160929         WHEN  dsp_aid = c_F05;
031800160929           wInzS01 = *on;
031900090520
032000160929         // F12=Ritorno?
032100160929         WHEN  dsp_aid = c_F12;
032200160929           IACAfxx = '2';
032300160929           Fine = *on;
032400090520
032500160929         // Roll-Up?
032600160929         WHEN  dsp_aid = c_RollUp;
032700160929           exsr RollUpS01;
032800090520
032900160929         // SubFile vuoto?
033000160929         WHEN  S01nrr = *zero;
033100090520
033200160929         //?Invio?
033300160929         OTHER;
033400160929           exsr OpzS01;
033500090520
033600090520         ENDSL;
033700090520
033800090520       ENDSR;
033900090520
034000090520       //--------------------------------------------------------------
034100090520       //?Inizializzazione subfile S01.                                ?
034200090520       //--------------------------------------------------------------
034300160929       BEGSR InzS01;
034400090520
034500160929       // Spegnimento degli indicatori di errore?
034600090520         %subst(IndDspF : 50) = *off;
034700090520
034800160929       // Settaggio indicatori dei tasti funzionali?
034900160929         exsr SetIndP01;
035000090520
035100160929       // Pulizia subfile?
035200160929         SflDsp    = *on;
035300160929         SflDspCtl = *on;
035400160929         write TBACA101;
035500160929         SflDspCtl = *off;
035600160929         SflEnd    = *off;
035700090520
035800160929         clear W_SflPag;
035900160929         clear C1RcdNbr;
036000160929         clear C1CsrRrn;
036100160929         clear S01nrr;
036200160929         clear V1Dmsg;
036300160929         ErrMessage  = *off;
036400090520         ErrGenerico = *off;
036500090520
036600160929       // Caricamento della 1ª pagina di dati nel subfile?
036700160929         clear k05tntbe01;
036800090520         k_TBEcod = c_Tab;
036900160929         setll %kds(k05tntbe01) TNTBE01L;
037000160929         reade %kds(k05tntbe01:1) TNTBE01L;
037100160929         exsr RollUpS01;
037200090520
037300160929       // Impaginazione subfile?
037400160929       // -> forza l'impaginazione sul 1° rec. del subfile
037500160929         IF  S01nrr > *zero;
037600160929           C1RcdNbr = 1;
037700160929           C1CsrRrn = 1;
037800160929         ELSE;
037900090520           clear C1RcdNbr;
038000160929         ENDIF;
038100090520
038200090520       ENDSR;
038300090520
038400090520       //--------------------------------------------------------------
038500090520       //?Caricamento singola pagina nel subfile S01                   ?
038600090520       //--------------------------------------------------------------
038700160929       BEGSR RollUpS01;
038800090520
038900090520         S01nrr    = (W_SflPag * c_SflPag);
039000090520         W_SflPag += 1;
039100090520         SflNxtChg = *off;
039200090520
039300160929       // Ciclo di caricamento dati da tab. "CCO"?
039400160929         DOW  S01nrr    < (W_SflPag * c_SflPag)   and
039500090520              S01nrr    < c_MaxRec                and
039600090520              Not %eof(TNTBE01L);
039700090520
039800160929         // Selezione singolo record file TNTBE00F?
039900160929           exsr SelezRec;
040000090520
040100160929         // Registrazione singolo record nel subfile?
040200160929           IF  RecOK;
040300090520
040400160929             clear TBACA1S01;
040500160929             dACA = TBEuni;
040600090520             S01cod = TBEke1;
040700160929             S01des = §ACAdes;
040800160929             S01ata = §ACAata;
040900160929             S01uti = §ACAuti;
041000160929             S01ann = §ACAann;
041100090520
041200090520             S01nrr += 1;
041300160929             write TBACA1S01;
041400090520
041500160929           ENDIF;
041600090520
041700160929         // Lettura record successivo?
041800160929           reade %kds(k05tntbe01:1) TNTBE01L;
041900090520
042000160929         ENDDO;
042100090520
042200160929       // Visualizzazione del SFL (se ci sono dati)?
042300160929         SflDsp = (S01nrr <= *zero);
042400090520
042500160929       // Attivazione del SFLEND?
042600160929         SflEnd = (S01nrr >= c_MaxRec   or   %eof(TNTBE01L));
042700090520
042800160929       // Posizionamento cursore al 1° rcd della pagina?
042900160929         IF  S01nrr > *zero;
043000090520           wPag     = S01nrr / c_SflPag;
043100090520           wRig     = S01nrr - (c_SflPag * wPag);
043200090520           C1RcdNbr = wPag * c_SflPag;
043300160929           IF  wRig > *zero;
043400090520             C1RcdNbr += 1;
043500160929           ELSE;
043600090520             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
043700160929           ENDIF;
043800160929         ELSE;
043900160929           clear C1RcdNbr;
044000160929         ENDIF;
044100090520
044200090520         C1CsrRrn = C1RcdNbr;
044300090520
044400090520       ENDSR;
044500090520
044600090520       //--------------------------------------------------------------
044700090520       //?Settaggio indicatori utilizzati in C01/P01.                  ?
044800090520       //--------------------------------------------------------------
044900160929       BEGSR SetIndP01;
045000090520
045100160929       // F3=Fine?
045200160929         F3Attivo = (not wCalled);
045300090520
045400160929       //?F12=Ritorno?
045500160929         F12Attivo = (wCalled);
045600090520
045700090520       ENDSR;
045800090520
045900090520       //--------------------------------------------------------------
046000090520       //?Selezione del singolo record letto dalla tabella "CCO".      ?
046100090520       //--------------------------------------------------------------
046200160929       BEGSR SelezRec;
046300090520
046400160929         reset RecOK;
046500090520
046600160929         dACA = TBEuni;
046700090520
046800160929         SELECT;
046900090520
047000160929       // Omissione records annullati?
047100160929         WHEN  TBEatb <> *blank;
047200160929           leavesr;
047300100421
047400160929       // Selezione tipo attività associata?
047500160929         WHEN  IACAata <> *blanks and IACAata <> §ACAata;
047600160929           leavesr;
047700091218
047800160929       // Selezione utilizzo causale attività?
047900160929       // S=Sì seleziono solo i rcd a blank
048000160929         WHEN  IACAuti = 'S' and §ACAuti <> *blanks;
048100160929           leavesr;
048200091218
048300160929       // Selezione utilizzo causale attività?
048400160929       //   N=No escludo i rcd a blank
048500160929         WHEN  IACAuti = 'N' and §ACAuti = *blanks;
048600160929           leavesr;
048700160929         ENDSL;
048800090520
048900160929         RecOK = *on;
049000090520
049100090520       ENDSR;
049200090520
049300090520       //--------------------------------------------------------------
049400090520       //?Gestione opzioni subfile S01.                                ?
049500090520       //--------------------------------------------------------------
049600160929       BEGSR OpzS01;
049700090520
049800160929         clear SavS01csr;
049900090520
050000160929       // Ciclo di lettura subfile?
050100160929         readc TBACA1S01;
050200090520
050300160929         DOW  not %eof(TNTBACA1D);
050400090520
050500090520           SflNxtChg = *off;
050600090520           %subst(IndDspF : 50) = *off;
050700090520           C1RcdNbr = S01nrr;
050800090520
050900160929           SELECT;
051000090520
051100160929         // Opz. 1=Selezione?
051200160929           WHEN  S01opz = '1' and not Fine;
051300160929             clear dACA;
051400160929             IACAke1 = S01cod;
051500160929             Fine = *on;
051600090520
051700160929         // Opz. 1=Selezione già reperita?
051800160929         // (ammessa una sola selezione)?
051900160929           WHEN  S01opz = '1' and Fine;
052000160929             ErrMessage  = *on;
052100160929             ErrGenerico = *on;
052200160929             PosCurOPZ   = *on;
052300160929             V1Dmsg = Msg(02);
052400160929             Fine  = *off;
052500090520
052600160929         // Nessuna opzione?
052700160929           WHEN  S01opz = *blank;
052800090520
052900160929         // Opzione errata?
053000160929           OTHER;
053100160929             ErrMessage  = *on;
053200160929             ErrGenerico = *on;
053300160929             PosCurOPZ   = *on;
053400160929             V1Dmsg = Msg(01);
053500160929             Fine  = *off;
053600090520
053700160929           ENDSL;
053800090520
053900160929         // Memorizzazione nrr del sfl con la 1ª opzione per?
054000160929         // riposizionarvici il cursore dopo il tasto "Invio"?
054100160929           IF  S01opz <> *blanks and
054200160929              (SavS01csr = *zeros or SavS01csr < C1RcdNbr);
054300160929             SavS01csr = C1RcdNbr;
054400160929           ENDIF;
054500090520
054600160929         // Aggiornamento sfl?
054700160929           SELECT;
054800160929           WHEN  ErrMessage;
054900160929             SflNxtChg = *on;
055000160929             C1CsrRrn  = C1RcdNbr;
055100160929           WHEN  ErrGenerico;
055200160929             C1CsrRrn  = C1RcdNbr;
055300160929             IF  S01opz <> '1';
055400160929               clear S01opz;
055500160929             ENDIF;
055600160929           OTHER;
055700160929             IF  S01opz <> '1';
055800160929               clear  S01opz;
055900160929             ENDIF;
056000160929           ENDSL;
056100090520
056200160929           IF  S01opz <> *blank;
056300090520             SflNxtChg = *on;
056400160929           ENDIF;
056500090520
056600160929           UPDATE TBACA1S01;
056700090520
056800160929           IF  ErrGenerico or ErrMessage;
056900090520             leave;
057000160929           ENDIF;
057100090520
057200160929           readc TBACA1S01;
057300090520
057400090520         ENDDO;
057500090520
057600160929       // Riposiziono il cursore sul record contenente la prima opz.?
057700160929       // (se non sono stati rilevati errori)?
057800160929         IF  not ErrMessage and not ErrGenerico and
057900090520             SavS01csr > *zero;
058000090520           C1CsrRrn = SavS01csr;
058100160929         ENDIF;
058200090520
058300090520       ENDSR;
058400090520
058500090520       //--------------------------------------------------------------
058600090520       //?Operazioni finali.                                           ?
058700090520       //--------------------------------------------------------------
058800160929       BEGSR RoutEnd;
058900090521
059000160929       // Ritorno parametri?
059100160929         IF  SDSprm > 1;
059200160929           p_TNTBACA1DS = TNTBACA1DS;
059300160929         ENDIF;
059400090520
059500160929       // "Chiusura" pgm?
059600090520         return;
059700090520
059800090520       ENDSR;
059900090520
060000090520      /end-free
060100090520
060200090520       //--------------------------------------------------------------
060300090520       //?Schiere a tempo di compilazione.                             ?
060400090520       //--------------------------------------------------------------
060500090520
060600160929** -  Msg -------------------------------------------------------------------*
060700090520Opzione errata                                                                 1
060800090520Ammessa la selezione di una sola causale                                       2
