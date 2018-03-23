000100090520       //--------------------------------------------------------------
000200161212       //?TNTBACAR1 * Selezione tab. "ACA" = Causali Assenza             ?
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
001300160930     fTNTBACAD1 cf   e             workstn
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
011000160930     d TNTBACADS     e ds                  inz
011100090520
011200090520       // - Parametri per Reperimento dati utente
011300160929     d TIBS34DS      e ds                  inz
011400090520
011500161212       // - Tab. ACA = Causali Assenza Autisti
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
013100160930     d p_TNTBACADS     s                   like(TNTBACADS)
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
016700160930     c                   parm                    p_TNTBACADS
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
020100160930           TNTBACADS = p_TNTBACADS;
020200160929           wCalled = *on;
020300160929         ELSE;
020400160930           clear TNTBACADS;
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
024600090520
024700090520       ENDSR;
024800160929
024900160929       //--------------------------------------------------------------
025000160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
025100160929       //--------------------------------------------------------------
025200160929       BEGSR DatiJob;
025300160929
025400160929         in(e) §AzUte;
025500160929         IF  not %error;
025600160929           in(e) §DatiUte;
025700160929         ENDIF;
025800160929         IF  %error or RSut = *blank;
025900160929           tibs34r ( tibs34ds );
026000160929           in §AzUte;
026100160929           in §DatiUte;
026200160929         ENDIF;
026300160929
026400160929       ENDSR;
026500090520
026600090520       //--------------------------------------------------------------
026700090520       //?Gestione subfile S01.                                        ?
026800090520       //--------------------------------------------------------------
026900160929       BEGSR GesS01;
027000090520
027100160929       // Inizializzazione videata?
027200160929         IF  wInzS01;
027300160929           exsr InzS01;
027400160929           wInzS01 = *off;
027500160929         ENDIF;
027600090520
027700160929       // Emissione Testata & Piede?
027800160929         write TBACA1T01;
027900160929         write TBACA1P01;
028000090520
028100160929       // Posizionamento cursore?
028200160929         IF  C1CsrRrn > *zero;
028300090520           C1RcdNbr = C1CsrRrn;
028400160929         ELSE;
028500090520           C1RcdNbr = 1;
028600160929           write TBACA1S00;
028700160929         ENDIF;
028800090520
028900160929       // Emissione videata?
029000160929         exfmt TBACA1C01;
029100090520
029200160929         reset ErrMessage;
029300160929         reset ErrGenerico;
029400160929         clear V1Dmsg;
029500090520         SELECT;
029600090520
029700160929         // F3=Fine?
029800160929         WHEN  dsp_aid = c_F03;
029900160929           OACAfxx = '1';
030000160929           Fine = *on;
030100090520
030200160929         // F5=Rivisualizza?
030300160929         WHEN  dsp_aid = c_F05;
030400160929           wInzS01 = *on;
030500090520
030600160929         // F12=Ritorno?
030700160929         WHEN  dsp_aid = c_F12;
030800160930           OACAfxx = '2';
030900160929           Fine = *on;
031000090520
031100160929         // Roll-Up?
031200160929         WHEN  dsp_aid = c_RollUp;
031300160929           exsr RollUpS01;
031400090520
031500160929         // SubFile vuoto?
031600160929         WHEN  S01nrr = *zero;
031700090520
031800160930         // Invio?
031900160929         OTHER;
032000160929           exsr OpzS01;
032100090520
032200090520         ENDSL;
032300090520
032400090520       ENDSR;
032500090520
032600090520       //--------------------------------------------------------------
032700090520       //?Inizializzazione subfile S01.                                ?
032800090520       //--------------------------------------------------------------
032900160929       BEGSR InzS01;
033000090520
033100160929       // Spegnimento degli indicatori di errore?
033200090520         %subst(IndDspF : 50) = *off;
033300090520
033400160929       // Settaggio indicatori dei tasti funzionali?
033500160929         exsr SetIndP01;
033600090520
033700160929       // Pulizia subfile?
033800160929         SflDsp    = *on;
033900160929         SflDspCtl = *on;
034000160930         write TBACA1C01;
034100160929         SflDspCtl = *off;
034200160929         SflEnd    = *off;
034300090520
034400160929         clear W_SflPag;
034500160929         clear C1RcdNbr;
034600160929         clear C1CsrRrn;
034700160929         clear S01nrr;
034800160929         clear V1Dmsg;
034900160929         ErrMessage  = *off;
035000090520         ErrGenerico = *off;
035100090520
035200160929       // Caricamento della 1ª pagina di dati nel subfile?
035300160929         clear k05tntbe01;
035400090520         k_TBEcod = c_Tab;
035500160929         setll %kds(k05tntbe01) TNTBE01L;
035600160929         reade %kds(k05tntbe01:1) TNTBE01L;
035700160929         exsr RollUpS01;
035800090520
035900160929       // Impaginazione subfile?
036000160929       // -> forza l'impaginazione sul 1° rec. del subfile
036100160929         IF  S01nrr > *zero;
036200160929           C1RcdNbr = 1;
036300160929           C1CsrRrn = 1;
036400160929         ELSE;
036500090520           clear C1RcdNbr;
036600160929         ENDIF;
036700090520
036800090520       ENDSR;
036900090520
037000090520       //--------------------------------------------------------------
037100090520       //?Caricamento singola pagina nel subfile S01                   ?
037200090520       //--------------------------------------------------------------
037300160929       BEGSR RollUpS01;
037400090520
037500090520         S01nrr    = (W_SflPag * c_SflPag);
037600090520         W_SflPag += 1;
037700090520         SflNxtChg = *off;
037800090520
037900161212       // Ciclo di caricamento
038000160929         DOW  S01nrr    < (W_SflPag * c_SflPag)   and
038100090520              S01nrr    < c_MaxRec                and
038200090520              Not %eof(TNTBE01L);
038300090520
038400160929         // Selezione singolo record file TNTBE00F?
038500160929           exsr SelezRec;
038600090520
038700160929         // Registrazione singolo record nel subfile?
038800160929           IF  RecOK;
038900090520
039000160929             clear TBACA1S01;
039100160929             dACA = TBEuni;
039200090520             S01cod = TBEke1;
039300160929             S01des = §ACAdes;
039400160929             S01ata = §ACAata;
039500170113             S01tpa = §ACAtpa;
039600160929             S01uti = §ACAuti;
039700160929             S01ann = §ACAann;
039800170113             S01flg = §ACAopz;
039900090520
040000090520             S01nrr += 1;
040100160929             write TBACA1S01;
040200090520
040300160929           ENDIF;
040400090520
040500160929         // Lettura record successivo?
040600160929           reade %kds(k05tntbe01:1) TNTBE01L;
040700090520
040800160929         ENDDO;
040900090520
041000160929       // Visualizzazione del SFL (se ci sono dati)?
041100160929         SflDsp = (S01nrr <= *zero);
041200090520
041300160929       // Attivazione del SFLEND?
041400160929         SflEnd = (S01nrr >= c_MaxRec   or   %eof(TNTBE01L));
041500090520
041600160929       // Posizionamento cursore al 1° rcd della pagina?
041700160929         IF  S01nrr > *zero;
041800090520           wPag     = S01nrr / c_SflPag;
041900090520           wRig     = S01nrr - (c_SflPag * wPag);
042000090520           C1RcdNbr = wPag * c_SflPag;
042100160929           IF  wRig > *zero;
042200090520             C1RcdNbr += 1;
042300160929           ELSE;
042400090520             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
042500160929           ENDIF;
042600160929         ELSE;
042700160929           clear C1RcdNbr;
042800160929         ENDIF;
042900090520
043000090520         C1CsrRrn = C1RcdNbr;
043100090520
043200090520       ENDSR;
043300090520
043400090520       //--------------------------------------------------------------
043500090520       //?Settaggio indicatori utilizzati in C01/P01.                  ?
043600090520       //--------------------------------------------------------------
043700160929       BEGSR SetIndP01;
043800090520
043900160929       // F3=Fine?
044000160929         F3Attivo = (not wCalled);
044100090520
044200160929       //?F12=Ritorno?
044300160929         F12Attivo = (wCalled);
044400090520
044500090520       ENDSR;
044600090520
044700090520       //--------------------------------------------------------------
044800161212       //?Selezione del singolo record letto dalla tabella.      ?
044900090520       //--------------------------------------------------------------
045000160929       BEGSR SelezRec;
045100090520
045200160929         reset RecOK;
045300090520
045400160929         dACA = TBEuni;
045500090520
045600160929         SELECT;
045700090520
045800160929       // Omissione records annullati?
045900160929         WHEN  TBEatb <> *blank;
046000160929           leavesr;
046100100421
046200161212       // Selezione tipo assenza
046300160929         WHEN  IACAata <> *blanks and IACAata <> §ACAata;
046400160929           leavesr;
046500091218
046600161212       // Selezione utilizzo causale assenza ?
046700160929       // S=Sì seleziono solo i rcd a blank
046800160929         WHEN  IACAuti = 'S' and §ACAuti <> *blanks;
046900160929           leavesr;
047000091218
047100161212       // Selezione utilizzo causale assenza ?
047200160929       //   N=No escludo i rcd a blank
047300160929         WHEN  IACAuti = 'N' and §ACAuti = *blanks;
047400160929           leavesr;
047500160929         ENDSL;
047600090520
047700160929         RecOK = *on;
047800090520
047900090520       ENDSR;
048000090520
048100090520       //--------------------------------------------------------------
048200090520       //?Gestione opzioni subfile S01.                                ?
048300090520       //--------------------------------------------------------------
048400160929       BEGSR OpzS01;
048500090520
048600160929         clear SavS01csr;
048700090520
048800160929       // Ciclo di lettura subfile?
048900160929         readc TBACA1S01;
049000090520
049100160930         DOW  not %eof(TNTBACAD1);
049200090520
049300090520           SflNxtChg = *off;
049400090520           %subst(IndDspF : 50) = *off;
049500090520           C1RcdNbr = S01nrr;
049600090520
049700160929           SELECT;
049800090520
049900160929         // Opz. 1=Selezione?
050000160929           WHEN  S01opz = '1' and not Fine;
050100160929             clear dACA;
050200160930             OACAke1 = S01cod;
050300160929             Fine = *on;
050400090520
050500160929         // Opz. 1=Selezione già reperita?
050600160929         // (ammessa una sola selezione)?
050700160929           WHEN  S01opz = '1' and Fine;
050800160929             ErrMessage  = *on;
050900160929             ErrGenerico = *on;
051000160929             PosCurOPZ   = *on;
051100160929             V1Dmsg = Msg(02);
051200160929             Fine  = *off;
051300090520
051400160929         // Nessuna opzione?
051500160929           WHEN  S01opz = *blank;
051600090520
051700160929         // Opzione errata?
051800160929           OTHER;
051900160929             ErrMessage  = *on;
052000160929             ErrGenerico = *on;
052100160929             PosCurOPZ   = *on;
052200160929             V1Dmsg = Msg(01);
052300160929             Fine  = *off;
052400090520
052500160929           ENDSL;
052600090520
052700160929         // Memorizzazione nrr del sfl con la 1ª opzione per?
052800160929         // riposizionarvici il cursore dopo il tasto "Invio"?
052900160929           IF  S01opz <> *blanks and
053000160929              (SavS01csr = *zeros or SavS01csr < C1RcdNbr);
053100160929             SavS01csr = C1RcdNbr;
053200160929           ENDIF;
053300090520
053400160929         // Aggiornamento sfl?
053500160929           SELECT;
053600160929           WHEN  ErrMessage;
053700160929             SflNxtChg = *on;
053800160929             C1CsrRrn  = C1RcdNbr;
053900160929           WHEN  ErrGenerico;
054000160929             C1CsrRrn  = C1RcdNbr;
054100160929             IF  S01opz <> '1';
054200160929               clear S01opz;
054300160929             ENDIF;
054400160929           OTHER;
054500160929             IF  S01opz <> '1';
054600160929               clear  S01opz;
054700160929             ENDIF;
054800160929           ENDSL;
054900090520
055000160929           IF  S01opz <> *blank;
055100090520             SflNxtChg = *on;
055200160929           ENDIF;
055300090520
055400160929           UPDATE TBACA1S01;
055500090520
055600160929           IF  ErrGenerico or ErrMessage;
055700090520             leave;
055800160929           ENDIF;
055900090520
056000160929           readc TBACA1S01;
056100090520
056200090520         ENDDO;
056300090520
056400160929       // Riposiziono il cursore sul record contenente la prima opz.?
056500160929       // (se non sono stati rilevati errori)?
056600160929         IF  not ErrMessage and not ErrGenerico and
056700090520             SavS01csr > *zero;
056800090520           C1CsrRrn = SavS01csr;
056900160929         ENDIF;
057000090520
057100090520       ENDSR;
057200090520
057300090520       //--------------------------------------------------------------
057400090520       //?Operazioni finali.                                           ?
057500090520       //--------------------------------------------------------------
057600160929       BEGSR RoutEnd;
057700090521
057800160929       // Ritorno parametri?
057900160929         IF  SDSprm > 1;
058000160930           p_TNTBACADS = TNTBACADS;
058100160929         ENDIF;
058200090520
058300160929       // "Chiusura" pgm?
058400090520         return;
058500090520
058600090520       ENDSR;
058700090520
058800090520      /end-free
058900090520
059000090520       //--------------------------------------------------------------
059100090520       //?Schiere a tempo di compilazione.                             ?
059200090520       //--------------------------------------------------------------
059300090520
059400160929** -  Msg -------------------------------------------------------------------*
059500090520Opzione errata                                                                 1
059600090520Ammessa la selezione di una sola causale                                       2
