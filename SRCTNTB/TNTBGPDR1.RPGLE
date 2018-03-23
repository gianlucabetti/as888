000100090520       //--------------------------------------------------------------
000200170315       //?TNTBGPDR1 * Selezione tab. "GPD" = Giorni Posticipo Data       ?
000300090520       //--------------------------------------------------------------
000400090520     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500090520
000600090520       //--------------------------------------------------------------
000700090520       //?Dichiarazione file.                                          ?
000800090520       //--------------------------------------------------------------
000900170315       // - Organigramma
001000170315     fAZORG01L  if   e           k disk
001100170315
001200090520       // - Tabelle
001300100309     fTNTBE01L  if   e           k disk
001400090520
001500090520       // - Video
001600170315     fTNTBGPDD1 cf   e             workstn
001700170315     f                                     sfile(TBGPD1S01:S01nrr)
001800090520     f                                     indds(IndDspF)
001900090520     f                                     infds(InfDspF)
002000090520
002100090520       //--------------------------------------------------------------
002200090520       //?Definizione costanti.                                        ?
002300090520       //--------------------------------------------------------------
002400090520       // - Codice tabella in gestione
002500170315     d c_Tab           c                   const('GPD')
002600090520
002700090520       // - Numero di record in ciascuna videata di subfile
002800090520     d c_SflPag        c                   const(16)
002900090520
003000090520       // - Numero massimo di record gestibili
003100110331     d c_MaxRec        c                   const(9999)
003200090520
003300090520       // - Tasti funzionali a video
003400090520     d c_F01           c                   const(x'31')
003500090520     d c_F02           c                   const(x'32')
003600090520     d c_F03           c                   const(x'33')
003700100504     d c_F04           c                   const(x'34')
003800090520     d c_F05           c                   const(x'35')
003900090520     d c_F06           c                   const(x'36')
004000090520     d c_F07           c                   const(x'37')
004100090520     d c_F08           c                   const(x'38')
004200090520     d c_F09           c                   const(x'39')
004300090520     d c_F10           c                   const(x'3A')
004400100504     d c_F11           c                   const(x'3B')
004500090520     d c_F12           c                   const(x'3C')
004600090520     d c_F13           c                   const(x'B1')
004700090520     d c_F14           c                   const(x'B2')
004800090520     d c_F15           c                   const(x'B3')
004900090520     d c_F16           c                   const(x'B4')
005000090520     d c_F17           c                   const(x'B5')
005100090520     d c_F18           c                   const(x'B6')
005200090520     d c_F19           c                   const(x'B7')
005300090520     d c_F20           c                   const(x'B8')
005400090520     d c_F21           c                   const(x'B9')
005500090520     d c_F22           c                   const(x'BA')
005600090520     d c_F23           c                   const(x'BB')
005700090520     d c_F24           c                   const(x'BC')
005800090520     d c_Enter         c                   const(x'F1')
005900090520     d c_RollDown      c                   const(x'F4')
006000090520     d c_RollUp        c                   const(x'F5')
006100090520
006200090520       //--------------------------------------------------------------
006300090520       //?Definizione schiere.                                         ?
006400090520       //--------------------------------------------------------------
006500090520       // - Messaggi di errore
006600160929     d Msg             s             78    dim( 2)  ctdata  perrcd(1)
006700090520
006800090520       //--------------------------------------------------------------
006900090520       //?Definizione aree dati.                                       ?
007000090520       //--------------------------------------------------------------
007100090520       // - Dati utente
007200090520     d §AzUte        e ds                  extname(AZUTE00F)
007300090520     d                                     dtaara
007400090520     d §DatiUte      e ds                  extname(dDatiUte)
007500090520     d                                     dtaara
007600090520
007700090520       //--------------------------------------------------------------
007800090520       //?Definizione strutture dati.                                  ?
007900090520       //--------------------------------------------------------------
008000090520       // - Status ds
008100090520     d Status         sds
008200090520     d  SDSpgm           *proc
008300090521     d  SDSprm           *parms
008400090520
008500090520       // - InfDS
008600090520     d InfDspF         ds
008700160929     d  dsp_aid              369    369a
008800160929     d  sfl_rrn              376    377i 0
008900160929     d  min_nrr              378    379i 0
009000160929     d  num_rcds             380    381i 0
009100090520
009200090520       // - Indicatori su DspF
009300090520     d IndDspF         ds                  inz
009400160929       // - Abilitazione tasti funzionali
009500160929     d  F3Attivo                       n   overlay(IndDspF : 03)
009600160929     d  F6Attivo                       n   overlay(IndDspF : 06)
009700160929     d  F12Attivo                      n   overlay(IndDspF : 12)
009800160929       // - Emissione messaggio di errore
009900160929     d  ErrMessage                     n   overlay(IndDspF : 28)
010000160929       // - Indicatori di gestione del subfile
010100160929     d  SflDsp                         n   overlay(IndDspF : 30)
010200160929     d  SflDspCtl                      n   overlay(IndDspF : 31)
010300160929     d  SflNxtChg                      n   overlay(IndDspF : 32)
010400160929     d  SflEnd                         n   overlay(IndDspF : 33)
010500160929       // - Indicatori per Attibuti di visualizzazione
010600160929       // - Posizionamento cursore & segnalazione errore
010700160929     d  PosCurOPZ                      n   overlay(IndDspF : 50)
010800160929       //   - Riemissione videata
010900160929     d  ErrGenerico                    n   overlay(IndDspF : 99)
011000110429
011100090520       // - Parametri ricevuti
011200090520     d KPJBA         e ds
011300170315     d TNTBGPDDS     e ds                  inz
011400090520
011500090520       // - Parametri per Reperimento dati utente
011600160929     d TIBS34DS      e ds                  inz
011700090520
011800170315       // - Tab. GPD = Causali Assenza Autisti
011900170315     d dGPD          e ds                  inz
012000090520
012100110405      // - Ricerca/Controllo tabelle
012200160929     d TIBS02DS      e ds                  inz
012300110405
012400090520       //--------------------------------------------------------------
012500090520       //?Definizione variabili globali.                               ?
012600090520       //--------------------------------------------------------------
012700090520       // - Flags booleani
012800160929     d Fine            s               n   inz
012900160929     d RecOK           s               n   inz
013000160929     d wInzS01         s               n   inz(*on)
013100160929     d wCalled         s               n   inz
013200090521
013300090521       // - Parametri di input facoltativi
013400170315     d p_TNTBGPDDS     s                   like(TNTBGPDDS)
013500090520
013600090520       // - Variabili per la gestione del video
013700160929     d Video           s              2    inz('S1')
013800090520     d W_SflPag        s              3  0 inz
013900090520     d wPag            s              4  0 inz
014000090520     d wRig            s              3  0 inz
014100090521     d S01nrr          s                   like(C1RcdNbr) inz
014200090521     d SavS01csr       s                   like(C1RcdNbr) inz
014300170315
014400170315       // - Variabili di comodo
014500170315     d w0030           s              3s 0 inz
014600090520
014700090520       //--------------------------------------------------------------
014800090520       //?Definizione prototipi procedure.                             ?
014900090520       //--------------------------------------------------------------
015000090520       // - Reperimento dati utente
015100090520      /copy gaitrasrc/srcProtoPR,TIBS34R
015200110405
015300110405      /copy gaitrasrc/srcprotopr,tibs02r
015400090520
015500090520       //--------------------------------------------------------------
015600090520       //?Definizione key-list.                                        ?
015700090520       //--------------------------------------------------------------
015800090520       // - File TNTBE01L
015900090520     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
016000090520     d                                     prefix(k_)   inz
016100090520
016200090520       //--------------------------------------------------------------
016300090520       //?Riepilogo indicatori utilizzati.                             ?
016400090520       //--------------------------------------------------------------
016500160929
016600090520       //--------------------------------------------------------------
016700090520
016800090520       //--------------------------------------------------------------
016900090520       //?M A I N - L I N E                                            ?
017000090520       //--------------------------------------------------------------
017100160929     c     *ENTRY        plist
017200090520     c                   parm                    KPJBA
017300170315     c                   parm                    p_TNTBGPDDS
017400090520
017500090520      /free
017600090520
017700160929       // Operazioni iniziali?
017800160929       exsr RoutInz;
017900090520
018000160929       // Ciclo di gestione del file video?
018100160929       DOW  Fine = *off;
018200160929         SELECT;
018300160929         // Gestione videata S1 (subfile)
018400160929         WHEN  Video = 'S1';
018500160929           exsr GesS01;
018600160929         OTHER;
018700160929           Fine = *on;
018800160929         ENDSL;
018900160929       ENDDO;
019000090520
019100160929       // Operazioni finali?
019200160929       exsr RoutEnd;
019300090521
019400090520       //--------------------------------------------------------------
019500090520       //?Operazioni iniziali.                                         ?
019600090520       //--------------------------------------------------------------
019700160929       BEGSR RoutInz;
019800160929
019900160929       // Reperimento dati job?
020000160929         exsr DatiJob;
020100160929
020200160929       // Impostazione nome programma a video?
020300160929         V1Tpgm = SDSpgm;
020400090520
020500160929       // Verifica se ricevuti parametri?
020600160929         IF  SDSprm > 1;
020700170315           TNTBGPDDS = p_TNTBGPDDS;
020800160929           wCalled = *on;
020900160929         ELSE;
021000170315           clear TNTBGPDDS;
021100160929           wCalled = *off;
021200160929         ENDIF;
021300090520
021400160929       // Impostazione chiusura?
021500160929         SELECT;
021600170315         WHEN  wCalled and IGPDtla = *blank;
021700160929           *inRT = *on;
021800170315         WHEN  wCalled and IGPDtla = 'L';
021900160929           *inLR = *on;
022000170315         WHEN  wCalled and IGPDtla = 'C';
022100160929           *inLR = *on;
022200160929           Fine = *on;
022300160929           leavesr;
022400160929         OTHER;
022500160929           *inLR = *on;
022600160929         ENDSL;
022700090520
022800160929       // Reperimento dati job?
022900160929         exsr DatiJob;
023000090520
023100160929       // Impostazione nome programma a video?
023200090520         V1Tpgm = SDSpgm;
023300090520
023400160929       // Impostazione iniziale / pulizia dei campi chiave?
023500090520         k_TBEcod = c_Tab;
023600160929         clear k_TBEke1;
023700160929         clear k_TBEke2;
023800160929         clear k_TBElin;
023900160929         clear k_TBEsif;
024000090521
024100160929       // Impostazione iniziale parametri di output?
024200170315         clear OGPDke1;
024300170315         clear OGPDuni;
024400170315         clear OGPDfxx;
024500170315         OGPDerr = *off;
024600170315         clear OGPDmsg;
024700090521
024800160929       // Re-impostazione flags booleani (fosse mai il 2° richiamo)?
024900160929         reset Fine;
025000160929         reset Video;
025100160929         reset wInzS01;
025200090520
025300090520       ENDSR;
025400160929
025500160929       //--------------------------------------------------------------
025600160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
025700160929       //--------------------------------------------------------------
025800160929       BEGSR DatiJob;
025900160929
026000160929         in(e) §AzUte;
026100160929         IF  not %error;
026200160929           in(e) §DatiUte;
026300160929         ENDIF;
026400160929         IF  %error or RSut = *blank;
026500160929           tibs34r ( tibs34ds );
026600160929           in §AzUte;
026700160929           in §DatiUte;
026800160929         ENDIF;
026900160929
027000160929       ENDSR;
027100090520
027200090520       //--------------------------------------------------------------
027300090520       //?Gestione subfile S01.                                        ?
027400090520       //--------------------------------------------------------------
027500160929       BEGSR GesS01;
027600090520
027700160929       // Inizializzazione videata?
027800160929         IF  wInzS01;
027900160929           exsr InzS01;
028000160929           wInzS01 = *off;
028100160929         ENDIF;
028200090520
028300160929       // Emissione Testata & Piede?
028400170315         write TBGPD1T01;
028500170315         write TBGPD1P01;
028600090520
028700160929       // Posizionamento cursore?
028800160929         IF  C1CsrRrn > *zero;
028900090520           C1RcdNbr = C1CsrRrn;
029000160929         ELSE;
029100090520           C1RcdNbr = 1;
029200170315           write TBGPD1S00;
029300160929         ENDIF;
029400090520
029500160929       // Emissione videata?
029600170315         exfmt TBGPD1C01;
029700090520
029800160929         reset ErrMessage;
029900160929         reset ErrGenerico;
030000160929         clear V1Dmsg;
030100090520         SELECT;
030200090520
030300160929         // F3=Fine?
030400160929         WHEN  dsp_aid = c_F03;
030500170315           OGPDfxx = '1';
030600160929           Fine = *on;
030700090520
030800160929         // F5=Rivisualizza?
030900160929         WHEN  dsp_aid = c_F05;
031000160929           wInzS01 = *on;
031100090520
031200160929         // F12=Ritorno?
031300160929         WHEN  dsp_aid = c_F12;
031400170315           OGPDfxx = '2';
031500160929           Fine = *on;
031600090520
031700160929         // Roll-Up?
031800160929         WHEN  dsp_aid = c_RollUp;
031900160929           exsr RollUpS01;
032000090520
032100160929         // SubFile vuoto?
032200160929         WHEN  S01nrr = *zero;
032300090520
032400160930         // Invio?
032500160929         OTHER;
032600160929           exsr OpzS01;
032700090520
032800090520         ENDSL;
032900090520
033000090520       ENDSR;
033100090520
033200090520       //--------------------------------------------------------------
033300090520       //?Inizializzazione subfile S01.                                ?
033400090520       //--------------------------------------------------------------
033500160929       BEGSR InzS01;
033600090520
033700160929       // Spegnimento degli indicatori di errore?
033800090520         %subst(IndDspF : 50) = *off;
033900090520
034000160929       // Settaggio indicatori dei tasti funzionali?
034100160929         exsr SetIndP01;
034200090520
034300160929       // Pulizia subfile?
034400160929         SflDsp    = *on;
034500160929         SflDspCtl = *on;
034600170315         write TBGPD1C01;
034700160929         SflDspCtl = *off;
034800160929         SflEnd    = *off;
034900090520
035000160929         clear W_SflPag;
035100160929         clear C1RcdNbr;
035200160929         clear C1CsrRrn;
035300160929         clear S01nrr;
035400160929         clear V1Dmsg;
035500160929         ErrMessage  = *off;
035600090520         ErrGenerico = *off;
035700090520
035800160929       // Caricamento della 1ª pagina di dati nel subfile?
035900160929         clear k05tntbe01;
036000090520         k_TBEcod = c_Tab;
036100160929         setll %kds(k05tntbe01) TNTBE01L;
036200160929         reade %kds(k05tntbe01:1) TNTBE01L;
036300160929         exsr RollUpS01;
036400090520
036500160929       // Impaginazione subfile?
036600160929       // -> forza l'impaginazione sul 1° rec. del subfile
036700160929         IF  S01nrr > *zero;
036800160929           C1RcdNbr = 1;
036900160929           C1CsrRrn = 1;
037000160929         ELSE;
037100090520           clear C1RcdNbr;
037200160929         ENDIF;
037300090520
037400090520       ENDSR;
037500090520
037600090520       //--------------------------------------------------------------
037700090520       //?Caricamento singola pagina nel subfile S01                   ?
037800090520       //--------------------------------------------------------------
037900160929       BEGSR RollUpS01;
038000090520
038100090520         S01nrr    = (W_SflPag * c_SflPag);
038200090520         W_SflPag += 1;
038300090520         SflNxtChg = *off;
038400090520
038500161212       // Ciclo di caricamento
038600160929         DOW  S01nrr    < (W_SflPag * c_SflPag)   and
038700090520              S01nrr    < c_MaxRec                and
038800090520              Not %eof(TNTBE01L);
038900090520
039000160929         // Selezione singolo record file TNTBE00F?
039100160929           exsr SelezRec;
039200090520
039300160929         // Registrazione singolo record nel subfile?
039400160929           IF  RecOK;
039500090520
039600170315             clear TBGPD1S01;
039700170315             dGPD = TBEuni;
039800170315             S01fil = TBEke1;
039900170315             IF  S01fil <> '999';
040000170315               w0030 = %int(S01fil);
040100170315               clear ORGdes;
040200170315               chain w0030 AZORG01L;
040300170315               S01des = ORGdes;
040400170315             ELSE;
040500170315               S01des = 'Tutte le Filiali';
040600170315             ENDIF;
040700170315
040800170315             S01lun = §GPDlun;
040900170315             S01mar = §GPDmar;
041000170315             S01mer = §GPDmer;
041100170315             S01gio = §GPDgio;
041200170315             S01ven = §GPDven;
041300170315             S01sab = §GPDsab;
041400170315             S01dom = §GPDdom;
041500170315             S01luna = §GPDluna;
041600170315             S01mara = §GPDmara;
041700170315             S01mera = §GPDmera;
041800170315             S01gioa = §GPDgioa;
041900170315             S01vena = §GPDvena;
042000170315             S01saba = §GPDsaba;
042100170315             S01doma = §GPDdoma;
042200090520
042300090520             S01nrr += 1;
042400170315             write TBGPD1S01;
042500090520
042600160929           ENDIF;
042700090520
042800160929         // Lettura record successivo?
042900160929           reade %kds(k05tntbe01:1) TNTBE01L;
043000090520
043100160929         ENDDO;
043200090520
043300160929       // Visualizzazione del SFL (se ci sono dati)?
043400160929         SflDsp = (S01nrr <= *zero);
043500090520
043600160929       // Attivazione del SFLEND?
043700160929         SflEnd = (S01nrr >= c_MaxRec   or   %eof(TNTBE01L));
043800090520
043900160929       // Posizionamento cursore al 1° rcd della pagina?
044000160929         IF  S01nrr > *zero;
044100090520           wPag     = S01nrr / c_SflPag;
044200090520           wRig     = S01nrr - (c_SflPag * wPag);
044300090520           C1RcdNbr = wPag * c_SflPag;
044400160929           IF  wRig > *zero;
044500090520             C1RcdNbr += 1;
044600160929           ELSE;
044700090520             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
044800160929           ENDIF;
044900160929         ELSE;
045000160929           clear C1RcdNbr;
045100160929         ENDIF;
045200090520
045300090520         C1CsrRrn = C1RcdNbr;
045400090520
045500090520       ENDSR;
045600090520
045700090520       //--------------------------------------------------------------
045800090520       //?Settaggio indicatori utilizzati in C01/P01.                  ?
045900090520       //--------------------------------------------------------------
046000160929       BEGSR SetIndP01;
046100090520
046200160929       // F3=Fine?
046300160929         F3Attivo = (not wCalled);
046400090520
046500160929       //?F12=Ritorno?
046600160929         F12Attivo = (wCalled);
046700090520
046800090520       ENDSR;
046900090520
047000090520       //--------------------------------------------------------------
047100161212       //?Selezione del singolo record letto dalla tabella.      ?
047200090520       //--------------------------------------------------------------
047300160929       BEGSR SelezRec;
047400090520
047500160929         reset RecOK;
047600090520
047700170315         dGPD = TBEuni;
047800090520
047900160929         SELECT;
048000090520
048100160929       // Omissione records annullati?
048200160929         WHEN  TBEatb <> *blank;
048300160929           leavesr;
048400100421
048500160929         ENDSL;
048600090520
048700160929         RecOK = *on;
048800090520
048900090520       ENDSR;
049000090520
049100090520       //--------------------------------------------------------------
049200090520       //?Gestione opzioni subfile S01.                                ?
049300090520       //--------------------------------------------------------------
049400160929       BEGSR OpzS01;
049500090520
049600160929         clear SavS01csr;
049700090520
049800160929       // Ciclo di lettura subfile?
049900170315         readc TBGPD1S01;
050000090520
050100170315         DOW  not %eof(TNTBGPDD1);
050200090520
050300090520           SflNxtChg = *off;
050400090520           %subst(IndDspF : 50) = *off;
050500090520           C1RcdNbr = S01nrr;
050600090520
050700160929           SELECT;
050800090520
050900160929         // Opz. 1=Selezione?
051000160929           WHEN  S01opz = '1' and not Fine;
051100170315             clear dGPD;
051200170315             OGPDke1 = S01fil;
051300160929             Fine = *on;
051400090520
051500160929         // Opz. 1=Selezione già reperita?
051600160929         // (ammessa una sola selezione)?
051700160929           WHEN  S01opz = '1' and Fine;
051800160929             ErrMessage  = *on;
051900160929             ErrGenerico = *on;
052000160929             PosCurOPZ   = *on;
052100160929             V1Dmsg = Msg(02);
052200160929             Fine  = *off;
052300090520
052400160929         // Nessuna opzione?
052500160929           WHEN  S01opz = *blank;
052600090520
052700160929         // Opzione errata?
052800160929           OTHER;
052900160929             ErrMessage  = *on;
053000160929             ErrGenerico = *on;
053100160929             PosCurOPZ   = *on;
053200160929             V1Dmsg = Msg(01);
053300160929             Fine  = *off;
053400090520
053500160929           ENDSL;
053600090520
053700160929         // Memorizzazione nrr del sfl con la 1ª opzione per?
053800160929         // riposizionarvici il cursore dopo il tasto "Invio"?
053900160929           IF  S01opz <> *blanks and
054000160929              (SavS01csr = *zeros or SavS01csr < C1RcdNbr);
054100160929             SavS01csr = C1RcdNbr;
054200160929           ENDIF;
054300090520
054400160929         // Aggiornamento sfl?
054500160929           SELECT;
054600160929           WHEN  ErrMessage;
054700160929             SflNxtChg = *on;
054800160929             C1CsrRrn  = C1RcdNbr;
054900160929           WHEN  ErrGenerico;
055000160929             C1CsrRrn  = C1RcdNbr;
055100160929             IF  S01opz <> '1';
055200160929               clear S01opz;
055300160929             ENDIF;
055400160929           OTHER;
055500160929             IF  S01opz <> '1';
055600160929               clear  S01opz;
055700160929             ENDIF;
055800160929           ENDSL;
055900090520
056000160929           IF  S01opz <> *blank;
056100090520             SflNxtChg = *on;
056200160929           ENDIF;
056300090520
056400170315           UPDATE TBGPD1S01;
056500090520
056600160929           IF  ErrGenerico or ErrMessage;
056700090520             leave;
056800160929           ENDIF;
056900090520
057000170315           readc TBGPD1S01;
057100090520
057200090520         ENDDO;
057300090520
057400160929       // Riposiziono il cursore sul record contenente la prima opz.?
057500160929       // (se non sono stati rilevati errori)?
057600160929         IF  not ErrMessage and not ErrGenerico and
057700090520             SavS01csr > *zero;
057800090520           C1CsrRrn = SavS01csr;
057900160929         ENDIF;
058000090520
058100090520       ENDSR;
058200090520
058300090520       //--------------------------------------------------------------
058400090520       //?Operazioni finali.                                           ?
058500090520       //--------------------------------------------------------------
058600160929       BEGSR RoutEnd;
058700090521
058800160929       // Ritorno parametri?
058900160929         IF  SDSprm > 1;
059000170315           p_TNTBGPDDS = TNTBGPDDS;
059100160929         ENDIF;
059200090520
059300160929       // "Chiusura" pgm?
059400090520         return;
059500090520
059600090520       ENDSR;
059700090520
059800090520      /end-free
059900090520
060000090520       //--------------------------------------------------------------
060100090520       //?Schiere a tempo di compilazione.                             ?
060200090520       //--------------------------------------------------------------
060300090520
060400160929** -  Msg -------------------------------------------------------------------*
060500090520Opzione errata                                                                 1
060600170315Ammessa la selezione di una sola filiale                                       2
