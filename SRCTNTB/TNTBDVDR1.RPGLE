000100160929       //--------------------------------------------------------------
000200180116       //?TNTBDVDR1 * Selezione tab. "DVD" = Codice Task DVD
000300160929       //--------------------------------------------------------------
000400160929     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160929     h dftactgrp(*no) actgrp(*caller)
000600160929
000700160929       //--------------------------------------------------------------
000800160929       //?Dichiarazione file.                                          ?
000900160929       //--------------------------------------------------------------
001000160929       // - Tabelle
001100160929     fTNTBE01L  if   e           k disk
001200160929
001300160929       // - Video
001400180116     fTNTBDVDD1 cf   e             workstn
001500180116     f                                     sfile(TBDVD1S01 : S01nrr)
001600160929     f                                     indds(IndDspF)
001700160929     f                                     infds(InfDspF)
001800160929
001900160929       //--------------------------------------------------------------
002000160929       //?Definizione costanti.                                        ?
002100160929       //--------------------------------------------------------------
002200160929       // - Codice tabella in gestione
002300180116     d c_Tab           c                   const('DVD')
002400160929
002500160929       // - Numero di record in ciascuna videata di subfile
002600160929     d c_SflPag        c                   const(16)
002700160929
002800160929       // - Numero massimo di record gestibili
002900160929     d c_MaxRec        c                   const(9999)
003000160929
003100160929       // - Tasti funzionali a video
003200160929     d c_F01           c                   const(x'31')
003300160929     d c_F02           c                   const(x'32')
003400160929     d c_F03           c                   const(x'33')
003500160929     d c_F05           c                   const(x'35')
003600160929     d c_F06           c                   const(x'36')
003700160929     d c_F07           c                   const(x'37')
003800160929     d c_F08           c                   const(x'38')
003900160929     d c_F09           c                   const(x'39')
004000160929     d c_F10           c                   const(x'3A')
004100160929     d c_F12           c                   const(x'3C')
004200160929     d c_F13           c                   const(x'B1')
004300160929     d c_F14           c                   const(x'B2')
004400160929     d c_F15           c                   const(x'B3')
004500160929     d c_F16           c                   const(x'B4')
004600160929     d c_F17           c                   const(x'B5')
004700160929     d c_F18           c                   const(x'B6')
004800160929     d c_F19           c                   const(x'B7')
004900160929     d c_F20           c                   const(x'B8')
005000160929     d c_F21           c                   const(x'B9')
005100160929     d c_F22           c                   const(x'BA')
005200160929     d c_F23           c                   const(x'BB')
005300160929     d c_F24           c                   const(x'BC')
005400160929     d c_Enter         c                   const(x'F1')
005500160929     d c_RollDown      c                   const(x'F4')
005600160929     d c_RollUp        c                   const(x'F5')
005700160929
005800160929       //--------------------------------------------------------------
005900160929       //?Definizione schiere.                                         ?
006000160929       //--------------------------------------------------------------
006100160929       // - Messaggi di errore
006200160929     d Msg             s             78    dim( 2)  ctdata  perrcd(1)
006300160929
006400160929       //--------------------------------------------------------------
006500160929       //?Definizione aree dati.                                       ?
006600160929       //--------------------------------------------------------------
006700160929       // - Dati utente
006800160929     d §AzUte        e ds                  extname(AZUTE00F)
006900160929     d                                     dtaara
007000160929     d §DatiUte      e ds                  extname(dDatiUte)
007100160929     d                                     dtaara
007200160929
007300160929       //--------------------------------------------------------------
007400160929       //?Definizione strutture dati.                                  ?
007500160929       //--------------------------------------------------------------
007600160929       // - Status ds
007700160929     d Status         sds
007800160929     d  SDSpgm           *proc
007900160929     d  SDSprm           *parms
008000160929
008100160929       // - InfDS
008200160929     d InfDspF         ds
008300160929     d  dsp_aid              369    369a
008400160929     d  sfl_rrn              376    377i 0
008500160929     d  min_nrr              378    379i 0
008600160929     d  num_rcds             380    381i 0
008700160929
008800160929       // - Indicatori su DspF
008900160929     d IndDspF         ds                  inz
009000160929       // - Abilitazione tasti funzionali
009100160929     d  F3Attivo                       n   overlay(IndDspF : 03)
009200160929     d  F6Attivo                       n   overlay(IndDspF : 06)
009300160929     d  F12Attivo                      n   overlay(IndDspF : 12)
009400160929       // - Emissione messaggio di errore
009500160929     d  ErrMessage                     n   overlay(IndDspF : 28)
009600160929       // - Indicatori di gestione del subfile
009700160929     d  SflDsp                         n   overlay(IndDspF : 30)
009800160929     d  SflDspCtl                      n   overlay(IndDspF : 31)
009900160929     d  SflNxtChg                      n   overlay(IndDspF : 32)
010000160929     d  SflEnd                         n   overlay(IndDspF : 33)
010100160929       // - Indicatori per Attibuti di visualizzazione
010200160929       // - Posizionamento cursore & segnalazione errore
010300160929     d  PosCurOPZ                      n   overlay(IndDspF : 50)
010400160929       //   - Riemissione videata
010500160929     d  ErrGenerico                    n   overlay(IndDspF : 99)
010600160929
010700160929       // - Parametri ricevuti
010800160929     d KPJBA         e ds
010900180116     d TNTBDVDDS     e ds                  inz
011000160929
011100160929       // - Parametri per Reperimento dati utente
011200160929     d TIBS34DS      e ds                  inz
011300160929
011400180116       // - Tab. DVD = Tipo Assenza Autisti
011500180116     d dDVD          e ds                  inz
011600160929
011700160929       //--------------------------------------------------------------
011800160929       //?Definizione variabili globali.                               ?
011900160929       //--------------------------------------------------------------
012000160929       // - Flags booleani
012100160929     d Fine            s               n   inz
012200160929     d RecOK           s               n   inz
012300160929     d wInzS01         s               n   inz(*on)
012400160929     d wCalled         s               n   inz
012500160929
012600160929       // - Parametri di input facoltativi
012700180116     d p_TNTBDVDDS     s                   like(TNTBDVDDS)
012800160929
012900160929       // - Variabili per la gestione del video
013000160929     d Video           s              2    inz('S1')
013100160929     d W_SflPag        s              3  0 inz
013200160929     d wPag            s              4  0 inz
013300160929     d wRig            s              3  0 inz
013400160929     d S01nrr          s                   like(C1RcdNbr) inz
013500160929     d SavS01csr       s                   like(C1RcdNbr) inz
013600160929
013700160929       //--------------------------------------------------------------
013800160929       //?Definizione prototipi procedure.                             ?
013900160929       //--------------------------------------------------------------
014000160929       // - Reperimento dati utente
014100160929      /copy gaitrasrc/srcProtoPR,TIBS34R
014200160929
014300160929       //--------------------------------------------------------------
014400160929       //?Definizione key-list.                                        ?
014500160929       //--------------------------------------------------------------
014600160929       // - File TNTBE01L
014700160929     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
014800160929     d                                     prefix(k_)   inz
014900160929
015000160929       //--------------------------------------------------------------
015100160929       //?Riepilogo indicatori utilizzati.                             ?
015200160929       //--------------------------------------------------------------
015300160929
015400160929       //--------------------------------------------------------------
015500160929
015600160929       //--------------------------------------------------------------
015700160929       //?M A I N - L I N E                                            ?
015800160929       //--------------------------------------------------------------
015900160929     c     *ENTRY        plist
016000160929     c                   parm                    KPJBA
016100180116     c                   parm                    p_TNTBDVDDS
016200160929
016300160929      /free
016400160929
016500160929       // Operazioni iniziali?
016600160929       exsr RoutInz;
016700160929
016800160929       // Ciclo di gestione del file video?
016900160929       DOW  not Fine;
017000160929         SELECT;
017100160929         // Gestione videata S1 (subfile)
017200160929         WHEN  Video = 'S1';
017300160929           exsr GesS01;
017400160929         OTHER;
017500160929           Fine = *on;
017600160929         ENDSL;
017700160929       ENDDO;
017800160929
017900160929       // Operazioni finali?
018000160929       exsr RoutEnd;
018100160929
018200160929       //--------------------------------------------------------------
018300160929       //?Operazioni iniziali.                                         ?
018400160929       //--------------------------------------------------------------
018500160929       BEGSR RoutInz;
018600160929
018700160929       // Reperimento dati job?
018800160929         exsr DatiJob;
018900160929
019000160929       // Impostazione nome programma a video?
019100160929         V1Tpgm = SDSpgm;
019200160929
019300160929       // Verifica se ricevuti parametri?
019400160929         IF  SDSprm > 1;
019500180116           TNTBDVDDS = p_TNTBDVDDS;
019600160929           wCalled = *on;
019700160929         ELSE;
019800180116           clear TNTBDVDDS;
019900160929           wCalled = *off;
020000160929         ENDIF;
020100160929
020200160929       // Impostazione chiusura?
020300160929         SELECT;
020400180116         WHEN  wCalled and IDVDtla = *blank;
020500160929           *inRT = *on;
020600180116         WHEN  wCalled and IDVDtla = 'L';
020700160929           *inLR = *on;
020800180116         WHEN  wCalled and IDVDtla = 'C';
020900160929           *inLR = *on;
021000160929           Fine = *on;
021100160929           leavesr;
021200160929         OTHER;
021300160929           *inLR = *on;
021400160929         ENDSL;
021500160929
021600160929       // Reperimento dati job?
021700160929         exsr DatiJob;
021800160929
021900160929       // Impostazione nome programma a video?
022000160929         V1Tpgm = SDSpgm;
022100160929
022200160929       // Impostazione iniziale / pulizia dei campi chiave?
022300160929         k_TBEcod = c_Tab;
022400160929         clear k_TBEke1;
022500160929         clear k_TBEke2;
022600160929         clear k_TBElin;
022700160929         clear k_TBEsif;
022800160929
022900160929       // Impostazione iniziale parametri di output?
023000180116         clear ODVDke1;
023100180116         clear ODVDuni;
023200180116         clear ODVDfxx;
023300180116         ODVDerr = *off;
023400180116         clear ODVDmsg;
023500160929
023600160929       // Re-impostazione flags booleani (fosse mai il 2° richiamo)?
023700160929         reset Fine;
023800160929         reset Video;
023900160929         reset wInzS01;
024000160929
024100160929       ENDSR;
024200160929
024300160929       //--------------------------------------------------------------
024400160929       //?Reperimento Dati del job (Utente/Operativi).                 ?
024500160929       //--------------------------------------------------------------
024600160929       BEGSR DatiJob;
024700160929
024800160929         in(e) §AzUte;
024900160929         IF  not %error;
025000160929           in(e) §DatiUte;
025100160929         ENDIF;
025200160929         IF  %error or RSut = *blank;
025300160929           tibs34r ( tibs34ds );
025400160929           in §AzUte;
025500160929           in §DatiUte;
025600160929         ENDIF;
025700160929
025800160929       ENDSR;
025900160929
026000160929       //--------------------------------------------------------------
026100160929       //?Gestione subfile S01.                                        ?
026200160929       //--------------------------------------------------------------
026300160929       BEGSR GesS01;
026400160929
026500160929       // Inizializzazione videata?
026600160929         IF  wInzS01;
026700160929           exsr InzS01;
026800160929           wInzS01 = *off;
026900160929         ENDIF;
027000160929
027100160929       // Emissione Testata & Piede?
027200180116         write TBDVD1T01;
027300180116         write TBDVD1P01;
027400160929
027500160929       //?Posizionamento cursore?
027600160929         IF  C1CsrRrn > *zero;
027700160929           C1RcdNbr = C1CsrRrn;
027800160929         ELSE;
027900160929           C1RcdNbr = 1;
028000180116           write TBDVD1S00;
028100160929         ENDIF;
028200160929
028300160929       // Emissione videata?
028400180116         exfmt TBDVD1C01;
028500160929
028600160929         reset ErrMessage;
028700160929         reset ErrGenerico;
028800160929         clear V1Dmsg;
028900160929         SELECT;
029000160929
029100160929       // F3=Fine?
029200160929         WHEN  dsp_aid = c_F03;
029300180116           ODVDfxx = '1';
029400160929           Fine = *on;
029500160929
029600160929       // F5=Rivisualizza?
029700160929         WHEN  dsp_aid = c_F05;
029800160929           wInzS01 = *on;
029900160929
030000160929       // F12=Ritorno?
030100160929         WHEN  dsp_aid = c_F12;
030200180116           ODVDfxx = '2';
030300160929           Fine = *on;
030400160929
030500160929       // Roll-Up?
030600160929         WHEN  dsp_aid = c_RollUp;
030700160929           exsr RollUpS01;
030800160929
030900160929       // SubFile vuoto?
031000160929         WHEN  S01nrr = *zero;
031100160929
031200160929       // Invio?
031300160929         OTHER;
031400160929           exsr OpzS01;
031500160929
031600160929         ENDSL;
031700160929
031800160929       ENDSR;
031900160929
032000160929       //--------------------------------------------------------------
032100160929       //?Inizializzazione subfile S01.                                ?
032200160929       //--------------------------------------------------------------
032300160929       BEGSR InzS01;
032400160929
032500160929       // Spegnimento degli indicatori di errore?
032600160929         %subst(IndDspF : 50) = *off;
032700160929
032800160929       // Settaggio indicatori dei tasti funzionali?
032900160929         exsr SetIndP01;
033000160929
033100160929       // Pulizia subfile?
033200160929         SflDsp    = *on;
033300160929         SflDspCtl = *on;
033400180116         write  TBDVD1C01;
033500160929         SflDspCtl = *off;
033600160929         SflEnd    = *off;
033700160929
033800160929         clear W_SflPag;
033900160929         clear C1RcdNbr;
034000160929         clear C1CsrRrn;
034100160929         clear S01nrr;
034200160929         clear V1Dmsg;
034300160929         ErrMessage  = *off;
034400160929         ErrGenerico = *off;
034500160929
034600160929       // Caricamento della 1ª pagina di dati nel subfile?
034700160929         clear k05tntbe01;
034800160929         k_TBEcod = c_Tab;
034900160929         setll %kds(k05tntbe01) TNTBE01L;
035000160929         reade %kds(k05tntbe01:1) TNTBE01L;
035100160929         exsr RollUpS01;
035200160929
035300160929       // Impaginazione subfile?
035400160929       // -> forza l'impaginazione sul 1° rec. del subfile
035500160929         IF  S01nrr > *zero;
035600160929           C1RcdNbr = 1;
035700160929           C1CsrRrn = 1;
035800160929         ELSE;
035900160929           clear C1RcdNbr;
036000160929         ENDIF;
036100160929
036200160929       ENDSR;
036300160929
036400160929       //--------------------------------------------------------------
036500160929       //?Caricamento singola pagina nel subfile S01                   ?
036600160929       //--------------------------------------------------------------
036700160929       BEGSR RollUpS01;
036800160929
036900160929         S01nrr    = (W_SflPag * c_SflPag);
037000160929         W_SflPag += 1;
037100160929         SflNxtChg = *off;
037200160929
037300161212       // Ciclo di caricamento dati da tabella   ?
037400160929         DOW  S01nrr    < (W_SflPag * c_SflPag)   and
037500160929              S01nrr    < c_MaxRec                and
037600160929              Not %eof(TNTBE01L);
037700160929
037800160929         //?Selezione singolo record file TNTBE00F?
037900160929           exsr SelezRec;
038000160929
038100160929         // Registrazione singolo record nel subfile?
038200160929           IF  RecOK;
038300160929
038400180116             clear TBDVD1S01;
038500180116             dDVD = TBEuni;
038600160929             S01cod = TBEke1;
038700180116             S1huni = dDVD;
038800180116             S01cover = §DVDcover;
038900160929
039000160929             S01nrr += 1;
039100180116             write TBDVD1S01;
039200160929
039300160929           ENDIF;
039400160929
039500160929       // Lettura record successivo?
039600160929           reade %kds(k05tntbe01:1) TNTBE01L;
039700160929
039800160929         ENDDO;
039900160929
040000160929       // Visualizzazione del SFL (se ci sono dati)?
040100160929         SflDsp = (S01nrr <= *zero);
040200160929
040300160929       // Attivazione del SFLEND?
040400160929         SflEnd = (S01nrr >= c_MaxRec or %eof(TNTBE01L));
040500160929
040600160929       // Posizionamento cursore al 1° rcd della pagina?
040700160929         IF  S01nrr > *zero;
040800160929           wPag     = S01nrr / c_SflPag;
040900160929           wRig     = S01nrr - (c_SflPag * wPag);
041000160929           C1RcdNbr = wPag * c_SflPag;
041100160929           IF  wRig > *zero;
041200160929             C1RcdNbr += 1;
041300160929           ELSE;
041400160929             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
041500160929           ENDIF;
041600160929         ELSE;
041700160929           clear C1RcdNbr;
041800160929         ENDIF;
041900160929
042000160929         C1CsrRrn = C1RcdNbr;
042100160929
042200160929       ENDSR;
042300160929
042400160929       //--------------------------------------------------------------
042500160929       //?Settaggio indicatori utilizzati in C01/P01.                  ?
042600160929       //--------------------------------------------------------------
042700160929       BEGSR SetIndP01;
042800160929
042900160929       // F3=Fine?
043000160929         F3Attivo = (Not wCalled);
043100160929
043200160929       //?F12=Ritorno?
043300160929         F12Attivo = (wCalled);
043400160929
043500160929       ENDSR;
043600160929
043700160929       //--------------------------------------------------------------
043800180116       //?Selezione del singolo record letto dalla tabella "DVD".      ?
043900160929       //--------------------------------------------------------------
044000160929       BEGSR SelezRec;
044100160929
044200160929         reset RecOK;
044300160929
044400180116         dDVD = TBEuni;
044500160929
044600160929         SELECT;
044700160929
044800160929       // Omissione records annullati?
044900160929         WHEN  TBEatb <> *blank;
045000160929           leavesr;
045100160929
045200160929         ENDSL;
045300160929
045400160929         RecOK = *on;
045500160929
045600160929       ENDSR;
045700160929
045800160929       //--------------------------------------------------------------
045900160929       //?Gestione opzioni subfile S01.                                ?
046000160929       //--------------------------------------------------------------
046100160929       BEGSR OpzS01;
046200160929
046300160929         clear SavS01csr;
046400160929
046500160929       // Ciclo di lettura subfile?
046600180116         readc TBDVD1S01;
046700160929
046800180116         DOW  not %eof(TNTBDVDD1);
046900160929
047000160929           SflNxtChg = *off;
047100160929           %subst(IndDspF : 50) = *off;
047200160929           C1RcdNbr = S01nrr;
047300160929
047400160929           SELECT;
047500160929
047600160929         // Opz. 1=Selezione?
047700160929           WHEN  S01opz = '1' and not Fine;
047800180116               clear dDVD;
047900180116               dDVD    = S1huni;
048000180116               ODVDke1 = S01cod;
048100180116               ODVDuni = dDVD;
048200160929               Fine    = *on;
048300160929
048400160929         // Opz. 1=Selezione già reperita?
048500160929         //   (ammessa una sola selezione)?
048600160929           WHEN  S01opz = '1' and Fine;
048700160929             ErrMessage  = *on;
048800160929             ErrGenerico = *on;
048900160929             PosCurOPZ   = *on;
049000160929             V1Dmsg = Msg(02);
049100160929             Fine  = *off;
049200160929
049300160929         // Nessuna opzione?
049400160929           WHEN  S01opz = *blank;
049500160929
049600160929         // Opzione errata?
049700160929           OTHER;
049800160929             ErrMessage  = *on;
049900160929             ErrGenerico = *on;
050000160929             PosCurOPZ   = *on;
050100160929             V1Dmsg = Msg(01);
050200160929             Fine = *off;
050300160929
050400160929           ENDSL;
050500160929
050600160929       // Memorizzazione nrr del sfl con la 1ª opzione per?
050700160929       // riposizionarvici il cursore dopo il tasto "Invio"?
050800160929           IF  S01opz <> *blank and
050900160929              (SavS01csr = *zeros or SavS01csr < C1RcdNbr);
051000160929             SavS01csr = C1RcdNbr;
051100160929           ENDIF;
051200160929
051300160929         // Aggiornamento sfl?
051400160929           SELECT;
051500160929           WHEN  ErrMessage;
051600160929             SflNxtChg = *on;
051700160929             C1CsrRrn  = C1RcdNbr;
051800160929           WHEN  ErrGenerico;
051900160929             C1CsrRrn  = C1RcdNbr;
052000160929             IF  S01opz <> '1';
052100160929               clear S01opz;
052200160929             ENDIF;
052300160929           OTHER;
052400160929             IF  S01opz <> '1';
052500160929               clear S01opz;
052600160929             ENDIF;
052700160929           ENDSL;
052800160929
052900160929           IF  S01opz <> *blank;
053000160929             SflNxtChg = *on;
053100160929           ENDIF;
053200160929
053300180116           UPDATE TBDVD1S01;
053400160929
053500160929           IF  ErrGenerico or ErrMessage;
053600160929             leave;
053700160929           ENDIF;
053800160929
053900180116           readc TBDVD1S01;
054000160929
054100160929         ENDDO;
054200160929
054300160929       // Riposiziono il cursore sul record contenente la prima opz.?
054400160929       //   (se non sono stati rilevati errori)?
054500160929         IF  not ErrMessage and not ErrGenerico and
054600160929             SavS01csr > *zero;
054700160929           C1CsrRrn = SavS01csr;
054800160929         ENDIF;
054900160929
055000160929       ENDSR;
055100160929
055200160929       //--------------------------------------------------------------
055300160929       //?Operazioni finali.                                           ?
055400160929       //--------------------------------------------------------------
055500160929       BEGSR RoutEnd;
055600160929
055700160929       // Ritorno parametri?
055800160929         IF  SDSprm > 1;
055900180116           p_TNTBDVDDS = TNTBDVDDS;
056000160929         ENDIF;
056100160929
056200160929       // "Chiusura" pgm?
056300160929         return;
056400160929
056500160929       ENDSR;
056600160929
056700160929      /end-free
056800160929
056900160929       //--------------------------------------------------------------
057000160929       //?Schiere a tempo di compilazione.                             ?
057100160929       //--------------------------------------------------------------
057200160929
057300160929** -  Msg -------------------------------------------------------------------*
057400160929Opzione errata                                                                 1
057500180116Ammessa la selezione di un solo codice                                         2
