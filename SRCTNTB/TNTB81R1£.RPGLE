000100090520      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200090520       //--------------------------------------------------------------
000300100504       //?TNTB81R1 * Selezione tab. "TTA" = Tipo Attività            ?
000400090520       //--------------------------------------------------------------
000500090520
000600090520     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090520     h dftactgrp(*no) actgrp(*caller)
000800090520
000900090520       //--------------------------------------------------------------
001000090520       //?Dichiarazione file.                                          ?
001100090520       //--------------------------------------------------------------
001200090520
001300090520       // - Tabelle
001400100309     fTNTBE01L  if   e           k disk
001500090520
001600090520       // - Video
001700100504     fTNTB81D1  cf   e             workstn
001800100504     f                                     sfile(TB811S01 : S01nrr)
001900090520     f                                     indds(IndDspF)
002000090520     f                                     infds(InfDspF)
002100090520
002200090520       //--------------------------------------------------------------
002300090520       //?Definizione costanti.                                        ?
002400090520       //--------------------------------------------------------------
002500090520
002600090520       // - Codice tabella in gestione
002700100504     d c_Tab           c                   const('TTA')
002800090520
002900090520       // - Numero di record in ciascuna videata di subfile
003000090520     d c_SflPag        c                   const(16)
003100090520
003200090520       // - Numero massimo di record gestibili
003300090520     d c_MaxRec        c                   const(999)
003400090520
003500090520       // - Tasti funzionali a video
003600090520     d c_F01           c                   const(x'31')
003700090520     d c_F02           c                   const(x'32')
003800090520     d c_F03           c                   const(x'33')
003900090520     d c_F05           c                   const(x'35')
004000090520     d c_F06           c                   const(x'36')
004100090520     d c_F07           c                   const(x'37')
004200090520     d c_F08           c                   const(x'38')
004300090520     d c_F09           c                   const(x'39')
004400090520     d c_F10           c                   const(x'3A')
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
006500090520
006600090520       // - Messaggi di errore
006700090520     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
006800090520
006900090520       //--------------------------------------------------------------
007000090520       //?Definizione aree dati.                                       ?
007100090520       //--------------------------------------------------------------
007200090520
007300090520       // - Dati utente
007400090520     d §AzUte        e ds                  extname(AZUTE00F)
007500090520     d                                     dtaara
007600090520     d §DatiUte      e ds                  extname(dDatiUte)
007700090520     d                                     dtaara
007800090520
007900090520       //--------------------------------------------------------------
008000090520       //?Definizione strutture dati.                                  ?
008100090520       //--------------------------------------------------------------
008200090520
008300090520       // - Status ds
008400090520     d Status         sds
008500090520     d  SDSpgm           *proc
008600090521     d  SDSprm           *parms
008700090520
008800090520       // - InfDS
008900090520     d InfDspF         ds
009000090520     d   dsp_aid             369    369a
009100090520     d   sfl_rrn             376    377i 0
009200090520     d   min_nrr             378    379i 0
009300090520     d   num_rcds            380    381i 0
009400090520
009500090520       // - Indicatori su DspF
009600090520     d IndDspF         ds                  inz
009700090520         // - Abilitazione tasti funzionali
009800090520     d   F3Attivo                      n   overlay(IndDspF : 03)
009900090520     d   F6Attivo                      n   overlay(IndDspF : 06)
010000090520     d   F12Attivo                     n   overlay(IndDspF : 12)
010100090520         // - Emissione messaggio di errore
010200090520     d   ErrMessage                    n   overlay(IndDspF : 28)
010300090520         // - Indicatori di gestione del subfile
010400090520     d   SflDsp_N                      n   overlay(IndDspF : 30)
010500090520     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
010600090520     d   SflNxtChg                     n   overlay(IndDspF : 32)
010700090520     d   SflEnd                        n   overlay(IndDspF : 33)
010800090520         // - Indicatori per Attibuti di visualizzazione
010900090520         // - Posizionamento cursore & segnalazione errore
011000090520     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011100090520         //   - Riemissione videata
011200090520     d   ErrGenerico                   n   overlay(IndDspF : 99)
011300090520
011400090520       // - Parametri ricevuti
011500090520     d KPJBA         e ds
011600100504     d TNTB81ds      e ds                  inz
011700090520
011800090520       // - Parametri per Reperimento dati utente
011900090525     d TIBS34ds      e ds                  inz
012000090520
012100100504       // - Tab. TTA = Tipo Attività marketing
012200100504     d dTTA          e ds                  inz
012300090520
012400090520       //--------------------------------------------------------------
012500090520       //?Definizione variabili globali.                               ?
012600090520       //--------------------------------------------------------------
012700090520
012800090520       // - Flags booleani
012900090520     d $Fine           s               n   inz
013000090520     d $InzS01         s               n   inz(*on)
013100090520     d $Called         s               n   inz
013200090520     d $RecOK          s               n   inz
013300090521
013400090521       // - Parametri di input facoltativi
013500100504     d p_TNTB81ds      s                   like(TNTB81ds)
013600090520
013700090520       // - Variabili per la gestione del video
013800090520     d $Video          s              2    inz('S1')
013900090520     d W_SflPag        s              3  0 inz
014000090520     d wPag            s              4  0 inz
014100090520     d wRig            s              3  0 inz
014200090521     d S01nrr          s                   like(C1RcdNbr) inz
014300090521     d SavS01csr       s                   like(C1RcdNbr) inz
014400090520
014500090520       //--------------------------------------------------------------
014600090520       //?Definizione prototipi procedure.                             ?
014700090520       //--------------------------------------------------------------
014800090520
014900090520       // - Reperimento dati utente
015000090520      /copy gaitrasrc/srcProtoPR,TIBS34R
015100090520
015200090520       //--------------------------------------------------------------
015300090520       //?Definizione key-list.                                        ?
015400090520       //--------------------------------------------------------------
015500090520
015600090520       // - File TNTBE01L
015700090520     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
015800090520     d                                     prefix(k_)   inz
015900090520
016000090520       //--------------------------------------------------------------
016100090520       //?Riepilogo indicatori utilizzati.                             ?
016200090520       //--------------------------------------------------------------
016300090520       //--------------------------------------------------------------
016400090520
016500090520       //--------------------------------------------------------------
016600090520       //?M A I N - L I N E                                            ?
016700090520       //--------------------------------------------------------------
016800090520
016900090520     c     *Entry        plist
017000090520     c                   parm                    KPJBA
017100100504     c                   parm                    p_TNTB81ds
017200090520
017300090520      /free
017400090520
017500090520       //?Operazioni iniziali?
017600090520       exsr sr_RoutInz;
017700090520
017800090520       //?Ciclo di gestione del file video?
017900090520       DoW  $Fine = *off;
018000090520         select;
018100090520           // - Gestione videata S1 (subfile)
018200090520           when $Video = 'S1';
018300090520             exsr sr_GesS01;
018400090520           other;
018500090520             $Fine = *on;
018600090520         endsl;
018700090520       enddo;
018800090520
018900090520       //?Operazioni finali?
019000090520       exsr sr_RoutEnd;
019100090520
019200090521       //--------------------------------------------------------------
019300090521       //?*InzSR.                                                      ?
019400090521       //--------------------------------------------------------------
019500090521       BEGSR  *InzSr;
019600090521
019700090521         //?Reperimento dati job?
019800090521         exsr  sr_DatiJob;
019900090521
020000090521         //?Impostazione nome programma a video?
020100090521         V1Tpgm = SDSpgm;
020200090521
020300090521       ENDSR;
020400090521
020500090521       //--------------------------------------------------------------
020600090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
020700090521       //--------------------------------------------------------------
020800090521       BEGSR  sr_DatiJob;
020900090521
021000090521         in(e) §AzUte;
021100090521         if NOT %error;
021200090521           in(e) §DatiUte;
021300090521         endif;
021400090521         if %error or RSut = *blank;
021500090521           tibs34r ( tibs34ds );
021600090521           in §AzUte;
021700090521           in §DatiUte;
021800090521         endif;
021900090521
022000090521       ENDSR;
022100090521
022200090520       //--------------------------------------------------------------
022300090520       //?Operazioni iniziali.                                         ?
022400090520       //--------------------------------------------------------------
022500090520       BEGSR  sr_RoutInz;
022600090520
022700090520         //?Verifica se ricevuti parametri?
022800090521         if  SDSprm > 1;
022900100504           TNTB81ds = p_TNTB81ds;
023000090520           $Called  = *on;
023100090520         else;
023200100504           clear  TNTB81ds;
023300090520           $Called  = *off;
023400090520         endif;
023500090520
023600090520         //?Impostazione chiusura?
023700090520         select;
023800100504           when  $Called = *on   and   iTTAtla = *blank;
023900090520             *inRT = *on;
024000100504           when  $Called = *on   and   iTTAtla = 'L';
024100090520             *inLR = *on;
024200100504           when  $Called = *on   and   iTTAtla = 'C';
024300090520             *inLR = *on;
024400090520             $Fine = *on;
024500090520             leavesr;
024600090520           other;
024700090520             *inLR = *on;
024800090520         endsl;
024900090520
025000090520         //?Reperimento dati job?
025100090520         exsr  sr_DatiJob;
025200090520
025300090520         //?Impostazione nome programma a video?
025400090520         V1Tpgm = SDSpgm;
025500090520
025600090520         //?Impostazione iniziale / pulizia dei campi chiave?
025700090520         k_TBEcod = c_Tab;
025800090520         clear  k_TBEke1;
025900090520         clear  k_TBEke2;
026000090520         clear  k_TBElin;
026100090520         clear  k_TBEsif;
026200090521
026300090521         //?Impostazione iniziale parametri di output?
026400100504         clear  oTTAke1;
026500100504         clear  oTTAuni;
026600100504         clear  oTTAfxx;
026700100504         oTTAerr = *off;
026800100504         clear  oTTAmsg;
026900090521
027000090521         //?Re-impostazione flags booleani (fosse mai il 2° richiamo)?
027100090521         reset  $Fine;
027200090521         reset  $Video;
027300090521         reset  $InzS01;
027400090520
027500090520       ENDSR;
027600090520
027700090520       //--------------------------------------------------------------
027800090520       //?Gestione subfile S01.                                        ?
027900090520       //--------------------------------------------------------------
028000090520       BEGSR  sr_GesS01;
028100090520
028200090520         //?Inizializzazione videata?
028300090520         if  $InzS01 = *on;
028400090520           exsr  sr_InzS01;
028500090520           $InzS01 = *off;
028600090520         endif;
028700090520
028800090520         //?Emissione Testata & Piede?
028900100504         write  TB811T01;
029000100504         write  TB811P01;
029100090520
029200090520         //?Posizionamento cursore?
029300090520         if  C1CsrRrn > *zero;
029400090520           C1RcdNbr = C1CsrRrn;
029500090520         else;
029600090520           C1RcdNbr = 1;
029700100504           write  TB811S00;             // (no rec.)
029800090520         endif;
029900090520
030000090520         //?Emissione videata?
030100100504         exfmt  TB811C01;
030200090520
030300090520         reset  ErrMessage;
030400090520         reset  ErrGenerico;
030500090520         clear  V1Dmsg;
030600090520         SELECT;
030700090520
030800090520           //?F3=Fine?
030900090520           WHEN  dsp_aid = c_F03;
031000100504             oTTAfxx = '1';
031100090520             $Fine = *on;
031200090520
031300090520           //?F5=Rivisualizza?
031400090520           WHEN  dsp_aid = c_F05;
031500090520             $InzS01 = *on;
031600090520
031700090520           //?F12=Ritorno?
031800090520           WHEN  dsp_aid = c_F12;
031900100504             oTTAfxx = '2';
032000090520             $Fine = *on;
032100090520
032200090520           //?Roll-Up?
032300090520           WHEN  dsp_aid = c_RollUp;
032400090520             exsr sr_RollUpS01;
032500090520
032600090520           //?SubFile vuoto?
032700090520           WHEN  S01nrr = *zero;
032800090520
032900090520           //?Invio?
033000090520           OTHER;
033100090520             exsr  sr_OpzS01;
033200090520
033300090520         ENDSL;
033400090520
033500090520       ENDSR;
033600090520
033700090520       //--------------------------------------------------------------
033800090520       //?Inizializzazione subfile S01.                                ?
033900090520       //--------------------------------------------------------------
034000090520       BEGSR  sr_InzS01;
034100090520
034200090520         //?Spegnimento degli indicatori di errore?
034300090520         %subst(IndDspF : 50) = *off;
034400090520
034500090520         //?Settaggio indicatori dei tasti funzionali?
034600090520         exsr  sr_SetIndP01;
034700090520
034800090520         //?Pulizia subfile?
034900090520         SflDsp_N    = *on;
035000090520         SflDspCtl_N = *on;
035100100504         write  TB811C01;
035200090520         SflDspCtl_N = *off;
035300090520         SflEnd      = *off;
035400090520
035500090520         clear  W_SflPag;
035600090520         clear  C1RcdNbr;
035700090520         clear  C1CsrRrn;
035800090520         clear  S01nrr;
035900090520         clear  V1Dmsg;
036000090520         ErrMessage  = *off;
036100090520         ErrGenerico = *off;
036200090520
036300090520         //?Caricamento della 1ª pagina di dati nel subfile?
036400090520         clear  k05tntbe01;
036500090520         k_TBEcod = c_Tab;
036600090520         setll  %kds(k05tntbe01)  TNTBE000;
036700090520         reade  %kds(k05tntbe01 : 1)  TNTBE000;
036800090520         exsr  sr_RollUpS01;
036900090520
037000090520         //?Impaginazione subfile?
037100090520         // -> forza l'impaginazione sul 1° rec. del subfile
037200090520         if S01nrr > *zero;
037300090520           C1RcdNbr  = 1;
037400090520           C1CsrRrn  = 1;
037500090520         else;
037600090520           clear C1RcdNbr;
037700090520         endif;
037800090520
037900090520       ENDSR;
038000090520
038100090520       //--------------------------------------------------------------
038200090520       //?Caricamento singola pagina nel subfile S01                   ?
038300090520       //--------------------------------------------------------------
038400090520       BEGSR  sr_RollUpS01;
038500090520
038600090520         S01nrr    = (W_SflPag * c_SflPag);
038700090520         W_SflPag += 1;
038800090520         SflNxtChg = *off;
038900090520
039000090520         //?Ciclo di caricamento dati da tab. "CCO"?
039100090520         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
039200090520              S01nrr    < c_MaxRec                and
039300090520              Not %eof(TNTBE01L);
039400090520
039500090520           // -?Selezione singolo record file TNTBE00F?
039600090520           exsr  sr_SelezRec;
039700090520
039800090520           // -?Registrazione singolo record nel subfile?
039900090520           if  $RecOK;
040000090520
040100100504             clear  TB811S01;
040200100504             dTTA = TBEuni;
040300090520             S01cod = TBEke1;
040400100504             S1huni = dtta;
040500100504             S01des = §TTAdes;
040600100504             S01leg = §TTAleg;
040700090520
040800090520             S01nrr += 1;
040900100504             write  TB811S01;
041000090520
041100090520           EndIf;
041200090520
041300090520           // -?Lettura record successivo?
041400090520           reade  %kds(k05tntbe01 : 1)  TNTBE000;
041500090520
041600090520         EndDo;
041700090520
041800090520         //?Visualizzazione del SFL (se ci sono dati)?
041900090520         SflDsp_N  = (S01nrr <= *zero);
042000090520
042100090520         //?Attivazione del SFLEND?
042200090520         SflEnd    = (S01nrr >= c_MaxRec   or   %eof(TNTBE01L));
042300090520
042400090520         //?Posizionamento cursore al 1° rcd della pagina?
042500090520         if  S01nrr > *zero;
042600090520           wPag     = S01nrr / c_SflPag;
042700090520           wRig     = S01nrr - (c_SflPag * wPag);
042800090520           C1RcdNbr = wPag * c_SflPag;
042900090520           if  wRig > *zero;
043000090520             C1RcdNbr += 1;
043100090520           else;
043200090520             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
043300090520           endif;
043400090520         else;
043500090520           clear  C1RcdNbr;
043600090520         endif;
043700090520
043800090520         C1CsrRrn = C1RcdNbr;
043900090520
044000090520       ENDSR;
044100090520
044200090520       //--------------------------------------------------------------
044300090520       //?Settaggio indicatori utilizzati in C01/P01.                  ?
044400090520       //--------------------------------------------------------------
044500090520       BEGSR  sr_SetIndP01;
044600090520
044700090520         //?F3=Fine?
044800090520         F3Attivo  = (Not $Called);
044900090520
045000090520         //?F6=Inserimento?
045100090520         //F6Attivo  = (Not $Called);
045200090520
045300090520         //?F12=Ritorno?
045400090520         F12Attivo  = ($Called);
045500090520
045600090520       ENDSR;
045700090520
045800090520       //--------------------------------------------------------------
045900090520       //?Selezione del singolo record letto dalla tabella "CCO".      ?
046000090520       //--------------------------------------------------------------
046100090520       BEGSR  sr_SelezRec;
046200090520
046300090520         reset  $RecOK;
046400090520
046500100504         dTTA = TBEuni;
046600090520
046700090520         select;
046800090520
046900090520           // -?Omissione records annullati?
047000090520           when  TBEatb <> *blank;
047100090520             leavesr;
047200100421
047300100504           // -?Selezione attività legata a cliente?
047400100504           when  iTTAleg = 'S' and iTTAleg <> §TTAleg;
047500090520             leavesr;
047600091218
047700100504           // -?Selezione attività NON legate a cliente?
047800100504           when  iTTAleg = 'N' and  §TTAleg <> *blanks;
047900091218             leavesr;
048000091218
048100090520         endsl;
048200090520
048300090520         $RecOK = *on;
048400090520
048500090520       ENDSR;
048600090520
048700090520       //--------------------------------------------------------------
048800090520       //?Gestione opzioni subfile S01.                                ?
048900090520       //--------------------------------------------------------------
049000090520       BEGSR  sr_OpzS01;
049100090520
049200090520         clear  SavS01csr;
049300090520
049400090520         //?Ciclo di lettura subfile?
049500100504         readc  TB811S01;
049600090520
049700100504         DoW  Not %eof(TNTB81D1);
049800090520
049900090520           SflNxtChg = *off;
050000090520           %subst(IndDspF : 50) = *off;
050100090520           C1RcdNbr = S01nrr;
050200090520
050300090520           select;
050400090520
050500090520             // -?Opz. 1=Selezione?
050600090520             when  S01opz = '1'   and   Not $Fine;
050700100504               clear  dTTA;
050800100504               dTTA    = S1huni;
050900100504               oTTAke1 = S01cod;
051000100504               oTTAuni = dTTA;
051100090520               $Fine   = *on;
051200090520
051300090520             // -?Opz. 1=Selezione già reperita?
051400090520             //  ?(ammessa una sola selezione)?
051500090520             when  S01opz = '1'   and   $Fine;
051600090520               ErrMessage  = *on;
051700090520               ErrGenerico = *on;
051800090520               PosCurOPZ   = *on;
051900090520               V1Dmsg = $Msg(02);
052000090520               $Fine  = *off;
052100090520
052200090520             // -?Nessuna opzione?
052300090520             when  S01opz = *blank;
052400090520
052500090520             // -?Opzione errata?
052600090520             other;
052700090520               ErrMessage  = *on;
052800090520               ErrGenerico = *on;
052900090520               PosCurOPZ   = *on;
053000090520               V1Dmsg = $Msg(01);
053100090520               $Fine  = *off;
053200090520
053300090520           endsl;
053400090520
053500090520           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
053600090520           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
053700090520           if  S01opz  <> *blank   and
053800090520              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
053900090520             SavS01csr   = C1RcdNbr;
054000090520           endif;
054100090520
054200090520           // -?Aggiornamento sfl?
054300090520           select;
054400090520             when  ErrMessage;
054500090520               SflNxtChg = *on;
054600090520               C1CsrRrn  = C1RcdNbr;
054700090520             when  ErrGenerico;
054800090520               C1CsrRrn  = C1RcdNbr;
054900090520               if  S01opz <> '1';
055000090520                 clear  S01opz;
055100090520               endif;
055200090520             other;
055300090520               if  S01opz <> '1';
055400090520                 clear  S01opz;
055500090520               endif;
055600090520           endsl;
055700090520
055800090520           if  S01opz <> *blank;
055900090520             SflNxtChg = *on;
056000090520           endif;
056100090520
056200100504           UPDATE  TB811S01;
056300090520
056400090520           if  ErrGenerico   or   ErrMessage;
056500090520             leave;
056600090520           endif;
056700090520
056800100504           readc  TB811S01;
056900090520
057000090520         ENDDO;
057100090520
057200090520         // -?Riposiziono il cursore sul record contenente la prima opz.?
057300090520         //  ?(se non sono stati rilevati errori)?
057400090520         if  NOT ErrMessage   and   NOT ErrGenerico   and
057500090520             SavS01csr > *zero;
057600090520           C1CsrRrn = SavS01csr;
057700090520         endif;
057800090520
057900090520       ENDSR;
058000090520
058100090520       //--------------------------------------------------------------
058200090520       //?Operazioni finali.                                           ?
058300090520       //--------------------------------------------------------------
058400090520       BEGSR  sr_RoutEnd;
058500090521
058600090521         //?Ritorno parametri?
058700090521         if  SDSprm > 1;
058800100504           p_TNTB81ds = TNTB81ds;
058900090521         endif;
059000090520
059100090520         //?"Chiusura" pgm?
059200090520         return;
059300090520
059400090520       ENDSR;
059500090520
059600090520      /end-free
059700090520
059800090520       //--------------------------------------------------------------
059900090520       //?Schiere a tempo di compilazione.                             ?
060000090520       //--------------------------------------------------------------
060100090520
060200090520** - $Msg -------------------------------------------------------------------*
060300090520Opzione errata                                                                 1
060400100504Ammessa la selezione di un solo tipo attività                                  2
