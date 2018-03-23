000100100519      *PARMS dftactgrp(*no) actgrp(*caller) dbgview(*source)
000200100519     /*PRM dftactgrp(*no) actgrp(*caller) dbgview(*source)
000300100519     /*END
000400090520       //--------------------------------------------------------------
000500090520       //?TNTB74R * Selezione tab. "CCO" = Causali Attività            ?
000600090520       //--------------------------------------------------------------
000700090520
000800090520     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900090520     h dftactgrp(*no) actgrp(*caller)
001000090520
001100090520       //--------------------------------------------------------------
001200090520       //?Dichiarazione file.                                          ?
001300090520       //--------------------------------------------------------------
001400090520
001500090520       // - Tabelle
001600100309     fTNTBE01L  if   e           k disk
001700090520
001800090520       // - Video
001900090520     fTNTB74D   cf   e             workstn
002000090520     f                                     sfile(TB74S01 : S01nrr)
002100090520     f                                     indds(IndDspF)
002200090520     f                                     infds(InfDspF)
002300090520
002400090520       //--------------------------------------------------------------
002500090520       //?Definizione costanti.                                        ?
002600090520       //--------------------------------------------------------------
002700090520
002800090520       // - Codice tabella in gestione
002900090520     d c_Tab           c                   const('CCO')
003000090520
003100090520       // - Numero di record in ciascuna videata di subfile
003200090520     d c_SflPag        c                   const(16)
003300090520
003400090520       // - Numero massimo di record gestibili
003500110331     d c_MaxRec        c                   const(9999)
003600090520
003700090520       // - Tasti funzionali a video
003800090520     d c_F01           c                   const(x'31')
003900090520     d c_F02           c                   const(x'32')
004000090520     d c_F03           c                   const(x'33')
004100100504     d c_F04           c                   const(x'34')
004200090520     d c_F05           c                   const(x'35')
004300090520     d c_F06           c                   const(x'36')
004400090520     d c_F07           c                   const(x'37')
004500090520     d c_F08           c                   const(x'38')
004600090520     d c_F09           c                   const(x'39')
004700090520     d c_F10           c                   const(x'3A')
004800100504     d c_F11           c                   const(x'3B')
004900090520     d c_F12           c                   const(x'3C')
005000090520     d c_F13           c                   const(x'B1')
005100090520     d c_F14           c                   const(x'B2')
005200090520     d c_F15           c                   const(x'B3')
005300090520     d c_F16           c                   const(x'B4')
005400090520     d c_F17           c                   const(x'B5')
005500090520     d c_F18           c                   const(x'B6')
005600090520     d c_F19           c                   const(x'B7')
005700090520     d c_F20           c                   const(x'B8')
005800090520     d c_F21           c                   const(x'B9')
005900090520     d c_F22           c                   const(x'BA')
006000090520     d c_F23           c                   const(x'BB')
006100090520     d c_F24           c                   const(x'BC')
006200090520     d c_Enter         c                   const(x'F1')
006300090520     d c_RollDown      c                   const(x'F4')
006400090520     d c_RollUp        c                   const(x'F5')
006500090520
006600090520       //--------------------------------------------------------------
006700090520       //?Definizione schiere.                                         ?
006800090520       //--------------------------------------------------------------
006900090520
007000090520       // - Messaggi di errore
007100090520     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
007200090520
007300090520       //--------------------------------------------------------------
007400090520       //?Definizione aree dati.                                       ?
007500090520       //--------------------------------------------------------------
007600090520
007700090520       // - Dati utente
007800090520     d §AzUte        e ds                  extname(AZUTE00F)
007900090520     d                                     dtaara
008000090520     d §DatiUte      e ds                  extname(dDatiUte)
008100090520     d                                     dtaara
008200090520
008300090520       //--------------------------------------------------------------
008400090520       //?Definizione strutture dati.                                  ?
008500090520       //--------------------------------------------------------------
008600090520
008700090520       // - Status ds
008800090520     d Status         sds
008900090520     d  SDSpgm           *proc
009000090521     d  SDSprm           *parms
009100090520
009200090520       // - InfDS
009300090520     d InfDspF         ds
009400090520     d   dsp_aid             369    369a
009500090520     d   sfl_rrn             376    377i 0
009600090520     d   min_nrr             378    379i 0
009700090520     d   num_rcds            380    381i 0
009800090520
009900090520       // - Indicatori su DspF
010000090520     d IndDspF         ds                  inz
010100090520         // - Abilitazione tasti funzionali
010200090520     d   F3Attivo                      n   overlay(IndDspF : 03)
010300090520     d   F6Attivo                      n   overlay(IndDspF : 06)
010400090520     d   F12Attivo                     n   overlay(IndDspF : 12)
010500090520         // - Emissione messaggio di errore
010600090520     d   ErrMessage                    n   overlay(IndDspF : 28)
010700090520         // - Indicatori di gestione del subfile
010800090520     d   SflDsp_N                      n   overlay(IndDspF : 30)
010900090520     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011000090520     d   SflNxtChg                     n   overlay(IndDspF : 32)
011100090520     d   SflEnd                        n   overlay(IndDspF : 33)
011200090520         // - Indicatori per Attibuti di visualizzazione
011300090520         // - Posizionamento cursore & segnalazione errore
011400090520     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011500090520         //   - Riemissione videata
011600090520     d   ErrGenerico                   n   overlay(IndDspF : 99)
011700090520
011800100219
011900100219       // - Tipi attività possibili per ogni Causale Attività
012000100219     d §ccoTAAds       ds            10    inz
012100100219     d   $Taa                  1     10    inz  dim(10)
012200100504
012300100504       // - Tipi trattativa abilitati per ogni Causale Attività
012400101214     d §ccoTPVds       ds            10    inz
012500101214     d   $Tpv                         1    inz  dim(10)
012600090520
012700110429       // - Categoria potenziali abilitatate
012800110429     d §ccoCPOds       ds             4    inz
012900110429     d   $Cpo                         1    inz  dim(4)
013000110429
013100090520       // - Parametri ricevuti
013200090520     d KPJBA         e ds
013300090520     d TNTB74ds      e ds                  inz
013400090520
013500090520       // - Parametri per Reperimento dati utente
013600090525     d TIBS34ds      e ds                  inz
013700090520
013800090520       // - Tab. CCO = Causali Attività marketing
013900090520     d dCCO          e ds                  inz
014000090520
014100110405      // - Ricerca/Controllo tabelle
014200110405     d TIBS02ds      e ds                  inz
014300110405
014400090520       //--------------------------------------------------------------
014500090520       //?Definizione variabili globali.                               ?
014600090520       //--------------------------------------------------------------
014700090520
014800090520       // - Flags booleani
014900090520     d $Fine           s               n   inz
015000090520     d $InzS01         s               n   inz(*on)
015100090520     d $Called         s               n   inz
015200090520     d $RecOK          s               n   inz
015300110405     d $Cli_non_sped   s               n   inz
015400090521
015500090521       // - Parametri di input facoltativi
015600090521     d p_TNTB74ds      s                   like(TNTB74ds)
015700090520
015800090520       // - Variabili per la gestione del video
015900090520     d $Video          s              2    inz('S1')
016000090520     d W_SflPag        s              3  0 inz
016100090520     d wPag            s              4  0 inz
016200090520     d wRig            s              3  0 inz
016300090521     d S01nrr          s                   like(C1RcdNbr) inz
016400090521     d SavS01csr       s                   like(C1RcdNbr) inz
016500090520
016600090520       //--------------------------------------------------------------
016700090520       //?Definizione prototipi procedure.                             ?
016800090520       //--------------------------------------------------------------
016900090520
017000090520       // - Reperimento dati utente
017100090520      /copy gaitrasrc/srcProtoPR,TIBS34R
017200110405
017300110405      /copy gaitrasrc/srcprotopr,tibs02r
017400090520
017500090520       //--------------------------------------------------------------
017600090520       //?Definizione key-list.                                        ?
017700090520       //--------------------------------------------------------------
017800090520
017900090520       // - File TNTBE01L
018000090520     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018100090520     d                                     prefix(k_)   inz
018200090520
018300090520       //--------------------------------------------------------------
018400090520       //?Riepilogo indicatori utilizzati.                             ?
018500090520       //--------------------------------------------------------------
018600090520       //--------------------------------------------------------------
018700090520
018800090520       //--------------------------------------------------------------
018900090520       //?M A I N - L I N E                                            ?
019000090520       //--------------------------------------------------------------
019100090520
019200090520     c     *Entry        plist
019300090520     c                   parm                    KPJBA
019400090521     c                   parm                    p_TNTB74ds
019500090520
019600090520      /free
019700090520
019800090520       //?Operazioni iniziali?
019900090520       exsr sr_RoutInz;
020000090520
020100090520       //?Ciclo di gestione del file video?
020200090520       DoW  $Fine = *off;
020300090520         select;
020400090520           // - Gestione videata S1 (subfile)
020500090520           when $Video = 'S1';
020600090520             exsr sr_GesS01;
020700090520           other;
020800090520             $Fine = *on;
020900090520         endsl;
021000090520       enddo;
021100090520
021200090520       //?Operazioni finali?
021300090520       exsr sr_RoutEnd;
021400090520
021500090521       //--------------------------------------------------------------
021600090521       //?*InzSR.                                                      ?
021700090521       //--------------------------------------------------------------
021800090521       BEGSR  *InzSr;
021900090521
022000090521         //?Reperimento dati job?
022100090521         exsr  sr_DatiJob;
022200090521
022300090521         //?Impostazione nome programma a video?
022400090521         V1Tpgm = SDSpgm;
022500090521
022600090521       ENDSR;
022700090521
022800090521       //--------------------------------------------------------------
022900090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
023000090521       //--------------------------------------------------------------
023100090521       BEGSR  sr_DatiJob;
023200090521
023300090521         in(e) §AzUte;
023400090521         if NOT %error;
023500090521           in(e) §DatiUte;
023600090521         endif;
023700090521         if %error or RSut = *blank;
023800090521           tibs34r ( tibs34ds );
023900090521           in §AzUte;
024000090521           in §DatiUte;
024100090521         endif;
024200090521
024300090521       ENDSR;
024400090521
024500090520       //--------------------------------------------------------------
024600090520       //?Operazioni iniziali.                                         ?
024700090520       //--------------------------------------------------------------
024800090520       BEGSR  sr_RoutInz;
024900090520
025000090520         //?Verifica se ricevuti parametri?
025100090521         if  SDSprm > 1;
025200090521           TNTB74ds = p_TNTB74ds;
025300090520           $Called  = *on;
025400090520         else;
025500090520           clear  TNTB74ds;
025600090520           $Called  = *off;
025700090520         endif;
025800090520
025900090520         //?Impostazione chiusura?
026000090520         select;
026100090520           when  $Called = *on   and   iCCOtla = *blank;
026200090520             *inRT = *on;
026300090520           when  $Called = *on   and   iCCOtla = 'L';
026400090520             *inLR = *on;
026500090520           when  $Called = *on   and   iCCOtla = 'C';
026600090520             *inLR = *on;
026700090520             $Fine = *on;
026800090520             leavesr;
026900090520           other;
027000090520             *inLR = *on;
027100090520         endsl;
027200090520
027300090520         //?Reperimento dati job?
027400090520         exsr  sr_DatiJob;
027500090520
027600090520         //?Impostazione nome programma a video?
027700090520         V1Tpgm = SDSpgm;
027800090520
027900090520         //?Impostazione iniziale / pulizia dei campi chiave?
028000090520         k_TBEcod = c_Tab;
028100090520         clear  k_TBEke1;
028200090520         clear  k_TBEke2;
028300090520         clear  k_TBElin;
028400090520         clear  k_TBEsif;
028500090521
028600090521         //?Impostazione iniziale parametri di output?
028700090521         clear  oCCOke1;
028800091218         clear  oCCOuni;
028900090521         clear  oCCOfxx;
029000090521         oCCOerr = *off;
029100090521         clear  oCCOmsg;
029200090521
029300090521         //?Re-impostazione flags booleani (fosse mai il 2° richiamo)?
029400090521         reset  $Fine;
029500090521         reset  $Video;
029600090521         reset  $InzS01;
029700110428         reset  $Cli_non_sped ;
029800110405
029900110405         //?verifico se passata una causale se si trattta di una causle su cliente che non spedisce
030000110405         If iccocco <> '  ' ;
030100110405            clear dCCO;
030200110405            clear TIBS02ds;
030300110405            T02mod = 'C';
030400110405            T02cod = 'CCO';
030500110405            T02ke1 = Iccocco  ;
030600110405            T02sif = KNSIF;
030700110405            TNTBE_RicercaControllo  (kpjba : tibs02ds);
030800110405            IF  T02err = *blanks;
030900110405              dCCO = t02uni ;
031000110405              If §ccocns = 'S' ;
031100110405                 $Cli_non_sped = *on ;
031200110405              Endif ;
031300110405            ENDIF;
031400110405         Endif ;
031500090520
031600090520       ENDSR;
031700090520
031800090520       //--------------------------------------------------------------
031900090520       //?Gestione subfile S01.                                        ?
032000090520       //--------------------------------------------------------------
032100090520       BEGSR  sr_GesS01;
032200090520
032300090520         //?Inizializzazione videata?
032400090520         if  $InzS01 = *on;
032500090520           exsr  sr_InzS01;
032600090520           $InzS01 = *off;
032700090520         endif;
032800090520
032900090520         //?Emissione Testata & Piede?
033000090520         write  TB74T01;
033100090520         write  TB74P01;
033200090520
033300090520         //?Posizionamento cursore?
033400090520         if  C1CsrRrn > *zero;
033500090520           C1RcdNbr = C1CsrRrn;
033600090520         else;
033700090520           C1RcdNbr = 1;
033800090520           write  TB74S00;             // (no rec.)
033900090520         endif;
034000090520
034100090520         //?Emissione videata?
034200090520         exfmt  TB74C01;
034300090520
034400090520         reset  ErrMessage;
034500090520         reset  ErrGenerico;
034600090520         clear  V1Dmsg;
034700090520         SELECT;
034800090520
034900090520           //?F3=Fine?
035000090520           WHEN  dsp_aid = c_F03;
035100090520             oCCOfxx = '1';
035200090520             $Fine = *on;
035300090520
035400090520           //?F5=Rivisualizza?
035500090520           WHEN  dsp_aid = c_F05;
035600090520             $InzS01 = *on;
035700090520
035800090520           //?F12=Ritorno?
035900090520           WHEN  dsp_aid = c_F12;
036000090520             oCCOfxx = '2';
036100090520             $Fine = *on;
036200090520
036300090520           //?Roll-Up?
036400090520           WHEN  dsp_aid = c_RollUp;
036500090520             exsr sr_RollUpS01;
036600090520
036700090520           //?SubFile vuoto?
036800090520           WHEN  S01nrr = *zero;
036900090520
037000090520           //?Invio?
037100090520           OTHER;
037200090520             exsr  sr_OpzS01;
037300090520
037400090520         ENDSL;
037500090520
037600090520       ENDSR;
037700090520
037800090520       //--------------------------------------------------------------
037900090520       //?Inizializzazione subfile S01.                                ?
038000090520       //--------------------------------------------------------------
038100090520       BEGSR  sr_InzS01;
038200090520
038300090520         //?Spegnimento degli indicatori di errore?
038400090520         %subst(IndDspF : 50) = *off;
038500090520
038600090520         //?Settaggio indicatori dei tasti funzionali?
038700090520         exsr  sr_SetIndP01;
038800090520
038900090520         //?Pulizia subfile?
039000090520         SflDsp_N    = *on;
039100090520         SflDspCtl_N = *on;
039200090520         write  TB74C01;
039300090520         SflDspCtl_N = *off;
039400090520         SflEnd      = *off;
039500090520
039600090520         clear  W_SflPag;
039700090520         clear  C1RcdNbr;
039800090520         clear  C1CsrRrn;
039900090520         clear  S01nrr;
040000090520         clear  V1Dmsg;
040100090520         ErrMessage  = *off;
040200090520         ErrGenerico = *off;
040300090520
040400090520         //?Caricamento della 1ª pagina di dati nel subfile?
040500090520         clear  k05tntbe01;
040600090520         k_TBEcod = c_Tab;
040700090520         setll  %kds(k05tntbe01)  TNTBE000;
040800090520         reade  %kds(k05tntbe01 : 1)  TNTBE000;
040900090520         exsr  sr_RollUpS01;
041000090520
041100090520         //?Impaginazione subfile?
041200090520         // -> forza l'impaginazione sul 1° rec. del subfile
041300090520         if S01nrr > *zero;
041400090520           C1RcdNbr  = 1;
041500090520           C1CsrRrn  = 1;
041600090520         else;
041700090520           clear C1RcdNbr;
041800090520         endif;
041900090520
042000090520       ENDSR;
042100090520
042200090520       //--------------------------------------------------------------
042300090520       //?Caricamento singola pagina nel subfile S01                   ?
042400090520       //--------------------------------------------------------------
042500090520       BEGSR  sr_RollUpS01;
042600090520
042700090520         S01nrr    = (W_SflPag * c_SflPag);
042800090520         W_SflPag += 1;
042900090520         SflNxtChg = *off;
043000090520
043100090520         //?Ciclo di caricamento dati da tab. "CCO"?
043200090520         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
043300090520              S01nrr    < c_MaxRec                and
043400090520              Not %eof(TNTBE01L);
043500090520
043600090520           // -?Selezione singolo record file TNTBE00F?
043700090520           exsr  sr_SelezRec;
043800090520
043900090520           // -?Registrazione singolo record nel subfile?
044000090520           if  $RecOK;
044100090520
044200090520             clear  TB74S01;
044300090520             dCCO = TBEuni;
044400090520             S01cod = TBEke1;
044500091217             S1huni = dcco;
044600090520             S01des = §CCOdes;
044700090520             S01gio = §CCOgio;
044800100924             S01upm = §CCOupm;
044900090520             S1Hstc = §CCOstc;
045000090629             S01tat = §CCOtat;
045100100408             IF  §CCOapt = 'S';
045200100408               S01apt = 'SI';
045300100408             ENDIF;
045400100519             if  §CCOcht <> *blank;
045500100519               S01cht = 'SI';
045600100519             endif;
045700090520
045800090520             S01nrr += 1;
045900090520             write  TB74S01;
046000090520
046100090520           EndIf;
046200090520
046300090520           // -?Lettura record successivo?
046400090520           reade  %kds(k05tntbe01 : 1)  TNTBE000;
046500090520
046600090520         EndDo;
046700090520
046800090520         //?Visualizzazione del SFL (se ci sono dati)?
046900090520         SflDsp_N  = (S01nrr <= *zero);
047000090520
047100090520         //?Attivazione del SFLEND?
047200090520         SflEnd    = (S01nrr >= c_MaxRec   or   %eof(TNTBE01L));
047300090520
047400090520         //?Posizionamento cursore al 1° rcd della pagina?
047500090520         if  S01nrr > *zero;
047600090520           wPag     = S01nrr / c_SflPag;
047700090520           wRig     = S01nrr - (c_SflPag * wPag);
047800090520           C1RcdNbr = wPag * c_SflPag;
047900090520           if  wRig > *zero;
048000090520             C1RcdNbr += 1;
048100090520           else;
048200090520             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
048300090520           endif;
048400090520         else;
048500090520           clear  C1RcdNbr;
048600090520         endif;
048700090520
048800090520         C1CsrRrn = C1RcdNbr;
048900090520
049000090520       ENDSR;
049100090520
049200090520       //--------------------------------------------------------------
049300090520       //?Settaggio indicatori utilizzati in C01/P01.                  ?
049400090520       //--------------------------------------------------------------
049500090520       BEGSR  sr_SetIndP01;
049600090520
049700090520         //?F3=Fine?
049800090520         F3Attivo  = (Not $Called);
049900090520
050000090520         //?F6=Inserimento?
050100090520         //F6Attivo  = (Not $Called);
050200090520
050300090520         //?F12=Ritorno?
050400090520         F12Attivo  = ($Called);
050500090520
050600090520       ENDSR;
050700090520
050800090520       //--------------------------------------------------------------
050900090520       //?Selezione del singolo record letto dalla tabella "CCO".      ?
051000090520       //--------------------------------------------------------------
051100090520       BEGSR  sr_SelezRec;
051200090520
051300090520         reset  $RecOK;
051400090520
051500090520         dCCO = TBEuni;
051600100219         §ccoTAAds = §CCOtaa;
051700100505         §ccoTPVds = §CCOtpv;
051800110429         §ccoCPOds = §CCOcpo;
051900090520
052000090520         select;
052100090520
052200090520           // -?Omissione records annullati?
052300090520           when  TBEatb <> *blank;
052400090520             leavesr;
052500090520
052600110428
052700110428           // -?Selezione per causale passata 32 causale 42?
052800110428              when  iCCOcco = '32' and tbeke1 <> '42             ';
052900110428               leavesr;
053000110428
053100110428
053200110428           // -?Selezione per causale passata 71 causale 72?
053300110428              when  iCCOcco = '71' and tbeke1 <> '72             ';
053400110428               leavesr;
053500110428
053600101214
053700110331           // -?Selezione per causale passata se non provengo da tntbe75r
053800110331           when §ccocae <> *blanks and Iccocco = *blanks
053900110331                   and %subst(kpjbu:1:7) <> 'TNTB75R' ;
054000110330             leavesr ;
054100110330
054200101214           // -?Selezione per causale passata in aggiunta alla precedente es 72
054300101214           when §ccocae <> *blanks and Iccocco <> *blanks and
054400101214                §ccocae <> Iccocco ;
054500101214             leavesr ;
054600100427
054700110405           // -?Selezione per causale passata e gestione delle sole causali su cliente che non spedi
054800110405           // -?sce
054900110406           when Iccocco <> *blanks and (($cli_non_sped = *on and
055000110406                §ccoucns = 'N') or ($cli_non_sped = *off and
055100110406                §ccoucns = 'S'));
055200110405             leavesr ;
055300110405
055400110406           // -?Selezione per causale passata e gestione delle sole causali su cliente che non spedi
055500110406           // -?sce
055600110406           when Iccocco <> *blanks and $cli_non_sped = *on and
055700110406                §ccoucns = 'N'  ;
055800110406             leavesr ;
055900110406
056000110502
056100110502           // -?Selezione per causale non passata e non TNTB75R escludo attività gestione causali su
056200110502           // -?cliente che non spedisce
056300110502           when Iccocco = *blanks and §ccoucns = 'S' and
056400110502                %subst(kpjbu:1:7) <> 'TNTB75R';
056500110502             leavesr ;
056600110502
056700100421
056800091218           // -?Selezione tipo attività associata?
056900091217           when  iCCOatt <> *blank   and   iCCOatt <> §CCOatt;
057000090520             leavesr;
057100091218
057200091218           // -?Selezione utilizzo causale attività?
057300091218           //   S=Sì seleziono solo i rcd a blank
057400091218           when  iCCOuti = 'S' and  §CCOuti <> *blanks;
057500091218             leavesr;
057600091218
057700091218           // -?Selezione utilizzo causale attività?
057800091218           //   N=No escludo i rcd a blank
057900091218           when  iCCOuti = 'N' and  §CCOuti = *blanks;
058000091218             leavesr;
058100100628
058200100628           // -?Selezione utilizzo causale attività?
058300100628           //   X=...Seleziono i rcd a blank +
058400100628           //                  i rcd a "N" con cod >= 14 e <= 99
058500100628           when  iCCOuti = 'X' and  (§CCOuti <> *blank  and
058600110708                (TBEke1  < '14 '  or  TBEke1 > '99 ' or TBEke1 = '50'));
058700100628             leavesr;
058800100219
058900100504           // -?Selezione utilizzo causale attività per esitare l'attività che ricevo in ds?
059000100219           //   schiera TAA = tipi attività abilitate tutto blank non faccio controllo
059100100219           when  iCCOese <> *blanks  and  §CCOtaa <> *blanks and
059200100219                 %lookup(iccoese : $Taa) = *zero ;
059300100219             leavesr;
059400100504
059500100504           // -?Selezione utilizzo tipo trattativa?
059600100504           //   schiera TPV = tipi trattativa (tutto blank => non faccio controllo)
059700100504           when  iCCOtpv <> *blanks  and  §CCOtpv <> *blanks and
059800100504                 %lookup(iCCOtpv : $Tpv) = *zero ;
059900100504             leavesr;
060000100322
060100110429           // -?Selezione categoria potenziale?
060200110429           //   schiera CPO = categoria potenziale (tutto blank => non faccio controllo)
060300110429           when  iCCOcod <> *blanks  and  §CCOcpo <> *blanks and
060400110429                 %lookup(iCCOcod : $Cpo) = *zero ;
060500110429             leavesr;
060600110429
060700110429           // -?Selezione codificato? obsoleto
060800110429           // when  §CCOcod <> *blank  and iccocod <> *blank and
060900110429           //       iCCOcod <> §ccocod ;
061000110429           //   leavesr;
061100100322
061200100504           // -?Selezione trattativa?
061300100322           when  §CCOtra = 'S' and §ccoapt <> 'S' and
061400100322                  iCCOtra = 'N' ;
061500100322             leavesr;
061600100322
061700100322           when  §CCOtra = 'N' and
061800100322                  iCCOtra = 'S' ;
061900100322             leavesr;
062000090520
062100100504           // -?Selezione causali con o senza offerte?
062200100324           when  §CCOoff = 'S' and
062300100324                  iCCOoff = 'N' ;
062400100324             leavesr;
062500100324           when  §CCOoff = 'N' and
062600100324                  iCCOoff = 'S' ;
062700100324             leavesr;
062800090520         endsl;
062900090520
063000090520         $RecOK = *on;
063100090520
063200090520       ENDSR;
063300090520
063400090520       //--------------------------------------------------------------
063500090520       //?Gestione opzioni subfile S01.                                ?
063600090520       //--------------------------------------------------------------
063700090520       BEGSR  sr_OpzS01;
063800090520
063900090520         clear  SavS01csr;
064000090520
064100090520         //?Ciclo di lettura subfile?
064200090520         readc  TB74S01;
064300090520
064400090520         DoW  Not %eof(TNTB74D);
064500090520
064600090520           SflNxtChg = *off;
064700090520           %subst(IndDspF : 50) = *off;
064800090520           C1RcdNbr = S01nrr;
064900090520
065000090520           select;
065100090520
065200090520             // -?Opz. 1=Selezione?
065300090520             when  S01opz = '1'   and   Not $Fine;
065400090520               clear  dCCO;
065500091217               dcco    = S1huni;
065600090520               oCCOke1 = S01cod;
065700090520               oCCOuni = dCCO;
065800090520               $Fine   = *on;
065900090520
066000090520             // -?Opz. 1=Selezione già reperita?
066100090520             //  ?(ammessa una sola selezione)?
066200090520             when  S01opz = '1'   and   $Fine;
066300090520               ErrMessage  = *on;
066400090520               ErrGenerico = *on;
066500090520               PosCurOPZ   = *on;
066600090520               V1Dmsg = $Msg(02);
066700090520               $Fine  = *off;
066800090520
066900090520             // -?Nessuna opzione?
067000090520             when  S01opz = *blank;
067100090520
067200090520             // -?Opzione errata?
067300090520             other;
067400090520               ErrMessage  = *on;
067500090520               ErrGenerico = *on;
067600090520               PosCurOPZ   = *on;
067700090520               V1Dmsg = $Msg(01);
067800090520               $Fine  = *off;
067900090520
068000090520           endsl;
068100090520
068200090520           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
068300090520           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
068400090520           if  S01opz  <> *blank   and
068500090520              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
068600090520             SavS01csr   = C1RcdNbr;
068700090520           endif;
068800090520
068900090520           // -?Aggiornamento sfl?
069000090520           select;
069100090520             when  ErrMessage;
069200090520               SflNxtChg = *on;
069300090520               C1CsrRrn  = C1RcdNbr;
069400090520             when  ErrGenerico;
069500090520               C1CsrRrn  = C1RcdNbr;
069600090520               if  S01opz <> '1';
069700090520                 clear  S01opz;
069800090520               endif;
069900090520             other;
070000090520               if  S01opz <> '1';
070100090520                 clear  S01opz;
070200090520               endif;
070300090520           endsl;
070400090520
070500090520           if  S01opz <> *blank;
070600090520             SflNxtChg = *on;
070700090520           endif;
070800090520
070900090520           UPDATE  TB74S01;
071000090520
071100090520           if  ErrGenerico   or   ErrMessage;
071200090520             leave;
071300090520           endif;
071400090520
071500090520           readc  TB74S01;
071600090520
071700090520         ENDDO;
071800090520
071900090520         // -?Riposiziono il cursore sul record contenente la prima opz.?
072000090520         //  ?(se non sono stati rilevati errori)?
072100090520         if  NOT ErrMessage   and   NOT ErrGenerico   and
072200090520             SavS01csr > *zero;
072300090520           C1CsrRrn = SavS01csr;
072400090520         endif;
072500090520
072600090520       ENDSR;
072700090520
072800090520       //--------------------------------------------------------------
072900090520       //?Operazioni finali.                                           ?
073000090520       //--------------------------------------------------------------
073100090520       BEGSR  sr_RoutEnd;
073200090521
073300090521         //?Ritorno parametri?
073400090521         if  SDSprm > 1;
073500090521           p_TNTB74ds = TNTB74ds;
073600090521         endif;
073700090520
073800090520         //?"Chiusura" pgm?
073900090520         return;
074000090520
074100090520       ENDSR;
074200090520
074300090520      /end-free
074400090520
074500090520       //--------------------------------------------------------------
074600090520       //?Schiere a tempo di compilazione.                             ?
074700090520       //--------------------------------------------------------------
074800090520
074900090520** - $Msg -------------------------------------------------------------------*
075000090520Opzione errata                                                                 1
075100090520Ammessa la selezione di una sola causale                                       2
