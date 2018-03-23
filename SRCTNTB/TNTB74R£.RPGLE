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
011800090520       // - Stati precedenti possibili per ogni Causale Attività
011900090520     d §ccoSTPds       ds            20    inz
012000090520     d   $StP                  1     20    inz  dim(10)
012100100219
012200100219       // - Tipi attività possibili per ogni Causale Attività
012300100219     d §ccoTAAds       ds            10    inz
012400100219     d   $Taa                  1     10    inz  dim(10)
012500100504
012600100504       // - Tipi trattativa abilitati per ogni Causale Attività
012700101214     d §ccoTPVds       ds            10    inz
012800101214     d   $Tpv                         1    inz  dim(10)
012900090520
013000110429       // - Categoria potenziali abilitatate
013100110429     d §ccoCPOds       ds             4    inz
013200110429     d   $Cpo                         1    inz  dim(4)
013300110429
013400090520       // - Parametri ricevuti
013500090520     d KPJBA         e ds
013600090520     d TNTB74ds      e ds                  inz
013700090520
013800090520       // - Parametri per Reperimento dati utente
013900090525     d TIBS34ds      e ds                  inz
014000090520
014100090520       // - Tab. CCO = Causali Attività marketing
014200090520     d dCCO          e ds                  inz
014300090520
014400110405      // - Ricerca/Controllo tabelle
014500110405     d TIBS02ds      e ds                  inz
014600110405
014700090520       //--------------------------------------------------------------
014800090520       //?Definizione variabili globali.                               ?
014900090520       //--------------------------------------------------------------
015000090520
015100090520       // - Flags booleani
015200090520     d $Fine           s               n   inz
015300090520     d $InzS01         s               n   inz(*on)
015400090520     d $Called         s               n   inz
015500090520     d $RecOK          s               n   inz
015600110405     d $Cli_non_sped   s               n   inz
015700090521
015800090521       // - Parametri di input facoltativi
015900090521     d p_TNTB74ds      s                   like(TNTB74ds)
016000090520
016100090520       // - Variabili per la gestione del video
016200090520     d $Video          s              2    inz('S1')
016300090520     d W_SflPag        s              3  0 inz
016400090520     d wPag            s              4  0 inz
016500090520     d wRig            s              3  0 inz
016600090521     d S01nrr          s                   like(C1RcdNbr) inz
016700090521     d SavS01csr       s                   like(C1RcdNbr) inz
016800090520
016900090520       //--------------------------------------------------------------
017000090520       //?Definizione prototipi procedure.                             ?
017100090520       //--------------------------------------------------------------
017200090520
017300090520       // - Reperimento dati utente
017400090520      /copy gaitrasrc/srcProtoPR,TIBS34R
017500110405
017600110405      /copy gaitrasrc/srcprotopr,tibs02r
017700090520
017800090520       //--------------------------------------------------------------
017900090520       //?Definizione key-list.                                        ?
018000090520       //--------------------------------------------------------------
018100090520
018200090520       // - File TNTBE01L
018300090520     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
018400090520     d                                     prefix(k_)   inz
018500090520
018600090520       //--------------------------------------------------------------
018700090520       //?Riepilogo indicatori utilizzati.                             ?
018800090520       //--------------------------------------------------------------
018900090520       //--------------------------------------------------------------
019000090520
019100090520       //--------------------------------------------------------------
019200090520       //?M A I N - L I N E                                            ?
019300090520       //--------------------------------------------------------------
019400090520
019500090520     c     *Entry        plist
019600090520     c                   parm                    KPJBA
019700090521     c                   parm                    p_TNTB74ds
019800090520
019900090520      /free
020000090520
020100090520       //?Operazioni iniziali?
020200090520       exsr sr_RoutInz;
020300090520
020400090520       //?Ciclo di gestione del file video?
020500090520       DoW  $Fine = *off;
020600090520         select;
020700090520           // - Gestione videata S1 (subfile)
020800090520           when $Video = 'S1';
020900090520             exsr sr_GesS01;
021000090520           other;
021100090520             $Fine = *on;
021200090520         endsl;
021300090520       enddo;
021400090520
021500090520       //?Operazioni finali?
021600090520       exsr sr_RoutEnd;
021700090520
021800090521       //--------------------------------------------------------------
021900090521       //?*InzSR.                                                      ?
022000090521       //--------------------------------------------------------------
022100090521       BEGSR  *InzSr;
022200090521
022300090521         //?Reperimento dati job?
022400090521         exsr  sr_DatiJob;
022500090521
022600090521         //?Impostazione nome programma a video?
022700090521         V1Tpgm = SDSpgm;
022800090521
022900090521       ENDSR;
023000090521
023100090521       //--------------------------------------------------------------
023200090521       //?Reperimento Dati del job (Utente/Operativi).                 ?
023300090521       //--------------------------------------------------------------
023400090521       BEGSR  sr_DatiJob;
023500090521
023600090521         in(e) §AzUte;
023700090521         if NOT %error;
023800090521           in(e) §DatiUte;
023900090521         endif;
024000090521         if %error or RSut = *blank;
024100090521           tibs34r ( tibs34ds );
024200090521           in §AzUte;
024300090521           in §DatiUte;
024400090521         endif;
024500090521
024600090521       ENDSR;
024700090521
024800090520       //--------------------------------------------------------------
024900090520       //?Operazioni iniziali.                                         ?
025000090520       //--------------------------------------------------------------
025100090520       BEGSR  sr_RoutInz;
025200090520
025300090520         //?Verifica se ricevuti parametri?
025400090521         if  SDSprm > 1;
025500090521           TNTB74ds = p_TNTB74ds;
025600090520           $Called  = *on;
025700090520         else;
025800090520           clear  TNTB74ds;
025900090520           $Called  = *off;
026000090520         endif;
026100090520
026200090520         //?Impostazione chiusura?
026300090520         select;
026400090520           when  $Called = *on   and   iCCOtla = *blank;
026500090520             *inRT = *on;
026600090520           when  $Called = *on   and   iCCOtla = 'L';
026700090520             *inLR = *on;
026800090520           when  $Called = *on   and   iCCOtla = 'C';
026900090520             *inLR = *on;
027000090520             $Fine = *on;
027100090520             leavesr;
027200090520           other;
027300090520             *inLR = *on;
027400090520         endsl;
027500090520
027600090520         //?Reperimento dati job?
027700090520         exsr  sr_DatiJob;
027800090520
027900090520         //?Impostazione nome programma a video?
028000090520         V1Tpgm = SDSpgm;
028100090520
028200090520         //?Impostazione iniziale / pulizia dei campi chiave?
028300090520         k_TBEcod = c_Tab;
028400090520         clear  k_TBEke1;
028500090520         clear  k_TBEke2;
028600090520         clear  k_TBElin;
028700090520         clear  k_TBEsif;
028800090521
028900090521         //?Impostazione iniziale parametri di output?
029000090521         clear  oCCOke1;
029100091218         clear  oCCOuni;
029200090521         clear  oCCOfxx;
029300090521         oCCOerr = *off;
029400090521         clear  oCCOmsg;
029500090521
029600090521         //?Re-impostazione flags booleani (fosse mai il 2° richiamo)?
029700090521         reset  $Fine;
029800090521         reset  $Video;
029900090521         reset  $InzS01;
030000110428         reset  $Cli_non_sped ;
030100110405
030200110405         //?verifico se passata una causale se si trattta di una causle su cliente che non spedisce
030300110405         If iccocco <> '  ' ;
030400110405            clear dCCO;
030500110405            clear TIBS02ds;
030600110405            T02mod = 'C';
030700110405            T02cod = 'CCO';
030800110405            T02ke1 = Iccocco  ;
030900110405            T02sif = KNSIF;
031000110405            TNTBE_RicercaControllo  (kpjba : tibs02ds);
031100110405            IF  T02err = *blanks;
031200110405              dCCO = t02uni ;
031300110405              If §ccocns = 'S' ;
031400110405                 $Cli_non_sped = *on ;
031500110405              Endif ;
031600110405            ENDIF;
031700110405         Endif ;
031800090520
031900090520       ENDSR;
032000090520
032100090520       //--------------------------------------------------------------
032200090520       //?Gestione subfile S01.                                        ?
032300090520       //--------------------------------------------------------------
032400090520       BEGSR  sr_GesS01;
032500090520
032600090520         //?Inizializzazione videata?
032700090520         if  $InzS01 = *on;
032800090520           exsr  sr_InzS01;
032900090520           $InzS01 = *off;
033000090520         endif;
033100090520
033200090520         //?Emissione Testata & Piede?
033300090520         write  TB74T01;
033400090520         write  TB74P01;
033500090520
033600090520         //?Posizionamento cursore?
033700090520         if  C1CsrRrn > *zero;
033800090520           C1RcdNbr = C1CsrRrn;
033900090520         else;
034000090520           C1RcdNbr = 1;
034100090520           write  TB74S00;             // (no rec.)
034200090520         endif;
034300090520
034400090520         //?Emissione videata?
034500090520         exfmt  TB74C01;
034600090520
034700090520         reset  ErrMessage;
034800090520         reset  ErrGenerico;
034900090520         clear  V1Dmsg;
035000090520         SELECT;
035100090520
035200090520           //?F3=Fine?
035300090520           WHEN  dsp_aid = c_F03;
035400090520             oCCOfxx = '1';
035500090520             $Fine = *on;
035600090520
035700090520           //?F5=Rivisualizza?
035800090520           WHEN  dsp_aid = c_F05;
035900090520             $InzS01 = *on;
036000090520
036100090520           //?F12=Ritorno?
036200090520           WHEN  dsp_aid = c_F12;
036300090520             oCCOfxx = '2';
036400090520             $Fine = *on;
036500090520
036600090520           //?Roll-Up?
036700090520           WHEN  dsp_aid = c_RollUp;
036800090520             exsr sr_RollUpS01;
036900090520
037000090520           //?SubFile vuoto?
037100090520           WHEN  S01nrr = *zero;
037200090520
037300090520           //?Invio?
037400090520           OTHER;
037500090520             exsr  sr_OpzS01;
037600090520
037700090520         ENDSL;
037800090520
037900090520       ENDSR;
038000090520
038100090520       //--------------------------------------------------------------
038200090520       //?Inizializzazione subfile S01.                                ?
038300090520       //--------------------------------------------------------------
038400090520       BEGSR  sr_InzS01;
038500090520
038600090520         //?Spegnimento degli indicatori di errore?
038700090520         %subst(IndDspF : 50) = *off;
038800090520
038900090520         //?Settaggio indicatori dei tasti funzionali?
039000090520         exsr  sr_SetIndP01;
039100090520
039200090520         //?Pulizia subfile?
039300090520         SflDsp_N    = *on;
039400090520         SflDspCtl_N = *on;
039500090520         write  TB74C01;
039600090520         SflDspCtl_N = *off;
039700090520         SflEnd      = *off;
039800090520
039900090520         clear  W_SflPag;
040000090520         clear  C1RcdNbr;
040100090520         clear  C1CsrRrn;
040200090520         clear  S01nrr;
040300090520         clear  V1Dmsg;
040400090520         ErrMessage  = *off;
040500090520         ErrGenerico = *off;
040600090520
040700090520         //?Caricamento della 1ª pagina di dati nel subfile?
040800090520         clear  k05tntbe01;
040900090520         k_TBEcod = c_Tab;
041000090520         setll  %kds(k05tntbe01)  TNTBE000;
041100090520         reade  %kds(k05tntbe01 : 1)  TNTBE000;
041200090520         exsr  sr_RollUpS01;
041300090520
041400090520         //?Impaginazione subfile?
041500090520         // -> forza l'impaginazione sul 1° rec. del subfile
041600090520         if S01nrr > *zero;
041700090520           C1RcdNbr  = 1;
041800090520           C1CsrRrn  = 1;
041900090520         else;
042000090520           clear C1RcdNbr;
042100090520         endif;
042200090520
042300090520       ENDSR;
042400090520
042500090520       //--------------------------------------------------------------
042600090520       //?Caricamento singola pagina nel subfile S01                   ?
042700090520       //--------------------------------------------------------------
042800090520       BEGSR  sr_RollUpS01;
042900090520
043000090520         S01nrr    = (W_SflPag * c_SflPag);
043100090520         W_SflPag += 1;
043200090520         SflNxtChg = *off;
043300090520
043400090520         //?Ciclo di caricamento dati da tab. "CCO"?
043500090520         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
043600090520              S01nrr    < c_MaxRec                and
043700090520              Not %eof(TNTBE01L);
043800090520
043900090520           // -?Selezione singolo record file TNTBE00F?
044000090520           exsr  sr_SelezRec;
044100090520
044200090520           // -?Registrazione singolo record nel subfile?
044300090520           if  $RecOK;
044400090520
044500090520             clear  TB74S01;
044600090520             dCCO = TBEuni;
044700090520             S01cod = TBEke1;
044800091217             S1huni = dcco;
044900090520             S01des = §CCOdes;
045000090520             S01gio = §CCOgio;
045100100924             S01upm = §CCOupm;
045200090520             S1Hstp = §CCOstp;
045300090520             S1Hstc = §CCOstc;
045400090629             S01tat = §CCOtat;
045500100408             IF  §CCOapt = 'S';
045600100408               S01apt = 'SI';
045700100408             ENDIF;
045800100519             if  §CCOcht <> *blank;
045900100519               S01cht = 'SI';
046000100519             endif;
046100090520
046200090520             S01nrr += 1;
046300090520             write  TB74S01;
046400090520
046500090520           EndIf;
046600090520
046700090520           // -?Lettura record successivo?
046800090520           reade  %kds(k05tntbe01 : 1)  TNTBE000;
046900090520
047000090520         EndDo;
047100090520
047200090520         //?Visualizzazione del SFL (se ci sono dati)?
047300090520         SflDsp_N  = (S01nrr <= *zero);
047400090520
047500090520         //?Attivazione del SFLEND?
047600090520         SflEnd    = (S01nrr >= c_MaxRec   or   %eof(TNTBE01L));
047700090520
047800090520         //?Posizionamento cursore al 1° rcd della pagina?
047900090520         if  S01nrr > *zero;
048000090520           wPag     = S01nrr / c_SflPag;
048100090520           wRig     = S01nrr - (c_SflPag * wPag);
048200090520           C1RcdNbr = wPag * c_SflPag;
048300090520           if  wRig > *zero;
048400090520             C1RcdNbr += 1;
048500090520           else;
048600090520             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
048700090520           endif;
048800090520         else;
048900090520           clear  C1RcdNbr;
049000090520         endif;
049100090520
049200090520         C1CsrRrn = C1RcdNbr;
049300090520
049400090520       ENDSR;
049500090520
049600090520       //--------------------------------------------------------------
049700090520       //?Settaggio indicatori utilizzati in C01/P01.                  ?
049800090520       //--------------------------------------------------------------
049900090520       BEGSR  sr_SetIndP01;
050000090520
050100090520         //?F3=Fine?
050200090520         F3Attivo  = (Not $Called);
050300090520
050400090520         //?F6=Inserimento?
050500090520         //F6Attivo  = (Not $Called);
050600090520
050700090520         //?F12=Ritorno?
050800090520         F12Attivo  = ($Called);
050900090520
051000090520       ENDSR;
051100090520
051200090520       //--------------------------------------------------------------
051300090520       //?Selezione del singolo record letto dalla tabella "CCO".      ?
051400090520       //--------------------------------------------------------------
051500090520       BEGSR  sr_SelezRec;
051600090520
051700090520         reset  $RecOK;
051800090520
051900090520         dCCO = TBEuni;
052000090520         §ccoSTPds = §CCOstp;
052100100219         §ccoTAAds = §CCOtaa;
052200100505         §ccoTPVds = §CCOtpv;
052300110429         §ccoCPOds = §CCOcpo;
052400090520
052500090520         select;
052600090520
052700090520           // -?Omissione records annullati?
052800090520           when  TBEatb <> *blank;
052900090520             leavesr;
053000090520
053100110428
053200110428           // -?Selezione per causale passata 32 causale 42?
053300110428              when  iCCOcco = '32' and tbeke1 <> '42             ';
053400110428               leavesr;
053500110428
053600110428
053700110428           // -?Selezione per causale passata 71 causale 72?
053800110428              when  iCCOcco = '71' and tbeke1 <> '72             ';
053900110428               leavesr;
054000110428
054100101214
054200110331           // -?Selezione per causale passata se non provengo da tntbe75r
054300110331           when §ccocae <> *blanks and Iccocco = *blanks
054400110331                   and %subst(kpjbu:1:7) <> 'TNTB75R' ;
054500110330             leavesr ;
054600110330
054700101214           // -?Selezione per causale passata in aggiunta alla precedente es 72
054800101214           when §ccocae <> *blanks and Iccocco <> *blanks and
054900101214                §ccocae <> Iccocco ;
055000101214             leavesr ;
055100100427
055200110405           // -?Selezione per causale passata e gestione delle sole causali su cliente che non spedi
055300110405           // -?sce
055400110406           when Iccocco <> *blanks and (($cli_non_sped = *on and
055500110406                §ccoucns = 'N') or ($cli_non_sped = *off and
055600110406                §ccoucns = 'S'));
055700110405             leavesr ;
055800110405
055900110406           // -?Selezione per causale passata e gestione delle sole causali su cliente che non spedi
056000110406           // -?sce
056100110406           when Iccocco <> *blanks and $cli_non_sped = *on and
056200110406                §ccoucns = 'N'  ;
056300110406             leavesr ;
056400110406
056500110502
056600110502           // -?Selezione per causale non passata e non TNTB75R escludo attività gestione causali su
056700110502           // -?cliente che non spedisce
056800110502           when Iccocco = *blanks and §ccoucns = 'S' and
056900110502                %subst(kpjbu:1:7) <> 'TNTB75R';
057000110502             leavesr ;
057100110502
057200100421
057300091218           // -?Selezione tipo attività associata?
057400091217           when  iCCOatt <> *blank   and   iCCOatt <> §CCOatt;
057500090520             leavesr;
057600091218
057700091218           // -?Selezione utilizzo causale attività?
057800091218           //   S=Sì seleziono solo i rcd a blank
057900091218           when  iCCOuti = 'S' and  §CCOuti <> *blanks;
058000091218             leavesr;
058100091218
058200091218           // -?Selezione utilizzo causale attività?
058300091218           //   N=No escludo i rcd a blank
058400091218           when  iCCOuti = 'N' and  §CCOuti = *blanks;
058500091218             leavesr;
058600100628
058700100628           // -?Selezione utilizzo causale attività?
058800100628           //   X=...Seleziono i rcd a blank +
058900100628           //                  i rcd a "N" con cod >= 14 e <= 99
059000100628           when  iCCOuti = 'X' and  (§CCOuti <> *blank  and
059100110708                (TBEke1  < '14 '  or  TBEke1 > '99 ' or TBEke1 = '50'));
059200100628             leavesr;
059300090520
059400090520           // -?Selezione stato?
059500090520           //  ?(cercando quello ricevuto tra i "precedenti")?
059600090520           when  iCCOfst <> *blank    and
059700090703                  %lookup(iCCOfst : $StP) = *zero and
059800090703                 %lookup('99' : $StP) = *zero   ;
059900090520             leavesr;
060000100219
060100100504           // -?Selezione utilizzo causale attività per esitare l'attività che ricevo in ds?
060200100219           //   schiera TAA = tipi attività abilitate tutto blank non faccio controllo
060300100219           when  iCCOese <> *blanks  and  §CCOtaa <> *blanks and
060400100219                 %lookup(iccoese : $Taa) = *zero ;
060500100219             leavesr;
060600100504
060700100504           // -?Selezione utilizzo tipo trattativa?
060800100504           //   schiera TPV = tipi trattativa (tutto blank => non faccio controllo)
060900100504           when  iCCOtpv <> *blanks  and  §CCOtpv <> *blanks and
061000100504                 %lookup(iCCOtpv : $Tpv) = *zero ;
061100100504             leavesr;
061200100322
061300110429           // -?Selezione categoria potenziale?
061400110429           //   schiera CPO = categoria potenziale (tutto blank => non faccio controllo)
061500110429           when  iCCOcod <> *blanks  and  §CCOcpo <> *blanks and
061600110429                 %lookup(iCCOcod : $Cpo) = *zero ;
061700110429             leavesr;
061800110429
061900110429           // -?Selezione codificato? obsoleto
062000110429           // when  §CCOcod <> *blank  and iccocod <> *blank and
062100110429           //       iCCOcod <> §ccocod ;
062200110429           //   leavesr;
062300100322
062400100504           // -?Selezione trattativa?
062500100322           when  §CCOtra = 'S' and §ccoapt <> 'S' and
062600100322                  iCCOtra = 'N' ;
062700100322             leavesr;
062800100322
062900100322           when  §CCOtra = 'N' and
063000100322                  iCCOtra = 'S' ;
063100100322             leavesr;
063200090520
063300100504           // -?Selezione causali con o senza offerte?
063400100324           when  §CCOoff = 'S' and
063500100324                  iCCOoff = 'N' ;
063600100324             leavesr;
063700100324           when  §CCOoff = 'N' and
063800100324                  iCCOoff = 'S' ;
063900100324             leavesr;
064000090520         endsl;
064100090520
064200090520         $RecOK = *on;
064300090520
064400090520       ENDSR;
064500090520
064600090520       //--------------------------------------------------------------
064700090520       //?Gestione opzioni subfile S01.                                ?
064800090520       //--------------------------------------------------------------
064900090520       BEGSR  sr_OpzS01;
065000090520
065100090520         clear  SavS01csr;
065200090520
065300090520         //?Ciclo di lettura subfile?
065400090520         readc  TB74S01;
065500090520
065600090520         DoW  Not %eof(TNTB74D);
065700090520
065800090520           SflNxtChg = *off;
065900090520           %subst(IndDspF : 50) = *off;
066000090520           C1RcdNbr = S01nrr;
066100090520
066200090520           select;
066300090520
066400090520             // -?Opz. 1=Selezione?
066500090520             when  S01opz = '1'   and   Not $Fine;
066600090520               clear  dCCO;
066700091217               dcco    = S1huni;
066800090520               oCCOke1 = S01cod;
066900090520               oCCOuni = dCCO;
067000090520               $Fine   = *on;
067100090520
067200090520             // -?Opz. 1=Selezione già reperita?
067300090520             //  ?(ammessa una sola selezione)?
067400090520             when  S01opz = '1'   and   $Fine;
067500090520               ErrMessage  = *on;
067600090520               ErrGenerico = *on;
067700090520               PosCurOPZ   = *on;
067800090520               V1Dmsg = $Msg(02);
067900090520               $Fine  = *off;
068000090520
068100090520             // -?Nessuna opzione?
068200090520             when  S01opz = *blank;
068300090520
068400090520             // -?Opzione errata?
068500090520             other;
068600090520               ErrMessage  = *on;
068700090520               ErrGenerico = *on;
068800090520               PosCurOPZ   = *on;
068900090520               V1Dmsg = $Msg(01);
069000090520               $Fine  = *off;
069100090520
069200090520           endsl;
069300090520
069400090520           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
069500090520           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
069600090520           if  S01opz  <> *blank   and
069700090520              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
069800090520             SavS01csr   = C1RcdNbr;
069900090520           endif;
070000090520
070100090520           // -?Aggiornamento sfl?
070200090520           select;
070300090520             when  ErrMessage;
070400090520               SflNxtChg = *on;
070500090520               C1CsrRrn  = C1RcdNbr;
070600090520             when  ErrGenerico;
070700090520               C1CsrRrn  = C1RcdNbr;
070800090520               if  S01opz <> '1';
070900090520                 clear  S01opz;
071000090520               endif;
071100090520             other;
071200090520               if  S01opz <> '1';
071300090520                 clear  S01opz;
071400090520               endif;
071500090520           endsl;
071600090520
071700090520           if  S01opz <> *blank;
071800090520             SflNxtChg = *on;
071900090520           endif;
072000090520
072100090520           UPDATE  TB74S01;
072200090520
072300090520           if  ErrGenerico   or   ErrMessage;
072400090520             leave;
072500090520           endif;
072600090520
072700090520           readc  TB74S01;
072800090520
072900090520         ENDDO;
073000090520
073100090520         // -?Riposiziono il cursore sul record contenente la prima opz.?
073200090520         //  ?(se non sono stati rilevati errori)?
073300090520         if  NOT ErrMessage   and   NOT ErrGenerico   and
073400090520             SavS01csr > *zero;
073500090520           C1CsrRrn = SavS01csr;
073600090520         endif;
073700090520
073800090520       ENDSR;
073900090520
074000090520       //--------------------------------------------------------------
074100090520       //?Operazioni finali.                                           ?
074200090520       //--------------------------------------------------------------
074300090520       BEGSR  sr_RoutEnd;
074400090521
074500090521         //?Ritorno parametri?
074600090521         if  SDSprm > 1;
074700090521           p_TNTB74ds = TNTB74ds;
074800090521         endif;
074900090520
075000090520         //?"Chiusura" pgm?
075100090520         return;
075200090520
075300090520       ENDSR;
075400090520
075500090520      /end-free
075600090520
075700090520       //--------------------------------------------------------------
075800090520       //?Schiere a tempo di compilazione.                             ?
075900090520       //--------------------------------------------------------------
076000090520
076100090520** - $Msg -------------------------------------------------------------------*
076200090520Opzione errata                                                                 1
076300090520Ammessa la selezione di una sola causale                                       2
