000100090811       //--------------------------------------------------------------
000200090811       //?TNTA28R - Gestione Note "10" di Tariffe/Offerte              ?
000300090811       //--------------------------------------------------------------
000400090811
000500090811     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090811
000700090811       //--------------------------------------------------------------
000800090811       //?Dichiarazione file.                                          ?
000900090811       //--------------------------------------------------------------
001000090811
001100090811       // - Offerte
001200090811     fTNOFM01L  if   e           k disk    usropn
001300110805     f                                     extfile(wlib)
001400090811     f                                     rename(TNTAM000 : TNOFM000)
001500090811       // - Tariffe
001600090811     fTNTAM01L  if   e           k disk    usropn
001700090811       // - Note
001800090811     fTFNTC01L  if   e           k disk
001900090811
002000090811       // - Video Gestione
002100090811     fTNTA28D   cf   e             workstn
002200090811     f                                     sfile(TA28S01 : S01nrr)
002300090811     f                                     indds(IndDspF)
002400090811     f                                     infds(InfDspF)
002500090811
002600090811       //--------------------------------------------------------------
002700090811       //?Definizione costanti.                                        ?
002800090811       //--------------------------------------------------------------
002900090811
003000090811       // - Tipo note
003100090811     d c_TipoNota      c                   const('10')
003200090811
003300090811       // - Numero di record in ciascuna videata di subfile
003400090811     d c_SflPag        c                   const(16)
003500090811
003600090811       // - Numero massimo di record gestibili
003700110228     d c_MaxRec        c                   const(9999)
003800090811
003900090811       // - Tasti funzionali a video
004000090811     d c_F01           c                   const(x'31')
004100090811     d c_F02           c                   const(x'32')
004200090811     d c_F03           c                   const(x'33')
004300090811     d c_F04           c                   const(x'34')
004400090811     d c_F05           c                   const(x'35')
004500090811     d c_F06           c                   const(x'36')
004600090811     d c_F07           c                   const(x'37')
004700090811     d c_F08           c                   const(x'38')
004800090811     d c_F09           c                   const(x'39')
004900090811     d c_F10           c                   const(x'3A')
005000090811     d c_F11           c                   const(x'3B')
005100090811     d c_F12           c                   const(x'3C')
005200090811     d c_F13           c                   const(x'B1')
005300090811     d c_F14           c                   const(x'B2')
005400090811     d c_F15           c                   const(x'B3')
005500090811     d c_F16           c                   const(x'B4')
005600090811     d c_F17           c                   const(x'B5')
005700090811     d c_F18           c                   const(x'B6')
005800090811     d c_F19           c                   const(x'B7')
005900090811     d c_F20           c                   const(x'B8')
006000090811     d c_F21           c                   const(x'B9')
006100090811     d c_F22           c                   const(x'BA')
006200090811     d c_F23           c                   const(x'BB')
006300090811     d c_F24           c                   const(x'BC')
006400090811     d c_Enter         c                   const(x'F1')
006500090811     d c_RollDown      c                   const(x'F4')
006600090811     d c_RollUp        c                   const(x'F5')
006700090811
006800090811       //--------------------------------------------------------------
006900090811       //?Definizione schiere.                                         ?
007000090811       //--------------------------------------------------------------
007100090811
007200090811       // - Messaggi di errore
007300090814     d $Msg            s             78    dim( 7) ctdata perrcd(1)
007400090811
007500090811       //--------------------------------------------------------------
007600090811       //?Definizione aree dati.                                       ?
007700090811       //--------------------------------------------------------------
007800090811
007900090811       // - Dati utente
008000090811     d §AzUte        e ds                  extname(AZUTE00F)
008100090811     d                                     dtaara
008200090811     d §DatiUte      e ds                  extname(dDatiUte)
008300090811     d                                     dtaara
008400090811
008500090811       //--------------------------------------------------------------
008600090811       //?Definizione strutture dati.                                  ?
008700090811       //--------------------------------------------------------------
008800090811
008900090811       // - Status ds
009000090811     d Status         sds
009100090811     d  SDSpgm           *proc
009200090811     d  SDSusr               254    263
009300090811
009400090811       // - InfDS
009500090811     d InfDspF         ds
009600090811     d   dsp_aid             369    369a
009700090811     d   sfl_rrn             376    377i 0
009800090811     d   min_nrr             378    379i 0
009900090811     d   num_rcds            380    381i 0
010000090811
010100090811       // - Indicatori su DspF
010200090811     d IndDspF         ds                  inz
010300090811         // - Abilitazione tasti funzionali
010400090813     d   AbilitF05                     n   overlay(IndDspF : 05)
010500090811         // - Emissione messaggio di errore
010600090811     d   ErrMessage                    n   overlay(IndDspF : 28)
010700090811         // - Indicatori di gestione del subfile
010800090811     d   SflDsp_N                      n   overlay(IndDspF : 30)
010900090811     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011000090811     d   SflNxtChg                     n   overlay(IndDspF : 32)
011100090811     d   SflEnd                        n   overlay(IndDspF : 33)
011200090811         // - Indicatori per Attibuti di visualizzazione
011300090811     d   ProtectOPZ                    n   overlay(IndDspF : 40)
011400090811     d   EvidenzCTR                    n   overlay(IndDspF : 41)
011500090811     d   UnderLineNOT                  n   overlay(IndDspF : 42)
011600090811         // - Posizionamento cursore & segnalazione errore
011700090811     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011800090811         //   - Riemissione videata
011900090811     d   ErrGenerico                   n   overlay(IndDspF : 99)
012000090811
012100090811       // - Parametri ricevuti
012200090811     d KPJBA         e ds
012300090811     d TNTA28ds      e ds                  inz
012400090812
012500090812       // - Parametri x *pgm di manutenzione note della singola tariffa
012600090812     d TNTA29ds      e ds                  inz
012700090811
012800090811       // - Parametri per Reperimento dati utente
012900090811     d TIBS34ds      e ds                  inz
013000090811
013100090811       //--------------------------------------------------------------
013200090811       //?Definizione variabili globali.                               ?
013300090811       //--------------------------------------------------------------
013400090811
013500090811       // - Flags booleani
013600090811     d $Fine           s               n   inz
013700090811     d $InzS01         s               n   inz(*on)
013800090811
013900090811       // - Variabili per la gestione del video
014000090811     d $Video          s              2    inz('S1')
014100090811     d W_SflPag        s              3  0 inz
014200090811     d wPag            s              4  0 inz
014300090811     d wRig            s              3  0 inz
014400090811     d S01nrr          s                   like(C1RcdNbr) inz
014500090811     d SavS01csr       s                   like(C1RcdNbr) inz
014600090811
014700090811       // - Campi di comodo
014800110805     d wLib            s             21    inz
014900090811     d wRecTarif       s              3  0 inz
015000090908     d wNumTarif       s                   like(C1RcdNbr) inz
015100090811
015200090811       //--------------------------------------------------------------
015300090811       //?Definizione prototipi procedure.                             ?
015400090811       //--------------------------------------------------------------
015500090811
015600090811       // - Reperimento dati utente
015700090811      /copy gaitrasrc/srcProtoPR,TIBS34R
015800090812
015900090812       // - Manutenzione/Cancellazione/Copia/ note singola tariffa
016000090812      /copy gaitrasrc/srcProtoPR,TNTA29R
016100090811
016200090811       //--------------------------------------------------------------
016300090811       //?Definizione key-list.                                        ?
016400090811       //--------------------------------------------------------------
016500090811
016600090811       // - File TNOFM01L / TNTAM01L
016700090811     d k03tntam01    e ds                  extname(TNTAM01L : *key)
016800090811     d                                     prefix(k_)   inz
016900090811
017000090811       // - File TFNTC01L
017100090811     d k04tfntc01    e ds                  extname(TFNTC01L : *key)
017200090811     d                                     prefix(k_)   inz
017300090811
017400090811       //--------------------------------------------------------------
017500090811       //?M A I N - L I N E                                            ?
017600090811       //--------------------------------------------------------------
017700090811
017800090811     c     *Entry        plist
017900090811     c                   parm                    KPJBA
018000090811
018100090811      /free
018200090811
018300090811       //?Operazioni iniziali?
018400090811       exsr sr_RoutInz;
018500090811
018600090811       DoW  $Fine = *off;
018700090811         select;
018800090811           // - Gestione subfile S1
018900090811           when $Video = 'S1';
019000090811             exsr sr_GesS01;
019100090811           other;
019200090811             $Fine = *on;
019300090811         endsl;
019400090811       enddo;
019500090811
019600090811       //?Operazioni finali?
019700090811       exsr sr_RoutEnd;
019800090811
019900090811       //--------------------------------------------------------------
020000090811       //?Operazioni iniziali.                                         ?
020100090811       //--------------------------------------------------------------
020200090811       BEGSR  sr_RoutInz;
020300090811
020400090811         *inLR = *on;
020500090811
020600090811         //?Ricezione parametri?
020700090811         TNTA28ds = kpjbu;
020800090811         oTA28err = *off;
020900090811         clear  oTA28msg;
021000090811
021100090811         //?Controllo dei parametri ricevuti?
021200090811         select;
021300090811           when  iTA28tip <> 'T'   and   iTA28tip <> 'C';
021400090811             oTA28err = *on;
021500090811             oTA28msg = $Msg(01);
021600090811           when  iTA28cod = *zero;
021700090811             oTA28err = *on;
021800090811             if  iTA28tip = 'T';
021900090811               oTA28msg = $Msg(02);
022000090811             else;
022100090811               oTA28msg = $Msg(03);
022200090811             endif;
022300090811         endsl;
022400090811         if  oTA28err = *on;
022500090811           $Fine = *on;
022600090811           leavesr;
022700090811         endif;
022800090811
022900090811         //?Reperimento dati job?
023000090811         exsr  sr_DatiJob;
023100090811
023200090811         //?Impostazione nome programma a video?
023300090811         V1Tpgm = SDSpgm;
023400090811         clear  V1Topz;
023500090811
023600090811       ENDSR;
023700090811
023800090811       //--------------------------------------------------------------
023900090811       //?Reperimento Dati del job (Utente/Operativi).                 ?
024000090811       //--------------------------------------------------------------
024100090811       BEGSR  sr_DatiJob;
024200090811
024300090811         in(e) §AzUte;
024400090811         if NOT %error;
024500090811           in(e) §DatiUte;
024600090811         endif;
024700090811         if %error or RSut = *blank;
024800090811           tibs34r ( tibs34ds );
024900090811           in §AzUte;
025000090811           in §DatiUte;
025100090811         endif;
025200090811
025300090811       ENDSR;
025400090811
025500090811       //--------------------------------------------------------------
025600090811       //?Gestione subfile S01                                         ?
025700090811       //--------------------------------------------------------------
025800090811       BEGSR  sr_GesS01;
025900090811
026000090811         //?Inizializzazione subfile?
026100090813         if  $InzS01 = *on;
026200090813           if  NOT ErrMessage;
026300090813             exsr  sr_InzS01;
026400090813             $InzS01 = *off;
026500090813           else;
026600090813             AbilitF05 = *on;
026700090813           endif;
026800090811         endif;
026900090811
027000090811         //?Emissione Testata e Piede con tasti funzionali abilitati?
027100090811         write  TA28T01;
027200090811         write  TA28P01;
027300090811
027400090811         //?Posizionamento cursore?
027500090811         if  C1CsrRrn > *zero;
027600090811           C1RcdNbr = C1CsrRrn;
027700090811         else;
027800090811           C1RcdNbr = 1;
027900090811           //write TA28S00;              // (no rec.)
028000090811         endif;
028100090811
028200090811         //?Emissione videata?
028300090811         exfmt  TA28C01;
028400090811         reset  ErrMessage;
028500090811         reset  ErrGenerico;
028600090811         clear  V1Dmsg;
028700090811
028800090811         SELECT;
028900090811
029000090811           //?F3=Fine?
029100090811           WHEN  dsp_aid = c_F03;
029200090811             $Fine = *on;
029300090813
029400090813           //?F5=Aggiornamento sfl?
029500090813           WHEN  dsp_aid = c_F05;
029600090813             $InzS01 = *on;
029700090813             AbilitF05 = *off;
029800090811
029900090811           //?Roll-Up?
030000090811           WHEN  dsp_aid = c_RollUp;
030100090811             exsr sr_RollUpS01;
030200090811
030300090811           //?SubFile vuoto?
030400090811           WHEN  S01nrr = *zero;
030500090811
030600090811           //?Invio?
030700090811           OTHER;
030800090811             // -?Gestione opzioni?
030900090811             exsr  sr_OpzS01;
031000090811             if  ErrGenerico;
031100090811               leavesr;
031200090811             endif;
031300090811
031400090811         ENDSL;
031500090811
031600090811       ENDSR;
031700090811
031800090811       //--------------------------------------------------------------
031900090811       //?Inizializzazione subfile S01                                 ?
032000090811       //--------------------------------------------------------------
032100090811       BEGSR  sr_InzS01;
032200090811
032300090811         //?Spegnimento degli indicatori di errore?
032400090811         %subst(IndDspF : 50) = *off;
032500090811
032600090811         //?Impostazione campi nel subfile-control?
032700090811         if  iTA28tip = 'T';
032800090908           C1txt1 = 'Trattativa n°';
032900090908           C1txt2 = %editc(iTA28cod : 'Z');
033000090811         else;
033100090908           C1txt1 = 'Cliente ....:';
033200090908           C1txt2 = %editc(iTA28cod : 'X');
033300090811         endif;
033400090908         C1txt2 = %trimr(C1txt2)
033500090811                  + '  '
033600090811                  + iTA28rsc;
033700090915         select;
033800090915           when  iTA28ela = 'I';
033900090915             C1Topz = '5=Visualizzazione';
034000090915           when  iTA28ela = 'M';
034100090915             C1Topz = 'G=Gestione, C=Copia';
034200090915           other;
034300090915             clear  C1Topz;
034400090915         endsl;
034500090811
034600090811         //?Pulizia subfile?
034700090811         SflDsp_N    = *on;
034800090811         SflDspCtl_N = *on;
034900090811         write  TA28C01;
035000090811         SflDspCtl_N = *off;
035100090811         SflEnd      = *off;
035200090811
035300090908         clear  wNumTarif;
035400090811         clear  W_SflPag;
035500090811         clear  C1RcdNbr;
035600090811         clear  C1CsrRrn;
035700090811         clear  S01nrr;
035800090811         clear  V1Dmsg;
035900090811         ErrMessage  = *off;
036000090811         ErrGenerico = *off;
036100090813         clear  NTCnk1;
036200090813         clear  NTCnk2;
036300090811
036400090811         //?Caricamento subfile con note "10"?
036500091120
036600091120         //?prima cerco con solo codice cliente o offerta?
036700091120         exsr  sr_NotaNOctr;
036800091120
036900091120         //?poi cerco con codice tariffa?
037000090813         if  iTA28tip = 'T'   and   %open(TNOFM01L);
037100090813           close  TNOFM01L;
037200090813         endif;
037300090813         if  iTA28tip = 'C'   and   %open(TNTAM01L);
037400090813           close  TNTAM01L;
037500090813         endif;
037600090811         k_TAMksc = iTA28cod;
037700090813         k_TAMctr = *loval;
037800090811         k_TAMprg = *loval;
037900090811         if  iTA28tip = 'T';
038000090811           exsr  sr_ReadTNOFM;
038100090811         else;
038200090811           exsr  sr_ReadTNTAM;
038300090811         endif;
038400090811         exsr  sr_RollUpS01;
038500090811
038600090811         //?Impaginazione subfile?
038700090811         // -> forza l'impaginazione sul 1° rec. del subfile
038800090811         if S01nrr > *zero;
038900090811           C1RcdNbr  = 1;
039000090811           C1CsrRrn  = 1;
039100090811         else;
039200090811           clear C1RcdNbr;
039300090811         endif;
039400090811
039500090811       ENDSR;
039600091120
039700091120       //--------------------------------------------------------------
039800091120       //?Lettura Nota 10 senza TARIFFA                                ?
039900091120       //--------------------------------------------------------------
040000091120       BEGSR  sr_NotaNOctr;
040100091120
040200091120         S01nrr    = (W_SflPag * c_SflPag);
040300091120
040400091120         if  iTA28tip = 'T';           // T = Trattativa
040500091120           k_NTCapl = 'T';
040600091120         else;                         // C = Tariffe cliente
040700091120           k_NTCapl = iTA28tip;
040800091120         endif;
040900091120         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( ITA28cod : 'X' );
041000091120         clear k_NTCnk2;
041100091120         k_NTCtnt = c_TipoNota;
041200091120
041300091120         setll  %kds(k04tfntc01)  TFNTC;
041400091120         IF  not %equal(tfntc01l);
041500091120           clear S1Cnot;
041600091120         ELSE;
041700091120           reade  %kds(k04tfntc01)  TFNTC;
041800091120           IF  not %eof(tfntc01l);
041900091120             S1Cnot = NTCrnt;
042000091120           ENDIF;
042100091120         ENDIF;
042200091120           wNumTarif += 1;
042300091120           S1Cctr = 'CLI';
042400091120           // -?Impostazione attributi di visualizzazione?
042500091120           exsr  sr_AttrS01;
042600091120           // -?Scrittura record nerl subfile?
042700091120           S01nrr += 1;
042800091120           write  TA28S01;
042900091120           leavesr;
043000091120
043100091120       ENDSR;
043200090811
043300090811       //--------------------------------------------------------------
043400090811       //?Lettura singolo record dal file TNOFM01L                     ?
043500090811       //--------------------------------------------------------------
043600090811       BEGSR  sr_ReadTNOFM;
043700090811
043800090811         if  not %open(TNOFM01L);
043900110805         IF  %subst(knsif:7:1) = 'P';
044000110805           wLib = 'FILTRAGRPF/';
044100110805         //?Apro il file
044200110805           %subst(wLib:12:8) = 'TNOFM01L';
044300090811           open  TNOFM01L;
044400110805         Endif ;
044500110805         IF  %subst(knsif:7:1) <> 'P';
044600110805           wLib = 'FILTRAGRU/';
044700110805         //?Apro il file
044800110805           %subst(wLib:12:8) = 'TNOFM01L';
044900110805           open  TNOFM01L;
045000110805         Endif ;
045100090811           setll  %kds(k03tntam01)  TNOFM000;
045200090811         endif;
045300090811
045400090811         DoU  %eof(TNOFM01L)   or   TAMatb = *blank;
045500090811           reade  %kds(k03tntam01 : 1)  TNOFM000;
045600090811         EndDo;
045700090811
045800090811         // - Lettura 1ª singola nota dal file TFNTC01L
045900090811         if  not %eof(TNOFM01L);
046000090908           wNumTarif += 1;
046100090811           exsr  sr_ReadTFNTC;
046200090811         endif;
046300090811
046400090811         clear  wRecTarif;
046500090811
046600090811       ENDSR;
046700090811
046800090811       //--------------------------------------------------------------
046900090811       //?Lettura singolo record dal file TNTAM01L                     ?
047000090811       //--------------------------------------------------------------
047100090811       BEGSR  sr_ReadTNTAM;
047200090811
047300090811         if  not %open(TNTAM01L);
047400090811           open  TNTAM01L;
047500090811           setll  %kds(k03tntam01)  TNTAM000;
047600090811         endif;
047700090811
047800090811         DoU  %eof(TNTAM01L)   or   TAMatb = *blank;
047900090811           reade  %kds(k03tntam01 : 1)  TNTAM000;
048000090811         EndDo;
048100090811
048200090811         // - Lettura 1ª singola nota dal file TFNTC01L
048300090811         if  not %eof(TNTAM01L);
048400090908           wNumTarif += 1;
048500090811           exsr  sr_ReadTFNTC;
048600090811         endif;
048700090811
048800090811         clear  wRecTarif;
048900090811
049000090811       ENDSR;
049100090811
049200090811       //--------------------------------------------------------------
049300090908       //?Lettura singolo record file TFNTC01L                         ?
049400090811       //--------------------------------------------------------------
049500090811       BEGSR  sr_ReadTFNTC;
049600090811
049700090811         if  NTCnk2 <> %editc( TAMctr : 'X' );
049800090811
049900090811           if  iTA28tip = 'T';           // T = Trattativa
050000091118             k_NTCapl = 'T';
050100090811           else;                         // C = Tariffe cliente
050200090811             k_NTCapl = iTA28tip;
050300090811           endif;
050400090811           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( TAMksc : 'X' );
050500090811           k_NTCnk2 = %editc( TAMctr : 'X' );
050600090811           k_NTCtnt = c_TipoNota;
050700090811
050800090811           setll  %kds(k04tfntc01)  TFNTC;
050900090811
051000090811         endif;
051100090811
051200090811         reade  %kds(k04tfntc01)  TFNTC;
051300090811
051400090811         if  %eof(TFNTC01L);
051500090811           clear  NTCrnt;
051600090811           k_TAMctr = TAMctr;
051700090811           k_TAMprg = *hival;
051800090811           if  iTA28tip = 'T';
051900090811             setgt  %kds(k03tntam01 : 2)  TNOFM000;
052000090811           else;
052100090811             setgt  %kds(k03tntam01 : 2)  TNTAM000;
052200090811           endif;
052300090811         endif;
052400090811
052500090811       ENDSR;
052600090811
052700090811       //--------------------------------------------------------------
052800090811       //?Caricamento singola pagina nel subfile S01                   ?
052900090811       //--------------------------------------------------------------
053000090811       BEGSR  sr_RollUpS01;
053100091120         // S01nrr    = (W_SflPag * c_SflPag);
053200090811         W_SflPag += 1;
053300090811         SflNxtChg = *off;
053400090811
053500090811         //?Ciclo di caricamento dati dai files?
053600090811         //?TNOFM01L/TNTAM01L e TFNTC01L?
053700090811         DoW  S01nrr   < (W_SflPag * c_SflPag)   and
053800090811              S01nrr   < c_MaxRec                and
053900090811             (iTA28tip = 'T'   and   Not  %eof(TNOFM01L)   or
054000090811              iTA28tip = 'C'   and   Not  %eof(TNTAM01L));
054100090811
054200090811           // -?Registrazione singolo record nel subfile?
054300090811           clear  TA28S01;
054400090811           S1Cctr = %editc( TAMctr : 'X' );
054500090811           S1Cnot = NTCrnt;
054600090811           // - Campi hidden
054700090811           wRecTarif += 1;
054800090811           H1in40 = (wRecTarif > 1);
054900090811
055000090811           // -?Impostazione attributi di visualizzazione?
055100090811           exsr  sr_AttrS01;
055200090811
055300090811           // -?Scrittura record nerl subfile?
055400090811           S01nrr += 1;
055500090811           write  TA28S01;
055600090811
055700090811           // -?Lettura record successivo?
055800090811           if  not %eof(TFNTC01L);
055900090811             exsr  sr_ReadTFNTC;
056000090811           endif;
056100090811           select;
056200090811             when  %eof(TFNTC01L)  and  iTA28tip = 'T';
056300090811               exsr  sr_ReadTNOFM;
056400090811             when  %eof(TFNTC01L)  and  iTA28tip = 'C';
056500090811               exsr  sr_ReadTNTAM;
056600090811           endsl;
056700090811
056800090811         EndDo;
056900090811
057000090811         //?Visualizzazione del SFL (se ci sono dati)?
057100090811         SflDsp_N  = (S01nrr <= *zero);
057200090811
057300090811         //?Attivazione del SFLEND?
057400090811         SflEnd = (S01nrr  >= c_MaxRec  or
057500090811                  (iTA28tip = 'T'  and  %eof(TNOFM01L))  or
057600090811                  (iTA28tip = 'C'  and  %eof(TNTAM01L)));
057700090811         if  S01nrr  >= c_MaxRec   and
057800090811            (iTA28tip = 'T'  and  NOT %eof(TNOFM01L)   or
057900090811             iTA28tip = 'C'  and  NOT %eof(TNTAM01L));
058000090811           V1Dmsg = $Msg(04);
058100090811           ErrMessage  = *on;
058200090811           ErrGenerico = *on;
058300090811         endif;
058400090811
058500090811         //?Posizionamento cursore al 1° rcd della pagina?
058600090811         if  S01nrr > *zero;
058700090811           wPag     = S01nrr / c_SflPag;
058800090811           wRig     = S01nrr - (c_SflPag * wPag);
058900090811           C1RcdNbr = wPag * c_SflPag;
059000090811           if  wRig > *zero;
059100090811             C1RcdNbr += 1;
059200090811           else;
059300090811             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
059400090811           endif;
059500090811         else;
059600090811           clear  C1RcdNbr;
059700090811         endif;
059800090811
059900090811         C1CsrRrn = C1RcdNbr;
060000090811
060100090811       ENDSR;
060200090811
060300090811       //--------------------------------------------------------------
060400090811       //?Impostazione attributi di visualizzazione nel rec. del sfl   ?
060500090811       //--------------------------------------------------------------
060600090811       BEGSR  sr_AttrS01;
060700090811
060800090811         // - Protezione campo opzione
060900090812         ProtectOPZ   = (H1in40 = *on);
061000090811         // - Evidenziazione codice tariffa (se ricevuta)
061100090811         EvidenzCTR   = (S1Cctr = iTA28ctr);
061200090811         // - Evidenziazione mancanza di note per la tariffa
061300090811         UnderLineNOT = (S1Cnot = *blank);
061400090811
061500090811       ENDSR;
061600090811
061700090811       //--------------------------------------------------------------
061800090811       //?Gestione opzioni del subfile S01                             ?
061900090811       //--------------------------------------------------------------
062000090811       BEGSR  sr_OpzS01;
062100090811
062200090811         clear  SavS01csr;
062300090811
062400090811         //?Ciclo di lettura subfile?
062500090811         readc  TA28S01;
062600090811
062700090811         DoW  Not %eof(TNTA28D);
062800090811
062900090811           SflNxtChg = *off;
063000090811           %subst(IndDspF : 50) = *off;
063100090811           C1RcdNbr = S01nrr;
063200090811
063300090811           select;
063400090915
063500090915             // -?5=Visualizzazione?
063600090915             when  S1Copz = '5'   and   iTA28ela = 'I';
063700090915               clear  TNTA29ds;
063800090915               iTA29opz = '5';
063900090915               exsr  sr_CallTNTA29;
064000090811
064100090811             // -?G=Gestione?
064200090915             when  S1Copz = 'G'   and   iTA28ela = 'M';
064300090812               clear  TNTA29ds;
064400090813               iTA29opz = '2';
064500090813               exsr  sr_CallTNTA29;
064600090811
064700090813             // -?C=Copia?
064800090915             when  S1Copz = 'C'   and   iTA28ela = 'M';
064900090814               select;
065000090814                 when  S1Cnot = *blank;
065100090814                   ErrMessage  = *on;
065200090814                   ErrGenerico = *on;
065300090814                   PosCurOPZ   = *on;
065400090814                   V1Dmsg = $Msg(06);
065500090908                 when  wNumTarif <= 1;
065600090814                   ErrMessage  = *on;
065700090814                   ErrGenerico = *on;
065800090814                   PosCurOPZ   = *on;
065900090814                   V1Dmsg = $Msg(07);
066000090814                 other;
066100090814                   clear  TNTA29ds;
066200090814                   iTA29opz = '3';
066300090814                   exsr  sr_CallTNTA29;
066400090814               endsl;
066500090811
066600090811             // -?Nessuna opzione?
066700090811             when  S1Copz = *blank;
066800090811
066900090811             // -?Opzione errata?
067000090811             other;
067100090811               ErrMessage  = *on;
067200090811               ErrGenerico = *on;
067300090811               PosCurOPZ   = *on;
067400090811               V1Dmsg = $Msg(05);
067500090811
067600090811           endsl;
067700090811
067800090811           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
067900090811           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
068000090811           if  S1Copz  <> *blank   and
068100090811              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
068200090811             SavS01csr   = C1RcdNbr;
068300090811           endif;
068400090811
068500090811           // -?ReImpostazione attributi di visualizzazione?
068600090811           exsr  sr_AttrS01;
068700090811
068800090811           // -?Aggiornamento sfl?
068900090811           select;
069000090811             when  ErrMessage;
069100090811               SflNxtChg = *on;
069200090811               C1CsrRrn  = C1RcdNbr;
069300090811             when  ErrGenerico;
069400090811               C1CsrRrn  = C1RcdNbr;
069500090811               clear  S1Copz;
069600090811             other;
069700090811               clear  S1Copz;
069800090811           endsl;
069900090811           UPDATE  TA28S01;
070000090811           if  ErrGenerico   or   ErrMessage;
070100090811             leave;
070200090811           endif;
070300090811
070400090811           readc  TA28S01;
070500090811
070600090811         ENDDO;
070700090811
070800090811         // -?Riposiziono il cursore sul record contenente la prima opz.?
070900090811         //  ?(se non sono stati rilevati errori)?
071000090811         if  NOT ErrMessage   and   NOT ErrGenerico   and
071100090811             SavS01csr > *zero;
071200090811           C1CsrRrn = SavS01csr;
071300090811         endif;
071400090811
071500090811       ENDSR;
071600090813
071700090813       //--------------------------------------------------------------
071800090813       //?Richiamo Inserimento/Manutenzione/Copia note tariffa         ?
071900090813       //--------------------------------------------------------------
072000090813       BEGSR  sr_CallTNTA29;
072100090813
072200090813         //clear  TNTA29ds;            (già fatto)
072300090813         //iTA29opz = '?';             (già valorizzato)
072400090813         iTA29tip = iTA28tip;
072500090813         iTA29cod = iTA28cod;
072600090813         iTA29rsc = iTA28rsc;
072700090813         iTA29ctr = S1Cctr;
072800090813
072900090813         kpjbu    = TNTA29ds;
073000090813
073100090813         tnta29r ( kpjba );
073200090813
073300090813         TNTA29ds = kpjbu;
073400090813
073500090813         select;
073600090813           // - Restituito errore
073700090813           when oTA29err = *on;
073800090813             ErrMessage  = *on;
073900090813             ErrGenerico = *on;
074000090813             PosCurOPZ   = *on;
074100090813             V1Dmsg = oTA29msg;
074200090813           // - Non premuto F12
074300090813           when  oTA29fxx <> '12';
074400090813             $InzS01 = *on;
074500090813         endsl;
074600090813
074700090813       ENDSR;
074800090811
074900090811       //--------------------------------------------------------------
075000090811       //?Operazioni finali.                                           ?
075100090811       //--------------------------------------------------------------
075200090811       BEGSR  sr_RoutEnd;
075300090811
075400090811         kpjbu = TNTA28ds;
075500090811
075600090811         return;
075700090811
075800090811       ENDSR;
075900090811
076000090811      /end-free
076100090811
076200090811** - $Msg -------------------------------------------------------------------*
076300090811Tipo richiamo errato. Ammessi: "T"=Trattativa e "C"=Tariffe clienti.           1
076400090811Manca il numero trattativa                                                     2
076500090811Manca il codice cliente                                                        3
076600090811Raggiunto il limite massimo di record caricabili nel subfile...                4
076700090811Opzione errata                                                                 5
076800090813La tariffa non ha nessuna nota da copiare                                      6
076900090814Non esistono altre tariffe in cui copiare queste note                          7
