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
001300090811     f                                     rename(TNTAM000 : TNOFM000)
001400090811       // - Tariffe
001500090811     fTNTAM01L  if   e           k disk    usropn
001600090811       // - Note
001700090811     fTFNTC01L  if   e           k disk
001800090811
001900090811       // - Video Gestione
002000090811     fTNTA28D   cf   e             workstn
002100090811     f                                     sfile(TA28S01 : S01nrr)
002200090811     f                                     indds(IndDspF)
002300090811     f                                     infds(InfDspF)
002400090811
002500090811       //--------------------------------------------------------------
002600090811       //?Definizione costanti.                                        ?
002700090811       //--------------------------------------------------------------
002800090811
002900090811       // - Tipo note
003000090811     d c_TipoNota      c                   const('10')
003100090811
003200090811       // - Numero di record in ciascuna videata di subfile
003300090811     d c_SflPag        c                   const(16)
003400090811
003500090811       // - Numero massimo di record gestibili
003600110228     d c_MaxRec        c                   const(9999)
003700090811
003800090811       // - Tasti funzionali a video
003900090811     d c_F01           c                   const(x'31')
004000090811     d c_F02           c                   const(x'32')
004100090811     d c_F03           c                   const(x'33')
004200090811     d c_F04           c                   const(x'34')
004300090811     d c_F05           c                   const(x'35')
004400090811     d c_F06           c                   const(x'36')
004500090811     d c_F07           c                   const(x'37')
004600090811     d c_F08           c                   const(x'38')
004700090811     d c_F09           c                   const(x'39')
004800090811     d c_F10           c                   const(x'3A')
004900090811     d c_F11           c                   const(x'3B')
005000090811     d c_F12           c                   const(x'3C')
005100090811     d c_F13           c                   const(x'B1')
005200090811     d c_F14           c                   const(x'B2')
005300090811     d c_F15           c                   const(x'B3')
005400090811     d c_F16           c                   const(x'B4')
005500090811     d c_F17           c                   const(x'B5')
005600090811     d c_F18           c                   const(x'B6')
005700090811     d c_F19           c                   const(x'B7')
005800090811     d c_F20           c                   const(x'B8')
005900090811     d c_F21           c                   const(x'B9')
006000090811     d c_F22           c                   const(x'BA')
006100090811     d c_F23           c                   const(x'BB')
006200090811     d c_F24           c                   const(x'BC')
006300090811     d c_Enter         c                   const(x'F1')
006400090811     d c_RollDown      c                   const(x'F4')
006500090811     d c_RollUp        c                   const(x'F5')
006600090811
006700090811       //--------------------------------------------------------------
006800090811       //?Definizione schiere.                                         ?
006900090811       //--------------------------------------------------------------
007000090811
007100090811       // - Messaggi di errore
007200090814     d $Msg            s             78    dim( 7) ctdata perrcd(1)
007300090811
007400090811       //--------------------------------------------------------------
007500090811       //?Definizione aree dati.                                       ?
007600090811       //--------------------------------------------------------------
007700090811
007800090811       // - Dati utente
007900090811     d §AzUte        e ds                  extname(AZUTE00F)
008000090811     d                                     dtaara
008100090811     d §DatiUte      e ds                  extname(dDatiUte)
008200090811     d                                     dtaara
008300090811
008400090811       //--------------------------------------------------------------
008500090811       //?Definizione strutture dati.                                  ?
008600090811       //--------------------------------------------------------------
008700090811
008800090811       // - Status ds
008900090811     d Status         sds
009000090811     d  SDSpgm           *proc
009100090811     d  SDSusr               254    263
009200090811
009300090811       // - InfDS
009400090811     d InfDspF         ds
009500090811     d   dsp_aid             369    369a
009600090811     d   sfl_rrn             376    377i 0
009700090811     d   min_nrr             378    379i 0
009800090811     d   num_rcds            380    381i 0
009900090811
010000090811       // - Indicatori su DspF
010100090811     d IndDspF         ds                  inz
010200090811         // - Abilitazione tasti funzionali
010300090813     d   AbilitF05                     n   overlay(IndDspF : 05)
010400090811         // - Emissione messaggio di errore
010500090811     d   ErrMessage                    n   overlay(IndDspF : 28)
010600090811         // - Indicatori di gestione del subfile
010700090811     d   SflDsp_N                      n   overlay(IndDspF : 30)
010800090811     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
010900090811     d   SflNxtChg                     n   overlay(IndDspF : 32)
011000090811     d   SflEnd                        n   overlay(IndDspF : 33)
011100090811         // - Indicatori per Attibuti di visualizzazione
011200090811     d   ProtectOPZ                    n   overlay(IndDspF : 40)
011300090811     d   EvidenzCTR                    n   overlay(IndDspF : 41)
011400090811     d   UnderLineNOT                  n   overlay(IndDspF : 42)
011500090811         // - Posizionamento cursore & segnalazione errore
011600090811     d   PosCurOPZ                     n   overlay(IndDspF : 50)
011700090811         //   - Riemissione videata
011800090811     d   ErrGenerico                   n   overlay(IndDspF : 99)
011900090811
012000090811       // - Parametri ricevuti
012100090811     d KPJBA         e ds
012200090811     d TNTA28ds      e ds                  inz
012300090812
012400090812       // - Parametri x *pgm di manutenzione note della singola tariffa
012500090812     d TNTA29ds      e ds                  inz
012600090811
012700090811       // - Parametri per Reperimento dati utente
012800090811     d TIBS34ds      e ds                  inz
012900090811
013000090811       //--------------------------------------------------------------
013100090811       //?Definizione variabili globali.                               ?
013200090811       //--------------------------------------------------------------
013300090811
013400090811       // - Flags booleani
013500090811     d $Fine           s               n   inz
013600090811     d $InzS01         s               n   inz(*on)
013700090811
013800090811       // - Variabili per la gestione del video
013900090811     d $Video          s              2    inz('S1')
014000090811     d W_SflPag        s              3  0 inz
014100090811     d wPag            s              4  0 inz
014200090811     d wRig            s              3  0 inz
014300090811     d S01nrr          s                   like(C1RcdNbr) inz
014400090811     d SavS01csr       s                   like(C1RcdNbr) inz
014500090811
014600090811       // - Campi di comodo
014700090811     d wRecTarif       s              3  0 inz
014800090908     d wNumTarif       s                   like(C1RcdNbr) inz
014900090811
015000090811       //--------------------------------------------------------------
015100090811       //?Definizione prototipi procedure.                             ?
015200090811       //--------------------------------------------------------------
015300090811
015400090811       // - Reperimento dati utente
015500090811      /copy gaitrasrc/srcProtoPR,TIBS34R
015600090812
015700090812       // - Manutenzione/Cancellazione/Copia/ note singola tariffa
015800090812      /copy gaitrasrc/srcProtoPR,TNTA29R
015900090811
016000090811       //--------------------------------------------------------------
016100090811       //?Definizione key-list.                                        ?
016200090811       //--------------------------------------------------------------
016300090811
016400090811       // - File TNOFM01L / TNTAM01L
016500090811     d k03tntam01    e ds                  extname(TNTAM01L : *key)
016600090811     d                                     prefix(k_)   inz
016700090811
016800090811       // - File TFNTC01L
016900090811     d k04tfntc01    e ds                  extname(TFNTC01L : *key)
017000090811     d                                     prefix(k_)   inz
017100090811
017200090811       //--------------------------------------------------------------
017300090811       //?M A I N - L I N E                                            ?
017400090811       //--------------------------------------------------------------
017500090811
017600090811     c     *Entry        plist
017700090811     c                   parm                    KPJBA
017800090811
017900090811      /free
018000090811
018100090811       //?Operazioni iniziali?
018200090811       exsr sr_RoutInz;
018300090811
018400090811       DoW  $Fine = *off;
018500090811         select;
018600090811           // - Gestione subfile S1
018700090811           when $Video = 'S1';
018800090811             exsr sr_GesS01;
018900090811           other;
019000090811             $Fine = *on;
019100090811         endsl;
019200090811       enddo;
019300090811
019400090811       //?Operazioni finali?
019500090811       exsr sr_RoutEnd;
019600090811
019700090811       //--------------------------------------------------------------
019800090811       //?Operazioni iniziali.                                         ?
019900090811       //--------------------------------------------------------------
020000090811       BEGSR  sr_RoutInz;
020100090811
020200090811         *inLR = *on;
020300090811
020400090811         //?Ricezione parametri?
020500090811         TNTA28ds = kpjbu;
020600090811         oTA28err = *off;
020700090811         clear  oTA28msg;
020800090811
020900090811         //?Controllo dei parametri ricevuti?
021000090811         select;
021100090811           when  iTA28tip <> 'T'   and   iTA28tip <> 'C';
021200090811             oTA28err = *on;
021300090811             oTA28msg = $Msg(01);
021400090811           when  iTA28cod = *zero;
021500090811             oTA28err = *on;
021600090811             if  iTA28tip = 'T';
021700090811               oTA28msg = $Msg(02);
021800090811             else;
021900090811               oTA28msg = $Msg(03);
022000090811             endif;
022100090811         endsl;
022200090811         if  oTA28err = *on;
022300090811           $Fine = *on;
022400090811           leavesr;
022500090811         endif;
022600090811
022700090811         //?Reperimento dati job?
022800090811         exsr  sr_DatiJob;
022900090811
023000090811         //?Impostazione nome programma a video?
023100090811         V1Tpgm = SDSpgm;
023200090811         clear  V1Topz;
023300090811
023400090811       ENDSR;
023500090811
023600090811       //--------------------------------------------------------------
023700090811       //?Reperimento Dati del job (Utente/Operativi).                 ?
023800090811       //--------------------------------------------------------------
023900090811       BEGSR  sr_DatiJob;
024000090811
024100090811         in(e) §AzUte;
024200090811         if NOT %error;
024300090811           in(e) §DatiUte;
024400090811         endif;
024500090811         if %error or RSut = *blank;
024600090811           tibs34r ( tibs34ds );
024700090811           in §AzUte;
024800090811           in §DatiUte;
024900090811         endif;
025000090811
025100090811       ENDSR;
025200090811
025300090811       //--------------------------------------------------------------
025400090811       //?Gestione subfile S01                                         ?
025500090811       //--------------------------------------------------------------
025600090811       BEGSR  sr_GesS01;
025700090811
025800090811         //?Inizializzazione subfile?
025900090813         if  $InzS01 = *on;
026000090813           if  NOT ErrMessage;
026100090813             exsr  sr_InzS01;
026200090813             $InzS01 = *off;
026300090813           else;
026400090813             AbilitF05 = *on;
026500090813           endif;
026600090811         endif;
026700090811
026800090811         //?Emissione Testata e Piede con tasti funzionali abilitati?
026900090811         write  TA28T01;
027000090811         write  TA28P01;
027100090811
027200090811         //?Posizionamento cursore?
027300090811         if  C1CsrRrn > *zero;
027400090811           C1RcdNbr = C1CsrRrn;
027500090811         else;
027600090811           C1RcdNbr = 1;
027700090811           //write TA28S00;              // (no rec.)
027800090811         endif;
027900090811
028000090811         //?Emissione videata?
028100090811         exfmt  TA28C01;
028200090811         reset  ErrMessage;
028300090811         reset  ErrGenerico;
028400090811         clear  V1Dmsg;
028500090811
028600090811         SELECT;
028700090811
028800090811           //?F3=Fine?
028900090811           WHEN  dsp_aid = c_F03;
029000090811             $Fine = *on;
029100090813
029200090813           //?F5=Aggiornamento sfl?
029300090813           WHEN  dsp_aid = c_F05;
029400090813             $InzS01 = *on;
029500090813             AbilitF05 = *off;
029600090811
029700090811           //?Roll-Up?
029800090811           WHEN  dsp_aid = c_RollUp;
029900090811             exsr sr_RollUpS01;
030000090811
030100090811           //?SubFile vuoto?
030200090811           WHEN  S01nrr = *zero;
030300090811
030400090811           //?Invio?
030500090811           OTHER;
030600090811             // -?Gestione opzioni?
030700090811             exsr  sr_OpzS01;
030800090811             if  ErrGenerico;
030900090811               leavesr;
031000090811             endif;
031100090811
031200090811         ENDSL;
031300090811
031400090811       ENDSR;
031500090811
031600090811       //--------------------------------------------------------------
031700090811       //?Inizializzazione subfile S01                                 ?
031800090811       //--------------------------------------------------------------
031900090811       BEGSR  sr_InzS01;
032000090811
032100090811         //?Spegnimento degli indicatori di errore?
032200090811         %subst(IndDspF : 50) = *off;
032300090811
032400090811         //?Impostazione campi nel subfile-control?
032500090811         if  iTA28tip = 'T';
032600090908           C1txt1 = 'Trattativa n°';
032700090908           C1txt2 = %editc(iTA28cod : 'Z');
032800090811         else;
032900090908           C1txt1 = 'Cliente ....:';
033000090908           C1txt2 = %editc(iTA28cod : 'X');
033100090811         endif;
033200090908         C1txt2 = %trimr(C1txt2)
033300090811                  + '  '
033400090811                  + iTA28rsc;
033500090915         select;
033600090915           when  iTA28ela = 'I';
033700090915             C1Topz = '5=Visualizzazione';
033800090915           when  iTA28ela = 'M';
033900090915             C1Topz = 'G=Gestione, C=Copia';
034000090915           other;
034100090915             clear  C1Topz;
034200090915         endsl;
034300090811
034400090811         //?Pulizia subfile?
034500090811         SflDsp_N    = *on;
034600090811         SflDspCtl_N = *on;
034700090811         write  TA28C01;
034800090811         SflDspCtl_N = *off;
034900090811         SflEnd      = *off;
035000090811
035100090908         clear  wNumTarif;
035200090811         clear  W_SflPag;
035300090811         clear  C1RcdNbr;
035400090811         clear  C1CsrRrn;
035500090811         clear  S01nrr;
035600090811         clear  V1Dmsg;
035700090811         ErrMessage  = *off;
035800090811         ErrGenerico = *off;
035900090813         clear  NTCnk1;
036000090813         clear  NTCnk2;
036100090811
036200090811         //?Caricamento subfile con note "10"?
036300091120
036400091120         //?prima cerco con solo codice cliente o offerta?
036500091120         exsr  sr_NotaNOctr;
036600091120
036700091120         //?poi cerco con codice tariffa?
036800090813         if  iTA28tip = 'T'   and   %open(TNOFM01L);
036900090813           close  TNOFM01L;
037000090813         endif;
037100090813         if  iTA28tip = 'C'   and   %open(TNTAM01L);
037200090813           close  TNTAM01L;
037300090813         endif;
037400090811         k_TAMksc = iTA28cod;
037500090813         k_TAMctr = *loval;
037600090811         k_TAMprg = *loval;
037700090811         if  iTA28tip = 'T';
037800090811           exsr  sr_ReadTNOFM;
037900090811         else;
038000090811           exsr  sr_ReadTNTAM;
038100090811         endif;
038200090811         exsr  sr_RollUpS01;
038300090811
038400090811         //?Impaginazione subfile?
038500090811         // -> forza l'impaginazione sul 1° rec. del subfile
038600090811         if S01nrr > *zero;
038700090811           C1RcdNbr  = 1;
038800090811           C1CsrRrn  = 1;
038900090811         else;
039000090811           clear C1RcdNbr;
039100090811         endif;
039200090811
039300090811       ENDSR;
039400091120
039500091120       //--------------------------------------------------------------
039600091120       //?Lettura Nota 10 senza TARIFFA                                ?
039700091120       //--------------------------------------------------------------
039800091120       BEGSR  sr_NotaNOctr;
039900091120
040000091120         S01nrr    = (W_SflPag * c_SflPag);
040100091120
040200091120         if  iTA28tip = 'T';           // T = Trattativa
040300091120           k_NTCapl = 'T';
040400091120         else;                         // C = Tariffe cliente
040500091120           k_NTCapl = iTA28tip;
040600091120         endif;
040700091120         k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( ITA28cod : 'X' );
040800091120         clear k_NTCnk2;
040900091120         k_NTCtnt = c_TipoNota;
041000091120
041100091120         setll  %kds(k04tfntc01)  TFNTC;
041200091120         IF  not %equal(tfntc01l);
041300091120           clear S1Cnot;
041400091120         ELSE;
041500091120           reade  %kds(k04tfntc01)  TFNTC;
041600091120           IF  not %eof(tfntc01l);
041700091120             S1Cnot = NTCrnt;
041800091120           ENDIF;
041900091120         ENDIF;
042000091120           wNumTarif += 1;
042100091120           S1Cctr = 'CLI';
042200091120           // -?Impostazione attributi di visualizzazione?
042300091120           exsr  sr_AttrS01;
042400091120           // -?Scrittura record nerl subfile?
042500091120           S01nrr += 1;
042600091120           write  TA28S01;
042700091120           leavesr;
042800091120
042900091120       ENDSR;
043000090811
043100090811       //--------------------------------------------------------------
043200090811       //?Lettura singolo record dal file TNOFM01L                     ?
043300090811       //--------------------------------------------------------------
043400090811       BEGSR  sr_ReadTNOFM;
043500090811
043600090811         if  not %open(TNOFM01L);
043700090811           open  TNOFM01L;
043800090811           setll  %kds(k03tntam01)  TNOFM000;
043900090811         endif;
044000090811
044100090811         DoU  %eof(TNOFM01L)   or   TAMatb = *blank;
044200090811           reade  %kds(k03tntam01 : 1)  TNOFM000;
044300090811         EndDo;
044400090811
044500090811         // - Lettura 1ª singola nota dal file TFNTC01L
044600090811         if  not %eof(TNOFM01L);
044700090908           wNumTarif += 1;
044800090811           exsr  sr_ReadTFNTC;
044900090811         endif;
045000090811
045100090811         clear  wRecTarif;
045200090811
045300090811       ENDSR;
045400090811
045500090811       //--------------------------------------------------------------
045600090811       //?Lettura singolo record dal file TNTAM01L                     ?
045700090811       //--------------------------------------------------------------
045800090811       BEGSR  sr_ReadTNTAM;
045900090811
046000090811         if  not %open(TNTAM01L);
046100090811           open  TNTAM01L;
046200090811           setll  %kds(k03tntam01)  TNTAM000;
046300090811         endif;
046400090811
046500090811         DoU  %eof(TNTAM01L)   or   TAMatb = *blank;
046600090811           reade  %kds(k03tntam01 : 1)  TNTAM000;
046700090811         EndDo;
046800090811
046900090811         // - Lettura 1ª singola nota dal file TFNTC01L
047000090811         if  not %eof(TNTAM01L);
047100090908           wNumTarif += 1;
047200090811           exsr  sr_ReadTFNTC;
047300090811         endif;
047400090811
047500090811         clear  wRecTarif;
047600090811
047700090811       ENDSR;
047800090811
047900090811       //--------------------------------------------------------------
048000090908       //?Lettura singolo record file TFNTC01L                         ?
048100090811       //--------------------------------------------------------------
048200090811       BEGSR  sr_ReadTFNTC;
048300090811
048400090811         if  NTCnk2 <> %editc( TAMctr : 'X' );
048500090811
048600090811           if  iTA28tip = 'T';           // T = Trattativa
048700091118             k_NTCapl = 'T';
048800090811           else;                         // C = Tariffe cliente
048900090811             k_NTCapl = iTA28tip;
049000090811           endif;
049100090811           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( TAMksc : 'X' );
049200090811           k_NTCnk2 = %editc( TAMctr : 'X' );
049300090811           k_NTCtnt = c_TipoNota;
049400090811
049500090811           setll  %kds(k04tfntc01)  TFNTC;
049600090811
049700090811         endif;
049800090811
049900090811         reade  %kds(k04tfntc01)  TFNTC;
050000090811
050100090811         if  %eof(TFNTC01L);
050200090811           clear  NTCrnt;
050300090811           k_TAMctr = TAMctr;
050400090811           k_TAMprg = *hival;
050500090811           if  iTA28tip = 'T';
050600090811             setgt  %kds(k03tntam01 : 2)  TNOFM000;
050700090811           else;
050800090811             setgt  %kds(k03tntam01 : 2)  TNTAM000;
050900090811           endif;
051000090811         endif;
051100090811
051200090811       ENDSR;
051300090811
051400090811       //--------------------------------------------------------------
051500090811       //?Caricamento singola pagina nel subfile S01                   ?
051600090811       //--------------------------------------------------------------
051700090811       BEGSR  sr_RollUpS01;
051800091120         // S01nrr    = (W_SflPag * c_SflPag);
051900090811         W_SflPag += 1;
052000090811         SflNxtChg = *off;
052100090811
052200090811         //?Ciclo di caricamento dati dai files?
052300090811         //?TNOFM01L/TNTAM01L e TFNTC01L?
052400090811         DoW  S01nrr   < (W_SflPag * c_SflPag)   and
052500090811              S01nrr   < c_MaxRec                and
052600090811             (iTA28tip = 'T'   and   Not  %eof(TNOFM01L)   or
052700090811              iTA28tip = 'C'   and   Not  %eof(TNTAM01L));
052800090811
052900090811           // -?Registrazione singolo record nel subfile?
053000090811           clear  TA28S01;
053100090811           S1Cctr = %editc( TAMctr : 'X' );
053200090811           S1Cnot = NTCrnt;
053300090811           // - Campi hidden
053400090811           wRecTarif += 1;
053500090811           H1in40 = (wRecTarif > 1);
053600090811
053700090811           // -?Impostazione attributi di visualizzazione?
053800090811           exsr  sr_AttrS01;
053900090811
054000090811           // -?Scrittura record nerl subfile?
054100090811           S01nrr += 1;
054200090811           write  TA28S01;
054300090811
054400090811           // -?Lettura record successivo?
054500090811           if  not %eof(TFNTC01L);
054600090811             exsr  sr_ReadTFNTC;
054700090811           endif;
054800090811           select;
054900090811             when  %eof(TFNTC01L)  and  iTA28tip = 'T';
055000090811               exsr  sr_ReadTNOFM;
055100090811             when  %eof(TFNTC01L)  and  iTA28tip = 'C';
055200090811               exsr  sr_ReadTNTAM;
055300090811           endsl;
055400090811
055500090811         EndDo;
055600090811
055700090811         //?Visualizzazione del SFL (se ci sono dati)?
055800090811         SflDsp_N  = (S01nrr <= *zero);
055900090811
056000090811         //?Attivazione del SFLEND?
056100090811         SflEnd = (S01nrr  >= c_MaxRec  or
056200090811                  (iTA28tip = 'T'  and  %eof(TNOFM01L))  or
056300090811                  (iTA28tip = 'C'  and  %eof(TNTAM01L)));
056400090811         if  S01nrr  >= c_MaxRec   and
056500090811            (iTA28tip = 'T'  and  NOT %eof(TNOFM01L)   or
056600090811             iTA28tip = 'C'  and  NOT %eof(TNTAM01L));
056700090811           V1Dmsg = $Msg(04);
056800090811           ErrMessage  = *on;
056900090811           ErrGenerico = *on;
057000090811         endif;
057100090811
057200090811         //?Posizionamento cursore al 1° rcd della pagina?
057300090811         if  S01nrr > *zero;
057400090811           wPag     = S01nrr / c_SflPag;
057500090811           wRig     = S01nrr - (c_SflPag * wPag);
057600090811           C1RcdNbr = wPag * c_SflPag;
057700090811           if  wRig > *zero;
057800090811             C1RcdNbr += 1;
057900090811           else;
058000090811             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
058100090811           endif;
058200090811         else;
058300090811           clear  C1RcdNbr;
058400090811         endif;
058500090811
058600090811         C1CsrRrn = C1RcdNbr;
058700090811
058800090811       ENDSR;
058900090811
059000090811       //--------------------------------------------------------------
059100090811       //?Impostazione attributi di visualizzazione nel rec. del sfl   ?
059200090811       //--------------------------------------------------------------
059300090811       BEGSR  sr_AttrS01;
059400090811
059500090811         // - Protezione campo opzione
059600090812         ProtectOPZ   = (H1in40 = *on);
059700090811         // - Evidenziazione codice tariffa (se ricevuta)
059800090811         EvidenzCTR   = (S1Cctr = iTA28ctr);
059900090811         // - Evidenziazione mancanza di note per la tariffa
060000090811         UnderLineNOT = (S1Cnot = *blank);
060100090811
060200090811       ENDSR;
060300090811
060400090811       //--------------------------------------------------------------
060500090811       //?Gestione opzioni del subfile S01                             ?
060600090811       //--------------------------------------------------------------
060700090811       BEGSR  sr_OpzS01;
060800090811
060900090811         clear  SavS01csr;
061000090811
061100090811         //?Ciclo di lettura subfile?
061200090811         readc  TA28S01;
061300090811
061400090811         DoW  Not %eof(TNTA28D);
061500090811
061600090811           SflNxtChg = *off;
061700090811           %subst(IndDspF : 50) = *off;
061800090811           C1RcdNbr = S01nrr;
061900090811
062000090811           select;
062100090915
062200090915             // -?5=Visualizzazione?
062300090915             when  S1Copz = '5'   and   iTA28ela = 'I';
062400090915               clear  TNTA29ds;
062500090915               iTA29opz = '5';
062600090915               exsr  sr_CallTNTA29;
062700090811
062800090811             // -?G=Gestione?
062900090915             when  S1Copz = 'G'   and   iTA28ela = 'M';
063000090812               clear  TNTA29ds;
063100090813               iTA29opz = '2';
063200090813               exsr  sr_CallTNTA29;
063300090811
063400090813             // -?C=Copia?
063500090915             when  S1Copz = 'C'   and   iTA28ela = 'M';
063600090814               select;
063700090814                 when  S1Cnot = *blank;
063800090814                   ErrMessage  = *on;
063900090814                   ErrGenerico = *on;
064000090814                   PosCurOPZ   = *on;
064100090814                   V1Dmsg = $Msg(06);
064200090908                 when  wNumTarif <= 1;
064300090814                   ErrMessage  = *on;
064400090814                   ErrGenerico = *on;
064500090814                   PosCurOPZ   = *on;
064600090814                   V1Dmsg = $Msg(07);
064700090814                 other;
064800090814                   clear  TNTA29ds;
064900090814                   iTA29opz = '3';
065000090814                   exsr  sr_CallTNTA29;
065100090814               endsl;
065200090811
065300090811             // -?Nessuna opzione?
065400090811             when  S1Copz = *blank;
065500090811
065600090811             // -?Opzione errata?
065700090811             other;
065800090811               ErrMessage  = *on;
065900090811               ErrGenerico = *on;
066000090811               PosCurOPZ   = *on;
066100090811               V1Dmsg = $Msg(05);
066200090811
066300090811           endsl;
066400090811
066500090811           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
066600090811           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
066700090811           if  S1Copz  <> *blank   and
066800090811              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
066900090811             SavS01csr   = C1RcdNbr;
067000090811           endif;
067100090811
067200090811           // -?ReImpostazione attributi di visualizzazione?
067300090811           exsr  sr_AttrS01;
067400090811
067500090811           // -?Aggiornamento sfl?
067600090811           select;
067700090811             when  ErrMessage;
067800090811               SflNxtChg = *on;
067900090811               C1CsrRrn  = C1RcdNbr;
068000090811             when  ErrGenerico;
068100090811               C1CsrRrn  = C1RcdNbr;
068200090811               clear  S1Copz;
068300090811             other;
068400090811               clear  S1Copz;
068500090811           endsl;
068600090811           UPDATE  TA28S01;
068700090811           if  ErrGenerico   or   ErrMessage;
068800090811             leave;
068900090811           endif;
069000090811
069100090811           readc  TA28S01;
069200090811
069300090811         ENDDO;
069400090811
069500090811         // -?Riposiziono il cursore sul record contenente la prima opz.?
069600090811         //  ?(se non sono stati rilevati errori)?
069700090811         if  NOT ErrMessage   and   NOT ErrGenerico   and
069800090811             SavS01csr > *zero;
069900090811           C1CsrRrn = SavS01csr;
070000090811         endif;
070100090811
070200090811       ENDSR;
070300090813
070400090813       //--------------------------------------------------------------
070500090813       //?Richiamo Inserimento/Manutenzione/Copia note tariffa         ?
070600090813       //--------------------------------------------------------------
070700090813       BEGSR  sr_CallTNTA29;
070800090813
070900090813         //clear  TNTA29ds;            (già fatto)
071000090813         //iTA29opz = '?';             (già valorizzato)
071100090813         iTA29tip = iTA28tip;
071200090813         iTA29cod = iTA28cod;
071300090813         iTA29rsc = iTA28rsc;
071400090813         iTA29ctr = S1Cctr;
071500090813
071600090813         kpjbu    = TNTA29ds;
071700090813
071800090813         tnta29r ( kpjba );
071900090813
072000090813         TNTA29ds = kpjbu;
072100090813
072200090813         select;
072300090813           // - Restituito errore
072400090813           when oTA29err = *on;
072500090813             ErrMessage  = *on;
072600090813             ErrGenerico = *on;
072700090813             PosCurOPZ   = *on;
072800090813             V1Dmsg = oTA29msg;
072900090813           // - Non premuto F12
073000090813           when  oTA29fxx <> '12';
073100090813             $InzS01 = *on;
073200090813         endsl;
073300090813
073400090813       ENDSR;
073500090811
073600090811       //--------------------------------------------------------------
073700090811       //?Operazioni finali.                                           ?
073800090811       //--------------------------------------------------------------
073900090811       BEGSR  sr_RoutEnd;
074000090811
074100090811         kpjbu = TNTA28ds;
074200090811
074300090811         return;
074400090811
074500090811       ENDSR;
074600090811
074700090811      /end-free
074800090811
074900090811** - $Msg -------------------------------------------------------------------*
075000090811Tipo richiamo errato. Ammessi: "T"=Trattativa e "C"=Tariffe clienti.           1
075100090811Manca il numero trattativa                                                     2
075200090811Manca il codice cliente                                                        3
075300090811Raggiunto il limite massimo di record caricabili nel subfile...                4
075400090811Opzione errata                                                                 5
075500090813La tariffa non ha nessuna nota da copiare                                      6
075600090814Non esistono altre tariffe in cui copiare queste note                          7
