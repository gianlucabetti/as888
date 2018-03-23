000100090609       //--------------------------------------------------------------
000200090609       //?TRMK21R * Interrogazione attività contatti                   ?
000300090609       //--------------------------------------------------------------
000400090609
000500090609     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600100617     h dftactgrp(*no) actgrp(*caller)
000700090609     h alwnull(*inputonly)
000800090609
000900090609       //--------------------------------------------------------------
001000090609       //?Dichiarazione file.                                          ?
001100090609       //--------------------------------------------------------------
001200090609
001300090609       // - Trattative a clienti
001400091019     fTIVIS05L  if   e           k disk
001500090610
001600090610       // - Tabelle
001700090610     fTABEL00F  if   e           k disk
001800110211
001900110211       // - Potenziale
002000110211     fTNCPO01L  if   e           k disk
002100090609
002200090609       // - Video Gestione
002300090609     fTRMK21D   cf   e             workstn
002400090609     f                                     sfile(MK21S01 : S01nrr)
002500090609     f                                     indds(IndDspF)
002600090609     f                                     infds(InfDspF)
002700090609
002800090609       //--------------------------------------------------------------
002900090609       //?Definizione costanti.                                        ?
003000090609       //--------------------------------------------------------------
003100090609
003200090609      // - Numero di record in ciascuna videata di subfile
003300090609     d c_SflPag        c                   const(15)
003400090609
003500090609      // - Numero massimo di record gestibili
003600110301     d c_MaxRec        c                   const(9999)
003700090609
003800090609       // - Tasti funzionali a video
003900090609     d c_F01           c                   const(x'31')
004000090609     d c_F02           c                   const(x'32')
004100090609     d c_F03           c                   const(x'33')
004200090609     d c_F05           c                   const(x'35')
004300090609     d c_F06           c                   const(x'36')
004400090609     d c_F07           c                   const(x'37')
004500090609     d c_F08           c                   const(x'38')
004600090609     d c_F09           c                   const(x'39')
004700090609     d c_F10           c                   const(x'3A')
004800090609     d c_F12           c                   const(x'3C')
004900090609     d c_F13           c                   const(x'B1')
005000090609     d c_F14           c                   const(x'B2')
005100090609     d c_F15           c                   const(x'B3')
005200090609     d c_F16           c                   const(x'B4')
005300090609     d c_F17           c                   const(x'B5')
005400090609     d c_F18           c                   const(x'B6')
005500090609     d c_F19           c                   const(x'B7')
005600090609     d c_F20           c                   const(x'B8')
005700090609     d c_F21           c                   const(x'B9')
005800090609     d c_F22           c                   const(x'BA')
005900090609     d c_F23           c                   const(x'BB')
006000090609     d c_F24           c                   const(x'BC')
006100090609     d c_Enter         c                   const(x'F1')
006200090609     d c_RollDown      c                   const(x'F4')
006300090609     d c_RollUp        c                   const(x'F5')
006400090609
006500090609       // - Attributi di Visualizzazione
006600090609     d c_DspAtrNorm    c                   const(x'A0')
006700090610     d c_DspAtrRed     c                   const(x'A8')
006800090609
006900090609       //--------------------------------------------------------------
007000090609       //?Definizione schiere.                                         ?
007100090609       //--------------------------------------------------------------
007200090609
007300090609       // - Messaggi di errore
007400100616     d $Msg            s             78    dim(10) ctdata perrcd(1)
007500090609
007600090609       //--------------------------------------------------------------
007700090609       //?Definizione aree dati.                                       ?
007800090609       //--------------------------------------------------------------
007900090609
008000090609       // - Dati utente
008100090609     d §AzUte        e ds                  extname(AZUTE00F)
008200090609     d                                     dtaara
008300090609     d §DatiUte      e ds                  extname(dDatiUte)
008400090609     d                                     dtaara
008500090609
008600090609       //--------------------------------------------------------------
008700090609       //?Definizione strutture dati.                                  ?
008800090609       //--------------------------------------------------------------
008900090609
009000090609       // - Status ds
009100090609     d Status         sds
009200090609     d  SDSpgm           *proc
009300090609     d  SDSusr               254    263
009400090609
009500090609       // - InfDS
009600090609     d InfDspF         ds
009700090609     d   dsp_aid             369    369a
009800090609     d   sfl_rrn             376    377i 0
009900090609     d   min_nrr             378    379i 0
010000090609     d   num_rcds            380    381i 0
010100090609
010200090609       // - Indicatori su DspF
010300090609     d IndDspF         ds                  inz
010400090609         // - Emissione messaggio di errore
010500090609     d   ErrMessage                    n   overlay(IndDspF : 28)
010600090610         // - Condizionamento di visualizzazione
010700090610     d   VisualTratt                   n   overlay(IndDspF : 41)
010800100616     d   VisGestione                   n   overlay(IndDspF : 42)
010900100809     d   VisNewAtt                     n   overlay(IndDspF : 43)
011000100809     d   VisNewTra                     n   overlay(IndDspF : 44)
011100100928     d   VisDaMk19                     n   overlay(IndDspF : 45)
011200110211     d   Codificato                    n   overlay(IndDspF : 46)
011300090609         // - Indicatori di gestione del subfile
011400090609     d   SflDsp_N                      n   overlay(IndDspF : 30)
011500090609     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011600090609     d   SflNxtChg                     n   overlay(IndDspF : 32)
011700090609     d   SflEnd                        n   overlay(IndDspF : 33)
011800090609         // - Posizionamento cursore & segnalazione errore
011900090609     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012000090609         //   - Riemissione videata
012100090609     d   ErrGenerico                   n   overlay(IndDspF : 99)
012200090609
012300090609       // - Parametri ricevuti
012400090609     d KPJBA         e ds
012500090610     d TRMK21ds      e ds                  inz
012600100416     d TRMK01ds      e ds                  inz
012700090610
012800090610       // - Dati estratti via SQL
012900090610     d TIATCds       e ds                  extname(TIATC00F)  inz
013000090609
013100090609       // - Parametri per Reperimento dati utente
013200090609     d TIBS34ds      e ds                  inz
013300100616
013400100616      // - controllo abilitazioni
013500100616     d TNTAA1DS      e ds                  inz
013600100616
013700100616      // - Gestione trattative commerciali
013800100616     d TNTA88ds      e ds                  inz
013900100809
014000100809       // - Primo contatto
014100100809     d TRMK17DS      e ds
014200090611
014300090611       // - Gestione/Visualizzazione contatti commerciali
014400100616     d TRMK19ds      e ds
014500100617
014600100617       // - Gestione/Visualizzazione contatti commerciali
014700100617     d TRMK19ds1     e ds                  extname(TRMK19DS) prefix(w_)
014800090609
014900090609       // - Gestione note clienti/potenziali/trattative
015000090609     d TRMK20ds      e ds                  inz
015100090609     d   IMK20tla    e                     inz('L')
015200090609     d   IMK20flm    e                     inz('I')
015300100616
015400100616      // -  Controllo se ci sono altre attività aperte
015500100616     d TRMK85ds      e ds                  inz
015600090609
015700090609       // - Ricerca/Controllo tabelle
015800090609     d TIBS02ds      e ds                  inz
015900090609     d   T02Mod      e                     inz('C')
016000090609
016100090609       // - Tabella "CCO" = Causale contatto
016200090609     d dCCO          e ds                  inz
016300090609
016400090609       // - Tabella "01" = Codici Commerciali
016500090609     d ds01          e ds                  inz
016600090609
016700090609       //--------------------------------------------------------------
016800090609       //?Definizione variabili globali.                               ?
016900090609       //--------------------------------------------------------------
017000090609
017100090609       // - Flags booleani
017200090609     d $Fine           s               n   inz
017300090609     d $EoF            s               n   inz
017400090609     d $Err            s               n   inz
017500090609     d $InzS01         s               n   inz(*on)
017600100616     d $InzW01         s               n   inz
017700090609
017800090609       // - Variabili per la gestione del video
017900090609     d $Video          s              2    inz('S1')
018000090609     d W_SflPag        s              3  0 inz
018100090609     d wPag            s              4  0 inz
018200090609     d wRig            s              3  0 inz
018300090609     d S01nrr          s                   like(C1RcdNbr) inz
018400090609     d SavS01csr       s                   like(C1RcdNbr) inz
018500090609
018600090609       // - Stringa SQL da eseguire
018700090609     d wSQL            s           2048    Varying        inz
018800090609
018900090609       // - Parametri per API QCAPCMD (Process Commands)
019000090609      /copy qsysinc/qrpglesrc,QCAPCMD
019100090609     d Qcmd            s            128    Varying        inz
019200090609
019300090609       // - Parametri gestione errori API.
019400090609      /copy qsysinc/qrpglesrc,QUSEC
019500090609
019600090609       // - Campi di comodo
019700090610     d wDate_dmy       s               d   datfmt(*dmy)   inz
019800100913     d wEsci           s              1    inz
019900090609
020000090609       //--------------------------------------------------------------
020100090609       //?Definizione prototipi procedure.                             ?
020200090609       //--------------------------------------------------------------
020300100616
020400100616     d TRMK85R         pr                  extpgm('TRMK85R')
020500100616     d  trmk85ds                           likeds(trmk85ds)
020600100809
020700100809      // - Inserimento attività primo contatto
020800100809     d trmk17r         pr                  extpgm('TRMK17R')
020900100809     d  kpjba                              likeds(KPJBA)
021000100809     d  trmk17ds                           likeds(TRMK17ds)
021100100617
021200100617      // - Inserimento attività
021300100617     d trmk19c         pr                  extpgm('TRMK19C')
021400100617     d  kpjba                              likeds(KPJBA)
021500100617     d  trmk19ds                           likeds(TRMK19ds)
021600090609
021700090609       // - Reperimento dati utente
021800090609      /copy gaitrasrc/srcProtoPR,TIBS34R
021900090609
022000090609       // - Ricerca/Controllo tabelle
022100090609      /copy gaitrasrc/srcProtoPR,TIBS02R
022200100616
022300100616       // - Controllo se utente abilitato
022400100616      /copy gaitrasrc/srcprotopr,tntaa1c
022500100616
022600100616       // - Gestione trattative
022700100616      /copy gaitrasrc/srcprotopr,tnta88r
022800090611
022900090611       // - Gestione/Visualizzazione contatti commerciali
023000100928      /copy gaitrasrc/srcProtoPR,TRMK19R
023100090609
023200090609       // - Gestione note clienti/potenziali/trattative
023300090609      /copy gaitrasrc/srcProtoPR,TRMK20R
023400090609
023500090609       // - Api QCAPCMD (Process Commands)
023600090609      /copy gaitrasrc/srcProtoPR,QCAPCMD
023700090609
023800090609       //--------------------------------------------------------------
023900090609       //?Definizione key-list.                                        ?
024000090609       //--------------------------------------------------------------
024100090609
024200090609
024300090609       //--------------------------------------------------------------
024400090609       //?M A I N - L I N E                                            ?
024500090609       //--------------------------------------------------------------
024600090609
024700090609     c     *Entry        plist
024800090609     c                   parm                    KPJBA
024900100617     c                   parm                    TRMK19ds1
025000090609
025100090609      /free
025200090609
025300090609       //?Operazioni iniziali?
025400090609       exsr sr_RoutInz;
025500090609
025600090609       DoW  $Fine = *off;
025700090609         select;
025800100616           // - Controllo se altre attività aperte
025900100616           when $Video = 'W1';
026000100616             exsr sr_GesW01;
026100090609           // - Gestione subfile
026200090609           when $Video = 'S1';
026300090609             exsr sr_GesS01;
026400090609           other;
026500090609             $Fine = *on;
026600090609         endsl;
026700090609       enddo;
026800090609
026900090609       //?Operazioni finali?
027000090609       exsr sr_RoutEnd;
027100090609
027200090609       //--------------------------------------------------------------
027300090609       //?Operazioni iniziali.                                         ?
027400090609       //--------------------------------------------------------------
027500090609       BEGSR  sr_RoutInz;
027600090609
027700090609         *inLR = *on;
027800090610
027900090610         //?Impostazione opzioni per SQL?
028000090610         exec sql   set  option  DynUsrPrf = *Owner,
028100090610                                 CloSqlCsr = *EndMod;
028200100616
028300100616       //?Controllo se passati + parametri
028400100616       //?forzo gestione
028500100809        IF  %parms > 1 and w_IMK19tco = 'T';
028600100810          //I21mod = 'G';
028700100809          VisNewAtt = *on;
028800100809          VisNewTra = *off;
028900100616        ENDIF;
029000100809        IF  %parms > 1 and w_IMK19tco = 'O';
029100100810          //I21mod = 'G';
029200100809          VisNewAtt = *off;
029300100809          VisNewTra = *on;
029400100809        ENDIF;
029500090609
029600090609         //?Reperimento dati job?
029700090609         exsr  sr_DatiJob;
029800090609
029900090609         //?Impostazione nome programma a video?
030000090609         V1Tpgm = SDSpgm;
030100090610
030200090610         //?Impostazione parametri ricevuti?
030300090610         if  kpjbu <> *blank;
030400090610           TRMK21ds = kpjbu;
030500090610         else;
030600090610           $Fine = *on;
030700090610         endif;
030800100810
030900100810       //?Se richiamato in modalità 'V' devo attivare la gestione e la modifica +
031000100810       //?tasti funzionali
031100100810         IF  I21mod = 'V';
031200100810           VisGestione = *on;
031300100810         ENDIF;
031400100616
031500100616       //?Se richiamato in modalità 'G' imposto $Video = 'W1'
031600100616         IF  I21mod = 'G';
031700100616           $Video = 'W1';
031800100616           $InzS01 = *off;
031900100616           $InzW01 = *on;
032000100616         ENDIF;
032100100928
032200100928       //?Se richiamato in modalità '9' arrivo da TRMK19R visualizzo solo come se
032300100928       //?richiamato con 'I', ma non do la possibilità di interrogare le attività con
032400100928       //?opzione 5
032500100928         IF  I21mod = '9';
032600100928           VisDaMk19 = *on;
032700100928         ENDIF;
032800110211
032900110211       //?Aggancio il potenziale per reperire la categoria
033000110211         Codificato = *off;
033100110211         IF  I21cpo > 0 and I21ksc = 0;
033200110211           chain  I21cpo  TNCPO01L;
033300110211           IF  %found(TNCPO01L);
033400110211             Codificato = (CPOfls = 'C');
033500110211           ENDIF;
033600110211         ENDIF;
033700090609
033800090609       ENDSR;
033900090609
034000090609       //--------------------------------------------------------------
034100090609       //?Reperimento Dati del job (Utente/Operativi).                 ?
034200090609       //--------------------------------------------------------------
034300090609       BEGSR  sr_DatiJob;
034400090609
034500090609         in(e) §AzUte;
034600090609         if NOT %error;
034700090609           in(e) §DatiUte;
034800090609         endif;
034900090609         if %error or RSut = *blank;
035000090609           tibs34r ( tibs34ds );
035100090609           in §AzUte;
035200090609           in §DatiUte;
035300090609         endif;
035400090609
035500090609       ENDSR;
035600090609
035700090609       //--------------------------------------------------------------
035800090609       //?Preparazione stringa SQL                                     ?
035900090609       //--------------------------------------------------------------
036000090609       BEGSR  sr_PrepSQL;
036100090609
036200090618         wSQL = 'select TIATC00F.*, case +
036300090622                           when (ATCdco*1000000)+ATChco > 0 +
036400090618                           then (ATCdco*1000000)+ATChco +
036500090618                           else (ATCdad*1000000)+ATChda +
036600090618                           end  as WWWord +
036700090609                   from TIATC00F +
036800090610                  where';
036900090610
037000090610         select;
037100090610           when  I21ksc > *zero;
037200090610             wSQL += ' ATCksc = ' + %editc(I21ksc:'X');
037300090610           when  I21cpo > *zero;
037400090610             wSQL += ' ATCcpo = ' + %editc(I21cpo:'X');
037500090610           when  I21ntc > *zero;
037600090610             wSQL += ' ATCnrv = ' + %editc(I21ntc:'X');
037700090610         endsl;
037800090610
037900090622         wSQL += ' order by WWWord desc +
038000090617
038100090617                   for fetch only';
038200090609
038300090609       ENDSR;
038400090609
038500090609       //--------------------------------------------------------------
038600090609       //?Apertura cursore.                                            ?
038700090609       //--------------------------------------------------------------
038800090609       BEGSR  sr_OpenCursor;
038900090609
039000090609         //?Dichiarazione cursore?
039100090609         exec sql   prepare S1   from :wSQL;
039200090609         exec sql   declare C1   cursor for S1;
039300090609
039400090609         //?Apertura del cursore?
039500090609         exec sql   open C1;
039600090609
039700090609         if  SQLcode < *zero;
039800090609           $Err = *on;
039900090609           $EoF = *on;
040000090609           exsr  sr_PrintErrSQL;
040100090609         endif;
040200090609
040300090609       ENDSR;
040400090609
040500090609       //--------------------------------------------------------------
040600090609       //?Chiusura cursore.                                            ?
040700090609       //--------------------------------------------------------------
040800090609       BEGSR  sr_CloseCursor;
040900090609
041000090609         //?Chiusura del cursore?
041100090609         exec sql   close C1;
041200090609
041300090609       ENDSR;
041400090609
041500090609       //--------------------------------------------------------------
041600090609       //?Lettura cursore.                                             ?
041700090609       //--------------------------------------------------------------
041800090609       BEGSR  sr_ReadCursor;
041900090609
042000090609         clear  TIATCds;
042100090609
042200090609         exec sql   fetch next   from C1   into :TIATCds;
042300090609
042400090609         select;
042500090609           when  SQLcode = 100;
042600090609             $EoF = *on;
042700090609           when  SQLcode < *zero;
042800090609             $Err = *on;
042900090609             $Eof = *on;
043000090609             exsr  sr_PrintErrSQL;
043100090609         endsl;
043200090609
043300090609       ENDSR;
043400100616
043500100616       //--------------------------------------------------------------
043600100616       //?Gestione controllo altre attività aperte                     ?
043700100616       //--------------------------------------------------------------
043800100616       BEGSR  sr_GesW01;
043900100616
044000100616       //?Controllo attività
044100100616         IF  $InzW01;
044200100616           exsr  sr_InzW01;
044300100616           $InzW01 = *off;
044400100616         ENDIF;
044500100722
044600100722       //?Se errore esco e non emetto la videata
044700100722         IF  $Fine;
044800100722           leavesr;
044900100722         ENDIF;
045000100616
045100100616       //?Emissione window di avviso
045200100616         exfmt  MK21W01;
045300100616
045400100616       //?Imposto i dati per visualizzare il subfile
045500100616         $InzS01 = *on;
045600100616         $Video = 'S1';
045700100616
045800100616       ENDSR;
045900100616
046000100616       //--------------------------------------------------------------
046100100616       //?Controllo attività per W01.
046200100616       //--------------------------------------------------------------
046300100616       BEGSR  sr_InzW01;
046400100616
046500100616         VisGestione = *off;
046600100616
046700100616       //?Per prima cosa richiamo pgm di controllo altre attività aperte
046800100616         clear TRMK85ds;
046900100616         IMK85cpo = I21cpo;
047000100616         IMK85ksc = I21ksc;
047100100616         IMK85nrv = I21ntc;
047200100616         trmk85r (trmk85ds);
047300100616         //?se torna errore <> "9" ho trovato delle attività aperte
047400100616         IF  OMK85err <> *blanks and OMK85err <> '9';
047500100616         //?gestisco l'errore emetto window e poi avanti con subfile
047600100616           VisGestione = *on;
047700100722           leavesr;
047800100616         ENDIF;
047900100722         //?se torna errore;
048000100722         //?torno al chiamante con errore
048100100722         IF  OMK85err <> *blanks;
048200100722           O21err = '1';
048300100722           O21msg = OMK85msg;
048400100722           $Fine = *on;
048500100722           leavesr;
048600100722         ENDIF;
048700100616         //?se NON torna errore esco dal pgm, potenziale senza attività aperte
048800100616         //?posso crearne una nuova
048900100616         IF  OMK85err = *blanks;
049000100616           $Fine = *on;
049100100616         ENDIF;
049200100616
049300100616       ENDSR;
049400090609
049500090609       //--------------------------------------------------------------
049600090609       //?Gestione subfile S01                                         ?
049700090609       //--------------------------------------------------------------
049800090609       BEGSR  sr_GesS01;
049900090609
050000090609         //?Inizializzazione subfile?
050100090609         if  $InzS01 = *on;
050200090609           exsr  sr_InzS01;
050300090609           $InzS01 = *off;
050400090609         endif;
050500090609
050600090609         //?Emissione Testata e Piede con tasti funzionali abilitati?
050700090609         write  MK21T01;
050800090609         write  MK21P01;
050900090609
051000090609         //?Posizionamento cursore?
051100090609         if  C1CsrRrn > *zero;
051200090609           C1RcdNbr = C1CsrRrn;
051300090609         else;
051400090609           C1RcdNbr = 1;
051500090609           write  MK21S00;             // (no rec.)
051600090609         endif;
051700090609
051800090609         //?Emissione videata?
051900090609         exfmt  MK21C01;
052000090609
052100090609         reset  ErrMessage;
052200090609         reset  ErrGenerico;
052300090609         clear  V1Dmsg;
052400090609
052500090609         SELECT;
052600100616
052700100616           //?F10=Nuova attività?
052800100616           WHEN  dsp_aid = c_F10;
052900100617             exsr sr_F10;
053000090609
053100090609           //?F12=Ritorno?
053200090609           WHEN  dsp_aid = c_F12;
053300100617           //?se non ho il campo impostato a '99' --> gestione attivita imposto F12
053400100617             IF  O21fxx <> '99';
053500100617               O21fxx = '12';
053600100617             ENDIF;
053700090609             $Fine = *on;
053800100809
053900100809           //?F22=Primo contatto?
054000100809           WHEN  dsp_aid = c_F22;
054100100809             exsr sr_F22;
054200090609
054300090609           //?Roll-Up?
054400090609           WHEN  dsp_aid = c_RollUp;
054500090609             exsr sr_RollUpS01;
054600090609
054700090609           //?SubFile vuoto?
054800090609           WHEN  S01nrr = *zero;
054900090609
055000090609           //?Invio?
055100090609           OTHER;
055200090609             // -?Gestione opzioni?
055300090609             exsr  sr_OpzS01;
055400090609             if  ErrGenerico;
055500090609               leavesr;
055600090609             endif;
055700100913           //?se ho il flag impostato a SI esco dal programma
055800100913           //?esco se ho fatto solo scelte 'G' altrimenti resto
055900100913           //?imposto O21fxx = '99' in modo da non richiamare il TRMK19R o TNTA88R dal chiamante
056000100913             IF  wEsci = 'S';
056100100913               O21fxx = '99';
056200100910               $Fine = *on;
056300100910             ENDIF;
056400090609
056500090609         ENDSL;
056600090609
056700090609       ENDSR;
056800090609
056900090609       //--------------------------------------------------------------
057000090609       //?Inizializzazione subfile S01                                 ?
057100090609       //--------------------------------------------------------------
057200090609       BEGSR  sr_InzS01;
057300090609
057400090609         //?Spegnimento degli indicatori di errore?
057500090609         %subst(IndDspF : 50) = *off;
057600090609
057700090609         //?Pulizia subfile?
057800090609         SflDsp_N    = *on;
057900090609         SflDspCtl_N = *on;
058000090609         write  MK21C01;
058100090609         SflDspCtl_N = *off;
058200090609         SflEnd      = *off;
058300090609
058400090609         clear  W_SflPag;
058500090609         clear  C1RcdNbr;
058600090609         clear  C1CsrRrn;
058700090609         clear  S01nrr;
058800090609         clear  V1Dmsg;
058900090609         ErrMessage  = *off;
059000090609         ErrGenerico = *off;
059100090609
059200090609         //?Impostazione campi di "testata"?
059300090609         select;
059400090609           when  I21ksc > *zero;
059500090609             C1Icpt = 'Cliente';
059600090609             C1Ccpt = %trim( %editc( I21ksc : 'X' ) );
059700090609             C1Dcpt = I21rsc;
059800090610             C1Icet = 'Potenziale  Tratt.';
059900090609           when  I21cpo > *zero;
060000090609             C1Icpt = 'Potenziale';
060100090609             C1Ccpt = %trim( %editc( I21cpo : 'X' ) );
060200090609             C1Dcpt = I21rsc;
060300090610             C1Icet = 'Cliente     Tratt.';
060400090609           when  I21ntc > *zero;
060500090609             C1Icpt = 'Trattativa';
060600090609             C1Ccpt = %trim( %editc( I21ntc : 'Z' ) );
060700090610             VisualTratt = *on;
060800090609             chain  ( I21ntc )  TIVIS000;
060900091019             if  %found(TIVIS05L);
061000090609               C1Dcpt = VISrag;
061100090610               wDate_dmy = %date( %editw( VISdat : '    -  -  ' ) : *iso );
061200090610               C1Cdat = %dec( wDate_dmy );
061300090609               C1Ctpv = VIStpv;
061400090609             else;
061500090609               C1Dcpt = *all'? ';
061600090609             endif;
061700090610             C1Icet = 'Cliente o Potenz. ';
061800090609         endsl;
061900090609
062000090609         //?Preparazione stringa SQL?
062100090609         exsr  sr_PrepSQL;
062200090609
062300090609         //?Caricamento della 1ª pagina di dati del subfile?
062400090609         exsr  sr_OpenCursor;
062500090609         exsr  sr_ReadCursor;
062600090609         exsr  sr_RollUpS01;
062700090609
062800090609         //?Impaginazione subfile?
062900090609         // -> forza l'impaginazione sul 1° rec. del subfile
063000090609         if S01nrr > *zero;
063100090609           C1RcdNbr  = 1;
063200090609           C1CsrRrn  = 1;
063300090609         else;
063400090609           clear C1RcdNbr;
063500090609         endif;
063600090609
063700090609       ENDSR;
063800100616
063900100616       //--------------------------------------------------------------
064000100616       //?F10 - Nuova attività.
064100100616       //--------------------------------------------------------------
064200100617       BEGSR  sr_F10;
064300100616
064400100809       //?Esco dal programma (arrivo da MK18 - MK01 - MK02 oppure da
064500100809       //?TA87 - TAI1 - TAI2)
064600100809       //?e il chiamante va diretto su MK19 nuova attività o TA88 nuova
064700100809       //?trattativa
064800100913         O21Fxx = '10';
064900100913         $Fine = *on;
065000100616
065100100616       ENDSR;
065200090609
065300100809       //--------------------------------------------------------------
065400100809       //?F22 - Primo contatto.
065500100809       //--------------------------------------------------------------
065600100809       BEGSR  sr_F22;
065700100809
065800100809         clear trmk17ds;
065900100809         IMK17patt = 'S';
066000100809         IMK17att  = 'T';
066100100809         IMK17pcau = 'S';
066200100809         IF  I21ksc = 0;
066300100809           IMK17cau  = '19';
066400100809         ENDIF;
066500100809         IF  I21ksc > 0;
066600100809           IMK17cau  = '20';
066700100809         ENDIF;
066800100809         IMK17cpo  = I21cpo;
066900100809         IMK17ksc  = I21ksc;
067000100809         IF w_IMK19com <> 0;
067100100809           IMK17co3  = w_IMK19com;
067200100809         ENDIF;
067300100809
067400100809         trmk17r (kpjba : TRMK17DS);
067500100810
067600100810       //?Imposto che devo ricaricare il subfile
067700100810         $InzS01 = *on;
067800100810       //?Chiudo cursore
067900100810         exsr sr_CloseCursor;
068000100810       //?Spengo indicatore di fine file
068100100810         $EoF = *off;
068200100809
068300100809       ENDSR;
068400100809
068500090609       //--------------------------------------------------------------
068600090609       //?Caricamento singola pagina nel subfile S01                   ?
068700090609       //--------------------------------------------------------------
068800090609       BEGSR  sr_RollUpS01;
068900090609
069000090609         S01nrr    = (W_SflPag * c_SflPag);
069100090609         W_SflPag += 1;
069200090609         SflNxtChg = *off;
069300090609
069400090609         //?Ciclo di caricamento dati da TIATC00F?
069500090609         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
069600090609              S01nrr    < c_MaxRec                and
069700090610             (Not $EoF   and   Not $Err);
069800090609
069900090617           clear  MK21S01;
070000090617
070100090609           // -?Impostazione attributi di visualizzazione?
070200090617           if  ATCdco > *zero;
070300090617             §DAdco = c_DspAtrNorm;
070400090609           else;
070500090617             §DAdco = c_DspAtrRed;
070600090609           endif;
070700090609
070800090609           // -?Caricamento del singolo record nel subfile S01?
070900090610           // - - Campi Hidden
071000090622           S1Hcpo  = ATCcpo;
071100090622           S1Hksc  = ATCksc;
071200090622           S1Hdco  = ATCdco;
071300090610           S1Htat  = ATCtat;
071400090610           S1Hatn  = ATCatn;
071500090610           S1Hatnp = ATCatnp;
071600090610           S1Hnrv  = ATCnrv;
071700090622           if  ATCdco > *zero;
071800090622             S1Hcco = ATCcco;
071900090622           else;
072000090622             S1Hcco = ATCcmm;
072100090622           endif;
072200090610           // - - Campi di Output
072300090609           if  ATCdco > *zero;
072400090610             wDate_dmy = %date( %editw( ATCdco : '    -  -  ' ) : *iso );
072500090609           else;
072600090610             wDate_dmy = %date( %editw( ATCdad : '    -  -  ' ) : *iso );
072700090609           endif;
072800090610           S1Cdco = %dec( wDate_dmy );
072900090609           if  ATCdco > *zero;
073000090609             S1Chco = ATChco / 100;
073100090609           else;
073200090609             S1Chco = ATChda / 100;
073300090609           endif;
073400090609           S1Ctat = ATCtat;
073500090610           if  ATCcac <> *blank;
073600090610             S1Ccac = ATCcac;
073700090610           else;
073800090610             S1Ccac = ATCcad;
073900090610           endif;
074000090610           select;
074100090610             when  I21ksc > *zero;
074200091022               S1Ccpo = %editc( ATCcpo : 'X' );
074300090610               if  ATCnra > *zero;
074400091022                 S1Cnrv = %editc( ATCnra : 'Z' );
074500090610               else;
074600091022                 S1Cnrv = %editc( ATCnrv : 'Z' );
074700090610               endif;
074800090610             when  I21cpo > *zero;
074900091022               if  ATCksc > *zero;
075000091022                 S1Ccpo = %editc( ATCksc : 'X' );
075100091022               endif;
075200090610               if  ATCnra > *zero;
075300091022                 S1Cnrv = %editc( ATCnra : 'Z' );
075400090610               else;
075500091022                 S1Cnrv = %editc( ATCnrv : 'Z' );
075600090610               endif;
075700090610             when  I21ntc > *zero;
075800090610               if  ATCksc > *zero;
075900090610                 S1Ccpo = %editc( ATCksc : 'X' );
076000090610               else;
076100090610                 S1Ccpo = %editc( ATCcpo : 'X' );
076200090610               endif;
076300090610           endsl;
076400090610
076500090610           // -?Decodifiche?
076600090610           // --> Commerciale
076700090610           clear  ds01;
076800090610           TBLcod = '01';
076900090622           TBLkey = %trim( %editc( S1Hcco : 'X' ) );
077000090610           chain (1 : '01' : TBLkey) TABEL;
077100090610           if  %found(TABEL00F)   and
077200090610               TBLflg = *blank;
077300090610             ds01 = TBLuni;
077400090610           else;
077500090610             §01age = %trimr(TBLkey) + ' ? ? ? ?';
077600090610           endif;
077700090610           S1Dcco = §01age;
077800090622           S1HccoD = §01age;
077900090610           // --> Causale contatto
078000090610           clear  dCCO;
078100090610           clear  TIBS02ds;
078200090610           T02mod = 'C';
078300090610           T02cod = 'CCO';
078400090622           T02ke1 = S1Ccac;
078500090610           T02sif = knsif;
078600090610           TNTBE_RicercaControllo ( kpjba : tibs02ds );
078700090610           if  T02err = *blank;
078800090611             dCCO    = T02uni;
078900090610           else;
079000090610             §CCOdes = *all'? ';
079100090610           endif;
079200090610           S1Dcac = §CCOdes;
079300100617         //?valorizzo il campo delle opzioni inibite per CAU attività
079400100617           S1Hopz = §CCOopz;
079500090609
079600090609           // -?Scrittura record nel subfile S01?
079700090609           S01nrr += 1;
079800090609           write  MK21S01;
079900090609
080000090609           // -?Lettura record successivo?
080100090609           exsr  sr_ReadCursor;
080200090609
080300090609         EndDo;
080400090609
080500090609         //?Visualizzazione del SFL (se ci sono dati)?
080600090609         SflDsp_N = (S01nrr <= *zero);
080700090609
080800090609         //?Attivazione del SFLEND?
080900090610         SflEnd   = (S01nrr >= c_MaxRec   or   $EoF   or   $Err);
081000090609
081100090609         //?Posizionamento cursore al 1° rcd della pagina?
081200090609         if  S01nrr > *zero;
081300090609           wPag     = S01nrr / c_SflPag;
081400090609           wRig     = S01nrr - (c_SflPag * wPag);
081500090609           C1RcdNbr = wPag * c_SflPag;
081600090609           if  wRig > *zero;
081700090609             C1RcdNbr += 1;
081800090609           else;
081900090609             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
082000090609           endif;
082100090609         else;
082200090609           clear  C1RcdNbr;
082300090609         endif;
082400090609
082500090609         C1CsrRrn = C1RcdNbr;
082600090609
082700090609       ENDSR;
082800090609
082900090609       //--------------------------------------------------------------
083000090609       //?Gestione opzioni del subfile S01                             ?
083100090609       //--------------------------------------------------------------
083200090609       BEGSR  sr_OpzS01;
083300090609
083400090609         clear  SavS01csr;
083500100913         clear  wEsci;
083600090609
083700090609         //?Ciclo di lettura subfile?
083800090609         readc  MK21S01;
083900090609
084000090609         DoW  Not %eof(TRMK21D);
084100090609
084200090609           SflNxtChg = *off;
084300090609           %subst(IndDspF : 50) = *off;
084400090609           C1RcdNbr = S01nrr;
084500090609
084600090609           select;
084700100617
084800100617             //?Controllo l'opzione inserita
084900100617             WHEN  S1Copz <> *blank and S1Hopz <> *blanks and
085000100617                   %scan(S1Copz:S1Hopz) > 0 ;
085100100617                   errMessage  = *on;
085200100617                   errGenerico = *on;
085300100617                   PosCurOpz   = *on;
085400100617                   V1Dmsg = $Msg(04);
085500100616
085600100617             //?G-Gestione
085700100616             when  S1Copz = 'G' and VisGestione;
085800100616               exsr sr_Gestione;
085900100913               IF  wEsci = *blanks;
086000100913                 wEsci = 'S';
086100100913               ENDIF;
086200100809
086300100809             //?2-Modifica
086400100809             when  S1Copz = '2' and VisGestione;
086500100809               exsr sr_Modifica;
086600100913               wEsci = 'N';
086700090609
086800090609             // -?5=Visualizzazione?
086900100928             when  S1Copz = '5' and not VisDaMk19;
087000090611               exsr sr_CallTRMK19;
087100100913               wEsci = 'N';
087200090609
087300090609             // -?8=Note?
087400090609             when  S1Copz = '8';
087500090609               exsr sr_CallTRMK20;
087600100913               wEsci = 'N';
087700090609
087800090609             // -?Nessuna opzione?
087900090609             when  S1Copz = *blank;
088000100913               wEsci = 'N';
088100090609
088200090609             // -?Opzione errata?
088300090609             other;
088400090609               ErrMessage  = *on;
088500090609               ErrGenerico = *on;
088600090609               PosCurOPZ   = *on;
088700090609               V1Dmsg = $Msg(01);
088800090609
088900090609           endsl;
089000090609
089100090609           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
089200090609           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
089300090609           if  S1Copz  <> *blank   and
089400090609              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
089500090609             SavS01csr   = C1RcdNbr;
089600090609           endif;
089700090609
089800090609           // -?Aggiornamento sfl?
089900090609           select;
090000090609             when  ErrMessage;
090100090609               SflNxtChg = *on;
090200090609               C1CsrRrn  = C1RcdNbr;
090300090609             when  ErrGenerico;
090400090609               C1CsrRrn  = C1RcdNbr;
090500090609               clear  S1Copz;
090600090609             other;
090700090609               clear  S1Copz;
090800090609           endsl;
090900090609           UPDATE  MK21S01;
091000090609           if  ErrGenerico   or   ErrMessage;
091100090609             leave;
091200090609           endif;
091300090609
091400090609           readc  MK21S01;
091500090609
091600090609         ENDDO;
091700090609
091800090609         // -?Riposiziono il cursore sul record contenente la prima opz.?
091900090609         //  ?(se non sono stati rilevati errori)?
092000090609         if  NOT ErrMessage   and   NOT ErrGenerico   and
092100090609             SavS01csr > *zero;
092200090609           C1CsrRrn = SavS01csr;
092300090609         endif;
092400090609
092500090609       ENDSR;
092600100616
092700100616       //--------------------------------------------------------------
092800100616       //?Richiamo gestione attività.
092900100616       //--------------------------------------------------------------
093000100616       BEGSR  sr_Gestione;
093100100616
093200100617       //?Come prima cosa controllo se che sia un'attività da eseguire
093300100616         IF  S1Hdco <> 0;
093400100616           ErrMessage  = *on;
093500100616           ErrGenerico = *on;
093600100616           PosCurOPZ   = *on;
093700100616           V1Dmsg = $Msg(02);
093800100616           leavesr;
093900100616         ENDIF;
094000100616
094100100617       //?Poi controllo se posso gestire l'attività
094200100616         exsr CtrAbiAtt;
094300100616         IF  ErrMessage;
094400100616           leavesr;
094500100616         ENDIF;
094600100617
094700100617       //?Imposto la DS buona dalla DS fasulla di richiamo
094800100617         TRMK19DS = TRMK19DS1;
094900100616
095000100616       //?A questo punto vado in gestione dell'attività
095100100617       //?se attività legata a trattativa richiamo gestione trattativa
095200100616         IF  S1Hnrv > 0;
095300100616           clear tnta88ds ;
095400100616           ITA88cmt = 'N';
095500100616           ITA88fle = 'G' ;
095600100616           ITA88nrv = S1Hnrv ;
095700100616           IF IMK19com <> 0;
095800100616             ITA88com  = IMK19com;
095900100616             ITA88comd = IMK19comd;
096000100616           ENDIF;
096100100616           Tnta88r ( kpjba : tnta88ds );
096200100910         //?ritorno il flag "99" in modo da NON richiamare di nuovo il TNTA88R dal chiamante
096300100617         //?non ho possibilità di controllare se F3 (F12 è attivo solo se nuova trattativa)
096400100617           O21fxx = '99';
096500100616         ENDIF;
096600100617       //?se attività NON legata a trattativa richiamo gestione attività
096700100616         IF  S1Hnrv = 0;
096800101014           IMK19cmt = 'S';
096900100616           IMK19fle = 'G';
097000100616           IMK19tat = S1Htat;
097100100616           IMK19atn = S1Hatn;
097200100616           IMK19atnp= S1Hatnp;
097300100930           // Trmk19c (kpjba : trmk19ds);
097400100930           Trmk19r (kpjba : trmk19ds);
097500100617         //?ritorno il flag "99" in modo da NON richiamare di nuovo il TRMK19R dal chiamante
097600100617         //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
097700100617           IF  OMK19err = *blanks;
097800100617             O21fxx = '99';
097900100617           ENDIF;
098000100616         ENDIF;
098100100617
098200100617       //?Imposto che devo ricaricare il subfile
098300100617         $InzS01 = *on;
098400100617       //?Chiudo cursore
098500100617         exsr sr_CloseCursor;
098600100617       //?Spengo indicatore di fine file
098700100617         $EoF = *off;
098800100616
098900100616       ENDSR;
099000100809
099100100809       //--------------------------------------------------------------
099200100809       //?Richiamo Modifica attività.
099300100809       //--------------------------------------------------------------
099400100809       BEGSR  sr_Modifica;
099500100809
099600100809       //?Come prima cosa controllo se che sia un'attività da eseguire
099700100809         IF  S1Hdco <> 0;
099800100809           ErrMessage  = *on;
099900100809           ErrGenerico = *on;
100000100809           PosCurOPZ   = *on;
100100100809           V1Dmsg = $Msg(02);
100200100809           leavesr;
100300100809         ENDIF;
100400100809
100500100809       //?Poi controllo se posso gestire l'attività
100600100809         exsr CtrAbiAtt;
100700100809         IF  ErrMessage;
100800100809           leavesr;
100900100809         ENDIF;
101000100809
101100100809       //?Imposto la DS buona dalla DS fasulla di richiamo
101200100809         TRMK19DS = TRMK19DS1;
101300100809
101400100809       //?A questo punto vado in gestione dell'attività
101500100913       //?sempre
101600101014         IMK19cmt = 'S';
101700100913         IMK19fle = '2';
101800100913         IMK19tat = S1Htat;
101900100913         IMK19atn = S1Hatn;
102000100913         IMK19atnp= S1Hatnp;
102100100930        // Trmk19c (kpjba : trmk19ds);
102200100930         Trmk19r (kpjba : trmk19ds);
102300100913       //?ritorno il flag "99" in modo da NON richiamare di nuovo il TRMK19R dal chiamante
102400100913       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
102500100913         IF  OMK19err = *blanks;
102600100913           O21fxx = '99';
102700100809         ENDIF;
102800100809
102900100809       //?Imposto che devo ricaricare il subfile
103000100809         $InzS01 = *on;
103100100809       //?Chiudo cursore
103200100809         exsr sr_CloseCursor;
103300100809       //?Spengo indicatore di fine file
103400100809         $EoF = *off;
103500100809
103600100809       ENDSR;
103700090609
103800090609       //--------------------------------------------------------------
103900090609       //?Richiamo pgm di visualizzazione dati contatto                ?
104000090609       //--------------------------------------------------------------
104100090611       BEGSR  sr_CallTRMK19;
104200090609
104300100616         clear  TRMK19ds;
104400100616         IMK19cmt  = 'N';
104500090611         IMK19fle  = '5';
104600091105         IMK19fpr  = 'P';
104700090618         IMK19tco  = S1Htat;
104800090622         IMK19cpo  = S1Hcpo;
104900090622         if  S1Hdco > *zero;
105000090611           IMK19fatt = 'S';
105100090611         endif;
105200090611         IMK19com  = S1Hcco;
105300090611         IMK19comd = S1HccoD;
105400090611         IMK19tat  = S1Htat;
105500090611         IMK19atn  = S1Hatn;
105600090611         IMK19atnp = S1Hatnp;
105700090611
105800100928         trmk19r ( KPJBA : TRMK19ds );
105900090611
106000090611         if  OMK19err = 'E';
106100090611           V1Dmsg  = OMK19msg;
106200090611           ErrMessage  = *on;
106300090611           ErrGenerico = *on;
106400090611         endif;
106500090609
106600090609       ENDSR;
106700090609
106800090609       //--------------------------------------------------------------
106900090609       //?Richiamo pgm di visualizzazione note contatto                ?
107000090609       //--------------------------------------------------------------
107100090609       BEGSR  sr_CallTRMK20;
107200090609
107300090611         reset  TRMK20ds;
107400090622         IMK20cpo  = S1Hcpo;
107500090622         IMK20ksc  = S1Hksc;
107600090609         IMK20tat  = S1Htat;
107700090609         IMK20atn  = S1Hatn;
107800090609         IMK20atnp = S1Hatnp;
107900090609         IMK20nrv  = S1Hnrv;
108000090609
108100090609         trmk20r ( KPJBA : TRMK20ds );
108200090609
108300090609         if  OMK20fxx  = '03';
108400090609           O21fxx = '03';
108500090609           $Fine  = *on;
108600090609           ErrGenerico = *on;
108700090609         endif;
108800090611
108900090611         if  OMK19err = 'E';
109000090611           V1Dmsg  = OMK20msg;
109100090611           ErrMessage  = *on;
109200090611           ErrGenerico = *on;
109300090611         endif;
109400090609
109500090609       ENDSR;
109600100616
109700100616       //--------------------------------------------------------------
109800100616       //?Controllo se utente abilitato all'attività.
109900100616       //--------------------------------------------------------------
110000100616       BEGSR CtrAbiAtt;
110100100616
110200100616       //?abilitato come commerciale
110300100616         clear TNTAA1DS;
110400100616         ITAA1tipo = 'N';
110500100616         ITAA1caut = '§utegtc';
110600100616         ITAA1cmm  = S1Hcco;
110700100616         kpjbu     = TNTAA1DS;
110800100616         tntaa1c (kpjba);
110900100616         TNTAA1DS = kpjbu;
111000100616         IF  OTAA1fabi <> 'N';
111100100616           leavesr;
111200100616         ENDIF;
111300100616
111400100616       //?abilitato come cliente
111500100616         IF  S1Hksc > 0;
111600100616           clear TNTAA1DS;
111700100616           ITAA1tipo = 'N';
111800100616           ITAA1caut = '§utegtc';
111900100616           ITAA1ksc  = %dec(S1Hksc:7:0);
112000100616           kpjbu     = tntaa1ds;
112100100616           tntaa1c (kpjba);
112200100616           TNTAA1DS = kpjbu;
112300100616           IF  OTAA1fabi <> 'N';
112400100616             leavesr;
112500100616           ENDIF;
112600100616         ENDIF;
112700100616
112800100616       //?se arrivo a questo punto vuol dire che l'utente non è abilitato
112900100616       //?al commerciale o al cliente
113000100616         ErrMessage  = *on;
113100100616         ErrGenerico = *on;
113200100616         PosCurOPZ   = *on;
113300100616         V1Dmsg = $Msg(03);
113400100616
113500100616       ENDSR;
113600090609
113700090609       //--------------------------------------------------------------
113800090609       //?Stampa segnalazione dell'errore rilevato via SQL             ?
113900090609       //--------------------------------------------------------------
114000090609       BEGSR  sr_PrintErrSQL;
114100090609
114200090609         //?Stampa del Dump?
114300090609         Dump(A);
114400090609
114500090609         //?Stampa del Job-Log?
114600090609         Qcmd = 'DSPJOBLOG job(*) output(*print)';
114700090609         exsr  sr_ExecCmd;
114800090609
114900090609         //?Chiusura del programma?
115000090609         exsr  sr_RoutEnd;
115100090609
115200090609       ENDSR;
115300090609
115400090609       //--------------------------------------------------------------
115500090609       //?Esecuzione del comando (già impostato).                      ?
115600090609       //--------------------------------------------------------------
115700090609       BEGSR  sr_ExecCmd;
115800090609
115900090609         clear Qcap0100;
116000090609         Qcabcsdh = *off;
116100090609         Qcapa    = *off;
116200090609         Qcacmdss = *off;
116300090609         Qcaerved = *allX'00';
116400090609
116500090609         clear Qusec;
116600090609         Qusbprv  = %size(Qusec);
116700090609
116800090609         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
116900090609                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
117000090609                           0 : 0 : Qusec);
117100090609
117200090609         if  Qusei <> *blank;
117300090609           exsr  sr_MsgErr;
117400090609         endif;
117500090609
117600090609       ENDSR;
117700090609
117800090609       //--------------------------------------------------------------
117900090609       //?Invio messaggio per segnalazione errori.                     ?
118000090609       //--------------------------------------------------------------
118100090609       BEGSR  sr_MsgErr;
118200090609
118300090609
118400090609       ENDSR;
118500090609
118600090609       //--------------------------------------------------------------
118700090609       //?Operazioni finali.                                           ?
118800090609       //--------------------------------------------------------------
118900090609       BEGSR  sr_RoutEnd;
119000090609
119100090609         //?Chiusura cursore SQL?
119200090609         exsr  sr_CloseCursor;
119300090609
119400090609         //?Chiusura pgm?
119500090610         kpjbu = TRMK21ds;
119600090609         return;
119700090609
119800090609       ENDSR;
119900090609
120000090609      /end-free
120100090609
120200090609       //--------------------------------------------------------------
120300090609       //?Schiere a tempo di compilazione.                             ?
120400090609       //--------------------------------------------------------------
120500090609
120600090609** - $Msg -------------------------------------------------------------------*
120700090609Opzione errata                                                                 1
120800100616Opzione errata Attività già eseguita                                           2
120900100616Opzione errata Utente non abilitato alla gestione dell'attività                3
121000100617Opzione non valida per questa attività                                         4
