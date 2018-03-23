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
001200120706
001300120706       // - Storico Attività
001400120706     fTICRM01L  if   e           k disk
001500090609
001600090609       // - Trattative a clienti
001700091019     fTIVIS05L  if   e           k disk
001800090610
001900090610       // - Tabelle
002000090610     fTABEL00F  if   e           k disk
002100110211
002200110211       // - Potenziale
002300110211     fTNCPO01L  if   e           k disk
002400090609
002500090609       // - Video Gestione
002600090609     fTRMK21D   cf   e             workstn
002700090609     f                                     sfile(MK21S01 : S01nrr)
002800090609     f                                     indds(IndDspF)
002900090609     f                                     infds(InfDspF)
003000090609
003100090609       //--------------------------------------------------------------
003200090609       //?Definizione costanti.                                        ?
003300090609       //--------------------------------------------------------------
003400090609
003500090609      // - Numero di record in ciascuna videata di subfile
003600090609     d c_SflPag        c                   const(15)
003700090609
003800090609      // - Numero massimo di record gestibili
003900110301     d c_MaxRec        c                   const(9999)
004000090609
004100090609       // - Tasti funzionali a video
004200090609     d c_F01           c                   const(x'31')
004300090609     d c_F02           c                   const(x'32')
004400090609     d c_F03           c                   const(x'33')
004500090609     d c_F05           c                   const(x'35')
004600090609     d c_F06           c                   const(x'36')
004700090609     d c_F07           c                   const(x'37')
004800090609     d c_F08           c                   const(x'38')
004900090609     d c_F09           c                   const(x'39')
005000090609     d c_F10           c                   const(x'3A')
005100090609     d c_F12           c                   const(x'3C')
005200090609     d c_F13           c                   const(x'B1')
005300090609     d c_F14           c                   const(x'B2')
005400090609     d c_F15           c                   const(x'B3')
005500090609     d c_F16           c                   const(x'B4')
005600090609     d c_F17           c                   const(x'B5')
005700090609     d c_F18           c                   const(x'B6')
005800090609     d c_F19           c                   const(x'B7')
005900090609     d c_F20           c                   const(x'B8')
006000090609     d c_F21           c                   const(x'B9')
006100090609     d c_F22           c                   const(x'BA')
006200090609     d c_F23           c                   const(x'BB')
006300090609     d c_F24           c                   const(x'BC')
006400090609     d c_Enter         c                   const(x'F1')
006500090609     d c_RollDown      c                   const(x'F4')
006600090609     d c_RollUp        c                   const(x'F5')
006700090609
006800090609       // - Attributi di Visualizzazione
006900090609     d c_DspAtrNorm    c                   const(x'A0')
007000090610     d c_DspAtrRed     c                   const(x'A8')
007100090609
007200090609       //--------------------------------------------------------------
007300090609       //?Definizione schiere.                                         ?
007400090609       //--------------------------------------------------------------
007500090609
007600090609       // - Messaggi di errore
007700100616     d $Msg            s             78    dim(10) ctdata perrcd(1)
007800090609
007900090609       //--------------------------------------------------------------
008000090609       //?Definizione aree dati.                                       ?
008100090609       //--------------------------------------------------------------
008200090609
008300090609       // - Dati utente
008400090609     d §AzUte        e ds                  extname(AZUTE00F)
008500090609     d                                     dtaara
008600090609     d §DatiUte      e ds                  extname(dDatiUte)
008700090609     d                                     dtaara
008800090609
008900090609       //--------------------------------------------------------------
009000090609       //?Definizione strutture dati.                                  ?
009100090609       //--------------------------------------------------------------
009200090609
009300090609       // - Status ds
009400090609     d Status         sds
009500090609     d  SDSpgm           *proc
009600090609     d  SDSusr               254    263
009700090609
009800090609       // - InfDS
009900090609     d InfDspF         ds
010000090609     d   dsp_aid             369    369a
010100090609     d   sfl_rrn             376    377i 0
010200090609     d   min_nrr             378    379i 0
010300090609     d   num_rcds            380    381i 0
010400090609
010500090609       // - Indicatori su DspF
010600090609     d IndDspF         ds                  inz
010700090609         // - Emissione messaggio di errore
010800090609     d   ErrMessage                    n   overlay(IndDspF : 28)
010900090610         // - Condizionamento di visualizzazione
011000090610     d   VisualTratt                   n   overlay(IndDspF : 41)
011100100616     d   VisGestione                   n   overlay(IndDspF : 42)
011200100809     d   VisNewAtt                     n   overlay(IndDspF : 43)
011300100809     d   VisNewTra                     n   overlay(IndDspF : 44)
011400100928     d   VisDaMk19                     n   overlay(IndDspF : 45)
011500110211     d   Codificato                    n   overlay(IndDspF : 46)
011600120704     d   F8Attivo                      n   overlay(IndDspF : 47)
011700120706     d   F8AttivoPink                  n   overlay(IndDspF : 48)
011800090609         // - Indicatori di gestione del subfile
011900090609     d   SflDsp_N                      n   overlay(IndDspF : 30)
012000090609     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012100090609     d   SflNxtChg                     n   overlay(IndDspF : 32)
012200090609     d   SflEnd                        n   overlay(IndDspF : 33)
012300090609         // - Posizionamento cursore & segnalazione errore
012400090609     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012500090609         //   - Riemissione videata
012600090609     d   ErrGenerico                   n   overlay(IndDspF : 99)
012700090609
012800090609       // - Parametri ricevuti
012900090609     d KPJBA         e ds
013000090610     d TRMK21ds      e ds                  inz
013100100416     d TRMK01ds      e ds                  inz
013200090610
013300090610       // - Dati estratti via SQL
013400090610     d TIATCds       e ds                  extname(TIATC00F)  inz
013500090609
013600090609       // - Parametri per Reperimento dati utente
013700090609     d TIBS34ds      e ds                  inz
013800100616
013900100616      // - controllo abilitazioni
014000100616     d TNTAA1DS      e ds                  inz
014100100616
014200100616      // - Gestione trattative commerciali
014300100616     d TNTA88ds      e ds                  inz
014400120704
014500120704       // - Storico attività
014600120704     d TRMK10DS      e ds
014700100809
014800100809       // - Primo contatto
014900100809     d TRMK17DS      e ds
015000090611
015100090611       // - Gestione/Visualizzazione contatti commerciali
015200100616     d TRMK19ds      e ds
015300100617
015400100617       // - Gestione/Visualizzazione contatti commerciali
015500100617     d TRMK19ds1     e ds                  extname(TRMK19DS) prefix(w_)
015600090609
015700090609       // - Gestione note clienti/potenziali/trattative
015800090609     d TRMK20ds      e ds                  inz
015900090609     d   IMK20tla    e                     inz('L')
016000090609     d   IMK20flm    e                     inz('I')
016100100616
016200100616      // -  Controllo se ci sono altre attività aperte
016300100616     d TRMK85ds      e ds                  inz
016400090609
016500090609       // - Ricerca/Controllo tabelle
016600090609     d TIBS02ds      e ds                  inz
016700090609     d   T02Mod      e                     inz('C')
016800090609
016900090609       // - Tabella "CCO" = Causale contatto
017000090609     d dCCO          e ds                  inz
017100090609
017200090609       // - Tabella "01" = Codici Commerciali
017300090609     d ds01          e ds                  inz
017400090609
017500090609       //--------------------------------------------------------------
017600090609       //?Definizione variabili globali.                               ?
017700090609       //--------------------------------------------------------------
017800090609
017900090609       // - Flags booleani
018000090609     d $Fine           s               n   inz
018100090609     d $EoF            s               n   inz
018200090609     d $Err            s               n   inz
018300090609     d $InzS01         s               n   inz(*on)
018400100616     d $InzW01         s               n   inz
018500090609
018600090609       // - Variabili per la gestione del video
018700090609     d $Video          s              2    inz('S1')
018800090609     d W_SflPag        s              3  0 inz
018900090609     d wPag            s              4  0 inz
019000090609     d wRig            s              3  0 inz
019100090609     d S01nrr          s                   like(C1RcdNbr) inz
019200090609     d SavS01csr       s                   like(C1RcdNbr) inz
019300090609
019400090609       // - Stringa SQL da eseguire
019500090609     d wSQL            s           2048    Varying        inz
019600090609
019700090609       // - Parametri per API QCAPCMD (Process Commands)
019800090609      /copy qsysinc/qrpglesrc,QCAPCMD
019900090609     d Qcmd            s            128    Varying        inz
020000090609
020100090609       // - Parametri gestione errori API.
020200090609      /copy qsysinc/qrpglesrc,QUSEC
020300090609
020400090609       // - Campi di comodo
020500090610     d wDate_dmy       s               d   datfmt(*dmy)   inz
020600100913     d wEsci           s              1    inz
020700090609
020800090609       //--------------------------------------------------------------
020900090609       //?Definizione prototipi procedure.                             ?
021000090609       //--------------------------------------------------------------
021100120704
021200120704      // - Storico attività
021300120704     d TRMK10R         pr                  extpgm('TRMK10R')
021400120704     d  kpjba                              likeds(KPJBA)
021500120704     d  trmk10ds                           likeds(trmk10ds)
021600100616
021700100616     d TRMK85R         pr                  extpgm('TRMK85R')
021800100616     d  trmk85ds                           likeds(trmk85ds)
021900100809
022000100809      // - Inserimento attività primo contatto
022100100809     d trmk17r         pr                  extpgm('TRMK17R')
022200100809     d  kpjba                              likeds(KPJBA)
022300100809     d  trmk17ds                           likeds(TRMK17ds)
022400100617
022500100617      // - Inserimento attività
022600100617     d trmk19c         pr                  extpgm('TRMK19C')
022700100617     d  kpjba                              likeds(KPJBA)
022800100617     d  trmk19ds                           likeds(TRMK19ds)
022900090609
023000090609       // - Reperimento dati utente
023100090609      /copy gaitrasrc/srcProtoPR,TIBS34R
023200090609
023300090609       // - Ricerca/Controllo tabelle
023400090609      /copy gaitrasrc/srcProtoPR,TIBS02R
023500100616
023600100616       // - Controllo se utente abilitato
023700100616      /copy gaitrasrc/srcprotopr,tntaa1c
023800100616
023900100616       // - Gestione trattative
024000100616      /copy gaitrasrc/srcprotopr,tnta88r
024100090611
024200090611       // - Gestione/Visualizzazione contatti commerciali
024300100928      /copy gaitrasrc/srcProtoPR,TRMK19R
024400090609
024500090609       // - Gestione note clienti/potenziali/trattative
024600090609      /copy gaitrasrc/srcProtoPR,TRMK20R
024700090609
024800090609       // - Api QCAPCMD (Process Commands)
024900090609      /copy gaitrasrc/srcProtoPR,QCAPCMD
025000090609
025100090609       //--------------------------------------------------------------
025200090609       //?Definizione key-list.                                        ?
025300090609       //--------------------------------------------------------------
025400090609
025500090609
025600090609       //--------------------------------------------------------------
025700090609       //?M A I N - L I N E                                            ?
025800090609       //--------------------------------------------------------------
025900090609
026000090609     c     *Entry        plist
026100090609     c                   parm                    KPJBA
026200100617     c                   parm                    TRMK19ds1
026300090609
026400090609      /free
026500090609
026600090609       //?Operazioni iniziali?
026700090609       exsr sr_RoutInz;
026800090609
026900090609       DoW  $Fine = *off;
027000090609         select;
027100100616           // - Controllo se altre attività aperte
027200100616           when $Video = 'W1';
027300100616             exsr sr_GesW01;
027400090609           // - Gestione subfile
027500090609           when $Video = 'S1';
027600090609             exsr sr_GesS01;
027700090609           other;
027800090609             $Fine = *on;
027900090609         endsl;
028000090609       enddo;
028100090609
028200090609       //?Operazioni finali?
028300090609       exsr sr_RoutEnd;
028400090609
028500090609       //--------------------------------------------------------------
028600090609       //?Operazioni iniziali.                                         ?
028700090609       //--------------------------------------------------------------
028800090609       BEGSR  sr_RoutInz;
028900090609
029000090609         *inLR = *on;
029100090610
029200090610         //?Impostazione opzioni per SQL?
029300090610         exec sql   set  option  DynUsrPrf = *Owner,
029400090610                                 CloSqlCsr = *EndMod;
029500100616
029600100616       //?Controllo se passati + parametri
029700100616       //?forzo gestione
029800100809        IF  %parms > 1 and w_IMK19tco = 'T';
029900100810          //I21mod = 'G';
030000100809          VisNewAtt = *on;
030100100809          VisNewTra = *off;
030200100616        ENDIF;
030300100809        IF  %parms > 1 and w_IMK19tco = 'O';
030400100810          //I21mod = 'G';
030500100809          VisNewAtt = *off;
030600100809          VisNewTra = *on;
030700100809        ENDIF;
030800090609
030900090609         //?Reperimento dati job?
031000090609         exsr  sr_DatiJob;
031100090609
031200090609         //?Impostazione nome programma a video?
031300090609         V1Tpgm = SDSpgm;
031400090610
031500090610         //?Impostazione parametri ricevuti?
031600090610         if  kpjbu <> *blank;
031700090610           TRMK21ds = kpjbu;
031800090610         else;
031900090610           $Fine = *on;
032000090610         endif;
032100100810
032200100810       //?Se richiamato in modalità 'V' devo attivare la gestione e la modifica +
032300100810       //?tasti funzionali
032400100810         IF  I21mod = 'V';
032500100810           VisGestione = *on;
032600100810         ENDIF;
032700100616
032800100616       //?Se richiamato in modalità 'G' imposto $Video = 'W1'
032900100616         IF  I21mod = 'G';
033000100616           $Video = 'W1';
033100100616           $InzS01 = *off;
033200100616           $InzW01 = *on;
033300100616         ENDIF;
033400100928
033500100928       //?Se richiamato in modalità '9' arrivo da TRMK19R visualizzo solo come se
033600100928       //?richiamato con 'I', ma non do la possibilità di interrogare le attività con
033700100928       //?opzione 5
033800100928         IF  I21mod = '9';
033900100928           VisDaMk19 = *on;
034000100928         ENDIF;
034100110211
034200110211       //?Aggancio il potenziale per reperire la categoria
034300110211         Codificato = *off;
034400110211         IF  I21cpo > 0 and I21ksc = 0;
034500110211           chain  I21cpo  TNCPO01L;
034600110211           IF  %found(TNCPO01L);
034700110211             Codificato = (CPOfls = 'C');
034800110211           ENDIF;
034900110211         ENDIF;
035000120704
035100120704         F8Attivo = (I21cpo > 0);
035200120706
035300120706         IF  F8Attivo;
035400120706           chain (I21cpo) TICRM01L;
035500120706           IF  %found(TICRM01L);
035600120706             F8AttivoPink = *on;
035700120706           ELSE;
035800120706             F8AttivoPink = *off;
035900120706           ENDIF;
036000120706         ENDIF;
036100090609
036200090609       ENDSR;
036300090609
036400090609       //--------------------------------------------------------------
036500090609       //?Reperimento Dati del job (Utente/Operativi).                 ?
036600090609       //--------------------------------------------------------------
036700090609       BEGSR  sr_DatiJob;
036800090609
036900090609         in(e) §AzUte;
037000090609         if NOT %error;
037100090609           in(e) §DatiUte;
037200090609         endif;
037300090609         if %error or RSut = *blank;
037400090609           tibs34r ( tibs34ds );
037500090609           in §AzUte;
037600090609           in §DatiUte;
037700090609         endif;
037800090609
037900090609       ENDSR;
038000090609
038100090609       //--------------------------------------------------------------
038200090609       //?Preparazione stringa SQL                                     ?
038300090609       //--------------------------------------------------------------
038400090609       BEGSR  sr_PrepSQL;
038500090609
038600090618         wSQL = 'select TIATC00F.*, case +
038700090622                           when (ATCdco*1000000)+ATChco > 0 +
038800090618                           then (ATCdco*1000000)+ATChco +
038900090618                           else (ATCdad*1000000)+ATChda +
039000090618                           end  as WWWord +
039100090609                   from TIATC00F +
039200090610                  where';
039300090610
039400090610         select;
039500090610           when  I21ksc > *zero;
039600090610             wSQL += ' ATCksc = ' + %editc(I21ksc:'X');
039700090610           when  I21cpo > *zero;
039800090610             wSQL += ' ATCcpo = ' + %editc(I21cpo:'X');
039900090610           when  I21ntc > *zero;
040000090610             wSQL += ' ATCnrv = ' + %editc(I21ntc:'X');
040100090610         endsl;
040200090610
040300090622         wSQL += ' order by WWWord desc +
040400090617
040500090617                   for fetch only';
040600090609
040700090609       ENDSR;
040800090609
040900090609       //--------------------------------------------------------------
041000090609       //?Apertura cursore.                                            ?
041100090609       //--------------------------------------------------------------
041200090609       BEGSR  sr_OpenCursor;
041300090609
041400090609         //?Dichiarazione cursore?
041500090609         exec sql   prepare S1   from :wSQL;
041600090609         exec sql   declare C1   cursor for S1;
041700090609
041800090609         //?Apertura del cursore?
041900090609         exec sql   open C1;
042000090609
042100090609         if  SQLcode < *zero;
042200090609           $Err = *on;
042300090609           $EoF = *on;
042400090609           exsr  sr_PrintErrSQL;
042500090609         endif;
042600090609
042700090609       ENDSR;
042800090609
042900090609       //--------------------------------------------------------------
043000090609       //?Chiusura cursore.                                            ?
043100090609       //--------------------------------------------------------------
043200090609       BEGSR  sr_CloseCursor;
043300090609
043400090609         //?Chiusura del cursore?
043500090609         exec sql   close C1;
043600090609
043700090609       ENDSR;
043800090609
043900090609       //--------------------------------------------------------------
044000090609       //?Lettura cursore.                                             ?
044100090609       //--------------------------------------------------------------
044200090609       BEGSR  sr_ReadCursor;
044300090609
044400090609         clear  TIATCds;
044500090609
044600090609         exec sql   fetch next   from C1   into :TIATCds;
044700090609
044800090609         select;
044900090609           when  SQLcode = 100;
045000090609             $EoF = *on;
045100090609           when  SQLcode < *zero;
045200090609             $Err = *on;
045300090609             $Eof = *on;
045400090609             exsr  sr_PrintErrSQL;
045500090609         endsl;
045600090609
045700090609       ENDSR;
045800100616
045900100616       //--------------------------------------------------------------
046000100616       //?Gestione controllo altre attività aperte                     ?
046100100616       //--------------------------------------------------------------
046200100616       BEGSR  sr_GesW01;
046300100616
046400100616       //?Controllo attività
046500100616         IF  $InzW01;
046600100616           exsr  sr_InzW01;
046700100616           $InzW01 = *off;
046800100616         ENDIF;
046900100722
047000100722       //?Se errore esco e non emetto la videata
047100100722         IF  $Fine;
047200100722           leavesr;
047300100722         ENDIF;
047400100616
047500100616       //?Emissione window di avviso
047600100616         exfmt  MK21W01;
047700100616
047800100616       //?Imposto i dati per visualizzare il subfile
047900100616         $InzS01 = *on;
048000100616         $Video = 'S1';
048100100616
048200100616       ENDSR;
048300100616
048400100616       //--------------------------------------------------------------
048500100616       //?Controllo attività per W01.
048600100616       //--------------------------------------------------------------
048700100616       BEGSR  sr_InzW01;
048800100616
048900100616         VisGestione = *off;
049000100616
049100100616       //?Per prima cosa richiamo pgm di controllo altre attività aperte
049200100616         clear TRMK85ds;
049300100616         IMK85cpo = I21cpo;
049400100616         IMK85ksc = I21ksc;
049500100616         IMK85nrv = I21ntc;
049600100616         trmk85r (trmk85ds);
049700100616         //?se torna errore <> "9" ho trovato delle attività aperte
049800100616         IF  OMK85err <> *blanks and OMK85err <> '9';
049900100616         //?gestisco l'errore emetto window e poi avanti con subfile
050000100616           VisGestione = *on;
050100100722           leavesr;
050200100616         ENDIF;
050300100722         //?se torna errore;
050400100722         //?torno al chiamante con errore
050500100722         IF  OMK85err <> *blanks;
050600100722           O21err = '1';
050700100722           O21msg = OMK85msg;
050800100722           $Fine = *on;
050900100722           leavesr;
051000100722         ENDIF;
051100100616         //?se NON torna errore esco dal pgm, potenziale senza attività aperte
051200100616         //?posso crearne una nuova
051300100616         IF  OMK85err = *blanks;
051400100616           $Fine = *on;
051500100616         ENDIF;
051600100616
051700100616       ENDSR;
051800090609
051900090609       //--------------------------------------------------------------
052000090609       //?Gestione subfile S01                                         ?
052100090609       //--------------------------------------------------------------
052200090609       BEGSR  sr_GesS01;
052300090609
052400090609         //?Inizializzazione subfile?
052500090609         if  $InzS01 = *on;
052600090609           exsr  sr_InzS01;
052700090609           $InzS01 = *off;
052800090609         endif;
052900090609
053000090609         //?Emissione Testata e Piede con tasti funzionali abilitati?
053100090609         write  MK21T01;
053200090609         write  MK21P01;
053300090609
053400090609         //?Posizionamento cursore?
053500090609         if  C1CsrRrn > *zero;
053600090609           C1RcdNbr = C1CsrRrn;
053700090609         else;
053800090609           C1RcdNbr = 1;
053900090609           write  MK21S00;             // (no rec.)
054000090609         endif;
054100090609
054200090609         //?Emissione videata?
054300090609         exfmt  MK21C01;
054400090609
054500090609         reset  ErrMessage;
054600090609         reset  ErrGenerico;
054700090609         clear  V1Dmsg;
054800090609
054900090609         SELECT;
055000120704
055100120704           //?F8=Storico?
055200120704           WHEN  dsp_aid = c_F08;
055300120704             exsr sr_F08;
055400100616
055500100616           //?F10=Nuova attività?
055600100616           WHEN  dsp_aid = c_F10;
055700100617             exsr sr_F10;
055800090609
055900090609           //?F12=Ritorno?
056000090609           WHEN  dsp_aid = c_F12;
056100100617           //?se non ho il campo impostato a '99' --> gestione attivita imposto F12
056200100617             IF  O21fxx <> '99';
056300100617               O21fxx = '12';
056400100617             ENDIF;
056500090609             $Fine = *on;
056600100809
056700100809           //?F22=Primo contatto?
056800100809           WHEN  dsp_aid = c_F22;
056900100809             exsr sr_F22;
057000090609
057100090609           //?Roll-Up?
057200090609           WHEN  dsp_aid = c_RollUp;
057300090609             exsr sr_RollUpS01;
057400090609
057500090609           //?SubFile vuoto?
057600090609           WHEN  S01nrr = *zero;
057700090609
057800090609           //?Invio?
057900090609           OTHER;
058000090609             // -?Gestione opzioni?
058100090609             exsr  sr_OpzS01;
058200090609             if  ErrGenerico;
058300090609               leavesr;
058400090609             endif;
058500100913           //?se ho il flag impostato a SI esco dal programma
058600100913           //?esco se ho fatto solo scelte 'G' altrimenti resto
058700100913           //?imposto O21fxx = '99' in modo da non richiamare il TRMK19R o TNTA88R dal chiamante
058800100913             IF  wEsci = 'S';
058900100913               O21fxx = '99';
059000100910               $Fine = *on;
059100100910             ENDIF;
059200090609
059300090609         ENDSL;
059400090609
059500090609       ENDSR;
059600090609
059700090609       //--------------------------------------------------------------
059800090609       //?Inizializzazione subfile S01                                 ?
059900090609       //--------------------------------------------------------------
060000090609       BEGSR  sr_InzS01;
060100090609
060200090609         //?Spegnimento degli indicatori di errore?
060300090609         %subst(IndDspF : 50) = *off;
060400090609
060500090609         //?Pulizia subfile?
060600090609         SflDsp_N    = *on;
060700090609         SflDspCtl_N = *on;
060800090609         write  MK21C01;
060900090609         SflDspCtl_N = *off;
061000090609         SflEnd      = *off;
061100090609
061200090609         clear  W_SflPag;
061300090609         clear  C1RcdNbr;
061400090609         clear  C1CsrRrn;
061500090609         clear  S01nrr;
061600090609         clear  V1Dmsg;
061700090609         ErrMessage  = *off;
061800090609         ErrGenerico = *off;
061900090609
062000090609         //?Impostazione campi di "testata"?
062100090609         select;
062200090609           when  I21ksc > *zero;
062300090609             C1Icpt = 'Cliente';
062400090609             C1Ccpt = %trim( %editc( I21ksc : 'X' ) );
062500090609             C1Dcpt = I21rsc;
062600090610             C1Icet = 'Potenziale  Tratt.';
062700090609           when  I21cpo > *zero;
062800090609             C1Icpt = 'Potenziale';
062900090609             C1Ccpt = %trim( %editc( I21cpo : 'X' ) );
063000090609             C1Dcpt = I21rsc;
063100090610             C1Icet = 'Cliente     Tratt.';
063200090609           when  I21ntc > *zero;
063300090609             C1Icpt = 'Trattativa';
063400090609             C1Ccpt = %trim( %editc( I21ntc : 'Z' ) );
063500090610             VisualTratt = *on;
063600090609             chain  ( I21ntc )  TIVIS000;
063700091019             if  %found(TIVIS05L);
063800090609               C1Dcpt = VISrag;
063900090610               wDate_dmy = %date( %editw( VISdat : '    -  -  ' ) : *iso );
064000090610               C1Cdat = %dec( wDate_dmy );
064100090609               C1Ctpv = VIStpv;
064200090609             else;
064300090609               C1Dcpt = *all'? ';
064400090609             endif;
064500090610             C1Icet = 'Cliente o Potenz. ';
064600090609         endsl;
064700090609
064800090609         //?Preparazione stringa SQL?
064900090609         exsr  sr_PrepSQL;
065000090609
065100090609         //?Caricamento della 1ª pagina di dati del subfile?
065200090609         exsr  sr_OpenCursor;
065300090609         exsr  sr_ReadCursor;
065400090609         exsr  sr_RollUpS01;
065500090609
065600090609         //?Impaginazione subfile?
065700090609         // -> forza l'impaginazione sul 1° rec. del subfile
065800090609         if S01nrr > *zero;
065900090609           C1RcdNbr  = 1;
066000090609           C1CsrRrn  = 1;
066100090609         else;
066200090609           clear C1RcdNbr;
066300090609         endif;
066400090609
066500090609       ENDSR;
066600120704
066700120704       //--------------------------------------------------------------
066800120704       //?F08 - Storico.
066900120704       //--------------------------------------------------------------
067000120704       BEGSR  sr_F08;
067100120704
067200120704       //?Interrogo lo storico del POTENZIALE
067300120704         clear TRMK10DS;
067400120704         IMK10cpo = I21cpo;
067500120704         IMK10rag = I21rsc;
067600120704         trmk10r (kpjba:trmk10ds);
067700120704         //?se torna errore emetto il messaggio
067800120704         IF  OMK10err <> *blanks;
067900120704         //?gestisco l'errore emetto window e poi avanti con subfile
068000120704           V1Dmsg = OMK10msg;
068100120704           ErrMessage  = *off;
068200120704           ErrGenerico = *off;
068300120704         ENDIF;
068400120704
068500120704       ENDSR;
068600100616
068700100616       //--------------------------------------------------------------
068800100616       //?F10 - Nuova attività.
068900100616       //--------------------------------------------------------------
069000100617       BEGSR  sr_F10;
069100100616
069200100809       //?Esco dal programma (arrivo da MK18 - MK01 - MK02 oppure da
069300100809       //?TA87 - TAI1 - TAI2)
069400100809       //?e il chiamante va diretto su MK19 nuova attività o TA88 nuova
069500100809       //?trattativa
069600100913         O21Fxx = '10';
069700100913         $Fine = *on;
069800100616
069900100616       ENDSR;
070000090609
070100100809       //--------------------------------------------------------------
070200100809       //?F22 - Primo contatto.
070300100809       //--------------------------------------------------------------
070400100809       BEGSR  sr_F22;
070500100809
070600100809         clear trmk17ds;
070700100809         IMK17patt = 'S';
070800100809         IMK17att  = 'T';
070900100809         IMK17pcau = 'S';
071000100809         IF  I21ksc = 0;
071100100809           IMK17cau  = '19';
071200100809         ENDIF;
071300100809         IF  I21ksc > 0;
071400100809           IMK17cau  = '20';
071500100809         ENDIF;
071600100809         IMK17cpo  = I21cpo;
071700100809         IMK17ksc  = I21ksc;
071800100809         IF w_IMK19com <> 0;
071900100809           IMK17co3  = w_IMK19com;
072000100809         ENDIF;
072100100809
072200100809         trmk17r (kpjba : TRMK17DS);
072300100810
072400100810       //?Imposto che devo ricaricare il subfile
072500100810         $InzS01 = *on;
072600100810       //?Chiudo cursore
072700100810         exsr sr_CloseCursor;
072800100810       //?Spengo indicatore di fine file
072900100810         $EoF = *off;
073000100809
073100100809       ENDSR;
073200100809
073300090609       //--------------------------------------------------------------
073400090609       //?Caricamento singola pagina nel subfile S01                   ?
073500090609       //--------------------------------------------------------------
073600090609       BEGSR  sr_RollUpS01;
073700090609
073800090609         S01nrr    = (W_SflPag * c_SflPag);
073900090609         W_SflPag += 1;
074000090609         SflNxtChg = *off;
074100090609
074200090609         //?Ciclo di caricamento dati da TIATC00F?
074300090609         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
074400090609              S01nrr    < c_MaxRec                and
074500090610             (Not $EoF   and   Not $Err);
074600090609
074700090617           clear  MK21S01;
074800090617
074900090609           // -?Impostazione attributi di visualizzazione?
075000090617           if  ATCdco > *zero;
075100090617             §DAdco = c_DspAtrNorm;
075200090609           else;
075300090617             §DAdco = c_DspAtrRed;
075400090609           endif;
075500090609
075600090609           // -?Caricamento del singolo record nel subfile S01?
075700090610           // - - Campi Hidden
075800090622           S1Hcpo  = ATCcpo;
075900090622           S1Hksc  = ATCksc;
076000090622           S1Hdco  = ATCdco;
076100090610           S1Htat  = ATCtat;
076200090610           S1Hatn  = ATCatn;
076300090610           S1Hatnp = ATCatnp;
076400090610           S1Hnrv  = ATCnrv;
076500090622           if  ATCdco > *zero;
076600090622             S1Hcco = ATCcco;
076700090622           else;
076800090622             S1Hcco = ATCcmm;
076900090622           endif;
077000090610           // - - Campi di Output
077100090609           if  ATCdco > *zero;
077200090610             wDate_dmy = %date( %editw( ATCdco : '    -  -  ' ) : *iso );
077300090609           else;
077400090610             wDate_dmy = %date( %editw( ATCdad : '    -  -  ' ) : *iso );
077500090609           endif;
077600090610           S1Cdco = %dec( wDate_dmy );
077700090609           if  ATCdco > *zero;
077800090609             S1Chco = ATChco / 100;
077900090609           else;
078000090609             S1Chco = ATChda / 100;
078100090609           endif;
078200090609           S1Ctat = ATCtat;
078300090610           if  ATCcac <> *blank;
078400090610             S1Ccac = ATCcac;
078500090610           else;
078600090610             S1Ccac = ATCcad;
078700090610           endif;
078800090610           select;
078900090610             when  I21ksc > *zero;
079000091022               S1Ccpo = %editc( ATCcpo : 'X' );
079100090610               if  ATCnra > *zero;
079200091022                 S1Cnrv = %editc( ATCnra : 'Z' );
079300090610               else;
079400091022                 S1Cnrv = %editc( ATCnrv : 'Z' );
079500090610               endif;
079600090610             when  I21cpo > *zero;
079700091022               if  ATCksc > *zero;
079800091022                 S1Ccpo = %editc( ATCksc : 'X' );
079900091022               endif;
080000090610               if  ATCnra > *zero;
080100091022                 S1Cnrv = %editc( ATCnra : 'Z' );
080200090610               else;
080300091022                 S1Cnrv = %editc( ATCnrv : 'Z' );
080400090610               endif;
080500090610             when  I21ntc > *zero;
080600090610               if  ATCksc > *zero;
080700090610                 S1Ccpo = %editc( ATCksc : 'X' );
080800090610               else;
080900090610                 S1Ccpo = %editc( ATCcpo : 'X' );
081000090610               endif;
081100090610           endsl;
081200090610
081300090610           // -?Decodifiche?
081400090610           // --> Commerciale
081500090610           clear  ds01;
081600090610           TBLcod = '01';
081700090622           TBLkey = %trim( %editc( S1Hcco : 'X' ) );
081800090610           chain (1 : '01' : TBLkey) TABEL;
081900090610           if  %found(TABEL00F)   and
082000090610               TBLflg = *blank;
082100090610             ds01 = TBLuni;
082200090610           else;
082300090610             §01age = %trimr(TBLkey) + ' ? ? ? ?';
082400090610           endif;
082500090610           S1Dcco = §01age;
082600090622           S1HccoD = §01age;
082700090610           // --> Causale contatto
082800090610           clear  dCCO;
082900090610           clear  TIBS02ds;
083000090610           T02mod = 'C';
083100090610           T02cod = 'CCO';
083200090622           T02ke1 = S1Ccac;
083300090610           T02sif = knsif;
083400090610           TNTBE_RicercaControllo ( kpjba : tibs02ds );
083500090610           if  T02err = *blank;
083600090611             dCCO    = T02uni;
083700090610           else;
083800090610             §CCOdes = *all'? ';
083900090610           endif;
084000090610           S1Dcac = §CCOdes;
084100100617         //?valorizzo il campo delle opzioni inibite per CAU attività
084200100617           S1Hopz = §CCOopz;
084300090609
084400090609           // -?Scrittura record nel subfile S01?
084500090609           S01nrr += 1;
084600090609           write  MK21S01;
084700090609
084800090609           // -?Lettura record successivo?
084900090609           exsr  sr_ReadCursor;
085000090609
085100090609         EndDo;
085200090609
085300090609         //?Visualizzazione del SFL (se ci sono dati)?
085400090609         SflDsp_N = (S01nrr <= *zero);
085500090609
085600090609         //?Attivazione del SFLEND?
085700090610         SflEnd   = (S01nrr >= c_MaxRec   or   $EoF   or   $Err);
085800090609
085900090609         //?Posizionamento cursore al 1° rcd della pagina?
086000090609         if  S01nrr > *zero;
086100090609           wPag     = S01nrr / c_SflPag;
086200090609           wRig     = S01nrr - (c_SflPag * wPag);
086300090609           C1RcdNbr = wPag * c_SflPag;
086400090609           if  wRig > *zero;
086500090609             C1RcdNbr += 1;
086600090609           else;
086700090609             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
086800090609           endif;
086900090609         else;
087000090609           clear  C1RcdNbr;
087100090609         endif;
087200090609
087300090609         C1CsrRrn = C1RcdNbr;
087400090609
087500090609       ENDSR;
087600090609
087700090609       //--------------------------------------------------------------
087800090609       //?Gestione opzioni del subfile S01                             ?
087900090609       //--------------------------------------------------------------
088000090609       BEGSR  sr_OpzS01;
088100090609
088200090609         clear  SavS01csr;
088300100913         clear  wEsci;
088400090609
088500090609         //?Ciclo di lettura subfile?
088600090609         readc  MK21S01;
088700090609
088800090609         DoW  Not %eof(TRMK21D);
088900090609
089000090609           SflNxtChg = *off;
089100090609           %subst(IndDspF : 50) = *off;
089200090609           C1RcdNbr = S01nrr;
089300090609
089400090609           select;
089500100617
089600100617             //?Controllo l'opzione inserita
089700100617             WHEN  S1Copz <> *blank and S1Hopz <> *blanks and
089800100617                   %scan(S1Copz:S1Hopz) > 0 ;
089900100617                   errMessage  = *on;
090000100617                   errGenerico = *on;
090100100617                   PosCurOpz   = *on;
090200100617                   V1Dmsg = $Msg(04);
090300100616
090400100617             //?G-Gestione
090500100616             when  S1Copz = 'G' and VisGestione;
090600100616               exsr sr_Gestione;
090700100913               IF  wEsci = *blanks;
090800100913                 wEsci = 'S';
090900100913               ENDIF;
091000100809
091100100809             //?2-Modifica
091200100809             when  S1Copz = '2' and VisGestione;
091300100809               exsr sr_Modifica;
091400100913               wEsci = 'N';
091500090609
091600090609             // -?5=Visualizzazione?
091700100928             when  S1Copz = '5' and not VisDaMk19;
091800090611               exsr sr_CallTRMK19;
091900100913               wEsci = 'N';
092000090609
092100090609             // -?8=Note?
092200090609             when  S1Copz = '8';
092300090609               exsr sr_CallTRMK20;
092400100913               wEsci = 'N';
092500090609
092600090609             // -?Nessuna opzione?
092700090609             when  S1Copz = *blank;
092800100913               wEsci = 'N';
092900090609
093000090609             // -?Opzione errata?
093100090609             other;
093200090609               ErrMessage  = *on;
093300090609               ErrGenerico = *on;
093400090609               PosCurOPZ   = *on;
093500090609               V1Dmsg = $Msg(01);
093600090609
093700090609           endsl;
093800090609
093900090609           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
094000090609           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
094100090609           if  S1Copz  <> *blank   and
094200090609              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
094300090609             SavS01csr   = C1RcdNbr;
094400090609           endif;
094500090609
094600090609           // -?Aggiornamento sfl?
094700090609           select;
094800090609             when  ErrMessage;
094900090609               SflNxtChg = *on;
095000090609               C1CsrRrn  = C1RcdNbr;
095100090609             when  ErrGenerico;
095200090609               C1CsrRrn  = C1RcdNbr;
095300090609               clear  S1Copz;
095400090609             other;
095500090609               clear  S1Copz;
095600090609           endsl;
095700090609           UPDATE  MK21S01;
095800090609           if  ErrGenerico   or   ErrMessage;
095900090609             leave;
096000090609           endif;
096100090609
096200090609           readc  MK21S01;
096300090609
096400090609         ENDDO;
096500090609
096600090609         // -?Riposiziono il cursore sul record contenente la prima opz.?
096700090609         //  ?(se non sono stati rilevati errori)?
096800090609         if  NOT ErrMessage   and   NOT ErrGenerico   and
096900090609             SavS01csr > *zero;
097000090609           C1CsrRrn = SavS01csr;
097100090609         endif;
097200090609
097300090609       ENDSR;
097400100616
097500100616       //--------------------------------------------------------------
097600100616       //?Richiamo gestione attività.
097700100616       //--------------------------------------------------------------
097800100616       BEGSR  sr_Gestione;
097900100616
098000100617       //?Come prima cosa controllo se che sia un'attività da eseguire
098100100616         IF  S1Hdco <> 0;
098200100616           ErrMessage  = *on;
098300100616           ErrGenerico = *on;
098400100616           PosCurOPZ   = *on;
098500100616           V1Dmsg = $Msg(02);
098600100616           leavesr;
098700100616         ENDIF;
098800100616
098900100617       //?Poi controllo se posso gestire l'attività
099000100616         exsr CtrAbiAtt;
099100100616         IF  ErrMessage;
099200100616           leavesr;
099300100616         ENDIF;
099400100617
099500100617       //?Imposto la DS buona dalla DS fasulla di richiamo
099600100617         TRMK19DS = TRMK19DS1;
099700100616
099800100616       //?A questo punto vado in gestione dell'attività
099900100617       //?se attività legata a trattativa richiamo gestione trattativa
100000100616         IF  S1Hnrv > 0;
100100100616           clear tnta88ds ;
100200100616           ITA88cmt = 'N';
100300100616           ITA88fle = 'G' ;
100400100616           ITA88nrv = S1Hnrv ;
100500100616           IF IMK19com <> 0;
100600100616             ITA88com  = IMK19com;
100700100616             ITA88comd = IMK19comd;
100800100616           ENDIF;
100900100616           Tnta88r ( kpjba : tnta88ds );
101000100910         //?ritorno il flag "99" in modo da NON richiamare di nuovo il TNTA88R dal chiamante
101100100617         //?non ho possibilità di controllare se F3 (F12 è attivo solo se nuova trattativa)
101200100617           O21fxx = '99';
101300100616         ENDIF;
101400100617       //?se attività NON legata a trattativa richiamo gestione attività
101500100616         IF  S1Hnrv = 0;
101600101014           IMK19cmt = 'S';
101700100616           IMK19fle = 'G';
101800130131           IMK19tco = S1Htat;
101900100616           IMK19tat = S1Htat;
102000100616           IMK19atn = S1Hatn;
102100100616           IMK19atnp= S1Hatnp;
102200100930           // Trmk19c (kpjba : trmk19ds);
102300100930           Trmk19r (kpjba : trmk19ds);
102400100617         //?ritorno il flag "99" in modo da NON richiamare di nuovo il TRMK19R dal chiamante
102500100617         //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
102600100617           IF  OMK19err = *blanks;
102700100617             O21fxx = '99';
102800100617           ENDIF;
102900100616         ENDIF;
103000100617
103100100617       //?Imposto che devo ricaricare il subfile
103200100617         $InzS01 = *on;
103300100617       //?Chiudo cursore
103400100617         exsr sr_CloseCursor;
103500100617       //?Spengo indicatore di fine file
103600100617         $EoF = *off;
103700100616
103800100616       ENDSR;
103900100809
104000100809       //--------------------------------------------------------------
104100100809       //?Richiamo Modifica attività.
104200100809       //--------------------------------------------------------------
104300100809       BEGSR  sr_Modifica;
104400100809
104500100809       //?Come prima cosa controllo se che sia un'attività da eseguire
104600100809         IF  S1Hdco <> 0;
104700100809           ErrMessage  = *on;
104800100809           ErrGenerico = *on;
104900100809           PosCurOPZ   = *on;
105000100809           V1Dmsg = $Msg(02);
105100100809           leavesr;
105200100809         ENDIF;
105300100809
105400100809       //?Poi controllo se posso gestire l'attività
105500100809         exsr CtrAbiAtt;
105600100809         IF  ErrMessage;
105700100809           leavesr;
105800100809         ENDIF;
105900100809
106000100809       //?Imposto la DS buona dalla DS fasulla di richiamo
106100100809         TRMK19DS = TRMK19DS1;
106200100809
106300100809       //?A questo punto vado in gestione dell'attività
106400100913       //?sempre
106500101014         IMK19cmt = 'S';
106600100913         IMK19fle = '2';
106700130131         IMK19tco = S1Htat;
106800100913         IMK19tat = S1Htat;
106900100913         IMK19atn = S1Hatn;
107000100913         IMK19atnp= S1Hatnp;
107100100930        // Trmk19c (kpjba : trmk19ds);
107200100930         Trmk19r (kpjba : trmk19ds);
107300100913       //?ritorno il flag "99" in modo da NON richiamare di nuovo il TRMK19R dal chiamante
107400100913       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
107500100913         IF  OMK19err = *blanks;
107600100913           O21fxx = '99';
107700100809         ENDIF;
107800100809
107900100809       //?Imposto che devo ricaricare il subfile
108000100809         $InzS01 = *on;
108100100809       //?Chiudo cursore
108200100809         exsr sr_CloseCursor;
108300100809       //?Spengo indicatore di fine file
108400100809         $EoF = *off;
108500100809
108600100809       ENDSR;
108700090609
108800090609       //--------------------------------------------------------------
108900090609       //?Richiamo pgm di visualizzazione dati contatto                ?
109000090609       //--------------------------------------------------------------
109100090611       BEGSR  sr_CallTRMK19;
109200090609
109300100616         clear  TRMK19ds;
109400100616         IMK19cmt  = 'N';
109500090611         IMK19fle  = '5';
109600091105         IMK19fpr  = 'P';
109700090618         IMK19tco  = S1Htat;
109800090622         IMK19cpo  = S1Hcpo;
109900090622         if  S1Hdco > *zero;
110000090611           IMK19fatt = 'S';
110100090611         endif;
110200090611         IMK19com  = S1Hcco;
110300090611         IMK19comd = S1HccoD;
110400090611         IMK19tat  = S1Htat;
110500090611         IMK19atn  = S1Hatn;
110600090611         IMK19atnp = S1Hatnp;
110700090611
110800100928         trmk19r ( KPJBA : TRMK19ds );
110900090611
111000090611         if  OMK19err = 'E';
111100090611           V1Dmsg  = OMK19msg;
111200090611           ErrMessage  = *on;
111300090611           ErrGenerico = *on;
111400090611         endif;
111500090609
111600090609       ENDSR;
111700090609
111800090609       //--------------------------------------------------------------
111900090609       //?Richiamo pgm di visualizzazione note contatto                ?
112000090609       //--------------------------------------------------------------
112100090609       BEGSR  sr_CallTRMK20;
112200090609
112300090611         reset  TRMK20ds;
112400090622         IMK20cpo  = S1Hcpo;
112500090622         IMK20ksc  = S1Hksc;
112600090609         IMK20tat  = S1Htat;
112700090609         IMK20atn  = S1Hatn;
112800090609         IMK20atnp = S1Hatnp;
112900090609         IMK20nrv  = S1Hnrv;
113000090609
113100090609         trmk20r ( KPJBA : TRMK20ds );
113200090609
113300090609         if  OMK20fxx  = '03';
113400090609           O21fxx = '03';
113500090609           $Fine  = *on;
113600090609           ErrGenerico = *on;
113700090609         endif;
113800090611
113900090611         if  OMK19err = 'E';
114000090611           V1Dmsg  = OMK20msg;
114100090611           ErrMessage  = *on;
114200090611           ErrGenerico = *on;
114300090611         endif;
114400090609
114500090609       ENDSR;
114600100616
114700100616       //--------------------------------------------------------------
114800100616       //?Controllo se utente abilitato all'attività.
114900100616       //--------------------------------------------------------------
115000100616       BEGSR CtrAbiAtt;
115100100616
115200100616       //?abilitato come commerciale
115300100616         clear TNTAA1DS;
115400100616         ITAA1caut = '§utegtc';
115500100616         ITAA1cmm  = S1Hcco;
115600100616         kpjbu     = TNTAA1DS;
115700100616         tntaa1c (kpjba);
115800100616         TNTAA1DS = kpjbu;
115900100616         IF  OTAA1fabi <> 'N';
116000100616           leavesr;
116100100616         ENDIF;
116200100616
116300100616       //?abilitato come cliente
116400100616         IF  S1Hksc > 0;
116500100616           clear TNTAA1DS;
116600100616           ITAA1caut = '§utegtc';
116700100616           ITAA1ksc  = %dec(S1Hksc:7:0);
116800100616           kpjbu     = tntaa1ds;
116900100616           tntaa1c (kpjba);
117000100616           TNTAA1DS = kpjbu;
117100100616           IF  OTAA1fabi <> 'N';
117200100616             leavesr;
117300100616           ENDIF;
117400100616         ENDIF;
117500100616
117600100616       //?se arrivo a questo punto vuol dire che l'utente non è abilitato
117700100616       //?al commerciale o al cliente
117800100616         ErrMessage  = *on;
117900100616         ErrGenerico = *on;
118000100616         PosCurOPZ   = *on;
118100100616         V1Dmsg = $Msg(03);
118200100616
118300100616       ENDSR;
118400090609
118500090609       //--------------------------------------------------------------
118600090609       //?Stampa segnalazione dell'errore rilevato via SQL             ?
118700090609       //--------------------------------------------------------------
118800090609       BEGSR  sr_PrintErrSQL;
118900090609
119000090609         //?Stampa del Dump?
119100090609         Dump(A);
119200090609
119300090609         //?Stampa del Job-Log?
119400090609         Qcmd = 'DSPJOBLOG job(*) output(*print)';
119500090609         exsr  sr_ExecCmd;
119600090609
119700090609         //?Chiusura del programma?
119800090609         exsr  sr_RoutEnd;
119900090609
120000090609       ENDSR;
120100090609
120200090609       //--------------------------------------------------------------
120300090609       //?Esecuzione del comando (già impostato).                      ?
120400090609       //--------------------------------------------------------------
120500090609       BEGSR  sr_ExecCmd;
120600090609
120700090609         clear Qcap0100;
120800090609         Qcabcsdh = *off;
120900090609         Qcapa    = *off;
121000090609         Qcacmdss = *off;
121100090609         Qcaerved = *allX'00';
121200090609
121300090609         clear Qusec;
121400090609         Qusbprv  = %size(Qusec);
121500090609
121600090609         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
121700090609                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
121800090609                           0 : 0 : Qusec);
121900090609
122000090609         if  Qusei <> *blank;
122100090609           exsr  sr_MsgErr;
122200090609         endif;
122300090609
122400090609       ENDSR;
122500090609
122600090609       //--------------------------------------------------------------
122700090609       //?Invio messaggio per segnalazione errori.                     ?
122800090609       //--------------------------------------------------------------
122900090609       BEGSR  sr_MsgErr;
123000090609
123100090609
123200090609       ENDSR;
123300090609
123400090609       //--------------------------------------------------------------
123500090609       //?Operazioni finali.                                           ?
123600090609       //--------------------------------------------------------------
123700090609       BEGSR  sr_RoutEnd;
123800090609
123900090609         //?Chiusura cursore SQL?
124000090609         exsr  sr_CloseCursor;
124100090609
124200090609         //?Chiusura pgm?
124300090610         kpjbu = TRMK21ds;
124400090609         return;
124500090609
124600090609       ENDSR;
124700090609
124800090609      /end-free
124900090609
125000090609       //--------------------------------------------------------------
125100090609       //?Schiere a tempo di compilazione.                             ?
125200090609       //--------------------------------------------------------------
125300090609
125400090609** - $Msg -------------------------------------------------------------------*
125500090609Opzione errata                                                                 1
125600100616Opzione errata Attività già eseguita                                           2
125700100616Opzione errata Utente non abilitato alla gestione dell'attività                3
125800100617Opzione non valida per questa attività                                         4
