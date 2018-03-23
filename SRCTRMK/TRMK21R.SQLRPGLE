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
001900130917       // - Anagrafica Commerciali
002000130917     fAZCMM01L  if   e           k disk
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
004500130917     d c_F04           c                   const(x'34')
004600090609     d c_F05           c                   const(x'35')
004700090609     d c_F06           c                   const(x'36')
004800090609     d c_F07           c                   const(x'37')
004900090609     d c_F08           c                   const(x'38')
005000090609     d c_F09           c                   const(x'39')
005100090609     d c_F10           c                   const(x'3A')
005200130917     d c_F11           c                   const(x'3B')
005300090609     d c_F12           c                   const(x'3C')
005400090609     d c_F13           c                   const(x'B1')
005500090609     d c_F14           c                   const(x'B2')
005600090609     d c_F15           c                   const(x'B3')
005700090609     d c_F16           c                   const(x'B4')
005800090609     d c_F17           c                   const(x'B5')
005900090609     d c_F18           c                   const(x'B6')
006000090609     d c_F19           c                   const(x'B7')
006100090609     d c_F20           c                   const(x'B8')
006200090609     d c_F21           c                   const(x'B9')
006300090609     d c_F22           c                   const(x'BA')
006400090609     d c_F23           c                   const(x'BB')
006500090609     d c_F24           c                   const(x'BC')
006600090609     d c_Enter         c                   const(x'F1')
006700090609     d c_RollDown      c                   const(x'F4')
006800090609     d c_RollUp        c                   const(x'F5')
006900090609
007000090609       // - Attributi di Visualizzazione
007100090609     d c_DspAtrNorm    c                   const(x'A0')
007200090610     d c_DspAtrRed     c                   const(x'A8')
007300090609
007400090609       //--------------------------------------------------------------
007500090609       //?Definizione schiere.                                         ?
007600090609       //--------------------------------------------------------------
007700090609
007800090609       // - Messaggi di errore
007900100616     d $Msg            s             78    dim(10) ctdata perrcd(1)
008000090609
008100090609       //--------------------------------------------------------------
008200090609       //?Definizione aree dati.                                       ?
008300090609       //--------------------------------------------------------------
008400090609
008500090609       // - Dati utente
008600090609     d §AzUte        e ds                  extname(AZUTE00F)
008700090609     d                                     dtaara
008800090609     d §DatiUte      e ds                  extname(dDatiUte)
008900090609     d                                     dtaara
009000090609
009100090609       //--------------------------------------------------------------
009200090609       //?Definizione strutture dati.                                  ?
009300090609       //--------------------------------------------------------------
009400090609
009500090609       // - Status ds
009600090609     d Status         sds
009700090609     d  SDSpgm           *proc
009800090609     d  SDSusr               254    263
009900090609
010000090609       // - InfDS
010100090609     d InfDspF         ds
010200090609     d   dsp_aid             369    369a
010300130917     d*//sfl_rrn             376    377i 0
010400130917     d*//min_nrr             378    379i 0
010500130917     d*//num_rcds            380    381i 0
010600090609
010700090609       // - Indicatori su DspF
010800090609     d IndDspF         ds                  inz
010900090609         // - Emissione messaggio di errore
011000090609     d   ErrMessage                    n   overlay(IndDspF : 28)
011100090610         // - Condizionamento di visualizzazione
011200090610     d   VisualTratt                   n   overlay(IndDspF : 41)
011300100616     d   VisGestione                   n   overlay(IndDspF : 42)
011400100809     d   VisNewAtt                     n   overlay(IndDspF : 43)
011500100809     d   VisNewTra                     n   overlay(IndDspF : 44)
011600100928     d   VisDaMk19                     n   overlay(IndDspF : 45)
011700110211     d   Codificato                    n   overlay(IndDspF : 46)
011800120704     d   F8Attivo                      n   overlay(IndDspF : 47)
011900120706     d   F8AttivoPink                  n   overlay(IndDspF : 48)
012000090609         // - Indicatori di gestione del subfile
012100090609     d   SflDsp_N                      n   overlay(IndDspF : 30)
012200090609     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012300090609     d   SflNxtChg                     n   overlay(IndDspF : 32)
012400090609     d   SflEnd                        n   overlay(IndDspF : 33)
012500090609         // - Posizionamento cursore & segnalazione errore
012600090609     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012700090609         //   - Riemissione videata
012800090609     d   ErrGenerico                   n   overlay(IndDspF : 99)
012900090609
013000090609       // - Parametri ricevuti
013100090609     d KPJBA         e ds
013200090610     d TRMK21ds      e ds                  inz
013300130917     d TRMK01ds      e ds                  inz
013400090610
013500090610       // - Dati estratti via SQL
013600090610     d TIATCds       e ds                  extname(TIATC00F)  inz
013700090609
013800090609       // - Parametri per Reperimento dati utente
013900090609     d TIBS34ds      e ds                  inz
014000100616
014100100616      // - controllo abilitazioni
014200100616     d TNTAA1DS      e ds                  inz
014300100616
014400100616      // - Gestione trattative commerciali
014500100616     d TNTA88ds      e ds                  inz
014600120704
014700120704       // - Storico attività
014800130917     d TRMK10DS      e ds                  inz
014900100809
015000100809       // - Primo contatto
015100130917     d TRMK17DS      e ds                  inz
015200090611
015300090611       // - Gestione/Visualizzazione contatti commerciali
015400100616     d TRMK19ds      e ds
015500100617
015600100617       // - Gestione/Visualizzazione contatti commerciali
015700100617     d TRMK19ds1     e ds                  extname(TRMK19DS) prefix(w_)
015800090609
015900090609       // - Gestione note clienti/potenziali/trattative
016000090609     d TRMK20ds      e ds                  inz
016100090609     d   IMK20tla    e                     inz('L')
016200090609     d   IMK20flm    e                     inz('I')
016300100616
016400100616      // -  Controllo se ci sono altre attività aperte
016500100616     d TRMK85ds      e ds                  inz
016600090609
016700090609       // - Ricerca/Controllo tabelle
016800090609     d TIBS02ds      e ds                  inz
016900090609     d   T02Mod      e                     inz('C')
017000090609
017100090609       // - Tabella "CCO" = Causale contatto
017200090609     d dCCO          e ds                  inz
017300090609
017400090609       //--------------------------------------------------------------
017500090609       //?Definizione variabili globali.                               ?
017600090609       //--------------------------------------------------------------
017700090609
017800090609       // - Flags booleani
017900090609     d $Fine           s               n   inz
018000090609     d $EoF            s               n   inz
018100090609     d $Err            s               n   inz
018200090609     d $InzS01         s               n   inz(*on)
018300100616     d $InzW01         s               n   inz
018400090609
018500090609       // - Variabili per la gestione del video
018600090609     d $Video          s              2    inz('S1')
018700090609     d W_SflPag        s              3  0 inz
018800090609     d wPag            s              4  0 inz
018900090609     d wRig            s              3  0 inz
019000090609     d S01nrr          s                   like(C1RcdNbr) inz
019100090609     d SavS01csr       s                   like(C1RcdNbr) inz
019200090609
019300090609       // - Stringa SQL da eseguire
019400090609     d wSQL            s           2048    Varying        inz
019500090609
019600090609       // - Parametri per API QCAPCMD (Process Commands)
019700090609      /copy qsysinc/qrpglesrc,QCAPCMD
019800090609     d Qcmd            s            128    Varying        inz
019900090609
020000090609       // - Parametri gestione errori API.
020100090609      /copy qsysinc/qrpglesrc,QUSEC
020200090609
020300090609       // - Campi di comodo
020400090610     d wDate_dmy       s               d   datfmt(*dmy)   inz
020500100913     d wEsci           s              1    inz
020600090609
020700090609       //--------------------------------------------------------------
020800090609       //?Definizione prototipi procedure.                             ?
020900090609       //--------------------------------------------------------------
021000120704
021100120704      // - Storico attività
021200120704     d TRMK10R         pr                  extpgm('TRMK10R')
021300120704     d  kpjba                              likeds(KPJBA)
021400120704     d  trmk10ds                           likeds(trmk10ds)
021500100616
021600100616     d TRMK85R         pr                  extpgm('TRMK85R')
021700100616     d  trmk85ds                           likeds(trmk85ds)
021800100809
021900100809      // - Inserimento attività primo contatto
022000100809     d trmk17r         pr                  extpgm('TRMK17R')
022100100809     d  kpjba                              likeds(KPJBA)
022200100809     d  trmk17ds                           likeds(TRMK17ds)
022300100617
022400100617      // - Inserimento attività
022500130917     d*// trmk19c         pr                  extpgm('TRMK19C')
022600130917     d*//  kpjba                              likeds(KPJBA)
022700130917     d*//  trmk19ds                           likeds(TRMK19ds)
022800090609
022900090609       // - Reperimento dati utente
023000090609      /copy gaitrasrc/srcProtoPR,TIBS34R
023100090609
023200090609       // - Ricerca/Controllo tabelle
023300090609      /copy gaitrasrc/srcProtoPR,TIBS02R
023400100616
023500100616       // - Controllo se utente abilitato
023600100616      /copy gaitrasrc/srcprotopr,tntaa1c
023700100616
023800100616       // - Gestione trattative
023900100616      /copy gaitrasrc/srcprotopr,tnta88r
024000090611
024100090611       // - Gestione/Visualizzazione contatti commerciali
024200100928      /copy gaitrasrc/srcProtoPR,TRMK19R
024300090609
024400090609       // - Gestione note clienti/potenziali/trattative
024500090609      /copy gaitrasrc/srcProtoPR,TRMK20R
024600090609
024700090609       // - Api QCAPCMD (Process Commands)
024800090609      /copy gaitrasrc/srcProtoPR,QCAPCMD
024900090609
025000090609       //--------------------------------------------------------------
025100090609       //?Definizione key-list.                                        ?
025200090609       //--------------------------------------------------------------
025300090609
025400090609
025500090609       //--------------------------------------------------------------
025600090609       //?M A I N - L I N E                                            ?
025700090609       //--------------------------------------------------------------
025800090609
025900090609     c     *Entry        plist
026000090609     c                   parm                    KPJBA
026100100617     c                   parm                    TRMK19ds1
026200090609
026300090609      /free
026400090609
026500090609       //?Operazioni iniziali?
026600090609       exsr sr_RoutInz;
026700090609
026800090609       DoW  $Fine = *off;
026900090609         select;
027000100616           // - Controllo se altre attività aperte
027100100616           when $Video = 'W1';
027200100616             exsr sr_GesW01;
027300090609           // - Gestione subfile
027400090609           when $Video = 'S1';
027500090609             exsr sr_GesS01;
027600090609           other;
027700090609             $Fine = *on;
027800090609         endsl;
027900090609       enddo;
028000090609
028100090609       //?Operazioni finali?
028200090609       exsr sr_RoutEnd;
028300090609
028400090609       //--------------------------------------------------------------
028500090609       //?Operazioni iniziali.                                         ?
028600090609       //--------------------------------------------------------------
028700090609       BEGSR  sr_RoutInz;
028800090609
028900090609         *inLR = *on;
029000090610
029100090610         //?Impostazione opzioni per SQL?
029200090610         exec sql   set  option  DynUsrPrf = *Owner,
029300090610                                 CloSqlCsr = *EndMod;
029400100616
029500100616       //?Controllo se passati + parametri
029600100616       //?forzo gestione
029700100809        IF  %parms > 1 and w_IMK19tco = 'T';
029800100810          //I21mod = 'G';
029900100809          VisNewAtt = *on;
030000100809          VisNewTra = *off;
030100100616        ENDIF;
030200100809        IF  %parms > 1 and w_IMK19tco = 'O';
030300100810          //I21mod = 'G';
030400100809          VisNewAtt = *off;
030500100809          VisNewTra = *on;
030600100809        ENDIF;
030700090609
030800090609         //?Reperimento dati job?
030900090609         exsr  sr_DatiJob;
031000090609
031100090609         //?Impostazione nome programma a video?
031200090609         V1Tpgm = SDSpgm;
031300090610
031400090610         //?Impostazione parametri ricevuti?
031500090610         if  kpjbu <> *blank;
031600090610           TRMK21ds = kpjbu;
031700090610         else;
031800090610           $Fine = *on;
031900090610         endif;
032000100810
032100100810       //?Se richiamato in modalità 'V' devo attivare la gestione e la modifica +
032200100810       //?tasti funzionali
032300100810         IF  I21mod = 'V';
032400100810           VisGestione = *on;
032500100810         ENDIF;
032600100616
032700100616       //?Se richiamato in modalità 'G' imposto $Video = 'W1'
032800100616         IF  I21mod = 'G';
032900100616           $Video = 'W1';
033000100616           $InzS01 = *off;
033100100616           $InzW01 = *on;
033200100616         ENDIF;
033300100928
033400100928       //?Se richiamato in modalità '9' arrivo da TRMK19R visualizzo solo come se
033500100928       //?richiamato con 'I', ma non do la possibilità di interrogare le attività con
033600100928       //?opzione 5
033700100928         IF  I21mod = '9';
033800100928           VisDaMk19 = *on;
033900100928         ENDIF;
034000110211
034100110211       //?Aggancio il potenziale per reperire la categoria
034200110211         Codificato = *off;
034300110211         IF  I21cpo > 0 and I21ksc = 0;
034400110211           chain  I21cpo  TNCPO01L;
034500110211           IF  %found(TNCPO01L);
034600110211             Codificato = (CPOfls = 'C');
034700110211           ENDIF;
034800110211         ENDIF;
034900120704
035000120704         F8Attivo = (I21cpo > 0);
035100120706
035200120706         IF  F8Attivo;
035300120706           chain (I21cpo) TICRM01L;
035400120706           IF  %found(TICRM01L);
035500120706             F8AttivoPink = *on;
035600120706           ELSE;
035700120706             F8AttivoPink = *off;
035800120706           ENDIF;
035900120706         ENDIF;
036000090609
036100090609       ENDSR;
036200090609
036300090609       //--------------------------------------------------------------
036400090609       //?Reperimento Dati del job (Utente/Operativi).                 ?
036500090609       //--------------------------------------------------------------
036600090609       BEGSR  sr_DatiJob;
036700090609
036800090609         in(e) §AzUte;
036900090609         if NOT %error;
037000090609           in(e) §DatiUte;
037100090609         endif;
037200090609         if %error or RSut = *blank;
037300090609           tibs34r ( tibs34ds );
037400090609           in §AzUte;
037500090609           in §DatiUte;
037600090609         endif;
037700090609
037800090609       ENDSR;
037900090609
038000090609       //--------------------------------------------------------------
038100090609       //?Preparazione stringa SQL                                     ?
038200090609       //--------------------------------------------------------------
038300090609       BEGSR  sr_PrepSQL;
038400090609
038500090618         wSQL = 'select TIATC00F.*, case +
038600090622                           when (ATCdco*1000000)+ATChco > 0 +
038700090618                           then (ATCdco*1000000)+ATChco +
038800090618                           else (ATCdad*1000000)+ATChda +
038900090618                           end  as WWWord +
039000090609                   from TIATC00F +
039100090610                  where';
039200090610
039300090610         select;
039400090610           when  I21ksc > *zero;
039500090610             wSQL += ' ATCksc = ' + %editc(I21ksc:'X');
039600090610           when  I21cpo > *zero;
039700090610             wSQL += ' ATCcpo = ' + %editc(I21cpo:'X');
039800090610           when  I21ntc > *zero;
039900090610             wSQL += ' ATCnrv = ' + %editc(I21ntc:'X');
040000090610         endsl;
040100090610
040200090622         wSQL += ' order by WWWord desc +
040300090617
040400090617                   for fetch only';
040500090609
040600090609       ENDSR;
040700090609
040800090609       //--------------------------------------------------------------
040900090609       //?Apertura cursore.                                            ?
041000090609       //--------------------------------------------------------------
041100090609       BEGSR  sr_OpenCursor;
041200090609
041300090609         //?Dichiarazione cursore?
041400090609         exec sql   prepare S1   from :wSQL;
041500090609         exec sql   declare C1   cursor for S1;
041600090609
041700090609         //?Apertura del cursore?
041800090609         exec sql   open C1;
041900090609
042000090609         if  SQLcode < *zero;
042100090609           $Err = *on;
042200090609           $EoF = *on;
042300090609           exsr  sr_PrintErrSQL;
042400090609         endif;
042500090609
042600090609       ENDSR;
042700090609
042800090609       //--------------------------------------------------------------
042900090609       //?Chiusura cursore.                                            ?
043000090609       //--------------------------------------------------------------
043100090609       BEGSR  sr_CloseCursor;
043200090609
043300090609         //?Chiusura del cursore?
043400090609         exec sql   close C1;
043500090609
043600090609       ENDSR;
043700090609
043800090609       //--------------------------------------------------------------
043900090609       //?Lettura cursore.                                             ?
044000090609       //--------------------------------------------------------------
044100090609       BEGSR  sr_ReadCursor;
044200090609
044300090609         clear  TIATCds;
044400090609
044500090609         exec sql   fetch next   from C1   into :TIATCds;
044600090609
044700090609         select;
044800090609           when  SQLcode = 100;
044900090609             $EoF = *on;
045000090609           when  SQLcode < *zero;
045100090609             $Err = *on;
045200090609             $Eof = *on;
045300090609             exsr  sr_PrintErrSQL;
045400090609         endsl;
045500090609
045600090609       ENDSR;
045700100616
045800100616       //--------------------------------------------------------------
045900100616       //?Gestione controllo altre attività aperte                     ?
046000100616       //--------------------------------------------------------------
046100100616       BEGSR  sr_GesW01;
046200100616
046300100616       //?Controllo attività
046400100616         IF  $InzW01;
046500100616           exsr  sr_InzW01;
046600100616           $InzW01 = *off;
046700100616         ENDIF;
046800100722
046900100722       //?Se errore esco e non emetto la videata
047000100722         IF  $Fine;
047100100722           leavesr;
047200100722         ENDIF;
047300100616
047400100616       //?Emissione window di avviso
047500100616         exfmt  MK21W01;
047600100616
047700100616       //?Imposto i dati per visualizzare il subfile
047800100616         $InzS01 = *on;
047900100616         $Video = 'S1';
048000100616
048100100616       ENDSR;
048200100616
048300100616       //--------------------------------------------------------------
048400100616       //?Controllo attività per W01.
048500100616       //--------------------------------------------------------------
048600100616       BEGSR  sr_InzW01;
048700100616
048800100616         VisGestione = *off;
048900100616
049000100616       //?Per prima cosa richiamo pgm di controllo altre attività aperte
049100100616         clear TRMK85ds;
049200100616         IMK85cpo = I21cpo;
049300100616         IMK85ksc = I21ksc;
049400100616         IMK85nrv = I21ntc;
049500100616         trmk85r (trmk85ds);
049600100616         //?se torna errore <> "9" ho trovato delle attività aperte
049700100616         IF  OMK85err <> *blanks and OMK85err <> '9';
049800100616         //?gestisco l'errore emetto window e poi avanti con subfile
049900100616           VisGestione = *on;
050000100722           leavesr;
050100100616         ENDIF;
050200100722         //?se torna errore;
050300100722         //?torno al chiamante con errore
050400100722         IF  OMK85err <> *blanks;
050500100722           O21err = '1';
050600100722           O21msg = OMK85msg;
050700100722           $Fine = *on;
050800100722           leavesr;
050900100722         ENDIF;
051000100616         //?se NON torna errore esco dal pgm, potenziale senza attività aperte
051100100616         //?posso crearne una nuova
051200100616         IF  OMK85err = *blanks;
051300100616           $Fine = *on;
051400100616         ENDIF;
051500100616
051600100616       ENDSR;
051700090609
051800090609       //--------------------------------------------------------------
051900090609       //?Gestione subfile S01                                         ?
052000090609       //--------------------------------------------------------------
052100090609       BEGSR  sr_GesS01;
052200090609
052300090609         //?Inizializzazione subfile?
052400090609         if  $InzS01 = *on;
052500090609           exsr  sr_InzS01;
052600090609           $InzS01 = *off;
052700090609         endif;
052800090609
052900090609         //?Emissione Testata e Piede con tasti funzionali abilitati?
053000090609         write  MK21T01;
053100090609         write  MK21P01;
053200090609
053300090609         //?Posizionamento cursore?
053400090609         if  C1CsrRrn > *zero;
053500090609           C1RcdNbr = C1CsrRrn;
053600090609         else;
053700090609           C1RcdNbr = 1;
053800090609           write  MK21S00;             // (no rec.)
053900090609         endif;
054000090609
054100090609         //?Emissione videata?
054200090609         exfmt  MK21C01;
054300090609
054400090609         reset  ErrMessage;
054500090609         reset  ErrGenerico;
054600090609         clear  V1Dmsg;
054700090609
054800090609         SELECT;
054900120704
055000120704           //?F8=Storico?
055100120704           WHEN  dsp_aid = c_F08;
055200120704             exsr sr_F08;
055300100616
055400100616           //?F10=Nuova attività?
055500100616           WHEN  dsp_aid = c_F10;
055600100617             exsr sr_F10;
055700090609
055800090609           //?F12=Ritorno?
055900090609           WHEN  dsp_aid = c_F12;
056000100617           //?se non ho il campo impostato a '99' --> gestione attivita imposto F12
056100100617             IF  O21fxx <> '99';
056200100617               O21fxx = '12';
056300100617             ENDIF;
056400090609             $Fine = *on;
056500100809
056600100809           //?F22=Primo contatto?
056700100809           WHEN  dsp_aid = c_F22;
056800100809             exsr sr_F22;
056900090609
057000090609           //?Roll-Up?
057100090609           WHEN  dsp_aid = c_RollUp;
057200090609             exsr sr_RollUpS01;
057300090609
057400090609           //?SubFile vuoto?
057500090609           WHEN  S01nrr = *zero;
057600090609
057700090609           //?Invio?
057800090609           OTHER;
057900090609             // -?Gestione opzioni?
058000090609             exsr  sr_OpzS01;
058100090609             if  ErrGenerico;
058200090609               leavesr;
058300090609             endif;
058400100913           //?se ho il flag impostato a SI esco dal programma
058500100913           //?esco se ho fatto solo scelte 'G' altrimenti resto
058600100913           //?imposto O21fxx = '99' in modo da non richiamare il TRMK19R o TNTA88R dal chiamante
058700100913             IF  wEsci = 'S';
058800100913               O21fxx = '99';
058900100910               $Fine = *on;
059000100910             ENDIF;
059100090609
059200090609         ENDSL;
059300090609
059400090609       ENDSR;
059500090609
059600090609       //--------------------------------------------------------------
059700090609       //?Inizializzazione subfile S01                                 ?
059800090609       //--------------------------------------------------------------
059900090609       BEGSR  sr_InzS01;
060000090609
060100090609         //?Spegnimento degli indicatori di errore?
060200090609         %subst(IndDspF : 50) = *off;
060300090609
060400090609         //?Pulizia subfile?
060500090609         SflDsp_N    = *on;
060600090609         SflDspCtl_N = *on;
060700090609         write  MK21C01;
060800090609         SflDspCtl_N = *off;
060900090609         SflEnd      = *off;
061000090609
061100090609         clear  W_SflPag;
061200090609         clear  C1RcdNbr;
061300090609         clear  C1CsrRrn;
061400090609         clear  S01nrr;
061500090609         clear  V1Dmsg;
061600090609         ErrMessage  = *off;
061700090609         ErrGenerico = *off;
061800090609
061900090609         //?Impostazione campi di "testata"?
062000090609         select;
062100090609           when  I21ksc > *zero;
062200090609             C1Icpt = 'Cliente';
062300090609             C1Ccpt = %trim( %editc( I21ksc : 'X' ) );
062400090609             C1Dcpt = I21rsc;
062500090610             C1Icet = 'Potenziale  Tratt.';
062600090609           when  I21cpo > *zero;
062700090609             C1Icpt = 'Potenziale';
062800090609             C1Ccpt = %trim( %editc( I21cpo : 'X' ) );
062900090609             C1Dcpt = I21rsc;
063000090610             C1Icet = 'Cliente     Tratt.';
063100090609           when  I21ntc > *zero;
063200090609             C1Icpt = 'Trattativa';
063300090609             C1Ccpt = %trim( %editc( I21ntc : 'Z' ) );
063400090610             VisualTratt = *on;
063500090609             chain  ( I21ntc )  TIVIS000;
063600091019             if  %found(TIVIS05L);
063700090609               C1Dcpt = VISrag;
063800090610               wDate_dmy = %date( %editw( VISdat : '    -  -  ' ) : *iso );
063900090610               C1Cdat = %dec( wDate_dmy );
064000090609               C1Ctpv = VIStpv;
064100090609             else;
064200090609               C1Dcpt = *all'? ';
064300090609             endif;
064400090610             C1Icet = 'Cliente o Potenz. ';
064500090609         endsl;
064600090609
064700090609         //?Preparazione stringa SQL?
064800090609         exsr  sr_PrepSQL;
064900090609
065000090609         //?Caricamento della 1ª pagina di dati del subfile?
065100090609         exsr  sr_OpenCursor;
065200090609         exsr  sr_ReadCursor;
065300090609         exsr  sr_RollUpS01;
065400090609
065500090609         //?Impaginazione subfile?
065600090609         // -> forza l'impaginazione sul 1° rec. del subfile
065700090609         if S01nrr > *zero;
065800090609           C1RcdNbr  = 1;
065900090609           C1CsrRrn  = 1;
066000090609         else;
066100090609           clear C1RcdNbr;
066200090609         endif;
066300090609
066400090609       ENDSR;
066500120704
066600120704       //--------------------------------------------------------------
066700120704       //?F08 - Storico.
066800120704       //--------------------------------------------------------------
066900120704       BEGSR  sr_F08;
067000120704
067100120704       //?Interrogo lo storico del POTENZIALE
067200120704         clear TRMK10DS;
067300120704         IMK10cpo = I21cpo;
067400120704         IMK10rag = I21rsc;
067500120704         trmk10r (kpjba:trmk10ds);
067600120704         //?se torna errore emetto il messaggio
067700120704         IF  OMK10err <> *blanks;
067800120704         //?gestisco l'errore emetto window e poi avanti con subfile
067900120704           V1Dmsg = OMK10msg;
068000120704           ErrMessage  = *off;
068100120704           ErrGenerico = *off;
068200120704         ENDIF;
068300120704
068400120704       ENDSR;
068500100616
068600100616       //--------------------------------------------------------------
068700100616       //?F10 - Nuova attività.
068800100616       //--------------------------------------------------------------
068900100617       BEGSR  sr_F10;
069000100616
069100100809       //?Esco dal programma (arrivo da MK18 - MK01 - MK02 oppure da
069200100809       //?TA87 - TAI1 - TAI2)
069300100809       //?e il chiamante va diretto su MK19 nuova attività o TA88 nuova
069400100809       //?trattativa
069500100913         O21Fxx = '10';
069600100913         $Fine = *on;
069700100616
069800100616       ENDSR;
069900090609
070000100809       //--------------------------------------------------------------
070100100809       //?F22 - Primo contatto.
070200100809       //--------------------------------------------------------------
070300100809       BEGSR  sr_F22;
070400100809
070500100809         clear trmk17ds;
070600100809         IMK17patt = 'S';
070700100809         IMK17att  = 'T';
070800100809         IMK17pcau = 'S';
070900100809         IF  I21ksc = 0;
071000100809           IMK17cau  = '19';
071100100809         ENDIF;
071200100809         IF  I21ksc > 0;
071300100809           IMK17cau  = '20';
071400100809         ENDIF;
071500100809         IMK17cpo  = I21cpo;
071600100809         IMK17ksc  = I21ksc;
071700100809         IF w_IMK19com <> 0;
071800100809           IMK17co3  = w_IMK19com;
071900100809         ENDIF;
072000100809
072100100809         trmk17r (kpjba : TRMK17DS);
072200100810
072300100810       //?Imposto che devo ricaricare il subfile
072400100810         $InzS01 = *on;
072500100810       //?Chiudo cursore
072600100810         exsr sr_CloseCursor;
072700100810       //?Spengo indicatore di fine file
072800100810         $EoF = *off;
072900100809
073000100809       ENDSR;
073100100809
073200090609       //--------------------------------------------------------------
073300090609       //?Caricamento singola pagina nel subfile S01                   ?
073400090609       //--------------------------------------------------------------
073500090609       BEGSR  sr_RollUpS01;
073600090609
073700090609         S01nrr    = (W_SflPag * c_SflPag);
073800090609         W_SflPag += 1;
073900090609         SflNxtChg = *off;
074000090609
074100090609         //?Ciclo di caricamento dati da TIATC00F?
074200090609         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
074300090609              S01nrr    < c_MaxRec                and
074400090610             (Not $EoF   and   Not $Err);
074500090609
074600090617           clear  MK21S01;
074700090617
074800090609           // -?Impostazione attributi di visualizzazione?
074900090617           if  ATCdco > *zero;
075000090617             §DAdco = c_DspAtrNorm;
075100090609           else;
075200090617             §DAdco = c_DspAtrRed;
075300090609           endif;
075400090609
075500090609           // -?Caricamento del singolo record nel subfile S01?
075600090610           // - - Campi Hidden
075700090622           S1Hcpo  = ATCcpo;
075800090622           S1Hksc  = ATCksc;
075900090622           S1Hdco  = ATCdco;
076000090610           S1Htat  = ATCtat;
076100090610           S1Hatn  = ATCatn;
076200090610           S1Hatnp = ATCatnp;
076300090610           S1Hnrv  = ATCnrv;
076400090622           if  ATCdco > *zero;
076500090622             S1Hcco = ATCcco;
076600090622           else;
076700090622             S1Hcco = ATCcmm;
076800090622           endif;
076900090610           // - - Campi di Output
077000090609           if  ATCdco > *zero;
077100090610             wDate_dmy = %date( %editw( ATCdco : '    -  -  ' ) : *iso );
077200090609           else;
077300090610             wDate_dmy = %date( %editw( ATCdad : '    -  -  ' ) : *iso );
077400090609           endif;
077500090610           S1Cdco = %dec( wDate_dmy );
077600090609           if  ATCdco > *zero;
077700090609             S1Chco = ATChco / 100;
077800090609           else;
077900090609             S1Chco = ATChda / 100;
078000090609           endif;
078100090609           S1Ctat = ATCtat;
078200090610           if  ATCcac <> *blank;
078300090610             S1Ccac = ATCcac;
078400090610           else;
078500090610             S1Ccac = ATCcad;
078600090610           endif;
078700090610           select;
078800090610             when  I21ksc > *zero;
078900091022               S1Ccpo = %editc( ATCcpo : 'X' );
079000090610               if  ATCnra > *zero;
079100091022                 S1Cnrv = %editc( ATCnra : 'Z' );
079200090610               else;
079300091022                 S1Cnrv = %editc( ATCnrv : 'Z' );
079400090610               endif;
079500090610             when  I21cpo > *zero;
079600091022               if  ATCksc > *zero;
079700091022                 S1Ccpo = %editc( ATCksc : 'X' );
079800091022               endif;
079900090610               if  ATCnra > *zero;
080000091022                 S1Cnrv = %editc( ATCnra : 'Z' );
080100090610               else;
080200091022                 S1Cnrv = %editc( ATCnrv : 'Z' );
080300090610               endif;
080400090610             when  I21ntc > *zero;
080500090610               if  ATCksc > *zero;
080600090610                 S1Ccpo = %editc( ATCksc : 'X' );
080700090610               else;
080800090610                 S1Ccpo = %editc( ATCcpo : 'X' );
080900090610               endif;
081000090610           endsl;
081100090610
081200090610           // -?Decodifiche?
081300090610           // --> Commerciale
081400130917           chain (S1Hcco) AZCMM000;
081500130917           if  NOT %found(AZCMM01L);
081600130917             CMMdes = %editc(S1Hcco : 'X') + ' ? ? ? ?';
081700090610           endif;
081800130917           S1Dcco  = CMMdes;
081900130917           S1HccoD = CMMdes;
082000090610           // --> Causale contatto
082100090610           clear  dCCO;
082200090610           clear  TIBS02ds;
082300090610           T02mod = 'C';
082400090610           T02cod = 'CCO';
082500090622           T02ke1 = S1Ccac;
082600090610           T02sif = knsif;
082700090610           TNTBE_RicercaControllo ( kpjba : tibs02ds );
082800090610           if  T02err = *blank;
082900090611             dCCO    = T02uni;
083000090610           else;
083100090610             §CCOdes = *all'? ';
083200090610           endif;
083300090610           S1Dcac = §CCOdes;
083400100617         //?valorizzo il campo delle opzioni inibite per CAU attività
083500100617           S1Hopz = §CCOopz;
083600090609
083700090609           // -?Scrittura record nel subfile S01?
083800090609           S01nrr += 1;
083900090609           write  MK21S01;
084000090609
084100090609           // -?Lettura record successivo?
084200090609           exsr  sr_ReadCursor;
084300090609
084400090609         EndDo;
084500090609
084600090609         //?Visualizzazione del SFL (se ci sono dati)?
084700090609         SflDsp_N = (S01nrr <= *zero);
084800090609
084900090609         //?Attivazione del SFLEND?
085000090610         SflEnd   = (S01nrr >= c_MaxRec   or   $EoF   or   $Err);
085100090609
085200090609         //?Posizionamento cursore al 1° rcd della pagina?
085300090609         if  S01nrr > *zero;
085400090609           wPag     = S01nrr / c_SflPag;
085500090609           wRig     = S01nrr - (c_SflPag * wPag);
085600090609           C1RcdNbr = wPag * c_SflPag;
085700090609           if  wRig > *zero;
085800090609             C1RcdNbr += 1;
085900090609           else;
086000090609             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
086100090609           endif;
086200090609         else;
086300090609           clear  C1RcdNbr;
086400090609         endif;
086500090609
086600090609         C1CsrRrn = C1RcdNbr;
086700090609
086800090609       ENDSR;
086900090609
087000090609       //--------------------------------------------------------------
087100090609       //?Gestione opzioni del subfile S01                             ?
087200090609       //--------------------------------------------------------------
087300090609       BEGSR  sr_OpzS01;
087400090609
087500090609         clear  SavS01csr;
087600100913         clear  wEsci;
087700090609
087800090609         //?Ciclo di lettura subfile?
087900090609         readc  MK21S01;
088000090609
088100090609         DoW  Not %eof(TRMK21D);
088200090609
088300090609           SflNxtChg = *off;
088400090609           %subst(IndDspF : 50) = *off;
088500090609           C1RcdNbr = S01nrr;
088600090609
088700090609           select;
088800100617
088900100617             //?Controllo l'opzione inserita
089000100617             WHEN  S1Copz <> *blank and S1Hopz <> *blanks and
089100100617                   %scan(S1Copz:S1Hopz) > 0 ;
089200100617                   errMessage  = *on;
089300100617                   errGenerico = *on;
089400100617                   PosCurOpz   = *on;
089500100617                   V1Dmsg = $Msg(04);
089600100616
089700100617             //?G-Gestione
089800100616             when  S1Copz = 'G' and VisGestione;
089900100616               exsr sr_Gestione;
090000100913               IF  wEsci = *blanks;
090100100913                 wEsci = 'S';
090200100913               ENDIF;
090300100809
090400100809             //?2-Modifica
090500100809             when  S1Copz = '2' and VisGestione;
090600100809               exsr sr_Modifica;
090700100913               wEsci = 'N';
090800090609
090900090609             // -?5=Visualizzazione?
091000100928             when  S1Copz = '5' and not VisDaMk19;
091100090611               exsr sr_CallTRMK19;
091200100913               wEsci = 'N';
091300090609
091400090609             // -?8=Note?
091500090609             when  S1Copz = '8';
091600090609               exsr sr_CallTRMK20;
091700100913               wEsci = 'N';
091800090609
091900090609             // -?Nessuna opzione?
092000090609             when  S1Copz = *blank;
092100100913               wEsci = 'N';
092200090609
092300090609             // -?Opzione errata?
092400090609             other;
092500090609               ErrMessage  = *on;
092600090609               ErrGenerico = *on;
092700090609               PosCurOPZ   = *on;
092800090609               V1Dmsg = $Msg(01);
092900090609
093000090609           endsl;
093100090609
093200090609           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
093300090609           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
093400090609           if  S1Copz  <> *blank   and
093500090609              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
093600090609             SavS01csr   = C1RcdNbr;
093700090609           endif;
093800090609
093900090609           // -?Aggiornamento sfl?
094000090609           select;
094100090609             when  ErrMessage;
094200090609               SflNxtChg = *on;
094300090609               C1CsrRrn  = C1RcdNbr;
094400090609             when  ErrGenerico;
094500090609               C1CsrRrn  = C1RcdNbr;
094600090609               clear  S1Copz;
094700090609             other;
094800090609               clear  S1Copz;
094900090609           endsl;
095000090609           UPDATE  MK21S01;
095100090609           if  ErrGenerico   or   ErrMessage;
095200090609             leave;
095300090609           endif;
095400090609
095500090609           readc  MK21S01;
095600090609
095700090609         ENDDO;
095800090609
095900090609         // -?Riposiziono il cursore sul record contenente la prima opz.?
096000090609         //  ?(se non sono stati rilevati errori)?
096100090609         if  NOT ErrMessage   and   NOT ErrGenerico   and
096200090609             SavS01csr > *zero;
096300090609           C1CsrRrn = SavS01csr;
096400090609         endif;
096500090609
096600090609       ENDSR;
096700100616
096800100616       //--------------------------------------------------------------
096900100616       //?Richiamo gestione attività.
097000100616       //--------------------------------------------------------------
097100100616       BEGSR  sr_Gestione;
097200100616
097300100617       //?Come prima cosa controllo se che sia un'attività da eseguire
097400100616         IF  S1Hdco <> 0;
097500100616           ErrMessage  = *on;
097600100616           ErrGenerico = *on;
097700100616           PosCurOPZ   = *on;
097800100616           V1Dmsg = $Msg(02);
097900100616           leavesr;
098000100616         ENDIF;
098100100616
098200100617       //?Poi controllo se posso gestire l'attività
098300100616         exsr CtrAbiAtt;
098400100616         IF  ErrMessage;
098500100616           leavesr;
098600100616         ENDIF;
098700100617
098800100617       //?Imposto la DS buona dalla DS fasulla di richiamo
098900100617         TRMK19DS = TRMK19DS1;
099000100616
099100100616       //?A questo punto vado in gestione dell'attività
099200100617       //?se attività legata a trattativa richiamo gestione trattativa
099300100616         IF  S1Hnrv > 0;
099400100616           clear tnta88ds ;
099500100616           ITA88cmt = 'N';
099600100616           ITA88fle = 'G' ;
099700100616           ITA88nrv = S1Hnrv ;
099800100616           IF IMK19com <> 0;
099900100616             ITA88com  = IMK19com;
100000100616             ITA88comd = IMK19comd;
100100100616           ENDIF;
100200100616           Tnta88r ( kpjba : tnta88ds );
100300100910         //?ritorno il flag "99" in modo da NON richiamare di nuovo il TNTA88R dal chiamante
100400100617         //?non ho possibilità di controllare se F3 (F12 è attivo solo se nuova trattativa)
100500100617           O21fxx = '99';
100600100616         ENDIF;
100700100617       //?se attività NON legata a trattativa richiamo gestione attività
100800100616         IF  S1Hnrv = 0;
100900101014           IMK19cmt = 'S';
101000100616           IMK19fle = 'G';
101100130131           IMK19tco = S1Htat;
101200100616           IMK19tat = S1Htat;
101300100616           IMK19atn = S1Hatn;
101400100616           IMK19atnp= S1Hatnp;
101500100930           // Trmk19c (kpjba : trmk19ds);
101600100930           Trmk19r (kpjba : trmk19ds);
101700100617         //?ritorno il flag "99" in modo da NON richiamare di nuovo il TRMK19R dal chiamante
101800100617         //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
101900100617           IF  OMK19err = *blanks;
102000100617             O21fxx = '99';
102100100617           ENDIF;
102200100616         ENDIF;
102300100617
102400100617       //?Imposto che devo ricaricare il subfile
102500100617         $InzS01 = *on;
102600100617       //?Chiudo cursore
102700100617         exsr sr_CloseCursor;
102800100617       //?Spengo indicatore di fine file
102900100617         $EoF = *off;
103000100616
103100100616       ENDSR;
103200100809
103300100809       //--------------------------------------------------------------
103400100809       //?Richiamo Modifica attività.
103500100809       //--------------------------------------------------------------
103600100809       BEGSR  sr_Modifica;
103700100809
103800100809       //?Come prima cosa controllo se che sia un'attività da eseguire
103900100809         IF  S1Hdco <> 0;
104000100809           ErrMessage  = *on;
104100100809           ErrGenerico = *on;
104200100809           PosCurOPZ   = *on;
104300100809           V1Dmsg = $Msg(02);
104400100809           leavesr;
104500100809         ENDIF;
104600100809
104700100809       //?Poi controllo se posso gestire l'attività
104800100809         exsr CtrAbiAtt;
104900100809         IF  ErrMessage;
105000100809           leavesr;
105100100809         ENDIF;
105200100809
105300100809       //?Imposto la DS buona dalla DS fasulla di richiamo
105400100809         TRMK19DS = TRMK19DS1;
105500100809
105600100809       //?A questo punto vado in gestione dell'attività
105700100913       //?sempre
105800101014         IMK19cmt = 'S';
105900100913         IMK19fle = '2';
106000130131         IMK19tco = S1Htat;
106100100913         IMK19tat = S1Htat;
106200100913         IMK19atn = S1Hatn;
106300100913         IMK19atnp= S1Hatnp;
106400100930        // Trmk19c (kpjba : trmk19ds);
106500100930         Trmk19r (kpjba : trmk19ds);
106600100913       //?ritorno il flag "99" in modo da NON richiamare di nuovo il TRMK19R dal chiamante
106700100913       //?se non torna errore  (in questo caso errore vale per F12 da TRMK19R)
106800100913         IF  OMK19err = *blanks;
106900100913           O21fxx = '99';
107000100809         ENDIF;
107100100809
107200100809       //?Imposto che devo ricaricare il subfile
107300100809         $InzS01 = *on;
107400100809       //?Chiudo cursore
107500100809         exsr sr_CloseCursor;
107600100809       //?Spengo indicatore di fine file
107700100809         $EoF = *off;
107800100809
107900100809       ENDSR;
108000090609
108100090609       //--------------------------------------------------------------
108200090609       //?Richiamo pgm di visualizzazione dati contatto                ?
108300090609       //--------------------------------------------------------------
108400090611       BEGSR  sr_CallTRMK19;
108500090609
108600100616         clear  TRMK19ds;
108700100616         IMK19cmt  = 'N';
108800090611         IMK19fle  = '5';
108900091105         IMK19fpr  = 'P';
109000090618         IMK19tco  = S1Htat;
109100090622         IMK19cpo  = S1Hcpo;
109200090622         if  S1Hdco > *zero;
109300090611           IMK19fatt = 'S';
109400090611         endif;
109500090611         IMK19com  = S1Hcco;
109600090611         IMK19comd = S1HccoD;
109700090611         IMK19tat  = S1Htat;
109800090611         IMK19atn  = S1Hatn;
109900090611         IMK19atnp = S1Hatnp;
110000090611
110100100928         trmk19r ( KPJBA : TRMK19ds );
110200090611
110300090611         if  OMK19err = 'E';
110400090611           V1Dmsg  = OMK19msg;
110500090611           ErrMessage  = *on;
110600090611           ErrGenerico = *on;
110700090611         endif;
110800090609
110900090609       ENDSR;
111000090609
111100090609       //--------------------------------------------------------------
111200090609       //?Richiamo pgm di visualizzazione note contatto                ?
111300090609       //--------------------------------------------------------------
111400090609       BEGSR  sr_CallTRMK20;
111500090609
111600090611         reset  TRMK20ds;
111700090622         IMK20cpo  = S1Hcpo;
111800090622         IMK20ksc  = S1Hksc;
111900090609         IMK20tat  = S1Htat;
112000090609         IMK20atn  = S1Hatn;
112100090609         IMK20atnp = S1Hatnp;
112200090609         IMK20nrv  = S1Hnrv;
112300090609
112400090609         trmk20r ( KPJBA : TRMK20ds );
112500090609
112600090609         if  OMK20fxx  = '03';
112700090609           O21fxx = '03';
112800090609           $Fine  = *on;
112900090609           ErrGenerico = *on;
113000090609         endif;
113100090611
113200090611         if  OMK19err = 'E';
113300090611           V1Dmsg  = OMK20msg;
113400090611           ErrMessage  = *on;
113500090611           ErrGenerico = *on;
113600090611         endif;
113700090609
113800090609       ENDSR;
113900100616
114000100616       //--------------------------------------------------------------
114100100616       //?Controllo se utente abilitato all'attività.
114200100616       //--------------------------------------------------------------
114300100616       BEGSR CtrAbiAtt;
114400100616
114500100616       //?abilitato come commerciale
114600100616         clear TNTAA1DS;
114700100616         ITAA1caut = '§utegtc';
114800100616         ITAA1cmm  = S1Hcco;
114900100616         kpjbu     = TNTAA1DS;
115000100616         tntaa1c (kpjba);
115100100616         TNTAA1DS = kpjbu;
115200100616         IF  OTAA1fabi <> 'N';
115300100616           leavesr;
115400100616         ENDIF;
115500100616
115600100616       //?abilitato come cliente
115700100616         IF  S1Hksc > 0;
115800100616           clear TNTAA1DS;
115900100616           ITAA1caut = '§utegtc';
116000100616           ITAA1ksc  = %dec(S1Hksc:7:0);
116100100616           kpjbu     = tntaa1ds;
116200100616           tntaa1c (kpjba);
116300100616           TNTAA1DS = kpjbu;
116400100616           IF  OTAA1fabi <> 'N';
116500100616             leavesr;
116600100616           ENDIF;
116700100616         ENDIF;
116800100616
116900100616       //?se arrivo a questo punto vuol dire che l'utente non è abilitato
117000100616       //?al commerciale o al cliente
117100100616         ErrMessage  = *on;
117200100616         ErrGenerico = *on;
117300100616         PosCurOPZ   = *on;
117400100616         V1Dmsg = $Msg(03);
117500100616
117600100616       ENDSR;
117700090609
117800090609       //--------------------------------------------------------------
117900090609       //?Stampa segnalazione dell'errore rilevato via SQL             ?
118000090609       //--------------------------------------------------------------
118100090609       BEGSR  sr_PrintErrSQL;
118200090609
118300090609         //?Stampa del Dump?
118400090609         Dump(A);
118500090609
118600090609         //?Stampa del Job-Log?
118700090609         Qcmd = 'DSPJOBLOG job(*) output(*print)';
118800090609         exsr  sr_ExecCmd;
118900090609
119000090609         //?Chiusura del programma?
119100090609         exsr  sr_RoutEnd;
119200090609
119300090609       ENDSR;
119400090609
119500090609       //--------------------------------------------------------------
119600090609       //?Esecuzione del comando (già impostato).                      ?
119700090609       //--------------------------------------------------------------
119800090609       BEGSR  sr_ExecCmd;
119900090609
120000090609         clear Qcap0100;
120100090609         Qcabcsdh = *off;
120200090609         Qcapa    = *off;
120300090609         Qcacmdss = *off;
120400090609         Qcaerved = *allX'00';
120500090609
120600090609         clear Qusec;
120700090609         Qusbprv  = %size(Qusec);
120800090609
120900090609         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
121000090609                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
121100090609                           0 : 0 : Qusec);
121200090609
121300090609         if  Qusei <> *blank;
121400090609           exsr  sr_MsgErr;
121500090609         endif;
121600090609
121700090609       ENDSR;
121800090609
121900090609       //--------------------------------------------------------------
122000090609       //?Invio messaggio per segnalazione errori.                     ?
122100090609       //--------------------------------------------------------------
122200090609       BEGSR  sr_MsgErr;
122300090609
122400090609
122500090609       ENDSR;
122600090609
122700090609       //--------------------------------------------------------------
122800090609       //?Operazioni finali.                                           ?
122900090609       //--------------------------------------------------------------
123000090609       BEGSR  sr_RoutEnd;
123100090609
123200090609         //?Chiusura cursore SQL?
123300090609         exsr  sr_CloseCursor;
123400090609
123500090609         //?Chiusura pgm?
123600090610         kpjbu = TRMK21ds;
123700090609         return;
123800090609
123900090609       ENDSR;
124000090609
124100090609      /end-free
124200090609
124300090609       //--------------------------------------------------------------
124400090609       //?Schiere a tempo di compilazione.                             ?
124500090609       //--------------------------------------------------------------
124600090609
124700090609** - $Msg -------------------------------------------------------------------*
124800090609Opzione errata                                                                 1
124900100616Opzione errata Attività già eseguita                                           2
125000100616Opzione errata Utente non abilitato alla gestione dell'attività                3
125100100617Opzione non valida per questa attività                                         4
