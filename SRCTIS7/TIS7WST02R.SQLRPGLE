000100111128       //==============================================================
000200120927       // TIS7WST02R - Scrittura WF
000300111128       //==============================================================
000400111128
000500120209
000600111128       //--------------------------------------------------------------
000700120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800111128       //--------------------------------------------------------------
000900111128
001000111128     /*PRM  dbgview(*source)
001100111128     /*END
001200111128
001300111128       //--------------------------------------------------------------
001400120207       // Specifiche di controllo.                                     ?
001500111128       //--------------------------------------------------------------
001600111128
001700111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800111128     h dftactgrp(*no)
001900111128     h alwnull(*inputonly)
002000111128
002100111128       //--------------------------------------------------------------
002200120207       // Dichiarazione file.                                          ?
002300111128       //--------------------------------------------------------------
002400111128
002500120928     fTITA430C  If   e           k disk
002600121019     fWFWST03l  IF A e           k disk
002700111128
002800111128       //--------------------------------------------------------------
002900120207       // Definizione costanti.                                        ?
003000111128       //--------------------------------------------------------------
003100120207
003200111128
003300111128
003400111128       //--------------------------------------------------------------
003500120207       // Definizione schiere.                                         ?
003600111128       //--------------------------------------------------------------
003700111128
003800121016     D KPJBA         E DS
003900121016     D TIS7F0DSI     E DS
004000121002     D                                     INZ
004100121002     D TIS7F0DSO     E DS
004200121002     D                                     INZ
004300121002     D  Col                          15
004400121002     D                                     DIM(25)
004500121002     D                                     OVERLAY(IdColloOF0)
004600121002     D  Chi                          35
004700121002     D                                     DIM(%ELEM(Col))
004800121002     D                                     OVERLAY(ChiSonoOF0)
004900111128
005000111128       //--------------------------------------------------------------
005100120207       // Definizione aree dati.                                       ?
005200111128       //--------------------------------------------------------------
005300111128
005400120928     d dta4a         e ds                  inz
005500111128
005600111128       //--------------------------------------------------------------
005700120207       // Definizione strutture dati.                                  ?
005800111128       //--------------------------------------------------------------
005900111128
006000121003     d TITAS00F      e ds
006100121016
006200121016      // - Ricerca unificante padre
006300121016     d TIBS10ds      e ds                  inz
006400121016
006500121003     D*------------------
006600121003     D* DS X PASSAGGIO PARAMETRI
006700121003     D*------------------
006800121003     D PARAM           DS                  INZ
006900121003     d    wNrChia                     7s 0
007000121003     d    wDAD                        8s 0
007100121003     d    wDAA                        8s 0
007200121003     d    wCCM                        7s 0
007300121016     d    wSU                         1
007400121003     d    wVerTrk                     1
007500121003     d    wChkXml                     1
007600121003     d    wTipExe                     4
007700121003     d    wRmn                       15s 0
007800121003     d    wRma                       15
007900121003     d    wAAS                        4s 0
008000121003     d    wLNP                        3s 0
008100121003     d    wNRS                        2s 0
008200121003     d    wNSP                        7s 0
008300121019     d    wIdJob                     50    varying
008400121029     d    wNRS1                       2s 0
008500120927
008600111128       //--------------------------------------------------------------
008700120207       // Definizione variabili globali.                               ?
008800111128       //--------------------------------------------------------------
008900111128
009000120208       // - Stringa SQL da eseguire
009100120928     d wSQl            s          11264    inz  varying
009200111128
009300120928       // - Parametri SQL
009400120207
009500120207       // - InfDS
009600120207
009700120207       // - Indicatori su DspF
009800120207
009900120207       // - Flags booleani
010000120207     d $Fine           s               n   inz(*off)
010100120207
010200120207       // - Campi associati al video
010300120207
010400120207       // - Variabili di appoggio
010500120928     d wX              s              8s 0
010600121003     d SKCDS           ds          5500
010700121003     d  wwSKC                        11  0 dim(500)
010800121001     d wwNrChia        s              7s 0
010900121016     D*
011000121016     d    wSKC         s           5500
011100121016     d nrSKC           s              4s 0
011200121019     d wId             s                   like(WFWId)
011300111128
011400121002       // variabili per l'uso del driver TIS7F000R
011500121002     D Operazione      S              1
011600121002     D*                                    NuovaRichiesta
011700121002     D                                     INZ('1')
011800121002     D RigheOut        S              3  0
011900121002     D Esito           S              1
012000121203     D i               S              5I 0
012100121002
012200111128       //--------------------------------------------------------------
012300120210       // Definizione procedure usate
012400111128       //--------------------------------------------------------------
012500120704
012600120927     d TIS7WST02R      pr
012700121016     d  Arch                               likeds(kpjba)
012800120704
012900120927     d TIS7WST02R      pi
013000121016     d  Arch                               likeds(kpjba)
013100121003
013200121016      /COPY GAITRASRC/SRCPROTOPR,tibs10r
013300111128
013400111128       //--------------------------------------------------------------
013500120208       // Definizione key-list.                                        ?
013600111128       //--------------------------------------------------------------
013700111128
013800120928     d k05TITA430    e ds                  extname(TITA430C : *key)  inz
013900111128
014000111128       //--------------------------------------------------------------
014100120208       // M A I N - L I N E                                            ?
014200111128       //--------------------------------------------------------------
014300111128
014400111128      /free
014500111128
014600120208       // - Operazioni iniziali
014700111128       exsr  sr_RoutInz;
014800111128
014900120928       // - Esecuzione SQL (e scrittura WF)
015000120927       exsr  eseguiSQL;
015100120207
015200120208       // - Operazioni finali
015300111128       exsr  sr_RoutEnd;
015400111128
015500111128       //--------------------------------------------------------------
015600120208       // Operazioni iniziali
015700111128       //--------------------------------------------------------------
015800111128       BEGSR sr_RoutInz;
015900111128
016000111128         *inLR = *on;
016100121016         kpjba    = Arch;
016200121016         param    = kpjbu;
016300121016
016400121019         // recupero l'ultimo ID usato
016500121019         setll *start WFWST03L;
016600121019         read WFWST000;
016700121019         if not %eof(WFWST03L);
016800121019           wId = WFWId;
016900121019           clear WFWST000;
017000121019         else;
017100121019           wId = 0;
017200121019         endif;
017300121019         WFWId = wId;
017400121019
017500121019         // se passato cod. cliente unificante recupero i figli
017600121016         if wSU = 'U';
017700121016           exsr RepFigli;
017800121016           // li metto nella schiera di appoggio
017900121016           SKCDS    = wSKC    ;
018000121016           // calcolo quanti sono effettivamente gli elementi estratti nella schiera
018100121016           nrSKC = %lookup (0 : wwSKC);
018200121016         // se passato cod. cliente singolo, valorizzo con quello la schiera
018300121016         else;
018400121016           wwSKC(1) = wCCM;
018500121016           // il nr. elementi nella schiera è 1
018600121016           nrSKC = 1;
018700121016         endif;
018800120927
018900120208         // - Impostazione opzioni per SQL
019000111128         exec SQL   set option   DynUsrPrf = *owner,
019100111128                                 CloSqlCsr = *endmod;
019200111128
019300111128       ENDSR;
019400111128
019500111128       //--------------------------------------------------------------
019600121016       // Reperisco figli di cod.cliente unificante
019700111128       //--------------------------------------------------------------
019800121016       BEGSR  RepFigli;
019900111128
020000121016         CLEAR TIBS10DS;
020100121016         D10DRF = %dec(%Date() : *ISO);              //data rif. = oggi
020200121016         D10TLE = 'WW';                              //tipo legame
020300121016         D10PAF = 'F';                               //reperisco i Figli
020400121016         D10COD = wCCM;
020500121016         TIBS10R(TIBS10DS);
020600121016         // già controllato nel chiamante che il cod.cliente sia PADRE (if inutile ma non si sa mai)
0207001210164        IF D10ERR = *BLANKS;
020800121016           wSKC = D10SKC;
0209001210163e       ELSE;
021000121016           return;
0211001210163e       ENDIF;
021200121016
021300121016       ENDSR;
021400120928
021500120928       //--------------------------------------------------------------
021600120928       // Apertura cursore C1
021700120928       //--------------------------------------------------------------
021800120928       BEGSR  sr_OpenCursor;
021900120928
022000120208         // - Dichiarazione del cursore
022100111128         exec sql  prepare S1  from :wSQL;
022200111128         exec sql  declare C1  cursor for S1;
022300111128
022400120208         // - Apertura del cursore
022500111128         exec sql   open C1;
022600111128
022700111128       ENDSR;
022800111128
022900111128       //--------------------------------------------------------------
023000120208       // Chiusura cursore C1
023100111128       //--------------------------------------------------------------
023200111128       BEGSR  sr_CloseCursor;
023300111128
023400120208         // - Chiusura del cursore
023500111128         exec sql   close C1;
023600111128
023700111128       ENDSR;
023800120208
023900120208       //--------------------------------------------------------------
024000120927       // Reperisco i dati da scrivere
024100120208       //--------------------------------------------------------------
024200120928       BEGSR  eseguiSQL;
024300120208
024400120927         // - Reperisci dati
024500120208
024600120927         //reperisco dati TITAS00F/10F
024700120210         //---------------------
024800120928         clear TITAS00F;
024900121016         wwNrChia = 1;
025000120927
025100121016         // ciclo per ogni elemento della schiera dei figli,
025200121016         //  (se all'interno del ciclo raggiungo il nr. chiamate richiesto, esco)
025300121016         for wX = 1 to nrSKC;
025400121016
025500121016           // - Creazione stringa SQL da eseguire
025600121016           exsr CreaStringaSQL;
025700121016
025800121016           // - Apertura cursore
025900121016           exsr  sr_OpenCursor;
026000121016
026100121016           // devo fare fetch SINGOLA E NON MULTIPLA
026200121016
026300121016           // ciclo finché non sono a fine blocco di rcd, oppure non ho un errore bloccante,
026400121016           //  oppure non ho raggiunto il nr. chiamate richiesto
026500121016           DoU  SQLCode = 100
026600121016            or SQLCode < 0
026700121016            or wNrChia < wwNrChia;
026800121016             exec sql  fetch next  from C1  into :TITAS00F;
026900120927
027000121016             select;
027100121016             // errore
027200121016             when SQLCode < 0;
027300121016               // non faccio nulla, il cursore si chiude a fine ciclo
027400121016             // nessuna lettura
027500121016             when SQLCode = 100;
027600121016               // non faccio nulla (per quel cliente non ci sono rcd nel WF)
027700121016             // lettura
027800121016             when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
027900121016               // scrivo il WF
028000121016               exsr ScriviWFWST;
028100121016               // incremento il numero di rcd TITAS estratti
028200121016               wwNrChia = wwNrChia + 1;
028300121016             // fine lettura
028400121016             other;
028500121016               // non faccio nulla
028600121016             endsl;
028700121016           EndDo;
028800121016           // ogni volta che finisco l'elaborazione per un elemento di schiera, chiudo il cursore
028900121016           exsr  sr_CloseCursor;
029000121016
029100121016           // se ho raggiunto il nr. max chiamate, esco
029200121016           if wNrChia < wwNrChia;
029300121016             leave;
029400121016           endif;
029500121016
029600121016         EndFor;
029700120208
029800120208       ENDSR;
029900120215
030000120215       //--------------------------------------------------------------
030100120927       // Creo la stringa SQL
030200120215       //--------------------------------------------------------------
030300120927       BEGSR  CreaStringaSQL;
030400120215
030500121016         wSQL = *blank;
030600121016
030700120928         // SQL parte "da fatturare"
030800120928
030900120927         wSQL =
031000120927          'SELECT * FROM TITAS00F ' +
031100120927          'WHERE ' +
031200120927          'digits(TASAAS) concat digits(TASMGS) between ' +
031300121029          %char(wDAD) + ' and ' + %char(wDAA) +
031400121029          ' and TASTBL in (''F1'' , ''A2'')';
031500121016          // TASCCM e TASKSC sono in OR
031600121016          wSQL = %trim(wSQL) + ' and ( TASCCM = ' + %char(wwSKC(wX)) +
031700121016                 ' OR TASKSC = ' + %char(wwSKC(wX)) + ') ';
031800120927
031900120927         // se richiesto un singolo rif. mittente numerico
032000120927         if wRMN <> 0;
032100120928           wSQL = %trim(wSQL) + ' and TASRMN=' + %char(wRMN);
032200120928         endif;
032300120928
032400120928         // se richiesto una singola spedizione
032500120928         if wAAS <> 0;
032600120928           wSQL = %trim(wSQL) +
032700120928                  ' and TASAAS=' + %char(wAAS) +
032800120928                  ' and TASLNP=' + %char(wLNP) +
032900120928                  ' and TASNRS=' + %char(wNRS) +
033000120928                  ' and TASNSP=' + %char(wNSP);
033100120928         endif;
033200121029
033300121029         // se richiesto una singola serie
033400121029         if wNRS1 <> 0;
033500121029           wSQL = %trim(wSQL) +
033600121029                  ' and TASNRS=' + %char(wNRS1);
033700121029         endif;
033800120928
033900121002         // SQL parte "fatturate ultimi mesi"
034000120928
034100121002         wSQL = %trim(wSQL) +
034200121002          ' UNION SELECT * FROM TITAS10F ' +
034300120928          'WHERE ' +
034400120928          'digits(TASAAS) concat digits(TASMGS) between ' +
034500121029          %char(wDAD) + ' and ' + %char(wDAA) +
034600121029          ' and TASTBL in (''F1'' , ''A2'')';
034700121016          // TASCCM e TASKSC sono in OR
034800121016          wSQL = %trim(wSQL) + ' and ( TASCCM = ' + %char(wwSKC(wX)) +
034900121029                 ' OR TASKSC = ' + %char(wwSKC(wX)) + ') ';
035000120928
035100120928         // se richiesto un singolo rif. mittente numerico
035200120928         if wRMN <> 0;
035300120928           wSQL = %trim(wSQL) + ' and TASRMN=' + %char(wRMN);
035400120928         endif;
035500120928
035600120928         // se richiesto una singola spedizione
035700120928         if wAAS <> 0;
035800120928           wSQL = %trim(wSQL) +
035900120928                  ' and TASAAS=' + %char(wAAS) +
036000120928                  ' and TASLNP=' + %char(wLNP) +
036100120928                  ' and TASNRS=' + %char(wNRS) +
036200120928                  ' and TASNSP=' + %char(wNSP);
036300120928         endif;
036400121029
036500121029         // se richiesto una singola serie
036600121029         if wNRS1 <> 0;
036700121029           wSQL = %trim(wSQL) +
036800121029                  ' and TASNRS=' + %char(wNRS1);
036900121029         endif;
037000120927
037100120927       ENDSR;
037200120927
037300120927       //--------------------------------------------------------------
037400120928       // Scrivi WFWST00F
037500120927       //--------------------------------------------------------------
037600120928       BEGSR  scriviWFWST;
037700120928
037800120928         // per prima cosa calcolo l'RMA per controllare se = a quello richiesto (se richiesto)
037900120928         exsr calcoloRMA;
038000120928         if wRMA = *blank or
038100120928            wRMA <> *blank and WFWRMA = wRMA;
038200120928
038300120928           WFWAAS = TASAAS;
038400120928           WFWLNP = TASLNP;
038500120928           WFWNRS = TASNRS;
038600120928           WFWNSP = TASNSP;
038700121016           WFWKSU = '0' + %editC(wCCM : 'X');
038800121016           WFWCCM = wwSKC(wX);
038900120928           WFWDATSPED = %dec(%char(TASAAS) + %editc(TASMGS:'X') : 8 : 0);
039000120928           WFWIDSPED = %dec(%editc(TASLNP:'X') + %editc(TASNRS:'X')
039100120928                       + %editc(TASNSP:'X') : 12 : 0);
039200120928           WFWRMN = TASRMN;
039300121002           WFWPROGR = 0;
039400121002           WFWTIMEEXE = 0;
039500121002           WFWVERTRK = wVerTrk;
039600121002           WFWCHKXML = wChkXml;
039700121002           clear WFWESIXML;
039800121105           clear WFWTSINI ;
039900121105           clear WFWTSFIN ;
040000121105           WFWThread = -1;
040100121019           WFWESIEXE = 999;
040200121019           WFWIdJob = wIdJob;
040300121002
040400121002           // se richieste interrogazioni Disk B/C i rcd di WFWST00F vanno sviluppati per i dati di
040500121002           // dettaglio
040600121002           WFWTIPEXE = wTipExe;
040700121002           if WFWTIPEXE = 'DSKC' or WFWTipExe = 'DSKB';
040800121002             exsr SviluppoDisk;
040900121002           else;
041000121018             // ID
041100121018             WFWId = WFWId + 1;
041200121002             write WFWST000;
041300121002           endif;
041400120928
041500120928         endif;
041600120928
041700120928       ENDSR;
041800120928
041900120928       //--------------------------------------------------------------
042000120928       // Esecuzione del comando (già impostato)
042100120928       //--------------------------------------------------------------
042200120928       BEGSR  calcoloRMA;
042300121002
042400121002         clear dta4a;
042500121002         TA4AAS = TASAAS;
042600121002         TA4LNP = TASLNP;
042700121002         TA4NRS = TASNRS;
042800121002         TA4NSP = TASNSP;
042900121002         TA4TRC = 'A';
043000120928         chain  %kds(k05TITA430)  TITA4000;
043100120928         if %found(tita430c);
043200120928           dta4a = ta4not;
043300120928         endif;
043400120928         WFWRMA = %trim(§TA4aRMA);
043500120928
043600120928       ENDSR;
043700120928
043800111128       //--------------------------------------------------------------
043900120208       // Operazioni finali
044000111128       //--------------------------------------------------------------
044100111128       BEGSR  sr_RoutEnd;
044200111128
044300120208         // - Uscita
044400111128         return;
044500111128
044600111128       ENDSR;
044700111128
044800111128      /end-free
044900121002
045000121002     C     SviluppoDisk  BEGSR
045100121002
045200121002         // importo la DS con i dati di input
045300121002     C                   EVAL      AASIF0 = TASAAS
045400121002     C                   EVAL      LnPIF0 = TASLnP
045500121002     C                   EVAL      NrSIF0 = TASNrS
045600121002     C                   EVAL      NSpIF0 = TASNSp
045700121002     C                   EVAL      TBlIF0 = TASTBl
045800121002     C                   RESET                   Operazione
045900121002
046000121002         // Reperisco l'elenco dei colli.
046100121002     C                   DO        *HIVAL
046200121002
046300121002     C                   CLEAR                   WFWDSKC
046400121002     C                   CLEAR                   WFWDSKB
046500121002     C                   CLEAR                   TIS7F0DSO
046600121002
046700121002     C                   CALL(E)   'TIS7F000R'
046800121002     C                   PARM                    TIS7F0DSI
046900121002     C                   PARM                    Operazione
047000121002     C                   PARM                    TIS7F0DSO
047100121002     C                   PARM      *ZERO         RigheOut
047200121002     C                   PARM      *BLANK        Esito
047300121002
047400121002     C                   IF        Esito = '1' OR %ERROR
047500121002       // - Chiusura cursore
047600121002     C                   exsr      sr_CloseCursor
047700121002       // - Operazioni finali
047800121002     C                   exsr      sr_RoutEnd
047900121002     C                   ENDIF
048000121002
048100121002     C                   CLEAR                   i
048200121002
048300121203     C     1             DO        25
048400121002     C                   EVAL      i = i + 1
048500121002     C                   IF        (Col(i) = *BLANK AND Chi(i) = *BLANK)
048600121002     C                   LEAVE
048700121002     C                   ENDIF
048800121002     C                   IF        Col(i) <> *BLANK
048900121002     C                             and WFWTipExe = 'DSKB'
049000121002     C                   EVAL      WFWDSKB = Col(i)
049100121002     C                   ENDIF
049200121002     C                   IF        Chi(i) <> *BLANK
049300121002     C                             and WFWTipExe = 'DSKC'
049400121002     C                   EVAL      WFWDSKC = %TRIMR(Chi(i))
049500121002     C                   ENDIF
049600121018       // ID
049700121018     C                   EVAL      WFWId = WFWId + 1
049800121018
049900121018     C                   write     WFWST000
050000121002
050100121002     C                   ENDDO
050200121002
050300121002       // fine dei dati
050400121002     C                   IF        Operazione = '9'
050500121002     C                   LEAVE
050600121002     C                   ENDIF
050700121002
050800121002       // richiesta successiva
050900121002     C                   EVAL      Operazione = '2'
051000121002
051100121002     C                   ENDDO
051200121002
051300121002     C                   ENDSR
