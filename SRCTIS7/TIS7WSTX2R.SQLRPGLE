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
002600120928     fWFWST00F  O  A e             disk
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
008300120927
008400111128       //--------------------------------------------------------------
008500120207       // Definizione variabili globali.                               ?
008600111128       //--------------------------------------------------------------
008700111128
008800120208       // - Stringa SQL da eseguire
008900120928     d wSQl            s          11264    inz  varying
009000111128
009100120928       // - Parametri SQL
009200120928     d parteInSKC      s           5500    inz  varying
009300120207
009400120207       // - InfDS
009500120207
009600120207       // - Indicatori su DspF
009700120207
009800120207       // - Flags booleani
009900120207     d $Fine           s               n   inz(*off)
010000120207
010100120207       // - Campi associati al video
010200120207
010300120207       // - Variabili di appoggio
010400120928     d wX              s              8s 0
010500121003     d SKCDS           ds          5500
010600121003     d  wwSKC                        11  0 dim(500)
010700121001     d wwNrChia        s              7s 0
010800121016     D*
010900121016     d    wSKC         s           5500
011000111128
011100121002       // variabili per l'uso del driver TIS7F000R
011200121002     D Operazione      S              1
011300121002     D*                                    NuovaRichiesta
011400121002     D                                     INZ('1')
011500121002     D RigheOut        S              3  0
011600121002     D Esito           S              1
011700121002     D i               S              5I 0
011800121002
011900111128       //--------------------------------------------------------------
012000120210       // Definizione procedure usate
012100111128       //--------------------------------------------------------------
012200120704
012300120927     d TIS7WST02R      pr
012400121003     d  ParamDS                            likeds(param)
012500120704
012600120927     d TIS7WST02R      pi
012700121016     d  Arch                               likeds(kpjba)
012800121003
012900121016      /COPY GAITRASRC/SRCPROTOPR,tibs10r
013000111128
013100111128       //--------------------------------------------------------------
013200120208       // Definizione key-list.                                        ?
013300111128       //--------------------------------------------------------------
013400111128
013500120928     d k05TITA430    e ds                  extname(TITA430C : *key)  inz
013600111128
013700111128       //--------------------------------------------------------------
013800120208       // M A I N - L I N E                                            ?
013900111128       //--------------------------------------------------------------
014000111128
014100111128      /free
014200111128
014300120208       // - Operazioni iniziali
014400111128       exsr  sr_RoutInz;
014500111128
014600120928       // - Esecuzione SQL (e scrittura WF)
014700120927       exsr  eseguiSQL;
014800120207
014900120208       // - Operazioni finali
015000111128       exsr  sr_RoutEnd;
015100111128
015200111128       //--------------------------------------------------------------
015300120208       // Operazioni iniziali
015400111128       //--------------------------------------------------------------
015500111128       BEGSR sr_RoutInz;
015600111128
015700111128         *inLR = *on;
015800121016         kpjba    = Arch;
015900121016         param    = kpjbu;
016000121016
016100121016         // se passato cod. cliente unificante recupero i figli
016200121016         if wSU = 'U';
016300121016           exsr RepFigli;
016400121016         // se passato cod. cliente singolo, valorizzo con quello la schiera
016500121016         else;
016600121016           %subst(wSKC:1:11) = '0000' + %editc(wCCM:'X');
016700121016         endif;
016800121016
016900121003         SKCDS    = wSKC    ;
017000120927
017100120208         // - Impostazione opzioni per SQL
017200111128         exec SQL   set option   DynUsrPrf = *owner,
017300111128                                 CloSqlCsr = *endmod;
017400111128
017500120928         // creo parte SQL per "in" su KSC
017600120928         exsr creoParteIn;
017700111128
017800111128       ENDSR;
017900111128
018000111128       //--------------------------------------------------------------
018100121016       // Reperisco figli di cod.cliente unificante
018200111128       //--------------------------------------------------------------
018300121016       BEGSR  RepFigli;
018400111128
018500121016         CLEAR TIBS10DS;
018600121016         D10DRF = %dec(%Date() : *ISO);              //data rif. = oggi
018700121016         D10TLE = 'WW';                              //tipo legame
018800121016         D10PAF = 'F';                               //reperisco i Figli
018900121016         D10COD = wCCM;
019000121016         TIBS10R(TIBS10DS);
019100121016         // già controllato nel chiamante che il cod.cliente sia PADRE (if inutile ma non si sa mai)
0192001210164        IF D10ERR <> *BLANKS;
019300121016           wSKC = D10SKC;
0194001210163e       ELSE;
019500121016           return;
0196001210163e       ENDIF;
019700121016
019800121016       ENDSR;
019900121016
020000121016       //--------------------------------------------------------------
020100121016       // Creo parte SQL per "in" su KSC
020200121016       //--------------------------------------------------------------
020300121016       BEGSR  creoParteIn;
020400121016
020500120928         parteInSKC = '(';
020600120928         for wX = 1 to 500;
020700120928           // attacco il codice cliente che mi arriva di 11 cifre ma ne devo prendere solo 7
020800121001           if wwSKC(wX) <> *zeros;
020900121002             // metto la virgola se non è il primo valore
021000121002             if wX > 1;
021100121002               parteInSKC = %trim(parteInSKC) + ' ,';
021200121002             endif;
021300121002             // metto il nuovo codice
021400121002             parteInSKC = %trim(parteInSKC) + ' ' +
021500121002                          %subst(%editc(wwSKC(wX):'X') : 5 : 7);
021600121002           // se non ci sono altri codici esco dal ciclo
021700121002           else;
021800121002             leave;
021900121002           endif;
022000120928         endfor;
022100121002         parteInSKC = %trim(parteInSKC) + ')';
022200120928
022300120928       ENDSR;
022400120928
022500120928       //--------------------------------------------------------------
022600120928       // Apertura cursore C1
022700120928       //--------------------------------------------------------------
022800120928       BEGSR  sr_OpenCursor;
022900120928
023000120208         // - Dichiarazione del cursore
023100111128         exec sql  prepare S1  from :wSQL;
023200111128         exec sql  declare C1  cursor for S1;
023300111128
023400120208         // - Apertura del cursore
023500111128         exec sql   open C1;
023600111128
023700111128       ENDSR;
023800111128
023900111128       //--------------------------------------------------------------
024000120208       // Chiusura cursore C1
024100111128       //--------------------------------------------------------------
024200111128       BEGSR  sr_CloseCursor;
024300111128
024400120208         // - Chiusura del cursore
024500111128         exec sql   close C1;
024600111128
024700111128       ENDSR;
024800120208
024900120208       //--------------------------------------------------------------
025000120927       // Reperisco i dati da scrivere
025100120208       //--------------------------------------------------------------
025200120928       BEGSR  eseguiSQL;
025300120208
025400120927         // - Reperisci dati
025500120208
025600120927         //reperisco dati TITAS00F/10F
025700120210         //---------------------
025800120928         clear TITAS00F;
025900120208         // - Creazione stringa SQL da eseguire
026000120927         exsr CreaStringaSQL;
026100120927
026200120208         // - Apertura cursore
026300120208         exsr  sr_OpenCursor;
026400120927
026500120927         // devo fare fetch SINGOLA E NON MULTIPLA
026600120927
026700120927         // ciclo finché non sono a fine blocco di rcd, oppure non ho un errore bloccante,
026800120927         //  oppure non ho raggiunto il nr. chiamate richiesto
026900121001         wwNrChia = 1;
027000120927         DoU  SQLCode = 100
027100120927          or SQLCode < 0
027200121002          or wNrChia < wwNrChia;
027300120927           exec sql  fetch next  from C1  into :TITAS00F;
027400120927
027500120208           select;
027600120209           // errore
027700120209           when SQLCode < 0;
027800120927             // non faccio nulla, il cursore si chiude a fine ciclo
027900120209           // nessuna lettura
028000120927           when SQLCode = 100 and wwNrChia = 1;
028100120927             // non faccio nulla (il WF rimane vuoto)
028200120209           // lettura
028300120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
028400120927             // scrivo il WF
028500120927             exsr ScriviWFWST;
028600120927             // incremento il numero di rcd TITAS estratti
028700121002             wwNrChia = wwNrChia + 1;
028800120209           // fine lettura
028900120209           other;
029000120209             // non faccio nulla
029100120209           endsl;
029200120208         EndDo;
029300120208         // - Chiusura cursore
029400120208         exsr  sr_CloseCursor;
029500120208
029600120208       ENDSR;
029700120215
029800120215       //--------------------------------------------------------------
029900120927       // Creo la stringa SQL
030000120215       //--------------------------------------------------------------
030100120927       BEGSR  CreaStringaSQL;
030200120215
030300120928         // SQL parte "da fatturare"
030400120928
030500120927         wSQL =
030600120927          'SELECT * FROM TITAS00F ' +
030700120927          'WHERE ' +
030800120927          'digits(TASAAS) concat digits(TASMGS) between ' +
030900121002          %char(wDAD) + ' and ' + %char(wDAA);
031000121002          if wCCM > 0;
031100121002            wSQL = %trim(wSQL) + ' and TASCCM in ' + %trim(parteInSKC);
031200121002          else;
031300121002            wSQL = %trim(wSQL) + ' and TASKSC in ' + %trim(parteInSKC);
031400121002          endif;
031500120927
031600120927         // se richiesto un singolo rif. mittente numerico
031700120927         if wRMN <> 0;
031800120928           wSQL = %trim(wSQL) + ' and TASRMN=' + %char(wRMN);
031900120928         endif;
032000120928
032100120928         // se richiesto una singola spedizione
032200120928         if wAAS <> 0;
032300120928           wSQL = %trim(wSQL) +
032400120928                  ' and TASAAS=' + %char(wAAS) +
032500120928                  ' and TASLNP=' + %char(wLNP) +
032600120928                  ' and TASNRS=' + %char(wNRS) +
032700120928                  ' and TASNSP=' + %char(wNSP);
032800120928         endif;
032900120928
033000121002         // SQL parte "fatturate ultimi mesi"
033100120928
033200121002         wSQL = %trim(wSQL) +
033300121002          ' UNION SELECT * FROM TITAS10F ' +
033400120928          'WHERE ' +
033500120928          'digits(TASAAS) concat digits(TASMGS) between ' +
033600121002          %char(wDAD) + ' and ' + %char(wDAA);
033700121016          // TASCCM e TASKSC sono in OR
033800121016          wSQL = %trim(wSQL) + ' and ( TASCCM in ' + %trim(parteInSKC);
033900121002          else;
034000121002            wSQL = %trim(wSQL) + ' and TASKSC in ' + %trim(parteInSKC);
034100121002          endif;
034200120928
034300120928         // se richiesto un singolo rif. mittente numerico
034400120928         if wRMN <> 0;
034500120928           wSQL = %trim(wSQL) + ' and TASRMN=' + %char(wRMN);
034600120928         endif;
034700120928
034800120928         // se richiesto una singola spedizione
034900120928         if wAAS <> 0;
035000120928           wSQL = %trim(wSQL) +
035100120928                  ' and TASAAS=' + %char(wAAS) +
035200120928                  ' and TASLNP=' + %char(wLNP) +
035300120928                  ' and TASNRS=' + %char(wNRS) +
035400120928                  ' and TASNSP=' + %char(wNSP);
035500120928         endif;
035600120927
035700120927       ENDSR;
035800120927
035900120927       //--------------------------------------------------------------
036000120928       // Scrivi WFWST00F
036100120927       //--------------------------------------------------------------
036200120928       BEGSR  scriviWFWST;
036300120928
036400120928         // per prima cosa calcolo l'RMA per controllare se = a quello richiesto (se richiesto)
036500120928         exsr calcoloRMA;
036600120928         if wRMA = *blank or
036700120928            wRMA <> *blank and WFWRMA = wRMA;
036800120928
036900120928           WFWAAS = TASAAS;
037000120928           WFWLNP = TASLNP;
037100120928           WFWNRS = TASNRS;
037200120928           WFWNSP = TASNSP;
037300121016           WFWKSU = '0' + %editC(wCCM : 'X');
037400121002           if wCCM > 0;
037500121002             WFWCCM = TASCCM;
037600121002           else;
037700121002             WFWCCM = TASKSC;
037800121002           endif;
037900120928           WFWDATSPED = %dec(%char(TASAAS) + %editc(TASMGS:'X') : 8 : 0);
038000120928           WFWIDSPED = %dec(%editc(TASLNP:'X') + %editc(TASNRS:'X')
038100120928                       + %editc(TASNSP:'X') : 12 : 0);
038200120928           WFWRMN = TASRMN;
038300121002           WFWPROGR = 0;
038400121002           WFWTIMEEXE = 0;
038500121002           WFWVERTRK = wVerTrk;
038600121002           WFWCHKXML = wChkXml;
038700121002           clear WFWESIXML;
038800121002           clear WFWESIEXE;
038900121002
039000121002           // se richieste interrogazioni Disk B/C i rcd di WFWST00F vanno sviluppati per i dati di
039100121002           // dettaglio
039200121002           WFWTIPEXE = wTipExe;
039300121002           if WFWTIPEXE = 'DSKC' or WFWTipExe = 'DSKB';
039400121002             exsr SviluppoDisk;
039500121002           else;
039600121002             write WFWST000;
039700121002           endif;
039800120928
039900120928         endif;
040000120928
040100120928       ENDSR;
040200120928
040300120928       //--------------------------------------------------------------
040400120928       // Esecuzione del comando (già impostato)
040500120928       //--------------------------------------------------------------
040600120928       BEGSR  calcoloRMA;
040700121002
040800121002         clear dta4a;
040900121002         TA4AAS = TASAAS;
041000121002         TA4LNP = TASLNP;
041100121002         TA4NRS = TASNRS;
041200121002         TA4NSP = TASNSP;
041300121002         TA4TRC = 'A';
041400120928         chain  %kds(k05TITA430)  TITA4000;
041500120928         if %found(tita430c);
041600120928           dta4a = ta4not;
041700120928         endif;
041800120928         WFWRMA = %trim(§TA4aRMA);
041900120928
042000120928       ENDSR;
042100120928
042200111128       //--------------------------------------------------------------
042300120208       // Operazioni finali
042400111128       //--------------------------------------------------------------
042500111128       BEGSR  sr_RoutEnd;
042600111128
042700120208         // - Uscita
042800111128         return;
042900111128
043000111128       ENDSR;
043100111128
043200111128      /end-free
043300121002
043400121002     C     SviluppoDisk  BEGSR
043500121002
043600121002         // importo la DS con i dati di input
043700121002     C                   EVAL      AASIF0 = TASAAS
043800121002     C                   EVAL      LnPIF0 = TASLnP
043900121002     C                   EVAL      NrSIF0 = TASNrS
044000121002     C                   EVAL      NSpIF0 = TASNSp
044100121002     C                   EVAL      TBlIF0 = TASTBl
044200121002     C                   RESET                   Operazione
044300121002
044400121002         // Reperisco l'elenco dei colli.
044500121002     C                   DO        *HIVAL
044600121002
044700121002     C                   CLEAR                   WFWDSKC
044800121002     C                   CLEAR                   WFWDSKB
044900121002     C                   CLEAR                   TIS7F0DSO
045000121002
045100121002     C                   CALL(E)   'TIS7F000R'
045200121002     C                   PARM                    TIS7F0DSI
045300121002     C                   PARM                    Operazione
045400121002     C                   PARM                    TIS7F0DSO
045500121002     C                   PARM      *ZERO         RigheOut
045600121002     C                   PARM      *BLANK        Esito
045700121002
045800121002     C                   IF        Esito = '1' OR %ERROR
045900121002       // - Chiusura cursore
046000121002     C                   exsr      sr_CloseCursor
046100121002       // - Operazioni finali
046200121002     C                   exsr      sr_RoutEnd
046300121002     C                   ENDIF
046400121002
046500121002     C                   CLEAR                   i
046600121002
046700121002     C                   DO        *HIVAL
046800121002     C                   EVAL      i = i + 1
046900121002     C                   IF        (Col(i) = *BLANK AND Chi(i) = *BLANK)
047000121002     C                   LEAVE
047100121002     C                   ENDIF
047200121002     C                   IF        Col(i) <> *BLANK
047300121002     C                             and WFWTipExe = 'DSKB'
047400121002     C                   EVAL      WFWDSKB = Col(i)
047500121002     C                   ENDIF
047600121002     C                   IF        Chi(i) <> *BLANK
047700121002     C                             and WFWTipExe = 'DSKC'
047800121002     C                   EVAL      WFWDSKC = %TRIMR(Chi(i))
047900121002     C                   ENDIF
048000121002
048100121002     C                   write     WFWST000
048200121002
048300121002     C                   ENDDO
048400121002
048500121002       // fine dei dati
048600121002     C                   IF        Operazione = '9'
048700121002     C                   LEAVE
048800121002     C                   ENDIF
048900121002
049000121002       // richiesta successiva
049100121002     C                   EVAL      Operazione = '2'
049200121002
049300121002     C                   ENDDO
049400121002
049500121002     C                   ENDSR
