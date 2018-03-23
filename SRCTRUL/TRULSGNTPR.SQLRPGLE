000100111128       //==============================================================
000200120405       // TRULSGNTPR - Aggiorna FISGN da SETRAS ad AS888
000300120405       // Questa funzione serve per aggiornare e/o aggiungere dati al file FISGN00F in modo che la funzione POSITROVA
000400120405       // possa avere sempre una massa critica di dati di test.
000500120405       // Siccome POSITROVA, quando si porta sul suo DB i dati, elimina i rcd già stampati (SGNST2>0) e comunque più
000600120405       // vecchi di una settimana
000700120405       // per evitare che accada questo sui dati di test viene detto che
000800120405       // - il rcd è da stampare
000900120405       // - è del prossimo anno. Se mese/giorno sono il 29 febbraio,  per evitare errori lo imposto al 28 febbraio.
001000120405       //==============================================================
001100111128
001200120404       // per compilare si deve avere in linea la lib. FILTRAPRD
001300120404       // OGNI volta che si compila ricordarsi:
001400120210       // - portare il pgm su SETRAS in una libreria che esista come nome anche su AS888
001500120210       // - creare su SETRAS il pacchetto SQL che risiederà su AS888:
001600120404       //   CRTSQLPKG PGM(TRULSGNTPR) RDB(AS888)
001700120209
001800111128       //--------------------------------------------------------------
001900120207       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002000111128       //--------------------------------------------------------------
002100111128
002200111128     /*PRM  dbgview(*source)
002300111128     /*END
002400111128
002500111128       //--------------------------------------------------------------
002600120207       // Specifiche di controllo.                                     ?
002700111128       //--------------------------------------------------------------
002800111128
002900111128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003000111128     h dftactgrp(*no)
003100111128     h alwnull(*inputonly)
003200111128
003300111128       //--------------------------------------------------------------
003400120207       // Dichiarazione file.                                          ?
003500111128       //--------------------------------------------------------------
003600120215       // - Video
003700120404     ftrulSGNTPDcf   e             workstn
003800120207     f                                     indds(IndDspF)
003900120207     f                                     infds(InfDspF)
004000111128
004100111128
004200111128       //--------------------------------------------------------------
004300120207       // Definizione costanti.                                        ?
004400111128       //--------------------------------------------------------------
004500120207
004600120207       // - Tasti funzionali a video
004700120207     d c_F01           c                   const(x'31')
004800120207     d c_F02           c                   const(x'32')
004900120207     d c_F03           c                   const(x'33')
005000120207     d c_F05           c                   const(x'35')
005100120207     d c_F06           c                   const(x'36')
005200120207     d c_F07           c                   const(x'37')
005300120207     d c_F08           c                   const(x'38')
005400120207     d c_F09           c                   const(x'39')
005500120207     d c_F10           c                   const(x'3A')
005600120207     d c_F12           c                   const(x'3C')
005700120207     d c_F13           c                   const(x'B1')
005800120207     d c_F14           c                   const(x'B2')
005900120207     d c_F15           c                   const(x'B3')
006000120207     d c_F16           c                   const(x'B4')
006100120207     d c_F17           c                   const(x'B5')
006200120207     d c_F18           c                   const(x'B6')
006300120207     d c_F19           c                   const(x'B7')
006400120207     d c_F20           c                   const(x'B8')
006500120207     d c_F21           c                   const(x'B9')
006600120207     d c_F22           c                   const(x'BA')
006700120207     d c_F23           c                   const(x'BB')
006800120207     d c_F24           c                   const(x'BC')
006900120207     d c_Enter         c                   const(x'F1')
007000120207     d c_RollDown      c                   const(x'F4')
007100120207     d c_RollUp        c                   const(x'F5')
007200111128
007300111128
007400111128       //--------------------------------------------------------------
007500120207       // Definizione schiere.                                         ?
007600111128       //--------------------------------------------------------------
007700111128
007800111128
007900111128       //--------------------------------------------------------------
008000120207       // Definizione aree dati.                                       ?
008100111128       //--------------------------------------------------------------
008200111128
008300111128
008400111128       //--------------------------------------------------------------
008500120207       // Definizione strutture dati.                                  ?
008600111128       //--------------------------------------------------------------
008700120404     d FISGN00F      e ds                  occurs(32766)
008800111128
008900111128       //--------------------------------------------------------------
009000120207       // Definizione variabili globali.                               ?
009100111128       //--------------------------------------------------------------
009200111128
009300120208       // - Stringa SQL da eseguire
009400111128     d wSQl            s           1024    inz  varying
009500111128
009600120208       // - Parametri SQL
009700120207
009800120207       // - InfDS
009900120207     d InfDspF         ds
010000120207     d   dsp_aid             369    369a                                        AID byte
010100120207
010200120207       // - Indicatori su DspF
010300120207     d IndDspF         ds
010400120207        // - Indicatori di errore
010500120208     d  ErrMsg                        1n   overlay(IndDspF : 28)
010600120207     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010700120404
010800120404     d KPJBA         e ds
010900120207
011000120207       // - Flags booleani
011100120207     d $Fine           s               n   inz(*off)
011200120207     d $InzD01         s               n   inz(*on)
011300120208     d ErrBlock        s               n   inz(*off)
011400120210     d ErrInsert       s               n   inz(*off)
011500120210     d Qcmd            s            500    inz
011600120207
011700120207       // - Campi associati al video
011800120207     d $Video          s              2    inz('D1')
011900120207
012000120207       // - Variabili di appoggio
012100120207     d DBRemote        s             10    inz('AS888')
012200120404     d FISGNOccurs     s              5s 0
012300120405     d wX              s              5s 0
012400111128
012500111128       //--------------------------------------------------------------
012600120210       // Definizione procedure usate
012700111128       //--------------------------------------------------------------
012800120207
012900120210       // - Parametri API QCAPCMD (Process Commands)
013000120207      /copy qSysInc/qRpgleSrc,QCAPCMD
013100120207      /copy gaitrasrc/srcProtoPR,QCAPCMD
013200120207
013300120210       // - Parametri gestione errori API
013400120207      /copy qSysInc/qRpgleSrc,QUSEC
013500120404
013600120404     d TRULSGNTPR      pr
013700120404     d  Arch                        502
013800120404
013900120404     d TRULSGNTPR      pi
014000120404     d  Arch                        502
014100111128
014200111128       //--------------------------------------------------------------
014300120208       // Definizione key-list.                                        ?
014400111128       //--------------------------------------------------------------
014500111128
014600111128
014700111128       //--------------------------------------------------------------
014800120208       // M A I N - L I N E                                            ?
014900111128       //--------------------------------------------------------------
015000111128
015100111128      /free
015200111128
015300120208       // - Operazioni iniziali
015400111128       exsr  sr_RoutInz;
015500111128
015600120207       // - Gestione video
015700120207       DOW  $Fine = *off;
015800120207         select;
015900120207           when  $Video = 'D1';
016000120207             exsr  sr_GesD01;
016100120207           other;
016200120207             $Fine = *on;
016300120207         endsl;
016400120207       ENDDO;
016500120207
016600120208       // - Operazioni finali
016700111128       exsr  sr_RoutEnd;
016800111128
016900111128       //--------------------------------------------------------------
017000120208       // Operazioni iniziali
017100111128       //--------------------------------------------------------------
017200111128       BEGSR sr_RoutInz;
017300111128
017400111128         *inLR = *on;
017500120207         IndDspF  = *off;
017600120404         KPJBA = Arch;
017700111128
017800120208         // - Impostazione opzioni per SQL
017900111128         exec SQL   set option   DynUsrPrf = *owner,
018000111128                                 CloSqlCsr = *endmod;
018100111128
018200111128
018300111128       ENDSR;
018400111128
018500111128       //--------------------------------------------------------------
018600120208       // Apertura cursore C1
018700111128       //--------------------------------------------------------------
018800111128       BEGSR  sr_OpenCursor;
018900111128
019000120208         // - Dichiarazione del cursore
019100111128         exec sql  prepare S1  from :wSQL;
019200111128         exec sql  declare C1  cursor for S1;
019300111128
019400120208         // - Apertura del cursore
019500111128         exec sql   open C1;
019600111128
019700111128       ENDSR;
019800111128
019900111128       //--------------------------------------------------------------
020000120208       // Chiusura cursore C1
020100111128       //--------------------------------------------------------------
020200111128       BEGSR  sr_CloseCursor;
020300111128
020400120208         // - Chiusura del cursore
020500111128         exec sql   close C1;
020600111128
020700111128       ENDSR;
020800111128
020900111128       //--------------------------------------------------------------
021000120208       // Lettura cursore C1
021100111128       //--------------------------------------------------------------
021200111128       BEGSR  sr_ReadCursor;
021300111128
021400111128         select;
021500111128
021600120208           // - Fine lettura
021700111128           when  sqlCode = 100;
021800120404             //non devo fare nulla perché devo scrivere tutti i rcd di tutti i cursori letti
021900120208           leavesr;
022000111128
022100120208           // - Errore
022200111128           when  sqlCode < *zero;
022300111128             exsr  sr_PrintErr;
022400111128
022500120208           // - Elaborazione
022600111128           other;
022700120208             //non devo fare nulla adesso perché prima devo leggere tutti i rcd di tutti i cursori
022800111128
022900111128         endsl;
023000111128
023100111128       ENDSR;
023200120207
023300120207       //--------------------------------------------------------------
023400120208       // Gestione videata D01
023500120207       //--------------------------------------------------------------
023600120207       BEGSR  sr_GesD01;
023700120207
023800120207         // - Inizializzazione videata
023900120207         if  $InzD01   = *on;
024000120207           exsr  sr_InzD01;
024100120207           $InzD01  = *off;
024200120207         endif;
024300120207
024400120207         // - Emissione Testata e Piede con tasti funzionali abilitati
024500120208         if  ErrGenerico = *off and ErrMsg = *off;
024600120207           write  trkuT01;
024700120207           write  trkuP01;
024800120207         endif;
024900120207
025000120207         // - Emissione videata
025100120207         exfmt  trkuD01;
025200120207
025300120208         reset  ErrMsg;
025400120207         reset  ErrGenerico;
025500120207         clear  V1Dmsg;
025600120207
025700120207         SELECT;
025800120207
025900120207           // - F3=Fine
026000120207           WHEN  dsp_aid = c_F03;
026100120207             exsr sr_F03D01;
026200120207
026300120207           // - F12=Ritorno
026400120207           WHEN  dsp_aid = c_F12;
026500120207             exsr sr_F12D01;
026600120207
026700120207           // - Invio / F6=Conferma
026800120207           OTHER;
026900120207             exsr  sr_CtrD01;
027000120207             if  ErrGenerico = *on;
027100120207               leavesr;
027200120207             endif;
027300120207             if  dsp_aid = c_F06;
027400120207               exsr sr_F06D01;
027500120207             endif;
027600120207
027700120207         ENDSL;
027800120207
027900120207       ENDSR;
028000120207
028100120207       //--------------------------------------------------------------
028200120208       // Inizializzazione videata D01
028300120207       //--------------------------------------------------------------
028400120207       BEGSR  sr_InzD01;
028500120207
028600120207         // - Pulizia videata
028700120207         clear  trkuD01;
028800120404
028900120404         // - impostazioni default
029000120404         VIDTNP  = 043;
029100120404         VIDCLR  = 'N';
029200120207
029300120207       ENDSR;
029400120207
029500120207       //--------------------------------------------------------------
029600120207       //?Gestione tasto funzionale F3 da videata D01                  ?
029700120207       //?F3=Fine                                                      ?
029800120207       //--------------------------------------------------------------
029900120207       BEGSR  sr_F03D01;
030000120207
030100120207         // - Chiusura del programma
030200120207         $Fine = *on;
030300120207
030400120207       ENDSR;
030500120207
030600120207       //--------------------------------------------------------------
030700120208       // Gestione tasto funzionale F6 da videata D01
030800120208       // F6=Conferma
030900120207       //--------------------------------------------------------------
031000120207       BEGSR  sr_F06D01;
031100120208
031200120216         // Fase 1
031300120216
031400120216         // Copio i dati da aggiungere su AS888
031500120216         exsr  CopiaDaSetras;
031600120216         if ErrBlock = *on;
031700120216           leavesr;
031800120216         endif;
031900120207
032000120405         // Modifica i dati copiati
032100120405         exsr  ModDatiCopiati;
032200120405
032300120405         // Scrivo dati da aggiungere se AS888
032400120405         exsr  CopiaSuAS888;
032500120216         if ErrBlock = *on or ErrInsert = *on;
032600120216           leavesr;
032700120216         endif;
032800120215
032900120208         // - Chiusura del programma (solo se non ho errori da mostrare)
033000120208         if ErrMsg = *off;
033100120208           $Fine = *on;
033200120208         endif;
033300120207
033400120207       ENDSR;
033500120208
033600120207       //--------------------------------------------------------------
033700120208       // Gestione tasto funzionale F12 da videata D01
033800120208       // F12=Ritorno
033900120207       //--------------------------------------------------------------
034000120207       BEGSR  sr_F12D01;
034100120207
034200120207         // - Chiusura del programma
034300120207         $Fine = *on;
034400120207
034500120207       ENDSR;
034600120207
034700120207       //--------------------------------------------------------------
034800120207       // Controllo videata D01                                        ?
034900120207       //--------------------------------------------------------------
035000120207       BEGSR  sr_CtrD01;
035100120207
035200120207         IndDspF  = *off;
035300120208
035400120208         // l'utente remoto va indicato
035500120208         if VIDUsr = *blank;
035600120208           V1Dmsg = 'Utente remoto obbligatorio';
035700120208           ErrMsg = *on;
035800120208           ErrGenerico = *on;
035900120208           leavesr;
036000120208         endif;
036100120208
036200120208         // la pwd dell'utente remoto va indicata
036300120208         if VIDPwd = *blank;
036400120208           V1Dmsg = 'Pwd utente remoto obbligatoria';
036500120208           ErrMsg = *on;
036600120208           ErrGenerico = *on;
036700120208           leavesr;
036800120208         endif;
036900120405
037000120405         // il terminal di partenza va indicato
037100120405         if VIDTNP = 0;
037200120405           V1Dmsg = 'Terminal di partenza obbligatorio';
037300120405           ErrMsg = *on;
037400120405           ErrGenerico = *on;
037500120405           leavesr;
037600120405         endif;
037700120207
037800120207       ENDSR;
037900120208
038000120208       //--------------------------------------------------------------
038100120208       // Reperisco i dati da copiare
038200120208       //--------------------------------------------------------------
038300120216       BEGSR  CopiaDaSetras;
038400120208
038500120208         ErrBlock = *off;
038600120208
038700120208         // - Reperisci dati da aggiungere
038800120208
038900120404         //reperisco dati FISGN
039000120210         //---------------------
039100120404         clear *all FISGN00F;
039200120208         // - Creazione stringa SQL da eseguire
039300120404         wSQL = 'select * from FILTRA201/FISGN00F +
039400120404                 where SGNTNP = 043';
039500120208         // - Apertura cursore
039600120208         exsr  sr_OpenCursor;
039700120404         FISGNoccurs = 1;
039800120209         // punto all'occorrenza da valorizzare
039900120404         %occur(FISGN00F) = FISGNoccurs;
040000120418         // ciclo finché non sono finiti i rcd da leggere
040100120418         DoU  SQLCode = 100
040200120418         // oppure non c'è stato un errore bloccante
040300120418          or SQLCode < 0
040400120418         // oppure abbiamo superato il limite delle occorrenze memorizzabili
040500120418          or FISGNoccurs > %elem(FISGN00F);
040600120404           exec sql  fetch next  from C1  into :FISGN00F;
040700120208           exsr  sr_ReadCursor;
040800120208           select;
040900120209           // errore
041000120209           when SQLCode < 0;
041100120404             V1Dmsg = 'Errore in  lettura FISGN00F - vedi stampa errore';
041200120208             ErrMsg = *on;
041300120208             ErrGenerico = *on;
041400120208             ErrBlock = *on;
041500120208             // - Chiusura cursore (visto che esco con leavesr)
041600120208             exsr  sr_CloseCursor;
041700120208             leavesr;
041800120209           // nessuna lettura
041900120404           when SQLCode = 100 and FISGNoccurs = 1;
042000120404             V1Dmsg = 'Nessun dato per terminal 043';
042100120209             ErrMsg = *on;
042200120209             ErrGenerico = *on;
042300120209             ErrBlock = *on;
042400120209             // - Chiusura cursore (visto che esco con leavesr)
042500120209             exsr  sr_CloseCursor;
042600120209             leavesr;
042700120209           // lettura
042800120209           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
042900120404             FISGNoccurs = FISGNoccurs + 1;
043000120404             // punto all'occorrenza da valorizzare se non siamo già arrivati a fine occorrenza
043100120404             if FISGNoccurs <= %elem(FISGN00F);
043200120404               // punto all'occorrenza da valorizzare
043300120404               %occur(FISGN00F) = FISGNoccurs;
043400120404             endif;
043500120209           // fine lettura
043600120209           other;
043700120209             // non faccio nulla
043800120209           endsl;
043900120208         EndDo;
044000120208         // - Chiusura cursore
044100120208         exsr  sr_CloseCursor;
044200120209         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
044300120209         // le occorrenze davvero valorizzate sono inferiori di 1
044400120404         FISGNoccurs = FISGNoccurs - 1;
044500120208
044600120208       ENDSR;
044700111128
044800111128       //--------------------------------------------------------------
044900120405       // Modifico i dati copiati
045000111128       //--------------------------------------------------------------
045100120405       BEGSR  ModDatiCopiati;
045200111128
045300120405         // ciclo per tutte le occorrenze reperite impostando i dati in modo che
045400120405         // - il progressivo di stampa = 0 = non stampato
045500120405         // - la data di spedizione = anno prossimo, stesso mese/giorno indicato, tranne se questo è il 29 febbraio che
045600120405         //                           per evitare errori lo imposto al 28 febbraio
045700120405         for wX = 1 to FISGNoccurs;
045800120405           // punto all'occorrenza da leggere
045900120405           %occur(FISGN00F) = wX;
046000120405           SGNST2 = 0;
046100120405           SGNAAS = SGNAAS + 1;
046200120405           if SGNMGS = 0229;
046300120405             SGNMGS = 0228;
046400120405           endif;
046500120405         endfor;
046600120405
046700120405       ENDSR;
046800120405
046900120405       //--------------------------------------------------------------
047000120405       // Scrivo rcd estratti
047100120405       //--------------------------------------------------------------
047200120405       BEGSR  CopiaSuAS888;
047300120405
047400120210         ErrMsg = *off;
047500120210         ErrGenerico = *off;
047600120216         ErrBlock = *off;
047700120216         ErrInsert = *off;
047800120210
047900120207         // mi connetto ad AS888
048000120216         exsr ConnectToAS888;
048100120216         if ErrBlock = *on;
048200120216           leavesr;
048300120216         endif;
048400120207
048500120208         // esecuzione scrittura rcd estratti
048600120208         exsr sr_InsertRcd;
048700120208
048800120208         // mi disconnetto da AS888
048900120216         exsr DisconnectFromAS888;
049000120216         if ErrBlock = *on;
049100120216           leavesr;
049200120216         endif;
049300120208
049400120208         // mi riconnetto a SETRAS
049500120216         exsr ConnectToSetras;
049600120216         if ErrBlock = *on;
049700120216           leavesr;
049800120216         endif;
049900120207
050000111128       ENDSR;
050100120216
050200120216       //--------------------------------------------------------------
050300120216       // Connessione
050400120216       //--------------------------------------------------------------
050500120216       BEGSR  ConnectToAS888;
050600120216
050700120216         // mi connetto ad AS888
050800120216         exec sql  connect to :DBRemote user :VIDUSR using :VIDPWD;
050900120216         if SQLCode < 0;
051000120216           //emetto msg di errore perentorio
051100120404           V1Dmsg = 'Non riuscita connessione ad AS888 +
051200120216                     - SCOLLEGARE la sessione e riprovare';
051300120216           ErrMsg = *on;
051400120216           ErrGenerico = *on;
051500120216           ErrBlock = *on;
051600120216         endif;
051700120216
051800120216       ENDSR;
051900120216
052000120216       //--------------------------------------------------------------
052100120216       // Disconnessione
052200120216       //--------------------------------------------------------------
052300120216       BEGSR  DisconnectFromAS888;
052400120216
052500120216         // mi disconnetto da AS888
052600120216         exec sql  disconnect :DBRemote;
052700120216         // se la disconnessione ha dato errore
052800120216         if SQLCode < 0;
052900120216           //emetto msg di errore perentorio
053000120216           if ErrInsert = *on;
053100120404             V1Dmsg = 'Agg. NON eseguito ED errore in +
053200120216                       connessione SETRAS - SCOLLEGARSI!';
053300120216           else;
053400120404             V1Dmsg = 'Agg. eseguito ma errore in +
053500120216                       connessione SETRAS - SCOLLEGARSI!';
053600120216           endif;
053700120216           ErrMsg = *on;
053800120216           ErrGenerico = *on;
053900120216           ErrBlock = *on;
054000120216         endif;
054100120216
054200120216       ENDSR;
054300120216
054400120216       //--------------------------------------------------------------
054500120216       // Riconnessione a SETRAS
054600120216       //--------------------------------------------------------------
054700120216       BEGSR  ConnectToSetras;
054800120216
054900120216         // mi riconnetto a SETRAS
055000120216         // siccome in BRT gli SQLRPGLE si compilano col parametro RDBCNNMTH = *DUW, la connect ad AS888 NON ha chiuso
055100120216         // la connessione a SETRAS, l'ha resa dorminte. In questo caso NON serve un cmd CONNECT RESET perché  darebbe
055200120216         // errore SQL0842 = Connessione al database relazionale &1 già esistente.
055300120216         // Però, la connessione al DB locale (SETRAS) esiste se prima c'è stato almeno uno statement sql se quel DB.
055400120216         // In questo caso la SET CONNECTION dà errore SQL0842 = Il collegamento al database relazionale &1 non esiste
055500120216         // e va eseguita la CONNECT RESET.
055600120216         // in questo pgm dovrebbe sempre esserci prima uno statement SQL su SETRAS per leggere i dati,
055700120216         // ma non si sa mai... magari vengono fatte modifiche
055800120216         // Per cui, dopo aver disconnesso AS888, fare il cmd SET CONNECTION per "risvegliare" SETRAS e, se dà errore
055900120216         // provare con CONNECT RESET.
056000120216         // Solo se entrambi hanno dato errore, emettere un msg a video.
056100120216         exec sql  set connection SETRAS;
056200120216         if SQLCode < 0;
056300120216           // provo con CONNECT RESET
056400120216           exec sql  connect reset;
056500120216           // se anche in questo caso ho errore, emetto msg
056600120216           if SQLCode < 0;
056700120216             //emetto msg di errore perentorio
056800120216             if ErrInsert = *on;
056900120404               V1Dmsg = 'Agg. NON eseguito ED errore in +
057000120216                         connessione SETRAS - SCOLLEGARSI!';
057100120216             else;
057200120404               V1Dmsg = 'Agg. eseguito ma errore in +
057300120216                         connessione DB SETRAS - SCOLLEGARSI!';
057400120216             endif;
057500120216             ErrMsg = *on;
057600120216             ErrGenerico = *on;
057700120216             ErrBlock = *on;
057800120216           endif;
057900120216         endif;
058000120216
058100120216       ENDSR;
058200111128
058300120208       //--------------------------------------------------------------
058400120208       // Scrivo rcd estratti
058500120208       //--------------------------------------------------------------
058600120208       BEGSR  sr_InsertRcd;
058700120208
058800120404         // se richiesto pulisco i rcd presenti per il terminal indicato
058900120404         if VIDClr = 'S';
059000120404           EXEC SQL delete from FILTRAPRD/FISGN00F where SGNTnp = :VIDTNP
059100120210                    with UR;
059200120208           if SQLCode < 0;
059300120404             // se ho avuto un errore nella delete interrompo la copia
059400120404             // dando errore
059500120404             exec sql rollback;
059600120404             V1Dmsg = 'Pulizia FISGN in errore: qualcuno alloca +
059700120404                     - controllare';
059800120404             ErrMsg = *on;
059900120404             ErrGenerico = *on;
060000120404             ErrInsert = *on;
060100120404             leavesr;
060200120208           endif;
060300120208         endif;
060400120208
060500120404         // aggiungo il record letto al FISGN00F di test
060600120404         if FISGNoccurs > 0;
060700120404           EXEC SQL insert into FILTRAPRD/FISGN00F :FISGNoccurs rows
060800120404                    values(:FISGN00F)
060900120404                    with UR;
061000120404           if SQLCode < 0;
061100120404             // se ho avuto un errore nell'insert, a regime sarà solo per chiave duplicata, quindi interrompo la copia
061200120404             // dando errore
061300120404             exec sql rollback;
061400120404             V1Dmsg = 'Problema in insert, probabile chiave duplicata +
061500120404                     - prima pulire';
061600120404             ErrMsg = *on;
061700120404             ErrGenerico = *on;
061800120404             ErrInsert = *on;
061900120404             leavesr;
062000120404           endif;
062100120404         endif;
062200120404
062300120210         // se sono qui vuol dire che ho scritto tutto per cui committo
062400120210         exec sql commit;
062500120210
062600120208       ENDSR;
062700120215
062800120215
062900120215       //--------------------------------------------------------------
063000120215       // Esecuzione del comando (già impostato)
063100120215       //--------------------------------------------------------------
063200120215       BEGSR  sr_ExecCmd;
063300120215
063400111128         clear Qcap0100;
063500111128         Qcabcsdh = *off;
063600111128         Qcapa    = *off;
063700111128         Qcacmdss = *off;
063800111128         Qcaerved = *allX'00';
063900111128
064000111128         clear Qusec;
064100111128         Qusbprv  = %size(Qusec);
064200111128
064300111128         ProcessCommands ( Qcmd : %len( %trimr( Qcmd ) ) : Qcap0100 :
064400111128                           %size(Qcap0100) : 'CPOP0100' : *omit :
064500111128                           0 : 0 : Qusec);
064600111128
064700120210         // - Stampa DUMP + JobLog  &  Chiusura *pgm  in caso di errore
064800111128         if  Qusei <> *blank;
064900111128           exsr  sr_PrintErr;
065000111128         endif;
065100111128
065200111128       ENDSR;
065300111128
065400111128       //--------------------------------------------------------------
065500120210       // Stampa segnalazione dell'errore rilevato
065600111128       //--------------------------------------------------------------
065700111128       BEGSR  sr_PrintErr;
065800111128
065900120210         // - Stampa del Dump
066000111128         Dump(A);
066100111128
066200120210         // - Stampa del Job-Log
066300111128         Qcmd = 'DSPJOBLOG job(*) output(*print)';
066400111128         exsr  sr_ExecCmd;
066500111128
066600111128       ENDSR;
066700111128
066800111128       //--------------------------------------------------------------
066900120208       // Operazioni finali
067000111128       //--------------------------------------------------------------
067100111128       BEGSR  sr_RoutEnd;
067200111128
067300120208         // - Uscita
067400111128         return;
067500111128
067600111128       ENDSR;
067700111128
067800111128      /end-free
