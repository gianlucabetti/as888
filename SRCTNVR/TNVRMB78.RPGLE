000100120423      //---------------------------------------------------------------
000200120423      //
000300120423      //?              Mod. Commerciale da file WORK
000400120423      //
000500120423      //---------------------------------------------------------------
000600120423
000700120423     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800120423
000900120423      //---------------------------------------------------------------
001000120423      //?Dichiarazione file.
001100120423      //---------------------------------------------------------------
001200120423
001300120423      // - File anagrafiche clienti
001400120423     fCNACO00F  uf   e           k disk
001500120423     fCNCLP00F  uf   e           k disk
001600120423
001700120423      // - File di WORK
001800120423     fFILEANA1  if   e             disk
001900120423     fFILEANA2  if   e             disk
002000120423
002100120423      // - File tabelle
002200120423     fTABEL00F  if   e           k disk
002300120423
002400120423      // - File tabelle
002500120423     fTNVRMB78P o    e             printer oflind(*in99)
002600120423
002700120423      //---------------------------------------------------------------
002800120423      //?Definizione costanti.
002900120423      //---------------------------------------------------------------
003000120423
003100120423      //---------------------------------------------------------------
003200120423      //?Definizione schiere.
003300120423      //---------------------------------------------------------------
003400120423
003500120423      //---------------------------------------------------------------
003600120423      //?Definizione aree dati.
003700120423      //---------------------------------------------------------------
003800120423
003900120423      //---------------------------------------------------------------
004000120423      //?Definizione strutture dati.
004100120423      //---------------------------------------------------------------
004200120423       // - Reperimento dati anagrafici
004300120423     d DS_cnaco      e ds                  inz extname(CNACO00F) prefix(w_)
004400120423     d DS_cnind      e ds                  inz extname(CNIND00F) prefix(w_)
004500120423     d DS_cnclp      e ds                  inz extname(CNCLP00F) prefix(w_)
004600120423     d DS_fncls      e ds                  inz extname(FNCLS00F) prefix(w_)
004700120423
004800120423      // - Registra variazioni
004900120423     d TIBS73ds      e ds                  inz
005000120423     d cnaco_73      e ds                  inz  extname(Cnaco00f)
005100120423     d cnind_73      e ds                  inz  extname(Cnind00f)
005200120423     d cnclp_73      e ds                  inz  extname(Cnclp00f)
005300120423     d fncls_73      e ds                  inz  extname(Fncls00f)
005400120423
005500120423      // - Tabella 01 = Codici Commerciali
005600120423     d ds01          e ds                  inz
005700120423
005800120423     d keyaco          ds                  likerec(cnaco000:*key)
005900120423     d keyclp          ds                  likerec(cnclp000:*key)
006000120423
006100120423      //---------------------------------------------------------------
006200120423      //?Definizione variabili globali.
006300120423      //---------------------------------------------------------------
006400120423      // - Flags booleani
006500120423     d wEoF            s               n   inz(*off)
006600120423     d wStampa         s               n   inz(*off)
006700120423
006800120423      // - Campi di comodo
006900120423     d wOggi           s              8  0
007000120423     d wdatadec        s              8  0
007100120423     d wdatafin        s              8  0
007200120423
007300120423      //---------------------------------------------------------------
007400120423      //?Definizione procedure esterne.
007500120423      //---------------------------------------------------------------
007600120423
007700120423      //---------------------------------------------------------------
007800120423      //?Definizione prototipi.
007900120423      //---------------------------------------------------------------
008000120423
008100120423      // - Memorizzo variazioni
008200120423      /copy gaitrasrc/srcprotopr,tibs73r
008300120423
008400120423      //---------------------------------------------------------------
008500120423      //?Definizione key-list.
008600120423      //---------------------------------------------------------------
008700120423
008800120423       // - File TABEL00F
008900120423     d k03tabel      e ds                  extname(TABEL00F:*key)
009000120423     d                                     prefix(k_)
009100120423
009200120423      //---------------------------------------------------------------
009300120423      //?Riepilogo indicatori.
009400120423      //---------------------------------------------------------------
009500120423
009600120423      //---------------------------------------------------------------
009700120423      //?M A I N - L I N E
009800120423      //---------------------------------------------------------------
009900120423
010000120423      /free
010100120423
010200120423       //?Operazioni iniziali
010300120423       exsr RoutInz;
010400120423
010500120423       //?Elabora file 1
010600120423       exsr Elabora_1;
010700120423
010800120423       //?Elabora file 2
010900120423       exsr Elabora_2;
011000120423
011100120423       //?Operazioni finali
011200120423       exsr RoutEnd;
011300120423
011400120423       //--------------------------------------------------------------
011500120423       //?Operazioni iniziali.
011600120423       //--------------------------------------------------------------
011700120423       BEGSR RoutInz;
011800120423
011900120423         k_TBLkut = 1;
012000120423         wOggi    = %dec(%date());
012100120423
012200120423       ENDSR;
012300120423
012400120423       //--------------------------------------------------------------
012500120423       //?Elabora file 1.
012600120423       //--------------------------------------------------------------
012700120423       BEGSR Elabora_1;
012800120423
012900120423         wEoF = *off;
013000120423
013100120423       //?Leggo il file di work
013200120423         DOW  not wEoF;
013300120423           read FILEANA1;
013400120423           IF  %eof;
013500120423             wEoF = *on;
013600120423             leavesr;
013700120423           ENDIF;
013800120424           clear cnaco_73;
013900120423
014000120423         //?Aggancio CNCLP
014100120423           keyclp.clpkut = 1;
014200120423           keyclp.clpkcc = 151;
014300120423           keyclp.clpksc = ksc;
014400051223
014500120423           chain %kds(keyclp) CNCLP00F;
014600120423         //?non trovo CNCLP NON aggiorno ma stampo
014700120423           IF  not %found(CNCLP00F);
014800120423             stperr = 'Non trovato CNLCP';
014900120423             exsr Stampa;
015000120423             iter;
015100120423           ENDIF;
015200120423         //?controllo commerciale su CNCLP
015300120423           clear ds01;
015400120423           k_TBLcod = '01';
015500120423           k_TBLkey = %editc(CLPage:'X');
015600120423           chain  %kds(K03tabel) TABEL00F;
015700120423         //?commerciale CNCLP valido NON aggiorno ma stampo
015800120423           IF  %found(TABEL00F);
015900120423             stperr = 'Commerciale su CLP valido';
016000120423             exsr Stampa;
016100120423             iter;
016200120423           ENDIF;
016300120423         //?commerciale CNCLP NON valido
016400120423         //?controllo il commerciale del file di work
016500120423           clear ds01;
016600120423           k_TBLcod = '01';
016700120423           k_TBLkey = %editc(com:'X');
016800120423           chain  %kds(K03tabel) TABEL00F;
016900120423         //?commerciale NON valido NON aggiorno e stampo
017000120423           IF  not %found(TABEL00F) or TBLflg <> *blanks;
017100120423             stperr = 'Commerciale passato NON valido';
017200120423             exsr Stampa;
017300120423             iter;
017400120423           ENDIF;
017500120423           ds01 = TBLuni;
017600120423           wdatafin = %dec(§01dtfin:8:0);
017700120423         //?commerciale scaduto NON aggiorno e stampo
017800120423           IF  wOggi >= wdatafin;
017900120423             stperr = 'Commerciale passato scaduto';
018000120423             exsr Stampa;
018100120423             iter;
018200120423           ENDIF;
018300120423         //?commerciale con particolarità VARI
018400120423           IF  §01par = '1';
018500120423             stperr = 'Commerciale passato VARI';
018600120423             exsr Stampa;
018700120423             iter;
018800120423           ENDIF;
018900120423         //?commerciale con particolarità INATTIVI
019000120423           IF  §01par = '2';
019100120423             stperr = 'Commerciale passato INATTIVI';
019200120423             exsr Stampa;
019300120423             iter;
019400120423           ENDIF;
019500120423         //?Se arrivo qua tutto OK
019600120423         //?salvo l'immagine precedente dell'anagrafica
019700120423           clear TIBS73ds;
019800120423           IBS73immag = 'P';
019900120424           IBS73yda   = 'C';
020000120423           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
020100120423         //?Aggiorno il commerciale
020200120423           CLPage = com;
020300120423           update CNCLP000;
020400120423         //?Sfleggo la testata per inviare variazione a ProJ
020500120423           keyaco.acokut = clpkut;
020600120423           keyaco.acokcc = clpkcc;
020700120423           keyaco.acoksc = clpksc;
020800120423           chain %kds(keyclp) CNACO00F;
020900120423           IF  %found(CNACO00F);
021000120423             clear ACOdtr;
021100120423             clear ACOftr;
021200120423             update CNACO000;
021300120423           ENDIF;
021400120423
021500120423         //?Scrivo la variazione
021600120423           clear TIBS73ds ;
021700120423           IBS73pru   = 'EDPCED';
021800120423           IBS73noj   = 'CRM';
021900120423           IBS73pgm   = 'TNVRMB78';
022000120423           IBS73immag = 'D';
022100120423           IBS73cta   = 'M' ;
022200120424           IBS73yda   = 'C';
022300120423           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
022400051223
022500120423         ENDDO;
022600051222
022700120423       ENDSR;
022800120423
022900120423       //--------------------------------------------------------------
023000120423       //?Elabora file 2.
023100120423       //--------------------------------------------------------------
023200120423       BEGSR Elabora_2;
023300120423
023400120423         wEoF = *off;
023500120423
023600120423       //?Leggo il file di work
023700120423         DOW  not wEoF;
023800120423           read FILEANA2;
023900120423           IF  %eof;
024000120423             wEoF = *on;
024100120423             leavesr;
024200120423           ENDIF;
024300120424           clear cnaco_73;
024400120423
024500120423         //?Aggancio CNCLP
024600120423           keyclp.clpkut = 1;
024700120423           keyclp.clpkcc = 151;
024800120423           keyclp.clpksc = ksc;
024900120423
025000120423           chain %kds(keyclp) CNCLP00F;
025100120423         //?non trovo CNCLP NON aggiorno ma stampo
025200120423           IF  not %found(CNCLP00F);
025300120423             stperr = 'Non trovato CNLCP';
025400120423             exsr Stampa;
025500120423             iter;
025600120423           ENDIF;
025700120423         //?controllo commerciale su CNCLP
025800120423           clear ds01;
025900120423           k_TBLcod = '01';
026000120423           k_TBLkey = %editc(CLPage:'X');
026100120423           chain  %kds(K03tabel) TABEL00F;
026200120423         //?commerciale CNCLP NON valido
026300120423           IF  not %found(TABEL00F);
026400120423             stperr = 'Commerciale su CLP NON valido';
026500120423             exsr Stampa;
026600120423             iter;
026700120423           ENDIF;
026800120423         //?commerciale CNCLP valido
026900120423           ds01 = TBLuni;
027000120423           wdatadec = %dec(§01dtdec:8:0);
027100120423           wdatafin = %dec(§01dtfin:8:0);
027200120423         //?commerciale CNCLP NON INATTIVI o VARI e NON scaduto stampo e NON aggiorno
027300120423           IF  §01par = *blanks and wOggi < wdatafin and wOggi >= wdatadec;
027400120423             stperr = 'Commerciale su CLP valido';
027500120423             exsr Stampa;
027600120423             iter;
027700120423           ENDIF;
027800120423         //?controllo il commerciale del file di work
027900120423           clear ds01;
028000120423           k_TBLcod = '01';
028100120423           k_TBLkey = %editc(com:'X');
028200120423           chain  %kds(K03tabel) TABEL00F;
028300120423         //?commerciale NON valido NON aggiorno e stampo
028400120423           IF  not %found(TABEL00F) or TBLflg <> *blanks;
028500120423             stperr = 'Commerciale passato NON valido';
028600120423             exsr Stampa;
028700120423             iter;
028800120423           ENDIF;
028900120423           ds01 = TBLuni;
029000120423           wdatafin = %dec(§01dtfin:8:0);
029100120423         //?commerciale scaduto NON aggiorno e stampo
029200120423           IF  wOggi >= wdatafin;
029300120423             stperr = 'Commerciale passato scaduto';
029400120423             exsr Stampa;
029500120423             iter;
029600120423           ENDIF;
029700120423         //?commerciale con particolarità VARI
029800120423           IF  §01par = '1';
029900120423             stperr = 'Commerciale passato VARI';
030000120423             exsr Stampa;
030100120423             iter;
030200120423           ENDIF;
030300120423         //?commerciale con particolarità INATTIVI
030400120423           IF  §01par = '2';
030500120423             stperr = 'Commerciale passato INATTIVI';
030600120423             exsr Stampa;
030700120423             iter;
030800120423           ENDIF;
030900120423         //?Se arrivo qua tutto OK
031000120423         //?salvo l'immagine precedente dell'anagrafica
031100120423           clear TIBS73ds;
031200120423           IBS73immag = 'P';
031300120424           IBS73yda   = 'C';
031400120423           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
031500120423         //?Aggiorno il commerciale
031600120423           CLPage = com;
031700120423           update CNCLP000;
031800120423         //?Sfleggo la testata per inviare variazione a ProJ
031900120423           keyaco.acokut = clpkut;
032000120423           keyaco.acokcc = clpkcc;
032100120423           keyaco.acoksc = clpksc;
032200120423           chain %kds(keyclp) CNACO00F;
032300120423           IF  %found(CNACO00F);
032400120423             clear ACOdtr;
032500120423             clear ACOftr;
032600120423             update CNACO000;
032700120423           ENDIF;
032800120423
032900120423         //?Scrivo la variazione
033000120423           clear TIBS73ds ;
033100120423           IBS73pru   = 'EDPCED';
033200120423           IBS73noj   = 'CRM';
033300120423           IBS73pgm   = 'TNVRMB78';
033400120423           IBS73immag = 'D';
033500120423           IBS73cta   = 'M' ;
033600120424           IBS73yda   = 'C';
033700120423           TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
033800120423
033900120423         ENDDO;
034000120423
034100120423       ENDSR;
034200120423
034300120423       //--------------------------------------------------------------
034400120423       //?Stampa.
034500120423       //--------------------------------------------------------------
034600120423       BEGSR Stampa;
034700120423
034800120423         IF  not wStampa or *in99;
034900120423           write MB7801P;
035000120423           write MB7802P;
035100120423           wStampa = *on;
035200120423           *in99 = *off;
035300120423         ENDIF;
035400120423
035500120423       //?Aggancio CNACO
035600120423         clear ACOrag;
035700120423         clear ACOksc;
035800120423         keyaco.acokut = clpkut;
035900120423         keyaco.acokcc = clpkcc;
036000120423         keyaco.acoksc = clpksc;
036100120423         chain(n) %kds(keyaco) CNACO00F;
036200120423         IF  %found(CNACO00F);
036300120423           unlock CNCLP00F;
036400120423         ENDIF;
036500120423
036600120423         write MB7803P;
036700120423
036800120423       ENDSR;
036900120423
037000120423       //--------------------------------------------------------------
037100120423       //?Operazioni finali.
037200120423       //--------------------------------------------------------------
037300120423       BEGSR RoutEnd;
037400120423
037500120423         *inLR = *on;
037600120423         return;
037700120423
037800120423       ENDSR;
037900051221
038000051221      /end-free
038100051221
