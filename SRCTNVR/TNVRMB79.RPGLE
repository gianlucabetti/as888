000100120423      //---------------------------------------------------------------
000200120423      //
000300120430      //?              Mod. Potenziale da file WORK
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
001500120423
001600120423      // - File di WORK
001700120430     fFILEANA3  if   e             disk
001800120430
001900120430      // - File potenziali
002000120430     fTNCPO01L  if   e           k disk
002100120423
002200120423      // - File tabelle
002300120430     fTNVRMB79P o    e             printer oflind(*in99)
002400120423
002500120423      //---------------------------------------------------------------
002600120423      //?Definizione costanti.
002700120423      //---------------------------------------------------------------
002800120423
002900120423      //---------------------------------------------------------------
003000120423      //?Definizione schiere.
003100120423      //---------------------------------------------------------------
003200120423
003300120423      //---------------------------------------------------------------
003400120423      //?Definizione aree dati.
003500120423      //---------------------------------------------------------------
003600120423
003700120423      //---------------------------------------------------------------
003800120423      //?Definizione strutture dati.
003900120423      //---------------------------------------------------------------
004000120423       // - Reperimento dati anagrafici
004100120423     d DS_cnaco      e ds                  inz extname(CNACO00F) prefix(w_)
004200120423     d DS_cnind      e ds                  inz extname(CNIND00F) prefix(w_)
004300120423     d DS_cnclp      e ds                  inz extname(CNCLP00F) prefix(w_)
004400120423     d DS_fncls      e ds                  inz extname(FNCLS00F) prefix(w_)
004500120423
004600120423      // - Registra variazioni
004700120423     d TIBS73ds      e ds                  inz
004800120423     d cnaco_73      e ds                  inz  extname(Cnaco00f)
004900120423     d cnind_73      e ds                  inz  extname(Cnind00f)
005000120423     d cnclp_73      e ds                  inz  extname(Cnclp00f)
005100120423     d fncls_73      e ds                  inz  extname(Fncls00f)
005200120423
005300120423     d keyaco          ds                  likerec(cnaco000:*key)
005400120423
005500120423      //---------------------------------------------------------------
005600120423      //?Definizione variabili globali.
005700120423      //---------------------------------------------------------------
005800120423      // - Flags booleani
005900120423     d wEoF            s               n   inz(*off)
006000120423     d wStampa         s               n   inz(*off)
006100120423
006200120423      // - Campi di comodo
006300120423     d wOggi           s              8  0
006400120423     d wdatadec        s              8  0
006500120423     d wdatafin        s              8  0
006600120423
006700120423      //---------------------------------------------------------------
006800120423      //?Definizione procedure esterne.
006900120423      //---------------------------------------------------------------
007000120423
007100120423      //---------------------------------------------------------------
007200120423      //?Definizione prototipi.
007300120423      //---------------------------------------------------------------
007400120423
007500120423      // - Memorizzo variazioni
007600120423      /copy gaitrasrc/srcprotopr,tibs73r
007700120423
007800120423      //---------------------------------------------------------------
007900120423      //?Definizione key-list.
008000120423      //---------------------------------------------------------------
008100120423
008200120423      //---------------------------------------------------------------
008300120423      //?Riepilogo indicatori.
008400120423      //---------------------------------------------------------------
008500120423
008600120423      //---------------------------------------------------------------
008700120423      //?M A I N - L I N E
008800120423      //---------------------------------------------------------------
008900120423
009000120423      /free
009100120423
009200120423       //?Operazioni iniziali
009300120423       exsr RoutInz;
009400120423
009500120430       //?Elabora file
009600120430       exsr Elabora;
009700120423
009800120423       //?Operazioni finali
009900120423       exsr RoutEnd;
010000120423
010100120423       //--------------------------------------------------------------
010200120423       //?Operazioni iniziali.
010300120423       //--------------------------------------------------------------
010400120423       BEGSR RoutInz;
010500120423
010600120423         wOggi    = %dec(%date());
010700120423
010800120423       ENDSR;
010900120423
011000120423       //--------------------------------------------------------------
011100120430       //?Elabora file
011200120423       //--------------------------------------------------------------
011300120430       BEGSR Elabora;
011400120423
011500120423         wEoF = *off;
011600120423
011700120423       //?Leggo il file di work
011800120423         DOW  not wEoF;
011900120430           read FILEANA3;
012000120423           IF  %eof;
012100120423             wEoF = *on;
012200120423             leavesr;
012300120423           ENDIF;
012400120424           clear cnaco_73;
012500120423
012600120423         //?Aggancio CNCLP
012700120430           keyaco.acokut = 1;
012800120430           keyaco.acokcc = 151;
012900120430           keyaco.acoksc = ksc;
013000051223
013100120430           chain %kds(keyaco) CNACO00F;
013200120430         //?non trovo CNACO NON aggiorno ma stampo
013300120430           IF  not %found(CNACO00F);
013400120430             stperr = 'Non trovato CNACO';
013500120423             exsr Stampa;
013600120423             iter;
013700120423           ENDIF;
013800120430         //?controllo potenziale su CNACO
013900120430           IF ACOlib > 0;
014000120430             chain ACOlib TNCPO01L;
014100120430           //?potenziale presente e valido
014200120430           //?NON aggiorno e stampo
014300120430             IF  %found(TNCPO01L);
014400120430               stperr = 'Potenziale presente su CNACO e valido';
014500120430               exsr Stampa;
014600120430               iter;
014700120430             ENDIF;
014800120430           //?potenziale presente e NON valido
014900120430           //?NON aggiorno e stampo
015000120430             IF  not %found(TNCPO01L);
015100120430               stperr = 'Potenziale presente su CNACO NON valido';
015200120430               exsr Stampa;
015300120430               iter;
015400120430             ENDIF;
015500120423           ENDIF;
015600120430         //?controllo potenziale del file di work
015700120430           chain pot TNCPO01L;
015800120430           IF  not %found(TNCPO01L);
015900120430           //?potenziale NON valido
016000120430           //?NON aggiorno e stampo
016100120430             stperr = 'Potenziale passato NON valido';
016200120423             exsr Stampa;
016300120423             iter;
016400120423           ENDIF;
016500120423         //?Se arrivo qua tutto OK
016600120423         //?salvo l'immagine precedente dell'anagrafica
016700120430           //clear TIBS73ds;
016800120430           //IBS73immag = 'P';
016900120430           //IBS73yda   = 'A';
017000120430           //TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
017100120430         //?Aggiorno il potenziale
017200120430           ACOlib = pot;
017300120430           clear ACOdtr;
017400120430           clear ACOftr;
017500120430           update CNACO000;
017600120423
017700120423         //?Scrivo la variazione
017800120430           //clear TIBS73ds ;
017900120430           //IBS73pru   = 'EDPCED';
018000120430           //IBS73noj   = 'CRM';
018100120430           //IBS73pgm   = 'TNVRMB78';
018200120430           //IBS73immag = 'D';
018300120430           //IBS73cta   = 'M' ;
018400120430           //IBS73yda   = 'C';
018500120430           //TIBS73R (tibs73ds:cnaco_73:cnind_73:cnclp_73:fncls_73);
018600051223
018700120423         ENDDO;
018800051222
018900120423       ENDSR;
019000120423
019100120423       //--------------------------------------------------------------
019200120423       //?Stampa.
019300120423       //--------------------------------------------------------------
019400120423       BEGSR Stampa;
019500120423
019600120423         IF  not wStampa or *in99;
019700120430           write MB7901P;
019800120430           write MB7902P;
019900120423           wStampa = *on;
020000120423           *in99 = *off;
020100120423         ENDIF;
020200120423
020300120430         write MB7903P;
020400120423
020500120423       ENDSR;
020600120423
020700120423       //--------------------------------------------------------------
020800120423       //?Operazioni finali.
020900120423       //--------------------------------------------------------------
021000120423       BEGSR RoutEnd;
021100120423
021200120423         *inLR = *on;
021300120423         return;
021400120423
021500120423       ENDSR;
021600051221
021700051221      /end-free
021800051221
