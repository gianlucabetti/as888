000100110803      //--------------------------------------------------------------
000200110811      //?TRTB401R - Interrogazione tabella 3K - VAC
000300110803      //--------------------------------------------------------------
000400110803     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500110803
000600110803      //---------------------------------------------------------------
000700110803      //?Dichiarazione file.
000800110803      //---------------------------------------------------------------
000900110803
001000110803      // - Tabelle
001100110810     fTABEL00F  if   e           k disk
001200110803
001300110803      // - Video
001400110811     fTRTB401D  cf   e             workstn
001500110803
001600110803      //---------------------------------------------------------------
001700110803      //?Definizione costanti.
001800110803      //---------------------------------------------------------------
001900110803
002000110803      //---------------------------------------------------------------
002100110803      //?Definizione schiere.
002200110803      //---------------------------------------------------------------
002300110803
002400110803      //---------------------------------------------------------------
002500110803      //?Definizione aree dati.
002600110803      //---------------------------------------------------------------
002700110803
002800110803      // - Dati utente
002900110803     d �AzUte        e ds                  extname(AZUTE00F)
003000110803     d                                     dtaara
003100110803     d �DatiUte      e ds                  extname(dDatiUte)
003200110803     d                                     dtaara
003300110803
003400110803      //---------------------------------------------------------------
003500110803      //?Definizione strutture dati.
003600110803      //---------------------------------------------------------------
003700110803
003800110803      // - Parametri ricevuti
003900110803     d KPJBA         e ds
004000110803     d TNTA61DS      e ds
004100110803
004200110803      // - Reperimento dati utente
004300110803     d TIBS34DS      e ds
004400110803
004500110811      // - Tabella 3K
004600110811     d ds3K          e ds                  inz
004700110803
004800110803      //---------------------------------------------------------------
004900110803      //?Definizione variabili globali.
005000110803      //---------------------------------------------------------------
005100110803
005200110803      // - Flags booleani
005300110803     d $Fine           s               n   inz(*off)
005400110803     d $InzD01         s               n   inz(*on)
005500110803
005600110803      //---------------------------------------------------------------
005700110803      //?Definizione procedure esterne.
005800110803      //---------------------------------------------------------------
005900110803
006000110803      //---------------------------------------------------------------
006100110803      //?Prototipi.
006200110803      //---------------------------------------------------------------
006300110803
006400110803      /copy gaitrasrc/srcprotopr,tibs34r
006500110810      /copy gaitrasrc/srcprotopi,tibs69r
006600110810      /copy gaitrasrc/srcprotopr,tibs69r
006700110803
006800110803      //---------------------------------------------------------------
006900110803      //?Definizione key-list.
007000110803      //---------------------------------------------------------------
007100110803
007200110803      //---------------------------------------------------------------
007300110803      //?Riepilogo indicatori.
007400110803      //---------------------------------------------------------------
007500110803
007600110803      //---------------------------------------------------------------
007700110803
007800110803      //---------------------------------------------------------------
007900110803      //?M A I N - L I N E
008000110803      //---------------------------------------------------------------
008100110803
008200110803     c     *Entry        plist
008300110803     c                   parm                    KPJBA
008400110803     c                   parm                    TNTA61DS
008500110803
008600110803      /free
008700110803
008800110803       //?Operazioni iniziali
008900110803       exsr RoutInz;
009000110803
009100110803       //?Gestione video
009200110803       DOW $Fine = *off;
009300110803         exsr GesD01;
009400110803       ENDDO;
009500110803
009600110803       //?Operazioni finali
009700110803       exsr RoutEnd;
009800110803
009900110803       //--------------------------------------------------------------
010000110803       //?Operazioni iniziali.
010100110803       //--------------------------------------------------------------
010200110803       BEGSR RoutInz;
010300110803
010400110803         //?Reperimento dati job
010500110803         exsr DatiJob;
010600110805
010700110805         IF  TA61ksc <= *zeros;
010800110805           TA61err = *on;
010900110805           TA61msg = 'Richiesto il codice cliente';
011000110805           $Fine = *on;
011100110805         ENDIF;
011200110803
011300110803       ENDSR;
011400110803
011500110803       //--------------------------------------------------------------
011600110803       //?Reperimento Dati del job (Utente/Operativi).
011700110803       //--------------------------------------------------------------
011800110803       BEGSR DatiJob;
011900110803
012000110803         in(E) �AzUte;
012100110803         if NOT %error;
012200110803           in(E) �DatiUte;
012300110803         endif;
012400110803         if %error or RSut = *blanks;
012500110803           clear TIBS34ds;
012600110803           tibs34r(tibs34ds);
012700110803           in �AzUte;
012800110803           in �DatiUte;
012900110803         endif;
013000110803
013100110803       ENDSR;
013200110803
013300110803       //--------------------------------------------------------------
013400110803       //?Gestione videata.
013500110803       //--------------------------------------------------------------
013600110803       BEGSR GesD01;
013700110803
013800110803         //?Iniziallizazione videata
013900110803         IF $InzD01 = *on;
014000110803           exsr InzD01;
014100110803           $InzD01 = *off;
014200110803         ENDIF;
014300110803
014400110803       //?Emissione videata
014500110811         exfmt TB40D01;
014600110803
014700110803       //?F12 = Ritorno
014800110803         IF  *inkl;
014900110803           $fine = *on ;
015000110803           leavesr ;
015100110803         ENDIF;
015200110803
015300110803       ENDSR;
015400110803
015500110803       //--------------------------------------------------------------
015600110803       //?Caricamento videata D01.
015700110803       //--------------------------------------------------------------
015800110803       BEGSR InzD01;
015900110803
016000110810         clear V1Dksc;
016100110810         clear V1Dksu;
016200110812         clear V1Ccq1;
016300110812         clear V1Ccq2;
016400110812         clear V1Ccq3;
016500110812         clear V1Ccq4;
016600110812         clear V1Ccq5;
016700110812         clear V1Ccq8;
016800110812         clear V1Cvto;
016900110812         clear V1Cctb;
017000110810
017100110810       //?Decodifico il cliente
017200110810         clear  tibs69ds;
017300110810         I69kac = TA61ksc;
017400110810         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
017500110810         V1Cksc = TA61ksc;
017600110811         IF  O69err = *blanks;
017700110811           V1Dksc = ACORag;
017800110811         ELSE;
017900110811           V1Dksc = '? ? ? ? ? ?';
018000110811         ENDIF;
018100110810
018200110810       //?Aggancio la tabella
018300110811         clear ds3K;
018400110810         TBLkut = 1;
018500110811         TBLcod = '3K';
018600110810         TBLkey = %editc(V1Cksc:'X');
018700110810
018800110810         chain (TBLkut : TBLcod : TBLkey) TABEL00F;
018900110810         IF  %Found(TABEL00F) and TBLflg = *blanks;
019000110811
019100110811         ds3K = TBLuni;
019200110810
019300110810         //?Se presente decodifico l'unificante
019400110811           IF  �3Kcks <> 0;
019500110810             clear  tibs69ds;
019600110811             I69kac = �3Kcks;
019700110810             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
019800110811             V1Cksu = �3Kcks;
019900110811             IF  O69err = *blanks;
020000110811               V1Dksu = ACORag;
020100110811             ELSE;
020200110811               V1Dksu = '? ? ? ? ? ?';
020300110811             ENDIF;
020400110810           ELSE;
020500110810             clear V1Cksu;
020600110810           ENDIF;
020700110810
020800110811         //?Flag
020900110812           IF  �3Kcq1 = 'S';
021000110812             V1Ccq1 = 'SI';
021100110914           ELSE;
021200110914             V1Ccq1 = 'NO';
021300110812           ENDIF;
021400110812           IF  �3Kcq2 = 'S';
021500110812             V1Ccq2 = 'SI';
021600110914           ELSE;
021700110914             V1Ccq2 = 'NO';
021800110812           ENDIF;
021900110812           IF  �3Kcq3 = 'S';
022000110812             V1Ccq3 = 'SI';
022100110914           ELSE;
022200110914             V1Ccq3 = 'NO';
022300110812           ENDIF;
022400110812           IF  �3Kcq4 = 'S';
022500110812             V1Ccq4 = 'SI';
022600110914           ELSE;
022700110914             V1Ccq4 = 'NO';
022800110812           ENDIF;
022900110812           IF  �3Kcq5 = 'S';
023000110812             V1Ccq5 = 'SI';
023100110914           ELSE;
023200110914             V1Ccq5 = 'NO';
023300110812           ENDIF;
023400110812           IF  �3Kcq8 = 'S';
023500110812             V1Ccq8 = 'SI';
023600110914           ELSE;
023700110914             V1Ccq8 = 'NO';
023800110812           ENDIF;
023900110812           IF  �3Kvto = 'S';
024000110812             V1Cvto = 'SI';
024100110914           ELSE;
024200110914             V1Cvto = 'NO';
024300110812           ENDIF;
024400110812           IF  �3Kctb = 'S';
024500110812             V1Cctb = 'SI';
024600110914           ELSE;
024700110914             V1Cctb = 'NO';
024800110812           ENDIF;
024900110810
025000110810         ENDIF;
025100110803
025200110803       ENDSR;
025300110803
025400110803       //--------------------------------------------------------------
025500110803       //?Operazioni finali.
025600110803       //--------------------------------------------------------------
025700110803       BEGSR RoutEnd;
025800110803
025900110803         *inLR = *on;
026000110803         return;
026100110803
026200110803       ENDSR;
