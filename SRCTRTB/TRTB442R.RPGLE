000100110803      //--------------------------------------------------------------
000200110811      //?TRTB442R - Interrogazione tabella 4C - fatture
000300110803      //--------------------------------------------------------------
000400110803     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500110803
000600110803      //---------------------------------------------------------------
000700110803      //?Dichiarazione file.
000800110803      //---------------------------------------------------------------
000900110803
001000110803      // - Tabelle
001100120207     fTABEL00F  if   e           k disk    usropn
001200120207     f                                     extfile(wLibTab)
001300120112      // - Regole cliente x scambio dati
001400120112     fTIVIR06L  if   e           k disk    usropn
001500120112     f                                     extfile(wLibVir)
001600110803
001700110803      // - Video
001800110811     fTRTB442D  cf   e             workstn
001900110803
002000110803      //---------------------------------------------------------------
002100110803      //?Definizione costanti.
002200110803      //---------------------------------------------------------------
002300110803
002400110803      //---------------------------------------------------------------
002500110803      //?Definizione schiere.
002600110803      //---------------------------------------------------------------
002700110803
002800110803      //---------------------------------------------------------------
002900110803      //?Definizione aree dati.
003000110803      //---------------------------------------------------------------
003100110803
003200110803      // - Dati utente
003300110803     d �AzUte        e ds                  extname(AZUTE00F)
003400110803     d                                     dtaara
003500110803     d �DatiUte      e ds                  extname(dDatiUte)
003600110803     d                                     dtaara
003700110803
003800110803      //---------------------------------------------------------------
003900110803      //?Definizione strutture dati.
004000110803      //---------------------------------------------------------------
004100110803
004200110803      // - Parametri ricevuti
004300110803     d KPJBA         e ds
004400110803     d TNTA61DS      e ds
004500120112
004600120112      // - Ricerca/Controllo tabelle
004700120112     d TIBS02ds      e ds                  inz
004800110803
004900110803      // - Reperimento dati utente
005000110803     d TIBS34DS      e ds
005100120112
005200120112      // - Tabella ISV
005300120112     d dISV          e ds                  inz
005400110803
005500110811      // - Tabella 4C
005600110811     d ds4C          e ds                  inz
005700110803
005800110803      //---------------------------------------------------------------
005900110803      //?Definizione variabili globali.
006000110803      //---------------------------------------------------------------
006100110803
006200110803      // - Flags booleani
006300110803     d $Fine           s               n   inz(*off)
006400110803     d $InzD01         s               n   inz(*on)
006500120112
006600120112      // - Nome libreria file tivir00f
006700120112     d wLibVir         s             21a   inz
006800120207
006900120207      // - Nome libreria file tabel00f
007000120207     d wLibTab         s             21a   inz
007100120112
007200120112      // - Campi di comodo
007300120112     d wTipoServ       s                   like(VIRfi1)
007400120112     d w008a           s              8a   inz
007500120112
007600120112      // - Campi di comodo x date
007700120112     d wOggi           s              8s 0 inz
007800110803
007900110803      //---------------------------------------------------------------
008000110803      //?Definizione procedure esterne.
008100110803      //---------------------------------------------------------------
008200110803
008300110803      //---------------------------------------------------------------
008400110803      //?Prototipi.
008500110803      //---------------------------------------------------------------
008600110803
008700120112      /copy gaitrasrc/srcprotopr,tibs02r
008800110803      /copy gaitrasrc/srcprotopr,tibs34r
008900110810      /copy gaitrasrc/srcprotopi,tibs69r
009000110810      /copy gaitrasrc/srcprotopr,tibs69r
009100110803
009200110803      //---------------------------------------------------------------
009300110803      //?Definizione key-list.
009400110803      //---------------------------------------------------------------
009500110803
009600110803      //---------------------------------------------------------------
009700110803      //?Riepilogo indicatori.
009800110803      //---------------------------------------------------------------
009900110803
010000110803      //---------------------------------------------------------------
010100110803
010200110803      //---------------------------------------------------------------
010300110803      //?M A I N - L I N E
010400110803      //---------------------------------------------------------------
010500110803
010600110803     c     *Entry        plist
010700110803     c                   parm                    KPJBA
010800110803     c                   parm                    TNTA61DS
010900110803
011000110803      /free
011100110803
011200110803       //?Operazioni iniziali
011300110803       exsr RoutInz;
011400110803
011500110803       //?Gestione video
011600110803       DOW $Fine = *off;
011700110803         exsr GesD01;
011800110803       ENDDO;
011900110803
012000110803       //?Operazioni finali
012100110803       exsr RoutEnd;
012200110803
012300110803       //--------------------------------------------------------------
012400110803       //?Operazioni iniziali.
012500110803       //--------------------------------------------------------------
012600110803       BEGSR RoutInz;
012700110803
012800110803         //?Reperimento dati job
012900110803         exsr DatiJob;
013000110805
013100110805         IF  TA61ksc <= *zeros;
013200110805           TA61err = *on;
013300110805           TA61msg = 'Richiesto il codice cliente';
013400110805           $Fine = *on;
013500110805         ENDIF;
013600120112
013700120112       //?Controllo in che sistema informativo sono
013800120112       //?S.I. di Prova o S.I. di Produzione
013900120112         IF  (%subst(knsif : 7 : 1) = 'P');
014000120207         //?Imposto la libreria per file TIVIR e TABEL
014100120112             wLibVir = 'GAITRAGRPS/TIVIR06L';
014200120207             wLibTab = 'GAITRAGRPS/TABEL00F';
014300120112         ELSE;
014400120112             wLibVir = 'GAITRAGRU/TIVIR06L';
014500120207             wLibTab = 'GAITRAGRU/TABEL00F';
014600120112         ENDIF;
014700120112
014800120207       //?Apro i file
014900120112         open  TIVIR06L;
015000120207         open  TABEL00F;
015100120112
015200120112       //?Imposto la data del giorno
015300120112         wOggi = %dec(%date());
015400110803
015500110803       ENDSR;
015600110803
015700110803       //--------------------------------------------------------------
015800110803       //?Reperimento Dati del job (Utente/Operativi).
015900110803       //--------------------------------------------------------------
016000110803       BEGSR DatiJob;
016100110803
016200110803         in(E) �AzUte;
016300110803         if NOT %error;
016400110803           in(E) �DatiUte;
016500110803         endif;
016600110803         if %error or RSut = *blanks;
016700110803           clear TIBS34ds;
016800110803           tibs34r(tibs34ds);
016900110803           in �AzUte;
017000110803           in �DatiUte;
017100110803         endif;
017200110803
017300110803       ENDSR;
017400110803
017500110803       //--------------------------------------------------------------
017600110803       //?Gestione videata.
017700110803       //--------------------------------------------------------------
017800110803       BEGSR GesD01;
017900110803
018000110803         //?Iniziallizazione videata
018100110803         IF $InzD01 = *on;
018200110803           exsr InzD01;
018300110803           $InzD01 = *off;
018400110803         ENDIF;
018500110803
018600110803       //?Emissione videata
018700110811         exfmt TB44D01;
018800110803
018900110803       //?F12 = Ritorno
019000110803         IF  *inkl;
019100110803           $fine = *on ;
019200110803           leavesr ;
019300110803         ENDIF;
019400110803
019500110803       ENDSR;
019600110803
019700110803       //--------------------------------------------------------------
019800110803       //?Caricamento videata D01.
019900110803       //--------------------------------------------------------------
020000110803       BEGSR InzD01;
020100110803
020200110810         clear V1Dksc;
020300110810         clear V1Dksu;
020400120112         clear V1Disv;
020500110810
020600110810       //?Decodifico il cliente
020700110810         clear  tibs69ds;
020800110810         I69kac = TA61ksc;
020900110810         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
021000110810         V1Cksc = TA61ksc;
021100110811         IF  O69err = *blanks;
021200110811           V1Dksc = ACORag;
021300110811         ELSE;
021400110811           V1Dksc = '? ? ? ? ? ?';
021500110811         ENDIF;
021600110810
021700110810       //?Aggancio la tabella
021800110811         clear ds4C;
021900110810         TBLkut = 1;
022000110811         TBLcod = '4C';
022100110810         TBLkey = %editc(V1Cksc:'X');
022200110810
022300110810         chain (TBLkut : TBLcod : TBLkey) TABEL00F;
022400110810         IF  %Found(TABEL00F) and TBLflg = *blanks;
022500110811
022600110811           ds4C = TBLuni;
022700110810
022800110810         //?Se presente decodifico l'unificante
022900110907           IF  �4Cfks <> 0;
023000110810             clear  tibs69ds;
023100110907             I69kac = �4Cfks;
023200110810             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
023300110907             V1Cksu = �4Cfks;
023400110811             IF  O69err = *blanks;
023500110811               V1Dksu = ACORag;
023600110811             ELSE;
023700110811               V1Dksu = '? ? ? ? ? ?';
023800110811             ENDIF;
023900110810           ELSE;
024000110810             clear V1Cksu;
024100110810           ENDIF;
024200120112
024300120112         //?Cerco la modalit� invio dati
024400120112           IF  V1Cksu > 0;
024500120112             clear wTipoServ;
024600120112             w008a = *all'0';
024700120112             %subst(w008a:2:7) = %editc(V1Cksu:'X');
024800120112             setll (w008a : 'VF') TIVIR06L;
024900120112             reade (w008a : 'VF') TIVIR06L;
025000120112             DOW  not %eof(TIVIR06L);
025100120112               IF  VIRdtf >= woggi;
025200120112                 wTipoServ = VIRfi1;
025300120112               ENDIF;
025400120112               reade (w008a : 'VF') TIVIR06L;
025500120112             ENDDO;
025600120112           //?Decodifico la modalit� invio dati
025700120112             clear TIBS02ds;
025800120112             T02mod = 'C';
025900120112             T02cod = 'ISV';
026000120112             T02ke1 = wTipoServ;
026100120112             T02sif = KNSIF;
026200120112             TNTBE_RicercaControllo  (kpjba : tibs02ds);
026300120112             IF  T02err = *blanks;
026400120112               dISV = T02Uni;
026500120112             ENDIF;
026600120112             V1Disv = �ISVdesc;
026700120112           ENDIF;
026800110810
026900110810         ENDIF;
027000110803
027100110803       ENDSR;
027200110803
027300110803       //--------------------------------------------------------------
027400110803       //?Operazioni finali.
027500110803       //--------------------------------------------------------------
027600110803       BEGSR RoutEnd;
027700110803
027800110803         *inLR = *on;
027900110803         return;
028000110803
028100110803       ENDSR;
