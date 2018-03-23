000100120808      //==============================================================
000200120808      //?TNTAA1R - Controlla Abilitazione Utente - Media
000300120808      //==============================================================
000400120808
000500120808     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120808
000700120808      //---------------------------------------------------------------
000800120808      //?Dichiarazione file.
000900120808      //---------------------------------------------------------------
001000120808
001100120808      //---------------------------------------------------------------
001200120808      //?Definizione costanti.
001300120808      //---------------------------------------------------------------
001400120808
001500120808      //---------------------------------------------------------------
001600120808      //?Definizione schiere.
001700120808      //---------------------------------------------------------------
001800120808
001900120808      //---------------------------------------------------------------
002000120808      //?Definizione aree dati.
002100120808      //---------------------------------------------------------------
002200120808
002300120808       // -?Dati utente
002400120808     d §AzUte        e ds                  extname(AZUTE00F)
002500120808     d                                     dtaara
002600120808     d §DatiUte      e ds                  extname(dDatiUte)
002700120808     d                                     dtaara
002800120808
002900120808      //---------------------------------------------------------------
003000120808      //?Definizione strutture dati.
003100120808      //---------------------------------------------------------------
003200120808
003300120808       // -?Paremetri ricevuti
003400120808     d KPJBA         e ds
003500120808
003600120808      // -?Ricerca/Controllo tabelle
003700120808     d TIBS02ds      e ds                  inz
003800120808
003900120808      // -?Reperimento dati utente
004000120808     d TIBS34ds      e ds
004100120808
004200120810       // -?Controllo abilitazioni utente
004300120810     d TNTAA1ds      e ds                  inz
004400120810
004500120810       // -?DS di appoggio da passare al TNTAA2R x recupero ABILITAZIONE
004600120810       // -?del profilo utente
004700120810     d TNTAA2ds      e ds                  extname(TNTAA1ds)
004800120810     d                                     prefix(w)
004900120808
005000120808       // -?Tabella autorizzazioni LAT
005100120808     d dLAT          e ds                  inz
005200120808
005300120808      //---------------------------------------------------------------
005400120808      //?Definizione variabili globali.
005500120808      //---------------------------------------------------------------
005600120810
005700120810       // -?Flags booleani
005800120810     d wFine           s               n   inz(*off)
005900120808
006000120808       // -?Campi di comodo
006100120808     d wABIpgm         s                   like(UteAut)
006200120808     d wABIprf         s                   like(UteAut)
006300120808     d wLATlivPGM      s                   like(§LATliv)
006400120808     d wLATlivPRF      s                   like(§LATliv)
006500120808
006600120808      //---------------------------------------------------------------
006700120808      //?Definizione procedure esterne.
006800120808      //---------------------------------------------------------------
006900120810
007000120810       // -?Reperimento abiltiazione utente
007100120810     d tntaa2r         pr                  extpgm('TNTAA2R')
007200120810     d  kpjba                              likeds(kpjba)
007300120808
007400120808      //---------------------------------------------------------------
007500120808      //?Definizione prototipi.
007600120808      //---------------------------------------------------------------
007700120808
007800120810       // -?Reperimento tabelle
007900120808      /copy gaitrasrc/srcprotopr,tibs02r
008000120808
008100120808       // -?Reperimento dati utente
008200120808      /copy gaitrasrc/srcprotopr,tibs34r
008300120808
008400120808      //---------------------------------------------------------------
008500120808      //?Definizione key-list.
008600120808      //---------------------------------------------------------------
008700120808
008800120808      //---------------------------------------------------------------
008900120808      //?Riepilogo indicatori.
009000120808      //---------------------------------------------------------------
009100120808
009200120808      //---------------------------------------------------------------
009300120808
009400120808      //---------------------------------------------------------------
009500120808      //?M A I N - L I N E
009600120808      //---------------------------------------------------------------
009700120808
009800120808     c     *Entry        plist
009900120808     c                   parm                    KPJBA
010000120808
010100120808      /free
010200120808
010300120808       //?Operazioni iniziali
010400120808       exsr RoutInz;
010500120808
010600120808       //?Elaborazione
010700120810       IF  not wFine;
010800120810         exsr Elabora;
010900120810       ENDIF;
011000120808
011100120808       //?Operazioni finali
011200120808       exsr RoutEnd;
011300120808
011400120808       //--------------------------------------------------------------
011500120808       //?Operazioni iniziali.
011600120808       //--------------------------------------------------------------
011700120808       BEGSR RoutInz;
011800120810
011900120810       //?Imposto la DS del chiamante
012000120810         TNTAA1ds = kpjbu;
012100120810
012200120810       //?Se non c'è l'abilitazione forzata e ITAA1tipo = 'N'
012300120810       //?esco dal pgm è un vecchio richiamo non ancora sistemato
012400120810       //?così va avanti con i controlli nel tntaa2r
012500120810         IF  ITAA1abi = *blanks and ITAA1tipo = 'N';
012600120810           wFine = *on;
012700120810           leavesr;
012800120810         ENDIF;
012900120810
013000120810       //?Per prima cosa verifico la presenza dell'abilitazione forzata
013100120810         IF  ITAA1abi = *blanks;
013200120810         //?se non passata è come se l'utente non fosse abilitato
013300120810         //?questo perchè non posso avere il flag della forzatura impostato
013400120810         //?senza nessuna abilitazione forzata presente
013500120810           OTAA1fabi = 'N';
013600120810           wFine = *on;
013700120810           leavesr;
013800120810         ENDIF;
013900120810
014000120810       //?Se ho l'abilitazione ma il campo ITAA1TIPO non è M o F errore
014100120810         IF  ITAA1tipo <> 'M' and ITAA1tipo <> 'F';
014200120810           OTAA1fabi = 'N';
014300120810           wFine = *on;
014400120810           leavesr;
014500120810         ENDIF;
014600120810
014700120810       //?Se richiamo con 'M' ma non ho l'abilitazione del profilo
014800120810       //?da controllare errore
014900120810         IF  ITAA1tipo = 'M' and ITAA1caut = *blanks;
015000120810           OTAA1fabi = 'N';
015100120810           wFine = *on;
015200120810           leavesr;
015300120810         ENDIF;
015400120810
015500120810       //?Se forzatura e basta posso già andare a fine
015600120810         IF  ITAA1tipo = 'F';
015700120810           wFine = *on;
015800120810           leavesr;
015900120810         ENDIF;
016000120808
016100120808       //?Reperimento dati job
016200120808         exsr DatiJob;
016300120808
016400120808       ENDSR;
016500120808
016600120808       //--------------------------------------------------------------
016700120808       //?Reperimento Dati del job (Utente/Operativi).
016800120808       //--------------------------------------------------------------
016900120808       BEGSR DatiJob;
017000120808
017100120808         in(E) §AzUte;
017200120808         IF NOT %error;
017300120808           in(E) §DatiUte;
017400120808         ENDIF;
017500120808         IF %error or RSut = *blanks;
017600120808           clear TIBS34ds;
017700120808           tibs34r(tibs34ds);
017800120808           in §AzUte;
017900120808           in §DatiUte;
018000120808         ENDIF;
018100120808
018200120808       ENDSR;
018300120809
018400120809       //--------------------------------------------------------------
018500120809       //?Elabora.
018600120809       //--------------------------------------------------------------
018700120809       BEGSR Elabora;
018800120810
018900120810       //?Richiamo il TNTAA2R per recuperare l'abilitazione dell'utente
019000120810         clear TNTAA2ds;
019100120810         wITAA1caut = ITAA1caut;
019200120810         kpjbu = TNTAA2ds;
019300120810         tntaa2r (kpjba);
019400120810         TNTAA2ds = kpjbu;
019500120810
019600120810       //?se non abilitato esco dal pgm
019700120810         IF  wOTAA1fabi = 'N';
019800120810           OTAA1fabi = 'N';
019900120810           wFine = *on;
020000120810           leavesr;
020100120810         endif;
020200120810
020300120810       //?Cerco il livello dell'abilitazione del profilo
020400120810         clear dLAT;
020500120810         clear TIBS02ds;
020600120810         T02mod = 'C';
020700120810         T02cod = 'LAT';
020800120810         T02ke1 = wOTAA1cabi;
020900120810         T02sif = KNSIF;
021000120810         TNTBE_RicercaControllo  (kpjba : tibs02ds);
021100120810         IF  T02err = *blanks;
021200120810           dLAT = T02uni;
021300120810         ENDIF;
021400120810         wLATlivPRF = §LATliv;
021500120809
021600120810       //?Cerco il livello dell'abilitazione forzata del PGM
021700120809         clear dLAT;
021800120809         clear TIBS02ds;
021900120809         T02mod = 'C';
022000120809         T02cod = 'LAT';
022100120810         T02ke1 = ITAA1abi;
022200120809         T02sif = KNSIF;
022300120809         TNTBE_RicercaControllo  (kpjba : tibs02ds);
022400120809         IF  T02err = *blanks;
022500120809           dLAT = T02uni;
022600120809         ENDIF;
022700120809         wLATlivPGM = §LATliv;
022800120810
022900120810       //?Se l'abilitazione del profilo ha un livello più alto rispetto
023000120810       //?a quella richiesta dal pgm imposto l'abilitazione del profilo
023100120810         IF  wLATlivPRF > wLATlivPGM;
023200120810           ITAA1abi = wOTAA1cabi;
023300120810         ENDIF;
023400120809
023500120809       ENDSR;
023600120808
023700120808       //--------------------------------------------------------------
023800120808       //?Operazioni finali.
023900120808       //--------------------------------------------------------------
024000120808       BEGSR RoutEnd;
024100120810
024200120810       //?ritorno la ds
024300120810         kpjbu = TNTAA1ds;
024400120808
024500120808         *inLR = *on;
024600120808         return;
024700120808
024800120808       ENDSR;
024900120808
025000120808      /end-free
