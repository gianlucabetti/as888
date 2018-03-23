000100091029      //---------------------------------------------------------------
000200110201      //?TRMK05R - Calcolo la categoria del potenziale
000300091029      //---------------------------------------------------------------
000400091029
000500091029     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600091029     h dftactgrp(*no) actgrp(*caller)
000700091029
000800091029      //---------------------------------------------------------------
000900091029      //?Dichiarazione file.
001000091029      //---------------------------------------------------------------
001100101217      // - File Anagrafica clienti
001200101217     fCNACO16L  if   e           k disk
001300091029
001400091029      //---------------------------------------------------------------
001500091029      //?Definizione costanti.
001600091029      //---------------------------------------------------------------
001700091029
001800091029      //---------------------------------------------------------------
001900091029      //?Definizione schiere.
002000091029      //---------------------------------------------------------------
002100091029
002200091029      //---------------------------------------------------------------
002300091029      //?Definizione aree dati.
002400091029      //---------------------------------------------------------------
002500091029      //---------------------------------------------------------------
002600091029      //?Definizione strutture dati.
002700091029      //---------------------------------------------------------------
002800101216
002900091029      // - Parametri ricevuti
003000091029     d KPJBA         e ds
003100110131     d TRMK05ds      e ds
003200091029
003300110201      // File Attività
003400110201     d tiatcDS       e ds                  ExtName(tiatc00f)
003500110201
003600110201      // File Trattative
003700110201     d TIVIS00F      e ds                  extname(TIVIS00F)
003800110201
003900091029      //---------------------------------------------------------------
004000091029      //?Definizione variabili globali.
004100091029      //---------------------------------------------------------------
004200091029
004300091029      // - Flags booleani
004400101217     d $AcoBlc         s               n   inz(*off)
004500091029     d $End            s               n   inz(*off)
004600110201     d $EndAtc         s               n   inz(*off)
004700091029     d $Fine           s               n   inz(*off)
004800101216
004900101216      // - Indici di schiera
005000101216     d xx              s              4  0 inz
005100091029
005200091029      // - Campi di comodo
005300101217
005400110131     d wNrAtc          s              5i 0
005500110131     d wNrVis          s              5i 0
005600110131
005700110131       // - Nome libreria del file TIVIS
005800110131     d wLibVis         s             10    inz
005900110131
006000110131       // - Stringhe SQL da eseguire
006100110131     d wSQL            s           2048    Varying        inz
006200110131
006300091029      //---------------------------------------------------------------
006400091029      //?Definizione procedure esterne.
006500091029      //---------------------------------------------------------------
006600101216
006700110131
006800101216      //---------------------------------------------------------------
006900101216      //?Definizione prototipi.
007000101216      //---------------------------------------------------------------
007100091029
007200091029      //---------------------------------------------------------------
007300091029      //?Definizione key-list.
007400091029      //---------------------------------------------------------------
007500091029
007600091029      //---------------------------------------------------------------
007700091029      //?Riepilogo indicatori.
007800091029      //---------------------------------------------------------------
007900091029
008000091029      //---------------------------------------------------------------
008100091029      //?M A I N - L I N E
008200091029      //---------------------------------------------------------------
008300091029
008400091029     c     *Entry        plist
008500091029     c                   parm                    KPJBA
008600110131     c                   parm                    TRMK05ds
008700091029
008800091029      /free
008900091029
009000091029       //?Operazioni iniziali
009100101216       exsr RoutInz;
009200101217
009300101217       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009400110131
009500101216       //?Elabora
009600091029       DOW  $Fine = *off;
009700101216         exsr Elabora;
009800091029       ENDDO;
009900091029
010000091029       //?Operazioni finali
010100101216       exsr RoutEnd;
010200091029
010300091029       //--------------------------------------------------------------
010400091029       //?Operazioni iniziali.
010500091029       //--------------------------------------------------------------
010600101216       BEGSR RoutInz;
010700101216
010800110131         clear OMK05err;
010900091029
011000101216         //?Se Non ho il potenziale esco dal pgm con errore
011100110131         IF  IMK05CPO = 0;
011200110131           OMK05err = '1';
011300091029           $Fine = *on;
011400091029           leavesr;
011500091029         ENDIF;
011600091029
011700091029       ENDSR;
011800101217
011900101216       //--------------------------------------------------------------
012000101216       //?Elabora.
012100101216       //--------------------------------------------------------------
012200101216       BEGSR Elabora;
012300101217
012400101216         //?Cerco tutti i clienti con il potenziale ricevuto
012500101217         $AcoBlc = *off;
012600101217         $End = *off;
012700110131         setll IMK05cpo CNACO16L;
012800110131
012900110131         //?Nessun KSC associato il potenziale è in Categoria mai codificato
013000110131         IF  not %Equal(CNACO16L);
013100110131           OMK05cat = 'M';
013200110407           exsr Sr_elimina ;
013300101216           $Fine = *on;
013400101216           leavesr;
013500101216         ENDIF;
013600101216
013700110201         //?Se tutti KSC associati bloccati oppure no
013800110131         reade IMK05cpo CNACO16L;
013900110131         DOW not %eof(CNACO16L) ;
014000110131           //?Cliente bloccato
014100101216             IF  ACOabl <> *blanks;
014200101217               $AcoBlc = *on;
014300110131             else ;
014400110131           //?almeno un codice Cliente non bloccato Potenziale caregoria Codificato
014500110131               $acoBlc = *off ;
014600110131               OMK05cat = 'C';
014700110131               $Fine = *on;
014800110131               leavesr;
014900101216             ENDIF;
015000110131
015100110131           reade IMK05cpo CNACO16L;
015200110131         ENDDO  ;
015300110131
015400110131         //?Se tutti i codici collegati sono bloccati bisogna verificare se ce ne è
015500110131
015600110131         If  $AcoBlc = *on ;
015700110131               OMK05cat = 'P';
015800110407               exsr Sr_elimina ;
015900110131               $Fine = *on;
016000110131               leavesr;
016100110131         Endif ;
016200101216
016300101216       ENDSR;
016400110131
016500110131       //--------------------------------------------------------------
016600110131       //?Verifico se Potenziale Eliminabile
016700110131       //--------------------------------------------------------------
016800110131       BEGSR Sr_elimina ;
016900110131
017000110131         //?Verifico se il potenziale Perso o mai codificato è ache eliminabile
017100110131
017200110201         //?Controllo se ci sono trattative aperte sul potenziale
017300110201         //?se ne trovo vuol dire che il potenizale non è eliminabile
017400110201         $End = *off;
017500110411         $Endatc = *off;
017600110201         clear wNrVis;
017700110201         clear wSQL;
017800110201         wSQL = 'select VISnrv from ';
017900110201         IF  %subst(knsif : 7 : 1) = 'P';
018000110201           wSQL += 'FILTRAGRPF/TIVIS00F ';
018100110201         ELSE;
018200110201           wSQL += 'FILTRAGRU/TIVIS00F ';
018300110201         ENDIF;
018400110201         wSQL += 'where VIScpo = ' + %editc(IMK05cpo:'X') + ' ' +
018500110201                 'and VISdch = 0 and VISffz = '' ''';
018600110201         exec sql prepare F1 from :wSQL;
018700110201         exec sql declare VIS cursor for F1;
018800110201         exec sql open VIS;
018900110201
019000110201         //?errore nella open del SQL torno errore al chiamante
019100110201         IF  sqlcode < 0;
019200110201           $End = *on;
019300110201           $Fine = *on;
019400110201           leavesr;
019500110201         ENDIF;
019600110201
019700110201         DOW  not $End;
019800110201           exec sql fetch next from VIS into :VISnrv;
019900110201           IF  sqlcod = 100 or sqlcod < 0;
020000110201             $End = *on;
020100110201             leave;
020200110201           ENDIF;
020300110201         //?alla prima trattativa esco dal ciclo
020400110201           wNrVis += 1;
020500110201           $End = *on;
020600110201         ENDDO;
020700110201         exec sql close VIS;
020800110201
020900110201         //?Ho trovato almeno 1 trattativa aperta
021000110201         IF  wNrVis > 0;
021100110201           $Fine = *on;
021200110201           leavesr;
021300110201         ENDIF;
021400110201
021500110201
021600110131         //?Controllo se ci sono attività da eseguire sul potenziale
021700110131         //?se ne trovo vuol dire che il potenziale lavora
021800110131
021900110131         clear wNrAtc;
022000110131         exec sql
022100110131         select count(*) into :wNrAtc from TIATC00F
022200110201         where ATCcpo = :IMK05cpo and ATCdco = 0;
022300110131         IF  wNrAtc > 0 ;
022400110131           $Fine = *on;
022500110131           leavesr;
022600110131         ENDIF;
022700110131
022800110131         //?Controllo se l'ultima attività eseguita sul potenziale è una 14 o 15
022900110131
023000110201         If wNrAtc = 0 ;
023100110201            exec sql
023200110201            declare ATC cursor for
023300110201            select * from TIATC00F
023400110201            where ATCcpo = :IMK05cpo and ATCdco > 0 order by atcdco desc ;
023500110201            exec sql
023600110201              open ATC ;
023700110201            IF sqlcode < 0;
023800110201              leavesr;
023900110201            ENDIF;
024000110131
024100110201         DOW not $EndAtc;
024200110131           exec sql
024300110201             fetch next from ATC into :tiatcds ;
024400110131             IF sqlcod = 100 or sqlcod < 0;
024500110201               leave  ;
024600110131             ENDIF;
024700110407             $EndAtc = *on ;
024800110201             If Atccac = '14' or Atccac = '15' ;
024900110201               OMK05cat = 'E';
025000110131             ENDIF;
025100110131         ENDDO;
025200110201         exec sql close ATC;
025300110201         ENDIF;
025400110201
025500110131       ENDSR;
025600110131
025700091029       //--------------------------------------------------------------
025800091029       //?Operazioni finali.
025900091029       //--------------------------------------------------------------
026000101216       BEGSR RoutEnd;
026100091029
026200091029         *inLR = *on;
026300091029         return;
026400091029
026500091029       ENDSR;
026600091029
026700091029      /end-free
