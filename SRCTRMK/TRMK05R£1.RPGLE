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
001400110411      // - File Attivit�
001500110411     fTiatc04L  if   e           k disk
001600110412
001700110412      // - File Trattative
001800110415     fTivis01L  if   e           k disk    usropn
001900110415     f                                     extfile(WFLib)
002000110415
002100091029      //---------------------------------------------------------------
002200091029      //?Definizione costanti.
002300091029      //---------------------------------------------------------------
002400091029
002500091029      //---------------------------------------------------------------
002600091029      //?Definizione schiere.
002700091029      //---------------------------------------------------------------
002800091029
002900091029      //---------------------------------------------------------------
003000091029      //?Definizione aree dati.
003100091029      //---------------------------------------------------------------
003200091029      //---------------------------------------------------------------
003300091029      //?Definizione strutture dati.
003400091029      //---------------------------------------------------------------
003500101216
003600091029      // - Parametri ricevuti
003700091029     d KPJBA         e ds
003800110131     d TRMK05ds      e ds
003900110201
004000110201      // File Trattative
004100110201     d TIVIS00F      e ds                  extname(TIVIS00F)
004200110201
004300091029      //---------------------------------------------------------------
004400091029      //?Definizione variabili globali.
004500091029      //---------------------------------------------------------------
004600091029
004700091029      // - Flags booleani
004800101217     d $AcoBlc         s               n   inz(*off)
004900091029     d $End            s               n   inz(*off)
005000110201     d $EndAtc         s               n   inz(*off)
005100091029     d $Fine           s               n   inz(*off)
005200101216
005300101216      // - Indici di schiera
005400101216     d xx              s              4  0 inz
005500091029
005600091029      // - Campi di comodo
005700101217
005800110131     d wNrAtc          s              5i 0
005900110131     d wNrVis          s              5i 0
006000110131
006100110415       // - Nome libreria del file TIVIS
006200110415     d wFLib           s             21    inz
006300110415
006400091029      //---------------------------------------------------------------
006500091029      //?Definizione procedure esterne.
006600091029      //---------------------------------------------------------------
006700101216
006800110131
006900101216      //---------------------------------------------------------------
007000101216      //?Definizione prototipi.
007100101216      //---------------------------------------------------------------
007200091029
007300091029      //---------------------------------------------------------------
007400091029      //?Definizione key-list.
007500091029      //---------------------------------------------------------------
007600091029
007700091029      //---------------------------------------------------------------
007800091029      //?Riepilogo indicatori.
007900091029      //---------------------------------------------------------------
008000091029
008100091029      //---------------------------------------------------------------
008200091029      //?M A I N - L I N E
008300091029      //---------------------------------------------------------------
008400091029
008500091029     c     *Entry        plist
008600091029     c                   parm                    KPJBA
008700110131     c                   parm                    TRMK05ds
008800091029
008900091029      /free
009000091029
009100091029       //?Operazioni iniziali
009200101216       exsr RoutInz;
009300110131
009400101216       //?Elabora
009500091029       DOW  $Fine = *off;
009600101216         exsr Elabora;
009700091029       ENDDO;
009800091029
009900091029       //?Operazioni finali
010000101216       exsr RoutEnd;
010100091029
010200091029       //--------------------------------------------------------------
010300091029       //?Operazioni iniziali.
010400091029       //--------------------------------------------------------------
010500101216       BEGSR RoutInz;
010600101216
010700110131         clear OMK05err;
010800091029
010900101216         //?Se Non ho il potenziale esco dal pgm con errore
011000110131         IF  IMK05CPO = 0;
011100110131           OMK05err = '1';
011200091029           $Fine = *on;
011300091029           leavesr;
011400091029         ENDIF;
011500110415
011600110415         //?Apro il file delle trattative
011700110415         IF  %subst(knsif : 7 : 1) = 'P';
011800110415           wFLib = 'FILTRAGRPF/TIVIS01L';
011900110415         ELSE;
012000110415           wFLib = 'FILTRAGRU/TIVIS01L';
012100110415         ENDIF;
012200110415         open  TIVIS01L;
012300110415
012400091029
012500091029       ENDSR;
012600101217
012700101216       //--------------------------------------------------------------
012800101216       //?Elabora.
012900101216       //--------------------------------------------------------------
013000101216       BEGSR Elabora;
013100101217
013200101216         //?Cerco tutti i clienti con il potenziale ricevuto
013300101217         $AcoBlc = *off;
013400101217         $End = *off;
013500110131         setll IMK05cpo CNACO16L;
013600110131
013700110131         //?Nessun KSC associato il potenziale � in Categoria mai codificato
013800110131         IF  not %Equal(CNACO16L);
013900110131           OMK05cat = 'M';
014000110407           exsr Sr_elimina ;
014100101216           $Fine = *on;
014200101216           leavesr;
014300101216         ENDIF;
014400101216
014500110201         //?Se tutti KSC associati bloccati oppure no
014600110131         reade IMK05cpo CNACO16L;
014700110131         DOW not %eof(CNACO16L) ;
014800110131           //?Cliente bloccato
014900101216             IF  ACOabl <> *blanks;
015000101217               $AcoBlc = *on;
015100110131             else ;
015200110131           //?almeno un codice Cliente non bloccato Potenziale caregoria Codificato
015300110131               $acoBlc = *off ;
015400110131               OMK05cat = 'C';
015500110131               $Fine = *on;
015600110131               leavesr;
015700101216             ENDIF;
015800110131
015900110131           reade IMK05cpo CNACO16L;
016000110131         ENDDO  ;
016100110131
016200110131         //?Se tutti i codici collegati sono bloccati bisogna verificare se ce ne �
016300110131
016400110131         If  $AcoBlc = *on ;
016500110131               OMK05cat = 'P';
016600110407               exsr Sr_elimina ;
016700110131               $Fine = *on;
016800110131               leavesr;
016900110131         Endif ;
017000101216
017100101216       ENDSR;
017200110131
017300110131       //--------------------------------------------------------------
017400110131       //?Verifico se Potenziale Eliminabile
017500110131       //--------------------------------------------------------------
017600110131       BEGSR Sr_elimina ;
017700110131
017800110131         //?Verifico se il potenziale Perso o mai codificato � ache eliminabile
017900110131
018000110201         //?Controllo se ci sono trattative aperte sul potenziale
018100110201         //?se ne trovo vuol dire che il potenizale non � eliminabile
018200110201         //?alla prima trattativa esco dal ciclo
018300110412         setll imk05cpo tivis01l ;
018400110412         reade imk05cpo tivis01l ;
018500110201
018600110201         //?Ho trovato almeno 1 trattativa aperta
018700110412         IF  not %eof(tivis01l) and visdch = 0 ;
018800110201           $Fine = *on;
018900110201           leavesr;
019000110201         ENDIF;
019100110201
019200110201
019300110131         //?Controllo se ci sono attivit� da eseguire sul potenziale
019400110131         //?se ne trovo vuol dire che il potenziale lavora
019500110131
019600110131         clear wNrAtc;
019700110411         setll imk05cpo tiatc04l ;
019800110411         reade imk05cpo tiatc04l ;
019900110412         If not %eof(tiatc04l) and atcdco = 0;
020000110411           $Fine = *on;
020100110411           leavesr;
020200110411         ENDIF;
020300110411         setgt imk05cpo tiatc04l ;
020400110411         readpe imk05cpo tiatc04l ;
020500110411         If not %eof(tiatc04l) ;
020600110131         //?Controllo se l'ultima attivit� eseguita sul potenziale � una 14 o 15
020700110201             If Atccac = '14' or Atccac = '15' ;
020800110201               OMK05cat = 'E';
020900110131             ENDIF;
021000110411         ENDIF ;
021100110201
021200110131       ENDSR;
021300110131
021400091029       //--------------------------------------------------------------
021500091029       //?Operazioni finali.
021600091029       //--------------------------------------------------------------
021700101216       BEGSR RoutEnd;
021800091029
021900091029         *inLR = *on;
022000091029         return;
022100091029
022200091029       ENDSR;
022300091029
022400091029      /end-free
