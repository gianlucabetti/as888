000100110803      //--------------------------------------------------------------
000200110812      //?TRTB281R - Interrogazione tabella 3C da P.d.c.
000300110803      //--------------------------------------------------------------
000400110803     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500110803
000600110803      //---------------------------------------------------------------
000700110803      //?Dichiarazione file.
000800110803      //---------------------------------------------------------------
000900110803
001000110803      // - Tabelle
001100110810     fTABEL00F  if   e           k disk
001200110906     fTNTBE01L  if   e           k disk
001300110803
001400110803      // - Video
001500110812     fTRTB281D  cf   e             workstn
001600110803
001700110803      //---------------------------------------------------------------
001800110803      //?Definizione costanti.
001900110803      //---------------------------------------------------------------
002000110803
002100110803      //---------------------------------------------------------------
002200110803      //?Definizione schiere.
002300110803      //---------------------------------------------------------------
002400110803
002500110803      //---------------------------------------------------------------
002600110803      //?Definizione aree dati.
002700110803      //---------------------------------------------------------------
002800110803
002900110803      // - Dati utente
003000110803     d §AzUte        e ds                  extname(AZUTE00F)
003100110803     d                                     dtaara
003200110803     d §DatiUte      e ds                  extname(dDatiUte)
003300110803     d                                     dtaara
003400110803
003500110803      //---------------------------------------------------------------
003600110803      //?Definizione strutture dati.
003700110803      //---------------------------------------------------------------
003800110803
003900110803      // - Parametri ricevuti
004000110803     d KPJBA         e ds
004100110803     d TNTA61DS      e ds
004200110803
004300110803      // - Reperimento dati utente
004400110803     d TIBS34DS      e ds
004500110812
004600110812      // - Parametri per Ricerca/Controllo tabelle (TNTBE)?
004700110812     d TIBS02ds      e ds                  inz
004800110812     d   T02mod      e                     inz('C')
004900110803
005000110812      // - Tabella 3C
005100110812     d ds3C          e ds                  inz
005200110812
005300110915      // - Tabella 3C - padre
005400110915     d ds3C_p        e ds                  inz extname(ds3C)
005500110915     d                                         prefix(p_)
005600110915
005700110812      // - Tabella 3CE
005800110812     d d3CE          e ds                  inz
005900110812
006000110812      // - Tabella 3CP
006100110812     d d3CP          e ds                  inz
006200110812
006300110812      // - Tabella 3EW
006400110812     d d3EW          e ds                  inz
006500110803
006600110803      //---------------------------------------------------------------
006700110803      //?Definizione variabili globali.
006800110803      //---------------------------------------------------------------
006900110803
007000110803      // - Flags booleani
007100110803     d $Fine           s               n   inz(*off)
007200110803     d $InzD01         s               n   inz(*on)
007300110913     d $Ok3CE          s               n   inz(*on)
007400110913     d $Ok3CP          s               n   inz(*on)
007500110913     d $Ok3EW          s               n   inz(*on)
007600111005     d $Riga1          s               n   inz(*off)
007700111005     d $Riga2          s               n   inz(*off)
007800111005     d $Riga3          s               n   inz(*off)
007900111005
008000111005      // - Campi associati al video
008100111005     d $Video          s              2    inz('D1')
008200110803
008300110803      //---------------------------------------------------------------
008400110803      //?Definizione procedure esterne.
008500110803      //---------------------------------------------------------------
008600110803
008700110803      //---------------------------------------------------------------
008800110803      //?Prototipi.
008900110803      //---------------------------------------------------------------
009000110803
009100110812      /copy gaitrasrc/srcprotopr,tibs02r
009200110803      /copy gaitrasrc/srcprotopr,tibs34r
009300110810      /copy gaitrasrc/srcprotopi,tibs69r
009400110810      /copy gaitrasrc/srcprotopr,tibs69r
009500110803
009600110803      //---------------------------------------------------------------
009700110803      //?Definizione key-list.
009800110803      //---------------------------------------------------------------
009900110803
010000110803      //---------------------------------------------------------------
010100110803      //?Riepilogo indicatori.
010200110803      //---------------------------------------------------------------
010300110812      // 40 = Serie > 0
010400110812      // 41 = DISK B diventa DISK C
010500110812      // 42 = Accorpamento bolle
010600110812      // 43 = Accorpamento bolle in consegna
010700110803
010800110803      //---------------------------------------------------------------
010900110803
011000110803      //---------------------------------------------------------------
011100110803      //?M A I N - L I N E
011200110803      //---------------------------------------------------------------
011300110803
011400110803     c     *Entry        plist
011500110803     c                   parm                    KPJBA
011600110803     c                   parm                    TNTA61DS
011700110803
011800110803      /free
011900110803
012000110803       //?Operazioni iniziali
012100110803       exsr RoutInz;
012200110803
012300110803       //?Gestione video
012400110803       DOW $Fine = *off;
012500111005         SELECT;
012600111005           WHEN  $Video = 'D1';
012700111005             exsr GesD01;
012800111005           WHEN  $Video = 'W1';
012900111005             exsr GesW01;
013000111005           OTHER;
013100111005             $Fine = *on;
013200111005         ENDSL;
013300110803       ENDDO;
013400110803
013500110803       //?Operazioni finali
013600110803       exsr RoutEnd;
013700110803
013800110803       //--------------------------------------------------------------
013900110803       //?Operazioni iniziali.
014000110803       //--------------------------------------------------------------
014100110803       BEGSR RoutInz;
014200110803
014300110803         //?Reperimento dati job
014400110803         exsr DatiJob;
014500110805
014600110805         IF  TA61ksc <= *zeros;
014700110805           TA61err = *on;
014800110805           TA61msg = 'Richiesto il codice cliente';
014900110805           $Fine = *on;
015000110805         ENDIF;
015100110803
015200110803       ENDSR;
015300110803
015400110803       //--------------------------------------------------------------
015500110803       //?Reperimento Dati del job (Utente/Operativi).
015600110803       //--------------------------------------------------------------
015700110803       BEGSR DatiJob;
015800110803
015900110803         in(E) §AzUte;
016000110803         if NOT %error;
016100110803           in(E) §DatiUte;
016200110803         endif;
016300110803         if %error or RSut = *blanks;
016400110803           clear TIBS34ds;
016500110803           tibs34r(tibs34ds);
016600110803           in §AzUte;
016700110803           in §DatiUte;
016800110803         endif;
016900110803
017000110803       ENDSR;
017100110803
017200110803       //--------------------------------------------------------------
017300111005       //?Gestione videata D01.
017400110803       //--------------------------------------------------------------
017500110803       BEGSR GesD01;
017600110803
017700110803         //?Iniziallizazione videata
017800110803         IF $InzD01 = *on;
017900110803           exsr InzD01;
018000110803           $InzD01 = *off;
018100110803         ENDIF;
018200110803
018300110803       //?Emissione videata
018400110812         exfmt TB28D01;
018500110803
018600110803       //?F12 = Ritorno
018700110803         IF  *inkl;
018800110803           $fine = *on ;
018900110803           leavesr ;
019000110803         ENDIF;
019100111005
019200111005       //?Interroga altri dati
019300111005         IF  V1IntCabD = '?';
019400111005           $Video = 'W1';
019500111005           leavesr ;
019600111005         ENDIF;
019700110803
019800110803       ENDSR;
019900110803
020000110803       //--------------------------------------------------------------
020100110803       //?Caricamento videata D01.
020200110803       //--------------------------------------------------------------
020300110803       BEGSR InzD01;
020400110803
020500110810         clear V1Dksc;
020600110812         clear V1Cksu;
020700110810         clear V1Dksu;
020800110812         clear V1easy1;
020900110812         clear V1easy2;
021000110812         clear V1easy3;
021100111005         clear V1IntCabD;
021200150128         *in47 = *off;
021300110810
021400110810       //?Decodifico il cliente
021500110810         clear  tibs69ds;
021600110810         I69kac = TA61ksc;
021700110810         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
021800110810         V1Cksc = TA61ksc;
021900110811         IF  O69err = *blanks;
022000110811           V1Dksc = ACORag;
022100110811         ELSE;
022200110811           V1Dksc = '? ? ? ? ? ?';
022300110811         ENDIF;
022400110810
022500110810       //?Aggancio la tabella
022600110812         clear ds3C;
022700110810         TBLkut = 1;
022800110812         TBLcod = '3C';
022900110810         TBLkey = %editc(V1Cksc:'X');
023000110810
023100110810         chain (TBLkut : TBLcod : TBLkey) TABEL00F;
023200110812         IF  not %Found(TABEL00F) or TBLflg <> *blanks;
023300110812           leavesr;
023400110812         ENDIF;
023500110811
023600110812         ds3C = TBLuni;
023700110810
023800110812         //?Decodifico l'unificante è sempre presente
023900110812         clear  tibs69ds;
024000110812         I69kac = §3Ccks;
024100110812         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
024200110812         V1Cksu = §3Ccks;
024300110812         IF  O69err = *blanks;
024400110812           V1Dksu = ACORag;
024500110812         ELSE;
024600110812           V1Dksu = '? ? ? ? ? ?';
024700110812         ENDIF;
024800110810
024900110812       //?Supporto da Cliente a BRT
025000110812         V1Ccba = §3Ccba;
025100110812         IF  V1Ccba <> *blanks;
025200110812           clear V1Dcba;
025300110812           reset TIBS02DS;
025400110812           T02cod = '3CS';
025500110812           T02sif = knsif;
025600110812           T02ke1 = V1Ccba;
025700110812           TNTBE_RicercaControllo (kpjba : TIBS02ds);
025800110812           IF  T02err = *blanks;
025900110812             V1Dcba = T02uni;
026000110812           ENDIF;
026100110812         ENDIF;
026200110913
026300110913       //?Imposto i dati da 3C sempre
026400110913       //?Serie
026500110913       //?Filiale segnacollo
026600110913       //?Segnacolli
026700110913         V1Cnrs = §3Cnrs;
026800110913         *in40 = (V1Cnrs > 0);
026900110913         V1Cfls = §3Cfls;
027000110913         V1Cnsd = 1;
027100110913         V1Cnsa = *all'9';
027200110812
027300110812         SELECT;
027400110812
027500110812       //?Se EasySpedWeb prendo i dati da 3EW
027600110812           WHEN  V1Ccba = 'ESWEB';
027700110913             $Ok3EW = *off;
027800110906             clear d3EW;
027900110906             %subst(TBEke1:1:8) = *all'0';
028000110906             %subst(TBEke1:2:7) = %editc(§3Ccks:'X');
028100110906             setll ('3EW' : TBEke1) TNTBE01L;
028200110906             reade ('3EW' : TBEke1) TNTBE01L;
028300110906             DOW  not %eof(TNTBE01L);
028400110906               IF  TBEatb = *blanks;
028500110906                 d3EW = TBEuni;
028600110913                 $Ok3EW = *on;
028700110906               ENDIF;
028800110906               reade ('3EW' : TBEke1) TNTBE01L;
028900110906             ENDDO;
029000110913             IF  $Ok3EW;
029100150128               *in47 = *on;
029200110913             //?Serie
029300110913               V1Cnrs = §3EWnrs;
029400110913               *in40 = (V1Cnrs > 0);
029500110913             //?Filiale segnacollo
029600110913               V1Cfls = §3EWfls;
029700110913             //?Segnacolli
029800110913               V1Cnsd = §3EWnsi;
029900110913               V1Cnsa = §3EWnsf;
030000150128             //?Filiale partenza
030100150128               V13EWLNP = §3EWLNP;
030200110913             //?Alcuni dati aggiuntivi per EasySpedWeb
030300110913               V1easy1 = 'Numero Max anagrafica destinatari ' +
030400110913                         %trim(%editc(§3EWmad:'4'));
030500110913               IF  §3EWaew = 'S';
030600110913               V1easy2 = 'Abilitato a creare utenti di tipo ' +
030700110913                         '''Bollettisti''';
030800110913               ELSE;
030900110913               V1easy2 = 'NON abilitato a creare utenti di tipo ' +
031000110812                       '''Bollettisti''';
031100110913               ENDIF;
031200110913             ENDIF;
031300110812
031400110913       //?Se EasySped prendo i dati da 3CE e 3CP
031500110812           WHEN  V1Ccba = 'ESYSP';
031600170223             $Ok3CE = *off;
031700110812             reset TIBS02DS;
031800110812             T02cod = '3CE';
031900110812             T02sif = knsif;
032000110812             T02ke1 = %editc(§3Ccks:'X');
032100110812             TNTBE_RicercaControllo (kpjba : TIBS02ds);
032200110812             IF  T02err <> *blanks;
032300110812               clear d3CE;
032400110913             ELSE;
032500110913               $Ok3CE = *on;
032600110913             ENDIF;
032700110913             IF  $Ok3CE;
032800110913               d3CE = T02uni;
032900170515             //?Correzione: i dati dei segnacolli li prendo solo dalla 3CP
033000110913             //?Serie
033100110913               IF  §3CEnrs > *zeros;
033200170515                 //V1Cnrs = %dec(§3CEnrs:2:0);
033300170515                 //*in40 = (V1Cnrs > 0);
033400110913               ENDIF;
033500170515               //V1Cfls = §3Cfls;
033600170223             //?Correzione, i segnacolli li prendo solo dalla 3CP
033700110913             //?Segnacolli
033800170223             //V1Cnsd = §3CEsncd;
033900170223             //V1Cnsa = §3CEsnca;
034000110913             //?Alcuni dati aggiuntivi per EasySped
034100110913               V1easy1 = 'Versione EasySped ' + %trim(§3CEever);
034200110913               V1easy1 = %trim(V1easy1) + ' Versione Cappario ' +
034300110913                         %trim(%editc(§3CEcver:'Z'));
034400110913               V1easy1 = %trim(V1easy1) + ' Scadenza Cappario ' +
034500110913                         %subst(%editc(§3CEcdsc:'X'):7:2) + '/' +
034600110913                         %subst(%editc(§3CEcdsc:'X'):5:2) + '/' +
034700110913                         %subst(%editc(§3CEcdsc:'X'):1:4);
034800110913               V1easy2 = 'Oggetto Mail ' + %trim(§3CEobj);
034900110913               V1easy3 = 'Versione Massima Cappario ' +
035000110913                         %trim(%editc(§3CEcvermx:'Z'));
035100110913               V1easy3 = %trim(V1easy3) + ' dal ' +
035200110913                         %subst(%editc(§3CEcddemx:'X'):7:2) + '/' +
035300110913                         %subst(%editc(§3CEcddemx:'X'):5:2) + '/' +
035400110913                         %subst(%editc(§3CEcddemx:'X'):1:4) +
035500110913                         ' al ' +
035600110913                         %subst(%editc(§3CEcdscmx:'X'):7:2) + '/' +
035700110913                         %subst(%editc(§3CEcdscmx:'X'):5:2) + '/' +
035800110913                         %subst(%editc(§3CEcdscmx:'X'):1:4);
035900110913             ENDIF;
036000110906           //?segnacolli da 3CP
036100110906             IF  *in40;
036200110913               $Ok3CP = *off;
036300110906               clear d3CP;
036400110906               TBEke1 = %editc(§3Ccks:'X');
036500110906               setll ('3CP' : TBEke1) TNTBE01L;
036600110906               reade ('3CP' : TBEke1) TNTBE01L;
036700110906               DOW  not %eof(TNTBE01L);
036800110906                 IF  TBEatb = *blanks;
036900110906                   d3CP = TBEuni;
037000110913                   $Ok3CP = *on;
037100110906                 ENDIF;
037200110906                 reade ('3CP' : TBEke1) TNTBE01L;
037300110906               ENDDO;
037400110913               IF  $Ok3CP;
037500110913                 V1Cnsd = %dec(%subst(TBEke2:3:7):7:0);
037600110913                 V1Cnsa = §3CPal;
037700110913               ENDIF;
037800110906             ENDIF;
037900130219
038000130219       //?per tutto il resto segnacolli da 3CP se c'è la serie
038100130219           OTHER;
038200130219             IF  *in40;
038300130219               $Ok3CP = *off;
038400130219               clear d3CP;
038500130219               TBEke1 = %editc(§3Ccks:'X');
038600130219               setll ('3CP' : TBEke1) TNTBE01L;
038700130219               reade ('3CP' : TBEke1) TNTBE01L;
038800130219               DOW  not %eof(TNTBE01L);
038900130219                 IF  TBEatb = *blanks;
039000130219                   d3CP = TBEuni;
039100130219                   $Ok3CP = *on;
039200130219                 ENDIF;
039300130219                 reade ('3CP' : TBEke1) TNTBE01L;
039400130219               ENDDO;
039500130219               IF  $Ok3CP;
039600130219                 V1Cnsd = %dec(%subst(TBEke2:3:7):7:0);
039700130219                 V1Cnsa = §3CPal;
039800130219               ENDIF;
039900130219             ENDIF;
040000110812
040100110812         ENDSL;
040200110812
040300110812         //?Supporto da BRT a Cliente
040400110812         //?solo se ho la serie
040500110812         IF  *in40;
040600110915           V1Cbac = §3Cbac;
040700110915           //?se non presente lo recupero dal padre
040800110915           IF  V1Cbac = *blanks and V1cksc <> V1cksu and
040900110915               V1cksu > 0;
041000110915             clear ds3C_p;
041100110915             TBLkey = %editc(V1Cksu:'X');
041200110915             chain (TBLkut : TBLcod : TBLkey) TABEL00F;
041300110915             IF  %Found(TABEL00F) and TBLflg = *blanks;
041400110915               ds3C_p = TBLuni;
041500110915               V1Cbac = p_§3Cbac;
041600110915             ENDIF;
041700110915           ENDIF;
041800110812           clear V1Dbac;
041900110812           reset TIBS02DS;
042000110812           T02cod = '3CS';
042100110812           T02sif = knsif;
042200110812           T02ke1 = V1Cbac;
042300110812           TNTBE_RicercaControllo (kpjba : TIBS02ds);
042400110812           IF  T02err = *blanks;
042500110912             V1Dbac = T02uni;
042600110812           ENDIF;
042700110812         ENDIF;
042800110812
042900110812         //?I prossimi dati li emetto solo se ci sono
043000110812         IF  §3Cdkc = 'S';
043100110812           *in41 = *on;
043200110812           V1Cdkc = 'SI';
043300110812         ENDIF;
043400110812         IF  §3Cabd <> *blanks;
043500110812           *in42 = *on;
043600110812           IF  §3Cabd = 'S';
043700110812             V1Cabd = 'Sì per RMN';
043800110812           ENDIF;
043900110812           IF  §3Cabd = 'A';
044000110812             V1Cabd = 'Sì per RMA';
044100110812           ENDIF;
044200111003           exsr inzW01;
044300110812         ENDIF;
044400110812         IF  §3Caus = 'S';
044500110812           *in43 = *on;
044600110812           V1Caus = 'SI';
044700110812         ENDIF;
044800110803
044900110803       ENDSR;
045000111005
045100111005       //--------------------------------------------------------------
045200111005       //?Gestione videata W01.
045300111005       //--------------------------------------------------------------
045400111005       BEGSR GesW01;
045500111005
045600111005       //?Emissione videata
045700111005         exfmt TB28W01;
045800111005
045900111005       //?F12 = Ritorno
046000111005         IF  *inkl;
046100111005           $Video = 'D1';
046200111005           clear V1IntCabD;
046300111005           leavesr ;
046400111005         ENDIF;
046500111005
046600111005       ENDSR;
046700111003
046800111003       //--------------------------------------------------------------
046900111003       //?Caricamento videata W01.
047000111003       //--------------------------------------------------------------
047100111003       BEGSR InzW01;
047200111003
047300111005         clear W1Riga1;
047400111005         clear W1Riga2;
047500111005         clear W1Riga3;
047600111005         $Riga1 = *off;
047700111005         $Riga2 = *off;
047800111005         $Riga3 = *off;
047900111003
048000111005       //?Riferimento + allineamento
048100111005         IF  §3Clrm > 0;
048200111005           W1Riga1 = 'Riferimento lungo ' + %editc(§3Clrm:'X');
048300111005           $Riga1 = *on;
048400111003         ENDIF;
048500111005
048600111005       //?Allineamento
048700111005         SELECT;
048800111005           WHEN  §3Calm = 'D' and $Riga1;
048900111005             W1Riga1 = %trim(W1Riga1) + '  allineato a destra';
049000111005           WHEN  §3Calm = 'S' and $Riga1;
049100111005             W1Riga1 = %trim(W1Riga1) + '  allineato a sinistra';
049200111005           WHEN  §3Calm = 'D';
049300111005             W1Riga1 = 'Riferimento allineato a destra';
049400111005           WHEN  §3Calm = 'S';
049500111005             W1Riga1 = 'Riferimento allineato a sinistra';
049600111005         ENDSL;
049700111005
049800111005       //?Nr max VAB da raggruppare
049900111005         IF  §3Cmvr > 0;
050000111005           W1Riga2 = 'Max VAB da raggruppare ' + %editc(§3Cmvr:'X');
050100111005           $Riga2 = *on;
050200111005         ENDIF;
050300111005
050400111005       //?Unisce/raggruppa note
050500111005         SELECT;
050600111005           WHEN  §3Cnot = 'S' and $Riga2;
050700111005             W1Riga2 = %trim(W1Riga2) + '  Raggruppa note ';
050800111005           WHEN  §3Cnot = 'U' and $Riga2;
050900111005             W1Riga2 = %trim(W1Riga2) + '  Unisce note ';
051000111005           WHEN  §3Cnot = 'S';
051100111005             W1Riga2 = 'Raggruppa note ';
051200111005             $Riga2 = *on;
051300111005           WHEN  §3Cnot = 'U';
051400111005             W1Riga2 = 'Unisce note ';
051500111005             $Riga2 = *on;
051600111005         ENDSL;
051700111005
051800111005       //?Accorpa VAX
051900111005         IF  §3Cavx = 'S' and $Riga2;
052000111005           W1Riga2 = %trim(W1Riga2) + '  Accorpa VAX';
052100111005         ENDIF;
052200111005         IF  §3Cavx = 'S';
052300111005           W1Riga2 = 'Accorpa VAX';
052400111005         ENDIF;
052500111005
052600111005       //?Accorpa bolle con c/assegno
052700111005         IF  §3Cabc = 'S';
052800111005           W1Riga3 = 'Accorpa bolle con c/assegno ';
052900111005           $Riga3 = *on;
053000111005         ENDIF;
053100111005
053200111005       //?Accorpa particolarità RC
053300111005         IF  §3Carc = 'S' and $Riga3;
053400111005           W1Riga3 = %trim(W1Riga3) + '  Accorpa particolarità "RC"';
053500111005         ENDIF;
053600111005         IF  §3Carc = 'S';
053700111005           W1Riga3 = 'Accorpa particolarità "RC"';
053800111005         ENDIF;
053900111003
054000111003       ENDSR;
054100110803
054200110803       //--------------------------------------------------------------
054300110803       //?Operazioni finali.
054400110803       //--------------------------------------------------------------
054500110803       BEGSR RoutEnd;
054600110803
054700110803         *inLR = *on;
054800110803         return;
054900110803
055000110803       ENDSR;
