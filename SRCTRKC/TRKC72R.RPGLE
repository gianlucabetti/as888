000100141113      //--------------------------------------------------------------
000200150112      //?TRKC72R - VERIFICA QUALE OBIETTIVO VARIARE
000300141113      //--------------------------------------------------------------
000400141113
000500141113     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141113
000700141113      //---------------------------------------------------------------
000800141113      //?Dichiarazione file.
000900141113      //---------------------------------------------------------------
001000141113
001100141113      // - File Clienti in Campagna
001200141113     fTICMC01L  if   e           k disk
001300141114
001400141114      // - Anagrafica Campagna
001500141114     fTICMP01L  if   e           k disk
001600141114
001700141114      // - Anagrafica Potenziali
001800141114     fTNCPO01L  if   e           k disk
001900141113
002000141113      //---------------------------------------------------------------
002100141113      //?Definizione costanti.
002200141113      //---------------------------------------------------------------
002300141113     d FaseObj         c                   const(' 10')
002400141113     d FaseObjProp     c                   const(' 20')
002500141113     d FaseObjFinale   c                   const(' 30')
002600141113
002700141113      //---------------------------------------------------------------
002800141113      //?Definizione schiere.
002900141113      //---------------------------------------------------------------
003000141113
003100141113      // - Messaggi di errore
003200141113     d Msg             s             78    dim(20) ctdata perrcd(1)
003300141113
003400141113      //---------------------------------------------------------------
003500141113      //?Definizione aree dati.
003600141113      //---------------------------------------------------------------
003700141113
003800141113      // - Dati utente
003900141113     d §AzUte        e ds                  extname(AZUTE00F)
004000141113     d                                     dtaara
004100141113     d §DatiUte      e ds                  extname(dDatiUte)
004200141113     d                                     dtaara
004300141113
004400141113      //---------------------------------------------------------------
004500141113      //?Definizione strutture dati.
004600141113      //---------------------------------------------------------------
004700141113
004800141113      // - Status
004900141113     d Psds           sds
005000141113     d   SDSpgm          *proc
005100141113
005200141113      // - Parametri ricevuti
005300141113     d KPJBA         e ds
005400150112     d TRKC72DS      e ds
005500141113
005600141113      // - Ricerca/Controllo tabelle
005700141113     d TIBS02DS      e ds                  inz
005800141113
005900141113      // - Reperimento dati utente
006000141113     d TIBS34DS      e ds
006100141114
006200141114      // - Reperimento dati Anagrafica Clienti
006300141114      /copy gaitrasrc/srcprotopi,TIBS69R
006400141113
006500141113      // - Tabella FCM - Fase Campagna
006600141113     d dFCM          e ds
006700141113
006800141113      //---------------------------------------------------------------
006900141113      //?Definizione variabili globali.
007000141113      //---------------------------------------------------------------
007100141113
007200141113      // - Campi di comodo data
007300141113     d Data_EUR        s               d   datfmt(*eur)
007400141113     d Data_ISO        s               d   datfmt(*iso)
007500141113
007600141113      // - Campi di comodo
007700141113     d Oggi            s              8s 0 inz
007800141113
007900141113      //---------------------------------------------------------------
008000141113      //?Definizione procedure usate.
008100141113      //---------------------------------------------------------------
008200141113
008300141113      //---------------------------------------------------------------
008400141113      //?Definizione Prototipi.
008500141113      //---------------------------------------------------------------
008600141113      /copy gaitrasrc/srcprotopr,TIBS02R
008700141113      /copy gaitrasrc/srcprotopr,TIBS34R
008800141114      /copy gaitrasrc/srcprotopr,TIBS69R
008900141113
009000141113      //---------------------------------------------------------------
009100141113      //?Definizione key-list.
009200141113      //---------------------------------------------------------------
009300141113
009400141113      //---------------------------------------------------------------
009500141113      //?M A I N - L I N E
009600141113      //---------------------------------------------------------------
009700141113
009800141113     c     *Entry        plist
009900141113     c                   parm                    kpjba
010000150112     c                   parm                    TRKC72DS
010100141113
010200141113      /free
010300141113
010400141113       //?Operazioni iniziali
010500141113       exsr RoutInz;
010600141113
010700141113       //?Controllo dati passati
010800141113       exsr Controlla;
010900141113
011000141113       //?Imposta l'obiettivo da variare
011100141113       exsr Obiettivo;
011200141113
011300141113       //?Operazioni finali
011400141113       exsr RoutEnd;
011500141113
011600141113       //--------------------------------------------------------------
011700141113       //?Operazioni iniziali.
011800141113       //--------------------------------------------------------------
011900141113       BEGSR RoutInz;
012000141113
012100141113       //?Imposto oggi
012200141113         Oggi = %dec(%date());
012300141113
012400141113       //?Reperimento dati job
012500141113         exsr DatiJob;
012600141113
012700150112         clear OKC72acm;
012800150112         clear OKC72err;
012900150112         clear OKC72msg;
013000141113
013100141113       ENDSR;
013200141113
013300141113       //--------------------------------------------------------------
013400141113       //?Reperimento Dati del job (Utente/Operativi).
013500141113       //--------------------------------------------------------------
013600141113       BEGSR DatiJob;
013700141113
013800141113         in(E) §AzUte;
013900141113         IF  NOT %error;
014000141113           in(E) §DatiUte;
014100141113         ENDIF;
014200141113         IF  %error or RSut = *blanks;
014300141113           clear TIBS34ds;
014400141113           tibs34r(tibs34ds);
014500141113           in §AzUte;
014600141113           in §DatiUte;
014700141113         ENDIF;
014800141113
014900141113       ENDSR;
015000141113
015100141113       //--------------------------------------------------------------
015200141113       //?Controllo i dati passati.
015300141113       //--------------------------------------------------------------
015400141113       BEGSR Controlla;
015500141113
015600141113       //?Numero campagna obbligatorio
015700150112         IF  IKC72ncm = 0;
015800150112           OKC72err = 'E';
015900150112           OKC72msg = Msg(01);
016000141113           exsr RoutEnd;
016100141113         ENDIF;
016200141114       //?Deve essere una campagna valida
016300150112         chain (IKC72ncm) TICMP01L;
016400150507         IF  not %found(TICMP01L);
016500150112           OKC72err = 'E';
016600150112           OKC72msg = Msg(01);
016700141114           exsr RoutEnd;
016800141114         ENDIF;
016900150507         IF  CMPdfc < Oggi;
017000150507           OKC72err = 'E';
017100150507           OKC72msg = %trim(Msg(01)) + '. Già chiusa.';
017200150507           exsr RoutEnd;
017300150507         ENDIF;
017400141113
017500141113       //?Almeno 1 tra KSU/KSC/CPO deve esserci
017600150112         IF  IKC72ksu = 0 and IKC72ksc = 0 and IKC72cpo = 0;
017700150112           OKC72err = 'E';
017800150112           OKC72msg = Msg(02);
017900141113           exsr RoutEnd;
018000141113         ENDIF;
018100141114       //?Deve essere una codice valido
018200150112         IF  IKC72ksu > 0;
018300141114           clear TIBS69DS;
018400150112           I69kac = %dec(IKC72ksu:7:0);
018500141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
018600141114           IF  O69err <> *blanks;
018700150112             OKC72err = 'E';
018800150112             OKC72msg = Msg(02);
018900141114             exsr RoutEnd;
019000141114           ENDIF;
019100141114         ENDIF;
019200150112         IF  IKC72ksc > 0;
019300141114           clear TIBS69DS;
019400150112           I69kac = %dec(IKC72ksc:7:0);
019500141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
019600141114           IF  O69err <> *blanks;
019700150112             OKC72err = 'E';
019800150112             OKC72msg = Msg(02);
019900141114             exsr RoutEnd;
020000141114           ENDIF;
020100141114         ENDIF;
020200150112         IF  IKC72cpo > 0;
020300150112           chain (IKC72cpo) TNCPO01L;
020400141114           IF  not %found(TNCPO01L);
020500150112             OKC72err = 'E';
020600150112             OKC72msg = Msg(02);
020700141114             exsr RoutEnd;
020800141114           ENDIF;
020900141114         ENDIF;
021000141113
021100141113       //?Deve essere indicata la fase attuale
021200150112         IF  IKC72acm = *blanks;
021300150112           OKC72err = 'E';
021400150112           OKC72msg = Msg(03);
021500141113           exsr RoutEnd;
021600141113         ENDIF;
021700141113
021800141113       //?Deve essere una fase valida
021900141113         clear TIBS02DS;
022000141113         clear dFCM;
022100141113         T02mod = 'C';
022200141113         T02cod = 'FCM';
022300150112         T02ke1 = IKC72acm;
022400141113         T02sif = KNSIF;
022500141113         TNTBE_RicercaControllo  (kpjba : tibs02ds);
022600141113         IF  T02err <> *blanks;
022700150112           OKC72err = 'E';
022800150112           OKC72msg = Msg(04);
022900141113           exsr RoutEnd;
023000141113         ENDIF;
023100141113       //?Dati Fase
023200141113         dFCM = T02uni;
023300141113
023400141113       ENDSR;
023500141113
023600141113       //--------------------------------------------------------------
023700141113       //?Imposta l'obiettivo da variare.
023800141113       //--------------------------------------------------------------
023900141113       BEGSR Obiettivo;
024000141113
024100141113         SELECT;
024200150112         WHEN  IKC72acm = FaseObj and DUTpou = 46;
024300150112           OKC72acm = FaseObj;
024400150112         WHEN  IKC72acm = FaseObj;
024500150112           OKC72acm = FaseObjProp;
024600150112         WHEN  IKC72acm = FaseObjProp and DUTpou = 46;
024700150112           OKC72acm = FaseObjFinale;
024800150112         WHEN  IKC72acm = FaseObjProp;
024900150112           OKC72acm = FaseObjProp;
025000150112         WHEN  IKC72acm = FaseObjFinale and DUTpou = 46;
025100150112           OKC72acm = FaseObjFinale;
025200141113         OTHER;
025300150112           OKC72err = 'E';
025400150115           OKC72msg = Msg(05);
025500141113         ENDSL;
025600141113
025700141113       ENDSR;
025800141113
025900141113       //--------------------------------------------------------------
026000141113       //?Operazioni finali.
026100141113       //--------------------------------------------------------------
026200141113       BEGSR RoutEnd;
026300141113
026400141113         *inLR = *on;
026500141113         return;
026600141113
026700141113       ENDSR;
026800141113
026900141113      /end-free
027000141113
027100141113       //--------------------------------------------------------------
027200141113       //?Schiere a tempo di compilazione.
027300141113       //--------------------------------------------------------------
027400141113
027500141113** -- MSG -------------------------------------------------------------------*
027600141114Campagna commerciale errata                                                    1
027700141114Cliente/Potenziale errato                                                      2
027800141113Manca la fase                                                                  3
027900141113Fase inesistente                                                               4
028000141120Obiettivo Finale modificabile solo dalla SEDE                                  5
