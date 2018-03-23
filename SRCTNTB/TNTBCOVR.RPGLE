000100151027      //--------------------------------------------------------------
000200151027      //?TNTBCOVR - Gestione tabella "COV" Conf.Automatica ORM VAs
000300151027      //--------------------------------------------------------------
000400151027     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500151027
000600151027      //--------------------------------------------------------------
000700151027      //?Dichiarazione file.
000800151027      //---------------------------------------------------------------
000900151027      // - File TABELLE
001000151027     fTNTBE01L  uf a e           k disk
001100151027     fTNTBE11L  uf a e           k disk    rename(TNTBE000:TNTBE11)
001200151027
001300151027      // - ORGANIGRAMMA
001400151027     fAZORG01L  if   e           k disk
001500151027
001600151027      // - ANAGRAFICA CLIENTI
001700151027     fCNACO00F  if   e           k disk
001800151027
001900151027      // - ANAGRAFICA CLIENTI RITIRO
002000151027     fFNACR01L  if   e           k disk
002100151027
002200151027      // - Video
002300151027     fTNTBCOVD  cf   e             workstn sfile(TBCOVS01:S01nrr)
002400151027
002500151027      //---------------------------------------------------------------
002600151027      //?Definizione costanti.
002700151027      //---------------------------------------------------------------
002800151027     d Errore          c                   '1'
002900151027     d Eseguito        c                   '0'
003000151027
003100151027     d Digits          c                   const('0123456789')
003200151027
003300151027      //---------------------------------------------------------------
003400151027      //?Definizione schiere.
003500151027      //---------------------------------------------------------------
003600151027     d Msg             s             78    dim(17) ctdata perrcd(1)
003700151027
003800151027      //---------------------------------------------------------------
003900151027      //?Definizione aree dati.
004000151027      //---------------------------------------------------------------
004100151027      // - Dati utente
004200151027     d §AzUte        e ds                  extname(AZUTE00F)
004300151027     d                                     dtaara
004400151027     d §DatiUte      e ds                  extname(dDatiUte)
004500151027     d                                     dtaara
004600151027
004700151027      //---------------------------------------------------------------
004800151027      //?Definizione strutture dati.
004900151027      //---------------------------------------------------------------
005000151027      // - Status
005100151027     d Psds           sds
005200151027     d  PGMname          *proc
005300151027
005400151027     d Wlbdat          ds                  inz
005500151027     d  G02dat                 1      8  0
005600151027     d  G02inv                 9     16  0
005700151027     d  G02err                17     17
005800151027     d  G02tgi                18     22  0
005900151027
006000151027      // - Parametri ricevuti
006100151027     d KPJBA         e ds
006200151027     d TNTBCOVDS     e ds
006300151027
006400151027      // - Reperimento dati utente
006500151027     d TIBS34ds      e ds
006600151027
006700151027      //---------------------------------------------------------------
006800151027      //?Definizione variabili globali.
006900151027      //---------------------------------------------------------------
007000151027      // - Flags booleani
007100151027     d wEoFS01         s               n   inz(*off)
007200151027     d wCarS01         s               n   inz(*off)
007300151027     d wCarW01         s               n   inz(*off)
007400151027
007500151027      // - Campi di comodo
007600151027     d blanks          s                   like(D1descopz)
007700151027     d kTBEke1         s                   like(TBEke1)
007800151027     d kTBEke2         s                   like(TBEke2)
007900151027     d len             s              5u 0
008000151027     d S01nrr          s              4  0
008100151027     d savopzione      s                   like(S1opzione)
008200151027     d savrcdnbr       s                   like(C1rcdnbr)
008300151027     d wKey            s              1
008400151027     d wVideo          s             10
008500151027     d w0010           s              1s 0 inz(1)
008600151027     d w0030           s              3s 0 inz
008700151027     d w0070           s              7s 0 inz
008800151027     d w0100           s             10s 0 inz
008900151027
009000151027      //---------------------------------------------------------------
009100151027      //?Definizione procedure usate.
009200151027      //---------------------------------------------------------------
009300151027
009400151027      //---------------------------------------------------------------
009500151027      //?Definizione prototipi.
009600151027      //---------------------------------------------------------------
009700151027      /copy gaitrasrc/srcprotopr,tibs34r
009800151027      /copy gaitrasrc/srcprotopr,xsrda8
009900151027
010000151027      //---------------------------------------------------------------
010100151027      //?Riepilogo indicatori.
010200151027      //---------------------------------------------------------------
010300151027      // 01 - ON ricerca -- OFF manutenzione
010400151027      // 02 - proteggo campo Ke1
010500151027      // 03 - proteggo campo Ke2
010600151027      // 04 - visualizzazione
010700151027      // 20 - gestione subfile
010800151027      // 21 - gestione subfile
010900151027      // 22 - gestione subfile
011000151027      // 23 - gestione subfile
011100151027      // 28 - messaggio Errore
011200151027      // 40 - Chiave 1
011300151027      // 41 - Chiave 2
011400151027
011500151027      //---------------------------------------------------------------
011600151027      //?M A I N - L I N E
011700151027      //---------------------------------------------------------------
011800151027
011900151027     c     *Entry        plist
012000151027     c                   parm                    KPJBA
012100151027
012200151027      /free
012300151027
012400151027       // Operazioni iniziali
012500151027       exsr RoutInz;
012600151027
012700151027       // Gestione videata introduttiva
012800151027       exsr GesD00;
012900151027
013000151027       // Gestione Subfile
013100151027       exsr GesS01;
013200151027
013300151027       // Operazioni finali
013400151027       exsr RoutEnd;
013500151027
013600151027       //--------------------------------------------------------------
013700151027       //?Operazioni iniziali.
013800151027       //--------------------------------------------------------------
013900151027       BEGSR RoutInz;
014000151027
014100151027       // Ricezione parametri
014200151027         TNTBCOVDS = kpjbu;
014300151027
014400151027         SELECT;
014500151027       // Ricerca e scelta
014600151027         WHEN  BCOVfun = '1';
014700151027           *in01 = *on;
014800151027       // Manutenzione
014900151027         WHEN  BCOVfun = *blanks;
015000151027          *in01 = *off;
015100151027         OTHER;
015200151027           BCOVesito = Errore;
015300151027           exsr RoutEnd;
015400151027         ENDSL;
015500151027
015600151027       // Reperimento dati job
015700151027         exsr DatiJob;
015800151027
015900151027       ENDSR;
016000151027
016100151027       //--------------------------------------------------------------
016200151027       //?Reperimento Dati del job (Utente/Operativi).
016300151027       //--------------------------------------------------------------
016400151027       BEGSR DatiJob;
016500151027
016600151027         in(E) §AzUte;
016700151027         IF  not %error;
016800151027           in(E) §DatiUte;
016900151027         ENDIF;
017000151027         IF  %error or RSut = *blanks;
017100151027           clear TIBS34ds;
017200151027           tibs34r(tibs34ds);
017300151027           in §AzUte;
017400151027           in §DatiUte;
017500151027         ENDIF;
017600151027
017700151027       ENDSR;
017800151027
017900151027       //--------------------------------------------------------------
018000151027       //?Gestione video D00.
018100151027       //--------------------------------------------------------------
018200151027       BEGSR GesD00;
018300151027
018400151027         wVideo  = 'D00';
018500151027
018600151027       // Emetto videata
018700151027         DOW  wVideo = 'D00';
018800151027           exfmt TBCOVD00;
018900151027
019000151027         // Controllo tasti funzionali del subfile
019100151027           SELECT;
019200151027
019300151027         // F3=Fine
019400151027           WHEN  *inKC;
019500151027             wVideo = *blanks;
019600151027             BCOVricez = 'C';
019700151027             exsr RoutEnd;
019800151027
019900151027         // In tutti gli altri casi
020000151027           OTHER;
020100151027             wVideo = 'S01';
020200151027           ENDSL;
020300151027
020400151027         ENDDO;
020500151027
020600151027       ENDSR;
020700151027
020800151027       //--------------------------------------------------------------
020900151027       //?Gestione video S01.
021000151027       //--------------------------------------------------------------
021100151027       BEGSR GesS01;
021200151027
021300151027       // Imposto variabili
021400151027         wCarS01 = *on;
021500151027         wVideo  = 'S01';
021600151027
021700151027       // Inizio elaborazione subfile
021800151027         DOU  wVideo <> 'S01';
021900151027
022000151027         // Caricamento subfile
022100151027           exsr Cars01;
022200151027
022300151027         // C1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
022400151027         // impostando C1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
022500151027         // l'ultimo tasto
022600151027           IF  C1csrrrn > 0;
022700151027             C1rcdnbr = C1csrrrn;
022800151027           ELSE;
022900151027             C1rcdnbr = savrcdnbr;
023000151027           ENDIF;
023100151027
023200151027         // Se non so quale pagina visualizzare forzo pagina 1
023300151027           IF  C1rcdnbr < 1;
023400151027             C1rcdnbr = 1;
023500151027           ENDIF;
023600151027
023700151027         // Salvo il record number del subfile
023800151027           savrcdnbr = C1rcdnbr;
023900151027
024000151027         // Emissione del subfile
024100151027           write TBCOVP01;
024200151027           exfmt TBCOVC01;
024300151027
024400151027         // Controllo tasti funzionali del subfile
024500151027           SELECT;
024600151027
024700151027         // F3=Fine
024800151027           WHEN  *inKC;
024900151027             wVideo = *blanks;
025000151027             BCOVricez = 'C';
025100151027             exsr RoutEnd;
025200151027
025300151027         // F5=Refresh
025400151027           WHEN  *inKE;
025500151027             wCarS01 = *on;
025600151027
025700151027         // F10=Immissione
025800151027           WHEN  *inKJ;
025900151027             wVideo = 'D01';
026000151027             clear TNTBCOVDS;
026100151027             BCOVcomand = 'J';
026200151027             exsr GesD01;
026300151027
026400151027         // F13=Ripetizione
026500151027           WHEN  *inKM;
026600151027             exsr RipetiOpz;
026700151027
026800151027         // In tutti gli altri casi
026900151027           OTHER;
027000151027         // Controllo ed elaborazione scelte su subfile
027100151027             exsr ContrS01;
027200151027           ENDSL;
027300151027
027400151027       // Fine elaborazione 'S01'
027500151027          ENDDO;
027600151027
027700151027        ENDSR;
027800151027
027900151027       //--------------------------------------------------------------
028000151027       //?Carica video S01.
028100151027       //--------------------------------------------------------------
028200151027       BEGSR Cars01;
028300151027
028400151027       // Se è stato richiesto il caricamento del subfile
028500151027         IF  wCarS01;
028600151027
028700151027         // Inizializzo il subfile
028800151027           S01nrr = 0;
028900151027           *in20 = *on;
029000151027           write TBCOVC01;
029100151027           *in20 = *off;
029200151027           *in21 = *off;
029300151027           *in22 = *off;
029400151027           *in23 = *off;
029500151027
029600151027         // Imposto la chiave di posizionamento e lettura file
029700151027           TBEcod = 'COV';
029800151027           TBEke1 = C1setll;
029900151027
030000151027         // Se è stato scelto il posizionamento
030100151027           IF  C1setll <> *blanks;
030200151027             wKey = '2';
030300151027         // Altrimenti
030400151027           ELSE;
030500151027             wKey = '1';
030600151027           ENDIF;
030700151027
030800151027         // Posizionamento file
030900151027           exsr SetllTBE01;
031000151027
031100151027         // Fino a che non è fine file
031200151027           DOU  %eof(TNTBE01L);
031300151027
031400151027           // Leggo file
031500151027             reade(n) TBEcod TNTBE01L;
031600151027
031700151027           // Fine file esco
031800151027             IF  %eof(TNTBE01L);
031900151027               leave;
032000151027             ENDIF;
032100151027
032200151027           // Se in "ricerca/scelta" non carico i records annullati
032300151027             IF  (*in01 and TBEatb = *blanks) or not *in01;
032400151027           // Scrivo subfile
032500151027               clear S1opzione;
032600151027               exsr WrtS01;
032700151027             ENDIF;
032800151027
032900151027           ENDDO;
033000151027
033100151027       // Fine caricamento subfile
033200151027         ENDIF;
033300151027
033400151027       // Se scritto almeno un record
033500151027         IF  S01nrr > 0;
033600151027       // Indicatore di sflend
033700151027           *in21 = *on;
033800151027         ENDIF;
033900151027
034000151027       // Se non scritto neanche un record
034100151027         IF  S01nrr = 0;
034200151027       // Indicatore di sfldsp
034300151027           *in23 = *on;
034400151027         ENDIF;
034500151027
034600151027       // Disattivo opzione di caricamento subfile
034700151027         wCarS01 = *off;
034800151027
034900151027       ENDSR;
035000151027
035100151027       //--------------------------------------------------------------
035200151027       //?Carica video D01.
035300151027       //--------------------------------------------------------------
035400151027       BEGSR GesD01;
035500151027
035600151027       // Imposto il video a seconda della funzione richiesta
035700151027         exsr CarD01;
035800151027
035900151027       // Imposto variabile
036000151027         wVideo = 'D01';
036100151027
036200151027       // Fino a che la variabile resta settata come 'D01'
036300151027         DOU  wVideo <> 'D01';
036400151027
036500151027           *in02 = *off;
036600151027           *in03 = *off;
036700151027           *in04 = *off;
036800151027
036900151027         // se immessa opzione di 'scelta'
037000151027           IF  BCOVopzio = '01';
037100151027             BCOVke1 = S1TBEke1;
037200151027             BCOVke2 = S1TBEke2;
037300151027             exsr RoutEnd;
037400151027
037500151027         // Se immessa un'altra opzione
037600151027           ELSE;
037700151027           // Se non è immissione/copia proteggo il campo della causale
037800151027             IF  BCOVcomand <> 'J' and BCOVopzio <> '03';
037900151027               *in02 = *on;
038000151027             ENDIF;
038100151027           // Se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
038200151027           // proteggo i campi del video
038300151027             IF  BCOVopzio = '04' or BCOVopzio = '05';
038400151027               *in03 = *on;
038500151027             ENDIF;
038600151027           // Se immessa opzione di 'visualizzazione'
038700151027           // non attivo F6
038800151027             IF  BCOVopzio = '05';
038900151027               *in04 = *on;
039000151027             ENDIF;
039100151027           // emetto il video
039200151027             exfmt TBCOVD01;
039300151027           ENDIF;
039400151027
039500151027         // Reset indicatore generico di Errore
039600151027           *in28 = *off;
039700151027
039800151027         // pulisco il campo messaggi
039900151027           clear VD1Msg;
040000151027
040100151027         // controllo tasti funzionali del video
040200151027           SELECT;
040300151027
040400151027         // F3=Fine
040500151027           WHEN  *inKC;
040600151027             BCOVricez = 'C';
040700151027             wVideo = *blanks;
040800151027             unlock TNTBE01L;
040900151027             exsr RoutEnd;
041000151027
041100151027         // F6=Conferma
041200151027           WHEN  *inKF;
041300151027             BCOVricez = 'F';
041400151027           // Controllo e decodifico i dati del video
041500151027             exsr ContrD01;
041600151027           // Conferma per annullo/ripristino
041700151027             IF  BCOVopzio = '04';
041800151027               SELECT;
041900151027             // Annullamento
042000151027               WHEN TBEatb = *blanks;
042100151027                 TBEatb = 'A';
042200151027             // Ripristino
042300151027               WHEN TBEatb = 'A';
042400151027                 clear TBEatb;
042500151027               ENDSL;
042600151027             ENDIF;
042700151027
042800151027           // Se non riscontrati errori emetto la finestra con i dati per la trasmissione
042900151027             IF  not *in28;
043000151027               wCarW01 = *on;
043100151027               wVideo = 'W01';
043200151027               exsr GesW01;
043300151027             ENDIF;
043400151027
043500151027         // F8=Record successivo
043600151027           WHEN  *inKH;
043700151027             clear S1opzione;
043800151027             wCarS01 = *off;
043900151027             wVideo = 'S01';
044000151027             BCOVricez = 'H';
044100151027
044200151027         // F12=Ritorno
044300151027           WHEN  *inKL;
044400151027             clear S1opzione;
044500151027             BCOVricez = 'L';
044600151027             unlock TNTBE01L;
044700151027           // Se non è f12 da immissione/copia non ricarico il subfile
044800151027             IF  BCOVcomand = 'J' or BCOVopzio = '03';
044900151027               wCarS01 = *on;
045000151027             ELSE;
045100151027               wCarS01 = *off;
045200151027             ENDIF;
045300151027             wVideo = 'S01';
045400151027
045500151027         // Invio
045600151027           OTHER;
045700151027             IF  not *in03;
045800151027               exsr ContrD01;
045900151027             ENDIF;
046000151027
046100151027          ENDSL;
046200151027
046300151027       // Fine gestione 'D01'
046400151027        ENDDO;
046500151027
046600151027       ENDSR;
046700151027
046800151027       //--------------------------------------------------------------
046900151027       //?Ripeti Opzione S01.
047000151027       //--------------------------------------------------------------
047100151027       BEGSR RipetiOpz;
047200151027
047300151027         IF  C1csrrrn > 0;
047400151027           S01nrr = C1csrrrn;
047500151027           chain S01nrr TBCOVS01;
047600151027           IF  %found and S1opzione > 0;
047700151027             savopzione = S1opzione;
047800151027             *in22 = *on;
047900151027             update TBCOVS01;
048000151027
048100151027             wEoFS01 = *off;
048200151027             DOU  wEoFS01;
048300151027               S01nrr += 1;
048400151027               chain S01nrr TBCOVS01;
048500151027               IF  %found;
048600151027                 S1opzione = savopzione;
048700151027                 update TBCOVS01;
048800151027               ELSE;
048900151027                 wEoFS01 = *on;
049000151027               ENDIF;
049100151027             ENDDO;
049200151027
049300151027             *in22 = *off;
049400151027           ENDIF;
049500151027
049600151027         ENDIF;
049700151027
049800151027       ENDSR;
049900151027
050000151027       //--------------------------------------------------------------
050100151027       //?Carico dati video D01.
050200151027       //--------------------------------------------------------------
050300151027       BEGSR CarD01;
050400151027
050500151027       // Pulisco il formato video D01
050600151027         exsr PulisciD01;
050700151027
050800151027       // Controllo se 'immissione' 'modifica' 'copia' o altro
050900151027         SELECT;
051000151027
051100151027       // F10=Immissione
051200151027         WHEN  BCOVcomand = 'J';
051300151027           D1descopz = 'Immissione';
051400151027
051500151027       // Opzione "02" = modifica
051600151027         WHEN  BCOVopzio = '02';
051700151027           D1descopz = 'Modifica';
051800151027           exsr RieD01;
051900151027
052000151027       // Opzione "03" = copia
052100151027         WHEN  BCOVopzio = '03';
052200151027           D1descopz = 'Copia';
052300151027           exsr RieD01;
052400151027
052500151027       // Opzione "04" = cancellazione/ripristino
052600151027         WHEN  BCOVopzio = '04';
052700151027           exsr RieD01;
052800151027         // Se richiesta 'cancellazione'
052900151027           IF  TBEatb = *blanks;
053000151027             D1descopz = 'Annullamento';
053100151027           ENDIF;
053200151027         // Se richiesto 'ripristino'
053300151027           IF  TBEatb = 'A';
053400151027             D1descopz = 'Ripristino';
053500151027           ENDIF;
053600151027
053700151027       // Opzione "05" = visualizzazione
053800151027         WHEN  BCOVopzio ='05';
053900151027           D1descopz = 'Visualizzazione';
054000151027           exsr RieD01;
054100151027
054200151027       // Fine scelta
054300151027         ENDSL;
054400151027
054500151027       // Centro la descrizione della funzione nel formato video
054600151027         len = (%len(D1descopz) - %len(%trimr(D1descopz))) / 2;
054700151027         D1descopz = %subst(blanks:1:len) + %trimr(D1descopz);
054800151027
054900151027       ENDSR;
055000151027
055100151027       //--------------------------------------------------------------
055200151027       //?Controllo dati video D01.
055300151027       //--------------------------------------------------------------
055400151027       BEGSR ContrD01;
055500151027
055600151027         *in28 = *off;
055700151027         *in40 = *off;
055800151027         *in41 = *off;
055900151027         clear D1TBEuni;
056000151027
056100151027       // Chiave 1 tabella
056200151027         IF  D1TBEke1 = *blanks;
056300151027           VD1Msg = Msg(01);
056400151027           *in28 = *on;
056500151027           *in40 = *on;
056600151027           leavesr;
056700151027         ENDIF;
056800151027
056900151027         IF D1TBEke1 <> 'POE ' and D1TBEke1 <> 'COR ' and
057000151027            D1TBEke1 <> 'COR7';
057100151027           VD1Msg = Msg(02);
057200151027           *in28 = *on;
057300151027           *in40 = *on;
057400151027           leavesr;
057500151027         ENDIF;
057600151027
057700151027       // Chiave 2 tabella
057800151027         IF  D1TBEke2 = *blanks;
057900151027           VD1Msg = Msg(04);
058000151027           *in28 = *on;
058100151027           *in41 = *on;
058200151027           leavesr;
058300151027         ENDIF;
058400151027
058500151027       // Congruenza tra key 1 e key 2 + controllo + decodifica
058600151027         SELECT;
058700151027         WHEN  D1TBEke1 = 'POE';
058800151027           IF  %subst(D1TBEke2:4:7) <> *blanks;
058900151027             VD1Msg = Msg(05);
059000151027             *in28 = *on;
059100151027             *in41 = *on;
059200151027             leavesr;
059300151027           ENDIF;
059400151027           IF  %check(digits:%subst(D1TBEke2:1:3)) > *zeros;
059500151027             VD1Msg = Msg(05);
059600151027             *in28 = *on;
059700151027             *in41 = *on;
059800151027             leavesr;
059900151027           ENDIF;
060000151027           w0030 = %dec(%subst(D1TBEke2:1:3):3:0);
060100151027           chain w0030 AZORG01L;
060200151027           IF  not %found(AZORG01L) or ORGfva <> *blanks;
060300151027             VD1Msg = Msg(05);
060400151027             *in28 = *on;
060500151027             *in41 = *on;
060600151027             leavesr;
060700151027           ENDIF;
060800151027           D1TBEuni = ORGdes;
060900151027         WHEN  D1TBEke1 = 'COR';
061000151027           IF  %check(digits:D1TBEke2) > *zeros;
061100151027             VD1Msg = Msg(05);
061200151027             *in28 = *on;
061300151027             *in41 = *on;
061400151027             leavesr;
061500151027           ENDIF;
061600151027           w0100 = %dec(D1TBEke2:10:0);
061700151027           chain w0100 FNACR01L;
061800151027           IF  not %found(FNACR01L) or ACRatb <> *blanks or
061900151027               ACRtcr = 'M';
062000151027             VD1Msg = Msg(05);
062100151027             *in28 = *on;
062200151027             *in41 = *on;
062300151027             leavesr;
062400151027           ENDIF;
062500151027           D1TBEuni = ACRrsr;
062600151027         WHEN  D1TBEke1 = 'COR7';
062700151027           IF  %subst(D1TBEke2:8:3) <> *blanks;
062800151027             VD1Msg = Msg(05);
062900151027             *in28 = *on;
063000151027             *in41 = *on;
063100151027             leavesr;
063200151027           ENDIF;
063300151027           IF  %check(digits:%subst(D1TBEke2:1:7)) > *zeros;
063400151027             VD1Msg = Msg(05);
063500151027             *in28 = *on;
063600151027             *in41 = *on;
063700151027             leavesr;
063800151027           ENDIF;
063900151027           w0070 = %dec(%subst(D1TBEke2:1:7):7:0);
064000151027           chain (w0010:DUTkci:w0070) CNACO00F;
064100151027           IF  not %found(CNACO00F) or ACOabl <> *blanks;
064200151027             VD1Msg = Msg(05);
064300151027             *in28 = *on;
064400151027             *in41 = *on;
064500151027             leavesr;
064600151027           ENDIF;
064700151027           D1TBEuni = ACOrag;
064800151027         ENDSL;
064900151027
065000151027       // Se immissione controllo che non esista già
065100151027         IF  BCOVcomand = 'J' or BCOVopzio = '03';
065200151027           TBEcod = 'COV';
065300151027           TBEke1 = D1TBEke1;
065400151027           TBEke2 = D1TBEke2;
065500151027           clear TBElin;
065600151027           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
065700151027           IF  %found(TNTBE01L);
065800151027             VD1Msg = Msg(03);
065900151027             *in28 = *on;
066000151027             *in40 = *on;
066100151027             leavesr;
066200151027           ENDIF;
066300151027         ENDIF;
066400151027
066500151027       ENDSR;
066600151027
066700151027       //--------------------------------------------------------------
066800151027       //?Gestione video W01.
066900151027       //--------------------------------------------------------------
067000151027       BEGSR GesW01;
067100151027
067200151027       // Imposto i dati a video
067300151027         exsr CarW01;
067400151027
067500151027       // Fino a che la variabile resta settata come 'W01'
067600151027         DOU  wVideo <> 'W01';
067700151027
067800151027         // Reset indicatore generico di Errore
067900151027           *in28 = *off;
068000151027
068100151027         // Emetto il video
068200151027           exfmt TBCOVW01;
068300151027
068400151027         // Controllo tasti funzionali del video
068500151027           SELECT;
068600151027
068700151027         // F6=Conferma
068800151027           WHEN  *inKF;
068900151027           // Controllo i dati del video
069000151027             exsr ContrW01;
069100151027           // Se non riscontrati errori aggiorno il record corrente
069200151027             IF  not *in28;
069300151027               exsr Aggiorna;
069400151027               IF  not *in28;
069500151027                 IF  BCOVcomand = 'J';
069600151027                   wVideo = 'D01';
069700151027                   exsr CarD01;
069800151027                 ELSE;
069900151027                   wVideo = 'S01';
070000151027                 ENDIF;
070100151027               ENDIF;
070200151027             ENDIF;
070300151027
070400151027         // F12=Ritorno
070500151027           WHEN  *inKL;
070600151027             wVideo = 'D01';
070700151027             clear BCOVricez;
070800151027
070900151027         // Invio
071000151027           OTHER;
071100151027             exsr ContrW01;
071200151027
071300151027           ENDSL;
071400151027
071500151027       // Fine gestione 'W01'
071600151027         ENDDO;
071700151027
071800151027       ENDSR;
071900151027
072000151027       //--------------------------------------------------------------
072100151027       //?Carico i dati video W01.
072200151027       //--------------------------------------------------------------
072300151027       BEGSR CarW01;
072400151027
072500151027       // Pulisco i campi
072600151027         clear W1ftt;
072700151027         clear W1flt;
072800151027         clear W1ftr;
072900151027         clear W1dtr;
073000151027
073100151027       // Se non immissione imposto i campi del file
073200151027         IF  BCOVcomand <> 'J';
073300151027           W1ftt = TBEftt;
073400151027           W1flt = TBEflt;
073500151027           W1ftr = TBEftr;
073600151027         // Imposto la data
073700151027           IF  TBEdtr <> *zeros;
073800151027             clear Wlbdat;
073900151027             G02inv = TBEdtr;
074000151027             G02err = '3';
074100151027             xsrda8(wlbdat);
074200151027             W1dtr = G02dat;
074300151027           ENDIF;
074400151027         ENDIF;
074500151027
074600151027       ENDSR;
074700151027
074800151027       //--------------------------------------------------------------
074900151027       //?Controllo video W01.
075000151027       //--------------------------------------------------------------
075100151027       BEGSR ContrW01;
075200151027
075300151027       ENDSR;
075400151027
075500151027       //--------------------------------------------------------------
075600151027       //?Aggiorno la tabella.
075700151027       //--------------------------------------------------------------
075800151027       BEGSR Aggiorna;
075900151027
076000151027       // Imposto campi del file
076100151027         clear TBElin;
076200151027         TBEke1 = D1TBEke1;
076300151027         TBEke2 = D1TBEke2;
076400151027
076500151027         TBEuni = D1TBEuni;
076600151027
076700151027         TBEftt = W1ftt;
076800151027         TBEflt = W1flt;
076900151027         clear TBEftr;
077000151027         clear TBEdtr;
077100151027
077200151027       // Controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
077300151027         SELECT;
077400151027
077500151027       // F10=immissione - Opzione "03"=copia
077600151027         WHEN  BCOVcomand = 'J' or BCOVopzio = '03';
077700151027         // Scrivo nuovo record con gestione Errore per chiave duplicata
077800151027           write TNTBE000;
077900151027
078000151027       // Opzione "02"=modIFica
078100151027         WHEN  BCOVopzio = '02';
078200151027         // update record e controllo Errore per chiave duplicata
078300151027          update TNTBE000;
078400151027
078500151027       // Opzione "04"=cancellazione/ripristino
078600151027         WHEN  BCOVopzio = '04';
078700151027           update TNTBE000;
078800151027
078900151027         ENDSL;
079000151027
079100151027       ENDSR;
079200151027
079300151027       //--------------------------------------------------------------
079400151027       //?Pulizia video D01.
079500151027       //--------------------------------------------------------------
079600151027       BEGSR PulisciD01;
079700151027
079800151027         clear d1descopz;
079900151027         clear d1TBEke1;
080000151027         clear d1TBEke2;
080100151027         clear d1TBEuni;
080200151027
080300151027         *in28 = *off;
080400151027         *in40 = *off;
080500151027         *in41 = *off;
080600151027
080700151027       ENDSR;
080800151027
080900151027       //--------------------------------------------------------------
081000151027       //?Imposto dati video D01.
081100151027       //--------------------------------------------------------------
081200151027       BEGSR RieD01;
081300151027
081400151027       // Recupero i dati dal file
081500151027         kTBEke1 = S1TBEke1;
081600151027         kTBEke2 = S1TBEke2;
081700151027         clear TBElin;
081800151027         chain (TBEcod:kTBEke1:kTBEke2:TBElin) TNTBE01L;
081900151027
082000151027       // Imposto i campi a video
082100151027         D1TBEke1  = TBEke1;
082200151027         D1TBEke2  = TBEke2;
082300151027         D1TBEuni  = TBEuni;
082400151027
082500151027       ENDSR;
082600151027
082700151027       //--------------------------------------------------------------
082800151027       //?Controllo dati video S01.
082900151027       //--------------------------------------------------------------
083000151027       BEGSR ContrS01;
083100151027
083200151027       // Set flag
083300151027         wEoFS01 = *off;
083400151027
083500151027       // Inizio lettura subfile
083600151027         DOW  not wEoFS01 and *in21;
083700151027           readc TBCOVS01;
083800151027         // Se fine subfile
083900151027           IF  %eof;
084000151027             wCarS01 = *on;
084100151027             leave;
084200151027           ENDIF;
084300151027         // Se è stata immessa un'opzione
084400151027           IF  S1opzione <> *zeros;
084500151027           // Reset ds di servizio
084600151027             clear TNTBCOVDS;
084700151027           // Controllo ed elaborazione opzione immessa
084800151027             SELECT;
084900151027           // Opzione "01" = scelta
085000151027             WHEN  S1opzione = 1 and *in01;
085100151027               BCOVopzio = '01';
085200151027           // Opzione "02" = modifica
085300151027             WHEN  S1opzione = 2 and not *in01 and S1TBEatb = *blanks;
085400151027               BCOVopzio = '02';
085500151027           // Opzione "03" = copia
085600151027             WHEN  S1opzione = 3 and not *in01 and S1TBEatb = *blanks;
085700151027               BCOVopzio = '03';
085800151027           // Opzione "04" = annullo/ripristino
085900151027             WHEN  S1opzione = 4 and not *in01;
086000151027               BCOVopzio = '04';
086100151027           // Opzione "05" = visualizzazione
086200151027             WHEN  S1opzione = 5;
086300151027               BCOVopzio = '05';
086400151027             ENDSL;
086500151027
086600151027           // Se immessa almeno un opzione valida
086700151027             IF  BCOVopzio <> *blanks;
086800151027             // Gstione del formato video
086900151027               exsr GesD01;
087000151027             // Se la gestione si è chiusa con ...
087100151027               SELECT;
087200151027             // F6=Conferma
087300151027               WHEN  BCOVricez = 'F';
087400151027                 clear S1opzione;
087500151027                 wCarS01 = *on;
087600151027             // F12=Ritorno
087700151027               WHEN  BCOVricez = 'L';
087800151027                 clear S1opzione;
087900151027                 wEoFS01 = *on;
088000151027             // Altrimenti
088100151027               OTHER;
088200151027                 *in22 = *on;
088300151027               ENDSL;
088400151027
088500151027               update TBCOVS01;
088600151027               *in22 = *off;
088700151027           // Fine opzione
088800151027             ENDIF;
088900151027         // Fine opzione
089000151027           ENDIF;
089100151027
089200151027         ENDDO;
089300151027
089400151027       ENDSR;
089500151027
089600151027       //--------------------------------------------------------------
089700151027       //?Posizionamento sul file tabelle.
089800151027       //--------------------------------------------------------------
089900151027       BEGSR SetllTBE01;
090000151027
090100151027         SELECT;
090200151027       // Chiave totale
090300151027         WHEN wKey = '1';
090400151027           setll TBEcod TNTBE01L;
090500151027       // chiave parziale
090600151027         WHEN wKey = '2';
090700151027           setll (TBEcod:TBEke1) TNTBE01L;
090800151027         ENDSL;
090900151027
091000151027       ENDSR;
091100151027
091200151027       //--------------------------------------------------------------
091300151027       //?Scrive video S01.
091400151027       //--------------------------------------------------------------
091500151027       BEGSR WrtS01;
091600151027
091700151027       // Se non raggiunto limite massimo di caricamento
091800151027         IF S01nrr < 9999;
091900151027         // Imposto campi di subfile
092000151027           exsr RieS01;
092100151027         // Imposto numeratore righe e numero relativo di record
092200151027           S01nrr += 1;
092300151027         // scrivo subfile
092400151027           write TBCOVS01;
092500151027         ENDIF;
092600151027
092700151027       ENDSR;
092800151027
092900151027       //--------------------------------------------------------------
093000151027       //?Imposto campi video S01.
093100151027       //--------------------------------------------------------------
093200151027       BEGSR RieS01;
093300151027
093400151027         S1TBEke1 = TBEke1;
093500151027         S1TBEke2 = TBEke2;
093600151027         S1TBEuni = TBEuni;
093700151027         S1TBEatb = TBEatb;
093800151027
093900151027       ENDSR;
094000151027
094100151027       //--------------------------------------------------------------
094200151027       //?Operazioni finali.
094300151027       //--------------------------------------------------------------
094400151027       BEGSR RoutEnd;
094500151027
094600151027         IF  BCOVesito = *blanks;
094700151027           BCOVesito = Eseguito;
094800151027         ENDIF;
094900151027
095000151027         kpjbu = TNTBCOVDS;
095100151027
095200151027         *inLR = *on;
095300151027         return;
095400151027
095500151027       ENDSR;
095600151027
095700151027      /end-free
095800151027
095900151027       //--------------------------------------------------------------
096000151027       //?Schiere a tempo di compilazione.
096100151027       //--------------------------------------------------------------
096200151027
096300151027** -Msg----------------------------------------------------------------------*
096400151027Immettere la Key 1                                                             01
096500151027Key 1 non prevista                                                             02
096600151027Tabella già esistente                                                          03
096700151027Immettere la Key 2                                                             04
096800151027Key 2 errata o non valida con Key 1                                            05
