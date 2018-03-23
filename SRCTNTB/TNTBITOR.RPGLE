000100141001      //--------------------------------------------------------------
000200141003      //?TNTBITOR - Gestione tabella "ITO" Info Trattativa
000300141001      //--------------------------------------------------------------
000400141001     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500141001
000600141001      //--------------------------------------------------------------
000700141001      //?Dichiarazione file.
000800141001      //---------------------------------------------------------------
000900141001      // - File TABELLE
001000141001     fTNTBE01L  uf a e           k disk
001100141001     fTNTBE11L  uf a e           k disk    rename(TNTBE000:TNTBE11)
001200141001
001300141001      // - ORGANIGRAMMA
001400141001     fAZUTE01L  if   e           k disk
001500141001
001600141001      // - Video
001700141003     fTNTBITOD  cf   e             workstn sfile(TBITOS01:S01nrr)
001800141001
001900141001      //---------------------------------------------------------------
002000141001      //?Definizione costanti.
002100141001      //---------------------------------------------------------------
002200141001     d Errore          c                   '1'
002300141001     d Eseguito        c                   '0'
002400141001
002500141001      //---------------------------------------------------------------
002600141001      //?Definizione schiere.
002700141001      //---------------------------------------------------------------
002800141001     d Msg             s             78    dim(17) ctdata perrcd(1)
002900141001
003000141001      //---------------------------------------------------------------
003100141001      //?Definizione aree dati.
003200141001      //---------------------------------------------------------------
003300141001      // - Dati utente
003400141001     d §AzUte        e ds                  extname(AZUTE00F)
003500141001     d                                     dtaara
003600141001     d §DatiUte      e ds                  extname(dDatiUte)
003700141001     d                                     dtaara
003800141001
003900141001      //---------------------------------------------------------------
004000141001      //?Definizione strutture dati.
004100141001      //---------------------------------------------------------------
004200141001      // - Status
004300141001     d Psds           sds
004400141001     d  PGMname          *proc
004500141001
004600141001     d Wlbdat          ds                  inz
004700141001     d  G02dat                 1      8  0
004800141001     d  G02inv                 9     16  0
004900141001     d  G02err                17     17
005000141001     d  G02tgi                18     22  0
005100141001
005200141001      // - Parametri ricevuti
005300141001     d KPJBA         e ds
005400141003     d TNTBITODS     e ds
005500141001
005600141003      // - Interrogazione tabella ITG
005700141003     d TNTBITGDS     e ds
005800141001
005900141001      // - Controllo tabella
006000141001     d TIBS02DS      e ds
006100141001
006200141001      // - Reperimento dati utente
006300141001     d TIBS34DS      e ds
006400141001
006500141003      // - Tabella ITO = Info Trattativa
006600141003     d dITO          e ds
006700141001
006800141003      // - Tabella ITG = Categoria Info Trattativa
006900141003     d dITG          e ds
007000141001
007100141001      //---------------------------------------------------------------
007200141001      //?Definizione variabili globali.
007300141001      //---------------------------------------------------------------
007400141001      // - Flags booleani
007500141001     d wEoFS01         s               n   inz(*off)
007600141001     d wCarS01         s               n   inz(*off)
007700141001     d wCarW01         s               n   inz(*off)
007800141001
007900141001      // - Campi di comodo
008000141001     d blanks          s                   like(D1descopz)
008100141001     d kTBEke1         s                   like(TBEke1)
008200141001     d len             s              5u 0
008300141001     d S01nrr          s              4  0
008400141001     d savopzione      s                   like(S1opzione)
008500141001     d savrcdnbr       s                   like(C1rcdnbr)
008600141001     d wKey            s              1
008700141001     d wVideo          s             10
008800141001
008900141001      //---------------------------------------------------------------
009000141001      //?Definizione procedure usate.
009100141001      //---------------------------------------------------------------
009200141003      // - Interrogazione tabella ITG
009300141003     d tntbitgr        pr                  extpgm('TNTBITGR')
009400141001     d  kpjba                              likeds(kpjba)
009500141001
009600141001      //---------------------------------------------------------------
009700141001      //?Definizione prototipi.
009800141001      //---------------------------------------------------------------
009900141001      /copy gaitrasrc/srcprotopr,tibs02r
010000141001      /copy gaitrasrc/srcprotopr,tibs34r
010100141001      /copy gaitrasrc/srcprotopr,xsrda8
010200141001
010300141001      //---------------------------------------------------------------
010400141001      //?Riepilogo indicatori.
010500141001      //---------------------------------------------------------------
010600141001      // 01 - ON ricerca -- OFF manutenzione
010700141002      // 02 - proteggo campo ke1
010800141001      // 03 - proteggo campi video 01
010900141001      // 04 - visualizzazione
011000141001      // 20 - gestione subfile
011100141001      // 21 - gestione subfile
011200141001      // 22 - gestione subfile
011300141001      // 23 - gestione subfile
011400141001      // 28 - messaggio Errore
011500141001      // 40 - Chiave tabella
011600141001      // 41 - Descrizione
011700141001      // 42 - Descrizione breve
011800141003      // 43 - Categoria Info Trattativa
011900141001
012000141001      //---------------------------------------------------------------
012100141001      //?M A I N - L I N E
012200141001      //---------------------------------------------------------------
012300141001
012400141001     c     *Entry        plist
012500141001     c                   parm                    KPJBA
012600141001
012700141001      /free
012800141001
012900141001       // Operazioni iniziali
013000141001       exsr RoutInz;
013100141001
013200141001       // Gestione Subfile
013300141001       exsr GesS01;
013400141001
013500141001       // Operazioni finali
013600141001       exsr RoutEnd;
013700141001
013800141001       //--------------------------------------------------------------
013900141001       //?Operazioni iniziali.
014000141001       //--------------------------------------------------------------
014100141001       BEGSR RoutInz;
014200141001
014300141001       // Ricezione parametri
014400141003         TNTBITODS = kpjbu;
014500141001
014600141001         SELECT;
014700141001       // Ricerca e scelta
014800141003         WHEN  BITOfun = '1';
014900141001           *in01 = *on;
015000141001       // Manutenzione
015100141003         WHEN  BITOfun = *blanks;
015200141001          *in01 = *off;
015300141001         OTHER;
015400141003           BITOesito = Errore;
015500141001           exsr RoutEnd;
015600141001         ENDSL;
015700141001
015800141001       // Reperimento dati job
015900141001         exsr DatiJob;
016000141001
016100141001       ENDSR;
016200141001
016300141001       //--------------------------------------------------------------
016400141001       //?Reperimento Dati del job (Utente/Operativi).
016500141001       //--------------------------------------------------------------
016600141001       BEGSR DatiJob;
016700141001
016800141001         in(E) §AzUte;
016900141001         IF  not %error;
017000141001           in(E) §DatiUte;
017100141001         ENDIF;
017200141001         IF  %error or RSut = *blanks;
017300141001           clear TIBS34ds;
017400141001           tibs34r(tibs34ds);
017500141001           in §AzUte;
017600141001           in §DatiUte;
017700141001         ENDIF;
017800141001
017900141001       ENDSR;
018000141001
018100141001       //--------------------------------------------------------------
018200141001       //?Gestione video S01.
018300141001       //--------------------------------------------------------------
018400141001       BEGSR GesS01;
018500141001
018600141001       // Imposto variabili
018700141001         wCarS01 = *on;
018800141001         wVideo  = 'S01';
018900141001
019000141001       // Inizio elaborazione subfile
019100141001         DOU  wVideo <> 'S01';
019200141001
019300141001         // Caricamento subfile
019400141001           exsr Cars01;
019500141001
019600141001         // C1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
019700141001         // impostando C1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
019800141001         // l'ultimo tasto
019900141001           IF  C1csrrrn > 0;
020000141001             C1rcdnbr = C1csrrrn;
020100141001           ELSE;
020200141001             C1rcdnbr = savrcdnbr;
020300141001           ENDIF;
020400141001
020500141001         // Se non so quale pagina visualizzare forzo pagina 1
020600141001           IF  C1rcdnbr < 1;
020700141001             C1rcdnbr = 1;
020800141001           ENDIF;
020900141001
021000141001         // Salvo il record number del subfile
021100141001           savrcdnbr = C1rcdnbr;
021200141001
021300141001         // Emissione del subfile
021400141003           write TBITOP01;
021500141003           exfmt TBITOC01;
021600141001
021700141001         // Controllo tasti funzionali del subfile
021800141001           SELECT;
021900141001
022000141001         // F3=Fine
022100141001           WHEN  *inKC;
022200141001             wVideo = *blanks;
022300141003             BITOricez = 'C';
022400141001             exsr RoutEnd;
022500141001
022600141001         // F5=Refresh
022700141001           WHEN  *inKE;
022800141001             wCarS01 = *on;
022900141001
023000141001         // F10=Immissione
023100141001           WHEN  *inKJ;
023200141001             wVideo = 'D01';
023300141003             clear TNTBITODS;
023400141003             BITOcomand = 'J';
023500141001             exsr GesD01;
023600141001
023700141001         // F13=Ripetizione
023800141001           WHEN  *inKM;
023900141001             exsr RipetiOpz;
024000141001
024100141001         // In tutti gli altri casi
024200141001           OTHER;
024300141001         // Controllo ed elaborazione scelte su subfile
024400141001             exsr ContrS01;
024500141001           ENDSL;
024600141001
024700141001       // Fine elaborazione 'S01'
024800141001          ENDDO;
024900141001
025000141001        ENDSR;
025100141001
025200141001       //--------------------------------------------------------------
025300141001       //?Carica video S01.
025400141001       //--------------------------------------------------------------
025500141001       BEGSR Cars01;
025600141001
025700141001       // Se è stato richiesto il caricamento del subfile
025800141001         IF  wCarS01;
025900141001
026000141001         // Inizializzo il subfile
026100141001           S01nrr = 0;
026200141001           *in20 = *on;
026300141003           write TBITOC01;
026400141001           *in20 = *off;
026500141001           *in21 = *off;
026600141001           *in22 = *off;
026700141001           *in23 = *off;
026800141001
026900141001         // Imposto la chiave di posizionamento e lettura file
027000141003           TBEcod = 'ITO';
027100141001           TBEke1 = C1setll;
027200141001
027300141001         // Se è stato scelto il posizionamento
027400141001           IF  C1setll <> *blanks;
027500141001             wKey = '2';
027600141001         // Altrimenti
027700141001           ELSE;
027800141001             wKey = '1';
027900141001           ENDIF;
028000141001
028100141001         // Posizionamento file
028200141001           exsr SetllTBE01;
028300141001
028400141001         // Fino a che non è fine file
028500141001           DOU  %eof(TNTBE01L);
028600141001
028700141001           // Leggo file
028800141001             reade(n) TBEcod TNTBE01L;
028900141001
029000141001           // Fine file esco
029100141001             IF  %eof(TNTBE01L);
029200141001               leave;
029300141001             ENDIF;
029400141001
029500141001           // Se in "ricerca/scelta" non carico i records annullati
029600141001             IF  (*in01 and TBEatb = *blanks) or not *in01;
029700141001           // Scrivo subfile
029800141001               clear S1opzione;
029900141001               exsr WrtS01;
030000141001             ENDIF;
030100141001
030200141001           ENDDO;
030300141001
030400141001       // Fine caricamento subfile
030500141001         ENDIF;
030600141001
030700141001       // Se scritto almeno un record
030800141001         IF  S01nrr > 0;
030900141001       // Indicatore di sflend
031000141001           *in21 = *on;
031100141001         ENDIF;
031200141001
031300141001       // Se non scritto neanche un record
031400141001         IF  S01nrr = 0;
031500141001       // Indicatore di sfldsp
031600141001           *in23 = *on;
031700141001         ENDIF;
031800141001
031900141001       // Disattivo opzione di caricamento subfile
032000141001         wCarS01 = *off;
032100141001
032200141001       ENDSR;
032300141001
032400141001       //--------------------------------------------------------------
032500141001       //?Carica video D01.
032600141001       //--------------------------------------------------------------
032700141001       BEGSR GesD01;
032800141001
032900141001       // Imposto il video a seconda della funzione richiesta
033000141001         exsr CarD01;
033100141001
033200141001       // Imposto variabile
033300141001         wVideo = 'D01';
033400141001
033500141001       // Fino a che la variabile resta settata come 'D01'
033600141001         DOU  wVideo <> 'D01';
033700141001
033800141001           *in02 = *off;
033900141001           *in03 = *off;
034000141001           *in04 = *off;
034100141001
034200141001         // se immessa opzione di 'scelta'
034300141003           IF  BITOopzio = '01';
034400141003             BITOke1 = S1TBEke1;
034500141003             BITOdes = S1TBEuni;
034600141001             exsr RoutEnd;
034700141001
034800141001         // Se immessa un'altra opzione
034900141001           ELSE;
035000141001           // Se non è immissione/copia proteggo il campo della causale
035100141003             IF  BITOcomand <> 'J' and BITOopzio <> '03';
035200141001               *in02 = *on;
035300141001             ENDIF;
035400141001           // Se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
035500141001           // proteggo i campi del video
035600141003             IF  BITOopzio = '04' or BITOopzio = '05';
035700141001               *in03 = *on;
035800141001             ENDIF;
035900141001           // Se immessa opzione di 'visualizzazione'
036000141001           // non attivo F6
036100141003             IF  BITOopzio = '05';
036200141001               *in04 = *on;
036300141001             ENDIF;
036400141001           // emetto il video
036500141003             exfmt TBITOD01;
036600141001           ENDIF;
036700141001
036800141001         // Reset indicatore generico di Errore
036900141001           *in28 = *off;
037000141001
037100141001         // pulisco il campo messaggi
037200141001           clear VD1Msg;
037300141001
037400141001         // controllo tasti funzionali del video
037500141001           SELECT;
037600141001
037700141001         // F3=Fine
037800141001           WHEN  *inKC;
037900141003             BITOricez = 'C';
038000141001             wVideo = *blanks;
038100141001             unlock TNTBE01L;
038200141001             exsr RoutEnd;
038300141001
038400141001         // F6=Conferma
038500141001           WHEN  *inKF;
038600141003             BITOricez = 'F';
038700141001           // Controllo e decodIFico i dati del video
038800141001             exsr ContrD01;
038900141001           // Conferma per annullo/ripristino
039000141003             IF  BITOopzio = '04';
039100141001               SELECT;
039200141001             // Annullamento
039300141001               WHEN TBEatb = *blanks;
039400141001                 TBEatb = 'A';
039500141001             // Ripristino
039600141001               WHEN TBEatb = 'A';
039700141001                 clear TBEatb;
039800141001               ENDSL;
039900141001             ENDIF;
040000141001
040100141001           // Se non riscontrati errori emetto la finestra con i dati per la trasmissione
040200141001             IF  not *in28;
040300141001               wCarW01 = *on;
040400141001               wVideo = 'W01';
040500141001               exsr GesW01;
040600141001             ENDIF;
040700141001
040800141001         // F8=Record successivo
040900141001           WHEN  *inKH;
041000141001             clear S1opzione;
041100141001             wCarS01 = *off;
041200141001             wVideo = 'S01';
041300141003             BITOricez = 'H';
041400141001
041500141001         // F12=Ritorno
041600141001           WHEN  *inKL;
041700141001             clear S1opzione;
041800141003             BITOricez = 'L';
041900141001             unlock TNTBE01L;
042000141001           // Se non è f12 da immissione/copia non ricarico il subfile
042100141003             IF  BITOcomand = 'J' or BITOopzio = '03';
042200141001               wCarS01 = *on;
042300141001             ELSE;
042400141001               wCarS01 = *off;
042500141001             ENDIF;
042600141001             wVideo = 'S01';
042700141001
042800141001         // Invio
042900141001           OTHER;
043000141001             IF  not *in03;
043100141001               exsr ContrD01;
043200141001             ENDIF;
043300141001
043400141001          ENDSL;
043500141001
043600141001       // Fine gestione 'D01'
043700141001        ENDDO;
043800141001
043900141001       ENDSR;
044000141001
044100141001       //--------------------------------------------------------------
044200141001       //?Ripeti Opzione S01.
044300141001       //--------------------------------------------------------------
044400141001       BEGSR RipetiOpz;
044500141001
044600141001         IF  C1csrrrn > 0;
044700141001           S01nrr = C1csrrrn;
044800141003           chain S01nrr TBITOS01;
044900141001           IF  %found and S1opzione > 0;
045000141001             savopzione = S1opzione;
045100141001             *in22 = *on;
045200141003             update TBITOS01;
045300141001
045400141001             wEoFS01 = *off;
045500141001             DOU  wEoFS01;
045600141001               S01nrr += 1;
045700141003               chain S01nrr TBITOS01;
045800141001               IF  %found;
045900141001                 S1opzione = savopzione;
046000141003                 update TBITOS01;
046100141001               ELSE;
046200141001                 wEoFS01 = *on;
046300141001               ENDIF;
046400141001             ENDDO;
046500141001
046600141001             *in22 = *off;
046700141001           ENDIF;
046800141001
046900141001         ENDIF;
047000141001
047100141001       ENDSR;
047200141001
047300141001       //--------------------------------------------------------------
047400141001       //?Carico dati video D01.
047500141001       //--------------------------------------------------------------
047600141001       BEGSR CarD01;
047700141001
047800141001       // Pulisco il formato video D01
047900141001         exsr PulisciD01;
048000141001
048100141001       // Controllo se 'immissione' 'modifica' 'copia' o altro
048200141001         SELECT;
048300141001
048400141001       // F10=Immissione
048500141003         WHEN  BITOcomand = 'J';
048600141001           D1descopz = 'Immissione';
048700141001
048800141001       // Opzione "02" = modifica
048900141003         WHEN  BITOopzio = '02';
049000141001           D1descopz = 'ModIFica';
049100141001           exsr RieD01;
049200141001
049300141001       // Opzione "03" = copia
049400141003         WHEN  BITOopzio = '03';
049500141001           D1descopz = 'Copia';
049600141001           exsr RieD01;
049700141001
049800141001       // Opzione "04" = cancellazione/ripristino
049900141003         WHEN  BITOopzio = '04';
050000141001           exsr RieD01;
050100141001         // Se richiesta 'cancellazione'
050200141001           IF  TBEatb = *blanks;
050300141001             D1descopz = 'Annullamento';
050400141001           ENDIF;
050500141001         // Se richiesto 'ripristino'
050600141001           IF  TBEatb = 'A';
050700141001             D1descopz = 'Ripristino';
050800141001           ENDIF;
050900141001
051000141001       // Opzione "05" = visualizzazione
051100141003         WHEN  BITOopzio ='05';
051200141001           D1descopz = 'Visualizzazione';
051300141001           exsr RieD01;
051400141001
051500141001       // Fine scelta
051600141001         ENDSL;
051700141001
051800141001       // Centro la descrizione della funzione nel formato video
051900141001         len = (%len(D1descopz) - %len(%trimr(D1descopz))) / 2;
052000141001         D1descopz = %subst(blanks:1:len) + %trimr(D1descopz);
052100141001
052200141001       ENDSR;
052300141001
052400141001       //--------------------------------------------------------------
052500141001       //?Controllo dati video D01.
052600141001       //--------------------------------------------------------------
052700141001       BEGSR ContrD01;
052800141001
052900141001         *in28 = *off;
053000141001         *in40 = *off;
053100141001         *in41 = *off;
053200141001         *in42 = *off;
053300141001         *in43 = *off;
053400141001
053500141001       // Chiave tabella
053600141001         IF D1TBEke1 = *blanks;
053700141001           VD1Msg = Msg(01);
053800141001           *in28 = *on;
053900141001           *in40 = *on;
054000141001           leavesr;
054100141001         ENDIF;
054200141001
054300141001       // Se immissione controllo che non esista già
054400141003         IF  BITOcomand = 'J' or BITOopzio = '03';
054500141003           TBEcod = 'ITO';
054600141001           TBEke1 = D1TBEke1;
054700141001           TBEke2 = D1TBEke2;
054800141001           clear TBElin;
054900141001           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
055000141001           IF  %found(TNTBE01L);
055100141001             VD1Msg = Msg(03);
055200141001             *in28 = *on;
055300141001             *in40 = *on;
055400141001             leavesr;
055500141001           ENDIF;
055600141001         ENDIF;
055700141001
055800141001       // Descrizione
055900141003         IF  D1ITOdes = *blanks;
056000141001           VD1Msg = Msg(02);
056100141001           *in28 = *on;
056200141001           *in41 = *on;
056300141001           leavesr;
056400141001         ENDIF;
056500141001
056600141001       // Descrizione breve
056700141003         IF  D1ITOdeb = *blanks;
056800141001           VD1Msg = Msg(02);
056900141001           *in28 = *on;
057000141001           *in42 = *on;
057100141001           leavesr;
057200141001         ENDIF;
057300141001
057400141003       // Categoria Info Trattativa
057500141003         clear D1desitg;
057600141003         IF  D1ITOitg = *blanks;
057700141001           VD1Msg = Msg(04);
057800141001           *in28 = *on;
057900141001           *in43 = *on;
058000141001           leavesr;
058100141001         ENDIF;
058200141001       // Ricerca
058300141003         IF  %scan('?' : D1ITOitg) > 0;
058400141001           *in43 = *on;
058500141003           clear TNTBITGDS;
058600141003           BITGfun = '1';
058700141003           kpjbu = TNTBITGDS;
058800141003           tntbitgr (kpjba);
058900141003           TNTBITGDS = kpjbu;
059000141003           IF  BITGesito <> Errore;
059100141003             D1ITOitg = BITGke1;
059200141001           ENDIF;
059300141001         ENDIF;
059400141001       // Controllo
059500141003         IF  D1ITOitg <> *blanks;
059600141003           clear DITG;
059700141001           clear TIBS02DS;
059800141001           T02mod = 'C';
059900141003           T02cod = 'ITG';
060000141003           T02ke1 = D1ITOitg;
060100141001           T02sif = knsif;
060200141001           TNTBE_RicercaControllo  (kpjba : tibs02ds);
060300141001           IF T02err <> *blanks;
060400141001             VD1Msg = Msg(05);
060500141001             *in28 = *on;
060600141001             *in43 = *on;
060700141001             leavesr;
060800141001           ENDIF;
060900141003           dITG = T02uni;
061000141003           D1desitg = §ITGdes;
061100141001         ENDIF;
061200141001
061300141001       ENDSR;
061400141001
061500141001       //--------------------------------------------------------------
061600141001       //?Gestione video W01.
061700141001       //--------------------------------------------------------------
061800141001       BEGSR GesW01;
061900141001
062000141001       // Imposto i dati a video
062100141001         exsr CarW01;
062200141001
062300141001       // Fino a che la variabile resta settata come 'W01'
062400141001         DOU  wVideo <> 'W01';
062500141001
062600141001         // Reset indicatore generico di Errore
062700141001           *in28 = *off;
062800141001
062900141001         // Emetto il video
063000141003           exfmt TBITOW01;
063100141001
063200141001         // Controllo tasti funzionali del video
063300141001           SELECT;
063400141001
063500141001         // F6=Conferma
063600141001           WHEN  *inKF;
063700141001           // Controllo i dati del video
063800141001             exsr ContrW01;
063900141001           // Se non riscontrati errori aggiorno il record corrente
064000141001             IF  not *in28;
064100141001               exsr Aggiorna;
064200141001               IF  not *in28;
064300141003                 IF  BITOcomand = 'J';
064400141001                   wVideo = 'D01';
064500141001                   exsr CarD01;
064600141001                 ELSE;
064700141001                   wVideo = 'S01';
064800141001                 ENDIF;
064900141001               ENDIF;
065000141001             ENDIF;
065100141001
065200141001         // F12=Ritorno
065300141001           WHEN  *inKL;
065400141001             wVideo = 'D01';
065500141003             clear BITOricez;
065600141001
065700141001         // Invio
065800141001           OTHER;
065900141001             exsr ContrW01;
066000141001
066100141001           ENDSL;
066200141001
066300141001       // Fine gestione 'W01'
066400141001         ENDDO;
066500141001
066600141001       ENDSR;
066700141001
066800141001       //--------------------------------------------------------------
066900141001       //?Carico i dati video W01.
067000141001       //--------------------------------------------------------------
067100141001       BEGSR CarW01;
067200141001
067300141001       // Pulisco i campi
067400141001         clear W1ftt;
067500141001         clear W1flt;
067600141001         clear W1ftr;
067700141001         clear W1dtr;
067800141001
067900141001       // Se non immissione imposto i campi del file
068000141003         IF  BITOcomand <> 'J';
068100141001           W1ftt = TBEftt;
068200141001           W1flt = TBEflt;
068300141001           W1ftr = TBEftr;
068400141001         // Imposto la data
068500141001           IF  TBEdtr <> *zeros;
068600141001             clear Wlbdat;
068700141001             G02inv = TBEdtr;
068800141001             G02err = '3';
068900141001             xsrda8(wlbdat);
069000141001             W1dtr = G02dat;
069100141001           ENDIF;
069200141001         ENDIF;
069300141001
069400141001       ENDSR;
069500141001
069600141001       //--------------------------------------------------------------
069700141001       //?Controllo video W01.
069800141001       //--------------------------------------------------------------
069900141001       BEGSR ContrW01;
070000141001
070100141001       ENDSR;
070200141001
070300141001       //--------------------------------------------------------------
070400141001       //?Aggiorno la tabella.
070500141001       //--------------------------------------------------------------
070600141001       BEGSR Aggiorna;
070700141001
070800141001       // Imposto campi del file
070900141001         clear TBElin;
071000141001         TBEke1 = D1TBEke1;
071100141001         TBEke2 = D1TBEke2;
071200141001
071300141003         §ITOdes  = D1ITOdes;
071400141003         §ITOobl  = D1ITOobl;
071500141003         §ITOits  = D1ITOits;
071600141003         §ITOifv  = D1ITOifv;
071700141003         §ITOsnv  = D1ITOsnv;
071800141003         §ITOsns  = D1ITOsns;
071900141003         §ITOsnf  = D1ITOsnf;
072000141003         §ITOsne  = D1ITOsne;
072100141003         §ITOitg  = D1ITOitg;
072200141003         §ITOogi  = D1ITOogi;
072300141003         §ITOtva  = D1ITOtva;
072400141003         §ITOann  = D1ITOann;
072500141003         §ITOtip  = D1ITOtip;
072600141003         §ITOsta  = D1ITOsta;
072700141003         §ITOsgn  = D1ITOsgn;
072800141003         §ITOl1   = D1ITOl1;
072900141003         §ITOnrec = D1ITOnrec;
073000141003         §ITOdeb  = D1ITOdeb;
073100141003         §ITOsnvd = D1ITOsnvd;
073200141001
073300141003         TBEuni = dITO;
073400141001
073500141001         TBEftt = W1ftt;
073600141001         TBEflt = W1flt;
073700141001         clear TBEftr;
073800141001         clear TBEdtr;
073900141001
074000141001       // Controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
074100141001         SELECT;
074200141001
074300141001       // F10=immissione - Opzione "03"=copia
074400141003         WHEN  BITOcomand = 'J' or bITOopzio = '03';
074500141001         // Scrivo nuovo record con gestione Errore per chiave duplicata
074600141001           write TNTBE000;
074700141001
074800141001       // Opzione "02"=modIFica
074900141003         WHEN  BITOopzio = '02';
075000141001         // update record e controllo Errore per chiave duplicata
075100141001          update TNTBE000;
075200141001
075300141001       // Opzione "04"=cancellazione/ripristino
075400141003         WHEN  BITOopzio = '04';
075500141001           update TNTBE000;
075600141001
075700141001         ENDSL;
075800141001
075900141001       ENDSR;
076000141001
076100141001       //--------------------------------------------------------------
076200141001       //?Pulizia video D01.
076300141001       //--------------------------------------------------------------
076400141001       BEGSR PulisciD01;
076500141001
076600141001         clear d1descopz;
076700141001         clear d1TBEke1;
076800141001         clear d1TBEke2;
076900141003         clear D1ITOdes;
077000141003         clear D1ITOobl;
077100141003         clear D1ITOits;
077200141003         clear D1ITOifv;
077300141003         clear D1ITOsnv;
077400141003         clear D1ITOsns;
077500141003         clear D1ITOsnf;
077600141003         clear D1ITOsne;
077700141003         clear D1ITOitg;
077800141003         clear D1desitg;
077900141003         clear D1ITOogi;
078000141003         clear D1ITOtva;
078100141003         clear D1ITOann;
078200141003         clear D1ITOtip;
078300141003         clear D1ITOsta;
078400141003         clear D1ITOsgn;
078500141003         clear D1ITOl1;
078600141003         clear D1ITOnrec;
078700141003         clear D1ITOdeb;
078800141003         clear D1ITOsnvd;
078900141001
079000141001         *in28 = *off;
079100141001         *in40 = *off;
079200141001         *in41 = *off;
079300141001         *in42 = *off;
079400141001         *in43 = *off;
079500141001
079600141001       ENDSR;
079700141001
079800141001       //--------------------------------------------------------------
079900141001       //?Imposto dati video D01.
080000141001       //--------------------------------------------------------------
080100141001       BEGSR RieD01;
080200141001
080300141001       // Recupero i dati dal file
080400141001         kTBEke1 = S1TBEke1;
080500141001         clear TBElin;
080600141001         chain (TBEcod:kTBEke1:S1TBEke2:TBElin) TNTBE01L;
080700141001
080800141001       // Imposto i campi a video
080900141001         D1tbeke2  = TBEke2;
081000141001         D1tbeke1  = TBEke1;
081100141003         dITO      = TBEuni;
081200141003         D1ITOdes  = §ITOdes;
081300141003         D1ITOobl  = §ITOobl;
081400141003         D1ITOits  = §ITOits;
081500141003         D1ITOifv  = §ITOifv;
081600141003         D1ITOsnv  = §ITOsnv;
081700141003         D1ITOsns  = §ITOsns;
081800141003         D1ITOsnf  = §ITOsnf;
081900141003         D1ITOsne  = §ITOsne;
082000141003         D1ITOitg  = §ITOitg;
082100141003         D1ITOogi  = §ITOogi;
082200141003         D1ITOtva  = §ITOtva;
082300141003         D1ITOann  = §ITOann;
082400141003         D1ITOtip  = §ITOtip;
082500141003         D1ITOsta  = §ITOsta;
082600141003         D1ITOsgn  = §ITOsgn;
082700141003         D1ITOl1   = §ITOl1;
082800141003         D1ITOnrec = §ITOnrec;
082900141003         D1ITOdeb  = §ITOdeb;
083000141003         D1ITOsnvd = §ITOsnvd;
083100141001
083200141001       // Decodifico la categoria
083300141003         clear DITG;
083400141001         clear TIBS02DS;
083500141001         T02mod = 'C';
083600141003         T02cod = 'ITG';
083700141003         T02ke1 = D1ITOitg;
083800141001         T02sif = knsif;
083900141001         TNTBE_RicercaControllo  (kpjba : tibs02ds);
084000141001         IF T02err <> *blanks;
084100141001           leavesr;
084200141001         ENDIF;
084300141003         dITG = T02uni;
084400141003         D1desitg = §ITGdes;
084500141001
084600141001       ENDSR;
084700141001
084800141001       //--------------------------------------------------------------
084900141001       //?Controllo dati video S01.
085000141001       //--------------------------------------------------------------
085100141001       BEGSR ContrS01;
085200141001
085300141001       // Set flag
085400141001         wEoFS01 = *off;
085500141001
085600141001       // Inizio lettura subfile
085700141001         DOW  not wEoFS01 and *in21;
085800141003           readc TBITOS01;
085900141001         // Se fine subfile
086000141001           IF  %eof;
086100141001             wCarS01 = *on;
086200141001             leave;
086300141001           ENDIF;
086400141001         // Se è stata immessa un'opzione
086500141001           IF  S1opzione <> *zeros;
086600141001           // Reset ds di servizio
086700141003             clear TNTBITODS;
086800141001           // Controllo ed elaborazione opzione immessa
086900141001             SELECT;
087000141001           // Opzione "01" = scelta
087100141001             WHEN  S1opzione = 1 and *in01;
087200141003               BITOopzio = '01';
087300141001           // Opzione "02" = modifica
087400141001             WHEN  S1opzione = 2 and not *in01 and S1TBEatb = *blanks;
087500141003               BITOopzio = '02';
087600141001           // Opzione "03" = copia
087700141001             WHEN  S1opzione = 3 and not *in01 and S1TBEatb = *blanks;
087800141003               BITOopzio = '03';
087900141001           // Opzione "04" = annullo/ripristino
088000141001             WHEN  S1opzione = 4 and not *in01;
088100141003               BITOopzio = '04';
088200141001           // Opzione "05" = visualizzazione
088300141001             WHEN  S1opzione = 5;
088400141003               BITOopzio = '05';
088500141001             ENDSL;
088600141001
088700141001           // Se immessa almeno un opzione valida
088800141003             IF  BITOopzio <> *blanks;
088900141001             // Gstione del formato video
089000141001               exsr GesD01;
089100141001             // Se la gestione si è chiusa con ...
089200141001               SELECT;
089300141001             // F6=Conferma
089400141003               WHEN  BITOricez = 'F';
089500141001                 clear S1opzione;
089600141001                 wCarS01 = *on;
089700141001             // F12=Ritorno
089800141003               WHEN  BITOricez = 'L';
089900141001                 clear S1opzione;
090000141001                 wEoFS01 = *on;
090100141001             // Altrimenti
090200141001               OTHER;
090300141001                 *in22 = *on;
090400141001               ENDSL;
090500141001
090600141003               update TBITOS01;
090700141001               *in22 = *off;
090800141001           // Fine opzione
090900141001             ENDIF;
091000141001         // Fine opzione
091100141001           ENDIF;
091200141001
091300141001         ENDDO;
091400141001
091500141001       ENDSR;
091600141001
091700141001       //--------------------------------------------------------------
091800141001       //?Posizionamento sul file tabelle.
091900141001       //--------------------------------------------------------------
092000141001       BEGSR SetllTBE01;
092100141001
092200141001         SELECT;
092300141001       // Chiave totale
092400141001         WHEN wKey = '1';
092500141001           setll TBEcod TNTBE01L;
092600141001       // chiave parziale
092700141001         WHEN wKey = '2';
092800141001           setll (TBEcod:TBEke1) TNTBE01L;
092900141001         ENDSL;
093000141001
093100141001       ENDSR;
093200141001
093300141001       //--------------------------------------------------------------
093400141001       //?Scrive video S01.
093500141001       //--------------------------------------------------------------
093600141001       BEGSR WrtS01;
093700141001
093800141001       // Se non raggiunto limite massimo di caricamento
093900141001         IF S01nrr < 9999;
094000141001         // Imposto campi di subfile
094100141001           exsr RieS01;
094200141001         // Imposto numeratore righe e numero relativo di record
094300141001           S01nrr += 1;
094400141001         // scrivo subfile
094500141003           write TBITOS01;
094600141001         ENDIF;
094700141001
094800141001       ENDSR;
094900141001
095000141001       //--------------------------------------------------------------
095100141001       //?Imposto campi video S01.
095200141001       //--------------------------------------------------------------
095300141001       BEGSR RieS01;
095400141001
095500141001         S1TBEke1 = TBEke1;
095600141001         S1TBEke2 = TBEke2;
095700141003         dITO = TBEuni;
095800141003         S1TBEuni = §ITOdes;
095900141001         S1TBEatb = TBEatb;
096000141001
096100141001       ENDSR;
096200141001
096300141001       //--------------------------------------------------------------
096400141001       //?Operazioni finali.
096500141001       //--------------------------------------------------------------
096600141001       BEGSR RoutEnd;
096700141001
096800141003         IF  BITOesito = *blanks;
096900141003           BITOesito = Eseguito;
097000141001         ENDIF;
097100141001
097200141003         kpjbu = TNTBITODS;
097300141001
097400141001         *inLR = *on;
097500141001         return;
097600141001
097700141001       ENDSR;
097800141001
097900141001      /end-free
098000141001
098100141001       //--------------------------------------------------------------
098200141001       //?Schiere a tempo di compilazione.
098300141001       //--------------------------------------------------------------
098400141001
098500141001** -Msg----------------------------------------------------------------------*
098600141001Immettere il codice Info                                                       01
098700141001Immettere la descrizione                                                       02
098800141003Info trattativa già esistente                                                  03
098900141001Immettere la categoria                                                         04
099000141001Categoria errata                                                               05
