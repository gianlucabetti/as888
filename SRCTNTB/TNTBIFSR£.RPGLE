000100141001      //--------------------------------------------------------------
000200141001      //?TNTBIFOR - Gestione tabella "IFO" Info Commerciali
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
001700141001     fTNTBIFOD  cf   e             workstn sfile(TBIFOS01:S01nrr)
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
005400141001     d TNTBIFODS     e ds
005500141001
005600141001      // - Interrogazione tabella IFG
005700141001     d TNTBIFGDS     e ds
005800141001
005900141001      // - Controllo tabella
006000141001     d TIBS02DS      e ds
006100141001
006200141001      // - Reperimento dati utente
006300141001     d TIBS34DS      e ds
006400141001
006500141001      // - Tabella IFO = Info Commerciali
006600141001     d dIFO          e ds
006700141001
006800141001      // - Tabella IFG = Categoria Info Commerciali
006900141001     d dIFG          e ds
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
009200141001      // - Interrogazione tabella IFG
009300141001     d tntbifgr        pr                  extpgm('TNTBIFGR')
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
010700141001      // 02 - proteggo campo fase
010800141001      // 03 - proteggo campi video 01
010900141001      // 04 - visualizzazione
011000141001      // 05 - visualizzo subfile lingue
011100141001      // 20 - gestione subfile
011200141001      // 21 - gestione subfile
011300141001      // 22 - gestione subfile
011400141001      // 23 - gestione subfile
011500141001      // 28 - messaggio Errore
011600141001      // 40 - Chiave tabella
011700141001      // 41 - Descrizione
011800141001      // 42 - Descrizione breve
011900141001      // 43 - Categoria Info Comm.le
012000141001
012100141001      //---------------------------------------------------------------
012200141001      //?M A I N - L I N E
012300141001      //---------------------------------------------------------------
012400141001
012500141001     c     *Entry        plist
012600141001     c                   parm                    KPJBA
012700141001
012800141001      /free
012900141001
013000141001       // Operazioni iniziali
013100141001       exsr RoutInz;
013200141001
013300141001       // Gestione Subfile
013400141001       exsr GesS01;
013500141001
013600141001       // Operazioni finali
013700141001       exsr RoutEnd;
013800141001
013900141001       //--------------------------------------------------------------
014000141001       //?Operazioni iniziali.
014100141001       //--------------------------------------------------------------
014200141001       BEGSR RoutInz;
014300141001
014400141001       // Ricezione parametri
014500141001         TNTBIFODS = kpjbu;
014600141001
014700141001         SELECT;
014800141001       // Ricerca e scelta
014900141001         WHEN  BIFOfun = '1';
015000141001           *in01 = *on;
015100141001       // Manutenzione
015200141001         WHEN  BIFOfun = *blanks;
015300141001          *in01 = *off;
015400141001         OTHER;
015500141001           BIFOesito = Errore;
015600141001           exsr RoutEnd;
015700141001         ENDSL;
015800141001
015900141001       // Reperimento dati job
016000141001         exsr DatiJob;
016100141001
016200141001       ENDSR;
016300141001
016400141001       //--------------------------------------------------------------
016500141001       //?Reperimento Dati del job (Utente/Operativi).
016600141001       //--------------------------------------------------------------
016700141001       BEGSR DatiJob;
016800141001
016900141001         in(E) §AzUte;
017000141001         IF  not %error;
017100141001           in(E) §DatiUte;
017200141001         ENDIF;
017300141001         IF  %error or RSut = *blanks;
017400141001           clear TIBS34ds;
017500141001           tibs34r(tibs34ds);
017600141001           in §AzUte;
017700141001           in §DatiUte;
017800141001         ENDIF;
017900141001
018000141001       ENDSR;
018100141001
018200141001       //--------------------------------------------------------------
018300141001       //?Gestione video S01.
018400141001       //--------------------------------------------------------------
018500141001       BEGSR GesS01;
018600141001
018700141001       // Imposto variabili
018800141001         wCarS01 = *on;
018900141001         wVideo  = 'S01';
019000141001
019100141001       // Inizio elaborazione subfile
019200141001         DOU  wVideo <> 'S01';
019300141001
019400141001         // Caricamento subfile
019500141001           exsr Cars01;
019600141001
019700141001         // C1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
019800141001         // impostando C1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
019900141001         // l'ultimo tasto
020000141001           IF  C1csrrrn > 0;
020100141001             C1rcdnbr = C1csrrrn;
020200141001           ELSE;
020300141001             C1rcdnbr = savrcdnbr;
020400141001           ENDIF;
020500141001
020600141001         // Se non so quale pagina visualizzare forzo pagina 1
020700141001           IF  C1rcdnbr < 1;
020800141001             C1rcdnbr = 1;
020900141001           ENDIF;
021000141001
021100141001         // Salvo il record number del subfile
021200141001           savrcdnbr = C1rcdnbr;
021300141001
021400141001         // Emissione del subfile
021500141001           write TBIFOP01;
021600141001           exfmt TBIFOC01;
021700141001
021800141001         // Controllo tasti funzionali del subfile
021900141001           SELECT;
022000141001
022100141001         // F3=Fine
022200141001           WHEN  *inKC;
022300141001             wVideo = *blanks;
022400141001             BIFOricez = 'C';
022500141001             exsr RoutEnd;
022600141001
022700141001         // F5=Refresh
022800141001           WHEN  *inKE;
022900141001             wCarS01 = *on;
023000141001
023100141001         // F10=Immissione
023200141001           WHEN  *inKJ;
023300141001             wVideo = 'D01';
023400141001             clear TNTBIFODS;
023500141001             BIFOcomand = 'J';
023600141001             exsr GesD01;
023700141001
023800141001         // F13=Ripetizione
023900141001           WHEN  *inKM;
024000141001             exsr RipetiOpz;
024100141001
024200141001         // In tutti gli altri casi
024300141001           OTHER;
024400141001         // Controllo ed elaborazione scelte su subfile
024500141001             exsr ContrS01;
024600141001           ENDSL;
024700141001
024800141001       // Fine elaborazione 'S01'
024900141001          ENDDO;
025000141001
025100141001        ENDSR;
025200141001
025300141001       //--------------------------------------------------------------
025400141001       //?Carica video S01.
025500141001       //--------------------------------------------------------------
025600141001       BEGSR Cars01;
025700141001
025800141001       // Se è stato richiesto il caricamento del subfile
025900141001         IF  wCarS01;
026000141001
026100141001         // Inizializzo il subfile
026200141001           S01nrr = 0;
026300141001           *in20 = *on;
026400141001           write TBIFOC01;
026500141001           *in20 = *off;
026600141001           *in21 = *off;
026700141001           *in22 = *off;
026800141001           *in23 = *off;
026900141001
027000141001         // Imposto la chiave di posizionamento e lettura file
027100141001           TBEcod = 'IFO';
027200141001           TBEke1 = C1setll;
027300141001
027400141001         // Se è stato scelto il posizionamento
027500141001           IF  C1setll <> *blanks;
027600141001             wKey = '2';
027700141001         // Altrimenti
027800141001           ELSE;
027900141001             wKey = '1';
028000141001           ENDIF;
028100141001
028200141001         // Posizionamento file
028300141001           exsr SetllTBE01;
028400141001
028500141001         // Fino a che non è fine file
028600141001           DOU  %eof(TNTBE01L);
028700141001
028800141001           // Leggo file
028900141001             reade(n) TBEcod TNTBE01L;
029000141001
029100141001           // Fine file esco
029200141001             IF  %eof(TNTBE01L);
029300141001               leave;
029400141001             ENDIF;
029500141001
029600141001           // Se in "ricerca/scelta" non carico i records annullati
029700141001             IF  (*in01 and TBEatb = *blanks) or not *in01;
029800141001           // Scrivo subfile
029900141001               clear S1opzione;
030000141001               exsr WrtS01;
030100141001             ENDIF;
030200141001
030300141001           ENDDO;
030400141001
030500141001       // Fine caricamento subfile
030600141001         ENDIF;
030700141001
030800141001       // Se scritto almeno un record
030900141001         IF  S01nrr > 0;
031000141001       // Indicatore di sflend
031100141001           *in21 = *on;
031200141001         ENDIF;
031300141001
031400141001       // Se non scritto neanche un record
031500141001         IF  S01nrr = 0;
031600141001       // Indicatore di sfldsp
031700141001           *in23 = *on;
031800141001         ENDIF;
031900141001
032000141001       // Disattivo opzione di caricamento subfile
032100141001         wCarS01 = *off;
032200141001
032300141001       ENDSR;
032400141001
032500141001       //--------------------------------------------------------------
032600141001       //?Carica video D01.
032700141001       //--------------------------------------------------------------
032800141001       BEGSR GesD01;
032900141001
033000141001       // Imposto il video a seconda della funzione richiesta
033100141001         exsr CarD01;
033200141001
033300141001       // Imposto variabile
033400141001         wVideo = 'D01';
033500141001
033600141001       // Fino a che la variabile resta settata come 'D01'
033700141001         DOU  wVideo <> 'D01';
033800141001
033900141001           *in02 = *off;
034000141001           *in03 = *off;
034100141001           *in04 = *off;
034200141001
034300141001         // se immessa opzione di 'scelta'
034400141001           IF  BIFOopzio = '01';
034500141001             BIFOke1 = S1TBEke1;
034600141001             BIFOdes = S1TBEuni;
034700141001             exsr RoutEnd;
034800141001
034900141001         // Se immessa un'altra opzione
035000141001           ELSE;
035100141001           // Se non è immissione/copia proteggo il campo della causale
035200141001             IF  BIFOcomand <> 'J' and BIFOopzio <> '03';
035300141001               *in02 = *on;
035400141001             ENDIF;
035500141001           // Se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
035600141001           // proteggo i campi del video
035700141001             IF  BIFOopzio = '04' or BIFOopzio = '05';
035800141001               *in03 = *on;
035900141001             ENDIF;
036000141001           // Se immessa opzione di 'visualizzazione'
036100141001           // non attivo F6
036200141001             IF  BIFOopzio = '05';
036300141001               *in04 = *on;
036400141001             ENDIF;
036500141001           // emetto il video
036600141001             exfmt TBIFOD01;
036700141001           ENDIF;
036800141001
036900141001         // Reset indicatore generico di Errore
037000141001           *in28 = *off;
037100141001
037200141001         // pulisco il campo messaggi
037300141001           clear VD1Msg;
037400141001
037500141001         // controllo tasti funzionali del video
037600141001           SELECT;
037700141001
037800141001         // F3=Fine
037900141001           WHEN  *inKC;
038000141001             BIFOricez = 'C';
038100141001             wVideo = *blanks;
038200141001             unlock TNTBE01L;
038300141001             exsr RoutEnd;
038400141001
038500141001         // F6=Conferma
038600141001           WHEN  *inKF;
038700141001             BIFOricez = 'F';
038800141001           // Controllo e decodIFico i dati del video
038900141001             exsr ContrD01;
039000141001           // Conferma per annullo/ripristino
039100141001             IF  BIFOopzio = '04';
039200141001               SELECT;
039300141001             // Annullamento
039400141001               WHEN TBEatb = *blanks;
039500141001                 TBEatb = 'A';
039600141001             // Ripristino
039700141001               WHEN TBEatb = 'A';
039800141001                 clear TBEatb;
039900141001               ENDSL;
040000141001             ENDIF;
040100141001
040200141001           // Se non riscontrati errori emetto la finestra con i dati per la trasmissione
040300141001             IF  not *in28;
040400141001               wCarW01 = *on;
040500141001               wVideo = 'W01';
040600141001               exsr GesW01;
040700141001             ENDIF;
040800141001
040900141001         // F8=Record successivo
041000141001           WHEN  *inKH;
041100141001             clear S1opzione;
041200141001             wCarS01 = *off;
041300141001             wVideo = 'S01';
041400141001             BIFOricez = 'H';
041500141001
041600141001         // F12=Ritorno
041700141001           WHEN  *inKL;
041800141001             clear S1opzione;
041900141001             BIFOricez = 'L';
042000141001             unlock TNTBE01L;
042100141001           // Se non è f12 da immissione/copia non ricarico il subfile
042200141001             IF  BIFOcomand = 'J' or BIFOopzio = '03';
042300141001               wCarS01 = *on;
042400141001             ELSE;
042500141001               wCarS01 = *off;
042600141001             ENDIF;
042700141001             wVideo = 'S01';
042800141001
042900141001         // Invio
043000141001           OTHER;
043100141001             IF  not *in03;
043200141001               exsr ContrD01;
043300141001             ENDIF;
043400141001
043500141001          ENDSL;
043600141001
043700141001       // Fine gestione 'D01'
043800141001        ENDDO;
043900141001
044000141001       ENDSR;
044100141001
044200141001       //--------------------------------------------------------------
044300141001       //?Ripeti Opzione S01.
044400141001       //--------------------------------------------------------------
044500141001       BEGSR RipetiOpz;
044600141001
044700141001         IF  C1csrrrn > 0;
044800141001           S01nrr = C1csrrrn;
044900141001           chain S01nrr TBIFOS01;
045000141001           IF  %found and S1opzione > 0;
045100141001             savopzione = S1opzione;
045200141001             *in22 = *on;
045300141001             update TBIFOS01;
045400141001
045500141001             wEoFS01 = *off;
045600141001             DOU  wEoFS01;
045700141001               S01nrr += 1;
045800141001               chain S01nrr TBIFOS01;
045900141001               IF  %found;
046000141001                 S1opzione = savopzione;
046100141001                 update TBIFOS01;
046200141001               ELSE;
046300141001                 wEoFS01 = *on;
046400141001               ENDIF;
046500141001             ENDDO;
046600141001
046700141001             *in22 = *off;
046800141001           ENDIF;
046900141001
047000141001         ENDIF;
047100141001
047200141001       ENDSR;
047300141001
047400141001       //--------------------------------------------------------------
047500141001       //?Carico dati video D01.
047600141001       //--------------------------------------------------------------
047700141001       BEGSR CarD01;
047800141001
047900141001       // Pulisco il formato video D01
048000141001         exsr PulisciD01;
048100141001
048200141001       // Controllo se 'immissione' 'modifica' 'copia' o altro
048300141001         SELECT;
048400141001
048500141001       // F10=Immissione
048600141001         WHEN  BIFOcomand = 'J';
048700141001           D1descopz = 'Immissione';
048800141001
048900141001       // Opzione "02" = modifica
049000141001         WHEN  BIFOopzio = '02';
049100141001           D1descopz = 'ModIFica';
049200141001           exsr RieD01;
049300141001
049400141001       // Opzione "03" = copia
049500141001         WHEN  BIFOopzio = '03';
049600141001           D1descopz = 'Copia';
049700141001           exsr RieD01;
049800141001
049900141001       // Opzione "04" = cancellazione/ripristino
050000141001         WHEN  BIFOopzio = '04';
050100141001           exsr RieD01;
050200141001         // Se richiesta 'cancellazione'
050300141001           IF  TBEatb = *blanks;
050400141001             D1descopz = 'Annullamento';
050500141001           ENDIF;
050600141001         // Se richiesto 'ripristino'
050700141001           IF  TBEatb = 'A';
050800141001             D1descopz = 'Ripristino';
050900141001           ENDIF;
051000141001
051100141001       // Opzione "05" = visualizzazione
051200141001         WHEN  BIFOopzio ='05';
051300141001           D1descopz = 'Visualizzazione';
051400141001           exsr RieD01;
051500141001
051600141001       // Fine scelta
051700141001         ENDSL;
051800141001
051900141001       // Centro la descrizione della funzione nel formato video
052000141001         len = (%len(D1descopz) - %len(%trimr(D1descopz))) / 2;
052100141001         D1descopz = %subst(blanks:1:len) + %trimr(D1descopz);
052200141001
052300141001       ENDSR;
052400141001
052500141001       //--------------------------------------------------------------
052600141001       //?Controllo dati video D01.
052700141001       //--------------------------------------------------------------
052800141001       BEGSR ContrD01;
052900141001
053000141001         *in28 = *off;
053100141001         *in40 = *off;
053200141001         *in41 = *off;
053300141001         *in42 = *off;
053400141001         *in43 = *off;
053500141001
053600141001       // Chiave tabella
053700141001         IF D1TBEke1 = *blanks;
053800141001           VD1Msg = Msg(01);
053900141001           *in28 = *on;
054000141001           *in40 = *on;
054100141001           leavesr;
054200141001         ENDIF;
054300141001
054400141001       // Se immissione controllo che non esista già
054500141001         IF  BIFOcomand = 'J' or BIFOopzio = '03';
054600141001           TBEcod = 'IFO';
054700141001           TBEke1 = D1TBEke1;
054800141001           TBEke2 = D1TBEke2;
054900141001           clear TBElin;
055000141001           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
055100141001           IF  %found(TNTBE01L);
055200141001             VD1Msg = Msg(03);
055300141001             *in28 = *on;
055400141001             *in40 = *on;
055500141001             leavesr;
055600141001           ENDIF;
055700141001         ENDIF;
055800141001
055900141001       // Descrizione
056000141001         IF  D1IFOdes = *blanks;
056100141001           VD1Msg = Msg(02);
056200141001           *in28 = *on;
056300141001           *in41 = *on;
056400141001           leavesr;
056500141001         ENDIF;
056600141001
056700141001       // Descrizione breve
056800141001         IF  D1IFOdeb = *blanks;
056900141001           VD1Msg = Msg(02);
057000141001           *in28 = *on;
057100141001           *in42 = *on;
057200141001           leavesr;
057300141001         ENDIF;
057400141001
057500141001       // Categoria Info Commerciale
057600141001         clear D1desifg;
057700141001         IF  D1IFOifg = *blanks;
057800141001           VD1Msg = Msg(04);
057900141001           *in28 = *on;
058000141001           *in43 = *on;
058100141001           leavesr;
058200141001         ENDIF;
058300141001       // Ricerca
058400141001         IF  %scan('?' : D1IFOifg) > 0;
058500141001           *in43 = *on;
058600141001           clear TNTBIFGDS;
058700141001           BIFGfun = '1';
058800141001           kpjbu = TNTBIFGDS;
058900141001           tntbifgr (kpjba);
059000141001           TNTBIFGDS = kpjbu;
059100141001           IF  BIFGesito <> Errore;
059200141001             D1IFOifg = BIFGke1;
059300141001           ENDIF;
059400141001         ENDIF;
059500141001       // Controllo
059600141001         IF  D1IFOifg <> *blanks;
059700141001           clear DIFG;
059800141001           clear TIBS02DS;
059900141001           T02mod = 'C';
060000141001           T02cod = 'IFG';
060100141001           T02ke1 = D1IFOifg;
060200141001           T02sif = knsif;
060300141001           TNTBE_RicercaControllo  (kpjba : tibs02ds);
060400141001           IF T02err <> *blanks;
060500141001             VD1Msg = Msg(05);
060600141001             *in28 = *on;
060700141001             *in43 = *on;
060800141001             leavesr;
060900141001           ENDIF;
061000141001           dIFG = T02uni;
061100141001           D1desifg = §IFGdes;
061200141001         ENDIF;
061300141001
061400141001       ENDSR;
061500141001
061600141001       //--------------------------------------------------------------
061700141001       //?Gestione video W01.
061800141001       //--------------------------------------------------------------
061900141001       BEGSR GesW01;
062000141001
062100141001       // Imposto i dati a video
062200141001         exsr CarW01;
062300141001
062400141001       // Fino a che la variabile resta settata come 'W01'
062500141001         DOU  wVideo <> 'W01';
062600141001
062700141001         // Reset indicatore generico di Errore
062800141001           *in28 = *off;
062900141001
063000141001         // Emetto il video
063100141001           exfmt TBIFOW01;
063200141001
063300141001         // Controllo tasti funzionali del video
063400141001           SELECT;
063500141001
063600141001         // F6=Conferma
063700141001           WHEN  *inKF;
063800141001           // Controllo i dati del video
063900141001             exsr ContrW01;
064000141001           // Se non riscontrati errori aggiorno il record corrente
064100141001             IF  not *in28;
064200141001               exsr Aggiorna;
064300141001               IF  not *in28;
064400141001                 IF  BIFOcomand = 'J';
064500141001                   wVideo = 'D01';
064600141001                   exsr CarD01;
064700141001                 ELSE;
064800141001                   wVideo = 'S01';
064900141001                 ENDIF;
065000141001               ENDIF;
065100141001             ENDIF;
065200141001
065300141001         // F12=Ritorno
065400141001           WHEN  *inKL;
065500141001             wVideo = 'D01';
065600141001             clear BIFOricez;
065700141001
065800141001         // Invio
065900141001           OTHER;
066000141001             exsr ContrW01;
066100141001
066200141001           ENDSL;
066300141001
066400141001       // Fine gestione 'W01'
066500141001         ENDDO;
066600141001
066700141001       ENDSR;
066800141001
066900141001       //--------------------------------------------------------------
067000141001       //?Carico i dati video W01.
067100141001       //--------------------------------------------------------------
067200141001       BEGSR CarW01;
067300141001
067400141001       // Pulisco i campi
067500141001         clear W1ftt;
067600141001         clear W1flt;
067700141001         clear W1ftr;
067800141001         clear W1dtr;
067900141001
068000141001       // Se non immissione imposto i campi del file
068100141001         IF  BIFOcomand <> 'J';
068200141001           W1ftt = TBEftt;
068300141001           W1flt = TBEflt;
068400141001           W1ftr = TBEftr;
068500141001         // Imposto la data
068600141001           IF  TBEdtr <> *zeros;
068700141001             clear Wlbdat;
068800141001             G02inv = TBEdtr;
068900141001             G02err = '3';
069000141001             xsrda8(wlbdat);
069100141001             W1dtr = G02dat;
069200141001           ENDIF;
069300141001         ENDIF;
069400141001
069500141001       ENDSR;
069600141001
069700141001       //--------------------------------------------------------------
069800141001       //?Controllo video W01.
069900141001       //--------------------------------------------------------------
070000141001       BEGSR ContrW01;
070100141001
070200141001       ENDSR;
070300141001
070400141001       //--------------------------------------------------------------
070500141001       //?Aggiorno la tabella.
070600141001       //--------------------------------------------------------------
070700141001       BEGSR Aggiorna;
070800141001
070900141001       // Imposto campi del file
071000141001         clear TBElin;
071100141001         TBEke1 = D1TBEke1;
071200141001         TBEke2 = D1TBEke2;
071300141001
071400141001         §IFOdes  = D1IFOdes;
071500141001         §IFOobl  = D1IFOobl;
071600141001         §IFOifs  = D1IFOifs;
071700141001         §IFOifv  = D1IFOifv;
071800141001         §IFOsnv  = D1IFOsnv;
071900141001         §IFOsns  = D1IFOsns;
072000141001         §IFOsnf  = D1IFOsnf;
072100141001         §IFOsne  = D1IFOsne;
072200141001         §IFOifg  = D1IFOifg;
072300141001         §IFOogi  = D1IFOogi;
072400141001         §IFOtva  = D1IFOtva;
072500141001         §IFOann  = D1IFOann;
072600141001         §IFOtip  = D1IFOtip;
072700141001         §IFOsta  = D1IFOsta;
072800141001         §IFOl1   = D1IFOl1;
072900141001         §IFOnrec = D1IFOnrec;
073000141001         §IFOdeb  = D1IFOdeb;
073100141001         §IFOsnvd = D1IFOsnvd;
073200141001
073300141001         TBEuni = dIFO;
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
074400141001         WHEN  BIFOcomand = 'J' or bIFOopzio = '03';
074500141001         // Scrivo nuovo record con gestione Errore per chiave duplicata
074600141001           write TNTBE000;
074700141001
074800141001       // Opzione "02"=modIFica
074900141001         WHEN  BIFOopzio = '02';
075000141001         // update record e controllo Errore per chiave duplicata
075100141001          update TNTBE000;
075200141001
075300141001       // Opzione "04"=cancellazione/ripristino
075400141001         WHEN  BIFOopzio = '04';
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
076900141001         clear D1IFOdes;
077000141001         clear D1IFOobl;
077100141001         clear D1IFOifs;
077200141001         clear D1IFOifv;
077300141001         clear D1IFOsnv;
077400141001         clear D1IFOsns;
077500141001         clear D1IFOsnf;
077600141001         clear D1IFOsne;
077700141001         clear D1IFOifg;
077800141001         clear D1desifg;
077900141001         clear D1IFOogi;
078000141001         clear D1IFOtva;
078100141001         clear D1IFOann;
078200141001         clear D1IFOtip;
078300141001         clear D1IFOsta;
078400141001         clear D1IFOl1;
078500141001         clear D1IFOnrec;
078600141001         clear D1IFOdeb;
078700141001         clear D1IFOsnvd;
078800141001
078900141001         *in28 = *off;
079000141001         *in40 = *off;
079100141001         *in41 = *off;
079200141001         *in42 = *off;
079300141001         *in43 = *off;
079400141001
079500141001       ENDSR;
079600141001
079700141001       //--------------------------------------------------------------
079800141001       //?Imposto dati video D01.
079900141001       //--------------------------------------------------------------
080000141001       BEGSR RieD01;
080100141001
080200141001       // Recupero i dati dal file
080300141001         kTBEke1 = S1TBEke1;
080400141001         clear TBElin;
080500141001         chain (TBEcod:kTBEke1:S1TBEke2:TBElin) TNTBE01L;
080600141001
080700141001       // Imposto i campi a video
080800141001         D1tbeke2  = TBEke2;
080900141001         D1tbeke1  = TBEke1;
081000141001         dIFO      = TBEuni;
081100141001         D1IFOdes  = §IFOdes;
081200141001         D1IFOobl  = §IFOobl;
081300141001         D1IFOifs  = §IFOifs;
081400141001         D1IFOifv  = §IFOifv;
081500141001         D1IFOsnv  = §IFOsnv;
081600141001         D1IFOsns  = §IFOsns;
081700141001         D1IFOsnf  = §IFOsnf;
081800141001         D1IFOsne  = §IFOsne;
081900141001         D1IFOifg  = §IFOifg;
082000141001         D1IFOogi  = §IFOogi;
082100141001         D1IFOtva  = §IFOtva;
082200141001         D1IFOann  = §IFOann;
082300141001         D1IFOtip  = §IFOtip;
082400141001         D1IFOsta  = §IFOsta;
082500141001         D1IFOl1   = §IFOl1;
082600141001         D1IFOnrec = §IFOnrec;
082700141001         D1IFOdeb  = §IFOdeb;
082800141001         D1IFOsnvd = §IFOsnvd;
082900141001
083000141001       // Decodifico la categoria
083100141001         clear DIFG;
083200141001         clear TIBS02DS;
083300141001         T02mod = 'C';
083400141001         T02cod = 'IFG';
083500141001         T02ke1 = D1IFOifg;
083600141001         T02sif = knsif;
083700141001         TNTBE_RicercaControllo  (kpjba : tibs02ds);
083800141001         IF T02err <> *blanks;
083900141001           leavesr;
084000141001         ENDIF;
084100141001         dIFG = T02uni;
084200141001         D1desifg = §IFGdes;
084300141001
084400141001       ENDSR;
084500141001
084600141001       //--------------------------------------------------------------
084700141001       //?Controllo dati video S01.
084800141001       //--------------------------------------------------------------
084900141001       BEGSR ContrS01;
085000141001
085100141001       // Set flag
085200141001         wEoFS01 = *off;
085300141001
085400141001       // Inizio lettura subfile
085500141001         DOW  not wEoFS01 and *in21;
085600141001           readc TBIFOS01;
085700141001         // Se fine subfile
085800141001           IF  %eof;
085900141001             wCarS01 = *on;
086000141001             leave;
086100141001           ENDIF;
086200141001         // Se è stata immessa un'opzione
086300141001           IF  S1opzione <> *zeros;
086400141001           // Reset ds di servizio
086500141001             clear TNTBIFODS;
086600141001           // Controllo ed elaborazione opzione immessa
086700141001             SELECT;
086800141001           // Opzione "01" = scelta
086900141001             WHEN  S1opzione = 1 and *in01;
087000141001               BIFOopzio = '01';
087100141001           // Opzione "02" = modifica
087200141001             WHEN  S1opzione = 2 and not *in01 and S1TBEatb = *blanks;
087300141001               BIFOopzio = '02';
087400141001           // Opzione "03" = copia
087500141001             WHEN  S1opzione = 3 and not *in01 and S1TBEatb = *blanks;
087600141001               BIFOopzio = '03';
087700141001           // Opzione "04" = annullo/ripristino
087800141001             WHEN  S1opzione = 4 and not *in01;
087900141001               BIFOopzio = '04';
088000141001           // Opzione "05" = visualizzazione
088100141001             WHEN  S1opzione = 5;
088200141001               BIFOopzio = '05';
088300141001             ENDSL;
088400141001
088500141001           // Se immessa almeno un opzione valida
088600141001             IF  BIFOopzio <> *blanks;
088700141001             // Gstione del formato video
088800141001               exsr GesD01;
088900141001             // Se la gestione si è chiusa con ...
089000141001               SELECT;
089100141001             // F6=Conferma
089200141001               WHEN  BIFOricez = 'F';
089300141001                 clear S1opzione;
089400141001                 wCarS01 = *on;
089500141001             // F12=Ritorno
089600141001               WHEN  BIFOricez = 'L';
089700141001                 clear S1opzione;
089800141001                 wEoFS01 = *on;
089900141001             // Altrimenti
090000141001               OTHER;
090100141001                 *in22 = *on;
090200141001               ENDSL;
090300141001
090400141001               update TBIFOS01;
090500141001               *in22 = *off;
090600141001           // Fine opzione
090700141001             ENDIF;
090800141001         // Fine opzione
090900141001           ENDIF;
091000141001
091100141001         ENDDO;
091200141001
091300141001       ENDSR;
091400141001
091500141001       //--------------------------------------------------------------
091600141001       //?Posizionamento sul file tabelle.
091700141001       //--------------------------------------------------------------
091800141001       BEGSR SetllTBE01;
091900141001
092000141001         SELECT;
092100141001       // Chiave totale
092200141001         WHEN wKey = '1';
092300141001           setll TBEcod TNTBE01L;
092400141001       // chiave parziale
092500141001         WHEN wKey = '2';
092600141001           setll (TBEcod:TBEke1) TNTBE01L;
092700141001         ENDSL;
092800141001
092900141001       ENDSR;
093000141001
093100141001       //--------------------------------------------------------------
093200141001       //?Scrive video S01.
093300141001       //--------------------------------------------------------------
093400141001       BEGSR WrtS01;
093500141001
093600141001       // Se non raggiunto limite massimo di caricamento
093700141001         IF S01nrr < 9999;
093800141001         // Imposto campi di subfile
093900141001           exsr RieS01;
094000141001         // Imposto numeratore righe e numero relativo di record
094100141001           S01nrr += 1;
094200141001         // scrivo subfile
094300141001           write TBIFOS01;
094400141001         ENDIF;
094500141001
094600141001       ENDSR;
094700141001
094800141001       //--------------------------------------------------------------
094900141001       //?Imposto campi video S01.
095000141001       //--------------------------------------------------------------
095100141001       BEGSR RieS01;
095200141001
095300141001         S1TBEke1 = TBEke1;
095400141001         S1TBEke2 = TBEke2;
095500141001         dIFO = TBEuni;
095600141001         S1TBEuni = §IFOdes;
095700141001         S1TBEatb = TBEatb;
095800141001
095900141001       ENDSR;
096000141001
096100141001       //--------------------------------------------------------------
096200141001       //?Operazioni finali.
096300141001       //--------------------------------------------------------------
096400141001       BEGSR RoutEnd;
096500141001
096600141001         IF  BIFOesito = *blanks;
096700141001           BIFOesito = Eseguito;
096800141001         ENDIF;
096900141001
097000141001         kpjbu = TNTBIFODS;
097100141001
097200141001         *inLR = *on;
097300141001         return;
097400141001
097500141001       ENDSR;
097600141001
097700141001      /end-free
097800141001
097900141001       //--------------------------------------------------------------
098000141001       //?Schiere a tempo di compilazione.
098100141001       //--------------------------------------------------------------
098200141001
098300141001** -Msg----------------------------------------------------------------------*
098400141001Immettere il codice Info                                                       01
098500141001Immettere la descrizione                                                       02
098600141001Info commerciale già esistente                                                 03
098700141001Immettere la categoria                                                         04
098800141001Categoria errata                                                               05
