000100141001      //--------------------------------------------------------------
000200141002      //?TNTBIFSR - Gestione tabella "IFS" Sottotipo Info Comm.li
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
001700141002     fTNTBIFSD  cf   e             workstn sfile(TBIFSS01:S01nrr)
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
005400141002     d TNTBIFSDS     e ds
005500141002
005600141002      // - Interrogazione tabella IFO
005700141002     d TNTBIFODS     e ds
005800141002
005900141002      // - Controllo tabella
006000141002     d TIBS02DS      e ds
006100141001
006200141001      // - Reperimento dati utente
006300141001     d TIBS34DS      e ds
006400141002
006500141002      // - Tabella IFO = Info Commerciali
006600141002     d dIFO          e ds
006700141001
006800141002      // - Tabella IFS = Sottotipo Info Commerciali
006900141002     d dIFS          e ds
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
009200141002      // - Interrogazione tabella IFO
009300141002     d tntbifor        pr                  extpgm('TNTBIFOR')
009400141001     d  kpjba                              likeds(kpjba)
009500141001
009600141001      //---------------------------------------------------------------
009700141001      //?Definizione prototipi.
009800141001      //---------------------------------------------------------------
009900141002      /copy gaitrasrc/srcprotopr,tibs02r
010000141001      /copy gaitrasrc/srcprotopr,tibs34r
010100141001      /copy gaitrasrc/srcprotopr,xsrda8
010200141001
010300141001      //---------------------------------------------------------------
010400141001      //?Riepilogo indicatori.
010500141001      //---------------------------------------------------------------
010600141001      // 01 - ON ricerca -- OFF manutenzione
010700141002      // 02 - proteggo campo Ke1
010800141001      // 03 - proteggo campi video 01
010900141001      // 04 - visualizzazione
011000141001      // 20 - gestione subfile
011100141001      // 21 - gestione subfile
011200141001      // 22 - gestione subfile
011300141001      // 23 - gestione subfile
011400141001      // 28 - messaggio Errore
011500141001      // 40 - Chiave tabella
011600141002      // 41 - Descrizione breve
011700141002      // 42 - Chiave 2 tabella
011800141001
011900141001      //---------------------------------------------------------------
012000141001      //?M A I N - L I N E
012100141001      //---------------------------------------------------------------
012200141001
012300141001     c     *Entry        plist
012400141001     c                   parm                    KPJBA
012500141001
012600141001      /free
012700141001
012800141001       // Operazioni iniziali
012900141001       exsr RoutInz;
013000141001
013100141001       // Gestione Subfile
013200141001       exsr GesS01;
013300141001
013400141001       // Operazioni finali
013500141001       exsr RoutEnd;
013600141001
013700141001       //--------------------------------------------------------------
013800141001       //?Operazioni iniziali.
013900141001       //--------------------------------------------------------------
014000141001       BEGSR RoutInz;
014100141001
014200141001       // Ricezione parametri
014300141002         TNTBIFSDS = kpjbu;
014400141001
014500141001         SELECT;
014600141001       // Ricerca e scelta
014700141002         WHEN  BIFSfun = '1';
014800141001           *in01 = *on;
014900141001       // Manutenzione
015000141002         WHEN  BIFSfun = *blanks;
015100141001          *in01 = *off;
015200141001         OTHER;
015300141002           BIFSesito = Errore;
015400141001           exsr RoutEnd;
015500141001         ENDSL;
015600141001
015700141001       // Reperimento dati job
015800141001         exsr DatiJob;
015900141001
016000141001       ENDSR;
016100141001
016200141001       //--------------------------------------------------------------
016300141001       //?Reperimento Dati del job (Utente/Operativi).
016400141001       //--------------------------------------------------------------
016500141001       BEGSR DatiJob;
016600141001
016700141001         in(E) §AzUte;
016800141001         IF  not %error;
016900141001           in(E) §DatiUte;
017000141001         ENDIF;
017100141001         IF  %error or RSut = *blanks;
017200141001           clear TIBS34ds;
017300141001           tibs34r(tibs34ds);
017400141001           in §AzUte;
017500141001           in §DatiUte;
017600141001         ENDIF;
017700141001
017800141001       ENDSR;
017900141001
018000141001       //--------------------------------------------------------------
018100141001       //?Gestione video S01.
018200141001       //--------------------------------------------------------------
018300141001       BEGSR GesS01;
018400141001
018500141001       // Imposto variabili
018600141001         wCarS01 = *on;
018700141001         wVideo  = 'S01';
018800141001
018900141001       // Inizio elaborazione subfile
019000141001         DOU  wVideo <> 'S01';
019100141001
019200141001         // Caricamento subfile
019300141001           exsr Cars01;
019400141001
019500141001         // C1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
019600141001         // impostando C1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
019700141001         // l'ultimo tasto
019800141001           IF  C1csrrrn > 0;
019900141001             C1rcdnbr = C1csrrrn;
020000141001           ELSE;
020100141001             C1rcdnbr = savrcdnbr;
020200141001           ENDIF;
020300141001
020400141001         // Se non so quale pagina visualizzare forzo pagina 1
020500141001           IF  C1rcdnbr < 1;
020600141001             C1rcdnbr = 1;
020700141001           ENDIF;
020800141001
020900141001         // Salvo il record number del subfile
021000141001           savrcdnbr = C1rcdnbr;
021100141001
021200141001         // Emissione del subfile
021300141002           write TBIFSP01;
021400141002           exfmt TBIFSC01;
021500141001
021600141001         // Controllo tasti funzionali del subfile
021700141001           SELECT;
021800141001
021900141001         // F3=Fine
022000141001           WHEN  *inKC;
022100141001             wVideo = *blanks;
022200141002             BIFSricez = 'C';
022300141001             exsr RoutEnd;
022400141001
022500141001         // F5=Refresh
022600141001           WHEN  *inKE;
022700141001             wCarS01 = *on;
022800141001
022900141001         // F10=Immissione
023000141001           WHEN  *inKJ;
023100141001             wVideo = 'D01';
023200141002             clear TNTBIFSDS;
023300141002             BIFScomand = 'J';
023400141001             exsr GesD01;
023500141001
023600141001         // F13=Ripetizione
023700141001           WHEN  *inKM;
023800141001             exsr RipetiOpz;
023900141001
024000141001         // In tutti gli altri casi
024100141001           OTHER;
024200141001         // Controllo ed elaborazione scelte su subfile
024300141001             exsr ContrS01;
024400141001           ENDSL;
024500141001
024600141001       // Fine elaborazione 'S01'
024700141001          ENDDO;
024800141001
024900141001        ENDSR;
025000141001
025100141001       //--------------------------------------------------------------
025200141001       //?Carica video S01.
025300141001       //--------------------------------------------------------------
025400141001       BEGSR Cars01;
025500141001
025600141001       // Se è stato richiesto il caricamento del subfile
025700141001         IF  wCarS01;
025800141001
025900141001         // Inizializzo il subfile
026000141001           S01nrr = 0;
026100141001           *in20 = *on;
026200141002           write TBIFSC01;
026300141001           *in20 = *off;
026400141001           *in21 = *off;
026500141001           *in22 = *off;
026600141001           *in23 = *off;
026700141001
026800141001         // Imposto la chiave di posizionamento e lettura file
026900141002           TBEcod = 'IFS';
027000141001           TBEke1 = C1setll;
027100141001
027200141001         // Se è stato scelto il posizionamento
027300141001           IF  C1setll <> *blanks;
027400141001             wKey = '2';
027500141001         // Altrimenti
027600141001           ELSE;
027700141001             wKey = '1';
027800141001           ENDIF;
027900141001
028000141001         // Posizionamento file
028100141001           exsr SetllTBE01;
028200141001
028300141001         // Fino a che non è fine file
028400141001           DOU  %eof(TNTBE01L);
028500141001
028600141001           // Leggo file
028700141001             reade(n) TBEcod TNTBE01L;
028800141001
028900141001           // Fine file esco
029000141001             IF  %eof(TNTBE01L);
029100141001               leave;
029200141001             ENDIF;
029300141001
029400141001           // Se in "ricerca/scelta" non carico i records annullati
029500141001             IF  (*in01 and TBEatb = *blanks) or not *in01;
029600141001           // Scrivo subfile
029700141001               clear S1opzione;
029800141001               exsr WrtS01;
029900141001             ENDIF;
030000141001
030100141001           ENDDO;
030200141001
030300141001       // Fine caricamento subfile
030400141001         ENDIF;
030500141001
030600141001       // Se scritto almeno un record
030700141001         IF  S01nrr > 0;
030800141001       // Indicatore di sflend
030900141001           *in21 = *on;
031000141001         ENDIF;
031100141001
031200141001       // Se non scritto neanche un record
031300141001         IF  S01nrr = 0;
031400141001       // Indicatore di sfldsp
031500141001           *in23 = *on;
031600141001         ENDIF;
031700141001
031800141001       // Disattivo opzione di caricamento subfile
031900141001         wCarS01 = *off;
032000141001
032100141001       ENDSR;
032200141001
032300141001       //--------------------------------------------------------------
032400141001       //?Carica video D01.
032500141001       //--------------------------------------------------------------
032600141001       BEGSR GesD01;
032700141001
032800141001       // Imposto il video a seconda della funzione richiesta
032900141001         exsr CarD01;
033000141001
033100141001       // Imposto variabile
033200141001         wVideo = 'D01';
033300141001
033400141001       // Fino a che la variabile resta settata come 'D01'
033500141001         DOU  wVideo <> 'D01';
033600141001
033700141001           *in02 = *off;
033800141001           *in03 = *off;
033900141001           *in04 = *off;
034000141001
034100141001         // se immessa opzione di 'scelta'
034200141002           IF  BIFSopzio = '01';
034300141002             BIFSke1 = S1TBEke1;
034400141002             BIFSdes = S1TBEuni;
034500141001             exsr RoutEnd;
034600141001
034700141001         // Se immessa un'altra opzione
034800141001           ELSE;
034900141001           // Se non è immissione/copia proteggo il campo della causale
035000141002             IF  BIFScomand <> 'J' and BIFSopzio <> '03';
035100141001               *in02 = *on;
035200141001             ENDIF;
035300141001           // Se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
035400141001           // proteggo i campi del video
035500141002             IF  BIFSopzio = '04' or BIFSopzio = '05';
035600141001               *in03 = *on;
035700141001             ENDIF;
035800141001           // Se immessa opzione di 'visualizzazione'
035900141001           // non attivo F6
036000141002             IF  BIFSopzio = '05';
036100141001               *in04 = *on;
036200141001             ENDIF;
036300141001           // emetto il video
036400141002             exfmt TBIFSD01;
036500141001           ENDIF;
036600141001
036700141001         // Reset indicatore generico di Errore
036800141001           *in28 = *off;
036900141001
037000141001         // pulisco il campo messaggi
037100141001           clear VD1Msg;
037200141001
037300141001         // controllo tasti funzionali del video
037400141001           SELECT;
037500141001
037600141001         // F3=Fine
037700141001           WHEN  *inKC;
037800141002             BIFSricez = 'C';
037900141001             wVideo = *blanks;
038000141001             unlock TNTBE01L;
038100141001             exsr RoutEnd;
038200141001
038300141001         // F6=Conferma
038400141001           WHEN  *inKF;
038500141002             BIFSricez = 'F';
038600141001           // Controllo e decodIFico i dati del video
038700141001             exsr ContrD01;
038800141001           // Conferma per annullo/ripristino
038900141002             IF  BIFSopzio = '04';
039000141001               SELECT;
039100141001             // Annullamento
039200141001               WHEN TBEatb = *blanks;
039300141001                 TBEatb = 'A';
039400141001             // Ripristino
039500141001               WHEN TBEatb = 'A';
039600141001                 clear TBEatb;
039700141001               ENDSL;
039800141001             ENDIF;
039900141001
040000141001           // Se non riscontrati errori emetto la finestra con i dati per la trasmissione
040100141001             IF  not *in28;
040200141001               wCarW01 = *on;
040300141001               wVideo = 'W01';
040400141001               exsr GesW01;
040500141001             ENDIF;
040600141001
040700141001         // F8=Record successivo
040800141001           WHEN  *inKH;
040900141001             clear S1opzione;
041000141001             wCarS01 = *off;
041100141001             wVideo = 'S01';
041200141002             BIFSricez = 'H';
041300141001
041400141001         // F12=Ritorno
041500141001           WHEN  *inKL;
041600141001             clear S1opzione;
041700141002             BIFSricez = 'L';
041800141001             unlock TNTBE01L;
041900141001           // Se non è f12 da immissione/copia non ricarico il subfile
042000141002             IF  BIFScomand = 'J' or BIFSopzio = '03';
042100141001               wCarS01 = *on;
042200141001             ELSE;
042300141001               wCarS01 = *off;
042400141001             ENDIF;
042500141001             wVideo = 'S01';
042600141001
042700141001         // Invio
042800141001           OTHER;
042900141001             IF  not *in03;
043000141001               exsr ContrD01;
043100141001             ENDIF;
043200141001
043300141001          ENDSL;
043400141001
043500141001       // Fine gestione 'D01'
043600141001        ENDDO;
043700141001
043800141001       ENDSR;
043900141001
044000141001       //--------------------------------------------------------------
044100141001       //?Ripeti Opzione S01.
044200141001       //--------------------------------------------------------------
044300141001       BEGSR RipetiOpz;
044400141001
044500141001         IF  C1csrrrn > 0;
044600141001           S01nrr = C1csrrrn;
044700141002           chain S01nrr TBIFSS01;
044800141001           IF  %found and S1opzione > 0;
044900141001             savopzione = S1opzione;
045000141001             *in22 = *on;
045100141002             update TBIFSS01;
045200141001
045300141001             wEoFS01 = *off;
045400141001             DOU  wEoFS01;
045500141001               S01nrr += 1;
045600141002               chain S01nrr TBIFSS01;
045700141001               IF  %found;
045800141001                 S1opzione = savopzione;
045900141002                 update TBIFSS01;
046000141001               ELSE;
046100141001                 wEoFS01 = *on;
046200141001               ENDIF;
046300141001             ENDDO;
046400141001
046500141001             *in22 = *off;
046600141001           ENDIF;
046700141001
046800141001         ENDIF;
046900141001
047000141001       ENDSR;
047100141001
047200141001       //--------------------------------------------------------------
047300141001       //?Carico dati video D01.
047400141001       //--------------------------------------------------------------
047500141001       BEGSR CarD01;
047600141001
047700141001       // Pulisco il formato video D01
047800141001         exsr PulisciD01;
047900141001
048000141001       // Controllo se 'immissione' 'modifica' 'copia' o altro
048100141001         SELECT;
048200141001
048300141001       // F10=Immissione
048400141002         WHEN  BIFScomand = 'J';
048500141001           D1descopz = 'Immissione';
048600141001
048700141001       // Opzione "02" = modifica
048800141002         WHEN  BIFSopzio = '02';
048900141001           D1descopz = 'ModIFica';
049000141001           exsr RieD01;
049100141001
049200141001       // Opzione "03" = copia
049300141002         WHEN  BIFSopzio = '03';
049400141001           D1descopz = 'Copia';
049500141001           exsr RieD01;
049600141001
049700141001       // Opzione "04" = cancellazione/ripristino
049800141002         WHEN  BIFSopzio = '04';
049900141001           exsr RieD01;
050000141001         // Se richiesta 'cancellazione'
050100141001           IF  TBEatb = *blanks;
050200141001             D1descopz = 'Annullamento';
050300141001           ENDIF;
050400141001         // Se richiesto 'ripristino'
050500141001           IF  TBEatb = 'A';
050600141001             D1descopz = 'Ripristino';
050700141001           ENDIF;
050800141001
050900141001       // Opzione "05" = visualizzazione
051000141002         WHEN  BIFSopzio ='05';
051100141001           D1descopz = 'Visualizzazione';
051200141001           exsr RieD01;
051300141001
051400141001       // Fine scelta
051500141001         ENDSL;
051600141001
051700141001       // Centro la descrizione della funzione nel formato video
051800141001         len = (%len(D1descopz) - %len(%trimr(D1descopz))) / 2;
051900141001         D1descopz = %subst(blanks:1:len) + %trimr(D1descopz);
052000141001
052100141001       ENDSR;
052200141001
052300141001       //--------------------------------------------------------------
052400141001       //?Controllo dati video D01.
052500141001       //--------------------------------------------------------------
052600141001       BEGSR ContrD01;
052700141001
052800141001         *in28 = *off;
052900141001         *in40 = *off;
053000141001         *in41 = *off;
053100141002         *in42 = *off;
053200141001
053300141001       // Chiave tabella
053400141001         IF D1TBEke1 = *blanks;
053500141001           VD1Msg = Msg(01);
053600141001           *in28 = *on;
053700141002           *in40 = *on;
053800141001           leavesr;
053900141001         ENDIF;
054000141002
054100141002       // Controllo Ke1 (Info commerciale)
054200141002         clear D1deske1;
054300141002       // Ricerca
054400141002         IF  %scan('?' : D1TBEke1) > 0;
054500141002           *in43 = *on;
054600141002           clear TNTBIFODS;
054700141002           BIFOfun = '1';
054800141002           kpjbu = TNTBIFODS;
054900141002           tntbifor (kpjba);
055000141002           TNTBIFODS = kpjbu;
055100141002           IF  BIFOesito <> Errore;
055200141002             D1TBEke1 = BIFOke1;
055300141002           ENDIF;
055400141002         ENDIF;
055500141002       // Controllo
055600141002         IF  D1TBEke1 <> *blanks;
055700141002           clear DIFO;
055800141002           clear TIBS02DS;
055900141002           T02mod = 'C';
056000141002           T02cod = 'IFO';
056100141002           T02ke1 = D1TBEke1;
056200141002           T02sif = knsif;
056300141002           TNTBE_RicercaControllo  (kpjba : tibs02ds);
056400141002           IF T02err <> *blanks;
056500141002             VD1Msg = Msg(05);
056600141002             *in28 = *on;
056700141002             *in40 = *on;
056800141002             leavesr;
056900141002           ENDIF;
057000141002           dIFO = T02uni;
057100141002           D1deske1 = §IFOdes;
057200141002         ENDIF;
057300141002
057400141002       // Chiave 2 tabella
057500141002         IF D1TBEke2 = *blanks;
057600141002           VD1Msg = Msg(04);
057700141002           *in28 = *on;
057800141002           *in42 = *on;
057900141002           leavesr;
058000141002         ENDIF;
058100141001
058200141001       // Se immissione controllo che non esista già
058300141002         IF  BIFScomand = 'J' or BIFSopzio = '03';
058400141002           TBEcod = 'IFS';
058500141001           TBEke1 = D1TBEke1;
058600141001           TBEke2 = D1TBEke2;
058700141001           clear TBElin;
058800141001           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
058900141001           IF  %found(TNTBE01L);
059000141001             VD1Msg = Msg(03);
059100141001             *in28 = *on;
059200141002             *in42 = *on;
059300141001             leavesr;
059400141001           ENDIF;
059500141001         ENDIF;
059600141001
059700141002       // Descrizione breve
059800141002         IF  D1IFSdes = *blanks;
059900141001           VD1Msg = Msg(02);
060000141001           *in28 = *on;
060100141001           *in41 = *on;
060200141001           leavesr;
060300141001         ENDIF;
060400141001
060500141001       ENDSR;
060600141001
060700141001       //--------------------------------------------------------------
060800141001       //?Gestione video W01.
060900141001       //--------------------------------------------------------------
061000141001       BEGSR GesW01;
061100141001
061200141001       // Imposto i dati a video
061300141001         exsr CarW01;
061400141001
061500141001       // Fino a che la variabile resta settata come 'W01'
061600141001         DOU  wVideo <> 'W01';
061700141001
061800141001         // Reset indicatore generico di Errore
061900141001           *in28 = *off;
062000141001
062100141001         // Emetto il video
062200141002           exfmt TBIFSW01;
062300141001
062400141001         // Controllo tasti funzionali del video
062500141001           SELECT;
062600141001
062700141001         // F6=Conferma
062800141001           WHEN  *inKF;
062900141001           // Controllo i dati del video
063000141001             exsr ContrW01;
063100141001           // Se non riscontrati errori aggiorno il record corrente
063200141001             IF  not *in28;
063300141001               exsr Aggiorna;
063400141001               IF  not *in28;
063500141002                 IF  BIFScomand = 'J';
063600141001                   wVideo = 'D01';
063700141001                   exsr CarD01;
063800141001                 ELSE;
063900141001                   wVideo = 'S01';
064000141001                 ENDIF;
064100141001               ENDIF;
064200141001             ENDIF;
064300141001
064400141001         // F12=Ritorno
064500141001           WHEN  *inKL;
064600141001             wVideo = 'D01';
064700141002             clear BIFSricez;
064800141001
064900141001         // Invio
065000141001           OTHER;
065100141001             exsr ContrW01;
065200141001
065300141001           ENDSL;
065400141001
065500141001       // Fine gestione 'W01'
065600141001         ENDDO;
065700141001
065800141001       ENDSR;
065900141001
066000141001       //--------------------------------------------------------------
066100141001       //?Carico i dati video W01.
066200141001       //--------------------------------------------------------------
066300141001       BEGSR CarW01;
066400141001
066500141001       // Pulisco i campi
066600141001         clear W1ftt;
066700141001         clear W1flt;
066800141001         clear W1ftr;
066900141001         clear W1dtr;
067000141001
067100141001       // Se non immissione imposto i campi del file
067200141002         IF  BIFScomand <> 'J';
067300141001           W1ftt = TBEftt;
067400141001           W1flt = TBEflt;
067500141001           W1ftr = TBEftr;
067600141001         // Imposto la data
067700141001           IF  TBEdtr <> *zeros;
067800141001             clear Wlbdat;
067900141001             G02inv = TBEdtr;
068000141001             G02err = '3';
068100141001             xsrda8(wlbdat);
068200141001             W1dtr = G02dat;
068300141001           ENDIF;
068400141001         ENDIF;
068500141001
068600141001       ENDSR;
068700141001
068800141001       //--------------------------------------------------------------
068900141001       //?Controllo video W01.
069000141001       //--------------------------------------------------------------
069100141001       BEGSR ContrW01;
069200141001
069300141001       ENDSR;
069400141001
069500141001       //--------------------------------------------------------------
069600141001       //?Aggiorno la tabella.
069700141001       //--------------------------------------------------------------
069800141001       BEGSR Aggiorna;
069900141001
070000141001       // Imposto campi del file
070100141001         clear TBElin;
070200141001         TBEke1 = D1TBEke1;
070300141001         TBEke2 = D1TBEke2;
070400141001
070500141002         §IFSdes  = D1IFSdes;
070600141002         §IFSdel  = D1IFSdel;
070700141002         §IFSann  = D1IFSann;
070800141001
070900141002         TBEuni = dIFS;
071000141001
071100141001         TBEftt = W1ftt;
071200141001         TBEflt = W1flt;
071300141001         clear TBEftr;
071400141001         clear TBEdtr;
071500141001
071600141001       // Controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
071700141001         SELECT;
071800141001
071900141001       // F10=immissione - Opzione "03"=copia
072000141002         WHEN  BIFScomand = 'J' or bIFSopzio = '03';
072100141001         // Scrivo nuovo record con gestione Errore per chiave duplicata
072200141001           write TNTBE000;
072300141001
072400141001       // Opzione "02"=modIFica
072500141002         WHEN  BIFSopzio = '02';
072600141001         // update record e controllo Errore per chiave duplicata
072700141001          update TNTBE000;
072800141001
072900141001       // Opzione "04"=cancellazione/ripristino
073000141002         WHEN  BIFSopzio = '04';
073100141001           update TNTBE000;
073200141001
073300141001         ENDSL;
073400141001
073500141001       ENDSR;
073600141001
073700141001       //--------------------------------------------------------------
073800141001       //?Pulizia video D01.
073900141001       //--------------------------------------------------------------
074000141001       BEGSR PulisciD01;
074100141001
074200141001         clear d1descopz;
074300141001         clear d1TBEke1;
074400141002         clear D1deske1;
074500141001         clear d1TBEke2;
074600141002         clear D1IFSdes;
074700141002         clear D1IFSdel;
074800141002         clear D1IFSann;
074900141001
075000141001         *in28 = *off;
075100141001         *in40 = *off;
075200141001         *in41 = *off;
075300141002         *in42 = *off;
075400141001
075500141001       ENDSR;
075600141001
075700141001       //--------------------------------------------------------------
075800141001       //?Imposto dati video D01.
075900141001       //--------------------------------------------------------------
076000141001       BEGSR RieD01;
076100141001
076200141001       // Recupero i dati dal file
076300141001         kTBEke1 = S1TBEke1;
076400141001         clear TBElin;
076500141001         chain (TBEcod:kTBEke1:S1TBEke2:TBElin) TNTBE01L;
076600141001
076700141001       // Imposto i campi a video
076800141001         D1tbeke2  = TBEke2;
076900141001         D1tbeke1  = TBEke1;
077000141002         dIFS      = TBEuni;
077100141002         D1IFSdes  = §IFSdes;
077200141002         D1IFSdel  = §IFSdel;
077300141002         D1IFSann  = §IFSann;
077400141002
077500141002       // Decodifico la info
077600141002         clear DIFO;
077700141002         clear TIBS02DS;
077800141002         T02mod = 'C';
077900141002         T02cod = 'IFO';
078000141002         T02ke1 = D1TBEke1;
078100141002         T02sif = knsif;
078200141002         TNTBE_RicercaControllo  (kpjba : tibs02ds);
078300141002         IF T02err <> *blanks;
078400141002           leavesr;
078500141002         ENDIF;
078600141002         dIFO = T02uni;
078700141002         D1deske1 = §IFOdes;
078800141001
078900141001       ENDSR;
079000141001
079100141001       //--------------------------------------------------------------
079200141001       //?Controllo dati video S01.
079300141001       //--------------------------------------------------------------
079400141001       BEGSR ContrS01;
079500141001
079600141001       // Set flag
079700141001         wEoFS01 = *off;
079800141001
079900141001       // Inizio lettura subfile
080000141001         DOW  not wEoFS01 and *in21;
080100141002           readc TBIFSS01;
080200141001         // Se fine subfile
080300141001           IF  %eof;
080400141001             wCarS01 = *on;
080500141001             leave;
080600141001           ENDIF;
080700141001         // Se è stata immessa un'opzione
080800141001           IF  S1opzione <> *zeros;
080900141001           // Reset ds di servizio
081000141002             clear TNTBIFSDS;
081100141001           // Controllo ed elaborazione opzione immessa
081200141001             SELECT;
081300141001           // Opzione "01" = scelta
081400141001             WHEN  S1opzione = 1 and *in01;
081500141002               BIFSopzio = '01';
081600141001           // Opzione "02" = modifica
081700141001             WHEN  S1opzione = 2 and not *in01 and S1TBEatb = *blanks;
081800141002               BIFSopzio = '02';
081900141001           // Opzione "03" = copia
082000141001             WHEN  S1opzione = 3 and not *in01 and S1TBEatb = *blanks;
082100141002               BIFSopzio = '03';
082200141001           // Opzione "04" = annullo/ripristino
082300141001             WHEN  S1opzione = 4 and not *in01;
082400141002               BIFSopzio = '04';
082500141001           // Opzione "05" = visualizzazione
082600141001             WHEN  S1opzione = 5;
082700141002               BIFSopzio = '05';
082800141001             ENDSL;
082900141001
083000141001           // Se immessa almeno un opzione valida
083100141002             IF  BIFSopzio <> *blanks;
083200141001             // Gstione del formato video
083300141001               exsr GesD01;
083400141001             // Se la gestione si è chiusa con ...
083500141001               SELECT;
083600141001             // F6=Conferma
083700141002               WHEN  BIFSricez = 'F';
083800141001                 clear S1opzione;
083900141001                 wCarS01 = *on;
084000141001             // F12=Ritorno
084100141002               WHEN  BIFSricez = 'L';
084200141001                 clear S1opzione;
084300141001                 wEoFS01 = *on;
084400141001             // Altrimenti
084500141001               OTHER;
084600141001                 *in22 = *on;
084700141001               ENDSL;
084800141001
084900141002               update TBIFSS01;
085000141001               *in22 = *off;
085100141001           // Fine opzione
085200141001             ENDIF;
085300141001         // Fine opzione
085400141001           ENDIF;
085500141001
085600141001         ENDDO;
085700141001
085800141001       ENDSR;
085900141001
086000141001       //--------------------------------------------------------------
086100141001       //?Posizionamento sul file tabelle.
086200141001       //--------------------------------------------------------------
086300141001       BEGSR SetllTBE01;
086400141001
086500141001         SELECT;
086600141001       // Chiave totale
086700141001         WHEN wKey = '1';
086800141001           setll TBEcod TNTBE01L;
086900141001       // chiave parziale
087000141001         WHEN wKey = '2';
087100141001           setll (TBEcod:TBEke1) TNTBE01L;
087200141001         ENDSL;
087300141001
087400141001       ENDSR;
087500141001
087600141001       //--------------------------------------------------------------
087700141001       //?Scrive video S01.
087800141001       //--------------------------------------------------------------
087900141001       BEGSR WrtS01;
088000141001
088100141001       // Se non raggiunto limite massimo di caricamento
088200141001         IF S01nrr < 9999;
088300141001         // Imposto campi di subfile
088400141001           exsr RieS01;
088500141001         // Imposto numeratore righe e numero relativo di record
088600141001           S01nrr += 1;
088700141001         // scrivo subfile
088800141002           write TBIFSS01;
088900141001         ENDIF;
089000141001
089100141001       ENDSR;
089200141001
089300141001       //--------------------------------------------------------------
089400141001       //?Imposto campi video S01.
089500141001       //--------------------------------------------------------------
089600141001       BEGSR RieS01;
089700141001
089800141001         S1TBEke1 = TBEke1;
089900141001         S1TBEke2 = TBEke2;
090000141002         dIFS = TBEuni;
090100141002         S1TBEuni = §IFSdes;
090200141001         S1TBEatb = TBEatb;
090300141001
090400141001       ENDSR;
090500141001
090600141001       //--------------------------------------------------------------
090700141001       //?Operazioni finali.
090800141001       //--------------------------------------------------------------
090900141001       BEGSR RoutEnd;
091000141001
091100141002         IF  BIFSesito = *blanks;
091200141002           BIFSesito = Eseguito;
091300141001         ENDIF;
091400141001
091500141002         kpjbu = TNTBIFSDS;
091600141001
091700141001         *inLR = *on;
091800141001         return;
091900141001
092000141001       ENDSR;
092100141001
092200141001      /end-free
092300141001
092400141001       //--------------------------------------------------------------
092500141001       //?Schiere a tempo di compilazione.
092600141001       //--------------------------------------------------------------
092700141001
092800141001** -Msg----------------------------------------------------------------------*
092900141001Immettere il codice Info                                                       01
093000141001Immettere la descrizione                                                       02
093100141001Info commerciale già esistente                                                 03
093200141002Immettere il Sottotipo                                                         04
093300141002Codice Info errato                                                             05
