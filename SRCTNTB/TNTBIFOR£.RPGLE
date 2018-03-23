000100141001      //--------------------------------------------------------------
000200141001      //?TNTBIFGR - Gestione tabella "IFG" Categoria Info Comm.li
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
001700141001     fTNTBIFGD  cf   e             workstn sfile(TBIFGS01:S01nrr)
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
005400141001     d TNTBIFGDS     e ds
005500141001
005600141001      // - Reperimento dati utente
005700141001     d TIBS34ds      e ds
005800141001
005900141001      // - Tabella IFG = Categoria Info Comm.le
006000141001     d dIFG          e ds
006100141001
006200141001      //---------------------------------------------------------------
006300141001      //?Definizione variabili globali.
006400141001      //---------------------------------------------------------------
006500141001      // - Flags booleani
006600141001     d wEoFS01         s               n   inz(*off)
006700141001     d wCarS01         s               n   inz(*off)
006800141001     d wCarW01         s               n   inz(*off)
006900141001
007000141001      // - Campi di comodo
007100141001     d blanks          s                   like(D1descopz)
007200141001     d kTBEke1         s                   like(TBEke1)
007300141001     d len             s              5u 0
007400141001     d S01nrr          s              4  0
007500141001     d savopzione      s                   like(S1opzione)
007600141001     d savrcdnbr       s                   like(C1rcdnbr)
007700141001     d wKey            s              1
007800141001     d wVideo          s             10
007900141001
008000141001      //---------------------------------------------------------------
008100141001      //?Definizione procedure usate.
008200141001      //---------------------------------------------------------------
008300141001
008400141001      //---------------------------------------------------------------
008500141001      //?Definizione prototipi.
008600141001      //---------------------------------------------------------------
008700141001      /copy gaitrasrc/srcprotopr,tibs34r
008800141001      /copy gaitrasrc/srcprotopr,xsrda8
008900141001
009000141001      //---------------------------------------------------------------
009100141001      //?Riepilogo indicatori.
009200141001      //---------------------------------------------------------------
009300141001      // 01 - ON ricerca -- OFF manutenzione
009400141001      // 02 - proteggo campo fase
009500141001      // 03 - proteggo campi video 01
009600141001      // 04 - visualizzazione
009700141001      // 05 - visualizzo subfile lingue
009800141001      // 20 - gestione subfile
009900141001      // 21 - gestione subfile
010000141001      // 22 - gestione subfile
010100141001      // 23 - gestione subfile
010200141001      // 28 - messaggio Errore
010300141001      // 40 - Chiave tabella
010400141001      // 41 - descrizione
010500141001
010600141001      //---------------------------------------------------------------
010700141001      //?M A I N - L I N E
010800141001      //---------------------------------------------------------------
010900141001
011000141001     c     *Entry        plist
011100141001     c                   parm                    KPJBA
011200141001
011300141001      /free
011400141001
011500141001       // Operazioni iniziali
011600141001       exsr RoutInz;
011700141001
011800141001       // Gestione Subfile
011900141001       exsr GesS01;
012000141001
012100141001       // Operazioni finali
012200141001       exsr RoutEnd;
012300141001
012400141001       //--------------------------------------------------------------
012500141001       //?Operazioni iniziali.
012600141001       //--------------------------------------------------------------
012700141001       BEGSR RoutInz;
012800141001
012900141001       // Ricezione parametri
013000141001         TNTBIFGDS = kpjbu;
013100141001
013200141001         SELECT;
013300141001       // Ricerca e scelta
013400141001         WHEN  BIFGfun = '1';
013500141001           *in01 = *on;
013600141001       // Manutenzione
013700141001         WHEN  BIFGfun = *blanks;
013800141001          *in01 = *off;
013900141001         OTHER;
014000141001           BIFGesito = Errore;
014100141001           exsr RoutEnd;
014200141001         ENDSL;
014300141001
014400141001       // Reperimento dati job
014500141001         exsr DatiJob;
014600141001
014700141001       ENDSR;
014800141001
014900141001       //--------------------------------------------------------------
015000141001       //?Reperimento Dati del job (Utente/Operativi).
015100141001       //--------------------------------------------------------------
015200141001       BEGSR DatiJob;
015300141001
015400141001         in(E) §AzUte;
015500141001         IF  not %error;
015600141001           in(E) §DatiUte;
015700141001         ENDIF;
015800141001         IF  %error or RSut = *blanks;
015900141001           clear TIBS34ds;
016000141001           tibs34r(tibs34ds);
016100141001           in §AzUte;
016200141001           in §DatiUte;
016300141001         ENDIF;
016400141001
016500141001       ENDSR;
016600141001
016700141001       //--------------------------------------------------------------
016800141001       //?Gestione video S01.
016900141001       //--------------------------------------------------------------
017000141001       BEGSR GesS01;
017100141001
017200141001       // Imposto variabili
017300141001         wCarS01 = *on;
017400141001         wVideo  = 'S01';
017500141001
017600141001       // Inizio elaborazione subfile
017700141001         DOU  wVideo <> 'S01';
017800141001
017900141001         // Caricamento subfile
018000141001           exsr Cars01;
018100141001
018200141001         // C1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
018300141001         // impostando C1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
018400141001         // l'ultimo tasto
018500141001           IF  C1csrrrn > 0;
018600141001             C1rcdnbr = C1csrrrn;
018700141001           ELSE;
018800141001             C1rcdnbr = savrcdnbr;
018900141001           ENDIF;
019000141001
019100141001         // Se non so quale pagina visualizzare forzo pagina 1
019200141001           IF  C1rcdnbr < 1;
019300141001             C1rcdnbr = 1;
019400141001           ENDIF;
019500141001
019600141001         // Salvo il record number del subfile
019700141001           savrcdnbr = C1rcdnbr;
019800141001
019900141001         // Emissione del subfile
020000141001           write TBIFGP01;
020100141001           exfmt TBIFGC01;
020200141001
020300141001         // Controllo tasti funzionali del subfile
020400141001           SELECT;
020500141001
020600141001         // F3=Fine
020700141001           WHEN  *inKC;
020800141001             wVideo = *blanks;
020900141001             BIFGricez = 'C';
021000141001             exsr RoutEnd;
021100141001
021200141001         // F5=Refresh
021300141001           WHEN  *inKE;
021400141001             wCarS01 = *on;
021500141001
021600141001         // F10=Immissione
021700141001           WHEN  *inKJ;
021800141001             wVideo = 'D01';
021900141001             clear TNTBIFGDS;
022000141001             BIFGcomand = 'J';
022100141001             exsr GesD01;
022200141001
022300141001         // F13=Ripetizione
022400141001           WHEN  *inKM;
022500141001             exsr RipetiOpz;
022600141001
022700141001         // In tutti gli altri casi
022800141001           OTHER;
022900141001         // Controllo ed elaborazione scelte su subfile
023000141001             exsr ContrS01;
023100141001           ENDSL;
023200141001
023300141001       // Fine elaborazione 'S01'
023400141001          ENDDO;
023500141001
023600141001        ENDSR;
023700141001
023800141001       //--------------------------------------------------------------
023900141001       //?Carica video S01.
024000141001       //--------------------------------------------------------------
024100141001       BEGSR Cars01;
024200141001
024300141001       // Se è stato richiesto il caricamento del subfile
024400141001         IF  wCarS01;
024500141001
024600141001         // Inizializzo il subfile
024700141001           S01nrr = 0;
024800141001           *in20 = *on;
024900141001           write TBIFGC01;
025000141001           *in20 = *off;
025100141001           *in21 = *off;
025200141001           *in22 = *off;
025300141001           *in23 = *off;
025400141001
025500141001         // Imposto la chiave di posizionamento e lettura file
025600141001           TBEcod = 'IFG';
025700141001           TBEke1 = C1setll;
025800141001
025900141001         // Se è stato scelto il posizionamento
026000141001           IF  C1setll <> *blanks;
026100141001             wKey = '2';
026200141001         // Altrimenti
026300141001           ELSE;
026400141001             wKey = '1';
026500141001           ENDIF;
026600141001
026700141001         // Posizionamento file
026800141001           exsr SetllTBE01;
026900141001
027000141001         // Fino a che non è fine file
027100141001           DOU  %eof(TNTBE01L);
027200141001
027300141001           // Leggo file
027400141001             reade(n) TBEcod TNTBE01L;
027500141001
027600141001           // Fine file esco
027700141001             IF  %eof(TNTBE01L);
027800141001               leave;
027900141001             ENDIF;
028000141001
028100141001           // Se in "ricerca/scelta" non carico i records annullati
028200141001             IF  (*in01 and TBEatb = *blanks) or not *in01;
028300141001           // Scrivo subfile
028400141001               clear S1opzione;
028500141001               exsr WrtS01;
028600141001             ENDIF;
028700141001
028800141001           ENDDO;
028900141001
029000141001       // Fine caricamento subfile
029100141001         ENDIF;
029200141001
029300141001       // Se scritto almeno un record
029400141001         IF  S01nrr > 0;
029500141001       // Indicatore di sflend
029600141001           *in21 = *on;
029700141001         ENDIF;
029800141001
029900141001       // Se non scritto neanche un record
030000141001         IF  S01nrr = 0;
030100141001       // Indicatore di sfldsp
030200141001           *in23 = *on;
030300141001         ENDIF;
030400141001
030500141001       // Disattivo opzione di caricamento subfile
030600141001         wCarS01 = *off;
030700141001
030800141001       ENDSR;
030900141001
031000141001       //--------------------------------------------------------------
031100141001       //?Carica video D01.
031200141001       //--------------------------------------------------------------
031300141001       BEGSR GesD01;
031400141001
031500141001       // Imposto il video a seconda della funzione richiesta
031600141001         exsr CarD01;
031700141001
031800141001       // Imposto variabile
031900141001         wVideo = 'D01';
032000141001
032100141001       // Fino a che la variabile resta settata come 'D01'
032200141001         DOU  wVideo <> 'D01';
032300141001
032400141001           *in02 = *off;
032500141001           *in03 = *off;
032600141001           *in04 = *off;
032700141001
032800141001         // se immessa opzione di 'scelta'
032900141001           IF  BIFGopzio = '01';
033000141001             BIFGke1 = S1TBEke1;
033100141001             BIFGdes = S1TBEuni;
033200141001             exsr RoutEnd;
033300141001
033400141001         // Se immessa un'altra opzione
033500141001           ELSE;
033600141001           // Se non è immissione/copia proteggo il campo della causale
033700141001             IF  BIFGcomand <> 'J' and BIFGopzio <> '03';
033800141001               *in02 = *on;
033900141001             ENDIF;
034000141001           // Se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
034100141001           // proteggo i campi del video
034200141001             IF  BIFGopzio = '04' or BIFGopzio = '05';
034300141001               *in03 = *on;
034400141001             ENDIF;
034500141001           // Se immessa opzione di 'visualizzazione'
034600141001           // non attivo F6
034700141001             IF  BIFGopzio = '05';
034800141001               *in04 = *on;
034900141001             ENDIF;
035000141001           // emetto il video
035100141001             exfmt TBIFGD01;
035200141001           ENDIF;
035300141001
035400141001         // Reset indicatore generico di Errore
035500141001           *in28 = *off;
035600141001
035700141001         // pulisco il campo messaggi
035800141001           clear VD1Msg;
035900141001
036000141001         // controllo tasti funzionali del video
036100141001           SELECT;
036200141001
036300141001         // F3=Fine
036400141001           WHEN  *inKC;
036500141001             BIFGricez = 'C';
036600141001             wVideo = *blanks;
036700141001             unlock TNTBE01L;
036800141001             exsr RoutEnd;
036900141001
037000141001         // F6=Conferma
037100141001           WHEN  *inKF;
037200141001             BIFGricez = 'F';
037300141001           // Controllo e decodIFico i dati del video
037400141001             exsr ContrD01;
037500141001           // Conferma per annullo/ripristino
037600141001             IF  BIFGopzio = '04';
037700141001               SELECT;
037800141001             // Annullamento
037900141001               WHEN TBEatb = *blanks;
038000141001                 TBEatb = 'A';
038100141001             // Ripristino
038200141001               WHEN TBEatb = 'A';
038300141001                 clear TBEatb;
038400141001               ENDSL;
038500141001             ENDIF;
038600141001
038700141001           // Se non riscontrati errori emetto la finestra con i dati per la trasmissione
038800141001             IF  not *in28;
038900141001               wCarW01 = *on;
039000141001               wVideo = 'W01';
039100141001               exsr GesW01;
039200141001             ENDIF;
039300141001
039400141001         // F8=Record successivo
039500141001           WHEN  *inKH;
039600141001             clear S1opzione;
039700141001             wCarS01 = *off;
039800141001             wVideo = 'S01';
039900141001             BIFGricez = 'H';
040000141001
040100141001         // F12=Ritorno
040200141001           WHEN  *inKL;
040300141001             clear S1opzione;
040400141001             BIFGricez = 'L';
040500141001             unlock TNTBE01L;
040600141001           // Se non è f12 da immissione/copia non ricarico il subfile
040700141001             IF  BIFGcomand = 'J' or BIFGopzio = '03';
040800141001               wCarS01 = *on;
040900141001             ELSE;
041000141001               wCarS01 = *off;
041100141001             ENDIF;
041200141001             wVideo = 'S01';
041300141001
041400141001         // Invio
041500141001           OTHER;
041600141001             IF  not *in03;
041700141001               exsr ContrD01;
041800141001             ENDIF;
041900141001
042000141001          ENDSL;
042100141001
042200141001       // Fine gestione 'D01'
042300141001        ENDDO;
042400141001
042500141001       ENDSR;
042600141001
042700141001       //--------------------------------------------------------------
042800141001       //?Ripeti Opzione S01.
042900141001       //--------------------------------------------------------------
043000141001       BEGSR RipetiOpz;
043100141001
043200141001         IF  C1csrrrn > 0;
043300141001           S01nrr = C1csrrrn;
043400141001           chain S01nrr TBIFGS01;
043500141001           IF  %found and S1opzione > 0;
043600141001             savopzione = S1opzione;
043700141001             *in22 = *on;
043800141001             update TBIFGS01;
043900141001
044000141001             wEoFS01 = *off;
044100141001             DOU  wEoFS01;
044200141001               S01nrr += 1;
044300141001               chain S01nrr TBIFGS01;
044400141001               IF  %found;
044500141001                 S1opzione = savopzione;
044600141001                 update TBIFGS01;
044700141001               ELSE;
044800141001                 wEoFS01 = *on;
044900141001               ENDIF;
045000141001             ENDDO;
045100141001
045200141001             *in22 = *off;
045300141001           ENDIF;
045400141001
045500141001         ENDIF;
045600141001
045700141001       ENDSR;
045800141001
045900141001       //--------------------------------------------------------------
046000141001       //?Carico dati video D01.
046100141001       //--------------------------------------------------------------
046200141001       BEGSR CarD01;
046300141001
046400141001       // Pulisco il formato video D01
046500141001         exsr PulisciD01;
046600141001
046700141001       // Controllo se 'immissione' 'modifica' 'copia' o altro
046800141001         SELECT;
046900141001
047000141001       // F10=Immissione
047100141001         WHEN  BIFGcomand = 'J';
047200141001           D1descopz = 'Immissione';
047300141001
047400141001       // Opzione "02" = modifica
047500141001         WHEN  BIFGopzio = '02';
047600141001           D1descopz = 'ModIFica';
047700141001           exsr RieD01;
047800141001
047900141001       // Opzione "03" = copia
048000141001         WHEN  BIFGopzio = '03';
048100141001           D1descopz = 'Copia';
048200141001           exsr RieD01;
048300141001
048400141001       // Opzione "04" = cancellazione/ripristino
048500141001         WHEN  BIFGopzio = '04';
048600141001           exsr RieD01;
048700141001         // Se richiesta 'cancellazione'
048800141001           IF  TBEatb = *blanks;
048900141001             D1descopz = 'Annullamento';
049000141001           ENDIF;
049100141001         // Se richiesto 'ripristino'
049200141001           IF  TBEatb = 'A';
049300141001             D1descopz = 'Ripristino';
049400141001           ENDIF;
049500141001
049600141001       // Opzione "05" = visualizzazione
049700141001         WHEN  BIFGopzio ='05';
049800141001           D1descopz = 'Visualizzazione';
049900141001           exsr RieD01;
050000141001
050100141001       // Fine scelta
050200141001         ENDSL;
050300141001
050400141001       // Centro la descrizione della funzione nel formato video
050500141001         len = (%len(D1descopz) - %len(%trimr(D1descopz))) / 2;
050600141001         D1descopz = %subst(blanks:1:len) + %trimr(D1descopz);
050700141001
050800141001       ENDSR;
050900141001
051000141001       //--------------------------------------------------------------
051100141001       //?Controllo dati video D01.
051200141001       //--------------------------------------------------------------
051300141001       BEGSR ContrD01;
051400141001
051500141001         *in28 = *off;
051600141001         *in40 = *off;
051700141001         *in41 = *off;
051800141001
051900141001       // Chiave tabella
052000141001         IF D1TBEke1 = *blanks;
052100141001           VD1Msg = Msg(01);
052200141001           *in28 = *on;
052300141001           *in40 = *on;
052400141001           leavesr;
052500141001         ENDIF;
052600141001
052700141001       // Se immissione controllo che non esista già
052800141001         IF  BIFGcomand = 'J' or BIFGopzio = '03';
052900141001           TBEcod = 'IFG';
053000141001           TBEke1 = D1TBEke1;
053100141001           TBEke2 = D1TBEke2;
053200141001           clear TBElin;
053300141001           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
053400141001           IF  %found(TNTBE01L);
053500141001             VD1Msg = Msg(03);
053600141001             *in28 = *on;
053700141001             *in40 = *on;
053800141001             leavesr;
053900141001           ENDIF;
054000141001         ENDIF;
054100141001
054200141001       // Descrizione
054300141001         IF  D1IFGdes = *blanks;
054400141001           VD1Msg = Msg(02);
054500141001           *in28 = *on;
054600141001           *in41 = *on;
054700141001           leavesr;
054800141001         ENDIF;
054900141001
055000141001       ENDSR;
055100141001
055200141001       //--------------------------------------------------------------
055300141001       //?Gestione video W01.
055400141001       //--------------------------------------------------------------
055500141001       BEGSR GesW01;
055600141001
055700141001       // Imposto i dati a video
055800141001         exsr CarW01;
055900141001
056000141001       // Fino a che la variabile resta settata come 'W01'
056100141001         DOU  wVideo <> 'W01';
056200141001
056300141001         // Reset indicatore generico di Errore
056400141001           *in28 = *off;
056500141001
056600141001         // Emetto il video
056700141001           exfmt TBIFGW01;
056800141001
056900141001         // Controllo tasti funzionali del video
057000141001           SELECT;
057100141001
057200141001         // F6=Conferma
057300141001           WHEN  *inKF;
057400141001           // Controllo i dati del video
057500141001             exsr ContrW01;
057600141001           // Se non riscontrati errori aggiorno il record corrente
057700141001             IF  not *in28;
057800141001               exsr Aggiorna;
057900141001               IF  not *in28;
058000141001                 IF  BIFGcomand = 'J';
058100141001                   wVideo = 'D01';
058200141001                   exsr CarD01;
058300141001                 ELSE;
058400141001                   wVideo = 'S01';
058500141001                 ENDIF;
058600141001               ENDIF;
058700141001             ENDIF;
058800141001
058900141001         // F12=Ritorno
059000141001           WHEN  *inKL;
059100141001             wVideo = 'D01';
059200141001             clear BIFGricez;
059300141001
059400141001         // Invio
059500141001           OTHER;
059600141001             exsr ContrW01;
059700141001
059800141001           ENDSL;
059900141001
060000141001       // Fine gestione 'W01'
060100141001         ENDDO;
060200141001
060300141001       ENDSR;
060400141001
060500141001       //--------------------------------------------------------------
060600141001       //?Carico i dati video W01.
060700141001       //--------------------------------------------------------------
060800141001       BEGSR CarW01;
060900141001
061000141001       // Pulisco i campi
061100141001         clear W1ftt;
061200141001         clear W1flt;
061300141001         clear W1ftr;
061400141001         clear W1dtr;
061500141001
061600141001       // Se non immissione imposto i campi del file
061700141001         IF  BIFGcomand <> 'J';
061800141001           W1ftt = TBEftt;
061900141001           W1flt = TBEflt;
062000141001           W1ftr = TBEftr;
062100141001         // Imposto la data
062200141001           IF  TBEdtr <> *zeros;
062300141001             clear Wlbdat;
062400141001             G02inv = TBEdtr;
062500141001             G02err = '3';
062600141001             xsrda8(wlbdat);
062700141001             W1dtr = G02dat;
062800141001           ENDIF;
062900141001         ENDIF;
063000141001
063100141001       ENDSR;
063200141001
063300141001       //--------------------------------------------------------------
063400141001       //?Controllo video W01.
063500141001       //--------------------------------------------------------------
063600141001       BEGSR ContrW01;
063700141001
063800141001       ENDSR;
063900141001
064000141001       //--------------------------------------------------------------
064100141001       //?Aggiorno la tabella.
064200141001       //--------------------------------------------------------------
064300141001       BEGSR Aggiorna;
064400141001
064500141001       // Imposto campi del file
064600141001         clear TBElin;
064700141001         TBEke1 = D1TBEke1;
064800141001         TBEke2 = D1TBEke2;
064900141001
065000141001         §IFGdes  = D1IFGdes;
065100141001         §IFGvis  = D1IFGvis;
065200141001         §IFGctpr = D1IFGctpr;
065300141001         §IFGval  = D1IFGval;
065400141001         §IFGstr  = D1IFGstr;
065500141001         §IFGdes = D1IFGdes;
065600141001
065700141001         TBEuni = dIFG;
065800141001
065900141001         TBEftt = W1ftt;
066000141001         TBEflt = W1flt;
066100141001         clear TBEftr;
066200141001         clear TBEdtr;
066300141001
066400141001       // Controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
066500141001         SELECT;
066600141001
066700141001       // F10=immissione - Opzione "03"=copia
066800141001         WHEN  BIFGcomand = 'J' or bIFGopzio = '03';
066900141001         // Scrivo nuovo record con gestione Errore per chiave duplicata
067000141001           write TNTBE000;
067100141001
067200141001       // Opzione "02"=modIFica
067300141001         WHEN  BIFGopzio = '02';
067400141001         // update record e controllo Errore per chiave duplicata
067500141001          update TNTBE000;
067600141001
067700141001       // Opzione "04"=cancellazione/ripristino
067800141001         WHEN  BIFGopzio = '04';
067900141001           update TNTBE000;
068000141001
068100141001         ENDSL;
068200141001
068300141001       ENDSR;
068400141001
068500141001       //--------------------------------------------------------------
068600141001       //?Pulizia video D01.
068700141001       //--------------------------------------------------------------
068800141001       BEGSR PulisciD01;
068900141001
069000141001         clear d1descopz;
069100141001         clear d1TBEke1;
069200141001         clear d1TBEke2;
069300141001         clear d1IFGdes;
069400141001         clear d1IFGvis;
069500141001         clear d1IFGctpr;
069600141001         clear d1IFGval;
069700141001         clear d1IFGstr;
069800141001
069900141001         *in28 = *off;
070000141001         *in40 = *off;
070100141001         *in41 = *off;
070200141001
070300141001       ENDSR;
070400141001
070500141001       //--------------------------------------------------------------
070600141001       //?Imposto dati video D01.
070700141001       //--------------------------------------------------------------
070800141001       BEGSR RieD01;
070900141001
071000141001       // Recupero i dati dal file
071100141001         kTBEke1 = S1TBEke1;
071200141001         clear TBElin;
071300141001         chain (TBEcod:kTBEke1:S1TBEke2:TBElin) TNTBE01L;
071400141001
071500141001       // Imposto i campi a video
071600141001         D1tbeke2  = TBEke2;
071700141001         D1tbeke1  = TBEke1;
071800141001         dIFG      = TBEuni;
071900141001         D1IFGdes  = §IFGdes;
072000141001         D1IFGvis  = §IFGvis;
072100141001         D1IFGctpr = §IFGctpr;
072200141001         D1IFGval  = §IFGval;
072300141001         D1IFGstr  = §IFGstr;
072400141001
072500141001       ENDSR;
072600141001
072700141001       //--------------------------------------------------------------
072800141001       //?Controllo dati video S01.
072900141001       //--------------------------------------------------------------
073000141001       BEGSR ContrS01;
073100141001
073200141001       // Set flag
073300141001         wEoFS01 = *off;
073400141001
073500141001       // Inizio lettura subfile
073600141001         DOW  not wEoFS01 and *in21;
073700141001           readc TBIFGS01;
073800141001         // Se fine subfile
073900141001           IF  %eof;
074000141001             wCarS01 = *on;
074100141001             leave;
074200141001           ENDIF;
074300141001         // Se è stata immessa un'opzione
074400141001           IF  S1opzione <> *zeros;
074500141001           // Reset ds di servizio
074600141001             clear TNTBIFGDS;
074700141001           // Controllo ed elaborazione opzione immessa
074800141001             SELECT;
074900141001           // Opzione "01" = scelta
075000141001             WHEN  S1opzione = 1 and *in01;
075100141001               BIFGopzio = '01';
075200141001           // Opzione "02" = modifica
075300141001             WHEN  S1opzione = 2 and not *in01 and S1TBEatb = *blanks;
075400141001               BIFGopzio = '02';
075500141001           // Opzione "03" = copia
075600141001             WHEN  S1opzione = 3 and not *in01 and S1TBEatb = *blanks;
075700141001               BIFGopzio = '03';
075800141001           // Opzione "04" = annullo/ripristino
075900141001             WHEN  S1opzione = 4 and not *in01;
076000141001               BIFGopzio = '04';
076100141001           // Opzione "05" = visualizzazione
076200141001             WHEN  S1opzione = 5;
076300141001               BIFGopzio = '05';
076400141001             ENDSL;
076500141001
076600141001           // Se immessa almeno un opzione valida
076700141001             IF  BIFGopzio <> *blanks;
076800141001             // Gstione del formato video
076900141001               exsr GesD01;
077000141001             // Se la gestione si è chiusa con ...
077100141001               SELECT;
077200141001             // F6=Conferma
077300141001               WHEN  BIFGricez = 'F';
077400141001                 clear S1opzione;
077500141001                 wCarS01 = *on;
077600141001             // F12=Ritorno
077700141001               WHEN  BIFGricez = 'L';
077800141001                 clear S1opzione;
077900141001                 wEoFS01 = *on;
078000141001             // Altrimenti
078100141001               OTHER;
078200141001                 *in22 = *on;
078300141001               ENDSL;
078400141001
078500141001               update TBIFGS01;
078600141001               *in22 = *off;
078700141001           // Fine opzione
078800141001             ENDIF;
078900141001         // Fine opzione
079000141001           ENDIF;
079100141001
079200141001         ENDDO;
079300141001
079400141001       ENDSR;
079500141001
079600141001       //--------------------------------------------------------------
079700141001       //?Posizionamento sul file tabelle.
079800141001       //--------------------------------------------------------------
079900141001       BEGSR SetllTBE01;
080000141001
080100141001         SELECT;
080200141001       // Chiave totale
080300141001         WHEN wKey = '1';
080400141001           setll TBEcod TNTBE01L;
080500141001       // chiave parziale
080600141001         WHEN wKey = '2';
080700141001           setll (TBEcod:TBEke1) TNTBE01L;
080800141001         ENDSL;
080900141001
081000141001       ENDSR;
081100141001
081200141001       //--------------------------------------------------------------
081300141001       //?Scrive video S01.
081400141001       //--------------------------------------------------------------
081500141001       BEGSR WrtS01;
081600141001
081700141001       // Se non raggiunto limite massimo di caricamento
081800141001         IF S01nrr < 9999;
081900141001         // Imposto campi di subfile
082000141001           exsr RieS01;
082100141001         // Imposto numeratore righe e numero relativo di record
082200141001           S01nrr += 1;
082300141001         // scrivo subfile
082400141001           write TBIFGS01;
082500141001         ENDIF;
082600141001
082700141001       ENDSR;
082800141001
082900141001       //--------------------------------------------------------------
083000141001       //?Imposto campi video S01.
083100141001       //--------------------------------------------------------------
083200141001       BEGSR RieS01;
083300141001
083400141001         S1TBEke1 = TBEke1;
083500141001         S1TBEke2 = TBEke2;
083600141001         dIFG = TBEuni;
083700141001         S1TBEuni = §IFGdes;
083800141001         S1TBEatb = TBEatb;
083900141001
084000141001       ENDSR;
084100141001
084200141001       //--------------------------------------------------------------
084300141001       //?Operazioni finali.
084400141001       //--------------------------------------------------------------
084500141001       BEGSR RoutEnd;
084600141001
084700141001         IF  BIFGesito = *blanks;
084800141001           BIFGesito = Eseguito;
084900141001         ENDIF;
085000141001
085100141001         kpjbu = TNTBIFGDS;
085200141001
085300141001         *inLR = *on;
085400141001         return;
085500141001
085600141001       ENDSR;
085700141001
085800141001      /end-free
085900141001
086000141001       //--------------------------------------------------------------
086100141001       //?Schiere a tempo di compilazione.
086200141001       //--------------------------------------------------------------
086300141001
086400141001** -Msg----------------------------------------------------------------------*
086500141001Immettere la categoria                                                         01
086600141001Immettere la descrizione                                                       02
086700141001Categoria già esistente                                                        03
