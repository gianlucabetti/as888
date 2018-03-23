000100150129       //--------------------------------------------------------------
000200150202       //?GESTIONE TABELLA "ECM" - Fasi avanzamento Campagna
000300150129       //--------------------------------------------------------------
000400150129     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500150129
000600150129       //--------------------------------------------------------------
000700150129       //?Dichiarazione file.                                          ?
000800150129       //--------------------------------------------------------------
000900150129
001000150129     fTNTBE01L  uf a e           k disk
001100150129     fTNTBE11L  uf a e           k disk    rename(TNTBE000:TNTBE11)
001200150129
001300150202     fTNTBECMD  cf   e             workstn sfile(TBECMS01:S01nrr)
001400150129
001500150129       //--------------------------------------------------------------
001600150129       //?Definizione costanti.                                        ?
001700150129       //--------------------------------------------------------------
001800150129     d Errore          c                   '1'
001900150129     d Eseguito        c                   '0'
002000150129
002100150129       //--------------------------------------------------------------
002200150129       //?Definizione schiere.                                         ?
002300150129       //--------------------------------------------------------------
002400150129
002500150129       // - Messaggi a video
002600150129     d Msg             s             78    dim(17) ctdata perrcd(1)
002700150129
002800150129       //--------------------------------------------------------------
002900150129       //?Definizione aree dati.                                       ?
003000150129       //--------------------------------------------------------------
003100150129
003200150129       // - Dati utente
003300150129     d §AzUte        e ds                  extname(AZUTE00F)
003400150129     d                                     dtaara
003500150129     d §DatiUte      e ds                  extname(dDatiUte)
003600150129     d                                     dtaara
003700150129
003800150129       //--------------------------------------------------------------
003900150129       //?Definizione strutture dati.                                  ?
004000150129       //--------------------------------------------------------------
004100150129
004200150129       // - Status
004300150129     d Psds           sds
004400150129     d   SDSpgm          *proc
004500150129
004600150129       // - Parametri ricevuti
004700150129     d KPJBA         e ds
004800150202     d TNTBECMDS     e ds
004900150129
005000150129       // - Reperimento dati utente
005100150129     d TIBS34ds      e ds
005200150129
005300150202       // - Tabella "ECM"
005400150202     d dECM          e ds
005500150129
005600150129       // - Controllo data
005700150129     d wlbdat          ds                  inz
005800150129     d  G02dat                 1      8  0
005900150129     d  G02inv                 9     16  0
006000150129     d  G02err                17     17
006100150129     d  G02tgi                18     22  0
006200150129
006300150129       //--------------------------------------------------------------
006400150129       //?Definizione procedure usate.                                 ?
006500150129       //--------------------------------------------------------------
006600150129
006700150129       // - Reperimento dati utente
006800150129      /copy gaitrasrc/srcProtoPR,TIBS34R
006900150129
007000150129       // - Controllo data
007100150129     d xsrda8          pr                  extpgm('XSRDA8')
007200150129     d  wlbdat                             likeds(wlbdat)
007300150129
007400150129       //--------------------------------------------------------------
007500150129       //?Definizione variabili globali.                               ?
007600150129       //--------------------------------------------------------------
007700150129
007800150129       // - Campi di comodo
007900150129     d blanks          s                   like(d1descopz)
008000150129     d comando         s              1
008100150129     d kTBEke1         s                   like(TBEke1)
008200150129     d len             s              5u 0
008300150129     d s01nrr          s              4  0
008400150129     d savopzione      s                   like(s1opzione)
008500150129     d savrcdnbr       s                   like(c1rcdnbr)
008600150205     d savECMuti       s                   like(BECMuti)
008700150129     d wrkkey          s              1
008800150129     d wrkeofs01       s              1
008900150129     d wrkcars01       s              1
009000150129     d wrkcarw01       s              1
009100150129     d $video          s             10
009200060428
009300150129
009400150129       //--------------------------------------------------------------
009500150129       //?Riepilogo indicatori.                                        ?
009600150129       //--------------------------------------------------------------
009700150129       // 01 - ON ricerca -- OFF manutenzione
009800150129       // 02 - proteggo campo fase
009900150202       // 03 - proteggo campi video
010000150129       // 04 - visualizzazione
010100150202       // 20 - gestione subfile
010200150202       // 21 - gestione subfile
010300150202       // 22 - gestione subfile
010400150129       // 28 - messaggio errore
010500150129       // 40 - fase
010600150129       // 41 - descrizione
010700150129       // 42 - descrizione subfile
010800150129       //--------------------------------------------------------------
010900150129
011000150129       //--------------------------------------------------------------
011100150129
011200150129     c     *Entry        plist
011300150129     c                   parm                    kpjba
011400150129
011500150129      /free
011600150129
011700150129       exsr sr_datijob;
011800150129       exsr sr_parm;
011900150129       exsr sr_c01;
012000150129       exsr sr_uscita;
012100150129
012200150129       // ------------------------------------------------------------------------
012300150129       // Elaborazione parametri ricevuti
012400150129       // ------------------------------------------------------------------------
012500150129       BEGSR sr_parm;
012600150129
012700150202         TNTBECMDS = kpjbu;
012800150205         savECMuti = BECMuti;
012900150129
013000150129         SELECT;
013100150129       // ricerca e scleta
013200150202         WHEN  BECMfun = '1';
013300150129           *in01 = *on;
013400150129       // manutenzione
013500150202         WHEN  BECMfun = *blanks;
013600150129           *in01 = *off;
013700150129       // altrimenti
013800150129         OTHER;
013900150202           BECMEsito = Errore;
014000150129           exsr sr_uscita;
014100150129         ENDSL;
014200150129
014300150129       ENDSR;
014400150129
014500150129       // ------------------------------------------------------------------------
014600150129       // Gestione subfile
014700150129       // ------------------------------------------------------------------------
014800150129       BEGSR sr_c01;
014900150129
015000150129       // imposto variabili
015100150129         wrkcars01 = *on;
015200150129         $video = 'C01';
015300150129
015400150129       // inizio elaborazione subfile
015500150129         DOU  $video <> 'C01';
015600150129
015700150129       // caricamento subfile
015800150129           exsr sr_cars01;
015900150129
016000150129       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
016100150129       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
016200150129       // l'ultimo tasto
016300150129           IF  c1csrrrn > 0;
016400150129             c1rcdnbr = c1csrrrn;
016500150129           ELSE;
016600150129             c1rcdnbr = savrcdnbr;
016700150129           ENDIF;
016800150129
016900150129       // se non so quale pagina visualizzare forzo pagina 1
017000150129           IF  c1rcdnbr < 1;
017100150129             c1rcdnbr = 1;
017200150129           ENDIF;
017300150129
017400150129       // salvo il record number del subfile
017500150129           savrcdnbr = c1rcdnbr;
017600150129
017700150129       // Emissione del subfile
017800150202           write TBECMP01;
017900150202           exfmt TBECMC01;
018000150129
018100150129       // controllo tasti funzionali del subfile
018200150129           SELECT;
018300150129
018400150129       // F3=Fine
018500150129           WHEN  *inkc;
018600150129             $video = *blanks;
018700150202             BECMricez = 'C';
018800150129             exsr sr_uscita;
018900150129
019000150129       // F5=Refresh
019100150129           WHEN  *inke;
019200150129             wrkcars01 = *on;
019300150129
019400150129       // F10=Immissione
019500150129           WHEN  *inkj;
019600150129             $video = 'D01';
019700150202             clear TNTBECMDS;
019800150202             BECMcomand = 'J';
019900150205             BECMuti = savECMuti;
020000150129             exsr sr_d01;
020100150129
020200150129       // F13=Ripetizione
020300150129           WHEN  *inkm;
020400150129             exsr sr_ripetiopz;
020500150129
020600150129       // in tutti gli altri casi
020700150129           OTHER;
020800150129       // controllo ed elaborazione scelte su subfile
020900150129             exsr sr_gestsfl;
021000150129           ENDSL;
021100150129
021200150129       // fine elaborazione 'C01'
021300150129         ENDDO;
021400150129
021500150129       ENDSR;
021600150129
021700150129       // ------------------------------------------------------------------------
021800150129       // Caricamento subfile
021900150129       // ------------------------------------------------------------------------
022000150129       BEGSR sr_cars01;
022100150129
022200150129       // se è stato richiesto il caricamento del subfile
022300150129         IF  wrkcars01 = *on;
022400150129
022500150129       // inizializzo il subfile
022600150129           s01nrr = 0;
022700150129           *in20 = *on;
022800150202           write TBECMC01;
022900150129           *in20 = *off;
023000150129           *in21 = *off;
023100150129           *in22 = *off;
023200150129           *in23 = *off;
023300150129
023400150129       // imposto la chiave di posizionamento e lettura file
023500150202           TBEcod = 'ECM';
023600150129           TBEke1 = c1setll;
023700150129
023800150129       // se è stato scelto il posizionamento
023900150129           IF  c1setll <> *blanks;
024000150129             wrkkey = '2';
024100150129       // altrimenti
024200150129           ELSE;
024300150129             wrkkey = '1';
024400150129           ENDIF;
024500150129
024600150129       // posizionamento file
024700150129           exsr sr_setlltbe01;
024800150129
024900150129       // fino a che non è fine file
025000150129           DOU  %eof(TNTBE01L);
025100150129
025200150129         // leggo file
025300150129             reade(n) TBEcod TNTBE01L;
025400150129
025500150129         // fine file esco
025600150129             IF  %eof(TNTBE01L);
025700150129               leave;
025800150129             ENDIF;
025900150129
026000150129         // se in "ricerca/scelta" non carico i records annullati
026100150129             IF  (*in01 and TBEatb = *blanks) or not *in01;
026200150129         // scrivo subfile
026300150129               clear s1opzione;
026400150129               exsr sr_wtrs01;
026500150129             ENDIF;
026600150129
026700150129           ENDDO;
026800150129
026900150129       // fine caricamento subfile
027000150129         ENDIF;
027100150129
027200150129       // se scritto almeno un record
027300150129         IF  s01nrr > 0;
027400150129       // indicatore di sflend
027500150129           *in21 = *on;
027600150129         ENDIF;
027700150129
027800150129       // se non scritto neanche un record
027900150129         IF  s01nrr = 0;
028000150129       // indicatore di sfldsp
028100150129           *in23 = *on;
028200150129         ENDIF;
028300150129
028400150129       // disattivo opzione di caricamento subfile
028500150129         wrkcars01 = *off;
028600150129
028700150129       ENDSR;
028800150129
028900150129       // ------------------------------------------------------------------------
029000150129       // Gestione videata
029100150129       // ------------------------------------------------------------------------
029200150129       BEGSR sr_d01;
029300150129
029400150129       // imposto il video a seconda della funzione richiesta
029500150129         exsr sr_setvideo;
029600150129
029700150129       // imposto variabile
029800150129         $video = 'D01';
029900150129
030000150129       // fino a che la variabile resta settata come 'D01'
030100150129         DOU $video <> 'D01';
030200150129
030300150129           *in02 = *off;
030400150129           *in03 = *off;
030500150129           *in04 = *off;
030600150129
030700150129       // se immessa opzione di 'scelta'
030800150202           IF  BECMopzio = '01';
030900150202             BECMke1 = s1TBEke1;
031000150202             BECMdes = s1TBEuni;
031100150129             exsr sr_uscita;
031200150129
031300150129         // se immessa un'altra opzione
031400150129           ELSE;
031500150129         // se non è immissione/copia proteggo il campo della causale
031600150202             IF  BECMcomand <> 'J' and BECMopzio <> '03';
031700150129               *in02 = *on;
031800150129             ENDIF;
031900150129          // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
032000150129          // proteggo i campi del video
032100150202             IF  BECMopzio = '04' or BECMopzio = '05';
032200150129               *in03 = *on;
032300150129             ENDIF;
032400150129          // se immessa opzione di 'visualizzazione'
032500150129          // non attivo F6
032600150202             IF  BECMopzio = '05';
032700150129               *in04 = *on;
032800150129             ENDIF;
032900150129          // emetto il video
033000150202             exfmt TBECMD01;
033100150129           ENDIF;
033200150129
033300150129         // reset indicatore generico di errore
033400150129           *in28 = *off;
033500150129
033600150129         // pulisco il campo messaggi
033700150129           clear vd1msg;
033800150129
033900150129         // controllo tasti funzionali del video
034000150129           SELECT;
034100150129
034200150129         // F3=Fine
034300150129           WHEN  *inkc;
034400150202             BECMricez = 'C';
034500150129             $video = *blanks;
034600150129             unlock TNTBE01L;
034700150129             exsr sr_uscita;
034800150129
034900150129         // F6=Conferma
035000150129           WHEN  *inkf;
035100150202             BECMricez = 'F';
035200150129         // controllo e decodifico i dati del video
035300150129             exsr sr_ctrd01;
035400150129         // conferma per annullo/ripristino
035500150202             IF  BECMopzio = '04';
035600150129               SELECT;
035700150129           // annullamento
035800150129               WHEN  TBEatb = *blanks;
035900150129                 TBEatb = 'A';
036000150129           // ripristino
036100150129               WHEN  TBEatb = 'A';
036200150129                 clear TBEatb;
036300150129               ENDSL;
036400150129             ENDIF;
036500150129
036600150129         // se non riscontrati errori emetto la finestra con i dati per la trasmissione
036700150129             IF  not *in28;
036800150129               wrkcarw01 = *on;
036900150129               $video = 'W01';
037000150129               exsr sr_w01;
037100150129             ENDIF;
037200150129
037300150129         // F8=Record successivo
037400150129           WHEN  *inkh;
037500150129             clear s1opzione;
037600150129             wrkcars01 = *off;
037700150129             $video = 'C01';
037800150202             BECMricez = 'H';
037900150129
038000150129         // F12=Ritorno
038100150129           WHEN  *inkl;
038200150129             clear s1opzione;
038300150202             BECMricez = 'L';
038400150129             unlock TNTBE01L;
038500150129         // se non è f12 da immissione/copia non ricarico il subfile
038600150202             IF  BECMcomand = 'J' or BECMopzio = '03';
038700150129               wrkcars01 = *on;
038800150129             ELSE;
038900150129               wrkcars01 = *off;
039000150129             ENDIF;
039100150129             $video = 'C01';
039200150129
039300150129         // Invio
039400150129           OTHER;
039500150129             IF  not *in03;
039600150129               exsr sr_ctrd01;
039700150129             ENDIF;
039800150129
039900150129           ENDSL;
040000150129
040100150129       // fine gestione 'D01'
040200150129         ENDDO;
040300150129
040400150129       ENDSR;
040500150129
040600150129       // ------------------------------------------------------------------------
040700150129       // Ripeto opzione in tutte le righe del subfile
040800150129       // ------------------------------------------------------------------------
040900150129       BEGSR sr_ripetiopz;
041000150129
041100150129         IF  c1csrrrn > 0;
041200150129           s01nrr = c1csrrrn;
041300150202           chain s01nrr TBECMS01;
041400150129           IF  %found and s1opzione > 0;
041500150129             savopzione = s1opzione;
041600150129             *in22 = *on;
041700150202             update TBECMS01;
041800150129
041900150129             wrkeofs01 = *off;
042000150129             DOU  wrkeofs01 = *on;
042100150129               s01nrr = s01nrr + 1;
042200150202               chain s01nrr TBECMS01;
042300150129               IF  %found;
042400150129                 s1opzione = savopzione;
042500150202                 update TBECMS01;
042600150129               ELSE;
042700150129                 wrkeofs01 = *on;
042800150129               ENDIF;
042900150129             ENDDO;
043000150129
043100150129             *in22 = *off;
043200150129
043300150129           ENDIF;
043400150129
043500150129         ENDIF;
043600150129
043700150129       ENDSR;
043800150129
043900150129       // ------------------------------------------------------------------------
044000150129       // Imposto i dati a video
044100150129       // ------------------------------------------------------------------------
044200150129       BEGSR sr_setvideo;
044300150129
044400150129       // pulisco il formato video D01
044500150129         exsr sr_puld01;
044600150129
044700150129       // controllo se 'immissione' 'modifica' 'copoa' o altro
044800150129         SELECT;
044900150129
045000150129       // F10=Immissione
045100150202         WHEN  BECMcomand = 'J';
045200150129           d1descopz = 'Immissione';
045300150129
045400150129       // Opzione "02"=modifica
045500150202         WHEN  BECMopzio = '02';
045600150129           d1descopz = 'Modifica';
045700150129           exsr sr_imposta;
045800150129
045900150129       // Opzione "03"=copia
046000150202         WHEN  BECMopzio = '03';
046100150129           d1descopz = 'Copia';
046200150129           exsr sr_imposta;
046300150129
046400150129       // Opzione "04"=cancellazione/ripristino
046500150202         WHEN  BECMopzio = '04';
046600150129           exsr sr_imposta;
046700150129        // se richiesta 'cancellazione'
046800150129           IF  TBEatb = *blanks;
046900150129             d1descopz = 'Annullamento';
047000150129           ENDIF;
047100150129        // se richiesto 'ripristino'
047200150129           IF TBEatb = 'A';
047300150129             d1descopz = 'Ripristino';
047400150129           ENDIF;
047500150129
047600150129       // Opzione "05"=visualizzazione
047700150202         WHEN  BECMOpzio ='05';
047800150129           d1descopz = 'Visualizzazione';
047900150129           exsr sr_imposta;
048000150129
048100150129       // Fine scelta
048200150129         ENDSL;
048300150129
048400150129       // centro la descrizione della funzione nel formato video
048500150129         len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
048600150129         d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
048700150129
048800150129       ENDSR;
048900150129
049000150129       // ------------------------------------------------------------------------
049100150129       // controllo video
049200150129       // ------------------------------------------------------------------------
049300150129       BEGSR sr_ctrd01;
049400150129
049500150129         *in28 = *off;
049600150129         *in40 = *off;
049700150129         *in41 = *off;
049800150129
049900150129       // fase ORM
050000150129         IF  d1tbeke1 = *blanks;
050100150129           vd1msg = Msg(01);
050200150129           *in28 = *on;
050300150129           *in40 = *on;
050400150129           leavesr;
050500150129         ENDIF;
050600150129
050700150129       // se immissione controllo che non esista già
050800150202         IF  BECMcomand = 'J' or BECMopzio = '03';
050900150202           TBEcod = 'ECM';
051000150129           TBEke1 = d1tbeke1;
051100150129           TBEke2 = d1tbeke2;
051200150129           clear TBElin;
051300150129           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
051400150129           IF  %found(TNTBE01L);
051500150129             vd1msg = Msg(03);
051600150129             *in28 = *on;
051700150129             *in40 = *on;
051800150129             leavesr;
051900150129           ENDIF;
052000150129         ENDIF;
052100150129
052200150129       // descrizione
052300150202         IF  d1ECMdes = *blanks;
052400150129           vd1msg = Msg(02);
052500150129           *in28 = *on;
052600150129           *in41 = *on;
052700150129           leavesr;
052800150129         ENDIF;
052900150129
053000150129       ENDSR;
053100150129
053200150129       // ------------------------------------------------------------------------
053300150129       // Gestione video dati di trasmissione
053400150129       // ------------------------------------------------------------------------
053500150129       BEGSR sr_w01;
053600150129
053700150129       // imposto i dati a video
053800150129         exsr sr_carw01;
053900150129
054000150129       // fino a che la variabile resta settata come 'W01'
054100150129         DOU  $video <> 'W01';
054200150129
054300150129       // reset indicatore generico di errore
054400150129           *in28 = *off;
054500150129
054600150129       // emetto il video
054700150202           exfmt TBECMW01;
054800150129
054900150129       // controllo tasti funzionali del video
055000150129           SELECT;
055100150129
055200150129       // F6=Conferma
055300150129           WHEN  *inkf;
055400150129         // controllo i dati del video
055500150129             exsr sr_ctrw01;
055600150129         // se non riscontrati errori aggiorno il record corrente
055700150129             IF  not *in28;
055800150129               exsr sr_aggiorna;
055900150202               IF  not *in28;
056000150202                 IF  BECMcomand = 'J';
056100150202                   $video = 'D01';
056200150202                   exsr sr_setvideo;
056300150202                 ELSE;
056400150202                   $video = 'C01';
056500150202                 ENDIF;
056600150202               ENDIF;
056700150129             ENDIF;
056800150129
056900150129       // F12=Ritorno
057000150129           WHEN  *inkl;
057100150129             $video = 'D01';
057200150202             clear BECMricez;
057300150129
057400150129       // Invio
057500150129           OTHER;
057600150129             exsr sr_ctrw01;
057700150129
057800150129           ENDSL;
057900150129
058000150129       // fine gestione 'W01'
058100150129         ENDDO;
058200150129
058300150129       ENDSR;
058400150129
058500150129       // ------------------------------------------------------------------------
058600150129       // Imposto i dati di trasmissione
058700150129       // ------------------------------------------------------------------------
058800150129       BEGSR sr_carw01;
058900150129
059000150129       // se pulisco i campi
059100150129         clear w1ftt;
059200150129         clear w1flt;
059300150129         clear w1ftr;
059400150129         clear w1dtr;
059500150129
059600150129       // se non immissione imposto i campi del file
059700150202         IF  BECMcomand <> 'J';
059800150129           w1ftt = TBEftt;
059900150129           w1flt = TBEflt;
060000150129           w1ftr = TBEftr;
060100150129       // imposto la data
060200150129           IF  TBEdtr <> *zeros;
060300150129             clear wlbdat;
060400150129             G02inv = TBEdtr;
060500150129             G02err = '3';
060600150129             xsrda8(wlbdat);
060700150129             w1dtr = G02dat;
060800150129           ENDIF;
060900150129
061000150129         ENDIF;
061100150129
061200150129       ENDSR;
061300150129
061400150129       // ------------------------------------------------------------------------
061500150129       // Controllo i dati di trasmissione
061600150129       // ------------------------------------------------------------------------
061700150129       BEGSR sr_ctrw01;
061800150129
061900150129       ENDSR;
062000150129
062100150129       // ------------------------------------------------------------------------
062200150129       // Aggiorno tabella
062300150129       // ------------------------------------------------------------------------
062400150129       BEGSR sr_aggiorna;
062500150129
062600150129       // imposto campi del file
062700150129         clear TBElin;
062800150129         TBEke1 = d1tbeke1;
062900150129         TBEke2 = d1tbeke2;
063000150129
063100150202         §ECMdes = d1ECMdes;
063200150202         §ECMaut = d1ECMaut;
063300150129
063400150202         TBEuni = dECM;
063500150129
063600150129         TBEftt = w1ftt;
063700150129         TBEflt = w1flt;
063800150129         clear TBEftr;
063900150129         clear TBEdtr;
064000150129
064100150129       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
064200150129         SELECT;
064300150129
064400150129       // F10=immissione - Opzione "03"=copia
064500150202         WHEN  BECMcomand = 'J' or BECMopzio = '03';
064600150129       // scrivo nuovo record con gestione errore per chiave duplicata
064700150129           write TNTBE000;
064800150129
064900150129       // Opzione "02"=modifica
065000150202         WHEN  BECMopzio = '02';
065100150129       // update record e controllo errore per chiave duplicata
065200150129           update TNTBE000;
065300150129
065400150129       // Opzione "04"=cancellazione/ripristino
065500150202         WHEN  BECMopzio = '04';
065600150129           update TNTBE000;
065700150129
065800150129         ENDSL;
065900150129
066000150129       ENDSR;
066100150129
066200150129       // ------------------------------------------------------------------------
066300150129       // pulizia video
066400150129       // ------------------------------------------------------------------------
066500150129       BEGSR sr_puld01;
066600150129
066700150129         clear d1descopz;
066800150129         clear d1tbeke1;
066900150129         clear d1tbeke2;
067000150202         clear d1ECMdes;
067100150202         clear d1ECMaut;
067200150129
067300150129         *in28 = *off;
067400150129         *in40 = *off;
067500150129         *in41 = *off;
067600150129
067700150129       ENDSR;
067800150129
067900150129       // ------------------------------------------------------------------------
068000150129       // Imposto i dati a video
068100150129       // ------------------------------------------------------------------------
068200150129       BEGSR sr_imposta;
068300150129
068400150129       // recupero i dati dal file
068500150129         kTBEke1 = s1tbeke1;
068600150129         clear TBElin;
068700150129         chain (TBEcod:kTBEke1:s1tbeke2:TBElin) TNTBE01L;
068800150129
068900150129       // imposto i campi a video
069000150129         d1tbeke2 = TBEke2;
069100150129         d1tbeke1 = TBEke1;
069200150202         dECM = TBEuni;
069300150202         d1ECMdes = §ECMdes;
069400150202         d1ECMaut = §ECMaut;
069500150129
069600150129       ENDSR;
069700150129
069800150129       // ------------------------------------------------------------------------
069900150129       // Gestione del subfile
070000150129       // ------------------------------------------------------------------------
070100150129       BEGSR sr_gestsfl;
070200150129
070300150129       // set flag
070400150129         wrkeofs01 = *off;
070500150129
070600150129       // inizio lettura subfile
070700150129         DOW  wrkeofs01 = *off and *in21;
070800150202           readc TBECMS01;
070900150129       // se fine subfile
071000150129           IF  %eof;
071100150129             wrkcars01 = *on;
071200150129             leave;
071300150129           ENDIF;
071400150129       // se è stata immessa un'opzione
071500150129           IF  s1opzione <> *zeros;
071600150129         // reset ds di servizio
071700150202             clear TNTBECMDS;
071800150205             BECMuti = savECMuti;
071900150129         // controllo ed elaborazione opzione immessa
072000150129             SELECT;
072100150129         // opzione "01"=scelta
072200150129             WHEN  s1opzione = 1 and *in01;
072300150202               BECMopzio = '01';
072400150129         // opzione "02"=modifica
072500150129             WHEN  s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
072600150202               BECMopzio = '02';
072700150129         // opzione "03"=copia
072800150129             WHEN  s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
072900150202               BECMopzio = '03';
073000150129         // opzione "04"=annullo/ripristino
073100150129             WHEN s1opzione = 4 and not *in01;
073200150202               BECMopzio = '04';
073300150129         // opzione "05"=visualizzazione
073400150129             WHEN s1opzione = 5;
073500150202               BECMopzio = '05';
073600150129             ENDSL;
073700150129
073800150129         // se immessa almeno un opzione valida
073900150202             IF  BECMopzio <> *blanks;
074000150129         // gstione del formato video
074100150129               exsr sr_d01;
074200150129         // se la gestione si è chiusa con ...
074300150129               SELECT;
074400150129           // F6=Conferma
074500150202               WHEN  BECMricez = 'F';
074600150129                 clear s1opzione;
074700150129                 wrkcars01 = *on;
074800150129          // F12=Ritorno
074900150202               WHEN  BECMricez = 'L';
075000150129                 clear s1opzione;
075100150129                 wrkeofs01 = *on;
075200150129          // altrimenti
075300150129               OTHER;
075400150129                 *in22 = *on;
075500150129               ENDSL;
075600150129
075700150202               update TBECMS01;
075800150129               *in22 = *off;
075900150129         // fine opzione
076000150129             ENDIF;
076100150129       // fine opzione
076200150129           ENDIF;
076300150129
076400150129         ENDDO;
076500150129
076600150129       ENDSR;
076700150129
076800150129       // ------------------------------------------------------------------------
076900150129       // Posizionamento sul file
077000150129       // ------------------------------------------------------------------------
077100150129       BEGSR sr_setlltbe01;
077200150129
077300150129         SELECT;
077400150129       // chiave totale
077500150129         WHEN  wrkkey = '1';
077600150129           setll TBEcod TNTBE01L;
077700150129       // chiave parziale
077800150129         WHEN  wrkkey = '2';
077900150129           setll (TBEcod:TBEke1) TNTBE01L;
078000150129         ENDSL;
078100150129
078200150129       ENDSR;
078300150129
078400150129       // ------------------------------------------------------------------------
078500150129       // Scrivo subfile
078600150129       // ------------------------------------------------------------------------
078700150129       BEGSR sr_wtrs01;
078800150129
078900150129       // se non raggiunto limite massimo di caricamento
079000150129         IF  s01nrr < 9999;
079100150129       // imposto campi di subfile
079200150129           exsr sr_sets01;
079300150202
079400150202         // se richiamato con flag di utilizzo da utente 'S'
079500150202         // NON visualizzo i rcd con flag di causale automatica
079600150202           IF  BECMuti = 'S' and §ECMaut <> *blanks;
079700150202             leavesr;
079800150202           ENDIF;
079900150202
080000150129       // imposto numeratore righe e numero relativo di record
080100150129           s01nrr = s01nrr + 1;
080200150129       // scrivo subfile
080300150202           write TBECMS01;
080400150129         ENDIF;
080500150129
080600150129       ENDSR;
080700150129
080800150129       // ------------------------------------------------------------------------
080900150129       // Imposto campi del subfile
081000150129       // ------------------------------------------------------------------------
081100150129       BEGSR sr_sets01;
081200150129
081300150129         s1tbeke1 = TBEke1;
081400150129         s1tbeke2 = TBEke2;
081500150202         dECM = TBEuni;
081600150202         s1tbeuni = §ECMdes;
081700150129         s1tbeatb = TBEatb;
081800150129
081900150129       ENDSR;
082000150129
082100150129       // ------------------------------------------------------------------------
082200150129       // Routine iniziale
082300150129       // ------------------------------------------------------------------------
082400150129         BEGSR sr_datijob;
082500150129
082600150129         in(E) §AzUte;
082700150129         IF  NOT %error;
082800150129           in(E) §DatiUte;
082900150129         ENDIF;
083000150129         IF  %error or RSut = *blanks;
083100150129           clear TIBS34ds;
083200150129           tibs34r(tibs34ds);
083300150129           in §AzUte;
083400150129           in §DatiUte;
083500150129         ENDIF;
083600150129
083700150129         ENDSR;
083800150129
083900150129       // ------------------------------------------------------------------------
084000150129       // Uscita dal programma
084100150129       // ------------------------------------------------------------------------
084200150129         BEGSR sr_uscita;
084300150129
084400150202           IF  BECMesito = *blanks;
084500150202             BECMesito = Eseguito;
084600150129           ENDIF;
084700150129
084800150202           kpjbu = TNTBECMDS;
084900150129
085000150129           *inlr = *on;
085100150129           return;
085200150129
085300150129         ENDSR;
085400150129
085500150129      /end-free
085600150129
085700150129** -MSG-                                                                     *
085800150202Immettere la cuasale                                                           01
085900150129Immettere la descrizione                                                       02
086000150202Causale già esistente                                                          03
086100150202Causale errata                                                                 04
