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
003300150129     d �AzUte        e ds                  extname(AZUTE00F)
003400150129     d                                     dtaara
003500150129     d �DatiUte      e ds                  extname(dDatiUte)
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
008600150129     d wrkkey          s              1
008700150129     d wrkeofs01       s              1
008800150129     d wrkcars01       s              1
008900150129     d wrkcarw01       s              1
009000150129     d $video          s             10
009100060428
009200150129
009300150129       //--------------------------------------------------------------
009400150129       //?Riepilogo indicatori.                                        ?
009500150129       //--------------------------------------------------------------
009600150129       // 01 - ON ricerca -- OFF manutenzione
009700150129       // 02 - proteggo campo fase
009800150202       // 03 - proteggo campi video
009900150129       // 04 - visualizzazione
010000150202       // 20 - gestione subfile
010100150202       // 21 - gestione subfile
010200150202       // 22 - gestione subfile
010300150129       // 28 - messaggio errore
010400150129       // 40 - fase
010500150129       // 41 - descrizione
010600150129       // 42 - descrizione subfile
010700150129       //--------------------------------------------------------------
010800150129
010900150129       //--------------------------------------------------------------
011000150129
011100150129     c     *Entry        plist
011200150129     c                   parm                    kpjba
011300150129
011400150129      /free
011500150129
011600150129       exsr sr_datijob;
011700150129       exsr sr_parm;
011800150129       exsr sr_c01;
011900150129       exsr sr_uscita;
012000150129
012100150129       // ------------------------------------------------------------------------
012200150129       // Elaborazione parametri ricevuti
012300150129       // ------------------------------------------------------------------------
012400150129       BEGSR sr_parm;
012500150129
012600150202         TNTBECMDS = kpjbu;
012700150129
012800150129         SELECT;
012900150129       // ricerca e scleta
013000150202         WHEN  BECMfun = '1';
013100150129           *in01 = *on;
013200150129       // manutenzione
013300150202         WHEN  BECMfun = *blanks;
013400150129           *in01 = *off;
013500150129       // altrimenti
013600150129         OTHER;
013700150202           BECMEsito = Errore;
013800150129           exsr sr_uscita;
013900150129         ENDSL;
014000150129
014100150129       ENDSR;
014200150129
014300150129       // ------------------------------------------------------------------------
014400150129       // Gestione subfile
014500150129       // ------------------------------------------------------------------------
014600150129       BEGSR sr_c01;
014700150129
014800150129       // imposto variabili
014900150129         wrkcars01 = *on;
015000150129         $video = 'C01';
015100150129
015200150129       // inizio elaborazione subfile
015300150129         DOU  $video <> 'C01';
015400150129
015500150129       // caricamento subfile
015600150129           exsr sr_cars01;
015700150129
015800150129       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
015900150129       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
016000150129       // l'ultimo tasto
016100150129           IF  c1csrrrn > 0;
016200150129             c1rcdnbr = c1csrrrn;
016300150129           ELSE;
016400150129             c1rcdnbr = savrcdnbr;
016500150129           ENDIF;
016600150129
016700150129       // se non so quale pagina visualizzare forzo pagina 1
016800150129           IF  c1rcdnbr < 1;
016900150129             c1rcdnbr = 1;
017000150129           ENDIF;
017100150129
017200150129       // salvo il record number del subfile
017300150129           savrcdnbr = c1rcdnbr;
017400150129
017500150129       // Emissione del subfile
017600150202           write TBECMP01;
017700150202           exfmt TBECMC01;
017800150129
017900150129       // controllo tasti funzionali del subfile
018000150129           SELECT;
018100150129
018200150129       // F3=Fine
018300150129           WHEN  *inkc;
018400150129             $video = *blanks;
018500150202             BECMricez = 'C';
018600150129             exsr sr_uscita;
018700150129
018800150129       // F5=Refresh
018900150129           WHEN  *inke;
019000150129             wrkcars01 = *on;
019100150129
019200150129       // F10=Immissione
019300150129           WHEN  *inkj;
019400150129             $video = 'D01';
019500150202             clear TNTBECMDS;
019600150202             BECMcomand = 'J';
019700150129             exsr sr_d01;
019800150129
019900150129       // F13=Ripetizione
020000150129           WHEN  *inkm;
020100150129             exsr sr_ripetiopz;
020200150129
020300150129       // in tutti gli altri casi
020400150129           OTHER;
020500150129       // controllo ed elaborazione scelte su subfile
020600150129             exsr sr_gestsfl;
020700150129           ENDSL;
020800150129
020900150129       // fine elaborazione 'C01'
021000150129         ENDDO;
021100150129
021200150129       ENDSR;
021300150129
021400150129       // ------------------------------------------------------------------------
021500150129       // Caricamento subfile
021600150129       // ------------------------------------------------------------------------
021700150129       BEGSR sr_cars01;
021800150129
021900150129       // se � stato richiesto il caricamento del subfile
022000150129         IF  wrkcars01 = *on;
022100150129
022200150129       // inizializzo il subfile
022300150129           s01nrr = 0;
022400150129           *in20 = *on;
022500150202           write TBECMC01;
022600150129           *in20 = *off;
022700150129           *in21 = *off;
022800150129           *in22 = *off;
022900150129           *in23 = *off;
023000150129
023100150129       // imposto la chiave di posizionamento e lettura file
023200150202           TBEcod = 'ECM';
023300150129           TBEke1 = c1setll;
023400150129
023500150129       // se � stato scelto il posizionamento
023600150129           IF  c1setll <> *blanks;
023700150129             wrkkey = '2';
023800150129       // altrimenti
023900150129           ELSE;
024000150129             wrkkey = '1';
024100150129           ENDIF;
024200150129
024300150129       // posizionamento file
024400150129           exsr sr_setlltbe01;
024500150129
024600150129       // fino a che non � fine file
024700150129           DOU  %eof(TNTBE01L);
024800150129
024900150129         // leggo file
025000150129             reade(n) TBEcod TNTBE01L;
025100150129
025200150129         // fine file esco
025300150129             IF  %eof(TNTBE01L);
025400150129               leave;
025500150129             ENDIF;
025600150129
025700150129         // se in "ricerca/scelta" non carico i records annullati
025800150129             IF  (*in01 and TBEatb = *blanks) or not *in01;
025900150129         // scrivo subfile
026000150129               clear s1opzione;
026100150129               exsr sr_wtrs01;
026200150129             ENDIF;
026300150129
026400150129           ENDDO;
026500150129
026600150129       // fine caricamento subfile
026700150129         ENDIF;
026800150129
026900150129       // se scritto almeno un record
027000150129         IF  s01nrr > 0;
027100150129       // indicatore di sflend
027200150129           *in21 = *on;
027300150129         ENDIF;
027400150129
027500150129       // se non scritto neanche un record
027600150129         IF  s01nrr = 0;
027700150129       // indicatore di sfldsp
027800150129           *in23 = *on;
027900150129         ENDIF;
028000150129
028100150129       // disattivo opzione di caricamento subfile
028200150129         wrkcars01 = *off;
028300150129
028400150129       ENDSR;
028500150129
028600150129       // ------------------------------------------------------------------------
028700150129       // Gestione videata
028800150129       // ------------------------------------------------------------------------
028900150129       BEGSR sr_d01;
029000150129
029100150129       // imposto il video a seconda della funzione richiesta
029200150129         exsr sr_setvideo;
029300150129
029400150129       // imposto variabile
029500150129         $video = 'D01';
029600150129
029700150129       // fino a che la variabile resta settata come 'D01'
029800150129         DOU $video <> 'D01';
029900150129
030000150129           *in02 = *off;
030100150129           *in03 = *off;
030200150129           *in04 = *off;
030300150129
030400150129       // se immessa opzione di 'scelta'
030500150202           IF  BECMopzio = '01';
030600150202             BECMke1 = s1TBEke1;
030700150202             BECMdes = s1TBEuni;
030800150129             exsr sr_uscita;
030900150129
031000150129         // se immessa un'altra opzione
031100150129           ELSE;
031200150129         // se non � immissione/copia proteggo il campo della causale
031300150202             IF  BECMcomand <> 'J' and BECMopzio <> '03';
031400150129               *in02 = *on;
031500150129             ENDIF;
031600150129          // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
031700150129          // proteggo i campi del video
031800150202             IF  BECMopzio = '04' or BECMopzio = '05';
031900150129               *in03 = *on;
032000150129             ENDIF;
032100150129          // se immessa opzione di 'visualizzazione'
032200150129          // non attivo F6
032300150202             IF  BECMopzio = '05';
032400150129               *in04 = *on;
032500150129             ENDIF;
032600150129          // emetto il video
032700150202             exfmt TBECMD01;
032800150129           ENDIF;
032900150129
033000150129         // reset indicatore generico di errore
033100150129           *in28 = *off;
033200150129
033300150129         // pulisco il campo messaggi
033400150129           clear vd1msg;
033500150129
033600150129         // controllo tasti funzionali del video
033700150129           SELECT;
033800150129
033900150129         // F3=Fine
034000150129           WHEN  *inkc;
034100150202             BECMricez = 'C';
034200150129             $video = *blanks;
034300150129             unlock TNTBE01L;
034400150129             exsr sr_uscita;
034500150129
034600150129         // F6=Conferma
034700150129           WHEN  *inkf;
034800150202             BECMricez = 'F';
034900150129         // controllo e decodifico i dati del video
035000150129             exsr sr_ctrd01;
035100150129         // conferma per annullo/ripristino
035200150202             IF  BECMopzio = '04';
035300150129               SELECT;
035400150129           // annullamento
035500150129               WHEN  TBEatb = *blanks;
035600150129                 TBEatb = 'A';
035700150129           // ripristino
035800150129               WHEN  TBEatb = 'A';
035900150129                 clear TBEatb;
036000150129               ENDSL;
036100150129             ENDIF;
036200150129
036300150129         // se non riscontrati errori emetto la finestra con i dati per la trasmissione
036400150129             IF  not *in28;
036500150129               wrkcarw01 = *on;
036600150129               $video = 'W01';
036700150129               exsr sr_w01;
036800150129             ENDIF;
036900150129
037000150129         // F8=Record successivo
037100150129           WHEN  *inkh;
037200150129             clear s1opzione;
037300150129             wrkcars01 = *off;
037400150129             $video = 'C01';
037500150202             BECMricez = 'H';
037600150129
037700150129         // F12=Ritorno
037800150129           WHEN  *inkl;
037900150129             clear s1opzione;
038000150202             BECMricez = 'L';
038100150129             unlock TNTBE01L;
038200150129         // se non � f12 da immissione/copia non ricarico il subfile
038300150202             IF  BECMcomand = 'J' or BECMopzio = '03';
038400150129               wrkcars01 = *on;
038500150129             ELSE;
038600150129               wrkcars01 = *off;
038700150129             ENDIF;
038800150129             $video = 'C01';
038900150129
039000150129         // Invio
039100150129           OTHER;
039200150129             IF  not *in03;
039300150129               exsr sr_ctrd01;
039400150129             ENDIF;
039500150129
039600150129           ENDSL;
039700150129
039800150129       // fine gestione 'D01'
039900150129         ENDDO;
040000150129
040100150129       ENDSR;
040200150129
040300150129       // ------------------------------------------------------------------------
040400150129       // Ripeto opzione in tutte le righe del subfile
040500150129       // ------------------------------------------------------------------------
040600150129       BEGSR sr_ripetiopz;
040700150129
040800150129         IF  c1csrrrn > 0;
040900150129           s01nrr = c1csrrrn;
041000150202           chain s01nrr TBECMS01;
041100150129           IF  %found and s1opzione > 0;
041200150129             savopzione = s1opzione;
041300150129             *in22 = *on;
041400150202             update TBECMS01;
041500150129
041600150129             wrkeofs01 = *off;
041700150129             DOU  wrkeofs01 = *on;
041800150129               s01nrr = s01nrr + 1;
041900150202               chain s01nrr TBECMS01;
042000150129               IF  %found;
042100150129                 s1opzione = savopzione;
042200150202                 update TBECMS01;
042300150129               ELSE;
042400150129                 wrkeofs01 = *on;
042500150129               ENDIF;
042600150129             ENDDO;
042700150129
042800150129             *in22 = *off;
042900150129
043000150129           ENDIF;
043100150129
043200150129         ENDIF;
043300150129
043400150129       ENDSR;
043500150129
043600150129       // ------------------------------------------------------------------------
043700150129       // Imposto i dati a video
043800150129       // ------------------------------------------------------------------------
043900150129       BEGSR sr_setvideo;
044000150129
044100150129       // pulisco il formato video D01
044200150129         exsr sr_puld01;
044300150129
044400150129       // controllo se 'immissione' 'modifica' 'copoa' o altro
044500150129         SELECT;
044600150129
044700150129       // F10=Immissione
044800150202         WHEN  BECMcomand = 'J';
044900150129           d1descopz = 'Immissione';
045000150129
045100150129       // Opzione "02"=modifica
045200150202         WHEN  BECMopzio = '02';
045300150129           d1descopz = 'Modifica';
045400150129           exsr sr_imposta;
045500150129
045600150129       // Opzione "03"=copia
045700150202         WHEN  BECMopzio = '03';
045800150129           d1descopz = 'Copia';
045900150129           exsr sr_imposta;
046000150129
046100150129       // Opzione "04"=cancellazione/ripristino
046200150202         WHEN  BECMopzio = '04';
046300150129           exsr sr_imposta;
046400150129        // se richiesta 'cancellazione'
046500150129           IF  TBEatb = *blanks;
046600150129             d1descopz = 'Annullamento';
046700150129           ENDIF;
046800150129        // se richiesto 'ripristino'
046900150129           IF TBEatb = 'A';
047000150129             d1descopz = 'Ripristino';
047100150129           ENDIF;
047200150129
047300150129       // Opzione "05"=visualizzazione
047400150202         WHEN  BECMOpzio ='05';
047500150129           d1descopz = 'Visualizzazione';
047600150129           exsr sr_imposta;
047700150129
047800150129       // Fine scelta
047900150129         ENDSL;
048000150129
048100150129       // centro la descrizione della funzione nel formato video
048200150129         len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
048300150129         d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
048400150129
048500150129       ENDSR;
048600150129
048700150129       // ------------------------------------------------------------------------
048800150129       // controllo video
048900150129       // ------------------------------------------------------------------------
049000150129       BEGSR sr_ctrd01;
049100150129
049200150129         *in28 = *off;
049300150129         *in40 = *off;
049400150129         *in41 = *off;
049500150129
049600150129       // fase ORM
049700150129         IF  d1tbeke1 = *blanks;
049800150129           vd1msg = Msg(01);
049900150129           *in28 = *on;
050000150129           *in40 = *on;
050100150129           leavesr;
050200150129         ENDIF;
050300150129
050400150129       // se immissione controllo che non esista gi�
050500150202         IF  BECMcomand = 'J' or BECMopzio = '03';
050600150202           TBEcod = 'ECM';
050700150129           TBEke1 = d1tbeke1;
050800150129           TBEke2 = d1tbeke2;
050900150129           clear TBElin;
051000150129           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
051100150129           IF  %found(TNTBE01L);
051200150129             vd1msg = Msg(03);
051300150129             *in28 = *on;
051400150129             *in40 = *on;
051500150129             leavesr;
051600150129           ENDIF;
051700150129         ENDIF;
051800150129
051900150129       // descrizione
052000150202         IF  d1ECMdes = *blanks;
052100150129           vd1msg = Msg(02);
052200150129           *in28 = *on;
052300150129           *in41 = *on;
052400150129           leavesr;
052500150129         ENDIF;
052600150129
052700150129       ENDSR;
052800150129
052900150129       // ------------------------------------------------------------------------
053000150129       // Gestione video dati di trasmissione
053100150129       // ------------------------------------------------------------------------
053200150129       BEGSR sr_w01;
053300150129
053400150129       // imposto i dati a video
053500150129         exsr sr_carw01;
053600150129
053700150129       // fino a che la variabile resta settata come 'W01'
053800150129         DOU  $video <> 'W01';
053900150129
054000150129       // reset indicatore generico di errore
054100150129           *in28 = *off;
054200150129
054300150129       // emetto il video
054400150202           exfmt TBECMW01;
054500150129
054600150129       // controllo tasti funzionali del video
054700150129           SELECT;
054800150129
054900150129       // F6=Conferma
055000150129           WHEN  *inkf;
055100150129         // controllo i dati del video
055200150129             exsr sr_ctrw01;
055300150129         // se non riscontrati errori aggiorno il record corrente
055400150129             IF  not *in28;
055500150129               exsr sr_aggiorna;
055600150202               IF  not *in28;
055700150202                 IF  BECMcomand = 'J';
055800150202                   $video = 'D01';
055900150202                   exsr sr_setvideo;
056000150202                 ELSE;
056100150202                   $video = 'C01';
056200150202                 ENDIF;
056300150202               ENDIF;
056400150129             ENDIF;
056500150129
056600150129       // F12=Ritorno
056700150129           WHEN  *inkl;
056800150129             $video = 'D01';
056900150202             clear BECMricez;
057000150129
057100150129       // Invio
057200150129           OTHER;
057300150129             exsr sr_ctrw01;
057400150129
057500150129           ENDSL;
057600150129
057700150129       // fine gestione 'W01'
057800150129         ENDDO;
057900150129
058000150129       ENDSR;
058100150129
058200150129       // ------------------------------------------------------------------------
058300150129       // Imposto i dati di trasmissione
058400150129       // ------------------------------------------------------------------------
058500150129       BEGSR sr_carw01;
058600150129
058700150129       // se pulisco i campi
058800150129         clear w1ftt;
058900150129         clear w1flt;
059000150129         clear w1ftr;
059100150129         clear w1dtr;
059200150129
059300150129       // se non immissione imposto i campi del file
059400150202         IF  BECMcomand <> 'J';
059500150129           w1ftt = TBEftt;
059600150129           w1flt = TBEflt;
059700150129           w1ftr = TBEftr;
059800150129       // imposto la data
059900150129           IF  TBEdtr <> *zeros;
060000150129             clear wlbdat;
060100150129             G02inv = TBEdtr;
060200150129             G02err = '3';
060300150129             xsrda8(wlbdat);
060400150129             w1dtr = G02dat;
060500150129           ENDIF;
060600150129
060700150129         ENDIF;
060800150129
060900150129       ENDSR;
061000150129
061100150129       // ------------------------------------------------------------------------
061200150129       // Controllo i dati di trasmissione
061300150129       // ------------------------------------------------------------------------
061400150129       BEGSR sr_ctrw01;
061500150129
061600150129       ENDSR;
061700150129
061800150129       // ------------------------------------------------------------------------
061900150129       // Aggiorno tabella
062000150129       // ------------------------------------------------------------------------
062100150129       BEGSR sr_aggiorna;
062200150129
062300150129       // imposto campi del file
062400150129         clear TBElin;
062500150129         TBEke1 = d1tbeke1;
062600150129         TBEke2 = d1tbeke2;
062700150129
062800150202         �ECMdes = d1ECMdes;
062900150202         �ECMaut = d1ECMaut;
063000150129
063100150202         TBEuni = dECM;
063200150129
063300150129         TBEftt = w1ftt;
063400150129         TBEflt = w1flt;
063500150129         clear TBEftr;
063600150129         clear TBEdtr;
063700150129
063800150129       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
063900150129         SELECT;
064000150129
064100150129       // F10=immissione - Opzione "03"=copia
064200150202         WHEN  BECMcomand = 'J' or BECMopzio = '03';
064300150129       // scrivo nuovo record con gestione errore per chiave duplicata
064400150129           write TNTBE000;
064500150129
064600150129       // Opzione "02"=modifica
064700150202         WHEN  BECMopzio = '02';
064800150129       // update record e controllo errore per chiave duplicata
064900150129           update TNTBE000;
065000150129
065100150129       // Opzione "04"=cancellazione/ripristino
065200150202         WHEN  BECMopzio = '04';
065300150129           update TNTBE000;
065400150129
065500150129         ENDSL;
065600150129
065700150129       ENDSR;
065800150129
065900150129       // ------------------------------------------------------------------------
066000150129       // pulizia video
066100150129       // ------------------------------------------------------------------------
066200150129       BEGSR sr_puld01;
066300150129
066400150129         clear d1descopz;
066500150129         clear d1tbeke1;
066600150129         clear d1tbeke2;
066700150202         clear d1ECMdes;
066800150202         clear d1ECMaut;
066900150129
067000150129         *in28 = *off;
067100150129         *in40 = *off;
067200150129         *in41 = *off;
067300150129
067400150129       ENDSR;
067500150129
067600150129       // ------------------------------------------------------------------------
067700150129       // Imposto i dati a video
067800150129       // ------------------------------------------------------------------------
067900150129       BEGSR sr_imposta;
068000150129
068100150129       // recupero i dati dal file
068200150129         kTBEke1 = s1tbeke1;
068300150129         clear TBElin;
068400150129         chain (TBEcod:kTBEke1:s1tbeke2:TBElin) TNTBE01L;
068500150129
068600150129       // imposto i campi a video
068700150129         d1tbeke2 = TBEke2;
068800150129         d1tbeke1 = TBEke1;
068900150202         dECM = TBEuni;
069000150202         d1ECMdes = �ECMdes;
069100150202         d1ECMaut = �ECMaut;
069200150129
069300150129       ENDSR;
069400150129
069500150129       // ------------------------------------------------------------------------
069600150129       // Gestione del subfile
069700150129       // ------------------------------------------------------------------------
069800150129       BEGSR sr_gestsfl;
069900150129
070000150129       // set flag
070100150129         wrkeofs01 = *off;
070200150129
070300150129       // inizio lettura subfile
070400150129         DOW  wrkeofs01 = *off and *in21;
070500150202           readc TBECMS01;
070600150129       // se fine subfile
070700150129           IF  %eof;
070800150129             wrkcars01 = *on;
070900150129             leave;
071000150129           ENDIF;
071100150129       // se � stata immessa un'opzione
071200150129           IF  s1opzione <> *zeros;
071300150129         // reset ds di servizio
071400150202             clear TNTBECMDS;
071500150129         // controllo ed elaborazione opzione immessa
071600150129             SELECT;
071700150129         // opzione "01"=scelta
071800150129             WHEN  s1opzione = 1 and *in01;
071900150202               BECMopzio = '01';
072000150129         // opzione "02"=modifica
072100150129             WHEN  s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
072200150202               BECMopzio = '02';
072300150129         // opzione "03"=copia
072400150129             WHEN  s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
072500150202               BECMopzio = '03';
072600150129         // opzione "04"=annullo/ripristino
072700150129             WHEN s1opzione = 4 and not *in01;
072800150202               BECMopzio = '04';
072900150129         // opzione "05"=visualizzazione
073000150129             WHEN s1opzione = 5;
073100150202               BECMopzio = '05';
073200150129             ENDSL;
073300150129
073400150129         // se immessa almeno un opzione valida
073500150202             IF  BECMopzio <> *blanks;
073600150129         // gstione del formato video
073700150129               exsr sr_d01;
073800150129         // se la gestione si � chiusa con ...
073900150129               SELECT;
074000150129           // F6=Conferma
074100150202               WHEN  BECMricez = 'F';
074200150129                 clear s1opzione;
074300150129                 wrkcars01 = *on;
074400150129          // F12=Ritorno
074500150202               WHEN  BECMricez = 'L';
074600150129                 clear s1opzione;
074700150129                 wrkeofs01 = *on;
074800150129          // altrimenti
074900150129               OTHER;
075000150129                 *in22 = *on;
075100150129               ENDSL;
075200150129
075300150202               update TBECMS01;
075400150129               *in22 = *off;
075500150129         // fine opzione
075600150129             ENDIF;
075700150129       // fine opzione
075800150129           ENDIF;
075900150129
076000150129         ENDDO;
076100150129
076200150129       ENDSR;
076300150129
076400150129       // ------------------------------------------------------------------------
076500150129       // Posizionamento sul file
076600150129       // ------------------------------------------------------------------------
076700150129       BEGSR sr_setlltbe01;
076800150129
076900150129         SELECT;
077000150129       // chiave totale
077100150129         WHEN  wrkkey = '1';
077200150129           setll TBEcod TNTBE01L;
077300150129       // chiave parziale
077400150129         WHEN  wrkkey = '2';
077500150129           setll (TBEcod:TBEke1) TNTBE01L;
077600150129         ENDSL;
077700150129
077800150129       ENDSR;
077900150129
078000150129       // ------------------------------------------------------------------------
078100150129       // Scrivo subfile
078200150129       // ------------------------------------------------------------------------
078300150129       BEGSR sr_wtrs01;
078400150129
078500150129       // se non raggiunto limite massimo di caricamento
078600150129         IF  s01nrr < 9999;
078700150129       // imposto campi di subfile
078800150129           exsr sr_sets01;
078900150202
079000150202         // se richiamato con flag di utilizzo da utente 'S'
079100150202         // NON visualizzo i rcd con flag di causale automatica
079200150202           IF  BECMuti = 'S' and �ECMaut <> *blanks;
079300150202             leavesr;
079400150202           ENDIF;
079500150202
079600150129       // imposto numeratore righe e numero relativo di record
079700150129           s01nrr = s01nrr + 1;
079800150129       // scrivo subfile
079900150202           write TBECMS01;
080000150129         ENDIF;
080100150129
080200150129       ENDSR;
080300150129
080400150129       // ------------------------------------------------------------------------
080500150129       // Imposto campi del subfile
080600150129       // ------------------------------------------------------------------------
080700150129       BEGSR sr_sets01;
080800150129
080900150129         s1tbeke1 = TBEke1;
081000150129         s1tbeke2 = TBEke2;
081100150202         dECM = TBEuni;
081200150202         s1tbeuni = �ECMdes;
081300150129         s1tbeatb = TBEatb;
081400150129
081500150129       ENDSR;
081600150129
081700150129       // ------------------------------------------------------------------------
081800150129       // Routine iniziale
081900150129       // ------------------------------------------------------------------------
082000150129         BEGSR sr_datijob;
082100150129
082200150129         in(E) �AzUte;
082300150129         IF  NOT %error;
082400150129           in(E) �DatiUte;
082500150129         ENDIF;
082600150129         IF  %error or RSut = *blanks;
082700150129           clear TIBS34ds;
082800150129           tibs34r(tibs34ds);
082900150129           in �AzUte;
083000150129           in �DatiUte;
083100150129         ENDIF;
083200150129
083300150129         ENDSR;
083400150129
083500150129       // ------------------------------------------------------------------------
083600150129       // Uscita dal programma
083700150129       // ------------------------------------------------------------------------
083800150129         BEGSR sr_uscita;
083900150129
084000150202           IF  BECMesito = *blanks;
084100150202             BECMesito = Eseguito;
084200150129           ENDIF;
084300150129
084400150202           kpjbu = TNTBECMDS;
084500150129
084600150129           *inlr = *on;
084700150129           return;
084800150129
084900150129         ENDSR;
085000150129
085100150129      /end-free
085200150129
085300150129** -MSG-                                                                     *
085400150202Immettere la cuasale                                                           01
085500150129Immettere la descrizione                                                       02
085600150202Causale gi� esistente                                                          03
085700150202Causale errata                                                                 04
