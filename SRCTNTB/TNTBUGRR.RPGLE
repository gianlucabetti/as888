000100150129       //--------------------------------------------------------------
000200150319       //?GESTIONE TABELLA "UGR" - Utente Gestione RA
000300150129       //--------------------------------------------------------------
000400150129     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500150129
000600150129       //--------------------------------------------------------------
000700150129       //?Dichiarazione file.                                          ?
000800150129       //--------------------------------------------------------------
000900150319
001000150319     fAZORG01L  if   e           k disk
001100150129
001200150129     fTNTBE01L  uf a e           k disk
001300150129     fTNTBE11L  uf a e           k disk    rename(TNTBE000:TNTBE11)
001400150129
001500150319     fTNTBUGRD  cf   e             workstn sfile(TBUGRS01:S01nrr)
001600150129
001700150129       //--------------------------------------------------------------
001800150129       //?Definizione costanti.                                        ?
001900150129       //--------------------------------------------------------------
002000150129     d Errore          c                   '1'
002100150129     d Eseguito        c                   '0'
002200150129
002300150129       //--------------------------------------------------------------
002400150129       //?Definizione schiere.                                         ?
002500150129       //--------------------------------------------------------------
002600150129
002700150129       // - Messaggi a video
002800150129     d Msg             s             78    dim(17) ctdata perrcd(1)
002900150129
003000150129       //--------------------------------------------------------------
003100150129       //?Definizione aree dati.                                       ?
003200150129       //--------------------------------------------------------------
003300150129
003400150129       // - Dati utente
003500150129     d §AzUte        e ds                  extname(AZUTE00F)
003600150129     d                                     dtaara
003700150129     d §DatiUte      e ds                  extname(dDatiUte)
003800150129     d                                     dtaara
003900150129
004000150129       //--------------------------------------------------------------
004100150129       //?Definizione strutture dati.                                  ?
004200150129       //--------------------------------------------------------------
004300150129
004400150129       // - Status
004500150129     d Psds           sds
004600150129     d   SDSpgm          *proc
004700150129
004800150129       // - Parametri ricevuti
004900150129     d KPJBA         e ds
005000150319     d TNTBUGRDS     e ds
005100150129
005200150129       // - Reperimento dati utente
005300150129     d TIBS34ds      e ds
005400150129
005500150319       // - Tabella "UGR"
005600150319     d dUGR          e ds
005700150129
005800150129       // - Controllo data
005900150129     d wlbdat          ds                  inz
006000150129     d  G02dat                 1      8  0
006100150129     d  G02inv                 9     16  0
006200150129     d  G02err                17     17
006300150129     d  G02tgi                18     22  0
006400150129
006500150129       //--------------------------------------------------------------
006600150129       //?Definizione procedure usate.                                 ?
006700150129       //--------------------------------------------------------------
006800150129
006900150129       // - Reperimento dati utente
007000150129      /copy gaitrasrc/srcProtoPR,TIBS34R
007100150129
007200150129       // - Controllo data
007300150129     d xsrda8          pr                  extpgm('XSRDA8')
007400150129     d  wlbdat                             likeds(wlbdat)
007500150129
007600150129       //--------------------------------------------------------------
007700150129       //?Definizione variabili globali.                               ?
007800150129       //--------------------------------------------------------------
007900150129
008000150129       // - Campi di comodo
008100150129     d blanks          s                   like(d1descopz)
008200150129     d comando         s              1
008300150129     d kTBEke1         s                   like(TBEke1)
008400150129     d len             s              5u 0
008500150129     d s01nrr          s              4  0
008600150129     d savopzione      s                   like(s1opzione)
008700150129     d savrcdnbr       s                   like(c1rcdnbr)
008800150129     d wrkkey          s              1
008900150129     d wrkeofs01       s              1
009000150129     d wrkcars01       s              1
009100150129     d wrkcarw01       s              1
009200150129     d $video          s             10
009300060428
009400150129
009500150129       //--------------------------------------------------------------
009600150129       //?Riepilogo indicatori.                                        ?
009700150129       //--------------------------------------------------------------
009800150129       // 01 - ON ricerca -- OFF manutenzione
009900150129       // 02 - proteggo campo fase
010000150202       // 03 - proteggo campi video
010100150129       // 04 - visualizzazione
010200150202       // 20 - gestione subfile
010300150202       // 21 - gestione subfile
010400150202       // 22 - gestione subfile
010500150129       // 28 - messaggio errore
010600150129       // 40 - fase
010700150129       // 41 - descrizione
010800150319       // 42 - filiale gestione
010900150129       //--------------------------------------------------------------
011000150129
011100150129       //--------------------------------------------------------------
011200150129
011300150129     c     *Entry        plist
011400150129     c                   parm                    kpjba
011500150129
011600150129      /free
011700150129
011800150129       exsr sr_datijob;
011900150129       exsr sr_parm;
012000150129       exsr sr_c01;
012100150129       exsr sr_uscita;
012200150129
012300150129       // ------------------------------------------------------------------------
012400150129       // Elaborazione parametri ricevuti
012500150129       // ------------------------------------------------------------------------
012600150129       BEGSR sr_parm;
012700150129
012800150319         TNTBUGRDS = kpjbu;
012900150129
013000150129         SELECT;
013100150129       // ricerca e scleta
013200150319         WHEN  BUGRfun = '1';
013300150129           *in01 = *on;
013400150129       // manutenzione
013500150319         WHEN  BUGRfun = *blanks;
013600150129           *in01 = *off;
013700150129       // altrimenti
013800150129         OTHER;
013900150319           BUGREsito = Errore;
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
017800150319           write TBUGRP01;
017900150319           exfmt TBUGRC01;
018000150129
018100150129       // controllo tasti funzionali del subfile
018200150129           SELECT;
018300150129
018400150129       // F3=Fine
018500150129           WHEN  *inkc;
018600150129             $video = *blanks;
018700150319             BUGRricez = 'C';
018800150129             exsr sr_uscita;
018900150129
019000150129       // F5=Refresh
019100150129           WHEN  *inke;
019200150129             wrkcars01 = *on;
019300150129
019400150129       // F10=Immissione
019500150129           WHEN  *inkj;
019600150129             $video = 'D01';
019700150319             clear TNTBUGRDS;
019800150319             BUGRcomand = 'J';
019900150129             exsr sr_d01;
020000150129
020100150129       // F13=Ripetizione
020200150129           WHEN  *inkm;
020300150129             exsr sr_ripetiopz;
020400150129
020500150129       // in tutti gli altri casi
020600150129           OTHER;
020700150129       // controllo ed elaborazione scelte su subfile
020800150129             exsr sr_gestsfl;
020900150129           ENDSL;
021000150129
021100150129       // fine elaborazione 'C01'
021200150129         ENDDO;
021300150129
021400150129       ENDSR;
021500150129
021600150129       // ------------------------------------------------------------------------
021700150129       // Caricamento subfile
021800150129       // ------------------------------------------------------------------------
021900150129       BEGSR sr_cars01;
022000150129
022100150129       // se è stato richiesto il caricamento del subfile
022200150129         IF  wrkcars01 = *on;
022300150129
022400150129       // inizializzo il subfile
022500150129           s01nrr = 0;
022600150129           *in20 = *on;
022700150319           write TBUGRC01;
022800150129           *in20 = *off;
022900150129           *in21 = *off;
023000150129           *in22 = *off;
023100150129           *in23 = *off;
023200150129
023300150129       // imposto la chiave di posizionamento e lettura file
023400150319           TBEcod = 'UGR';
023500150129           TBEke1 = c1setll;
023600150129
023700150129       // se è stato scelto il posizionamento
023800150129           IF  c1setll <> *blanks;
023900150129             wrkkey = '2';
024000150129       // altrimenti
024100150129           ELSE;
024200150129             wrkkey = '1';
024300150129           ENDIF;
024400150129
024500150129       // posizionamento file
024600150129           exsr sr_setlltbe01;
024700150129
024800150129       // fino a che non è fine file
024900150129           DOU  %eof(TNTBE01L);
025000150129
025100150129         // leggo file
025200150129             reade(n) TBEcod TNTBE01L;
025300150129
025400150129         // fine file esco
025500150129             IF  %eof(TNTBE01L);
025600150129               leave;
025700150129             ENDIF;
025800150319
025900150319             dUGR = TBEuni;
026000150129
026100150129         // se in "ricerca/scelta" non carico i records annullati
026200150319         // e non carico quelli con §UGRint = 'S'
026300150319             IF  (*in01 and TBEatb = *blanks and §UGRint = 'S') or not *in01;
026400150129         // scrivo subfile
026500150129               clear s1opzione;
026600150129               exsr sr_wtrs01;
026700150129             ENDIF;
026800150129
026900150129           ENDDO;
027000150129
027100150129       // fine caricamento subfile
027200150129         ENDIF;
027300150129
027400150129       // se scritto almeno un record
027500150129         IF  s01nrr > 0;
027600150129       // indicatore di sflend
027700150129           *in21 = *on;
027800150129         ENDIF;
027900150129
028000150129       // se non scritto neanche un record
028100150129         IF  s01nrr = 0;
028200150129       // indicatore di sfldsp
028300150129           *in23 = *on;
028400150129         ENDIF;
028500150129
028600150129       // disattivo opzione di caricamento subfile
028700150129         wrkcars01 = *off;
028800150129
028900150129       ENDSR;
029000150129
029100150129       // ------------------------------------------------------------------------
029200150129       // Gestione videata
029300150129       // ------------------------------------------------------------------------
029400150129       BEGSR sr_d01;
029500150129
029600150129       // imposto il video a seconda della funzione richiesta
029700150129         exsr sr_setvideo;
029800150129
029900150129       // imposto variabile
030000150129         $video = 'D01';
030100150129
030200150129       // fino a che la variabile resta settata come 'D01'
030300150129         DOU $video <> 'D01';
030400150129
030500150129           *in02 = *off;
030600150129           *in03 = *off;
030700150129           *in04 = *off;
030800150129
030900150129       // se immessa opzione di 'scelta'
031000150319           IF  BUGRopzio = '01';
031100150319             BUGRke1 = s1TBEke1;
031200150319             BUGRdes = s1TBEuni;
031300150129             exsr sr_uscita;
031400150129
031500150129         // se immessa un'altra opzione
031600150129           ELSE;
031700150129         // se non è immissione/copia proteggo il campo della causale
031800150319             IF  BUGRcomand <> 'J' and BUGRopzio <> '03';
031900150129               *in02 = *on;
032000150129             ENDIF;
032100150129          // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
032200150129          // proteggo i campi del video
032300150319             IF  BUGRopzio = '04' or BUGRopzio = '05';
032400150129               *in03 = *on;
032500150129             ENDIF;
032600150129          // se immessa opzione di 'visualizzazione'
032700150129          // non attivo F6
032800150319             IF  BUGRopzio = '05';
032900150129               *in04 = *on;
033000150129             ENDIF;
033100150129          // emetto il video
033200150319             exfmt TBUGRD01;
033300150129           ENDIF;
033400150129
033500150129         // reset indicatore generico di errore
033600150129           *in28 = *off;
033700150129
033800150129         // pulisco il campo messaggi
033900150129           clear vd1msg;
034000150129
034100150129         // controllo tasti funzionali del video
034200150129           SELECT;
034300150129
034400150129         // F3=Fine
034500150129           WHEN  *inkc;
034600150319             BUGRricez = 'C';
034700150129             $video = *blanks;
034800150129             unlock TNTBE01L;
034900150129             exsr sr_uscita;
035000150129
035100150129         // F6=Conferma
035200150129           WHEN  *inkf;
035300150319             BUGRricez = 'F';
035400150129         // controllo e decodifico i dati del video
035500150129             exsr sr_ctrd01;
035600150129         // conferma per annullo/ripristino
035700150319             IF  BUGRopzio = '04';
035800150129               SELECT;
035900150129           // annullamento
036000150129               WHEN  TBEatb = *blanks;
036100150129                 TBEatb = 'A';
036200150129           // ripristino
036300150129               WHEN  TBEatb = 'A';
036400150129                 clear TBEatb;
036500150129               ENDSL;
036600150129             ENDIF;
036700150129
036800150129         // se non riscontrati errori emetto la finestra con i dati per la trasmissione
036900150129             IF  not *in28;
037000150129               wrkcarw01 = *on;
037100150129               $video = 'W01';
037200150129               exsr sr_w01;
037300150129             ENDIF;
037400150129
037500150129         // F8=Record successivo
037600150129           WHEN  *inkh;
037700150129             clear s1opzione;
037800150129             wrkcars01 = *off;
037900150129             $video = 'C01';
038000150319             BUGRricez = 'H';
038100150129
038200150129         // F12=Ritorno
038300150129           WHEN  *inkl;
038400150129             clear s1opzione;
038500150319             BUGRricez = 'L';
038600150129             unlock TNTBE01L;
038700150129         // se non è f12 da immissione/copia non ricarico il subfile
038800150319             IF  BUGRcomand = 'J' or BUGRopzio = '03';
038900150129               wrkcars01 = *on;
039000150129             ELSE;
039100150129               wrkcars01 = *off;
039200150129             ENDIF;
039300150129             $video = 'C01';
039400150129
039500150129         // Invio
039600150129           OTHER;
039700150129             IF  not *in03;
039800150129               exsr sr_ctrd01;
039900150129             ENDIF;
040000150129
040100150129           ENDSL;
040200150129
040300150129       // fine gestione 'D01'
040400150129         ENDDO;
040500150129
040600150129       ENDSR;
040700150129
040800150129       // ------------------------------------------------------------------------
040900150129       // Ripeto opzione in tutte le righe del subfile
041000150129       // ------------------------------------------------------------------------
041100150129       BEGSR sr_ripetiopz;
041200150129
041300150129         IF  c1csrrrn > 0;
041400150129           s01nrr = c1csrrrn;
041500150319           chain s01nrr TBUGRS01;
041600150129           IF  %found and s1opzione > 0;
041700150129             savopzione = s1opzione;
041800150129             *in22 = *on;
041900150319             update TBUGRS01;
042000150129
042100150129             wrkeofs01 = *off;
042200150129             DOU  wrkeofs01 = *on;
042300150129               s01nrr = s01nrr + 1;
042400150319               chain s01nrr TBUGRS01;
042500150129               IF  %found;
042600150129                 s1opzione = savopzione;
042700150319                 update TBUGRS01;
042800150129               ELSE;
042900150129                 wrkeofs01 = *on;
043000150129               ENDIF;
043100150129             ENDDO;
043200150129
043300150129             *in22 = *off;
043400150129
043500150129           ENDIF;
043600150129
043700150129         ENDIF;
043800150129
043900150129       ENDSR;
044000150129
044100150129       // ------------------------------------------------------------------------
044200150129       // Imposto i dati a video
044300150129       // ------------------------------------------------------------------------
044400150129       BEGSR sr_setvideo;
044500150129
044600150129       // pulisco il formato video D01
044700150129         exsr sr_puld01;
044800150129
044900150129       // controllo se 'immissione' 'modifica' 'copoa' o altro
045000150129         SELECT;
045100150129
045200150129       // F10=Immissione
045300150319         WHEN  BUGRcomand = 'J';
045400150129           d1descopz = 'Immissione';
045500150129
045600150129       // Opzione "02"=modifica
045700150319         WHEN  BUGRopzio = '02';
045800150129           d1descopz = 'Modifica';
045900150129           exsr sr_imposta;
046000150129
046100150129       // Opzione "03"=copia
046200150319         WHEN  BUGRopzio = '03';
046300150129           d1descopz = 'Copia';
046400150129           exsr sr_imposta;
046500150129
046600150129       // Opzione "04"=cancellazione/ripristino
046700150319         WHEN  BUGRopzio = '04';
046800150129           exsr sr_imposta;
046900150129        // se richiesta 'cancellazione'
047000150129           IF  TBEatb = *blanks;
047100150129             d1descopz = 'Annullamento';
047200150129           ENDIF;
047300150129        // se richiesto 'ripristino'
047400150129           IF TBEatb = 'A';
047500150129             d1descopz = 'Ripristino';
047600150129           ENDIF;
047700150129
047800150129       // Opzione "05"=visualizzazione
047900150319         WHEN  BUGROpzio ='05';
048000150129           d1descopz = 'Visualizzazione';
048100150129           exsr sr_imposta;
048200150129
048300150129       // Fine scelta
048400150129         ENDSL;
048500150129
048600150129       // centro la descrizione della funzione nel formato video
048700150129         len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
048800150129         d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
048900150129
049000150129       ENDSR;
049100150129
049200150129       // ------------------------------------------------------------------------
049300150129       // controllo video
049400150129       // ------------------------------------------------------------------------
049500150129       BEGSR sr_ctrd01;
049600150129
049700150129         *in28 = *off;
049800150129         *in40 = *off;
049900150319         *in41 = *off;
050000150129
050100150319       // Chiave
050200150129         IF  d1tbeke1 = *blanks;
050300150129           vd1msg = Msg(01);
050400150129           *in28 = *on;
050500150129           *in40 = *on;
050600150129           leavesr;
050700150129         ENDIF;
050800150129
050900150129       // se immissione controllo che non esista già
051000150319         IF  BUGRcomand = 'J' or BUGRopzio = '03';
051100150319           TBEcod = 'UGR';
051200150129           TBEke1 = d1tbeke1;
051300150129           TBEke2 = d1tbeke2;
051400150129           clear TBElin;
051500150129           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
051600150129           IF  %found(TNTBE01L);
051700150129             vd1msg = Msg(03);
051800150129             *in28 = *on;
051900150129             *in40 = *on;
052000150129             leavesr;
052100150129           ENDIF;
052200150129         ENDIF;
052300150129
052400150129       // descrizione
052500150319         IF  d1UGRdes = *blanks;
052600150129           vd1msg = Msg(02);
052700150129           *in28 = *on;
052800150129           *in41 = *on;
052900150129           leavesr;
053000150129         ENDIF;
053100150319
053200150319       // Filiale gestione
053300150319         IF  d1UGRpofo > *zeros;
053400150319           chain d1UGRpofo AZORG01L;
053500150319           IF  not %found(AZORG01L) or ORGfva <> *blanks;
053600150319             vd1msg = Msg(04);
053700150319             *in28 = *on;
053800150319             *in42 = *on;
053900150319             leavesr;
054000150319           ENDIF;
054100150319           D1Dpog = ORGdes;
054200150319         ENDIF;
054300150129
054400150129       ENDSR;
054500150129
054600150129       // ------------------------------------------------------------------------
054700150129       // Gestione video dati di trasmissione
054800150129       // ------------------------------------------------------------------------
054900150129       BEGSR sr_w01;
055000150129
055100150129       // imposto i dati a video
055200150129         exsr sr_carw01;
055300150129
055400150129       // fino a che la variabile resta settata come 'W01'
055500150129         DOU  $video <> 'W01';
055600150129
055700150129       // reset indicatore generico di errore
055800150129           *in28 = *off;
055900150129
056000150129       // emetto il video
056100150319           exfmt TBUGRW01;
056200150129
056300150129       // controllo tasti funzionali del video
056400150129           SELECT;
056500150129
056600150129       // F6=Conferma
056700150129           WHEN  *inkf;
056800150129         // controllo i dati del video
056900150129             exsr sr_ctrw01;
057000150129         // se non riscontrati errori aggiorno il record corrente
057100150129             IF  not *in28;
057200150129               exsr sr_aggiorna;
057300150202               IF  not *in28;
057400150319                 IF  BUGRcomand = 'J';
057500150202                   $video = 'D01';
057600150202                   exsr sr_setvideo;
057700150202                 ELSE;
057800150202                   $video = 'C01';
057900150202                 ENDIF;
058000150202               ENDIF;
058100150129             ENDIF;
058200150129
058300150129       // F12=Ritorno
058400150129           WHEN  *inkl;
058500150129             $video = 'D01';
058600150319             clear BUGRricez;
058700150129
058800150129       // Invio
058900150129           OTHER;
059000150129             exsr sr_ctrw01;
059100150129
059200150129           ENDSL;
059300150129
059400150129       // fine gestione 'W01'
059500150129         ENDDO;
059600150129
059700150129       ENDSR;
059800150129
059900150129       // ------------------------------------------------------------------------
060000150129       // Imposto i dati di trasmissione
060100150129       // ------------------------------------------------------------------------
060200150129       BEGSR sr_carw01;
060300150129
060400150129       // se pulisco i campi
060500150129         clear w1ftt;
060600150129         clear w1flt;
060700150129         clear w1ftr;
060800150129         clear w1dtr;
060900150129
061000150129       // se non immissione imposto i campi del file
061100150319         IF  BUGRcomand <> 'J';
061200150129           w1ftt = TBEftt;
061300150129           w1flt = TBEflt;
061400150129           w1ftr = TBEftr;
061500150129       // imposto la data
061600150129           IF  TBEdtr <> *zeros;
061700150129             clear wlbdat;
061800150129             G02inv = TBEdtr;
061900150129             G02err = '3';
062000150129             xsrda8(wlbdat);
062100150129             w1dtr = G02dat;
062200150129           ENDIF;
062300150129
062400150129         ENDIF;
062500150129
062600150129       ENDSR;
062700150129
062800150129       // ------------------------------------------------------------------------
062900150129       // Controllo i dati di trasmissione
063000150129       // ------------------------------------------------------------------------
063100150129       BEGSR sr_ctrw01;
063200150129
063300150129       ENDSR;
063400150129
063500150129       // ------------------------------------------------------------------------
063600150129       // Aggiorno tabella
063700150129       // ------------------------------------------------------------------------
063800150129       BEGSR sr_aggiorna;
063900150129
064000150129       // imposto campi del file
064100150129         clear TBElin;
064200150129         TBEke1 = d1tbeke1;
064300150129         TBEke2 = d1tbeke2;
064400150129
064500150319         §UGRdesc = d1UGRdes;
064600150319         §UGRpref = d1UGRpref;
064700150319         §UGRprof = d1UGRprof;
064800150319         §UGRpofo = d1UGRpofo;
064900150319         §UGRint  = d1UGRint;
065000150129
065100150319         TBEuni = dUGR;
065200150129
065300150129         TBEftt = w1ftt;
065400150129         TBEflt = w1flt;
065500150129         clear TBEftr;
065600150129         clear TBEdtr;
065700150129
065800150129       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
065900150129         SELECT;
066000150129
066100150129       // F10=immissione - Opzione "03"=copia
066200150319         WHEN  BUGRcomand = 'J' or BUGRopzio = '03';
066300150129       // scrivo nuovo record con gestione errore per chiave duplicata
066400150129           write TNTBE000;
066500150129
066600150129       // Opzione "02"=modifica
066700150319         WHEN  BUGRopzio = '02';
066800150129       // update record e controllo errore per chiave duplicata
066900150129           update TNTBE000;
067000150129
067100150129       // Opzione "04"=cancellazione/ripristino
067200150319         WHEN  BUGRopzio = '04';
067300150129           update TNTBE000;
067400150129
067500150129         ENDSL;
067600150129
067700150129       ENDSR;
067800150129
067900150129       // ------------------------------------------------------------------------
068000150129       // pulizia video
068100150129       // ------------------------------------------------------------------------
068200150129       BEGSR sr_puld01;
068300150129
068400150129         clear d1descopz;
068500150129         clear d1tbeke1;
068600150129         clear d1tbeke2;
068700150319         clear d1UGRdes;
068800150319         clear d1UGRpref;
068900150319         clear d1UGRprof;
069000150319         clear d1UGRpofo;
069100150319         clear d1dpog;
069200150319         clear d1UGRint;
069300150129
069400150129         *in28 = *off;
069500150129         *in40 = *off;
069600150129         *in41 = *off;
069700150319         *in42 = *off;
069800150129
069900150129       ENDSR;
070000150129
070100150129       // ------------------------------------------------------------------------
070200150129       // Imposto i dati a video
070300150129       // ------------------------------------------------------------------------
070400150129       BEGSR sr_imposta;
070500150129
070600150129       // recupero i dati dal file
070700150129         kTBEke1 = s1tbeke1;
070800150129         clear TBElin;
070900150129         chain (TBEcod:kTBEke1:s1tbeke2:TBElin) TNTBE01L;
071000150129
071100150129       // imposto i campi a video
071200150129         d1tbeke2 = TBEke2;
071300150129         d1tbeke1 = TBEke1;
071400150319         dUGR = TBEuni;
071500150319         d1UGRdes  = §UGRdesc;
071600150319         d1UGRpref = §UGRpref;
071700150319         d1UGRprof = §UGRprof;
071800150319         d1UGRpofo = §UGRpofo;
071900150319         d1UGRint  = §UGRint;
072000150319
072100150319         IF  d1UGRpofo > *zeros;
072200150319           chain d1UGRpofo AZORG01L;
072300150319           IF  %found(AZORG01L);
072400150319             D1Dpog = ORGdes;
072500150319           ENDIF;
072600150319         ENDIF;
072700150129
072800150129       ENDSR;
072900150129
073000150129       // ------------------------------------------------------------------------
073100150129       // Gestione del subfile
073200150129       // ------------------------------------------------------------------------
073300150129       BEGSR sr_gestsfl;
073400150129
073500150129       // set flag
073600150129         wrkeofs01 = *off;
073700150129
073800150129       // inizio lettura subfile
073900150129         DOW  wrkeofs01 = *off and *in21;
074000150319           readc TBUGRS01;
074100150129       // se fine subfile
074200150129           IF  %eof;
074300150129             wrkcars01 = *on;
074400150129             leave;
074500150129           ENDIF;
074600150129       // se è stata immessa un'opzione
074700150129           IF  s1opzione <> *zeros;
074800150129         // reset ds di servizio
074900150319             clear TNTBUGRDS;
075000150129         // controllo ed elaborazione opzione immessa
075100150129             SELECT;
075200150129         // opzione "01"=scelta
075300150129             WHEN  s1opzione = 1 and *in01;
075400150319               BUGRopzio = '01';
075500150129         // opzione "02"=modifica
075600150129             WHEN  s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
075700150319               BUGRopzio = '02';
075800150129         // opzione "03"=copia
075900150129             WHEN  s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
076000150319               BUGRopzio = '03';
076100150129         // opzione "04"=annullo/ripristino
076200150129             WHEN s1opzione = 4 and not *in01;
076300150319               BUGRopzio = '04';
076400150129         // opzione "05"=visualizzazione
076500150129             WHEN s1opzione = 5;
076600150319               BUGRopzio = '05';
076700150129             ENDSL;
076800150129
076900150129         // se immessa almeno un opzione valida
077000150319             IF  BUGRopzio <> *blanks;
077100150129         // gstione del formato video
077200150129               exsr sr_d01;
077300150129         // se la gestione si è chiusa con ...
077400150129               SELECT;
077500150129           // F6=Conferma
077600150319               WHEN  BUGRricez = 'F';
077700150129                 clear s1opzione;
077800150129                 wrkcars01 = *on;
077900150129          // F12=Ritorno
078000150319               WHEN  BUGRricez = 'L';
078100150129                 clear s1opzione;
078200150129                 wrkeofs01 = *on;
078300150129          // altrimenti
078400150129               OTHER;
078500150129                 *in22 = *on;
078600150129               ENDSL;
078700150129
078800150319               update TBUGRS01;
078900150129               *in22 = *off;
079000150129         // fine opzione
079100150129             ENDIF;
079200150129       // fine opzione
079300150129           ENDIF;
079400150129
079500150129         ENDDO;
079600150129
079700150129       ENDSR;
079800150129
079900150129       // ------------------------------------------------------------------------
080000150129       // Posizionamento sul file
080100150129       // ------------------------------------------------------------------------
080200150129       BEGSR sr_setlltbe01;
080300150129
080400150129         SELECT;
080500150129       // chiave totale
080600150129         WHEN  wrkkey = '1';
080700150129           setll TBEcod TNTBE01L;
080800150129       // chiave parziale
080900150129         WHEN  wrkkey = '2';
081000150129           setll (TBEcod:TBEke1) TNTBE01L;
081100150129         ENDSL;
081200150129
081300150129       ENDSR;
081400150129
081500150129       // ------------------------------------------------------------------------
081600150129       // Scrivo subfile
081700150129       // ------------------------------------------------------------------------
081800150129       BEGSR sr_wtrs01;
081900150129
082000150129       // se non raggiunto limite massimo di caricamento
082100150129         IF  s01nrr < 9999;
082200150129       // imposto campi di subfile
082300150129           exsr sr_sets01;
082400150129       // imposto numeratore righe e numero relativo di record
082500150129           s01nrr = s01nrr + 1;
082600150129       // scrivo subfile
082700150319           write TBUGRS01;
082800150129         ENDIF;
082900150129
083000150129       ENDSR;
083100150129
083200150129       // ------------------------------------------------------------------------
083300150129       // Imposto campi del subfile
083400150129       // ------------------------------------------------------------------------
083500150129       BEGSR sr_sets01;
083600150129
083700150129         s1tbeke1 = TBEke1;
083800150129         s1tbeke2 = TBEke2;
083900150319         s1tbeuni = §UGRdesc;
084000150129         s1tbeatb = TBEatb;
084100150129
084200150129       ENDSR;
084300150129
084400150129       // ------------------------------------------------------------------------
084500150129       // Routine iniziale
084600150129       // ------------------------------------------------------------------------
084700150129         BEGSR sr_datijob;
084800150129
084900150129         in(E) §AzUte;
085000150129         IF  NOT %error;
085100150129           in(E) §DatiUte;
085200150129         ENDIF;
085300150129         IF  %error or RSut = *blanks;
085400150129           clear TIBS34ds;
085500150129           tibs34r(tibs34ds);
085600150129           in §AzUte;
085700150129           in §DatiUte;
085800150129         ENDIF;
085900150129
086000150129         ENDSR;
086100150129
086200150129       // ------------------------------------------------------------------------
086300150129       // Uscita dal programma
086400150129       // ------------------------------------------------------------------------
086500150129         BEGSR sr_uscita;
086600150129
086700150319           IF  BUGResito = *blanks;
086800150319             BUGResito = Eseguito;
086900150129           ENDIF;
087000150129
087100150319           kpjbu = TNTBUGRDS;
087200150129
087300150129           *inlr = *on;
087400150129           return;
087500150129
087600150129         ENDSR;
087700150129
087800150129      /end-free
087900150129
088000150129** -MSG-                                                                     *
088100150319Immettere il profilo                                                           01
088200150129Immettere la descrizione                                                       02
088300150319Profilo già esistente                                                          03
088400150319Filiale gestione errata                                                        04
