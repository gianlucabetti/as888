000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300060509      * Gestione tabella "FAR" = Fasi ORM                                      *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600060508     ftntbe11l  uf a e           k disk    rename(tntbe000:tntbe11)
000700060502     fazute01l  if   e           k disk
000800060505     fazlin02l  if   e           k disk
000900060509     ftntb56d   cf   e             workstn sfile(tb56s01:s01nrr)
001000060509     f                                     sfile(tb56s02:s02nrr)
001100060428
001200060428      *------------------------------------------------------------------------*
001300060428      * riepilogo indicatori
001400060428      *------------------------------------------------------------------------*
001500060428      * 01 - ON ricerca -- OFF manutenzione
001600060509      * 02 - proteggo campo fase
001700060508      * 03 - proteggo campi video 01
001800060509      * 04 - visualizzazione
001900060508      * 05 - visualizzo subfile lingue
002000060508      * 20 - gestione subfile 01
002100060508      * 21 - gestione subfile 01
002200060508      * 22 - gestione subfile 01
002300060508      * 23 - gestione subfile 01
002400060508      * 24 - gestione subfile 02
002500060508      * 25 - gestione subfile 02
002600060428      * 28 - messaggio errore
002700060509      * 40 - fase
002800060502      * 41 - descrizione
002900060509      * 42 - descrizione subfile
003000060502
003100060502      *------------------------------------------------------------------------*
003200060502      * schiere
003300060502      *------------------------------------------------------------------------*
003400060502     d msg             s             78    dim(17) ctdata perrcd(1)
003500060427
003600060428      *------------------------------------------------------------------------*
003700060428      * campi di work
003800060428      *------------------------------------------------------------------------*
003900060428     d blanks          s                   like(d1descopz)
004000060502     d comando         s              1
004100060505     d ktbeke1         s                   like(tbeke1)
004200060428     d len             s              5u 0
004300060505     d s01nrr          s              4  0
004400060505     d s02nrr          s              4  0
004500060428     d savopzione      s                   like(s1opzione)
004600060428     d savrcdnbr       s                   like(c1rcdnbr)
004700060505     d savrec          s                   like(s02nrr)
004800060531     d savtbeftt       s                   like(tbeftt)
004900060531     d savtbeflt       s                   like(tbeflt)
005000060505     d savtbeuni       s                   like(tbeuni)
005100060428     d wrkkey          s              1
005200060428     d wrkeofs01       s              1
005300060428     d wrkcars01       s              1
005400060505     d wrkcarw01       s              1
005500060505     d wrksfl02        s              1
005600060504     d $video          s             10
005700060428
005800060428      *------------------------------------------------------------------------*
005900060428      * ds interne/esterne
006000060428      *------------------------------------------------------------------------*
006100060504     d wlbdat          ds                  inz
006200060504     d  g02dat                 1      8  0
006300060504     d  g02inv                 9     16  0
006400060504     d  g02err                17     17
006500060504     d  g02tgi                18     22  0
006600060428
006700060509     d dfar          e ds
006800060502     d kpjba         e ds
006900060503     d tibs34ds      e ds                  inz
007000060509     d tntb56ds      e ds
007100060428     d §azute        e ds                  extname(azute00f)
007200060428     d                                     dtaara
007300060428     d §datiute      e ds                  extname(ddatiute)
007400060428     d                                     dtaara
007500010503
007600060428     d psds           sds
007700060428     d  pgmname          *proc
007800060428
007900060428      *------------------------------------------------------------------------*
008000060428      * pgm richiamati
008100060428      *------------------------------------------------------------------------*
008200060428     d tibs34r         pr                  extpgm('TIBS34R')
008300060502     d  tibs34ds                           likeds(tibs34ds)
008400060504
008500060504     d xsrda8          pr                  extpgm('XSRDA8')
008600060504     d  wlbdat                             likeds(wlbdat)
008700060428
008800060428      *------------------------------------------------------------------------*
008900060428      * costanti
009000060428      *------------------------------------------------------------------------*
009100060428     d errore          c                   '1'
009200060428     d eseguito        c                   '0'
009300010503
009400060428      *------------------------------------------------------------------------*
009500010423
009600060428      /free
009700060428
009800060428       exsr sr_parm;
009900060428       exsr sr_c01;
010000060428       exsr sr_uscita;
010100010423
010200060502       // ------------------------------------------------------------------------
010300060502       // elaborazione parametri ricevuti
010400060502       // ------------------------------------------------------------------------
010500060428       begsr sr_parm;
010600060428
010700060509        tntb56dS = kpjbu;
010800060428
010900060428        select;
011000060502       // ricerca e scleta
011100060509         when b56Fun = '1';
011200060428          *in01 = *on;
011300060502       // manutenzione
011400060509         when  b56Fun = *blanks;
011500060428          *in01 = *off;
011600060502       // altrimenti
011700060428         other;
011800060509          b56Esito = errore;
011900060428           exsr sr_uscita;
012000060428         endsl;
012100060428
012200060428       endsr;
012300010430
012400060502       // ------------------------------------------------------------------------
012500060502       // gestione subfile
012600060502       // ------------------------------------------------------------------------
012700060428       begsr sr_c01;
012800060428
012900060502       // imposto variabili
013000060428        wrkcars01 = *on;
013100060428        $video = 'C01';
013200060428
013300060502       // inizio elaborazione subfile
013400060428         dou  $video <> 'C01';
013500060428
013600060502       // caricamento subfile
013700060428          exsr sr_cars01;
013800060428
013900060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
014000060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
014100060502       // l'ultimo tasto
014200060428          if c1csrrrn > 0;
014300060428           c1rcdnbr = c1csrrrn;
014400060428          else;
014500060428           c1rcdnbr = savrcdnbr;
014600060428          endif;
014700060428
014800060502       // se non so quale pagina visualizzare forzo pagina 1
014900060428          if c1rcdnbr < 1;
015000060428           eval c1rcdnbr = 1;
015100060428          endif;
015200060428
015300060502       // salvo il record number del subfile
015400060509          savrcdnbr = c1rcdnbr;
015500060428
015600060502       // Emissione del subfile
015700060509          write tb56p01;
015800060509          exfmt tb56c01;
015900060428
016000060502       // controllo tasti funzionali del subfile
016100060428          select;
016200060428
016300060502       // F3=Fine
016400060428           when *inkc;
016500060502            $video = *blanks;
016600060509            b56ricez = 'C';
016700060428            exsr sr_uscita;
016800060428
016900060502       // F5=Refresh
017000060428           when *inke;
017100060428            wrkcars01 = *on;
017200060428
017300060502       // F10=Immissione
017400060428           when *inkj;
017500060502            $video = 'D01';
017600060509            clear tntb56ds;
017700060509            b56comand = 'J';
017800060428            exsr sr_d01;
017900060428
018000060502       // F13=Ripetizione
018100060428           when *inkm;
018200060428            exsr sr_ripetiopz;
018300060428
018400060502       // in tutti gli altri casi
018500060428           other;
018600060502       // controllo ed elaborazione scelte su subfile
018700060428            exsr sr_gestsfl;
018800060428          endsl;
018900060428
019000060502       // fine elaborazione 'C01'
019100060428         enddo;
019200060428
019300060428        endsr;
019400060428
019500060502       // ------------------------------------------------------------------------
019600060502       // caricamento subfile
019700060502       // ------------------------------------------------------------------------
019800060428       begsr sr_cars01;
019900010430
020000060502       // se è stato richiesto il caricamento del subfile
020100060428        if wrkcars01 = *on;
020200060428
020300060502       // inizializzo il subfile
020400060505         s01nrr = 0;
020500060428         *in20 = *on;
020600060509         write tb56c01;
020700060428         *in20 = *off;
020800060428         *in21 = *off;
020900060428         *in22 = *off;
021000060428         *in23 = *off;
021100060428
021200060502       // imposto la chiave di posizionamento e lettura file
021300060509         tbecod = 'FAR';
021400060428         tbeke1 = c1setll;
021500060428
021600060502       // se è stato scelto il posizionamento
021700060502         if c1setll <> *blanks;
021800060428          wrkkey = '2';
021900060502       // altrimenti
022000060428         else;
022100060428          wrkkey = '1';
022200060428         endif;
022300060428
022400060502       // posizionamento file
022500060428         exsr sr_setlltbe01;
022600060428
022700060502       // fino a che non è fine file
022800060428         dou %eof(tntbe01l);
022900060428
023000060502       // leggo file
023100060505          reade(n) tbecod tntbe01l;
023200060428
023300060502       // fine file esco
023400060428          if %eof(tntbe01l);
023500060428           leave;
023600060428          endif;
023700060428
023800060502       // se in "ricerca/scelta" non carico i records annullati
023900060502          if (*in01 and tbeatb = *blanks) or not *in01;
024000060502       // scrivo subfile
024100060428           clear s1opzione;
024200060428           exsr sr_wtrs01;
024300060428          endif;
024400060428
024500060428         enddo;
024600060428
024700060502       // fine caricamento subfile
024800060428        endif;
024900060428
025000060502       // se scritto almeno un record
025100060505        if s01nrr > 0;
025200060502       // indicatore di sflend
025300060428         eval *in21 = *on;
025400060428        endif;
025500060428
025600060502       // se non scritto neanche un record
025700060505        if s01nrr = 0;
025800060502       // indicatore di sfldsp
025900060428         eval *in23 = *on;
026000060428        endif;
026100060428
026200060502       // disattivo opzione di caricamento subfile
026300060428        wrkcars01 = *off;
026400060428
026500060428       endsr;
026600060428
026700060502       // ------------------------------------------------------------------------
026800060502       // gestione videata
026900060502       // ------------------------------------------------------------------------
027000060428       begsr sr_d01;
027100060428
027200060504       // imposto il video a seconda della funzione richiesta
027300060428        exsr sr_setvideo;
027400060428
027500060502       // imposto variabile
027600060428        $video = 'D01';
027700060428
027800060502       // fino a che la variabile resta settata come 'D01'
027900060428        dou $video <> 'D01';
028000060428
028100060508         *in02 = *off;
028200060508         *in03 = *off;
028300060508         *in04 = *off;
028400060428
028500060502       // se immessa opzione di 'scelta'
028600060509         if b56opzio = '01';
028700060509          b56ke1 = s1tbeke1;
028800060509          b56des = s1tbeuni;
028900060428          exsr sr_uscita;
029000060428
029100060508       // se immessa un'altra opzione
029200060508         else;
029300060508       // se non è immissione/copia proteggo il campo della causale
029400060509          if b56comand <> 'J' and b56opzio <> '03';
029500060428           *in02 = *on;
029600060428          endif;
029700060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
029800060508       // proteggo i campi del video
029900060509          if b56opzio = '04' or b56opzio = '05';
030000060508           *in03 = *on;
030100060508          endif;
030200060509       // se immessa opzione di 'visualizzazione'
030300060509       // non attivo F6
030400060509          if b56opzio = '05';
030500060508           *in04 = *on;
030600060508          endif;
030700060502       // emetto il video
030800060509          exfmt tb56d01;
030900060428         endif;
031000060428
031100060502       // reset indicatore generico di errore
031200060428         *in28 = *off;
031300060508
031400060508       // pulisco il campo messaggi
031500060508         clear vd1msg;
031600010430
031700060502       // controllo tasti funzionali del video
031800060428         select;
031900060428
032000060502       // F3=Fine
032100060428          when *inkc;
032200060509           b56ricez = 'C';
032300060502           $video = *blanks;
032400060505           unlock tntbe01l;
032500060428           exsr sr_uscita;
032600060428
032700060502       // F6=Conferma
032800060428          when *inkf;
032900060509           b56ricez = 'F';
033000060502       // controllo e decodifico i dati del video
033100060428           exsr sr_ctrd01;
033200060502       // conferma per annullo/ripristino
033300060509           if b56opzio = '04';
033400060428            select;
033500060502       // annullamento
033600060502             when tbeatb = *blanks;
033700060428              tbeatb = 'A';
033800060502       // ripristino
033900060502             when tbeatb = 'A';
034000060428              clear tbeatb;
034100060428            endsl;
034200060428           endif;
034300060428
034400060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
034500060428           if not *in28;
034600060504            wrkcarw01 = *on;
034700060504            $video = 'W01';
034800060504            exsr sr_w01;
034900060428           endif;
035000060428
035100060502       // F8=Record successivo
035200060428          when  *inkh;
035300060428           clear s1opzione;
035400060428           wrkcars01 = *off;
035500060428           $video = 'C01';
035600060509           b56ricez = 'H';
035700060505
035800060505       // F9=Traduzione
035900060505          when  *inki;
036000060505           *in05 = *on;
036100060505           exsr sr_lingua;
036200060505           *in05 = *off;
036300060428
036400060502       // F12=Ritorno
036500060428          when *inkl;
036600060428           clear s1opzione;
036700060509           b56ricez = 'L';
036800060505           unlock tntbe01l;
036900060508       // se non è f12 da immissione/copia non ricarico il subfile
037000060509           if b56comand = 'J' or b56opzio = '03';
037100060428            wrkcars01 = *on;
037200060428           else;
037300060428            wrkcars01 = *off;
037400060428           endif;
037500060428           $video = 'C01';
037600060428
037700060502       // Invio
037800060428          other;
037900060508           if not *in03;
038000060508            exsr sr_ctrd01;
038100060508           endif;
038200060428
038300060502         endsl;
038400060428
038500060502       // fine gestione 'D01'
038600060428        enddo;
038700060428
038800060428       endsr;
038900060502
039000060502       // ------------------------------------------------------------------------
039100060502       // ripeto opzione in tutte le righe del subfile
039200060502       // ------------------------------------------------------------------------
039300060428       begsr sr_ripetiopz;
039400060428
039500060428        if c1csrrrn > 0;
039600060505         s01nrr = c1csrrrn;
039700060509         chain s01nrr tb56s01;
039800060428         if %found and s1opzione > 0;
039900060428          savopzione = s1opzione;
040000060428          *in22 = *on;
040100060509          update tb56s01;
040200060428
040300060428          wrkeofs01 = *off;
040400060428          dou wrkeofs01 = *on;
040500060505           s01nrr = s01nrr + 1;
040600060509           chain s01nrr tb56s01;
040700060428           if %found;
040800060428            s1opzione = savopzione;
040900060509            update tb56s01;
041000060428           else;
041100060428            wrkeofs01 = *on;
041200060428           endif;
041300060428          enddo;
041400060428
041500060428          *in22 = *off;
041600060428
041700060428         endif;
041800060428
041900060428        endif;
042000060428
042100060428       endsr;
042200060428
042300060502       // ------------------------------------------------------------------------
042400060502       // imposto i dati a video
042500060502       // ------------------------------------------------------------------------
042600060428       begsr sr_setvideo;
042700060428
042800060502       // pulisco il formato video D01
042900060502        exsr sr_puld01;
043000060428
043100060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
043200060428        select;
043300060428
043400060502       // F10=Immissione
043500060509         when b56comand = 'J';
043600060428          d1descopz = 'Immissione';
043700060428
043800060502       // Opzione "02"=modifica
043900060509         when b56opzio = '02';
044000060428          d1descopz = 'Modifica';
044100060428          exsr sr_imposta;
044200060428
044300060502       // Opzione "03"=copia
044400060509         when b56opzio = '03';
044500060428          d1descopz = 'Copia';
044600060428          exsr sr_imposta;
044700060428
044800060502       // Opzione "04"=cancellazione/ripristino
044900060509         when b56opzio = '04';
045000060428          exsr sr_imposta;
045100060502       // se richiesta 'cancellazione'
045200060502          if tbeatb = *blanks;
045300060502           d1descopz = 'Annullamento';
045400060502          endif;
045500060502       // se richiesto 'ripristino'
045600060502          if tbeatb = 'A';
045700060502           d1descopz = 'Ripristino';
045800060502          endif;
045900060502
046000060502       // Opzione "05"=visualizzazione
046100060509         when b56Opzio ='05';
046200060502          d1descopz = 'Visualizzazione';
046300060502          exsr sr_imposta;
046400060502
046500060502       // Fine scelta
046600060502        endsl;
046700060502
046800060502       // centro la descrizione della funzione nel formato video
046900060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
047000060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
047100060502
047200060502       endsr;
047300060502
047400060502       // ------------------------------------------------------------------------
047500060502       // controllo video
047600060502       // ------------------------------------------------------------------------
047700060502       begsr sr_ctrd01;
047800060502
047900060502        *in28 = *off;
048000060502        *in40 = *off;
048100060502        *in41 = *off;
048200010430
048300060509       // fase ORM
048400060502        if d1tbeke1 = *blanks;
048500060502         vd1msg = msg(01);
048600060502         *in28 = *on;
048700060502         *in40 = *on;
048800060502         leavesr;
048900060502        endif;
049000060509
049100060509       // deve essere numerica
049200060509        if (%subst(d1tbeke1:1:1) < *zeros and %subst(d1tbeke1:1:1) <> *blanks)
049300060509        or (%subst(d1tbeke1:2:1) < *zeros and %subst(d1tbeke1:2:1) <> *blanks)
049400060509        or (%subst(d1tbeke1:3:1) < *zeros and %subst(d1tbeke1:3:1) <> *blanks);
049500060509         vd1msg = msg(04);
049600060509         *in28 = *on;
049700060509         *in40 = *on;
049800060509         leavesr;
049900060509        endif;
050000060508
050100060508       // se immissione controllo che non esista già
050200060509        if b56comand = 'J' or b56opzio = '03';
050300060509         tbecod = 'FAR';
050400060508         tbeke1 = d1tbeke1;
050500060508         tbeke2 = d1tbeke2;
050600060508         clear tbelin;
050700060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
050800060508         if %found(tntbe01l);
050900060509          vd1msg = msg(03);
051000060508          *in28 = *on;
051100060508          *in40 = *on;
051200060508          leavesr;
051300060508         endif;
051400060508        endif;
051500060502
051600060502       // descrizione
051700060509        if d1fardes = *blanks;
051800060502         vd1msg = msg(02);
051900060502         *in28 = *on;
052000060502         *in41 = *on;
052100060502         leavesr;
052200060502        endif;
052300060502
052400060502       endsr;
052500060504
052600060504       // ------------------------------------------------------------------------
052700060504       // gestione video dati di trasmissione
052800060504       // ------------------------------------------------------------------------
052900060504       begsr sr_w01;
053000060504
053100060504       // imposto i dati a video
053200060504        exsr sr_carw01;
053300060504
053400060505       // fino a che la variabile resta settata come 'W01'
053500060504        dou $video <> 'W01';
053600060504
053700060504       // reset indicatore generico di errore
053800060504         *in28 = *off;
053900060504
054000060504       // emetto il video
054100060509          exfmt tb56w01;
054200060504
054300060504       // controllo tasti funzionali del video
054400060504         select;
054500060504
054600060504       // F6=Conferma
054700060504          when *inkf;
054800060504       // controllo i dati del video
054900060504           exsr sr_ctrw01;
055000060504       // se non riscontrati errori aggiorno il record corrente
055100060504           if not *in28;
055200060504            exsr sr_aggiorna;
055300060504            if not *in28;
055400060505       // emetto le descrizioni in lingua
055500060509       // se non è annullo/ripristino
055600060509             if b56opzio <> '04';
055700060509              *in05 = *off;
055800060509              exsr sr_lingua;
055900060509             endif;
056000060509             if b56comand = 'J';
056100060504              $video = 'D01';
056200060504              exsr sr_setvideo;
056300060504             else;
056400060504              $video = 'C01';
056500060504             endif;
056600060504            endif;
056700060504           endif;
056800060504
056900060504       // F12=Ritorno
057000060504          when *inkl;
057100060504           $video = 'D01';
057200060509           clear b56ricez;
057300060504
057400060504       // Invio
057500060504          other;
057600060504           exsr sr_ctrw01;
057700060504
057800060504         endsl;
057900060504
058000060504       // fine gestione 'W01'
058100060504        enddo;
058200060504
058300060504       endsr;
058400060504
058500060504       // ------------------------------------------------------------------------
058600060504       // imposto i dati di trasmissione
058700060504       // ------------------------------------------------------------------------
058800060504       begsr sr_carw01;
058900060504
059000060504       // se pulisco i campi
059100060504         clear w1ftt;
059200060504         clear w1flt;
059300060504         clear w1ftr;
059400060504         clear w1dtr;
059500060504
059600060504       // se non immissione imposto i campi del file
059700060509        if b56comand <> 'J';
059800060504         w1ftt = tbeftt;
059900060504         w1flt = tbeflt;
060000060504         w1ftr = tbeftr;
060100060504       // imposto la data
060200060504         if tbedtr <> *zeros;
060300060504          clear wlbdat;
060400060504          g02inv = tbedtr;
060500060504          g02err = '3';
060600060504          xsrda8(wlbdat);
060700060504          w1dtr = g02dat;
060800060504         endif;
060900060504
061000060504        endif;
061100060504
061200060504       endsr;
061300060504
061400060504       // ------------------------------------------------------------------------
061500060504       // controllo i dati di trasmissione
061600060504       // ------------------------------------------------------------------------
061700060504       begsr sr_ctrw01;
061800060504
061900060504       endsr;
062000060502
062100060502       // ------------------------------------------------------------------------
062200060502       // aggiorno tabella
062300060502       // ------------------------------------------------------------------------
062400060502       begsr sr_aggiorna;
062500060502
062600060502       // imposto campi del file
062700060509        clear tbelin;
062800060502        tbeke1 = d1tbeke1;
062900060509        tbeke2 = d1tbeke2;
063000060502
063100060509        d§fardes = d1fardes;
063200060509        d§farffb = d1farffb;
063300060509        d§farnonf = d1farnonf;
063400060509        d§farman = d1farman;
063500060509        d§farstp = d1farstp;
063600060509        d§farafvv = d1farafvv;
063700060509        d§farcfvv = d1farcfvv;
063800070704        d§farass = d1farass;
063900070705        d§fardar = d1fardar;
064000060509        d§farnoo = d1farnoo;
064100060926        d§farsta = d1farsta;
064200060509        d§faruva = d1faruva;
064300060509        d§fardva = d1fardva;
064400160302        d§farutt = d1farutt;
064500060502
064600060509        tbeuni = dfar;
064700060502
064800060504        tbeftt = w1ftt;
064900060504        tbeflt = w1flt;
065000060502        clear tbeftr;
065100060502        clear tbedtr;
065200060502
065300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
065400060502        select;
065500060502
065600060502       // F10=immissione - Opzione "03"=copia
065700060509         when b56comand = 'J' or b56opzio = '03';
065800060502       // scrivo nuovo record con gestione errore per chiave duplicata
065900060508          write tntbe000;
066000060502
066100060502       // Opzione "02"=modifica
066200060509         when b56opzio = '02';
066300060502       // update record e controllo errore per chiave duplicata
066400060508          update tntbe000;
066500060502
066600060502       // Opzione "04"=cancellazione/ripristino
066700060509         when b56opzio = '04';
066800060502          update tntbe000;
066900010430
067000060502        endsl;
067100060509
067200060509       // se Opzione "04"=cancellazione/ripristino
067300060509       // aggiorno automaticamente anche le descrizioni in linea
067400060509        if b56opzio = '04';
067500060509         clear lintntbe;
067600060509         setgt lintntbe azlin02l;
067700060509         dou %eof(azlin02l);
067800060509          read azlin02l;
067900060509       // fine file
068000060509          if %eof(azlin02l);
068100060509           leave;
068200060509          endif;
068300060509       // aggiorno
068400060509          tbelin = lintntbe;
068500060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
068600060509          if %found(tntbe11l);
068700060509           select;
068800060509            when tbeatb <> *blanks;
068900060509             clear tbeatb;
069000060509            when tbeatb = *blanks;
069100060509             tbeatb = 'A';
069200060509           endsl;
069300060509            update tntbe11;
069400060509          endif;
069500060509         enddo;
069600060509        endif;
069700060502
069800060502       endsr;
069900060502
070000060502       // ------------------------------------------------------------------------
070100060502       // pulizia video
070200060502       // ------------------------------------------------------------------------
070300060502       begsr sr_puld01;
070400060502
070500060504        clear d1descopz;
070600060504        clear d1tbeke1;
070700060509        clear d1tbeke2;
070800060509        clear d1fardes;
070900060509        clear d1farffb;
071000060509        clear d1farnonf;
071100060509        clear d1farman;
071200060509        clear d1farstp;
071300060509        clear d1farafvv;
071400060509        clear d1farcfvv;
071500070704        clear d1farass;
071600070705        clear d1fardar;
071700060509        clear d1farnoo;
071800060926        clear d1farsta;
071900060509        clear d1faruva;
072000060509        clear d1fardva;
072100160302        clear d1farutt;
072200060504
072300060504        *in28 = *off;
072400060504        *in40 = *off;
072500060504        *in41 = *off;
072600060502
072700060502       endsr;
072800060502
072900060502       // ------------------------------------------------------------------------
073000060502       // imposto i dati a video
073100060502       // ------------------------------------------------------------------------
073200060502       begsr sr_imposta;
073300060502
073400060502       // recupero i dati dal file
073500060505        ktbeke1 = s1tbeke1;
073600060508        clear tbelin;
073700060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
073800060502
073900060502       // imposto i campi a video
074000060505        d1tbeke2 = tbeke2;
074100060502        d1tbeke1 = tbeke1;
074200060509        dfar = tbeuni;
074300060509        d1fardes = d§fardes;
074400060509        d1farffb = d§farffb;
074500060509        d1farnonf = d§farnonf;
074600060509        d1farman = d§farman;
074700060509        d1farstp = d§farstp;
074800060509        d1farafvv = d§farafvv;
074900060509        d1farcfvv = d§farcfvv;
075000070704        d1farass = d§farass;
075100070705        d1fardar = d§fardar;
075200060509        d1farnoo = d§farnoo;
075300060926        d1farsta = d§farsta;
075400060509        d1faruva = d§faruva;
075500060509        d1fardva = d§fardva;
075600160302        d1farutt = d§farutt;
075700060502
075800060502       endsr;
075900060502
076000060502       // ------------------------------------------------------------------------
076100060502       // gestione del subfile
076200060502       // ------------------------------------------------------------------------
076300060502       begsr sr_gestsfl;
076400060502
076500060502       // set flag
076600060502        wrkeofs01 = *off;
076700060502
076800060502       // inizio lettura subfile
076900060502        dow wrkeofs01 = *off and *in21;
077000060509         readc tb56s01;
077100060502       // se fine subfile
077200060502         if %eof;
077300060502          wrkcars01 = *on;
077400060502          leave;
077500060502         endif;
077600060502       // se è stata immessa un'opzione
077700060502         if s1opzione <> *zeros;
077800060502       // reset ds di servizio
077900060509          clear tntb56ds;
078000060502       // controllo ed elaborazione opzione immessa
078100060502          select;
078200060502       // opzione "01"=scelta
078300060502           when s1opzione = 1 and *in01;
078400060509            b56opzio = '01';
078500060502       // opzione "02"=modifica
078600060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
078700060509            b56opzio = '02';
078800060502       // opzione "03"=copia
078900060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
079000060509            b56opzio = '03';
079100060502       // opzione "04"=annullo/ripristino
079200060502           when s1opzione = 4 and not *in01;
079300060509            b56opzio = '04';
079400060502       // opzione "05"=visualizzazione
079500060502           when s1opzione = 5;
079600060509            b56opzio = '05';
079700060502          endsl;
079800010503
079900060502       // se immessa almeno un opzione valida
080000060509          if  b56opzio <> *blanks;
080100060502       // gstione del formato video
080200060504           exsr sr_d01;
080300060502       // se la gestione si è chiusa con ...
080400060504           select;
080500060502       // F6=Conferma
080600060509            when b56ricez = 'F';
080700060504             clear s1opzione;
080800060504             wrkcars01 = *on;
080900060502       // F12=Ritorno
081000060509            when b56ricez = 'L';
081100060504             clear s1opzione;
081200060504             wrkeofs01 = *on;
081300060502       // altrimenti
081400060504            other;
081500060504             *in22 = *on;
081600060504           endsl;
081700060502
081800060509           update tb56s01;
081900060504           *in22 = *off;
082000060504       // fine opzione
082100060504          endif;
082200060502       // fine opzione
082300060502         endif;
082400060502
082500060502        enddo;
082600060502
082700060502       endsr;
082800060502
082900060502       // ------------------------------------------------------------------------
083000060502       // posizionamento sul file
083100060502       // ------------------------------------------------------------------------
083200060502       begsr sr_setlltbe01;
083300060502
083400060502        select;
083500060502       // chiave totale
083600060502         when wrkkey = '1';
083700060502          setll tbecod tntbe01l;
083800060502       // chiave parziale
083900060502         when wrkkey = '2';
084000060502          setll (tbecod:tbeke1) tntbe01l;
084100060502        endsl;
084200060502
084300060502       endsr;
084400060502
084500060502       // ------------------------------------------------------------------------
084600060502       // scrivo subfile
084700060502       // ------------------------------------------------------------------------
084800060502       begsr sr_wtrs01;
084900060502
085000060502       // se non raggiunto limite massimo di caricamento
085100060505        if s01nrr < 9999;
085200060502       // imposto campi di subfile
085300060502         exsr sr_sets01;
085400060502       // imposto numeratore righe e numero relativo di record
085500060505         s01nrr = s01nrr + 1;
085600060502       // scrivo subfile
085700060509         write tb56s01;
085800060502        endif;
085900060502
086000060502       endsr;
086100060502
086200060502       // ------------------------------------------------------------------------
086300060502       // imposto campi del subfile
086400060502       // ------------------------------------------------------------------------
086500060502       begsr sr_sets01;
086600060502
086700060502        s1tbeke1 = tbeke1;
086800060505        s1tbeke2 = tbeke2;
086900060509        dfar = tbeuni;
087000060509        s1tbeuni = d§fardes;
087100060502        s1tbeatb = tbeatb;
087200060502
087300060502       endsr;
087400060505
087500060505       // ------------------------------------------------------------------------
087600060505       // gestione tabelle in lingua
087700060505       // ------------------------------------------------------------------------
087800060505       begsr sr_lingua;
087900060505
088000060505       // pulisco il subfile
088100060505        exsr sr_pulsfl;
088200060505       // carico il subfile con i dati che trovo o vuoto
088300060505        exsr sr_carsfl;
088400060505
088500060505        wrksfl02 = *on;
088600060505
088700060505        dou wrksfl02 = *off;
088800060505         rec = 1;
088900060505         if savrec > *zeros;
089000060505          rec = savrec;
089100060505         endif;
089200060509         write tb56p02;
089300060509         exfmt tb56c02;
089400060505
089500060505         *in28 = *off;
089600060509         *in42 = *off;
089700060505
089800060505       // F12=Ritorno
089900060505          if *inkl;
090000060505           wrksfl02 = *off;
090100060508           leavesr;
090200060505          endif;
090300060505
090400060505       // Controlli
090500060505          if not *in05;
090600060505           exsr sr_ctrsfl;
090700060505          endif;
090800060505
090900060505       // F6=Conferma
091000060505          if *inkf and not *in28;
091100060508           exsr sr_confsfl;
091200060505           wrksfl02 = *off;
091300060505          endif;
091400060505
091500060505         enddo;
091600060505
091700060505       endsr;
091800060505
091900060505       // ------------------------------------------------------------------------
092000060505       // pulisco il subfile delle descrizioni in lingua
092100060505       // ------------------------------------------------------------------------
092200060505       begsr sr_pulsfl;
092300060505
092400060505        clear s02nrr;
092500060505        clear savrec;
092600060508        *in24 = *off;
092700060508        *in25 = *off;
092800060509        write tb56c02;
092900060508        *in24 = *on;
093000060508        *in25 = *on;
093100060505
093200060505       endsr;
093300060505
093400060505       // ------------------------------------------------------------------------
093500060505       // carico il subfile delle descrizioni in lingua
093600060505       // ------------------------------------------------------------------------
093700060505       begsr sr_carsfl;
093800060505
093900060505        *in30 = *off;
094000060505       // carico un record per ogni lingua inserite in AZLIN
094100060505        clear lintntbe;
094200060505        setgt lintntbe azlin02l;
094300060505        dou %eof(azlin02l);
094400060505         read azlin02l;
094500060505       // fine file
094600060505         if %eof(azlin02l);
094700060505          leave;
094800060505         endif;
094900060509         clear dfar;
095000060505         s2dlin = lindesita;
095100060509         tbecod = 'FAR';
095200060505         tbeke1 = d1tbeke1;
095300060505         tbeke2 = d1tbeke2;
095400060505         tbelin = lintntbe;
095500060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
095600060508         if not %found(tntbe11l);
095700060505          clear s2cdes;
095800060509          clear s2cdesvas;
095900060505         else;
096000060509          dfar = tbeuni;
096100060509          s2cdes = d§fardes;
096200060509          s2cdesvas = d§fardva;
096300060505         endif;
096400060505         s2hcdlin = tbelin;
096500060505         s02nrr = s02nrr + 1;
096600060509         write tb56s02;
096700060505        enddo;
096800060505
096900060505        *in30 = *on;
097000060505
097100060505       endsr;
097200060505
097300060505       // ------------------------------------------------------------------------
097400060505       // controllo il subfile delle descrizioni in lingua
097500060505       // ------------------------------------------------------------------------
097600060505       begsr sr_ctrsfl;
097700060505
097800060505        s02nrr = 1;
097900060509        chain s02nrr tb56s02;
098000060505        dow %found;
098100060505       // deve esserci la descrizione
098200060505         if s2cdes = *blanks;
098300060505          vc2msg = msg(02);
098400060505          *in28 = *on;
098500060509          *in42 = *on;
098600060505          savrec = s02nrr;
098700060509          update tb56s02;
098800060505          leave;
098900060505         endif;
099000060509         update tb56s02;
099100060505         s02nrr = s02nrr + 1;
099200060509         chain s02nrr tb56s02;
099300060505        enddo;
099400060505
099500060505       endsr;
099600060505
099700060505       // ------------------------------------------------------------------------
099800060505       // confermo il dati del subfile delle descrizioni in lingua
099900060505       // ------------------------------------------------------------------------
100000060505       begsr sr_confsfl;
100100060505
100200060505       // recupero i dati della tabella in italiano
100300060505        clear savtbeuni;
100400060509        tbecod = 'FAR';
100500060505        tbeke1 = d1tbeke1;
100600060505        tbeke2 = d1tbeke2;
100700060505        clear tbelin;
100800060531        chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
100900060508        if %found(tntbe11l);
101000060505         savtbeuni = tbeuni;
101100060505        endif;
101200060505
101300060505       // leggo il subfile e aggiorno/scrivo i dati in lingua
101400060505        s02nrr = 1;
101500060509        chain s02nrr tb56s02;
101600060505        dow %found;
101700060505         tbelin = s2hcdlin;
101800060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
101900060509         dfar = savtbeuni;
102000060509         d§fardes = s2cdes;
102100060509         d§fardva = s2cdesvas;
102200060509         tbeuni = dfar;
102300060508         tbeftt = w1ftt;
102400060505         clear tbeftr;
102500060505         clear tbedtr;
102600060505       // scrivo
102700060508         if not %found(tntbe11l);
102800060509          tbecod = 'FAR';
102900060505          tbeke1 = d1tbeke1;
103000060505          tbeke2 = d1tbeke2;
103100060505          tbelin = s2hcdlin;
103200060508          write tntbe11;
103300060505         else;
103400060505       // aggiorno
103500060508          update tntbe11;
103600060505         endif;
103700060505         s02nrr = s02nrr + 1;
103800060509         chain s02nrr tb56s02;
103900060505        enddo;
104000060505
104100060505       endsr;
104200060428
104300060502       // ------------------------------------------------------------------------
104400060502       // routine iniziale
104500060502       // ------------------------------------------------------------------------
104600060428         begsr *inzsr;
104700060428
104800060428      /end-free
104900060428
105000060428     c     *entry        plist
105100060428     c                   parm                    kpjba
105200060428
105300060428      /free
105400060428         in(e) §azute;
105500060428         if not %error;
105600060428          in(e) §datiute;
105700060428         endif;
105800060502         if %error or rsut = *blanks;
105900060428          tibs34r(tibs34ds);
106000060428          in §azute;
106100060428          in §datiute;
106200060428         endif;
106300060428
106400060428         endsr;
106500060502
106600060502       // ------------------------------------------------------------------------
106700060502       // uscita dal programma
106800060502       // ------------------------------------------------------------------------
106900060502         begsr sr_uscita;
107000060502
107100060509          if b56esito = *blanks;
107200060509           b56esito = eseguito;
107300060502          endif;
107400060502
107500060509          kpjbu = tntb56ds;
107600060502
107700060502          *inlr = *on;
107800060502          return;
107900060502
108000060502         endsr;
108100060502
108200060502      /end-free
108300060502
108400060502** -MSG-                                                                     *
108500060509Immettere la fase                                                              01
108600060502Immettere la descrizione                                                       02
108700060509Fase già esistente                                                             03
108800060509Fase errata                                                                    04
