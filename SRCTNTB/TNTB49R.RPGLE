000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300060427      * Gestione tabella "CCH" = Causali chiusura C.A.                         *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600060508     ftntbe11l  uf a e           k disk    rename(tntbe000:tntbe11)
000700060505     fazlin02l  if   e           k disk
000800060505     ftntb49d   cf   e             workstn sfile(tb49s01:s01nrr)
000900060505     f                                     sfile(tb49s02:s02nrr)
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
001400060428      * 01 - ON ricerca -- OFF manutenzione
001500060508      * 02 - proteggo campo causale
001600060508      * 03 - proteggo campi video 01
001700060509      * 04 - visualizzazione
001800060508      * 05 - visualizzo subfile lingue
001900060508      * 20 - gestione subfile 01
002000060508      * 21 - gestione subfile 01
002100060508      * 22 - gestione subfile 01
002200060508      * 23 - gestione subfile 01
002300060508      * 24 - gestione subfile 02
002400060508      * 25 - gestione subfile 02
002500060428      * 28 - messaggio errore
002600060502      * 40 - causale
002700060502      * 41 - descrizione
002800060505      * 47 - descrizione subfile
002900060502
003000060502      *------------------------------------------------------------------------*
003100060502      * schiere
003200060502      *------------------------------------------------------------------------*
003300060502     d msg             s             78    dim(17) ctdata perrcd(1)
003400060427
003500060428      *------------------------------------------------------------------------*
003600060428      * campi di work
003700060428      *------------------------------------------------------------------------*
003800060428     d blanks          s                   like(d1descopz)
003900060502     d comando         s              1
004000060505     d ktbeke1         s                   like(tbeke1)
004100060428     d len             s              5u 0
004200060505     d s01nrr          s              4  0
004300060505     d s02nrr          s              4  0
004400060428     d savopzione      s                   like(s1opzione)
004500060428     d savrcdnbr       s                   like(c1rcdnbr)
004600060505     d savrec          s                   like(s02nrr)
004700060531     d savtbeftt       s                   like(tbeftt)
004800060531     d savtbeflt       s                   like(tbeflt)
004900060505     d savtbeuni       s                   like(tbeuni)
005000060428     d wrkkey          s              1
005100060428     d wrkeofs01       s              1
005200060428     d wrkcars01       s              1
005300060505     d wrkcarw01       s              1
005400060505     d wrksfl02        s              1
005500060504     d $video          s             10
005600060428
005700060428      *------------------------------------------------------------------------*
005800060428      * ds interne/esterne
005900060428      *------------------------------------------------------------------------*
006000060504     d wlbdat          ds                  inz
006100060504     d  g02dat                 1      8  0
006200060504     d  g02inv                 9     16  0
006300060504     d  g02err                17     17
006400060504     d  g02tgi                18     22  0
006500060428
006600060502     d dcch          e ds
006700060502     d kpjba         e ds
006800060503     d tibs34ds      e ds                  inz
006900060502     d tntb49ds      e ds
007000060428     d §azute        e ds                  extname(azute00f)
007100060428     d                                     dtaara
007200060428     d §datiute      e ds                  extname(ddatiute)
007300060428     d                                     dtaara
007400010503
007500060428     d psds           sds
007600060428     d  pgmname          *proc
007700060428
007800060428      *------------------------------------------------------------------------*
007900060428      * pgm richiamati
008000060428      *------------------------------------------------------------------------*
008100060428     d tibs34r         pr                  extpgm('TIBS34R')
008200060502     d  tibs34ds                           likeds(tibs34ds)
008300060504
008400060504     d xsrda8          pr                  extpgm('XSRDA8')
008500060504     d  wlbdat                             likeds(wlbdat)
008600060428
008700060428      *------------------------------------------------------------------------*
008800060428      * costanti
008900060428      *------------------------------------------------------------------------*
009000060428     d errore          c                   '1'
009100060428     d eseguito        c                   '0'
009200010503
009300060428      *------------------------------------------------------------------------*
009400010423
009500060428      /free
009600060428
009700060428       exsr sr_parm;
009800060428       exsr sr_c01;
009900060428       exsr sr_uscita;
010000010423
010100060502       // ------------------------------------------------------------------------
010200060502       // elaborazione parametri ricevuti
010300060502       // ------------------------------------------------------------------------
010400060428       begsr sr_parm;
010500060428
010600060428        tntb49dS = kpjbu;
010700060428
010800060428        select;
010900060502       // ricerca e scleta
011000060428         when b49Fun = '1';
011100060428          *in01 = *on;
011200060502       // manutenzione
011300060502         when  b49Fun = *blanks;
011400060428          *in01 = *off;
011500060502       // altrimenti
011600060428         other;
011700060428          b49Esito = errore;
011800060428           exsr sr_uscita;
011900060428         endsl;
012000060428
012100060428       endsr;
012200010430
012300060502       // ------------------------------------------------------------------------
012400060502       // gestione subfile
012500060502       // ------------------------------------------------------------------------
012600060428       begsr sr_c01;
012700060428
012800060502       // imposto variabili
012900060428        wrkcars01 = *on;
013000060428        $video = 'C01';
013100060428
013200060502       // inizio elaborazione subfile
013300060428         dou  $video <> 'C01';
013400060428
013500060502       // caricamento subfile
013600060428          exsr sr_cars01;
013700060428
013800060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
013900060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
014000060502       // l'ultimo tasto
014100060428          if c1csrrrn > 0;
014200060428           c1rcdnbr = c1csrrrn;
014300060428          else;
014400060428           c1rcdnbr = savrcdnbr;
014500060428          endif;
014600060428
014700060502       // se non so quale pagina visualizzare forzo pagina 1
014800060428          if c1rcdnbr < 1;
014900060428           eval c1rcdnbr = 1;
015000060428          endif;
015100060428
015200060502       // salvo il record number del subfile
015300060428          savrcdnbr = c1Rcdnbr;
015400060428
015500060502       // Emissione del subfile
015600060508          write tb49p01;
015700060428          exfmt tb49c01;
015800060428
015900060502       // controllo tasti funzionali del subfile
016000060428          select;
016100060428
016200060502       // F3=Fine
016300060428           when *inkc;
016400060502            $video = *blanks;
016500060428            b49ricez = 'C';
016600060428            exsr sr_uscita;
016700060428
016800060502       // F5=Refresh
016900060428           when *inke;
017000060428            wrkcars01 = *on;
017100060428
017200060502       // F10=Immissione
017300060428           when *inkj;
017400060502            $video = 'D01';
017500060428            clear tntb49ds;
017600060428            b49comand = 'J';
017700060428            exsr sr_d01;
017800060428
017900060502       // F13=Ripetizione
018000060428           when *inkm;
018100060428            exsr sr_ripetiopz;
018200060428
018300060502       // in tutti gli altri casi
018400060428           other;
018500060502       // controllo ed elaborazione scelte su subfile
018600060428            exsr sr_gestsfl;
018700060428          endsl;
018800060428
018900060502       // fine elaborazione 'C01'
019000060428         enddo;
019100060428
019200060428        endsr;
019300060428
019400060502       // ------------------------------------------------------------------------
019500060502       // caricamento subfile
019600060502       // ------------------------------------------------------------------------
019700060428       begsr sr_cars01;
019800010430
019900060502       // se è stato richiesto il caricamento del subfile
020000060428        if wrkcars01 = *on;
020100060428
020200060502       // inizializzo il subfile
020300060505         s01nrr = 0;
020400060428         *in20 = *on;
020500060428         write tb49c01;
020600060428         *in20 = *off;
020700060428         *in21 = *off;
020800060428         *in22 = *off;
020900060428         *in23 = *off;
021000060428
021100060502       // imposto la chiave di posizionamento e lettura file
021200060428         tbecod = 'CCH';
021300060428         tbeke1 = c1setll;
021400060428
021500060502       // se è stato scelto il posizionamento
021600060502         if c1setll <> *blanks;
021700060428          wrkkey = '2';
021800060502       // altrimenti
021900060428         else;
022000060428          wrkkey = '1';
022100060428         endif;
022200060428
022300060502       // posizionamento file
022400060428         exsr sr_setlltbe01;
022500060428
022600060502       // fino a che non è fine file
022700060428         dou %eof(tntbe01l);
022800060428
022900060502       // leggo file
023000060505          reade(n) tbecod tntbe01l;
023100060428
023200060502       // fine file esco
023300060428          if %eof(tntbe01l);
023400060428           leave;
023500060428          endif;
023600060428
023700060502       // se in "ricerca/scelta" non carico i records annullati
023800060502          if (*in01 and tbeatb = *blanks) or not *in01;
023900060502       // scrivo subfile
024000060428           clear s1opzione;
024100060428           exsr sr_wtrs01;
024200060428          endif;
024300060428
024400060428         enddo;
024500060428
024600060502       // fine caricamento subfile
024700060428        endif;
024800060428
024900060502       // se scritto almeno un record
025000060505        if s01nrr > 0;
025100060502       // indicatore di sflend
025200060428         eval *in21 = *on;
025300060428        endif;
025400060428
025500060502       // se non scritto neanche un record
025600060505        if s01nrr = 0;
025700060502       // indicatore di sfldsp
025800060428         eval *in23 = *on;
025900060428        endif;
026000060428
026100060502       // disattivo opzione di caricamento subfile
026200060428        wrkcars01 = *off;
026300060428
026400060428       endsr;
026500060428
026600060502       // ------------------------------------------------------------------------
026700060502       // gestione videata
026800060502       // ------------------------------------------------------------------------
026900060428       begsr sr_d01;
027000060428
027100060504       // imposto il video a seconda della funzione richiesta
027200060428        exsr sr_setvideo;
027300060428
027400060502       // imposto variabile
027500060428        $video = 'D01';
027600060428
027700060502       // fino a che la variabile resta settata come 'D01'
027800060428        dou $video <> 'D01';
027900060428
028000060508         *in02 = *off;
028100060508         *in03 = *off;
028200060509         *in04 = *off;
028300060428
028400060502       // se immessa opzione di 'scelta'
028500060502         if b49opzio = '01';
028600060428          b49ke1 = s1tbeke1;
028700060428          b49des = s1tbeuni;
028800060428          exsr sr_uscita;
028900060428
029000060508       // se immessa un'altra opzione
029100060508         else;
029200060508       // se non è immissione/copia proteggo il campo della causale
029300060508          if b49comand <> 'J' and b49opzio <> '03';
029400060428           *in02 = *on;
029500060428          endif;
029600060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
029700060508       // proteggo i campi del video
029800060509          if b49opzio = '04' or b49opzio = '05';
029900060508           *in03 = *on;
030000060508          endif;
030100060509       // se immessa opzione di 'visualizzazione'
030200060509       // non attivo F6
030300060509          if b49opzio = '05';
030400060509           *in04 = *on;
030500060509          endif;
030600060502       // emetto il video
030700060428          exfmt tb49d01;
030800060428         endif;
030900060428
031000060502       // reset indicatore generico di errore
031100060428         *in28 = *off;
031200060508
031300060508       // pulisco il campo messaggi
031400060508         clear vd1msg;
031500010430
031600060502       // controllo tasti funzionali del video
031700060428         select;
031800060428
031900060502       // F3=Fine
032000060428          when *inkc;
032100060428           b49ricez = 'C';
032200060502           $video = *blanks;
032300060505           unlock tntbe01l;
032400060428           exsr sr_uscita;
032500060428
032600060502       // F6=Conferma
032700060428          when *inkf;
032800060428           b49ricez = 'F';
032900060502       // controllo e decodifico i dati del video
033000060428           exsr sr_ctrd01;
033100060502       // conferma per annullo/ripristino
033200060428           if b49opzio = '04';
033300060428            select;
033400060502       // annullamento
033500060502             when tbeatb = *blanks;
033600060428              tbeatb = 'A';
033700060502       // ripristino
033800060502             when tbeatb = 'A';
033900060428              clear tbeatb;
034000060428            endsl;
034100060428           endif;
034200060428
034300060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
034400060428           if not *in28;
034500060504            wrkcarw01 = *on;
034600060504            $video = 'W01';
034700060504            exsr sr_w01;
034800060428           endif;
034900060428
035000060502       // F8=Record successivo
035100060428          when  *inkh;
035200060428           clear s1opzione;
035300060428           wrkcars01 = *off;
035400060428           $video = 'C01';
035500060428           b49ricez = 'H';
035600060505
035700060505       // F9=Traduzione
035800060505          when  *inki;
035900060505           *in05 = *on;
036000060505           exsr sr_lingua;
036100060505           *in05 = *off;
036200060428
036300060502       // F12=Ritorno
036400060428          when *inkl;
036500060428           clear s1opzione;
036600060428           b49ricez = 'L';
036700060505           unlock tntbe01l;
036800060508       // se non è f12 da immissione/copia non ricarico il subfile
036900060508           if b49comand = 'J' or b49opzio = '03';
037000060428            wrkcars01 = *on;
037100060428           else;
037200060428            wrkcars01 = *off;
037300060428           endif;
037400060428           $video = 'C01';
037500060428
037600060502       // Invio
037700060428          other;
037800060508           if not *in03;
037900060508            exsr sr_ctrd01;
038000060508           endif;
038100060428
038200060502         endsl;
038300060428
038400060502       // fine gestione 'D01'
038500060428        enddo;
038600060428
038700060428       endsr;
038800060502
038900060502       // ------------------------------------------------------------------------
039000060502       // ripeto opzione in tutte le righe del subfile
039100060502       // ------------------------------------------------------------------------
039200060428       begsr sr_ripetiopz;
039300060428
039400060428        if c1csrrrn > 0;
039500060505         s01nrr = c1csrrrn;
039600060505         chain s01nrr tb49s01;
039700060428         if %found and s1opzione > 0;
039800060428          savopzione = s1opzione;
039900060428          *in22 = *on;
040000060428          update tb49s01;
040100060428
040200060428          wrkeofs01 = *off;
040300060428          dou wrkeofs01 = *on;
040400060505           s01nrr = s01nrr + 1;
040500060505           chain s01nrr tb49s01;
040600060428           if %found;
040700060428            s1opzione = savopzione;
040800060428            update tb49s01;
040900060428           else;
041000060428            wrkeofs01 = *on;
041100060428           endif;
041200060428          enddo;
041300060428
041400060428          *in22 = *off;
041500060428
041600060428         endif;
041700060428
041800060428        endif;
041900060428
042000060428       endsr;
042100060428
042200060502       // ------------------------------------------------------------------------
042300060502       // imposto i dati a video
042400060502       // ------------------------------------------------------------------------
042500060428       begsr sr_setvideo;
042600060428
042700060502       // pulisco il formato video D01
042800060502        exsr sr_puld01;
042900060428
043000060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
043100060428        select;
043200060428
043300060502       // F10=Immissione
043400060428         when b49comand = 'J';
043500060428          d1descopz = 'Immissione';
043600060428
043700060502       // Opzione "02"=modifica
043800060428         when b49opzio = '02';
043900060428          d1descopz = 'Modifica';
044000060428          exsr sr_imposta;
044100060428
044200060502       // Opzione "03"=copia
044300060428         when b49opzio = '03';
044400060428          d1descopz = 'Copia';
044500060428          exsr sr_imposta;
044600060428
044700060502       // Opzione "04"=cancellazione/ripristino
044800060428         when b49opzio = '04';
044900060428          exsr sr_imposta;
045000060502       // se richiesta 'cancellazione'
045100060502          if tbeatb = *blanks;
045200060502           d1descopz = 'Annullamento';
045300060502          endif;
045400060502       // se richiesto 'ripristino'
045500060502          if tbeatb = 'A';
045600060502           d1descopz = 'Ripristino';
045700060502          endif;
045800060502
045900060502       // Opzione "05"=visualizzazione
046000060502         when b49Opzio ='05';
046100060502          d1descopz = 'Visualizzazione';
046200060502          exsr sr_imposta;
046300060502
046400060502       // Fine scelta
046500060502        endsl;
046600060502
046700060502       // centro la descrizione della funzione nel formato video
046800060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
046900060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
047000060502
047100060502       endsr;
047200060502
047300060502       // ------------------------------------------------------------------------
047400060502       // controllo video
047500060502       // ------------------------------------------------------------------------
047600060502       begsr sr_ctrd01;
047700060502
047800060502        *in28 = *off;
047900060502        *in40 = *off;
048000060502        *in41 = *off;
048100010430
048200060502       // causale chiusura C.A.
048300060502        if d1tbeke1 = *blanks;
048400060502         vd1msg = msg(01);
048500060502         *in28 = *on;
048600060502         *in40 = *on;
048700060502         leavesr;
048800060502        endif;
048900060508
049000060508       // se immissione controllo che non esista già
049100060508        if b49comand = 'J' or b49opzio = '03';
049200060508         tbecod = 'CCH';
049300060508         tbeke1 = d1tbeke1;
049400060508         tbeke2 = d1tbeke2;
049500060508         clear tbelin;
049600060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
049700060508         if %found(tntbe01l);
049800060508          vd1msg = msg(05);
049900060508          *in28 = *on;
050000060508          *in40 = *on;
050100060508          leavesr;
050200060508         endif;
050300060508        endif;
050400060502
050500060502       // descrizione
050600060502        if d1cchdes = *blanks;
050700060502         vd1msg = msg(02);
050800060502         *in28 = *on;
050900060502         *in41 = *on;
051000060502         leavesr;
051100060502        endif;
051200060502
051300060502       endsr;
051400060504
051500060504       // ------------------------------------------------------------------------
051600060504       // gestione video dati di trasmissione
051700060504       // ------------------------------------------------------------------------
051800060504       begsr sr_w01;
051900060504
052000060504       // imposto i dati a video
052100060504        exsr sr_carw01;
052200060504
052300060505       // fino a che la variabile resta settata come 'W01'
052400060504        dou $video <> 'W01';
052500060504
052600060504       // reset indicatore generico di errore
052700060504         *in28 = *off;
052800060504
052900060504       // emetto il video
053000060504          exfmt tb49w01;
053100060504
053200060504       // controllo tasti funzionali del video
053300060504         select;
053400060504
053500060504       // F6=Conferma
053600060504          when *inkf;
053700060504       // controllo i dati del video
053800060504           exsr sr_ctrw01;
053900060504       // se non riscontrati errori aggiorno il record corrente
054000060504           if not *in28;
054100060504            exsr sr_aggiorna;
054200060504            if not *in28;
054300060505       // emetto le descrizioni in lingua
054400060509       // se non è annullo/ripristino
054500060509             if b49opzio <> '04';
054600060509              *in05 = *off;
054700060509              exsr sr_lingua;
054800060509             endif;
054900060504             if b49comand = 'J';
055000060504              $video = 'D01';
055100060504              exsr sr_setvideo;
055200060504             else;
055300060504              $video = 'C01';
055400060504             endif;
055500060504            endif;
055600060504           endif;
055700060504
055800060504       // F12=Ritorno
055900060504          when *inkl;
056000060504           $video = 'D01';
056100060504           clear b49ricez;
056200060504
056300060504       // Invio
056400060504          other;
056500060504           exsr sr_ctrw01;
056600060504
056700060504         endsl;
056800060504
056900060504       // fine gestione 'W01'
057000060504        enddo;
057100060504
057200060504       endsr;
057300060504
057400060504       // ------------------------------------------------------------------------
057500060504       // imposto i dati di trasmissione
057600060504       // ------------------------------------------------------------------------
057700060504       begsr sr_carw01;
057800060504
057900060504       // se pulisco i campi
058000060504         clear w1ftt;
058100060504         clear w1flt;
058200060504         clear w1ftr;
058300060504         clear w1dtr;
058400060504
058500060504       // se non immissione imposto i campi del file
058600060504        if b49comand <> 'J';
058700060504         w1ftt = tbeftt;
058800060504         w1flt = tbeflt;
058900060504         w1ftr = tbeftr;
059000060504       // imposto la data
059100060504         if tbedtr <> *zeros;
059200060504          clear wlbdat;
059300060504          g02inv = tbedtr;
059400060504          g02err = '3';
059500060504          xsrda8(wlbdat);
059600060504          w1dtr = g02dat;
059700060504         endif;
059800060504
059900060504        endif;
060000060504
060100060504       endsr;
060200060504
060300060504       // ------------------------------------------------------------------------
060400060504       // controllo i dati di trasmissione
060500060504       // ------------------------------------------------------------------------
060600060504       begsr sr_ctrw01;
060700060504
060800060504       endsr;
060900060502
061000060502       // ------------------------------------------------------------------------
061100060502       // aggiorno tabella
061200060502       // ------------------------------------------------------------------------
061300060502       begsr sr_aggiorna;
061400060502
061500060502       // imposto campi del file
061600060509        clear tbelin;
061700060502        tbeke1 = d1tbeke1;
061800060509        tbeke2 = d1tbeke2;
061900060502
062000060502        §cchdesc = d1cchdes;
062100060502        §cchcran = d1cchcran;
062200060502        §cchnre = d1cchnre;
062300060502        §cchima = d1cchima;
062400060502        §cchlr52 = d1cchlr52;
062500060502        §cchgevi = d1cchgevi;
062600060502        §cchmerp = d1cchmerp;
062700060502        §cchinte = d1cchinte;
062800060502        §cchchte = d1cchchte;
062900060502        §cchprot = d1cchprot;
063000060502        §cchute1 = d1cchute1;
063100060502        §cchute2 = d1cchute2;
063200060502        §cchute3 = d1cchute3;
063300060502        §cchute4 = d1cchute4;
063400060502        §cchute5 = d1cchute5;
063500060502        §cchpaga = d1cchpaga;
063600060502        §cchutil = d1cchutil;
063700060911        §cchccco = d1cchccco;
063800060502
063900060502        tbeuni = dcch;
064000060502
064100060504        tbeftt = w1ftt;
064200060504        tbeflt = w1flt;
064300060502        clear tbeftr;
064400060502        clear tbedtr;
064500060502
064600060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
064700060502        select;
064800060502
064900060502       // F10=immissione - Opzione "03"=copia
065000060502         when b49comand = 'J' or b49opzio = '03';
065100060502       // scrivo nuovo record con gestione errore per chiave duplicata
065200060508          write tntbe000;
065300060502
065400060502       // Opzione "02"=modifica
065500060502         when b49opzio = '02';
065600060502       // update record e controllo errore per chiave duplicata
065700060508          update tntbe000;
065800060502
065900060502       // Opzione "04"=cancellazione/ripristino
066000060502         when b49opzio = '04';
066100060502          update tntbe000;
066200010430
066300060502        endsl;
066400060509
066500060509       // se Opzione "04"=cancellazione/ripristino
066600060509       // aggiorno automaticamente anche le descrizioni in linea
066700060509        if b49opzio = '04';
066800060509         clear lintntbe;
066900060509         setgt lintntbe azlin02l;
067000060509         dou %eof(azlin02l);
067100060509          read azlin02l;
067200060509       // fine file
067300060509          if %eof(azlin02l);
067400060509           leave;
067500060509          endif;
067600060509       // aggiorno
067700060509          tbelin = lintntbe;
067800060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
067900060509          if %found(tntbe11l);
068000060509           select;
068100060509            when tbeatb <> *blanks;
068200060509             clear tbeatb;
068300060509            when tbeatb = *blanks;
068400060509             tbeatb = 'A';
068500060509           endsl;
068600161013            tbeFTT = w1ftt;
068700161013            tbeFLT = w1flt;
068800161013            clear tbeFTR;
068900161013            clear tbeDTR;
069000060509            update tntbe11;
069100060509          endif;
069200060509         enddo;
069300060509        endif;
069400060502
069500060502       endsr;
069600060502
069700060502       // ------------------------------------------------------------------------
069800060502       // pulizia video
069900060502       // ------------------------------------------------------------------------
070000060502       begsr sr_puld01;
070100060502
070200060504        clear d1descopz;
070300060504        clear d1tbeke1;
070400060504        clear d1cchdes;
070500060504        clear d1cchcran;
070600060504        clear d1cchnre;
070700060504        clear d1cchima;
070800060504        clear d1cchlr52;
070900060504        clear d1cchgevi;
071000060504        clear d1cchmerp;
071100060504        clear d1cchinte;
071200060504        clear d1cchchte;
071300060504        clear d1cchprot;
071400060504        clear d1cchute1;
071500060504        clear d1cchute2;
071600060504        clear d1cchute3;
071700060504        clear d1cchute4;
071800060504        clear d1cchute5;
071900060504        clear d1cchpaga;
072000060504        clear d1cchutil;
072100060911        clear d1cchccco;
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
074200060502        dcch = tbeuni;
074300060502        d1cchdes = §cchdesc;
074400060502        d1cchcran = §cchcran;
074500060502        d1cchnre = §cchnre;
074600060502        d1cchima = §cchima;
074700060502        d1cchlr52 = §cchlr52;
074800060502        d1cchgevi = §cchgevi;
074900060502        d1cchmerp = §cchmerp;
075000060502        d1cchinte = §cchinte;
075100060502        d1cchchte = §cchchte;
075200060502        d1cchprot = §cchprot;
075300060502        d1cchute1 = §cchute1;
075400060502        d1cchute2 = §cchute2;
075500060502        d1cchute3 = §cchute3;
075600060502        d1cchute4 = §cchute4;
075700060502        d1cchute5 = §cchute5;
075800060502        d1cchpaga = §cchpaga;
075900060502        d1cchutil = §cchutil;
076000060911        d1cchccco = §cchccco;
076100060502
076200060502       endsr;
076300060502
076400060502       // ------------------------------------------------------------------------
076500060502       // gestione del subfile
076600060502       // ------------------------------------------------------------------------
076700060502       begsr sr_gestsfl;
076800060502
076900060502       // set flag
077000060502        wrkeofs01 = *off;
077100060502
077200060502       // inizio lettura subfile
077300060502        dow wrkeofs01 = *off and *in21;
077400060502         readc tb49s01;
077500060502       // se fine subfile
077600060502         if %eof;
077700060502          wrkcars01 = *on;
077800060502          leave;
077900060502         endif;
078000060502       // se è stata immessa un'opzione
078100060502         if s1opzione <> *zeros;
078200060502       // reset ds di servizio
078300060502          clear tntb49ds;
078400060502       // controllo ed elaborazione opzione immessa
078500060502          select;
078600060502       // opzione "01"=scelta
078700060502           when s1opzione = 1 and *in01;
078800060502            b49opzio = '01';
078900060502       // opzione "02"=modifica
079000060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
079100060502            b49opzio = '02';
079200060502       // opzione "03"=copia
079300060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
079400060502            b49opzio = '03';
079500060502       // opzione "04"=annullo/ripristino
079600060502           when s1opzione = 4 and not *in01;
079700060502            b49opzio = '04';
079800060502       // opzione "05"=visualizzazione
079900060502           when s1opzione = 5;
080000060502            b49opzio = '05';
080100060502          endsl;
080200010503
080300060502       // se immessa almeno un opzione valida
080400060502          if  b49opzio <> *blanks;
080500060502       // gstione del formato video
080600060504           exsr sr_d01;
080700060502       // se la gestione si è chiusa con ...
080800060504           select;
080900060502       // F6=Conferma
081000060504            when b49ricez = 'F';
081100060504             clear s1opzione;
081200060504             wrkcars01 = *on;
081300060502       // F12=Ritorno
081400060504            when b49ricez = 'L';
081500060504             clear s1opzione;
081600060504             wrkeofs01 = *on;
081700060502       // altrimenti
081800060504            other;
081900060504             *in22 = *on;
082000060504           endsl;
082100060502
082200060504           update tb49s01;
082300060504           *in22 = *off;
082400060504       // fine opzione
082500060504          endif;
082600060502       // fine opzione
082700060502         endif;
082800060502
082900060502        enddo;
083000060502
083100060502       endsr;
083200060502
083300060502       // ------------------------------------------------------------------------
083400060502       // posizionamento sul file
083500060502       // ------------------------------------------------------------------------
083600060502       begsr sr_setlltbe01;
083700060502
083800060502        select;
083900060502       // chiave totale
084000060502         when wrkkey = '1';
084100060502          setll tbecod tntbe01l;
084200060502       // chiave parziale
084300060502         when wrkkey = '2';
084400060502          setll (tbecod:tbeke1) tntbe01l;
084500060502        endsl;
084600060502
084700060502       endsr;
084800060502
084900060502       // ------------------------------------------------------------------------
085000060502       // scrivo subfile
085100060502       // ------------------------------------------------------------------------
085200060502       begsr sr_wtrs01;
085300060502
085400060502       // se non raggiunto limite massimo di caricamento
085500060505        if s01nrr < 9999;
085600060502       // imposto campi di subfile
085700060502         exsr sr_sets01;
085800060502       // imposto numeratore righe e numero relativo di record
085900060505         s01nrr = s01nrr + 1;
086000060502       // scrivo subfile
086100060502         write tb49s01;
086200060502        endif;
086300060502
086400060502       endsr;
086500060502
086600060502       // ------------------------------------------------------------------------
086700060502       // imposto campi del subfile
086800060502       // ------------------------------------------------------------------------
086900060502       begsr sr_sets01;
087000060502
087100060502        s1tbeke1 = tbeke1;
087200060505        s1tbeke2 = tbeke2;
087300060502        dcch = tbeuni;
087400060502        s1tbeuni = §cchdesc;
087500060502        s1tbeatb = tbeatb;
087600060502
087700060502       endsr;
087800060505
087900060505       // ------------------------------------------------------------------------
088000060505       // gestione tabelle in lingua
088100060505       // ------------------------------------------------------------------------
088200060505       begsr sr_lingua;
088300060505
088400060505       // pulisco il subfile
088500060505        exsr sr_pulsfl;
088600060505       // carico il subfile con i dati che trovo o vuoto
088700060505        exsr sr_carsfl;
088800060505
088900060505        wrksfl02 = *on;
089000060505
089100060505        dou wrksfl02 = *off;
089200060505         rec = 1;
089300060505         if savrec > *zeros;
089400060505          rec = savrec;
089500060505         endif;
089600060508         write tb49p02;
089700060505         exfmt tb49c02;
089800060505
089900060505         *in28 = *off;
090000060505         *in47 = *off;
090100060505
090200060505       // F12=Ritorno
090300060505          if *inkl;
090400060505           wrksfl02 = *off;
090500060508           leavesr;
090600060505          endif;
090700060505
090800060505       // Controlli
090900060505          if not *in05;
091000060505           exsr sr_ctrsfl;
091100060505          endif;
091200060505
091300060505       // F6=Conferma
091400060505          if *inkf and not *in28;
091500060508           exsr sr_confsfl;
091600060505           wrksfl02 = *off;
091700060505          endif;
091800060505
091900060505         enddo;
092000060505
092100060505       endsr;
092200060505
092300060505       // ------------------------------------------------------------------------
092400060505       // pulisco il subfile delle descrizioni in lingua
092500060505       // ------------------------------------------------------------------------
092600060505       begsr sr_pulsfl;
092700060505
092800060505        clear s02nrr;
092900060505        clear savrec;
093000060508        *in24 = *off;
093100060508        *in25 = *off;
093200060505        write tb49c02;
093300060508        *in24 = *on;
093400060508        *in25 = *on;
093500060505
093600060505       endsr;
093700060505
093800060505       // ------------------------------------------------------------------------
093900060505       // carico il subfile delle descrizioni in lingua
094000060505       // ------------------------------------------------------------------------
094100060505       begsr sr_carsfl;
094200060505
094300060505        *in30 = *off;
094400060505       // carico un record per ogni lingua inserite in AZLIN
094500060505        clear lintntbe;
094600060505        setgt lintntbe azlin02l;
094700060505        dou %eof(azlin02l);
094800060505         read azlin02l;
094900060505       // fine file
095000060505         if %eof(azlin02l);
095100060505          leave;
095200060505         endif;
095300060505         clear dcch;
095400060505         s2dlin = lindesita;
095500060505         tbecod = 'CCH';
095600060505         tbeke1 = d1tbeke1;
095700060505         tbeke2 = d1tbeke2;
095800060505         tbelin = lintntbe;
095900060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
096000060508         if not %found(tntbe11l);
096100060505          clear s2cdes;
096200060505         else;
096300060505          dcch = tbeuni;
096400060505          s2cdes = §cchdesc;
096500060505         endif;
096600060505         s2hcdlin = tbelin;
096700060505         s02nrr = s02nrr + 1;
096800060505         write tb49s02;
096900060505        enddo;
097000060505
097100060505        *in30 = *on;
097200060505
097300060505       endsr;
097400060505
097500060505       // ------------------------------------------------------------------------
097600060505       // controllo il subfile delle descrizioni in lingua
097700060505       // ------------------------------------------------------------------------
097800060505       begsr sr_ctrsfl;
097900060505
098000060505        s02nrr = 1;
098100060505        chain s02nrr tb49s02;
098200060505        dow %found;
098300060505       // deve esserci la descrizione
098400060505         if s2cdes = *blanks;
098500060505          vc2msg = msg(02);
098600060505          *in28 = *on;
098700060508          *in47 = *on;
098800060505          savrec = s02nrr;
098900060505          update tb49s02;
099000060505          leave;
099100060505         endif;
099200060505         update tb49s02;
099300060505         s02nrr = s02nrr + 1;
099400060505         chain s02nrr tb49s02;
099500060505        enddo;
099600060505
099700060505       endsr;
099800060505
099900060505       // ------------------------------------------------------------------------
100000060505       // confermo il dati del subfile delle descrizioni in lingua
100100060505       // ------------------------------------------------------------------------
100200060505       begsr sr_confsfl;
100300060505
100400060505       // recupero i dati della tabella in italiano
100500060505        clear savtbeuni;
100600060505        tbecod = 'CCH';
100700060505        tbeke1 = d1tbeke1;
100800060505        tbeke2 = d1tbeke2;
100900060505        clear tbelin;
101000060531        chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
101100060508        if %found(tntbe11l);
101200060505         savtbeuni = tbeuni;
101300060505        endif;
101400060505
101500060505       // leggo il subfile e aggiorno/scrivo i dati in lingua
101600060505        s02nrr = 1;
101700060505        chain s02nrr tb49s02;
101800060505        dow %found;
101900060505         tbelin = s2hcdlin;
102000060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
102100060505         dcch = savtbeuni;
102200060505         §cchdesc = s2cdes;
102300060505         tbeuni = dcch;
102400060508         tbeftt = w1ftt;
102500060505         clear tbeftr;
102600060505         clear tbedtr;
102700060505       // scrivo
102800060508         if not %found(tntbe11l);
102900060505          tbecod = 'CCH';
103000060505          tbeke1 = d1tbeke1;
103100060505          tbeke2 = d1tbeke2;
103200060505          tbelin = s2hcdlin;
103300060508          write tntbe11;
103400060505         else;
103500060505       // aggiorno
103600060508          update tntbe11;
103700060505         endif;
103800060505         s02nrr = s02nrr + 1;
103900060505         chain s02nrr tb49s02;
104000060505        enddo;
104100060505
104200060505       endsr;
104300060428
104400060502       // ------------------------------------------------------------------------
104500060502       // routine iniziale
104600060502       // ------------------------------------------------------------------------
104700060428         begsr *inzsr;
104800060428
104900060428      /end-free
105000060428
105100060428     c     *entry        plist
105200060428     c                   parm                    kpjba
105300060428
105400060428      /free
105500060428         in(e) §azute;
105600060428         if not %error;
105700060428          in(e) §datiute;
105800060428         endif;
105900060502         if %error or rsut = *blanks;
106000060428          tibs34r(tibs34ds);
106100060428          in §azute;
106200060428          in §datiute;
106300060428         endif;
106400060428
106500060428         endsr;
106600060502
106700060502       // ------------------------------------------------------------------------
106800060502       // uscita dal programma
106900060502       // ------------------------------------------------------------------------
107000060502         begsr sr_uscita;
107100060502
107200060502          if b49esito = *blanks;
107300060502           b49esito = eseguito;
107400060502          endif;
107500060502
107600060502          kpjbu = tntb49ds;
107700060502
107800060502          *inlr = *on;
107900060502          return;
108000060502
108100060502         endsr;
108200060502
108300060502      /end-free
108400060502
108500060502** -MSG-                                                                     *
108600060502Immettere la causale                                                           01
108700060502Immettere la descrizione                                                       02
108800060510
108900060510
109000060502Causale già esistente                                                          05
