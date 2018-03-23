000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300060704      * Gestione tabella "I15" = Causali chiusura C.A.                         *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600060508     ftntbe11l  uf a e           k disk    rename(tntbe000:tntbe11)
000700060505     fazlin02l  if   e           k disk
000800060704     ftntbi15d  cf   e             workstn sfile(tb49s01:s01nrr)
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
006600060704     d di15            ds
006700060704     D  §i15des                      25
006800060704     d kpjba         e ds
006900060503     d tibs34ds      e ds                  inz
007000060704     d tntbi15s      e ds
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
010700060704        tntbi15s = kpjbu;
010800060428
010900060428        select;
011000060502       // ricerca e scleta
011100060704         when i15Fun = '1';
011200060428          *in01 = *on;
011300060502       // manutenzione
011400060704         when  i15Fun = *blanks;
011500060428          *in01 = *off;
011600060502       // altrimenti
011700060428         other;
011800060704          i15Esito = errore;
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
015400060428          savrcdnbr = c1Rcdnbr;
015500060428
015600060502       // Emissione del subfile
015700060508          write tb49p01;
015800060428          exfmt tb49c01;
015900060428
016000060502       // controllo tasti funzionali del subfile
016100060428          select;
016200060428
016300060502       // F3=Fine
016400060428           when *inkc;
016500060502            $video = *blanks;
016600060704            i15ricez = 'C';
016700060428            exsr sr_uscita;
016800060428
016900060502       // F5=Refresh
017000060428           when *inke;
017100060428            wrkcars01 = *on;
017200060428
017300060502       // F10=Immissione
017400060428           when *inkj;
017500060502            $video = 'D01';
017600060704            clear tntbi15s;
017700060704            clear di15;
017800060704            i15comand = 'J';
017900060428            exsr sr_d01;
018000060428
018100060502       // F13=Ripetizione
018200060428           when *inkm;
018300060428            exsr sr_ripetiopz;
018400060428
018500060502       // in tutti gli altri casi
018600060428           other;
018700060502       // controllo ed elaborazione scelte su subfile
018800060428            exsr sr_gestsfl;
018900060428          endsl;
019000060428
019100060502       // fine elaborazione 'C01'
019200060428         enddo;
019300060428
019400060428        endsr;
019500060428
019600060502       // ------------------------------------------------------------------------
019700060502       // caricamento subfile
019800060502       // ------------------------------------------------------------------------
019900060428       begsr sr_cars01;
020000010430
020100060502       // se è stato richiesto il caricamento del subfile
020200060428        if wrkcars01 = *on;
020300060428
020400060502       // inizializzo il subfile
020500060505         s01nrr = 0;
020600060428         *in20 = *on;
020700060428         write tb49c01;
020800060428         *in20 = *off;
020900060428         *in21 = *off;
021000060428         *in22 = *off;
021100060428         *in23 = *off;
021200060428
021300060502       // imposto la chiave di posizionamento e lettura file
021400060704         tbecod = 'I15';
021500060428         tbeke1 = c1setll;
021600060428
021700060502       // se è stato scelto il posizionamento
021800060502         if c1setll <> *blanks;
021900060428          wrkkey = '2';
022000060502       // altrimenti
022100060428         else;
022200060428          wrkkey = '1';
022300060428         endif;
022400060428
022500060502       // posizionamento file
022600060428         exsr sr_setlltbe01;
022700060428
022800060502       // fino a che non è fine file
022900060428         dou %eof(tntbe01l);
023000060428
023100060502       // leggo file
023200060505          reade(n) tbecod tntbe01l;
023300060428
023400060502       // fine file esco
023500060428          if %eof(tntbe01l);
023600060428           leave;
023700060428          endif;
023800060428
023900060502       // se in "ricerca/scelta" non carico i records annullati
024000060502          if (*in01 and tbeatb = *blanks) or not *in01;
024100060502       // scrivo subfile
024200060428           clear s1opzione;
024300060428           exsr sr_wtrs01;
024400060428          endif;
024500060428
024600060428         enddo;
024700060428
024800060502       // fine caricamento subfile
024900060428        endif;
025000060428
025100060502       // se scritto almeno un record
025200060505        if s01nrr > 0;
025300060502       // indicatore di sflend
025400060428         eval *in21 = *on;
025500060428        endif;
025600060428
025700060502       // se non scritto neanche un record
025800060505        if s01nrr = 0;
025900060502       // indicatore di sfldsp
026000060428         eval *in23 = *on;
026100060428        endif;
026200060428
026300060502       // disattivo opzione di caricamento subfile
026400060428        wrkcars01 = *off;
026500060428
026600060428       endsr;
026700060428
026800060502       // ------------------------------------------------------------------------
026900060502       // gestione videata
027000060502       // ------------------------------------------------------------------------
027100060428       begsr sr_d01;
027200060428
027300060504       // imposto il video a seconda della funzione richiesta
027400060428        exsr sr_setvideo;
027500060428
027600060502       // imposto variabile
027700060428        $video = 'D01';
027800060428
027900060502       // fino a che la variabile resta settata come 'D01'
028000060428        dou $video <> 'D01';
028100060428
028200060508         *in02 = *off;
028300060508         *in03 = *off;
028400060509         *in04 = *off;
028500060428
028600060502       // se immessa opzione di 'scelta'
028700060704         if i15opzio = '01';
028800060704          i15ke1 = s1tbeke1;
028900060704          i15des = s1tbeuni;
029000060428          exsr sr_uscita;
029100060428
029200060508       // se immessa un'altra opzione
029300060508         else;
029400060508       // se non è immissione/copia proteggo il campo della causale
029500060704          if i15comand <> 'J' and i15opzio <> '03';
029600060428           *in02 = *on;
029700060428          endif;
029800060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
029900060508       // proteggo i campi del video
030000060704          if i15opzio = '04' or i15opzio = '05';
030100060508           *in03 = *on;
030200060508          endif;
030300060509       // se immessa opzione di 'visualizzazione'
030400060509       // non attivo F6
030500060704          if i15opzio = '05';
030600060509           *in04 = *on;
030700060509          endif;
030800060502       // emetto il video
030900060428          exfmt tb49d01;
031000060428         endif;
031100060428
031200060502       // reset indicatore generico di errore
031300060428         *in28 = *off;
031400060508
031500060508       // pulisco il campo messaggi
031600060508         clear vd1msg;
031700010430
031800060502       // controllo tasti funzionali del video
031900060428         select;
032000060428
032100060502       // F3=Fine
032200060428          when *inkc;
032300060704           i15ricez = 'C';
032400060502           $video = *blanks;
032500060505           unlock tntbe01l;
032600060428           exsr sr_uscita;
032700060428
032800060502       // F6=Conferma
032900060428          when *inkf;
033000060704           i15ricez = 'F';
033100060502       // controllo e decodifico i dati del video
033200060428           exsr sr_ctrd01;
033300060502       // conferma per annullo/ripristino
033400060704           if i15opzio = '04';
033500060428            select;
033600060502       // annullamento
033700060502             when tbeatb = *blanks;
033800060428              tbeatb = 'A';
033900060502       // ripristino
034000060502             when tbeatb = 'A';
034100060428              clear tbeatb;
034200060428            endsl;
034300060428           endif;
034400060428
034500060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
034600060428           if not *in28;
034700060504            wrkcarw01 = *on;
034800060504            $video = 'W01';
034900060504            exsr sr_w01;
035000060428           endif;
035100060428
035200060502       // F8=Record successivo
035300060428          when  *inkh;
035400060428           clear s1opzione;
035500060428           wrkcars01 = *off;
035600060428           $video = 'C01';
035700060704           i15ricez = 'H';
035800060505
035900060505       // F9=Traduzione
036000060505          when  *inki;
036100060505           *in05 = *on;
036200060505           exsr sr_lingua;
036300060505           *in05 = *off;
036400060428
036500060502       // F12=Ritorno
036600060428          when *inkl;
036700060428           clear s1opzione;
036800060704           i15ricez = 'L';
036900060505           unlock tntbe01l;
037000060508       // se non è f12 da immissione/copia non ricarico il subfile
037100060704           if i15comand = 'J' or i15opzio = '03';
037200060428            wrkcars01 = *on;
037300060428           else;
037400060428            wrkcars01 = *off;
037500060428           endif;
037600060428           $video = 'C01';
037700060428
037800060502       // Invio
037900060428          other;
038000060508           if not *in03;
038100060508            exsr sr_ctrd01;
038200060508           endif;
038300060428
038400060502         endsl;
038500060428
038600060502       // fine gestione 'D01'
038700060428        enddo;
038800060428
038900060428       endsr;
039000060502
039100060502       // ------------------------------------------------------------------------
039200060502       // ripeto opzione in tutte le righe del subfile
039300060502       // ------------------------------------------------------------------------
039400060428       begsr sr_ripetiopz;
039500060428
039600060428        if c1csrrrn > 0;
039700060505         s01nrr = c1csrrrn;
039800060505         chain s01nrr tb49s01;
039900060428         if %found and s1opzione > 0;
040000060428          savopzione = s1opzione;
040100060428          *in22 = *on;
040200060428          update tb49s01;
040300060428
040400060428          wrkeofs01 = *off;
040500060428          dou wrkeofs01 = *on;
040600060505           s01nrr = s01nrr + 1;
040700060505           chain s01nrr tb49s01;
040800060428           if %found;
040900060428            s1opzione = savopzione;
041000060428            update tb49s01;
041100060428           else;
041200060428            wrkeofs01 = *on;
041300060428           endif;
041400060428          enddo;
041500060428
041600060428          *in22 = *off;
041700060428
041800060428         endif;
041900060428
042000060428        endif;
042100060428
042200060428       endsr;
042300060428
042400060502       // ------------------------------------------------------------------------
042500060502       // imposto i dati a video
042600060502       // ------------------------------------------------------------------------
042700060428       begsr sr_setvideo;
042800060428
042900060502       // pulisco il formato video D01
043000060502        exsr sr_puld01;
043100060428
043200060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
043300060428        select;
043400060428
043500060502       // F10=Immissione
043600060704         when i15comand = 'J';
043700060428          d1descopz = 'Immissione';
043800060428
043900060502       // Opzione "02"=modifica
044000060704         when i15opzio = '02';
044100060428          d1descopz = 'Modifica';
044200060428          exsr sr_imposta;
044300060428
044400060502       // Opzione "03"=copia
044500060704         when i15opzio = '03';
044600060428          d1descopz = 'Copia';
044700060428          exsr sr_imposta;
044800060428
044900060502       // Opzione "04"=cancellazione/ripristino
045000060704         when i15opzio = '04';
045100060428          exsr sr_imposta;
045200060502       // se richiesta 'cancellazione'
045300060502          if tbeatb = *blanks;
045400060502           d1descopz = 'Annullamento';
045500060502          endif;
045600060502       // se richiesto 'ripristino'
045700060502          if tbeatb = 'A';
045800060502           d1descopz = 'Ripristino';
045900060502          endif;
046000060502
046100060502       // Opzione "05"=visualizzazione
046200060704         when i15Opzio ='05';
046300060502          d1descopz = 'Visualizzazione';
046400060502          exsr sr_imposta;
046500060502
046600060502       // Fine scelta
046700060502        endsl;
046800060502
046900060502       // centro la descrizione della funzione nel formato video
047000060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
047100060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
047200060502
047300060502       endsr;
047400060502
047500060502       // ------------------------------------------------------------------------
047600060502       // controllo video
047700060502       // ------------------------------------------------------------------------
047800060502       begsr sr_ctrd01;
047900060502
048000060502        *in28 = *off;
048100060502        *in40 = *off;
048200060502        *in41 = *off;
048300060508
048400060508       // se immissione controllo che non esista già
048500060704        if i15comand = 'J' or i15opzio = '03';
048600060704         tbecod = 'I15';
048700060508         tbeke1 = d1tbeke1;
048800060508         tbeke2 = d1tbeke2;
048900060508         clear tbelin;
049000060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
049100060508         if %found(tntbe01l);
049200060508          vd1msg = msg(05);
049300060508          *in28 = *on;
049400060508          *in40 = *on;
049500060508          leavesr;
049600060508         endif;
049700060508        endif;
049800060502
049900060502       // descrizione
050000060704        if d1i15des = *blanks;
050100060502         vd1msg = msg(02);
050200060502         *in28 = *on;
050300060502         *in41 = *on;
050400060502         leavesr;
050500060502        endif;
050600060502
050700060502       endsr;
050800060504
050900060504       // ------------------------------------------------------------------------
051000060504       // gestione video dati di trasmissione
051100060504       // ------------------------------------------------------------------------
051200060504       begsr sr_w01;
051300060504
051400060504       // imposto i dati a video
051500060504        exsr sr_carw01;
051600060504
051700060505       // fino a che la variabile resta settata come 'W01'
051800060504        dou $video <> 'W01';
051900060504
052000060504       // reset indicatore generico di errore
052100060504         *in28 = *off;
052200060504
052300060504       // emetto il video
052400060504          exfmt tb49w01;
052500060504
052600060504       // controllo tasti funzionali del video
052700060504         select;
052800060504
052900060504       // F6=Conferma
053000060504          when *inkf;
053100060504       // controllo i dati del video
053200060504           exsr sr_ctrw01;
053300060504       // se non riscontrati errori aggiorno il record corrente
053400060504           if not *in28;
053500060504            exsr sr_aggiorna;
053600060504            if not *in28;
053700060505       // emetto le descrizioni in lingua
053800060509       // se non è annullo/ripristino
053900060704             if i15opzio <> '04';
054000060509              *in05 = *off;
054100060509              exsr sr_lingua;
054200060509             endif;
054300060704             if i15comand = 'J';
054400060504              $video = 'D01';
054500060504              exsr sr_setvideo;
054600060504             else;
054700060504              $video = 'C01';
054800060504             endif;
054900060504            endif;
055000060504           endif;
055100060504
055200060504       // F12=Ritorno
055300060504          when *inkl;
055400060504           $video = 'D01';
055500060704           clear i15ricez;
055600060504
055700060504       // Invio
055800060504          other;
055900060504           exsr sr_ctrw01;
056000060504
056100060504         endsl;
056200060504
056300060504       // fine gestione 'W01'
056400060504        enddo;
056500060504
056600060504       endsr;
056700060504
056800060504       // ------------------------------------------------------------------------
056900060504       // imposto i dati di trasmissione
057000060504       // ------------------------------------------------------------------------
057100060504       begsr sr_carw01;
057200060504
057300060504       // se pulisco i campi
057400060504         clear w1ftt;
057500060504         clear w1flt;
057600060504         clear w1ftr;
057700060504         clear w1dtr;
057800060504
057900060504       // se non immissione imposto i campi del file
058000060704        if i15comand <> 'J';
058100060504         w1ftt = tbeftt;
058200060504         w1flt = tbeflt;
058300060504         w1ftr = tbeftr;
058400060504       // imposto la data
058500060504         if tbedtr <> *zeros;
058600060504          clear wlbdat;
058700060504          g02inv = tbedtr;
058800060504          g02err = '3';
058900060504          xsrda8(wlbdat);
059000060504          w1dtr = g02dat;
059100060504         endif;
059200060504
059300060504        endif;
059400060504
059500060504       endsr;
059600060504
059700060504       // ------------------------------------------------------------------------
059800060504       // controllo i dati di trasmissione
059900060504       // ------------------------------------------------------------------------
060000060504       begsr sr_ctrw01;
060100060504
060200060504       endsr;
060300060502
060400060502       // ------------------------------------------------------------------------
060500060502       // aggiorno tabella
060600060502       // ------------------------------------------------------------------------
060700060502       begsr sr_aggiorna;
060800060502
060900060502       // imposto campi del file
061000060509        clear tbelin;
061100060502        tbeke1 = d1tbeke1;
061200060509        tbeke2 = d1tbeke2;
061300060502
061400060704        §i15des = d1i15des;
061500060502
061600060704        tbeuni = di15;
061700060502
061800060504        tbeftt = w1ftt;
061900060504        tbeflt = w1flt;
062000060502        clear tbeftr;
062100060502        clear tbedtr;
062200060502
062300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
062400060502        select;
062500060502
062600060502       // F10=immissione - Opzione "03"=copia
062700060704         when i15comand = 'J' or i15opzio = '03';
062800060502       // scrivo nuovo record con gestione errore per chiave duplicata
062900060508          write tntbe000;
063000060502
063100060502       // Opzione "02"=modifica
063200060704         when i15opzio = '02';
063300060502       // update record e controllo errore per chiave duplicata
063400060508          update tntbe000;
063500060502
063600060502       // Opzione "04"=cancellazione/ripristino
063700060704         when i15opzio = '04';
063800060502          update tntbe000;
063900010430
064000060502        endsl;
064100060509
064200060509       // se Opzione "04"=cancellazione/ripristino
064300060509       // aggiorno automaticamente anche le descrizioni in linea
064400060704        if i15opzio = '04';
064500060509         clear lintntbe;
064600060509         setgt lintntbe azlin02l;
064700060509         dou %eof(azlin02l);
064800060509          read azlin02l;
064900060509       // fine file
065000060509          if %eof(azlin02l);
065100060509           leave;
065200060509          endif;
065300060509       // aggiorno
065400060509          tbelin = lintntbe;
065500060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
065600060509          if %found(tntbe11l);
065700060509           select;
065800060509            when tbeatb <> *blanks;
065900060509             clear tbeatb;
066000060509            when tbeatb = *blanks;
066100060509             tbeatb = 'A';
066200060509           endsl;
066300060509            update tntbe11;
066400060509          endif;
066500060509         enddo;
066600060509        endif;
066700060502
066800060502       endsr;
066900060502
067000060502       // ------------------------------------------------------------------------
067100060502       // pulizia video
067200060502       // ------------------------------------------------------------------------
067300060502       begsr sr_puld01;
067400060502
067500060504        clear d1descopz;
067600060504        clear d1tbeke1;
067700060704        clear d1i15des;
067800060504
067900060504        *in28 = *off;
068000060504        *in40 = *off;
068100060504        *in41 = *off;
068200060502
068300060502       endsr;
068400060502
068500060502       // ------------------------------------------------------------------------
068600060502       // imposto i dati a video
068700060502       // ------------------------------------------------------------------------
068800060502       begsr sr_imposta;
068900060502
069000060502       // recupero i dati dal file
069100060505        ktbeke1 = s1tbeke1;
069200060508        clear tbelin;
069300060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
069400060502
069500060502       // imposto i campi a video
069600060505        d1tbeke2 = tbeke2;
069700060502        d1tbeke1 = tbeke1;
069800060704        di15 = tbeuni;
069900060704        d1i15des = §i15des;
070000060502
070100060502       endsr;
070200060502
070300060502       // ------------------------------------------------------------------------
070400060502       // gestione del subfile
070500060502       // ------------------------------------------------------------------------
070600060502       begsr sr_gestsfl;
070700060502
070800060502       // set flag
070900060502        wrkeofs01 = *off;
071000060502
071100060502       // inizio lettura subfile
071200060502        dow wrkeofs01 = *off and *in21;
071300060502         readc tb49s01;
071400060502       // se fine subfile
071500060502         if %eof;
071600060502          wrkcars01 = *on;
071700060502          leave;
071800060502         endif;
071900060502       // se è stata immessa un'opzione
072000060502         if s1opzione <> *zeros;
072100060502       // reset ds di servizio
072200060704          clear tntbi15s;
072300060502       // controllo ed elaborazione opzione immessa
072400060502          select;
072500060502       // opzione "01"=scelta
072600060502           when s1opzione = 1 and *in01;
072700060704            i15opzio = '01';
072800060502       // opzione "02"=modifica
072900060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
073000060704            i15opzio = '02';
073100060502       // opzione "03"=copia
073200060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
073300060704            i15opzio = '03';
073400060502       // opzione "04"=annullo/ripristino
073500060502           when s1opzione = 4 and not *in01;
073600060704            i15opzio = '04';
073700060502       // opzione "05"=visualizzazione
073800060502           when s1opzione = 5;
073900060704            i15opzio = '05';
074000060502          endsl;
074100010503
074200060502       // se immessa almeno un opzione valida
074300060704          if  i15opzio <> *blanks;
074400060502       // gstione del formato video
074500060504           exsr sr_d01;
074600060502       // se la gestione si è chiusa con ...
074700060504           select;
074800060502       // F6=Conferma
074900060704            when i15ricez = 'F';
075000060504             clear s1opzione;
075100060504             wrkcars01 = *on;
075200060502       // F12=Ritorno
075300060704            when i15ricez = 'L';
075400060504             clear s1opzione;
075500060504             wrkeofs01 = *on;
075600060502       // altrimenti
075700060504            other;
075800060504             *in22 = *on;
075900060504           endsl;
076000060502
076100060504           update tb49s01;
076200060504           *in22 = *off;
076300060504       // fine opzione
076400060504          endif;
076500060502       // fine opzione
076600060502         endif;
076700060502
076800060502        enddo;
076900060502
077000060502       endsr;
077100060502
077200060502       // ------------------------------------------------------------------------
077300060502       // posizionamento sul file
077400060502       // ------------------------------------------------------------------------
077500060502       begsr sr_setlltbe01;
077600060502
077700060502        select;
077800060502       // chiave totale
077900060502         when wrkkey = '1';
078000060502          setll tbecod tntbe01l;
078100060502       // chiave parziale
078200060502         when wrkkey = '2';
078300060502          setll (tbecod:tbeke1) tntbe01l;
078400060502        endsl;
078500060502
078600060502       endsr;
078700060502
078800060502       // ------------------------------------------------------------------------
078900060502       // scrivo subfile
079000060502       // ------------------------------------------------------------------------
079100060502       begsr sr_wtrs01;
079200060502
079300060502       // se non raggiunto limite massimo di caricamento
079400060505        if s01nrr < 9999;
079500060502       // imposto campi di subfile
079600060502         exsr sr_sets01;
079700060502       // imposto numeratore righe e numero relativo di record
079800060505         s01nrr = s01nrr + 1;
079900060502       // scrivo subfile
080000060502         write tb49s01;
080100060502        endif;
080200060502
080300060502       endsr;
080400060502
080500060502       // ------------------------------------------------------------------------
080600060502       // imposto campi del subfile
080700060502       // ------------------------------------------------------------------------
080800060502       begsr sr_sets01;
080900060502
081000060502        s1tbeke1 = tbeke1;
081100060505        s1tbeke2 = tbeke2;
081200060704        di15 = tbeuni;
081300060704        s1tbeuni = §i15des;
081400060502        s1tbeatb = tbeatb;
081500060502
081600060502       endsr;
081700060505
081800060505       // ------------------------------------------------------------------------
081900060505       // gestione tabelle in lingua
082000060505       // ------------------------------------------------------------------------
082100060505       begsr sr_lingua;
082200060505
082300060505       // pulisco il subfile
082400060505        exsr sr_pulsfl;
082500060505       // carico il subfile con i dati che trovo o vuoto
082600060505        exsr sr_carsfl;
082700060505
082800060505        wrksfl02 = *on;
082900060505
083000060505        dou wrksfl02 = *off;
083100060505         rec = 1;
083200060505         if savrec > *zeros;
083300060505          rec = savrec;
083400060505         endif;
083500060508         write tb49p02;
083600060505         exfmt tb49c02;
083700060505
083800060505         *in28 = *off;
083900060505         *in47 = *off;
084000060505
084100060505       // F12=Ritorno
084200060505          if *inkl;
084300060505           wrksfl02 = *off;
084400060508           leavesr;
084500060505          endif;
084600060505
084700060505       // Controlli
084800060505          if not *in05;
084900060505           exsr sr_ctrsfl;
085000060505          endif;
085100060505
085200060505       // F6=Conferma
085300060505          if *inkf and not *in28;
085400060508           exsr sr_confsfl;
085500060505           wrksfl02 = *off;
085600060505          endif;
085700060505
085800060505         enddo;
085900060505
086000060505       endsr;
086100060505
086200060505       // ------------------------------------------------------------------------
086300060505       // pulisco il subfile delle descrizioni in lingua
086400060505       // ------------------------------------------------------------------------
086500060505       begsr sr_pulsfl;
086600060505
086700060505        clear s02nrr;
086800060505        clear savrec;
086900060508        *in24 = *off;
087000060508        *in25 = *off;
087100060505        write tb49c02;
087200060508        *in24 = *on;
087300060508        *in25 = *on;
087400060505
087500060505       endsr;
087600060505
087700060505       // ------------------------------------------------------------------------
087800060505       // carico il subfile delle descrizioni in lingua
087900060505       // ------------------------------------------------------------------------
088000060505       begsr sr_carsfl;
088100060505
088200060505        *in30 = *off;
088300060505       // carico un record per ogni lingua inserite in AZLIN
088400060505        clear lintntbe;
088500060505        setgt lintntbe azlin02l;
088600060505        dou %eof(azlin02l);
088700060505         read azlin02l;
088800060505       // fine file
088900060505         if %eof(azlin02l);
089000060505          leave;
089100060505         endif;
089200060704         clear di15;
089300060505         s2dlin = lindesita;
089400060704         tbecod = 'I15';
089500060505         tbeke1 = d1tbeke1;
089600060505         tbeke2 = d1tbeke2;
089700060505         tbelin = lintntbe;
089800060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
089900060508         if not %found(tntbe11l);
090000060704          clear s2cdes;
090100060505         else;
090200060704          di15 = tbeuni;
090300060704          s2cdes = §i15des;
090400060505         endif;
090500060505         s2hcdlin = tbelin;
090600060505         s02nrr = s02nrr + 1;
090700060505         write tb49s02;
090800060505        enddo;
090900060505
091000060505        *in30 = *on;
091100060505
091200060505       endsr;
091300060505
091400060505       // ------------------------------------------------------------------------
091500060505       // controllo il subfile delle descrizioni in lingua
091600060505       // ------------------------------------------------------------------------
091700060505       begsr sr_ctrsfl;
091800060505
091900060505        s02nrr = 1;
092000060505        chain s02nrr tb49s02;
092100060505        dow %found;
092200060505       // deve esserci la descrizione
092300060704         if s2cdes = *blanks;
092400060505          vc2msg = msg(02);
092500060505          *in28 = *on;
092600060508          *in47 = *on;
092700060505          savrec = s02nrr;
092800060505          update tb49s02;
092900060505          leave;
093000060505         endif;
093100060505         update tb49s02;
093200060505         s02nrr = s02nrr + 1;
093300060505         chain s02nrr tb49s02;
093400060505        enddo;
093500060505
093600060505       endsr;
093700060505
093800060505       // ------------------------------------------------------------------------
093900060505       // confermo il dati del subfile delle descrizioni in lingua
094000060505       // ------------------------------------------------------------------------
094100060505       begsr sr_confsfl;
094200060505
094300060505       // recupero i dati della tabella in italiano
094400060505        clear savtbeuni;
094500060704        tbecod = 'I15';
094600060505        tbeke1 = d1tbeke1;
094700060505        tbeke2 = d1tbeke2;
094800060505        clear tbelin;
094900060531        chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
095000060508        if %found(tntbe11l);
095100060505         savtbeuni = tbeuni;
095200060505        endif;
095300060505
095400060505       // leggo il subfile e aggiorno/scrivo i dati in lingua
095500060505        s02nrr = 1;
095600060505        chain s02nrr tb49s02;
095700060505        dow %found;
095800060505         tbelin = s2hcdlin;
095900060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
096000060704         di15 = savtbeuni;
096100060704         §i15des = s2cdes;
096200060704         tbeuni = di15;
096300060508         tbeftt = w1ftt;
096400060505         clear tbeftr;
096500060505         clear tbedtr;
096600060505       // scrivo
096700060508         if not %found(tntbe11l);
096800060704          tbecod = 'I15';
096900060505          tbeke1 = d1tbeke1;
097000060505          tbeke2 = d1tbeke2;
097100060505          tbelin = s2hcdlin;
097200060508          write tntbe11;
097300060505         else;
097400060505       // aggiorno
097500060508          update tntbe11;
097600060505         endif;
097700060505         s02nrr = s02nrr + 1;
097800060505         chain s02nrr tb49s02;
097900060505        enddo;
098000060505
098100060505       endsr;
098200060428
098300060502       // ------------------------------------------------------------------------
098400060502       // routine iniziale
098500060502       // ------------------------------------------------------------------------
098600060428         begsr *inzsr;
098700060428
098800060428      /end-free
098900060428
099000060428     c     *entry        plist
099100060428     c                   parm                    kpjba
099200060428
099300060428      /free
099400060428         in(e) §azute;
099500060428         if not %error;
099600060428          in(e) §datiute;
099700060428         endif;
099800060502         if %error or rsut = *blanks;
099900060428          tibs34r(tibs34ds);
100000060428          in §azute;
100100060428          in §datiute;
100200060428         endif;
100300060428
100400060428         endsr;
100500060502
100600060502       // ------------------------------------------------------------------------
100700060502       // uscita dal programma
100800060502       // ------------------------------------------------------------------------
100900060502         begsr sr_uscita;
101000060502
101100060704          if i15esito = *blanks;
101200060704           i15esito = eseguito;
101300060502          endif;
101400060502
101500060704          kpjbu = tntbi15s;
101600060502
101700060502          *inlr = *on;
101800060502          return;
101900060502
102000060502         endsr;
102100060502
102200060502      /end-free
102300060502
102400060502** -MSG-                                                                     *
102500060502Immettere la causale                                                           01
102600060502Immettere la descrizione                                                       02
102700060510
102800060510
102900060502Causale già esistente                                                          05
