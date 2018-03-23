000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300060703      * Gestione tabella "ICE" = Causali chiusura C.A.                         *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600060508     ftntbe11l  uf a e           k disk    rename(tntbe000:tntbe11)
000700060505     fazlin02l  if   e           k disk
000800060703     ftntbiced  cf   e             workstn sfile(tb49s01:s01nrr)
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
006600060703     d dice          e ds
006700060502     d kpjba         e ds
006800060503     d tibs34ds      e ds                  inz
006900060703     d tntbices      e ds
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
010600060703        tntbices = kpjbu;
010700060428
010800060428        select;
010900060502       // ricerca e scleta
011000060703         when iceFun = '1';
011100060428          *in01 = *on;
011200060502       // manutenzione
011300060703         when  iceFun = *blanks;
011400060428          *in01 = *off;
011500060502       // altrimenti
011600060428         other;
011700060703          iceEsito = errore;
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
016500060703            icericez = 'C';
016600060428            exsr sr_uscita;
016700060428
016800060502       // F5=Refresh
016900060428           when *inke;
017000060428            wrkcars01 = *on;
017100060428
017200060502       // F10=Immissione
017300060428           when *inkj;
017400060502            $video = 'D01';
017500060703            clear tntbices;
017501060703            clear dice;
017600060703            icecomand = 'J';
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
021200060703         tbecod = 'ICE';
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
028500060703         if iceopzio = '01';
028600060703          iceke1 = s1tbeke1;
028700060703          icedes = s1tbeuni;
028800060428          exsr sr_uscita;
028900060428
029000060508       // se immessa un'altra opzione
029100060508         else;
029200060508       // se non è immissione/copia proteggo il campo della causale
029300060703          if icecomand <> 'J' and iceopzio <> '03';
029400060428           *in02 = *on;
029500060428          endif;
029600060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
029700060508       // proteggo i campi del video
029800060703          if iceopzio = '04' or iceopzio = '05';
029900060508           *in03 = *on;
030000060508          endif;
030100060509       // se immessa opzione di 'visualizzazione'
030200060509       // non attivo F6
030300060703          if iceopzio = '05';
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
032100060703           icericez = 'C';
032200060502           $video = *blanks;
032300060505           unlock tntbe01l;
032400060428           exsr sr_uscita;
032500060428
032600060502       // F6=Conferma
032700060428          when *inkf;
032800060703           icericez = 'F';
032900060502       // controllo e decodifico i dati del video
033000060428           exsr sr_ctrd01;
033100060502       // conferma per annullo/ripristino
033200060703           if iceopzio = '04';
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
035500060703           icericez = 'H';
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
036600060703           icericez = 'L';
036700060505           unlock tntbe01l;
036800060508       // se non è f12 da immissione/copia non ricarico il subfile
036900060703           if icecomand = 'J' or iceopzio = '03';
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
043400060703         when icecomand = 'J';
043500060428          d1descopz = 'Immissione';
043600060428
043700060502       // Opzione "02"=modifica
043800060703         when iceopzio = '02';
043900060428          d1descopz = 'Modifica';
044000060428          exsr sr_imposta;
044100060428
044200060502       // Opzione "03"=copia
044300060703         when iceopzio = '03';
044400060428          d1descopz = 'Copia';
044500060428          exsr sr_imposta;
044600060428
044700060502       // Opzione "04"=cancellazione/ripristino
044800060703         when iceopzio = '04';
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
046000060703         when iceOpzio ='05';
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
049100060703        if icecomand = 'J' or iceopzio = '03';
049200060703         tbecod = 'ICE';
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
050600060703        if d1icedei = *blanks;
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
054500060703             if iceopzio <> '04';
054600060509              *in05 = *off;
054700060509              exsr sr_lingua;
054800060509             endif;
054900060703             if icecomand = 'J';
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
056100060703           clear icericez;
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
058600060703        if icecomand <> 'J';
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
062000060703        §icedei = d1icedei;
062001060703        §icedep = d1icedep;
062100060502
062200060703        tbeuni = dice;
062300060502
062400060504        tbeftt = w1ftt;
062500060504        tbeflt = w1flt;
062600060502        clear tbeftr;
062700060502        clear tbedtr;
062800060502
062900060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
063000060502        select;
063100060502
063200060502       // F10=immissione - Opzione "03"=copia
063300060703         when icecomand = 'J' or iceopzio = '03';
063400060502       // scrivo nuovo record con gestione errore per chiave duplicata
063500060508          write tntbe000;
063600060502
063700060502       // Opzione "02"=modifica
063800060703         when iceopzio = '02';
063900060502       // update record e controllo errore per chiave duplicata
064000060508          update tntbe000;
064100060502
064200060502       // Opzione "04"=cancellazione/ripristino
064300060703         when iceopzio = '04';
064400060502          update tntbe000;
064500010430
064600060502        endsl;
064700060509
064800060509       // se Opzione "04"=cancellazione/ripristino
064900060509       // aggiorno automaticamente anche le descrizioni in linea
065000060703        if iceopzio = '04';
065100060509         clear lintntbe;
065200060509         setgt lintntbe azlin02l;
065300060509         dou %eof(azlin02l);
065400060509          read azlin02l;
065500060509       // fine file
065600060509          if %eof(azlin02l);
065700060509           leave;
065800060509          endif;
065900060509       // aggiorno
066000060509          tbelin = lintntbe;
066100060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
066200060509          if %found(tntbe11l);
066300060509           select;
066400060509            when tbeatb <> *blanks;
066500060509             clear tbeatb;
066600060509            when tbeatb = *blanks;
066700060509             tbeatb = 'A';
066800060509           endsl;
066900060509            update tntbe11;
067000060509          endif;
067100060509         enddo;
067200060509        endif;
067300060502
067400060502       endsr;
067500060502
067600060502       // ------------------------------------------------------------------------
067700060502       // pulizia video
067800060502       // ------------------------------------------------------------------------
067900060502       begsr sr_puld01;
068000060502
068100060504        clear d1descopz;
068200060504        clear d1tbeke1;
068300060703        clear d1icedei;
068301060703        clear d1icedep;
068400060504
068500060504        *in28 = *off;
068600060504        *in40 = *off;
068700060504        *in41 = *off;
068800060502
068900060502       endsr;
069000060502
069100060502       // ------------------------------------------------------------------------
069200060502       // imposto i dati a video
069300060502       // ------------------------------------------------------------------------
069400060502       begsr sr_imposta;
069500060502
069600060502       // recupero i dati dal file
069700060505        ktbeke1 = s1tbeke1;
069800060508        clear tbelin;
069900060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
070000060502
070100060502       // imposto i campi a video
070200060505        d1tbeke2 = tbeke2;
070300060502        d1tbeke1 = tbeke1;
070400060703        dice = tbeuni;
070500060703        d1icedei = §icedei;
070501060703        d1icedep = §icedep;
070600060502
070700060502       endsr;
070800060502
070900060502       // ------------------------------------------------------------------------
071000060502       // gestione del subfile
071100060502       // ------------------------------------------------------------------------
071200060502       begsr sr_gestsfl;
071300060502
071400060502       // set flag
071500060502        wrkeofs01 = *off;
071600060502
071700060502       // inizio lettura subfile
071800060502        dow wrkeofs01 = *off and *in21;
071900060502         readc tb49s01;
072000060502       // se fine subfile
072100060502         if %eof;
072200060502          wrkcars01 = *on;
072300060502          leave;
072400060502         endif;
072500060502       // se è stata immessa un'opzione
072600060502         if s1opzione <> *zeros;
072700060502       // reset ds di servizio
072800060703          clear tntbices;
072900060502       // controllo ed elaborazione opzione immessa
073000060502          select;
073100060502       // opzione "01"=scelta
073200060502           when s1opzione = 1 and *in01;
073300060703            iceopzio = '01';
073400060502       // opzione "02"=modifica
073500060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
073600060703            iceopzio = '02';
073700060502       // opzione "03"=copia
073800060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
073900060703            iceopzio = '03';
074000060502       // opzione "04"=annullo/ripristino
074100060502           when s1opzione = 4 and not *in01;
074200060703            iceopzio = '04';
074300060502       // opzione "05"=visualizzazione
074400060502           when s1opzione = 5;
074500060703            iceopzio = '05';
074600060502          endsl;
074700010503
074800060502       // se immessa almeno un opzione valida
074900060703          if  iceopzio <> *blanks;
075000060502       // gstione del formato video
075100060504           exsr sr_d01;
075200060502       // se la gestione si è chiusa con ...
075300060504           select;
075400060502       // F6=Conferma
075500060703            when icericez = 'F';
075600060504             clear s1opzione;
075700060504             wrkcars01 = *on;
075800060502       // F12=Ritorno
075900060703            when icericez = 'L';
076000060504             clear s1opzione;
076100060504             wrkeofs01 = *on;
076200060502       // altrimenti
076300060504            other;
076400060504             *in22 = *on;
076500060504           endsl;
076600060502
076700060504           update tb49s01;
076800060504           *in22 = *off;
076900060504       // fine opzione
077000060504          endif;
077100060502       // fine opzione
077200060502         endif;
077300060502
077400060502        enddo;
077500060502
077600060502       endsr;
077700060502
077800060502       // ------------------------------------------------------------------------
077900060502       // posizionamento sul file
078000060502       // ------------------------------------------------------------------------
078100060502       begsr sr_setlltbe01;
078200060502
078300060502        select;
078400060502       // chiave totale
078500060502         when wrkkey = '1';
078600060502          setll tbecod tntbe01l;
078700060502       // chiave parziale
078800060502         when wrkkey = '2';
078900060502          setll (tbecod:tbeke1) tntbe01l;
079000060502        endsl;
079100060502
079200060502       endsr;
079300060502
079400060502       // ------------------------------------------------------------------------
079500060502       // scrivo subfile
079600060502       // ------------------------------------------------------------------------
079700060502       begsr sr_wtrs01;
079800060502
079900060502       // se non raggiunto limite massimo di caricamento
080000060505        if s01nrr < 9999;
080100060502       // imposto campi di subfile
080200060502         exsr sr_sets01;
080300060502       // imposto numeratore righe e numero relativo di record
080400060505         s01nrr = s01nrr + 1;
080500060502       // scrivo subfile
080600060502         write tb49s01;
080700060502        endif;
080800060502
080900060502       endsr;
081000060502
081100060502       // ------------------------------------------------------------------------
081200060502       // imposto campi del subfile
081300060502       // ------------------------------------------------------------------------
081400060502       begsr sr_sets01;
081500060502
081600060502        s1tbeke1 = tbeke1;
081700060505        s1tbeke2 = tbeke2;
081800060703        dice = tbeuni;
081900060703        s1tbeuni = §icedei;
082000060502        s1tbeatb = tbeatb;
082100060502
082200060502       endsr;
082300060505
082400060505       // ------------------------------------------------------------------------
082500060505       // gestione tabelle in lingua
082600060505       // ------------------------------------------------------------------------
082700060505       begsr sr_lingua;
082800060505
082900060509       // per prima cosa devo creare la testata in lingua
083000060505       // se non esistono già
083100060505        if not *in05;
083200060505         exsr sr_teslin;
083300060505        endif;
083400060505       // pulisco il subfile
083500060505        exsr sr_pulsfl;
083600060505       // carico il subfile con i dati che trovo o vuoto
083700060505        exsr sr_carsfl;
083800060505
083900060505        wrksfl02 = *on;
084000060505
084100060505        dou wrksfl02 = *off;
084200060505         rec = 1;
084300060505         if savrec > *zeros;
084400060505          rec = savrec;
084500060505         endif;
084600060508         write tb49p02;
084700060505         exfmt tb49c02;
084800060505
084900060505         *in28 = *off;
085000060505         *in47 = *off;
085100060505
085200060505       // F12=Ritorno
085300060505          if *inkl;
085400060505           wrksfl02 = *off;
085500060508           leavesr;
085600060505          endif;
085700060505
085800060505       // Controlli
085900060505          if not *in05;
086000060505           exsr sr_ctrsfl;
086100060505          endif;
086200060505
086300060505       // F6=Conferma
086400060505          if *inkf and not *in28;
086500060508           exsr sr_confsfl;
086600060505           wrksfl02 = *off;
086700060505          endif;
086800060505
086900060505         enddo;
087000060505
087100060505       endsr;
087200060505
087300060505       // ------------------------------------------------------------------------
087400060505       // scrivo le testate in lingua
087500060505       // ------------------------------------------------------------------------
087600060505       begsr sr_teslin;
087700060505
087800060505       // recupero i dati dalla testata in italiano
087900060505        clear savtbeuni;
088000060531        clear savtbeftt;
088100060531        clear savtbeflt;
088200060505        clear tbecod;
088300060505        clear tbeke2;
088400060505        tbeke1 = *all'0';
088500060703        %subst(tbeke1:13:3) = 'ICE';
088600060505        chain(n) (tbecod:tbeke1) tntbe01l;
088700060505        if %found(tntbe01l);
088800060505         savtbeuni = tbeuni;
088900060531         savtbeftt = tbeftt;
089000060531         savtbeflt = tbeflt;
089100060505        endif;
089200060505
089300060505       // scrivo la testata nelle varie lingue inserite in AZLIN
089400060505        clear lintntbe;
089500060505        setgt lintntbe azlin02l;
089600060505        dou %eof(azlin02l);
089700060505         read azlin02l;
089800060505       // fine file
089900060505         if %eof(azlin02l);
090000060505          leave;
090100060505         endif;
090200060505       // scrivo
090300060505         tbelin = lintntbe;
090400060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
090500060508         if not %found(tntbe11l);
090600060505          clear tbecod;
090700060505          tbeke1 = *all'0';
090800060703          %subst(tbeke1:13:3) = 'ICE';
090900060505          tbelin = lintntbe;
091000060505          tbeuni = savtbeuni;
091100060531          tbeftt = savtbeftt;
091200060531          tbeflt = savtbeflt;
091300060531          clear tbeftr;
091400060531          clear tbedtr;
091500060508          write tntbe11;
091600060505         endif;
091700060505        enddo;
091800060505
091900060505       endsr;
092000060505
092100060505
092200060505       // ------------------------------------------------------------------------
092300060505       // pulisco il subfile delle descrizioni in lingua
092400060505       // ------------------------------------------------------------------------
092500060505       begsr sr_pulsfl;
092600060505
092700060505        clear s02nrr;
092800060505        clear savrec;
092900060508        *in24 = *off;
093000060508        *in25 = *off;
093100060505        write tb49c02;
093200060508        *in24 = *on;
093300060508        *in25 = *on;
093400060505
093500060505       endsr;
093600060505
093700060505       // ------------------------------------------------------------------------
093800060505       // carico il subfile delle descrizioni in lingua
093900060505       // ------------------------------------------------------------------------
094000060505       begsr sr_carsfl;
094100060505
094200060505        *in30 = *off;
094300060505       // carico un record per ogni lingua inserite in AZLIN
094400060505        clear lintntbe;
094500060505        setgt lintntbe azlin02l;
094600060505        dou %eof(azlin02l);
094700060505         read azlin02l;
094800060505       // fine file
094900060505         if %eof(azlin02l);
095000060505          leave;
095100060505         endif;
095200060703         clear dice;
095300060505         s2dlin = lindesita;
095400060703         tbecod = 'ICE';
095500060505         tbeke1 = d1tbeke1;
095600060505         tbeke2 = d1tbeke2;
095700060505         tbelin = lintntbe;
095800060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
095900060508         if not %found(tntbe11l);
096000060703          clear s2cdei;
096001060703          clear s2cdep;
096100060505         else;
096200060703          dice = tbeuni;
096300060703          s2cdei = §icedei;
096301060703          s2cdep = §icedep;
096400060505         endif;
096500060505         s2hcdlin = tbelin;
096600060505         s02nrr = s02nrr + 1;
096700060505         write tb49s02;
096800060505        enddo;
096900060505
097000060505        *in30 = *on;
097100060505
097200060505       endsr;
097300060505
097400060505       // ------------------------------------------------------------------------
097500060505       // controllo il subfile delle descrizioni in lingua
097600060505       // ------------------------------------------------------------------------
097700060505       begsr sr_ctrsfl;
097800060505
097900060505        s02nrr = 1;
098000060505        chain s02nrr tb49s02;
098100060505        dow %found;
098200060505       // deve esserci la descrizione
098300060703         if s2cdei = *blanks;
098400060505          vc2msg = msg(02);
098500060505          *in28 = *on;
098600060508          *in47 = *on;
098700060505          savrec = s02nrr;
098800060505          update tb49s02;
098900060505          leave;
099000060505         endif;
099100060505         update tb49s02;
099200060505         s02nrr = s02nrr + 1;
099300060505         chain s02nrr tb49s02;
099400060505        enddo;
099500060505
099600060505       endsr;
099700060505
099800060505       // ------------------------------------------------------------------------
099900060505       // confermo il dati del subfile delle descrizioni in lingua
100000060505       // ------------------------------------------------------------------------
100100060505       begsr sr_confsfl;
100200060505
100300060505       // recupero i dati della tabella in italiano
100400060505        clear savtbeuni;
100500060703        tbecod = 'ICE';
100600060505        tbeke1 = d1tbeke1;
100700060505        tbeke2 = d1tbeke2;
100800060505        clear tbelin;
100900060531        chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
101000060508        if %found(tntbe11l);
101100060505         savtbeuni = tbeuni;
101200060505        endif;
101300060505
101400060505       // leggo il subfile e aggiorno/scrivo i dati in lingua
101500060505        s02nrr = 1;
101600060505        chain s02nrr tb49s02;
101700060505        dow %found;
101800060505         tbelin = s2hcdlin;
101900060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
102000060703         dice = savtbeuni;
102100060703         §icedei = s2cdei;
102101060703         §icedep = s2cdep;
102200060703         tbeuni = dice;
102300060508         tbeftt = w1ftt;
102400060505         clear tbeftr;
102500060505         clear tbedtr;
102600060505       // scrivo
102700060508         if not %found(tntbe11l);
102800060703          tbecod = 'ICE';
102900060505          tbeke1 = d1tbeke1;
103000060505          tbeke2 = d1tbeke2;
103100060505          tbelin = s2hcdlin;
103200060508          write tntbe11;
103300060505         else;
103400060505       // aggiorno
103500060508          update tntbe11;
103600060505         endif;
103700060505         s02nrr = s02nrr + 1;
103800060505         chain s02nrr tb49s02;
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
107100060703          if iceesito = *blanks;
107200060703           iceesito = eseguito;
107300060502          endif;
107400060502
107500060703          kpjbu = tntbices;
107600060502
107700060502          *inlr = *on;
107800060502          return;
107900060502
108000060502         endsr;
108100060502
108200060502      /end-free
108300060502
108400060502** -MSG-                                                                     *
108500060502Immettere la causale                                                           01
108600060502Immettere la descrizione                                                       02
108700060510
108800060510
108900060502Causale già esistente                                                          05
