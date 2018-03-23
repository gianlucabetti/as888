000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300090115      * Gestione tabella "ICE" = Causali eventi su internet                    *
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
005000090115     d wdes            s                   like(icedes)
005100060428     d wrkkey          s              1
005200060428     d wrkeofs01       s              1
005300060428     d wrkcars01       s              1
005400060505     d wrkcarw01       s              1
005500060505     d wrksfl02        s              1
005600060504     d $video          s             10
005700090115     d richiamo        s              1
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006200060504     d wlbdat          ds                  inz
006300060504     d  g02dat                 1      8  0
006400060504     d  g02inv                 9     16  0
006500060504     d  g02err                17     17
006600060504     d  g02tgi                18     22  0
006700060428
006800060703     d dice          e ds
006900060502     d kpjba         e ds
007000060503     d tibs34ds      e ds                  inz
007100060703     d tntbices      e ds
007200060428     d §azute        e ds                  extname(azute00f)
007300060428     d                                     dtaara
007400060428     d §datiute      e ds                  extname(ddatiute)
007500060428     d                                     dtaara
007600010503
007700060428     d psds           sds
007800060428     d  pgmname          *proc
007900060428
008000060428      *------------------------------------------------------------------------*
008100060428      * pgm richiamati
008200060428      *------------------------------------------------------------------------*
008300060428     d tibs34r         pr                  extpgm('TIBS34R')
008400060502     d  tibs34ds                           likeds(tibs34ds)
008500060504
008600060504     d xsrda8          pr                  extpgm('XSRDA8')
008700060504     d  wlbdat                             likeds(wlbdat)
008800060428
008900060428      *------------------------------------------------------------------------*
009000060428      * costanti
009100060428      *------------------------------------------------------------------------*
009200060428     d errore          c                   '1'
009300060428     d eseguito        c                   '0'
009400010503
009500060428      *------------------------------------------------------------------------*
009600010423
009700060428      /free
009800060428
009900060428       exsr sr_parm;
010000060428       exsr sr_c01;
010100060428       exsr sr_uscita;
010200010423
010300060502       // ------------------------------------------------------------------------
010400060502       // elaborazione parametri ricevuti
010500060502       // ------------------------------------------------------------------------
010600060428       begsr sr_parm;
010700060428
010800060703        tntbices = kpjbu;
010900090115        clear richiamo   ;
011000060428
011100060428        select;
011200090115       // ricerca e scelta
011300060703         when iceFun = '1';
011400060428          *in01 = *on;
011500090115
011600090115       // manutenzione o immissione di un codice specifico
011700090115       //  pgm richiamato
011800090115         when  iceFun = *blanks and iceke1<>*blanks  ;
011900090115          *in01 = *off;
012000090115          richiamo='S'   ;
012100090115           exsr Gescodice;
012200090115          exsr sr_uscita;
012300090115
012400090115       // manutenzione  generica
012500060703         when  iceFun = *blanks;
012600060428          *in01 = *off;
012700060502       // altrimenti
012800060428         other;
012900060703          iceEsito = errore;
013000060428           exsr sr_uscita;
013100060428         endsl;
013200060428
013300060428       endsr;
013400010430
013500090115       // ------------------------------------------------------------------------
013600090115       // gestione di un codice specifico in immissione o manutenzione
013700090115       // ------------------------------------------------------------------------
013800090115       begsr Gescodice   ;
013900090115
014000090115       tbecod = 'ICE';
014100090115       tbeke1  =iceke1   ;
014200090115       s1tbeke1=iceke1  ;
014300090115       chain(n)  (tbecod: tbeke1)   tntbe01l    ;
014400090115
014500090115       if %found(tntbe01l)   ;
014600090115            iceopzio = '02';
014700090115            exsr sr_d01;
014800090115       else   ;
014900090115       // non trovato: procedo ad inserire
015000090115            $video = 'D01';
015100090115            wdes=icedes   ;
015200090115
015300090115            clear tntbices;
015400090115            clear dice;
015500090115            icecomand = 'J';
015600090115            exsr sr_d01;
015700090115       endif   ;
015800090115
015900090115       endsr;
016000060502       // ------------------------------------------------------------------------
016100060502       // gestione subfile
016200060502       // ------------------------------------------------------------------------
016300060428       begsr sr_c01;
016400060428
016500060502       // imposto variabili
016600060428        wrkcars01 = *on;
016700060428        $video = 'C01';
016800060428
016900060502       // inizio elaborazione subfile
017000060428         dou  $video <> 'C01';
017100060428
017200060502       // caricamento subfile
017300060428          exsr sr_cars01;
017400060428
017500060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
017600060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
017700060502       // l'ultimo tasto
017800060428          if c1csrrrn > 0;
017900060428           c1rcdnbr = c1csrrrn;
018000060428          else;
018100060428           c1rcdnbr = savrcdnbr;
018200060428          endif;
018300060428
018400060502       // se non so quale pagina visualizzare forzo pagina 1
018500060428          if c1rcdnbr < 1;
018600060428           eval c1rcdnbr = 1;
018700060428          endif;
018800060428
018900060502       // salvo il record number del subfile
019000060428          savrcdnbr = c1Rcdnbr;
019100060428
019200060502       // Emissione del subfile
019300060508          write tb49p01;
019400060428          exfmt tb49c01;
019500060428
019600060502       // controllo tasti funzionali del subfile
019700060428          select;
019800060428
019900060502       // F3=Fine
020000060428           when *inkc;
020100060502            $video = *blanks;
020200060703            icericez = 'C';
020300060428            exsr sr_uscita;
020400060428
020500060502       // F5=Refresh
020600060428           when *inke;
020700060428            wrkcars01 = *on;
020800060428
020900060502       // F10=Immissione
021000060428           when *inkj;
021100060502            $video = 'D01';
021200060703            clear tntbices;
021300060703            clear dice;
021400060703            icecomand = 'J';
021500060428            exsr sr_d01;
021600060428
021700060502       // F13=Ripetizione
021800060428           when *inkm;
021900060428            exsr sr_ripetiopz;
022000060428
022100060502       // in tutti gli altri casi
022200060428           other;
022300060502       // controllo ed elaborazione scelte su subfile
022400060428            exsr sr_gestsfl;
022500060428          endsl;
022600060428
022700060502       // fine elaborazione 'C01'
022800060428         enddo;
022900060428
023000060428        endsr;
023100060428
023200060502       // ------------------------------------------------------------------------
023300060502       // caricamento subfile
023400060502       // ------------------------------------------------------------------------
023500060428       begsr sr_cars01;
023600010430
023700060502       // se è stato richiesto il caricamento del subfile
023800060428        if wrkcars01 = *on;
023900060428
024000060502       // inizializzo il subfile
024100060505         s01nrr = 0;
024200060428         *in20 = *on;
024300060428         write tb49c01;
024400060428         *in20 = *off;
024500060428         *in21 = *off;
024600060428         *in22 = *off;
024700060428         *in23 = *off;
024800060428
024900060502       // imposto la chiave di posizionamento e lettura file
025000060703         tbecod = 'ICE';
025100060428         tbeke1 = c1setll;
025200060428
025300060502       // se è stato scelto il posizionamento
025400060502         if c1setll <> *blanks;
025500060428          wrkkey = '2';
025600060502       // altrimenti
025700060428         else;
025800060428          wrkkey = '1';
025900060428         endif;
026000060428
026100060502       // posizionamento file
026200060428         exsr sr_setlltbe01;
026300060428
026400060502       // fino a che non è fine file
026500060428         dou %eof(tntbe01l);
026600060428
026700060502       // leggo file
026800060505          reade(n) tbecod tntbe01l;
026900060428
027000060502       // fine file esco
027100060428          if %eof(tntbe01l);
027200060428           leave;
027300060428          endif;
027400060428
027500060502       // se in "ricerca/scelta" non carico i records annullati
027600060502          if (*in01 and tbeatb = *blanks) or not *in01;
027700060502       // scrivo subfile
027800060428           clear s1opzione;
027900060428           exsr sr_wtrs01;
028000060428          endif;
028100060428
028200060428         enddo;
028300060428
028400060502       // fine caricamento subfile
028500060428        endif;
028600060428
028700060502       // se scritto almeno un record
028800060505        if s01nrr > 0;
028900060502       // indicatore di sflend
029000060428         eval *in21 = *on;
029100060428        endif;
029200060428
029300060502       // se non scritto neanche un record
029400060505        if s01nrr = 0;
029500060502       // indicatore di sfldsp
029600060428         eval *in23 = *on;
029700060428        endif;
029800060428
029900060502       // disattivo opzione di caricamento subfile
030000060428        wrkcars01 = *off;
030100060428
030200060428       endsr;
030300060428
030400060502       // ------------------------------------------------------------------------
030500060502       // gestione videata
030600060502       // ------------------------------------------------------------------------
030700060428       begsr sr_d01;
030800060428
030900060504       // imposto il video a seconda della funzione richiesta
031000060428        exsr sr_setvideo;
031100060428
031200060502       // imposto variabile
031300060428        $video = 'D01';
031400060428
031500060502       // fino a che la variabile resta settata come 'D01'
031600060428        dou $video <> 'D01';
031700060428
031800060508         *in02 = *off;
031900060508         *in03 = *off;
032000060509         *in04 = *off;
032100060428
032200060502       // se immessa opzione di 'scelta'
032300060703         if iceopzio = '01';
032400060703          iceke1 = s1tbeke1;
032500060703          icedes = s1tbeuni;
032600060428          exsr sr_uscita;
032700060428
032800060508       // se immessa un'altra opzione
032900060508         else;
033000060508       // se non è immissione/copia proteggo il campo della causale
033100060703          if icecomand <> 'J' and iceopzio <> '03';
033200060428           *in02 = *on;
033300060428          endif;
033400060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
033500060508       // proteggo i campi del video
033600060703          if iceopzio = '04' or iceopzio = '05';
033700060508           *in03 = *on;
033800060508          endif;
033900060509       // se immessa opzione di 'visualizzazione'
034000060509       // non attivo F6
034100060703          if iceopzio = '05';
034200060509           *in04 = *on;
034300060509          endif;
034400060502       // emetto il video
034500060428          exfmt tb49d01;
034600060428         endif;
034700060428
034800060502       // reset indicatore generico di errore
034900060428         *in28 = *off;
035000060508
035100060508       // pulisco il campo messaggi
035200060508         clear vd1msg;
035300010430
035400060502       // controllo tasti funzionali del video
035500060428         select;
035600060428
035700060502       // F3=Fine
035800060428          when *inkc;
035900060703           icericez = 'C';
036000060502           $video = *blanks;
036100060505           unlock tntbe01l;
036200060428           exsr sr_uscita;
036300060428
036400060502       // F6=Conferma
036500060428          when *inkf;
036600060703           icericez = 'F';
036700060502       // controllo e decodifico i dati del video
036800060428           exsr sr_ctrd01;
036900060502       // conferma per annullo/ripristino
037000060703           if iceopzio = '04';
037100060428            select;
037200060502       // annullamento
037300060502             when tbeatb = *blanks;
037400060428              tbeatb = 'A';
037500060502       // ripristino
037600060502             when tbeatb = 'A';
037700060428              clear tbeatb;
037800060428            endsl;
037900060428           endif;
038000060428
038100060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
038200060428           if not *in28;
038300060504            wrkcarw01 = *on;
038400060504            $video = 'W01';
038500060504            exsr sr_w01;
038600060428           endif;
038700060428
038800060502       // F8=Record successivo
038900060428          when  *inkh;
039000060428           clear s1opzione;
039100060428           wrkcars01 = *off;
039200060428           $video = 'C01';
039300060703           icericez = 'H';
039400060505
039500060505       // F9=Traduzione
039600060505          when  *inki;
039700060505           *in05 = *on;
039800060505           exsr sr_lingua;
039900060505           *in05 = *off;
040000060428
040100060502       // F12=Ritorno
040200060428          when *inkl;
040300060428           clear s1opzione;
040400060703           icericez = 'L';
040500060505           unlock tntbe01l;
040600060508       // se non è f12 da immissione/copia non ricarico il subfile
040700060703           if icecomand = 'J' or iceopzio = '03';
040800060428            wrkcars01 = *on;
040900060428           else;
041000060428            wrkcars01 = *off;
041100060428           endif;
041200060428           $video = 'C01';
041300060428
041400060502       // Invio
041500060428          other;
041600060508           if not *in03;
041700060508            exsr sr_ctrd01;
041800060508           endif;
041900060428
042000060502         endsl;
042100060428
042200060502       // fine gestione 'D01'
042300060428        enddo;
042400060428
042500060428       endsr;
042600060502
042700060502       // ------------------------------------------------------------------------
042800060502       // ripeto opzione in tutte le righe del subfile
042900060502       // ------------------------------------------------------------------------
043000060428       begsr sr_ripetiopz;
043100060428
043200060428        if c1csrrrn > 0;
043300060505         s01nrr = c1csrrrn;
043400060505         chain s01nrr tb49s01;
043500060428         if %found and s1opzione > 0;
043600060428          savopzione = s1opzione;
043700060428          *in22 = *on;
043800060428          update tb49s01;
043900060428
044000060428          wrkeofs01 = *off;
044100060428          dou wrkeofs01 = *on;
044200060505           s01nrr = s01nrr + 1;
044300060505           chain s01nrr tb49s01;
044400060428           if %found;
044500060428            s1opzione = savopzione;
044600060428            update tb49s01;
044700060428           else;
044800060428            wrkeofs01 = *on;
044900060428           endif;
045000060428          enddo;
045100060428
045200060428          *in22 = *off;
045300060428
045400060428         endif;
045500060428
045600060428        endif;
045700060428
045800060428       endsr;
045900060428
046000060502       // ------------------------------------------------------------------------
046100060502       // imposto i dati a video
046200060502       // ------------------------------------------------------------------------
046300060428       begsr sr_setvideo;
046400060428
046500060502       // pulisco il formato video D01
046600060502        exsr sr_puld01;
046700060428
046800060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
046900060428        select;
047000060428
047100060502       // F10=Immissione
047200060703         when icecomand = 'J';
047300060428          d1descopz = 'Immissione';
047400090115          // per pgm richiamo imposto codice e descrizione
047500090115          if richiamo='S'   ;
047600090115             d1icedei=wdes   ;
047700090115             d1tbeke1=s1tbeke1   ;
047800090115          endif    ;
047900060428
048000060502       // Opzione "02"=modifica
048100060703         when iceopzio = '02';
048200060428          d1descopz = 'Modifica';
048300060428          exsr sr_imposta;
048400060428
048500060502       // Opzione "03"=copia
048600060703         when iceopzio = '03';
048700060428          d1descopz = 'Copia';
048800060428          exsr sr_imposta;
048900060428
049000060502       // Opzione "04"=cancellazione/ripristino
049100060703         when iceopzio = '04';
049200060428          exsr sr_imposta;
049300060502       // se richiesta 'cancellazione'
049400060502          if tbeatb = *blanks;
049500060502           d1descopz = 'Annullamento';
049600060502          endif;
049700060502       // se richiesto 'ripristino'
049800060502          if tbeatb = 'A';
049900060502           d1descopz = 'Ripristino';
050000060502          endif;
050100060502
050200060502       // Opzione "05"=visualizzazione
050300060703         when iceOpzio ='05';
050400060502          d1descopz = 'Visualizzazione';
050500060502          exsr sr_imposta;
050600060502
050700060502       // Fine scelta
050800060502        endsl;
050900060502
051000060502       // centro la descrizione della funzione nel formato video
051100060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
051200060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
051300060502
051400060502       endsr;
051500060502
051600060502       // ------------------------------------------------------------------------
051700060502       // controllo video
051800060502       // ------------------------------------------------------------------------
051900060502       begsr sr_ctrd01;
052000060502
052100060502        *in28 = *off;
052200060502        *in40 = *off;
052300060502        *in41 = *off;
052400010430
052500060502       // causale chiusura C.A.
052600060502        if d1tbeke1 = *blanks;
052700060502         vd1msg = msg(01);
052800060502         *in28 = *on;
052900060502         *in40 = *on;
053000060502         leavesr;
053100060502        endif;
053200060508
053300060508       // se immissione controllo che non esista già
053400060703        if icecomand = 'J' or iceopzio = '03';
053500060703         tbecod = 'ICE';
053600060508         tbeke1 = d1tbeke1;
053700060508         tbeke2 = d1tbeke2;
053800060508         clear tbelin;
053900060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
054000060508         if %found(tntbe01l);
054100060508          vd1msg = msg(05);
054200060508          *in28 = *on;
054300060508          *in40 = *on;
054400060508          leavesr;
054500060508         endif;
054600060508        endif;
054700060502
054800060502       // descrizione
054900060703        if d1icedei = *blanks;
055000060502         vd1msg = msg(02);
055100060502         *in28 = *on;
055200060502         *in41 = *on;
055300060502         leavesr;
055400060502        endif;
055500060502
055600060502       endsr;
055700060504
055800060504       // ------------------------------------------------------------------------
055900060504       // gestione video dati di trasmissione
056000060504       // ------------------------------------------------------------------------
056100060504       begsr sr_w01;
056200060504
056300060504       // imposto i dati a video
056400060504        exsr sr_carw01;
056500060504
056600060505       // fino a che la variabile resta settata come 'W01'
056700060504        dou $video <> 'W01';
056800060504
056900060504       // reset indicatore generico di errore
057000060504         *in28 = *off;
057100060504
057200060504       // emetto il video
057300060504          exfmt tb49w01;
057400060504
057500060504       // controllo tasti funzionali del video
057600060504         select;
057700060504
057800060504       // F6=Conferma
057900060504          when *inkf;
058000060504       // controllo i dati del video
058100060504           exsr sr_ctrw01;
058200060504       // se non riscontrati errori aggiorno il record corrente
058300060504           if not *in28;
058400060504            exsr sr_aggiorna;
058500060504            if not *in28;
058600060505       // emetto le descrizioni in lingua
058700060509       // se non è annullo/ripristino
058800060703             if iceopzio <> '04';
058900060509              *in05 = *off;
059000060509              exsr sr_lingua;
059100060509             endif;
059200090115             if icecomand = 'J' and  richiamo=' ';
059300060504              $video = 'D01';
059400060504              exsr sr_setvideo;
059500060504             else;
059600060504              $video = 'C01';
059700060504             endif;
059800060504            endif;
059900060504           endif;
060000060504
060100060504       // F12=Ritorno
060200060504          when *inkl;
060300060504           $video = 'D01';
060400060703           clear icericez;
060500060504
060600060504       // Invio
060700060504          other;
060800060504           exsr sr_ctrw01;
060900060504
061000060504         endsl;
061100060504
061200060504       // fine gestione 'W01'
061300060504        enddo;
061400060504
061500060504       endsr;
061600060504
061700060504       // ------------------------------------------------------------------------
061800060504       // imposto i dati di trasmissione
061900060504       // ------------------------------------------------------------------------
062000060504       begsr sr_carw01;
062100060504
062200060504       // se pulisco i campi
062300060504         clear w1ftt;
062400060504         clear w1flt;
062500060504         clear w1ftr;
062600060504         clear w1dtr;
062700060504
062800060504       // se non immissione imposto i campi del file
062900060703        if icecomand <> 'J';
063000060504         w1ftt = tbeftt;
063100060504         w1flt = tbeflt;
063200060504         w1ftr = tbeftr;
063300060504       // imposto la data
063400060504         if tbedtr <> *zeros;
063500060504          clear wlbdat;
063600060504          g02inv = tbedtr;
063700060504          g02err = '3';
063800060504          xsrda8(wlbdat);
063900060504          w1dtr = g02dat;
064000060504         endif;
064100060504
064200060504        endif;
064300060504
064400060504       endsr;
064500060504
064600060504       // ------------------------------------------------------------------------
064700060504       // controllo i dati di trasmissione
064800060504       // ------------------------------------------------------------------------
064900060504       begsr sr_ctrw01;
065000060504
065100060504       endsr;
065200060502
065300060502       // ------------------------------------------------------------------------
065400060502       // aggiorno tabella
065500060502       // ------------------------------------------------------------------------
065600060502       begsr sr_aggiorna;
065700060502
065800060502       // imposto campi del file
065900060509        clear tbelin;
066000060502        tbeke1 = d1tbeke1;
066100060509        tbeke2 = d1tbeke2;
066200060502
066300060703        §icedei = d1icedei;
066400060703        §icedep = d1icedep;
066500060502
066600060703        tbeuni = dice;
066700060502
066800060504        tbeftt = w1ftt;
066900060504        tbeflt = w1flt;
067000060502        clear tbeftr;
067100060502        clear tbedtr;
067200060502
067300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
067400060502        select;
067500060502
067600060502       // F10=immissione - Opzione "03"=copia
067700060703         when icecomand = 'J' or iceopzio = '03';
067800060502       // scrivo nuovo record con gestione errore per chiave duplicata
067900060508          write tntbe000;
068000060502
068100060502       // Opzione "02"=modifica
068200060703         when iceopzio = '02';
068300060502       // update record e controllo errore per chiave duplicata
068400060508          update tntbe000;
068500060502
068600060502       // Opzione "04"=cancellazione/ripristino
068700060703         when iceopzio = '04';
068800060502          update tntbe000;
068900010430
069000060502        endsl;
069100060509
069200060509       // se Opzione "04"=cancellazione/ripristino
069300060509       // aggiorno automaticamente anche le descrizioni in linea
069400060703        if iceopzio = '04';
069500060509         clear lintntbe;
069600060509         setgt lintntbe azlin02l;
069700060509         dou %eof(azlin02l);
069800060509          read azlin02l;
069900060509       // fine file
070000060509          if %eof(azlin02l);
070100060509           leave;
070200060509          endif;
070300060509       // aggiorno
070400060509          tbelin = lintntbe;
070500060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
070600060509          if %found(tntbe11l);
070700060509           select;
070800060509            when tbeatb <> *blanks;
070900060509             clear tbeatb;
071000060509            when tbeatb = *blanks;
071100060509             tbeatb = 'A';
071200060509           endsl;
071300060509            update tntbe11;
071400060509          endif;
071500060509         enddo;
071600060509        endif;
071700060502
071800060502       endsr;
071900060502
072000060502       // ------------------------------------------------------------------------
072100060502       // pulizia video
072200060502       // ------------------------------------------------------------------------
072300060502       begsr sr_puld01;
072400060502
072500060504        clear d1descopz;
072600060504        clear d1tbeke1;
072700060703        clear d1icedei;
072800060703        clear d1icedep;
072900060504
073000090115        *in06 = *off;
073100090115        *in28 = *off;
073200060504        *in40 = *off;
073300060504        *in41 = *off;
073400060502
073500060502       endsr;
073600060502
073700060502       // ------------------------------------------------------------------------
073800060502       // imposto i dati a video
073900060502       // ------------------------------------------------------------------------
074000060502       begsr sr_imposta;
074100090115
074200090115       *in06 =*off    ;
074300060502
074400060502       // recupero i dati dal file
074500060505        ktbeke1 = s1tbeke1;
074600060508        clear tbelin;
074700060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
074800060502
074900060502       // imposto i campi a video
075000060505        d1tbeke2 = tbeke2;
075100060502        d1tbeke1 = tbeke1;
075200060703        dice = tbeuni;
075300060703        d1icedei = §icedei;
075400060703        d1icedep = §icedep;
075500090115        if tbeatb<>' '   ;
075600090115        *in06=*on    ;
075700090115        endif   ;
075800060502
075900060502       endsr;
076000060502
076100060502       // ------------------------------------------------------------------------
076200060502       // gestione del subfile
076300060502       // ------------------------------------------------------------------------
076400060502       begsr sr_gestsfl;
076500060502
076600060502       // set flag
076700060502        wrkeofs01 = *off;
076800060502
076900060502       // inizio lettura subfile
077000060502        dow wrkeofs01 = *off and *in21;
077100060502         readc tb49s01;
077200060502       // se fine subfile
077300060502         if %eof;
077400060502          wrkcars01 = *on;
077500060502          leave;
077600060502         endif;
077700060502       // se è stata immessa un'opzione
077800060502         if s1opzione <> *zeros;
077900060502       // reset ds di servizio
078000060703          clear tntbices;
078100060502       // controllo ed elaborazione opzione immessa
078200060502          select;
078300060502       // opzione "01"=scelta
078400060502           when s1opzione = 1 and *in01;
078500060703            iceopzio = '01';
078600060502       // opzione "02"=modifica
078700060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
078800060703            iceopzio = '02';
078900060502       // opzione "03"=copia
079000060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
079100060703            iceopzio = '03';
079200060502       // opzione "04"=annullo/ripristino
079300060502           when s1opzione = 4 and not *in01;
079400060703            iceopzio = '04';
079500060502       // opzione "05"=visualizzazione
079600060502           when s1opzione = 5;
079700060703            iceopzio = '05';
079800060502          endsl;
079900010503
080000060502       // se immessa almeno un opzione valida
080100060703          if  iceopzio <> *blanks;
080200060502       // gstione del formato video
080300060504           exsr sr_d01;
080400060502       // se la gestione si è chiusa con ...
080500060504           select;
080600060502       // F6=Conferma
080700060703            when icericez = 'F';
080800060504             clear s1opzione;
080900060504             wrkcars01 = *on;
081000060502       // F12=Ritorno
081100060703            when icericez = 'L';
081200060504             clear s1opzione;
081300060504             wrkeofs01 = *on;
081400060502       // altrimenti
081500060504            other;
081600060504             *in22 = *on;
081700060504           endsl;
081800060502
081900060504           update tb49s01;
082000060504           *in22 = *off;
082100060504       // fine opzione
082200060504          endif;
082300060502       // fine opzione
082400060502         endif;
082500060502
082600060502        enddo;
082700060502
082800060502       endsr;
082900060502
083000060502       // ------------------------------------------------------------------------
083100060502       // posizionamento sul file
083200060502       // ------------------------------------------------------------------------
083300060502       begsr sr_setlltbe01;
083400060502
083500060502        select;
083600060502       // chiave totale
083700060502         when wrkkey = '1';
083800060502          setll tbecod tntbe01l;
083900060502       // chiave parziale
084000060502         when wrkkey = '2';
084100060502          setll (tbecod:tbeke1) tntbe01l;
084200060502        endsl;
084300060502
084400060502       endsr;
084500060502
084600060502       // ------------------------------------------------------------------------
084700060502       // scrivo subfile
084800060502       // ------------------------------------------------------------------------
084900060502       begsr sr_wtrs01;
085000060502
085100060502       // se non raggiunto limite massimo di caricamento
085200060505        if s01nrr < 9999;
085300060502       // imposto campi di subfile
085400060502         exsr sr_sets01;
085500060502       // imposto numeratore righe e numero relativo di record
085600060505         s01nrr = s01nrr + 1;
085700060502       // scrivo subfile
085800060502         write tb49s01;
085900060502        endif;
086000060502
086100060502       endsr;
086200060502
086300060502       // ------------------------------------------------------------------------
086400060502       // imposto campi del subfile
086500060502       // ------------------------------------------------------------------------
086600060502       begsr sr_sets01;
086700060502
086800060502        s1tbeke1 = tbeke1;
086900060505        s1tbeke2 = tbeke2;
087000060703        dice = tbeuni;
087100060703        s1tbeuni = §icedei;
087200060502        s1tbeatb = tbeatb;
087300060502
087400060502       endsr;
087500060505
087600060505       // ------------------------------------------------------------------------
087700060505       // gestione tabelle in lingua
087800060505       // ------------------------------------------------------------------------
087900060505       begsr sr_lingua;
088000060505
088100060505       // pulisco il subfile
088200060505        exsr sr_pulsfl;
088300060505       // carico il subfile con i dati che trovo o vuoto
088400060505        exsr sr_carsfl;
088500060505
088600060505        wrksfl02 = *on;
088700060505
088800060505        dou wrksfl02 = *off;
088900060505         rec = 1;
089000060505         if savrec > *zeros;
089100060505          rec = savrec;
089200060505         endif;
089300060508         write tb49p02;
089400060505         exfmt tb49c02;
089500060505
089600060505         *in28 = *off;
089700060505         *in47 = *off;
089800060505
089900060505       // F12=Ritorno
090000060505          if *inkl;
090100060505           wrksfl02 = *off;
090200060508           leavesr;
090300060505          endif;
090400060505
090500060505       // Controlli
090600060505          if not *in05;
090700060505           exsr sr_ctrsfl;
090800060505          endif;
090900060505
091000060505       // F6=Conferma
091100060505          if *inkf and not *in28;
091200060508           exsr sr_confsfl;
091300060505           wrksfl02 = *off;
091400060505          endif;
091500060505
091600060505         enddo;
091700060505
091800060505       endsr;
091900060505
092000060505       // ------------------------------------------------------------------------
092100060505       // pulisco il subfile delle descrizioni in lingua
092200060505       // ------------------------------------------------------------------------
092300060505       begsr sr_pulsfl;
092400060505
092500060505        clear s02nrr;
092600060505        clear savrec;
092700060508        *in24 = *off;
092800060508        *in25 = *off;
092900060505        write tb49c02;
093000060508        *in24 = *on;
093100060508        *in25 = *on;
093200060505
093300060505       endsr;
093400060505
093500060505       // ------------------------------------------------------------------------
093600060505       // carico il subfile delle descrizioni in lingua
093700060505       // ------------------------------------------------------------------------
093800060505       begsr sr_carsfl;
093900060505
094000060505        *in30 = *off;
094100060505       // carico un record per ogni lingua inserite in AZLIN
094200060505        clear lintntbe;
094300060505        setgt lintntbe azlin02l;
094400060505        dou %eof(azlin02l);
094500060505         read azlin02l;
094600060505       // fine file
094700060505         if %eof(azlin02l);
094800060505          leave;
094900060505         endif;
095000060703         clear dice;
095100060505         s2dlin = lindesita;
095200060703         tbecod = 'ICE';
095300060505         tbeke1 = d1tbeke1;
095400060505         tbeke2 = d1tbeke2;
095500060505         tbelin = lintntbe;
095600060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
095700060508         if not %found(tntbe11l);
095800060703          clear s2cdei;
095900060703          clear s2cdep;
096000060505         else;
096100060703          dice = tbeuni;
096200060703          s2cdei = §icedei;
096300060703          s2cdep = §icedep;
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
102200060703         §icedep = s2cdep;
102300060703         tbeuni = dice;
102400060508         tbeftt = w1ftt;
102500060505         clear tbeftr;
102600060505         clear tbedtr;
102700060505       // scrivo
102800060508         if not %found(tntbe11l);
102900060703          tbecod = 'ICE';
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
107200060703          if iceesito = *blanks;
107300060703           iceesito = eseguito;
107400060502          endif;
107500060502
107600060703          kpjbu = tntbices;
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
