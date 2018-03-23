000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300150304      * Gestione tabella "SPC" = stato data cons prevista cliente              *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600060508     ftntbe11l  uf a e           k disk    rename(tntbe000:tntbe11)
000700060505     fazlin02l  if   e           k disk
000800150304     ftntbspcd  cf   e             workstn sfile(tbspcs01:s01nrr)
000900150304     f                                     sfile(tbspcs02:s02nrr)
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
006600150304     d dSPC          e ds
006700060502     d kpjba         e ds
006800060503     d tibs34ds      e ds                  inz
006900150304     d tntbspcds     e ds
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
010600150304        tntbspcdS = kpjbu;
010700060428
010800060428        select;
010900060502       // ricerca e scleta
011000150304         when bSPCFun = '1';
011100060428          *in01 = *on;
011200060502       // manutenzione
011300150304         when  bSPCFun = *blanks;
011400060428          *in01 = *off;
011500060502       // altrimenti
011600060428         other;
011700150304          bSPCEsito = errore;
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
015600150304          write tbspcp01;
015700150304          exfmt tbspcc01;
015800060428
015900060502       // controllo tasti funzionali del subfile
016000060428          select;
016100060428
016200060502       // F3=Fine
016300060428           when *inkc;
016400060502            $video = *blanks;
016500150304            bSPCricez = 'C';
016600060428            exsr sr_uscita;
016700060428
016800060502       // F5=Refresh
016900060428           when *inke;
017000060428            wrkcars01 = *on;
017100060428
017200060502       // F10=Immissione
017300060428           when *inkj;
017400060502            $video = 'D01';
017500150304            clear tntbspcds;
017600150304            bSPCcomand = 'J';
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
020500150304         write tbspcc01;
020600060428         *in20 = *off;
020700060428         *in21 = *off;
020800060428         *in22 = *off;
020900060428         *in23 = *off;
021000060428
021100060502       // imposto la chiave di posizionamento e lettura file
021200150304         tbecod = 'SPC';
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
028500150304         if bSPCopzio = '01';
028600150304          bSPCke1 = s1tbeke1;
028700150304          bSPCdeC1 = s1tbeuni;
028800060428          exsr sr_uscita;
028900060428
029000060508       // se immessa un'altra opzione
029100060508         else;
029200060508       // se non è immissione/copia proteggo il campo della causale
029300150304          if bSPCcomand <> 'J' and bSPCopzio <> '03';
029400060428           *in02 = *on;
029500060428          endif;
029600060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
029700060508       // proteggo i campi del video
029800150304          if bSPCopzio = '04' or bSPCopzio = '05';
029900060508           *in03 = *on;
030000060508          endif;
030100060509       // se immessa opzione di 'visualizzazione'
030200060509       // non attivo F6
030300150304          if bSPCopzio = '05';
030400060509           *in04 = *on;
030500060509          endif;
030600060502       // emetto il video
030700150304          exfmt tbspcd01;
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
032100150304           bSPCricez = 'C';
032200060502           $video = *blanks;
032300060505           unlock tntbe01l;
032400060428           exsr sr_uscita;
032500060428
032600060502       // F6=Conferma
032700060428          when *inkf;
032800150304           bSPCricez = 'F';
032900060502       // controllo e decodifico i dati del video
033000060428           exsr sr_ctrd01;
033100060502       // conferma per annullo/ripristino
033200150304           if bSPCopzio = '04';
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
035500150304           bSPCricez = 'H';
035600060505
035700060505       // F9=Traduzione
035800060505          when  *inki;
035900150507          if bSPCopzio = '05';
036000060505           *in05 = *on;
036100150507          endif  ;
036200060505           exsr sr_lingua;
036300060505           *in05 = *off;
036400060428
036500060502       // F12=Ritorno
036600060428          when *inkl;
036700060428           clear s1opzione;
036800150304           bSPCricez = 'L';
036900060505           unlock tntbe01l;
037000060508       // se non è f12 da immissione/copia non ricarico il subfile
037100150304           if bSPCcomand = 'J' or bSPCopzio = '03';
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
039800150304         chain s01nrr tbspcs01;
039900060428         if %found and s1opzione > 0;
040000060428          savopzione = s1opzione;
040100060428          *in22 = *on;
040200150304          update tbspcs01;
040300060428
040400060428          wrkeofs01 = *off;
040500060428          dou wrkeofs01 = *on;
040600060505           s01nrr = s01nrr + 1;
040700150304           chain s01nrr tbspcs01;
040800060428           if %found;
040900060428            s1opzione = savopzione;
041000150304            update tbspcs01;
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
043600150304         when bSPCcomand = 'J';
043700060428          d1descopz = 'Immissione';
043800060428
043900060502       // Opzione "02"=modifica
044000150304         when bSPCopzio = '02';
044100060428          d1descopz = 'Modifica';
044200060428          exsr sr_imposta;
044300060428
044400060502       // Opzione "03"=copia
044500150304         when bSPCopzio = '03';
044600060428          d1descopz = 'Copia';
044700060428          exsr sr_imposta;
044800060428
044900060502       // Opzione "04"=cancellazione/ripristino
045000150304         when bSPCopzio = '04';
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
046200150304         when bSPCopzio ='05';
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
048300150304
048400150304        if d1tbeke1 = *blanks;
048500150304         vd1msg = msg(01);
048600150304         *in28 = *on;
048700150304         *in40 = *on;
048800150304         leavesr;
048900150304        endif;
049000150304
049100060508
049200060508       // se immissione controllo che non esista già
049300150304        if bSPCcomand = 'J' or bSPCopzio = '03';
049400150304         tbecod = 'SPC';
049500060508         tbeke1 = d1tbeke1;
049600060508         tbeke2 = d1tbeke2;
049700060508         clear tbelin;
049800060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
049900060508         if %found(tntbe01l);
050000060508          vd1msg = msg(05);
050100060508          *in28 = *on;
050200060508          *in40 = *on;
050300060508          leavesr;
050400060508         endif;
050500060508        endif;
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
052400150304          exfmt tbspcw01;
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
053900150304             if bSPCopzio <> '04';
054000060509              *in05 = *off;
054100060509              exsr sr_lingua;
054200060509             endif;
054300150304             if bSPCcomand = 'J';
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
055500150304           clear bSPCricez;
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
058000150304        if bSPCcomand <> 'J';
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
061400150304        §spcdec1 = d1spcdec1 ;
061500150304        §spcdec2 = d1spcdec2 ;
061600150306        §spcnot1 = d1spcnot1+ d1spcnot12;
061700150306        §spcnot2 = d1spcnot2+ d1spcnot22;
061800060502
061900150304        tbeuni = dspc;
062000060502
062100060504        tbeftt = w1ftt;
062200060504        tbeflt = w1flt;
062300060502        clear tbeftr;
062400060502        clear tbedtr;
062500060502
062600060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
062700060502        select;
062800060502
062900060502       // F10=immissione - Opzione "03"=copia
063000150304         when bSPCcomand = 'J' or bSPCopzio = '03';
063100060502       // scrivo nuovo record con gestione errore per chiave duplicata
063200060508          write tntbe000;
063300060502
063400060502       // Opzione "02"=modifica
063500150304         when bSPCopzio = '02';
063600060502       // update record e controllo errore per chiave duplicata
063700060508          update tntbe000;
063800060502
063900060502       // Opzione "04"=cancellazione/ripristino
064000150304         when bSPCopzio = '04';
064100060502          update tntbe000;
064200010430
064300060502        endsl;
064400060509
064500060509       // se Opzione "04"=cancellazione/ripristino
064600060509       // aggiorno automaticamente anche le descrizioni in linea
064700150304        if bSPCopzio = '04';
064800060509         clear lintntbe;
064900060509         setgt lintntbe azlin02l;
065000060509         dou %eof(azlin02l);
065100060509          read azlin02l;
065200060509       // fine file
065300060509          if %eof(azlin02l);
065400060509           leave;
065500060509          endif;
065600060509       // aggiorno
065700060509          tbelin = lintntbe;
065800060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
065900060509          if %found(tntbe11l);
066000060509           select;
066100060509            when tbeatb <> *blanks;
066200060509             clear tbeatb;
066300060509            when tbeatb = *blanks;
066400060509             tbeatb = 'A';
066500060509           endsl;
066600060509            update tntbe11;
066700060509          endif;
066800060509         enddo;
066900060509        endif;
067000060502
067100060502       endsr;
067200060502
067300060502       // ------------------------------------------------------------------------
067400060502       // pulizia video
067500060502       // ------------------------------------------------------------------------
067600060502       begsr sr_puld01;
067700060502
067800060504        clear d1descopz;
067900060504        clear d1tbeke1;
068000150304        clear d1spcdec1 ;
068100150304        clear d1spcdec2 ;
068200150304        clear d1spcnot1 ;
068300150306        clear d1spcnot12;
068400150306        clear d1spcnot2 ;
068500150306        clear d1spcnot22;
068600060504
068700060504        *in28 = *off;
068800060504        *in40 = *off;
068900060504        *in41 = *off;
069000060502
069100060502       endsr;
069200060502
069300060502       // ------------------------------------------------------------------------
069400060502       // imposto i dati a video
069500060502       // ------------------------------------------------------------------------
069600060502       begsr sr_imposta;
069700060502
069800060502       // recupero i dati dal file
069900060505        ktbeke1 = s1tbeke1;
070000060508        clear tbelin;
070100060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
070200060502
070300060502       // imposto i campi a video
070400060505        d1tbeke2 = tbeke2;
070500060502        d1tbeke1 = tbeke1;
070600150304        dspc = tbeuni;
070700150304        d1spcdec1= §spcdec1;
070800150304        d1spcdec2= §spcdec2;
070900150306        d1spcnot1 = §spcnot1;
071000150306        d1spcnot12= %SUBST(§spcnot1:36:35);
071100150306        d1spcnot2 = §spcnot2;
071200150306        d1spcnot22= %SUBST(§spcnot2:36:35);
071300060502
071400060502       endsr;
071500060502
071600060502       // ------------------------------------------------------------------------
071700060502       // gestione del subfile
071800060502       // ------------------------------------------------------------------------
071900060502       begsr sr_gestsfl;
072000060502
072100060502       // set flag
072200060502        wrkeofs01 = *off;
072300060502
072400060502       // inizio lettura subfile
072500060502        dow wrkeofs01 = *off and *in21;
072600150304         readc tbspcs01;
072700060502       // se fine subfile
072800060502         if %eof;
072900060502          wrkcars01 = *on;
073000060502          leave;
073100060502         endif;
073200060502       // se è stata immessa un'opzione
073300060502         if s1opzione <> *zeros;
073400060502       // reset ds di servizio
073500150304          clear tntbspcds;
073600060502       // controllo ed elaborazione opzione immessa
073700060502          select;
073800060502       // opzione "01"=scelta
073900060502           when s1opzione = 1 and *in01;
074000150304            bSPCopzio = '01';
074100060502       // opzione "02"=modifica
074200060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
074300150304            bSPCopzio = '02';
074400060502       // opzione "03"=copia
074500060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
074600150304            bSPCopzio = '03';
074700060502       // opzione "04"=annullo/ripristino
074800060502           when s1opzione = 4 and not *in01;
074900150304            bSPCopzio = '04';
075000060502       // opzione "05"=visualizzazione
075100060502           when s1opzione = 5;
075200150304            bSPCopzio = '05';
075300060502          endsl;
075400010503
075500060502       // se immessa almeno un opzione valida
075600150304          if  bSPCopzio <> *blanks;
075700060502       // gstione del formato video
075800060504           exsr sr_d01;
075900060502       // se la gestione si è chiusa con ...
076000060504           select;
076100060502       // F6=Conferma
076200150304            when bSPCricez = 'F';
076300060504             clear s1opzione;
076400060504             wrkcars01 = *on;
076500060502       // F12=Ritorno
076600150304            when bSPCricez = 'L';
076700060504             clear s1opzione;
076800060504             wrkeofs01 = *on;
076900060502       // altrimenti
077000060504            other;
077100060504             *in22 = *on;
077200060504           endsl;
077300060502
077400150304           update tbspcs01;
077500060504           *in22 = *off;
077600060504       // fine opzione
077700060504          endif;
077800060502       // fine opzione
077900060502         endif;
078000060502
078100060502        enddo;
078200060502
078300060502       endsr;
078400060502
078500060502       // ------------------------------------------------------------------------
078600060502       // posizionamento sul file
078700060502       // ------------------------------------------------------------------------
078800060502       begsr sr_setlltbe01;
078900060502
079000060502        select;
079100060502       // chiave totale
079200060502         when wrkkey = '1';
079300060502          setll tbecod tntbe01l;
079400060502       // chiave parziale
079500060502         when wrkkey = '2';
079600060502          setll (tbecod:tbeke1) tntbe01l;
079700060502        endsl;
079800060502
079900060502       endsr;
080000060502
080100060502       // ------------------------------------------------------------------------
080200060502       // scrivo subfile
080300060502       // ------------------------------------------------------------------------
080400060502       begsr sr_wtrs01;
080500060502
080600060502       // se non raggiunto limite massimo di caricamento
080700060505        if s01nrr < 9999;
080800060502       // imposto campi di subfile
080900060502         exsr sr_sets01;
081000060502       // imposto numeratore righe e numero relativo di record
081100060505         s01nrr = s01nrr + 1;
081200060502       // scrivo subfile
081300150304         write tbspcs01;
081400060502        endif;
081500060502
081600060502       endsr;
081700060502
081800060502       // ------------------------------------------------------------------------
081900060502       // imposto campi del subfile
082000060502       // ------------------------------------------------------------------------
082100060502       begsr sr_sets01;
082200060502
082300060502        s1tbeke1 = tbeke1;
082400060505        s1tbeke2 = tbeke2;
082500150304        dspc = tbeuni;
082600150304        s1tbeuni = §SPCDEC1 ;
082700060502        s1tbeatb = tbeatb;
082800060502
082900060502       endsr;
083000060505
083100060505       // ------------------------------------------------------------------------
083200060505       // gestione tabelle in lingua
083300060505       // ------------------------------------------------------------------------
083400060505       begsr sr_lingua;
083500060505
083600060505       // pulisco il subfile
083700060505        exsr sr_pulsfl;
083800060505       // carico il subfile con i dati che trovo o vuoto
083900060505        exsr sr_carsfl;
084000060505
084100060505        wrksfl02 = *on;
084200060505
084300060505        dou wrksfl02 = *off;
084400060505         rec = 1;
084500060505         if savrec > *zeros;
084600060505          rec = savrec;
084700060505         endif;
084800150304         write tbspcp02;
084900150304         exfmt tbspcc02;
085000060505
085100060505         *in28 = *off;
085200060505         *in47 = *off;
085300060505
085400060505       // F12=Ritorno
085500060505          if *inkl;
085600060505           wrksfl02 = *off;
085700060508           leavesr;
085800060505          endif;
085900060505
086000060505       // Controlli
086100060505          if not *in05;
086200060505           exsr sr_ctrsfl;
086300060505          endif;
086400060505
086500060505       // F6=Conferma
086600060505          if *inkf and not *in28;
086700060508           exsr sr_confsfl;
086800060505           wrksfl02 = *off;
086900060505          endif;
087000060505
087100060505         enddo;
087200060505
087300060505       endsr;
087400060505
087500060505       // ------------------------------------------------------------------------
087600060505       // pulisco il subfile delle descrizioni in lingua
087700060505       // ------------------------------------------------------------------------
087800060505       begsr sr_pulsfl;
087900060505
088000060505        clear s02nrr;
088100060505        clear savrec;
088200060508        *in24 = *off;
088300060508        *in25 = *off;
088400150304        write tbspcc02;
088500060508        *in24 = *on;
088600060508        *in25 = *on;
088700060505
088800060505       endsr;
088900060505
089000060505       // ------------------------------------------------------------------------
089100060505       // carico il subfile delle descrizioni in lingua
089200060505       // ------------------------------------------------------------------------
089300060505       begsr sr_carsfl;
089400060505
089500060505        *in30 = *off;
089600060505       // carico un record per ogni lingua inserite in AZLIN
089700060505        clear lintntbe;
089800060505        setgt lintntbe azlin02l;
089900060505        dou %eof(azlin02l);
090000060505         read azlin02l;
090100060505       // fine file
090200060505         if %eof(azlin02l);
090300060505          leave;
090400060505         endif;
090500150304         clear dspc;
090600060505         s2dlin = lindesita;
090700150304         tbecod = 'SPC';
090800060505         tbeke1 = d1tbeke1;
090900060505         tbeke2 = d1tbeke2;
091000060505         tbelin = lintntbe;
091100060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
091200060508         if not %found(tntbe11l);
091300060505          clear s2cdes;
091400150304          clear s2cdes2;
091500150304          clear s2cnot1;
091600150306          clear s2cnot2;
091700060505         else;
091800150304          dspc = tbeuni;
091900150304          s2cdes = §spcdec1;
092000150304          s2cdes2= §spcdec2;
092100150304          s2cnot1= §spcnot1;
092200150306          s2cnot2= §spcnot2;
092300060505         endif;
092400060505         s2hcdlin = tbelin;
092500060505         s02nrr = s02nrr + 1;
092600150304         write tbspcs02;
092700060505        enddo;
092800060505
092900060505        *in30 = *on;
093000060505
093100060505       endsr;
093200060505
093300060505       // ------------------------------------------------------------------------
093400060505       // controllo il subfile delle descrizioni in lingua
093500060505       // ------------------------------------------------------------------------
093600060505       begsr sr_ctrsfl;
093700060505
093800060505        s02nrr = 1;
093900150304        chain s02nrr tbspcs02;
094000060505        dow %found;
094100150304         update tbspcs02;
094200060505         s02nrr = s02nrr + 1;
094300150304         chain s02nrr tbspcs02;
094400060505        enddo;
094500060505
094600060505       endsr;
094700060505
094800060505       // ------------------------------------------------------------------------
094900060505       // confermo il dati del subfile delle descrizioni in lingua
095000060505       // ------------------------------------------------------------------------
095100060505       begsr sr_confsfl;
095200060505
095300060505       // recupero i dati della tabella in italiano
095400060505        clear savtbeuni;
095500150304        tbecod = 'SPC';
095600060505        tbeke1 = d1tbeke1;
095700060505        tbeke2 = d1tbeke2;
095800060505        clear tbelin;
095900060531        chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
096000060508        if %found(tntbe11l);
096100060505         savtbeuni = tbeuni;
096200060505        endif;
096300060505
096400060505       // leggo il subfile e aggiorno/scrivo i dati in lingua
096500060505        s02nrr = 1;
096600150304        chain s02nrr tbspcs02;
096700060505        dow %found;
096800060505         tbelin = s2hcdlin;
096900060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
097000150304         dspc = savtbeuni;
097100150304         §spcdec1 = s2cdes;
097200150304         §spcdec2 = s2cdes2;
097300150304         §spcnot1 = s2cnot1;
097400150306         §spcnot2 = s2cnot2;
097500150304         tbeuni = dspc;
097600060508         tbeftt = w1ftt;
097700060505         clear tbeftr;
097800060505         clear tbedtr;
097900060505       // scrivo
098000060508         if not %found(tntbe11l);
098100150304          tbecod = 'SPC';
098200060505          tbeke1 = d1tbeke1;
098300060505          tbeke2 = d1tbeke2;
098400060505          tbelin = s2hcdlin;
098500060508          write tntbe11;
098600060505         else;
098700060505       // aggiorno
098800060508          update tntbe11;
098900060505         endif;
099000060505         s02nrr = s02nrr + 1;
099100150304         chain s02nrr tbspcs02;
099200060505        enddo;
099300060505
099400060505       endsr;
099500060428
099600060502       // ------------------------------------------------------------------------
099700060502       // routine iniziale
099800060502       // ------------------------------------------------------------------------
099900060428         begsr *inzsr;
100000060428
100100060428      /end-free
100200060428
100300060428     c     *entry        plist
100400060428     c                   parm                    kpjba
100500060428
100600060428      /free
100700060428         in(e) §azute;
100800060428         if not %error;
100900060428          in(e) §datiute;
101000060428         endif;
101100060502         if %error or rsut = *blanks;
101200060428          tibs34r(tibs34ds);
101300060428          in §azute;
101400060428          in §datiute;
101500060428         endif;
101600060428
101700060428         endsr;
101800060502
101900060502       // ------------------------------------------------------------------------
102000060502       // uscita dal programma
102100060502       // ------------------------------------------------------------------------
102200060502         begsr sr_uscita;
102300060502
102400150304          if bSPCesito = *blanks;
102500150304           bSPCesito = eseguito;
102600060502          endif;
102700060502
102800150304          kpjbu = tntbspcds;
102900060502
103000060502          *inlr = *on;
103100060502          return;
103200060502
103300060502         endsr;
103400060502
103500060502      /end-free
103600060502
103700060502** -MSG-                                                                     *
103800060502Immettere la causale                                                           01
103900060502Immettere la descrizione                                                       02
104000060510
104100060510
104200060502Causale già esistente                                                          05
