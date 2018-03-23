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
061600160506        §SPCDEIB1= d1spcdeci1 ;
061700160506        §SPCDEIB2= d1spcdeci2 ;
061800150306        §spcnot1 = d1spcnot1+ d1spcnot12;
061900150306        §spcnot2 = d1spcnot2+ d1spcnot22;
062000060502
062100150304        tbeuni = dspc;
062200060502
062300060504        tbeftt = w1ftt;
062400060504        tbeflt = w1flt;
062500060502        clear tbeftr;
062600060502        clear tbedtr;
062700060502
062800060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
062900060502        select;
063000060502
063100060502       // F10=immissione - Opzione "03"=copia
063200150304         when bSPCcomand = 'J' or bSPCopzio = '03';
063300060502       // scrivo nuovo record con gestione errore per chiave duplicata
063400060508          write tntbe000;
063500060502
063600060502       // Opzione "02"=modifica
063700150304         when bSPCopzio = '02';
063800060502       // update record e controllo errore per chiave duplicata
063900060508          update tntbe000;
064000060502
064100060502       // Opzione "04"=cancellazione/ripristino
064200150304         when bSPCopzio = '04';
064300060502          update tntbe000;
064400010430
064500060502        endsl;
064600060509
064700060509       // se Opzione "04"=cancellazione/ripristino
064800060509       // aggiorno automaticamente anche le descrizioni in linea
064900150304        if bSPCopzio = '04';
065000060509         clear lintntbe;
065100060509         setgt lintntbe azlin02l;
065200060509         dou %eof(azlin02l);
065300060509          read azlin02l;
065400060509       // fine file
065500060509          if %eof(azlin02l);
065600060509           leave;
065700060509          endif;
065800060509       // aggiorno
065900060509          tbelin = lintntbe;
066000060531          chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
066100060509          if %found(tntbe11l);
066200060509           select;
066300060509            when tbeatb <> *blanks;
066400060509             clear tbeatb;
066500060509            when tbeatb = *blanks;
066600060509             tbeatb = 'A';
066700060509           endsl;
066800060509            update tntbe11;
066900060509          endif;
067000060509         enddo;
067100060509        endif;
067200060502
067300060502       endsr;
067400060502
067500060502       // ------------------------------------------------------------------------
067600060502       // pulizia video
067700060502       // ------------------------------------------------------------------------
067800060502       begsr sr_puld01;
067900060502
068000060504        clear d1descopz;
068100060504        clear d1tbeke1;
068200150304        clear d1spcdec1 ;
068300150304        clear d1spcdec2 ;
068400160506        clear d1spcdeci1 ;
068500160506        clear d1spcdeci2 ;
068600150304        clear d1spcnot1 ;
068700150306        clear d1spcnot12;
068800150306        clear d1spcnot2 ;
068900150306        clear d1spcnot22;
069000060504
069100060504        *in28 = *off;
069200060504        *in40 = *off;
069300060504        *in41 = *off;
069400060502
069500060502       endsr;
069600060502
069700060502       // ------------------------------------------------------------------------
069800060502       // imposto i dati a video
069900060502       // ------------------------------------------------------------------------
070000060502       begsr sr_imposta;
070100060502
070200060502       // recupero i dati dal file
070300060505        ktbeke1 = s1tbeke1;
070400060508        clear tbelin;
070500060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
070600060502
070700060502       // imposto i campi a video
070800060505        d1tbeke2 = tbeke2;
070900060502        d1tbeke1 = tbeke1;
071000150304        dspc = tbeuni;
071100160506        d1spcdec1 = §spcdec1;
071200160506        d1spcdec2 = §spcdec2;
071300160506        d1spcdeci1= §SPCDEIB1;
071400160506        d1spcdeci2= §SPCDEIB2;
071500150306        d1spcnot1 = §spcnot1;
071600150306        d1spcnot12= %SUBST(§spcnot1:36:35);
071700150306        d1spcnot2 = §spcnot2;
071800150306        d1spcnot22= %SUBST(§spcnot2:36:35);
071900060502
072000060502       endsr;
072100060502
072200060502       // ------------------------------------------------------------------------
072300060502       // gestione del subfile
072400060502       // ------------------------------------------------------------------------
072500060502       begsr sr_gestsfl;
072600060502
072700060502       // set flag
072800060502        wrkeofs01 = *off;
072900060502
073000060502       // inizio lettura subfile
073100060502        dow wrkeofs01 = *off and *in21;
073200150304         readc tbspcs01;
073300060502       // se fine subfile
073400060502         if %eof;
073500060502          wrkcars01 = *on;
073600060502          leave;
073700060502         endif;
073800060502       // se è stata immessa un'opzione
073900060502         if s1opzione <> *zeros;
074000060502       // reset ds di servizio
074100150304          clear tntbspcds;
074200060502       // controllo ed elaborazione opzione immessa
074300060502          select;
074400060502       // opzione "01"=scelta
074500060502           when s1opzione = 1 and *in01;
074600150304            bSPCopzio = '01';
074700060502       // opzione "02"=modifica
074800060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
074900150304            bSPCopzio = '02';
075000060502       // opzione "03"=copia
075100060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
075200150304            bSPCopzio = '03';
075300060502       // opzione "04"=annullo/ripristino
075400060502           when s1opzione = 4 and not *in01;
075500150304            bSPCopzio = '04';
075600060502       // opzione "05"=visualizzazione
075700060502           when s1opzione = 5;
075800150304            bSPCopzio = '05';
075900060502          endsl;
076000010503
076100060502       // se immessa almeno un opzione valida
076200150304          if  bSPCopzio <> *blanks;
076300060502       // gstione del formato video
076400060504           exsr sr_d01;
076500060502       // se la gestione si è chiusa con ...
076600060504           select;
076700060502       // F6=Conferma
076800150304            when bSPCricez = 'F';
076900060504             clear s1opzione;
077000060504             wrkcars01 = *on;
077100060502       // F12=Ritorno
077200150304            when bSPCricez = 'L';
077300060504             clear s1opzione;
077400060504             wrkeofs01 = *on;
077500060502       // altrimenti
077600060504            other;
077700060504             *in22 = *on;
077800060504           endsl;
077900060502
078000150304           update tbspcs01;
078100060504           *in22 = *off;
078200060504       // fine opzione
078300060504          endif;
078400060502       // fine opzione
078500060502         endif;
078600060502
078700060502        enddo;
078800060502
078900060502       endsr;
079000060502
079100060502       // ------------------------------------------------------------------------
079200060502       // posizionamento sul file
079300060502       // ------------------------------------------------------------------------
079400060502       begsr sr_setlltbe01;
079500060502
079600060502        select;
079700060502       // chiave totale
079800060502         when wrkkey = '1';
079900060502          setll tbecod tntbe01l;
080000060502       // chiave parziale
080100060502         when wrkkey = '2';
080200060502          setll (tbecod:tbeke1) tntbe01l;
080300060502        endsl;
080400060502
080500060502       endsr;
080600060502
080700060502       // ------------------------------------------------------------------------
080800060502       // scrivo subfile
080900060502       // ------------------------------------------------------------------------
081000060502       begsr sr_wtrs01;
081100060502
081200060502       // se non raggiunto limite massimo di caricamento
081300060505        if s01nrr < 9999;
081400060502       // imposto campi di subfile
081500060502         exsr sr_sets01;
081600060502       // imposto numeratore righe e numero relativo di record
081700060505         s01nrr = s01nrr + 1;
081800060502       // scrivo subfile
081900150304         write tbspcs01;
082000060502        endif;
082100060502
082200060502       endsr;
082300060502
082400060502       // ------------------------------------------------------------------------
082500060502       // imposto campi del subfile
082600060502       // ------------------------------------------------------------------------
082700060502       begsr sr_sets01;
082800060502
082900060502        s1tbeke1 = tbeke1;
083000060505        s1tbeke2 = tbeke2;
083100150304        dspc = tbeuni;
083200150304        s1tbeuni = §SPCDEC1 ;
083300060502        s1tbeatb = tbeatb;
083400060502
083500060502       endsr;
083600060505
083700060505       // ------------------------------------------------------------------------
083800060505       // gestione tabelle in lingua
083900060505       // ------------------------------------------------------------------------
084000060505       begsr sr_lingua;
084100060505
084200060505       // pulisco il subfile
084300060505        exsr sr_pulsfl;
084400060505       // carico il subfile con i dati che trovo o vuoto
084500060505        exsr sr_carsfl;
084600060505
084700060505        wrksfl02 = *on;
084800060505
084900060505        dou wrksfl02 = *off;
085000060505         rec = 1;
085100060505         if savrec > *zeros;
085200060505          rec = savrec;
085300060505         endif;
085400150304         write tbspcp02;
085500150304         exfmt tbspcc02;
085600060505
085700060505         *in28 = *off;
085800060505         *in47 = *off;
085900060505
086000060505       // F12=Ritorno
086100060505          if *inkl;
086200060505           wrksfl02 = *off;
086300060508           leavesr;
086400060505          endif;
086500060505
086600060505       // Controlli
086700060505          if not *in05;
086800060505           exsr sr_ctrsfl;
086900060505          endif;
087000060505
087100060505       // F6=Conferma
087200060505          if *inkf and not *in28;
087300060508           exsr sr_confsfl;
087400060505           wrksfl02 = *off;
087500060505          endif;
087600060505
087700060505         enddo;
087800060505
087900060505       endsr;
088000060505
088100060505       // ------------------------------------------------------------------------
088200060505       // pulisco il subfile delle descrizioni in lingua
088300060505       // ------------------------------------------------------------------------
088400060505       begsr sr_pulsfl;
088500060505
088600060505        clear s02nrr;
088700060505        clear savrec;
088800060508        *in24 = *off;
088900060508        *in25 = *off;
089000150304        write tbspcc02;
089100060508        *in24 = *on;
089200060508        *in25 = *on;
089300060505
089400060505       endsr;
089500060505
089600060505       // ------------------------------------------------------------------------
089700060505       // carico il subfile delle descrizioni in lingua
089800060505       // ------------------------------------------------------------------------
089900060505       begsr sr_carsfl;
090000060505
090100060505        *in30 = *off;
090200060505       // carico un record per ogni lingua inserite in AZLIN
090300060505        clear lintntbe;
090400060505        setgt lintntbe azlin02l;
090500060505        dou %eof(azlin02l);
090600060505         read azlin02l;
090700060505       // fine file
090800060505         if %eof(azlin02l);
090900060505          leave;
091000060505         endif;
091100150304         clear dspc;
091200060505         s2dlin = lindesita;
091300150304         tbecod = 'SPC';
091400060505         tbeke1 = d1tbeke1;
091500060505         tbeke2 = d1tbeke2;
091600060505         tbelin = lintntbe;
091700060531         chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
091800060508         if not %found(tntbe11l);
091900060505          clear s2cdes;
092000150304          clear s2cdes2;
092100150304          clear s2cnot1;
092200150306          clear s2cnot2;
092300060505         else;
092400150304          dspc = tbeuni;
092500150304          s2cdes = §spcdec1;
092600150304          s2cdes2= §spcdec2;
092700150304          s2cnot1= §spcnot1;
092800150306          s2cnot2= §spcnot2;
092900060505         endif;
093000060505         s2hcdlin = tbelin;
093100060505         s02nrr = s02nrr + 1;
093200150304         write tbspcs02;
093300060505        enddo;
093400060505
093500060505        *in30 = *on;
093600060505
093700060505       endsr;
093800060505
093900060505       // ------------------------------------------------------------------------
094000060505       // controllo il subfile delle descrizioni in lingua
094100060505       // ------------------------------------------------------------------------
094200060505       begsr sr_ctrsfl;
094300060505
094400060505        s02nrr = 1;
094500150304        chain s02nrr tbspcs02;
094600060505        dow %found;
094700150304         update tbspcs02;
094800060505         s02nrr = s02nrr + 1;
094900150304         chain s02nrr tbspcs02;
095000060505        enddo;
095100060505
095200060505       endsr;
095300060505
095400060505       // ------------------------------------------------------------------------
095500060505       // confermo il dati del subfile delle descrizioni in lingua
095600060505       // ------------------------------------------------------------------------
095700060505       begsr sr_confsfl;
095800060505
095900060505       // recupero i dati della tabella in italiano
096000060505        clear savtbeuni;
096100150304        tbecod = 'SPC';
096200060505        tbeke1 = d1tbeke1;
096300060505        tbeke2 = d1tbeke2;
096400060505        clear tbelin;
096500060531        chain(n) (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
096600060508        if %found(tntbe11l);
096700060505         savtbeuni = tbeuni;
096800060505        endif;
096900060505
097000060505       // leggo il subfile e aggiorno/scrivo i dati in lingua
097100060505        s02nrr = 1;
097200150304        chain s02nrr tbspcs02;
097300060505        dow %found;
097400060505         tbelin = s2hcdlin;
097500060531         chain (tbelin:tbecod:tbeke1:tbeke2) tntbe11l;
097600150304         dspc = savtbeuni;
097700150304         §spcdec1 = s2cdes;
097800150304         §spcdec2 = s2cdes2;
097900150304         §spcnot1 = s2cnot1;
098000150306         §spcnot2 = s2cnot2;
098100160506         clear §SPCDEIB1 ;
098200160506         clear §SPCDEIB2 ;
098300150304         tbeuni = dspc;
098400060508         tbeftt = w1ftt;
098500060505         clear tbeftr;
098600060505         clear tbedtr;
098700060505       // scrivo
098800060508         if not %found(tntbe11l);
098900150304          tbecod = 'SPC';
099000060505          tbeke1 = d1tbeke1;
099100060505          tbeke2 = d1tbeke2;
099200060505          tbelin = s2hcdlin;
099300060508          write tntbe11;
099400060505         else;
099500060505       // aggiorno
099600060508          update tntbe11;
099700060505         endif;
099800060505         s02nrr = s02nrr + 1;
099900150304         chain s02nrr tbspcs02;
100000060505        enddo;
100100060505
100200060505       endsr;
100300060428
100400060502       // ------------------------------------------------------------------------
100500060502       // routine iniziale
100600060502       // ------------------------------------------------------------------------
100700060428         begsr *inzsr;
100800060428
100900060428      /end-free
101000060428
101100060428     c     *entry        plist
101200060428     c                   parm                    kpjba
101300060428
101400060428      /free
101500060428         in(e) §azute;
101600060428         if not %error;
101700060428          in(e) §datiute;
101800060428         endif;
101900060502         if %error or rsut = *blanks;
102000060428          tibs34r(tibs34ds);
102100060428          in §azute;
102200060428          in §datiute;
102300060428         endif;
102400060428
102500060428         endsr;
102600060502
102700060502       // ------------------------------------------------------------------------
102800060502       // uscita dal programma
102900060502       // ------------------------------------------------------------------------
103000060502         begsr sr_uscita;
103100060502
103200150304          if bSPCesito = *blanks;
103300150304           bSPCesito = eseguito;
103400060502          endif;
103500060502
103600150304          kpjbu = tntbspcds;
103700060502
103800060502          *inlr = *on;
103900060502          return;
104000060502
104100060502         endsr;
104200060502
104300060502      /end-free
104400060502
104500060502** -MSG-                                                                     *
104600060502Immettere la causale                                                           01
104700060502Immettere la descrizione                                                       02
104800060510
104900060510
105000060502Causale già esistente                                                          05
