000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120620      * Gestione tabella "FLA" = Forzatura linea di arrivo per linea di partenz*
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000501090424     fazorg01l  if   e           k disk
000800120620     ftntbflad  cf   e             workstn sfile(tbfls01:s01nrr)
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
001301120621      * 02 - visualizzazione
001600060508      * 03 - proteggo campi video 01
001700120621      * 04 - manutenzione
001900060508      * 20 - gestione subfile 01
002000060508      * 21 - gestione subfile 01
002100060508      * 22 - gestione subfile 01
002200060508      * 23 - gestione subfile 01
002500060428      * 28 - messaggio errore
002700090424      * 41 - indirizzo IP abilitato
002900060502
003000060502      *------------------------------------------------------------------------*
003100060502      * schiere
003200060502      *------------------------------------------------------------------------*
003300060502     d msg             s             78    dim(17) ctdata perrcd(1)
003400060427
003500060428      *------------------------------------------------------------------------*
003600060428      * campi di work
003700060428      *------------------------------------------------------------------------*
003701090428     d data_eur        s               d   datfmt(*eur)
003800060428     d blanks          s                   like(d1descopz)
003900090427     d comand          s              1
003901090427     d opzio           s              2
003902090427     d ricez           s              1
004100060428     d len             s              5u 0
004200060505     d s01nrr          s              4  0
004400060428     d savopzione      s                   like(s1opzione)
004500060428     d savrcdnbr       s                   like(c1rcdnbr)
004700060531     d savtbeftt       s                   like(tbeftt)
004800060531     d savtbeflt       s                   like(tbeflt)
004900060505     d savtbeuni       s                   like(tbeuni)
004901120620     d kfil            s                   like(orgfil)
005200060428     d wrkeofs01       s              1
005300060428     d wrkcars01       s              1
005400060505     d wrkcarw01       s              1
005500060505     d wrksfl02        s              1
005600060504     d $video          s             10
005601120620     d i               s              3  0 inz
005602120620     d w0030           s              3  0 inz
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006101120620     d campolp         ds           255
006103120620     d  D01LP1
006104120620     d  D01LP2
006105120620     d  D01LP3
006106120620     d  D01LP4
006107120620     d  D01LP5
006108120620     d  D01LP6
006109120620     d  D01LP7
006110120620     d  D01LP8
006111120620     d  D01LP9
006112120620     d  D01L10
006113120620     d  D01L11
006114120620     d  D01L12
006115120620     d  D01L13
006116120620     d  D01L14
006117120620     d  D01L15
006118120620     d  D01L16
006119120620     d  D01L17
006120120620     d  D01L18
006121120620     d  D01L19
006122120620     d  D01L20
006123120620     d  D01L21
006124120620     d  D01L22
006125120620     d  D01L23
006126120620     d  D01L24
006127120620     d  D01L25
006128120620     d  D01L26
006129120620     d  D01L27
006130120620     d  D01L28
006131120620     d  D01L29
006132120620     d  D01L30
006133120620     d  D01L31
006134120620     d  D01L32
006135120620     d  D01L33
006136120620     d  D01L34
006137120620     d  D01L35
006138120620     d  D01L36
006139120620     d  D01L37
006140120620     d  D01L38
006141120620     d  D01L39
006142120620     d  D01L40
006143120620     d  D01L41
006144120620     d  D01L42
006145120620     d  D01L43
006146120620     d  D01L44
006147120620     d  D01L45
006148120620     d  D01L46
006149120620     d  D01L47
006150120620     d  D01L48
006151120620     d  D01L49
006152120620     d  D01L50
006153120620     d  D01L51
006154120620     d  D01L52
006155120620     d  D01L53
006156120620     d  D01L54
006157120620     d  D01L55
006158120620     d  D01L56
006159120620     d  D01L57
006160120620     d  D01L58
006161120620     d  D01L59
006162120620     d  D01L60
006163120620     d  D01L61
006164120620     d  D01L62
006165120620     d  D01L63
006166120620     d  D01L64
006167120620     d  D01L65
006168120620     d  D01L66
006169120620     d  D01L67
006170120620     d  D01L68
006171120620     d  D01L69
006172120620     d  D01L70
006173120620     d  D01L71
006174120620     d  D01L72
006175120620     d  D01L73
006176120620     d  D01L74
006177120620     d  D01L75
006178120620     d  D01L76
006179120620     d  D01L77
006180120620     d  D01L78
006181120620     d  D01L79
006182120620     d  D01L80
006183120620     d  D01L81
006184120620     d  D01L82
006185120620     d  D01L83
006186120620     d  D01L84
006187120620     d  D01L85
006188120620     D  Slp                    1    255    dim(85)
006189120621     d  wlp                    1    255
006224120620     d
006900060502     d kpjba         e ds
007000060503     d tibs34ds      e ds                  inz
007200060428     d §azute        e ds                  extname(azute00f)
007300060428     d                                     dtaara
007400060428     d §datiute      e ds                  extname(ddatiute)
007500060428     d                                     dtaara
007600010503
007700060428     d psds           sds
007800060428     d  pgmname          *proc
007900060428
008701090424      // ? PROTOTIPI ?
008702090424      /copy gaitrasrc/srcprotopr,tibs34r
008800060428
008900060428      *------------------------------------------------------------------------*
009000060428      * costanti
009100060428      *------------------------------------------------------------------------*
009200060428     d errore          c                   '1'
009300060428     d eseguito        c                   '0'
009400010503
009500060428      *------------------------------------------------------------------------*
009800060428
009801120620      /free
009802120621       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009803120621
010000060428       exsr sr_c01;
010100060428       exsr sr_uscita;
010101090424
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
017100090427
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
019300120618          write tbflp01;
019400120618          exfmt tbflc01;
019500060428
019600060502       // controllo tasti funzionali del subfile
019700060428          select;
019800060428
019900060502       // F3=Fine
020000060428           when *inkc;
020100060502            $video = *blanks;
020101090427            ricez='C';
020300060428            exsr sr_uscita;
020400060428
020500060502       // F5=Refresh
020600060428           when *inke;
020700060428            wrkcars01 = *on;
020800060428
020900060502       // F10=Immissione
021000060428           when *inkj;
021100060502            $video = 'D01';
021101090427            comand='J';
021102120703            opzio='  ';
021500060428            exsr sr_d01;
021600060428
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
024300120615         write tbflc01;
024400060428         *in20 = *off;
024500060428         *in21 = *off;
024600060428         *in22 = *off;
024700060428         *in23 = *off;
024701120620       // imposto la chiave di posizionamento e lettura file
024702120620         tbecod = 'FLA';
024703120620       // posizionamento file
024704120620         setll tbecod tntbe01l;
024705120620       // fino a che non è fine file
024706120620         dou %eof(tntbe01l);
024707120620
024708120620       // leggo file
024709120620          reade(n) tbecod tntbe01l;
024710120620
024711120620       // fine file esco
024712120620          if %eof(tntbe01l);
024713120620           leave;
024714120620          endif;
024715120620
024716120620       // scrivo subfile
024717120620           clear s1opzione;
024718120620           exsr sr_wtrs01;
024719120620
024732120615        enddo;
024800060428
028202090428         clear opzio;
028203090428         clear comand;
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
031900120621         *in02 = *off;
031901120621         *in03 = *off;
031902120621         *in04 = *off;
032100060428
033400090424       // se immessa opzione di  'cancellazione/ripristino'
033500060508       // proteggo i campi del video
033600120621          if opzio = '04';
033700060508           *in03 = *on;
033800060508          endif;
033801120621          if opzio = '05';
033802120621           *in03 = *on;
033803120621           *in02 = *on;
033804120621          endif;
033805120621       // se immessa opzione di  manutenzione
033806120621       // proteggo i campi del video che costituiscono kla chiave
033807120621          if opzio = '02';
033808120621           *in04 = *on;
033809120621          endif;
033810090424
034400060502       // emetto il video
034500120618          exfmt tbfld01;
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
035801090427           ricez='C';
036000060502           $video = *blanks;
036100060505           unlock tntbe01l;
036200060428           exsr sr_uscita;
036300060428
036400060502       // F6=Conferma
036500060428          when *inkf;
036501090427          ricez='F';
036700060502       // controllo e decodifico i dati del video
036800060428           exsr sr_ctrd01;
036900060502       // conferma per annullo/ripristino
037000090424           if opzio = '04';
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
040000060428
040100060502       // F12=Ritorno
040200060428          when *inkl;
040300060428           clear s1opzione;
040301090427           ricez='L';
040500060505           unlock tntbe01l;
040600090424       // se non è f12 da immissione non ricarico il subfile
040700090424           if comand = 'J';
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
045900060428
046000060502       // ------------------------------------------------------------------------
046100060502       // imposto i dati a video
046200060502       // ------------------------------------------------------------------------
046300060428       begsr sr_setvideo;
046400060428
046500060502       // pulisco il formato video D01
046600060502        exsr sr_puld01;
046700060428
046800090424       // controllo se 'immissione' o altro
046900060428        select;
047000060428
047100060502       // F10=Immissione
047200090424         when comand = 'J';
047300120704          d1descopz = 'Immissione';
048900060428
049000060502       // Opzione "04"=cancellazione/ripristino
049100090424         when opzio = '04';
049200060428          exsr sr_imposta;
049300060502       // se richiesta 'cancellazione'
049400060502          if tbeatb = *blanks;
049500120704           d1descopz = 'Annullamento';
049600060502          endif;
049700060502       // se richiesto 'ripristino'
049800060502          if tbeatb = 'A';
049900120704           d1descopz = 'Ripristino';
050000060502          endif;
050001120621       // Opzione "05"=visualizzazione o "02"=manutenzione
050002120703         when opzio = '05' or opzio='02' ;
050003120703          if opzio= '05';
050004120704           d1descopz = 'Visualizzazione';
050005120703          else;
050006120704           d1descopz = 'Manutenzione';
050007120703          endif;
050008120621          exsr sr_imposta;
050100060502
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
052301120621        *in42 = *off;
052400010430
052401120620       // Linea di arrivo obbligatoria
052402120620        if d01lna=*zeros;
052700060502         vd1msg = msg(01);
052800060502         *in28 = *on;
052900060502         *in40 = *on;
053000060502         leavesr;
053100060502        endif;
053200060508
054501120620       // errore se non è una lna
054502120620        chain d01lna azorg01l;
054503120620        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
054505120620           vd1msg = msg(01);
054506120620           *in28 = *on;
054507120620           *in40 = *on;
054508120620           leavesr;
054509120620        endif;
054510120620        d01lnad=orgdes;
054511120620
054512120620       // Linea di arrivo forzata obbligatoria
054513120620        if d01lfz=*zeros;
054514120620         vd1msg = msg(01);
054515120620         *in28 = *on;
054516120620         *in41 = *on;
054517120620         leavesr;
054518120620        endif;
054519120620
054520120620       // errore se non è una lna
054521120620        chain d01lfz azorg01l;
054522120620        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
054523120620           vd1msg = msg(01);
054524120620           *in28 = *on;
054525120620           *in41 = *on;
054526120620           leavesr;
054527120620        endif;
054528120620        d01lfzd=orgdes;
054529090508
054530120620       // Linee di partenza: devono esistere. E' ammesso 999 per
054531120620       // indicarle tutte
054700120620        for I=1 to %elem(slp);
054701120620           if slp(i)<>*zeros and slp(i)<>*blanks and slp(i)<>'999';
054702120620              // può contenere solo numeri
054703120620      /end-free
054704120620     C                   testn                   slp(i)               30
054705120620    2c                   if             *in30  = *off
054706120620     c                             or  %subst(slp(i): 3: 1) < *zero
054707120620     c                   eval      vd1msg = msg(02)
054708120621     c                   eval      %subst(vd1msg:19:3)=slp(i)
054709120620     c                   eval      *in28 = *on
054710120621     c                   eval      *in42 = *on
054711120620     c                   leavesr
054712120620    2c                   endif
054713120620      /free
054714120620              w0030=%dec(slp(i):3:0);
054715120620              chain w0030 azorg01l;
054716120620              if not %found (azorg01l) or (orgfag <>'A' and orgfag<>'F');
054717120620                 vd1msg = msg(02);
054718120621                 eval %subst(vd1msg:19:3)=slp(i);
054719120620                 *in28 = *on;
054720120621                 *in42 = *on;
054721120620                 leavesr;
054722120620              endif;
054723120621           // controllo che la linea non sia ripetuta
054724120621              if i>1 ;
054725120621                 if %lookup(slp(i):slp:1:(i-1))>0;
054726120621                    vd1msg = msg(03);
054727120621                    eval %subst(vd1msg:19:3)=slp(i);
054728120621                    *in28 = *on;
054729120621                    *in42 = *on;
054730120621                    leavesr;
054731120621                 endif;
054732120621              endif;
054733120620           endif;
054734120703           if slp(i)=*zeros;
054735120703              slp(i)='   ' ;
054736120703           endif;
054737120620        endfor;
054738090508
054740090508       // In immissione la chiave non deve essere già presente
054741090508        if comand = 'J';
054742120620         tbecod = 'FLA';
054743120620         tbeke1 = %editc(d01lna:'X');
054744120620         tbeke2 = %editc(d01lfz:'X');
054745090508         chain(n) (tbecod:tbeke1:tbeke2) tntbe01l;
054746090508         if %found(tntbe01l);
054748090508            vd1msg = msg(05);
054749090508            *in28 = *on;
054750090508            *in40 = *on;
054751090508            leavesr;
054753090508         endif;
054754090508        endif;
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
057300120618          exfmt tbflw01;
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
059200090424             if comand = 'J';
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
060301090427           clear ricez;
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
062900090424        if comand <> 'J';
063000060504         w1ftt = tbeftt;
063100060504         w1flt = tbeflt;
063200060504         w1ftr = tbeftr;
063300060504       // imposto la data
063400060504         if tbedtr <> *zeros;
063401090428          data_eur=%date(tbedtr:*iso);
063402090428          w1dtr=%dec(data_eur);
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
066000120620        tbeke1 = %editc(d01lna:'X');
066100120620        tbeke2 = %editc(d01lfz:'X');
066200060502
066700060502
066800060504        tbeftt = w1ftt;
066900060504        tbeflt = w1flt;
067000060502        clear tbeftr;
067100060502        clear tbedtr;
067102120621        exec sql SET :wlp = REPLACE(:wlp, ' ' , '');
067104120621        tbeuni=campolp;
067200060502
067300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
067400060502        select;
067500060502
067600090427       // F10=immissione -
067700090427         when comand = 'J';
067701090427          clear tbelin;
067703090428          clear tbeatb;
067900060508          write tntbe000;
068500060502
068600060502       // Opzione "04"=cancellazione/ripristino
068601120621       // Opzione "02"=manutenzione
068700120621         when opzio = '04' or opzio='02';
068800060502          update tntbe000;
068900010430
069000060502        endsl;
071700060502
071800060502       endsr;
071900060502
072000060502       // ------------------------------------------------------------------------
072100060502       // pulizia video
072200060502       // ------------------------------------------------------------------------
072300060502       begsr sr_puld01;
072400060502
072500060504        clear d1descopz;
072600120618        clear d01lna  ;
072601120618        clear d01lnad ;
072602120618        clear d01lfz  ;
072603120618        clear d01lfzd ;
072604120620        clear campolp ;
072900060504
073000090115        *in06 = *off;
073100090115        *in28 = *off;
073200120620        *in40 = *off;
073201120620        *in41 = *off;
073202120621        *in42 = *off;
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
074700120620        chain ('FLA':s1tbeke1:s1tbeke2) tntbe01l;
074701120620        if %found(tntbe01l);
074702120620           campolp=tbeuni;
074703120620        endif;
074800060502
074900060502       // imposto i campi a video
075000120620        d01lna = %dec(s1tbeke1:3:0);
075001120621        d01lnad= s1dlna;
075100120620        d01lfz = %dec(s1tbeke2:3:0);
075101120621        d01lfzd= s1dlfz;
075500090115        if tbeatb<>' '   ;
075600120620           *in06=*on    ;
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
077100120618         readc tbfls01;
077200060502       // se fine subfile
077300060502         if %eof;
077400060502          wrkcars01 = *on;
077500060502          leave;
077600060502         endif;
077700060502       // se è stata immessa un'opzione
077800060502         if s1opzione <> *zeros;
078100060502       // controllo ed elaborazione opzione immessa
078200060502          select;
079200060502       // opzione "04"=annullo/ripristino
079300090428           when s1opzione = 4;
079400090424            opzio = '04';
079401120621       // opzione "02"=manutenzione
079402120621           when s1opzione = 2;
079403120621            opzio = '02';
079404120704            if s1tbeatb='Ann';
079405120704               clear opzio;
079406120704               clear s1opzione;
079408120704               update tbfls01;
079409120704            endif;
079410120621       // opzione "05"=Visualizzazione
079411120621           when s1opzione = 5;
079412120621            opzio = '05';
079800060502          endsl;
079900010503
080000060502       // se immessa almeno un opzione valida
080100090424          if  opzio <> *blanks;
080200060502       // gstione del formato video
080300060504           exsr sr_d01;
080400060502       // se la gestione si è chiusa con ...
080500060504           select;
080600060502       // F6=Conferma
080700090427            when ricez = 'F';
080800060504             clear s1opzione;
080900060504             wrkcars01 = *on;
081000060502       // F12=Ritorno
081100090427            when ricez = 'L';
081200060504             clear s1opzione;
081300060504             wrkeofs01 = *on;
081400060502       // altrimenti
081500060504            other;
081600060504             *in22 = *on;
081700060504           endsl;
081800060502
081900120618           update tbfls01;
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
085401120703         if s1tbeke1<>*blanks;
085402120620       // imposto numeratore righe e numero relativo di record
085403120620         s01nrr = s01nrr + 1;
085404120620       // scrivo subfile
085405120620         write tbfls01;
085406120703         endif;
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
086801120615        s1tbeke2 = tbeke2;
086808120615        clear s1dlna;
086809120615        clear s1dlfz;
086810120618        // Decodifica delle due linee di arrivo
086811120703      /end-free
086812120703     C                   testn                   s1tbeke1             30
086813120703    2c                   if             *in30  = *off
086814120703     c                             or  %subst(s1tbeke1: 3: 1) < *zero
086815120703     c                   clear                   s1tbeke1
086816120703     c                   leavesr
086817120703     c                   endif
086818120703      /free
086819120618        kfil=%dec(s1tbeke1:3:0);
086820120618        chain (kfil) azorg01l;
086821120618        s1dlna=orgdes;
086822120618        kfil=%dec(s1tbeke2:3:0);
086823120618        chain (kfil) azorg01l;
086824120618        s1dlfz=orgdes;
086825120703        if tbeatb='A';
087200120703           s1tbeatb = 'Ann';
087201120703        else;
087202120703           clear s1tbeatb;
087203120703        endif;
087300060502
087400060502       endsr;
087500060505
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
107800060502          *inlr = *on;
107900060502          return;
108000060502
108100060502         endsr;
108200060502
108300060502      /end-free
108400060502
108500060502** -MSG-                                                                     *
108600120620Linea di arrivo mancante o errata                                              01
108700120621Linea di partenza xxx mancante o errata                                        02
108800120621Linea di partenza xxx indicata più volte                                       03
108900060510
109000120620Chiave già presente in tabella                                                 05
