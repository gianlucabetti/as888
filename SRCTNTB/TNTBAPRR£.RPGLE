000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300090424      * Gestione tabella "APR" = Abilitazione indirizzo Ip per scarico spunte  *
000301090424      *                          da pistole radio                              *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000501090424     fazorg01l  if   e           k disk
000800090424     ftntbaprd  cf   e             workstn sfile(tbaps01:s01nrr)
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
001600060508      * 03 - proteggo campi video 01
001700060509      * 04 - visualizzazione
001900060508      * 20 - gestione subfile 01
002000060508      * 21 - gestione subfile 01
002100060508      * 22 - gestione subfile 01
002200060508      * 23 - gestione subfile 01
002500060428      * 28 - messaggio errore
002600090424      * 40 - Filiale di spunta
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
004000060505     d ktbeke1         s                   like(tbeke1)
004001090428     d camponum        s                   like(orgfil)
004100060428     d len             s              5u 0
004200060505     d s01nrr          s              4  0
004400060428     d savopzione      s                   like(s1opzione)
004500060428     d savrcdnbr       s                   like(c1rcdnbr)
004700060531     d savtbeftt       s                   like(tbeftt)
004800060531     d savtbeflt       s                   like(tbeflt)
004900060505     d savtbeuni       s                   like(tbeuni)
005100060428     d wrkkey          s              1
005200060428     d wrkeofs01       s              1
005300060428     d wrkcars01       s              1
005400060505     d wrkcarw01       s              1
005500060505     d wrksfl02        s              1
005600060504     d $video          s             10
005601090427     d ii              s              3  0
005602090428     d fil             s              3  0 dim(500)
005603090428     d des             s             20    dim(500)
005604090428     d iip             s              3    dim(500)
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006701090427     d og143         e ds
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
009600010423
009700060428      /free
009800060428
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
019300090424          write tbapp01;
019400090424          exfmt tbapc01;
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
024300090424         write tbapc01;
024400060428         *in20 = *off;
024500060428         *in21 = *off;
024600060428         *in22 = *off;
024700060428         *in23 = *off;
024800060428
024900060502       // imposto la chiave di posizionamento e lettura file
025000090424         tbecod = 'APR';
026000060428
026100090424       // posizionamento file
026201090424         setll tbecod tntbe01l;
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
027700060502       // scrivo subfile
027800060428           clear s1opzione;
027900060428           exsr sr_wtrs01;
028100060428
028200060428         enddo;
028201090428
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
031800060508         *in02 = *off;
031900060508         *in03 = *off;
032000060509         *in04 = *off;
032100060428
033400090424       // se immessa opzione di  'cancellazione/ripristino'
033500060508       // proteggo i campi del video
033600090424          if opzio = '04';
033700060508           *in03 = *on;
033800060508          endif;
033801090424
034400060502       // emetto il video
034500090424          exfmt tbapd01;
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
047300060428          d1descopz = 'Immissione';
048900060428
049000060502       // Opzione "04"=cancellazione/ripristino
049100090424         when opzio = '04';
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
052301120907        clear d1dfil;
052302120907        clear d1dsca;
052400010430
052401120907       // Filiale del profilo di spunta obbligatorio
052402120907        if d1tbeke1=*blanks or d1tbeke1=*zeros;
052700120907         vd1msg = msg(02);
052800060502         *in28 = *on;
052900060502         *in40 = *on;
053000060502         leavesr;
053100060502        endif;
053200060508
054501120126       // errore se > 254  e diverso da 999
054502120907       //if d1tbeke1>'254' and d1tbeke1<>'999';
054503120907       //   vd1msg = msg(01);
054504120907       //   *in28 = *on;
054505120907       //   *in40 = *on;
054506120907       //   leavesr;
054507120907       //endif;
054508120907       // errore se non è una filiale
054509120907         if d1tbeke1<>'999';
054510120907            camponum=%dec(d1tbeke1:5:0);
054511120907            ii=%lookup(camponum:fil);
054512120907            if ii=0;
054513120907               vd1msg = msg(02);
054514120907               *in28 = *on;
054515120907               *in40 = *on;
054516120907               leavesr;
054517120907            endif;
054518120907            d1dfil=des(ii);
054519120907         endif;
054520090508
054521090508       // Decodifico indirizzo IP: prima lo cerco come indirizzo ip e se non lo trovo lo cerco
054522090508       // come filiale
054523120907       //ii=%lookup(d1tbeke1:iip);
054524120907       //if ii=0;
054525120907       //   camponum=%dec(d1tbeke1:5:0);
054526120907       //   ii=%lookup(camponum:fil);
054527120907       //endif;
054528120907       //if ii>0;
054529120907       //   d1dfil=des(ii);
054530120907       //endif;
054700060502
054701120907       // Indirizzo ip abilitato al caricamento
054702120907        if d1tbeke2=*blanks or d1tbeke2=*zeros;
054703090508         vd1msg = msg(01);
054704090508         *in28 = *on;
054705090508         *in41 = *on;
054706090508         leavesr;
054707090508        endif;
054708090508
054722090508       // errore se > 254
054723090508         if d1tbeke2>'254';
054724090508            vd1msg = msg(01);
054725090508            *in28 = *on;
054726090508            *in41 = *on;
054727090508            leavesr;
054728090508         endif;
054730090508       // Decodifico indirizzo IP: prima lo cerco come indirizzo ip e se non lo trovo lo cerco
054731090508       // come filiale
054732090508         ii=%lookup(d1tbeke2:iip);
054733090508         if ii=0;
054734090508            camponum=%dec(d1tbeke2:5:0);
054735090508            ii=%lookup(camponum:fil);
054736090508         endif;
054737090508         if ii>0;
054738090508            d1dsca=des(ii);
054739090508         endif;
054740090508       // In immissione la chiave non deve essere già presente
054741090508        if comand = 'J';
054742090508         tbecod = 'APR';
054743090508         tbeke1 = d1tbeke1;
054744090508         tbeke2 = d1tbeke2;
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
057300090427          exfmt tbapw01;
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
066000060502        tbeke1 = d1tbeke1;
066100060509        tbeke2 = d1tbeke2;
066200060502
066700060502
066800060504        tbeftt = w1ftt;
066900060504        tbeflt = w1flt;
067000060502        clear tbeftr;
067100060502        clear tbedtr;
067200060502
067300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
067400060502        select;
067500060502
067600090427       // F10=immissione -
067700090427         when comand = 'J';
067701090427          clear tbelin;
067702090427          clear tbeuni;
067703090428          clear tbeatb;
067900060508          write tntbe000;
068500060502
068600060502       // Opzione "04"=cancellazione/ripristino
068700090424         when opzio = '04';
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
072600060504        clear d1tbeke1;
072601090424        clear d1tbeke2;
072602090424        clear d1dfil;
072603090507        clear d1dsca;
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
074700090508        chain ('APR':s1tbeke1:s1tbeke2) tntbe01l;
074800060502
074900060502       // imposto i campi a video
075000060505        d1tbeke2 = tbeke2;
075100060502        d1tbeke1 = tbeke1;
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
077100090427         readc tbaps01;
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
081900090427           update tbaps01;
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
085500060502       // imposto numeratore righe e numero relativo di record
085600060505         s01nrr = s01nrr + 1;
085700060502       // scrivo subfile
085800090424         write tbaps01;
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
086808090424        clear s1dfil;
086809090507        clear s1dsca;
086816090428        camponum=%dec(s1tbeke1:5:0);
086817120907       //ii=%lookup(s1tbeke1:iip);
086818120907       //if ii=0;
086819120907        ii=%lookup(camponum:fil);
086820120907       //endif;
086822090507        if ii>0;
086823090507           s1dfil=des(ii);
086824090507        endif;
086900060505        s1tbeke2 = tbeke2;
086901090507        camponum=%dec(s1tbeke2:5:0);
086902090507        ii=%lookup(s1tbeke2:iip);
086903090507        if ii=0;
086904090507           ii=%lookup(camponum:fil);
086905090507        endif;
086906090507        if ii>0;
086907090507           s1dsca=des(ii);
086908090507        endif;
087200060502        s1tbeatb = tbeatb;
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
106301090427       // Caricamento filiali di azorg con corrispondente eventuale indirizzo ip
106302090427         clear fil;
106303090427         clear iip;
106304090427         clear des;
106305090427         clear ii;
106306090427         setll *loval azorg01l;
106307090427         read azorg01l;
106308090427         dow not %eof;
106309090427       // Considero solo record non annullati e relativi a filiale/agenzia
106310090427            if orgfva=*blanks and (orgfag='F' or orgfag='A');
106311090427               ii+=1;
106312090427               fil(ii)=orgfil;
106313090427               des(II)=orgdes;
106314090427               og143=orgde3;
106315090427               if §ogiip<>*blanks;
106316090427                  iip(ii)=§ogiip;
106317090427               endif;
106318090427            endif;
106319090427            read azorg01l;
106320090427         enddo;
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
108600090508Indirizzo IP errato o mancante                                                 01
108700120907Filiale del profilo di spunta errato o mancante                                02
108800060510
108900060510
109000090508Codice già presente in tabella                                                 05
