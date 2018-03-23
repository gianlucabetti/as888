000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120622      * Gestione tabella "FLB" = Forzatura linea di arrivo per cliente mittente*
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000501090424     fazorg01l  if   e           k disk
000800120615     ftntbflbd  cf   e             workstn sfile(tbfls01:s01nrr)
000801120706     f                                     sfile(tbfls02:s02nrr)
001100120622
001200120622      *------------------------------------------------------------------------*
001300120622      * riepilogo indicatori
001400120622      *------------------------------------------------------------------------*
001500120622      * 02 - visualizzazione
001600120622      * 03 - proteggo campi video 01
001700120622      * 04 - manutenzione
001800120622      * 20 - gestione subfile 01
001900120622      * 21 - gestione subfile 01
002000120622      * 22 - gestione subfile 01
002100120622      * 23 - gestione subfile 01
002101120706      * 24 - gestione subfile 02
002102120706      * 25 - gestione subfile 02
002103120706      * 26 - gestione subfile 02
002104120706      * 27 - gestione subfile 02
002200120622      * 28 - messaggio errore
002202120706      * 40/42Errori campi subfile/control
002400120622
002500120622      *------------------------------------------------------------------------*
002600120622      * schiere
002700120622      *------------------------------------------------------------------------*
002800120622     d msg             s             78    dim(17) ctdata perrcd(1)
002900120622
003000120622      *------------------------------------------------------------------------*
003100120622      * campi di work
003200120622      *------------------------------------------------------------------------*
003300120622     d data_eur        s               d   datfmt(*eur)
003400120622     d blanks          s                   like(d1descopz)
003500120622     d comand          s              1
003600120622     d opzio           s              2
003700120622     d ricez           s              1
003800120622     d len             s              5u 0
003900120622     d s01nrr          s              4  0
003901120706     d s02nrr          s              4  0
004000120622     d savopzione      s                   like(s1opzione)
004100120622     d savrcdnbr       s                   like(c1rcdnbr)
004101120706     d sv2rcdnbr       s                   like(c2rcdnbr)
004200120622     d savtbeftt       s                   like(tbeftt)
004300120622     d savtbeflt       s                   like(tbeflt)
004400120622     d savtbeuni       s                   like(tbeuni)
004500120622     d kfil            s                   like(orgfil)
004600120622     d wrkeofs01       s              1
004700120622     d wrkcars01       s              1
004800120622     d wrkcarw01       s              1
004900120622     d wrksfl02        s              1
005000120622     d $video          s             10
005100120622     d i               s              3  0 inz
005200120622     d w0030           s              3  0 inz
005300120622
005400120622      *------------------------------------------------------------------------*
005500120622      * ds interne/esterne
005600120622      *------------------------------------------------------------------------*
005601120706     d campocm         ds           252
005602120706     D  Scm                    1    252    dim(36)
005700120706     d campocms01      ds            21
005800120706     d  s01cm1
005900120706     d  s01CM2
006000120706     d  s01CM3
006100120706     d* s01CM4
006200120706     d* s01CM5
006300120706     d* s01CM6
006400120706     d* s01CM7
014500120622     d
014501120622     d TIBS69DS      E DS                  INZ
014502120622     d DS_cnaco      E DS                  extname(CNACO00F)
014503120622     d DS_cnind      E DS                  extname(CNIND00F)
014504120622     d DS_cnclp      E DS                  extname(CNCLP00F)
014505120622     d DS_fncls      E DS                  extname(FNCLS00F)
014600120622     d kpjba         e ds
014700120622     d tibs34ds      e ds                  inz
014800120622     d §azute        e ds                  extname(azute00f)
014900120622     d                                     dtaara
015000120622     d §datiute      e ds                  extname(ddatiute)
015100120622     d                                     dtaara
015200120622
015300120622     d psds           sds
015400120622     d  pgmname          *proc
015500120622
015600120622      // ? PROTOTIPI ?
015700120622      /copy gaitrasrc/srcprotopr,tibs34r
015701120622      /copy gaitrasrc/srcprotopr,tibs69r
015800120622
015900120622      *------------------------------------------------------------------------*
016000120622      * costanti
016100120622      *------------------------------------------------------------------------*
016200120622     d errore          c                   '1'
016300120622     d eseguito        c                   '0'
016400120622
016500120622      *------------------------------------------------------------------------*
016600120622
016700120622      /free
016900120622
017000120622       exsr sr_c01;
017100120622       exsr sr_uscita;
017200120622
017300120622       // ------------------------------------------------------------------------
017400120622       // gestione subfile
017500120622       // ------------------------------------------------------------------------
017600120622       begsr sr_c01;
017700120622
017800120622       // imposto variabili
017900120622        wrkcars01 = *on;
018000120622        $video = 'C01';
018100120622
018200120622       // inizio elaborazione subfile
018300120622         dou  $video <> 'C01';
018400120622
018500120622       // caricamento subfile
018600120622          exsr sr_cars01;
018700120622
018800120622       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
018900120622       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
019000120622       // l'ultimo tasto
019100120622          if c1csrrrn > 0;
019200120622           c1rcdnbr = c1csrrrn;
019300120622          else;
019400120622           c1rcdnbr = savrcdnbr;
019500120622          endif;
019600120622
019700120622       // se non so quale pagina visualizzare forzo pagina 1
019800120622          if c1rcdnbr < 1;
019900120622           eval c1rcdnbr = 1;
020000120622          endif;
020100120622
020200120622       // salvo il record number del subfile
020300120622          savrcdnbr = c1Rcdnbr;
020400120622
020500120622       // Emissione del subfile
020600120622          write tbflp01;
020700120622          exfmt tbflc01;
020800120622
020900120622       // controllo tasti funzionali del subfile
021000120622          select;
021100120622
021200120622       // F3=Fine
021300120622           when *inkc;
021400120622            $video = *blanks;
021500120622            ricez='C';
021600120622            exsr sr_uscita;
021700120622
021800120622       // F5=Refresh
021900120622           when *inke;
022000120622            wrkcars01 = *on;
022100120622
022200120622       // F10=Immissione
022300120622           when *inkj;
022400120622            $video = 'D01';
022500120622            comand='J';
022501120704            opzio='  ';
022600120706            exsr sr_c02;
022700120622
022800120622       // in tutti gli altri casi
022900120622           other;
023000120622       // controllo ed elaborazione scelte su subfile
023100120622            exsr sr_gestsfl;
023200120622          endsl;
023300120622
023400120622       // fine elaborazione 'C01'
023500120622         enddo;
023600120622
023700120622        endsr;
023800120622
023900120622       // ------------------------------------------------------------------------
024000120622       // caricamento subfile
024100120622       // ------------------------------------------------------------------------
024200120622       begsr sr_cars01;
024300120622
024400120622       // se è stato richiesto il caricamento del subfile
024500120622        if wrkcars01 = *on;
024600120622
024700120622       // inizializzo il subfile
024800120622         s01nrr = 0;
024900120622         *in20 = *on;
025000120622         write tbflc01;
025100120622         *in20 = *off;
025200120622         *in21 = *off;
025300120622         *in22 = *off;
025400120622         *in23 = *off;
025500120622       // imposto la chiave di posizionamento e lettura file
025600120622         tbecod = 'FLB';
025700120622       // posizionamento file
025800120622         setll tbecod tntbe01l;
025900120622       // fino a che non è fine file
026000120622         dou %eof(tntbe01l);
026100120622
026200120622       // leggo file
026300120622          reade(n) tbecod tntbe01l;
026400120622
026500120622       // fine file esco
026600120622          if %eof(tntbe01l);
026700120622           leave;
026800120622          endif;
026900120622
027000120622       // scrivo subfile
027100120622           clear s1opzione;
027200120622           exsr sr_wtrs01;
027300120622
027400120622        enddo;
027500120622
027600120622         clear opzio;
027700120622         clear comand;
027800120622
027900120622       // fine caricamento subfile
028000120622        endif;
028100120622
028200120622       // se scritto almeno un record
028300120622        if s01nrr > 0;
028400120622       // indicatore di sflend
028500120622         eval *in21 = *on;
028600120622        endif;
028700120622
028800120622       // se non scritto neanche un record
028900120622        if s01nrr = 0;
029000120622       // indicatore di sfldsp
029100120622         eval *in23 = *on;
029200120622        endif;
029300120622
029400120622       // disattivo opzione di caricamento subfile
029500120622        wrkcars01 = *off;
029600120622
029700120622       endsr;
054000120622       // ------------------------------------------------------------------------
054100120622       // gestione video dati di trasmissione
054200120622       // ------------------------------------------------------------------------
054300120622       begsr sr_w01;
054400120622
054500120622       // imposto i dati a video
054600120622        exsr sr_carw01;
054700120622
054800120622       // fino a che la variabile resta settata come 'W01'
054900120622        dou $video <> 'W01';
055000120622
055100120622       // reset indicatore generico di errore
055200120622         *in28 = *off;
055300120622
055400120622       // emetto il video
055500120622          exfmt tbflw01;
055600120622
055700120622       // controllo tasti funzionali del video
055800120622         select;
055900120622
056000120622       // F6=Conferma
056100120622          when *inkf;
056200120622       // controllo i dati del video
056300120622           exsr sr_ctrw01;
056400120622       // se non riscontrati errori aggiorno il record corrente
056500120622           if not *in28;
056600120622            exsr sr_aggiorna;
056700120622            if not *in28;
056800120622             if comand = 'J';
056900120706              $video = 'C02';
057000120706              exsr sr_setc02;
057001120706              exsr sr_cars02;
057100120622             else;
057200120622              $video = 'C01';
057300120622             endif;
057400120622            endif;
057500120622           endif;
057600120622
057700120622       // F12=Ritorno
057800120622          when *inkl;
057900120706           $video = 'C02';
058000120622           clear ricez;
058100120622
058200120622       // Invio
058300120622          other;
058400120622           exsr sr_ctrw01;
058500120622
058600120622         endsl;
058700120622
058800120622       // fine gestione 'W01'
058900120622        enddo;
059000120622
059100120622       endsr;
059200120622
059300120622       // ------------------------------------------------------------------------
059400120622       // imposto i dati di trasmissione
059500120622       // ------------------------------------------------------------------------
059600120622       begsr sr_carw01;
059700120622
059800120622       // se pulisco i campi
059900120622         clear w1ftt;
060000120622         clear w1flt;
060100120622         clear w1ftr;
060200120622         clear w1dtr;
060300120622
060400120622       // se non immissione imposto i campi del file
060500120622        if comand <> 'J';
060600120622         w1ftt = tbeftt;
060700120622         w1flt = tbeflt;
060800120622         w1ftr = tbeftr;
060900120622       // imposto la data
061000120622         if tbedtr <> *zeros;
061100120622          data_eur=%date(tbedtr:*iso);
061200120622          w1dtr=%dec(data_eur);
061300120622         endif;
061400120622
061500120622        endif;
061600120622
061700120622       endsr;
061800120622
061900120622       // ------------------------------------------------------------------------
062000120622       // controllo i dati di trasmissione
062100120622       // ------------------------------------------------------------------------
062200120622       begsr sr_ctrw01;
062300120622
062400120622       endsr;
062500120622
062600120622       // ------------------------------------------------------------------------
062700120622       // aggiorno tabella
062800120622       // ------------------------------------------------------------------------
062900120622       begsr sr_aggiorna;
063000120622
063001120706       // imposto campi del file
063002120706
063600120622        tbeftt = w1ftt;
063700120622        tbeflt = w1flt;
063800120622        clear tbeftr;
063900120622        clear tbedtr;
064100120622        tbeuni=campocm;
064200120622
064300120622       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
064400120622        select;
064500120622
064600120622       // F10=immissione -
064700120622         when comand = 'J';
064701120706          tbeke1 = %editc(d01lna:'X');
064702120706          tbeke2 = %editc(d01lfz:'X');
064800120622          clear tbelin;
064900120622          clear tbeatb;
065000120622          write tntbe000;
065100120622
065200120622       // Opzione "04"=cancellazione/ripristino
065300120622       // Opzione "02"=manutenzione
065400120622         when opzio = '04' or opzio='02';
065500120622          update tntbe000;
065600120622
065700120622        endsl;
065800120622
065900120622       endsr;
070500120622       // ------------------------------------------------------------------------
070600120622       // gestione del subfile
070700120622       // ------------------------------------------------------------------------
070800120622       begsr sr_gestsfl;
070900120622
071000120622       // set flag
071100120622        wrkeofs01 = *off;
071200120622
071300120622       // inizio lettura subfile
071400120622        dow wrkeofs01 = *off and *in21;
071500120622         readc tbfls01;
071600120622       // se fine subfile
071700120622         if %eof;
071800120622          wrkcars01 = *on;
071900120622          leave;
072000120622         endif;
072100120622       // se è stata immessa un'opzione
072200120622         if s1opzione <> *zeros;
072300120622       // controllo ed elaborazione opzione immessa
072400120622          select;
072500120622       // opzione "04"=annullo/ripristino
072600120622           when s1opzione = 4;
072700120622            opzio = '04';
072800120622       // opzione "02"=manutenzione
072900120622           when s1opzione = 2;
073000120622            opzio = '02';
073001120704            if s1tbeatb='Ann';
073002120704               clear opzio;
073003120704               clear s1opzione;
073005120704               update tbfls01;
073006120704            endif;
073100120622       // opzione "05"=Visualizzazione
073200120622           when s1opzione = 5;
073300120622            opzio = '05';
073400120622          endsl;
073500120622
073600120622       // se immessa almeno un opzione valida
073700120622          if  opzio <> *blanks;
073800120622       // gstione del formato video
073900120706           exsr sr_c02;
074000120622       // se la gestione si è chiusa con ...
074100120622           select;
074200120622       // F6=Conferma
074300120622            when ricez = 'F';
074400120622             clear s1opzione;
074500120622             wrkcars01 = *on;
074600120622       // F12=Ritorno
074700120622            when ricez = 'L';
074800120622             clear s1opzione;
074900120622             wrkeofs01 = *on;
075000120622       // altrimenti
075100120622            other;
075200120622             *in22 = *on;
075300120622           endsl;
075400120622
075500120622           update tbfls01;
075600120622           *in22 = *off;
075700120622       // fine opzione
075800120622          endif;
075900120622       // fine opzione
076000120622         endif;
076100120622
076200120622        enddo;
076300120622
076400120622       endsr;
076500120622
076600120622
076700120622       // ------------------------------------------------------------------------
076800120622       // scrivo subfile
076900120622       // ------------------------------------------------------------------------
077000120622       begsr sr_wtrs01;
077100120622
077200120622       // se non raggiunto limite massimo di caricamento
077300120622        if s01nrr < 9999;
077400120622       // imposto campi di subfile
077500120622         exsr sr_sets01;
077501120704         if s1tbeke1<>*blanks;
077600120622       // imposto numeratore righe e numero relativo di record
077700120622         s01nrr = s01nrr + 1;
077800120622       // scrivo subfile
077900120622         write tbfls01;
078000120622        endif;
078001120704        endif;
078100120622
078200120622       endsr;
078300120622
078400120622       // ------------------------------------------------------------------------
078500120622       // imposto campi del subfile
078600120622       // ------------------------------------------------------------------------
078700120622       begsr sr_sets01;
078800120622
078900120622        s1tbeke1 = tbeke1;
079000120622        s1tbeke2 = tbeke2;
079100120622        clear s1dlna;
079200120622        clear s1dlfz;
079300120622        // Decodifica delle due linee di arrivo
079301120704      /end-free
079302120704     C                   testn                   s1tbeke1             30
079303120704    2c                   if             *in30  = *off
079304120706     c                             or  %subst(s1tbeke1: 3: 1) < *zero
079305120704     c                   clear                   s1tbeke1
079306120704     c                   leavesr
079307120704     c                   endif
079308120704      /free
079400120622        kfil=%dec(s1tbeke1:3:0);
079500120622        chain (kfil) azorg01l;
079600120622        s1dlna=orgdes;
079700120622        kfil=%dec(s1tbeke2:3:0);
079800120622        chain (kfil) azorg01l;
079900120622        s1dlfz=orgdes;
080001120704        if tbeatb='A';
080002120704           s1tbeatb = 'Ann';
080003120704        else;
080004120704           clear s1tbeatb;
080005120704        endif;
080006120706        campocms01=tbeuni;
080007120706        if %subst(tbeuni:22:7)<>*blanks and %subst(tbeuni:22:7)<>*zeros;
080008120706           s01piu='+';
080009120706        else;
080010120706           s01piu=*blanks;
080011120706        endif;
080012120706        //decodifico i clienti caricati a video
080013120706                clear tibs69ds;
080014120706                clear DS_cnaco;
080015120706                clear DS_cnind;
080016120706                clear DS_cnclp;
080017120706                clear DS_fncls;
080018120706                clear s01dm1;
080019120706                clear s01dm2;
080020120706                clear s01dm3;
080021120706           for I=1 to 3;
080023120706             select;
080024120706             when I=1;
080025120706                if s01cm1=*blanks or s01cm1=*zeros;
080026120706                  leave;
080027120706                endif;
080028120706                I69kac=%dec(s01cm1:7:0);
080029120706             when i=2;
080030120706                if s01cm2=*blanks or s01cm2=*zeros;
080031120706                  leave;
080032120706                endif;
080033120706                I69kac=%dec(s01cm2:7:0);
080034120706             when i=3;
080035120706                if s01cm3=*blanks or s01cm3=*zeros;
080036120706                  leave;
080037120706                endif;
080038120706                I69kac=%dec(s01cm3:7:0);
080039120706             endsl;
080040120706                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
080041120706                if o69err=*blanks;
080042120706             select;
080043120706             when I=1;
080044120706                   s01dm1=acorag;
080045120706             when i=2;
080046120706                   s01dm2=acorag;
080047120706             when i=3;
080048120706                   s01dm3=acorag;
080049120706             endsl;
080050120706                endif;
080051120706           endfor;
080100120622
080200120622       endsr;
080201120706       // ------------------------------------------------------------------------
080202120706       // gestione subfile 2 - dettaglio per lna/lna forzata
080203120706       // ------------------------------------------------------------------------
080204120706       begsr sr_c02;
080205120706       // imposto il video a seconda della funzione richiesta
080206120706        exsr sr_setc02;
080207120706        $video = 'C02';
080208120706       // caricamento subfile
080209120706        exsr sr_cars02;
080210120706       // inizio elaborazione subfile
080211120706         dou  $video <> 'C02';
080212120706       // c2csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
080213120706       // impostando c2rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
080214120706       // l'ultimo tasto
080215120706          if c2csrrrn > 0;
080216120706           c2rcdnbr = c2csrrrn;
080217120706          else;
080218120706           c2rcdnbr = sv2rcdnbr;
080219120706          endif;
080220120706       // se non so quale pagina visualizzare forzo pagina 1
080221120706          if c2rcdnbr < 1;
080222120706           eval c2rcdnbr = 1;
080223120706          endif;
080224120706
080225120706       // salvo il record number del subfile
080226120706          sv2rcdnbr = c2Rcdnbr;
080227120706
080228120706       // Emissione del subfile
080229120706          write tbflp02;
080230120706          exfmt tbflc02;
080231120706
080232120706       // controllo tasti funzionali del subfile
080233120706          select;
080234120706       // F3=Fine
080235120706           when *inkc;
080236120706            $video = *blanks;
080237120706            ricez='C';
080238120706            exsr sr_uscita;
080239120706       // F12=Ritorno
080240120706           when *inkl;
080241120706            ricez='L';
080242120706            unlock tntbe01l;
080243120706            $video = 'C01';
080244120706            // se non è f12 da immissione non ricarico il subfile
080245120706            if comand = 'J';
080246120706               wrkcars01 = *on;
080247120706            else;
080248120706               wrkcars01 = *off;
080249120706            endif;
080250120706       // F6=Conferma
080251120706           when *inkf;
080252120706            ricez='F';
080253120706            // controllo e decodifico i dati del video
080254120706            exsr sr_ctrs02;
080255120706            if *in28;
080256120706               *in43=*off;
080257120706            else;
080258120706               *in43=*on;
080259120706            endif;
080260120706            // conferma per annullo/ripristino
080261120706            if opzio = '04';
080262120706             select;
080263120706             // annullamento
080264120706             when tbeatb = *blanks;
080265120706              tbeatb = 'A';
080266120706             // ripristino
080267120706             when tbeatb = 'A';
080268120706              clear tbeatb;
080269120706             endsl;
080270120706            endif;
080271120706
080272120706       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
080273120706            if not *in28;
080274120706             wrkcarw01 = *on;
080275120706             $video = 'W01';
080276120706             exsr sr_w01;
080277120706            endif;
080286120706       // Invio
080287120706           other;
080288120706            if not *in03;
080289120706             exsr sr_ctrs02;
080290120706            if *in28;
080291120706               *in43=*off;
080292120706            else;
080293120706               *in43=*on;
080294120706            endif;
080295120706            endif;
080296120706          endsl;
080297120706       // fine elaborazione 'C02'
080298120706         enddo;
080299120706       endsr;
080300120706       // ------------------------------------------------------------------------
080301120706       // imposto control C02
080302120706       // ------------------------------------------------------------------------
080303120706       begsr sr_setc02;
080304120706
080305120706        // pulizia campi control
080306120706        clear d1descopz;
080307120706        clear d01lna;
080308120706        clear d01lfz;
080309120706        clear d01lnad;
080310120706        clear d01lfzd;
080311120706
080312120706        *in06 = *off;
080313120706        *in28 = *off;
080314120706        *in40 = *off;
080315120706        *in41 = *off;
080316120706        *in42 = *off;
080317120706        *in43 = *on;
080318120706        *in02 = *off;
080319120706        *in03 = *off;
080320120706        *in04 = *off;
080321120706
080322120706       // controllo se 'immissione' o altro
080323120706        select;
080324120706
080325120706       // F10=Immissione
080326120706         when comand = 'J';
080327120706          d1descopz = 'Immissione';
080328120706
080329120706       // Opzione "04"=cancellazione/ripristino
080330120706         when opzio = '04';
080331120706           *in03 = *on;
080332120706       // Opzione "05"=visualizzazione
080333120706         when opzio = '05';
080334120706           *in03 = *on;
080335120706           *in02 = *on;
080336120706           d1descopz = 'Visualizzazione';
080337120706       // Opzione "02"=manutenzione
080338120706         when opzio = '02' ;
080339120706           *in04 = *on;
080340120706           d1descopz = 'Manutenzione';
080341120706        endsl;
080342120706
080343120706       // centro la descrizione della funzione nel formato video
080344120706        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
080345120706        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
080346120706
080347120706       if comand<>'J';
080348120706        d01lna=%dec(s1tbeke1:3:0);
080349120706        d01lfz=%dec(s1tbeke2:3:0);
080350120706        kfil=%dec(d01lna:3:0);
080351120706        chain (kfil) azorg01l;
080352120706        d01lnad=orgdes;
080353120706        kfil=%dec(d01lfz:3:0);
080354120706        chain (kfil) azorg01l;
080355120706        d01lfzd=orgdes;
080356120706       endif;
080357120706
080358120706       endsr;
080359120706       // ------------------------------------------------------------------------
080360120706       // caricamento subfile2 - dettaglio per lna/lna forzata
080361120706       // ------------------------------------------------------------------------
080362120706       begsr sr_cars02;
080363120706
080364120706       // inizializzo il subfile
080365120706         s02nrr = 0;
080366120706         *in24 = *on;
080367120706         write tbflc02;
080368120706         *in24 = *off;
080369120706         *in25 = *off;
080370120706         *in26 = *off;
080371120706         *in27 = *off;
080372120706         clear campocm;
080373120706       if comand<>'J';
080374120706       // recupero i dati dal file
080375120706        chain ('FLB':s1tbeke1:s1tbeke2) tntbe01l;
080376120706        if %found(tntbe01l);
080377120706           campocm=tbeuni;
080378120706           if tbeatb<>' ';
080379120706              *in06=*on    ;
080380120706           endif;
080381120706          if opzio='04';
080382120706          // se richiesta 'cancellazione'
080383120706            if tbeatb = *blanks;
080384120706             d1descopz = 'Annullamento';
080385120706            endif;
080386120706          // se richiesto 'ripristino'
080387120706            if tbeatb = 'A';
080388120706             d1descopz = 'Ripristino';
080389120706            endif;
080390120706          endif;
080391120706        endif;
080392120706       endif;
080393120706           for I=1 to 36;
080394120706              clear s02cm;
080395120706              clear s02dcm;
080396120706              *in26=*off;
080397120706              if scm(I)<>*blanks;
080398120706                s02cm=Scm(i);
080399120706                // decodifico il cliente mittente
080400120706                clear tibs69ds;
080401120706                clear DS_cnaco;
080402120706                clear DS_cnind;
080403120706                clear DS_cnclp;
080404120706                clear DS_fncls;
080405120706                I69kac=%dec(scm(i):7:0);
080406120706                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
080407120706                if o69err=*blanks;
080411120706                   s02dcm=acorag;
080415120706                endif;
080416120706                *in26=*on;
080417120706              endif;
080418120706              // imposto numeratore righe e numero relativo di record
080419120706              s02nrr = s02nrr + 1;
080420120706              // scrivo subfile
080421120706              write tbfls02;
080422120706           endfor;
080423120706
080424120706       // se scritto almeno un record
080425120706        if s02nrr > 0;
080426120706           C2RCDNBR=1;
080427120706           c2csrrrn=1;
080428120706       // indicatore di sflend
080429120706         eval *in25 = *on;
080430120706        endif;
080431120706
080432120706       // se non scritto neanche un record
080433120706        if s02nrr = 0;
080434120706       // indicatore di sfldsp
080435120706         eval *in27 = *on;
080436120706        endif;
080437120706
080438120706       endsr;
080439120706       // ------------------------------------------------------------------------
080440120706       // controllo subfile 2 (dettaglio per linea di partenza)
080441120706       // ------------------------------------------------------------------------
080442120706       begsr sr_ctrs02;
080443120706
080444120706        *in28 = *off;
080445120706        *in40 = *off;
080446120706        *in41 = *off;
080447120706        *in42 = *off;
080448120706        clear vc2msg;
080449120706
080450120706       // Linea di arrivo obbligatoria
080451120706        if d01lna=*zeros;
080452120706         vc2msg = msg(01);
080453120706         *in28 = *on;
080454120706         *in40 = *on;
080455120706         leavesr;
080456120706        endif;
080457120706
080458120706       // errore se non è una lna
080459120706        chain d01lna azorg01l;
080460120706        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
080461120706           vc2msg = msg(01);
080462120706           *in28 = *on;
080463120706           *in40 = *on;
080464120706           leavesr;
080465120706        endif;
080466120706        d01lnad=orgdes;
080467120706
080468120706       // Linea di arrivo forzata obbligatoria
080469120706        if d01lfz=*zeros;
080470120706         vc2msg = msg(01);
080471120706         *in28 = *on;
080472120706         *in41 = *on;
080473120706         leavesr;
080474120706        endif;
080475120706
080476120706       // errore se non è una lna
080477120706        chain d01lfz azorg01l;
080478120706        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
080479120706           vc2msg = msg(01);
080480120706           *in28 = *on;
080481120706           *in41 = *on;
080482120706           leavesr;
080483120706        endif;
080484120706        d01lfzd=orgdes;
080485120706       // In immissione la chiave non deve essere già presente
080486120706        if comand = 'J';
080487120706         tbecod = 'FLB';
080488120706         tbeke1 = %editc(d01lna:'X');
080489120706         tbeke2 = %editc(d01lfz:'X');
080490120706         chain(n) (tbecod:tbeke1:tbeke2) tntbe01l;
080491120706         if %found(tntbe01l);
080492120706            vc2msg = msg(05);
080493120706            *in28 = *on;
080494120706            *in40 = *on;
080495120706            leavesr;
080496120706         endif;
080497120706        endif;
080498120706
080499120706        clear scm;
080500120706        *in26=*on;
080501120706           readc tbfls02;
080502120706            dow not(%eof);
080503120706              clear s02dcm;
080504120706              if s02cm<>*zeros and s02cm<>*blanks;
080505120706      /end-free
080506120706     C                   testn                   s02cm                30
080507120706    2c                   if             *in30  = *off
080508120706     c                             or  %subst(s02cm: 7: 1) < *zero
080509120706     c                   eval      vc2msg = msg(02)
080510120706     c                   eval      *in28 = *on
080511120706     c                   eval      *in42 = *on
080512120706     c                   update    tbfls02
080513120706     c                   eval      c2csrrrn=s02nrr
080514120706     c                   leavesr
080515120706    2c                   endif
080516120706      /free
080517120706           clear tibs69ds;
080518120706           clear DS_cnaco;
080519120706           clear DS_cnind;
080520120706           clear DS_cnclp;
080521120706           clear DS_fncls;
080522120706           I69kac=%dec(s02cm:7:0);
080523120706           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
080524120706              if o69err<>*blanks;
080525120706                 vc2msg = msg(02);
080526120706                 *in28 = *on;
080527120706                 *in42 = *on;
080528120706                 update tbfls02;
080529120706                 c2csrrrn=s02nrr;
080530120706                 leavesr;
080531120706              endif;
080532120706                // decodifico il codice cliente
080533120706                s02dcm=acorag;
080536120706
080537120706              endif;
080538120706              if s02cm=*zeros;
080539120706                 clear s02cm;
080540120706                 clear s02dcm;
080541120706              endif;
080542120706              if s02cm<>*blanks;
080543120706                 // errore se cliente ripetuto all'interno del sfl
080544120706                 if %lookup(s02cm:scm)>0;
080545120706                    vc2msg = msg(03);
080546120706                    *in28 = *on;
080547120706                    *in42 = *on;
080548120706                    update tbfls02;
080549120706                    c2csrrrn=s02nrr;
080550120706                    leavesr;
080551120706                 else;
080552120706                 // se codice non ripetuto memorizzo in schiera
080553120706                    i=%lookup('       ':scm);
080554120706                    if i>0;
080555120706                       scm(i)=s02cm;
080556120706                    endif;
080557120706                 endif;
080558120706              endif;
080559120706                 *in26 = *on;
080560120706                 update tbfls02;
080561120706               readc tbfls02;
080562120706            enddo;
080563120706       endsr;
080564120706
080565120622
080566120622       // ------------------------------------------------------------------------
080567120622       // routine iniziale
080600120622       // ------------------------------------------------------------------------
080700120622         begsr *inzsr;
080800120622
080900120622      /end-free
081000120622
081100120622     c     *entry        plist
081200120622     c                   parm                    kpjba
081300120622
081400120622      /free
081500120622         in(e) §azute;
081600120622         if not %error;
081700120622          in(e) §datiute;
081800120622         endif;
081900120622         if %error or rsut = *blanks;
082000120622          tibs34r(tibs34ds);
082100120622          in §azute;
082200120622          in §datiute;
082300120622         endif;
082400120622
082500120622         endsr;
082600120622
082700120622       // ------------------------------------------------------------------------
082800120622       // uscita dal programma
082900120622       // ------------------------------------------------------------------------
083000120622         begsr sr_uscita;
083100120622
083200120622          *inlr = *on;
083300120622          return;
083400120622
083500120622         endsr;
083600120622
083700120622      /end-free
083800120622
083900120622** -MSG-                                                                     *
084000120622Linea di arrivo mancante o errata                                              01
084100120706Codice cliente mancante o errato                                               02
084200120706Codcie cliente indicato più volte                                              03
084300120622
084400120622Chiave già presente in tabella                                                 05
