000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300170922      * Gestione tabella "DP5" = Gestione attivazione nuova etichetta DP5      *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000501090424     fazorg01l  if   e           k disk
000800170922     ftntbdp5d  cf   e             workstn sfile(tbdps01:s01nrr)
000801170922     f                                     sfile(tbdps02:s02nrr)
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
001301120621      * 02 - visualizzazione
001600060508      * 03 - proteggo campi video 01
001700120621      * 04 - manutenzione
001701120704      * 06 - dicitura "Annullamento" a video
001900060508      * 20 - gestione subfile 01
002000060508      * 21 - gestione subfile 01
002100060508      * 22 - gestione subfile 01
002200060508      * 23 - gestione subfile 01
002201120704      * 24 - gestione subfile 02
002202120704      * 25 - gestione subfile 02
002203120704      * 26 - gestione subfile 02
002204120704      * 27 - gestione subfile 02
002500060428      * 28 - messaggio errore
002700120704      * 40/42Errori campi subfile/control
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
003702170922     d data_iso        s               d   datfmt(*iso)
003703171006     d*d01dde_i        s                   like(d01dde)
003705170922     d wdata           s              8  0
003800060428     d blanks          s                   like(d1descopz)
003900090427     d comand          s              1
003901090427     d opzio           s              2
003902090427     d ricez           s              1
004100060428     d len             s              5u 0
004200060505     d s01nrr          s              4  0
004201120704     d s02nrr          s              4  0
004400060428     d savopzione      s                   like(s1opzione)
004500060428     d savrcdnbr       s                   like(c1rcdnbr)
004501120704     d sv2rcdnbr       s                   like(c2rcdnbr)
004700060531     d savtbeftt       s                   like(tbeftt)
004800060531     d savtbeflt       s                   like(tbeflt)
004900060505     d savtbeuni       s                   like(tbeuni)
004901120620     d kfil            s                   like(orgfil)
005200060428     d wrkeofs01       s              1
005300060428     d wrkcars01       s              1
005400060505     d wrkcarw01       s              1
005600060504     d $video          s             10
005601120620     d i               s              3  0 inz
005602120620     d w0030           s              3  0 inz
005603120706     d ix              s              3  0 inz
005605120705
005606120705       // - Parametri per TNSD19R
005607120705     d SD19cod         s              3    inz
005608120705     d SD19tip         s              1    inz
005609120705     d SD19des         s             25    inz
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006101120620     d campolp         ds           255
006188120620     D  Slp                    1    255    dim(85)
006190120704     d campolps01      ds            42
006191120704     d  S01LP1
006192120704     d  S01LP2
006193120704     d  S01LP3
006194120704     d  S01LP4
006195120704     d  S01LP5
006196120704     d  S01LP6
006197120704     d  S01LP7
006198120704     d  S01LP8
006199120704     d  S01LP9
006200120704     d  S01L10
006201120704     d  S01L11
006202120704     d  S01L12
006203120704     d  S01L13
006204120704     d  S01L14
006224120620     d
006900060502     d kpjba         e ds
007000060503     d tibs34ds      e ds                  inz
007200060428     d §azute        e ds                  extname(azute00f)
007300060428     d                                     dtaara
007400060428     d §datiute      e ds                  extname(ddatiute)
007500060428     d                                     dtaara
007501120706     d trul06ds      e ds
007502120706     D  LIN                    1     90    dim(30)
007503170922     d og143         e ds
007600010503
007700060428     d psds           sds
007800060428     d  pgmname          *proc
007900060428
008701090424      // ? PROTOTIPI ?
008702090424      /copy gaitrasrc/srcprotopr,tibs34r
008703120706      /copy gaitrasrc/srcprotopr,trul06r
008704120705       // - Ricerca codici Organigramma
008705120705     d tnsd19r         pr                  extpgm('TNSD19R')
008706120705     d  D19cod                             like(SD19cod)
008707120705     d  D19tip                             like(SD19tip)
008708120705     d  D19des                             like(SD19des)
008800060428
008900060428      *------------------------------------------------------------------------*
009000060428      * costanti
009100060428      *------------------------------------------------------------------------*
009200060428     d errore          c                   '1'
009300060428     d eseguito        c                   '0'
009400010503
009500060428      *------------------------------------------------------------------------*
009803120621
009804120704      /free
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
019300170922          write tbdpp01;
019400170922          exfmt tbdpc01;
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
021100120704            $video = 'C02';
021101090427            comand='J';
021102120703            opzio='  ';
021500120704            exsr sr_c02;
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
024300170922         write tbdpc01;
024400060428         *in20 = *off;
024500060428         *in21 = *off;
024600060428         *in22 = *off;
024700060428         *in23 = *off;
024701120620       // imposto la chiave di posizionamento e lettura file
024702170922         tbecod = 'DP5';
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
057300170922          exfmt tbdpw01;
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
059200090424             if comand = 'J';
059300120704              $video = 'C02';
059402120705              exsr sr_setc02;
059405120705              exsr sr_cars02;
059500060504             else;
059600060504              $video = 'C01';
059700060504             endif;
059900060504           endif;
060000060504
060100060504       // F12=Ritorno
060200060504          when *inkl;
060300120704           $video = 'C02';
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
066700060502
066800060504        tbeftt = w1ftt;
066900060504        tbeflt = w1flt;
067000060502        clear tbeftr;
067100060502        clear tbedtr;
067104120621        tbeuni=campolp;
067200060502
067300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
067400060502        select;
067500060502
067600090427       // F10=immissione -
067700090427         when comand = 'J';
067701120705          tbeke1 = %editc(d01lna:'X');
067702171006         // tbeke2 = %editc(d01dde_i:'X');
067703171006          clear tbeke2;
067704090427          clear tbelin;
067705090428          clear tbeatb;
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
077100170922         readc tbdps01;
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
079408170922               update tbdps01;
079409120704            endif;
079410120621       // opzione "05"=Visualizzazione
079411120621           when s1opzione = 5;
079412120621            opzio = '05';
079800060502          endsl;
079900010503
080000060502       // se immessa almeno un opzione valida
080100090424          if  opzio <> *blanks;
080200060502       // gstione del formato video
080300120704           exsr sr_c02;
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
081900170922           update tbdps01;
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
085405170922         write tbdps01;
085406120703         endif;
085900060502        endif;
086000060502
086100060502       endsr;
086200060502
086300060502       // ------------------------------------------------------------------------
086400120704       // imposto campi del subfile 1
086500060502       // ------------------------------------------------------------------------
086600060502       begsr sr_sets01;
086700060502
086800060502        s1tbeke1 = tbeke1;
086801171006        //wdata = %dec(tbeke2:8:0);
086802171006        //data_eur = %date(wdata);
086803171006        //data_iso = %date(wdata);
086804171006        //s1tbeke2 = %char(data_eur);
086805171006        //shtbeke2 = tbeke2;
086808120615        clear s1dlna;
086810170922        // Decodifica della linea di arrivo
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
086825120703        if tbeatb='A';
087200120703           s1tbeatb = 'Ann';
087201120703        else;
087202120703           clear s1tbeatb;
087203120703        endif;
087204120704        campolps01=tbeuni;
087205120704        if %subst(tbeuni:43:3)<>'   ';
087206120704           s01piu='+';
087207120704        else;
087208120704           s01piu=*blanks;
087209120704        endif;
087300060502
087400060502       endsr;
087444120704       // ------------------------------------------------------------------------
087445170922       // gestione subfile 2 - dettaglio per lna /data
087446120704       // ------------------------------------------------------------------------
087447120704       begsr sr_c02;
087448120704       // imposto il video a seconda della funzione richiesta
087449120704        exsr sr_setc02;
087450120704        $video = 'C02';
087451120704       // caricamento subfile
087452120704        exsr sr_cars02;
087453120704       // inizio elaborazione subfile
087454120704         dou  $video <> 'C02';
087455120704       // c2csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
087456120704       // impostando c2rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
087457120704       // l'ultimo tasto
087458120704          if c2csrrrn > 0;
087459120704           c2rcdnbr = c2csrrrn;
087460120704          else;
087461120704           c2rcdnbr = sv2rcdnbr;
087462120704          endif;
087463120704       // se non so quale pagina visualizzare forzo pagina 1
087464120704          if c2rcdnbr < 1;
087465120704           eval c2rcdnbr = 1;
087466120704          endif;
087467120704
087468120704       // salvo il record number del subfile
087469120704          sv2rcdnbr = c2Rcdnbr;
087470120704
087471120704       // Emissione del subfile
087472170922          write tbdpp02;
087473170922          exfmt tbdpc02;
087474120704
087475120704       // controllo tasti funzionali del subfile
087476120704          select;
087477120704       // F3=Fine
087478120704           when *inkc;
087479120704            $video = *blanks;
087480120704            ricez='C';
087481120704            exsr sr_uscita;
087482120704       // F12=Ritorno
087483120704           when *inkl;
087485120704            ricez='L';
087486120704            unlock tntbe01l;
087487120704            $video = 'C01';
087488120705            // se non è f12 da immissione non ricarico il subfile
087489120705            if comand = 'J';
087490120705               wrkcars01 = *on;
087491120705            else;
087492120705               wrkcars01 = *off;
087493120705            endif;
087494120704       // F6=Conferma
087495120704           when *inkf;
087496120704            ricez='F';
087497120704            // controllo e decodifico i dati del video
087498120704            exsr sr_ctrs02;
087499120704            if *in28;
087500120704               *in43=*off;
087501120704            else;
087502120704               *in43=*on;
087503120704            endif;
087504120704            // conferma per annullo/ripristino
087505120704            if opzio = '04';
087506120704             select;
087507120704             // annullamento
087508120704             when tbeatb = *blanks;
087509120704              tbeatb = 'A';
087510120704             // ripristino
087511120704             when tbeatb = 'A';
087512120704              clear tbeatb;
087513120704             endsl;
087514120704            endif;
087519120704
087520120704       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
087521120704            if not *in28;
087522120704             wrkcarw01 = *on;
087523120704             $video = 'W01';
087524120704             exsr sr_w01;
087525120704            endif;
087526120705       // F14=Inserimento per terminal di partenza
087527120705           when *inkn;
087528120706              exsr sr_ctrs02;
087529120706              if not *in28;
087530120706                 exsr sr_w02;
087531120706                 *inkl=*off;
087532120706              endif;
087533120704
087534120704       // Invio
087535120704           other;
087536120704            if not *in03;
087537120704             exsr sr_ctrs02;
087538120705            if *in28;
087539120705               *in43=*off;
087540120705            else;
087541120705               *in43=*on;
087542120705            endif;
087543120704            endif;
087544120704          endsl;
087545120704       // fine elaborazione 'C02'
087546120704         enddo;
087547120704       endsr;
087548120704       // ------------------------------------------------------------------------
087549120704       // imposto control C02
087550120704       // ------------------------------------------------------------------------
087551120704       begsr sr_setc02;
087552120704
087553120704        // pulizia campi control
087554120704        clear d1descopz;
087555120705        clear d01lna;
087556171006        //clear d01dde;
087557120705        clear d01lnad;
087559120704
087560120704        *in06 = *off;
087561120704        *in28 = *off;
087562120704        *in40 = *off;
087563120704        *in41 = *off;
087564120704        *in42 = *off;
087565120704        *in43 = *on;
087566120705        *in02 = *off;
087567120705        *in03 = *off;
087568120705        *in04 = *off;
087569120704
087570120704       // controllo se 'immissione' o altro
087571120704        select;
087572120704
087573120704       // F10=Immissione
087574120704         when comand = 'J';
087575120704          d1descopz = 'Immissione';
087576120704
087577120704       // Opzione "04"=cancellazione/ripristino
087578120704         when opzio = '04';
087579120705           *in03 = *on;
087588120704       // Opzione "05"=visualizzazione
087589120704         when opzio = '05';
087590120705           *in03 = *on;
087591120705           *in02 = *on;
087592120704           d1descopz = 'Visualizzazione';
087593120704       // Opzione "02"=manutenzione
087594120704         when opzio = '02' ;
087595120705           *in04 = *on;
087596120704           d1descopz = 'Manutenzione';
087597120704        endsl;
087598120704
087599120704       // centro la descrizione della funzione nel formato video
087600120704        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
087601120704        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
087602120705
087603120705       if comand<>'J';
087604120705        d01lna=%dec(s1tbeke1:3:0);
087605171006        //DATA_EUR = %DATE(s1tbeke2:*EUR);
087606171006        //D01DDE = %DEC(DATA_EUR);
087609120705        kfil=%dec(d01lna:3:0);
087610120705        chain (kfil) azorg01l;
087611120705        d01lnad=orgdes;
087612120705       endif;
087613120704
087614120704       endsr;
087615120704       // ------------------------------------------------------------------------
087616170922       // caricamento subfile2 - dettaglio per lna/data decorrenza
087617120704       // ------------------------------------------------------------------------
087618120704       begsr sr_cars02;
087619120704
087620120704       // inizializzo il subfile
087621120704         s02nrr = 0;
087622120704         *in24 = *on;
087623170922         write tbdpc02;
087624120704         *in24 = *off;
087625120704         *in25 = *off;
087626120704         *in26 = *off;
087627120704         *in27 = *off;
087628120705         clear campolp;
087629120704       if comand<>'J';
087630120704       // recupero i dati dal file
087631171006       //chain ('DP5':s1tbeke1:shtbeke2) tntbe01l;
087632171006        chain ('DP5':s1tbeke1) tntbe01l;
087633120704        if %found(tntbe01l);
087634120705           campolp=tbeuni;
087635120704           if tbeatb<>' ';
087636120704              *in06=*on    ;
087637120704           endif;
087638120706          if opzio='04';
087639120706          // se richiesta 'cancellazione'
087640120706            if tbeatb = *blanks;
087641120706             d1descopz = 'Annullamento';
087642120706            endif;
087643120706          // se richiesto 'ripristino'
087644120706            if tbeatb = 'A';
087645120706             d1descopz = 'Ripristino';
087646120706            endif;
087647120706          endif;
087648120704        endif;
087649120704       endif;
087650120704           for I=1 to 85;
087651120705              clear s02lp;
087652120705              clear s02dlp;
087653120705              *in26=*off;
087654120705              if slp(I)<>*blanks;
087655120705                s02lp=Slp(i);
087656120704                // decodifico la linea di partenza
087657120704                kfil=%dec(s02lp:3:0);
087658120704                chain (kfil) azorg01l;
087659120705                if %found(azorg01l);
087660120705                   s02dlp=orgde5;
087661120705                   if s02dlp=*blanks;
087662120705                      s02dlp=orgdes;
087663120705                   endif;
087664120705                   endif;
087665120705                *in26=*on;
087666120704              endif;
087667120704              // imposto numeratore righe e numero relativo di record
087668120704              s02nrr = s02nrr + 1;
087669120704              // scrivo subfile
087670170922              write tbdps02;
087671120704           endfor;
087672120704
087673120704       // se scritto almeno un record
087674120704        if s02nrr > 0;
087675120705           C2RCDNBR=1;
087676120705           c2csrrrn=1;
087677120704       // indicatore di sflend
087678120704         eval *in25 = *on;
087679120704        endif;
087680120704
087681120704       // se non scritto neanche un record
087682120704        if s02nrr = 0;
087683120704       // indicatore di sfldsp
087684120704         eval *in27 = *on;
087685120704        endif;
087686120704
087687120704       endsr;
087688120704       // ------------------------------------------------------------------------
087689120704       // controllo subfile 2 (dettaglio per linea di partenza)
087690120704       // ------------------------------------------------------------------------
087691120704       begsr sr_ctrs02;
087692120704
087693120704        *in28 = *off;
087694120704        *in40 = *off;
087695120704        *in41 = *off;
087696120704        *in42 = *off;
087697120704        clear vc2msg;
087698120704
087699170922       // Linea di arrivo dpd obbligatoria
087700120704        if d01lna=*zeros;
087701170922         vc2msg = msg(01);
087702120704         *in28 = *on;
087703120704         *in40 = *on;
087704120704         leavesr;
087705120704        endif;
087706120704
087707170922       // errore se non è una lna dpd
087708120704        chain d01lna azorg01l;
087709120704        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
087710170922           vc2msg = msg(01);
087711120704           *in28 = *on;
087712120704           *in40 = *on;
087713120704           leavesr;
087714170922        else;
087715170922           og143 = orgde3;
087716170922           if §ogntw <> 'DPD'  ;
087717170922              vc2msg = msg(01);
087718170922              *in28 = *on;
087719170922              *in40 = *on;
087720170922              leavesr;
087721170922           endif ;
087722120704        endif;
087723120704        d01lnad=orgdes;
087724120704
087725170922       // data decorrenza obbligatoria
087726171006       // if d01dde=*zeros;
087727171006       //  vc2msg = msg(04);
087728171006       //  *in28 = *on;
087729171006       //  *in41 = *on;
087730171006       //  leavesr;
087731171006       // endif;
087732120704
087733170922       // controllo formale data
087734171006       //if %subst(%editc(d01dde:'X'):1:2)='00';
087735171006       //   test(DE) *dmy d01dde;
087736171006       //   if %error;
087737171006       //      *in41=*on;
087738171006       //      *in28=*on;
087739171006       //      vc2msg=msg(4);
087740171006       //      leavesr;
087741171006       //   else;
087742171006       //      data_iso=%date(d01dde:*dmy);
087743171006       //   endif;
087744171006       //else;
087745171006       //   test(DE) *eur d01dde;
087746171006       //   if %error;
087748171006       //      *in41=*on;
087749171006       //      *in28=*on;
087751171006        //     vc2msg=msg(4);
087752171006        //     leavesr;
087753171006        //  else;
087754171006        //     data_iso=%date(d01dde:*eur);
087755171006        //  endif;
087756171006       //endif;
087757171006        //  data_eur=data_iso;
087758171006        //  d01dde=%dec(data_eur);
087759171006        //  d01dde_i=%dec(data_iso);
087765120704       // In immissione la chiave non deve essere già presente
087766120704        if comand = 'J';
087767170922         tbecod = 'DP5';
087768120704         tbeke1 = %editc(d01lna:'X');
087769171006         //tbeke2 = %editc(d01dde_i:'X');
087770170925         chain(n) (tbecod:tbeke1) tntbe01l;
087771120704         if %found(tntbe01l);
087772170922            vc2msg = msg(05);
087773120704            *in28 = *on;
087774170925            *in40 = *on;
087775120704            leavesr;
087776120704         endif;
087777120704        endif;
087778120704
087779120704        clear slp;
087780120706        *in26=*on;
087781170922           readc tbdps02;
087782120704            dow not(%eof);
087783120705              clear s02dlp;
087784120704              if s02lp<>*zeros and s02lp<>*blanks and s02lp<>'999';
087785120704      /end-free
087786120704     C                   testn                   s02lp                30
087787120704    2c                   if             *in30  = *off
087788120704     c                             or  %subst(s02lp: 3: 1) < *zero
087789120704     c                   eval      vc2msg = msg(02)
087790120704     c                   eval      *in28 = *on
087791120704     c                   eval      *in42 = *on
087792170922     c                   update    tbdps02
087793120705     c                   eval      c2csrrrn=s02nrr
087794120704     c                   leavesr
087795120704    2c                   endif
087796120704      /free
087797120704              w0030=%dec(s02lp:3:0);
087798120704              chain w0030 azorg01l;
087799120704              if not %found (azorg01l) or (orgfag <>'A' and orgfag<>'F');
087800120704                 vc2msg = msg(02);
087801120704                 *in28 = *on;
087802120704                 *in42 = *on;
087803170922                 update tbdps02;
087804120705                 c2csrrrn=s02nrr;
087805120704                 leavesr;
087806120704              endif;
087807120704                // decodifico la linea di partenza
087808120704                s02dlp=orgde5;
087809120704                if s02dlp=*blanks;
087810120704                   s02dlp=orgdes;
087811120704                endif;
087812120704
087813120704              endif;
087814120704              if s02lp=*zeros;
087815120704                 clear s02lp;
087816120704                 clear s02dlp;
087817120704              endif;
087818120705              if s02lp<>*blanks;
087819120705                 // errore se linea di partenza ripetuta all'interno del sfl
087820120705                 if %lookup(s02lp:slp)>0;
087821120705                    vc2msg = msg(03);
087822120705                    *in28 = *on;
087823120705                    *in42 = *on;
087824170922                    update tbdps02;
087825120705                    c2csrrrn=s02nrr;
087826120705                    leavesr;
087827120705                 else;
087828120705                 // se linea di partenza non ripetuta memorizzo in schiera
087829120705                    i=%lookup('   ':slp);
087830120705                    if i>0;
087831120705                       slp(i)=s02lp;
087832120705                    endif;
087833120705                 endif;
087834120705              endif;
087835170922                 update tbdps02;
087836170922               readc tbdps02;
087837120704            enddo;
087838120704       endsr;
087839060505
087840120705       // ------------------------------------------------------------------------
087841120705       // finestra di richiesta terminal di partenza
087842120705       // ------------------------------------------------------------------------
087843120705       begsr sr_w02;
087844120705          clear w02tfp;
087845120705          clear w02tfpd;
087846120706          $video='W02';
087847120706       // fino a che la variabile resta settata come 'W02'
087848120706        dou $video <> 'W02';
087849170922          exfmt TBdpW02;
087850120706          *in29=*off;
087851120705          *in90=*off;
087852120705          select;
087853120705          when *inkl;
087854120705             $video = 'C02';
087855120705          when *inkf;
087856120705             exsr sr_ctrw02;
087857120705             if not *in90;
087858120705                exsr aggc02;
087859120706             $video = 'C02';
087860120705             endif;
087861120705          other;
087862120705             exsr sr_ctrw02;
087863120705          endsl;
087864120705        enddo;
087865120705       endsr;
087866120705       // ------------------------------------------------------------------------
087867120705       //
087868120705       // ------------------------------------------------------------------------
087869120705       begsr sr_ctrw02;
087870120705          clear w02tfpd;
087871120705          if %scan('?':w02tfp)>0;
087872120705             clear  SD19cod;
087873120705             clear  SD19tip;
087874120705             clear  SD19des;
087875120705             tnsd19r(SD19cod : SD19tip : SD19des );
087876120705             w02tfp= SD19cod;
087877120705             w02tfpd= SD19des;
087878120705             *in90=*on;
087879120705             leavesr;
087880120705          endif;
087881120705          // errore se non immesso il terminal di partenza
087882120705          if w02tfp = *blanks or w02tfp=*zeros;
087883120705             *in90=*on;
087884120706             *in29=*on;
087885120705             eval w02msg=msg(6);
087886120705             leavesr;
087887120705          endif;
087888120705      /end-free
087889120705     C                   testn                   w02tfp               30
087890120705    2c                   if             *in30  = *off
087891120705     c                             or  %subst(w02tfp: 3: 1) < *zero
087892120705     c                   eval      *in90=*on
087893120706     c                   eval      *in29=*on
087894120705     c                   eval      w02msg=msg(6)
087895120705     c                   leavesr
087896120705     c                   endif
087897120705      /free
087898120705          // errore se non è un terminal di partenza
087899120705          kfil=%dec(w02tfp:3:0);
087900120705          chain (kfil) azorg01l;
087901120705          if not %found(azorg01l) or (orgfag<>'A' and orgfag<>'F')
087902120706                                  or orgfva<>' ' ;
087903120705             *in90=*on;
087904120706             *in29=*on;
087905120705             eval w02msg=msg(6);
087906120705             leavesr;
087907120705          endif;
087908120706          W02TFPD=orgdes;
087909120705          // verifico se è un terminal di partenza e determino l'elenco delle
087910120706          // filiali del terminal richiamando il trul06r con £1
087911120706          clear trul06ds;
087912120706          d06cod='£1';
087913120706          d06key=w02tfp;
087914120706          d06tla='L';
087915120706          kpjbu=trul06ds;
087916120706          trul06r(kpjba);
087917120706          trul06ds=kpjbu;
087918120706          if d06err<>' ';
087919120706             *in90=*on;
087920120706             *in29=*on;
087921120706             eval w02msg=msg(6);
087922120706             leavesr;
087923120706          endif;
087924120705       endsr;
087925120706       // ------------------------------------------------------------------------
087926120706       // Scrittura delle filiali dalla schiera lin al subfile
087927120706       // ------------------------------------------------------------------------
087928120706       begsr aggc02;
087929120706          s02nrr=1;
087930120706          *in26=*on;
087931120706          Ix=1;
087932170922          chain s02nrr tbdps02;
087933120706          dow lin(Ix) > *zeros and s02nrr<=85;
087934120706           // elemento da caricare già presente nel sfl
087935120706              if %lookup(lin(Ix):slp)>0;
087936120706                 ix+=1;
087937120706                 iter;
087938120706              endif;
087939120706              if s02lp=*blanks or s02lp=*zeros;
087940120706                 s02lp=lin(Ix);
087941120706                 Ix+=1;
087942120706              endif;
087943170922                 update tbdps02;
087944120706                 s02nrr+=1;
087945170922              chain s02nrr tbdps02;
087946120706           enddo;
087947120706           // Se non caricati tutte le linee di partenza per mancanza
087948120706           // di spazio emetto messaggio a video
087949120706           if lin(IX)> *zeros;
087950120706              vc2msg=msg(07);
087951120706              *in28=*on;
087952120706           endif;
087953120706       endsr;
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
108600170922Linea di arrivo Dpd mancante o errata                                          01
108700170922Linea di partenza mancante o errata                                            02
108800120705Linea di partenza già indicata                                                 03
108900170922Data mancante o formalmente errata                                             04
109000120709Chiave già presente in tabella                                                 05
109100120705Terminal di partenza mancante o errato                                         06
109200120706Attenzione! Non inserite tutte le linee per mancanza di spazio                 07
