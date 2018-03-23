000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300140808      * Gestione tabella "FEE" = Festività estere: differenze rispetto a Italia*
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000501090424     fazorg01l  if   e           k disk
000800140808     ftntbfeed  cf   e             workstn sfile(tbfes01:s01nrr)
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
003800060428     d blanks          s                   like(d1descopz)
003900090427     d comand          s              1
003901090427     d opzio           s              2
003902090427     d ricez           s              1
004100060428     d len             s              5u 0
004200060505     d s01nrr          s              4  0
004201120704     d s02nrr          s              4  0
004400060428     d savopzione      s                   like(s1opzione)
004500060428     d savrcdnbr       s                   like(c1rcdnbr)
004700060531     d savtbeftt       s                   like(tbeftt)
004800060531     d savtbeflt       s                   like(tbeflt)
004900060505     d savtbeuni       s                   like(tbeuni)
004901120620     d kfil            s                   like(orgfil)
005200060428     d wrkeofs01       s              1
005300060428     d wrkcars01       s              1
005400060505     d wrkcarw01       s              1
005600060504     d $video          s             10
005601120620     d i               s              3  0 inz
005602140808     d x               s              3  0 inz
005603120620     d w0030           s              3  0 inz
005604120706     d ix              s              3  0 inz
005605141104     d w0040           s              4  0 inz
005606120705
005607120705       // - Parametri per TNSD19R
005608120705     d SD19cod         s              3    inz
005609120705     d SD19tip         s              1    inz
005610120705     d SD19des         s             25    inz
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006101141103     d campomm         ds            40
006102141103     d  smm                    1     40    dim(20)
006103141103     d campogg         ds            40
006104141103     d  sgg                    1     40    dim(20)
006105141103     d campotp         ds            20
006106141103     d  stp                    1     20    dim(20)
006190140903     d campogmts01     ds            49
006192140808     d  S01GM1
006193140808     d  S01GM2
006194140808     d  S01GM3
006195140808     d  S01GM4
006196140808     d  S01GM5
006197140808     d  S01GM6
006198140808     d  S01GM7
006199140903     d  gmt                           7    dim(7) overlay(campogmts01)
006200140903
006201140903     d campogmd02      ds
006202140903     d  d02GMm1
006203140903     d  d02GMm2
006204140903     d  d02GMm3
006205140903     d  d02GMm4
006206140903     d  d02GMm5
006207140903     d  d02GMm6
006208140903     d  d02GMm7
006209140903     d  d02GMm8
006210140903     d  d02GMm9
006211140903     d  d02GMm10
006212140903     d  d02GMm11
006213140903     d  d02GMm12
006214140903     d  d02GMm13
006215140903     d  d02GMm14
006216140903     d  d02GMm15
006217140903     d  d02GMm16
006218140903     d  d02GMm17
006219140903     d  d02GMm18
006220140903     d  d02GMm19
006221140903     d  d02GMm20
006222140918     d  sgmv2                         4    dim(20) overlay(cAMPOGMd02)
006223140903     d campoTpD02      ds
006225140903     d  d02tip1
006226140903     d  d02tip2
006227140903     d  d02tip3
006228140903     d  d02tip4
006229140903     d  d02tip5
006230140903     d  d02tip6
006231140903     d  d02tip7
006232140903     d  d02tip8
006233140903     d  d02tip9
006234140903     d  d02tip10
006235140903     d  d02tip11
006236140903     d  d02tip12
006237140903     d  d02tip13
006238140903     d  d02tip14
006239140903     d  d02tip15
006240140903     d  d02tip16
006241140903     d  d02tip17
006242140903     d  d02tip18
006243140903     d  d02tip19
006244140903     d  d02tip20
006245140903     d  stpv2                         1    dim(20) overlay(CampoTpd02)
006246120620     d
006900060502     d kpjba         e ds
007000060503     d tibs34ds      e ds                  inz
007200060428     d §azute        e ds                  extname(azute00f)
007300060428     d                                     dtaara
007400060428     d §datiute      e ds                  extname(ddatiute)
007500060428     d                                     dtaara
007501120706     d trul06ds      e ds
007502120706     D  LIN                    1     90    dim(30)
007600010503
007601140903     d Dfee          e ds
007602140903
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
019300140808          write tbfep01;
019400140808          exfmt tbfec01;
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
021100140903            $video = 'D02';
021101090427            comand='J';
021102120703            opzio='  ';
021500140903            exsr sr_D02;
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
024300140808         write tbfec01;
024400060428         *in20 = *off;
024500060428         *in21 = *off;
024600060428         *in22 = *off;
024700060428         *in23 = *off;
024701120620       // imposto la chiave di posizionamento e lettura file
024702140808         tbecod = 'FEE';
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
057300140918          exfmt tbfew01;
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
059200141113       //    if comand = 'J';
059300141113       //     $video = 'D02';
059402141113       //     exsr sr_card02;
059500141113       //    else;
059600060504              $video = 'C01';
059601141113              wrkcars01=*on ;
059700141113       //    endif;
059900060504           endif;
060000060504
060100060504       // F12=Ritorno
060200060504          when *inkl;
060300140903           $video = 'D02';
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
062900141113        if comand <> 'J' and opzio<>'03';
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
067101141104        clear smm   ;
067102141104        clear sgg   ;
067103141104        clear stp   ;
067104141103        x=1;
067105141103        for i=1 to 20;
067106141104           if sgmv2(i)>*zeros   ;
067107141103              smm(x)=%subst(sgmv2(i):3:2);
067108141103              sgg(x)=%subst(sgmv2(i):1:2);
067109141103              stp(x)=stpv2(i)         ;
067110141103              x+=1;
067111141103           endif;
067112141103        endfor;
067113141103        §FEEMM=campomm;
067114141103        §FEEGG=campogg;
067115141103        §FEETP=campotp;
067116140903        tbeuni=dfee   ;
067200060502
067300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
067400060502        select;
067500060502
067600090427       // F10=immissione -
067700141113         when comand = 'J' or opzio='03';
067701120705          tbeke1 = %editc(d01lna:'X');
067702140903          clear tbeke2               ;
067703090427          clear tbelin;
067704090428          clear tbeatb;
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
077100140903         readc tbfes01;
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
079401141113       // opzione "03"=Duplica
079402141113           when s1opzione = 3;
079403141113            opzio = '03';
079404120621       // opzione "02"=manutenzione
079405120621           when s1opzione = 2;
079406120621            opzio = '02';
079407120704            if s1tbeatb='Ann';
079408120704               clear opzio;
079409120704               clear s1opzione;
079410140918               update tbfes01;
079411120704            endif;
079412120621       // opzione "05"=Visualizzazione
079413120621           when s1opzione = 5;
079414120621            opzio = '05';
079800060502          endsl;
079900010503
080000060502       // se immessa almeno un opzione valida
080100090424          if  opzio <> *blanks;
080200060502       // gstione del formato video
080300140903           exsr sr_D02;
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
081900140903           update tbfes01;
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
085405140808         write tbfes01;
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
086808120615        clear s1dlna;
086810140808        // Decodifica delle linea estera
086811120703      /end-free
086812120703     C                   testn                   s1tbeke1             30
086813120703    2c                   if             *in30  = *off
086814120703     c                             or  %subst(s1tbeke1: 3: 1) < *zero
086815120703     c                   clear                   s1tbeke1
086816120703     c                   leavesr
086817120703     c                   endif
086818120703      /free
086819120618        kfil=%dec(s1tbeke1:3:0);
086820141113        if kfil=999;
086821141113           s1dlna='STANDARD';
086822141113        else;
086823141113           chain (kfil) azorg01l;
086824141113           s1dlna=orgdes;
086825141113        endif;
086826141113           if tbeatb='A';
087200141113              s1tbeatb = 'Ann';
087201141113           else;
087202141113              clear s1tbeatb;
087203141113           endif;
087205140808        // Valorizzo campo del sfl contenente gg/mm e tipo
087206140903        dfee=tbeuni;
087207141104        campomm=§FEEMM     ;
087208141104        campogg=§FEEGG     ;
087209141104        campotp=§FEETP     ;
087210141113        clear gmt;
087211140808        x=1;
087212140808        for i=1 to 20;
087213141104           if smm(i)>*zeros  and x<=%elem(gmt);
087214140918              %subst(gmt(x):4:2)=smm(i);
087215140918              %subst(gmt(x):1:2)=sgg(i);
087216140918              %subst(gmt(x):7:1)=stp(i);
087217140918              %subst(gmt(x):3:1)='/'   ;
087218140918              %subst(gmt(x):6:1)='-'   ;
087219140808              x+=1;
087220140808           endif;
087221140808        endfor;
087222141104        if smm(8)>*zeros;
087223120704           s01piu='+';
087224120704        else;
087225120704           s01piu=*blanks;
087226120704        endif;
087300060502
087400060502       endsr;
087444120704       // ------------------------------------------------------------------------
087445140903       // gestione video   2 - dettaglio per filiale
087446120704       // ------------------------------------------------------------------------
087447140903       begsr sr_D02;
087448120704       // imposto il video a seconda della funzione richiesta
087449140903        exsr sr_card02;
087450140903        $video = 'D02';
087453120704       // inizio elaborazione subfile
087454140903         dou  $video <> 'D02';
087470120704
087471140903       // Emissione D02
087473140903          exfmt tbfed02;
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
087489141113            if comand = 'J' or opzio='03';
087490120705               wrkcars01 = *on;
087491120705            else;
087492120705               wrkcars01 = *off;
087493120705            endif;
087494120704       // F6=Conferma
087495120704           when *inkf;
087496120704            ricez='F';
087497120704            // controllo e decodifico i dati del video
087498140903            exsr sr_ctrd02;
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
087533120704
087534120704       // Invio
087535120704           other;
087536120704            if not *in03;
087537140903             exsr sr_ctrd02;
087538120705            if *in28;
087539120705               *in43=*off;
087540120705            else;
087541120705               *in43=*on;
087542120705            endif;
087543120704            endif;
087544120704          endsl;
087545140903       // fine elaborazione 'D02'
087546120704         enddo;
087547120704       endsr;
087548120704       // ------------------------------------------------------------------------
087549140903       // Carico video D02
087550120704       // ------------------------------------------------------------------------
087551140903       begsr sr_card02;
087552120704
087554120704        clear d1descopz;
087555140903        clear d01lna;
087556140918        clear d02lnad;
087557141103        sgmv2 =*zeros;
087558141104        stpv2 =*blanks;
087559120704
087560120704        *in06 = *off;
087561120704        *in28 = *off;
087562120704        *in40 = *off;
087563120704        *in41 = *off;
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
087580141113       // Opzione "03"=duplica
087581141113         when opzio = '03';
087584141113           d1descopz = 'Duplica';
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
087602140903
087603140903       clear dfee;
087604141104       clear campomm;
087605141104       clear campogg;
087606141104       clear campotp;
087607120705
087608141113       select;
087609141113       when comand<>'J' and opzio<>'03';
087611120705        d01lna=%dec(s1tbeke1:3:0);
087612120705        kfil=%dec(d01lna:3:0);
087613141113        if kfil=999;
087614141113           d02lnad='STANDARD';
087615141113        else;
087616141113           chain (kfil) azorg01l;
087617141113           d02lnad=orgdes;
087618141113        endif;
087619140903
087620140903       // recupero i dati dal file
087621140903        chain ('FEE':s1tbeke1) tntbe01l;
087622141113       when comand='J';
087623141113       // In immissione:
087624141113       // recupero i dati del record generico dal file
087625141113        chain(n) ('FEE':'999           ') tntbe01l;
087626141113       other;
087627141113       // in duplica: recupero i dati dal record da cui eseguo la duplica
087628141113        chain(n) ('FEE':s1tbeke1) tntbe01l;
087629141113       endsl;
087630140903        if %found(tntbe01l);
087631140903           dfee=tbeuni;
087632141104           campomm=§feemm;
087633141104           campogg=§feegg;
087634141104           campotp=§feetp;
087635140903           if tbeatb<>' ';
087636140903              *in06=*on    ;
087637140903           endif;
087638140903          if opzio='04';
087639140903          // se richiesta 'cancellazione'
087640140903            if tbeatb = *blanks;
087641140903             d1descopz = 'Annullamento';
087642140903            endif;
087643140903          // se richiesto 'ripristino'
087644140903            if tbeatb = 'A';
087645140903             d1descopz = 'Ripristino';
087646140903            endif;
087647140903          endif;
087648140903        endif;
087649141113       //endif;
087650140903           for I=1 to 20;
087651141104              if smm(I)>*zeros ;
087652140903       //       d02hmg(I)=%editc(Smm(i):'X')*100+%editc(sgg(i):'X');
087653141104                w0040   =%int(Sgg(i))*100+%int(smm(i));
087654141104                sgmv2(I)=%editc(w0040:'X');
087655140903                stpv2(I)=Stp(i);
087656140903              endif;
087657140903           endfor;
087658120704
087659120704       endsr;
087687120704       // ------------------------------------------------------------------------
087688140903       // controllo video 2 (dettaglio per linea)
087689120704       // ------------------------------------------------------------------------
087690140903       begsr sr_ctrd02;
087691120704
087692120704        *in28 = *off;
087693120704        *in40 = *off;
087694141104        for i=41 to 80;
087695141104           *in(i)=*off;
087696141104        endfor;
087735120704        clear vc2msg;
087736120704
087737140903       // Filiale obbligatoria
087738120704        if d01lna=*zeros;
087739120704         vc2msg = msg(01);
087740120704         *in28 = *on;
087741120704         *in40 = *on;
087742120704         leavesr;
087743120704        endif;
087744120704
087745140903       // errore se non è una filiale
087746141113        if d01lna=999;
087747141113           d02lnad='STANDARD';
087748141113        else;
087749141113           chain d01lna azorg01l;
087750141113           if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
087751141113              vc2msg = msg(01);
087752141113              *in28 = *on;
087753141113              *in40 = *on;
087754141113              leavesr;
087755141113           endif;
087756141113           d02lnad=orgdes;
087757141113        endif;
087758120704
087759120704
087760120704       // In immissione la chiave non deve essere già presente
087761141113        if comand = 'J' or opzio='03';
087762140903         tbecod = 'FEE';
087763120704         tbeke1 = %editc(d01lna:'X');
087764140903         chain(n) (tbecod:tbeke1) tntbe01l;
087765120704         if %found(tntbe01l);
087766120704            vc2msg = msg(05);
087767120704            *in28 = *on;
087768120704            *in40 = *on;
087769120704            leavesr;
087770120704         endif;
087771120704        endif;
087772120704
087773141104            for i=1 to %elem(sgmv2);
087774140903              if sgmv2(i)<>*zeros and sgmv2(i)<>*blanks ;
087775120704      /end-free
087776140903     C                   testn                   sgmv2(i)             30
087777120704    2c                   if             *in30  = *off
087778140918     c                             or  %subst(sgmv2(i): 3: 1) < *zero
087779141104     c                   eval      vc2msg = msg(03)
087780120704     c                   eval      *in28 = *on
087781141104     c                   eval      *in(I+40) = *on
087782120704     c                   leavesr
087783120704    2c                   endif
087784140918      /free
087785140918              // errore se mese giorno ripetuto
087786141104                 if i <%elem(sgmv2) and %lookup(sgmv2(i):sgmv2:i+1)>0 ;
087787140918                    vc2msg = msg(03);
087788140918                    *in28 = *on;
087789141104                    *in(i+40) = *on;
087790140918                    leavesr;
087791140918                 endif;
087792141104              // Controllo mese
087793141104                 if %subst(sgmv2(i):3:2)>'12' or
087794141104                    %subst(sgmv2(i):3:2)<'01' ;
087795141104                    vc2msg = msg(04);
087796141104                    *in28 = *on;
087797141104                    *in(i+40) = *on;
087798141104                    leavesr;
087799141104                 endif;
087800141104              // Controllo giorno Min: 01
087801141104                 if %subst(sgmv2(i):1:2)<'01' ;
087802141104                    vc2msg = msg(04);
087803141104                    *in28 = *on;
087804141104                    *in(i+40) = *on;
087805141104                    leavesr;
087806141104                 endif;
087807141104              // Controlli giorno Max: in base al mese
087808141104                 select;
087809141104                 when  %subst(sgmv2(i):3:2) = '11' or
087810141104                       %subst(sgmv2(i):3:2) = '04' or
087811141104                       %subst(sgmv2(i):3:2) = '06' or
087812141104                       %subst(sgmv2(i):3:2) = '09';
087813141104                       if %subst(sgmv2(i):1:2)>'30';
087814141104                          vc2msg = msg(04);
087815141104                          *in28 = *on;
087816141104                          *in(i+40) = *on;
087817141104                          leavesr;
087818141104                       endif;
087819141104                 when  %subst(sgmv2(i):3:2) = '02'
087821141104                       and %subst(sgmv2(i):1:2)>'29';
087822141104                          vc2msg = msg(04);
087823141104                          *in28 = *on;
087824141104                          *in(i+40) = *on;
087825141104                          leavesr;
087826141104                 other;
087827141104                       if  %subst(sgmv2(i):1:2)>'31';
087828141104                          vc2msg = msg(04);
087829141104                          *in28 = *on;
087830141104                          *in(i+40) = *on;
087831141104                          leavesr;
087832141104                       endif ;
087833141104                 endsl;
087834141104             // il tipo deve essere o "S" o "N"
087835141104                 if stpv2(i)<>'S' and stpv2(i)<>'N' ;
087836141104                    vc2msg = msg(02);
087837141104                    *in28 = *on;
087838141104                    *in(i+60) = *on;
087839141104                    leavesr;
087840141104                 endif;
087841141104              else;
087842141104             // se data vuota pulisco anche il tipo
087843141104                 clear stpv2(i);
087844140918              endif;
087845141104            endfor;
087846140918
087847120704       endsr;
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
108600140903Filiale mancante o errata                                                      01
108700141104Immettere "S" se Festivo; "N" se NON Festivo                                   02
108800141104Data già immessa                                                               03
108900141104Data Errata                                                                    04
109000120709Chiave già presente in tabella                                                 05
109100140903                                                                               06
109200140903                                                                               07
