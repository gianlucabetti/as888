000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120614      * Gestione tabella "YBO" = Codici IVA non soggetti a bollo               *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600120614     ftabel00f  if   e           k disk
000700120614     ftntbybod  cf   e             workstn sfile(tbybos01:s01nrr)
000800060428
000900060428      *------------------------------------------------------------------------*
001000060428      * riepilogo indicatori
001100060428      *------------------------------------------------------------------------*
001200060508      * 03 - proteggo campi video 01
001300060509      * 04 - visualizzazione
001400060508      * 20 - gestione subfile 01
001500060508      * 21 - gestione subfile 01
001600060508      * 22 - gestione subfile 01
001700060508      * 23 - gestione subfile 01
001800060428      * 28 - messaggio errore
001900090424      * 40 - Filiale di spunta
002000060502
002100060502      *------------------------------------------------------------------------*
002200060502      * schiere
002300060502      *------------------------------------------------------------------------*
002400060502     d msg             s             78    dim(17) ctdata perrcd(1)
002500060427
002600060428      *------------------------------------------------------------------------*
002700060428      * campi di work
002800060428      *------------------------------------------------------------------------*
002900090428     d data_eur        s               d   datfmt(*eur)
003000060428     d blanks          s                   like(d1descopz)
003100090427     d comand          s              1
003200090427     d opzio           s              2
003300090427     d ricez           s              1
003400060505     d ktbeke1         s                   like(tbeke1)
003500060428     d len             s              5u 0
003600060505     d s01nrr          s              4  0
003700060428     d savopzione      s                   like(s1opzione)
003800060428     d savrcdnbr       s                   like(c1rcdnbr)
003900060531     d savtbeftt       s                   like(tbeftt)
004000060531     d savtbeflt       s                   like(tbeflt)
004100060505     d savtbeuni       s                   like(tbeuni)
004200060428     d wrkkey          s              1
004300060428     d wrkeofs01       s              1
004400060428     d wrkcars01       s              1
004500060505     d wrkcarw01       s              1
004600060505     d wrksfl02        s              1
004700060504     d $video          s             10
004800090427     d ii              s              3  0
004900090428     d fil             s              3  0 dim(500)
005000090428     d des             s             20    dim(500)
005100090428     d iip             s              3    dim(500)
005200060428
005300060428      *------------------------------------------------------------------------*
005400060428      * ds interne/esterne
005500060428      *------------------------------------------------------------------------*
005600060502     d kpjba         e ds
005700060503     d tibs34ds      e ds                  inz
005800060428     d §azute        e ds                  extname(azute00f)
005900060428     d                                     dtaara
006000060428     d §datiute      e ds                  extname(ddatiute)
006100060428     d                                     dtaara
006200120614
006300120614     d dsei          e ds
006400010503
006500060428     d psds           sds
006600060428     d  pgmname          *proc
006700060428
006800090424      // ? PROTOTIPI ?
006900090424      /copy gaitrasrc/srcprotopr,tibs34r
007000060428
007100060428      *------------------------------------------------------------------------*
007200060428      * costanti
007300060428      *------------------------------------------------------------------------*
007400060428     d errore          c                   '1'
007500060428     d eseguito        c                   '0'
007600010503
007700060428      *------------------------------------------------------------------------*
007800010423
007900060428      /free
008000060428
008100060428       exsr sr_c01;
008200060428       exsr sr_uscita;
008300090424
008400060502       // ------------------------------------------------------------------------
008500060502       // gestione subfile
008600060502       // ------------------------------------------------------------------------
008700060428       begsr sr_c01;
008800060428
008900060502       // imposto variabili
009000060428        wrkcars01 = *on;
009100060428        $video = 'C01';
009200060428
009300060502       // inizio elaborazione subfile
009400060428         dou  $video <> 'C01';
009500090427
009600060502       // caricamento subfile
009700060428          exsr sr_cars01;
009800060428
009900060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
010000060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
010100060502       // l'ultimo tasto
010200060428          if c1csrrrn > 0;
010300060428           c1rcdnbr = c1csrrrn;
010400060428          else;
010500060428           c1rcdnbr = savrcdnbr;
010600060428          endif;
010700060428
010800060502       // se non so quale pagina visualizzare forzo pagina 1
010900060428          if c1rcdnbr < 1;
011000060428           eval c1rcdnbr = 1;
011100060428          endif;
011200060428
011300060502       // salvo il record number del subfile
011400060428          savrcdnbr = c1Rcdnbr;
011500060428
011600060502       // Emissione del subfile
011700120614          write tbybop01;
011800120614          exfmt tbyboc01;
011900060428
012000060502       // controllo tasti funzionali del subfile
012100060428          select;
012200060428
012300060502       // F3=Fine
012400060428           when *inkc;
012500060502            $video = *blanks;
012600090427            ricez='C';
012700060428            exsr sr_uscita;
012800060428
012900060502       // F5=Refresh
013000060428           when *inke;
013100060428            wrkcars01 = *on;
013200060428
013300060502       // F10=Immissione
013400060428           when *inkj;
013500060502            $video = 'D01';
013600090427            comand='J';
013700060428            exsr sr_d01;
013800060428
013900060502       // in tutti gli altri casi
014000060428           other;
014100060502       // controllo ed elaborazione scelte su subfile
014200060428            exsr sr_gestsfl;
014300060428          endsl;
014400060428
014500060502       // fine elaborazione 'C01'
014600060428         enddo;
014700060428
014800060428        endsr;
014900060428
015000060502       // ------------------------------------------------------------------------
015100060502       // caricamento subfile
015200060502       // ------------------------------------------------------------------------
015300060428       begsr sr_cars01;
015400010430
015500060502       // se è stato richiesto il caricamento del subfile
015600060428        if wrkcars01 = *on;
015700060428
015800060502       // inizializzo il subfile
015900060505         s01nrr = 0;
016000060428         *in20 = *on;
016100120614         write tbyboc01;
016200060428         *in20 = *off;
016300060428         *in21 = *off;
016400060428         *in22 = *off;
016500060428         *in23 = *off;
016600060428
016700060502       // imposto la chiave di posizionamento e lettura file
016800120614         tbecod = 'YBO';
016900060428
017000090424       // posizionamento file
017100090424         setll tbecod tntbe01l;
017200060428
017300060502       // fino a che non è fine file
017400060428         dou %eof(tntbe01l);
017500060428
017600060502       // leggo file
017700060505          reade(n) tbecod tntbe01l;
017800060428
017900060502       // fine file esco
018000060428          if %eof(tntbe01l);
018100060428           leave;
018200060428          endif;
018300060428
018400060502       // scrivo subfile
018500060428           clear s1opzione;
018600060428           exsr sr_wtrs01;
018700060428
018800060428         enddo;
018900090428
019000090428         clear opzio;
019100090428         clear comand;
019200060428
019300060502       // fine caricamento subfile
019400060428        endif;
019500060428
019600060502       // se scritto almeno un record
019700060505        if s01nrr > 0;
019800060502       // indicatore di sflend
019900060428         eval *in21 = *on;
020000060428        endif;
020100060428
020200060502       // se non scritto neanche un record
020300060505        if s01nrr = 0;
020400060502       // indicatore di sfldsp
020500060428         eval *in23 = *on;
020600060428        endif;
020700060428
020800060502       // disattivo opzione di caricamento subfile
020900060428        wrkcars01 = *off;
021000060428
021100060428       endsr;
021200060428
021300060502       // ------------------------------------------------------------------------
021400060502       // gestione videata
021500060502       // ------------------------------------------------------------------------
021600060428       begsr sr_d01;
021700060428
021800060504       // imposto il video a seconda della funzione richiesta
021900060428        exsr sr_setvideo;
022000060428
022100060502       // imposto variabile
022200060428        $video = 'D01';
022300060428
022400060502       // fino a che la variabile resta settata come 'D01'
022500060428        dou $video <> 'D01';
022600060428
022700060508         *in02 = *off;
022800060508         *in03 = *off;
022900060509         *in04 = *off;
023000060428
023100090424       // se immessa opzione di  'cancellazione/ripristino'
023200060508       // proteggo i campi del video
023300090424          if opzio = '04';
023400060508           *in03 = *on;
023500060508          endif;
023600090424
023700060502       // emetto il video
023800120614          exfmt tbybod01;
023900060428
024000060502       // reset indicatore generico di errore
024100060428         *in28 = *off;
024200060508
024300060508       // pulisco il campo messaggi
024400060508         clear vd1msg;
024500010430
024600060502       // controllo tasti funzionali del video
024700060428         select;
024800060428
024900060502       // F3=Fine
025000060428          when *inkc;
025100090427           ricez='C';
025200060502           $video = *blanks;
025300060505           unlock tntbe01l;
025400060428           exsr sr_uscita;
025500060428
025600060502       // F6=Conferma
025700060428          when *inkf;
025800090427          ricez='F';
025900060502       // controllo e decodifico i dati del video
026000060428           exsr sr_ctrd01;
026100060502       // conferma per annullo/ripristino
026200090424           if opzio = '04';
026300060428            select;
026400060502       // annullamento
026500060502             when tbeatb = *blanks;
026600060428              tbeatb = 'A';
026700060502       // ripristino
026800060502             when tbeatb = 'A';
026900060428              clear tbeatb;
027000060428            endsl;
027100060428           endif;
027200060428
027300060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
027400060428           if not *in28;
027500060504            wrkcarw01 = *on;
027600060504            $video = 'W01';
027700060504            exsr sr_w01;
027800060428           endif;
027900060428
028000060502       // F12=Ritorno
028100060428          when *inkl;
028200060428           clear s1opzione;
028300090427           ricez='L';
028400060505           unlock tntbe01l;
028500090424       // se non è f12 da immissione non ricarico il subfile
028600090424           if comand = 'J';
028700060428            wrkcars01 = *on;
028800060428           else;
028900060428            wrkcars01 = *off;
029000060428           endif;
029100060428           $video = 'C01';
029200060428
029300060502       // Invio
029400060428          other;
029500060508           if not *in03;
029600060508            exsr sr_ctrd01;
029700060508           endif;
029800060428
029900060502         endsl;
030000060428
030100060502       // fine gestione 'D01'
030200060428        enddo;
030300060428
030400060428       endsr;
030500060428
030600060502       // ------------------------------------------------------------------------
030700060502       // imposto i dati a video
030800060502       // ------------------------------------------------------------------------
030900060428       begsr sr_setvideo;
031000060428
031100060502       // pulisco il formato video D01
031200060502        exsr sr_puld01;
031300060428
031400090424       // controllo se 'immissione' o altro
031500060428        select;
031600060428
031700060502       // F10=Immissione
031800090424         when comand = 'J';
031900060428          d1descopz = 'Immissione';
032000060428
032100060502       // Opzione "04"=cancellazione/ripristino
032200090424         when opzio = '04';
032300060428          exsr sr_imposta;
032400060502       // se richiesta 'cancellazione'
032500060502          if tbeatb = *blanks;
032600060502           d1descopz = 'Annullamento';
032700060502          endif;
032800060502       // se richiesto 'ripristino'
032900060502          if tbeatb = 'A';
033000060502           d1descopz = 'Ripristino';
033100060502          endif;
033200060502
033300060502       // Fine scelta
033400060502        endsl;
033500060502
033600060502       // centro la descrizione della funzione nel formato video
033700060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
033800060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
033900060502
034000060502       endsr;
034100060502
034200060502       // ------------------------------------------------------------------------
034300060502       // controllo video
034400060502       // ------------------------------------------------------------------------
034500060502       begsr sr_ctrd01;
034600060502
034700060502        *in28 = *off;
034800060502        *in40 = *off;
034900010430
035000120614       // Codice IVA
035100090424        if d1tbeke1=*blanks;
035200060502         vd1msg = msg(01);
035300060502         *in28 = *on;
035400060502         *in40 = *on;
035500060502         leavesr;
035600060502        endif;
035700090508
035800120614       // Controllo se il codice è presente su tabella EI
035900120614        clear d1okei;
036000120614        tblkut = 1;
036100120614        tblcod = 'EI';
036200120614        setll (TBLkut:TBLcod) TABEL00F;
036300120614        dow not %eof(TABEL00F);
036400120614          reade (TBLkut:TBLcod) TABEL00F;
036500120614          dsei = TBLuni;
036600120614          IF  §EIjei = d1TBEke1;
036700120614            d1okei = 'Presente in tabella EI';
036800120614            leave;
036900120614          ENDIF;
037000120614        ENDDO;
037100120614
037200090508       // In immissione la chiave non deve essere già presente
037300090508        if comand = 'J';
037400120614         tbecod = 'YBO';
037500090508         tbeke1 = d1tbeke1;
037600090508         chain(n) (tbecod:tbeke1:tbeke2) tntbe01l;
037700090508         if %found(tntbe01l);
037800090508            vd1msg = msg(05);
037900090508            *in28 = *on;
038000090508            *in40 = *on;
038100090508            leavesr;
038200090508         endif;
038300090508        endif;
038400060502
038500060502       endsr;
038600060504
038700060504       // ------------------------------------------------------------------------
038800060504       // gestione video dati di trasmissione
038900060504       // ------------------------------------------------------------------------
039000060504       begsr sr_w01;
039100060504
039200060504       // imposto i dati a video
039300060504        exsr sr_carw01;
039400060504
039500060505       // fino a che la variabile resta settata come 'W01'
039600060504        dou $video <> 'W01';
039700060504
039800060504       // reset indicatore generico di errore
039900060504         *in28 = *off;
040000060504
040100060504       // emetto il video
040200120614          exfmt tbybow01;
040300060504
040400060504       // controllo tasti funzionali del video
040500060504         select;
040600060504
040700060504       // F6=Conferma
040800060504          when *inkf;
040900060504       // controllo i dati del video
041000060504           exsr sr_ctrw01;
041100060504       // se non riscontrati errori aggiorno il record corrente
041200060504           if not *in28;
041300060504            exsr sr_aggiorna;
041400060504            if not *in28;
041500090424             if comand = 'J';
041600060504              $video = 'D01';
041700060504              exsr sr_setvideo;
041800060504             else;
041900060504              $video = 'C01';
042000060504             endif;
042100060504            endif;
042200060504           endif;
042300060504
042400060504       // F12=Ritorno
042500060504          when *inkl;
042600060504           $video = 'D01';
042700090427           clear ricez;
042800060504
042900060504       // Invio
043000060504          other;
043100060504           exsr sr_ctrw01;
043200060504
043300060504         endsl;
043400060504
043500060504       // fine gestione 'W01'
043600060504        enddo;
043700060504
043800060504       endsr;
043900060504
044000060504       // ------------------------------------------------------------------------
044100060504       // imposto i dati di trasmissione
044200060504       // ------------------------------------------------------------------------
044300060504       begsr sr_carw01;
044400060504
044500060504       // se pulisco i campi
044600060504         clear w1ftt;
044700060504         clear w1flt;
044800060504         clear w1ftr;
044900060504         clear w1dtr;
045000060504
045100060504       // se non immissione imposto i campi del file
045200090424        if comand <> 'J';
045300060504         w1ftt = tbeftt;
045400060504         w1flt = tbeflt;
045500060504         w1ftr = tbeftr;
045600060504       // imposto la data
045700060504         if tbedtr <> *zeros;
045800090428          data_eur=%date(tbedtr:*iso);
045900090428          w1dtr=%dec(data_eur);
046000060504         endif;
046100060504
046200060504        endif;
046300060504
046400060504       endsr;
046500060504
046600060504       // ------------------------------------------------------------------------
046700060504       // controllo i dati di trasmissione
046800060504       // ------------------------------------------------------------------------
046900060504       begsr sr_ctrw01;
047000060504
047100060504       endsr;
047200060502
047300060502       // ------------------------------------------------------------------------
047400060502       // aggiorno tabella
047500060502       // ------------------------------------------------------------------------
047600060502       begsr sr_aggiorna;
047700060502
047800060502       // imposto campi del file
047900060502        tbeke1 = d1tbeke1;
048000060502
048100060504        tbeftt = w1ftt;
048200060504        tbeflt = w1flt;
048300060502        clear tbeftr;
048400060502        clear tbedtr;
048500060502
048600060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
048700060502        select;
048800060502
048900090427       // F10=immissione -
049000090427         when comand = 'J';
049100090427          clear tbelin;
049200090427          clear tbeuni;
049300090428          clear tbeatb;
049400060508          write tntbe000;
049500060502
049600060502       // Opzione "04"=cancellazione/ripristino
049700090424         when opzio = '04';
049800060502          update tntbe000;
049900010430
050000060502        endsl;
050100060502
050200060502       endsr;
050300060502
050400060502       // ------------------------------------------------------------------------
050500060502       // pulizia video
050600060502       // ------------------------------------------------------------------------
050700060502       begsr sr_puld01;
050800060502
050900060504        clear d1tbeke1;
051000120614        clear d1okei;
051100060504
051200090115        *in06 = *off;
051300090115        *in28 = *off;
051400060504        *in40 = *off;
051500060502
051600060502       endsr;
051700060502
051800060502       // ------------------------------------------------------------------------
051900060502       // imposto i dati a video
052000060502       // ------------------------------------------------------------------------
052100060502       begsr sr_imposta;
052200090115
052300090115       *in06 =*off    ;
052400060502
052500060502       // recupero i dati dal file
052600120614        chain ('YBO':s1tbeke1) tntbe01l;
052700060502
052800060502       // imposto i campi a video
052900120614        d1tbeke1 = tbeke1;
053000090115        if tbeatb<>' '   ;
053100090115        *in06=*on    ;
053200090115        endif   ;
053300120614
053400120614       // Visualizzo se il codice è presente su tabella EI
053500120614        clear d1okei;
053600120614        tblkut = 1;
053700120614        tblcod = 'EI';
053800120614        setll (TBLkut:TBLcod) TABEL00F;
053900120614        dow not %eof(TABEL00F);
054000120614          reade (TBLkut:TBLcod) TABEL00F;
054100120614          dsei = TBLuni;
054200120614          IF  §EIjei = d1TBEke1;
054300120614            d1okei = 'Presente in tabella EI';
054400120614            leave;
054500120614          ENDIF;
054600120614        ENDDO;
054700060502
054800060502       endsr;
054900060502
055000060502       // ------------------------------------------------------------------------
055100060502       // gestione del subfile
055200060502       // ------------------------------------------------------------------------
055300060502       begsr sr_gestsfl;
055400060502
055500060502       // set flag
055600060502        wrkeofs01 = *off;
055700060502
055800060502       // inizio lettura subfile
055900060502        dow wrkeofs01 = *off and *in21;
056000120614         readc tbybos01;
056100060502       // se fine subfile
056200060502         if %eof;
056300060502          wrkcars01 = *on;
056400060502          leave;
056500060502         endif;
056600060502       // se è stata immessa un'opzione
056700060502         if s1opzione <> *zeros;
056800060502       // controllo ed elaborazione opzione immessa
056900060502          select;
057000060502       // opzione "04"=annullo/ripristino
057100090428           when s1opzione = 4;
057200090424            opzio = '04';
057300060502          endsl;
057400010503
057500060502       // se immessa almeno un opzione valida
057600090424          if  opzio <> *blanks;
057700060502       // gstione del formato video
057800060504           exsr sr_d01;
057900060502       // se la gestione si è chiusa con ...
058000060504           select;
058100060502       // F6=Conferma
058200090427            when ricez = 'F';
058300060504             clear s1opzione;
058400060504             wrkcars01 = *on;
058500060502       // F12=Ritorno
058600090427            when ricez = 'L';
058700060504             clear s1opzione;
058800060504             wrkeofs01 = *on;
058900060502       // altrimenti
059000060504            other;
059100060504             *in22 = *on;
059200060504           endsl;
059300060502
059400120614           update tbybos01;
059500060504           *in22 = *off;
059600060504       // fine opzione
059700060504          endif;
059800060502       // fine opzione
059900060502         endif;
060000060502
060100060502        enddo;
060200060502
060300060502       endsr;
060400060502
060500060502
060600060502       // ------------------------------------------------------------------------
060700060502       // scrivo subfile
060800060502       // ------------------------------------------------------------------------
060900060502       begsr sr_wtrs01;
061000060502
061100060502       // se non raggiunto limite massimo di caricamento
061200060505        if s01nrr < 9999;
061300060502       // imposto campi di subfile
061400060502         exsr sr_sets01;
061500060502       // imposto numeratore righe e numero relativo di record
061600060505         s01nrr = s01nrr + 1;
061700060502       // scrivo subfile
061800120614         write tbybos01;
061900060502        endif;
062000060502
062100060502       endsr;
062200060502
062300060502       // ------------------------------------------------------------------------
062400060502       // imposto campi del subfile
062500060502       // ------------------------------------------------------------------------
062600060502       begsr sr_sets01;
062700060502
062800060502        s1tbeke1 = tbeke1;
062900060502        s1tbeatb = tbeatb;
063000120614
063100120614       // scrivo se codice iva è presente sui codici esenzione
063200120614        clear s1okei;
063300120614        tblkut = 1;
063400120614        tblcod = 'EI';
063500120614        setll (TBLkut:TBLcod) TABEL00F;
063600120614        dow not %eof(TABEL00F);
063700120614          reade (TBLkut:TBLcod) TABEL00F;
063800120614          dsei = TBLuni;
063900120614          IF  §EIjei = s1TBEke1;
064000120614            s1okei = 'Presente in tabella EI';
064100120614            leave;
064200120614          ENDIF;
064300120614        ENDDO;
064400060502
064500060502       endsr;
064600060505
064700060502       // ------------------------------------------------------------------------
064800060502       // routine iniziale
064900060502       // ------------------------------------------------------------------------
065000060428         begsr *inzsr;
065100060428
065200060428      /end-free
065300060428
065400060428     c     *entry        plist
065500060428     c                   parm                    kpjba
065600060428
065700060428      /free
065800060428         in(e) §azute;
065900060428         if not %error;
066000060428          in(e) §datiute;
066100060428         endif;
066200060502         if %error or rsut = *blanks;
066300060428          tibs34r(tibs34ds);
066400060428          in §azute;
066500060428          in §datiute;
066600060428         endif;
066700060428
066800060428         endsr;
066900060502
067000060502       // ------------------------------------------------------------------------
067100060502       // uscita dal programma
067200060502       // ------------------------------------------------------------------------
067300060502         begsr sr_uscita;
067400060502
067500060502          *inlr = *on;
067600060502          return;
067700060502
067800060502         endsr;
067900060502
068000060502      /end-free
068100060502
068200060502** -MSG-                                                                     *
068300120614Immettere il Codice                                                            01
068400090508                                                                               02
068500060510
068600060510
068700090508Codice già presente in tabella                                                 05
