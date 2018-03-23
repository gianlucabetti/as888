000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120330      * Gestione tabella "IMP" = Limiti imponibile bolla                       *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600120330     ftntbimpd  cf   e             workstn sfile(tbimps01:s01nrr)
000700060428
000800060428      *------------------------------------------------------------------------*
000900060428      * riepilogo indicatori
001000060428      *------------------------------------------------------------------------*
001100060428      * 01 - ON ricerca -- OFF manutenzione
001200120330      * 02 - proteggo campo codice tabella
001300060508      * 03 - proteggo campi video 01
001400060509      * 04 - visualizzazione
001500060508      * 20 - gestione subfile 01
001600060508      * 21 - gestione subfile 01
001700060508      * 22 - gestione subfile 01
001800060508      * 23 - gestione subfile 01
001900060428      * 28 - messaggio errore
002000120330      * 40 - codice tabella
002100120330      * 41 - peso
002200120330      * 42 - imp. 1
002300120330      * 43 - imp. 2
002400060502
002500060502      *------------------------------------------------------------------------*
002600060502      * schiere
002700060502      *------------------------------------------------------------------------*
002800060502     d msg             s             78    dim(17) ctdata perrcd(1)
002900060427
003000060428      *------------------------------------------------------------------------*
003100060428      * campi di work
003200060428      *------------------------------------------------------------------------*
003300060428     d blanks          s                   like(d1descopz)
003400060502     d comando         s              1
003500060505     d ktbeke1         s                   like(tbeke1)
003600060428     d len             s              5u 0
003700060505     d s01nrr          s              4  0
003800060428     d savopzione      s                   like(s1opzione)
003900060428     d savrcdnbr       s                   like(c1rcdnbr)
004000060531     d savtbeftt       s                   like(tbeftt)
004100060531     d savtbeflt       s                   like(tbeflt)
004200060505     d savtbeuni       s                   like(tbeuni)
004300060428     d wrkkey          s              1
004400060428     d wrkeofs01       s              1
004500060428     d wrkcars01       s              1
004600060505     d wrkcarw01       s              1
004700060504     d $video          s             10
004800060428
004900060428      *------------------------------------------------------------------------*
005000060428      * ds interne/esterne
005100060428      *------------------------------------------------------------------------*
005200060504     d wlbdat          ds                  inz
005300060504     d  g02dat                 1      8  0
005400060504     d  g02inv                 9     16  0
005500060504     d  g02err                17     17
005600060504     d  g02tgi                18     22  0
005700060428
005800120330     d dIMP          e ds
005900060502     d kpjba         e ds
006000060503     d tibs34ds      e ds                  inz
006100120330     d tntbimpds     e ds
006200060428     d §azute        e ds                  extname(azute00f)
006300060428     d                                     dtaara
006400060428     d §datiute      e ds                  extname(ddatiute)
006500060428     d                                     dtaara
006600010503
006700060428     d psds           sds
006800060428     d  pgmname          *proc
006900060428
007000060428      *------------------------------------------------------------------------*
007100060428      * pgm richiamati
007200060428      *------------------------------------------------------------------------*
007300060428     d tibs34r         pr                  extpgm('TIBS34R')
007400060502     d  tibs34ds                           likeds(tibs34ds)
007500060504
007600060504     d xsrda8          pr                  extpgm('XSRDA8')
007700060504     d  wlbdat                             likeds(wlbdat)
007800060428
007900060428      *------------------------------------------------------------------------*
008000060428      * costanti
008100060428      *------------------------------------------------------------------------*
008200060428     d errore          c                   '1'
008300060428     d eseguito        c                   '0'
008400010503
008500060428      *------------------------------------------------------------------------*
008600010423
008700060428      /free
008800060428
008900060428       exsr sr_parm;
009000060428       exsr sr_c01;
009100060428       exsr sr_uscita;
009200010423
009300060502       // ------------------------------------------------------------------------
009400060502       // elaborazione parametri ricevuti
009500060502       // ------------------------------------------------------------------------
009600060428       begsr sr_parm;
009700060428
009800120330        tntbimpdS = kpjbu;
009900060428
010000060428        select;
010100060502       // ricerca e scleta
010200120330         when bimpFun = '1';
010300060428          *in01 = *on;
010400060502       // manutenzione
010500120330         when  bimpFun = *blanks;
010600060428          *in01 = *off;
010700060502       // altrimenti
010800060428         other;
010900120330          bimpEsito = errore;
011000060428           exsr sr_uscita;
011100060428         endsl;
011200060428
011300060428       endsr;
011400010430
011500060502       // ------------------------------------------------------------------------
011600060502       // gestione subfile
011700060502       // ------------------------------------------------------------------------
011800060428       begsr sr_c01;
011900060428
012000060502       // imposto variabili
012100060428        wrkcars01 = *on;
012200060428        $video = 'C01';
012300060428
012400060502       // inizio elaborazione subfile
012500060428         dou  $video <> 'C01';
012600060428
012700060502       // caricamento subfile
012800060428          exsr sr_cars01;
012900060428
013000060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
013100060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
013200060502       // l'ultimo tasto
013300060428          if c1csrrrn > 0;
013400060428           c1rcdnbr = c1csrrrn;
013500060428          else;
013600060428           c1rcdnbr = savrcdnbr;
013700060428          endif;
013800060428
013900060502       // se non so quale pagina visualizzare forzo pagina 1
014000060428          if c1rcdnbr < 1;
014100060428           eval c1rcdnbr = 1;
014200060428          endif;
014300060428
014400060502       // salvo il record number del subfile
014500060428          savrcdnbr = c1Rcdnbr;
014600060428
014700060502       // Emissione del subfile
014800120330          write tbimpp01;
014900120330          exfmt tbimpc01;
015000060428
015100060502       // controllo tasti funzionali del subfile
015200060428          select;
015300060428
015400060502       // F3=Fine
015500060428           when *inkc;
015600060502            $video = *blanks;
015700120330            bimpricez = 'C';
015800060428            exsr sr_uscita;
015900060428
016000060502       // F5=Refresh
016100060428           when *inke;
016200060428            wrkcars01 = *on;
016300060428
016400060502       // F10=Immissione
016500060428           when *inkj;
016600060502            $video = 'D01';
016700120330            clear tntbimpds;
016800120330            bimpcomand = 'J';
016900060428            exsr sr_d01;
017000060428
017100060502       // F13=Ripetizione
017200060428           when *inkm;
017300060428            exsr sr_ripetiopz;
017400060428
017500060502       // in tutti gli altri casi
017600060428           other;
017700060502       // controllo ed elaborazione scelte su subfile
017800060428            exsr sr_gestsfl;
017900060428          endsl;
018000060428
018100060502       // fine elaborazione 'C01'
018200060428         enddo;
018300060428
018400060428        endsr;
018500060428
018600060502       // ------------------------------------------------------------------------
018700060502       // caricamento subfile
018800060502       // ------------------------------------------------------------------------
018900060428       begsr sr_cars01;
019000010430
019100060502       // se è stato richiesto il caricamento del subfile
019200060428        if wrkcars01 = *on;
019300060428
019400060502       // inizializzo il subfile
019500060505         s01nrr = 0;
019600060428         *in20 = *on;
019700120330         write tbimpc01;
019800060428         *in20 = *off;
019900060428         *in21 = *off;
020000060428         *in22 = *off;
020100060428         *in23 = *off;
020200060428
020300060502       // imposto la chiave di posizionamento e lettura file
020400120330         tbecod = 'IMP';
020500060428         tbeke1 = c1setll;
020600060428
020700060502       // se è stato scelto il posizionamento
020800060502         if c1setll <> *blanks;
020900060428          wrkkey = '2';
021000060502       // altrimenti
021100060428         else;
021200060428          wrkkey = '1';
021300060428         endif;
021400060428
021500060502       // posizionamento file
021600060428         exsr sr_setlltbe01;
021700060428
021800060502       // fino a che non è fine file
021900060428         dou %eof(tntbe01l);
022000060428
022100060502       // leggo file
022200060505          reade(n) tbecod tntbe01l;
022300060428
022400060502       // fine file esco
022500060428          if %eof(tntbe01l);
022600060428           leave;
022700060428          endif;
022800060428
022900060502       // se in "ricerca/scelta" non carico i records annullati
023000060502          if (*in01 and tbeatb = *blanks) or not *in01;
023100060502       // scrivo subfile
023200060428           clear s1opzione;
023300060428           exsr sr_wtrs01;
023400060428          endif;
023500060428
023600060428         enddo;
023700060428
023800060502       // fine caricamento subfile
023900060428        endif;
024000060428
024100060502       // se scritto almeno un record
024200060505        if s01nrr > 0;
024300060502       // indicatore di sflend
024400060428         eval *in21 = *on;
024500060428        endif;
024600060428
024700060502       // se non scritto neanche un record
024800060505        if s01nrr = 0;
024900060502       // indicatore di sfldsp
025000060428         eval *in23 = *on;
025100060428        endif;
025200060428
025300060502       // disattivo opzione di caricamento subfile
025400060428        wrkcars01 = *off;
025500060428
025600060428       endsr;
025700060428
025800060502       // ------------------------------------------------------------------------
025900060502       // gestione videata
026000060502       // ------------------------------------------------------------------------
026100060428       begsr sr_d01;
026200060428
026300060504       // imposto il video a seconda della funzione richiesta
026400060428        exsr sr_setvideo;
026500060428
026600060502       // imposto variabile
026700060428        $video = 'D01';
026800060428
026900060502       // fino a che la variabile resta settata come 'D01'
027000060428        dou $video <> 'D01';
027100060428
027200060508         *in02 = *off;
027300060508         *in03 = *off;
027400060509         *in04 = *off;
027500060428
027600060502       // se immessa opzione di 'scelta'
027700120330         if bimpopzio = '01';
027800120330          bimpke1 = s1tbeke1;
027900060428          exsr sr_uscita;
028000060428
028100060508       // se immessa un'altra opzione
028200060508         else;
028300060508       // se non è immissione/copia proteggo il campo della causale
028400120330          if bimpcomand <> 'J' and bimpopzio <> '03';
028500060428           *in02 = *on;
028600060428          endif;
028700060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
028800060508       // proteggo i campi del video
028900120330          if bimpopzio = '04' or bimpopzio = '05';
029000060508           *in03 = *on;
029100060508          endif;
029200060509       // se immessa opzione di 'visualizzazione'
029300060509       // non attivo F6
029400120330          if bimpopzio = '05';
029500060509           *in04 = *on;
029600060509          endif;
029700060502       // emetto il video
029800120330          exfmt tbimpd01;
029900060428         endif;
030000060428
030100060502       // reset indicatore generico di errore
030200060428         *in28 = *off;
030300060508
030400060508       // pulisco il campo messaggi
030500060508         clear vd1msg;
030600010430
030700060502       // controllo tasti funzionali del video
030800060428         select;
030900060428
031000060502       // F3=Fine
031100060428          when *inkc;
031200120330           bimpricez = 'C';
031300060502           $video = *blanks;
031400060505           unlock tntbe01l;
031500060428           exsr sr_uscita;
031600060428
031700060502       // F6=Conferma
031800060428          when *inkf;
031900120330           bimpricez = 'F';
032000060502       // controllo e decodifico i dati del video
032100060428           exsr sr_ctrd01;
032200060502       // conferma per annullo/ripristino
032300120330           if bimpopzio = '04';
032400060428            select;
032500060502       // annullamento
032600060502             when tbeatb = *blanks;
032700060428              tbeatb = 'A';
032800060502       // ripristino
032900060502             when tbeatb = 'A';
033000060428              clear tbeatb;
033100060428            endsl;
033200060428           endif;
033300060428
033400060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
033500060428           if not *in28;
033600060504            wrkcarw01 = *on;
033700060504            $video = 'W01';
033800060504            exsr sr_w01;
033900060428           endif;
034000060428
034100060502       // F8=Record successivo
034200060428          when  *inkh;
034300060428           clear s1opzione;
034400060428           wrkcars01 = *off;
034500060428           $video = 'C01';
034600120330           bimpricez = 'H';
034700060428
034800060502       // F12=Ritorno
034900060428          when *inkl;
035000060428           clear s1opzione;
035100120330           bimpricez = 'L';
035200060505           unlock tntbe01l;
035300060508       // se non è f12 da immissione/copia non ricarico il subfile
035400120330           if bimpcomand = 'J' or bimpopzio = '03';
035500060428            wrkcars01 = *on;
035600060428           else;
035700060428            wrkcars01 = *off;
035800060428           endif;
035900060428           $video = 'C01';
036000060428
036100060502       // Invio
036200060428          other;
036300060508           if not *in03;
036400060508            exsr sr_ctrd01;
036500060508           endif;
036600060428
036700060502         endsl;
036800060428
036900060502       // fine gestione 'D01'
037000060428        enddo;
037100060428
037200060428       endsr;
037300060502
037400060502       // ------------------------------------------------------------------------
037500060502       // ripeto opzione in tutte le righe del subfile
037600060502       // ------------------------------------------------------------------------
037700060428       begsr sr_ripetiopz;
037800060428
037900060428        if c1csrrrn > 0;
038000060505         s01nrr = c1csrrrn;
038100120330         chain s01nrr tbimps01;
038200060428         if %found and s1opzione > 0;
038300060428          savopzione = s1opzione;
038400060428          *in22 = *on;
038500120330          update tbimps01;
038600060428
038700060428          wrkeofs01 = *off;
038800060428          dou wrkeofs01 = *on;
038900060505           s01nrr = s01nrr + 1;
039000120330           chain s01nrr tbimps01;
039100060428           if %found;
039200060428            s1opzione = savopzione;
039300120330            update tbimps01;
039400060428           else;
039500060428            wrkeofs01 = *on;
039600060428           endif;
039700060428          enddo;
039800060428
039900060428          *in22 = *off;
040000060428
040100060428         endif;
040200060428
040300060428        endif;
040400060428
040500060428       endsr;
040600060428
040700060502       // ------------------------------------------------------------------------
040800060502       // imposto i dati a video
040900060502       // ------------------------------------------------------------------------
041000060428       begsr sr_setvideo;
041100060428
041200060502       // pulisco il formato video D01
041300060502        exsr sr_puld01;
041400060428
041500060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
041600060428        select;
041700060428
041800060502       // F10=Immissione
041900120330         when bimpcomand = 'J';
042000060428          d1descopz = 'Immissione';
042100060428
042200060502       // Opzione "02"=modifica
042300120330         when bimpopzio = '02';
042400060428          d1descopz = 'Modifica';
042500060428          exsr sr_imposta;
042600060428
042700060502       // Opzione "03"=copia
042800120330         when bimpopzio = '03';
042900060428          d1descopz = 'Copia';
043000060428          exsr sr_imposta;
043100060428
043200060502       // Opzione "04"=cancellazione/ripristino
043300120330         when bimpopzio = '04';
043400060428          exsr sr_imposta;
043500060502       // se richiesta 'cancellazione'
043600060502          if tbeatb = *blanks;
043700060502           d1descopz = 'Annullamento';
043800060502          endif;
043900060502       // se richiesto 'ripristino'
044000060502          if tbeatb = 'A';
044100060502           d1descopz = 'Ripristino';
044200060502          endif;
044300060502
044400060502       // Opzione "05"=visualizzazione
044500120330         when bimpOpzio ='05';
044600060502          d1descopz = 'Visualizzazione';
044700060502          exsr sr_imposta;
044800060502
044900060502       // Fine scelta
045000060502        endsl;
045100060502
045200060502       // centro la descrizione della funzione nel formato video
045300060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
045400060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
045500060502
045600060502       endsr;
045700060502
045800060502       // ------------------------------------------------------------------------
045900060502       // controllo video
046000060502       // ------------------------------------------------------------------------
046100060502       begsr sr_ctrd01;
046200060502
046300060502        *in28 = *off;
046400060502        *in40 = *off;
046500060502        *in41 = *off;
046600010430
046700120330       // codice tabella
046800060502        if d1tbeke1 = *blanks;
046900060502         vd1msg = msg(01);
047000060502         *in28 = *on;
047100060502         *in40 = *on;
047200060502         leavesr;
047300060502        endif;
047400060508
047500060508       // se immissione controllo che non esista già
047600120330        if bimpcomand = 'J' or bimpopzio = '03';
047700120330         tbecod = 'IMP';
047800060508         tbeke1 = d1tbeke1;
047900060508         tbeke2 = d1tbeke2;
048000060508         clear tbelin;
048100060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
048200060508         if %found(tntbe01l);
048300060508          vd1msg = msg(05);
048400060508          *in28 = *on;
048500060508          *in40 = *on;
048600060508          leavesr;
048700060508         endif;
048800060508        endif;
048900060502
049000120330       //?Peso
049100120330         IF  D1peso = 0;
049200120330          vd1msg = msg(02);
049300120330          *in28 = *on;
049400120330          *in41 = *on;
049500120330          leavesr;
049600120330         ENDIF;
049700120330
049800120330       //?Imponibile 1
049900120330         IF  D1imp1 = 0;
050000120330          vd1msg = msg(03);
050100120330          *in28 = *on;
050200120330          *in42 = *on;
050300120330          leavesr;
050400120330         ENDIF;
050500120330
050600120330       //?Imponibile 2
050700120330         IF  D1imp2 = 0;
050800120330          vd1msg = msg(03);
050900120330          *in28 = *on;
051000120330          *in43 = *on;
051100120330          leavesr;
051200120330         ENDIF;
051300120330
051400120330       //?Imponibile 2 > Imponibile 1
051500120330         IF  D1imp2 <= D1Imp1;
051600120330          vd1msg = msg(04);
051700120330          *in28 = *on;
051800120330          *in43 = *on;
051900120330          leavesr;
052000120330         ENDIF;
052100060502
052200060502       endsr;
052300060504
052400060504       // ------------------------------------------------------------------------
052500060504       // gestione video dati di trasmissione
052600060504       // ------------------------------------------------------------------------
052700060504       begsr sr_w01;
052800060504
052900060504       // imposto i dati a video
053000060504        exsr sr_carw01;
053100060504
053200060505       // fino a che la variabile resta settata come 'W01'
053300060504        dou $video <> 'W01';
053400060504
053500060504       // reset indicatore generico di errore
053600060504         *in28 = *off;
053700060504
053800060504       // emetto il video
053900120330          exfmt tbimpw01;
054000060504
054100060504       // controllo tasti funzionali del video
054200060504         select;
054300060504
054400060504       // F6=Conferma
054500060504          when *inkf;
054600060504       // controllo i dati del video
054700060504           exsr sr_ctrw01;
054800060504       // se non riscontrati errori aggiorno il record corrente
054900060504           if not *in28;
055000060504            exsr sr_aggiorna;
055100120330            $video = 'C01';
055200060504           endif;
055300060504
055400060504       // F12=Ritorno
055500060504          when *inkl;
055600060504           $video = 'D01';
055700120330           clear bimpricez;
055800060504
055900060504       // Invio
056000060504          other;
056100060504           exsr sr_ctrw01;
056200060504
056300060504         endsl;
056400060504
056500060504       // fine gestione 'W01'
056600060504        enddo;
056700060504
056800060504       endsr;
056900060504
057000060504       // ------------------------------------------------------------------------
057100060504       // imposto i dati di trasmissione
057200060504       // ------------------------------------------------------------------------
057300060504       begsr sr_carw01;
057400060504
057500060504       // se pulisco i campi
057600060504         clear w1ftt;
057700060504         clear w1flt;
057800060504         clear w1ftr;
057900060504         clear w1dtr;
058000060504
058100060504       // se non immissione imposto i campi del file
058200120330        if bimpcomand <> 'J';
058300060504         w1ftt = tbeftt;
058400060504         w1flt = tbeflt;
058500060504         w1ftr = tbeftr;
058600060504       // imposto la data
058700060504         if tbedtr <> *zeros;
058800060504          clear wlbdat;
058900060504          g02inv = tbedtr;
059000060504          g02err = '3';
059100060504          xsrda8(wlbdat);
059200060504          w1dtr = g02dat;
059300060504         endif;
059400060504
059500060504        endif;
059600060504
059700060504       endsr;
059800060504
059900060504       // ------------------------------------------------------------------------
060000060504       // controllo i dati di trasmissione
060100060504       // ------------------------------------------------------------------------
060200060504       begsr sr_ctrw01;
060300060504
060400060504       endsr;
060500060502
060600060502       // ------------------------------------------------------------------------
060700060502       // aggiorno tabella
060800060502       // ------------------------------------------------------------------------
060900060502       begsr sr_aggiorna;
061000060502
061100060502       // imposto campi del file
061200060509        clear tbelin;
061300060502        tbeke1 = d1tbeke1;
061400060509        tbeke2 = d1tbeke2;
061500060502
061600120330        §imppeso = d1peso;
061700120330        §impimp1 = d1imp1;
061800120330        §impimp2 = d1imp2;
061900060502
062000120330        tbeuni = dIMP;
062100060502
062200060504        tbeftt = w1ftt;
062300060504        tbeflt = w1flt;
062400060502        clear tbeftr;
062500060502        clear tbedtr;
062600060502
062700060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
062800060502        select;
062900060502
063000060502       // F10=immissione - Opzione "03"=copia
063100120330         when bimpcomand = 'J' or bimpopzio = '03';
063200060502       // scrivo nuovo record con gestione errore per chiave duplicata
063300060508          write tntbe000;
063400060502
063500060502       // Opzione "02"=modifica
063600120330         when bimpopzio = '02';
063700060502       // update record e controllo errore per chiave duplicata
063800060508          update tntbe000;
063900060502
064000060502       // Opzione "04"=cancellazione/ripristino
064100120330         when bimpopzio = '04';
064200060502          update tntbe000;
064300010430
064400060502        endsl;
064500060502
064600060502       endsr;
064700060502
064800060502       // ------------------------------------------------------------------------
064900060502       // pulizia video
065000060502       // ------------------------------------------------------------------------
065100060502       begsr sr_puld01;
065200060502
065300060504        clear d1descopz;
065400060504        clear d1tbeke1;
065500120330        clear d1peso;
065600120330        clear d1imp1;
065700120330        clear d1imp2;
065800060504
065900060504        *in28 = *off;
066000060504        *in40 = *off;
066100060504        *in41 = *off;
066200060502
066300060502       endsr;
066400060502
066500060502       // ------------------------------------------------------------------------
066600060502       // imposto i dati a video
066700060502       // ------------------------------------------------------------------------
066800060502       begsr sr_imposta;
066900060502
067000060502       // recupero i dati dal file
067100060505        ktbeke1 = s1tbeke1;
067200060508        clear tbelin;
067300060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
067400060502
067500060502       // imposto i campi a video
067600060505        d1tbeke2 = tbeke2;
067700060502        d1tbeke1 = tbeke1;
067800120330        dimp = tbeuni;
067900120330        d1peso = §imppeso;
068000120330        d1imp1 = §impimp1;
068100120330        d1imp2 = §impimp2;
068200060502
068300060502       endsr;
068400060502
068500060502       // ------------------------------------------------------------------------
068600060502       // gestione del subfile
068700060502       // ------------------------------------------------------------------------
068800060502       begsr sr_gestsfl;
068900060502
069000060502       // set flag
069100060502        wrkeofs01 = *off;
069200060502
069300060502       // inizio lettura subfile
069400060502        dow wrkeofs01 = *off and *in21;
069500120330         readc tbimps01;
069600060502       // se fine subfile
069700060502         if %eof;
069800060502          wrkcars01 = *on;
069900060502          leave;
070000060502         endif;
070100060502       // se è stata immessa un'opzione
070200060502         if s1opzione <> *zeros;
070300060502       // reset ds di servizio
070400120330          clear tntbimpds;
070500060502       // controllo ed elaborazione opzione immessa
070600060502          select;
070700060502       // opzione "01"=scelta
070800060502           when s1opzione = 1 and *in01;
070900120330            bimpopzio = '01';
071000060502       // opzione "02"=modifica
071100060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
071200120330            bimpopzio = '02';
071300060502       // opzione "03"=copia
071400060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
071500120330            bimpopzio = '03';
071600060502       // opzione "04"=annullo/ripristino
071700060502           when s1opzione = 4 and not *in01;
071800120330            bimpopzio = '04';
071900060502       // opzione "05"=visualizzazione
072000060502           when s1opzione = 5;
072100120330            bimpopzio = '05';
072200060502          endsl;
072300010503
072400060502       // se immessa almeno un opzione valida
072500120330          if  bimpopzio <> *blanks;
072600060502       // gstione del formato video
072700060504           exsr sr_d01;
072800060502       // se la gestione si è chiusa con ...
072900060504           select;
073000060502       // F6=Conferma
073100120330            when bimpricez = 'F';
073200060504             clear s1opzione;
073300060504             wrkcars01 = *on;
073400060502       // F12=Ritorno
073500120330            when bimpricez = 'L';
073600060504             clear s1opzione;
073700060504             wrkeofs01 = *on;
073800060502       // altrimenti
073900060504            other;
074000060504             *in22 = *on;
074100060504           endsl;
074200060502
074300120330           update tbimps01;
074400060504           *in22 = *off;
074500060504       // fine opzione
074600060504          endif;
074700060502       // fine opzione
074800060502         endif;
074900060502
075000060502        enddo;
075100060502
075200060502       endsr;
075300060502
075400060502       // ------------------------------------------------------------------------
075500060502       // posizionamento sul file
075600060502       // ------------------------------------------------------------------------
075700060502       begsr sr_setlltbe01;
075800060502
075900060502        select;
076000060502       // chiave totale
076100060502         when wrkkey = '1';
076200060502          setll tbecod tntbe01l;
076300060502       // chiave parziale
076400060502         when wrkkey = '2';
076500060502          setll (tbecod:tbeke1) tntbe01l;
076600060502        endsl;
076700060502
076800060502       endsr;
076900060502
077000060502       // ------------------------------------------------------------------------
077100060502       // scrivo subfile
077200060502       // ------------------------------------------------------------------------
077300060502       begsr sr_wtrs01;
077400060502
077500060502       // se non raggiunto limite massimo di caricamento
077600060505        if s01nrr < 9999;
077700060502       // imposto campi di subfile
077800060502         exsr sr_sets01;
077900060502       // imposto numeratore righe e numero relativo di record
078000060505         s01nrr = s01nrr + 1;
078100060502       // scrivo subfile
078200120330         write tbimps01;
078300060502        endif;
078400060502
078500060502       endsr;
078600060502
078700060502       // ------------------------------------------------------------------------
078800060502       // imposto campi del subfile
078900060502       // ------------------------------------------------------------------------
079000060502       begsr sr_sets01;
079100060502
079200060502        s1tbeke1 = tbeke1;
079300060505        s1tbeke2 = tbeke2;
079400120330        dimp = tbeuni;
079500120330        s1peso = §imppeso;
079600120330        s1imp1 = §impimp1;
079700120330        s1imp2 = §impimp2;
079800060502        s1tbeatb = tbeatb;
079900060502
080000060502       endsr;
080100060428
080200060502       // ------------------------------------------------------------------------
080300060502       // routine iniziale
080400060502       // ------------------------------------------------------------------------
080500060428         begsr *inzsr;
080600060428
080700060428      /end-free
080800060428
080900060428     c     *entry        plist
081000060428     c                   parm                    kpjba
081100060428
081200060428      /free
081300060428         in(e) §azute;
081400060428         if not %error;
081500060428          in(e) §datiute;
081600060428         endif;
081700060502         if %error or rsut = *blanks;
081800060428          tibs34r(tibs34ds);
081900060428          in §azute;
082000060428          in §datiute;
082100060428         endif;
082200060428
082300060428         endsr;
082400060502
082500060502       // ------------------------------------------------------------------------
082600060502       // uscita dal programma
082700060502       // ------------------------------------------------------------------------
082800060502         begsr sr_uscita;
082900060502
083000120330          if bimpesito = *blanks;
083100120330           bimpesito = eseguito;
083200060502          endif;
083300060502
083400120330          kpjbu = tntbimpds;
083500060502
083600060502          *inlr = *on;
083700060502          return;
083800060502
083900060502         endsr;
084000060502
084100060502      /end-free
084200060502
084300060502** -MSG-                                                                     *
084400120330Immettere il codice tabella                                                    01
084500120330Immettere il peso                                                              02
084600120330Immettere l'imponibile                                                         03
084700120330Imponibile 2 deve essere maggiore di Imponibile 1                              04
084800120330Codice già esistente                                                           05
