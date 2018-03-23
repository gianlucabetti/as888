000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300130116      * Tabella raggruppamento clienti   - testata
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600121217     ftntb96d   cf   e             workstn sfile(tb96s01:s01nrr)
000700060428
000800060428      *------------------------------------------------------------------------*
000900060428      * riepilogo indicatori
001000060428      *------------------------------------------------------------------------*
001100060428      * 01 - ON ricerca -- OFF manutenzione
001200060508      * 02 - proteggo campo causale
001300060508      * 03 - proteggo campi video 01
001400060509      * 04 - visualizzazione
001500121220      * 05 - vedo sse tabella piena o vuota
001600060508      * 20 - gestione subfile 01
001700060508      * 21 - gestione subfile 01
001800060508      * 22 - gestione subfile 01
001900060508      * 23 - gestione subfile 01
002000060508      * 24 - gestione subfile 02
002100060508      * 25 - gestione subfile 02
002200060428      * 28 - messaggio errore
002300060502      * 40 - causale
002400060502      * 41 - descrizione
002500060505      * 47 - descrizione subfile
002600060502
002700060502      *------------------------------------------------------------------------*
002800060502      * schiere
002900060502      *------------------------------------------------------------------------*
003000060502     d msg             s             78    dim(17) ctdata perrcd(1)
003100060427
003200060428      *------------------------------------------------------------------------*
003300060428      * campi di work
003400060428      *------------------------------------------------------------------------*
003500060428     d blanks          s                   like(d1descopz)
003600060502     d comando         s              1
003700060505     d ktbeke1         s                   like(tbeke1)
003800060428     d len             s              5u 0
003900060505     d s01nrr          s              4  0
004000060505     d s02nrr          s              4  0
004100060428     d savopzione      s                   like(s1opzione)
004200060428     d savrcdnbr       s                   like(c1rcdnbr)
004300060505     d savrec          s                   like(s02nrr)
004400060531     d savtbeftt       s                   like(tbeftt)
004500060531     d savtbeflt       s                   like(tbeflt)
004600060505     d savtbeuni       s                   like(tbeuni)
004700060428     d wrkkey          s              1
004800060428     d wrkeofs01       s              1
004900060428     d wrkcars01       s              1
005000060505     d wrkcarw01       s              1
005100060505     d wrksfl02        s              1
005200060504     d $video          s             10
005300121220     d wnum            s              4  0
005400121220     d wke1            s             15
005500060428
005600060428      *------------------------------------------------------------------------*
005700060428      * ds interne/esterne
005800060428      *------------------------------------------------------------------------*
005900060504     d wlbdat          ds                  inz
006000060504     d  g02dat                 1      8  0
006100060504     d  g02inv                 9     16  0
006200060504     d  g02err                17     17
006300060504     d  g02tgi                18     22  0
006400121217     d
006500121217     d TNTB97DS        ds                  inz
006600121217     d  B97ke1                 1      3
006700121217     d  B97ke2                 4     10  0
006800121217     d  B97descr              11     55
006900060428
007000121219     d kpjba         e ds
007100060503     d tibs34ds      e ds                  inz
007200121217     d tntb96ds      e ds
007300060428     d §azute        e ds                  extname(azute00f)
007400060428     d                                     dtaara
007500060428     d §datiute      e ds                  extname(ddatiute)
007600060428     d                                     dtaara
007700010503
007800060428     d psds           sds
007900060428     d  pgmname          *proc
008000060428
008100060428      *------------------------------------------------------------------------*
008200060428      * pgm richiamati
008300060428      *------------------------------------------------------------------------*
008400060428     d tibs34r         pr                  extpgm('TIBS34R')
008500060502     d  tibs34ds                           likeds(tibs34ds)
008600121217
008700121217     d tntb97r         pr                  extpgm('TNTB97R')
008800121217     d  kpjba                              likeds(kpjba)
008900060504
009000060504     d xsrda8          pr                  extpgm('XSRDA8')
009100060504     d  wlbdat                             likeds(wlbdat)
009200060428
009300060428      *------------------------------------------------------------------------*
009400060428      * costanti
009500060428      *------------------------------------------------------------------------*
009600060428     d errore          c                   '1'
009700060428     d eseguito        c                   '0'
009800010503
009900060428      *------------------------------------------------------------------------*
010000010423
010100060428      /free
010200060428
010300060428       exsr sr_parm;
010400060428       exsr sr_c01;
010500060428       exsr sr_uscita;
010600010423
010700060502       // ------------------------------------------------------------------------
010800060502       // elaborazione parametri ricevuti
010900060502       // ------------------------------------------------------------------------
011000060428       begsr sr_parm;
011100060428
011200121217        tntb96dS = kpjbu;
011300060428
011400060428        select;
011500120402       // ricerca e scelta
011600121217         when b96Fun = '1';
011700060428          *in01 = *on;
011800121217          ktbeke1=b96ke1  ;
011900120402
012000060502       // manutenzione
012100121217         when  b96Fun = *blanks;
012200060428          *in01 = *off;
012300121217          ktbeke1=b96ke1  ;
012400120402
012500060502       // altrimenti
012600060428         other;
012700121217          b96Esito = errore;
012800060428           exsr sr_uscita;
012900060428         endsl;
013000060428
013100060428       endsr;
013200010430
013300060502       // ------------------------------------------------------------------------
013400060502       // gestione subfile
013500060502       // ------------------------------------------------------------------------
013600060428       begsr sr_c01;
013700060428
013800060502       // imposto variabili
013900060428        wrkcars01 = *on;
014000060428        $video = 'C01';
014100060428
014200060502       // inizio elaborazione subfile
014300060428         dou  $video <> 'C01';
014400060428
014500060502       // caricamento subfile
014600060428          exsr sr_cars01;
014700060428
014800060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
014900060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
015000060502       // l'ultimo tasto
015100060428          if c1csrrrn > 0;
015200060428           c1rcdnbr = c1csrrrn;
015300060428          else;
015400060428           c1rcdnbr = savrcdnbr;
015500060428          endif;
015600060428
015700060502       // se non so quale pagina visualizzare forzo pagina 1
015800060428          if c1rcdnbr < 1;
015900060428           eval c1rcdnbr = 1;
016000060428          endif;
016100060428
016200060502       // salvo il record number del subfile
016300060428          savrcdnbr = c1Rcdnbr;
016400060428
016500060502       // Emissione del subfile
016600121219          write tb96Z01;
016700121217          exfmt tb96c01;
016800060428
016900060502       // controllo tasti funzionali del subfile
017000060428          select;
017100060428
017200060502       // F3=Fine
017300060428           when *inkc;
017400060502            $video = *blanks;
017500121217            b96ricez = 'C';
017600060428            exsr sr_uscita;
017700060428
017800060502       // F5=Refresh
017900060428           when *inke;
018000060428            wrkcars01 = *on;
018100060428
018200060502       // F10=Immissione
018300060428           when *inkj;
018400060502            $video = 'D01';
018500121217            clear tntb96ds;
018600121217            b96comand = 'J';
018700060428            exsr sr_d01;
018800060428
018900060502       // in tutti gli altri casi
019000060428           other;
019100060502       // controllo ed elaborazione scelte su subfile
019200060428            exsr sr_gestsfl;
019300060428          endsl;
019400060428
019500060502       // fine elaborazione 'C01'
019600060428         enddo;
019700060428
019800060428        endsr;
019900060428
020000060502       // ------------------------------------------------------------------------
020100060502       // caricamento subfile
020200060502       // ------------------------------------------------------------------------
020300060428       begsr sr_cars01;
020400010430
020500060502       // se è stato richiesto il caricamento del subfile
020600060428        if wrkcars01 = *on;
020700060428
020800060502       // inizializzo il subfile
020900060505         s01nrr = 0;
021000060428         *in20 = *on;
021100121217         write tb96c01;
021200060428         *in20 = *off;
021300060428         *in21 = *off;
021400060428         *in22 = *off;
021500060428         *in23 = *off;
021600060428
021700060502       // imposto la chiave di posizionamento e lettura file
021800121217         tbecod = 'KPV';
021900120329         tbeke2 = c1setll;
022000121219          if ktbeke1=*blanks    ;
022100121219           ktbeke1=%editc(dutpou:'X')  ;
022200121219          endif  ;
022300060428
022400060502       // se è stato scelto il posizionamento
022500060502         if c1setll <> *blanks;
022600060428          wrkkey = '2';
022700120402
022800060502       // altrimenti
022900060428         else;
023000060428          wrkkey = '1';
023100060428         endif;
023200060428
023300060502       // posizionamento file
023400060428         exsr sr_setlltbe01;
023500060428
023600060502       // fino a che non è fine file
023700060428         dou %eof(tntbe01l);
023800060428
023900060502       // leggo file
024000120330         select  ;
024100120402         when wrkkey = '1'and ktbeke1=*blanks ;
024200120330          reade(n) (tbecod) tntbe01l;
024300120330
024400120402         when wrkkey = '1'and ktbeke1<>*blanks ;
024500120402          reade(n) (tbecod:ktbeke1) tntbe01l;
024600120402
024700120330         when wrkkey = '2';
024800121219          reade(n) (tbecod:ktbeke1) tntbe01l;
024900120330        endsl;
025000060428
025100060502       // fine file esco
025200060428          if %eof(tntbe01l);
025300060428           leave;
025400060428          endif;
025500060428
025600060502       // se in "ricerca/scelta" non carico i records annullati
025700060502          if (*in01 and tbeatb = *blanks) or not *in01;
025800060502       // scrivo subfile
025900060428           clear s1opzione;
026000060428           exsr sr_wtrs01;
026100060428          endif;
026200060428
026300060428         enddo;
026400060428
026500060502       // fine caricamento subfile
026600060428        endif;
026700060428
026800060502       // se scritto almeno un record
026900060505        if s01nrr > 0;
027000060502       // indicatore di sflend
027100060428         eval *in21 = *on;
027200060428        endif;
027300060428
027400060502       // se non scritto neanche un record
027500060505        if s01nrr = 0;
027600060502       // indicatore di sfldsp
027700060428         eval *in23 = *on;
027800060428        endif;
027900060428
028000060502       // disattivo opzione di caricamento subfile
028100060428        wrkcars01 = *off;
028200060428
028300060428       endsr;
028400060428
028500060502       // ------------------------------------------------------------------------
028600060502       // gestione videata
028700060502       // ------------------------------------------------------------------------
028800060428       begsr sr_d01;
028900060428
029000060504       // imposto il video a seconda della funzione richiesta
029100060428        exsr sr_setvideo;
029200060428
029300060502       // imposto variabile
029400060428        $video = 'D01';
029500060428
029600060502       // fino a che la variabile resta settata come 'D01'
029700060428        dou $video <> 'D01';
029800060428
029900060508         *in02 = *off;
030000060508         *in03 = *off;
030100060509         *in04 = *off;
030200060428
030300060502       // se immessa opzione di 'scelta'
030400121217         if b96opzio = '01';
030500121217          b96ke1 = s1tbeke1;
030600121217          b96ke2 = s1tbeke2;
030700121219          b96des = s1tbeuni  ;
030800060428          exsr sr_uscita;
030900060428
031000060508       // se immessa un'altra opzione
031100060508         else;
031200060508       // se non è immissione/copia proteggo il campo della causale
031300121217          if b96comand <> 'J' and b96opzio <> '03';
031400060428           *in02 = *on;
031500060428          endif;
031600060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
031700060508       // proteggo i campi del video
031800121217          if b96opzio = '04' or b96opzio = '05';
031900060508           *in03 = *on;
032000060508          endif;
032100060509       // se immessa opzione di 'visualizzazione'
032200060509       // non attivo F6
032300121217          if b96opzio = '05';
032400060509           *in04 = *on;
032500060509          endif;
032600060502       // emetto il video
032700121217          exfmt tb96d01;
032800060428         endif;
032900060428
033000060502       // reset indicatore generico di errore
033100060428         *in28 = *off;
033200060508
033300060508       // pulisco il campo messaggi
033400060508         clear vd1msg;
033500010430
033600060502       // controllo tasti funzionali del video
033700060428         select;
033800060428
033900060502       // F3=Fine
034000060428          when *inkc;
034100121217           b96ricez = 'C';
034200060502           $video = *blanks;
034300060505           unlock tntbe01l;
034400060428           exsr sr_uscita;
034500060428
034600060502       // F6=Conferma
034700060428          when *inkf;
034800121217           b96ricez = 'F';
034900060502       // controllo e decodifico i dati del video
035000060428           exsr sr_ctrd01;
035100060502       // conferma per annullo/ripristino
035200121217           if b96opzio = '04';
035300060428            select;
035400060502       // annullamento
035500060502             when tbeatb = *blanks;
035600060428              tbeatb = 'A';
035700060502       // ripristino
035800060502             when tbeatb = 'A';
035900060428              clear tbeatb;
036000060428            endsl;
036100060428           endif;
036200120329
036300120329            exsr sr_aggiorna;
036400120329
036500120329              $video = 'C01';
036600120329            wrkcars01 = *on;
036700060428
036800060502       // F12=Ritorno
036900060428          when *inkl;
037000060428           clear s1opzione;
037100121217           b96ricez = 'L';
037200060505           unlock tntbe01l;
037300060508       // se non è f12 da immissione/copia non ricarico il subfile
037400121217           if b96comand = 'J' or b96opzio = '03';
037500060428            wrkcars01 = *on;
037600060428           else;
037700060428            wrkcars01 = *off;
037800060428           endif;
037900060428           $video = 'C01';
038000060428
038100060502       // Invio
038200060428          other;
038300060508           if not *in03;
038400060508            exsr sr_ctrd01;
038500060508           endif;
038600060428
038700060502         endsl;
038800060428
038900060502       // fine gestione 'D01'
039000060428        enddo;
039100060428
039200060428       endsr;
039300060502
039400060502       // ------------------------------------------------------------------------
039500060502       // imposto i dati a video
039600060502       // ------------------------------------------------------------------------
039700060428       begsr sr_setvideo;
039800060428
039900060502       // pulisco il formato video D01
040000060502        exsr sr_puld01;
040100060428
040200120329       // controllo se 'immissione' 'modifica' 'copia' o altro
040300060428        select;
040400060428
040500060502       // F10=Immissione
040600121217         when b96comand = 'J';
040700060428          d1descopz = 'Immissione';
040800120402
040900120402          //imposto la filiale
041000121217          if b96ke1<>*blanks   ;
041100121217          d1tbeke1=b96ke1  ;
041200120402          else   ;
041300120402          d1tbeke1=%editc(dutpou:'X')  ;
041400120402          endif   ;
041500060428
041600060502       // Opzione "02"=modifica
041700121217         when b96opzio = '02';
041800060428          d1descopz = 'Modifica';
041900060428          exsr sr_imposta;
042000060428
042100060502       // Opzione "03"=copia
042200121217         when b96opzio = '03';
042300060428          d1descopz = 'Copia';
042400060428          exsr sr_imposta;
042500060428
042600060502       // Opzione "04"=cancellazione/ripristino
042700121217         when b96opzio = '04';
042800060428          exsr sr_imposta;
042900060502       // se richiesta 'cancellazione'
043000060502          if tbeatb = *blanks;
043100060502           d1descopz = 'Annullamento';
043200060502          endif;
043300060502       // se richiesto 'ripristino'
043400060502          if tbeatb = 'A';
043500060502           d1descopz = 'Ripristino';
043600060502          endif;
043700060502
043800060502       // Opzione "05"=visualizzazione
043900121217         when b96Opzio ='05';
044000060502          d1descopz = 'Visualizzazione';
044100060502          exsr sr_imposta;
044200060502
044300060502       // Fine scelta
044400060502        endsl;
044500060502
044600060502       // centro la descrizione della funzione nel formato video
044700060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
044800060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
044900060502
045000060502       endsr;
045100060502
045200060502       // ------------------------------------------------------------------------
045300060502       // controllo video
045400060502       // ------------------------------------------------------------------------
045500060502       begsr sr_ctrd01;
045600060502
045700060502        *in28 = *off;
045800060502        *in40 = *off;
045900060502        *in41 = *off;
046000010430
046100120329       // causale motivazione
046200121217        if d1tbeke2 <=*zeros ;
046300060502         vd1msg = msg(01);
046400060502         *in28 = *on;
046500060502         *in40 = *on;
046600060502         leavesr;
046700060502        endif;
046800060508
046900060508       // se immissione controllo che non esista già
047000121217        if b96comand = 'J' or b96opzio = '03';
047100121217         tbecod = 'KPV';
047200120329         tbeke1 = %editc(dutpou:'X') ;
047300121217         tbeke2 = %editc(d1tbeke2:'X') ;
047400060508         clear tbelin;
047500060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
047600060508         if %found(tntbe01l);
047700060508          vd1msg = msg(05);
047800060508          *in28 = *on;
047900060508          *in40 = *on;
048000060508          leavesr;
048100060508         endif;
048200060508        endif;
048300060502
048400060502       // descrizione
048500120329        if v1cdes   = *blanks;
048600060502         vd1msg = msg(02);
048700060502         *in28 = *on;
048800060502         *in41 = *on;
048900060502         leavesr;
049000060502        endif;
049100060502
049200060502       endsr;
049300060502       // ------------------------------------------------------------------------
049400060502       // aggiorno tabella
049500060502       // ------------------------------------------------------------------------
049600060502       begsr sr_aggiorna;
049700060502
049800060502       // imposto campi del file
049900060509        clear tbelin;
050000120329        tbeke1 = d1tbeke1;
050100120329        if tbeke1=*blanks ;
050200120329         tbeke1=%editc(dutpou:'X') ;
050300120329        endif  ;
050400121217        tbeke2 = %editc(d1tbeke2:'X');
050500121219        tbeuni=v1cdes ;
050600060502
050700060502        clear tbeftr;
050800060502        clear tbedtr;
050900120329        clear tbeftt;
051000120329        clear tbeflt;
051100060502
051200060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
051300060502        select;
051400060502
051500060502       // F10=immissione - Opzione "03"=copia
051600121217         when b96comand = 'J' or b96opzio = '03';
051700060502       // scrivo nuovo record con gestione errore per chiave duplicata
051800060508          write tntbe000;
051900060502
052000060502       // Opzione "02"=modifica
052100121217         when b96opzio = '02';
052200060502       // update record e controllo errore per chiave duplicata
052300060508          update tntbe000;
052400060502
052500060502       // Opzione "04"=cancellazione/ripristino
052600121217         when b96opzio = '04';
052700060502          update tntbe000;
052800010430
052900060502        endsl;
053000060502
053100060502       endsr;
053200060502
053300060502       // ------------------------------------------------------------------------
053400060502       // pulizia video
053500060502       // ------------------------------------------------------------------------
053600060502       begsr sr_puld01;
053700060502
053800120329        clear v1cdes   ;
053900120329        clear d1tbeke2 ;
054000060504
054100060504        *in28 = *off;
054200060504        *in40 = *off;
054300060504        *in41 = *off;
054400060502
054500060502       endsr;
054600060502
054700060502       // ------------------------------------------------------------------------
054800060502       // imposto i dati a video
054900060502       // ------------------------------------------------------------------------
055000060502       begsr sr_imposta;
055100060502
055200060502       // recupero i dati dal file
055300060508        clear tbelin;
055400120329        clear tbeke1;
055500120329        clear tbeke2;
055600120329        tbeke1=s1tbeke1 ;
055700121217        tbeke2=%editc(s1tbeke2:'X') ;
055800120329        chain (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
055900060502
056000060502       // imposto i campi a video
056100121217        d1tbeke2 = %int(%subst(tbeke2:1:7)) ;
056200060502        d1tbeke1 = tbeke1;
056300121219        v1cdes=tbeuni    ;
056400060502
056500060502       endsr;
056600060502
056700060502       // ------------------------------------------------------------------------
056800060502       // gestione del subfile
056900060502       // ------------------------------------------------------------------------
057000060502       begsr sr_gestsfl;
057100060502
057200060502       // set flag
057300060502        wrkeofs01 = *off;
057400060502
057500060502       // inizio lettura subfile
057600060502        dow wrkeofs01 = *off and *in21;
057700121217         readc tb96s01;
057800060502       // se fine subfile
057900060502         if %eof;
058000060502          wrkcars01 = *on;
058100060502          leave;
058200060502         endif;
058300060502       // se è stata immessa un'opzione
058400060502         if s1opzione <> *zeros;
058500060502       // reset ds di servizio
058600121217          clear tntb96ds;
058700060502       // controllo ed elaborazione opzione immessa
058800060502          select;
058900060502       // opzione "01"=scelta
059000060502           when s1opzione = 1 and *in01;
059100121217            b96opzio = '01';
059200060502       // opzione "02"=modifica
059300060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
059400121217            b96opzio = '02';
059500060502       // opzione "03"=copia
059600060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
059700121217            b96opzio = '03';
059800060502       // opzione "04"=annullo/ripristino
059900060502           when s1opzione = 4 and not *in01;
060000121217            b96opzio = '04';
060100060502       // opzione "05"=visualizzazione
060200060502           when s1opzione = 5;
060300121217            b96opzio = '05';
060400121217       // opzione "10"=gestine codici della tabella
060500121217           when s1opzione = 10 and not *in01 and s1tbeatb = *blanks;
060600121217            b96opzio = '10';
060700060502          endsl;
060800010503
060900060502       // se immessa almeno un opzione valida
061000121217          if  b96opzio <> *blanks;
061100121217
061200121217       // se opzione 10  richiamo il pgm tnsb97r
061300121217          if  b96opzio = '10'     ;
061400121217          clear tntb97ds  ;
061500121217          b97ke1= s1tbeke1  ;
061600121217          b97ke2= s1tbeke2  ;
061700121219          b97descr= s1tbeuni  ;
061800121217          eval kpjbu=tntb97ds  ;
061900121217
062000121217            TNTB97R(KPJbA);
062100121217          else  ;
062200121217
062300121217       // gstione del formato video
062400060504           exsr sr_d01;
062500121217           endif  ;
062600121217
062700060502       // se la gestione si è chiusa con ...
062800121217
062900060504           select;
063000060502       // F6=Conferma
063100121217            when b96ricez = 'F';
063200060504             clear s1opzione;
063300060504             wrkcars01 = *on;
063400060502       // F12=Ritorno
063500121217            when b96ricez = 'L';
063600060504             clear s1opzione;
063700060504             wrkeofs01 = *on;
063800060502       // altrimenti
063900060504            other;
064000060504             *in22 = *on;
064100060504           endsl;
064200060502
064300121217           update tb96s01;
064400060504           *in22 = *off;
064500060504       // fine opzione
064600060504          endif;
064700060502       // fine opzione
064800060502         endif;
064900060502
065000060502        enddo;
065100060502
065200060502       endsr;
065300060502
065400060502       // ------------------------------------------------------------------------
065500060502       // posizionamento sul file
065600060502       // ------------------------------------------------------------------------
065700060502       begsr sr_setlltbe01;
065800120329
065900060502        select;
066000120330       //
066100120402         when wrkkey = '1' and ktbeke1<>*blanks ;
066200120402          setll (tbecod:ktbeke1) tntbe01l;
066300120402
066400120402         when wrkkey = '1' and ktbeke1= *blanks ;
066500120402          setll (tbecod) tntbe01l;
066600120330       //
066700060502         when wrkkey = '2';
066800120402          setll (tbecod:ktbeke1:tbeke2) tntbe01l;
066900060502        endsl;
067000060502
067100060502       endsr;
067200060502
067300060502       // ------------------------------------------------------------------------
067400060502       // scrivo subfile
067500060502       // ------------------------------------------------------------------------
067600060502       begsr sr_wtrs01;
067700060502
067800121219       // elaboto solo quelli con  la pirma parte della tabella vuota
067900121219
068000060502       // se non raggiunto limite massimo di caricamento
068100060505        if s01nrr < 9999;
068200060502       // imposto campi di subfile
068300060502         exsr sr_sets01;
068400060502       // imposto numeratore righe e numero relativo di record
068500060505         s01nrr = s01nrr + 1;
068600060502       // scrivo subfile
068700121217         write tb96s01;
068800060502        endif;
068900121219
069000060502       endsr;
069100060502
069200060502       // ------------------------------------------------------------------------
069300060502       // imposto campi del subfile
069400060502       // ------------------------------------------------------------------------
069500060502       begsr sr_sets01;
069600060502
069700060502        s1tbeke1 = tbeke1;
069800121220        s1tbeke2 = %int(%subst(tbeke2:1:7));
069900121219        s1tbeuni = tbeuni     ;
070000060502        s1tbeatb = tbeatb;
070100121220
070200121220        // veriifico se ha record
070300121220        wke1=s1tbeke1  + %editc(s1tbeke2:'X')   ;
070400121220        *in05=*off    ;
070500121220        exec sql   select count(*) into :wnum from tntbe00f where tbecod='KPV'
070600121220                  and tbeatb=' '   and tbeke1=:wke1            ;
070700121220
070800121220        if wnum=0 ;
070900121220         *in05 = *on  ;
071000121220        endif  ;
071100060502
071200060502       endsr;
071300060428
071400060502       // ------------------------------------------------------------------------
071500060502       // routine iniziale
071600060502       // ------------------------------------------------------------------------
071700060428         begsr *inzsr;
071800060428
071900060428      /end-free
072000060428
072100060428     c     *entry        plist
072200060428     c                   parm                    kpjba
072300060428
072400060428      /free
072500060428         in(e) §azute;
072600060428         if not %error;
072700060428          in(e) §datiute;
072800060428         endif;
072900060502         if %error or rsut = *blanks;
073000060428          tibs34r(tibs34ds);
073100060428          in §azute;
073200060428          in §datiute;
073300060428         endif;
073400060428
073500060428         endsr;
073600060502
073700060502       // ------------------------------------------------------------------------
073800060502       // uscita dal programma
073900060502       // ------------------------------------------------------------------------
074000060502         begsr sr_uscita;
074100060502
074200121217          if b96esito = *blanks;
074300121217           b96esito = eseguito;
074400060502          endif;
074500060502
074600121217          kpjbu = tntb96ds;
074700060502
074800060502          *inlr = *on;
074900060502          return;
075000060502
075100060502         endsr;
075200060502
075300060502      /end-free
075400060502
075500060502** -MSG-                                                                     *
075600060502Immettere la causale                                                           01
075700060502Immettere la descrizione                                                       02
075800060510
075900060510
076000060502Causale già esistente                                                          05
