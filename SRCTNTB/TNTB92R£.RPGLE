000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120329      * motivazioni errata ricezione DDT
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600120329     ftntb92d   cf   e             workstn sfile(tb92s01:s01nrr)
000700060428
000800060428      *------------------------------------------------------------------------*
000900060428      * riepilogo indicatori
001000060428      *------------------------------------------------------------------------*
001100060428      * 01 - ON ricerca -- OFF manutenzione
001200060508      * 02 - proteggo campo causale
001300060508      * 03 - proteggo campi video 01
001400060509      * 04 - visualizzazione
001500060508      * 05 - visualizzo subfile lingue
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
005300060428
005400060428      *------------------------------------------------------------------------*
005500060428      * ds interne/esterne
005600060428      *------------------------------------------------------------------------*
005700060504     d wlbdat          ds                  inz
005800060504     d  g02dat                 1      8  0
005900060504     d  g02inv                 9     16  0
006000060504     d  g02err                17     17
006100060504     d  g02tgi                18     22  0
006200060428
006300060502     d kpjba         e ds
006400060503     d tibs34ds      e ds                  inz
006500120329     d tntb92ds      e ds
006600060428     d §azute        e ds                  extname(azute00f)
006700060428     d                                     dtaara
006800060428     d §datiute      e ds                  extname(ddatiute)
006900060428     d                                     dtaara
007000010503
007100060428     d psds           sds
007200060428     d  pgmname          *proc
007300060428
007400060428      *------------------------------------------------------------------------*
007500060428      * pgm richiamati
007600060428      *------------------------------------------------------------------------*
007700060428     d tibs34r         pr                  extpgm('TIBS34R')
007800060502     d  tibs34ds                           likeds(tibs34ds)
007900060504
008000060504     d xsrda8          pr                  extpgm('XSRDA8')
008100060504     d  wlbdat                             likeds(wlbdat)
008200060428
008300060428      *------------------------------------------------------------------------*
008400060428      * costanti
008500060428      *------------------------------------------------------------------------*
008600060428     d errore          c                   '1'
008700060428     d eseguito        c                   '0'
008800010503
008900060428      *------------------------------------------------------------------------*
009000010423
009100060428      /free
009200060428
009300060428       exsr sr_parm;
009400060428       exsr sr_c01;
009500060428       exsr sr_uscita;
009600010423
009700060502       // ------------------------------------------------------------------------
009800060502       // elaborazione parametri ricevuti
009900060502       // ------------------------------------------------------------------------
010000060428       begsr sr_parm;
010100060428
010200120329        tntb92dS = kpjbu;
010300060428
010400060428        select;
010500120402       // ricerca e scelta
010600120329         when B92Fun = '1';
010700060428          *in01 = *on;
010800120402          ktbeke1=b92ke1  ;
010900120402
011000060502       // manutenzione
011100120329         when  B92Fun = *blanks;
011200060428          *in01 = *off;
011300120402          ktbeke1=b92ke1  ;
011400120402
011500060502       // altrimenti
011600060428         other;
011700120329          B92Esito = errore;
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
015600120329          write tB92p01;
015700120329          exfmt tB92c01;
015800060428
015900060502       // controllo tasti funzionali del subfile
016000060428          select;
016100060428
016200060502       // F3=Fine
016300060428           when *inkc;
016400060502            $video = *blanks;
016500120329            B92ricez = 'C';
016600060428            exsr sr_uscita;
016700060428
016800060502       // F5=Refresh
016900060428           when *inke;
017000060428            wrkcars01 = *on;
017100060428
017200060502       // F10=Immissione
017300060428           when *inkj;
017400060502            $video = 'D01';
017500120329            clear tntB92ds;
017600120329            B92comand = 'J';
017700060428            exsr sr_d01;
017800060428
017900060502       // in tutti gli altri casi
018000060428           other;
018100060502       // controllo ed elaborazione scelte su subfile
018200060428            exsr sr_gestsfl;
018300060428          endsl;
018400060428
018500060502       // fine elaborazione 'C01'
018600060428         enddo;
018700060428
018800060428        endsr;
018900060428
019000060502       // ------------------------------------------------------------------------
019100060502       // caricamento subfile
019200060502       // ------------------------------------------------------------------------
019300060428       begsr sr_cars01;
019400010430
019500060502       // se è stato richiesto il caricamento del subfile
019600060428        if wrkcars01 = *on;
019700060428
019800060502       // inizializzo il subfile
019900060505         s01nrr = 0;
020000060428         *in20 = *on;
020100120329         write tB92c01;
020200060428         *in20 = *off;
020300060428         *in21 = *off;
020400060428         *in22 = *off;
020500060428         *in23 = *off;
020600060428
020700060502       // imposto la chiave di posizionamento e lettura file
020800120329         tbecod = 'MDE';
020900120329         tbeke2 = c1setll;
021000060428
021100060502       // se è stato scelto il posizionamento
021200060502         if c1setll <> *blanks;
021300060428          wrkkey = '2';
021400120402          if ktbeke1=*blanks    ;
021500120402           ktbeke1=%editc(dutpou:'X')  ;
021600120402          endif  ;
021700120402
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
023000120330         select  ;
023100120402         when wrkkey = '1'and ktbeke1=*blanks ;
023200120330          reade(n) (tbecod) tntbe01l;
023300120330
023400120402         when wrkkey = '1'and ktbeke1<>*blanks ;
023500120402          reade(n) (tbecod:ktbeke1) tntbe01l;
023600120402
023700120330         when wrkkey = '2';
023800120402          reade(n) (tbecod:ktbeke1:tbeke2) tntbe01l;
023900120330        endsl;
024000060428
024100060502       // fine file esco
024200060428          if %eof(tntbe01l);
024300060428           leave;
024400060428          endif;
024500060428
024600060502       // se in "ricerca/scelta" non carico i records annullati
024700060502          if (*in01 and tbeatb = *blanks) or not *in01;
024800060502       // scrivo subfile
024900060428           clear s1opzione;
025000060428           exsr sr_wtrs01;
025100060428          endif;
025200060428
025300060428         enddo;
025400060428
025500060502       // fine caricamento subfile
025600060428        endif;
025700060428
025800060502       // se scritto almeno un record
025900060505        if s01nrr > 0;
026000060502       // indicatore di sflend
026100060428         eval *in21 = *on;
026200060428        endif;
026300060428
026400060502       // se non scritto neanche un record
026500060505        if s01nrr = 0;
026600060502       // indicatore di sfldsp
026700060428         eval *in23 = *on;
026800060428        endif;
026900060428
027000060502       // disattivo opzione di caricamento subfile
027100060428        wrkcars01 = *off;
027200060428
027300060428       endsr;
027400060428
027500060502       // ------------------------------------------------------------------------
027600060502       // gestione videata
027700060502       // ------------------------------------------------------------------------
027800060428       begsr sr_d01;
027900060428
028000060504       // imposto il video a seconda della funzione richiesta
028100060428        exsr sr_setvideo;
028200060428
028300060502       // imposto variabile
028400060428        $video = 'D01';
028500060428
028600060502       // fino a che la variabile resta settata come 'D01'
028700060428        dou $video <> 'D01';
028800060428
028900060508         *in02 = *off;
029000060508         *in03 = *off;
029100060509         *in04 = *off;
029200060428
029300060502       // se immessa opzione di 'scelta'
029400120329         if B92opzio = '01';
029500120329          B92ke1 = s1tbeke1;
029600120329          B92ke2 = s1tbeke2;
029700120329          B92des = s1tbeuni;
029800060428          exsr sr_uscita;
029900060428
030000060508       // se immessa un'altra opzione
030100060508         else;
030200060508       // se non è immissione/copia proteggo il campo della causale
030300120329          if B92comand <> 'J' and B92opzio <> '03';
030400060428           *in02 = *on;
030500060428          endif;
030600060509       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
030700060508       // proteggo i campi del video
030800120329          if B92opzio = '04' or B92opzio = '05';
030900060508           *in03 = *on;
031000060508          endif;
031100060509       // se immessa opzione di 'visualizzazione'
031200060509       // non attivo F6
031300120329          if B92opzio = '05';
031400060509           *in04 = *on;
031500060509          endif;
031600060502       // emetto il video
031700120329          exfmt tB92d01;
031800060428         endif;
031900060428
032000060502       // reset indicatore generico di errore
032100060428         *in28 = *off;
032200060508
032300060508       // pulisco il campo messaggi
032400060508         clear vd1msg;
032500010430
032600060502       // controllo tasti funzionali del video
032700060428         select;
032800060428
032900060502       // F3=Fine
033000060428          when *inkc;
033100120329           B92ricez = 'C';
033200060502           $video = *blanks;
033300060505           unlock tntbe01l;
033400060428           exsr sr_uscita;
033500060428
033600060502       // F6=Conferma
033700060428          when *inkf;
033800120329           B92ricez = 'F';
033900060502       // controllo e decodifico i dati del video
034000060428           exsr sr_ctrd01;
034100060502       // conferma per annullo/ripristino
034200120329           if B92opzio = '04';
034300060428            select;
034400060502       // annullamento
034500060502             when tbeatb = *blanks;
034600060428              tbeatb = 'A';
034700060502       // ripristino
034800060502             when tbeatb = 'A';
034900060428              clear tbeatb;
035000060428            endsl;
035100060428           endif;
035200120329
035300120329            exsr sr_aggiorna;
035400120329
035500120329              $video = 'C01';
035600120329            wrkcars01 = *on;
035700060428
035800060502       // F12=Ritorno
035900060428          when *inkl;
036000060428           clear s1opzione;
036100120329           B92ricez = 'L';
036200060505           unlock tntbe01l;
036300060508       // se non è f12 da immissione/copia non ricarico il subfile
036400120329           if B92comand = 'J' or B92opzio = '03';
036500060428            wrkcars01 = *on;
036600060428           else;
036700060428            wrkcars01 = *off;
036800060428           endif;
036900060428           $video = 'C01';
037000060428
037100060502       // Invio
037200060428          other;
037300060508           if not *in03;
037400060508            exsr sr_ctrd01;
037500060508           endif;
037600060428
037700060502         endsl;
037800060428
037900060502       // fine gestione 'D01'
038000060428        enddo;
038100060428
038200060428       endsr;
038300060502
038400060502       // ------------------------------------------------------------------------
038500060502       // imposto i dati a video
038600060502       // ------------------------------------------------------------------------
038700060428       begsr sr_setvideo;
038800060428
038900060502       // pulisco il formato video D01
039000060502        exsr sr_puld01;
039100060428
039200120329       // controllo se 'immissione' 'modifica' 'copia' o altro
039300060428        select;
039400060428
039500060502       // F10=Immissione
039600120329         when B92comand = 'J';
039700060428          d1descopz = 'Immissione';
039800120402
039900120402          //imposto la filiale
040000120402          if b92ke1<>*blanks   ;
040100120402          d1tbeke1=b92ke1  ;
040200120402          else   ;
040300120402          d1tbeke1=%editc(dutpou:'X')  ;
040400120402          endif   ;
040500060428
040600060502       // Opzione "02"=modifica
040700120329         when B92opzio = '02';
040800060428          d1descopz = 'Modifica';
040900060428          exsr sr_imposta;
041000060428
041100060502       // Opzione "03"=copia
041200120329         when B92opzio = '03';
041300060428          d1descopz = 'Copia';
041400060428          exsr sr_imposta;
041500060428
041600060502       // Opzione "04"=cancellazione/ripristino
041700120329         when B92opzio = '04';
041800060428          exsr sr_imposta;
041900060502       // se richiesta 'cancellazione'
042000060502          if tbeatb = *blanks;
042100060502           d1descopz = 'Annullamento';
042200060502          endif;
042300060502       // se richiesto 'ripristino'
042400060502          if tbeatb = 'A';
042500060502           d1descopz = 'Ripristino';
042600060502          endif;
042700060502
042800060502       // Opzione "05"=visualizzazione
042900120329         when B92Opzio ='05';
043000060502          d1descopz = 'Visualizzazione';
043100060502          exsr sr_imposta;
043200060502
043300060502       // Fine scelta
043400060502        endsl;
043500060502
043600060502       // centro la descrizione della funzione nel formato video
043700060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
043800060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
043900060502
044000060502       endsr;
044100060502
044200060502       // ------------------------------------------------------------------------
044300060502       // controllo video
044400060502       // ------------------------------------------------------------------------
044500060502       begsr sr_ctrd01;
044600060502
044700060502        *in28 = *off;
044800060502        *in40 = *off;
044900060502        *in41 = *off;
045000010430
045100120329       // causale motivazione
045200120329        if d1tbeke2 = *blanks;
045300060502         vd1msg = msg(01);
045400060502         *in28 = *on;
045500060502         *in40 = *on;
045600060502         leavesr;
045700060502        endif;
045800060508
045900060508       // se immissione controllo che non esista già
046000120329        if B92comand = 'J' or B92opzio = '03';
046100120329         tbecod = 'MDE';
046200120329         tbeke1 = %editc(dutpou:'X') ;
046300060508         tbeke2 = d1tbeke2;
046400060508         clear tbelin;
046500060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
046600060508         if %found(tntbe01l);
046700060508          vd1msg = msg(05);
046800060508          *in28 = *on;
046900060508          *in40 = *on;
047000060508          leavesr;
047100060508         endif;
047200060508        endif;
047300060502
047400060502       // descrizione
047500120329        if v1cdes   = *blanks;
047600060502         vd1msg = msg(02);
047700060502         *in28 = *on;
047800060502         *in41 = *on;
047900060502         leavesr;
048000060502        endif;
048100060502
048200060502       endsr;
048300060502       // ------------------------------------------------------------------------
048400060502       // aggiorno tabella
048500060502       // ------------------------------------------------------------------------
048600060502       begsr sr_aggiorna;
048700060502
048800060502       // imposto campi del file
048900060509        clear tbelin;
049000120329        tbeke1 = d1tbeke1;
049100120329        if tbeke1=*blanks ;
049200120329         tbeke1=%editc(dutpou:'X') ;
049300120329        endif  ;
049400060509        tbeke2 = d1tbeke2;
049500120329        tbeuni=v1cdes ;
049600060502
049700060502        clear tbeftr;
049800060502        clear tbedtr;
049900120329        clear tbeftt;
050000120329        clear tbeflt;
050100060502
050200060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
050300060502        select;
050400060502
050500060502       // F10=immissione - Opzione "03"=copia
050600120329         when B92comand = 'J' or B92opzio = '03';
050700060502       // scrivo nuovo record con gestione errore per chiave duplicata
050800060508          write tntbe000;
050900060502
051000060502       // Opzione "02"=modifica
051100120329         when B92opzio = '02';
051200060502       // update record e controllo errore per chiave duplicata
051300060508          update tntbe000;
051400060502
051500060502       // Opzione "04"=cancellazione/ripristino
051600120329         when B92opzio = '04';
051700060502          update tntbe000;
051800010430
051900060502        endsl;
052000060502
052100060502       endsr;
052200060502
052300060502       // ------------------------------------------------------------------------
052400060502       // pulizia video
052500060502       // ------------------------------------------------------------------------
052600060502       begsr sr_puld01;
052700060502
052800120329        clear v1cdes   ;
052900120329        clear d1tbeke2 ;
053000060504
053100060504        *in28 = *off;
053200060504        *in40 = *off;
053300060504        *in41 = *off;
053400060502
053500060502       endsr;
053600060502
053700060502       // ------------------------------------------------------------------------
053800060502       // imposto i dati a video
053900060502       // ------------------------------------------------------------------------
054000060502       begsr sr_imposta;
054100060502
054200060502       // recupero i dati dal file
054300060508        clear tbelin;
054400120329        clear tbeke1;
054500120329        clear tbeke2;
054600120329        tbeke1=s1tbeke1 ;
054700120329        tbeke2=s1tbeke2 ;
054800120329        chain (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
054900060502
055000060502       // imposto i campi a video
055100060505        d1tbeke2 = tbeke2;
055200060502        d1tbeke1 = tbeke1;
055300120329        v1cdes=tbeuni    ;
055400060502
055500060502       endsr;
055600060502
055700060502       // ------------------------------------------------------------------------
055800060502       // gestione del subfile
055900060502       // ------------------------------------------------------------------------
056000060502       begsr sr_gestsfl;
056100060502
056200060502       // set flag
056300060502        wrkeofs01 = *off;
056400060502
056500060502       // inizio lettura subfile
056600060502        dow wrkeofs01 = *off and *in21;
056700120329         readc tB92s01;
056800060502       // se fine subfile
056900060502         if %eof;
057000060502          wrkcars01 = *on;
057100060502          leave;
057200060502         endif;
057300060502       // se è stata immessa un'opzione
057400060502         if s1opzione <> *zeros;
057500060502       // reset ds di servizio
057600120329          clear tntB92ds;
057700060502       // controllo ed elaborazione opzione immessa
057800060502          select;
057900060502       // opzione "01"=scelta
058000060502           when s1opzione = 1 and *in01;
058100120329            B92opzio = '01';
058200060502       // opzione "02"=modifica
058300060502           when s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
058400120329            B92opzio = '02';
058500060502       // opzione "03"=copia
058600060502           when s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
058700120329            B92opzio = '03';
058800060502       // opzione "04"=annullo/ripristino
058900060502           when s1opzione = 4 and not *in01;
059000120329            B92opzio = '04';
059100060502       // opzione "05"=visualizzazione
059200060502           when s1opzione = 5;
059300120329            B92opzio = '05';
059400060502          endsl;
059500010503
059600060502       // se immessa almeno un opzione valida
059700120329          if  B92opzio <> *blanks;
059800060502       // gstione del formato video
059900060504           exsr sr_d01;
060000060502       // se la gestione si è chiusa con ...
060100060504           select;
060200060502       // F6=Conferma
060300120329            when B92ricez = 'F';
060400060504             clear s1opzione;
060500060504             wrkcars01 = *on;
060600060502       // F12=Ritorno
060700120329            when B92ricez = 'L';
060800060504             clear s1opzione;
060900060504             wrkeofs01 = *on;
061000060502       // altrimenti
061100060504            other;
061200060504             *in22 = *on;
061300060504           endsl;
061400060502
061500120329           update tB92s01;
061600060504           *in22 = *off;
061700060504       // fine opzione
061800060504          endif;
061900060502       // fine opzione
062000060502         endif;
062100060502
062200060502        enddo;
062300060502
062400060502       endsr;
062500060502
062600060502       // ------------------------------------------------------------------------
062700060502       // posizionamento sul file
062800060502       // ------------------------------------------------------------------------
062900060502       begsr sr_setlltbe01;
063000120329
063100060502        select;
063200120330       //
063300120402         when wrkkey = '1' and ktbeke1<>*blanks ;
063400120402          setll (tbecod:ktbeke1) tntbe01l;
063500120402
063600120402         when wrkkey = '1' and ktbeke1= *blanks ;
063700120402          setll (tbecod) tntbe01l;
063800120330       //
063900060502         when wrkkey = '2';
064000120402          setll (tbecod:ktbeke1:tbeke2) tntbe01l;
064100060502        endsl;
064200060502
064300060502       endsr;
064400060502
064500060502       // ------------------------------------------------------------------------
064600060502       // scrivo subfile
064700060502       // ------------------------------------------------------------------------
064800060502       begsr sr_wtrs01;
064900060502
065000060502       // se non raggiunto limite massimo di caricamento
065100060505        if s01nrr < 9999;
065200060502       // imposto campi di subfile
065300060502         exsr sr_sets01;
065400060502       // imposto numeratore righe e numero relativo di record
065500060505         s01nrr = s01nrr + 1;
065600060502       // scrivo subfile
065700120329         write tB92s01;
065800060502        endif;
065900060502
066000060502       endsr;
066100060502
066200060502       // ------------------------------------------------------------------------
066300060502       // imposto campi del subfile
066400060502       // ------------------------------------------------------------------------
066500060502       begsr sr_sets01;
066600060502
066700060502        s1tbeke1 = tbeke1;
066800060505        s1tbeke2 = tbeke2;
066900120329        s1tbeuni = tbeuni ;
067000060502        s1tbeatb = tbeatb;
067100060502
067200060502       endsr;
067300060428
067400060502       // ------------------------------------------------------------------------
067500060502       // routine iniziale
067600060502       // ------------------------------------------------------------------------
067700060428         begsr *inzsr;
067800060428
067900060428      /end-free
068000060428
068100060428     c     *entry        plist
068200060428     c                   parm                    kpjba
068300060428
068400060428      /free
068500060428         in(e) §azute;
068600060428         if not %error;
068700060428          in(e) §datiute;
068800060428         endif;
068900060502         if %error or rsut = *blanks;
069000060428          tibs34r(tibs34ds);
069100060428          in §azute;
069200060428          in §datiute;
069300060428         endif;
069400060428
069500060428         endsr;
069600060502
069700060502       // ------------------------------------------------------------------------
069800060502       // uscita dal programma
069900060502       // ------------------------------------------------------------------------
070000060502         begsr sr_uscita;
070100060502
070200120329          if B92esito = *blanks;
070300120329           B92esito = eseguito;
070400060502          endif;
070500060502
070600120329          kpjbu = tntB92ds;
070700060502
070800060502          *inlr = *on;
070900060502          return;
071000060502
071100060502         endsr;
071200060502
071300060502      /end-free
071400060502
071500060502** -MSG-                                                                     *
071600060502Immettere la causale                                                           01
071700060502Immettere la descrizione                                                       02
071800060510
071900060510
072000060502Causale già esistente                                                          05
