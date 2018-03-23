000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300071001      * Gestione tabella "FSC" = Fil. di arrivo con forzatura t.incasso "SC"   *
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600071002     fazorg01l  if   e           k disk
000700070928     ftntb60d   cf   e             workstn sfile(tb60s01:s01nrr)
000800060428
000900060428      *------------------------------------------------------------------------*
001000060428      * riepilogo indicatori
001100060428      *------------------------------------------------------------------------*
001200060508      * 03 - proteggo campi video 01
001300060508      * 20 - gestione subfile 01
001400060508      * 21 - gestione subfile 01
001500060508      * 22 - gestione subfile 01
001600060428      * 28 - messaggio errore
001700060502
001800060502      *------------------------------------------------------------------------*
001900060502      * schiere
002000060502      *------------------------------------------------------------------------*
002100060502     d msg             s             78    dim(17) ctdata perrcd(1)
002200060427
002300060428      *------------------------------------------------------------------------*
002400060428      * campi di work
002500060428      *------------------------------------------------------------------------*
002600071002     d blanks          s                   like(w1descopz)
002700060502     d comando         s              1
002800060505     d ktbeke1         s                   like(tbeke1)
002900060428     d len             s              5u 0
003000060505     d s01nrr          s              4  0
003100060505     d s02nrr          s              4  0
003200060428     d savopzione      s                   like(s1opzione)
003300060428     d savrcdnbr       s                   like(c1rcdnbr)
003400060505     d savrec          s                   like(s02nrr)
003500060531     d savtbeftt       s                   like(tbeftt)
003600060531     d savtbeflt       s                   like(tbeflt)
003700060505     d savtbeuni       s                   like(tbeuni)
003800060428     d wrkkey          s              1
003900060428     d wrkeofs01       s              1
004000071008     d*wrkcars01       s              1
004100060505     d wrkcarw01       s              1
004200060505     d wrksfl02        s              1
004300060504     d $video          s             10
004400071005     d d19cod1         s              3
004500071005     d d19tip          s              1
004600071005     d d19des1         s             25
004700060428
004800060428      *------------------------------------------------------------------------*
004900060428      * ds interne/esterne
005000060428      *------------------------------------------------------------------------*
005100060504     d wlbdat          ds                  inz
005200060504     d  g02dat                 1      8  0
005300060504     d  g02inv                 9     16  0
005400060504     d  g02err                17     17
005500060504     d  g02tgi                18     22  0
005600060428
005700071001     d dfsc          e ds
005800060502     d kpjba         e ds
005900071002     d tntb60ds        ds
006000071002     d  b60comand                     1
006100071002     d  b60opzio                      2
006200071002     d  b60ricez                      1
006300060503     d tibs34ds      e ds                  inz
006400060428     d §azute        e ds                  extname(azute00f)
006500060428     d                                     dtaara
006600060428     d §datiute      e ds                  extname(ddatiute)
006700060428     d                                     dtaara
006800010503
006900060428     d psds           sds
007000060428     d  pgmname          *proc
007100060428
007200060428      *------------------------------------------------------------------------*
007300060428      * pgm richiamati
007400060428      *------------------------------------------------------------------------*
007500060428     d tibs34r         pr                  extpgm('TIBS34R')
007600060502     d  tibs34ds                           likeds(tibs34ds)
007700071003
007800071003     d tnsd19r         pr                  extpgm('TNSD19R')
007900071005     d a                              3
008000071005     d b                              1
008100071005     d c                             25
008200060504
008300071005     d xsrda8          pr                  extpgm('XSRDA8')
008400060504     d  wlbdat                             likeds(wlbdat)
008500060428
008600060428      *------------------------------------------------------------------------*
008700060428      * costanti
008800060428      *------------------------------------------------------------------------*
008900060428     d errore          c                   '1'
009000060428     d eseguito        c                   '0'
009100010503
009200060428      *------------------------------------------------------------------------*
009300010423
009400060428      /free
009500060428
009600071120       exsr sr_psw;
009700060428       exsr sr_c01;
009800060428       exsr sr_uscita;
009900010430
010000071120       // ------------------------------------------------------------------------
010100071120       // gestione videata password
010200071120       // ------------------------------------------------------------------------
010300071120       begsr sr_psw;
010400071120        clear w1cpsw;
010500071120        clear widmsg;
010600071120        dou not *in28;
010700071120        exfmt tb60wps;
010800071120        clear widmsg;
010900071120        *in28='0';
011000071120         if *inkc;
011100071120          exsr sr_uscita;
011200071120         endif;
011300071120         select;
011400071120          when w1cpsw=*blanks;
011500071120           *in28='1';
011600071120           widmsg=msg(03);
011700071120          when w1cpsw<>'SEDE      ';
011800071120           *in28='1';
011900071120           widmsg=msg(04);
012000071120           clear w1cpsw;
012100071120         endsl;
012200071120        enddo;
012300071120
012400071120        endsr;
012500060502       // ------------------------------------------------------------------------
012600060502       // gestione subfile
012700060502       // ------------------------------------------------------------------------
012800060428       begsr sr_c01;
012900060428
013000060502       // imposto variabili
013100071008       //wrkcars01 = *on;
013200060428        $video = 'C01';
013300060428
013400060502       // inizio elaborazione subfile
013500060428         dou  $video <> 'C01';
013600060428
013700060502       // caricamento subfile
013800060428          exsr sr_cars01;
013900060428
014000060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
014100060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
014200060502       // l'ultimo tasto
014300060428          if c1csrrrn > 0;
014400060428           c1rcdnbr = c1csrrrn;
014500060428          else;
014600060428           c1rcdnbr = savrcdnbr;
014700060428          endif;
014800060428
014900060502       // se non so quale pagina visualizzare forzo pagina 1
015000060428          if c1rcdnbr < 1;
015100060428           eval c1rcdnbr = 1;
015200060428          endif;
015300060428
015400060502       // salvo il record number del subfile
015500060428          savrcdnbr = c1Rcdnbr;
015600060428
015700060502       // Emissione del subfile
015800071001          write tb60p01;
015900071001          exfmt tb60c01;
016000060428
016100060502       // controllo tasti funzionali del subfile
016200060428          select;
016300060428
016400060502       // F3=Fine
016500060428           when *inkc;
016600060502            $video = *blanks;
016700060428            exsr sr_uscita;
016800060428
016900060502       // F5=Refresh
017000071008       //  when *inke;
017100071008       //   wrkcars01 = *on;
017200060428
017300060502       // F10=Immissione
017400060428           when *inkj;
017500071001            $video = 'W01';
017600071003            clear tntb60ds;
017700071003            b60comand = 'J';
017800071001            exsr sr_W01;
017900060428
018000060502       // in tutti gli altri casi
018100060428           other;
018200060502       // controllo ed elaborazione scelte su subfile
018300060428            exsr sr_gestsfl;
018400060428          endsl;
018500060428
018600060502       // fine elaborazione 'C01'
018700060428         enddo;
018800060428
018900060428        endsr;
019000060428
019100060502       // ------------------------------------------------------------------------
019200060502       // caricamento subfile
019300060502       // ------------------------------------------------------------------------
019400060428       begsr sr_cars01;
019500010430
019600060502       // se è stato richiesto il caricamento del subfile
019700071008       //if wrkcars01 = *on;
019800060428
019900060502       // inizializzo il subfile
020000060505         s01nrr = 0;
020100060428         *in20 = *on;
020200071001         write tb60c01;
020300060428         *in20 = *off;
020400060428         *in21 = *off;
020500060428         *in22 = *off;
020600060428         *in23 = *off;
020700060428
020800060502       // imposto la chiave di posizionamento e lettura file
020900071001         tbecod = 'FSC';
021000060428         tbeke1 = c1setll;
021100060428
021200060502       // se è stato scelto il posizionamento
021300060502         if c1setll <> *blanks;
021400060428          wrkkey = '2';
021500060502       // altrimenti
021600060428         else;
021700060428          wrkkey = '1';
021800060428         endif;
021900060428
022000060502       // posizionamento file
022100060428         exsr sr_setlltbe01;
022200060428
022300060502       // fino a che non è fine file
022400060428         dou %eof(tntbe01l);
022500060428
022600060502       // leggo file
022700060505          reade(n) tbecod tntbe01l;
022800060428
022900060502       // fine file esco
023000060428          if %eof(tntbe01l);
023100060428           leave;
023200060428          endif;
023300060428
023400060502       // scrivo subfile
023500060428           clear s1opzione;
023600060428           exsr sr_wtrs01;
023700060428
023800060428         enddo;
023900060428
024000060502       // fine caricamento subfile
024100071008       // endif;
024200060428
024300060502       // se scritto almeno un record
024400060505        if s01nrr > 0;
024500060502       // indicatore di sflend
024600060428         eval *in21 = *on;
024700060428        endif;
024800060428
024900060502       // se non scritto neanche un record
025000060505        if s01nrr = 0;
025100060502       // indicatore di sfldsp
025200060428         eval *in23 = *on;
025300060428        endif;
025400060428
025500060502       // disattivo opzione di caricamento subfile
025600071008       //wrkcars01 = *off;
025700060428
025800060428       endsr;
025900060428
026000060502       // ------------------------------------------------------------------------
026100071001       // gestione window immissione lna
026200060502       // ------------------------------------------------------------------------
026300071001       begsr sr_W01;
026400060428
026500060504       // imposto il video a seconda della funzione richiesta
026600060428        exsr sr_setvideo;
026700060428
026800060502       // imposto variabile
026900071001        $video = 'W01';
027000060428
027100071001       // fino a che la variabile resta settata come 'W01'
027200071001        dou $video <> 'W01';
027300060502       // emetto il video
027400071001          exfmt tb60w01;
027500060428         *in28 = *off;
027600071001         clear widmsg;
027700010430
027800060502       // controllo tasti funzionali del video
027900060428         select;
028000060428
028100071120       // F12=Ritorno
028200071120          when *inkl;
028300071120           clear s1opzione;
028400071120           b60ricez = 'L';
028500071120           unlock tntbe01l;
028600071120           $video = 'C01';
028700071120
028800060502       // F6=Conferma
028900060428          when *inkf;
029000071002          // controllo e decodifico i dati del video
029100071001           exsr sr_ctrW01;
029200071120           if not *in28;
029300071120            exsr sr_aggiorna;
029400071120            $video = 'C01';
029500071120           endif;
029600060428
029700060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
029800071001       //  if not *in28;
029900071001       //   wrkcarw01 = *on;
030000071001       //   $video = 'W01';
030100071001       //   exsr sr_w01;
030200071001       //  endif;
030300060428
030400060428
030500060502       // Invio
030600060428          other;
030700071001            exsr sr_ctrW01;
030800060428
030900060502         endsl;
031000060428
031100071001       // fine gestione 'W01'
031200060428        enddo;
031300060428
031400060428       endsr;
031500060428
031600060502       // ------------------------------------------------------------------------
031700060502       // imposto i dati a video
031800060502       // ------------------------------------------------------------------------
031900060428       begsr sr_setvideo;
032000060428
032100071001       // pulisco il formato video W01
032200071001        exsr sr_pulW01;
032300060428
032400071001       // controllo se 'immissione' o 'cancellazione/ripristino'
032500060428        select;
032600060428
032700060502       // F10=Immissione
032800071002         when b60comand = 'J';
032900071001          w1descopz = 'Immissione';
033000060428
033100060502       // Opzione "04"=cancellazione/ripristino
033200071002         when b60opzio = '04';
033300071120          *in03=*on;
033400060428          exsr sr_imposta;
033500060502       // se richiesta 'cancellazione'
033600060502          if tbeatb = *blanks;
033700071001           w1descopz = 'Annullamento';
033800060502          endif;
033900060502       // se richiesto 'ripristino'
034000060502          if tbeatb = 'A';
034100071001           w1descopz = 'Ripristino';
034200060502          endif;
034300060502
034400060502       // Fine scelta
034500060502        endsl;
034600060502
034700060502       // centro la descrizione della funzione nel formato video
034800071001        len = (%len(w1descopz) - %len(%trimr(w1descopz))) / 2;
034900071001        w1descopz = %subst(blanks:1:len) + %trimr(w1descopz);
035000060502
035100060502       endsr;
035200060502
035300060502       // ------------------------------------------------------------------------
035400060502       // controllo video
035500060502       // ------------------------------------------------------------------------
035600071001       begsr sr_ctrw01;
035700060502
035800060502        *in28 = *off;
035900071001        *in46 = *off;
036000010430
036100071001       // fililale di arrivo
036200071002        if w1clna = *blanks or w1clna=*zeros;
036300071001         widmsg = msg(01);
036400060502         *in28 = *on;
036500071001         *in46 = *on;
036600060502         leavesr;
036700060502        endif;
036800060508
036900060508       // se immissione controllo che non esista già
037000071002        if b60comand = 'J';
037100071003         if %scan('?':w1clna)>0;
037200071003         d19cod1=*blanks;
037300071003         tnsd19r(d19cod1:d19tip:d19des1);
037400071003         w1clna=d19cod1;
037500071003         w1dlna=d19des1;
037600071005         write tb60p01;
037700071005         write tb60c01;
037800071008          leavesr;
037900071003         endif;
038000071003
038100071001         tbecod = 'FSC';
038200071001         tbeke1 = w1clna;
038300071001         tbeke2 = *blanks;
038400060508         clear tbelin;
038500060508         chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
038600060508         if %found(tntbe01l);
038700071001          widmsg = msg(05);
038800060508          *in28 = *on;
038900071001          *in46 = *on;
039000060508          leavesr;
039100060508         endif;
039200071120        endif;
039300071120       // se immissione o ripristino
039400071120        if b60comand = 'J' or tbeatb='A';
039500071002       // deve anche esistere in organigramma
039600071003         orgfil=%dec(w1clna:3:0);
039700071003         chain (orgfil) azorg01l;
039800071003         if not %found(azorg01l)
039900071003         or (orgfag<>'F' and orgfag<>'A')
040000071003         or (orgfva<>' ');
040100071002          widmsg = msg(02);
040200071002          *in28 = *on;
040300071002          *in46 = *on;
040400071002          leavesr;
040500071002         endif;
040600071008         w1dlna=orgdes;
040700071120        endif;
040800060502
040900060502       endsr;
041000060504
041100060502       // ------------------------------------------------------------------------
041200060502       // aggiorno tabella
041300060502       // ------------------------------------------------------------------------
041400060502       begsr sr_aggiorna;
041500060502
041600071019        clear tbeftr;
041700071019
041800060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
041900060502        select;
042000060502
042100071002       // F10=immissione
042200071002         when b60comand = 'J';
042300060502       // scrivo nuovo record con gestione errore per chiave duplicata
042400071019       // imposto campi del file
042500071019        clear tntbe000;
042600071019        clear dfsc;
042700071019        tbecod='FSC';
042800071019        tbeke1 = w1clna;
042900071019        tbeapl = 'GE';
043000071019        §fscdes  = w1dlna;
043100071019        tbeuni = dfsc;
043200071019        tbeftt = 'S';
043300060508          write tntbe000;
043400060502
043500060502       // Opzione "04"=cancellazione/ripristino
043600071002         when b60opzio = '04';
043700071019            select;
043800071019          // annullamento
043900071019             when tbeatb = *blanks;
044000071019              tbeatb = 'A';
044100071019          // ripristino
044200071019             when tbeatb = 'A';
044300071019              clear tbeatb;
044400071019            endsl;
044500060502          update tntbe000;
044600010430
044700060502        endsl;
044800060502
044900060502       endsr;
045000060502
045100060502       // ------------------------------------------------------------------------
045200060502       // pulizia video
045300060502       // ------------------------------------------------------------------------
045400071002       begsr sr_pulw01;
045500060502
045600071002        clear w1clna;
045700071002        clear w1dlna;
045800060504
045900060504        *in28 = *off;
046000071002        *in46 = *off;
046100071120        *in03 = *off;
046200060502
046300060502       endsr;
046400060502
046500060502       // ------------------------------------------------------------------------
046600060502       // imposto i dati a video
046700060502       // ------------------------------------------------------------------------
046800060502       begsr sr_imposta;
046900060502
047000060502       // recupero i dati dal file
047100060505        ktbeke1 = s1tbeke1;
047200060508        clear tbelin;
047300060508        chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
047400060502
047500060502       // imposto i campi a video
047600071002        w1clna = tbeke1;
047700071002        dfsc = tbeuni;
047800071002        w1dlna = §fscdes;
047900071002
048000060502       endsr;
048100060502
048200060502       // ------------------------------------------------------------------------
048300060502       // gestione del subfile
048400060502       // ------------------------------------------------------------------------
048500060502       begsr sr_gestsfl;
048600060502
048700060502       // set flag
048800060502        wrkeofs01 = *off;
048900060502
049000060502       // inizio lettura subfile
049100060502        dow wrkeofs01 = *off and *in21;
049200071002         readc tb60s01;
049300060502       // se fine subfile
049400071008         if %eof;
049500071008       // wrkcars01 = *on;
049600071008          leave;
049700071008         endif;
049800060502       // se è stata immessa un'opzione
049900060502         if s1opzione <> *zeros;
050000060502       // reset ds di servizio
050100071002          clear tntb60ds;
050200060502       // controllo ed elaborazione opzione immessa
050300060502          select;
050400060502       // opzione "04"=annullo/ripristino
050500071002           when s1opzione = 4;
050600071002            b60opzio = '04';
050700060502          endsl;
050800010503
050900060502       // se immessa almeno un opzione valida
051000071002          if  b60opzio <> *blanks;
051100060502       // gstione del formato video
051200071002           exsr sr_W01;
051300060502       // se la gestione si è chiusa con ...
051400060504           select;
051500060502       // F6=Conferma
051600071002            when b60ricez = 'F';
051700060504             clear s1opzione;
051800071008       //    wrkcars01 = *on;
051900060502       // F12=Ritorno
052000071002            when b60ricez = 'L';
052100060504             clear s1opzione;
052200060504             wrkeofs01 = *on;
052300060502       // altrimenti
052400060504            other;
052500060504             *in22 = *on;
052600060504           endsl;
052700060502
052800071002           update tb60s01;
052900060504           *in22 = *off;
053000060504       // fine opzione
053100060504          endif;
053200060502       // fine opzione
053300060502         endif;
053400060502
053500060502        enddo;
053600060502
053700060502       endsr;
053800060502
053900060502       // ------------------------------------------------------------------------
054000060502       // posizionamento sul file
054100060502       // ------------------------------------------------------------------------
054200060502       begsr sr_setlltbe01;
054300060502
054400060502        select;
054500060502       // chiave totale
054600060502         when wrkkey = '1';
054700060502          setll tbecod tntbe01l;
054800060502       // chiave parziale
054900060502         when wrkkey = '2';
055000060502          setll (tbecod:tbeke1) tntbe01l;
055100060502        endsl;
055200060502
055300060502       endsr;
055400060502
055500060502       // ------------------------------------------------------------------------
055600060502       // scrivo subfile
055700060502       // ------------------------------------------------------------------------
055800060502       begsr sr_wtrs01;
055900060502
056000060502       // se non raggiunto limite massimo di caricamento
056100060505        if s01nrr < 9999;
056200060502       // imposto campi di subfile
056300060502         exsr sr_sets01;
056400060502       // imposto numeratore righe e numero relativo di record
056500060505         s01nrr = s01nrr + 1;
056600060502       // scrivo subfile
056700071001         write tb60s01;
056800060502        endif;
056900060502
057000060502       endsr;
057100060502
057200060502       // ------------------------------------------------------------------------
057300060502       // imposto campi del subfile
057400060502       // ------------------------------------------------------------------------
057500060502       begsr sr_sets01;
057600060502
057700060502        s1tbeke1 = tbeke1;
057800060505        s1tbeke2 = tbeke2;
057900071001        dfsc = tbeuni;
058000071001        s1tbeuni = §fscdes;
058100060502        s1tbeatb = tbeatb;
058200060502
058300060502       endsr;
058400060428
058500060502       // ------------------------------------------------------------------------
058600060502       // routine iniziale
058700060502       // ------------------------------------------------------------------------
058800060428         begsr *inzsr;
058900060428
059000060428      /end-free
059100060428
059200060428     c     *entry        plist
059300060428     c                   parm                    kpjba
059400060428
059500060428      /free
059600060428         in(e) §azute;
059700060428         if not %error;
059800060428          in(e) §datiute;
059900060428         endif;
060000060502         if %error or rsut = *blanks;
060100060428          tibs34r(tibs34ds);
060200060428          in §azute;
060300060428          in §datiute;
060400060428         endif;
060500060428
060600060428         endsr;
060700060502
060800060502       // ------------------------------------------------------------------------
060900060502       // uscita dal programma
061000060502       // ------------------------------------------------------------------------
061100060502         begsr sr_uscita;
061200060502
061300060502
061400060502          *inlr = *on;
061500060502          return;
061600060502
061700060502         endsr;
061800060502
061900060502      /end-free
062000060502
062100060502** -MSG-                                                                     *
062200071002Immettere la filiale di arrivo                                                 01
062300071002Filiale errata                                                                 02
062400071120Immettere la password
062500071120Password errata
062600071002Filiale già presente                                                           05
