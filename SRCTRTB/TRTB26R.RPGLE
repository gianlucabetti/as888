000100060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060418
000300060418      *--------------------------------------------------------------*
000400060706      * GESTIONE TABELLA DIVISE                                      *
000500060418      *--------------------------------------------------------------*
000600060418
000700060418     ftabel00f  uf a e           k disk
000800060426     fazlin01l  if   e           k disk
000900060706     ftrtb26d   cf   e             workstn sfile(tb26s01:s01nrr)
001000060706     f                                     sfile(tb26s02:s02nrr)
001100060418      *---------------------------------------------------------------*
001200060510      * riepilogo indicatori
001300060418      *---------------------------------------------------------------*
001400060522      * 01 - protegge campi video 02
001500060510      * 02 - variazione
001600060522      * 03 - non attiva F6 video 02
001700060524      * 04 - rcd annullato
001800060510      * 05 - visualizzo subfile lingue
001900060510      * 10 - ON ricerca/scelta -- OFF gestione
002000060510      * 20 - sfldsp
002100060510      * 21 - sfldspctl -- sflclr
002200060510      * 22 - sflend
002300060524      * 28 - visualizza messaggio errore
002400060524      * 39 - errore subfile
002500060524      * 40 - codice tabella
002600060524      * 41 - descrizione
002700060706      * 42 - data limite
002800060706      * 43 - data da per tariffe
002900060706      * 44 - data  a per tariffe
003000060418      *--------------------------------------------------------------*
003100060418
003200060418      *--------------------------------------------------------------*
003300060510      * campi di work
003400060418      *--------------------------------------------------------------*
003500060522     d blanks          s                   like(vt1dopz)
003600060418     d comando         s              1
003700060523     d dataalfa        s              8
003800060523     d dtaiso          s               d   inz
003900060706     d inv_v2cdli      s                   like(v2cdli)
004000060706     d inv_v2cvdd      s                   like(v2cvdd)
004100060706     d inv_v2cvfd      s                   like(v2cvfd)
004200060510     d len             s              5u 0
004300060510     d savrec          s                   like(s01nrr)
004400060517     d savopzione      s                   like(vs1opz)
004500060531     d savtblftt       s                   like(tblftt)
004600060531     d savtblflt       s                   like(tblflt)
004700060510     d savtbluni       s                   like(tbluni)
004800060510     d s01nrr          s              4  0
004900060510     d s02nrr          s              4  0
005000060523     d t19cod          s                   like(tblcod)
005100060523     d t19ord          s              1
005200060523     d t19key          s                   like(tblkey)
005300060523     d t19com          s              1
005400060522     d wrkcard02       s              1    inz('1')
005500060522     d wrkcars01       s              1    inz('1')
005600060522     d wrkcarw01       s              1
005700060524     d wrkeofs01       s              1
005800060522     d wrksfl02        s              1
005900060517     d $video          s              3    inz('C01')
006000060517     d xx              s              2  0
006100060524     d yy              s              2  0
006200060418
006300060418      *--------------------------------------------------------------*
006400060510      * schiere
006500060418      *--------------------------------------------------------------*
006600060418     d msg             s             78    dim(10) ctdata perrcd(1)
006700060523     d opz             s              2  0 dim(10)
006800060418
006900060418      *--------------------------------------------------------------*
007000060510      * ds interne/esterne
007100060418      *--------------------------------------------------------------*
007200060523     d                 ds
007300060523     d aar                     1      2  0
007400060523     d mmr                     3      4  0
007500060523     d ggr                     5      6  0
007600060523     d datar                   1      6  0
007700060523
007800060523     d                 ds
007900060523     d ggd                     1      2  0
008000060523     d mmd                     3      4  0
008100060523     d aad                     5      6  0
008200060523     d datad                   1      6  0
008300060706
008400060706     d wlbdat          ds                  inz
008500060706     d  g02dat                 1      8  0
008600060706     d  g02inv                 9     16  0
008700060706     d  g02err                17     17
008800060706     d  g02tgi                18     22  0
008900060418
009000060706     d dscv          e ds
009100060418     d kpjba         e ds
009200060523     d tibs34ds      e ds                  inz
009300060706     d trtb26ds      e ds
009400060510     d §azute        e ds                  extname(azute00f)
009500060510     d                                     dtaara
009600060510     d §datiute      e ds                  extname(ddatiute)
009700060510     d                                     dtaara
009800060510
009900060510     d psds           sds
010000060510     d  pgmname          *proc
010100060510
010200060510      *------------------------------------------------------------------------*
010300060510      * pgm richiamati
010400060510      *------------------------------------------------------------------------*
010500060510     d tibs34r         pr                  extpgm('TIBS34R')
010600060510     d  tibs34ds                           likeds(tibs34ds)
010700060523
010800060523     d trul19r         pr                  extpgm('TRUL19R')
010900060523     d  t19cod1                            like(t19cod)
011000060523     d  t19ord1                            like(t19ord)
011100060523     d  t19key1                            like(t19key)
011200060523     d  t19com1                            like(t19com)
011300060510
011400060706     d xsrda8          pr                  extpgm('XSRDA8')
011500060706     d  wlbdat                             likeds(wlbdat)
011600060706
011700060510      *--------------------------------------------------------------*
011800060510      * costanti
011900060510      *--------------------------------------------------------------*
012000060510     d errore          c                   '1'
012100060510     d eseguito        c                   '0'
012200060706     d titolo          c                   const('Tabella Divise e Cambi')
012300060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
012400060510     d                                     o/Ripristino  5=Visualizza')
012500060510     d opzs            c                   const('1=Scelta')
012600060418      *--------------------------------------------------------------*
012700060510
012800060510      /free
012900060510
013000060510       exsr sr_parm;
013100060517       exsr sr_video;
013200060510       exsr sr_uscita;
013300060510
013400060510       // ----------------------------------------------------------------------
013500060510       // elaborazione parametri ricevuti
013600060510       // ----------------------------------------------------------------------
013700060510       begsr sr_parm;
013800060510
013900060706        trtb26ds = kpjbu;
014000060510
014100060510        select;
014200060510       // ricerca e scleta
014300060706         when tb26fun = '1';
014400060510          *in10 = *on;
014500060510       // imposto le opzioni
014600060522          vc1opz = opzs;
014700060522          clear opz;
014800060523          opz(1) = 01;
014900060510       // gestione
015000060706         when  tb26fun = *blanks;
015100060510          *in10 = *off;
015200060517       // imposto le opzioni
015300060522          vc1opz = opzg;
015400060522          clear opz;
015500060523          opz(1) = 02;
015600060523          opz(2) = 03;
015700060523          opz(3) = 04;
015800060523          opz(4) = 05;
015900060510       // altrimenti
016000060510         other;
016100060517       // esco
016200060706          tb26esito = errore;
016300060510          exsr sr_uscita;
016400060510         endsl;
016500060510
016600060510       endsr;
016700060517
016800060517       // ----------------------------------------------------------------------
016900060517       // gestione delle videate
017000060517       // ----------------------------------------------------------------------
017100060517       begsr sr_video;
017200060517
017300060522        dow $video <> *blanks;
017400060522
017500060522         select;
017600060706       // subfile 01 - tutte le divise presenti
017700060522          when $video = 'C01';
017800060522           exsr sr_c01;
017900060517
018000060706       // video 01 - nessuna divisa presente
018100060522          when $video = 'D01';
018200060522           exsr sr_d01;
018300060517
018400060706       // video 02 - gestione/visualizzazione divisa scelta
018500060522          when $video = 'D02';
018600060522           exsr sr_d02;
018700060522
018800060522         endsl;
018900060522
019000060522        enddo;
019100060517
019200060517       endsr;
019300060510
019400060510       // ----------------------------------------------------------------------
019500060510       // gestione subfile
019600060510       // ----------------------------------------------------------------------
019700060510       begsr sr_c01;
019800060510
019900060510       // caricamento subfile
020000060517         exsr sr_cars01;
020100060511
020200060511       // se scritto almeno un record
020300060517         if s01nrr > 0;
020400060511       // indicatore di sflend
020500060523          *in22 = *on;
020600060517         endif;
020700060511
020800060511       // se non scritto neanche un record
020900060517         if s01nrr = 0;
021000060511       // emetto videata di dati non trovati
021100060517          $video = 'D01';
021200060524          leavesr;
021300060517         endif;
021400060510
021500060523         if rrrn01 > 0;
021600060523          rec01 = rrrn01;
021700060523         else;
021800060523          rec01 = 1;
021900060517         endif;
022000060523
022100060523       // se non so quale pagina visualizzare forzo pagina 1
022200060523          if rec01 < 1;
022300060523           rec01 = 1;
022400060523          endif;
022500060510
022600060510       // Emissione del subfile
022700060706          write tb26t01;
022800060706          write tb26z01;
022900060706          exfmt tb26c01;
023000060522
023100060522       // reset indicatore generico di errore
023200060522         *in28 = *off;
023300060522       // pulisco il campo messaggi
023400060522         clear vc1msg;
023500060510
023600060510       // controllo tasti funzionali del subfile
023700060510          select;
023800060510
023900060510       // F3=Fine
024000060510           when *inkc;
024100060510            $video = *blanks;
024200060706            tb26ricez = 'C';
024300060510            exsr sr_uscita;
024400060510
024500060510       // F5=Refresh
024600060510           when *inke;
024700060510            wrkcars01 = *on;
024800060510
024900060510       // F10=Immissione
025000060510           when *inkj;
025100060511            $video = 'D02';
025200060706            clear trtb26ds;
025300060706            tb26comand = 'J';
025400060510
025500060510       // F13=Ripetizione
025600060510           when *inkm;
025700060510            exsr sr_ripetiopz;
025800060510
025900060510       // in tutti gli altri casi
026000060510           other;
026100060510       // controllo ed elaborazione scelte su subfile
026200060510            exsr sr_gestsfl;
026300060510          endsl;
026400060510
026500060510        endsr;
026600060522
026700060522       // ----------------------------------------------------------------------
026800060522       // gestione videata
026900060522       // ----------------------------------------------------------------------
027000060522       begsr sr_d01;
027100060522
027200060522       // emetto il video
027300060706          write tb26t01;
027400060706          exfmt tb26d01;
027500060522
027600060522       // controllo tasti funzionali del video
027700060522         select;
027800060522
027900060522       // F3=Fine
028000060522          when *inkc;
028100060706           tb26ricez = 'C';
028200060522           $video = *blanks;
028300060522           exsr sr_uscita;
028400060522           leavesr;
028500060522
028600060522       // F10=Immissione
028700060522          when *inkj;
028800060522           $video = 'D02';
028900060706           clear trtb26ds;
029000060706           tb26comand = 'J';
029100060522           leavesr;
029200060522
029300060522         endsl;
029400060522
029500060522       endsr;
029600060522
029700060522       // ----------------------------------------------------------------------
029800060522       // gestione videata
029900060522       // ----------------------------------------------------------------------
030000060522       begsr sr_d02;
030100060522
030200060522       // imposto il video a seconda della funzione richiesta
030300060522         exsr sr_setvideo;
030400060522
030500060605       // fino a che la variabile resta settata come 'D02'
030600060605        dow $video = 'D02';
030700060605
030800060522       // se immessa opzione di 'scelta'
030900060706         if tb26opzio = 01;
031000060522          $video = *blanks;
031100060706          tb26cod = vs1cod;
031200060706          tb26des = vs1des;
031300060522          exsr sr_uscita;
031400060522          leavesr;
031500060522         endif;
031600060522
031700060522       // se non è immissione/copia proteggo il codice tabella
031800060706          if tb26comand <> 'J' and tb26opzio <> 03;
031900060522           *in02 = *on;
032000060522          endif;
032100060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
032200060522       // proteggo i campi del video
032300060706          if tb26opzio = 04 or tb26opzio = 05;
032400060522           *in01 = *on;
032500060522          endif;
032600060522       // se immessa opzione di 'visualizzazione'
032700060522       // non attivo F6
032800060706          if tb26opzio = 05;
032900060522           *in03 = *on;
033000060522          endif;
033100060522
033200060522       // emetto il video
033300060706          write tb26t01;
033400060706          exfmt tb26d02;
033500060522
033600060522       // reset indicatore generico di errore
033700060522         *in28 = *off;
033800060522       // pulisco il campo messaggi
033900060522         clear v2cmsg;
034000060522
034100060522       // controllo tasti funzionali del video
034200060522         select;
034300060522
034400060522       // F3=Fine
034500060522          when *inkc;
034600060706           tb26ricez = 'C';
034700060522           $video = *blanks;
034800060522           exsr sr_uscita;
034900060522           leavesr;
035000060522
035100060522       // F6=Conferma
035200060522          when *inkf;
035300060706           tb26ricez = 'F';
035400060522       // controllo e decodifico i dati del video
035500060522           exsr sr_ctrd02;
035600060522       // se non riscontrati errori emetto la finestra con i dati per la
035700060522       // tramissione
035800060522           if not *in28;
035900060522            wrkcard02 = *on;
036000060523            wrkcars01 = *off;
036100060522            wrkcarw01 = *on;
036200060522            $video = 'W01';
036300060524            exsr sr_w01;
036400060523            leavesr;
036500060522           endif;
036600060523
036700060523       // F8=Record successivo
036800060524          when *inkh;
036900060524            wrkcars01 = *off;
037000060524            wrkcard02 = *on;
037100060523           $video = 'C01';
037200060706           tb26ricez = 'H';
037300060523           leavesr;
037400060522
037500060522       // F9=Traduzione
037600060524          when *inki;
037700060522           *in05 = *on;
037800060522           $video = 'C02';
037900060523           exsr sr_c02;
038000060522
038100060522       // F12=Ritorno
038200060522          when *inkl;
038300060522           wrkcard02 = *on;
038400060524           wrkcars01 = *off;
038500060706           tb26ricez = 'L';
038600060706           if tb26comand = 'J' or tb26opzio = 03;
038700060523            wrkcars01 = *on;
038800060522           endif;
038900060522           $video = 'C01';
039000060522           leavesr;
039100060522
039200060522       // Invio
039300060522          other;
039400060522           if not *in01;
039500060522            exsr sr_ctrd02;
039600060522           endif;
039700060522
039800060522         endsl;
039900060605
040000060605       // fine gestione 'D02'
040100060605        enddo;
040200060522
040300060522        wrkcard02 = *off;
040400060522
040500060522       endsr;
040600060510
040700060510       // ----------------------------------------------------------------------
040800060510       // caricamento subfile
040900060510       // ----------------------------------------------------------------------
041000060510       begsr sr_cars01;
041100060510
041200060510       // se è stato richiesto il caricamento del subfile
041300060510        if wrkcars01 = *on;
041400060510
041500060510       // inizializzo il subfile
041600060510         s01nrr = 0;
041700060510         *in20 = *off;
041800060510         *in21 = *off;
041900060523         *in22 = *on;
042000060706         write tb26c01;
042100060510         *in20 = *on;
042200060510         *in21 = *on;
042300060510
042400060510       // imposto la chiave di posizionamento e lettura file
042500060510         tblkut = 1;
042600060706         tblcod = 'CV';
042700060517         tblkey = vc1cod;
042800060510
042900060510       // posizionamento sul file
043000060510       // se è stato scelto il posizionamento
043100060517         if vc1cod <> *blanks;
043200060524          rrrn01 = 0;
043300060522          setll (tblkut:tblcod:tblkey) tabel00f;
043400060510       // altrimenti
043500060510         else;
043600060522          setll (tblkut:tblcod) tabel00f;
043700060510         endif;
043800060510
043900060510       // fino a che non è fine file
044000060510         dou %eof(tabel00f);
044100060510
044200060510       // leggo file
044300060510          reade(n) (tblkut:tblcod) tabel00f;
044400060510
044500060510       // fine file esco
044600060510          if %eof(tabel00f);
044700060510           leave;
044800060510          endif;
044900060510
045000060510       // se "ricerca/scelta" non carico i records annullati
045100060510          if (*in10 and tblflg = *blanks) or not *in10;
045200060510       // scrivo subfile
045300060517           clear vs1opz;
045400060510           exsr sr_wtrs01;
045500060510          endif;
045600060510
045700060510         enddo;
045800060510
045900060510       // fine caricamento subfile
046000060510        endif;
046100060510
046200060510       // disattivo opzione di caricamento subfile
046300060510        wrkcars01 = *off;
046400060510
046500060510       endsr;
046600060510
046700060510       // ----------------------------------------------------------------------
046800060510       // scrivo subfile
046900060510       // ----------------------------------------------------------------------
047000060510       begsr sr_wtrs01;
047100060510
047200060510       // se non raggiunto limite massimo di caricamento
047300060510        if s01nrr < 9999;
047400060510       // imposto campi di subfile
047500060510         exsr sr_sets01;
047600060510       // imposto numeratore righe e numero relativo di record
047700060510         s01nrr = s01nrr + 1;
047800060510       // scrivo subfile
047900060706         write tb26s01;
048000060510        endif;
048100060510
048200060510       endsr;
048300060510
048400060510       // ----------------------------------------------------------------------
048500060510       // imposto campi del subfile
048600060510       // ----------------------------------------------------------------------
048700060510       begsr sr_sets01;
048800060510
048900060522        vs1cod = tblkey;
049000060706        dscv = tbluni;
049100060706        vs1des = §cvdes;
049200060706        vs1cmb = §cvcmb;
049300060706
049400060706       // giro data limite inserimento
049500060706        g02inv = §cvdli;
049600060706        g02err = '3';
049700060706        clear g02dat;
049800060706        clear g02tgi;
049900060706        xsrda8(wlbdat);
050000060706        vs1dli = g02dat;
050100060706
050200060517        clear vs1atb;
050300060522        if tblflg <> *blanks;
050400060517         vs1atb = 'A';
050500060517        endif;
050600060510
050700060510       endsr;
050800060517
050900060517       // ----------------------------------------------------------------------
051000060517       // gestione del subfile
051100060517       // ----------------------------------------------------------------------
051200060517       begsr sr_gestsfl;
051300060517
051400060517        wrkeofs01 = *off;
051500060522        clear s01nrr;
051600060523        *in39 = *off;
051700060517
051800060517       // inizio lettura subfile
051900060522        dow wrkeofs01 = *off;
052000060522         s01nrr = s01nrr + 1;
052100060706         chain s01nrr tb26s01;
052200060522
052300060522       // se non trovo esco
052400060522         if not %found;
052500060524          if vc1cod <> *blanks;
052600060524           wrkcars01 = *on;
052700060524          endif;
052800060522          leavesr;
052900060517         endif;
053000060522
053100060517       // se è stata immessa un'opzione
053200060523         if vs1opz <> *zeros;
053300060517       // controllo se opzione valida
053400060517          xx = %lookup(vs1opz:opz);
053500060517          if xx = *zeros;
053600060517           rec01 = s01nrr;
053700060522           vc1msg = msg(03);
053800060517           *in28 = *on;
053900060523           *in39 = *on;
054000060706           update tb26s01;
054100060518           leavesr;
054200060517          endif;
054300060522
054400060517       // reset ds di servizio
054500060706          clear trtb26ds;
054600060706          tb26opzio = vs1opz;
054700060517
054800060523       // gestione del formato video
054900060522          rec01 = s01nrr;
055000060522          $video = 'D02';
055100060523          exsr sr_d02;
055200060524       // se F12 di ritorno esco dalla lettura del subfile
055300060706          if tb26ricez = 'L';
055400060524          wrkeofs01 = *on;
055500060523          endif;
055600060518         endif;
055700060517
055800060523         clear vs1opz;
055900060522         rec01 = s01nrr;
056000060706         update tb26s01;
056100060517        enddo;
056200060517
056300060517       endsr;
056400060517
056500060517       // ----------------------------------------------------------------------
056600060517       // ripeto opzione in tutte le righe del subfile
056700060517       // ----------------------------------------------------------------------
056800060517       begsr sr_ripetiopz;
056900060517
057000060523        if rrrn01 > 0;
057100060523         s01nrr = rrrn01;
057200060706         chain s01nrr tb26s01;
057300060523         if %found and vs1opz <> *zeros;
057400060522          savopzione = vs1opz;
057500060706          update tb26s01;
057600060517
057700060517          wrkeofs01 = *off;
057800060522          dow wrkeofs01 = *off;
057900060517           s01nrr = s01nrr + 1;
058000060706           chain s01nrr tb26s01;
058100060517           if %found;
058200060522            vs1opz = savopzione;
058300060706            update tb26s01;
058400060517           else;
058500060517            wrkeofs01 = *on;
058600060517           endif;
058700060517          enddo;
058800060517
058900060517         endif;
059000060517
059100060517        endif;
059200060517
059300060517       endsr;
059400060517
059500060517       // ----------------------------------------------------------------------
059600060517       // imposto i dati a video
059700060517       // ----------------------------------------------------------------------
059800060517       begsr sr_setvideo;
059900060517
060000060524        if wrkcard02 = *on;
060100060518       // pulisco il formato video D02
060200060524         exsr sr_puld02;
060300060517
060400060522       // controllo se 'immissione' 'modifica' 'copia' o altro
060500060524         select;
060600060517
060700060517       // F10=Immissione
060800060706          when tb26comand = 'J';
060900060524           vt1dopz = 'Immissione';
061000060517
061100060523       // Opzione "02"=modifica
061200060706          when tb26opzio = 02;
061300060524           vt1dopz = 'Modifica';
061400060524           exsr sr_imposta;
061500060517
061600060523       // Opzione "03"=copia
061700060706          when tb26opzio = 03;
061800060524           vt1dopz = 'Copia';
061900060524           exsr sr_imposta;
062000060517
062100060523       // Opzione "04"=cancellazione/ripristino
062200060706          when tb26opzio = 04;
062300060524           exsr sr_imposta;
062400060517       // se richiesta 'cancellazione'
062500060524           if tblflg = *blanks;
062600060524            vt1dopz = 'Annullamento';
062700060524           endif;
062800060517       // se richiesto 'ripristino'
062900060524           if tblflg <> *blanks;
063000060524            vt1dopz = 'Ripristino';
063100060524           endif;
063200060517
063300060523       // Opzione "05"=visualizzazione
063400060706          when tb26opzio =05;
063500060524           vt1dopz = 'Visualizzazione';
063600060524           exsr sr_imposta;
063700060517
063800060517       // Fine scelta
063900060524         endsl;
064000060517
064100060517       // centro la descrizione della funzione nel formato video
064200060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
064300060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
064400060524
064500060524        endif;
064600060517
064700060517       endsr;
064800060524
064900060524       // ----------------------------------------------------------------------
065000060524       // pulizia video
065100060524       // ----------------------------------------------------------------------
065200060524       begsr sr_puld02;
065300060524
065400060524        clear vt1dopz;
065500060524        clear v2ccod;
065600060524        clear v2cdes;
065700060706        clear v2ccmb;
065800060706        clear v2cfdc;
065900060706        clear v2cdec;
066000060706        clear v2cdli;
066100060706        clear v2cita;
066200060706        clear v2cuem;
066300060706        clear v2cdva;
066400060706        clear v2cabi;
066500060706        clear v2cvdd;
066600060706        clear v2cvfd;
066700060706        clear v2ctra;
066800060706        clear v2cvas;
066900060524
067000060524        *in01 = *off;
067100060524        *in02 = *off;
067200060524        *in03 = *off;
067300060524        *in04 = *off;
067400060524        *in05 = *off;
067500060524        *in28 = *off;
067600060524        *in40 = *off;
067700060524        *in41 = *off;
067800060524        *in42 = *off;
067900060524        *in43 = *off;
068000060524        *in44 = *off;
068100060524
068200060524       endsr;
068300060524
068400060524       // ----------------------------------------------------------------------
068500060524       // imposto i dati a video
068600060524       // ----------------------------------------------------------------------
068700060524       begsr sr_imposta;
068800060524
068900060524       // recupero i dati dal file
069000060524        tblkut = 1;
069100060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
069200060524
069300060524       // imposto i campi a video
069400060524        v2ccod = tblkey;
069500060706        dscv = tbluni;
069600060706        v2cdes = §cvdes;
069700060706        v2ccmb = §cvcmb;
069800060706        v2cvas = §cvvas;
069900060706        v2cfdc = §cvfdc;
070000060706        v2cita = §cvita;
070100060706        v2cabi = §cvabi;
070200060706        v2cdva = §cvdva;
070300060706        v2cdec = §cvdec;
070400060706        v2ctra = §cvtra;
070500060706        v2cuem = §cvuem;
070600060706
070700060706       // giro le date
070800060706        g02inv = §cvdli;
070900060706        g02err = '3';
071000060706        clear g02dat;
071100060706        clear g02tgi;
071200060706        xsrda8(wlbdat);
071300060706        v2cdli = g02dat;
071400060524
071500060706        g02inv = §cvvdd;
071600060706        g02err = '3';
071700060706        clear g02dat;
071800060706        clear g02tgi;
071900060706        xsrda8(wlbdat);
072000060706        v2cvdd = g02dat;
072100060706
072200060706        g02inv = §cvvfd;
072300060706        g02err = '3';
072400060706        clear g02dat;
072500060706        clear g02tgi;
072600060706        xsrda8(wlbdat);
072700060706        v2cvfd = g02dat;
072800060706
072900060524        if tblflg <> *blanks;
073000060524         *in04 = *on;
073100060524        endif;
073200060524
073300060524       endsr;
073400060517
073500060517       // ----------------------------------------------------------------------
073600060517       // controllo video
073700060517       // ----------------------------------------------------------------------
073800060522       begsr sr_ctrd02;
073900060517
074000060517        *in28 = *off;
074100060517        *in40 = *off;
074200060517        *in41 = *off;
074300060523        *in42 = *off;
074400060524        *in43 = *off;
074500060524        *in44 = *off;
074600060706        clear inv_v2cdli;
074700060706        clear inv_v2cvdd;
074800060706        clear inv_v2cvfd;
074900060517
075000060522       // codice tabella
075100060522        if v2ccod = *blanks;
075200060522         v2cmsg = msg(01);
075300060517         *in28 = *on;
075400060517         *in40 = *on;
075500060517         leavesr;
075600060517        endif;
075700060517
075800060517       // se immissione controllo che non esista già
075900060706        if tb26comand = 'J' or tb26opzio = 03;
076000060522         tblkut = 1;
076100060706         tblcod = 'CV';
076200060522         tblkey = v2ccod;
076300060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
076400060522         if %found(tabel00f);
076500060522          v2cmsg = msg(05);
076600060517          *in28 = *on;
076700060517          *in40 = *on;
076800060517          leavesr;
076900060517         endif;
077000060517        endif;
077100060517
077200060517       // descrizione
077300060522        if v2cdes = *blanks;
077400060522         v2cmsg = msg(02);
077500060517         *in28 = *on;
077600060517         *in41 = *on;
077700060517         leavesr;
077800060517        endif;
077900060523
078000060706       // data limite inserimento
078100060706        if v2cdli <> *zeros;
078200060706         g02dat = v2cdli;
078300060706         clear g02err;
078400060706         clear g02inv;
078500060706         clear g02tgi;
078600060706         xsrda8(wlbdat);
078700060706         if g02err = '1';
078800060523          v2cmsg = msg(04);
078900060523          *in28 = *on;
079000060523          *in42 = *on;
079100060523          leavesr;
079200060523         endif;
079300060706         v2cdli = g02dat;
079400060706         inv_v2cdli = g02inv;
079500060523        endif;
079600060706
079700060706       // data inizio validità tariffe
079800060706        if v2cvdd <> *zeros;
079900060706         g02dat = v2cvdd;
080000060706         clear g02err;
080100060706         clear g02inv;
080200060706         clear g02tgi;
080300060706         xsrda8(wlbdat);
080400060706         if g02err = '1';
080500060706          v2cmsg = msg(04);
080600060706          *in28 = *on;
080700060706          *in43 = *on;
080800060706          leavesr;
080900060706         endif;
081000060706         v2cvdd = g02dat;
081100060706         inv_v2cvdd = g02inv;
081200060706        endif;
081300060706
081400060706       // data fine validità tariffe
081500060706        if v2cvfd <> *zeros;
081600060706         g02dat = v2cvfd;
081700060706         clear g02err;
081800060706         clear g02inv;
081900060706         clear g02tgi;
082000060706         xsrda8(wlbdat);
082100060706         if g02err = '1';
082200060706          v2cmsg = msg(04);
082300060706          *in28 = *on;
082400060706          *in44 = *on;
082500060706          leavesr;
082600060706         endif;
082700060706         v2cvfd = g02dat;
082800060706         inv_v2cvfd = g02inv;
082900060706        endif;
083000060706
083100060706       // se inserita data inizio validità controllo che non sia superiore
083200060706       // alla data fine validità
083300060706        if v2cvdd <> *zeros and inv_v2cvdd > inv_v2cvfd;
083400060706         v2cmsg = msg(05);
083500060706         *in28 = *on;
083600060706         *in43 = *on;
083700060706         leavesr;
083800060706        endif;
083900060706
084000060706       // se inserita data fine validità deve esserci anche la data inizio
084100060706       // validità
084200060706        if v2cvfd <> *zeros and v2cvdd = *zeros;
084300060706         v2cmsg = msg(04);
084400060706         *in28 = *on;
084500060706         *in43 = *on;
084600060706         leavesr;
084700060706        endif;
084800060517
084900060517       endsr;
085000060524
085100060524       // ----------------------------------------------------------------------
085200060524       // gestione video dati di trasmissione
085300060524       // ----------------------------------------------------------------------
085400060524       begsr sr_w01;
085500060524
085600060524       // imposto i dati a video
085700060524        exsr sr_carw01;
085800060524
085900060524       // fino a che la variabile resta settata come 'W01'
086000060524        dow $video = 'W01';
086100060524
086200060524       // reset indicatore generico di errore
086300060524         *in28 = *off;
086400060524
086500060524       // emetto il video
086600060706          exfmt tb26w01;
086700060524
086800060524       // controllo tasti funzionali del video
086900060524         select;
087000060524
087100060524       // F6=Conferma
087200060524          when *inkf;
087300060524       // controllo i dati del video
087400060524           exsr sr_ctrw01;
087500060524       // se non riscontrati errori aggiorno il record corrente
087600060524           if not *in28;
087700060524            exsr sr_aggiorna;
087800060524            if not *in28;
087900060524       // emetto le descrizioni in lingua
088000060524       // se non è annullo/ripristino
088100060706             if tb26opzio <> 04;
088200060524              *in05 = *off;
088300060524              $video = 'C02';
088400060524              exsr sr_c02;
088500060524              leavesr;
088600060524             endif;
088700060706             if tb26comand = 'J';
088800060524              $video = 'D02';
088900060524              exsr sr_setvideo;
089000060524             else;
089100060524              wrkcars01 = *on;
089200060524              $video = 'C01';
089300060524             endif;
089400060524            endif;
089500060524           endif;
089600060524
089700060524       // F12=Ritorno
089800060524          when *inkl;
089900060524           $video = 'D02';
090000060706           clear tb26ricez;
090100060524
090200060524       // Invio
090300060524          other;
090400060524           exsr sr_ctrw01;
090500060524
090600060524         endsl;
090700060524
090800060524       // fine gestione 'W01'
090900060524        enddo;
091000060524
091100060524       endsr;
091200060517
091300060517       // ----------------------------------------------------------------------
091400060517       // imposto i dati di trasmissione
091500060517       // ----------------------------------------------------------------------
091600060517       begsr sr_carw01;
091700060517
091800060517       // se pulisco i campi
091900060522         clear w01ftt;
092000060522         clear w01flt;
092100060522         clear w01ftr;
092200060522         clear w01dtr;
092300060517
092400060517       // se non immissione imposto i campi del file
092500060706        if tb26comand <> 'J';
092600060522         w01ftt = tblftt;
092700060522         w01flt = tblflt;
092800060522         w01ftr = tblftr;
092900060517       // imposto la data
093000060522         if tbldtr <> *zeros;
093100060523          datar = tbldtr;
093200060523          aad = aar;
093300060523          mmd = mmr;
093400060523          ggd = ggr;
093500060523          w01dtr = datad;
093600060517         endif;
093700060517
093800060517        endif;
093900060517
094000060517       endsr;
094100060517
094200060517       // ----------------------------------------------------------------------
094300060517       // controllo i dati di trasmissione
094400060517       // ----------------------------------------------------------------------
094500060517       begsr sr_ctrw01;
094600060517
094700060517       endsr;
094800060517
094900060517       // ----------------------------------------------------------------------
095000060517       // aggiorno tabella
095100060517       // ----------------------------------------------------------------------
095200060517       begsr sr_aggiorna;
095300060517
095400060517       // imposto campi del file
095500060522        tblkut = 1;
095600060706        tblcod = 'CV';
095700060522        tblkey = v2ccod;
095800060523
095900060523       // aggancio il rcd della tabella
096000060523        chain (tblkut:tblcod:tblkey) tabel00f;
096100060517
096200060706        §cvdes = v2cdes;
096300060706        §cvcmb = v2ccmb;
096400060706        §cvvas = v2cvas;
096500060706        §cvfdc = v2cfdc;
096600060706        §cvita = v2cita;
096700060706        §cvabi = v2cabi;
096800060706        §cvdva = v2cdva;
096900060706        §cvdec = v2cdec;
097000060706        §cvtra = v2ctra;
097100060706        §cvuem = v2cuem;
097200060706        §cvdli = inv_v2cdli;
097300060706        §cvvdd = inv_v2cvdd;
097400060706        §cvvfd = inv_v2cvfd;
097500060517
097600060706        tbluni = dscv;
097700060517
097800060522        tblftt = w01ftt;
097900060522        tblflt = w01flt;
098000060522        clear tblftr;
098100060522        clear tbldtr;
098200060524
098300060524       // annullo/ripristino
098400060706        if tb26opzio = 04;
098500060524         select;
098600060524       // annullamento
098700060524          when tblflg = *blanks;
098800060524           tblflg = '*';
098900060524       // ripristino
099000060524          when tblflg <> *blanks;
099100060524           clear tblflg;
099200060524         endsl;
099300060524        endif;
099400060517
099500060523       // controllo se devo scrivere o aggiornare il record
099600060523        if %found(tabel00f);
099700060523         update tabel;
099800060523        else;
099900060523         write tabel;
100000060523        endif;
100100060517
100200060523       // se Opzione "04"=cancellazione/ripristino
100300060517       // aggiorno automaticamente anche le descrizioni in linea
100400060706        if tb26opzio = 04;
100500060522         lintabel = 1;
100600060522         setgt lintabel azlin01l;
100700060522         dou %eof(azlin01l);
100800060522          read azlin01l;
100900060517       // fine file
101000060522          if %eof(azlin01l);
101100060517           leave;
101200060517          endif;
101300060517       // aggiorno
101400060522          tblkut = lintabel;
101500060522          chain (tblkut:tblcod:tblkey) tabel00f;
101600060522          if %found(tabel00f);
101700060517           select;
101800060522            when tblflg <> *blanks;
101900060522             clear tblflg;
102000060523            when tblflg = *blanks;
102100060522             tblflg = '*';
102200060517           endsl;
102300060522            update tabel;
102400060517          endif;
102500060517         enddo;
102600060517        endif;
102700060517
102800060517       endsr;
102900060524
103000060524       // ----------------------------------------------------------------------
103100060524       // gestione descrizioni in lingua
103200060524       // ----------------------------------------------------------------------
103300060524       begsr sr_c02;
103400060524
103500060524       // imposto il codice tabella e la descrizione
103600060524       vc2cod = v2ccod;
103700060524       vc2des = v2cdes;
103800060524
103900060524       // pulisco il subfile
104000060524        exsr sr_puls02;
104100060524
104200060524       // carico il subfile con i dati che trovo o vuoto
104300060524        exsr sr_cars02;
104400060524
104500060524        wrksfl02 = *on;
104600060524
104700060524        dou wrksfl02 = *off;
104800060524         rec02 = 1;
104900060524         if savrec > *zeros;
105000060524          rec02 = savrec;
105100060524         endif;
105200060706         write tb26z02;
105300060706         exfmt tb26c02;
105400060524
105500060524         *in28 = *off;
105600060524         *in39 = *off;
105700060524
105800060524       // F12=Ritorno
105900060524          if *inkl;
106000060524           wrkcard02 = *off;
106100060524           $video = 'D02';
106200060524           leavesr;
106300060524          endif;
106400060524
106500060524       // Controlli
106600060524          if not *in05;
106700060524           exsr sr_ctrsfl;
106800060524          endif;
106900060524
107000060524       // F6=Conferma
107100060524          if *inkf and not *in28;
107200060524           exsr sr_confsfl;
107300060524           wrksfl02 = *off;
107400060524          endif;
107500060524
107600060524         enddo;
107700060524
107800060524       endsr;
107900060524
108000060524       // ----------------------------------------------------------------------
108100060524       // pulisco il subfile delle descrizioni in lingua
108200060524       // ----------------------------------------------------------------------
108300060524       begsr sr_puls02;
108400060524
108500060524        clear s02nrr;
108600060524        clear savrec;
108700060524        *in20 = *off;
108800060524        *in21 = *off;
108900060706        write tb26c02;
109000060524        *in20 = *on;
109100060524        *in21 = *on;
109200060524
109300060524       endsr;
109400060524
109500060524       // ----------------------------------------------------------------------
109600060524       // carico il subfile delle descrizioni in lingua
109700060524       // ----------------------------------------------------------------------
109800060524       begsr sr_cars02;
109900060524
110000060524        *in22 = *off;
110100060524       // carico un record per ogni lingua inserite in AZLIN
110200060524        lintabel = 1;
110300060524        setgt lintabel azlin01l;
110400060524        dou %eof(azlin01l);
110500060524         read azlin01l;
110600060524       // fine file
110700060524         if %eof(azlin01l);
110800060524          leave;
110900060524         endif;
111000060706         clear dscv;
111100060524         s2dlin = lindesita;
111200060524         tblkut = lintabel;
111300060706         tblcod = 'CV';
111400060524         tblkey = v2ccod;
111500060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
111600060524         if not %found(tabel00f);
111700060524          clear s2cdes;
111800060524         else;
111900060706          dscv = tbluni;
112000060706          s2cdes = §cvdes;
112100060524         endif;
112200060524         s2hcdlin = tblkut;
112300060524         s02nrr = s02nrr + 1;
112400060706         write tb26s02;
112500060524        enddo;
112600060524
112700060524        *in22 = *on;
112800060524
112900060524       endsr;
113000060524
113100060524       // ----------------------------------------------------------------------
113200060524       // controllo il subfile delle descrizioni in lingua
113300060524       // ----------------------------------------------------------------------
113400060524       begsr sr_ctrsfl;
113500060524
113600060524        s02nrr = 1;
113700060706        chain s02nrr tb26s02;
113800060524        dow %found;
113900060524       // deve esserci la descrizione
114000060524         if s2cdes = *blanks;
114100060524          vc2msg = msg(02);
114200060524          *in28 = *on;
114300060524          *in39 = *on;
114400060524          savrec = s02nrr;
114500060706          update tb26s02;
114600060524          leave;
114700060524         endif;
114800060706         update tb26s02;
114900060524         s02nrr = s02nrr + 1;
115000060706         chain s02nrr tb26s02;
115100060524        enddo;
115200060524
115300060524       endsr;
115400060524
115500060524       // ----------------------------------------------------------------------
115600060524       // confermo il dati del subfile delle descrizioni in lingua
115700060524       // ----------------------------------------------------------------------
115800060524       begsr sr_confsfl;
115900060524
116000060524       // recupero i dati della tabella in italiano
116100060524        clear savtbluni;
116200060524        tblkut = 1;
116300060706        tblcod = 'CV';
116400060524        tblkey = v2ccod;
116500060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
116600060524        if %found(tabel00f);
116700060524         savtbluni = tbluni;
116800060524        endif;
116900060524
117000060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
117100060524        s02nrr = 1;
117200060706        chain s02nrr tb26s02;
117300060524        dow %found;
117400060524         tblkut = s2hcdlin;
117500060524         chain (tblkut:tblcod:tblkey) tabel00f;
117600060706         dscv = savtbluni;
117700060706         §cvdes = s2cdes;
117800060706         tbluni = dscv;
117900060524         tblftt = w01ftt;
118000060524         clear tblftr;
118100060524         clear tbldtr;
118200060524       // scrivo
118300060524         if not %found(tabel00f);
118400060524          tblkut = s2hcdlin;
118500060706          tblcod = 'CV';
118600060524          tblkey = v2ccod;
118700060524          write tabel;
118800060524         else;
118900060524       // aggiorno
119000060524          update tabel;
119100060524         endif;
119200060524         s02nrr = s02nrr + 1;
119300060706         chain s02nrr tb26s02;
119400060524        enddo;
119500060524
119600060524        wrkcars01 = *on;
119700060524        $video = 'C01';
119800060524
119900060524       endsr;
120000060524
120100060524       // ----------------------------------------------------------------------
120200060524       // uscita dal programma
120300060524       // ----------------------------------------------------------------------
120400060524         begsr sr_uscita;
120500060524
120600060706          if tb26esito = *blanks;
120700060706           tb26esito = eseguito;
120800060524          endif;
120900060524
121000060706          kpjbu = trtb26ds;
121100060524
121200060524          *inlr = *on;
121300060524          return;
121400060524
121500060524         endsr;
121600060510
121700060510       // ----------------------------------------------------------------------
121800060510       // routine iniziale
121900060510       // ----------------------------------------------------------------------
122000060510         begsr *inzsr;
122100060510
122200060510      /end-free
122300060510
122400060510     c     *entry        plist
122500060510     c                   parm                    kpjba
122600060510
122700060510      /free
122800060510         in(e) §azute;
122900060510         if not %error;
123000060510          in(e) §datiute;
123100060510         endif;
123200060510         if %error or rsut = *blanks;
123300060510          tibs34r(tibs34ds);
123400060510          in §azute;
123500060510          in §datiute;
123600060510         endif;
123700060510
123800060510       // centro il titolo
123900060510        vt1tit = titolo;
124000060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
124100060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
124200060510
124300060510         endsr;
124400060510
124500060510      /end-free
124600060418
124700060418** -- MSG -------------------------------------------------------------------*
124800060418Immettere il codice                                                            1
124900060418Immettere la descrizione                                                       2
125000060522Opzione errata                                                                 3
125100060523Data errata                                                                    4
125200060706Data dal maggiore data al                                                      5
