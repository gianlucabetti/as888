000100060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060418
000300060418      *--------------------------------------------------------------*
000400090603      * GESTIONE TABELLA TIPI SERVIZIO                               *
000500060418      *--------------------------------------------------------------*
000600060418
000700060418     ftabel00f  uf a e           k disk
000800060426     fazlin01l  if   e           k disk
000900090603     ftrtb5ed   cf   e             workstn sfile(tb5es01:s01nrr)
001000090603     f                                     sfile(tb5es02:s02nrr)
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
002700090603      * 42 - descrizione 03
002800090603      * 43 - descrizione 05
002900090603      * 44 - descrizione 08
003000090603      * 45 - descrizione stp.dly
003100090603      * 46 - tariffa particolare
003200060418      *--------------------------------------------------------------*
003300060418
003400060418      *--------------------------------------------------------------*
003500060510      * campi di work
003600060418      *--------------------------------------------------------------*
003700060522     d blanks          s                   like(vt1dopz)
003800060418     d comando         s              1
003900060523     d dataalfa        s              8
004000060523     d dtaiso          s               d   inz
004100060510     d len             s              5u 0
004200060510     d savrec          s                   like(s01nrr)
004300060517     d savopzione      s                   like(vs1opz)
004400060531     d savtblftt       s                   like(tblftt)
004500060531     d savtblflt       s                   like(tblflt)
004600060510     d savtbluni       s                   like(tbluni)
004700060510     d s01nrr          s              4  0
004800060510     d s02nrr          s              4  0
004900060523     d t19cod          s                   like(tblcod)
005000060523     d t19ord          s              1
005100060523     d t19key          s                   like(tblkey)
005200060523     d t19com          s              1
005300060522     d wrkcard02       s              1    inz('1')
005400060522     d wrkcars01       s              1    inz('1')
005500060522     d wrkcarw01       s              1
005600060524     d wrkeofs01       s              1
005700060522     d wrksfl02        s              1
005800060517     d $video          s              3    inz('C01')
005900060517     d xx              s              2  0
006000060524     d yy              s              2  0
006100090827     d Indx            s              2  0
006200090827     D CARATTERI       C                   CONST('0123456789x')
006300060418
006400060418      *--------------------------------------------------------------*
006500060510      * schiere
006600060418      *--------------------------------------------------------------*
006700060418     d msg             s             78    dim(10) ctdata perrcd(1)
006800060523     d opz             s              2  0 dim(10)
006900060418
007000060418      *--------------------------------------------------------------*
007100060510      * ds interne/esterne
007200060418      *--------------------------------------------------------------*
007300060523     d                 ds
007400060523     d aar                     1      2  0
007500060523     d mmr                     3      4  0
007600060523     d ggr                     5      6  0
007700060523     d datar                   1      6  0
007800060523
007900060523     d                 ds
008000060523     d ggd                     1      2  0
008100060523     d mmd                     3      4  0
008200060523     d aad                     5      6  0
008300060523     d datad                   1      6  0
008400060418
008500090603     d ds1p          e ds
008600090603     d ds5e          e ds
008700060418     d kpjba         e ds
008800090622     d tibs02ds      e ds
008900060523     d tibs34ds      e ds                  inz
009000090603     d trtb5eds      e ds
009100060510     d §azute        e ds                  extname(azute00f)
009200060510     d                                     dtaara
009300060510     d §datiute      e ds                  extname(ddatiute)
009400060510     d                                     dtaara
009500060510
009600060510     d psds           sds
009700060510     d  pgmname          *proc
009800060510
009900060510      *------------------------------------------------------------------------*
010000060510      * pgm richiamati
010100060510      *------------------------------------------------------------------------*
010200090622      /copy gaitrasrc/srcProtoPR,TIBS02R
010300090622
010400060510     d tibs34r         pr                  extpgm('TIBS34R')
010500060510     d  tibs34ds                           likeds(tibs34ds)
010600060523
010700060523     d trul19r         pr                  extpgm('TRUL19R')
010800060523     d  t19cod1                            like(t19cod)
010900060523     d  t19ord1                            like(t19ord)
011000060523     d  t19key1                            like(t19key)
011100060523     d  t19com1                            like(t19com)
011200090622
011300090622     d tntb73r         pr                  extpgm('TNTB73R')
011400090622     d  kpjba                              likeds(kpjba)
011500060510
011600060510      *--------------------------------------------------------------*
011700060510      * costanti
011800060510      *--------------------------------------------------------------*
011900060510     d errore          c                   '1'
012000060510     d eseguito        c                   '0'
012100090603     d titolo          c                   const('Tabella Tipi Servizio')
012200060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
012300060510     d                                     o/Ripristino  5=Visualizza')
012400060510     d opzs            c                   const('1=Scelta')
012500060418      *--------------------------------------------------------------*
012600060510
012700060510      /free
012800060510
012900060510       exsr sr_parm;
013000060517       exsr sr_video;
013100060510       exsr sr_uscita;
013200060510
013300060510       // ----------------------------------------------------------------------
013400060510       // elaborazione parametri ricevuti
013500060510       // ----------------------------------------------------------------------
013600060510       begsr sr_parm;
013700060510
013800090603        trtb5eds = kpjbu;
013900060510
014000060510        select;
014100060510       // ricerca e scleta
014200090603         when tb5efun = '1';
014300060510          *in10 = *on;
014400060510       // imposto le opzioni
014500060522          vc1opz = opzs;
014600060522          clear opz;
014700060523          opz(1) = 01;
014800060510       // gestione
014900090603         when  tb5efun = *blanks;
015000060510          *in10 = *off;
015100060517       // imposto le opzioni
015200060522          vc1opz = opzg;
015300060522          clear opz;
015400060523          opz(1) = 02;
015500060523          opz(2) = 03;
015600060523          opz(3) = 04;
015700060523          opz(4) = 05;
015800060510       // altrimenti
015900060510         other;
016000060517       // esco
016100090603          tb5eesito = errore;
016200060510          exsr sr_uscita;
016300060510         endsl;
016400060510
016500060510       endsr;
016600060517
016700060517       // ----------------------------------------------------------------------
016800060517       // gestione delle videate
016900060517       // ----------------------------------------------------------------------
017000060517       begsr sr_video;
017100060517
017200060522        dow $video <> *blanks;
017300060522
017400060522         select;
017500060517       // subfile 01 - tutte le tar.particolari presenti
017600060522          when $video = 'C01';
017700060522           exsr sr_c01;
017800060517
017900060517       // video 01 - nessuna tar.particolare presente
018000060522          when $video = 'D01';
018100060522           exsr sr_d01;
018200060517
018300060517       // video 02 - gestione/visualizzazione tar.particolare scelta
018400060522          when $video = 'D02';
018500060522           exsr sr_d02;
018600060522
018700060522         endsl;
018800060522
018900060522        enddo;
019000060517
019100060517       endsr;
019200060510
019300060510       // ----------------------------------------------------------------------
019400060510       // gestione subfile
019500060510       // ----------------------------------------------------------------------
019600060510       begsr sr_c01;
019700060510
019800060510       // caricamento subfile
019900060517         exsr sr_cars01;
020000060511
020100060511       // se scritto almeno un record
020200060517         if s01nrr > 0;
020300060511       // indicatore di sflend
020400060523          *in22 = *on;
020500060517         endif;
020600060511
020700060511       // se non scritto neanche un record
020800060517         if s01nrr = 0;
020900060511       // emetto videata di dati non trovati
021000060517          $video = 'D01';
021100060524          leavesr;
021200060517         endif;
021300060510
021400060523         if rrrn01 > 0;
021500060523          rec01 = rrrn01;
021600060523         else;
021700060523          rec01 = 1;
021800060517         endif;
021900060523
022000060523       // se non so quale pagina visualizzare forzo pagina 1
022100060523          if rec01 < 1;
022200060523           rec01 = 1;
022300060523          endif;
022400060510
022500060510       // Emissione del subfile
022600090603          write tb5et01;
022700090603          write tb5ez01;
022800090603          exfmt tb5ec01;
022900060522
023000060522       // reset indicatore generico di errore
023100060522         *in28 = *off;
023200060522       // pulisco il campo messaggi
023300060522         clear vc1msg;
023400060510
023500060510       // controllo tasti funzionali del subfile
023600060510          select;
023700060510
023800060510       // F3=Fine
023900060510           when *inkc;
024000060510            $video = *blanks;
024100090603            tb5ericez = 'C';
024200060510            exsr sr_uscita;
024300060510
024400060510       // F5=Refresh
024500060510           when *inke;
024600060510            wrkcars01 = *on;
024700060510
024800090622       // F8=Tabella LTS Limiti
024900090622           when *inkh;
025000090622            exsr sr_f08;
025100090622
025200060510       // F10=Immissione
025300060510           when *inkj;
025400060511            $video = 'D02';
025500090603            clear trtb5eds;
025600090603            tb5ecomand = 'J';
025700060510
025800060510       // F13=Ripetizione
025900060510           when *inkm;
026000060510            exsr sr_ripetiopz;
026100060510
026200060510       // in tutti gli altri casi
026300060510           other;
026400060510       // controllo ed elaborazione scelte su subfile
026500060510            exsr sr_gestsfl;
026600060510          endsl;
026700060510
026800060510        endsr;
026900060522
027000060522       // ----------------------------------------------------------------------
027100060522       // gestione videata
027200060522       // ----------------------------------------------------------------------
027300060522       begsr sr_d01;
027400060522
027500060522       // emetto il video
027600090603          write tb5et01;
027700090603          exfmt tb5ed01;
027800060522
027900060522       // controllo tasti funzionali del video
028000060522         select;
028100060522
028200060522       // F3=Fine
028300060522          when *inkc;
028400090603           tb5ericez = 'C';
028500060522           $video = *blanks;
028600060522           exsr sr_uscita;
028700060522           leavesr;
028800060522
028900060522       // F10=Immissione
029000060522          when *inkj;
029100060522           $video = 'D02';
029200090603           clear trtb5eds;
029300090603           tb5ecomand = 'J';
029400060522           leavesr;
029500060522
029600060522         endsl;
029700060522
029800060522       endsr;
029900060522
030000060522       // ----------------------------------------------------------------------
030100060522       // gestione videata
030200060522       // ----------------------------------------------------------------------
030300060522       begsr sr_d02;
030400060522
030500060522       // imposto il video a seconda della funzione richiesta
030600060522         exsr sr_setvideo;
030700060522
030800060605       // fino a che la variabile resta settata come 'D02'
030900060605        dow $video = 'D02';
031000060605
031100060522       // se immessa opzione di 'scelta'
031200090603         if tb5eopzio = 01;
031300060522          $video = *blanks;
031400090603          tb5ecod = vs1cod;
031500090603          tb5edes = vs1des;
031600060522          exsr sr_uscita;
031700060522          leavesr;
031800060522         endif;
031900060522
032000060522       // se non è immissione/copia proteggo il codice tabella
032100090603          if tb5ecomand <> 'J' and tb5eopzio <> 03;
032200060522           *in02 = *on;
032300060522          endif;
032400060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
032500060522       // proteggo i campi del video
032600090603          if tb5eopzio = 04 or tb5eopzio = 05;
032700060522           *in01 = *on;
032800060522          endif;
032900060522       // se immessa opzione di 'visualizzazione'
033000060522       // non attivo F6
033100090603          if tb5eopzio = 05;
033200060522           *in03 = *on;
033300060522          endif;
033400060522
033500060522       // emetto il video
033600090603          write tb5et01;
033700090603          exfmt tb5ed02;
033800060522
033900060522       // reset indicatore generico di errore
034000060522         *in28 = *off;
034100060522       // pulisco il campo messaggi
034200060522         clear v2cmsg;
034300060522
034400060522       // controllo tasti funzionali del video
034500060522         select;
034600060522
034700060522       // F3=Fine
034800060522          when *inkc;
034900090603           tb5ericez = 'C';
035000060522           $video = *blanks;
035100060522           exsr sr_uscita;
035200060522           leavesr;
035300060522
035400060522       // F6=Conferma
035500060522          when *inkf;
035600090603           tb5ericez = 'F';
035700060522       // controllo e decodifico i dati del video
035800060522           exsr sr_ctrd02;
035900060522       // se non riscontrati errori emetto la finestra con i dati per la
036000060522       // tramissione
036100060522           if not *in28;
036200060522            wrkcard02 = *on;
036300060523            wrkcars01 = *off;
036400060522            wrkcarw01 = *on;
036500060522            $video = 'W01';
036600060524            exsr sr_w01;
036700060523            leavesr;
036800060522           endif;
036900060523
037000060523       // F8=Record successivo
037100060524          when *inkh;
037200060524            wrkcars01 = *off;
037300060524            wrkcard02 = *on;
037400060523           $video = 'C01';
037500090603           tb5ericez = 'H';
037600060523           leavesr;
037700060522
037800060522       // F9=Traduzione
037900060524          when *inki;
038000060522           *in05 = *on;
038100060522           $video = 'C02';
038200060523           exsr sr_c02;
038300060522
038400060522       // F12=Ritorno
038500060522          when *inkl;
038600060522           wrkcard02 = *on;
038700060524           wrkcars01 = *off;
038800090603           tb5ericez = 'L';
038900090603           if tb5ecomand = 'J' or tb5eopzio = 03;
039000060523            wrkcars01 = *on;
039100060522           endif;
039200060522           $video = 'C01';
039300060522           leavesr;
039400060522
039500060522       // Invio
039600060522          other;
039700060522           if not *in01;
039800060522            exsr sr_ctrd02;
039900060522           endif;
040000060522
040100060522         endsl;
040200060605
040300060605       // fine gestione 'D02'
040400060605        enddo;
040500060522
040600060522        wrkcard02 = *off;
040700060522
040800060522       endsr;
040900060510
041000060510       // ----------------------------------------------------------------------
041100060510       // caricamento subfile
041200060510       // ----------------------------------------------------------------------
041300060510       begsr sr_cars01;
041400060510
041500060510       // se è stato richiesto il caricamento del subfile
041600060510        if wrkcars01 = *on;
041700060510
041800060510       // inizializzo il subfile
041900060510         s01nrr = 0;
042000060510         *in20 = *off;
042100060510         *in21 = *off;
042200060523         *in22 = *on;
042300090603         write tb5ec01;
042400060510         *in20 = *on;
042500060510         *in21 = *on;
042600060510
042700060510       // imposto la chiave di posizionamento e lettura file
042800060510         tblkut = 1;
042900090603         tblcod = '5E';
043000060517         tblkey = vc1cod;
043100060510
043200060510       // posizionamento sul file
043300060510       // se è stato scelto il posizionamento
043400060517         if vc1cod <> *blanks;
043500060524          rrrn01 = 0;
043600060522          setll (tblkut:tblcod:tblkey) tabel00f;
043700060510       // altrimenti
043800060510         else;
043900060522          setll (tblkut:tblcod) tabel00f;
044000060510         endif;
044100060510
044200060510       // fino a che non è fine file
044300060510         dou %eof(tabel00f);
044400060510
044500060510       // leggo file
044600060510          reade(n) (tblkut:tblcod) tabel00f;
044700060510
044800060510       // fine file esco
044900060510          if %eof(tabel00f);
045000060510           leave;
045100060510          endif;
045200060510
045300060510       // se "ricerca/scelta" non carico i records annullati
045400060510          if (*in10 and tblflg = *blanks) or not *in10;
045500060510       // scrivo subfile
045600060517           clear vs1opz;
045700060510           exsr sr_wtrs01;
045800060510          endif;
045900060510
046000060510         enddo;
046100060510
046200060510       // fine caricamento subfile
046300060510        endif;
046400060510
046500060510       // disattivo opzione di caricamento subfile
046600060510        wrkcars01 = *off;
046700060510
046800060510       endsr;
046900060510
047000060510       // ----------------------------------------------------------------------
047100060510       // scrivo subfile
047200060510       // ----------------------------------------------------------------------
047300060510       begsr sr_wtrs01;
047400060510
047500060510       // se non raggiunto limite massimo di caricamento
047600060510        if s01nrr < 9999;
047700060510       // imposto campi di subfile
047800060510         exsr sr_sets01;
047900060510       // imposto numeratore righe e numero relativo di record
048000060510         s01nrr = s01nrr + 1;
048100060510       // scrivo subfile
048200090603         write tb5es01;
048300060510        endif;
048400060510
048500060510       endsr;
048600060510
048700060510       // ----------------------------------------------------------------------
048800060510       // imposto campi del subfile
048900060510       // ----------------------------------------------------------------------
049000060510       begsr sr_sets01;
049100060510
049200060522        vs1cod = tblkey;
049300090603        ds5e = tbluni;
049400090603        vs1des = §5edes;
049500090603        vs1ord = §5eord;
049600090603        vs1fub = §5efub;
049700090825        vs1fum = §5efum;
049800090603        vs1fst = §5efst;
049900090603        vs1ftc = §5eftc;
050000090603        vs1d03 = §5ed03;
050100090603        vs1d05 = §5ed05;
050200090603        vs1d08 = §5ed08;
050300060517
050400060517        clear vs1atb;
050500060522        if tblflg <> *blanks;
050600060517         vs1atb = 'A';
050700060517        endif;
050800060510
050900060510       endsr;
051000060517
051100060517       // ----------------------------------------------------------------------
051200060517       // gestione del subfile
051300060517       // ----------------------------------------------------------------------
051400060517       begsr sr_gestsfl;
051500060517
051600060517        wrkeofs01 = *off;
051700060522        clear s01nrr;
051800060523        *in39 = *off;
051900060517
052000060517       // inizio lettura subfile
052100060522        dow wrkeofs01 = *off;
052200060522         s01nrr = s01nrr + 1;
052300090603         chain s01nrr tb5es01;
052400060522
052500060522       // se non trovo esco
052600060522         if not %found;
052700060524          if vc1cod <> *blanks;
052800060524           wrkcars01 = *on;
052900060524          endif;
053000060522          leavesr;
053100060517         endif;
053200060522
053300060517       // se è stata immessa un'opzione
053400060523         if vs1opz <> *zeros;
053500060517       // controllo se opzione valida
053600060517          xx = %lookup(vs1opz:opz);
053700060517          if xx = *zeros;
053800060517           rec01 = s01nrr;
053900060522           vc1msg = msg(03);
054000060517           *in28 = *on;
054100060523           *in39 = *on;
054200090603           update tb5es01;
054300060518           leavesr;
054400060517          endif;
054500060522
054600060517       // reset ds di servizio
054700090603          clear trtb5eds;
054800090603          tb5eopzio = vs1opz;
054900060517
055000060523       // gestione del formato video
055100060522          rec01 = s01nrr;
055200060522          $video = 'D02';
055300060523          exsr sr_d02;
055400060524       // se F12 di ritorno esco dalla lettura del subfile
055500090603          if tb5ericez = 'L';
055600060524          wrkeofs01 = *on;
055700060523          endif;
055800060518         endif;
055900060517
056000060523         clear vs1opz;
056100060522         rec01 = s01nrr;
056200090603         update tb5es01;
056300060517        enddo;
056400060517
056500060517       endsr;
056600090622
056700090622       // ----------------------------------------------------------------------
056800090622       // richiamo pgm gestione limiti tabella LTS
056900090622       // ----------------------------------------------------------------------
057000090622       begsr sr_f08;
057100090622
057200090622        tntb73r(kpjba);
057300090622
057400090622       endsr;
057500060517
057600060517       // ----------------------------------------------------------------------
057700060517       // ripeto opzione in tutte le righe del subfile
057800060517       // ----------------------------------------------------------------------
057900060517       begsr sr_ripetiopz;
058000060517
058100060523        if rrrn01 > 0;
058200060523         s01nrr = rrrn01;
058300090603         chain s01nrr tb5es01;
058400060523         if %found and vs1opz <> *zeros;
058500060522          savopzione = vs1opz;
058600090603          update tb5es01;
058700060517
058800060517          wrkeofs01 = *off;
058900060522          dow wrkeofs01 = *off;
059000060517           s01nrr = s01nrr + 1;
059100090603           chain s01nrr tb5es01;
059200060517           if %found;
059300060522            vs1opz = savopzione;
059400090603            update tb5es01;
059500060517           else;
059600060517            wrkeofs01 = *on;
059700060517           endif;
059800060517          enddo;
059900060517
060000060517         endif;
060100060517
060200060517        endif;
060300060517
060400060517       endsr;
060500060517
060600060517       // ----------------------------------------------------------------------
060700060517       // imposto i dati a video
060800060517       // ----------------------------------------------------------------------
060900060517       begsr sr_setvideo;
061000060517
061100060524        if wrkcard02 = *on;
061200060518       // pulisco il formato video D02
061300060524         exsr sr_puld02;
061400060517
061500060522       // controllo se 'immissione' 'modifica' 'copia' o altro
061600060524         select;
061700060517
061800060517       // F10=Immissione
061900090603          when tb5ecomand = 'J';
062000060524           vt1dopz = 'Immissione';
062100060517
062200060523       // Opzione "02"=modifica
062300090603          when tb5eopzio = 02;
062400060524           vt1dopz = 'Modifica';
062500060524           exsr sr_imposta;
062600060517
062700060523       // Opzione "03"=copia
062800090603          when tb5eopzio = 03;
062900060524           vt1dopz = 'Copia';
063000060524           exsr sr_imposta;
063100060517
063200060523       // Opzione "04"=cancellazione/ripristino
063300090603          when tb5eopzio = 04;
063400060524           exsr sr_imposta;
063500060517       // se richiesta 'cancellazione'
063600060524           if tblflg = *blanks;
063700060524            vt1dopz = 'Annullamento';
063800060524           endif;
063900060517       // se richiesto 'ripristino'
064000060524           if tblflg <> *blanks;
064100060524            vt1dopz = 'Ripristino';
064200060524           endif;
064300060517
064400060523       // Opzione "05"=visualizzazione
064500090603          when tb5eopzio =05;
064600060524           vt1dopz = 'Visualizzazione';
064700060524           exsr sr_imposta;
064800060517
064900060517       // Fine scelta
065000060524         endsl;
065100060517
065200060517       // centro la descrizione della funzione nel formato video
065300060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
065400060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
065500060524
065600060524        endif;
065700060517
065800060517       endsr;
065900060524
066000060524       // ----------------------------------------------------------------------
066100060524       // pulizia video
066200060524       // ----------------------------------------------------------------------
066300060524       begsr sr_puld02;
066400060524
066500060524        clear vt1dopz;
066600060524        clear v2ccod;
066700060524        clear v2cdes;
066800090603        clear v2cd03;
066900090603        clear v2cd05;
067000090603        clear v2cd08;
067100090603        clear v2cddly;
067200090603        clear v2cfub;
067300090825        clear v2cfum;
067400090827        clear v2cttc;
067500090603        clear v2cfst;
067600090603        clear v2cord;
067700090603        clear v2cftc;
067800090603        clear v2dftc;
067900090603        clear v2ctsr;
068000090603        clear v2cuso;
068100090603        clear v2cvas;
068200091030        clear V2Cordint;
068300091030        clear V2Cvasint;
068400060524
068500060524        *in01 = *off;
068600060524        *in02 = *off;
068700060524        *in03 = *off;
068800060524        *in04 = *off;
068900060524        *in05 = *off;
069000060524        *in28 = *off;
069100060524        *in40 = *off;
069200060524        *in41 = *off;
069300060524        *in42 = *off;
069400060524        *in43 = *off;
069500060524        *in44 = *off;
069600060524        *in45 = *off;
069700060524        *in46 = *off;
069800060524        *in47 = *off;
069900060524        *in48 = *off;
070000060524        *in49 = *off;
070100060524        *in50 = *off;
070200060524        *in51 = *off;
070300060524        *in52 = *off;
070400060524        *in53 = *off;
070500060524        *in54 = *off;
070600060524        *in55 = *off;
070700060524
070800060524       endsr;
070900060524
071000060524       // ----------------------------------------------------------------------
071100060524       // imposto i dati a video
071200060524       // ----------------------------------------------------------------------
071300060524       begsr sr_imposta;
071400060524
071500060524       // recupero i dati dal file
071600060524        tblkut = 1;
071700090603        tblcod = '5E';
071800060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
071900060524
072000060524       // imposto i campi a video
072100060524        v2ccod = tblkey;
072200090603        ds5e = tbluni;
072300090603        v2cdes = §5edes;
072400090603        v2cd03 = §5ed03;
072500090603        v2cd05 = §5ed05;
072600090603        v2cd08 = §5ed08;
072700090603        v2cddly = §5eddly;
072800090603        v2cfub = §5efub;
072900090825        v2cfum = §5efum;
073000090827        v2cttc = §5ettc;
073100090603        v2cfst = §5efst;
073200090603        v2cord = §5eord;
073300090603        v2cftc = §5eftc;
073400090603        v2ctsr = §5etsr;
073500090603        v2cuso = §5euso;
073600090603        v2cvas = §5evas;
073700091030        V2Cordint = §5Eordint;
073800091030        V2Cvasint = §5Evasint;
073900060524
074000060524        if tblflg <> *blanks;
074100060524         *in04 = *on;
074200060524        endif;
074300090603
074400090603       // Decodifico tariffa particolare se inserita
074500090603        if v2cftc <> *blanks;
074600090603          tblcod = '1P';
074700090603          chain(n) (tblkut:tblcod:v2cftc) tabel00f;
074800090603          ds1p = tbluni;
074900090603          v2dftc = §1pdes;
075000090603        endif;
075100060524
075200060524       endsr;
075300060517
075400060517       // ----------------------------------------------------------------------
075500060517       // controllo video
075600060517       // ----------------------------------------------------------------------
075700060522       begsr sr_ctrd02;
075800060517
075900060517        *in28 = *off;
076000060517        *in40 = *off;
076100060517        *in41 = *off;
076200060523        *in42 = *off;
076300060524        *in43 = *off;
076400060524        *in44 = *off;
076500060524        *in45 = *off;
076600060524        *in46 = *off;
076700060524        *in47 = *off;
076800060524        *in48 = *off;
076900060524        *in49 = *off;
077000060524        *in50 = *off;
077100060524        *in51 = *off;
077200060524        *in52 = *off;
077300060524        *in53 = *off;
077400060524        *in54 = *off;
077500060524        *in55 = *off;
077600060517
077700060522       // codice tabella
077800060522        if v2ccod = *blanks;
077900060522         v2cmsg = msg(01);
078000060517         *in28 = *on;
078100060517         *in40 = *on;
078200060517         leavesr;
078300060517        endif;
078400060517
078500060517       // se immissione controllo che non esista già
078600090603        if tb5ecomand = 'J' or tb5eopzio = 03;
078700060522         tblkut = 1;
078800090603         tblcod = '5E';
078900060522         tblkey = v2ccod;
079000060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
079100060522         if %found(tabel00f);
079200060522          v2cmsg = msg(05);
079300060517          *in28 = *on;
079400060517          *in40 = *on;
079500060517          leavesr;
079600060517         endif;
079700060517        endif;
079800060517
079900060517       // descrizione
080000060522        if v2cdes = *blanks;
080100060522         v2cmsg = msg(02);
080200060517         *in28 = *on;
080300060517         *in41 = *on;
080400060517         leavesr;
080500060517        endif;
080600060523
080700090603       // descrizioni brevi
080800090603        if v2cd03 = *blanks;
080900090603         v2cmsg = msg(02);
081000090603         *in28 = *on;
081100090603         *in42 = *on;
081200090603         leavesr;
081300090603        endif;
081400090603        if v2cd05 = *blanks;
081500090603         v2cmsg = msg(02);
081600090603         *in28 = *on;
081700090603         *in43 = *on;
081800090603         leavesr;
081900090603        endif;
082000090603        if v2cd08 = *blanks;
082100090603         v2cmsg = msg(02);
082200090603         *in28 = *on;
082300090603         *in44 = *on;
082400090603         leavesr;
082500090603        endif;
082600090603
082700090603       // descrizione stp. delivery time
082800090603        if v2cddly = *blanks;
082900090603         v2cmsg = msg(02);
083000090603         *in28 = *on;
083100090603         *in45 = *on;
083200090603         leavesr;
083300090603        endif;
083400060523
083500090603       // tariffa particolare
083600090603        if v2cftc <> *blanks;
083700090603          exsr sr_ctrftc;
083800060524           if *in28;
083900090603            v2cmsg = msg(04);
084000090603            *in46 = *on;
084100060524            leavesr;
084200060524           endif;
084300090603        endif;
084400090827
084500090827        // codice tempo consegna
084600090827        // se immesso non devono essere spazi vuoti
084700090827        if v2cttc<>*blanks ;
084800090827        Indx=%scan(' ':v2cttc) ;
084900090827        if Indx>0   ;
085000090827            v2cmsg = msg(06);
085100090827            *in47 = *on;
085200090827            *in28 = *on;
085300090827            leavesr;
085400090827           endif;
085500090827        Indx=%check(caratteri:V2cttc) ;
085600090827        if Indx>0   ;
085700090827            v2cmsg = msg(07);
085800090827            *in47 = *on;
085900090827            *in28 = *on;
086000090827            leavesr;
086100090827           endif;
086200090827         endif;
086300061212
086400061212       // riaggancio di nuovo la tabella xchè mi sono sposizionata
086500061212        tblkut = 1;
086600061212        tblcod = '1P';
086700061212        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
086800060517
086900060517       endsr;
087000060524
087100060524       // ----------------------------------------------------------------------
087200090603       // controllo tariffa particolare
087300060524       // ----------------------------------------------------------------------
087400090603       begsr sr_ctrftc;
087500060524
087600060524        tblkut = 1;
087700090603        tblcod = '1P';
087800090603        tblkey = v2cftc;
087900060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
088000060524        if not %found(tabel00f) or tblflg <> *blanks;
088100090603          *in28 = *on;
088200090603          leavesr;
088300060524        endif;
088400090603        ds1p = tbluni;
088500090603        v2dftc = §1pdes;
088600060524
088700060524       endsr;
088800060524
088900060524       // ----------------------------------------------------------------------
089000060524       // gestione video dati di trasmissione
089100060524       // ----------------------------------------------------------------------
089200060524       begsr sr_w01;
089300060524
089400060524       // imposto i dati a video
089500060524        exsr sr_carw01;
089600060524
089700060524       // fino a che la variabile resta settata come 'W01'
089800060524        dow $video = 'W01';
089900060524
090000060524       // reset indicatore generico di errore
090100060524         *in28 = *off;
090200060524
090300060524       // emetto il video
090400090603          exfmt tb5ew01;
090500060524
090600060524       // controllo tasti funzionali del video
090700060524         select;
090800060524
090900060524       // F6=Conferma
091000060524          when *inkf;
091100060524       // controllo i dati del video
091200060524           exsr sr_ctrw01;
091300060524       // se non riscontrati errori aggiorno il record corrente
091400060524           if not *in28;
091500060524            exsr sr_aggiorna;
091600060524            if not *in28;
091700060524       // emetto le descrizioni in lingua
091800060524       // se non è annullo/ripristino
091900090603             if tb5eopzio <> 04;
092000060524              *in05 = *off;
092100060524              $video = 'C02';
092200060524              exsr sr_c02;
092300060524              leavesr;
092400060524             endif;
092500090603             if tb5ecomand = 'J';
092600060524              $video = 'D02';
092700060524              exsr sr_setvideo;
092800060524             else;
092900060524              wrkcars01 = *on;
093000060524              $video = 'C01';
093100060524             endif;
093200060524            endif;
093300060524           endif;
093400060524
093500060524       // F12=Ritorno
093600060524          when *inkl;
093700060524           $video = 'D02';
093800090603           clear tb5ericez;
093900090603           tb5ericez = 'L';
094000061212           wrkcard02 = *off;
094100060524
094200060524       // Invio
094300060524          other;
094400060524           exsr sr_ctrw01;
094500060524
094600060524         endsl;
094700060524
094800060524       // fine gestione 'W01'
094900060524        enddo;
095000060524
095100060524       endsr;
095200060517
095300060517       // ----------------------------------------------------------------------
095400060517       // imposto i dati di trasmissione
095500060517       // ----------------------------------------------------------------------
095600060517       begsr sr_carw01;
095700060517
095800060517       // se pulisco i campi
095900060522         clear w01ftt;
096000060522         clear w01flt;
096100060522         clear w01ftr;
096200060522         clear w01dtr;
096300060517
096400060517       // se non immissione imposto i campi del file
096500090603        if tb5ecomand <> 'J';
096600060522         w01ftt = tblftt;
096700060522         w01flt = tblflt;
096800060522         w01ftr = tblftr;
096900060517       // imposto la data
097000060522         if tbldtr <> *zeros;
097100060523          datar = tbldtr;
097200060523          aad = aar;
097300060523          mmd = mmr;
097400060523          ggd = ggr;
097500060523          w01dtr = datad;
097600060517         endif;
097700060517
097800060517        endif;
097900060517
098000060517       endsr;
098100060517
098200060517       // ----------------------------------------------------------------------
098300060517       // controllo i dati di trasmissione
098400060517       // ----------------------------------------------------------------------
098500060517       begsr sr_ctrw01;
098600060517
098700060517       endsr;
098800060517
098900060517       // ----------------------------------------------------------------------
099000060517       // aggiorno tabella
099100060517       // ----------------------------------------------------------------------
099200060517       begsr sr_aggiorna;
099300060517
099400060517       // imposto campi del file
099500060522        tblkut = 1;
099600090603        tblcod = '5E';
099700060522        tblkey = v2ccod;
099800060523
099900060523       // aggancio il rcd della tabella
100000060523        chain (tblkut:tblcod:tblkey) tabel00f;
100100060517
100200090603        §5edes = v2cdes;
100300090603        §5ed03 = v2cd03;
100400090603        §5ed05 = v2cd05;
100500090603        §5ed08 = v2cd08;
100600090603        §5eddly = v2cddly;
100700090603        §5efub = v2cfub;
100800090825        §5efum = v2cfum;
100900090827        §5ettc = v2cttc;
101000090603        §5efst = v2cfst;
101100090603        §5eord = v2cord;
101200090603        §5eftc = v2cftc;
101300090603        §5etsr = v2ctsr;
101400090603        §5euso = v2cuso;
101500090603        §5evas = v2cvas;
101600091030        §5Eordint = V2Cordint;
101700091030        §5Evasint = V2Cvasint;
101800060517
101900090603        tbluni = ds5e;
102000060517
102100060522        tblftt = w01ftt;
102200060522        tblflt = w01flt;
102300060522        clear tblftr;
102400060522        clear tbldtr;
102500060524
102600060524       // annullo/ripristino
102700090603        if tb5eopzio = 04;
102800060524         select;
102900060524       // annullamento
103000060524          when tblflg = *blanks;
103100060524           tblflg = '*';
103200060524       // ripristino
103300060524          when tblflg <> *blanks;
103400060524           clear tblflg;
103500060524         endsl;
103600060524        endif;
103700060517
103800060523       // controllo se devo scrivere o aggiornare il record
103900060523        if %found(tabel00f);
104000060523         update tabel;
104100060523        else;
104200060523         write tabel;
104300060523        endif;
104400060517
104500060523       // se Opzione "04"=cancellazione/ripristino
104600060517       // aggiorno automaticamente anche le descrizioni in linea
104700090603        if tb5eopzio = 04;
104800060522         lintabel = 1;
104900060522         setgt lintabel azlin01l;
105000060522         dou %eof(azlin01l);
105100060522          read azlin01l;
105200060517       // fine file
105300060522          if %eof(azlin01l);
105400060517           leave;
105500060517          endif;
105600060517       // aggiorno
105700060522          tblkut = lintabel;
105800060522          chain (tblkut:tblcod:tblkey) tabel00f;
105900060522          if %found(tabel00f);
106000060517           select;
106100060522            when tblflg <> *blanks;
106200060522             clear tblflg;
106300060523            when tblflg = *blanks;
106400060522             tblflg = '*';
106500060517           endsl;
106600060522            update tabel;
106700060517          endif;
106800060517         enddo;
106900060517        endif;
107000060517
107100060517       endsr;
107200060524
107300060524       // ----------------------------------------------------------------------
107400060524       // gestione descrizioni in lingua
107500060524       // ----------------------------------------------------------------------
107600060524       begsr sr_c02;
107700060524
107800060524       // imposto il codice tabella e la descrizione
107900060524       vc2cod = v2ccod;
108000060524       vc2des = v2cdes;
108100060524
108200060524       // pulisco il subfile
108300060524        exsr sr_puls02;
108400060524
108500060524       // carico il subfile con i dati che trovo o vuoto
108600060524        exsr sr_cars02;
108700060524
108800060524        wrksfl02 = *on;
108900060524
109000060524        dou wrksfl02 = *off;
109100060524         rec02 = 1;
109200060524         if savrec > *zeros;
109300060524          rec02 = savrec;
109400060524         endif;
109500090603         write tb5ez02;
109600090603         exfmt tb5ec02;
109700060524
109800060524         *in28 = *off;
109900060524         *in39 = *off;
110000060524
110100060524       // F12=Ritorno
110200060524          if *inkl;
110300060524           wrkcard02 = *off;
110400060524           $video = 'D02';
110500060524           leavesr;
110600060524          endif;
110700060524
110800060524       // Controlli
110900060524          if not *in05;
111000060524           exsr sr_ctrsfl;
111100060524          endif;
111200060524
111300060524       // F6=Conferma
111400060524          if *inkf and not *in28;
111500060524           exsr sr_confsfl;
111600060524           wrksfl02 = *off;
111700060524          endif;
111800060524
111900060524         enddo;
112000060524
112100060524       endsr;
112200060524
112300060524       // ----------------------------------------------------------------------
112400060524       // pulisco il subfile delle descrizioni in lingua
112500060524       // ----------------------------------------------------------------------
112600060524       begsr sr_puls02;
112700060524
112800060524        clear s02nrr;
112900060524        clear savrec;
113000060524        *in20 = *off;
113100060524        *in21 = *off;
113200090603        write tb5ec02;
113300060524        *in20 = *on;
113400060524        *in21 = *on;
113500060524
113600060524       endsr;
113700060524
113800060524       // ----------------------------------------------------------------------
113900060524       // carico il subfile delle descrizioni in lingua
114000060524       // ----------------------------------------------------------------------
114100060524       begsr sr_cars02;
114200060524
114300060524        *in22 = *off;
114400060524       // carico un record per ogni lingua inserite in AZLIN
114500060524        lintabel = 1;
114600060524        setgt lintabel azlin01l;
114700060524        dou %eof(azlin01l);
114800060524         read azlin01l;
114900060524       // fine file
115000060524         if %eof(azlin01l);
115100060524          leave;
115200060524         endif;
115300090603         clear ds5e;
115400060524         s2dlin = lindesita;
115500060524         tblkut = lintabel;
115600090603         tblcod = '5E';
115700060524         tblkey = v2ccod;
115800060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
115900060524         if not %found(tabel00f);
116000060524          clear s2cdes;
116100090603          clear s2cd03;
116200090603          clear s2cd05;
116300090603          clear s2cd08;
116400090603          clear s2cddly;
116500060524         else;
116600090603          ds5e = tbluni;
116700090603          s2cdes = §5edes;
116800090603          s2cddly = §5eddly;
116900090603          s2cd03 = §5ed03;
117000090603          s2cd05 = §5ed05;
117100090603          s2cd08 = §5ed08;
117200060524         endif;
117300060524         s2hcdlin = tblkut;
117400060524         s02nrr = s02nrr + 1;
117500090603         write tb5es02;
117600060524        enddo;
117700060524
117800060524        *in22 = *on;
117900060524
118000060524       endsr;
118100060524
118200060524       // ----------------------------------------------------------------------
118300060524       // controllo il subfile delle descrizioni in lingua
118400060524       // ----------------------------------------------------------------------
118500060524       begsr sr_ctrsfl;
118600060524
118700060524        s02nrr = 1;
118800090603        chain s02nrr tb5es02;
118900060524        dow %found;
119000060524       // deve esserci la descrizione
119100090603         if s2cdes = *blanks or s2cd03 = *blanks or
119200090603            s2cd05 = *blanks or s2cd08 = *blanks or
119300090603            s2cddly = *blanks;
119400060524          vc2msg = msg(02);
119500060524          *in28 = *on;
119600060524          *in39 = *on;
119700060524          savrec = s02nrr;
119800090603          update tb5es02;
119900060524          leave;
120000060524         endif;
120100090603         update tb5es02;
120200060524         s02nrr = s02nrr + 1;
120300090603         chain s02nrr tb5es02;
120400060524        enddo;
120500060524
120600060524       endsr;
120700060524
120800060524       // ----------------------------------------------------------------------
120900060524       // confermo il dati del subfile delle descrizioni in lingua
121000060524       // ----------------------------------------------------------------------
121100060524       begsr sr_confsfl;
121200060524
121300060524       // recupero i dati della tabella in italiano
121400060524        clear savtbluni;
121500060524        tblkut = 1;
121600090603        tblcod = '5E';
121700060524        tblkey = v2ccod;
121800060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
121900060524        if %found(tabel00f);
122000060524         savtbluni = tbluni;
122100060524        endif;
122200060524
122300060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
122400060524        s02nrr = 1;
122500090603        chain s02nrr tb5es02;
122600060524        dow %found;
122700060524         tblkut = s2hcdlin;
122800060524         chain (tblkut:tblcod:tblkey) tabel00f;
122900090603         ds5e = savtbluni;
123000090603         §5edes = s2cdes;
123100090603         §5ed03 = s2cd03;
123200090603         §5ed05 = s2cd05;
123300090603         §5ed08 = s2cd08;
123400090603         §5eddly = s2cddly;
123500090603         tbluni = ds5e;
123600060524         tblftt = w01ftt;
123700060524         clear tblftr;
123800060524         clear tbldtr;
123900060524       // scrivo
124000060524         if not %found(tabel00f);
124100060524          tblkut = s2hcdlin;
124200090603          tblcod = '5E';
124300060524          tblkey = v2ccod;
124400060524          write tabel;
124500060524         else;
124600060524       // aggiorno
124700060524          update tabel;
124800060524         endif;
124900060524         s02nrr = s02nrr + 1;
125000090603         chain s02nrr tb5es02;
125100060524        enddo;
125200060524
125300060524        wrkcars01 = *on;
125400060524        $video = 'C01';
125500060524
125600060524       endsr;
125700060524
125800060524       // ----------------------------------------------------------------------
125900060524       // uscita dal programma
126000060524       // ----------------------------------------------------------------------
126100060524         begsr sr_uscita;
126200060524
126300090622       // se F3 controllo che tutti i tipi servizio siano presenti nella tabella
126400090622       // LTS - Limiti
126500090622          if *inkc;
126600090622       // leggo tabella 5E
126700090622           tblkut = 1;
126800090622           tblcod = '5E';
126900090622           setll (tblkut:tblcod) tabel00f;
127000090622           dou %eof(tabel00f);
127100090622             reade(n) (tblkut:tblcod) tabel00f;
127200090622             if %eof(tabel00f);
127300090622               leave;
127400090622             endif;
127500090622       // per ogni tipo servizio controllo se esiste tabella LTS
127600090622             clear tibs02ds;
127700090622             t02mod = 'C';
127800090622             t02cod = 'LTS';
127900090622             t02ke1 = tblkey;
128000090622             t02sif = knsif;
128100090622             TNTBE_RicercaControllo(kpjba:tibs02ds);
128200090622             if t02err = *blanks;
128300090622             else;
128400090622             $video = 'C01';
128500090622             *in28 = *on;
128600090622             vc1msg = msg(05);
128700090622             %subst(vc1msg:22:1) = %subst(tblkey:1:1);
128800090622             leavesr;
128900090622             endif;
129000090622           enddo;
129100090622          endif;
129200090622
129300090603          if tb5eesito = *blanks;
129400090603           tb5eesito = eseguito;
129500060524          endif;
129600060524
129700090603          kpjbu = trtb5eds;
129800060524
129900060524          *inlr = *on;
130000060524          return;
130100060524
130200060524         endsr;
130300060510
130400060510       // ----------------------------------------------------------------------
130500060510       // routine iniziale
130600060510       // ----------------------------------------------------------------------
130700060510         begsr *inzsr;
130800060510
130900060510      /end-free
131000060510
131100060510     c     *entry        plist
131200060510     c                   parm                    kpjba
131300060510
131400060510      /free
131500060510         in(e) §azute;
131600060510         if not %error;
131700060510          in(e) §datiute;
131800060510         endif;
131900060510         if %error or rsut = *blanks;
132000060510          tibs34r(tibs34ds);
132100060510          in §azute;
132200060510          in §datiute;
132300060510         endif;
132400060510
132500060510       // centro il titolo
132600060510        vt1tit = titolo;
132700060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
132800060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
132900060510
133000060510         endsr;
133100060510
133200060510      /end-free
133300060418
133400060418** -- MSG -------------------------------------------------------------------*
133500060418Immettere il codice                                                            1
133600060418Immettere la descrizione                                                       2
133700060522Opzione errata                                                                 3
133800090622Tariffa particolare errata                                                     4
133900090622Per il tipo servizio X non è presente la relativa tabella LTS - Limiti         5
134000090827Immettere il codice tempo di consegna NUMERICO senza spazi vuoti               6
134100090827Immettere un "NUMERO" per la parte fissa, una "x" per la parte variabile       6
