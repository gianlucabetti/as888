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
068200060524
068300060524        *in01 = *off;
068400060524        *in02 = *off;
068500060524        *in03 = *off;
068600060524        *in04 = *off;
068700060524        *in05 = *off;
068800060524        *in28 = *off;
068900060524        *in40 = *off;
069000060524        *in41 = *off;
069100060524        *in42 = *off;
069200060524        *in43 = *off;
069300060524        *in44 = *off;
069400060524        *in45 = *off;
069500060524        *in46 = *off;
069600060524        *in47 = *off;
069700060524        *in48 = *off;
069800060524        *in49 = *off;
069900060524        *in50 = *off;
070000060524        *in51 = *off;
070100060524        *in52 = *off;
070200060524        *in53 = *off;
070300060524        *in54 = *off;
070400060524        *in55 = *off;
070500060524
070600060524       endsr;
070700060524
070800060524       // ----------------------------------------------------------------------
070900060524       // imposto i dati a video
071000060524       // ----------------------------------------------------------------------
071100060524       begsr sr_imposta;
071200060524
071300060524       // recupero i dati dal file
071400060524        tblkut = 1;
071500090603        tblcod = '5E';
071600060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
071700060524
071800060524       // imposto i campi a video
071900060524        v2ccod = tblkey;
072000090603        ds5e = tbluni;
072100090603        v2cdes = §5edes;
072200090603        v2cd03 = §5ed03;
072300090603        v2cd05 = §5ed05;
072400090603        v2cd08 = §5ed08;
072500090603        v2cddly = §5eddly;
072600090603        v2cfub = §5efub;
072700090825        v2cfum = §5efum;
072800090827        v2cttc = §5ettc;
072900090603        v2cfst = §5efst;
073000090603        v2cord = §5eord;
073100090603        v2cftc = §5eftc;
073200090603        v2ctsr = §5etsr;
073300090603        v2cuso = §5euso;
073400090603        v2cvas = §5evas;
073500060524
073600060524        if tblflg <> *blanks;
073700060524         *in04 = *on;
073800060524        endif;
073900090603
074000090603       // Decodifico tariffa particolare se inserita
074100090603        if v2cftc <> *blanks;
074200090603          tblcod = '1P';
074300090603          chain(n) (tblkut:tblcod:v2cftc) tabel00f;
074400090603          ds1p = tbluni;
074500090603          v2dftc = §1pdes;
074600090603        endif;
074700060524
074800060524       endsr;
074900060517
075000060517       // ----------------------------------------------------------------------
075100060517       // controllo video
075200060517       // ----------------------------------------------------------------------
075300060522       begsr sr_ctrd02;
075400060517
075500060517        *in28 = *off;
075600060517        *in40 = *off;
075700060517        *in41 = *off;
075800060523        *in42 = *off;
075900060524        *in43 = *off;
076000060524        *in44 = *off;
076100060524        *in45 = *off;
076200060524        *in46 = *off;
076300060524        *in47 = *off;
076400060524        *in48 = *off;
076500060524        *in49 = *off;
076600060524        *in50 = *off;
076700060524        *in51 = *off;
076800060524        *in52 = *off;
076900060524        *in53 = *off;
077000060524        *in54 = *off;
077100060524        *in55 = *off;
077200060517
077300060522       // codice tabella
077400060522        if v2ccod = *blanks;
077500060522         v2cmsg = msg(01);
077600060517         *in28 = *on;
077700060517         *in40 = *on;
077800060517         leavesr;
077900060517        endif;
078000060517
078100060517       // se immissione controllo che non esista già
078200090603        if tb5ecomand = 'J' or tb5eopzio = 03;
078300060522         tblkut = 1;
078400090603         tblcod = '5E';
078500060522         tblkey = v2ccod;
078600060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
078700060522         if %found(tabel00f);
078800060522          v2cmsg = msg(05);
078900060517          *in28 = *on;
079000060517          *in40 = *on;
079100060517          leavesr;
079200060517         endif;
079300060517        endif;
079400060517
079500060517       // descrizione
079600060522        if v2cdes = *blanks;
079700060522         v2cmsg = msg(02);
079800060517         *in28 = *on;
079900060517         *in41 = *on;
080000060517         leavesr;
080100060517        endif;
080200060523
080300090603       // descrizioni brevi
080400090603        if v2cd03 = *blanks;
080500090603         v2cmsg = msg(02);
080600090603         *in28 = *on;
080700090603         *in42 = *on;
080800090603         leavesr;
080900090603        endif;
081000090603        if v2cd05 = *blanks;
081100090603         v2cmsg = msg(02);
081200090603         *in28 = *on;
081300090603         *in43 = *on;
081400090603         leavesr;
081500090603        endif;
081600090603        if v2cd08 = *blanks;
081700090603         v2cmsg = msg(02);
081800090603         *in28 = *on;
081900090603         *in44 = *on;
082000090603         leavesr;
082100090603        endif;
082200090603
082300090603       // descrizione stp. delivery time
082400090603        if v2cddly = *blanks;
082500090603         v2cmsg = msg(02);
082600090603         *in28 = *on;
082700090603         *in45 = *on;
082800090603         leavesr;
082900090603        endif;
083000060523
083100090603       // tariffa particolare
083200090603        if v2cftc <> *blanks;
083300090603          exsr sr_ctrftc;
083400060524           if *in28;
083500090603            v2cmsg = msg(04);
083600090603            *in46 = *on;
083700060524            leavesr;
083800060524           endif;
083900090603        endif;
084000090827
084100090827        // codice tempo consegna
084200090827        // se immesso non devono essere spazi vuoti
084300090827        if v2cttc<>*blanks ;
084400090827        Indx=%scan(' ':v2cttc) ;
084500090827        if Indx>0   ;
084600090827            v2cmsg = msg(06);
084700090827            *in47 = *on;
084800090827            *in28 = *on;
084900090827            leavesr;
085000090827           endif;
085100090827        Indx=%check(caratteri:V2cttc) ;
085200090827        if Indx>0   ;
085300090827            v2cmsg = msg(07);
085400090827            *in47 = *on;
085500090827            *in28 = *on;
085600090827            leavesr;
085700090827           endif;
085800090827         endif;
085900061212
086000061212       // riaggancio di nuovo la tabella xchè mi sono sposizionata
086100061212        tblkut = 1;
086200061212        tblcod = '1P';
086300061212        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
086400060517
086500060517       endsr;
086600060524
086700060524       // ----------------------------------------------------------------------
086800090603       // controllo tariffa particolare
086900060524       // ----------------------------------------------------------------------
087000090603       begsr sr_ctrftc;
087100060524
087200060524        tblkut = 1;
087300090603        tblcod = '1P';
087400090603        tblkey = v2cftc;
087500060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
087600060524        if not %found(tabel00f) or tblflg <> *blanks;
087700090603          *in28 = *on;
087800090603          leavesr;
087900060524        endif;
088000090603        ds1p = tbluni;
088100090603        v2dftc = §1pdes;
088200060524
088300060524       endsr;
088400060524
088500060524       // ----------------------------------------------------------------------
088600060524       // gestione video dati di trasmissione
088700060524       // ----------------------------------------------------------------------
088800060524       begsr sr_w01;
088900060524
089000060524       // imposto i dati a video
089100060524        exsr sr_carw01;
089200060524
089300060524       // fino a che la variabile resta settata come 'W01'
089400060524        dow $video = 'W01';
089500060524
089600060524       // reset indicatore generico di errore
089700060524         *in28 = *off;
089800060524
089900060524       // emetto il video
090000090603          exfmt tb5ew01;
090100060524
090200060524       // controllo tasti funzionali del video
090300060524         select;
090400060524
090500060524       // F6=Conferma
090600060524          when *inkf;
090700060524       // controllo i dati del video
090800060524           exsr sr_ctrw01;
090900060524       // se non riscontrati errori aggiorno il record corrente
091000060524           if not *in28;
091100060524            exsr sr_aggiorna;
091200060524            if not *in28;
091300060524       // emetto le descrizioni in lingua
091400060524       // se non è annullo/ripristino
091500090603             if tb5eopzio <> 04;
091600060524              *in05 = *off;
091700060524              $video = 'C02';
091800060524              exsr sr_c02;
091900060524              leavesr;
092000060524             endif;
092100090603             if tb5ecomand = 'J';
092200060524              $video = 'D02';
092300060524              exsr sr_setvideo;
092400060524             else;
092500060524              wrkcars01 = *on;
092600060524              $video = 'C01';
092700060524             endif;
092800060524            endif;
092900060524           endif;
093000060524
093100060524       // F12=Ritorno
093200060524          when *inkl;
093300060524           $video = 'D02';
093400090603           clear tb5ericez;
093500090603           tb5ericez = 'L';
093600061212           wrkcard02 = *off;
093700060524
093800060524       // Invio
093900060524          other;
094000060524           exsr sr_ctrw01;
094100060524
094200060524         endsl;
094300060524
094400060524       // fine gestione 'W01'
094500060524        enddo;
094600060524
094700060524       endsr;
094800060517
094900060517       // ----------------------------------------------------------------------
095000060517       // imposto i dati di trasmissione
095100060517       // ----------------------------------------------------------------------
095200060517       begsr sr_carw01;
095300060517
095400060517       // se pulisco i campi
095500060522         clear w01ftt;
095600060522         clear w01flt;
095700060522         clear w01ftr;
095800060522         clear w01dtr;
095900060517
096000060517       // se non immissione imposto i campi del file
096100090603        if tb5ecomand <> 'J';
096200060522         w01ftt = tblftt;
096300060522         w01flt = tblflt;
096400060522         w01ftr = tblftr;
096500060517       // imposto la data
096600060522         if tbldtr <> *zeros;
096700060523          datar = tbldtr;
096800060523          aad = aar;
096900060523          mmd = mmr;
097000060523          ggd = ggr;
097100060523          w01dtr = datad;
097200060517         endif;
097300060517
097400060517        endif;
097500060517
097600060517       endsr;
097700060517
097800060517       // ----------------------------------------------------------------------
097900060517       // controllo i dati di trasmissione
098000060517       // ----------------------------------------------------------------------
098100060517       begsr sr_ctrw01;
098200060517
098300060517       endsr;
098400060517
098500060517       // ----------------------------------------------------------------------
098600060517       // aggiorno tabella
098700060517       // ----------------------------------------------------------------------
098800060517       begsr sr_aggiorna;
098900060517
099000060517       // imposto campi del file
099100060522        tblkut = 1;
099200090603        tblcod = '5E';
099300060522        tblkey = v2ccod;
099400060523
099500060523       // aggancio il rcd della tabella
099600060523        chain (tblkut:tblcod:tblkey) tabel00f;
099700060517
099800090603        §5edes = v2cdes;
099900090603        §5ed03 = v2cd03;
100000090603        §5ed05 = v2cd05;
100100090603        §5ed08 = v2cd08;
100200090603        §5eddly = v2cddly;
100300090603        §5efub = v2cfub;
100400090825        §5efum = v2cfum;
100500090827        §5ettc = v2cttc;
100600090603        §5efst = v2cfst;
100700090603        §5eord = v2cord;
100800090603        §5eftc = v2cftc;
100900090603        §5etsr = v2ctsr;
101000090603        §5euso = v2cuso;
101100090603        §5evas = v2cvas;
101200060517
101300090603        tbluni = ds5e;
101400060517
101500060522        tblftt = w01ftt;
101600060522        tblflt = w01flt;
101700060522        clear tblftr;
101800060522        clear tbldtr;
101900060524
102000060524       // annullo/ripristino
102100090603        if tb5eopzio = 04;
102200060524         select;
102300060524       // annullamento
102400060524          when tblflg = *blanks;
102500060524           tblflg = '*';
102600060524       // ripristino
102700060524          when tblflg <> *blanks;
102800060524           clear tblflg;
102900060524         endsl;
103000060524        endif;
103100060517
103200060523       // controllo se devo scrivere o aggiornare il record
103300060523        if %found(tabel00f);
103400060523         update tabel;
103500060523        else;
103600060523         write tabel;
103700060523        endif;
103800060517
103900060523       // se Opzione "04"=cancellazione/ripristino
104000060517       // aggiorno automaticamente anche le descrizioni in linea
104100090603        if tb5eopzio = 04;
104200060522         lintabel = 1;
104300060522         setgt lintabel azlin01l;
104400060522         dou %eof(azlin01l);
104500060522          read azlin01l;
104600060517       // fine file
104700060522          if %eof(azlin01l);
104800060517           leave;
104900060517          endif;
105000060517       // aggiorno
105100060522          tblkut = lintabel;
105200060522          chain (tblkut:tblcod:tblkey) tabel00f;
105300060522          if %found(tabel00f);
105400060517           select;
105500060522            when tblflg <> *blanks;
105600060522             clear tblflg;
105700060523            when tblflg = *blanks;
105800060522             tblflg = '*';
105900060517           endsl;
106000060522            update tabel;
106100060517          endif;
106200060517         enddo;
106300060517        endif;
106400060517
106500060517       endsr;
106600060524
106700060524       // ----------------------------------------------------------------------
106800060524       // gestione descrizioni in lingua
106900060524       // ----------------------------------------------------------------------
107000060524       begsr sr_c02;
107100060524
107200060524       // imposto il codice tabella e la descrizione
107300060524       vc2cod = v2ccod;
107400060524       vc2des = v2cdes;
107500060524
107600060524       // pulisco il subfile
107700060524        exsr sr_puls02;
107800060524
107900060524       // carico il subfile con i dati che trovo o vuoto
108000060524        exsr sr_cars02;
108100060524
108200060524        wrksfl02 = *on;
108300060524
108400060524        dou wrksfl02 = *off;
108500060524         rec02 = 1;
108600060524         if savrec > *zeros;
108700060524          rec02 = savrec;
108800060524         endif;
108900090603         write tb5ez02;
109000090603         exfmt tb5ec02;
109100060524
109200060524         *in28 = *off;
109300060524         *in39 = *off;
109400060524
109500060524       // F12=Ritorno
109600060524          if *inkl;
109700060524           wrkcard02 = *off;
109800060524           $video = 'D02';
109900060524           leavesr;
110000060524          endif;
110100060524
110200060524       // Controlli
110300060524          if not *in05;
110400060524           exsr sr_ctrsfl;
110500060524          endif;
110600060524
110700060524       // F6=Conferma
110800060524          if *inkf and not *in28;
110900060524           exsr sr_confsfl;
111000060524           wrksfl02 = *off;
111100060524          endif;
111200060524
111300060524         enddo;
111400060524
111500060524       endsr;
111600060524
111700060524       // ----------------------------------------------------------------------
111800060524       // pulisco il subfile delle descrizioni in lingua
111900060524       // ----------------------------------------------------------------------
112000060524       begsr sr_puls02;
112100060524
112200060524        clear s02nrr;
112300060524        clear savrec;
112400060524        *in20 = *off;
112500060524        *in21 = *off;
112600090603        write tb5ec02;
112700060524        *in20 = *on;
112800060524        *in21 = *on;
112900060524
113000060524       endsr;
113100060524
113200060524       // ----------------------------------------------------------------------
113300060524       // carico il subfile delle descrizioni in lingua
113400060524       // ----------------------------------------------------------------------
113500060524       begsr sr_cars02;
113600060524
113700060524        *in22 = *off;
113800060524       // carico un record per ogni lingua inserite in AZLIN
113900060524        lintabel = 1;
114000060524        setgt lintabel azlin01l;
114100060524        dou %eof(azlin01l);
114200060524         read azlin01l;
114300060524       // fine file
114400060524         if %eof(azlin01l);
114500060524          leave;
114600060524         endif;
114700090603         clear ds5e;
114800060524         s2dlin = lindesita;
114900060524         tblkut = lintabel;
115000090603         tblcod = '5E';
115100060524         tblkey = v2ccod;
115200060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
115300060524         if not %found(tabel00f);
115400060524          clear s2cdes;
115500090603          clear s2cd03;
115600090603          clear s2cd05;
115700090603          clear s2cd08;
115800090603          clear s2cddly;
115900060524         else;
116000090603          ds5e = tbluni;
116100090603          s2cdes = §5edes;
116200090603          s2cddly = §5eddly;
116300090603          s2cd03 = §5ed03;
116400090603          s2cd05 = §5ed05;
116500090603          s2cd08 = §5ed08;
116600060524         endif;
116700060524         s2hcdlin = tblkut;
116800060524         s02nrr = s02nrr + 1;
116900090603         write tb5es02;
117000060524        enddo;
117100060524
117200060524        *in22 = *on;
117300060524
117400060524       endsr;
117500060524
117600060524       // ----------------------------------------------------------------------
117700060524       // controllo il subfile delle descrizioni in lingua
117800060524       // ----------------------------------------------------------------------
117900060524       begsr sr_ctrsfl;
118000060524
118100060524        s02nrr = 1;
118200090603        chain s02nrr tb5es02;
118300060524        dow %found;
118400060524       // deve esserci la descrizione
118500090603         if s2cdes = *blanks or s2cd03 = *blanks or
118600090603            s2cd05 = *blanks or s2cd08 = *blanks or
118700090603            s2cddly = *blanks;
118800060524          vc2msg = msg(02);
118900060524          *in28 = *on;
119000060524          *in39 = *on;
119100060524          savrec = s02nrr;
119200090603          update tb5es02;
119300060524          leave;
119400060524         endif;
119500090603         update tb5es02;
119600060524         s02nrr = s02nrr + 1;
119700090603         chain s02nrr tb5es02;
119800060524        enddo;
119900060524
120000060524       endsr;
120100060524
120200060524       // ----------------------------------------------------------------------
120300060524       // confermo il dati del subfile delle descrizioni in lingua
120400060524       // ----------------------------------------------------------------------
120500060524       begsr sr_confsfl;
120600060524
120700060524       // recupero i dati della tabella in italiano
120800060524        clear savtbluni;
120900060524        tblkut = 1;
121000090603        tblcod = '5E';
121100060524        tblkey = v2ccod;
121200060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
121300060524        if %found(tabel00f);
121400060524         savtbluni = tbluni;
121500060524        endif;
121600060524
121700060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
121800060524        s02nrr = 1;
121900090603        chain s02nrr tb5es02;
122000060524        dow %found;
122100060524         tblkut = s2hcdlin;
122200060524         chain (tblkut:tblcod:tblkey) tabel00f;
122300090603         ds5e = savtbluni;
122400090603         §5edes = s2cdes;
122500090603         §5ed03 = s2cd03;
122600090603         §5ed05 = s2cd05;
122700090603         §5ed08 = s2cd08;
122800090603         §5eddly = s2cddly;
122900090603         tbluni = ds5e;
123000060524         tblftt = w01ftt;
123100060524         clear tblftr;
123200060524         clear tbldtr;
123300060524       // scrivo
123400060524         if not %found(tabel00f);
123500060524          tblkut = s2hcdlin;
123600090603          tblcod = '5E';
123700060524          tblkey = v2ccod;
123800060524          write tabel;
123900060524         else;
124000060524       // aggiorno
124100060524          update tabel;
124200060524         endif;
124300060524         s02nrr = s02nrr + 1;
124400090603         chain s02nrr tb5es02;
124500060524        enddo;
124600060524
124700060524        wrkcars01 = *on;
124800060524        $video = 'C01';
124900060524
125000060524       endsr;
125100060524
125200060524       // ----------------------------------------------------------------------
125300060524       // uscita dal programma
125400060524       // ----------------------------------------------------------------------
125500060524         begsr sr_uscita;
125600060524
125700090622       // se F3 controllo che tutti i tipi servizio siano presenti nella tabella
125800090622       // LTS - Limiti
125900090622          if *inkc;
126000090622       // leggo tabella 5E
126100090622           tblkut = 1;
126200090622           tblcod = '5E';
126300090622           setll (tblkut:tblcod) tabel00f;
126400090622           dou %eof(tabel00f);
126500090622             reade(n) (tblkut:tblcod) tabel00f;
126600090622             if %eof(tabel00f);
126700090622               leave;
126800090622             endif;
126900090622       // per ogni tipo servizio controllo se esiste tabella LTS
127000090622             clear tibs02ds;
127100090622             t02mod = 'C';
127200090622             t02cod = 'LTS';
127300090622             t02ke1 = tblkey;
127400090622             t02sif = knsif;
127500090622             TNTBE_RicercaControllo(kpjba:tibs02ds);
127600090622             if t02err = *blanks;
127700090622             else;
127800090622             $video = 'C01';
127900090622             *in28 = *on;
128000090622             vc1msg = msg(05);
128100090622             %subst(vc1msg:22:1) = %subst(tblkey:1:1);
128200090622             leavesr;
128300090622             endif;
128400090622           enddo;
128500090622          endif;
128600090622
128700090603          if tb5eesito = *blanks;
128800090603           tb5eesito = eseguito;
128900060524          endif;
129000060524
129100090603          kpjbu = trtb5eds;
129200060524
129300060524          *inlr = *on;
129400060524          return;
129500060524
129600060524         endsr;
129700060510
129800060510       // ----------------------------------------------------------------------
129900060510       // routine iniziale
130000060510       // ----------------------------------------------------------------------
130100060510         begsr *inzsr;
130200060510
130300060510      /end-free
130400060510
130500060510     c     *entry        plist
130600060510     c                   parm                    kpjba
130700060510
130800060510      /free
130900060510         in(e) §azute;
131000060510         if not %error;
131100060510          in(e) §datiute;
131200060510         endif;
131300060510         if %error or rsut = *blanks;
131400060510          tibs34r(tibs34ds);
131500060510          in §azute;
131600060510          in §datiute;
131700060510         endif;
131800060510
131900060510       // centro il titolo
132000060510        vt1tit = titolo;
132100060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
132200060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
132300060510
132400060510         endsr;
132500060510
132600060510      /end-free
132700060418
132800060418** -- MSG -------------------------------------------------------------------*
132900060418Immettere il codice                                                            1
133000060418Immettere la descrizione                                                       2
133100060522Opzione errata                                                                 3
133200090622Tariffa particolare errata                                                     4
133300090622Per il tipo servizio X non è presente la relativa tabella LTS - Limiti         5
133400090827Immettere il codice tempo di consegna NUMERICO senza spazi vuoti               6
133500090827Immettere un "NUMERO" per la parte fissa, una "x" per la parte variabile       6
