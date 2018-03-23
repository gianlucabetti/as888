000100060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060418
000300060418      *--------------------------------------------------------------*
000400060509      * GESTIONE TABELLA TARIFFE PARTICOLARI                         *
000500060418      *--------------------------------------------------------------*
000600060418
000700060418     ftabel00f  uf a e           k disk
000800060426     fazlin01l  if   e           k disk
000900060510     ftrtb24d   cf   e             workstn sfile(tb24s01:s01nrr)
001000060510     f                                     sfile(tb24s02:s02nrr)
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
002700060524      * 42 - data eliminazione
002800060524      * 43/53 - tipo tariffa
002900060524      * 54 - codice tassazione italia
003000060524      * 55 - codice tassazione estero
003100060418      *--------------------------------------------------------------*
003200060418
003300060418      *--------------------------------------------------------------*
003400060510      * campi di work
003500060418      *--------------------------------------------------------------*
003600060522     d blanks          s                   like(vt1dopz)
003700060418     d comando         s              1
003800060523     d dataalfa        s              8
003900060523     d dtaiso          s               d   inz
004000060510     d len             s              5u 0
004100060510     d savrec          s                   like(s01nrr)
004200060517     d savopzione      s                   like(vs1opz)
004300060531     d savtblftt       s                   like(tblftt)
004400060531     d savtblflt       s                   like(tblflt)
004500060510     d savtbluni       s                   like(tbluni)
004600060510     d s01nrr          s              4  0
004700060510     d s02nrr          s              4  0
004800060523     d t19cod          s                   like(tblcod)
004900060523     d t19ord          s              1
005000060523     d t19key          s                   like(tblkey)
005100060523     d t19com          s              1
005200060522     d wrkcard02       s              1    inz('1')
005300060522     d wrkcars01       s              1    inz('1')
005400060522     d wrkcarw01       s              1
005500060524     d wrkct           s                   like(v2ccts)
005600060524     d wrkeofs01       s              1
005700060522     d wrksfl02        s              1
005800060517     d $video          s              3    inz('C01')
005900060517     d xx              s              2  0
006000060524     d yy              s              2  0
006100060418
006200060418      *--------------------------------------------------------------*
006300060510      * schiere
006400060418      *--------------------------------------------------------------*
006500060418     d msg             s             78    dim(10) ctdata perrcd(1)
006600060523     d opz             s              2  0 dim(10)
006700060418
006800060418      *--------------------------------------------------------------*
006900060510      * ds interne/esterne
007000060418      *--------------------------------------------------------------*
007100060523     d v2celi          ds
007200060523     d  v2celig
007300060523     d  v2celim
007400060523     d  v2celia
007500060523
007600060523     d                 ds
007700060523     d v2ctt0                  1      1
007800060523     d v2ctt1                  2      2
007900060524     d v2ctt2                  3      3
008000060524     d v2ctt3                  4      4
008100060524     d v2ctt4                  5      5
008200060524     d v2ctt5                  6      6
008300060524     d v2ctt6                  7      7
008400060524     d v2ctt7                  8      8
008500060524     d v2ctt8                  9      9
008600060524     d v2ctt9                 10     10
008700060524     d v2cttv                 11     11
008800060524     d skctt                   1     11    dim(11)
008900060523
009000060523     d                 ds
009100060523     d aar                     1      2  0
009200060523     d mmr                     3      4  0
009300060523     d ggr                     5      6  0
009400060523     d datar                   1      6  0
009500060523
009600060523     d                 ds
009700060523     d ggd                     1      2  0
009800060523     d mmd                     3      4  0
009900060523     d aad                     5      6  0
010000060523     d datad                   1      6  0
010100060418
010200060509     d ds1p          e ds
010300060418     d kpjba         e ds
010400060523     d tibs34ds      e ds                  inz
010500060510     d trtb24ds      e ds
010600060510     d §azute        e ds                  extname(azute00f)
010700060510     d                                     dtaara
010800060510     d §datiute      e ds                  extname(ddatiute)
010900060510     d                                     dtaara
011000060510
011100060510     d psds           sds
011200060510     d  pgmname          *proc
011300060510
011400060510      *------------------------------------------------------------------------*
011500060510      * pgm richiamati
011600060510      *------------------------------------------------------------------------*
011700060510     d tibs34r         pr                  extpgm('TIBS34R')
011800060510     d  tibs34ds                           likeds(tibs34ds)
011900060523
012000060523     d trul19r         pr                  extpgm('TRUL19R')
012100060523     d  t19cod1                            like(t19cod)
012200060523     d  t19ord1                            like(t19ord)
012300060523     d  t19key1                            like(t19key)
012400060523     d  t19com1                            like(t19com)
012500060510
012600060510      *--------------------------------------------------------------*
012700060510      * costanti
012800060510      *--------------------------------------------------------------*
012900060510     d errore          c                   '1'
013000060510     d eseguito        c                   '0'
013100060510     d titolo          c                   const('Tabella Tariffe Particolari')
013200060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
013300060510     d                                     o/Ripristino  5=Visualizza')
013400060510     d opzs            c                   const('1=Scelta')
013500060418      *--------------------------------------------------------------*
013600060510
013700060510      /free
013800060510
013900060510       exsr sr_parm;
014000060517       exsr sr_video;
014100060510       exsr sr_uscita;
014200060510
014300060510       // ----------------------------------------------------------------------
014400060510       // elaborazione parametri ricevuti
014500060510       // ----------------------------------------------------------------------
014600060510       begsr sr_parm;
014700060510
014800060510        trtb24ds = kpjbu;
014900060510
015000060510        select;
015100060510       // ricerca e scleta
015200060510         when tb24fun = '1';
015300060510          *in10 = *on;
015400060510       // imposto le opzioni
015500060522          vc1opz = opzs;
015600060522          clear opz;
015700060523          opz(1) = 01;
015800060510       // gestione
015900060510         when  tb24fun = *blanks;
016000060510          *in10 = *off;
016100060517       // imposto le opzioni
016200060522          vc1opz = opzg;
016300060522          clear opz;
016400060523          opz(1) = 02;
016500060523          opz(2) = 03;
016600060523          opz(3) = 04;
016700060523          opz(4) = 05;
016800060510       // altrimenti
016900060510         other;
017000060517       // esco
017100060510          tb24esito = errore;
017200060510          exsr sr_uscita;
017300060510         endsl;
017400060510
017500060510       endsr;
017600060517
017700060517       // ----------------------------------------------------------------------
017800060517       // gestione delle videate
017900060517       // ----------------------------------------------------------------------
018000060517       begsr sr_video;
018100060517
018200060522        dow $video <> *blanks;
018300060522
018400060522         select;
018500060517       // subfile 01 - tutte le tar.particolari presenti
018600060522          when $video = 'C01';
018700060522           exsr sr_c01;
018800060517
018900060517       // video 01 - nessuna tar.particolare presente
019000060522          when $video = 'D01';
019100060522           exsr sr_d01;
019200060517
019300060517       // video 02 - gestione/visualizzazione tar.particolare scelta
019400060522          when $video = 'D02';
019500060522           exsr sr_d02;
019600060522
019700060522         endsl;
019800060522
019900060522        enddo;
020000060517
020100060517       endsr;
020200060510
020300060510       // ----------------------------------------------------------------------
020400060510       // gestione subfile
020500060510       // ----------------------------------------------------------------------
020600060510       begsr sr_c01;
020700060510
020800060510       // caricamento subfile
020900060517         exsr sr_cars01;
021000060511
021100060511       // se scritto almeno un record
021200060517         if s01nrr > 0;
021300060511       // indicatore di sflend
021400060523          *in22 = *on;
021500060517         endif;
021600060511
021700060511       // se non scritto neanche un record
021800060517         if s01nrr = 0;
021900060511       // emetto videata di dati non trovati
022000060517          $video = 'D01';
022100060524          leavesr;
022200060517         endif;
022300060510
022400060523         if rrrn01 > 0;
022500060523          rec01 = rrrn01;
022600060523         else;
022700060523          rec01 = 1;
022800060517         endif;
022900060523
023000060523       // se non so quale pagina visualizzare forzo pagina 1
023100060523          if rec01 < 1;
023200060523           rec01 = 1;
023300060523          endif;
023400060510
023500060510       // Emissione del subfile
023600060517          write tb24t01;
023700060517          write tb24z01;
023800060517          exfmt tb24c01;
023900060522
024000060522       // reset indicatore generico di errore
024100060522         *in28 = *off;
024200060522       // pulisco il campo messaggi
024300060522         clear vc1msg;
024400060510
024500060510       // controllo tasti funzionali del subfile
024600060510          select;
024700060510
024800060510       // F3=Fine
024900060510           when *inkc;
025000060510            $video = *blanks;
025100060517            tb24ricez = 'C';
025200060510            exsr sr_uscita;
025300060510
025400060510       // F5=Refresh
025500060510           when *inke;
025600060510            wrkcars01 = *on;
025700060510
025800060510       // F10=Immissione
025900060510           when *inkj;
026000060511            $video = 'D02';
026100060517            clear trtb24ds;
026200060517            tb24comand = 'J';
026300060510
026400060510       // F13=Ripetizione
026500060510           when *inkm;
026600060510            exsr sr_ripetiopz;
026700060510
026800060510       // in tutti gli altri casi
026900060510           other;
027000060510       // controllo ed elaborazione scelte su subfile
027100060510            exsr sr_gestsfl;
027200060510          endsl;
027300060510
027400060510        endsr;
027500060522
027600060522       // ----------------------------------------------------------------------
027700060522       // gestione videata
027800060522       // ----------------------------------------------------------------------
027900060522       begsr sr_d01;
028000060522
028100060522       // emetto il video
028200060522          write tb24t01;
028300060522          exfmt tb24d01;
028400060522
028500060522       // controllo tasti funzionali del video
028600060522         select;
028700060522
028800060522       // F3=Fine
028900060522          when *inkc;
029000060522           tb24ricez = 'C';
029100060522           $video = *blanks;
029200060522           exsr sr_uscita;
029300060522           leavesr;
029400060522
029500060522       // F10=Immissione
029600060522          when *inkj;
029700060522           $video = 'D02';
029800060522           clear trtb24ds;
029900060522           tb24comand = 'J';
030000060522           leavesr;
030100060522
030200060522         endsl;
030300060522
030400060522       endsr;
030500060522
030600060522       // ----------------------------------------------------------------------
030700060522       // gestione videata
030800060522       // ----------------------------------------------------------------------
030900060522       begsr sr_d02;
031000060522
031100060522       // imposto il video a seconda della funzione richiesta
031200060522         exsr sr_setvideo;
031300060522
031400060605       // fino a che la variabile resta settata come 'D02'
031500060605        dow $video = 'D02';
031600060605
031700060522       // se immessa opzione di 'scelta'
031800060523         if tb24opzio = 01;
031900060522          $video = *blanks;
032000060522          tb24cod = vs1cod;
032100060522          tb24des = vs1des;
032200060522          exsr sr_uscita;
032300060522          leavesr;
032400060522         endif;
032500060522
032600060522       // se non è immissione/copia proteggo il codice tabella
032700060523          if tb24comand <> 'J' and tb24opzio <> 03;
032800060522           *in02 = *on;
032900060522          endif;
033000060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
033100060522       // proteggo i campi del video
033200060523          if tb24opzio = 04 or tb24opzio = 05;
033300060522           *in01 = *on;
033400060522          endif;
033500060522       // se immessa opzione di 'visualizzazione'
033600060522       // non attivo F6
033700060523          if tb24opzio = 05;
033800060522           *in03 = *on;
033900060522          endif;
034000060522
034100060522       // emetto il video
034200060522          write tb24t01;
034300060522          exfmt tb24d02;
034400060522
034500060522       // reset indicatore generico di errore
034600060522         *in28 = *off;
034700060522       // pulisco il campo messaggi
034800060522         clear v2cmsg;
034900060522
035000060522       // controllo tasti funzionali del video
035100060522         select;
035200060522
035300060522       // F3=Fine
035400060522          when *inkc;
035500060522           tb24ricez = 'C';
035600060522           $video = *blanks;
035700060522           exsr sr_uscita;
035800060522           leavesr;
035900060522
036000060522       // F6=Conferma
036100060522          when *inkf;
036200060522           tb24ricez = 'F';
036300060522       // controllo e decodifico i dati del video
036400060522           exsr sr_ctrd02;
036500060522       // se non riscontrati errori emetto la finestra con i dati per la
036600060522       // tramissione
036700060522           if not *in28;
036800060522            wrkcard02 = *on;
036900060523            wrkcars01 = *off;
037000060522            wrkcarw01 = *on;
037100060522            $video = 'W01';
037200060524            exsr sr_w01;
037300060523            leavesr;
037400060522           endif;
037500060523
037600060523       // F8=Record successivo
037700060524          when *inkh;
037800060524            wrkcars01 = *off;
037900060524            wrkcard02 = *on;
038000060523           $video = 'C01';
038100060523           tb24ricez = 'H';
038200060523           leavesr;
038300060522
038400060522       // F9=Traduzione
038500060524          when *inki;
038600060522           *in05 = *on;
038700060522           $video = 'C02';
038800060523           exsr sr_c02;
038900060522
039000060522       // F12=Ritorno
039100060522          when *inkl;
039200060522           wrkcard02 = *on;
039300060524           wrkcars01 = *off;
039400060522           tb24ricez = 'L';
039500060523           if tb24comand = 'J' or tb24opzio = 03;
039600060523            wrkcars01 = *on;
039700060522           endif;
039800060522           $video = 'C01';
039900060522           leavesr;
040000060522
040100060522       // Invio
040200060522          other;
040300060522           if not *in01;
040400060522            exsr sr_ctrd02;
040500060522           endif;
040600060522
040700060522         endsl;
040800060605
040900060605       // fine gestione 'D02'
041000060605        enddo;
041100060522
041200060522        wrkcard02 = *off;
041300060522
041400060522       endsr;
041500060510
041600060510       // ----------------------------------------------------------------------
041700060510       // caricamento subfile
041800060510       // ----------------------------------------------------------------------
041900060510       begsr sr_cars01;
042000060510
042100060510       // se è stato richiesto il caricamento del subfile
042200060510        if wrkcars01 = *on;
042300060510
042400060510       // inizializzo il subfile
042500060510         s01nrr = 0;
042600060510         *in20 = *off;
042700060510         *in21 = *off;
042800060523         *in22 = *on;
042900060510         write tb24c01;
043000060510         *in20 = *on;
043100060510         *in21 = *on;
043200060510
043300060510       // imposto la chiave di posizionamento e lettura file
043400060510         tblkut = 1;
043500060510         tblcod = '1P';
043600060517         tblkey = vc1cod;
043700060510
043800060510       // posizionamento sul file
043900060510       // se è stato scelto il posizionamento
044000060517         if vc1cod <> *blanks;
044100060524          rrrn01 = 0;
044200060522          setll (tblkut:tblcod:tblkey) tabel00f;
044300060510       // altrimenti
044400060510         else;
044500060522          setll (tblkut:tblcod) tabel00f;
044600060510         endif;
044700060510
044800060510       // fino a che non è fine file
044900060510         dou %eof(tabel00f);
045000060510
045100060510       // leggo file
045200060510          reade(n) (tblkut:tblcod) tabel00f;
045300060510
045400060510       // fine file esco
045500060510          if %eof(tabel00f);
045600060510           leave;
045700060510          endif;
045800060510
045900060510       // se "ricerca/scelta" non carico i records annullati
046000060510          if (*in10 and tblflg = *blanks) or not *in10;
046100060510       // scrivo subfile
046200060517           clear vs1opz;
046300060510           exsr sr_wtrs01;
046400060510          endif;
046500060510
046600060510         enddo;
046700060510
046800060510       // fine caricamento subfile
046900060510        endif;
047000060510
047100060510       // disattivo opzione di caricamento subfile
047200060510        wrkcars01 = *off;
047300060510
047400060510       endsr;
047500060510
047600060510       // ----------------------------------------------------------------------
047700060510       // scrivo subfile
047800060510       // ----------------------------------------------------------------------
047900060510       begsr sr_wtrs01;
048000060510
048100060510       // se non raggiunto limite massimo di caricamento
048200060510        if s01nrr < 9999;
048300060510       // imposto campi di subfile
048400060510         exsr sr_sets01;
048500060510       // imposto numeratore righe e numero relativo di record
048600060510         s01nrr = s01nrr + 1;
048700060510       // scrivo subfile
048800060510         write tb24s01;
048900060510        endif;
049000060510
049100060510       endsr;
049200060510
049300060510       // ----------------------------------------------------------------------
049400060510       // imposto campi del subfile
049500060510       // ----------------------------------------------------------------------
049600060510       begsr sr_sets01;
049700060510
049800060522        vs1cod = tblkey;
049900060517        ds1p = tbluni;
050000060517        vs1des = §1pdes;
050100060517        vs1var = §1pvar;
050200060517        vs1uta = §1puta;
050300060517        vs1upr = §1pupr;
050400060517        vs1uar = §1puar;
050500060517        vs1tco = §1ptco;
050600060517        vs1fie = §1pfie;
050700060517        vs1fdp = §1pfdp;
050800060517        vs1ffe = §1pffe;
050900060517        vs1tst = §1ptst;
051000060517        vs1ost = §1post;
051100060517
051200060517        clear vs1atb;
051300060522        if tblflg <> *blanks;
051400060517         vs1atb = 'A';
051500060517        endif;
051600060510
051700060510       endsr;
051800060517
051900060517       // ----------------------------------------------------------------------
052000060517       // gestione del subfile
052100060517       // ----------------------------------------------------------------------
052200060517       begsr sr_gestsfl;
052300060517
052400060517        wrkeofs01 = *off;
052500060522        clear s01nrr;
052600060523        *in39 = *off;
052700060517
052800060517       // inizio lettura subfile
052900060522        dow wrkeofs01 = *off;
053000060522         s01nrr = s01nrr + 1;
053100060522         chain s01nrr tb24s01;
053200060522
053300060522       // se non trovo esco
053400060522         if not %found;
053500060524          if vc1cod <> *blanks;
053600060524           wrkcars01 = *on;
053700060524          endif;
053800060522          leavesr;
053900060517         endif;
054000060522
054100060517       // se è stata immessa un'opzione
054200060523         if vs1opz <> *zeros;
054300060517       // controllo se opzione valida
054400060517          xx = %lookup(vs1opz:opz);
054500060517          if xx = *zeros;
054600060517           rec01 = s01nrr;
054700060522           vc1msg = msg(03);
054800060517           *in28 = *on;
054900060523           *in39 = *on;
055000060517           update tb24s01;
055100060518           leavesr;
055200060517          endif;
055300060522
055400060517       // reset ds di servizio
055500060517          clear trtb24ds;
055600060517          tb24opzio = vs1opz;
055700060517
055800060523       // gestione del formato video
055900060522          rec01 = s01nrr;
056000060522          $video = 'D02';
056100060523          exsr sr_d02;
056200060524       // se F12 di ritorno esco dalla lettura del subfile
056300060524          if tb24ricez = 'L';
056400060524          wrkeofs01 = *on;
056500060523          endif;
056600060518         endif;
056700060517
056800060523         clear vs1opz;
056900060522         rec01 = s01nrr;
057000060522         update tb24s01;
057100060517        enddo;
057200060517
057300060517       endsr;
057400060517
057500060517       // ----------------------------------------------------------------------
057600060517       // ripeto opzione in tutte le righe del subfile
057700060517       // ----------------------------------------------------------------------
057800060517       begsr sr_ripetiopz;
057900060517
058000060523        if rrrn01 > 0;
058100060523         s01nrr = rrrn01;
058200060522         chain s01nrr tb24s01;
058300060523         if %found and vs1opz <> *zeros;
058400060522          savopzione = vs1opz;
058500060523          update tb24s01;
058600060517
058700060517          wrkeofs01 = *off;
058800060522          dow wrkeofs01 = *off;
058900060517           s01nrr = s01nrr + 1;
059000060522           chain s01nrr tb24s01;
059100060517           if %found;
059200060522            vs1opz = savopzione;
059300060522            update tb24s01;
059400060517           else;
059500060517            wrkeofs01 = *on;
059600060517           endif;
059700060517          enddo;
059800060517
059900060517         endif;
060000060517
060100060517        endif;
060200060517
060300060517       endsr;
060400060517
060500060517       // ----------------------------------------------------------------------
060600060517       // imposto i dati a video
060700060517       // ----------------------------------------------------------------------
060800060517       begsr sr_setvideo;
060900060517
061000060524        if wrkcard02 = *on;
061100060518       // pulisco il formato video D02
061200060524         exsr sr_puld02;
061300060517
061400060522       // controllo se 'immissione' 'modifica' 'copia' o altro
061500060524         select;
061600060517
061700060517       // F10=Immissione
061800060524          when tb24comand = 'J';
061900060524           vt1dopz = 'Immissione';
062000060517
062100060523       // Opzione "02"=modifica
062200060524          when tb24opzio = 02;
062300060524           vt1dopz = 'Modifica';
062400060524           exsr sr_imposta;
062500060517
062600060523       // Opzione "03"=copia
062700060524          when tb24opzio = 03;
062800060524           vt1dopz = 'Copia';
062900060524           exsr sr_imposta;
063000060517
063100060523       // Opzione "04"=cancellazione/ripristino
063200060524          when tb24opzio = 04;
063300060524           exsr sr_imposta;
063400060517       // se richiesta 'cancellazione'
063500060524           if tblflg = *blanks;
063600060524            vt1dopz = 'Annullamento';
063700060524           endif;
063800060517       // se richiesto 'ripristino'
063900060524           if tblflg <> *blanks;
064000060524            vt1dopz = 'Ripristino';
064100060524           endif;
064200060517
064300060523       // Opzione "05"=visualizzazione
064400060524          when tb24opzio =05;
064500060524           vt1dopz = 'Visualizzazione';
064600060524           exsr sr_imposta;
064700060517
064800060517       // Fine scelta
064900060524         endsl;
065000060517
065100060517       // centro la descrizione della funzione nel formato video
065200060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
065300060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
065400060524
065500060524        endif;
065600060517
065700060517       endsr;
065800060524
065900060524       // ----------------------------------------------------------------------
066000060524       // pulizia video
066100060524       // ----------------------------------------------------------------------
066200060524       begsr sr_puld02;
066300060524
066400060524        clear vt1dopz;
066500060524        clear v2ccod;
066600060524        clear v2cdes;
066700060524        clear v2cfcp;
066800060524        clear v2cuta;
066900060524        clear v2cupr;
067000060524        clear v2cuar;
067100060524        clear v2cvar;
067200060524        clear v2ctco;
067300060524        clear v2cfie;
067400060524        clear v2cfpt;
067500060524        clear v2cfdp;
067600060524        clear v2cffe;
067700060524        clear v2celi;
067800060524        clear v2csta;
067900060524        clear v2ctt0;
068000060524        clear v2ctt1;
068100060524        clear v2ctt2;
068200060524        clear v2ctt3;
068300060524        clear v2ctt4;
068400060524        clear v2ctt5;
068500060524        clear v2ctt6;
068600060524        clear v2ctt7;
068700060524        clear v2ctt8;
068800060524        clear v2ctt9;
068900060524        clear v2cttv;
069000060524        clear v2casc;
069100060524        clear v2crcv;
069200060524        clear v2cvas;
069300060524        clear v2cvvm;
069400060524        clear v2cvca;
069500060524        clear v2cvmn;
069600060524        clear v2cvmx;
069700060524        clear v2ccts;
069800060524        clear v2ccte;
069900060524        clear v2cfqt;
070000060524        clear v2cdfe;
070100060524        clear v2ctps;
070200060524        clear v2ccab;
070300060524        clear v2cfpc;
070400060524        clear v2ctst;
070500060524        clear v2cutc;
070600060524        clear v2cost;
070700060524        clear v2cptv;
070800060524        clear v2cain;
070900060524
071000060524        *in01 = *off;
071100060524        *in02 = *off;
071200060524        *in03 = *off;
071300060524        *in04 = *off;
071400060524        *in05 = *off;
071500060524        *in28 = *off;
071600060524        *in40 = *off;
071700060524        *in41 = *off;
071800060524        *in42 = *off;
071900060524        *in43 = *off;
072000060524        *in44 = *off;
072100060524        *in45 = *off;
072200060524        *in46 = *off;
072300060524        *in47 = *off;
072400060524        *in48 = *off;
072500060524        *in49 = *off;
072600060524        *in50 = *off;
072700060524        *in51 = *off;
072800060524        *in52 = *off;
072900060524        *in53 = *off;
073000060524        *in54 = *off;
073100060524        *in55 = *off;
073200060524
073300060524       endsr;
073400060524
073500060524       // ----------------------------------------------------------------------
073600060524       // imposto i dati a video
073700060524       // ----------------------------------------------------------------------
073800060524       begsr sr_imposta;
073900060524
074000060524       // recupero i dati dal file
074100060524        tblkut = 1;
074200060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
074300060524
074400060524       // imposto i campi a video
074500060524        v2ccod = tblkey;
074600060524        ds1p = tbluni;
074700060524        v2cdes = §1pdes;
074800060524        v2cfcp = §1pfcp;
074900060524        v2cuta = §1puta;
075000060524        v2cuar = §1puar;
075100060524        v2cupr = §1pupr;
075200060524        v2ctt0 = §1ptt0;
075300060524        v2ctt1 = §1ptt1;
075400060524        v2ctt2 = §1ptt2;
075500060524        v2ctt3 = §1ptt3;
075600060524        v2ctt4 = §1ptt4;
075700060524        v2ctt5 = §1ptt5;
075800060524        v2ctt6 = §1ptt6;
075900060524        v2ctt7 = §1ptt7;
076000060524        v2ctt8 = §1ptt8;
076100060524        v2ctt9 = §1ptt9;
076200060524        v2ccts = §1pcts;
076300060524        v2ccte = §1pcte;
076400060524        v2cptv = §1pptv;
076500060524        v2cvas = §1pvas;
076600060524        v2cvvm = §1pvvm;
076700060524        v2cvca = §1pvca;
076800060524        v2cvar = §1pvar;
076900060524        v2cvmn = §1pvmn;
077000060524        v2cvmx = §1pvmx;
077100060524        v2csta = §1psta;
077200060524        v2cfie = §1pfie;
077300060524        v2cain = §1pain;
077400060524        v2cttv = §1pttv;
077500060524        v2casc = §1pasc;
077600060524        v2crcv = §1prcv;
077700060524        v2ccab = §1pcab;
077800060524        v2cfpt = §1pfpt;
077900060524        v2cfpc = §1pfpc;
078000060524        v2ctco = §1ptco;
078100060524        v2ctps = §1ptps;
078200060524        v2cost = §1post;
078300060524        v2ctst = §1ptst;
078400060524        v2cutc = §1putc;
078500060524        v2cffe = §1pffe;
078600060524        v2cfdp = §1pfdp;
078700060524        v2cdfe = §1pdfe;
078800060524        v2cfqt = §1pfqt;
078900060524        v2celig = %subst(§1peli:7:2);
079000060524        v2celim = %subst(§1peli:5:2);
079100060524        v2celia = %subst(§1peli:1:4);
079200060524
079300060524        if tblflg <> *blanks;
079400060524         *in04 = *on;
079500060524        endif;
079600060524
079700060524       endsr;
079800060517
079900060517       // ----------------------------------------------------------------------
080000060517       // controllo video
080100060517       // ----------------------------------------------------------------------
080200060522       begsr sr_ctrd02;
080300060517
080400060517        *in28 = *off;
080500060517        *in40 = *off;
080600060517        *in41 = *off;
080700060523        *in42 = *off;
080800060524        *in43 = *off;
080900060524        *in44 = *off;
081000060524        *in45 = *off;
081100060524        *in46 = *off;
081200060524        *in47 = *off;
081300060524        *in48 = *off;
081400060524        *in49 = *off;
081500060524        *in50 = *off;
081600060524        *in51 = *off;
081700060524        *in52 = *off;
081800060524        *in53 = *off;
081900060524        *in54 = *off;
082000060524        *in55 = *off;
082100060517
082200060522       // codice tabella
082300060522        if v2ccod = *blanks;
082400060522         v2cmsg = msg(01);
082500060517         *in28 = *on;
082600060517         *in40 = *on;
082700060517         leavesr;
082800060517        endif;
082900060517
083000060517       // se immissione controllo che non esista già
083100060523        if tb24comand = 'J' or tb24opzio = 03;
083200060522         tblkut = 1;
083300060522         tblcod = '1P';
083400060522         tblkey = v2ccod;
083500060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
083600060522         if %found(tabel00f);
083700060522          v2cmsg = msg(05);
083800060517          *in28 = *on;
083900060517          *in40 = *on;
084000060517          leavesr;
084100060517         endif;
084200060517        endif;
084300060517
084400060517       // descrizione
084500060522        if v2cdes = *blanks;
084600060522         v2cmsg = msg(02);
084700060517         *in28 = *on;
084800060517         *in41 = *on;
084900060517         leavesr;
085000060517        endif;
085100060523
085200060523       // data eliminazione
085300060523        if v2celi <> *blanks;
085400060523         dataalfa = v2celia + v2celim + v2celig;
085500060523         test(de) *iso0 dataalfa;
085600060523         if %error;
085700060523          v2cmsg = msg(04);
085800060523          *in28 = *on;
085900060523          *in42 = *on;
086000060523          leavesr;
086100060523         endif;
086200060523        endif;
086300060523
086400060523       // tipo tariffa
086500060523        xx = 1;
086600060524        for xx by 1 to 11;
086700060523         select;
086800060523          when skctt(xx) = *blanks;
086900060523          when skctt(xx) = '?';
087000060523           t19cod = 'TR';
087100060523           clear t19key;
087200060523           trul19r(t19cod:t19ord:t19key:t19com);
087300060523           skctt(xx) = t19key;
087400060524           yy = xx + 42;
087500060524           *in(yy) = *on;
087600060523          other;
087700060524           exsr sr_ctrctt;
087800060524           if *in28;
087900060524            v2cmsg = msg(06);
088000060524            yy = xx + 42;
088100060524            *in(yy) = *on;
088200060524            leavesr;
088300060524           endif;
088400060523         endsl;
088500060523        endfor;
088600060524
088700060524       // codice tassazione italia
088800060524        if v2ccts = '?';
088900060524         t19cod = 'CT';
089000060524         clear t19key;
089100060524         trul19r(t19cod:t19ord:t19key:t19com);
089200060524         v2ccts = t19key;
089300060524         *in54 = *on;
089400060524        endif;
089500060524        if v2ccts <> *blanks;
089600060524         wrkct = v2ccts;
089700060524         exsr sr_ctrct;
089800060524          if *in28;
089900060524           v2cmsg = msg(07);
090000060524           *in54 = *on;
090100060524           leavesr;
090200060524          endif;
090300060524         endif;
090400060524
090500060524       // codice tassazione estero
090600060524        if v2ccte = '?';
090700060524         t19cod = 'CT';
090800060524         clear t19key;
090900060524         trul19r(t19cod:t19ord:t19key:t19com);
091000060524         v2ccte = t19key;
091100060524         *in55 = *on;
091200060524        endif;
091300060524        if v2ccte <> *blanks;
091400060524         wrkct = v2ccte;
091500060524         exsr sr_ctrct;
091600060524          if *in28;
091700060524           v2cmsg = msg(07);
091800060524           *in55 = *on;
091900060524           leavesr;
092000060524          endif;
092100060524         endif;
092200060517
092300060517       endsr;
092400060524
092500060524       // ----------------------------------------------------------------------
092600060524       // controllo tipo tariffa
092700060524       // ----------------------------------------------------------------------
092800060524       begsr sr_ctrctt;
092900060524
093000060524        tblkut = 1;
093100060524        tblcod = 'TR';
093200060524        tblkey = skctt(xx);
093300060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
093400060524        if not %found(tabel00f) or tblflg <> *blanks;
093500060524         *in28 = *on;
093600060524        endif;
093700060524
093800060524       endsr;
093900060524
094000060524       // ----------------------------------------------------------------------
094100060524       // controllo codice tassazione
094200060524       // ----------------------------------------------------------------------
094300060524       begsr sr_ctrct;
094400060524
094500060524        tblkut = 1;
094600060524        tblcod = 'CT';
094700060524        tblkey = wrkct;
094800060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
094900060524        if not %found(tabel00f) or tblflg <> *blanks;
095000060524         *in28 = *on;
095100060524        endif;
095200060524
095300060524       endsr;
095400060524
095500060524       // ----------------------------------------------------------------------
095600060524       // gestione video dati di trasmissione
095700060524       // ----------------------------------------------------------------------
095800060524       begsr sr_w01;
095900060524
096000060524       // imposto i dati a video
096100060524        exsr sr_carw01;
096200060524
096300060524       // fino a che la variabile resta settata come 'W01'
096400060524        dow $video = 'W01';
096500060524
096600060524       // reset indicatore generico di errore
096700060524         *in28 = *off;
096800060524
096900060524       // emetto il video
097000060524          exfmt tb24w01;
097100060524
097200060524       // controllo tasti funzionali del video
097300060524         select;
097400060524
097500060524       // F6=Conferma
097600060524          when *inkf;
097700060524       // controllo i dati del video
097800060524           exsr sr_ctrw01;
097900060524       // se non riscontrati errori aggiorno il record corrente
098000060524           if not *in28;
098100060524            exsr sr_aggiorna;
098200060524            if not *in28;
098300060524       // emetto le descrizioni in lingua
098400060524       // se non è annullo/ripristino
098500060524             if tb24opzio <> 04;
098600060524              *in05 = *off;
098700060524              $video = 'C02';
098800060524              exsr sr_c02;
098900060524              leavesr;
099000060524             endif;
099100060524             if tb24comand = 'J';
099200060524              $video = 'D02';
099300060524              exsr sr_setvideo;
099400060524             else;
099500060524              wrkcars01 = *on;
099600060524              $video = 'C01';
099700060524             endif;
099800060524            endif;
099900060524           endif;
100000060524
100100060524       // F12=Ritorno
100200060524          when *inkl;
100300060524           $video = 'D02';
100400060524           clear tb24ricez;
100500060524
100600060524       // Invio
100700060524          other;
100800060524           exsr sr_ctrw01;
100900060524
101000060524         endsl;
101100060524
101200060524       // fine gestione 'W01'
101300060524        enddo;
101400060524
101500060524       endsr;
101600060517
101700060517       // ----------------------------------------------------------------------
101800060517       // imposto i dati di trasmissione
101900060517       // ----------------------------------------------------------------------
102000060517       begsr sr_carw01;
102100060517
102200060517       // se pulisco i campi
102300060522         clear w01ftt;
102400060522         clear w01flt;
102500060522         clear w01ftr;
102600060522         clear w01dtr;
102700060517
102800060517       // se non immissione imposto i campi del file
102900060522        if tb24comand <> 'J';
103000060522         w01ftt = tblftt;
103100060522         w01flt = tblflt;
103200060522         w01ftr = tblftr;
103300060517       // imposto la data
103400060522         if tbldtr <> *zeros;
103500060523          datar = tbldtr;
103600060523          aad = aar;
103700060523          mmd = mmr;
103800060523          ggd = ggr;
103900060523          w01dtr = datad;
104000060517         endif;
104100060517
104200060517        endif;
104300060517
104400060517       endsr;
104500060517
104600060517       // ----------------------------------------------------------------------
104700060517       // controllo i dati di trasmissione
104800060517       // ----------------------------------------------------------------------
104900060517       begsr sr_ctrw01;
105000060517
105100060517       endsr;
105200060517
105300060517       // ----------------------------------------------------------------------
105400060517       // aggiorno tabella
105500060517       // ----------------------------------------------------------------------
105600060517       begsr sr_aggiorna;
105700060517
105800060517       // imposto campi del file
105900060522        tblkut = 1;
106000060522        tblcod = '1P';
106100060522        tblkey = v2ccod;
106200060523
106300060523       // aggancio il rcd della tabella
106400060523        chain (tblkut:tblcod:tblkey) tabel00f;
106500060517
106600060522        §1pdes = v2cdes;
106700060522        §1pfcp = v2cfcp;
106800060522        §1puta = v2cuta;
106900060522        §1puar = v2cuar;
107000060522        §1pupr = v2cupr;
107100060522        §1ptt0 = v2ctt0;
107200060522        §1ptt1 = v2ctt1;
107300060522        §1ptt2 = v2ctt2;
107400060522        §1ptt3 = v2ctt3;
107500060522        §1ptt4 = v2ctt4;
107600060522        §1ptt5 = v2ctt5;
107700060522        §1ptt6 = v2ctt6;
107800060522        §1ptt7 = v2ctt7;
107900060522        §1ptt8 = v2ctt8;
108000060522        §1ptt9 = v2ctt9;
108100060522        §1pcts = v2ccts;
108200060522        §1pcte = v2ccte;
108300060522        §1pptv = v2cptv;
108400060522        §1pvas = v2cvas;
108500060522        §1pvvm = v2cvvm;
108600060522        §1pvca = v2cvca;
108700060522        §1pvar = v2cvar;
108800060522        §1pvmn = v2cvmn;
108900060522        §1pvmx = v2cvmx;
109000060522        §1psta = v2csta;
109100060522        §1pfie = v2cfie;
109200060522        §1pain = v2cain;
109300060522        §1pttv = v2cttv;
109400060522        §1pasc = v2casc;
109500060522        §1prcv = v2crcv;
109600060522        §1pcab = v2ccab;
109700060522        §1pfpt = v2cfpt;
109800060522        §1pfpc = v2cfpc;
109900060522        §1ptco = v2ctco;
110000060522        §1ptps = v2ctps;
110100060522        §1post = v2cost;
110200060522        §1ptst = v2ctst;
110300060522        §1putc = v2cutc;
110400060522        §1pffe = v2cffe;
110500060522        §1pfdp = v2cfdp;
110600060522        §1pdfe = v2cdfe;
110700060522        §1pfqt = v2cfqt;
110800060523        §1peli = v2celia + v2celim + v2celig;
110900060517
111000060522        tbluni = ds1p;
111100060517
111200060522        tblftt = w01ftt;
111300060522        tblflt = w01flt;
111400060522        clear tblftr;
111500060522        clear tbldtr;
111600060524
111700060524       // annullo/ripristino
111800060524        if tb24opzio = 04;
111900060524         select;
112000060524       // annullamento
112100060524          when tblflg = *blanks;
112200060524           tblflg = '*';
112300060524       // ripristino
112400060524          when tblflg <> *blanks;
112500060524           clear tblflg;
112600060524         endsl;
112700060524        endif;
112800060517
112900060523       // controllo se devo scrivere o aggiornare il record
113000060523        if %found(tabel00f);
113100060523         update tabel;
113200060523        else;
113300060523         write tabel;
113400060523        endif;
113500060517
113600060523       // se Opzione "04"=cancellazione/ripristino
113700060517       // aggiorno automaticamente anche le descrizioni in linea
113800060523        if tb24opzio = 04;
113900060522         lintabel = 1;
114000060522         setgt lintabel azlin01l;
114100060522         dou %eof(azlin01l);
114200060522          read azlin01l;
114300060517       // fine file
114400060522          if %eof(azlin01l);
114500060517           leave;
114600060517          endif;
114700060517       // aggiorno
114800060522          tblkut = lintabel;
114900060522          chain (tblkut:tblcod:tblkey) tabel00f;
115000060522          if %found(tabel00f);
115100060517           select;
115200060522            when tblflg <> *blanks;
115300060522             clear tblflg;
115400060523            when tblflg = *blanks;
115500060522             tblflg = '*';
115600060517           endsl;
115700060522            update tabel;
115800060517          endif;
115900060517         enddo;
116000060517        endif;
116100060517
116200060517       endsr;
116300060524
116400060524       // ----------------------------------------------------------------------
116500060524       // gestione descrizioni in lingua
116600060524       // ----------------------------------------------------------------------
116700060524       begsr sr_c02;
116800060524
116900060524       // imposto il codice tabella e la descrizione
117000060524       vc2cod = v2ccod;
117100060524       vc2des = v2cdes;
117200060524
117300060524       // per prima cosa devo creare la testata in lingua
117400060524       // se non esistono già
117500060524        if not *in05;
117600060524         exsr sr_teslin;
117700060524        endif;
117800060524
117900060524       // pulisco il subfile
118000060524        exsr sr_puls02;
118100060524
118200060524       // carico il subfile con i dati che trovo o vuoto
118300060524        exsr sr_cars02;
118400060524
118500060524        wrksfl02 = *on;
118600060524
118700060524        dou wrksfl02 = *off;
118800060524         rec02 = 1;
118900060524         if savrec > *zeros;
119000060524          rec02 = savrec;
119100060524         endif;
119200060524         write tb24z02;
119300060524         exfmt tb24c02;
119400060524
119500060524         *in28 = *off;
119600060524         *in39 = *off;
119700060524
119800060524       // F12=Ritorno
119900060524          if *inkl;
120000060524           wrkcard02 = *off;
120100060524           $video = 'D02';
120200060524           leavesr;
120300060524          endif;
120400060524
120500060524       // Controlli
120600060524          if not *in05;
120700060524           exsr sr_ctrsfl;
120800060524          endif;
120900060524
121000060524       // F6=Conferma
121100060524          if *inkf and not *in28;
121200060524           exsr sr_confsfl;
121300060524           wrksfl02 = *off;
121400060524          endif;
121500060524
121600060524         enddo;
121700060524
121800060524       endsr;
121900060524
122000060524       // ----------------------------------------------------------------------
122100060524       // scrivo le testate in lingua
122200060524       // ----------------------------------------------------------------------
122300060524       begsr sr_teslin;
122400060524
122500060524       // recupero i dati dalla testata in italiano
122600060524        clear savtbluni;
122700060531        clear savtblftt;
122800060531        clear savtblflt;
122900060524        tblkut = 1;
123000060524        clear tblcod;
123100060524        tblkey = *all'0';
123200060524        %subst(tblkey:7:2) = '1P';
123300060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
123400060524        if %found(tabel00f);
123500060524         savtbluni = tbluni;
123600060531         savtblftt = tblftt;
123700060531         savtblflt = tblflt;
123800060524        endif;
123900060524
124000060524       // scrivo la testata nelle varie lingue inserite in AZLIN
124100060524        lintabel = 1;
124200060524        setgt lintabel azlin01l;
124300060524        dou %eof(azlin01l);
124400060524         read azlin01l;
124500060524       // fine file
124600060524         if %eof(azlin01l);
124700060524          leave;
124800060524         endif;
124900060524       // scrivo
125000060524         tblkut = lintabel;
125100060524         chain (tblkut:tblcod:tblkey) tabel00f;
125200060524         if not %found(tabel00f);
125300060524          clear tblcod;
125400060524          tblkey = *all'0';
125500060524          %subst(tblkey:7:2) = '1P';
125600060524          tblkut = lintabel;
125700060524          tbluni = savtbluni;
125800060531          tblftt = savtblftt;
125900060531          tblflt = savtblflt;
126000060531          clear tblftr;
126100060531          clear tbldtr;
126200060524          write tabel;
126300060524         endif;
126400060524        enddo;
126500060524
126600060524       endsr;
126700060524
126800060524       // ----------------------------------------------------------------------
126900060524       // pulisco il subfile delle descrizioni in lingua
127000060524       // ----------------------------------------------------------------------
127100060524       begsr sr_puls02;
127200060524
127300060524        clear s02nrr;
127400060524        clear savrec;
127500060524        *in20 = *off;
127600060524        *in21 = *off;
127700060524        write tb24c02;
127800060524        *in20 = *on;
127900060524        *in21 = *on;
128000060524
128100060524       endsr;
128200060524
128300060524       // ----------------------------------------------------------------------
128400060524       // carico il subfile delle descrizioni in lingua
128500060524       // ----------------------------------------------------------------------
128600060524       begsr sr_cars02;
128700060524
128800060524        *in22 = *off;
128900060524       // carico un record per ogni lingua inserite in AZLIN
129000060524        lintabel = 1;
129100060524        setgt lintabel azlin01l;
129200060524        dou %eof(azlin01l);
129300060524         read azlin01l;
129400060524       // fine file
129500060524         if %eof(azlin01l);
129600060524          leave;
129700060524         endif;
129800060524         clear ds1p;
129900060524         s2dlin = lindesita;
130000060524         tblkut = lintabel;
130100060524         tblcod = '1P';
130200060524         tblkey = v2ccod;
130300060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
130400060524         if not %found(tabel00f);
130500060524          clear s2cdes;
130600060524         else;
130700060524          ds1p = tbluni;
130800060524          s2cdes = §1pdes;
130900060524         endif;
131000060524         s2hcdlin = tblkut;
131100060524         s02nrr = s02nrr + 1;
131200060524         write tb24s02;
131300060524        enddo;
131400060524
131500060524        *in22 = *on;
131600060524
131700060524       endsr;
131800060524
131900060524       // ----------------------------------------------------------------------
132000060524       // controllo il subfile delle descrizioni in lingua
132100060524       // ----------------------------------------------------------------------
132200060524       begsr sr_ctrsfl;
132300060524
132400060524        s02nrr = 1;
132500060524        chain s02nrr tb24s02;
132600060524        dow %found;
132700060524       // deve esserci la descrizione
132800060524         if s2cdes = *blanks;
132900060524          vc2msg = msg(02);
133000060524          *in28 = *on;
133100060524          *in39 = *on;
133200060524          savrec = s02nrr;
133300060524          update tb24s02;
133400060524          leave;
133500060524         endif;
133600060524         update tb24s02;
133700060524         s02nrr = s02nrr + 1;
133800060524         chain s02nrr tb24s02;
133900060524        enddo;
134000060524
134100060524       endsr;
134200060524
134300060524       // ----------------------------------------------------------------------
134400060524       // confermo il dati del subfile delle descrizioni in lingua
134500060524       // ----------------------------------------------------------------------
134600060524       begsr sr_confsfl;
134700060524
134800060524       // recupero i dati della tabella in italiano
134900060524        clear savtbluni;
135000060524        tblkut = 1;
135100060524        tblcod = '1P';
135200060524        tblkey = v2ccod;
135300060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
135400060524        if %found(tabel00f);
135500060524         savtbluni = tbluni;
135600060524        endif;
135700060524
135800060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
135900060524        s02nrr = 1;
136000060524        chain s02nrr tb24s02;
136100060524        dow %found;
136200060524         tblkut = s2hcdlin;
136300060524         chain (tblkut:tblcod:tblkey) tabel00f;
136400060524         ds1p = savtbluni;
136500060524         §1pdes = s2cdes;
136600060524         tbluni = ds1p;
136700060524         tblftt = w01ftt;
136800060524         clear tblftr;
136900060524         clear tbldtr;
137000060524       // scrivo
137100060524         if not %found(tabel00f);
137200060524          tblkut = s2hcdlin;
137300060524          tblcod = '1P';
137400060524          tblkey = v2ccod;
137500060524          write tabel;
137600060524         else;
137700060524       // aggiorno
137800060524          update tabel;
137900060524         endif;
138000060524         s02nrr = s02nrr + 1;
138100060524         chain s02nrr tb24s02;
138200060524        enddo;
138300060524
138400060524        wrkcars01 = *on;
138500060524        $video = 'C01';
138600060524
138700060524       endsr;
138800060524
138900060524       // ----------------------------------------------------------------------
139000060524       // uscita dal programma
139100060524       // ----------------------------------------------------------------------
139200060524         begsr sr_uscita;
139300060524
139400060524          if tb24esito = *blanks;
139500060524           tb24esito = eseguito;
139600060524          endif;
139700060524
139800060524          kpjbu = trtb24ds;
139900060524
140000060524          *inlr = *on;
140100060524          return;
140200060524
140300060524         endsr;
140400060510
140500060510       // ----------------------------------------------------------------------
140600060510       // routine iniziale
140700060510       // ----------------------------------------------------------------------
140800060510         begsr *inzsr;
140900060510
141000060510      /end-free
141100060510
141200060510     c     *entry        plist
141300060510     c                   parm                    kpjba
141400060510
141500060510      /free
141600060510         in(e) §azute;
141700060510         if not %error;
141800060510          in(e) §datiute;
141900060510         endif;
142000060510         if %error or rsut = *blanks;
142100060510          tibs34r(tibs34ds);
142200060510          in §azute;
142300060510          in §datiute;
142400060510         endif;
142500060510
142600060510       // centro il titolo
142700060510        vt1tit = titolo;
142800060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
142900060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
143000060510
143100060510         endsr;
143200060510
143300060510      /end-free
143400060418
143500060418** -- MSG -------------------------------------------------------------------*
143600060418Immettere il codice                                                            1
143700060418Immettere la descrizione                                                       2
143800060522Opzione errata                                                                 3
143900060523Data errata                                                                    4
144000060522Codice errato                                                                  5
144100060524Tipo tariffa errato                                                            6
144200060524Codice tassazione errato                                                       7
