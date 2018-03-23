000100130125     /*END
000200130125
000300060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400060418
000500060418      *--------------------------------------------------------------*
000600060509      * GESTIONE TABELLA TARIFFE PARTICOLARI                         *
000700060418      *--------------------------------------------------------------*
000800060418
000900060418     ftabel00f  uf a e           k disk
001000060426     fazlin01l  if   e           k disk
001100060510     ftrtb24d   cf   e             workstn sfile(tb24s01:s01nrr)
001200060510     f                                     sfile(tb24s02:s02nrr)
001300060418      *---------------------------------------------------------------*
001400060510      * riepilogo indicatori
001500060418      *---------------------------------------------------------------*
001600060522      * 01 - protegge campi video 02
001700060510      * 02 - variazione
001800060522      * 03 - non attiva F6 video 02
001900060524      * 04 - rcd annullato
002000060510      * 05 - visualizzo subfile lingue
002100060510      * 10 - ON ricerca/scelta -- OFF gestione
002200110921      * 11 - ON escludo assicurazioni x conto
002300060510      * 20 - sfldsp
002400060510      * 21 - sfldspctl -- sflclr
002500060510      * 22 - sflend
002600060524      * 28 - visualizza messaggio errore
002700060524      * 39 - errore subfile
002800060524      * 40 - codice tabella
002900060524      * 41 - descrizione
003000060524      * 42 - data eliminazione
003100060524      * 43/53 - tipo tariffa
003200060524      * 54 - codice tassazione italia
003300060524      * 55 - codice tassazione estero
003400060418      *--------------------------------------------------------------*
003500060418
003600060418      *--------------------------------------------------------------*
003700060510      * campi di work
003800060418      *--------------------------------------------------------------*
003900060522     d blanks          s                   like(vt1dopz)
004000060418     d comando         s              1
004100060523     d dataalfa        s              8
004200060523     d dtaiso          s               d   inz
004300060510     d len             s              5u 0
004400060510     d savrec          s                   like(s01nrr)
004500060517     d savopzione      s                   like(vs1opz)
004600060531     d savtblftt       s                   like(tblftt)
004700060531     d savtblflt       s                   like(tblflt)
004800060510     d savtbluni       s                   like(tbluni)
004900060510     d s01nrr          s              4  0
005000060510     d s02nrr          s              4  0
005100060523     d t19cod          s                   like(tblcod)
005200060523     d t19ord          s              1
005300060523     d t19key          s                   like(tblkey)
005400060523     d t19com          s              1
005500060522     d wrkcard02       s              1    inz('1')
005600060522     d wrkcars01       s              1    inz('1')
005700060522     d wrkcarw01       s              1
005800060524     d wrkct           s                   like(v2ccts)
005900060524     d wrkeofs01       s              1
006000060522     d wrksfl02        s              1
006100060517     d $video          s              3    inz('C01')
006200060517     d xx              s              2  0
006300060524     d yy              s              2  0
006400060418
006500060418      *--------------------------------------------------------------*
006600060510      * schiere
006700060418      *--------------------------------------------------------------*
006800060418     d msg             s             78    dim(10) ctdata perrcd(1)
006900060523     d opz             s              2  0 dim(10)
007000060418
007100060418      *--------------------------------------------------------------*
007200060510      * ds interne/esterne
007300060418      *--------------------------------------------------------------*
007400060523     d v2celi          ds
007500060523     d  v2celig
007600060523     d  v2celim
007700060523     d  v2celia
007800060523
007900060523     d                 ds
008000060523     d v2ctt0                  1      1
008100060523     d v2ctt1                  2      2
008200060524     d v2ctt2                  3      3
008300060524     d v2ctt3                  4      4
008400060524     d v2ctt4                  5      5
008500060524     d v2ctt5                  6      6
008600060524     d v2ctt6                  7      7
008700060524     d v2ctt7                  8      8
008800060524     d v2ctt8                  9      9
008900060524     d v2ctt9                 10     10
009000060524     d v2cttv                 11     11
009100060524     d skctt                   1     11    dim(11)
009200060523
009300060523     d                 ds
009400060523     d aar                     1      2  0
009500060523     d mmr                     3      4  0
009600060523     d ggr                     5      6  0
009700060523     d datar                   1      6  0
009800060523
009900060523     d                 ds
010000060523     d ggd                     1      2  0
010100060523     d mmd                     3      4  0
010200060523     d aad                     5      6  0
010300060523     d datad                   1      6  0
010400060418
010500060509     d ds1p          e ds
010600060418     d kpjba         e ds
010700060523     d tibs34ds      e ds                  inz
010800120713     d trtb09ds      e ds
010900060510     d trtb24ds      e ds
011000060510     d §azute        e ds                  extname(azute00f)
011100060510     d                                     dtaara
011200060510     d §datiute      e ds                  extname(ddatiute)
011300060510     d                                     dtaara
011400060510
011500060510     d psds           sds
011600060510     d  pgmname          *proc
011700060510
011800060510      *------------------------------------------------------------------------*
011900060510      * pgm richiamati
012000060510      *------------------------------------------------------------------------*
012100060510     d tibs34r         pr                  extpgm('TIBS34R')
012200060510     d  tibs34ds                           likeds(tibs34ds)
012300060523
012400120713     d trtb09r         pr                  extpgm('TRTB09R')
012500120713     d  kpjba                              likeds(kpjba)
012600120713
012700060523     d trul19r         pr                  extpgm('TRUL19R')
012800060523     d  t19cod1                            like(t19cod)
012900060523     d  t19ord1                            like(t19ord)
013000060523     d  t19key1                            like(t19key)
013100060523     d  t19com1                            like(t19com)
013200060510
013300060510      *--------------------------------------------------------------*
013400060510      * costanti
013500060510      *--------------------------------------------------------------*
013600060510     d errore          c                   '1'
013700060510     d eseguito        c                   '0'
013800060510     d titolo          c                   const('Tabella Tariffe Particolari')
013900060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
014000060510     d                                     o/Ripristino  5=Visualizza')
014100060510     d opzs            c                   const('1=Scelta')
014200060418      *--------------------------------------------------------------*
014300060510
014400060510      /free
014500060510
014600060510       exsr sr_parm;
014700060517       exsr sr_video;
014800060510       exsr sr_uscita;
014900060510
015000060510       // ----------------------------------------------------------------------
015100060510       // elaborazione parametri ricevuti
015200060510       // ----------------------------------------------------------------------
015300060510       begsr sr_parm;
015400060510
015500060510        trtb24ds = kpjbu;
015600060510
015700060510        select;
015800060510       // ricerca e scleta
015900060510         when tb24fun = '1';
016000060510          *in10 = *on;
016100060510       // imposto le opzioni
016200060522          vc1opz = opzs;
016300060522          clear opz;
016400060523          opz(1) = 01;
016500110921          *in11 = (tb24ass = 'S');
016600060510       // gestione
016700060510         when  tb24fun = *blanks;
016800060510          *in10 = *off;
016900110921          *in11 = *off;
017000060517       // imposto le opzioni
017100060522          vc1opz = opzg;
017200060522          clear opz;
017300060523          opz(1) = 02;
017400060523          opz(2) = 03;
017500060523          opz(3) = 04;
017600060523          opz(4) = 05;
017700060510       // altrimenti
017800060510         other;
017900060517       // esco
018000060510          tb24esito = errore;
018100060510          exsr sr_uscita;
018200060510         endsl;
018300060510
018400060510       endsr;
018500060517
018600060517       // ----------------------------------------------------------------------
018700060517       // gestione delle videate
018800060517       // ----------------------------------------------------------------------
018900060517       begsr sr_video;
019000060517
019100060522        dow $video <> *blanks;
019200060522
019300060522         select;
019400060517       // subfile 01 - tutte le tar.particolari presenti
019500060522          when $video = 'C01';
019600060522           exsr sr_c01;
019700060517
019800060517       // video 01 - nessuna tar.particolare presente
019900060522          when $video = 'D01';
020000060522           exsr sr_d01;
020100060517
020200060517       // video 02 - gestione/visualizzazione tar.particolare scelta
020300060522          when $video = 'D02';
020400060522           exsr sr_d02;
020500060522
020600060522         endsl;
020700060522
020800060522        enddo;
020900060517
021000060517       endsr;
021100060510
021200060510       // ----------------------------------------------------------------------
021300060510       // gestione subfile
021400060510       // ----------------------------------------------------------------------
021500060510       begsr sr_c01;
021600060510
021700060510       // caricamento subfile
021800060517         exsr sr_cars01;
021900060511
022000060511       // se scritto almeno un record
022100060517         if s01nrr > 0;
022200060511       // indicatore di sflend
022300060523          *in22 = *on;
022400060517         endif;
022500060511
022600060511       // se non scritto neanche un record
022700060517         if s01nrr = 0;
022800060511       // emetto videata di dati non trovati
022900060517          $video = 'D01';
023000060524          leavesr;
023100060517         endif;
023200060510
023300060523         if rrrn01 > 0;
023400060523          rec01 = rrrn01;
023500060523         else;
023600060523          rec01 = 1;
023700060517         endif;
023800060523
023900060523       // se non so quale pagina visualizzare forzo pagina 1
024000060523          if rec01 < 1;
024100060523           rec01 = 1;
024200060523          endif;
024300060510
024400060510       // Emissione del subfile
024500060517          write tb24t01;
024600060517          write tb24z01;
024700060517          exfmt tb24c01;
024800060522
024900060522       // reset indicatore generico di errore
025000060522         *in28 = *off;
025100060522       // pulisco il campo messaggi
025200060522         clear vc1msg;
025300060510
025400060510       // controllo tasti funzionali del subfile
025500060510          select;
025600060510
025700060510       // F3=Fine
025800060510           when *inkc;
025900060510            $video = *blanks;
026000060517            tb24ricez = 'C';
026100060510            exsr sr_uscita;
026200060510
026300060510       // F5=Refresh
026400060510           when *inke;
026500060510            wrkcars01 = *on;
026600060510
026700060510       // F10=Immissione
026800060510           when *inkj;
026900060511            $video = 'D02';
027000060517            clear trtb24ds;
027100060517            tb24comand = 'J';
027200060510
027300060510       // F13=Ripetizione
027400060510           when *inkm;
027500060510            exsr sr_ripetiopz;
027600060510
027700060510       // in tutti gli altri casi
027800060510           other;
027900060510       // controllo ed elaborazione scelte su subfile
028000060510            exsr sr_gestsfl;
028100060510          endsl;
028200060510
028300060510        endsr;
028400060522
028500060522       // ----------------------------------------------------------------------
028600060522       // gestione videata
028700060522       // ----------------------------------------------------------------------
028800060522       begsr sr_d01;
028900060522
029000060522       // emetto il video
029100060522          write tb24t01;
029200060522          exfmt tb24d01;
029300060522
029400060522       // controllo tasti funzionali del video
029500060522         select;
029600060522
029700060522       // F3=Fine
029800060522          when *inkc;
029900060522           tb24ricez = 'C';
030000060522           $video = *blanks;
030100060522           exsr sr_uscita;
030200060522           leavesr;
030300060522
030400060522       // F10=Immissione
030500060522          when *inkj;
030600060522           $video = 'D02';
030700060522           clear trtb24ds;
030800060522           tb24comand = 'J';
030900060522           leavesr;
031000060522
031100060522         endsl;
031200060522
031300060522       endsr;
031400060522
031500060522       // ----------------------------------------------------------------------
031600060522       // gestione videata
031700060522       // ----------------------------------------------------------------------
031800060522       begsr sr_d02;
031900060522
032000060522       // imposto il video a seconda della funzione richiesta
032100060522         exsr sr_setvideo;
032200060522
032300060605       // fino a che la variabile resta settata come 'D02'
032400060605        dow $video = 'D02';
032500060605
032600060522       // se immessa opzione di 'scelta'
032700060523         if tb24opzio = 01;
032800060522          $video = *blanks;
032900060522          tb24cod = vs1cod;
033000060522          tb24des = vs1des;
033100060522          exsr sr_uscita;
033200060522          leavesr;
033300060522         endif;
033400060522
033500060522       // se non è immissione/copia proteggo il codice tabella
033600060523          if tb24comand <> 'J' and tb24opzio <> 03;
033700060522           *in02 = *on;
033800060522          endif;
033900060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
034000060522       // proteggo i campi del video
034100060523          if tb24opzio = 04 or tb24opzio = 05;
034200060522           *in01 = *on;
034300060522          endif;
034400060522       // se immessa opzione di 'visualizzazione'
034500060522       // non attivo F6
034600060523          if tb24opzio = 05;
034700060522           *in03 = *on;
034800060522          endif;
034900060522
035000060522       // emetto il video
035100060522          write tb24t01;
035200060522          exfmt tb24d02;
035300060522
035400060522       // reset indicatore generico di errore
035500060522         *in28 = *off;
035600060522       // pulisco il campo messaggi
035700060522         clear v2cmsg;
035800060522
035900060522       // controllo tasti funzionali del video
036000060522         select;
036100060522
036200060522       // F3=Fine
036300060522          when *inkc;
036400060522           tb24ricez = 'C';
036500060522           $video = *blanks;
036600060522           exsr sr_uscita;
036700060522           leavesr;
036800060522
036900060522       // F6=Conferma
037000060522          when *inkf;
037100060522           tb24ricez = 'F';
037200060522       // controllo e decodifico i dati del video
037300060522           exsr sr_ctrd02;
037400060522       // se non riscontrati errori emetto la finestra con i dati per la
037500060522       // tramissione
037600060522           if not *in28;
037700060522            wrkcard02 = *on;
037800060523            wrkcars01 = *off;
037900060522            wrkcarw01 = *on;
038000060522            $video = 'W01';
038100060524            exsr sr_w01;
038200060523            leavesr;
038300060522           endif;
038400060523
038500060523       // F8=Record successivo
038600060524          when *inkh;
038700060524            wrkcars01 = *off;
038800060524            wrkcard02 = *on;
038900060523           $video = 'C01';
039000060523           tb24ricez = 'H';
039100060523           leavesr;
039200060522
039300060522       // F9=Traduzione
039400060524          when *inki;
039500060522           *in05 = *on;
039600060522           $video = 'C02';
039700060523           exsr sr_c02;
039800060522
039900060522       // F12=Ritorno
040000060522          when *inkl;
040100060522           wrkcard02 = *on;
040200060524           wrkcars01 = *off;
040300060522           tb24ricez = 'L';
040400060523           if tb24comand = 'J' or tb24opzio = 03;
040500060523            wrkcars01 = *on;
040600060522           endif;
040700060522           $video = 'C01';
040800060522           leavesr;
040900060522
041000060522       // Invio
041100060522          other;
041200060522           if not *in01;
041300060522            exsr sr_ctrd02;
041400060522           endif;
041500060522
041600060522         endsl;
041700060605
041800060605       // fine gestione 'D02'
041900060605        enddo;
042000060522
042100060522        wrkcard02 = *off;
042200060522
042300060522       endsr;
042400060510
042500060510       // ----------------------------------------------------------------------
042600060510       // caricamento subfile
042700060510       // ----------------------------------------------------------------------
042800060510       begsr sr_cars01;
042900060510
043000060510       // se è stato richiesto il caricamento del subfile
043100060510        if wrkcars01 = *on;
043200060510
043300060510       // inizializzo il subfile
043400060510         s01nrr = 0;
043500060510         *in20 = *off;
043600060510         *in21 = *off;
043700060523         *in22 = *on;
043800060510         write tb24c01;
043900060510         *in20 = *on;
044000060510         *in21 = *on;
044100060510
044200060510       // imposto la chiave di posizionamento e lettura file
044300060510         tblkut = 1;
044400060510         tblcod = '1P';
044500060517         tblkey = vc1cod;
044600060510
044700060510       // posizionamento sul file
044800060510       // se è stato scelto il posizionamento
044900060517         if vc1cod <> *blanks;
045000060524          rrrn01 = 0;
045100060522          setll (tblkut:tblcod:tblkey) tabel00f;
045200060510       // altrimenti
045300060510         else;
045400060522          setll (tblkut:tblcod) tabel00f;
045500060510         endif;
045600060510
045700060510       // fino a che non è fine file
045800060510         dou %eof(tabel00f);
045900060510
046000060510       // leggo file
046100060510          reade(n) (tblkut:tblcod) tabel00f;
046200060510
046300060510       // fine file esco
046400060510          if %eof(tabel00f);
046500060510           leave;
046600060510          endif;
046700110921
046800110921          ds1p = tbluni;
046900060510
047000060510       // se "ricerca/scelta" non carico i records annullati
047100060510          if (*in10 and tblflg = *blanks) or not *in10;
047200110921       // carico tutto oppure tutto senza assicurazione per conto
047300111003            IF not *in11 or (*in11 and §1Pasc <> 'S' and §1Prcv <> 'S');
047400060510       // scrivo subfile
047500060517           clear vs1opz;
047600060510           exsr sr_wtrs01;
047700111003            ENDIF;
047800060510          endif;
047900060510
048000060510         enddo;
048100060510
048200060510       // fine caricamento subfile
048300060510        endif;
048400060510
048500060510       // disattivo opzione di caricamento subfile
048600060510        wrkcars01 = *off;
048700060510
048800060510       endsr;
048900060510
049000060510       // ----------------------------------------------------------------------
049100060510       // scrivo subfile
049200060510       // ----------------------------------------------------------------------
049300060510       begsr sr_wtrs01;
049400060510
049500060510       // se non raggiunto limite massimo di caricamento
049600060510        if s01nrr < 9999;
049700060510       // imposto campi di subfile
049800060510         exsr sr_sets01;
049900060510       // imposto numeratore righe e numero relativo di record
050000060510         s01nrr = s01nrr + 1;
050100060510       // scrivo subfile
050200060510         write tb24s01;
050300060510        endif;
050400060510
050500060510       endsr;
050600060510
050700060510       // ----------------------------------------------------------------------
050800060510       // imposto campi del subfile
050900060510       // ----------------------------------------------------------------------
051000060510       begsr sr_sets01;
051100060510
051200060522        vs1cod = tblkey;
051300110921        // ds1p = tbluni;
051400060517        vs1des = §1pdes;
051500060517        vs1var = §1pvar;
051600060517        vs1uta = §1puta;
051700060517        vs1upr = §1pupr;
051800060517        vs1uar = §1puar;
051900060517        vs1tco = §1ptco;
052000060517        vs1fie = §1pfie;
052100060517        vs1fdp = §1pfdp;
052200060517        vs1ffe = §1pffe;
052300060517        vs1tst = §1ptst;
052400060517        vs1ost = §1post;
052500060517
052600060517        clear vs1atb;
052700060522        if tblflg <> *blanks;
052800060517         vs1atb = 'A';
052900060517        endif;
053000060510
053100060510       endsr;
053200060517
053300060517       // ----------------------------------------------------------------------
053400060517       // gestione del subfile
053500060517       // ----------------------------------------------------------------------
053600060517       begsr sr_gestsfl;
053700060517
053800060517        wrkeofs01 = *off;
053900060522        clear s01nrr;
054000060523        *in39 = *off;
054100060517
054200060517       // inizio lettura subfile
054300060522        dow wrkeofs01 = *off;
054400060522         s01nrr = s01nrr + 1;
054500060522         chain s01nrr tb24s01;
054600060522
054700060522       // se non trovo esco
054800060522         if not %found;
054900060524          if vc1cod <> *blanks;
055000060524           wrkcars01 = *on;
055100060524          endif;
055200060522          leavesr;
055300060517         endif;
055400060522
055500060517       // se è stata immessa un'opzione
055600060523         if vs1opz <> *zeros;
055700060517       // controllo se opzione valida
055800060517          xx = %lookup(vs1opz:opz);
055900060517          if xx = *zeros;
056000060517           rec01 = s01nrr;
056100060522           vc1msg = msg(03);
056200060517           *in28 = *on;
056300060523           *in39 = *on;
056400060517           update tb24s01;
056500060518           leavesr;
056600060517          endif;
056700060522
056800060517       // reset ds di servizio
056900060517          clear trtb24ds;
057000060517          tb24opzio = vs1opz;
057100060517
057200060523       // gestione del formato video
057300060522          rec01 = s01nrr;
057400060522          $video = 'D02';
057500060523          exsr sr_d02;
057600060524       // se F12 di ritorno esco dalla lettura del subfile
057700060524          if tb24ricez = 'L';
057800060524          wrkeofs01 = *on;
057900060523          endif;
058000060518         endif;
058100060517
058200060523         clear vs1opz;
058300060522         rec01 = s01nrr;
058400060522         update tb24s01;
058500060517        enddo;
058600060517
058700060517       endsr;
058800060517
058900060517       // ----------------------------------------------------------------------
059000060517       // ripeto opzione in tutte le righe del subfile
059100060517       // ----------------------------------------------------------------------
059200060517       begsr sr_ripetiopz;
059300060517
059400060523        if rrrn01 > 0;
059500060523         s01nrr = rrrn01;
059600060522         chain s01nrr tb24s01;
059700060523         if %found and vs1opz <> *zeros;
059800060522          savopzione = vs1opz;
059900060523          update tb24s01;
060000060517
060100060517          wrkeofs01 = *off;
060200060522          dow wrkeofs01 = *off;
060300060517           s01nrr = s01nrr + 1;
060400060522           chain s01nrr tb24s01;
060500060517           if %found;
060600060522            vs1opz = savopzione;
060700060522            update tb24s01;
060800060517           else;
060900060517            wrkeofs01 = *on;
061000060517           endif;
061100060517          enddo;
061200060517
061300060517         endif;
061400060517
061500060517        endif;
061600060517
061700060517       endsr;
061800060517
061900060517       // ----------------------------------------------------------------------
062000060517       // imposto i dati a video
062100060517       // ----------------------------------------------------------------------
062200060517       begsr sr_setvideo;
062300060517
062400060524        if wrkcard02 = *on;
062500060518       // pulisco il formato video D02
062600060524         exsr sr_puld02;
062700060517
062800060522       // controllo se 'immissione' 'modifica' 'copia' o altro
062900060524         select;
063000060517
063100060517       // F10=Immissione
063200060524          when tb24comand = 'J';
063300060524           vt1dopz = 'Immissione';
063400060517
063500060523       // Opzione "02"=modifica
063600060524          when tb24opzio = 02;
063700060524           vt1dopz = 'Modifica';
063800060524           exsr sr_imposta;
063900060517
064000060523       // Opzione "03"=copia
064100060524          when tb24opzio = 03;
064200060524           vt1dopz = 'Copia';
064300060524           exsr sr_imposta;
064400060517
064500060523       // Opzione "04"=cancellazione/ripristino
064600060524          when tb24opzio = 04;
064700060524           exsr sr_imposta;
064800060517       // se richiesta 'cancellazione'
064900060524           if tblflg = *blanks;
065000060524            vt1dopz = 'Annullamento';
065100060524           endif;
065200060517       // se richiesto 'ripristino'
065300060524           if tblflg <> *blanks;
065400060524            vt1dopz = 'Ripristino';
065500060524           endif;
065600060517
065700060523       // Opzione "05"=visualizzazione
065800060524          when tb24opzio =05;
065900060524           vt1dopz = 'Visualizzazione';
066000060524           exsr sr_imposta;
066100060517
066200060517       // Fine scelta
066300060524         endsl;
066400060517
066500060517       // centro la descrizione della funzione nel formato video
066600060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
066700060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
066800060524
066900060524        endif;
067000060517
067100060517       endsr;
067200060524
067300060524       // ----------------------------------------------------------------------
067400060524       // pulizia video
067500060524       // ----------------------------------------------------------------------
067600060524       begsr sr_puld02;
067700060524
067800060524        clear vt1dopz;
067900060524        clear v2ccod;
068000060524        clear v2cdes;
068100060524        clear v2cfcp;
068200060524        clear v2cuta;
068300060524        clear v2cupr;
068400060524        clear v2cuar;
068500060524        clear v2cvar;
068600060524        clear v2ctco;
068700060524        clear v2cfie;
068800060524        clear v2cfpt;
068900060524        clear v2cfdp;
069000060524        clear v2cffe;
069100060524        clear v2celi;
069200060524        clear v2ctt0;
069300060524        clear v2ctt1;
069400060524        clear v2ctt2;
069500060524        clear v2ctt3;
069600060524        clear v2ctt4;
069700060524        clear v2ctt5;
069800060524        clear v2ctt6;
069900060524        clear v2ctt7;
070000060524        clear v2ctt8;
070100060524        clear v2ctt9;
070200060524        clear v2cttv;
070300060524        clear v2casc;
070400060524        clear v2crcv;
070500060524        clear v2cvas;
070600060524        clear v2cvvm;
070700060524        clear v2cvca;
070800060524        clear v2cvmn;
070900060524        clear v2cvmx;
071000060524        clear v2ccts;
071100060524        clear v2ccte;
071200060524        clear v2cfqt;
071300060524        clear v2cdfe;
071400060524        clear v2ctps;
071500060524        clear v2cfpc;
071600060524        clear v2ctst;
071700060524        clear v2cost;
071800060524        clear v2cptv;
071900060524        clear v2cain;
072000060731        clear v2cdrf;
072100080227        clear v2cpda;
072200060524
072300060524        *in01 = *off;
072400060524        *in02 = *off;
072500060524        *in03 = *off;
072600060524        *in04 = *off;
072700060524        *in05 = *off;
072800060524        *in28 = *off;
072900060524        *in40 = *off;
073000060524        *in41 = *off;
073100060524        *in42 = *off;
073200060524        *in43 = *off;
073300060524        *in44 = *off;
073400060524        *in45 = *off;
073500060524        *in46 = *off;
073600060524        *in47 = *off;
073700060524        *in48 = *off;
073800060524        *in49 = *off;
073900060524        *in50 = *off;
074000060524        *in51 = *off;
074100060524        *in52 = *off;
074200060524        *in53 = *off;
074300060524        *in54 = *off;
074400060524        *in55 = *off;
074500060524
074600060524       endsr;
074700060524
074800060524       // ----------------------------------------------------------------------
074900060524       // imposto i dati a video
075000060524       // ----------------------------------------------------------------------
075100060524       begsr sr_imposta;
075200060524
075300060524       // recupero i dati dal file
075400060524        tblkut = 1;
075500060731        tblcod = '1P';
075600060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
075700060524
075800060524       // imposto i campi a video
075900060524        v2ccod = tblkey;
076000060524        ds1p = tbluni;
076100060524        v2cdes = §1pdes;
076200060524        v2cfcp = §1pfcp;
076300060524        v2cuta = §1puta;
076400060524        v2cuar = §1puar;
076500060524        v2cupr = §1pupr;
076600060524        v2ctt0 = §1ptt0;
076700060524        v2ctt1 = §1ptt1;
076800060524        v2ctt2 = §1ptt2;
076900060524        v2ctt3 = §1ptt3;
077000060524        v2ctt4 = §1ptt4;
077100060524        v2ctt5 = §1ptt5;
077200060524        v2ctt6 = §1ptt6;
077300060524        v2ctt7 = §1ptt7;
077400060524        v2ctt8 = §1ptt8;
077500060524        v2ctt9 = §1ptt9;
077600060524        v2ccts = §1pcts;
077700060524        v2ccte = §1pcte;
077800060524        v2cptv = §1pptv;
077900060524        v2cvas = §1pvas;
078000060524        v2cvvm = §1pvvm;
078100060524        v2cvca = §1pvca;
078200060524        v2cvar = §1pvar;
078300060524        v2cvmn = §1pvmn;
078400060524        v2cvmx = §1pvmx;
078500060524        v2cfie = §1pfie;
078600060524        v2cain = §1pain;
078700060524        v2cttv = §1pttv;
078800060524        v2casc = §1pasc;
078900060524        v2crcv = §1prcv;
079000060524        v2cfpt = §1pfpt;
079100060524        v2cfpc = §1pfpc;
079200060524        v2ctco = §1ptco;
079300060524        v2ctps = §1ptps;
079400060524        v2cost = §1post;
079500060524        v2ctst = §1ptst;
079600060524        v2cffe = §1pffe;
079700060524        v2cfdp = §1pfdp;
079800060524        v2cdfe = §1pdfe;
079900060524        v2cfqt = §1pfqt;
080000060524        v2celig = %subst(§1peli:7:2);
080100060524        v2celim = %subst(§1peli:5:2);
080200060524        v2celia = %subst(§1peli:1:4);
080300060731        v2cdrf = §1pdrf;
080400080227        v2cpda = §1ppda;
080500130125        v2cModTP = §1PmodTp;
080600060524
080700060524        if tblflg <> *blanks;
080800060524         *in04 = *on;
080900060524        endif;
081000060524
081100060524       endsr;
081200060517
081300060517       // ----------------------------------------------------------------------
081400060517       // controllo video
081500060517       // ----------------------------------------------------------------------
081600060522       begsr sr_ctrd02;
081700060517
081800060517        *in28 = *off;
081900060517        *in40 = *off;
082000060517        *in41 = *off;
082100060523        *in42 = *off;
082200060524        *in43 = *off;
082300060524        *in44 = *off;
082400060524        *in45 = *off;
082500060524        *in46 = *off;
082600060524        *in47 = *off;
082700060524        *in48 = *off;
082800060524        *in49 = *off;
082900060524        *in50 = *off;
083000060524        *in51 = *off;
083100060524        *in52 = *off;
083200060524        *in53 = *off;
083300060524        *in54 = *off;
083400060524        *in55 = *off;
083500060517
083600060522       // codice tabella
083700060522        if v2ccod = *blanks;
083800060522         v2cmsg = msg(01);
083900060517         *in28 = *on;
084000060517         *in40 = *on;
084100060517         leavesr;
084200060517        endif;
084300060517
084400060517       // se immissione controllo che non esista già
084500060523        if tb24comand = 'J' or tb24opzio = 03;
084600060522         tblkut = 1;
084700060522         tblcod = '1P';
084800060522         tblkey = v2ccod;
084900060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
085000060522         if %found(tabel00f);
085100060522          v2cmsg = msg(05);
085200060517          *in28 = *on;
085300060517          *in40 = *on;
085400060517          leavesr;
085500060517         endif;
085600060517        endif;
085700060517
085800060517       // descrizione
085900060522        if v2cdes = *blanks;
086000060522         v2cmsg = msg(02);
086100060517         *in28 = *on;
086200060517         *in41 = *on;
086300060517         leavesr;
086400060517        endif;
086500060523
086600060523       // tipo tariffa
086700060523        xx = 1;
086800060524        for xx by 1 to 11;
086900060523         select;
087000060523          when skctt(xx) = *blanks;
087100060523          when skctt(xx) = '?';
087200060523           t19cod = 'TR';
087300060523           clear t19key;
087400060523           trul19r(t19cod:t19ord:t19key:t19com);
087500060523           skctt(xx) = t19key;
087600060524           yy = xx + 42;
087700060524           *in(yy) = *on;
087800060523          other;
087900060524           exsr sr_ctrctt;
088000060524           if *in28;
088100060524            v2cmsg = msg(06);
088200060524            yy = xx + 42;
088300060524            *in(yy) = *on;
088400060524            leavesr;
088500060524           endif;
088600060523         endsl;
088700060523        endfor;
088800060524
088900060524       // codice tassazione italia
089000060524        if v2ccts = '?';
089100120713         clear TRTB09DS;
089200120713         TB09fun = '1';
089300120713         TB09ord = '1';
089400120713         kpjbu = TRTB09DS;
089500120713         trtb09r (kpjba);
089600120713         TRTB09DS = kpjbu;
089700120713         v2ccts = TB09cod;
089800060524         *in54 = *on;
089900060524        endif;
090000060524        if v2ccts <> *blanks;
090100060524         wrkct = v2ccts;
090200060524         exsr sr_ctrct;
090300060524          if *in28;
090400060524           v2cmsg = msg(07);
090500060524           *in54 = *on;
090600060524           leavesr;
090700060524          endif;
090800060524         endif;
090900060524
091000060524       // codice tassazione estero
091100060524        if v2ccte = '?';
091200120713         clear TRTB09DS;
091300120713         TB09fun = '1';
091400120713         TB09ord = '1';
091500120713         kpjbu = TRTB09DS;
091600120713         trtb09r (kpjba);
091700120713         TRTB09DS = kpjbu;
091800120713         v2ccts = TB09cod;
091900060524         *in55 = *on;
092000060524        endif;
092100060524        if v2ccte <> *blanks;
092200060524         wrkct = v2ccte;
092300060524         exsr sr_ctrct;
092400060524          if *in28;
092500060524           v2cmsg = msg(07);
092600060524           *in55 = *on;
092700060524           leavesr;
092800060524          endif;
092900060524         endif;
093000130125
093100130125       // data eliminazione
093200130125        if v2celi <> *blanks;
093300130125         dataalfa = v2celia + v2celim + v2celig;
093400130125         test(de) *iso0 dataalfa;
093500130125         if %error;
093600130125          v2cmsg = msg(04);
093700130125          *in28 = *on;
093800130125          *in42 = *on;
093900130125          leavesr;
094000130125         endif;
094100130125        endif;
094200061212
094300061212       // riaggancio di nuovo la tabella xchè mi sono sposizionata
094400061212        tblkut = 1;
094500061212        tblcod = '1P';
094600061212        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
094700060517
094800060517       endsr;
094900060524
095000060524       // ----------------------------------------------------------------------
095100060524       // controllo tipo tariffa
095200060524       // ----------------------------------------------------------------------
095300060524       begsr sr_ctrctt;
095400060524
095500060524        tblkut = 1;
095600060524        tblcod = 'TR';
095700060524        tblkey = skctt(xx);
095800060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
095900060524        if not %found(tabel00f) or tblflg <> *blanks;
096000060524         *in28 = *on;
096100060524        endif;
096200060524
096300060524       endsr;
096400060524
096500060524       // ----------------------------------------------------------------------
096600060524       // controllo codice tassazione
096700060524       // ----------------------------------------------------------------------
096800060524       begsr sr_ctrct;
096900060524
097000060524        tblkut = 1;
097100060524        tblcod = 'CT';
097200060524        tblkey = wrkct;
097300060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
097400060524        if not %found(tabel00f) or tblflg <> *blanks;
097500060524         *in28 = *on;
097600060524        endif;
097700060524
097800060524       endsr;
097900060524
098000060524       // ----------------------------------------------------------------------
098100060524       // gestione video dati di trasmissione
098200060524       // ----------------------------------------------------------------------
098300060524       begsr sr_w01;
098400060524
098500060524       // imposto i dati a video
098600060524        exsr sr_carw01;
098700060524
098800060524       // fino a che la variabile resta settata come 'W01'
098900060524        dow $video = 'W01';
099000060524
099100060524       // reset indicatore generico di errore
099200060524         *in28 = *off;
099300060524
099400060524       // emetto il video
099500060524          exfmt tb24w01;
099600060524
099700060524       // controllo tasti funzionali del video
099800060524         select;
099900060524
100000060524       // F6=Conferma
100100060524          when *inkf;
100200060524       // controllo i dati del video
100300060524           exsr sr_ctrw01;
100400060524       // se non riscontrati errori aggiorno il record corrente
100500060524           if not *in28;
100600060524            exsr sr_aggiorna;
100700060524            if not *in28;
100800060524       // emetto le descrizioni in lingua
100900060524       // se non è annullo/ripristino
101000060524             if tb24opzio <> 04;
101100060524              *in05 = *off;
101200060524              $video = 'C02';
101300060524              exsr sr_c02;
101400060524              leavesr;
101500060524             endif;
101600060524             if tb24comand = 'J';
101700060524              $video = 'D02';
101800060524              exsr sr_setvideo;
101900060524             else;
102000060524              wrkcars01 = *on;
102100060524              $video = 'C01';
102200060524             endif;
102300060524            endif;
102400060524           endif;
102500060524
102600060524       // F12=Ritorno
102700060524          when *inkl;
102800060524           $video = 'D02';
102900060524           clear tb24ricez;
103000061212           tb24ricez = 'L';
103100061212           wrkcard02 = *off;
103200060524
103300060524       // Invio
103400060524          other;
103500060524           exsr sr_ctrw01;
103600060524
103700060524         endsl;
103800060524
103900060524       // fine gestione 'W01'
104000060524        enddo;
104100060524
104200060524       endsr;
104300060517
104400060517       // ----------------------------------------------------------------------
104500060517       // imposto i dati di trasmissione
104600060517       // ----------------------------------------------------------------------
104700060517       begsr sr_carw01;
104800060517
104900060517       // se pulisco i campi
105000060522         clear w01ftt;
105100060522         clear w01flt;
105200060522         clear w01ftr;
105300060522         clear w01dtr;
105400060517
105500060517       // se non immissione imposto i campi del file
105600060522        if tb24comand <> 'J';
105700060522         w01ftt = tblftt;
105800060522         w01flt = tblflt;
105900060522         w01ftr = tblftr;
106000060517       // imposto la data
106100060522         if tbldtr <> *zeros;
106200060523          datar = tbldtr;
106300060523          aad = aar;
106400060523          mmd = mmr;
106500060523          ggd = ggr;
106600060523          w01dtr = datad;
106700060517         endif;
106800060517
106900060517        endif;
107000060517
107100060517       endsr;
107200060517
107300060517       // ----------------------------------------------------------------------
107400060517       // controllo i dati di trasmissione
107500060517       // ----------------------------------------------------------------------
107600060517       begsr sr_ctrw01;
107700060517
107800060517       endsr;
107900060517
108000060517       // ----------------------------------------------------------------------
108100060517       // aggiorno tabella
108200060517       // ----------------------------------------------------------------------
108300060517       begsr sr_aggiorna;
108400060517
108500060517       // imposto campi del file
108600060522        tblkut = 1;
108700060522        tblcod = '1P';
108800060522        tblkey = v2ccod;
108900060523
109000060523       // aggancio il rcd della tabella
109100060523        chain (tblkut:tblcod:tblkey) tabel00f;
109200060517
109300060522        §1pdes = v2cdes;
109400060522        §1pfcp = v2cfcp;
109500060522        §1puta = v2cuta;
109600060522        §1puar = v2cuar;
109700060522        §1pupr = v2cupr;
109800060522        §1ptt0 = v2ctt0;
109900060522        §1ptt1 = v2ctt1;
110000060522        §1ptt2 = v2ctt2;
110100060522        §1ptt3 = v2ctt3;
110200060522        §1ptt4 = v2ctt4;
110300060522        §1ptt5 = v2ctt5;
110400060522        §1ptt6 = v2ctt6;
110500060522        §1ptt7 = v2ctt7;
110600060522        §1ptt8 = v2ctt8;
110700060522        §1ptt9 = v2ctt9;
110800060522        §1pcts = v2ccts;
110900060522        §1pcte = v2ccte;
111000060522        §1pptv = v2cptv;
111100060522        §1pvas = v2cvas;
111200060522        §1pvvm = v2cvvm;
111300060522        §1pvca = v2cvca;
111400060522        §1pvar = v2cvar;
111500060522        §1pvmn = v2cvmn;
111600060522        §1pvmx = v2cvmx;
111700060522        §1pfie = v2cfie;
111800060522        §1pain = v2cain;
111900060522        §1pttv = v2cttv;
112000060522        §1pasc = v2casc;
112100060522        §1prcv = v2crcv;
112200060522        §1pfpt = v2cfpt;
112300060522        §1pfpc = v2cfpc;
112400060522        §1ptco = v2ctco;
112500060522        §1ptps = v2ctps;
112600060522        §1post = v2cost;
112700060522        §1ptst = v2ctst;
112800060522        §1pffe = v2cffe;
112900060522        §1pfdp = v2cfdp;
113000060522        §1pdfe = v2cdfe;
113100060522        §1pfqt = v2cfqt;
113200060523        §1peli = v2celia + v2celim + v2celig;
113300060731        §1pdrf = v2cdrf;
113400080227        §1ppda = v2cpda;
113500130125        §1PmodTp = v2cModTP;
113600060517
113700060522        tbluni = ds1p;
113800060517
113900060522        tblftt = w01ftt;
114000060522        tblflt = w01flt;
114100060522        clear tblftr;
114200060522        clear tbldtr;
114300060524
114400060524       // annullo/ripristino
114500060524        if tb24opzio = 04;
114600060524         select;
114700060524       // annullamento
114800060524          when tblflg = *blanks;
114900060524           tblflg = '*';
115000060524       // ripristino
115100060524          when tblflg <> *blanks;
115200060524           clear tblflg;
115300060524         endsl;
115400060524        endif;
115500060517
115600060523       // controllo se devo scrivere o aggiornare il record
115700060523        if %found(tabel00f);
115800060523         update tabel;
115900060523        else;
116000060523         write tabel;
116100060523        endif;
116200060517
116300060523       // se Opzione "04"=cancellazione/ripristino
116400060517       // aggiorno automaticamente anche le descrizioni in linea
116500060523        if tb24opzio = 04;
116600060522         lintabel = 1;
116700060522         setgt lintabel azlin01l;
116800060522         dou %eof(azlin01l);
116900060522          read azlin01l;
117000060517       // fine file
117100060522          if %eof(azlin01l);
117200060517           leave;
117300060517          endif;
117400060517       // aggiorno
117500060522          tblkut = lintabel;
117600060522          chain (tblkut:tblcod:tblkey) tabel00f;
117700060522          if %found(tabel00f);
117800060517           select;
117900060522            when tblflg <> *blanks;
118000060522             clear tblflg;
118100060523            when tblflg = *blanks;
118200060522             tblflg = '*';
118300060517           endsl;
118400060522            update tabel;
118500060517          endif;
118600060517         enddo;
118700060517        endif;
118800060517
118900060517       endsr;
119000060524
119100060524       // ----------------------------------------------------------------------
119200060524       // gestione descrizioni in lingua
119300060524       // ----------------------------------------------------------------------
119400060524       begsr sr_c02;
119500060524
119600060524       // imposto il codice tabella e la descrizione
119700060524       vc2cod = v2ccod;
119800060524       vc2des = v2cdes;
119900060524
120000060524       // pulisco il subfile
120100060524        exsr sr_puls02;
120200060524
120300060524       // carico il subfile con i dati che trovo o vuoto
120400060524        exsr sr_cars02;
120500060524
120600060524        wrksfl02 = *on;
120700060524
120800060524        dou wrksfl02 = *off;
120900060524         rec02 = 1;
121000060524         if savrec > *zeros;
121100060524          rec02 = savrec;
121200060524         endif;
121300060524         write tb24z02;
121400060524         exfmt tb24c02;
121500060524
121600060524         *in28 = *off;
121700060524         *in39 = *off;
121800060524
121900060524       // F12=Ritorno
122000060524          if *inkl;
122100060524           wrkcard02 = *off;
122200060524           $video = 'D02';
122300060524           leavesr;
122400060524          endif;
122500060524
122600060524       // Controlli
122700060524          if not *in05;
122800060524           exsr sr_ctrsfl;
122900060524          endif;
123000060524
123100060524       // F6=Conferma
123200060524          if *inkf and not *in28;
123300060524           exsr sr_confsfl;
123400060524           wrksfl02 = *off;
123500060524          endif;
123600060524
123700060524         enddo;
123800060524
123900060524       endsr;
124000060524
124100060524       // ----------------------------------------------------------------------
124200060524       // pulisco il subfile delle descrizioni in lingua
124300060524       // ----------------------------------------------------------------------
124400060524       begsr sr_puls02;
124500060524
124600060524        clear s02nrr;
124700060524        clear savrec;
124800060524        *in20 = *off;
124900060524        *in21 = *off;
125000060524        write tb24c02;
125100060524        *in20 = *on;
125200060524        *in21 = *on;
125300060524
125400060524       endsr;
125500060524
125600060524       // ----------------------------------------------------------------------
125700060524       // carico il subfile delle descrizioni in lingua
125800060524       // ----------------------------------------------------------------------
125900060524       begsr sr_cars02;
126000060524
126100060524        *in22 = *off;
126200060524       // carico un record per ogni lingua inserite in AZLIN
126300060524        lintabel = 1;
126400060524        setgt lintabel azlin01l;
126500060524        dou %eof(azlin01l);
126600060524         read azlin01l;
126700060524       // fine file
126800060524         if %eof(azlin01l);
126900060524          leave;
127000060524         endif;
127100060524         clear ds1p;
127200060524         s2dlin = lindesita;
127300060524         tblkut = lintabel;
127400060524         tblcod = '1P';
127500060524         tblkey = v2ccod;
127600060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
127700060524         if not %found(tabel00f);
127800060524          clear s2cdes;
127900060524         else;
128000060524          ds1p = tbluni;
128100060524          s2cdes = §1pdes;
128200060524         endif;
128300060524         s2hcdlin = tblkut;
128400060524         s02nrr = s02nrr + 1;
128500060524         write tb24s02;
128600060524        enddo;
128700060524
128800060524        *in22 = *on;
128900060524
129000060524       endsr;
129100060524
129200060524       // ----------------------------------------------------------------------
129300060524       // controllo il subfile delle descrizioni in lingua
129400060524       // ----------------------------------------------------------------------
129500060524       begsr sr_ctrsfl;
129600060524
129700060524        s02nrr = 1;
129800060524        chain s02nrr tb24s02;
129900060524        dow %found;
130000060524       // deve esserci la descrizione
130100060524         if s2cdes = *blanks;
130200060524          vc2msg = msg(02);
130300060524          *in28 = *on;
130400060524          *in39 = *on;
130500060524          savrec = s02nrr;
130600060524          update tb24s02;
130700060524          leave;
130800060524         endif;
130900060524         update tb24s02;
131000060524         s02nrr = s02nrr + 1;
131100060524         chain s02nrr tb24s02;
131200060524        enddo;
131300060524
131400060524       endsr;
131500060524
131600060524       // ----------------------------------------------------------------------
131700060524       // confermo il dati del subfile delle descrizioni in lingua
131800060524       // ----------------------------------------------------------------------
131900060524       begsr sr_confsfl;
132000060524
132100060524       // recupero i dati della tabella in italiano
132200060524        clear savtbluni;
132300060524        tblkut = 1;
132400060524        tblcod = '1P';
132500060524        tblkey = v2ccod;
132600060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
132700060524        if %found(tabel00f);
132800060524         savtbluni = tbluni;
132900060524        endif;
133000060524
133100060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
133200060524        s02nrr = 1;
133300060524        chain s02nrr tb24s02;
133400060524        dow %found;
133500060524         tblkut = s2hcdlin;
133600060524         chain (tblkut:tblcod:tblkey) tabel00f;
133700060524         ds1p = savtbluni;
133800060524         §1pdes = s2cdes;
133900060524         tbluni = ds1p;
134000060524         tblftt = w01ftt;
134100060524         clear tblftr;
134200060524         clear tbldtr;
134300060524       // scrivo
134400060524         if not %found(tabel00f);
134500060524          tblkut = s2hcdlin;
134600060524          tblcod = '1P';
134700060524          tblkey = v2ccod;
134800060524          write tabel;
134900060524         else;
135000060524       // aggiorno
135100060524          update tabel;
135200060524         endif;
135300060524         s02nrr = s02nrr + 1;
135400060524         chain s02nrr tb24s02;
135500060524        enddo;
135600060524
135700060524        wrkcars01 = *on;
135800060524        $video = 'C01';
135900060524
136000060524       endsr;
136100060524
136200060524       // ----------------------------------------------------------------------
136300060524       // uscita dal programma
136400060524       // ----------------------------------------------------------------------
136500060524         begsr sr_uscita;
136600060524
136700060524          if tb24esito = *blanks;
136800060524           tb24esito = eseguito;
136900060524          endif;
137000060524
137100060524          kpjbu = trtb24ds;
137200060524
137300060524          *inlr = *on;
137400060524          return;
137500060524
137600060524         endsr;
137700060510
137800060510       // ----------------------------------------------------------------------
137900060510       // routine iniziale
138000060510       // ----------------------------------------------------------------------
138100060510         begsr *inzsr;
138200060510
138300060510      /end-free
138400060510
138500060510     c     *entry        plist
138600060510     c                   parm                    kpjba
138700060510
138800060510      /free
138900060510         in(e) §azute;
139000060510         if not %error;
139100060510          in(e) §datiute;
139200060510         endif;
139300060510         if %error or rsut = *blanks;
139400060510          tibs34r(tibs34ds);
139500060510          in §azute;
139600060510          in §datiute;
139700060510         endif;
139800060510
139900060510       // centro il titolo
140000060510        vt1tit = titolo;
140100060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
140200060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
140300060510
140400060510         endsr;
140500060510
140600060510      /end-free
140700060418
140800060418** -- MSG -------------------------------------------------------------------*
140900060418Immettere il codice                                                            1
141000060418Immettere la descrizione                                                       2
141100060522Opzione errata                                                                 3
141200060523Data errata                                                                    4
141300060522Codice errato                                                                  5
141400060524Tipo tariffa errato                                                            6
141500060524Codice tassazione errato                                                       7
