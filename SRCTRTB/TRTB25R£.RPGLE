000100060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200060418
000300060418      *--------------------------------------------------------------*
000400060524      * GESTIONE TABELLA ESTENSIONE TARIFFE PARTICOLARI              *
000500060418      *--------------------------------------------------------------*
000600060418
000700060418     ftabel00f  uf a e           k disk
000800060426     fazlin01l  if   e           k disk
000900060524     ftrtb25d   cf   e             workstn sfile(tb25s01:s01nrr)
001000060524     f                                     sfile(tb25s02:s02nrr)
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
002700060418      *--------------------------------------------------------------*
002800060418
002900060418      *--------------------------------------------------------------*
003000060510      * campi di work
003100060418      *--------------------------------------------------------------*
003200060522     d blanks          s                   like(vt1dopz)
003300060418     d comando         s              1
003400060523     d dataalfa        s              8
003500060523     d dtaiso          s               d   inz
003600060510     d len             s              5u 0
003700060510     d savrec          s                   like(s01nrr)
003800060517     d savopzione      s                   like(vs1opz)
003900060531     d savtblftt       s                   like(tblftt)
004000060531     d savtblflt       s                   like(tblflt)
004100060510     d savtbluni       s                   like(tbluni)
004200060510     d s01nrr          s              4  0
004300060510     d s02nrr          s              4  0
004400060523     d t19cod          s                   like(tblcod)
004500060523     d t19ord          s              1
004600060523     d t19key          s                   like(tblkey)
004700060523     d t19com          s              1
004800060522     d wrkcard02       s              1    inz('1')
004900060522     d wrkcars01       s              1    inz('1')
005000060522     d wrkcarw01       s              1
005100060524     d wrkeofs01       s              1
005200060522     d wrksfl02        s              1
005300060517     d $video          s              3    inz('C01')
005400060517     d xx              s              2  0
005500060418
005600060418      *--------------------------------------------------------------*
005700060510      * schiere
005800060418      *--------------------------------------------------------------*
005900060418     d msg             s             78    dim(10) ctdata perrcd(1)
006000060523     d opz             s              2  0 dim(10)
006100060418
006200060418      *--------------------------------------------------------------*
006300060510      * ds interne/esterne
006400060418      *--------------------------------------------------------------*
006500060523     d                 ds
006600060523     d aar                     1      2  0
006700060523     d mmr                     3      4  0
006800060523     d ggr                     5      6  0
006900060523     d datar                   1      6  0
007000060523
007100060523     d                 ds
007200060523     d ggd                     1      2  0
007300060523     d mmd                     3      4  0
007400060523     d aad                     5      6  0
007500060523     d datad                   1      6  0
007600060418
007700060524     d dssp          e ds
007800060418     d kpjba         e ds
007900060523     d tibs34ds      e ds                  inz
008000060524     d trtb25ds      e ds
008100060510     d §azute        e ds                  extname(azute00f)
008200060510     d                                     dtaara
008300060510     d §datiute      e ds                  extname(ddatiute)
008400060510     d                                     dtaara
008500060510
008600060510     d psds           sds
008700060510     d  pgmname          *proc
008800060510
008900060510      *------------------------------------------------------------------------*
009000060510      * pgm richiamati
009100060510      *------------------------------------------------------------------------*
009200060510     d tibs34r         pr                  extpgm('TIBS34R')
009300060510     d  tibs34ds                           likeds(tibs34ds)
009400060523
009500060523     d trul19r         pr                  extpgm('TRUL19R')
009600060523     d  t19cod1                            like(t19cod)
009700060523     d  t19ord1                            like(t19ord)
009800060523     d  t19key1                            like(t19key)
009900060523     d  t19com1                            like(t19com)
010000060510
010100060510      *--------------------------------------------------------------*
010200060510      * costanti
010300060510      *--------------------------------------------------------------*
010400060510     d errore          c                   '1'
010500060510     d eseguito        c                   '0'
010600060510     d titolo          c                   const('Tabella Tariffe Particolari')
010700060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
010800060510     d                                     o/Ripristino  5=Visualizza')
010900060510     d opzs            c                   const('1=Scelta')
011000060418      *--------------------------------------------------------------*
011100060510
011200060510      /free
011300060510
011400060510       exsr sr_parm;
011500060517       exsr sr_video;
011600060510       exsr sr_uscita;
011700060510
011800060510       // ----------------------------------------------------------------------
011900060510       // elaborazione parametri ricevuti
012000060510       // ----------------------------------------------------------------------
012100060510       begsr sr_parm;
012200060510
012300060524        trtb25ds = kpjbu;
012400060510
012500060510        select;
012600060510       // ricerca e scleta
012700060524         when tb25fun = '1';
012800060510          *in10 = *on;
012900060510       // imposto le opzioni
013000060522          vc1opz = opzs;
013100060522          clear opz;
013200060523          opz(1) = 01;
013300060510       // gestione
013400060524         when  tb25fun = *blanks;
013500060510          *in10 = *off;
013600060517       // imposto le opzioni
013700060522          vc1opz = opzg;
013800060522          clear opz;
013900060523          opz(1) = 02;
014000060523          opz(2) = 03;
014100060523          opz(3) = 04;
014200060523          opz(4) = 05;
014300060510       // altrimenti
014400060510         other;
014500060517       // esco
014600060524          tb25esito = errore;
014700060510          exsr sr_uscita;
014800060510         endsl;
014900060510
015000060510       endsr;
015100060517
015200060517       // ----------------------------------------------------------------------
015300060517       // gestione delle videate
015400060517       // ----------------------------------------------------------------------
015500060517       begsr sr_video;
015600060517
015700060522        dow $video <> *blanks;
015800060522
015900060522         select;
016000060517       // subfile 01 - tutte le tar.particolari presenti
016100060522          when $video = 'C01';
016200060522           exsr sr_c01;
016300060517
016400060517       // video 01 - nessuna tar.particolare presente
016500060522          when $video = 'D01';
016600060522           exsr sr_d01;
016700060517
016800060517       // video 02 - gestione/visualizzazione tar.particolare scelta
016900060522          when $video = 'D02';
017000060522           exsr sr_d02;
017100060522
017200060522         endsl;
017300060522
017400060522        enddo;
017500060517
017600060517       endsr;
017700060510
017800060510       // ----------------------------------------------------------------------
017900060510       // gestione subfile
018000060510       // ----------------------------------------------------------------------
018100060510       begsr sr_c01;
018200060510
018300060510       // caricamento subfile
018400060517         exsr sr_cars01;
018500060511
018600060511       // se scritto almeno un record
018700060517         if s01nrr > 0;
018800060511       // indicatore di sflend
018900060523          *in22 = *on;
019000060517         endif;
019100060511
019200060511       // se non scritto neanche un record
019300060517         if s01nrr = 0;
019400060511       // emetto videata di dati non trovati
019500060517          $video = 'D01';
019600060524          leavesr;
019700060517         endif;
019800060510
019900060523         if rrrn01 > 0;
020000060523          rec01 = rrrn01;
020100060523         else;
020200060523          rec01 = 1;
020300060517         endif;
020400060523
020500060523       // se non so quale pagina visualizzare forzo pagina 1
020600060523          if rec01 < 1;
020700060523           rec01 = 1;
020800060523          endif;
020900060510
021000060510       // Emissione del subfile
021100060524          write tb25t01;
021200060524          write tb25z01;
021300060524          exfmt tb25c01;
021400060522
021500060522       // reset indicatore generico di errore
021600060522         *in28 = *off;
021700060522       // pulisco il campo messaggi
021800060522         clear vc1msg;
021900060510
022000060510       // controllo tasti funzionali del subfile
022100060510          select;
022200060510
022300060510       // F3=Fine
022400060510           when *inkc;
022500060510            $video = *blanks;
022600060524            tb25ricez = 'C';
022700060510            exsr sr_uscita;
022800060510
022900060510       // F5=Refresh
023000060510           when *inke;
023100060510            wrkcars01 = *on;
023200060510
023300060510       // F10=Immissione
023400060510           when *inkj;
023500060511            $video = 'D02';
023600060524            clear trtb25ds;
023700060524            tb25comand = 'J';
023800060510
023900060510       // F13=Ripetizione
024000060510           when *inkm;
024100060510            exsr sr_ripetiopz;
024200060510
024300060510       // in tutti gli altri casi
024400060510           other;
024500060510       // controllo ed elaborazione scelte su subfile
024600060510            exsr sr_gestsfl;
024700060510          endsl;
024800060510
024900060510        endsr;
025000060522
025100060522       // ----------------------------------------------------------------------
025200060522       // gestione videata
025300060522       // ----------------------------------------------------------------------
025400060522       begsr sr_d01;
025500060522
025600060522       // emetto il video
025700060524          write tb25t01;
025800060524          exfmt tb25d01;
025900060522
026000060522       // controllo tasti funzionali del video
026100060522         select;
026200060522
026300060522       // F3=Fine
026400060522          when *inkc;
026500060524           tb25ricez = 'C';
026600060522           $video = *blanks;
026700060522           exsr sr_uscita;
026800060522           leavesr;
026900060522
027000060522       // F10=Immissione
027100060522          when *inkj;
027200060522           $video = 'D02';
027300060524           clear trtb25ds;
027400060524           tb25comand = 'J';
027500060522           leavesr;
027600060522
027700060522         endsl;
027800060522
027900060522       endsr;
028000060522
028100060522       // ----------------------------------------------------------------------
028200060522       // gestione videata
028300060522       // ----------------------------------------------------------------------
028400060522       begsr sr_d02;
028500060522
028600060522       // imposto il video a seconda della funzione richiesta
028700060522         exsr sr_setvideo;
028800060605
028900060605       // fino a che la variabile resta settata come 'D02'
029000060605        dow $video = 'D02';
029100060522
029200060522       // se immessa opzione di 'scelta'
029300060524         if tb25opzio = 01;
029400060522          $video = *blanks;
029500060524          tb25cod = vs1cod;
029600060524          tb25des = vs1des;
029700060522          exsr sr_uscita;
029800060522          leavesr;
029900060522         endif;
030000060522
030100060522       // se non è immissione/copia proteggo il codice tabella
030200060524          if tb25comand <> 'J' and tb25opzio <> 03;
030300060522           *in02 = *on;
030400060522          endif;
030500060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
030600060522       // proteggo i campi del video
030700060524          if tb25opzio = 04 or tb25opzio = 05;
030800060522           *in01 = *on;
030900060522          endif;
031000060522       // se immessa opzione di 'visualizzazione'
031100060522       // non attivo F6
031200060524          if tb25opzio = 05;
031300060522           *in03 = *on;
031400060522          endif;
031500060522
031600060522       // emetto il video
031700060524          write tb25t01;
031800060524          exfmt tb25d02;
031900060522
032000060522       // reset indicatore generico di errore
032100060522         *in28 = *off;
032200060522       // pulisco il campo messaggi
032300060522         clear v2cmsg;
032400060522
032500060522       // controllo tasti funzionali del video
032600060522         select;
032700060522
032800060522       // F3=Fine
032900060522          when *inkc;
033000060524           tb25ricez = 'C';
033100060522           $video = *blanks;
033200060522           exsr sr_uscita;
033300060522           leavesr;
033400060522
033500060522       // F6=Conferma
033600060522          when *inkf;
033700060524           tb25ricez = 'F';
033800060522       // controllo e decodifico i dati del video
033900060522           exsr sr_ctrd02;
034000060522       // se non riscontrati errori emetto la finestra con i dati per la
034100060522       // tramissione
034200060522           if not *in28;
034300060522            wrkcard02 = *on;
034400060523            wrkcars01 = *off;
034500060522            wrkcarw01 = *on;
034600060522            $video = 'W01';
034700060524            exsr sr_w01;
034800060523            leavesr;
034900060522           endif;
035000060523
035100060523       // F8=Record successivo
035200060524          when *inkh;
035300060524            wrkcars01 = *off;
035400060524            wrkcard02 = *on;
035500060523           $video = 'C01';
035600060524           tb25ricez = 'H';
035700060523           leavesr;
035800060522
035900060522       // F9=Traduzione
036000060524          when *inki;
036100060522           *in05 = *on;
036200060522           $video = 'C02';
036300060523           exsr sr_c02;
036400060522
036500060522       // F12=Ritorno
036600060522          when *inkl;
036700060522           wrkcard02 = *on;
036800060524           wrkcars01 = *off;
036900060524           tb25ricez = 'L';
037000060524           if tb25comand = 'J' or tb25opzio = 03;
037100060523            wrkcars01 = *on;
037200060522           endif;
037300060522           $video = 'C01';
037400060522           leavesr;
037500060522
037600060522       // Invio
037700060522          other;
037800060522           if not *in01;
037900060522            exsr sr_ctrd02;
038000060522           endif;
038100060522
038200060522         endsl;
038300060605
038400060605       // fine gestione 'D02'
038500060605        enddo;
038600060522
038700060522        wrkcard02 = *off;
038800060522
038900060522       endsr;
039000060510
039100060510       // ----------------------------------------------------------------------
039200060510       // caricamento subfile
039300060510       // ----------------------------------------------------------------------
039400060510       begsr sr_cars01;
039500060510
039600060510       // se è stato richiesto il caricamento del subfile
039700060510        if wrkcars01 = *on;
039800060510
039900060510       // inizializzo il subfile
040000060510         s01nrr = 0;
040100060510         *in20 = *off;
040200060510         *in21 = *off;
040300060523         *in22 = *on;
040400060524         write tb25c01;
040500060510         *in20 = *on;
040600060510         *in21 = *on;
040700060510
040800060510       // imposto la chiave di posizionamento e lettura file
040900060510         tblkut = 1;
041000060524         tblcod = 'SP';
041100060517         tblkey = vc1cod;
041200060510
041300060510       // posizionamento sul file
041400060510       // se è stato scelto il posizionamento
041500060517         if vc1cod <> *blanks;
041600060524          rrrn01 = 0;
041700060522          setll (tblkut:tblcod:tblkey) tabel00f;
041800060510       // altrimenti
041900060510         else;
042000060522          setll (tblkut:tblcod) tabel00f;
042100060510         endif;
042200060510
042300060510       // fino a che non è fine file
042400060510         dou %eof(tabel00f);
042500060510
042600060510       // leggo file
042700060510          reade(n) (tblkut:tblcod) tabel00f;
042800060510
042900060510       // fine file esco
043000060510          if %eof(tabel00f);
043100060510           leave;
043200060510          endif;
043300060510
043400060510       // se "ricerca/scelta" non carico i records annullati
043500060510          if (*in10 and tblflg = *blanks) or not *in10;
043600060510       // scrivo subfile
043700060517           clear vs1opz;
043800060510           exsr sr_wtrs01;
043900060510          endif;
044000060510
044100060510         enddo;
044200060510
044300060510       // fine caricamento subfile
044400060510        endif;
044500060510
044600060510       // disattivo opzione di caricamento subfile
044700060510        wrkcars01 = *off;
044800060510
044900060510       endsr;
045000060510
045100060510       // ----------------------------------------------------------------------
045200060510       // scrivo subfile
045300060510       // ----------------------------------------------------------------------
045400060510       begsr sr_wtrs01;
045500060510
045600060510       // se non raggiunto limite massimo di caricamento
045700060510        if s01nrr < 9999;
045800060510       // imposto campi di subfile
045900060510         exsr sr_sets01;
046000060510       // imposto numeratore righe e numero relativo di record
046100060510         s01nrr = s01nrr + 1;
046200060510       // scrivo subfile
046300060524         write tb25s01;
046400060510        endif;
046500060510
046600060510       endsr;
046700060510
046800060510       // ----------------------------------------------------------------------
046900060510       // imposto campi del subfile
047000060510       // ----------------------------------------------------------------------
047100060510       begsr sr_sets01;
047200060510
047300060522        vs1cod = tblkey;
047400060524        dssp = tbluni;
047500060524        vs1des = §spdes;
047600060524        vs1dfe = §spdfe;
047700060517
047800060517        clear vs1atb;
047900060522        if tblflg <> *blanks;
048000060517         vs1atb = 'A';
048100060517        endif;
048200060510
048300060510       endsr;
048400060517
048500060517       // ----------------------------------------------------------------------
048600060517       // gestione del subfile
048700060517       // ----------------------------------------------------------------------
048800060517       begsr sr_gestsfl;
048900060517
049000060517        wrkeofs01 = *off;
049100060522        clear s01nrr;
049200060523        *in39 = *off;
049300060517
049400060517       // inizio lettura subfile
049500060522        dow wrkeofs01 = *off;
049600060522         s01nrr = s01nrr + 1;
049700060524         chain s01nrr tb25s01;
049800060522
049900060522       // se non trovo esco
050000060522         if not %found;
050100060524          if vc1cod <> *blanks;
050200060524           wrkcars01 = *on;
050300060524          endif;
050400060522          leavesr;
050500060517         endif;
050600060522
050700060517       // se è stata immessa un'opzione
050800060523         if vs1opz <> *zeros;
050900060517       // controllo se opzione valida
051000060517          xx = %lookup(vs1opz:opz);
051100060517          if xx = *zeros;
051200060517           rec01 = s01nrr;
051300060522           vc1msg = msg(03);
051400060517           *in28 = *on;
051500060523           *in39 = *on;
051600060524           update tb25s01;
051700060518           leavesr;
051800060517          endif;
051900060522
052000060517       // reset ds di servizio
052100060524          clear trtb25ds;
052200060524          tb25opzio = vs1opz;
052300060517
052400060523       // gestione del formato video
052500060522          rec01 = s01nrr;
052600060522          $video = 'D02';
052700060523          exsr sr_d02;
052800060524       // se F12 di ritorno esco dalla lettura del subfile
052900060524          if tb25ricez = 'L';
053000060524          wrkeofs01 = *on;
053100060523          endif;
053200060518         endif;
053300060517
053400060523         clear vs1opz;
053500060522         rec01 = s01nrr;
053600060524         update tb25s01;
053700060517        enddo;
053800060517
053900060517       endsr;
054000060517
054100060517       // ----------------------------------------------------------------------
054200060517       // ripeto opzione in tutte le righe del subfile
054300060517       // ----------------------------------------------------------------------
054400060517       begsr sr_ripetiopz;
054500060517
054600060523        if rrrn01 > 0;
054700060523         s01nrr = rrrn01;
054800060524         chain s01nrr tb25s01;
054900060523         if %found and vs1opz <> *zeros;
055000060522          savopzione = vs1opz;
055100060524          update tb25s01;
055200060517
055300060517          wrkeofs01 = *off;
055400060522          dow wrkeofs01 = *off;
055500060517           s01nrr = s01nrr + 1;
055600060524           chain s01nrr tb25s01;
055700060517           if %found;
055800060522            vs1opz = savopzione;
055900060524            update tb25s01;
056000060517           else;
056100060517            wrkeofs01 = *on;
056200060517           endif;
056300060517          enddo;
056400060517
056500060517         endif;
056600060517
056700060517        endif;
056800060517
056900060517       endsr;
057000060517
057100060517       // ----------------------------------------------------------------------
057200060517       // imposto i dati a video
057300060517       // ----------------------------------------------------------------------
057400060517       begsr sr_setvideo;
057500060517
057600060524        if wrkcard02 = *on;
057700060518       // pulisco il formato video D02
057800060524         exsr sr_puld02;
057900060517
058000060522       // controllo se 'immissione' 'modifica' 'copia' o altro
058100060524         select;
058200060517
058300060517       // F10=Immissione
058400060524          when tb25comand = 'J';
058500060524           vt1dopz = 'Immissione';
058600060517
058700060523       // Opzione "02"=modifica
058800060524          when tb25opzio = 02;
058900060524           vt1dopz = 'Modifica';
059000060524           exsr sr_imposta;
059100060517
059200060523       // Opzione "03"=copia
059300060524          when tb25opzio = 03;
059400060524           vt1dopz = 'Copia';
059500060524           exsr sr_imposta;
059600060517
059700060523       // Opzione "04"=cancellazione/ripristino
059800060524          when tb25opzio = 04;
059900060524           exsr sr_imposta;
060000060517       // se richiesta 'cancellazione'
060100060524           if tblflg = *blanks;
060200060524            vt1dopz = 'Annullamento';
060300060524           endif;
060400060517       // se richiesto 'ripristino'
060500060524           if tblflg <> *blanks;
060600060524            vt1dopz = 'Ripristino';
060700060524           endif;
060800060517
060900060523       // Opzione "05"=visualizzazione
061000060524          when tb25opzio =05;
061100060524           vt1dopz = 'Visualizzazione';
061200060524           exsr sr_imposta;
061300060517
061400060517       // Fine scelta
061500060524         endsl;
061600060517
061700060517       // centro la descrizione della funzione nel formato video
061800060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
061900060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
062000060524
062100060524        endif;
062200060517
062300060517       endsr;
062400060524
062500060524       // ----------------------------------------------------------------------
062600060524       // pulizia video
062700060524       // ----------------------------------------------------------------------
062800060524       begsr sr_puld02;
062900060524
063000060524        clear vt1dopz;
063100060524        clear v2ccod;
063200060524        clear v2cdes;
063300060524        clear v2cdfe;
063400060524        clear v2cdac;
063500060524        clear v2csud;
063600060524        clear v2caba;
063700060524
063800060524        *in01 = *off;
063900060524        *in02 = *off;
064000060524        *in03 = *off;
064100060524        *in04 = *off;
064200060524        *in05 = *off;
064300060524        *in28 = *off;
064400060524        *in40 = *off;
064500060524        *in41 = *off;
064600060524
064700060524       endsr;
064800060524
064900060524       // ----------------------------------------------------------------------
065000060524       // imposto i dati a video
065100060524       // ----------------------------------------------------------------------
065200060524       begsr sr_imposta;
065300060524
065400060524       // recupero i dati dal file
065500060524        tblkut = 1;
065600060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
065700060524
065800060524       // imposto i campi a video
065900060524        v2ccod = tblkey;
066000060524        dssp = tbluni;
066100060524        v2cdes = §spdes;
066200060524        v2cdfe = §spdfe;
066300060524        v2cdac = §spdac;
066400060524        v2csud = §spsud;
066500060524        v2caba = §spaba;
066600060524
066700060524        if tblflg <> *blanks;
066800060524         *in04 = *on;
066900060524        endif;
067000060524
067100060524       endsr;
067200060517
067300060517       // ----------------------------------------------------------------------
067400060517       // controllo video
067500060517       // ----------------------------------------------------------------------
067600060522       begsr sr_ctrd02;
067700060517
067800060517        *in28 = *off;
067900060517        *in40 = *off;
068000060517        *in41 = *off;
068100060517
068200060522       // codice tabella
068300060522        if v2ccod = *blanks;
068400060522         v2cmsg = msg(01);
068500060517         *in28 = *on;
068600060517         *in40 = *on;
068700060517         leavesr;
068800060517        endif;
068900060517
069000060517       // se immissione controllo che non esista già
069100060524        if tb25comand = 'J' or tb25opzio = 03;
069200060522         tblkut = 1;
069300060524         tblcod = 'SP';
069400060522         tblkey = v2ccod;
069500060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
069600060522         if %found(tabel00f);
069700060522          v2cmsg = msg(05);
069800060517          *in28 = *on;
069900060517          *in40 = *on;
070000060517          leavesr;
070100060517         endif;
070200060517        endif;
070300060517
070400060517       // descrizione
070500060522        if v2cdes = *blanks;
070600060522         v2cmsg = msg(02);
070700060517         *in28 = *on;
070800060517         *in41 = *on;
070900060517         leavesr;
071000060517        endif;
071100060517
071200060517       endsr;
071300060524
071400060524       // ----------------------------------------------------------------------
071500060524       // gestione video dati di trasmissione
071600060524       // ----------------------------------------------------------------------
071700060524       begsr sr_w01;
071800060524
071900060524       // imposto i dati a video
072000060524        exsr sr_carw01;
072100060524
072200060524       // fino a che la variabile resta settata come 'W01'
072300060524        dow $video = 'W01';
072400060524
072500060524       // reset indicatore generico di errore
072600060524         *in28 = *off;
072700060524
072800060524       // emetto il video
072900060524          exfmt tb25w01;
073000060524
073100060524       // controllo tasti funzionali del video
073200060524         select;
073300060524
073400060524       // F6=Conferma
073500060524          when *inkf;
073600060524       // controllo i dati del video
073700060524           exsr sr_ctrw01;
073800060524       // se non riscontrati errori aggiorno il record corrente
073900060524           if not *in28;
074000060524            exsr sr_aggiorna;
074100060524            if not *in28;
074200060524       // emetto le descrizioni in lingua
074300060524       // se non è annullo/ripristino
074400060524             if tb25opzio <> 04;
074500060524              *in05 = *off;
074600060524              $video = 'C02';
074700060524              exsr sr_c02;
074800060524              leavesr;
074900060524             endif;
075000060524             if tb25comand = 'J';
075100060524              $video = 'D02';
075200060524              exsr sr_setvideo;
075300060524             else;
075400060524              wrkcars01 = *on;
075500060524              $video = 'C01';
075600060524             endif;
075700060524            endif;
075800060524           endif;
075900060524
076000060524       // F12=Ritorno
076100060524          when *inkl;
076200060524           $video = 'D02';
076300060524           clear tb25ricez;
076400060524
076500060524       // Invio
076600060524          other;
076700060524           exsr sr_ctrw01;
076800060524
076900060524         endsl;
077000060524
077100060524       // fine gestione 'W01'
077200060524        enddo;
077300060524
077400060524       endsr;
077500060517
077600060517       // ----------------------------------------------------------------------
077700060517       // imposto i dati di trasmissione
077800060517       // ----------------------------------------------------------------------
077900060517       begsr sr_carw01;
078000060517
078100060517       // se pulisco i campi
078200060522         clear w01ftt;
078300060522         clear w01flt;
078400060522         clear w01ftr;
078500060522         clear w01dtr;
078600060517
078700060517       // se non immissione imposto i campi del file
078800060524        if tb25comand <> 'J';
078900060522         w01ftt = tblftt;
079000060522         w01flt = tblflt;
079100060522         w01ftr = tblftr;
079200060517       // imposto la data
079300060522         if tbldtr <> *zeros;
079400060523          datar = tbldtr;
079500060523          aad = aar;
079600060523          mmd = mmr;
079700060523          ggd = ggr;
079800060523          w01dtr = datad;
079900060517         endif;
080000060517
080100060517        endif;
080200060517
080300060517       endsr;
080400060517
080500060517       // ----------------------------------------------------------------------
080600060517       // controllo i dati di trasmissione
080700060517       // ----------------------------------------------------------------------
080800060517       begsr sr_ctrw01;
080900060517
081000060517       endsr;
081100060517
081200060517       // ----------------------------------------------------------------------
081300060517       // aggiorno tabella
081400060517       // ----------------------------------------------------------------------
081500060517       begsr sr_aggiorna;
081600060517
081700060517       // imposto campi del file
081800060522        tblkut = 1;
081900060524        tblcod = 'SP';
082000060522        tblkey = v2ccod;
082100060523
082200060523       // aggancio il rcd della tabella
082300060523        chain (tblkut:tblcod:tblkey) tabel00f;
082400060517
082500060524        §spdes = v2cdes;
082600060524        §spdfe = v2cdfe;
082700060524        §spdac = v2cdac;
082800060524        §spsud = v2csud;
082900060524        §spaba = v2caba;
083000060517
083100060524        tbluni = dssp;
083200060517
083300060522        tblftt = w01ftt;
083400060522        tblflt = w01flt;
083500060522        clear tblftr;
083600060522        clear tbldtr;
083700060524
083800060524       // annullo/ripristino
083900060524        if tb25opzio = 04;
084000060524         select;
084100060524       // annullamento
084200060524          when tblflg = *blanks;
084300060524           tblflg = '*';
084400060524       // ripristino
084500060524          when tblflg <> *blanks;
084600060524           clear tblflg;
084700060524         endsl;
084800060524        endif;
084900060517
085000060523       // controllo se devo scrivere o aggiornare il record
085100060523        if %found(tabel00f);
085200060523         update tabel;
085300060523        else;
085400060523         write tabel;
085500060523        endif;
085600060517
085700060523       // se Opzione "04"=cancellazione/ripristino
085800060517       // aggiorno automaticamente anche le descrizioni in linea
085900060524        if tb25opzio = 04;
086000060522         lintabel = 1;
086100060522         setgt lintabel azlin01l;
086200060522         dou %eof(azlin01l);
086300060522          read azlin01l;
086400060517       // fine file
086500060522          if %eof(azlin01l);
086600060517           leave;
086700060517          endif;
086800060517       // aggiorno
086900060522          tblkut = lintabel;
087000060522          chain (tblkut:tblcod:tblkey) tabel00f;
087100060522          if %found(tabel00f);
087200060517           select;
087300060522            when tblflg <> *blanks;
087400060522             clear tblflg;
087500060523            when tblflg = *blanks;
087600060522             tblflg = '*';
087700060517           endsl;
087800060522            update tabel;
087900060517          endif;
088000060517         enddo;
088100060517        endif;
088200060517
088300060517       endsr;
088400060524
088500060524       // ----------------------------------------------------------------------
088600060524       // gestione descrizioni in lingua
088700060524       // ----------------------------------------------------------------------
088800060524       begsr sr_c02;
088900060524
089000060524       // imposto il codice tabella e la descrizione
089100060524       vc2cod = v2ccod;
089200060524       vc2des = v2cdes;
089300060524       vc2dfe = v2cdfe;
089400060524
089500060524       // pulisco il subfile
089600060524        exsr sr_puls02;
089700060524
089800060524       // carico il subfile con i dati che trovo o vuoto
089900060524        exsr sr_cars02;
090000060524
090100060524        wrksfl02 = *on;
090200060524
090300060524        dou wrksfl02 = *off;
090400060524         rec02 = 1;
090500060524         if savrec > *zeros;
090600060524          rec02 = savrec;
090700060524         endif;
090800060524         write tb25z02;
090900060524         exfmt tb25c02;
091000060524
091100060524         *in28 = *off;
091200060524         *in39 = *off;
091300060524
091400060524       // F12=Ritorno
091500060524          if *inkl;
091600060524           wrkcard02 = *off;
091700060524           $video = 'D02';
091800060524           leavesr;
091900060524          endif;
092000060524
092100060524       // Controlli
092200060524          if not *in05;
092300060524           exsr sr_ctrsfl;
092400060524          endif;
092500060524
092600060524       // F6=Conferma
092700060524          if *inkf and not *in28;
092800060524           exsr sr_confsfl;
092900060524           wrksfl02 = *off;
093000060524          endif;
093100060524
093200060524         enddo;
093300060524
093400060524       endsr;
093500060524
093600060524       // ----------------------------------------------------------------------
093700060524       // pulisco il subfile delle descrizioni in lingua
093800060524       // ----------------------------------------------------------------------
093900060524       begsr sr_puls02;
094000060524
094100060524        clear s02nrr;
094200060524        clear savrec;
094300060524        *in20 = *off;
094400060524        *in21 = *off;
094500060524        write tb25c02;
094600060524        *in20 = *on;
094700060524        *in21 = *on;
094800060524
094900060524       endsr;
095000060524
095100060524       // ----------------------------------------------------------------------
095200060524       // carico il subfile delle descrizioni in lingua
095300060524       // ----------------------------------------------------------------------
095400060524       begsr sr_cars02;
095500060524
095600060524        *in22 = *off;
095700060524       // carico un record per ogni lingua inserite in AZLIN
095800060524        lintabel = 1;
095900060524        setgt lintabel azlin01l;
096000060524        dou %eof(azlin01l);
096100060524         read azlin01l;
096200060524       // fine file
096300060524         if %eof(azlin01l);
096400060524          leave;
096500060524         endif;
096600060524         clear dssp;
096700060524         s2dlin = lindesita;
096800060524         tblkut = lintabel;
096900060524         tblcod = 'SP';
097000060524         tblkey = v2ccod;
097100060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
097200060524         if not %found(tabel00f);
097300060524          clear s2cdes;
097400060727          clear s2cdfe;
097500060524         else;
097600060524          dssp = tbluni;
097700060524          s2cdes = §spdes;
097800060524          s2cdfe = §spdfe;
097900060524         endif;
098000060524         s2hcdlin = tblkut;
098100060524         s02nrr = s02nrr + 1;
098200060524         write tb25s02;
098300060524        enddo;
098400060524
098500060524        *in22 = *on;
098600060524
098700060524       endsr;
098800060524
098900060524       // ----------------------------------------------------------------------
099000060524       // controllo il subfile delle descrizioni in lingua
099100060524       // ----------------------------------------------------------------------
099200060524       begsr sr_ctrsfl;
099300060524
099400060524        s02nrr = 1;
099500060524        chain s02nrr tb25s02;
099600060524        dow %found;
099700060524       // deve esserci la descrizione
099800060524         if s2cdes = *blanks;
099900060524          vc2msg = msg(02);
100000060524          *in28 = *on;
100100060524          *in39 = *on;
100200060524          savrec = s02nrr;
100300060524          update tb25s02;
100400060524          leave;
100500060524         endif;
100600060524         update tb25s02;
100700060524         s02nrr = s02nrr + 1;
100800060524         chain s02nrr tb25s02;
100900060524        enddo;
101000060524
101100060524       endsr;
101200060524
101300060524       // ----------------------------------------------------------------------
101400060524       // confermo il dati del subfile delle descrizioni in lingua
101500060524       // ----------------------------------------------------------------------
101600060524       begsr sr_confsfl;
101700060524
101800060524       // recupero i dati della tabella in italiano
101900060524        clear savtbluni;
102000060524        tblkut = 1;
102100060524        tblcod = 'SP';
102200060524        tblkey = v2ccod;
102300060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
102400060524        if %found(tabel00f);
102500060524         savtbluni = tbluni;
102600060524        endif;
102700060524
102800060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
102900060524        s02nrr = 1;
103000060524        chain s02nrr tb25s02;
103100060524        dow %found;
103200060524         tblkut = s2hcdlin;
103300060524         chain (tblkut:tblcod:tblkey) tabel00f;
103400060524         dssp = savtbluni;
103500060524         §spdes = s2cdes;
103600060524         §spdfe = s2cdfe;
103700060524         tbluni = dssp;
103800060524         tblftt = w01ftt;
103900060524         clear tblftr;
104000060524         clear tbldtr;
104100060524       // scrivo
104200060524         if not %found(tabel00f);
104300060524          tblkut = s2hcdlin;
104400060524          tblcod = 'SP';
104500060524          tblkey = v2ccod;
104600060524          write tabel;
104700060524         else;
104800060524       // aggiorno
104900060524          update tabel;
105000060524         endif;
105100060524         s02nrr = s02nrr + 1;
105200060524         chain s02nrr tb25s02;
105300060524        enddo;
105400060524
105500060524        wrkcars01 = *on;
105600060524        $video = 'C01';
105700060524
105800060524       endsr;
105900060524
106000060524       // ----------------------------------------------------------------------
106100060524       // uscita dal programma
106200060524       // ----------------------------------------------------------------------
106300060524         begsr sr_uscita;
106400060524
106500060524          if tb25esito = *blanks;
106600060524           tb25esito = eseguito;
106700060524          endif;
106800060524
106900060524          kpjbu = trtb25ds;
107000060524
107100060524          *inlr = *on;
107200060524          return;
107300060524
107400060524         endsr;
107500060510
107600060510       // ----------------------------------------------------------------------
107700060510       // routine iniziale
107800060510       // ----------------------------------------------------------------------
107900060510         begsr *inzsr;
108000060510
108100060510      /end-free
108200060510
108300060510     c     *entry        plist
108400060510     c                   parm                    kpjba
108500060510
108600060510      /free
108700060510         in(e) §azute;
108800060510         if not %error;
108900060510          in(e) §datiute;
109000060510         endif;
109100060510         if %error or rsut = *blanks;
109200060510          tibs34r(tibs34ds);
109300060510          in §azute;
109400060510          in §datiute;
109500060510         endif;
109600060510
109700060510       // centro il titolo
109800060510        vt1tit = titolo;
109900060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
110000060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
110100060510
110200060510         endsr;
110300060510
110400060510      /end-free
110500060418
110600060418** -- MSG -------------------------------------------------------------------*
110700060418Immettere il codice                                                            1
110800060418Immettere la descrizione                                                       2
110900060522Opzione errata                                                                 3
111000060523Data errata                                                                    4
111100060522Codice errato                                                                  5
111200060524Tipo tariffa errato                                                            6
111300060524Codice tassazione errato                                                       7
