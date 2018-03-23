000100060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200120614     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000300060418
000400060418      *--------------------------------------------------------------*
000500120614      * GESTIONE TABELLA ESENZIONI IVA                               *
000600060418      *--------------------------------------------------------------*
000700060418
000800060418     ftabel00f  uf a e           k disk
000900120614     FAnRiv01L  IF   E           K DISK
001000120614     ftrtb10d   cf   e             workstn sfile(tb10s01:s01nrr)
001100060418      *---------------------------------------------------------------*
001200060510      * riepilogo indicatori
001300060418      *---------------------------------------------------------------*
001400060522      * 01 - protegge campi video 02
001500060510      * 02 - variazione
001600060522      * 03 - non attiva F6 video 02
001700060524      * 04 - rcd annullato
001800060510      * 10 - ON ricerca/scelta -- OFF gestione
001900060510      * 20 - sfldsp
002000060510      * 21 - sfldspctl -- sflclr
002100060510      * 22 - sflend
002200060524      * 28 - visualizza messaggio errore
002300060524      * 39 - errore subfile
002400060524      * 40 - codice tabella
002500060524      * 41 - descrizione
002600120614      * 42 - data inizio
002700120614      * 43 - data fine
002800060418      *--------------------------------------------------------------*
002900060418
003000060418      *--------------------------------------------------------------*
003100060510      * campi di work
003200060418      *--------------------------------------------------------------*
003300060522     d blanks          s                   like(vt1dopz)
003400060418     d comando         s              1
003500120614     d data_eur        s               d   Datfmt(*eur)
003600120614     d data_iso        s               d   Datfmt(*iso)
003700060510     d len             s              5u 0
003800060510     d savrec          s                   like(s01nrr)
003900060517     d savopzione      s                   like(vs1opz)
004000060531     d savtblftt       s                   like(tblftt)
004100060531     d savtblflt       s                   like(tblflt)
004200060510     d savtbluni       s                   like(tbluni)
004300060510     d s01nrr          s              4  0
004400060523     d t19cod          s                   like(tblcod)
004500060523     d t19ord          s              1
004600060523     d t19key          s                   like(tblkey)
004700060523     d t19com          s              1
004800120614     d wdata           s              8  0 inz
004900120614     d wdatai          s              8  0 inz
005000120614     d wdataf          s              8  0 inz
005100060522     d wrkcard02       s              1    inz('1')
005200060522     d wrkcars01       s              1    inz('1')
005300060522     d wrkcarw01       s              1
005400060524     d wrkeofs01       s              1
005500060517     d $video          s              3    inz('C01')
005600060517     d xx              s              2  0
005700060524     d yy              s              2  0
005800060418
005900060418      *--------------------------------------------------------------*
006000060510      * schiere
006100060418      *--------------------------------------------------------------*
006200060418     d msg             s             78    dim(10) ctdata perrcd(1)
006300060523     d opz             s              2  0 dim(10)
006400060418
006500060418      *--------------------------------------------------------------*
006600060510      * ds interne/esterne
006700060418      *--------------------------------------------------------------*
006800120614
006900120614     d                 ds
007000120614     d aar                     1      2  0
007100120614     d mmr                     3      4  0
007200120614     d ggr                     5      6  0
007300120614     d datar                   1      6  0
007400120614
007500120614     d                 ds
007600120614     d ggd                     1      2  0
007700120614     d mmd                     3      4  0
007800120614     d aad                     5      6  0
007900120614     d datad                   1      6  0
008000060418
008100120614     d dsei          e ds
008200120614
008300060418     d kpjba         e ds
008400120614     d  MsgErr               453    502
008500120614
008600120614     d Tibs02ds      e ds
008700060523     d tibs34ds      e ds                  inz
008800120614     d trtb10ds      e ds
008900120614     d trul12ds      e ds
009000120614     d XSoc001DS     e ds                  inz
009100120614
009200060510     d §azute        e ds                  extname(azute00f)
009300060510     d                                     dtaara
009400060510     d §datiute      e ds                  extname(ddatiute)
009500060510     d                                     dtaara
009600120614
009700120614     d wlbdat          ds                  inz
009800120614     d  g02dat                 1      8  0
009900120614     d  g02inv                 9     16  0
010000120614     d  g02err                17     17
010100120614     d  g02tgi                18     22  0
010200060510
010300060510     d psds           sds
010400060510     d  pgmname          *proc
010500060510
010600060510      *------------------------------------------------------------------------*
010700060510      * pgm richiamati
010800060510      *------------------------------------------------------------------------*
010900120614      /copy gaitrasrc/srcprotopr,tibs02r
011000120614
011100060510     d tibs34r         pr                  extpgm('TIBS34R')
011200060510     d  tibs34ds                           likeds(tibs34ds)
011300120614
011400120614     d trul12r         pr                  extpgm('TRUL12R')
011500120614     d  trul12ds                           likeds(trul12ds)
011600060523
011700060523     d trul19r         pr                  extpgm('TRUL19R')
011800060523     d  t19cod1                            like(t19cod)
011900060523     d  t19ord1                            like(t19ord)
012000060523     d  t19key1                            like(t19key)
012100060523     d  t19com1                            like(t19com)
012200120614
012300120614      /copy gaitrasrc/srcprotopr,xsrda8
012400060510
012500060510      *--------------------------------------------------------------*
012600060510      * costanti
012700060510      *--------------------------------------------------------------*
012800060510     d errore          c                   '1'
012900060510     d eseguito        c                   '0'
013000120614     d titolo          c                   const('Tabella Esenzioni IVA')
013100060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
013200060510     d                                     o/Ripristino  5=Visualizza')
013300060510     d opzs            c                   const('1=Scelta')
013400120614     d Vendite         c                   '2'
013500060418      *--------------------------------------------------------------*
013600060510
013700120614     ** Mi collego ad una società.
013800120614     C                   CALLB     'XSOC'
013900120614     C                   PARM      'SOC001'      TipXsc            6
014000120614     C                   PARM      *BLANK        SocXsc            3
014100120614     C                   PARM      *ZERO         CdsXsc            9 0
014200120614     C                   PARM      *BLANK        ModXsc            3
014300120614     C                   PARM      *BLANK        RtnXsc            1
014400120614     C                   PARM                    XSoc001DS
014500120614     C                   PARM                    Kpjba
014600120614     C                   IF        RtnXsc = '1'
014700120614     C                   EVAL      MsgErr =
014800120614     C                             'Non è possibile collegarsi alla società '
014900120614     C                             + XscSoc + '.'
015000120614     C                   EVAL      *INLR = *ON
015100120614     C                   RETURN
015200120614     C                   ENDIF
015300120614
015400060510      /free
015500120614       // controllo se tabella modificabile
015600120614       // solo se non sono in ambiente di prova
015700120614        clear trul12ds;
015800120614        IF  %subst(knsif:7:1) <> 'P';
015900120614          I12tla = 'L';
016000120614          I12nsi = knsif;
016100120614          I12cod = 'EI';
016200120614          trul12r (trul12ds);
016300120614        ENDIF;
016400120614        IF O12err = *blanks;
016500060510
016600120614       exsr sr_parm;
016700060517       exsr sr_video;
016800120614
016900120614        ENDIF;
017000120614
017100060510       exsr sr_uscita;
017200060510
017300060510       // ----------------------------------------------------------------------
017400060510       // elaborazione parametri ricevuti
017500060510       // ----------------------------------------------------------------------
017600060510       begsr sr_parm;
017700060510
017800120614        trtb10ds = kpjbu;
017900060510
018000060510        select;
018100060510       // ricerca e scleta
018200120614         when tb10fun = '1';
018300060510          *in10 = *on;
018400060510       // imposto le opzioni
018500060522          vc1opz = opzs;
018600060522          clear opz;
018700060523          opz(1) = 01;
018800060510       // gestione
018900120614         when  tb10fun = *blanks;
019000060510          *in10 = *off;
019100110921          *in11 = *off;
019200060517       // imposto le opzioni
019300060522          vc1opz = opzg;
019400060522          clear opz;
019500060523          opz(1) = 02;
019600060523          opz(2) = 03;
019700060523          opz(3) = 04;
019800060523          opz(4) = 05;
019900060510       // altrimenti
020000060510         other;
020100060517       // esco
020200120614          tb10esito = errore;
020300060510          exsr sr_uscita;
020400060510         endsl;
020500060510
020600060510       endsr;
020700060517
020800060517       // ----------------------------------------------------------------------
020900060517       // gestione delle videate
021000060517       // ----------------------------------------------------------------------
021100060517       begsr sr_video;
021200060517
021300060522        dow $video <> *blanks;
021400060522
021500060522         select;
021600120614       // subfile 01 - tutti i codici presenti
021700060522          when $video = 'C01';
021800060522           exsr sr_c01;
021900060517
022000120614       // video 01 - nessun codice presente
022100060522          when $video = 'D01';
022200060522           exsr sr_d01;
022300060517
022400120614       // video 02 - gestione/visualizzazione codice scelto
022500060522          when $video = 'D02';
022600060522           exsr sr_d02;
022700060522
022800060522         endsl;
022900060522
023000060522        enddo;
023100060517
023200060517       endsr;
023300060510
023400060510       // ----------------------------------------------------------------------
023500060510       // gestione subfile
023600060510       // ----------------------------------------------------------------------
023700060510       begsr sr_c01;
023800060510
023900060510       // caricamento subfile
024000060517         exsr sr_cars01;
024100060511
024200060511       // se scritto almeno un record
024300060517         if s01nrr > 0;
024400060511       // indicatore di sflend
024500060523          *in22 = *on;
024600060517         endif;
024700060511
024800060511       // se non scritto neanche un record
024900060517         if s01nrr = 0;
025000060511       // emetto videata di dati non trovati
025100060517          $video = 'D01';
025200060524          leavesr;
025300060517         endif;
025400060510
025500060523         if rrrn01 > 0;
025600060523          rec01 = rrrn01;
025700060523         else;
025800060523          rec01 = 1;
025900060517         endif;
026000060523
026100060523       // se non so quale pagina visualizzare forzo pagina 1
026200060523          if rec01 < 1;
026300060523           rec01 = 1;
026400060523          endif;
026500060510
026600060510       // Emissione del subfile
026700120614          write tb10t01;
026800120614          write tb10z01;
026900120614          exfmt tb10c01;
027000060522
027100060522       // reset indicatore generico di errore
027200060522         *in28 = *off;
027300060522       // pulisco il campo messaggi
027400060522         clear vc1msg;
027500060510
027600060510       // controllo tasti funzionali del subfile
027700060510          select;
027800060510
027900060510       // F3=Fine
028000060510           when *inkc;
028100060510            $video = *blanks;
028200120614            tb10ricez = 'C';
028300060510            exsr sr_uscita;
028400060510
028500060510       // F5=Refresh
028600060510           when *inke;
028700060510            wrkcars01 = *on;
028800060510
028900060510       // F10=Immissione
029000060510           when *inkj;
029100060511            $video = 'D02';
029200120614            clear trtb10ds;
029300120614            tb10comand = 'J';
029400060510
029500060510       // F13=Ripetizione
029600060510           when *inkm;
029700060510            exsr sr_ripetiopz;
029800060510
029900060510       // in tutti gli altri casi
030000060510           other;
030100060510       // controllo ed elaborazione scelte su subfile
030200060510            exsr sr_gestsfl;
030300060510          endsl;
030400060510
030500060510        endsr;
030600060522
030700060522       // ----------------------------------------------------------------------
030800060522       // gestione videata
030900060522       // ----------------------------------------------------------------------
031000060522       begsr sr_d01;
031100060522
031200060522       // emetto il video
031300120614          write tb10t01;
031400120614          exfmt tb10d01;
031500060522
031600060522       // controllo tasti funzionali del video
031700060522         select;
031800060522
031900060522       // F3=Fine
032000060522          when *inkc;
032100120614           tb10ricez = 'C';
032200060522           $video = *blanks;
032300060522           exsr sr_uscita;
032400060522           leavesr;
032500060522
032600060522       // F10=Immissione
032700060522          when *inkj;
032800060522           $video = 'D02';
032900120614           clear trtb10ds;
033000120614           tb10comand = 'J';
033100060522           leavesr;
033200060522
033300060522         endsl;
033400060522
033500060522       endsr;
033600060522
033700060522       // ----------------------------------------------------------------------
033800060522       // gestione videata
033900060522       // ----------------------------------------------------------------------
034000060522       begsr sr_d02;
034100060522
034200060522       // imposto il video a seconda della funzione richiesta
034300060522         exsr sr_setvideo;
034400060522
034500060605       // fino a che la variabile resta settata come 'D02'
034600060605        dow $video = 'D02';
034700060605
034800060522       // se immessa opzione di 'scelta'
034900120614         if tb10opzio = 01;
035000060522          $video = *blanks;
035100120614          tb10cod = vs1cod;
035200120614          tb10des = vs1des;
035300060522          exsr sr_uscita;
035400060522          leavesr;
035500060522         endif;
035600060522
035700060522       // se non è immissione/copia proteggo il codice tabella
035800120614          if tb10comand <> 'J' and tb10opzio <> 03;
035900060522           *in02 = *on;
036000060522          endif;
036100060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
036200060522       // proteggo i campi del video
036300120614          if tb10opzio = 04 or tb10opzio = 05;
036400060522           *in01 = *on;
036500060522          endif;
036600060522       // se immessa opzione di 'visualizzazione'
036700060522       // non attivo F6
036800120614          if tb10opzio = 05;
036900060522           *in03 = *on;
037000060522          endif;
037100060522
037200060522       // emetto il video
037300120614          write tb10t01;
037400120614          exfmt tb10d02;
037500060522
037600060522       // reset indicatore generico di errore
037700060522         *in28 = *off;
037800060522       // pulisco il campo messaggi
037900060522         clear v2cmsg;
038000060522
038100060522       // controllo tasti funzionali del video
038200060522         select;
038300060522
038400060522       // F3=Fine
038500060522          when *inkc;
038600120614           tb10ricez = 'C';
038700060522           $video = *blanks;
038800060522           exsr sr_uscita;
038900060522           leavesr;
039000060522
039100060522       // F6=Conferma
039200060522          when *inkf;
039300120614           tb10ricez = 'F';
039400060522       // controllo e decodifico i dati del video
039500060522           exsr sr_ctrd02;
039600060522       // se non riscontrati errori emetto la finestra con i dati per la
039700060522       // tramissione
039800060522           if not *in28;
039900060522            wrkcard02 = *on;
040000060523            wrkcars01 = *off;
040100060522            wrkcarw01 = *on;
040200060522            $video = 'W01';
040300060524            exsr sr_w01;
040400060523            leavesr;
040500060522           endif;
040600060523
040700060523       // F8=Record successivo
040800060524          when *inkh;
040900060524            wrkcars01 = *off;
041000060524            wrkcard02 = *on;
041100060523           $video = 'C01';
041200120614           tb10ricez = 'H';
041300060523           leavesr;
041400060522
041500060522       // F12=Ritorno
041600060522          when *inkl;
041700060522           wrkcard02 = *on;
041800060524           wrkcars01 = *off;
041900120614           tb10ricez = 'L';
042000120614           if tb10comand = 'J' or tb10opzio = 03;
042100060523            wrkcars01 = *on;
042200060522           endif;
042300060522           $video = 'C01';
042400060522           leavesr;
042500060522
042600060522       // Invio
042700060522          other;
042800060522           if not *in01;
042900060522            exsr sr_ctrd02;
043000060522           endif;
043100060522
043200060522         endsl;
043300060605
043400060605       // fine gestione 'D02'
043500060605        enddo;
043600060522
043700060522        wrkcard02 = *off;
043800060522
043900060522       endsr;
044000060510
044100060510       // ----------------------------------------------------------------------
044200060510       // caricamento subfile
044300060510       // ----------------------------------------------------------------------
044400060510       begsr sr_cars01;
044500060510
044600060510       // se è stato richiesto il caricamento del subfile
044700060510        if wrkcars01 = *on;
044800060510
044900060510       // inizializzo il subfile
045000060510         s01nrr = 0;
045100060510         *in20 = *off;
045200060510         *in21 = *off;
045300060523         *in22 = *on;
045400120614         write tb10c01;
045500060510         *in20 = *on;
045600060510         *in21 = *on;
045700060510
045800060510       // imposto la chiave di posizionamento e lettura file
045900060510         tblkut = 1;
046000120614         tblcod = 'EI';
046100060517         tblkey = vc1cod;
046200060510
046300060510       // posizionamento sul file
046400060510       // se è stato scelto il posizionamento
046500060517         if vc1cod <> *blanks;
046600060524          rrrn01 = 0;
046700060522          setll (tblkut:tblcod:tblkey) tabel00f;
046800060510       // altrimenti
046900060510         else;
047000060522          setll (tblkut:tblcod) tabel00f;
047100060510         endif;
047200060510
047300060510       // fino a che non è fine file
047400060510         dou %eof(tabel00f);
047500060510
047600060510       // leggo file
047700060510          reade(n) (tblkut:tblcod) tabel00f;
047800060510
047900060510       // fine file esco
048000060510          if %eof(tabel00f);
048100060510           leave;
048200060510          endif;
048300110921
048400120614          dsEI = tbluni;
048500060510
048600060510       // se "ricerca/scelta" non carico i records annullati
048700060510          if (*in10 and tblflg = *blanks) or not *in10;
048800060510       // scrivo subfile
048900060517           clear vs1opz;
049000060510           exsr sr_wtrs01;
049100060510          endif;
049200060510
049300060510         enddo;
049400060510
049500060510       // fine caricamento subfile
049600060510        endif;
049700060510
049800060510       // disattivo opzione di caricamento subfile
049900060510        wrkcars01 = *off;
050000060510
050100060510       endsr;
050200060510
050300060510       // ----------------------------------------------------------------------
050400060510       // scrivo subfile
050500060510       // ----------------------------------------------------------------------
050600060510       begsr sr_wtrs01;
050700060510
050800060510       // se non raggiunto limite massimo di caricamento
050900060510        if s01nrr < 9999;
051000060510       // imposto campi di subfile
051100060510         exsr sr_sets01;
051200060510       // imposto numeratore righe e numero relativo di record
051300060510         s01nrr = s01nrr + 1;
051400060510       // scrivo subfile
051500120614         write tb10s01;
051600060510        endif;
051700060510
051800060510       endsr;
051900060510
052000060510       // ----------------------------------------------------------------------
052100060510       // imposto campi del subfile
052200060510       // ----------------------------------------------------------------------
052300060510       begsr sr_sets01;
052400060510
052500060522        vs1cod = tblkey;
052600120614        vs1des = §EIdes;
052700120614        vs1jei = §EIjei;
052800120614        IF  §EIdti > *zeros;
052900120614          wdata = %dec(§EIdti:8:0);
053000120614          data_eur = %date(wdata:*iso);
053100120614          vs1dti   = %dec(data_eur);
053200120614        ENDIF;
053300120614        IF  §EIdtf > *zeros;
053400120614          wdata = %dec(§EIdtf:8:0);
053500120614          data_eur = %date(wdata:*iso);
053600120614          vs1dtf   = %dec(data_eur);
053700120614        ENDIF;
053800120614        IF  §EIjei <> *blanks;
053900120614          clear vs1okybo;
054000120614          clear TIBS02DS;
054100120614          T02mod = 'C';
054200120614          T02cod = 'YBO';
054300120614          T02ke1 = §EIjei;
054400120614          TNTBE_RicercaControllo (kpjba : tibs02ds);
054500120614          IF  T02err = *blanks;
054600120614            vs1okybo = 'Presente in YBO';
054700120614          ENDIF;
054800120614        ENDIF;
054900060517
055000060517        clear vs1atb;
055100060522        if tblflg <> *blanks;
055200060517         vs1atb = 'A';
055300060517        endif;
055400060510
055500060510       endsr;
055600060517
055700060517       // ----------------------------------------------------------------------
055800060517       // gestione del subfile
055900060517       // ----------------------------------------------------------------------
056000060517       begsr sr_gestsfl;
056100060517
056200060517        wrkeofs01 = *off;
056300060522        clear s01nrr;
056400060523        *in39 = *off;
056500060517
056600060517       // inizio lettura subfile
056700060522        dow wrkeofs01 = *off;
056800060522         s01nrr = s01nrr + 1;
056900120614         chain s01nrr tb10s01;
057000060522
057100060522       // se non trovo esco
057200060522         if not %found;
057300060524          if vc1cod <> *blanks;
057400060524           wrkcars01 = *on;
057500060524          endif;
057600060522          leavesr;
057700060517         endif;
057800060522
057900060517       // se è stata immessa un'opzione
058000060523         if vs1opz <> *zeros;
058100060517       // controllo se opzione valida
058200060517          xx = %lookup(vs1opz:opz);
058300060517          if xx = *zeros;
058400060517           rec01 = s01nrr;
058500060522           vc1msg = msg(03);
058600060517           *in28 = *on;
058700060523           *in39 = *on;
058800120614           update tb10s01;
058900060518           leavesr;
059000060517          endif;
059100120614       // se opzione "4" possibile solo da EDP
059200120614          IF  %subst(KNMUS:1:3) <> 'EDP' and vs1opz = 4;
059300120614           rec01 = s01nrr;
059400120614           vc1msg = msg(03);
059500120614           vc1msg = %trim(VC1msg) + ' utilizzabile solo da EDP';
059600120614           *in28 = *on;
059700120614           *in39 = *on;
059800120614           update tb10s01;
059900120614           leavesr;
060000120614          endif;
060100060522
060200060517       // reset ds di servizio
060300120614          clear trtb10ds;
060400120614          tb10opzio = vs1opz;
060500060517
060600060523       // gestione del formato video
060700060522          rec01 = s01nrr;
060800060522          $video = 'D02';
060900060523          exsr sr_d02;
061000060524       // se F12 di ritorno esco dalla lettura del subfile
061100120614          if tb10ricez = 'L';
061200060524          wrkeofs01 = *on;
061300060523          endif;
061400060518         endif;
061500060517
061600060523         clear vs1opz;
061700060522         rec01 = s01nrr;
061800120614         update tb10s01;
061900060517        enddo;
062000060517
062100060517       endsr;
062200060517
062300060517       // ----------------------------------------------------------------------
062400060517       // ripeto opzione in tutte le righe del subfile
062500060517       // ----------------------------------------------------------------------
062600060517       begsr sr_ripetiopz;
062700060517
062800060523        if rrrn01 > 0;
062900060523         s01nrr = rrrn01;
063000120614         chain s01nrr tb10s01;
063100060523         if %found and vs1opz <> *zeros;
063200060522          savopzione = vs1opz;
063300120614          update tb10s01;
063400060517
063500060517          wrkeofs01 = *off;
063600060522          dow wrkeofs01 = *off;
063700060517           s01nrr = s01nrr + 1;
063800120614           chain s01nrr tb10s01;
063900060517           if %found;
064000060522            vs1opz = savopzione;
064100120614            update tb10s01;
064200060517           else;
064300060517            wrkeofs01 = *on;
064400060517           endif;
064500060517          enddo;
064600060517
064700060517         endif;
064800060517
064900060517        endif;
065000060517
065100060517       endsr;
065200060517
065300060517       // ----------------------------------------------------------------------
065400060517       // imposto i dati a video
065500060517       // ----------------------------------------------------------------------
065600060517       begsr sr_setvideo;
065700060517
065800060524        if wrkcard02 = *on;
065900060518       // pulisco il formato video D02
066000060524         exsr sr_puld02;
066100060517
066200060522       // controllo se 'immissione' 'modifica' 'copia' o altro
066300060524         select;
066400060517
066500060517       // F10=Immissione
066600120614          when tb10comand = 'J';
066700060524           vt1dopz = 'Immissione';
066800060517
066900060523       // Opzione "02"=modifica
067000120614          when tb10opzio = 02;
067100060524           vt1dopz = 'Modifica';
067200060524           exsr sr_imposta;
067300060517
067400060523       // Opzione "03"=copia
067500120614          when tb10opzio = 03;
067600060524           vt1dopz = 'Copia';
067700060524           exsr sr_imposta;
067800060517
067900060523       // Opzione "04"=cancellazione/ripristino
068000120614          when tb10opzio = 04;
068100060524           exsr sr_imposta;
068200060517       // se richiesta 'cancellazione'
068300060524           if tblflg = *blanks;
068400060524            vt1dopz = 'Annullamento';
068500060524           endif;
068600060517       // se richiesto 'ripristino'
068700060524           if tblflg <> *blanks;
068800060524            vt1dopz = 'Ripristino';
068900060524           endif;
069000060517
069100060523       // Opzione "05"=visualizzazione
069200120614          when tb10opzio =05;
069300060524           vt1dopz = 'Visualizzazione';
069400060524           exsr sr_imposta;
069500060517
069600060517       // Fine scelta
069700060524         endsl;
069800060517
069900060517       // centro la descrizione della funzione nel formato video
070000060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
070100060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
070200060524
070300060524        endif;
070400060517
070500060517       endsr;
070600060524
070700060524       // ----------------------------------------------------------------------
070800060524       // pulizia video
070900060524       // ----------------------------------------------------------------------
071000060524       begsr sr_puld02;
071100060524
071200060524        clear vt1dopz;
071300060524        clear v2ccod;
071400060524        clear v2cdes;
071500120614        clear v2cjei;
071600120614        clear v2okybo;
071700120614        clear v2cdti;
071800120614        clear v2cdtf;
071900060524
072000060524        *in01 = *off;
072100060524        *in02 = *off;
072200060524        *in03 = *off;
072300060524        *in04 = *off;
072400060524        *in05 = *off;
072500060524        *in28 = *off;
072600060524        *in40 = *off;
072700060524        *in41 = *off;
072800060524        *in42 = *off;
072900060524        *in43 = *off;
073000060524        *in44 = *off;
073100060524
073200060524       endsr;
073300060524
073400060524       // ----------------------------------------------------------------------
073500060524       // imposto i dati a video
073600060524       // ----------------------------------------------------------------------
073700060524       begsr sr_imposta;
073800060524
073900060524       // recupero i dati dal file
074000060524        tblkut = 1;
074100120614        tblcod = 'EI';
074200060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
074300060524
074400060524       // imposto i campi a video
074500060524        v2ccod = tblkey;
074600120614        dsEI = tbluni;
074700120614        v2cdes = §EIdes;
074800120614        v2cjei = §EIjei;
074900120614        IF  §EIdti > *zeros;
075000120614          wdata = %dec(§EIdti:8:0);
075100120614          data_eur = %date(wdata:*iso);
075200120614          v2cdti   = %dec(data_eur);
075300120614        ENDIF;
075400120614        IF  §EIdtf > *zeros;
075500120614          wdata = %dec(§EIdtf:8:0);
075600120614          data_eur = %date(wdata:*iso);
075700120614          v2cdtf   = %dec(data_eur);
075800120614        ENDIF;
075900120614        IF  §EIjei <> *blanks;
076000120614          clear TIBS02DS;
076100120614          T02mod = 'C';
076200120614          T02cod = 'YBO';
076300120614          T02ke1 = §EIjei;
076400120614          TNTBE_RicercaControllo (kpjba : tibs02ds);
076500120614          IF  T02err = *blanks;
076600120614            v2okybo = 'Presente in tabella YBO';
076700120614          ENDIF;
076800120614        ENDIF;
076900060524
077000060524        if tblflg <> *blanks;
077100060524         *in04 = *on;
077200060524        endif;
077300060524
077400060524       endsr;
077500060517
077600060517       // ----------------------------------------------------------------------
077700060517       // controllo video
077800060517       // ----------------------------------------------------------------------
077900060522       begsr sr_ctrd02;
078000060517
078100060517        *in28 = *off;
078200060517        *in40 = *off;
078300060517        *in41 = *off;
078400060523        *in42 = *off;
078500060524        *in43 = *off;
078600060524        *in44 = *off;
078700060517
078800060522       // codice tabella
078900060522        if v2ccod = *blanks;
079000060522         v2cmsg = msg(01);
079100060517         *in28 = *on;
079200060517         *in40 = *on;
079300060517         leavesr;
079400060517        endif;
079500060517
079600060517       // se immissione controllo che non esista già
079700120614        if tb10comand = 'J' or tb10opzio = 03;
079800060522         tblkut = 1;
079900120614         tblcod = 'EI';
080000060522         tblkey = v2ccod;
080100060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
080200060522         if %found(tabel00f);
080300060522          v2cmsg = msg(05);
080400060517          *in28 = *on;
080500060517          *in40 = *on;
080600060517          leavesr;
080700060517         endif;
080800060517        endif;
080900060523
081000120614       // Codice ProJ
081100120614        IF  v2cjei = *blanks;
081200120614          v2cmsg = msg(06);
081300120614          *in28 = *on;
081400120614          *in42 = *on;
081500120614          leavesr;
081600120614        ENDIF;
081700120614        RIVsocieta = xscsoc;
081800120614        RIVtpreg   = vendite;
081900120614        RIVrifiva = v2cjei;
082000120614        clear RIValiq;
082100120614        chain (RIVsocieta:Rivtpreg:Rivaliq:RIVrifiva)ANRIV01L;
082200120614        IF  not %found(ANRIV01L);
082300120614          v2cmsg = msg(06);
082400120614          *in28 = *on;
082500120614          *in42 = *on;
082600120614          leavesr;
082700120614        ENDIF;
082800120614        IF  V2Cdes = *blanks;
082900120614          V2Cdes = RIVdesbrev;
083000120614        ENDIF;
083100120614
083200120614       //?Se presente codice esenzione ProJ controllo se esiste in
083300120614       //?tabella YBO
083400120614        clear v2okybo;
083500120614        IF  §EIjei <> *blanks;
083600120614          clear TIBS02DS;
083700120614          T02mod = 'C';
083800120614          T02cod = 'YBO';
083900120614          T02ke1 = V2Cjei;
084000120614          TNTBE_RicercaControllo (kpjba : tibs02ds);
084100120614          IF  T02err = *blanks;
084200120614            V2okybo = 'Presente in tabella YBO';
084300120614          ENDIF;
084400120614        ENDIF;
084500120614
084600120614       // descrizione
084700120614        if v2cdes = *blanks;
084800120614         v2cmsg = msg(02);
084900120614         *in28 = *on;
085000120614         *in41 = *on;
085100120614         leavesr;
085200120614        endif;
085300120614
085400120614         //?Data inizio e fine validità
085500120614        IF  V2Cdti  = *zeros;
085600120614          v2cmsg = msg(04);
085700120614          *in28 = *on;
085800120614          *in43 = *on;
085900120614          leavesr;
086000120614        ENDIF;
086100120614        IF  V2Cdtf  = *zeros;
086200120614          v2cmsg = msg(04);
086300120614          *in28 = *on;
086400120614          *in44 = *on;
086500120614          leavesr;
086600120614        ENDIF;
086700120614        IF  V2Cdti  > *zeros;
086800120614          clear wlbdat;
086900120614          G02dat = V2Cdti;
087000120614          xsrda8(wlbdat);
087100120614          IF  G02err = '1';
087200120614            v2cmsg = msg(04);
087300120614            *in28 = *on;
087400120614            *in43 = *on;
087500120614            leavesr;
087600120614          ENDIF;
087700120614          V2Cdti = G02dat;
087800120614          wdatai = G02inv;
087900120614        ENDIF;
088000120614        IF  V2Cdtf  > *zeros;
088100120614          clear wlbdat;
088200120614          G02dat = V2Cdtf;
088300120614          xsrda8(wlbdat);
088400120614          IF  G02err = '1';
088500120614            v2cmsg = msg(04);
088600120614            *in28 = *on;
088700120614            *in44 = *on;
088800120614            leavesr;
088900120614          ENDIF;
089000120614          V2Cdtf = G02dat;
089100120614          wdataf = G02inv;
089200120614        ENDIF;
089300120614        IF  wdatai > wdataf;
089400120614          v2cmsg = msg(07);
089500120614          *in28 = *on;
089600120614          *in43 = *on;
089700120614          leavesr;
089800120614        ENDIF;
089900060517
090000060517       endsr;
090100060524
090200060524       // ----------------------------------------------------------------------
090300060524       // gestione video dati di trasmissione
090400060524       // ----------------------------------------------------------------------
090500060524       begsr sr_w01;
090600060524
090700060524       // imposto i dati a video
090800060524        exsr sr_carw01;
090900060524
091000060524       // fino a che la variabile resta settata come 'W01'
091100060524        dow $video = 'W01';
091200060524
091300060524       // reset indicatore generico di errore
091400060524         *in28 = *off;
091500060524
091600060524       // emetto il video
091700120614          exfmt tb10w01;
091800060524
091900060524       // controllo tasti funzionali del video
092000060524         select;
092100060524
092200060524       // F6=Conferma
092300060524          when *inkf;
092400060524       // controllo i dati del video
092500060524           exsr sr_ctrw01;
092600060524       // se non riscontrati errori aggiorno il record corrente
092700060524           if not *in28;
092800060524            exsr sr_aggiorna;
092900060524            if not *in28;
093000120614             if tb10comand = 'J';
093100060524              $video = 'D02';
093200060524              exsr sr_setvideo;
093300060524             else;
093400060524              wrkcars01 = *on;
093500060524              $video = 'C01';
093600060524             endif;
093700060524            endif;
093800060524           endif;
093900060524
094000060524       // F12=Ritorno
094100060524          when *inkl;
094200060524           $video = 'D02';
094300120614           clear tb10ricez;
094400120614           tb10ricez = 'L';
094500061212           wrkcard02 = *off;
094600060524
094700060524       // Invio
094800060524          other;
094900060524           exsr sr_ctrw01;
095000060524
095100060524         endsl;
095200060524
095300060524       // fine gestione 'W01'
095400060524        enddo;
095500060524
095600060524       endsr;
095700060517
095800060517       // ----------------------------------------------------------------------
095900060517       // imposto i dati di trasmissione
096000060517       // ----------------------------------------------------------------------
096100060517       begsr sr_carw01;
096200060517
096300060517       // se pulisco i campi
096400060522         clear w01ftt;
096500060522         clear w01flt;
096600060522         clear w01ftr;
096700060522         clear w01dtr;
096800060517
096900060517       // se non immissione imposto i campi del file
097000120614        if tb10comand <> 'J';
097100060522         w01ftt = tblftt;
097200060522         w01flt = tblflt;
097300060522         w01ftr = tblftr;
097400060517       // imposto la data
097500060522         if tbldtr <> *zeros;
097600060523          datar = tbldtr;
097700060523          aad = aar;
097800060523          mmd = mmr;
097900060523          ggd = ggr;
098000060523          w01dtr = datad;
098100060517         endif;
098200060517
098300060517        endif;
098400060517
098500060517       endsr;
098600060517
098700060517       // ----------------------------------------------------------------------
098800060517       // controllo i dati di trasmissione
098900060517       // ----------------------------------------------------------------------
099000060517       begsr sr_ctrw01;
099100060517
099200060517       endsr;
099300060517
099400060517       // ----------------------------------------------------------------------
099500060517       // aggiorno tabella
099600060517       // ----------------------------------------------------------------------
099700060517       begsr sr_aggiorna;
099800060517
099900060517       // imposto campi del file
100000060522        tblkut = 1;
100100120614        tblcod = 'EI';
100200060522        tblkey = v2ccod;
100300060523
100400060523       // aggancio il rcd della tabella
100500060523        chain (tblkut:tblcod:tblkey) tabel00f;
100600060517
100700120614        §EIdes = v2cdes;
100800120614        §EIjei = v2cjei;
100900120614        §EIdti = %editc(wdatai:'X');
101000120614        §EIdtf = %editc(wdataf:'X');
101100060517
101200120614        tbluni = dsEI;
101300060517
101400060522        tblftt = w01ftt;
101500060522        tblflt = w01flt;
101600060522        clear tblftr;
101700060522        clear tbldtr;
101800060524
101900060524       // annullo/ripristino
102000120614        if tb10opzio = 04;
102100060524         select;
102200060524       // annullamento
102300060524          when tblflg = *blanks;
102400060524           tblflg = '*';
102500060524       // ripristino
102600060524          when tblflg <> *blanks;
102700060524           clear tblflg;
102800060524         endsl;
102900060524        endif;
103000060517
103100060523       // controllo se devo scrivere o aggiornare il record
103200060523        if %found(tabel00f);
103300060523         update tabel;
103400060523        else;
103500060523         write tabel;
103600060523        endif;
103700060517
103800060517       endsr;
103900060524
104000060524       // ----------------------------------------------------------------------
104100060524       // uscita dal programma
104200060524       // ----------------------------------------------------------------------
104300060524         begsr sr_uscita;
104400120614
104500120614          IF  msgerr <> *blanks;
104600120614
104700120614          ELSE;
104800060524
104900120614          if tb10esito = *blanks;
105000120614           tb10esito = eseguito;
105100060524          endif;
105200060524
105300120614          kpjbu = trtb10ds;
105400120614
105500120614          ENDIF;
105600060524
105700060524          *inlr = *on;
105800060524          return;
105900060524
106000060524         endsr;
106100060510
106200060510       // ----------------------------------------------------------------------
106300060510       // routine iniziale
106400060510       // ----------------------------------------------------------------------
106500060510         begsr *inzsr;
106600060510
106700060510      /end-free
106800060510
106900060510     c     *entry        plist
107000060510     c                   parm                    kpjba
107100060510
107200060510      /free
107300060510         in(e) §azute;
107400060510         if not %error;
107500060510          in(e) §datiute;
107600060510         endif;
107700060510         if %error or rsut = *blanks;
107800060510          tibs34r(tibs34ds);
107900060510          in §azute;
108000060510          in §datiute;
108100060510         endif;
108200060510
108300060510       // centro il titolo
108400060510        vt1tit = titolo;
108500060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
108600060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
108700060510
108800060510         endsr;
108900060510
109000060510      /end-free
109100060418
109200060418** -- MSG -------------------------------------------------------------------*
109300060418Immettere il codice                                                            1
109400060418Immettere la descrizione                                                       2
109500060522Opzione errata                                                                 3
109600060523Data errata                                                                    4
109700060522Codice errato                                                                  5
109800120614Immettere un riferimento IVA esistente nell'anagrafico di PROJ                 6
109900120614Data "Inizio" incongruente con data "Fine"                                     7
