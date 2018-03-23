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
005800120702
005900120702     d tipxsc          s              6    inz('SOC001')
006000120702     d socxsc          s              3    inz
006100120702     d cdsxsc          s              9  0 inz
006200120702     d modxsc          s              3    inz
006300120702     d rtnxsc          s              1    inz
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
007400120614
007500120614     d                 ds
007600120614     d aar                     1      2  0
007700120614     d mmr                     3      4  0
007800120614     d ggr                     5      6  0
007900120614     d datar                   1      6  0
008000120614
008100120614     d                 ds
008200120614     d ggd                     1      2  0
008300120614     d mmd                     3      4  0
008400120614     d aad                     5      6  0
008500120614     d datad                   1      6  0
008600060418
008700120614     d dsei          e ds
008800120614
008900060418     d kpjba         e ds
009000120614     d  MsgErr               453    502
009100120614
009200120614     d Tibs02ds      e ds
009300060523     d tibs34ds      e ds                  inz
009400120614     d trtb10ds      e ds
009500120614     d trul12ds      e ds
009600120614     d XSoc001DS     e ds                  inz
009700120702     d XSocDS          ds          1000
009800120614
009900060510     d §azute        e ds                  extname(azute00f)
010000060510     d                                     dtaara
010100060510     d §datiute      e ds                  extname(ddatiute)
010200060510     d                                     dtaara
010300120614
010400120614     d wlbdat          ds                  inz
010500120614     d  g02dat                 1      8  0
010600120614     d  g02inv                 9     16  0
010700120614     d  g02err                17     17
010800120614     d  g02tgi                18     22  0
010900060510
011000060510     d psds           sds
011100060510     d  pgmname          *proc
011200060510
011300060510      *------------------------------------------------------------------------*
011400060510      * pgm richiamati
011500060510      *------------------------------------------------------------------------*
011600120614      /copy gaitrasrc/srcprotopr,tibs02r
011700120614
011800060510     d tibs34r         pr                  extpgm('TIBS34R')
011900060510     d  tibs34ds                           likeds(tibs34ds)
012000120614
012100120614     d trul12r         pr                  extpgm('TRUL12R')
012200120614     d  trul12ds                           likeds(trul12ds)
012300060523
012400060523     d trul19r         pr                  extpgm('TRUL19R')
012500060523     d  t19cod1                            like(t19cod)
012600060523     d  t19ord1                            like(t19ord)
012700060523     d  t19key1                            like(t19key)
012800060523     d  t19com1                            like(t19com)
012900120614
013000120614      /copy gaitrasrc/srcprotopr,xsrda8
013100060510
013200060510      *--------------------------------------------------------------*
013300060510      * costanti
013400060510      *--------------------------------------------------------------*
013500060510     d errore          c                   '1'
013600060510     d eseguito        c                   '0'
013700120614     d titolo          c                   const('Tabella Esenzioni IVA')
013800060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
013900060510     d                                     o/Ripristino  5=Visualizza')
014000060510     d opzs            c                   const('1=Scelta')
014100120614     d Vendite         c                   '2'
014200060418      *--------------------------------------------------------------*
014300060510
014400120614     ** Mi collego ad una società.
014500120702     C                   CALL      'XSOC'
014600120702     C                   PARM                    TipXsc
014700120702     C                   PARM                    SocXsc
014800120702     C                   PARM                    CdsXsc
014900120702     C                   PARM                    ModXsc
015000120702     C                   PARM                    RtnXsc
015100120702     C                   PARM                    XSocDS
015200120614     C                   PARM                    Kpjba
015300120614     C                   IF        RtnXsc = '1'
015400120614     C                   EVAL      MsgErr =
015500120614     C                             'Non è possibile collegarsi alla società '
015600120614     C                             + XscSoc + '.'
015700120614     C                   EVAL      *INLR = *ON
015800120614     C                   RETURN
015900120702     C                   ELSE
016000120702     C                   MOVEL     XSOCDS        xsoc001ds
016100120614     C                   ENDIF
016200120614
016300060510      /free
016400120614       // controllo se tabella modificabile
016500120614       // solo se non sono in ambiente di prova
016600120614        clear trul12ds;
016700120614        IF  %subst(knsif:7:1) <> 'P';
016800120614          I12tla = 'L';
016900120614          I12nsi = knsif;
017000120614          I12cod = 'EI';
017100120614          trul12r (trul12ds);
017200120614        ENDIF;
017300120614        IF O12err = *blanks;
017400060510
017500120614       exsr sr_parm;
017600060517       exsr sr_video;
017700120614
017800120614        ENDIF;
017900120614
018000060510       exsr sr_uscita;
018100060510
018200060510       // ----------------------------------------------------------------------
018300060510       // elaborazione parametri ricevuti
018400060510       // ----------------------------------------------------------------------
018500060510       begsr sr_parm;
018600060510
018700120614        trtb10ds = kpjbu;
018800060510
018900060510        select;
019000060510       // ricerca e scleta
019100120614         when tb10fun = '1';
019200060510          *in10 = *on;
019300060510       // imposto le opzioni
019400060522          vc1opz = opzs;
019500060522          clear opz;
019600060523          opz(1) = 01;
019700060510       // gestione
019800120614         when  tb10fun = *blanks;
019900060510          *in10 = *off;
020000110921          *in11 = *off;
020100060517       // imposto le opzioni
020200060522          vc1opz = opzg;
020300060522          clear opz;
020400060523          opz(1) = 02;
020500060523          opz(2) = 03;
020600060523          opz(3) = 04;
020700060523          opz(4) = 05;
020800060510       // altrimenti
020900060510         other;
021000060517       // esco
021100120614          tb10esito = errore;
021200060510          exsr sr_uscita;
021300060510         endsl;
021400060510
021500060510       endsr;
021600060517
021700060517       // ----------------------------------------------------------------------
021800060517       // gestione delle videate
021900060517       // ----------------------------------------------------------------------
022000060517       begsr sr_video;
022100060517
022200060522        dow $video <> *blanks;
022300060522
022400060522         select;
022500120614       // subfile 01 - tutti i codici presenti
022600060522          when $video = 'C01';
022700060522           exsr sr_c01;
022800060517
022900120614       // video 01 - nessun codice presente
023000060522          when $video = 'D01';
023100060522           exsr sr_d01;
023200060517
023300120614       // video 02 - gestione/visualizzazione codice scelto
023400060522          when $video = 'D02';
023500060522           exsr sr_d02;
023600060522
023700060522         endsl;
023800060522
023900060522        enddo;
024000060517
024100060517       endsr;
024200060510
024300060510       // ----------------------------------------------------------------------
024400060510       // gestione subfile
024500060510       // ----------------------------------------------------------------------
024600060510       begsr sr_c01;
024700060510
024800060510       // caricamento subfile
024900060517         exsr sr_cars01;
025000060511
025100060511       // se scritto almeno un record
025200060517         if s01nrr > 0;
025300060511       // indicatore di sflend
025400060523          *in22 = *on;
025500060517         endif;
025600060511
025700060511       // se non scritto neanche un record
025800060517         if s01nrr = 0;
025900060511       // emetto videata di dati non trovati
026000060517          $video = 'D01';
026100060524          leavesr;
026200060517         endif;
026300060510
026400060523         if rrrn01 > 0;
026500060523          rec01 = rrrn01;
026600060523         else;
026700060523          rec01 = 1;
026800060517         endif;
026900060523
027000060523       // se non so quale pagina visualizzare forzo pagina 1
027100060523          if rec01 < 1;
027200060523           rec01 = 1;
027300060523          endif;
027400060510
027500060510       // Emissione del subfile
027600120614          write tb10t01;
027700120614          write tb10z01;
027800120614          exfmt tb10c01;
027900060522
028000060522       // reset indicatore generico di errore
028100060522         *in28 = *off;
028200060522       // pulisco il campo messaggi
028300060522         clear vc1msg;
028400060510
028500060510       // controllo tasti funzionali del subfile
028600060510          select;
028700060510
028800060510       // F3=Fine
028900060510           when *inkc;
029000060510            $video = *blanks;
029100120614            tb10ricez = 'C';
029200060510            exsr sr_uscita;
029300060510
029400060510       // F5=Refresh
029500060510           when *inke;
029600060510            wrkcars01 = *on;
029700060510
029800060510       // F10=Immissione
029900060510           when *inkj;
030000060511            $video = 'D02';
030100120614            clear trtb10ds;
030200120614            tb10comand = 'J';
030300060510
030400060510       // F13=Ripetizione
030500060510           when *inkm;
030600060510            exsr sr_ripetiopz;
030700060510
030800060510       // in tutti gli altri casi
030900060510           other;
031000060510       // controllo ed elaborazione scelte su subfile
031100060510            exsr sr_gestsfl;
031200060510          endsl;
031300060510
031400060510        endsr;
031500060522
031600060522       // ----------------------------------------------------------------------
031700060522       // gestione videata
031800060522       // ----------------------------------------------------------------------
031900060522       begsr sr_d01;
032000060522
032100060522       // emetto il video
032200120614          write tb10t01;
032300120614          exfmt tb10d01;
032400060522
032500060522       // controllo tasti funzionali del video
032600060522         select;
032700060522
032800060522       // F3=Fine
032900060522          when *inkc;
033000120614           tb10ricez = 'C';
033100060522           $video = *blanks;
033200060522           exsr sr_uscita;
033300060522           leavesr;
033400060522
033500060522       // F10=Immissione
033600060522          when *inkj;
033700060522           $video = 'D02';
033800120614           clear trtb10ds;
033900120614           tb10comand = 'J';
034000060522           leavesr;
034100060522
034200060522         endsl;
034300060522
034400060522       endsr;
034500060522
034600060522       // ----------------------------------------------------------------------
034700060522       // gestione videata
034800060522       // ----------------------------------------------------------------------
034900060522       begsr sr_d02;
035000060522
035100060522       // imposto il video a seconda della funzione richiesta
035200060522         exsr sr_setvideo;
035300060522
035400060605       // fino a che la variabile resta settata come 'D02'
035500060605        dow $video = 'D02';
035600060605
035700060522       // se immessa opzione di 'scelta'
035800120614         if tb10opzio = 01;
035900060522          $video = *blanks;
036000120614          tb10cod = vs1cod;
036100120614          tb10des = vs1des;
036200060522          exsr sr_uscita;
036300060522          leavesr;
036400060522         endif;
036500060522
036600060522       // se non è immissione/copia proteggo il codice tabella
036700120614          if tb10comand <> 'J' and tb10opzio <> 03;
036800060522           *in02 = *on;
036900060522          endif;
037000060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
037100060522       // proteggo i campi del video
037200120614          if tb10opzio = 04 or tb10opzio = 05;
037300060522           *in01 = *on;
037400060522          endif;
037500060522       // se immessa opzione di 'visualizzazione'
037600060522       // non attivo F6
037700120614          if tb10opzio = 05;
037800060522           *in03 = *on;
037900060522          endif;
038000060522
038100060522       // emetto il video
038200120614          write tb10t01;
038300120614          exfmt tb10d02;
038400060522
038500060522       // reset indicatore generico di errore
038600060522         *in28 = *off;
038700060522       // pulisco il campo messaggi
038800060522         clear v2cmsg;
038900060522
039000060522       // controllo tasti funzionali del video
039100060522         select;
039200060522
039300060522       // F3=Fine
039400060522          when *inkc;
039500120614           tb10ricez = 'C';
039600060522           $video = *blanks;
039700060522           exsr sr_uscita;
039800060522           leavesr;
039900060522
040000060522       // F6=Conferma
040100060522          when *inkf;
040200120614           tb10ricez = 'F';
040300060522       // controllo e decodifico i dati del video
040400060522           exsr sr_ctrd02;
040500060522       // se non riscontrati errori emetto la finestra con i dati per la
040600060522       // tramissione
040700060522           if not *in28;
040800060522            wrkcard02 = *on;
040900060523            wrkcars01 = *off;
041000060522            wrkcarw01 = *on;
041100060522            $video = 'W01';
041200060524            exsr sr_w01;
041300060523            leavesr;
041400060522           endif;
041500060523
041600060523       // F8=Record successivo
041700060524          when *inkh;
041800060524            wrkcars01 = *off;
041900060524            wrkcard02 = *on;
042000060523           $video = 'C01';
042100120614           tb10ricez = 'H';
042200060523           leavesr;
042300060522
042400060522       // F12=Ritorno
042500060522          when *inkl;
042600060522           wrkcard02 = *on;
042700060524           wrkcars01 = *off;
042800120614           tb10ricez = 'L';
042900120614           if tb10comand = 'J' or tb10opzio = 03;
043000060523            wrkcars01 = *on;
043100060522           endif;
043200060522           $video = 'C01';
043300060522           leavesr;
043400060522
043500060522       // Invio
043600060522          other;
043700060522           if not *in01;
043800060522            exsr sr_ctrd02;
043900060522           endif;
044000060522
044100060522         endsl;
044200060605
044300060605       // fine gestione 'D02'
044400060605        enddo;
044500060522
044600060522        wrkcard02 = *off;
044700060522
044800060522       endsr;
044900060510
045000060510       // ----------------------------------------------------------------------
045100060510       // caricamento subfile
045200060510       // ----------------------------------------------------------------------
045300060510       begsr sr_cars01;
045400060510
045500060510       // se è stato richiesto il caricamento del subfile
045600060510        if wrkcars01 = *on;
045700060510
045800060510       // inizializzo il subfile
045900060510         s01nrr = 0;
046000060510         *in20 = *off;
046100060510         *in21 = *off;
046200060523         *in22 = *on;
046300120614         write tb10c01;
046400060510         *in20 = *on;
046500060510         *in21 = *on;
046600060510
046700060510       // imposto la chiave di posizionamento e lettura file
046800060510         tblkut = 1;
046900120614         tblcod = 'EI';
047000060517         tblkey = vc1cod;
047100060510
047200060510       // posizionamento sul file
047300060510       // se è stato scelto il posizionamento
047400060517         if vc1cod <> *blanks;
047500060524          rrrn01 = 0;
047600060522          setll (tblkut:tblcod:tblkey) tabel00f;
047700060510       // altrimenti
047800060510         else;
047900060522          setll (tblkut:tblcod) tabel00f;
048000060510         endif;
048100060510
048200060510       // fino a che non è fine file
048300060510         dou %eof(tabel00f);
048400060510
048500060510       // leggo file
048600060510          reade(n) (tblkut:tblcod) tabel00f;
048700060510
048800060510       // fine file esco
048900060510          if %eof(tabel00f);
049000060510           leave;
049100060510          endif;
049200110921
049300120614          dsEI = tbluni;
049400060510
049500060510       // se "ricerca/scelta" non carico i records annullati
049600060510          if (*in10 and tblflg = *blanks) or not *in10;
049700060510       // scrivo subfile
049800060517           clear vs1opz;
049900060510           exsr sr_wtrs01;
050000060510          endif;
050100060510
050200060510         enddo;
050300060510
050400060510       // fine caricamento subfile
050500060510        endif;
050600060510
050700060510       // disattivo opzione di caricamento subfile
050800060510        wrkcars01 = *off;
050900060510
051000060510       endsr;
051100060510
051200060510       // ----------------------------------------------------------------------
051300060510       // scrivo subfile
051400060510       // ----------------------------------------------------------------------
051500060510       begsr sr_wtrs01;
051600060510
051700060510       // se non raggiunto limite massimo di caricamento
051800060510        if s01nrr < 9999;
051900060510       // imposto campi di subfile
052000060510         exsr sr_sets01;
052100060510       // imposto numeratore righe e numero relativo di record
052200060510         s01nrr = s01nrr + 1;
052300060510       // scrivo subfile
052400120614         write tb10s01;
052500060510        endif;
052600060510
052700060510       endsr;
052800060510
052900060510       // ----------------------------------------------------------------------
053000060510       // imposto campi del subfile
053100060510       // ----------------------------------------------------------------------
053200060510       begsr sr_sets01;
053300060510
053400060522        vs1cod = tblkey;
053500120614        vs1des = §EIdes;
053600120614        vs1jei = §EIjei;
053700120614        IF  §EIdti > *zeros;
053800120614          wdata = %dec(§EIdti:8:0);
053900120614          data_eur = %date(wdata:*iso);
054000120614          vs1dti   = %dec(data_eur);
054100120614        ENDIF;
054200120614        IF  §EIdtf > *zeros;
054300120614          wdata = %dec(§EIdtf:8:0);
054400120614          data_eur = %date(wdata:*iso);
054500120614          vs1dtf   = %dec(data_eur);
054600120614        ENDIF;
054700120614        IF  §EIjei <> *blanks;
054800120614          clear vs1okybo;
054900120614          clear TIBS02DS;
055000120614          T02mod = 'C';
055100120614          T02cod = 'YBO';
055200120614          T02ke1 = §EIjei;
055300120614          TNTBE_RicercaControllo (kpjba : tibs02ds);
055400120614          IF  T02err = *blanks;
055500120614            vs1okybo = 'Presente in YBO';
055600120614          ENDIF;
055700120614        ENDIF;
055800060517
055900060517        clear vs1atb;
056000060522        if tblflg <> *blanks;
056100060517         vs1atb = 'A';
056200060517        endif;
056300060510
056400060510       endsr;
056500060517
056600060517       // ----------------------------------------------------------------------
056700060517       // gestione del subfile
056800060517       // ----------------------------------------------------------------------
056900060517       begsr sr_gestsfl;
057000060517
057100060517        wrkeofs01 = *off;
057200060522        clear s01nrr;
057300060523        *in39 = *off;
057400060517
057500060517       // inizio lettura subfile
057600060522        dow wrkeofs01 = *off;
057700060522         s01nrr = s01nrr + 1;
057800120614         chain s01nrr tb10s01;
057900060522
058000060522       // se non trovo esco
058100060522         if not %found;
058200060524          if vc1cod <> *blanks;
058300060524           wrkcars01 = *on;
058400060524          endif;
058500060522          leavesr;
058600060517         endif;
058700060522
058800060517       // se è stata immessa un'opzione
058900060523         if vs1opz <> *zeros;
059000060517       // controllo se opzione valida
059100060517          xx = %lookup(vs1opz:opz);
059200060517          if xx = *zeros;
059300060517           rec01 = s01nrr;
059400060522           vc1msg = msg(03);
059500060517           *in28 = *on;
059600060523           *in39 = *on;
059700120614           update tb10s01;
059800060518           leavesr;
059900060517          endif;
060000120614       // se opzione "4" possibile solo da EDP
060100120614          IF  %subst(KNMUS:1:3) <> 'EDP' and vs1opz = 4;
060200120614           rec01 = s01nrr;
060300120614           vc1msg = msg(03);
060400120614           vc1msg = %trim(VC1msg) + ' utilizzabile solo da EDP';
060500120614           *in28 = *on;
060600120614           *in39 = *on;
060700120614           update tb10s01;
060800120614           leavesr;
060900120614          endif;
061000060522
061100060517       // reset ds di servizio
061200120614          clear trtb10ds;
061300120614          tb10opzio = vs1opz;
061400060517
061500060523       // gestione del formato video
061600060522          rec01 = s01nrr;
061700060522          $video = 'D02';
061800060523          exsr sr_d02;
061900060524       // se F12 di ritorno esco dalla lettura del subfile
062000120614          if tb10ricez = 'L';
062100060524          wrkeofs01 = *on;
062200060523          endif;
062300060518         endif;
062400060517
062500060523         clear vs1opz;
062600060522         rec01 = s01nrr;
062700120614         update tb10s01;
062800060517        enddo;
062900060517
063000060517       endsr;
063100060517
063200060517       // ----------------------------------------------------------------------
063300060517       // ripeto opzione in tutte le righe del subfile
063400060517       // ----------------------------------------------------------------------
063500060517       begsr sr_ripetiopz;
063600060517
063700060523        if rrrn01 > 0;
063800060523         s01nrr = rrrn01;
063900120614         chain s01nrr tb10s01;
064000060523         if %found and vs1opz <> *zeros;
064100060522          savopzione = vs1opz;
064200120614          update tb10s01;
064300060517
064400060517          wrkeofs01 = *off;
064500060522          dow wrkeofs01 = *off;
064600060517           s01nrr = s01nrr + 1;
064700120614           chain s01nrr tb10s01;
064800060517           if %found;
064900060522            vs1opz = savopzione;
065000120614            update tb10s01;
065100060517           else;
065200060517            wrkeofs01 = *on;
065300060517           endif;
065400060517          enddo;
065500060517
065600060517         endif;
065700060517
065800060517        endif;
065900060517
066000060517       endsr;
066100060517
066200060517       // ----------------------------------------------------------------------
066300060517       // imposto i dati a video
066400060517       // ----------------------------------------------------------------------
066500060517       begsr sr_setvideo;
066600060517
066700060524        if wrkcard02 = *on;
066800060518       // pulisco il formato video D02
066900060524         exsr sr_puld02;
067000060517
067100060522       // controllo se 'immissione' 'modifica' 'copia' o altro
067200060524         select;
067300060517
067400060517       // F10=Immissione
067500120614          when tb10comand = 'J';
067600060524           vt1dopz = 'Immissione';
067700060517
067800060523       // Opzione "02"=modifica
067900120614          when tb10opzio = 02;
068000060524           vt1dopz = 'Modifica';
068100060524           exsr sr_imposta;
068200060517
068300060523       // Opzione "03"=copia
068400120614          when tb10opzio = 03;
068500060524           vt1dopz = 'Copia';
068600060524           exsr sr_imposta;
068700060517
068800060523       // Opzione "04"=cancellazione/ripristino
068900120614          when tb10opzio = 04;
069000060524           exsr sr_imposta;
069100060517       // se richiesta 'cancellazione'
069200060524           if tblflg = *blanks;
069300060524            vt1dopz = 'Annullamento';
069400060524           endif;
069500060517       // se richiesto 'ripristino'
069600060524           if tblflg <> *blanks;
069700060524            vt1dopz = 'Ripristino';
069800060524           endif;
069900060517
070000060523       // Opzione "05"=visualizzazione
070100120614          when tb10opzio =05;
070200060524           vt1dopz = 'Visualizzazione';
070300060524           exsr sr_imposta;
070400060517
070500060517       // Fine scelta
070600060524         endsl;
070700060517
070800060517       // centro la descrizione della funzione nel formato video
070900060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
071000060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
071100060524
071200060524        endif;
071300060517
071400060517       endsr;
071500060524
071600060524       // ----------------------------------------------------------------------
071700060524       // pulizia video
071800060524       // ----------------------------------------------------------------------
071900060524       begsr sr_puld02;
072000060524
072100060524        clear vt1dopz;
072200060524        clear v2ccod;
072300060524        clear v2cdes;
072400120614        clear v2cjei;
072500120614        clear v2okybo;
072600120614        clear v2cdti;
072700120614        clear v2cdtf;
072800060524
072900060524        *in01 = *off;
073000060524        *in02 = *off;
073100060524        *in03 = *off;
073200060524        *in04 = *off;
073300060524        *in05 = *off;
073400060524        *in28 = *off;
073500060524        *in40 = *off;
073600060524        *in41 = *off;
073700060524        *in42 = *off;
073800060524        *in43 = *off;
073900060524        *in44 = *off;
074000060524
074100060524       endsr;
074200060524
074300060524       // ----------------------------------------------------------------------
074400060524       // imposto i dati a video
074500060524       // ----------------------------------------------------------------------
074600060524       begsr sr_imposta;
074700060524
074800060524       // recupero i dati dal file
074900060524        tblkut = 1;
075000120614        tblcod = 'EI';
075100060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
075200060524
075300060524       // imposto i campi a video
075400060524        v2ccod = tblkey;
075500120614        dsEI = tbluni;
075600120614        v2cdes = §EIdes;
075700120614        v2cjei = §EIjei;
075800120614        IF  §EIdti > *zeros;
075900120614          wdata = %dec(§EIdti:8:0);
076000120614          data_eur = %date(wdata:*iso);
076100120614          v2cdti   = %dec(data_eur);
076200120614        ENDIF;
076300120614        IF  §EIdtf > *zeros;
076400120614          wdata = %dec(§EIdtf:8:0);
076500120614          data_eur = %date(wdata:*iso);
076600120614          v2cdtf   = %dec(data_eur);
076700120614        ENDIF;
076800120614        IF  §EIjei <> *blanks;
076900120614          clear TIBS02DS;
077000120614          T02mod = 'C';
077100120614          T02cod = 'YBO';
077200120614          T02ke1 = §EIjei;
077300120614          TNTBE_RicercaControllo (kpjba : tibs02ds);
077400120614          IF  T02err = *blanks;
077500120614            v2okybo = 'Presente in tabella YBO';
077600120614          ENDIF;
077700120614        ENDIF;
077800060524
077900060524        if tblflg <> *blanks;
078000060524         *in04 = *on;
078100060524        endif;
078200060524
078300060524       endsr;
078400060517
078500060517       // ----------------------------------------------------------------------
078600060517       // controllo video
078700060517       // ----------------------------------------------------------------------
078800060522       begsr sr_ctrd02;
078900060517
079000060517        *in28 = *off;
079100060517        *in40 = *off;
079200060517        *in41 = *off;
079300060523        *in42 = *off;
079400060524        *in43 = *off;
079500060524        *in44 = *off;
079600060517
079700060522       // codice tabella
079800060522        if v2ccod = *blanks;
079900060522         v2cmsg = msg(01);
080000060517         *in28 = *on;
080100060517         *in40 = *on;
080200060517         leavesr;
080300060517        endif;
080400060517
080500060517       // se immissione controllo che non esista già
080600120614        if tb10comand = 'J' or tb10opzio = 03;
080700060522         tblkut = 1;
080800120614         tblcod = 'EI';
080900060522         tblkey = v2ccod;
081000060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
081100060522         if %found(tabel00f);
081200060522          v2cmsg = msg(05);
081300060517          *in28 = *on;
081400060517          *in40 = *on;
081500060517          leavesr;
081600060517         endif;
081700060517        endif;
081800060523
081900120614       // Codice ProJ
082000120614        IF  v2cjei = *blanks;
082100120614          v2cmsg = msg(06);
082200120614          *in28 = *on;
082300120614          *in42 = *on;
082400120614          leavesr;
082500120614        ENDIF;
082600120614        RIVsocieta = xscsoc;
082700120614        RIVtpreg   = vendite;
082800120614        RIVrifiva = v2cjei;
082900120614        clear RIValiq;
083000120614        chain (RIVsocieta:Rivtpreg:Rivaliq:RIVrifiva)ANRIV01L;
083100120614        IF  not %found(ANRIV01L);
083200120614          v2cmsg = msg(06);
083300120614          *in28 = *on;
083400120614          *in42 = *on;
083500120614          leavesr;
083600120614        ENDIF;
083700120614        IF  V2Cdes = *blanks;
083800120614          V2Cdes = RIVdesbrev;
083900120614        ENDIF;
084000120614
084100120614       //?Se presente codice esenzione ProJ controllo se esiste in
084200120614       //?tabella YBO
084300120614        clear v2okybo;
084400120614        IF  §EIjei <> *blanks;
084500120614          clear TIBS02DS;
084600120614          T02mod = 'C';
084700120614          T02cod = 'YBO';
084800120614          T02ke1 = V2Cjei;
084900120614          TNTBE_RicercaControllo (kpjba : tibs02ds);
085000120614          IF  T02err = *blanks;
085100120614            V2okybo = 'Presente in tabella YBO';
085200120614          ENDIF;
085300120614        ENDIF;
085400120614
085500120614       // descrizione
085600120614        if v2cdes = *blanks;
085700120614         v2cmsg = msg(02);
085800120614         *in28 = *on;
085900120614         *in41 = *on;
086000120614         leavesr;
086100120614        endif;
086200120614
086300120614         //?Data inizio e fine validità
086400120614        IF  V2Cdti  = *zeros;
086500120614          v2cmsg = msg(04);
086600120614          *in28 = *on;
086700120614          *in43 = *on;
086800120614          leavesr;
086900120614        ENDIF;
087000120614        IF  V2Cdtf  = *zeros;
087100120614          v2cmsg = msg(04);
087200120614          *in28 = *on;
087300120614          *in44 = *on;
087400120614          leavesr;
087500120614        ENDIF;
087600120614        IF  V2Cdti  > *zeros;
087700120614          clear wlbdat;
087800120614          G02dat = V2Cdti;
087900120614          xsrda8(wlbdat);
088000120614          IF  G02err = '1';
088100120614            v2cmsg = msg(04);
088200120614            *in28 = *on;
088300120614            *in43 = *on;
088400120614            leavesr;
088500120614          ENDIF;
088600120614          V2Cdti = G02dat;
088700120614          wdatai = G02inv;
088800120614        ENDIF;
088900120614        IF  V2Cdtf  > *zeros;
089000120614          clear wlbdat;
089100120614          G02dat = V2Cdtf;
089200120614          xsrda8(wlbdat);
089300120614          IF  G02err = '1';
089400120614            v2cmsg = msg(04);
089500120614            *in28 = *on;
089600120614            *in44 = *on;
089700120614            leavesr;
089800120614          ENDIF;
089900120614          V2Cdtf = G02dat;
090000120614          wdataf = G02inv;
090100120614        ENDIF;
090200120614        IF  wdatai > wdataf;
090300120614          v2cmsg = msg(07);
090400120614          *in28 = *on;
090500120614          *in43 = *on;
090600120614          leavesr;
090700120614        ENDIF;
090800060517
090900060517       endsr;
091000060524
091100060524       // ----------------------------------------------------------------------
091200060524       // gestione video dati di trasmissione
091300060524       // ----------------------------------------------------------------------
091400060524       begsr sr_w01;
091500060524
091600060524       // imposto i dati a video
091700060524        exsr sr_carw01;
091800060524
091900060524       // fino a che la variabile resta settata come 'W01'
092000060524        dow $video = 'W01';
092100060524
092200060524       // reset indicatore generico di errore
092300060524         *in28 = *off;
092400060524
092500060524       // emetto il video
092600120614          exfmt tb10w01;
092700060524
092800060524       // controllo tasti funzionali del video
092900060524         select;
093000060524
093100060524       // F6=Conferma
093200060524          when *inkf;
093300060524       // controllo i dati del video
093400060524           exsr sr_ctrw01;
093500060524       // se non riscontrati errori aggiorno il record corrente
093600060524           if not *in28;
093700060524            exsr sr_aggiorna;
093800060524            if not *in28;
093900120614             if tb10comand = 'J';
094000060524              $video = 'D02';
094100060524              exsr sr_setvideo;
094200060524             else;
094300060524              wrkcars01 = *on;
094400060524              $video = 'C01';
094500060524             endif;
094600060524            endif;
094700060524           endif;
094800060524
094900060524       // F12=Ritorno
095000060524          when *inkl;
095100060524           $video = 'D02';
095200120614           clear tb10ricez;
095300120614           tb10ricez = 'L';
095400061212           wrkcard02 = *off;
095500060524
095600060524       // Invio
095700060524          other;
095800060524           exsr sr_ctrw01;
095900060524
096000060524         endsl;
096100060524
096200060524       // fine gestione 'W01'
096300060524        enddo;
096400060524
096500060524       endsr;
096600060517
096700060517       // ----------------------------------------------------------------------
096800060517       // imposto i dati di trasmissione
096900060517       // ----------------------------------------------------------------------
097000060517       begsr sr_carw01;
097100060517
097200060517       // se pulisco i campi
097300060522         clear w01ftt;
097400060522         clear w01flt;
097500060522         clear w01ftr;
097600060522         clear w01dtr;
097700060517
097800060517       // se non immissione imposto i campi del file
097900120614        if tb10comand <> 'J';
098000060522         w01ftt = tblftt;
098100060522         w01flt = tblflt;
098200060522         w01ftr = tblftr;
098300060517       // imposto la data
098400060522         if tbldtr <> *zeros;
098500060523          datar = tbldtr;
098600060523          aad = aar;
098700060523          mmd = mmr;
098800060523          ggd = ggr;
098900060523          w01dtr = datad;
099000060517         endif;
099100060517
099200060517        endif;
099300060517
099400060517       endsr;
099500060517
099600060517       // ----------------------------------------------------------------------
099700060517       // controllo i dati di trasmissione
099800060517       // ----------------------------------------------------------------------
099900060517       begsr sr_ctrw01;
100000060517
100100060517       endsr;
100200060517
100300060517       // ----------------------------------------------------------------------
100400060517       // aggiorno tabella
100500060517       // ----------------------------------------------------------------------
100600060517       begsr sr_aggiorna;
100700060517
100800060517       // imposto campi del file
100900060522        tblkut = 1;
101000120614        tblcod = 'EI';
101100060522        tblkey = v2ccod;
101200060523
101300060523       // aggancio il rcd della tabella
101400060523        chain (tblkut:tblcod:tblkey) tabel00f;
101500060517
101600120614        §EIdes = v2cdes;
101700120614        §EIjei = v2cjei;
101800120614        §EIdti = %editc(wdatai:'X');
101900120614        §EIdtf = %editc(wdataf:'X');
102000060517
102100120614        tbluni = dsEI;
102200060517
102300060522        tblftt = w01ftt;
102400060522        tblflt = w01flt;
102500060522        clear tblftr;
102600060522        clear tbldtr;
102700060524
102800060524       // annullo/ripristino
102900120614        if tb10opzio = 04;
103000060524         select;
103100060524       // annullamento
103200060524          when tblflg = *blanks;
103300060524           tblflg = '*';
103400060524       // ripristino
103500060524          when tblflg <> *blanks;
103600060524           clear tblflg;
103700060524         endsl;
103800060524        endif;
103900060517
104000060523       // controllo se devo scrivere o aggiornare il record
104100060523        if %found(tabel00f);
104200060523         update tabel;
104300060523        else;
104400060523         write tabel;
104500060523        endif;
104600060517
104700060517       endsr;
104800060524
104900060524       // ----------------------------------------------------------------------
105000060524       // uscita dal programma
105100060524       // ----------------------------------------------------------------------
105200060524         begsr sr_uscita;
105300120614
105400120614          IF  msgerr <> *blanks;
105500120614
105600120614          ELSE;
105700060524
105800120614          if tb10esito = *blanks;
105900120614           tb10esito = eseguito;
106000060524          endif;
106100060524
106200120614          kpjbu = trtb10ds;
106300120614
106400120614          ENDIF;
106500060524
106600060524          *inlr = *on;
106700060524          return;
106800060524
106900060524         endsr;
107000060510
107100060510       // ----------------------------------------------------------------------
107200060510       // routine iniziale
107300060510       // ----------------------------------------------------------------------
107400060510         begsr *inzsr;
107500060510
107600060510      /end-free
107700060510
107800060510     c     *entry        plist
107900060510     c                   parm                    kpjba
108000060510
108100060510      /free
108200060510         in(e) §azute;
108300060510         if not %error;
108400060510          in(e) §datiute;
108500060510         endif;
108600060510         if %error or rsut = *blanks;
108700060510          tibs34r(tibs34ds);
108800060510          in §azute;
108900060510          in §datiute;
109000060510         endif;
109100060510
109200060510       // centro il titolo
109300060510        vt1tit = titolo;
109400060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
109500060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
109600060510
109700060510         endsr;
109800060510
109900060510      /end-free
110000060418
110100060418** -- MSG -------------------------------------------------------------------*
110200060418Immettere il codice                                                            1
110300060418Immettere la descrizione                                                       2
110400060522Opzione errata                                                                 3
110500060523Data errata                                                                    4
110600060522Codice errato                                                                  5
110700120614Immettere un riferimento IVA esistente nell'anagrafico di PROJ                 6
110800120614Data "Inizio" incongruente con data "Fine"                                     7
