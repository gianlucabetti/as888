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
002100140527      * 09 - abilitato F21=Tab."SP" (non inserimento)
002200060510      * 10 - ON ricerca/scelta -- OFF gestione
002300110921      * 11 - ON escludo assicurazioni x conto
002400060510      * 20 - sfldsp
002500060510      * 21 - sfldspctl -- sflclr
002600060510      * 22 - sflend
002700060524      * 28 - visualizza messaggio errore
002800060524      * 39 - errore subfile
002900060524      * 40 - codice tabella
003000060524      * 41 - descrizione
003100060524      * 42 - data eliminazione
003200060524      * 43/53 - tipo tariffa
003300060524      * 54 - codice tassazione italia
003400060524      * 55 - codice tassazione estero
003500060418      *--------------------------------------------------------------*
003600060418
003700060418      *--------------------------------------------------------------*
003800060510      * campi di work
003900060418      *--------------------------------------------------------------*
004000060522     d blanks          s                   like(vt1dopz)
004100060418     d comando         s              1
004200060523     d dataalfa        s              8
004300060523     d dtaiso          s               d   inz
004400060510     d len             s              5u 0
004500060510     d savrec          s                   like(s01nrr)
004600060517     d savopzione      s                   like(vs1opz)
004700140527     d SaveTBLflg      s                   like(TBLflg)
004800060531     d savtblftt       s                   like(tblftt)
004900060531     d savtblflt       s                   like(tblflt)
005000060510     d savtbluni       s                   like(tbluni)
005100060510     d s01nrr          s              4  0
005200060510     d s02nrr          s              4  0
005300060523     d t19cod          s                   like(tblcod)
005400060523     d t19ord          s              1
005500060523     d t19key          s                   like(tblkey)
005600060523     d t19com          s              1
005700060522     d wrkcard02       s              1    inz('1')
005800060522     d wrkcars01       s              1    inz('1')
005900060522     d wrkcarw01       s              1
006000060524     d wrkct           s                   like(v2ccts)
006100060524     d wrkeofs01       s              1
006200060522     d wrksfl02        s              1
006300060517     d $video          s              3    inz('C01')
006400060517     d xx              s              2  0
006500060524     d yy              s              2  0
006600060418
006700060418      *--------------------------------------------------------------*
006800060510      * schiere
006900060418      *--------------------------------------------------------------*
007000060418     d msg             s             78    dim(10) ctdata perrcd(1)
007100060523     d opz             s              2  0 dim(10)
007200060418
007300060418      *--------------------------------------------------------------*
007400060510      * ds interne/esterne
007500060418      *--------------------------------------------------------------*
007600060523     d v2celi          ds
007700060523     d  v2celig
007800060523     d  v2celim
007900060523     d  v2celia
008000060523
008100060523     d                 ds
008200060523     d v2ctt0                  1      1
008300060523     d v2ctt1                  2      2
008400060524     d v2ctt2                  3      3
008500060524     d v2ctt3                  4      4
008600060524     d v2ctt4                  5      5
008700060524     d v2ctt5                  6      6
008800060524     d v2ctt6                  7      7
008900060524     d v2ctt7                  8      8
009000060524     d v2ctt8                  9      9
009100060524     d v2ctt9                 10     10
009200060524     d v2cttv                 11     11
009300060524     d skctt                   1     11    dim(11)
009400060523
009500060523     d                 ds
009600060523     d aar                     1      2  0
009700060523     d mmr                     3      4  0
009800060523     d ggr                     5      6  0
009900060523     d datar                   1      6  0
010000060523
010100060523     d                 ds
010200060523     d ggd                     1      2  0
010300060523     d mmd                     3      4  0
010400060523     d aad                     5      6  0
010500060523     d datad                   1      6  0
010600060418
010700060509     d ds1p          e ds
010800060418     d kpjba         e ds
010900060523     d tibs34ds      e ds                  inz
011000120713     d trtb09ds      e ds
011100060510     d trtb24ds      e ds
011200140527     d TRTB25ds      e ds                  inz
011300060510     d §azute        e ds                  extname(azute00f)
011400060510     d                                     dtaara
011500060510     d §datiute      e ds                  extname(ddatiute)
011600060510     d                                     dtaara
011700060510
011800060510     d psds           sds
011900060510     d  pgmname          *proc
012000060510
012100060510      *------------------------------------------------------------------------*
012200060510      * pgm richiamati
012300060510      *------------------------------------------------------------------------*
012400060510     d tibs34r         pr                  extpgm('TIBS34R')
012500060510     d  tibs34ds                           likeds(tibs34ds)
012600060523
012700120713     d trtb09r         pr                  extpgm('TRTB09R')
012800120713     d  kpjba                              likeds(kpjba)
012900140527
013000140527     d trtb25r         pr                  extpgm('TRTB25R')
013100140527     d   kpjba                             likeds(kpjba)
013200140527     d   prm_CodCopy                       like(VS1cod)
013300140527     d                                      options(*nopass)
013400120713
013500060523     d trul19r         pr                  extpgm('TRUL19R')
013600060523     d  t19cod1                            like(t19cod)
013700060523     d  t19ord1                            like(t19ord)
013800060523     d  t19key1                            like(t19key)
013900060523     d  t19com1                            like(t19com)
014000060510
014100060510      *--------------------------------------------------------------*
014200060510      * costanti
014300060510      *--------------------------------------------------------------*
014400060510     d errore          c                   '1'
014500060510     d eseguito        c                   '0'
014600060510     d titolo          c                   const('Tabella Tariffe Particolari')
014700060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
014800060510     d                                     o/Ripristino  5=Visualizza')
014900060510     d opzs            c                   const('1=Scelta')
015000140527
015100140527       // - Attributi di Visualizzazione
015200140527     d c_DspAtrNorm    c                   const(x'A0')
015300140527     d c_DspAtrRI      c                   const(x'A1')
015400140527     d c_DspAtrRED     c                   const(x'A8')
015500140527
015600060418      *--------------------------------------------------------------*
015700060510
015800060510      /free
015900060510
016000060510       exsr sr_parm;
016100060517       exsr sr_video;
016200060510       exsr sr_uscita;
016300060510
016400060510       // ----------------------------------------------------------------------
016500060510       // elaborazione parametri ricevuti
016600060510       // ----------------------------------------------------------------------
016700060510       begsr sr_parm;
016800060510
016900060510        trtb24ds = kpjbu;
017000060510
017100060510        select;
017200060510       // ricerca e scleta
017300060510         when tb24fun = '1';
017400060510          *in10 = *on;
017500060510       // imposto le opzioni
017600060522          vc1opz = opzs;
017700060522          clear opz;
017800060523          opz(1) = 01;
017900110921          *in11 = (tb24ass = 'S');
018000060510       // gestione
018100060510         when  tb24fun = *blanks;
018200060510          *in10 = *off;
018300110921          *in11 = *off;
018400060517       // imposto le opzioni
018500060522          vc1opz = opzg;
018600060522          clear opz;
018700060523          opz(1) = 02;
018800060523          opz(2) = 03;
018900060523          opz(3) = 04;
019000060523          opz(4) = 05;
019100060510       // altrimenti
019200060510         other;
019300060517       // esco
019400060510          tb24esito = errore;
019500060510          exsr sr_uscita;
019600060510         endsl;
019700060510
019800060510       endsr;
019900060517
020000060517       // ----------------------------------------------------------------------
020100060517       // gestione delle videate
020200060517       // ----------------------------------------------------------------------
020300060517       begsr sr_video;
020400060517
020500060522        dow $video <> *blanks;
020600060522
020700060522         select;
020800060517       // subfile 01 - tutte le tar.particolari presenti
020900060522          when $video = 'C01';
021000060522           exsr sr_c01;
021100060517
021200060517       // video 01 - nessuna tar.particolare presente
021300060522          when $video = 'D01';
021400060522           exsr sr_d01;
021500060517
021600060517       // video 02 - gestione/visualizzazione tar.particolare scelta
021700060522          when $video = 'D02';
021800060522           exsr sr_d02;
021900060522
022000060522         endsl;
022100060522
022200060522        enddo;
022300060517
022400060517       endsr;
022500060510
022600060510       // ----------------------------------------------------------------------
022700060510       // gestione subfile
022800060510       // ----------------------------------------------------------------------
022900060510       begsr sr_c01;
023000060510
023100060510       // caricamento subfile
023200060517         exsr sr_cars01;
023300060511
023400060511       // se scritto almeno un record
023500060517         if s01nrr > 0;
023600060511       // indicatore di sflend
023700060523          *in22 = *on;
023800060517         endif;
023900060511
024000060511       // se non scritto neanche un record
024100060517         if s01nrr = 0;
024200060511       // emetto videata di dati non trovati
024300060517          $video = 'D01';
024400060524          leavesr;
024500060517         endif;
024600060510
024700060523         if rrrn01 > 0;
024800060523          rec01 = rrrn01;
024900060523         else;
025000060523          rec01 = 1;
025100060517         endif;
025200060523
025300060523       // se non so quale pagina visualizzare forzo pagina 1
025400060523          if rec01 < 1;
025500060523           rec01 = 1;
025600060523          endif;
025700060510
025800060510       // Emissione del subfile
025900060517          write tb24t01;
026000060517          write tb24z01;
026100060517          exfmt tb24c01;
026200060522
026300060522       // reset indicatore generico di errore
026400060522         *in28 = *off;
026500060522       // pulisco il campo messaggi
026600060522         clear vc1msg;
026700060510
026800060510       // controllo tasti funzionali del subfile
026900060510          select;
027000060510
027100060510       // F3=Fine
027200060510           when *inkc;
027300060510            $video = *blanks;
027400060517            tb24ricez = 'C';
027500060510            exsr sr_uscita;
027600060510
027700060510       // F5=Refresh
027800060510           when *inke;
027900060510            wrkcars01 = *on;
028000060510
028100060510       // F10=Immissione
028200060510           when *inkj;
028300060511            $video = 'D02';
028400060517            clear trtb24ds;
028500060517            tb24comand = 'J';
028600060510
028700060510       // F13=Ripetizione
028800060510           when *inkm;
028900060510            exsr sr_ripetiopz;
029000060510
029100060510       // in tutti gli altri casi
029200060510           other;
029300060510       // controllo ed elaborazione scelte su subfile
029400060510            exsr sr_gestsfl;
029500060510          endsl;
029600060510
029700060510        endsr;
029800060522
029900060522       // ----------------------------------------------------------------------
030000060522       // gestione videata
030100060522       // ----------------------------------------------------------------------
030200060522       begsr sr_d01;
030300060522
030400060522       // emetto il video
030500060522          write tb24t01;
030600060522          exfmt tb24d01;
030700060522
030800060522       // controllo tasti funzionali del video
030900060522         select;
031000060522
031100060522       // F3=Fine
031200060522          when *inkc;
031300060522           tb24ricez = 'C';
031400060522           $video = *blanks;
031500060522           exsr sr_uscita;
031600060522           leavesr;
031700060522
031800060522       // F10=Immissione
031900060522          when *inkj;
032000060522           $video = 'D02';
032100060522           clear trtb24ds;
032200060522           tb24comand = 'J';
032300060522           leavesr;
032400060522
032500060522         endsl;
032600060522
032700060522       endsr;
032800060522
032900060522       // ----------------------------------------------------------------------
033000060522       // gestione videata
033100060522       // ----------------------------------------------------------------------
033200060522       begsr sr_d02;
033300060522
033400060522       // imposto il video a seconda della funzione richiesta
033500060522         exsr sr_setvideo;
033600060522
033700060605       // fino a che la variabile resta settata come 'D02'
033800060605        dow $video = 'D02';
033900060605
034000060522       // se immessa opzione di 'scelta'
034100060523         if tb24opzio = 01;
034200060522          $video = *blanks;
034300060522          tb24cod = vs1cod;
034400060522          tb24des = vs1des;
034500060522          exsr sr_uscita;
034600060522          leavesr;
034700060522         endif;
034800060522
034900060522       // se non è immissione/copia proteggo il codice tabella
035000060523          if tb24comand <> 'J' and tb24opzio <> 03;
035100060522           *in02 = *on;
035200060522          endif;
035300060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
035400060522       // proteggo i campi del video
035500060523          if tb24opzio = 04 or tb24opzio = 05;
035600060522           *in01 = *on;
035700060522          endif;
035800060522       // se immessa opzione di 'visualizzazione'
035900060522       // non attivo F6
036000060523          if tb24opzio = 05;
036100060522           *in03 = *on;
036200060522          endif;
036300140527         // -?Se è immissione/copia: non attivo F21=Tab."SP"?
036400140527         *in09 = ( TB24comand <> 'J' and TB24opzio <> 03 );
036500060522
036600060522       // emetto il video
036700060522          write tb24t01;
036800060522          exfmt tb24d02;
036900060522
037000060522       // reset indicatore generico di errore
037100060522         *in28 = *off;
037200060522       // pulisco il campo messaggi
037300060522         clear v2cmsg;
037400060522
037500060522       // controllo tasti funzionali del video
037600060522         select;
037700060522
037800060522       // F3=Fine
037900060522          when *inkc;
038000060522           tb24ricez = 'C';
038100060522           $video = *blanks;
038200060522           exsr sr_uscita;
038300060522           leavesr;
038400060522
038500060522       // F6=Conferma
038600060522          when *inkf;
038700060522           tb24ricez = 'F';
038800060522       // controllo e decodifico i dati del video
038900060522           exsr sr_ctrd02;
039000060522       // se non riscontrati errori emetto la finestra con i dati per la
039100060522       // tramissione
039200060522           if not *in28;
039300060522            wrkcard02 = *on;
039400060523            wrkcars01 = *off;
039500060522            wrkcarw01 = *on;
039600060522            $video = 'W01';
039700060524            exsr sr_w01;
039800060523            leavesr;
039900060522           endif;
040000060523
040100060523       // F8=Record successivo
040200060524          when *inkh;
040300060524            wrkcars01 = *off;
040400060524            wrkcard02 = *on;
040500060523           $video = 'C01';
040600060523           tb24ricez = 'H';
040700060523           leavesr;
040800060522
040900060522       // F9=Traduzione
041000060524          when *inki;
041100060522           *in05 = *on;
041200060522           $video = 'C02';
041300060523           exsr sr_c02;
041400060522
041500060522       // F12=Ritorno
041600060522          when *inkl;
041700060522           wrkcard02 = *on;
041800060524           wrkcars01 = *off;
041900060522           tb24ricez = 'L';
042000060523           if tb24comand = 'J' or tb24opzio = 03;
042100060523            wrkcars01 = *on;
042200060522           endif;
042300060522           $video = 'C01';
042400060522           leavesr;
042500140527
042600140527           // -?F21=Tab."SP"?
042700140527           when  *inKV;
042800140527             exsr  sr_TabSP;
042900060522
043000060522       // Invio
043100060522          other;
043200060522           if not *in01;
043300060522            exsr sr_ctrd02;
043400060522           endif;
043500060522
043600060522         endsl;
043700060605
043800060605       // fine gestione 'D02'
043900060605        enddo;
044000060522
044100060522        wrkcard02 = *off;
044200060522
044300060522       endsr;
044400060510
044500060510       // ----------------------------------------------------------------------
044600060510       // caricamento subfile
044700060510       // ----------------------------------------------------------------------
044800060510       begsr sr_cars01;
044900060510
045000060510       // se è stato richiesto il caricamento del subfile
045100060510        if wrkcars01 = *on;
045200060510
045300060510       // inizializzo il subfile
045400060510         s01nrr = 0;
045500060510         *in20 = *off;
045600060510         *in21 = *off;
045700060523         *in22 = *on;
045800060510         write tb24c01;
045900060510         *in20 = *on;
046000060510         *in21 = *on;
046100060510
046200060510       // imposto la chiave di posizionamento e lettura file
046300060510         tblkut = 1;
046400060510         tblcod = '1P';
046500060517         tblkey = vc1cod;
046600060510
046700060510       // posizionamento sul file
046800060510       // se è stato scelto il posizionamento
046900060517         if vc1cod <> *blanks;
047000060524          rrrn01 = 0;
047100060522          setll (tblkut:tblcod:tblkey) tabel00f;
047200060510       // altrimenti
047300060510         else;
047400060522          setll (tblkut:tblcod) tabel00f;
047500060510         endif;
047600060510
047700060510       // fino a che non è fine file
047800060510         dou %eof(tabel00f);
047900060510
048000060510       // leggo file
048100060510          reade(n) (tblkut:tblcod) tabel00f;
048200060510
048300060510       // fine file esco
048400060510          if %eof(tabel00f);
048500060510           leave;
048600060510          endif;
048700110921
048800110921          ds1p = tbluni;
048900060510
049000060510       // se "ricerca/scelta" non carico i records annullati
049100060510          if (*in10 and tblflg = *blanks) or not *in10;
049200110921       // carico tutto oppure tutto senza assicurazione per conto
049300111003            IF not *in11 or (*in11 and §1Pasc <> 'S' and §1Prcv <> 'S');
049400060510       // scrivo subfile
049500060517           clear vs1opz;
049600060510           exsr sr_wtrs01;
049700111003            ENDIF;
049800060510          endif;
049900060510
050000060510         enddo;
050100060510
050200060510       // fine caricamento subfile
050300060510        endif;
050400060510
050500060510       // disattivo opzione di caricamento subfile
050600060510        wrkcars01 = *off;
050700060510
050800060510       endsr;
050900060510
051000060510       // ----------------------------------------------------------------------
051100060510       // scrivo subfile
051200060510       // ----------------------------------------------------------------------
051300060510       begsr sr_wtrs01;
051400060510
051500060510       // se non raggiunto limite massimo di caricamento
051600060510        if s01nrr < 9999;
051700060510       // imposto campi di subfile
051800060510         exsr sr_sets01;
051900060510       // imposto numeratore righe e numero relativo di record
052000060510         s01nrr = s01nrr + 1;
052100060510       // scrivo subfile
052200060510         write tb24s01;
052300060510        endif;
052400060510
052500060510       endsr;
052600060510
052700060510       // ----------------------------------------------------------------------
052800060510       // imposto campi del subfile
052900060510       // ----------------------------------------------------------------------
053000060510       begsr sr_sets01;
053100060510
053200060522        vs1cod = tblkey;
053300110921        // ds1p = tbluni;
053400060517        vs1des = §1pdes;
053500060517        vs1var = §1pvar;
053600060517        vs1uta = §1puta;
053700060517        vs1upr = §1pupr;
053800060517        vs1uar = §1puar;
053900060517        vs1tco = §1ptco;
054000060517        vs1fie = §1pfie;
054100060517        vs1fdp = §1pfdp;
054200060517        vs1ffe = §1pffe;
054300060517        vs1tst = §1ptst;
054400060517        vs1ost = §1post;
054500060517
054600060517        clear vs1atb;
054700140527        §DAann = c_DspAtrNorm;
054800060522        if tblflg <> *blanks;
054900060517         vs1atb = 'A';
055000140527          §DAann = c_DspAtrRI;
055100060517        endif;
055200060510
055300060510       endsr;
055400060517
055500060517       // ----------------------------------------------------------------------
055600060517       // gestione del subfile
055700060517       // ----------------------------------------------------------------------
055800060517       begsr sr_gestsfl;
055900060517
056000060517        wrkeofs01 = *off;
056100060522        clear s01nrr;
056200060523        *in39 = *off;
056300060517
056400060517       // inizio lettura subfile
056500060522        dow wrkeofs01 = *off;
056600060522         s01nrr = s01nrr + 1;
056700060522         chain s01nrr tb24s01;
056800060522
056900060522       // se non trovo esco
057000060522         if not %found;
057100060524          if vc1cod <> *blanks;
057200060524           wrkcars01 = *on;
057300060524          endif;
057400060522          leavesr;
057500060517         endif;
057600060522
057700060517       // se è stata immessa un'opzione
057800060523         if vs1opz <> *zeros;
057900060517       // controllo se opzione valida
058000060517          xx = %lookup(vs1opz:opz);
058100060517          if xx = *zeros;
058200060517           rec01 = s01nrr;
058300060522           vc1msg = msg(03);
058400060517           *in28 = *on;
058500060523           *in39 = *on;
058600060517           update tb24s01;
058700060518           leavesr;
058800060517          endif;
058900060522
059000060517       // reset ds di servizio
059100060517          clear trtb24ds;
059200060517          tb24opzio = vs1opz;
059300060517
059400060523       // gestione del formato video
059500060522          rec01 = s01nrr;
059600060522          $video = 'D02';
059700060523          exsr sr_d02;
059800060524       // se F12 di ritorno esco dalla lettura del subfile
059900060524          if tb24ricez = 'L';
060000060524          wrkeofs01 = *on;
060100060523          endif;
060200060518         endif;
060300060517
060400060523         clear vs1opz;
060500060522         rec01 = s01nrr;
060600060522         update tb24s01;
060700060517        enddo;
060800060517
060900060517       endsr;
061000060517
061100060517       // ----------------------------------------------------------------------
061200060517       // ripeto opzione in tutte le righe del subfile
061300060517       // ----------------------------------------------------------------------
061400060517       begsr sr_ripetiopz;
061500060517
061600060523        if rrrn01 > 0;
061700060523         s01nrr = rrrn01;
061800060522         chain s01nrr tb24s01;
061900060523         if %found and vs1opz <> *zeros;
062000060522          savopzione = vs1opz;
062100060523          update tb24s01;
062200060517
062300060517          wrkeofs01 = *off;
062400060522          dow wrkeofs01 = *off;
062500060517           s01nrr = s01nrr + 1;
062600060522           chain s01nrr tb24s01;
062700060517           if %found;
062800060522            vs1opz = savopzione;
062900060522            update tb24s01;
063000060517           else;
063100060517            wrkeofs01 = *on;
063200060517           endif;
063300060517          enddo;
063400060517
063500060517         endif;
063600060517
063700060517        endif;
063800060517
063900060517       endsr;
064000060517
064100060517       // ----------------------------------------------------------------------
064200060517       // imposto i dati a video
064300060517       // ----------------------------------------------------------------------
064400060517       begsr sr_setvideo;
064500060517
064600060524        if wrkcard02 = *on;
064700060518       // pulisco il formato video D02
064800060524         exsr sr_puld02;
064900060517
065000060522       // controllo se 'immissione' 'modifica' 'copia' o altro
065100060524         select;
065200060517
065300060517       // F10=Immissione
065400060524          when tb24comand = 'J';
065500060524           vt1dopz = 'Immissione';
065600060517
065700060523       // Opzione "02"=modifica
065800060524          when tb24opzio = 02;
065900060524           vt1dopz = 'Modifica';
066000060524           exsr sr_imposta;
066100060517
066200060523       // Opzione "03"=copia
066300060524          when tb24opzio = 03;
066400060524           vt1dopz = 'Copia';
066500060524           exsr sr_imposta;
066600060517
066700060523       // Opzione "04"=cancellazione/ripristino
066800060524          when tb24opzio = 04;
066900060524           exsr sr_imposta;
067000060517       // se richiesta 'cancellazione'
067100060524           if tblflg = *blanks;
067200060524            vt1dopz = 'Annullamento';
067300060524           endif;
067400060517       // se richiesto 'ripristino'
067500060524           if tblflg <> *blanks;
067600060524            vt1dopz = 'Ripristino';
067700060524           endif;
067800060517
067900060523       // Opzione "05"=visualizzazione
068000060524          when tb24opzio =05;
068100060524           vt1dopz = 'Visualizzazione';
068200060524           exsr sr_imposta;
068300060517
068400060517       // Fine scelta
068500060524         endsl;
068600060517
068700060517       // centro la descrizione della funzione nel formato video
068800060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
068900060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
069000060524
069100060524        endif;
069200140527
069300140527         // -?(Dis)Abilitazione tasti funzionali?
069400140527         // ·?F21=Tab."SP"?
069500140527         *in09 = ( TB24comand <> 'J' );
069600140527
069700060517       endsr;
069800060524
069900060524       // ----------------------------------------------------------------------
070000060524       // pulizia video
070100060524       // ----------------------------------------------------------------------
070200060524       begsr sr_puld02;
070300060524
070400060524        clear vt1dopz;
070500060524        clear v2ccod;
070600060524        clear v2cdes;
070700060524        clear v2cfcp;
070800060524        clear v2cuta;
070900060524        clear v2cupr;
071000060524        clear v2cuar;
071100060524        clear v2cvar;
071200060524        clear v2ctco;
071300060524        clear v2cfie;
071400060524        clear v2cfpt;
071500060524        clear v2cfdp;
071600060524        clear v2cffe;
071700060524        clear v2celi;
071800060524        clear v2ctt0;
071900060524        clear v2ctt1;
072000060524        clear v2ctt2;
072100060524        clear v2ctt3;
072200060524        clear v2ctt4;
072300060524        clear v2ctt5;
072400060524        clear v2ctt6;
072500060524        clear v2ctt7;
072600060524        clear v2ctt8;
072700060524        clear v2ctt9;
072800060524        clear v2cttv;
072900060524        clear v2casc;
073000060524        clear v2crcv;
073100060524        clear v2cvas;
073200060524        clear v2cvvm;
073300060524        clear v2cvca;
073400060524        clear v2cvmn;
073500060524        clear v2cvmx;
073600060524        clear v2ccts;
073700060524        clear v2ccte;
073800060524        clear v2cfqt;
073900060524        clear v2cdfe;
074000060524        clear v2ctps;
074100060524        clear v2cfpc;
074200060524        clear v2ctst;
074300060524        clear v2cost;
074400060524        clear v2cptv;
074500060524        clear v2cain;
074600060731        clear v2cdrf;
074700080227        clear v2cpda;
074800060524
074900060524        *in01 = *off;
075000060524        *in02 = *off;
075100060524        *in03 = *off;
075200060524        *in04 = *off;
075300060524        *in05 = *off;
075400060524        *in28 = *off;
075500060524        *in40 = *off;
075600060524        *in41 = *off;
075700060524        *in42 = *off;
075800060524        *in43 = *off;
075900060524        *in44 = *off;
076000060524        *in45 = *off;
076100060524        *in46 = *off;
076200060524        *in47 = *off;
076300060524        *in48 = *off;
076400060524        *in49 = *off;
076500060524        *in50 = *off;
076600060524        *in51 = *off;
076700060524        *in52 = *off;
076800060524        *in53 = *off;
076900060524        *in54 = *off;
077000060524        *in55 = *off;
077100060524
077200060524       endsr;
077300060524
077400060524       // ----------------------------------------------------------------------
077500060524       // imposto i dati a video
077600060524       // ----------------------------------------------------------------------
077700060524       begsr sr_imposta;
077800060524
077900060524       // recupero i dati dal file
078000060524        tblkut = 1;
078100060731        tblcod = '1P';
078200060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
078300060524
078400060524       // imposto i campi a video
078500060524        v2ccod = tblkey;
078600060524        ds1p = tbluni;
078700060524        v2cdes = §1pdes;
078800060524        v2cfcp = §1pfcp;
078900060524        v2cuta = §1puta;
079000060524        v2cuar = §1puar;
079100060524        v2cupr = §1pupr;
079200060524        v2ctt0 = §1ptt0;
079300060524        v2ctt1 = §1ptt1;
079400060524        v2ctt2 = §1ptt2;
079500060524        v2ctt3 = §1ptt3;
079600060524        v2ctt4 = §1ptt4;
079700060524        v2ctt5 = §1ptt5;
079800060524        v2ctt6 = §1ptt6;
079900060524        v2ctt7 = §1ptt7;
080000060524        v2ctt8 = §1ptt8;
080100060524        v2ctt9 = §1ptt9;
080200060524        v2ccts = §1pcts;
080300060524        v2ccte = §1pcte;
080400060524        v2cptv = §1pptv;
080500060524        v2cvas = §1pvas;
080600060524        v2cvvm = §1pvvm;
080700060524        v2cvca = §1pvca;
080800060524        v2cvar = §1pvar;
080900060524        v2cvmn = §1pvmn;
081000060524        v2cvmx = §1pvmx;
081100060524        v2cfie = §1pfie;
081200060524        v2cain = §1pain;
081300060524        v2cttv = §1pttv;
081400060524        v2casc = §1pasc;
081500060524        v2crcv = §1prcv;
081600060524        v2cfpt = §1pfpt;
081700060524        v2cfpc = §1pfpc;
081800060524        v2ctco = §1ptco;
081900060524        v2ctps = §1ptps;
082000060524        v2cost = §1post;
082100060524        v2ctst = §1ptst;
082200060524        v2cffe = §1pffe;
082300060524        v2cfdp = §1pfdp;
082400060524        v2cdfe = §1pdfe;
082500060524        v2cfqt = §1pfqt;
082600060524        v2celig = %subst(§1peli:7:2);
082700060524        v2celim = %subst(§1peli:5:2);
082800060524        v2celia = %subst(§1peli:1:4);
082900060731        v2cdrf = §1pdrf;
083000080227        v2cpda = §1ppda;
083100130125        v2cModTP = §1PmodTp;
083200060524
083300060524        if tblflg <> *blanks;
083400060524         *in04 = *on;
083500060524        endif;
083600060524
083700060524       endsr;
083800060517
083900060517       // ----------------------------------------------------------------------
084000060517       // controllo video
084100060517       // ----------------------------------------------------------------------
084200060522       begsr sr_ctrd02;
084300060517
084400060517        *in28 = *off;
084500060517        *in40 = *off;
084600060517        *in41 = *off;
084700060523        *in42 = *off;
084800060524        *in43 = *off;
084900060524        *in44 = *off;
085000060524        *in45 = *off;
085100060524        *in46 = *off;
085200060524        *in47 = *off;
085300060524        *in48 = *off;
085400060524        *in49 = *off;
085500060524        *in50 = *off;
085600060524        *in51 = *off;
085700060524        *in52 = *off;
085800060524        *in53 = *off;
085900060524        *in54 = *off;
086000060524        *in55 = *off;
086100060517
086200060522       // codice tabella
086300060522        if v2ccod = *blanks;
086400060522         v2cmsg = msg(01);
086500060517         *in28 = *on;
086600060517         *in40 = *on;
086700060517         leavesr;
086800060517        endif;
086900060517
087000060517       // se immissione controllo che non esista già
087100060523        if tb24comand = 'J' or tb24opzio = 03;
087200060522         tblkut = 1;
087300060522         tblcod = '1P';
087400060522         tblkey = v2ccod;
087500060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
087600060522         if %found(tabel00f);
087700060522          v2cmsg = msg(05);
087800060517          *in28 = *on;
087900060517          *in40 = *on;
088000060517          leavesr;
088100060517         endif;
088200060517        endif;
088300060517
088400060517       // descrizione
088500060522        if v2cdes = *blanks;
088600060522         v2cmsg = msg(02);
088700060517         *in28 = *on;
088800060517         *in41 = *on;
088900060517         leavesr;
089000060517        endif;
089100060523
089200060523       // tipo tariffa
089300060523        xx = 1;
089400060524        for xx by 1 to 11;
089500060523         select;
089600060523          when skctt(xx) = *blanks;
089700060523          when skctt(xx) = '?';
089800060523           t19cod = 'TR';
089900060523           clear t19key;
090000060523           trul19r(t19cod:t19ord:t19key:t19com);
090100060523           skctt(xx) = t19key;
090200060524           yy = xx + 42;
090300060524           *in(yy) = *on;
090400060523          other;
090500060524           exsr sr_ctrctt;
090600060524           if *in28;
090700060524            v2cmsg = msg(06);
090800060524            yy = xx + 42;
090900060524            *in(yy) = *on;
091000060524            leavesr;
091100060524           endif;
091200060523         endsl;
091300060523        endfor;
091400060524
091500060524       // codice tassazione italia
091600060524        if v2ccts = '?';
091700120713         clear TRTB09DS;
091800120713         TB09fun = '1';
091900120713         TB09ord = '1';
092000120713         kpjbu = TRTB09DS;
092100120713         trtb09r (kpjba);
092200120713         TRTB09DS = kpjbu;
092300120713         v2ccts = TB09cod;
092400060524         *in54 = *on;
092500060524        endif;
092600060524        if v2ccts <> *blanks;
092700060524         wrkct = v2ccts;
092800060524         exsr sr_ctrct;
092900060524          if *in28;
093000060524           v2cmsg = msg(07);
093100060524           *in54 = *on;
093200060524           leavesr;
093300060524          endif;
093400060524         endif;
093500060524
093600060524       // codice tassazione estero
093700060524        if v2ccte = '?';
093800120713         clear TRTB09DS;
093900120713         TB09fun = '1';
094000120713         TB09ord = '1';
094100120713         kpjbu = TRTB09DS;
094200120713         trtb09r (kpjba);
094300120713         TRTB09DS = kpjbu;
094400120713         v2ccts = TB09cod;
094500060524         *in55 = *on;
094600060524        endif;
094700060524        if v2ccte <> *blanks;
094800060524         wrkct = v2ccte;
094900060524         exsr sr_ctrct;
095000060524          if *in28;
095100060524           v2cmsg = msg(07);
095200060524           *in55 = *on;
095300060524           leavesr;
095400060524          endif;
095500060524         endif;
095600130125
095700130125       // data eliminazione
095800130125        if v2celi <> *blanks;
095900130125         dataalfa = v2celia + v2celim + v2celig;
096000130125         test(de) *iso0 dataalfa;
096100130125         if %error;
096200130125          v2cmsg = msg(04);
096300130125          *in28 = *on;
096400130125          *in42 = *on;
096500130125          leavesr;
096600130125         endif;
096700130125        endif;
096800061212
096900061212       // riaggancio di nuovo la tabella xchè mi sono sposizionata
097000061212        tblkut = 1;
097100061212        tblcod = '1P';
097200061212        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
097300060517
097400060517       endsr;
097500060524
097600060524       // ----------------------------------------------------------------------
097700060524       // controllo tipo tariffa
097800060524       // ----------------------------------------------------------------------
097900060524       begsr sr_ctrctt;
098000060524
098100060524        tblkut = 1;
098200060524        tblcod = 'TR';
098300060524        tblkey = skctt(xx);
098400060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
098500060524        if not %found(tabel00f) or tblflg <> *blanks;
098600060524         *in28 = *on;
098700060524        endif;
098800060524
098900060524       endsr;
099000060524
099100060524       // ----------------------------------------------------------------------
099200060524       // controllo codice tassazione
099300060524       // ----------------------------------------------------------------------
099400060524       begsr sr_ctrct;
099500060524
099600060524        tblkut = 1;
099700060524        tblcod = 'CT';
099800060524        tblkey = wrkct;
099900060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
100000060524        if not %found(tabel00f) or tblflg <> *blanks;
100100060524         *in28 = *on;
100200060524        endif;
100300060524
100400060524       endsr;
100500060524
100600060524       // ----------------------------------------------------------------------
100700060524       // gestione video dati di trasmissione
100800060524       // ----------------------------------------------------------------------
100900060524       begsr sr_w01;
101000060524
101100060524       // imposto i dati a video
101200060524        exsr sr_carw01;
101300060524
101400060524       // fino a che la variabile resta settata come 'W01'
101500060524        dow $video = 'W01';
101600060524
101700060524       // reset indicatore generico di errore
101800060524         *in28 = *off;
101900060524
102000060524       // emetto il video
102100060524          exfmt tb24w01;
102200060524
102300060524       // controllo tasti funzionali del video
102400060524         select;
102500060524
102600060524       // F6=Conferma
102700060524          when *inkf;
102800060524       // controllo i dati del video
102900060524           exsr sr_ctrw01;
103000060524       // se non riscontrati errori aggiorno il record corrente
103100060524           if not *in28;
103200060524            exsr sr_aggiorna;
103300060524            if not *in28;
103400060524       // emetto le descrizioni in lingua
103500060524       // se non è annullo/ripristino
103600060524             if tb24opzio <> 04;
103700060524              *in05 = *off;
103800060524              $video = 'C02';
103900060524              exsr sr_c02;
104000140527              exsr sr_TabSP;
104100060524              leavesr;
104200060524             endif;
104300060524             if tb24comand = 'J';
104400060524              $video = 'D02';
104500060524              exsr sr_setvideo;
104600060524             else;
104700060524              wrkcars01 = *on;
104800060524              $video = 'C01';
104900060524             endif;
105000060524            endif;
105100060524           endif;
105200060524
105300060524       // F12=Ritorno
105400060524          when *inkl;
105500060524           $video = 'D02';
105600060524           clear tb24ricez;
105700061212           tb24ricez = 'L';
105800061212           wrkcard02 = *off;
105900060524
106000060524       // Invio
106100060524          other;
106200060524           exsr sr_ctrw01;
106300060524
106400060524         endsl;
106500060524
106600060524       // fine gestione 'W01'
106700060524        enddo;
106800060524
106900060524       endsr;
107000060517
107100060517       // ----------------------------------------------------------------------
107200060517       // imposto i dati di trasmissione
107300060517       // ----------------------------------------------------------------------
107400060517       begsr sr_carw01;
107500060517
107600060517       // se pulisco i campi
107700060522         clear w01ftt;
107800060522         clear w01flt;
107900060522         clear w01ftr;
108000060522         clear w01dtr;
108100060517
108200060517       // se non immissione imposto i campi del file
108300060522        if tb24comand <> 'J';
108400060522         w01ftt = tblftt;
108500060522         w01flt = tblflt;
108600060522         w01ftr = tblftr;
108700060517       // imposto la data
108800060522         if tbldtr <> *zeros;
108900060523          datar = tbldtr;
109000060523          aad = aar;
109100060523          mmd = mmr;
109200060523          ggd = ggr;
109300060523          w01dtr = datad;
109400060517         endif;
109500060517
109600060517        endif;
109700060517
109800060517       endsr;
109900060517
110000060517       // ----------------------------------------------------------------------
110100060517       // controllo i dati di trasmissione
110200060517       // ----------------------------------------------------------------------
110300060517       begsr sr_ctrw01;
110400060517
110500060517       endsr;
110600060517
110700060517       // ----------------------------------------------------------------------
110800060517       // aggiorno tabella
110900060517       // ----------------------------------------------------------------------
111000060517       begsr sr_aggiorna;
111100060517
111200060517       // imposto campi del file
111300060522        tblkut = 1;
111400060522        tblcod = '1P';
111500060522        tblkey = v2ccod;
111600060523
111700060523       // aggancio il rcd della tabella
111800060523        chain (tblkut:tblcod:tblkey) tabel00f;
111900060517
112000060522        §1pdes = v2cdes;
112100060522        §1pfcp = v2cfcp;
112200060522        §1puta = v2cuta;
112300060522        §1puar = v2cuar;
112400060522        §1pupr = v2cupr;
112500060522        §1ptt0 = v2ctt0;
112600060522        §1ptt1 = v2ctt1;
112700060522        §1ptt2 = v2ctt2;
112800060522        §1ptt3 = v2ctt3;
112900060522        §1ptt4 = v2ctt4;
113000060522        §1ptt5 = v2ctt5;
113100060522        §1ptt6 = v2ctt6;
113200060522        §1ptt7 = v2ctt7;
113300060522        §1ptt8 = v2ctt8;
113400060522        §1ptt9 = v2ctt9;
113500060522        §1pcts = v2ccts;
113600060522        §1pcte = v2ccte;
113700060522        §1pptv = v2cptv;
113800060522        §1pvas = v2cvas;
113900060522        §1pvvm = v2cvvm;
114000060522        §1pvca = v2cvca;
114100060522        §1pvar = v2cvar;
114200060522        §1pvmn = v2cvmn;
114300060522        §1pvmx = v2cvmx;
114400060522        §1pfie = v2cfie;
114500060522        §1pain = v2cain;
114600060522        §1pttv = v2cttv;
114700060522        §1pasc = v2casc;
114800060522        §1prcv = v2crcv;
114900060522        §1pfpt = v2cfpt;
115000060522        §1pfpc = v2cfpc;
115100060522        §1ptco = v2ctco;
115200060522        §1ptps = v2ctps;
115300060522        §1post = v2cost;
115400060522        §1ptst = v2ctst;
115500060522        §1pffe = v2cffe;
115600060522        §1pfdp = v2cfdp;
115700060522        §1pdfe = v2cdfe;
115800060522        §1pfqt = v2cfqt;
115900060523        §1peli = v2celia + v2celim + v2celig;
116000060731        §1pdrf = v2cdrf;
116100080227        §1ppda = v2cpda;
116200130125        §1PmodTp = v2cModTP;
116300060517
116400060522        tbluni = ds1p;
116500060517
116600060522        tblftt = w01ftt;
116700060522        tblflt = w01flt;
116800060522        clear tblftr;
116900060522        clear tbldtr;
117000060524
117100060524       // annullo/ripristino
117200060524        if tb24opzio = 04;
117300060524         select;
117400060524       // annullamento
117500060524          when tblflg = *blanks;
117600060524           tblflg = '*';
117700060524       // ripristino
117800060524          when tblflg <> *blanks;
117900060524           clear tblflg;
118000060524         endsl;
118100060524        endif;
118200140527
118300140527        SaveTBLflg = TBLflg;
118400060517
118500060523       // controllo se devo scrivere o aggiornare il record
118600060523        if %found(tabel00f);
118700060523         update tabel;
118800060523        else;
118900060523         write tabel;
119000060523        endif;
119100140527
119200140527        // -?Se opzione "04"=cancellazione/ripristino?
119300140527        //  ?aggiorno automaticamente anche la tab. "SP"?
119400140527        IF  TB24opzio = 04;
119500140527          chain  ( 1 : 'SP' : TBLkey )  TABEL;
119600140527          if  %found(TABEL00F);
119700140527            TBLflg = SaveTBLflg;
119800140527            UPDATE  TABEL;
119900140527          endif;
120000140527        ENDIF;
120100060517
120200060523       // se Opzione "04"=cancellazione/ripristino
120300060517       // aggiorno automaticamente anche le descrizioni in linea
120400060523        if tb24opzio = 04;
120500060522         lintabel = 1;
120600060522         setgt lintabel azlin01l;
120700060522         dou %eof(azlin01l);
120800060522          read azlin01l;
120900060517       // fine file
121000060522          if %eof(azlin01l);
121100060517           leave;
121200060517          endif;
121300060517       // aggiorno
121400060522          tblkut = lintabel;
121500140527          // -?Annullo/Ripristino prima la tab. "1P"?
121600140527          chain (tblkut:'1P':tblkey) tabel00f;
121700060522          if %found(tabel00f);
121800140527            TBLflg = SaveTBLflg;
121900060522            update tabel;
122000060517          endif;
122100140527          // -?Poi Annullo/Ripristino ANCHE la tab. "SP"?
122200140527          chain (tblkut:'SP':tblkey) tabel00f;
122300140527          if %found(tabel00f);
122400140527            TBLflg = SaveTBLflg;
122500140527            update tabel;
122600140527          endif;
122700060517         enddo;
122800060517        endif;
122900140527
123000140527         // -?Reimpostazione campi chiave di default?
123100140527         TBLkut = 1;
123200140527         TBLcod = '1P';
123300060517
123400060517       endsr;
123500060524
123600060524       // ----------------------------------------------------------------------
123700060524       // gestione descrizioni in lingua
123800060524       // ----------------------------------------------------------------------
123900060524       begsr sr_c02;
124000060524
124100060524       // imposto il codice tabella e la descrizione
124200060524       vc2cod = v2ccod;
124300060524       vc2des = v2cdes;
124400060524
124500060524       // pulisco il subfile
124600060524        exsr sr_puls02;
124700060524
124800060524       // carico il subfile con i dati che trovo o vuoto
124900060524        exsr sr_cars02;
125000060524
125100060524        wrksfl02 = *on;
125200060524
125300060524        dou wrksfl02 = *off;
125400060524         rec02 = 1;
125500060524         if savrec > *zeros;
125600060524          rec02 = savrec;
125700060524         endif;
125800060524         write tb24z02;
125900060524         exfmt tb24c02;
126000060524
126100060524         *in28 = *off;
126200060524         *in39 = *off;
126300060524
126400060524       // F12=Ritorno
126500060524          if *inkl;
126600060524           wrkcard02 = *off;
126700060524           $video = 'D02';
126800060524           leavesr;
126900060524          endif;
127000060524
127100060524       // Controlli
127200060524          if not *in05;
127300060524           exsr sr_ctrsfl;
127400060524          endif;
127500060524
127600060524       // F6=Conferma
127700060524          if *inkf and not *in28;
127800060524           exsr sr_confsfl;
127900060524           wrksfl02 = *off;
128000060524          endif;
128100060524
128200060524         enddo;
128300060524
128400060524       endsr;
128500060524
128600060524       // ----------------------------------------------------------------------
128700060524       // pulisco il subfile delle descrizioni in lingua
128800060524       // ----------------------------------------------------------------------
128900060524       begsr sr_puls02;
129000060524
129100060524        clear s02nrr;
129200060524        clear savrec;
129300060524        *in20 = *off;
129400060524        *in21 = *off;
129500060524        write tb24c02;
129600060524        *in20 = *on;
129700060524        *in21 = *on;
129800060524
129900060524       endsr;
130000060524
130100060524       // ----------------------------------------------------------------------
130200060524       // carico il subfile delle descrizioni in lingua
130300060524       // ----------------------------------------------------------------------
130400060524       begsr sr_cars02;
130500060524
130600060524        *in22 = *off;
130700060524       // carico un record per ogni lingua inserite in AZLIN
130800060524        lintabel = 1;
130900060524        setgt lintabel azlin01l;
131000060524        dou %eof(azlin01l);
131100060524         read azlin01l;
131200060524       // fine file
131300060524         if %eof(azlin01l);
131400060524          leave;
131500060524         endif;
131600060524         clear ds1p;
131700060524         s2dlin = lindesita;
131800060524         tblkut = lintabel;
131900060524         tblcod = '1P';
132000060524         tblkey = v2ccod;
132100060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
132200060524         if not %found(tabel00f);
132300060524          clear s2cdes;
132400060524         else;
132500060524          ds1p = tbluni;
132600060524          s2cdes = §1pdes;
132700060524         endif;
132800060524         s2hcdlin = tblkut;
132900060524         s02nrr = s02nrr + 1;
133000060524         write tb24s02;
133100060524        enddo;
133200060524
133300060524        *in22 = *on;
133400060524
133500060524       endsr;
133600060524
133700060524       // ----------------------------------------------------------------------
133800060524       // controllo il subfile delle descrizioni in lingua
133900060524       // ----------------------------------------------------------------------
134000060524       begsr sr_ctrsfl;
134100060524
134200060524        s02nrr = 1;
134300060524        chain s02nrr tb24s02;
134400060524        dow %found;
134500060524       // deve esserci la descrizione
134600060524         if s2cdes = *blanks;
134700060524          vc2msg = msg(02);
134800060524          *in28 = *on;
134900060524          *in39 = *on;
135000060524          savrec = s02nrr;
135100060524          update tb24s02;
135200060524          leave;
135300060524         endif;
135400060524         update tb24s02;
135500060524         s02nrr = s02nrr + 1;
135600060524         chain s02nrr tb24s02;
135700060524        enddo;
135800060524
135900060524       endsr;
136000060524
136100060524       // ----------------------------------------------------------------------
136200060524       // confermo il dati del subfile delle descrizioni in lingua
136300060524       // ----------------------------------------------------------------------
136400060524       begsr sr_confsfl;
136500060524
136600060524       // recupero i dati della tabella in italiano
136700060524        clear savtbluni;
136800060524        tblkut = 1;
136900060524        tblcod = '1P';
137000060524        tblkey = v2ccod;
137100060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
137200060524        if %found(tabel00f);
137300060524         savtbluni = tbluni;
137400060524        endif;
137500060524
137600060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
137700060524        s02nrr = 1;
137800060524        chain s02nrr tb24s02;
137900060524        dow %found;
138000060524         tblkut = s2hcdlin;
138100060524         chain (tblkut:tblcod:tblkey) tabel00f;
138200060524         ds1p = savtbluni;
138300060524         §1pdes = s2cdes;
138400060524         tbluni = ds1p;
138500060524         tblftt = w01ftt;
138600060524         clear tblftr;
138700060524         clear tbldtr;
138800060524       // scrivo
138900060524         if not %found(tabel00f);
139000060524          tblkut = s2hcdlin;
139100060524          tblcod = '1P';
139200060524          tblkey = v2ccod;
139300060524          write tabel;
139400060524         else;
139500060524       // aggiorno
139600060524          update tabel;
139700060524         endif;
139800060524         s02nrr = s02nrr + 1;
139900060524         chain s02nrr tb24s02;
140000060524        enddo;
140100060524
140200060524        wrkcars01 = *on;
140300060524        $video = 'C01';
140400060524
140500060524       endsr;
140600140527
140700140527       //--------------------------------------------------------------
140800140527       //?Richiamo *pgm di gestione tab. "SP" per allineamento.        ?
140900140527       //--------------------------------------------------------------
141000140527       BEGSR  sr_TabSP;
141100140527
141200140527         clear  TRTB25ds;
141300140527         TB25cod    = V2Ccod;
141400140527         TB25des    = V2Cdes;
141500140527         if  *inKV;
141600140527           TB25opzio  = 5;
141700140527         else;
141800140527           TB25comand = TB24comand;
141900140527           TB25opzio  = TB24opzio;
142000140527         endif;
142100140527
142200140527         KPJBU = TRTB25ds;
142300140527
142400140527         if  TB24opzio = 3;
142500140527           trtb25r ( kpjba : VS1cod );
142600140527         else;
142700140527           trtb25r ( kpjba );
142800140527         endif;
142900140527
143000140527         //TRTB25ds = KPJBU;
143100140527
143200140527         //if  TB25comand <> 'F';
143300140527         //  ......;
143400140527         //endif;
143500140527
143600140527       ENDSR;
143700060524
143800060524       // ----------------------------------------------------------------------
143900060524       // uscita dal programma
144000060524       // ----------------------------------------------------------------------
144100060524         begsr sr_uscita;
144200060524
144300060524          if tb24esito = *blanks;
144400060524           tb24esito = eseguito;
144500060524          endif;
144600060524
144700060524          kpjbu = trtb24ds;
144800060524
144900060524          *inlr = *on;
145000060524          return;
145100060524
145200060524         endsr;
145300060510
145400060510       // ----------------------------------------------------------------------
145500060510       // routine iniziale
145600060510       // ----------------------------------------------------------------------
145700060510         begsr *inzsr;
145800060510
145900060510      /end-free
146000060510
146100060510     c     *entry        plist
146200060510     c                   parm                    kpjba
146300060510
146400060510      /free
146500060510         in(e) §azute;
146600060510         if not %error;
146700060510          in(e) §datiute;
146800060510         endif;
146900060510         if %error or rsut = *blanks;
147000060510          tibs34r(tibs34ds);
147100060510          in §azute;
147200060510          in §datiute;
147300060510         endif;
147400060510
147500060510       // centro il titolo
147600060510        vt1tit = titolo;
147700060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
147800060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
147900060510
148000060510         endsr;
148100060510
148200060510      /end-free
148300060418
148400060418** -- MSG -------------------------------------------------------------------*
148500060418Immettere il codice                                                            1
148600060418Immettere la descrizione                                                       2
148700060522Opzione errata                                                                 3
148800060523Data errata                                                                    4
148900060522Codice errato                                                                  5
149000060524Tipo tariffa errato                                                            6
149100060524Codice tassazione errato                                                       7
