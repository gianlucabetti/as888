000100140527     /*END
000200140527
000300060418     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400060418
000500060418      *--------------------------------------------------------------*
000600060524      * GESTIONE TABELLA ESTENSIONE TARIFFE PARTICOLARI              *
000700060418      *--------------------------------------------------------------*
000800060418
000900060418     ftabel00f  uf a e           k disk
001000060426     fazlin01l  if   e           k disk
001100060524     ftrtb25d   cf   e             workstn sfile(tb25s01:s01nrr)
001200060524     f                                     sfile(tb25s02:s02nrr)
001300060418      *---------------------------------------------------------------*
001400060510      * riepilogo indicatori
001500060418      *---------------------------------------------------------------*
001600060522      * 01 - protegge campi video 02
001700060510      * 02 - variazione
001800060522      * 03 - non attiva F6 video 02
001900060524      * 04 - rcd annullato
002000060510      * 05 - visualizzo subfile lingue
002100060510      * 10 - ON ricerca/scelta -- OFF gestione
002200060510      * 20 - sfldsp
002300060510      * 21 - sfldspctl -- sflclr
002400060510      * 22 - sflend
002500060524      * 28 - visualizza messaggio errore
002600060524      * 39 - errore subfile
002700060524      * 40 - codice tabella
002800060524      * 41 - descrizione
002900060418      *--------------------------------------------------------------*
003000060418
003100060418      *--------------------------------------------------------------*
003200060510      * campi di work
003300060418      *--------------------------------------------------------------*
003400060522     d blanks          s                   like(vt1dopz)
003500060523     d dataalfa        s              8
003600060523     d dtaiso          s               d   inz
003700060510     d len             s              5u 0
003800060510     d savrec          s                   like(s01nrr)
003900060517     d savopzione      s                   like(vs1opz)
004000060531     d savtblftt       s                   like(tblftt)
004100060531     d savtblflt       s                   like(tblflt)
004200060510     d savtbluni       s                   like(tbluni)
004300060510     d s01nrr          s              4  0
004400060510     d s02nrr          s              4  0
004500060523     d t19cod          s                   like(tblcod)
004600060523     d t19ord          s              1
004700060523     d t19key          s                   like(tblkey)
004800060523     d t19com          s              1
004900140527     d $Called         s               n   inz
005000060522     d wrkcard02       s              1    inz('1')
005100060522     d wrkcars01       s              1    inz('1')
005200060522     d wrkcarw01       s              1
005300060524     d wrkeofs01       s              1
005400060522     d wrksfl02        s              1
005500060517     d $video          s              3    inz('C01')
005600060517     d xx              s              2  0
005700140527
005800140527      *--------------------------------------------------------------*
005900140527      * Parametri
006000140527      *--------------------------------------------------------------*
006100140527     d prm_CodCopy     s                   like(TB25cod)
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
008300060418
008400060524     d dssp          e ds
008500060418     d kpjba         e ds
008600060523     d tibs34ds      e ds                  inz
008700060524     d trtb25ds      e ds
008800140527     d SaveTRTB25ds  e ds                  extname(TRTB25ds)
008900140527     d                                     qualified   inz
009000060510     d §azute        e ds                  extname(azute00f)
009100060510     d                                     dtaara
009200060510     d §datiute      e ds                  extname(ddatiute)
009300060510     d                                     dtaara
009400060510
009500060510     d psds           sds
009600060510     d  pgmname          *proc
009700060510
009800060510      *------------------------------------------------------------------------*
009900060510      * pgm richiamati
010000060510      *------------------------------------------------------------------------*
010100060510     d tibs34r         pr                  extpgm('TIBS34R')
010200060510     d  tibs34ds                           likeds(tibs34ds)
010300060523
010400060523     d trul19r         pr                  extpgm('TRUL19R')
010500060523     d  t19cod1                            like(t19cod)
010600060523     d  t19ord1                            like(t19ord)
010700060523     d  t19key1                            like(t19key)
010800060523     d  t19com1                            like(t19com)
010900060510
011000060510      *--------------------------------------------------------------*
011100060510      * costanti
011200060510      *--------------------------------------------------------------*
011300060510     d errore          c                   '1'
011400060510     d eseguito        c                   '0'
011500060510     d titolo          c                   const('Tabella Tariffe Particolari')
011600060510     d opzg            c                   const('2=Modifica  3=Copia  4=Annull-
011700060510     d                                     o/Ripristino  5=Visualizza')
011800060510     d opzs            c                   const('1=Scelta')
011900060418      *--------------------------------------------------------------*
012000060510
012100060510      /free
012200060510
012300060510       exsr sr_parm;
012400060517       exsr sr_video;
012500060510       exsr sr_uscita;
012600060510
012700060510       // ----------------------------------------------------------------------
012800060510       // elaborazione parametri ricevuti
012900060510       // ----------------------------------------------------------------------
013000060510       begsr sr_parm;
013100060510
013200060524        trtb25ds = kpjbu;
013300140527
013400140527        $Called = ( TB25fun    = *blank  and  TB25cod   <> *blank  and
013500140527                   (TB25comand = 'J'     or   TB25opzio <> *zero) );
013600140527        if  $Called;
013700140527          SaveTRTB25ds = TRTB25ds;
013800140527        else;
013900140527          clear  SaveTRTB25ds;
014000140527        endif;
014100060510
014200060510        select;
014300060510       // ricerca e scleta
014400060524         when tb25fun = '1';
014500060510          *in10 = *on;
014600060510       // imposto le opzioni
014700060522          vc1opz = opzs;
014800060522          clear opz;
014900060523          opz(1) = 01;
015000060510       // gestione
015100060524         when  tb25fun = *blanks;
015200060510          *in10 = *off;
015300060517       // imposto le opzioni
015400060522          vc1opz = opzg;
015500060522          clear opz;
015600060523          opz(1) = 02;
015700060523          opz(2) = 03;
015800060523          opz(3) = 04;
015900060523          opz(4) = 05;
016000060510       // altrimenti
016100060510         other;
016200060517       // esco
016300060524          tb25esito = errore;
016400060510          exsr sr_uscita;
016500060510         endsl;
016600060510
016700060510       endsr;
016800060517
016900060517       // ----------------------------------------------------------------------
017000060517       // gestione delle videate
017100060517       // ----------------------------------------------------------------------
017200060517       begsr sr_video;
017300060517
017400060522        dow $video <> *blanks;
017500060522
017600060522         select;
017700060517       // subfile 01 - tutte le tar.particolari presenti
017800060522          when $video = 'C01';
017900060522           exsr sr_c01;
018000060517
018100060517       // video 01 - nessuna tar.particolare presente
018200060522          when $video = 'D01';
018300060522           exsr sr_d01;
018400060517
018500060517       // video 02 - gestione/visualizzazione tar.particolare scelta
018600060522          when $video = 'D02';
018700060522           exsr sr_d02;
018800060522
018900060522         endsl;
019000060522
019100060522        enddo;
019200060517
019300060517       endsr;
019400060510
019500060510       // ----------------------------------------------------------------------
019600060510       // gestione subfile
019700060510       // ----------------------------------------------------------------------
019800060510       begsr sr_c01;
019900060510
020000060510       // caricamento subfile
020100060517         exsr sr_cars01;
020200060511
020300060511       // se scritto almeno un record
020400060517         if s01nrr > 0;
020500060511       // indicatore di sflend
020600060523          *in22 = *on;
020700060517         endif;
020800140527
020900140527         // -?*pgm richiamato per IMMISSIONE?
021000140527         if  $Called  and  SaveTRTB25ds.TB25comand = 'J';
021100140527           $Video = 'D02';
021200140527           leavesr;
021300140527         endif;
021400060511
021500060511       // se non scritto neanche un record
021600060517         if s01nrr = 0;
021700060511       // emetto videata di dati non trovati
021800060517          $video = 'D01';
021900060524          leavesr;
022000060517         endif;
022100060510
022200060523         if rrrn01 > 0;
022300060523          rec01 = rrrn01;
022400060523         else;
022500060523          rec01 = 1;
022600060517         endif;
022700060523
022800060523       // se non so quale pagina visualizzare forzo pagina 1
022900060523          if rec01 < 1;
023000060523           rec01 = 1;
023100060523          endif;
023200060510
023300060510       // Emissione del subfile
023400140527         IF  Not $Called;
023500060524          write tb25t01;
023600060524          write tb25z01;
023700060524          exfmt tb25c01;
023800140527         ENDIF;
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
025100060524            tb25ricez = 'C';
025200060510            exsr sr_uscita;
025300060510
025400060510       // F5=Refresh
025500060510           when *inke;
025600060510            wrkcars01 = *on;
025700060510
025800060510       // F10=Immissione
025900060510           when *inkj;
026000060511            $video = 'D02';
026100060524            clear trtb25ds;
026200060524            tb25comand = 'J';
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
027700140527       // gestione videata (subfile vuoto)
027800060522       // ----------------------------------------------------------------------
027900060522       begsr sr_d01;
028000060522
028100060522       // emetto il video
028200060524          write tb25t01;
028300060524          exfmt tb25d01;
028400060522
028500060522       // controllo tasti funzionali del video
028600060522         select;
028700060522
028800060522       // F3=Fine
028900060522          when *inkc;
029000060524           tb25ricez = 'C';
029100060522           $video = *blanks;
029200060522           exsr sr_uscita;
029300060522           leavesr;
029400060522
029500060522       // F10=Immissione
029600060522          when *inkj;
029700060522           $video = 'D02';
029800060524           clear trtb25ds;
029900060524           tb25comand = 'J';
030000060522           leavesr;
030100060522
030200060522         endsl;
030300060522
030400060522       endsr;
030500060522
030600060522       // ----------------------------------------------------------------------
030700140527       // gestione videata (singola tariffa particolare)
030800060522       // ----------------------------------------------------------------------
030900060522       begsr sr_d02;
031000060522
031100060522       // imposto il video a seconda della funzione richiesta
031200060522         exsr sr_setvideo;
031300060605
031400060605       // fino a che la variabile resta settata come 'D02'
031500060605        dow $video = 'D02';
031600060522
031700060522       // se immessa opzione di 'scelta'
031800060524         if tb25opzio = 01;
031900060522          $video = *blanks;
032000060524          tb25cod = vs1cod;
032100060524          tb25des = vs1des;
032200060522          exsr sr_uscita;
032300060522          leavesr;
032400060522         endif;
032500060522
032600060522       // se non è immissione/copia proteggo il codice tabella
032700060524          if tb25comand <> 'J' and tb25opzio <> 03;
032800060522           *in02 = *on;
032900060522          endif;
033000060522       // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
033100060522       // proteggo i campi del video
033200060524          if tb25opzio = 04 or tb25opzio = 05;
033300060522           *in01 = *on;
033400060522          endif;
033500060522       // se immessa opzione di 'visualizzazione'
033600060522       // non attivo F6
033700060524          if tb25opzio = 05;
033800060522           *in03 = *on;
033900060522          endif;
034000140527          //?Se richiamato: non attivo F8=Successivo?
034100140527          *in08 = ( Not $Called );
034200060522
034300060522       // emetto il video
034400060524          write tb25t01;
034500060524          exfmt tb25d02;
034600060522
034700060522       // reset indicatore generico di errore
034800060522         *in28 = *off;
034900060522       // pulisco il campo messaggi
035000060522         clear v2cmsg;
035100060522
035200060522       // controllo tasti funzionali del video
035300060522         select;
035400060522
035500060522       // F3=Fine
035600060522          when *inkc;
035700060524           tb25ricez = 'C';
035800060522           $video = *blanks;
035900060522           exsr sr_uscita;
036000060522           leavesr;
036100060522
036200060522       // F6=Conferma
036300060522          when *inkf;
036400060524           tb25ricez = 'F';
036500060522       // controllo e decodifico i dati del video
036600060522           exsr sr_ctrd02;
036700060522       // se non riscontrati errori emetto la finestra con i dati per la
036800060522       // tramissione
036900060522           if not *in28;
037000060522            wrkcard02 = *on;
037100060523            wrkcars01 = *off;
037200060522            wrkcarw01 = *on;
037300060522            $video = 'W01';
037400060524            exsr sr_w01;
037500060523            leavesr;
037600060522           endif;
037700060523
037800060523       // F8=Record successivo
037900060524          when *inkh;
038000060524            wrkcars01 = *off;
038100060524            wrkcard02 = *on;
038200060524           tb25ricez = 'H';
038300140527            if  Not $Called;
038400140527              $Video = 'C01';
038500140527              leavesr;
038600140527            else;
038700140527              $Video = *blanks;
038800140527              exsr  sr_Uscita;
038900140527            endif;
039000060522
039100060522       // F9=Traduzione
039200060524          when *inki;
039300060522           *in05 = *on;
039400060522           $video = 'C02';
039500060523           exsr sr_c02;
039600060522
039700060522       // F12=Ritorno
039800060522          when *inkl;
039900060522           wrkcard02 = *on;
040000060524           wrkcars01 = *off;
040100060524           tb25ricez = 'L';
040200060524           if tb25comand = 'J' or tb25opzio = 03;
040300060523            wrkcars01 = *on;
040400060522           endif;
040500140527            if  Not $Called;
040600060522           $video = 'C01';
040700060522           leavesr;
040800140527            else;
040900140527              $Video = *blanks;
041000140527              exsr sr_Uscita;
041100140527            endif;
041200060522
041300060522       // Invio
041400060522          other;
041500060522           if not *in01;
041600060522            exsr sr_ctrd02;
041700060522           endif;
041800060522
041900060522         endsl;
042000060605
042100060605       // fine gestione 'D02'
042200060605        enddo;
042300060522
042400060522        wrkcard02 = *off;
042500060522
042600060522       endsr;
042700060510
042800060510       // ----------------------------------------------------------------------
042900060510       // caricamento subfile
043000060510       // ----------------------------------------------------------------------
043100060510       begsr sr_cars01;
043200060510
043300060510       // se è stato richiesto il caricamento del subfile
043400060510        if wrkcars01 = *on;
043500060510
043600060510       // inizializzo il subfile
043700060510         s01nrr = 0;
043800060510         *in20 = *off;
043900060510         *in21 = *off;
044000060523         *in22 = *on;
044100060524         write tb25c01;
044200060510         *in20 = *on;
044300060510         *in21 = *on;
044400060510
044500060510       // imposto la chiave di posizionamento e lettura file
044600140527         if  $Called;
044700140527           if  SaveTRTB25ds.TB25opzio = 3  and  %parms() > 1;
044800140527             VC1cod = prm_CodCopy;
044900140527           else;
045000140527             VC1cod = SaveTRTB25ds.TB25cod;
045100140527           endif;
045200140527         endif;
045300060510         tblkut = 1;
045400060524         tblcod = 'SP';
045500140527         tblkey = vc1cod;
045600060510
045700060510       // posizionamento sul file
045800060510       // se è stato scelto il posizionamento
045900060517         if vc1cod <> *blanks;
046000060524          rrrn01 = 0;
046100060522          setll (tblkut:tblcod:tblkey) tabel00f;
046200060510       // altrimenti
046300060510         else;
046400060522          setll (tblkut:tblcod) tabel00f;
046500060510         endif;
046600060510
046700060510       // fino a che non è fine file
046800060510         dou %eof(tabel00f);
046900060510
047000060510       // leggo file
047100060510          reade(n) (tblkut:tblcod) tabel00f;
047200060510
047300060510       // fine file esco
047400060510          if %eof(tabel00f);
047500060510           leave;
047600060510          endif;
047700060510
047800060510       // se "ricerca/scelta" non carico i records annullati
047900060510          if (*in10 and tblflg = *blanks) or not *in10;
048000140527            // -?Se *pgm richiamato x singola Tariffa Particolare?
048100140527            //  ?carico solo quella?
048200140527            If  Not $Called  or
048300140527                ($Called    and  (SaveTRTB25ds.TB25opzio = 3  and
048400140527                                  %parms() > 1                and
048500140527                                  TBLkey = prm_CodCopy)
048600140527                             or  TBLkey  = SaveTRTB25ds.TB25cod);
048700060510       // scrivo subfile
048800060517           clear vs1opz;
048900140527              if  $Called;
049000140527                VS1opz = SaveTRTB25ds.TB25opzio;
049100140527              endif;
049200060510           exsr sr_wtrs01;
049300140527            EndIf;
049400060510          endif;
049500060510
049600060510         enddo;
049700060510
049800060510       // fine caricamento subfile
049900060510        endif;
050000060510
050100060510       // disattivo opzione di caricamento subfile
050200060510        wrkcars01 = *off;
050300060510
050400060510       endsr;
050500060510
050600060510       // ----------------------------------------------------------------------
050700060510       // scrivo subfile
050800060510       // ----------------------------------------------------------------------
050900060510       begsr sr_wtrs01;
051000060510
051100060510       // se non raggiunto limite massimo di caricamento
051200060510        if s01nrr < 9999;
051300060510       // imposto campi di subfile
051400060510         exsr sr_sets01;
051500060510       // imposto numeratore righe e numero relativo di record
051600060510         s01nrr = s01nrr + 1;
051700060510       // scrivo subfile
051800060524         write tb25s01;
051900060510        endif;
052000060510
052100060510       endsr;
052200060510
052300060510       // ----------------------------------------------------------------------
052400060510       // imposto campi del subfile
052500060510       // ----------------------------------------------------------------------
052600060510       begsr sr_sets01;
052700060510
052800060522        vs1cod = tblkey;
052900060524        dssp = tbluni;
053000060524        vs1des = §spdes;
053100060524        vs1dfe = §spdfe;
053200060517
053300060517        clear vs1atb;
053400060522        if tblflg <> *blanks;
053500060517         vs1atb = 'A';
053600060517        endif;
053700060510
053800060510       endsr;
053900060517
054000060517       // ----------------------------------------------------------------------
054100060517       // gestione del subfile
054200060517       // ----------------------------------------------------------------------
054300060517       begsr sr_gestsfl;
054400060517
054500060517        wrkeofs01 = *off;
054600060522        clear s01nrr;
054700060523        *in39 = *off;
054800060517
054900060517       // inizio lettura subfile
055000060522        dow wrkeofs01 = *off;
055100060522         s01nrr = s01nrr + 1;
055200060524         chain s01nrr tb25s01;
055300060522
055400060522       // se non trovo esco
055500060522         if not %found;
055600060524          if vc1cod <> *blanks;
055700060524           wrkcars01 = *on;
055800060524          endif;
055900060522          leavesr;
056000060517         endif;
056100060522
056200060517       // se è stata immessa un'opzione
056300060523         if vs1opz <> *zeros;
056400060517       // controllo se opzione valida
056500060517          xx = %lookup(vs1opz:opz);
056600060517          if xx = *zeros;
056700060517           rec01 = s01nrr;
056800060522           vc1msg = msg(03);
056900060517           *in28 = *on;
057000060523           *in39 = *on;
057100060524           update tb25s01;
057200060518           leavesr;
057300060517          endif;
057400060522
057500060517       // reset ds di servizio
057600060524          clear trtb25ds;
057700060524          tb25opzio = vs1opz;
057800060517
057900060523       // gestione del formato video
058000060522          rec01 = s01nrr;
058100060522          $video = 'D02';
058200060523          exsr sr_d02;
058300060524       // se F12 di ritorno esco dalla lettura del subfile
058400060524          if tb25ricez = 'L';
058500060524          wrkeofs01 = *on;
058600060523          endif;
058700060518         endif;
058800060517
058900060523         clear vs1opz;
059000060522         rec01 = s01nrr;
059100060524         update tb25s01;
059200060517        enddo;
059300060517
059400060517       endsr;
059500060517
059600060517       // ----------------------------------------------------------------------
059700060517       // ripeto opzione in tutte le righe del subfile
059800060517       // ----------------------------------------------------------------------
059900060517       begsr sr_ripetiopz;
060000060517
060100060523        if rrrn01 > 0;
060200060523         s01nrr = rrrn01;
060300060524         chain s01nrr tb25s01;
060400060523         if %found and vs1opz <> *zeros;
060500060522          savopzione = vs1opz;
060600060524          update tb25s01;
060700060517
060800060517          wrkeofs01 = *off;
060900060522          dow wrkeofs01 = *off;
061000060517           s01nrr = s01nrr + 1;
061100060524           chain s01nrr tb25s01;
061200060517           if %found;
061300060522            vs1opz = savopzione;
061400060524            update tb25s01;
061500060517           else;
061600060517            wrkeofs01 = *on;
061700060517           endif;
061800060517          enddo;
061900060517
062000060517         endif;
062100060517
062200060517        endif;
062300060517
062400060517       endsr;
062500060517
062600060517       // ----------------------------------------------------------------------
062700060517       // imposto i dati a video
062800060517       // ----------------------------------------------------------------------
062900060517       begsr sr_setvideo;
063000060517
063100060524        if wrkcard02 = *on;
063200060518       // pulisco il formato video D02
063300060524         exsr sr_puld02;
063400060517
063500060522       // controllo se 'immissione' 'modifica' 'copia' o altro
063600060524         select;
063700060517
063800060517       // F10=Immissione
063900060524          when tb25comand = 'J';
064000060524           vt1dopz = 'Immissione';
064100140527               if  $Called;
064200140527                 V2Ccod = SaveTRTB25ds.TB25cod;
064300140527                 V2Dcod = SaveTRTB25ds.TB25des;
064400140527               endif;
064500060517
064600060523       // Opzione "02"=modifica
064700060524          when tb25opzio = 02;
064800060524           vt1dopz = 'Modifica';
064900060524           exsr sr_imposta;
065000060517
065100060523       // Opzione "03"=copia
065200060524          when tb25opzio = 03;
065300060524           vt1dopz = 'Copia';
065400060524           exsr sr_imposta;
065500060517
065600060523       // Opzione "04"=cancellazione/ripristino
065700060524          when tb25opzio = 04;
065800060524           exsr sr_imposta;
065900060517       // se richiesta 'cancellazione'
066000060524           if tblflg = *blanks;
066100060524            vt1dopz = 'Annullamento';
066200060524           endif;
066300060517       // se richiesto 'ripristino'
066400060524           if tblflg <> *blanks;
066500060524            vt1dopz = 'Ripristino';
066600060524           endif;
066700060517
066800060523       // Opzione "05"=visualizzazione
066900060524          when tb25opzio =05;
067000060524           vt1dopz = 'Visualizzazione';
067100060524           exsr sr_imposta;
067200060517
067300060517       // Fine scelta
067400060524         endsl;
067500060517
067600060517       // centro la descrizione della funzione nel formato video
067700060524         len = (%len(vt1dopz) - %len(%trimr(vt1dopz))) / 2;
067800060524         vt1dopz = %subst(blanks:1:len) + %trimr(vt1dopz);
067900060524
068000060524        endif;
068100060517
068200060517       endsr;
068300060524
068400060524       // ----------------------------------------------------------------------
068500060524       // pulizia video
068600060524       // ----------------------------------------------------------------------
068700060524       begsr sr_puld02;
068800060524
068900060524        clear vt1dopz;
069000060524        clear v2ccod;
069100140527        clear V2Dcod;
069200060524        clear v2cdes;
069300060524        clear v2cdfe;
069400060524        clear v2cdac;
069500060524        clear v2csud;
069600060524        clear v2caba;
069700060524
069800060524        *in01 = *off;
069900060524        *in02 = *off;
070000060524        *in03 = *off;
070100060524        *in04 = *off;
070200060524        *in05 = *off;
070300060524        *in28 = *off;
070400060524        *in40 = *off;
070500060524        *in41 = *off;
070600060524
070700060524       endsr;
070800060524
070900060524       // ----------------------------------------------------------------------
071000060524       // imposto i dati a video
071100060524       // ----------------------------------------------------------------------
071200060524       begsr sr_imposta;
071300060524
071400060524       // recupero i dati dal file
071500060524        tblkut = 1;
071600060524        chain(n) (tblkut:tblcod:vs1cod) tabel00f;
071700060524
071800060524       // imposto i campi a video
071900140527         if  $Called;
072000140527           V2Ccod = SaveTRTB25ds.TB25cod;
072100140527           V2Dcod = SaveTRTB25ds.TB25des;
072200140527         else;
072300060524        v2ccod = tblkey;
072400140527         endif;
072500060524        dssp = tbluni;
072600060524        v2cdes = §spdes;
072700060524        v2cdfe = §spdfe;
072800060524        v2cdac = §spdac;
072900060524        v2csud = §spsud;
073000060524        v2caba = §spaba;
073100060524
073200060524        if tblflg <> *blanks;
073300060524         *in04 = *on;
073400060524        endif;
073500060524
073600060524       endsr;
073700060517
073800060517       // ----------------------------------------------------------------------
073900060517       // controllo video
074000060517       // ----------------------------------------------------------------------
074100060522       begsr sr_ctrd02;
074200060517
074300060517        *in28 = *off;
074400060517        *in40 = *off;
074500060517        *in41 = *off;
074600060517
074700060522       // codice tabella
074800060522        if v2ccod = *blanks;
074900060522         v2cmsg = msg(01);
075000060517         *in28 = *on;
075100060517         *in40 = *on;
075200060517         leavesr;
075300060517        endif;
075400060517
075500060517       // se immissione controllo che non esista già
075600060524        if tb25comand = 'J' or tb25opzio = 03;
075700060522         tblkut = 1;
075800060524         tblcod = 'SP';
075900060522         tblkey = v2ccod;
076000060522         chain(n) (tblkut:tblcod:tblkey) tabel00f;
076100060522         if %found(tabel00f);
076200060522          v2cmsg = msg(05);
076300060517          *in28 = *on;
076400060517          *in40 = *on;
076500060517          leavesr;
076600060517         endif;
076700060517        endif;
076800060517
076900060517       // descrizione
077000140527         //  ?NON più obbligatoria?
077100140527         //if v2cdes = *blanks;
077200140527         // v2cmsg = msg(02);
077300140527         // *in28 = *on;
077400140527         // *in41 = *on;
077500140527         // leavesr;
077600140527         //endif;
077700060517
077800060517       endsr;
077900060524
078000060524       // ----------------------------------------------------------------------
078100060524       // gestione video dati di trasmissione
078200060524       // ----------------------------------------------------------------------
078300060524       begsr sr_w01;
078400060524
078500060524       // imposto i dati a video
078600060524        exsr sr_carw01;
078700060524
078800060524       // fino a che la variabile resta settata come 'W01'
078900060524        dow $video = 'W01';
079000060524
079100060524       // reset indicatore generico di errore
079200060524         *in28 = *off;
079300060524
079400060524       // emetto il video
079500060524          exfmt tb25w01;
079600060524
079700060524       // controllo tasti funzionali del video
079800060524         select;
079900060524
080000060524       // F6=Conferma
080100060524          when *inkf;
080200060524       // controllo i dati del video
080300060524           exsr sr_ctrw01;
080400060524       // se non riscontrati errori aggiorno il record corrente
080500060524           if not *in28;
080600060524            exsr sr_aggiorna;
080700060524            if not *in28;
080800060524       // emetto le descrizioni in lingua
080900060524       // se non è annullo/ripristino
081000060524             if tb25opzio <> 04;
081100060524              *in05 = *off;
081200060524              $video = 'C02';
081300060524              exsr sr_c02;
081400060524              leavesr;
081500060524             endif;
081600140527             if  $Called;
081700140527               $Video = *blanks;
081800140527               exsr  sr_Uscita;
081900140527             endif;
082000060524             if tb25comand = 'J';
082100060524              $video = 'D02';
082200060524              exsr sr_setvideo;
082300060524             else;
082400060524              wrkcars01 = *on;
082500060524              $video = 'C01';
082600060524             endif;
082700060524            endif;
082800060524           endif;
082900060524
083000060524       // F12=Ritorno
083100060524          when *inkl;
083200060524           $video = 'D02';
083300060524           clear tb25ricez;
083400060524
083500060524       // Invio
083600060524          other;
083700060524           exsr sr_ctrw01;
083800060524
083900060524         endsl;
084000060524
084100060524       // fine gestione 'W01'
084200060524        enddo;
084300060524
084400060524       endsr;
084500060517
084600060517       // ----------------------------------------------------------------------
084700060517       // imposto i dati di trasmissione
084800060517       // ----------------------------------------------------------------------
084900060517       begsr sr_carw01;
085000060517
085100060517       // se pulisco i campi
085200060522         clear w01ftt;
085300060522         clear w01flt;
085400060522         clear w01ftr;
085500060522         clear w01dtr;
085600060517
085700060517       // se non immissione imposto i campi del file
085800060524        if tb25comand <> 'J';
085900060522         w01ftt = tblftt;
086000060522         w01flt = tblflt;
086100060522         w01ftr = tblftr;
086200060517       // imposto la data
086300060522         if tbldtr <> *zeros;
086400060523          datar = tbldtr;
086500060523          aad = aar;
086600060523          mmd = mmr;
086700060523          ggd = ggr;
086800060523          w01dtr = datad;
086900060517         endif;
087000060517
087100060517        endif;
087200060517
087300060517       endsr;
087400060517
087500060517       // ----------------------------------------------------------------------
087600060517       // controllo i dati di trasmissione
087700060517       // ----------------------------------------------------------------------
087800060517       begsr sr_ctrw01;
087900060517
088000060517       endsr;
088100060517
088200060517       // ----------------------------------------------------------------------
088300060517       // aggiorno tabella
088400060517       // ----------------------------------------------------------------------
088500060517       begsr sr_aggiorna;
088600060517
088700060517       // imposto campi del file
088800060522        tblkut = 1;
088900060524        tblcod = 'SP';
089000060522        tblkey = v2ccod;
089100060523
089200060523       // aggancio il rcd della tabella
089300060523        chain (tblkut:tblcod:tblkey) tabel00f;
089400060517
089500060524        §spdes = v2cdes;
089600060524        §spdfe = v2cdfe;
089700060524        §spdac = v2cdac;
089800060524        §spsud = v2csud;
089900060524        §spaba = v2caba;
090000060517
090100060524        tbluni = dssp;
090200060517
090300060522        tblftt = w01ftt;
090400060522        tblflt = w01flt;
090500060522        clear tblftr;
090600060522        clear tbldtr;
090700060524
090800060524       // annullo/ripristino
090900060524        if tb25opzio = 04;
091000060524         select;
091100060524       // annullamento
091200060524          when tblflg = *blanks;
091300060524           tblflg = '*';
091400060524       // ripristino
091500060524          when tblflg <> *blanks;
091600060524           clear tblflg;
091700060524         endsl;
091800060524        endif;
091900060517
092000060523       // controllo se devo scrivere o aggiornare il record
092100060523        if %found(tabel00f);
092200060523         update tabel;
092300060523        else;
092400060523         write tabel;
092500060523        endif;
092600060517
092700060523       // se Opzione "04"=cancellazione/ripristino
092800060517       // aggiorno automaticamente anche le descrizioni in linea
092900060524        if tb25opzio = 04;
093000060522         lintabel = 1;
093100060522         setgt lintabel azlin01l;
093200060522         dou %eof(azlin01l);
093300060522          read azlin01l;
093400060517       // fine file
093500060522          if %eof(azlin01l);
093600060517           leave;
093700060517          endif;
093800060517       // aggiorno
093900060522          tblkut = lintabel;
094000060522          chain (tblkut:tblcod:tblkey) tabel00f;
094100060522          if %found(tabel00f);
094200060517           select;
094300060522            when tblflg <> *blanks;
094400060522             clear tblflg;
094500060523            when tblflg = *blanks;
094600060522             tblflg = '*';
094700060517           endsl;
094800060522            update tabel;
094900060517          endif;
095000060517         enddo;
095100060517        endif;
095200060517
095300060517       endsr;
095400060524
095500060524       // ----------------------------------------------------------------------
095600060524       // gestione descrizioni in lingua
095700060524       // ----------------------------------------------------------------------
095800060524       begsr sr_c02;
095900060524
096000060524       // imposto il codice tabella e la descrizione
096100060524       vc2cod = v2ccod;
096200060524       vc2des = v2cdes;
096300060524       vc2dfe = v2cdfe;
096400060524
096500060524       // pulisco il subfile
096600060524        exsr sr_puls02;
096700060524
096800060524       // carico il subfile con i dati che trovo o vuoto
096900060524        exsr sr_cars02;
097000060524
097100060524        wrksfl02 = *on;
097200060524
097300060524        dou wrksfl02 = *off;
097400060524         rec02 = 1;
097500060524         if savrec > *zeros;
097600060524          rec02 = savrec;
097700060524         endif;
097800060524         write tb25z02;
097900060524         exfmt tb25c02;
098000060524
098100060524         *in28 = *off;
098200060524         *in39 = *off;
098300060524
098400060524       // F12=Ritorno
098500060524          if *inkl;
098600060524           wrkcard02 = *off;
098700060524           $video = 'D02';
098800060524           leavesr;
098900060524          endif;
099000060524
099100060524       // Controlli
099200060524          if not *in05;
099300060524           exsr sr_ctrsfl;
099400060524          endif;
099500060524
099600060524       // F6=Conferma
099700060524          if *inkf and not *in28;
099800060524           exsr sr_confsfl;
099900060524           wrksfl02 = *off;
100000060524          endif;
100100060524
100200060524         enddo;
100300140527
100400140527         if  $Called;
100500140527           $Video = *blanks;
100600140527           exsr  sr_Uscita;
100700140527         endif;
100800060524
100900060524       endsr;
101000060524
101100060524       // ----------------------------------------------------------------------
101200060524       // pulisco il subfile delle descrizioni in lingua
101300060524       // ----------------------------------------------------------------------
101400060524       begsr sr_puls02;
101500060524
101600060524        clear s02nrr;
101700060524        clear savrec;
101800060524        *in20 = *off;
101900060524        *in21 = *off;
102000060524        write tb25c02;
102100060524        *in20 = *on;
102200060524        *in21 = *on;
102300060524
102400060524       endsr;
102500060524
102600060524       // ----------------------------------------------------------------------
102700060524       // carico il subfile delle descrizioni in lingua
102800060524       // ----------------------------------------------------------------------
102900060524       begsr sr_cars02;
103000060524
103100060524        *in22 = *off;
103200060524       // carico un record per ogni lingua inserite in AZLIN
103300060524        lintabel = 1;
103400060524        setgt lintabel azlin01l;
103500060524        dou %eof(azlin01l);
103600060524         read azlin01l;
103700060524       // fine file
103800060524         if %eof(azlin01l);
103900060524          leave;
104000060524         endif;
104100060524         clear dssp;
104200060524         s2dlin = lindesita;
104300060524         tblkut = lintabel;
104400060524         tblcod = 'SP';
104500060524         tblkey = v2ccod;
104600060524         chain(n) (tblkut:tblcod:tblkey) tabel00f;
104700060524         if not %found(tabel00f);
104800060524          clear s2cdes;
104900060727          clear s2cdfe;
105000060524         else;
105100060524          dssp = tbluni;
105200060524          s2cdes = §spdes;
105300060524          s2cdfe = §spdfe;
105400060524         endif;
105500060524         s2hcdlin = tblkut;
105600060524         s02nrr = s02nrr + 1;
105700060524         write tb25s02;
105800060524        enddo;
105900060524
106000060524        *in22 = *on;
106100060524
106200060524       endsr;
106300060524
106400060524       // ----------------------------------------------------------------------
106500060524       // controllo il subfile delle descrizioni in lingua
106600060524       // ----------------------------------------------------------------------
106700060524       begsr sr_ctrsfl;
106800060524
106900060524        s02nrr = 1;
107000060524        chain s02nrr tb25s02;
107100060524        dow %found;
107200060524       // deve esserci la descrizione
107300060524         if s2cdes = *blanks;
107400060524          vc2msg = msg(02);
107500060524          *in28 = *on;
107600060524          *in39 = *on;
107700060524          savrec = s02nrr;
107800060524          update tb25s02;
107900060524          leave;
108000060524         endif;
108100060524         update tb25s02;
108200060524         s02nrr = s02nrr + 1;
108300060524         chain s02nrr tb25s02;
108400060524        enddo;
108500060524
108600060524       endsr;
108700060524
108800060524       // ----------------------------------------------------------------------
108900060524       // confermo il dati del subfile delle descrizioni in lingua
109000060524       // ----------------------------------------------------------------------
109100060524       begsr sr_confsfl;
109200060524
109300060524       // recupero i dati della tabella in italiano
109400060524        clear savtbluni;
109500060524        tblkut = 1;
109600060524        tblcod = 'SP';
109700060524        tblkey = v2ccod;
109800060524        chain(n) (tblkut:tblcod:tblkey) tabel00f;
109900060524        if %found(tabel00f);
110000060524         savtbluni = tbluni;
110100060524        endif;
110200060524
110300060524       // leggo il subfile e aggiorno/scrivo i dati in lingua
110400060524        s02nrr = 1;
110500060524        chain s02nrr tb25s02;
110600060524        dow %found;
110700060524         tblkut = s2hcdlin;
110800060524         chain (tblkut:tblcod:tblkey) tabel00f;
110900060524         dssp = savtbluni;
111000060524         §spdes = s2cdes;
111100060524         §spdfe = s2cdfe;
111200060524         tbluni = dssp;
111300060524         tblftt = w01ftt;
111400060524         clear tblftr;
111500060524         clear tbldtr;
111600060524       // scrivo
111700060524         if not %found(tabel00f);
111800060524          tblkut = s2hcdlin;
111900060524          tblcod = 'SP';
112000060524          tblkey = v2ccod;
112100060524          write tabel;
112200060524         else;
112300060524       // aggiorno
112400060524          update tabel;
112500060524         endif;
112600060524         s02nrr = s02nrr + 1;
112700060524         chain s02nrr tb25s02;
112800060524        enddo;
112900060524
113000060524        wrkcars01 = *on;
113100060524        $video = 'C01';
113200060524
113300060524       endsr;
113400060524
113500060524       // ----------------------------------------------------------------------
113600060524       // uscita dal programma
113700060524       // ----------------------------------------------------------------------
113800060524         begsr sr_uscita;
113900060524
114000060524          if tb25esito = *blanks;
114100060524           tb25esito = eseguito;
114200060524          endif;
114300060524
114400060524          kpjbu = trtb25ds;
114500060524
114600060524          *inlr = *on;
114700060524          return;
114800060524
114900060524         endsr;
115000060510
115100060510       // ----------------------------------------------------------------------
115200060510       // routine iniziale
115300060510       // ----------------------------------------------------------------------
115400060510         begsr *inzsr;
115500060510
115600060510      /end-free
115700060510
115800060510     c     *entry        plist
115900060510     c                   parm                    kpjba
116000140527     c                   parm                    prm_CodCopy
116100060510
116200060510      /free
116300060510         in(e) §azute;
116400060510         if not %error;
116500060510          in(e) §datiute;
116600060510         endif;
116700060510         if %error or rsut = *blanks;
116800060510          tibs34r(tibs34ds);
116900060510          in §azute;
117000060510          in §datiute;
117100060510         endif;
117200060510
117300060510       // centro il titolo
117400060510        vt1tit = titolo;
117500060510        len = (%len(vt1tit) - %len(%trimr(vt1tit))) / 2;
117600060510        vt1tit = %subst(blanks:1:len) + %trimr(vt1tit);
117700060510
117800060510         endsr;
117900060510
118000060510      /end-free
118100060418
118200060418** -- MSG -------------------------------------------------------------------*
118300060418Immettere il codice                                                            1
118400060418Immettere la descrizione                                                       2
118500060522Opzione errata                                                                 3
118600060523Data errata                                                                    4
118700060522Codice errato                                                                  5
118800060524Tipo tariffa errato                                                            6
118900060524Codice tassazione errato                                                       7
