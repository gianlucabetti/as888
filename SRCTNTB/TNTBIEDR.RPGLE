000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200141219     h dftactgrp(*no)
000300141219     h bnddir('UBBNDDIR')
000400060427      *------------------------------------------------------------------------*
000500160418      * Gestione tabella "IED" = Forzatura instradamento da EEX a DPD          *
000600060427      *------------------------------------------------------------------------*
000700141219     ftntbe01l  uf a e           k disk    extfile(wlibfile1) usropn
000800090424     fazorg01l  if   e           k disk
000900160413     ftabel00f  if   e           k disk
001000160413     ftntbiedd  cf   e             workstn sfile(tbies01:s01nrr)
001100060428
001200060428      *------------------------------------------------------------------------*
001300060428      * riepilogo indicatori
001400060428      *------------------------------------------------------------------------*
001500160419      * 02 - proteggo campi video 01 in manutenezione
001600160419      * 04 - Proteggo campi video 01 in annullameno/ripristino
001700060508      * 20 - gestione subfile 01
001800060508      * 21 - gestione subfile 01
001900060508      * 22 - gestione subfile 01
002000060508      * 23 - gestione subfile 01
002100060428      * 28 - messaggio errore
002200160419      * 40 - Errore lna
002300160419      * 41 - errore nazione
002400060502
002500141219       // -?Costanti per la definizione delle schiere con i nomi?
002600141219       //  ?degli iSeries da elaborare e delle relative librerie?
002700141219     d c_NrSyst        c                   const(2)
002800141219     d c_NrLibr        c                   const(2)
002900160415     D digits          c                   '0123456789'
003000060502      *------------------------------------------------------------------------*
003100060502      * schiere
003200060502      *------------------------------------------------------------------------*
003300160419     d msg             s             78    dim(12) ctdata perrcd(1)
003400141219       // -?iSeries  &  Librerie con entrambi i file tabelle?
003500141219     d $iSystem        s                   like(currSysNetA)
003600141219     d                                     dim(c_NrSyst)
003700141219     d                                     ctdata   perrcd( 1)
003800141219     d $Libraries      s                   like(ds_Libr)
003900141219     d                                     dim(c_NrSyst)
004000141219     d                                     alt($iSystem)
004100160415     d SD19cod         s              3    inz
004200160415     d SD19tip         s              1    inz
004300160415     d SD19des         s             25    inz
004400160415     d tnsd19r         pr                  extpgm('TNSD19R')
004500160415     d   D19cod                            like(SD19cod)
004600160415     d   D19tip                            like(SD19tip)
004700160415     d   D19des                            like(SD19des)
004800060427
004900060428      *------------------------------------------------------------------------*
005000060428      * campi di work
005100060428      *------------------------------------------------------------------------*
005200090428     d data_eur        s               d   datfmt(*eur)
005300060428     d blanks          s                   like(d1descopz)
005400090427     d comand          s              1
005500090427     d opzio           s              2
005600090427     d ricez           s              1
005700060428     d len             s              5u 0
005800060505     d s01nrr          s              4  0
005900060428     d savrcdnbr       s                   like(c1rcdnbr)
006000160415     d wkey            s                   like(tblkey)
006100160415     d kcod            s                   like(tblcod)
006200160415     D ordina          S              1
006300160415     D comando         S              1
006400060428     d wrkeofs01       s              1
006500060428     d wrkcars01       s              1
006600060504     d $video          s             10
006700160415     d w0030           s              3  0
006800141219     d $Fine           s               n   inz(*off)
006900141222     d wDate           s              8  0 inz
007000060428
007100060428      *------------------------------------------------------------------------*
007200060428      * ds interne/esterne
007300060428      *------------------------------------------------------------------------*
007400090427     d og143         e ds
007500160415     d died          e ds
007600160415     d ds15          e ds
007700060502     d kpjba         e ds
007800060503     d tibs34ds      e ds                  inz
007900060428     d §azute        e ds                  extname(azute00f)
008000060428     d                                     dtaara
008100060428     d §datiute      e ds                  extname(ddatiute)
008200060428     d                                     dtaara
008300160415     d TIBS69DS      E DS                  INZ
008400160415     d DS_cnaco      E DS                  extname(CNACO00F)
008500160415     d DS_cnind      E DS                  extname(CNIND00F)
008600160415     d DS_cnclp      E DS                  extname(CNCLP00F)
008700160415     d DS_fncls      E DS                  extname(FNCLS00F)
008800141219       // -?Ridefinizione elenco librerie in elaborare le tabelle?
008900141219     d ds_Libr         ds                  inz
009000141219     d   $Libr                       10    dim(c_NrLibr) inz
009100010503
009200141219       // -?Nome del sistema?
009300141219     d currSysNeta     s              8a   inz
009400141219
009500141219       // -?Nome esteso Libreria/File dei 2 file tabella?
009600141219     d wLibFile1       s             21a   inz
009700141219     d w_iSystem       s              1  0 inz
009800141219     d w_SisInf        s              3  0 inz
009900160415
010000160415      //  ricerca alfabetica
010100160415     d  xpardut        s             30
010200160415     d  xparkut        s              1  0
010300160415     d  xparrag        s             48
010400160415     d  xparkcc        s              4  0
010500160415     d  xparsta        s              1  0
010600160415     d  xparflr        s             90
010700160415     d  xpardit        s              3
010800160415     d  xparnum        s              2  0
010900160415     d  xparkcm        s             80
011000160415     d  xparksm        s            140
011100160415     d  xparkdm        s             60
011200160415     d  xparesci       s              1
011300160415     d  xparerr        s              2
011400160415     d  xpariva        s             16
011500160415     d  xparcdf        s             16
011600141219
011700060428     d psds           sds
011800060428     d  pgmname          *proc
011900060428
012000090424      // ? PROTOTIPI ?
012100090424      /copy gaitrasrc/srcprotopr,tibs34r
012200160415      /copy gaitrasrc/srcprotopr,TRUL19R
012300160415      /copy gaitrasrc/srcprotopr,tibs69r
012400160415      /copy gaitrasrc/srcprotopr,xalfa3br
012500141219       // -?Reperimento NETA sistema AS/400 corrente?
012600141219      /copy gaitrasrc/srcProtoPR,UBRTVNETA
012700060428
012800060428      *------------------------------------------------------------------------*
012900060428      * costanti
013000160419      *------------------------------------------------------------------------*
013100010503
013200060428      *------------------------------------------------------------------------*
013300010423
013400060428      /free
013500060428
013600141219       if $fine=*off;
013700141219          exsr sr_c01;
013800141219       endif;
013900060428       exsr sr_uscita;
014000090424
014100060502       // ------------------------------------------------------------------------
014200060502       // gestione subfile
014300060502       // ------------------------------------------------------------------------
014400060428       begsr sr_c01;
014500060428
014600060502       // imposto variabili
014700060428        wrkcars01 = *on;
014800060428        $video = 'C01';
014900060428
015000060502       // inizio elaborazione subfile
015100060428         dou  $video <> 'C01';
015200090427
015300060502       // caricamento subfile
015400060428          exsr sr_cars01;
015500060428
015600060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
015700060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
015800060502       // l'ultimo tasto
015900060428          if c1csrrrn > 0;
016000060428           c1rcdnbr = c1csrrrn;
016100060428          else;
016200060428           c1rcdnbr = savrcdnbr;
016300060428          endif;
016400060428
016500060502       // se non so quale pagina visualizzare forzo pagina 1
016600060428          if c1rcdnbr < 1;
016700060428           eval c1rcdnbr = 1;
016800060428          endif;
016900060428
017000060502       // salvo il record number del subfile
017100060428          savrcdnbr = c1Rcdnbr;
017200060428
017300060502       // Emissione del subfile
017400160415          write tbiez01;
017500160413          exfmt tbiec01;
017600060428
017700060502       // controllo tasti funzionali del subfile
017800060428          select;
017900060428
018000060502       // F3=Fine
018100060428           when *inkc;
018200060502            $video = *blanks;
018300090427            ricez='C';
018400060428            exsr sr_uscita;
018500060428
018600060502       // F5=Refresh
018700060428           when *inke;
018800060428            wrkcars01 = *on;
018900060428
019000060502       // F10=Immissione
019100060428           when *inkj;
019200060502            $video = 'D01';
019300090427            comand='J';
019400060428            exsr sr_d01;
019500060428
019600060502       // in tutti gli altri casi
019700060428           other;
019800060502       // controllo ed elaborazione scelte su subfile
019900060428            exsr sr_gestsfl;
020000060428          endsl;
020100060428
020200060502       // fine elaborazione 'C01'
020300060428         enddo;
020400060428
020500060428        endsr;
020600060428
020700060502       // ------------------------------------------------------------------------
020800060502       // caricamento subfile
020900060502       // ------------------------------------------------------------------------
021000060428       begsr sr_cars01;
021100010430
021200060502       // se è stato richiesto il caricamento del subfile
021300060428        if wrkcars01 = *on;
021400060428
021500060502       // inizializzo il subfile
021600060505         s01nrr = 0;
021700060428         *in20 = *on;
021800160415         write tbiec01;
021900060428         *in20 = *off;
022000060428         *in21 = *off;
022100060428         *in22 = *off;
022200060428         *in23 = *off;
022300141222
022400141222         // -?Apertura file TNTBE00F del 1° S.I. (sede)?
022500141222         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
022600141222           w_SisInf = 1;
022700141222           exsr  sr_OpenFileTab;
022800141222         endif;
022900060428
023000060502       // imposto la chiave di posizionamento e lettura file
023100160413         tbecod = 'IED';
023200060428
023300090424       // posizionamento file
023400090424         setll tbecod tntbe01l;
023500060428
023600060502       // fino a che non è fine file
023700060428         dou %eof(tntbe01l);
023800060428
023900060502       // leggo file
024000060505          reade(n) tbecod tntbe01l;
024100060428
024200060502       // fine file esco
024300060428          if %eof(tntbe01l);
024400060428           leave;
024500060428          endif;
024600160418       // Scarto se record "descrittivo"
024700160418          if %check(digits:(%subst(tbeke1:1:3)))=0 ;
024800060428
024900060502       // scrivo subfile
025000060428           clear s1opzione;
025100060428           exsr sr_wtrs01;
025200160418
025300160418          endif ;
025400060428
025500060428         enddo;
025600090428
025700090428         clear opzio;
025800090428         clear comand;
025900060428
026000060502       // fine caricamento subfile
026100060428        endif;
026200060428
026300060502       // se scritto almeno un record
026400060505        if s01nrr > 0;
026500060502       // indicatore di sflend
026600060428         eval *in21 = *on;
026700060428        endif;
026800060428
026900060502       // se non scritto neanche un record
027000060505        if s01nrr = 0;
027100060502       // indicatore di sfldsp
027200060428         eval *in23 = *on;
027300060428        endif;
027400060428
027500060502       // disattivo opzione di caricamento subfile
027600060428        wrkcars01 = *off;
027700060428
027800060428       endsr;
027900060428
028000060502       // ------------------------------------------------------------------------
028100060502       // gestione videata
028200060502       // ------------------------------------------------------------------------
028300060428       begsr sr_d01;
028400060428
028500060504       // imposto il video a seconda della funzione richiesta
028600060428        exsr sr_setvideo;
028700060428
028800060502       // imposto variabile
028900060428        $video = 'D01';
029000060428
029100060502       // fino a che la variabile resta settata come 'D01'
029200060428        dou $video <> 'D01';
029300060428
029400060508         *in02 = *off;
029500060509         *in04 = *off;
029600060428
029700090424       // se immessa opzione di  'cancellazione/ripristino'
029800060508       // proteggo i campi del video
029900090424          if opzio = '04';
030000160419           *in04 = *on;
030100060508          endif;
030200160419          if opzio = '02';
030300160419           *in02 = *on;
030400160419          endif;
030500090424
030600060502       // emetto il video
030700160415          exfmt tbied01;
030800060428
030900060502       // reset indicatore generico di errore
031000060428         *in28 = *off;
031100160415         *in90 = *off;
031200060508
031300060508       // pulisco il campo messaggi
031400060508         clear vd1msg;
031500010430
031600060502       // controllo tasti funzionali del video
031700060428         select;
031800060428
031900060502       // F3=Fine
032000060428          when *inkc;
032100090427           ricez='C';
032200060502           $video = *blanks;
032300060505           unlock tntbe01l;
032400060428           exsr sr_uscita;
032500060428
032600060502       // F6=Conferma
032700060428          when *inkf;
032800090427          ricez='F';
032900060502       // controllo e decodifico i dati del video
033000060428           exsr sr_ctrd01;
033100060428
033200060504       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
033300160415           if not *in90;
033400141222           //   clear w1ftt;
033500141222           //   clear w1flt;
033600141219                exsr sr_UpdateAll;
033700141219            if not *in28;
033800141219             if comand = 'J';
033900141219              $video = 'D01';
034000141219              exsr sr_setvideo;
034100141219             else;
034200141219              $video = 'C01';
034300141219             endif;
034400141219            endif;
034500060428           endif;
034600060428
034700060502       // F12=Ritorno
034800060428          when *inkl;
034900060428           clear s1opzione;
035000090427           ricez='L';
035100060505           unlock tntbe01l;
035200090424       // se non è f12 da immissione non ricarico il subfile
035300090424           if comand = 'J';
035400060428            wrkcars01 = *on;
035500060428           else;
035600060428            wrkcars01 = *off;
035700060428           endif;
035800060428           $video = 'C01';
035900060428
036000060502       // Invio
036100060428          other;
036200160419           if not *in04;
036300060508            exsr sr_ctrd01;
036400060508           endif;
036500060428
036600060502         endsl;
036700060428
036800060502       // fine gestione 'D01'
036900060428        enddo;
037000060428
037100060428       endsr;
037200060428
037300060502       // ------------------------------------------------------------------------
037400060502       // imposto i dati a video
037500060502       // ------------------------------------------------------------------------
037600060428       begsr sr_setvideo;
037700060428
037800060502       // pulisco il formato video D01
037900060502        exsr sr_puld01;
038000060428
038100090424       // controllo se 'immissione' o altro
038200060428        select;
038300060428
038400060502       // F10=Immissione
038500090424         when comand = 'J';
038600160419          d1descopz = 'IMMISSIONE';
038700060428
038800060502       // Opzione "04"=cancellazione/ripristino
038900090424         when opzio = '04';
039000060428          exsr sr_imposta;
039100060502       // se richiesta 'cancellazione'
039200060502          if tbeatb = *blanks;
039300160419           d1descopz = 'ANNULLAMENTO';
039400060502          endif;
039500060502       // se richiesto 'ripristino'
039600060502          if tbeatb = 'A';
039700160419           d1descopz = 'RIPRISTINO';
039800060502          endif;
039900160415         when opzio = '02' ;
040000160419           *in02 = *on;
040100160419           d1descopz = 'MANUTENZIONE';
040200160419           exsr sr_imposta;
040300060502
040400060502       // Fine scelta
040500060502        endsl;
040600060502
040700060502       // centro la descrizione della funzione nel formato video
040800060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
040900060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
041000060502
041100060502       endsr;
041200060502
041300060502       // ------------------------------------------------------------------------
041400060502       // controllo video
041500060502       // ------------------------------------------------------------------------
041600060502       begsr sr_ctrd01;
041700060502
041800060502        *in28 = *off;
041900160415        *in90 = *off;
042000060502        *in40 = *off;
042100060502        *in41 = *off;
042200160418        *in42 = *off;
042300160418        *in43 = *off;
042400120907        clear d1dfil;
042500160415        clear d1dnar;
042600010430
042700160415       // LINEA DI ARRIVO
042800160415       // Linea di arrivo obbligatoria
042900120907        if d1tbeke1=*blanks or d1tbeke1=*zeros;
043000120907         vd1msg = msg(02);
043100060502         *in28 = *on;
043200160415         *in90 = *on;
043300060502         *in40 = *on;
043400060502         leavesr;
043500060502        endif;
043600160415        if %scan('?':d1tbeke1)>0;
043700160415           clear  SD19cod;
043800160415           clear  SD19tip;
043900160415           clear  SD19des;
044000160415           tnsd19r( SD19cod : SD19tip : SD19des );
044100160415           d1tbeke1  = SD19cod;
044200160415           d1dfil    = SD19des;
044300160415           *in90     = *on;
044400160415           leavesr;
044500160415        endif ;
044600160415       // deve contenere solo numeri
044700160418        if %check(digits:d1tbeke1)>0;
044800160415           *in90     = *on;
044900160415           *in28     = *on;
045000160415           *in40     = *on;
045100160415           vd1msg = msg(02);
045200160415           leavesr;
045300160415        endif;
045400160419        if d1tbeke1= '999'      ;
045500160419           d1dfil    = 'TUTTE' ;
045600160419        else;
045700160415           w0030 = %dec(d1tbeke1:3:0);
045800160415           chain w0030 azorg01l ;
045900160415           if not %found(azorg01l) or ((orgfag <>'F' and orgfag<> 'A')
046000160415                                        or orgfva<>' ');
046100160415              *in90     = *on;
046200160415              *in28     = *on;
046300160415              *in40     = *on;
046400160415              vd1msg = msg(02);
046500160415              leavesr;
046600160415           endif;
046700160415           d1dfil    = orgdes  ;
046800160415       // errore se non è una linea EEX
046900160415           og143=orgde3;
047000160415           if §ogntw<>'EEX';
047100160415              *in90     = *on;
047200160415              *in28     = *on;
047300160415              *in40     = *on;
047400160415              vd1msg = msg(01);
047500160415              leavesr;
047600160415           endif;
047700160415        endif  ;
047800160415       //NAZIONE
047900160418       // Se lna = '999' anche la nazione deve essere 999 e viceversa
048000160418           if (d1tbeke1='999' and d1nar<>'999') or
048100160418              (d1tbeke1<>'999' and d1nar='999');
048200160418              *in90     = *on;
048300160418              *in28     = *on;
048400160418              *in41     = *on;
048500160418              vd1msg = msg(03);
048600160418              leavesr;
048700160418           endif ;
048800160415       // Se immessa la controllo
048900160419        if d1nar='999';
049000160419                 d1dnar='TUTTE';
049100160419        endif ;
049200160418        if d1nar<>*blanks and d1nar<>'999';
049300160415       // immesso ?
049400160415              if %scan('?':d1nar)>0;
049500160415                 kcod='15'    ;
049600160415                 clear wkey    ;
049700160418                 TABEL_Ricerca (kcod:ordina:wkey:comando);
049800160419                 d1nar=%subst(wkey:1:3);
049900160415       // decodifico
050000160415                 clear §15des ;
050100160415                 chain (1:'15':wkey) tabel00f ;
050200160415                 if %found(tabel00f);
050300160415                    ds15=tbluni;
050400160415                 endif;
050500160415                 d1dnar=§15des;
050600160418                 *in90     = *on;
050700160415                 leavesr;
050800160415              endif;
050900160415              wkey=d1nar;
051000160415              chain (1 : '15' : wkey) tabel00f   ;
051100160415              if not %found(tabel00f) or tblflg<> *blanks   ;
051200160415                 *in90     = *on;
051300160415                 *in28     = *on;
051400160415                 *in41     = *on;
051500160415                 vd1msg = msg(04);
051600160415                 leavesr;
051700160415              endif;
051800160415              ds15=tbluni;
051900160415              d1dnar=§15des;
052000160418       // Deve essere una nazione servita sia da EEX che da DPD
052100160418              if §15EEX= *blanks;
052200160418                 *in90     = *on;
052300160418                 *in28     = *on;
052400160418                 *in41     = *on;
052500160418                 vd1msg = msg(09);
052600160418                 leavesr;
052700160418              endif;
052800160418              if §15lad= 0;
052900160418                 *in90     = *on;
053000160418                 *in28     = *on;
053100160418                 *in41     = *on;
053200160418                 vd1msg = msg(10);
053300160418                 leavesr;
053400160418              endif;
053500160415        endif;
053600160415       // CLIENTE MITTENTE
053700160418       // Ricerca Alfabetica
053800160418          if d1ccm=0 and d1dccm<>*blanks;
053900160418             exsr sr_ricalfa;
054000160418             *in90=*on;
054100160418             leavesr ;
054200160418          endif;
054300160419       // Cliente obbligatorio se lna e nazione 999
054400160419        if d1ccm=0 and d1nar='999' ;
054500160419                 *in90     = *on;
054600160419                 *in28     = *on;
054700160419                 *in42     = *on;
054800160419                 vd1msg = msg(12);
054900160419                 leavesr;
055000160419        endif;
055100160415        if d1ccm>0                         ;
055200160415       // Se immesso errore se non immessa la nazione
055300160418           if d1nar=*blanks ;
055400160415                 *in90     = *on;
055500160415                 *in28     = *on;
055600160415                 *in41     = *on;
055700160415                 vd1msg = msg(05);
055800160415                 leavesr;
055900160415           endif  ;
056000160415           clear DS_cnaco;
056100160415           clear DS_cnind;
056200160415           clear DS_cnclp;
056300160415           clear DS_fncls;
056400160415           clear d1dccm;
056500160415           I69kac=d1ccm  ;
056600160415           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
056700160415           if o69err=*blanks;
056800160415              d1dccm=acorag;
056900160415           else;
057000160415              *in42=*on;
057100160415              *in90=*on;
057200160415              *in28=*on  ;
057300160418              vd1msg=msg(6);
057400160415              leavesr;
057500160415           endif;
057600160415
057700160415        endif;
057800160415       // FLAG DI ABILITAZIONE
057900160415        if d1abi=*blanks;
058000160415           *in90 = *on;
058100160415           *in28 = *on;
058200160415           *in43 = *on;
058300160418              vd1msg=msg(8);
058400160415              leavesr;
058500160415        endif;
058600160418       // Non accetto la "S" se immessa solo la lna --> non ha senso
058700160418        if d1abi='S'  and d1nar=*blanks and d1ccm =0;
058800160418           *in90 = *on;
058900160418           *in28 = *on;
059000160418           *in43 = *on;
059100160418              vd1msg=msg(11);
059200160418              leavesr;
059300160418
059400160418        endif;
059500060502
059600090508
059700090508       // In immissione la chiave non deve essere già presente
059800090508        if comand = 'J';
059900160415         tbecod = 'IED';
060000090508         tbeke1 = d1tbeke1;
060100160418         clear tbeke2 ;
060200160418         if d1nar<>*blanks and d1ccm=0;
060300160418         tbeke2 = d1nar ;
060400160418         else ;
060500160418         if d1ccm>0       ;
060600160418            tbeke2 = d1nar + %editc(d1ccm:'X');
060700160418         endif;
060800160418         endif;
060900090508         chain(n) (tbecod:tbeke1:tbeke2) tntbe01l;
061000090508         if %found(tntbe01l);
061100160415            vd1msg = msg(07);
061200090508            *in28 = *on;
061300160418            *in90 = *on;
061400090508            *in40 = *on;
061500090508            leavesr;
061600090508         endif;
061700090508        endif;
061800060502
061900060502       endsr;
062000060504
062100060504       // ------------------------------------------------------------------------
062200060504       // gestione video dati di trasmissione
062300060504       // ------------------------------------------------------------------------
062400060504       begsr sr_w01;
062500060504
062600060504       // imposto i dati a video
062700060504        exsr sr_carw01;
062800060504
062900060505       // fino a che la variabile resta settata come 'W01'
063000060504        dou $video <> 'W01';
063100060504
063200060504       // reset indicatore generico di errore
063300060504         *in28 = *off;
063400060504
063500060504       // emetto il video
063600160415          exfmt tbiew01;
063700060504
063800060504       // controllo tasti funzionali del video
063900060504         select;
064000060504
064100060504       // F6=Conferma
064200060504          when *inkf;
064300060504       // controllo i dati del video
064400060504           exsr sr_ctrw01;
064500060504       // se non riscontrati errori aggiorno il record corrente
064600060504           if not *in28;
064700060504            exsr sr_aggiorna;
064800060504            if not *in28;
064900090424             if comand = 'J';
065000060504              $video = 'D01';
065100060504              exsr sr_setvideo;
065200060504             else;
065300060504              $video = 'C01';
065400060504             endif;
065500060504            endif;
065600060504           endif;
065700060504
065800060504       // F12=Ritorno
065900060504          when *inkl;
066000060504           $video = 'D01';
066100090427           clear ricez;
066200060504
066300060504       // Invio
066400060504          other;
066500060504           exsr sr_ctrw01;
066600060504
066700060504         endsl;
066800060504
066900060504       // fine gestione 'W01'
067000060504        enddo;
067100060504
067200060504       endsr;
067300060504
067400060504       // ------------------------------------------------------------------------
067500060504       // imposto i dati di trasmissione
067600060504       // ------------------------------------------------------------------------
067700060504       begsr sr_carw01;
067800060504
067900060504       // se pulisco i campi
068000060504         clear w1ftt;
068100060504         clear w1flt;
068200060504         clear w1ftr;
068300060504         clear w1dtr;
068400060504
068500060504       // se non immissione imposto i campi del file
068600090424        if comand <> 'J';
068700060504         w1ftt = tbeftt;
068800060504         w1flt = tbeflt;
068900060504         w1ftr = tbeftr;
069000060504       // imposto la data
069100060504         if tbedtr <> *zeros;
069200090428          data_eur=%date(tbedtr:*iso);
069300090428          w1dtr=%dec(data_eur);
069400060504         endif;
069500060504
069600060504        endif;
069700060504
069800060504       endsr;
069900060504
070000060504       // ------------------------------------------------------------------------
070100060504       // controllo i dati di trasmissione
070200060504       // ------------------------------------------------------------------------
070300060504       begsr sr_ctrw01;
070400060504
070500060504       endsr;
070600060502
070700141219       //--------------------------------------------------------------
070800141219       //?Aggiornam. tab. nei file di tutti i sistemi informativi?
070900141219       //--------------------------------------------------------------
071000141219       BEGSR  sr_UpdateAll;
071100141219
071200141219         // -?Ciclo di elaborazione per ogni sistema informativo?
071300141219         For  w_SisInf = 1  To  %elem($Libr);
071400141219
071500141219           // -?Apertura degli archivi?
071600141219           if  w_SisInf > 1;
071700141219             exsr  sr_OpenFileTab;
071800141219           endif;
071900141219
072000141219           exsr  sr_aggiorna;
072100141219
072200141219         EndFor;
072300160419
072400160419         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
072500160419           w_SisInf = 1;
072600160419           exsr  sr_OpenFileTab;
072700160419         endif;
072800141219
072900141219       ENDSR;
073000060502       // ------------------------------------------------------------------------
073100060502       // aggiorno tabella
073200060502       // ------------------------------------------------------------------------
073300060502       begsr sr_aggiorna;
073400060502
073500141222         // -?RI-aggancio record da aggiornare?
073600141222         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
073700141222         If  w_SisInf > 1;
073800160419            if d1nar<>*blanks and d1ccm=0;
073900160419               tbeke2 = d1nar ;
074000160419            else ;
074100160419               if d1ccm>0       ;
074200160419                  tbeke2 = d1nar + %editc(d1ccm:'X');
074300160419               endif;
074400160419            endif;
074500160415           chain  ('IED':d1tbeke1:tbeke2) tntbe01l;
074600141222         EndIf;
074700060502       // imposto campi del file
074800141222       // tbeke1 = d1tbeke1;
074900141222       // tbeke2 = d1tbeke2;
075000060502
075100060502
075200141222       // tbeftt = w1ftt;
075300141222       // tbeflt = w1flt;
075400141222       // clear tbeftr;
075500141222       // clear tbedtr;
075600060502
075700060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
075800060502        select;
075900060502
076000160419       // F10=immissione o  opzione 02=Manutenzione
076100160419         when comand = 'J' or opzio = '02';
076200141222             exsr  sr_RieRecTBE;
076300141222             clear tbeatb;
076400141222             // -?Aggiornamento record?
076500141222             if  %found(TNTBE01L);
076600141222               //_____________
076700141222               UPDATE TNTBE000;
076800141222               //¯¯¯¯¯¯¯¯¯¯¯¯¯
076900141222             else;
077000141222               //_____________
077100141222               WRITE  TNTBE000;
077200141222               //¯¯¯¯¯¯¯¯¯¯¯¯¯
077300141222             endif;
077400060502
077500060502       // Opzione "04"=cancellazione/ripristino
077600090424         when opzio = '04';
077700141222            select;
077800141222       // annullamento
077900141222             when tbeatb = *blanks;
078000141222             if  %found(TNTBE01L)   and   TBEatb <> 'A';
078100141222              tbeatb = 'A';
078200160419       //      TBEftt = 'S';
078300160419       //      clear  TBEftr;
078400141222               //_______________
078500141222               UPDATE  TNTBE000;
078600141222               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078700141222             endif;
078800141222       // ripristino
078900141222             when tbeatb = 'A';
079000141222              exsr  sr_RieRecTBE;
079100141222              select;
079200141222               when  %found(TNTBE01L)   and   TBEatb <> *blank;
079300141222                 clear  TBEatb;
079400141222                 //_______________
079500141222                 UPDATE  TNTBE000;
079600141222                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079700141222               // -?Record appena cancellato fisicamente?
079800141222               when  NOT %found(TNTBE01L);
079900141222                 clear  TBEatb;
080000141222                 //_______________
080100141222                 WRITE   TNTBE000;
080200141222                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080300141222              endsl;
080400141222            endsl;
080500010430
080600060502        endsl;
080700060502
080800060502       endsr;
080900141222       //--------------------------------------------------------------
081000160419       //?Caricamento dati in tab. "IED".                              ?
081100141222       //--------------------------------------------------------------
081200141222       BEGSR  sr_RieRecTBE;
081300141222
081400141222         // -?Impostazione campi chiave se inserimento?
081500141222         if  NOT %found(TNTBE01L);
081600141222           clear TNTBE000;
081700160415           TBEcod ='IED'    ;
081800141222           TBEke1 = d1TBEke1;
081900160419           clear tbeke2 ;
082000160419           if d1nar<>*blanks and d1ccm=0;
082100160419              tbeke2 = d1nar ;
082200160419           else ;
082300160419              if d1ccm>0       ;
082400160419                 tbeke2 = d1nar + %editc(d1ccm:'X');
082500160419              endif;
082600160419           endif;
082700141222           clear tbelin;
082800141222           clear TBEsif;
082900141222         endif;
083000141222
083100141222         // -?Impostazione dati tabella da aggiornare?
083200160419         clear died ;
083300160419         §iedabi=d1abi;
083400160419         tbeuni=died ;
083500141222
083600141222         // -?Impostazione altri campi del record?
083700141222         clear TBEapl ;
083800141222         //clear  TBEatb;       //Sarà impostato caso per caso...?
083900141222         //TBEftt = W1ftt;
084000141222         TBEftt = 'S';
084100141222         clear  TBEflt;
084200141222         if  w_SisInf = 1;
084300141222           TBEftr = 'T';
084400141222         else;
084500141222           TBEftr = 'R';
084600141222         endif;
084700141222         TBEdtr = wDate;
084800141222
084900141222       ENDSR;
085000060502
085100060502       // ------------------------------------------------------------------------
085200060502       // pulizia video
085300060502       // ------------------------------------------------------------------------
085400060502       begsr sr_puld01;
085500060502
085600060504        clear d1descopz;
085700060504        clear d1tbeke1;
085800160415        clear d1dfil  ;
085900160415        clear d1nar ;
086000160415        clear d1dnar;
086100160415        clear d1ccm ;
086200160415        clear d1dccm;
086300160419        d1abi = 'N';
086400060504
086500090115        *in06 = *off;
086600090115        *in28 = *off;
086700060504        *in40 = *off;
086800060504        *in41 = *off;
086900160415        *in42 = *off;
087000160415        *in43 = *off;
087100060502
087200060502       endsr;
087300060502
087400060502       // ------------------------------------------------------------------------
087500060502       // imposto i dati a video
087600060502       // ------------------------------------------------------------------------
087700060502       begsr sr_imposta;
087800090115
087900090115       *in06 =*off    ;
088000060502
088100060502       // recupero i dati dal file
088200160415        tbeke2 = s1cnaz + s1cccm ;
088300160415        chain ('IED':s1tbeke1:tbeke2) tntbe01l;
088400060502
088500160419        if %found(tntbe01l);
088600060502       // imposto i campi a video
088700160415        d1nar  = %subst(tbeke2:1:3);
088800160418        if %check(digits:(%subst(tbeke2:4:7)))=0 ;
088900160418           d1ccm  = %dec(%subst(tbeke2:4:7):7:0)   ;
089000160418        Endif;
089100060502        d1tbeke1 = tbeke1;
089200160418        died=tbeuni;
089300160418        d1abi=§iedabi;
089400090115        if tbeatb<>' '   ;
089500090115        *in06=*on    ;
089600090115        endif   ;
089700160419        endif;
089800060502
089900060502       endsr;
090000060502
090100060502       // ------------------------------------------------------------------------
090200060502       // gestione del subfile
090300060502       // ------------------------------------------------------------------------
090400060502       begsr sr_gestsfl;
090500060502
090600060502       // set flag
090700060502        wrkeofs01 = *off;
090800060502
090900060502       // inizio lettura subfile
091000060502        dow wrkeofs01 = *off and *in21;
091100160415         readc tbies01;
091200060502       // se fine subfile
091300060502         if %eof;
091400060502          wrkcars01 = *on;
091500060502          leave;
091600060502         endif;
091700060502       // se è stata immessa un'opzione
091800060502         if s1opzione <> *zeros;
091900060502       // controllo ed elaborazione opzione immessa
092000060502          select;
092100060502       // opzione "04"=annullo/ripristino
092200090428           when s1opzione = 4;
092300090424            opzio = '04';
092400160419       // opzione "02"=Manutenzione
092500160419           when s1opzione = 2;
092600160419            opzio = '02';
092700160419            if s1tbeatb='Ann';
092800160419               clear opzio;
092900160419               clear s1opzione;
093000160419               update tbies01;
093100160419            endif;
093200060502          endsl;
093300010503
093400060502       // se immessa almeno un opzione valida
093500090424          if  opzio <> *blanks;
093600060502       // gstione del formato video
093700060504           exsr sr_d01;
093800060502       // se la gestione si è chiusa con ...
093900060504           select;
094000060502       // F6=Conferma
094100090427            when ricez = 'F';
094200060504             clear s1opzione;
094300160419             clear opzio    ;
094400060504             wrkcars01 = *on;
094500060502       // F12=Ritorno
094600090427            when ricez = 'L';
094700060504             clear s1opzione;
094800160419             clear opzio    ;
094900060504             wrkeofs01 = *on;
095000060502       // altrimenti
095100060504            other;
095200060504             *in22 = *on;
095300060504           endsl;
095400060502
095500160415           update tbies01;
095600060504           *in22 = *off;
095700060504       // fine opzione
095800060504          endif;
095900060502       // fine opzione
096000060502         endif;
096100060502
096200060502        enddo;
096300060502
096400060502       endsr;
096500060502
096600060502
096700060502       // ------------------------------------------------------------------------
096800060502       // scrivo subfile
096900060502       // ------------------------------------------------------------------------
097000060502       begsr sr_wtrs01;
097100060502
097200060502       // se non raggiunto limite massimo di caricamento
097300060505        if s01nrr < 9999;
097400060502       // imposto campi di subfile
097500060502         exsr sr_sets01;
097600060502       // imposto numeratore righe e numero relativo di record
097700060505         s01nrr = s01nrr + 1;
097800060502       // scrivo subfile
097900160413         write tbies01;
098000060502        endif;
098100060502
098200060502       endsr;
098300060502
098400060502       // ------------------------------------------------------------------------
098500060502       // imposto campi del subfile
098600060502       // ------------------------------------------------------------------------
098700060502       begsr sr_sets01;
098800060502
098900060502        s1tbeke1 = tbeke1;
099000090424        clear s1dfil;
099100160419        if s1tbeke1='999'  ;
099200160419              s1dfil='TUTTE';
099300160419        else;
099400160419           chain (%int(s1tbeke1)) azorg01l;
099500160419           if %found(azorg01l);
099600160419              s1dfil=orgdes;
099700160419           endif;
099800160419        endif;
099900160413        clear s1cnaz;
100000160413        clear s1dnaz;
100100160413        clear s1cccm;
100200160413        clear s1dccm;
100300160413        if tbeke2 <> *blanks;
100400160415           s1cnaz=%subst(tbeke2:1:3);
100500160419           if s1cnaz  = '999';
100600160419                s1dnaz='TUTTE';
100700160419           else;
100800160419              wkey=s1cnaz;
100900160419              chain (1:'15':wkey)tabel00f;
101000160419              if %found(tabel00f);
101100160419                ds15=tbluni;
101200160419                s1dnaz=§15des;
101300160419              endif;
101400160419           endif;
101500160413           if %subst(tbeke2:4:7)<>*blanks;
101600160415              s1cccm=%subst(tbeke2:4:7);
101700160419       // Decodifica cliente mittente
101800160419              clear DS_cnaco;
101900160419              clear DS_cnind;
102000160419              clear DS_cnclp;
102100160419              clear DS_fncls;
102200160419              I69kac=%dec(s1cccm:7:0) ;
102300160419              tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
102400160419              if o69err=*blanks;
102500160419                 s1dccm=acorag;
102600160419              endif;
102700160413           endif;
102800160413        endif;
102900160418        died=tbeuni;
103000160413        s1cabi=§IEDABI  ;
103100160419        if tbeatb='A';
103200160419           s1tbeatb = 'Ann';
103300160419        else;
103400160419           clear s1tbeatb;
103500160419        endif;
103600060502
103700060502       endsr;
103800160415       //--------------------------------------------------------------
103900160415       //? Ricerca alfabetica cliente
104000160415       //--------------------------------------------------------------
104100160415       BEGSR sr_ricalfa;
104200160415           clear xparsta  ;
104300160415           xparkcc=dutkci ;
104400160415           xparkut= 1     ;
104500160415           xpardut=rsut   ;
104600160419           xparrag=d1dccm ;
104700160415           clear xparflr ;
104800160415           clear xparkcm  ;
104900160415           clear xparksm  ;
105000160415           clear xparkdm  ;
105100160415           xparnum=1      ;
105200160415           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
105300160415                           :xparflr:xpardit:xparnum:xparkcm:xparksm
105400160415                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
105500160415 2          if xparsta<>-1  ;
105600160418                  d1dccm=xparrag   ;
105700160418                  d1ccm =%dec(xparksm:7:0)      ;
105800160415               else;
105900160415 2          endif            ;
106000160415       endsr ;
106100060505
106200060502       // ------------------------------------------------------------------------
106300060502       // routine iniziale
106400060502       // ------------------------------------------------------------------------
106500060428         begsr *inzsr;
106600060428
106700060428      /end-free
106800060428
106900060428     c     *entry        plist
107000060428     c                   parm                    kpjba
107100060428
107200060428      /free
107300141219         // -?Verifica del sistema AS/400 corrente?
107400141219         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
107500141219           $Fine = *on;
107600141219           leavesr;
107700141219         endif;
107800141219
107900141219         // -?Impostazione elenco librerie in cui gestire le tabelle?
108000141219         //  ?(a seconda del sistema in cui si sta lavorando)?
108100141219         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
108200141219         if  w_iSystem = *zero;
108300141219           $Fine = *on;
108400141219           leavesr;
108500141219         endif;
108600141219         // eseguo apertura a tntbe puntando all'ambiente di sede
108700141219         w_SisInf = 1;
108800141219         // -?Apertura file tabelle                                 ?
108900141219         exsr sr_OpenFileTab;
109000141219
109100060428         in(e) §azute;
109200060428         if not %error;
109300060428          in(e) §datiute;
109400060428         endif;
109500060502         if %error or rsut = *blanks;
109600060428          tibs34r(tibs34ds);
109700060428          in §azute;
109800060428          in §datiute;
109900060428         endif;
110000141222         // -?Reperimento data odierna?
110100141222         wDate = %int( %subst( %char( %dec( %timestamp() ) )
110200141222                               : 1 : 8 ) );
110300060428
110400060428         endsr;
110500141219       //--------------------------------------------------------------
110600141219       //?Apertura dei files tabelle nel sistema informativo impostato.?
110700141219       //--------------------------------------------------------------
110800141219       BEGSR  sr_OpenFileTab;
110900141219
111000141219         // -?Chiusura (eventuale) archivi?
111100141219         if  %open(Tntbe01l);
111200141219           close  Tntbe01l;
111300141219         endif;
111400141219
111500141219         // -?Apertura archivi?
111600141219         ds_Libr  = $Libraries(w_iSystem);
111700141219         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
111800141219         open  TNTBE01L;
111900141219
112000141219       ENDSR;
112100060502
112200060502       // ------------------------------------------------------------------------
112300060502       // uscita dal programma
112400060502       // ------------------------------------------------------------------------
112500060502         begsr sr_uscita;
112600060502
112700060502          *inlr = *on;
112800060502          return;
112900060502
113000060502         endsr;
113100060502
113200060502      /end-free
113300060502
113400060502** -MSG-                                                                     *
113500160415La Linea immessa non è un network EEX                                          01
113600160415Linea di Arrivo mancante o errata                                              02
113700160418Se Linea di arr. uguale a '999' la naz. deve essere uguale a '999' e viceversa 03
113800160415Nazione errata
113900160418Se immesso il codice cliente è obbligatorio inserire anche la Nazione          05
114000160415Codice inesistente                                                             06
114100160419Record già presente in questa tabella                                          07
114200160415Immettere S=Si o N=No per attivare o non attivare l'instradamento da EEX a DPD 08
114300160418Nazione non servita da network EEX                                             90
114400160418Nazione non servita da network DPD                                             10
114500160418L'instradam.forzato EEX->DPD per SOLA Linea di Arr. è automatico: non immettere11
114600160419Per il blocco totale il Cliente è obbligatorio                                 12
114700141222** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
114800141222SETRAS  GAITRAGRU FILTRAGRU
114900141222AS888   GAITRAGRPSFILTRAGRPF
