000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300150513      * Gestione file orari massimi di fiiliale                                *
000400060427      *------------------------------------------------------------------------*
000500151023     fwaoma11l  uf a e           k disk
000600141120     fazorg01l  if   e           k disk
000700141120     ftntls1d   cf   e             workstn sfile(tls1s01:s01nrr)
000800060428
000900060428      *------------------------------------------------------------------------*
001000060428      * riepilogo indicatori
001100060428      *------------------------------------------------------------------------*
001200060428      * 01 - ON ricerca -- OFF manutenzione
001300060508      * 02 - proteggo campo causale
001400060508      * 03 - proteggo campi video 01
001500060509      * 04 - visualizzazione
001600141120      * 05 -
001700060508      * 20 - gestione subfile 01
001800060508      * 21 - gestione subfile 01
001900060508      * 22 - gestione subfile 01
002000060508      * 23 - gestione subfile 01
002100141120      * 24 -
002200141120      * 25 -
002300060428      * 28 - messaggio errore
002400060502      * 40 - causale
002500060502      * 41 - descrizione
002600060505      * 47 - descrizione subfile
002700060502
002800060502      *------------------------------------------------------------------------*
002900060502      * schiere
003000060502      *------------------------------------------------------------------------*
003100151026     d msg             s             78    dim(10) ctdata perrcd(1)
003200060427
003300060428      *------------------------------------------------------------------------*
003400060428      * campi di work
003500060428      *------------------------------------------------------------------------*
003600060428     d blanks          s                   like(d1descopz)
003700141120     d comand          s              1
003800141120     d opzio           s              2
003900141120     d ricez           s              1
004000060428     d len             s              5u 0
004100060505     d s01nrr          s              4  0
004200060428     d savopzione      s                   like(s1opzione)
004300060428     d savrcdnbr       s                   like(c1rcdnbr)
004400060428     d wrkeofs01       s              1
004500060428     d wrkcars01       s              1
004600060504     d $video          s             10
004700141120     d Dataeur         s               d   datfmt(*eur)
004800141120     d Dataisocor      s               d   datfmt(*iso)
004900141120     d Dataiso         s               d   datfmt(*iso)
005000141121      // ricerca filiale su organigramma
005100141121     d   pInFIL        S              3
005200141121     d   pInFAG        S              1
005300141121     d   pInDES        S             25
005400141121     d   pInDIT        S              3
005500060428
005600060428      *------------------------------------------------------------------------*
005700060428      * ds interne/esterne
005800060428      *------------------------------------------------------------------------*
005900060428
006000060502     d kpjba         e ds
006100060503     d tibs34ds      e ds                  inz
006200060428     d §azute        e ds                  extname(azute00f)
006300060428     d                                     dtaara
006400060428     d §datiute      e ds                  extname(ddatiute)
006500060428     d                                     dtaara
006600141121       // -?File AZORG01L?
006700141121     d k01azorg01    e ds                  extname(AZORG01L : *key)
006800141121     d                                     prefix(k_)   inz
006900141121     d trul06ds      e ds
007000141121     D  LIN                    1     90    dim(30)
007100151026     d fnlv55ds      e ds
007200010503
007300060428     d psds           sds
007400060428     d  pgmname          *proc
007500141121
007600060428      *------------------------------------------------------------------------*
007700060428      * pgm richiamati
007800060428      *------------------------------------------------------------------------*
007900060428     d tibs34r         pr                  extpgm('TIBS34R')
008000060502     d  tibs34ds                           likeds(tibs34ds)
008100060504
008200060428
008300060428      *------------------------------------------------------------------------*
008400060428      * costanti
008500060428      *------------------------------------------------------------------------*
008600060428     d errore          c                   '1'
008700060428     d eseguito        c                   '0'
008800141121       // -?Costante per controllo "caratteri solo numerici"?
008900141121     d c_Digits        c                   const('0123456789')
009000010503
009100141121      /copy gaitrasrc/srcprotopr,tnsd24r
009200141121      /copy gaitrasrc/srcprotopr,trul06r
009300151026      /copy gaitrasrc/srcprotopr,fnlv55r
009400060428      *------------------------------------------------------------------------*
009500010423
009600060428      /free
009700060428
009800141120       *in01 =*off;
009900141120       //exsr sr_parm;
010000060428       exsr sr_c01;
010100060428       exsr sr_uscita;
010200010423
010300060502       // ------------------------------------------------------------------------
010400060502       // elaborazione parametri ricevuti
010500060502       // ------------------------------------------------------------------------
010600141120       //begsr sr_parm;
010700060428
010800141120       // tntb49dS = kpjbu;
010900060428
011000141120       // Select;
011100060502       // ricerca e scleta
011200141120       //  when b49Fun = '1';
011300141120       //   *in01 = *on;
011400060502       // manutenzione
011500141120       //  when  b49Fun = *blanks;
011600141120       // *in01 = *off;
011700060502       // altrimenti
011800141120       //  other;
011900141120       //   b49Esito = errore;
012000141120       //    exsr sr_uscita;
012100141120       //  endsl;
012200060428
012300141120       //endsr;
012400010430
012500060502       // ------------------------------------------------------------------------
012600060502       // gestione subfile
012700060502       // ------------------------------------------------------------------------
012800060428       begsr sr_c01;
012900060428
013000060502       // imposto variabili
013100060428        wrkcars01 = *on;
013200060428        $video = 'C01';
013300060428
013400060502       // inizio elaborazione subfile
013500060428         dou  $video <> 'C01';
013600060428
013700060502       // caricamento subfile
013800060428          exsr sr_cars01;
013900060428
014000060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
014100060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
014200060502       // l'ultimo tasto
014300060428          if c1csrrrn > 0;
014400060428           c1rcdnbr = c1csrrrn;
014500060428          else;
014600060428           c1rcdnbr = savrcdnbr;
014700060428          endif;
014800060428
014900060502       // se non so quale pagina visualizzare forzo pagina 1
015000060428          if c1rcdnbr < 1;
015100060428           eval c1rcdnbr = 1;
015200060428          endif;
015300060428
015400060502       // salvo il record number del subfile
015500060428          savrcdnbr = c1Rcdnbr;
015600060428
015700060502       // Emissione del subfile
015800141120          write tls1p01;
015900141120          exfmt tls1c01;
016000060428
016100060502       // controllo tasti funzionali del subfile
016200060428          select;
016300060428
016400060502       // F3=Fine
016500060428           when *inkc;
016600060502            $video = *blanks;
016700141120       //     b49ricez = 'C';
016800060428            exsr sr_uscita;
016900060428
017000060502       // F5=Refresh
017100060428           when *inke;
017200060428            wrkcars01 = *on;
017300060428
017400060502       // F10=Immissione
017500060428           when *inkj;
017600060502            $video = 'D01';
017700141120       //   clear tntb49ds;
017800141120               comand = 'J';
017900141120               clear opzio;
018000060428            exsr sr_d01;
018100060428
018200060502       // F13=Ripetizione
018300141120       //  when *inkm;
018400141120       //   exsr sr_ripetiopz;
018500060428
018600060502       // in tutti gli altri casi
018700060428           other;
018800060502       // controllo ed elaborazione scelte su subfile
018900060428            exsr sr_gestsfl;
019000060428          endsl;
019100060428
019200060502       // fine elaborazione 'C01'
019300060428         enddo;
019400060428
019500060428        endsr;
019600060428
019700060502       // ------------------------------------------------------------------------
019800060502       // caricamento subfile
019900060502       // ------------------------------------------------------------------------
020000060428       begsr sr_cars01;
020100010430
020200060502       // se è stato richiesto il caricamento del subfile
020300060428        if wrkcars01 = *on;
020400060428
020500060502       // inizializzo il subfile
020600060505         s01nrr = 0;
020700060428         *in20 = *on;
020800141120         write tls1c01;
020900060428         *in20 = *off;
021000060428         *in21 = *off;
021100060428         *in22 = *off;
021200060428         *in23 = *off;
021300060428
021400060428
021500060502       // posizionamento file
021600141120         exsr sr_setll     ;
021700060428
021800060502       // fino a che non è fine file
021900151026         dou %eof(waoma11l);
022000060428
022100060502       // leggo file
022200151026          read(n) waoma11l;
022300060428
022400060502       // fine file esco
022500151026          if %eof(waoma11l);
022600060428           leave;
022700060428          endif;
022800060428
022900060502       // scrivo subfile
023000060428           clear s1opzione;
023100060428           exsr sr_wtrs01;
023200060428
023300060428         enddo;
023400060428
023500060502       // fine caricamento subfile
023600060428        endif;
023700060428
023800060502       // se scritto almeno un record
023900060505        if s01nrr > 0;
024000060502       // indicatore di sflend
024100060428         eval *in21 = *on;
024200060428        endif;
024300060428
024400060502       // se non scritto neanche un record
024500060505        if s01nrr = 0;
024600060502       // indicatore di sfldsp
024700060428         eval *in23 = *on;
024800060428        endif;
024900060428
025000060502       // disattivo opzione di caricamento subfile
025100060428        wrkcars01 = *off;
025200060428
025300060428       endsr;
025400060428
025500060502       // ------------------------------------------------------------------------
025600060502       // gestione videata
025700060502       // ------------------------------------------------------------------------
025800060428       begsr sr_d01;
025900060428
026000060504       // imposto il video a seconda della funzione richiesta
026100060428        exsr sr_setvideo;
026200060428
026300060502       // imposto variabile
026400060428        $video = 'D01';
026500060428
026600060502       // fino a che la variabile resta settata come 'D01'
026700060428        dou $video <> 'D01';
026800060428
026900060508         *in02 = *off;
027000060508         *in03 = *off;
027100060509         *in04 = *off;
027200060428
027300060502       // se immessa opzione di 'scelta'
027400141120         if opzio = '01';
027500141120       //   b49ke1 = s1tbeke1;
027600141120       //   b49des = s1tbeuni;
027700060428          exsr sr_uscita;
027800060428
027900060508       // se immessa un'altra opzione
028000060508         else;
028100060508       // se non è immissione/copia proteggo il campo della causale
028200141120          if comand <> 'J' and opzio <> '03';
028300060428           *in02 = *on;
028400060428          endif;
028500141121       // se immessa opzione di 'visualizzazione' 'cancellazione'
028600060508       // proteggo i campi del video
028700141120          if opzio = '04' or opzio = '05';
028800060508           *in03 = *on;
028900060508          endif;
029000060509       // se immessa opzione di 'visualizzazione'
029100060509       // non attivo F6
029200141120          if opzio = '05';
029300060509           *in04 = *on;
029400060509          endif;
029500060502       // emetto il video
029600141120          exfmt tls1d01;
029700060428         endif;
029800060428
029900141121       // reset indicatore
030000141121         *in29 = *off;
030100060508
030200060508       // pulisco il campo messaggi
030300060508         clear vd1msg;
030400010430
030500060502       // controllo tasti funzionali del video
030600060428         select;
030700060428
030800060502       // F3=Fine
030900060428          when *inkc;
031000141120           ricez = 'C';
031100060502           $video = *blanks;
031200151026           unlock waoma11l;
031300060428           exsr sr_uscita;
031400060428
031500060502       // F6=Conferma
031600060428          when *inkf;
031700141120           ricez = 'F';
031800060502       // controllo e decodifico i dati del video
031900060428           exsr sr_ctrd01;
032000060428
032100141121       // se non rdevo riemettere la videata aggiorno
032200141121           if not *in90;
032300141120              exsr sr_aggiorna;
032400141121              if comand='J';
032500141121                 $video = 'D01';
032600141121                 exsr sr_setvideo;
032700141121              else;
032800141121                 $video = 'C01';
032900141121              endif;
033000060428           endif;
033100060428
033200060502       // F8=Record successivo
033300141120       // when  *inkh;
033400141120       //  clear s1opzione;
033500141120       //  wrkcars01 = *off;
033600141120       //  $video = 'C01';
033700141120       //  b49ricez = 'H';
033800060505
033900060505       // F9=Traduzione
034000141120       // when  *inki;
034100141120       //  *in05 = *on;
034200141120       //  exsr sr_lingua;
034300141120       //  *in05 = *off;
034400060428
034500060502       // F12=Ritorno
034600060428          when *inkl;
034700060428           clear s1opzione;
034800141120           ricez = 'L';
034900151026           unlock waoma11l;
035000060508       // se non è f12 da immissione/copia non ricarico il subfile
035100141120           if comand = 'J' or opzio = '03';
035200060428            wrkcars01 = *on;
035300060428           else;
035400060428            wrkcars01 = *off;
035500060428           endif;
035600060428           $video = 'C01';
035700060428
035800060502       // Invio
035900060428          other;
036000060508           if not *in03;
036100060508            exsr sr_ctrd01;
036200060508           endif;
036300060428
036400060502         endsl;
036500060428
036600060502       // fine gestione 'D01'
036700060428        enddo;
036800060428
036900060428       endsr;
037000060428
037100060502       // ------------------------------------------------------------------------
037200060502       // imposto i dati a video
037300060502       // ------------------------------------------------------------------------
037400060428       begsr sr_setvideo;
037500060428
037600060502       // pulisco il formato video D01
037700060502        exsr sr_puld01;
037800060428
037900060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
038000060428        select;
038100060428
038200060502       // F10=Immissione
038300141120         when comand = 'J';
038400060428          d1descopz = 'Immissione';
038500060428
038600060502       // Opzione "02"=modifica
038700141120         when opzio = '02';
038800060428          d1descopz = 'Modifica';
038900060428          exsr sr_imposta;
039000060428
039100060502       // Opzione "03"=copia
039200141120         when opzio = '03';
039300060428          d1descopz = 'Copia';
039400060428          exsr sr_imposta;
039500060428
039600141121       // Opzione "04"=cancellazione
039700141120         when opzio = '04';
039800060428          exsr sr_imposta;
039900060502           d1descopz = 'Annullamento';
040000060502
040100060502       // Opzione "05"=visualizzazione
040200141120         when Opzio ='05';
040300060502          d1descopz = 'Visualizzazione';
040400060502          exsr sr_imposta;
040500060502
040600060502       // Fine scelta
040700060502        endsl;
040800060502
040900060502       // centro la descrizione della funzione nel formato video
041000060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
041100060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
041200060502
041300060502       endsr;
041400060502
041500060502       // ------------------------------------------------------------------------
041600060502       // controllo video
041700060502       // ------------------------------------------------------------------------
041800060502       begsr sr_ctrd01;
041900060502
042000141121        *in29 = *off;
042100141121        *in90 = *off;
042200060502        *in40 = *off;
042300060502        *in41 = *off;
042400141121        *in42 = *off;
042500150513        *in43 = *off;
042600151026        *in44 = *off;
042700151026        *in45 = *off;
042800151026        *in46 = *off;
042900151026        *in47 = *off;
043000151026        *in48 = *off;
043100010430
043200141121       // TERMINAL DI PARTENZA
043300141121        clear d1tfp_d;
043400141121       // Obbligatorio
043500141121        if d1tfp  = *blanks;
043600151026        d1tfp='000'  ;
043700151026        endif  ;
043800141121       // Ricerca su organigramma
043900141121          if %scan('?':d1tfp)>0;
044000141121           clear pinfil   ;
044100141121           clear pinfag   ;
044200141121           clear pindes   ;
044300141121           clear pindit   ;
044400141121           tnsd24r(pinfil:pinfag:pindes:pindit)   ;
044500141121           if pinfil > *zeros;
044600141121              d1tfp=pinfil;
044700141121              d1tfp_d=pindes;
044800141121           else;
044900141121              clear d1tfp ;
045000141121              clear d1tfp_d ;
045100141121           endif;
045200141121           *in90=*on;
045300141121           *in40=*on;
045400141121           leavesr;
045500141121          endif;
045600141121       // Deve essere numerico
045700141121           if    %check( c_Digits : d1tfp  ) > *zero;
045800141121             Vd1msg = Msg(01);
045900141121             *in40  = *on;
046000141121             *in90  = *on;
046100141121             *in29  = *on;
046200141121             leavesr;
046300141121           endif;
046400141121        // controlla esistenza del codice
046500141121           if d1tfp <>*zeros;
046600141121              k_ORGfil = %int( d1tfp );
046700141121              chain  %kds( k01azorg01 )  AZORG;
046800141121
046900141121              if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
047000141121                 Vd1msg = Msg(01);
047100141121                 *in40  = *on;
047200141121                 *in90  = *on;
047300141121                 *in29  = *on;
047400141121                 leavesr;
047500141121              endif;
047600141121
047700141121              d1tfp_d = ORGdes;
047800141121       // Deve essere un terminal di partenza
047900141121          clear trul06ds;
048000141121          d06cod='£1';
048100141121          d06key=d1tfp;
048200141121          d06tla='L';
048300141121          kpjbu=trul06ds;
048400141121          trul06r(kpjba);
048500141121          trul06ds=kpjbu;
048600141121          if d06err<>' ';
048700141121             *in90=*on;
048800141121             *in29=*on;
048900141121             eval vd1msg=msg(01);
049000141121             leavesr;
049100141121          endif;
049200141121           else;
049300141121              d1tfp_d = 'STANDARD';
049400141121           endif;
049500141121
049600141121       // FILIALE DI ARRIVO
049700141121        clear d1lna_d;
049800141121       // Obbligatorio
049900141121        if d1lna  = *blanks;
050000141121         vd1msg = msg(02);
050100141121         *in29 = *on;
050200141121         *in90 = *on;
050300141121         *in41 = *on;
050400141121         leavesr;
050500141121        endif;
050600141121       // Ricerca su organigramma
050700141121          if %scan('?':d1lna)>0;
050800141121           clear pinfil   ;
050900141121           clear pinfag   ;
051000141121           clear pindes   ;
051100141121           clear pindit   ;
051200141121           tnsd24r(pinfil:pinfag:pindes:pindit)   ;
051300141121           if pinfil > *zeros;
051400141121              d1lna=pinfil;
051500141121              d1lna_d=pindes;
051600141121           else;
051700141121              clear d1lna ;
051800141121              clear d1lna_d ;
051900141121           endif;
052000141121           *in90=*on;
052100141121           *in41=*on;
052200141121           leavesr;
052300141121          endif;
052400141121       // Deve essere numerico
052500141121           if    %check( c_Digits : d1lna  ) > *zero;
052600141121             Vd1msg = Msg(02);
052700141121             *in41  = *on;
052800141121             *in90  = *on;
052900141121             *in29  = *on;
053000141121             leavesr;
053100141121           endif;
053200141121        // controlla esistenza del codice
053300141121           if d1lna <>*zeros;
053400141121              k_ORGfil = %int( d1lna );
053500141121              chain  %kds( k01azorg01 )  AZORG;
053600141121
053700141121              if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
053800141121                 Vd1msg = Msg(02);
053900141121                 *in41  = *on;
054000141121                 *in90  = *on;
054100141121                 *in29  = *on;
054200141121                 leavesr;
054300141121              endif;
054400141121
054500141121              d1lna_d = ORGdes;
054600141121           else;
054700141121              d1lna_d = 'STANDARD';
054800141121           endif;
054900060508
055000141121       // se immissione controllo che non esista un record con la stessa chiave
055100141120        if comand = 'J' or opzio = '03';
055200151026         chain(n) (%int(d1lna):%int(d1tfp)) waoma11l;
055300151026         if %found(waoma11l);
055400060508          vd1msg = msg(05);
055500141121          *in29 = *on;
055600141121          *in90 = *on;
055700060508          *in40 = *on;
055800060508          leavesr;
055900060508         endif;
056000151026         // propongo il terminal di arrivo
056100151026             clear fnlv55ds;
056200151026             d55tpt='A'   ;
056300151026             d55lin = %int(d1lna)  ;
056400151026             d55drf = %dec(dataisocor:*iso)  ;
056500151026             fnlv55r(fnlv55ds);
056600151026
056700151026          if d1tfa<>d55tfa  ;
056800151026             d1tfa =d55tfa   ;
056900151026              k_ORGfil = %int( d55tfa);
057000151026              chain  %kds( k01azorg01 )  AZORG;
057100151026              d1tfa_d=orgdes  ;
057200151026          *in90 = *on;
057300151026          leavesr;
057400151026          endif  ;
057500060508        endif;
057600141121
057700141121       // Controllo correttezza formale dell'orario
057800151026          if %dec(%subst(%editc(d1hmx:'X'):1:2):2:0)>72 or
057900151026             %dec(%subst(%editc(d1hmx:'X'):3:2):2:0)>59;
058000141121          vd1msg = msg(03);
058100141121          *in29 = *on;
058200141121          *in90 = *on;
058300141121          *in42 = *on;
058400141121          leavesr;
058500141121       endif;
058600150513       // Errore se presente orario arrivo   ma filiale di arr.000
058700150513          if d1hmx>0 and d1lna=*zeros;
058800150513             vd1msg = msg(06);
058900150513             *in29 = *on;
059000150513             *in90 = *on;
059100150513             *in42 = *on;
059200150513             leavesr;
059300150513          endif;
059400150513          if %dec(%subst(%editc(d1hmxp:'X'):3:2):2:0)>59;
059500150513          vd1msg = msg(03);
059600150513          *in29 = *on;
059700150513          *in90 = *on;
059800150513          *in43 = *on;
059900150513          leavesr;
060000150513       endif;
060100150513       // Errore se presente orario partenza ma terminal 000
060200150513          if d1hmxp>0 and d1tfp=*zeros;
060300150513             vd1msg = msg(04);
060400150513             *in29 = *on;
060500150513             *in90 = *on;
060600150513             *in43 = *on;
060700150513             leavesr;
060800150513          endif;
060900060502
061000151026       // i nuovi campi si possono mettere solo se tfp = 0
061100151026       if d1tfp>*zeros  ;
061200151026       if d1faut<>0 or d1fpos<>0 or d1nbap<>0 or d1nbaa<>0 or d1ncas<>0 or
061300151026          d1afai<>0 or d1afaf<>0 or d1afpi<>0 or d1afpf<>0 or d1tfic<>0 or
061400151029          d1tfis<>0 or d1tvvc<>0 or d1tvvs<>0 or d1tfae<>0 or d1nsmr<>0 ;
061500151026             vd1msg = msg(07);
061600151026             *in29 = *on;
061700151026             *in90 = *on;
061800151026             *in44 = *on;
061900151026             leavesr;
062000151026       endif ;
062100151026       endif ;
062200151026
062300151026       // Controllo correttezza formale dell'orario
062400151026          if %dec(%subst(%editc(d1afai:'X'):1:2):2:0)>72 or
062500151026             %dec(%subst(%editc(d1afai:'X'):3:2):2:0)>59;
062600151026          vd1msg = msg(03);
062700151026          *in29 = *on;
062800151026          *in90 = *on;
062900151026          *in44 = *on;
063000151026          leavesr;
063100151026       endif;
063200151026
063300151026       // Controllo correttezza formale dell'orario
063400151026          if %dec(%subst(%editc(d1afaf:'X'):1:2):2:0)>72 or
063500151026             %dec(%subst(%editc(d1afaf:'X'):3:2):2:0)>59;
063600151026          vd1msg = msg(03);
063700151026          *in29 = *on;
063800151026          *in90 = *on;
063900151026          *in45 = *on;
064000151026          leavesr;
064100151026       endif;
064200151026       // Controllo correttezza formale dell'orario
064300151026          if %dec(%subst(%editc(d1afpi:'X'):1:2):2:0)>72 or
064400151026             %dec(%subst(%editc(d1afpi:'X'):3:2):2:0)>59;
064500151026          vd1msg = msg(03);
064600151026          *in29 = *on;
064700151026          *in90 = *on;
064800151026          *in46 = *on;
064900151026          leavesr;
065000151026       endif;
065100151026       // Controllo correttezza formale dell'orario
065200151026          if %dec(%subst(%editc(d1afpf:'X'):1:2):2:0)>72 or
065300151026             %dec(%subst(%editc(d1afpf:'X'):3:2):2:0)>59;
065400151026          vd1msg = msg(03);
065500151026          *in29 = *on;
065600151026          *in90 = *on;
065700151026          *in47 = *on;
065800151026          leavesr;
065900151026       endif;
066000151026
066100151026       // se inserita eccezione terminal controllo
066200151026       clear d1tfae_d;
066300151026       if d1tfae<>d1tfa   and d1tfae>0  ;
066400151026       // deve essere un terminal di arrivo
066500151026             clear fnlv55ds;
066600151026             d55tpt='A'   ;
066700151026             d55lin = d1tfae ;
066800151026             d55drf = %dec(dataisocor:*iso)  ;
066900151026             fnlv55r(fnlv55ds);
067000151026
067100151026          if d1tfae<>d55tfa or d55err<>' ' or d1tfae=d1tfa  ;
067200151026          vd1msg = msg(08);
067300151026          *in29 = *on;
067400151026          *in90 = *on;
067500151026          *in48 = *on;
067600151026          leavesr;
067700151026       endif;
067800151026              k_ORGfil = %int( d1tfae);
067900151026              chain  %kds( k01azorg01 )  AZORG;
068000151026              d1tfae_d = ORGdes;
068100151026       endif    ;
068200151026
068300060502       endsr;
068400060504
068500060504
068600060502       // ------------------------------------------------------------------------
068700060502       // aggiorno tabella
068800060502       // ------------------------------------------------------------------------
068900060502       begsr sr_aggiorna;
069000060502
069100060502       // imposto campi del file
069200141121        omatfp = %dec(d1tfp:3:0);
069300141121        omalna = %int(d1lna)   ;
069400141120        omahmx = d1hmx   ;
069500150513        omahmp = d1hmxp   ;
069600151026        omafaut= d1faut  ;
069700151026        omafpos= d1fpos  ;
069800151026        omanbap= d1nbap  ;
069900151026        omanbaa= d1nbaa  ;
070000151026        omancas= d1ncas  ;
070100151029        omansmr= d1nsmr  ;
070200151026        omaafai= d1afai  ;
070300151026        omaafaf= d1afaf  ;
070400151026        omaafpi= d1afpi  ;
070500151026        omaafpf= d1afpf  ;
070600151026        omatfic= d1tfic  ;
070700151026        omatfis= d1tfis  ;
070800151026        omatvvc= d1tvvc  ;
070900151026        omatvvs= d1tvvs  ;
071000151026        if d1tfae<>d1tfa and d1tfae>0  ;
071100151026        omatfae= d1tfae  ;
071200151026        else   ;
071300151026        clear omatfae  ;
071400151026        endif  ;
071500151026
071600141121        if d1dim1>0 ;
071700141121            dataiso= %date(d1dim1:*eur);
071800141120            omaduv = %dec(dataiso);
071900141120        else;
072000141121            omaduv = %dec(dataisocor:*iso) ;
072100141120        endif;
072200060502
072300060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
072400060502        select;
072500060502
072600060502       // F10=immissione - Opzione "03"=copia
072700141120         when comand = 'J' or opzio = '03';
072800060502       // scrivo nuovo record con gestione errore per chiave duplicata
072900151026          write waoma010;
073000060502
073100060502       // Opzione "02"=modifica
073200141120         when opzio = '02';
073300060502       // update record e controllo errore per chiave duplicata
073400151026          update waoma010;
073500060502
073600141121       // Opzione "04"=cancellazione
073700141120         when opzio = '04';
073800151026          delete waoma010;
073900010430
074000060502        endsl;
074100060509
074200060502
074300060502       endsr;
074400060502
074500060502       // ------------------------------------------------------------------------
074600060502       // pulizia video
074700060502       // ------------------------------------------------------------------------
074800060502       begsr sr_puld01;
074900060502
075000141120        clear d1tfp    ;
075100141120        clear d1tfp_d  ;
075200141120        clear d1lna   ;
075300141120        clear d1lna_d ;
075400141120        clear d1hmx   ;
075500150513        clear d1hmxp  ;
075600141121        clear d1dim1  ;
075700151026        clear d1faut  ;
075800151026        clear d1fpos  ;
075900151026        clear d1nbap  ;
076000151026        clear d1nbaa  ;
076100151026        clear d1ncas  ;
076200151029        clear d1nsmr  ;
076300151026        clear d1afai  ;
076400151026        clear d1afaf  ;
076500151026        clear d1afpi  ;
076600151026        clear d1afpf  ;
076700151026        clear d1tfic  ;
076800151026        clear d1tfis  ;
076900151026        clear d1tvvc  ;
077000151026        clear d1tvvs  ;
077100151026        clear d1tfae  ;
077200151026        clear d1tfae_d;
077300151026        clear d1tfa   ;
077400151026        clear d1tfa_d ;
077500060504
077600141121        *in29 = *off;
077700060504        *in40 = *off;
077800060504        *in41 = *off;
077900141121        *in42 = *off;
078000150513        *in43 = *off;
078100151026        *in44 = *off;
078200151026        *in45 = *off;
078300151026        *in46 = *off;
078400151026        *in47 = *off;
078500151026        *in48 = *off;
078600060502
078700060502       endsr;
078800060502
078900060502       // ------------------------------------------------------------------------
079000060502       // imposto i dati a video
079100060502       // ------------------------------------------------------------------------
079200060502       begsr sr_imposta;
079300060502
079400060502       // recupero i dati dal file
079500151026        chain (s1lna:s1tfp) waoma11l;
079600060502
079700151026       if %found(waoma11l)     ;
079800060502       // imposto i campi a video
079900141120        d1tfp    = %editc(omatfp:'X');
080000141120        clear orgdes;
080100141120        chain omatfp azorg01l;
080200141120        d1tfp_D=orgdes;
080300141120        d1lna    = %editc(omalna:'X');
080400141120        clear orgdes;
080500141120        chain omalna azorg01l;
080600141120        d1lna_D=orgdes;
080700141120        d1hmx    = omahmx;
080800150513        d1hmxp   = omahmp;
080900141120        if omaduv>0 ;
081000141120           dataeur =%date(omaduv:*iso);
081100141121           d1dim1= %dec(dataeur) ;
081200141120        endif;
081300151026        d1faut=omafaut   ;
081400151026        d1fpos=omafpos   ;
081500151026        d1nbap=omanbap   ;
081600151026        d1nbaa=omanbaa   ;
081700151026        d1ncas=omancas   ;
081800151029        d1nsmr=omansmr   ;
081900151026        d1afai=omaafai   ;
082000151026        d1afaf=omaafaf   ;
082100151026        d1afpi=omaafpi   ;
082200151026        d1afpf=omaafpf   ;
082300151026        d1tfic=omatfic   ;
082400151026        d1tfis=omatfis   ;
082500151026        d1tvvc=omatvvc   ;
082600151026        d1tvvs=omatvvs   ;
082700151026        d1tfae=omatfae   ;
082800151026        endif;
082900060502
083000151026        // imposto il campo terminal di arrivo
083100151026             clear fnlv55ds;
083200151026             d55tpt='A'   ;
083300151026             d55lin = omalna      ;
083400151026             d55drf = %dec(dataisocor:*iso)  ;
083500151026             fnlv55r(fnlv55ds);
083600151026             d1tfa=d55tfa   ;
083700151026              k_ORGfil = %int( d55tfa);
083800151026              chain  %kds( k01azorg01 )  AZORG;
083900151026              d1tfa_d=orgdes  ;
084000151026
084100151026         // decodifico eccezione terminal
084200151026         if d1tfae>0  ;
084300151026              k_ORGfil = %int(d1tfae);
084400151026              chain  %kds( k01azorg01 )  AZORG;
084500151026              d1tfae_d=orgdes  ;
084600151026          endif  ;
084700151026
084800060502       endsr;
084900060502
085000060502       // ------------------------------------------------------------------------
085100060502       // gestione del subfile
085200060502       // ------------------------------------------------------------------------
085300060502       begsr sr_gestsfl;
085400060502
085500060502       // set flag
085600060502        wrkeofs01 = *off;
085700060502
085800060502       // inizio lettura subfile
085900060502        dow wrkeofs01 = *off and *in21;
086000141120         readc tls1s01;
086100060502       // se fine subfile
086200060502         if %eof;
086300060502          wrkcars01 = *on;
086400060502          leave;
086500060502         endif;
086600060502       // se è stata immessa un'opzione
086700060502         if s1opzione <> *zeros;
086800141120          clear opzio;
086900141120          clear comand;
087000060502       // controllo ed elaborazione opzione immessa
087100060502          select;
087200060502       // opzione "01"=scelta
087300060502           when s1opzione = 1 and *in01;
087400141120            opzio = '01';
087500060502       // opzione "02"=modifica
087600141121           when s1opzione = 2 and not *in01 ;
087700141120            opzio = '02';
087800060502       // opzione "03"=copia
087900141121           when s1opzione = 3 and not *in01 ;
088000141120            opzio = '03';
088100141121       // opzione "04"=annullo
088200060502           when s1opzione = 4 and not *in01;
088300141120            opzio = '04';
088400060502       // opzione "05"=visualizzazione
088500060502           when s1opzione = 5;
088600141120            opzio = '05';
088700060502          endsl;
088800010503
088900060502       // se immessa almeno un opzione valida
089000141120          if  opzio <> *blanks;
089100060502       // gstione del formato video
089200060504           exsr sr_d01;
089300060502       // se la gestione si è chiusa con ...
089400060504           select;
089500060502       // F6=Conferma
089600141120            when ricez = 'F';
089700060504             clear s1opzione;
089800060504             wrkcars01 = *on;
089900060502       // F12=Ritorno
090000141120            when ricez = 'L';
090100060504             clear s1opzione;
090200060504             wrkeofs01 = *on;
090300060502       // altrimenti
090400060504            other;
090500060504             *in22 = *on;
090600060504           endsl;
090700060502
090800141120           update tls1s01;
090900060504           *in22 = *off;
091000060504       // fine opzione
091100060504          endif;
091200060502       // fine opzione
091300060502         endif;
091400060502
091500060502        enddo;
091600060502
091700060502       endsr;
091800060502
091900060502       // ------------------------------------------------------------------------
092000060502       // posizionamento sul file
092100060502       // ------------------------------------------------------------------------
092200141120       begsr sr_setll     ;
092300060502
092400060502        select;
092500141120       // con posizionamento richiesto
092600141120         when c1setll >0;
092700151026          setll c1setll waoma11l;
092800141120       // dall'inizio
092900141120         when c1setll = 0;
093000151026          setll *loval waoma11l;
093100060502        endsl;
093200060502
093300060502       endsr;
093400060502
093500060502       // ------------------------------------------------------------------------
093600060502       // scrivo subfile
093700060502       // ------------------------------------------------------------------------
093800060502       begsr sr_wtrs01;
093900060502
094000060502       // se non raggiunto limite massimo di caricamento
094100060505        if s01nrr < 9999;
094200060502       // imposto campi di subfile
094300060502         exsr sr_sets01;
094400060502       // imposto numeratore righe e numero relativo di record
094500060505         s01nrr = s01nrr + 1;
094600060502       // scrivo subfile
094700141120         write tls1s01;
094800060502        endif;
094900060502
095000060502       endsr;
095100060502
095200060502       // ------------------------------------------------------------------------
095300060502       // imposto campi del subfile
095400060502       // ------------------------------------------------------------------------
095500060502       begsr sr_sets01;
095600060502
095700141120        // Terminal di partenza e decodifica
095800141120        s1tfp    = omatfp;
095900141120
096000141120        clear orgdes;
096100141121        if omatfp>0;
096200141121           chain s1tfp azorg01l;
096300141121           s1tfp_d=orgdes;
096400141121        else;
096500141125           s1tfp_d=*blanks   ;
096600141121        endif;
096700141120
096800141120        // filiale di arrivo  e decodifica
096900141120        s1lna    = omalna;
097000141120
097100141120        clear orgdes;
097200141120        chain s1lna azorg01l;
097300141120        s1lna_d=orgdes;
097400141120
097500141120        // ora max di arrivo
097600141120        s1hmx    = omahmx;
097700150513        s1hmxp   = omahmp;
097800141120
097900141120        // data ultima variazione
098000141120        if omaduv>0;
098100141120           dataeur=%date(omaduv:*iso);
098200141120           s1dim =%dec(dataeur);
098300141120        endif;
098400141120        //s1tbeatb = tbeatb;
098500060502
098600060502       endsr;
098700060505
098800060502       // ------------------------------------------------------------------------
098900060502       // routine iniziale
099000060502       // ------------------------------------------------------------------------
099100060428         begsr *inzsr;
099200060428
099300060428      /end-free
099400060428
099500060428     c     *entry        plist
099600060428     c                   parm                    kpjba
099700060428
099800060428      /free
099900060428         in(e) §azute;
100000060428         if not %error;
100100060428          in(e) §datiute;
100200060428         endif;
100300060502         if %error or rsut = *blanks;
100400060428          tibs34r(tibs34ds);
100500060428          in §azute;
100600060428          in §datiute;
100700060428         endif;
100800141120         // Data corrente
100900141120         dataisocor=%date();
101000060428
101100060428         endsr;
101200060502
101300060502       // ------------------------------------------------------------------------
101400060502       // uscita dal programma
101500060502       // ------------------------------------------------------------------------
101600060502         begsr sr_uscita;
101700060502
101800141120       //   if b49esito = *blanks;
101900141120       //    b49esito = eseguito;
102000141120       //   endif;
102100060502
102200141120       //   kpjbu = tntb49ds;
102300060502
102400151028             clear fnlv55ds;
102500151028             d55tla='C'   ;
102600151028             fnlv55r(fnlv55ds);
102700151028
102800060502          *inlr = *on;
102900060502          return;
103000060502
103100060502         endsr;
103200060502
103300060502      /end-free
103400060502
103500060502** -MSG-                                                                     *
103600141121Terminal di partenza mancante o errato                                         01
103700141121Filiale di arrivo mancante o errata                                            02
103800141121Orario formalmente errato                                                      03
103900150513Lasciare vuoto l'orario oppure inserire un Terminal di Partenza specifico      04
104000141121Già presente orario per il terminal di partenza/filiale di arrivo immessi      05
104100150513Lasciare vuoto l'orario oppure inserire una Filiale di arrivo specifica        06
104200151026Dati aggiuntivi inseribili solo per filiale di arrivo con terminal =0          07
104300151026Il terminal immesso è errato o è un secondo livello                            08
