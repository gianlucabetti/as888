000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300150513      * Gestione file orari massimi di fiiliale                                *
000400060427      *------------------------------------------------------------------------*
000500141120     fwaoma01l  uf a e           k disk
000501141120     fazorg01l  if   e           k disk
000800141120     ftntls1d   cf   e             workstn sfile(tls1s01:s01nrr)
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
001400060428      * 01 - ON ricerca -- OFF manutenzione
001500060508      * 02 - proteggo campo causale
001600060508      * 03 - proteggo campi video 01
001700060509      * 04 - visualizzazione
001800141120      * 05 -
001900060508      * 20 - gestione subfile 01
002000060508      * 21 - gestione subfile 01
002100060508      * 22 - gestione subfile 01
002200060508      * 23 - gestione subfile 01
002300141120      * 24 -
002400141120      * 25 -
002500060428      * 28 - messaggio errore
002600060502      * 40 - causale
002700060502      * 41 - descrizione
002800060505      * 47 - descrizione subfile
002900060502
003000060502      *------------------------------------------------------------------------*
003100060502      * schiere
003200060502      *------------------------------------------------------------------------*
003300060502     d msg             s             78    dim(17) ctdata perrcd(1)
003400060427
003500060428      *------------------------------------------------------------------------*
003600060428      * campi di work
003700060428      *------------------------------------------------------------------------*
003800060428     d blanks          s                   like(d1descopz)
003900141120     d comand          s              1
003901141120     d opzio           s              2
003902141120     d ricez           s              1
004100060428     d len             s              5u 0
004200060505     d s01nrr          s              4  0
004400060428     d savopzione      s                   like(s1opzione)
004500060428     d savrcdnbr       s                   like(c1rcdnbr)
005100060428     d wrkeofs01       s              1
005200060428     d wrkcars01       s              1
005500060504     d $video          s             10
005501141120     d Dataeur         s               d   datfmt(*eur)
005502141120     d Dataisocor      s               d   datfmt(*iso)
005503141120     d Dataiso         s               d   datfmt(*iso)
005504141121      // ricerca filiale su organigramma
005505141121     d   pInFIL        S              3
005506141121     d   pInFAG        S              1
005507141121     d   pInDES        S             25
005508141121     d   pInDIT        S              3
005600060428
005700060428      *------------------------------------------------------------------------*
005800060428      * ds interne/esterne
005900060428      *------------------------------------------------------------------------*
006500060428
006700060502     d kpjba         e ds
006800060503     d tibs34ds      e ds                  inz
007000060428     d §azute        e ds                  extname(azute00f)
007100060428     d                                     dtaara
007200060428     d §datiute      e ds                  extname(ddatiute)
007300060428     d                                     dtaara
007301141121       // -?File AZORG01L?
007302141121     d k01azorg01    e ds                  extname(AZORG01L : *key)
007303141121     d                                     prefix(k_)   inz
007304141121     d trul06ds      e ds
007305141121     D  LIN                    1     90    dim(30)
007400010503
007500060428     d psds           sds
007600060428     d  pgmname          *proc
007700141121
007800060428      *------------------------------------------------------------------------*
007900060428      * pgm richiamati
008000060428      *------------------------------------------------------------------------*
008100060428     d tibs34r         pr                  extpgm('TIBS34R')
008200060502     d  tibs34ds                           likeds(tibs34ds)
008300060504
008600060428
008700060428      *------------------------------------------------------------------------*
008800060428      * costanti
008900060428      *------------------------------------------------------------------------*
009000060428     d errore          c                   '1'
009100060428     d eseguito        c                   '0'
009101141121       // -?Costante per controllo "caratteri solo numerici"?
009102141121     d c_Digits        c                   const('0123456789')
009200010503
009201141121      /copy gaitrasrc/srcprotopr,tnsd24r
009202141121      /copy gaitrasrc/srcprotopr,trul06r
009300060428      *------------------------------------------------------------------------*
009400010423
009500060428      /free
009600060428
009601141120       *in01 =*off;
009700141120       //exsr sr_parm;
009800060428       exsr sr_c01;
009900060428       exsr sr_uscita;
010000010423
010100060502       // ------------------------------------------------------------------------
010200060502       // elaborazione parametri ricevuti
010300060502       // ------------------------------------------------------------------------
010400141120       //begsr sr_parm;
010500060428
010600141120       // tntb49dS = kpjbu;
010700060428
010800141120       // Select;
010900060502       // ricerca e scleta
011000141120       //  when b49Fun = '1';
011100141120       //   *in01 = *on;
011200060502       // manutenzione
011300141120       //  when  b49Fun = *blanks;
011400141120       // *in01 = *off;
011500060502       // altrimenti
011600141120       //  other;
011700141120       //   b49Esito = errore;
011800141120       //    exsr sr_uscita;
011900141120       //  endsl;
012000060428
012100141120       //endsr;
012200010430
012300060502       // ------------------------------------------------------------------------
012400060502       // gestione subfile
012500060502       // ------------------------------------------------------------------------
012600060428       begsr sr_c01;
012700060428
012800060502       // imposto variabili
012900060428        wrkcars01 = *on;
013000060428        $video = 'C01';
013100060428
013200060502       // inizio elaborazione subfile
013300060428         dou  $video <> 'C01';
013400060428
013500060502       // caricamento subfile
013600060428          exsr sr_cars01;
013700060428
013800060502       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
013900060502       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
014000060502       // l'ultimo tasto
014100060428          if c1csrrrn > 0;
014200060428           c1rcdnbr = c1csrrrn;
014300060428          else;
014400060428           c1rcdnbr = savrcdnbr;
014500060428          endif;
014600060428
014700060502       // se non so quale pagina visualizzare forzo pagina 1
014800060428          if c1rcdnbr < 1;
014900060428           eval c1rcdnbr = 1;
015000060428          endif;
015100060428
015200060502       // salvo il record number del subfile
015300060428          savrcdnbr = c1Rcdnbr;
015400060428
015500060502       // Emissione del subfile
015600141120          write tls1p01;
015700141120          exfmt tls1c01;
015800060428
015900060502       // controllo tasti funzionali del subfile
016000060428          select;
016100060428
016200060502       // F3=Fine
016300060428           when *inkc;
016400060502            $video = *blanks;
016500141120       //     b49ricez = 'C';
016600060428            exsr sr_uscita;
016700060428
016800060502       // F5=Refresh
016900060428           when *inke;
017000060428            wrkcars01 = *on;
017100060428
017200060502       // F10=Immissione
017300060428           when *inkj;
017400060502            $video = 'D01';
017500141120       //   clear tntb49ds;
017600141120               comand = 'J';
017601141120               clear opzio;
017700060428            exsr sr_d01;
017800060428
017900060502       // F13=Ripetizione
018000141120       //  when *inkm;
018100141120       //   exsr sr_ripetiopz;
018200060428
018300060502       // in tutti gli altri casi
018400060428           other;
018500060502       // controllo ed elaborazione scelte su subfile
018600060428            exsr sr_gestsfl;
018700060428          endsl;
018800060428
018900060502       // fine elaborazione 'C01'
019000060428         enddo;
019100060428
019200060428        endsr;
019300060428
019400060502       // ------------------------------------------------------------------------
019500060502       // caricamento subfile
019600060502       // ------------------------------------------------------------------------
019700060428       begsr sr_cars01;
019800010430
019900060502       // se è stato richiesto il caricamento del subfile
020000060428        if wrkcars01 = *on;
020100060428
020200060502       // inizializzo il subfile
020300060505         s01nrr = 0;
020400060428         *in20 = *on;
020500141120         write tls1c01;
020600060428         *in20 = *off;
020700060428         *in21 = *off;
020800060428         *in22 = *off;
020900060428         *in23 = *off;
021000060428
022200060428
022300060502       // posizionamento file
022400141120         exsr sr_setll     ;
022500060428
022600060502       // fino a che non è fine file
022700141120         dou %eof(waoma01l);
022800060428
022900060502       // leggo file
023000141120          read(n) waoma01l;
023100060428
023200060502       // fine file esco
023300141120          if %eof(waoma01l);
023400060428           leave;
023500060428          endif;
023600060428
023900060502       // scrivo subfile
024000060428           clear s1opzione;
024100060428           exsr sr_wtrs01;
024300060428
024400060428         enddo;
024500060428
024600060502       // fine caricamento subfile
024700060428        endif;
024800060428
024900060502       // se scritto almeno un record
025000060505        if s01nrr > 0;
025100060502       // indicatore di sflend
025200060428         eval *in21 = *on;
025300060428        endif;
025400060428
025500060502       // se non scritto neanche un record
025600060505        if s01nrr = 0;
025700060502       // indicatore di sfldsp
025800060428         eval *in23 = *on;
025900060428        endif;
026000060428
026100060502       // disattivo opzione di caricamento subfile
026200060428        wrkcars01 = *off;
026300060428
026400060428       endsr;
026500060428
026600060502       // ------------------------------------------------------------------------
026700060502       // gestione videata
026800060502       // ------------------------------------------------------------------------
026900060428       begsr sr_d01;
027000060428
027100060504       // imposto il video a seconda della funzione richiesta
027200060428        exsr sr_setvideo;
027300060428
027400060502       // imposto variabile
027500060428        $video = 'D01';
027600060428
027700060502       // fino a che la variabile resta settata come 'D01'
027800060428        dou $video <> 'D01';
027900060428
028000060508         *in02 = *off;
028100060508         *in03 = *off;
028200060509         *in04 = *off;
028300060428
028400060502       // se immessa opzione di 'scelta'
028500141120         if opzio = '01';
028600141120       //   b49ke1 = s1tbeke1;
028700141120       //   b49des = s1tbeuni;
028800060428          exsr sr_uscita;
028900060428
029000060508       // se immessa un'altra opzione
029100060508         else;
029200060508       // se non è immissione/copia proteggo il campo della causale
029300141120          if comand <> 'J' and opzio <> '03';
029400060428           *in02 = *on;
029500060428          endif;
029600141121       // se immessa opzione di 'visualizzazione' 'cancellazione'
029700060508       // proteggo i campi del video
029800141120          if opzio = '04' or opzio = '05';
029900060508           *in03 = *on;
030000060508          endif;
030100060509       // se immessa opzione di 'visualizzazione'
030200060509       // non attivo F6
030300141120          if opzio = '05';
030400060509           *in04 = *on;
030500060509          endif;
030600060502       // emetto il video
030700141120          exfmt tls1d01;
030800060428         endif;
030900060428
031000141121       // reset indicatore
031100141121         *in29 = *off;
031200060508
031300060508       // pulisco il campo messaggi
031400060508         clear vd1msg;
031500010430
031600060502       // controllo tasti funzionali del video
031700060428         select;
031800060428
031900060502       // F3=Fine
032000060428          when *inkc;
032100141120           ricez = 'C';
032200060502           $video = *blanks;
032300141120           unlock waoma01l;
032400060428           exsr sr_uscita;
032500060428
032600060502       // F6=Conferma
032700060428          when *inkf;
032800141120           ricez = 'F';
032900060502       // controllo e decodifico i dati del video
033000060428           exsr sr_ctrd01;
034200060428
034300141121       // se non rdevo riemettere la videata aggiorno
034400141121           if not *in90;
034401141120              exsr sr_aggiorna;
034402141121              if comand='J';
034403141121                 $video = 'D01';
034404141121                 exsr sr_setvideo;
034405141121              else;
034406141121                 $video = 'C01';
034407141121              endif;
034800060428           endif;
034900060428
035000060502       // F8=Record successivo
035100141120       // when  *inkh;
035200141120       //  clear s1opzione;
035300141120       //  wrkcars01 = *off;
035400141120       //  $video = 'C01';
035500141120       //  b49ricez = 'H';
035600060505
035700060505       // F9=Traduzione
035800141120       // when  *inki;
035900141120       //  *in05 = *on;
036000141120       //  exsr sr_lingua;
036100141120       //  *in05 = *off;
036200060428
036300060502       // F12=Ritorno
036400060428          when *inkl;
036500060428           clear s1opzione;
036600141120           ricez = 'L';
036700141120           unlock waoma01l;
036800060508       // se non è f12 da immissione/copia non ricarico il subfile
036900141120           if comand = 'J' or opzio = '03';
037000060428            wrkcars01 = *on;
037100060428           else;
037200060428            wrkcars01 = *off;
037300060428           endif;
037400060428           $video = 'C01';
037500060428
037600060502       // Invio
037700060428          other;
037800060508           if not *in03;
037900060508            exsr sr_ctrd01;
038000060508           endif;
038100060428
038200060502         endsl;
038300060428
038400060502       // fine gestione 'D01'
038500060428        enddo;
038600060428
038700060428       endsr;
042100060428
042200060502       // ------------------------------------------------------------------------
042300060502       // imposto i dati a video
042400060502       // ------------------------------------------------------------------------
042500060428       begsr sr_setvideo;
042600060428
042700060502       // pulisco il formato video D01
042800060502        exsr sr_puld01;
042900060428
043000060502       // controllo se 'immissione' 'modifica' 'copoa' o altro
043100060428        select;
043200060428
043300060502       // F10=Immissione
043400141120         when comand = 'J';
043500060428          d1descopz = 'Immissione';
043600060428
043700060502       // Opzione "02"=modifica
043800141120         when opzio = '02';
043900060428          d1descopz = 'Modifica';
044000060428          exsr sr_imposta;
044100060428
044200060502       // Opzione "03"=copia
044300141120         when opzio = '03';
044400060428          d1descopz = 'Copia';
044500060428          exsr sr_imposta;
044600060428
044700141121       // Opzione "04"=cancellazione
044800141120         when opzio = '04';
044900060428          exsr sr_imposta;
045200060502           d1descopz = 'Annullamento';
045800060502
045900060502       // Opzione "05"=visualizzazione
046000141120         when Opzio ='05';
046100060502          d1descopz = 'Visualizzazione';
046200060502          exsr sr_imposta;
046300060502
046400060502       // Fine scelta
046500060502        endsl;
046600060502
046700060502       // centro la descrizione della funzione nel formato video
046800060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
046900060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
047000060502
047100060502       endsr;
047200060502
047300060502       // ------------------------------------------------------------------------
047400060502       // controllo video
047500060502       // ------------------------------------------------------------------------
047600060502       begsr sr_ctrd01;
047700060502
047800141121        *in29 = *off;
047801141121        *in90 = *off;
047900060502        *in40 = *off;
048000060502        *in41 = *off;
048001141121        *in42 = *off;
048002150513        *in43 = *off;
048100010430
048200141121       // TERMINAL DI PARTENZA
048201141121        clear d1tfp_d;
048202141121       // Obbligatorio
048300141121        if d1tfp  = *blanks;
048400060502         vd1msg = msg(01);
048500141121         *in29 = *on;
048501141121         *in90 = *on;
048600060502         *in40 = *on;
048700060502         leavesr;
048800060502        endif;
048801141121       // Ricerca su organigramma
048802141121          if %scan('?':d1tfp)>0;
048803141121           clear pinfil   ;
048804141121           clear pinfag   ;
048805141121           clear pindes   ;
048806141121           clear pindit   ;
048807141121           tnsd24r(pinfil:pinfag:pindes:pindit)   ;
048808141121           if pinfil > *zeros;
048809141121              d1tfp=pinfil;
048810141121              d1tfp_d=pindes;
048811141121           else;
048812141121              clear d1tfp ;
048813141121              clear d1tfp_d ;
048814141121           endif;
048815141121           *in90=*on;
048816141121           *in40=*on;
048817141121           leavesr;
048818141121          endif;
048819141121       // Deve essere numerico
048821141121           if    %check( c_Digits : d1tfp  ) > *zero;
048822141121             Vd1msg = Msg(01);
048823141121             *in40  = *on;
048824141121             *in90  = *on;
048825141121             *in29  = *on;
048826141121             leavesr;
048827141121           endif;
048828141121        // controlla esistenza del codice
048829141121           if d1tfp <>*zeros;
048830141121              k_ORGfil = %int( d1tfp );
048831141121              chain  %kds( k01azorg01 )  AZORG;
048832141121
048833141121              if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
048834141121                 Vd1msg = Msg(01);
048835141121                 *in40  = *on;
048836141121                 *in90  = *on;
048837141121                 *in29  = *on;
048838141121                 leavesr;
048839141121              endif;
048840141121
048841141121              d1tfp_d = ORGdes;
048842141121       // Deve essere un terminal di partenza
048843141121          clear trul06ds;
048844141121          d06cod='£1';
048845141121          d06key=d1tfp;
048846141121          d06tla='L';
048847141121          kpjbu=trul06ds;
048848141121          trul06r(kpjba);
048849141121          trul06ds=kpjbu;
048850141121          if d06err<>' ';
048851141121             *in90=*on;
048852141121             *in29=*on;
048853141121             eval vd1msg=msg(01);
048854141121             leavesr;
048855141121          endif;
048856141121           else;
048857141121              d1tfp_d = 'STANDARD';
048858141121           endif;
048861141121
048862141121       // FILIALE DI ARRIVO
048863141121        clear d1lna_d;
048864141121       // Obbligatorio
048865141121        if d1lna  = *blanks;
048866141121         vd1msg = msg(02);
048867141121         *in29 = *on;
048868141121         *in90 = *on;
048869141121         *in41 = *on;
048870141121         leavesr;
048871141121        endif;
048872141121       // Ricerca su organigramma
048873141121          if %scan('?':d1lna)>0;
048874141121           clear pinfil   ;
048875141121           clear pinfag   ;
048876141121           clear pindes   ;
048877141121           clear pindit   ;
048878141121           tnsd24r(pinfil:pinfag:pindes:pindit)   ;
048879141121           if pinfil > *zeros;
048880141121              d1lna=pinfil;
048881141121              d1lna_d=pindes;
048882141121           else;
048883141121              clear d1lna ;
048884141121              clear d1lna_d ;
048885141121           endif;
048886141121           *in90=*on;
048887141121           *in41=*on;
048888141121           leavesr;
048889141121          endif;
048890141121       // Deve essere numerico
048892141121           if    %check( c_Digits : d1lna  ) > *zero;
048893141121             Vd1msg = Msg(02);
048894141121             *in41  = *on;
048895141121             *in90  = *on;
048896141121             *in29  = *on;
048897141121             leavesr;
048898141121           endif;
048899141121        // controlla esistenza del codice
048900141121           if d1lna <>*zeros;
048901141121              k_ORGfil = %int( d1lna );
048902141121              chain  %kds( k01azorg01 )  AZORG;
048903141121
048904141121              if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
048905141121                 Vd1msg = Msg(02);
048906141121                 *in41  = *on;
048907141121                 *in90  = *on;
048908141121                 *in29  = *on;
048909141121                 leavesr;
048910141121              endif;
048911141121
048912141121              d1lna_d = ORGdes;
048913141121           else;
048914141121              d1lna_d = 'STANDARD';
048915141121           endif;
048918060508
049000141121       // se immissione controllo che non esista un record con la stessa chiave
049100141120        if comand = 'J' or opzio = '03';
049600141121         chain(n) (%int(d1lna):%int(d1tfp)) waoma01l;
049700141121         if %found(waoma01l);
049800060508          vd1msg = msg(05);
049900141121          *in29 = *on;
049901141121          *in90 = *on;
050000060508          *in40 = *on;
050100060508          leavesr;
050200060508         endif;
050300060508        endif;
050301141121
050302141121       // Controllo correttezza formale dell'orario
050303150430       // if %dec(%subst(%editc(d1hmx:'X'):1:2):2:0)>24 or
050304150430          if %dec(%subst(%editc(d1hmx:'X'):3:2):2:0)>59;
050305141121          vd1msg = msg(03);
050306141121          *in29 = *on;
050307141121          *in90 = *on;
050308141121          *in42 = *on;
050309141121          leavesr;
050310141121       endif;
050311150513       // Errore se presente orario arrivo   ma filiale di arr.000
050312150513          if d1hmx>0 and d1lna=*zeros;
050313150513             vd1msg = msg(06);
050314150513             *in29 = *on;
050315150513             *in90 = *on;
050316150513             *in42 = *on;
050317150513             leavesr;
050318150513          endif;
050319150513          if %dec(%subst(%editc(d1hmxp:'X'):3:2):2:0)>59;
050320150513          vd1msg = msg(03);
050321150513          *in29 = *on;
050322150513          *in90 = *on;
050323150513          *in43 = *on;
050324150513          leavesr;
050325150513       endif;
050326150513       // Errore se presente orario partenza ma terminal 000
050327150513          if d1hmxp>0 and d1tfp=*zeros;
050328150513             vd1msg = msg(04);
050329150513             *in29 = *on;
050330150513             *in90 = *on;
050331150513             *in43 = *on;
050332150513             leavesr;
050333150513          endif;
050400060502
051200060502
051300060502       endsr;
051400060504
057300060504
061000060502       // ------------------------------------------------------------------------
061100060502       // aggiorno tabella
061200060502       // ------------------------------------------------------------------------
061300060502       begsr sr_aggiorna;
061400060502
061500060502       // imposto campi del file
061700141121        omatfp = %dec(d1tfp:3:0);
061800141121        omalna = %int(d1lna)   ;
061801141120        omahmx = d1hmx   ;
061802150513        omahmp = d1hmxp   ;
061803141121        if d1dim1>0 ;
061804141121            dataiso= %date(d1dim1:*eur);
061805141120            omaduv = %dec(dataiso);
061806141120        else;
061807141121            omaduv = %dec(dataisocor:*iso) ;
061808141120        endif;
064500060502
064600060509       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
064700060502        select;
064800060502
064900060502       // F10=immissione - Opzione "03"=copia
065000141120         when comand = 'J' or opzio = '03';
065100060502       // scrivo nuovo record con gestione errore per chiave duplicata
065200141120          write waoma000;
065300060502
065400060502       // Opzione "02"=modifica
065500141120         when opzio = '02';
065600060502       // update record e controllo errore per chiave duplicata
065700141120          update waoma000;
065800060502
065900141121       // Opzione "04"=cancellazione
066000141120         when opzio = '04';
066100141121          delete waoma000;
066200010430
066300060502        endsl;
066400060509
069000060502
069100060502       endsr;
069200060502
069300060502       // ------------------------------------------------------------------------
069400060502       // pulizia video
069500060502       // ------------------------------------------------------------------------
069600060502       begsr sr_puld01;
069700060502
069800141120        clear d1tfp    ;
069801141120        clear d1tfp_d  ;
069900141120        clear d1lna   ;
069901141120        clear d1lna_d ;
070000141120        clear d1hmx   ;
070001150513        clear d1hmxp  ;
070002141121        clear d1dim1  ;
071800060504
071900141121        *in29 = *off;
072000060504        *in40 = *off;
072100060504        *in41 = *off;
072101141121        *in42 = *off;
072102150513        *in43 = *off;
072200060502
072300060502       endsr;
072400060502
072500060502       // ------------------------------------------------------------------------
072600060502       // imposto i dati a video
072700060502       // ------------------------------------------------------------------------
072800060502       begsr sr_imposta;
072900060502
073000060502       // recupero i dati dal file
073300141120        chain (s1lna:s1tfp) waoma01l;
073400060502
073500060502       // imposto i campi a video
073600141120        d1tfp    = %editc(omatfp:'X');
073601141120        clear orgdes;
073602141120        chain omatfp azorg01l;
073603141120        d1tfp_D=orgdes;
073700141120        d1lna    = %editc(omalna:'X');
073701141120        clear orgdes;
073702141120        chain omalna azorg01l;
073703141120        d1lna_D=orgdes;
073800141120        d1hmx    = omahmx;
073801150513        d1hmxp   = omahmp;
073802141120        if omaduv>0 ;
073803141120           dataeur =%date(omaduv:*iso);
073804141121           d1dim1= %dec(dataeur) ;
073805141120        endif;
075700060502
075800060502       endsr;
075900060502
076000060502       // ------------------------------------------------------------------------
076100060502       // gestione del subfile
076200060502       // ------------------------------------------------------------------------
076300060502       begsr sr_gestsfl;
076400060502
076500060502       // set flag
076600060502        wrkeofs01 = *off;
076700060502
076800060502       // inizio lettura subfile
076900060502        dow wrkeofs01 = *off and *in21;
077000141120         readc tls1s01;
077100060502       // se fine subfile
077200060502         if %eof;
077300060502          wrkcars01 = *on;
077400060502          leave;
077500060502         endif;
077600060502       // se è stata immessa un'opzione
077700060502         if s1opzione <> *zeros;
077701141120          clear opzio;
077702141120          clear comand;
078000060502       // controllo ed elaborazione opzione immessa
078100060502          select;
078200060502       // opzione "01"=scelta
078300060502           when s1opzione = 1 and *in01;
078400141120            opzio = '01';
078500060502       // opzione "02"=modifica
078600141121           when s1opzione = 2 and not *in01 ;
078700141120            opzio = '02';
078800060502       // opzione "03"=copia
078900141121           when s1opzione = 3 and not *in01 ;
079000141120            opzio = '03';
079100141121       // opzione "04"=annullo
079200060502           when s1opzione = 4 and not *in01;
079300141120            opzio = '04';
079400060502       // opzione "05"=visualizzazione
079500060502           when s1opzione = 5;
079600141120            opzio = '05';
079700060502          endsl;
079800010503
079900060502       // se immessa almeno un opzione valida
080000141120          if  opzio <> *blanks;
080100060502       // gstione del formato video
080200060504           exsr sr_d01;
080300060502       // se la gestione si è chiusa con ...
080400060504           select;
080500060502       // F6=Conferma
080600141120            when ricez = 'F';
080700060504             clear s1opzione;
080800060504             wrkcars01 = *on;
080900060502       // F12=Ritorno
081000141120            when ricez = 'L';
081100060504             clear s1opzione;
081200060504             wrkeofs01 = *on;
081300060502       // altrimenti
081400060504            other;
081500060504             *in22 = *on;
081600060504           endsl;
081700060502
081800141120           update tls1s01;
081900060504           *in22 = *off;
082000060504       // fine opzione
082100060504          endif;
082200060502       // fine opzione
082300060502         endif;
082400060502
082500060502        enddo;
082600060502
082700060502       endsr;
082800060502
082900060502       // ------------------------------------------------------------------------
083000060502       // posizionamento sul file
083100060502       // ------------------------------------------------------------------------
083200141120       begsr sr_setll     ;
083300060502
083400060502        select;
083500141120       // con posizionamento richiesto
083600141120         when c1setll >0;
083700141120          setll c1setll waoma01l;
083800141120       // dall'inizio
083900141120         when c1setll = 0;
084000141120          setll *loval waoma01l;
084100060502        endsl;
084200060502
084300060502       endsr;
084400060502
084500060502       // ------------------------------------------------------------------------
084600060502       // scrivo subfile
084700060502       // ------------------------------------------------------------------------
084800060502       begsr sr_wtrs01;
084900060502
085000060502       // se non raggiunto limite massimo di caricamento
085100060505        if s01nrr < 9999;
085200060502       // imposto campi di subfile
085300060502         exsr sr_sets01;
085400060502       // imposto numeratore righe e numero relativo di record
085500060505         s01nrr = s01nrr + 1;
085600060502       // scrivo subfile
085700141120         write tls1s01;
085800060502        endif;
085900060502
086000060502       endsr;
086100060502
086200060502       // ------------------------------------------------------------------------
086300060502       // imposto campi del subfile
086400060502       // ------------------------------------------------------------------------
086500060502       begsr sr_sets01;
086600060502
086601141120        // Terminal di partenza e decodifica
086700141120        s1tfp    = omatfp;
086701141120
086702141120        clear orgdes;
086703141121        if omatfp>0;
086704141121           chain s1tfp azorg01l;
086705141121           s1tfp_d=orgdes;
086706141121        else;
086707141125           s1tfp_d=*blanks   ;
086708141121        endif;
086709141120
086710141120        // filiale di arrivo  e decodifica
086800141120        s1lna    = omalna;
086801141120
086802141120        clear orgdes;
086803141120        chain s1lna azorg01l;
086804141120        s1lna_d=orgdes;
086805141120
086806141120        // ora max di arrivo
086900141120        s1hmx    = omahmx;
086901150513        s1hmxp   = omahmp;
086902141120
086903141120        // data ultima variazione
086904141120        if omaduv>0;
086905141120           dataeur=%date(omaduv:*iso);
086906141120           s1dim =%dec(dataeur);
086907141120        endif;
087100141120        //s1tbeatb = tbeatb;
087200060502
087300060502       endsr;
087400060505
104000060502       // ------------------------------------------------------------------------
104100060502       // routine iniziale
104200060502       // ------------------------------------------------------------------------
104300060428         begsr *inzsr;
104400060428
104500060428      /end-free
104600060428
104700060428     c     *entry        plist
104800060428     c                   parm                    kpjba
104900060428
105000060428      /free
105100060428         in(e) §azute;
105200060428         if not %error;
105300060428          in(e) §datiute;
105400060428         endif;
105500060502         if %error or rsut = *blanks;
105600060428          tibs34r(tibs34ds);
105700060428          in §azute;
105800060428          in §datiute;
105900060428         endif;
105901141120         // Data corrente
105902141120         dataisocor=%date();
106000060428
106100060428         endsr;
106200060502
106300060502       // ------------------------------------------------------------------------
106400060502       // uscita dal programma
106500060502       // ------------------------------------------------------------------------
106600060502         begsr sr_uscita;
106700060502
106800141120       //   if b49esito = *blanks;
106900141120       //    b49esito = eseguito;
107000141120       //   endif;
107100060502
107200141120       //   kpjbu = tntb49ds;
107300060502
107400060502          *inlr = *on;
107500060502          return;
107600060502
107700060502         endsr;
107800060502
107900060502      /end-free
108000060502
108100060502** -MSG-                                                                     *
108200141121Terminal di partenza mancante o errato                                         01
108300141121Filiale di arrivo mancante o errata                                            02
108400141121Orario formalmente errato                                                      03
108500150513Lasciare vuoto l'orario oppure inserire un Terminal di Partenza specifico      04
108600141121Già presente orario per il terminal di partenza/filiale di arrivo immessi      05
108700150513Lasciare vuoto l'orario oppure inserire una Filiale di arrivo specifica        06
