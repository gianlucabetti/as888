000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120622      * Gestione tabella "FLB" = Forzatura linea di arrivo per cliente mittente*
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000501090424     fazorg01l  if   e           k disk
000800120615     ftntbflbd  cf   e             workstn sfile(tbfls01:s01nrr)
001100120622
001200120622      *------------------------------------------------------------------------*
001300120622      * riepilogo indicatori
001400120622      *------------------------------------------------------------------------*
001500120622      * 02 - visualizzazione
001600120622      * 03 - proteggo campi video 01
001700120622      * 04 - manutenzione
001800120622      * 20 - gestione subfile 01
001900120622      * 21 - gestione subfile 01
002000120622      * 22 - gestione subfile 01
002100120622      * 23 - gestione subfile 01
002200120622      * 28 - messaggio errore
002400120622
002500120622      *------------------------------------------------------------------------*
002600120622      * schiere
002700120622      *------------------------------------------------------------------------*
002800120622     d msg             s             78    dim(17) ctdata perrcd(1)
002900120622
003000120622      *------------------------------------------------------------------------*
003100120622      * campi di work
003200120622      *------------------------------------------------------------------------*
003300120622     d data_eur        s               d   datfmt(*eur)
003400120622     d blanks          s                   like(d1descopz)
003500120622     d comand          s              1
003600120622     d opzio           s              2
003700120622     d ricez           s              1
003800120622     d len             s              5u 0
003900120622     d s01nrr          s              4  0
004000120622     d savopzione      s                   like(s1opzione)
004100120622     d savrcdnbr       s                   like(c1rcdnbr)
004200120622     d savtbeftt       s                   like(tbeftt)
004300120622     d savtbeflt       s                   like(tbeflt)
004400120622     d savtbeuni       s                   like(tbeuni)
004500120622     d kfil            s                   like(orgfil)
004600120622     d wrkeofs01       s              1
004700120622     d wrkcars01       s              1
004800120622     d wrkcarw01       s              1
004900120622     d wrksfl02        s              1
005000120622     d $video          s             10
005100120622     d i               s              3  0 inz
005200120622     d w0030           s              3  0 inz
005300120622
005400120622      *------------------------------------------------------------------------*
005500120622      * ds interne/esterne
005600120622      *------------------------------------------------------------------------*
005700120622     d campocm         ds           252
005800120622     d  D01CM1
005900120622     d  D01CM2
006000120622     d  D01CM3
006100120622     d  D01CM4
006200120622     d  D01CM5
006300120622     d  D01CM6
006400120622     d  D01CM7
006500120622     d  D01CM8
006600120622     d  D01CM9
006700120622     d  D01C10
006800120622     d  D01C11
006900120622     d  D01C12
007000120622     d  D01C13
007100120622     d  D01C14
007200120622     d  D01C15
007300120622     d  D01C16
007400120622     d  D01C17
007500120622     d  D01C18
007600120622     d  D01C19
007700120622     d  D01C20
007800120622     d  D01C21
007900120622     d  D01C22
008000120622     d  D01C23
008100120622     d  D01C24
008200120622     d  D01C25
008300120622     d  D01C26
008400120622     d  D01C27
008500120622     d  D01C28
008600120622     d  D01C29
008700120622     d  D01C30
008800120622     d  D01C31
008900120622     d  D01C32
009000120622     d  D01C33
009100120622     d  D01C34
009200120622     d  D01C35
009300120622     d  D01C36
014300120622     D  Scm                    1    252    dim(36)
014400120622     d  wcm                    1    252
014500120622     d
014501120622     d TIBS69DS      E DS                  INZ
014502120622     d DS_cnaco      E DS                  extname(CNACO00F)
014503120622     d DS_cnind      E DS                  extname(CNIND00F)
014504120622     d DS_cnclp      E DS                  extname(CNCLP00F)
014505120622     d DS_fncls      E DS                  extname(FNCLS00F)
014600120622     d kpjba         e ds
014700120622     d tibs34ds      e ds                  inz
014800120622     d §azute        e ds                  extname(azute00f)
014900120622     d                                     dtaara
015000120622     d §datiute      e ds                  extname(ddatiute)
015100120622     d                                     dtaara
015200120622
015300120622     d psds           sds
015400120622     d  pgmname          *proc
015500120622
015600120622      // ? PROTOTIPI ?
015700120622      /copy gaitrasrc/srcprotopr,tibs34r
015701120622      /copy gaitrasrc/srcprotopr,tibs69r
015800120622
015900120622      *------------------------------------------------------------------------*
016000120622      * costanti
016100120622      *------------------------------------------------------------------------*
016200120622     d errore          c                   '1'
016300120622     d eseguito        c                   '0'
016400120622
016500120622      *------------------------------------------------------------------------*
016600120622
016700120622      /free
016800120622       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016900120622
017000120622       exsr sr_c01;
017100120622       exsr sr_uscita;
017200120622
017300120622       // ------------------------------------------------------------------------
017400120622       // gestione subfile
017500120622       // ------------------------------------------------------------------------
017600120622       begsr sr_c01;
017700120622
017800120622       // imposto variabili
017900120622        wrkcars01 = *on;
018000120622        $video = 'C01';
018100120622
018200120622       // inizio elaborazione subfile
018300120622         dou  $video <> 'C01';
018400120622
018500120622       // caricamento subfile
018600120622          exsr sr_cars01;
018700120622
018800120622       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
018900120622       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
019000120622       // l'ultimo tasto
019100120622          if c1csrrrn > 0;
019200120622           c1rcdnbr = c1csrrrn;
019300120622          else;
019400120622           c1rcdnbr = savrcdnbr;
019500120622          endif;
019600120622
019700120622       // se non so quale pagina visualizzare forzo pagina 1
019800120622          if c1rcdnbr < 1;
019900120622           eval c1rcdnbr = 1;
020000120622          endif;
020100120622
020200120622       // salvo il record number del subfile
020300120622          savrcdnbr = c1Rcdnbr;
020400120622
020500120622       // Emissione del subfile
020600120622          write tbflp01;
020700120622          exfmt tbflc01;
020800120622
020900120622       // controllo tasti funzionali del subfile
021000120622          select;
021100120622
021200120622       // F3=Fine
021300120622           when *inkc;
021400120622            $video = *blanks;
021500120622            ricez='C';
021600120622            exsr sr_uscita;
021700120622
021800120622       // F5=Refresh
021900120622           when *inke;
022000120622            wrkcars01 = *on;
022100120622
022200120622       // F10=Immissione
022300120622           when *inkj;
022400120622            $video = 'D01';
022500120622            comand='J';
022501120704            opzio='  ';
022600120622            exsr sr_d01;
022700120622
022800120622       // in tutti gli altri casi
022900120622           other;
023000120622       // controllo ed elaborazione scelte su subfile
023100120622            exsr sr_gestsfl;
023200120622          endsl;
023300120622
023400120622       // fine elaborazione 'C01'
023500120622         enddo;
023600120622
023700120622        endsr;
023800120622
023900120622       // ------------------------------------------------------------------------
024000120622       // caricamento subfile
024100120622       // ------------------------------------------------------------------------
024200120622       begsr sr_cars01;
024300120622
024400120622       // se è stato richiesto il caricamento del subfile
024500120622        if wrkcars01 = *on;
024600120622
024700120622       // inizializzo il subfile
024800120622         s01nrr = 0;
024900120622         *in20 = *on;
025000120622         write tbflc01;
025100120622         *in20 = *off;
025200120622         *in21 = *off;
025300120622         *in22 = *off;
025400120622         *in23 = *off;
025500120622       // imposto la chiave di posizionamento e lettura file
025600120622         tbecod = 'FLB';
025700120622       // posizionamento file
025800120622         setll tbecod tntbe01l;
025900120622       // fino a che non è fine file
026000120622         dou %eof(tntbe01l);
026100120622
026200120622       // leggo file
026300120622          reade(n) tbecod tntbe01l;
026400120622
026500120622       // fine file esco
026600120622          if %eof(tntbe01l);
026700120622           leave;
026800120622          endif;
026900120622
027000120622       // scrivo subfile
027100120622           clear s1opzione;
027200120622           exsr sr_wtrs01;
027300120622
027400120622        enddo;
027500120622
027600120622         clear opzio;
027700120622         clear comand;
027800120622
027900120622       // fine caricamento subfile
028000120622        endif;
028100120622
028200120622       // se scritto almeno un record
028300120622        if s01nrr > 0;
028400120622       // indicatore di sflend
028500120622         eval *in21 = *on;
028600120622        endif;
028700120622
028800120622       // se non scritto neanche un record
028900120622        if s01nrr = 0;
029000120622       // indicatore di sfldsp
029100120622         eval *in23 = *on;
029200120622        endif;
029300120622
029400120622       // disattivo opzione di caricamento subfile
029500120622        wrkcars01 = *off;
029600120622
029700120622       endsr;
029800120622
029900120622       // ------------------------------------------------------------------------
030000120622       // gestione videata
030100120622       // ------------------------------------------------------------------------
030200120622       begsr sr_d01;
030300120622
030400120622       // imposto il video a seconda della funzione richiesta
030500120622        exsr sr_setvideo;
030600120622
030700120622       // imposto variabile
030800120622        $video = 'D01';
030900120622
031000120622       // fino a che la variabile resta settata come 'D01'
031100120622        dou $video <> 'D01';
031200120622
031300120622         *in02 = *off;
031400120622         *in03 = *off;
031500120622         *in04 = *off;
031600120622
031700120622       // se immessa opzione di  'cancellazione/ripristino'
031800120622       // proteggo i campi del video
031900120622          if opzio = '04';
032000120622           *in03 = *on;
032100120622          endif;
032200120622          if opzio = '05';
032300120622           *in03 = *on;
032400120622           *in02 = *on;
032500120622          endif;
032600120622       // se immessa opzione di  manutenzione
032700120622       // proteggo i campi del video che costituiscono kla chiave
032800120622          if opzio = '02';
032900120622           *in04 = *on;
033000120622          endif;
033100120622
033200120622       // emetto il video
033300120622          exfmt tbfld01;
033400120622
033500120622       // reset indicatore generico di errore
033600120622         *in28 = *off;
033700120622
033800120622       // pulisco il campo messaggi
033900120622         clear vd1msg;
034000120622
034100120622       // controllo tasti funzionali del video
034200120622         select;
034300120622
034400120622       // F3=Fine
034500120622          when *inkc;
034600120622           ricez='C';
034700120622           $video = *blanks;
034800120622           unlock tntbe01l;
034900120622           exsr sr_uscita;
035000120622
035100120622       // F6=Conferma
035200120622          when *inkf;
035300120622          ricez='F';
035400120622       // controllo e decodifico i dati del video
035500120622           exsr sr_ctrd01;
035600120622       // conferma per annullo/ripristino
035700120622           if opzio = '04';
035800120622            select;
035900120622       // annullamento
036000120622             when tbeatb = *blanks;
036100120622              tbeatb = 'A';
036200120622       // ripristino
036300120622             when tbeatb = 'A';
036400120622              clear tbeatb;
036500120622            endsl;
036600120622           endif;
036700120622
036800120622       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
036900120622           if not *in28;
037000120622            wrkcarw01 = *on;
037100120622            $video = 'W01';
037200120622            exsr sr_w01;
037300120622           endif;
037400120622
037500120622       // F12=Ritorno
037600120622          when *inkl;
037700120622           clear s1opzione;
037800120622           ricez='L';
037900120622           unlock tntbe01l;
038000120622       // se non è f12 da immissione non ricarico il subfile
038100120622           if comand = 'J';
038200120622            wrkcars01 = *on;
038300120622           else;
038400120622            wrkcars01 = *off;
038500120622           endif;
038600120622           $video = 'C01';
038700120622
038800120622       // Invio
038900120622          other;
039000120622           if not *in03;
039100120622            exsr sr_ctrd01;
039200120622           endif;
039300120622
039400120622         endsl;
039500120622
039600120622       // fine gestione 'D01'
039700120622        enddo;
039800120622
039900120622       endsr;
040000120622
040100120622       // ------------------------------------------------------------------------
040200120622       // imposto i dati a video
040300120622       // ------------------------------------------------------------------------
040400120622       begsr sr_setvideo;
040500120622
040600120622       // pulisco il formato video D01
040700120622        exsr sr_puld01;
040800120622
040900120622       // controllo se 'immissione' o altro
041000120622        select;
041100120622
041200120622       // F10=Immissione
041300120622         when comand = 'J';
041400120622          d1descopz = 'Immissione';
041500120622
041600120622       // Opzione "04"=cancellazione/ripristino
041700120622         when opzio = '04';
041800120622          exsr sr_imposta;
041900120622       // se richiesta 'cancellazione'
042000120622          if tbeatb = *blanks;
042100120622           d1descopz = 'Annullamento';
042200120622          endif;
042300120622       // se richiesto 'ripristino'
042400120622          if tbeatb = 'A';
042500120622           d1descopz = 'Ripristino';
042600120622          endif;
042700120622       // Opzione "05"=visualizzazione o "02"=manutenzione
042800120704         when opzio = '05' or opzio='02' ;
042801120704          if opzio= '05';
042802120704           d1descopz = 'Visualizzazione';
042803120704          else;
042804120704           d1descopz = 'Manutenzione';
042805120704          endif;
042900120622          exsr sr_imposta;
043000120622
043100120622       // Fine scelta
043200120622        endsl;
043300120622
043400120622       // centro la descrizione della funzione nel formato video
043500120622        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
043600120622        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
043700120622
043800120622       endsr;
043900120622
044000120622       // ------------------------------------------------------------------------
044100120622       // controllo video
044200120622       // ------------------------------------------------------------------------
044300120622       begsr sr_ctrd01;
044400120622
044500120622        *in28 = *off;
044600120622        *in40 = *off;
044700120622        *in41 = *off;
044800120622        *in42 = *off;
044900120622
045000120622       // Linea di arrivo obbligatoria
045100120622        if d01lna=*zeros;
045200120622         vd1msg = msg(01);
045300120622         *in28 = *on;
045400120622         *in40 = *on;
045500120622         leavesr;
045600120622        endif;
045700120622
045800120622       // errore se non è una lna
045900120622        chain d01lna azorg01l;
046000120622        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
046100120622           vd1msg = msg(01);
046200120622           *in28 = *on;
046300120622           *in40 = *on;
046400120622           leavesr;
046500120622        endif;
046600120622        d01lnad=orgdes;
046700120622
046800120622       // Linea di arrivo forzata obbligatoria
046900120622        if d01lfz=*zeros;
047000120622         vd1msg = msg(01);
047100120622         *in28 = *on;
047200120622         *in41 = *on;
047300120622         leavesr;
047400120622        endif;
047500120622
047600120622       // errore se non è una lna
047700120622        chain d01lfz azorg01l;
047800120622        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
047900120622           vd1msg = msg(01);
048000120622           *in28 = *on;
048100120622           *in41 = *on;
048200120622           leavesr;
048300120622        endif;
048400120622        d01lfzd=orgdes;
048500120622
048600120622       // Codici clienti
048800120622        for I=1 to %elem(scm);
048900120622           if scm(i)<>*zeros and scm(i)<>*blanks ;
049000120622              // può contenere solo numeri
049100120622      /end-free
049200120622     C                   testn                   scm(i)               30
049300120622    2c                   if             *in30  = *off
049400120622     c                             or  %subst(scm(i): 3: 1) < *zero
049500120622     c                   eval      vd1msg = msg(02)
049600120622     c                   eval      %subst(vd1msg:16:7)=scm(i)
049700120622     c                   eval      *in28 = *on
049800120622     c                   eval      *in42 = *on
049900120622     c                   leavesr
050000120622    2c                   endif
050100120622      /free
050101120622           I69kac=%dec(scm(i):7:0);
050102120622           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
050103120622              if o69err<>*blanks;
050500120622                 vd1msg = msg(02);
050600120622                 eval %subst(vd1msg:16:7)=scm(i);
050700120622                 *in28 = *on;
050800120622                 *in42 = *on;
050900120622                 leavesr;
051000120622              endif;
051100120622           // controllo che il codice non sia ripetuto
051200120622              if i>1 ;
051300120622                 if %lookup(scm(i):scm:1:(i-1))>0;
051400120622                    vd1msg = msg(03);
051500120622                    eval %subst(vd1msg:16:7)=scm(i);
051600120622                    *in28 = *on;
051700120622                    *in42 = *on;
051800120622                    leavesr;
051900120622                 endif;
052000120622              endif;
052100120622           endif;
052101120704           if scm(i)=*zeros;
052102120704              scm(i)='   ' ;
052103120704           endif;
052200120622        endfor;
052300120622
052400120622       // In immissione la chiave non deve essere già presente
052500120622        if comand = 'J';
052600120622         tbecod = 'FLB';
052700120622         tbeke1 = %editc(d01lna:'X');
052800120622         tbeke2 = %editc(d01lfz:'X');
052900120622         chain(n) (tbecod:tbeke1:tbeke2) tntbe01l;
053000120622         if %found(tntbe01l);
053100120622            vd1msg = msg(05);
053200120622            *in28 = *on;
053300120622            *in40 = *on;
053400120622            leavesr;
053500120622         endif;
053600120622        endif;
053700120622
053800120622       endsr;
053900120622
054000120622       // ------------------------------------------------------------------------
054100120622       // gestione video dati di trasmissione
054200120622       // ------------------------------------------------------------------------
054300120622       begsr sr_w01;
054400120622
054500120622       // imposto i dati a video
054600120622        exsr sr_carw01;
054700120622
054800120622       // fino a che la variabile resta settata come 'W01'
054900120622        dou $video <> 'W01';
055000120622
055100120622       // reset indicatore generico di errore
055200120622         *in28 = *off;
055300120622
055400120622       // emetto il video
055500120622          exfmt tbflw01;
055600120622
055700120622       // controllo tasti funzionali del video
055800120622         select;
055900120622
056000120622       // F6=Conferma
056100120622          when *inkf;
056200120622       // controllo i dati del video
056300120622           exsr sr_ctrw01;
056400120622       // se non riscontrati errori aggiorno il record corrente
056500120622           if not *in28;
056600120622            exsr sr_aggiorna;
056700120622            if not *in28;
056800120622             if comand = 'J';
056900120622              $video = 'D01';
057000120622              exsr sr_setvideo;
057100120622             else;
057200120622              $video = 'C01';
057300120622             endif;
057400120622            endif;
057500120622           endif;
057600120622
057700120622       // F12=Ritorno
057800120622          when *inkl;
057900120622           $video = 'D01';
058000120622           clear ricez;
058100120622
058200120622       // Invio
058300120622          other;
058400120622           exsr sr_ctrw01;
058500120622
058600120622         endsl;
058700120622
058800120622       // fine gestione 'W01'
058900120622        enddo;
059000120622
059100120622       endsr;
059200120622
059300120622       // ------------------------------------------------------------------------
059400120622       // imposto i dati di trasmissione
059500120622       // ------------------------------------------------------------------------
059600120622       begsr sr_carw01;
059700120622
059800120622       // se pulisco i campi
059900120622         clear w1ftt;
060000120622         clear w1flt;
060100120622         clear w1ftr;
060200120622         clear w1dtr;
060300120622
060400120622       // se non immissione imposto i campi del file
060500120622        if comand <> 'J';
060600120622         w1ftt = tbeftt;
060700120622         w1flt = tbeflt;
060800120622         w1ftr = tbeftr;
060900120622       // imposto la data
061000120622         if tbedtr <> *zeros;
061100120622          data_eur=%date(tbedtr:*iso);
061200120622          w1dtr=%dec(data_eur);
061300120622         endif;
061400120622
061500120622        endif;
061600120622
061700120622       endsr;
061800120622
061900120622       // ------------------------------------------------------------------------
062000120622       // controllo i dati di trasmissione
062100120622       // ------------------------------------------------------------------------
062200120622       begsr sr_ctrw01;
062300120622
062400120622       endsr;
062500120622
062600120622       // ------------------------------------------------------------------------
062700120622       // aggiorno tabella
062800120622       // ------------------------------------------------------------------------
062900120622       begsr sr_aggiorna;
063000120622
063100120622       // imposto campi del file
063200120622        tbeke1 = %editc(d01lna:'X');
063300120622        tbeke2 = %editc(d01lfz:'X');
063400120622
063500120622
063600120622        tbeftt = w1ftt;
063700120622        tbeflt = w1flt;
063800120622        clear tbeftr;
063900120622        clear tbedtr;
064000120622        exec sql SET :wcm = REPLACE(:wcm, ' ' , '');
064100120622        tbeuni=campocm;
064200120622
064300120622       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
064400120622        select;
064500120622
064600120622       // F10=immissione -
064700120622         when comand = 'J';
064800120622          clear tbelin;
064900120622          clear tbeatb;
065000120622          write tntbe000;
065100120622
065200120622       // Opzione "04"=cancellazione/ripristino
065300120622       // Opzione "02"=manutenzione
065400120622         when opzio = '04' or opzio='02';
065500120622          update tntbe000;
065600120622
065700120622        endsl;
065800120622
065900120622       endsr;
066000120622
066100120622       // ------------------------------------------------------------------------
066200120622       // pulizia video
066300120622       // ------------------------------------------------------------------------
066400120622       begsr sr_puld01;
066500120622
066600120622        clear d1descopz;
066700120622        clear d01lna  ;
066800120622        clear d01lnad ;
066900120622        clear d01lfz  ;
067000120622        clear d01lfzd ;
067100120622        clear campocm ;
067200120622
067300120622        *in06 = *off;
067400120622        *in28 = *off;
067500120622        *in40 = *off;
067600120622        *in41 = *off;
067700120622        *in42 = *off;
067800120622
067900120622       endsr;
068000120622
068100120622       // ------------------------------------------------------------------------
068200120622       // imposto i dati a video
068300120622       // ------------------------------------------------------------------------
068400120622       begsr sr_imposta;
068500120622
068600120622       *in06 =*off    ;
068700120622
068800120622       // recupero i dati dal file
068900120622        chain ('FLB':s1tbeke1:s1tbeke2) tntbe01l;
069000120622        if %found(tntbe01l);
069100120622           campocm=tbeuni;
069200120622        endif;
069300120622
069400120622       // imposto i campi a video
069500120622        d01lna = %dec(s1tbeke1:3:0);
069600120622        d01lnad= s1dlna;
069700120622        d01lfz = %dec(s1tbeke2:3:0);
069800120622        d01lfzd= s1dlfz;
069900120622        if tbeatb<>' '   ;
070000120622           *in06=*on    ;
070100120622        endif   ;
070200120622
070300120622       endsr;
070400120622
070500120622       // ------------------------------------------------------------------------
070600120622       // gestione del subfile
070700120622       // ------------------------------------------------------------------------
070800120622       begsr sr_gestsfl;
070900120622
071000120622       // set flag
071100120622        wrkeofs01 = *off;
071200120622
071300120622       // inizio lettura subfile
071400120622        dow wrkeofs01 = *off and *in21;
071500120622         readc tbfls01;
071600120622       // se fine subfile
071700120622         if %eof;
071800120622          wrkcars01 = *on;
071900120622          leave;
072000120622         endif;
072100120622       // se è stata immessa un'opzione
072200120622         if s1opzione <> *zeros;
072300120622       // controllo ed elaborazione opzione immessa
072400120622          select;
072500120622       // opzione "04"=annullo/ripristino
072600120622           when s1opzione = 4;
072700120622            opzio = '04';
072800120622       // opzione "02"=manutenzione
072900120622           when s1opzione = 2;
073000120622            opzio = '02';
073001120704            if s1tbeatb='Ann';
073002120704               clear opzio;
073003120704               clear s1opzione;
073005120704               update tbfls01;
073006120704            endif;
073100120622       // opzione "05"=Visualizzazione
073200120622           when s1opzione = 5;
073300120622            opzio = '05';
073400120622          endsl;
073500120622
073600120622       // se immessa almeno un opzione valida
073700120622          if  opzio <> *blanks;
073800120622       // gstione del formato video
073900120622           exsr sr_d01;
074000120622       // se la gestione si è chiusa con ...
074100120622           select;
074200120622       // F6=Conferma
074300120622            when ricez = 'F';
074400120622             clear s1opzione;
074500120622             wrkcars01 = *on;
074600120622       // F12=Ritorno
074700120622            when ricez = 'L';
074800120622             clear s1opzione;
074900120622             wrkeofs01 = *on;
075000120622       // altrimenti
075100120622            other;
075200120622             *in22 = *on;
075300120622           endsl;
075400120622
075500120622           update tbfls01;
075600120622           *in22 = *off;
075700120622       // fine opzione
075800120622          endif;
075900120622       // fine opzione
076000120622         endif;
076100120622
076200120622        enddo;
076300120622
076400120622       endsr;
076500120622
076600120622
076700120622       // ------------------------------------------------------------------------
076800120622       // scrivo subfile
076900120622       // ------------------------------------------------------------------------
077000120622       begsr sr_wtrs01;
077100120622
077200120622       // se non raggiunto limite massimo di caricamento
077300120622        if s01nrr < 9999;
077400120622       // imposto campi di subfile
077500120622         exsr sr_sets01;
077501120704         if s1tbeke1<>*blanks;
077600120622       // imposto numeratore righe e numero relativo di record
077700120622         s01nrr = s01nrr + 1;
077800120622       // scrivo subfile
077900120622         write tbfls01;
078000120622        endif;
078001120704        endif;
078100120622
078200120622       endsr;
078300120622
078400120622       // ------------------------------------------------------------------------
078500120622       // imposto campi del subfile
078600120622       // ------------------------------------------------------------------------
078700120622       begsr sr_sets01;
078800120622
078900120622        s1tbeke1 = tbeke1;
079000120622        s1tbeke2 = tbeke2;
079100120622        clear s1dlna;
079200120622        clear s1dlfz;
079300120622        // Decodifica delle due linee di arrivo
079301120704      /end-free
079302120704     C                   testn                   s1tbeke1             30
079303120704    2c                   if             *in30  = *off
079304120704     c                             or  %subst(s1tbeke1: 3: 1) < *zero
079305120704     c                   clear                   s1tbeke1
079306120704     c                   leavesr
079307120704     c                   endif
079308120704      /free
079400120622        kfil=%dec(s1tbeke1:3:0);
079500120622        chain (kfil) azorg01l;
079600120622        s1dlna=orgdes;
079700120622        kfil=%dec(s1tbeke2:3:0);
079800120622        chain (kfil) azorg01l;
079900120622        s1dlfz=orgdes;
080001120704        if tbeatb='A';
080002120704           s1tbeatb = 'Ann';
080003120704        else;
080004120704           clear s1tbeatb;
080005120704        endif;
080100120622
080200120622       endsr;
080300120622
080400120622       // ------------------------------------------------------------------------
080500120622       // routine iniziale
080600120622       // ------------------------------------------------------------------------
080700120622         begsr *inzsr;
080800120622
080900120622      /end-free
081000120622
081100120622     c     *entry        plist
081200120622     c                   parm                    kpjba
081300120622
081400120622      /free
081500120622         in(e) §azute;
081600120622         if not %error;
081700120622          in(e) §datiute;
081800120622         endif;
081900120622         if %error or rsut = *blanks;
082000120622          tibs34r(tibs34ds);
082100120622          in §azute;
082200120622          in §datiute;
082300120622         endif;
082400120622
082500120622         endsr;
082600120622
082700120622       // ------------------------------------------------------------------------
082800120622       // uscita dal programma
082900120622       // ------------------------------------------------------------------------
083000120622         begsr sr_uscita;
083100120622
083200120622          *inlr = *on;
083300120622          return;
083400120622
083500120622         endsr;
083600120622
083700120622      /end-free
083800120622
083900120622** -MSG-                                                                     *
084000120622Linea di arrivo mancante o errata                                              01
084100120622Codice cliente xxxxxxx mancante o errato                                       02
084200120622Codcie cliente xxxxxxx indicato più volte                                      03
084300120622
084400120622Chiave già presente in tabella                                                 05
