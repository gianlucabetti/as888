000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300120622      * Gestione tabella "FLB" = Forzatura linea di arrivo per cliente mittente*
000400060427      *------------------------------------------------------------------------*
000500060505     ftntbe01l  uf a e           k disk
000600090424     fazorg01l  if   e           k disk
000700120615     ftntbflbd  cf   e             workstn sfile(tbfls01:s01nrr)
000800120706     f                                     sfile(tbfls02:s02nrr)
000900120622
001000120622      *------------------------------------------------------------------------*
001100120622      * riepilogo indicatori
001200120622      *------------------------------------------------------------------------*
001300120622      * 02 - visualizzazione
001400120622      * 03 - proteggo campi video 01
001500120622      * 04 - manutenzione
001600120622      * 20 - gestione subfile 01
001700120622      * 21 - gestione subfile 01
001800120622      * 22 - gestione subfile 01
001900120622      * 23 - gestione subfile 01
002000120706      * 24 - gestione subfile 02
002100120706      * 25 - gestione subfile 02
002200120706      * 26 - gestione subfile 02
002300120706      * 27 - gestione subfile 02
002400120622      * 28 - messaggio errore
002500120706      * 40/42Errori campi subfile/control
002600120622
002700120622      *------------------------------------------------------------------------*
002800120622      * schiere
002900120622      *------------------------------------------------------------------------*
003000120622     d msg             s             78    dim(17) ctdata perrcd(1)
003100120622
003200120622      *------------------------------------------------------------------------*
003300120622      * campi di work
003400120622      *------------------------------------------------------------------------*
003500120622     d data_eur        s               d   datfmt(*eur)
003600120622     d blanks          s                   like(d1descopz)
003700120622     d comand          s              1
003800120622     d opzio           s              2
003900120622     d ricez           s              1
004000120622     d len             s              5u 0
004100120622     d s01nrr          s              4  0
004200120706     d s02nrr          s              4  0
004300120622     d savopzione      s                   like(s1opzione)
004400120622     d savrcdnbr       s                   like(c1rcdnbr)
004500120706     d sv2rcdnbr       s                   like(c2rcdnbr)
004600120622     d savtbeftt       s                   like(tbeftt)
004700120622     d savtbeflt       s                   like(tbeflt)
004800120622     d savtbeuni       s                   like(tbeuni)
004900120622     d kfil            s                   like(orgfil)
005000120622     d wrkeofs01       s              1
005100120622     d wrkcars01       s              1
005200120622     d wrkcarw01       s              1
005300120622     d wrksfl02        s              1
005400120622     d $video          s             10
005500120622     d i               s              3  0 inz
005600120622     d w0030           s              3  0 inz
005700120622
005800120622      *------------------------------------------------------------------------*
005900120622      * ds interne/esterne
006000120622      *------------------------------------------------------------------------*
006100120706     d campocm         ds           252
006200120706     D  Scm                    1    252    dim(36)
006300120706     d campocms01      ds            21
006400120706     d  s01cm1
006500120706     d  s01CM2
006600120706     d  s01CM3
006700120706     d* s01CM4
006800120706     d* s01CM5
006900120706     d* s01CM6
007000120706     d* s01CM7
007100120622     d
007200120622     d TIBS69DS      E DS                  INZ
007300120622     d DS_cnaco      E DS                  extname(CNACO00F)
007400120622     d DS_cnind      E DS                  extname(CNIND00F)
007500120622     d DS_cnclp      E DS                  extname(CNCLP00F)
007600120622     d DS_fncls      E DS                  extname(FNCLS00F)
007700120622     d kpjba         e ds
007800120622     d tibs34ds      e ds                  inz
007900120622     d §azute        e ds                  extname(azute00f)
008000120622     d                                     dtaara
008100120622     d §datiute      e ds                  extname(ddatiute)
008200120622     d                                     dtaara
008300120622
008400120622     d psds           sds
008500120622     d  pgmname          *proc
008600120622
008700120622      // ? PROTOTIPI ?
008800120622      /copy gaitrasrc/srcprotopr,tibs34r
008900120622      /copy gaitrasrc/srcprotopr,tibs69r
009000120622
009100120622      *------------------------------------------------------------------------*
009200120622      * costanti
009300120622      *------------------------------------------------------------------------*
009400120622     d errore          c                   '1'
009500120622     d eseguito        c                   '0'
009600120622
009700120622      *------------------------------------------------------------------------*
009800120622
009900120622      /free
010000120622
010100120622       exsr sr_c01;
010200120622       exsr sr_uscita;
010300120622
010400120622       // ------------------------------------------------------------------------
010500120622       // gestione subfile
010600120622       // ------------------------------------------------------------------------
010700120622       begsr sr_c01;
010800120622
010900120622       // imposto variabili
011000120622        wrkcars01 = *on;
011100120622        $video = 'C01';
011200120622
011300120622       // inizio elaborazione subfile
011400120622         dou  $video <> 'C01';
011500120622
011600120622       // caricamento subfile
011700120622          exsr sr_cars01;
011800120622
011900120622       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
012000120622       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
012100120622       // l'ultimo tasto
012200120622          if c1csrrrn > 0;
012300120622           c1rcdnbr = c1csrrrn;
012400120622          else;
012500120622           c1rcdnbr = savrcdnbr;
012600120622          endif;
012700120622
012800120622       // se non so quale pagina visualizzare forzo pagina 1
012900120622          if c1rcdnbr < 1;
013000120622           eval c1rcdnbr = 1;
013100120622          endif;
013200120622
013300120622       // salvo il record number del subfile
013400120622          savrcdnbr = c1Rcdnbr;
013500120622
013600120622       // Emissione del subfile
013700120622          write tbflp01;
013800120622          exfmt tbflc01;
013900120622
014000120622       // controllo tasti funzionali del subfile
014100120622          select;
014200120622
014300120622       // F3=Fine
014400120622           when *inkc;
014500120622            $video = *blanks;
014600120622            ricez='C';
014700120622            exsr sr_uscita;
014800120622
014900120622       // F5=Refresh
015000120622           when *inke;
015100120622            wrkcars01 = *on;
015200120622
015300120622       // F10=Immissione
015400120622           when *inkj;
015500120622            $video = 'D01';
015600120622            comand='J';
015700120704            opzio='  ';
015800120706            exsr sr_c02;
015900120622
016000120622       // in tutti gli altri casi
016100120622           other;
016200120622       // controllo ed elaborazione scelte su subfile
016300120622            exsr sr_gestsfl;
016400120622          endsl;
016500120622
016600120622       // fine elaborazione 'C01'
016700120622         enddo;
016800120622
016900120622        endsr;
017000120622
017100120622       // ------------------------------------------------------------------------
017200120622       // caricamento subfile
017300120622       // ------------------------------------------------------------------------
017400120622       begsr sr_cars01;
017500120622
017600120622       // se è stato richiesto il caricamento del subfile
017700120622        if wrkcars01 = *on;
017800120622
017900120622       // inizializzo il subfile
018000120622         s01nrr = 0;
018100120622         *in20 = *on;
018200120622         write tbflc01;
018300120622         *in20 = *off;
018400120622         *in21 = *off;
018500120622         *in22 = *off;
018600120622         *in23 = *off;
018700120622       // imposto la chiave di posizionamento e lettura file
018800120622         tbecod = 'FLB';
018900120622       // posizionamento file
019000120622         setll tbecod tntbe01l;
019100120622       // fino a che non è fine file
019200120622         dou %eof(tntbe01l);
019300120622
019400120622       // leggo file
019500120622          reade(n) tbecod tntbe01l;
019600120622
019700120622       // fine file esco
019800120622          if %eof(tntbe01l);
019900120622           leave;
020000120622          endif;
020100120622
020200120622       // scrivo subfile
020300120622           clear s1opzione;
020400120622           exsr sr_wtrs01;
020500120622
020600120622        enddo;
020700120622
020800120622         clear opzio;
020900120622         clear comand;
021000120622
021100120622       // fine caricamento subfile
021200120622        endif;
021300120622
021400120622       // se scritto almeno un record
021500120622        if s01nrr > 0;
021600120622       // indicatore di sflend
021700120622         eval *in21 = *on;
021800120622        endif;
021900120622
022000120622       // se non scritto neanche un record
022100120622        if s01nrr = 0;
022200120622       // indicatore di sfldsp
022300120622         eval *in23 = *on;
022400120622        endif;
022500120622
022600120622       // disattivo opzione di caricamento subfile
022700120622        wrkcars01 = *off;
022800120622
022900120622       endsr;
023000120622       // ------------------------------------------------------------------------
023100120622       // gestione video dati di trasmissione
023200120622       // ------------------------------------------------------------------------
023300120622       begsr sr_w01;
023400120622
023500120622       // imposto i dati a video
023600120622        exsr sr_carw01;
023700120622
023800120622       // fino a che la variabile resta settata come 'W01'
023900120622        dou $video <> 'W01';
024000120622
024100120622       // reset indicatore generico di errore
024200120622         *in28 = *off;
024300120622
024400120622       // emetto il video
024500120622          exfmt tbflw01;
024600120622
024700120622       // controllo tasti funzionali del video
024800120622         select;
024900120622
025000120622       // F6=Conferma
025100120622          when *inkf;
025200120622       // controllo i dati del video
025300120622           exsr sr_ctrw01;
025400120622       // se non riscontrati errori aggiorno il record corrente
025500120622           if not *in28;
025600120622            exsr sr_aggiorna;
025700120622            if not *in28;
025800120622             if comand = 'J';
025900120706              $video = 'C02';
026000120706              exsr sr_setc02;
026100120706              exsr sr_cars02;
026200120622             else;
026300120622              $video = 'C01';
026400120622             endif;
026500120622            endif;
026600120622           endif;
026700120622
026800120622       // F12=Ritorno
026900120622          when *inkl;
027000120706           $video = 'C02';
027100120622           clear ricez;
027200120622
027300120622       // Invio
027400120622          other;
027500120622           exsr sr_ctrw01;
027600120622
027700120622         endsl;
027800120622
027900120622       // fine gestione 'W01'
028000120622        enddo;
028100120622
028200120622       endsr;
028300120622
028400120622       // ------------------------------------------------------------------------
028500120622       // imposto i dati di trasmissione
028600120622       // ------------------------------------------------------------------------
028700120622       begsr sr_carw01;
028800120622
028900120622       // se pulisco i campi
029000120622         clear w1ftt;
029100120622         clear w1flt;
029200120622         clear w1ftr;
029300120622         clear w1dtr;
029400120622
029500120622       // se non immissione imposto i campi del file
029600120622        if comand <> 'J';
029700120622         w1ftt = tbeftt;
029800120622         w1flt = tbeflt;
029900120622         w1ftr = tbeftr;
030000120622       // imposto la data
030100120622         if tbedtr <> *zeros;
030200120622          data_eur=%date(tbedtr:*iso);
030300120622          w1dtr=%dec(data_eur);
030400120622         endif;
030500120622
030600120622        endif;
030700120622
030800120622       endsr;
030900120622
031000120622       // ------------------------------------------------------------------------
031100120622       // controllo i dati di trasmissione
031200120622       // ------------------------------------------------------------------------
031300120622       begsr sr_ctrw01;
031400120622
031500120622       endsr;
031600120622
031700120622       // ------------------------------------------------------------------------
031800120622       // aggiorno tabella
031900120622       // ------------------------------------------------------------------------
032000120622       begsr sr_aggiorna;
032100120622
032200120706       // imposto campi del file
032300120706
032400120622        tbeftt = w1ftt;
032500120622        tbeflt = w1flt;
032600120622        clear tbeftr;
032700120622        clear tbedtr;
032800120622        tbeuni=campocm;
032900120622
033000120622       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
033100120622        select;
033200120622
033300120622       // F10=immissione -
033400120622         when comand = 'J';
033500120706          tbeke1 = %editc(d01lna:'X');
033600120706          tbeke2 = %editc(d01lfz:'X');
033700120622          clear tbelin;
033800120622          clear tbeatb;
033900120622          write tntbe000;
034000120622
034100120622       // Opzione "04"=cancellazione/ripristino
034200120622       // Opzione "02"=manutenzione
034300120622         when opzio = '04' or opzio='02';
034400120622          update tntbe000;
034500120622
034600120622        endsl;
034700120622
034800120622       endsr;
034900120622       // ------------------------------------------------------------------------
035000120622       // gestione del subfile
035100120622       // ------------------------------------------------------------------------
035200120622       begsr sr_gestsfl;
035300120622
035400120622       // set flag
035500120622        wrkeofs01 = *off;
035600120622
035700120622       // inizio lettura subfile
035800120622        dow wrkeofs01 = *off and *in21;
035900120622         readc tbfls01;
036000120622       // se fine subfile
036100120622         if %eof;
036200120622          wrkcars01 = *on;
036300120622          leave;
036400120622         endif;
036500120622       // se è stata immessa un'opzione
036600120622         if s1opzione <> *zeros;
036700120622       // controllo ed elaborazione opzione immessa
036800120622          select;
036900120622       // opzione "04"=annullo/ripristino
037000120622           when s1opzione = 4;
037100120622            opzio = '04';
037200120622       // opzione "02"=manutenzione
037300120622           when s1opzione = 2;
037400120622            opzio = '02';
037500120704            if s1tbeatb='Ann';
037600120704               clear opzio;
037700120704               clear s1opzione;
037800120704               update tbfls01;
037900120704            endif;
038000120622       // opzione "05"=Visualizzazione
038100120622           when s1opzione = 5;
038200120622            opzio = '05';
038300120622          endsl;
038400120622
038500120622       // se immessa almeno un opzione valida
038600120622          if  opzio <> *blanks;
038700120622       // gstione del formato video
038800120706           exsr sr_c02;
038900120622       // se la gestione si è chiusa con ...
039000120622           select;
039100120622       // F6=Conferma
039200120622            when ricez = 'F';
039300120622             clear s1opzione;
039400120622             wrkcars01 = *on;
039500120622       // F12=Ritorno
039600120622            when ricez = 'L';
039700120622             clear s1opzione;
039800120622             wrkeofs01 = *on;
039900120622       // altrimenti
040000120622            other;
040100120622             *in22 = *on;
040200120622           endsl;
040300120622
040400120622           update tbfls01;
040500120622           *in22 = *off;
040600120622       // fine opzione
040700120622          endif;
040800120622       // fine opzione
040900120622         endif;
041000120622
041100120622        enddo;
041200120622
041300120622       endsr;
041400120622
041500120622
041600120622       // ------------------------------------------------------------------------
041700120622       // scrivo subfile
041800120622       // ------------------------------------------------------------------------
041900120622       begsr sr_wtrs01;
042000120622
042100120622       // se non raggiunto limite massimo di caricamento
042200120622        if s01nrr < 9999;
042300120622       // imposto campi di subfile
042400120622         exsr sr_sets01;
042500120704         if s1tbeke1<>*blanks;
042600120622       // imposto numeratore righe e numero relativo di record
042700120622         s01nrr = s01nrr + 1;
042800120622       // scrivo subfile
042900120622         write tbfls01;
043000120622        endif;
043100120704        endif;
043200120622
043300120622       endsr;
043400120622
043500120622       // ------------------------------------------------------------------------
043600120622       // imposto campi del subfile
043700120622       // ------------------------------------------------------------------------
043800120622       begsr sr_sets01;
043900120622
044000120622        s1tbeke1 = tbeke1;
044100120622        s1tbeke2 = tbeke2;
044200120622        clear s1dlna;
044300120622        clear s1dlfz;
044400120622        // Decodifica delle due linee di arrivo
044500120704      /end-free
044600120704     C                   testn                   s1tbeke1             30
044700120704    2c                   if             *in30  = *off
044800120706     c                             or  %subst(s1tbeke1: 3: 1) < *zero
044900120704     c                   clear                   s1tbeke1
045000120704     c                   leavesr
045100120704     c                   endif
045200120704      /free
045300120622        kfil=%dec(s1tbeke1:3:0);
045400170301        if kfil<999   ;
045500120622        chain (kfil) azorg01l;
045600120622        s1dlna=orgdes;
045700170301        else  ;
045800170301        s1dlna='Tutte'   ;
045900170301        endif  ;
046000120622        kfil=%dec(s1tbeke2:3:0);
046100120622        chain (kfil) azorg01l;
046200120622        s1dlfz=orgdes;
046300120704        if tbeatb='A';
046400120704           s1tbeatb = 'Ann';
046500120704        else;
046600120704           clear s1tbeatb;
046700120704        endif;
046800120706        campocms01=tbeuni;
046900120706        if %subst(tbeuni:22:7)<>*blanks and %subst(tbeuni:22:7)<>*zeros;
047000120706           s01piu='+';
047100120706        else;
047200120706           s01piu=*blanks;
047300120706        endif;
047400120706        //decodifico i clienti caricati a video
047500120706                clear tibs69ds;
047600120706                clear DS_cnaco;
047700120706                clear DS_cnind;
047800120706                clear DS_cnclp;
047900120706                clear DS_fncls;
048000120706                clear s01dm1;
048100120706                clear s01dm2;
048200120706                clear s01dm3;
048300120706           for I=1 to 3;
048400120706             select;
048500120706             when I=1;
048600120706                if s01cm1=*blanks or s01cm1=*zeros;
048700120706                  leave;
048800120706                endif;
048900120706                I69kac=%dec(s01cm1:7:0);
049000120706             when i=2;
049100120706                if s01cm2=*blanks or s01cm2=*zeros;
049200120706                  leave;
049300120706                endif;
049400120706                I69kac=%dec(s01cm2:7:0);
049500120706             when i=3;
049600120706                if s01cm3=*blanks or s01cm3=*zeros;
049700120706                  leave;
049800120706                endif;
049900120706                I69kac=%dec(s01cm3:7:0);
050000120706             endsl;
050100120706                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
050200120706                if o69err=*blanks;
050300120706             select;
050400120706             when I=1;
050500120706                   s01dm1=acorag;
050600120706             when i=2;
050700120706                   s01dm2=acorag;
050800120706             when i=3;
050900120706                   s01dm3=acorag;
051000120706             endsl;
051100120706                endif;
051200120706           endfor;
051300120622
051400120622       endsr;
051500120706       // ------------------------------------------------------------------------
051600120706       // gestione subfile 2 - dettaglio per lna/lna forzata
051700120706       // ------------------------------------------------------------------------
051800120706       begsr sr_c02;
051900120706       // imposto il video a seconda della funzione richiesta
052000120706        exsr sr_setc02;
052100120706        $video = 'C02';
052200120706       // caricamento subfile
052300120706        exsr sr_cars02;
052400120706       // inizio elaborazione subfile
052500120706         dou  $video <> 'C02';
052600120706       // c2csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
052700120706       // impostando c2rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
052800120706       // l'ultimo tasto
052900120706          if c2csrrrn > 0;
053000120706           c2rcdnbr = c2csrrrn;
053100120706          else;
053200120706           c2rcdnbr = sv2rcdnbr;
053300120706          endif;
053400120706       // se non so quale pagina visualizzare forzo pagina 1
053500120706          if c2rcdnbr < 1;
053600120706           eval c2rcdnbr = 1;
053700120706          endif;
053800120706
053900120706       // salvo il record number del subfile
054000120706          sv2rcdnbr = c2Rcdnbr;
054100120706
054200120706       // Emissione del subfile
054300120706          write tbflp02;
054400120706          exfmt tbflc02;
054500120706
054600120706       // controllo tasti funzionali del subfile
054700120706          select;
054800120706       // F3=Fine
054900120706           when *inkc;
055000120706            $video = *blanks;
055100120706            ricez='C';
055200120706            exsr sr_uscita;
055300120706       // F12=Ritorno
055400120706           when *inkl;
055500120706            ricez='L';
055600120706            unlock tntbe01l;
055700120706            $video = 'C01';
055800120706            // se non è f12 da immissione non ricarico il subfile
055900120706            if comand = 'J';
056000120706               wrkcars01 = *on;
056100120706            else;
056200120706               wrkcars01 = *off;
056300120706            endif;
056400120706       // F6=Conferma
056500120706           when *inkf;
056600120706            ricez='F';
056700120706            // controllo e decodifico i dati del video
056800120706            exsr sr_ctrs02;
056900120706            if *in28;
057000120706               *in43=*off;
057100120706            else;
057200120706               *in43=*on;
057300120706            endif;
057400120706            // conferma per annullo/ripristino
057500120706            if opzio = '04';
057600120706             select;
057700120706             // annullamento
057800120706             when tbeatb = *blanks;
057900120706              tbeatb = 'A';
058000120706             // ripristino
058100120706             when tbeatb = 'A';
058200120706              clear tbeatb;
058300120706             endsl;
058400120706            endif;
058500120706
058600120706       // se non riscontrati errori emetto la finestra con i dati per la trasmissione
058700120706            if not *in28;
058800120706             wrkcarw01 = *on;
058900120706             $video = 'W01';
059000120706             exsr sr_w01;
059100120706            endif;
059200120706       // Invio
059300120706           other;
059400120706            if not *in03;
059500120706             exsr sr_ctrs02;
059600120706            if *in28;
059700120706               *in43=*off;
059800120706            else;
059900120706               *in43=*on;
060000120706            endif;
060100120706            endif;
060200120706          endsl;
060300120706       // fine elaborazione 'C02'
060400120706         enddo;
060500120706       endsr;
060600120706       // ------------------------------------------------------------------------
060700120706       // imposto control C02
060800120706       // ------------------------------------------------------------------------
060900120706       begsr sr_setc02;
061000120706
061100120706        // pulizia campi control
061200120706        clear d1descopz;
061300120706        clear d01lna;
061400120706        clear d01lfz;
061500120706        clear d01lnad;
061600120706        clear d01lfzd;
061700120706
061800120706        *in06 = *off;
061900120706        *in28 = *off;
062000120706        *in40 = *off;
062100120706        *in41 = *off;
062200120706        *in42 = *off;
062300120706        *in43 = *on;
062400120706        *in02 = *off;
062500120706        *in03 = *off;
062600120706        *in04 = *off;
062700120706
062800120706       // controllo se 'immissione' o altro
062900120706        select;
063000120706
063100120706       // F10=Immissione
063200120706         when comand = 'J';
063300120706          d1descopz = 'Immissione';
063400120706
063500120706       // Opzione "04"=cancellazione/ripristino
063600120706         when opzio = '04';
063700120706           *in03 = *on;
063800120706       // Opzione "05"=visualizzazione
063900120706         when opzio = '05';
064000120706           *in03 = *on;
064100120706           *in02 = *on;
064200120706           d1descopz = 'Visualizzazione';
064300120706       // Opzione "02"=manutenzione
064400120706         when opzio = '02' ;
064500120706           *in04 = *on;
064600120706           d1descopz = 'Manutenzione';
064700120706        endsl;
064800120706
064900120706       // centro la descrizione della funzione nel formato video
065000120706        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
065100120706        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
065200120706
065300120706       if comand<>'J';
065400120706        d01lna=%dec(s1tbeke1:3:0);
065500120706        d01lfz=%dec(s1tbeke2:3:0);
065600120706        kfil=%dec(d01lna:3:0);
065700170301        if kfil<999  ;
065800120706        chain (kfil) azorg01l;
065900120706        d01lnad=orgdes;
066000170301        else  ;
066100170301        d01lnad='Tutte'  ;
066200170301        endif  ;
066300120706        kfil=%dec(d01lfz:3:0);
066400120706        chain (kfil) azorg01l;
066500120706        d01lfzd=orgdes;
066600120706       endif;
066700120706
066800120706       endsr;
066900120706       // ------------------------------------------------------------------------
067000120706       // caricamento subfile2 - dettaglio per lna/lna forzata
067100120706       // ------------------------------------------------------------------------
067200120706       begsr sr_cars02;
067300120706
067400120706       // inizializzo il subfile
067500120706         s02nrr = 0;
067600120706         *in24 = *on;
067700120706         write tbflc02;
067800120706         *in24 = *off;
067900120706         *in25 = *off;
068000120706         *in26 = *off;
068100120706         *in27 = *off;
068200120706         clear campocm;
068300120706       if comand<>'J';
068400120706       // recupero i dati dal file
068500120706        chain ('FLB':s1tbeke1:s1tbeke2) tntbe01l;
068600120706        if %found(tntbe01l);
068700120706           campocm=tbeuni;
068800120706           if tbeatb<>' ';
068900120706              *in06=*on    ;
069000120706           endif;
069100120706          if opzio='04';
069200120706          // se richiesta 'cancellazione'
069300120706            if tbeatb = *blanks;
069400120706             d1descopz = 'Annullamento';
069500120706            endif;
069600120706          // se richiesto 'ripristino'
069700120706            if tbeatb = 'A';
069800120706             d1descopz = 'Ripristino';
069900120706            endif;
070000120706          endif;
070100120706        endif;
070200120706       endif;
070300120706           for I=1 to 36;
070400120706              clear s02cm;
070500120706              clear s02dcm;
070600120706              *in26=*off;
070700120706              if scm(I)<>*blanks;
070800120706                s02cm=Scm(i);
070900120706                // decodifico il cliente mittente
071000120706                clear tibs69ds;
071100120706                clear DS_cnaco;
071200120706                clear DS_cnind;
071300120706                clear DS_cnclp;
071400120706                clear DS_fncls;
071500120706                I69kac=%dec(scm(i):7:0);
071600120706                tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
071700120706                if o69err=*blanks;
071800120706                   s02dcm=acorag;
071900120706                endif;
072000120706                *in26=*on;
072100120706              endif;
072200120706              // imposto numeratore righe e numero relativo di record
072300120706              s02nrr = s02nrr + 1;
072400120706              // scrivo subfile
072500120706              write tbfls02;
072600120706           endfor;
072700120706
072800120706       // se scritto almeno un record
072900120706        if s02nrr > 0;
073000120706           C2RCDNBR=1;
073100120706           c2csrrrn=1;
073200120706       // indicatore di sflend
073300120706         eval *in25 = *on;
073400120706        endif;
073500120706
073600120706       // se non scritto neanche un record
073700120706        if s02nrr = 0;
073800120706       // indicatore di sfldsp
073900120706         eval *in27 = *on;
074000120706        endif;
074100120706
074200120706       endsr;
074300120706       // ------------------------------------------------------------------------
074400120706       // controllo subfile 2 (dettaglio per linea di partenza)
074500120706       // ------------------------------------------------------------------------
074600120706       begsr sr_ctrs02;
074700120706
074800120706        *in28 = *off;
074900120706        *in40 = *off;
075000120706        *in41 = *off;
075100120706        *in42 = *off;
075200120706        clear vc2msg;
075300120706
075400120706       // Linea di arrivo obbligatoria
075500120706        if d01lna=*zeros;
075600120706         vc2msg = msg(01);
075700120706         *in28 = *on;
075800120706         *in40 = *on;
075900120706         leavesr;
076000120706        endif;
076100120706
076200120706       // errore se non è una lna
076300170301       if d01lna<999 ;
076400120706        chain d01lna azorg01l;
076500120706        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
076600120706           vc2msg = msg(01);
076700120706           *in28 = *on;
076800120706           *in40 = *on;
076900120706           leavesr;
077000120706        endif;
077100120706        d01lnad=orgdes;
077200170301        endif  ;
077300170301        if d01lna=999  ;
077400170301        d01lnad='Tutte'  ;
077500170301        endif  ;
077600120706
077700120706       // Linea di arrivo forzata obbligatoria
077800120706        if d01lfz=*zeros;
077900120706         vc2msg = msg(01);
078000120706         *in28 = *on;
078100120706         *in41 = *on;
078200120706         leavesr;
078300120706        endif;
078400120706
078500120706       // errore se non è una lna
078600120706        chain d01lfz azorg01l;
078700120706        if not %found(azorg01l) or (orgfag <>'F' and orgfag<>'A');
078800120706           vc2msg = msg(01);
078900120706           *in28 = *on;
079000120706           *in41 = *on;
079100120706           leavesr;
079200120706        endif;
079300120706        d01lfzd=orgdes;
079400120706       // In immissione la chiave non deve essere già presente
079500120706        if comand = 'J';
079600120706         tbecod = 'FLB';
079700120706         tbeke1 = %editc(d01lna:'X');
079800120706         tbeke2 = %editc(d01lfz:'X');
079900120706         chain(n) (tbecod:tbeke1:tbeke2) tntbe01l;
080000120706         if %found(tntbe01l);
080100120706            vc2msg = msg(05);
080200120706            *in28 = *on;
080300120706            *in40 = *on;
080400120706            leavesr;
080500120706         endif;
080600120706        endif;
080700120706
080800120706        clear scm;
080900120706        *in26=*on;
081000120706           readc tbfls02;
081100120706            dow not(%eof);
081200120706              clear s02dcm;
081300120706              if s02cm<>*zeros and s02cm<>*blanks;
081400120706      /end-free
081500120706     C                   testn                   s02cm                30
081600120706    2c                   if             *in30  = *off
081700120706     c                             or  %subst(s02cm: 7: 1) < *zero
081800120706     c                   eval      vc2msg = msg(02)
081900120706     c                   eval      *in28 = *on
082000120706     c                   eval      *in42 = *on
082100120706     c                   update    tbfls02
082200120706     c                   eval      c2csrrrn=s02nrr
082300120706     c                   leavesr
082400120706    2c                   endif
082500120706      /free
082600120706           clear tibs69ds;
082700120706           clear DS_cnaco;
082800120706           clear DS_cnind;
082900120706           clear DS_cnclp;
083000120706           clear DS_fncls;
083100120706           I69kac=%dec(s02cm:7:0);
083200120706           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
083300120706              if o69err<>*blanks;
083400120706                 vc2msg = msg(02);
083500120706                 *in28 = *on;
083600120706                 *in42 = *on;
083700120706                 update tbfls02;
083800120706                 c2csrrrn=s02nrr;
083900120706                 leavesr;
084000120706              endif;
084100120706                // decodifico il codice cliente
084200120706                s02dcm=acorag;
084300120706
084400120706              endif;
084500120706              if s02cm=*zeros;
084600120706                 clear s02cm;
084700120706                 clear s02dcm;
084800120706              endif;
084900120706              if s02cm<>*blanks;
085000120706                 // errore se cliente ripetuto all'interno del sfl
085100120706                 if %lookup(s02cm:scm)>0;
085200120706                    vc2msg = msg(03);
085300120706                    *in28 = *on;
085400120706                    *in42 = *on;
085500120706                    update tbfls02;
085600120706                    c2csrrrn=s02nrr;
085700120706                    leavesr;
085800120706                 else;
085900120706                 // se codice non ripetuto memorizzo in schiera
086000120706                    i=%lookup('       ':scm);
086100120706                    if i>0;
086200120706                       scm(i)=s02cm;
086300120706                    endif;
086400120706                 endif;
086500120706              endif;
086600120706                 *in26 = *on;
086700120706                 update tbfls02;
086800120706               readc tbfls02;
086900120706            enddo;
087000120706       endsr;
087100120706
087200120622
087300120622       // ------------------------------------------------------------------------
087400120622       // routine iniziale
087500120622       // ------------------------------------------------------------------------
087600120622         begsr *inzsr;
087700120622
087800120622      /end-free
087900120622
088000120622     c     *entry        plist
088100120622     c                   parm                    kpjba
088200120622
088300120622      /free
088400120622         in(e) §azute;
088500120622         if not %error;
088600120622          in(e) §datiute;
088700120622         endif;
088800120622         if %error or rsut = *blanks;
088900120622          tibs34r(tibs34ds);
089000120622          in §azute;
089100120622          in §datiute;
089200120622         endif;
089300120622
089400120622         endsr;
089500120622
089600120622       // ------------------------------------------------------------------------
089700120622       // uscita dal programma
089800120622       // ------------------------------------------------------------------------
089900120622         begsr sr_uscita;
090000120622
090100120622          *inlr = *on;
090200120622          return;
090300120622
090400120622         endsr;
090500120622
090600120622      /end-free
090700120622
090800120622** -MSG-                                                                     *
090900120622Linea di arrivo mancante o errata                                              01
091000120706Codice cliente mancante o errato                                               02
091100120706Codcie cliente indicato più volte                                              03
091200120622
091300120622Chiave già presente in tabella                                                 05
