000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300090311      //-----------------------------------------------------------------------
000400090311      // Creazione bolle per addebito invio immagini a CLI
000500090311      //-----------------------------------------------------------------------
000600090310     ftilac03l  uf   e           k disk
000700090303     ffnrig00f  o    e             disk
000800090311
000900090311      //-----------------------------------------------------------------------
001000090313      //  Definizione variabili.
001100090311      //-----------------------------------------------------------------------
001200090313     d impbolla        s                   like(lacimp)
001300090313     d impimg          s                   like(lacimp)
001400090313     d nrbolle         s              2  0
001500090313     d nrimg           s             10i 0
001600090312     d nrlac           s             10i 0
001700090311     d nrweek          s              2  0
001800090317     d oldidjob        s                   like(lacidjob)
001900090317     d oldidl          s                   like(lacidl)
002000090312     d restoncl        s             10i 0
002100090313     d savidjob        s                   like(lacidjob)
002200090317     d savncl          s                   like(rigncl)
002300090312     d xx              s              2  0
002400090311     d wdatiso         s               d
002500090311     d widjob          s                   like(lacidjob)
002600090313     d widl            s                   like(lacidl)
002700090313     d wimp            s                   like(rigimp)
002800090312     d wlacaddms       s                   like(lacaddms)
002900090313     d wlacela         s              2  0
003000090313     d wmonth          s              2  0
003100090313     d wncl            s                   like(rigncl)
003200050318     d wora            s              6  0
003300050318     d woggi           s              8  0
003400090318     d woggiiso        s               d
003500090311     d wtadu           s                   like(lactadu)
003600090312     d wyear           s              4  0
003700090313     d $add            s               n
003800090313     d $fine           s               n
003900090311     d $finelac        s               n
004000090311
004100090311      //-----------------------------------------------------------------------
004200090313      //  Definizione strutture dati.
004300090311      //-----------------------------------------------------------------------
004400090317     d dvpo          e ds
004500090317     d kpjba         e ds
004600090317     d tibs02ds      e ds
004700090313
004800090313      //-----------------------------------------------------------------------
004900090313      //  Definizione costanti.
005000090313      //----------------------------------------------------------------------
005100090313      // - Costante per controllo "caratteri solo numerici"
005200090313     d c_Digits        c                   const('0123456789')
005300090317
005400090317      //--------------------------------------------------------------
005500090317      // Definizione procedure usate.                                 ?
005600090317      //--------------------------------------------------------------
005700090317      /copy gaitrasrc/srcprotopr,tibs02r
005800090313
005900090311      //-----------------------------------------------------------------------
006000090311      // RIEPILOGO INDICATORI
006100090311      //-----------------------------------------------------------------------
006200040511
006300090311      //-----------------------------------------------------------------------
006400090311     c     *entry        plist
006500090311     c                   parm                    kpjba
006600090311
006700090311      /free
006800090317
006900090317       // n.colli possbili per addebito pod image a riepilogo
007000090317       clear tibs02ds;
007100090317       clear dvpo;
007200090317       t02tla = 'L';
007300090317       t02mod = 'C';
007400090317       t02sif = knsif;
007500090317       t02cod = 'VPO';
007600090317       t02ke1 = 'VPO';
007700090317       TNTBE_RicercaControllo(kpjba : tibs02ds);
007800090317       if t02err = *blank;
007900090317         dvpo = t02uni;
008000090317       else;
008100090320         §vpopod = 5000;
008200090317       endif;
008300090311
008400090313       // data/ora del job
008500090313       woggi = %dec(%date());
008600090313       wora  = %dec(%time());
008700090318       woggiiso = %date(woggi);
008800090311
008900090311       $fine = *off;
009000090311
009100090311       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009200090402
009300090402       // prima di tutto devo rimettere a '11' i rcd che hanno la spia add.
009400090402       // perchè vuol dire che non devo rifare l'addebito, sono stati messi
009500090402       // a '01' da laguna dopo la creazione di FNRIG.
009600090402       // Caso di: oggi scannerizzo, rcd a '00', laguna lo passa a '01' e addebito
009700090402       //          lo passa a '11', domani riscannerizzo, nuovo rcd a '00', laguna
009800090402       //          aggiorna tutti i rcd con stessa spedizione a '01', passa il pgm
009900090402       //          dell'addebito e mi fa 2 addebiti, non va bene, uno lo ha già fatto
010000090402       //          ieri, con la spia evitiamo il doppio addebito
010100090402       exec sql
010200090402         update tilac00f
010300090402           set lacela = '11'
010400090402           where
010500090402           lactimecd <> ' ' and lacela > '00' and lacela < '10';
010600090311
010700090313       // leggo tilac00f raggruppato per id job e ksc + ctr
010800090311       exec sql
010900090313         declare idjob cursor for select distinct lacidjob, lacidl,
011000090313                                                  lactadu
011100090402           from tilac03l where lacidjob <> ' '
011200090401                         and lacela <> '33'
011300090313           group by lacidjob, lacidl, lactadu;
011400090311       exec sql
011500090311         open idjob;
011600090311             if sqlcode < 0;
011700090311               $fine = *on;
011800090311             ENDIF;
011900090311
012000090311       dow not $fine;
012100090311
012200090313         exsr sr_leggijob;
012300090311
012400090311       ENDDO;
012500090311
012600090311       *inlr = *on;
012700090311
012800090313       //-----------------------------------------------------------------------
012900090313       // Leggo tutti i Job di Tilac
013000090313       //-----------------------------------------------------------------------
013100090311       begsr sr_leggijob;
013200090311
013300090311         exec sql
013400090313           fetch next from idjob into :widjob, :widl, :wtadu;
013500090311
013600090313       // fine file esco
013700090311               if sqlcod = 100 or sqlcod < 0;
013800090311                 $fine = *on;
013900090311                 leavesr;
014000090311               ENDIF;
014100090311
014200090313       // a rottura di job
014300090313         if widjob <> savidjob;
014400090313           clear nrlac;
014500090313
014600090313       // per il job letto controllo se ci sono ancora dei rcd a stato '00' o
014700090317       // con timer maggiore uguale oggi
014800090313           savidjob = widjob;
014900090313           exec sql
015000090313             select count(*) into: nrlac from tilac00f
015100090401               where lacidjob = :widjob and lacela <> '33' and
015200090402               (lacela = '00' or
015300090402                substr(lactim, 1, 8) >= digits(:woggi));
015400090313           if sqlcod <> 0;
015500090313             nrlac = 1;
015600090313             leavesr;
015700090313           ENDIF;
015800090313           if nrlac > 0;
015900090313             leavesr;
016000090313           ENDIF;
016100090316
016200090313       // se stesso job e non ok tutti i rcd non devo elaborare
016300090313         else;
016400090313           if nrlac > 0;
016500090313             leavesr;
016600090313           ENDIF;
016700090313
016800090313         ENDIF;
016900090313
017000090313       // se tutto ok elaboro, se arrivo qua vuol dire che il job è completo
017100090313       // anche se ho + ksc e ctr per lo stesso job
017200090313         exsr sr_elabora;
017300090311
017400090311       ENDSR;
017500090303
017600090313       //-----------------------------------------------------------------------
017700090313       // Elaboro il Job e ksc+ctr
017800090313       //-----------------------------------------------------------------------
017900090311       begsr sr_elabora;
018000090313
018100090316         clear nrimg;
018200090316
018300090316       // conto quante immagini devo impostare nella spedizione di addebito
018400090316         exec sql
018500090402           select count(*) into: nrimg from tilac03l
018600090402             where lacidjob = :widjob and lacidl = :widl;
018700090316           if sqlcod <> 0;
018800090316             leavesr;
018900090316           ENDIF;
019000090311
019100090313       // leggo tilac per JOB/ksc+ctr (LACIDL)
019200090311         exsr sr_leggilac;
019300090311
019400090311       ENDSR;
019500090311
019600090313       //-----------------------------------------------------------------------
019700090313       // Leggo TILAC per Job e ksc+ctr
019800090313       //-----------------------------------------------------------------------
019900090311       begsr sr_leggilac;
020000090313
020100090313        $finelac = *off;
020200090311
020300090313       // Leggo TILAC
020400090313        setll (widjob:widl) tilac03l;
020500090311
020600090311        dow not $finelac;
020700090313          reade (widjob:widl) tilac03l;
020800090311          if %eof(tilac03l);
020900090311            $finelac = *on;
021000090311            leave;
021100090311          ENDIF;
021200090317
021300090317       // scrivo fnrig se richiesto
021400090317          select;
021500090317            when lactad = 'N';
021600090330         // Aggiorno TILAC
021700090330            exsr sr_agglac;
021800090317
021900090317       // se frequenza addebito per Immagine scrivo un rcd di FNRIG x ogni immagine
022000090317            when lactadu = 'I';
022100090317             wncl = 1;
022200090317             wimp = lacimp;
022300090317             exsr sr_wrtrig;
022400090330         // Aggiorno TILAC
022500090330            exsr sr_agglac;
022600090317
022700090317       // se frequenza addebito a Job/gg o Settimana o Mese calcolo quanti rcd di
022800090317       // FNRIG devo scrivere
022900090317            when lactadu <> 'I';
023000090317       // per id job e ksc+ctr
023100090317              if lacidjob <> oldidjob or lacidl <> oldidl;
023200090401                $add = *off;
023300090317                exsr sr_conta;
023400090317                oldidjob = lacidjob;
023500090317                oldidl   = lacidl;
023600090317              endif;
023700090401           // Aggiorno TILAC
023800090401           // se scritto FNRIG
023900090401              if $add;
024000090401                exsr sr_agglac;
024100090401              endif;
024200090317
024300090317          endsl;
024400090311        ENDDO;
024500090317
024600090317        clear oldidjob;
024700090317        clear oldidl;
024800090311
024900090311       ENDSR;
025000090311
025100090313       //-----------------------------------------------------------------------
025200090317       // Conta quanti rcd bolle devo creare e i relativi colli + importo
025300090313       //-----------------------------------------------------------------------
025400090317       begsr sr_conta;
025500090317
025600090317         clear nrbolle;
025700090317         clear restoncl;
025800090317         clear wncl;
025900090317         clear wimp;
026000090317         clear impimg;
026100090317         clear savncl;
026200090317
026300090317       // conto quante bolle e i colli per bolla
026400090317         select;
026500090320       // controllo che il n. dei colli non superi il max consentito 5000
026600090320       // se supera devo fare più bolle
026700090317           when nrimg > §vpopod;
026800090317             nrbolle  = %div(nrimg:§vpopod);
026900090317             restoncl = %rem(nrimg:§vpopod);
027000090317             wncl = §vpopod;
027100090317             savncl = §vpopod;
027200090317           when nrimg <= §vpopod;
027300090317             nrbolle = 1;
027400090317             wncl = nrimg;
027500090317         endsl;
027600090317       // se ho resto aggiungo 1 al n. di bolle da creare
027700090317         if restoncl > 0;
027800090317           nrbolle = nrbolle + 1;
027900090317         endif;
028000090317
028100090317       // calcolo l'importo per bolla
028200090317         select;
028300090317       // se devo creare più bolle devo ripartire l'importo forzato a totale
028400090317           when nrbolle > 1 and lacfimp = 'T' and lacimp <> *zeros;
028500090317             impimg = lacimp / nrimg;
028600090317       // se devo creare 1 bolla imposto l'importo forzato a totale
028700090317           when nrbolle = 1 and lacfimp = 'T' and lacimp <> *zeros;
028800090317             wimp = lacimp;
028900090317       // se l'importo forzato è a immagine imposto l'importo a immagine
029000090317           when lacfimp = 'I' and lacimp <> *zeros;
029100090317             impimg = lacimp;
029200090317          ENDSL;
029300090312
029400090317       // scrive FNRIG in base alle frequenza addebito
029500090317         xx = 1;
029600090312
029700090317         select;
029800090313       // addebito per job scrivo un solo rcd di fnrig
029900090313       // con il totale dei rcd letti da tilac
030000090317           when lactadu = 'J';
030100090313
030200090317       // --> se ho più di 90000 o 5000 immagini
030300090317             if nrbolle > 1;
030400090313       // --  devo ciclare per il n. di bolle da creare
030500090317               for xx by 1 to nrbolle;
030600090313       // --  importo a totale forzato
030700090317                 if impimg > *zeros and lacfimp = 'T';
030800090317                   wimp = impimg * wncl;
030900090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
031000090313       //       devo calcolare il totale
031100090317                   if xx = nrbolle and restoncl = 0;
031200090317                     wimp = lacimp - (impimg * savncl * (nrbolle -1));
031300090317                   endif;
031400090317                 endif;
031500090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
031600090317       //       imposto i colli
031700090317                 if xx = nrbolle and restoncl > 0;
031800090317                   wncl = restoncl;
031900090317       //       se importo a totale forzato calcolo il totale
032000090317                   if impimg > *zeros and lacfimp = 'T';
032100090317                     wimp = lacimp - (impimg * savncl * (nrbolle-1));
032200090317                   endif;
032300090317                 endif;
032400090313       // ----  importo a immagine forzato
032500090317                 if impimg > *zeros and lacfimp = 'I';
032600090317                   wimp = impimg * wncl;
032700090317                 endif;
032800090313       // ----  scrivo addebito
032900090317                 exsr sr_wrtrig;
033000090317               ENDFOR;
033100090317             ENDIF;
033200090313
033300090313       // --> ho una sola bolla da creare
033400090317             if nrbolle = 1;
033500090317               wncl = nrimg;
033600090317               if impimg > *zeros and lacfimp = 'T';
033700090317                 wimp = lacimp;
033800090317               endif;
033900090317               if impimg > *zeros and lacfimp = 'I';
034000090317                 wimp = impimg * wncl;
034100090317               endif;
034200090313       // --  scrivo addebito
034300090317               exsr sr_wrtrig;
034400090317             ENDIF;
034500090311
034600090313
034700090313       // addebito per settimana scrivo un solo rcd di fnrig
034800090313       // con il totale dei rcd letti da tilac
034900090317           when lactadu = 'S';
035000090313       // se sono nella settimana sucessiva a quella prevista per l'addebito
035100090317             exec sql
035200090318               set :nrweek = week_iso(:woggiiso);
035300090318             wyear = %subdt(woggiiso:*years);
035400090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(nrweek:'X'):6:0);
035500090317             if lacaddms < wlacaddms;
035600090313
035700090313       // --> se ho elaborato più di 9000 immagini
035800090317               if nrbolle > 1;
035900090313       // --  devo ciclare per il n. di bolle da creare
036000090317                 for xx by 1 to nrbolle;
036100090313       // --  importo a totale forzato
036200090317                   if impimg > *zeros and lacfimp = 'T';
036300090317                     wimp = impimg * wncl;
036400090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
036500090313       //       devo calcolare il totale
036600090317                     if xx = nrbolle and restoncl = 0;
036700090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
036800090317                     endif;
036900090317                   endif;
037000090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
037100090317       //       imposto i colli
037200090317                   if xx = nrbolle and restoncl > 0;
037300090317                     wncl = restoncl;
037400090317       //       importo a totale forzato calcolo il totale
037500090317                     if impimg > *zeros and lacfimp = 'T';
037600090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
037700090317                     endif;
037800090317                   endif;
037900090313       // ----  importo a immagine forzato
038000090317                   if impimg > *zeros and lacfimp = 'I';
038100090317                     wimp = impimg * wncl;
038200090317                   endif;
038300090313       // ----  scrivo addebito
038400090317                   exsr sr_wrtrig;
038500090317                 ENDFOR;
038600090317               ENDIF;
038700090313
038800090313       // --> ho una sola bolla da creare
038900090317               if nrbolle = 1;
039000090317                 wncl = nrimg;
039100090317                 if impimg > *zeros and lacfimp = 'T';
039200090317                   wimp = lacimp;
039300090317                 endif;
039400090317                 if impimg > *zeros and lacfimp = 'I';
039500090317                   wimp = impimg * wncl;
039600090317                 endif;
039700090313       // --  scrivo addebito
039800090317                 exsr sr_wrtrig;
039900090317               ENDIF;
040000090317             ENDIF;
040100090313
040200090311
040300090313       // addebito per mese scrivo un solo rcd di fnrig
040400090313       // con il totale dei rcd letti da tilac
040500090317           when lactadu = 'M';
040600090313       // se sono nel mese sucessivo a quella previsto per l'addebito
040700090318             wyear  = %subdt(woggiiso:*years);
040800090318             wmonth = %subdt(woggiiso:*months);
040900090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(wmonth:'X'):6:0);
041000090317             if lacaddms < wlacaddms;
041100090313
041200090313       // --> se ho elaborato più di 9000 immagini
041300090317               if nrbolle > 1;
041400090313       // --  devo ciclare per il n. di bolle da creare
041500090317                 for xx by 1 to nrbolle;
041600090313       // --  importo a totale forzato
041700090317                   if impimg > *zeros and lacfimp = 'T';
041800090317                     wimp = impimg * wncl;
041900090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
042000090313       //       devo calcolare il totale
042100090317                     if xx = nrbolle and restoncl = 0;
042200090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
042300090317                     endif;
042400090317                   endif;
042500090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
042600090317       //       imposto i colli
042700090317                   if xx = nrbolle and restoncl > 0;
042800090317                     wncl = restoncl;
042900090317       //       importo a totale forzato
043000090317                     if impimg > *zeros and lacfimp = 'T';
043100090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
043200090317                     endif;
043300090317                   endif;
043400090313       // ----  importo a immagine forzato
043500090317                   if impimg > *zeros and lacfimp = 'I';
043600090317                     wimp = impimg * wncl;
043700090317                   endif;
043800090313       // ----  scrivo addebito
043900090317                   exsr sr_wrtrig;
044000090317                 ENDFOR;
044100090317               ENDIF;
044200090313
044300090313       // --> ho una sola bolla da creare
044400090317               if nrbolle = 1;
044500090317                 wncl = nrimg;
044600090317                 if impimg > *zeros and lacfimp = 'T';
044700090317                   wimp = lacimp;
044800090317                 endif;
044900090317                 if impimg > *zeros and lacfimp = 'I';
045000090317                   wimp = impimg * wncl;
045100090317                 endif;
045200090313       // --  scrivo addebito
045300090317                 exsr sr_wrtrig;
045400090317               ENDIF;
045500090317             ENDIF;
045600090317         ENDSL;
045700090313
045800090311       ENDSR;
045900090313
046000090313       //-----------------------------------------------------------------------
046100090313       // Controllo se e come srivere FNRIG
046200090313       //-----------------------------------------------------------------------
046300090313       begsr sr_wrtrig;
046400090330
046500090330         $add = *on;
046600090313
046700090313         clear fnrig000;
046800090313
046900090316       // spedizione solo se addebito ad immagine
047000090316         if lactadu = 'I';
047100090316           rigaas = 2000 + %dec(lacaas:2:0);
047200090316           riglnp = %dec(laclnp:3:0);
047300090316           rignrs = %dec(lacnrs:2:0);
047400090316           rignsp = %dec(lacnsp:7:0);
047500090316         ENDIF;
047600090313
047700090313       // cliente tassazione + tariffa
047800090313       // il cliente forzato + la tariffa sono memorizzati nel campo lacidl
047900090313       // prima però delle nuove modifiche questo campo conteneva il nome del lavoro che
048000090313       // scrive TILAC, per riconoscere i nuovi rcd dai vecchi uso il campo LACTELA che è nuovo
048100090313       // e viene impostato con le nuove modifiche
048200090313         if lactela <> *blanks;
048300090313       // per sicurezza comunque controllo anche il campo LACIDL
048400090317           if %check(c_digits:lacidl) = 0 and lacidl <> *blanks;
048500090313             rigksc = %dec(%subst(lacidl:1:7):7:0);
048600090313             rigctr = %subst(lacidl:8:3);
048700090313           ENDIF;
048800090313         ENDIF;
048900090313
049000090313        // tipo tassazione
049100090313         if lactad = 'S';
049200090313           rigtas = 'T';
049300090313         ENDIF;
049400090313         if lactad = 'V';
049500090313           rigtas = 'N';
049600090313         ENDIF;
049700090313
049800090313        // tassazione forzata
049900090313         if wimp > *zeros;
050000090313           rigimp = wimp;
050100090313           rigtas = 'F';
050200090313         endif;
050300090313
050400090313         rigcau = 'P';
050500090313         rigncl = wncl;
050600090313         rigtra = 'N';
050700090313         rigpru = 'TILAC';
050800090313         rigdata = woggi;
050900090313         rigora = wora;
051000090317
051100090320       // tassazione mensile memorizzo la data di spedizione
051200090320       // ultimo gg del mese precedente
051300090318         if lactadu = 'M';
051400090318           rigdsp = lacaddms*100+1;
051500090318           wdatiso = %date(rigdsp) + %months(1) - %days(1);
051600090318           rigdsp = %dec(wdatiso);
051700090317         endif;
051800090317
051900090313         write fnrig000;
052000090313
052100090313       ENDSR;
052200090311
052300090313       //-----------------------------------------------------------------------
052400090313       // Aggiorno TILAC
052500090313       //-----------------------------------------------------------------------
052600090313       begsr sr_agglac;
052700090313
052800090313       // se lacela non è più a '01' non aggiorno
052900090313         if lacela > '00' and lacela < '10';
053000090313           wlacela = %dec(lacela:2:0);
053100090313           wlacela = wlacela + 10;
053200090313           lacela = %editc(wlacela:'X');
053300090402        // imposto anche data spia add.
053400090402           lactimecd = %char(%timestamp:*iso0);
053500090313           update tilac000;
053600090313         ENDIF;
053700090313
053800090313       ENDSR;
053900090303
