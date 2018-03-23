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
009200090311
009300090313       // leggo tilac00f raggruppato per id job e ksc + ctr
009400090311       exec sql
009500090313         declare idjob cursor for select distinct lacidjob, lacidl,
009600090313                                                  lactadu
009700090311           from tilac00f where lacidjob <> ' '
009800090313           group by lacidjob, lacidl, lactadu;
009900090311       exec sql
010000090311         open idjob;
010100090311             if sqlcode < 0;
010200090311               $fine = *on;
010300090311             ENDIF;
010400090311
010500090311       dow not $fine;
010600090311
010700090313         exsr sr_leggijob;
010800090311
010900090311       ENDDO;
011000090311
011100090311       *inlr = *on;
011200090311
011300090313       //-----------------------------------------------------------------------
011400090313       // Leggo tutti i Job di Tilac
011500090313       //-----------------------------------------------------------------------
011600090311       begsr sr_leggijob;
011700090311
011800090311         exec sql
011900090313           fetch next from idjob into :widjob, :widl, :wtadu;
012000090311
012100090313       // fine file esco
012200090311               if sqlcod = 100 or sqlcod < 0;
012300090311                 $fine = *on;
012400090311                 leavesr;
012500090311               ENDIF;
012600090311
012700090313       // a rottura di job
012800090313         if widjob <> savidjob;
012900090313           clear nrlac;
013000090313
013100090313       // per il job letto controllo se ci sono ancora dei rcd a stato '00' o
013200090317       // con timer maggiore uguale oggi
013300090313           savidjob = widjob;
013400090313           exec sql
013500090313             select count(*) into: nrlac from tilac00f
013600090313               where lacidjob = :widjob and (lacela = '00' or
013700090317               lacela > '09' or
013800090317               substr(lactim, 1, 8) >= digits(:woggi));
013900090313           if sqlcod <> 0;
014000090313             nrlac = 1;
014100090313             leavesr;
014200090313           ENDIF;
014300090313           if nrlac > 0;
014400090313             leavesr;
014500090313           ENDIF;
014600090316
014700090313       // se stesso job e non ok tutti i rcd non devo elaborare
014800090313         else;
014900090313           if nrlac > 0;
015000090313             leavesr;
015100090313           ENDIF;
015200090313
015300090313         ENDIF;
015400090313
015500090313       // se tutto ok elaboro, se arrivo qua vuol dire che il job è completo
015600090313       // anche se ho + ksc e ctr per lo stesso job
015700090313         exsr sr_elabora;
015800090311
015900090311       ENDSR;
016000090303
016100090313       //-----------------------------------------------------------------------
016200090313       // Elaboro il Job e ksc+ctr
016300090313       //-----------------------------------------------------------------------
016400090311       begsr sr_elabora;
016500090313
016600090316         clear nrimg;
016700090316
016800090316       // conto quante immagini devo impostare nella spedizione di addebito
016900090316         exec sql
017000090316           select count(*) into: nrimg from tilac00f
017100090316             where lacidjob = :widjob and lacidl = :widl;
017200090316           if sqlcod <> 0;
017300090316             leavesr;
017400090316           ENDIF;
017500090311
017600090313       // leggo tilac per JOB/ksc+ctr (LACIDL)
017700090311         exsr sr_leggilac;
017800090311
017900090311       ENDSR;
018000090311
018100090313       //-----------------------------------------------------------------------
018200090313       // Leggo TILAC per Job e ksc+ctr
018300090313       //-----------------------------------------------------------------------
018400090311       begsr sr_leggilac;
018500090313
018600090313        $add = *off;
018700090313        $finelac = *off;
018800090311
018900090313       // Leggo TILAC
019000090313        setll (widjob:widl) tilac03l;
019100090311
019200090311        dow not $finelac;
019300090313          reade (widjob:widl) tilac03l;
019400090311          if %eof(tilac03l);
019500090311            $finelac = *on;
019600090311            leave;
019700090311          ENDIF;
019800090317
019900090317       // scrivo fnrig se richiesto
020000090317          select;
020100090317            when lactad = 'N';
020200090317
020300090317       // se frequenza addebito per Immagine scrivo un rcd di FNRIG x ogni immagine
020400090317            when lactadu = 'I';
020500090317             wncl = 1;
020600090317             wimp = lacimp;
020700090317             exsr sr_wrtrig;
020800090317
020900090317       // se frequenza addebito a Job/gg o Settimana o Mese calcolo quanti rcd di
021000090317       // FNRIG devo scrivere
021100090317            when lactadu <> 'I';
021200090317       // per id job e ksc+ctr
021300090317              if lacidjob <> oldidjob or lacidl <> oldidl;
021400090317                exsr sr_conta;
021500090317                oldidjob = lacidjob;
021600090317                oldidl   = lacidl;
021700090317              endif;
021800090317
021900090317          endsl;
022000090311
022100090313       // Aggiorno TILAC
022200090313            exsr sr_agglac;
022300090311        ENDDO;
022400090317
022500090317        clear oldidjob;
022600090317        clear oldidl;
022700090311
022800090311       ENDSR;
022900090311
023000090313       //-----------------------------------------------------------------------
023100090317       // Conta quanti rcd bolle devo creare e i relativi colli + importo
023200090313       //-----------------------------------------------------------------------
023300090317       begsr sr_conta;
023400090317
023500090317         clear nrbolle;
023600090317         clear restoncl;
023700090317         clear wncl;
023800090317         clear wimp;
023900090317         clear impimg;
024000090317         clear savncl;
024100090317
024200090317       // conto quante bolle e i colli per bolla
024300090317         select;
024400090320       // controllo che il n. dei colli non superi il max consentito 5000
024500090320       // se supera devo fare più bolle
024600090317           when nrimg > §vpopod;
024700090317             nrbolle  = %div(nrimg:§vpopod);
024800090317             restoncl = %rem(nrimg:§vpopod);
024900090317             wncl = §vpopod;
025000090317             savncl = §vpopod;
025100090317           when nrimg <= §vpopod;
025200090317             nrbolle = 1;
025300090317             wncl = nrimg;
025400090317         endsl;
025500090317       // se ho resto aggiungo 1 al n. di bolle da creare
025600090317         if restoncl > 0;
025700090317           nrbolle = nrbolle + 1;
025800090317         endif;
025900090317
026000090317       // calcolo l'importo per bolla
026100090317         select;
026200090317       // se devo creare più bolle devo ripartire l'importo forzato a totale
026300090317           when nrbolle > 1 and lacfimp = 'T' and lacimp <> *zeros;
026400090317             impimg = lacimp / nrimg;
026500090317       // se devo creare 1 bolla imposto l'importo forzato a totale
026600090317           when nrbolle = 1 and lacfimp = 'T' and lacimp <> *zeros;
026700090317             wimp = lacimp;
026800090317       // se l'importo forzato è a immagine imposto l'importo a immagine
026900090317           when lacfimp = 'I' and lacimp <> *zeros;
027000090317             impimg = lacimp;
027100090317          ENDSL;
027200090312
027300090317       // scrive FNRIG in base alle frequenza addebito
027400090317         xx = 1;
027500090312
027600090317         select;
027700090313       // addebito per job scrivo un solo rcd di fnrig
027800090313       // con il totale dei rcd letti da tilac
027900090317           when lactadu = 'J';
028000090313
028100090317       // --> se ho più di 90000 o 5000 immagini
028200090317             if nrbolle > 1;
028300090313       // --  devo ciclare per il n. di bolle da creare
028400090317               for xx by 1 to nrbolle;
028500090313       // --  importo a totale forzato
028600090317                 if impimg > *zeros and lacfimp = 'T';
028700090317                   wimp = impimg * wncl;
028800090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
028900090313       //       devo calcolare il totale
029000090317                   if xx = nrbolle and restoncl = 0;
029100090317                     wimp = lacimp - (impimg * savncl * (nrbolle -1));
029200090317                   endif;
029300090317                 endif;
029400090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
029500090317       //       imposto i colli
029600090317                 if xx = nrbolle and restoncl > 0;
029700090317                   wncl = restoncl;
029800090317       //       se importo a totale forzato calcolo il totale
029900090317                   if impimg > *zeros and lacfimp = 'T';
030000090317                     wimp = lacimp - (impimg * savncl * (nrbolle-1));
030100090317                   endif;
030200090317                 endif;
030300090313       // ----  importo a immagine forzato
030400090317                 if impimg > *zeros and lacfimp = 'I';
030500090317                   wimp = impimg * wncl;
030600090317                 endif;
030700090313       // ----  scrivo addebito
030800090317                 exsr sr_wrtrig;
030900090317               ENDFOR;
031000090317             ENDIF;
031100090313
031200090313       // --> ho una sola bolla da creare
031300090317             if nrbolle = 1;
031400090317               wncl = nrimg;
031500090317               if impimg > *zeros and lacfimp = 'T';
031600090317                 wimp = lacimp;
031700090317               endif;
031800090317               if impimg > *zeros and lacfimp = 'I';
031900090317                 wimp = impimg * wncl;
032000090317               endif;
032100090313       // --  scrivo addebito
032200090317               exsr sr_wrtrig;
032300090317             ENDIF;
032400090311
032500090313
032600090313       // addebito per settimana scrivo un solo rcd di fnrig
032700090313       // con il totale dei rcd letti da tilac
032800090317           when lactadu = 'S';
032900090313       // se sono nella settimana sucessiva a quella prevista per l'addebito
033000090317             exec sql
033100090318               set :nrweek = week_iso(:woggiiso);
033200090318             wyear = %subdt(woggiiso:*years);
033300090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(nrweek:'X'):6:0);
033400090317             if lacaddms < wlacaddms;
033500090313
033600090313       // --> se ho elaborato più di 9000 immagini
033700090317               if nrbolle > 1;
033800090313       // --  devo ciclare per il n. di bolle da creare
033900090317                 for xx by 1 to nrbolle;
034000090313       // --  importo a totale forzato
034100090317                   if impimg > *zeros and lacfimp = 'T';
034200090317                     wimp = impimg * wncl;
034300090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
034400090313       //       devo calcolare il totale
034500090317                     if xx = nrbolle and restoncl = 0;
034600090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
034700090317                     endif;
034800090317                   endif;
034900090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
035000090317       //       imposto i colli
035100090317                   if xx = nrbolle and restoncl > 0;
035200090317                     wncl = restoncl;
035300090317       //       importo a totale forzato calcolo il totale
035400090317                     if impimg > *zeros and lacfimp = 'T';
035500090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
035600090317                     endif;
035700090317                   endif;
035800090313       // ----  importo a immagine forzato
035900090317                   if impimg > *zeros and lacfimp = 'I';
036000090317                     wimp = impimg * wncl;
036100090317                   endif;
036200090313       // ----  scrivo addebito
036300090317                   exsr sr_wrtrig;
036400090317                 ENDFOR;
036500090317               ENDIF;
036600090313
036700090313       // --> ho una sola bolla da creare
036800090317               if nrbolle = 1;
036900090317                 wncl = nrimg;
037000090317                 if impimg > *zeros and lacfimp = 'T';
037100090317                   wimp = lacimp;
037200090317                 endif;
037300090317                 if impimg > *zeros and lacfimp = 'I';
037400090317                   wimp = impimg * wncl;
037500090317                 endif;
037600090313       // --  scrivo addebito
037700090317                 exsr sr_wrtrig;
037800090317               ENDIF;
037900090317             ENDIF;
038000090313
038100090311
038200090313       // addebito per mese scrivo un solo rcd di fnrig
038300090313       // con il totale dei rcd letti da tilac
038400090317           when lactadu = 'M';
038500090313       // se sono nel mese sucessivo a quella previsto per l'addebito
038600090318             wyear  = %subdt(woggiiso:*years);
038700090318             wmonth = %subdt(woggiiso:*months);
038800090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(wmonth:'X'):6:0);
038900090317             if lacaddms < wlacaddms;
039000090313
039100090313       // --> se ho elaborato più di 9000 immagini
039200090317               if nrbolle > 1;
039300090313       // --  devo ciclare per il n. di bolle da creare
039400090317                 for xx by 1 to nrbolle;
039500090313       // --  importo a totale forzato
039600090317                   if impimg > *zeros and lacfimp = 'T';
039700090317                     wimp = impimg * wncl;
039800090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
039900090313       //       devo calcolare il totale
040000090317                     if xx = nrbolle and restoncl = 0;
040100090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
040200090317                     endif;
040300090317                   endif;
040400090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
040500090317       //       imposto i colli
040600090317                   if xx = nrbolle and restoncl > 0;
040700090317                     wncl = restoncl;
040800090317       //       importo a totale forzato
040900090317                     if impimg > *zeros and lacfimp = 'T';
041000090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
041100090317                     endif;
041200090317                   endif;
041300090313       // ----  importo a immagine forzato
041400090317                   if impimg > *zeros and lacfimp = 'I';
041500090317                     wimp = impimg * wncl;
041600090317                   endif;
041700090313       // ----  scrivo addebito
041800090317                   exsr sr_wrtrig;
041900090317                 ENDFOR;
042000090317               ENDIF;
042100090313
042200090313       // --> ho una sola bolla da creare
042300090317               if nrbolle = 1;
042400090317                 wncl = nrimg;
042500090317                 if impimg > *zeros and lacfimp = 'T';
042600090317                   wimp = lacimp;
042700090317                 endif;
042800090317                 if impimg > *zeros and lacfimp = 'I';
042900090317                   wimp = impimg * wncl;
043000090317                 endif;
043100090313       // --  scrivo addebito
043200090317                 exsr sr_wrtrig;
043300090317               ENDIF;
043400090317             ENDIF;
043500090317         ENDSL;
043600090313
043700090311       ENDSR;
043800090313
043900090313       //-----------------------------------------------------------------------
044000090313       // Controllo se e come srivere FNRIG
044100090313       //-----------------------------------------------------------------------
044200090313       begsr sr_wrtrig;
044300090313
044400090313         clear fnrig000;
044500090313
044600090316       // spedizione solo se addebito ad immagine
044700090316         if lactadu = 'I';
044800090316           rigaas = 2000 + %dec(lacaas:2:0);
044900090316           riglnp = %dec(laclnp:3:0);
045000090316           rignrs = %dec(lacnrs:2:0);
045100090316           rignsp = %dec(lacnsp:7:0);
045200090316         ENDIF;
045300090313
045400090313       // cliente tassazione + tariffa
045500090313       // il cliente forzato + la tariffa sono memorizzati nel campo lacidl
045600090313       // prima però delle nuove modifiche questo campo conteneva il nome del lavoro che
045700090313       // scrive TILAC, per riconoscere i nuovi rcd dai vecchi uso il campo LACTELA che è nuovo
045800090313       // e viene impostato con le nuove modifiche
045900090313         if lactela <> *blanks;
046000090313       // per sicurezza comunque controllo anche il campo LACIDL
046100090317           if %check(c_digits:lacidl) = 0 and lacidl <> *blanks;
046200090313             rigksc = %dec(%subst(lacidl:1:7):7:0);
046300090313             rigctr = %subst(lacidl:8:3);
046400090313           ENDIF;
046500090313         ENDIF;
046600090313
046700090313        // tipo tassazione
046800090313         if lactad = 'S';
046900090313           rigtas = 'T';
047000090313         ENDIF;
047100090313         if lactad = 'V';
047200090313           rigtas = 'N';
047300090313         ENDIF;
047400090313
047500090313        // tassazione forzata
047600090313         if wimp > *zeros;
047700090313           rigimp = wimp;
047800090313           rigtas = 'F';
047900090313         endif;
048000090313
048100090313         rigcau = 'P';
048200090313         rigncl = wncl;
048300090313         rigtra = 'N';
048400090313         rigpru = 'TILAC';
048500090313         rigdata = woggi;
048600090313         rigora = wora;
048700090317
048800090320       // tassazione mensile memorizzo la data di spedizione
048900090320       // ultimo gg del mese precedente
049000090318         if lactadu = 'M';
049100090318           rigdsp = lacaddms*100+1;
049200090318           wdatiso = %date(rigdsp) + %months(1) - %days(1);
049300090318           rigdsp = %dec(wdatiso);
049400090317         endif;
049500090317
049600090313         write fnrig000;
049700090313
049800090313       ENDSR;
049900090311
050000090313       //-----------------------------------------------------------------------
050100090313       // Aggiorno TILAC
050200090313       //-----------------------------------------------------------------------
050300090313       begsr sr_agglac;
050400090313
050500090313       // se lacela non è più a '01' non aggiorno
050600090313         if lacela > '00' and lacela < '10';
050700090313           wlacela = %dec(lacela:2:0);
050800090313           wlacela = wlacela + 10;
050900090313           lacela = %editc(wlacela:'X');
051000090313           update tilac000;
051100090313         ENDIF;
051200090313
051300090313       ENDSR;
051400090303
