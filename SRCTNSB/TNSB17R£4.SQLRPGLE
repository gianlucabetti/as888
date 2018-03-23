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
009800090401                         and lacela <> '33'
009900090313           group by lacidjob, lacidl, lactadu;
010000090311       exec sql
010100090311         open idjob;
010200090311             if sqlcode < 0;
010300090311               $fine = *on;
010400090311             ENDIF;
010500090311
010600090311       dow not $fine;
010700090311
010800090313         exsr sr_leggijob;
010900090311
011000090311       ENDDO;
011100090311
011200090311       *inlr = *on;
011300090311
011400090313       //-----------------------------------------------------------------------
011500090313       // Leggo tutti i Job di Tilac
011600090313       //-----------------------------------------------------------------------
011700090311       begsr sr_leggijob;
011800090311
011900090311         exec sql
012000090313           fetch next from idjob into :widjob, :widl, :wtadu;
012100090311
012200090313       // fine file esco
012300090311               if sqlcod = 100 or sqlcod < 0;
012400090311                 $fine = *on;
012500090311                 leavesr;
012600090311               ENDIF;
012700090311
012800090313       // a rottura di job
012900090313         if widjob <> savidjob;
013000090313           clear nrlac;
013100090313
013200090313       // per il job letto controllo se ci sono ancora dei rcd a stato '00' o
013300090317       // con timer maggiore uguale oggi
013400090313           savidjob = widjob;
013500090313           exec sql
013600090313             select count(*) into: nrlac from tilac00f
013700090401               where lacidjob = :widjob and lacela <> '33' and
013800090402          // (lacela = '00' or lacela > '09' or
013900090402               (lacela = '00' or
014000090317               substr(lactim, 1, 8) >= digits(:woggi));
014100090313           if sqlcod <> 0;
014200090313             nrlac = 1;
014300090313             leavesr;
014400090313           ENDIF;
014500090313           if nrlac > 0;
014600090313             leavesr;
014700090313           ENDIF;
014800090316
014900090313       // se stesso job e non ok tutti i rcd non devo elaborare
015000090313         else;
015100090313           if nrlac > 0;
015200090313             leavesr;
015300090313           ENDIF;
015400090313
015500090313         ENDIF;
015600090313
015700090313       // se tutto ok elaboro, se arrivo qua vuol dire che il job è completo
015800090313       // anche se ho + ksc e ctr per lo stesso job
015900090313         exsr sr_elabora;
016000090311
016100090311       ENDSR;
016200090303
016300090313       //-----------------------------------------------------------------------
016400090313       // Elaboro il Job e ksc+ctr
016500090313       //-----------------------------------------------------------------------
016600090311       begsr sr_elabora;
016700090313
016800090316         clear nrimg;
016900090316
017000090316       // conto quante immagini devo impostare nella spedizione di addebito
017100090316         exec sql
017200090316           select count(*) into: nrimg from tilac00f
017300090402             where lacidjob = :widjob and lacidl = :widl
017400090402             and lacela > '00' and lacela < '10';
017500090316           if sqlcod <> 0;
017600090316             leavesr;
017700090316           ENDIF;
017800090311
017900090313       // leggo tilac per JOB/ksc+ctr (LACIDL)
018000090311         exsr sr_leggilac;
018100090311
018200090311       ENDSR;
018300090311
018400090313       //-----------------------------------------------------------------------
018500090313       // Leggo TILAC per Job e ksc+ctr
018600090313       //-----------------------------------------------------------------------
018700090311       begsr sr_leggilac;
018800090313
018900090313        $finelac = *off;
019000090311
019100090313       // Leggo TILAC
019200090313        setll (widjob:widl) tilac03l;
019300090311
019400090311        dow not $finelac;
019500090313          reade (widjob:widl) tilac03l;
019600090311          if %eof(tilac03l);
019700090311            $finelac = *on;
019800090311            leave;
019900090311          ENDIF;
020000090330
020100090402       // scarto se lacela = '00' o > '09'
020200090402          if lacela = '00' or lacela > '09';
020300090402            iter;
020400090402          endif;
020500090317
020600090317       // scrivo fnrig se richiesto
020700090317          select;
020800090317            when lactad = 'N';
020900090330         // Aggiorno TILAC
021000090330            exsr sr_agglac;
021100090317
021200090317       // se frequenza addebito per Immagine scrivo un rcd di FNRIG x ogni immagine
021300090317            when lactadu = 'I';
021400090317             wncl = 1;
021500090317             wimp = lacimp;
021600090317             exsr sr_wrtrig;
021700090330         // Aggiorno TILAC
021800090330            exsr sr_agglac;
021900090317
022000090317       // se frequenza addebito a Job/gg o Settimana o Mese calcolo quanti rcd di
022100090317       // FNRIG devo scrivere
022200090317            when lactadu <> 'I';
022300090317       // per id job e ksc+ctr
022400090317              if lacidjob <> oldidjob or lacidl <> oldidl;
022500090401                $add = *off;
022600090317                exsr sr_conta;
022700090317                oldidjob = lacidjob;
022800090317                oldidl   = lacidl;
022900090317              endif;
023000090401           // Aggiorno TILAC
023100090401           // se scritto FNRIG
023200090401              if $add;
023300090401                exsr sr_agglac;
023400090401              endif;
023500090317
023600090317          endsl;
023700090311        ENDDO;
023800090317
023900090317        clear oldidjob;
024000090317        clear oldidl;
024100090311
024200090311       ENDSR;
024300090311
024400090313       //-----------------------------------------------------------------------
024500090317       // Conta quanti rcd bolle devo creare e i relativi colli + importo
024600090313       //-----------------------------------------------------------------------
024700090317       begsr sr_conta;
024800090317
024900090317         clear nrbolle;
025000090317         clear restoncl;
025100090317         clear wncl;
025200090317         clear wimp;
025300090317         clear impimg;
025400090317         clear savncl;
025500090317
025600090317       // conto quante bolle e i colli per bolla
025700090317         select;
025800090320       // controllo che il n. dei colli non superi il max consentito 5000
025900090320       // se supera devo fare più bolle
026000090317           when nrimg > §vpopod;
026100090317             nrbolle  = %div(nrimg:§vpopod);
026200090317             restoncl = %rem(nrimg:§vpopod);
026300090317             wncl = §vpopod;
026400090317             savncl = §vpopod;
026500090317           when nrimg <= §vpopod;
026600090317             nrbolle = 1;
026700090317             wncl = nrimg;
026800090317         endsl;
026900090317       // se ho resto aggiungo 1 al n. di bolle da creare
027000090317         if restoncl > 0;
027100090317           nrbolle = nrbolle + 1;
027200090317         endif;
027300090317
027400090317       // calcolo l'importo per bolla
027500090317         select;
027600090317       // se devo creare più bolle devo ripartire l'importo forzato a totale
027700090317           when nrbolle > 1 and lacfimp = 'T' and lacimp <> *zeros;
027800090317             impimg = lacimp / nrimg;
027900090317       // se devo creare 1 bolla imposto l'importo forzato a totale
028000090317           when nrbolle = 1 and lacfimp = 'T' and lacimp <> *zeros;
028100090317             wimp = lacimp;
028200090317       // se l'importo forzato è a immagine imposto l'importo a immagine
028300090317           when lacfimp = 'I' and lacimp <> *zeros;
028400090317             impimg = lacimp;
028500090317          ENDSL;
028600090312
028700090317       // scrive FNRIG in base alle frequenza addebito
028800090317         xx = 1;
028900090312
029000090317         select;
029100090313       // addebito per job scrivo un solo rcd di fnrig
029200090313       // con il totale dei rcd letti da tilac
029300090317           when lactadu = 'J';
029400090313
029500090317       // --> se ho più di 90000 o 5000 immagini
029600090317             if nrbolle > 1;
029700090313       // --  devo ciclare per il n. di bolle da creare
029800090317               for xx by 1 to nrbolle;
029900090313       // --  importo a totale forzato
030000090317                 if impimg > *zeros and lacfimp = 'T';
030100090317                   wimp = impimg * wncl;
030200090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
030300090313       //       devo calcolare il totale
030400090317                   if xx = nrbolle and restoncl = 0;
030500090317                     wimp = lacimp - (impimg * savncl * (nrbolle -1));
030600090317                   endif;
030700090317                 endif;
030800090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
030900090317       //       imposto i colli
031000090317                 if xx = nrbolle and restoncl > 0;
031100090317                   wncl = restoncl;
031200090317       //       se importo a totale forzato calcolo il totale
031300090317                   if impimg > *zeros and lacfimp = 'T';
031400090317                     wimp = lacimp - (impimg * savncl * (nrbolle-1));
031500090317                   endif;
031600090317                 endif;
031700090313       // ----  importo a immagine forzato
031800090317                 if impimg > *zeros and lacfimp = 'I';
031900090317                   wimp = impimg * wncl;
032000090317                 endif;
032100090313       // ----  scrivo addebito
032200090317                 exsr sr_wrtrig;
032300090317               ENDFOR;
032400090317             ENDIF;
032500090313
032600090313       // --> ho una sola bolla da creare
032700090317             if nrbolle = 1;
032800090317               wncl = nrimg;
032900090317               if impimg > *zeros and lacfimp = 'T';
033000090317                 wimp = lacimp;
033100090317               endif;
033200090317               if impimg > *zeros and lacfimp = 'I';
033300090317                 wimp = impimg * wncl;
033400090317               endif;
033500090313       // --  scrivo addebito
033600090317               exsr sr_wrtrig;
033700090317             ENDIF;
033800090311
033900090313
034000090313       // addebito per settimana scrivo un solo rcd di fnrig
034100090313       // con il totale dei rcd letti da tilac
034200090317           when lactadu = 'S';
034300090313       // se sono nella settimana sucessiva a quella prevista per l'addebito
034400090317             exec sql
034500090318               set :nrweek = week_iso(:woggiiso);
034600090318             wyear = %subdt(woggiiso:*years);
034700090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(nrweek:'X'):6:0);
034800090317             if lacaddms < wlacaddms;
034900090313
035000090313       // --> se ho elaborato più di 9000 immagini
035100090317               if nrbolle > 1;
035200090313       // --  devo ciclare per il n. di bolle da creare
035300090317                 for xx by 1 to nrbolle;
035400090313       // --  importo a totale forzato
035500090317                   if impimg > *zeros and lacfimp = 'T';
035600090317                     wimp = impimg * wncl;
035700090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
035800090313       //       devo calcolare il totale
035900090317                     if xx = nrbolle and restoncl = 0;
036000090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
036100090317                     endif;
036200090317                   endif;
036300090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
036400090317       //       imposto i colli
036500090317                   if xx = nrbolle and restoncl > 0;
036600090317                     wncl = restoncl;
036700090317       //       importo a totale forzato calcolo il totale
036800090317                     if impimg > *zeros and lacfimp = 'T';
036900090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
037000090317                     endif;
037100090317                   endif;
037200090313       // ----  importo a immagine forzato
037300090317                   if impimg > *zeros and lacfimp = 'I';
037400090317                     wimp = impimg * wncl;
037500090317                   endif;
037600090313       // ----  scrivo addebito
037700090317                   exsr sr_wrtrig;
037800090317                 ENDFOR;
037900090317               ENDIF;
038000090313
038100090313       // --> ho una sola bolla da creare
038200090317               if nrbolle = 1;
038300090317                 wncl = nrimg;
038400090317                 if impimg > *zeros and lacfimp = 'T';
038500090317                   wimp = lacimp;
038600090317                 endif;
038700090317                 if impimg > *zeros and lacfimp = 'I';
038800090317                   wimp = impimg * wncl;
038900090317                 endif;
039000090313       // --  scrivo addebito
039100090317                 exsr sr_wrtrig;
039200090317               ENDIF;
039300090317             ENDIF;
039400090313
039500090311
039600090313       // addebito per mese scrivo un solo rcd di fnrig
039700090313       // con il totale dei rcd letti da tilac
039800090317           when lactadu = 'M';
039900090313       // se sono nel mese sucessivo a quella previsto per l'addebito
040000090318             wyear  = %subdt(woggiiso:*years);
040100090318             wmonth = %subdt(woggiiso:*months);
040200090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(wmonth:'X'):6:0);
040300090317             if lacaddms < wlacaddms;
040400090313
040500090313       // --> se ho elaborato più di 9000 immagini
040600090317               if nrbolle > 1;
040700090313       // --  devo ciclare per il n. di bolle da creare
040800090317                 for xx by 1 to nrbolle;
040900090313       // --  importo a totale forzato
041000090317                   if impimg > *zeros and lacfimp = 'T';
041100090317                     wimp = impimg * wncl;
041200090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
041300090313       //       devo calcolare il totale
041400090317                     if xx = nrbolle and restoncl = 0;
041500090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
041600090317                     endif;
041700090317                   endif;
041800090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
041900090317       //       imposto i colli
042000090317                   if xx = nrbolle and restoncl > 0;
042100090317                     wncl = restoncl;
042200090317       //       importo a totale forzato
042300090317                     if impimg > *zeros and lacfimp = 'T';
042400090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
042500090317                     endif;
042600090317                   endif;
042700090313       // ----  importo a immagine forzato
042800090317                   if impimg > *zeros and lacfimp = 'I';
042900090317                     wimp = impimg * wncl;
043000090317                   endif;
043100090313       // ----  scrivo addebito
043200090317                   exsr sr_wrtrig;
043300090317                 ENDFOR;
043400090317               ENDIF;
043500090313
043600090313       // --> ho una sola bolla da creare
043700090317               if nrbolle = 1;
043800090317                 wncl = nrimg;
043900090317                 if impimg > *zeros and lacfimp = 'T';
044000090317                   wimp = lacimp;
044100090317                 endif;
044200090317                 if impimg > *zeros and lacfimp = 'I';
044300090317                   wimp = impimg * wncl;
044400090317                 endif;
044500090313       // --  scrivo addebito
044600090317                 exsr sr_wrtrig;
044700090317               ENDIF;
044800090317             ENDIF;
044900090317         ENDSL;
045000090313
045100090311       ENDSR;
045200090313
045300090313       //-----------------------------------------------------------------------
045400090313       // Controllo se e come srivere FNRIG
045500090313       //-----------------------------------------------------------------------
045600090313       begsr sr_wrtrig;
045700090330
045800090330         $add = *on;
045900090313
046000090313         clear fnrig000;
046100090313
046200090316       // spedizione solo se addebito ad immagine
046300090316         if lactadu = 'I';
046400090316           rigaas = 2000 + %dec(lacaas:2:0);
046500090316           riglnp = %dec(laclnp:3:0);
046600090316           rignrs = %dec(lacnrs:2:0);
046700090316           rignsp = %dec(lacnsp:7:0);
046800090316         ENDIF;
046900090313
047000090313       // cliente tassazione + tariffa
047100090313       // il cliente forzato + la tariffa sono memorizzati nel campo lacidl
047200090313       // prima però delle nuove modifiche questo campo conteneva il nome del lavoro che
047300090313       // scrive TILAC, per riconoscere i nuovi rcd dai vecchi uso il campo LACTELA che è nuovo
047400090313       // e viene impostato con le nuove modifiche
047500090313         if lactela <> *blanks;
047600090313       // per sicurezza comunque controllo anche il campo LACIDL
047700090317           if %check(c_digits:lacidl) = 0 and lacidl <> *blanks;
047800090313             rigksc = %dec(%subst(lacidl:1:7):7:0);
047900090313             rigctr = %subst(lacidl:8:3);
048000090313           ENDIF;
048100090313         ENDIF;
048200090313
048300090313        // tipo tassazione
048400090313         if lactad = 'S';
048500090313           rigtas = 'T';
048600090313         ENDIF;
048700090313         if lactad = 'V';
048800090313           rigtas = 'N';
048900090313         ENDIF;
049000090313
049100090313        // tassazione forzata
049200090313         if wimp > *zeros;
049300090313           rigimp = wimp;
049400090313           rigtas = 'F';
049500090313         endif;
049600090313
049700090313         rigcau = 'P';
049800090313         rigncl = wncl;
049900090313         rigtra = 'N';
050000090313         rigpru = 'TILAC';
050100090313         rigdata = woggi;
050200090313         rigora = wora;
050300090317
050400090320       // tassazione mensile memorizzo la data di spedizione
050500090320       // ultimo gg del mese precedente
050600090318         if lactadu = 'M';
050700090318           rigdsp = lacaddms*100+1;
050800090318           wdatiso = %date(rigdsp) + %months(1) - %days(1);
050900090318           rigdsp = %dec(wdatiso);
051000090317         endif;
051100090317
051200090313         write fnrig000;
051300090313
051400090313       ENDSR;
051500090311
051600090313       //-----------------------------------------------------------------------
051700090313       // Aggiorno TILAC
051800090313       //-----------------------------------------------------------------------
051900090313       begsr sr_agglac;
052000090313
052100090313       // se lacela non è più a '01' non aggiorno
052200090313         if lacela > '00' and lacela < '10';
052300090313           wlacela = %dec(lacela:2:0);
052400090313           wlacela = wlacela + 10;
052500090313           lacela = %editc(wlacela:'X');
052600090313           update tilac000;
052700090313         ENDIF;
052800090313
052900090313       ENDSR;
053000090303
