000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300090311      //-----------------------------------------------------------------------
000400090311      // Creazione bolle per addebito invio immagini a CLI
000500090311      //-----------------------------------------------------------------------
000600090310     ftilac03l  uf   e           k disk
000700090616     ftilac00f  uf   e             disk    rename(tilac000:tilac0) prefix(l_)
000800090303     ffnrig00f  o    e             disk
000900090311
001000090311      //-----------------------------------------------------------------------
001100090313      //  Definizione variabili.
001200090311      //-----------------------------------------------------------------------
001300090313     d impbolla        s                   like(lacimp)
001400090313     d impimg          s                   like(lacimp)
001500090313     d nrbolle         s              2  0
001600090313     d nrimg           s             10i 0
001700090312     d nrlac           s             10i 0
001800090311     d nrweek          s              2  0
001900090317     d oldidjob        s                   like(lacidjob)
002000090317     d oldidl          s                   like(lacidl)
002100090312     d restoncl        s             10i 0
002200090616     d rrnlac          s              9  0
002300090313     d savidjob        s                   like(lacidjob)
002400090317     d savncl          s                   like(rigncl)
002500090616     d savnim          s                   like(lacnim)
002600090312     d xx              s              2  0
002700090311     d wdatiso         s               d
002800090311     d widjob          s                   like(lacidjob)
002900090313     d widl            s                   like(lacidl)
003000090313     d wimp            s                   like(rigimp)
003100090312     d wlacaddms       s                   like(lacaddms)
003200090313     d wlacela         s              2  0
003300090313     d wmonth          s              2  0
003400090313     d wncl            s                   like(rigncl)
003500050318     d wora            s              6  0
003600050318     d woggi           s              8  0
003700090318     d woggiiso        s               d
003800090311     d wtadu           s                   like(lactadu)
003900090312     d wyear           s              4  0
004000090313     d $add            s               n
004100090313     d $fine           s               n
004200090311     d $finelac        s               n
004300090616     d $finenim        s               n
004400090311
004500090311      //-----------------------------------------------------------------------
004600090313      //  Definizione strutture dati.
004700090311      //-----------------------------------------------------------------------
004800090317     d dvpo          e ds
004900090317     d kpjba         e ds
005000090317     d tibs02ds      e ds
005100090616     d tilacds       e ds                  extname(tilac00f) prefix(n_)
005200090313
005300090313      //-----------------------------------------------------------------------
005400090313      //  Definizione costanti.
005500090313      //----------------------------------------------------------------------
005600090313      // - Costante per controllo "caratteri solo numerici"
005700090313     d c_Digits        c                   const('0123456789')
005800090317
005900090317      //--------------------------------------------------------------
006000090317      // Definizione procedure usate.                                 ?
006100090317      //--------------------------------------------------------------
006200090317      /copy gaitrasrc/srcprotopr,tibs02r
006300090313
006400090311      //-----------------------------------------------------------------------
006500090311      // RIEPILOGO INDICATORI
006600090311      //-----------------------------------------------------------------------
006700040511
006800090311      //-----------------------------------------------------------------------
006900090311     c     *entry        plist
007000090311     c                   parm                    kpjba
007100090311
007200090311      /free
007300090317
007400090317       // n.colli possbili per addebito pod image a riepilogo
007500090317       clear tibs02ds;
007600090317       clear dvpo;
007700090317       t02tla = 'L';
007800090317       t02mod = 'C';
007900090317       t02sif = knsif;
008000090317       t02cod = 'VPO';
008100090317       t02ke1 = 'VPO';
008200090317       TNTBE_RicercaControllo(kpjba : tibs02ds);
008300090317       if t02err = *blank;
008400090317         dvpo = t02uni;
008500090317       else;
008600090320         §vpopod = 5000;
008700090317       endif;
008800090311
008900090313       // data/ora del job
009000090313       woggi = %dec(%date());
009100090313       wora  = %dec(%time());
009200090318       woggiiso = %date(woggi);
009300090311
009400090311       $fine = *off;
009500090311
009600090311       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009700090402
009800090402       // prima di tutto devo rimettere a '11' i rcd che hanno la spia add.
009900090402       // perchè vuol dire che non devo rifare l'addebito, sono stati messi
010000090402       // a '01' da laguna dopo la creazione di FNRIG.
010100090402       // Caso di: oggi scannerizzo, rcd a '00', laguna lo passa a '01' e addebito
010200090402       //          lo passa a '11', domani riscannerizzo, nuovo rcd a '00', laguna
010300090402       //          aggiorna tutti i rcd con stessa spedizione a '01', passa il pgm
010400090402       //          dell'addebito e mi fa 2 addebiti, non va bene, uno lo ha già fatto
010500090402       //          ieri, con la spia evitiamo il doppio addebito
010600090402       exec sql
010700090402         update tilac00f
010800090402           set lacela = '11'
010900090402           where
011000090402           lactimecd <> ' ' and lacela > '00' and lacela < '10';
011100090311
011200090313       // leggo tilac00f raggruppato per id job e ksc + ctr
011300090311       exec sql
011400090313         declare idjob cursor for select distinct lacidjob, lacidl,
011500090313                                                  lactadu
011600090402           from tilac03l where lacidjob <> ' '
011700090401                         and lacela <> '33'
011800090313           group by lacidjob, lacidl, lactadu;
011900090311       exec sql
012000090311         open idjob;
012100090311             if sqlcode < 0;
012200090311               $fine = *on;
012300090311             ENDIF;
012400090311
012500090311       dow not $fine;
012600090311
012700090313         exsr sr_leggijob;
012800090311
012900090311       ENDDO;
013000090311
013100090311       *inlr = *on;
013200090311
013300090313       //-----------------------------------------------------------------------
013400090313       // Leggo tutti i Job di Tilac
013500090313       //-----------------------------------------------------------------------
013600090311       begsr sr_leggijob;
013700090311
013800090311         exec sql
013900090313           fetch next from idjob into :widjob, :widl, :wtadu;
014000090311
014100090313       // fine file esco
014200090311               if sqlcod = 100 or sqlcod < 0;
014300090311                 $fine = *on;
014400090311                 leavesr;
014500090311               ENDIF;
014600090311
014700090313       // a rottura di job
014800090313         if widjob <> savidjob;
014900090313           clear nrlac;
015000090313
015100090313       // per il job letto controllo se ci sono ancora dei rcd a stato '00' o
015200090317       // con timer maggiore uguale oggi
015300090313           savidjob = widjob;
015400090313           exec sql
015500090313             select count(*) into: nrlac from tilac00f
015600090401               where lacidjob = :widjob and lacela <> '33' and
015700090402               (lacela = '00' or
015800090402                substr(lactim, 1, 8) >= digits(:woggi));
015900090313           if sqlcod <> 0;
016000090313             nrlac = 1;
016100090313             leavesr;
016200090313           ENDIF;
016300090313           if nrlac > 0;
016400090313             leavesr;
016500090313           ENDIF;
016600090316
016700090313       // se stesso job e non ok tutti i rcd non devo elaborare
016800090313         else;
016900090313           if nrlac > 0;
017000090313             leavesr;
017100090313           ENDIF;
017200090313
017300090313         ENDIF;
017400090313
017500090616       // se tutto ok per pod image con addebito a Job (CD) devo prima controllare se
017600090615       // ci sono immagini con stesso nome per non addebitarle
017700090616         if wtadu = 'J';
017800090616           exsr sr_nim;
017900090616         endif;
018000090615
018100090615
018200090313       // se tutto ok elaboro, se arrivo qua vuol dire che il job è completo
018300090313       // anche se ho + ksc e ctr per lo stesso job
018400090313         exsr sr_elabora;
018500090311
018600090311       ENDSR;
018700090616
018800090616       //-----------------------------------------------------------------------
018900090616       // Controllo se ci sono immagini con lo stesso nome all'interno del job
019000090616       //-----------------------------------------------------------------------
019100090616       begsr sr_nim;
019200090616
019300090616         exec sql
019400090616           declare nim cursor for select rrn(tilac03l), tilac03l.*
019500090616           from tilac03l where lacidjob = :widjob
019600090616                         and lacela <> '33'
019700090616           order by lacnim;
019800090616         exec sql
019900090616           open nim;
020000090616             if sqlcode < 0;
020100090616               $finenim = *on;
020200090616             ENDIF;
020300090616
020400090616         dow not $finenim;
020500090616           exec sql
020600090616             fetch next from nim into :rrnlac, :tilacds;
020700090616
020800090616       // fine file esco
020900090616               if sqlcod = 100 or sqlcod < 0;
021000090616                 $finenim = *on;
021100090616                 leavesr;
021200090616               ENDIF;
021300090616
021400090616       // a immagine uguale aggiorno il rcd come già elaborato
021500090616             if n_lacnim <> savnim;
021600090616               savnim = n_lacnim;
021700090616             else;
021800090616               chain rrnlac tilac00f;
021900090616               if %found(tilac00f);
022000090616                 l_lacela = '11';
022100090616                 l_lactimecd = %char(%timestamp:*iso0);
022200090616                 update tilac0;
022300090616               endif;
022400090616             endif;
022500090616
022600090616         enddo;
022700090616
022800090616         exec sql
022900090616           close nim;
023000090616
023100090616       ENDSR;
023200090303
023300090313       //-----------------------------------------------------------------------
023400090313       // Elaboro il Job e ksc+ctr
023500090313       //-----------------------------------------------------------------------
023600090311       begsr sr_elabora;
023700090313
023800090316         clear nrimg;
023900090316
024000090316       // conto quante immagini devo impostare nella spedizione di addebito
024100090316         exec sql
024200090402           select count(*) into: nrimg from tilac03l
024300090402             where lacidjob = :widjob and lacidl = :widl;
024400090316           if sqlcod <> 0;
024500090316             leavesr;
024600090316           ENDIF;
024700090311
024800090313       // leggo tilac per JOB/ksc+ctr (LACIDL)
024900090311         exsr sr_leggilac;
025000090311
025100090311       ENDSR;
025200090311
025300090313       //-----------------------------------------------------------------------
025400090313       // Leggo TILAC per Job e ksc+ctr
025500090313       //-----------------------------------------------------------------------
025600090311       begsr sr_leggilac;
025700090313
025800090313        $finelac = *off;
025900090311
026000090313       // Leggo TILAC
026100090313        setll (widjob:widl) tilac03l;
026200090311
026300090311        dow not $finelac;
026400090313          reade (widjob:widl) tilac03l;
026500090311          if %eof(tilac03l);
026600090311            $finelac = *on;
026700090311            leave;
026800090311          ENDIF;
026900090317
027000090317       // scrivo fnrig se richiesto
027100090317          select;
027200090317            when lactad = 'N';
027300090330         // Aggiorno TILAC
027400090330            exsr sr_agglac;
027500090317
027600090317       // se frequenza addebito per Immagine scrivo un rcd di FNRIG x ogni immagine
027700090317            when lactadu = 'I';
027800090317             wncl = 1;
027900090317             wimp = lacimp;
028000090317             exsr sr_wrtrig;
028100090330         // Aggiorno TILAC
028200090330            exsr sr_agglac;
028300090317
028400090317       // se frequenza addebito a Job/gg o Settimana o Mese calcolo quanti rcd di
028500090317       // FNRIG devo scrivere
028600090317            when lactadu <> 'I';
028700090317       // per id job e ksc+ctr
028800090317              if lacidjob <> oldidjob or lacidl <> oldidl;
028900090401                $add = *off;
029000090317                exsr sr_conta;
029100090317                oldidjob = lacidjob;
029200090317                oldidl   = lacidl;
029300090317              endif;
029400090401           // Aggiorno TILAC
029500090401           // se scritto FNRIG
029600090401              if $add;
029700090401                exsr sr_agglac;
029800090401              endif;
029900090317
030000090317          endsl;
030100090311        ENDDO;
030200090317
030300090317        clear oldidjob;
030400090317        clear oldidl;
030500090311
030600090311       ENDSR;
030700090311
030800090313       //-----------------------------------------------------------------------
030900090317       // Conta quanti rcd bolle devo creare e i relativi colli + importo
031000090313       //-----------------------------------------------------------------------
031100090317       begsr sr_conta;
031200090317
031300090317         clear nrbolle;
031400090317         clear restoncl;
031500090317         clear wncl;
031600090317         clear wimp;
031700090317         clear impimg;
031800090317         clear savncl;
031900090317
032000090317       // conto quante bolle e i colli per bolla
032100090317         select;
032200090320       // controllo che il n. dei colli non superi il max consentito 5000
032300090320       // se supera devo fare più bolle
032400090317           when nrimg > §vpopod;
032500090317             nrbolle  = %div(nrimg:§vpopod);
032600090317             restoncl = %rem(nrimg:§vpopod);
032700090317             wncl = §vpopod;
032800090317             savncl = §vpopod;
032900090317           when nrimg <= §vpopod;
033000090317             nrbolle = 1;
033100090317             wncl = nrimg;
033200090317         endsl;
033300090317       // se ho resto aggiungo 1 al n. di bolle da creare
033400090317         if restoncl > 0;
033500090317           nrbolle = nrbolle + 1;
033600090317         endif;
033700090317
033800090317       // calcolo l'importo per bolla
033900090317         select;
034000090317       // se devo creare più bolle devo ripartire l'importo forzato a totale
034100090317           when nrbolle > 1 and lacfimp = 'T' and lacimp <> *zeros;
034200090317             impimg = lacimp / nrimg;
034300090317       // se devo creare 1 bolla imposto l'importo forzato a totale
034400090317           when nrbolle = 1 and lacfimp = 'T' and lacimp <> *zeros;
034500090317             wimp = lacimp;
034600090317       // se l'importo forzato è a immagine imposto l'importo a immagine
034700090317           when lacfimp = 'I' and lacimp <> *zeros;
034800090317             impimg = lacimp;
034900090317          ENDSL;
035000090312
035100090317       // scrive FNRIG in base alle frequenza addebito
035200090317         xx = 1;
035300090312
035400090317         select;
035500090313       // addebito per job scrivo un solo rcd di fnrig
035600090313       // con il totale dei rcd letti da tilac
035700090317           when lactadu = 'J';
035800090313
035900090317       // --> se ho più di 90000 o 5000 immagini
036000090317             if nrbolle > 1;
036100090313       // --  devo ciclare per il n. di bolle da creare
036200090317               for xx by 1 to nrbolle;
036300090313       // --  importo a totale forzato
036400090317                 if impimg > *zeros and lacfimp = 'T';
036500090317                   wimp = impimg * wncl;
036600090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
036700090313       //       devo calcolare il totale
036800090317                   if xx = nrbolle and restoncl = 0;
036900090317                     wimp = lacimp - (impimg * savncl * (nrbolle -1));
037000090317                   endif;
037100090317                 endif;
037200090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
037300090317       //       imposto i colli
037400090317                 if xx = nrbolle and restoncl > 0;
037500090317                   wncl = restoncl;
037600090317       //       se importo a totale forzato calcolo il totale
037700090317                   if impimg > *zeros and lacfimp = 'T';
037800090317                     wimp = lacimp - (impimg * savncl * (nrbolle-1));
037900090317                   endif;
038000090317                 endif;
038100090313       // ----  importo a immagine forzato
038200090317                 if impimg > *zeros and lacfimp = 'I';
038300090317                   wimp = impimg * wncl;
038400090317                 endif;
038500090313       // ----  scrivo addebito
038600090317                 exsr sr_wrtrig;
038700090317               ENDFOR;
038800090317             ENDIF;
038900090313
039000090313       // --> ho una sola bolla da creare
039100090317             if nrbolle = 1;
039200090317               wncl = nrimg;
039300090317               if impimg > *zeros and lacfimp = 'T';
039400090317                 wimp = lacimp;
039500090317               endif;
039600090317               if impimg > *zeros and lacfimp = 'I';
039700090317                 wimp = impimg * wncl;
039800090317               endif;
039900090313       // --  scrivo addebito
040000090317               exsr sr_wrtrig;
040100090317             ENDIF;
040200090311
040300090313
040400090313       // addebito per settimana scrivo un solo rcd di fnrig
040500090313       // con il totale dei rcd letti da tilac
040600090317           when lactadu = 'S';
040700090313       // se sono nella settimana sucessiva a quella prevista per l'addebito
040800090317             exec sql
040900090318               set :nrweek = week_iso(:woggiiso);
041000090318             wyear = %subdt(woggiiso:*years);
041100090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(nrweek:'X'):6:0);
041200090317             if lacaddms < wlacaddms;
041300090313
041400090313       // --> se ho elaborato più di 9000 immagini
041500090317               if nrbolle > 1;
041600090313       // --  devo ciclare per il n. di bolle da creare
041700090317                 for xx by 1 to nrbolle;
041800090313       // --  importo a totale forzato
041900090317                   if impimg > *zeros and lacfimp = 'T';
042000090317                     wimp = impimg * wncl;
042100090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
042200090313       //       devo calcolare il totale
042300090317                     if xx = nrbolle and restoncl = 0;
042400090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
042500090317                     endif;
042600090317                   endif;
042700090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
042800090317       //       imposto i colli
042900090317                   if xx = nrbolle and restoncl > 0;
043000090317                     wncl = restoncl;
043100090317       //       importo a totale forzato calcolo il totale
043200090317                     if impimg > *zeros and lacfimp = 'T';
043300090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
043400090317                     endif;
043500090317                   endif;
043600090313       // ----  importo a immagine forzato
043700090317                   if impimg > *zeros and lacfimp = 'I';
043800090317                     wimp = impimg * wncl;
043900090317                   endif;
044000090313       // ----  scrivo addebito
044100090317                   exsr sr_wrtrig;
044200090317                 ENDFOR;
044300090317               ENDIF;
044400090313
044500090313       // --> ho una sola bolla da creare
044600090317               if nrbolle = 1;
044700090317                 wncl = nrimg;
044800090317                 if impimg > *zeros and lacfimp = 'T';
044900090317                   wimp = lacimp;
045000090317                 endif;
045100090317                 if impimg > *zeros and lacfimp = 'I';
045200090317                   wimp = impimg * wncl;
045300090317                 endif;
045400090313       // --  scrivo addebito
045500090317                 exsr sr_wrtrig;
045600090317               ENDIF;
045700090317             ENDIF;
045800090313
045900090311
046000090313       // addebito per mese scrivo un solo rcd di fnrig
046100090313       // con il totale dei rcd letti da tilac
046200090317           when lactadu = 'M';
046300090313       // se sono nel mese sucessivo a quella previsto per l'addebito
046400090318             wyear  = %subdt(woggiiso:*years);
046500090318             wmonth = %subdt(woggiiso:*months);
046600090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(wmonth:'X'):6:0);
046700090317             if lacaddms < wlacaddms;
046800090313
046900090313       // --> se ho elaborato più di 9000 immagini
047000090317               if nrbolle > 1;
047100090313       // --  devo ciclare per il n. di bolle da creare
047200090317                 for xx by 1 to nrbolle;
047300090313       // --  importo a totale forzato
047400090317                   if impimg > *zeros and lacfimp = 'T';
047500090317                     wimp = impimg * wncl;
047600090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
047700090313       //       devo calcolare il totale
047800090317                     if xx = nrbolle and restoncl = 0;
047900090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
048000090317                     endif;
048100090317                   endif;
048200090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
048300090317       //       imposto i colli
048400090317                   if xx = nrbolle and restoncl > 0;
048500090317                     wncl = restoncl;
048600090317       //       importo a totale forzato
048700090317                     if impimg > *zeros and lacfimp = 'T';
048800090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
048900090317                     endif;
049000090317                   endif;
049100090313       // ----  importo a immagine forzato
049200090317                   if impimg > *zeros and lacfimp = 'I';
049300090317                     wimp = impimg * wncl;
049400090317                   endif;
049500090313       // ----  scrivo addebito
049600090317                   exsr sr_wrtrig;
049700090317                 ENDFOR;
049800090317               ENDIF;
049900090313
050000090313       // --> ho una sola bolla da creare
050100090317               if nrbolle = 1;
050200090317                 wncl = nrimg;
050300090317                 if impimg > *zeros and lacfimp = 'T';
050400090317                   wimp = lacimp;
050500090317                 endif;
050600090317                 if impimg > *zeros and lacfimp = 'I';
050700090317                   wimp = impimg * wncl;
050800090317                 endif;
050900090313       // --  scrivo addebito
051000090317                 exsr sr_wrtrig;
051100090317               ENDIF;
051200090317             ENDIF;
051300090317         ENDSL;
051400090313
051500090311       ENDSR;
051600090313
051700090313       //-----------------------------------------------------------------------
051800090313       // Controllo se e come srivere FNRIG
051900090313       //-----------------------------------------------------------------------
052000090313       begsr sr_wrtrig;
052100090330
052200090330         $add = *on;
052300090313
052400090313         clear fnrig000;
052500090313
052600090316       // spedizione solo se addebito ad immagine
052700090316         if lactadu = 'I';
052800090316           rigaas = 2000 + %dec(lacaas:2:0);
052900090316           riglnp = %dec(laclnp:3:0);
053000090316           rignrs = %dec(lacnrs:2:0);
053100090316           rignsp = %dec(lacnsp:7:0);
053200090316         ENDIF;
053300090313
053400090313       // cliente tassazione + tariffa
053500090313       // il cliente forzato + la tariffa sono memorizzati nel campo lacidl
053600090313       // prima però delle nuove modifiche questo campo conteneva il nome del lavoro che
053700090313       // scrive TILAC, per riconoscere i nuovi rcd dai vecchi uso il campo LACTELA che è nuovo
053800090313       // e viene impostato con le nuove modifiche
053900090313         if lactela <> *blanks;
054000090313       // per sicurezza comunque controllo anche il campo LACIDL
054100090317           if %check(c_digits:lacidl) = 0 and lacidl <> *blanks;
054200090313             rigksc = %dec(%subst(lacidl:1:7):7:0);
054300090313             rigctr = %subst(lacidl:8:3);
054400090313           ENDIF;
054500090313         ENDIF;
054600090313
054700090313        // tipo tassazione
054800090313         if lactad = 'S';
054900090313           rigtas = 'T';
055000090313         ENDIF;
055100090313         if lactad = 'V';
055200090313           rigtas = 'N';
055300090313         ENDIF;
055400090313
055500090313        // tassazione forzata
055600090313         if wimp > *zeros;
055700090313           rigimp = wimp;
055800090313           rigtas = 'F';
055900090313         endif;
056000090313
056100090313         rigcau = 'P';
056200090313         rigncl = wncl;
056300090313         rigtra = 'N';
056400090313         rigpru = 'TILAC';
056500090313         rigdata = woggi;
056600090313         rigora = wora;
056700090317
056800090320       // tassazione mensile memorizzo la data di spedizione
056900090320       // ultimo gg del mese precedente
057000090318         if lactadu = 'M';
057100090318           rigdsp = lacaddms*100+1;
057200090318           wdatiso = %date(rigdsp) + %months(1) - %days(1);
057300090318           rigdsp = %dec(wdatiso);
057400090317         endif;
057500090317
057600090313         write fnrig000;
057700090313
057800090313       ENDSR;
057900090311
058000090313       //-----------------------------------------------------------------------
058100090313       // Aggiorno TILAC
058200090313       //-----------------------------------------------------------------------
058300090313       begsr sr_agglac;
058400090313
058500090313       // se lacela non è più a '01' non aggiorno
058600090313         if lacela > '00' and lacela < '10';
058700090313           wlacela = %dec(lacela:2:0);
058800090313           wlacela = wlacela + 10;
058900090313           lacela = %editc(wlacela:'X');
059000090402        // imposto anche data spia add.
059100090402           lactimecd = %char(%timestamp:*iso0);
059200090313           update tilac000;
059300090313         ENDIF;
059400090313
059500090313       ENDSR;
059600090303
