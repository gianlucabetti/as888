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
010600090624       //          però solo se non sono rcd relativi ad un addebito a JOB
010700090402       exec sql
010800090402         update tilac00f
010900090402           set lacela = '11'
011000090402           where
011100090624           lactimecd <> ' ' and lacela > '00' and lacela < '10'
011200090624           and lactadu <> 'J';
011300090311
011400090313       // leggo tilac00f raggruppato per id job e ksc + ctr
011500090311       exec sql
011600090313         declare idjob cursor for select distinct lacidjob, lacidl,
011700090313                                                  lactadu
011800090402           from tilac03l where lacidjob <> ' '
011900090401                         and lacela <> '33'
012000090313           group by lacidjob, lacidl, lactadu;
012100090311       exec sql
012200090311         open idjob;
012300090311             if sqlcode < 0;
012400090311               $fine = *on;
012500090311             ENDIF;
012600090311
012700090311       dow not $fine;
012800090311
012900090313         exsr sr_leggijob;
013000090311
013100090311       ENDDO;
013200090311
013300090311       *inlr = *on;
013400090311
013500090313       //-----------------------------------------------------------------------
013600090313       // Leggo tutti i Job di Tilac
013700090313       //-----------------------------------------------------------------------
013800090311       begsr sr_leggijob;
013900090311
014000090311         exec sql
014100090313           fetch next from idjob into :widjob, :widl, :wtadu;
014200090311
014300090313       // fine file esco
014400090311               if sqlcod = 100 or sqlcod < 0;
014500090311                 $fine = *on;
014600090311                 leavesr;
014700090311               ENDIF;
014800090311
014900090313       // a rottura di job
015000090313         if widjob <> savidjob;
015100090313           clear nrlac;
015200090313
015300090313       // per il job letto controllo se ci sono ancora dei rcd a stato '00' o
015400090317       // con timer maggiore uguale oggi
015500090313           savidjob = widjob;
015600090313           exec sql
015700090313             select count(*) into: nrlac from tilac00f
015800090401               where lacidjob = :widjob and lacela <> '33' and
015900090402               (lacela = '00' or
016000090402                substr(lactim, 1, 8) >= digits(:woggi));
016100090313           if sqlcod <> 0;
016200090313             nrlac = 1;
016300090313             leavesr;
016400090313           ENDIF;
016500090313           if nrlac > 0;
016600090313             leavesr;
016700090313           ENDIF;
016800090316
016900090313       // se stesso job e non ok tutti i rcd non devo elaborare
017000090313         else;
017100090313           if nrlac > 0;
017200090313             leavesr;
017300090313           ENDIF;
017400090313
017500090313         ENDIF;
017600090313
017700090616       // se tutto ok per pod image con addebito a Job (CD) devo prima controllare se
017800090615       // ci sono immagini con stesso nome per non addebitarle
017900090616         if wtadu = 'J';
018000090616           exsr sr_nim;
018100090616         endif;
018200090615
018300090615
018400090313       // se tutto ok elaboro, se arrivo qua vuol dire che il job è completo
018500090313       // anche se ho + ksc e ctr per lo stesso job
018600090313         exsr sr_elabora;
018700090311
018800090311       ENDSR;
018900090616
019000090616       //-----------------------------------------------------------------------
019100090616       // Controllo se ci sono immagini con lo stesso nome all'interno del job
019200090616       //-----------------------------------------------------------------------
019300090616       begsr sr_nim;
019400090616
019500090616         exec sql
019600090616           declare nim cursor for select rrn(tilac03l), tilac03l.*
019700090616           from tilac03l where lacidjob = :widjob
019800090616                         and lacela <> '33'
019900090616           order by lacnim;
020000090616         exec sql
020100090616           open nim;
020200090616             if sqlcode < 0;
020300090616               $finenim = *on;
020400090616             ENDIF;
020500090616
020600090616         dow not $finenim;
020700090616           exec sql
020800090616             fetch next from nim into :rrnlac, :tilacds;
020900090616
021000090616       // fine file esco
021100090616               if sqlcod = 100 or sqlcod < 0;
021200090616                 $finenim = *on;
021300090616                 leavesr;
021400090616               ENDIF;
021500090616
021600090616       // a immagine uguale aggiorno il rcd come già elaborato
021700090616             if n_lacnim <> savnim;
021800090616               savnim = n_lacnim;
021900090616             else;
022000090616               chain rrnlac tilac00f;
022100090616               if %found(tilac00f);
022200090616                 l_lacela = '11';
022300090616                 l_lactimecd = %char(%timestamp:*iso0);
022400090616                 update tilac0;
022500090616               endif;
022600090616             endif;
022700090616
022800090616         enddo;
022900090616
023000090616         exec sql
023100090616           close nim;
023200090616
023300090616       ENDSR;
023400090303
023500090313       //-----------------------------------------------------------------------
023600090313       // Elaboro il Job e ksc+ctr
023700090313       //-----------------------------------------------------------------------
023800090311       begsr sr_elabora;
023900090313
024000090316         clear nrimg;
024100090316
024200090316       // conto quante immagini devo impostare nella spedizione di addebito
024300090316         exec sql
024400090402           select count(*) into: nrimg from tilac03l
024500090402             where lacidjob = :widjob and lacidl = :widl;
024600090316           if sqlcod <> 0;
024700090316             leavesr;
024800090316           ENDIF;
024900090311
025000090313       // leggo tilac per JOB/ksc+ctr (LACIDL)
025100090311         exsr sr_leggilac;
025200090311
025300090311       ENDSR;
025400090311
025500090313       //-----------------------------------------------------------------------
025600090313       // Leggo TILAC per Job e ksc+ctr
025700090313       //-----------------------------------------------------------------------
025800090311       begsr sr_leggilac;
025900090313
026000090313        $finelac = *off;
026100090311
026200090313       // Leggo TILAC
026300090313        setll (widjob:widl) tilac03l;
026400090311
026500090311        dow not $finelac;
026600090313          reade (widjob:widl) tilac03l;
026700090311          if %eof(tilac03l);
026800090311            $finelac = *on;
026900090311            leave;
027000090311          ENDIF;
027100090317
027200090317       // scrivo fnrig se richiesto
027300090317          select;
027400090317            when lactad = 'N';
027500090330         // Aggiorno TILAC
027600090330            exsr sr_agglac;
027700090317
027800090317       // se frequenza addebito per Immagine scrivo un rcd di FNRIG x ogni immagine
027900090317            when lactadu = 'I';
028000090317             wncl = 1;
028100090317             wimp = lacimp;
028200090317             exsr sr_wrtrig;
028300090330         // Aggiorno TILAC
028400090330            exsr sr_agglac;
028500090317
028600090317       // se frequenza addebito a Job/gg o Settimana o Mese calcolo quanti rcd di
028700090317       // FNRIG devo scrivere
028800090317            when lactadu <> 'I';
028900090317       // per id job e ksc+ctr
029000090317              if lacidjob <> oldidjob or lacidl <> oldidl;
029100090401                $add = *off;
029200090317                exsr sr_conta;
029300090317                oldidjob = lacidjob;
029400090317                oldidl   = lacidl;
029500090317              endif;
029600090401           // Aggiorno TILAC
029700090401           // se scritto FNRIG
029800090401              if $add;
029900090401                exsr sr_agglac;
030000090401              endif;
030100090317
030200090317          endsl;
030300090311        ENDDO;
030400090317
030500090317        clear oldidjob;
030600090317        clear oldidl;
030700090311
030800090311       ENDSR;
030900090311
031000090313       //-----------------------------------------------------------------------
031100090317       // Conta quanti rcd bolle devo creare e i relativi colli + importo
031200090313       //-----------------------------------------------------------------------
031300090317       begsr sr_conta;
031400090317
031500090317         clear nrbolle;
031600090317         clear restoncl;
031700090317         clear wncl;
031800090317         clear wimp;
031900090317         clear impimg;
032000090317         clear savncl;
032100090317
032200090317       // conto quante bolle e i colli per bolla
032300090317         select;
032400090320       // controllo che il n. dei colli non superi il max consentito 5000
032500090320       // se supera devo fare più bolle
032600090317           when nrimg > §vpopod;
032700090317             nrbolle  = %div(nrimg:§vpopod);
032800090317             restoncl = %rem(nrimg:§vpopod);
032900090317             wncl = §vpopod;
033000090317             savncl = §vpopod;
033100090317           when nrimg <= §vpopod;
033200090317             nrbolle = 1;
033300090317             wncl = nrimg;
033400090317         endsl;
033500090317       // se ho resto aggiungo 1 al n. di bolle da creare
033600090317         if restoncl > 0;
033700090317           nrbolle = nrbolle + 1;
033800090317         endif;
033900090317
034000090317       // calcolo l'importo per bolla
034100090317         select;
034200090317       // se devo creare più bolle devo ripartire l'importo forzato a totale
034300090317           when nrbolle > 1 and lacfimp = 'T' and lacimp <> *zeros;
034400090317             impimg = lacimp / nrimg;
034500090317       // se devo creare 1 bolla imposto l'importo forzato a totale
034600090317           when nrbolle = 1 and lacfimp = 'T' and lacimp <> *zeros;
034700090317             wimp = lacimp;
034800090317       // se l'importo forzato è a immagine imposto l'importo a immagine
034900090317           when lacfimp = 'I' and lacimp <> *zeros;
035000090317             impimg = lacimp;
035100090317          ENDSL;
035200090312
035300090317       // scrive FNRIG in base alle frequenza addebito
035400090317         xx = 1;
035500090312
035600090317         select;
035700090313       // addebito per job scrivo un solo rcd di fnrig
035800090313       // con il totale dei rcd letti da tilac
035900090317           when lactadu = 'J';
036000090313
036100090716       // --> se le immagini superano il max consentito
036200090317             if nrbolle > 1;
036300090313       // --  devo ciclare per il n. di bolle da creare
036400090317               for xx by 1 to nrbolle;
036500090313       // --  importo a totale forzato
036600090317                 if impimg > *zeros and lacfimp = 'T';
036700090317                   wimp = impimg * wncl;
036800090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
036900090313       //       devo calcolare il totale
037000090317                   if xx = nrbolle and restoncl = 0;
037100090317                     wimp = lacimp - (impimg * savncl * (nrbolle -1));
037200090317                   endif;
037300090317                 endif;
037400090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
037500090317       //       imposto i colli
037600090317                 if xx = nrbolle and restoncl > 0;
037700090317                   wncl = restoncl;
037800090317       //       se importo a totale forzato calcolo il totale
037900090317                   if impimg > *zeros and lacfimp = 'T';
038000090317                     wimp = lacimp - (impimg * savncl * (nrbolle-1));
038100090317                   endif;
038200090317                 endif;
038300090313       // ----  importo a immagine forzato
038400090317                 if impimg > *zeros and lacfimp = 'I';
038500090317                   wimp = impimg * wncl;
038600090317                 endif;
038700090313       // ----  scrivo addebito
038800090317                 exsr sr_wrtrig;
038900090317               ENDFOR;
039000090317             ENDIF;
039100090313
039200090313       // --> ho una sola bolla da creare
039300090317             if nrbolle = 1;
039400090317               wncl = nrimg;
039500090317               if impimg > *zeros and lacfimp = 'T';
039600090317                 wimp = lacimp;
039700090317               endif;
039800090317               if impimg > *zeros and lacfimp = 'I';
039900090317                 wimp = impimg * wncl;
040000090317               endif;
040100090313       // --  scrivo addebito
040200090317               exsr sr_wrtrig;
040300090317             ENDIF;
040400090311
040500090313
040600090313       // addebito per settimana scrivo un solo rcd di fnrig
040700090313       // con il totale dei rcd letti da tilac
040800090317           when lactadu = 'S';
040900090313       // se sono nella settimana sucessiva a quella prevista per l'addebito
041000090317             exec sql
041100090318               set :nrweek = week_iso(:woggiiso);
041200090318             wyear = %subdt(woggiiso:*years);
041300090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(nrweek:'X'):6:0);
041400090317             if lacaddms < wlacaddms;
041500090313
041600090716       // --> se le immagini superano il max consentito
041700090317               if nrbolle > 1;
041800090313       // --  devo ciclare per il n. di bolle da creare
041900090317                 for xx by 1 to nrbolle;
042000090313       // --  importo a totale forzato
042100090317                   if impimg > *zeros and lacfimp = 'T';
042200090317                     wimp = impimg * wncl;
042300090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
042400090313       //       devo calcolare il totale
042500090317                     if xx = nrbolle and restoncl = 0;
042600090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
042700090317                     endif;
042800090317                   endif;
042900090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
043000090317       //       imposto i colli
043100090317                   if xx = nrbolle and restoncl > 0;
043200090317                     wncl = restoncl;
043300090317       //       importo a totale forzato calcolo il totale
043400090317                     if impimg > *zeros and lacfimp = 'T';
043500090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
043600090317                     endif;
043700090317                   endif;
043800090313       // ----  importo a immagine forzato
043900090317                   if impimg > *zeros and lacfimp = 'I';
044000090317                     wimp = impimg * wncl;
044100090317                   endif;
044200090313       // ----  scrivo addebito
044300090317                   exsr sr_wrtrig;
044400090317                 ENDFOR;
044500090317               ENDIF;
044600090313
044700090313       // --> ho una sola bolla da creare
044800090317               if nrbolle = 1;
044900090317                 wncl = nrimg;
045000090317                 if impimg > *zeros and lacfimp = 'T';
045100090317                   wimp = lacimp;
045200090317                 endif;
045300090317                 if impimg > *zeros and lacfimp = 'I';
045400090317                   wimp = impimg * wncl;
045500090317                 endif;
045600090313       // --  scrivo addebito
045700090317                 exsr sr_wrtrig;
045800090317               ENDIF;
045900090317             ENDIF;
046000090313
046100090311
046200090313       // addebito per mese scrivo un solo rcd di fnrig
046300090313       // con il totale dei rcd letti da tilac
046400090317           when lactadu = 'M';
046500090313       // se sono nel mese sucessivo a quella previsto per l'addebito
046600090318             wyear  = %subdt(woggiiso:*years);
046700090318             wmonth = %subdt(woggiiso:*months);
046800090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(wmonth:'X'):6:0);
046900090317             if lacaddms < wlacaddms;
047000090313
047100090716       // --> se le immagini superano il max consentito
047200090317               if nrbolle > 1;
047300090313       // --  devo ciclare per il n. di bolle da creare
047400090317                 for xx by 1 to nrbolle;
047500090313       // --  importo a totale forzato
047600090317                   if impimg > *zeros and lacfimp = 'T';
047700090317                     wimp = impimg * wncl;
047800090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
047900090313       //       devo calcolare il totale
048000090317                     if xx = nrbolle and restoncl = 0;
048100090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
048200090317                     endif;
048300090317                   endif;
048400090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
048500090317       //       imposto i colli
048600090317                   if xx = nrbolle and restoncl > 0;
048700090317                     wncl = restoncl;
048800090317       //       importo a totale forzato
048900090317                     if impimg > *zeros and lacfimp = 'T';
049000090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
049100090317                     endif;
049200090317                   endif;
049300090313       // ----  importo a immagine forzato
049400090317                   if impimg > *zeros and lacfimp = 'I';
049500090317                     wimp = impimg * wncl;
049600090317                   endif;
049700090313       // ----  scrivo addebito
049800090317                   exsr sr_wrtrig;
049900090317                 ENDFOR;
050000090317               ENDIF;
050100090313
050200090313       // --> ho una sola bolla da creare
050300090317               if nrbolle = 1;
050400090317                 wncl = nrimg;
050500090317                 if impimg > *zeros and lacfimp = 'T';
050600090317                   wimp = lacimp;
050700090317                 endif;
050800090317                 if impimg > *zeros and lacfimp = 'I';
050900090317                   wimp = impimg * wncl;
051000090317                 endif;
051100090313       // --  scrivo addebito
051200090317                 exsr sr_wrtrig;
051300090317               ENDIF;
051400090317             ENDIF;
051500090317         ENDSL;
051600090313
051700090311       ENDSR;
051800090313
051900090313       //-----------------------------------------------------------------------
052000090313       // Controllo se e come srivere FNRIG
052100090313       //-----------------------------------------------------------------------
052200090313       begsr sr_wrtrig;
052300090330
052400090330         $add = *on;
052500090313
052600090313         clear fnrig000;
052700090313
052800090316       // spedizione solo se addebito ad immagine
052900090316         if lactadu = 'I';
053000090316           rigaas = 2000 + %dec(lacaas:2:0);
053100090316           riglnp = %dec(laclnp:3:0);
053200090316           rignrs = %dec(lacnrs:2:0);
053300090316           rignsp = %dec(lacnsp:7:0);
053400090316         ENDIF;
053500090313
053600090313       // cliente tassazione + tariffa
053700090313       // il cliente forzato + la tariffa sono memorizzati nel campo lacidl
053800090313       // prima però delle nuove modifiche questo campo conteneva il nome del lavoro che
053900090313       // scrive TILAC, per riconoscere i nuovi rcd dai vecchi uso il campo LACTELA che è nuovo
054000090313       // e viene impostato con le nuove modifiche
054100090313         if lactela <> *blanks;
054200090313       // per sicurezza comunque controllo anche il campo LACIDL
054300090317           if %check(c_digits:lacidl) = 0 and lacidl <> *blanks;
054400090313             rigksc = %dec(%subst(lacidl:1:7):7:0);
054500090313             rigctr = %subst(lacidl:8:3);
054600090313           ENDIF;
054700090313         ENDIF;
054800090313
054900090313        // tipo tassazione
055000090313         if lactad = 'S';
055100090313           rigtas = 'T';
055200090313         ENDIF;
055300090313         if lactad = 'V';
055400090313           rigtas = 'N';
055500090313         ENDIF;
055600090313
055700090313        // tassazione forzata
055800090313         if wimp > *zeros;
055900090313           rigimp = wimp;
056000090313           rigtas = 'F';
056100090313         endif;
056200090313
056300090313         rigcau = 'P';
056400090313         rigncl = wncl;
056500090313         rigtra = 'N';
056600090313         rigpru = 'TILAC';
056700090313         rigdata = woggi;
056800090313         rigora = wora;
056900090317
057000090320       // tassazione mensile memorizzo la data di spedizione
057100090320       // ultimo gg del mese precedente
057200090318         if lactadu = 'M';
057300090318           rigdsp = lacaddms*100+1;
057400090318           wdatiso = %date(rigdsp) + %months(1) - %days(1);
057500090318           rigdsp = %dec(wdatiso);
057600090317         endif;
057700090317
057800090313         write fnrig000;
057900090313
058000090313       ENDSR;
058100090311
058200090313       //-----------------------------------------------------------------------
058300090313       // Aggiorno TILAC
058400090313       //-----------------------------------------------------------------------
058500090313       begsr sr_agglac;
058600090313
058700090313       // se lacela non è più a '01' non aggiorno
058800090313         if lacela > '00' and lacela < '10';
058900090313           wlacela = %dec(lacela:2:0);
059000090313           wlacela = wlacela + 10;
059100090313           lacela = %editc(wlacela:'X');
059200090402        // imposto anche data spia add.
059300090402           lactimecd = %char(%timestamp:*iso0);
059400090313           update tilac000;
059500090313         ENDIF;
059600090313
059700090313       ENDSR;
059800090303
