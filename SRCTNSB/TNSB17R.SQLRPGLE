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
002600121213     d savdir          s                   like(lacdir)
002700090312     d xx              s              2  0
002800090311     d wdatiso         s               d
002900090311     d widjob          s                   like(lacidjob)
003000090313     d widl            s                   like(lacidl)
003100090313     d wimp            s                   like(rigimp)
003200090312     d wlacaddms       s                   like(lacaddms)
003300090313     d wlacela         s              2  0
003400090313     d wmonth          s              2  0
003500090313     d wncl            s                   like(rigncl)
003600050318     d wora            s              6  0
003700050318     d woggi           s              8  0
003800090318     d woggiiso        s               d
003900090311     d wtadu           s                   like(lactadu)
004000090312     d wyear           s              4  0
004100090313     d $add            s               n
004200090313     d $fine           s               n
004300090311     d $finelac        s               n
004400090616     d $finenim        s               n
004500090311
004600090311      //-----------------------------------------------------------------------
004700090313      //  Definizione strutture dati.
004800090311      //-----------------------------------------------------------------------
004900090317     d dvpo          e ds
005000090317     d kpjba         e ds
005100090317     d tibs02ds      e ds
005200090616     d tilacds       e ds                  extname(tilac00f) prefix(n_)
005300090313
005400090313      //-----------------------------------------------------------------------
005500090313      //  Definizione costanti.
005600090313      //----------------------------------------------------------------------
005700090313      // - Costante per controllo "caratteri solo numerici"
005800090313     d c_Digits        c                   const('0123456789')
005900090317
006000090317      //--------------------------------------------------------------
006100090317      // Definizione procedure usate.                                 ?
006200090317      //--------------------------------------------------------------
006300090317      /copy gaitrasrc/srcprotopr,tibs02r
006400090313
006500090311      //-----------------------------------------------------------------------
006600090311      // RIEPILOGO INDICATORI
006700090311      //-----------------------------------------------------------------------
006800040511
006900090311      //-----------------------------------------------------------------------
007000090311     c     *entry        plist
007100090311     c                   parm                    kpjba
007200090311
007300090311      /free
007400090317
007500090317       // n.colli possbili per addebito pod image a riepilogo
007600090317       clear tibs02ds;
007700090317       clear dvpo;
007800090317       t02tla = 'L';
007900090317       t02mod = 'C';
008000090317       t02sif = knsif;
008100090317       t02cod = 'VPO';
008200090317       t02ke1 = 'VPO';
008300090317       TNTBE_RicercaControllo(kpjba : tibs02ds);
008400090317       if t02err = *blank;
008500090317         dvpo = t02uni;
008600090317       else;
008700090320         §vpopod = 5000;
008800090317       endif;
008900090311
009000090313       // data/ora del job
009100090313       woggi = %dec(%date());
009200090313       wora  = %dec(%time());
009300090318       woggiiso = %date(woggi);
009400090311
009500090311       $fine = *off;
009600090311
009700090311       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009800090402
009900090402       // prima di tutto devo rimettere a '11' i rcd che hanno la spia add.
010000090402       // perchè vuol dire che non devo rifare l'addebito, sono stati messi
010100090402       // a '01' da laguna dopo la creazione di FNRIG.
010200090402       // Caso di: oggi scannerizzo, rcd a '00', laguna lo passa a '01' e addebito
010300090402       //          lo passa a '11', domani riscannerizzo, nuovo rcd a '00', laguna
010400090402       //          aggiorna tutti i rcd con stessa spedizione a '01', passa il pgm
010500090402       //          dell'addebito e mi fa 2 addebiti, non va bene, uno lo ha già fatto
010600090402       //          ieri, con la spia evitiamo il doppio addebito
010700090624       //          però solo se non sono rcd relativi ad un addebito a JOB
010800090402       exec sql
010900090402         update tilac00f
011000090402           set lacela = '11'
011100090402           where
011200090624           lactimecd <> ' ' and lacela > '00' and lacela < '10'
011300090624           and lactadu <> 'J';
011400090311
011500090313       // leggo tilac00f raggruppato per id job e ksc + ctr
011600090311       exec sql
011700090313         declare idjob cursor for select distinct lacidjob, lacidl,
011800090313                                                  lactadu
011900090402           from tilac03l where lacidjob <> ' '
012000110801                         and lacela not in('33', '34')
012100090313           group by lacidjob, lacidl, lactadu;
012200090311       exec sql
012300090311         open idjob;
012400090311             if sqlcode < 0;
012500090311               $fine = *on;
012600090311             ENDIF;
012700090311
012800090311       dow not $fine;
012900090311
013000090313         exsr sr_leggijob;
013100090311
013200090311       ENDDO;
013300090311
013400090311       *inlr = *on;
013500090311
013600090313       //-----------------------------------------------------------------------
013700090313       // Leggo tutti i Job di Tilac
013800090313       //-----------------------------------------------------------------------
013900090311       begsr sr_leggijob;
014000090311
014100090311         exec sql
014200090313           fetch next from idjob into :widjob, :widl, :wtadu;
014300090311
014400090313       // fine file esco
014500090311               if sqlcod = 100 or sqlcod < 0;
014600090311                 $fine = *on;
014700090311                 leavesr;
014800090311               ENDIF;
014900090311
015000090313       // a rottura di job
015100090313         if widjob <> savidjob;
015200090313           clear nrlac;
015300090313
015400090313       // per il job letto controllo se ci sono ancora dei rcd a stato '00' o
015500090317       // con timer maggiore uguale oggi
015600090313           savidjob = widjob;
015700090313           exec sql
015800090313             select count(*) into: nrlac from tilac00f
015900110801               where lacidjob = :widjob and lacela not in('33', '34')
016000110801               and (lacela = '00' or
016100090402                substr(lactim, 1, 8) >= digits(:woggi));
016200090313           if sqlcod <> 0;
016300090313             nrlac = 1;
016400090313             leavesr;
016500090313           ENDIF;
016600090313           if nrlac > 0;
016700090313             leavesr;
016800090313           ENDIF;
016900090316
017000090313       // se stesso job e non ok tutti i rcd non devo elaborare
017100090313         else;
017200090313           if nrlac > 0;
017300090313             leavesr;
017400090313           ENDIF;
017500090313
017600090313         ENDIF;
017700090313
017800090616       // se tutto ok per pod image con addebito a Job (CD) devo prima controllare se
017900090615       // ci sono immagini con stesso nome per non addebitarle
018000090616         if wtadu = 'J';
018100090616           exsr sr_nim;
018200090616         endif;
018300090615
018400090615
018500090313       // se tutto ok elaboro, se arrivo qua vuol dire che il job è completo
018600090313       // anche se ho + ksc e ctr per lo stesso job
018700090313         exsr sr_elabora;
018800090311
018900090311       ENDSR;
019000090616
019100090616       //-----------------------------------------------------------------------
019200090616       // Controllo se ci sono immagini con lo stesso nome all'interno del job
019300121213       // con stessa directory
019400090616       //-----------------------------------------------------------------------
019500090616       begsr sr_nim;
019600090616
019700090616         exec sql
019800090616           declare nim cursor for select rrn(tilac03l), tilac03l.*
019900090616           from tilac03l where lacidjob = :widjob
020000110801                         and lacela not in('33', '34')
020100121213           order by lacnim, lacdir;
020200090616         exec sql
020300090616           open nim;
020400090616             if sqlcode < 0;
020500090616               $finenim = *on;
020600090616             ENDIF;
020700090616
020800090616         dow not $finenim;
020900090616           exec sql
021000090616             fetch next from nim into :rrnlac, :tilacds;
021100090616
021200090616       // fine file esco
021300090616               if sqlcod = 100 or sqlcod < 0;
021400090616                 $finenim = *on;
021500090616                 leavesr;
021600090616               ENDIF;
021700090616
021800090616       // a immagine uguale aggiorno il rcd come già elaborato
021900090616             if n_lacnim <> savnim;
022000090616               savnim = n_lacnim;
022100121213               savdir = n_lacdir;
022200090616             else;
022300121213               if n_lacdir <> savdir;
022400121213               savdir = n_lacdir;
022500121213             else;
022600090616               chain rrnlac tilac00f;
022700090616               if %found(tilac00f);
022800090616                 l_lacela = '11';
022900090616                 l_lactimecd = %char(%timestamp:*iso0);
023000090616                 update tilac0;
023100090616               endif;
023200121213               endif;
023300090616             endif;
023400090616
023500090616         enddo;
023600090616
023700090616         exec sql
023800090616           close nim;
023900090616
024000090616       ENDSR;
024100090303
024200090313       //-----------------------------------------------------------------------
024300090313       // Elaboro il Job e ksc+ctr
024400090313       //-----------------------------------------------------------------------
024500090311       begsr sr_elabora;
024600090313
024700090316         clear nrimg;
024800090316
024900090316       // conto quante immagini devo impostare nella spedizione di addebito
025000090316         exec sql
025100090402           select count(*) into: nrimg from tilac03l
025200090402             where lacidjob = :widjob and lacidl = :widl;
025300090316           if sqlcod <> 0;
025400090316             leavesr;
025500090316           ENDIF;
025600090311
025700090313       // leggo tilac per JOB/ksc+ctr (LACIDL)
025800090311         exsr sr_leggilac;
025900090311
026000090311       ENDSR;
026100090311
026200090313       //-----------------------------------------------------------------------
026300090313       // Leggo TILAC per Job e ksc+ctr
026400090313       //-----------------------------------------------------------------------
026500090311       begsr sr_leggilac;
026600090313
026700090313        $finelac = *off;
026800090311
026900090313       // Leggo TILAC
027000090313        setll (widjob:widl) tilac03l;
027100090311
027200090311        dow not $finelac;
027300090313          reade (widjob:widl) tilac03l;
027400090311          if %eof(tilac03l);
027500090311            $finelac = *on;
027600090311            leave;
027700090311          ENDIF;
027800090317
027900090317       // scrivo fnrig se richiesto
028000090317          select;
028100090317            when lactad = 'N';
028200090330         // Aggiorno TILAC
028300090330            exsr sr_agglac;
028400090317
028500090317       // se frequenza addebito per Immagine scrivo un rcd di FNRIG x ogni immagine
028600090317            when lactadu = 'I';
028700090317             wncl = 1;
028800090317             wimp = lacimp;
028900090317             exsr sr_wrtrig;
029000090330         // Aggiorno TILAC
029100090330            exsr sr_agglac;
029200090317
029300090317       // se frequenza addebito a Job/gg o Settimana o Mese calcolo quanti rcd di
029400090317       // FNRIG devo scrivere
029500090317            when lactadu <> 'I';
029600090317       // per id job e ksc+ctr
029700090317              if lacidjob <> oldidjob or lacidl <> oldidl;
029800090401                $add = *off;
029900090317                exsr sr_conta;
030000090317                oldidjob = lacidjob;
030100090317                oldidl   = lacidl;
030200090317              endif;
030300090401           // Aggiorno TILAC
030400090401           // se scritto FNRIG
030500090401              if $add;
030600090401                exsr sr_agglac;
030700090401              endif;
030800090317
030900090317          endsl;
031000090311        ENDDO;
031100090317
031200090317        clear oldidjob;
031300090317        clear oldidl;
031400090311
031500090311       ENDSR;
031600090311
031700090313       //-----------------------------------------------------------------------
031800090317       // Conta quanti rcd bolle devo creare e i relativi colli + importo
031900090313       //-----------------------------------------------------------------------
032000090317       begsr sr_conta;
032100090317
032200090317         clear nrbolle;
032300090317         clear restoncl;
032400090317         clear wncl;
032500090317         clear wimp;
032600090317         clear impimg;
032700090317         clear savncl;
032800090317
032900090317       // conto quante bolle e i colli per bolla
033000090317         select;
033100090320       // controllo che il n. dei colli non superi il max consentito 5000
033200090320       // se supera devo fare più bolle
033300090317           when nrimg > §vpopod;
033400090317             nrbolle  = %div(nrimg:§vpopod);
033500090317             restoncl = %rem(nrimg:§vpopod);
033600090317             wncl = §vpopod;
033700090317             savncl = §vpopod;
033800090317           when nrimg <= §vpopod;
033900090317             nrbolle = 1;
034000090317             wncl = nrimg;
034100090317         endsl;
034200090317       // se ho resto aggiungo 1 al n. di bolle da creare
034300090317         if restoncl > 0;
034400090317           nrbolle = nrbolle + 1;
034500090317         endif;
034600090317
034700090317       // calcolo l'importo per bolla
034800090317         select;
034900090317       // se devo creare più bolle devo ripartire l'importo forzato a totale
035000090317           when nrbolle > 1 and lacfimp = 'T' and lacimp <> *zeros;
035100090317             impimg = lacimp / nrimg;
035200090317       // se devo creare 1 bolla imposto l'importo forzato a totale
035300090317           when nrbolle = 1 and lacfimp = 'T' and lacimp <> *zeros;
035400090317             wimp = lacimp;
035500090317       // se l'importo forzato è a immagine imposto l'importo a immagine
035600090317           when lacfimp = 'I' and lacimp <> *zeros;
035700090317             impimg = lacimp;
035800090317          ENDSL;
035900090312
036000090317       // scrive FNRIG in base alle frequenza addebito
036100090317         xx = 1;
036200090312
036300090317         select;
036400090313       // addebito per job scrivo un solo rcd di fnrig
036500090313       // con il totale dei rcd letti da tilac
036600090317           when lactadu = 'J';
036700090313
036800090716       // --> se le immagini superano il max consentito
036900090317             if nrbolle > 1;
037000090313       // --  devo ciclare per il n. di bolle da creare
037100090317               for xx by 1 to nrbolle;
037200090313       // --  importo a totale forzato
037300090317                 if impimg > *zeros and lacfimp = 'T';
037400090317                   wimp = impimg * wncl;
037500090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
037600090313       //       devo calcolare il totale
037700090317                   if xx = nrbolle and restoncl = 0;
037800090317                     wimp = lacimp - (impimg * savncl * (nrbolle -1));
037900090317                   endif;
038000090317                 endif;
038100090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
038200090317       //       imposto i colli
038300090317                 if xx = nrbolle and restoncl > 0;
038400090317                   wncl = restoncl;
038500090317       //       se importo a totale forzato calcolo il totale
038600090317                   if impimg > *zeros and lacfimp = 'T';
038700090317                     wimp = lacimp - (impimg * savncl * (nrbolle-1));
038800090317                   endif;
038900090317                 endif;
039000090313       // ----  importo a immagine forzato
039100090317                 if impimg > *zeros and lacfimp = 'I';
039200090317                   wimp = impimg * wncl;
039300090317                 endif;
039400090313       // ----  scrivo addebito
039500090317                 exsr sr_wrtrig;
039600090317               ENDFOR;
039700090317             ENDIF;
039800090313
039900090313       // --> ho una sola bolla da creare
040000090317             if nrbolle = 1;
040100090317               wncl = nrimg;
040200090317               if impimg > *zeros and lacfimp = 'T';
040300090317                 wimp = lacimp;
040400090317               endif;
040500090317               if impimg > *zeros and lacfimp = 'I';
040600090317                 wimp = impimg * wncl;
040700090317               endif;
040800090313       // --  scrivo addebito
040900090317               exsr sr_wrtrig;
041000090317             ENDIF;
041100090311
041200090313
041300090313       // addebito per settimana scrivo un solo rcd di fnrig
041400090313       // con il totale dei rcd letti da tilac
041500090317           when lactadu = 'S';
041600090313       // se sono nella settimana sucessiva a quella prevista per l'addebito
041700090317             exec sql
041800090318               set :nrweek = week_iso(:woggiiso);
041900090318             wyear = %subdt(woggiiso:*years);
042000090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(nrweek:'X'):6:0);
042100090317             if lacaddms < wlacaddms;
042200090313
042300090716       // --> se le immagini superano il max consentito
042400090317               if nrbolle > 1;
042500090313       // --  devo ciclare per il n. di bolle da creare
042600090317                 for xx by 1 to nrbolle;
042700090313       // --  importo a totale forzato
042800090317                   if impimg > *zeros and lacfimp = 'T';
042900090317                     wimp = impimg * wncl;
043000090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
043100090313       //       devo calcolare il totale
043200090317                     if xx = nrbolle and restoncl = 0;
043300090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
043400090317                     endif;
043500090317                   endif;
043600090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
043700090317       //       imposto i colli
043800090317                   if xx = nrbolle and restoncl > 0;
043900090317                     wncl = restoncl;
044000090317       //       importo a totale forzato calcolo il totale
044100090317                     if impimg > *zeros and lacfimp = 'T';
044200090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
044300090317                     endif;
044400090317                   endif;
044500090313       // ----  importo a immagine forzato
044600090317                   if impimg > *zeros and lacfimp = 'I';
044700090317                     wimp = impimg * wncl;
044800090317                   endif;
044900090313       // ----  scrivo addebito
045000090317                   exsr sr_wrtrig;
045100090317                 ENDFOR;
045200090317               ENDIF;
045300090313
045400090313       // --> ho una sola bolla da creare
045500090317               if nrbolle = 1;
045600090317                 wncl = nrimg;
045700090317                 if impimg > *zeros and lacfimp = 'T';
045800090317                   wimp = lacimp;
045900090317                 endif;
046000090317                 if impimg > *zeros and lacfimp = 'I';
046100090317                   wimp = impimg * wncl;
046200090317                 endif;
046300090313       // --  scrivo addebito
046400090317                 exsr sr_wrtrig;
046500090317               ENDIF;
046600090317             ENDIF;
046700090313
046800090311
046900090313       // addebito per mese scrivo un solo rcd di fnrig
047000090313       // con il totale dei rcd letti da tilac
047100090317           when lactadu = 'M';
047200090313       // se sono nel mese sucessivo a quella previsto per l'addebito
047300090318             wyear  = %subdt(woggiiso:*years);
047400090318             wmonth = %subdt(woggiiso:*months);
047500090317             wlacaddms = %dec(%editc(wyear:'X') + %editc(wmonth:'X'):6:0);
047600090317             if lacaddms < wlacaddms;
047700090313
047800090716       // --> se le immagini superano il max consentito
047900090317               if nrbolle > 1;
048000090313       // --  devo ciclare per il n. di bolle da creare
048100090317                 for xx by 1 to nrbolle;
048200090313       // --  importo a totale forzato
048300090317                   if impimg > *zeros and lacfimp = 'T';
048400090317                     wimp = impimg * wncl;
048500090313       // ----> se non ho resto di colli e sto elaborando l'ultima bolla
048600090313       //       devo calcolare il totale
048700090317                     if xx = nrbolle and restoncl = 0;
048800090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
048900090317                     endif;
049000090317                   endif;
049100090317       // ----> se ho resto di colli e sto elaborando l'ultima bolla
049200090317       //       imposto i colli
049300090317                   if xx = nrbolle and restoncl > 0;
049400090317                     wncl = restoncl;
049500090317       //       importo a totale forzato
049600090317                     if impimg > *zeros and lacfimp = 'T';
049700090317                       wimp = lacimp - (impimg * savncl * (nrbolle -1));
049800090317                     endif;
049900090317                   endif;
050000090313       // ----  importo a immagine forzato
050100090317                   if impimg > *zeros and lacfimp = 'I';
050200090317                     wimp = impimg * wncl;
050300090317                   endif;
050400090313       // ----  scrivo addebito
050500090317                   exsr sr_wrtrig;
050600090317                 ENDFOR;
050700090317               ENDIF;
050800090313
050900090313       // --> ho una sola bolla da creare
051000090317               if nrbolle = 1;
051100090317                 wncl = nrimg;
051200090317                 if impimg > *zeros and lacfimp = 'T';
051300090317                   wimp = lacimp;
051400090317                 endif;
051500090317                 if impimg > *zeros and lacfimp = 'I';
051600090317                   wimp = impimg * wncl;
051700090317                 endif;
051800090313       // --  scrivo addebito
051900090317                 exsr sr_wrtrig;
052000090317               ENDIF;
052100090317             ENDIF;
052200090317         ENDSL;
052300090313
052400090311       ENDSR;
052500090313
052600090313       //-----------------------------------------------------------------------
052700090313       // Controllo se e come srivere FNRIG
052800090313       //-----------------------------------------------------------------------
052900090313       begsr sr_wrtrig;
053000090330
053100090330         $add = *on;
053200090313
053300090313         clear fnrig000;
053400090313
053500090316       // spedizione solo se addebito ad immagine
053600090316         if lactadu = 'I';
053700090316           rigaas = 2000 + %dec(lacaas:2:0);
053800090316           riglnp = %dec(laclnp:3:0);
053900090316           rignrs = %dec(lacnrs:2:0);
054000090316           rignsp = %dec(lacnsp:7:0);
054100090316         ENDIF;
054200090313
054300090313       // cliente tassazione + tariffa
054400090313       // il cliente forzato + la tariffa sono memorizzati nel campo lacidl
054500090313       // prima però delle nuove modifiche questo campo conteneva il nome del lavoro che
054600090313       // scrive TILAC, per riconoscere i nuovi rcd dai vecchi uso il campo LACTELA che è nuovo
054700090313       // e viene impostato con le nuove modifiche
054800090313         if lactela <> *blanks;
054900090313       // per sicurezza comunque controllo anche il campo LACIDL
055000090317           if %check(c_digits:lacidl) = 0 and lacidl <> *blanks;
055100090313             rigksc = %dec(%subst(lacidl:1:7):7:0);
055200090313             rigctr = %subst(lacidl:8:3);
055300090313           ENDIF;
055400090313         ENDIF;
055500090313
055600090313        // tipo tassazione
055700090313         if lactad = 'S';
055800090313           rigtas = 'T';
055900090313         ENDIF;
056000090313         if lactad = 'V';
056100090313           rigtas = 'N';
056200090313         ENDIF;
056300090313
056400090313        // tassazione forzata
056500090313         if wimp > *zeros;
056600090313           rigimp = wimp;
056700090313           rigtas = 'F';
056800090313         endif;
056900090313
057000090313         rigcau = 'P';
057100090313         rigncl = wncl;
057200090313         rigtra = 'N';
057300090313         rigpru = 'TILAC';
057400090313         rigdata = woggi;
057500090313         rigora = wora;
057600090317
057700090320       // tassazione mensile memorizzo la data di spedizione
057800090320       // ultimo gg del mese precedente
057900090318         if lactadu = 'M';
058000090318           rigdsp = lacaddms*100+1;
058100090318           wdatiso = %date(rigdsp) + %months(1) - %days(1);
058200090318           rigdsp = %dec(wdatiso);
058300090317         endif;
058400090317
058500090313         write fnrig000;
058600090313
058700090313       ENDSR;
058800090311
058900090313       //-----------------------------------------------------------------------
059000090313       // Aggiorno TILAC
059100090313       //-----------------------------------------------------------------------
059200090313       begsr sr_agglac;
059300090313
059400090313       // se lacela non è più a '01' non aggiorno
059500090313         if lacela > '00' and lacela < '10';
059600090313           wlacela = %dec(lacela:2:0);
059700090313           wlacela = wlacela + 10;
059800090313           lacela = %editc(wlacela:'X');
059900090402        // imposto anche data spia add.
060000090402           lactimecd = %char(%timestamp:*iso0);
060100090313           update tilac000;
060200090313         ENDIF;
060300090313
060400090313       ENDSR;
060500090303
