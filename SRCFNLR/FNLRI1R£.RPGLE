000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300121107      * Localizzazione filiale CHI SONO BRT: trova i chi sono adiacenti
000400060427      *------------------------------------------------------------------------*
000500121003     ffiars02l  if   e           k disk
000600121003     ffnblt27l  if   e           k disk
000700121112     ffnblp01l  if   e           k disk
000800121003     ffnbrv07l  if   e           k disk
000900121003     ftabel00f  if   e           k disk
001000121003     fazorg01l  if   e           k disk
001100121005     ffnfvv01l  if   e           k disk
001200121009     ffnfgv01l  if   e           k disk
001300121003     ffnlri1d   cf   e             workstn
001400060428
001500060428      *------------------------------------------------------------------------*
001600060428      * riepilogo indicatori
001700060428      *------------------------------------------------------------------------*
001800060428      * 28 - messaggio errore
001900121009      * 90 - Errore video
002000060502
002100060502      *------------------------------------------------------------------------*
002200060502      * schiere
002300060502      *------------------------------------------------------------------------*
002400121009     d msg             s             78    dim(4) ctdata perrcd(1)
002500060427
002600060428      *------------------------------------------------------------------------*
002700060428      * campi di work
002800060428      *------------------------------------------------------------------------*
002900121005
003000121005     d data_eur        s               d   datfmt(*eur)
003100121009     d wblt            s              1
003200121009     d wbrv            s              1
003300121005     d ii              s              3  0 inz
003400121005     d n               s              3  0 inz
003500121005     d s               s              3  0 inz
003600121005     d wdnpg           s              4    inz
003700121005     d cat             S              1    DIM(20) inz
003800121005     d dec             S              3    DIM(20) inz
003900121005     d spg             S              1    DIM(20) inz
004000161003     d des             S              3    DIM(20) inz
004100121009     d def             S              1    DIM(10) inz
004200121005     d wdfs            s                   like(brvdfs)
004300121005     d whms            s                   like(brvhms)
004400121005     d wnpg            s                   like(brvnpg)
004500121005     d wpes            s                   like(brvpes)
004600121005     d wnps            s                   like(brvnps)
004700121005     d wfgs            s                   like(brvfgs)
004800121005     d wnfv            s                   like(brvnfv)
004900121005     d knot            s                   like(arsnot)
005000121003     d ktrc            s                   like(arstrc) inz('C')
005100121003     d $Fine           s               n   inz(*off)
005200121003     d $Video          s              2    inz('D1')
005300121031     d wchi            s                   like(ii1chi)
005400121112     d dataiso_cor     s               d   datfmt(*iso)
005500121112     d dataiso_sped    s               d   datfmt(*iso)
005600121112     d num_days        s             15p 0
005700060428
005800060428      *------------------------------------------------------------------------*
005900060428      * ds interne/esterne
006000060428      *------------------------------------------------------------------------*
006100121005     D* composizione del chi sono Bartolini
006200121005     d chib1           ds            14
006300121005     d  radchib1               1      2
006400121005     d  prgchib1               3     13s 0
006500121005     d  chkdig1               14     14s 0
006600121005     d chib2           ds            14
006700121005     d  radchib2               1      2
006800121005     d  prgchib2               3     13s 0
006900121005     d  chkdig2               14     14s 0
007000060502     d kpjba         e ds
007100121031     d fnlri1ds      e ds                  inz
007200121031     d fnlsz1ds      e ds                  inz
007300121003     d ds1b          e ds                  inz
007400121005     d ds7n          e ds                  inz
007500161003     d ds7b          e ds                  inz
007600121009     d dstv          e ds                  inz
007700121003     d tibs34ds      e ds                  inz
007800060428     d §azute        e ds                  extname(azute00f)
007900060428     d                                     dtaara
008000060428     d §datiute      e ds                  extname(ddatiute)
008100060428     d                                     dtaara
008200060428     d psds           sds
008300060428     d  pgmname          *proc
008400060428
008500090424      // ? PROTOTIPI ?
008600090424      /copy gaitrasrc/srcprotopr,tibs34r
008700121003     d fnlsz1r         pr                  extpgm('FNLSZ1R')
008800121003     d  kpjba                              likeds(kpjba)
008900121003     d  fnlsz1ds                           likeds(fnlsz1ds)
009000121003     d  ds1b                               likeds(ds1b)
009100060428
009200060428      *------------------------------------------------------------------------*
009300060428      * costanti
009400060428      *------------------------------------------------------------------------*
009500010503
009600060428      *------------------------------------------------------------------------*
009700121003     c     *Entry        plist
009800121003     c                   parm                    kpjba
009900121031     c                   eval      fnlri1ds=kpjbu
010000121031     c                   clear                   oi1err
010100121031     c                   clear                   oi1msg
010200120621
010300120704      /free
010400121003       exsr RoutInz;
010500121003
010600121031       // Pgm richiamato
010700121031           if   ii1chi<>' ' ;
010800121031             exsr ctrd01;
010900121031             if *in90;
011000121031                oi1err='1';
011100121031                oi1msg=vidmsg;
011200121031                kpjbu=fnlri1ds;
011300121031                exsr routend;
011400121031             else;
011500121031                $video='D2';
011600121031             endif;
011700121031           endif;
011800121031
011900121003       DOW $Fine = *off;
012000121003         SELECT;
012100121031       // Prima videata
012200121003           WHEN $Video = 'D1';
012300121003             exsr GesD01;
012400121031       // Seconda videata
012500121003           WHEN $Video = 'D2';
012600121003             exsr Gesd02;
012700121003           OTHER;
012800121003             $Fine = *on;
012900121003         ENDSL;
013000121003       ENDDO;
013100121003
013200121003       exsr RoutEnd;
013300060428
013400121003       // ------------------------------------------------------------------------
013500121003       // Operazioni iniziali
013600121003       // ------------------------------------------------------------------------
013700121003         begsr RoutInz;
013800121003
013900121003       // Dati Job
014000060428         in(e) §azute;
014100060428         if not %error;
014200060428          in(e) §datiute;
014300060428         endif;
014400060502         if %error or rsut = *blanks;
014500060428          tibs34r(tibs34ds);
014600060428          in §azute;
014700060428          in §datiute;
014800060428         endif;
014900121005
015000121009       // Tabella Categoria fogli
015100121005         clear ii;
015200121005         clear cat;
015300121005         clear dec;
015400121005         setll (1:'7N') tabel00f;
015500121005         reade (1:'7N') tabel00f;
015600121005         dow not %eof(tabel00f);
015700121005            if tblflg = ' ';
015800121005               ii+=1;
015900121005               ds7n=tbluni;
016000121005               cat(ii)=tblkey;
016100121005               dec(ii)=§7ndes;
016200121005            endif;
016300121005         reade (1:'7N') tabel00f;
016400121005         enddo;
016500121009       // Tabella Tipi inventario
016600121005         clear ii;
016700121005         clear spg;
016800121005         clear des;
016900121005         setll (1:'7B') tabel00f;
017000121005         reade (1:'7B') tabel00f;
017100121005         dow not %eof(tabel00f);
017200121005            if tblflg = ' ';
017300121005               ii+=1;
017400161003               ds7b=tbluni ;
017500121005               spg(ii)=tblkey;
017600161003               des(ii)=§7BDESB ;
017700121005            endif;
017800121005         reade (1:'7B') tabel00f;
017900121005         enddo;
018000121009       // Tabella Tipi traino defluenza
018100121009         clear ii;
018200121009         clear def;
018300121009         setll (1:'TV') tabel00f;
018400121009         reade (1:'TV') tabel00f;
018500121009         dow not %eof(tabel00f);
018600121009            dstv=tbluni;
018700121009            if tblflg = ' ' and §tvdef='S';
018800121009               ii+=1;
018900121009               def(ii)=tblkey;
019000121009            endif;
019100121010         reade (1:'TV') tabel00f;
019200121009         enddo;
019300060428
019400121112       // Reperisco la data corrente
019500121112       dataiso_cor=(%date());
019600121112
019700060428         endsr;
019800060502
019900060502       // ------------------------------------------------------------------------
020000060502       // uscita dal programma
020100060502       // ------------------------------------------------------------------------
020200121003         begsr RoutEnd;
020300060502
020400060502          *inlr = *on;
020500060502          return;
020600060502
020700060502         endsr;
020800121003       // ------------------------------------------------------------------------
020900121003       // Gestione prima videata - Richiesta Codice chi sono
021000121003       // ------------------------------------------------------------------------
021100121003         begsr Gesd01;
021200121009         if not *in90;
021300121009            clear d01chi;
021400121009         endif;
021500121003         write lri1t01;
021600121003         dow $video='D1';
021700121003            exfmt lri1d01;
021800121003            *in28=*off;
021900121003            *in90=*off;
022000121003            clear vidmsg;
022100121003            select;
022200121003       // f3=Fine
022300121003            when *inkc;
022400121003               $fine=*on;
022500121003               leavesr;
022600121003            other;
022700121003               exsr ctrd01;
022800121003       // no errore passo all'altra videata
022900121003               if not *in90;
023000121003                  $video='D2';
023100121003               endif;
023200121003            endsl;
023300121003         enddo;
023400121003         endsr;
023500121003       // ------------------------------------------------------------------------
023600121003       // Controlli prima videata -
023700121003       // ------------------------------------------------------------------------
023800121003         begsr ctrd01;
023900121031       // Se     richiamato utilizzo il chi sono ricevuto
024000121031       // Se non richiamato utilizzo il chi sono della prima videata
024100121031         if ii1chi<>' ';
024200121031            wchi=ii1chi;
024300121031         else;
024400121031            wchi=d01chi;
024500121031         endif;
024600121031
024700121003         clear fnlsz1ds;
024800121003         iz1mod='C';
024900121031         iz1ch1=wchi;
025000121003         clear ds1b;
025100121003         fnlsz1r(kpjba:fnlsz1ds:ds1b);
025200121003       // Errore se non si tratta di chi sono BRT
025300121003         if oz1err<>*blanks ;
025400121003            *in90=*on;
025500121003            *in28=*on;
025600121003            vidmsg=msg(1);
025700121031            leavesr;
025800121003         endif;
025900121112       // Errore se CHI sono presente su una bolla non più vecchia di 40 gg rispetto
026000121112       // ad oggi:in questo caso non ho bisogno di cercare i chi sono adiacenti
026100121031         knot=wchi;
026200121112         setll (knot:ktrc) fiars02l;
026300121112         reade (knot:ktrc) fiars02l;
026400121112         dow not %eof(fiars02l);
026500121112             dataiso_sped=%date(arsduv:*iso);
026600121112             eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
026700121112             if num_days<=40;
026800121112                *in90=*on;
026900121112                *in28=*on;
027000121112                chain (arsfls:arslna:arsnrs:arsnsc) fnblt27l;
027100121112                if %found(fnblt27l);
027200121112                     vidmsg=msg(2);
027300121112                     vidmsg=%trim(vidmsg)+ ' ' + %char(bltlnp);
027400121112                     if bltnrs>0;
027500121112                        vidmsg=%trim(vidmsg)+ ' ' + %char(bltnrs) + '-' +
027600121112                        %char(bltnsp);
027700121112                     else;
027800121112                        vidmsg=%trim(vidmsg) + ' ' + %char(bltnsp) ;
027900121112                     endif;
028000121112                     vidmsg=%trim(vidmsg) + ' ' + %char(bltaas);
028100121112                else;
028200121112       // se non trovata spedizione fornisco dati del segnacollo
028300121112                     vidmsg=msg(3);
028400121112                     vidmsg=%trim(vidmsg)+ ' ' + %char(arsfls) +
028500121112                                     '/' + %char(arslna);
028600121112                     if arsnrs>0;
028700121112                        vidmsg=%trim(vidmsg)+ ' ' + %char(arsnrs) + '-' +
028800121112                        %char(arsnsc);
028900121112                     else;
029000121112                        vidmsg=%trim(vidmsg) + ' ' + %char(arsnsc) ;
029100121112                     endif;
029200121112                endif;
029300121112                leavesr;
029400121112             endif;
029500121112             reade (knot:ktrc) fiars02l;
029600121112         enddo;
029700121003         endsr;
029800121003       // ------------------------------------------------------------------------
029900121003       // Gestione prima videata - Richiesta Codice chi sono
030000121003       // ------------------------------------------------------------------------
030100121003         begsr Gesd02;
030200121003       // Caricamento video
030300121003         exsr card02;
030400121005       // Se  non trovati codici adiacenti lo segnalo con messaggio di errore
030500121005         if d02inf=*blanks and d02sup=*blanks;
030600121009            *in28=*on;
030700121009            *in90=*on;
030800121009            vidmsg=msg(4);
030900121031            if ii1chi<>' ';
031000161003                oi1err='1';
031100161003                oi1msg=vidmsg;
031200161003                kpjbu=fnlri1ds;
031300121031               $fine=*on;
031400121031               leavesr    ;
031500121031            else;
031600121031               $video='D1';
031700121031               leavesr    ;
031800121031            endif;
031900121005         endif;
032000121009         if d02inf=*blanks;
032100121009            d02inf='Non presente';
032200121009         endif;
032300121009         if d02sup=*blanks;
032400121009            d02sup='Non presente';
032500121009         endif;
032600121003         write lri1t01;
032700121003         dow $video='D2';
032800121009            exfmt lri1d02;
032900121003            *in28=*off;
033000121003            *in90=*off;
033100121003            clear vidmsg;
033200121003            select;
033300121003       // f3=Fine
033400121003            when *inkc;
033500121003               $fine=*on;
033600121003               leavesr;
033700121031       // f12=Ritorno  se pgm non richiamato
033800121031            when *inkl and ii1chi=' ';
033900121003               $video='D1';
034000121003               leavesr;
034100121031       // f12=Ritorno  se pgm richiamato
034200121031            when *inkl and ii1chi<>' ';
034300121031               $fine =*on;
034400121031               leavesr;
034500121003            other;
034600121003            endsl;
034700121003         enddo;
034800121003         endsr;
034900121003       // ------------------------------------------------------------------------
035000121003       // Caricamento seconda videata
035100121003       // ------------------------------------------------------------------------
035200121003         begsr card02;
035300121003
035400121003         clear lri1d02;
035500121031         d02chi=wchi;
035600121003       // Cerco il chi sono precedente
035700121003         setll (d02chi) fiars02l;
035800121005         readp  fiars02l;
035900121009         dow not %eof(fiars02l)and %subst(d02chi:1:2)=%subst(arsnot:1:2);
036000121005            if arstrc='C';
036100121005               clear fnlsz1ds;
036200121005               iz1mod='C';
036300121005               iz1ch1=arsnot;
036400121005               clear ds1b;
036500121005               fnlsz1r(kpjba:fnlsz1ds:ds1b);
036600121005       // Errore se non si tratta di chi sono BRT
036700121005               if oz1err=*blanks ;
036800121112       // Lo carico solo se data spedizione non inferiore di 40 giorni da oggi
036900121112       //         exsr rep_dsp;
037000121112                  dataiso_sped=%date(arsduv:*iso);
037100121112                  eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
037200121112                  if num_days<=40;
037300121112                     d02inf=arsnot;
037400121112                     exsr repdati;
037500121112                     exsr carvidi;
037600121112                     leave;
037700121112                  endif;
037800121112                  readp fiars02l;
037900121005               else;
038000121005                  readp fiars02l;
038100121005               endif;
038200121005            else;
038300121005               readp fiars02l;
038400121005            endif;
038500121003         enddo;
038600121003       // Cerco il chi sono successivo
038700121005         setll (d02chi) fiars02l;
038800121005         read fiars02l;
038900121009         dow not %eof(fiars02l) and %subst(d02chi:1:2)=%subst(arsnot:1:2);
039000121005            if arstrc='C';
039100121005               clear fnlsz1ds;
039200121005               iz1mod='C';
039300121005               iz1ch1=arsnot;
039400121005               clear ds1b;
039500121005               fnlsz1r(kpjba:fnlsz1ds:ds1b);
039600121005       // Errore se non si tratta di chi sono BRT
039700121005               if oz1err=*blanks ;
039800121112       // Lo carico solo se data spedizione non inferiore di 40 giorni da oggi
039900121112       //         exsr rep_dsp;
040000121112                  dataiso_sped=%date(arsduv:*iso);
040100121112                  eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
040200121112                  if num_days<=40;
040300121112                     d02sup=arsnot;
040400121112                     exsr repdati;
040500121112                     exsr carvids;
040600121112                     leave;
040700121112                  endif;
040800121112                  read fiars02l;
040900121005               else;
041000121005                  read fiars02l;
041100121005               endif;
041200121005            else;
041300121005               read fiars02l;
041400121005            endif;
041500121005         enddo;
041600121003         endsr;
041700121112       // ------------------------------------------------------------------------
041800121112       // Reperimento dati della spedizione e della prima spunta del collo
041900121112       // ------------------------------------------------------------------------
042000121112         begsr rep_dsp;
042100121112
042200121112         num_days=*hival;
042300121112
042400121112         chain (arsfls:arslna:arsnrs:arsnsc) fnblt27l;
042500121112         if %found(fnblt27l);
042600121112            chain (bltaas:bltlnp:bltnrs:bltnsp) fnblp01l;
042700121112               if %found(fnblp01l);
042800121112                  dataiso_sped=%date((blpaas*10000+blpmgs):*iso);
042900121112                  eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
043000121112               endif;
043100121112         endif;
043200121112
043300121112         endsr;
043400121005       // ------------------------------------------------------------------------
043500121005       // Reperimento dati della spedizione e della prima spunta del collo
043600121005       // ------------------------------------------------------------------------
043700121005         begsr repdati;
043800121005       //Numero spedizione
043900121009         clear wblt;
044000121005         chain (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnblt27l;
044100121009         if %found(fnblt27l);
044200121009            wblt='S';
044300121009         endif;
044400121005       //Prima spunta del collo
044500121005         wdfs=*hival;
044600121005         clear whms;
044700121005         clear wnpg;
044800121005         clear wpes;
044900121005         clear wnps;
045000121005         clear wfgs;
045100121005         clear wnfv;
045200121005         clear wbrv;
045300121005         setll (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnbrv07l;
045400121005         reade (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnbrv07l;
045500121005         dow not %eof (fnbrv07l);
045600121005         if brvdfs<wdfs or (brvdfs=wdfs and brvhms<whms);
045700121005            wbrv='S';
045800121005            wdfs=brvdfs;
045900121005            whms=brvhms;
046000121005            wnpg=brvnpg;
046100121005            wpes=brvpes;
046200121005            wnps=brvnps;
046300121005            wnfv=brvnfv;
046400121005            wfgs=brvfgs;
046500121005         endif;
046600121005         reade (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnbrv07l;
046700121005         enddo;
046800121005         endsr;
046900121005       // ------------------------------------------------------------------------
047000121005       // Caricamento videata: dati del collo precedente
047100121005       // ------------------------------------------------------------------------
047200121005         begsr carvidi;
047300121008       // Spedizione
047400121009         if wblt='S';
047500121009            d02lnpi=bltlnp;
047600121009            if bltnrs>0;
047700121009               d02nspi=%editc(bltnrs:'X')+'-'+%editc(bltnsp:'Z');
047800121009            else;
047900121009               d02nspi=%editc(bltnsp:'Z');
048000121009            endif;
048100121009            d02aasi=bltaas;
048200121009         endif;
048300121009       // Segnacollo
048400121009         d02flsi=%editc(arsfls:'X')+'/'+%editc(arslna:'X');
048500121009         if arsnrs>0;
048600121009            d02nsci=%editc(arsnrs:'X')+'-'+%editc(arsnsc:'Z');
048700121009         else;
048800121009            d02nsci=%editc(arsnsc:'Z');
048900121009         endif;
049000121005       // Categoria di spunta
049100121005         if wbrv='S';
049200121005         eval d02cati=%editc(wnpg:'X');
049300121005         exsr impo_dnpg;
049400121005         eval d02cati=%trim(d02cati)+ '-' + wdnpg;
049500121005       // filiale
049600121005         d02pesi=%editc(wpes:'X');
049700121005         chain wpes azorg01l;
049800121008         d02pesi=%trim(d02pesi)+ ' ' + orgdes;
049900121005       // Data immissione spunta
050000121005         data_eur=%date(wdfs:*iso);
050100121005         d02dfsi=%dec(data_eur);
050200121005       // Pistola
050300121008         d02npsi=%editc(wnps:'X');
050400121008       //
050500121008         d02dii='di';
050600121008         d02deli='del';
050700121008         d02psti='Pst';
050800121005         endif;
050900121005       // Scarto fra i due chi sono
051000121005         chib1=d02chi;
051100121005         chib2=d02inf;
051200121005         d02numi=prgchib1-prgchib2;
051300121005         endsr;
051400121005       // ------------------------------------------------------------------------
051500121005       // Caricamento videata: dati del collo successivo
051600121005       // ------------------------------------------------------------------------
051700121005         begsr carvids;
051800121009         if wblt='S';
051900121009            d02lnps=bltlnp;
052000121009            if bltnrs>0;
052100121009               d02nsps=%editc(bltnrs:'X')+'-'+%editc(bltnsp:'Z');
052200121009            else;
052300121009               d02nsps=%editc(bltnsp:'Z');
052400121009            endif;
052500121009            d02aass=bltaas;
052600121009         endif;
052700121009       // Segnacollo
052800121009         d02flss=%editc(arsfls:'X')+'/'+%editc(arslna:'X');
052900121009         if arsnrs>0;
053000121009            d02nscs=%editc(arsnrs:'X')+'-'+%editc(arsnsc:'Z');
053100121009         else;
053200121009            d02nscs=%editc(arsnsc:'Z');
053300121009         endif;
053400121005         if wbrv='S';
053500121005       // Categoria di spunta
053600121005         eval d02cats=%editc(wnpg:'X');
053700121005         exsr impo_dnpg;
053800121005         eval d02cats=%trim(d02cats)+ '-' + wdnpg;
053900121005       // filiale
054000121005         d02pess=%editc(wpes:'X');
054100121005         chain wpes azorg01l;
054200121008         d02pess=%trim(d02pess)+ ' ' + orgdes;
054300121005       // Data immissione spunta
054400121005         data_eur=%date(wdfs:*iso);
054500121005         d02dfss=%dec(data_eur);
054600121005       // Pistola
054700121008         d02npss=%editc(wnps:'X');
054800121008         d02dis='di';
054900121008         d02dels='del';
055000121008         d02psts='Pst';
055100121005         endif;
055200121005       // Scarto fra i due chi sono
055300121005         chib1=d02sup;
055400121005         chib2=d02chi;
055500121005         d02nums=prgchib1-prgchib2;
055600121005         endsr;
055700121005       // ------------------------------------------------------------------------
055800121005       // Impostazione campo descrizione categoria spunta
055900121005       // ------------------------------------------------------------------------
056000121005         begsr impo_dnpg;
056100121005         clear wdnpg;
056200121005         n=%lookup(%editc(wnpg:'X'):cat) ;
056300121005         if wnpg<>1;
056400121005            clear fvvspg;
056500121005            chain (wnpg:wnfv:wfgs) fnfvv01l;
056600121005            if fvvspg<>*blanks;
056700121005               s=%lookup(fvvspg:spg);
056800161003               eval wdnpg=des(s);
056900121005            else ;
057000121005               eval wdnpg=dec(n);
057100121005            endif;
057200121005         else;
057300121009             chain (wnfv:wfgs) fnfgv01l;
057400121009             if %found(fnfgv01l) and %lookup(fgvttr:def)>0;
057500121009                eval wdnpg='DEF';
057600121009             else;
057700121009                eval wdnpg=dec(n);
057800121009             endif;
057900121005         endif;
058000121005         endsr;
058100121005
058200121005      /end-free
058300060502
058400060502** -MSG-                                                                     *
058500121031Il codice immesso non è un "CHI SONO"  BRT                                     01
058600121003Codice CHI SONO presente sulla spedizione                                      02
058700121112Codice CHI SONO attribuito al segnacollo                                       03
058800121009Non tovati codici CHI SONO adiacenti al codice richiesto                       04
