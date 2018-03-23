000100060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200060427      *------------------------------------------------------------------------*
000300121107      * Localizzazione filiale CHI SONO BRT: trova i chi sono adiacenti
000400060427      *------------------------------------------------------------------------*
000500121003     ffiars02l  if   e           k disk
000501121003     ffnblt27l  if   e           k disk
000502121112     ffnblp01l  if   e           k disk
000503121003     ffnbrv07l  if   e           k disk
000504121003     ftabel00f  if   e           k disk
000505121003     fazorg01l  if   e           k disk
000506121005     ffnfvv01l  if   e           k disk
000507121009     ffnfgv01l  if   e           k disk
000800121003     ffnlri1d   cf   e             workstn
001000060428
001100060428      *------------------------------------------------------------------------*
001200060428      * riepilogo indicatori
001300060428      *------------------------------------------------------------------------*
002500060428      * 28 - messaggio errore
002501121009      * 90 - Errore video
002900060502
003000060502      *------------------------------------------------------------------------*
003100060502      * schiere
003200060502      *------------------------------------------------------------------------*
003300121009     d msg             s             78    dim(4) ctdata perrcd(1)
003400060427
003500060428      *------------------------------------------------------------------------*
003600060428      * campi di work
003700060428      *------------------------------------------------------------------------*
005603121005
005604121005     d data_eur        s               d   datfmt(*eur)
005605121009     d wblt            s              1
005606121009     d wbrv            s              1
005607121005     d ii              s              3  0 inz
005608121005     d n               s              3  0 inz
005609121005     d s               s              3  0 inz
005610121005     d wdnpg           s              4    inz
005611121005     d cat             S              1    DIM(20) inz
005612121005     d dec             S              3    DIM(20) inz
005613121005     d spg             S              1    DIM(20) inz
005614121005     d des             S              1    DIM(20) inz
005615121009     d def             S              1    DIM(10) inz
005616121005     d wdfs            s                   like(brvdfs)
005617121005     d whms            s                   like(brvhms)
005618121005     d wnpg            s                   like(brvnpg)
005619121005     d wpes            s                   like(brvpes)
005620121005     d wnps            s                   like(brvnps)
005621121005     d wfgs            s                   like(brvfgs)
005622121005     d wnfv            s                   like(brvnfv)
005623121005     d knot            s                   like(arsnot)
005624121003     d ktrc            s                   like(arstrc) inz('C')
005625121003     d $Fine           s               n   inz(*off)
005626121003     d $Video          s              2    inz('D1')
005627121031     d wchi            s                   like(ii1chi)
005628121112     d dataiso_cor     s               d   datfmt(*iso)
005629121112     d dataiso_sped    s               d   datfmt(*iso)
005630121112     d num_days        s             15p 0
005800060428
005900060428      *------------------------------------------------------------------------*
006000060428      * ds interne/esterne
006100060428      *------------------------------------------------------------------------*
006101121005     D* composizione del chi sono Bartolini
006102121005     d chib1           ds            14
006103121005     d  radchib1               1      2
006104121005     d  prgchib1               3     13s 0
006105121005     d  chkdig1               14     14s 0
006106121005     d chib2           ds            14
006107121005     d  radchib2               1      2
006108121005     d  prgchib2               3     13s 0
006109121005     d  chkdig2               14     14s 0
006900060502     d kpjba         e ds
007000121031     d fnlri1ds      e ds                  inz
007001121031     d fnlsz1ds      e ds                  inz
007002121003     d ds1b          e ds                  inz
007003121005     d ds7n          e ds                  inz
007004121009     d dstv          e ds                  inz
007005121003     d tibs34ds      e ds                  inz
007200060428     d §azute        e ds                  extname(azute00f)
007300060428     d                                     dtaara
007400060428     d §datiute      e ds                  extname(ddatiute)
007500060428     d                                     dtaara
007700060428     d psds           sds
007800060428     d  pgmname          *proc
007900060428
008701090424      // ? PROTOTIPI ?
008702090424      /copy gaitrasrc/srcprotopr,tibs34r
008703121003     d fnlsz1r         pr                  extpgm('FNLSZ1R')
008704121003     d  kpjba                              likeds(kpjba)
008705121003     d  fnlsz1ds                           likeds(fnlsz1ds)
008706121003     d  ds1b                               likeds(ds1b)
008800060428
008900060428      *------------------------------------------------------------------------*
009000060428      * costanti
009100060428      *------------------------------------------------------------------------*
009400010503
009500060428      *------------------------------------------------------------------------*
009501121003     c     *Entry        plist
009502121003     c                   parm                    kpjba
009503121031     c                   eval      fnlri1ds=kpjbu
009504121031     c                   clear                   oi1err
009505121031     c                   clear                   oi1msg
009803120621
009804120704      /free
009805121003       exsr RoutInz;
009806121003
009807121031       // Pgm richiamato
009808121031           if   ii1chi<>' ' ;
009809121031             exsr ctrd01;
009810121031             if *in90;
009811121031                oi1err='1';
009812121031                oi1msg=vidmsg;
009813121031                kpjbu=fnlri1ds;
009814121031                exsr routend;
009815121031             else;
009816121031                $video='D2';
009817121031             endif;
009818121031           endif;
009819121031
009820121003       DOW $Fine = *off;
009821121003         SELECT;
009822121031       // Prima videata
009823121003           WHEN $Video = 'D1';
009824121003             exsr GesD01;
009825121031       // Seconda videata
009826121003           WHEN $Video = 'D2';
009827121003             exsr Gesd02;
009828121003           OTHER;
009829121003             $Fine = *on;
009830121003         ENDSL;
009831121003       ENDDO;
009832121003
009833121003       exsr RoutEnd;
105300060428
105401121003       // ------------------------------------------------------------------------
105402121003       // Operazioni iniziali
105403121003       // ------------------------------------------------------------------------
105404121003         begsr RoutInz;
105405121003
105406121003       // Dati Job
105500060428         in(e) §azute;
105600060428         if not %error;
105700060428          in(e) §datiute;
105800060428         endif;
105900060502         if %error or rsut = *blanks;
106000060428          tibs34r(tibs34ds);
106100060428          in §azute;
106200060428          in §datiute;
106300060428         endif;
106301121005
106302121009       // Tabella Categoria fogli
106303121005         clear ii;
106304121005         clear cat;
106305121005         clear dec;
106306121005         setll (1:'7N') tabel00f;
106307121005         reade (1:'7N') tabel00f;
106308121005         dow not %eof(tabel00f);
106309121005            if tblflg = ' ';
106310121005               ii+=1;
106311121005               ds7n=tbluni;
106312121005               cat(ii)=tblkey;
106313121005               dec(ii)=§7ndes;
106314121005            endif;
106315121005         reade (1:'7N') tabel00f;
106316121005         enddo;
106317121009       // Tabella Tipi inventario
106318121005         clear ii;
106319121005         clear spg;
106320121005         clear des;
106321121005         setll (1:'7B') tabel00f;
106322121005         reade (1:'7B') tabel00f;
106323121005         dow not %eof(tabel00f);
106324121005            if tblflg = ' ';
106325121005               ii+=1;
106326121005               spg(ii)=tblkey;
106327121005               des(ii)=tbluni;
106328121005            endif;
106329121005         reade (1:'7B') tabel00f;
106330121005         enddo;
106331121009       // Tabella Tipi traino defluenza
106332121009         clear ii;
106333121009         clear def;
106335121009         setll (1:'TV') tabel00f;
106336121009         reade (1:'TV') tabel00f;
106337121009         dow not %eof(tabel00f);
106338121009            dstv=tbluni;
106339121009            if tblflg = ' ' and §tvdef='S';
106340121009               ii+=1;
106341121009               def(ii)=tblkey;
106342121009            endif;
106343121010         reade (1:'TV') tabel00f;
106344121009         enddo;
106400060428
106401121112       // Reperisco la data corrente
106402121112       dataiso_cor=(%date());
106405121112
106500060428         endsr;
106600060502
106700060502       // ------------------------------------------------------------------------
106800060502       // uscita dal programma
106900060502       // ------------------------------------------------------------------------
107000121003         begsr RoutEnd;
107100060502
107800060502          *inlr = *on;
107900060502          return;
108000060502
108100060502         endsr;
108101121003       // ------------------------------------------------------------------------
108102121003       // Gestione prima videata - Richiesta Codice chi sono
108103121003       // ------------------------------------------------------------------------
108104121003         begsr Gesd01;
108105121009         if not *in90;
108106121009            clear d01chi;
108107121009         endif;
108108121003         write lri1t01;
108109121003         dow $video='D1';
108110121003            exfmt lri1d01;
108111121003            *in28=*off;
108112121003            *in90=*off;
108113121003            clear vidmsg;
108114121003            select;
108115121003       // f3=Fine
108116121003            when *inkc;
108117121003               $fine=*on;
108118121003               leavesr;
108119121003            other;
108120121003               exsr ctrd01;
108121121003       // no errore passo all'altra videata
108122121003               if not *in90;
108123121003                  $video='D2';
108124121003               endif;
108125121003            endsl;
108126121003         enddo;
108127121003         endsr;
108128121003       // ------------------------------------------------------------------------
108129121003       // Controlli prima videata -
108130121003       // ------------------------------------------------------------------------
108131121003         begsr ctrd01;
108132121031       // Se     richiamato utilizzo il chi sono ricevuto
108133121031       // Se non richiamato utilizzo il chi sono della prima videata
108134121031         if ii1chi<>' ';
108135121031            wchi=ii1chi;
108136121031         else;
108137121031            wchi=d01chi;
108138121031         endif;
108139121031
108140121003         clear fnlsz1ds;
108141121003         iz1mod='C';
108145121031         iz1ch1=wchi;
108147121003         clear ds1b;
108148121003         fnlsz1r(kpjba:fnlsz1ds:ds1b);
108149121003       // Errore se non si tratta di chi sono BRT
108150121003         if oz1err<>*blanks ;
108151121003            *in90=*on;
108152121003            *in28=*on;
108153121003            vidmsg=msg(1);
108154121031            leavesr;
108155121003         endif;
108156121112       // Errore se CHI sono presente su una bolla non più vecchia di 40 gg rispetto
108157121112       // ad oggi:in questo caso non ho bisogno di cercare i chi sono adiacenti
108161121031         knot=wchi;
108163121112         setll (knot:ktrc) fiars02l;
108164121112         reade (knot:ktrc) fiars02l;
108165121112         dow not %eof(fiars02l);
108172121112             dataiso_sped=%date(arsduv:*iso);
108173121112             eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
108174121112             if num_days<=40;
108178121112                *in90=*on;
108179121112                *in28=*on;
108180121112                chain (arsfls:arslna:arsnrs:arsnsc) fnblt27l;
108181121112                if %found(fnblt27l);
108182121112                     vidmsg=msg(2);
108183121112                     vidmsg=%trim(vidmsg)+ ' ' + %char(bltlnp);
108184121112                     if bltnrs>0;
108185121112                        vidmsg=%trim(vidmsg)+ ' ' + %char(bltnrs) + '-' +
108186121112                        %char(bltnsp);
108187121112                     else;
108188121112                        vidmsg=%trim(vidmsg) + ' ' + %char(bltnsp) ;
108189121112                     endif;
108190121112                     vidmsg=%trim(vidmsg) + ' ' + %char(bltaas);
108191121112                else;
108192121112       // se non trovata spedizione fornisco dati del segnacollo
108193121112                     vidmsg=msg(3);
108194121112                     vidmsg=%trim(vidmsg)+ ' ' + %char(arsfls) +
108195121112                                     '/' + %char(arslna);
108196121112                     if arsnrs>0;
108197121112                        vidmsg=%trim(vidmsg)+ ' ' + %char(arsnrs) + '-' +
108198121112                        %char(arsnsc);
108199121112                     else;
108200121112                        vidmsg=%trim(vidmsg) + ' ' + %char(arsnsc) ;
108201121112                     endif;
108202121112                endif;
108203121112                leavesr;
108204121112             endif;
108212121112             reade (knot:ktrc) fiars02l;
108213121112         enddo;
108214121003         endsr;
108215121003       // ------------------------------------------------------------------------
108216121003       // Gestione prima videata - Richiesta Codice chi sono
108217121003       // ------------------------------------------------------------------------
108218121003         begsr Gesd02;
108219121003       // Caricamento video
108220121003         exsr card02;
108221121005       // Se  non trovati codici adiacenti lo segnalo con messaggio di errore
108222121005         if d02inf=*blanks and d02sup=*blanks;
108223121009            *in28=*on;
108224121009            *in90=*on;
108225121009            vidmsg=msg(4);
108226121031            if ii1chi<>' ';
108227121031               $fine=*on;
108228121031               leavesr    ;
108229121031            else;
108230121031               $video='D1';
108231121031               leavesr    ;
108232121031            endif;
108233121005         endif;
108234121009         if d02inf=*blanks;
108235121009            d02inf='Non presente';
108236121009         endif;
108237121009         if d02sup=*blanks;
108238121009            d02sup='Non presente';
108239121009         endif;
108240121003         write lri1t01;
108241121003         dow $video='D2';
108242121009            exfmt lri1d02;
108243121003            *in28=*off;
108244121003            *in90=*off;
108245121003            clear vidmsg;
108246121003            select;
108247121003       // f3=Fine
108248121003            when *inkc;
108249121003               $fine=*on;
108250121003               leavesr;
108251121031       // f12=Ritorno  se pgm non richiamato
108252121031            when *inkl and ii1chi=' ';
108253121003               $video='D1';
108254121003               leavesr;
108255121031       // f12=Ritorno  se pgm richiamato
108256121031            when *inkl and ii1chi<>' ';
108257121031               $fine =*on;
108258121031               leavesr;
108259121003            other;
108260121003            endsl;
108261121003         enddo;
108262121003         endsr;
108263121003       // ------------------------------------------------------------------------
108264121003       // Caricamento seconda videata
108265121003       // ------------------------------------------------------------------------
108266121003         begsr card02;
108267121003
108268121003         clear lri1d02;
108269121031         d02chi=wchi;
108270121003       // Cerco il chi sono precedente
108271121003         setll (d02chi) fiars02l;
108272121005         readp  fiars02l;
108273121009         dow not %eof(fiars02l)and %subst(d02chi:1:2)=%subst(arsnot:1:2);
108274121005            if arstrc='C';
108275121005               clear fnlsz1ds;
108276121005               iz1mod='C';
108277121005               iz1ch1=arsnot;
108278121005               clear ds1b;
108279121005               fnlsz1r(kpjba:fnlsz1ds:ds1b);
108280121005       // Errore se non si tratta di chi sono BRT
108281121005               if oz1err=*blanks ;
108282121112       // Lo carico solo se data spedizione non inferiore di 40 giorni da oggi
108283121112       //         exsr rep_dsp;
108284121112                  dataiso_sped=%date(arsduv:*iso);
108285121112                  eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
108290121112                  if num_days<=40;
108291121112                     d02inf=arsnot;
108292121112                     exsr repdati;
108293121112                     exsr carvidi;
108294121112                     leave;
108295121112                  endif;
108298121112                  readp fiars02l;
108306121005               else;
108307121005                  readp fiars02l;
108308121005               endif;
108309121005            else;
108310121005               readp fiars02l;
108311121005            endif;
108312121003         enddo;
108313121003       // Cerco il chi sono successivo
108314121005         setll (d02chi) fiars02l;
108315121005         read fiars02l;
108316121009         dow not %eof(fiars02l) and %subst(d02chi:1:2)=%subst(arsnot:1:2);
108317121005            if arstrc='C';
108318121005               clear fnlsz1ds;
108319121005               iz1mod='C';
108320121005               iz1ch1=arsnot;
108321121005               clear ds1b;
108322121005               fnlsz1r(kpjba:fnlsz1ds:ds1b);
108323121005       // Errore se non si tratta di chi sono BRT
108324121005               if oz1err=*blanks ;
108325121112       // Lo carico solo se data spedizione non inferiore di 40 giorni da oggi
108326121112       //         exsr rep_dsp;
108327121112                  dataiso_sped=%date(arsduv:*iso);
108328121112                  eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
108329121112                  if num_days<=40;
108330121112                     d02sup=arsnot;
108331121112                     exsr repdati;
108332121112                     exsr carvids;
108333121112                     leave;
108334121112                  endif;
108335121112                  read fiars02l;
108336121005               else;
108337121005                  read fiars02l;
108338121005               endif;
108339121005            else;
108340121005               read fiars02l;
108341121005            endif;
108342121005         enddo;
108343121003         endsr;
108344121112       // ------------------------------------------------------------------------
108345121112       // Reperimento dati della spedizione e della prima spunta del collo
108346121112       // ------------------------------------------------------------------------
108347121112         begsr rep_dsp;
108348121112
108349121112         num_days=*hival;
108350121112
108351121112         chain (arsfls:arslna:arsnrs:arsnsc) fnblt27l;
108352121112         if %found(fnblt27l);
108353121112            chain (bltaas:bltlnp:bltnrs:bltnsp) fnblp01l;
108354121112               if %found(fnblp01l);
108355121112                  dataiso_sped=%date((blpaas*10000+blpmgs):*iso);
108356121112                  eval num_days=%diff(dataiso_cor:dataiso_sped: *days);
108357121112               endif;
108358121112         endif;
108359121112
108360121112         endsr;
108361121005       // ------------------------------------------------------------------------
108362121005       // Reperimento dati della spedizione e della prima spunta del collo
108363121005       // ------------------------------------------------------------------------
108364121005         begsr repdati;
108365121005       //Numero spedizione
108366121009         clear wblt;
108367121005         chain (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnblt27l;
108368121009         if %found(fnblt27l);
108369121009            wblt='S';
108370121009         endif;
108371121005       //Prima spunta del collo
108372121005         wdfs=*hival;
108373121005         clear whms;
108374121005         clear wnpg;
108375121005         clear wpes;
108376121005         clear wnps;
108377121005         clear wfgs;
108378121005         clear wnfv;
108379121005         clear wbrv;
108380121005         setll (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnbrv07l;
108381121005         reade (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnbrv07l;
108382121005         dow not %eof (fnbrv07l);
108383121005         if brvdfs<wdfs or (brvdfs=wdfs and brvhms<whms);
108384121005            wbrv='S';
108385121005            wdfs=brvdfs;
108386121005            whms=brvhms;
108387121005            wnpg=brvnpg;
108388121005            wpes=brvpes;
108389121005            wnps=brvnps;
108390121005            wnfv=brvnfv;
108391121005            wfgs=brvfgs;
108392121005         endif;
108393121005         reade (ARSFLS:ARSLNA:ARSNRS:arsnsc) fnbrv07l;
108394121005         enddo;
108395121005         endsr;
108396121005       // ------------------------------------------------------------------------
108397121005       // Caricamento videata: dati del collo precedente
108398121005       // ------------------------------------------------------------------------
108399121005         begsr carvidi;
108400121008       // Spedizione
108401121009         if wblt='S';
108402121009            d02lnpi=bltlnp;
108403121009            if bltnrs>0;
108404121009               d02nspi=%editc(bltnrs:'X')+'-'+%editc(bltnsp:'Z');
108405121009            else;
108406121009               d02nspi=%editc(bltnsp:'Z');
108407121009            endif;
108408121009            d02aasi=bltaas;
108409121009         endif;
108410121009       // Segnacollo
108411121009         d02flsi=%editc(arsfls:'X')+'/'+%editc(arslna:'X');
108412121009         if arsnrs>0;
108413121009            d02nsci=%editc(arsnrs:'X')+'-'+%editc(arsnsc:'Z');
108414121009         else;
108415121009            d02nsci=%editc(arsnsc:'Z');
108416121009         endif;
108417121005       // Categoria di spunta
108418121005         if wbrv='S';
108419121005         eval d02cati=%editc(wnpg:'X');
108420121005         exsr impo_dnpg;
108421121005         eval d02cati=%trim(d02cati)+ '-' + wdnpg;
108422121005       // filiale
108423121005         d02pesi=%editc(wpes:'X');
108424121005         chain wpes azorg01l;
108425121008         d02pesi=%trim(d02pesi)+ ' ' + orgdes;
108426121005       // Data immissione spunta
108427121005         data_eur=%date(wdfs:*iso);
108428121005         d02dfsi=%dec(data_eur);
108429121005       // Pistola
108430121008         d02npsi=%editc(wnps:'X');
108431121008       //
108432121008         d02dii='di';
108433121008         d02deli='del';
108434121008         d02psti='Pst';
108435121005         endif;
108436121005       // Scarto fra i due chi sono
108437121005         chib1=d02chi;
108438121005         chib2=d02inf;
108439121005         d02numi=prgchib1-prgchib2;
108440121005         endsr;
108441121005       // ------------------------------------------------------------------------
108442121005       // Caricamento videata: dati del collo successivo
108443121005       // ------------------------------------------------------------------------
108444121005         begsr carvids;
108445121009         if wblt='S';
108446121009            d02lnps=bltlnp;
108447121009            if bltnrs>0;
108448121009               d02nsps=%editc(bltnrs:'X')+'-'+%editc(bltnsp:'Z');
108449121009            else;
108450121009               d02nsps=%editc(bltnsp:'Z');
108451121009            endif;
108452121009            d02aass=bltaas;
108453121009         endif;
108454121009       // Segnacollo
108455121009         d02flss=%editc(arsfls:'X')+'/'+%editc(arslna:'X');
108456121009         if arsnrs>0;
108457121009            d02nscs=%editc(arsnrs:'X')+'-'+%editc(arsnsc:'Z');
108458121009         else;
108459121009            d02nscs=%editc(arsnsc:'Z');
108460121009         endif;
108461121005         if wbrv='S';
108462121005       // Categoria di spunta
108463121005         eval d02cats=%editc(wnpg:'X');
108464121005         exsr impo_dnpg;
108465121005         eval d02cats=%trim(d02cats)+ '-' + wdnpg;
108466121005       // filiale
108467121005         d02pess=%editc(wpes:'X');
108468121005         chain wpes azorg01l;
108469121008         d02pess=%trim(d02pess)+ ' ' + orgdes;
108470121005       // Data immissione spunta
108471121005         data_eur=%date(wdfs:*iso);
108472121005         d02dfss=%dec(data_eur);
108473121005       // Pistola
108474121008         d02npss=%editc(wnps:'X');
108475121008         d02dis='di';
108476121008         d02dels='del';
108477121008         d02psts='Pst';
108478121005         endif;
108479121005       // Scarto fra i due chi sono
108480121005         chib1=d02sup;
108481121005         chib2=d02chi;
108482121005         d02nums=prgchib1-prgchib2;
108483121005         endsr;
108484121005       // ------------------------------------------------------------------------
108485121005       // Impostazione campo descrizione categoria spunta
108486121005       // ------------------------------------------------------------------------
108487121005         begsr impo_dnpg;
108488121005         clear wdnpg;
108489121005         n=%lookup(%editc(wnpg:'X'):cat) ;
108490121005         if wnpg<>1;
108491121005            clear fvvspg;
108492121005            chain (wnpg:wnfv:wfgs) fnfvv01l;
108493121005            if fvvspg<>*blanks;
108494121005               s=%lookup(fvvspg:spg);
108495121005               eval wdnpg='IM' + des(s);
108496121005            else ;
108497121005               eval wdnpg=dec(n);
108498121005            endif;
108499121005         else;
108500121009             chain (wnfv:wfgs) fnfgv01l;
108501121009             if %found(fnfgv01l) and %lookup(fgvttr:def)>0;
108502121009                eval wdnpg='DEF';
108503121009             else;
108504121009                eval wdnpg=dec(n);
108505121009             endif;
108506121005         endif;
108507121005         endsr;
108508121005
108509121005      /end-free
108510060502
108511060502** -MSG-                                                                     *
108600121031Il codice immesso non è un "CHI SONO"  BRT                                     01
108700121003Codice CHI SONO presente sulla spedizione                                      02
108801121112Codice CHI SONO attribuito al segnacollo                                       03
108900121009Non tovati codici CHI SONO adiacenti al codice richiesto                       04
